/*
  Copyright information from the original package:
*/
/*
  The authors of this software is Rob Pike.
        Copyright (c) 2002 by Lucent Technologies.
  Permission to use, copy, modify, and distribute this software for any
  purpose without fee is hereby granted, provided that this entire notice
  is included in all copies of any software which is or includes a copy
  or modification of this software and in all copies of the supporting
  documentation for such software.
  THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
  WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR LUCENT TECHNOLOGIES MAKE ANY
  REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
  OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.


  This is a Unix port of the Plan 9 regular expression library.

  Please send comments about the packaging
  to Russ Cox <rsc@swtch.com>.

  ----

  This software is also made available under the Lucent Public License
  version 1.02; see http://plan9.bell-labs.com/plan9dist/license.html
*/
/*
  This software was packaged for Unix by Russ Cox.
  Please send comments to rsc@swtch.com.

  http://swtch.com/plan9port/unix
*/

#include <stdlib.h>
#include <stdint.h>
#include <setjmp.h>
#include <string.h>
#include "regexp9.h"

/*************
 * regexp9.h *
 *************/

typedef uint32_t Rune;

/*
 *    character class, each pair of rune's defines a range
 */
typedef struct Reclass  Reclass;
struct Reclass
{
    Rune    *end;
    Rune    spans[64];
};

/*
 *    Machine instructions
 */
typedef struct Reinst   Reinst;
struct Reinst
{
    int type;
    union {
        Reclass *classp;     /* class pointer */
        Rune    rune;        /* character */
        int     subid;       /* sub-expression id for RBRA and LBRA */
        Reinst  *right;      /* right child of OR */
    } r;
    union {    /* regexp relies on these two being in the same union */
        Reinst *left;        /* left child of OR */
        Reinst *next;        /* next instruction for CAT & LBRA */
    } l;
};

/*
 *    Reprogram definition
 */
struct Reprog
{
    Reinst  *startinst;     /* start pc */
    Reclass class[16];      /* .data */
    Reinst  firstinst[5];   /* .text */
};

/*************
 * regcomp.h *
 *************/

/*
 *  substitution list
 */
#define uchar __reuchar
typedef unsigned char uchar;
#define nelem(x) (sizeof(x)/sizeof((x)[0]))

#define NSUBEXP 32
typedef struct Resublist    Resublist;
struct    Resublist
{
    Resub   m[NSUBEXP];
};

/* max character classes per program */
extern Reprog   RePrOg;
#define NCLASS  (sizeof(RePrOg.class)/sizeof(Reclass))

/* max rune ranges per character class */
#define NCCRUNE (sizeof(Reclass)/sizeof(Rune))

/*
 * Actions and Tokens (Reinst types)
 *
 *    02xx are operators, value == precedence
 *    03xx are tokens, i.e. operands for operators
 */
#define RUNE        0177
#define OPERATOR    0200    /* Bitmask of all operators */
#define START       0200    /* Start, used for marker on stack */
#define RBRA        0201    /* Right bracket, ) */
#define LBRA        0202    /* Left bracket, ( */
#define OR          0203    /* Alternation, | */
#define CAT         0204    /* Concatentation, implicit operator */
#define STAR        0205    /* Closure, * */
#define PLUS        0206    /* a+ == aa* */
#define QUEST       0207    /* a? == a|nothing, i.e. 0 or 1 a's */
#define ANY         0260    /* Any character except newline, . */
#define ANYNL       0261    /* Any character including newline, . */
#define NOP         0262    /* No operation, internal use only */
#define BOL         0263    /* Beginning of line, ^ */
#define EOL         0264    /* End of line, $ */
#define CCLASS      0265    /* Character class, [] */
#define NCCLASS     0266    /* Negated character class, [] */
#define END         0277    /* Terminate: match found */

/*
 *  regexec execution lists
 */
#define LISTSIZE    10
#define BIGLISTSIZE (10*LISTSIZE)
typedef struct Relist   Relist;
struct Relist
{
    Reinst*     inst;       /* Reinstruction of the thread */
    Resublist   se;         /* matched subexpressions in this thread */
};

typedef struct Reljunk  Reljunk;
struct Reljunk
{
    Relist*     relist[2];
    Relist*     reliste[2];
    int         starttype;
    Rune        startchar;
    const char* starts;
    const char* eol;
};

/**********************
 * utf8 and Rune code
 **********************/

static inline int
utf8_codep_size(const char* s) /* branchless */
{
    uint8_t u = (uint8_t)*s;
    int ret = (u & 0xF0) == 0xE0;
    ret += ret << 1;                       /* 3 */
    ret |= u < 0x80;                       /* 1 */
    ret |= ((0xC1 < u) & (u < 0xE0)) << 1; /* 2 */
    ret |= ((0xEF < u) & (u < 0xF5)) << 2; /* 4 */
    return ret;
}

static int
chartorune(Rune *rune, const char *s)
{
    int n = utf8_codep_size(s);
    *rune = s[0];
    switch (n) {
    case 4: *rune |= s[3] << 24;
    case 3: *rune |= s[2] << 16;
    case 2: *rune |= s[1] << 8;
    }
    return n;    
}

static const char*
utfrune(const char *s, Rune c)
{
    Rune r;
    int n;

    if (c < 128)        /* ascii */
        return strchr((char *)s, c);

    for (;;) {
        n = chartorune(&r, s);
        if (r == c) return s;
        if ((r == 0) | (n == 0)) return NULL;
        s += n;
    }
}

/************
 * regaux.c *
 ************/

/*
 *  save a new match in mp
 */
static void
_renewmatch(Resub *mp, int ms, Resublist *sp)
{
    int i;

    if (mp==NULL || ms<=0)
        return;
    if (mp[0].sp==NULL || sp->m[0].sp<mp[0].sp ||
       (sp->m[0].sp==mp[0].sp && sp->m[0].ep>mp[0].ep)) {
        for (i=0; i<ms && i<NSUBEXP; i++)
            mp[i] = sp->m[i];
        for (; i<ms; i++)
            mp[i].sp = mp[i].ep = NULL;
    }
}

/*
 * Note optimization in _renewthread:
 *     *lp must be pending when _renewthread called; if *l has been looked
 *        at already, the optimization is a bug.
 */
static Relist*
_renewthread(Relist *lp,    /* _relist to add to */
    Reinst *ip,        /* instruction to add */
    int ms,
    Resublist *sep)        /* pointers to subexpressions */
{
    Relist *p;

    for (p=lp; p->inst; p++) {
        if (p->inst == ip) {
            if (sep->m[0].sp < p->se.m[0].sp) {
                if (ms > 1)
                    p->se = *sep;
                else
                    p->se.m[0] = sep->m[0];
            }
            return 0;
        }
    }
    p->inst = ip;
    if (ms > 1)
        p->se = *sep;
    else
        p->se.m[0] = sep->m[0];
    (++p)->inst = NULL;
    return p;
}

/*
 * same as renewthread, but called with
 * initial empty start pointer.
 */
static Relist*
_renewemptythread(Relist *lp,    /* _relist to add to */
    Reinst *ip,        /* instruction to add */
    int ms,
    const char *sp)        /* pointers to subexpressions */
{
    Relist *p;

    for (p=lp; p->inst; p++) {
        if (p->inst == ip) {
            if (sp < p->se.m[0].sp) {
                if (ms > 1)
                    memset(&p->se, 0, sizeof(p->se));
                p->se.m[0].sp = sp;
            }
            return 0;
        }
    }
    p->inst = ip;
    if (ms > 1)
        memset(&p->se, 0, sizeof(p->se));
    p->se.m[0].sp = sp;
    (++p)->inst = NULL;
    return p;
}

/*************
 * regcomp.c *
 *************/

#define    TRUE    1
#define    FALSE    0

/*
 * Parser Information
 */
typedef struct Node
{
    Reinst*    first;
    Reinst*    last;
} Node;

#define NSTACK 20
typedef struct Parser
{
    Node andstack[NSTACK];
    Node* andp;
    int atorstack[NSTACK];
    int* atorp;
    int cursubid;        /* id of current subexpression */
    int subidstack[NSTACK]; /* parallel to atorstack */
    int* subidp;
    int lastwasand;      /* Last token was operand */
    int nbra;
    const char* exprp;   /* pointer to next character in source expression */
    int lexdone;
    int nclass;
    Reclass*classp;
    Reinst* freep;
    int errors;
    Rune yyrune;         /* last lex'd rune */
    Reclass *yyclassp;   /* last lex'd class */
    jmp_buf regkaboom;
} Parser;

/* predeclared crap */
static void operator(Parser *par, int type);
static void pushand(Parser *par, Reinst *first, Reinst *last);
static void pushator(Parser *par, int type);
static void evaluntil(Parser *par, int type);
static int  bldcclass(Parser *par);
extern void regerror9(const char*);

static void
rcerror(Parser *par, const char *s)
{
    par->errors++;
    regerror9(s);
    longjmp(par->regkaboom, 1);
}

static Reinst*
newinst(Parser *par, int t)
{
    par->freep->type = t;
    par->freep->l.left = 0;
    par->freep->r.right = 0;
    return par->freep++;
}

static void
operand(Parser *par, int t)
{
    Reinst *i;

    if (par->lastwasand)
        operator(par, CAT);    /* catenate is implicit */
    i = newinst(par, t);

    if (t == CCLASS || t == NCCLASS)
        i->r.classp = par->yyclassp;
    if (t == RUNE)
        i->r.rune = par->yyrune;

    pushand(par, i, i);
    par->lastwasand = TRUE;
}

static void
operator(Parser *par, int t)
{
    if (t==RBRA && --par->nbra<0)
        rcerror(par, "unmatched right paren");
    if (t==LBRA) {
        if (++par->cursubid >= NSUBEXP)
            rcerror(par, "too many subexpressions");
        par->nbra++;
        if (par->lastwasand)
            operator(par, CAT);
    } else
        evaluntil(par, t);
    if (t != RBRA)
        pushator(par, t);
    par->lastwasand = 0;
    if (t==STAR || t==QUEST || t==PLUS || t==RBRA)
        par->lastwasand = TRUE;    /* these look like operands */
}

static void
regerr2(Parser *par, const char *s, int c)
{
    char buf[100];
    char *cp = buf;
    while (*s)
        *cp++ = *s++;
    *cp++ = c;
    *cp = '\0'; 
    rcerror(par, buf);
}

static void
cant(Parser *par, const char *s)
{
    char buf[100];
    strcpy(buf, "can't happen: ");
    strcat(buf, s);
    rcerror(par, buf);
}

static void
pushand(Parser *par, Reinst *f, Reinst *l)
{
    if (par->andp >= &par->andstack[NSTACK])
        cant(par, "operand stack overflow");
    par->andp->first = f;
    par->andp->last = l;
    par->andp++;
}

static void
pushator(Parser *par, int t)
{
    if (par->atorp >= &par->atorstack[NSTACK])
        cant(par, "operator stack overflow");
    *par->atorp++ = t;
    *par->subidp++ = par->cursubid;
}

static Node*
popand(Parser *par, int op)
{
    Reinst *inst;

    if (par->andp <= &par->andstack[0]) {
        regerr2(par, "missing operand for ", op);
        inst = newinst(par, NOP);
        pushand(par, inst, inst);
    }
    return --par->andp;
}

static int
popator(Parser *par)
{
    if (par->atorp <= &par->atorstack[0])
        cant(par, "operator stack underflow");
    --par->subidp;
    return *--par->atorp;
}

static void
evaluntil(Parser *par, int pri)
{
    Node *op1, *op2;
    Reinst *inst1, *inst2;

    while (pri==RBRA || par->atorp[-1]>=pri) {
        switch (popator(par)) {
        default:
            rcerror(par, "unknown operator in evaluntil");
            break;
        case LBRA:        /* must have been RBRA */
            op1 = popand(par, '(');
            inst2 = newinst(par, RBRA);
            inst2->r.subid = *par->subidp;
            op1->last->l.next = inst2;
            inst1 = newinst(par, LBRA);
            inst1->r.subid = *par->subidp;
            inst1->l.next = op1->first;
            pushand(par, inst1, inst2);
            return;
        case OR:
            op2 = popand(par, '|');
            op1 = popand(par, '|');
            inst2 = newinst(par, NOP);
            op2->last->l.next = inst2;
            op1->last->l.next = inst2;
            inst1 = newinst(par, OR);
            inst1->r.right = op1->first;
            inst1->l.left = op2->first;
            pushand(par, inst1, inst2);
            break;
        case CAT:
            op2 = popand(par, 0);
            op1 = popand(par, 0);
            op1->last->l.next = op2->first;
            pushand(par, op1->first, op2->last);
            break;
        case STAR:
            op2 = popand(par, '*');
            inst1 = newinst(par, OR);
            op2->last->l.next = inst1;
            inst1->r.right = op2->first;
            pushand(par, inst1, inst1);
            break;
        case PLUS:
            op2 = popand(par, '+');
            inst1 = newinst(par, OR);
            op2->last->l.next = inst1;
            inst1->r.right = op2->first;
            pushand(par, op2->first, inst1);
            break;
        case QUEST:
            op2 = popand(par, '?');
            inst1 = newinst(par, OR);
            inst2 = newinst(par, NOP);
            inst1->l.left = inst2;
            inst1->r.right = op2->first;
            op2->last->l.next = inst2;
            pushand(par, inst1, inst2);
            break;
        }
    }
}

static Reprog*
optimize(Parser *par, Reprog *pp)
{
    Reinst *inst, *target;
    size_t size;
    Reprog *npp;
    Reclass *cl;
    ptrdiff_t diff;

    /*
     *  get rid of NOOP chains
     */
    for (inst = pp->firstinst; inst->type != END; inst++) {
        target = inst->l.next;
        while (target->type == NOP)
            target = target->l.next;
        inst->l.next = target;
    }

    /*
     *  The original allocation is for an area larger than
     *  necessary.  Reallocate to the actual space used
     *  and then relocate the code.
     */
    size = sizeof(Reprog) + (par->freep - pp->firstinst)*sizeof(Reinst);
    npp = (Reprog*)realloc(pp, size);
    if (npp==NULL || npp==pp)
        return pp;
    diff = (char *)npp - (char *)pp;
    par->freep = (Reinst *)((char *)par->freep + diff);
    for (inst = npp->firstinst; inst < par->freep; inst++) {
        switch (inst->type) {
        case OR:
        case STAR:
        case PLUS:
        case QUEST:
            inst->r.right = (Reinst*)((char*)inst->r.right + diff);
            break;
        case CCLASS:
        case NCCLASS:
            inst->r.right = (Reinst*)((char*)inst->r.right + diff);
            cl = inst->r.classp;
            cl->end = (Rune*)((char*)cl->end + diff);
            break;
        }
        inst->l.left = (Reinst*)((char*)inst->l.left + diff);
    }
    npp->startinst = (Reinst*)((char*)npp->startinst + diff);
    return npp;
}

#ifdef  DEBUG
static void
dumpstack(Parser *par) {
    Node *stk;
    int *ip;

    print("operators\n");
    for (ip = par->atorstack; ip < par->atorp; ip++)
        print("0%o\n", *ip);
    print("operands\n");
    for (stk = par->andstack; stk < par->andp; stk++)
        print("0%o\t0%o\n", stk->first->type, stk->last->type);
}

static void
dump(Reprog *pp)
{
    Reinst *l;
    Rune *p;

    l = pp->firstinst;
    do {
        print("%d:\t0%o\t%d\t%d", l-pp->firstinst, l->type,
            l->l.left-pp->firstinst, l->r.right-pp->firstinst);
        if (l->type == RUNE)
            print("\t%C\n", l->r.rune);
        else if (l->type == CCLASS || l->type == NCCLASS) {
            print("\t[");
            if (l->type == NCCLASS)
                print("^");
            for (p = l->r.classp->spans; p < l->r.classp->end; p += 2)
                if (p[0] == p[1])
                    print("%C", p[0]);
                else
                    print("%C-%C", p[0], p[1]);
            print("]\n");
        } else
            print("\n");
    } while (l++->type);
}
#endif

static Reclass*
newclass(Parser *par)
{
    if (par->nclass >= NCLASS)
        regerr2(par, "too many character classes; limit", NCLASS+'0');
    return &(par->classp[par->nclass++]);
}

static int
nextc(Parser *par, Rune *rp)
{
    if (par->lexdone) {
        *rp = 0;
        return 1;
    }
    par->exprp += chartorune(rp, par->exprp);
    if (*rp == '\\') {
        par->exprp += chartorune(rp, par->exprp);
        return 1;
    }
    if (*rp == 0)
        par->lexdone = 1;
    return 0;
}

static int
lex(Parser *par, int literal, int dot_type)
{
    int quoted;

    quoted = nextc(par, &par->yyrune);
    if (literal || quoted) {
        if (par->yyrune == 0)
            return END;
        return RUNE;
    }

    switch (par->yyrune) {
    case 0:
        return END;
    case '*':
        return STAR;
    case '?':
        return QUEST;
    case '+':
        return PLUS;
    case '|':
        return OR;
    case '.':
        return dot_type;
    case '(':
        return LBRA;
    case ')':
        return RBRA;
    case '^':
        return BOL;
    case '$':
        return EOL;
    case '[':
        return bldcclass(par);
    }
    return RUNE;
}

static int
bldcclass(Parser *par)
{
    int type;
    Rune r[NCCRUNE];
    Rune *p, *ep, *np;
    Rune rune;
    int quoted;

    /* we have already seen the '[' */
    type = CCLASS;
    par->yyclassp = newclass(par);

    /* look ahead for negation */
    /* SPECIAL CASE!!! negated classes don't match \n */
    ep = r;
    quoted = nextc(par, &rune);
    if (!quoted && rune == '^') {
        type = NCCLASS;
        quoted = nextc(par, &rune);
        *ep++ = '\n';
        *ep++ = '\n';
    }

    /* parse class into a set of spans */
    for (; ep < &r[NCCRUNE];) {
        if (rune == 0) {
            rcerror(par, "malformed '[]'");
            return 0;
        }
        if (!quoted && rune == ']')
            break;
        if (!quoted && rune == '-') {
            if (ep == r) {
                rcerror(par, "malformed '[]'");
                return 0;
            }
            quoted = nextc(par, &rune);
            if ((!quoted && rune == ']') || rune == 0) {
                rcerror(par, "malformed '[]'");
                return 0;
            }
            *(ep-1) = rune;
        } else {
            *ep++ = rune;
            *ep++ = rune;
        }
        quoted = nextc(par, &rune);
    }

    /* sort on span start */
    for (p = r; p < ep; p += 2) {
        for (np = p; np < ep; np += 2)
            if (*np < *p) {
                rune = np[0];
                np[0] = p[0];
                p[0] = rune;
                rune = np[1];
                np[1] = p[1];
                p[1] = rune;
            }
    }

    /* merge spans */
    np = par->yyclassp->spans;
    p = r;
    if (r == ep)
        par->yyclassp->end = np;
    else {
        np[0] = *p++;
        np[1] = *p++;
        for (; p < ep; p += 2)
            if (p[0] <= np[1]) {
                if (p[1] > np[1])
                    np[1] = p[1];
            } else {
                np += 2;
                np[0] = p[0];
                np[1] = p[1];
            }
        par->yyclassp->end = np+2;
    }

    return type;
}

static Reprog*
regcomp1(Parser *par, const char *s, int literal, int dot_type)
{
    int token;
    Reprog *volatile pp;

    /* get memory for the program */
    pp = malloc(sizeof(Reprog) + 6*sizeof(Reinst)*strlen(s));
    if (pp == NULL) {
        regerror9("out of memory");
        return NULL;
    }
    par->freep = pp->firstinst;
    par->classp = pp->class;
    par->errors = 0;

    if (setjmp(par->regkaboom))
        goto out;

    /* go compile the sucker */
    par->lexdone = 0;
    par->exprp = s;
    par->nclass = 0;
    par->nbra = 0;
    par->atorp = par->atorstack;
    par->andp = par->andstack;
    par->subidp = par->subidstack;
    par->lastwasand = FALSE;
    par->cursubid = 0;

    /* Start with a low priority operator to prime parser */
    pushator(par, START-1);
    while ((token = lex(par, literal, dot_type)) != END) {
        if ((token & 0360) == OPERATOR)
            operator(par, token);
        else
            operand(par, token);
    }

    /* Close with a low priority operator */
    evaluntil(par, START);

    /* Force END */
    operand(par, END);
    evaluntil(par, START);
#ifdef DEBUG
    dumpstack(par);
#endif
    if (par->nbra)
        rcerror(par, "unmatched left paren");
    --par->andp;    /* points to first and only operand */
    pp->startinst = par->andp->first;
#ifdef DEBUG
    dump(pp);
#endif
    pp = optimize(par, pp);
#ifdef DEBUG
    print("start: %d\n", par->andp->first-pp->firstinst);
    dump(pp);
#endif
out:
    if (par->errors) {
        free(pp);
        pp = NULL;
    }
    return pp;
}

extern    Reprog*
regcomp9(const char *s)
{
    Parser par;
    return regcomp1(&par, s, 0, ANY);
}

extern    Reprog*
regcomplit9(const char *s)
{
    Parser par;
    return regcomp1(&par, s, 1, ANY);
}

extern    Reprog*
regcompnl9(const char *s)
{
    Parser par;
    return regcomp1(&par, s, 0, ANYNL);
}

/*************
 * regexec.c *
 *************/

/*
 *  return    0 if no match
 *        >0 if a match
 *        <0 if we ran out of _relist space
 */
static int
regexec1(const Reprog *progp,    /* program to run */
    const char *bol,    /* string to run machine on */
    Resub *mp,    /* subexpression elements */
    int ms,        /* number of elements at mp */
    Reljunk *j
)
{
    int flag=0;
    Reinst *inst;
    Relist *tlp;
    const char *s;
    int i, checkstart;
    Rune r, *rp, *ep;
    int n;
    Relist* tl;        /* This list, next list */
    Relist* nl;
    Relist* tle;        /* ends of this and next list */
    Relist* nle;
    int match;
    const char *p;

    match = 0;
    checkstart = j->starttype;
    if (mp)
        for (i=0; i<ms; i++) {
            mp[i].sp = NULL;
            mp[i].ep = NULL;
        }
    j->relist[0][0].inst = NULL;
    j->relist[1][0].inst = NULL;

    /* Execute machine once for each character, including terminal NUL */
    s = j->starts;
    do {
        /* fast check for first char */
        if (checkstart) {
            switch (j->starttype) {
            case RUNE:
                p = utfrune(s, j->startchar);
                if (p == NULL || s == j->eol)
                    return match;
                s = p;
                break;
            case BOL:
                if (s == bol)
                    break;
                p = utfrune(s, '\n');
                if (p == NULL || s == j->eol)
                    return match;
                s = p+1;
                break;
            }
        }
        n = chartorune(&r, s);

        /* switch run lists */
        tl = j->relist[flag];
        tle = j->reliste[flag];
        nl = j->relist[flag^=1];
        nle = j->reliste[flag];
        nl->inst = NULL;

        /* Add first instruction to current list */
        if (match == 0)
            _renewemptythread(tl, progp->startinst, ms, s);

        /* Execute machine until current list is empty */
        for (tlp=tl; tlp->inst; tlp++) {    /* assignment = */
            for (inst = tlp->inst; ; inst = inst->l.next) {
                switch (inst->type) {
                case RUNE:    /* regular character */
                    if (inst->r.rune == r) {
                        if (_renewthread(nl, inst->l.next, ms, &tlp->se)==nle)
                            return -1;
                    }
                    break;
                case LBRA:
                    tlp->se.m[inst->r.subid].sp = s;
                    continue;
                case RBRA:
                    tlp->se.m[inst->r.subid].ep = s;
                    continue;
                case ANY:
                    if (r != '\n')
                        if (_renewthread(nl, inst->l.next, ms, &tlp->se)==nle)
                            return -1;
                    break;
                case ANYNL:
                    if (_renewthread(nl, inst->l.next, ms, &tlp->se)==nle)
                            return -1;
                    break;
                case BOL:
                    if (s == bol || *(s-1) == '\n')
                        continue;
                    break;
                case EOL:
                    if (s == j->eol || r == 0 || r == '\n')
                        continue;
                    break;
                case CCLASS:
                    ep = inst->r.classp->end;
                    for (rp = inst->r.classp->spans; rp < ep; rp += 2)
                        if (r >= rp[0] && r <= rp[1]) {
                            if (_renewthread(nl, inst->l.next, ms, &tlp->se)==nle)
                                return -1;
                            break;
                        }
                    break;
                case NCCLASS:
                    ep = inst->r.classp->end;
                    for (rp = inst->r.classp->spans; rp < ep; rp += 2)
                        if (r >= rp[0] && r <= rp[1])
                            break;
                    if (rp == ep)
                        if (_renewthread(nl, inst->l.next, ms, &tlp->se)==nle)
                            return -1;
                    break;
                case OR:
                    /* evaluate right choice later */
                    if (_renewthread(tlp, inst->r.right, ms, &tlp->se) == tle)
                        return -1;
                    /* efficiency: advance and re-evaluate */
                    continue;
                case END:    /* Match! */
                    match = 1;
                    tlp->se.m[0].ep = s;
                    if (mp != NULL)
                        _renewmatch(mp, ms, &tlp->se);
                    break;
                }
                break;
            }
        }
        if (s == j->eol)
            break;
        checkstart = j->starttype && nl->inst==NULL;
        s += n;
    } while (r);
    return match;
}

static int
regexec2(const Reprog *progp,    /* program to run */
    const char *bol,    /* string to run machine on */
    Resub *mp,    /* subexpression elements */
    int ms,        /* number of elements at mp */
    Reljunk *j
)
{
    int rv;
    Relist *relist0, *relist1;

    /* mark space */
    relist0 = malloc(BIGLISTSIZE*sizeof(Relist));
    if (relist0 == NULL)
        return -1;
    relist1 = malloc(BIGLISTSIZE*sizeof(Relist));
    if (relist1 == NULL) {
        free(relist1);
        return -1;
    }
    j->relist[0] = relist0;
    j->relist[1] = relist1;
    j->reliste[0] = relist0 + BIGLISTSIZE - 2;
    j->reliste[1] = relist1 + BIGLISTSIZE - 2;

    rv = regexec1(progp, bol, mp, ms, j);
    free(relist0);
    free(relist1);
    return rv;
}

extern int
regexec9(const Reprog *progp,    /* program to run */
    const char *bol,    /* string to run machine on */
    Resub *mp,    /* subexpression elements */
    int ms)        /* number of elements at mp */
{
    Reljunk j;
    Relist relist0[LISTSIZE], relist1[LISTSIZE];
    int rv;

    /*
      *  use user-specified starting/ending location if specified
     */
    j.starts = bol;
    j.eol = NULL;
    if (mp && ms>0) {
        if (mp->sp)
            j.starts = mp->sp;
        if (mp->ep)
            j.eol = mp->ep;
    }
    j.starttype = 0;
    j.startchar = 0;
    if (progp->startinst->type == RUNE && progp->startinst->r.rune < 128) {
        j.starttype = RUNE;
        j.startchar = progp->startinst->r.rune;
    }
    if (progp->startinst->type == BOL)
        j.starttype = BOL;

    /* mark space */
    j.relist[0] = relist0;
    j.relist[1] = relist1;
    j.reliste[0] = relist0 + nelem(relist0) - 2;
    j.reliste[1] = relist1 + nelem(relist1) - 2;

    rv = regexec1(progp, bol, mp, ms, &j);
    if (rv >= 0)
        return rv;
    rv = regexec2(progp, bol, mp, ms, &j);
    if (rv >= 0)
        return rv;
    return -1;
}

/************
 * regsub.c *
 ************/

/* substitute into one string using the matches from the last regexec() */
extern    void
regsub9(const char *sp,    /* source string */
    char *dp,    /* destination string */
    int dlen,
    Resub *mp,    /* subexpression elements */
    int ms)        /* number of elements pointed to by mp */
{
    const char *ssp, *ep;
    int i;

    ep = dp+dlen-1;
    while (*sp != '\0') {
        if (*sp == '\\') {
            switch (*++sp) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                i = *sp-'0';
                if (mp[i].sp != NULL && mp!=NULL && ms>i)
                    for (ssp = mp[i].sp;
                         ssp < mp[i].ep;
                         ssp++)
                        if (dp < ep)
                            *dp++ = *ssp;
                break;
            case '\\':
                if (dp < ep)
                    *dp++ = '\\';
                break;
            case '\0':
                sp--;
                break;
            default:
                if (dp < ep)
                    *dp++ = *sp;
                break;
            }
        } else if (*sp == '&') {                
            if (mp[0].sp != NULL && mp!=NULL && ms>0)
            if (mp[0].sp != NULL)
                for (ssp = mp[0].sp;
                     ssp < mp[0].ep; ssp++)
                    if (dp < ep)
                        *dp++ = *ssp;
        } else {
            if (dp < ep)
                *dp++ = *sp;
        }
        sp++;
    }
    *dp = '\0';
}

/**************
 * regerror.c *
 **************/

#include <stdio.h>

void
regerror9(const char *s)
{
    fprintf(stderr, "regerror: %s\n", s);
}
