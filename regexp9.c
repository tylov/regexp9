/*
This is a Unix port of the Plan 9 regular expression library, by Rob Pike.
Please send comments about the packaging to Russ Cox <rsc@swtch.com>.

Copyright © 2021 Plan 9 Foundation
Copyright © 2022 Tyge Løvset, for additions made in 2022.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <setjmp.h>
#include <string.h>
#include <ctype.h>
#include "regexp9.h"
#include "utf8_tables.h"

/*************
 * regexp9.h *
 *************/

typedef uint32_t Rune;
/* max character classes per program */
#define NCLASS 16
/* max subexpressions */
#define NSUBEXP 32
/* max rune ranges per character class */
#define NCCRUNE (NSUBEXP * 2)

/*
 *    character class, each pair of rune's defines a range
 */
typedef struct
{
    Rune *end;
    Rune spans[NCCRUNE];
} Reclass;

/*
 *    Machine instructions
 */
typedef struct Reinst
{
    int type;
    union {
        Reclass *classp;        /* class pointer */
        Rune    rune;           /* character */
        int     subid;          /* sub-expression id for RBRA and LBRA */
        struct Reinst *right;   /* right child of OR */
    } r;
    union {    /* regexp relies on these two being in the same union */
        struct Reinst *left;    /* left child of OR */
        struct Reinst *next;    /* next instruction for CAT & LBRA */
    } l;
} Reinst;

typedef struct {
    bool ignorecase;
    bool dotall;
    bool newline;
} Reflags;

/*
 *    Reprogram definition
 */
typedef struct Reprog
{
    Reinst  *startinst;     /* start pc */
    Reflags flags;
    int nsubids;
    Reclass cclass[NCLASS]; /* .data */
    Reinst  firstinst[];    /* .text : originally 5 elements? */
} Reprog;

/*************
 * regcomp.h *
 *************/

/*
 *    Sub expression matches
 */
typedef struct Resub {
    const char *sp; /* start pos */
    const char *ep; /* end pos */
} Resub;

/*
 *  substitution list
 */
typedef struct Resublist
{
    Resub m[NSUBEXP];
} Resublist;

/*
 * Actions and Tokens (Reinst types)
 *
 *  0x80-0x8F: operators, value => precedence
 *  0x90-0xAF: RUNE and char classes.
 *  0xB0-0xBF: tokens, i.e. operands for operators
 */
enum {
    OPERATOR    = 0x80,  /* Bitmask of all operators */
    START       = 0x80,  /* Start, used for marker on stack */
    RBRA        ,        /* Right bracket, ) */
    LBRA        ,        /* Left bracket, ( */
    OR          ,        /* Alternation, | */
    CAT         ,        /* Concatentation, implicit operator */
    STAR        ,        /* Closure, * */
    PLUS        ,        /* a+ == aa* */
    QUEST       ,        /* a? == a|nothing, i.e. 0 or 1 a's */
    RUNE        = 0x90,
    CLS_d       , CLS_D, /* digit, non-digit */
    CLS_s       , CLS_S, /* space, non-space */
    CLS_w       , CLS_W, /* word, non-word */
    CLS_an      , CLS_AN, /* alphanum */
    CLS_al      , CLS_AL, /* alpha */
    CLS_bl      , CLS_BL, /* blank */
    CLS_pu      , CLS_PU, /* punct */    
    CLS_ct      , CLS_CT, /* ctrl */
    CLS_gr      , CLS_GR, /* graphic */
    CLS_lo      , CLS_LO, /* lower */
    CLS_pr      , CLS_PR, /* print */
    CLS_up      , CLS_UP, /* upper */
    CLS_xd      , CLS_XD, /* xdigit */    
    ANY         = 0xB0,   /* Any character except newline, . */
    ANYNL       ,         /* Any character including newline, . */
    NOP         ,         /* No operation, internal use only */
    BOL         ,         /* Beginning of line, ^ */
    EOL         ,         /* End of line, $ */
    CCLASS      ,         /* Character class, [] */
    NCCLASS     ,         /* Negated character class, [] */
    WBOUND      ,         /* Non-word boundary, not consuming meta char */
    NWBOUND     ,         /* Word boundary, not consuming meta char */
    END         = 0xBF,   /* Terminate: match found */
};

/*
 *  regexec execution lists
 */
#define LISTSIZE    10
#define BIGLISTSIZE (10*LISTSIZE)

typedef struct Relist
{
    Reinst*     inst;       /* Reinstruction of the thread */
    Resublist   se;         /* matched subexpressions in this thread */
} Relist;

typedef struct Reljunk
{
    Relist*     relist[2];
    Relist*     reliste[2];
    int         starttype;
    Rune        startchar;
    const char* starts;
    const char* eol;
} Reljunk;

/**********************
 * utf8 and Rune code
 **********************/

enum { UTF8_OK = 0, UTF8_ERROR = 4 };
typedef struct { uint32_t state, codep, len; } utf8_decode_t;

static inline uint32_t 
utf8_decode(utf8_decode_t *c, const uint8_t b)
{
    switch (c->state) {
    case UTF8_OK:
        if      (b < 0x80) c->codep = b, c->len = 1;
        else if (b < 0xc2) c->state = UTF8_ERROR;
        else if (b < 0xe0) c->state = 1, c->codep = b & 0x1f, c->len = 2;
        else if (b < 0xf0) c->state = 2, c->codep = b & 0x0f, c->len = 3;
        else if (b < 0xf5) c->state = 3, c->codep = b & 0x07, c->len = 4;
        else c->state = UTF8_ERROR;
        break;
    case 1: case 2: case 3:
        if ((b & 0xc0) == 0x80) {
            c->state -= 1;
            c->codep = (c->codep << 6) | (b & 0x3f);
        } else
            c->state = UTF8_ERROR;
    }
    return c->state;
}

static int
chartorune(Rune *rune, const char *s)
{
    utf8_decode_t ctx = {UTF8_OK};
    const uint8_t *b = (const uint8_t*)s;
    utf8_decode(&ctx, *b++);
    switch (ctx.len) {
    case 4: utf8_decode(&ctx, *b++);
    case 3: utf8_decode(&ctx, *b++);
    case 2: utf8_decode(&ctx, *b++);
    }
    *rune = ctx.codep;
    return ctx.len;    
}

static const char*
utfrune(const char *s, Rune c)
{
    Rune r;

    if (c < 128)        /* ascii */
        return strchr((char *)s, c);

    for (;;) {
        int n = chartorune(&r, s);
        if (r == c) return s;
        if ((r == 0) | (n == 0)) return NULL;
        s += n;
    }
}

static const char*
utfruneicase(const char *s, Rune c)
{
    Rune r;
    c = utf8_tolower(c);
    for (;;) {
        int n = chartorune(&r, s);
        if (utf8_tolower(r) == c) return s;
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
_renewmatch(Resub *mp, int ms, Resublist *sp, int nsubids)
{
    int i;

    if (mp==NULL || ms<=0)
        return;
    if (mp[0].sp==NULL || sp->m[0].sp<mp[0].sp ||
       (sp->m[0].sp==mp[0].sp && sp->m[0].ep>mp[0].ep)) {
        for (i=0; i<ms && i<=nsubids; i++)
            mp[i] = sp->m[i];
        //for (; i<ms; i++)
        //    mp[i].sp = mp[i].ep = NULL;
    }
}

/*
 * Note optimization in _renewthread:
 *   *lp must be pending when _renewthread called; if *l has been looked
 *   at already, the optimization is a bug.
 */
static Relist*
_renewthread(Relist *lp,  /* _relist to add to */
    Reinst *ip,           /* instruction to add */
    int ms,
    Resublist *sep)       /* pointers to subexpressions */
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
_renewemptythread(Relist *lp,   /* _relist to add to */
    Reinst *ip,                 /* instruction to add */
    int ms,
    const char *sp)             /* pointers to subexpressions */
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
    const char* exprp;   /* pointer to next character in source expression */
    Node andstack[NSTACK];
    Node* andp;
    short atorstack[NSTACK];
    short* atorp;
    short subidstack[NSTACK]; /* parallel to atorstack */
    short* subidp;
    short cursubid;      /* id of current subexpression */
    int errors;
    bool ignorecase;
    bool lastwasand;     /* Last token was operand */
    bool lexdone;
    short nbra;
    short nclass;
    Rune yyrune;         /* last lex'd rune */
    Reclass *yyclassp;   /* last lex'd class */
    Reclass* classp;
    Reinst* freep;
    jmp_buf regkaboom;
} Parser;

/* predeclared crap */
static void _operator(Parser *par, int type);
static void pushand(Parser *par, Reinst *first, Reinst *last);
static void pushator(Parser *par, int type);
static void evaluntil(Parser *par, int type);
static int  bldcclass(Parser *par);

static void
rcerror(Parser *par, cregex_error_t err)
{
    par->errors |= err;
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
        _operator(par, CAT);    /* catenate is implicit */
    i = newinst(par, t);

    if (t == CCLASS || t == NCCLASS)
        i->r.classp = par->yyclassp;
    if (t == RUNE)
        i->r.rune = par->yyrune;

    pushand(par, i, i);
    par->lastwasand = true;
}

static void
_operator(Parser *par, int t)
{
    if (t==RBRA && --par->nbra<0)
        rcerror(par, creg_unmatchedrightparenthesis);
    if (t==LBRA) {
        if (++par->cursubid >= NSUBEXP)
            rcerror(par, creg_toomanysubexpressions);
        par->nbra++;
        if (par->lastwasand)
            _operator(par, CAT);
    } else
        evaluntil(par, t);
    if (t != RBRA)
        pushator(par, t);
    par->lastwasand = 0;
    if (t==STAR || t==QUEST || t==PLUS || t==RBRA)
        par->lastwasand = true;    /* these look like operands */
}

static void
pushand(Parser *par, Reinst *f, Reinst *l)
{
    if (par->andp >= &par->andstack[NSTACK])
        rcerror(par, creg_operandstackoverflow);
    par->andp->first = f;
    par->andp->last = l;
    par->andp++;
}

static void
pushator(Parser *par, int t)
{
    if (par->atorp >= &par->atorstack[NSTACK])
        rcerror(par, creg_operatorstackoverflow);
    *par->atorp++ = t;
    *par->subidp++ = par->cursubid;
}

static Node*
popand(Parser *par, int op)
{
    Reinst *inst;

    if (par->andp <= &par->andstack[0]) {
        rcerror(par, creg_missingoperand);
        inst = newinst(par, NOP);
        pushand(par, inst, inst);
    }
    return --par->andp;
}

static int
popator(Parser *par)
{
    if (par->atorp <= &par->atorstack[0])
        rcerror(par, creg_operatorstackunderflow);
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
            rcerror(par, creg_unknownoperator);
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
    npp = (Reprog *)realloc(pp, size);
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
            inst->r.right = (Reinst *)((char*)inst->r.right + diff);
            break;
        case CCLASS:
        case NCCLASS:
            inst->r.right = (Reinst *)((char*)inst->r.right + diff);
            cl = inst->r.classp;
            cl->end = (Rune *)((char*)cl->end + diff);
            break;
        }
        inst->l.left = (Reinst *)((char*)inst->l.left + diff);
    }
    npp->startinst = (Reinst *)((char*)npp->startinst + diff);
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
        rcerror(par, creg_toomanycharacterclasses);
    return &(par->classp[par->nclass++]);
}

static int
nextc(Parser *par, Rune *rp)
{
    if (par->lexdone) {
        *rp = 0;
        return true;
    }
    par->exprp += chartorune(rp, par->exprp);
    if (*rp == '\\') {
        par->exprp += chartorune(rp, par->exprp);
        switch (*rp) {
            case 't': *rp = '\t'; break;
            case 'n': *rp = '\n'; break;
            case 'r': *rp = '\r'; break;
            case 'v': *rp = '\v'; break;
            case 'f': *rp = '\f'; break;
            case 'd': *rp = CLS_d; break;
            case 'D': *rp = CLS_D; break;
            case 's': *rp = CLS_s; break;
            case 'S': *rp = CLS_S; break;
            case 'w': *rp = CLS_w; break;
            case 'W': *rp = CLS_W; break;
        }      
        return true;
    }
    if (*rp == 0)
        par->lexdone = true;
    return false;
}

static int
lex(Parser *par, int* dot_type)
{
    int quoted;
    start:
    quoted = nextc(par, &par->yyrune);
    if (quoted) {
        if (par->yyrune == 'b')
            return WBOUND;
        if (par->yyrune == 'B')
            return NWBOUND;
        if (par->yyrune != 0)
            return RUNE;
    }

    switch (par->yyrune) {
    case  0 : return END;
    case '*': return STAR;
    case '?': return QUEST;
    case '+': return PLUS;
    case '|': return OR;
    case '.': return *dot_type;
    case '(': 
        if (par->exprp[0] == '?') switch (par->exprp[1]) {
            case 's': if (par->exprp[2] == ')')
                { *dot_type = ANYNL; par->exprp += 3; goto start; }
            case 'i': if (par->exprp[2] == ')')
                { par->ignorecase = true; par->exprp += 3; goto start; }
        }
        return LBRA;
    case ')': return RBRA;
    case '^': return BOL;
    case '$': return EOL;
    case '[': return bldcclass(par);
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
    for (; ep < &r[NCCRUNE]; quoted = nextc(par, &rune)) {
        if (rune == 0) {
            rcerror(par, creg_malformedsquarebrackets);
            return 0;
        }
        if (!quoted) {
            if (rune == ']')
                break;
            if (rune == '-') {
                if (ep == r) {
                    rcerror(par, creg_malformedsquarebrackets);
                    return 0;
                }
                quoted = nextc(par, &rune);
                if ((!quoted && rune == ']') || rune == 0) {
                    rcerror(par, creg_malformedsquarebrackets);
                    return 0;
                }
                *(ep-1) = rune;
                continue;
            }
            if (rune == '[' && *par->exprp == ':') {
                static struct { const char* c; int n, r; } cls[] = {
                    {":alnum:]", 8, CLS_an}, {":alpha:]", 8, CLS_al}, {":blank:]", 8, CLS_bl}, 
                    {":cntrl:]", 8, CLS_ct}, {":digit:]", 8, CLS_d}, {":graph:]", 8, CLS_gr}, 
                    {":lower:]", 8, CLS_lo}, {":print:]", 8, CLS_pr}, {":punct:]", 8, CLS_pu},
                    {":space:]", 8, CLS_s}, {":upper:]", 8, CLS_up}, {":xdigit:]", 9, CLS_xd},
                    {":word:]", 7, CLS_w},
                };
                for (unsigned i = 0; i < (sizeof cls/sizeof *cls); ++i)
                    if (!strncmp(par->exprp, cls[i].c, cls[i].n)) {
                        par->exprp += cls[i].n;
                        *ep++ = cls[i].r;
                        *ep++ = cls[i].r;
                        goto proceed;
                    }
            }
        }
        *ep++ = rune;
        *ep++ = rune;
        proceed:;
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
regcomp1(Parser *par, const char *s, int dot_type)
{
    int token;
    Reprog *volatile pp;

    /* get memory for the program. estimated max usage */
    const int instcap = 5 + 6*strlen(s);
    pp = (Reprog *)malloc(sizeof(Reprog) + instcap*sizeof(Reinst));
    if (pp == NULL) {
        rcerror(par, creg_outofmemory);
        return NULL;
    }
    pp->flags.ignorecase = false;
    pp->flags.dotall = false;
    par->freep = pp->firstinst;
    par->classp = pp->cclass;
    par->errors = 0;

    if (setjmp(par->regkaboom))
        goto out;

    /* go compile the sucker */
    par->lexdone = false;
    par->ignorecase = false;
    par->exprp = s;
    par->nclass = 0;
    par->nbra = 0;
    par->atorp = par->atorstack;
    par->andp = par->andstack;
    par->subidp = par->subidstack;
    par->lastwasand = false;
    par->cursubid = 0;

    /* Start with a low priority operator to prime parser */
    pushator(par, START-1);
    while ((token = lex(par, &dot_type)) != END) {
        if ((token & 0xF0) == OPERATOR)
            _operator(par, token);
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
        rcerror(par, creg_unmatchedleftparenthesis);
    --par->andp;    /* points to first and only operand */
    pp->startinst = par->andp->first;
#ifdef DEBUG
    dump(pp);
#endif
    pp = optimize(par, pp);
    pp->flags.ignorecase |= par->ignorecase;
    pp->nsubids = par->cursubid;
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


/*************
 * regexec.c *
 *************/

static int 
runematch(Rune s, Rune r, bool icase)
{
    int inv = 0;
    switch (s) {
        case CLS_D: inv = true; /* fallthrough */
        case CLS_d: return inv ^ (isdigit(r) != 0);
        case CLS_S: inv = true;
        case CLS_s: return inv ^ (isspace(r) != 0);
        case CLS_W: inv = true;
        case CLS_w: return inv ^ (utf8_isalnum(r) | (r == '_'));
        case CLS_al: return utf8_isalpha(r);
        case CLS_bl: return ((r == ' ') | (r == '\t'));
        case CLS_ct: return iscntrl(r) != 0;
        case CLS_gr: return isgraph(r) != 0;
        case CLS_an: return utf8_isalnum(r);
        case CLS_pr: return isprint(r) != 0;
        case CLS_pu: return ispunct(r) != 0;
        case CLS_xd: return isxdigit(r) != 0;
        case CLS_lo: return icase ? utf8_isalpha(s) : utf8_islower(r);
        case CLS_up: return icase ? utf8_isalpha(s) : utf8_isupper(r);
    }
    return icase ? utf8_tolower(s) == utf8_tolower(r) : s == r;
}

/*
 *  return 0 if no match
 *        >0 if a match
 *        <0 if we ran out of _relist space
 */
static int
regexec1(const Reprog *progp,    /* program to run */
    const char *bol,    /* string to run machine on */
    Resub *mp,    /* subexpression elements */
    int ms,        /* number of elements at mp */
    Reljunk *j,
    int mflags
)
{
    int flag=0;
    Reinst *inst;
    Relist *tlp;
    Relist *tl, *nl;    /* This list, next list */
    Relist *tle, *nle;  /* Ends of this and next list */
    const char *s, *p;
    int i, n, checkstart;
    Rune r, *rp, *ep;
    int match = 0;

    bool icase = progp->flags.ignorecase || (mflags & creg_ignorecase);
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
                p = icase ? utfruneicase(s, j->startchar)
                          : utfrune(s, j->startchar);
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
                int ok = false;

                switch (inst->type) {
                case RUNE:    /* regular character */
                    ok = runematch(inst->r.rune, r, icase);
                    break;
                case LBRA:
                    tlp->se.m[inst->r.subid].sp = s;
                    continue;
                case RBRA:
                    tlp->se.m[inst->r.subid].ep = s;
                    continue;
                case ANY:
                    ok = (r != '\n');
                    break;
                case ANYNL:
                    ok = true;
                    break;
                case BOL:
                    if (s == bol || *(s-1) == '\n')
                        continue;
                    break;
                case EOL:
                    if (s == j->eol || r == 0 || r == '\n')
                        continue;
                    break;
                case NWBOUND:
                    ok = true; /* fallthrough */
                case WBOUND:
                    if (ok ^ (s == bol || s == j->eol || ((isalnum(s[-1]) || s[-1] == '_')
                                                        ^ (isalnum(s[ 0]) || s[ 0] == '_'))))
                        continue;
                    break;
                case NCCLASS:
                    ok = true; /* fallthrough */
                case CCLASS:
                    ep = inst->r.classp->end;
                    for (rp = inst->r.classp->spans; rp < ep; rp += 2)
                        if ((r >= rp[0] && r <= rp[1]) || (rp[0] == rp[1] && runematch(rp[0], r, icase)))
                            break;
                    ok ^= (rp < ep);
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
                        _renewmatch(mp, ms, &tlp->se, progp->nsubids);
                    break;
                }

                if (ok && _renewthread(nl, inst->l.next, ms, &tlp->se) == nle)
                    return -1;
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
    Reljunk *j,
    int mflags
)
{
    int rv;
    Relist *relists;

    /* mark space */
    relists = (Relist *)malloc(2 * BIGLISTSIZE*sizeof(Relist));
    if (relists == NULL)
        return -1;

    j->relist[0] = relists;
    j->relist[1] = relists + BIGLISTSIZE;
    j->reliste[0] = relists + BIGLISTSIZE - 2;
    j->reliste[1] = relists + 2*BIGLISTSIZE - 2;

    rv = regexec1(progp, bol, mp, ms, j, mflags);
    free(relists);
    return rv;
}

static int
regexec9(const Reprog *progp,    /* program to run */
    const char *bol,    /* string to run machine on */
    Resub *mp,    /* subexpression elements */
    int ms,
    int mflags)   /* number of elements at mp */
{
    Reljunk j;
    Relist relist0[LISTSIZE], relist1[LISTSIZE];
    int rv;

    /*
      *  use user-specified starting/ending location if specified
     */
    j.starts = bol;
    j.eol = NULL;
    /*
    if (mp && ms>0) {
        if (mp->sp)
            j.starts = mp->sp;
        if (mp->ep)
            j.eol = mp->ep;
    }
    */
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
    j.reliste[0] = relist0 + LISTSIZE - 2;
    j.reliste[1] = relist1 + LISTSIZE - 2;

    rv = regexec1(progp, bol, mp, ms, &j, mflags);
    if (rv >= 0)
        return rv;
    rv = regexec2(progp, bol, mp, ms, &j, mflags);
    return rv;
}

/************
 * regsub.c *
 ************/

/* substitute into one string using the matches from the last regexec() */
static void
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
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
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

/*
 * API functions
 */

int cregex_compile(cregex_t *rx, const char* pattern, int cflags) {
    Parser par;
    rx->prog = regcomp1(&par, pattern, cflags & creg_dotall ? ANYNL : ANY);
    return par.errors;
}

int cregex_find(const cregex_t *rx, const char* string, 
                size_t nmatch, cregmatch_t match[], int mflags) {
    Resub m[32] = {0};
    int res = regexec9(rx->prog, string, m, nmatch, mflags);
    if (res != 1) return creg_nomatch;

    for (size_t i = 0; m[i].ep && i < nmatch; ++i) {
        match[i].rm_so = m[i].sp - string;
        match[i].rm_eo = m[i].ep - string;
    }
    return creg_ok;
}

void cregex_replace(const char* src, char* dst, int dsize,
                    int nmatch, const cregmatch_t match[]) {
    Resub m[32];
    for (size_t i = 0; i < nmatch; ++i) {
        m[i].sp = src + match[i].rm_so;
        m[i].ep = src + match[i].rm_eo;
    }
    regsub9(src, dst, dsize, m, nmatch);
}

int cregex_subexp_count(cregex_t rx) {
    return rx.prog->nsubids;
}

void cregex_drop(cregex_t* preg) {
    free(preg->prog);
}
