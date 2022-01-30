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
#ifndef PLAN9_REGEXP9_H_
#define PLAN9_REGEXP9_H_
/*
 * regexp9.h
 * 
 * This is a extended version of regexp9, supporting UTF8 input, common 
 * shorthand character classes, ++.
 */

#include <stdint.h>
typedef struct Reprog   Reprog;
typedef struct Resub    Resub;

/*
 *    Sub expression matches
 */
struct Resub {
    const char *sp; /* start pos */
    const char *ep; /* end pos */
};

extern Reprog*  regcomp9(const char* exp);
extern Reprog*  regcompnl9(const char* exp);
extern int      regexec9(const Reprog* prog, const char* string, Resub* match, int msize);
extern void     regsub9(const char* source, char* dest, int, Resub* match, int msize);
extern void     regfree9(Reprog* prog);

// --------------- POSIX similar ------------------

typedef enum {
    creg_ok = 0,
    creg_outofmemory = 1<<0,
    creg_unmatchedleftparenthesis = 1<<1,
    creg_unmatchedrightparenthesis = 1<<2,
    creg_toomanysubexpressions = 1<<3,
    creg_toomanycharacterclasses = 1<<4,
    creg_malformedsquarebrackets = 1<<5,
    creg_missingoperand = 1<<6,
    creg_unknownoperator = 1<<7,
    /* can't happen(?): */
    creg_operandstackoverflow = 1<<28,
    creg_operatorstackoverflow = 1<<29,
    creg_operatorstackunderflow = 1<<30,
} cregex_error_t;

typedef struct {
    Reprog* prog;
} regex_t;

typedef intptr_t regoff_t;
typedef struct {
    regoff_t rm_so;
    regoff_t rm_eo;
} regmatch_t;

enum { REG_EXTENDED = 1, REG_NEWLINE = 2, REG_NOMATCH = 4, REG_NOSUB = 8 };

static inline int regcomp(regex_t *preg, const char* regex, int cflags) {
    preg->prog = cflags & REG_NEWLINE ? regcompnl9(regex)
                                      : regcomp9(regex);
    return preg->prog ? 0 : 1;
}

static inline int regexec(const regex_t *preg, const char* string, 
                          size_t nmatch, regmatch_t pmatch[], int eflags) {
    Resub m[32] = {0};
    int res = regexec9(preg->prog, string, m, nmatch);
    for (size_t i = 0; i < nmatch; ++i) {
        pmatch[i].rm_so = m[i].sp - string;
        pmatch[i].rm_eo = m[i].ep - string;
    }
    return res == 1 ? 0 : REG_NOMATCH;
}

static inline void regsub(const char* src, char* dst, int dsize,
                          int nmatch, const regmatch_t pmatch[]) {
    Resub m[32];
    for (size_t i = 0; i < nmatch; ++i) {
        m[i].sp = src + pmatch[i].rm_so;
        m[i].ep = src + pmatch[i].rm_eo;
    }
    regsub9(src, dst, dsize, m, nmatch);
}

static inline void regfree(regex_t* preg) {
    regfree9(preg->prog);
}

#endif
