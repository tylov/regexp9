#ifndef _REGEXP9_H_
#define _REGEXP9_H_ 1

/*************
 * regexp9.h *
 *************/

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
extern Reprog*  regcomplit9(const char* exp);
extern Reprog*  regcompnl9(const char* exp);
extern int      regexec9(const Reprog* prog, const char* string, Resub* match, int msize);
extern void     regsub9(const char* source, char* dest, int, Resub* match, int msize);
extern void     regerror9(const char* msg);

#endif
