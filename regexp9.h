#ifndef _REGEXP9_H_
#define _REGEXP9_H_ 1

/*************
 * regexp9.h *
 *************/

#if defined(__cplusplus)
extern "C" { 
#endif

typedef unsigned short  Rune;
typedef struct Resub    Resub;
typedef struct Reprog   Reprog;

/*
 *    Sub expression matches
 */
struct Resub{
    union
    {
        char *sp;
        Rune *rsp;
    }s;
    union
    {
        char *ep;
        Rune *rep;
    }e;
};

extern Reprog   *regcomp9(char*);
extern Reprog   *regcomplit9(char*);
extern Reprog   *regcompnl9(char*);
extern int      regexec9(const Reprog*, char*, Resub*, int);
extern void     regsub9(char*, char*, int, Resub*, int);

#if defined(__cplusplus)
}
#endif
#endif
