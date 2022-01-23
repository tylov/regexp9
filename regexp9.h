#ifndef _REGEXP9_H_
#define _REGEXP9_H_ 1

/*************
 * regexp9.h *
 *************/

#if defined(__cplusplus)
extern "C" { 
#endif

typedef struct Resub    Resub;
typedef struct Reprog   Reprog;

/*
 *    Sub expression matches
 */
struct Resub{
    const char *sp;
    const char *ep;
};

extern Reprog   *regcomp9(const char*);
extern Reprog   *regcomplit9(const char*);
extern Reprog   *regcompnl9(const char*);
extern int      regexec9(const Reprog*, const char*, Resub*, int);
extern void     regsub9(const char*, char*, int, Resub*, int);

#if defined(__cplusplus)
}
#endif
#endif
