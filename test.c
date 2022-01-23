#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "regexp9.h"

#define BUF_SIZE 0x10000

int main(int argc, char *argv[])
{
    enum {N=4};
    Resub rs[N];
    Reprog *p;
    char *buf, *q; 
    const char *pos;
    int l = 0;
    if (argc == 1) {
        fprintf(stderr, "Usage: cat in.file | %s <regexp>\n", argv[0]);
        return 0;
    }
    p = regcomp9(argv[1]);
    buf = (char *)calloc(BUF_SIZE, 1);
    while (fgets(buf, BUF_SIZE - 1, stdin)) {
        ++l;
        for (q = buf; *q; ++q);
        if (q > buf) *(q-1) = 0;
        memset(rs, 0, sizeof(Resub));
        pos = buf;
        while (regexec9(p, pos, rs, N) > 0) {
            printf("%d:", l);
            for (int i=0; i<N; ++i) printf("%.*s|", (int)(rs[i].ep - rs[i].sp), rs[i].sp);
            puts("");
            pos = rs[0].ep;
            rs[0].sp = rs[0].ep = 0;
        }
    }
    free(buf);
    free(p);
}
