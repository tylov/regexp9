#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "regexp9.h"

#define BUF_SIZE 0x10000

int main(int argc, char *argv[])
{
    enum {N=16};
    cregmatch_t m[N];
    cregex_t rx;
    char *buf, *q; 
    const char *pos;
    int l = 0, n;
    if (argc == 1) {
        fprintf(stderr, "Usage: cat in.file | %s <regexp>\n", argv[0]);
        return 0;
    }
    int res = cregex_compile(&rx, argv[1], 0);
    if (res <= 0) return res;
    buf = (char *)calloc(BUF_SIZE, 1);
    while (fgets(buf, BUF_SIZE - 1, stdin)) {
        ++l;
        for (q = buf; *q; ++q);
        if (q > buf) *(q-1) = 0;
        pos = buf;
        while ((n = cregex_find(&rx, pos, N, m, 0)) > 0) {
            printf("%d:", l);
            for (int i=0; i<n; ++i) printf("%.*s|", (int)(m[i].rm_eo - m[i].rm_so), pos+m[i].rm_so);
            puts("");
            pos = buf + m[0].rm_eo;
            m[0].rm_so = m[0].rm_eo = 0;
        }
    }
    free(buf);
    cregex_drop(&rx);
}
