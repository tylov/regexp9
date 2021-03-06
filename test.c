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
            for (int i=0; i<n; ++i) printf("(%.*s) ", (int)m[i].len, m[i].str);
            puts("");
            pos = m[0].str + m[0].len;
            m[0].str = NULL; m[0].len = 0;
        }
    }
    free(buf);
    cregex_free(&rx);
}
