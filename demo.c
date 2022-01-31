#include "regexp9.h"
#include <stdio.h>

int main() {

    //const char* pattern = "(?i)(hell.)([ \\t]w.rld)+";
    //const char* input = "Hell😀 W😀rld\tworld wXrld";

    const char * pattern = "(5[1-5]\\d\\d)[\\- ]?(\\d\\d\\d\\d)[\\- ]?"
                           "(\\d\\d\\d\\d)[\\- ]?(\\d\\d\\d\\d)";
    const char * input = "5111 2222-3333-4444";
/*
    const char* pattern = "([[:lower:]]+[., ]*)+";
    const char* input = "Sáhtán borrat lása, dat ii leat bávččas";
*/

    enum {N=5};
    cregmatch_t m[N] = {0};
    int n;
    cregex_t rx;
    int ret = cregex_compile(&rx, pattern, 0);
    printf("ret %d\n", ret);
    while ((n = cregex_find(&rx, input, N, m, creg_fullmatch|creg_next)) > 0)
    {
        printf("`%s` => matched `%s`", input, pattern);
        for (int i=0; i<n; ++i)
            printf(" (%d, %d)", (int)(m[i].str - input), (int)m[i].len);
        puts("");
        for (int i=0; i<n; ++i)
            printf("%d: (%.*s)\n", i, (int)m[i].len, m[i].str);
    }
    cregex_free(&rx);
}
