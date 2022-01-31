#include "regexp9.h"
#include <stdio.h>

int main() {

    const char* pattern = "(?i)(hell.)([ \\t]w.rld)+";
    const char* input = "HELL😀 w😀rld\tworld wxrld";
/*
    const char* pattern = "([[:lower:]]+[., ]*)+";
    const char* input = "Sáhtán borrat lása, dat ii leat bávččas";
*/

    enum {N=5};
    cregmatch_t m[N] = {0};

    cregex_t rx = cregex_new(pattern, 0);
    int n = cregex_find(&rx, input, N, m, 0);
    if (n > 0) {
        printf("`%s` => matched `%s`", input, pattern);
        for (int i=0; i<n; ++i)
            printf(" (%d, %d)", m[i].rm_so, m[i].rm_eo);
        puts("");
        for (int i=0; i<n; ++i)
            printf("%d: (%.*s)\n", i, (int)(m[i].rm_eo - m[i].rm_so), input+m[i].rm_so);
    }
    else
        printf("No match\n");
    cregex_drop(&rx);
}
