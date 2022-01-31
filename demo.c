#include "regexp9.h"
#include <stdio.h>

int main() {
    const char* pattern = "(?i)(hell.)([ \\t]w.rld)+";
    const char* input = "HELL😀 w😀rld\tworld wxrld";

    enum {N=5};
    cregmatch_t m[N] = {0};

    cregex_t rx = cregex_new(pattern, 0);
    if (cregex_find(&rx, input, N, m, 0) == creg_ok) {
        printf("regexp9: `%s` => matched `%s`", input, pattern);
        for (int i=0; i<=cregex_subexp_count(rx); ++i)
            printf(" (%d, %d)", m[i].rm_so, m[i].rm_eo);
        puts("");
    }
    else
        printf("regexp9: No match\n");
    cregex_drop(&rx);
}
