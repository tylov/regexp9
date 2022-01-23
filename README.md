# regexp9
Regular expressions based on Plan9 code.

## Changes from original code by Rob Pike
- Fully supports UTF8 (redefined old Rune-type).
- Made reentrant: moved global parser variables to stack.
- Added support for character classes (shorthands only): \a \A \w \W \s \S \d \D \x \X \c \C \p \P \l \u
- Added support for word boundary meta character: \b
- Added support for escaped ctrl chars in expressions: tab, newline etc. \t, \n, \r, \v, \f
- Removed obsolete rregexec9() and rregsub9(), and the rather pointless regcomplit9().
- Constified (const char*) references to input strings.
- Formatting changes: tabs to space, etc.
- Optimizations: malloc usage and shorter code. Fast UTF8 code.
- Compiles with C99, C++.
- Reduced source code size from about 1600 to 1200 lines.

## Examples
```c
#include "regexp9.h"
#include <stdio.h>

int main() {
    const char* pattern = "hell.([ \\t]w.rld)+";
    const char* input = "hell😀 w😀rld\tworld wxrld";

    enum {N=5};
    Resub rs[N] = {0};

    Reprog *p = regcomp9(pattern);
    if (regexec9(p, input, rs, N))
        printf("regexp9: '%s' => matched: %s\n", input, pattern);
    else
        printf("regexp9: No match\n");
    free(p);
}
```
