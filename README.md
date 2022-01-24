# regexp9
Regular expressions based on Plan9 code.

## Changes from original code by Rob Pike
- Fully supports **UTF8** (transformed old Rune-type).
- Made **reentrant**: moved global parser variables to stack.
- Added support for escaped ctrl chars in expressions: tab, newline etc. `\t \n \r \v \f`
- Added support for shorthand character classes and inverse: `\d \D \s \S \w \W`
- Added support for POSIX char classes `[:alnum:] [:alpha:] [:blank:] [:cntrl:] [:digit:] [:graph:] [:lower:] [:print:] [:punct:] [:space:] [:upper:] [:xdigit:] [:d:] [:s:] [:w:]`
- Added support for word boundary meta character and inverse: `\b \B`
- Removed obsolete *rregexec9()* and *rregsub9()*, and the rather pointless *regcomplit9()*.
- Constified (const char*) references to input strings.
- Formatting changes: tabs to space, etc.
- Optimizations: malloc usage and shorter code. Fast UTF8 code.
- Compiles with C99, C++.
- Reduced total source code size from about 1600 to 1200 lines.

## Example
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
    regfree9(p);
}
```
