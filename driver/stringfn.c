#include <stdio.h>

extern char *stringfn(void);

int main(int argc, char **argv) {
        printf("%s\n", stringfn());
        return 0;
}
