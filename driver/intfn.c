#include <stdio.h>

extern int intfn(void);

int main(int argc, char **argv) {
        printf("%d\n", intfn());
        return 0;
}
