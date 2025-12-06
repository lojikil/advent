#include<stdint.h>
#include<stdio.h>
#include<string.h>
#include<strings.h>

#define nil NULL
#define nul '\0'

int
main(int ac, char **al) {
    FILE *fdin = nil;

    if(ac > 1) {
        fdin = fopen(al[1], "r");
        if(!fdin) {
            fprintf(stderr, "cannot open file \"%s\"\n", al[1]);
            return 1;
        }
    } else {
        fdin = stdin;
    }
    if(fgets(buf, 8192, fdin) <= 0) {
        fprintf(stderr, "cannot read from file\n");
        return 2;
    }

    return 0;
}
