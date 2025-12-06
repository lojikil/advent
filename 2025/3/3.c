#include<stdio.h>
#include<stdint.h>
#include<string.h>

#define nil NULL
#define nul '\0'

uint32_t
atoj(char a, char b) {
    return ((a - '0') * 10) + (b - '0');
}

uint32_t
joltage(char *line, size_t len) {
    uint32_t start = 0, cur_max = 0, tmp = 0;
    for(; start < (len - 1); start++) {
        for(int chase = start + 1; chase < (len - 1); chase++) {
            tmp = atoj(line[start], line[chase]);
            if(tmp > cur_max) {
                cur_max = tmp;
            }
        }
    }
    printf("p1:joltage: %d\n", cur_max);
    return cur_max;
}

int
main(int ac, char **al) {
    FILE *fdin = nil;
    char buf[8192] = {0};
    uint64_t start = 0, end = 0, accum = 0, accum2 = 0;

    if(ac > 1) {
        fdin = fopen(al[1], "r");
        if(!fdin) {
            fprintf(stderr, "cannot open file \"%s\"\n", al[1]);
            return 1;
        }
    } else {
        fdin = stdin;
    }

    while(!feof(fdin)) {
        fgets(buf, 8192, fdin);
        if(feof(fdin)) {
            break;
        }
        accum += joltage(buf, strnlen(buf, 8192));
    }

    printf("p1: %ld\n", accum);
    fclose(fdin);
    return 0;
}
