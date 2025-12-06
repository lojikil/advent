#include<stdint.h>
#include<stdio.h>
#include<string.h>
#include<strings.h>

#define nil NULL
#define nul '\0'

uint64_t
analyze(uint64_t i) {
    char iterbuf[64] = {0}, *fp = nil, *sp = nil;
    uint32_t midpoint = 0, l = 0, eq = 0, idx = 0;
    snprintf(iterbuf, 64, "%ld", i);
    l = strnlen(iterbuf, 64);

    if((l % 2) == 1) {
        return 0;
    }

    midpoint = l / 2;
    fp = &iterbuf[0];
    sp = &iterbuf[midpoint];
    while(midpoint < l && *sp != nul) {
        eq = *fp == *sp;
        if(!eq) {
            break;
        }
        fp++;
        sp++;
    }
    return eq;
}

uint64_t
analyze2(uint64_t i) {
    char iterbuf[64] = {0}, *fp = nil, *sp = nil, mbsrc[32] = {0}, mbdst[32] = {0};
    uint32_t midpoint = 0, l = 0, eq = 0, idx = 1;
    snprintf(iterbuf, 64, "%ld", i);
    l = strnlen(iterbuf, 64);

    midpoint = l / 2;
    //printf("p2:top level: %ld\n", i);
    while(idx < midpoint + 1) {
        //printf("p2:buffer size:%d\n", idx);
        strncpy(mbsrc, &iterbuf[0], idx);
        strncpy(mbdst, &iterbuf[idx], idx);
        //printf("p2:comparing: %s and %s\n", mbsrc, mbdst);
        if(!strncmp(mbsrc, mbdst, idx)) {
            //printf("p2:analyze: %s == %s\n", mbsrc, mbdst);
            for(int subidx = idx + idx; subidx < l; subidx += idx) {
                strncpy(mbdst, &iterbuf[subidx], idx);
                //printf("p2:sub_analyze: %s == %s\n", mbsrc, mbdst);
                if(strncmp(mbsrc, mbdst, idx) != 0) {
                    //printf("p2:sub_analyze:breaking, ne\n");
                    eq = 0;
                    break;
                } else {
                    eq = 1;
                }
            }
            if(eq) {
                break;
            }
        }
        idx++;
    }
    return eq;
}

int
main(int ac, char **al) {
    FILE *fdin = nil;
    char buf[8192] = {0}, *bp = &buf[0], *cur = nil, iterbuf[64] = {0};
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
    if(fgets(buf, 8192, fdin) <= 0) {
        fprintf(stderr, "cannot read from file\n");
        return 2;
    }

    while((cur = strsep(&bp, ",")) != nil) {
        //printf("we got range: %s\n", cur);
        sscanf(cur, "%ld-%ld", &start, &end);
        //printf("range has length: %ld\n", end - start);
        for(; start <= end; start++) {
            if(analyze(start)) {
                //printf("p1:adding %ld\n", start);
                accum += start;
                accum2 += start;
                continue;
            }
            if(analyze2(start)) {
                //printf("p2:adding %ld\n", start);
                accum2 += start;
            }
        }
    }

    printf("p1: %ld\np2: %ld\n", accum, accum2);

    return 0;
}
