#include<stdint.h>
#include<stdio.h>
#include<string.h>
#include<strings.h>

#define nil NULL
#define nul '\0'

int
main(int ac, char **al) {
    FILE *fdin = nil;
    char buf[512] = {0};
    uint64_t db[256][2], dp = 0, idx = 0, item = 0, accum = 0;
    uint64_t odb[256][2], op = 0, s = 0, e = 0;
        
    if(ac > 1) {
        fdin = fopen(al[1], "r");
        if(!fdin) {
            fprintf(stderr, "cannot open file \"%s\"\n", al[1]);
            return 1;
        }
    } else {
        fdin = stdin;
    }

    while(1) {
        if(feof(fdin)) {
            break;
        }
        if(fgets(buf, 512, fdin) <= 0) {
            fprintf(stderr, "cannot read from file\n");
            break;
        }

        if(strchr(buf, '-') != nil) {
            sscanf(buf, "%ld-%ld", &db[dp][0], &db[dp][1]);
            printf("we split: %ld and %ld\n", db[dp][0], db[dp][1]);
            dp++;
        } else if(buf[0] == '\n') {
            continue;
        } else {
            sscanf(buf, "%ld", &item);
            printf("we scanned an item: %ld\n", item);
            for(idx = 0; idx < dp; idx++) {
                if(item >= db[idx][0] && item <= db[idx][1]) {
                    accum += 1;
                    break;
                }
            }
        }
    }

    printf("p1: %ld\n", accum);
    accum = 0;

    /*
    // tempting, but wrong; we need to find the overlapping ranges
    for(int idx = 0; idx < dp; idx++ ){
        accum += (db[idx][1] - db[idx][0]); 
    }

    printf("p2: %ld\n", accum);
    */

    /** ok...
     * I think my approach should be
     *
     * . sort the ranges
     * . find overlaps
     *
     * wait no... maybe what we do is...
     *
     * . take the current item
     * . scan over the rest of the database
     * .. if there is an overlap, take those parameters, remove them from the list
     * .. continue scanning until no overlap
     * .. add that overlap to the list
     * . scan over the overlap list, and calculate the sizes
     *
     * can even make it slightly simpler
     *
     * . take the current item
     * . scan over the rest of the database
     * . if there is overlap, take those parameters
     * . check the overlap database
     * .. if we overlap there, update the entry
     * . done
     */
    for(int idx = 0; idx < dp; idx++) {
        s = db[idx][0];
        e = db[idx][1];
    }
    return 0;
}
