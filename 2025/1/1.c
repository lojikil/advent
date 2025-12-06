#include<stdio.h>
#include<stdint.h>

#define nil NULL
#define nul '\0'

uint32_t
clamp_dir(uint32_t accum, char dir, uint32_t rot, uint32_t *az) {
    uint32_t ret = 0;
    if(dir == 'L') {
        if(accum >= rot) {
            ret = accum - rot;
        } else {
            *az += 1;
            ret = clamp_dir(100, dir, rot - accum, az);
        }
    } else {
        if((accum + rot) >= 100) {
            *az += 1;
            ret = clamp_dir(0, dir, (accum + rot) - 100, az);
        } else {
            ret = accum + rot;
        }
    }
    if(ret == 0) {
        *az += 1;
    }
    return ret;
}

int
main(int ac, char **al) {
    FILE *fdin = nil;
    char dir = ' ', line[256] = {0};
    uint32_t rot = 0, accum = 50, zc = 0, az = 0;
    if(ac == 1) {
        fdin = stdin;
    } else {
        fdin = fopen(al[1], "r");
        if(!fdin) {
            fprintf(stderr, "cannot open file: \"%s\"\n", al[1]);
            return 1;
        }
    }

    while(1) {
        if(!fgets(line, 256, fdin)) {
            break;
        }
        if(sscanf(line, "%c%d", &dir, &rot) <= 0) {
            printf("here? %d\n", __LINE__);
            break;
        }
        if(feof(fdin)) {
            break;
        }
        printf("direction: %c, rotation: %d\n", dir, rot);

        if(dir == 'L') {
            accum = clamp_dir(accum, dir, rot, &az);
        } else {
            accum = clamp_dir(accum, dir, rot, &az);
        }

        printf("pointing at: %d\n", accum);

        if(accum == 0) {
            zc += 1;
        }
    }
    fclose(fdin);
    printf("accum: %d\nzc: %d\naz: %d\n----\np1: %d\np2: %d\n", accum, zc, az, zc, az + zc);
    return 0;
}
