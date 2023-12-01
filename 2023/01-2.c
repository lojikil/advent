#include <stdio.h>
#include <stdbool.h>
#include <strings.h>
#include <ctype.h>

int
findword(char *line, int offset) {
    if(!strncmp("one", &line[offset], 3)) {
        return 1;
    } else if(!strncmp("zero", &line[offset], 4)) {
        return 0;
    } else if(!strncmp("two", &line[offset], 3)) {
        return 2;
    } else if(!strncmp("three", &line[offset], 5)) {
        return 3;
    } else if(!strncmp("four", &line[offset], 4)) {
        return 4;
    } else if(!strncmp("five", &line[offset], 4)) {
        return 5;
    } else if(!strncmp("six", &line[offset], 3)) {
        return 6;
    } else if(!strncmp("seven", &line[offset], 5)) {
        return 7;
    } else if(!strncmp("eight", &line[offset], 5)) {
        return 8;
    } else if(!strncmp("nine", &line[offset], 4)) {
        return 9;
    } else {
        return -1;
    }
}

int
main(int ac, char **al) {
    char buf[2048] = {0};
    FILE *fh = NULL;
    unsigned int start = 0, end = 0, val = 0, sum = 0;
    unsigned int top = 0, bottom = 0;
    bool sstop = false, estop = false;

    if(ac != 2) {
        printf("usage %s [file]\n", al[0]);
        return 1;
    }

    if((fh = fopen(al[1], "r")) == NULL) {
        printf("cannot open file %s\n", al[1]);
        return 2;
    }

    while(fgets(buf, 2048, fh) != NULL) {
        sstop = false;
        estop = false;
        val = 0;
        top = 0;
        bottom = 0;
        start = 0;
        end = strnlen(buf, 2048);
        while(1) {
            printf("\t%d,%d\n", start, end);

            if(sstop && estop) {
                val = (top * 10) + bottom;
                break;
            }

            if(isdigit(buf[start])) {
                top = (buf[start] - '0');
                sstop = true;
            } else if(findword(buf, start) >= 0) {
                top = findword(buf, start);
                sstop = true;
            } else {
                start++;
            }

            if(isdigit(buf[end])) {
                bottom = (buf[end] - '0');
                estop = true;
            } else if(findword(buf, end) >= 0) {
                bottom = findword(buf, end);
                estop = true;
            } else {
                end--;
            }

            if(end < start) {
                break;
            } 

        }
        printf("val for the line: %d\n", val);
        sum += val;
    }
    printf("%d\n", sum);
    fclose(fh);
    return 0;
}

