#include <stdio.h>
#include <stdbool.h>
#include <strings.h>
#include <ctype.h>

int
main(int ac, char **al) {
    char buf[2048] = {0};
    FILE *fh = NULL;
    unsigned int start = 0, end = 0, val = 0, sum = 0;
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
        start = 0;
        end = strnlen(buf, 2048);
        while(1) {
            printf("\t%d,%d\n", start, end);

            if(isdigit(buf[start]) && isdigit(buf[end])) {
                val += ((buf[start] - '0') * 10);
                val += (buf[end] - '0'); 
                break;
            }

            if(!isdigit(buf[start])) {
                start++;
            }

            if(!isdigit(buf[end])) {
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

