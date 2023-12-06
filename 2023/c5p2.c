#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>

int
main(int ac, char **al) {
    FILE *fh = NULL;
    uint32_t offset = 0;
    uint8_t state = 0;
    uint8_t buf[2048] = {0};
    uint8_t *pbuf = NULL, *pend = NULL;
    uint64_t iseeds[256] = {0};
    uint64_t temp = 0;
    uint8_t seedcap = 0;
    uint64_t min = INT_MAX;
    uint64_t seed2soil_start[256] = {0};
    uint64_t seed2soil_dest[256] = {0};
    uint64_t seed2soil_rle[256] = {0};
    uint8_t seed2soil_cap = 0;
    uint64_t soil2fert_start[256] = {0};
    uint64_t soil2fert_dest[256] = {0};
    uint64_t soil2fert_rle[256] = {0};
    uint8_t soil2fert_cap = 0;
    uint64_t fert2water_start[256] = {0};
    uint64_t fert2water_dest[256] = {0};
    uint64_t fert2water_rle[256] = {0};
    uint8_t fert2water_cap = 0;
    uint64_t water2light_start[256] = {0};
    uint64_t water2light_dest[256] = {0};
    uint64_t water2light_rle[256] = {0};
    uint8_t water2light_cap = 0;
    uint64_t light2temp_start[256] = {0};
    uint64_t light2temp_dest[256] = {0};
    uint64_t light2temp_rle[256] = {0};
    uint8_t light2temp_cap = 0;
    uint64_t temp2humid_start[256] = {0};
    uint64_t temp2humid_dest[256] = {0};
    uint64_t temp2humid_rle[256] = {0};
    uint8_t temp2humid_cap = 0;
    uint64_t humid2loc_start[256] = {0};
    uint64_t humid2loc_dest[256] = {0};
    uint64_t humid2loc_rle[256] = {0};
    uint8_t humid2loc_cap = 0;

    if(ac != 2) {
        printf("Usage: c3p2 [file]\n");
        return 1;
    }

    fh = fopen(al[1], "r");
    if(fh == NULL) {
        printf("cannot open file: %s\n", al[1]);
        return 2;
    }
    
    if(fgets(buf, 2048, fh) == NULL) {
        printf("empty file: initial read empty.\n");
        return 3;
    }
  
    offset = 7;
    pbuf = &buf[7];
    //printf("buffer: %s\n", buf);
    while(1) {
        //printf("%x\n", pbuf[0]);
        temp = strtoll(pbuf, &pend, 10);
        //printf("read: %l\n", temp);
        iseeds[seedcap] = temp;
        seedcap++;
        pbuf = pend;
        if(pend[0] == '\0' || pend[0] == '\n' || pend[0] == '\r') {
            break;
        }
    }

    for(int i = 0; i < seedcap; i++) {
        printf("%llu\n", iseeds[i]);
    }
     
    while(fgets(buf, 2048, fh)) {
        if((buf == NULL) || feof(fh)){
            break;
        }
        if(strnlen(buf, 2048) == 0) {
            // blank line
            continue;
        } else if(strstr(buf, "map:") != NULL) {
            if(!strncmp("seed-to-soil", buf, 12)) {
                state = 1;
            } else if(!strncmp("soil-to-fertilizer", buf, 18)) {
                state = 2;
            } else if(!strncmp("fertilizer-to-water", buf, 19)) {
                state = 3;
            } else if(!strncmp("water-to-light", buf, 14)) {
                state = 4;
            } else if(!strncmp("light-to-temperature", buf, 20)) {
                state = 5;
            } else if(!strncmp("temperature-to-humidity", buf, 23)) {
                state = 6;
            } else if(!strncmp("humidity-to-location", buf, 20)) {
                state = 7;
            }
        } else {
            if(state == 1) {
                pbuf = &buf[0];
                seed2soil_dest[seed2soil_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                seed2soil_start[seed2soil_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                seed2soil_rle[seed2soil_cap] = strtoll(pbuf, &pend, 10);
                seed2soil_cap++;
            } else if(state == 2) {
                pbuf = &buf[0];
                soil2fert_dest[soil2fert_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                soil2fert_start[soil2fert_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                soil2fert_rle[soil2fert_cap] = strtoll(pbuf, &pend, 10);
                soil2fert_cap++;
            } else if(state == 3) {
                pbuf = &buf[0];
                fert2water_dest[fert2water_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                fert2water_start[fert2water_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                fert2water_rle[fert2water_cap] = strtoll(pbuf, &pend, 10);
                fert2water_cap++;
            } else if(state == 4) {
                pbuf = &buf[0];
                water2light_dest[water2light_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                water2light_start[water2light_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                water2light_rle[water2light_cap] = strtoll(pbuf, &pend, 10);
                water2light_cap++;
            } else if(state == 5) {
                pbuf = &buf[0];
                light2temp_dest[light2temp_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                light2temp_start[light2temp_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                light2temp_rle[light2temp_cap] = strtoll(pbuf, &pend, 10);
                light2temp_cap++;
            } else if(state == 6) {
                pbuf = &buf[0];
                temp2humid_dest[temp2humid_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                temp2humid_start[temp2humid_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                temp2humid_rle[temp2humid_cap] = strtoll(pbuf, &pend, 10);
                temp2humid_cap++;
            } else if(state == 7) {
                pbuf = &buf[0];
                humid2loc_dest[humid2loc_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                humid2loc_start[humid2loc_cap] = strtoll(pbuf, &pend, 10);
                pbuf = pend;
                humid2loc_rle[humid2loc_cap] = strtoll(pbuf, &pend, 10);
                humid2loc_cap++;
            }
        }
    }
    printf("soil2fert_cap: %d\nfert2water_cap: %d\nhumid2loc_cap: %d\n", soil2fert_cap, fert2water_cap, humid2loc_cap);
    fclose(fh);
    /*
    sections = [ "seed-to-soil", "soil-to-fertilizer",
        "fertilizer-to-water", "water-to-light",
        "light-to-temperature", "temperature-to-humidity",
        "humidity-to-location"]
    minseed = sys.maxsize
    # we honestly could have done these in order above...
    for seedidx in range(0, len(iseeds), 2):
        start = iseeds[seedidx]
        count = iseeds[seedidx + 1]
        print("on range", seedidx, "out of", len(iseeds))
        seed = start
        for seed in range(start, start + count):
            for section in sections:
                #print("\tseed: ", seed)
                #print("\tmappings:", section,":", mappings[section])
                for source_idx in range(0, len(mappings[section]["source"])):
                    source = mappings[section]["source"][source_idx]
                    source_end = mappings[section]["source-end"][source_idx]
                    if seed >= source and seed < source_end:
                        delta = seed - source
                        nseed = mappings[section]["destination"][source_idx] + delta
                        #print("\t\tmapping:", seed, delta, nseed)
                        seed = nseed
                        break
            if seed < minseed:
                minseed = seed
    print(minseed)
    */
    for(int idx = 0; idx < seedcap; idx += 2) {
        state = 1;
        uint64_t start = iseeds[idx];
        uint64_t count = iseeds[idx + 1];
        uint64_t max = start + count;
        printf("on range %d out of %d: %ld -> %ld\n", idx, seedcap, start, max);
        for(uint64_t seed = start; seed < max; seed++) {
            if(seed == 0 || ((seed % 10000000) == 0)) {
                printf("seed: %ld\n", seed);
            }
            uint64_t nseed = seed;

            for(uint64_t sectidx = 0; sectidx < seed2soil_cap; sectidx++) {
                uint64_t source_check = seed2soil_start[sectidx];
                uint64_t end_check = source_check + seed2soil_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = seed2soil_dest[sectidx] + delta;
                    break;
                 }
            }
            for(uint64_t sectidx = 0; sectidx < soil2fert_cap; sectidx++) {
                uint64_t source_check = soil2fert_start[sectidx];
                uint64_t end_check = source_check + soil2fert_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = soil2fert_dest[sectidx] + delta;
                    break;
                 }
            }
            for(uint64_t sectidx = 0; sectidx < fert2water_cap; sectidx++) {
                uint64_t source_check = fert2water_start[sectidx];
                uint64_t end_check = source_check + fert2water_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = fert2water_dest[sectidx] + delta;
                    break;
                 }
            }
            for(uint64_t sectidx = 0; sectidx < water2light_cap; sectidx++) {
                uint64_t source_check = water2light_start[sectidx];
                uint64_t end_check = source_check + water2light_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = water2light_dest[sectidx] + delta;
                    break;
                 }
            }
            for(uint64_t sectidx = 0; sectidx < light2temp_cap; sectidx++) {
                uint64_t source_check = light2temp_start[sectidx];
                uint64_t end_check = source_check + light2temp_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = light2temp_dest[sectidx] + delta;
                    break;
                 }
            }
            for(uint64_t sectidx = 0; sectidx < temp2humid_cap; sectidx++) {
                uint64_t source_check = temp2humid_start[sectidx];
                uint64_t end_check = source_check + temp2humid_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = temp2humid_dest[sectidx] + delta;
                    break;
                 }
            }
            for(uint64_t sectidx = 0; sectidx < humid2loc_cap; sectidx++) {
                uint64_t source_check = humid2loc_start[sectidx];
                uint64_t end_check = source_check + humid2loc_rle[sectidx];
                if(nseed >= source_check && nseed < end_check) {
                    uint64_t delta = nseed - source_check;
                    nseed = humid2loc_dest[sectidx] + delta;
                    break;
                 }
            } 
            if(nseed < min) {
                min = nseed;
            }
        }
    }
    printf("%llu\n", min);
    return 0;
}
