sections = ['seed-to-soil', 'soil-to-fertilizer', 'fertilizer-to-water', 'water-to-light', 'light-to-temperature', 'temperature-to-humidity', 'humidity-to-location']
for s in sections:
    print("else if(!strncmp(\"{0}\", buf, {1})) {{".format(s, len(s)))

short = lambda x: x.replace('-to-','2').replace('ilizer', '').replace('erature', '').replace('ity', '').replace('ation','')

for i in range(0, len(sections)):
    print("else if(state == {0}) {{\npbuf = &buf[0];".format(i + 1))
    s = short(sections[i])
    print("{0}_start[{0}_cap] = strtoll(pbuf, &pend, 10);".format(s))
    print("pbuf = pend;\n{0}_dest[{0}_cap] = strtoll(pbuf, &pend, 10);".format(s))
    print("pbuf = pend;\n{0}_rle[{0}_cap] = strtoll(pbuf, &pend, 10);".format(s))
    print("{0}_cap++;\n}}".format(s))

for i in range(0, len(sections)):
    s = short(sections[i])
    print("for(uint64_t sectidx = 0; sectidx < {0}_cap; sectidx++) {{".format(s))
    print("    source_check = {0}_start[sectidx];".format(s))
    print("    end_check = source_check + {0}_rle[sectidx];".format(s))
    print("    if(seed >= source_check && seed < end_check) {")
    print("        uint64_t delta = seed - source_check;")
    print("        uint64_t nseed = {0}_dest[sectidx] + delta;".format(s))
    print("        seed = nseed;")
    print("        break;")
    print("     }")
    print("}")

