let iter_channel = (s:in_channel) => {
    Stream.from((_) => {
        switch(input_line(s)) {
            | f => Some(f)
            | exception End_of_file => None
        }
    })
}

let array_every = (fn: ('a => bool), a:array('a)):bool => {
    let rec inner = (idx:int):bool => {
        if(idx >= Array.length(a)) {
            true
        } else if(fn(Array.get(a, idx))) {
            inner(idx + 1)
        } else {
            false
        }
    }
    inner(0)
}

type packet = 
    | LiteralValue(int, int)
    | OperatorFifteen(int, int, int, list(int)) // version, operator, 15bit, subpackets
    | OperatorEleven(int, int, int, list(int)) // version, operator, 11bit, subpackets

let bitstring_of_string = (s:string):string => {
    let res = ref([])
    // doing it this way since there is no String.fold
    // lambda
    String.iter((x) => {
        switch(x) {
            | '0' => {
                res := List.append(res^, ["0000"])
            }
            | '1' => {
                res := List.append(res^, ["0001"])
            }
            | '2' => {
                res := List.append(res^, ["0010"])
            }
            | '3' => {
                res := List.append(res^, ["0011"])
            }
            | '4' => {
                res := List.append(res^, ["0100"])
            }
            | '5' => {
                res := List.append(res^, ["0101"])
            }
            | '6' => {
                res := List.append(res^, ["0110"])
            }
            | '7' => {
                res := List.append(res^, ["0111"])
            }
            | '8' => {
                res := List.append(res^, ["1000"])
            }
            | '9' => {
                res := List.append(res^, ["1001"])
            }
            | 'A' => {
                res := List.append(res^, ["1010"])
            }
            | 'B' => {
                res := List.append(res^, ["1011"])
            }
            | 'C' => {
                res := List.append(res^, ["1100"])
            }
            | 'D' => {
                res := List.append(res^, ["1101"])
            }
            | 'E' => {
                res := List.append(res^, ["1110"])
            }
            | 'F' => {
                res := List.append(res^, ["1111"])
            }
            | _ => {
                ()
            }
        }
    }, s)
    String.concat("", res^)
}

let version_of_bitstream = (src:string, offset:int):int => {
    ((4 * Int.abs(Char.compare('0', String.get(src, offset)))) +
     (2 * Int.abs(Char.compare('0', String.get(src, offset + 1)))) +
     (1 * Int.abs(Char.compare('0', String.get(src, offset + 2)))))
}

let type_of_bitstream = (src:string, offset:int):int => {
    ((4 * Int.abs(Char.compare('0', String.get(src, offset + 3)))) +
     (2 * Int.abs(Char.compare('0', String.get(src, offset + 4)))) +
     (1 * Int.abs(Char.compare('0', String.get(src, offset + 5)))))
}

let length_of_bitstream = (src:string, offset:int, cnt:int):int => {
    let res = ref(0)
    for(idx in 0 to cnt - 1) {
        let tmp = Int.abs(Char.compare('0', String.get(src, offset + 7 + idx)))
        res := res^ + (tmp * (1 lsl (cnt - idx - 1)))
    }
    res^
}

let length_of_literal = (src:string, offset:int):int => {
    let cont = ref(true)
    //let res = ref(0)
    let ioffset = ref(offset + 6)
    while(cont^) {
        if(String.get(src, ioffset^) == '0') {
            cont := false
        } else {
            ()
        }
        ioffset := ioffset^ + 5
    };
    (ioffset^) - offset
}

/*

Literal values (type ID 4) represent a single number as described above. The remaining type IDs are more interesting:

Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.

*/

let rec packet_of_bitstream = (src:string, offset:int) => {
    let version = version_of_bitstream(src, offset)
    let typeid = type_of_bitstream(src, offset)
    print_endline("version: " ++ string_of_int(version))
    switch(typeid) {
        | 4 => {
            print_endline("literal packet, length: " ++ string_of_int(length_of_literal(src, offset)))
            length_of_literal(src, offset)
        }
        | _ when (String.get(src, 6 + offset) == '0') => {
            let cnt = length_of_bitstream(src, offset, 15)
            let ioffset = ref(offset + 22) 
            print_endline("operator packet 15 bit length packet: " ++ string_of_int(cnt))
            while(ioffset^ < (offset + 22 + cnt)) {
                ioffset := ioffset^ + packet_of_bitstream(src, ioffset^) 
            };
            22 + cnt
        }
        | _ => {
            let cnt = length_of_bitstream(src, offset, 11)
            let ioffset = ref(offset + 18)
            print_endline("11-bit count packet: " ++ string_of_int(cnt))
            for(i in 1 to cnt) {
                print_endline("parsing next subpacket: " ++ string_of_int(i))
                ioffset := ioffset^ + packet_of_bitstream(src, ioffset^)
            }
            ioffset^ - offset
        }
    }
}

let fh = open_in(Array.get(Sys.argv, 1))
let stream = iter_channel(fh)

Stream.iter((x) => {
    print_endline(x)
    print_endline(bitstring_of_string(x))
    let _ = packet_of_bitstream(bitstring_of_string(x), 0)
}, stream)
