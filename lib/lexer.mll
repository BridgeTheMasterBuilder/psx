{
exception Refill

type qxfer =
    | FeaturesRead of string

type packet =
    | QueryHaltReason
    | ReadGeneralRegisters
    | ReadMemory of { addr: int; length: int }
    | WriteMemory of { addr: int; length: int; data: int }
    | QSupported of string list
    | Kill
    | Step of int option
    | InsertSwBreak of { addr: int; kind: int; }
    | RemoveSwBreak of { addr: int; kind: int; }
    | InsertWriteWatchpoint of { addr: int; kind: int; }
    | RemoveWriteWatchpoint of { addr: int; kind: int; }
    | InsertReadWatchpoint of { addr: int; kind: int; }
    | RemoveReadWatchpoint of { addr: int; kind: int; }
    | Continue
    | SetOperation of { op: char; tid: int }
    | QueryThreadId
    | QueryAttached
    | QueryVContSupported
    | QueryThreadInfoFirst
    | QueryThreadInfoSubsequent
    | QXfer of qxfer
    | Unimplemented

type message = 
| Packet of packet
| Acknowledge
| RequestRetransmission

let calculate_checksum payload =
  String.fold_left (fun sum char -> sum + Char.code char) 0 payload mod 256

let parse_checksum checksum =
    "0x" ^ checksum |> int_of_string

let parse_hex = parse_checksum 

let sanity_check data checksum =
    let checksum_in = parse_checksum checksum in 
    let checksum_out = calculate_checksum data in 
    if checksum_in <> checksum_out then failwith "Bad checksum" 
}

let hex_digit = ['0'-'9''A'-'F''a'-'f']
let checksum = hex_digit hex_digit
let gdbfeature = [^';''#']+
let operation = ['m''M''g''G''c']

rule lex = parse 
| '+' { Acknowledge }
| '-' { RequestRetransmission }
| "$?#" (checksum as cs) { sanity_check "?" cs; Packet QueryHaltReason }
| "$g#" (checksum as cs) { sanity_check "g" cs; Packet ReadGeneralRegisters }
| "$qSupported:" ((gdbfeature (';' gdbfeature)+) as gdbfeatures) '#' (checksum as cs) { 
    sanity_check ("qSupported:" ^ gdbfeatures) cs; 
    let features = (String.split_on_char ';' gdbfeatures) in 
    Packet (QSupported features) 
} 
| "$m" (hex_digit+ as addr) ',' (hex_digit+ as length) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "m%s,%s" addr length) cs; 
    let addr = parse_hex addr in 
    let length = parse_hex length in 
    Packet (ReadMemory { addr; length }) 
} 
| "$M" (hex_digit+ as addr) ',' (hex_digit+ as length) ':' (hex_digit+ as data) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "M%s,%s:%s" addr length data) cs; 
    let addr = parse_hex addr in 
    let length = parse_hex length in 
    let data = parse_hex data in 
    Packet (WriteMemory { addr; length; data }) 
} 
| "$k#" (checksum as cs) { sanity_check "k" cs; Packet Kill } 
| "$s" (hex_digit+ as addr)? '#' (checksum as cs) { 
    match addr with 
|   Some addr -> sanity_check ("s" ^ addr) cs; let addr = parse_hex addr in Packet (Step (Some addr))
|   None -> Packet (Step None) 
}
| "$Z0," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "Z0,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (InsertSwBreak { addr; kind }) 
} 
| "$z0," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "z0,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (RemoveSwBreak { addr; kind }) 
} 
| "$Z2," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "Z2,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (InsertWriteWatchpoint { addr; kind }) 
} 
| "$z2," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "z2,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (RemoveWriteWatchpoint { addr; kind }) 
} 
| "$Z3," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "Z3,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (InsertReadWatchpoint { addr; kind }) 
} 
| "$z3," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "z3,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (RemoveReadWatchpoint { addr; kind }) 
} 
| "$c#" (checksum as cs) { sanity_check "c" cs; Packet Continue } 
| "$H" (operation as op) (hex_digit+ as id) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "H%c%s" op id) cs; 
    let tid = parse_hex id in 
    Packet (SetOperation { op; tid }) 
} 
| "$H" (operation as op) "-1#" (checksum as cs) { 
    sanity_check (Printf.sprintf "H%c-1" op) cs; 
    Packet (SetOperation { op; tid = -1 }) 
} 
| "$qC#" (checksum as cs) { sanity_check "qC" cs; Packet QueryThreadId } 
| "$qAttached#" (checksum as cs) { sanity_check "qAttached" cs; Packet QueryAttached } 
| "$vCont?#" (checksum as cs) { sanity_check "vCont?" cs; Packet QueryVContSupported } 
| "$qfThreadInfo#" (checksum as cs) { sanity_check "qfThreadInfo" cs; Packet QueryThreadInfoFirst } 
| "$qsThreadInfo#" (checksum as cs) { sanity_check "qsThreadInfo" cs; Packet QueryThreadInfoSubsequent } 
| "$qXfer:features:read:target.xml:" (hex_digit+ as offset) ',' (hex_digit+ as length) '#' (checksum as cs) { sanity_check (Printf.sprintf "qXfer:features:read:target.xml:%s,%s" offset length) cs; Packet (QXfer (FeaturesRead "target.xml")) } 
| '$' [^'#']* '#' checksum { Packet Unimplemented }