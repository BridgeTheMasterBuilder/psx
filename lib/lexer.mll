{
exception Refill

type packet =
    | QueryHaltReason
    | ReadGeneralRegisters
    | ReadMemory of { addr: int; length: int }
    | WriteMemory of { addr: int; length: int; data: int }
    | QSupported of string list
    | Kill
    | Step of int option
    | SwBreak of { addr: int; kind: int }
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
(* | "$s" (hex_digit+ as addr)? '#' (checksum as cs) { 
    match addr with 
|   Some addr -> sanity_check ("s" ^ addr) cs; let addr = parse_hex addr in Packet (Step (Some addr))
|   None -> Packet (Step None) 
} *)
| "$Z0," (hex_digit+ as addr) ',' (hex_digit+ as kind) '#' (checksum as cs) { 
    sanity_check (Printf.sprintf "Z0,%s,%s" addr kind) cs; 
    let addr = parse_hex addr in 
    let kind = parse_hex kind in 
    Packet (SwBreak { addr; kind }) 
} 
| "$" [^'#']* '#' checksum { Packet Unimplemented }