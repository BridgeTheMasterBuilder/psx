{
exception Refill

type addr = int
type length = int

type packet =
    | QueryHaltReason
    | ReadGeneralRegisters
    | ReadMemory of addr * length
    | QSupported of string list
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
| "$qSupported:" ((gdbfeature (';' gdbfeature)+) as gdbfeatures) '#' (checksum as cs) { sanity_check ("qSupported:" ^ gdbfeatures) cs; let features = (String.split_on_char ';' gdbfeatures) in Packet (QSupported features) } 
| "$m" (hex_digit+ as addr) ',' (hex_digit+ as length) '#' (checksum as cs) { sanity_check (Printf.sprintf "m%s,%s" addr length) cs; let addr = parse_hex addr in let length = parse_hex length in Packet (ReadMemory (addr, length)) } 
| "$" [^'#']* '#' checksum { Packet Unimplemented }