use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

extern crate clap;
use clap::{Arg, App};

extern crate ncurses;

mod dasm;
mod emu;
mod cpu;
mod opcodes;
mod utils;

fn main() {
    let matches = App::new("retro6502")
        .version("0.1")
        .author("Salim Alam")
        .about("6502 Disassembler and Emulator")
        .arg(Arg::with_name("disassemble")
            .short("d")
            .long("disasemble")
            .help("Disassemble the specified binary")
            .conflicts_with("execute"))
        .arg(Arg::with_name("execute")
            .short("e")
            .long("execute")
            .help("Execute the specified binary")
            .conflicts_with("disasemble"))
        .arg(Arg::with_name("m")
            .short("m")
            .long("monitor")
            .help("Debug the specified binary in monitor mode (Default)")
            .conflicts_with("disasemble"))
        .arg(Arg::with_name("start_addr")
            .short("s")
            .long("start_addr")
            .value_name("ADDRESS")
            .help("Start address for Program Counter (hexadecimal, e.g: FE02)")
            .takes_value(true))
        .arg(Arg::with_name("load_addr")
            .short("l")
            .long("load_addr")
            .value_name("ADDRESS")
            .help("Address to load input file at (hexadecimal, e.g: FE02)")
            .takes_value(true))
        .arg(Arg::with_name("read_addr")
            .short("r")
            .long("read_addr")
            .value_name("ADDRESS")
            .help("Address for I/O read at (hexadecimal, e.g: FE02)")
            .takes_value(true))
        .arg(Arg::with_name("write_addr")
            .short("w")
            .long("write_addr")
            .value_name("ADDRESS")
            .help("Address for I/O write (hexadecimal, e.g: FE02)")
            .takes_value(true))
        .arg(Arg::with_name("FILENAME")
            .help("The binary file to use")
            .required(true)
            .index(1))
        .get_matches();

    let filename = matches.value_of("FILENAME").unwrap();
    let path = Path::new(&filename);
    let mut file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", filename, why.description()),
        Ok(file) => file,
    };

    let mut b = Vec::new();
    match file.read_to_end(&mut b) {
        Err(why) => panic!("couldn't read {}: {}", filename, why.description()),
        Ok(_) => {}
    }

    let l_addr = matches.value_of("load_addr").unwrap_or("0");
    let load_addr = i64::from_str_radix(l_addr.trim(), 16).unwrap() as u16;

    let s_addr = matches.value_of("start_addr").unwrap_or(l_addr);
    let start_addr = i64::from_str_radix(s_addr.trim(), 16).unwrap() as u16;

    let r_addr = matches.value_of("read_addr").unwrap_or("0");
    let read_addr = i64::from_str_radix(r_addr.trim(), 16).unwrap() as u16;

    let w_addr = matches.value_of("write_addr").unwrap_or("0");
    let write_addr = i64::from_str_radix(w_addr.trim(), 16).unwrap() as u16;

    let result = if matches.is_present("disassemble") {
        dasm::disassemble(&b, load_addr, start_addr)
    } else if matches.is_present("execute") {
        emu::execute(&b, load_addr, start_addr, read_addr, write_addr)
    } else {
        emu::monitor(&b, load_addr, start_addr)
    };

    match result {
        Ok(val) => println!("{}", val),
        Err(err) => println!("Failed with error: {}", err),
    }
}
