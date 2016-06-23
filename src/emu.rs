use std::io;
use std::io::Write;
use cpu::*;
use dasm::*;
use ncurses::*;

fn read_io_byte() -> u8 {
    match getch() {
        ERR => 0,
        10 => 13, // LF -> CR
        127 => 8, // DEL -> BS
        ch => ch as u8,
    }
}

fn write_io_byte(val: u8) {
    let mut curx: i32 = 0;
    let mut cury: i32 = 0;
    let mut maxx: i32 = 0;
    let mut maxy: i32 = 0;

    getmaxyx(stdscr, &mut maxy, &mut maxx);
    getyx(stdscr, &mut cury, &mut curx);

    match val {
        13 => {
            if cury >= maxy - 1 {
                mv(0, 0);
            };
            10
        }
        _ => addch(val as chtype),
    };

    refresh();
}

pub fn execute(code: &Vec<u8>,
               load_addr: u16,
               start_addr: u16,
               read_addr: u16,
               write_addr: u16)
               -> Result<String, String> {
    let mut cpu = CPU::new();
    cpu.load(&code, load_addr);
    cpu.set_pc(start_addr);

    if read_addr != write_addr {
        cpu.hook_io(read_addr, read_io_byte, write_addr, write_io_byte);
    }

    initscr();
    start_color();
    init_pair(1, COLOR_WHITE, COLOR_BLUE);
    wbkgd(stdscr, COLOR_PAIR(1));
    noecho();
    timeout(10);
    clear();
    refresh();
    do_run(&mut cpu);
    endwin();

    Ok("done".to_string())
}

pub fn monitor(code: &Vec<u8>, load_addr: u16, start_addr: u16) -> Result<String, String> {
    let mut cpu = CPU::new();
    cpu.load(&code, load_addr);
    cpu.set_pc(start_addr);

    let mut exit = false;
    while !exit {
        println!("");
        let mut ip = cpu.get_pc();

        for index in 0..3 {
            match disassemble_step(cpu.get_mem(), &mut ip) {
                Ok(s) => {
                    if index == 0 {
                        println!("* {}", s)
                    } else {
                        println!("  {}", s)
                    }
                }
                Err(e) => {
                    println!("Error: {}", e);
                    break;
                }
            }
        }

        print!("\n(s)tep (g)o (r)eg (m)em [addr] (p)c [addr] (b)p [addr] e(x)it : ");
        io::stdout().flush().ok().expect("Could not flush stdout");

        let mut cmd = String::new();
        io::stdin().read_line(&mut cmd).unwrap();

        let mut iter = cmd.trim_right().split_whitespace();
        match iter.next() {
            Some("x") => exit = true,
            Some("g") => {
                do_run(&mut cpu);
                println!("stopped at PC: ${:04X}", cpu.get_pc());
                println!("{}", cpu)
            }
            Some("s") => {
                let _ = do_single_step(&mut cpu);
                println!("stopped at PC: ${:04X}", cpu.get_pc());
                println!("{}", cpu)
            }
            Some("r") => println!("{}", cpu),
            Some("m") => {
                match iter.next() {
                    Some(a) => {
                        let mut addr = i64::from_str_radix(a.trim(), 16).unwrap() as usize;
                        for _ in 0..3 {
                            cpu.memdump(addr, 16);
                            addr = addr + 16;
                        }
                        // cpu.memdump(addr.trim().parse::<usize>().unwrap(), 16)
                    }
                    None => {}
                }
            }
            Some("p") => {
                match iter.next() {
                    Some(a) => {
                        let addr = i64::from_str_radix(a.trim(), 16).unwrap() as u16;
                        cpu.set_pc(addr)
                    }
                    None => {}
                }
            }
            Some("b") => {
                match iter.next() {
                    Some(a) => {
                        let addr = i64::from_str_radix(a.trim(), 16).unwrap() as u16;
                        cpu.set_breakpoint(addr)
                    }
                    None => {}
                }
            }
            Some(_) => {}
            None => {}
        }
    }

    Ok("done".to_string())
}

fn do_run(mut cpu: &mut CPU) {
    let mut break_set = false;
    while !break_set {
        break_set = do_single_step(&mut cpu)
    }
}

fn do_single_step(cpu: &mut CPU) -> bool {
    match cpu.single_step() {
        Ok(val) => val,
        Err(e) => {
            println!("Single step error: {}", e);
            true
        }
    }
}
