
pub fn hexdump(buf: &Vec<u8>, from: usize, bytes: usize) {
    // Print offset
    print!("{:04X}: ", from);

    // Print hex bytes
    for n in from..from + bytes {
        match buf.get(n) {
            Some(x) => print!("{:02X} ", x),
            None => print!("   "),
        }
    }

    // Print ascii bytes
    print!(" ");
    for n in from..from + bytes {
        match buf.get(n) {
            Some(x) => {
                if (*x as char).is_alphanumeric() {
                    print!("{}", *x as char);
                } else {
                    print!(".");
                }
            }
            None => print!("."),
        }
    }

    println!("");
}
