use std::{cell::RefCell, rc::Rc};

use crate::{cpu::CPU, nes::RAM};

mod bus;
mod cpu;
mod nes;

fn main() {
    let bus = Rc::new(RefCell::new(nes::Bus6502::new()));
    let cpu = Rc::new(RefCell::new(nes::Cpu6502::new()));

    bus.borrow_mut().connect_cpu(&cpu);
    cpu.borrow_mut().connect_bus(&bus);

    // Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
    /*
        *=$8000
        LDX #10
        STX $0000
        LDX #3
        STX $0001
        LDY $0000
        LDA #0
        CLC
        loop
        ADC $0001
        DEY
        BNE loop
        STA $0002
        NOP
        NOP
        NOP
    */

    // Convert hex string into bytes for RAM
    let mut ram: [u8; RAM] = [0; RAM];
    let rom: Vec<u8> =
        "A2 0A 8E 00 00 A2 03 8E 01 00 AC 00 00 A9 00 18 6D 01 00 88 D0 FA 8D 02 00 EA EA EA"
            .split(" ")
            .map(|x| u8::from_str_radix(x, 16).expect("Invalid value for hex"))
            .collect();

    for (i, v) in rom.iter().enumerate() {
        ram[0x8000 + i] = *v;
    }

    // Set Reset Vector
    ram[0xFFFC] = 0x00;
    ram[0xFFFD] = 0x80;

    bus.borrow_mut().set_ram(&ram);

    // Reset now that we've updated the ram
    cpu.borrow_mut().reset();

    cpu.borrow_mut().clock();

    println!("Hello, world!");
}
