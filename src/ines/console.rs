use super::cpu::{Bus as CpuBus, CpuRef};

#[derive(Debug, Default)]
pub struct Nes {
    cpu: CpuRef<CpuBus>,
}

impl Nes {
    #[allow(unused)]
    pub fn new() -> Self {
        Nes::default()
    }
}

// impl  for Nes {
//     fn default() -> Self {
//         Self {
//             cpu: CpuRef::default()
//         }
//     }
// }

impl Iterator for Nes {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        self.cpu.borrow_mut().next();
        None
    }
}
