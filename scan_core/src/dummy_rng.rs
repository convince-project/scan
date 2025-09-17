use rand::{RngCore, SeedableRng};

#[derive(Debug, Clone)]
pub(crate) struct DummyRng;

impl RngCore for DummyRng {
    fn next_u32(&mut self) -> u32 {
        panic!("DummyRng should never be called")
    }

    fn next_u64(&mut self) -> u64 {
        panic!("DummyRng should never be called")
    }

    fn fill_bytes(&mut self, dst: &mut [u8]) {
        let _ = dst;
        panic!("DummyRng should never be called")
    }
}

impl SeedableRng for DummyRng {
    type Seed = [u8; 0];

    fn from_seed(_seed: Self::Seed) -> Self {
        Self
    }
}
