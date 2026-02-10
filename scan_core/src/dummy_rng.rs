use rand::SeedableRng;
use rand::rand_core::TryRng;

#[derive(Debug, Clone)]
pub(crate) struct DummyRng;

impl TryRng for DummyRng {
    type Error = core::convert::Infallible;

    fn try_next_u32(&mut self) -> Result<u32, Self::Error> {
        panic!("DummyRng should never be called")
    }

    fn try_next_u64(&mut self) -> Result<u64, Self::Error> {
        panic!("DummyRng should never be called")
    }

    fn try_fill_bytes(&mut self, dst: &mut [u8]) -> Result<(), Self::Error> {
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
