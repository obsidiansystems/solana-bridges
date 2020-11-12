#![cfg(feature = "program")]
pub mod epoch_roots;
pub mod eth;
pub mod instruction;
pub mod ledger_ring_buffer;
pub mod pow_proof;
pub mod processor;
pub mod prove;
pub mod types;

#[cfg(test)]
mod tests;

use processor::process_instruction;
use solana_sdk::entrypoint_deprecated;

// Declare and export the program's entrypoint
entrypoint_deprecated!(process_instruction);
