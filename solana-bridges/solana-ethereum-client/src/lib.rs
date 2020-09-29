#![cfg(feature = "program")]
mod eth;
mod processor;

#[cfg(test)]
mod tests;

use processor::process_instruction;
use solana_sdk::entrypoint_deprecated;

// Declare and export the program's entrypoint
entrypoint_deprecated!(process_instruction);
