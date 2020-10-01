#!/usr/bin/env node

const web3 = require("@solana/web3.js");
const fs = require("mz/fs");
const yargs = require("yargs");

const argv = yargs
    .options(
        { 'url':
            { default: "http://localhost:8899"
            }
        , 'program' : { demand: true }
        , 'payer' : { demand: true }
        })
    .help().alias('help', 'h').argv;

console.log(argv)

const programAccount = new web3.Account();
console.log ("program id:" + programAccount.publicKey.toBase58())

async function calcFees (connection, data, storageSize) {
  let fees = 0;
  const {feeCalculator} = await connection.getRecentBlockhash();

  // Calculate the cost to load the program
  const NUM_RETRIES = 500; // allow some number of retries
  fees +=
    feeCalculator.lamportsPerSignature *
      (web3.BpfLoader.getMinNumSignatures(data.length) + NUM_RETRIES) +
    (await connection.getMinimumBalanceForRentExemption(data.length));

  // Calculate the cost to fund the greeter account
  fees += await await connection.getMinimumBalanceForRentExemption(storageSize);

  // Calculate the cost of sending the transactions
  fees += feeCalculator.lamportsPerSignature * 100; // wag

  return fees;
}

async function doit() {
    const connection = new web3.Connection(argv.url);
    var v = await connection.getVersion()
    console.log(v);

    v = await connection.getAccountInfo(programAccount.publicKey);
    console.log(v);


    const progData = await fs.readFile(argv.program);
    const fees = calcFees(connection, progData, );

    console.log(fees);
    var payerAccount = await fs.readFile(argv.payer);
    console.log ("payer id:" + payerAccount)
    payerAccount = new web3.Account(JSON.parse(payerAccount));
    console.log ("payer id:" + payerAccount.publicKey.toBase58())

    v = await web3.BpfLoader.load(
        connection,
        payerAccount,
        programAccount,
        progData,
        web3.BPF_LOADER_DEPRECATED_PROGRAM_ID);
    console.log(v);
};

doit().then(process.exit);
