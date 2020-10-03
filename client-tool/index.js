#!/usr/bin/env node

const web3 = require("@solana/web3.js");
const fs = require("mz/fs");
const yargs = require("yargs");

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

async function readAccount(filename) {
    return new web3.Account(JSON.parse(await fs.readFile(filename)));
}
async function readOrGenerateAccount(filename, hint) {
    if (!!filename) {
         return readAccount(filename);
    } else {
        var programAccount = new web3.Account();
        console.log(hint + " keypair:" + JSON.stringify(Array.prototype.slice.call(programAccount.secretKey)));
        return programAccount;
    }
}


async function doDeploy(argv) {

    const progData = await fs.readFile(argv.program);

    const programAccount = await readOrGenerateAccount(argv.programId, "program");
    console.log ("program id:" + programAccount.publicKey.toBase58());

    const storageAccount = await readOrGenerateAccount(argv.storageAccount, "storage");
    console.log ("storage id:" + storageAccount.publicKey.toBase58());

    const payerAccount = await readAccount(argv.payer);
    console.log ("payer id:" + payerAccount.publicKey.toBase58())

    const connection = new web3.Connection(argv.url);
    console.log(await connection.getVersion());

    const payerInfo = await connection.getAccountInfo(payerAccount.publicKey);

    if (null !== await connection.getAccountInfo(programAccount.publicKey)) {
        console.log("already deployed");
        return;
    }

    const fees = await calcFees(connection, progData, argv.space);

    console.log( "payer balance: " + payerInfo.lamports);
    console.log( "deployment fees: " + fees);

    if (fees > payerInfo.lamports) {
        console.log("balance too low");
        process.exit(1);
    }

    const loaderVersion = argv.useDeprecatedLoader
        ? web3.BPF_LOADER_DEPRECATED_PROGRAM_ID
        : web3.BPF_LOADER_PROGRAM_ID
        ;

    console.log("loaderVersion:" + loaderVersion);

    var v = await web3.Loader.load(
        connection,
        payerAccount,
        programAccount,
        loaderVersion,
        progData,
        );

    console.log("loader result:" + v);

    // todo solana cli can do the part above;

    const progAcctTxn = new web3.Transaction()
        .add(web3.SystemProgram.createAccount(
            { fromPubkey: payerAccount.publicKey
            , newAccountPubkey: storageAccount.publicKey
            , lamports: fees
            , space: argv.space
            , programId: programAccount.publicKey
            }))
        ;
    console.log(await progAcctTxn)
    console.log("storageAccount:" + storageAccount.publicKey.toBase58());

    v = web3.sendAndConfirmTransaction(
        connection,
        progAcctTxn,
        [payerAccount, storageAccount],
        {skipPreflight: true, commitment: 'recent'});

    console.log("alloc txn id: " + await v);

    const sleep = (milliseconds) => {
      return new Promise(resolve => setTimeout(resolve, milliseconds))
    }

    var stgInfo = null;
    while (stgInfo === null) {
        stgInfo = await connection.getAccountInfo(storageAccount.publicKey);
        if (stgInfo === null) {
            console.log("...");
            await sleep(1000);
        }
    }
    console.log(stgInfo);
    console.log("owner:" + stgInfo.owner.toBase58());

};

async function doCall(argv) {
    // console.log(argv);

    const storageId = new web3.PublicKey(argv.storageId);
    const programId = new web3.PublicKey(argv.programId);

    const connection = new web3.Connection(argv.url);
    console.log(await connection.getVersion());

    const payerAccount = await readAccount(argv.payer);
    console.log ("payer id:" + payerAccount.publicKey.toBase58())


    const instructionData = argv.hasOwnProperty("instruction")
        ? Buffer.from(argv.instruction, argv.instructionEncoding)
        : Buffer.alloc(0)
        ;

    const key = { pubkey:storageId ,isSigner:false, isWritable:true };
    const txnI = { keys:[key] , programId, data: instructionData };
    const txn = new web3.Transaction().add(new web3.TransactionInstruction(txnI));
    console.log(txn);
    var v = await web3.sendAndConfirmTransaction(connection, txn, [payerAccount]);
    console.log(v);
}

function callCmd (fn) {
    return function (argv) {
        return fn(argv)
            .then(() => {console.log("Ok!"); process.exit();})
            .catch(bad => console.error(bad))
            ;
    };
}

yargs
    .options(
        { 'url': { default: "http://localhost:8899" }
        , 'payer' : { default: process.env.HOME + "/.config/solana/id.json"}
        })
    .command('deploy', 'Deploy program'
        , (yargv) => yargv.options(
            { 'program' : { demand: true }
            , 'program-id' : {}
            , 'space' : { demand : true, type: 'number' }
            , 'storage-account' : {}
            , 'use-deprecated-loader': {type: 'boolean', default: false}
            })
        , callCmd(doDeploy))
    .command('call', 'Call program'
        , (yargv) => yargv.options(
            { 'storage-id' : {demand: true}
            , 'program-id' : {demand: true}
            , 'instruction': {}
            , 'instruction-encoding': {default: 'hex'}
            })
        , callCmd(doCall))
    .help().alias('help', 'h').argv;

