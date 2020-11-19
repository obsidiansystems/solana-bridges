#!/usr/bin/env node

const logger = new console.Console(process.stderr, process.stderr);
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
function readAccountSync(filename) {
    return new web3.Account(JSON.parse(fs.readFileSync(filename)));
}
async function readOrGenerateAccount(filename, hint) {
    if (!!filename) {
         return readAccount(filename);
    } else {
        var programAccount = new web3.Account();
        logger.log(hint + " keypair:" + JSON.stringify(Array.prototype.slice.call(programAccount.secretKey)));
        return programAccount;
    }
}

async function openConnection(url) {
    const connection = new web3.Connection(url);
    logger.log(await connection.getVersion());
    return connection;
}

async function doAlloc(argv) {
    const payerAccount = argv.payer;
    logger.log ("payer id:" + payerAccount.publicKey.toBase58())

    const storageAccount = await readOrGenerateAccount(argv.storageAccount, "storage");
    logger.log ("storage id:" + storageAccount.publicKey.toBase58());

    const programId = new web3.PublicKey(argv.programId);

    const connection = await openConnection(argv.url);

    const { space } = argv;

    return doAllocReal(connection, payerAccount, storageAccount, programId, space);
}

async function doAllocReal(connection, payerAccount, storageAccount, programId, space) {
    const fees = await calcFees(connection, new Buffer(0), space);

    const progAcctTxn = new web3.Transaction()
        .add(web3.SystemProgram.createAccount(
            { fromPubkey: payerAccount.publicKey
            , newAccountPubkey: storageAccount.publicKey
            , lamports: fees
            , space: space
            , programId: programId
            }))
        ;
    logger.log(await progAcctTxn)
    logger.log("storageAccount:" + storageAccount.publicKey.toBase58());

    const payerInfo = await connection.getAccountInfo(payerAccount.publicKey);

    logger.log( "payer balance: " + payerInfo.lamports);
    logger.log( "deployment fees: " + fees);

    if (fees > payerInfo.lamports) {
        logger.log("balance too low");
        process.exit(1);
    }

    var v = web3.sendAndConfirmTransaction(
        connection,
        progAcctTxn,
        [payerAccount, storageAccount],
        {skipPreflight: true, commitment: 'recent'});

    logger.log("alloc txn id: " + await v);

    const sleep = (milliseconds) => {
      return new Promise(resolve => setTimeout(resolve, milliseconds))
    }

    var stgInfo = null;
    while (stgInfo === null) {
        stgInfo = await connection.getAccountInfo(storageAccount.publicKey);
        if (stgInfo === null) {
            logger.log("...");
            await sleep(1000);
        }
    }
    logger.log(stgInfo);
    logger.log("owner:" + stgInfo.owner.toBase58());

    return {
        programId: programId.toBase58(),
        accountId: storageAccount.publicKey.toBase58(),
        accountKey: Array.prototype.slice.call(storageAccount.secretKey),
    };

};



function doCall(fn) {
    return async function (argv) {
        logger.log("args", argv);

        const storageId = argv.accountKey.publicKey;
        const programId = argv.programId;
        const payerAccount = argv.payer;
        logger.log ("payer id:" + payerAccount.publicKey.toBase58())

        const connection = await openConnection(argv.url);

        const {instructionData, isSigner, isWritable} = await fn(argv);

        const key = { pubkey:storageId ,isSigner, isWritable };
        logger.log("key", key.pubkey.toBase58());
        const txnI = { keys:[key] , programId, data: instructionData };

        const txn = new web3.Transaction().add(new web3.TransactionInstruction(txnI));
        logger.log("txn", txn);
        logger.log("txn", JSON.stringify(txn));

        let signers0 = isSigner
                ? [payerAccount, argv.accountKey]
                : [payerAccount]
                ;
        logger.log("signers", signers0.map(x => x.publicKey.toBase58()));

        var v = await connection.simulateTransaction(
            txn,
            signers0,
            );
        logger.log("simulation", JSON.stringify(v));

        var txId = await web3.sendAndConfirmTransaction(
            connection,
            txn,
            signers0,
            {
                commitment: 'singleGossip',
                skipPreflight: true,
            },
            );
        return {"sig": txId};
    };
}


async function noop(/* argv */) {
    return {
        instructionData :Buffer.from("00", "hex"),
        isSigner: false,
        isWritable: false
    };
}
async function initialize(argv) {
    const instructionData = Buffer.from("01" + argv.instruction, argv.instructionEncoding);
    return {
        instructionData,
        isSigner: true,
        isWritable: true
    };
}
async function newBlock(argv) {
    const instructionData = Buffer.from("02" + argv.instruction, argv.instructionEncoding);
    return {
        instructionData,
        isSigner: false,
        isWritable: true
    };
}
async function provideEthashElement(argv) {
    const instructionData = Buffer.from("03" + argv.element, argv.elementEncoding);
    return {
        instructionData,
        isSigner: false,
        isWritable: true
    };
}

async function doInclusionProof(argv) {
    logger.log("args", argv);

    const chainLogId = argv.accountId;
    const programId = argv.programId;
    const payerAccount = argv.payer;
    logger.log ("payer id:" + payerAccount.publicKey.toBase58())

    const connection = await openConnection(argv.url);

    const proofAccount = new web3.Account();
    logger.log ("proof storage id:" + proofAccount.publicKey.toBase58());

    const space = 9999; // ?
    const newAccountInfo = await doAllocReal(connection, payerAccount, proofAccount, chainLogId, space);

    const chainLogKey =
        { pubkey: chainLogId
        , isSigner: false
        , isWritable: false
        };
    logger.log("chain log key", chainLogKey.pubkey.toBase58());

    const proofKey =
        { pubkey: proofAccount.publicKey
        , isSigner: true
        , isWritable: true
        };
    logger.log("proof key", proofKey.pubkey.toBase58());

    async function doTx(data) {
        const txnI =
            { keys: [ chainLogKey, proofKey ]
            , programId
            , data
            };

        const txn = new web3.Transaction().add(new web3.TransactionInstruction(txnI));
        logger.log("txn", txn);
        logger.log("txn", JSON.stringify(txn));

        let signers0 = [payerAccount, proofAccount];
        logger.log("signers", signers0.map(x => x.publicKey.toBase58()));

        var v = await connection.simulateTransaction(
            txn,
            signers0
            );
        logger.log("simulation", JSON.stringify(v));

        var txId = await web3.sendAndConfirmTransaction(connection,
            txn,
            signers0
            );
        return {"sig": txId};
    }

    //doTx(Buffer.from("04", "hex"));
    const v = doTx(Buffer.from("05" + argv.inclusionProof, argv.inclusionProofEncoding));
    //doTx(Buffer.from("05" + argv.inclusionProof, argv.inclusionProofEncoding));
    //doTx(Buffer.from("05" + argv.inclusionProof, argv.inclusionProofEncoding));
    //doTx(Buffer.from("06", "hex"));

    return {"sig": v};
}


function callCmd (fn) {
    return function (argv) {
        return fn(argv)
            .then(result => {process.stdout.write(JSON.stringify(result)); process.exit();})
            .catch(bad => {logger.error(bad); process.exit(99)})
            ;
    };
}

function commandArgs(yargv) {
    return (yargv.config("config", x => JSON.parse(fs.readFileSync(x)))
     .options(
      { 'program-id' :
        { demand: true
        , coerce: arg => new web3.PublicKey(arg)
        }
      , 'account-key':
       { demand: true
       , coerce: arg => new web3.Account(typeof arg === "string" ? JSON.parse(arg) : arg)
       }
      }));
}

yargs
    .demandCommand().recommendCommands()

    .options(
        { 'url': { default: "http://localhost:8899" }
        , 'payer' :
            { default: process.env.HOME + "/.config/solana/id.json"
            , coerce: readAccountSync
            }
        })

    .command('alloc', 'Deploy program'
        , (yargv) => yargv.options(
            { 'program-id' : {demand : true }
            , 'space' : { demand : true, type: 'number' }
            , 'storage-account' : {}
            })
        , callCmd(doAlloc))

    .command('noop', 'noop'
        , (yargv) => commandArgs(yargv).options({
        })
        , callCmd(doCall(noop)))

    .command('initialize', 'initialize'
        , (yargv) => commandArgs(yargv).options(
            { 'instruction': {demand: true}
            , 'instruction-encoding': {default: 'hex'}
            })
        , callCmd(doCall(initialize)))
    .command('new-block', 'new block'
        , (yargv) => commandArgs(yargv).options(
            { 'instruction': {demand: true}
            , 'instruction-encoding': {default: 'hex'}
            })
        , callCmd(doCall(newBlock)))
    .command('provide-ethash-element', 'provide ethash element'
        , (yargv) => commandArgs(yargv).options(
            { 'element': {demand: true}
              , 'element-encoding': {default: 'hex'}
            })
        , callCmd(doCall(provideEthashElement)))

    .command('inclusion-proof', 'inclusion proof'
        , (yargv) => yargv.options(
            { 'program-id' :
                  { demand: true
                  , coerce: arg => new web3.PublicKey(arg)
                  }
            , 'account-id':
                  { demand: true
                  , coerce: arg => new web3.PublicKey(arg)
                  }
            , 'inclusion-proof': {demand: true}
            , 'inclusino-proof-encoding': {default: 'hex'}
            })
        , callCmd(doInclusionProof))

    .help().alias('help', 'h').argv;
