pragma solidity >=0.6.0 <0.8.0;

struct Slot {
    bool hasBlock;
    bytes32 blockHash;
    bytes32 blockMerkleRoot;
}

struct LeaderSchedule {
    uint256[] publicKeys;
    uint64[] slotKeys;
}

contract SolanaClient {
    uint64 constant HISTORY_SIZE = 100;
    address immutable public creator;

    bool public initialized;

    uint64 public seenBlocks;
    bytes32 public lastHash;
    uint64 public lastSlot;
    Slot[HISTORY_SIZE] public slots;

    uint64 public epoch;
    LeaderSchedule schedule;

    event Success();

    constructor () public {
        creator = msg.sender;
    }

    function authorize() internal view {
        if(creator != msg.sender)
            revert("Sender not trusted");
    }

    function setEpoch(uint64 newEpoch, uint256[] calldata schedulePublicKeys, uint64[] calldata scheduleSlotKeys) external {
        authorize();
        epoch = newEpoch;
        schedule.publicKeys = schedulePublicKeys;
        schedule.slotKeys = scheduleSlotKeys;
        emit Success();
    }

    function getSlotLeader(uint64 slot) external view returns (uint256) {
        if (slot >= schedule.slotKeys.length)
            revert("Slot out of bounds for epoch");

        return schedule.publicKeys[schedule.slotKeys[slot]];
    }

    function addBlocks(uint64[] calldata blockSlots,
                       bytes32[] calldata blockHashes,
                       uint64[] calldata parentSlots,
                       bytes32[] calldata parentBlockHashes) external {
        authorize();
        for(uint i = 0; i < blockSlots.length; i++)
            addBlockAuthorized(blockSlots[i], blockHashes[i], parentSlots[i], parentBlockHashes[i]);
        emit Success();
    }

    function addBlock(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash) external {
        authorize();
        addBlockAuthorized(slot, blockHash, parentSlot, parentBlockHash);
        emit Success();
    }

    function addBlockAuthorized(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash) private {
        if(initialized) {
            if(slot <= lastSlot)
                revert("Already seen slot");
            if(parentSlot != lastSlot)
                revert("Unexpected parent slot");
            if(parentBlockHash != lastHash)
                revert("Unexpected parent hash");

            for(uint64 s = lastSlot + 1; s < slot; s++) {
                emptySlot(s);
            }
        }
        fillSlot(slot, blockHash);

        lastSlot = slot;
        lastHash = blockHash;
        seenBlocks++;
        initialized = true;
    }

    function slotOffset(uint64 s) private pure returns (uint64) {
        return s % HISTORY_SIZE;
    }

    function fillSlot(uint64 s, bytes32 hash) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.hasBlock = true;
        slot.blockHash = hash;
        //TODO: store merkle roots
    }

    function emptySlot(uint64 s) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.hasBlock = false;
        slot.blockHash = 0;
        slot.blockMerkleRoot = 0;
    }

    function verifyTransaction(bytes32[16][] calldata proof, uint64 slot, bytes calldata transaction, uint64 index) external view returns (bool) {
        return this.verifyMerkleProof(proof, slots[slotOffset(slot)].blockMerkleRoot, transaction, index);
    }

    function verifyMerkleProof(bytes32[16][] calldata proof, bytes32 root, bytes calldata leaf, uint64 index) external pure returns (bool) {
        bytes32 hash = sha256(leaf);

        for (uint height = 0; height < proof.length; height++) {
            uint64 offset = index % 16;

            if(hash != proof[height][offset]) {
                return false;
            }

            bytes memory hashable = new bytes(32 * 16);
            for(uint i = 0; i < 16; i++) {
                for(uint j = 0; j < 32; j++) {
                    hashable[i*32+j] = proof[height][i][j];
                }
            }

            index = index / 16;
            hash = sha256(hashable);
        }

        return hash == root;
    }
}
