pragma solidity >=0.6.0 <0.8.0;

struct Slot {
    bool hasBlock;
    bytes32 blockHash;
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
    Slot[] public slots;

    uint64 public epoch;
    LeaderSchedule schedule;

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
    }

    function addBlock(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash) external {
        authorize();
        addBlockAuthorized(slot, blockHash, parentSlot, parentBlockHash);
    }

    function addBlockAuthorized(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash) private {
        if(initialized) {
            if(slot <= lastSlot)
                revert("Already seen slot");
            if(parentSlot != lastSlot)
                revert("Unexpected parent slot");
            if(parentBlockHash != lastHash)
                revert("Unexpected parent hash");
        }
        initialized = true;

        for(uint64 s = lastSlot + 1; s < slot; s++) {
            emptySlot(s);
        }
        fillSlot(slot, blockHash);
        lastSlot = slot;
        lastHash = blockHash;

        seenBlocks++;
    }

    function slotOffset(uint64 s) private pure returns (uint64) {
        return s % HISTORY_SIZE;
    }

    function fillSlot(uint64 s, bytes32 hash) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.blockHash = hash;
        slot.hasBlock = true;
    }

    function emptySlot(uint64 s) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.blockHash = 0;
        slot.hasBlock = false;
    }
}
