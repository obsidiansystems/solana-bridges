pragma solidity >=0.6.0 <0.8.0;

struct Slot {
    bool hasBlock;
    bytes32 blockHash;
    bytes32 leaderPublicKey;
}

contract SolanaClient {
    uint64 constant HISTORY_SIZE = 100;
    address immutable public creator;
    bool public initialized;

    // root of trust
    bytes32 public rootHash;
    uint64 public rootSlot;
    bytes32 public rootPublicKey;
    // epoch schedule
    bool warmup;
    uint64 firstNormalEpoch;
    uint64 leaderScheduleSlotOffset;
    uint64 firstNormalSlot;
    uint64 slotsPerEpoch;


    uint64 public seenBlocks;
    bytes32 public lastHash;
    uint64 public lastSlot;
    Slot[HISTORY_SIZE] public slots;

    event Success();

    constructor () public {
        creator = msg.sender;
    }
    function initialize(
            uint64 slot,
            bytes32 blockHash,
            bytes32 leader,
            bool scheduleWarmup,
            uint64 scheduleFirstNormalEpoch,
            uint64 scheduleLeaderScheduleSlotOffset,
            uint64 scheduleFirstNormalSlot,
            uint64 scheduleSlotsPerEpoch
        ) public {
        if(initialized)
            revert("already initialized");
        if(creator != msg.sender)
            revert("Sender not trusted");

        fillSlot(slot, blockHash, leader);
        lastSlot = slot;
        lastHash = blockHash;

        rootHash = blockHash;
        rootSlot = slot;
        rootPublicKey = leader;
        warmup = scheduleWarmup;
        firstNormalEpoch = scheduleFirstNormalEpoch;
        leaderScheduleSlotOffset = scheduleLeaderScheduleSlotOffset;
        firstNormalSlot = scheduleFirstNormalSlot;
        slotsPerEpoch = scheduleSlotsPerEpoch;

        initialized = true;

        emit Success();
    }

    function authorize() internal view {
        if(!initialized)
            revert("not initialized");
        if(creator != msg.sender)
            revert("Sender not trusted");
    }

    function addBlocks(uint64[] calldata blockSlots,
                       bytes32[] calldata blockHashes,
                       uint64[] calldata parentSlots,
                       bytes32[] calldata parentBlockHashes,
                       bytes32[] calldata leaders
                       ) external {
        authorize();
        for(uint i = 0; i < blockSlots.length; i++)
            addBlockAuthorized(blockSlots[i], blockHashes[i], parentSlots[i], parentBlockHashes[i], leaders[i]);
        emit Success();
    }

    function addBlock(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash, bytes32 leaderPublicKey) external {
        authorize();
        addBlockAuthorized(slot, blockHash, parentSlot, parentBlockHash, leaderPublicKey);
        emit Success();
    }

    function addBlockAuthorized(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash, bytes32 leader) private {
        if(slot <= lastSlot)
            revert("Already seen slot");
        if(parentSlot != lastSlot)
            revert("Unexpected parent slot");
        if(parentBlockHash != lastHash)
            revert("Unexpected parent hash");

        for(uint64 s = lastSlot + 1; s < slot; s++) {
            emptySlot(s);
        }
        fillSlot(slot, blockHash, leader);

        lastSlot = slot;
        lastHash = blockHash;
        seenBlocks++;
    }

    function slotOffset(uint64 s) private pure returns (uint64) {
        return s % HISTORY_SIZE;
    }

    function fillSlot(uint64 s, bytes32 hash, bytes32 leader) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.blockHash = hash;
        slot.hasBlock = true;
        slot.leaderPublicKey = leader;
    }

    function emptySlot(uint64 s) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.blockHash = 0;
        slot.hasBlock = false;
    }
}
