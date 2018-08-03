## The Power blockchain proof of concept implementation

That is implementation of The Power blockchain node.

Here is a list of main modules with short description:

* tpecdsa.erl - secp256k1 cryptography primitives
* naddress.erl - Address format, encoder, parser, constructor
* tx.erl - transaction constructor, packer, unpacker, verifier
* txpool.erl - pool of transactions
* ledger.erl - ledger (chain's accounts keeper)
* ledger_sync.erl - synchronization of ledgers
* bal.erl - ledger item (individual account), constructor, packer, unpacker
* block.erl - block manipulation functions
* mkblock.erl - block constructor
* blockvote.erl - collector of votes for block
* blockchain.erl - block chain tracker
* bsig.erl - block signature packer
* chainsettings.erl - API for chain's settings manipulating
* synchronizer.erl - chain's time synchronizer
* xchain* - set of modules for cross chain blocks passing


