
**That is implementation of The Power blockchain node.**

Here is a list of main modules with short description:

 - *tpecdsa.erl* - secp256k1 cryptography primitives 
 - *naddress.erl* - Address format, encoder, parser, constructor tx.erl - transaction constructor, packer, unpacker, verifier 
 - *txpool.erl* - pool of transactions ledger.erl - ledger (chain's accounts keeper)
 - *ledger_sync.erl* - synchronization of ledgers 
 - *bal.erl* - ledger item (individual account), constructor, packer, unpacker 
 - *block.erl* - block manipulation functions 
 - *mkblock.erl* - block constructor 
 - *blockvote.erl* - collector of votes for block 
 - *blockchain.erl* - block chain tracker 
 - *bsig.erl* - block signature packer 
 - *chainsettings.erl* - API for chain's settings manipulating 
 - *synchronizer.erl* - chain's time synchronizer
 - *xchain** - set of modules for cross chain blocks passing

_______________________________
**"ThePower.io"** 
*Copyright (C) 2018 Mihaylenko Maxim, Belousov Igor
This program is not free software; you can not redistribute it and/or
modify it in accordance the following terms and conditions,* 
_______________________________

TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. This License applies to any program or other work which contains a notice placed by the copyright holder saying it may be distributed under the terms of this License. The "Program", below, refers to any such program or work.Each licensee is addressed as "you".

1. You can use this Program only in case of personal non-commercial use.

2. You may not copy and distribute copies of the Program and Program's source code as you receive it.

3. You may not modify your copy or copies of the Program or any portion of it, thus forming a work based on the Program, and copy and distribute such modifications or work.

4. You may not copy, modify, sublicense, or distribute the Program in object code or executable form. Any attempt to copy, modify, sublicense or distribute the Program is void, and will automatically terminate your rights under this License. 

NO WARRANTY

5. THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

6. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

END OF TERMS AND CONDITIONS