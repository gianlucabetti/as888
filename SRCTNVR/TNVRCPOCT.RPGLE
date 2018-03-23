000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200050119      *------------------------------------------------------------------------*
000300080227     fTncpo01l  if   e           K Disk
000400080228     fTncps11l  uf   e           k Disk
000500080228     fTncpi01l  uf   e           k Disk
000600080228     fTfntc01l  uf   e           k Disk
000700050119     fQsysprt   o    f  132        printer
000800050119
000900050119      *------------------------------------------------------------------------*
001000050119      *   C A M P I   D I   L A V O R O
001100050119      *------------------------------------------------------------------------*
001200080227     d kcpo            s             11  0
001300080227     d totcpi          s             11  0
001400080227     d totcps          s             11  0
001500080227     d totntc          s             11  0
001600050119      *------------------------------------------------------------------------*
001700080227      /free
001800080227       setll *loval  tncpi01l   ;
001900080227       read          tncpi01l   ;
002000080227
002100080227       dow  not %eof(tncpi01l)  ;
002200080227       kcpo=%int(%subst(cpink1:1:11))   ;
002300080227       chain   kcpo   tncpo01l  ;
002400080227       if  not %found(tncpo01l)  ;
002500080227       except stacpi   ;
002600080227       totcpi=totcpi+1   ;
002700080228       delete tncpi000   ;
002800080227       endif  ;
002900080227
003000080227       read          tncpi01l   ;
003100080227       enddo   ;
003200080227
003300080227       setll 'P'     tfntc01l   ;
003400080227       reade 'P'     tfntc01l   ;
003500080227
003600080227       dow  not %eof(tfntc01l)  ;
003700080227       kcpo=%int(%subst(ntcnk1:1:11))   ;
003800080227       chain   kcpo   tncpo01l  ;
003900080227       if  not %found(tncpo01l)  ;
004000080227       except staNTC   ;
004100080227       totntc=totntc+1   ;
004200080228       delete tfntc     ;
004300080227       endif  ;
004400080227
004500080227       reade 'P'     tfntc01l   ;
004600080227       enddo   ;
004700050119
004800080227       setll *loval  tncps11l   ;
004900080227       read          tncps11l   ;
005000080227
005100080227       dow  not %eof(tncps11l)  ;
005200080227       chain  cpscpo  tncpo01l  ;
005300080227       if  not %found(tncpo01l)  ;
005400080227       except stacps   ;
005500080228       totcps=totcps+1   ;
005600080228       delete tncps000   ;
005700080227       endif  ;
005800080227
005900080227       read          tncps11l   ;
006000080227       enddo   ;
006100080227
006200080227       except fine     ;
006300080227       Eval      *InLr = *On         ;
006400080227
006500080227      /end-free
006600080227
006700080227     oQsysprt   e            STACPI      1
006800080227     o                       cpiapl
006900080227     o                       cpink1             + 1
007000080227     o                       cpitpr             + 1
007100080227     o                       cpirnt             + 1
007200080227     o                       cpiDTR             + 1
007300080227     oQsysprt   e            STANTC      1
007400080227     o                       ntcapl
007500080227     o                       ntcnk1             + 1
007600080227     o                       ntcnk2             + 1
007700080227     o                       ntctnt             + 1
007800080227     o                       ntcrnt             + 1
007900080227     o                       ntcNTR             + 1
008000080227     oQsysprt   e            STACPS      1
008100080227     o                       cpscpo
008200080227     o                       cpsdst             + 1
008300080227     o                       cpsfst             + 1
008400080227     o                       cpscnc             + 1
008500080227     o                       cpSDTR             + 1
008600080227     oQsysprt   e            fine        1
008700080227     o                                              'CPI'
008800080227     o                       totcpi        2  +   1
008900080227     o                                        +   1 'NTC'
009000080227     o                       totntc        2  +   1
009100080227     o                                        +   1 'CPS'
009200080227     o                       totcps        2  +   1
