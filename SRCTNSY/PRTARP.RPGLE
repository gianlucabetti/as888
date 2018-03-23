000100000000     **
000200060206     **  Programma . . : PRTARP
000300060206     **  Description : Stampa ARP Table per una data interfaccia
000400000000     **
000500000000     **  Parameters:
000600000000     **    PxLinNam    INPUT   Line name for which the ARP table is retrieved.
000700000000     **
000800000000     **
000900000000     **  Compile options:
001000000000     **
001100000000     **    CrtRpgMod Module( CBX1063 )  DbgView( *LIST )
001200000000     **
001300000000     **    CrtPgm    Pgm( CBX1063 )
001400000000     **              Module( CBX1063 )
001500000000     **
001600000000     **-- Header specifications:  --------------------------------------------**
001700000000     H Option( *SrcStmt )
001800000000     **-- Printer file:  -----------------------------------------------------**
001900000000     FQSYSPRT   O    F  132        Printer  InfDs( PrtLinInf )  OflInd( *InOf )
002000000000     **-- Printer file information:  -----------------------------------------**
002100060206     D PrtLinInf       Ds
002200000000     D  PlOvfLin                      5i 0  Overlay( PrtLinInf: 188 )
002300000000     D  PlCurLin                      5i 0  Overlay( PrtLinInf: 367 )
002400000000     D  PlCurPag                      5i 0  Overlay( PrtLinInf: 369 )
002500000000     **-- System information:  -----------------------------------------------**
002600000000     D                SDs
002700000000     D  PsPgmNam         *Proc
002800000000     **-- Global declarations:  ----------------------------------------------**
002900000000     D Ix              s             10u 0
003000000000     D UsrSpc          c                   'LSTARPTBL QTEMP'
003100000000     **
003200000000     D Time            s              6s 0
003300000000     D NbrRcds         s             10u 0
003400000000     D PtLinTyp        s              5a
003500000000     D PtEntTyp        s              7a
003600000000     D PtDLCI          s              3i 0
003700000000     **-- Line type table:  --------------------------------------------------**
003800000000     D LinTbl          Ds
003900000000     D  LinTyp                        6a   Dim( 5 )
004000000000     D                               30a   Overlay( LinTbl )
004100000000     D                                     Inz( 'ELAN  TRLAN FR    WLS   DDI ' )
004200000000     **-- Entry type table:  -------------------------------------------------**
004300000000     D EntTbl          Ds
004400000000     D  EntTyp                        8a   Dim( 3 )
004500000000     D                               24a   Overlay( EntTbl )
004600000000     D                                     Inz( 'Dynamic Local   Proxy   ' )
004700000000     **-- Api error data structure:  -----------------------------------------**
004800000000     D ApiError        Ds
004900000000     D  AeBytPro                     10i 0 Inz( %Size( ApiError ))
005000000000     D  AeBytAvl                     10i 0 Inz
005100000000     D  AeMsgId                       7a
005200000000     D                                1a
005300000000     D  AeMsgDta                    128a
005400000000     **-- API Header information:  -------------------------------------------**
005500000000     D HdrInf          Ds                  Based( pHdrInf )
005600000000     D  HiUsrSpcNamSp                10a
005700000000     D  HiUsrSpcLibSp                10a
005800000000     D  HiLinNamUsd                  10a
005900000000     **-- User space generic header:  ---------- -----------------------------**
006000000000     D UsrSpcHdr       Ds                  Based( pUsrSpc )
006100000000     D  UsOfsHdr                     10i 0 Overlay( UsrSpcHdr: 117 )
006200000000     D  UsOfsLst                     10i 0 Overlay( UsrSpcHdr: 125 )
006300000000     D  UsNumLstEnt                  10i 0 Overlay( UsrSpcHdr: 133 )
006400000000     D  UsSizLstEnt                  10i 0 Overlay( UsrSpcHdr: 137 )
006500000000     **-- User space pointers:  ----------------------------------------------**
006600000000     D pUsrSpc         s               *   Inz( *Null )
006700000000     D pHdrInf         s               *   Inz( *Null )
006800000000     D pLstEnt         s               *   Inz( *Null )
006900000000     **-- ARP table entry:  --------------------------------------------------**
007000000000     D ARPT0100        Ds                  Based( pLstEnt )
007100000000     D  A1IntAdr                     15a
007200000000     D                                1a
007300000000     D  A1IntAdrBin                  10u 0
007400000000     D  A1LinTyp                     10i 0
007500000000     D  A1EthTyp                     10i 0
007600000000     D  A1EntTyp                     10i 0
007700000000     D  A1DLCI                       10u 0
007800000000     D  A1RifVldMsk                  10i 0
007900000000     D  A1RtgInfFld                  18a
008000000000     D  A1PhyAdr                     17a
008100000000     D                                1a
008200000000     **-- Create user space: -------------------------------------------------**
008300000000     D CrtUsrSpc       Pr                  ExtPgm( 'QUSCRTUS' )
008400000000     D  CsSpcNamQ                    20a   Const
008500000000     D  CsExtAtr                     10a   Const
008600000000     D  CsInzSiz                     10i 0 Const
008700000000     D  CsInzVal                      1a   Const
008800000000     D  CsPubAut                     10a   Const
008900000000     D  CsText                       50a   Const
009000000000     **-- Optional 1:
009100000000     D  CsReplace                    10a   Const  Options( *NoPass )
009200000000     D  CsError                   32767a          Options( *NoPass: *VarSize )
009300000000     **-- Optional 2:
009400000000     D  CsDomain                     10a   Const  Options( *NoPass )
009500000000     **-- Delete user space: -------------------------------------------------**
009600000000     D DltUsrSpc       Pr                  ExtPgm( 'QUSDLTUS' )
009700000000     D  DsSpcNamQ                    20a   Const
009800000000     D  DsError                   32767a          Options( *VarSize )
009900000000     **-- Retrieve pointer to user space: ------------------------------------**
010000000000     D RtvPtrSpc       Pr                  ExtPgm( 'QUSPTRUS' )
010100000000     D  RpSpcNamQ                    20a   Const
010200000000     D  RpPointer                      *
010300000000     D  RpError                   32767a          Options( *NoPass: *VarSize )
010400000000     **-- List ARP table:  ---------------------------------------------------**
010500000000     D LstArpTbl       Pr                  ExtProc( 'QtocLstPhyIfcARPTbl' )
010600000000     D  LaSpcNamQ                    20a   Const
010700000000     D  LaFmtNam                      8a   Const
010800000000     D  LaLinNam                     10a   Const
010900000000     D  LcError                   32767a          Options( *VarSize )
011000000000     **-- Parameters:  -------------------------------------------------------**
011100000000     D PxLinNam        s             10a
011200000000     **
011300000000     C     *Entry        Plist
011400000000     C                   Parm                    PxLinNam
011500000000     **
011600000000     **-- Mainline:  ---------------------------------------------------------**
011700000000     **
011800000000     C                   Time                    Time
011900000000     C                   Except    Header
012000000000     **
012100000000     C                   If        %Parms      > *Zero
012200000000     **
012300000000     C                   CallP     CrtUsrSpc( UsrSpc
012400000000     C                                      : *Blanks
012500000000     C                                      : 65535
012600000000     C                                      : x'00'
012700000000     C                                      : '*CHANGE'
012800000000     C                                      : *Blanks
012900000000     C                                      : '*YES'
013000000000     C                                      : ApiError
013100000000     C                                      )
013200000000     **
013300000000     C                   CallP     LstArpTbl( UsrSpc
013400000000     C                                      : 'ARPT0100'
013500000000     C                                      : PxLinNam
013600000000     C                                      : ApiError
013700000000     C                                      )
013800000000     **
013900000000     C                   If        AeBytAvl    = *Zero
014000000000     C                   ExSr      PrcLstEnt
014100000000     C                   EndIf
014200000000     **
014300000000     C                   CallP     DltUsrSpc( UsrSpc
014400000000     C                                      : ApiError
014500000000     C                                      )
014600000000     **
014700000000     C                   EndIf
014800000000     **
014900000000     C                   If        NbrRcds    =  *Zero
015000000000     C                   Except    NoRcds
015100000000     C                   EndIf
015200000000     **
015300000000     C                   Eval      *InLr       = *On
015400000000     C                   Return
015500000000     **
015600000000     **-- Process list entries:  ---------------------------------------------**
015700000000     C     PrcLstEnt     BegSr
015800000000     **
015900000000     C                   CallP     RtvPtrSpc( UsrSpc
016000000000     C                                      : pUsrSpc
016100000000     C                                      )
016200000000     **
016300000000     C                   Eval      pHdrInf     = pUsrSpc + UsOfsHdr
016400000000     C                   Eval      pLstEnt     = pUsrSpc + UsOfsLst
016500000000     **
016600000000     C                   For       Ix          = 1  to UsNumLstEnt
016700000000     **
016800000000     C                   ExSr      PrtArpTblE
016900000000     **
017000000000     C                   If        Ix          < UsNumLstEnt
017100000000     C                   Eval      pLstEnt     = pLstEnt + UsSizLstEnt
017200000000     C                   EndIf
017300000000     C                   EndFor
017400000000     **
017500000000     C                   EndSr
017600000000     **-- Print connection detail line:  -------------------------------------**
017700000000     C     PrtArpTblE    BegSr
017800000000     **
017900000000     C                   If        PlCurLin    > PlOvfLin - 3
018000000000     C                   Except    Header
018100000000     C                   EndIf
018200000000     **
018300000000     C                   Eval      PtLinTyp   =  LinTyp(A1LinTyp)
018400000000     C                   Eval      PtEntTyp   =  EntTyp(A1EntTyp)
018500000000     C                   Eval      PtDLCI     =  A1DLCI
018600000000     **
018700000000     C                   Eval      NbrRcds    =  NbrRcds + 1
018800000000     C                   Except    ArpTblE
018900000000     **
019000000000     C                   EndSr
019100000000     **-- Print file definition:  --------------------------------------------**
019200000000     OQSYSPRT   EF           Header         2  3
019300000000     O                       UDATE         Y      8
019400000000     O                       Time                18 '  :  :  '
019500000000     O                                           76 'Print physical interface-
019600000000     O                                               ARP table'
019700000000     O                                          107 'Program:'
019800000000     O                       PsPgmNam           118
019900000000     O                                          126 'Page:'
020000000000     O                       PAGE             +   1
020100000000     OQSYSPRT   EF           Header         1
020200000000     O                                           16 'Internet address'
020300000000     O                                           27 'Line type'
020400000000     O                                           37 'Eth type'
020500000000     O                                           49 'Entry type'
020600000000     O                                           56 'DLCI'
020700000000     O                                           69 'RIF vld msk'
020800000000     O                                           85 'Rtg info field'
020900000000     O                                          108 'Physical address'
021000000000     **
021100000000     OQSYSPRT   EF           ArpTblE        1
021200000000     O                       A1IntAdr            15
021300000000     O                       PtLinTyp            26
021400000000     O                       A1EthTyp      3     35
021500000000     O                       PtEntTyp            48
021600000000     O                       PtDLCI        3     55
021700000000     O                       A1RifVldMsk   3     65
021800000000     O                       A1RtgInfFld         90
021900000000     O                       A1PhyAdr           109
022000000000     **
022100000000     OQSYSPRT   EF           NoRcds      1
022200000000     O                                           26 '(No entries found)'
