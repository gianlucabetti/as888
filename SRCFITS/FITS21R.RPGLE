000100040804     H DECEDIT('0,') DATEDIT(*ymd.)
000200040914      * FNTS21R *----------------------------------------------------*
000300040914      *  SALDI CLIENTE :  SISDO SISDC SISDA SISDP SISDE
000400030211      *--------------------------------------------------------------*
000500040914     FSISDO01L  iF   E           K DISK
000600040914     FSISDC01L  iF   E           K DISK
000700040914     FSISDA01L  iF   E           K DISK
000800040914     FSISDP01L  iF   E           K DISK
000900040914     FSISDE99L  iF   E           K DISK
001000040910      *
001100040914     FSISDONEW  UF a E           K DISK
001200040914     F                                     RENAME(SISDO000:SDONEW)
001300040914     FSISDCNEW  UF a E           K DISK
001400040914     F                                     RENAME(SISDC000:SDCNEW)
001500040914     FSISDANEW  UF a E           K DISK
001600040914     F                                     RENAME(SISDA000:SDANEW)
001700040914     FSISDPNEW  UF a E           K DISK
001800040914     F                                     RENAME(SISDP000:SDPNEW)
001900040914     FSISDENEW  UF a E           K DISK
002000040914     F                                     RENAME(SISDE000:SDENEW)
002100040910     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002200040804     D* Schiera p.o. da trasferire
002300040804     D  SKPO           s              3    dim(500)
002400030211      *
002500040914     d KKSC            s                   like(SDOKSC)
002600040914     d wKSCfine        s                   like(SDOKSC)
002700030211      *
002800900511     D KPJBA         E DS
002900900523     C*****************************************************************
003000000000     C     *ENTRY        PLIST
003100000000     C                   PARM                    KPJBA
003200040804     C                   parm                    PARAM          1500
003300040804     c
003400040804     c     ' ':'0'       xlate     param         param
003500040804     c
003600040804     c                   movea     param         skpo
003700900511     C*---------------------------------------------------------------*
003800040804     C* LEGGO le linee passate
003900040804     C*
004000040804     c                   z-add     1             x                 4 0
004100040804    1c                   dow       skpo(x)>*zeros
004200040914     C                   CLEAR                   KKSC
004300040914     C                   eval      wKSCfine=9999999
004400040914     c                   movel     skpo(X)       kKSC
004500040914     c                   movel     skpo(X)       wKSCfine
004600040914
004700040914     c                   EXSR      ELASDO
004800040914
004900040914     c                   EXSR      ELASDC
005000040914
005100040914     c                   EXSR      ELASDA
005200040914
005300040914     c                   EXSR      ELASDP
005400040914
005500040914     c                   EXSR      ELASDE
005600040914     C
005700040804     C                   ADD       1             X
005800040804     C                   ENDDO
005900900523     C*
006000040910     c                   EXCEPT    RINUM1
006100900523     C                   SETON                                        LR
006200040804     C**-------------------------------------------------------------**
006300040914     C**  elabora SISDO00F
006400040804     C**-------------------------------------------------------------**
006500040914     C     ELAsdo        BEGSR
006600040914     C     kKSC          SETLL     SISDO01l
006700040914     C                   READ      SISDO01l                               30
006800040914     c*
006900040914     c                   IF        not %eof(SISDO01l)  and SDOKSC<=wKSCfine
007000040914     c
007100040914     C* SE NE TROVO ALMENO UNO CANCELLO TUTTO E RISCRIVO
007200040914     C     SDOKSC        SETLL     SISDONEW
007300040914     C     SDOKSC        reade     SISDONEW
007400040914     c                   dow       not %eof(sisdonew)
007500040914     c                   delete    sdonew
007600040914     C     SDOKSC        reade     SISDONEW
007700040914     c                   enddo
007800040914     C                   ENDIF
007900040914     C
008000040914     C     kKSC          SETLL     SISDO01l
008100040914     C                   READ      SISDO01l                               30
008200040914     c                   dow       not %eof(SISDO01l)  and SDOKSC<=wKSCfine
008300040914     C                   WRITE     SDONEW
008400040914     c                   add       1             totsdo            7 0
008500040914     C*
008600040914     C                   READ      SISDO01L                               30
008700040914    2C                   ENDDO
008800040804     C*
008900040804     C                   ENDSR
009000040914     C**-------------------------------------------------------------**
009100040914     C**  elabora SISDC00F
009200040914     C**-------------------------------------------------------------**
009300040914     C     ELAsdC        BEGSR
009400040914     C     kKSC          SETLL     SISDC01l
009500040914     C                   READ      SISDC01l                               30
009600040914     c*
009700040914     c                   IF        not %eof(SISDC01l)  and SDCKSC<=wKSCfine
009800040914     C* SE NE TROVO ALMENO UNO CANCELLO TUTTO E RISCRIVO
009900040914     C     SDCKSC        SETLL     SISDCNEW
010000040914     C     SDCKSC        reade     SISDCNEW
010100040914     c                   dow       not %eof(sisdcnew)
010200040914     c                   delete    sdonew
010300040914     C     SDCKSC        reade     SISDCNEW
010400040914     c                   enddo
010500040914     C                   ENDIF
010600040914     C
010700040914     C     kKSC          SETLL     SISDC01l
010800040914     C                   READ      SISDC01l                               30
010900040914     c                   dow       not %eof(SISDC01l)  and SDCKSC<=wKSCfine
011000040914     C                   WRITE     SDCNEW
011100040914     c                   add       1             totsdc            7 0
011200040914     C*
011300040914     C                   READ      SISDc01L                               30
011400040914    2C                   ENDDO
011500040914     C*
011600040914     C                   ENDSR
011700040914     C**-------------------------------------------------------------**
011800040914     C**  elabora SISDA00F
011900040914     C**-------------------------------------------------------------**
012000040914     C     ELAsda        BEGSR
012100040914     C     kKSC          SETLL     SISDA01l
012200040914     C                   READ      SISDA01l                               30
012300040914     c*
012400040914     c                   IF        not %eof(SISDA01l)  and SDAKSC<=wKSCfine
012500040914     C* SE NE TROVO ALMENO UNO CANCELLO TUTTO E RISCRIVO
012600040914     C     SDAKSC        SETLL     SISDANEW
012700040914     C     SDAKSC        reade     SISDANEW
012800040914     c                   dow       not %eof(sisdANew)
012900040914     c                   delete    sdAnew
013000040914     C     SDAKSC        reade     SISDANEW
013100040914     c                   enddo
013200040914     C                   ENDIF
013300040914     C
013400040914     C     kKSC          SETLL     SISDA01l
013500040914     C                   READ      SISDA01l                               30
013600040914     c                   dow       not %eof(SISDA01l)  and SDAKSC<=wKSCfine
013700040914     C                   WRITE     SDANEW
013800040914     c                   add       1             totsda            7 0
013900040914     C*
014000040914     C                   READ      SISDa01L                               30
014100040914    2C                   ENDDO
014200040914     C*
014300040914     C                   ENDSR
014400040914     C**-------------------------------------------------------------**
014500040914     C**  elabora SISDP00F
014600040914     C**-------------------------------------------------------------**
014700040914     C     ELAsdP        BEGSR
014800040914     C     kKSC          SETLL     SISDP01l
014900040914     C                   READ      SISDP01l                               30
015000040914     c*
015100040914     c                   IF        not %eof(SISDP01l)  and SDPKSC<=wKSCfine
015200040914     C* SE NE TROVO ALMENO UNO CANCELLO TUTTO E RISCRIVO
015300040914     C     SDPKSC        SETLL     SISDPNEW
015400040914     C     SDPKSC        reade     SISDPNEW
015500040914     c                   dow       not %eof(sisdPnew)
015600040914     c                   delete    sdPnew
015700040914     C     SDPKSC        reade     SISDPNEW
015800040914     c                   enddo
015900040914     C                   ENDIF
016000040914     C
016100040914     c                   dow       not %eof(SISDP01l)  and SDPKSC<=wKSCfine
016200040914     C                   WRITE     SDPNEW
016300040914     c                   add       1             totsdP            7 0
016400040914     C*
016500040914     C*
016600040914     C                   READ      SISDP01L                               30
016700040914    2C                   ENDDO
016800040914     C*
016900040914     C                   ENDSR
017000040914     C**-------------------------------------------------------------**
017100040914     C**  elabora SISDE00F
017200040914     C**-------------------------------------------------------------**
017300040914     C     ELAsdE        BEGSR
017400040914     C     kKSC          SETLL     SISDE99l
017500040914     C                   READ      SISDE99l                               30
017600040914     c*
017700040914     c                   IF        not %eof(SISDE99l)  and SDEKSC<=wKSCfine
017800040914     C* SE NE TROVO ALMENO UNO CANCELLO TUTTO E RISCRIVO
017900040914     C     SDEKSC        SETLL     SISDENEW
018000040914     C     SDEKSC        reade     SISDENEW
018100040914     c                   dow       not %eof(sisdEnew)
018200040914     c                   delete    sdEnew
018300040914     C     SDEKSC        reade     SISDENEW
018400040914     c                   enddo
018500040914     C                   ENDIF
018600040914     C
018700040914     c                   dow       not %eof(SISDE99l)  and SDEKSC<=wKSCfine
018800040914     C                   WRITE     SDENEW
018900040914     c                   add       1             totsdE            7 0
019000040914     C*
019100040914     C*
019200040914     C                   READ      SISDE99L                               30
019300040914    2C                   ENDDO
019400040914     C*
019500040914     C                   ENDSR
019600040910      *---------------------------------------------------------------*
019700040910     oQSYSPRT   E            RINUM1         2 02
019800040914     o                                         +  5 'SALDI CLIENTE'
019900040914     o                                         +  5 'Fits21R'
020000040910     o                       UDATE             +  2 '  /  /  '
020100040910     o          E            RINUM1      1
020200040914     o                                         +  3 'SISDO00F:'
020300040914     o                       TOTSDO        2   +  2
020400040914     o                                         +  3 'SISDC00F:'
020500040914     o                       TOTSDC        2   +  2
020600040914     o                                         +  3 'SISDA00F:'
020700040914     o                       TOTSDA        2   +  2
020800040914     o                                         +  3 'SISDP00F:'
020900040914     o                       TOTSDP        2   +  2
021000040914     o                                         +  3 'SISDE00F:'
021100040914     o                       TOTSDE        2   +  2
