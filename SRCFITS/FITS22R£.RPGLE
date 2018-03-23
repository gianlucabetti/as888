000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040914      * FITS22R  *----------------------------------------------------*
000400040914      *  Danni
000500021010      *---------------------------------------------------------------* c
000600040914     fFndctNEW  If A e           k disk    rename(FNdct000:dctNEW)
000700040914     fFndcCNEW  o  A e           k disk    rename(FNdcC000:dcCNEW)
000800040914     fFndcdNEW  o  A e           k disk    rename(FNdcd000:dcdNEW)
000900040914     fFndcfNEW  o  A e           k disk    rename(FNdcf000:dcfNEW)
001000040914     fFndclNEW  o  A e           k disk    rename(FNdcl000:dclNEW)
001100040914     fFndcrNEW  o  A e           k disk    rename(FNdcr000:dcrNEW)
001200040914     fFndcsNEW  o  A e           k disk    rename(FNdcs000:dcsNEW)
001300040914     fFndptNEW  o  A e           k disk    rename(FNdpt000:dptNEW)
001400040914     fFndkaNEW  o  A e           k disk    rename(FNdka000:dkaNEW)
001500040914     fFndadNEW  o  A e           k disk    rename(FNdad000:dadNEW)
001600040914     fFndCKNEW  o  A e           k disk    rename(FNdCK000:dCKNEW)
001700040914     fFndct00F  If   e             disk
001800040914     fFndcC01L  If   e           K disk
001900040914     fFndcd01L  If   e           K disk
002000040914     fFndcf01L  If   e           K disk
002100040914     fFndcl01L  If   e           K disk
002200040914     fFndcr01L  If   e           K disk
002300040914     fFndcs01L  If   e           K disk
002400040914     fFndpt01L  If   e           K disk
002500040914     fFndka01L  If   e           K disk
002600040914     fFndad01L  If   e           K disk
002700040914     fFndCK01L  If   e           K disk
002800040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002900021008      *
003000021010     d KPJBA         E DS
003100040805     D* Schiera p.o. da trasferire
003200040805     D  SKPO           s              3    dim(500)
003300040914     D DDCT01        E DS                  inz
003400040914     D knkt            DS
003500040914     D  DCTAAC
003600040914     D  DCTFIL
003700040914     D  DCTNCA
003800040914     D ktpd            s                   LIKE(DCStpd)  INZ('C')
003900040914     d KLIN            s                   like(dadfil)
004000040805      *
004100021008      *****************************************************************
004200021010     c     *ENTRY        PLIST
004300021010     c                   PARM                    KPJBA
004400040805     C                   parm                    PARAM          1500
004500040805     c
004600040805     c     ' ':'0'       xlate     param         param
004700040805     c                   movea     param         skpo
004800040805     C*
004900040914     c     Knca          KLIST
005000040914     c                   KFLD                    DCTAAC
005100040914     c                   KFLD                    DCTFIL
005200040914     c                   KFLD                    DCTNCA
005300040914     C     KDCS          KLIST
005400040914     C                   KFLD                    KTPD
005500040914     C                   KFLD                    KNKT
005600021010      *---------------------------------------------------------------*
005700040914     c                   READ      Fndct00f                               22
005800040914     c
005900040910    1c     *IN22         DOWEQ     *OFF
006000040914     c
006100040914     c* elaboro danni con lna o lnp = ai p.o. richiesti
006200040914     c                   movel     dctlna        wlinea            3
006300040805     c     wlinea        lookup    skpo                                   35
006400040914     c
006500040910    2c                   if        not *in35
006600040914     c                   movel     dctflo        ddct01
006700040914     c                   movel     §dctlnpc      wlinea
006800040914     c     wlinea        lookup    skpo                                   35
006900040914     c                   endif
007000040914     c
007100040910    2c                   if        *in35
007200040914     c     kNCA          setll     fndctnew                               31
007300040910    3c                   if        not *in31
007400040914     c                   EXSR      ELABORA
007500040910    3c                   endif
007600040910    2c                   endif
007700021008      *
007800040914     c                   READ      FnDCT00f                               22
007900040910    1c                   ENDDO
008000040914     c* FNDAD
008100040914     c                   z-add     1             x                 4 0
008200040914    1c                   dow       skpo(x)>*zeros
008300040914     c                   movel     skpo(X)       klin
008400040914     C     klin          SETLL     fndad01l
008500040914     C     klin          READe     fndad01l                               30
008600040914     C*
008700040914    2C     *IN30         DOWEQ     *OFF
008800040914     C*
008900040914     C** ELABORAZIONE RECORDS RICEVUTI DELLE BOLLE
009000040914     C                   write     dadnew                               99
009100040914     C  n99              add       1             totdad            7 0
009200040914     C*
009300040914     C     KLIN          READE     FNdad01l                               30
009400040914    2C                   ENDDO
009500040914     c
009600040914     C                   ADD       1             X
009700040914     C                   ENDDO
009800021010      *
009900040805     c                   EXCEPT    RINUM1
010000021011     c                   seton                                        LR
010100040914      **-----------------------------------------------*
010200040914      ** ELABORO FILE COLLEGATI
010300040914      **-----------------------------------------------*
010400040914     C     ELABORA       BEGSR
010500040914     C                   WRITE     DCTNEW
010600040914     C                   ADD       1             TOTDCT            7 0
010700040914      * FNDCC
010800040914     c     knca          setll     fndcc000
010900040914     c     knca          reade     fndcc000
011000040914     c                   DOW       NOT %EOF(FNDCC01L)
011100040914
011200040914     C                   write     DCCNEW                               99
011300040914     C  N99              add       1             TOTDCC            7 0
011400040914
011500040914     c     knca          reade     fndcc000
011600040914     c                   ENDDO
011700040914      * FNDCD
011800040914     c     knca          setll     fndcD000
011900040914     c     knca          reade     fndcD000
012000040914     c                   DOW       NOT %EOF(FNDCD01L)
012100040914
012200040914     C                   write     DCDNEW                               99
012300040914     C  N99              add       1             TOTDCD            7 0
012400040914
012500040914     c     knca          reade     fndcD000
012600040914     c                   ENDDO
012700040914      * FNDCF
012800040914     c     knca          setll     fndcf000
012900040914     c     knca          reade     fndcf000
013000040914     c                   DOW       NOT %EOF(FNDCf01L)
013100040914
013200040914     C                   write     DCfNEW                               99
013300040914     C  N99              add       1             TOTDCf            7 0
013400040914
013500040914     c     knca          reade     fndcf000
013600040914     c                   ENDDO
013700040914      * FNDCl
013800040914     c     knca          setll     fndcl000
013900040914     c     knca          reade     fndcl000
014000040914     c                   DOW       NOT %EOF(FNDCl01L)
014100040914
014200040914     C                   write     DClNEW                               99
014300040914     C  N99              add       1             TOTDCl            7 0
014400040914
014500040914     c     knca          reade     fndcl000
014600040914     c                   ENDDO
014700040914      * FNDCr
014800040914     c     knca          setll     fndcr000
014900040914     c     knca          reade     fndcr000
015000040914     c                   DOW       NOT %EOF(FNDCr01L)
015100040914
015200040914     C                   write     DCrNEW                               99
015300040914     C  N99              add       1             TOTDCr            7 0
015400040914
015500040914     c     knca          reade     fndcr000
015600040914     c                   ENDDO
015700040914      * FNDCs
015800040914     c     kdcs          setll     fndcs000
015900040914     c     kdcs          reade     fndcs000
016000040914     c                   DOW       NOT %EOF(FNDCs01L)
016100040914
016200040914     C                   write     DCsNEW                               99
016300040914     C  N99              add       1             TOTDCs            7 0
016400040914
016500040914     c     kdcs          reade     fndcs000
016600040914     c                   ENDDO
016700040914      * FNDPT
016800040914     c     knca          setll     fndpt000
016900040914     c     knca          reade     fndpt000
017000040914     c                   DOW       NOT %EOF(FNDpt01L)
017100040914
017200040914     C                   write     DptNEW                               99
017300040914     C  N99              add       1             TOTDpt            7 0
017400040914
017500040914     c     knca          reade     fndpt000
017600040914     c                   ENDDO
017700040914      * FNDKA
017800040914     c     knca          setll     fndka000
017900040914     c     knca          reade     fndka000
018000040914     c                   DOW       NOT %EOF(FNDka01L)
018100040914
018200040914     C                   write     DkaNEW                               99
018300040914     C  N99              add       1             TOTDka            7 0
018400040914
018500040914     c     knca          reade     fndka000
018600040914     c                   ENDDO
018700040914      * FNDCK
018800040914     c     knca          setll     fndCK000
018900040914     c     knca          reade     fndCK000
019000040914     c                   DOW       NOT %EOF(FNDCK01L)
019100040914
019200040914     C                   write     DCKNEW                               99
019300040914     C  N99              add       1             TOTDCK            7 0
019400040914
019500040914     c     knca          reade     fndCK000
019600040914     c                   ENDDO
019700040914     c                   ENDSR
019800021010      *
019900040914      *C--------------------------------------------------------------*
020000040805     oQSYSPRT   E            RINUM1         2 02
020100040914     o                                         +  5 'DANNI'
020200040914     o                                         +  5 'Fits22R'
020300021010     o                       UDATE             +  2 '  /  /  '
020400021010     o          E            RINUM1      1
020500040914     o                                         +  3 'FNDCT00F:'
020600040914     o                       TOTDCT        2   +  2
020700040914     o                                         +  3 'FNDCD00F:'
020800040914     o                       TOTDCD        2   +  2
020900040914     o                                         +  3 'FNDCC00F:'
021000040914     o                       TOTDCC        2   +  2
021100040914     o                                         +  3 'FNDCF00F:'
021200040914     o                       TOTDCF        2   +  2
021300040914     o          E            RINUM1      1
021400040914     o                                         +  3 'FNDCL00F:'
021500040914     o                       TOTDCL        2   +  2
021600040914     o                                         +  3 'FNDCK00F:'
021700040914     o                       TOTDCK        2   +  2
021800040914     o                                         +  3 'FNDCR00F:'
021900040914     o                       TOTDCR        2   +  2
022000040914     o                                         +  3 'FNDCS00F:'
022100040914     o                       TOTDCS        2   +  2
022200040914     o          E            RINUM1      1
022300040914     o                                         +  3 'FNDPT00F:'
022400040914     o                       TOTDPT        2   +  2
022500040914     o                                         +  3 'FNDKA00F:'
022600040914     o                       TOTDKA        2   +  2
022700040914     o                                         +  3 'FNDAD00F:'
022800040914     o                       TOTDAD        2   +  2
