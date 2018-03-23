000100040804     H DECEDIT('0,') DATEDIT(*ymd.)
000200040915      * FNTS23R *----------------------------------------------------*
000300040915      *  tassazione autotrasportatori
000400030211      *--------------------------------------------------------------*
000500040915     Ffiftt01l  iF   E           K DISK
000600040915     FfifSt01l  iF   E           K DISK
000700040915     F                                     RENAME(fiftt000:FIFST000)
000800040915     Ffiftd01l  iF   E           K DISK
000900040915     FfifSd01l  iF   E           K DISK
001000040915     F                                     RENAME(fiftd000:FIFSd000)
001100040915     Ffifre01l  iF   E           K DISK
001200040915     FfiFCE01l  iF   E           K DISK
001300040910      *
001400040915     FfifttNEW  iF a E           K DISK
001500040915     F                                     RENAME(fiftt000:fttnew)
001600040915     FfifstNEW  iF a E           K DISK
001700040915     F                                     RENAME(fiftt000:fstnew)
001800040915     FfiftdNEW  o  a E           K DISK
001900040915     F                                     RENAME(fiftd000:ftdnew)
002000040915     FfifsdNEW  o  a E           K DISK
002100040915     F                                     RENAME(fiftd000:fsdnew)
002200040915     FfifreNEW  o  a E           K DISK
002300040915     F                                     RENAME(fifre000:frenew)
002400040915     FfifceNEW  o  a E           K DISK
002500040915     F                                     RENAME(fifce000:fcenew)
002600040910     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002700040804     D* Schiera p.o. da trasferire
002800040804     D  SKPO           s              3    dim(500)
002900030211      *
003000040915     d Klin            s                   like(fcefgs)
003100040915     d Kpdr            s                   like(fttpdr)
003200040915     d wpdrfine        s                   like(fttpdr)
003300030211      *
003400900511     D KPJBA         E DS
003500900523     C*****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800040804     C                   parm                    PARAM          1500
003900040910      *
004000040804     c     ' ':'0'       xlate     param         param
004100040804     c
004200040804     c                   movea     param         skpo
004300950621     C*---------------------------------------------------------------*
004400040915     C     KFTT          KLIST
004500040915     C                   KFLD                    FTTPDR
004600040915     C                   KFLD                    FTTTSR
004700040915     C                   KFLD                    FTTNDC
004800040915     C                   KFLD                    FTTDDC
004900040915     C*---------------------------------------------------------------*
005000040804     c                   z-add     1             x                 4 0
005100040804    1c                   dow       skpo(x)>*zeros
005200040915     C                   CLEAR                   Kpdr
005300040915     C                   eval      wpdrfine=9999999
005400040915     c                   movel     skpo(X)       klin
005500040915     c                   movel     skpo(X)       kpdr
005600040915     c                   movel     skpo(X)       wpdrfine
005700040915     C     kpdr          SETLL     fiftt01l
005800040915     C                   READ      fiftt01l                               30
005900040910     c*
006000040915     c                   dow       not %eof(fiftt01l)  and fttpdr<=wpdrfine
006100040804     C*
006200040910     C** ELABORAZIONE RECORDS relativi
006300040915     C                   EXSR      elaFTT
006400040804     C*
006500040915     C                   READ      fiftt01l                               30
006600040804    2C                   ENDDO
006700040915     C* SIMULAZIONE
006800040915     C     kpdr          SETLL     fifSt01l
006900040915     C                   READ      fifSt01l                               30
007000040915     c*
007100040915     c                   dow       not %eof(fifSt01l)  and fttpdr<=wpdrfine
007200040915     C*
007300040915     C** ELABORAZIONE RECORDS relativi
007400040915     C                   EXSR      elaFST
007500040915     C*
007600040915     C                   READ      fifSt01l                               30
007700040915    2C                   ENDDO
007800040915     C* rettifiche conteggi
007900040915     C     kpdr          SETLL     fifre01l
008000040915     C                   READ      fifre01l                               30
008100040915     c*
008200040915     c                   dow       not %eof(fifre01l)  and frepdr<=wpdrfine
008300040915     C                   WRITE     FreNEW
008400040915     c                   add       1             totfre            7 0
008500040915     C*
008600040915     C                   READ      fifre01l                               30
008700040915    2C                   ENDDO
008800040915     C* competenze di C/E
008900040915     C     klin          SETLL     fifce01l
009000040915     C     klin          READe     fifce01l                               30
009100040915     c*
009200040915     c                   dow       not %eof(fifce01l)
009300040915     C                   WRITE     FceNEW                               99
009400040915     c  n99              add       1             totfce            7 0
009500040915     C*
009600040915     C     klin          READe     fifce01l                               30
009700040915    2C                   ENDDO
009800040915
009900040804     C                   ADD       1             X
010000040804     C                   ENDDO
010100900523     C*
010200040910     c                   EXCEPT    RINUM1
010300900523     C                   SETON                                        LR
010400040804     C**-------------------------------------------------------------**
010500040910     C**  elabora record relativi                                    **
010600040804     C**-------------------------------------------------------------**
010700040915     C     ELAFTT        BEGSR
010800040804     C*
010900040910     C
011000040915     C     KFTT          SETLL     fifttnew                               31
011100040804     C* se non trovata
011200040915    0C                   IF        not *in31  and fttatb=' '
011300040915     C                   WRITE     FTTNEW
011400040915     c                   add       1             totftt            7 0
011500040915     c* FTD
011600040915     c     kftt          setll     fiftd01l
011700040915     c     kftt          reade     fiftd01l
011800040915     c                   dow       not %eof(fiftd01l)
011900040915     c                   if        ftdatb=' '
012000040915     C                   WRITE     FTdNEW
012100040915     c                   add       1             totftd            7 0
012200040915     c                   endif
012300040915     c     kftt          reade     fiftd01l
012400040915     c                   enddo
012500040915     c                   endif
012600040804     C*
012700040804     C                   ENDSR
012800040915     C**-------------------------------------------------------------**
012900040915     C**  elabora record relativi                                    **
013000040915     C**-------------------------------------------------------------**
013100040915     C     ELAFST        BEGSR
013200040915     C*
013300040915     C
013400040915     C     KFTT          SETLL     fifStnew                               31
013500040915     C* se non trovata
013600040915    0C                   IF        not *in31  and fttatb=' '
013700040915     C                   WRITE     FSTNEW
013800040915     c                   add       1             totfSt            7 0
013900040915     c* FTD
014000040915     c     kftt          setll     fifSd01l
014100040915     c     kftt          reade     fifSd01l
014200040915     c                   dow       not %eof(fifSd01l)
014300040915     c                   if        ftdatb=' '
014400040915     C                   WRITE     FSdNEW
014500040915     c                   add       1             totfSd            7 0
014600040915     c                   endif
014700040915     c     kftt          reade     fifSd01l
014800040915     c                   enddo
014900040915     c                   endif
015000040915     C*
015100040915     C                   ENDSR
015200040910      *---------------------------------------------------------------*
015300040910     oQSYSPRT   E            RINUM1         2 02
015400040915     o                                         +  5 'TASSAZIONE AUTOTRASP.'
015500040915     o                                         +  5 'Fits23R'
015600040910     o                       UDATE             +  2 '  /  /  '
015700040910     o          E            RINUM1      1
015800040915     o                                         +  3 'FIFTT00F'
015900040915     o                       TOTFTT        2   +  2
016000040915     o                                         +  3 'FIFTD00F'
016100040915     o                       TOTFTD        2   +  2
016200040915     o                                         +  3 'FIFST00F'
016300040915     o                       TOTFST        2   +  2
016400040915     o                                         +  3 'FIFSD00F'
016500040915     o                       TOTFSD        2   +  2
016600040915     o          E            RINUM1      1
016700040915     o                                         +  3 'FIFRE00F'
016800040915     o                       TOTFRE        2   +  2
016900040915     o                                         +  3 'FIFCE00F'
017000040915     o                       TOTFcE        2   +  2
