000100040804     H DECEDIT('0,') DATEDIT(*ymd.)
000200040915      * FNTS23R *----------------------------------------------------*
000300040915      *  Tariffe    autotrasportatori
000400030211      *--------------------------------------------------------------*
000500090114     Ffitgt01l  iF   E           K DISK
000501090114     Ffifgt01l  iF   E           K DISK
000600040915     Ffifpt01l  iF   E           K DISK
000700040915     Ffifpd01l  iF   E           K DISK
000701110916     Ffiadt01l  iF   E           K DISK
000702110916     Ffiadd01l  iF   E           K DISK
000800040910      *
000900090115     FfitgtNEW  iF a E           K DISK
001000090115     F                                     RENAME(fitgt000:tgtnew)
001001090115     FfifgtNEW  iF a E           K DISK
001002090115     F                                     RENAME(fifgt000:fgtnew)
001100040915     FfifptNEW  o  a E           K DISK
001200040915     F                                     RENAME(fifpt000:fptnew)
001300040915     FfifpdNEW  o  a E           K DISK
001400040915     F                                     RENAME(fifpd000:fpdnew)
001401110916     FfiadtNEW  o  a E           K DISK
001402110916     F                                     RENAME(fiadt000:adtnew)
001403110916     FfiaddNEW  o  a E           K DISK
001404110916     F                                     RENAME(fiadd000:addnew)
001500040910     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600040804     D* Schiera p.o. da trasferire
001700040804     D  SKPO           s              3    dim(500)
001800030211      *
001900040915     d Kpdr            s                   like(fGtpdr)
002000040915     d wpdrfine        s                   like(fGtpdr)
002100030211      *
002200900511     D KPJBA         E DS
002300900523     C*****************************************************************
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600040804     C                   parm                    PARAM          1500
002700040910      *
002800040804     c     ' ':'0'       xlate     param         param
002900040804     c
003000040804     c                   movea     param         skpo
003100950621     C*---------------------------------------------------------------*
003200040915     C     KFgT          KLIST
003300040915     C                   KFLD                    FgTPDR
003400040915     C                   KFLD                    FgTsml
003500040915     C                   KFLD                    FgTtsr
003600040915     C                   KFLD                    FgTctr
003700040915     C                   KFLD                    FgTprg
003701090115     C     KtgT          KLIST
003702090115     C                   KFLD                    FgTPDR
003703090115     C                   KFLD                    FgTsml
003706090115     C                   KFLD                    FgTprg
003707110916     C     Kadd          KLIST
003708110916     C                   KFLD                    adtpdr
003710110916     C                   KFLD                    adtprg
003800040915     C*---------------------------------------------------------------*
003900040804     c                   z-add     1             x                 4 0
004000040804    1c                   dow       skpo(x)>*zeros
004100040915     C                   CLEAR                   Kpdr
004200040915     C                   eval      wpdrfine=9999999
004300040915     c                   movel     skpo(X)       kpdr
004400040915     c                   movel     skpo(X)       wpdrfine
004500040915     C     kpdr          SETLL     fifgt01l
004600040915     C                   READ      fifgt01l                               30
004700040910     c*
004800040915     c                   dow       not %eof(fifgt01l)  and fgtpdr<=wpdrfine
004900040804     C*
005000040910     C** ELABORAZIONE RECORDS relativi
005100040915     C                   EXSR      elaFgT
005200040804     C*
005300040915     C                   READ      fifgt01l                               30
005400040804    2C                   ENDDO
005401110916
005402110916     C** ELABORAZIONE RECORDS relativi a tariffe aff/def TESTATE- dettagli
005403110916     C     kpdr          SETLL     fiadt01l
005404110916     C                   READ      fiadt01l                               30
005405110916     c*
005406110916     c                   dow       not %eof(fiadt01l)  and adtpdr<=wpdrfine
005407110916     C*
005408110916     C** ELABORAZIONE RECORDS relativi
005409110916     c* add
005410110916     c     kadd          setll     fiadd01l
005411110916     c                   do        *hival
005412110916     c     kadd          reade     fiadd01l
005414120307     c                   if        %eof(fiadd01l)
005415120307     c                   leave
005416120307     c                   end
005417110916     c                   if        addatb=' '
005418110916     C                   WRITE     addNEW
005419110916     c                   add       1             totadd            7 0
005420110916     c                   endif
005422110916     c                   enddo
005423110916     c                   add       1             totadt            7 0
005424110916     C                   WRITE     adtNEW
005425110916     C*
005426110916     C                   READ      fiadt01l                               30
005427110916    2C                   ENDDO
005500040915
005600040804     C                   ADD       1             X
005700040804     C                   ENDDO
005800900523     C*
005900040910     c                   EXCEPT    RINUM1
006000900523     C                   SETON                                        LR
006100040804     C**-------------------------------------------------------------**
006200040910     C**  elabora record relativi                                    **
006300040804     C**-------------------------------------------------------------**
006400040915     C     ELAFGT        BEGSR
006500090115     C* per le tariffe prima del 1/2/2009 non esiste la supertestata
006501090115     c* quindi non parto da questa ma la considero un eventuale "dettaglio"
006601090115     C     KtgT          SETLL     fitgtnew                               31
006602090115    0C                   IF        not *in31
006603090115     C     KtgT          chain     fitgt01l
006604090115    0C                   IF        %found(fitgt01l) and tgtatb=' '
006605090115     c                   add       1             tottgt            7 0
006606090115     C                   WRITE     tgtNEW
006607090115     c                   end
006608090115     c                   end
006609090115     C*
006700040915     C     KFgT          SETLL     fifgtnew                               31
006800040804     C* se non trovata
006900040915    0C                   IF        not *in31  and fgtatb=' '
007000040915     C                   WRITE     FgTNEW
007100040915     c                   add       1             totfgt            7 0
007200040915     c* FPT
007300040915     c     kfgt          setll     fifpt01l
007400040915     c     kfgt          reade     fifpt01l
007500040915     c                   dow       not %eof(fifpt01l)
007600040915     c                   if        fptatb=' '
007700040915     C                   WRITE     FptNEW
007800040915     c                   add       1             totfpt            7 0
007900040915     c                   endif
008000040915     c     kfgt          reade     fifpt01l
008100040915     c                   enddo
008200040915     c* FPD
008300040915     c     kfgt          setll     fifpd01l
008400040915     c     kfgt          reade     fifpd01l
008500040915     c                   dow       not %eof(fifpd01l)
008600040915     c                   if        fpdatb=' '
008700040915     C                   WRITE     FpdNEW
008800040915     c                   add       1             totfpd            7 0
008900040915     c                   endif
009000040915     c     kfgt          reade     fifpd01l
009100040915     c                   enddo
009200040915
009300040915     c                   endif
009400040804     C*
009500040804     C                   ENDSR
009600040910      *---------------------------------------------------------------*
009700040910     oQSYSPRT   E            RINUM1         2 02
009800040915     o                                         +  5 'TARIFFE AUTOTRASP.'
009900040915     o                                         +  5 'Fits24R'
010000040910     o                       UDATE             +  2 '  /  /  '
010100040910     o          E            RINUM1      1
010200090115     o                                         +  3 'FITGT00F'
010201090115     o                       TOTTGT        2   +  2
010202090115     o                                         +  3 'FIFGT00F'
010300040915     o                       TOTFGT        2   +  2
010400040915     o                                         +  3 'FIFPT00F'
010500040915     o                       TOTFPT        2   +  2
010600040915     o                                         +  3 'FIFPD00F'
010700040915     o                       TOTFPD        2   +  2
010800110916     o                                         +  3 'FIADT00F'
010900110916     o                       TOTadt        2   +  2
011000110916     o                                         +  3 'FIADD00F'
011100110916     o                       TOTadd        2   +  2
