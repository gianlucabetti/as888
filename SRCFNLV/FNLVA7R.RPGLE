000100930527     H DECEDIT('0,') DATEDIT(*DMY.)
000200070914     H* FNLVA7R *-----------------------------------------------------*
000300070914     H* - Calcolo numero stop per tassazione padroncini per ritiri
000400930322     H*---------------------------------------------------------------*
000500930322     H* INDICATORI USATI                                              *
000600930322     H*---------------------------------------------------------------*
000700941231     H* 30 - DI COMODO
000800941231     H* 31 - DI COMODO
000900930322     H*---------------------------------------------------------------*
001000930322     H* DATA BASE                                                     *
001100930322     H*---------------------------------------------------------------*
001200070914     Ffiftd01L  IF   E           K DISK
001300070914     Ffiftd04L  iF   E           K DISK
001400070914     F                                     RENAME(fiftd000:fiftd004)
001500070914     Ffncls01l  IF   E           K DISK
001600930322     D*------------------
001700930322     D* PARAMETRI ESTERNI
001800930322     D*------------------
001900930322     D KPJBA         E DS
002000070914     D fnlva7ds      E DS
002100070914     D*------------------
002200070914     d w_tsr           s                   like(ftdtsr) inz('R')
002300930322     C*---------------------------------------------------------------*
002400070914     C     *ENTRY        PLIST
002500070914     C                   PARM                    KPJBA
002600070914     C                   PARM                    fnlva7ds
002700070914     c                   clear                   oa7stp
002800970902     C**
002900070914     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
003000070914     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DELLO STOP
003100070914     C                   Z-ADD     ia7ksc        w0040             4 0
003200070914    1C     w0040         IFNE      *ALL'8'
003300070914     C     w0040         ANDNE     *ALL'9'
003400070914     C     ia7ksc        CHAIN     FNCLS000
003500070914     c                   if        %found(fncls01l)
003600070914     C                   z-add     CLSSTP        oa7stp
003700070914     c                   endif
003800070914    1C                   ENDIF
003900070914    1c     oa7stp        ifeq      *zeros
004000070914     C     kftd4         CHAIN     fiftd004
004100070914    2c                   if        %found(fiftd04l)
004200070914     c                   z-add     ftdstp        oa7stp
004300070914    2c                   endif
004400070914     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
004500070914     c* nella distinta il primo numero disponibile
004600070914    2c     oa7stp        ifeq      *zeros
004700070914     C     kftd1         setgt     fiftd000
004800070914     C     kftd1         readpe    fiftd000
004900070914    3c                   if        not %eof(fiftd01l)
005000070914     c                   eval      oa7stp = (ftdstp+1)
005100070914    4c     oa7stp        iflt      1000
005200070914     c                   z-add     1000          oa7stp
005300070914    4c                   endif
005400070914   x3c                   else
005500070914     c                   z-add     1000          oa7stp
005600070914    3c                   endif
005700070914    2c                   endif
005800070914     c*
005900070914    1c                   endif
006000970902     C**
006100070914     C                   SETON                                        LR
006200070703     c
006300950105     C*---------------------------------------------------------------*
006400950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
006500950105     C*---------------------------------------------------------------*
006600070914     C     *inzsr        BEGSR
006700950105     C*
006800950105     C*
006900070914     C     kftd4         KLIST
007000070914     C                   KFLD                    ia7pdr
007100070914     C                   KFLD                    w_tsr
007200070914     C                   KFLD                    ia7ndc
007300070914     C                   KFLD                    ia7ddc
007400070914     C                   KFLD                    ia7ksc
007500070914     C                   KFLD                    ia7rsm
007600070914     C     kftd1         KLIST
007700070914     C                   KFLD                    ia7pdr
007800070914     C                   KFLD                    w_tsr
007900070914     C                   KFLD                    ia7ndc
008000070914     C                   KFLD                    ia7ddc
008100950105     C*
008200950105     C                   ENDSR
