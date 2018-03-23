000100030620     ************************************************************************
000200030623      *   IMPORTA DATI da FEDEX                                             *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030623     FFIFEI00W  UF   E             DISK    rename(FIFEI00W:FIFEI0)
000700030623     FFIFEIS0W  O  a E             DISK    rename(FIFEI00W:FIFEIW)
000800030623     FTNEFE00F  O  a E             DISK
000900030620      *
001000030623     D TRTCIA1DS     e ds
001100030623     D TRTCIA2DS     e ds
001200030623     D TRTCIB1DS     e ds
001300030623     D TRTCIB2DS     e ds
001400030623     D TRTCIC1DS     e ds
001500030623     D TRTCID1DS     e ds
001600030623     D TRTCIE1DS     e ds
001700030623     D TRTCIF1DS     e ds
001800030623     D TRTCIW1DS     e ds
001900030623     D TRTCIZ1DS     e ds
002000030623     D TRTCIZ2DS     e ds
002100030623      *
002200030623      *---------------------------------------------------------------------*
002300030623     c* il problema di ricevere un file txt con la virgola nei campi
002400030623     c* numerici costringe ad eseguire un passaggio fra workfile e file
002500030623     c* con i campi formattati in modo normale tramite dds.
002600030623      *---------------------------------------------------------------------*
002700030623     c                   clear                   numfat            9 0
002800030623     c                   clear                   datfat            8 0
002900030623     c                   clear                   valfat           15 2
003000030623      *
003100030623     c     *start        setll     FiFEI00w
003200030623      *
003300030620     c                   do        *hival
003400030623      *
003500030623     c                   read      FiFEI00w
003600030623     c                   if        %eof(FiFEI00w)
003700030620     C                   leave
003800030620     C                   Endif
003900030623      *
004000030623     c                   clear                   TNEFE000
004100030623      *
004200030623      *  controlla il Tipo record.
004300030623      *   deve leggere solamente il tipo H1 -> testata e
004400030623      *                                  F1 -> dettaglio importo causale
004500030623      * Dovendo però leggere tutta la fattura deve cambiare i dati di testata a rottura
004600030623      *  della fattura e allo stesso modo deve fare per l'AWB.
004700030623      *
004800030623      *  A rottura di fattura imposta i dati di testata nuovamente
004900030623     C                   if        %subst(wrkdat:1:3)= 'HA1'
005000030623     c                   EVAL      trtciA1ds = wrkdat
005100030623     c                   MOVE      ha1inn        NUMFAT
005200030623     c                   MOVE      ha1inD        DATFAT
005300030623      *
005400030623     c                   if        ha1chs = '+'
005500030623     c                   z-add     ha1tic        valfat
005600030623     c                   else
005700030623     c                   z-sub     ha1tic        valfat
005800030623     c                   end
005900030623      *
006000030623     c                   endIF
006100030623      *
006200030623      *  Dati di testata
006300030623     c                   eval      EFENRF = numfat
006400030623     c                   eval      EFEDTF = datfat
006500030623     c                   eval      EFEVAF = valfat
006600030623      *
006700030623     C                   if        %subst(wrkdat:1:3)= 'DF1'
006800030623     c                   EVAL      trtciF1ds = wrkdat
006900030623     c                   eval      EFEawb = DF1TRN
007000030623     c                   eval      EFEcau = DF1CHC
007100030623     c                   eval      EFEdiv = DF1BCC
007200030623     c                   if        DF1CBS = '+'
007300030623     c                   z-add     DF1BCA        EFEvad
007400030623     c                   else
007500030623     c                   z-sub     DF1BCA        EFEvad
007600030623     c                   end
007700030623     c                   write     tnefe000
007800030623     c                   endIF
007900030620      *
008000030623     c                   write     FiFEIw
008100030623     c                   delete    FiFEI0
008200030620      *
008300030620     C                   ENDdo
008400030620     c
008500161298      *----------------------------------------------------------------
008600030620     c                   seton                                        LR
