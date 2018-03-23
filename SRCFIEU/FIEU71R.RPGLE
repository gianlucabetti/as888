000100030620     ************************************************************************
000200030623      *   IMPORTA DATI da FEDEX                                             *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030623     FFIFEI00W  UF   E             DISK    rename(FIFEI00W:FIFEI0)
000700030623     FFIFEIS0W  O  a E             DISK    rename(FIFEI00W:FIFEIW)
000800030623     FTNEFE00F  O  a E             DISK
000900041207     Ffieu71D   CF   E             WORKSTN
001000030620      *
001100030623     D TRTCIA1DS     e ds
001200030623     D TRTCIA2DS     e ds
001300030623     D TRTCIB1DS     e ds
001400030623     D TRTCIB2DS     e ds
001500030623     D TRTCIC1DS     e ds
001600030623     D TRTCID1DS     e ds
001700030623     D TRTCIE1DS     e ds
001800030623     D TRTCIF1DS     e ds
001900030623     D TRTCIW1DS     e ds
002000030623     D TRTCIZ1DS     e ds
002100030623     D TRTCIZ2DS     e ds
002200041207      *
002300041207     D CNCR80        E DS
002400041207     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500030623      *
002600041207     D WLBDAT          DS
002700041207     D  G02DAT                 1      8  0
002800041207     D  G02INV                 9     16  0
002900041207     D  G02ERR                17     17
003000041207     D  G02TGI                18     22  0
003100041207      *
003200041207     D  almeno_uno     s              1
003300041207      *
003400041207     D digits          C                   CONST('0123456789')
003500030623      *---------------------------------------------------------------------*
003600030623     c* il problema di ricevere un file txt con la virgola nei campi
003700030623     c* numerici costringe ad eseguire un passaggio fra workfile e file
003800030623     c* con i campi formattati in modo normale tramite dds.
003900030623      *---------------------------------------------------------------------*
004000041207      *
004100041207     C* Imposta la UDATE  da TIME
004200041207     C                   TIME                    W0140            14 0
004300041207     C                   MOVE      W0140         UDATE8            8 0
004400041207     C                   Z-ADD     1             CODUT
004500041207     C                   CALL      'X§PARUT'
004600041207     C                   PARM                    UT§DSE
004700041207     C                   MOVEL     RAGUT         RSUT             20
004800041207     C                   MOVEL     RAGUT         RSUT2            20
004900041207     C                   MOVEL     REC80         CNCR80
005000041207     c                   clear                   almeno_uno
005100041207      *
005200041207      * Emetto il formato video
005300041207     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
005400041207     C     Video1        TAG
005500041207     C                   SETOFF                                       90
005600041207     C                   EXFMT     eu71d01
005700041207     C   KC              GOTO      FINE                                         F3=USCITA
005800041207      * Eseguo i  controlli sui campi immessi a video
005900041207     C                   EXSR      CONTR
006000041207      * Se non vi sono errori ma non e' premuto F6 emetto video
006100041207     C   90
006200041207     CorNkf              GOTO      Video1
006300041207      *
006400030623     c                   clear                   numfat            9 0
006500030623     c                   clear                   datfat            8 0
006600030623     c                   clear                   valfat           15 2
006700030623      *
006800030623     c     *start        setll     FiFEI00w
006900030623      *
007000030620     c                   do        *hival
007100030623      *
007200030623     c                   read      FiFEI00w
007300030623     c                   if        %eof(FiFEI00w)
007400030620     C                   leave
007500030620     C                   Endif
007600041207      *
007700030623     c                   clear                   TNEFE000
007800030623      *
007900030623      *  controlla il Tipo record.
008000030623      *   deve leggere solamente il tipo H1 -> testata e
008100030623      *                                  F1 -> dettaglio importo causale
008200030623      * Dovendo però leggere tutta la fattura deve cambiare i dati di testata a rottura
008300030623      *  della fattura e allo stesso modo deve fare per l'AWB.
008400030623      *
008500030623      *  A rottura di fattura imposta i dati di testata nuovamente
008600030623     C                   if        %subst(wrkdat:1:3)= 'HA1'
008700030623     c                   EVAL      trtciA1ds = wrkdat
008800030623     c                   MOVE      ha1inn        NUMFAT
008900030623     c                   MOVE      ha1inD        DATFAT
009000041207      * Attenzione:
009100041207      * Deve caricare fino alla data Fattura richiesta a Video
009200041207      *    esce per data superiore
009300041207     c                   if        datfat > alamg
009400041207     C                   leave
009500041207     C                   Endif
009600030623      *
009700030623     c                   if        ha1chs = '+'
009800030623     c                   z-add     ha1tic        valfat
009900030623     c                   else
010000030623     c                   z-sub     ha1tic        valfat
010100030623     c                   end
010200030623      *
010300030623     c                   endIF
010400030623      *
010500030623      *  Dati di testata
010600030623     c                   eval      EFENRF = numfat
010700030623     c                   eval      EFEDTF = datfat
010800030623     c                   eval      EFEVAF = valfat
010900030623      *
011000030623     C                   if        %subst(wrkdat:1:3)= 'DF1'
011100030623     c                   EVAL      trtciF1ds = wrkdat
011200030623     c                   eval      EFEawb = DF1TRN
011300030623     c                   eval      EFEcau = DF1CHC
011400030623     c                   eval      EFEdiv = DF1BCC
011500030623     c                   if        DF1CBS = '+'
011600030623     c                   z-add     DF1BCA        EFEvad
011700030623     c                   else
011800030623     c                   z-sub     DF1BCA        EFEvad
011900030623     c                   end
012000041207     c                   move      'S'           almeno_uno
012100030623     c                   write     tnefe000
012200030623     c                   endIF
012300030620      *
012400030623     c                   write     FiFEIw
012500030623     c                   delete    FiFEI0
012600030620      *
012700030620     C                   ENDdo
012800161298      *----------------------------------------------------------------
012900041207      *  Se ha elaborato almeno un record allora chiama il pgm seguente
013000041207      *   che riporta i dati sul TNEFR finale.
013100041207     C                   if        almeno_uno = 'S'
013200041207     c                   call      'FIEU71R1'
013300041207     C                   END
013400041207      *----------------------------------------------------------------
013500041207     c     FINE          tag
013600030620     c                   seton                                        LR
013700041207      *--------------------------------------------------------------*
013800041207      * Routine per i controlli sui campi del formato video          *
013900041207      *--------------------------------------------------------------*
014000041207     C     CONTR         BEGSR
014100041207      *
014200041207     c                   movea     '0000000000'  *in(41)
014300041207     c                   movea     '0000000000'  *in(51)
014400041207      * --------------
014500041207      * controllo immissione delle date
014600041207      *
014700041207     C                   Z-ADD     d1Dal         G02DAT
014800041207     C                   MOVEL     *BLANK        G02ERR
014900041207     C                   CALL      'XSRDA8'
015000041207     C                   PARM                    WLBDAT
015100041207     C     G02ERR        IFEQ      '1'
015200041207     C                   SETON                                        42  90
015300041207     C                   GOTO      endct1
015400041207     C                   ENDIF
015500041207     c*
015600041207     C                   Z-ADD     G02INV        AlAmG             8 0
015700041207     C                   Z-ADD     G02dat        d1dal
015800041207      *
015900041207     C     endct1        ENDSR
016000041207      *--------------------------------------------------------------*
