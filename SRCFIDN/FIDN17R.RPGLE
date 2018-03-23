000100981008      *-------------------------------------------------------------------------*
000200981008      *    INVIO A SEDE DOCUMENTI PER DENUNCIA EVENTI                           *
000300981008      *-------------------------------------------------------------------------*
000400980721
000500980721      ****************************************************************
000600981026      *  FASI C.A. FISSATE A PROGRAMMA
000700980721      ****************************************************************
000800980721      ***************************************************************
000900980623
001000980521      ****************************************************************
001100980521      *  RIEPILOGO INDICATORI
001200980521      ***************************************************************
001300980904      * 30 - COMODO
001400980904      * 31 - COMODO
001500980904      ***************************************************************
001600980623
001700980521     H DECEDIT('0,') DATEDIT(*DMY.)
001800980623
001900981027     FFIDN00D   CF   E             WORKSTN  sfile(FI00S02:nrr1)
002000981026     FFNDET01L  IF   E           K DISK
002100981027     FFNDCT01L  UF   E           K DISK
002200980929     FFNDCF01L  UF A E           K DISK
002300980521      *------------------------------------------------------------------------*
002400980929     D Wpta            S              3
002500980907     D xx              S              3  0
002600980721     D W0140           S             14  0
002700980521     D Wdtgio          S              8  0
002800980521     D dateu           S              8  0
002900980529     D nrr1            S              4  0
003000980623     D nrr2            S              4  0
003100980929     D WORA            S              4  0
003200981027
003300980521      *   S C H I E R E
003400980929     D PTA             S              3    DIM(100)                             PTR + TAD
003500980929     D GFS             S              1    DIM(100)                             gestione fase
003600981027
003700980521      *   D S   I N T E R N E / E S T E R N E
003800980521     D WLBDAT          DS                  INZ
003900980521     D  G02DAT                 1      8  0
004000980521     D  G02INV                 9     16  0
004100980521     D  G02ERR                17     17
004200980521     D  G02TGI                18     22  0
004300980525      *
004400980915      *----------------------------------
004500980929     D FIDN00DS      E DS
004600980529     D FIDN05DS      E DS
004700980529     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
004800981229     D DDCT01        E DS
004900980521     D KPJBA         E DS
005000980930      *
005100980521     D                SDS
005200980521     D  VTCPGM                 1     10
005300980623      *------------------------------------------------------------------------*
005400980521      *---------------  C A L C O L O  ----------------------------------------*
005500980521      *------------------------------------------------------------------------*
005600981026
005700981026      * Recupero dati mancanti dell'Evento
005800981026     c     KDET          CHAIN     FNDET000                           30
005900981027     c                   movel     DETtad        I00tae
006000981027     c                   z-add     DETfce        I00fce
006100981027     c                   z-add     DETmge        I00dev
006200981027     c                   movel     DETaae        I00dev
006300980623
006400981026      * Richiamo PGM di stampa lettera
006500981027     C                   movel     Fidn00ds      Kpjbu
006600981026     c                   call      'FIDN09R'
006700981026     c                   parm                    kpjba
006800980623
006900981026      * Aggiorno testata C.A. e file fasi
007000981027     c                   IF        i00nce = 1
007100981027     c                   exsr      CA_singola
007200981027     c                   ELSE
007300981027     c                   exsr      CA_multi
007400981027     c                   ENDIF
007500981026
007600980907     c                   EVAL      *INLR = *ON
007700980910      *****************************************************************
007800981027      * AGGIORNO FILES SE SELEZIONATA UNA C.A.
007900980910      *****************************************************************
008000981027     C     CA_singola    BEGSR
008100980929
008200981027     C     KDCT00        CHAIN     FNDCT000                           30
008300981027
008400981027      * Ricavo gestore fase successiva
008500981027     c                   EXSR      FASEsucc
008600981027
008700981126      *  Memorizzo in schiera i parametri
008800981126     c                   IF        O05err = *blanks
008900981126     c                   movel     O05REC        DS_FNDFA
009000981126     c                   ENDIF
009100981126
009200981027      * Imposto campi per trasmissione dati
009300981027     c                   EXSR      RIEfndct
009400981027
009500981027      * Aggiorno testata C.A.
009600981027     c                   EXCEPT    UPDDCT
009700981027
009800981027      * Scrivo fasi C.A.
009900981027     c                   EXSR      WRTDCF
010000981027
010100981027     c                   ENDSR
010200981027      *****************************************************************
010300981027      * AGGIORNO FILES LEGGENDO SUBFILE
010400981027      *****************************************************************
010500981027     C     CA_multi      BEGSR
010600981027
010700981026      * Leggo  subfile
010800980929     c                   z-add     1             nrr2
010900981027     c     NRR2          CHAIN     FI00S02                            31
011000981026      *
011100981027      * Elaboro se C.A. non esclusa e scelta non bloccata
011200981027     c     *IN31         DOWEQ     *OFF
011300981027     c                   IF        V2Csce <> 'E'  and  V2Hi82 <> '1'
011400981027      *
011500981027     C     KDCT          CHAIN     FNDCT000                           30
011600980929      *
011700981027      * Ricavo gestore fase successiva
011800981027      *        ricerco in schiera se parametri già elaborati, altrimenti calcolo
011900981027     c                   z-add     1             XX
012000980929     c                   eval      WPTA = DCTptr + DCTtad
012100980929     c     WPTA          LOOKUP    PTA(XX)                                30
012200980929      *
012300980929     c                   IF        *IN30 = *ON
012400980929     c                   movel     GFS(XX)       DFAgfs
012500980929     c                   ELSE
012600981027      *
012700981027      *  Calcolo la fase successiva
012800981027     c                   EXSR      FASEsucc
012900981027      *
013000981027      *  Memorizzo in schiera i parametri
013100980929     c                   IF        O05err = *blanks
013200980929     c                   movel     O05REC        DS_FNDFA
013300980929     c                   Z-add     1             XX
013400980929     c     *blanks       LOOKUP    PTA(XX)                                30
013500980929     c   30              eval      PTA(XX) = DCTptr + DCTtad
013600980929     c   30              movel     DFAgfs        GFS(XX)
013700980929     c                   ENDIF
013800980929      *
013900980929     c                   ENDIF
014000981027      *
014100981027      * Imposto campi per trasmissione dati
014200981027     c                   EXSR      RIEfndct
014300981027      *
014400981027      * Aggiorno testata C.A.
014500981027     c                   EXCEPT    UPDDCT
014600981027      *
014700981027      * Scrivo fasi C.A.
014800981027     c                   EXSR      WRTDCF
014900981027      *
015000981027     c                   ENDIF
015100981027      *
015200980929     c                   add       1             nrr2
015300981027     c     NRR2          CHAIN     FI00S02                            31
015400980929     c                   ENDDO
015500980929      *
015600980910     c                   ENDSR
015700980929      *****************************************************************
015800981027      * RICAVO FASE SUCCESSIVA
015900980929      *****************************************************************
016000981027     C     FASEsucc      BEGSR
016100980929      *
016200981027     c                   CLEAR                   FIDN05DS
016300981027     c                   movel     'F'           I05MOD
016400981027     c                   z-add     I00fca        I05fca
016500981027     c                   movel     'P'           I05fpr
016600981027     c                   movel     dctptr        I05tpc
016700981027     c                   movel     'O'           I05ffs
016800981027     c                   movel     'E'           I05fpe
016900981030     c                   movel     'D'           I05fde
017000981027     c                   z-add     DATEU         i05dta
017100981027     c                   move      dcttad        i05tad
017200981125      * Valorizzo numero CA
017300981125     c                   z-add     DCTaac        i05aac
017400981125     c                   z-add     DCTfil        i05fil
017500981125     c                   z-add     DCTnca        i05nca
017600981027
017700981027     C                   CALL      'FIDN05R'
017800981027     c                   PARM                    KPJBA
017900981027     c                   PARM                    FIDN05DS
018000981027      *
018100981027     c                   ENDSR
018200981027      *****************************************************************
018300981027      * IMPOSTO CAMPI DI FNDCT
018400981027      *****************************************************************
018500981027     C     RIEfndct      BEGSR
018600981027      *
018700981229     c                   movel     DCTflo        DDCT01
018800981229      *
018900981027     c                   SELECT
019000981027     c                   WHEN      DFAgfs = 'P'
019100981229     c                   movel     §DCTlnpc      dctgfc
019200981027     c                   WHEN      DFAgfs = 'S'
019300981027     c                   z-add     46            dctgfc
019400981027     c                   WHEN      DFAgfs = 'A'
019500981027     c                   z-add     DCTlna        dctgfc
019600981027     c                   OTHER
019700981027     c                   z-add     DCTfil        dctgfc
019800981027     c                   ENDSL
019900981027      *
020000981027     c                   z-add     I00fca        DCTfca
020100981222     c                   clear                   DCTft1
020200981222     c                   clear                   DCTft2
020300981027      *
020400981027     c                   ENDSR
020500981027      *****************************************************************
020600981027      * AGGIORNO FNDCF FASI C.A.
020700981027      *****************************************************************
020800981027     C     WRTDCF        BEGSR
020900981027      *
021000980929     C                   z-add     DCTaaC        DCFaac
021100980929     C                   z-add     DCTfil        DCFfil
021200980929     C                   z-add     DCTnca        DCFnca
021300981027     c                   z-add     I00fca        DCFfca
021400980929     c                   z-add     dateu         DCFdfc
021500980929     c                   z-add     wora          DCFhfc
021600981027     c                   z-add     I00fgs        DCFfev
021700980929     c                   movel     knmus         DCFpru
021800980929      *
021900980929     c                   WRITE     FNDCF000
022000980929      *
022100980929     c                   ENDSR
022200980910      *****************************************************************
022300980527      * ROUTINE INIZIALE
022400980527      *****************************************************************
022500980527     C     *INZSR        BEGSR
022600980615      *
022700980521     C     *ENTRY        PLIST
022800980521     C                   PARM                    KPJBA
022900981026      *
023000981026     C                   MOVEL     KPJBU         FIDN00DS
023100980907      *
023200980721      * reperisco data e ora
023300980721     C                   TIME                    W0140
023400980521      * UDATE IN GGMMAAAA
023500980521     C                   MOVE      W0140         WDTGIO
023600980929     C                   MOVEL     W0140         WORA
023700980521      * UDATE IN AAAAMMGG
023800980521     C                   Z-ADD     WDTGIO        G02DAT
023900980521     C                   MOVEL     *BLANK        G02ERR
024000980521     C                   CALL      'XSRDA8'
024100980521     C                   PARM                    WLBDAT
024200980521     C                   MOVEL     G02INV        DATEU
024300980930      *    Imposto anno denuncia
024400980930     c                   move      wdtgio        V1Caae
024500981027      *
024600981026     C     KDET          klist
024700981026     C                   KFLD                    I00AAE
024800981026     C                   KFLD                    I00NEV
024900981026      *
025000981026     C     KDCT00        KLIST
025100981026     C                   KFLD                    I00AAC
025200981026     C                   KFLD                    I00FIL
025300981026     C                   KFLD                    I00NCA
025400980522      *
025500980929     C     KDCT          KLIST
025600980929     C                   KFLD                    V2HAAC
025700980929     C                   KFLD                    V2CFIL
025800980929     C                   KFLD                    V2CNCA
025900981027      *
026000980521     C                   ENDSR
026100980521      *---------------------------------------------------------------------------------------------
026200981027     OFNDCT000  E            UPDDCT
026300980929     O                       DCTgfc
026400980929     O                       DCTfca
026500981222     O                       DCTft1
026600981222     O                       DCTft2
