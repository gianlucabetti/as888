000100981007      *-------------------------------------------------------------------------*
000200010405      *    Sollecito Avvisi danni mai pervenuti                                 *
000300981007      *-------------------------------------------------------------------------*
000400981016      *
000500981013      *
000600980521      ****************************************************************
000700980521      *  RIEPILOGO INDICATORI
000800980521      ***************************************************************
000900980923      * 28 - ERRORE GENERICO DSPF
001000010406      * 50 - ERRORE Data dal errata
001100010405      * 51 - ERRORE Data dal minore di data al
001200010406      * 52 - ERRORE Data al  errata
001300980904      ***************************************************************
001400980623
001500980521     H DECEDIT('0,') DATEDIT(*DMY.)
001600980623
001700010405     fFIDN62D   CF   E             WORKSTN
001800980623
001900980521      *------------------------------------------------------------------------*
002000010403
002100010406     D w_viddal        S                   LIKE(Viddal)
002200010406     D w_vidall        S                   LIKE(Vidall)
002300980923
002400010405      *  titolo videata (lunghezza massima 36)
002500010405     D TIT_soll        C                   CONST('SOLLECITO AVVISI DANNO MAI PE-
002600010405     D                                     RVENUTI')
002700980923      *
002800980915      *----------------------------------
002900010403     D KPJBA         E DS
003000980529     D UT§DSE0F      E DS
003100980521      *
003200980521     D                SDS
003300980521     D  VTCPGM                 1     10
003400980623
003500010406      *
003600010406     D WLBDAT          DS                  INZ
003700010406     D  G02DAT                 1      8  0
003800010406     D  G02INV                 9     16  0
003900010406     D  G02ERR                17     17
004000010406     D  G02TGI                18     22  0
004100010406      *
004200010406     D param           DS                  inz
004300010406     d  datadal                       8  0
004400010406     d  dataall                       8  0
004500010406      *
004600990122     c*---------------------------------------------------------------------------------------------
004700010403     c*                   C   I   C   L   O
004800990122     c*---------------------------------------------------------------------------------------------
004900010404      *
005000010406     c                   write     FI62t01
005100010406     c                   write     FI62z01
005200010403      *
005300981013     C                   do        *hival
005400980623
005500980521      * Emissione VIDEO1
005600010406     c                   exfmt     FI62d01
005700980923
005800010403     c                   setoff                                       2890
005900980623
006000980525      * f3=Fine
006100981013     c   kc              leave
006200980623
006300981014      * Controlli  videata
006400980521     c                   exsr      ctrd01
006500010406      * se è acceso il 28 ritorno ad emettere il formato video
006600010403     c                   if        *in28 or *in90
006700981105     c                   iter
006800981105     c                   endif
006900990511      *
007000981014      * Se non ho confermato o ci sono errori riemetto video
007100981015     c                   IF        *INKF = *ON
007200010403      *
007300010406      * richiamo pgm di stampa solleciti in batch
007400010403      *
007500010411     C                   EVAL      datadal = W_viddal
007600010411     c                   eval      dataall = W_vidall
007700010403      *
007800010403     c                   clear                   kpjbu
007900010406     c                   movel     param         kpjbu
008000010403     c*
008100010406     C                   MOVEL     'DN31'        KCOAZ
008200010403     C                   CALL      'BCH10'
008300010403     C                   PARM                    KPJBA
008400010403      *
008500010403     c
008600981015     c                   leave
008700980908     c                   ELSE
008800981013     c                   iter
008900980908     c                   ENDIF
009000981014      *
009100981014     c                   enddo
009200980623
009300990122      *
009400980907     c                   EVAL      *INLR = *ON
009500980521      **********************************************************************
009600980521      * CONTROLLI VIDEO 1
009700980521      **********************************************************************
009800980923     C     CTRD01        BEGSR
009900010403      *
010000010406      * Controllo DATA dal
010100010406    2c                   if        viddal = *zeros
010200010406     c                   seton                                        5028
010300010403     c                   goto      ectrd01
010400010403    2c                   endif
010500010406      * controllo se data dal corretta
010600010403      *
010700010406     C                   clear                   W_viddal
010800010406     C                   MOVE      Viddal        G02DAT
010900010406     C                   eval      g02err = *blanks
011000010406     C                   CALL      'XSRDA8'
011100010406     C                   PARM                    WLBDAT
011200010406     C                   if        G02ERR = '1'
011300010406     C                   SETON                                        2850
011400010406     C                   goto      ectrd01
011500010406     C                   else
011600010406     C                   MOVEL     G02DAT        Viddal
011700010411     C                   MOVE      G02INV        W_viddal
011800010406     C                   endif
011900010406      *
012000010406      * Controllo DATA al
012100010406    2c                   if        vidall = *zeros
012200010406     c                   seton                                        5228
012300010406     c                   goto      ectrd01
012400010406    2c                   endif
012500010406      * controllo se data dal corretta
012600010406      *
012700010406     C                   clear                   W_vidall
012800010406     C                   MOVE      Vidall        G02DAT
012900010406     C                   eval      g02err = *blanks
013000010406     C                   CALL      'XSRDA8'
013100010406     C                   PARM                    WLBDAT
013200010406     C                   if        G02ERR = '1'
013300010406     C                   SETON                                        2852
013400010406     C                   goto      ectrd01
013500010406     C                   else
013600010406     C                   MOVEL     G02DAT        Vidall
013700010406     C                   MOVE      G02INV        W_vidall
013800010406     C                   endif
013900010406      * Data dal deve essere minore di data al
014000010403      *
014100010406     c                   if        W_viddal > W_vidall
014200010406     c                   seton                                        5128
014300010403     c                   goto      ectrd01
014400010403     C                   endif
014500980923
014600980923     c     Ectrd01       ENDSR
014700980910      *****************************************************************
014800980527      * ROUTINE INIZIALE
014900980527      *****************************************************************
015000980527     C     *INZSR        BEGSR
015100980615      *
015200980521     C     *ENTRY        PLIST
015300980521     C                   PARM                    KPJBA
015400980907      *
015500980907     C                   Z-ADD     1             CODUT
015600980907     C                   CALL      'X§PARUT'
015700980907     C                   PARM                    UT§DSE0F
015800980907     C                   MOVEL     RAGUT         RSUT
015900010403      *
016000981007      * Imposto il TITOLO
016100981014      * chiusura
016200010406     c                   movel     TIT_soll      VTCtit
016300010403      *
016400010403     c                   ENDSR
016500980923
