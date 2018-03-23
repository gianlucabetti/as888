000100981007      *-------------------------------------------------------------------------*
000200010403      *    CHIUSURA AUTOMATICA C.A. IN PRESCRIZIONE                             *
000300981007      *-------------------------------------------------------------------------*
000400981016      *
000500981014      ****************************************************************
000600981014      *  FASI C.A. FISSATE A PROGRAMMA
000700981014      ****************************************************************
000800010403      *    545 = Avviso danno               fase minore da utilizzare  (WFMIN)
000900010403      *    635 = Ultima ricezione Documenti fase massima da utilizzare (WFMAX)
001000990311      ****************************************************************
001100990311      *  CAUSALI FISSATE A PROGRAMMA
001200990311      ****************************************************************
001300010403      *    09  = Chiusura C.A. per decorrenza termini                  (WCACH)
001400981014      ***************************************************************
001500981013      *
001600980521      ****************************************************************
001700980521      *  RIEPILOGO INDICATORI
001800980521      ***************************************************************
001900980923      * 28 - ERRORE GENERICO DSPF
002000980904      * 30 - COMODO
002100010403      * 31 - COMODO
002200010403      * 50 - ERRORE fase C.A. richiesta
002300010403      * 51 - ERRORE inserimento obbligatorio fase C.A.
002400010403      * 52 - ERRORE formale fase C.A.
002500010403      * 90 - COMODO riemissione
002600980904      ***************************************************************
002700980623
002800980521     H DECEDIT('0,') DATEDIT(*DMY.)
002900980623
003000010403     fFIDN60D   CF   E             WORKSTN
003100980623
003200980521      *------------------------------------------------------------------------*
003300010403     d Wfase           s                   like(dfafca)
003400010403     d Wfmin           s                   like(dfafca)
003500010403     d                                     inz(545)
003600010403     d Wfmax           s                   like(dfafca)
003700010403     d                                     inz(635)
003800010403     d Wcach           s                   like(i61cch)
003900010403     d                                     inz('09')
004000010403
004100980923
004200980622      *  titolo videata (lunghezza massima 34)
004300010403     D TIT_chca        C                   CONST('     CHIUSURA AUTOMATICA C.A.-
004400981013     D                                          ')
004500980923      *
004600980915      *----------------------------------
004700010403     D KPJBA         E DS
004800980529     D UT§DSE0F      E DS
004900990311     D DCCH          E DS
005000981014     D DSbs02        E DS                  EXTNAME(tibs02ds)
005100010403     D FIDN61DS      E DS
005200010403     D FIDN05DS      E DS
005300010403     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
005400980521      *
005500980521     D                SDS
005600980521     D  VTCPGM                 1     10
005700980623
005800990122     c*---------------------------------------------------------------------------------------------
005900010403     c*                   C   I   C   L   O
006000990122     c*---------------------------------------------------------------------------------------------
006100010404      *
006200010404     c                   write     FI60t01
006300010404     c                   write     FI60z01
006400010403      *
006500981013     C                   do        *hival
006600980623
006700980521      * Emissione VIDEO1
006800010403     c                   exfmt     FI60d01
006900980923
007000010403     c                   setoff                                       2890
007100980623
007200980525      * f3=Fine
007300981013     c   kc              leave
007400980623
007500981014      * Controlli  videata
007600980521     c                   exsr      ctrd01
007700010403      * se è acceso il 28 ritotrno ad emettere il formato video
007800010403     c                   if        *in28 or *in90
007900981105     c                   iter
008000981105     c                   endif
008100990511      *
008200981014      * Se non ho confermato o ci sono errori riemetto video
008300981015     c                   IF        *INKF = *ON
008400010403      *
008500010403      * preparazione DS per richiamo pgm chiusura automatica
008600010403      *
008700010403     c                   clear                   fidn61ds
008800010403     c                   eval      i61tca = 'P'
008900010403     c                   eval      i61fca = wfase
009000010403     c                   eval      i61cch = vidccH
009100010403      *
009200010403     c                   clear                   kpjbu
009300010403     c                   movel     fidn61ds      kpjbu
009400010403     c*
009500010403     C                   MOVEL     'DN61'        KCOAZ
009600010403     C                   CALL      'BCH10'
009700010403     C                   PARM                    KPJBA
009800010403      *
009900010403     c
010000981015     c                   leave
010100980908     c                   ELSE
010200981013     c                   iter
010300980908     c                   ENDIF
010400981014      *
010500981014     c                   enddo
010600980623
010700990122      *
010800981021
010900981029     c                   clear                   dsbs02
011000981029     c                   movel     'C'           t02tla
011100981029     c                   call      'TIBS02R'
011200981029     c                   parm                    kpjba
011300981029     c                   parm                    dsbs02
011400980907     c                   EVAL      *INLR = *ON
011500980521      **********************************************************************
011600980521      * CONTROLLI VIDEO 1
011700980521      **********************************************************************
011800980923     C     CTRD01        BEGSR
011900010403      *
012000010403      * Controllo la fase C.A.
012100010403    2c                   if        vidfca = '   '
012200010403     c                   seton                                        5128
012300010403     c                   goto      ectrd01
012400010403    2c                   endif
012500010403      * controllo la fase
012600010403      *
012700010403     C     '?'           SCAN      vidfca                                 31
012800010403      *
012900010403    3C                   IF        *IN31 = *ON
013000010403     c                   seton                                        90
013100010403     c                   clear                   fidn05ds
013200010403     c                   movel     'R'           I05MOD
013300010403     C                   CALL      'FIDN05R'
013400010403     c                   PARM                    KPJBA
013500010403     c                   PARM                    FIDN05DS
013600010403     c                   if        O05err = 'E'
013700010403     c                   movel     '000'         Vidfca
013800010403     c                   clear                   Viddca
013900010403     c                   else
014000010403     c                   movel     O05REC        DS_FNDFA
014100010403     c                   movel     DFAfca        Vidfca
014200010403     c                   movel     DFAdes        Viddca
014300010403     C                   endif
014400010403      *
014500010403     c                   ELSE
014600010403      *
014700010403     C                   testn                   Vidfca               30
014800010403      * testo ultimo carattere
014900010403     C   30              move      VIDFCA        W001A             1
015000010403     C   30              eval      *in30 = (W001A >= '0')
015100010403      * Se codice non numerico errore
015200010403     C                   IF         *IN30 = *OFF
015300010403     C                   eval      *in28 = *on
015400010403     C                   eval      *in52 = *on
015500010403     c                   goto      ectrd01
015600010403     C                   ENDIF
015700010403      * Controllo
015800010403     c                   clear                   FIDN05DS
015900010403     c                   movel     'C'           I05MOD
016000010403     c                   movel     Vidfca        I05fca
016100010403     C                   CALL      'FIDN05R'
016200010403     c                   PARM                    KPJBA
016300010403     c                   PARM                    FIDN05DS
016400010403     c                   if        O05rec <> *blanks
016500010403     c                   movel     O05REC        DS_FNDFA
016600010403     c                   movel     DFAdes        Viddca
016700010403     c*
016800010403     c                   else
016900010403     c                   seton                                        5228
017000010403     c                   goto      ectrd01
017100010403     C                   endif
017200010403     c*
017300010403    5C                   ENDIF
017400010403      * verifico se la fase è valida per questa chiusura automatica
017500010403      *
017600010403     c                   movel     vidfca        wfase
017700010403     c                   if        wfase  > wfmax  or wfase  < wfmin
017800010403     c                   seton                                        5028
017900010403     c                   goto      ectrd01
018000010403     C                   endif
018100980923
018200980923     c     Ectrd01       ENDSR
018300980910      *****************************************************************
018400980527      * ROUTINE INIZIALE
018500980527      *****************************************************************
018600980527     C     *INZSR        BEGSR
018700980615      *
018800980521     C     *ENTRY        PLIST
018900980521     C                   PARM                    KPJBA
019000010403      *
019100010403      * valorizzo la causale di chiusura
019200010403
019300010403     c                   eval      vidcch = Wcach
019400981021     c* decodifico causale chiusura se c'è
019500010403
019600981021     c                   clear                   dsbs02
019700981021     C                   MOVEL     'C'           t02mod
019800981021     C                   MOVEL     knsif         t02sif
019900981021     C                   MOVEL     'CCH'         t02cod
020000010403     C                   MOVEL(P)  vidcch        t02ke1
020100981021      *
020200981021     C                   CALL      'TIBS02R'
020300981021     C                   PARM                    KPJBA
020400981021     C                   PARM                    dsbs02
020500981021      *
020600981021    4C                   if        t02err = *BLANKS
020700010403     C                   MOVEL     T02UNI        viddch
020800981021     c                   endif
020900990122     c*
021000980907      *
021100980907     C                   Z-ADD     1             CODUT
021200980907     C                   CALL      'X§PARUT'
021300980907     C                   PARM                    UT§DSE0F
021400980907     C                   MOVEL     RAGUT         RSUT
021500010403      *
021600981007      * Imposto il TITOLO
021700981014      * chiusura
021800981014     c                   movel     TIT_chca      VTCtit
021900010403      *
022000010403     c                   ENDSR
022100980923
