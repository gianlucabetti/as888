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
002500040310      * 53 - ERRORE data
002600010403      * 90 - COMODO riemissione
002700980904      ***************************************************************
002800980623
002900040310     H DECEDIT('0,') DATEDIT(*dmy/)
003000980623
003100010403     fFIDN60D   CF   E             WORKSTN
003200980623
003300980521      *------------------------------------------------------------------------*
003400040310     d msg             s             78    Dim(10) Ctdata Perrcd(1)
003500040310
003600010403     d Wfase           s                   like(dfafca)
003700010403     d Wfmin           s                   like(dfafca)
003800010403     d                                     inz(545)
003900010403     d Wfmax           s                   like(dfafca)
004000010403     d                                     inz(635)
004100010403     d Wcach           s                   like(i61cch)
004200010403     d                                     inz('09')
004300040311     d wdataiso        s               d   DatFmt(*iso)
004400040310     d wdatamax        s               d   DatFmt(*iso)
004500040310     d wdtgio          s              8  0
004600040311     d wdtmax          s              8  0
004700040311     d wdtdfc          s              8  0
004800010403
004900040310      *
005000040310     D WLBDAT          DS                  INZ
005100040310     D  G02DAT                 1      8  0
005200040310     D  G02INV                 9     16  0
005300040310     D  G02ERR                17     17
005400040310     D  G02TGI                18     22  0
005500980923
005600980622      *  titolo videata (lunghezza massima 34)
005700010403     D TIT_chca        C                   CONST('     CHIUSURA AUTOMATICA C.A.-
005800981013     D                                          ')
005900980923      *
006000980915      *----------------------------------
006100010403     D KPJBA         E DS
006200980529     D UT§DSE0F      E DS
006300990311     D DCCH          E DS
006400981014     D DSbs02        E DS                  EXTNAME(tibs02ds)
006500010403     D FIDN61DS      E DS
006600010403     D FIDN05DS      E DS
006700010403     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
006800980521      *
006900980521     D                SDS
007000980521     D  VTCPGM                 1     10
007100980623
007200990122     c*---------------------------------------------------------------------------------------------
007300010403     c*                   C   I   C   L   O
007400990122     c*---------------------------------------------------------------------------------------------
007500010404      *
007600010404     c                   write     FI60t01
007700010404     c                   write     FI60z01
007800010403      *
007900981013     C                   do        *hival
008000980623
008100980521      * Emissione VIDEO1
008200010403     c                   exfmt     FI60d01
008300980923
008400010403     c                   setoff                                       2890
008500040310     c                   setoff                                       53
008600980623
008700980525      * f3=Fine
008800981013     c   kc              leave
008900980623
009000981014      * Controlli  videata
009100980521     c                   exsr      ctrd01
009200010403      * se è acceso il 28 ritotrno ad emettere il formato video
009300010403     c                   if        *in28 or *in90
009400981105     c                   iter
009500981105     c                   endif
009600990511      *
009700981014      * Se non ho confermato o ci sono errori riemetto video
009800981015     c                   IF        *INKF = *ON
009900010403      *
010000010403      * preparazione DS per richiamo pgm chiusura automatica
010100010403      *
010200010403     c                   clear                   fidn61ds
010300010403     c                   eval      i61tca = 'P'
010400010403     c                   eval      i61fca = wfase
010500010403     c                   eval      i61cch = vidccH
010600040311     c                   Eval      i61dfc = wdtdfc
010700010403      *
010800010403     c                   clear                   kpjbu
010900010403     c                   movel     fidn61ds      kpjbu
011000010403     c*
011100010403     C                   MOVEL     'DN61'        KCOAZ
011200010403     C                   CALL      'BCH10'
011300010403     C                   PARM                    KPJBA
011400010403      *
011500010403     c
011600981015     c                   leave
011700980908     c                   ELSE
011800981013     c                   iter
011900980908     c                   ENDIF
012000981014      *
012100981014     c                   enddo
012200980623
012300990122      *
012400981021
012500981029     c                   clear                   dsbs02
012600981029     c                   movel     'C'           t02tla
012700981029     c                   call      'TIBS02R'
012800981029     c                   parm                    kpjba
012900981029     c                   parm                    dsbs02
013000980907     c                   EVAL      *INLR = *ON
013100980521      **********************************************************************
013200980521      * CONTROLLI VIDEO 1
013300980521      **********************************************************************
013400980923     C     CTRD01        BEGSR
013500010403      *
013600010403      * Controllo la fase C.A.
013700010403    2c                   if        vidfca = '   '
013800010403     c                   seton                                        5128
013900010403     c                   goto      ectrd01
014000010403    2c                   endif
014100010403      * controllo la fase
014200010403      *
014300010403     C     '?'           SCAN      vidfca                                 31
014400010403      *
014500010403    3C                   IF        *IN31 = *ON
014600010403     c                   seton                                        90
014700010403     c                   clear                   fidn05ds
014800010403     c                   movel     'R'           I05MOD
014900010403     C                   CALL      'FIDN05R'
015000010403     c                   PARM                    KPJBA
015100010403     c                   PARM                    FIDN05DS
015200010403     c                   if        O05err = 'E'
015300010403     c                   movel     '000'         Vidfca
015400010403     c                   clear                   Viddca
015500010403     c                   else
015600010403     c                   movel     O05REC        DS_FNDFA
015700010403     c                   movel     DFAfca        Vidfca
015800010403     c                   movel     DFAdes        Viddca
015900010403     C                   endif
016000010403      *
016100010403     c                   ELSE
016200010403      *
016300010403     C                   testn                   Vidfca               30
016400010403      * testo ultimo carattere
016500010403     C   30              move      VIDFCA        W001A             1
016600010403     C   30              eval      *in30 = (W001A >= '0')
016700010403      * Se codice non numerico errore
016800010403     C                   IF         *IN30 = *OFF
016900010403     C                   eval      *in28 = *on
017000010403     C                   eval      *in52 = *on
017100010403     c                   goto      ectrd01
017200010403     C                   ENDIF
017300010403      * Controllo
017400010403     c                   clear                   FIDN05DS
017500010403     c                   movel     'C'           I05MOD
017600010403     c                   movel     Vidfca        I05fca
017700010403     C                   CALL      'FIDN05R'
017800010403     c                   PARM                    KPJBA
017900010403     c                   PARM                    FIDN05DS
018000010403     c                   if        O05rec <> *blanks
018100010403     c                   movel     O05REC        DS_FNDFA
018200010403     c                   movel     DFAdes        Viddca
018300010403     c*
018400010403     c                   else
018500010403     c                   seton                                        5228
018600010403     c                   goto      ectrd01
018700010403     C                   endif
018800010403     c*
018900010403    5C                   ENDIF
019000010403      * verifico se la fase è valida per questa chiusura automatica
019100010403      *
019200010403     c                   movel     vidfca        wfase
019300010403     c                   if        wfase  > wfmax  or wfase  < wfmin
019400010403     c                   seton                                        5028
019500010403     c                   goto      ectrd01
019600010403     C                   endif
019700040310
019800040310      * Controllo la data
019900040310      * no a zero
020000040310     c                   If        VidDfc = *Zeros
020100040310     c                   Eval      VidMsg = msg(01)
020200040310     c                   Seton                                        5328
020300040310     c                   goto      ectrd01
020400040310     c                   EndIf
020500040310      * la giro
020600040310     c                   Clear                   wlbdat
020700040310     c                   Z-add     VidDfc        G02dat
020800040310     c                   Call      'XSRDA8'
020900040310     c                   Parm                    wlbdat
021000040310     c                   If        G02err = '1'
021100040310     c                   Seton                                        5328
021200040310     c                   Eval      VidMsg = msg(02)
021300040310     c                   goto      ectrd01
021400040310     c                   EndIf
021500040310     c                   Z-add     G02dat        VidDfc
021600040311     c                   Move      g02inv        wdataiso
021700040310      * non può essere maggiore a quella impostata da me in automatico
021800040311     c                   If        wdataiso > wdatamax
021900040310     c                   Seton                                        5328
022000040310     c                   Eval      VidMsg = msg(03)
022100040310     c                   Eval      VidMsg = %trim (VidMsg) + ' ' +
022200040311     c                             %editc(wdtmax:'Y')
022300040310     c                   goto      ectrd01
022400040310     c                   EndIf
022500040311     c                   Z-add     G02inv        wdtdfc
022600980923
022700980923     c     Ectrd01       ENDSR
022800980910      *****************************************************************
022900980527      * ROUTINE INIZIALE
023000980527      *****************************************************************
023100980527     C     *INZSR        BEGSR
023200980615      *
023300980521     C     *ENTRY        PLIST
023400980521     C                   PARM                    KPJBA
023500010403      *
023600010403      * valorizzo la causale di chiusura
023700010403
023800010403     c                   eval      vidcch = Wcach
023900981021     c* decodifico causale chiusura se c'è
024000010403
024100981021     c                   clear                   dsbs02
024200981021     C                   MOVEL     'C'           t02mod
024300981021     C                   MOVEL     knsif         t02sif
024400981021     C                   MOVEL     'CCH'         t02cod
024500010403     C                   MOVEL(P)  vidcch        t02ke1
024600981021      *
024700981021     C                   CALL      'TIBS02R'
024800981021     C                   PARM                    KPJBA
024900981021     C                   PARM                    dsbs02
025000981021      *
025100981021    4C                   if        t02err = *BLANKS
025200010403     C                   MOVEL     T02UNI        viddch
025300981021     c                   endif
025400040310
025500040310      * Calcolo oggi meno 365 giorni
025600040310     c                   Z-add     *date         wdtgio
025700040310     c                   Z-add     wdtgio        g02dat
025800040310     c                   Clear                   g02err
025900040310     c                   Call      'XSRDA8'
026000040310     c                   Parm                    wlbdat
026100040311     c                   Movel     g02inv        wdataiso
026200040311     c     wdataiso      subdur    365:*d        wdatamax
026300040310     c     *eur          Move      wdatamax      viddfc
026400040311     c                   Eval      wdtmax = viddfc
026500980907      *
026600980907     C                   Z-ADD     1             CODUT
026700980907     C                   CALL      'X§PARUT'
026800980907     C                   PARM                    UT§DSE0F
026900980907     C                   MOVEL     RAGUT         RSUT
027000010403      *
027100981007      * Imposto il TITOLO
027200981014      * chiusura
027300981014     c                   movel     TIT_chca      VTCtit
027400010403      *
027500010403     c                   ENDSR
027600980923
027700040310** MSG  Lungh. 78                                                            *
027800040310Immettere la data                                                                   1
027900040310Data Formalmente errata                                                             2
028000040310La data inserita è maggiore di                                                      3
