000100001113     H DECEDIT('0,') DATEDIT(*DMY.)
000200001113
000300001113      *****************************************************************
000400001113      *                                                               *
000500010205      *       CREAZIONE AUTOMATICA ANAGRAFICHE ORM                    *
000600001113      *                                                               *
000700001113      *****************************************************************
000800001113
000900001113     FAZORG01L  IF   E           K DISK
001000010208     FFNACR01L  IF   E           K DISK
001100010205     FTNVRORMD  CF   E             WORKSTN
001200010205
001300010205     D kazorg          S                   LIKE(ORGfil)
001400010205     D poparte         S                   LIKE(ORGfil)
001500010208     D kfnacr          S                   LIKE(ACRcro)
001600010208     D contracr        S                   LIKE(v1poparte)
001700010405     D maxkfnacr       S                   LIKE(ACRcro)
001800001113
001900010208     D MSG             S             78    DIM(4)  CTDATA PERRCD(1)             MSG VIDEO
002000001113     D L1              S              3  0 DIM(30)                              P.O. Gestiti £1
002100010119
002200001113     D KPJBA         E DS
002300010205     D OG148         E DS
002400010205     D UT§DSE0F      E DS
002500010205     D CNCR80        E DS
002600010205     D TRUL06DS      E DS
002700010205     D  LIN                    1     90  0
002800010205     D                                     DIM(30)
002900001116
003000001116      * Per Programmi Richiamati
003100001116
003200001116     D Parm01          DS
003300001116     D  ppoe                          3  0
003400010205
003500010205     D Dstnsd          DS
003600010205     D  §cod                          3
003700010205     D  §tip                          1
003800010205     D  §des                         25
003900010205     D  §pra                          3
004000001113
004100001113      *---------------------------------------------------------------*
004200001113      *                                                               *
004300001113      *    UTILIZZO DEGLI INDICATORI                                  *
004400001113      *                                                               *
004500001113      *---------------------------------------------------------------*
004600010205      *
004700010205      * 28 - ERRORE GENERICO DSPF
004800010205      * 30 - Comodo
004900010205      * 41 - ERRORE P.O. che parte
005000010205      * 90 - riemissione videata
005100010205      *
005200001113      *---------------------------------------------------------------*
005300001113
005400001120      *---------------------------------------------------------------*
005500001120      *                                                               *
005600001120      *---------------------------------------------------------------*
005700001113
005800010205      * Pulizia Videata
005900010205     C                   exsr      Sr_Pulvid
006000010206      * Emissione Videata
006100010206
006200010206     C                   do        *hival
006300010205
006400010205     C                   exfmt     tnvrorm1
006500010205      * F3=Fine
006600010205     C   kc              leave
006700010205
006800010205     C                   setoff                                       2890
006900010205
007000010205      * F6=Conferma
007100010205     C                   if        *inkf = *on
007200010205     C                   exsr      Sr_Contr
007300010205     C   28              iter
007400010205     C                   exsr      Sr_Conferma
007500010205     C                   leave
007600010205     C                   endif
007700010205
007800010205      * Controlli videata
007900010205     C                   exsr      Sr_Contr
008000010205
008100010205     C   28              iter
008200010205     C   90              iter
008300010205
008400010205     C                   enddo
008500010205
008600010205     C                   eval      *inlr = *on
008700010205      *****************************************************************
008800010205      * PULIZIA VIDEATA
008900010205      *****************************************************************
009000010205     C     Sr_Pulvid     BEGSR
009100010205
009200010205     C                   clear                   v1poparte
009300010205     C                   clear                   v1despo
009400010205
009500010205     C                   endsr
009600010205      **********************************************************************
009700010205      * CONTROLLI VIDEO
009800010205      **********************************************************************
009900010205     C     Sr_Contr      begsr
010000010205
010100010205      * Reset indicatori
010200010205
010300010205     C                   setoff                                       41
010400010205
010500010205     C                   exsr      sr_contrpo
010600010205     C   28              goto      endsrcontr
010700010208
010800010208     C                   exsr      sr_contrfnacr
010900010208     C   28              goto      endsrcontr
011000010205
011100010205     C     endsrcontr    endsr
011200010205      **********************************************************************
011300010205      * CONTROLLO     * P.O. che parte *
011400010205      **********************************************************************
011500010205     C     Sr_Contrpo    begsr
011600010205
011700010205     C     '?'           scan      v1poparte                              30
011800010205     C     *in30         ifeq      *on
011900010205     C                   eval      *in41 = *on
012000010205     C                   eval      *in90 = *on
012100010205     C                   clear                   dstnsd
012200010205     C                   call      'TNSD24R'
012300010205     C                   parm                    §cod
012400010205     C                   parm                    §tip
012500010205     C                   parm                    §des
012600010205     C                   parm                    §pra
012700010205     C                   movel     §cod          v1poparte
012800010205     C                   movel     §des          v1despo
012900010205     C                   goto      endsrcpo
013000010205     C                   endif
013100010205
013200010205     C                   testn                   v1poparte            30
013300010205     C                   move      v1poparte     w001a             1
013400010205     C     *in30         ifeq      *off
013500010205     C     w001a         orlt      '0'
013600010205     C                   seton                                        41  28
013700010205     C                   movel     msg(1)        v1cmsg
013800010205     C                   goto      endsrcpo
013900010205     C                   endif
014000010205
014100010205     C                   if        v1poparte > *zeros
014200010205     C                   movel     v1poparte     kazorg
014300010205     C     kazorg        chain     azorg01L
014400010205      * Non trovato
014500010205     C                   if        not%found(azorg01l)
014600010205     C                   movel     msg(1)        v1cmsg
014700010205     C                   seton                                        41  28
014800010205     C                   goto      endsrcpo
014900010205     C                   endif
015000010205      * Annullato
015100010205     C                   if        ORGfva <> *blanks
015200010205     C                   movel     msg(1)        v1cmsg
015300010205     C                   seton                                        41  28
015400010205     C                   goto      endsrcpo
015500010205     C                   endif
015600010205      * Deve essere della mia l1
015700010205     C                   movel     v1poparte     poparte
015800010205     C     poparte       lookup    l1                                     30
015900010205     C                   if        *in30 = *off
016000010205     C                   movel     msg(2)        v1cmsg
016100010205     C                   seton                                        41  28
016200010205     C                   goto      endsrcpo
016300010205     C                   endif
016400010205      * Controllo se messo flag
016500010205     C                   movel     orgde8        og148
016600010205     C                   if        §ogorm <> 'S'
016700010205     C                   movel     msg(3)        v1cmsg
016800010205     C                   seton                                        41  28
016900010205     C                   goto      endsrcpo
017000010205     C                   endif
017100010205
017200010205     C                   movel     ORGdes        v1despo
017300010205     C                   endif
017400010205
017500010205     C     endsrcpo      endsr
017600010208      **********************************************************************
017700010208      * CONTROLLO     * Su fnacr non deve esserci la filiale che parte *
017800010208      **********************************************************************
017900010208     C     Sr_Contrfnacr begsr
018000010208
018100010208     C                   movel     v1poparte     kfnacr
018200010405     C                   movel     *all'9'       maxkfnacr
018300010405     C                   movel     v1poparte     kfnacr
018400010208     C     kfnacr        setll     fnacr01l
018500010405     C                   do        *hival
018600010208     C                   read      fnacr01l
018700010405     C                   if        %eof(fnacr01l)
018800010405     C                   leave
018900010405     C                   endif
019000010405     C                   if        acrcro > maxkfnacr
019100010405     C                   leave
019200010405     C                   endif
019300010405     C                   if        acratb <> *blanks
019400010405     C                   iter
019500010405     C                   endif
019600010208     C                   movel     acrcro        contracr
019700010208     C                   if        contracr = v1poparte
019800010208     C                   movel     msg(4)        v1cmsg
019900010208     C                   seton                                        41  28
020000010208     C                   goto      endsrfnacr
020100010208     C                   endif
020200010405     C                   enddo
020300010208
020400010208     C     endsrfnacr    endsr
020500010205      **********************************************************************
020600010205      * RICHIAMA IL PROGRAMMA DI CREAZIONE ANAGRAFICHE
020700010205      **********************************************************************
020800010205     C     Sr_Conferma   BEGSR
020900010205
021000010205     C                   clear                   parm01
021100010205     C                   movel     v1poparte     ppoe
021200010205
021300010205     C                   movel     parm01        kpjbu
021400010709     C                   call      'TNVRORM1'
021500010205     C                   parm                    kpjba
021600010205
021700010205     C     endsrstampa   endsr
021800010205      *****************************************************************
021900010205      * ROUTINE INIZIALE
022000010205      *****************************************************************
022100010205     C     *INZSR        BEGSR
022200010205
022300010205     C     *ENTRY        PLIST
022400010205     C                   PARM                    KPJBA
022500010205
022600010205     C                   Z-ADD     1             CODUT
022700010205     C                   CALL      'X§PARUT'
022800010205     C                   PARM                    UT§DSE0F
022900010205     C                   MOVEL     REC80         CNCR80
023000010205      * Carico schiera P.O. gestiti
023100010205     C                   CLEAR                   TRUL06ds
023200010205     C                   MOVE      '£1'          D06COD
023300010205     C                   MOVEL     SIMFEL        D06KEY
023400010205     C                   MOVEL     TRUL06DS      KPJBU
023500010205     C                   CALL      'TRUL06R'
023600010205     C                   PARM                    KPJBA
023700010205     C                   MOVEL     KPJBU         TRUL06DS
023800010205     C                   MOVEA     LIN           L1
023900001113
024000010205     C                   endsr
024100010205      *----------------
024200001113** MSG
024300010205Filiale errata                                                                      1
024400010205Filiale non in gestione                                                             2
024500010205Manca il flag di ok in organigramma                                                 3
024600010208Filiale già elaborata                                                               4
