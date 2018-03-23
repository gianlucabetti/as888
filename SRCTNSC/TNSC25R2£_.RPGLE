000100931018     H DECEDIT('0,') DATEDIT(*DMY.)
000200931018?     *--------------------------------------------------------------*
000300941104      * TNSC25R2                                                     *
000400931018      *       *-----------------------------------------*            *
000500931018      *       *          STAMPE  CONTRASSEGNI           *            *
000600931018      *       *       STAMPA SCOPERTI IN ARRIVO         *            *
000700931018      *       *-----------------------------------------*            *
000800931018      *                                                              *
000900931018?     *--------------------------------------------------------------*
001000940218      * 01 -ORDINATO PER LINEA DI PARTENZA                           *
001100940218      * 02 -ORDINATO PER LINEA DI ARRIVO                             *
001200940218      * 21 -CHAIN SU CNACO                                           *
001300980520      * 23 -ON SE ESTERO SDI                                         *
001400940218      * 40 -CONFRONTO CLIENTE CON STRINGA                            *
001500940218      * 75 -INDICATORE DI COMODO                                     *
001600940218?     *--------------------------------------------------------------*
001700941104     FFNARB26L  IF   E           K DISK
001800931018?     *---------
001900980612     FFNARB55L  IF   E           K DISK
002000980612     F                                     RENAME(FNARB000:FNARB00E)
002100980612?     *---------
002200051114     FFIAR901L  IF   E           K DISK
002300980520?     *---------
002400060214     FFiAR401L  IF   E           K DISK
002500941104?     *---------
002600050309     FTIGCP21L  IF   E           K DISK
002700931018?     *---------
002800941104     FFNCSBWRK  O  A E             DISK
002900931025?     *---------
003000931025     FCNACO00F  IF   E           K DISK
003100941104?     *---------
003200941104     FTABEL00F  IF   E           K DISK    USROPN
003300980113?    F*---------
003400010108     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003401070702?     *---------
003402070702     FFNDCT02L  IF   E           K DISK
003403070702     FFNLBL01L  IF   E           K DISK
003500931025     D*--------------------------------------------------------------*
003600931025     D*  SCHIERE                                                     *
003700931025     D*--------------------------------------------------------------*
003800941104     D C3A             S              2    DIM(100)
003900941104     D FCA             S              1    DIM(100)
004000020507     D F£3             S              3  0 DIM(30)
004100980113     D FIL             S              3  0 DIM(200)
004200931018?     *--------------------------------------------------------------*
004300931018?     *  DS                                                          *
004400931018?     *--------------------------------------------------------------*
004500931018     D KPJBA         E DS
004600931018      *
004700941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
004800941104     D DS3A          E DS
004900931018      *
005000931025     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100931025     D  TCU                  398    697
005200931025     D                                     DIM(50)
005300931025     D  KCU                  698    847P 0
005400931025     D                                     DIM(50)
005500931025     D                                     PACKEVEN
005600931025?    D*-
005700980113     D TCUDS           DS
005800931025     D  F56                    5      6
005900931025     D  F34                    3      4
006000931112?    D*-
006100931018     D CNCR80        E DS
006200020506     d trul06ds      e ds
006300020506     d  lin                    1     90  0 dim(30)
006400020731     d savpjbu         s                   like(kpjbu)
006500010108     D OG143         E DS
006600980113?     *--------------------------------------------------------------*
006700980113?     *  Ridefinizione interna campi azorg                           *
006800980113?     *--------------------------------------------------------------*
006900980113     IAZORG01L  AA
007000980520     I                                  1    3  ORGDIT
007100980113     I                             P    4    5 0ORGFIL
007200980113     I                                 11   11  ORGFL1
007300010108     I                               3776 3800  ORGDE3
007400931018      *---------------------------------------------------------------*
007500931018      *   C I C L O      P R I N C I P A L E                          *
007600931018      *---------------------------------------------------------------*
007700931018     C*  Lettura archivio bolle arrivate
007800980612     C     D24TPI        IFEQ      'E'
007900980612     C                   MOVEL     D24SDD        KDT3
008000980612     C                   ELSE
008100941104     C                   MOVEL     D24SDD        ANNO              4 0
008200941104     C                   MOVE      D24SDD        MMGG              4 0
008300980612     C                   END
008400980113     C* loop letture x lna
008500020506     C                   Z-ADD     1             XY                3 0
008600980113     C     FIL(XY)       DOWNE     *HIVAL
008700980113     C     XY            ANDLT     200
008800980113     C                   Z-ADD     FIL(XY)       KLNA
008900980612     C     D24TPI        IFEQ      'E'
009000980612     C     KEYSP1        SETLL     FNARB55L
009100980612     C     KLNA          READE     FNARB55L                               80
009200980612     C     *IN80         IFEQ      '0'
009300980612     C                   MOVE      ARBLNA        SAVLIN            3 0
009400980612     C                   EXSR      LEGGIE
009500980612     C                   END
009600980612     C                   ELSE
009700941104     C     KEYSPE        SETLL     FNARB26L
009800980113     C     KLNA          READE     FNARB26L                               80
009900931018     C     *IN80         IFEQ      '0'
010000931018     C                   MOVE      ARBLNA        SAVLIN            3 0
010100941104     C                   MOVEL     ARBAAS        DATSPE            8 0
010200931018     C                   MOVE      ARBMGS        DATSPE
010300931018     C                   EXSR      LEGGI
010400931018     C                   END
010500980612     C*
010600980612     C                   END
010700980113     C*
010800980113     C                   ADD       1             XY
010900980113     C*
011000980113     C                   END
011100931018     C*
011200931025     C     FINE          TAG
011300980520     C                   MOVEL     TNSC24        KPJBU
011400931018     C                   SETON                                        LR
011500931018      *---------------------------------------------------------------*
011600931018      *  LEGGI: Lettura record rientranti nelle linee-date spediz.    *
011700931018      *---------------------------------------------------------------*
011800931018     C     LEGGI         BEGSR
011900931018     C*
012000931018     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
012100941104     C     DATSPE        DOWLE     D24SDA
012200931018     C     *IN80         ANDEQ     '0'
012300931018     C*  Scrivo record se rientra nei limiti richiesti
012400931018     C                   EXSR      WRTREC
012500931018     C*  Calcolo la nuova data di spedizione x il record letto
012600980113     C     KLNA          READE     FNARB26L                               80
012700931018     C     *IN80         IFEQ      '0'
012800941104     C                   MOVEL     ARBAAS        DATSPE            8 0
012900931018     C                   MOVE      ARBMGS        DATSPE
013000931018     C                   END
013100931018     C*
013200931018     C                   END
013300931018     C*
013400931018     C                   ENDSR
013500980612      *---------------------------------------------------------------*
013600980612      *  LEGGIE: Lettura record rientranti nelle linee-data manifest  *
013700980612      *---------------------------------------------------------------*
013800980612     C     LEGGIE        BEGSR
013900980612     C*
014000980612     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
014100980612     C     ARBDT3        DOWLE     D24SDA
014200980612     C     *IN80         ANDEQ     '0'
014300980612     C*  Scrivo record se rientra nei limiti richiesti
014400980612     C                   EXSR      WRTREC
014500980612     C*  Calcolo la nuova data di spedizione x il record letto
014600980612     C     KLNA          READE     FNARB55L                               80
014700980612     C*
014800980612     C                   END
014900980612     C*
015000980612     C                   ENDSR
015100931018      *---------------------------------------------------------------*
015200931018      *  WRTREC: Scrivo record se rientra nei limiti richiesti        *
015300931018      *---------------------------------------------------------------*
015400931018     C     WRTREC        BEGSR
015500931018     C*
015600941104     C                   Z-ADD     1             XX
015700941104     C     ARBCBO        LOOKUP    C3A(XX)                                32
015800941104     C     *IN32         IFEQ      '1'
015900941104     C     FCA(XX)       ANDGT     '0'
016000010108      *SELEZIONATA PARZIALIZZAZIONE PER BARTOLINI/POSTE
016100010108      *
016200010108     C     D24PAR        IFNE      *BLANK
016300010108     C     ARBLNP        CHAIN     AZORG01L                           99
016400010108     C     *IN99         IFEQ      '0'
016500010108     C                   MOVEL     ORGDE3        OG143
016600010108      * PARZIALIZ. POSTE SE FILIALE PARTENZA NON POSTE NON SCRIVO W.F
016700010108     C     D24PAR        IFEQ      'P'
016800020729     C**** §OGPT         CABEQ     *BLANK        NOWRT
016900020729     C     §OgNtw        CABne     'PPT'         NOWRT
017000010108     C                   ELSE
017100010108      * PARZIALIZ. BART. SE FILIALE PARTENZA NON BART. NON SCRIVO W.F
017200020729     C**** §OGPT         CABEQ     'S'           NOWRT
017300020729     C     §OgNtw        CABeq     'PPT'         NOWRT
017400010108     C                   END
017500010108     C                   END
017600010108     C                   END
017700010108      *
017800941104     C*  esiste C/Assegno !!!
017900941104     C                   MOVE      ARBAAS        KAAS
018000941104     C                   MOVE      ARBLNP        KLNP
018100941104     C                   MOVE      ARBNRS        KNRS
018200941104     C                   MOVE      ARBNSP        KNSP
018300051114     C     KSPE          CHAIN     FIAR901L                           32
018400941104     C     *IN32         IFEQ      '0'
018500941104     C     AR9CAS        ANDNE     0
018501070702      *
018600931018     C*  Controllo se contrassegno incassato
018700950306     C     ARBICC        IFEQ      *BLANKS
018800950306     C     ARBICC        OREQ      'R'
018801070702      *
018803070702      *? Controlla le C.A. solo se il parametro passato dal video è <> da "S"  ?
018804070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
018805070702     c                   move      'S'           ctrl_CA_OK        1
018806070702     c                   if        d24xCA <> 'S'
018807070702     c                   exsr      Controlla_C_A
018808070702     c                   end
018809070702     C*
018810070702     c                   if        ctrl_CA_OK = 'S'
018900941104     C                   CLEAR                   FNCSWRK
019000931018     C* Scrivo record se rientra nelle selezioni di codice richiesto
019100931025     C                   MOVEL     ARBRSM        CSBRSM
019200931019     C     ARBCCM        IFNE      0
019300931019     C                   MOVE      ARBCCM        CDI               7 0
019400931025     C* Ragione sociale mittente
019500931025     C                   MOVE      ARBCCM        SERV              4 0
019600931025     C                   SETOFF                                       75
019700931025     C*
019800931025     C     SERV          IFNE      *ALL'8'
019900931025     C     SERV          ORNE      *ALL'9'
020000931025     C     KACO          CHAIN     CNACO000                           21
020100931025     C  N21              MOVEL     ACORAG        CSBRSM
020200931025     C                   END
020300931025     C*
020400931025     C                   ELSE
020500931018     C                   MOVE      ARBKSC        CDI               7 0
020600931018     C                   END
020700931018     C*  Test. x Codice cliente
020800941104     C     D24CDI        IFEQ      0
020900941104     C     D24CDI        OREQ      CDI
021000931018     C*  Test. x Ragione sociale
021100980520     C     D§§RAG        IFNE      *BLANK
021200931018     C                   SETOFF                                       40
021300931018     C*  Loop di ricerca sottostringa nel campo del subfile
021400931018     C                   DO        X             T                 2 0
021500941104     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
021600980520     C     RESULT        COMP      D§§RAG                                 40
021700931018     C  N40              END
021800931018     C*
021900931018     C                   ELSE
022000931018     C                   SETON                                        40
022100931018     C                   END
022200931018     C*
022300931018     C     *IN40         IFEQ      '1'
022400931018     C*  Imposto le chiavi
022500931018     C   01              MOVE      ARBLNP        CSBKE1
022600931018     C   02              MOVE      ARBLNA        CSBKE1
022700931018     C                   MOVE      *ZERO         CSBKE2
022800931018     C                   MOVEL     ARBRSM        CSBKE3
022900931018     C                   MOVE      ARBNSP        CSBKE3
023000931018     C*  Scrivo i dati
023100931018     C                   MOVE      ARBFLE        CSBFLE
023200931018     C                   MOVE      ARBLNA        CSBLNA
023300931018     C                   MOVE      ARBLNP        CSBLNP
023400931018     C                   MOVE      ARBAAS        CSBAAS
023500931018     C                   MOVE      ARBNRS        CSBNRS
023600931018     C                   MOVE      ARBNSP        CSBNSP
023700970625     C* Se stampa per estero imposto data trasmissione manifest in
023800970625     C* data PAGAMENTO
023900970625     C     D24TPI        IFNE      'I'
024000970625     C                   Z-ADD     ARBDT3        CSBDDP
024100970625     C                   END
024200941104     C                   MOVE      ARBAAS        CSBAAS
024300941104     C                   MOVE      ARBMGS        CSBMGS
024400941104     C                   Z-ADD     AR9CAS        CSBCAS
024500011001     C** CON L'EURO NON SI DEVE PIÙ MEMORIZZARE IL CAMBIO
024600011001     C** SUGLI ARCHIVI.
024700011001     C                   Z-ADD     0             CSBCMB
024800011001     C*****                Z-ADDAR9CMB    CSBCMB
024900941104     C                   MOVEL     AR9VCA        CSBVCA
025000931019     C                   MOVEL     ARBRSD        CSBRSD
025100941104     C                   MOVE      ARBRMA        CSBRMA
025200931018     C                   MOVE      ARBRMN        CSBRMN
025300950117     C                   MOVE      ARBCCA        CSBCCA
025400931112     C                   MOVE      *ZERO         CSBDCM
025500941104     C     ARBDCM        IFEQ      0
025600931018     C                   MOVE      ARBDCP        CSBDCM
025700931018     C                   ELSE
025800931018     C                   MOVE      ARBDCM        CSBDCM
025900931018     C                   END
026000931018     C*  Reperisco data inizio giacenza
026100931018     C                   MOVE      ARBAAS        GCPAAS
026200931018     C                   MOVE      ARBLNP        GCPLNP
026300931018     C                   MOVE      ARBNRS        GCPNRS
026400931018     C                   MOVE      ARBNSP        GCPNSP
026500931018     C                   MOVE      *ZERO         GCPFRG
026600050309     C     KEYGCA        CHAIN     TIGCP21L                           71
026700931018     C     *IN71         IFEQ      '0'
026800931018     C                   MOVEL     GCPAGC        CSBDGC
026900931018     C                   MOVE      GCPMGC        CSBDGC
027000931018     C                   END
027100980520      *
027200980520      * Se stampa per estero SDI imposto AWB nel rif. mittente alfabetico
027300980520     C     *IN23         IFEQ      '1'
027400980520     C                   MOVEL     *BLANKS       CSBRMA
027500980520     C                   MOVEL     'E'           KTRC
027600060214     C     KEYAR4        CHAIN     FiAR401L                           72
027700980520     C  N72              MOVEL     AR4NOT        CSBRMA
027800980520     C                   END
027900980520      *
028000941104     C                   WRITE     FNCSWRK
028100931018     C*
028200931018     C                   END
028300931018     C                   END
028301070702     C*
028302070702     c                   endIF
028400931018     C*
028500931018     C                   END
028700950306     C                   END
028900941104     C                   END
029000931018     C*
029100010108     C     NOWRT         ENDSR
029101070702      *---------------------------------------------------------------*
029102070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
029103070702      *---------------------------------------------------------------*
029104070702     C     Controlla_C_A begSR
029105070702     C*
029106070702     C* Se la spedizione ha una pratica CA
029107070702     C                   z-add     Kaas          K_aas
029108070702     C                   z-add     Klnp          K_lnp
029109070702     C                   z-add     Knrs          K_nrs
029110070702     C                   z-add     Knsp          K_nsp
029111070702     C     Kspe          CHAIN     FNLBL01L
029112070702     C                   if        %found(FNLBL01L)
029113070702     C                   z-add     lblaao        K_aas
029114070702     C                   z-add     lbllpo        K_lnp
029115070702     C                   z-add     lblnro        K_nrs
029116070702     C                   z-add     lblnso        K_nsp
029117070702     C                   end
029118070702      *
029119070702      *  Controlla se ci sono C.A. e se sono aperte
029120070702     c                   move      'N'           ci_sono_CA        1
029121070702     c                   move      'N'           CA_aperte         1
029122070702     C     KDCT          setll     FNDCT02L
029123070702     C     KDCT          reade     FNDCT02L
029124070702     c                   dow       not %eof(FNDCT02L)
029125070702     c                   if        DCTatb <> 'A'
029126070702     c                   move      'S'           ci_sono_CA
029127070702      * se solo C.A. Aperte
029128070702     C                   if        Dctdch=0
029129070702     c                   move      'S'           CA_aperte         1
029130070702     c                   end
029131070702     c                   end
029132070702     C     KDCT          READE     FNDCT02L                               34
029133070702     C                   ENDdo
029134070702      *
029135070702      *  Controlla quindi con la selezione impostata a video
029136070702      *    Si devono scartare le spedizioni che hanno C.A. (aperte e chiuse)
029137070702     c                   if        d24xCA = 'N' and ci_sono_CA = 'S'
029138070702     c                   move      'N'           ctrl_CA_OK
029139070702     c                   end
029140070702      *
029141070702      *    Si devono scartare le spedizioni che hanno C.A. chiuse
029142070702     c                   if        d24xCA = 'C' and ci_sono_CA = 'S' and
029143070702     c                             CA_aperte = 'N'
029144070702     c                   move      'N'           ctrl_CA_OK
029145070702     c                   end
029146070702      *
029147070702     C                   ENDSR
029200931018      *---------------------------------------------------------------*
029300941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
029400931018      *---------------------------------------------------------------*
029500941104     C     *INZSR        BEGSR
029600931018      *---------------------------------------------------------------*
029700931018     C     *ENTRY        PLIST
029800931018     C                   PARM                    KPJBA
029900941104     C                   MOVEL     KPJBU         TNSC24
030000931025     C*
030100931025     C                   Z-ADD     1             CODUT
030200931025     C                   CALL      'X§PARUT'
030300931025     C                   PARM                    UT§DSE
030400931025     C     CODUT         CABEQ     -1            FINE
030500980113     C                   MOVEL     REC80         CNCR80
030600931025     C*
030700931025     C                   DO        50            F                 2 0
030800931025     C                   MOVE      TCU(F)        TCUDS
030900931025     C     F34           IFEQ      '01'
031000931025     C     F56           ANDEQ     'CG'
031100931025     C                   Z-ADD     KCU(F)        CLIITA            4 0
031200931025     C                   END
031300931025     C                   END
031400931025     C*
031500931025     C* CNACO00F
031600931025     C     KACO          KLIST
031700931025     C                   KFLD                    CODUT
031800931025     C                   KFLD                    CLIITA
031900931025     C                   KFLD                    CDI
032000931018     C*
032100931018     C     KEYSPE        KLIST
032200980113     C                   KFLD                    KLNA
032300931018     C                   KFLD                    ANNO
032400931018     C                   KFLD                    MMGG
032500980612     C     KEYSP1        KLIST
032600980612     C                   KFLD                    KLNA
032700980612     C                   KFLD                    KDT3
032800931018     C*
032900931018     C     KEYCSB        KLIST
033000931018     C                   KFLD                    CSBAAS
033100931018     C                   KFLD                    CSBLNP
033200931018     C                   KFLD                    CSBNRS
033300931018     C                   KFLD                    CSBNSP
033301070702     C*
033302070702     C     KDCT          KLIST
033303070702     C                   KFLD                    K_AAS
033304070702     C                   KFLD                    K_LNP
033305070702     C                   KFLD                    K_NRS
033306070702     C                   KFLD                    K_NSP
033400980520     C*
033500980520     C     KEYAR4        KLIST
033600980520     C                   KFLD                    CSBAAS
033700980520     C                   KFLD                    CSBLNP
033800980520     C                   KFLD                    CSBNRS
033900980520     C                   KFLD                    CSBNSP
034000980520     C                   KFLD                    KTRC
034100931018     C*
034200931018     C     KEYGCA        KLIST
034300931018     C                   KFLD                    GCPAAS
034400931018     C                   KFLD                    GCPLNP
034500931018     C                   KFLD                    GCPNRS
034600931018     C                   KFLD                    GCPNSP
034700931018     C                   KFLD                    GCPFRG
034800941104     C*
034900941104     C     KSPE          KLIST
035000941104     C                   KFLD                    KAAS
035100941104     C                   KFLD                    KLNP
035200941104     C                   KFLD                    KNRS
035300941104     C                   KFLD                    KNSP
035400941104     C*
035500980113     C     KTAB1         KLIST
035600941104     C                   KFLD                    KKUT
035700941104     C                   KFLD                    KCOD
035800980113     C*
035900980113     C     KTAB2         KLIST
036000980113     C                   KFLD                    KKUT
036100980113     C                   KFLD                    KCOD
036200980113     C                   KFLD                    KKEY
036300941104     C*---------------------------------------------------------------*
036400941104     C*  Definizione delle variabili
036500980520     C     *LIKE         DEFINE    AR4TRC        KTRC
036600980520     C     *LIKE         DEFINE    D24RAG        D§§RAG
036700941104     C     *LIKE         DEFINE    ARBLNP        KLNP
036800941104     C     *LIKE         DEFINE    ARBAAS        KAAS
036900941104     C     *LIKE         DEFINE    ARBMGS        KMGS
037000941104     C     *LIKE         DEFINE    ARBNRS        KNRS
037100941104     C     *LIKE         DEFINE    ARBNSP        KNSP
037200980113     C     *LIKE         DEFINE    ARBLNA        KLNA
037300980612     C     *LIKE         DEFINE    ARBDT3        KDT3
037400941104     C     *LIKE         DEFINE    TBLKUT        KKUT
037500941104     C     *LIKE         DEFINE    TBLCOD        KCOD
037600941104     C     *LIKE         DEFINE    TBLKEY        KKEY
037601070702     C     *LIKE         DEFINE    arbAAS        K_AAS
037602070702     C     *LIKE         DEFINE    arbLNP        K_LNP
037603070702     C     *LIKE         DEFINE    arbNRS        K_NRS
037604070702     C     *LIKE         DEFINE    arbNSP        K_NSP
037700931018     C*----------------------------------------------------*
037800931018     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
037900980520     C                   MOVE      D24RAG        D§§RAG
038000980520     C                   MOVE      '  '          D§§RAG
038100980520     C     '  '          SCAN      D§§RAG        LENGHT            2 0
038200931018     C                   SUB       1             LENGHT
038300931018     C     25            SUB       LENGHT        X                 2 0
038400980113     C*----------------------------------------------------*
038500980113     C                   OPEN      TABEL00F
038600931018     C*----------------------------------------------------*
038700980113     C* Se ho selezionato una sola filiale carico solo
038800980113     C* quella in schiera altrimenti scorro la £3 e carico
038900980113     C* solo filiali Italia/Estero
039000980113     C                   Z-ADD     *HIVAL        FIL
039100020506     c                   clear                   trul06ds
039200020731     c                   movel(p)  kpjbu         savpjbu
039300020506     c                   eval      d06cod = '£1'
039400020506     c                   movel     simfel        d06key
039500020506     c                   movel     trul06ds      kpjbu
039600020506     c                   call      'TRUL06R'
039700020506     c                   parm                    kpjba
039800020506     c                   movel     kpjbu         trul06ds
039900020507     c                   movea     lin           f£3
040000020731     c                   movel(p)  savpjbu       kpjbu
040100020906     C     D24LIN        IFNE      0
040200020906     C                   Z-ADD     D24LIN        FIL(1)
040300020906     C                   ELSE
040400020506     C*                  Z-ADD     1             KKUT
040500020506     C*                  Z-ADD     0             XX                3 0
040600020506     C*                  MOVE      '£3'          KCOD
040700020506     C*                  MOVEL     *BLANKS       KKEY
040800020506     C*                  MOVEL     SIMFEL        KKEY
040900020506     C*    KTAB2         CHAIN     TABEL00F                           31
041000020506     C*    *IN31         IFEQ      '0'
041100020506     C*    TBLFLG        ANDEQ     ' '
041200020506     C*                  MOVEA     TBLUNI        F£3
041300020507     C                   DO        30            XY                3 0
041400980113     C* Filiale £3
041500020507     C     F£3(XY)       IFGT      *ZEROS
041600980113     C* Aggancio AZORG per controllare se filiale italia/estero
041700020507     C                   MOVEL     *BLANKS       ORGFL1
041800020507     C                   MOVE      F£3(XY)       ORGFIL
041900020507     C     ORGFIL        CHAIN     AZORG01L                           32
042000020729     C                   clear                   OG143
042100020729     C  n32              movel     ORGDE3        OG143
042200020729     C                   movel     *blank        flgest            1
042300020729     C     §OgNTW        ifeq      'FED'
042400020729     C     §OgNTW        oreq      'EEX'
042500020729     C     §OgNTW        oreq      'EUP'
042600020729     C                   MOVEL     'E'           flgest
042700020729     c                   end
042800020729     C***  ORGFL1        IFEQ      ' '
042900020729     C     flgest        IFEQ      ' '
043000020507     C     D24TPI        ANDEQ     'I'
043100020729     C***  ORGFL1        OREQ      'E'
043200020729     C     flgest        OREQ      'E'
043300020507     C     D24TPI        ANDEQ     'E'
043400020507     C                   ADD       1             XX
043500020507     C                   MOVE      F£3(XY)       FIL(XX)
043600020507     C                   END
043700020507     C                   END
043800020507     C
043900020507     C                   END
044000980113     C*
044100020506     C*                  END
044200020906     C                   END
044300980520      *----------------------------------------------------*
044400980520      *  Se estero controllo ditta, se è SDI  *IN23 = ON  e  D24AWB = '1'
044500980520     C     D24TPI        IFEQ      'E'
044600980520     C     FIL(1)        CHAIN     AZORG01L                           31
044700020729     C                   clear                   OG143
044800020729     C  n31              movel     ORGDE3        OG143
044900020729     C                   movel     *blank        flgest            1
045000020729     C     §OgNTW        ifeq      'FED'
045100020729     C     §OgNTW        oreq      'EEX'
045200020729     C     §OgNTW        oreq      'EUP'
045300020729     C                   MOVEL     'E'           flgest
045400020729     c                   end
045500980520     C     *IN31         IFEQ      '0'
045600020729     C***  ORGFL1        ANDEQ     'E'
045700020729     C     flgest        ANDEQ     'E'
045800980520     C     ORGDIT        ANDEQ     'SDI'
045900980520     C                   SETON                                        23
046000980520     C                   MOVEL     '1'           D24AWB
046100980520     C                   ENDIF
046200980520     C                   ENDIF
046300980520      *
046400931018     C*----------------------------------------------------*
046500941104     C     D24ORD        COMP      'P'                                020201
046600931018     C*----------------------------------------------------*
046700941104     C*  Carico codici bolla
046800941104     C                   Z-ADD     1             KKUT
046900941104     C                   Z-ADD     0             XX                3 0
047000941104     C                   MOVE      '3A'          KCOD
047100980113     C     KTAB1         CHAIN     TABEL00F                           31
047200941104     C     *IN31         DOWEQ     '0'
047300941104     C     XX            ANDLT     100
047400941104     C     TBLFLG        IFEQ      ' '
047500941104     C                   MOVEL     TBLUNI        DS3A
047600941104     C                   ADD       1             XX
047700941104     C                   MOVEL     TBLKEY        C3A(XX)
047800941104     C                   MOVEL     §3AFCA        FCA(XX)
047900941104     C                   END
048000980113     C     KTAB1         READE     TABEL00F                               31
048100941104     C                   END
048200941104     C                   CLOSE     TABEL00F
048300980520      *----------------------------------------------------*
048400931018     C                   ENDSR
