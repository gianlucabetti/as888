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
003500070702?     *---------
003600070702     FFNDCT02L  IF   E           K DISK
003700070702     FFNLBL01L  IF   E           K DISK
003800931025     D*--------------------------------------------------------------*
003900931025     D*  SCHIERE                                                     *
004000931025     D*--------------------------------------------------------------*
004100941104     D C3A             S              2    DIM(100)
004200941104     D FCA             S              1    DIM(100)
004300080813     D F£3             S              3  0 DIM(60)
004400980113     D FIL             S              3  0 DIM(200)
004500931018?     *--------------------------------------------------------------*
004600931018?     *  DS                                                          *
004700931018?     *--------------------------------------------------------------*
004800931018     D KPJBA         E DS
004900931018      *
005000941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
005100941104     D DS3A          E DS
005200931018      *
005300931025     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005400931025     D  TCU                  398    697
005500931025     D                                     DIM(50)
005600931025     D  KCU                  698    847P 0
005700931025     D                                     DIM(50)
005800931025     D                                     PACKEVEN
005900931025?    D*-
006000980113     D TCUDS           DS
006100931025     D  F56                    5      6
006200931025     D  F34                    3      4
006300931112?    D*-
006400931018     D CNCR80        E DS
006500080813     D TRUL72DS      E DS
006600080813     D  Lin                           3s 0 DESCEND
006700080813     D                                     DIM(60)
006800080813     D                                     OVERLAY(O72Lia)
006900020731     d savpjbu         s                   like(kpjbu)
007000010108     D OG143         E DS
007100980113?     *--------------------------------------------------------------*
007200980113?     *  Ridefinizione interna campi azorg                           *
007300980113?     *--------------------------------------------------------------*
007400980113     IAZORG01L  AA
007500980520     I                                  1    3  ORGDIT
007600980113     I                             P    4    5 0ORGFIL
007700980113     I                                 11   11  ORGFL1
007800010108     I                               3776 3800  ORGDE3
007900931018      *---------------------------------------------------------------*
008000931018      *   C I C L O      P R I N C I P A L E                          *
008100931018      *---------------------------------------------------------------*
008200931018     C*  Lettura archivio bolle arrivate
008300980612     C     D24TPI        IFEQ      'E'
008400980612     C                   MOVEL     D24SDD        KDT3
008500980612     C                   ELSE
008600941104     C                   MOVEL     D24SDD        ANNO              4 0
008700941104     C                   MOVE      D24SDD        MMGG              4 0
008800980612     C                   END
008900980113     C* loop letture x lna
009000020506     C                   Z-ADD     1             XY                3 0
009100980113     C     FIL(XY)       DOWNE     *HIVAL
009200980113     C     XY            ANDLT     200
009300980113     C                   Z-ADD     FIL(XY)       KLNA
009400980612     C     D24TPI        IFEQ      'E'
009500980612     C     KEYSP1        SETLL     FNARB55L
009600980612     C     KLNA          READE     FNARB55L                               80
009700980612     C     *IN80         IFEQ      '0'
009800980612     C                   MOVE      ARBLNA        SAVLIN            3 0
009900980612     C                   EXSR      LEGGIE
010000980612     C                   END
010100980612     C                   ELSE
010200941104     C     KEYSPE        SETLL     FNARB26L
010300980113     C     KLNA          READE     FNARB26L                               80
010400931018     C     *IN80         IFEQ      '0'
010500931018     C                   MOVE      ARBLNA        SAVLIN            3 0
010600941104     C                   MOVEL     ARBAAS        DATSPE            8 0
010700931018     C                   MOVE      ARBMGS        DATSPE
010800931018     C                   EXSR      LEGGI
010900931018     C                   END
011000980612     C*
011100980612     C                   END
011200980113     C*
011300980113     C                   ADD       1             XY
011400980113     C*
011500980113     C                   END
011600931018     C*
011700931025     C     FINE          TAG
011800980520     C                   MOVEL     TNSC24        KPJBU
011900931018     C                   SETON                                        LR
012000931018      *---------------------------------------------------------------*
012100931018      *  LEGGI: Lettura record rientranti nelle linee-date spediz.    *
012200931018      *---------------------------------------------------------------*
012300931018     C     LEGGI         BEGSR
012400931018     C*
012500931018     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
012600941104     C     DATSPE        DOWLE     D24SDA
012700931018     C     *IN80         ANDEQ     '0'
012800931018     C*  Scrivo record se rientra nei limiti richiesti
012900931018     C                   EXSR      WRTREC
013000931018     C*  Calcolo la nuova data di spedizione x il record letto
013100980113     C     KLNA          READE     FNARB26L                               80
013200931018     C     *IN80         IFEQ      '0'
013300941104     C                   MOVEL     ARBAAS        DATSPE            8 0
013400931018     C                   MOVE      ARBMGS        DATSPE
013500931018     C                   END
013600931018     C*
013700931018     C                   END
013800931018     C*
013900931018     C                   ENDSR
014000980612      *---------------------------------------------------------------*
014100980612      *  LEGGIE: Lettura record rientranti nelle linee-data manifest  *
014200980612      *---------------------------------------------------------------*
014300980612     C     LEGGIE        BEGSR
014400980612     C*
014500980612     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
014600980612     C     ARBDT3        DOWLE     D24SDA
014700980612     C     *IN80         ANDEQ     '0'
014800980612     C*  Scrivo record se rientra nei limiti richiesti
014900980612     C                   EXSR      WRTREC
015000980612     C*  Calcolo la nuova data di spedizione x il record letto
015100980612     C     KLNA          READE     FNARB55L                               80
015200980612     C*
015300980612     C                   END
015400980612     C*
015500980612     C                   ENDSR
015600931018      *---------------------------------------------------------------*
015700931018      *  WRTREC: Scrivo record se rientra nei limiti richiesti        *
015800931018      *---------------------------------------------------------------*
015900931018     C     WRTREC        BEGSR
016000931018     C*
016100941104     C                   Z-ADD     1             XX
016200941104     C     ARBCBO        LOOKUP    C3A(XX)                                32
016300941104     C     *IN32         IFEQ      '1'
016400941104     C     FCA(XX)       ANDGT     '0'
016500010108      *SELEZIONATA PARZIALIZZAZIONE PER BARTOLINI/POSTE
016600010108      *
016700010108     C     D24PAR        IFNE      *BLANK
016800010108     C     ARBLNP        CHAIN     AZORG01L                           99
016900010108     C     *IN99         IFEQ      '0'
017000010108     C                   MOVEL     ORGDE3        OG143
017100010108      * PARZIALIZ. POSTE SE FILIALE PARTENZA NON POSTE NON SCRIVO W.F
017200010108     C     D24PAR        IFEQ      'P'
017300020729     C**** §OGPT         CABEQ     *BLANK        NOWRT
017400020729     C     §OgNtw        CABne     'PPT'         NOWRT
017500010108     C                   ELSE
017600010108      * PARZIALIZ. BART. SE FILIALE PARTENZA NON BART. NON SCRIVO W.F
017700020729     C**** §OGPT         CABEQ     'S'           NOWRT
017800020729     C     §OgNtw        CABeq     'PPT'         NOWRT
017900010108     C                   END
018000010108     C                   END
018100010108     C                   END
018200010108      *
018300941104     C*  esiste C/Assegno !!!
018400941104     C                   MOVE      ARBAAS        KAAS
018500941104     C                   MOVE      ARBLNP        KLNP
018600941104     C                   MOVE      ARBNRS        KNRS
018700941104     C                   MOVE      ARBNSP        KNSP
018800051114     C     KSPE          CHAIN     FIAR901L                           32
018900941104     C     *IN32         IFEQ      '0'
019000941104     C     AR9CAS        ANDNE     0
019100070702      *
019200931018     C*  Controllo se contrassegno incassato
019300950306     C     ARBICC        IFEQ      *BLANKS
019400950306     C     ARBICC        OREQ      'R'
019500070702      *
019600070702      *? Controlla le C.A. solo se il parametro passato dal video è <> da "S"  ?
019700070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
019800070702     c                   move      'S'           ctrl_CA_OK        1
019900070702     c                   if        d24xCA <> 'S'
020000070702     c                   exsr      Controlla_C_A
020100070702     c                   end
020200070702     C*
020300070702     c                   if        ctrl_CA_OK = 'S'
020400941104     C                   CLEAR                   FNCSWRK
020500931018     C* Scrivo record se rientra nelle selezioni di codice richiesto
020600931025     C                   MOVEL     ARBRSM        CSBRSM
020700931019     C     ARBCCM        IFNE      0
020800931019     C                   MOVE      ARBCCM        CDI               7 0
020900931025     C* Ragione sociale mittente
021000931025     C                   MOVE      ARBCCM        SERV              4 0
021100931025     C                   SETOFF                                       75
021200931025     C*
021300931025     C     SERV          IFNE      *ALL'8'
021400931025     C     SERV          ORNE      *ALL'9'
021500931025     C     KACO          CHAIN     CNACO000                           21
021600931025     C  N21              MOVEL     ACORAG        CSBRSM
021700931025     C                   END
021800931025     C*
021900931025     C                   ELSE
022000931018     C                   MOVE      ARBKSC        CDI               7 0
022100931018     C                   END
022200931018     C*  Test. x Codice cliente
022300941104     C     D24CDI        IFEQ      0
022400941104     C     D24CDI        OREQ      CDI
022500931018     C*  Test. x Ragione sociale
022600980520     C     D§§RAG        IFNE      *BLANK
022700931018     C                   SETOFF                                       40
022800931018     C*  Loop di ricerca sottostringa nel campo del subfile
022900931018     C                   DO        X             T                 2 0
023000941104     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
023100980520     C     RESULT        COMP      D§§RAG                                 40
023200931018     C  N40              END
023300931018     C*
023400931018     C                   ELSE
023500931018     C                   SETON                                        40
023600931018     C                   END
023700931018     C*
023800931018     C     *IN40         IFEQ      '1'
023900931018     C*  Imposto le chiavi
024000931018     C   01              MOVE      ARBLNP        CSBKE1
024100931018     C   02              MOVE      ARBLNA        CSBKE1
024200931018     C                   MOVE      *ZERO         CSBKE2
024300931018     C                   MOVEL     ARBRSM        CSBKE3
024400931018     C                   MOVE      ARBNSP        CSBKE3
024500931018     C*  Scrivo i dati
024600931018     C                   MOVE      ARBFLE        CSBFLE
024700931018     C                   MOVE      ARBLNA        CSBLNA
024800931018     C                   MOVE      ARBLNP        CSBLNP
024900931018     C                   MOVE      ARBAAS        CSBAAS
025000931018     C                   MOVE      ARBNRS        CSBNRS
025100931018     C                   MOVE      ARBNSP        CSBNSP
025200970625     C* Se stampa per estero imposto data trasmissione manifest in
025300970625     C* data PAGAMENTO
025400970625     C     D24TPI        IFNE      'I'
025500970625     C                   Z-ADD     ARBDT3        CSBDDP
025600970625     C                   END
025700941104     C                   MOVE      ARBAAS        CSBAAS
025800941104     C                   MOVE      ARBMGS        CSBMGS
025900941104     C                   Z-ADD     AR9CAS        CSBCAS
026000011001     C** CON L'EURO NON SI DEVE PIÙ MEMORIZZARE IL CAMBIO
026100011001     C** SUGLI ARCHIVI.
026200011001     C                   Z-ADD     0             CSBCMB
026300011001     C*****                Z-ADDAR9CMB    CSBCMB
026400941104     C                   MOVEL     AR9VCA        CSBVCA
026500931019     C                   MOVEL     ARBRSD        CSBRSD
026600941104     C                   MOVE      ARBRMA        CSBRMA
026700931018     C                   MOVE      ARBRMN        CSBRMN
026800950117     C                   MOVE      ARBCCA        CSBCCA
026900931112     C                   MOVE      *ZERO         CSBDCM
027000941104     C     ARBDCM        IFEQ      0
027100931018     C                   MOVE      ARBDCP        CSBDCM
027200931018     C                   ELSE
027300931018     C                   MOVE      ARBDCM        CSBDCM
027400931018     C                   END
027500931018     C*  Reperisco data inizio giacenza
027600931018     C                   MOVE      ARBAAS        GCPAAS
027700931018     C                   MOVE      ARBLNP        GCPLNP
027800931018     C                   MOVE      ARBNRS        GCPNRS
027900931018     C                   MOVE      ARBNSP        GCPNSP
028000931018     C                   MOVE      *ZERO         GCPFRG
028100050309     C     KEYGCA        CHAIN     TIGCP21L                           71
028200931018     C     *IN71         IFEQ      '0'
028300931018     C                   MOVEL     GCPAGC        CSBDGC
028400931018     C                   MOVE      GCPMGC        CSBDGC
028500931018     C                   END
028600980520      *
028700980520      * Se stampa per estero SDI imposto AWB nel rif. mittente alfabetico
028800980520     C     *IN23         IFEQ      '1'
028900980520     C                   MOVEL     *BLANKS       CSBRMA
029000980520     C                   MOVEL     'E'           KTRC
029100060214     C     KEYAR4        CHAIN     FiAR401L                           72
029200980520     C  N72              MOVEL     AR4NOT        CSBRMA
029300980520     C                   END
029400980520      *
029500941104     C                   WRITE     FNCSWRK
029600931018     C*
029700931018     C                   END
029800931018     C                   END
029900070702     C*
030000070702     c                   endIF
030100931018     C*
030200931018     C                   END
030300950306     C                   END
030400941104     C                   END
030500931018     C*
030600010108     C     NOWRT         ENDSR
030700070702      *---------------------------------------------------------------*
030800070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
030900070702      *---------------------------------------------------------------*
031000070702     C     Controlla_C_A begSR
031100070702     C*
031200070702     C* Se la spedizione ha una pratica CA
031300070702     C                   z-add     Kaas          K_aas
031400070702     C                   z-add     Klnp          K_lnp
031500070702     C                   z-add     Knrs          K_nrs
031600070702     C                   z-add     Knsp          K_nsp
031700070702     C     Kspe          CHAIN     FNLBL01L
031800070702     C                   if        %found(FNLBL01L)
031900070702     C                   z-add     lblaao        K_aas
032000070702     C                   z-add     lbllpo        K_lnp
032100070702     C                   z-add     lblnro        K_nrs
032200070702     C                   z-add     lblnso        K_nsp
032300070702     C                   end
032400070702      *
032500070702      *  Controlla se ci sono C.A. e se sono aperte
032600070702     c                   move      'N'           ci_sono_CA        1
032700070702     c                   move      'N'           CA_aperte         1
032800070702     C     KDCT          setll     FNDCT02L
032900070702     C     KDCT          reade     FNDCT02L
033000070702     c                   dow       not %eof(FNDCT02L)
033100070702     c                   if        DCTatb <> 'A'
033200070702     c                   move      'S'           ci_sono_CA
033300070702      * se solo C.A. Aperte
033400070702     C                   if        Dctdch=0
033500070702     c                   move      'S'           CA_aperte         1
033600070702     c                   end
033700070702     c                   end
033800070702     C     KDCT          READE     FNDCT02L                               34
033900070702     C                   ENDdo
034000070702      *
034100070702      *  Controlla quindi con la selezione impostata a video
034200070702      *    Si devono scartare le spedizioni che hanno C.A. (aperte e chiuse)
034300070702     c                   if        d24xCA = 'N' and ci_sono_CA = 'S'
034400070702     c                   move      'N'           ctrl_CA_OK
034500070702     c                   end
034600070702      *
034700070702      *    Si devono scartare le spedizioni che hanno C.A. chiuse
034800070702     c                   if        d24xCA = 'C' and ci_sono_CA = 'S' and
034900070702     c                             CA_aperte = 'N'
035000070702     c                   move      'N'           ctrl_CA_OK
035100070702     c                   end
035200070702      *
035300070702     C                   ENDSR
035400931018      *---------------------------------------------------------------*
035500941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
035600931018      *---------------------------------------------------------------*
035700941104     C     *INZSR        BEGSR
035800931018      *---------------------------------------------------------------*
035900931018     C     *ENTRY        PLIST
036000931018     C                   PARM                    KPJBA
036100941104     C                   MOVEL     KPJBU         TNSC24
036200931025     C*
036300931025     C                   Z-ADD     1             CODUT
036400931025     C                   CALL      'X§PARUT'
036500931025     C                   PARM                    UT§DSE
036600931025     C     CODUT         CABEQ     -1            FINE
036700980113     C                   MOVEL     REC80         CNCR80
036800931025     C*
036900931025     C                   DO        50            F                 2 0
037000931025     C                   MOVE      TCU(F)        TCUDS
037100931025     C     F34           IFEQ      '01'
037200931025     C     F56           ANDEQ     'CG'
037300931025     C                   Z-ADD     KCU(F)        CLIITA            4 0
037400931025     C                   END
037500931025     C                   END
037600931025     C*
037700931025     C* CNACO00F
037800931025     C     KACO          KLIST
037900931025     C                   KFLD                    CODUT
038000931025     C                   KFLD                    CLIITA
038100931025     C                   KFLD                    CDI
038200931018     C*
038300931018     C     KEYSPE        KLIST
038400980113     C                   KFLD                    KLNA
038500931018     C                   KFLD                    ANNO
038600931018     C                   KFLD                    MMGG
038700980612     C     KEYSP1        KLIST
038800980612     C                   KFLD                    KLNA
038900980612     C                   KFLD                    KDT3
039000931018     C*
039100931018     C     KEYCSB        KLIST
039200931018     C                   KFLD                    CSBAAS
039300931018     C                   KFLD                    CSBLNP
039400931018     C                   KFLD                    CSBNRS
039500931018     C                   KFLD                    CSBNSP
039600070702     C*
039700070702     C     KDCT          KLIST
039800070702     C                   KFLD                    K_AAS
039900070702     C                   KFLD                    K_LNP
040000070702     C                   KFLD                    K_NRS
040100070702     C                   KFLD                    K_NSP
040200980520     C*
040300980520     C     KEYAR4        KLIST
040400980520     C                   KFLD                    CSBAAS
040500980520     C                   KFLD                    CSBLNP
040600980520     C                   KFLD                    CSBNRS
040700980520     C                   KFLD                    CSBNSP
040800980520     C                   KFLD                    KTRC
040900931018     C*
041000931018     C     KEYGCA        KLIST
041100931018     C                   KFLD                    GCPAAS
041200931018     C                   KFLD                    GCPLNP
041300931018     C                   KFLD                    GCPNRS
041400931018     C                   KFLD                    GCPNSP
041500931018     C                   KFLD                    GCPFRG
041600941104     C*
041700941104     C     KSPE          KLIST
041800941104     C                   KFLD                    KAAS
041900941104     C                   KFLD                    KLNP
042000941104     C                   KFLD                    KNRS
042100941104     C                   KFLD                    KNSP
042200941104     C*
042300980113     C     KTAB1         KLIST
042400941104     C                   KFLD                    KKUT
042500941104     C                   KFLD                    KCOD
042600980113     C*
042700980113     C     KTAB2         KLIST
042800980113     C                   KFLD                    KKUT
042900980113     C                   KFLD                    KCOD
043000980113     C                   KFLD                    KKEY
043100941104     C*---------------------------------------------------------------*
043200941104     C*  Definizione delle variabili
043300980520     C     *LIKE         DEFINE    AR4TRC        KTRC
043400980520     C     *LIKE         DEFINE    D24RAG        D§§RAG
043500941104     C     *LIKE         DEFINE    ARBLNP        KLNP
043600941104     C     *LIKE         DEFINE    ARBAAS        KAAS
043700941104     C     *LIKE         DEFINE    ARBMGS        KMGS
043800941104     C     *LIKE         DEFINE    ARBNRS        KNRS
043900941104     C     *LIKE         DEFINE    ARBNSP        KNSP
044000980113     C     *LIKE         DEFINE    ARBLNA        KLNA
044100980612     C     *LIKE         DEFINE    ARBDT3        KDT3
044200941104     C     *LIKE         DEFINE    TBLKUT        KKUT
044300941104     C     *LIKE         DEFINE    TBLCOD        KCOD
044400941104     C     *LIKE         DEFINE    TBLKEY        KKEY
044500070702     C     *LIKE         DEFINE    arbAAS        K_AAS
044600070702     C     *LIKE         DEFINE    arbLNP        K_LNP
044700070702     C     *LIKE         DEFINE    arbNRS        K_NRS
044800070702     C     *LIKE         DEFINE    arbNSP        K_NSP
044900931018     C*----------------------------------------------------*
045000931018     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
045100980520     C                   MOVE      D24RAG        D§§RAG
045200980520     C                   MOVE      '  '          D§§RAG
045300980520     C     '  '          SCAN      D§§RAG        LENGHT            2 0
045400931018     C                   SUB       1             LENGHT
045500931018     C     25            SUB       LENGHT        X                 2 0
045600980113     C*----------------------------------------------------*
045700980113     C                   OPEN      TABEL00F
045800931018     C*----------------------------------------------------*
045900980113     C* Se ho selezionato una sola filiale carico solo
046000980113     C* quella in schiera altrimenti scorro la £3 e carico
046100980113     C* solo filiali Italia/Estero
046200980113     C                   Z-ADD     *HIVAL        FIL
046300080813     c                   clear                   trul72ds
046400080813     c                   eval      i72tla = 'L'
046500080813     c                   eval      I72POU = simfel
046600080813     c                   eval      kpjbu = trul72ds
046700080813     C                   CALL      'TRUL72R'
046800080813     C                   PARM                    KPJBA
046900080813     c                   movel     kpjbu         trul72ds
047000080813     c                   movea     lin           f£3
047100020731     c                   movel(p)  savpjbu       kpjbu
047200020906     C     D24LIN        IFNE      0
047300020906     C                   Z-ADD     D24LIN        FIL(1)
047400020906     C                   ELSE
047500020506     C*                  Z-ADD     1             KKUT
047600020506     C*                  Z-ADD     0             XX                3 0
047700020506     C*                  MOVE      '£3'          KCOD
047800020506     C*                  MOVEL     *BLANKS       KKEY
047900020506     C*                  MOVEL     SIMFEL        KKEY
048000020506     C*    KTAB2         CHAIN     TABEL00F                           31
048100020506     C*    *IN31         IFEQ      '0'
048200020506     C*    TBLFLG        ANDEQ     ' '
048300020506     C*                  MOVEA     TBLUNI        F£3
048400020507     C                   DO        30            XY                3 0
048500980113     C* Filiale £3
048600020507     C     F£3(XY)       IFGT      *ZEROS
048700980113     C* Aggancio AZORG per controllare se filiale italia/estero
048800020507     C                   MOVEL     *BLANKS       ORGFL1
048900020507     C                   MOVE      F£3(XY)       ORGFIL
049000020507     C     ORGFIL        CHAIN     AZORG01L                           32
049100020729     C                   clear                   OG143
049200020729     C  n32              movel     ORGDE3        OG143
049300020729     C                   movel     *blank        flgest            1
049400020729     C     §OgNTW        ifeq      'FED'
049500020729     C     §OgNTW        oreq      'EEX'
049600020729     C     §OgNTW        oreq      'EUP'
049700020729     C                   MOVEL     'E'           flgest
049800020729     c                   end
049900020729     C***  ORGFL1        IFEQ      ' '
050000020729     C     flgest        IFEQ      ' '
050100020507     C     D24TPI        ANDEQ     'I'
050200020729     C***  ORGFL1        OREQ      'E'
050300020729     C     flgest        OREQ      'E'
050400020507     C     D24TPI        ANDEQ     'E'
050500020507     C                   ADD       1             XX
050600020507     C                   MOVE      F£3(XY)       FIL(XX)
050700020507     C                   END
050800020507     C                   END
050900020507     C
051000020507     C                   END
051100980113     C*
051200020506     C*                  END
051300020906     C                   END
051400980520      *----------------------------------------------------*
051500980520      *  Se estero controllo ditta, se è SDI  *IN23 = ON  e  D24AWB = '1'
051600980520     C     D24TPI        IFEQ      'E'
051700980520     C     FIL(1)        CHAIN     AZORG01L                           31
051800020729     C                   clear                   OG143
051900020729     C  n31              movel     ORGDE3        OG143
052000020729     C                   movel     *blank        flgest            1
052100020729     C     §OgNTW        ifeq      'FED'
052200020729     C     §OgNTW        oreq      'EEX'
052300020729     C     §OgNTW        oreq      'EUP'
052400020729     C                   MOVEL     'E'           flgest
052500020729     c                   end
052600980520     C     *IN31         IFEQ      '0'
052700020729     C***  ORGFL1        ANDEQ     'E'
052800020729     C     flgest        ANDEQ     'E'
052900980520     C     ORGDIT        ANDEQ     'SDI'
053000980520     C                   SETON                                        23
053100980520     C                   MOVEL     '1'           D24AWB
053200980520     C                   ENDIF
053300980520     C                   ENDIF
053400980520      *
053500931018     C*----------------------------------------------------*
053600941104     C     D24ORD        COMP      'P'                                020201
053700931018     C*----------------------------------------------------*
053800941104     C*  Carico codici bolla
053900941104     C                   Z-ADD     1             KKUT
054000941104     C                   Z-ADD     0             XX                3 0
054100941104     C                   MOVE      '3A'          KCOD
054200980113     C     KTAB1         CHAIN     TABEL00F                           31
054300941104     C     *IN31         DOWEQ     '0'
054400941104     C     XX            ANDLT     100
054500941104     C     TBLFLG        IFEQ      ' '
054600941104     C                   MOVEL     TBLUNI        DS3A
054700941104     C                   ADD       1             XX
054800941104     C                   MOVEL     TBLKEY        C3A(XX)
054900941104     C                   MOVEL     §3AFCA        FCA(XX)
055000941104     C                   END
055100980113     C     KTAB1         READE     TABEL00F                               31
055200941104     C                   END
055300941104     C                   CLOSE     TABEL00F
055400980520      *----------------------------------------------------*
055500931018     C                   ENDSR
