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
003800140120     Ftntbe01L  IF   E           K DISK
003900931025     D*--------------------------------------------------------------*
004000931025     D*  SCHIERE                                                     *
004100931025     D*--------------------------------------------------------------*
004200941104     D C3A             S              2    DIM(100)
004300941104     D FCA             S              1    DIM(100)
004400140120     D***** F£3             S              3  0 DIM(60)
004500140120     D F£3             S              3  0 DIM(250)
004600140120     D FIL             S              3  0 DIM(250)
004700931018?     *--------------------------------------------------------------*
004800931018?     *  DS                                                          *
004900931018?     *--------------------------------------------------------------*
005000931018     D KPJBA         E DS
005100931018      *
005200941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
005300941104     D DS3A          E DS
005400140120     D DNTW          E DS
005500931018      *
005600931025     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700931025     D  TCU                  398    697
005800931025     D                                     DIM(50)
005900931025     D  KCU                  698    847P 0
006000931025     D                                     DIM(50)
006100931025     D                                     PACKEVEN
006200931025?    D*-
006300980113     D TCUDS           DS
006400931025     D  F56                    5      6
006500931025     D  F34                    3      4
006600931112?    D*-
006700931018     D CNCR80        E DS
006800140120      ****-?Caricamento filiali contabili
006900140120     d TRUL31ds      e ds                  inz
007000140120     d $POg            s              3    inz   dim(250)
007100140120     d  Lin            s              3s 0 inz   dim(250)
007200140120      ****-?
007300020731     d savpjbu         s                   like(kpjbu)
007400010108     D OG143         E DS
007500980113?     *--------------------------------------------------------------*
007600980113?     *  Ridefinizione interna campi azorg                           *
007700980113?     *--------------------------------------------------------------*
007800980113     IAZORG01L  AA
007900980520     I                                  1    3  ORGDIT
008000980113     I                             P    4    5 0ORGFIL
008100980113     I                                 11   11  ORGFL1
008200010108     I                               3776 3800  ORGDE3
008300931018      *---------------------------------------------------------------*
008400931018      *   C I C L O      P R I N C I P A L E                          *
008500931018      *---------------------------------------------------------------*
008600931018     C*  Lettura archivio bolle arrivate
008700980612     C     D24TPI        IFEQ      'E'
008800980612     C                   MOVEL     D24SDD        KDT3
008900980612     C                   ELSE
009000941104     C                   MOVEL     D24SDD        ANNO              4 0
009100941104     C                   MOVE      D24SDD        MMGG              4 0
009200980612     C                   END
009300980113     C* loop letture x lna
009400020506     C                   Z-ADD     1             XY                3 0
009500980113     C     FIL(XY)       DOWNE     *HIVAL
009600140120     C     XY            ANDLT     250
009700980113     C                   Z-ADD     FIL(XY)       KLNA
009800980612     C     D24TPI        IFEQ      'E'
009900980612     C     KEYSP1        SETLL     FNARB55L
010000980612     C     KLNA          READE     FNARB55L                               80
010100980612     C     *IN80         IFEQ      '0'
010200980612     C                   MOVE      ARBLNA        SAVLIN            3 0
010300980612     C                   EXSR      LEGGIE
010400980612     C                   END
010500980612     C                   ELSE
010600941104     C     KEYSPE        SETLL     FNARB26L
010700980113     C     KLNA          READE     FNARB26L                               80
010800931018     C     *IN80         IFEQ      '0'
010900931018     C                   MOVE      ARBLNA        SAVLIN            3 0
011000941104     C                   MOVEL     ARBAAS        DATSPE            8 0
011100931018     C                   MOVE      ARBMGS        DATSPE
011200931018     C                   EXSR      LEGGI
011300931018     C                   END
011400980612     C*
011500980612     C                   END
011600980113     C*
011700980113     C                   ADD       1             XY
011800980113     C*
011900980113     C                   END
012000931018     C*
012100931025     C     FINE          TAG
012200980520     C                   MOVEL     TNSC24        KPJBU
012300931018     C                   SETON                                        LR
012400931018      *---------------------------------------------------------------*
012500931018      *  LEGGI: Lettura record rientranti nelle linee-date spediz.    *
012600931018      *---------------------------------------------------------------*
012700931018     C     LEGGI         BEGSR
012800931018     C*
012900931018     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
013000941104     C     DATSPE        DOWLE     D24SDA
013100931018     C     *IN80         ANDEQ     '0'
013200931018     C*  Scrivo record se rientra nei limiti richiesti
013300931018     C                   EXSR      WRTREC
013400931018     C*  Calcolo la nuova data di spedizione x il record letto
013500980113     C     KLNA          READE     FNARB26L                               80
013600931018     C     *IN80         IFEQ      '0'
013700941104     C                   MOVEL     ARBAAS        DATSPE            8 0
013800931018     C                   MOVE      ARBMGS        DATSPE
013900931018     C                   END
014000931018     C*
014100931018     C                   END
014200931018     C*
014300931018     C                   ENDSR
014400980612      *---------------------------------------------------------------*
014500980612      *  LEGGIE: Lettura record rientranti nelle linee-data manifest  *
014600980612      *---------------------------------------------------------------*
014700980612     C     LEGGIE        BEGSR
014800980612     C*
014900980612     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
015000980612     C     ARBDT3        DOWLE     D24SDA
015100980612     C     *IN80         ANDEQ     '0'
015200980612     C*  Scrivo record se rientra nei limiti richiesti
015300980612     C                   EXSR      WRTREC
015400980612     C*  Calcolo la nuova data di spedizione x il record letto
015500980612     C     KLNA          READE     FNARB55L                               80
015600980612     C*
015700980612     C                   END
015800980612     C*
015900980612     C                   ENDSR
016000931018      *---------------------------------------------------------------*
016100931018      *  WRTREC: Scrivo record se rientra nei limiti richiesti        *
016200931018      *---------------------------------------------------------------*
016300931018     C     WRTREC        BEGSR
016400931018     C*
016500941104     C                   Z-ADD     1             XX
016600941104     C     ARBCBO        LOOKUP    C3A(XX)                                32
016700941104     C     *IN32         IFEQ      '1'
016800941104     C     FCA(XX)       ANDGT     '0'
016900010108      *SELEZIONATA PARZIALIZZAZIONE PER BARTOLINI/POSTE
017000010108      *
017100010108     C     D24PAR        IFNE      *BLANK
017200010108     C     ARBLNP        CHAIN     AZORG01L                           99
017300010108     C     *IN99         IFEQ      '0'
017400010108     C                   MOVEL     ORGDE3        OG143
017500010108      * PARZIALIZ. POSTE SE FILIALE PARTENZA NON POSTE NON SCRIVO W.F
017600010108     C     D24PAR        IFEQ      'P'
017700020729     C     §OgNtw        CABne     'PPT'         NOWRT
017800010108     C                   ELSE
017900010108      * PARZIALIZ. BART. SE FILIALE PARTENZA NON BART. NON SCRIVO W.F
018000020729     C     §OgNtw        CABeq     'PPT'         NOWRT
018100010108     C                   END
018200010108     C                   END
018300010108     C                   END
018400010108      *
018500941104     C*  esiste C/Assegno !!!
018600941104     C                   MOVE      ARBAAS        KAAS
018700941104     C                   MOVE      ARBLNP        KLNP
018800941104     C                   MOVE      ARBNRS        KNRS
018900941104     C                   MOVE      ARBNSP        KNSP
019000051114     C     KSPE          CHAIN     FIAR901L                           32
019100941104     C     *IN32         IFEQ      '0'
019200941104     C     AR9CAS        ANDNE     0
019300070702      *
019400931018     C*  Controllo se contrassegno incassato
019500950306     C     ARBICC        IFEQ      *BLANKS
019600950306     C     ARBICC        OREQ      'R'
019700070702      *
019800070702      *? Controlla le C.A. solo se il parametro passato dal video è <> da "S"  ?
019900070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
020000070702     c                   move      'S'           ctrl_CA_OK        1
020100070702     c                   if        d24xCA <> 'S'
020200070702     c                   exsr      Controlla_C_A
020300070702     c                   end
020400070702     C*
020500070702     c                   if        ctrl_CA_OK = 'S'
020600941104     C                   CLEAR                   FNCSWRK
020700931018     C* Scrivo record se rientra nelle selezioni di codice richiesto
020800931025     C                   MOVEL     ARBRSM        CSBRSM
020900931019     C     ARBCCM        IFNE      0
021000931019     C                   MOVE      ARBCCM        CDI               7 0
021100931025     C* Ragione sociale mittente
021200931025     C                   MOVE      ARBCCM        SERV              4 0
021300931025     C                   SETOFF                                       75
021400931025     C*
021500931025     C     SERV          IFNE      *ALL'8'
021600931025     C     SERV          ORNE      *ALL'9'
021700931025     C     KACO          CHAIN     CNACO000                           21
021800931025     C  N21              MOVEL     ACORAG        CSBRSM
021900931025     C                   END
022000931025     C*
022100931025     C                   ELSE
022200931018     C                   MOVE      ARBKSC        CDI               7 0
022300931018     C                   END
022400931018     C*  Test. x Codice cliente
022500941104     C     D24CDI        IFEQ      0
022600941104     C     D24CDI        OREQ      CDI
022700931018     C*  Test. x Ragione sociale
022800980520     C     D§§RAG        IFNE      *BLANK
022900931018     C                   SETOFF                                       40
023000931018     C*  Loop di ricerca sottostringa nel campo del subfile
023100140120     C                   DO        Xlen          T                 2 0
023200941104     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
023300980520     C     RESULT        COMP      D§§RAG                                 40
023400931018     C  N40              END
023500931018     C*
023600931018     C                   ELSE
023700931018     C                   SETON                                        40
023800931018     C                   END
023900931018     C*
024000931018     C     *IN40         IFEQ      '1'
024100931018     C*  Imposto le chiavi
024200931018     C   01              MOVE      ARBLNP        CSBKE1
024300931018     C   02              MOVE      ARBLNA        CSBKE1
024400931018     C                   MOVE      *ZERO         CSBKE2
024500931018     C                   MOVEL     ARBRSM        CSBKE3
024600931018     C                   MOVE      ARBNSP        CSBKE3
024700931018     C*  Scrivo i dati
024800931018     C                   MOVE      ARBFLE        CSBFLE
024900931018     C                   MOVE      ARBLNA        CSBLNA
025000931018     C                   MOVE      ARBLNP        CSBLNP
025100931018     C                   MOVE      ARBAAS        CSBAAS
025200931018     C                   MOVE      ARBNRS        CSBNRS
025300931018     C                   MOVE      ARBNSP        CSBNSP
025400970625     C* Se stampa per estero imposto data trasmissione manifest in
025500970625     C* data PAGAMENTO
025600970625     C     D24TPI        IFNE      'I'
025700970625     C                   Z-ADD     ARBDT3        CSBDDP
025800970625     C                   END
025900941104     C                   MOVE      ARBAAS        CSBAAS
026000941104     C                   MOVE      ARBMGS        CSBMGS
026100941104     C                   Z-ADD     AR9CAS        CSBCAS
026200011001     C** CON L'EURO NON SI DEVE PIÙ MEMORIZZARE IL CAMBIO
026300011001     C** SUGLI ARCHIVI.
026400011001     C                   Z-ADD     0             CSBCMB
026500941104     C                   MOVEL     AR9VCA        CSBVCA
026600931019     C                   MOVEL     ARBRSD        CSBRSD
026700941104     C                   MOVE      ARBRMA        CSBRMA
026800931018     C                   MOVE      ARBRMN        CSBRMN
026900950117     C                   MOVE      ARBCCA        CSBCCA
027000931112     C                   MOVE      *ZERO         CSBDCM
027100941104     C     ARBDCM        IFEQ      0
027200931018     C                   MOVE      ARBDCP        CSBDCM
027300931018     C                   ELSE
027400931018     C                   MOVE      ARBDCM        CSBDCM
027500931018     C                   END
027600931018     C*  Reperisco data inizio giacenza
027700931018     C                   MOVE      ARBAAS        GCPAAS
027800931018     C                   MOVE      ARBLNP        GCPLNP
027900931018     C                   MOVE      ARBNRS        GCPNRS
028000931018     C                   MOVE      ARBNSP        GCPNSP
028100931018     C                   MOVE      *ZERO         GCPFRG
028200050309     C     KEYGCA        CHAIN     TIGCP21L                           71
028300931018     C     *IN71         IFEQ      '0'
028400931018     C                   MOVEL     GCPAGC        CSBDGC
028500931018     C                   MOVE      GCPMGC        CSBDGC
028600931018     C                   END
028700980520      *
028800980520      * Se stampa per estero SDI imposto AWB nel rif. mittente alfabetico
028900980520     C     *IN23         IFEQ      '1'
029000980520     C                   MOVEL     *BLANKS       CSBRMA
029100980520     C                   MOVEL     'E'           KTRC
029200060214     C     KEYAR4        CHAIN     FiAR401L                           72
029300980520     C  N72              MOVEL     AR4NOT        CSBRMA
029400980520     C                   END
029500980520      *
029600941104     C                   WRITE     FNCSWRK
029700931018     C*
029800931018     C                   END
029900931018     C                   END
030000070702     C*
030100070702     c                   endIF
030200931018     C*
030300931018     C                   END
030400950306     C                   END
030500941104     C                   END
030600931018     C*
030700010108     C     NOWRT         ENDSR
030800070702      *---------------------------------------------------------------*
030900070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
031000070702      *---------------------------------------------------------------*
031100070702     C     Controlla_C_A begSR
031200070702     C*
031300070702     C* Se la spedizione ha una pratica CA
031400070702     C                   z-add     Kaas          K_aas
031500070702     C                   z-add     Klnp          K_lnp
031600070702     C                   z-add     Knrs          K_nrs
031700070702     C                   z-add     Knsp          K_nsp
031800070702     C     Kspe          CHAIN     FNLBL01L
031900070702     C                   if        %found(FNLBL01L)
032000070702     C                   z-add     lblaao        K_aas
032100070702     C                   z-add     lbllpo        K_lnp
032200070702     C                   z-add     lblnro        K_nrs
032300070702     C                   z-add     lblnso        K_nsp
032400070702     C                   end
032500070702      *
032600070702      *  Controlla se ci sono C.A. e se sono aperte
032700070702     c                   move      'N'           ci_sono_CA        1
032800070702     c                   move      'N'           CA_aperte         1
032900070702     C     KDCT          setll     FNDCT02L
033000070702     C     KDCT          reade     FNDCT02L
033100070702     c                   dow       not %eof(FNDCT02L)
033200070702     c                   if        DCTatb <> 'A'
033300070702     c                   move      'S'           ci_sono_CA
033400070702      * se solo C.A. Aperte
033500070702     C                   if        Dctdch=0
033600070702     c                   move      'S'           CA_aperte         1
033700070702     c                   end
033800070702     c                   end
033900070702     C     KDCT          READE     FNDCT02L                               34
034000070702     C                   ENDdo
034100070702      *
034200070702      *  Controlla quindi con la selezione impostata a video
034300070702      *    Si devono scartare le spedizioni che hanno C.A. (aperte e chiuse)
034400070702     c                   if        d24xCA = 'N' and ci_sono_CA = 'S'
034500070702     c                   move      'N'           ctrl_CA_OK
034600070702     c                   end
034700070702      *
034800070702      *    Si devono scartare le spedizioni che hanno C.A. chiuse
034900070702     c                   if        d24xCA = 'C' and ci_sono_CA = 'S' and
035000070702     c                             CA_aperte = 'N'
035100070702     c                   move      'N'           ctrl_CA_OK
035200070702     c                   end
035300070702      *
035400070702     C                   ENDSR
035500931018      *---------------------------------------------------------------*
035600941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
035700931018      *---------------------------------------------------------------*
035800941104     C     *INZSR        BEGSR
035900931018      *---------------------------------------------------------------*
036000931018     C     *ENTRY        PLIST
036100931018     C                   PARM                    KPJBA
036200941104     C                   MOVEL     KPJBU         TNSC24
036300931025     C*
036400931025     C                   Z-ADD     1             CODUT
036500931025     C                   CALL      'X§PARUT'
036600931025     C                   PARM                    UT§DSE
036700931025     C     CODUT         CABEQ     -1            FINE
036800980113     C                   MOVEL     REC80         CNCR80
036900931025     C*
037000931025     C                   DO        50            F                 2 0
037100931025     C                   MOVE      TCU(F)        TCUDS
037200931025     C     F34           IFEQ      '01'
037300931025     C     F56           ANDEQ     'CG'
037400931025     C                   Z-ADD     KCU(F)        CLIITA            4 0
037500931025     C                   END
037600931025     C                   END
037700931025     C*
037800931025     C* CNACO00F
037900931025     C     KACO          KLIST
038000931025     C                   KFLD                    CODUT
038100931025     C                   KFLD                    CLIITA
038200931025     C                   KFLD                    CDI
038300931018     C*
038400931018     C     KEYSPE        KLIST
038500980113     C                   KFLD                    KLNA
038600931018     C                   KFLD                    ANNO
038700931018     C                   KFLD                    MMGG
038800980612     C     KEYSP1        KLIST
038900980612     C                   KFLD                    KLNA
039000980612     C                   KFLD                    KDT3
039100931018     C*
039200931018     C     KEYCSB        KLIST
039300931018     C                   KFLD                    CSBAAS
039400931018     C                   KFLD                    CSBLNP
039500931018     C                   KFLD                    CSBNRS
039600931018     C                   KFLD                    CSBNSP
039700070702     C*
039800070702     C     KDCT          KLIST
039900070702     C                   KFLD                    K_AAS
040000070702     C                   KFLD                    K_LNP
040100070702     C                   KFLD                    K_NRS
040200070702     C                   KFLD                    K_NSP
040300980520     C*
040400980520     C     KEYAR4        KLIST
040500980520     C                   KFLD                    CSBAAS
040600980520     C                   KFLD                    CSBLNP
040700980520     C                   KFLD                    CSBNRS
040800980520     C                   KFLD                    CSBNSP
040900980520     C                   KFLD                    KTRC
041000931018     C*
041100931018     C     KEYGCA        KLIST
041200931018     C                   KFLD                    GCPAAS
041300931018     C                   KFLD                    GCPLNP
041400931018     C                   KFLD                    GCPNRS
041500931018     C                   KFLD                    GCPNSP
041600931018     C                   KFLD                    GCPFRG
041700941104     C*
041800941104     C     KSPE          KLIST
041900941104     C                   KFLD                    KAAS
042000941104     C                   KFLD                    KLNP
042100941104     C                   KFLD                    KNRS
042200941104     C                   KFLD                    KNSP
042300941104     C*
042400980113     C     KTAB1         KLIST
042500941104     C                   KFLD                    KKUT
042600941104     C                   KFLD                    KCOD
042700980113     C*
042800980113     C     KTAB2         KLIST
042900980113     C                   KFLD                    KKUT
043000980113     C                   KFLD                    KCOD
043100980113     C                   KFLD                    KKEY
043200140120     C*
043300140120     C     KTBE          KLIST
043400140120     C                   KFLD                    TBECOD
043500140120     C                   KFLD                    TBEKE1
043600941104     C*---------------------------------------------------------------*
043700941104     C*  Definizione delle variabili
043800980520     C     *LIKE         DEFINE    AR4TRC        KTRC
043900980520     C     *LIKE         DEFINE    D24RAG        D§§RAG
044000941104     C     *LIKE         DEFINE    ARBLNP        KLNP
044100941104     C     *LIKE         DEFINE    ARBAAS        KAAS
044200941104     C     *LIKE         DEFINE    ARBMGS        KMGS
044300941104     C     *LIKE         DEFINE    ARBNRS        KNRS
044400941104     C     *LIKE         DEFINE    ARBNSP        KNSP
044500980113     C     *LIKE         DEFINE    ARBLNA        KLNA
044600980612     C     *LIKE         DEFINE    ARBDT3        KDT3
044700941104     C     *LIKE         DEFINE    TBLKUT        KKUT
044800941104     C     *LIKE         DEFINE    TBLCOD        KCOD
044900941104     C     *LIKE         DEFINE    TBLKEY        KKEY
045000070702     C     *LIKE         DEFINE    arbAAS        K_AAS
045100070702     C     *LIKE         DEFINE    arbLNP        K_LNP
045200070702     C     *LIKE         DEFINE    arbNRS        K_NRS
045300070702     C     *LIKE         DEFINE    arbNSP        K_NSP
045400931018     C*----------------------------------------------------*
045500931018     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
045600980520     C                   MOVE      D24RAG        D§§RAG
045700980520     C                   MOVE      '  '          D§§RAG
045800980520     C     '  '          SCAN      D§§RAG        LENGHT            2 0
045900931018     C                   SUB       1             LENGHT
046000140120     C     25            SUB       LENGHT        Xlen              2 0
046100980113     C*----------------------------------------------------*
046200980113     C                   OPEN      TABEL00F
046300931018     C*----------------------------------------------------*
046400980113     C* Se ho selezionato una sola filiale carico solo
046500980113     C* quella in schiera altrimenti scorro la £3 e carico
046600980113     C* solo filiali Italia/Estero
046700980113     C                   Z-ADD     *HIVAL        FIL
046800140120     C***
046900140120      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
047000140120     c                   clear                   TRUL31DS
047100140120     c                   eval      I31abi = 'AC'
047200140120     c                   eval      I31cpo = simfel
047300140120     c                   call      'TRUL31R'
047400140120     c                   parm                    KPJBA
047500140120     c                   parm                    TRUL31DS
047600140120     c                   if        o31pog > *zeros
047700140120     c                   movea     o31pog        $pog
047800140120     c                   endif
047900140120      *
048000140120     C                   DO        250           Xg                3 0
048100140120     c                   if           $pog(Xg) <> *blank
048200140120     c                   move      $pog(Xg)      lin(Xg)
048300140120     c                   else
048400140120     c                   move      '000'         lin(Xg)
048500140120     c                   endif
048600140120     c                   endDO
048700140120      ***
048800080813     c                   movea     lin           f£3
048900140120      ***
049000020731     c                   movel(p)  savpjbu       kpjbu
049100020906     C     D24LIN        IFNE      0
049200020906     C                   Z-ADD     D24LIN        FIL(1)
049300020906     C                   ELSE
049400020506     C*                  Z-ADD     1             KKUT
049500020506     C*                  Z-ADD     0             XX                3 0
049600020506     C*                  MOVE      '£3'          KCOD
049700020506     C*                  MOVEL     *BLANKS       KKEY
049800020506     C*                  MOVEL     SIMFEL        KKEY
049900020506     C*    KTAB2         CHAIN     TABEL00F                           31
050000020506     C*    *IN31         IFEQ      '0'
050100020506     C*    TBLFLG        ANDEQ     ' '
050200020506     C*                  MOVEA     TBLUNI        F£3
050300020507     C                   DO        30            XY                3 0
050400980113     C* Filiale £3
050500020507     C     F£3(XY)       IFGT      *ZEROS
050600980113     C* Aggancio AZORG per controllare se filiale italia/estero
050700020507     C                   MOVEL     *BLANKS       ORGFL1
050800020507     C                   MOVE      F£3(XY)       ORGFIL
050900020507     C     ORGFIL        CHAIN     AZORG01L                           32
051000020729     C                   clear                   OG143
051100020729     C  n32              movel     ORGDE3        OG143
051200020729     C                   movel     *blank        flgest            1
051300140120      *-- ITALIA/ESTERO
051400140120     c                   movel     'NTW'         tbeCOD
051500140120     c                   movel(p)  §OgNTW        tbeKE1
051600140120     c     kTBE          chain     tnTBE01L
051700140120     c                   if        %Found(tnTBE01L)
051800140120     c                   eval      dNTW = tbeUNI
051900140120     c                   eval      flgest = §NTWFIE
052000140120     c                   end
052100140120      *--
052200020729     C     flgest        IFEQ      ' '
052300020507     C     D24TPI        ANDEQ     'I'
052400020729     C     flgest        OREQ      'E'
052500020507     C     D24TPI        ANDEQ     'E'
052600020507     C                   ADD       1             XX
052700020507     C                   MOVE      F£3(XY)       FIL(XX)
052800020507     C                   END
052900020507     C                   END
053000020507     C
053100020507     C                   END
053200980113     C*
053300020506     C*                  END
053400020906     C                   END
053500980520      *----------------------------------------------------*
053600980520      *  Se estero controllo ditta, se è SDI  *IN23 = ON  e  D24AWB = '1'
053700980520     C     D24TPI        IFEQ      'E'
053800980520     C     FIL(1)        CHAIN     AZORG01L                           31
053900020729     C                   clear                   OG143
054000020729     C  n31              movel     ORGDE3        OG143
054100020729     C                   movel     *blank        flgest            1
054200140120      *-- ITALIA/ESTERO
054300140120     c                   movel     'NTW'         tbeCOD
054400140120     c                   movel(p)  §OgNTW        tbeKE1
054500140120     c     kTBE          chain     tnTBE01L
054600140120     c                   if        %Found(tnTBE01L)
054700140120     c                   eval      dNTW = tbeUNI
054800140120     c                   eval      flgest = §NTWFIE
054900140120     c                   end
055000140120      *--
055100980520     C     *IN31         IFEQ      '0'
055200020729     C     flgest        ANDEQ     'E'
055300980520     C     ORGDIT        ANDEQ     'SDI'
055400980520     C                   SETON                                        23
055500980520     C                   MOVEL     '1'           D24AWB
055600980520     C                   ENDIF
055700980520     C                   ENDIF
055800980520      *
055900931018     C*----------------------------------------------------*
056000941104     C     D24ORD        COMP      'P'                                020201
056100931018     C*----------------------------------------------------*
056200941104     C*  Carico codici bolla
056300941104     C                   Z-ADD     1             KKUT
056400941104     C                   Z-ADD     0             XX                3 0
056500941104     C                   MOVE      '3A'          KCOD
056600980113     C     KTAB1         CHAIN     TABEL00F                           31
056700941104     C     *IN31         DOWEQ     '0'
056800941104     C     XX            ANDLT     100
056900941104     C     TBLFLG        IFEQ      ' '
057000941104     C                   MOVEL     TBLUNI        DS3A
057100941104     C                   ADD       1             XX
057200941104     C                   MOVEL     TBLKEY        C3A(XX)
057300941104     C                   MOVEL     §3AFCA        FCA(XX)
057400941104     C                   END
057500980113     C     KTAB1         READE     TABEL00F                               31
057600941104     C                   END
057700941104     C                   CLOSE     TABEL00F
057800980520      *----------------------------------------------------*
057900931018     C                   ENDSR
