000100931018     H DECEDIT('0,') DATEDIT(*DMY.)
000200931018?     *--------------------------------------------------------------*
000300941104      * TNSC25R1                                                     *
000400931018      *       *-----------------------------------------*            *
000500931018      *       *          STAMPE  CONTRASSEGNI           *            *
000600931018      *       *       STAMPA SCOPERTI IN PARTENZA       *            *
000700931018      *       *-----------------------------------------*            *
000800931018      *                                                              *
000900931018?     *--------------------------------------------------------------*
001000940218      * 01 -ORDINATO PER LINEA DI PARTENZA                           *
001100940218      * 02 -ORDINATO PER LINEA DI ARRIVO                             *
001200940218      * 21 -CHAIN SU CNACO                                           *
001300941104      * 31 -READ FNBLP                                               *
001400941104      * 32 -PER READ O CHAIN                                         *
001500940218      * 24 -CONFRONTO CLIENTE CON STRINGA                            *
001600940218      * 75 -INDICATORE DI COMODO                                     *
001700940218?     *--------------------------------------------------------------*
001800010717     F*****FNBLP31CIF  E           K        DISK
001900010717     F*****            FNSTB000                          KIGNORE
002000010717     FFNBLP31L  IF   E           K DISK
002100941104?     *---------
002200051114     FFIAR901L  IF   E           K DISK
002300931018?     *---------
002400050408     Ftigcp01L  IF   E           K DISK
002500931018?     *---------
002600941104     FFNCSB01L  IF   E           K DISK
002700931018?     *---------
002800941104     FFNCSBWRK  O  A E             DISK
002900931025?     *---------
003000931025     FCNACO00F  IF   E           K DISK
003100941104?     *---------
003200941104     FTABEL00F  IF   E           K DISK    USROPN
003300010108?     *---------
003400010108     FAZORG01L  IF   E           K DISK
003401070702?     *---------
003402070702     FFNDCT02L  IF   E           K DISK
003403070702     FFNLBL01L  IF   E           K DISK
003500931025     D*--------------------------------------------------------------*
003600931025     D*  SCHIERE                                                     *
003700931025     D*--------------------------------------------------------------*
003800941104     D C3A             S              2    DIM(100)
003900941104     D FCA             S              1    DIM(100)
004000941104     D FIL             S              3  0 DIM(30)
004100020506     D*FIL1            S              3    DIM(30)
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
005700931025     D TCUDS           DS
005800931025     D  F56                    5      6
005900931025     D  F34                    3      4
006000931025?    D*-
006100931018     D CNCR80        E DS
006200020506     d trul06ds      e ds
006300020506     d  lin                    1     90  0 dim(30)
006400020731     d savpjbu         s                   like(kpjbu)
006500010108     D OG143         E DS
006600931018      *
006700931018      *---------------------------------------------------------------*
006800931018      *   C I C L O      P R I N C I P A L E                          *
006900931018      *---------------------------------------------------------------*
007000941104     C*  Leggo l'archivio bolle in base alla filiali selez.
007100020506     C                   DO        30            X
007200941104     C                   MOVE      FIL(X)        KLNP
007300941104     C                   MOVEL     D24SDD        KAAS
007400941104     C                   MOVE      D24SDD        KMGS
007500010108      *PER PARZIALIZZAZIONE NEL LANCIO VERIFICO L'APPARTENENZA DELLA
007600010108      *FILIALE ALLA RICHIESTA IMMESSA BARTOLINI/POSTE
007700010108      *
007800010108     C     D24PAR        IFNE      *BLANK
007900010108     C     FIL(X)        CHAIN     AZORG01L                           99
008000010108     C     *IN99         IFEQ      '0'
008100010108     C                   MOVE      ORGDE3        OG143
008200010108      *PARZIALIZZAZIONE POSTE
008300010108     C     D24PAR        IFEQ      'P'
008400020729     C**** §OGPT         IFEQ      *BLANK
008500020729     C     §OgNtw        IFne      'PPT'
008600010108     C                   ITER
008700010108     C                   END
008800010108     C                   ELSE
008900010108      *PARZIALIZZAZIONE BARTOLINI
009000020729     C**** §OGPT         IFEQ      'S'
009100020729     C     §OgNtw        IFEQ      'PPT'
009200010108     C                   ITER
009300010108     C                   END
009400010108     C                   END
009500010108      *
009600010108     C                   END
009700010108     C                   END
009800010108      *
009900010717     C*****      KBLP1     SETLLFNBLP31C
010000010717     C*****      KBLP2     READEFNBLP31C                 31
010100010717     C     KBLP1         SETLL     FNBLP31L
010200010717     C     KBLP2         READE     FNBLP31L                               31
010300941104     C     *IN31         IFEQ      '0'
010400941104     C                   MOVEL     BLPAAS        DATSPE            8 0
010500931018     C                   MOVE      BLPMGS        DATSPE
010600931018     C*
010700931018     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
010800941104     C     *IN31         DOWEQ     '0'
010900941104     C     DATSPE        ANDLE     D24SDA
011000941104     C*  Bolle arrivate a destinazione
011100941104     C     BLPFT1        IFNE      *BLANKS
011200941104     C                   EXSR      WRTREC
011300941104     C                   END
011400931018     C*  Calcolo la nuova data di spedizione x il record letto
011500010717     C*****      KBLP2     READEFNBLP31C                 31
011600010717     C     KBLP2         READE     FNBLP31L                               31
011700941104     C     *IN31         IFEQ      '0'
011800941104     C                   MOVEL     BLPAAS        DATSPE            8 0
011900931018     C                   MOVE      BLPMGS        DATSPE
012000931018     C                   END
012100941104     C                   END
012200931018     C*
012300941104     C                   END
012400931018     C*
012500941104     C                   END
012600941104     C*
012700931025     C     FINE          TAG
012800931018     C                   SETON                                        LR
012900931018      *---------------------------------------------------------------*
013000931018      *  WRTREC: Scrivo record se rientra nei limiti richiesti        *
013100931018      *---------------------------------------------------------------*
013200931018     C     WRTREC        BEGSR
013300941104     C*
013400941104     C*  Controllo se la bolla ha il contrassegno
013500941104     C                   Z-ADD     1             XX
013600941104     C     BLPCBO        LOOKUP    C3A(XX)                                32
013700941104     C     *IN32         IFEQ      '1'
013800941104     C     FCA(XX)       ANDGT     '0'
013900941104     C*  esiste C/Assegno !!!
014000941104     C                   MOVE      BLPAAS        KAAS
014100941104     C                   MOVE      BLPLNP        KLNP
014200941104     C                   MOVE      BLPNRS        KNRS
014300941104     C                   MOVE      BLPNSP        KNSP
014400051114     C     KSPE          CHAIN     FIAR901L                           32
014500941104     C     *IN32         IFEQ      '0'
014600051114     C     AR9CAS        ANDNE     0
014601070702     C*
014700931018     C*  Controllo se contrassegno incassato
014800941104     C     KSPE          CHAIN     FNCSB01L                           32
014900941104     C     *IN32         IFEQ      '1'
015000941104     C     CSBDDC        OREQ      0
015001070702     C*
015002070702      *? Controlla le C.A. solo se il parametro passato dal video è <> da "S"  ?
015003070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
015004070702     c                   move      'S'           ctrl_CA_OK        1
015005070702     c                   if        d24xCA <> 'S'
015006070702     c                   exsr      Controlla_C_A
015008070702     c                   end
015009070702     C*
015010070702     c                   if        ctrl_CA_OK = 'S'
015100941104     C                   CLEAR                   FNCSWRK
015200931018     C* Scrivo record se rientra nelle selezioni di codice richiesto
015300931025     C                   MOVE      BLPRSM        CSBRSM
015400931019     C     BLPCCM        IFNE      0
015500931025     C* Ragione sociale mittente
015600931025     C                   MOVE      BLPCCM        SERV              4 0
015700931025     C                   SETOFF                                       75
015800931025     C*
015900941107     C                   MOVE      BLPCCM        KKSC
016000931025     C     SERV          IFNE      *ALL'8'
016100931025     C     SERV          ANDNE     *ALL'9'
016200931025     C     KACO          CHAIN     CNACO000                           21
016300931025     C  N21              MOVEL     ACORAG        CSBRSM
016400931025     C                   END
016500931025     C*
016600931018     C                   ELSE
016700941107     C                   MOVE      BLPKSC        KKSC
016800931018     C                   END
016900931018     C*  Test. x Codice cliente
017000941104     C     D24CDI        IFEQ      0
017100941107     C     D24CDI        OREQ      KKSC
017200931018     C*  Test. x Ragione sociale
017300941104     C     D24RAG        IFNE      *BLANK
017400931018     C                   SETOFF                                       40
017500931018     C*  Loop di ricerca sottostringa nel campo del subfile
017600931018     C                   DO        X             T                 2 0
017700941104     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
017800941104     C     RESULT        COMP      D24RAG                                 40
017900931018     C  N40              END
018000931018     C*
018100931018     C                   ELSE
018200931018     C                   SETON                                        40
018300931018     C                   END
018400931018     C*
018500931018     C     *IN40         IFEQ      '1'
018600931018     C*  Imposto le chiavi
018700931018     C   01              MOVE      BLPLNP        CSBKE1
018800931018     C   02              MOVE      BLPLNA        CSBKE1
018900931018     C                   MOVE      *ZERO         CSBKE2
019000931018     C                   MOVEL     BLPRSM        CSBKE3
019100931018     C                   MOVE      BLPNSP        CSBKE3
019104070702     C*
019200931018     C*  Scrivo i dati
019300931018     C                   MOVE      BLPFLE        CSBFLE
019400931018     C                   MOVE      BLPLNA        CSBLNA
019500931018     C                   MOVE      BLPLNP        CSBLNP
019600931018     C                   MOVE      BLPAAS        CSBAAS
019700931018     C                   MOVE      BLPNRS        CSBNRS
019800931018     C                   MOVE      BLPNSP        CSBNSP
019900941104     C                   MOVE      BLPAAS        CSBAAS
020000941104     C                   MOVE      BLPMGS        CSBMGS
020100011001     C** CON L'EURO NON SI DEVE PIÙ MEMORIZZARE IL CAMBIO
020200011001     C** SUGLI ARCHIVI.
020300011001     C                   Z-ADD     0             CSBCMB
020400011001     C*****                Z-ADDBL9CMB    CSBCMB
020500051114     C                   Z-ADD     AR9CAS        CSBCAS
020600051114     C                   MOVEL     AR9VCA        CSBVCA
020700931019     C                   MOVEL     BLPRSD        CSBRSD
020800950117     C                   MOVEL     BLPCCA        CSBCCA
020900941104     C                   MOVE      BLPRMA        CSBRMA
021000931018     C                   MOVE      BLPRMN        CSBRMN
021100931018     C     BLPDCM        IFEQ      0
021200931018     C                   MOVE      BLPDCP        CSBDCM
021300931018     C                   ELSE
021400931018     C                   MOVE      BLPDCM        CSBDCM
021500931018     C                   END
021600931018     C*  Reperisco data inizio giacenza
021700931018     C                   MOVE      BLPAAS        GCPAAS
021800931018     C                   MOVE      BLPLNP        GCPLNP
021900931018     C                   MOVE      BLPNRS        GCPNRS
022000931018     C                   MOVE      BLPNSP        GCPNSP
022100931018     C                   MOVE      *ZERO         GCPFRG
022200050408     C     KGCP          CHAIN     tigcp01L                           32
022300941104     C     *IN32         IFEQ      '0'
022400931018     C                   MOVEL     GCPAGC        CSBDGC
022500931018     C                   MOVE      GCPMGC        CSBDGC
022600931018     C                   END
022700931018     C*
022800941104     C                   WRITE     FNCSWRK
022900931018     C*
023000931018     C                   END
023100931018     C                   END
023200931018     C*
023201070702     c                   endIF
023202070702     C*
023300931018     C                   END
023500941104     C                   END
023700941104     C                   END
023800931018     C*
023900931018     C                   ENDSR
023901070702      *---------------------------------------------------------------*
023903070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
024000931018      *---------------------------------------------------------------*
024001070702     C     Controlla_C_A begSR
024002070702     C*
024006070702     C* Se la spedizione ha una pratica CA
024008070702     C                   z-add     Kaas          K_aas
024009070702     C                   z-add     Klnp          K_lnp
024010070702     C                   z-add     Knrs          K_nrs
024011070702     C                   z-add     Knsp          K_nsp
024012070702     C     Kspe          CHAIN     FNLBL01L
024013070702     C                   if        %found(FNLBL01L)
024014070702     C                   z-add     lblaao        K_aas
024015070702     C                   z-add     lbllpo        K_lnp
024016070702     C                   z-add     lblnro        K_nrs
024017070702     C                   z-add     lblnso        K_nsp
024019070702     C                   end
024020070702      *
024021070702      *  Controlla se ci sono C.A. e se sono aperte
024022070702     c                   move      'N'           ci_sono_CA        1
024023070702     c                   move      'N'           CA_aperte         1
024024070702     C     KDCT          setll     FNDCT02L
024025070702     C     KDCT          reade     FNDCT02L
024026070702     c                   dow       not %eof(FNDCT02L)
024027070702     c                   if        DCTatb <> 'A'
024028070702     c                   move      'S'           ci_sono_CA
024030070702      * se solo C.A. Aperte
024031070702     C                   if        Dctdch=0
024032070702     c                   move      'S'           CA_aperte         1
024034070702     c                   end
024035070702     c                   end
024040070702     C     KDCT          READE     FNDCT02L                               34
024041070702     C                   ENDdo
024042070702      *
024043070702      *  Controlla quindi con la selezione impostata a video
024044070702      *    Si devono scartare le spedizioni che hanno C.A. (aperte e chiuse)
024045070702     c                   if        d24xCA = 'N' and ci_sono_CA = 'S'
024047070702     c                   move      'N'           ctrl_CA_OK
024048070702     c                   end
024049070702      *
024050070702      *    Si devono scartare le spedizioni che hanno C.A. chiuse
024051070702     c                   if        d24xCA = 'C' and ci_sono_CA = 'S' and
024052070702     c                             CA_aperte = 'N'
024053070702     c                   move      'N'           ctrl_CA_OK
024054070702     c                   end
024055070702      *
024056070702     C                   ENDSR
024057070702      *---------------------------------------------------------------*
024100941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
024200931018      *---------------------------------------------------------------*
024300941104     C     *INZSR        BEGSR
024400931018      *---------------------------------------------------------------*
024500931018     C     *ENTRY        PLIST
024600931018     C                   PARM                    KPJBA
024700941104     C                   MOVEL     KPJBU         TNSC24
024800931025     C*
024900931025     C                   Z-ADD     1             CODUT
025000931025     C                   CALL      'X§PARUT'
025100931025     C                   PARM                    UT§DSE
025200931025     C     CODUT         CABEQ     -1            FINE
025300941104     C                   MOVEL     REC80         CNCR80
025400931025     C*
025500941104     C                   DO        50            X                 2 0
025600941104     C                   MOVE      TCU(X)        TCUDS
025700931025     C     F34           IFEQ      '01'
025800931025     C     F56           ANDEQ     'CG'
025900941104     C                   Z-ADD     KCU(X)        CLIITA            4 0
026000931025     C                   END
026100931025     C                   END
026200941104     C*---------------------------------------------------------------*
026300941104     C*  Definizione delle chiavi
026400931025     C     KACO          KLIST
026500941104     C                   KFLD                    KKUT
026600941104     C                   KFLD                    KKCC
026700941104     C                   KFLD                    KKSC
026800931025     C*
026900941104     C     KBLP1         KLIST
027000941104     C                   KFLD                    KLNP
027100941104     C                   KFLD                    KAAS
027200941104     C                   KFLD                    KMGS
027300941104     C*
027400941104     C     KBLP2         KLIST
027500941104     C                   KFLD                    KLNP
027600931018     C*
027700941104     C     KSPE          KLIST
027800941104     C                   KFLD                    KAAS
027900941104     C                   KFLD                    KLNP
028000941104     C                   KFLD                    KNRS
028100941104     C                   KFLD                    KNSP
028101070702     C*
028102070702     C     KDCT          KLIST
028103070702     C                   KFLD                    K_AAS
028104070702     C                   KFLD                    K_LNP
028105070702     C                   KFLD                    K_NRS
028106070702     C                   KFLD                    K_NSP
028200931018     C*
028300941104     C     KGCP          KLIST
028400941104     C                   KFLD                    KAAS
028500941104     C                   KFLD                    KLNP
028600941104     C                   KFLD                    KNRS
028700941104     C                   KFLD                    KNSP
028800941104     C                   KFLD                    KFRG
028900941104     C*
029000941104     C     KTAB1         KLIST
029100941104     C                   KFLD                    KKUT
029200941104     C                   KFLD                    KCOD
029300941104     C                   KFLD                    KKEY
029400941104     C*
029500941104     C     KTAB2         KLIST
029600941104     C                   KFLD                    KKUT
029700941104     C                   KFLD                    KCOD
029800941104     C*---------------------------------------------------------------*
029900941104     C*  Definizione delle variabili
030000941104     C     *LIKE         DEFINE    ACOKUT        KKUT
030100941104     C     *LIKE         DEFINE    ACOKCC        KKCC
030200941104     C     *LIKE         DEFINE    ACOKSC        KKSC
030300941104     C     *LIKE         DEFINE    BLPLNP        KLNP
030400941104     C     *LIKE         DEFINE    BLPAAS        KAAS
030500941104     C     *LIKE         DEFINE    BLPMGS        KMGS
030600941104     C     *LIKE         DEFINE    BLPNRS        KNRS
030700941104     C     *LIKE         DEFINE    BLPNSP        KNSP
030800941104     C     *LIKE         DEFINE    GCPFRG        KFRG
030900941104     C     *LIKE         DEFINE    TBLCOD        KCOD
031000941104     C     *LIKE         DEFINE    TBLKEY        KKEY
031001070702     C     *LIKE         DEFINE    BLPAAS        K_AAS
031002070702     C     *LIKE         DEFINE    BLPLNP        K_LNP
031003070702     C     *LIKE         DEFINE    BLPNRS        K_NRS
031004070702     C     *LIKE         DEFINE    BLPNSP        K_NSP
031100931018     C*----------------------------------------------------*
031200931018     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
031300941104     C                   MOVE      '  '          D24RAG
031400941104     C     '  '          SCAN      D24RAG        LENGHT            2 0
031500931018     C                   SUB       1             LENGHT
031600941104     C     35            SUB       LENGHT        X                 2 0
031700931018     C*----------------------------------------------------*
031800941104     C*  Inizializzo varibili
031900941104     C                   Z-ADD     1             KKUT
032000941104     C                   Z-ADD     CLIITA        KKCC
032100941104     C     D24ORD        COMP      'P'                                020201
032200941104     C*----------------------------------------------------*
032300941104     C*  Carico codici bolla
032400941104     C                   Z-ADD     0             XX                3 0
032500941104     C                   MOVE      '3A'          KCOD
032600941104     C                   OPEN      TABEL00F
032700941104     C     KTAB2         CHAIN     TABEL00F                           31
032800941104     C     *IN31         DOWEQ     '0'
032900941104     C     XX            ANDLT     100
033000941104     C     TBLFLG        IFEQ      ' '
033100941104     C                   MOVEL     TBLUNI        DS3A
033200941104     C                   ADD       1             XX
033300941104     C                   MOVEL     TBLKEY        C3A(XX)
033400941104     C                   MOVEL     §3AFCA        FCA(XX)
033500941104     C                   END
033600941104     C     KTAB2         READE     TABEL00F                               31
033700941104     C                   END
033800941104     C*----------------------------------------------------*
033900941104     C*  Controllo se devo caricare in schiera le filiali
034000941104     C*  gestite sull'AS
034100941104     C     D24LIN        IFEQ      *ZEROS
034200020731     c                   movel(p)  kpjbu         savpjbu
034300020506     c                   clear                   trul06ds
034400020506     c                   eval      d06cod = '£1'
034500020506     c                   movel     simfel        d06key
034600020506     c                   movel     trul06ds      kpjbu
034700020506     c                   call      'TRUL06R'
034800020506     c                   parm                    kpjba
034900020506     c                   movel     kpjbu         trul06ds
035000020506     c                   movea     lin           fil
035100020731     c                   movel(p)  savpjbu       kpjbu
035200020506     C*                  MOVEL     '£3'          KCOD
035300020506     C*                  MOVEL     SIMFEL        KKEY
035400020506     C*    KTAB1         CHAIN     TABEL00F                           31
035500020506     C*    *IN21         IFEQ      '0'
035600020506     C*    TBLFLG        ANDEQ     ' '
035700020506     C*                  MOVEA     TBLUNI        FIL1
035800020506     C*                  Z-ADD     0             Y                 2 0
035900020506     C*                  DO        30            X                 2 0
036000020506     C*    FIL1(X)       IFGT      *ZEROS
036100020506     C*                  ADD       1             Y
036200020506     C*                  MOVE      FIL1(X)       FIL(Y)
036300020506     C*                  END
036400020506     C*                  END
036500020506     C*                  ELSE
036600020506     C*                  MOVE      SIMFEL        FIL(1)
036700020506     C*                  Z-ADD     1             Y
036800020506     C*                  END
036900941104     C*
037000020906     C                   ELSE
037100020906     C                   MOVE      D24LIN        FIL(1)
037200020506     C*                  Z-ADD     1             Y
037300941104     C                   END
037400941104     C*
037500941104     C                   CLOSE     TABEL00F
037600931018     C*----------------------------------------------------*
037700931018     C                   ENDSR
