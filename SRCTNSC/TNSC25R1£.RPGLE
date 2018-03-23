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
003500070702?     *---------
003600070702     FFNDCT02L  IF   E           K DISK
003700070702     FFNLBL01L  IF   E           K DISK
003800931025     D*--------------------------------------------------------------*
003900931025     D*  SCHIERE                                                     *
004000931025     D*--------------------------------------------------------------*
004100941104     D C3A             S              2    DIM(100)
004200941104     D FCA             S              1    DIM(100)
004300080813     D FIL             S              3  0 DIM(60)
004400020506     D*FIL1            S              3    DIM(30)
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
006000931025     D TCUDS           DS
006100931025     D  F56                    5      6
006200931025     D  F34                    3      4
006300931025?    D*-
006400931018     D CNCR80        E DS
006500080813     D TRUL72DS      E DS
006600080813     D  Lin                           3s 0 DESCEND
006700080813     D                                     DIM(60)
006800080813     D                                     OVERLAY(O72Lia)
006900020731     d savpjbu         s                   like(kpjbu)
007000010108     D OG143         E DS
007100931018      *
007200931018      *---------------------------------------------------------------*
007300931018      *   C I C L O      P R I N C I P A L E                          *
007400931018      *---------------------------------------------------------------*
007500941104     C*  Leggo l'archivio bolle in base alla filiali selez.
007600080813     C                   DO        60            X
007700941104     C                   MOVE      FIL(X)        KLNP
007800941104     C                   MOVEL     D24SDD        KAAS
007900941104     C                   MOVE      D24SDD        KMGS
008000010108      *PER PARZIALIZZAZIONE NEL LANCIO VERIFICO L'APPARTENENZA DELLA
008100010108      *FILIALE ALLA RICHIESTA IMMESSA BARTOLINI/POSTE
008200010108      *
008300010108     C     D24PAR        IFNE      *BLANK
008400010108     C     FIL(X)        CHAIN     AZORG01L                           99
008500010108     C     *IN99         IFEQ      '0'
008600010108     C                   MOVE      ORGDE3        OG143
008700010108      *PARZIALIZZAZIONE POSTE
008800010108     C     D24PAR        IFEQ      'P'
008900020729     C**** §OGPT         IFEQ      *BLANK
009000020729     C     §OgNtw        IFne      'PPT'
009100010108     C                   ITER
009200010108     C                   END
009300010108     C                   ELSE
009400010108      *PARZIALIZZAZIONE BARTOLINI
009500020729     C**** §OGPT         IFEQ      'S'
009600020729     C     §OgNtw        IFEQ      'PPT'
009700010108     C                   ITER
009800010108     C                   END
009900010108     C                   END
010000010108      *
010100010108     C                   END
010200010108     C                   END
010300010108      *
010400010717     C*****      KBLP1     SETLLFNBLP31C
010500010717     C*****      KBLP2     READEFNBLP31C                 31
010600010717     C     KBLP1         SETLL     FNBLP31L
010700010717     C     KBLP2         READE     FNBLP31L                               31
010800941104     C     *IN31         IFEQ      '0'
010900941104     C                   MOVEL     BLPAAS        DATSPE            8 0
011000931018     C                   MOVE      BLPMGS        DATSPE
011100931018     C*
011200931018     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
011300941104     C     *IN31         DOWEQ     '0'
011400941104     C     DATSPE        ANDLE     D24SDA
011500941104     C*  Bolle arrivate a destinazione
011600941104     C     BLPFT1        IFNE      *BLANKS
011700941104     C                   EXSR      WRTREC
011800941104     C                   END
011900931018     C*  Calcolo la nuova data di spedizione x il record letto
012000010717     C*****      KBLP2     READEFNBLP31C                 31
012100010717     C     KBLP2         READE     FNBLP31L                               31
012200941104     C     *IN31         IFEQ      '0'
012300941104     C                   MOVEL     BLPAAS        DATSPE            8 0
012400931018     C                   MOVE      BLPMGS        DATSPE
012500931018     C                   END
012600941104     C                   END
012700931018     C*
012800941104     C                   END
012900931018     C*
013000941104     C                   END
013100941104     C*
013200931025     C     FINE          TAG
013300931018     C                   SETON                                        LR
013400931018      *---------------------------------------------------------------*
013500931018      *  WRTREC: Scrivo record se rientra nei limiti richiesti        *
013600931018      *---------------------------------------------------------------*
013700931018     C     WRTREC        BEGSR
013800941104     C*
013900941104     C*  Controllo se la bolla ha il contrassegno
014000941104     C                   Z-ADD     1             XX
014100941104     C     BLPCBO        LOOKUP    C3A(XX)                                32
014200941104     C     *IN32         IFEQ      '1'
014300941104     C     FCA(XX)       ANDGT     '0'
014400941104     C*  esiste C/Assegno !!!
014500941104     C                   MOVE      BLPAAS        KAAS
014600941104     C                   MOVE      BLPLNP        KLNP
014700941104     C                   MOVE      BLPNRS        KNRS
014800941104     C                   MOVE      BLPNSP        KNSP
014900051114     C     KSPE          CHAIN     FIAR901L                           32
015000941104     C     *IN32         IFEQ      '0'
015100051114     C     AR9CAS        ANDNE     0
015200070702     C*
015300931018     C*  Controllo se contrassegno incassato
015400941104     C     KSPE          CHAIN     FNCSB01L                           32
015500941104     C     *IN32         IFEQ      '1'
015600941104     C     CSBDDC        OREQ      0
015700070702     C*
015800070702      *? Controlla le C.A. solo se il parametro passato dal video è <> da "S"  ?
015900070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
016000070702     c                   move      'S'           ctrl_CA_OK        1
016100070702     c                   if        d24xCA <> 'S'
016200070702     c                   exsr      Controlla_C_A
016300070702     c                   end
016400070702     C*
016500070702     c                   if        ctrl_CA_OK = 'S'
016600941104     C                   CLEAR                   FNCSWRK
016700931018     C* Scrivo record se rientra nelle selezioni di codice richiesto
016800931025     C                   MOVE      BLPRSM        CSBRSM
016900931019     C     BLPCCM        IFNE      0
017000931025     C* Ragione sociale mittente
017100931025     C                   MOVE      BLPCCM        SERV              4 0
017200931025     C                   SETOFF                                       75
017300931025     C*
017400941107     C                   MOVE      BLPCCM        KKSC
017500931025     C     SERV          IFNE      *ALL'8'
017600931025     C     SERV          ANDNE     *ALL'9'
017700931025     C     KACO          CHAIN     CNACO000                           21
017800931025     C  N21              MOVEL     ACORAG        CSBRSM
017900931025     C                   END
018000931025     C*
018100931018     C                   ELSE
018200941107     C                   MOVE      BLPKSC        KKSC
018300931018     C                   END
018400931018     C*  Test. x Codice cliente
018500941104     C     D24CDI        IFEQ      0
018600941107     C     D24CDI        OREQ      KKSC
018700931018     C*  Test. x Ragione sociale
018800941104     C     D24RAG        IFNE      *BLANK
018900931018     C                   SETOFF                                       40
019000931018     C*  Loop di ricerca sottostringa nel campo del subfile
019100931018     C                   DO        X             T                 2 0
019200941104     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
019300941104     C     RESULT        COMP      D24RAG                                 40
019400931018     C  N40              END
019500931018     C*
019600931018     C                   ELSE
019700931018     C                   SETON                                        40
019800931018     C                   END
019900931018     C*
020000931018     C     *IN40         IFEQ      '1'
020100931018     C*  Imposto le chiavi
020200931018     C   01              MOVE      BLPLNP        CSBKE1
020300931018     C   02              MOVE      BLPLNA        CSBKE1
020400931018     C                   MOVE      *ZERO         CSBKE2
020500931018     C                   MOVEL     BLPRSM        CSBKE3
020600931018     C                   MOVE      BLPNSP        CSBKE3
020700070702     C*
020800931018     C*  Scrivo i dati
020900931018     C                   MOVE      BLPFLE        CSBFLE
021000931018     C                   MOVE      BLPLNA        CSBLNA
021100931018     C                   MOVE      BLPLNP        CSBLNP
021200931018     C                   MOVE      BLPAAS        CSBAAS
021300931018     C                   MOVE      BLPNRS        CSBNRS
021400931018     C                   MOVE      BLPNSP        CSBNSP
021500941104     C                   MOVE      BLPAAS        CSBAAS
021600941104     C                   MOVE      BLPMGS        CSBMGS
021700011001     C** CON L'EURO NON SI DEVE PIÙ MEMORIZZARE IL CAMBIO
021800011001     C** SUGLI ARCHIVI.
021900011001     C                   Z-ADD     0             CSBCMB
022000011001     C*****                Z-ADDBL9CMB    CSBCMB
022100051114     C                   Z-ADD     AR9CAS        CSBCAS
022200051114     C                   MOVEL     AR9VCA        CSBVCA
022300931019     C                   MOVEL     BLPRSD        CSBRSD
022400950117     C                   MOVEL     BLPCCA        CSBCCA
022500941104     C                   MOVE      BLPRMA        CSBRMA
022600931018     C                   MOVE      BLPRMN        CSBRMN
022700931018     C     BLPDCM        IFEQ      0
022800931018     C                   MOVE      BLPDCP        CSBDCM
022900931018     C                   ELSE
023000931018     C                   MOVE      BLPDCM        CSBDCM
023100931018     C                   END
023200931018     C*  Reperisco data inizio giacenza
023300931018     C                   MOVE      BLPAAS        GCPAAS
023400931018     C                   MOVE      BLPLNP        GCPLNP
023500931018     C                   MOVE      BLPNRS        GCPNRS
023600931018     C                   MOVE      BLPNSP        GCPNSP
023700931018     C                   MOVE      *ZERO         GCPFRG
023800050408     C     KGCP          CHAIN     tigcp01L                           32
023900941104     C     *IN32         IFEQ      '0'
024000931018     C                   MOVEL     GCPAGC        CSBDGC
024100931018     C                   MOVE      GCPMGC        CSBDGC
024200931018     C                   END
024300931018     C*
024400941104     C                   WRITE     FNCSWRK
024500931018     C*
024600931018     C                   END
024700931018     C                   END
024800931018     C*
024900070702     c                   endIF
025000070702     C*
025100931018     C                   END
025200941104     C                   END
025300941104     C                   END
025400931018     C*
025500931018     C                   ENDSR
025600070702      *---------------------------------------------------------------*
025700070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
025800931018      *---------------------------------------------------------------*
025900070702     C     Controlla_C_A begSR
026000070702     C*
026100070702     C* Se la spedizione ha una pratica CA
026200070702     C                   z-add     Kaas          K_aas
026300070702     C                   z-add     Klnp          K_lnp
026400070702     C                   z-add     Knrs          K_nrs
026500070702     C                   z-add     Knsp          K_nsp
026600070702     C     Kspe          CHAIN     FNLBL01L
026700070702     C                   if        %found(FNLBL01L)
026800070702     C                   z-add     lblaao        K_aas
026900070702     C                   z-add     lbllpo        K_lnp
027000070702     C                   z-add     lblnro        K_nrs
027100070702     C                   z-add     lblnso        K_nsp
027200070702     C                   end
027300070702      *
027400070702      *  Controlla se ci sono C.A. e se sono aperte
027500070702     c                   move      'N'           ci_sono_CA        1
027600070702     c                   move      'N'           CA_aperte         1
027700070702     C     KDCT          setll     FNDCT02L
027800070702     C     KDCT          reade     FNDCT02L
027900070702     c                   dow       not %eof(FNDCT02L)
028000070702     c                   if        DCTatb <> 'A'
028100070702     c                   move      'S'           ci_sono_CA
028200070702      * se solo C.A. Aperte
028300070702     C                   if        Dctdch=0
028400070702     c                   move      'S'           CA_aperte         1
028500070702     c                   end
028600070702     c                   end
028700070702     C     KDCT          READE     FNDCT02L                               34
028800070702     C                   ENDdo
028900070702      *
029000070702      *  Controlla quindi con la selezione impostata a video
029100070702      *    Si devono scartare le spedizioni che hanno C.A. (aperte e chiuse)
029200070702     c                   if        d24xCA = 'N' and ci_sono_CA = 'S'
029300070702     c                   move      'N'           ctrl_CA_OK
029400070702     c                   end
029500070702      *
029600070702      *    Si devono scartare le spedizioni che hanno C.A. chiuse
029700070702     c                   if        d24xCA = 'C' and ci_sono_CA = 'S' and
029800070702     c                             CA_aperte = 'N'
029900070702     c                   move      'N'           ctrl_CA_OK
030000070702     c                   end
030100070702      *
030200070702     C                   ENDSR
030300070702      *---------------------------------------------------------------*
030400941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
030500931018      *---------------------------------------------------------------*
030600941104     C     *INZSR        BEGSR
030700931018      *---------------------------------------------------------------*
030800931018     C     *ENTRY        PLIST
030900931018     C                   PARM                    KPJBA
031000941104     C                   MOVEL     KPJBU         TNSC24
031100931025     C*
031200931025     C                   Z-ADD     1             CODUT
031300931025     C                   CALL      'X§PARUT'
031400931025     C                   PARM                    UT§DSE
031500931025     C     CODUT         CABEQ     -1            FINE
031600941104     C                   MOVEL     REC80         CNCR80
031700931025     C*
031800941104     C                   DO        50            X                 2 0
031900941104     C                   MOVE      TCU(X)        TCUDS
032000931025     C     F34           IFEQ      '01'
032100931025     C     F56           ANDEQ     'CG'
032200941104     C                   Z-ADD     KCU(X)        CLIITA            4 0
032300931025     C                   END
032400931025     C                   END
032500941104     C*---------------------------------------------------------------*
032600941104     C*  Definizione delle chiavi
032700931025     C     KACO          KLIST
032800941104     C                   KFLD                    KKUT
032900941104     C                   KFLD                    KKCC
033000941104     C                   KFLD                    KKSC
033100931025     C*
033200941104     C     KBLP1         KLIST
033300941104     C                   KFLD                    KLNP
033400941104     C                   KFLD                    KAAS
033500941104     C                   KFLD                    KMGS
033600941104     C*
033700941104     C     KBLP2         KLIST
033800941104     C                   KFLD                    KLNP
033900931018     C*
034000941104     C     KSPE          KLIST
034100941104     C                   KFLD                    KAAS
034200941104     C                   KFLD                    KLNP
034300941104     C                   KFLD                    KNRS
034400941104     C                   KFLD                    KNSP
034500070702     C*
034600070702     C     KDCT          KLIST
034700070702     C                   KFLD                    K_AAS
034800070702     C                   KFLD                    K_LNP
034900070702     C                   KFLD                    K_NRS
035000070702     C                   KFLD                    K_NSP
035100931018     C*
035200941104     C     KGCP          KLIST
035300941104     C                   KFLD                    KAAS
035400941104     C                   KFLD                    KLNP
035500941104     C                   KFLD                    KNRS
035600941104     C                   KFLD                    KNSP
035700941104     C                   KFLD                    KFRG
035800941104     C*
035900941104     C     KTAB1         KLIST
036000941104     C                   KFLD                    KKUT
036100941104     C                   KFLD                    KCOD
036200941104     C                   KFLD                    KKEY
036300941104     C*
036400941104     C     KTAB2         KLIST
036500941104     C                   KFLD                    KKUT
036600941104     C                   KFLD                    KCOD
036700941104     C*---------------------------------------------------------------*
036800941104     C*  Definizione delle variabili
036900941104     C     *LIKE         DEFINE    ACOKUT        KKUT
037000941104     C     *LIKE         DEFINE    ACOKCC        KKCC
037100941104     C     *LIKE         DEFINE    ACOKSC        KKSC
037200941104     C     *LIKE         DEFINE    BLPLNP        KLNP
037300941104     C     *LIKE         DEFINE    BLPAAS        KAAS
037400941104     C     *LIKE         DEFINE    BLPMGS        KMGS
037500941104     C     *LIKE         DEFINE    BLPNRS        KNRS
037600941104     C     *LIKE         DEFINE    BLPNSP        KNSP
037700941104     C     *LIKE         DEFINE    GCPFRG        KFRG
037800941104     C     *LIKE         DEFINE    TBLCOD        KCOD
037900941104     C     *LIKE         DEFINE    TBLKEY        KKEY
038000070702     C     *LIKE         DEFINE    BLPAAS        K_AAS
038100070702     C     *LIKE         DEFINE    BLPLNP        K_LNP
038200070702     C     *LIKE         DEFINE    BLPNRS        K_NRS
038300070702     C     *LIKE         DEFINE    BLPNSP        K_NSP
038400931018     C*----------------------------------------------------*
038500931018     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
038600941104     C                   MOVE      '  '          D24RAG
038700941104     C     '  '          SCAN      D24RAG        LENGHT            2 0
038800931018     C                   SUB       1             LENGHT
038900941104     C     35            SUB       LENGHT        X                 2 0
039000931018     C*----------------------------------------------------*
039100941104     C*  Inizializzo varibili
039200941104     C                   Z-ADD     1             KKUT
039300941104     C                   Z-ADD     CLIITA        KKCC
039400941104     C     D24ORD        COMP      'P'                                020201
039500941104     C*----------------------------------------------------*
039600941104     C*  Carico codici bolla
039700941104     C                   Z-ADD     0             XX                3 0
039800941104     C                   MOVE      '3A'          KCOD
039900941104     C                   OPEN      TABEL00F
040000941104     C     KTAB2         CHAIN     TABEL00F                           31
040100941104     C     *IN31         DOWEQ     '0'
040200941104     C     XX            ANDLT     100
040300941104     C     TBLFLG        IFEQ      ' '
040400941104     C                   MOVEL     TBLUNI        DS3A
040500941104     C                   ADD       1             XX
040600941104     C                   MOVEL     TBLKEY        C3A(XX)
040700941104     C                   MOVEL     §3AFCA        FCA(XX)
040800941104     C                   END
040900941104     C     KTAB2         READE     TABEL00F                               31
041000941104     C                   END
041100941104     C*----------------------------------------------------*
041200941104     C*  Controllo se devo caricare in schiera le filiali
041300941104     C*  gestite sull'AS
041400941104     C     D24LIN        IFEQ      *ZEROS
041500020731     c                   movel(p)  kpjbu         savpjbu
041600080813     c                   clear                   trul72ds
041700080813     c                   eval      i72tla = 'L'
041800080813     c                   eval      I72POU = simfel
041900080813     c                   eval      kpjbu = trul72ds
042000080813     C                   CALL      'TRUL72R'
042100080813     C                   PARM                    KPJBA
042200080813     c                   movel     kpjbu         trul72ds
042300080813     c                   movea     lin           fil
042400020731     c                   movel(p)  savpjbu       kpjbu
042500020506     C*                  MOVEL     '£3'          KCOD
042600020506     C*                  MOVEL     SIMFEL        KKEY
042700020506     C*    KTAB1         CHAIN     TABEL00F                           31
042800020506     C*    *IN21         IFEQ      '0'
042900020506     C*    TBLFLG        ANDEQ     ' '
043000020506     C*                  MOVEA     TBLUNI        FIL1
043100020506     C*                  Z-ADD     0             Y                 2 0
043200020506     C*                  DO        30            X                 2 0
043300020506     C*    FIL1(X)       IFGT      *ZEROS
043400020506     C*                  ADD       1             Y
043500020506     C*                  MOVE      FIL1(X)       FIL(Y)
043600020506     C*                  END
043700020506     C*                  END
043800020506     C*                  ELSE
043900020506     C*                  MOVE      SIMFEL        FIL(1)
044000020506     C*                  Z-ADD     1             Y
044100020506     C*                  END
044200941104     C*
044300020906     C                   ELSE
044400020906     C                   MOVE      D24LIN        FIL(1)
044500020506     C*                  Z-ADD     1             Y
044600941104     C                   END
044700941104     C*
044800941104     C                   CLOSE     TABEL00F
044900931018     C*----------------------------------------------------*
045000931018     C                   ENDSR
