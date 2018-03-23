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
001800010717     FFNBLP31L  IF   E           K DISK
001900941104?     *---------
002000051114     FFIAR901L  IF   E           K DISK
002100931018?     *---------
002200050408     Ftigcp01L  IF   E           K DISK
002300931018?     *---------
002400941104     FFNCSB01L  IF   E           K DISK
002500931018?     *---------
002600941104     FFNCSBWRK  O  A E             DISK
002700931025?     *---------
002800931025     FCNACO00F  IF   E           K DISK
002900941104?     *---------
003000941104     FTABEL00F  IF   E           K DISK    USROPN
003100010108?     *---------
003200010108     FAZORG01L  IF   E           K DISK
003300070702?     *---------
003400070702     FFNDCT02L  IF   E           K DISK
003500070702     FFNLBL01L  IF   E           K DISK
003600931025     D*--------------------------------------------------------------*
003700931025     D*  SCHIERE                                                     *
003800931025     D*--------------------------------------------------------------*
003900941104     D C3A             S              2    DIM(100)
004000941104     D FCA             S              1    DIM(100)
004100140120     D FIL             S              3  0 DIM(250)
004200140120     D**** FIL             S              3  0 DIM(60)
004300020506     D*FIL1            S              3    DIM(30)
004400931018?     *--------------------------------------------------------------*
004500931018?     *  DS                                                          *
004600931018?     *--------------------------------------------------------------*
004700931018     D KPJBA         E DS
004800931018      *
004900941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
005000941104     D DS3A          E DS
005100931018      *
005200931025     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005300931025     D  TCU                  398    697
005400931025     D                                     DIM(50)
005500931025     D  KCU                  698    847P 0
005600931025     D                                     DIM(50)
005700931025     D                                     PACKEVEN
005800931025?    D*-
005900931025     D TCUDS           DS
006000931025     D  F56                    5      6
006100931025     D  F34                    3      4
006200931025?    D*-
006300140120     D CNCR80        E DS
006400140120      ****-?Caricamento filiali contabili
006500140120     d TRUL31ds      e ds                  inz
006600140120     d $POg            s              3    inz   dim(250)
006700140120      ****-?
006800140120      *
006900020731     d savpjbu         s                   like(kpjbu)
007000010108     D OG143         E DS
007100931018      *
007200931018      *---------------------------------------------------------------*
007300931018      *   C I C L O      P R I N C I P A L E                          *
007400931018      *---------------------------------------------------------------*
007500941104     C*  Leggo l'archivio bolle in base alla filiali selez.
007600140120     C*****              DO        60            X
007700140120     C                   DO        250           Xg
007800140120     C                   MOVE      FIL(Xg)       KLNP
007900941104     C                   MOVEL     D24SDD        KAAS
008000941104     C                   MOVE      D24SDD        KMGS
008100010108      *PER PARZIALIZZAZIONE NEL LANCIO VERIFICO L'APPARTENENZA DELLA
008200010108      *FILIALE ALLA RICHIESTA IMMESSA BARTOLINI/POSTE
008300010108      *
008400010108     C     D24PAR        IFNE      *BLANK
008500140120     C     FIL(Xg)       CHAIN     AZORG01L                           99
008600010108     C     *IN99         IFEQ      '0'
008700010108     C                   MOVE      ORGDE3        OG143
008800010108      *PARZIALIZZAZIONE POSTE
008900010108     C     D24PAR        IFEQ      'P'
009000020729     C     §OgNtw        IFne      'PPT'
009100010108     C                   ITER
009200010108     C                   END
009300010108     C                   ELSE
009400010108      *PARZIALIZZAZIONE BARTOLINI
009500020729     C     §OgNtw        IFEQ      'PPT'
009600010108     C                   ITER
009700010108     C                   END
009800010108     C                   END
009900010108      *
010000010108     C                   END
010100010108     C                   END
010200010108      *
010300010717     C     KBLP1         SETLL     FNBLP31L
010400010717     C     KBLP2         READE     FNBLP31L                               31
010500941104     C     *IN31         IFEQ      '0'
010600941104     C                   MOVEL     BLPAAS        DATSPE            8 0
010700931018     C                   MOVE      BLPMGS        DATSPE
010800931018     C*
010900931018     C*  Ciclo di letture fino a fine file o data sped. > limite richiesto
011000941104     C     *IN31         DOWEQ     '0'
011100941104     C     DATSPE        ANDLE     D24SDA
011200941104     C*  Bolle arrivate a destinazione
011300941104     C     BLPFT1        IFNE      *BLANKS
011400941104     C                   EXSR      WRTREC
011500941104     C                   END
011600931018     C*  Calcolo la nuova data di spedizione x il record letto
011700010717     C     KBLP2         READE     FNBLP31L                               31
011800941104     C     *IN31         IFEQ      '0'
011900941104     C                   MOVEL     BLPAAS        DATSPE            8 0
012000931018     C                   MOVE      BLPMGS        DATSPE
012100931018     C                   END
012200941104     C                   END
012300931018     C*
012400941104     C                   END
012500931018     C*
012600941104     C                   END
012700941104     C*
012800931025     C     FINE          TAG
012900931018     C                   SETON                                        LR
013000931018      *---------------------------------------------------------------*
013100931018      *  WRTREC: Scrivo record se rientra nei limiti richiesti        *
013200931018      *---------------------------------------------------------------*
013300931018     C     WRTREC        BEGSR
013400941104     C*
013500941104     C*  Controllo se la bolla ha il contrassegno
013600941104     C                   Z-ADD     1             XX
013700941104     C     BLPCBO        LOOKUP    C3A(XX)                                32
013800941104     C     *IN32         IFEQ      '1'
013900941104     C     FCA(XX)       ANDGT     '0'
014000941104     C*  esiste C/Assegno !!!
014100941104     C                   MOVE      BLPAAS        KAAS
014200941104     C                   MOVE      BLPLNP        KLNP
014300941104     C                   MOVE      BLPNRS        KNRS
014400941104     C                   MOVE      BLPNSP        KNSP
014500051114     C     KSPE          CHAIN     FIAR901L                           32
014600941104     C     *IN32         IFEQ      '0'
014700051114     C     AR9CAS        ANDNE     0
014800070702     C*
014900931018     C*  Controllo se contrassegno incassato
015000941104     C     KSPE          CHAIN     FNCSB01L                           32
015100941104     C     *IN32         IFEQ      '1'
015200941104     C     CSBDDC        OREQ      0
015300070702     C*
015400070702      *? Controlla le C.A. solo se il parametro passato dal video è <> da "S"  ?
015500070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
015600070702     c                   move      'S'           ctrl_CA_OK        1
015700070702     c                   if        d24xCA <> 'S'
015800070702     c                   exsr      Controlla_C_A
015900070702     c                   end
016000070702     C*
016100070702     c                   if        ctrl_CA_OK = 'S'
016200941104     C                   CLEAR                   FNCSWRK
016300931018     C* Scrivo record se rientra nelle selezioni di codice richiesto
016400931025     C                   MOVE      BLPRSM        CSBRSM
016500931019     C     BLPCCM        IFNE      0
016600931025     C* Ragione sociale mittente
016700931025     C                   MOVE      BLPCCM        SERV              4 0
016800931025     C                   SETOFF                                       75
016900931025     C*
017000941107     C                   MOVE      BLPCCM        KKSC
017100931025     C     SERV          IFNE      *ALL'8'
017200931025     C     SERV          ANDNE     *ALL'9'
017300931025     C     KACO          CHAIN     CNACO000                           21
017400931025     C  N21              MOVEL     ACORAG        CSBRSM
017500931025     C                   END
017600931025     C*
017700931018     C                   ELSE
017800941107     C                   MOVE      BLPKSC        KKSC
017900931018     C                   END
018000931018     C*  Test. x Codice cliente
018100941104     C     D24CDI        IFEQ      0
018200941107     C     D24CDI        OREQ      KKSC
018300931018     C*  Test. x Ragione sociale
018400941104     C     D24RAG        IFNE      *BLANK
018500931018     C                   SETOFF                                       40
018600931018     C*  Loop di ricerca sottostringa nel campo del subfile
018700140120     C                   DO        Xlen          T                 2 0
018800941104     C     LENGHT        SUBST(P)  CSBRSM:T      RESULT           35
018900941104     C     RESULT        COMP      D24RAG                                 40
019000931018     C  N40              END
019100931018     C*
019200931018     C                   ELSE
019300931018     C                   SETON                                        40
019400931018     C                   END
019500931018     C*
019600931018     C     *IN40         IFEQ      '1'
019700931018     C*  Imposto le chiavi
019800931018     C   01              MOVE      BLPLNP        CSBKE1
019900931018     C   02              MOVE      BLPLNA        CSBKE1
020000931018     C                   MOVE      *ZERO         CSBKE2
020100931018     C                   MOVEL     BLPRSM        CSBKE3
020200931018     C                   MOVE      BLPNSP        CSBKE3
020300070702     C*
020400931018     C*  Scrivo i dati
020500931018     C                   MOVE      BLPFLE        CSBFLE
020600931018     C                   MOVE      BLPLNA        CSBLNA
020700931018     C                   MOVE      BLPLNP        CSBLNP
020800931018     C                   MOVE      BLPAAS        CSBAAS
020900931018     C                   MOVE      BLPNRS        CSBNRS
021000931018     C                   MOVE      BLPNSP        CSBNSP
021100941104     C                   MOVE      BLPAAS        CSBAAS
021200941104     C                   MOVE      BLPMGS        CSBMGS
021300011001     C** CON L'EURO NON SI DEVE PIÙ MEMORIZZARE IL CAMBIO
021400011001     C** SUGLI ARCHIVI.
021500011001     C                   Z-ADD     0             CSBCMB
021600051114     C                   Z-ADD     AR9CAS        CSBCAS
021700051114     C                   MOVEL     AR9VCA        CSBVCA
021800931019     C                   MOVEL     BLPRSD        CSBRSD
021900950117     C                   MOVEL     BLPCCA        CSBCCA
022000941104     C                   MOVE      BLPRMA        CSBRMA
022100931018     C                   MOVE      BLPRMN        CSBRMN
022200931018     C     BLPDCM        IFEQ      0
022300931018     C                   MOVE      BLPDCP        CSBDCM
022400931018     C                   ELSE
022500931018     C                   MOVE      BLPDCM        CSBDCM
022600931018     C                   END
022700931018     C*  Reperisco data inizio giacenza
022800931018     C                   MOVE      BLPAAS        GCPAAS
022900931018     C                   MOVE      BLPLNP        GCPLNP
023000931018     C                   MOVE      BLPNRS        GCPNRS
023100931018     C                   MOVE      BLPNSP        GCPNSP
023200931018     C                   MOVE      *ZERO         GCPFRG
023300050408     C     KGCP          CHAIN     tigcp01L                           32
023400941104     C     *IN32         IFEQ      '0'
023500931018     C                   MOVEL     GCPAGC        CSBDGC
023600931018     C                   MOVE      GCPMGC        CSBDGC
023700931018     C                   END
023800931018     C*
023900941104     C                   WRITE     FNCSWRK
024000931018     C*
024100931018     C                   END
024200931018     C                   END
024300931018     C*
024400070702     c                   endIF
024500070702     C*
024600931018     C                   END
024700941104     C                   END
024800941104     C                   END
024900931018     C*
025000931018     C                   ENDSR
025100070702      *---------------------------------------------------------------*
025200070702      *? N= Senza C.A. / C= Solo C.A. Aperte.  ?
025300931018      *---------------------------------------------------------------*
025400070702     C     Controlla_C_A begSR
025500070702     C*
025600070702     C* Se la spedizione ha una pratica CA
025700070702     C                   z-add     Kaas          K_aas
025800070702     C                   z-add     Klnp          K_lnp
025900070702     C                   z-add     Knrs          K_nrs
026000070702     C                   z-add     Knsp          K_nsp
026100070702     C     Kspe          CHAIN     FNLBL01L
026200070702     C                   if        %found(FNLBL01L)
026300070702     C                   z-add     lblaao        K_aas
026400070702     C                   z-add     lbllpo        K_lnp
026500070702     C                   z-add     lblnro        K_nrs
026600070702     C                   z-add     lblnso        K_nsp
026700070702     C                   end
026800070702      *
026900070702      *  Controlla se ci sono C.A. e se sono aperte
027000070702     c                   move      'N'           ci_sono_CA        1
027100070702     c                   move      'N'           CA_aperte         1
027200070702     C     KDCT          setll     FNDCT02L
027300070702     C     KDCT          reade     FNDCT02L
027400070702     c                   dow       not %eof(FNDCT02L)
027500070702     c                   if        DCTatb <> 'A'
027600070702     c                   move      'S'           ci_sono_CA
027700070702      * se solo C.A. Aperte
027800070702     C                   if        Dctdch=0
027900070702     c                   move      'S'           CA_aperte         1
028000070702     c                   end
028100070702     c                   end
028200070702     C     KDCT          READE     FNDCT02L                               34
028300070702     C                   ENDdo
028400070702      *
028500070702      *  Controlla quindi con la selezione impostata a video
028600070702      *    Si devono scartare le spedizioni che hanno C.A. (aperte e chiuse)
028700070702     c                   if        d24xCA = 'N' and ci_sono_CA = 'S'
028800070702     c                   move      'N'           ctrl_CA_OK
028900070702     c                   end
029000070702      *
029100070702      *    Si devono scartare le spedizioni che hanno C.A. chiuse
029200070702     c                   if        d24xCA = 'C' and ci_sono_CA = 'S' and
029300070702     c                             CA_aperte = 'N'
029400070702     c                   move      'N'           ctrl_CA_OK
029500070702     c                   end
029600070702      *
029700070702     C                   ENDSR
029800070702      *---------------------------------------------------------------*
029900941104      *  *INZSR: OPERAZIONI INIZIALI                                  *
030000931018      *---------------------------------------------------------------*
030100941104     C     *INZSR        BEGSR
030200931018      *---------------------------------------------------------------*
030300931018     C     *ENTRY        PLIST
030400931018     C                   PARM                    KPJBA
030500941104     C                   MOVEL     KPJBU         TNSC24
030600931025     C*
030700931025     C                   Z-ADD     1             CODUT
030800931025     C                   CALL      'X§PARUT'
030900931025     C                   PARM                    UT§DSE
031000931025     C     CODUT         CABEQ     -1            FINE
031100941104     C                   MOVEL     REC80         CNCR80
031200931025     C*
031300140120     C                   DO        50            X                 2 0
031400941104     C                   MOVE      TCU(X)        TCUDS
031500931025     C     F34           IFEQ      '01'
031600931025     C     F56           ANDEQ     'CG'
031700941104     C                   Z-ADD     KCU(X)        CLIITA            4 0
031800931025     C                   END
031900931025     C                   END
032000941104     C*---------------------------------------------------------------*
032100941104     C*  Definizione delle chiavi
032200931025     C     KACO          KLIST
032300941104     C                   KFLD                    KKUT
032400941104     C                   KFLD                    KKCC
032500941104     C                   KFLD                    KKSC
032600931025     C*
032700941104     C     KBLP1         KLIST
032800941104     C                   KFLD                    KLNP
032900941104     C                   KFLD                    KAAS
033000941104     C                   KFLD                    KMGS
033100941104     C*
033200941104     C     KBLP2         KLIST
033300941104     C                   KFLD                    KLNP
033400931018     C*
033500941104     C     KSPE          KLIST
033600941104     C                   KFLD                    KAAS
033700941104     C                   KFLD                    KLNP
033800941104     C                   KFLD                    KNRS
033900941104     C                   KFLD                    KNSP
034000070702     C*
034100070702     C     KDCT          KLIST
034200070702     C                   KFLD                    K_AAS
034300070702     C                   KFLD                    K_LNP
034400070702     C                   KFLD                    K_NRS
034500070702     C                   KFLD                    K_NSP
034600931018     C*
034700941104     C     KGCP          KLIST
034800941104     C                   KFLD                    KAAS
034900941104     C                   KFLD                    KLNP
035000941104     C                   KFLD                    KNRS
035100941104     C                   KFLD                    KNSP
035200941104     C                   KFLD                    KFRG
035300941104     C*
035400941104     C     KTAB1         KLIST
035500941104     C                   KFLD                    KKUT
035600941104     C                   KFLD                    KCOD
035700941104     C                   KFLD                    KKEY
035800941104     C*
035900941104     C     KTAB2         KLIST
036000941104     C                   KFLD                    KKUT
036100941104     C                   KFLD                    KCOD
036200941104     C*---------------------------------------------------------------*
036300941104     C*  Definizione delle variabili
036400941104     C     *LIKE         DEFINE    ACOKUT        KKUT
036500941104     C     *LIKE         DEFINE    ACOKCC        KKCC
036600941104     C     *LIKE         DEFINE    ACOKSC        KKSC
036700941104     C     *LIKE         DEFINE    BLPLNP        KLNP
036800941104     C     *LIKE         DEFINE    BLPAAS        KAAS
036900941104     C     *LIKE         DEFINE    BLPMGS        KMGS
037000941104     C     *LIKE         DEFINE    BLPNRS        KNRS
037100941104     C     *LIKE         DEFINE    BLPNSP        KNSP
037200941104     C     *LIKE         DEFINE    GCPFRG        KFRG
037300941104     C     *LIKE         DEFINE    TBLCOD        KCOD
037400941104     C     *LIKE         DEFINE    TBLKEY        KKEY
037500070702     C     *LIKE         DEFINE    BLPAAS        K_AAS
037600070702     C     *LIKE         DEFINE    BLPLNP        K_LNP
037700070702     C     *LIKE         DEFINE    BLPNRS        K_NRS
037800070702     C     *LIKE         DEFINE    BLPNSP        K_NSP
037900931018     C*----------------------------------------------------*
038000931018     C*  Ricerco lunghezza sottostringa rag.sociale da ricercare
038100941104     C                   MOVE      '  '          D24RAG
038200941104     C     '  '          SCAN      D24RAG        LENGHT            2 0
038300931018     C                   SUB       1             LENGHT
038400140120     C     35            SUB       LENGHT        Xlen              2 0
038500931018     C*----------------------------------------------------*
038600941104     C*  Inizializzo varibili
038700941104     C                   Z-ADD     1             KKUT
038800941104     C                   Z-ADD     CLIITA        KKCC
038900941104     C     D24ORD        COMP      'P'                                020201
039000941104     C*----------------------------------------------------*
039100941104     C*  Carico codici bolla
039200941104     C                   Z-ADD     0             XX                3 0
039300941104     C                   MOVE      '3A'          KCOD
039400941104     C                   OPEN      TABEL00F
039500941104     C     KTAB2         CHAIN     TABEL00F                           31
039600941104     C     *IN31         DOWEQ     '0'
039700941104     C     XX            ANDLT     100
039800941104     C     TBLFLG        IFEQ      ' '
039900941104     C                   MOVEL     TBLUNI        DS3A
040000941104     C                   ADD       1             XX
040100941104     C                   MOVEL     TBLKEY        C3A(XX)
040200941104     C                   MOVEL     §3AFCA        FCA(XX)
040300941104     C                   END
040400941104     C     KTAB2         READE     TABEL00F                               31
040500941104     C                   END
040600941104     C*----------------------------------------------------*
040700941104     C*  Controllo se devo caricare in schiera le filiali
040800941104     C*  gestite sull'AS
040900941104     C     D24LIN        IFEQ      *ZEROS
041000020731     c                   movel(p)  kpjbu         savpjbu
041100140120      *********
041200140120      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
041300140120     c                   clear                   TRUL31DS
041400140120     c                   eval      I31abi = 'AC'
041500140120     c                   eval      I31cpo = simfel
041600140120     c                   call      'TRUL31R'
041700140120     c                   parm                    KPJBA
041800140120     c                   parm                    TRUL31DS
041900140120     c                   if        o31pog > *zeros
042000140120     c                   movea     o31pog        $pog
042100140120     c                   endif
042200140120      *
042300140120     C                   DO        250           Xg                3 0
042400140120     c                   if           $pog(Xg) <> *blank
042500140120     c                   move      $pog(Xg)      fil(Xg)
042600140120     c                   else
042700140120     c                   move      '000'         fil(Xg)
042800140120     c                   endif
042900140120     c                   endDO
043000140120      ***
043100020731     c                   movel(p)  savpjbu       kpjbu
043200020506     C*                  MOVEL     '£3'          KCOD
043300020506     C*                  MOVEL     SIMFEL        KKEY
043400020506     C*    KTAB1         CHAIN     TABEL00F                           31
043500020506     C*    *IN21         IFEQ      '0'
043600020506     C*    TBLFLG        ANDEQ     ' '
043700020506     C*                  MOVEA     TBLUNI        FIL1
043800020506     C*                  Z-ADD     0             Y                 2 0
043900140120     C*                  DO        30            X
044000020506     C*    FIL1(X)       IFGT      *ZEROS
044100020506     C*                  ADD       1             Y
044200020506     C*                  MOVE      FIL1(X)       FIL(Y)
044300020506     C*                  END
044400020506     C*                  END
044500020506     C*                  ELSE
044600020506     C*                  MOVE      SIMFEL        FIL(1)
044700020506     C*                  Z-ADD     1             Y
044800020506     C*                  END
044900941104     C*
045000020906     C                   ELSE
045100020906     C                   MOVE      D24LIN        FIL(1)
045200020506     C*                  Z-ADD     1             Y
045300941104     C                   END
045400941104     C*
045500941104     C                   CLOSE     TABEL00F
045600931018     C*----------------------------------------------------*
045700931018     C                   ENDSR
