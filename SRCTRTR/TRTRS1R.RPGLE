000100931020     H DECEDIT('0,') DATEDIT(*YMD.)
000200931020     F* TRTRS1R  *---------------------------------------------------*
000300931020     F*----------*                                                   *
000400931020     F* - TRASMISSIONE PAGAMENTO CONTRASSEGNI DA SEDE A FILIALE      *
000500931020     F*--------------------------------------------------------------*
000600931213     F* - I N D I C A T O R I                                        *
000700931213     F*--------------------------------------------------------------*
000800931213     F* 20 - INDICATORE DI ERRORE SU QCMDEXE                         *
000900931213     F* 21 - CHAIN SU BLCSA00L/CNACO00F: RECUPERO RAG.SOC. MITTENTE  *
001000931213     F* 30 - CHAIN SU BLCSB03L: RECUOERO BOLLA MAMMA                 *
001100931213     F* 40 - FILIALE IN GESTIONE NELLA MECCANIZZAZIONE ARRIVI        *
001200931213     F* 52 - READ DI TABEL PER CARICAMENTO DATI '£3' IN SCHIERA      *
001300931213     F* 60 - CHAIN SU FLCSB00T X CONTROLLO SE RECORD GIA' ESISTENTE  *
001400931213     F* 70 - CHAIN SU FLLBL01L X CONTROLLO SE ESISTE MAMMA           *
001500931213     F* 80 - LOOP PRINCIPALE X LETTURA BLCSB08L                      *
001600931213     F* 81 - CHAIN SU BLTAS00L/BLTAS10L: REPERIMENTO RIF.ALFAB.      *
001700931213     F*--------------------------------------------------------------*
001800941031     FTNCSB08L  UF   E           K DISK
001900931020     F*-------------
002000941031     FTNCSB03L  IF   E           K DISK
002100941031     F                                     RENAME(TNCSB000:TNCSB003)
002200941031     F*-------------
002300941031     FFNCSB00T  UF A E           K DISK    USROPN
002400931020     F*-------------
002500941031     FFNLBL01L  IF   E           K DISK
002600931020     F*-------------
002700941031     FTNCSA01L  IF   E           K DISK
002800931020     F*-------------
002900931020     FTABEL00F  IF   E           K DISK
003000020909     F                                     USROPN
003100020912
003200020912     D Partenza        C                   'P'
003300020912
003400931020     D*--------------------------------------------------------------*
003500931020     D*  SCHIERE                                                     *
003600931020     D*--------------------------------------------------------------*
003700931020     D CSB             S              7  0 DIM(999)                             C/A  X FILIALE
003800931020     D*----------
003900020905     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)              ADDPFM FNSCB00T
004000020905     D CMD2            S             49    DIM(1) CTDATA PERRCD(1)              OVRDBF FNCSB00T MBR.
004100020905     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)              CHKOBJ FNCSB00T
004200931020     D*----------
004300960919     D*----------
004400961023     D STA             S              1    DIM(100)                             STATO C/A  X FIL.
004500931020     D*--------------------------------------------------------------*
004600931020     D*  DS                                                          *
004700931020     D*--------------------------------------------------------------*
004800941031     D WLBDA8          DS
004900941031     D  G02DAT                 1      8  0
005000941031     D  G02INV                 9     16  0
005100941031     D  G02ERR                17     17
005200941031     D  G02TGI                18     22  0
005300931020     D DSCMZ         E DS
005400931020     D KPJBA         E DS
005500960919     D  LIBSYS                92    101
005600971203     D FNLV55        E DS                  EXTNAME(FNLV55DS)
005700020905     D TIBS55        E DS                  EXTNAME(TIBS55DS)
005800980216     D TIBS69        E DS                  EXTNAME(TIBS69DS)
005900980216     D CNACO         E DS                  EXTNAME(CNACO00F)
006000980216     D CNIND         E DS                  EXTNAME(CNIND00F)
006100980216     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006200980216     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006300971128     D DS4S          E DS
006400971128     D CNCR80        E DS
006500020911
006600020912     D Trul26ds      E DS
006700020912     D                                     INZ
006800020912     D FIL                            3  0                                      FIL.MECCAN
006900020912     D                                     DIM(250)
007000020912     D                                     OVERLAY(O26Lin)
007100020912     D TFP                            3  0                                      FIL.MECCAN
007200020912     D                                     DIM(250)
007300020912     D                                     OVERLAY(O26Tfe)
007400020912
007500020911     D Tibs56ds      E DS
007600020911     D                                     INZ
007700020912     D  Ski                           3  0
007800020911     D                                     DIM(250)
007900020911     D                                     OVERLAY(O56Ski)
008000020911
008100971128     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008200931020     D  TCU                  398    697
008300931020     D                                     DIM(50)                              TAB FLAG CNT
008400931020     D  KCU                  698    847P 0
008500931020     D                                     DIM(50)                              TAB CAPO CONTO
008600931020     D                                     PACKEVEN
008700931020?    D*-
008800931020     D TCUDS           DS
008900931020     D  F3                     3      3
009000931020     D  F4                     4      4
009100931020     D  F56                    5      6
009200931020     D  F34                    3      4
009300941031?    D*-- COMMAND --*
009400941031     D                 DS
009500941031     D  WFILE                 13     20
009600020905     D  WMBR                  43     48
009700020905     D  WCMD                   1     49
009800020906     D B               S              5I 0
009900020912     D X               S              5I 0
010000020906     D Y               S              5I 0
010100020906
010200941031     I*--------------------------------------------------------------*
010300941031     I*  RIFENOMINO CAMPI TNCSB03L                                   *
010400941031     I*--------------------------------------------------------------*
010500941031     ITNCSB003
010600941031     I              CSBCAS                      MCAS
010700941031     I              CSBVCA                      MVCA
010800931111     I              CSBNRA                      MNRA
010900931111     I              CSBDTE                      MDTE
011000931111     I              CSBABI                      MABI
011100931111     I              CSBCAB                      MCAB
011200931111     I              CSBTPA                      MTPA
011300931111     I              CSBTPI                      MTPI
011400931111     I              CSBDDC                      MDDC
011500931111     I              CSBNDT                      MNDT
011600931111     I              CSBCDI                      MCDI
011700931111     I              CSBFPC                      MFPC
011800931111     I              CSBBNA                      MBNA
011900931111     I              CSBNDP                      MNDP
012000931111     I              CSBDDP                      MDDP
012100931111     I              CSBFTR                      MFTR
012200931111     I              CSBDTR                      MDTR
012300961023     I              CSBSTA                      MSTA
012400931020     C*---------------------------------------------------------------*
012500931020     C*   CICLO PRINCIPALE                                            *
012600931020     C*---------------------------------------------------------------*
012700931020     C*  Caricamento dati in tabella
012800931020     C                   EXSR      CARTAB
012900971203     C*  Caricamento in schiera filiali a cui spedire
013000971203     C                   EXSR      CARORG
013100931020     C*  Prima lettura
013200941031     C     *LOVAL        SETLL     TNCSB08L
013300941031     C                   READ      TNCSB08L                               80
013400931020     C     *IN80         IFEQ      '0'
013500931020     C*  Operazioni inizio filiale
013600931021     C                   MOVE      CSBLNP        LIN
013700931020     C                   EXSR      NEWFIL                                       Nuova filiale
013800931020     C                   END
013900931020     C*
014000931020     C     *IN80         DOWEQ     '0'
014100960918     C*  Dettaglio: considero contrassegno se pagato o se
014200960918     C*             trasmessa variazione stato
014300960919     C                   MOVEL     'N'           WTRASM            1
014400960919     C     CSBDDP        IFEQ      0                                            Stato da trasme
014500961023     C                   MOVEL     CSBSTA        WSTA              1
014600961023     C     WSTA          LOOKUP    STA                                    32
014700960919     C   32              MOVEL     'S'           WTRASM
014800960919     C                   ELSE
014900960919     C                   MOVEL     'S'           WTRASM                         C/A Pagato
015000960919     C                   END
015100960919     C     WTRASM        IFEQ      'S'
015200931020     C                   EXSR      RDYCA
015300960919     C                   END
015400931020     C*  Nuova lettura
015500941031     C                   READ      TNCSB08L                               80
015600931020     C*  Operazioni inizio filiale
015700931020     C     SAVLNP        IFNE      CSBLNP
015800941031     C                   CLOSE     FNCSB00T
015900931021     C                   MOVE      CSBLNP        LIN
016000931020     C                   EXSR      NEWFIL                                       Nuova filiale
016100931020     C                   END
016200931020     C*---------------------------------------------------*
016300931020     C                   END
016400931020     C*---------------------------------------------------*
016500941031     C*  Fine programma: ultima chiusura
016600941031     C                   CLOSE     FNCSB00T
016700941031     C*  Fine programma: sottomissione trasmissioni
016800010412     C                   EXSR      SBMTRA                                                membro
016900931020     C*
017000931020     C     FINE          TAG
017100971128     C* Imposto i dati in BS50
017200020905     C                   CLEAR                   TIBS55
017300971128     C                   MOVEL     'C'           I50TLA
017400020906     C                   EXSR      CalTIBS55R
017500980216     C*
017600980216     C                   CLEAR                   TIBS69
017700980216     C                   MOVEL     'C'           I69TLA
017800980216     C                   CALL      'TIBS69R'
017900980216     C                   PARM                    TIBS69
018000980216     C                   PARM                    CNACO
018100980216     C                   PARM                    CNIND
018200980216     C                   PARM                    CNCLP
018300980216     C                   PARM                    FNCLS
018400971128     C*
018500931020     C                   SETON                                        LR
018600931020     C*---------------------------------------------------------------*
018700931020     C*   Newfil: nuova filiale  gestione apertura nuovo membro       *
018800931020     C*---------------------------------------------------------------*
018900931020     C     NEWFIL        BEGSR
019000931020     C*
019100020912     C* Determino termial del PO.
019200020912     C                   Z-ADD     1             X
019300020906     C                   Z-ADD     1             Y
019400020912     C     LIN           LOOKUP    FIL(X)                                 40
019500020912     C   40TFP(X)        LOOKUP    SKI(Y)                                 40
019600931020     C*----------------------------------------------------*
019700931111     C     *IN40         IFEQ      '1'
019800960918     C*  Chkobj/Addlfm/Ovrdbf/Open FNCSB00T
019900941031     C                   MOVEL     CMD3(1)       WCMD
020000020905     C                   Z-ADD     49            LUNG             15 5
020100020912     C                   MOVE      SKI(Y)        WMBR
020200020912     C                   MOVEL     SimFEl        WMBR
020300020905     C                   MOVEL     WMBR          WMBRSV            6
020400960918     C* Imposto nome file
020500941031     C                   MOVEL     'FNCSB00T'    WFILE
020600941031     C                   MOVEL     'FNCSB00T'    WFILE1            8
020700931020     C** Controllo se esiste il membro
020800931020     C                   SETOFF                                       20
020900931020     C                   MOVEL     *BLANKS       COMMAN
021000941031     C                   MOVEL     WCMD          COMMAN           80
021100931020     C                   CALL      'QCMDEXC'                            20
021200931020     C                   PARM                    COMMAN
021300931020     C                   PARM                    LUNG
021400931020     C** Esegue addpfm del membro nuovo
021500931020     C   20              DO
021600941031     C                   MOVEL     CMD1(1)       WCMD
021700941031     C                   MOVE      WMBRSV        WMBR
021800941031     C                   MOVE      WFILE1        WFILE
021900941031     C                   MOVEL     *BLANKS       COMMAN
022000941031     C                   MOVEL     WCMD          COMMAN
022100931020     C                   CALL      'QCMDEXC'
022200931020     C                   PARM                    COMMAN
022300931020     C                   PARM                    LUNG
022400931020     C                   END
022500931020     C** Esegue in ogni caso OVRDBF del membro desiderato
022600941031     C                   MOVEL     CMD2(1)       WCMD
022700941031     C                   MOVE      WMBRSV        WMBR
022800941031     C                   MOVE      WFILE1        WFILE
022900931020     C                   MOVEL     *BLANKS       COMMAN
023000941031     C                   MOVEL     WCMD          COMMAN
023100931020     C                   CALL      'QCMDEXC'
023200931020     C                   PARM                    COMMAN
023300931020     C                   PARM                    LUNG
023400941031     C*  Apro file
023500941031     C                   OPEN      FNCSB00T
023600931111     C                   END
023700931020     C*----------------------------------------------------*
023800931020     C*  Inizio filiale
023900931022     C                   MOVE      LIN           SAVLNP
024000931020     C*
024100931020     C                   ENDSR
024200931020     C*---------------------------------------------------------------*
024300931020     C*   RDYCA: Esamino contrassegno                                 *
024400931020     C*---------------------------------------------------------------*
024500931020     C     RDYCA         BEGSR
024600931111     C*
024700931020     C*  Preimposto i campi identificativi della bolla di sped.orig.
024800931020     C*  con quella dell'ultima bolla letta.
024900931020     C                   MOVE      CSBLNP        PLP
025000931020     C                   MOVE      CSBAAS        PAA
025100931020     C                   MOVE      CSBNRS        PNS
025200931020     C                   MOVE      CSBNSP        PSP
025300931020     C*  Controllo se bolla con mamma
025400931020     C                   MOVE      CSBLNP        WBLLPN
025500931020     C                   MOVE      CSBAAS        WBLAAN
025600931020     C                   MOVE      CSBNRS        WBLNRN
025700931020     C                   MOVE      CSBNSP        WBLNSN
025800931020     C                   MOVE      'N'           TRMAM             1
025900010412     C                   CLEAR                   LblLpp
026000941031     C     KEYSPE        CHAIN     FNLBL01L                           70
026100931020     C*----------------------------------------------------*
026200931020     C     *IN70         IFEQ      '0'
026300931020     C*----------------------------------------------------*
026400931020     C     *IN70         DOWEQ     '0'
026500931020     C                   MOVE      LBLLPP        WBLLPN
026600931020     C                   MOVE      LBLAAP        WBLAAN
026700931020     C                   MOVE      LBLNRP        WBLNRN
026800931020     C                   MOVE      LBLNSP        WBLNSN
026900941031     C     KEYSPE        CHAIN     FNLBL01L                           70
027000931020     C                   END
027100931020     C*
027200931020     C                   MOVE      'S'           TRMAM             1
027300931020     C*  Se trovata reimposto i campi della bolla di partenza
027400931020     C*----------------------------------------------------*
027500931020     C                   END                                                    *IN70 = '0'
027600931020     C*----------------------------------------------------*
027700931020     C*  Scrittura record x ultima bolla
027800931020     C                   EXSR      WRTCA
027900931022     C* Flaggp reord BLCSB:
028000931022     C                   MOVE      'T'           CSBFTR
028100941031     C                   MOVE      OGGI          CSBDTR
028200931022     C                   EXCEPT    TRASM
028300020906
028400931021     C*----------------------------------------------------*
028500931021     C*  Controllo se cliente intestazione bolla non è della
028600931021     C*  filiale di partenza
028700020906     C*  Eseguo l'invio al PO del cliente solo se appartiene
028800020906     C*  ad un AS/400 e sistema informativo diverso da quello
028900020906     C*  della linea di partenza.
029000931021     C                   MOVEL     CSBCDI        APP
029100931021     C     APP           IFNE      CSBLNP
029200931111     C     APP           ANDNE     LBLLPP                                       fil.cli. = fil.mamma
029300931111     C     SERV          ANDNE     *ALL'8'                                      Cliente no
029400931111     C     SERV          ANDNE     *ALL'9'                                      generico
029500020906
029600020906     C                   CLEAR                   TIBS55
029700020906     C                   EVAL      I50APO = App                                  DESTINAT.
029800040402     C                   MOVEL     CSBLNP        I50PPO
029900040402     C                   CALL      'TIBS55R'
030000040402     C                   PARM                    TIBS55
030100020906
030200020906     ** Invio il record anche al PO del cliente solo se risiede
030300020906     ** in un AS/400 e sistema informativo diverso.
030400040402     C                   IF        O50Sas = *BLANK
030500941031     C                   CLOSE     FNCSB00T
030600931021     C                   MOVE      APP           LIN
030700931021     C                   EXSR      NEWFIL                                       Nuova filiale
030800931021     C                   EXSR      WRTCA
030900020906     C                   ENDIF
031000020906
031100931021     C                   END
031200020906
031300931020     C*----------------------------------------------------*
031400931021     C*  Scrittura record x bolla mamma solo se trovato record
031500931111     C     TRMAM         IFEQ      'S'
031600931111     C                   EXSR      WRTMAM
031700931111     C                   END
031800931111     C*
031900931020     C                   ENDSR
032000931020     C*---------------------------------------------------------------*
032100931020     C*   WRTCA: Scrittura dati contrassegni in FLCSB00T              *
032200931020     C*---------------------------------------------------------------*
032300931020     C     WRTCA         BEGSR
032400931020     C*
032500931111     C* Procedo alla scrittura del nuovo record su flcsb solo se
032600931111     C* la filiale ad esso legata è gestita nelle trasmissioni
032700931111     C     *IN40         IFEQ      '1'
032800960918     C* Lettura FNCSB00T
032900941102     C                   SETON                                        60
033000941031     C     KCSB3         CHAIN     FNCSB00T                           60
033100941031     C*
033200931111     C     *IN60         IFEQ      '1'
033300941031     C* Imposto campi bolla di partenza iniziale
033400941031     C                   MOVE      PLP           CSBPLP
033500941031     C                   MOVE      PAA           CSBPAA
033600941031     C                   MOVE      PNS           CSBPNS
033700941031     C                   MOVE      PSP           CSBPSP
033800941031     C* Ragione sociale mittente
033900941031     C                   MOVE      CSBCDI        SERV              4 0
034000941031     C                   MOVE      *BLANKS       CSBRSM
034100941031     C     SERV          IFEQ      *ALL'8'
034200941031     C     SERV          OREQ      *ALL'9'
034300941031     C     KCSA          CHAIN     TNCSA000                           21
034400941031     C  N21              MOVEL     CSARSD        CSBRSM
034500941031     C                   ELSE
034600980216      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
034700980216     C                   CLEAR                   TIBS69
034800980216     C                   CLEAR                   CNACO
034900980216     C                   CLEAR                   CNIND
035000980216     C                   CLEAR                   CNCLP
035100980216     C                   CLEAR                   FNCLS
035200980216     C                   MOVEL     ' '           I69TLA
035300980216     C                   MOVEL     KNSIF         I69SIF
035400980216     C                   Z-ADD     CSBCDI        I69KAC
035500980216     C                   CALL      'TIBS69R'
035600980216     C                   PARM                    TIBS69
035700980216     C                   PARM                    CNACO
035800980216     C                   PARM                    CNIND
035900980216     C                   PARM                    CNCLP
036000980216     C                   PARM                    FNCLS
036100980216     C     O69ERR        IFEQ      *BLANK
036200980216     C                   MOVEL     ACORAG        CSBRSM
036300980216     C                   END
036400941031     C                   END
036500941031     C*
036600941031     C* Reperisco Abi/Cab banca di pagamento
036700941031     C     CSBBNA        IFNE      9999999
036800941031     C                   MOVE      CSBBNA        INDWSC
036900941031     C                   MOVE      *ZERO         INDABI
037000941031     C                   MOVE      *ZERO         INDCAB
037100980216     C                   CLEAR                   TIBS69
037200980216     C                   CLEAR                   CNACO
037300980216     C                   CLEAR                   CNIND
037400980216     C                   CLEAR                   CNCLP
037500980216     C                   CLEAR                   FNCLS
037600980216     C                   MOVEL     ' '           I69TLA
037700980216     C                   MOVEL     KNSIF         I69SIF
037800980216     C                   Z-ADD     CCNBAN        I69KCC
037900980216     C                   Z-ADD     CSBBNA        I69KIN
038000980216     C                   MOVEL     *BLANKS       ACORAG
038100980216     C                   CALL      'TIBS69R'
038200980216     C                   PARM                    TIBS69
038300980216     C                   PARM                    CNACO
038400980216     C                   PARM                    CNIND
038500980216     C                   PARM                    CNCLP
038600980216     C                   PARM                    FNCLS
038700980216     C     O69ERR        IFEQ      *BLANK
038800980216     C                   MOVE      INDABI        CSBPAB
038900980216     C                   MOVE      INDCAB        CSBPCA
039000980216     C                   END
039100941031     C                   ELSE
039200941031     C                   MOVE      99999         CSBPAB
039300941031     C                   MOVE      99999         CSBPCA
039400941031     C                   END
039500941031     C                   WRITE     FNCSB000
039600971203     C                   ADD       1             CSB(Y)
039700931111     C                   END
039800931020     C*
039900931111     C                   END
040000931111     C*
040100931020     C                   ENDSR
040200931020     C*---------------------------------------------------------------*
040300931020     C*   Sbmtra: sottometto transazioni                              *
040400931020     C*---------------------------------------------------------------*
040500931020     C     SBMTRA        BEGSR
040600931020     C*
040700941102     C                   MOVEL     'FNCSB00T'    WFILDA           10
040800941102     C                   MOVEL     'FNCSB00R'    WFILA            10
040900941102     C                   MOVEL     'TRS3'        WCOAZ             4
041000941102     C*
041100931020     C                   DO        999           B
041200971203     C     CSB(B)        IFGT      0
041300971128     C                   MOVEL     WFILDA        §FILDA           10
041400941102     C                   MOVEL     WFILA         §FILA            10
041500941102     C                   MOVEL     WCOAZ         §COAZ             4
041600020912     C* Determino terminal a cui spedire.
041700020912     C                   MOVE      Ski(B)        §Ski              3             DESTINAT.
041800020912     C                   MOVE      Ski(B)        §MBR              6             MEMBRO
041900020912     C                   MOVEL     SimFEl        §MBR
042000971128     C                   MOVE      'S'           §CLR              1             CLEAR
042100971128     C* Recupero librerie e S.Informativi a cui inviare
042200971128     C                   EXSR      RICLIB
042300971128     C                   EXSR      X25T
042400931022     C                   END
042500971203     C                   END
042600931020     C*
042700931020     C                   ENDSR
042800971128     C*---------------------------------------------------------------*
042900971128     C* Riclib: Richiamo pgm per recupero lib. e sif. da/a cui spedire*
043000971128     C*---------------------------------------------------------------*
043100971128     C     RICLIB        BEGSR
043200971128     C*
043300020905     C                   CLEAR                   TIBS55
043400020912     C                   MOVE      Ski(B)        I50APO                          DESTINAT.
043500020906     C                   EXSR      CalTIBS55R
043600971128     C*
043700971128     C                   ENDSR
043800931111     C*---------------------------------------------------------------*
043900931111     C*   Wrtmam: Scrittura dati bolla mamma                          *
044000931111     C*---------------------------------------------------------------*
044100931111     C     WRTMAM        BEGSR
044200931111     C*  Reperisco i dati della bolla  mamma
044300931111     C                   MOVE      LBLLPP        CSBLNP
044400931111     C                   MOVE      LBLAAP        CSBAAS
044500931111     C                   MOVE      LBLNRP        CSBNRS
044600931111     C                   MOVE      LBLNSP        CSBNSP
044700941031     C     KCSB3         CHAIN     TNCSB03L                           30
044800931111     C     *IN30         IFEQ      '0'
044900931111     C*  Controllo che bolla non sia già stata spedita
045000931111     C*  Operazioni inizio filiale
045100931111     C     SAVLNP        IFNE      LBLLPP
045200020906
045300020906     C                   CLEAR                   TIBS55
045400020906     C                   EVAL      I50APO = LblLpp                               DESTINAT.
045500040402     C                   MOVEL     SAVLNP        I50PPO
045600040402     C                   CALL      'TIBS55R'
045700040402     C                   PARM                    TIBS55
045800020906
045900020906     ** Invio il record anche alla linea di partenza madre solo se risiede
046000020906     ** in un AS/400 e sistema informativo diverso.
046100040402     C                   IF        O50Sas = *BLANK
046200941031     C                   CLOSE     FNCSB00T
046300931111     C                   MOVE      LBLLPP        LIN
046400931111     C                   EXSR      NEWFIL                                       Nuova filiale
046500020906     C                   ENDIF
046600020906
046700931111     C                   END
046800931111     C*
046900931111     C                   EXSR      WRTCA
047000931111     C                   END                                                    No MAMMA
047100931111     C*
047200931111     C                   ENDSR
047300931020     C*---------------------------------------------------------------*
047400931020     C*   X25T: Sottomissione della singola trasmissione              *
047500931020     C*---------------------------------------------------------------*
047600931020     C     X25T          BEGSR
047700931020     C*
047800971128     C                   CLEAR                   W236
047900020912     C                   MOVEL     §Ski          CMZDST                          DESTINAT.
048000941102     C                   MOVEL     §FILDA        CMZFLD                          DA: >>>
048100020905     C                   MOVEL     'M      '     VAR               7
048200931020     C                   MOVE      §MBR          VAR
048300931020     C                   MOVEL     *BLANK        CMZMBD
048400931020     C                   MOVEL     VAR           CMZMBD
048500971223     C                   MOVEL     O50PLG        CMZLBD
048600941102     C                   MOVEL     §FILA         CMZFLA                          DA: >>>
048700931020     C                   MOVEL     *BLANK        CMZMBA
048800931020     C                   MOVEL     VAR           CMZMBA
048900971128     C                   MOVEL     O50ALA        CMZLBA
049000931020     C                   MOVE      '0000'        CMZORI                          ORA
049100931020     C                   MOVE      '0000000'     CMZDTI                          DATA
049200931020     C                   MOVE      'T'           CMZURG                          NOTTE
049300931020     C                   MOVE      'A'           CMZADD                          IN ADD
049400931020     C                   MOVE      §CLR          CMZCLR                          CLEAR
049500931020     C                   MOVE      'S'           CMZFTR                          DA TRANS.
049600931020     C                   MOVE      *BLANK        CMZERR                          1= ERR.
049700941102     C                   MOVE      §COAZ         CMZAZN                          AZIONE
049800971128     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
049900971203     C                   MOVE      CSB(B)        CMZNRR                          N.REC.
050000931020     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
050100020905     C                   CALL      'TIBS53C'
050200971128     C                   PARM                    O50LCL
050300971128     C                   PARM                    CMZDST
050400931020     C                   PARM                    CMZFLD
050500931020     C                   PARM                    CMZMBD
050600931020     C                   PARM                    CMZLBD
050700931020     C                   PARM                    CMZFLA
050800931020     C                   PARM                    CMZMBA
050900931020     C                   PARM                    CMZLBA
051000931020     C                   PARM                    CMZORI
051100931020     C                   PARM                    CMZDTI
051200931020     C                   PARM                    CMZURG
051300931020     C                   PARM                    CMZADD
051400931020     C                   PARM                    CMZCLR
051500931020     C                   PARM                    CMZFTR
051600931020     C                   PARM                    CMZERR
051700931020     C                   PARM                    CMZAZN
051800931020     C                   PARM                    CMZSYF
051900931020     C                   PARM                    CMZNRR
052000931020     C                   PARM                    CMZPRR
052100971128     C                   PARM                    W236            236
052200971128     C*
052300931020     C                   ENDSR
052400971203     C*---------------------------------------------------------------*
052500971203     C*   Carorg: caricamento dati da azorg                           *
052600971203     C*---------------------------------------------------------------*
052700971203     C     CARORG        BEGSR
052800020912     C*
052900020912     C                   CLEAR                   Trul26DS
053000020912     C                   EVAL      I26TpT = Partenza
053100020912     C                   CALL      'TRUL26R'
053200020912     C                   PARM                    Trul26DS
053300020911     C*
053400020912     C                   CLEAR                   Tibs56DS
053500020911     C                   EVAL      I56Ppo = SimFEl
053600020924     C                   EVAL      I56Mod = 'AZI'
053700020911     C                   CALL      'TIBS56R'
053800020912     C                   PARM                    Tibs56DS
053900971203     C*
054000971203     C                   ENDSR
054100931020     C*---------------------------------------------------------------*
054200020909     C* Caricamento tabelle.                                          *
054300931020     C*---------------------------------------------------------------*
054400931020     C     CARTAB        BEGSR
054500960919     C*
054600020909     C                   OPEN      Tabel00F
054700960919     C* Carico Stati C/Assegni da trasmettere  in filiale
054800961023     C                   MOVEA     *LOVAL        STA
054900960919     C                   MOVEL     '4S'          COD
055000020912     C                   Z-ADD     0             Y
055100960919     C     KTAB          CHAIN     TABEL                              52
055200960919     C*
055300960919     C     *IN52         DOWEQ     '0'
055400960919     C     TBLFLG        IFEQ      *BLANKS
055500960919     C                   MOVEL     TBLUNI        DS4S
055600960919     C     §4SFIL        IFEQ      'S'
055700020912     C                   ADD       1             Y
055800020912     C                   MOVE      TBLKEY        STA(Y)
055900960919     C                   END
056000960919     C                   END
056100960919     C*
056200960919     C     KTAB          READE     TABEL                                  52
056300960919     C                   END
056400020909     C*
056500020909     C                   CLOSE     Tabel00F
056600960919     C*
056700931020     C                   ENDSR
056800931020     C*---------------------------------------------------------------*
056900931020     C*   Defcam: operazioni iniziali                                 *
057000931020     C*---------------------------------------------------------------*
057100941031     C     *INZSR        BEGSR
057200931020     C*
057300931020     C     *ENTRY        PLIST
057400931020     C                   PARM                    KPJBA
057500931020     C*
057600931020     C                   Z-ADD     1             CODUT
057700931020     C                   CALL      'X§PARUT'
057800931020     C                   PARM                    UT§DSE
057900971128     C                   MOVEL     REC80         CNCR80
058000931020     C     CODUT         CABEQ     -1            FINE
058100931020     C*
058200931020     C                   DO        50            F                 2 0
058300931020     C                   MOVE      TCU(F)        TCUDS
058400931020     C     F34           IFEQ      '01'
058500931020     C     F56           ANDEQ     'CG'
058600931020     C                   Z-ADD     KCU(F)        CLIITA            4 0
058700931020     C                   END
058800931020     C     F34           IFEQ      '06'
058900931020     C     F56           ANDEQ     'CG'
059000931020     C                   Z-ADD     KCU(F)        CCNBAN            4 0
059100931020     C                   END
059200931020     C  N21              END
059300931020     C* BLCSA00F
059400931020     C     KCSA          KLIST
059500931020     C                   KFLD                    CSBAAS
059600931020     C                   KFLD                    CSBLNP
059700931020     C                   KFLD                    CSBNRS
059800931020     C                   KFLD                    CSBNSP
059900931020     C* TABEL00F
060000931020     C     KTAB          KLIST
060100931020     C                   KFLD                    CODUT
060200931020     C                   KFLD                    COD               2
060300931020     C*
060400931020     C* BLTAS00F
060500931020     C     KTAS          KLIST
060600931020     C                   KFLD                    CSBAAS
060700931020     C                   KFLD                    CSBLNP
060800931020     C                   KFLD                    CSBNRS
060900931020     C                   KFLD                    CSBNSP
061000931021     C                   KFLD                    CSBTBL
061100931020     C*
061200931111     C* BLCSB03L
061300931111     C     KCSB3         KLIST
061400941031     C                   KFLD                    CSBAAS
061500931111     C                   KFLD                    CSBLNP
061600931111     C                   KFLD                    CSBNRS
061700931111     C                   KFLD                    CSBNSP
061800931020     C*
061900931020     C* BLLBL01L
062000931020     C     KEYSPE        KLIST
062100931020     C                   KFLD                    WBLAAN
062200931020     C                   KFLD                    WBLLPN
062300931020     C                   KFLD                    WBLNRN
062400931020     C                   KFLD                    WBLNSN
062500931020     C*
062600931020     C                   Z-ADD     1             CODUT             1 0
062700931020     C*
062800931020     C*  Definizione Variabili
062900931020     C     *LIKE         DEFINE    LBLLPN        WBLLPN
063000931020     C     *LIKE         DEFINE    LBLAAN        WBLAAN
063100931020     C     *LIKE         DEFINE    LBLNRN        WBLNRN
063200931020     C     *LIKE         DEFINE    LBLNSN        WBLNSN
063300931020     C     *LIKE         DEFINE    INDKSC        INDWSC
063400931020     C     *LIKE         DEFINE    CSBLNP        SAVLNP
063500931021     C     *LIKE         DEFINE    CSBLNP        LIN
063600931021     C     *LIKE         DEFINE    CSBLNP        APP
063700931020     C     *LIKE         DEFINE    CSBPLP        PLP
063800931020     C     *LIKE         DEFINE    CSBPAA        PAA
063900931020     C     *LIKE         DEFINE    CSBPNS        PNS
064000931020     C     *LIKE         DEFINE    CSBPSP        PSP
064100931020     C*
064200941031     C                   TIME                    WHHDAT           14 0
064300941031     C                   MOVE      WHHDAT        G02DAT
064400941031     C                   Z-ADD     0             G02INV
064500941031     C                   MOVEL     ' '           G02ERR
064600941031     C                   CALL      'XSRDA8'
064700941031     C                   PARM                    WLBDA8
064800941031     C                   Z-ADD     G02INV        OGGI              8 0
064900941031     C*
065000931020     C                   ENDSR
065100020906
065200020906     ***********************************************************************
065300020906     **
065400020906     **
065500020906     **
065600020906     ***********************************************************************
065700020906     C     CalTIBS55R    BEGSR
065800020906
065900020912     C                   MOVE      SIMFEL        I50PPO
066000020912     C                   MOVE      KNSIF         I50PSI
066100020906     C                   CALL      'TIBS55R'
066200020906     C                   PARM                    TIBS55
066300020906
066400020906     C                   ENDSR
066500040402
066600020906
066700941031     OTNCSB000  E            TRASM
066800931020     O                       CSBFTR
066900931020     O                       CSBDTR
067000941031**         CMD1
067100020905ADDPFM FILE(        )                MBR(M000000)
067200931020**         CMD2
067300020905OVRDBF FILE(        )                MBR(M000000)
067400931020**         CMD3
067500020905CHKOBJ  OBJ(        ) OBJTYPE(*FILE) MBR(M000000)
