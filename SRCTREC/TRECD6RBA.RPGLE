000100940509      ****************************************************************
000200000000      *                                                              *
000300931102      *   PARTENDO DALLA DS ESTERNA PASSATA DAL PGM DI LETTURA       *
000400941128      *   DELLA DATAQ, IMPOSTA LA DS UNIFICATA IN BASE AL FILE       *
000500941128      *   MODIFICATO.                                                *
000600930204      *                                                              *
000700930204      ****************************************************************
000800930413     H DECEDIT('0,') DATEDIT(*DMY.)
000900950201     FTABEL00F  UF   E           K DISK    USROPN
001000011106     FTNTBE01L  IF   E           K DISK    USROPN
001100941128     FFNLBL01L  IF   E           K DISK
001200990804     FTITAS30C  IF   E           K DISK
001300990806     FTITA730C  IF   E           K DISK
001400941129     FTNCSB03L  IF   E           K DISK
001500950324     FTNCSV01L  IF   E           K DISK
001600040617     FFIAR531C  IF   E           K DISK
001700050309     FTIGCP51L  IF   E           K DISK
001800950120     FECERR03L  UF A E           K DISK
001900930304     D*****************************************************************
002000950411     D* COMANDI
002100950411     D CMD1            S             45    DIM(1) CTDATA PERRCD(1)
002200971128     D CMD2            S             45    DIM(1) CTDATA PERRCD(1)
002300930304     D* MESSAGGI
002400930304     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500941128     D* SCHIERE VARIE E IMPORTI
002600990804     D SV7             S              1    DIM(20)
002700011213     D VA7             S             11    DIM(20)
002800941129     D SVN             S              1    DIM(20)
002900011213     D VAN             S             11    DIM(20)
003000930405     D*****************************************************************
003100931103     D* SCHIERA TABELLA TIPI BOLLA (TB)
003200931104     D TBL             S              2    DIM(50)
003300931104     D TPO             S              1    DIM(50)
003400940808     D RBL             S              1    DIM(50)
003500940808     D CSF             S              1    DIM(50)
003600940930     D CCS             S              1    DIM(50)
003700940808     D DBO             S              1    DIM(50)
003800941130     D*****************************************************************
003900941130     D* SCHIERA TABELLA FLAG SPEDIZIONI/RICAVI (2W)
004000941130     D FRB             S              1    DIM(10)
004100941130     D FSP             S              1    DIM(10)
004200941130     D FRI             S              1    DIM(10)
004300020528     D*****************************************************************
004400020528     D* FILE E RELATIVI FLAGS DI GESTIONE
004500020528     D FIL             S             10    DIM(20)
004600020528     D SDA             S              1    DIM(20)
004700020528     D SDC             S              1    DIM(20)
004800020528     D SDF             S              1    DIM(20)
004900020528     D SDO             S              1    DIM(20)
005000020528     D SDP             S              1    DIM(20)
005100020528     D SDR             S              1    DIM(20)
005200020528     D SDT             S              1    DIM(20)
005300040316     D SDE             S              1    DIM(20)
005400020528     D CET             S              1    DIM(20)
005500020528     D MOV             S              1    DIM(20)
005600020531     D FIM             S              1    DIM(20)
005700020531     D FCE             S              1    DIM(20)
005800030108     D COP             S              1    DIM(20)
005900930323     D*****************************************************************
006000930323     D* DS ESTERNA IN INPUT AL PGM
006100940808     D DS$ECO        E DS
006200971229     D DS£ECO        E DS
006300940405     D*****************************************************************
006400990804     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI TITAS10F + BOLLA ORIG.
006500941130     D TRECDS        E DS
006600940405     D  D$PDS1                 1    250
006700940405     D  D$PDS2               251    500
006800940405     D  D$DDS1               501    750
006900940405     D  D$DDS2               751   1000
007000941128     D  D$ODS1              1001   1250
007100941128     D  D$ODS2              1251   1500
007200941129     D*****************************************************************
007300941129     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI TNCSB00F
007400941129     D DS$CSB        E DS
007500941129     D  D$PCS1                 1    250
007600941129     D  D$PCS2               251    500
007700941129     D  D$DCS1               501    750
007800941129     D  D$DCS2               751   1000
007900030108     D*****************************************************************
008000030108     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI FIAR500F -TRANSITI-
008100030108     D DS$AR5T       E DS
008200030108     D  D$PAT1                 1    250
008300030108     D  D$DAT1               251    500
008400030108     D*****************************************************************
008500030108     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI FIAR500F -BANCALI-
008600030108     D DS$AR5B       E DS
008700030108     D  D$PAB1                 1    250
008800030108     D  D$DAB1               251    500
008900030108     D*****************************************************************
009000011106     D* DS  CONTROLLO DATA (8)
009100011106     D WLBDA8          DS                  INZ
009200011106     D  G08DAT                 1      8  0
009300011106     D  G08INV                 9     16  0
009400011106     D  G08ERR                17     17
009500011106     D  G08TGI                18     22  0
009600930405     D*****************************************************************
009700931103     D* DS ESTERNA TABELLA TB (TIPI BOLLE)
009800931105     D DSTB          E DS
009900940103     D*****************************************************************
010000020528     D* DS ESTERNA TABELLA 2V
010100020528     D DS2V          E DS
010200941130     D*****************************************************************
010300941130     D* DS ESTERNA TABELLA 2W (FLAG SPEDIZ/RICAVI)
010400941130     D DS2W          E DS
010500950517     D*****************************************************************
010600990804     D* DS DATE AVANZAMENTO TRASFERIMENTI TITAS00 > 10 > P
010700950517     D DS$3          E DS
010800030113     D*****************************************************************
010900030113     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD TRANSITI
011000030113     D DAR5TRS       E DS
011100030113     D*****************************************************************
011200030113     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI
011300030113     D DAR5BAN       E DS
011400030801     D*****************************************************************
011500030801     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI BARTOLINI
011600030801     D DAR5BNB       E DS
011700950404     D*****************************************************************
011800011106     D* CONVERSIONI IMPORTI
011900011106     D YEUR          E DS                  EXTNAME(YEURCODS)
012000011106     D*****************************************************************
012100011106     D* DS DIVISE PER CONVERSIONI PER IL C/E
012200011106     D DSGEDCE       E DS
012300980130     D*****************************************************************
012400980130     D* DATA AREA PER DATI VARI SALDI
012500090306     D TREC00        E DS
012600930325     D***************************************************************
012700930325     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
012800941026     D DS§DAT          DS
012900941026     D  DSSEC                  1      2  0
013000941026     D  DSANN                  3      4  0
013100941026     D  DSMES                  5      6  0
013200941026     D  DSGIO                  7      8  0
013300941129     D  DSAAS                  1      4  0
013400941129     D  DSMGS                  5      8  0
013500941026     D  DSDAT6                 3      8  0
013600941026     D  DSDAT8                 1      8  0
013700941026     D DS§TIM          DS
013800941026     D  DTORA                  1      2  0
013900941026     D  DTMIN                  3      4  0
014000941026     D  DTSCN                  5      6  0
014100941026     D  DTGIO                  7      8  0
014200941026     D  DTMES                  9     10  0
014300941026     D  DTSEC                 11     12  0
014400941026     D  DTANN                 13     14  0
014500941026     D  DTORA6                 1      6  0
014600941026     D  DTTIME                 1     14  0
014700941026     D***************************************************************
014800941026     D* DS PER SCOMPOSIZIONE MESSAGGIO
014900941026     D                 DS
015000941026     D  DSMSG                  1     80
015100941026     D  DSDER                  1     74
015200941026     D  DSCER                 75     78
015300140130     D* DS PER SCOMPOSIZIONE CAMPO D$GXXX
015400140130     D*-------------------
015500140130     D* DS dsd$g informazioni x simulazione
015600140130     D*-------------------
015700140227     D Dsd$g         E DS                  prefix(£)
015800950124     I*****************************************************************
015900990804     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS30C
016000990804     ITITAS000      01
016100990804     ITITAS010      02
016200990804     ITITASP00      03
016300930304     C*****************************************************************
016400930323     C*/ INIZIO MAIN DEL PGM
016500931105     C*
016600931105     C* ESEGUE ROUTINE INIZIALE
016700931105     C                   EXSR      RUTINZ
016800940919     C*
016900991214     C* SE NEL CAMPO STATUS TROVO 'C' (CLOSE) OPPURE 'S' (SHUTDOWN)
017000991214     C* CHIUDO I FILE E CHIUDO PGM CON LR
017100991214     C     D$GSTS        IFEQ      'C'
017200991214     C     D$GSTS        OREQ      'S'
017300140227     c     £D$GFSI       andne     'S'
017400991214     C     $ERR          ORNE      'N'
017500991214     C                   ELSE
017600940808     C*
017700940808     C                   EXSR      AZZW
017800941129     C*
017900941129     C* GESTIONE DEI FLAGS PRIMA/DOPO
018000941129     C     $ERR          IFEQ      'N'
018100941129     C                   EXSR      GESFLA
018200941129     C                   ENDIF
018300941129     C*
018400941129     C* GESTIONE VARIAZIONE RECORD DI TNCSB00F (CONTRASSEGNI)
018500941129     C     $ERR          IFEQ      'N'
018600941129     C     D$GFIL        IFEQ      'TNCSB00F'
018700941129     C                   EXSR      GESCSB
018800941129     C                   ENDIF
018900941129     C                   ENDIF
019000030108     C*
019100030108     C* GESTIONE VARIAZIONE RECORD DI FIAR500F (TRANSITI o BANCALI)
019200030108     C     $ERR          IFEQ      'N'
019300030108     C     D$GFIL        IFEQ      'FIAR500F'
019400030108     C                   EXSR      GESAR5
019500030108     C                   ENDIF
019600030108     C                   ENDIF
019700941129     C*
019800990804     C* GESTIONE VARIAZIONE RECORD DI TITAS10F
019900941129     C     $ERR          IFEQ      'N'
020000990804     C     D$GFIL        IFEQ      'TITAS10F'
020100020527     C     D$GFIL        OREQ      'TITAS00F'
020200941129     C                   EXSR      GESTAS
020300941129     C                   ENDIF
020400941129     C                   ENDIF
020500941128     C*
020600941128     C* TRATTA IL TIPO BOLLA (E RELATIVI FLAGS) PRIMA E DOPO
020700941128     C     $ERR          IFEQ      'N'
020800941128     C                   EXSR      GESTBL
020900941128     C                   ENDIF
021000940808     C*
021100941128     C* CERCA LEGAMI ED EVENTUALMENTE MEMORIZZA IMMAGINE MAMMA
021200941021     C     $ERR          IFEQ      'N'
021300941003     C                   EXSR      GESLEG
021400940808     C                   ENDIF
021500940808     C*
021600990804     C* CARICA LE VARIE IN SCHIERA ED EVENTUALMENTE LEGGE TITA7
021700941021     C     $ERR          IFEQ      'N'
021800941128     C                   EXSR      GESVAR
021900940808     C                   ENDIF
022000940809     C*
022100950404     C* TRATTA ASSICURAZIONE PER CONTO
022200011227     C     $ERR          IFEQ      'N'
022300011227     C                   EXSR      GESASC
022400011227     C                   ENDIF
022500941128     C*
022600951213     C* TRATTA NUMERO DI STOP (RITIRO/CONSEGNA)
022700941128     C     $ERR          IFEQ      'N'
022800941128     C                   EXSR      GESSTP
022900941128     C                   ENDIF
023000030113     C*
023100030113     C* TRATTA GIACENZA
023200030113     C     $ERR          IFEQ      'N'
023300030113     C                   EXSR      GESGCP
023400030113     C                   ENDIF
023500991116     C*
023600991116     C* GESTIONE DELL'ANNO PER IL LANCI IN SIMULAZIONE
023700991116     C     $ERR          IFEQ      'N'
023800140227     C     £D$GFSI       ANDEQ     'S'
023900991116     C                   EXSR      GESASI
024000991116     C                   ENDIF
024100941128     C*
024200941128     C* LANCIA IL PGM DI GESTIONE DEL C/ECONOMICO
024300941128     C     $ERR          IFEQ      'N'
024400941202     C                   CALL      'TRECD7R'                            99
024500941128     C                   PARM                    DS$ECO
024600020605     c*
024700971128     C*   SE LANCIO TERMINATO IN ERRORE CHIUDO IL SOTTOSISTEMA GAIDTAQ
024800970507     C     *IN99         IFEQ      '1'
024900140227     C     £D$GFSI       IFNE      'S'
025000970507     C                   Z-ADD     45            LUNG             15 5
025100970507     C                   MOVEL     *BLANKS       COMMAN
025200971128     C                   MOVEA     CMD2(1)       COMMAN           80
025300970507     C                   CALL      'QCMDEXC'
025400970507     C                   PARM                    COMMAN
025500970507     C                   PARM                    LUNG
025600970507     C                   ENDIF
025700991116     C                   ENDIF
025800991116     C                   ENDIF
025900931104     C*
026000940217     C                   END
026100930323     C*
026200940405     C* SE PGM LANCIATO CON 'C' (CLOSE) CHIUDE CON LR (CHIUDE FILE)
026300930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
026400950201     C     D$GSTS        IFEQ      'C'
026500950405     C     D$GPGF        IFEQ      'TNSB23R'
026600980130     C                   EXSR      AGGTAB
026700950411     C                   EXSR      RKECON
026800950405     C                   Z-ADD     *ZEROS        DTSB23
026900950405     C                   ENDIF
027000950201     C* DURANTE IL TRASFERIMENTO NON CHIUDE I FILE
027100950201     C* DURANTE IL LANCIO DEI MESI PASSATI NON CHIUDE I FILE
027200950201     C     DTSB23        IFEQ      *ZEROS
027300941202     C                   CALL      'TRECD7R'                            99
027400941128     C                   PARM                    DS$ECO
027500930323     C                   SETON                                        LR
027600950201     C                   ELSE
027700950201     C                   SETON                                        RT
027800950201     C                   ENDIF
027900950201     C                   ELSE
028000991214     C* STATUS UGUALE A 'S' (SHUTDOWN)
028100991214     C     D$GSTS        IFEQ      'S'
028200140227     c     £D$GFSI       andne     'S'
028300991214     C                   EXSR      AGGTAB
028400991214     C                   CALL      'TRECD7R'                            99
028500991214     C                   PARM                    DS$ECO
028600991214     C                   SETON                                        LR
028700991214     C                   ELSE
028800991214     C* STATUS DIVERSO DA 'C' E 'S'
028900930324     C                   SETON                                        RT
029000991214     C                   ENDIF
029100991214     C                   ENDIF
029200940916     C*/ FINE MAIN DEL PGM
029300941129     C***********************************************************************
029400941129     C*   GESFLA - GESTIONE FLAGS PRIMA/DOPO
029500941129     C***********************************************************************
029600941129     C     GESFLA        BEGSR
029700941129     C*
029800941129     C* GESTIONE FLAGS PRIMA/DOPO
029900941130     C                   MOVEL     'N'           WFPTAS            1
030000941130     C                   MOVEL     'N'           WFDTAS            1
030100941130     C                   MOVEL     'N'           WFPCSB            1
030200941130     C                   MOVEL     'N'           WFDCSB            1
030300030108     C                   MOVEL     'N'           WFPTRS            1
030400030108     C                   MOVEL     'N'           WFDTRS            1
030500030108     C                   MOVEL     'N'           WFPBAN            1
030600030108     C                   MOVEL     'N'           WFDBAN            1
030700941130     C                   MOVEL     'N'           WFPALT            1
030800941130     C                   MOVEL     'N'           WFDALT            1
030900030108     C*
031000990804     C     D$GFIL        IFEQ      'TITAS10F'
031100020527     C     D$GFIL        OREQ      'TITAS00F'
031200941129     C     D$GSTS        IFEQ      'D'
031300990804     C                   MOVEL     'S'           WFPTAS
031400941129     C                   MOVEL     'S'           WFPCSB
031500030108     C                   MOVEL     'S'           WFPTRS
031600030108     C                   MOVEL     'S'           WFPBAN
031700941129     C                   MOVEL     'S'           WFPALT
031800941129     C                   ENDIF
031900941129     C     D$GSTS        IFEQ      'U'
032000990804     C                   MOVEL     'S'           WFPTAS
032100941129     C                   MOVEL     'S'           WFPCSB
032200030108     C                   MOVEL     'S'           WFPTRS
032300030108     C                   MOVEL     'S'           WFPBAN
032400941129     C                   MOVEL     'S'           WFPALT
032500990804     C                   MOVEL     'S'           WFDTAS
032600941129     C                   MOVEL     'S'           WFDCSB
032700030108     C                   MOVEL     'S'           WFDTRS
032800030108     C                   MOVEL     'S'           WFDBAN
032900941129     C                   MOVEL     'S'           WFDALT
033000941129     C                   ENDIF
033100941129     C     D$GSTS        IFEQ      'I'
033200990804     C                   MOVEL     'S'           WFDTAS
033300941129     C                   MOVEL     'S'           WFDCSB
033400030108     C                   MOVEL     'S'           WFDTRS
033500030108     C                   MOVEL     'S'           WFDBAN
033600941129     C                   MOVEL     'S'           WFDALT
033700941129     C                   ENDIF
033800941129     C                   ENDIF
033900030108     C*
034000941129     C     D$GFIL        IFEQ      'TNCSB00F'
034100990804     C                   MOVEL     'S'           WFPTAS
034200030108     C                   MOVEL     'S'           WFPTRS
034300030108     C                   MOVEL     'S'           WFPBAN
034400941129     C                   MOVEL     'S'           WFPALT
034500990804     C                   MOVEL     'S'           WFDTAS
034600030108     C                   MOVEL     'S'           WFDTRS
034700030108     C                   MOVEL     'S'           WFDBAN
034800941129     C                   MOVEL     'S'           WFDALT
034900941129     C     D$GSTS        IFEQ      'D'
035000941129     C                   MOVEL     'S'           WFPCSB
035100941129     C                   ENDIF
035200941129     C     D$GSTS        IFEQ      'U'
035300941129     C                   MOVEL     'S'           WFPCSB
035400941129     C                   MOVEL     'S'           WFDCSB
035500941129     C                   ENDIF
035600941129     C     D$GSTS        IFEQ      'I'
035700941129     C                   MOVEL     'S'           WFDCSB
035800941129     C                   ENDIF
035900941129     C                   ENDIF
036000030108     C*
036100030108     C     D$GFIL        IFEQ      'FIAR500F'
036200030108     C     D$GTRD        IFEQ      'TRS'
036300030108     C                   MOVEL     'S'           WFPTAS
036400030108     C                   MOVEL     'S'           WFPCSB
036500030108     C                   MOVEL     'S'           WFPBAN
036600030108     C                   MOVEL     'S'           WFPALT
036700030108     C                   MOVEL     'S'           WFDTAS
036800030108     C                   MOVEL     'S'           WFDCSB
036900030108     C                   MOVEL     'S'           WFDBAN
037000030108     C                   MOVEL     'S'           WFDALT
037100030108     C     D$GSTS        IFEQ      'D'
037200030108     C                   MOVEL     'S'           WFPTRS
037300030108     C                   ENDIF
037400030108     C     D$GSTS        IFEQ      'U'
037500030108     C                   MOVEL     'S'           WFPTRS
037600030108     C                   MOVEL     'S'           WFDTRS
037700030108     C                   ENDIF
037800030108     C     D$GSTS        IFEQ      'I'
037900030108     C                   MOVEL     'S'           WFDTRS
038000030108     C                   ENDIF
038100030108     C                   ENDIF
038200030108     C                   ENDIF
038300941129     C*
038400030108     C     D$GFIL        IFEQ      'FIAR500F'
038500030108     C     D$GTRD        IFEQ      'BAN'
038600030801     C     D$GTRD        OREQ      'BNB'
038700030801     C                   MOVEL     'S'           WFPTAS
038800030801     C                   MOVEL     'S'           WFPCSB
038900030801     C                   MOVEL     'S'           WFPTRS
039000030801     C                   MOVEL     'S'           WFPALT
039100030108     C                   MOVEL     'S'           WFDTAS
039200030108     C                   MOVEL     'S'           WFDCSB
039300030108     C                   MOVEL     'S'           WFDTRS
039400030108     C                   MOVEL     'S'           WFDALT
039500030108     C     D$GSTS        IFEQ      'D'
039600030108     C                   MOVEL     'S'           WFPBAN
039700030108     C                   ENDIF
039800030108     C     D$GSTS        IFEQ      'U'
039900030108     C                   MOVEL     'S'           WFPBAN
040000030108     C                   MOVEL     'S'           WFDBAN
040100030108     C                   ENDIF
040200030108     C     D$GSTS        IFEQ      'I'
040300030108     C                   MOVEL     'S'           WFDBAN
040400030108     C                   ENDIF
040500030108     C                   ENDIF
040600030108     C                   ENDIF
040700030108     C*
040800941129     C                   ENDSR
040900941129     C***********************************************************************
041000941129     C*   GESCSB - GESTIONE PER VARIAZIONE SU TNCSB10F
041100941129     C***********************************************************************
041200941129     C     GESCSB        BEGSR
041300941129     C*
041400941129     C* PORTO LE IMMAGINI DEL FILE VARIATO (TNSCB) NELLA DS DI TNSCB
041500941129     C                   MOVEL     D$PRE1        D$PCS1
041600941129     C                   MOVEL     D$PRE2        D$PCS2
041700941129     C                   MOVEL     D$DRE1        D$DCS1
041800941129     C                   MOVEL     D$DRE2        D$DCS2
041900950123     C*
042000950123     C* PORTO LE IMMAGINI DEL FILE VARIATO (TNSCB) NEI CAMPI DELLA DS$ECO
042100950123     C* PER USARLE POI EVENTUALMENTE NEGLI ERRORI
042200950123     C                   MOVEL     D$PRE1        D$PCO1
042300950123     C                   MOVEL     D$PRE2        D$PCO2
042400950123     C                   MOVEL     D$DRE1        D$DCO1
042500950123     C                   MOVEL     D$DRE2        D$DCO2
042600950119     C*
042700950119     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
042800950119     C     WFPCSB        IFEQ      'S'
042900950120     C                   Z-ADD     DCPAAS        KERAAS
043000950120     C                   Z-ADD     DCPLNP        KERLNP
043100950120     C                   Z-ADD     DCPNRS        KERNRS
043200950120     C                   Z-ADD     DCPNSP        KERNSP
043300950125     C                   MOVEL     DCPTBL        WTBL
043400950125     C                   Z-ADD     DCPMGS        WMGS
043500950119     C                   ELSE
043600950120     C                   Z-ADD     DCDAAS        KERAAS
043700950120     C                   Z-ADD     DCDLNP        KERLNP
043800950120     C                   Z-ADD     DCDNRS        KERNRS
043900950120     C                   Z-ADD     DCDNSP        KERNSP
044000950125     C                   MOVEL     DCDTBL        WTBL
044100950125     C                   Z-ADD     DCDMGS        WMGS
044200950119     C                   ENDIF
044300950120     C                   EXSR      GESERR
044400950130     C*
044500950201     C* MI CONSTRUISCO LA DATA DELLA BOLLA (DI 8)
044600950130     C                   Z-ADD     KERAAS        DSAAS
044700950130     C                   Z-ADD     WMGS          DSMGS
044800960130     C*
044900960130     C* ESCLUDE SE DATA SPEDIZIONE MINORE DATA ULTIMO TRASF. TAS10 > TASP
045000960130     C     DSDAT8        IFLE      WDT10P
045100960130     C                   MOVEL     'S'           $ERR
045200960130     C                   ENDIF
045300960130     C*
045400040123    1C     $ERR          IFEQ      'N'
045500950201     C*
045600950201     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
045700950405     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
045800040123    2C     D$GPGF        IFEQ      'TNSB23R'
045900950405     C     DSDAT8        ANDGT     DTSB23
046000950201     C                   Z-ADD     DSDAT8        DTSB23            8 0
046100040123   e2C                   ENDIF
046200950217     C*
046300040123    2C     $ERR          IFEQ      'N'
046400941129     C*
046500941129     C* IMPOSTO CAMPI DS UNIFICATA RELATIVI AL CONTRASSEGNO
046600040123    3C     WFPCSB        IFEQ      'S'
046700950207     C                   MOVEL     DCPTPI        D$PTPI
046800040521     C                   MOVEL     DCPsta        D$Psta
046900040123    4C     DCPVCA        IFNE      WDIVCE
047000011106     C                   Z-ADD     DCPCAS        WIMI             30 9
047100011106     C                   MOVEL     DCPVCA        WDVI              3
047200011106     C                   EXSR      CNVEUR
047300011106     C                   Z-ADD     WIMO          DCPCAS
047400040123   e4C                   ENDIF
047500011106     C                   Z-ADD     DCPCAS        D$PCAS
047600040123   e3C                   ENDIF
047700011106     C*
047800040123    3C     WFDCSB        IFEQ      'S'
047900950207     C                   MOVEL     DCDTPI        D$DTPI
048000040521     C                   MOVEL     DCDsta        D$Dsta
048100040123    4C     DCDVCA        IFNE      WDIVCE
048200011106     C                   Z-ADD     DCDCAS        WIMI             30 9
048300011106     C                   MOVEL     DCDVCA        WDVI              3
048400011106     C                   EXSR      CNVEUR
048500011106     C                   Z-ADD     WIMO          DCDCAS
048600040123    4C                   ENDIF
048700011106     C                   Z-ADD     DCDCAS        D$DCAS
048800040123    3C                   ENDIF
048900950125     C*
049000950125     C* SE NON VI SONO VARIAZIONI TRA I DUE CAMPI CHE CI INTERESSANO DI TNCSB
049100950125     C* E SONO IN VARIAZIONE --> ESCO PGM
049200040123    3C     D$GSTO        IFEQ      'U'
049300950207     C     D$PCAS        ANDEQ     D$DCAS
049400950125     C     D$PTPI        ANDEQ     D$DTPI
049500950125     C                   MOVEL     'S'           $ERR
049600040123    3C                   ENDIF
049700040122     c*
049800040123    3C     $ERR          IFEQ      'N'
049900040122     C* LEGGO TITAS10F E IMPOSTO SUA DS
050000040123    4C     WFPCSB        IFEQ      'S'
050100040122     C                   Z-ADD     DCPAAS        TASAAS
050200040122     C                   Z-ADD     DCPLNP        TASLNP
050300040122     C                   Z-ADD     DCPNRS        TASNRS
050400040122     C                   Z-ADD     DCPNSP        TASNSP
050500040122     C                   MOVEL     DCPTBL        TASTBL
050600040123   x4C                   ELSE
050700040122     C                   Z-ADD     DCDAAS        TASAAS
050800040122     C                   Z-ADD     DCDLNP        TASLNP
050900040122     C                   Z-ADD     DCDNRS        TASNRS
051000040122     C                   Z-ADD     DCDNSP        TASNSP
051100040122     C                   MOVEL     DCDTBL        TASTBL
051200040123    4C                   ENDIF
051300040122     C                   SETOFF                                       010203
051400040122     C     KEYTAS        CHAIN     TITAS30C                           99
051500040123    4C     *IN99         IFEQ      '0'
051600040122     C*
051700040122     C* ACCETTO VARIAZIONI DI TNCSB SOLO SE LA BOLLA NON E' IN TITASP0F
051800040122     C* INDICATORE 02 ON --> TIPO RECORD TITAS10F
051900040122     C* INDICATORE 01 ON --> TIPO RECORD TITAS00F
052000040123    5C     *IN02         IFEQ      '1'
052100040122     C     *IN01         OREQ      '1'
052200040122     C*
052300040122     C* MEMORIZZA LA DATA SPEDIZIONE DELLA BOLLA
052400040122     C                   MOVEL     TASAAS        $TASDSP           8 0
052500040122     C                   MOVE      TASMGS        $TASDSP
052600040122     C*
052700040122     C* GESTIONE TITAS PRIMA
052800040123    6C     WFPTAS        IFEQ      'S'
052900040122     C                   EXSR      CPDTAQ
053000040122     c                   EXSR      AZZnCPDTAQ
053100040123   x6C                   ELSE
053200040122     C                   EXSR      APDTAQ
053300040123    6C                   ENDIF
053400040122     C                   MOVEL     D$PDS1        D$PRE1
053500040122     C                   MOVEL     D$PDS2        D$PRE2
053600040122     C* GESTIONE TITAS DOPO
053700040123    6C     WFDTAS        IFEQ      'S'
053800040122     C                   EXSR      CDDTAQ
053900040122     c                   EXSR      AZZnCDDTAQ
054000040123   x6C                   ELSE
054100040122     C                   EXSR      ADDTAQ
054200040123    6C                   ENDIF
054300040122     C                   MOVEL     D$DDS1        D$DRE1
054400040122     C                   MOVEL     D$DDS2        D$DRE2
054500040622     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
054600040622     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
054700040622     c* titas. Perl quelle > invece verranno presi dal fiar5
054800040622     c                   if        dsdat8 < 20040719
054900040621     C                   Z-ADD     d$pflp        D$PAR5flp
055000040621     C                   Z-ADD     d$pfl2        D$PAR5fl2
055100040621     C                   Z-ADD     d$dflp        D$dAR5flp
055200040621     C                   Z-ADD     d$dfl2        D$dAR5fl2
055300040622     c                   end
055400040122     C* IMPOSTO STATUS A 'U' UPDATE
055500040122     C                   MOVEL     'U'           D$GSTS
055600040122     c*
055700040122     C* Controlla SE e COME lanciare il conto economico se record di TNCSB00F
055800040122     C                   EXSR      CHKLANCSB
055900040122     C*
056000040123   x5C                   ELSE
056100040122     C*
056200040122     C* LA BOLLA E' ANCORA IN TITAS00F OPPURE GIA' IN TITASP0F
056300040122     C* NON LA CONSIDERO ED ESCO PGM
056400040122     C                   MOVEL     'S'           $ERR
056500040122     C*
056600040123   e5c                   ENDIF
056700040122     c* aggancio FIAR500F x BANCALI E TRANSITI
056800040123    5C                   IF        $err = 'N'
056900040123     C*
057000040123     C                   Z-ADD     tasAAS        ar5AAS
057100040123     C                   Z-ADD     tasLNP        ar5LNP
057200040123     C                   Z-ADD     tasNRS        ar5NRS
057300040123     C                   Z-ADD     tasNSP        ar5NSP
057400040517     C                   Z-ADD     0             ar5prg
057500040622     c                   if        dsdat8 >= 20040719
057600040517     C                   Z-ADD     tasflp        ar5prg
057700040517     c                   end
057800040122     C* LEGGO fiar500F x transiti
057900040123    6C                   if        (D$PFLP > *zeros OR D$PFL2 > *zeros or
058000040123     C                             D$DFLP > *zeros OR D$DFL2 > *zeros)
058100040122     C                   MOVEL     'TRS'         AR5TRD
058200040617     C     KEYAR5        CHAIN     FIAR531C                           99
058300040517if  7C     *IN99         IFEQ      '0'
058400040617     C     AR5ATB        andeq     *BLANKS
058500040122     C*
058600040617if  8C                   IF        WFPTRS = 'S'
058700040519     c                   exsr      srar5trsp
058800040617e   8C                   ENDIF
058900040122     C*
059000040617if  8C                   IF        WFDTRS = 'S'
059100040519     c                   exsr      srar5trsd
059200040617e   8C                   ENDIF
059300040123    7C                   ENDIF
059400040123    6C                   ENDIF
059500040123     C* LEGGO FIAR5 RECORD BANCALI E IMPOSTO I CAMPI DELLA DS UNIFICATA
059600040123    6c                   if        %subst(d$pgva: 1: 1) = 'B' or
059700040123     c                             %subst(d$dgva: 1: 1) = 'B'
059800040122     C                   MOVEL     'BAN'         AR5TRD
059900040521     c                   clear                   ar5prg
060000040617     C     KEYAR5        CHAIN     FIAR531C                           99
060100040123if  7C     *IN99         IFEQ      '0'
060200040617     C     AR5ATB        andeq     *BLANKS
060300040122     C*
060400040617if  8C                   IF        WFPBAN = 'S'
060500040122     C                   MOVEL     AR5UNI        DAR5BAN
060600040122     C                   Z-ADD     §AR5TBA       D$PAR5TBA
060700040122     C                   Z-ADD     §AR5NBA       D$PAR5NBA
060800040122     C                   Z-ADD     §AR5TB2       D$PAR5TB2
060900040122     C                   Z-ADD     §AR5NB2       D$PAR5NB2
061000040617e   8C                   ENDIF
061100040122     C*
061200040617if  8C                   IF        WFDBAN = 'S'
061300040122     C                   MOVEL     AR5UNI        DAR5BAN
061400040122     C                   Z-ADD     §AR5TBA       D$DAR5TBA
061500040122     C                   Z-ADD     §AR5NBA       D$DAR5NBA
061600040122     C                   Z-ADD     §AR5TB2       D$DAR5TB2
061700040122     C                   Z-ADD     §AR5NB2       D$DAR5NB2
061800040123e   8C                   ENDIF
061900040123e   7C                   ENDIF
062000040123e   6C                   ENDIF
062100941129     C*
062200040122     C* LEGGO fiar500f x bancali
062300040123    6c                   if        %subst(d$pgva: 1: 1) = 'O' or
062400040123     c                             %subst(d$dgva: 1: 1) = 'O'
062500040122     C                   MOVEL     'BNB'         AR5TRD
062600040521     c                   clear                   ar5prg
062700040617     C     KEYAR5        CHAIN     FIAR531C                           99
062800040123if  7C     *IN99         IFEQ      '0'
062900040617     C     AR5ATB        andeq     *BLANKS
063000040122     C*
063100040617if  8C                   IF        WFPBAN = 'S'
063200040122     C                   MOVEL     AR5UNI        DAR5BNB
063300040122     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
063400040122     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
063500040122     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
063600040122     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
063700040218     C                   Z-ADD     §AR5bcsf      D$PAR5csf
063800040218     C                   Z-ADD     §AR5bcor      D$PAR5cor
063900040617e   8C                   ENDIF
064000040122     C*
064100040617if  8C                   IF        WFDBAN = 'S'
064200040122     C                   MOVEL     AR5UNI        DAR5BNB
064300040122     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
064400040122     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
064500040122     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
064600040122     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
064700040218     C                   Z-ADD     §AR5bcsf      D$DAR5csf
064800040218     C                   Z-ADD     §AR5bcor      D$DAR5cor
064900040617e   8C                   ENDIF
065000040123e   7C                   ENDIF
065100040123e   6C                   ENDIF
065200040123    5C                   ENDIF
065300941129     C*
065400040123   x4C                   ELSE
065500941129     C*
065600950324     C* CONTROLLO SE SI TRATTA DI UN CONTRASSEGNO OMESSO (CIOE' INSERITO
065700950324     C* DALLA SEDE ANCHE SENZA BOLLA), IN QUESTO CASO NON TRATTO IL RECORD.
065800950324     C                   MOVEL     'N'           FLCSBO            1
065900950324     C                   Z-ADD     TASAAS        CSVAAS
066000950324     C                   Z-ADD     TASLNP        CSVLNP
066100950324     C                   Z-ADD     TASNRS        CSVNRS
066200950324     C                   Z-ADD     TASNSP        CSVNSP
066300950324     C     KEYCSV        SETLL     TNCSV01L                           99
066400950324     C  N99KEYCSV        READE     TNCSV01L                               99
066500040123    5C     *IN99         DOWEQ     *OFF
066600950324     C     CSVAAS        ANDEQ     TASAAS
066700950324     C     CSVLNP        ANDEQ     TASLNP
066800950324     C     CSVNRS        ANDEQ     TASNRS
066900950324     C     CSVNSP        ANDEQ     TASNSP
067000040123    6C     CSVCAV        IFEQ      'IMCA'
067100950327     C     CSVFEV        ANDEQ     046
067200950324     C                   MOVEL     'S'           FLCSBO
067300040123    6C                   ENDIF
067400950324     C  N99KEYCSV        READE     TNCSV01L                               99
067500040123    5C                   ENDDO
067600950324     C*
067700040123    5C     FLCSBO        IFEQ      'N'
067800941130     C                   MOVEL     MSG(2)        $MSG
067900941129     C                   EXSR      SCRERR
068000040123   x5C                   ELSE
068100950324     C                   MOVEL     'S'           $ERR
068200040123    5C                   ENDIF
068300941129     C*
068400040123    4C                   ENDIF
068500040123    3C                   ENDIF
068600040123    2C                   ENDIF
068700040123    1C                   ENDIF
068800941129     C*
068900941129     C                   ENDSR
069000020605     C*-------------------------------------------------------------------------
069100020605     C* Controlla se lanciare il conto economico se record di TNCSB00F
069200020605     C*-------------------------------------------------------------------------
069300020605     C     CHKLANCSB     BEGSR
069400020605     C*---
069500020605     C* non lancia il conto economico SE:
069600020611     C* . contrassegno di una bolla di TITAS00F con data prima del x giugno 2002
069700020607     C* lancia il conto economico IN MODO:
069800020607     C* . da aggiornare solo i files come TITAS00F se il contrassengo in lì
069900020605     C*---
070000020605IF  1C     D$GFIL        IFEQ      'TNCSB00F'
070100020605     C     *IN01         ANDEQ     *ON
070200020619IF  2C     $TASDSP       IFLT      20020601
070300020605     C                   MOVEL     'S'           $ERR
070400020611X   2C                   ELSE
070500020607     C                   Z-ADD     1             I
070600020607     C     'TITAS00F  '  LOOKUP    FIL(I)                                 99
070700020607IF  3C     *IN99         IFEQ      '1'
070800020607     C                   MOVEL     SDA(I)        D$GSDA
070900020607     C                   MOVEL     SDC(I)        D$GSDC
071000020607     C                   MOVEL     SDF(I)        D$GSDF
071100020607     C                   MOVEL     SDO(I)        D$GSDO
071200020607     C                   MOVEL     SDP(I)        D$GSDP
071300020607     C                   MOVEL     SDR(I)        D$GSDR
071400020607     C                   MOVEL     SDT(I)        D$GSDT
071500040316     C                   MOVEL     SDE(I)        D$GSDE
071600020607     C                   MOVEL     CET(I)        D$GCET
071700020607     C                   MOVEL     MOV(I)        D$GWFM
071800020607     C                   MOVEL     FIM(I)        D$GFIM
071900020607     C                   MOVEL     FCE(I)        D$GFCE
072000030108     C                   MOVEL     COP(I)        D$GCOP
072100020607E   3C                   ENDIF
072200020611E   2C                   ENDIF
072300020611E   1C                   ENDIF
072400020605     C*
072500020605     C                   ENDSR
072600030108     C***********************************************************************
072700030108     C*   GESAR5 - GESTIONE PER VARIAZIONE SU FIAR500F (TRANSITI o BANCALI)
072800030108     C***********************************************************************
072900030108     C     GESAR5        BEGSR
073000030108     C*
073100030108     C* TRANSITI
073200030108IF  1C     D$GTRD        IFEQ      'TRS'
073300030108     C                   EXSR      GESTRS
073400030108E   1C                   ENDIF
073500030108     C* BANCALI
073600030108IF  1C     D$GTRD        IFEQ      'BAN'
073700030801     C     D$GTRD        OREQ      'BNB'
073800030108     C                   EXSR      GESBAN
073900030108E   1C                   ENDIF
074000030108     C*
074100030108     C                   ENDSR
074200030108     C***********************************************************************
074300030108     C*   GESTRS - GESTIONE PER VARIAZIONE SU FIAR500F -TRANSITI-
074400030108     C***********************************************************************
074500030108     C     GESTRS        BEGSR
074600030108     C*
074700030108     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIARB) NELLA DS DI FIARB TRANSITI
074800030108     C                   MOVEL     D$PRE1        D$PAT1
074900030108     C                   MOVEL     D$DRE1        D$DAT1
075000030108     C*
075100030108     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIAR5) TRANSITI NEI CAMPI DELLA
075200030108     C* DS$ECO PER USARLE POI EVENTUALMENTE NEGLI ERRORI
075300030108     C                   MOVEL     D$PRE1        D$PAR5T
075400030108     C                   MOVEL     D$DRE1        D$DAR5T
075500030108     C*
075600030108     C* LEGGO TITAS10F E IMPOSTO SUA DS
075700030109IF  1C     WFPTRS        IFEQ      'S'
075800030109     C                   Z-ADD     DATPAAS       TASAAS
075900030109     C                   Z-ADD     DATPLNP       TASLNP
076000030109     C                   Z-ADD     DATPNRS       TASNRS
076100030109     C                   Z-ADD     DATPNSP       TASNSP
076200030109X   1C                   ELSE
076300030109     C                   Z-ADD     DATDAAS       TASAAS
076400030109     C                   Z-ADD     DATDLNP       TASLNP
076500030109     C                   Z-ADD     DATDNRS       TASNRS
076600030109     C                   Z-ADD     DATDNSP       TASNSP
076700030109E   1C                   ENDIF
076800030108     C                   SETOFF                                       010203
076900030109     C     KE2TAS        CHAIN     TITAS30C                           99
077000030109IF  1C     *IN99         IFEQ      '0'
077100030108     C*
077200030109     C* ACCETTO VARIAZIONI DI FIAR5 SOLO SE LA BOLLA NON E' IN TITASP0F
077300030108     C* INDICATORE 02 ON --> TIPO RECORD TITAS10F
077400030108     C* INDICATORE 01 ON --> TIPO RECORD TITAS00F
077500030109IF  2C     *IN02         IFEQ      '1'
077600030108     C     *IN01         OREQ      '1'
077700030109     C*
077800030109     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
077900030109IF  3C     WFPTRS        IFEQ      'S'
078000030109     C                   Z-ADD     DATPAAS       KERAAS
078100030109     C                   Z-ADD     DATPLNP       KERLNP
078200030109     C                   Z-ADD     DATPNRS       KERNRS
078300030109     C                   Z-ADD     DATPNSP       KERNSP
078400030109     C                   MOVEL     TASTBL        WTBL
078500030109     C                   Z-ADD     TASMGS        WMGS
078600030109X   3C                   ELSE
078700030109     C                   Z-ADD     DATDAAS       KERAAS
078800030109     C                   Z-ADD     DATDLNP       KERLNP
078900030109     C                   Z-ADD     DATDNRS       KERNRS
079000030109     C                   Z-ADD     DATDNSP       KERNSP
079100030109     C                   MOVEL     TASTBL        WTBL
079200030109     C                   Z-ADD     TASMGS        WMGS
079300030109E   3C                   ENDIF
079400030109     C                   EXSR      GESERR
079500030109     C*
079600030109     C* MI CONSTRUISCO LA DATA DELLA BOLLA (DI 8)
079700030109     C                   Z-ADD     KERAAS        DSAAS
079800030109     C                   Z-ADD     WMGS          DSMGS
079900030109     C*
080000030109     C* ESCLUDE SE DATA SPEDIZIONE MINORE DATA ULTIMO TRASF. TAS10 > TASP
080100030109IF  3C     DSDAT8        IFLE      WDT10P
080200030109     C                   MOVEL     'S'           $ERR
080300030109E   3C                   ENDIF
080400040514     C* ESCLUDE VARIAZIONE FATTE SUI TRANSITI X SPEDIZIONE >=19/7/2004
080500040514     C* XCHÈ RECEPISCE VARIAZIONE FATTA SUL TITAS
080600040514IF  3C     DSDAT8        IFGE      20040719
080700040514     C                   MOVEL     'S'           $ERR
080800040514E   3C                   ENDIF
080900030113     C*
081000030113     C* NO ERRORI
081100030113IF  3C     $ERR          IFEQ      'N'
081200030113     C*
081300030109     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
081400030109     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
081500030109IF  4C     D$GPGF        IFEQ      'TNSB23R'
081600030109     C     DSDAT8        ANDGT     DTSB23
081700030109     C                   Z-ADD     DSDAT8        DTSB23            8 0
081800030109E   4C                   ENDIF
081900030113     C*
082000030113     C* IMPOSTO CAMPI DS UNIFICATA RELATIVI AI TRANSITI
082100030113IF  4C     WFPTRS        IFEQ      'S'
082200030113     C                   MOVEL     DATPUNI       DAR5TRS
082300040621     C                   Z-ADD     §AR5DET       D$PAR5DET
082400030113     C                   Z-ADD     §AR5DUT       D$PAR5DUT
082500030113     C                   Z-ADD     §AR5DE2       D$PAR5DE2
082600030113     C                   Z-ADD     §AR5DU2       D$PAR5DU2
082700030113E   4C                   ENDIF
082800030113     C*
082900030113IF  4C     WFDTRS        IFEQ      'S'
083000030113     C                   MOVEL     DATDUNI       DAR5TRS
083100040621     C                   Z-ADD     §AR5DET       D$DAR5DET
083200030113     C                   Z-ADD     §AR5DUT       D$DAR5DUT
083300030113     C                   Z-ADD     §AR5DE2       D$DAR5DE2
083400030113     C                   Z-ADD     §AR5DU2       D$DAR5DU2
083500030113E   4C                   ENDIF
083600030113     C*
083700030113     C* SE NON VI SONO VARIAZIONI TRA I CAMPI CHE CI INTERESSANO DI FIAR5
083800030113     C* E SONO IN VARIAZIONE --> ESCO PGM
083900030113IF  4C     D$GSTO        IFEQ      'U'
084000030113     C     D$PAR5DET     ANDEQ     D$DAR5DET
084100030113     C     D$PAR5DUT     ANDEQ     D$DAR5DUT
084200030113     C     D$PAR5DE2     ANDEQ     D$DAR5DE2
084300030113     C     D$PAR5DU2     ANDEQ     D$DAR5DU2
084400030113     C                   MOVEL     'S'           $ERR
084500030113E   4C                   ENDIF
084600030113     C*
084700030113     C* NO ERRORI
084800030113IF  4C     $ERR          IFEQ      'N'
084900030108     C*
085000030108     C* GESTIONE TITAS PRIMA
085100030113IF  5C     WFPTAS        IFEQ      'S'
085200030108     C                   EXSR      CPDTAQ
085300030108     c                   EXSR      AZZnCPDTAQ
085400030113X   5C                   ELSE
085500030108     C                   EXSR      APDTAQ
085600030113E   5C                   ENDIF
085700030108     C                   MOVEL     D$PDS1        D$PRE1
085800030108     C                   MOVEL     D$PDS2        D$PRE2
085900030108     C* GESTIONE TITAS DOPO
086000030113IF  5C     WFDTAS        IFEQ      'S'
086100030108     C                   EXSR      CDDTAQ
086200030108     c                   EXSR      AZZnCDDTAQ
086300030113X   5C                   ELSE
086400030108     C                   EXSR      ADDTAQ
086500030113E   5C                   ENDIF
086600030108     C                   MOVEL     D$DDS1        D$DRE1
086700030108     C                   MOVEL     D$DDS2        D$DRE2
086800030108     C*
086900040621     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
087000040621     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
087100040621     c* titas
087200040622     c                   if        dsdat8 < 20040719
087300040621     c                   z-add     d$pflp        d$par5flp
087400040621     c                   z-add     d$pfl2        d$par5fl2
087500040621     c                   z-add     d$dflp        d$dar5flp
087600040621     c                   z-add     d$dfl2        d$dar5fl2
087700040622     c                   end
087800030108     C* IMPOSTO STATUS A 'U' UPDATE
087900030108     C                   MOVEL     'U'           D$GSTS
088000030113E   4C                   ENDIF
088100030113E   3C                   ENDIF
088200040122     c* LEGGO FIAR500F X BAN
088300040521IF  3C                   IF        $err='N'
088400040521   3ac                   if        (%subst(d$pgva: 1: 1) = 'O' or
088500040123     c                             %subst(d$dgva: 1: 1) = 'O' or
088600040123     c                             %subst(d$pgva: 1: 1) = 'B' or
088700040123     c                             %subst(d$dgva: 1: 1) = 'B')
088800040122     c*
088900040123     C                   Z-ADD     tasAAS        AR5AAS
089000040123     C                   Z-ADD     tasLNP        AR5LNP
089100040123     C                   Z-ADD     tasNRS        AR5NRS
089200040123     C                   Z-ADD     tasNSP        AR5NSP
089300040123     c* solo se esiste il flag bancali (B) sul TITAS
089400040123    4c                   if        %subst(d$pgva: 1: 1) = 'B' or
089500040123     c                             %subst(d$dgva: 1: 1) = 'B'
089600040122     C                   MOVEL     'BAN'         AR5TRD
089700040521     c                   clear                   ar5prg
089800040617     C     KEYAR5        CHAIN     FIAR531C                           99
089900040123if  5C     *IN99         IFEQ      '0'
090000040617     C     AR5ATB        andeq     *BLANKS
090100040122     C*
090200040617if  6C                   IF        WFPBAN = 'S'
090300040122     C                   MOVEL     AR5UNI        DAR5BAN
090400040122     C                   Z-ADD     §AR5TBA       D$PAR5TBA
090500040122     C                   Z-ADD     §AR5NBA       D$PAR5NBA
090600040122     C                   Z-ADD     §AR5TB2       D$PAR5TB2
090700040122     C                   Z-ADD     §AR5NB2       D$PAR5NB2
090800040617e   6C                   ENDIF
090900040122     C*
091000040617if  6C                   IF        WFDBAN = 'S'
091100040122     C                   MOVEL     AR5UNI        DAR5BAN
091200040122     C                   Z-ADD     §AR5TBA       D$DAR5TBA
091300040122     C                   Z-ADD     §AR5NBA       D$DAR5NBA
091400040122     C                   Z-ADD     §AR5TB2       D$DAR5TB2
091500040122     C                   Z-ADD     §AR5NB2       D$DAR5NB2
091600040123e   6C                   ENDIF
091700040123e   5C                   ENDIF
091800040123e   4C                   ENDIF
091900040123     c* LEGGO FIAR500F X BNB solo se il flag bancali (O) su TITAS
092000040123    4c                   if        %subst(d$pgva: 1: 1) = 'O' or
092100040123     c                             %subst(d$dgva: 1: 1) = 'O'
092200040122     C                   MOVEL     'BNB'         AR5TRD
092300040521     c                   clear                   ar5prg
092400040617     C     KEYAR5        CHAIN     FIAR531C                           99
092500040123if  5C     *IN99         IFEQ      '0'
092600040617     C     AR5ATB        andeq     *BLANKS
092700040122     C*
092800040617if  6C                   IF        WFPBAN = 'S'
092900040122     C                   MOVEL     AR5UNI        DAR5BNB
093000040122     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
093100040122     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
093200040122     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
093300040122     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
093400040218     C                   Z-ADD     §AR5bcsf      D$PAR5csf
093500040218     C                   Z-ADD     §AR5bcor      D$PAR5cor
093600040617e   6C                   ENDIF
093700040122     C*
093800040617if  6C                   IF        WFDBAN = 'S'
093900040122     C                   MOVEL     AR5UNI        DAR5BNB
094000040122     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
094100040122     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
094200040122     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
094300040122     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
094400040218     C                   Z-ADD     §AR5bcsf      D$dAR5csf
094500040218     C                   Z-ADD     §AR5bcor      D$dAR5cor
094600040123e   6C                   ENDIF
094700040123e   5C                   ENDIF
094800040123e   4C                   ENDIF
094900040521e  3aC                   ENDIF
095000040122     C*
095100040122     C* CONTRASSEGNO
095200040122     C* LEGGO TNCSB SOLO SE FLAG CONTRASSEGNO SUL TITAS = S
095300040123if  4C     D$PFCA        IFEQ      'S'
095400040122     C     D$DFCA        OREQ      'S'
095500040122     C*
095600040122     C* LEGGO TNCSB00F E IMPOSTO I CAMPI DELLA DS UNIFICATA
095700040123     C                   Z-ADD     tasAAS        CSBAAS
095800040123     C                   Z-ADD     tasLNP        CSBLNP
095900040123     C                   Z-ADD     tasNRS        CSBNRS
096000040123     C                   Z-ADD     tasNSP        CSBNSP
096100040123     C*
096200040122     C     KEYCSB        CHAIN     TNCSB03L                           99
096300040123if  5C     *IN99         IFEQ      '0'
096400040521if  7C     WFPCSB        IFEQ      'S'
096500040521     C                   MOVEL     CSBsta        D$Psta
096600040521     c                   end
096700040521if  7C     WFdCSB        IFEQ      'S'
096800040521     C                   MOVEL     CSBsta        D$dsta
096900040521     c                   end
097000040122     C* CONTROLLO ANNULLAMENTO
097100040123if  6C     CSBSTA        IFNE      9
097200040122     C*
097300040123if  7C     WFPCSB        IFEQ      'S'
097400040122     C     D$PFCA        ANDEQ     'S'
097500040122     C                   MOVEL     CSBTPI        D$PTPI
097600040123if  8C     CSBVCA        IFNE      WDIVCE
097700040122     C                   Z-ADD     CSBCAS        WIMI
097800040122     C                   MOVEL     CSBVCA        WDVI
097900040122     C                   EXSR      CNVEUR
098000040122     C                   Z-ADD     WIMO          D$PCAS
098100040123x   8C                   ELSE
098200040122     C                   Z-ADD     CSBCAS        D$PCAS
098300040123e   8C                   ENDIF
098400040123e   7C                   ENDIF
098500040122     C*
098600040123if  7C     WFDCSB        IFEQ      'S'
098700040122     C     D$DFCA        ANDEQ     'S'
098800040122     C                   MOVEL     CSBTPI        D$DTPI
098900040123if  8C     CSBVCA        IFNE      WDIVCE
099000040122     C                   Z-ADD     CSBCAS        WIMI
099100040122     C                   MOVEL     CSBVCA        WDVI
099200040122     C                   EXSR      CNVEUR
099300040122     C                   Z-ADD     WIMO          D$DCAS
099400040123x   8c                   ELSE
099500040122     C                   Z-ADD     CSBCAS        D$DCAS
099600040123e   8C                   ENDIF
099700040122e   7C                   ENDIF
099800040122e   6C                   ENDIF
099900040122e   5C                   ENDIF
100000040122     c*
100100040122e   4C                   ENDIF
100200040122e   3C                   ENDIF
100300040122     c*
100400030113X   2C                   ELSE
100500030109     C* LA BOLLA E' GIA' IN TITASP0F NON LA CONSIDERO ED ESCO PGM
100600030108     C                   MOVEL     'S'           $ERR
100700030113E   2C                   ENDIF
100800040122     C* NON TROVATO IN TITAS
100900030113X   1C                   ELSE
101000030109     C                   MOVEL     MSG(7)        $MSG
101100030109     C                   EXSR      SCRERR
101200030113E   1C                   ENDIF
101300030108     C*
101400030108     C                   ENDSR
101500030113     C*
101600030109     C***********************************************************************
101700030109     C*   GESBAN - GESTIONE PER VARIAZIONE SU FIAR500F -BANCALI-
101800030109     C***********************************************************************
101900030109     C     GESBAN        BEGSR
102000030109     C*
102100030109     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIARB) NELLA DS DI FIARB BANCALI
102200030109     C                   MOVEL     D$PRE1        D$PAB1
102300030109     C                   MOVEL     D$DRE1        D$DAB1
102400030109     C*
102500030109     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIAR5) BANCALI NEI CAMPI DELLA
102600030109     C* DS$ECO PER USARLE POI EVENTUALMENTE NEGLI ERRORI
102700030109     C                   MOVEL     D$PRE1        D$PAR5B
102800030109     C                   MOVEL     D$DRE1        D$DAR5B
102900030109     C*
103000030109     C* LEGGO TITAS10F E IMPOSTO SUA DS
103100030109IF  1C     WFPBAN        IFEQ      'S'
103200030109     C                   Z-ADD     DABPAAS       TASAAS
103300030109     C                   Z-ADD     DABPLNP       TASLNP
103400030109     C                   Z-ADD     DABPNRS       TASNRS
103500030109     C                   Z-ADD     DABPNSP       TASNSP
103600030109X   1C                   ELSE
103700030109     C                   Z-ADD     DABDAAS       TASAAS
103800030109     C                   Z-ADD     DABDLNP       TASLNP
103900030109     C                   Z-ADD     DABDNRS       TASNRS
104000030109     C                   Z-ADD     DABDNSP       TASNSP
104100030109E   1C                   ENDIF
104200030109     C                   SETOFF                                       010203
104300030109     C     KE2TAS        CHAIN     TITAS30C                           99
104400030109IF  1C     *IN99         IFEQ      '0'
104500030109     C*
104600030109     C* ACCETTO VARIAZIONI DI FIAR5 SOLO SE LA BOLLA NON E' IN TITASP0F
104700030109     C* INDICATORE 02 ON --> TIPO RECORD TITAS10F
104800030109     C* INDICATORE 01 ON --> TIPO RECORD TITAS00F
104900030109IF  2C     *IN02         IFEQ      '1'
105000030109     C     *IN01         OREQ      '1'
105100030109     C*
105200030109     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
105300030109IF  3C     WFPBAN        IFEQ      'S'
105400030109     C                   Z-ADD     DABPAAS       KERAAS
105500030109     C                   Z-ADD     DABPLNP       KERLNP
105600030109     C                   Z-ADD     DABPNRS       KERNRS
105700030109     C                   Z-ADD     DABPNSP       KERNSP
105800030109     C                   MOVEL     TASTBL        WTBL
105900030109     C                   Z-ADD     TASMGS        WMGS
106000030109X   3C                   ELSE
106100030109     C                   Z-ADD     DABDAAS       KERAAS
106200030109     C                   Z-ADD     DABDLNP       KERLNP
106300030109     C                   Z-ADD     DABDNRS       KERNRS
106400030109     C                   Z-ADD     DABDNSP       KERNSP
106500030109     C                   MOVEL     TASTBL        WTBL
106600030109     C                   Z-ADD     TASMGS        WMGS
106700030109E   3C                   ENDIF
106800030109     C                   EXSR      GESERR
106900030109     C*
107000030109     C* MI CONSTRUISCO LA DATA DELLA BOLLA (DI 8)
107100030109     C                   Z-ADD     KERAAS        DSAAS
107200030109     C                   Z-ADD     WMGS          DSMGS
107300030109     C*
107400030109     C* ESCLUDE SE DATA SPEDIZIONE MINORE DATA ULTIMO TRASF. TAS10 > TASP
107500030109IF  3C     DSDAT8        IFLE      WDT10P
107600030109     C                   MOVEL     'S'           $ERR
107700030109E   3C                   ENDIF
107800030109     C* NO ERRORI
107900030109IF  3C     $ERR          IFEQ      'N'
108000030109     C*
108100030109     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
108200030109     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
108300030109IF  4C     D$GPGF        IFEQ      'TNSB23R'
108400030109     C     DSDAT8        ANDGT     DTSB23
108500030109     C                   Z-ADD     DSDAT8        DTSB23            8 0
108600030109E   4C                   ENDIF
108700030113     C*
108800030729     C* IMPOSTO CAMPI DS UNIFICATA RELATIVI AI BANCALI
108900030801IF  4C     WFPBAN        IFEQ      'S'
109000030801IF  5C     D$GTRD        IFEQ      'BAN'
109100030801     C                   MOVEL     DABPUNI       DAR5BAN
109200030801     C                   Z-ADD     §AR5TBA       D$PAR5TBA
109300030801     C                   Z-ADD     §AR5NBA       D$PAR5NBA
109400030801     C                   Z-ADD     §AR5TB2       D$PAR5TB2
109500030801     C                   Z-ADD     §AR5NB2       D$PAR5NB2
109600030801E   5C                   ENDIF
109700030801IF  5C     D$GTRD        IFEQ      'BNB'
109800030801     C                   MOVEL     DABPUNI       DAR5BNB
109900030801     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
110000030801     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
110100030801     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
110200030801     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
110300040218     C                   Z-ADD     §AR5Bcsf      D$PAR5csf
110400040218     C                   Z-ADD     §AR5Bcor      D$PAR5cor
110500030801E   5C                   ENDIF
110600030801E   4C                   ENDIF
110700030113     C*
110800030113IF  4C     WFDBAN        IFEQ      'S'
110900030801IF  5C     D$GTRD        IFEQ      'BAN'
111000030801     C                   MOVEL     DABDUNI       DAR5BAN
111100030801     C                   Z-ADD     §AR5TBA       D$DAR5TBA
111200030801     C                   Z-ADD     §AR5NBA       D$DAR5NBA
111300030801     C                   Z-ADD     §AR5TB2       D$DAR5TB2
111400030801     C                   Z-ADD     §AR5NB2       D$DAR5NB2
111500030801E   5C                   ENDIF
111600030801IF  5C     D$GTRD        IFEQ      'BNB'
111700030801     C                   MOVEL     DABDUNI       DAR5BNB
111800030801     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
111900030801     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
112000030801     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
112100030801     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
112200040218     C                   Z-ADD     §AR5Bcsf      D$DAR5csf
112300040218     C                   Z-ADD     §AR5Bcor      D$DAR5cor
112400030801E   5C                   ENDIF
112500030801E   4C                   ENDIF
112600030113     C*
112700030113     C* SE NON VI SONO VARIAZIONI TRA I CAMPI CHE CI INTERESSANO DI FIAR5
112800030113     C* E SONO IN VARIAZIONE --> ESCO PGM
112900030113IF  4C     D$GSTO        IFEQ      'U'
113000030113     C     D$PAR5TBA     ANDEQ     D$DAR5TBA
113100030113     C     D$PAR5NBA     ANDEQ     D$DAR5NBA
113200030113     C     D$PAR5TB2     ANDEQ     D$DAR5TB2
113300030113     C     D$PAR5NB2     ANDEQ     D$DAR5NB2
113400040218     C     D$PAR5CSF     ANDEQ     D$DAR5CSF
113500040218     C     D$PAR5COR     ANDEQ     D$DAR5COR
113600030113     C                   MOVEL     'S'           $ERR
113700030113E   4C                   ENDIF
113800030113     C*
113900030113     C* NO ERRORI
114000030113IF  4C     $ERR          IFEQ      'N'
114100030109     C*
114200030109     C* GESTIONE TITAS PRIMA
114300030113IF  5C     WFPTAS        IFEQ      'S'
114400030109     C                   EXSR      CPDTAQ
114500030109     c                   EXSR      AZZnCPDTAQ
114600030113X   5C                   ELSE
114700030109     C                   EXSR      APDTAQ
114800030113E   5C                   ENDIF
114900030109     C                   MOVEL     D$PDS1        D$PRE1
115000030109     C                   MOVEL     D$PDS2        D$PRE2
115100030109     C* GESTIONE TITAS DOPO
115200030113IF  5C     WFDTAS        IFEQ      'S'
115300030109     C                   EXSR      CDDTAQ
115400030109     c                   EXSR      AZZnCDDTAQ
115500030113X   5C                   ELSE
115600030109     C                   EXSR      ADDTAQ
115700030113E   5C                   ENDIF
115800030109     C                   MOVEL     D$DDS1        D$DRE1
115900030109     C                   MOVEL     D$DDS2        D$DRE2
116000040622     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
116100040622     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
116200040622     c* titas.Per quelle > verranno presi dal fiar5
116300040622     c                   if        dsdat8 < 20040719
116400040621     C                   Z-ADD     d$pflp        D$PAR5flp
116500040621     C                   Z-ADD     d$pfl2        D$PAR5fl2
116600040621     C                   Z-ADD     d$dflp        D$dAR5flp
116700040621     C                   Z-ADD     d$dfl2        D$dAR5fl2
116800040622     c                   end
116900030109     C* IMPOSTO STATUS A 'U' UPDATE
117000030109     C                   MOVEL     'U'           D$GSTS
117100030113E   4C                   ENDIF
117200030113E   3C                   ENDIF
117300040123     C* CONTRASSEGNO
117400040123     C* LEGGO TNCSB SOLO SE FLAG CONTRASSEGNO SUL TITAS = S
117500040123if  3C     D$PFCA        IFEQ      'S'
117600040123     C     D$DFCA        OREQ      'S'
117700040123     C*
117800040123     C* LEGGO TNCSB00F E IMPOSTO I CAMPI DELLA DS UNIFICATA
117900040123     C                   Z-ADD     tasAAS        CSBAAS
118000040123     C                   Z-ADD     tasLNP        CSBLNP
118100040123     C                   Z-ADD     tasNRS        CSBNRS
118200040123     C                   Z-ADD     tasNSP        CSBNSP
118300040123     C*
118400040123     C     KEYCSB        CHAIN     TNCSB03L                           99
118500040123if  4C     *IN99         IFEQ      '0'
118600040617     C     WFPCSB        IFEQ      'S'
118700040521     C                   MOVEL     CSBsta        D$Psta
118800040521     c                   end
118900040617     C     WFdCSB        IFEQ      'S'
119000040521     C                   MOVEL     CSBsta        D$dsta
119100040521     c                   end
119200040123     C* CONTROLLO ANNULLAMENTO
119300040123if  5C     CSBSTA        IFNE      9
119400040123     C*
119500040123if  6C     WFPCSB        IFEQ      'S'
119600040123     C     D$PFCA        ANDEQ     'S'
119700040123     C                   MOVEL     CSBTPI        D$PTPI
119800040123if  7C     CSBVCA        IFNE      WDIVCE
119900040123     C                   Z-ADD     CSBCAS        WIMI
120000040123     C                   MOVEL     CSBVCA        WDVI
120100040123     C                   EXSR      CNVEUR
120200040123     C                   Z-ADD     WIMO          D$PCAS
120300040123x   7C                   ELSE
120400040123     C                   Z-ADD     CSBCAS        D$PCAS
120500040123e   7C                   ENDIF
120600040123e   6C                   ENDIF
120700040123     C*
120800040123if  6C     WFDCSB        IFEQ      'S'
120900040123     C     D$DFCA        ANDEQ     'S'
121000040123     C                   MOVEL     CSBTPI        D$DTPI
121100040123if  7C     CSBVCA        IFNE      WDIVCE
121200040123     C                   Z-ADD     CSBCAS        WIMI
121300040123     C                   MOVEL     CSBVCA        WDVI
121400040123     C                   EXSR      CNVEUR
121500040123     C                   Z-ADD     WIMO          D$DCAS
121600040123x   7c                   ELSE
121700040123     C                   Z-ADD     CSBCAS        D$DCAS
121800040123e   7C                   ENDIF
121900040123e   6C                   ENDIF
122000040123e   5C                   ENDIF
122100040123e   4C                   ENDIF
122200040123e   3C                   ENDIF
122300040123     C* LEGGO fiar500F x transiti
122400040123    3C                   if        (D$PFLP > *zeros OR D$PFL2 > *zeros or
122500040123     C                             D$DFLP > *zeros OR D$DFL2 > *zeros)
122600040123     C                   Z-ADD     tasAAS        ar5AAS
122700040123     C                   Z-ADD     tasLNP        ar5LNP
122800040123     C                   Z-ADD     tasNRS        ar5NRS
122900040123     C                   Z-ADD     tasNSP        ar5NSP
123000040521     C                   clear                   ar5prg
123100040622     c                   if        dsdat8 >= 20040719
123200040517     C                   Z-ADD     tasflp        ar5prg
123300040517     c                   end
123400040123     C*
123500040123     C                   MOVEL     'TRS'         AR5TRD
123600040617     C     KEYAR5        CHAIN     FIAR531C                           99
123700040517if  4C     *IN99         IFEQ      '0'
123800040617     C     AR5ATB        andeq     *BLANKS
123900040123     C*
124000040617     C                   IF        WFPTRS = 'S'
124100040519     c                   exsr      srar5trsp
124200040617     c                   end
124300040123     C*
124400040617     C                   IF        WFDTRS = 'S'
124500040519     c                   exsr      srar5trsd
124600040617     C                   ENDIF
124700040123e   4C                   ENDIF
124800040123e   3C                   ENDIF
124900040123     c*
125000030109X   2C                   ELSE
125100030109     C*
125200030109     C* LA BOLLA E' GIA' IN TITASP0F NON LA CONSIDERO ED ESCO PGM
125300030109     C                   MOVEL     'S'           $ERR
125400030109E   2C                   ENDIF
125500030109     C*
125600040123     C* FIAR5 NON TROVATO IN TITAS
125700030109X   1C                   ELSE
125800030113     C                   MOVEL     MSG(8)        $MSG
125900030109     C                   EXSR      SCRERR
126000030109E   1C                   ENDIF
126100030109     C*
126200030109     C                   ENDSR
126300030109     C***********************************************************************
126400990804     C*   GESTAS - GESTIONE VARIAZIONE DI TITAS00F
126500941129     C***********************************************************************
126600941129     C     GESTAS        BEGSR
126700941129     C*
126800990804     C* PORTO LE IMMAGINI DEL FILE VARIATO (TITAS) NELLA DS DI TITAS
126900040123if  2C     WFPTAS        IFEQ      'S'
127000941129     C                   MOVEL     D$PRE1        D$PDS1
127100941129     C                   MOVEL     D$PRE2        D$PDS2
127200040123     c                   end
127300040123if  2C     WFDTAS        IFEQ      'S'
127400941129     C                   MOVEL     D$DRE1        D$DDS1
127500941129     C                   MOVEL     D$DRE2        D$DDS2
127600040123     c                   end
127700030113     C* NO ERRORI
127800030113if  1C     $ERR          IFEQ      'N'
127900950119     C*
128000950119     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
128100030113if  2C     WFPTAS        IFEQ      'S'
128200950120     C                   Z-ADD     D$PAAS        KERAAS
128300950120     C                   Z-ADD     D$PLNP        KERLNP
128400950120     C                   Z-ADD     D$PNRS        KERNRS
128500950120     C                   Z-ADD     D$PNSP        KERNSP
128600950120     C                   MOVEL     D$PTBL        WTBL
128700950120     C                   Z-ADD     D$PMGS        WMGS
128800030113x   2C                   ELSE
128900950120     C                   Z-ADD     D$DAAS        KERAAS
129000950120     C                   Z-ADD     D$DLNP        KERLNP
129100950120     C                   Z-ADD     D$DNRS        KERNRS
129200950120     C                   Z-ADD     D$DNSP        KERNSP
129300950120     C                   MOVEL     D$DTBL        WTBL
129400950120     C                   Z-ADD     D$DMGS        WMGS
129500030113e   2C                   ENDIF
129600950120     C                   EXSR      GESERR
129700950201     C*
129800950201     C* COSTRUISCO LA DATA BOLLA
129900950201     C                   Z-ADD     KERAAS        DSAAS
130000950201     C                   Z-ADD     WMGS          DSMGS
130100020605     c*
130200020605     C* Controlla se lanciare il conto economico se record di TITAS00F
130300020605     C                   Z-ADD     DSDAT8        $TASDSP
130400020605     C                   EXSR      CHKLANTAS
130500950201     C*
130600950201     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
130700950405     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
130800030113if  2C     D$GPGF        IFEQ      'TNSB23R'
130900950405     C     DSDAT8        ANDGT     DTSB23
131000950201     C                   Z-ADD     DSDAT8        DTSB23            8 0
131100030113e   2C                   ENDIF
131200030113     C* NO ERRORI
131300030113if  2C     $ERR          IFEQ      'N'
131400950203     C*
131500020701     C* Azzera la varia "N" immagine -PRIMA-
131600030113if  3C     WFPTAS        IFEQ      'S'
131700020701     c                   EXSR      AZZnCPDTAQ
131800020701     C                   MOVEL     D$PDS1        D$PRE1
131900020701     C                   MOVEL     D$PDS2        D$PRE2
132000030113e   3C                   ENDIF
132100020701     C*
132200020701     C* Azzera la varia "N" immagine -DOPO-
132300030113if  3C     WFDTAS        IFEQ      'S'
132400020701     c                   EXSR      AZZnCDDTAQ
132500020701     C                   MOVEL     D$DDS1        D$DRE1
132600020701     C                   MOVEL     D$DDS2        D$DRE2
132700030113e   3C                   ENDIF
132800030113     C*
132900030113     C* ESTENSIONE: TRANSITI
133000030113     C* LEGGO FIAR5 RECORD TRANSITI E IMPOSTO I CAMPI DELLA DS UNIFICATA
133100040503     C*
133200040517     C* per spesizioni prima del 19/7
133300040622     c                   if        dsdat8 < 20040719
133400040622     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
133500040622     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
133600040622     c* titas.Per quelle > verranno presi dal fiar5
133700040622     C                   Z-ADD     d$pflp        D$PAR5flp
133800040622     C                   Z-ADD     d$pfl2        D$PAR5fl2
133900040622     C                   Z-ADD     d$dflp        D$dAR5flp
134000040622     C                   Z-ADD     d$dfl2        D$dAR5fl2
134100040517     c                   exsr      srTRSold
134200040517     c                   else
134300040517     c                   exsr      srTRSnew
134400040517     c                   end
134500040503     c*
134600040503     c* non prendiamo in considerazioni i bancali e
134700040503     c* contrassegni se INSERIMENTO DI TITAS00F
134800040503     c                   movel     'S'           $rep              1
134900040503if 3ac                   if        d$gsts='I' and d$gfil='TITAS00F'
135000040503     c                   movel     'N'           $rep
135100040503     c                   endif
135200040503     c* per TREC78R deve reperire sempre FIAR5 e TNCSB
135300040503if 3ac                   if        d$gpgf='TREC78R'
135400040503     c                   movel     'S'           $rep
135500040503     c                   endif
135600040503if 3ac                   if        $rep='S'
135700030113     C*
135800040426     c* ATTUALMENTE UNA BOLLA NASCE CON I BANCALI QUINDI NON PREVEDIAMO
135900040426     c* IL CASO DI MODIFICHE DEL FLAG TASGVA SUCCESSIVE
136000030113     C* ESTENSIONE: BANCALI
136100030113     C* LEGGO FIAR5 RECORD BANCALI E IMPOSTO I CAMPI DELLA DS UNIFICATA
136200040123    3c                   if        %subst(d$pgva: 1: 1) = 'B' or
136300040123     c                             %subst(d$dgva: 1: 1) = 'B'
136400040123if  4C     WFPTAS        IFEQ      'S'
136500030113     C                   Z-ADD     D$PAAS        AR5AAS
136600030113     C                   Z-ADD     D$PLNP        AR5LNP
136700030113     C                   Z-ADD     D$PNRS        AR5NRS
136800030113     C                   Z-ADD     D$PNSP        AR5NSP
136900040123x   4C                   ELSE
137000030113     C                   Z-ADD     D$DAAS        AR5AAS
137100030113     C                   Z-ADD     D$DLNP        AR5LNP
137200030113     C                   Z-ADD     D$DNRS        AR5NRS
137300030113     C                   Z-ADD     D$DNSP        AR5NSP
137400040123e   4C                   ENDIF
137500040123     C                   MOVEL     'BAN'         AR5TRD
137600040521     c                   clear                   ar5prg
137700040617     C     KEYAR5        CHAIN     FIAR531C                           99
137800040123if  4C     *IN99         IFEQ      '0'
137900040617     C     AR5ATB        andeq     *BLANKS
138000030113     C*
138100040617if  5C                   IF        WFPBAN = 'S'
138200030113     C                   MOVEL     AR5UNI        DAR5BAN
138300030113     C                   Z-ADD     §AR5TBA       D$PAR5TBA
138400030113     C                   Z-ADD     §AR5NBA       D$PAR5NBA
138500030113     C                   Z-ADD     §AR5TB2       D$PAR5TB2
138600030113     C                   Z-ADD     §AR5NB2       D$PAR5NB2
138700040617e   5C                   ENDIF
138800030113     C*
138900040617if  5C                   IF        WFDBAN = 'S'
139000030113     C                   MOVEL     AR5UNI        DAR5BAN
139100030113     C                   Z-ADD     §AR5TBA       D$DAR5TBA
139200030113     C                   Z-ADD     §AR5NBA       D$DAR5NBA
139300030113     C                   Z-ADD     §AR5TB2       D$DAR5TB2
139400030113     C                   Z-ADD     §AR5NB2       D$DAR5NB2
139500040123e   5C                   ENDIF
139600040123e   4C                   ENDIF
139700040123e   3C                   ENDIF
139800030801     C*
139900030801     C* ESTENSIONE: BANCALI BARTOLINI
140000030801     C* LEGGO FIAR5 RECORD BANCALI BARTOLINI E IMPOSTO CAMPI DELLA DS UNIFICATA
140100040123    3c                   if        %subst(d$pgva: 1: 1) = 'O' or
140200040123     c                             %subst(d$dgva: 1: 1) = 'O'
140300040123if  4C     WFPTAS        IFEQ      'S'
140400030801     C                   Z-ADD     D$PAAS        AR5AAS
140500030801     C                   Z-ADD     D$PLNP        AR5LNP
140600030801     C                   Z-ADD     D$PNRS        AR5NRS
140700030801     C                   Z-ADD     D$PNSP        AR5NSP
140800040123x   4C                   ELSE
140900030801     C                   Z-ADD     D$DAAS        AR5AAS
141000030801     C                   Z-ADD     D$DLNP        AR5LNP
141100030801     C                   Z-ADD     D$DNRS        AR5NRS
141200030801     C                   Z-ADD     D$DNSP        AR5NSP
141300040123e   4C                   ENDIF
141400040123     C                   MOVEL     'BNB'         AR5TRD
141500040521     c                   clear                   ar5prg
141600040617     C     KEYAR5        CHAIN     FIAR531C                           99
141700040123if  4C     *IN99         IFEQ      '0'
141800040617     C     AR5ATB        andeq     *BLANKS
141900030801     C*
142000040617if  5C                   IF        WFPBAN = 'S'
142100030801     C                   MOVEL     AR5UNI        DAR5BNB
142200030801     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
142300030801     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
142400030801     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
142500030801     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
142600040218     C                   Z-ADD     §AR5BCSF      D$PAR5CSF
142700040218     C                   Z-ADD     §AR5BCOR      D$PAR5COR
142800040617e   5C                   ENDIF
142900030729     C*
143000040617if  5C                   IF        WFDBAN = 'S'
143100030729     C                   MOVEL     AR5UNI        DAR5BNB
143200030729     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
143300030801     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
143400030729     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
143500030801     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
143600040218     C                   Z-ADD     §AR5BCSF      D$DAR5CSF
143700040218     C                   Z-ADD     §AR5BCOR      D$DAR5COR
143800040123e   5C                   ENDIF
143900040123e   4C                   ENDIF
144000040123e   3C                   ENDIF
144100030113     C*
144200030113     C* CONTRASSEGNO
144300030113     C* LEGGO TNCSB SOLO SE FLAG CONTRASSEGNO SUL TITAS = S
144400030113if  3C     D$PFCA        IFEQ      'S'
144500030113     C     D$DFCA        OREQ      'S'
144600030113     C*
144700030113     C* LEGGO TNCSB00F E IMPOSTO I CAMPI DELLA DS UNIFICATA
144800030113if  4C     WFPTAS        IFEQ      'S'
144900030113     C                   Z-ADD     D$PAAS        CSBAAS
145000030113     C                   Z-ADD     D$PLNP        CSBLNP
145100030113     C                   Z-ADD     D$PNRS        CSBNRS
145200030113     C                   Z-ADD     D$PNSP        CSBNSP
145300030113x   4C                   ELSE
145400030113     C                   Z-ADD     D$DAAS        CSBAAS
145500030113     C                   Z-ADD     D$DLNP        CSBLNP
145600030113     C                   Z-ADD     D$DNRS        CSBNRS
145700030113     C                   Z-ADD     D$DNSP        CSBNSP
145800030113e   4C                   ENDIF
145900030113     C     KEYCSB        CHAIN     TNCSB03L                           99
146000030113if  4C     *IN99         IFEQ      '0'
146100040521if  7C     WFPCSB        IFEQ      'S'
146200040521     C                   MOVEL     CSBsta        D$Psta
146300040521     c                   end
146400040521if  7C     WFdCSB        IFEQ      'S'
146500040521     C                   MOVEL     CSBsta        D$dsta
146600040521     c                   end
146700030113     C* CONTROLLO ANNULLAMENTO
146800030113if  5C     CSBSTA        IFNE      9
146900030113     C*
147000040123    6C     D$PFCA        ifeq      'S'
147100040521if  7C     WFPCSB        andeq     'S'
147200030113     C                   MOVEL     CSBTPI        D$PTPI
147300030113if  7C     CSBVCA        IFNE      WDIVCE
147400030113     C                   Z-ADD     CSBCAS        WIMI
147500030113     C                   MOVEL     CSBVCA        WDVI
147600030113     C                   EXSR      CNVEUR
147700030113     C                   Z-ADD     WIMO          D$PCAS
147800030113x   7C                   ELSE
147900030113     C                   Z-ADD     CSBCAS        D$PCAS
148000030113e   7C                   ENDIF
148100030113e   6C                   ENDIF
148200030113     C*
148300040123    6C     D$DFCA        ifeq      'S'
148400040521if  7C     WFdCSB        andeq     'S'
148500030113     C                   MOVEL     CSBTPI        D$DTPI
148600030113if  7C     CSBVCA        IFNE      WDIVCE
148700030113     C                   Z-ADD     CSBCAS        WIMI
148800030113     C                   MOVEL     CSBVCA        WDVI
148900030113     C                   EXSR      CNVEUR
149000030113     C                   Z-ADD     WIMO          D$DCAS
149100030113x   7C                   ELSE
149200030113     C                   Z-ADD     CSBCAS        D$DCAS
149300030113e   7C                   ENDIF
149400030113e   6C                   ENDIF
149500030113e   5C                   ENDIF
149600030113e   4C                   ENDIF
149700030113e   3C                   ENDIF
149800040123e  3aC                   ENDIF
149900030113     C*
150000030113e   2C                   ENDIF
150100030113e   1C                   ENDIF
150200941129     C*
150300941129     C                   ENDSR
150400940824     C***********************************************************************
150500040517     C*   srTRSold - per spediz. con data < 20040719
150600940824     C***********************************************************************
150700040517     C     srTRSold      BEGSR
150800040517     C* per update e delete di TITAS00F e TITAS10F attendo arrivo di FIAR5
150900040517     c* per insert di TITAS00F attendo arrivo di FIAR5
151000040517     c* per insert di TITAS10F devo reperire FIAR5 xchè non arriva
151100040517     C*
151200040517     C                   IF        ((d$gsts='U' OR d$gsts='D') AND
151300040517    3C                             ((D$PFLP <> D$DFLP AND D$PFLP <> 0) OR
151400040517    3C                             (D$PFL2 <> D$DFL2 AND D$PFL2 <> 0)))
151500040517     C                             OR (d$gsts='I' and d$gfil='TITAS10F')
151600040517     C                             OR (d$gpgf='TREC78R')
151700040517if  4C     WFPTAS        IFEQ      'S'
151800040517     C                   Z-ADD     D$PAAS        AR5AAS
151900040517     C                   Z-ADD     D$PLNP        AR5LNP
152000040517     C                   Z-ADD     D$PNRS        AR5NRS
152100040517     C                   Z-ADD     D$PNSP        AR5NSP
152200040517x   4C                   ELSE
152300040517     C                   Z-ADD     D$DAAS        AR5AAS
152400040517     C                   Z-ADD     D$DLNP        AR5LNP
152500040517     C                   Z-ADD     D$DNRS        AR5NRS
152600040517     C                   Z-ADD     D$DNSP        AR5NSP
152700040517e   4C                   ENDIF
152800040517     c*
152900040517     C                   MOVEL     'TRS'         AR5TRD
153000040521     c                   clear                   ar5prg
153100040617     C     KEYAR5        CHAIN     FIAR531C                           99
153200040517if  4C     *IN99         IFEQ      '0'
153300040617     C     AR5ATB        andeq     *BLANKS
153400040617if  5C     WFPTRS        ifeq      'S'
153500040519     c                   exsr      srar5trsp
153600040519e   5C                   ENDIF
153700040617if  5C     WFDTRS        ifeq      'S'
153800040519     c                   exsr      srar5trsd
153900040519e   5C                   ENDIF
154000040517e   4C                   ENDIF
154100040517     C*
154200040517E   3C                   ENDIF
154300940824     C*
154400040517     C                   ENDSR
154500040517     C***********************************************************************
154600040517     C*   srTRSnew - per spediz. con data >=20040719
154700040517     c* recepisco solo le variazioni del titas (tasflp = progressivo di
154800040517     c* fiar5)
154900040517     C***********************************************************************
155000040517     C     srTRSnew      BEGSR
155100040517     C*
155200040517     C                   MOVEL     'TRS'         AR5TRD
155300040517     c                   clear                   ar5uni
155400040517     c* prima
155500040617if  1C     WFPTAS        IFEQ      'S'
155600040517     C                   Z-ADD     D$PAAS        AR5AAS
155700040517     C                   Z-ADD     D$PLNP        AR5LNP
155800040517     C                   Z-ADD     D$PNRS        AR5NRS
155900040517     C                   Z-ADD     D$PNSP        AR5NSP
156000040517     C                   Z-ADD     D$Pflp        AR5prg
156100040617     C     KEYAR5        CHAIN     FIAR531C                           99
156200040617if  2C     *IN99         IFEQ      '0'
156300040617     C     AR5ATB        andeq     *BLANKS
156400040617     C     WFPTRS        andeq     'S'
156500040519     c                   exsr      srar5trsp
156600040617e   2C                   ENDIF
156700040617e   1C                   Endif
156800040517     c* dopo
156900040517     c                   clear                   ar5uni
157000040617if  1C     WFDTAS        IFEQ      'S'
157100040517     C                   Z-ADD     D$DAAS        AR5AAS
157200040517     C                   Z-ADD     D$DLNP        AR5LNP
157300040517     C                   Z-ADD     D$DNRS        AR5NRS
157400040517     C                   Z-ADD     D$DNSP        AR5NSP
157500040517     C                   Z-ADD     D$Dflp        AR5prg
157600040617     C     KEYAR5        CHAIN     FIAR531C                           99
157700040617if  2C     *IN99         IFEQ      '0'
157800040617     C     AR5ATB        andeq     *BLANKS
157900040617     C     WFDTRS        andeq     'S'
158000040519     c                   exsr      srar5trsd
158100040617e   2C                   ENDIF
158200040617e   1C                   ENDIF
158300040517     C*
158400040517     C                   ENDSR
158500040517     C***********************************************************************
158600040519     C* srar5trsp - riempe i campi della ds$eco x transiti immagine prima
158700040517     C***********************************************************************
158800040519     C     srar5trsp     BEGSR
158900040519     C*
159000040519     C                   MOVEL     AR5UNI        DAR5TRS
159100040622     c                   if        dsdat8 >=20040719
159200040519     C                   Z-ADD     §AR5flp       D$PAR5flp
159300040519     C                   Z-ADD     §AR5fl2       D$PAR5fl2
159400040524     C                   Z-ADD     §AR5fl3       D$PAR5fl3
159500040524     C                   Z-ADD     §AR5DE3       D$PAR5DE3
159600040524     C                   Z-ADD     §AR5DU3       D$PAR5DU3
159700040524     C                   Z-ADD     §AR5fl4       D$PAR5fl4
159800040524     C                   Z-ADD     §AR5DE4       D$PAR5DE4
159900040524     C                   Z-ADD     §AR5DU4       D$PAR5DU4
160000040524     C                   Z-ADD     §AR5fl5       D$PAR5fl5
160100040524     C                   Z-ADD     §AR5DE5       D$PAR5DE5
160200040524     C                   Z-ADD     §AR5DU5       D$PAR5DU5
160300040519     c                   end
160400040519     C                   Z-ADD     §AR5DET       D$PAR5DET
160500040519     C                   Z-ADD     §AR5DUT       D$PAR5DUT
160600040519     C                   Z-ADD     §AR5DE2       D$PAR5DE2
160700040519     C                   Z-ADD     §AR5DU2       D$PAR5DU2
160800040519     C*
160900040519     C                   ENDSR
161000040519     C***********************************************************************
161100040519     C* srar5trsp - riempe i campi della ds$eco x transiti immagine dopo
161200040519     C***********************************************************************
161300040519     C     srar5trsd     BEGSR
161400040519     C*
161500040519     C                   MOVEL     AR5UNI        DAR5TRS
161600040622     c                   if        dsdat8 >=20040719
161700040519     C                   Z-ADD     §AR5flp       D$DAR5flp
161800040519     C                   Z-ADD     §AR5fl2       D$DAR5fl2
161900040524     C                   Z-ADD     §AR5fl3       D$DAR5fl3
162000040524     C                   Z-ADD     §AR5DE3       D$DAR5DE3
162100040524     C                   Z-ADD     §AR5DU3       D$DAR5DU3
162200040524     C                   Z-ADD     §AR5fl4       D$DAR5fl4
162300040524     C                   Z-ADD     §AR5DE4       D$DAR5DE4
162400040524     C                   Z-ADD     §AR5DU4       D$DAR5DU4
162500040524     C                   Z-ADD     §AR5fl5       D$DAR5fl5
162600040524     C                   Z-ADD     §AR5DE5       D$DAR5DE5
162700040524     C                   Z-ADD     §AR5DU5       D$DAR5DU5
162800040519     c                   end
162900040519     C                   Z-ADD     §AR5DET       D$DAR5DET
163000040519     C                   Z-ADD     §AR5DUT       D$DAR5DUT
163100040519     C                   Z-ADD     §AR5DE2       D$DAR5DE2
163200040519     C                   Z-ADD     §AR5DU2       D$DAR5DU2
163300040519     C*
163400040519     C                   ENDSR
163500040519     C***********************************************************************
163600040519     C*   GESTBL - GESTIONE TIPO BOLLA PRIMA E DOPO
163700040519     C***********************************************************************
163800040519     C     GESTBL        BEGSR
163900040517     C*
164000940824     C* TIPO PORTO  E  RAGGRUPPAMENTO BOLLE DA TABELLA 'TB'     PRIMA
164100970507IF  1C     D$PTBL        IFNE      *BLANKS
164200941130     C                   Z-ADD     1             I                 6 0
164300940824     C     D$PTBL        LOOKUP    TBL(I)                                 99
164400970507IF  2C     *IN99         IFEQ      '0'
164500940824     C                   MOVEL     MSG(4)        $MSG
164600941026     C                   EXSR      SCRERR
164700970507X   2C                   ELSE
164800941130     C                   MOVEL     TPO(I)        D$PTPO
164900941130     C                   MOVEL     RBL(I)        D$PRBL
165000941130     C                   MOVEL     CSF(I)        D$PCSF
165100941130     C                   MOVEL     CCS(I)        D$PCCS
165200941130     C                   MOVEL     DBO(I)        D$PDBO
165300940915     C* RECUPERO FLAG RICAVI E FLAG SPEDIZIONI
165400940824     C                   Z-ADD     1             I
165500941130     C     D$PRBL        LOOKUP    FRB(I)                                 99
165600940824     C     *IN99         IFEQ      '0'
165700940824     C                   MOVEL     MSG(12)       $MSG
165800941026     C                   EXSR      SCRERR
165900940824     C                   ELSE
166000941130     C                   MOVEL     FRI(I)        D$PFRI
166100941130     C                   MOVEL     FSP(I)        D$PFSP
166200940824     C                   END
166300970507E   2C                   END
166400970507E   1C                   ENDIF
166500940824     C*
166600940824     C*
166700940824     C* TIPO PORTO  E  RAGGRUPPAMENTO BOLLE DA TABELLA 'TB'     DOPO
166800970507IF  1C     D$DTBL        IFNE      *BLANKS
166900940824     C                   Z-ADD     1             I
167000940824     C     D$DTBL        LOOKUP    TBL(I)                                 99
167100940824     C     *IN99         IFEQ      '0'
167200940824     C                   MOVEL     MSG(4)        $MSG
167300941026     C                   EXSR      SCRERR
167400940824     C                   ELSE
167500941130     C                   MOVEL     TPO(I)        D$DTPO
167600941130     C                   MOVEL     RBL(I)        D$DRBL
167700941130     C                   MOVEL     CSF(I)        D$DCSF
167800941130     C                   MOVEL     CCS(I)        D$DCCS
167900941130     C                   MOVEL     DBO(I)        D$DDBO
168000940915     C* RECUPERO FLAG RICAVI E SPEDIZIONI
168100940824     C                   Z-ADD     1             I
168200941130     C     D$DRBL        LOOKUP    FRB(I)                                 99
168300940824     C     *IN99         IFEQ      '0'
168400940824     C                   MOVEL     MSG(12)       $MSG
168500941026     C                   EXSR      SCRERR
168600940824     C                   ELSE
168700941130     C                   MOVEL     FRI(I)        D$DFRI
168800941130     C                   MOVEL     FSP(I)        D$DFSP
168900940824     C                   END
169000940824     C                   END
169100970507E   1C                   ENDIF
169200940824     C*
169300940824     C                   ENDSR
169400020605     C*-------------------------------------------------------------------------
169500020605     C* Controlla se lanciare il conto economico se record di TITAS00F
169600020605     C*-------------------------------------------------------------------------
169700020605     C     CHKLANTAS     BEGSR
169800020605     C*---
169900020605     C* non lancia il conto economico SE:
170000020605     C* . modifica / cancellazione di TITAS00F con data prima del giugno 2002
170100020605     C*---
170200020605IF  1C     D$GFIL        IFEQ      'TITAS00F'
170300020619     C     $TASDSP       ANDLT     20020601
170400020605     C                   MOVEL     'S'           $ERR
170500020605E   1C                   ENDIF
170600020605     C*
170700020605     C                   ENDSR
170800940808     C***********************************************************************
170900940808     C*   GESLEG - GESTIONE LEGAMI BOLLA
171000940808     C***********************************************************************
171100940808     C     GESLEG        BEGSR
171200940808     C*
171300950905     C                   MOVEL     'N'           D$PFIG
171400950905     C                   MOVEL     'N'           D$DFIG
171500950905     C*
171600950905     C* SE E' BOLLA DI RIENTRO NON LA CONSIDERO COME LEGATA
171700950905     C     D$PFBR        IFEQ      'S'
171800950905     C     D$DFBR        ANDEQ     'S'
171900950915     C********** D$GPGF    ANDNE'TRECE9R'                  *CANC
172000950905     C                   ELSE
172100950905     C*
172200990804     C* SE FLAG 'HA LEGAMI' IN TITAS = 'SI' LEGGO SUL FILE LEGAMI
172300941007     C     D$PFA1        IFEQ      'S'
172400941007     C     D$DFA1        OREQ      'S'
172500941128     C* SE INSERIMENTO LEGGO CON DS DOPO ALTRIMENTI CON DS PRIMA
172600941129     C     WFDTAS        IFEQ      'S'
172700940808     C                   Z-ADD     D$DAAS        LBLAAN
172800940808     C                   Z-ADD     D$DLNP        LBLLPN
172900940808     C                   Z-ADD     D$DNRS        LBLNRN
173000940808     C                   Z-ADD     D$DNSP        LBLNSN
173100940808     C                   ELSE
173200940808     C                   Z-ADD     D$PAAS        LBLAAN
173300940808     C                   Z-ADD     D$PLNP        LBLLPN
173400940808     C                   Z-ADD     D$PNRS        LBLNRN
173500940808     C                   Z-ADD     D$PNSP        LBLNSN
173600940808     C                   ENDIF
173700941130     C     KEYLBL        CHAIN     FNLBL01L                           99
173800941007     C     *IN99         IFEQ      '0'
173900950905     C*
174000950915     C********** D$GPGF    IFEQ 'TRECE9R'                  *CANC
174100950915     C******************** MOVEL'S'       D$PFIG           *CANC
174200950915     C******************** ELSE                            *CANC
174300950905     C     D$PFBR        IFNE      'S'
174400950905     C                   MOVEL     'S'           D$PFIG
174500950905     C                   ENDIF
174600950915     C******************** ENDIF                           *CANC
174700950905     C*
174800950905     C     D$DFBR        IFNE      'S'
174900950905     C                   MOVEL     'S'           D$DFIG
175000950905     C                   ENDIF
175100950905     C*
175200940808     C* CICLO PER TROVARE LA BOLLA ORIGINALE (LA PRIMA MAMMA)
175300940808     C     *IN99         DOUEQ     '1'
175400940808     C                   Z-ADD     LBLAAP        LBLAAN
175500940808     C                   Z-ADD     LBLLPP        LBLLPN
175600940808     C                   Z-ADD     LBLNRP        LBLNRN
175700940808     C                   Z-ADD     LBLNSP        LBLNSN
175800941128     C     KEYLBL        CHAIN     FNLBL01L                           99
175900940808     C                   ENDDO
176000940808     C*
176100940808     C* LEGGO I DATI DELLA MAMMA
176200941128     C                   Z-ADD     LBLAAP        TASAAS
176300940808     C                   Z-ADD     LBLLPP        TASLNP
176400940808     C                   Z-ADD     LBLNRP        TASNRS
176500940808     C                   Z-ADD     LBLNSP        TASNSP
176600941213     C                   MOVEL     *BLANKS       TASTBL
176700990804     C     KEYTAS        SETLL     TITAS30C                           99
176800990804     C  N99              READ      TITAS30C                               99
176900941212     C     *IN99         IFEQ      '0'
177000941212     C     TASAAS        ANDEQ     LBLAAP
177100940809     C     TASLNP        ANDEQ     LBLLPP
177200940809     C     TASNRS        ANDEQ     LBLNRP
177300940809     C     TASNSP        ANDEQ     LBLNSP
177400941129     C* CARICO LA DS BOLLA ORIGINALE
177500941128     C                   EXSR      CODTAQ
177600020621     C                   EXSR      AZZnCODTAQ
177700941128     C                   MOVEL     D$ODS1        D$ORE1
177800941128     C                   MOVEL     D$ODS2        D$ORE2
177900990804     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
178000020527     C                   Z-ADD     *ZEROS        V                 6 0
178100941129     C     D$OSV1        IFNE      *BLANKS
178200941129     C                   ADD       1             V
178300941129     C                   MOVEL     D$OSV1        SVN(V)
178400011213     C                   Z-ADD     D$OVA1        WVAR             11 3
178500990806     C                   MOVE      WVAR          VAN(V)
178600941129     C                   ENDIF
178700941129     C     D$OSV2        IFNE      *BLANKS
178800941129     C                   ADD       1             V
178900941129     C                   MOVEL     D$OSV2        SVN(V)
179000011213     C                   Z-ADD     D$OVA2        WVAR
179100990806     C                   MOVE      WVAR          VAN(V)
179200941129     C                   ENDIF
179300941129     C     D$OSV3        IFNE      *BLANKS
179400941129     C                   ADD       1             V
179500941129     C                   MOVEL     D$OSV3        SVN(V)
179600011213     C                   Z-ADD     D$OVA3        WVAR
179700990806     C                   MOVE      WVAR          VAN(V)
179800941129     C                   ENDIF
179900990804     C* CARICO LE VARIE BOLLA ORIGINALE DA TITA7 (SOLO SE C'E' LA VARIA 3)
180000941129     C     D$OSV3        IFNE      *BLANKS
180100941130     C                   Z-ADD     D$OAAS        TA7AAS
180200941130     C                   Z-ADD     D$OLNP        TA7LNP
180300941130     C                   Z-ADD     D$ONRS        TA7NRS
180400941130     C                   Z-ADD     D$ONSP        TA7NSP
180500941130     C                   MOVEL     D$OTBL        TA7TBL
180600990806     C     KEYTA7        SETLL     TITA730C                           99
180700990806     C  N99KEYTA7        READE     TITA730C                               99
180800941202     C     *IN99         DOWEQ     *OFF
180900020621     C                   EXSR      AZZnTA7
181000941129     C                   ADD       1             V
181100941129     C                   MOVEL     TA7SVN        SVN(V)
181200011213     C                   Z-ADD     TA7VAN        WVAR
181300990806     C                   MOVE      WVAR          VAN(V)
181400990806     C  N99KEYTA7        READE     TITA730C                               99
181500941129     C                   ENDDO
181600941129     C*
181700941129     C                   MOVEA     SVN           D$OSVN
181800941129     C                   MOVEA     VAN           D$OVAN
181900941129     C                   ENDIF
182000941212     C*
182100990804     C                   ELSE
182200941212     C*
182300941212     C                   MOVEL     MSG(11)       $MSG
182400941212     C                   EXSR      SCRERR
182500941212     C*
182600941129     C                   ENDIF
182700940901     C*
182800941007     C                   ENDIF
182900941007     C                   ENDIF
183000950905     C                   ENDIF
183100940808     C*
183200940808     C                   ENDSR
183300941129     C***********************************************************************
183400941129     C*   GESVAR - GESTIONE VARIE BOLLA PRIMA E DOPO
183500941129     C***********************************************************************
183600941129     C     GESVAR        BEGSR
183700941129     C*
183800990804     C* LEGGO VARIE DA TITA7
183900941129     C                   EXSR      LETTA7
184000941129     C*
184100990804     C* SE IMMAGINE PRIMA DI TITA7 DA IMPOSTARE
184200941129     C     WFPALT        IFEQ      'S'
184300990804     C* IMPOSTO VARIE DI TITA7
184400941129     C                   MOVEA     SV7           SVN
184500941129     C                   MOVEA     VA7           VAN
184600941129     C* AGGIUNGO VARIE BOLLA PRIMA
184700941129     C                   Z-ADD     WULT7         V
184800941129     C     D$PSV1        IFNE      *BLANKS
184900941129     C                   ADD       1             V
185000941129     C                   MOVEL     D$PSV1        SVN(V)
185100011213     C                   Z-ADD     D$PVA1        WVAR
185200990806     C                   MOVE      WVAR          VAN(V)
185300941129     C                   ENDIF
185400941129     C     D$PSV2        IFNE      *BLANKS
185500941129     C                   ADD       1             V
185600941129     C                   MOVEL     D$PSV2        SVN(V)
185700011213     C                   Z-ADD     D$PVA2        WVAR
185800990806     C                   MOVE      WVAR          VAN(V)
185900941129     C                   ENDIF
186000941129     C     D$PSV3        IFNE      *BLANKS
186100941129     C                   ADD       1             V
186200941129     C                   MOVEL     D$PSV3        SVN(V)
186300011213     C                   Z-ADD     D$PVA3        WVAR
186400990806     C                   MOVE      WVAR          VAN(V)
186500941129     C                   ENDIF
186600941129     C* IMPOSTO VARIE PRIMA NELLA DS UNIFICATA
186700941129     C                   MOVEA     SVN           D$PSVN
186800941129     C                   MOVEA     VAN           D$PVAN
186900941129     C                   ENDIF
187000941129     C*
187100990804     C* SE IMMAGINE DOPO DI TITA7 DA GESTIRE
187200941129     C     WFDALT        IFEQ      'S'
187300990804     C* IMPOSTO VARIE DI TITA7
187400941129     C                   MOVEA     SV7           SVN
187500941129     C                   MOVEA     VA7           VAN
187600941212     C* AGGIUNGO VARIE BOLLA DOPO
187700941129     C                   Z-ADD     WULT7         V
187800941129     C     D$DSV1        IFNE      *BLANKS
187900941129     C                   ADD       1             V
188000941129     C                   MOVEL     D$DSV1        SVN(V)
188100011213     C                   Z-ADD     D$DVA1        WVAR
188200990806     C                   MOVE      WVAR          VAN(V)
188300941129     C                   ENDIF
188400941129     C     D$DSV2        IFNE      *BLANKS
188500941129     C                   ADD       1             V
188600941129     C                   MOVEL     D$DSV2        SVN(V)
188700011213     C                   Z-ADD     D$DVA2        WVAR
188800990806     C                   MOVE      WVAR          VAN(V)
188900941129     C                   ENDIF
189000941129     C     D$DSV3        IFNE      *BLANKS
189100941129     C                   ADD       1             V
189200941129     C                   MOVEL     D$DSV3        SVN(V)
189300011213     C                   Z-ADD     D$DVA3        WVAR
189400990806     C                   MOVE      WVAR          VAN(V)
189500941129     C                   ENDIF
189600941212     C* IMPOSTO DOPO PRIMA NELLA DS UNIFICATA
189700941129     C                   MOVEA     SVN           D$DSVN
189800941129     C                   MOVEA     VAN           D$DVAN
189900941129     C                   ENDIF
190000941129     C*
190100941129     C                   ENDSR
190200941129     C***********************************************************************
190300990804     C* LETTA7 - CARICA LE SIGLE E GLI IMPORTI DELLE VARIE DA TITA7
190400941129     C***********************************************************************
190500941129     C     LETTA7        BEGSR
190600941129     C*
190700941129     C                   Z-ADD     *ZEROS        WULT7             6 0
190800941129     C                   MOVEA     *BLANKS       SV7
190900941129     C                   MOVEA     *ZEROS        VA7
191000941129     C* CARICO LE VARIE ULTERIORI (UGUALE PER -PRIMA- E -DOPO-)
191100941129IF  1C     D$DSV3        IFNE      *BLANKS
191200941129     C     D$PSV3        ORNE      *BLANKS
191300941129     C* P R I M A
191400941129IF  2C     WFPTAS        IFEQ      'S'
191500941130     C                   Z-ADD     D$PAAS        TA7AAS
191600941130     C                   Z-ADD     D$PLNP        TA7LNP
191700941130     C                   Z-ADD     D$PNRS        TA7NRS
191800941130     C                   Z-ADD     D$PNSP        TA7NSP
191900941130     C                   MOVEL     D$PTBL        TA7TBL
192000941129X   2C                   ELSE
192100941130     C                   Z-ADD     D$DAAS        TA7AAS
192200941130     C                   Z-ADD     D$DLNP        TA7LNP
192300941130     C                   Z-ADD     D$DNRS        TA7NRS
192400941130     C                   Z-ADD     D$DNSP        TA7NSP
192500941130     C                   MOVEL     D$DTBL        TA7TBL
192600941129E   2C                   ENDIF
192700941129     C* CARICA VARIE
192800941129     C                   Z-ADD     *ZEROS        V
192900990806     C     KEYTA7        SETLL     TITA730C                           99
193000990806     C  N99KEYTA7        READE     TITA730C                               99
193100941202DO  2C     *IN99         DOWEQ     *OFF
193200020621     C                   EXSR      AZZnTA7
193300941129     C                   ADD       1             V
193400941129     C                   MOVEL     TA7SVN        SV7(V)
193500011213     C                   Z-ADD     TA7VAN        WVAR
193600990806     C                   MOVE      WVAR          VA7(V)
193700990806     C  N99KEYTA7        READE     TITA730C                               99
193800941129E   2C                   ENDDO
193900941129     C*
194000941129     C                   Z-ADD     V             WULT7
194100941129E   1C                   ENDIF
194200941129     C*
194300941129     C                   ENDSR
194400941129     C***********************************************************************
194500941129     C* GESASC - GESTIONE ASSICURAZIONE PER CONTO
194600941129     C***********************************************************************
194700011227     C     GESASC        BEGSR
194800011227     C*
194900011227     C* -PRIMA-
195000011227IF  1C     WFPALT        IFEQ      'S'
195100011227IF  2C     D$PIAS        IFNE      *ZEROS
195200011227IF  3C     D$PVAS        IFNE      WDIVCE
195300011227     C                   Z-ADD     D$PIAS        WIMI
195400011227     C                   MOVEL     D$PVAS        WDVI
195500011227     C                   EXSR      CNVEUR
195600011227     C                   Z-ADD     WIMO          D$PVAE
195700011227X   3C                   ELSE
195800011227     C                   Z-ADD     D$PIAS        D$PVAE
195900011227E   3C                   ENDIF
196000011227E   2C                   ENDIF
196100011227E   1C                   ENDIF
196200011227     C*
196300011227     C* -DOPO-
196400011227IF  1C     WFDALT        IFEQ      'S'
196500011227IF  2C     D$DIAS        IFNE      *ZEROS
196600011227IF  3C     D$DVAS        IFNE      WDIVCE
196700011227     C                   Z-ADD     D$DIAS        WIMI
196800011227     C                   MOVEL     D$DVAS        WDVI
196900011227     C                   EXSR      CNVEUR
197000011227     C                   Z-ADD     WIMO          D$DVAE
197100011227X   3C                   ELSE
197200011227     C                   Z-ADD     D$DIAS        D$DVAE
197300011227E   3C                   ENDIF
197400011227E   2C                   ENDIF
197500011227E   1C                   ENDIF
197600011227     C*
197700011227     C                   ENDSR
197800941129     C***********************************************************************
197900951213     C* GESSTP - GESTIONE NUMERO DI STOP (RITIRO/CONSEGNA)
198000941129     C***********************************************************************
198100941129     C     GESSTP        BEGSR
198200950203     C*
198300951213     C* ====== RITIRO ========
198400951213     C*
198500951213     C* IMPOSTA NUMERO DI STOP RITIRO (FRAZIONE DELLA QUANTITA' DA ADDEBITARE)
198600950203     C* (DA 0,001 A 1 DI 6.000 LIRE)
198700950203     C* PRIMA
198800950203     C     WFPALT        IFEQ      'S'
198900950203     C                   Z-ADD     1             D$PNSR
199000950203     C     D$PSTR        IFGT      1
199100950203     C     1             DIV       D$PSTR        D$PNSR
199200011213     C     D$PNSR        IFLT      0,00001
199300011213     C                   Z-ADD     0,00001       D$PNSR
199400950203     C                   ENDIF
199500950203     C                   ENDIF
199600950203     C                   ENDIF
199700950203     C* DOPO
199800950203     C     WFDALT        IFEQ      'S'
199900950203     C                   Z-ADD     1             D$DNSR
200000950203     C     D$DSTR        IFGT      1
200100950203     C     1             DIV       D$DSTR        D$DNSR
200200011213     C     D$DNSR        IFLT      0,00001
200300011213     C                   Z-ADD     0,00001       D$DNSR
200400950203     C                   ENDIF
200500950203     C                   ENDIF
200600950203     C                   ENDIF
200700951213     C*
200800951213     C* ====== CONSEGNA =======
200900951213     C*
201000951213     C* IMPOSTA NUMERO DI STOP CONS. (FRAZIONE DELLA QUANTITA' DA ADDEBITARE)
201100951213     C* (DA 0,001 A 1 DI 6.000 LIRE)
201200951213     C*
201300951213     C* PRIMA
201400951213     C     WFPALT        IFEQ      'S'
201500951213     C                   Z-ADD     1             D$PNSC
201600951213     C     D$PSTP        IFGT      1
201700951213     C     1             DIV       D$PSTP        D$PNSC
201800011213     C     D$PNSC        IFLT      0,00001
201900011213     C                   Z-ADD     0,00001       D$PNSC
202000951213     C                   ENDIF
202100951213     C                   ENDIF
202200951213     C                   ENDIF
202300951213     C*
202400951213     C* DOPO
202500951213     C     WFDALT        IFEQ      'S'
202600951213     C                   Z-ADD     1             D$DNSC
202700951213     C     D$DSTP        IFGT      1
202800951213     C     1             DIV       D$DSTP        D$DNSC
202900011213     C     D$DNSC        IFLT      0,00001
203000011213     C                   Z-ADD     0,00001       D$DNSC
203100951213     C                   ENDIF
203200951213     C                   ENDIF
203300951213     C                   ENDIF
203400950203     C*
203500950203     C                   ENDSR
203600030113     C***********************************************************************
203700030113     C* GESGCP - GESTIONE GIACENZA
203800030113     C***********************************************************************
203900030113     C     GESGCP        BEGSR
204000030113     C*
204100030113     C* LEGGE LA GIACENZA SOLO SE AVUTA
204200030221IF  1C     D$DFGC        IFEQ      'S'
204300030221     C     D$PFGC        OREQ      'S'
204400030113     C* PRIMA
204500030113IF  2C     WFPTAS        IFEQ      'S'
204600030113     C                   Z-ADD     D$PAAS        GCPAAS
204700030113     C                   Z-ADD     D$PLNP        GCPLNP
204800030113     C                   Z-ADD     D$PNRS        GCPNRS
204900030113     C                   Z-ADD     D$PNSP        GCPNSP
205000030113     C                   Z-ADD     *ZEROS        GCPFRG
205100030113     C* DOPO
205200030113X   2C                   ELSE
205300030113     C                   Z-ADD     D$DAAS        GCPAAS
205400030113     C                   Z-ADD     D$DLNP        GCPLNP
205500030113     C                   Z-ADD     D$DNRS        GCPNRS
205600030113     C                   Z-ADD     D$DNSP        GCPNSP
205700030113     C                   Z-ADD     *ZEROS        GCPFRG
205800030113E   2C                   ENDIF
205900030113     C* ESISTE GIACENZA
206000050309     C     KEYGCP        CHAIN     TIGCP51L                           99
206100030113IF  2C     *IN99         IFEQ      *OFF
206200030113     C*
206300030113     C* SE IMMAGINE PRIMA DI FIGCP DA IMPOSTARE
206400030113IF  3C     WFPALT        IFEQ      'S'
206500030113     C                   Z-ADD     GCPAGC        D$PAGC
206600030113     C                   Z-ADD     GCPMGC        D$PMGC
206700030113E   3C                   ENDIF
206800030113     C*
206900030113     C* SE IMMAGINE DOPO DI FIGCP DA IMPOSTARE
207000030113IF  3C     WFDALT        IFEQ      'S'
207100030113     C                   Z-ADD     GCPAGC        D$DAGC
207200030113     C                   Z-ADD     GCPMGC        D$DMGC
207300030113E   3C                   ENDIF
207400030113     C*
207500030113E   2C                   ENDIF
207600030113E   1C                   ENDIF
207700030113     C*
207800030113     C                   ENDSR
207900950120     C***********************************************************************
208000950120     C*   GESERR - GESTIONE ERRORI PRECEDENTI PER LA STESSA BOLLA
208100950120     C***********************************************************************
208200950120     C     GESERR        BEGSR
208300950120     C*
208400950120     C* LA CHIAVE E' GIA' STATA IMPOSTATA NELLE SUBROUTINE PRECEDENTI.
208500950123     C                   Z-ADD     *ZEROS        KERDAM
208600950123     C                   Z-ADD     *ZEROS        KERORM
208700950120     C*
208800950120     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
208900950120     C     KEYERR        SETLL     ECERR03L                           99
209000950609     C  N99              READ(N)   ECERR03L                               99
209100950124     C     *IN99         IFEQ      '0'
209200950120     C     ERRAAS        ANDEQ     KERAAS
209300950120     C     ERRLNP        ANDEQ     KERLNP
209400950120     C     ERRNRS        ANDEQ     KERNRS
209500950120     C     ERRNSP        ANDEQ     KERNSP
209600030407     c*
209700030407     c     errpgf        ifne      'TREC90R'
209800950411     C                   MOVEL     MSG(5)        $MSG
209900950411     C                   EXSR      SCRERR
210000030407     C                   endif
210100950124     C                   ENDIF
210200950120     C*
210300950120     C                   ENDSR
210400941026     C*------------------------------------------------------------------------*
210500941026     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
210600941026     C*------------------------------------------------------------------------*
210700941026     C     SCRERR        BEGSR
210800941026     C*
210900941026     C                   MOVEL     'S'           $ERR
211000941026     C*
211100941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
211200941026     C     D$GERR        IFEQ      *BLANKS
211300941026     C*
211400941026     C                   EXSR      SUBDAT
211500941026     C                   Z-ADD     DSDAT8        ERRDAT
211600941026     C                   Z-ADD     DTORA6        ERRTIM
211700941026     C                   ADD       1             D$GPRE
211800941026     C                   Z-ADD     D$GPRE        ERRPRG
211900950120     C                   Z-ADD     KERAAS        ERRAAS
212000950120     C                   MOVEL     WTBL          ERRTBL
212100950120     C                   Z-ADD     KERLNP        ERRLNP
212200950120     C                   Z-ADD     KERNRS        ERRNRS
212300950120     C                   Z-ADD     KERNSP        ERRNSP
212400950120     C                   Z-ADD     KERAAS        DSAAS
212500950120     C                   Z-ADD     WMGS          DSMGS
212600950120     C                   Z-ADD     DSDAT8        ERRDSP
212700941026     C                   MOVEL     $MSG          DSMSG
212800941026     C                   MOVEL     DSDER         ERRDER
212900941026     C                   MOVEL     DSCER         ERRCER
213000950123     C                   MOVEL     D$GSTO        ERRSTS
213100941026     C                   Z-ADD     D$GDAT        ERRDAM
213200941026     C                   Z-ADD     D$GTIM        ERRORM
213300941026     C                   MOVEL     D$GUSR        ERRUSR
213400941026     C                   MOVEL     D$GPGF        ERRPGF
213500950118     C                   MOVEL     D$GFIL        ERRFIL
213600950118     C                   MOVEL     D$GMBR        ERRMBR
213700950118     C                   MOVEL     D$GLIB        ERRLIB
213800030109     C*
213900030109     C     D$GFIL        IFEQ      'FIAR500F'
214000030109     C     D$GTRD        IFEQ      'TRS'
214100030109     C                   MOVEL     D$PAR5T       ERRRP1
214200030109     C                   MOVEL     D$DAR5T       ERRRD1
214300030109     C                   ENDIF
214400030109     C     D$GTRD        IFEQ      'BAN'
214500030801     C     D$GTRD        OREQ      'BNB'
214600030801     C                   MOVEL     D$PAR5B       ERRRP1
214700030801     C                   MOVEL     D$DAR5B       ERRRD1
214800030801     C                   ENDIF
214900030801     C                   ENDIF
215000030801     C*
215100950123     C     D$GFIL        IFEQ      'TNCSB00F'
215200030801     C                   MOVEL     D$PCO1        ERRRP1
215300030801     C                   MOVEL     D$PCO2        ERRRP2
215400030801     C                   MOVEL     D$DCO1        ERRRD1
215500030801     C                   MOVEL     D$DCO2        ERRRD2
215600030801     C                   ENDIF
215700030801     C*
215800030801     C     D$GFIL        IFEQ      'TITAS10F'
215900020527     C     D$GFIL        OREQ      'TITAS00F'
216000950123     C                   MOVEL     D$PRE1        ERRRP1
216100950123     C                   MOVEL     D$PRE2        ERRRP2
216200950123     C                   MOVEL     D$DRE1        ERRRD1
216300950123     C                   MOVEL     D$DRE2        ERRRD2
216400950123     C                   ENDIF
216500030109     C*
216600941026     C                   WRITE     ECERR
216700941026     C*
216800941026     C                   MOVEL     DSCER         D$GERR
216900941026     C                   MOVEL     DSDER         D$GMSG
217000941026     C*
217100941026     C                   ENDIF
217200941026     C*
217300941026     C                   ENDSR
217400941026     C*------------------------------------------------------------------------*
217500941026     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
217600941026     C*------------------------------------------------------------------------*
217700941026     C     SUBDAT        BEGSR
217800941026     C*
217900941026     C                   TIME                    DTTIME
218000941026     C                   Z-ADD     DTANN         DSANN
218100941026     C                   Z-ADD     DTMES         DSMES
218200941026     C                   Z-ADD     DTGIO         DSGIO
218300941026     C                   Z-ADD     DTSEC         DSSEC
218400941026     C*
218500941026     C                   ENDSR
218600940808     C********************************************************************
218700940808     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
218800940808     C********************************************************************
218900940808     C     AZZW          BEGSR
219000940808     C*
219100040123     C* AZZERO CAMPI DS ESTERNA
219200040123     c* è possibile usare all'interno del pgm solo i campi prima e dopo
219300040123     c* del TITAS senza testare i relativi flag prima o dopo.
219400040123     c* Se si vogliono usare i campi degli altri files senza testare i
219500040123     c* flag prima o dopo, bisogna inserire le routine di inizializzazione
219600040123     c* contenute nel /COPY
219700040123     C                   EXSR      APDTAQ
219800040123     C                   EXSR      ADDTAQ
219900040123     C                   EXSR      AODTAQ
220000941201     C                   MOVEL     D$ODS1        D$ORE1
220100941201     C                   MOVEL     D$ODS2        D$ORE2
220200950123     C                   MOVEL     *BLANKS       D$PCO1
220300950123     C                   MOVEL     *BLANKS       D$PCO2
220400950123     C                   MOVEL     *BLANKS       D$DCO1
220500950123     C                   MOVEL     *BLANKS       D$DCO2
220600941129     C                   MOVEL     *BLANKS       D$PSVN
220700941129     C                   MOVEL     *BLANKS       D$PVAN
220800941129     C                   MOVEL     *BLANKS       D$DSVN
220900941129     C                   MOVEL     *BLANKS       D$DVAN
221000941129     C                   MOVEL     *BLANKS       D$OSVN
221100941129     C                   MOVEL     *BLANKS       D$OVAN
221200941129     C                   Z-ADD     *ZEROS        D$PCAS
221300941129     C                   MOVEL     *BLANKS       D$PTPI
221400040521     C                   MOVEL     *BLANKS       D$Psta
221500941129     C                   Z-ADD     *ZEROS        D$DCAS
221600941129     C                   MOVEL     *BLANKS       D$DTPI
221700040521     C                   MOVEL     *BLANKS       D$Dsta
221800941129     C                   Z-ADD     *ZEROS        D$PVAE
221900941129     C                   Z-ADD     *ZEROS        D$DVAE
222000941209     C                   Z-ADD     *ZEROS        D$PNSR
222100941209     C                   Z-ADD     *ZEROS        D$DNSR
222200030113     C                   Z-ADD     *ZEROS        D$PAGC
222300030113     C                   Z-ADD     *ZEROS        D$PMGC
222400030113     C                   Z-ADD     *ZEROS        D$DAGC
222500030113     C                   Z-ADD     *ZEROS        D$DMGC
222600941130     C                   MOVEL     *BLANKS       D$PTPO
222700941130     C                   MOVEL     *BLANKS       D$PRBL
222800970507     C                   MOVEL     'N'           D$PCSF
222900970507     C                   MOVEL     'N'           D$PCCS
223000970507     C                   MOVEL     'N'           D$PDBO
223100970507     C                   MOVEL     'N'           D$PFRI
223200970507     C                   MOVEL     'N'           D$PFSP
223300941130     C                   MOVEL     *BLANKS       D$DTPO
223400941130     C                   MOVEL     *BLANKS       D$DRBL
223500970507     C                   MOVEL     'N'           D$DCSF
223600970507     C                   MOVEL     'N'           D$DCCS
223700970507     C                   MOVEL     'N'           D$DDBO
223800970507     C                   MOVEL     'N'           D$DFRI
223900970507     C                   MOVEL     'N'           D$DFSP
224000030108     C                   MOVEL     *BLANKS       D$PAR5T
224100030108     C                   MOVEL     *BLANKS       D$DAR5T
224200030108     C                   MOVEL     *BLANKS       D$PAR5B
224300030108     C                   MOVEL     *BLANKS       D$DAR5B
224400040517     C                   Z-ADD     *ZEROS        D$PAR5flp
224500040517     C                   Z-ADD     *ZEROS        D$PAR5DET
224600030113     C                   Z-ADD     *ZEROS        D$PAR5DUT
224700040517     C                   Z-ADD     *ZEROS        D$PAR5fl2
224800030113     C                   Z-ADD     *ZEROS        D$PAR5DE2
224900030113     C                   Z-ADD     *ZEROS        D$PAR5DU2
225000040517     C                   Z-ADD     *ZEROS        D$PAR5fl3
225100040517     C                   Z-ADD     *ZEROS        D$PAR5DE3
225200040517     C                   Z-ADD     *ZEROS        D$PAR5DU3
225300040517     C                   Z-ADD     *ZEROS        D$PAR5fl4
225400040517     C                   Z-ADD     *ZEROS        D$PAR5DE4
225500040517     C                   Z-ADD     *ZEROS        D$PAR5DU4
225600040517     C                   Z-ADD     *ZEROS        D$PAR5fl5
225700040517     C                   Z-ADD     *ZEROS        D$PAR5DE5
225800040517     C                   Z-ADD     *ZEROS        D$PAR5DU5
225900040517     C                   Z-ADD     *ZEROS        D$DAR5flp
226000030113     C                   Z-ADD     *ZEROS        D$DAR5DET
226100030113     C                   Z-ADD     *ZEROS        D$DAR5DUT
226200040517     C                   Z-ADD     *ZEROS        D$DAR5fl2
226300040517     C                   Z-ADD     *ZEROS        D$DAR5DE2
226400030113     C                   Z-ADD     *ZEROS        D$DAR5DU2
226500040517     C                   Z-ADD     *ZEROS        D$DAR5fl3
226600040517     C                   Z-ADD     *ZEROS        D$DAR5DE3
226700040517     C                   Z-ADD     *ZEROS        D$DAR5DU3
226800040517     C                   Z-ADD     *ZEROS        D$DAR5fl4
226900040517     C                   Z-ADD     *ZEROS        D$DAR5DE4
227000040517     C                   Z-ADD     *ZEROS        D$DAR5DU4
227100040517     C                   Z-ADD     *ZEROS        D$DAR5fl5
227200040517     C                   Z-ADD     *ZEROS        D$DAR5DE5
227300040517     C                   Z-ADD     *ZEROS        D$DAR5DU5
227400030113     C                   Z-ADD     *ZEROS        D$PAR5TBA
227500030113     C                   Z-ADD     *ZEROS        D$PAR5NBA
227600030113     C                   Z-ADD     *ZEROS        D$PAR5TB2
227700030113     C                   Z-ADD     *ZEROS        D$PAR5NB2
227800030113     C                   Z-ADD     *ZEROS        D$DAR5TBA
227900030113     C                   Z-ADD     *ZEROS        D$DAR5NBA
228000030113     C                   Z-ADD     *ZEROS        D$DAR5TB2
228100030113     C                   Z-ADD     *ZEROS        D$DAR5NB2
228200040218     C                   Z-ADD     *ZEROS        D$PAR5CSF
228300040218     C                   Z-ADD     *ZEROS        D$PAR5COR
228400040218     C                   Z-ADD     *ZEROS        D$DAR5CSF
228500040218     C                   Z-ADD     *ZEROS        D$DAR5COR
228600941129     C                   MOVEL     *BLANKS       D$XXXX
228700940808     C*
228800940808     C                   ENDSR
228900011106     C*------------------------------------------------------------------------*
229000011106     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
229100011106     C*------------------------------------------------------------------------*
229200011106     C     CNVEUR        BEGSR
229300011106     C*
229400011106     C* AZZERA VARIABILI DI LAVORO
229500011106     C                   Z-ADD     *ZEROS        WIMO             30 9
229600011106     C*
229700011106IF  1C     WIMI          IFNE      *ZEROS
229800011106     C                   CLEAR                   YEUR
229900011106     C                   MOVEL     WDVI          YECDVI
230000011106     C                   Z-ADD     WIMI          YECIMI
230100011106     C                   MOVEL     WDIVCE        YECDVO
230200011213     C                   MOVEL     '3'           YECDCO
230300011213     C                   MOVEL     'H'           YECTAR
230400011106     C                   CALL      'YEURCO'
230500011106     C                   PARM                    YEUR
230600011106IF  2C     YECESI        IFEQ      ' '
230700011106     C                   Z-ADD     YECIMO        WIMO
230800011106X   2C                   ELSE
230900011106     C                   Z-ADD     WIMI          WIMO
231000011106E   2C                   ENDIF
231100011106E   1C                   ENDIF
231200011106     C*
231300011106     C                   ENDSR
231400930414     C********************************************************************
231500931105     C*   RUTINZ - ROUTINE INIZIALE
231600930414     C********************************************************************
231700931105     C     RUTINZ        BEGSR
231800930414     C*
231900930414     C* PARAMETRI IN INPUT ALLA ROUTINE
232000930414     C     *ENTRY        PLIST
232100940808     C                   PARM                    DS$ECO
232200971229     C                   PARM                    DS£ECO
232300930414     C*
232400930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
232500930414     C     KEYTAB        KLIST
232600930414     C                   KFLD                    KTBUTE
232700930414     C                   KFLD                    KTBCOD
232800930414     C                   KFLD                    KTBKEY
232900011106     C*
233000011106     C* CHIAVE LETTURA TNTBE01L - PARZIALE
233100011106     C     KEYTBE        KLIST
233200011106     C                   KFLD                    KTECOD
233300011106     C                   KFLD                    KTEKE1
233400941129     C*
233500950324     C* CHIAVE LETTURA TNCSB03L - COMPLETA
233600941129     C     KEYCSB        KLIST
233700941130     C                   KFLD                    CSBAAS
233800030109     C                   KFLD                    CSBLNP
233900941129     C                   KFLD                    CSBNRS
234000941129     C                   KFLD                    CSBNSP
234100950324     C*
234200950324     C* CHIAVE LETTURA TNCSV01L - PARZIALE
234300950324     C     KEYCSV        KLIST
234400950324     C                   KFLD                    CSVAAS
234500950324     C                   KFLD                    CSVLNP
234600950324     C                   KFLD                    CSVNRS
234700950324     C                   KFLD                    CSVNSP
234800940808     C*
234900941129     C* CHIAVE LETTURA FLLBL01L - COMPLETA
235000940808     C     KEYLBL        KLIST
235100940809     C                   KFLD                    LBLAAN
235200940809     C                   KFLD                    LBLLPN
235300940809     C                   KFLD                    LBLNRN
235400940809     C                   KFLD                    LBLNSN
235500940809     C*
235600990804     C* CHIAVE LETTURA TITAS30C - COMPLETA
235700940809     C     KEYTAS        KLIST
235800940809     C                   KFLD                    TASAAS
235900940809     C                   KFLD                    TASLNP
236000940809     C                   KFLD                    TASNRS
236100940809     C                   KFLD                    TASNSP
236200940809     C                   KFLD                    TASTBL
236300030109     C*
236400030109     C* CHIAVE LETTURA TITAS30C - PARZIALE
236500030109     C     KE2TAS        KLIST
236600030109     C                   KFLD                    TASAAS
236700030109     C                   KFLD                    TASLNP
236800030109     C                   KFLD                    TASNRS
236900030109     C                   KFLD                    TASNSP
237000941130     C*
237100990806     C* CHIAVE LETTURA TITA730C - COMPLETA
237200941130     C     KEYTA7        KLIST
237300941130     C                   KFLD                    TA7AAS
237400941130     C                   KFLD                    TA7LNP
237500941130     C                   KFLD                    TA7NRS
237600941130     C                   KFLD                    TA7NSP
237700941130     C                   KFLD                    TA7TBL
237800030108     C*
237900030108     C* CHIAVE LETTURA FIAR503C - PARZIALE
238000030108     C     KEYAR5        KLIST
238100030108     C                   KFLD                    AR5AAS
238200030108     C                   KFLD                    AR5LNP
238300030108     C                   KFLD                    AR5NRS
238400030108     C                   KFLD                    AR5NSP
238500030108     C                   KFLD                    AR5TRD
238600040521     C                   KFLD                    AR5prg
238700030113     C*
238800050309     C* CHIAVE LETTURA TIGCP51L - COMPLETA
238900030113     C     KEYGCP        KLIST
239000030113     C                   KFLD                    GCPAAS
239100030113     C                   KFLD                    GCPLNP
239200030113     C                   KFLD                    GCPNRS
239300030113     C                   KFLD                    GCPNSP
239400030113     C                   KFLD                    GCPFRG
239500941026     C*
239600950120     C* CHIAVE LETTURA ECERR03L - PARZIALE
239700941026     C     KEYERR        KLIST
239800950120     C                   KFLD                    KERAAS
239900950120     C                   KFLD                    KERLNP
240000950120     C                   KFLD                    KERNRS
240100950120     C                   KFLD                    KERNSP
240200950120     C                   KFLD                    KERDAM
240300950120     C                   KFLD                    KERORM
240400930414     C*
240500931111     C* DEFINISCO CAMPI
240600930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
240700930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
240800930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
240900011106     C     *LIKE         DEFINE    TBECOD        KTECOD
241000011106     C     *LIKE         DEFINE    TBEKE1        KTEKE1
241100950120     C     *LIKE         DEFINE    ERRAAS        KERAAS
241200950120     C     *LIKE         DEFINE    ERRLNP        KERLNP
241300950120     C     *LIKE         DEFINE    ERRNRS        KERNRS
241400950120     C     *LIKE         DEFINE    ERRNSP        KERNSP
241500950120     C     *LIKE         DEFINE    ERRDAM        KERDAM
241600950120     C     *LIKE         DEFINE    ERRORM        KERORM
241700950120     C     *LIKE         DEFINE    ERRTBL        WTBL
241800950120     C     *LIKE         DEFINE    DSMGS         WMGS
241900090306     C     *DTAARA       DEFINE                  TREC00
242000931105     C*
242100931105     C* IMPOSTO CAMPI DI GESTIONE PGM
242200941021     C                   MOVEL     'N'           $ERR              1
242300950117     C                   MOVEL     *BLANKS       $MSG             78
242400940411     C                   MOVEL     'N'           FLAGGS            1
242500930414     C*
242600930414     C* RECUPERO IL CODICE DELL'AZIENDA
242700930414     C                   Z-ADD     1             CODUT             1 0
242800991116     C*
242900991116     C* RECUPERA I CAMPI PER LA SIMULAZIONE DEL BUDGET
243000140130     C                   MOVEL     D$GXXX        Dsd$g
243100140227     C                   MOVE      £d$gasi       D$GASI            4 0
243200931104     C*
243300971203     C* CARICA LE TABELLE NECESSARIE AL PGM + REPERIM.LISTA LIB.
243400931104     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
243500931104     C     CTABSN        IFNE      'S'
243600931104     C                   MOVEL     'S'           CTABSN            1
243700931111     C                   EXSR      CARTAB
243800020805     C                   ENDIF
243900940103     C*
244000020528     C* CARICA I FLAG AGG. SALDI DALLA TABELLA 2V SE NON PRESENTI NELLA DS INPUT
244100940103     C                   EXSR      CARFLG
244200930414     C*
244300930414     C                   ENDSR
244400930414     C********************************************************************
244500931111     C*   CARTAB - CARICA LE TABELLE
244600930414     C********************************************************************
244700930414     C     CARTAB        BEGSR
244800941025     C*
244900011106     C* APRE I FILES
245000941025     C                   OPEN      TABEL00F
245100011106     C                   OPEN      TNTBE01L
245200011106     C*
245300011106     C* REPERISCE LA DATA CORRENTE
245400011106     C                   TIME                    N14              14 0
245500011106     C                   MOVE      N14           N8                8 0
245600011106     C                   Z-ADD     N8            G08DAT
245700011106     C                   Z-ADD     *ZEROS        G08INV
245800011106     C                   MOVEL     '0'           G08ERR
245900011106     C                   CALL      'XSRDA8'
246000011106     C                   PARM                    WLBDA8
246100011106     C                   Z-ADD     G08INV        DATCOR            8 0
246200930414     C*
246300931104     C* CARICA TABELLA -TB- TIPI BOLLA
246400941021     C     $ERR          IFEQ      'N'
246500930421     C                   Z-ADD     0             I
246600930414     C                   Z-ADD     CODUT         KTBUTE
246700931104     C                   MOVEL     'TB'          KTBCOD
246800930414     C                   MOVEL     *BLANKS       KTBKEY
246900930414     C     KEYTAB        SETLL     TABEL00F                           99
247000950203     C  N99              READ(N)   TABEL00F                               99
247100930414     C     *IN99         IFEQ      '1'
247200931104     C                   MOVEL     MSG(1)        $MSG
247300941026     C                   EXSR      SCRERR
247400930414     C                   END
247500931104     C     *IN99         DOWEQ     '0'
247600931104     C     TBLCOD        ANDEQ     'TB'
247700931104     C     TBLFLG        IFEQ      *BLANKS
247800930421     C                   ADD       1             I
247900931104     C                   MOVEL     TBLKEY        TBL(I)
248000931104     C                   MOVEL     TBLUNI        DSTB
248100931104     C                   MOVEL     §TBTPO        TPO(I)
248200931104     C                   MOVEL     §TBRBL        RBL(I)
248300940808     C                   MOVEL     §TBCSF        CSF(I)
248400940930     C                   MOVEL     §TBCCS        CCS(I)
248500940808     C                   MOVEL     §TBDBO        DBO(I)
248600931104     C                   END
248700950203     C                   READ(N)   TABEL00F                               99
248800931104     C                   END
248900930414     C                   END
249000940824     C*
249100940824     C* CARICA TABELLA -2W- FLAG SPEDIZIONI/RICAVI S/N
249200941021     C     $ERR          IFEQ      'N'
249300940824     C                   Z-ADD     0             I
249400940824     C                   Z-ADD     CODUT         KTBUTE
249500940824     C                   MOVEL     '2W'          KTBCOD
249600940824     C                   MOVEL     *BLANKS       KTBKEY
249700940824     C     KEYTAB        SETLL     TABEL00F                           99
249800950203     C  N99              READ(N)   TABEL00F                               99
249900940824     C     *IN99         IFEQ      '1'
250000940824     C                   MOVEL     MSG(13)       $MSG
250100941026     C                   EXSR      SCRERR
250200940824     C                   END
250300940824     C     *IN99         DOWEQ     '0'
250400940824     C     TBLCOD        ANDEQ     '2W'
250500940824     C     TBLFLG        IFEQ      *BLANKS
250600940824     C                   ADD       1             I
250700940824     C                   MOVEL     TBLKEY        FRB(I)
250800940824     C                   MOVEL     TBLUNI        DS2W
250900940824     C                   MOVE      §2WFSP        FSP(I)
251000940824     C                   MOVE      §2WFRI        FRI(I)
251100940824     C                   END
251200950203     C                   READ(N)   TABEL00F                               99
251300940824     C                   END
251400940824     C                   END
251500020528     C*
251600020528     C* CARICA TABELLA DATI VARI CONTO ECONOMICO
251700020530     C                   Z-ADD     0             I
251800020528     C                   Z-ADD     CODUT         KTBUTE
251900020528     C                   MOVEL     '2V'          KTBCOD
252000020528     C                   MOVEL     *BLANKS       KTBKEY
252100020528     C     KEYTAB        SETLL     TABEL00F                           99
252200020528     C  N99              READ(N)   TABEL00F                               99
252300020528     C     *IN99         DOWEQ     '0'
252400020528     C     TBLCOD        ANDEQ     '2V'
252500020528     C     TBLFLG        IFEQ      *BLANKS
252600020528     C                   ADD       1             I
252700020528     C                   MOVEL     TBLUNI        DS2V
252800020528     C                   MOVEL     TBLKEY        FIL(I)
252900020528     C                   MOVEL     §2VSDA        SDA(I)
253000020528     C                   MOVEL     §2VSDC        SDC(I)
253100020528     C                   MOVEL     §2VSDF        SDF(I)
253200020528     C                   MOVEL     §2VSDO        SDO(I)
253300020528     C                   MOVEL     §2VSDP        SDP(I)
253400020528     C                   MOVEL     §2VSDR        SDR(I)
253500020528     C                   MOVEL     §2VSDT        SDT(I)
253600040316     C                   MOVEL     §2VSDE        SDE(I)
253700020528     C                   MOVEL     §2VCET        CET(I)
253800020528     C                   MOVEL     §2VMOV        MOV(I)
253900020531     C                   MOVEL     §2VFIM        FIM(I)
254000020531     C                   MOVEL     §2VFCE        FCE(I)
254100030108     C                   MOVEL     §2VCOP        COP(I)
254200020528     C                   ENDIF
254300020528     C  N99              READ(N)   TABEL00F                               99
254400020528     C                   ENDDO
254500960130     C*
254600990804     C* CARICO DATA ULTIMO TRASF. DA TITAS10 > TITASP PER ESCLUDERE LE BOLLE
254700960130     C* CON DATA INFERIORE
254800960130     C                   Z-ADD     1             KTBUTE
254900960130     C                   MOVEL     '$3'          KTBCOD
255000960130     C                   MOVEL     '1'           KTBKEY
255100960130     C     KEYTAB        CHAIN     TABEL00F                           99
255200960130IF  1C     *IN99         IFEQ      *ON
255300960130     C                   MOVEL     MSG(10)       $MSG
255400960130     C                   EXSR      SCRERR
255500960130X   1C                   ELSE
255600960130     C                   MOVEL     TBLUNI        DS$3
255700960130     C                   Z-ADD     §$3D10        WDT10P            8 0
255800960130E   1C                   ENDIF
255900020117     C*
256000011106     C* DIVISE PER CONVERSIONE
256100011106     C                   CLEAR                   DSGEDCE
256200011106     C                   MOVEL     *BLANKS       WDIVCE            3
256300011106     C                   MOVEL(P)  'GED'         KTECOD
256400011106     C                   MOVEL(P)  'CE'          KTEKE1
256500011106     C     KEYTBE        CHAIN     TNTBE01L
256600011106IF  1C                   IF        %found(TNTBE01L)
256700011106     C                   MOVEL     TBEUNI        DSGEDCE
256800011106E   1C                   ENDIF
256900011106IF  1C                   IF        §GEDCEDAT > DATCOR
257000011106     C                   EVAL      WDIVCE = §GEDCEDIP
257100011106X   1C                   ELSE
257200011106     C                   EVAL      WDIVCE = §GEDCEDIV
257300011106E   1C                   ENDIF
257400941025     C*
257500011106     C* CHIUDE I FILES
257600941025     C                   CLOSE     TABEL00F
257700011106     C                   CLOSE     TNTBE01L
257800930414     C*
257900930414     C                   ENDSR
258000950201     C*------------------------------------------------------------------------*
258100980130     C*?AGGTAB - AGGIORNO DATA AREA E LA TAB.'$3' CON L'ULTIMA DATA BOLLA
258200950201     C*------------------------------------------------------------------------*
258300950517     C     AGGTAB        BEGSR
258400950201     C*
258500020529     C* AGGIORNA A 'SI' IL FLAG PER SCONGELARE LA KECONOM E FAR PARTIRE LA
258600020529     C* ELABORAZIONE DELLE STATISTICHE IN CODA
258700090306     C     *LOCK         IN        TREC00
258800090306     C                   MOVEL     'S'           D00RCB
258900090306     C                   OUT       TREC00
259000980130     C*
259100950517     C* AGGIORNO TABELLA '$3'
259200980130     C                   OPEN      TABEL00F
259300950517     C                   Z-ADD     1             KTBUTE
259400950517     C                   MOVEL     '$3'          KTBCOD
259500950517     C                   MOVEL     '1'           KTBKEY
259600950517     C     KEYTAB        CHAIN     TABEL00F                           99
259700950517IF  1C     *IN99         IFEQ      *ON
259800950517     C                   MOVEL     MSG(10)       $MSG
259900950517     C                   EXSR      SCRERR
260000950517X   1C                   ELSE
260100950517     C                   MOVEL     TBLUNI        DS$3
260200950517     C* SE LA DATA DELLA TABELLA E' INFERIORE A QUELLA MEMORIZZATA, AGGIORNO
260300950517IF  2C     §$3D00        IFLT      DTSB23
260400950517     C                   Z-ADD     DTSB23        §$3D00
260500950517     C                   MOVEL     DS$3          TBLUNI
260600980828     C                   MOVEL     *BLANKS       TBLFTR
260700980828     C                   Z-ADD     *ZEROS        TBLDTR
260800950517     C                   UPDATE    TABEL
260900950517E   2C                   ENDIF
261000950517E   1C                   ENDIF
261100950201     C                   CLOSE     TABEL00F
261200950201     C*
261300950201     C                   ENDSR
261400950411     C*------------------------------------------------------------------------*
261500950411     C*?RKECON - LANCIA CL CHE RILASCIA DOPO 2 ORE LA CODA KECONOM
261600950411     C*------------------------------------------------------------------------*
261700950411     C     RKECON        BEGSR
261800950411     C*
261900980130     C* SE IL GUFO SDI E QUELLO BARTOLINI HANNO GIA' TRATTATO LA CHIUSURA
262000980130     C     D00RCB        IFEQ      'S'
262100950411     C                   Z-ADD     45            LUNG             15 5
262200950411     C                   MOVEL     *BLANKS       COMMAN
262300950411     C                   MOVEA     CMD1(1)       COMMAN           80
262400950411     C                   CALL      'QCMDEXC'
262500950411     C                   PARM                    COMMAN
262600950411     C                   PARM                    LUNG
262700980130     C                   ENDIF
262800950411     C*
262900950411     C                   ENDSR
263000940103     C*-------------------------------------------------------------------
263100940103     C*? CARFLG - CARICA FLAG PER AGGIORNAMENTO SALDI
263200940103     C*-------------------------------------------------------------------
263300940103     C     CARFLG        BEGSR
263400020620     c*
263500020620IF  1C     D$GPGF        IFNE      'TRECE9R'
263600020620     C     D$GPGF        ANDNE     'TREC78R'
263700020620     C     D$GPGF        ANDNE     'TREC11R'
263800020607     C                   MOVEL     'N'           D$GSDA
263900020607     C                   MOVEL     'N'           D$GSDC
264000020607     C                   MOVEL     'N'           D$GSDF
264100020607     C                   MOVEL     'N'           D$GSDO
264200020607     C                   MOVEL     'N'           D$GSDP
264300020607     C                   MOVEL     'N'           D$GSDR
264400020607     C                   MOVEL     'N'           D$GSDT
264500040316     C                   MOVEL     'N'           D$GSDE
264600020607     C                   MOVEL     'N'           D$GCET
264700020607     C                   MOVEL     'N'           D$GWFM
264800020607     C                   MOVEL     'N'           D$GFIM
264900020607     C                   MOVEL     'N'           D$GFCE
265000030108     C                   MOVEL     'N'           D$GCOP
265100020528     C                   Z-ADD     1             I
265200020528     C     D$GFIL        LOOKUP    FIL(I)                                 99
265300020617if  2C     *IN99         IFEQ      '1'
265400020528     C                   MOVEL     SDA(I)        D$GSDA
265500020528     C                   MOVEL     SDC(I)        D$GSDC
265600020528     C                   MOVEL     SDF(I)        D$GSDF
265700020528     C                   MOVEL     SDO(I)        D$GSDO
265800020528     C                   MOVEL     SDP(I)        D$GSDP
265900020528     C                   MOVEL     SDR(I)        D$GSDR
266000020528     C                   MOVEL     SDT(I)        D$GSDT
266100040316     C                   MOVEL     SDE(I)        D$GSDE
266200020528     C                   MOVEL     CET(I)        D$GCET
266300020528     C                   MOVEL     MOV(I)        D$GWFM
266400020531     C                   MOVEL     FIM(I)        D$GFIM
266500020531     C                   MOVEL     FCE(I)        D$GFCE
266600030108     C                   MOVEL     COP(I)        D$GCOP
266700020617e   2c                   ENDIF
266800020617e   1C                   ENDIF
266900940103     C*
267000940103     C                   ENDSR
267100991116     C*-------------------------------------------------------------------
267200991116     C*? GESASI - GESTIONE ANNO PER SIMULAZIONE
267300991116     C*-------------------------------------------------------------------
267400991116     C     GESASI        BEGSR
267500991116     C*
267600991116     C* IMPOSTA L'ANNO DELLA SIMULAZIONE NELLE DATE UTILIZZATE NEL C.E.
267700030129     C*---
267800030129     C* DS BOLLA
267900030129     C*---
268000991116     C* ANNO DELLA BOLLA
268100991116     C                   Z-ADD     D$GASI        D$DAAS
268200991116     C* DATA DI CONSEGNA
268300030129IF  1C     D$DDCM        IFGT      *ZEROS
268400991116     C                   Z-ADD     D$DDCM        DSDAT8
268500991116     C                   Z-ADD     D$GASI        DSAAS
268600991116     C                   Z-ADD     DSDAT8        D$DDCM
268700030129E   1C                   ENDIF
268800991116     C* DATA FATTURA
268900030129IF  1C     D$DDFT        IFGT      *ZEROS
269000991116     C                   Z-ADD     D$DDFT        DSDAT8
269100991116     C                   Z-ADD     D$GASI        DSAAS
269200991116     C                   Z-ADD     DSDAT8        D$DDFT
269300030129E   1C                   ENDIF
269400030129     C* DATA ARRIVO ULTIMO COLLO
269500030129IF  1C     D$DDTI        IFGT      *ZEROS
269600030129     C                   Z-ADD     D$DDTI        DSDAT8
269700030129     C                   Z-ADD     D$GASI        DSAAS
269800030129     C                   Z-ADD     DSDAT8        D$DDTI
269900030129E   1C                   ENDIF
270000030129     C* DATA CONSEGNA MERCE
270100030129IF  1C     D$DDCM        IFGT      *ZEROS
270200030129     C                   Z-ADD     D$DDCM        DSDAT8
270300030129     C                   Z-ADD     D$GASI        DSAAS
270400030129     C                   Z-ADD     DSDAT8        D$DDCM
270500030129E   1C                   ENDIF
270600030129     C* DATA DISTINTA CONSEGNA
270700030129IF  1C     D$DDDC        IFGT      *ZEROS
270800030129     C                   Z-ADD     D$DDDC        DSDAT8
270900030129     C                   Z-ADD     D$GASI        DSAAS
271000030129     C                   Z-ADD     DSDAT8        D$DDDC
271100030129E   1C                   ENDIF
271200030129     C* ANNO APERTURA GIACENZA
271300030129IF  1C     D$DAGC        IFGT      *ZEROS
271400030129     C                   Z-ADD     D$GASI        D$DAGC
271500030129E   1C                   ENDIF
271600030129     C* DATA PARTENZA ULTIMO COLLO
271700030129IF  1C     D$DDUC        IFGT      *ZEROS
271800030129     C                   Z-ADD     D$DDUC        DSDAT8
271900030129     C                   Z-ADD     D$GASI        DSAAS
272000030129     C                   Z-ADD     DSDAT8        D$DDUC
272100030129E   1C                   ENDIF
272200030129     C* DATA RITIRO
272300030129IF  1C     D$DDRT        IFGT      *ZEROS
272400030129     C                   Z-ADD     D$DDRT        DSDAT8
272500030129     C                   Z-ADD     D$GASI        DSAAS
272600030129     C                   Z-ADD     DSDAT8        D$DDRT
272700030129E   1C                   ENDIF
272800030129     C*
272900991116     C* REIMPOSTA CAMPI DELLA DS DS$ECO
273000991116     C                   MOVEL     D$DDS1        D$DRE1
273100991116     C                   MOVEL     D$DDS2        D$DRE2
273200030129     C*---
273300030129     C* DS GENERICA
273400030129     C*---
273500030129     C* DATA ENTRATA TRANSITO 1
273600030129IF  1C     D$DAR5DET     IFGT      *ZEROS
273700030129     C                   Z-ADD     D$DAR5DET     DSDAT8
273800030129     C                   Z-ADD     D$GASI        DSAAS
273900030129     C                   Z-ADD     DSDAT8        D$DAR5DET
274000030129E   1C                   ENDIF
274100030129     C* DATA USCITA  TRANSITO 1
274200030129IF  1C     D$DAR5DUT     IFGT      *ZEROS
274300030129     C                   Z-ADD     D$DAR5DUT     DSDAT8
274400030129     C                   Z-ADD     D$GASI        DSAAS
274500030129     C                   Z-ADD     DSDAT8        D$DAR5DUT
274600030129E   1C                   ENDIF
274700030129     C* DATA ENTRATA TRANSITO 2
274800030129IF  1C     D$DAR5DE2     IFGT      *ZEROS
274900030129     C                   Z-ADD     D$DAR5DE2     DSDAT8
275000030129     C                   Z-ADD     D$GASI        DSAAS
275100030129     C                   Z-ADD     DSDAT8        D$DAR5DE2
275200030129E   1C                   ENDIF
275300030129     C* DATA USCITA  TRANSITO 2
275400030129IF  1C     D$DAR5DU2     IFGT      *ZEROS
275500030129     C                   Z-ADD     D$DAR5DU2     DSDAT8
275600030129     C                   Z-ADD     D$GASI        DSAAS
275700030129     C                   Z-ADD     DSDAT8        D$DAR5DU2
275800030129E   1C                   ENDIF
275900040517     C* DATA ENTRATA TRANSITO 3
276000040517IF  1C     D$DAR5DE3     IFGT      *ZEROS
276100040517     C                   Z-ADD     D$DAR5DE3     DSDAT8
276200040517     C                   Z-ADD     D$GASI        DSAAS
276300040517     C                   Z-ADD     DSDAT8        D$DAR5DE3
276400040517E   1C                   ENDIF
276500040517     C* DATA USCITA  TRANSITO 3
276600040517IF  1C     D$DAR5DU3     IFGT      *ZEROS
276700040517     C                   Z-ADD     D$DAR5DU3     DSDAT8
276800040517     C                   Z-ADD     D$GASI        DSAAS
276900040517     C                   Z-ADD     DSDAT8        D$DAR5DU3
277000040517E   1C                   ENDIF
277100040517     C* DATA ENTRATA TRANSITO 4
277200040517IF  1C     D$DAR5DE4     IFGT      *ZEROS
277300040517     C                   Z-ADD     D$DAR5DE4     DSDAT8
277400040517     C                   Z-ADD     D$GASI        DSAAS
277500040517     C                   Z-ADD     DSDAT8        D$DAR5DE4
277600040517E   1C                   ENDIF
277700040517     C* DATA USCITA  TRANSITO 4
277800040517IF  1C     D$DAR5DU4     IFGT      *ZEROS
277900040517     C                   Z-ADD     D$DAR5DU4     DSDAT8
278000040517     C                   Z-ADD     D$GASI        DSAAS
278100040517     C                   Z-ADD     DSDAT8        D$DAR5DU4
278200040517E   1C                   ENDIF
278300040517     C* DATA ENTRATA TRANSITO 5
278400040517IF  1C     D$DAR5DE5     IFGT      *ZEROS
278500040517     C                   Z-ADD     D$DAR5DE5     DSDAT8
278600040517     C                   Z-ADD     D$GASI        DSAAS
278700040517     C                   Z-ADD     DSDAT8        D$DAR5DE5
278800040517E   1C                   ENDIF
278900040517     C* DATA USCITA  TRANSITO 5
279000040517IF  1C     D$DAR5DU5     IFGT      *ZEROS
279100040517     C                   Z-ADD     D$DAR5DU5     DSDAT8
279200040517     C                   Z-ADD     D$GASI        DSAAS
279300040517     C                   Z-ADD     DSDAT8        D$DAR5DU5
279400040617E   1C                   ENDIF
279500991116     C*
279600991116     C                   ENDSR
279700020621     C*-------------------------------------------------------------------
279800020621     C*?Azzera varia "N" immagine -prima- TITAS
279900020621     C*-------------------------------------------------------------------
280000020621     C     AZZnCPDTAQ    BEGSR
280100020621     c*
280200031114if  1C                   IF        (D$PVA1 = 88888888,000)
280300020621     C                   Z-ADD     *zeros        D$PVA1
280400020621e   1c                   ENDIF
280500031114if  1C                   IF        (D$PVA2 = 88888888,000)
280600020621     C                   Z-ADD     *zeros        D$PVA2
280700020621e   1c                   ENDIF
280800031114if  1C                   IF        (D$PVA3 = 88888888,000)
280900020621     C                   Z-ADD     *zeros        D$PVA3
281000020621e   1c                   ENDIF
281100020621     c*
281200020621     C                   ENDSR
281300020621     C*-------------------------------------------------------------------
281400020621     C*?Azzera varia "N" immagine -dopo- TITAS
281500020621     C*-------------------------------------------------------------------
281600020621     C     AZZnCDDTAQ    BEGSR
281700020621     c*
281800031114if  1C                   IF        (D$DVA1 = 88888888,000)
281900020621     C                   Z-ADD     *zeros        D$DVA1
282000020621e   1c                   ENDIF
282100031114if  1C                   IF        (D$DVA2 = 88888888,000)
282200020621     C                   Z-ADD     *zeros        D$DVA2
282300020621e   1c                   ENDIF
282400031114if  1C                   IF        (D$DVA3 = 88888888,000)
282500020621     C                   Z-ADD     *zeros        D$DVA3
282600020621e   1c                   ENDIF
282700020621     c*
282800020621     C                   ENDSR
282900020621     C*-------------------------------------------------------------------
283000020621     C*?Azzera varia "N" immagine TITAS originale
283100020621     C*-------------------------------------------------------------------
283200020621     C     AZZnCODTAQ    BEGSR
283300020621     c*
283400031114if  1C                   IF        (D$OVA1 = 88888888,000)
283500020621     C                   Z-ADD     *zeros        D$OVA1
283600020621e   1c                   ENDIF
283700031114if  1C                   IF        (D$OVA2 = 88888888,000)
283800020621     C                   Z-ADD     *zeros        D$OVA2
283900020621e   1c                   ENDIF
284000031114if  1C                   IF        (D$OVA3 = 88888888,000)
284100020621     C                   Z-ADD     *zeros        D$OVA3
284200020621e   1c                   ENDIF
284300020621     c*
284400020621     C                   ENDSR
284500020621     C*-------------------------------------------------------------------
284600020621     C*?Azzera varia "N" di TITA7
284700020621     C*-------------------------------------------------------------------
284800020621     C     AZZnTA7       BEGSR
284900020621     c*
285000031114if  1C                   IF        (TA7VAN = 88888888,000)
285100020621     C                   Z-ADD     *zeros        TA7VAN
285200020621e   1c                   ENDIF
285300020621     c*
285400020621     C                   ENDSR
285500930305     C********************************************************************
285600941130      /COPY GAITRASRC/SRCTREC,TRECDT
285700940808     C********************************************************************
285800941130** ======== COMANDO 1                                      ================
285900950411SBMJOB CMD(CALL PGM(TRECD9C)) JOBQ(QCTL)
286000970507** ======== COMANDO 2                                      ================
286100970507ENDSBS SBS(GAIDTAQ)
286200930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
286300941130TRECD6R- Tabella 'TB' non trovata. Avvertire CED.                         TBTB 1
286400030109TRECD6R- Arrivata variazione di TNCSB e non è stato trovato TITAS         NTTA 2
286500030113TRECD6R- 3
286600941130TRECD6R- Tipo bolla non trovato in tab. 'TB'. Avvertire CED.              TBNT 4
286700950118TRECD6R- Trovato gia un altro errore per questa bolla.                    TEQB 5
286800030113TRECD6R- 6
286900030113TRECD6R- Arrivata variazione di FIARB -TRS- e non è stato trovato TITAS   NTFT 7
287000030113TRECD6R- Arrivata variazione di FIARB -BAN- e non è stato trovato TITAS   NTFB 8
287100011106TRECD6R- 9                                                                     9
287200950517TRECD6R- Tabella '$3' non trovata. Avvertire CED.                         TB$310
287300990804TRECD6R- Bolla mamma trovata nei legami ma non in TITAS10F.               BMNT11
287400941130TRECD6R- Flag raggruppamento bolle non trovato in tab. '2W'.              FRNT12
287500941130TRECD6R- Tabella '2W' non trovata. Avvertire CED.                         TB2W13
287600941130TRECD6R- 14                                                                   14
287700941130TRECD6R- 15                                                                   15
287800941130TRECD6R- 16                                                                   16
287900941130TRECD6R- 17                                                                   17
288000941130TRECD6R- 18                                                                   18
288100941130TRECD6R- 19                                                                   19
288200941130TRECD6R- 20                                                                   20
