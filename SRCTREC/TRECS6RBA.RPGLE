000100940509      ****************************************************************
000200000000      *                                                              *
000300931102      *   PARTENDO DALLA DS ESTERNA PASSATA DAL PGM DI LETTURA       *
000400941128      *   DELLA DATAQ, IMPOSTA LA DS UNIFICATA IN BASE AL FILE       *
000500941128      *   MODIFICATO.                                                *
000600930204      *                                                              *
000700930204      ****************************************************************
000800930413     H DECEDIT('0,') DATEDIT(*DMY.)
000900091012     FTABEL00F  iF   E           K DISK    USROPN
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
002000930304     D* MESSAGGI
002100930304     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200941128     D* SCHIERE VARIE E IMPORTI
002300990804     D SV7             S              1    DIM(20)
002400011213     D VA7             S             11    DIM(20)
002500941129     D SVN             S              1    DIM(20)
002600011213     D VAN             S             11    DIM(20)
002700930405     D*****************************************************************
002800931103     D* SCHIERA TABELLA TIPI BOLLA (TB)
002900931104     D TBL             S              2    DIM(50)
003000931104     D TPO             S              1    DIM(50)
003100940808     D RBL             S              1    DIM(50)
003200940808     D CSF             S              1    DIM(50)
003300940930     D CCS             S              1    DIM(50)
003400940808     D DBO             S              1    DIM(50)
003500941130     D*****************************************************************
003600941130     D* SCHIERA TABELLA FLAG SPEDIZIONI/RICAVI (2W)
003700941130     D FRB             S              1    DIM(10)
003800941130     D FSP             S              1    DIM(10)
003900941130     D FRI             S              1    DIM(10)
004000020528     D*****************************************************************
004100020528     D* FILE E RELATIVI FLAGS DI GESTIONE
004200020528     D FIL             S             10    DIM(20)
004300020528     D SDA             S              1    DIM(20)
004400020528     D SDC             S              1    DIM(20)
004500020528     D SDF             S              1    DIM(20)
004600020528     D SDO             S              1    DIM(20)
004700020528     D SDP             S              1    DIM(20)
004800020528     D SDR             S              1    DIM(20)
004900020528     D SDT             S              1    DIM(20)
005000040316     D SDE             S              1    DIM(20)
005100020528     D CET             S              1    DIM(20)
005200020528     D MOV             S              1    DIM(20)
005300020531     D FIM             S              1    DIM(20)
005400020531     D FCE             S              1    DIM(20)
005500030108     D COP             S              1    DIM(20)
005600930323     D*****************************************************************
005700930323     D* DS ESTERNA IN INPUT AL PGM
005800940808     D DS$ECO        E DS
005900971229     D DS£ECO        E DS
006000940405     D*****************************************************************
006100990804     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI TITAS10F + BOLLA ORIG.
006200941130     D TRECDS        E DS
006300940405     D  D$PDS1                 1    250
006400940405     D  D$PDS2               251    500
006500940405     D  D$DDS1               501    750
006600940405     D  D$DDS2               751   1000
006700941128     D  D$ODS1              1001   1250
006800941128     D  D$ODS2              1251   1500
006900941129     D*****************************************************************
007000941129     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI TNCSB00F
007100941129     D DS$CSB        E DS
007200941129     D  D$PCS1                 1    250
007300941129     D  D$PCS2               251    500
007400941129     D  D$DCS1               501    750
007500941129     D  D$DCS2               751   1000
007600030108     D*****************************************************************
007700030108     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI FIAR500F -TRANSITI-
007800030108     D DS$AR5T       E DS
007900030108     D  D$PAT1                 1    250
008000030108     D  D$DAT1               251    500
008100030108     D*****************************************************************
008200030108     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI FIAR500F -BANCALI-
008300030108     D DS$AR5B       E DS
008400030108     D  D$PAB1                 1    250
008500030108     D  D$DAB1               251    500
008600030108     D*****************************************************************
008700011106     D* DS  CONTROLLO DATA (8)
008800011106     D WLBDA8          DS                  INZ
008900011106     D  G08DAT                 1      8  0
009000011106     D  G08INV                 9     16  0
009100011106     D  G08ERR                17     17
009200011106     D  G08TGI                18     22  0
009300930405     D*****************************************************************
009400931103     D* DS ESTERNA TABELLA TB (TIPI BOLLE)
009500931105     D DSTB          E DS
009600940103     D*****************************************************************
009700020528     D* DS ESTERNA TABELLA 2V
009800020528     D DS2V          E DS
009900941130     D*****************************************************************
010000941130     D* DS ESTERNA TABELLA 2W (FLAG SPEDIZ/RICAVI)
010100941130     D DS2W          E DS
010200950517     D*****************************************************************
010300990804     D* DS DATE AVANZAMENTO TRASFERIMENTI TITAS00 > 10 > P
010400950517     D DS$3          E DS
010500030113     D*****************************************************************
010600030113     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD TRANSITI
010700030113     D DAR5TRS       E DS
010800030113     D*****************************************************************
010900030113     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI
011000030113     D DAR5BAN       E DS
011100030801     D*****************************************************************
011200030801     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI BARTOLINI
011300030801     D DAR5BNB       E DS
011400950404     D*****************************************************************
011500011106     D* CONVERSIONI IMPORTI
011600011106     D YEUR          E DS                  EXTNAME(YEURCODS)
011700011106     D*****************************************************************
011800011106     D* DS DIVISE PER CONVERSIONI PER IL C/E
011900011106     D DSGEDCE       E DS
012000980130     D*****************************************************************
012100980130     D* DATA AREA PER DATI VARI SALDI
012200130409     D*TREC00        E DS
012300930325     D***************************************************************
012400930325     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
012500941026     D DS§DAT          DS
012600941026     D  DSSEC                  1      2  0
012700941026     D  DSANN                  3      4  0
012800941026     D  DSMES                  5      6  0
012900941026     D  DSGIO                  7      8  0
013000941129     D  DSAAS                  1      4  0
013100941129     D  DSMGS                  5      8  0
013200941026     D  DSDAT6                 3      8  0
013300941026     D  DSDAT8                 1      8  0
013400941026     D DS§TIM          DS
013500941026     D  DTORA                  1      2  0
013600941026     D  DTMIN                  3      4  0
013700941026     D  DTSCN                  5      6  0
013800941026     D  DTGIO                  7      8  0
013900941026     D  DTMES                  9     10  0
014000941026     D  DTSEC                 11     12  0
014100941026     D  DTANN                 13     14  0
014200941026     D  DTORA6                 1      6  0
014300941026     D  DTTIME                 1     14  0
014400941026     D***************************************************************
014500941026     D* DS PER SCOMPOSIZIONE MESSAGGIO
014600941026     D                 DS
014700941026     D  DSMSG                  1     80
014800941026     D  DSDER                  1     74
014900941026     D  DSCER                 75     78
015000991116     D***************************************************************
015100991116     D* DS PER SCOMPOSIZIONE CAMPO D$GXXX
015200140519     D*                DS
015300140519     D* DSX                    1     33
015400140519     D* DSXASI                 1      4
015500140519     D* DSXFSI                 5      5
015600140519     D* DSXXXX                 6     33
015700140519     D Dsd$g         E DS                  prefix(£)
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
016900091012     C* SE NEL CAMPO STATUS TROVO 'C' (CLOSE)
017000991214     C* CHIUDO I FILE E CHIUDO PGM CON LR
017100991214     C     D$GSTS        IFEQ      'C'
017200991214     C     $ERR          ORNE      'N'
017300991214     C                   ELSE
017400940808     C*
017500940808     C                   EXSR      AZZW
017600941129     C*
017700941129     C* GESTIONE DEI FLAGS PRIMA/DOPO
017800941129     C     $ERR          IFEQ      'N'
017900941129     C                   EXSR      GESFLA
018000941129     C                   ENDIF
018100941129     C*
018200941129     C* GESTIONE VARIAZIONE RECORD DI TNCSB00F (CONTRASSEGNI)
018300941129     C     $ERR          IFEQ      'N'
018400941129     C     D$GFIL        IFEQ      'TNCSB00F'
018500941129     C                   EXSR      GESCSB
018600941129     C                   ENDIF
018700941129     C                   ENDIF
018800030108     C*
018900030108     C* GESTIONE VARIAZIONE RECORD DI FIAR500F (TRANSITI o BANCALI)
019000030108     C     $ERR          IFEQ      'N'
019100030108     C     D$GFIL        IFEQ      'FIAR500F'
019200030108     C                   EXSR      GESAR5
019300030108     C                   ENDIF
019400030108     C                   ENDIF
019500941129     C*
019600990804     C* GESTIONE VARIAZIONE RECORD DI TITAS10F
019700941129     C     $ERR          IFEQ      'N'
019800990804     C     D$GFIL        IFEQ      'TITAS10F'
019900020527     C     D$GFIL        OREQ      'TITAS00F'
020000941129     C                   EXSR      GESTAS
020100941129     C                   ENDIF
020200941129     C                   ENDIF
020300941128     C*
020400941128     C* TRATTA IL TIPO BOLLA (E RELATIVI FLAGS) PRIMA E DOPO
020500941128     C     $ERR          IFEQ      'N'
020600941128     C                   EXSR      GESTBL
020700941128     C                   ENDIF
020800940808     C*
020900941128     C* CERCA LEGAMI ED EVENTUALMENTE MEMORIZZA IMMAGINE MAMMA
021000941021     C     $ERR          IFEQ      'N'
021100941003     C                   EXSR      GESLEG
021200940808     C                   ENDIF
021300940808     C*
021400990804     C* CARICA LE VARIE IN SCHIERA ED EVENTUALMENTE LEGGE TITA7
021500941021     C     $ERR          IFEQ      'N'
021600941128     C                   EXSR      GESVAR
021700940808     C                   ENDIF
021800940809     C*
021900950404     C* TRATTA ASSICURAZIONE PER CONTO
022000011227     C     $ERR          IFEQ      'N'
022100011227     C                   EXSR      GESASC
022200011227     C                   ENDIF
022300941128     C*
022400951213     C* TRATTA NUMERO DI STOP (RITIRO/CONSEGNA)
022500941128     C     $ERR          IFEQ      'N'
022600941128     C                   EXSR      GESSTP
022700941128     C                   ENDIF
022800030113     C*
022900030113     C* TRATTA GIACENZA
023000030113     C     $ERR          IFEQ      'N'
023100030113     C                   EXSR      GESGCP
023200030113     C                   ENDIF
023300991116     C*
023400991116     C* GESTIONE DELL'ANNO PER IL LANCI IN SIMULAZIONE
023500991116     C     $ERR          IFEQ      'N'
023600991116     C     D$GFSI        ANDEQ     'S'
023700991116     C                   EXSR      GESASI
023800991116     C                   ENDIF
023900941128     C* LANCIA IL PGM DI GESTIONE DEL C/ECONOMICO
024000091014     c* se bolla è da calcolare
024100941128     C     $ERR          IFEQ      'N'
024200140519     C                   CALL      'TRECD7R'                            99
024300941128     C                   PARM                    DS$ECO
024400020605     c*
024500991116     C                   ENDIF
024600931104     C*
024700940217     C                   END
024800930323     C*
024900940405     C* SE PGM LANCIATO CON 'C' (CLOSE) CHIUDE CON LR (CHIUDE FILE)
025000930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
025100950201     C     D$GSTS        IFEQ      'C'
025200140519     C                   CALL      'TRECD7R'                            99
025300941128     C                   PARM                    DS$ECO
025400930323     C                   SETON                                        LR
025500950201     C                   ELSE
025600950201     C                   SETON                                        RT
025700991214     C                   ENDIF
025800940916     C*/ FINE MAIN DEL PGM
025900941129     C***********************************************************************
026000941129     C*   GESFLA - GESTIONE FLAGS PRIMA/DOPO
026100941129     C***********************************************************************
026200941129     C     GESFLA        BEGSR
026300941129     C*
026400941129     C* GESTIONE FLAGS PRIMA/DOPO
026500941130     C                   MOVEL     'N'           WFPTAS            1
026600941130     C                   MOVEL     'N'           WFDTAS            1
026700941130     C                   MOVEL     'N'           WFPCSB            1
026800941130     C                   MOVEL     'N'           WFDCSB            1
026900030108     C                   MOVEL     'N'           WFPTRS            1
027000030108     C                   MOVEL     'N'           WFDTRS            1
027100030108     C                   MOVEL     'N'           WFPBAN            1
027200030108     C                   MOVEL     'N'           WFDBAN            1
027300941130     C                   MOVEL     'N'           WFPALT            1
027400941130     C                   MOVEL     'N'           WFDALT            1
027500030108     C*
027600990804     C     D$GFIL        IFEQ      'TITAS10F'
027700020527     C     D$GFIL        OREQ      'TITAS00F'
027800941129     C     D$GSTS        IFEQ      'D'
027900990804     C                   MOVEL     'S'           WFPTAS
028000941129     C                   MOVEL     'S'           WFPCSB
028100030108     C                   MOVEL     'S'           WFPTRS
028200030108     C                   MOVEL     'S'           WFPBAN
028300941129     C                   MOVEL     'S'           WFPALT
028400941129     C                   ENDIF
028500941129     C     D$GSTS        IFEQ      'U'
028600990804     C                   MOVEL     'S'           WFPTAS
028700941129     C                   MOVEL     'S'           WFPCSB
028800030108     C                   MOVEL     'S'           WFPTRS
028900030108     C                   MOVEL     'S'           WFPBAN
029000941129     C                   MOVEL     'S'           WFPALT
029100990804     C                   MOVEL     'S'           WFDTAS
029200941129     C                   MOVEL     'S'           WFDCSB
029300030108     C                   MOVEL     'S'           WFDTRS
029400030108     C                   MOVEL     'S'           WFDBAN
029500941129     C                   MOVEL     'S'           WFDALT
029600941129     C                   ENDIF
029700941129     C     D$GSTS        IFEQ      'I'
029800990804     C                   MOVEL     'S'           WFDTAS
029900941129     C                   MOVEL     'S'           WFDCSB
030000030108     C                   MOVEL     'S'           WFDTRS
030100030108     C                   MOVEL     'S'           WFDBAN
030200941129     C                   MOVEL     'S'           WFDALT
030300941129     C                   ENDIF
030400941129     C                   ENDIF
030500030108     C*
030600941129     C     D$GFIL        IFEQ      'TNCSB00F'
030700990804     C                   MOVEL     'S'           WFPTAS
030800030108     C                   MOVEL     'S'           WFPTRS
030900030108     C                   MOVEL     'S'           WFPBAN
031000941129     C                   MOVEL     'S'           WFPALT
031100990804     C                   MOVEL     'S'           WFDTAS
031200030108     C                   MOVEL     'S'           WFDTRS
031300030108     C                   MOVEL     'S'           WFDBAN
031400941129     C                   MOVEL     'S'           WFDALT
031500941129     C     D$GSTS        IFEQ      'D'
031600941129     C                   MOVEL     'S'           WFPCSB
031700941129     C                   ENDIF
031800941129     C     D$GSTS        IFEQ      'U'
031900941129     C                   MOVEL     'S'           WFPCSB
032000941129     C                   MOVEL     'S'           WFDCSB
032100941129     C                   ENDIF
032200941129     C     D$GSTS        IFEQ      'I'
032300941129     C                   MOVEL     'S'           WFDCSB
032400941129     C                   ENDIF
032500941129     C                   ENDIF
032600030108     C*
032700030108     C     D$GFIL        IFEQ      'FIAR500F'
032800030108     C     D$GTRD        IFEQ      'TRS'
032900030108     C                   MOVEL     'S'           WFPTAS
033000030108     C                   MOVEL     'S'           WFPCSB
033100030108     C                   MOVEL     'S'           WFPBAN
033200030108     C                   MOVEL     'S'           WFPALT
033300030108     C                   MOVEL     'S'           WFDTAS
033400030108     C                   MOVEL     'S'           WFDCSB
033500030108     C                   MOVEL     'S'           WFDBAN
033600030108     C                   MOVEL     'S'           WFDALT
033700030108     C     D$GSTS        IFEQ      'D'
033800030108     C                   MOVEL     'S'           WFPTRS
033900030108     C                   ENDIF
034000030108     C     D$GSTS        IFEQ      'U'
034100030108     C                   MOVEL     'S'           WFPTRS
034200030108     C                   MOVEL     'S'           WFDTRS
034300030108     C                   ENDIF
034400030108     C     D$GSTS        IFEQ      'I'
034500030108     C                   MOVEL     'S'           WFDTRS
034600030108     C                   ENDIF
034700030108     C                   ENDIF
034800030108     C                   ENDIF
034900941129     C*
035000030108     C     D$GFIL        IFEQ      'FIAR500F'
035100030108     C     D$GTRD        IFEQ      'BAN'
035200030801     C     D$GTRD        OREQ      'BNB'
035300030801     C                   MOVEL     'S'           WFPTAS
035400030801     C                   MOVEL     'S'           WFPCSB
035500030801     C                   MOVEL     'S'           WFPTRS
035600030801     C                   MOVEL     'S'           WFPALT
035700030108     C                   MOVEL     'S'           WFDTAS
035800030108     C                   MOVEL     'S'           WFDCSB
035900030108     C                   MOVEL     'S'           WFDTRS
036000030108     C                   MOVEL     'S'           WFDALT
036100030108     C     D$GSTS        IFEQ      'D'
036200030108     C                   MOVEL     'S'           WFPBAN
036300030108     C                   ENDIF
036400030108     C     D$GSTS        IFEQ      'U'
036500030108     C                   MOVEL     'S'           WFPBAN
036600030108     C                   MOVEL     'S'           WFDBAN
036700030108     C                   ENDIF
036800030108     C     D$GSTS        IFEQ      'I'
036900030108     C                   MOVEL     'S'           WFDBAN
037000030108     C                   ENDIF
037100030108     C                   ENDIF
037200030108     C                   ENDIF
037300030108     C*
037400941129     C                   ENDSR
037500941129     C***********************************************************************
037600941129     C*   GESCSB - GESTIONE PER VARIAZIONE SU TNCSB10F
037700941129     C***********************************************************************
037800941129     C     GESCSB        BEGSR
037900941129     C*
038000941129     C* PORTO LE IMMAGINI DEL FILE VARIATO (TNSCB) NELLA DS DI TNSCB
038100941129     C                   MOVEL     D$PRE1        D$PCS1
038200941129     C                   MOVEL     D$PRE2        D$PCS2
038300941129     C                   MOVEL     D$DRE1        D$DCS1
038400941129     C                   MOVEL     D$DRE2        D$DCS2
038500950123     C*
038600950123     C* PORTO LE IMMAGINI DEL FILE VARIATO (TNSCB) NEI CAMPI DELLA DS$ECO
038700950123     C* PER USARLE POI EVENTUALMENTE NEGLI ERRORI
038800950123     C                   MOVEL     D$PRE1        D$PCO1
038900950123     C                   MOVEL     D$PRE2        D$PCO2
039000950123     C                   MOVEL     D$DRE1        D$DCO1
039100950123     C                   MOVEL     D$DRE2        D$DCO2
039200950119     C*
039300950119     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
039400950119     C     WFPCSB        IFEQ      'S'
039500950120     C                   Z-ADD     DCPAAS        KERAAS
039600950120     C                   Z-ADD     DCPLNP        KERLNP
039700950120     C                   Z-ADD     DCPNRS        KERNRS
039800950120     C                   Z-ADD     DCPNSP        KERNSP
039900950125     C                   MOVEL     DCPTBL        WTBL
040000950125     C                   Z-ADD     DCPMGS        WMGS
040100950119     C                   ELSE
040200950120     C                   Z-ADD     DCDAAS        KERAAS
040300950120     C                   Z-ADD     DCDLNP        KERLNP
040400950120     C                   Z-ADD     DCDNRS        KERNRS
040500950120     C                   Z-ADD     DCDNSP        KERNSP
040600950125     C                   MOVEL     DCDTBL        WTBL
040700950125     C                   Z-ADD     DCDMGS        WMGS
040800950119     C                   ENDIF
040900950120     C                   EXSR      GESERR
041000950130     C*
041100950201     C* MI CONSTRUISCO LA DATA DELLA BOLLA (DI 8)
041200950130     C                   Z-ADD     KERAAS        DSAAS
041300950130     C                   Z-ADD     WMGS          DSMGS
041400960130     C*
041500960130     C* ESCLUDE SE DATA SPEDIZIONE MINORE DATA ULTIMO TRASF. TAS10 > TASP
041600960130     C     DSDAT8        IFLE      WDT10P
041700960130     C                   MOVEL     'S'           $ERR
041800960130     C                   ENDIF
041900960130     C*
042000040123    1C     $ERR          IFEQ      'N'
042100950201     C*
042200950201     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
042300950405     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
042400040123    2C     D$GPGF        IFEQ      'TNSB23R'
042500950405     C     DSDAT8        ANDGT     DTSB23
042600950201     C                   Z-ADD     DSDAT8        DTSB23            8 0
042700040123   e2C                   ENDIF
042800950217     C*
042900040123    2C     $ERR          IFEQ      'N'
043000941129     C*
043100941129     C* IMPOSTO CAMPI DS UNIFICATA RELATIVI AL CONTRASSEGNO
043200040123    3C     WFPCSB        IFEQ      'S'
043300950207     C                   MOVEL     DCPTPI        D$PTPI
043400040521     C                   MOVEL     DCPsta        D$Psta
043500040123    4C     DCPVCA        IFNE      WDIVCE
043600011106     C                   Z-ADD     DCPCAS        WIMI             30 9
043700011106     C                   MOVEL     DCPVCA        WDVI              3
043800011106     C                   EXSR      CNVEUR
043900011106     C                   Z-ADD     WIMO          DCPCAS
044000040123   e4C                   ENDIF
044100011106     C                   Z-ADD     DCPCAS        D$PCAS
044200040123   e3C                   ENDIF
044300011106     C*
044400040123    3C     WFDCSB        IFEQ      'S'
044500950207     C                   MOVEL     DCDTPI        D$DTPI
044600040521     C                   MOVEL     DCDsta        D$Dsta
044700040123    4C     DCDVCA        IFNE      WDIVCE
044800011106     C                   Z-ADD     DCDCAS        WIMI             30 9
044900011106     C                   MOVEL     DCDVCA        WDVI              3
045000011106     C                   EXSR      CNVEUR
045100011106     C                   Z-ADD     WIMO          DCDCAS
045200040123    4C                   ENDIF
045300011106     C                   Z-ADD     DCDCAS        D$DCAS
045400040123    3C                   ENDIF
045500950125     C*
045600950125     C* SE NON VI SONO VARIAZIONI TRA I DUE CAMPI CHE CI INTERESSANO DI TNCSB
045700950125     C* E SONO IN VARIAZIONE --> ESCO PGM
045800040123    3C     D$GSTO        IFEQ      'U'
045900950207     C     D$PCAS        ANDEQ     D$DCAS
046000950125     C     D$PTPI        ANDEQ     D$DTPI
046100950125     C                   MOVEL     'S'           $ERR
046200040123    3C                   ENDIF
046300040122     c*
046400040123    3C     $ERR          IFEQ      'N'
046500040122     C* LEGGO TITAS10F E IMPOSTO SUA DS
046600040123    4C     WFPCSB        IFEQ      'S'
046700040122     C                   Z-ADD     DCPAAS        TASAAS
046800040122     C                   Z-ADD     DCPLNP        TASLNP
046900040122     C                   Z-ADD     DCPNRS        TASNRS
047000040122     C                   Z-ADD     DCPNSP        TASNSP
047100040122     C                   MOVEL     DCPTBL        TASTBL
047200040123   x4C                   ELSE
047300040122     C                   Z-ADD     DCDAAS        TASAAS
047400040122     C                   Z-ADD     DCDLNP        TASLNP
047500040122     C                   Z-ADD     DCDNRS        TASNRS
047600040122     C                   Z-ADD     DCDNSP        TASNSP
047700040122     C                   MOVEL     DCDTBL        TASTBL
047800040123    4C                   ENDIF
047900040122     C                   SETOFF                                       010203
048000040122     C     KEYTAS        CHAIN     TITAS30C                           99
048100040123    4C     *IN99         IFEQ      '0'
048200040122     C*
048300040122     C* ACCETTO VARIAZIONI DI TNCSB SOLO SE LA BOLLA NON E' IN TITASP0F
048400040122     C* INDICATORE 02 ON --> TIPO RECORD TITAS10F
048500040122     C* INDICATORE 01 ON --> TIPO RECORD TITAS00F
048600040123    5C     *IN02         IFEQ      '1'
048700040122     C     *IN01         OREQ      '1'
048800040122     C*
048900040122     C* MEMORIZZA LA DATA SPEDIZIONE DELLA BOLLA
049000040122     C                   MOVEL     TASAAS        $TASDSP           8 0
049100040122     C                   MOVE      TASMGS        $TASDSP
049200040122     C*
049300040122     C* GESTIONE TITAS PRIMA
049400040123    6C     WFPTAS        IFEQ      'S'
049500040122     C                   EXSR      CPDTAQ
049600150313     c                   EXSR      AZZnCPDTAQ
049700040123   x6C                   ELSE
049800040122     C                   EXSR      APDTAQ
049900040123    6C                   ENDIF
050000040122     C                   MOVEL     D$PDS1        D$PRE1
050100040122     C                   MOVEL     D$PDS2        D$PRE2
050200040122     C* GESTIONE TITAS DOPO
050300040123    6C     WFDTAS        IFEQ      'S'
050400040122     C                   EXSR      CDDTAQ
050500040122     c                   EXSR      AZZnCDDTAQ
050600040123   x6C                   ELSE
050700040122     C                   EXSR      ADDTAQ
050800040123    6C                   ENDIF
050900040122     C                   MOVEL     D$DDS1        D$DRE1
051000040122     C                   MOVEL     D$DDS2        D$DRE2
051100040622     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
051200040622     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
051300040622     c* titas. Perl quelle > invece verranno presi dal fiar5
051400040622     c                   if        dsdat8 < 20040719
051500040621     C                   Z-ADD     d$pflp        D$PAR5flp
051600040621     C                   Z-ADD     d$pfl2        D$PAR5fl2
051700040621     C                   Z-ADD     d$dflp        D$dAR5flp
051800040621     C                   Z-ADD     d$dfl2        D$dAR5fl2
051900040622     c                   end
052000040122     C* IMPOSTO STATUS A 'U' UPDATE
052100040122     C                   MOVEL     'U'           D$GSTS
052200040122     c*
052300040122     C* Controlla SE e COME lanciare il conto economico se record di TNCSB00F
052400040122     C                   EXSR      CHKLANCSB
052500040122     C*
052600040123   x5C                   ELSE
052700040122     C*
052800040122     C* LA BOLLA E' ANCORA IN TITAS00F OPPURE GIA' IN TITASP0F
052900040122     C* NON LA CONSIDERO ED ESCO PGM
053000040122     C                   MOVEL     'S'           $ERR
053100040122     C*
053200040123   e5c                   ENDIF
053300040122     c* aggancio FIAR500F x BANCALI E TRANSITI
053400040123    5C                   IF        $err = 'N'
053500040123     C*
053600040123     C                   Z-ADD     tasAAS        ar5AAS
053700040123     C                   Z-ADD     tasLNP        ar5LNP
053800040123     C                   Z-ADD     tasNRS        ar5NRS
053900040123     C                   Z-ADD     tasNSP        ar5NSP
054000040517     C                   Z-ADD     0             ar5prg
054100040622     c                   if        dsdat8 >= 20040719
054200040517     C                   Z-ADD     tasflp        ar5prg
054300040517     c                   end
054400040122     C* LEGGO fiar500F x transiti
054500040123    6C                   if        (D$PFLP > *zeros OR D$PFL2 > *zeros or
054600040123     C                             D$DFLP > *zeros OR D$DFL2 > *zeros)
054700040122     C                   MOVEL     'TRS'         AR5TRD
054800040617     C     KEYAR5        CHAIN     FIAR531C                           99
054900040517if  7C     *IN99         IFEQ      '0'
055000040617     C     AR5ATB        andeq     *BLANKS
055100040122     C*
055200040617if  8C                   IF        WFPTRS = 'S'
055300040519     c                   exsr      srar5trsp
055400040617e   8C                   ENDIF
055500040122     C*
055600040617if  8C                   IF        WFDTRS = 'S'
055700040519     c                   exsr      srar5trsd
055800040617e   8C                   ENDIF
055900040123    7C                   ENDIF
056000040123    6C                   ENDIF
056100040123     C* LEGGO FIAR5 RECORD BANCALI E IMPOSTO I CAMPI DELLA DS UNIFICATA
056200040123    6c                   if        %subst(d$pgva: 1: 1) = 'B' or
056300040123     c                             %subst(d$dgva: 1: 1) = 'B'
056400040122     C                   MOVEL     'BAN'         AR5TRD
056500040521     c                   clear                   ar5prg
056600040617     C     KEYAR5        CHAIN     FIAR531C                           99
056700040123if  7C     *IN99         IFEQ      '0'
056800040617     C     AR5ATB        andeq     *BLANKS
056900040122     C*
057000040617if  8C                   IF        WFPBAN = 'S'
057100040122     C                   MOVEL     AR5UNI        DAR5BAN
057200040122     C                   Z-ADD     §AR5TBA       D$PAR5TBA
057300040122     C                   Z-ADD     §AR5NBA       D$PAR5NBA
057400040122     C                   Z-ADD     §AR5TB2       D$PAR5TB2
057500040122     C                   Z-ADD     §AR5NB2       D$PAR5NB2
057600040617e   8C                   ENDIF
057700040122     C*
057800040617if  8C                   IF        WFDBAN = 'S'
057900040122     C                   MOVEL     AR5UNI        DAR5BAN
058000040122     C                   Z-ADD     §AR5TBA       D$DAR5TBA
058100040122     C                   Z-ADD     §AR5NBA       D$DAR5NBA
058200040122     C                   Z-ADD     §AR5TB2       D$DAR5TB2
058300040122     C                   Z-ADD     §AR5NB2       D$DAR5NB2
058400040123e   8C                   ENDIF
058500040123e   7C                   ENDIF
058600040123e   6C                   ENDIF
058700941129     C*
058800040122     C* LEGGO fiar500f x bancali
058900040123    6c                   if        %subst(d$pgva: 1: 1) = 'O' or
059000040123     c                             %subst(d$dgva: 1: 1) = 'O'
059100040122     C                   MOVEL     'BNB'         AR5TRD
059200040521     c                   clear                   ar5prg
059300040617     C     KEYAR5        CHAIN     FIAR531C                           99
059400040123if  7C     *IN99         IFEQ      '0'
059500040617     C     AR5ATB        andeq     *BLANKS
059600040122     C*
059700040617if  8C                   IF        WFPBAN = 'S'
059800040122     C                   MOVEL     AR5UNI        DAR5BNB
059900040122     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
060000040122     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
060100040122     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
060200040122     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
060300040218     C                   Z-ADD     §AR5bcsf      D$PAR5csf
060400040218     C                   Z-ADD     §AR5bcor      D$PAR5cor
060500040617e   8C                   ENDIF
060600040122     C*
060700040617if  8C                   IF        WFDBAN = 'S'
060800040122     C                   MOVEL     AR5UNI        DAR5BNB
060900040122     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
061000040122     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
061100040122     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
061200040122     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
061300040218     C                   Z-ADD     §AR5bcsf      D$DAR5csf
061400040218     C                   Z-ADD     §AR5bcor      D$DAR5cor
061500040617e   8C                   ENDIF
061600040123e   7C                   ENDIF
061700040123e   6C                   ENDIF
061800040123    5C                   ENDIF
061900941129     C*
062000040123   x4C                   ELSE
062100941129     C*
062200950324     C* CONTROLLO SE SI TRATTA DI UN CONTRASSEGNO OMESSO (CIOE' INSERITO
062300950324     C* DALLA SEDE ANCHE SENZA BOLLA), IN QUESTO CASO NON TRATTO IL RECORD.
062400950324     C                   MOVEL     'N'           FLCSBO            1
062500950324     C                   Z-ADD     TASAAS        CSVAAS
062600950324     C                   Z-ADD     TASLNP        CSVLNP
062700950324     C                   Z-ADD     TASNRS        CSVNRS
062800950324     C                   Z-ADD     TASNSP        CSVNSP
062900950324     C     KEYCSV        SETLL     TNCSV01L                           99
063000950324     C  N99KEYCSV        READE     TNCSV01L                               99
063100040123    5C     *IN99         DOWEQ     *OFF
063200950324     C     CSVAAS        ANDEQ     TASAAS
063300950324     C     CSVLNP        ANDEQ     TASLNP
063400950324     C     CSVNRS        ANDEQ     TASNRS
063500950324     C     CSVNSP        ANDEQ     TASNSP
063600040123    6C     CSVCAV        IFEQ      'IMCA'
063700950327     C     CSVFEV        ANDEQ     046
063800950324     C                   MOVEL     'S'           FLCSBO
063900040123    6C                   ENDIF
064000950324     C  N99KEYCSV        READE     TNCSV01L                               99
064100040123    5C                   ENDDO
064200950324     C*
064300040123    5C     FLCSBO        IFEQ      'N'
064400941130     C                   MOVEL     MSG(2)        $MSG
064500941129     C                   EXSR      SCRERR
064600040123   x5C                   ELSE
064700950324     C                   MOVEL     'S'           $ERR
064800040123    5C                   ENDIF
064900941129     C*
065000040123    4C                   ENDIF
065100040123    3C                   ENDIF
065200040123    2C                   ENDIF
065300040123    1C                   ENDIF
065400941129     C*
065500941129     C                   ENDSR
065600020605     C*-------------------------------------------------------------------------
065700020605     C* Controlla se lanciare il conto economico se record di TNCSB00F
065800020605     C*-------------------------------------------------------------------------
065900020605     C     CHKLANCSB     BEGSR
066000020605     C*---
066100020605     C* non lancia il conto economico SE:
066200020611     C* . contrassegno di una bolla di TITAS00F con data prima del x giugno 2002
066300020607     C* lancia il conto economico IN MODO:
066400020607     C* . da aggiornare solo i files come TITAS00F se il contrassengo in lì
066500020605     C*---
066600020605IF  1C     D$GFIL        IFEQ      'TNCSB00F'
066700020605     C     *IN01         ANDEQ     *ON
066800020619IF  2C     $TASDSP       IFLT      20020601
066900020605     C                   MOVEL     'S'           $ERR
067000020611X   2C                   ELSE
067100020607     C                   Z-ADD     1             I
067200020607     C     'TITAS00F  '  LOOKUP    FIL(I)                                 99
067300020607IF  3C     *IN99         IFEQ      '1'
067400020607     C                   MOVEL     SDA(I)        D$GSDA
067500020607     C                   MOVEL     SDC(I)        D$GSDC
067600020607     C                   MOVEL     SDF(I)        D$GSDF
067700020607     C                   MOVEL     SDO(I)        D$GSDO
067800020607     C                   MOVEL     SDP(I)        D$GSDP
067900020607     C                   MOVEL     SDR(I)        D$GSDR
068000020607     C                   MOVEL     SDT(I)        D$GSDT
068100040316     C                   MOVEL     SDE(I)        D$GSDE
068200020607     C                   MOVEL     CET(I)        D$GCET
068300020607     C                   MOVEL     MOV(I)        D$GWFM
068400020607     C                   MOVEL     FIM(I)        D$GFIM
068500020607     C                   MOVEL     FCE(I)        D$GFCE
068600030108     C                   MOVEL     COP(I)        D$GCOP
068700020607E   3C                   ENDIF
068800020611E   2C                   ENDIF
068900020611E   1C                   ENDIF
069000020605     C*
069100020605     C                   ENDSR
069200030108     C***********************************************************************
069300030108     C*   GESAR5 - GESTIONE PER VARIAZIONE SU FIAR500F (TRANSITI o BANCALI)
069400030108     C***********************************************************************
069500030108     C     GESAR5        BEGSR
069600030108     C*
069700030108     C* TRANSITI
069800030108IF  1C     D$GTRD        IFEQ      'TRS'
069900030108     C                   EXSR      GESTRS
070000030108E   1C                   ENDIF
070100030108     C* BANCALI
070200030108IF  1C     D$GTRD        IFEQ      'BAN'
070300030801     C     D$GTRD        OREQ      'BNB'
070400030108     C                   EXSR      GESBAN
070500030108E   1C                   ENDIF
070600030108     C*
070700030108     C                   ENDSR
070800030108     C***********************************************************************
070900030108     C*   GESTRS - GESTIONE PER VARIAZIONE SU FIAR500F -TRANSITI-
071000030108     C***********************************************************************
071100030108     C     GESTRS        BEGSR
071200030108     C*
071300030108     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIARB) NELLA DS DI FIARB TRANSITI
071400030108     C                   MOVEL     D$PRE1        D$PAT1
071500030108     C                   MOVEL     D$DRE1        D$DAT1
071600030108     C*
071700030108     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIAR5) TRANSITI NEI CAMPI DELLA
071800030108     C* DS$ECO PER USARLE POI EVENTUALMENTE NEGLI ERRORI
071900030108     C                   MOVEL     D$PRE1        D$PAR5T
072000030108     C                   MOVEL     D$DRE1        D$DAR5T
072100030108     C*
072200030108     C* LEGGO TITAS10F E IMPOSTO SUA DS
072300030109IF  1C     WFPTRS        IFEQ      'S'
072400030109     C                   Z-ADD     DATPAAS       TASAAS
072500030109     C                   Z-ADD     DATPLNP       TASLNP
072600030109     C                   Z-ADD     DATPNRS       TASNRS
072700030109     C                   Z-ADD     DATPNSP       TASNSP
072800030109X   1C                   ELSE
072900030109     C                   Z-ADD     DATDAAS       TASAAS
073000030109     C                   Z-ADD     DATDLNP       TASLNP
073100030109     C                   Z-ADD     DATDNRS       TASNRS
073200030109     C                   Z-ADD     DATDNSP       TASNSP
073300030109E   1C                   ENDIF
073400030108     C                   SETOFF                                       010203
073500030109     C     KE2TAS        CHAIN     TITAS30C                           99
073600030109IF  1C     *IN99         IFEQ      '0'
073700030108     C*
073800030109     C* ACCETTO VARIAZIONI DI FIAR5 SOLO SE LA BOLLA NON E' IN TITASP0F
073900030108     C* INDICATORE 02 ON --> TIPO RECORD TITAS10F
074000030108     C* INDICATORE 01 ON --> TIPO RECORD TITAS00F
074100030109IF  2C     *IN02         IFEQ      '1'
074200030108     C     *IN01         OREQ      '1'
074300030109     C*
074400030109     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
074500030109IF  3C     WFPTRS        IFEQ      'S'
074600030109     C                   Z-ADD     DATPAAS       KERAAS
074700030109     C                   Z-ADD     DATPLNP       KERLNP
074800030109     C                   Z-ADD     DATPNRS       KERNRS
074900030109     C                   Z-ADD     DATPNSP       KERNSP
075000030109     C                   MOVEL     TASTBL        WTBL
075100030109     C                   Z-ADD     TASMGS        WMGS
075200030109X   3C                   ELSE
075300030109     C                   Z-ADD     DATDAAS       KERAAS
075400030109     C                   Z-ADD     DATDLNP       KERLNP
075500030109     C                   Z-ADD     DATDNRS       KERNRS
075600030109     C                   Z-ADD     DATDNSP       KERNSP
075700030109     C                   MOVEL     TASTBL        WTBL
075800030109     C                   Z-ADD     TASMGS        WMGS
075900030109E   3C                   ENDIF
076000030109     C                   EXSR      GESERR
076100030109     C*
076200030109     C* MI CONSTRUISCO LA DATA DELLA BOLLA (DI 8)
076300030109     C                   Z-ADD     KERAAS        DSAAS
076400030109     C                   Z-ADD     WMGS          DSMGS
076500030109     C*
076600030109     C* ESCLUDE SE DATA SPEDIZIONE MINORE DATA ULTIMO TRASF. TAS10 > TASP
076700030109IF  3C     DSDAT8        IFLE      WDT10P
076800030109     C                   MOVEL     'S'           $ERR
076900030109E   3C                   ENDIF
077000040514     C* ESCLUDE VARIAZIONE FATTE SUI TRANSITI X SPEDIZIONE >=19/7/2004
077100040514     C* XCHÈ RECEPISCE VARIAZIONE FATTA SUL TITAS
077200040514IF  3C     DSDAT8        IFGE      20040719
077300040514     C                   MOVEL     'S'           $ERR
077400040514E   3C                   ENDIF
077500030113     C*
077600030113     C* NO ERRORI
077700030113IF  3C     $ERR          IFEQ      'N'
077800030113     C*
077900030109     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
078000030109     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
078100030109IF  4C     D$GPGF        IFEQ      'TNSB23R'
078200030109     C     DSDAT8        ANDGT     DTSB23
078300030109     C                   Z-ADD     DSDAT8        DTSB23            8 0
078400030109E   4C                   ENDIF
078500030113     C*
078600030113     C* IMPOSTO CAMPI DS UNIFICATA RELATIVI AI TRANSITI
078700030113IF  4C     WFPTRS        IFEQ      'S'
078800030113     C                   MOVEL     DATPUNI       DAR5TRS
078900040621     C                   Z-ADD     §AR5DET       D$PAR5DET
079000030113     C                   Z-ADD     §AR5DUT       D$PAR5DUT
079100030113     C                   Z-ADD     §AR5DE2       D$PAR5DE2
079200030113     C                   Z-ADD     §AR5DU2       D$PAR5DU2
079300030113E   4C                   ENDIF
079400030113     C*
079500030113IF  4C     WFDTRS        IFEQ      'S'
079600030113     C                   MOVEL     DATDUNI       DAR5TRS
079700040621     C                   Z-ADD     §AR5DET       D$DAR5DET
079800030113     C                   Z-ADD     §AR5DUT       D$DAR5DUT
079900030113     C                   Z-ADD     §AR5DE2       D$DAR5DE2
080000030113     C                   Z-ADD     §AR5DU2       D$DAR5DU2
080100030113E   4C                   ENDIF
080200030113     C*
080300030113     C* SE NON VI SONO VARIAZIONI TRA I CAMPI CHE CI INTERESSANO DI FIAR5
080400030113     C* E SONO IN VARIAZIONE --> ESCO PGM
080500030113IF  4C     D$GSTO        IFEQ      'U'
080600030113     C     D$PAR5DET     ANDEQ     D$DAR5DET
080700030113     C     D$PAR5DUT     ANDEQ     D$DAR5DUT
080800030113     C     D$PAR5DE2     ANDEQ     D$DAR5DE2
080900030113     C     D$PAR5DU2     ANDEQ     D$DAR5DU2
081000030113     C                   MOVEL     'S'           $ERR
081100030113E   4C                   ENDIF
081200030113     C*
081300030113     C* NO ERRORI
081400030113IF  4C     $ERR          IFEQ      'N'
081500030108     C*
081600030108     C* GESTIONE TITAS PRIMA
081700030113IF  5C     WFPTAS        IFEQ      'S'
081800030108     C                   EXSR      CPDTAQ
081900030108     c                   EXSR      AZZnCPDTAQ
082000030113X   5C                   ELSE
082100030108     C                   EXSR      APDTAQ
082200030113E   5C                   ENDIF
082300030108     C                   MOVEL     D$PDS1        D$PRE1
082400030108     C                   MOVEL     D$PDS2        D$PRE2
082500030108     C* GESTIONE TITAS DOPO
082600030113IF  5C     WFDTAS        IFEQ      'S'
082700030108     C                   EXSR      CDDTAQ
082800030108     c                   EXSR      AZZnCDDTAQ
082900030113X   5C                   ELSE
083000030108     C                   EXSR      ADDTAQ
083100030113E   5C                   ENDIF
083200030108     C                   MOVEL     D$DDS1        D$DRE1
083300030108     C                   MOVEL     D$DDS2        D$DRE2
083400030108     C*
083500040621     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
083600040621     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
083700040621     c* titas
083800040622     c                   if        dsdat8 < 20040719
083900040621     c                   z-add     d$pflp        d$par5flp
084000040621     c                   z-add     d$pfl2        d$par5fl2
084100040621     c                   z-add     d$dflp        d$dar5flp
084200040621     c                   z-add     d$dfl2        d$dar5fl2
084300040622     c                   end
084400030108     C* IMPOSTO STATUS A 'U' UPDATE
084500030108     C                   MOVEL     'U'           D$GSTS
084600030113E   4C                   ENDIF
084700030113E   3C                   ENDIF
084800040122     c* LEGGO FIAR500F X BAN
084900040521IF  3C                   IF        $err='N'
085000040521   3ac                   if        (%subst(d$pgva: 1: 1) = 'O' or
085100040123     c                             %subst(d$dgva: 1: 1) = 'O' or
085200040123     c                             %subst(d$pgva: 1: 1) = 'B' or
085300040123     c                             %subst(d$dgva: 1: 1) = 'B')
085400040122     c*
085500040123     C                   Z-ADD     tasAAS        AR5AAS
085600040123     C                   Z-ADD     tasLNP        AR5LNP
085700040123     C                   Z-ADD     tasNRS        AR5NRS
085800040123     C                   Z-ADD     tasNSP        AR5NSP
085900040123     c* solo se esiste il flag bancali (B) sul TITAS
086000040123    4c                   if        %subst(d$pgva: 1: 1) = 'B' or
086100040123     c                             %subst(d$dgva: 1: 1) = 'B'
086200040122     C                   MOVEL     'BAN'         AR5TRD
086300040521     c                   clear                   ar5prg
086400040617     C     KEYAR5        CHAIN     FIAR531C                           99
086500040123if  5C     *IN99         IFEQ      '0'
086600040617     C     AR5ATB        andeq     *BLANKS
086700040122     C*
086800040617if  6C                   IF        WFPBAN = 'S'
086900040122     C                   MOVEL     AR5UNI        DAR5BAN
087000040122     C                   Z-ADD     §AR5TBA       D$PAR5TBA
087100040122     C                   Z-ADD     §AR5NBA       D$PAR5NBA
087200040122     C                   Z-ADD     §AR5TB2       D$PAR5TB2
087300040122     C                   Z-ADD     §AR5NB2       D$PAR5NB2
087400040617e   6C                   ENDIF
087500040122     C*
087600040617if  6C                   IF        WFDBAN = 'S'
087700040122     C                   MOVEL     AR5UNI        DAR5BAN
087800040122     C                   Z-ADD     §AR5TBA       D$DAR5TBA
087900040122     C                   Z-ADD     §AR5NBA       D$DAR5NBA
088000040122     C                   Z-ADD     §AR5TB2       D$DAR5TB2
088100040122     C                   Z-ADD     §AR5NB2       D$DAR5NB2
088200040123e   6C                   ENDIF
088300040123e   5C                   ENDIF
088400040123e   4C                   ENDIF
088500040123     c* LEGGO FIAR500F X BNB solo se il flag bancali (O) su TITAS
088600040123    4c                   if        %subst(d$pgva: 1: 1) = 'O' or
088700040123     c                             %subst(d$dgva: 1: 1) = 'O'
088800040122     C                   MOVEL     'BNB'         AR5TRD
088900040521     c                   clear                   ar5prg
089000040617     C     KEYAR5        CHAIN     FIAR531C                           99
089100040123if  5C     *IN99         IFEQ      '0'
089200040617     C     AR5ATB        andeq     *BLANKS
089300040122     C*
089400040617if  6C                   IF        WFPBAN = 'S'
089500040122     C                   MOVEL     AR5UNI        DAR5BNB
089600040122     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
089700040122     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
089800040122     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
089900040122     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
090000040218     C                   Z-ADD     §AR5bcsf      D$PAR5csf
090100040218     C                   Z-ADD     §AR5bcor      D$PAR5cor
090200040617e   6C                   ENDIF
090300040122     C*
090400040617if  6C                   IF        WFDBAN = 'S'
090500040122     C                   MOVEL     AR5UNI        DAR5BNB
090600040122     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
090700040122     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
090800040122     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
090900040122     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
091000040218     C                   Z-ADD     §AR5bcsf      D$dAR5csf
091100040218     C                   Z-ADD     §AR5bcor      D$dAR5cor
091200040123e   6C                   ENDIF
091300040123e   5C                   ENDIF
091400040123e   4C                   ENDIF
091500040521e  3aC                   ENDIF
091600040122     C*
091700040122     C* CONTRASSEGNO
091800040122     C* LEGGO TNCSB SOLO SE FLAG CONTRASSEGNO SUL TITAS = S
091900040123if  4C     D$PFCA        IFEQ      'S'
092000040122     C     D$DFCA        OREQ      'S'
092100040122     C*
092200040122     C* LEGGO TNCSB00F E IMPOSTO I CAMPI DELLA DS UNIFICATA
092300040123     C                   Z-ADD     tasAAS        CSBAAS
092400040123     C                   Z-ADD     tasLNP        CSBLNP
092500040123     C                   Z-ADD     tasNRS        CSBNRS
092600040123     C                   Z-ADD     tasNSP        CSBNSP
092700040123     C*
092800040122     C     KEYCSB        CHAIN     TNCSB03L                           99
092900040123if  5C     *IN99         IFEQ      '0'
093000040521if  7C     WFPCSB        IFEQ      'S'
093100040521     C                   MOVEL     CSBsta        D$Psta
093200040521     c                   end
093300040521if  7C     WFdCSB        IFEQ      'S'
093400040521     C                   MOVEL     CSBsta        D$dsta
093500040521     c                   end
093600040122     C* CONTROLLO ANNULLAMENTO
093700040123if  6C     CSBSTA        IFNE      9
093800040122     C*
093900040123if  7C     WFPCSB        IFEQ      'S'
094000040122     C     D$PFCA        ANDEQ     'S'
094100040122     C                   MOVEL     CSBTPI        D$PTPI
094200040123if  8C     CSBVCA        IFNE      WDIVCE
094300040122     C                   Z-ADD     CSBCAS        WIMI
094400040122     C                   MOVEL     CSBVCA        WDVI
094500040122     C                   EXSR      CNVEUR
094600040122     C                   Z-ADD     WIMO          D$PCAS
094700040123x   8C                   ELSE
094800040122     C                   Z-ADD     CSBCAS        D$PCAS
094900040123e   8C                   ENDIF
095000040123e   7C                   ENDIF
095100040122     C*
095200040123if  7C     WFDCSB        IFEQ      'S'
095300040122     C     D$DFCA        ANDEQ     'S'
095400040122     C                   MOVEL     CSBTPI        D$DTPI
095500040123if  8C     CSBVCA        IFNE      WDIVCE
095600040122     C                   Z-ADD     CSBCAS        WIMI
095700040122     C                   MOVEL     CSBVCA        WDVI
095800040122     C                   EXSR      CNVEUR
095900040122     C                   Z-ADD     WIMO          D$DCAS
096000040123x   8c                   ELSE
096100040122     C                   Z-ADD     CSBCAS        D$DCAS
096200040123e   8C                   ENDIF
096300040122e   7C                   ENDIF
096400040122e   6C                   ENDIF
096500040122e   5C                   ENDIF
096600040122     c*
096700040122e   4C                   ENDIF
096800040122e   3C                   ENDIF
096900040122     c*
097000030113X   2C                   ELSE
097100030109     C* LA BOLLA E' GIA' IN TITASP0F NON LA CONSIDERO ED ESCO PGM
097200030108     C                   MOVEL     'S'           $ERR
097300030113E   2C                   ENDIF
097400040122     C* NON TROVATO IN TITAS
097500030113X   1C                   ELSE
097600030109     C                   MOVEL     MSG(7)        $MSG
097700030109     C                   EXSR      SCRERR
097800030113E   1C                   ENDIF
097900030108     C*
098000030108     C                   ENDSR
098100030113     C*
098200030109     C***********************************************************************
098300030109     C*   GESBAN - GESTIONE PER VARIAZIONE SU FIAR500F -BANCALI-
098400030109     C***********************************************************************
098500030109     C     GESBAN        BEGSR
098600030109     C*
098700030109     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIARB) NELLA DS DI FIARB BANCALI
098800030109     C                   MOVEL     D$PRE1        D$PAB1
098900030109     C                   MOVEL     D$DRE1        D$DAB1
099000030109     C*
099100030109     C* PORTO LE IMMAGINI DEL FILE VARIATO (FIAR5) BANCALI NEI CAMPI DELLA
099200030109     C* DS$ECO PER USARLE POI EVENTUALMENTE NEGLI ERRORI
099300030109     C                   MOVEL     D$PRE1        D$PAR5B
099400030109     C                   MOVEL     D$DRE1        D$DAR5B
099500030109     C*
099600030109     C* LEGGO TITAS10F E IMPOSTO SUA DS
099700030109IF  1C     WFPBAN        IFEQ      'S'
099800030109     C                   Z-ADD     DABPAAS       TASAAS
099900030109     C                   Z-ADD     DABPLNP       TASLNP
100000030109     C                   Z-ADD     DABPNRS       TASNRS
100100030109     C                   Z-ADD     DABPNSP       TASNSP
100200030109X   1C                   ELSE
100300030109     C                   Z-ADD     DABDAAS       TASAAS
100400030109     C                   Z-ADD     DABDLNP       TASLNP
100500030109     C                   Z-ADD     DABDNRS       TASNRS
100600030109     C                   Z-ADD     DABDNSP       TASNSP
100700030109E   1C                   ENDIF
100800030109     C                   SETOFF                                       010203
100900030109     C     KE2TAS        CHAIN     TITAS30C                           99
101000030109IF  1C     *IN99         IFEQ      '0'
101100030109     C*
101200030109     C* ACCETTO VARIAZIONI DI FIAR5 SOLO SE LA BOLLA NON E' IN TITASP0F
101300030109     C* INDICATORE 02 ON --> TIPO RECORD TITAS10F
101400030109     C* INDICATORE 01 ON --> TIPO RECORD TITAS00F
101500030109IF  2C     *IN02         IFEQ      '1'
101600030109     C     *IN01         OREQ      '1'
101700030109     C*
101800030109     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
101900030109IF  3C     WFPBAN        IFEQ      'S'
102000030109     C                   Z-ADD     DABPAAS       KERAAS
102100030109     C                   Z-ADD     DABPLNP       KERLNP
102200030109     C                   Z-ADD     DABPNRS       KERNRS
102300030109     C                   Z-ADD     DABPNSP       KERNSP
102400030109     C                   MOVEL     TASTBL        WTBL
102500030109     C                   Z-ADD     TASMGS        WMGS
102600030109X   3C                   ELSE
102700030109     C                   Z-ADD     DABDAAS       KERAAS
102800030109     C                   Z-ADD     DABDLNP       KERLNP
102900030109     C                   Z-ADD     DABDNRS       KERNRS
103000030109     C                   Z-ADD     DABDNSP       KERNSP
103100030109     C                   MOVEL     TASTBL        WTBL
103200030109     C                   Z-ADD     TASMGS        WMGS
103300030109E   3C                   ENDIF
103400030109     C                   EXSR      GESERR
103500030109     C*
103600030109     C* MI CONSTRUISCO LA DATA DELLA BOLLA (DI 8)
103700030109     C                   Z-ADD     KERAAS        DSAAS
103800030109     C                   Z-ADD     WMGS          DSMGS
103900030109     C*
104000030109     C* ESCLUDE SE DATA SPEDIZIONE MINORE DATA ULTIMO TRASF. TAS10 > TASP
104100030109IF  3C     DSDAT8        IFLE      WDT10P
104200030109     C                   MOVEL     'S'           $ERR
104300030109E   3C                   ENDIF
104400030109     C* NO ERRORI
104500030109IF  3C     $ERR          IFEQ      'N'
104600030109     C*
104700030109     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
104800030109     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
104900030109IF  4C     D$GPGF        IFEQ      'TNSB23R'
105000030109     C     DSDAT8        ANDGT     DTSB23
105100030109     C                   Z-ADD     DSDAT8        DTSB23            8 0
105200030109E   4C                   ENDIF
105300030113     C*
105400030729     C* IMPOSTO CAMPI DS UNIFICATA RELATIVI AI BANCALI
105500030801IF  4C     WFPBAN        IFEQ      'S'
105600030801IF  5C     D$GTRD        IFEQ      'BAN'
105700030801     C                   MOVEL     DABPUNI       DAR5BAN
105800030801     C                   Z-ADD     §AR5TBA       D$PAR5TBA
105900030801     C                   Z-ADD     §AR5NBA       D$PAR5NBA
106000030801     C                   Z-ADD     §AR5TB2       D$PAR5TB2
106100030801     C                   Z-ADD     §AR5NB2       D$PAR5NB2
106200030801E   5C                   ENDIF
106300030801IF  5C     D$GTRD        IFEQ      'BNB'
106400030801     C                   MOVEL     DABPUNI       DAR5BNB
106500030801     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
106600030801     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
106700030801     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
106800030801     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
106900040218     C                   Z-ADD     §AR5Bcsf      D$PAR5csf
107000040218     C                   Z-ADD     §AR5Bcor      D$PAR5cor
107100030801E   5C                   ENDIF
107200030801E   4C                   ENDIF
107300030113     C*
107400030113IF  4C     WFDBAN        IFEQ      'S'
107500030801IF  5C     D$GTRD        IFEQ      'BAN'
107600030801     C                   MOVEL     DABDUNI       DAR5BAN
107700030801     C                   Z-ADD     §AR5TBA       D$DAR5TBA
107800030801     C                   Z-ADD     §AR5NBA       D$DAR5NBA
107900030801     C                   Z-ADD     §AR5TB2       D$DAR5TB2
108000030801     C                   Z-ADD     §AR5NB2       D$DAR5NB2
108100030801E   5C                   ENDIF
108200030801IF  5C     D$GTRD        IFEQ      'BNB'
108300030801     C                   MOVEL     DABDUNI       DAR5BNB
108400030801     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
108500030801     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
108600030801     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
108700030801     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
108800040218     C                   Z-ADD     §AR5Bcsf      D$DAR5csf
108900040218     C                   Z-ADD     §AR5Bcor      D$DAR5cor
109000030801E   5C                   ENDIF
109100030801E   4C                   ENDIF
109200030113     C*
109300030113     C* SE NON VI SONO VARIAZIONI TRA I CAMPI CHE CI INTERESSANO DI FIAR5
109400030113     C* E SONO IN VARIAZIONE --> ESCO PGM
109500030113IF  4C     D$GSTO        IFEQ      'U'
109600030113     C     D$PAR5TBA     ANDEQ     D$DAR5TBA
109700030113     C     D$PAR5NBA     ANDEQ     D$DAR5NBA
109800030113     C     D$PAR5TB2     ANDEQ     D$DAR5TB2
109900030113     C     D$PAR5NB2     ANDEQ     D$DAR5NB2
110000040218     C     D$PAR5CSF     ANDEQ     D$DAR5CSF
110100040218     C     D$PAR5COR     ANDEQ     D$DAR5COR
110200030113     C                   MOVEL     'S'           $ERR
110300030113E   4C                   ENDIF
110400030113     C*
110500030113     C* NO ERRORI
110600030113IF  4C     $ERR          IFEQ      'N'
110700030109     C*
110800030109     C* GESTIONE TITAS PRIMA
110900030113IF  5C     WFPTAS        IFEQ      'S'
111000030109     C                   EXSR      CPDTAQ
111100030109     c                   EXSR      AZZnCPDTAQ
111200030113X   5C                   ELSE
111300030109     C                   EXSR      APDTAQ
111400030113E   5C                   ENDIF
111500030109     C                   MOVEL     D$PDS1        D$PRE1
111600030109     C                   MOVEL     D$PDS2        D$PRE2
111700030109     C* GESTIONE TITAS DOPO
111800030113IF  5C     WFDTAS        IFEQ      'S'
111900030109     C                   EXSR      CDDTAQ
112000030109     c                   EXSR      AZZnCDDTAQ
112100030113X   5C                   ELSE
112200030109     C                   EXSR      ADDTAQ
112300030113E   5C                   ENDIF
112400030109     C                   MOVEL     D$DDS1        D$DRE1
112500030109     C                   MOVEL     D$DDS2        D$DRE2
112600040622     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
112700040622     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
112800040622     c* titas.Per quelle > verranno presi dal fiar5
112900040622     c                   if        dsdat8 < 20040719
113000040621     C                   Z-ADD     d$pflp        D$PAR5flp
113100040621     C                   Z-ADD     d$pfl2        D$PAR5fl2
113200040621     C                   Z-ADD     d$dflp        D$dAR5flp
113300040621     C                   Z-ADD     d$dfl2        D$dAR5fl2
113400040622     c                   end
113500030109     C* IMPOSTO STATUS A 'U' UPDATE
113600030109     C                   MOVEL     'U'           D$GSTS
113700030113E   4C                   ENDIF
113800030113E   3C                   ENDIF
113900040123     C* CONTRASSEGNO
114000040123     C* LEGGO TNCSB SOLO SE FLAG CONTRASSEGNO SUL TITAS = S
114100040123if  3C     D$PFCA        IFEQ      'S'
114200040123     C     D$DFCA        OREQ      'S'
114300040123     C*
114400040123     C* LEGGO TNCSB00F E IMPOSTO I CAMPI DELLA DS UNIFICATA
114500040123     C                   Z-ADD     tasAAS        CSBAAS
114600040123     C                   Z-ADD     tasLNP        CSBLNP
114700040123     C                   Z-ADD     tasNRS        CSBNRS
114800040123     C                   Z-ADD     tasNSP        CSBNSP
114900040123     C*
115000040123     C     KEYCSB        CHAIN     TNCSB03L                           99
115100040123if  4C     *IN99         IFEQ      '0'
115200040617     C     WFPCSB        IFEQ      'S'
115300040521     C                   MOVEL     CSBsta        D$Psta
115400040521     c                   end
115500040617     C     WFdCSB        IFEQ      'S'
115600040521     C                   MOVEL     CSBsta        D$dsta
115700040521     c                   end
115800040123     C* CONTROLLO ANNULLAMENTO
115900040123if  5C     CSBSTA        IFNE      9
116000040123     C*
116100040123if  6C     WFPCSB        IFEQ      'S'
116200040123     C     D$PFCA        ANDEQ     'S'
116300040123     C                   MOVEL     CSBTPI        D$PTPI
116400040123if  7C     CSBVCA        IFNE      WDIVCE
116500040123     C                   Z-ADD     CSBCAS        WIMI
116600040123     C                   MOVEL     CSBVCA        WDVI
116700040123     C                   EXSR      CNVEUR
116800040123     C                   Z-ADD     WIMO          D$PCAS
116900040123x   7C                   ELSE
117000040123     C                   Z-ADD     CSBCAS        D$PCAS
117100040123e   7C                   ENDIF
117200040123e   6C                   ENDIF
117300040123     C*
117400040123if  6C     WFDCSB        IFEQ      'S'
117500040123     C     D$DFCA        ANDEQ     'S'
117600040123     C                   MOVEL     CSBTPI        D$DTPI
117700040123if  7C     CSBVCA        IFNE      WDIVCE
117800040123     C                   Z-ADD     CSBCAS        WIMI
117900040123     C                   MOVEL     CSBVCA        WDVI
118000040123     C                   EXSR      CNVEUR
118100040123     C                   Z-ADD     WIMO          D$DCAS
118200040123x   7c                   ELSE
118300040123     C                   Z-ADD     CSBCAS        D$DCAS
118400040123e   7C                   ENDIF
118500040123e   6C                   ENDIF
118600040123e   5C                   ENDIF
118700040123e   4C                   ENDIF
118800040123e   3C                   ENDIF
118900040123     C* LEGGO fiar500F x transiti
119000040123    3C                   if        (D$PFLP > *zeros OR D$PFL2 > *zeros or
119100040123     C                             D$DFLP > *zeros OR D$DFL2 > *zeros)
119200040123     C                   Z-ADD     tasAAS        ar5AAS
119300040123     C                   Z-ADD     tasLNP        ar5LNP
119400040123     C                   Z-ADD     tasNRS        ar5NRS
119500040123     C                   Z-ADD     tasNSP        ar5NSP
119600040521     C                   clear                   ar5prg
119700040622     c                   if        dsdat8 >= 20040719
119800040517     C                   Z-ADD     tasflp        ar5prg
119900040517     c                   end
120000040123     C*
120100040123     C                   MOVEL     'TRS'         AR5TRD
120200040617     C     KEYAR5        CHAIN     FIAR531C                           99
120300040517if  4C     *IN99         IFEQ      '0'
120400040617     C     AR5ATB        andeq     *BLANKS
120500040123     C*
120600040617     C                   IF        WFPTRS = 'S'
120700040519     c                   exsr      srar5trsp
120800040617     c                   end
120900040123     C*
121000040617     C                   IF        WFDTRS = 'S'
121100040519     c                   exsr      srar5trsd
121200040617     C                   ENDIF
121300040123e   4C                   ENDIF
121400040123e   3C                   ENDIF
121500040123     c*
121600030109X   2C                   ELSE
121700030109     C*
121800030109     C* LA BOLLA E' GIA' IN TITASP0F NON LA CONSIDERO ED ESCO PGM
121900030109     C                   MOVEL     'S'           $ERR
122000030109E   2C                   ENDIF
122100030109     C*
122200040123     C* FIAR5 NON TROVATO IN TITAS
122300030109X   1C                   ELSE
122400030113     C                   MOVEL     MSG(8)        $MSG
122500030109     C                   EXSR      SCRERR
122600030109E   1C                   ENDIF
122700030109     C*
122800030109     C                   ENDSR
122900030109     C***********************************************************************
123000990804     C*   GESTAS - GESTIONE VARIAZIONE DI TITAS00F
123100941129     C***********************************************************************
123200941129     C     GESTAS        BEGSR
123300941129     C*
123400990804     C* PORTO LE IMMAGINI DEL FILE VARIATO (TITAS) NELLA DS DI TITAS
123500040123if  2C     WFPTAS        IFEQ      'S'
123600941129     C                   MOVEL     D$PRE1        D$PDS1
123700941129     C                   MOVEL     D$PRE2        D$PDS2
123800040123     c                   end
123900040123if  2C     WFDTAS        IFEQ      'S'
124000941129     C                   MOVEL     D$DRE1        D$DDS1
124100941129     C                   MOVEL     D$DRE2        D$DDS2
124200040123     c                   end
124300030113     C* NO ERRORI
124400030113if  1C     $ERR          IFEQ      'N'
124500950119     C*
124600950119     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
124700030113if  2C     WFPTAS        IFEQ      'S'
124800950120     C                   Z-ADD     D$PAAS        KERAAS
124900950120     C                   Z-ADD     D$PLNP        KERLNP
125000950120     C                   Z-ADD     D$PNRS        KERNRS
125100950120     C                   Z-ADD     D$PNSP        KERNSP
125200950120     C                   MOVEL     D$PTBL        WTBL
125300950120     C                   Z-ADD     D$PMGS        WMGS
125400030113x   2C                   ELSE
125500950120     C                   Z-ADD     D$DAAS        KERAAS
125600950120     C                   Z-ADD     D$DLNP        KERLNP
125700950120     C                   Z-ADD     D$DNRS        KERNRS
125800950120     C                   Z-ADD     D$DNSP        KERNSP
125900950120     C                   MOVEL     D$DTBL        WTBL
126000950120     C                   Z-ADD     D$DMGS        WMGS
126100030113e   2C                   ENDIF
126200950120     C                   EXSR      GESERR
126300950201     C*
126400950201     C* COSTRUISCO LA DATA BOLLA
126500950201     C                   Z-ADD     KERAAS        DSAAS
126600950201     C                   Z-ADD     WMGS          DSMGS
126700020605     c*
126800020605     C* Controlla se lanciare il conto economico se record di TITAS00F
126900020605     C                   Z-ADD     DSDAT8        $TASDSP
127000020605     C                   EXSR      CHKLANTAS
127100950201     C*
127200950201     C* SE RICEVO BOLLA DAL PGM DI TRASFERIMENTO TAS00 -> TAS10 'TNSB23R'
127300950405     C* TENGO AGGIORNATA LA DATA + ALTA PASSATA
127400030113if  2C     D$GPGF        IFEQ      'TNSB23R'
127500950405     C     DSDAT8        ANDGT     DTSB23
127600950201     C                   Z-ADD     DSDAT8        DTSB23            8 0
127700030113e   2C                   ENDIF
127800030113     C* NO ERRORI
127900030113if  2C     $ERR          IFEQ      'N'
128000950203     C*
128100020701     C* Azzera la varia "N" immagine -PRIMA-
128200030113if  3C     WFPTAS        IFEQ      'S'
128300020701     c                   EXSR      AZZnCPDTAQ
128400020701     C                   MOVEL     D$PDS1        D$PRE1
128500020701     C                   MOVEL     D$PDS2        D$PRE2
128600030113e   3C                   ENDIF
128700020701     C*
128800020701     C* Azzera la varia "N" immagine -DOPO-
128900030113if  3C     WFDTAS        IFEQ      'S'
129000020701     c                   EXSR      AZZnCDDTAQ
129100020701     C                   MOVEL     D$DDS1        D$DRE1
129200020701     C                   MOVEL     D$DDS2        D$DRE2
129300030113e   3C                   ENDIF
129400030113     C*
129500030113     C* ESTENSIONE: TRANSITI
129600030113     C* LEGGO FIAR5 RECORD TRANSITI E IMPOSTO I CAMPI DELLA DS UNIFICATA
129700040503     C*
129800040517     C* per spesizioni prima del 19/7
129900040622     c                   if        dsdat8 < 20040719
130000040622     c* per spedizioni < 19/7/2004 imposto i nuovi campi po di transito 1 e
130100040622     c* 2 (d$par5flp, d$par5fl2, d$dar5flp, d$dar5fl2) come quelli del
130200040622     c* titas.Per quelle > verranno presi dal fiar5
130300040622     C                   Z-ADD     d$pflp        D$PAR5flp
130400040622     C                   Z-ADD     d$pfl2        D$PAR5fl2
130500040622     C                   Z-ADD     d$dflp        D$dAR5flp
130600040622     C                   Z-ADD     d$dfl2        D$dAR5fl2
130700040517     c                   exsr      srTRSold
130800040517     c                   else
130900040517     c                   exsr      srTRSnew
131000040517     c                   end
131100040503     c*
131200040503     c* non prendiamo in considerazioni i bancali e
131300040503     c* contrassegni se INSERIMENTO DI TITAS00F
131400040503     c                   movel     'S'           $rep              1
131500040503if 3ac                   if        d$gsts='I' and d$gfil='TITAS00F'
131600040503     c                   movel     'N'           $rep
131700040503     c                   endif
131800040503     c* per TREC78R deve reperire sempre FIAR5 e TNCSB
131900040503if 3ac                   if        d$gpgf='TREC78R'
132000040503     c                   movel     'S'           $rep
132100040503     c                   endif
132200040503if 3ac                   if        $rep='S'
132300030113     C*
132400040426     c* ATTUALMENTE UNA BOLLA NASCE CON I BANCALI QUINDI NON PREVEDIAMO
132500040426     c* IL CASO DI MODIFICHE DEL FLAG TASGVA SUCCESSIVE
132600030113     C* ESTENSIONE: BANCALI
132700030113     C* LEGGO FIAR5 RECORD BANCALI E IMPOSTO I CAMPI DELLA DS UNIFICATA
132800040123    3c                   if        %subst(d$pgva: 1: 1) = 'B' or
132900040123     c                             %subst(d$dgva: 1: 1) = 'B'
133000040123if  4C     WFPTAS        IFEQ      'S'
133100030113     C                   Z-ADD     D$PAAS        AR5AAS
133200030113     C                   Z-ADD     D$PLNP        AR5LNP
133300030113     C                   Z-ADD     D$PNRS        AR5NRS
133400030113     C                   Z-ADD     D$PNSP        AR5NSP
133500040123x   4C                   ELSE
133600030113     C                   Z-ADD     D$DAAS        AR5AAS
133700030113     C                   Z-ADD     D$DLNP        AR5LNP
133800030113     C                   Z-ADD     D$DNRS        AR5NRS
133900030113     C                   Z-ADD     D$DNSP        AR5NSP
134000040123e   4C                   ENDIF
134100040123     C                   MOVEL     'BAN'         AR5TRD
134200040521     c                   clear                   ar5prg
134300040617     C     KEYAR5        CHAIN     FIAR531C                           99
134400040123if  4C     *IN99         IFEQ      '0'
134500040617     C     AR5ATB        andeq     *BLANKS
134600030113     C*
134700040617if  5C                   IF        WFPBAN = 'S'
134800030113     C                   MOVEL     AR5UNI        DAR5BAN
134900030113     C                   Z-ADD     §AR5TBA       D$PAR5TBA
135000030113     C                   Z-ADD     §AR5NBA       D$PAR5NBA
135100030113     C                   Z-ADD     §AR5TB2       D$PAR5TB2
135200030113     C                   Z-ADD     §AR5NB2       D$PAR5NB2
135300040617e   5C                   ENDIF
135400030113     C*
135500040617if  5C                   IF        WFDBAN = 'S'
135600030113     C                   MOVEL     AR5UNI        DAR5BAN
135700030113     C                   Z-ADD     §AR5TBA       D$DAR5TBA
135800030113     C                   Z-ADD     §AR5NBA       D$DAR5NBA
135900030113     C                   Z-ADD     §AR5TB2       D$DAR5TB2
136000030113     C                   Z-ADD     §AR5NB2       D$DAR5NB2
136100040123e   5C                   ENDIF
136200040123e   4C                   ENDIF
136300040123e   3C                   ENDIF
136400030801     C*
136500030801     C* ESTENSIONE: BANCALI BARTOLINI
136600030801     C* LEGGO FIAR5 RECORD BANCALI BARTOLINI E IMPOSTO CAMPI DELLA DS UNIFICATA
136700040123    3c                   if        %subst(d$pgva: 1: 1) = 'O' or
136800040123     c                             %subst(d$dgva: 1: 1) = 'O'
136900040123if  4C     WFPTAS        IFEQ      'S'
137000030801     C                   Z-ADD     D$PAAS        AR5AAS
137100030801     C                   Z-ADD     D$PLNP        AR5LNP
137200030801     C                   Z-ADD     D$PNRS        AR5NRS
137300030801     C                   Z-ADD     D$PNSP        AR5NSP
137400040123x   4C                   ELSE
137500030801     C                   Z-ADD     D$DAAS        AR5AAS
137600030801     C                   Z-ADD     D$DLNP        AR5LNP
137700030801     C                   Z-ADD     D$DNRS        AR5NRS
137800030801     C                   Z-ADD     D$DNSP        AR5NSP
137900040123e   4C                   ENDIF
138000040123     C                   MOVEL     'BNB'         AR5TRD
138100040521     c                   clear                   ar5prg
138200040617     C     KEYAR5        CHAIN     FIAR531C                           99
138300040123if  4C     *IN99         IFEQ      '0'
138400040617     C     AR5ATB        andeq     *BLANKS
138500030801     C*
138600040617if  5C                   IF        WFPBAN = 'S'
138700030801     C                   MOVEL     AR5UNI        DAR5BNB
138800030801     C                   Z-ADD     §AR5BTBA      D$PAR5TBA
138900030801     C                   Z-ADD     §AR5BNBA      D$PAR5NBA
139000030801     C                   Z-ADD     §AR5BTB2      D$PAR5TB2
139100030801     C                   Z-ADD     §AR5BNB2      D$PAR5NB2
139200040218     C                   Z-ADD     §AR5BCSF      D$PAR5CSF
139300040218     C                   Z-ADD     §AR5BCOR      D$PAR5COR
139400040617e   5C                   ENDIF
139500030729     C*
139600040617if  5C                   IF        WFDBAN = 'S'
139700030729     C                   MOVEL     AR5UNI        DAR5BNB
139800030729     C                   Z-ADD     §AR5BTBA      D$DAR5TBA
139900030801     C                   Z-ADD     §AR5BNBA      D$DAR5NBA
140000030729     C                   Z-ADD     §AR5BTB2      D$DAR5TB2
140100030801     C                   Z-ADD     §AR5BNB2      D$DAR5NB2
140200040218     C                   Z-ADD     §AR5BCSF      D$DAR5CSF
140300040218     C                   Z-ADD     §AR5BCOR      D$DAR5COR
140400040123e   5C                   ENDIF
140500040123e   4C                   ENDIF
140600040123e   3C                   ENDIF
140700030113     C*
140800030113     C* CONTRASSEGNO
140900030113     C* LEGGO TNCSB SOLO SE FLAG CONTRASSEGNO SUL TITAS = S
141000030113if  3C     D$PFCA        IFEQ      'S'
141100030113     C     D$DFCA        OREQ      'S'
141200030113     C*
141300030113     C* LEGGO TNCSB00F E IMPOSTO I CAMPI DELLA DS UNIFICATA
141400030113if  4C     WFPTAS        IFEQ      'S'
141500030113     C                   Z-ADD     D$PAAS        CSBAAS
141600030113     C                   Z-ADD     D$PLNP        CSBLNP
141700030113     C                   Z-ADD     D$PNRS        CSBNRS
141800030113     C                   Z-ADD     D$PNSP        CSBNSP
141900030113x   4C                   ELSE
142000030113     C                   Z-ADD     D$DAAS        CSBAAS
142100030113     C                   Z-ADD     D$DLNP        CSBLNP
142200030113     C                   Z-ADD     D$DNRS        CSBNRS
142300030113     C                   Z-ADD     D$DNSP        CSBNSP
142400030113e   4C                   ENDIF
142500030113     C     KEYCSB        CHAIN     TNCSB03L                           99
142600030113if  4C     *IN99         IFEQ      '0'
142700040521if  7C     WFPCSB        IFEQ      'S'
142800040521     C                   MOVEL     CSBsta        D$Psta
142900040521     c                   end
143000040521if  7C     WFdCSB        IFEQ      'S'
143100040521     C                   MOVEL     CSBsta        D$dsta
143200040521     c                   end
143300030113     C* CONTROLLO ANNULLAMENTO
143400030113if  5C     CSBSTA        IFNE      9
143500030113     C*
143600040123    6C     D$PFCA        ifeq      'S'
143700040521if  7C     WFPCSB        andeq     'S'
143800030113     C                   MOVEL     CSBTPI        D$PTPI
143900030113if  7C     CSBVCA        IFNE      WDIVCE
144000030113     C                   Z-ADD     CSBCAS        WIMI
144100030113     C                   MOVEL     CSBVCA        WDVI
144200030113     C                   EXSR      CNVEUR
144300030113     C                   Z-ADD     WIMO          D$PCAS
144400030113x   7C                   ELSE
144500030113     C                   Z-ADD     CSBCAS        D$PCAS
144600030113e   7C                   ENDIF
144700030113e   6C                   ENDIF
144800030113     C*
144900040123    6C     D$DFCA        ifeq      'S'
145000040521if  7C     WFdCSB        andeq     'S'
145100030113     C                   MOVEL     CSBTPI        D$DTPI
145200030113if  7C     CSBVCA        IFNE      WDIVCE
145300030113     C                   Z-ADD     CSBCAS        WIMI
145400030113     C                   MOVEL     CSBVCA        WDVI
145500030113     C                   EXSR      CNVEUR
145600030113     C                   Z-ADD     WIMO          D$DCAS
145700030113x   7C                   ELSE
145800030113     C                   Z-ADD     CSBCAS        D$DCAS
145900030113e   7C                   ENDIF
146000030113e   6C                   ENDIF
146100030113e   5C                   ENDIF
146200030113e   4C                   ENDIF
146300030113e   3C                   ENDIF
146400040123e  3aC                   ENDIF
146500030113     C*
146600030113e   2C                   ENDIF
146700030113e   1C                   ENDIF
146800941129     C*
146900941129     C                   ENDSR
147000940824     C***********************************************************************
147100040517     C*   srTRSold - per spediz. con data < 20040719
147200940824     C***********************************************************************
147300040517     C     srTRSold      BEGSR
147400040517     C* per update e delete di TITAS00F e TITAS10F attendo arrivo di FIAR5
147500040517     c* per insert di TITAS00F attendo arrivo di FIAR5
147600040517     c* per insert di TITAS10F devo reperire FIAR5 xchè non arriva
147700040517     C*
147800040517     C                   IF        ((d$gsts='U' OR d$gsts='D') AND
147900040517    3C                             ((D$PFLP <> D$DFLP AND D$PFLP <> 0) OR
148000040517    3C                             (D$PFL2 <> D$DFL2 AND D$PFL2 <> 0)))
148100040517     C                             OR (d$gsts='I' and d$gfil='TITAS10F')
148200040517     C                             OR (d$gpgf='TREC78R')
148300040517if  4C     WFPTAS        IFEQ      'S'
148400040517     C                   Z-ADD     D$PAAS        AR5AAS
148500040517     C                   Z-ADD     D$PLNP        AR5LNP
148600040517     C                   Z-ADD     D$PNRS        AR5NRS
148700040517     C                   Z-ADD     D$PNSP        AR5NSP
148800040517x   4C                   ELSE
148900040517     C                   Z-ADD     D$DAAS        AR5AAS
149000040517     C                   Z-ADD     D$DLNP        AR5LNP
149100040517     C                   Z-ADD     D$DNRS        AR5NRS
149200040517     C                   Z-ADD     D$DNSP        AR5NSP
149300040517e   4C                   ENDIF
149400040517     c*
149500040517     C                   MOVEL     'TRS'         AR5TRD
149600040521     c                   clear                   ar5prg
149700040617     C     KEYAR5        CHAIN     FIAR531C                           99
149800040517if  4C     *IN99         IFEQ      '0'
149900040617     C     AR5ATB        andeq     *BLANKS
150000040617if  5C     WFPTRS        ifeq      'S'
150100040519     c                   exsr      srar5trsp
150200040519e   5C                   ENDIF
150300040617if  5C     WFDTRS        ifeq      'S'
150400040519     c                   exsr      srar5trsd
150500040519e   5C                   ENDIF
150600040517e   4C                   ENDIF
150700040517     C*
150800040517E   3C                   ENDIF
150900940824     C*
151000040517     C                   ENDSR
151100040517     C***********************************************************************
151200040517     C*   srTRSnew - per spediz. con data >=20040719
151300040517     c* recepisco solo le variazioni del titas (tasflp = progressivo di
151400040517     c* fiar5)
151500040517     C***********************************************************************
151600040517     C     srTRSnew      BEGSR
151700040517     C*
151800040517     C                   MOVEL     'TRS'         AR5TRD
151900040517     c                   clear                   ar5uni
152000040517     c* prima
152100040617if  1C     WFPTAS        IFEQ      'S'
152200040517     C                   Z-ADD     D$PAAS        AR5AAS
152300040517     C                   Z-ADD     D$PLNP        AR5LNP
152400040517     C                   Z-ADD     D$PNRS        AR5NRS
152500040517     C                   Z-ADD     D$PNSP        AR5NSP
152600040517     C                   Z-ADD     D$Pflp        AR5prg
152700040617     C     KEYAR5        CHAIN     FIAR531C                           99
152800040617if  2C     *IN99         IFEQ      '0'
152900040617     C     AR5ATB        andeq     *BLANKS
153000040617     C     WFPTRS        andeq     'S'
153100040519     c                   exsr      srar5trsp
153200040617e   2C                   ENDIF
153300040617e   1C                   Endif
153400040517     c* dopo
153500040517     c                   clear                   ar5uni
153600040617if  1C     WFDTAS        IFEQ      'S'
153700040517     C                   Z-ADD     D$DAAS        AR5AAS
153800040517     C                   Z-ADD     D$DLNP        AR5LNP
153900040517     C                   Z-ADD     D$DNRS        AR5NRS
154000040517     C                   Z-ADD     D$DNSP        AR5NSP
154100040517     C                   Z-ADD     D$Dflp        AR5prg
154200040617     C     KEYAR5        CHAIN     FIAR531C                           99
154300040617if  2C     *IN99         IFEQ      '0'
154400040617     C     AR5ATB        andeq     *BLANKS
154500040617     C     WFDTRS        andeq     'S'
154600040519     c                   exsr      srar5trsd
154700040617e   2C                   ENDIF
154800040617e   1C                   ENDIF
154900040517     C*
155000040517     C                   ENDSR
155100040517     C***********************************************************************
155200040519     C* srar5trsp - riempe i campi della ds$eco x transiti immagine prima
155300040517     C***********************************************************************
155400040519     C     srar5trsp     BEGSR
155500040519     C*
155600040519     C                   MOVEL     AR5UNI        DAR5TRS
155700040622     c                   if        dsdat8 >=20040719
155800040519     C                   Z-ADD     §AR5flp       D$PAR5flp
155900040519     C                   Z-ADD     §AR5fl2       D$PAR5fl2
156000040524     C                   Z-ADD     §AR5fl3       D$PAR5fl3
156100040524     C                   Z-ADD     §AR5DE3       D$PAR5DE3
156200040524     C                   Z-ADD     §AR5DU3       D$PAR5DU3
156300040524     C                   Z-ADD     §AR5fl4       D$PAR5fl4
156400040524     C                   Z-ADD     §AR5DE4       D$PAR5DE4
156500040524     C                   Z-ADD     §AR5DU4       D$PAR5DU4
156600040524     C                   Z-ADD     §AR5fl5       D$PAR5fl5
156700040524     C                   Z-ADD     §AR5DE5       D$PAR5DE5
156800040524     C                   Z-ADD     §AR5DU5       D$PAR5DU5
156900040519     c                   end
157000040519     C                   Z-ADD     §AR5DET       D$PAR5DET
157100040519     C                   Z-ADD     §AR5DUT       D$PAR5DUT
157200040519     C                   Z-ADD     §AR5DE2       D$PAR5DE2
157300040519     C                   Z-ADD     §AR5DU2       D$PAR5DU2
157400040519     C*
157500040519     C                   ENDSR
157600040519     C***********************************************************************
157700040519     C* srar5trsp - riempe i campi della ds$eco x transiti immagine dopo
157800040519     C***********************************************************************
157900040519     C     srar5trsd     BEGSR
158000040519     C*
158100040519     C                   MOVEL     AR5UNI        DAR5TRS
158200040622     c                   if        dsdat8 >=20040719
158300040519     C                   Z-ADD     §AR5flp       D$DAR5flp
158400040519     C                   Z-ADD     §AR5fl2       D$DAR5fl2
158500040524     C                   Z-ADD     §AR5fl3       D$DAR5fl3
158600040524     C                   Z-ADD     §AR5DE3       D$DAR5DE3
158700040524     C                   Z-ADD     §AR5DU3       D$DAR5DU3
158800040524     C                   Z-ADD     §AR5fl4       D$DAR5fl4
158900040524     C                   Z-ADD     §AR5DE4       D$DAR5DE4
159000040524     C                   Z-ADD     §AR5DU4       D$DAR5DU4
159100040524     C                   Z-ADD     §AR5fl5       D$DAR5fl5
159200040524     C                   Z-ADD     §AR5DE5       D$DAR5DE5
159300040524     C                   Z-ADD     §AR5DU5       D$DAR5DU5
159400040519     c                   end
159500040519     C                   Z-ADD     §AR5DET       D$DAR5DET
159600040519     C                   Z-ADD     §AR5DUT       D$DAR5DUT
159700040519     C                   Z-ADD     §AR5DE2       D$DAR5DE2
159800040519     C                   Z-ADD     §AR5DU2       D$DAR5DU2
159900040519     C*
160000040519     C                   ENDSR
160100040519     C***********************************************************************
160200040519     C*   GESTBL - GESTIONE TIPO BOLLA PRIMA E DOPO
160300040519     C***********************************************************************
160400040519     C     GESTBL        BEGSR
160500040517     C*
160600940824     C* TIPO PORTO  E  RAGGRUPPAMENTO BOLLE DA TABELLA 'TB'     PRIMA
160700970507IF  1C     D$PTBL        IFNE      *BLANKS
160800941130     C                   Z-ADD     1             I                 6 0
160900940824     C     D$PTBL        LOOKUP    TBL(I)                                 99
161000970507IF  2C     *IN99         IFEQ      '0'
161100940824     C                   MOVEL     MSG(4)        $MSG
161200941026     C                   EXSR      SCRERR
161300970507X   2C                   ELSE
161400941130     C                   MOVEL     TPO(I)        D$PTPO
161500941130     C                   MOVEL     RBL(I)        D$PRBL
161600941130     C                   MOVEL     CSF(I)        D$PCSF
161700941130     C                   MOVEL     CCS(I)        D$PCCS
161800941130     C                   MOVEL     DBO(I)        D$PDBO
161900940915     C* RECUPERO FLAG RICAVI E FLAG SPEDIZIONI
162000940824     C                   Z-ADD     1             I
162100941130     C     D$PRBL        LOOKUP    FRB(I)                                 99
162200940824     C     *IN99         IFEQ      '0'
162300940824     C                   MOVEL     MSG(12)       $MSG
162400941026     C                   EXSR      SCRERR
162500940824     C                   ELSE
162600941130     C                   MOVEL     FRI(I)        D$PFRI
162700941130     C                   MOVEL     FSP(I)        D$PFSP
162800940824     C                   END
162900970507E   2C                   END
163000970507E   1C                   ENDIF
163100940824     C*
163200940824     C*
163300940824     C* TIPO PORTO  E  RAGGRUPPAMENTO BOLLE DA TABELLA 'TB'     DOPO
163400970507IF  1C     D$DTBL        IFNE      *BLANKS
163500940824     C                   Z-ADD     1             I
163600940824     C     D$DTBL        LOOKUP    TBL(I)                                 99
163700940824     C     *IN99         IFEQ      '0'
163800940824     C                   MOVEL     MSG(4)        $MSG
163900941026     C                   EXSR      SCRERR
164000940824     C                   ELSE
164100941130     C                   MOVEL     TPO(I)        D$DTPO
164200941130     C                   MOVEL     RBL(I)        D$DRBL
164300941130     C                   MOVEL     CSF(I)        D$DCSF
164400941130     C                   MOVEL     CCS(I)        D$DCCS
164500941130     C                   MOVEL     DBO(I)        D$DDBO
164600940915     C* RECUPERO FLAG RICAVI E SPEDIZIONI
164700940824     C                   Z-ADD     1             I
164800941130     C     D$DRBL        LOOKUP    FRB(I)                                 99
164900940824     C     *IN99         IFEQ      '0'
165000940824     C                   MOVEL     MSG(12)       $MSG
165100941026     C                   EXSR      SCRERR
165200940824     C                   ELSE
165300941130     C                   MOVEL     FRI(I)        D$DFRI
165400941130     C                   MOVEL     FSP(I)        D$DFSP
165500940824     C                   END
165600940824     C                   END
165700970507E   1C                   ENDIF
165800940824     C*
165900940824     C                   ENDSR
166000020605     C*-------------------------------------------------------------------------
166100020605     C* Controlla se lanciare il conto economico se record di TITAS00F
166200020605     C*-------------------------------------------------------------------------
166300020605     C     CHKLANTAS     BEGSR
166400020605     C*---
166500020605     C* non lancia il conto economico SE:
166600020605     C* . modifica / cancellazione di TITAS00F con data prima del giugno 2002
166700020605     C*---
166800020605IF  1C     D$GFIL        IFEQ      'TITAS00F'
166900020619     C     $TASDSP       ANDLT     20020601
167000020605     C                   MOVEL     'S'           $ERR
167100020605E   1C                   ENDIF
167200020605     C*
167300020605     C                   ENDSR
167400940808     C***********************************************************************
167500940808     C*   GESLEG - GESTIONE LEGAMI BOLLA
167600940808     C***********************************************************************
167700940808     C     GESLEG        BEGSR
167800940808     C*
167900950905     C                   MOVEL     'N'           D$PFIG
168000950905     C                   MOVEL     'N'           D$DFIG
168100950905     C*
168200950905     C* SE E' BOLLA DI RIENTRO NON LA CONSIDERO COME LEGATA
168300950905     C     D$PFBR        IFEQ      'S'
168400950905     C     D$DFBR        ANDEQ     'S'
168500950915     C********** D$GPGF    ANDNE'TRECE9R'                  *CANC
168600950905     C                   ELSE
168700950905     C*
168800990804     C* SE FLAG 'HA LEGAMI' IN TITAS = 'SI' LEGGO SUL FILE LEGAMI
168900941007     C     D$PFA1        IFEQ      'S'
169000941007     C     D$DFA1        OREQ      'S'
169100941128     C* SE INSERIMENTO LEGGO CON DS DOPO ALTRIMENTI CON DS PRIMA
169200941129     C     WFDTAS        IFEQ      'S'
169300940808     C                   Z-ADD     D$DAAS        LBLAAN
169400940808     C                   Z-ADD     D$DLNP        LBLLPN
169500940808     C                   Z-ADD     D$DNRS        LBLNRN
169600940808     C                   Z-ADD     D$DNSP        LBLNSN
169700940808     C                   ELSE
169800940808     C                   Z-ADD     D$PAAS        LBLAAN
169900940808     C                   Z-ADD     D$PLNP        LBLLPN
170000940808     C                   Z-ADD     D$PNRS        LBLNRN
170100940808     C                   Z-ADD     D$PNSP        LBLNSN
170200940808     C                   ENDIF
170300941130     C     KEYLBL        CHAIN     FNLBL01L                           99
170400941007     C     *IN99         IFEQ      '0'
170500950905     C*
170600950915     C********** D$GPGF    IFEQ 'TRECE9R'                  *CANC
170700950915     C******************** MOVEL'S'       D$PFIG           *CANC
170800950915     C******************** ELSE                            *CANC
170900950905     C     D$PFBR        IFNE      'S'
171000950905     C                   MOVEL     'S'           D$PFIG
171100950905     C                   ENDIF
171200950915     C******************** ENDIF                           *CANC
171300950905     C*
171400950905     C     D$DFBR        IFNE      'S'
171500950905     C                   MOVEL     'S'           D$DFIG
171600950905     C                   ENDIF
171700950905     C*
171800940808     C* CICLO PER TROVARE LA BOLLA ORIGINALE (LA PRIMA MAMMA)
171900940808     C     *IN99         DOUEQ     '1'
172000940808     C                   Z-ADD     LBLAAP        LBLAAN
172100940808     C                   Z-ADD     LBLLPP        LBLLPN
172200940808     C                   Z-ADD     LBLNRP        LBLNRN
172300940808     C                   Z-ADD     LBLNSP        LBLNSN
172400941128     C     KEYLBL        CHAIN     FNLBL01L                           99
172500940808     C                   ENDDO
172600940808     C*
172700940808     C* LEGGO I DATI DELLA MAMMA
172800941128     C                   Z-ADD     LBLAAP        TASAAS
172900940808     C                   Z-ADD     LBLLPP        TASLNP
173000940808     C                   Z-ADD     LBLNRP        TASNRS
173100940808     C                   Z-ADD     LBLNSP        TASNSP
173200941213     C                   MOVEL     *BLANKS       TASTBL
173300990804     C     KEYTAS        SETLL     TITAS30C                           99
173400990804     C  N99              READ      TITAS30C                               99
173500941212     C     *IN99         IFEQ      '0'
173600941212     C     TASAAS        ANDEQ     LBLAAP
173700940809     C     TASLNP        ANDEQ     LBLLPP
173800940809     C     TASNRS        ANDEQ     LBLNRP
173900940809     C     TASNSP        ANDEQ     LBLNSP
174000941129     C* CARICO LA DS BOLLA ORIGINALE
174100941128     C                   EXSR      CODTAQ
174200020621     C                   EXSR      AZZnCODTAQ
174300941128     C                   MOVEL     D$ODS1        D$ORE1
174400941128     C                   MOVEL     D$ODS2        D$ORE2
174500990804     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
174600020527     C                   Z-ADD     *ZEROS        V                 6 0
174700941129     C     D$OSV1        IFNE      *BLANKS
174800941129     C                   ADD       1             V
174900941129     C                   MOVEL     D$OSV1        SVN(V)
175000011213     C                   Z-ADD     D$OVA1        WVAR             11 3
175100990806     C                   MOVE      WVAR          VAN(V)
175200941129     C                   ENDIF
175300941129     C     D$OSV2        IFNE      *BLANKS
175400941129     C                   ADD       1             V
175500941129     C                   MOVEL     D$OSV2        SVN(V)
175600011213     C                   Z-ADD     D$OVA2        WVAR
175700990806     C                   MOVE      WVAR          VAN(V)
175800941129     C                   ENDIF
175900941129     C     D$OSV3        IFNE      *BLANKS
176000941129     C                   ADD       1             V
176100941129     C                   MOVEL     D$OSV3        SVN(V)
176200011213     C                   Z-ADD     D$OVA3        WVAR
176300990806     C                   MOVE      WVAR          VAN(V)
176400941129     C                   ENDIF
176500990804     C* CARICO LE VARIE BOLLA ORIGINALE DA TITA7 (SOLO SE C'E' LA VARIA 3)
176600941129     C     D$OSV3        IFNE      *BLANKS
176700941130     C                   Z-ADD     D$OAAS        TA7AAS
176800941130     C                   Z-ADD     D$OLNP        TA7LNP
176900941130     C                   Z-ADD     D$ONRS        TA7NRS
177000941130     C                   Z-ADD     D$ONSP        TA7NSP
177100941130     C                   MOVEL     D$OTBL        TA7TBL
177200990806     C     KEYTA7        SETLL     TITA730C                           99
177300990806     C  N99KEYTA7        READE     TITA730C                               99
177400941202     C     *IN99         DOWEQ     *OFF
177500020621     C                   EXSR      AZZnTA7
177600941129     C                   ADD       1             V
177700941129     C                   MOVEL     TA7SVN        SVN(V)
177800011213     C                   Z-ADD     TA7VAN        WVAR
177900990806     C                   MOVE      WVAR          VAN(V)
178000990806     C  N99KEYTA7        READE     TITA730C                               99
178100941129     C                   ENDDO
178200941129     C*
178300941129     C                   MOVEA     SVN           D$OSVN
178400941129     C                   MOVEA     VAN           D$OVAN
178500941129     C                   ENDIF
178600941212     C*
178700990804     C                   ELSE
178800941212     C*
178900941212     C                   MOVEL     MSG(11)       $MSG
179000941212     C                   EXSR      SCRERR
179100941212     C*
179200941129     C                   ENDIF
179300940901     C*
179400941007     C                   ENDIF
179500941007     C                   ENDIF
179600950905     C                   ENDIF
179700940808     C*
179800940808     C                   ENDSR
179900941129     C***********************************************************************
180000941129     C*   GESVAR - GESTIONE VARIE BOLLA PRIMA E DOPO
180100941129     C***********************************************************************
180200941129     C     GESVAR        BEGSR
180300941129     C*
180400990804     C* LEGGO VARIE DA TITA7
180500941129     C                   EXSR      LETTA7
180600941129     C*
180700990804     C* SE IMMAGINE PRIMA DI TITA7 DA IMPOSTARE
180800941129     C     WFPALT        IFEQ      'S'
180900990804     C* IMPOSTO VARIE DI TITA7
181000941129     C                   MOVEA     SV7           SVN
181100941129     C                   MOVEA     VA7           VAN
181200941129     C* AGGIUNGO VARIE BOLLA PRIMA
181300941129     C                   Z-ADD     WULT7         V
181400941129     C     D$PSV1        IFNE      *BLANKS
181500941129     C                   ADD       1             V
181600941129     C                   MOVEL     D$PSV1        SVN(V)
181700011213     C                   Z-ADD     D$PVA1        WVAR
181800990806     C                   MOVE      WVAR          VAN(V)
181900941129     C                   ENDIF
182000941129     C     D$PSV2        IFNE      *BLANKS
182100941129     C                   ADD       1             V
182200941129     C                   MOVEL     D$PSV2        SVN(V)
182300011213     C                   Z-ADD     D$PVA2        WVAR
182400990806     C                   MOVE      WVAR          VAN(V)
182500941129     C                   ENDIF
182600941129     C     D$PSV3        IFNE      *BLANKS
182700941129     C                   ADD       1             V
182800941129     C                   MOVEL     D$PSV3        SVN(V)
182900011213     C                   Z-ADD     D$PVA3        WVAR
183000990806     C                   MOVE      WVAR          VAN(V)
183100941129     C                   ENDIF
183200941129     C* IMPOSTO VARIE PRIMA NELLA DS UNIFICATA
183300941129     C                   MOVEA     SVN           D$PSVN
183400941129     C                   MOVEA     VAN           D$PVAN
183500941129     C                   ENDIF
183600941129     C*
183700990804     C* SE IMMAGINE DOPO DI TITA7 DA GESTIRE
183800941129     C     WFDALT        IFEQ      'S'
183900990804     C* IMPOSTO VARIE DI TITA7
184000941129     C                   MOVEA     SV7           SVN
184100941129     C                   MOVEA     VA7           VAN
184200941212     C* AGGIUNGO VARIE BOLLA DOPO
184300941129     C                   Z-ADD     WULT7         V
184400941129     C     D$DSV1        IFNE      *BLANKS
184500941129     C                   ADD       1             V
184600941129     C                   MOVEL     D$DSV1        SVN(V)
184700011213     C                   Z-ADD     D$DVA1        WVAR
184800990806     C                   MOVE      WVAR          VAN(V)
184900941129     C                   ENDIF
185000941129     C     D$DSV2        IFNE      *BLANKS
185100941129     C                   ADD       1             V
185200941129     C                   MOVEL     D$DSV2        SVN(V)
185300011213     C                   Z-ADD     D$DVA2        WVAR
185400990806     C                   MOVE      WVAR          VAN(V)
185500941129     C                   ENDIF
185600941129     C     D$DSV3        IFNE      *BLANKS
185700941129     C                   ADD       1             V
185800941129     C                   MOVEL     D$DSV3        SVN(V)
185900011213     C                   Z-ADD     D$DVA3        WVAR
186000990806     C                   MOVE      WVAR          VAN(V)
186100941129     C                   ENDIF
186200941212     C* IMPOSTO DOPO PRIMA NELLA DS UNIFICATA
186300941129     C                   MOVEA     SVN           D$DSVN
186400941129     C                   MOVEA     VAN           D$DVAN
186500941129     C                   ENDIF
186600941129     C*
186700941129     C                   ENDSR
186800941129     C***********************************************************************
186900990804     C* LETTA7 - CARICA LE SIGLE E GLI IMPORTI DELLE VARIE DA TITA7
187000941129     C***********************************************************************
187100941129     C     LETTA7        BEGSR
187200941129     C*
187300941129     C                   Z-ADD     *ZEROS        WULT7             6 0
187400941129     C                   MOVEA     *BLANKS       SV7
187500941129     C                   MOVEA     *ZEROS        VA7
187600941129     C* CARICO LE VARIE ULTERIORI (UGUALE PER -PRIMA- E -DOPO-)
187700941129IF  1C     D$DSV3        IFNE      *BLANKS
187800941129     C     D$PSV3        ORNE      *BLANKS
187900941129     C* P R I M A
188000941129IF  2C     WFPTAS        IFEQ      'S'
188100941130     C                   Z-ADD     D$PAAS        TA7AAS
188200941130     C                   Z-ADD     D$PLNP        TA7LNP
188300941130     C                   Z-ADD     D$PNRS        TA7NRS
188400941130     C                   Z-ADD     D$PNSP        TA7NSP
188500941130     C                   MOVEL     D$PTBL        TA7TBL
188600941129X   2C                   ELSE
188700941130     C                   Z-ADD     D$DAAS        TA7AAS
188800941130     C                   Z-ADD     D$DLNP        TA7LNP
188900941130     C                   Z-ADD     D$DNRS        TA7NRS
189000941130     C                   Z-ADD     D$DNSP        TA7NSP
189100941130     C                   MOVEL     D$DTBL        TA7TBL
189200941129E   2C                   ENDIF
189300941129     C* CARICA VARIE
189400941129     C                   Z-ADD     *ZEROS        V
189500990806     C     KEYTA7        SETLL     TITA730C                           99
189600990806     C  N99KEYTA7        READE     TITA730C                               99
189700941202DO  2C     *IN99         DOWEQ     *OFF
189800020621     C                   EXSR      AZZnTA7
189900941129     C                   ADD       1             V
190000941129     C                   MOVEL     TA7SVN        SV7(V)
190100011213     C                   Z-ADD     TA7VAN        WVAR
190200990806     C                   MOVE      WVAR          VA7(V)
190300990806     C  N99KEYTA7        READE     TITA730C                               99
190400941129E   2C                   ENDDO
190500941129     C*
190600941129     C                   Z-ADD     V             WULT7
190700941129E   1C                   ENDIF
190800941129     C*
190900941129     C                   ENDSR
191000941129     C***********************************************************************
191100941129     C* GESASC - GESTIONE ASSICURAZIONE PER CONTO
191200941129     C***********************************************************************
191300011227     C     GESASC        BEGSR
191400011227     C*
191500011227     C* -PRIMA-
191600011227IF  1C     WFPALT        IFEQ      'S'
191700011227IF  2C     D$PIAS        IFNE      *ZEROS
191800011227IF  3C     D$PVAS        IFNE      WDIVCE
191900011227     C                   Z-ADD     D$PIAS        WIMI
192000011227     C                   MOVEL     D$PVAS        WDVI
192100011227     C                   EXSR      CNVEUR
192200011227     C                   Z-ADD     WIMO          D$PVAE
192300011227X   3C                   ELSE
192400011227     C                   Z-ADD     D$PIAS        D$PVAE
192500011227E   3C                   ENDIF
192600011227E   2C                   ENDIF
192700011227E   1C                   ENDIF
192800011227     C*
192900011227     C* -DOPO-
193000011227IF  1C     WFDALT        IFEQ      'S'
193100011227IF  2C     D$DIAS        IFNE      *ZEROS
193200011227IF  3C     D$DVAS        IFNE      WDIVCE
193300011227     C                   Z-ADD     D$DIAS        WIMI
193400011227     C                   MOVEL     D$DVAS        WDVI
193500011227     C                   EXSR      CNVEUR
193600011227     C                   Z-ADD     WIMO          D$DVAE
193700011227X   3C                   ELSE
193800011227     C                   Z-ADD     D$DIAS        D$DVAE
193900011227E   3C                   ENDIF
194000011227E   2C                   ENDIF
194100011227E   1C                   ENDIF
194200011227     C*
194300011227     C                   ENDSR
194400941129     C***********************************************************************
194500951213     C* GESSTP - GESTIONE NUMERO DI STOP (RITIRO/CONSEGNA)
194600941129     C***********************************************************************
194700941129     C     GESSTP        BEGSR
194800950203     C*
194900951213     C* ====== RITIRO ========
195000951213     C*
195100951213     C* IMPOSTA NUMERO DI STOP RITIRO (FRAZIONE DELLA QUANTITA' DA ADDEBITARE)
195200950203     C* (DA 0,001 A 1 DI 6.000 LIRE)
195300950203     C* PRIMA
195400950203     C     WFPALT        IFEQ      'S'
195500950203     C                   Z-ADD     1             D$PNSR
195600950203     C     D$PSTR        IFGT      1
195700950203     C     1             DIV       D$PSTR        D$PNSR
195800011213     C     D$PNSR        IFLT      0,00001
195900011213     C                   Z-ADD     0,00001       D$PNSR
196000950203     C                   ENDIF
196100950203     C                   ENDIF
196200950203     C                   ENDIF
196300950203     C* DOPO
196400950203     C     WFDALT        IFEQ      'S'
196500950203     C                   Z-ADD     1             D$DNSR
196600950203     C     D$DSTR        IFGT      1
196700950203     C     1             DIV       D$DSTR        D$DNSR
196800011213     C     D$DNSR        IFLT      0,00001
196900011213     C                   Z-ADD     0,00001       D$DNSR
197000950203     C                   ENDIF
197100950203     C                   ENDIF
197200950203     C                   ENDIF
197300951213     C*
197400951213     C* ====== CONSEGNA =======
197500951213     C*
197600951213     C* IMPOSTA NUMERO DI STOP CONS. (FRAZIONE DELLA QUANTITA' DA ADDEBITARE)
197700951213     C* (DA 0,001 A 1 DI 6.000 LIRE)
197800951213     C*
197900951213     C* PRIMA
198000951213     C     WFPALT        IFEQ      'S'
198100951213     C                   Z-ADD     1             D$PNSC
198200951213     C     D$PSTP        IFGT      1
198300951213     C     1             DIV       D$PSTP        D$PNSC
198400011213     C     D$PNSC        IFLT      0,00001
198500011213     C                   Z-ADD     0,00001       D$PNSC
198600951213     C                   ENDIF
198700951213     C                   ENDIF
198800951213     C                   ENDIF
198900951213     C*
199000951213     C* DOPO
199100951213     C     WFDALT        IFEQ      'S'
199200951213     C                   Z-ADD     1             D$DNSC
199300951213     C     D$DSTP        IFGT      1
199400951213     C     1             DIV       D$DSTP        D$DNSC
199500011213     C     D$DNSC        IFLT      0,00001
199600011213     C                   Z-ADD     0,00001       D$DNSC
199700951213     C                   ENDIF
199800951213     C                   ENDIF
199900951213     C                   ENDIF
200000950203     C*
200100950203     C                   ENDSR
200200030113     C***********************************************************************
200300030113     C* GESGCP - GESTIONE GIACENZA
200400030113     C***********************************************************************
200500030113     C     GESGCP        BEGSR
200600030113     C*
200700030113     C* LEGGE LA GIACENZA SOLO SE AVUTA
200800030221IF  1C     D$DFGC        IFEQ      'S'
200900030221     C     D$PFGC        OREQ      'S'
201000030113     C* PRIMA
201100030113IF  2C     WFPTAS        IFEQ      'S'
201200030113     C                   Z-ADD     D$PAAS        GCPAAS
201300030113     C                   Z-ADD     D$PLNP        GCPLNP
201400030113     C                   Z-ADD     D$PNRS        GCPNRS
201500030113     C                   Z-ADD     D$PNSP        GCPNSP
201600030113     C                   Z-ADD     *ZEROS        GCPFRG
201700030113     C* DOPO
201800030113X   2C                   ELSE
201900030113     C                   Z-ADD     D$DAAS        GCPAAS
202000030113     C                   Z-ADD     D$DLNP        GCPLNP
202100030113     C                   Z-ADD     D$DNRS        GCPNRS
202200030113     C                   Z-ADD     D$DNSP        GCPNSP
202300030113     C                   Z-ADD     *ZEROS        GCPFRG
202400030113E   2C                   ENDIF
202500030113     C* ESISTE GIACENZA
202600050309     C     KEYGCP        CHAIN     TIGCP51L                           99
202700030113IF  2C     *IN99         IFEQ      *OFF
202800030113     C*
202900030113     C* SE IMMAGINE PRIMA DI FIGCP DA IMPOSTARE
203000030113IF  3C     WFPALT        IFEQ      'S'
203100030113     C                   Z-ADD     GCPAGC        D$PAGC
203200030113     C                   Z-ADD     GCPMGC        D$PMGC
203300030113E   3C                   ENDIF
203400030113     C*
203500030113     C* SE IMMAGINE DOPO DI FIGCP DA IMPOSTARE
203600030113IF  3C     WFDALT        IFEQ      'S'
203700030113     C                   Z-ADD     GCPAGC        D$DAGC
203800030113     C                   Z-ADD     GCPMGC        D$DMGC
203900030113E   3C                   ENDIF
204000030113     C*
204100030113E   2C                   ENDIF
204200030113E   1C                   ENDIF
204300030113     C*
204400030113     C                   ENDSR
204500950120     C***********************************************************************
204600950120     C*   GESERR - GESTIONE ERRORI PRECEDENTI PER LA STESSA BOLLA
204700950120     C***********************************************************************
204800950120     C     GESERR        BEGSR
204900950120     C*
205000950120     C* LA CHIAVE E' GIA' STATA IMPOSTATA NELLE SUBROUTINE PRECEDENTI.
205100950123     C                   Z-ADD     *ZEROS        KERDAM
205200950123     C                   Z-ADD     *ZEROS        KERORM
205300950120     C*
205400950120     C* CONTROLLA SE E' GIA' STATO REGISTRATO UN ERRORE PER QUESTA BOLLA
205500950120     C     KEYERR        SETLL     ECERR03L                           99
205600950609     C  N99              READ(N)   ECERR03L                               99
205700950124     C     *IN99         IFEQ      '0'
205800950120     C     ERRAAS        ANDEQ     KERAAS
205900950120     C     ERRLNP        ANDEQ     KERLNP
206000950120     C     ERRNRS        ANDEQ     KERNRS
206100950120     C     ERRNSP        ANDEQ     KERNSP
206200030407     c*
206300030407     c     errpgf        ifne      'TREC90R'
206400950411     C                   MOVEL     MSG(5)        $MSG
206500950411     C                   EXSR      SCRERR
206600030407     C                   endif
206700950124     C                   ENDIF
206800950120     C*
206900950120     C                   ENDSR
207000941026     C*------------------------------------------------------------------------*
207100941026     C*?SCRERR - SCRIVE UN RECORD DI ERRORE NEL FILE SLSDE (ERRORI SALDI)
207200941026     C*------------------------------------------------------------------------*
207300941026     C     SCRERR        BEGSR
207400941026     C*
207500941026     C                   MOVEL     'S'           $ERR
207600941026     C*
207700941026     C* SCRIVE SOLO SE NON VI E' GIA' STATO ALTRO ERRORE (IN QUESTO O ALTRO PGM)
207800941026     C     D$GERR        IFEQ      *BLANKS
207900941026     C*
208000941026     C                   EXSR      SUBDAT
208100941026     C                   Z-ADD     DSDAT8        ERRDAT
208200941026     C                   Z-ADD     DTORA6        ERRTIM
208300941026     C                   ADD       1             D$GPRE
208400941026     C                   Z-ADD     D$GPRE        ERRPRG
208500950120     C                   Z-ADD     KERAAS        ERRAAS
208600950120     C                   MOVEL     WTBL          ERRTBL
208700950120     C                   Z-ADD     KERLNP        ERRLNP
208800950120     C                   Z-ADD     KERNRS        ERRNRS
208900950120     C                   Z-ADD     KERNSP        ERRNSP
209000950120     C                   Z-ADD     KERAAS        DSAAS
209100950120     C                   Z-ADD     WMGS          DSMGS
209200950120     C                   Z-ADD     DSDAT8        ERRDSP
209300941026     C                   MOVEL     $MSG          DSMSG
209400941026     C                   MOVEL     DSDER         ERRDER
209500941026     C                   MOVEL     DSCER         ERRCER
209600950123     C                   MOVEL     D$GSTO        ERRSTS
209700941026     C                   Z-ADD     D$GDAT        ERRDAM
209800941026     C                   Z-ADD     D$GTIM        ERRORM
209900941026     C                   MOVEL     D$GUSR        ERRUSR
210000941026     C                   MOVEL     D$GPGF        ERRPGF
210100950118     C                   MOVEL     D$GFIL        ERRFIL
210200950118     C                   MOVEL     D$GMBR        ERRMBR
210300950118     C                   MOVEL     D$GLIB        ERRLIB
210400030109     C*
210500030109     C     D$GFIL        IFEQ      'FIAR500F'
210600030109     C     D$GTRD        IFEQ      'TRS'
210700030109     C                   MOVEL     D$PAR5T       ERRRP1
210800030109     C                   MOVEL     D$DAR5T       ERRRD1
210900030109     C                   ENDIF
211000030109     C     D$GTRD        IFEQ      'BAN'
211100030801     C     D$GTRD        OREQ      'BNB'
211200030801     C                   MOVEL     D$PAR5B       ERRRP1
211300030801     C                   MOVEL     D$DAR5B       ERRRD1
211400030801     C                   ENDIF
211500030801     C                   ENDIF
211600030801     C*
211700950123     C     D$GFIL        IFEQ      'TNCSB00F'
211800030801     C                   MOVEL     D$PCO1        ERRRP1
211900030801     C                   MOVEL     D$PCO2        ERRRP2
212000030801     C                   MOVEL     D$DCO1        ERRRD1
212100030801     C                   MOVEL     D$DCO2        ERRRD2
212200030801     C                   ENDIF
212300030801     C*
212400030801     C     D$GFIL        IFEQ      'TITAS10F'
212500020527     C     D$GFIL        OREQ      'TITAS00F'
212600950123     C                   MOVEL     D$PRE1        ERRRP1
212700950123     C                   MOVEL     D$PRE2        ERRRP2
212800950123     C                   MOVEL     D$DRE1        ERRRD1
212900950123     C                   MOVEL     D$DRE2        ERRRD2
213000950123     C                   ENDIF
213100030109     C*
213200941026     C                   WRITE     ECERR
213300941026     C*
213400941026     C                   MOVEL     DSCER         D$GERR
213500941026     C                   MOVEL     DSDER         D$GMSG
213600941026     C*
213700941026     C                   ENDIF
213800941026     C*
213900941026     C                   ENDSR
214000941026     C*------------------------------------------------------------------------*
214100941026     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
214200941026     C*------------------------------------------------------------------------*
214300941026     C     SUBDAT        BEGSR
214400941026     C*
214500941026     C                   TIME                    DTTIME
214600941026     C                   Z-ADD     DTANN         DSANN
214700941026     C                   Z-ADD     DTMES         DSMES
214800941026     C                   Z-ADD     DTGIO         DSGIO
214900941026     C                   Z-ADD     DTSEC         DSSEC
215000941026     C*
215100941026     C                   ENDSR
215200940808     C********************************************************************
215300940808     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
215400940808     C********************************************************************
215500940808     C     AZZW          BEGSR
215600940808     C*
215700040123     C* AZZERO CAMPI DS ESTERNA
215800040123     c* è possibile usare all'interno del pgm solo i campi prima e dopo
215900040123     c* del TITAS senza testare i relativi flag prima o dopo.
216000040123     c* Se si vogliono usare i campi degli altri files senza testare i
216100040123     c* flag prima o dopo, bisogna inserire le routine di inizializzazione
216200040123     c* contenute nel /COPY
216300040123     C                   EXSR      APDTAQ
216400040123     C                   EXSR      ADDTAQ
216500040123     C                   EXSR      AODTAQ
216600941201     C                   MOVEL     D$ODS1        D$ORE1
216700941201     C                   MOVEL     D$ODS2        D$ORE2
216800950123     C                   MOVEL     *BLANKS       D$PCO1
216900950123     C                   MOVEL     *BLANKS       D$PCO2
217000950123     C                   MOVEL     *BLANKS       D$DCO1
217100950123     C                   MOVEL     *BLANKS       D$DCO2
217200941129     C                   MOVEL     *BLANKS       D$PSVN
217300941129     C                   MOVEL     *BLANKS       D$PVAN
217400941129     C                   MOVEL     *BLANKS       D$DSVN
217500941129     C                   MOVEL     *BLANKS       D$DVAN
217600941129     C                   MOVEL     *BLANKS       D$OSVN
217700941129     C                   MOVEL     *BLANKS       D$OVAN
217800941129     C                   Z-ADD     *ZEROS        D$PCAS
217900941129     C                   MOVEL     *BLANKS       D$PTPI
218000040521     C                   MOVEL     *BLANKS       D$Psta
218100941129     C                   Z-ADD     *ZEROS        D$DCAS
218200941129     C                   MOVEL     *BLANKS       D$DTPI
218300040521     C                   MOVEL     *BLANKS       D$Dsta
218400941129     C                   Z-ADD     *ZEROS        D$PVAE
218500941129     C                   Z-ADD     *ZEROS        D$DVAE
218600941209     C                   Z-ADD     *ZEROS        D$PNSR
218700941209     C                   Z-ADD     *ZEROS        D$DNSR
218800030113     C                   Z-ADD     *ZEROS        D$PAGC
218900030113     C                   Z-ADD     *ZEROS        D$PMGC
219000030113     C                   Z-ADD     *ZEROS        D$DAGC
219100030113     C                   Z-ADD     *ZEROS        D$DMGC
219200941130     C                   MOVEL     *BLANKS       D$PTPO
219300941130     C                   MOVEL     *BLANKS       D$PRBL
219400970507     C                   MOVEL     'N'           D$PCSF
219500970507     C                   MOVEL     'N'           D$PCCS
219600970507     C                   MOVEL     'N'           D$PDBO
219700970507     C                   MOVEL     'N'           D$PFRI
219800970507     C                   MOVEL     'N'           D$PFSP
219900941130     C                   MOVEL     *BLANKS       D$DTPO
220000941130     C                   MOVEL     *BLANKS       D$DRBL
220100970507     C                   MOVEL     'N'           D$DCSF
220200970507     C                   MOVEL     'N'           D$DCCS
220300970507     C                   MOVEL     'N'           D$DDBO
220400970507     C                   MOVEL     'N'           D$DFRI
220500970507     C                   MOVEL     'N'           D$DFSP
220600030108     C                   MOVEL     *BLANKS       D$PAR5T
220700030108     C                   MOVEL     *BLANKS       D$DAR5T
220800030108     C                   MOVEL     *BLANKS       D$PAR5B
220900030108     C                   MOVEL     *BLANKS       D$DAR5B
221000040517     C                   Z-ADD     *ZEROS        D$PAR5flp
221100040517     C                   Z-ADD     *ZEROS        D$PAR5DET
221200030113     C                   Z-ADD     *ZEROS        D$PAR5DUT
221300040517     C                   Z-ADD     *ZEROS        D$PAR5fl2
221400030113     C                   Z-ADD     *ZEROS        D$PAR5DE2
221500030113     C                   Z-ADD     *ZEROS        D$PAR5DU2
221600040517     C                   Z-ADD     *ZEROS        D$PAR5fl3
221700040517     C                   Z-ADD     *ZEROS        D$PAR5DE3
221800040517     C                   Z-ADD     *ZEROS        D$PAR5DU3
221900040517     C                   Z-ADD     *ZEROS        D$PAR5fl4
222000040517     C                   Z-ADD     *ZEROS        D$PAR5DE4
222100040517     C                   Z-ADD     *ZEROS        D$PAR5DU4
222200040517     C                   Z-ADD     *ZEROS        D$PAR5fl5
222300040517     C                   Z-ADD     *ZEROS        D$PAR5DE5
222400040517     C                   Z-ADD     *ZEROS        D$PAR5DU5
222500040517     C                   Z-ADD     *ZEROS        D$DAR5flp
222600030113     C                   Z-ADD     *ZEROS        D$DAR5DET
222700030113     C                   Z-ADD     *ZEROS        D$DAR5DUT
222800040517     C                   Z-ADD     *ZEROS        D$DAR5fl2
222900040517     C                   Z-ADD     *ZEROS        D$DAR5DE2
223000030113     C                   Z-ADD     *ZEROS        D$DAR5DU2
223100040517     C                   Z-ADD     *ZEROS        D$DAR5fl3
223200040517     C                   Z-ADD     *ZEROS        D$DAR5DE3
223300040517     C                   Z-ADD     *ZEROS        D$DAR5DU3
223400040517     C                   Z-ADD     *ZEROS        D$DAR5fl4
223500040517     C                   Z-ADD     *ZEROS        D$DAR5DE4
223600040517     C                   Z-ADD     *ZEROS        D$DAR5DU4
223700040517     C                   Z-ADD     *ZEROS        D$DAR5fl5
223800040517     C                   Z-ADD     *ZEROS        D$DAR5DE5
223900040517     C                   Z-ADD     *ZEROS        D$DAR5DU5
224000030113     C                   Z-ADD     *ZEROS        D$PAR5TBA
224100030113     C                   Z-ADD     *ZEROS        D$PAR5NBA
224200030113     C                   Z-ADD     *ZEROS        D$PAR5TB2
224300030113     C                   Z-ADD     *ZEROS        D$PAR5NB2
224400030113     C                   Z-ADD     *ZEROS        D$DAR5TBA
224500030113     C                   Z-ADD     *ZEROS        D$DAR5NBA
224600030113     C                   Z-ADD     *ZEROS        D$DAR5TB2
224700030113     C                   Z-ADD     *ZEROS        D$DAR5NB2
224800040218     C                   Z-ADD     *ZEROS        D$PAR5CSF
224900040218     C                   Z-ADD     *ZEROS        D$PAR5COR
225000040218     C                   Z-ADD     *ZEROS        D$DAR5CSF
225100040218     C                   Z-ADD     *ZEROS        D$DAR5COR
225200941129     C                   MOVEL     *BLANKS       D$XXXX
225300940808     C*
225400940808     C                   ENDSR
225500011106     C*------------------------------------------------------------------------*
225600011106     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
225700011106     C*------------------------------------------------------------------------*
225800011106     C     CNVEUR        BEGSR
225900011106     C*
226000011106     C* AZZERA VARIABILI DI LAVORO
226100011106     C                   Z-ADD     *ZEROS        WIMO             30 9
226200011106     C*
226300011106IF  1C     WIMI          IFNE      *ZEROS
226400011106     C                   CLEAR                   YEUR
226500011106     C                   MOVEL     WDVI          YECDVI
226600011106     C                   Z-ADD     WIMI          YECIMI
226700011106     C                   MOVEL     WDIVCE        YECDVO
226800011213     C                   MOVEL     '3'           YECDCO
226900011213     C                   MOVEL     'H'           YECTAR
227000011106     C                   CALL      'YEURCO'
227100011106     C                   PARM                    YEUR
227200011106IF  2C     YECESI        IFEQ      ' '
227300011106     C                   Z-ADD     YECIMO        WIMO
227400011106X   2C                   ELSE
227500011106     C                   Z-ADD     WIMI          WIMO
227600011106E   2C                   ENDIF
227700011106E   1C                   ENDIF
227800011106     C*
227900011106     C                   ENDSR
228000930414     C********************************************************************
228100931105     C*   RUTINZ - ROUTINE INIZIALE
228200930414     C********************************************************************
228300931105     C     RUTINZ        BEGSR
228400930414     C*
228500930414     C* PARAMETRI IN INPUT ALLA ROUTINE
228600930414     C     *ENTRY        PLIST
228700940808     C                   PARM                    DS$ECO
228800971229     C                   PARM                    DS£ECO
228900930414     C*
229000930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
229100930414     C     KEYTAB        KLIST
229200930414     C                   KFLD                    KTBUTE
229300930414     C                   KFLD                    KTBCOD
229400930414     C                   KFLD                    KTBKEY
229500011106     C*
229600011106     C* CHIAVE LETTURA TNTBE01L - PARZIALE
229700011106     C     KEYTBE        KLIST
229800011106     C                   KFLD                    KTECOD
229900011106     C                   KFLD                    KTEKE1
230000941129     C*
230100950324     C* CHIAVE LETTURA TNCSB03L - COMPLETA
230200941129     C     KEYCSB        KLIST
230300941130     C                   KFLD                    CSBAAS
230400030109     C                   KFLD                    CSBLNP
230500941129     C                   KFLD                    CSBNRS
230600941129     C                   KFLD                    CSBNSP
230700950324     C*
230800950324     C* CHIAVE LETTURA TNCSV01L - PARZIALE
230900950324     C     KEYCSV        KLIST
231000950324     C                   KFLD                    CSVAAS
231100950324     C                   KFLD                    CSVLNP
231200950324     C                   KFLD                    CSVNRS
231300950324     C                   KFLD                    CSVNSP
231400940808     C*
231500941129     C* CHIAVE LETTURA FLLBL01L - COMPLETA
231600940808     C     KEYLBL        KLIST
231700940809     C                   KFLD                    LBLAAN
231800940809     C                   KFLD                    LBLLPN
231900940809     C                   KFLD                    LBLNRN
232000940809     C                   KFLD                    LBLNSN
232100940809     C*
232200990804     C* CHIAVE LETTURA TITAS30C - COMPLETA
232300940809     C     KEYTAS        KLIST
232400940809     C                   KFLD                    TASAAS
232500940809     C                   KFLD                    TASLNP
232600940809     C                   KFLD                    TASNRS
232700940809     C                   KFLD                    TASNSP
232800940809     C                   KFLD                    TASTBL
232900030109     C*
233000030109     C* CHIAVE LETTURA TITAS30C - PARZIALE
233100030109     C     KE2TAS        KLIST
233200030109     C                   KFLD                    TASAAS
233300030109     C                   KFLD                    TASLNP
233400030109     C                   KFLD                    TASNRS
233500030109     C                   KFLD                    TASNSP
233600941130     C*
233700990806     C* CHIAVE LETTURA TITA730C - COMPLETA
233800941130     C     KEYTA7        KLIST
233900941130     C                   KFLD                    TA7AAS
234000941130     C                   KFLD                    TA7LNP
234100941130     C                   KFLD                    TA7NRS
234200941130     C                   KFLD                    TA7NSP
234300941130     C                   KFLD                    TA7TBL
234400030108     C*
234500030108     C* CHIAVE LETTURA FIAR503C - PARZIALE
234600030108     C     KEYAR5        KLIST
234700030108     C                   KFLD                    AR5AAS
234800030108     C                   KFLD                    AR5LNP
234900030108     C                   KFLD                    AR5NRS
235000030108     C                   KFLD                    AR5NSP
235100030108     C                   KFLD                    AR5TRD
235200040521     C                   KFLD                    AR5prg
235300030113     C*
235400050309     C* CHIAVE LETTURA TIGCP51L - COMPLETA
235500030113     C     KEYGCP        KLIST
235600030113     C                   KFLD                    GCPAAS
235700030113     C                   KFLD                    GCPLNP
235800030113     C                   KFLD                    GCPNRS
235900030113     C                   KFLD                    GCPNSP
236000030113     C                   KFLD                    GCPFRG
236100941026     C*
236200950120     C* CHIAVE LETTURA ECERR03L - PARZIALE
236300941026     C     KEYERR        KLIST
236400950120     C                   KFLD                    KERAAS
236500950120     C                   KFLD                    KERLNP
236600950120     C                   KFLD                    KERNRS
236700950120     C                   KFLD                    KERNSP
236800950120     C                   KFLD                    KERDAM
236900950120     C                   KFLD                    KERORM
237000930414     C*
237100931111     C* DEFINISCO CAMPI
237200930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
237300930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
237400930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
237500011106     C     *LIKE         DEFINE    TBECOD        KTECOD
237600011106     C     *LIKE         DEFINE    TBEKE1        KTEKE1
237700950120     C     *LIKE         DEFINE    ERRAAS        KERAAS
237800950120     C     *LIKE         DEFINE    ERRLNP        KERLNP
237900950120     C     *LIKE         DEFINE    ERRNRS        KERNRS
238000950120     C     *LIKE         DEFINE    ERRNSP        KERNSP
238100950120     C     *LIKE         DEFINE    ERRDAM        KERDAM
238200950120     C     *LIKE         DEFINE    ERRORM        KERORM
238201150313     C     *LIKE         DEFINE    d$dpkb        compes
238202150313     C     *LIKE         DEFINE    d$dpvl        compvl
238203150313     C     *LIKE         DEFINE    d$dvlb        comvol
238300950120     C     *LIKE         DEFINE    ERRTBL        WTBL
238400950120     C     *LIKE         DEFINE    DSMGS         WMGS
238500130409     C*    *DTAARA       DEFINE                  TREC00
238600931105     C*
238700931105     C* IMPOSTO CAMPI DI GESTIONE PGM
238800941021     C                   MOVEL     'N'           $ERR              1
238900950117     C                   MOVEL     *BLANKS       $MSG             78
239000940411     C                   MOVEL     'N'           FLAGGS            1
239100930414     C*
239200930414     C* RECUPERO IL CODICE DELL'AZIENDA
239300930414     C                   Z-ADD     1             CODUT             1 0
239400991116     C*
239500991116     C* RECUPERA I CAMPI PER LA SIMULAZIONE DEL BUDGET
239600140519     C*                  MOVEL     D$GXXX        DSX
239700140519     C*                  MOVEL     DSXFSI        D$GFSI            1
239800140519     C*                  MOVE      DSXASI        D$GASI            4 0
239900140519     C                   MOVEL     D$GXXX        dsd$g
240000140519     C                   MOVEL     £D$GFSI       D$GFSI            1
240100140519     C                   MOVE      £D$GASI       D$GASI            4 0
240200140519     C                   MOVE      £D$GFSF       D$GFSF            1
240300931104     C*
240400971203     C* CARICA LE TABELLE NECESSARIE AL PGM + REPERIM.LISTA LIB.
240500931104     C* SOLO NEL CASO IN CUI NON SIANO ANCORA STATE CARICATE
240600931104     C     CTABSN        IFNE      'S'
240700931104     C                   MOVEL     'S'           CTABSN            1
240800931111     C                   EXSR      CARTAB
240900020805     C                   ENDIF
241000940103     C*
241100020528     C* CARICA I FLAG AGG. SALDI DALLA TABELLA 2V SE NON PRESENTI NELLA DS INPUT
241200940103     C                   EXSR      CARFLG
241300930414     C*
241400930414     C                   ENDSR
241500930414     C********************************************************************
241600931111     C*   CARTAB - CARICA LE TABELLE
241700930414     C********************************************************************
241800930414     C     CARTAB        BEGSR
241900941025     C*
242000011106     C* APRE I FILES
242100941025     C                   OPEN      TABEL00F
242200011106     C                   OPEN      TNTBE01L
242300011106     C*
242400011106     C* REPERISCE LA DATA CORRENTE
242500011106     C                   TIME                    N14              14 0
242600011106     C                   MOVE      N14           N8                8 0
242700011106     C                   Z-ADD     N8            G08DAT
242800011106     C                   Z-ADD     *ZEROS        G08INV
242900011106     C                   MOVEL     '0'           G08ERR
243000011106     C                   CALL      'XSRDA8'
243100011106     C                   PARM                    WLBDA8
243200011106     C                   Z-ADD     G08INV        DATCOR            8 0
243300930414     C*
243400931104     C* CARICA TABELLA -TB- TIPI BOLLA
243500941021     C     $ERR          IFEQ      'N'
243600930421     C                   Z-ADD     0             I
243700930414     C                   Z-ADD     CODUT         KTBUTE
243800931104     C                   MOVEL     'TB'          KTBCOD
243900930414     C                   MOVEL     *BLANKS       KTBKEY
244000930414     C     KEYTAB        SETLL     TABEL00F                           99
244100950203     C  N99              READ(N)   TABEL00F                               99
244200930414     C     *IN99         IFEQ      '1'
244300931104     C                   MOVEL     MSG(1)        $MSG
244400941026     C                   EXSR      SCRERR
244500930414     C                   END
244600931104     C     *IN99         DOWEQ     '0'
244700931104     C     TBLCOD        ANDEQ     'TB'
244800931104     C     TBLFLG        IFEQ      *BLANKS
244900930421     C                   ADD       1             I
245000931104     C                   MOVEL     TBLKEY        TBL(I)
245100931104     C                   MOVEL     TBLUNI        DSTB
245200931104     C                   MOVEL     §TBTPO        TPO(I)
245300931104     C                   MOVEL     §TBRBL        RBL(I)
245400940808     C                   MOVEL     §TBCSF        CSF(I)
245500940930     C                   MOVEL     §TBCCS        CCS(I)
245600940808     C                   MOVEL     §TBDBO        DBO(I)
245700931104     C                   END
245800950203     C                   READ(N)   TABEL00F                               99
245900931104     C                   END
246000930414     C                   END
246100940824     C*
246200940824     C* CARICA TABELLA -2W- FLAG SPEDIZIONI/RICAVI S/N
246300941021     C     $ERR          IFEQ      'N'
246400940824     C                   Z-ADD     0             I
246500940824     C                   Z-ADD     CODUT         KTBUTE
246600940824     C                   MOVEL     '2W'          KTBCOD
246700940824     C                   MOVEL     *BLANKS       KTBKEY
246800940824     C     KEYTAB        SETLL     TABEL00F                           99
246900950203     C  N99              READ(N)   TABEL00F                               99
247000940824     C     *IN99         IFEQ      '1'
247100940824     C                   MOVEL     MSG(13)       $MSG
247200941026     C                   EXSR      SCRERR
247300940824     C                   END
247400940824     C     *IN99         DOWEQ     '0'
247500940824     C     TBLCOD        ANDEQ     '2W'
247600940824     C     TBLFLG        IFEQ      *BLANKS
247700940824     C                   ADD       1             I
247800940824     C                   MOVEL     TBLKEY        FRB(I)
247900940824     C                   MOVEL     TBLUNI        DS2W
248000940824     C                   MOVE      §2WFSP        FSP(I)
248100940824     C                   MOVE      §2WFRI        FRI(I)
248200940824     C                   END
248300950203     C                   READ(N)   TABEL00F                               99
248400940824     C                   END
248500940824     C                   END
248600020528     C*
248700020528     C* CARICA TABELLA DATI VARI CONTO ECONOMICO
248800020530     C                   Z-ADD     0             I
248900020528     C                   Z-ADD     CODUT         KTBUTE
249000020528     C                   MOVEL     '2V'          KTBCOD
249100020528     C                   MOVEL     *BLANKS       KTBKEY
249200020528     C     KEYTAB        SETLL     TABEL00F                           99
249300020528     C  N99              READ(N)   TABEL00F                               99
249400020528     C     *IN99         DOWEQ     '0'
249500020528     C     TBLCOD        ANDEQ     '2V'
249600020528     C     TBLFLG        IFEQ      *BLANKS
249700020528     C                   ADD       1             I
249800020528     C                   MOVEL     TBLUNI        DS2V
249900020528     C                   MOVEL     TBLKEY        FIL(I)
250000020528     C                   MOVEL     §2VSDA        SDA(I)
250100020528     C                   MOVEL     §2VSDC        SDC(I)
250200020528     C                   MOVEL     §2VSDF        SDF(I)
250300020528     C                   MOVEL     §2VSDO        SDO(I)
250400020528     C                   MOVEL     §2VSDP        SDP(I)
250500020528     C                   MOVEL     §2VSDR        SDR(I)
250600020528     C                   MOVEL     §2VSDT        SDT(I)
250700040316     C                   MOVEL     §2VSDE        SDE(I)
250800020528     C                   MOVEL     §2VCET        CET(I)
250900020528     C                   MOVEL     §2VMOV        MOV(I)
251000020531     C                   MOVEL     §2VFIM        FIM(I)
251100020531     C                   MOVEL     §2VFCE        FCE(I)
251200030108     C                   MOVEL     §2VCOP        COP(I)
251300020528     C                   ENDIF
251400020528     C  N99              READ(N)   TABEL00F                               99
251500020528     C                   ENDDO
251600960130     C*
251700990804     C* CARICO DATA ULTIMO TRASF. DA TITAS10 > TITASP PER ESCLUDERE LE BOLLE
251800960130     C* CON DATA INFERIORE
251900960130     C                   Z-ADD     1             KTBUTE
252000960130     C                   MOVEL     '$3'          KTBCOD
252100960130     C                   MOVEL     '1'           KTBKEY
252200960130     C     KEYTAB        CHAIN     TABEL00F                           99
252300960130IF  1C     *IN99         IFEQ      *ON
252400960130     C                   MOVEL     MSG(10)       $MSG
252500960130     C                   EXSR      SCRERR
252600960130X   1C                   ELSE
252700960130     C                   MOVEL     TBLUNI        DS$3
252800960130     C                   Z-ADD     §$3D10        WDT10P            8 0
252900960130E   1C                   ENDIF
253000020117     C*
253100011106     C* DIVISE PER CONVERSIONE
253200011106     C                   CLEAR                   DSGEDCE
253300011106     C                   MOVEL     *BLANKS       WDIVCE            3
253400011106     C                   MOVEL(P)  'GED'         KTECOD
253500011106     C                   MOVEL(P)  'CE'          KTEKE1
253600011106     C     KEYTBE        CHAIN     TNTBE01L
253700011106IF  1C                   IF        %found(TNTBE01L)
253800011106     C                   MOVEL     TBEUNI        DSGEDCE
253900011106E   1C                   ENDIF
254000011106IF  1C                   IF        §GEDCEDAT > DATCOR
254100011106     C                   EVAL      WDIVCE = §GEDCEDIP
254200011106X   1C                   ELSE
254300011106     C                   EVAL      WDIVCE = §GEDCEDIV
254400011106E   1C                   ENDIF
254500941025     C*
254600011106     C* CHIUDE I FILES
254700941025     C                   CLOSE     TABEL00F
254800011106     C                   CLOSE     TNTBE01L
254900930414     C*
255000930414     C                   ENDSR
255100940103     C*-------------------------------------------------------------------
255200940103     C*? CARFLG - CARICA FLAG PER AGGIORNAMENTO SALDI
255300940103     C*-------------------------------------------------------------------
255400940103     C     CARFLG        BEGSR
255500020620     c*
255600020620IF  1C     D$GPGF        IFNE      'TRECE9R'
255700020620     C     D$GPGF        ANDNE     'TREC78R'
255800020620     C     D$GPGF        ANDNE     'TREC11R'
255900020607     C                   MOVEL     'N'           D$GSDA
256000020607     C                   MOVEL     'N'           D$GSDC
256100020607     C                   MOVEL     'N'           D$GSDF
256200020607     C                   MOVEL     'N'           D$GSDO
256300020607     C                   MOVEL     'N'           D$GSDP
256400020607     C                   MOVEL     'N'           D$GSDR
256500020607     C                   MOVEL     'N'           D$GSDT
256600040316     C                   MOVEL     'N'           D$GSDE
256700020607     C                   MOVEL     'N'           D$GCET
256800020607     C                   MOVEL     'N'           D$GWFM
256900020607     C                   MOVEL     'N'           D$GFIM
257000020607     C                   MOVEL     'N'           D$GFCE
257100030108     C                   MOVEL     'N'           D$GCOP
257200020528     C                   Z-ADD     1             I
257300020528     C     D$GFIL        LOOKUP    FIL(I)                                 99
257400020617if  2C     *IN99         IFEQ      '1'
257500020528     C                   MOVEL     SDA(I)        D$GSDA
257600020528     C                   MOVEL     SDC(I)        D$GSDC
257700020528     C                   MOVEL     SDF(I)        D$GSDF
257800020528     C                   MOVEL     SDO(I)        D$GSDO
257900020528     C                   MOVEL     SDP(I)        D$GSDP
258000020528     C                   MOVEL     SDR(I)        D$GSDR
258100020528     C                   MOVEL     SDT(I)        D$GSDT
258200040316     C                   MOVEL     SDE(I)        D$GSDE
258300020528     C                   MOVEL     CET(I)        D$GCET
258400020528     C                   MOVEL     MOV(I)        D$GWFM
258500020531     C                   MOVEL     FIM(I)        D$GFIM
258600020531     C                   MOVEL     FCE(I)        D$GFCE
258700030108     C                   MOVEL     COP(I)        D$GCOP
258800020617e   2c                   ENDIF
258900020617e   1C                   ENDIF
259000940103     C*
259100940103     C                   ENDSR
259200991116     C*-------------------------------------------------------------------
259300991116     C*? GESASI - GESTIONE ANNO PER SIMULAZIONE
259400991116     C*-------------------------------------------------------------------
259500991116     C     GESASI        BEGSR
259501150313     c* simulazione con pesi e volumi diminuiti del xxx%
259525150313     c                   if        d$dpkb <> 0
259526150313     c                   eval(h)   compes = d$dpkb * 5  / 100
259527150313     c                   if        compes <> 0
259528150313     c                   eval      d$dpkb =
259529150313     c                             d$dpkb -compes
259530150313     c                   end
259531150313     c                   end
259532150313     c                   if        d$dpkc <> 0
259533150313     c                   eval(h)   compes = d$dpkc * 5  / 100
259534150313     c                   if        compes <> 0
259535150313     c                   eval      d$dpkc =
259536150313     c                             d$dpkc -compes
259537150313     c                   end
259538150313     c                   end
259539150313     c                   if        d$dpkf <> 0
259540150313     c                   eval(h)   compes = d$dpkf * 5  / 100
259541150313     c                   if        compes <> 0
259542150313     c                   eval      d$dpkf =
259543150313     c                             d$dpkf -compes
259544150313     c                   end
259545150313     c                   end
259546150313     c                   if        d$dpvl <> 0
259547150313     c                   eval(h)   compvl = d$dpvl * 5  / 100
259548150313     c                   if        compvl <> 0
259549150313     c                   eval      d$dpvl =
259550150313     c                             d$dpvl -compvl
259551150313     c                   end
259552150313     c                   end
259553150313     c                   if        d$dvlb <> 0
259554150313     c                   eval(h)   comvol = d$dvlb * 5 / 100
259555150313     c                   if        comvol <> 0
259556150313     c                   eval      d$dvlb =
259557150313     c                             d$dvlb -comvol
259558150313     c                   end
259559150313     c                   end
259560150313     c                   if        d$dvlc <> 0
259561150313     c                   eval(h)   comvol = d$dvlc * 5  / 100
259562150313     c                   if        comvol <> 0
259563150313     c                   eval      d$dvlc =
259564150313     c                             d$dvlc -comvol
259565150313     c                   end
259566150313     c                   end
259567150313     c                   if        d$dvlf <> 0
259568150313     c                   eval(h)   comvol = d$dvlf * 5 / 100
259569150313     c                   if        comvol <> 0
259570150313     c                   eval      d$dvlf =
259571150313     c                             d$dvlf -comvol
259572150313     c                   end
259573150313     c                   end
259600991116     C*
259700991116     C* IMPOSTA L'ANNO DELLA SIMULAZIONE NELLE DATE UTILIZZATE NEL C.E.
259800030129     C*---
259900030129     C* DS BOLLA
260000030129     C*---
260100991116     C* ANNO DELLA BOLLA
260200991116     C                   Z-ADD     D$GASI        D$DAAS
260300991116     C* DATA DI CONSEGNA
260400030129IF  1C     D$DDCM        IFGT      *ZEROS
260500991116     C                   Z-ADD     D$DDCM        DSDAT8
260600991116     C                   Z-ADD     D$GASI        DSAAS
260700991116     C                   Z-ADD     DSDAT8        D$DDCM
260800030129E   1C                   ENDIF
260900991116     C* DATA FATTURA
261000030129IF  1C     D$DDFT        IFGT      *ZEROS
261100991116     C                   Z-ADD     D$DDFT        DSDAT8
261200991116     C                   Z-ADD     D$GASI        DSAAS
261300991116     C                   Z-ADD     DSDAT8        D$DDFT
261400030129E   1C                   ENDIF
261500030129     C* DATA ARRIVO ULTIMO COLLO
261600030129IF  1C     D$DDTI        IFGT      *ZEROS
261700030129     C                   Z-ADD     D$DDTI        DSDAT8
261800030129     C                   Z-ADD     D$GASI        DSAAS
261900030129     C                   Z-ADD     DSDAT8        D$DDTI
262000030129E   1C                   ENDIF
262100030129     C* DATA CONSEGNA MERCE
262200030129IF  1C     D$DDCM        IFGT      *ZEROS
262300030129     C                   Z-ADD     D$DDCM        DSDAT8
262400030129     C                   Z-ADD     D$GASI        DSAAS
262500030129     C                   Z-ADD     DSDAT8        D$DDCM
262600030129E   1C                   ENDIF
262700030129     C* DATA DISTINTA CONSEGNA
262800030129IF  1C     D$DDDC        IFGT      *ZEROS
262900030129     C                   Z-ADD     D$DDDC        DSDAT8
263000030129     C                   Z-ADD     D$GASI        DSAAS
263100030129     C                   Z-ADD     DSDAT8        D$DDDC
263200030129E   1C                   ENDIF
263300030129     C* ANNO APERTURA GIACENZA
263400030129IF  1C     D$DAGC        IFGT      *ZEROS
263500030129     C                   Z-ADD     D$GASI        D$DAGC
263600030129E   1C                   ENDIF
263700030129     C* DATA PARTENZA ULTIMO COLLO
263800030129IF  1C     D$DDUC        IFGT      *ZEROS
263900030129     C                   Z-ADD     D$DDUC        DSDAT8
264000030129     C                   Z-ADD     D$GASI        DSAAS
264100030129     C                   Z-ADD     DSDAT8        D$DDUC
264200030129E   1C                   ENDIF
264300030129     C* DATA RITIRO
264400030129IF  1C     D$DDRT        IFGT      *ZEROS
264500030129     C                   Z-ADD     D$DDRT        DSDAT8
264600030129     C                   Z-ADD     D$GASI        DSAAS
264700030129     C                   Z-ADD     DSDAT8        D$DDRT
264800030129E   1C                   ENDIF
264900030129     C*
265000991116     C* REIMPOSTA CAMPI DELLA DS DS$ECO
265100991116     C                   MOVEL     D$DDS1        D$DRE1
265200991116     C                   MOVEL     D$DDS2        D$DRE2
265300030129     C*---
265400030129     C* DS GENERICA
265500030129     C*---
265600030129     C* DATA ENTRATA TRANSITO 1
265700030129IF  1C     D$DAR5DET     IFGT      *ZEROS
265800030129     C                   Z-ADD     D$DAR5DET     DSDAT8
265900030129     C                   Z-ADD     D$GASI        DSAAS
266000030129     C                   Z-ADD     DSDAT8        D$DAR5DET
266100030129E   1C                   ENDIF
266200030129     C* DATA USCITA  TRANSITO 1
266300030129IF  1C     D$DAR5DUT     IFGT      *ZEROS
266400030129     C                   Z-ADD     D$DAR5DUT     DSDAT8
266500030129     C                   Z-ADD     D$GASI        DSAAS
266600030129     C                   Z-ADD     DSDAT8        D$DAR5DUT
266700030129E   1C                   ENDIF
266800030129     C* DATA ENTRATA TRANSITO 2
266900030129IF  1C     D$DAR5DE2     IFGT      *ZEROS
267000030129     C                   Z-ADD     D$DAR5DE2     DSDAT8
267100030129     C                   Z-ADD     D$GASI        DSAAS
267200030129     C                   Z-ADD     DSDAT8        D$DAR5DE2
267300030129E   1C                   ENDIF
267400030129     C* DATA USCITA  TRANSITO 2
267500030129IF  1C     D$DAR5DU2     IFGT      *ZEROS
267600030129     C                   Z-ADD     D$DAR5DU2     DSDAT8
267700030129     C                   Z-ADD     D$GASI        DSAAS
267800030129     C                   Z-ADD     DSDAT8        D$DAR5DU2
267900030129E   1C                   ENDIF
268000040517     C* DATA ENTRATA TRANSITO 3
268100040517IF  1C     D$DAR5DE3     IFGT      *ZEROS
268200040517     C                   Z-ADD     D$DAR5DE3     DSDAT8
268300040517     C                   Z-ADD     D$GASI        DSAAS
268400040517     C                   Z-ADD     DSDAT8        D$DAR5DE3
268500040517E   1C                   ENDIF
268600040517     C* DATA USCITA  TRANSITO 3
268700040517IF  1C     D$DAR5DU3     IFGT      *ZEROS
268800040517     C                   Z-ADD     D$DAR5DU3     DSDAT8
268900040517     C                   Z-ADD     D$GASI        DSAAS
269000040517     C                   Z-ADD     DSDAT8        D$DAR5DU3
269100040517E   1C                   ENDIF
269200040517     C* DATA ENTRATA TRANSITO 4
269300040517IF  1C     D$DAR5DE4     IFGT      *ZEROS
269400040517     C                   Z-ADD     D$DAR5DE4     DSDAT8
269500040517     C                   Z-ADD     D$GASI        DSAAS
269600040517     C                   Z-ADD     DSDAT8        D$DAR5DE4
269700040517E   1C                   ENDIF
269800040517     C* DATA USCITA  TRANSITO 4
269900040517IF  1C     D$DAR5DU4     IFGT      *ZEROS
270000040517     C                   Z-ADD     D$DAR5DU4     DSDAT8
270100040517     C                   Z-ADD     D$GASI        DSAAS
270200040517     C                   Z-ADD     DSDAT8        D$DAR5DU4
270300040517E   1C                   ENDIF
270400040517     C* DATA ENTRATA TRANSITO 5
270500040517IF  1C     D$DAR5DE5     IFGT      *ZEROS
270600040517     C                   Z-ADD     D$DAR5DE5     DSDAT8
270700040517     C                   Z-ADD     D$GASI        DSAAS
270800040517     C                   Z-ADD     DSDAT8        D$DAR5DE5
270900040517E   1C                   ENDIF
271000040517     C* DATA USCITA  TRANSITO 5
271100040517IF  1C     D$DAR5DU5     IFGT      *ZEROS
271200040517     C                   Z-ADD     D$DAR5DU5     DSDAT8
271300040517     C                   Z-ADD     D$GASI        DSAAS
271400040517     C                   Z-ADD     DSDAT8        D$DAR5DU5
271500040617E   1C                   ENDIF
271600991116     C*
271700991116     C                   ENDSR
271800020621     C*-------------------------------------------------------------------
271900020621     C*?Azzera varia "N" immagine -prima- TITAS
272000020621     C*-------------------------------------------------------------------
272100020621     C     AZZnCPDTAQ    BEGSR
272200020621     c*
272300031114if  1C                   IF        (D$PVA1 = 88888888,000)
272400020621     C                   Z-ADD     *zeros        D$PVA1
272500020621e   1c                   ENDIF
272600031114if  1C                   IF        (D$PVA2 = 88888888,000)
272700020621     C                   Z-ADD     *zeros        D$PVA2
272800020621e   1c                   ENDIF
272900031114if  1C                   IF        (D$PVA3 = 88888888,000)
273000020621     C                   Z-ADD     *zeros        D$PVA3
273100020621e   1c                   ENDIF
273200020621     c*
273300020621     C                   ENDSR
273400020621     C*-------------------------------------------------------------------
273500020621     C*?Azzera varia "N" immagine -dopo- TITAS
273600020621     C*-------------------------------------------------------------------
273700020621     C     AZZnCDDTAQ    BEGSR
273800020621     c*
273900031114if  1C                   IF        (D$DVA1 = 88888888,000)
274000020621     C                   Z-ADD     *zeros        D$DVA1
274100020621e   1c                   ENDIF
274200031114if  1C                   IF        (D$DVA2 = 88888888,000)
274300020621     C                   Z-ADD     *zeros        D$DVA2
274400020621e   1c                   ENDIF
274500031114if  1C                   IF        (D$DVA3 = 88888888,000)
274600020621     C                   Z-ADD     *zeros        D$DVA3
274700020621e   1c                   ENDIF
274800020621     c*
274900020621     C                   ENDSR
275000020621     C*-------------------------------------------------------------------
275100020621     C*?Azzera varia "N" immagine TITAS originale
275200020621     C*-------------------------------------------------------------------
275300020621     C     AZZnCODTAQ    BEGSR
275400020621     c*
275500031114if  1C                   IF        (D$OVA1 = 88888888,000)
275600020621     C                   Z-ADD     *zeros        D$OVA1
275700020621e   1c                   ENDIF
275800031114if  1C                   IF        (D$OVA2 = 88888888,000)
275900020621     C                   Z-ADD     *zeros        D$OVA2
276000020621e   1c                   ENDIF
276100031114if  1C                   IF        (D$OVA3 = 88888888,000)
276200020621     C                   Z-ADD     *zeros        D$OVA3
276300020621e   1c                   ENDIF
276400020621     c*
276500020621     C                   ENDSR
276600020621     C*-------------------------------------------------------------------
276700020621     C*?Azzera varia "N" di TITA7
276800020621     C*-------------------------------------------------------------------
276900020621     C     AZZnTA7       BEGSR
277000020621     c*
277100031114if  1C                   IF        (TA7VAN = 88888888,000)
277200020621     C                   Z-ADD     *zeros        TA7VAN
277300020621e   1c                   ENDIF
277400020621     c*
277500020621     C                   ENDSR
277600930305     C********************************************************************
277700941130      /COPY GAITRASRC/SRCTREC,TRECDT
277800940808     C********************************************************************
277900930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
278000941130TRECD6R- Tabella 'TB' non trovata. Avvertire CED.                         TBTB 1
278100030109TRECD6R- Arrivata variazione di TNCSB e non è stato trovato TITAS         NTTA 2
278200030113TRECD6R- 3
278300941130TRECD6R- Tipo bolla non trovato in tab. 'TB'. Avvertire CED.              TBNT 4
278400950118TRECD6R- Trovato gia un altro errore per questa bolla.                    TEQB 5
278500030113TRECD6R- 6
278600030113TRECD6R- Arrivata variazione di FIARB -TRS- e non è stato trovato TITAS   NTFT 7
278700030113TRECD6R- Arrivata variazione di FIARB -BAN- e non è stato trovato TITAS   NTFB 8
278800011106TRECD6R- 9                                                                     9
278900950517TRECD6R- Tabella '$3' non trovata. Avvertire CED.                         TB$310
279000990804TRECD6R- Bolla mamma trovata nei legami ma non in TITAS10F.               BMNT11
279100941130TRECD6R- Flag raggruppamento bolle non trovato in tab. '2W'.              FRNT12
279200941130TRECD6R- Tabella '2W' non trovata. Avvertire CED.                         TB2W13
279300941130TRECD6R- 14                                                                   14
279400941130TRECD6R- 15                                                                   15
279500941130TRECD6R- 16                                                                   16
279600941130TRECD6R- 17                                                                   17
279700941130TRECD6R- 18                                                                   18
279800941130TRECD6R- 19                                                                   19
279900941130TRECD6R- 20                                                                   20
