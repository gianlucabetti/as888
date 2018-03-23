000100970117     H*------------------------------------------------------------------------*
000200970117     H* TARIFFE DA CONTO ECONOMICO - RICHIESTA SCAGLIONI                       *
000300970117     H*------------------------------------------------------------------------*
000400060829     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000500970117     F*------------------------------------------------------------------------*
000600970117     F* DATA BASE
000700970117     F*------------------------------------------------------------------------*
000800970117     FTNTE02D   CF   E             WORKSTN
000900970117     F                                     SFILE(TE02S01:NRR)
001000970117     FTABEL00F  IF   E           K DISK
001100970117     FAZORG01L  IF   E           K DISK
001200970117     FCNACO00F  IF   E           K DISK
001300041202     FTFACO00F  IF   E           K DISK    usropn
001400000211     F                                     RENAME(CNACO000:TFACO000)
001500970117     FTNTAM01L  IF   E           K DISK
001600041202     FTNOFM01L  IF   E           K DISK    usropn
001700970117     F                                     RENAME(TNTAM000:TNOFM000)
001800990818     FTITAD04L  IF   E           K DISK
001900011001     FTEETC01L  IF   E           K DISK
002000011001     FTEETS01L  UF A E           K DISK
002100970117     D*------------------------------------------------------------------------*
002200970117     D* SCHIERE
002300970117     D*------------------------------------------------------------------------*
002400970619     D*-------------------
002500970117     D* DECODIFICHE VARIE
002600970117     D*-------------------
002700980505     D DDEC            S             20    DIM(10) CTDATA PERRCD(1)
002800970117     D*-------------------
002900970117     D* DESCRIZIONE VIDEO
003000970117     D*-------------------
003100970117     D DVID            S             78    DIM(5) CTDATA PERRCD(1)
003200970117     D*-------------------
003300970117     D* DESCRIZIONE OPERAZIONE
003400970117     D*-------------------
003500970117     D DOPE            S             15    DIM(5) CTDATA PERRCD(1)
003600970117     D*-------------------
003700970117     D* MESSAGGI VIDEO
003800970117     D*-------------------
003900060825     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
004000970117     D*-------------------
004100970117     D* TIPO SERVIZIO
004200970117     D*-------------------
004300970117     D TSP             S              1    DIM(10)                              *CODICE
004400970117     D DTSP            S             20    DIM(10)                              *DESCRIZIONE
004500970117     D*-------------------
004600970117     D* TIPI TARIFFA
004700970117     D*-------------------
004800970117     D TTR             S              1    DIM(10)                              *CODICE
004900970117     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
005000970117     D*-------------------
005100970117     D* DI LAVORO
005200970117     D*-------------------
005300970117     D TSGL            S             13  3 DIM(10)                              *SCAGLIONI IN TARIFF
005400980505     D*-------------------
005500980505     D* MEMORIZZAZIONE DEL VIDEO
005600980505     D*-------------------
005700970117     D VSGL            S             13  3 DIM(10)                              *SCAGLIONI A VIDEO
005800980505     D VPSC            S              7  1 DIM(10)                              *PESO MEDIO
005900980505     D VCSC            S              7  1 DIM(10)                              *COLLI MEDI
006000980505     D VVSC            S              7  3 DIM(10)                              *VOLUME MEDIO
006100011001     D VINL            S             11  3 DIM(10)                              *INOLTRO
006200980505     D VRPV            S              3  1 DIM(10)                              *RAPPORTO PESO/VOLUM
006300980505     D VSDE            S              1    DIM(10)                              *SEGNO DELTA
006400980505     D VPDE            S              4  1 DIM(10)                              *% DELTA
006500980505     D VPPE            S              4  1 DIM(10)                              *% PESO
006600980505     D VAST            S              1    DIM(10)                              *ASTERISCO
006700970117     D*-------------------
006800970117     D* CARICAMENTO ALTRA TARIFFA
006900970117     D*-------------------
007000970117     D CSGL            S             13  3 DIM(10)                              *SCAGLIONE
007100970117     D CPSC            S              7  1 DIM(10)                              *PESO MEDIO
007200970414     D CCSC            S              7  1 DIM(10)                              *COLLI MEDI
007300970117     D CVSC            S              7  3 DIM(10)                              *VOLUME MEDIO
007400011001     D CINL            S             11  3 DIM(10)                              *INOLTRO
007500970317     D CRPV            S              3  1 DIM(10)                              *RAPPORTO PESO/VOLUM
007600970402     D CSDE            S              1    DIM(10)                              *SEGNO DELTA
007700970402     D CPDE            S              4  1 DIM(10)                              *% DELTA
007800970117     D CPPE            S              4  1 DIM(10)                              *% PESO
007900970117     D*------------------
008000970117     D* TABELLE CAPICONTO
008100970117     D*------------------
008200970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
008300970117     D*------------------------------------------------------------------------*
008400970117     D* INPUT
008500970117     D*------------------------------------------------------------------------*
008600091203     D Tnte04DS      E DS                                                       *Profili utente
008700970117     D*-------------------
008800970117     D* DS TIPI SERVIZIO
008900970117     D*-------------------
009000970117     D DS5E          E DS
009100970117     D*-------------------
009200970117     D* DS TIPI TARIFFA
009300970117     D*-------------------
009400970117     D DSTR          E DS
009500021202     D*-------------------
009600021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
009700021202     D*-------------------
009800160229     D TRULc7DS      E DS
009900970117     D*-------------------
010000970117     D* DS PASSAGGIO PARAMETRI
010100970117     D*-------------------
010200970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
010300980504     D*------------
010400980504     D* DS RICERCA DATI TIPO
010500980504     D*------------
010600980504     D DSTE30        E DS                  EXTNAME(TNTE30DS)
010700970117     D*------------------
010800970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
010900970117     D*------------------
011000970117     D WLBDA8          DS
011100970117     D  G08DAT                 1      8  0
011200970117     D  G08INV                 9     16  0
011300970117     D  G08ERR                17     17
011400970117     D  G08TGI                18     22  0
011500970117     D*------------------
011600020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
011700970117     D*------------------
011800970117     D CARCNT          DS
011900970117     D  FL12CN                 1      2
012000970117     D  FL34CN                 3      4
012100970117     D  TIPCNT                 5      6
012200020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
012300970117     D  FLU                   66     97
012400970117     D                                     DIM(32)                              FLAG PAR.UTENT
012500970117     D  TCU                  398    697
012600970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
012700970117     D  KCU                  698    847P 0
012800970117     D                                     DIM(50)                              *TAB CAPICONTO
012900970117     D                                     PACKEVEN
013000970117     D TCUDS           DS
013100970117     D  E1                     1      1
013200970117     D  E3                     3      3
013300970117     D  E2                     2      2
013400970117     D  E4                     4      4
013500970117     D  E56                    5      6
013600970117     D*------------------
013700970117     D* DS SCOMPOSIZIONE DECODIFICA VIDEO
013800970117     D*------------------
013900970117     D* DVID,2
014000970117     D                 DS
014100970414     D  D02SGL                 7     11
014200970414     D  D02PSC                20     23
014300970117     D  DVID02                 1     78
014400970117     D*-------------------
014500970117     D* ARCHITETTURA
014600970117     D*-------------------
014700970117     D KPJBA         E DS
014800060829     D param           DS
014900060829     D  compric                       1
015000011026     D*------------------
015100011026     D* DS REPERIMENTO DIVISA TARIFFE
015200011026     D*------------------
015300011026     D DSTA01        E DS
015400011008     D*------------------
015500011008     D* VARIABILI DI WRK
015600011008     D*------------------
015700011008     D  NUMDEC         S              9  9 INZ(*zeros)
015800970117     C*------------------------------------------------------------------------*
015900970117     C* MAIN LINE
016000970117     C*------------------------------------------------------------------------*
016100970117     C*
016200970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
016300970117IF  1C     $FINE         IFNE      'S'
016400970117     C*
016500970117     C* IMPOSTAZIONI PRIMA VIDEATA
016600970117     C                   EXSR      IMPSFL
016700970117     C*
016800970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
016900970117DO  2C     $FINE         DOUEQ     'S'
017000970117     C     TIPVID        CASEQ     '1'           GESSFL                         *1° VIDEO
017100970117     C                   ENDCS
017200970117E   2C                   ENDDO
017300970117E   1C                   ENDIF
017400970117     C*
017500970117     C                   SETON                                        LR
017600970117     C*------------------------------------------------------------------------*
017700970117     C* IMPSFL - IMPOSTAZIONI INIZIALI SUBFILE
017800970117     C*------------------------------------------------------------------------*
017900970117     C     IMPSFL        BEGSR
018000970117     C*
018100970117     C* SE SCAGLIONI INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
018200091120     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE O FILIALE
018300980505     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
018400970117     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
018500970117     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
018600970117     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
018700970117     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
018800011001     C     KE2ETS        SETLL     TEETS01L                               99
018900970619IF  1C     *IN99         IFEQ      *OFF
019000970117     C                   WRITE     TE02D00                                      *TESTATA
019100970117     C                   WRITE     TE02D01                                      *MESSAGGIO SF VUOTO
019200970117     C                   EXSR      F05GES
019300970117E   1C                   ENDIF
019400970117     C*
019500970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
019600970117     C                   EXSR      SETSFL
019700970117     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
019800970117     C* CARICA IL SUBFILE
019900970117     C                   EXSR      POSSFL
020000970117     C*
020100970117     C                   ENDSR
020200970117     C*------------------------------------------------------------------------*
020300970117     C* SETSFL - IMPOSTAZIONI INDICATORI SUBFILE
020400970117     C*------------------------------------------------------------------------*
020500970117     C     SETSFL        BEGSR
020600970117     C*
020700970117     C                   SETOFF                                       19         ERRORI CONTROL
020800970117     C                   SETOFF                                       202122     ERRORI SFILE
020900970317     C                   SETOFF                                       232425
021000970402     C                   SETOFF                                       262728
021100970402     C                   SETOFF                                       29
021200970117     C                   SETOFF                                       90         GENERICO DI ERRORE
021300970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
021400970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
021500970506     C                   SETON                                        02        *ND ALCUNI CAMPI
021600970506X   1C                   ELSE                                                   *CALCOLO DELTA
021700970506     C                   SETOFF                                       02        *NESSUNA ND
021800970506E   1C                   ENDIF
021900970117     C*
022000970117     C                   ENDSR
022100970117     C*------------------------------------------------------------------------*
022200970117     C* GESSFL - GESTIONE SUBFILE
022300970117     C*------------------------------------------------------------------------*
022400970117     C     GESSFL        BEGSR
022500970117     C*
022600970117     C* EMISSIONE VIDEATA
022700970117IF  1C     $TESVD        IFEQ      '1'
022800970117     C                   WRITE     TE02D00                                      *TESTATA
022900970117     C                   MOVEL     '0'           $TESVD
023000970117E   1C                   ENDIF
023100970117     C                   WRITE     TE02Z01                                      *PIEDI
023200970117     C                   EXFMT     TE02C01                                      *SFILE
023300970117     C                   EXSR      SETSFL
023400970117     C* F03-FINE
023500970117IF  1C     *INKC         IFEQ      *ON
023600970117     C                   EXSR      F03GES
023700970117     C                   GOTO      FINSFL
023800970117E   1C                   ENDIF
023900970117     C* F12-RITORNO
024000970117IF  1C     *INKL         IFEQ      *ON
024100970117     C                   EXSR      F12GES
024200970117     C                   GOTO      FINSFL
024300970117E   1C                   ENDIF
024400970117     C* F05-CARICA DA ALTRA TARIFFA
024500970117IF  1C     *INKE         IFEQ      *ON
024600970117     C                   EXSR      F05GES
024700970117     C                   GOTO      FINSFL
024800970117E   1C                   ENDIF
024900060829     C* F07-scaglioni competenze/ricavi
025000060829IF  1C     *INKG         IFEQ      *ON
025100060829IF  1C     *IN44         andeq     *ON
025200060829     C                   EXSR      F07GES
025300060829     C                   GOTO      FINSFL
025400060829E   1C                   ENDIF
025500980507     C* CONTROLLA VIDEO
025600970117     C                   EXSR      CTRSFL
025700970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
025800970117     C                   GOTO      FINSFL
025900970117E   1C                   ENDIF
026000980507     C*
026100980507     C                   EXSR      CARSCV                                       *MEMORIZZA SCHIERE
026200980507     C                   EXSR      POSSFL                                       *RICARICA SFILE
026300980507     C*
026400970117     C* F06-AGGIORNA E PROSEGUE
026500970117IF  1C     *INKF         IFEQ      *ON
026600970117     C                   EXSR      F06GES
026700970117E   1C                   ENDIF
026800970117     C*
026900970117     C     FINSFL        ENDSR
027000970117     C*------------------------------------------------------------------------*
027100980505     C* CTRSFL - CONTROLLA SUBFILE 1^ PARTE
027200970117     C*------------------------------------------------------------------------*
027300970117     C     CTRSFL        BEGSR
027400970117     C*
027500980505     C* REIMPOSTA LE VARIABILI DI LAVORO
027600970117     C                   MOVEL     '0'           WERR              1            *ERRORE
027700980505     C                   MOVEL     '0'           WESGL             1            *ESISTENZA SCAGLIONI
027800980505     C                   Z-ADD     *ZEROS        V1CPPE                         *TOTALE %INCIDENZA
027900970117     C*
028000970117     C* LEGGE TUTTI I RECORD DEL SFILE
028100970117     C                   Z-ADD     1             NRR
028200970117     C     NRR           CHAIN     TE02S01                            98
028300970117DO  1C     *IN98         DOWEQ     *OFF
028400980505     C*
028500980505     C* CAMPI DI DUPLICA -> CAMPI SFILE
028600970117     C   13              Z-ADD     DUPPSC        V1SPSC                         *DI DUPLICA
028700970117     C   11              Z-ADD     DUPCSC        V1SCSC
028800970117     C   12              Z-ADD     DUPVSC        V1SVSC
028900970317     C   15              Z-ADD     DUPINL        V1SINL
029000970317     C   16              Z-ADD     DUPRPV        V1SRPV
029100970402     C   17              MOVEL     DUPSDE        V1SSDE
029200970402     C   18              Z-ADD     DUPPDE        V1SPDE
029300970317     C   14              Z-ADD     DUPPPE        V1SPPE
029400970117     C                   SETOFF                                       80        *NO SFLNXTCHG
029500980505     C*
029600980505     C* CONTROLLA CAMPI
029700970117IF  2C     V1SSGL        IFGT      *ZEROS                                       *IMMESSO QC.
029800970117     C     V1SPSC        ORGT      *ZEROS
029900970117     C     V1SCSC        ORGT      *ZEROS
030000970117     C     V1SVSC        ORGT      *ZEROS
030100970317     C     V1SINL        ORGT      *ZEROS
030200970317     C     V1SRPV        ORGT      *ZEROS
030300970414     C     V1SSDE        ORNE      *BLANKS
030400970402     C     V1SPDE        ORGT      *ZEROS
030500970117     C     V1SPPE        ORGT      *ZEROS
030600970117     C*--------------------
030700970117     C* ANNULLAMENTO
030800970117     C*--------------------
030900970117IF  3C     WERR          IFEQ      '0'
031000970117IF  4C     V1SATB        IFNE      'A'
031100970117     C     V1SATB        ANDNE     *BLANKS
031200970117     C                   SETON                                        2090
031300970117     C                   MOVEL     MSG(9)        $MSG
031400970117     C                   MOVEL     '1'           WERR
031500970117E   4C                   ENDIF
031600970117E   3C                   ENDIF
031700980505     C*
031800980505     C* SE IL RECORD NON E' DA ANNULLARE
031900980505     C                   MOVEL     '1'           WESGL                          *ESISTENZA SCAGLIONI
032000091119IF  3C     V1SATB        IFEQ      *BLANKS
032100970117     C*--------------------
032200970117     C* SCAGLIONE
032300970117     C*--------------------
032400980505IF  4C     WERR          IFEQ      '0'
032500980505IF  5C     V1SSGL        IFEQ      *ZEROS
032600970117     C                   SETON                                        2190
032700970117     C                   MOVEL     MSG(1)        $MSG
032800970117     C                   MOVEL     '1'           WERR
032900980505E   5C                   ENDIF
033000980505E   4C                   ENDIF
033100970117     C*--------------------
033200970117     C* PESO MEDIO
033300970117     C*--------------------
033400980505IF  4C     WERR          IFEQ      '0'
033500980505IF  5C     V1SPSC        IFEQ      *ZEROS
033600970117     C                   SETON                                        2290
033700970117     C                   MOVEL     MSG(2)        $MSG
033800970117     C                   MOVEL     '1'           WERR
033900980505E   5C                   ENDIF
034000980505E   4C                   ENDIF
034100980505     C* PESO MASSIMO
034200980505IF  4C     WERR          IFEQ      '0'
034300000621IF  5C     V1SPSC        IFGT      9999
034400971008     C                   SETON                                        2290
034500971008     C                   MOVEL     MSG(20)       $MSG
034600971008     C                   MOVEL     '1'           WERR
034700980505E   5C                   ENDIF
034800980505E   4C                   ENDIF
034900970117     C*--------------------
035000970117     C* COLLI MEDI
035100970117     C*--------------------
035200981030     C     *LIKE         DEFINE    ETSCSC        DEPCSC
035300980505IF  4C     WERR          IFEQ      '0'
035400980505IF  5C     V1SCSC        IFEQ      *ZEROS
035500970117     C     V1SPSC        DIV       V1CPMC        V1SCSC
035600981030     C                   Z-ADD     V1SCSC        DEPCSC
035700980928IF  6C***********V1SCSC    IFEQ *ZEROS
035800980928IF  6C     V1SCSC        IFLT      1
035900970117     C                   Z-ADD     1             V1SCSC
036000980505E   6C                   ENDIF
036100991105X   5C                   ELSE
036200991105     C                   Z-ADD     V1SCSC        DEPCSC
036300980505E   5C                   ENDIF
036400980928IF  5C***********V1SCSC    IFEQ *ZEROS
036500980928IF  5C     V1SCSC        IFLT      1
036600970117     C                   SETON                                        2390
036700970117     C                   MOVEL     MSG(3)        $MSG
036800970117     C                   MOVEL     '1'           WERR
036900980505E   5C                   ENDIF
037000980505E   4C                   ENDIF
037100970117     C*--------------------
037200970117     C* VOLUME MEDIO
037300970117     C*--------------------
037400980505IF  4C     WERR          IFEQ      '0'
037500980505IF  5C     V1SVSC        IFEQ      *ZEROS
037600981030     C*---       V1CVMC    MULT V1SCSC    V1SVSC
037700981030     C     V1CVMC        MULT      DEPCSC        V1SVSC
037800980505E   5C                   ENDIF
037900980505IF  5C     V1SVSC        IFEQ      *ZEROS
038000970117     C                   Z-ADD     0,001         V1SVSC
038100980505E   5C                   ENDIF
038200980505IF  5C     V1SVSC        IFEQ      *ZEROS
038300970117     C                   SETON                                        2490
038400970117     C                   MOVEL     MSG(4)        $MSG
038500970117     C                   MOVEL     '1'           WERR
038600980505E   5C                   ENDIF
038700980505E   4C                   ENDIF
038800970513     C*--------------------
038900970513     C* RPV PER SCAGLIONE
039000970513     C*--------------------
039100980505IF  4C     WERR          IFEQ      '0'
039200011009     C***                MOVEL     *BLANKS       V1SAST                         *NO INDICAZIONE "*"
039300011009     C                   SETOFF                                       70
039400970513     C                   EXSR      CHKAST
039500980505E   4C                   ENDIF
039600970317     C*--------------------
039700970317     C* INOLTRO
039800970317     C*--------------------
039900980505IF  4C     WERR          IFEQ      '0'
040000980505IF  5C     ETCPPV        IFEQ      *ZEROS
040100970317     C     V1SINL        ANDGT     *ZEROS
040200970317     C                   SETON                                        2590
040300970317     C                   MOVEL     MSG(15)       $MSG
040400970317     C                   MOVEL     '1'           WERR
040500980505E   5C                   ENDIF
040600980505E   4C                   ENDIF
040700011008     C*
040800011008     C     WERR          IFEQ      '0'
040900011008     C     V1SINL        IFGT      *ZEROS
041000011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
041100011008     C     D00DIV        IFEQ      'ITL'
041200011008     C                   Z-ADD     V1SINL        NUMDEC
041300011008     C     NUMDEC        IFGT      *ZEROS
041400011008     C                   SETON                                        2590
041500011008     C                   MOVEL     MSG(23)       $MSG
041600011008     C                   MOVEL     '1'           WERR
041700011008     C                   ENDIF
041800011008     C                   ENDIF
041900011008     C                   ENDIF
042000011008     C                   ENDIF
042100011008     C*
042200970402     C*--------------------
042300970402     C* SEGNO E % SCOSTAMENTO DELTA
042400970402     C*--------------------
042500980505IF  4C     WERR          IFEQ      '0'
042600980505IF  5C     V1SSDE        IFNE      *BLANKS                                      *SEGNO DELTA
042700970402     C     V1SSDE        ANDNE     '+'
042800970402     C     V1SSDE        ANDNE     '-'
042900970402     C                   SETON                                        2890
043000970402     C                   MOVEL     MSG(18)       $MSG
043100970402     C                   MOVEL     '1'           WERR
043200980505E   5C                   ENDIF
043300980505E   4C                   ENDIF
043400980505IF  4C     WERR          IFEQ      '0'
043500980505IF  5C     V1SSDE        IFNE      *BLANKS                                      *% DELTA
043600970402     C     V1SPDE        ANDEQ     *ZEROS
043700970402     C     V1SPDE        ORGT      *ZEROS
043800970402     C     V1SSDE        ANDEQ     *BLANKS
043900970402     C                   SETON                                        2990
044000970402     C                   MOVEL     MSG(19)       $MSG
044100970402     C                   MOVEL     '1'           WERR
044200980505E   5C                   ENDIF
044300980505E   4C                   ENDIF
044400970117     C*--------------------
044500970117     C* % INCIDENZA
044600970117     C*--------------------
044700980505IF  4C     WERR          IFEQ      '0'
044800980505IF  5C     V1SPPE        IFEQ      *ZEROS
044900970117     C                   SETON                                        2690
045000970117     C                   MOVEL     MSG(5)        $MSG
045100970117     C                   MOVEL     '1'           WERR
045200980505E   5C                   ENDIF
045300980505E   4C                   ENDIF
045400980505     C*--------------------
045500980505     C* RANGE SCAGLIONE
045600980505     C*--------------------
045700980505     C* VALORE MASSIMO -> MINORE DELLO SCAGLIONE
045800980505IF  4C     WERR          IFEQ      '0'
045900980507     C                   MOVEL     D00CTR        A1                1
046000980507IF  5C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
046100980505     C                   Z-ADD     V1SCSC        W133             13 3
046200980505X   5C                   ELSE                                                   *TARIFFA A QLI/KG
046300980505     C                   Z-ADD     V1SPSC        W133             13 3
046400980505E   5C                   ENDIF
046500980505IF  5C     W133          IFGT      V1SSGL
046600980507IF  6C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
046700980507     C                   SETON                                        23
046800980507X   6C                   ELSE                                                   *TARIFFA A QLI/KG
046900980507     C                   SETON                                        22
047000980507E   6C                   ENDIF
047100980507     C                   SETON                                        90
047200980505     C                   MOVEL     MSG(16)       $MSG
047300980505     C                   MOVEL     '1'           WERR
047400980505E   5C                   ENDIF
047500980505E   4C                   ENDIF
047600980505     C*
047700980505     C* CALCOLO %INCIDENZA TOTALE
047800980505IF  4C     V1SATB        IFEQ      *BLANKS                                      *NO ANNULLATI
047900980505     C     V1CPPE        ADD       V1SPPE        V1CPPE                         *TOTALE PESI %
048000980505E   4C                   ENDIF
048100980505     C*
048200980505     C* AGGIORNA RIGA SFILE
048300980505     C                   Z-ADD     NRR           $NRR                           *POSIZIONAMENTO
048400980505     C                   EXSR      AGGSFL                                       *AGGIORNA RIGA
048500980505IF  4C     WERR          IFEQ      *ON                                          * E R R O R E
048600980505     C                   GOTO      FINCS1                                       *ESCE
048700980505X   4C                   ELSE                                                   *N O N  ERRORE
048800980505     C                   Z-ADD     V1SPSC        DUPPSC                         *DI DUPLICA
048900980505     C                   Z-ADD     V1SCSC        DUPCSC
049000980505     C                   Z-ADD     V1SVSC        DUPVSC
049100980505     C                   Z-ADD     V1SINL        DUPINL
049200980505     C                   Z-ADD     V1SRPV        DUPRPV
049300980505     C                   MOVEL     V1SSDE        DUPSDE
049400980505     C                   Z-ADD     V1SPDE        DUPPDE
049500980505     C                   Z-ADD     V1SPPE        DUPPPE
049600980505E   4C                   ENDIF
049700980505E   3C                   ENDIF
049800980505E   2C                   ENDIF
049900980505     C*
050000980505     C* LETTURA SUCCESSIVA RIGA SFILE
050100980505     C                   ADD       1             NRR                            *LEGGE ALTRO RECORD
050200980505     C     NRR           CHAIN     TE02S01                            98
050300980505E   1C                   ENDDO                                                  *FINE SUBFILE
050400980505     C*---
050500980505     C* ALMENO UNO SCAGLIONE
050600980505     C*---
050700980505IF  1C     WERR          IFEQ      '0'
050800980505IF  2C     WESGL         IFEQ      '0'
050900980505     C                   SETON                                        90
051000980505     C                   MOVEL     MSG(7)        $MSG
051100980505     C                   MOVEL     '1'           WERR
051200980505     C                   Z-ADD     1             $NRR                           *POSIZIONAMENTO
051300980505E   2C                   ENDIF
051400980505E   1C                   ENDIF
051500970117     C*
051600980505     C     FINCS1        ENDSR
051700970513     C*------------------------------------------------------------------------*
051800970513     C* CHKAST - CONTROLLA L'ASTERISCO
051900970513     C*------------------------------------------------------------------------*
052000970513     C     CHKAST        BEGSR
052100011009     C*
052200011009     C********
052300011009     C* La routine è rimasta la stessa ma invece di gestire il campo V1SAST (l'asterisco)
052400011009     C* per questioni grafiche di spazio a video gestisce in sostituzione un indicatore
052500011009     C* legato al campo SCAGLIONE
052600011009     C********
052700970513     C*
052800980507     C                   MOVEL     D00CTR        A1                1
052900980507IF  1C     A1            IFEQ      '0'                                          *TARIFFA A QLI
053000980507     C     A1            OREQ      '3'                                          *TARIFFA A KG
053100970513IF  2C     V1SRPV        IFGT      *ZEROS                                       *IMMESSO RAPPORTO P/
053200970513     C                   Z-ADD     *ZEROS        PESTAS
053300970513     C                   EXSR      CALPT                                        *CALCOLA P.TASSABILE
053400970513IF  3C     PESTAS        IFGT      V1SSGL                                       *PESO > SCAGLIONE
053500011009     C***                MOVEL     '*'           V1SAST                         *SI INDICAZIONE "*"
053600011009     C                   SETON                                        70
053700970513E   3C                   ENDIF
053800970513E   2C                   ENDIF
053900970513E   1C                   ENDIF
054000970513     C*
054100970513     C                   ENDSR
054200970513     C*------------------------------------------------------------------------*
054300970513     C* CALPT - CALCOLA IL PESO TASSABILE
054400970513     C*------------------------------------------------------------------------*
054500970513     C     CALPT         BEGSR
054600970513     C*
054700970513     C* IMPOSTA COME PESO QUELLO DEI DATI TIPO SCAGLIONE
054800970513     C                   Z-ADD     V1SSGL        PESTAS           13 3          *KG O QLI
054900970513     C*
055000970513     C* CALCOLA IL PESO TASSABILE
055100970513     C                   Z-ADD     *ZEROS        COMPRV            9 3
055200970513     C     V1SVSC        MULT      V1SRPV        COMPRV
055300970513     C     COMPRV        MULT      100           PESTAS                         *CALCOLO PESO TASS.
055400970513IF  1C     PESTAS        IFLT      V1SPSC                                       *PESO TASS < REALE
055500970513     C                   Z-ADD     V1SPSC        PESTAS                         *PESO TASS=PESO REAL
055600970513E   1C                   ENDIF
055700970513     C*
055800970513     C                   ENDSR
055900980505     C*------------------------------------------------------------------------*
056000980505     C* CARSCV - MEMORIZZA IL SFILE NELLE SCHIERE VIDEO
056100980505     C*------------------------------------------------------------------------*
056200980505     C     CARSCV        BEGSR
056300980505     C*
056400980505     C* PULISCE LE SCHIERE VIDEO
056500980505     C                   EXSR      CLRSCV
056600980505     C*
056700980505     C* LEGGE TUTTI I RECORD DEL SFILE
056800980505     C                   Z-ADD     1             NRR
056900980505     C     NRR           CHAIN     TE02S01                            98
057000980505DO  1C     *IN98         DOWEQ     *OFF
057100980505IF  2C     V1SATB        IFEQ      *BLANKS                                      *NON ANNULLATI
057200980505IF  3C     V1SSGL        IFGT      *ZEROS                                       *SCAGLIONE INSERITO
057300980505     C                   ADD       1             V
057400980505     C                   Z-ADD     V1SSGL        VSGL(V)
057500980505     C                   Z-ADD     V1SPSC        VPSC(V)                        *PESO MEDIO
057600980505     C                   Z-ADD     V1SCSC        VCSC(V)                        *COLLI MEDI
057700980505     C                   Z-ADD     V1SVSC        VVSC(V)                        *VOLUME MEDIO
057800980505     C                   Z-ADD     V1SINL        VINL(V)                        *INOLTRO
057900980505     C                   Z-ADD     V1SRPV        VRPV(V)                        *RAPPORTO PESO/VOLUM
058000980505     C                   MOVEL     V1SSDE        VSDE(V)                        *SEGNO DELTA
058100980505     C                   Z-ADD     V1SPDE        VPDE(V)                        *% DELTA
058200980505     C                   Z-ADD     V1SPPE        VPPE(V)                        *% PESO
058300011009     C***                MOVEL     V1SAST        VAST(V)                        *ASTERISCO
058400980505E   3C                   ENDIF
058500980505E   2C                   ENDIF
058600980505     C                   ADD       1             NRR                            *LEGGE ALTRO RECORD
058700980505     C     NRR           CHAIN     TE02S01                            98
058800980505E   1C                   ENDDO                                                  *FINE SUBFILE
058900980505     C*
059000980505     C* ORDINA LE SCHIERE VIDEO
059100980505     C                   EXSR      ORDSCH
059200980505     C*
059300980505     C                   ENDSR
059400980505     C*------------------------------------------------------------------------*
059500980505     C* ORDSCH - ORDINA LE SCHIERE VIDEO
059600980505     C*------------------------------------------------------------------------*
059700980505     C     ORDSCH        BEGSR
059800980505     C*
059900980505DO  1C     1             DO        10            J                 4 0          1°  C I C L O
060000980505IF  2C     VSGL(J)       IFGT      *ZEROS
060100980505     C     1             ADD       J             K                 4 0
060200980505DO  3C     K             DO        10            K                              2°  C I C L O
060300980505IF  4C     VSGL(K)       IFGT      *ZEROS
060400980505IF  5C     VSGL(J)       IFGT      VSGL(K)                                      *ORDINAMENTO
060500980507     C* SCAGLIONE
060600980507     C                   Z-ADD     VSGL(J)       N133             13 3
060700980507     C                   Z-ADD     VSGL(K)       VSGL(J)
060800980507     C                   Z-ADD     N133          VSGL(K)
060900980505     C* PESO
061000980505     C                   Z-ADD     VPSC(J)       N71               7 1
061100980505     C                   Z-ADD     VPSC(K)       VPSC(J)
061200980505     C                   Z-ADD     N71           VPSC(K)
061300980505     C* COLLI
061400980505     C                   Z-ADD     VCSC(J)       N71               7 1
061500980505     C                   Z-ADD     VCSC(K)       VCSC(J)
061600980505     C                   Z-ADD     N71           VCSC(K)
061700980505     C* VOLUME
061800980505     C                   Z-ADD     VVSC(J)       N73               7 3
061900980505     C                   Z-ADD     VVSC(K)       VVSC(J)
062000980505     C                   Z-ADD     N73           VVSC(K)
062100980505     C* INOLTRO
062200980505     C                   Z-ADD     VINL(J)       N11              11 0
062300980505     C                   Z-ADD     VINL(K)       VINL(J)
062400980505     C                   Z-ADD     N11           VINL(K)
062500980505     C* RPV
062600980505     C                   Z-ADD     VRPV(J)       N31               3 1
062700980505     C                   Z-ADD     VRPV(K)       VRPV(J)
062800980505     C                   Z-ADD     N31           VRPV(K)
062900980505     C* SEGNO %DELTA
063000980505     C                   MOVEL     VSDE(J)       A1                1
063100980505     C                   MOVEL     VSDE(K)       VSDE(J)
063200980505     C                   MOVEL     A1            VSDE(K)
063300980505     C* %DELTA
063400980505     C                   Z-ADD     VPDE(J)       N63               6 3
063500980505     C                   Z-ADD     VPDE(K)       VPDE(J)
063600980505     C                   Z-ADD     N63           VPDE(K)
063700980505     C* %INCIDENZA
063800980505     C                   Z-ADD     VPPE(J)       N41               4 1
063900980505     C                   Z-ADD     VPPE(K)       VPPE(J)
064000980505     C                   Z-ADD     N41           VPPE(K)
064100980505     C* ASTERISCO
064200980505     C                   MOVEL     VAST(J)       A1                1
064300980505     C                   MOVEL     VAST(K)       VAST(J)
064400980505     C                   MOVEL     A1            VAST(K)
064500980505E   5C                   ENDIF
064600980505E   4C                   ENDIF
064700980505E   3C                   ENDDO
064800980505E   2C                   ENDIF
064900980505E   1C                   ENDDO
065000980505     C*
065100980505     C                   ENDSR
065200970117     C*------------------------------------------------------------------------*
065300970117     C* AGGSFL - AGGIORNA RIGA DI SUBFILE
065400970117     C*------------------------------------------------------------------------*
065500970117     C     AGGSFL        BEGSR
065600970117     C*
065700970117     C                   SETON                                        80        *SI SFLNXTCHG
065800970117IF  1C     $TGES         IFEQ      'I'
065900970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
066000970117X   1C                   ELSE
066100970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
066200970117E   1C                   ENDIF
066300970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
066400970506     C                   SETON                                        02        *ND ALCUNI CAMPI
066500970506X   1C                   ELSE                                                   *CALCOLO DELTA
066600970506     C                   SETOFF                                       02        *NESSUNA ND
066700970506E   1C                   ENDIF
066800970117     C                   UPDATE    TE02S01                                      *AGGIORNA SUBFILE
066900970117     C                   SETOFF                                       80        *NO SFLNXTCHH
067000970117     C*
067100970117     C                   ENDSR
067200970117     C*------------------------------------------------------------------------*
067300970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
067400970117     C*------------------------------------------------------------------------*
067500970117     C     F03GES        BEGSR
067600970117     C*
067700970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
067800970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
067900011001     C                   MOVEL     'N'           D00ETS                         *S=TEETS CONTROLLATO
068000970117     C*
068100970117     C                   ENDSR
068200970117     C*------------------------------------------------------------------------*
068300970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
068400970117     C*------------------------------------------------------------------------*
068500970117     C     F12GES        BEGSR
068600970117     C*
068700970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
068800970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
068900011001     C                   MOVEL     'N'           D00ETS                         *S=TEETS CONTROLLATO
069000970117     C*
069100970117     C                   ENDSR
069200060829     C*------------------------------------------------------------------------*
069300060829     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
069400060829     C*------------------------------------------------------------------------*
069500060829     C     F05GES        BEGSR
069600060829     C*
069700060829     C* GESTIONE VIDETA
069800060829     C                   EXSR      IMPWD2
069900060829DO  1C     $FINV         DOUEQ     'S'
070000060829     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
070100060829E   1C                   ENDDO
070200060829     C*
070300060829     C* MEMORIZZA GLI SCAGLIONI DELLA TARIFFA RICHIESTA E CARICA IL SFILE
070400060829IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
070500060829     C                   EXSR      MEMETS                                       *MEMORIZZA
070600060829     C                   EXSR      POSSFL                                       *CARICA
070700060829E   1C                   ENDIF
070800060829     C*
070900060829     C                   ENDSR
071000970117     C*------------------------------------------------------------------------*
071100060829     C* F07GES - GESTIONE TASTO FUNZIONALE F07 ==> scaglioni x competenze
071200970117     C*------------------------------------------------------------------------*
071300060829     C     F07GES        BEGSR
071400970117     C*
071500060829     c                   if        compric = ' '
071600060829     c                   eval      compric = 'C'
071700060829     c                   movel     param         kpjbu
071800060829     c                   call      'TNTE02C'
071900060829     c                   parm                    kpjba
072000060829     c                   parm                    dste00
072100060829     c                   clear                   compric
072200060829     c                   movel(p)  param         kpjbu
072300060829     c                   else
072400060829     c                   clear                   compric
072500060829     c                   movel(p)  param         kpjbu
072600060829     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
072700060829     c                   end
072800970117     C*
072900970117     C                   ENDSR
073000970117     C*------------------------------------------------------------------------*
073100970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
073200970117     C*------------------------------------------------------------------------*
073300970117     C     IMPWD2        BEGSR
073400970117     C*
073500091120     c                   if        d00dsf = ' '
073600091120     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
073700091120     c                   eval      s2wcto = '(T/O)'
073800091120     c                   eval      d2wksc = 'Cliente o Visita ...........:'
073900091120     c                   eval      d2wprg = 'Progressivo ................:'
074000091120     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
074100091120     c                             Tariffa/Offerta'
074200091120     c                   else
074300091201     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
074400091130     c                   eval      s2wcto = '(T/O)'
074500091120     c                   eval      d2wksc = 'Cliente o Trattativa .......:'
074600091120     c                   eval      d2wprg = 'Versione ...................:'
074700091120     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
074800091120     c                             Tariffa/Trattativa'
074900091120     c                   end
075000970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
075100970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
075200970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
075300980505     C                   MOVEL     D0DCTO        V2WCTO
075400970319     C                   Z-ADD     D0DKSC        V2WKSC
075500970319     C                   Z-ADD     D0DCTR        V2WCTR
075600970319     C                   Z-ADD     D0DPRG        V2WPRG
075700970319     C                   Z-ADD     D0DLNP        V2WLNP
075800011003     C                   MOVEL     D0DDIV        V2WDIV
075900970117     C                   EXSR      SETWD2
076000970117     C*
076100970117     C                   ENDSR
076200970117     C*------------------------------------------------------------------------*
076300970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
076400970117     C*------------------------------------------------------------------------*
076500970117     C     SETWD2        BEGSR
076600970117     C*
076700970117     C                   SETOFF                                       303132     ERRORI
076800011003     C                   SETOFF                                       333435
076900011003     C                   SETOFF                                       36
077000970117     C                   SETOFF                                       90         GENERICO DI ERRORE
077100970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
077200970117     C*
077300970117     C                   ENDSR
077400970117     C*------------------------------------------------------------------------*
077500970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
077600970117     C*------------------------------------------------------------------------*
077700970117     C     GESWD2        BEGSR
077800970117     C*
077900970117     C* EMISSIONE VIDEATA
078000970117     C                   EXFMT     TE02W02
078100970117     C                   EXSR      SETWD2
078200970117     C* F12-RITORNO
078300970117IF  1C     *INKL         IFEQ      *ON
078400970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
078500970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
078600970117     C                   GOTO      FINWD2
078700970117E   1C                   ENDIF
078800980504     C* F11-RICERCA
078900980504IF  1C     *INKK         IFEQ      *ON
079000980504     C                   EXSR      GESRIC
079100980504     C                   GOTO      FINWD2
079200980504E   1C                   ENDIF
079300970117     C* CONTROLLA VIDEO
079400970117     C                   EXSR      CTRWD2
079500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
079600060825     C*                  GOTO      FINWD2
079700970117E   1C                   ENDIF
079800970117     C* F06-CONFERMA
079900970117IF  1C     *INKF         IFEQ      *ON
080000970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
080100970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
080200970117     C                   GOTO      FINWD2
080300970117E   1C                   ENDIF
080400970117     C*
080500970117     C     FINWD2        ENDSR
080600980504     C*------------------------------------------------------------------------*
080700980504     C* GESRIC - GESTIONE RICERCA DATI TIPO
080800980504     C*------------------------------------------------------------------------*
080900980504     C     GESRIC        BEGSR
081000980504     C*
081100980504     C                   CLEAR                   DSTE30
081200980504     C                   MOVEL     'C01'         D30OP0
081300060829     c* ricerca scaglioni x ricavi
081400060829     c                   if        compric = *blanks
081500980708     C                   MOVEL     'S'           D30ETX
081600060829     c                   else
081700060829     c* ricerca scaglioni x competenze
081800060829     C                   MOVEL     '1'           D30ETX
081900060829     c                   end
082000980804     C                   Z-ADD     D00KSC        D30KSC
082100011011     C                   MOVEL     D00DIV        D30DIV
082200091123     C                   MOVEL     D00Dsf        D30Dsf
082300980504     C                   CALL      'TNTE32R'
082400980504     C                   PARM                    KPJBA
082500980504     C                   PARM                    DSTE30
082600980504IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
082700980504     C                   SETON                                        90
082800980504     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
082900980504X   1C                   ELSE                                                   *NO ERRORE
083000980504IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
083100980504     C     D30F12        OREQ      '1'
083200980504X   2C                   ELSE                                                   *SCELTA
083300980504     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
083400980504     C                   MOVEL     D30CTO        V2WCTO
083500980504     C                   Z-ADD     D30KSC        V2WKSC
083600980504     C                   Z-ADD     D30CTR        V2WCTR
083700980504     C                   Z-ADD     D30PRG        V2WPRG
083800980504     C                   Z-ADD     D30LNP        V2WLNP
083900011003     C                   MOVEL     D30DIV        V2WDIV
084000980504E   2C                   ENDIF
084100980504E   1C                   ENDIF
084200980504     C*
084300060825     C*                  MOVEL     'S'           $FINV                          *FINE FINESTRA
084400980504     C*
084500980504     C                   ENDSR
084600970117     C*------------------------------------------------------------------------*
084700970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
084800970117     C*------------------------------------------------------------------------*
084900970117     C     CTRWD2        BEGSR
085000970117     C*--------------------
085100091120     C* TARIFFA / OFFERTA  / TRATTATIVA
085200970117     C*--------------------
085300091120     c                   if        (d00dsf = ' '
085400091120IF  1C                             and v2wcto <> 'T' and
085500091120IF  1C                             v2wcto <> 'O') or
085600091120     c                             (d00dsf = 'X'
085700091120IF  1C                             and v2wcto <> 'T' and
085800091130IF  1C                             v2wcto <> 'O')
085900091120     C                   SETON                                        90
086000091123     C                   MOVEL     MSG(12)       $MSG
086100091120     C                   GOTO      FINCW2
086200091120E   1C                   ENDIF
086300011026     C* REPERIMENTO DATI TARIFFA
086400011026     C                   Z-ADD     V2WKSC        KTMKSC                         *CLIENTE
086500011026     C                   Z-ADD     V2WCTR        KTMCTR                         *TARIFFA
086600011026     C                   Z-ADD     V2WPRG        KTMPRG                         *PROGRESSIVO
086700011026     C     KEYTAM        CHAIN     TNTAM01L
086800011026     C                   IF        %found(TNTAM01L)
086900011026     C                   MOVEL     TAMFLO        DSTA01
087000011026     C                   MOVEL     §TADIV        V2WDIV
087100011026E   2C                   ENDIF
087200060825     C*--------------------
087300061128     C* stessa tipologia di codice tariffa eccetto x 0 e 3 che li considero
087400061128     c* congruenti
087500060825     C*--------------------
087600060825     c                   movel     d00ctr        comd00            1 0
087700060825     c                   movel     v2wctr        comv2w            1 0
087800061128     c                   if        comd00 <> comv2w
087900061128     c                   if        (comd00 = 0  or
088000061128     c                             comd00 = 3) and
088100061128     c                             (comv2w = 0 or
088200061128     c                             comv2w = 3)
088300061128     c                   else
088400060825     C                   SETON                                        31
088500060825     C                   SETON                                        90
088600060825     C                   MOVEL     MSG(24)       $MSG
088700060825     C                   GOTO      FINCW2
088800060825     c                   end
088900061128     c                   end
089000970117     C*--------------------
089100970117     C* LINEA DI PARTENZA
089200970117     C*--------------------
089300970117     C                   MOVEL     V2WLNP        WLNP
089400970117     C                   MOVEL     *BLANKS       WDLNP
089500970117     C                   MOVEL     '0'           WERR
089600970117     C                   EXSR      CHKLNP
089700980818IF  1C     WERR          IFEQ      '1'
089800980818IF  2C     WLNP          IFEQ      888
089900980818     C     WLNP          OREQ      889
090000980818     C                   MOVEL     '0'           WERR
090100980818     C                   MOVEL     DDEC(10)      WDLNP
090200980818X   2C                   ELSE
090300011003     C                   SETON                                        343690
090400970117     C                   MOVEL     MSG(11)       $MSG
090500970117     C                   GOTO      FINCW2
090600980818E   2C                   ENDIF
090700980818E   1C                   ENDIF
090800970117     C*--------------------
090900970117     C* DATI TIPO ESISTENTI
091000970117     C*--------------------
091100091120     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE/FILIALE
091200970117     C                   MOVEL     V2WCTO        KTSCTO                         *TARIFFA O OFFERTA
091300970117     C                   Z-ADD     V2WKSC        KTSKSC                         *CLIENTE O VISITA
091400970117     C                   Z-ADD     V2WCTR        KTSCTR                         *TARIFFA
091500970117     C                   Z-ADD     V2WPRG        KTSPRG                         *PROGRESSIVO
091600970117     C                   Z-ADD     V2WLNP        KTSLNP                         *LINEA DI PARTENZA
091700970619     C                   MOVEL     'N'           WEETS                          *ESISTENZA SGL
091800011001     C     KE2ETS        SETLL     TEETS01L
091900011001     C     KE2ETS        READE     TEETS01L                               99
092000970619DO  1C     *IN99         DOWEQ     *OFF
092100970619IF  2C     ETSATB        IFEQ      *BLANKS
092200970619     C                   MOVEL     'S'           WEETS                          *ESISTENZA SGL
092300970619E   2C                   ENDIF
092400011001     C     KE2ETS        READE     TEETS01L                               99
092500970619E   1C                   ENDDO                                                  *FINE SCAGLIONI
092600970619IF  1C     WEETS         IFEQ      'N'                                          *NON TROVATI
092700970117     C                   SETON                                        303132
092800980504     C                   SETON                                        333435
092900011003     C                   SETON                                        36
093000980504     C                   SETON                                        90
093100970117     C                   MOVEL     MSG(13)       $MSG
093200970117E   1C                   ENDIF
093300970117     C*
093400970117     C     FINCW2        ENDSR
093500970117     C*------------------------------------------------------------------------*
093600970117     C* MEMETS - MEMORIZZA GLI SCAGLIONI DELLA TARIFFA RICHIESTA
093700970117     C*------------------------------------------------------------------------*
093800970117     C     MEMETS        BEGSR
093900980505     C*
094000980505     C* PULISCE LE SCHIERE DI CARICAMENTO DA ALTRA TARIFFA
094100980505     C                   EXSR      CLRSCC
094200970117     C*
094300970117     C* CARICA I DATI TIPO DELLA TARIFFA RICHIESTA
094400091120     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE/FILIALE
094500970117     C                   MOVEL     V2WCTO        KTSCTO                         *TARIFFA O OFFERTA
094600970117     C                   Z-ADD     V2WKSC        KTSKSC                         *CLIENTE O VISITA
094700970117     C                   Z-ADD     V2WCTR        KTSCTR                         *TARIFFA
094800970117     C                   Z-ADD     V2WPRG        KTSPRG                         *PROGRESSIVO
094900970117     C                   Z-ADD     V2WLNP        KTSLNP                         *LINEA DI PARTENZA
095000011001     C     KE2ETS        SETLL     TEETS01L                               99
095100011001     C     KE2ETS        READE(N)  TEETS01L                               99
095200970117DO  1C     *IN99         DOWEQ     *OFF
095300970117IF  2C     ETSATB        IFEQ      *BLANKS
095400980505     C                   ADD       1             C
095500980505     C                   Z-ADD     ETSSGL        CSGL(C)                        *SCAGLIONE
095600980505     C                   Z-ADD     ETSPSC        CPSC(C)                        *PESO MEDIO
095700980928     C******************** Z-ADDETSCSC    CCSC,C           *COLLI MEDI
095800980928     C     ETSCSC        IFLT      1
095900980928     C                   Z-ADD     1             CCSC(C)
096000980928     C                   ELSE
096100980928     C                   Z-ADD     ETSCSC        CCSC(C)                        *COLLI MEDI
096200980928     C                   ENDIF
096300980505     C                   Z-ADD     ETSVSC        CVSC(C)                        *VOLUME MEDIO
096400980505     C                   Z-ADD     ETSINL        CINL(C)                        *INOLTRO
096500980505     C                   Z-ADD     ETSRPV        CRPV(C)                        *RAPPORTO PESO/VOLUM
096600980505     C                   MOVEL     ETSSDE        CSDE(C)                        *SEGNO DELTA
096700980505     C                   Z-ADD     ETSPDE        CPDE(C)                        *% DELTA
096800980505     C                   Z-ADD     ETSPPE        CPPE(C)                        *% PESO
096900970117E   2C                   ENDIF
097000011001     C     KE2ETS        READE(N)  TEETS01L                               99
097100970117E   1C                   ENDDO
097200980505     C*
097300980505     C* PULISCE LE SCHIERE VIDEO
097400980505     C                   EXSR      CLRSCV
097500970117     C*
097600980505     C* MEMORIZZA I DATI TIPO DELLA TARIFFA RICHIESTA NELLE SCHIERE VIDEO
097700980505     C                   MOVEA     CSGL          VSGL                           *CAMPI
097800980505     C                   MOVEA     CPSC          VPSC
097900980505     C                   MOVEA     CCSC          VCSC
098000980505     C                   MOVEA     CVSC          VVSC
098100980505     C                   MOVEA     CINL          VINL
098200980505     C                   MOVEA     CRPV          VRPV
098300980505     C                   MOVEA     CSDE          VSDE
098400980505     C                   MOVEA     CPDE          VPDE
098500980505     C                   MOVEA     CPPE          VPPE
098600980505     C                   MOVEL     *BLANKS       VAST
098700980508     C*
098800980508     C* DEPOSITA NEI CAMPI DELLA DS LA TARIFFA RICHIESTA IN COPIA
098900980508     C                   MOVEL     V2WCTO        D0DCTO
099000980508     C                   Z-ADD     V2WKSC        D0DKSC
099100980508     C                   Z-ADD     V2WCTR        D0DCTR
099200980508     C                   Z-ADD     V2WPRG        D0DPRG
099300980508     C                   Z-ADD     V2WLNP        D0DLNP
099400011003     C                   MOVEL     V2WDIV        D0DDIV
099500980508     C*
099600970117     C                   ENDSR
099700980505     C*------------------------------------------------------------------------*
099800980505     C* CLRSCC - PULISCE LE SCHIERE DI CARICAMENTO DA ALTRA TARIFFA
099900980505     C*------------------------------------------------------------------------*
100000980505     C     CLRSCC        BEGSR
100100980505     C*
100200980505     C                   Z-ADD     *ZEROS        C                 4 0          *INDICE
100300980505     C                   CLEAR                   CSGL                           *CAMPI
100400980505     C                   CLEAR                   CPSC
100500980505     C                   CLEAR                   CCSC
100600980505     C                   CLEAR                   CVSC
100700980505     C                   CLEAR                   CINL
100800980505     C                   CLEAR                   CRPV
100900980505     C                   CLEAR                   CSDE
101000980505     C                   CLEAR                   CPDE
101100980505     C                   CLEAR                   CPPE
101200980505     C*
101300980505     C                   ENDSR
101400970117     C*------------------------------------------------------------------------*
101500970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
101600970117     C*------------------------------------------------------------------------*
101700970117     C     F06GES        BEGSR
101800980507     C*
101900980507     C* PER TUTTI GLI SCAGLIONI INSERITI
102000980507     C                   Z-ADD     1             $NRR
102100980507DO  1C     1             DO        10            V
102200980507IF  2C     VSGL(V)       IFGT      *ZEROS
102300980507     C*--------------------
102400980507     C* SCAGLIONE UNA VOLTA SOLA
102500980507     C*--------------------
102600980507IF  3C     WERR          IFEQ      '0'                                          *NO ERRORI
102700980507     C                   Z-ADD     1             I
102800980507     C                   SETOFF                                           98
102900980507     C     VSGL(V)       LOOKUP    VSGL(I)                                98
103000980507IF  4C     *IN98         IFEQ      *ON                                          *TROVATO
103100980507     C     V             ANDNE     I                                             *<> DA SE' STESSO
103200980507     C                   SETON                                        2190
103300980507     C                   MOVEL     MSG(6)        $MSG
103400980507     C                   MOVEL     '1'           WERR
103500980507     C                   Z-ADD     V             $NRR
103600980507     C                   LEAVE
103700980507E   4C                   ENDIF
103800980507E   3C                   ENDIF
103900980507     C*--------------------
104000980507     C* VALORE SCAGLIONE
104100980507     C*--------------------
104200980507     C* IL VALORE SCAGLIONE DEVE ESSERE > DELLO SCAGLIONE PRECEDENTE
104300980507IF  3C     WERR          IFEQ      '0'                                          *NO ERRORI
104400980507IF  4C     V             IFGT      1                                            *NON PER IL PRIMO
104500980507     C                   MOVEL     D00CTR        A1                1
104600980507IF  5C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
104700980507     C                   Z-ADD     VCSC(V)       W133             13 3
104800980507X   5C                   ELSE                                                   *TARIFFA A QLI/KG
104900980507     C                   Z-ADD     VPSC(V)       W133             13 3
105000980507E   5C                   ENDIF
105100980507     C     V             SUB       1             I
105200980507IF  5C     W133          IFLE      VSGL(I)
105300980507     C                   MOVEL     D00CTR        A1                1
105400980507IF  6C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
105500980507     C                   SETON                                        23
105600980507X   6C                   ELSE                                                   *TARIFFA A QLI/KG
105700980507     C                   SETON                                        22
105800980507E   6C                   ENDIF
105900980507     C                   SETON                                        90
106000980507     C                   MOVEL     MSG(17)       $MSG
106100980507     C                   MOVEL     '1'           WERR
106200980507     C                   Z-ADD     V             $NRR
106300980507     C                   LEAVE
106400980507E   5C                   ENDIF
106500980507E   4C                   ENDIF
106600980507E   3C                   ENDIF
106700980507     C*---
106800980507     C* DELTA -> SCAGLIONI DATI TIPO = SCAGLIONI TARIFFA
106900980507     C*---
107000980507IF  3C     WERR          IFEQ      '0'
107100980507IF  4C     D00TLA        IFEQ      'D'
107200980507IF  5C     TSGL(V)       IFNE      VSGL(V)
107300980507     C                   SETON                                        90
107400980507     C                   MOVEL     MSG(10)       $MSG
107500980507     C                   MOVEL     '1'           WERR
107600980507     C                   Z-ADD     V             $NRR
107700980507     C                   LEAVE
107800980507E   5C                   ENDIF
107900980507E   4C                   ENDIF
108000980507E   3C                   ENDIF
108100980507     C*---
108200980507     C* TOTALE PESO % = 100
108300980507     C*---
108400980507IF  3C     WERR          IFEQ      '0'
108500980507IF  4C     V1CPPE        IFNE      100
108600980507     C                   SETON                                        1990
108700980507     C                   MOVEL     MSG(8)        $MSG
108800980507     C                   MOVEL     '1'           WERR
108900980507     C                   Z-ADD     1             $NRR                           *INIZIO SFILE
109000980507E   4C                   ENDIF
109100980507E   3C                   ENDIF
109200980909     C*---
109300980909     C* ASTERISCO > SOLO PORTO: NO F6 SE CI SONO ASTERISCHI: SCAGL. INCONGR. CON RPV
109400980909     C*---
109500980909IF  3C     WERR          IFEQ      '0'
109600980909IF  4C     D00TLA        IFEQ      'P'                                          *SOLO PER IL PORTO
109700980909IF  5C     VAST(V)       IFNE      *BLANK
109800980909     C                   SETON                                        90
109900980909     C                   MOVEL     MSG(22)       $MSG
110000980909     C                   MOVEL     '1'           WERR
110100980909     C                   Z-ADD     V             $NRR
110200980909     C                   LEAVE
110300980909E   5C                   ENDIF
110400980909E   4C                   ENDIF
110500980909E   3C                   ENDIF
110600980507     C*
110700980507E   2C                   ENDIF
110800980507E   1C                   ENDDO                                                  *FINE SCAGLIONI
110900980507     C*
111000980507     C* LEGGE IL RECORD DEL SFILE
111100980507     C                   Z-ADD     $NRR          NRR                            *POSIZIONAMENTO
111200980507     C     NRR           CHAIN     TE02S01                            98
111300980507     C                   EXSR      AGGSFL                                       *AGGIORNA SFILE
111400980505     C*
111500970117     C* AGGIORNA L'ARCHIVIO
111600980507IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
111700980507IF  2C     $TGES         IFEQ      'G'                                          *SE GESTIONE
111800970117     C                   EXSR      AGGETS
111900980507E   2C                   ENDIF
112000011001     C                   MOVEL     'S'           D00ETS                         *S=TEETS CONTROLLATO
112100970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
112200980507E   1C                   ENDIF
112300970117     C*
112400970117     C                   ENDSR
112500970117     C*------------------------------------------------------------------------*
112600011001     C* AGGETS - AGGIORNAMENTO ARCHIVIO -TEETS-
112700970117     C*------------------------------------------------------------------------*
112800970117     C     AGGETS        BEGSR
112900980505     C*
113000980505     C* ANNULLA TUTTI I DATI DELLA TARIFFA IN LINEA
113100091120     C                   MOVEL     'S'           KTSDSF                         *SEDE O FILIALE
113200980505     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
113300980505     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
113400980505     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
113500980505     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
113600980505     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
113700011001     C     KE2ETS        SETLL     TEETS01L                               99
113800011001     C     KE2ETS        READE     TEETS01L                               99
113900980505DO  1C     *IN99         DOWEQ     *OFF
114000980505IF  2C     ETSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
114100980505     C                   MOVEL     'A'           ETSATB
114200980505     C                   MOVEL     *BLANKS       ETSFTR
114300011001     C                   UPDATE    TEETS000                                     *AGGIORNA
114400980505X   2C                   ELSE                                                   *MAI TRASMESSO
114500011001     C                   DELETE    TEETS000                                     *CANCELLA
114600980505E   2C                   ENDIF
114700011001     C     KE2ETS        READE     TEETS01L                               99
114800980505E   1C                   ENDDO
114900011001     C                   UNLOCK    TEETS01L
115000970117     C*
115100980505     C* LEGGE TUTTI I RECORD CARICATI DAL VIDEO E AGGIORNA IL FILE
115200980505DO  1C     1             DO        10            V
115300980506IF  2C     VSGL(V)       IFGT      *ZEROS
115400970117     C*
115500970117     C* LEGGE IL RECORD IN ARCHIVIO
115600091120     C                   MOVEL     'S'           KTSDSF
115700970117     C                   MOVEL     D00CTO        KTSCTO
115800970117     C                   Z-ADD     D00KSC        KTSKSC
115900970117     C                   Z-ADD     D00CTR        KTSCTR
116000970117     C                   Z-ADD     D00PRG        KTSPRG
116100970117     C                   Z-ADD     D00LNP        KTSLNP
116200980506     C                   Z-ADD     VSGL(V)       KTSSGL
116300011001     C     KEYETS        CHAIN     TEETS01L                           99
116400970117     C* INSERIMENTO
116500970117IF  3C     *IN99         IFEQ      *ON
116600970117     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
116700011001     C                   WRITE     TEETS000
116800970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZ.IN TESTATA
116900970117X   3C                   ELSE
117000970117     C* VARIAZIONE
117100970117     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
117200011001     C                   UPDATE    TEETS000
117300970117E   3C                   ENDIF
117400970117E   2C                   ENDIF
117500980506E   1C                   ENDDO                                                  *FINE SCHIERA
117600980505     C*
117700970117     C                   ENDSR
117800970117     C*------------------------------------------------------------------------*
117900011001     C* IMPETS - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETS-
118000970117     C*------------------------------------------------------------------------*
118100970117     C     IMPETS        BEGSR
118200970117     C*
118300970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
118400980507     C                   MOVEL     KTSDSF        ETSDSF
118500980507     C                   MOVEL     KTSCTO        ETSCTO
118600970117     C                   Z-ADD     KTSKSC        ETSKSC
118700970117     C                   Z-ADD     KTSCTR        ETSCTR
118800970117     C                   Z-ADD     KTSPRG        ETSPRG
118900970117     C                   Z-ADD     KTSLNP        ETSLNP
119000970117     C                   Z-ADD     KTSSGL        ETSSGL
119100970117     C                   Z-ADD     DATCOR        ETSDIR
119200970117     C                   Z-ADD     *ZEROS        ETSDUV
119300970117     C                   MOVEL     *BLANKS       ETSFTR
119400970117     C                   Z-ADD     *ZEROS        ETSDTR
119500970117X   1C                   ELSE                                                   *VARIAZIONE
119600970117     C                   MOVEL     *BLANKS       ETSFTR
119700970117     C                   Z-ADD     DATCOR        ETSDUV
119800970117E   1C                   ENDIF
119900980506     C* IMPOSTA CAMPI
120000980506     C                   MOVEL     *BLANKS       ETSATB
120100980507     C                   Z-ADD     VPSC(V)       ETSPSC
120200980507     C                   Z-ADD     VCSC(V)       ETSCSC
120300980507     C                   Z-ADD     VVSC(V)       ETSVSC
120400980507     C                   Z-ADD     VINL(V)       ETSINL
120500980507     C                   Z-ADD     VRPV(V)       ETSRPV
120600980507     C                   MOVEL     VSDE(V)       ETSSDE
120700980507     C                   Z-ADD     VPDE(V)       ETSPDE
120800980507     C                   Z-ADD     VPPE(V)       ETSPPE
120900970117     C*
121000970117     C                   ENDSR
121100970117     C*------------------------------------------------------------------------*
121200970117     C* POSSFL - CARICA IL SUBFILE
121300970117     C*------------------------------------------------------------------------*
121400970117     C     POSSFL        BEGSR
121500970117     C*
121600970117     C* ELIMINA IL CONTENUTO DEL SFILE
121700970117     C                   EXSR      CLRSFL
121800970117     C*
121900970117     C* LEGGE IL PRIMO RECORD DA CARICARE
122000980505     C                   EXSR      SETVID
122100970117     C* CARICA IL SUBFILE
122200970117     C                   EXSR      CARSFL
122300970117     C*
122400970117     C                   ENDSR
122500970117     C*------------------------------------------------------------------------*
122600970117     C* CARSFL - CARICA UNA PAGINA DEL SUBFILE
122700970117     C*------------------------------------------------------------------------*
122800970117     C     CARSFL        BEGSR
122900970117     C*
123000980507     C                   Z-ADD     *ZEROS        NRR               4 0
123100970117DO  1C                   DO        SFLPAG
123200980507     C                   ADD       1             NRR
123300970117     C* VARIAZIONE
123400970117IF  2C     SFLEOF        IFEQ      *ZEROS
123500970117     C                   EXSR      IMPREC                                       *IMPOSTA CAMPI
123600970117     C* IMMISSIONE
123700970117X   2C                   ELSE
123800970117     C                   EXSR      AZZREC                                       *AZZERA CAMPI
123900970117E   2C                   ENDIF
124000970117     C* SCRIVE RECORD SFILE
124100970117     C                   WRITE     TE02S01
124200980507     C*
124300980507     C* LETTURA SUCESSIVA RECORD DA CARICARE
124400980507     C                   EXSR      LETVID                                       *LEGGE RECORD
124500970117E   1C                   ENDDO
124600970117     C*
124700970117     C                   ENDSR
124800970117     C*------------------------------------------------------------------------*
124900970117     C* CLRSFL - ELIMINA IL CONTENUTO DEL SUBFILE
125000970117     C*------------------------------------------------------------------------*
125100970117     C     CLRSFL        BEGSR
125200970117     C*
125300970117     C                   Z-ADD     1             $NRR
125400970117     C                   SETOFF                                       81        *NO SFLDSP
125500970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
125600970117     C                   WRITE     TE02C01
125700970117     C                   SETON                                        81        *SI SFLDSP
125800970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
125900970117     C*
126000970117     C                   ENDSR
126100970117     C*------------------------------------------------------------------------*
126200980505     C* SETVID - POSIZIONAMENTO E PRIMA LETTURA RECORD
126300970117     C*------------------------------------------------------------------------*
126400980505     C     SETVID        BEGSR
126500980505     C*
126600980505     C                   Z-ADD     *ZEROS        SFLEOF
126700970117     C*
126800970117     C* SI POSIZIONA DAL RECORD RICHIESTO
126900980505     C                   Z-ADD     1             V                              *INDICE SCHIERE VIDE
127000980505     C*
127100980505     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
127200980507     C     VSGL(V)       IFEQ      *ZEROS                                       *NESSUNA MEMORIZZAZI
127300980505     C                   Z-ADD     1             SFLEOF                         *F I N E   F I L E
127400980505E   1C                   ENDIF
127500970117     C*
127600970117     C                   ENDSR
127700970117     C*------------------------------------------------------------------------*
127800980505     C* LETVID - LETTURA SUCESSIVA RECORD
127900970117     C*------------------------------------------------------------------------*
128000980505     C     LETVID        BEGSR
128100980505     C*
128200980505     C                   ADD       1             V                              *INDICE SCHIERE VIDE
128300980505     C*
128400980505     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
128500980505IF  1C     V             IFGT      10                                           LUNGHEZZA MAX SCHIER
128600980505     C     VSGL(V)       OREQ      *ZEROS                                       *SCHIERA VUOTA
128700980505     C                   Z-ADD     1             SFLEOF
128800980505E   1C                   ENDIF
128900970117     C*
129000970117     C                   ENDSR
129100970117     C*------------------------------------------------------------------------*
129200970117     C* AZZREC - AZZERA I CAMPI DEL SUBFILE
129300970117     C*------------------------------------------------------------------------*
129400970117     C     AZZREC        BEGSR
129500970117     C*
129600980505     C                   Z-ADD     *ZEROS        V1HNRR                         *SE =0 IMMISSIONE
129700970117     C                   MOVEL     *BLANKS       V1SATB
129800970117     C                   Z-ADD     *ZEROS        V1SSGL
129900970117     C                   Z-ADD     *ZEROS        V1SPSC
130000970117     C                   Z-ADD     *ZEROS        V1SCSC
130100970117     C                   Z-ADD     *ZEROS        V1SVSC
130200970317     C                   Z-ADD     *ZEROS        V1SINL
130300970317     C                   Z-ADD     *ZEROS        V1SRPV
130400970402     C                   MOVEL     *BLANKS       V1SSDE
130500970402     C                   Z-ADD     *ZEROS        V1SPDE
130600970117     C                   Z-ADD     *ZEROS        V1SPPE
130700970117     C*
130800970117     C                   Z-ADD     *ZEROS        DUPPSC                         *TASTI DI DUPLICA
130900970117     C                   Z-ADD     *ZEROS        DUPCSC
131000970117     C                   Z-ADD     *ZEROS        DUPVSC
131100970317     C                   Z-ADD     *ZEROS        DUPINL
131200970317     C                   Z-ADD     *ZEROS        DUPRPV
131300970402     C                   MOVEL     *BLANKS       DUPSDE
131400970402     C                   Z-ADD     *ZEROS        DUPPDE
131500970117     C                   Z-ADD     *ZEROS        DUPPPE
131600011009     C***                MOVEL     *BLANKS       V1SAST
131700011009     C                   SETOFF                                       70
131800970203     C* INDICATORI
131900970203     C                   SETOFF                                       80        *NO SFLNXTCHH
132000970203IF  1C     $TGES         IFEQ      'I'
132100970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
132200970203X   1C                   ELSE
132300970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
132400970203E   1C                   ENDIF
132500970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
132600970506     C                   SETON                                        02        *ND ALCUNI CAMPI
132700970506X   1C                   ELSE                                                   *CALCOLO DELTA
132800970506     C                   SETOFF                                       02        *NESSUNA ND
132900970506E   1C                   ENDIF
133000970117     C*
133100970117     C                   ENDSR
133200970117     C*------------------------------------------------------------------------*
133300970117     C* IMPREC - IMPOSTA I CAMPI DEL SUBFILE
133400970117     C*------------------------------------------------------------------------*
133500970117     C     IMPREC        BEGSR
133600970117     C*
133700980505     C                   Z-ADD     V             V1HNRR                         *SE =1 VARIAZIONE
133800970117     C                   MOVEL     *BLANKS       V1SATB
133900980505     C                   Z-ADD     VSGL(V)       V1SSGL
134000980505     C                   Z-ADD     VPSC(V)       V1SPSC
134100011009     C                   Z-ADD     VCSC(V)       V1SCSC
134200980505     C                   Z-ADD     VVSC(V)       V1SVSC
134300980505     C                   Z-ADD     VINL(V)       V1SINL
134400980505     C                   Z-ADD     VRPV(V)       V1SRPV
134500980505     C                   MOVEL     VSDE(V)       V1SSDE
134600980505     C                   Z-ADD     VPDE(V)       V1SPDE
134700980505     C                   Z-ADD     VPPE(V)       V1SPPE
134800980505     C*
134900011009     C***                MOVEL     *BLANKS       V1SAST
135000011009     C                   SETOFF                                       70
135100980505     C                   EXSR      CHKAST                                        *CONTROLLA ASTERISC
135200970117     C*
135300970117     C                   Z-ADD     V1SPSC        DUPPSC                         *TASTI DI DUPLICA
135400970117     C                   Z-ADD     V1SCSC        DUPCSC
135500970117     C                   Z-ADD     V1SVSC        DUPVSC
135600970317     C                   Z-ADD     V1SINL        DUPINL
135700970317     C                   Z-ADD     V1SRPV        DUPRPV
135800970402     C                   MOVEL     V1SSDE        DUPSDE
135900970402     C                   Z-ADD     V1SPDE        DUPPDE
136000970117     C                   Z-ADD     V1SPPE        DUPPPE
136100970203     C*
136200970203     C                   SETOFF                                       80        *NO SFLNXTCHH
136300970203IF  1C     $TGES         IFEQ      'I'
136400970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
136500970203X   1C                   ELSE
136600970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
136700970203E   1C                   ENDIF
136800970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
136900970506     C                   SETON                                        02        *ND ALCUNI CAMPI
137000970506X   1C                   ELSE                                                   *CALCOLO DELTA
137100970506     C                   SETOFF                                       02        *NESSUNA ND
137200970506E   1C                   ENDIF
137300970117     C*
137400970117     C                   ENDSR
137500970117     C*------------------------------------------------------------------------*
137600970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
137700970117     C*------------------------------------------------------------------------*
137800970117     C     CHKLNP        BEGSR
137900970117     C*
138000970117     C     *LIKE         DEFINE    V0CLNP        WLNP
138100970117     C     *LIKE         DEFINE    V0DLNP        WDLNP
138200970117     C*
138300970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
138400970117     C                   MOVEL     WLNP          KORFIL
138500970117     C     KEYORG        CHAIN     AZORG01L                           99
138600970117IF  1C     *IN99         IFEQ      *OFF
138700970117     C     ORGFVA        ANDEQ     *BLANKS
138800970117     C     ORGFAG        ANDNE     'V'
138900970117     C                   MOVEL     ORGDES        WDLNP
139000970117X   1C                   ELSE
139100970117     C                   MOVEL     '1'           WERR
139200970117E   1C                   ENDIF
139300970117     C*
139400970117     C                   ENDSR
139500970117     C*------------------------------------------------------------------------*
139600970117     C* IMPVD0 - IMPOSTA LA TESTATA
139700970117     C*------------------------------------------------------------------------*
139800970117     C     IMPVD0        BEGSR
139900970117     C*
140000970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
140100970117     C*---
140200970117     C* TESTATA
140300970117     C*---
140400970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
140500970117IF  1C     D00TLA        IFEQ      'P'
140600970117     C                   MOVEL(P)  'PORTO'       V0CTLA                         *LIVELLO PROCEDURA
140700970117X   1C                   ELSE
140800970117     C                   MOVEL(P)  'DELTA'       V0CTLA                         *LIVELLO PROCEDURA
140900970117E   1C                   ENDIF
141000091120     C                   MOVEL     'Prg.'        d0cprg                         *TARIFFA O OFFERTA
141100091120     c                   if        %subst(d00op0: 1: 1) = 'O'
141200091120     c                   if        d00dsf =' '
141300091120     C                   MOVEL     'Visita '     d0cKsc                         *TARIFFA O OFFERTA
141400091120X   1C                   ELSE
141500091120     C                   MOVEL     'Tratt. '     d0cksc                         *TARIFFA O OFFERTA
141600091120     C                   MOVEL     'Ver.'        d0cprg                         *TARIFFA O OFFERTA
141700091120E   1C                   ENDIF
141800091120X   1C                   ELSE
141900091120     C                   MOVEL     'Cliente'     d0cksc                         *TARIFFA O OFFERTA
142000091120E   1C                   ENDIF
142100970117IF  1C     $TGES         IFEQ      'I'
142200970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
142300970117     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
142400970117X   1C                   ELSE
142500970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
142600970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
142700970117E   1C                   ENDIF
142800970117     C* CAMPI
142900970117     C                   Z-ADD     D00KSC        V0CKSC
143000970117     C                   Z-ADD     D00CTR        V0CCTR
143100970117     C                   Z-ADD     D00PRG        V0CPRG
143200970117     C                   MOVEL     D00FIE        V0CFIE
143300970117     C                   MOVEL     D00TSP        V0CTSP
143400970117     C                   Z-ADD     D00LNP        V0CLNP
143500011003     C                   MOVEL     D00DIV        V0CDIV
143600970117IF  1C     D00DTC        IFGT      *ZEROS
143700970117     C                   Z-ADD     D00DTC        G08INV
143800970117     C                   Z-ADD     *ZEROS        G08DAT
143900970117     C                   MOVEL     '3'           G08ERR
144000970117     C                   CALL      'XSRDA8'
144100970117     C                   PARM                    WLBDA8
144200970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
144300970117E   1C                   ENDIF
144400970117     C* DECODIFICHE
144500970117     C                   MOVEL     *BLANKS       V0DKSC
144600970117IF  1C     D00KSC        IFGT      *ZEROS
144700020906     C                   Z-ADD     1             KACKUT
144800970117     C                   Z-ADD     KCCCL         KACKCC
144900970117     C                   Z-ADD     D00KSC        KACKSC
145000091203     C*    KEYACO        CHAIN     CNACO00F                           99
145100000211IF  2C*---       *IN99     IFEQ *OFF
145200000211     C*---                 MOVELACORAG    V0DKSC           *CLIENTE
145300000211E   2C*---                 ENDIF
145400091120IF  2C                   IF        %subst(d00op0: 1: 1) ='T'                    *TARIFFA
145500000211     C     KEYACO        CHAIN     CNACO00F                           99
145600000211X   2C                   ELSE                                                   *OFFERTA
145700000211     C     KEYACO        CHAIN     TFACO00F                           99
145800000211E   2C                   ENDIF
145900000211IF  2C     *IN99         IFEQ      *OFF
146000000211     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
146100091203     c                   else
146200091203     c                   clear                   tnte04ds
146300091203     C                   movel     D00dsf        d04dsf
146400091203     C                   Z-ADD     D00KSC        d04nrv
146500091203     c                   call      'TNTE04R'
146600091203     c                   parm                    kpjba
146700091203     c                   parm                    tnte04ds
146800091203     c                   movel     d04rag        v0dksc
146900000211E   2C                   ENDIF
147000970117E   1C                   ENDIF
147100970117     C*
147200970117     C                   MOVEL     *BLANKS       V0DCTR
147300970117IF  1C     D00KSC        IFGT      *ZEROS
147400980507     C                   MOVEL     D00CTR        A1                1
147500970117     C                   SETOFF                                           99
147600970117     C                   Z-ADD     1             I
147700980507     C     A1            LOOKUP    TTR(I)                                 99
147800970117IF  2C     *IN99         IFEQ      *ON
147900970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
148000970117E   2C                   ENDIF
148100970117E   1C                   ENDIF
148200970117     C*
148300970117     C                   MOVEL     *BLANKS       V0DTSP
148400970117IF  1C     D00TSP        IFNE      *BLANKS
148500970117     C                   SETOFF                                           99
148600970117     C                   Z-ADD     1             I
148700970117     C     D00TSP        LOOKUP    TSP(I)                                 99
148800970117IF  2C     *IN99         IFEQ      *ON
148900970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
149000970117E   2C                   ENDIF
149100970117E   1C                   ENDIF
149200970117     C*
149300970117     C                   MOVEL     *BLANKS       V0DFIE
149400970117IF  1C     D00FIE        IFEQ      'I'
149500970117     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
149600970117E   1C                   ENDIF
149700970117IF  1C     D00FIE        IFEQ      'E'
149800970117     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
149900970117E   1C                   ENDIF
150000970117     C*
150100970117     C                   MOVEL     *BLANKS       V0DLNP
150200970117IF  1C     D00LNP        IFGT      *ZEROS
150300970117     C                   Z-ADD     D00LNP        KORFIL
150400970117     C     KEYORG        CHAIN     AZORG01L                           99
150500970117IF  2C     *IN99         IFEQ      *OFF
150600970117     C     ORGFVA        ANDEQ     *BLANKS
150700970117     C     ORGFAG        ANDNE     'V'
150800970117     C                   MOVEL     ORGDES        V0DLNP
150900970117E   2C                   ENDIF
151000970117E   1C                   ENDIF
151100970117     C*---
151200970117     C* SUBFILE CONTROL
151300970117     C*---
151400091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE O FILIALE
151500980505     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
151600970117     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE
151700970117     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
151800970117     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
151900970117     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
152000060829     c                   clear                   etcbnb
152100011001     C     KEYETC        CHAIN     TEETC01L                           99
152200970117IF  1C     *IN99         IFEQ      *OFF
152300970117     C                   Z-ADD     ETCPMC        V1CPMC                         *PESO MEDIO COLLO
152400970117     C                   Z-ADD     ETCVMC        V1CVMC                         *VOLUME MEDIO COLLO
152500970117E   1C                   ENDIF
152600060829     c* abilito il comando x vedere scaglioni competenze
152700060829     c     etcbnb        comp      0                                  4444
152800060829     c* se sono nel sfl competetnze abilito sempre comando x vedere scaglion
152900060829     c* ricavi
153000060829     c                   if        compric <> ' '
153100060829     c                   seton                                        44
153200060829     c                   end
153300970117     C                   MOVEL     DVID(2)       DVID02
153400980507     C                   MOVEL     D00CTR        A1                1
153500980507IF  1C     A1            IFEQ      '0'                                          *QUINTALI
153600980507     C     A1            OREQ      '3'                                          *KG
153700970117     C                   MOVEL     DDEC(4)       D02SGL                         *SCAGLIONI ->KG
153800970117     C                   MOVEL     DDEC(4)       D02PSC                         *PESO MEDIO->KG
153900970117E   1C                   ENDIF
154000980507IF  1C     A1            IFEQ      '1'                                          *COLLI
154100970117     C                   MOVEL     DDEC(3)       D02SGL                         *SCAGLIONI ->COLLI
154200970117     C                   MOVEL     DDEC(4)       D02PSC                         *PESO MEDIO->KG
154300970117E   1C                   ENDIF
154400970117     C                   MOVEL     DVID02        V1CDES                         *DESCRIZIONE CAMPI
154500970117     C*
154600970117     C                   ENDSR
154700970117     C*------------------------------------------------------------------------*
154800970117     C* CARTAD - CARICA GLI SCAGLIONI DEL DETTAGLIO TARIFFA
154900970117     C*------------------------------------------------------------------------*
155000970117     C     CARTAD        BEGSR
155100970117     C*
155200970117     C     *LIKE         DEFINE    TADORP        WORP                           *DI LAVORO
155300970117     C*
155400970117     C* CARICA I PRIMI SCAGLIONI DEL DETTAGLIO TARIFFA DI UN CODICE TASSAZIONE
155500970117     C                   CLEAR                   TSGL
155600970117     C                   Z-ADD     *ZEROS        I
155700970117     C                   Z-ADD     D00KSC        KTDKSC
155800970117     C                   Z-ADD     D00CTR        KTDCTR
155900970117     C                   Z-ADD     D00PRG        KTDPRG
156000970117     C                   Z-ADD     D00LNP        KTDLNP
156100990818     C     KEYTAD        SETLL     TITAD04L
156200990818     C     KEYTAD        READE     TITAD04L                               99
156300970117DO  1C     *IN99         DOWEQ     *OFF
156400970117     C                   MOVEL     TADORP        WORP
156500970117DO  2C     *IN99         DOWEQ     *OFF
156600970117     C     TADORP        ANDEQ     WORP
156700970117IF  3C     TADATB        IFEQ      *BLANKS                                      *NO ANNULLATI
156800970117     C                   ADD       1             I
156900970117     C                   Z-ADD     TADSGL        TSGL(I)
157000970117E   3C                   ENDIF
157100990818     C     KEYTAD        READE     TITAD04L                               99
157200970117E   2C                   ENDDO                                                  *FINE COD.TASSAZIONE
157300970117     C                   LEAVE
157400970117E   1C                   ENDDO                                                  *FINE DETTAGLIO
157500970117     C*
157600970117     C                   ENDSR
157700970117     C*------------------------------------------------------------------------*
157800970117     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
157900970117     C*------------------------------------------------------------------------*
158000970117     C     CTRENT        BEGSR
158100970117     C*--------------------
158200980505     C* INTERROGAZIONE -> DATI TIPO INESISTENTI
158300970117     C*--------------------
158400970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
158500970130     C                   MOVEL     '0'           WSGL              1
158600091120     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE/FILIALE
158700970117     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
158800970117     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE P VISITA
158900970117     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
159000970117     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
159100970117     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
159200011001     C     KE2ETS        SETLL     TEETS01L
159300011001     C     KE2ETS        READE(N)  TEETS01L                               99
159400970130DO  2C     *IN99         DOWEQ     *OFF                                         *NON TROVATO
159500970130IF  3C     ETSATB        IFEQ      *BLANKS
159600970130     C                   MOVEL     '1'           WSGL                           *SCAGLIONI ESISTENTI
159700970130     C                   LEAVE
159800970130E   3C                   ENDIF
159900011001     C     KE2ETS        READE(N)  TEETS01L                               99
160000970130E   2C                   ENDDO
160100970130IF  2C     WSGL          IFEQ      '0'
160200980505     C                   MOVEL     '1'           WERR                           *ERRORE
160300970117     C                   MOVEL     MSG(14)       D00MSG
160400970117     C                   MOVEL     '1'           D00ERR
160500970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
160600970117E   2C                   ENDIF
160700970117E   1C                   ENDIF
160800970117     C*
160900970117     C                   ENDSR
161000980505     C*------------------------------------------------------------------------*
161100980505     C* CARSGL - CARICA GLI SCAGLIONI NELLE SCHIERE VIDEO
161200980505     C*------------------------------------------------------------------------*
161300980505     C     CARSGL        BEGSR
161400980505     C*
161500980505     C* PULISCE LE SCHIERE VIDEO
161600980505     C                   EXSR      CLRSCV
161700980505     C*
161800980505     C* CARICA GLI SCAGLIONI
161900091120     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE/FILIALE
162000980505     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
162100980505     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
162200980505     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
162300980505     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
162400980505     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
162500011001     C     KE2ETS        SETLL     TEETS01L                               99
162600980505IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
162700980505     C                   MOVEL     *BLANKS       V0COPE                         *NESSUNA OPERAZIONE
162800980505X   2C                   ELSE                                                   *GESTIONE
162900980505IF  2C     *IN99         IFEQ      *OFF                                         *NON TROVATO
163000980505     C                   MOVEL     DOPE(1)       V0COPE                         *INSERIMENTO
163100980505X   2C                   ELSE                                                   *TROVATO
163200980505     C                   MOVEL     DOPE(2)       V0COPE                         *VARIAZIONE
163300980505E   2C                   ENDIF
163400980505E   1C                   ENDIF
163500011001     C     KE2ETS        READE(N)  TEETS01L                               99
163600980505DO  1C     *IN99         DOWEQ     *OFF
163700101007     c                   exsr      chkets
163800980505IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
163900980505     C                   ADD       1             V
164000980505     C                   Z-ADD     ETSSGL        VSGL(V)                        *SCAGLIONE
164100980505     C                   Z-ADD     ETSPSC        VPSC(V)                        *PESO MEDIO
164200980928     C******************** Z-ADDETSCSC    VCSC,V           *COLLI MEDI
164300980928     C     ETSCSC        IFLT      1
164400980928     C                   Z-ADD     1             VCSC(V)
164500980928     C                   ELSE
164600980928     C                   Z-ADD     ETSCSC        VCSC(V)                        *COLLI MEDI
164700980928     C                   ENDIF
164800980505     C                   Z-ADD     ETSVSC        VVSC(V)                        *VOLUME MEDIO
164900980505     C                   Z-ADD     ETSINL        VINL(V)                        *INOLTRO
165000980505     C                   Z-ADD     ETSRPV        VRPV(V)                        *RAPPORTO PESO/VOLUM
165100980505     C                   MOVEL     ETSSDE        VSDE(V)                        *SEGNO DELTA
165200980505     C                   Z-ADD     ETSPDE        VPDE(V)                        *% DELTA
165300980505     C                   Z-ADD     ETSPPE        VPPE(V)                        *% PESO
165400980505     C                   MOVEL     *BLANKS       VAST(V)                        *ASTERISCO
165500980505E   2C                   ENDIF
165600011001     C     KE2ETS        READE(N)  TEETS01L                               99
165700980505E   1C                   ENDDO                                                  *FINE FILE
165800980505     C*
165900980505     C                   ENDSR
166000980505     C*------------------------------------------------------------------------*
166100011001     C* CHKETS - CONTROLLA VALIDITA' RECORD -TEETS-
166200980505     C*------------------------------------------------------------------------*
166300980505     C     CHKETS        BEGSR
166400980505     C*
166500980505     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
166600980505     C*
166700980505     C* SCAGLIONI ANNULLATI, ESCLUDE
166800980505IF  1C     ETSATB        IFNE      *BLANKS
166900980505     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
167000980505E   1C                   ENDIF
167100980505     C*
167200980505     C                   ENDSR
167300980505     C*------------------------------------------------------------------------*
167400980505     C* CLRSCV - PULISCE LE SCHIERE VIDEO
167500980505     C*------------------------------------------------------------------------*
167600980505     C     CLRSCV        BEGSR
167700980505     C*
167800980505     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE
167900980505     C                   CLEAR                   VSGL                           *CAMPI
168000980505     C                   CLEAR                   VPSC
168100980505     C                   CLEAR                   VCSC
168200980505     C                   CLEAR                   VVSC
168300980505     C                   CLEAR                   VINL
168400980505     C                   CLEAR                   VRPV
168500980505     C                   CLEAR                   VSDE
168600980505     C                   CLEAR                   VPDE
168700980505     C                   CLEAR                   VPPE
168800980505     C                   CLEAR                   VAST
168900980505     C*
169000980505     C                   ENDSR
169100970117     C*------------------------------------------------------------------------*
169200970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
169300970117     C*------------------------------------------------------------------------*
169400970117     C     CARTAB        BEGSR
169500970117     C*--------------------
169600970117     C* TIPI SERVIZIO
169700970117     C*--------------------
169800970117     C                   Z-ADD     *ZEROS        I                 4 0
169900020906     C                   Z-ADD     1             KTBKUT
170000970117     C                   MOVEL     '5E'          KTBCOD
170100970117     C     KE2TAB        SETLL     TABEL00F
170200970117     C     KE2TAB        READE     TABEL00F                               99
170300970117DO  1C     *IN99         DOWEQ     *OFF
170400970117IF  2C     TBLFLG        IFEQ      *BLANKS
170500970117     C                   ADD       1             I
170600970117     C                   MOVEL     TBLUNI        DS5E
170700970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
170800970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
170900970117E   2C                   ENDIF
171000970117     C     KE2TAB        READE     TABEL00F                               99
171100970117E   1C                   ENDDO
171200970117     C*--------------------
171300970117     C* TIPI TARIFFA
171400970117     C*--------------------
171500970117     C                   Z-ADD     *ZEROS        I                 4 0
171600020906     C                   Z-ADD     1             KTBKUT
171700970117     C                   MOVEL     'TR'          KTBCOD
171800970117     C     KE2TAB        SETLL     TABEL00F
171900970117     C     KE2TAB        READE     TABEL00F                               99
172000970117DO  1C     *IN99         DOWEQ     *OFF
172100970117IF  2C     TBLFLG        IFEQ      *BLANKS
172200970117     C                   MOVEL     TBLUNI        DSTR
172300970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
172400970117     C                   ADD       1             I
172500970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
172600970117     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
172700970117E   3C                   ENDIF
172800970117E   2C                   ENDIF
172900970117     C     KE2TAB        READE     TABEL00F                               99
173000970117E   1C                   ENDDO
173100970117     C*--------------------
173200970117     C* TARIFFA DI CARTELLO
173300970117     C*--------------------
173400970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
173500970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
173600970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
173700160229     c* il trulc7r restituisce anche il progressivo per cui non serve leggere le tariffe
173800021202     C                   Z-ADD     *ZEROS        CARKSC
173900021202     C                   Z-ADD     *ZEROS        CARCTR
174000021202     C                   Z-ADD     *ZEROS        CARPRG
174100160229     C                   CLEAR                   TRULc7DS
174200160229     C                   EVAL      Ic7TLA = 'L'
174300160229     c                   eval      ic7tfie='I'
174400160229     C                   EVAL      Ic7DTA = DATCOR
174500160229     C                   CALL      'TRULC7R'
174600160229     C                   PARM                    TRULC7DS
174700021202     C*
174800160229IF  1C                   IF        Oc7ERR <> 'E'
174900160229     C                   Z-ADD     oc7kscc       CARKSC
175000160229     C                   Z-ADD     oc7ctrc       CARCTR
175100160229     C                   movel     oc7prgc       CARprg
175200160229     c                   endif
175300160229
175400160229     c***
175500160229     C***                Z-ADD     O27KSC        KTMKSC
175600160229IF  2C***                IF        %subst(d00op0:1 :1)='O'
175700160229     C***  KTMKSC        SETLL     TNOFM01L
175800160229     C***  KTMKSC        READE     TNOFM01L                               99
175900160229X   2C***                ELSE
176000160229     C***  KTMKSC        SETLL     TNTAM01L
176100160229     C***  KTMKSC        READE     TNTAM01L                               99
176200160229E   2C***                ENDIF
176300160229DO  2C***  *IN99         DOWEQ     *OFF
176400160229IF  3C***  TAMATB        IFNE      'A'                                          *NO ANNULLATE
176500160229     C***  TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
176600160229IF  4C***  DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
176700160229     C***  DATCOR        ANDLE     TAMDST
176800160229     C***                Z-ADD     TAMKSC        CARKSC
176900160229     C***                Z-ADD     TAMCTR        CARCTR
177000160229     C***                Z-ADD     TAMPRG        CARPRG
177100160229     C***                LEAVE
177200160229E   4C***                ENDIF
177300160229E   3C***                ENDIF
177400160229IF  3C***                IF        %subst(d00op0: 1: 1)= 'O'
177500160229     C***  KTMKSC        READE     TNOFM01L                               99
177600160229X   3C***                ELSE
177700160229     C***  KTMKSC        READE     TNTAM01L                               99
177800160229E   3C***                ENDIF
177900160229E   2C***                ENDDO
178000160229E   1C***                ENDIF
178100970117     C*
178200970117     C                   ENDSR
178300970117     C*------------------------------------------------------------------------*
178400970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
178500970117     C*------------------------------------------------------------------------*
178600970117     C     CARCC         BEGSR
178700970117     C*
178800970117     C* RECUPERA I DATI DELL'UTENTE
178900020906     C                   Z-ADD     1             CODUT
179000020830     C                   CALL      'X§PARUT'
179100970117     C                   PARM                    PARUT
179200970117     C*
179300970117     C                   Z-ADD     1             Z
179400970117DO  1C                   DO        50            X2                2 0
179500970117     C                   MOVE      TCU(X2)       CARCNT
179600970117IF  2C     TIPCNT        IFEQ      'CI'
179700970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
179800970117E   2C                   ENDIF
179900970117IF  2C     FL34CN        IFEQ      '01'
180000970117     C     TIPCNT        ANDEQ     'CG'
180100970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
180200970117E   2C                   ENDIF
180300970117IF  2C     FL34CN        IFEQ      '02'
180400970117     C     TIPCNT        ANDEQ     'CG'
180500970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
180600970117E   2C                   ENDIF
180700970117IF  2C     TIPCNT        IFEQ      'RI'
180800970117     C                   Z-ADD     KCU(X2)       RI(Z)
180900970117     C                   ADD       1             Z                 3 0
181000970117E   2C                   ENDIF
181100970117E   1C                   ENDDO
181200970117     C*
181300970117     C                   ENDSR
181400970117     C*------------------------------------------------------------------------*
181500970117     C* *INZSR - OPERAZIONI INIZIALI
181600970117     C*------------------------------------------------------------------------*
181700970117     C     *INZSR        BEGSR
181800970117     C*--------------------
181900970117     C* RICEVIMENTO PARAMETRI
182000970117     C*--------------------
182100970117     C     *ENTRY        PLIST
182200970117     C                   PARM                    KPJBA
182300970117     C                   PARM                    DSTE00
182400060829     c                   movel     kpjbu         param
182500060829     c                   if        param = ' '
182600060829     c                   eval      dectes = '  RICAVI  '
182700060829     c                   eval      decf07 = 'COMPETENZE'
182800060829     c                   else
182900060829     c                   eval      decf07 = '  RICAVI  '
183000060829     c                   eval      dectes = 'COMPETENZE'
183100060829     c                   end
183200041202     c* apertura controllata di alcuni file xchè solo di filiale
183300041202     c                   open      tfaco00f                             95
183400041202     c                   open      tnofm01l                             95
183500970117     C*--------------------
183600970117     C* VARIABILI RIFERITE AL DATABASE
183700970117     C*--------------------
183800970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
183900970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
184000970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
184100970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
184200970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
184300970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
184400970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
184500970117     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
184600990818     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
184700970117     C     *LIKE         DEFINE    TADCTR        KTDCTR
184800970117     C     *LIKE         DEFINE    TADPRG        KTDPRG
184900970117     C     *LIKE         DEFINE    TADLNP        KTDLNP
185000011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
185100980505     C     *LIKE         DEFINE    ETCCTO        KTCCTO
185200970117     C     *LIKE         DEFINE    ETCKSC        KTCKSC
185300970117     C     *LIKE         DEFINE    ETCCTR        KTCCTR
185400970117     C     *LIKE         DEFINE    ETCPRG        KTCPRG
185500970117     C     *LIKE         DEFINE    ETCLNP        KTCLNP
185600011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
185700980505     C     *LIKE         DEFINE    ETSCTO        KTSCTO
185800970117     C     *LIKE         DEFINE    ETSKSC        KTSKSC
185900970117     C     *LIKE         DEFINE    ETSCTR        KTSCTR
186000970117     C     *LIKE         DEFINE    ETSPRG        KTSPRG
186100970117     C     *LIKE         DEFINE    ETSLNP        KTSLNP
186200970117     C     *LIKE         DEFINE    ETSSGL        KTSSGL
186300970117     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
186400970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
186500970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
186600011026     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
186700011026     C     *LIKE         DEFINE    TAMCTR        KTMCTR
186800011026     C     *LIKE         DEFINE    TAMPRG        KTMPRG
186900970117     C*--------------------
187000970117     C* CHIAVI DI LETTURA
187100970117     C*--------------------
187200970117     C* LETTURA TABEL00F - COMPLETA
187300970117     C     KEYTAB        KLIST
187400970117     C                   KFLD                    KTBKUT                         *UTENTE
187500970117     C                   KFLD                    KTBCOD                         *TABELLA
187600970117     C                   KFLD                    KTBKEY                         *CODICE
187700970117     C* LETTURA TABEL00F - PARZIALE
187800970117     C     KE2TAB        KLIST
187900970117     C                   KFLD                    KTBKUT                         *UTENTE
188000970117     C                   KFLD                    KTBCOD                         *TABELLA
188100970117     C* LETTURA AZORG01L - COMPLETA
188200970117     C     KEYORG        KLIST
188300970117     C                   KFLD                    KORFIL                         *FILIALE
188400970117     C* LETTURA CNACO01L - COMPLETA
188500970117     C     KEYACO        KLIST
188600970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
188700970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
188800970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
188900990818     C* LETTURA TITAD04L - PARZIALE
189000970117     C     KEYTAD        KLIST
189100970117     C                   KFLD                    KTDKSC                         *CLIENTE
189200970117     C                   KFLD                    KTDCTR                         *TARIFFA
189300970117     C                   KFLD                    KTDPRG                         *PROGRESSIVO
189400970117     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
189500011001     C* LETTURA TEETC01L - COMPLETA
189600970117     C     KEYETC        KLIST
189700980505     C                   KFLD                    KTCDSF                         *DATI SEDE  O FILIAL
189800980505     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
189900970117     C                   KFLD                    KTCKSC                         *CLIENTE
190000970117     C                   KFLD                    KTCCTR                         *TARIFFA
190100970117     C                   KFLD                    KTCPRG                         *PROGRESSIVO
190200970117     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
190300011001     C* LETTURA TEETS01L - COMPLETA
190400980505     C     KEYETS        KLIST
190500980505     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
190600980505     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
190700980505     C                   KFLD                    KTSKSC                         *CLIENTE
190800980505     C                   KFLD                    KTSCTR                         *TARIFFA
190900980505     C                   KFLD                    KTSPRG                         *PROGRESSIVO
191000980505     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
191100980505     C                   KFLD                    KTSSGL                         *SCAGLIONE
191200011001     C* LETTURA TEETS01L - PARZIALE
191300970117     C     KE2ETS        KLIST
191400980505     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
191500980505     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
191600970117     C                   KFLD                    KTSKSC                         *CLIENTE
191700970117     C                   KFLD                    KTSCTR                         *TARIFFA
191800970117     C                   KFLD                    KTSPRG                         *PROGRESSIVO
191900970117     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
192000011026     C* LETTURA TNTAM01L - COMPLETA
192100011026     C     KEYTAM        KLIST
192200011026     C                   KFLD                    KTMKSC                         *CLIENTE
192300011026     C                   KFLD                    KTMCTR                         *TARIFFA
192400011026     C                   KFLD                    KTMPRG                         *PROGRESSIVO
192500970117     C*--------------------
192600970117     C* VARIABILI NON RIFERITE AL DATABASE
192700970117     C*--------------------
192800970117     C     *LIKE         DEFINE    NRR           SFLPAG                         *DI DEPOSITO
192900970117     C     *LIKE         DEFINE    NRR           SFLEOF
193000970318     C     *LIKE         DEFINE    NRR           DEPNRR
193100970117     C     *LIKE         DEFINE    V1SPSC        DUPPSC                         *DI DUPLICA
193200970117     C     *LIKE         DEFINE    V1SCSC        DUPCSC
193300970117     C     *LIKE         DEFINE    V1SVSC        DUPVSC
193400970317     C     *LIKE         DEFINE    V1SINL        DUPINL
193500970317     C     *LIKE         DEFINE    V1SRPV        DUPRPV
193600970402     C     *LIKE         DEFINE    V1SSDE        DUPSDE
193700970402     C     *LIKE         DEFINE    V1SPDE        DUPPDE
193800970117     C     *LIKE         DEFINE    V1SPPE        DUPPPE
193900970117     C*--------------------
194000970117     C* VARIABILI DI CONTROLLO
194100970117     C*--------------------
194200970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
194300970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
194400970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
194500970117     C                   MOVEL     '0'           WERR              1            *ERRORE
194600970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
194700970117     C                   Z-ADD     10            SFLPAG                         *RECORD X PAGINA
194800970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
194900970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
195000970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
195100970619     C                   MOVEL     'N'           WEETS             1            *ESISTENZA SGL
195200970117     C*
195300970117     C* REPERISCE LA DATA CORRENTE
195400970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
195500970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
195600970117     C                   Z-ADD     WN8           G08DAT
195700970117     C                   Z-ADD     *ZEROS        G08INV
195800970117     C                   MOVEL     '0'           G08ERR
195900970117     C                   CALL      'XSRDA8'
196000970117     C                   PARM                    WLBDA8
196100970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
196200970117     C*
196300970117     C* 02->GESTIONE, 05->INTERROGAZIONE
196400970117     C                   MOVE      D00OP0        WA2               2
196500970117IF  1C     WA2           IFEQ      '02'
196600970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
196700970117X   1C                   ELSE
196800970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
196900970117E   1C                   ENDIF
197000970117     C*
197100970117     C* CARICA LE TABELLE OCCORRENTI
197200970117     C                   EXSR      CARTAB
197300970117     C*
197400970117     C* CARICA I CAPICONTO DI CONTABILITA'
197500970117     C                   EXSR      CARCC
197600980505     C*
197700980505     C* CONTROLLI INIZIALI DI ENTRATA
197800980505     C                   EXSR      CTRENT
197900980505     C*
198000980505     C* CARICA GLI SCAGLIONI NELLE SCHIERE VIDEO
198100980505IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
198200980505     C                   EXSR      CARSGL
198300980505E   1C                   ENDIF
198400970117     C*
198500970117     C* CARICA GLI SCAGLIONI DEL DETTAGLIO TARIFFA
198600980505IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
198700980505IF  2C     D00TLA        IFEQ      'D'                                          *DELTA
198800970117     C                   EXSR      CARTAD
198900980505E   2C                   ENDIF
199000980505E   1C                   ENDIF
199100970117     C*
199200970117     C* IMPOSTA LA TESTATA
199300980505IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
199400970319     C                   EXSR      IMPVD0
199500980505E   1C                   ENDIF
199600970117     C*
199700970117     C                   ENDSR
199800970117     O*------------------------------------------------------------------------*
199900980505** -DDEC-
200000970117ITALIA              1
200100970117ESTERO              2
200200970117COLLI               3
200300970117 KG                 4
200400980505SEDE                5
200500980505FILIALE             6
2006009805057
2007009805057
2008009805059
200900980818TUTTE LE FILIALI    10
201000970117** -DVID-
201100970414A   Scaglione      Peso                                                       1
201200970414      XXXXX        XXXX                                                   Inc 2
2013009701173
2014009701174
2015009701175
201600970117** -DOPE-
201700970117* INSERIMENTO *1
201800970117* VARIAZIONE * 2
201900970117INTERROGAZIONE 3
202000970117 * GESTIONE *  4
2021009701175
202200980505** -MSG-
202300970117TNTE02R- Immettere lo scaglione                                               1
202400970117TNTE02R- Indicare il peso medio per spedizione                                2
202500980928TNTE02R- Indicare i colli medi per spedizione (minimo 1)                      3
202600970117TNTE02R- Indicare il volume medio per spedizione                              4
202700970117TNTE02R- Indicare l'incidenza % dello scaglione rispetto al totale spedizioni 5
202800970117TNTE02R- Scaglione già esistente                                              6
202900970117TNTE02R- Immettere almeno uno scaglione                                       7
203000970117TNTE02R- La % d'incidenza di tutti gli scaglioni deve essere uguale a 100     8
203100970117TNTE02R- Immettere 'A' per cancellare lo scaglione o lasciare vuoto           9
203200970117TNTE02R- Immettere gli scaglioni uguali a quelli della tariffa o viceversa    10
203300970117TNTE02R- Filiale errata                                                       11
203400091123TNTE02R- Il valore scelto non è corretto                                      12
203500970117TNTE02R- Dati tipo richiesti non trovati                                      13
203600970117TNTE02R- Non esistono i dati tipo per la tariffa/visita richiesta             14
203700970317TNTE02R- Non esistono spedizioni in provincia. Non inserire l'inoltro         15
203800970318TNTE02R- Il valore non può essere maggiore dello scaglione                    16
203900970318TNTE02R- Il valore non può essere minore dello scaglione precedente           17
204000970414TNTE02R- Segno delta richiesto errato ( /+/-).                                18
204100970414TNTE02R- Percentuale delta richiesto errata.                                  19
204200971008TNTE02R- Il peso dello scaglione non può superare i 9.999 kg                  20
204300980505TNTE02R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    21
204400980909TNTE02R- Scaglioni incongruenti con RPV.                                      22
204500011008TNTE02R- Impossibile inserire importi con decimali quando la divisa è ITL.    23
204600060825Il codice tariffa scelto incongruente con quello originale                    24
204700980505TNTE02R- 25
204800980505TNTE02R- 26
204900980505TNTE02R- 27
205000980505TNTE02R- 28
205100980505TNTE02R- 29
205200980505TNTE02R- 30
