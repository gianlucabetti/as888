000100970117     H*------------------------------------------------------------------------*
000200970117     H* TARIFFE DA CONTO ECONOMICO - RICHIESTA SCAGLIONI                       *
000300970117     H*------------------------------------------------------------------------*
000400970117     H DECEDIT('0,') DATEDIT(*DMY.)
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
008600970117     D*-------------------
008700970117     D* DS TIPI SERVIZIO
008800970117     D*-------------------
008900970117     D DS5E          E DS
009000970117     D*-------------------
009100970117     D* DS TIPI TARIFFA
009200970117     D*-------------------
009300970117     D DSTR          E DS
009400021202     D*-------------------
009500021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
009600021202     D*-------------------
009700021202     D TRUL27DS      E DS
009800970117     D*-------------------
009900970117     D* DS PASSAGGIO PARAMETRI
010000970117     D*-------------------
010100970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
010200980504     D*------------
010300980504     D* DS RICERCA DATI TIPO
010400980504     D*------------
010500980504     D DSTE30        E DS                  EXTNAME(TNTE30DS)
010600970117     D*------------------
010700970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
010800970117     D*------------------
010900970117     D WLBDA8          DS
011000970117     D  G08DAT                 1      8  0
011100970117     D  G08INV                 9     16  0
011200970117     D  G08ERR                17     17
011300970117     D  G08TGI                18     22  0
011400970117     D*------------------
011500020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
011600970117     D*------------------
011700970117     D CARCNT          DS
011800970117     D  FL12CN                 1      2
011900970117     D  FL34CN                 3      4
012000970117     D  TIPCNT                 5      6
012100020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
012200970117     D  FLU                   66     97
012300970117     D                                     DIM(32)                              FLAG PAR.UTENT
012400970117     D  TCU                  398    697
012500970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
012600970117     D  KCU                  698    847P 0
012700970117     D                                     DIM(50)                              *TAB CAPICONTO
012800970117     D                                     PACKEVEN
012900970117     D TCUDS           DS
013000970117     D  E1                     1      1
013100970117     D  E3                     3      3
013200970117     D  E2                     2      2
013300970117     D  E4                     4      4
013400970117     D  E56                    5      6
013500970117     D*------------------
013600970117     D* DS SCOMPOSIZIONE DECODIFICA VIDEO
013700970117     D*------------------
013800970117     D* DVID,2
013900970117     D                 DS
014000970414     D  D02SGL                 7     11
014100970414     D  D02PSC                20     23
014200970117     D  DVID02                 1     78
014300970117     D*-------------------
014400970117     D* ARCHITETTURA
014500970117     D*-------------------
014600970117     D KPJBA         E DS
014700011026     D*------------------
014800011026     D* DS REPERIMENTO DIVISA TARIFFE
014900011026     D*------------------
015000011026     D DSTA01        E DS
015100011008     D*------------------
015200011008     D* VARIABILI DI WRK
015300011008     D*------------------
015400011008     D  NUMDEC         S              9  9 INZ(*zeros)
015500970117     C*------------------------------------------------------------------------*
015600970117     C* MAIN LINE
015700970117     C*------------------------------------------------------------------------*
015800970117     C*
015900970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
016000970117IF  1C     $FINE         IFNE      'S'
016100970117     C*
016200970117     C* IMPOSTAZIONI PRIMA VIDEATA
016300970117     C                   EXSR      IMPSFL
016400970117     C*
016500970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
016600970117DO  2C     $FINE         DOUEQ     'S'
016700970117     C     TIPVID        CASEQ     '1'           GESSFL                         *1° VIDEO
016800970117     C                   ENDCS
016900970117E   2C                   ENDDO
017000970117E   1C                   ENDIF
017100970117     C*
017200970117     C                   SETON                                        LR
017300970117     C*------------------------------------------------------------------------*
017400970117     C* IMPSFL - IMPOSTAZIONI INIZIALI SUBFILE
017500970117     C*------------------------------------------------------------------------*
017600970117     C     IMPSFL        BEGSR
017700970117     C*
017800970117     C* SE SCAGLIONI INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
017900980505     C                   MOVEL     D00DSF        KTSDSF                         *DATI SEDE O FILIALE
018000980505     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
018100970117     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
018200970117     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
018300970117     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
018400970117     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
018500011001     C     KE2ETS        SETLL     TEETS01L                               99
018600970619IF  1C     *IN99         IFEQ      *OFF
018700970117     C                   WRITE     TE02D00                                      *TESTATA
018800970117     C                   WRITE     TE02D01                                      *MESSAGGIO SF VUOTO
018900970117     C                   EXSR      F05GES
019000970117E   1C                   ENDIF
019100970117     C*
019200970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
019300970117     C                   EXSR      SETSFL
019400970117     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
019500970117     C* CARICA IL SUBFILE
019600970117     C                   EXSR      POSSFL
019700970117     C*
019800970117     C                   ENDSR
019900970117     C*------------------------------------------------------------------------*
020000970117     C* SETSFL - IMPOSTAZIONI INDICATORI SUBFILE
020100970117     C*------------------------------------------------------------------------*
020200970117     C     SETSFL        BEGSR
020300970117     C*
020400970117     C                   SETOFF                                       19         ERRORI CONTROL
020500970117     C                   SETOFF                                       202122     ERRORI SFILE
020600970317     C                   SETOFF                                       232425
020700970402     C                   SETOFF                                       262728
020800970402     C                   SETOFF                                       29
020900970117     C                   SETOFF                                       90         GENERICO DI ERRORE
021000970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
021100970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
021200970506     C                   SETON                                        02        *ND ALCUNI CAMPI
021300970506X   1C                   ELSE                                                   *CALCOLO DELTA
021400970506     C                   SETOFF                                       02        *NESSUNA ND
021500970506E   1C                   ENDIF
021600970117     C*
021700970117     C                   ENDSR
021800970117     C*------------------------------------------------------------------------*
021900970117     C* GESSFL - GESTIONE SUBFILE
022000970117     C*------------------------------------------------------------------------*
022100970117     C     GESSFL        BEGSR
022200970117     C*
022300970117     C* EMISSIONE VIDEATA
022400970117IF  1C     $TESVD        IFEQ      '1'
022500970117     C                   WRITE     TE02D00                                      *TESTATA
022600970117     C                   MOVEL     '0'           $TESVD
022700970117E   1C                   ENDIF
022800970117     C                   WRITE     TE02Z01                                      *PIEDI
022900970117     C                   EXFMT     TE02C01                                      *SFILE
023000970117     C                   EXSR      SETSFL
023100970117     C* F03-FINE
023200970117IF  1C     *INKC         IFEQ      *ON
023300970117     C                   EXSR      F03GES
023400970117     C                   GOTO      FINSFL
023500970117E   1C                   ENDIF
023600970117     C* F12-RITORNO
023700970117IF  1C     *INKL         IFEQ      *ON
023800970117     C                   EXSR      F12GES
023900970117     C                   GOTO      FINSFL
024000970117E   1C                   ENDIF
024100970117     C* F05-CARICA DA ALTRA TARIFFA
024200970117IF  1C     *INKE         IFEQ      *ON
024300970117     C                   EXSR      F05GES
024400970117     C                   GOTO      FINSFL
024500970117E   1C                   ENDIF
024600980507     C* CONTROLLA VIDEO
024700970117     C                   EXSR      CTRSFL
024800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
024900970117     C                   GOTO      FINSFL
025000970117E   1C                   ENDIF
025100980507     C*
025200980507     C                   EXSR      CARSCV                                       *MEMORIZZA SCHIERE
025300980507     C                   EXSR      POSSFL                                       *RICARICA SFILE
025400980507     C*
025500970117     C* F06-AGGIORNA E PROSEGUE
025600970117IF  1C     *INKF         IFEQ      *ON
025700970117     C                   EXSR      F06GES
025800970117E   1C                   ENDIF
025900970117     C*
026000970117     C     FINSFL        ENDSR
026100970117     C*------------------------------------------------------------------------*
026200980505     C* CTRSFL - CONTROLLA SUBFILE 1^ PARTE
026300970117     C*------------------------------------------------------------------------*
026400970117     C     CTRSFL        BEGSR
026500970117     C*
026600980505     C* REIMPOSTA LE VARIABILI DI LAVORO
026700970117     C                   MOVEL     '0'           WERR              1            *ERRORE
026800980505     C                   MOVEL     '0'           WESGL             1            *ESISTENZA SCAGLIONI
026900980505     C                   Z-ADD     *ZEROS        V1CPPE                         *TOTALE %INCIDENZA
027000970117     C*
027100970117     C* LEGGE TUTTI I RECORD DEL SFILE
027200970117     C                   Z-ADD     1             NRR
027300970117     C     NRR           CHAIN     TE02S01                            98
027400970117DO  1C     *IN98         DOWEQ     *OFF
027500980505     C*
027600980505     C* CAMPI DI DUPLICA -> CAMPI SFILE
027700970117     C   13              Z-ADD     DUPPSC        V1SPSC                         *DI DUPLICA
027800970117     C   11              Z-ADD     DUPCSC        V1SCSC
027900970117     C   12              Z-ADD     DUPVSC        V1SVSC
028000970317     C   15              Z-ADD     DUPINL        V1SINL
028100970317     C   16              Z-ADD     DUPRPV        V1SRPV
028200970402     C   17              MOVEL     DUPSDE        V1SSDE
028300970402     C   18              Z-ADD     DUPPDE        V1SPDE
028400970317     C   14              Z-ADD     DUPPPE        V1SPPE
028500970117     C                   SETOFF                                       80        *NO SFLNXTCHG
028600980505     C*
028700980505     C* CONTROLLA CAMPI
028800970117IF  2C     V1SSGL        IFGT      *ZEROS                                       *IMMESSO QC.
028900970117     C     V1SPSC        ORGT      *ZEROS
029000970117     C     V1SCSC        ORGT      *ZEROS
029100970117     C     V1SVSC        ORGT      *ZEROS
029200970317     C     V1SINL        ORGT      *ZEROS
029300970317     C     V1SRPV        ORGT      *ZEROS
029400970414     C     V1SSDE        ORNE      *BLANKS
029500970402     C     V1SPDE        ORGT      *ZEROS
029600970117     C     V1SPPE        ORGT      *ZEROS
029700970117     C*--------------------
029800970117     C* ANNULLAMENTO
029900970117     C*--------------------
030000970117IF  3C     WERR          IFEQ      '0'
030100970117IF  4C     V1SATB        IFNE      'A'
030200970117     C     V1SATB        ANDNE     *BLANKS
030300970117     C                   SETON                                        2090
030400970117     C                   MOVEL     MSG(9)        $MSG
030500970117     C                   MOVEL     '1'           WERR
030600970117E   4C                   ENDIF
030700970117E   3C                   ENDIF
030800980505     C*
030900980505     C* SE IL RECORD NON E' DA ANNULLARE
031000980505IF  3C     V1SATB        IFEQ      *BLANKS
031100980505     C                   MOVEL     '1'           WESGL                          *ESISTENZA SCAGLIONI
031200970117     C*--------------------
031300970117     C* SCAGLIONE
031400970117     C*--------------------
031500980505IF  4C     WERR          IFEQ      '0'
031600980505IF  5C     V1SSGL        IFEQ      *ZEROS
031700970117     C                   SETON                                        2190
031800970117     C                   MOVEL     MSG(1)        $MSG
031900970117     C                   MOVEL     '1'           WERR
032000980505E   5C                   ENDIF
032100980505E   4C                   ENDIF
032200970117     C*--------------------
032300970117     C* PESO MEDIO
032400970117     C*--------------------
032500980505IF  4C     WERR          IFEQ      '0'
032600980505IF  5C     V1SPSC        IFEQ      *ZEROS
032700970117     C                   SETON                                        2290
032800970117     C                   MOVEL     MSG(2)        $MSG
032900970117     C                   MOVEL     '1'           WERR
033000980505E   5C                   ENDIF
033100980505E   4C                   ENDIF
033200980505     C* PESO MASSIMO
033300980505IF  4C     WERR          IFEQ      '0'
033400000621IF  5C     V1SPSC        IFGT      9999
033500971008     C                   SETON                                        2290
033600971008     C                   MOVEL     MSG(20)       $MSG
033700971008     C                   MOVEL     '1'           WERR
033800980505E   5C                   ENDIF
033900980505E   4C                   ENDIF
034000970117     C*--------------------
034100970117     C* COLLI MEDI
034200970117     C*--------------------
034300981030     C     *LIKE         DEFINE    ETSCSC        DEPCSC
034400980505IF  4C     WERR          IFEQ      '0'
034500980505IF  5C     V1SCSC        IFEQ      *ZEROS
034600970117     C     V1SPSC        DIV       V1CPMC        V1SCSC
034700981030     C                   Z-ADD     V1SCSC        DEPCSC
034800980928IF  6C***********V1SCSC    IFEQ *ZEROS
034900980928IF  6C     V1SCSC        IFLT      1
035000970117     C                   Z-ADD     1             V1SCSC
035100980505E   6C                   ENDIF
035200991105X   5C                   ELSE
035300991105     C                   Z-ADD     V1SCSC        DEPCSC
035400980505E   5C                   ENDIF
035500980928IF  5C***********V1SCSC    IFEQ *ZEROS
035600980928IF  5C     V1SCSC        IFLT      1
035700970117     C                   SETON                                        2390
035800970117     C                   MOVEL     MSG(3)        $MSG
035900970117     C                   MOVEL     '1'           WERR
036000980505E   5C                   ENDIF
036100980505E   4C                   ENDIF
036200970117     C*--------------------
036300970117     C* VOLUME MEDIO
036400970117     C*--------------------
036500980505IF  4C     WERR          IFEQ      '0'
036600980505IF  5C     V1SVSC        IFEQ      *ZEROS
036700981030     C*---       V1CVMC    MULT V1SCSC    V1SVSC
036800981030     C     V1CVMC        MULT      DEPCSC        V1SVSC
036900980505E   5C                   ENDIF
037000980505IF  5C     V1SVSC        IFEQ      *ZEROS
037100970117     C                   Z-ADD     0,001         V1SVSC
037200980505E   5C                   ENDIF
037300980505IF  5C     V1SVSC        IFEQ      *ZEROS
037400970117     C                   SETON                                        2490
037500970117     C                   MOVEL     MSG(4)        $MSG
037600970117     C                   MOVEL     '1'           WERR
037700980505E   5C                   ENDIF
037800980505E   4C                   ENDIF
037900970513     C*--------------------
038000970513     C* RPV PER SCAGLIONE
038100970513     C*--------------------
038200980505IF  4C     WERR          IFEQ      '0'
038300011009     C***                MOVEL     *BLANKS       V1SAST                         *NO INDICAZIONE "*"
038400011009     C                   SETOFF                                       70
038500970513     C                   EXSR      CHKAST
038600980505E   4C                   ENDIF
038700970317     C*--------------------
038800970317     C* INOLTRO
038900970317     C*--------------------
039000980505IF  4C     WERR          IFEQ      '0'
039100980505IF  5C     ETCPPV        IFEQ      *ZEROS
039200970317     C     V1SINL        ANDGT     *ZEROS
039300970317     C                   SETON                                        2590
039400970317     C                   MOVEL     MSG(15)       $MSG
039500970317     C                   MOVEL     '1'           WERR
039600980505E   5C                   ENDIF
039700980505E   4C                   ENDIF
039800011008     C*
039900011008     C     WERR          IFEQ      '0'
040000011008     C     V1SINL        IFGT      *ZEROS
040100011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
040200011008     C     D00DIV        IFEQ      'ITL'
040300011008     C                   Z-ADD     V1SINL        NUMDEC
040400011008     C     NUMDEC        IFGT      *ZEROS
040500011008     C                   SETON                                        2590
040600011008     C                   MOVEL     MSG(23)       $MSG
040700011008     C                   MOVEL     '1'           WERR
040800011008     C                   ENDIF
040900011008     C                   ENDIF
041000011008     C                   ENDIF
041100011008     C                   ENDIF
041200011008     C*
041300970402     C*--------------------
041400970402     C* SEGNO E % SCOSTAMENTO DELTA
041500970402     C*--------------------
041600980505IF  4C     WERR          IFEQ      '0'
041700980505IF  5C     V1SSDE        IFNE      *BLANKS                                      *SEGNO DELTA
041800970402     C     V1SSDE        ANDNE     '+'
041900970402     C     V1SSDE        ANDNE     '-'
042000970402     C                   SETON                                        2890
042100970402     C                   MOVEL     MSG(18)       $MSG
042200970402     C                   MOVEL     '1'           WERR
042300980505E   5C                   ENDIF
042400980505E   4C                   ENDIF
042500980505IF  4C     WERR          IFEQ      '0'
042600980505IF  5C     V1SSDE        IFNE      *BLANKS                                      *% DELTA
042700970402     C     V1SPDE        ANDEQ     *ZEROS
042800970402     C     V1SPDE        ORGT      *ZEROS
042900970402     C     V1SSDE        ANDEQ     *BLANKS
043000970402     C                   SETON                                        2990
043100970402     C                   MOVEL     MSG(19)       $MSG
043200970402     C                   MOVEL     '1'           WERR
043300980505E   5C                   ENDIF
043400980505E   4C                   ENDIF
043500970117     C*--------------------
043600970117     C* % INCIDENZA
043700970117     C*--------------------
043800980505IF  4C     WERR          IFEQ      '0'
043900980505IF  5C     V1SPPE        IFEQ      *ZEROS
044000970117     C                   SETON                                        2690
044100970117     C                   MOVEL     MSG(5)        $MSG
044200970117     C                   MOVEL     '1'           WERR
044300980505E   5C                   ENDIF
044400980505E   4C                   ENDIF
044500980505     C*--------------------
044600980505     C* RANGE SCAGLIONE
044700980505     C*--------------------
044800980505     C* VALORE MASSIMO -> MINORE DELLO SCAGLIONE
044900980505IF  4C     WERR          IFEQ      '0'
045000980507     C                   MOVEL     D00CTR        A1                1
045100980507IF  5C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
045200980505     C                   Z-ADD     V1SCSC        W133             13 3
045300980505X   5C                   ELSE                                                   *TARIFFA A QLI/KG
045400980505     C                   Z-ADD     V1SPSC        W133             13 3
045500980505E   5C                   ENDIF
045600980505IF  5C     W133          IFGT      V1SSGL
045700980507IF  6C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
045800980507     C                   SETON                                        23
045900980507X   6C                   ELSE                                                   *TARIFFA A QLI/KG
046000980507     C                   SETON                                        22
046100980507E   6C                   ENDIF
046200980507     C                   SETON                                        90
046300980505     C                   MOVEL     MSG(16)       $MSG
046400980505     C                   MOVEL     '1'           WERR
046500980505E   5C                   ENDIF
046600980505E   4C                   ENDIF
046700980505     C*
046800980505     C* CALCOLO %INCIDENZA TOTALE
046900980505IF  4C     V1SATB        IFEQ      *BLANKS                                      *NO ANNULLATI
047000980505     C     V1CPPE        ADD       V1SPPE        V1CPPE                         *TOTALE PESI %
047100980505E   4C                   ENDIF
047200980505     C*
047300980505     C* AGGIORNA RIGA SFILE
047400980505     C                   Z-ADD     NRR           $NRR                           *POSIZIONAMENTO
047500980505     C                   EXSR      AGGSFL                                       *AGGIORNA RIGA
047600980505IF  4C     WERR          IFEQ      *ON                                          * E R R O R E
047700980505     C                   GOTO      FINCS1                                       *ESCE
047800980505X   4C                   ELSE                                                   *N O N  ERRORE
047900980505     C                   Z-ADD     V1SPSC        DUPPSC                         *DI DUPLICA
048000980505     C                   Z-ADD     V1SCSC        DUPCSC
048100980505     C                   Z-ADD     V1SVSC        DUPVSC
048200980505     C                   Z-ADD     V1SINL        DUPINL
048300980505     C                   Z-ADD     V1SRPV        DUPRPV
048400980505     C                   MOVEL     V1SSDE        DUPSDE
048500980505     C                   Z-ADD     V1SPDE        DUPPDE
048600980505     C                   Z-ADD     V1SPPE        DUPPPE
048700980505E   4C                   ENDIF
048800980505E   3C                   ENDIF
048900980505E   2C                   ENDIF
049000980505     C*
049100980505     C* LETTURA SUCCESSIVA RIGA SFILE
049200980505     C                   ADD       1             NRR                            *LEGGE ALTRO RECORD
049300980505     C     NRR           CHAIN     TE02S01                            98
049400980505E   1C                   ENDDO                                                  *FINE SUBFILE
049500980505     C*---
049600980505     C* ALMENO UNO SCAGLIONE
049700980505     C*---
049800980505IF  1C     WERR          IFEQ      '0'
049900980505IF  2C     WESGL         IFEQ      '0'
050000980505     C                   SETON                                        90
050100980505     C                   MOVEL     MSG(7)        $MSG
050200980505     C                   MOVEL     '1'           WERR
050300980505     C                   Z-ADD     1             $NRR                           *POSIZIONAMENTO
050400980505E   2C                   ENDIF
050500980505E   1C                   ENDIF
050600970117     C*
050700980505     C     FINCS1        ENDSR
050800970513     C*------------------------------------------------------------------------*
050900970513     C* CHKAST - CONTROLLA L'ASTERISCO
051000970513     C*------------------------------------------------------------------------*
051100970513     C     CHKAST        BEGSR
051200011009     C*
051300011009     C********
051400011009     C* La routine è rimasta la stessa ma invece di gestire il campo V1SAST (l'asterisco)
051500011009     C* per questioni grafiche di spazio a video gestisce in sostituzione un indicatore
051600011009     C* legato al campo SCAGLIONE
051700011009     C********
051800970513     C*
051900980507     C                   MOVEL     D00CTR        A1                1
052000980507IF  1C     A1            IFEQ      '0'                                          *TARIFFA A QLI
052100980507     C     A1            OREQ      '3'                                          *TARIFFA A KG
052200970513IF  2C     V1SRPV        IFGT      *ZEROS                                       *IMMESSO RAPPORTO P/
052300970513     C                   Z-ADD     *ZEROS        PESTAS
052400970513     C                   EXSR      CALPT                                        *CALCOLA P.TASSABILE
052500970513IF  3C     PESTAS        IFGT      V1SSGL                                       *PESO > SCAGLIONE
052600011009     C***                MOVEL     '*'           V1SAST                         *SI INDICAZIONE "*"
052700011009     C                   SETON                                        70
052800970513E   3C                   ENDIF
052900970513E   2C                   ENDIF
053000970513E   1C                   ENDIF
053100970513     C*
053200970513     C                   ENDSR
053300970513     C*------------------------------------------------------------------------*
053400970513     C* CALPT - CALCOLA IL PESO TASSABILE
053500970513     C*------------------------------------------------------------------------*
053600970513     C     CALPT         BEGSR
053700970513     C*
053800970513     C* IMPOSTA COME PESO QUELLO DEI DATI TIPO SCAGLIONE
053900970513     C                   Z-ADD     V1SSGL        PESTAS           13 3          *KG O QLI
054000970513     C*
054100970513     C* CALCOLA IL PESO TASSABILE
054200970513     C                   Z-ADD     *ZEROS        COMPRV            9 3
054300970513     C     V1SVSC        MULT      V1SRPV        COMPRV
054400970513     C     COMPRV        MULT      100           PESTAS                         *CALCOLO PESO TASS.
054500970513IF  1C     PESTAS        IFLT      V1SPSC                                       *PESO TASS < REALE
054600970513     C                   Z-ADD     V1SPSC        PESTAS                         *PESO TASS=PESO REAL
054700970513E   1C                   ENDIF
054800970513     C*
054900970513     C                   ENDSR
055000980505     C*------------------------------------------------------------------------*
055100980505     C* CARSCV - MEMORIZZA IL SFILE NELLE SCHIERE VIDEO
055200980505     C*------------------------------------------------------------------------*
055300980505     C     CARSCV        BEGSR
055400980505     C*
055500980505     C* PULISCE LE SCHIERE VIDEO
055600980505     C                   EXSR      CLRSCV
055700980505     C*
055800980505     C* LEGGE TUTTI I RECORD DEL SFILE
055900980505     C                   Z-ADD     1             NRR
056000980505     C     NRR           CHAIN     TE02S01                            98
056100980505DO  1C     *IN98         DOWEQ     *OFF
056200980505IF  2C     V1SATB        IFEQ      *BLANKS                                      *NON ANNULLATI
056300980505IF  3C     V1SSGL        IFGT      *ZEROS                                       *SCAGLIONE INSERITO
056400980505     C                   ADD       1             V
056500980505     C                   Z-ADD     V1SSGL        VSGL(V)
056600980505     C                   Z-ADD     V1SPSC        VPSC(V)                        *PESO MEDIO
056700980505     C                   Z-ADD     V1SCSC        VCSC(V)                        *COLLI MEDI
056800980505     C                   Z-ADD     V1SVSC        VVSC(V)                        *VOLUME MEDIO
056900980505     C                   Z-ADD     V1SINL        VINL(V)                        *INOLTRO
057000980505     C                   Z-ADD     V1SRPV        VRPV(V)                        *RAPPORTO PESO/VOLUM
057100980505     C                   MOVEL     V1SSDE        VSDE(V)                        *SEGNO DELTA
057200980505     C                   Z-ADD     V1SPDE        VPDE(V)                        *% DELTA
057300980505     C                   Z-ADD     V1SPPE        VPPE(V)                        *% PESO
057400011009     C***                MOVEL     V1SAST        VAST(V)                        *ASTERISCO
057500980505E   3C                   ENDIF
057600980505E   2C                   ENDIF
057700980505     C                   ADD       1             NRR                            *LEGGE ALTRO RECORD
057800980505     C     NRR           CHAIN     TE02S01                            98
057900980505E   1C                   ENDDO                                                  *FINE SUBFILE
058000980505     C*
058100980505     C* ORDINA LE SCHIERE VIDEO
058200980505     C                   EXSR      ORDSCH
058300980505     C*
058400980505     C                   ENDSR
058500980505     C*------------------------------------------------------------------------*
058600980505     C* ORDSCH - ORDINA LE SCHIERE VIDEO
058700980505     C*------------------------------------------------------------------------*
058800980505     C     ORDSCH        BEGSR
058900980505     C*
059000980505DO  1C     1             DO        10            J                 4 0          1°  C I C L O
059100980505IF  2C     VSGL(J)       IFGT      *ZEROS
059200980505     C     1             ADD       J             K                 4 0
059300980505DO  3C     K             DO        10            K                              2°  C I C L O
059400980505IF  4C     VSGL(K)       IFGT      *ZEROS
059500980505IF  5C     VSGL(J)       IFGT      VSGL(K)                                      *ORDINAMENTO
059600980507     C* SCAGLIONE
059700980507     C                   Z-ADD     VSGL(J)       N133             13 3
059800980507     C                   Z-ADD     VSGL(K)       VSGL(J)
059900980507     C                   Z-ADD     N133          VSGL(K)
060000980505     C* PESO
060100980505     C                   Z-ADD     VPSC(J)       N71               7 1
060200980505     C                   Z-ADD     VPSC(K)       VPSC(J)
060300980505     C                   Z-ADD     N71           VPSC(K)
060400980505     C* COLLI
060500980505     C                   Z-ADD     VCSC(J)       N71               7 1
060600980505     C                   Z-ADD     VCSC(K)       VCSC(J)
060700980505     C                   Z-ADD     N71           VCSC(K)
060800980505     C* VOLUME
060900980505     C                   Z-ADD     VVSC(J)       N73               7 3
061000980505     C                   Z-ADD     VVSC(K)       VVSC(J)
061100980505     C                   Z-ADD     N73           VVSC(K)
061200980505     C* INOLTRO
061300980505     C                   Z-ADD     VINL(J)       N11              11 0
061400980505     C                   Z-ADD     VINL(K)       VINL(J)
061500980505     C                   Z-ADD     N11           VINL(K)
061600980505     C* RPV
061700980505     C                   Z-ADD     VRPV(J)       N31               3 1
061800980505     C                   Z-ADD     VRPV(K)       VRPV(J)
061900980505     C                   Z-ADD     N31           VRPV(K)
062000980505     C* SEGNO %DELTA
062100980505     C                   MOVEL     VSDE(J)       A1                1
062200980505     C                   MOVEL     VSDE(K)       VSDE(J)
062300980505     C                   MOVEL     A1            VSDE(K)
062400980505     C* %DELTA
062500980505     C                   Z-ADD     VPDE(J)       N63               6 3
062600980505     C                   Z-ADD     VPDE(K)       VPDE(J)
062700980505     C                   Z-ADD     N63           VPDE(K)
062800980505     C* %INCIDENZA
062900980505     C                   Z-ADD     VPPE(J)       N41               4 1
063000980505     C                   Z-ADD     VPPE(K)       VPPE(J)
063100980505     C                   Z-ADD     N41           VPPE(K)
063200980505     C* ASTERISCO
063300980505     C                   MOVEL     VAST(J)       A1                1
063400980505     C                   MOVEL     VAST(K)       VAST(J)
063500980505     C                   MOVEL     A1            VAST(K)
063600980505E   5C                   ENDIF
063700980505E   4C                   ENDIF
063800980505E   3C                   ENDDO
063900980505E   2C                   ENDIF
064000980505E   1C                   ENDDO
064100980505     C*
064200980505     C                   ENDSR
064300970117     C*------------------------------------------------------------------------*
064400970117     C* AGGSFL - AGGIORNA RIGA DI SUBFILE
064500970117     C*------------------------------------------------------------------------*
064600970117     C     AGGSFL        BEGSR
064700970117     C*
064800970117     C                   SETON                                        80        *SI SFLNXTCHG
064900970117IF  1C     $TGES         IFEQ      'I'
065000970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
065100970117X   1C                   ELSE
065200970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
065300970117E   1C                   ENDIF
065400970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
065500970506     C                   SETON                                        02        *ND ALCUNI CAMPI
065600970506X   1C                   ELSE                                                   *CALCOLO DELTA
065700970506     C                   SETOFF                                       02        *NESSUNA ND
065800970506E   1C                   ENDIF
065900970117     C                   UPDATE    TE02S01                                      *AGGIORNA SUBFILE
066000970117     C                   SETOFF                                       80        *NO SFLNXTCHH
066100970117     C*
066200970117     C                   ENDSR
066300970117     C*------------------------------------------------------------------------*
066400970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
066500970117     C*------------------------------------------------------------------------*
066600970117     C     F03GES        BEGSR
066700970117     C*
066800970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
066900970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
067000011001     C                   MOVEL     'N'           D00ETS                         *S=TEETS CONTROLLATO
067100970117     C*
067200970117     C                   ENDSR
067300970117     C*------------------------------------------------------------------------*
067400970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
067500970117     C*------------------------------------------------------------------------*
067600970117     C     F12GES        BEGSR
067700970117     C*
067800970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
067900970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
068000011001     C                   MOVEL     'N'           D00ETS                         *S=TEETS CONTROLLATO
068100970117     C*
068200970117     C                   ENDSR
068300970117     C*------------------------------------------------------------------------*
068400970117     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
068500970117     C*------------------------------------------------------------------------*
068600970117     C     F05GES        BEGSR
068700970117     C*
068800970117     C* GESTIONE VIDETA
068900970117     C                   EXSR      IMPWD2
069000970117DO  1C     $FINV         DOUEQ     'S'
069100970117     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
069200970117E   1C                   ENDDO
069300970117     C*
069400970117     C* MEMORIZZA GLI SCAGLIONI DELLA TARIFFA RICHIESTA E CARICA IL SFILE
069500970117IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
069600970117     C                   EXSR      MEMETS                                       *MEMORIZZA
069700970117     C                   EXSR      POSSFL                                       *CARICA
069800970117E   1C                   ENDIF
069900970117     C*
070000970117     C                   ENDSR
070100970117     C*------------------------------------------------------------------------*
070200970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
070300970117     C*------------------------------------------------------------------------*
070400970117     C     IMPWD2        BEGSR
070500970117     C*
070600970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
070700970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
070800970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
070900980505     C                   MOVEL     D0DDSF        V2WDSF
071000980505     C                   MOVEL     D0DCTO        V2WCTO
071100970319     C                   Z-ADD     D0DKSC        V2WKSC
071200970319     C                   Z-ADD     D0DCTR        V2WCTR
071300970319     C                   Z-ADD     D0DPRG        V2WPRG
071400970319     C                   Z-ADD     D0DLNP        V2WLNP
071500011003     C                   MOVEL     D0DDIV        V2WDIV
071600970117     C                   EXSR      SETWD2
071700970117     C*
071800970117     C                   ENDSR
071900970117     C*------------------------------------------------------------------------*
072000970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
072100970117     C*------------------------------------------------------------------------*
072200970117     C     SETWD2        BEGSR
072300970117     C*
072400970117     C                   SETOFF                                       303132     ERRORI
072500011003     C                   SETOFF                                       333435
072600011003     C                   SETOFF                                       36
072700970117     C                   SETOFF                                       90         GENERICO DI ERRORE
072800970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
072900970117     C*
073000970117     C                   ENDSR
073100970117     C*------------------------------------------------------------------------*
073200970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
073300970117     C*------------------------------------------------------------------------*
073400970117     C     GESWD2        BEGSR
073500970117     C*
073600970117     C* EMISSIONE VIDEATA
073700970117     C                   EXFMT     TE02W02
073800970117     C                   EXSR      SETWD2
073900970117     C* F12-RITORNO
074000970117IF  1C     *INKL         IFEQ      *ON
074100970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
074200970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
074300970117     C                   GOTO      FINWD2
074400970117E   1C                   ENDIF
074500980504     C* F11-RICERCA
074600980504IF  1C     *INKK         IFEQ      *ON
074700980504     C                   EXSR      GESRIC
074800980504     C                   GOTO      FINWD2
074900980504E   1C                   ENDIF
075000970117     C* CONTROLLA VIDEO
075100970117     C                   EXSR      CTRWD2
075200970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
075300060825     C*                  GOTO      FINWD2
075400970117E   1C                   ENDIF
075500970117     C* F06-CONFERMA
075600970117IF  1C     *INKF         IFEQ      *ON
075700970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
075800970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
075900970117     C                   GOTO      FINWD2
076000970117E   1C                   ENDIF
076100970117     C*
076200970117     C     FINWD2        ENDSR
076300980504     C*------------------------------------------------------------------------*
076400980504     C* GESRIC - GESTIONE RICERCA DATI TIPO
076500980504     C*------------------------------------------------------------------------*
076600980504     C     GESRIC        BEGSR
076700980504     C*
076800980504     C                   CLEAR                   DSTE30
076900980504     C                   MOVEL     'C01'         D30OP0
077000980708     C                   MOVEL     'S'           D30ETX
077100980804     C                   Z-ADD     D00KSC        D30KSC
077200011011     C                   MOVEL     D00DIV        D30DIV
077300980504     C                   CALL      'TNTE32R'
077400980504     C                   PARM                    KPJBA
077500980504     C                   PARM                    DSTE30
077600980504IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
077700980504     C                   SETON                                        90
077800980504     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
077900980504X   1C                   ELSE                                                   *NO ERRORE
078000980504IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
078100980504     C     D30F12        OREQ      '1'
078200980504X   2C                   ELSE                                                   *SCELTA
078300980504     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
078400980504     C                   MOVEL     D30DSF        V2WDSF                         *CARICA CODICE
078500980504     C                   MOVEL     D30CTO        V2WCTO
078600980504     C                   Z-ADD     D30KSC        V2WKSC
078700980504     C                   Z-ADD     D30CTR        V2WCTR
078800980504     C                   Z-ADD     D30PRG        V2WPRG
078900980504     C                   Z-ADD     D30LNP        V2WLNP
079000011003     C                   MOVEL     D30DIV        V2WDIV
079100980504E   2C                   ENDIF
079200980504E   1C                   ENDIF
079300980504     C*
079400060825     C*                  MOVEL     'S'           $FINV                          *FINE FINESTRA
079500980504     C*
079600980504     C                   ENDSR
079700970117     C*------------------------------------------------------------------------*
079800970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
079900970117     C*------------------------------------------------------------------------*
080000970117     C     CTRWD2        BEGSR
080100980504     C*--------------------
080200980505     C* DATI SEDE O FILIALE
080300980504     C*--------------------
080400980504IF  1C     V2WDSF        IFEQ      'S'
080500980504     C     V2WDSF        OREQ      'F'
080600980504X   1C                   ELSE
080700980504     C                   SETON                                        35
080800980504     C                   SETON                                        90
080900980505     C                   MOVEL     MSG(21)       $MSG
081000980504     C                   GOTO      FINCW2
081100980504E   1C                   ENDIF
081200970117     C*--------------------
081300970117     C* TARIFFA O OFFERTA
081400970117     C*--------------------
081500970117IF  1C     V2WCTO        IFEQ      'T'
081600970117     C     V2WCTO        OREQ      'O'
081700011026     C* REPERIMENTO DATI TARIFFA
081800011026     C                   Z-ADD     V2WKSC        KTMKSC                         *CLIENTE
081900011026     C                   Z-ADD     V2WCTR        KTMCTR                         *TARIFFA
082000011026     C                   Z-ADD     V2WPRG        KTMPRG                         *PROGRESSIVO
082100011026     C     KEYTAM        CHAIN     TNTAM01L
082200011026     C                   IF        %found(TNTAM01L)
082300011026     C                   MOVEL     TAMFLO        DSTA01
082400011026     C                   MOVEL     §TADIV        V2WDIV
082500011026E   2C                   ENDIF
082600970117X   1C                   ELSE
082700970117     C                   SETON                                        33
082800970117     C                   SETON                                        90
082900970117     C                   MOVEL     MSG(12)       $MSG
083000970117     C                   GOTO      FINCW2
083100970117E   1C                   ENDIF
083101060825     C*--------------------
083102060825     C* stessa tipologia di codice tariffa
083103060825     C*--------------------
083104060825     c                   movel     d00ctr        comd00            1 0
083105060825     c                   movel     v2wctr        comv2w            1 0
083106060825     c                   if        comd00 <> comv2w
083107060825     C                   SETON                                        31
083108060825     C                   SETON                                        90
083109060825     C                   MOVEL     MSG(24)       $MSG
083110060825     C                   GOTO      FINCW2
083111060825     c                   end
083200970117     C*--------------------
083300970117     C* LINEA DI PARTENZA
083400970117     C*--------------------
083500970117     C                   MOVEL     V2WLNP        WLNP
083600970117     C                   MOVEL     *BLANKS       WDLNP
083700970117     C                   MOVEL     '0'           WERR
083800970117     C                   EXSR      CHKLNP
083900980818IF  1C     WERR          IFEQ      '1'
084000980818IF  2C     WLNP          IFEQ      888
084100980818     C     WLNP          OREQ      889
084200980818     C                   MOVEL     '0'           WERR
084300980818     C                   MOVEL     DDEC(10)      WDLNP
084400980818X   2C                   ELSE
084500011003     C                   SETON                                        343690
084600970117     C                   MOVEL     MSG(11)       $MSG
084700970117     C                   GOTO      FINCW2
084800980818E   2C                   ENDIF
084900980818E   1C                   ENDIF
085000970117     C*--------------------
085100970117     C* DATI TIPO ESISTENTI
085200970117     C*--------------------
085300980504     C                   MOVEL     V2WDSF        KTSDSF                         *DATI SEDE/FILIALE
085400970117     C                   MOVEL     V2WCTO        KTSCTO                         *TARIFFA O OFFERTA
085500970117     C                   Z-ADD     V2WKSC        KTSKSC                         *CLIENTE O VISITA
085600970117     C                   Z-ADD     V2WCTR        KTSCTR                         *TARIFFA
085700970117     C                   Z-ADD     V2WPRG        KTSPRG                         *PROGRESSIVO
085800970117     C                   Z-ADD     V2WLNP        KTSLNP                         *LINEA DI PARTENZA
085900970619     C                   MOVEL     'N'           WEETS                          *ESISTENZA SGL
086000011001     C     KE2ETS        SETLL     TEETS01L
086100011001     C     KE2ETS        READE     TEETS01L                               99
086200970619DO  1C     *IN99         DOWEQ     *OFF
086300970619IF  2C     ETSATB        IFEQ      *BLANKS
086400970619     C                   MOVEL     'S'           WEETS                          *ESISTENZA SGL
086500970619E   2C                   ENDIF
086600011001     C     KE2ETS        READE     TEETS01L                               99
086700970619E   1C                   ENDDO                                                  *FINE SCAGLIONI
086800970619IF  1C     WEETS         IFEQ      'N'                                          *NON TROVATI
086900970117     C                   SETON                                        303132
087000980504     C                   SETON                                        333435
087100011003     C                   SETON                                        36
087200980504     C                   SETON                                        90
087300970117     C                   MOVEL     MSG(13)       $MSG
087400970117E   1C                   ENDIF
087500970117     C*
087600970117     C     FINCW2        ENDSR
087700970117     C*------------------------------------------------------------------------*
087800970117     C* MEMETS - MEMORIZZA GLI SCAGLIONI DELLA TARIFFA RICHIESTA
087900970117     C*------------------------------------------------------------------------*
088000970117     C     MEMETS        BEGSR
088100980505     C*
088200980505     C* PULISCE LE SCHIERE DI CARICAMENTO DA ALTRA TARIFFA
088300980505     C                   EXSR      CLRSCC
088400970117     C*
088500970117     C* CARICA I DATI TIPO DELLA TARIFFA RICHIESTA
088600980504     C                   MOVEL     V2WDSF        KTSDSF                         *DATI SEDE/FILIALE
088700970117     C                   MOVEL     V2WCTO        KTSCTO                         *TARIFFA O OFFERTA
088800970117     C                   Z-ADD     V2WKSC        KTSKSC                         *CLIENTE O VISITA
088900970117     C                   Z-ADD     V2WCTR        KTSCTR                         *TARIFFA
089000970117     C                   Z-ADD     V2WPRG        KTSPRG                         *PROGRESSIVO
089100970117     C                   Z-ADD     V2WLNP        KTSLNP                         *LINEA DI PARTENZA
089200011001     C     KE2ETS        SETLL     TEETS01L                               99
089300011001     C     KE2ETS        READE(N)  TEETS01L                               99
089400970117DO  1C     *IN99         DOWEQ     *OFF
089500970117IF  2C     ETSATB        IFEQ      *BLANKS
089600980505     C                   ADD       1             C
089700980505     C                   Z-ADD     ETSSGL        CSGL(C)                        *SCAGLIONE
089800980505     C                   Z-ADD     ETSPSC        CPSC(C)                        *PESO MEDIO
089900980928     C******************** Z-ADDETSCSC    CCSC,C           *COLLI MEDI
090000980928     C     ETSCSC        IFLT      1
090100980928     C                   Z-ADD     1             CCSC(C)
090200980928     C                   ELSE
090300980928     C                   Z-ADD     ETSCSC        CCSC(C)                        *COLLI MEDI
090400980928     C                   ENDIF
090500980505     C                   Z-ADD     ETSVSC        CVSC(C)                        *VOLUME MEDIO
090600980505     C                   Z-ADD     ETSINL        CINL(C)                        *INOLTRO
090700980505     C                   Z-ADD     ETSRPV        CRPV(C)                        *RAPPORTO PESO/VOLUM
090800980505     C                   MOVEL     ETSSDE        CSDE(C)                        *SEGNO DELTA
090900980505     C                   Z-ADD     ETSPDE        CPDE(C)                        *% DELTA
091000980505     C                   Z-ADD     ETSPPE        CPPE(C)                        *% PESO
091100970117E   2C                   ENDIF
091200011001     C     KE2ETS        READE(N)  TEETS01L                               99
091300970117E   1C                   ENDDO
091400980505     C*
091500980505     C* PULISCE LE SCHIERE VIDEO
091600980505     C                   EXSR      CLRSCV
091700970117     C*
091800980505     C* MEMORIZZA I DATI TIPO DELLA TARIFFA RICHIESTA NELLE SCHIERE VIDEO
091900980505     C                   MOVEA     CSGL          VSGL                           *CAMPI
092000980505     C                   MOVEA     CPSC          VPSC
092100980505     C                   MOVEA     CCSC          VCSC
092200980505     C                   MOVEA     CVSC          VVSC
092300980505     C                   MOVEA     CINL          VINL
092400980505     C                   MOVEA     CRPV          VRPV
092500980505     C                   MOVEA     CSDE          VSDE
092600980505     C                   MOVEA     CPDE          VPDE
092700980505     C                   MOVEA     CPPE          VPPE
092800980505     C                   MOVEL     *BLANKS       VAST
092900980508     C*
093000980508     C* DEPOSITA NEI CAMPI DELLA DS LA TARIFFA RICHIESTA IN COPIA
093100980508     C                   MOVEL     V2WDSF        D0DDSF
093200980508     C                   MOVEL     V2WCTO        D0DCTO
093300980508     C                   Z-ADD     V2WKSC        D0DKSC
093400980508     C                   Z-ADD     V2WCTR        D0DCTR
093500980508     C                   Z-ADD     V2WPRG        D0DPRG
093600980508     C                   Z-ADD     V2WLNP        D0DLNP
093700011003     C                   MOVEL     V2WDIV        D0DDIV
093800980508     C*
093900970117     C                   ENDSR
094000980505     C*------------------------------------------------------------------------*
094100980505     C* CLRSCC - PULISCE LE SCHIERE DI CARICAMENTO DA ALTRA TARIFFA
094200980505     C*------------------------------------------------------------------------*
094300980505     C     CLRSCC        BEGSR
094400980505     C*
094500980505     C                   Z-ADD     *ZEROS        C                 4 0          *INDICE
094600980505     C                   CLEAR                   CSGL                           *CAMPI
094700980505     C                   CLEAR                   CPSC
094800980505     C                   CLEAR                   CCSC
094900980505     C                   CLEAR                   CVSC
095000980505     C                   CLEAR                   CINL
095100980505     C                   CLEAR                   CRPV
095200980505     C                   CLEAR                   CSDE
095300980505     C                   CLEAR                   CPDE
095400980505     C                   CLEAR                   CPPE
095500980505     C*
095600980505     C                   ENDSR
095700970117     C*------------------------------------------------------------------------*
095800970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
095900970117     C*------------------------------------------------------------------------*
096000970117     C     F06GES        BEGSR
096100980507     C*
096200980507     C* PER TUTTI GLI SCAGLIONI INSERITI
096300980507     C                   Z-ADD     1             $NRR
096400980507DO  1C     1             DO        10            V
096500980507IF  2C     VSGL(V)       IFGT      *ZEROS
096600980507     C*--------------------
096700980507     C* SCAGLIONE UNA VOLTA SOLA
096800980507     C*--------------------
096900980507IF  3C     WERR          IFEQ      '0'                                          *NO ERRORI
097000980507     C                   Z-ADD     1             I
097100980507     C                   SETOFF                                           98
097200980507     C     VSGL(V)       LOOKUP    VSGL(I)                                98
097300980507IF  4C     *IN98         IFEQ      *ON                                          *TROVATO
097400980507     C     V             ANDNE     I                                             *<> DA SE' STESSO
097500980507     C                   SETON                                        2190
097600980507     C                   MOVEL     MSG(6)        $MSG
097700980507     C                   MOVEL     '1'           WERR
097800980507     C                   Z-ADD     V             $NRR
097900980507     C                   LEAVE
098000980507E   4C                   ENDIF
098100980507E   3C                   ENDIF
098200980507     C*--------------------
098300980507     C* VALORE SCAGLIONE
098400980507     C*--------------------
098500980507     C* IL VALORE SCAGLIONE DEVE ESSERE > DELLO SCAGLIONE PRECEDENTE
098600980507IF  3C     WERR          IFEQ      '0'                                          *NO ERRORI
098700980507IF  4C     V             IFGT      1                                            *NON PER IL PRIMO
098800980507     C                   MOVEL     D00CTR        A1                1
098900980507IF  5C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
099000980507     C                   Z-ADD     VCSC(V)       W133             13 3
099100980507X   5C                   ELSE                                                   *TARIFFA A QLI/KG
099200980507     C                   Z-ADD     VPSC(V)       W133             13 3
099300980507E   5C                   ENDIF
099400980507     C     V             SUB       1             I
099500980507IF  5C     W133          IFLE      VSGL(I)
099600980507     C                   MOVEL     D00CTR        A1                1
099700980507IF  6C     A1            IFEQ      '1'                                          *TARIFFA A COLLI
099800980507     C                   SETON                                        23
099900980507X   6C                   ELSE                                                   *TARIFFA A QLI/KG
100000980507     C                   SETON                                        22
100100980507E   6C                   ENDIF
100200980507     C                   SETON                                        90
100300980507     C                   MOVEL     MSG(17)       $MSG
100400980507     C                   MOVEL     '1'           WERR
100500980507     C                   Z-ADD     V             $NRR
100600980507     C                   LEAVE
100700980507E   5C                   ENDIF
100800980507E   4C                   ENDIF
100900980507E   3C                   ENDIF
101000980507     C*---
101100980507     C* DELTA -> SCAGLIONI DATI TIPO = SCAGLIONI TARIFFA
101200980507     C*---
101300980507IF  3C     WERR          IFEQ      '0'
101400980507IF  4C     D00TLA        IFEQ      'D'
101500980507IF  5C     TSGL(V)       IFNE      VSGL(V)
101600980507     C                   SETON                                        90
101700980507     C                   MOVEL     MSG(10)       $MSG
101800980507     C                   MOVEL     '1'           WERR
101900980507     C                   Z-ADD     V             $NRR
102000980507     C                   LEAVE
102100980507E   5C                   ENDIF
102200980507E   4C                   ENDIF
102300980507E   3C                   ENDIF
102400980507     C*---
102500980507     C* TOTALE PESO % = 100
102600980507     C*---
102700980507IF  3C     WERR          IFEQ      '0'
102800980507IF  4C     V1CPPE        IFNE      100
102900980507     C                   SETON                                        1990
103000980507     C                   MOVEL     MSG(8)        $MSG
103100980507     C                   MOVEL     '1'           WERR
103200980507     C                   Z-ADD     1             $NRR                           *INIZIO SFILE
103300980507E   4C                   ENDIF
103400980507E   3C                   ENDIF
103500980909     C*---
103600980909     C* ASTERISCO > SOLO PORTO: NO F6 SE CI SONO ASTERISCHI: SCAGL. INCONGR. CON RPV
103700980909     C*---
103800980909IF  3C     WERR          IFEQ      '0'
103900980909IF  4C     D00TLA        IFEQ      'P'                                          *SOLO PER IL PORTO
104000980909IF  5C     VAST(V)       IFNE      *BLANK
104100980909     C                   SETON                                        90
104200980909     C                   MOVEL     MSG(22)       $MSG
104300980909     C                   MOVEL     '1'           WERR
104400980909     C                   Z-ADD     V             $NRR
104500980909     C                   LEAVE
104600980909E   5C                   ENDIF
104700980909E   4C                   ENDIF
104800980909E   3C                   ENDIF
104900980507     C*
105000980507E   2C                   ENDIF
105100980507E   1C                   ENDDO                                                  *FINE SCAGLIONI
105200980507     C*
105300980507     C* LEGGE IL RECORD DEL SFILE
105400980507     C                   Z-ADD     $NRR          NRR                            *POSIZIONAMENTO
105500980507     C     NRR           CHAIN     TE02S01                            98
105600980507     C                   EXSR      AGGSFL                                       *AGGIORNA SFILE
105700980505     C*
105800970117     C* AGGIORNA L'ARCHIVIO
105900980507IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
106000980507IF  2C     $TGES         IFEQ      'G'                                          *SE GESTIONE
106100970117     C                   EXSR      AGGETS
106200980507E   2C                   ENDIF
106300011001     C                   MOVEL     'S'           D00ETS                         *S=TEETS CONTROLLATO
106400970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
106500980507E   1C                   ENDIF
106600970117     C*
106700970117     C                   ENDSR
106800970117     C*------------------------------------------------------------------------*
106900011001     C* AGGETS - AGGIORNAMENTO ARCHIVIO -TEETS-
107000970117     C*------------------------------------------------------------------------*
107100970117     C     AGGETS        BEGSR
107200980505     C*
107300980505     C* ANNULLA TUTTI I DATI DELLA TARIFFA IN LINEA
107400980505     C                   MOVEL     D00DSF        KTSDSF                         *SEDE O FILIALE
107500980505     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
107600980505     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
107700980505     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
107800980505     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
107900980505     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
108000011001     C     KE2ETS        SETLL     TEETS01L                               99
108100011001     C     KE2ETS        READE     TEETS01L                               99
108200980505DO  1C     *IN99         DOWEQ     *OFF
108300980505IF  2C     ETSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
108400980505     C                   MOVEL     'A'           ETSATB
108500980505     C                   MOVEL     *BLANKS       ETSFTR
108600011001     C                   UPDATE    TEETS000                                     *AGGIORNA
108700980505X   2C                   ELSE                                                   *MAI TRASMESSO
108800011001     C                   DELETE    TEETS000                                     *CANCELLA
108900980505E   2C                   ENDIF
109000011001     C     KE2ETS        READE     TEETS01L                               99
109100980505E   1C                   ENDDO
109200011001     C                   UNLOCK    TEETS01L
109300970117     C*
109400980505     C* LEGGE TUTTI I RECORD CARICATI DAL VIDEO E AGGIORNA IL FILE
109500980505DO  1C     1             DO        10            V
109600980506IF  2C     VSGL(V)       IFGT      *ZEROS
109700970117     C*
109800970117     C* LEGGE IL RECORD IN ARCHIVIO
109900980505     C                   MOVEL     D00DSF        KTSDSF
110000970117     C                   MOVEL     D00CTO        KTSCTO
110100970117     C                   Z-ADD     D00KSC        KTSKSC
110200970117     C                   Z-ADD     D00CTR        KTSCTR
110300970117     C                   Z-ADD     D00PRG        KTSPRG
110400970117     C                   Z-ADD     D00LNP        KTSLNP
110500980506     C                   Z-ADD     VSGL(V)       KTSSGL
110600011001     C     KEYETS        CHAIN     TEETS01L                           99
110700970117     C* INSERIMENTO
110800970117IF  3C     *IN99         IFEQ      *ON
110900970117     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
111000011001     C                   WRITE     TEETS000
111100970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZ.IN TESTATA
111200970117X   3C                   ELSE
111300970117     C* VARIAZIONE
111400970117     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
111500011001     C                   UPDATE    TEETS000
111600970117E   3C                   ENDIF
111700970117E   2C                   ENDIF
111800980506E   1C                   ENDDO                                                  *FINE SCHIERA
111900980505     C*
112000970117     C                   ENDSR
112100970117     C*------------------------------------------------------------------------*
112200011001     C* IMPETS - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETS-
112300970117     C*------------------------------------------------------------------------*
112400970117     C     IMPETS        BEGSR
112500970117     C*
112600970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
112700980507     C                   MOVEL     KTSDSF        ETSDSF
112800980507     C                   MOVEL     KTSCTO        ETSCTO
112900970117     C                   Z-ADD     KTSKSC        ETSKSC
113000970117     C                   Z-ADD     KTSCTR        ETSCTR
113100970117     C                   Z-ADD     KTSPRG        ETSPRG
113200970117     C                   Z-ADD     KTSLNP        ETSLNP
113300970117     C                   Z-ADD     KTSSGL        ETSSGL
113400970117     C                   Z-ADD     DATCOR        ETSDIR
113500970117     C                   Z-ADD     *ZEROS        ETSDUV
113600970117     C                   MOVEL     *BLANKS       ETSFTR
113700970117     C                   Z-ADD     *ZEROS        ETSDTR
113800970117X   1C                   ELSE                                                   *VARIAZIONE
113900970117     C                   MOVEL     *BLANKS       ETSFTR
114000970117     C                   Z-ADD     DATCOR        ETSDUV
114100970117E   1C                   ENDIF
114200980506     C* IMPOSTA CAMPI
114300980506     C                   MOVEL     *BLANKS       ETSATB
114400980507     C                   Z-ADD     VPSC(V)       ETSPSC
114500980507     C                   Z-ADD     VCSC(V)       ETSCSC
114600980507     C                   Z-ADD     VVSC(V)       ETSVSC
114700980507     C                   Z-ADD     VINL(V)       ETSINL
114800980507     C                   Z-ADD     VRPV(V)       ETSRPV
114900980507     C                   MOVEL     VSDE(V)       ETSSDE
115000980507     C                   Z-ADD     VPDE(V)       ETSPDE
115100980507     C                   Z-ADD     VPPE(V)       ETSPPE
115200970117     C*
115300970117     C                   ENDSR
115400970117     C*------------------------------------------------------------------------*
115500970117     C* POSSFL - CARICA IL SUBFILE
115600970117     C*------------------------------------------------------------------------*
115700970117     C     POSSFL        BEGSR
115800970117     C*
115900970117     C* ELIMINA IL CONTENUTO DEL SFILE
116000970117     C                   EXSR      CLRSFL
116100970117     C*
116200970117     C* LEGGE IL PRIMO RECORD DA CARICARE
116300980505     C                   EXSR      SETVID
116400970117     C* CARICA IL SUBFILE
116500970117     C                   EXSR      CARSFL
116600970117     C*
116700970117     C                   ENDSR
116800970117     C*------------------------------------------------------------------------*
116900970117     C* CARSFL - CARICA UNA PAGINA DEL SUBFILE
117000970117     C*------------------------------------------------------------------------*
117100970117     C     CARSFL        BEGSR
117200970117     C*
117300980507     C                   Z-ADD     *ZEROS        NRR               4 0
117400970117DO  1C                   DO        SFLPAG
117500980507     C                   ADD       1             NRR
117600970117     C* VARIAZIONE
117700970117IF  2C     SFLEOF        IFEQ      *ZEROS
117800970117     C                   EXSR      IMPREC                                       *IMPOSTA CAMPI
117900970117     C* IMMISSIONE
118000970117X   2C                   ELSE
118100970117     C                   EXSR      AZZREC                                       *AZZERA CAMPI
118200970117E   2C                   ENDIF
118300970117     C* SCRIVE RECORD SFILE
118400970117     C                   WRITE     TE02S01
118500980507     C*
118600980507     C* LETTURA SUCESSIVA RECORD DA CARICARE
118700980507     C                   EXSR      LETVID                                       *LEGGE RECORD
118800970117E   1C                   ENDDO
118900970117     C*
119000970117     C                   ENDSR
119100970117     C*------------------------------------------------------------------------*
119200970117     C* CLRSFL - ELIMINA IL CONTENUTO DEL SUBFILE
119300970117     C*------------------------------------------------------------------------*
119400970117     C     CLRSFL        BEGSR
119500970117     C*
119600970117     C                   Z-ADD     1             $NRR
119700970117     C                   SETOFF                                       81        *NO SFLDSP
119800970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
119900970117     C                   WRITE     TE02C01
120000970117     C                   SETON                                        81        *SI SFLDSP
120100970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
120200970117     C*
120300970117     C                   ENDSR
120400970117     C*------------------------------------------------------------------------*
120500980505     C* SETVID - POSIZIONAMENTO E PRIMA LETTURA RECORD
120600970117     C*------------------------------------------------------------------------*
120700980505     C     SETVID        BEGSR
120800980505     C*
120900980505     C                   Z-ADD     *ZEROS        SFLEOF
121000970117     C*
121100970117     C* SI POSIZIONA DAL RECORD RICHIESTO
121200980505     C                   Z-ADD     1             V                              *INDICE SCHIERE VIDE
121300980505     C*
121400980505     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
121500980507     C     VSGL(V)       IFEQ      *ZEROS                                       *NESSUNA MEMORIZZAZI
121600980505     C                   Z-ADD     1             SFLEOF                         *F I N E   F I L E
121700980505E   1C                   ENDIF
121800970117     C*
121900970117     C                   ENDSR
122000970117     C*------------------------------------------------------------------------*
122100980505     C* LETVID - LETTURA SUCESSIVA RECORD
122200970117     C*------------------------------------------------------------------------*
122300980505     C     LETVID        BEGSR
122400980505     C*
122500980505     C                   ADD       1             V                              *INDICE SCHIERE VIDE
122600980505     C*
122700980505     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
122800980505IF  1C     V             IFGT      10                                           LUNGHEZZA MAX SCHIER
122900980505     C     VSGL(V)       OREQ      *ZEROS                                       *SCHIERA VUOTA
123000980505     C                   Z-ADD     1             SFLEOF
123100980505E   1C                   ENDIF
123200970117     C*
123300970117     C                   ENDSR
123400970117     C*------------------------------------------------------------------------*
123500970117     C* AZZREC - AZZERA I CAMPI DEL SUBFILE
123600970117     C*------------------------------------------------------------------------*
123700970117     C     AZZREC        BEGSR
123800970117     C*
123900980505     C                   Z-ADD     *ZEROS        V1HNRR                         *SE =0 IMMISSIONE
124000970117     C                   MOVEL     *BLANKS       V1SATB
124100970117     C                   Z-ADD     *ZEROS        V1SSGL
124200970117     C                   Z-ADD     *ZEROS        V1SPSC
124300970117     C                   Z-ADD     *ZEROS        V1SCSC
124400970117     C                   Z-ADD     *ZEROS        V1SVSC
124500970317     C                   Z-ADD     *ZEROS        V1SINL
124600970317     C                   Z-ADD     *ZEROS        V1SRPV
124700970402     C                   MOVEL     *BLANKS       V1SSDE
124800970402     C                   Z-ADD     *ZEROS        V1SPDE
124900970117     C                   Z-ADD     *ZEROS        V1SPPE
125000970117     C*
125100970117     C                   Z-ADD     *ZEROS        DUPPSC                         *TASTI DI DUPLICA
125200970117     C                   Z-ADD     *ZEROS        DUPCSC
125300970117     C                   Z-ADD     *ZEROS        DUPVSC
125400970317     C                   Z-ADD     *ZEROS        DUPINL
125500970317     C                   Z-ADD     *ZEROS        DUPRPV
125600970402     C                   MOVEL     *BLANKS       DUPSDE
125700970402     C                   Z-ADD     *ZEROS        DUPPDE
125800970117     C                   Z-ADD     *ZEROS        DUPPPE
125900011009     C***                MOVEL     *BLANKS       V1SAST
126000011009     C                   SETOFF                                       70
126100970203     C* INDICATORI
126200970203     C                   SETOFF                                       80        *NO SFLNXTCHH
126300970203IF  1C     $TGES         IFEQ      'I'
126400970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
126500970203X   1C                   ELSE
126600970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
126700970203E   1C                   ENDIF
126800970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
126900970506     C                   SETON                                        02        *ND ALCUNI CAMPI
127000970506X   1C                   ELSE                                                   *CALCOLO DELTA
127100970506     C                   SETOFF                                       02        *NESSUNA ND
127200970506E   1C                   ENDIF
127300970117     C*
127400970117     C                   ENDSR
127500970117     C*------------------------------------------------------------------------*
127600970117     C* IMPREC - IMPOSTA I CAMPI DEL SUBFILE
127700970117     C*------------------------------------------------------------------------*
127800970117     C     IMPREC        BEGSR
127900970117     C*
128000980505     C                   Z-ADD     V             V1HNRR                         *SE =1 VARIAZIONE
128100970117     C                   MOVEL     *BLANKS       V1SATB
128200980505     C                   Z-ADD     VSGL(V)       V1SSGL
128300980505     C                   Z-ADD     VPSC(V)       V1SPSC
128400011009     C                   Z-ADD     VCSC(V)       V1SCSC
128500980505     C                   Z-ADD     VVSC(V)       V1SVSC
128600980505     C                   Z-ADD     VINL(V)       V1SINL
128700980505     C                   Z-ADD     VRPV(V)       V1SRPV
128800980505     C                   MOVEL     VSDE(V)       V1SSDE
128900980505     C                   Z-ADD     VPDE(V)       V1SPDE
129000980505     C                   Z-ADD     VPPE(V)       V1SPPE
129100980505     C*
129200011009     C***                MOVEL     *BLANKS       V1SAST
129300011009     C                   SETOFF                                       70
129400980505     C                   EXSR      CHKAST                                        *CONTROLLA ASTERISC
129500970117     C*
129600970117     C                   Z-ADD     V1SPSC        DUPPSC                         *TASTI DI DUPLICA
129700970117     C                   Z-ADD     V1SCSC        DUPCSC
129800970117     C                   Z-ADD     V1SVSC        DUPVSC
129900970317     C                   Z-ADD     V1SINL        DUPINL
130000970317     C                   Z-ADD     V1SRPV        DUPRPV
130100970402     C                   MOVEL     V1SSDE        DUPSDE
130200970402     C                   Z-ADD     V1SPDE        DUPPDE
130300970117     C                   Z-ADD     V1SPPE        DUPPPE
130400970203     C*
130500970203     C                   SETOFF                                       80        *NO SFLNXTCHH
130600970203IF  1C     $TGES         IFEQ      'I'
130700970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
130800970203X   1C                   ELSE
130900970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
131000970203E   1C                   ENDIF
131100970506IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
131200970506     C                   SETON                                        02        *ND ALCUNI CAMPI
131300970506X   1C                   ELSE                                                   *CALCOLO DELTA
131400970506     C                   SETOFF                                       02        *NESSUNA ND
131500970506E   1C                   ENDIF
131600970117     C*
131700970117     C                   ENDSR
131800970117     C*------------------------------------------------------------------------*
131900970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
132000970117     C*------------------------------------------------------------------------*
132100970117     C     CHKLNP        BEGSR
132200970117     C*
132300970117     C     *LIKE         DEFINE    V0CLNP        WLNP
132400970117     C     *LIKE         DEFINE    V0DLNP        WDLNP
132500970117     C*
132600970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
132700970117     C                   MOVEL     WLNP          KORFIL
132800970117     C     KEYORG        CHAIN     AZORG01L                           99
132900970117IF  1C     *IN99         IFEQ      *OFF
133000970117     C     ORGFVA        ANDEQ     *BLANKS
133100970117     C     ORGFAG        ANDNE     'V'
133200970117     C                   MOVEL     ORGDES        WDLNP
133300970117X   1C                   ELSE
133400970117     C                   MOVEL     '1'           WERR
133500970117E   1C                   ENDIF
133600970117     C*
133700970117     C                   ENDSR
133800970117     C*------------------------------------------------------------------------*
133900970117     C* IMPVD0 - IMPOSTA LA TESTATA
134000970117     C*------------------------------------------------------------------------*
134100970117     C     IMPVD0        BEGSR
134200970117     C*
134300970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
134400970117     C*---
134500970117     C* TESTATA
134600970117     C*---
134700970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
134800970117IF  1C     D00TLA        IFEQ      'P'
134900970117     C                   MOVEL(P)  'PORTO'       V0CTLA                         *LIVELLO PROCEDURA
135000970117X   1C                   ELSE
135100970117     C                   MOVEL(P)  'DELTA'       V0CTLA                         *LIVELLO PROCEDURA
135200970117E   1C                   ENDIF
135300970117IF  1C     D00CTO        IFEQ      'O'
135400970117     C                   MOVEL     'Visita '     V0DKOV                         *TARIFFA O OFFERTA
135500970117X   1C                   ELSE
135600970117     C                   MOVEL     'Cliente'     V0DKOV                         *TARIFFA O OFFERTA
135700970117E   1C                   ENDIF
135800970117IF  1C     $TGES         IFEQ      'I'
135900970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
136000970117     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
136100970117X   1C                   ELSE
136200970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
136300970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
136400970117E   1C                   ENDIF
136500970117     C* CAMPI
136600970117     C                   Z-ADD     D00KSC        V0CKSC
136700970117     C                   Z-ADD     D00CTR        V0CCTR
136800970117     C                   Z-ADD     D00PRG        V0CPRG
136900970117     C                   MOVEL     D00FIE        V0CFIE
137000980505     C                   MOVEL     D00DSF        V0CDSF
137100970117     C                   MOVEL     D00TSP        V0CTSP
137200970117     C                   Z-ADD     D00LNP        V0CLNP
137300011003     C                   MOVEL     D00DIV        V0CDIV
137400970117IF  1C     D00DTC        IFGT      *ZEROS
137500970117     C                   Z-ADD     D00DTC        G08INV
137600970117     C                   Z-ADD     *ZEROS        G08DAT
137700970117     C                   MOVEL     '3'           G08ERR
137800970117     C                   CALL      'XSRDA8'
137900970117     C                   PARM                    WLBDA8
138000970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
138100970117E   1C                   ENDIF
138200970117     C* DECODIFICHE
138300970117     C                   MOVEL     *BLANKS       V0DKSC
138400970117IF  1C     D00KSC        IFGT      *ZEROS
138500020906     C                   Z-ADD     1             KACKUT
138600970117     C                   Z-ADD     KCCCL         KACKCC
138700970117     C                   Z-ADD     D00KSC        KACKSC
138800970117     C     KEYACO        CHAIN     CNACO00F                           99
138900000211IF  2C*---       *IN99     IFEQ *OFF
139000000211     C*---                 MOVELACORAG    V0DKSC           *CLIENTE
139100000211E   2C*---                 ENDIF
139200000211IF  2C     D00CTO        IFEQ      'T'                                          *TARIFFA
139300000211     C     KEYACO        CHAIN     CNACO00F                           99
139400000211X   2C                   ELSE                                                   *OFFERTA
139500000211     C     KEYACO        CHAIN     TFACO00F                           99
139600000211E   2C                   ENDIF
139700000211IF  2C     *IN99         IFEQ      *OFF
139800000211     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
139900000211E   2C                   ENDIF
140000970117E   1C                   ENDIF
140100970117     C*
140200970117     C                   MOVEL     *BLANKS       V0DCTR
140300970117IF  1C     D00KSC        IFGT      *ZEROS
140400980507     C                   MOVEL     D00CTR        A1                1
140500970117     C                   SETOFF                                           99
140600970117     C                   Z-ADD     1             I
140700980507     C     A1            LOOKUP    TTR(I)                                 99
140800970117IF  2C     *IN99         IFEQ      *ON
140900970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
141000970117E   2C                   ENDIF
141100970117E   1C                   ENDIF
141200970117     C*
141300970117     C                   MOVEL     *BLANKS       V0DTSP
141400970117IF  1C     D00TSP        IFNE      *BLANKS
141500970117     C                   SETOFF                                           99
141600970117     C                   Z-ADD     1             I
141700970117     C     D00TSP        LOOKUP    TSP(I)                                 99
141800970117IF  2C     *IN99         IFEQ      *ON
141900970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
142000970117E   2C                   ENDIF
142100970117E   1C                   ENDIF
142200970117     C*
142300970117     C                   MOVEL     *BLANKS       V0DFIE
142400970117IF  1C     D00FIE        IFEQ      'I'
142500970117     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
142600970117E   1C                   ENDIF
142700970117IF  1C     D00FIE        IFEQ      'E'
142800970117     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
142900970117E   1C                   ENDIF
143000980505     C*
143100980505     C                   MOVEL     *BLANKS       V0DDSF
143200980505IF  1C     D00DSF        IFEQ      'S'
143300980505     C                   MOVEL     DDEC(5)       V0DDSF                         *DATI SEDE/FILIALE
143400980505E   1C                   ENDIF
143500980505IF  1C     D00DSF        IFEQ      'F'
143600980505     C                   MOVEL     DDEC(6)       V0DDSF                         *DATI SEDE/FILIALE
143700980505E   1C                   ENDIF
143800970117     C*
143900970117     C                   MOVEL     *BLANKS       V0DLNP
144000970117IF  1C     D00LNP        IFGT      *ZEROS
144100970117     C                   Z-ADD     D00LNP        KORFIL
144200970117     C     KEYORG        CHAIN     AZORG01L                           99
144300970117IF  2C     *IN99         IFEQ      *OFF
144400970117     C     ORGFVA        ANDEQ     *BLANKS
144500970117     C     ORGFAG        ANDNE     'V'
144600970117     C                   MOVEL     ORGDES        V0DLNP
144700970117E   2C                   ENDIF
144800970117E   1C                   ENDIF
144900970117     C*---
145000970117     C* SUBFILE CONTROL
145100970117     C*---
145200980505     C                   MOVEL     D00DSF        KTCDSF                         *DATI SEDE O FILIALE
145300980505     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
145400970117     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE
145500970117     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
145600970117     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
145700970117     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
145800011001     C     KEYETC        CHAIN     TEETC01L                           99
145900970117IF  1C     *IN99         IFEQ      *OFF
146000970117     C                   Z-ADD     ETCPMC        V1CPMC                         *PESO MEDIO COLLO
146100970117     C                   Z-ADD     ETCVMC        V1CVMC                         *VOLUME MEDIO COLLO
146200970117E   1C                   ENDIF
146300970117     C                   MOVEL     DVID(2)       DVID02
146400980507     C                   MOVEL     D00CTR        A1                1
146500980507IF  1C     A1            IFEQ      '0'                                          *QUINTALI
146600980507     C     A1            OREQ      '3'                                          *KG
146700970117     C                   MOVEL     DDEC(4)       D02SGL                         *SCAGLIONI ->KG
146800970117     C                   MOVEL     DDEC(4)       D02PSC                         *PESO MEDIO->KG
146900970117E   1C                   ENDIF
147000980507IF  1C     A1            IFEQ      '1'                                          *COLLI
147100970117     C                   MOVEL     DDEC(3)       D02SGL                         *SCAGLIONI ->COLLI
147200970117     C                   MOVEL     DDEC(4)       D02PSC                         *PESO MEDIO->KG
147300970117E   1C                   ENDIF
147400970117     C                   MOVEL     DVID02        V1CDES                         *DESCRIZIONE CAMPI
147500970117     C*
147600970117     C                   ENDSR
147700970117     C*------------------------------------------------------------------------*
147800970117     C* CARTAD - CARICA GLI SCAGLIONI DEL DETTAGLIO TARIFFA
147900970117     C*------------------------------------------------------------------------*
148000970117     C     CARTAD        BEGSR
148100970117     C*
148200970117     C     *LIKE         DEFINE    TADORP        WORP                           *DI LAVORO
148300970117     C*
148400970117     C* CARICA I PRIMI SCAGLIONI DEL DETTAGLIO TARIFFA DI UN CODICE TASSAZIONE
148500970117     C                   CLEAR                   TSGL
148600970117     C                   Z-ADD     *ZEROS        I
148700970117     C                   Z-ADD     D00KSC        KTDKSC
148800970117     C                   Z-ADD     D00CTR        KTDCTR
148900970117     C                   Z-ADD     D00PRG        KTDPRG
149000970117     C                   Z-ADD     D00LNP        KTDLNP
149100990818     C     KEYTAD        SETLL     TITAD04L
149200990818     C     KEYTAD        READE     TITAD04L                               99
149300970117DO  1C     *IN99         DOWEQ     *OFF
149400970117     C                   MOVEL     TADORP        WORP
149500970117DO  2C     *IN99         DOWEQ     *OFF
149600970117     C     TADORP        ANDEQ     WORP
149700970117IF  3C     TADATB        IFEQ      *BLANKS                                      *NO ANNULLATI
149800970117     C                   ADD       1             I
149900970117     C                   Z-ADD     TADSGL        TSGL(I)
150000970117E   3C                   ENDIF
150100990818     C     KEYTAD        READE     TITAD04L                               99
150200970117E   2C                   ENDDO                                                  *FINE COD.TASSAZIONE
150300970117     C                   LEAVE
150400970117E   1C                   ENDDO                                                  *FINE DETTAGLIO
150500970117     C*
150600970117     C                   ENDSR
150700970117     C*------------------------------------------------------------------------*
150800970117     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
150900970117     C*------------------------------------------------------------------------*
151000970117     C     CTRENT        BEGSR
151100970117     C*--------------------
151200980505     C* INTERROGAZIONE -> DATI TIPO INESISTENTI
151300970117     C*--------------------
151400970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
151500970130     C                   MOVEL     '0'           WSGL              1
151600980505     C                   MOVEL     D00DSF        KTSDSF                         *DATI SEDE/FILIALE
151700970117     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
151800970117     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE P VISITA
151900970117     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
152000970117     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
152100970117     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
152200011001     C     KE2ETS        SETLL     TEETS01L
152300011001     C     KE2ETS        READE(N)  TEETS01L                               99
152400970130DO  2C     *IN99         DOWEQ     *OFF                                         *NON TROVATO
152500970130IF  3C     ETSATB        IFEQ      *BLANKS
152600970130     C                   MOVEL     '1'           WSGL                           *SCAGLIONI ESISTENTI
152700970130     C                   LEAVE
152800970130E   3C                   ENDIF
152900011001     C     KE2ETS        READE(N)  TEETS01L                               99
153000970130E   2C                   ENDDO
153100970130IF  2C     WSGL          IFEQ      '0'
153200980505     C                   MOVEL     '1'           WERR                           *ERRORE
153300970117     C                   MOVEL     MSG(14)       D00MSG
153400970117     C                   MOVEL     '1'           D00ERR
153500970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
153600970117E   2C                   ENDIF
153700970117E   1C                   ENDIF
153800970117     C*
153900970117     C                   ENDSR
154000980505     C*------------------------------------------------------------------------*
154100980505     C* CARSGL - CARICA GLI SCAGLIONI NELLE SCHIERE VIDEO
154200980505     C*------------------------------------------------------------------------*
154300980505     C     CARSGL        BEGSR
154400980505     C*
154500980505     C* PULISCE LE SCHIERE VIDEO
154600980505     C                   EXSR      CLRSCV
154700980505     C*
154800980505     C* CARICA GLI SCAGLIONI
154900980505     C                   MOVEL     D00DSF        KTSDSF                         *DATI SEDE/FILIALE
155000980505     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
155100980505     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
155200980505     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
155300980505     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
155400980505     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
155500011001     C     KE2ETS        SETLL     TEETS01L                               99
155600980505IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
155700980505     C                   MOVEL     *BLANKS       V0COPE                         *NESSUNA OPERAZIONE
155800980505X   2C                   ELSE                                                   *GESTIONE
155900980505IF  2C     *IN99         IFEQ      *OFF                                         *NON TROVATO
156000980505     C                   MOVEL     DOPE(1)       V0COPE                         *INSERIMENTO
156100980505X   2C                   ELSE                                                   *TROVATO
156200980505     C                   MOVEL     DOPE(2)       V0COPE                         *VARIAZIONE
156300980505E   2C                   ENDIF
156400980505E   1C                   ENDIF
156500011001     C     KE2ETS        READE(N)  TEETS01L                               99
156600980505DO  1C     *IN99         DOWEQ     *OFF
156700980505IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
156800980505     C                   ADD       1             V
156900980505     C                   Z-ADD     ETSSGL        VSGL(V)                        *SCAGLIONE
157000980505     C                   Z-ADD     ETSPSC        VPSC(V)                        *PESO MEDIO
157100980928     C******************** Z-ADDETSCSC    VCSC,V           *COLLI MEDI
157200980928     C     ETSCSC        IFLT      1
157300980928     C                   Z-ADD     1             VCSC(V)
157400980928     C                   ELSE
157500980928     C                   Z-ADD     ETSCSC        VCSC(V)                        *COLLI MEDI
157600980928     C                   ENDIF
157700980505     C                   Z-ADD     ETSVSC        VVSC(V)                        *VOLUME MEDIO
157800980505     C                   Z-ADD     ETSINL        VINL(V)                        *INOLTRO
157900980505     C                   Z-ADD     ETSRPV        VRPV(V)                        *RAPPORTO PESO/VOLUM
158000980505     C                   MOVEL     ETSSDE        VSDE(V)                        *SEGNO DELTA
158100980505     C                   Z-ADD     ETSPDE        VPDE(V)                        *% DELTA
158200980505     C                   Z-ADD     ETSPPE        VPPE(V)                        *% PESO
158300980505     C                   MOVEL     *BLANKS       VAST(V)                        *ASTERISCO
158400980505E   2C                   ENDIF
158500011001     C     KE2ETS        READE(N)  TEETS01L                               99
158600980505E   1C                   ENDDO                                                  *FINE FILE
158700980505     C*
158800980505     C                   ENDSR
158900980505     C*------------------------------------------------------------------------*
159000011001     C* CHKETS - CONTROLLA VALIDITA' RECORD -TEETS-
159100980505     C*------------------------------------------------------------------------*
159200980505     C     CHKETS        BEGSR
159300980505     C*
159400980505     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
159500980505     C*
159600980505     C* SCAGLIONI ANNULLATI, ESCLUDE
159700980505IF  1C     ETSATB        IFNE      *BLANKS
159800980505     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
159900980505E   1C                   ENDIF
160000980505     C*
160100980505     C                   ENDSR
160200980505     C*------------------------------------------------------------------------*
160300980505     C* CLRSCV - PULISCE LE SCHIERE VIDEO
160400980505     C*------------------------------------------------------------------------*
160500980505     C     CLRSCV        BEGSR
160600980505     C*
160700980505     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE
160800980505     C                   CLEAR                   VSGL                           *CAMPI
160900980505     C                   CLEAR                   VPSC
161000980505     C                   CLEAR                   VCSC
161100980505     C                   CLEAR                   VVSC
161200980505     C                   CLEAR                   VINL
161300980505     C                   CLEAR                   VRPV
161400980505     C                   CLEAR                   VSDE
161500980505     C                   CLEAR                   VPDE
161600980505     C                   CLEAR                   VPPE
161700980505     C                   CLEAR                   VAST
161800980505     C*
161900980505     C                   ENDSR
162000970117     C*------------------------------------------------------------------------*
162100970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
162200970117     C*------------------------------------------------------------------------*
162300970117     C     CARTAB        BEGSR
162400970117     C*--------------------
162500970117     C* TIPI SERVIZIO
162600970117     C*--------------------
162700970117     C                   Z-ADD     *ZEROS        I                 4 0
162800020906     C                   Z-ADD     1             KTBKUT
162900970117     C                   MOVEL     '5E'          KTBCOD
163000970117     C     KE2TAB        SETLL     TABEL00F
163100970117     C     KE2TAB        READE     TABEL00F                               99
163200970117DO  1C     *IN99         DOWEQ     *OFF
163300970117IF  2C     TBLFLG        IFEQ      *BLANKS
163400970117     C                   ADD       1             I
163500970117     C                   MOVEL     TBLUNI        DS5E
163600970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
163700970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
163800970117E   2C                   ENDIF
163900970117     C     KE2TAB        READE     TABEL00F                               99
164000970117E   1C                   ENDDO
164100970117     C*--------------------
164200970117     C* TIPI TARIFFA
164300970117     C*--------------------
164400970117     C                   Z-ADD     *ZEROS        I                 4 0
164500020906     C                   Z-ADD     1             KTBKUT
164600970117     C                   MOVEL     'TR'          KTBCOD
164700970117     C     KE2TAB        SETLL     TABEL00F
164800970117     C     KE2TAB        READE     TABEL00F                               99
164900970117DO  1C     *IN99         DOWEQ     *OFF
165000970117IF  2C     TBLFLG        IFEQ      *BLANKS
165100970117     C                   MOVEL     TBLUNI        DSTR
165200970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
165300970117     C                   ADD       1             I
165400970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
165500970117     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
165600970117E   3C                   ENDIF
165700970117E   2C                   ENDIF
165800970117     C     KE2TAB        READE     TABEL00F                               99
165900970117E   1C                   ENDDO
166000970117     C*--------------------
166100970117     C* TARIFFA DI CARTELLO
166200970117     C*--------------------
166300970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
166400970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
166500970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
166600021202     C                   Z-ADD     *ZEROS        CARKSC
166700021202     C                   Z-ADD     *ZEROS        CARCTR
166800021202     C                   Z-ADD     *ZEROS        CARPRG
166900021202     C                   CLEAR                   TRUL27DS
167000021202     C                   EVAL      I27TLA = 'L'
167100021202     C                   EVAL      I27DTA = DATCOR
167200021202     C                   CALL      'TRUL27R'
167300021202     C                   PARM                    TRUL27DS
167400021202     C*
167500021202IF  1C                   IF        O27ERR <> 'E'
167600021202     C                   Z-ADD     O27KSC        KTMKSC
167700970117IF  2C     D00CTO        IFEQ      'O'
167800970117     C     KTMKSC        SETLL     TNOFM01L
167900970117     C     KTMKSC        READE     TNOFM01L                               99
168000970117X   2C                   ELSE
168100970117     C     KTMKSC        SETLL     TNTAM01L
168200970117     C     KTMKSC        READE     TNTAM01L                               99
168300970117E   2C                   ENDIF
168400970117DO  2C     *IN99         DOWEQ     *OFF
168500970117IF  3C     TAMATB        IFNE      'A'                                          *NO ANNULLATE
168600970117     C     TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
168700970117IF  4C     DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
168800970117     C     DATCOR        ANDLE     TAMDST
168900970117     C                   Z-ADD     TAMKSC        CARKSC
169000970117     C                   Z-ADD     TAMCTR        CARCTR
169100970117     C                   Z-ADD     TAMPRG        CARPRG
169200970117     C                   LEAVE
169300970117E   4C                   ENDIF
169400970117E   3C                   ENDIF
169500970117IF  3C     D00CTO        IFEQ      'O'
169600970117     C     KTMKSC        READE     TNOFM01L                               99
169700970117X   3C                   ELSE
169800970117     C     KTMKSC        READE     TNTAM01L                               99
169900970117E   3C                   ENDIF
170000970117E   2C                   ENDDO
170100970117E   1C                   ENDIF
170200970117     C*
170300970117     C                   ENDSR
170400970117     C*------------------------------------------------------------------------*
170500970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
170600970117     C*------------------------------------------------------------------------*
170700970117     C     CARCC         BEGSR
170800970117     C*
170900970117     C* RECUPERA I DATI DELL'UTENTE
171000020906     C                   Z-ADD     1             CODUT
171100020830     C                   CALL      'X§PARUT'
171200970117     C                   PARM                    PARUT
171300970117     C*
171400970117     C                   Z-ADD     1             Z
171500970117DO  1C                   DO        50            X2                2 0
171600970117     C                   MOVE      TCU(X2)       CARCNT
171700970117IF  2C     TIPCNT        IFEQ      'CI'
171800970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
171900970117E   2C                   ENDIF
172000970117IF  2C     FL34CN        IFEQ      '01'
172100970117     C     TIPCNT        ANDEQ     'CG'
172200970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
172300970117E   2C                   ENDIF
172400970117IF  2C     FL34CN        IFEQ      '02'
172500970117     C     TIPCNT        ANDEQ     'CG'
172600970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
172700970117E   2C                   ENDIF
172800970117IF  2C     TIPCNT        IFEQ      'RI'
172900970117     C                   Z-ADD     KCU(X2)       RI(Z)
173000970117     C                   ADD       1             Z                 3 0
173100970117E   2C                   ENDIF
173200970117E   1C                   ENDDO
173300970117     C*
173400970117     C                   ENDSR
173500970117     C*------------------------------------------------------------------------*
173600970117     C* *INZSR - OPERAZIONI INIZIALI
173700970117     C*------------------------------------------------------------------------*
173800970117     C     *INZSR        BEGSR
173900970117     C*--------------------
174000970117     C* RICEVIMENTO PARAMETRI
174100970117     C*--------------------
174200970117     C     *ENTRY        PLIST
174300970117     C                   PARM                    KPJBA
174400970117     C                   PARM                    DSTE00
174500041202     c* apertura controllata di alcuni file xchè solo di filiale
174600041202     c                   open      tfaco00f                             95
174700041202     c                   open      tnofm01l                             95
174800970117     C*--------------------
174900970117     C* VARIABILI RIFERITE AL DATABASE
175000970117     C*--------------------
175100970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
175200970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
175300970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
175400970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
175500970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
175600970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
175700970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
175800970117     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
175900990818     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
176000970117     C     *LIKE         DEFINE    TADCTR        KTDCTR
176100970117     C     *LIKE         DEFINE    TADPRG        KTDPRG
176200970117     C     *LIKE         DEFINE    TADLNP        KTDLNP
176300011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
176400980505     C     *LIKE         DEFINE    ETCCTO        KTCCTO
176500970117     C     *LIKE         DEFINE    ETCKSC        KTCKSC
176600970117     C     *LIKE         DEFINE    ETCCTR        KTCCTR
176700970117     C     *LIKE         DEFINE    ETCPRG        KTCPRG
176800970117     C     *LIKE         DEFINE    ETCLNP        KTCLNP
176900011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
177000980505     C     *LIKE         DEFINE    ETSCTO        KTSCTO
177100970117     C     *LIKE         DEFINE    ETSKSC        KTSKSC
177200970117     C     *LIKE         DEFINE    ETSCTR        KTSCTR
177300970117     C     *LIKE         DEFINE    ETSPRG        KTSPRG
177400970117     C     *LIKE         DEFINE    ETSLNP        KTSLNP
177500970117     C     *LIKE         DEFINE    ETSSGL        KTSSGL
177600970117     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
177700970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
177800970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
177900011026     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
178000011026     C     *LIKE         DEFINE    TAMCTR        KTMCTR
178100011026     C     *LIKE         DEFINE    TAMPRG        KTMPRG
178200970117     C*--------------------
178300970117     C* CHIAVI DI LETTURA
178400970117     C*--------------------
178500970117     C* LETTURA TABEL00F - COMPLETA
178600970117     C     KEYTAB        KLIST
178700970117     C                   KFLD                    KTBKUT                         *UTENTE
178800970117     C                   KFLD                    KTBCOD                         *TABELLA
178900970117     C                   KFLD                    KTBKEY                         *CODICE
179000970117     C* LETTURA TABEL00F - PARZIALE
179100970117     C     KE2TAB        KLIST
179200970117     C                   KFLD                    KTBKUT                         *UTENTE
179300970117     C                   KFLD                    KTBCOD                         *TABELLA
179400970117     C* LETTURA AZORG01L - COMPLETA
179500970117     C     KEYORG        KLIST
179600970117     C                   KFLD                    KORFIL                         *FILIALE
179700970117     C* LETTURA CNACO01L - COMPLETA
179800970117     C     KEYACO        KLIST
179900970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
180000970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
180100970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
180200990818     C* LETTURA TITAD04L - PARZIALE
180300970117     C     KEYTAD        KLIST
180400970117     C                   KFLD                    KTDKSC                         *CLIENTE
180500970117     C                   KFLD                    KTDCTR                         *TARIFFA
180600970117     C                   KFLD                    KTDPRG                         *PROGRESSIVO
180700970117     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
180800011001     C* LETTURA TEETC01L - COMPLETA
180900970117     C     KEYETC        KLIST
181000980505     C                   KFLD                    KTCDSF                         *DATI SEDE  O FILIAL
181100980505     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
181200970117     C                   KFLD                    KTCKSC                         *CLIENTE
181300970117     C                   KFLD                    KTCCTR                         *TARIFFA
181400970117     C                   KFLD                    KTCPRG                         *PROGRESSIVO
181500970117     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
181600011001     C* LETTURA TEETS01L - COMPLETA
181700980505     C     KEYETS        KLIST
181800980505     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
181900980505     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
182000980505     C                   KFLD                    KTSKSC                         *CLIENTE
182100980505     C                   KFLD                    KTSCTR                         *TARIFFA
182200980505     C                   KFLD                    KTSPRG                         *PROGRESSIVO
182300980505     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
182400980505     C                   KFLD                    KTSSGL                         *SCAGLIONE
182500011001     C* LETTURA TEETS01L - PARZIALE
182600970117     C     KE2ETS        KLIST
182700980505     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
182800980505     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
182900970117     C                   KFLD                    KTSKSC                         *CLIENTE
183000970117     C                   KFLD                    KTSCTR                         *TARIFFA
183100970117     C                   KFLD                    KTSPRG                         *PROGRESSIVO
183200970117     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
183300011026     C* LETTURA TNTAM01L - COMPLETA
183400011026     C     KEYTAM        KLIST
183500011026     C                   KFLD                    KTMKSC                         *CLIENTE
183600011026     C                   KFLD                    KTMCTR                         *TARIFFA
183700011026     C                   KFLD                    KTMPRG                         *PROGRESSIVO
183800970117     C*--------------------
183900970117     C* VARIABILI NON RIFERITE AL DATABASE
184000970117     C*--------------------
184100970117     C     *LIKE         DEFINE    NRR           SFLPAG                         *DI DEPOSITO
184200970117     C     *LIKE         DEFINE    NRR           SFLEOF
184300970318     C     *LIKE         DEFINE    NRR           DEPNRR
184400970117     C     *LIKE         DEFINE    V1SPSC        DUPPSC                         *DI DUPLICA
184500970117     C     *LIKE         DEFINE    V1SCSC        DUPCSC
184600970117     C     *LIKE         DEFINE    V1SVSC        DUPVSC
184700970317     C     *LIKE         DEFINE    V1SINL        DUPINL
184800970317     C     *LIKE         DEFINE    V1SRPV        DUPRPV
184900970402     C     *LIKE         DEFINE    V1SSDE        DUPSDE
185000970402     C     *LIKE         DEFINE    V1SPDE        DUPPDE
185100970117     C     *LIKE         DEFINE    V1SPPE        DUPPPE
185200970117     C*--------------------
185300970117     C* VARIABILI DI CONTROLLO
185400970117     C*--------------------
185500970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
185600970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
185700970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
185800970117     C                   MOVEL     '0'           WERR              1            *ERRORE
185900970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
186000970117     C                   Z-ADD     10            SFLPAG                         *RECORD X PAGINA
186100970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
186200970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
186300970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
186400970619     C                   MOVEL     'N'           WEETS             1            *ESISTENZA SGL
186500970117     C*
186600970117     C* REPERISCE LA DATA CORRENTE
186700970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
186800970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
186900970117     C                   Z-ADD     WN8           G08DAT
187000970117     C                   Z-ADD     *ZEROS        G08INV
187100970117     C                   MOVEL     '0'           G08ERR
187200970117     C                   CALL      'XSRDA8'
187300970117     C                   PARM                    WLBDA8
187400970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
187500970117     C*
187600970117     C* 02->GESTIONE, 05->INTERROGAZIONE
187700970117     C                   MOVE      D00OP0        WA2               2
187800970117IF  1C     WA2           IFEQ      '02'
187900970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
188000970117X   1C                   ELSE
188100970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
188200970117E   1C                   ENDIF
188300970117     C*
188400970117     C* CARICA LE TABELLE OCCORRENTI
188500970117     C                   EXSR      CARTAB
188600970117     C*
188700970117     C* CARICA I CAPICONTO DI CONTABILITA'
188800970117     C                   EXSR      CARCC
188900980505     C*
189000980505     C* CONTROLLI INIZIALI DI ENTRATA
189100980505     C                   EXSR      CTRENT
189200980505     C*
189300980505     C* CARICA GLI SCAGLIONI NELLE SCHIERE VIDEO
189400980505IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
189500980505     C                   EXSR      CARSGL
189600980505E   1C                   ENDIF
189700970117     C*
189800970117     C* CARICA GLI SCAGLIONI DEL DETTAGLIO TARIFFA
189900980505IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
190000980505IF  2C     D00TLA        IFEQ      'D'                                          *DELTA
190100970117     C                   EXSR      CARTAD
190200980505E   2C                   ENDIF
190300980505E   1C                   ENDIF
190400970117     C*
190500970117     C* IMPOSTA LA TESTATA
190600980505IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
190700970319     C                   EXSR      IMPVD0
190800980505E   1C                   ENDIF
190900970117     C*
191000970117     C                   ENDSR
191100970117     O*------------------------------------------------------------------------*
191200980505** -DDEC-
191300970117ITALIA              1
191400970117ESTERO              2
191500970117COLLI               3
191600970117 KG                 4
191700980505SEDE                5
191800980505FILIALE             6
1919009805057
1920009805057
1921009805059
192200980818TUTTE LE FILIALI    10
192300970117** -DVID-
192400970414A   Scaglione      Peso                                                       1
192500970414      XXXXX        XXXX                                                   Inc 2
1926009701173
1927009701174
1928009701175
192900970117** -DOPE-
193000970117* INSERIMENTO *1
193100970117* VARIAZIONE * 2
193200970117INTERROGAZIONE 3
193300970117 * GESTIONE *  4
1934009701175
193500980505** -MSG-
193600970117TNTE02R- Immettere lo scaglione                                               1
193700970117TNTE02R- Indicare il peso medio per spedizione                                2
193800980928TNTE02R- Indicare i colli medi per spedizione (minimo 1)                      3
193900970117TNTE02R- Indicare il volume medio per spedizione                              4
194000970117TNTE02R- Indicare l'incidenza % dello scaglione rispetto al totale spedizioni 5
194100970117TNTE02R- Scaglione già esistente                                              6
194200970117TNTE02R- Immettere almeno uno scaglione                                       7
194300970117TNTE02R- La % d'incidenza di tutti gli scaglioni deve essere uguale a 100     8
194400970117TNTE02R- Immettere 'A' per cancellare lo scaglione o lasciare vuoto           9
194500970117TNTE02R- Immettere gli scaglioni uguali a quelli della tariffa o viceversa    10
194600970117TNTE02R- Filiale errata                                                       11
194700970117TNTE02R- "T" per caricare i dati tariffa, "O" per caricare i dati offerta     12
194800970117TNTE02R- Dati tipo richiesti non trovati                                      13
194900970117TNTE02R- Non esistono i dati tipo per la tariffa/visita richiesta             14
195000970317TNTE02R- Non esistono spedizioni in provincia. Non inserire l'inoltro         15
195100970318TNTE02R- Il valore non può essere maggiore dello scaglione                    16
195200970318TNTE02R- Il valore non può essere minore dello scaglione precedente           17
195300970414TNTE02R- Segno delta richiesto errato ( /+/-).                                18
195400970414TNTE02R- Percentuale delta richiesto errata.                                  19
195500971008TNTE02R- Il peso dello scaglione non può superare i 9.999 kg                  20
195600980505TNTE02R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    21
195700980909TNTE02R- Scaglioni incongruenti con RPV.                                      22
195800011008TNTE02R- Impossibile inserire importi con decimali quando la divisa è ITL.    23
195801060825Il codice tariffa scelto incongruente con quello originale                    24
196000980505TNTE02R- 25
196100980505TNTE02R- 26
196200980505TNTE02R- 27
196300980505TNTE02R- 28
196400980505TNTE02R- 29
196500980505TNTE02R- 30
