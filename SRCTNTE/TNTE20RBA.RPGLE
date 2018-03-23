000100970117     H*------------------------------------------------------------------------*
000200970117     H* TARIFFE DA CONTO ECONOMICO - VISUALIZZA DELTA                          *
000300970117     H*------------------------------------------------------------------------*
000400970117     H DECEDIT('0,') DATEDIT(*DMY.)
000500970117     F*------------------------------------------------------------------------*
000600970117     F* DATA BASE
000700970117     F*------------------------------------------------------------------------*
000800970117     FTNTE20D   CF   E             WORKSTN
000900970117     F                                     SFILE(TE20S01:NR1)
001000970117     F                                     SFILE(TE20S02:NR2)
001100970117     F                                     SFILE(TE20S03:NR3)
001200970117     FTABEL00F  IF   E           K DISK
001300970117     FCNACO00F  IF   E           K DISK
001400041202     FTFACO00F  IF   E           K DISK    usropn
001500970512     F                                     RENAME(CNACO000:TFACO000)
001600041202     FTNVIS05L  IF   E           K DISK    usropn
001700970117     FAZORG01L  IF   E           K DISK
001800040811     F*EETC01L  IF   E           K DISK
001900011001     FTEETW02L  IF   E           K DISK
002000970117     FTNTE21P   O    E             PRINTER OFLIND(*IN01)
002100970117     F                                     USROPN
002200970117     D*------------------------------------------------------------------------*
002300970117     D* SCHIERE
002400970117     D*------------------------------------------------------------------------*
002500970117     D*-------------------
002600970117     D* DESCRIZIONE VIDEO
002700970117     D*-------------------
002800970117     D DVID            S             20    DIM(5) CTDATA PERRCD(1)
002900970117     D*-------------------
003000970117     D* MESSAGGI VIDEO
003100970117     D*-------------------
003200970117     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003300970117     D*-------------------
003400970117     D* RAGGRUPPAMENTI DELTA
003500970117     D*-------------------
003600970117     D RDL             S              3  0 DIM(20)                              *CODICE
003700970117     D DRD             S             30    DIM(20)                              *DESCRIZIONE
003800970117     D TIP             S              1    DIM(20)                              *TIPO
003900970117     D*-------------------
004000970117     D* TIPO SERVIZIO
004100970117     D*-------------------
004200970117     D TSP             S              1    DIM(10)                              *CODICE
004300970117     D DTSP            S             20    DIM(10)                              *DESCRIZIONE
004400970117     D*-------------------
004500970117     D* TIPI TARIFFA
004600970117     D*-------------------
004700970117     D TTR             S              1    DIM(10)                              *CODICE
004800970117     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
004900970117     D*-------------------
005000970117     D* CODICI TASSAZIONE
005100970117     D*-------------------
005200050103     D CTS             S              2    DIM(600)                             *CODICE
005300050103     D DCT             S             20    DIM(600)                             *DESCRIZIONE
005400050103     D COR             S              3    DIM(600)                             *ORDINAMENTO
005500050103     D NAR             S              3    DIM(600)                             *CODICE NAZIONE
005600050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)
005700970117     D*-------------------
005800970117     D* SCHIERE DI MEMORIZZAZIONE
005900970117     D*-------------------
006000970117     D*---
006100970117     D* TOTALE
006200970117     D*---
006300050103     D TCTS            S              2    DIM(600)                             *CODICE TASSAZIONE
006400050103     D TRAP            S              2    DIM(600)                             *CODICE REGIONE
006500050103     D TNAR            S              2    DIM(600)                             *CODICE NAZIONE
006600050103     D TRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO (T/R
006700970117     D TSGL            S             13  3 DIM(10)                              *SCAGLIONI
006800970117     D TRDL            S              3  0 DIM(30)                              *RAGGRUPPAMENTO DELT
006900970117     D* MULTIPLE          -10 SCAGLIONI-
007000050103     D* MULTIPLE          -600 CODICI TASSAZIONE
007100970117     D*---
007200970117     D* DI LAVORO
007300970117     D*---
007400970117     D* MULTIPLE          -10 SCAGLIONI-
007500970117     D*---
007600970117     D* OUTPUT - 1° SFILE
007700970117     D*---
007800050103     D CTSP            S              2    DIM(601)                             *CODICE TASSAZIONE
007900050103     D PPAP            S              7  4 DIM(601)                             *% PESO ASSOLUTA TOT
008000050103     D DCTP            S             20    DIM(601)                             *DESCRIZIONE
008100050103     D TCTP            S              1    DIM(601)                             *TIPO (E/R/N/*)
008200050103     D CORP            S              3    DIM(601)                             *ORDINAMENTO
008300050103     D COPP            S             15  5 DIM(601)                             *COMPETENZA PORTO
008400050103     D RIPP            S             15  5 DIM(601)                             *RICAVI     PORTO
008500050103     D DEPP            S              4  1 DIM(601)                             *DELTA      PORTO
008600050103     D COTP            S             15  5 DIM(601)                             *COMPETENZA PORTO +
008700050103     D RITP            S             15  5 DIM(601)                             *RICAVI     PORTO +
008800050103     D DETP            S              4  1 DIM(601)                             *DELTA      PORTO +
008900970117     D*---
009000970117     D* OUTPUT - 2° SFILE
009100970117     D*---
009200970117     D SGLS            S             13  3 DIM(11)                              *SCAGLIONE
009300011018     D COPS            S             15  5 DIM(11)                              *COMPETENZA PORTO
009400011018     D RIPS            S             15  5 DIM(11)                              *RICAVI     PORTO
009500970117     D DEPS            S              4  1 DIM(11)                              *DELTA      PORTO
009600011018     D COTS            S             15  5 DIM(11)                              *COMPETENZA PORTO +
009700011018     D RITS            S             15  5 DIM(11)                              *RICAVI     PORTO +
009800970117     D DETS            S              4  1 DIM(11)                              *DELTA      PORTO +
009900970117     D*---
010000970117     D* OUTPUT - 2° SFILE
010100970117     D*---
010200970117     D RDLT            S              3  0 DIM(31)                              *RAGGRUPPAMENTO DELT
010300011018     D COMT            S             15  5 DIM(31)                              *COMPETENZE
010400011018     D RICT            S             15  5 DIM(31)                              *RICAVI
010500970117     D DELT            S              4  1 DIM(31)                              *DELTA
010600970117     D*-------------------
010700020902     D* TISE65T - EDITAZIONE CAMPI ALFANUMERICI
010800970117     D*-------------------
010900970117     D $N$             S              1    DIM(16)                              *NUMERO DA EDITARE
011000970117     D $A$             S              1    DIM(22)                              *NUMERO EDITATO
011100970117     D*------------------
011200970117     D* TABELLE CAPICONTO
011300970117     D*------------------
011400970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
011500970117     D*------------------------------------------------------------------------*
011600970117     D* INPUT
011700970117     D*------------------------------------------------------------------------*
011800970117     D*------------------
011900970117     D* DS SCHIERE MULTIPLE
012000970117     D*------------------
012100970117     D*---
012200970117     D* BYTE IN PACKED ->
012300050103     D* (7:2)+1=4 ; (4X600) =2400
012400050103     D* (15:2)+1=8 ; (8X600) =4800
012500011018     D* (15:2)+1=8 ; (8X 30) = 240
012600970117     D*---
012700970117     D* -10 SCAGLIONI-
012800970117     D DSGL            DS                  OCCURS(10) INZ
012900050103     D  TPPA                   1   2400P 4
013000050103     D                                     DIM(600)                             *% PESO ASSOLUTA
013100050103     D  TCOP                2401   7200P 5
013200050103     D                                     DIM(600)                             *COMPETENZA PORTO
013300050103     D  TRIP                7201  12000P 5
013400050103     D                                     DIM(600)                             *RICAVI     PORTO
013500050103     D  TCOT               12001  16800P 5
013600050103     D                                     DIM(600)                             *COMPETENZA PORTO+AC
013700050103     D  TRIT               16801  21600P 5
013800050103     D                                     DIM(600)                             *RICAVI     PORTO+AC
013900050103     D* -600 CODICI TASSAZIONE-
014000050103     D DRDL            DS                  OCCURS(600) INZ
014100011018     D  TCO1                   1    240P 5
014200011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 1
014300011018     D  TRI1                 241    480P 5
014400011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 1
014500011018     D  TCO2                 481    720P 5
014600011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 2
014700011018     D  TRI2                 721    960P 5
014800011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 2
014900011018     D  TCO3                 961   1200P 5
015000011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 3
015100011018     D  TRI3                1201   1440P 5
015200011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 3
015300011018     D  TCO4                1441   1680P 5
015400011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 4
015500011018     D  TRI4                1681   1920P 5
015600011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 4
015700011018     D  TCO5                1921   2160P 5
015800011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 5
015900011018     D  TRI5                2161   2400P 5
016000011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 5
016100011018     D  TCO6                2401   2640P 5
016200011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 6
016300011018     D  TRI6                2641   2880P 5
016400011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 6
016500011018     D  TCO7                2881   3120P 5
016600011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 7
016700011018     D  TRI7                3121   3360P 5
016800011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 7
016900011018     D  TCO8                3361   3600P 5
017000011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 8
017100011018     D  TRI8                3601   3840P 5
017200011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 8
017300011018     D  TCO9                3841   4080P 5
017400011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 9
017500011018     D  TRI9                4081   4320P 5
017600011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 9
017700011018     D  TCO0                4321   4560P 5
017800011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 10
017900011018     D  TRI0                4561   4800P 5
018000011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 10
018100970117     D* -10 SCAGLIONI-
018200970117     D DLAV            DS                  OCCURS(10) INZ
018300050103     D  PPAS                   1   2404P 4
018400050103     D                                     DIM(601)                             *% PESO ASSOLUTA TOT
018500970117     D*-------------------
018600970117     D* DS TIPI SERVIZIO
018700970117     D*-------------------
018800970117     D DS5E          E DS
018900970117     D*-------------------
019000970117     D* DS TIPI TARIFFA
019100970117     D*-------------------
019200970117     D DSTR          E DS
019300040906     D*-------------------
019400040906     D* DS nazioni
019500040906     D*-------------------
019600040906     D DS15          E DS
019700970117     D*-------------------
019800970117     D* DS CODICI TASSAZIONE
019900970117     D*-------------------
020000970117     D DSCT          E DS
020100970117     D*-------------------
020200970117     D* DS RAGGRUPPAMENTI DELTA
020300970117     D*-------------------
020400970117     D DS9B          E DS
020500970117     D*-------------------
020600970117     D* DS PASSAGGIO PARAMETRI
020700970117     D*-------------------
020800970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
020900970117     D*------------------
021000970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
021100970117     D*------------------
021200970117     D WLBDA8          DS
021300970117     D  G08DAT                 1      8  0
021400970117     D  G08INV                 9     16  0
021500970117     D  G08ERR                17     17
021600970117     D  G08TGI                18     22  0
021700970117     D*------------------
021800020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
021900970117     D*------------------
022000970117     D CARCNT          DS
022100970117     D  FL12CN                 1      2
022200970117     D  FL34CN                 3      4
022300970117     D  TIPCNT                 5      6
022400020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
022500970117     D  FLU                   66     97
022600970117     D                                     DIM(32)                              FLAG PAR.UTENT
022700970117     D  TCU                  398    697
022800970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
022900970117     D  KCU                  698    847P 0
023000970117     D                                     DIM(50)                              *TAB CAPICONTO
023100970117     D                                     PACKEVEN
023200970117     D TCUDS           DS
023300970117     D  E1                     1      1
023400970117     D  E3                     3      3
023500970117     D  E2                     2      2
023600970117     D  E4                     4      4
023700970117     D  E56                    5      6
023800970213     D*-------------------
023900970213     D* INFDS - INFORMAZIONI PROGRAMMA
024000970213     D*-------------------
024100970213     D                SDS
024200980909     D  JOB                  244    253
024300970213     D  USER                 254    263
024400970117     D*-------------------
024500970117     D* ARCHITETTURA
024600970117     D*-------------------
024700970117     D KPJBA         E DS
024800970117     C*------------------------------------------------------------------------*
024900970117     C* MAIN LINE
025000970117     C*------------------------------------------------------------------------*
025100970117     C*
025200970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
025300970117IF  1C     $FINE         IFNE      'S'
025400970117     C*
025500970117     C* IMPOSTAZIONI PRIMO SUBFILE
025600970117     C                   EXSR      IMPSF1
025700970117     C*
025800970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
025900970117DO  2C     $FINE         DOUEQ     'S'
026000970117     C     TIPVID        CASEQ     '1'           GESSF1                         *1° VIDEO
026100970117     C     TIPVID        CASEQ     '2'           GESSF2                         *2° VIDEO
026200970117     C     TIPVID        CASEQ     '3'           GESSF3                         *3° VIDEO
026300970117     C                   ENDCS
026400970117E   2C                   ENDDO
026500970117E   1C                   ENDIF
026600970117     C*
026700970117     C                   SETON                                        LR
026800970117     C*------------------------------------------------------------------------*
026900970117     C* IMPSF1 - IMPOSTAZIONI INIZIALI PRIMO SUBFILE
027000970117     C*------------------------------------------------------------------------*
027100970117     C     IMPSF1        BEGSR
027200970117     C*
027300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
027400970117     C                   EXSR      SETSF1                                       *SFILE
027500970117     C* CARICA LE SCHIERE
027600970117     C                   EXSR      MS1OUT
027700970117     C* CARICA IL SUBFILE
027800970117     C                   EXSR      POSSF1
027900970117     C*
028000970117     C                   ENDSR
028100970117     C*------------------------------------------------------------------------*
028200970117     C* SETSF1 - IMPOSTAZIONI INDICATORI PRIMO SUBFILE
028300970117     C*------------------------------------------------------------------------*
028400970117     C     SETSF1        BEGSR
028500970117     C*
028600970117     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
028700970117     C                   MOVEL     '1'           DEPVID                         *VIDEO IN GESTIONE
028800970117     C                   SETOFF                                       21         ERRORI SFILE
028900970117     C                   SETOFF                                       90         GENERICO DI ERRORE
029000970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
029100970117     C*
029200970117     C                   ENDSR
029300970117     C*------------------------------------------------------------------------*
029400970117     C* GESSF1 - GESTIONE PRIMO SUBFILE
029500970117     C*------------------------------------------------------------------------*
029600970117     C     GESSF1        BEGSR
029700970117     C*
029800970117     C* EMISSIONE VIDEATA
029900970117IF  1C     $TESVD        IFEQ      '1'
030000970117     C                   WRITE     TE20D00                                      *TESTATA
030100970117     C                   MOVEL     '0'           $TESVD
030200970117E   1C                   ENDIF
030300970117     C                   WRITE     TE20Z01                                      *PIEDI
030400970117     C                   EXFMT     TE20C01                                      *SFILE
030500970117     C                   EXSR      SETSF1
030600970117     C* F03-FINE
030700970117IF  1C     *INKC         IFEQ      *ON
030800970117     C                   EXSR      F03GES
030900970117     C                   GOTO      FINSF1
031000970117E   1C                   ENDIF
031100970117     C* F12-RITORNO
031200970117IF  1C     *INKL         IFEQ      *ON
031300970117     C                   EXSR      F12GES
031400970117     C                   GOTO      FINSF1
031500970117E   1C                   ENDIF
031600970117     C* F21-STAMPA DELTA
031700970117IF  1C     *INKV         IFEQ      *ON
031800970117     C                   EXSR      F21GES
031900970117     C                   GOTO      FINSF1
032000970117E   1C                   ENDIF
032100970117     C* F22-STAMPA DATI TIPO
032200970117IF  1C     *INKW         IFEQ      *ON
032300970117     C                   EXSR      F22GES
032400970117     C                   GOTO      FINSF1
032500970117E   1C                   ENDIF
032600970117     C* CONTROLLA VIDEO
032700970117     C                   EXSR      CTRSF1
032800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
032900970117     C                   GOTO      FINSF1
033000970117E   1C                   ENDIF
033100970117     C* ENTER-RIMANE
033200970117IF  1C     $PC1          IFEQ      *ZEROS                                       *SE FUORI DAL SFILE
033300970117     C                   Z-ADD     1             $PC1                           *SI POSIZIONA AL 1°
033400970117X   1C                   ELSE
033500970117     C                   Z-ADD     $PC1          $NR1                           *SI POSIZIONA DOV'E'
033600970117E   1C                   ENDIF
033700970117     C*
033800970117     C     FINSF1        ENDSR
033900970117     C*------------------------------------------------------------------------*
034000970117     C* CTRSF1 - CONTROLLA PRIMO SUBFILE
034100970117     C*------------------------------------------------------------------------*
034200970117     C     CTRSF1        BEGSR
034300970117     C*
034400970117     C* REIMPOSTA VARIABILI DI LAVORO
034500970117     C                   MOVEL     '0'           WERR                           *ERRORE
034600970117     C*
034700970117     C* LEGGE TUTTI I RECORD DEL SFILE
034800970117     C                   Z-ADD     1             NR1
034900970117     C     NR1           CHAIN     TE20S01                            98
035000970117DO  1C     *IN98         DOWEQ     *OFF
035100970117     C                   SETOFF                                       80        *NO SFLNXTCHG
035200970117     C   10              MOVEL     DU1OPZ        V1SOPZ                         *DI DUPLICA
035300970117     C*--------------------
035400970117     C* OPZIONE
035500970117     C*--------------------
035600970117IF  2C     WERR          IFEQ      '0'
035700970117IF  3C     V1SOPZ        IFEQ      *BLANKS
035800970117     C     V1SOPZ        OREQ      '5 '
035900970117     C                   ELSE
036000970117     C                   MOVEL     '1'           WERR
036100970117     C                   SETON                                        2190
036200970117     C                   MOVEL     MSG(11)       $MSG
036300970117E   3C                   ENDIF
036400970117E   2C                   ENDIF
036500970117     C*
036600970117IF  2C     WERR          IFEQ      '1'
036700970117     C                   Z-ADD     NR1           $NR1                           *POSIZIONAMENTO
036800970117E   2C                   ENDIF
036900970117     C* GESTIONE OPZIONI
037000970117     C                   EXSR      GESOP1
037100970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
037200970117     C                   GOTO      FINCT1                                       *ESCE
037300970117X   2C                   ELSE                                                   *N O N  ERRORE
037400970117     C                   MOVEL     V1SOPZ        DU1OPZ                         *DI DUPLICA
037500970117     C                   ADD       1             NR1                            *LEGGE ALTRO RECORD
037600970117     C     NR1           CHAIN     TE20S01                            98
037700970117E   2C                   ENDIF
037800970117E   1C                   ENDDO                                                  *FINE SUBFILE
037900970117     C*
038000970117     C     FINCT1        ENDSR
038100970117     C*------------------------------------------------------------------------*
038200970117     C* GESOP1 - GESTIONE OPZIONI DEL PRIMO SUBFILE
038300970117     C*------------------------------------------------------------------------*
038400970117     C     GESOP1        BEGSR
038500970117     C*
038600970117SL  1C                   SELECT
038700970117     C     V1SOPZ        WHENEQ    '5 '
038800970117     C                   EXSR      O05GE1                                       *VISUALIZZA SCAGLION
038900970117     C                   OTHER
039000970117E   1C                   ENDSL
039100970117     C*
039200970117     C                   ENDSR
039300970117     C*------------------------------------------------------------------------*
039400970117     C* O05GE1 - GESTIONE OPZIONE PRIMO SFILE 5 ==> VISUALIZZA SCAGLIONI
039500970117     C*------------------------------------------------------------------------*
039600970117     C     O05GE1        BEGSR
039700970117     C*
039800970117     C* AGGIORNA 1° SFILE
039900970117     C                   EXSR      AGGSF1
040000970117     C*
040100970117     C* GESTIONE 2° SFILE
040200970117     C                   Z-ADD     NR1           $DPC1                          *DEPOSITO NRR
040300970117     C                   EXSR      IMPSF2                                       *IMPOSTAZIONI
040400970117     C                   EXSR      GESSF2                                       *GESTIONE
040500970117     C                   Z-ADD     $DPC1         NR1                            *DEPOSITO NRR
040600970117     C*
040700970117     C                   ENDSR
040800970117     C*------------------------------------------------------------------------*
040900970117     C* AGGSF1 - AGGIORNA RIGA DEL PRIMO SUBFILE
041000970117     C*------------------------------------------------------------------------*
041100970117     C     AGGSF1        BEGSR
041200970117     C*
041300970117     C                   MOVEL     *BLANKS       V1SOPZ
041400970117     C                   SETON                                        80        *SI SFLNXTCHG
041500970117     C                   UPDATE    TE20S01                                      *AGGIORNA SUBFILE
041600970117     C                   SETOFF                                       80        *NO SFLNXTCHG
041700970117     C*
041800970117     C                   ENDSR
041900970117     C*------------------------------------------------------------------------*
042000970117     C* F03GES - GESTIONE TASTO PRIMO SFILE F03 ==> FINE
042100970117     C*------------------------------------------------------------------------*
042200970117     C     F03GES        BEGSR
042300970117     C*
042400970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
042500970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
042600970117     C*
042700970117     C                   ENDSR
042800970117     C*------------------------------------------------------------------------*
042900970117     C* F12GES - GESTIONE TASTO PRIMO SFILE F12 ==> RITORNO
043000970117     C*------------------------------------------------------------------------*
043100970117     C     F12GES        BEGSR
043200970117     C*
043300970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
043400970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
043500970117     C*
043600970117     C                   ENDSR
043700970117     C*------------------------------------------------------------------------*
043800970117     C* F21GES - GESTIONE TASTO PRIMO SFILE F21 ==> STAMPA DELTA
043900970117     C*------------------------------------------------------------------------*
044000970418     C     F21GES        BEGSR
044100970418     C*
044200970418     C* RICHIESTA PARAMETRI DI LANCIO
044300970418     C                   EXSR      IMPWD1
044400970418DO  1C     $FINV         DOUEQ     'S'
044500970418     C                   EXSR      GESWD1                                       *GESTIONE FINESTRA
044600970418E   1C                   ENDDO
044700970418     C*
044800970117     C* IMPOSTA VARIABILI DI LAVORO
044900970418IF  1C     $CONF         IFEQ      'S'                                          *STAMPA CONFERMATA
045000970117     C                   OPEN      TNTE21P
045100970418     C                   MOVEL     '1'           *IN01                          *FINE PAGINA
045200970117     C*
045300970117     C* PER CIASCUN CODICE TASSAZIONE EMESSO A VIDEO
045400050103DO  2C     1             DO        601           A                 4 0
045500970418IF  3C     CTSP(A)       IFNE      *BLANKS
045600970117     C                   MOVEL     '0'           $STMC                          *STAMPARE COD.TASS.
045700970117     C*
045800970117     C* MEMORIZZA I DATI PER SCAGLIONE
045900970117     C                   MOVEL     CTSP(A)       WCTS
046000970117     C                   Z-ADD     RIPP(A)       WRIP
046100970117     C                   Z-ADD     COPP(A)       WCOP
046200970117     C                   Z-ADD     DEPP(A)       WDEP
046300970117     C                   Z-ADD     RITP(A)       WRIT
046400970117     C                   Z-ADD     COTP(A)       WCOT
046500970117     C                   Z-ADD     DETP(A)       WDET
046600970117     C                   EXSR      MS2OUT
046700970117     C*
046800970117     C* PER CIASCUN SCAGLIONE MEMORIZZATO
046900970418DO  4C     1             DO        11            B                 4 0
047000970418IF  5C     COTS(B)       IFGT      *ZEROS
047100970117     C     RITS(B)       ORGT      *ZEROS
047200970117     C                   MOVEL     '0'           $STMS                          *STAMPARE SCAGLIONE
047300970117     C*
047400970117     C* MEMORIZZA I DATI PER SCAGLIONE
047500970117     C                   MOVEL     CTSP(A)       WCTS
047600970117     C                   Z-ADD     SGLS(B)       WSGL
047700970117     C                   Z-ADD     COTS(B)       WCOM
047800970117     C                   Z-ADD     RITS(B)       WRIC
047900970117     C                   Z-ADD     DETS(B)       WDEL
048000970117     C                   EXSR      MS3OUT
048100970117     C*
048200970117     C* PER CIASCUNA VOCE MEMORIZZATA
048300970418DO  6C     1             DO        30            D                 4 0
048400970418     C                   MOVEL     '0'           $STMV                          *STAMPARE VOCE
048500970418IF  7C     COMT(D)       IFGT      *ZEROS
048600970117     C     RICT(D)       ORGT      *ZEROS
048700970117     C*
048800970319     C* STAMPA RIGA DI DETTAGLIO
048900970117     C                   EXSR      STADET
049000970117     C*
049100970418E   7C                   ENDIF
049200970418E   6C                   ENDDO                                                  *FINE VOCI
049300970418E   5C                   ENDIF
049400970418E   4C                   ENDDO                                                  *FINE SCAGLIONI
049500970418E   3C                   ENDIF
049600970418E   2C                   ENDDO                                                  *FINE COD.TASSAZIONE
049700970117     C*
049800970117     C* STAMPA RIGA DI FINE PROSPETTO
049900970117     C                   WRITE     TE21FI
050000970117     C                   CLOSE     TNTE21P
050100970418E   1C                   ENDIF
050200970117     C*
050300970117     C                   ENDSR
050400970117     C*------------------------------------------------------------------------*
050500970117     C* STADET - STAMPA RIGA DI DETTAGLIO
050600970117     C*------------------------------------------------------------------------*
050700970117     C     STADET        BEGSR
050800970418     C*
050900970418     C* REIMPOSTA VARIABILI DI LAVORO
051000970418     C                   MOVEL     '0'           $STMRD            1            *RIGA DA NON STAMPAR
051100970117     C*
051200970117     C* FINE PAGINA, STAMPA LA TESTATA
051300970117IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
051400970117     C                   WRITE     TE21TE                                       *TESTATA
051500970117     C                   WRITE     TE21IN                                       *INTESTAZIONE
051600970117     C                   WRITE     TE21I2                                       *INTESTAZIONE CAMPI
051700970117     C                   SETOFF                                       01
051800970418     C                   MOVEL     '0'           $STMC                          *COD.TASS. DA STAMP
051900970418     C                   MOVEL     '0'           $STMS                          *SCAGLIONE DA STAMP
052000970418     C                   MOVEL     '0'           $STMV                          *VOCE      DA STAMP
052100970117E   1C                   ENDIF
052200970117     C*
052300970117     C* IMPOSTA I CAMPI -CODICE TASSAZIONE-
052400970117IF  1C     $STMC         IFEQ      '0'
052500970117     C                   MOVEL     WCTS          DACCTS                         *CAMPI
052600970117     C                   MOVEL     DCTP(A)       DADCTS
052700970117     C                   Z-ADD     WRIP          DACCRP
052800970117     C                   Z-ADD     WCOP          DACCCP
052900970117     C                   Z-ADD     WDEP          DACCDP
053000970117     C                   Z-ADD     WRIT          DACCRT
053100970117     C                   Z-ADD     WCOT          DACCCT
053200970117     C                   Z-ADD     WDET          DACCDT
053300970418     C                   MOVEL     '1'           $STMC                          *NON STAMP.COD.TASS.
053400970418     C                   MOVEL     '1'           $STMRD                         *RIGA DA STAMPARE
053500970117X   1C                   ELSE
053600970117     C                   MOVEL     *BLANKS       DACCTS                         *CAMPI
053700970117     C                   MOVEL     *BLANKS       DADCTS
053800970117     C                   Z-ADD     *ZEROS        DACCRP
053900970117     C                   Z-ADD     *ZEROS        DACCCP
054000970117     C                   Z-ADD     *ZEROS        DACCDP
054100970117     C                   Z-ADD     *ZEROS        DACCRT
054200970117     C                   Z-ADD     *ZEROS        DACCCT
054300970117     C                   Z-ADD     *ZEROS        DACCDT
054400970117E   1C                   ENDIF
054500970117     C*
054600970117     C* IMPOSTA I CAMPI -SCAGLIONE-
054700970418IF  1C     $STMS         IFEQ      '0'                                          *SGL DA STAMPARE
054800970418IF  2C     SGLS(B)       IFGT      *ZEROS                                       *CAMPI
054900970117     C                   Z-ADD     SGLS(B)       $NUM             16 3          *NUMERO DA EDITARE
055000970117     C                   Z-ADD     3             $DEC              1 0          *NUMERO DECIMALI
055100020902    ? /COPY GAITRASRC/SRCTISE,TISE65T
055200970117     C                   MOVE(P)   $ALF          DACSGL                         *NUMERO EDITATO
055300970418X   2C                   ELSE
055400970117     C                   MOVE(P)   'TOTALE'      DACSGL
055500970418E   2C                   ENDIF
055600970117     C                   Z-ADD     RIPS(B)       DACSRP
055700970117     C                   Z-ADD     COPS(B)       DACSCP
055800970117     C                   Z-ADD     DEPS(B)       DACSDP
055900970117     C                   Z-ADD     RITS(B)       DACSRT
056000970117     C                   Z-ADD     COTS(B)       DACSCT
056100970117     C                   Z-ADD     DETS(B)       DACSDT
056200970418     C                   MOVEL     '1'           $STMS                          *NON STAMP.SCAGLIONE
056300970418     C                   MOVEL     '1'           $STMRD                         *RIGA DA STAMPARE
056400970418X   1C                   ELSE
056500970117     C                   MOVEL     *BLANKS       DACSGL                         *CAMPI
056600970117     C                   Z-ADD     *ZEROS        DACSRP
056700970117     C                   Z-ADD     *ZEROS        DACSCP
056800970117     C                   Z-ADD     *ZEROS        DACSDP
056900970117     C                   Z-ADD     *ZEROS        DACSRT
057000970117     C                   Z-ADD     *ZEROS        DACSCT
057100970117     C                   Z-ADD     *ZEROS        DACSDT
057200970418E   1C                   ENDIF
057300970117     C*
057400970117     C* IMPOSTA I CAMPI -VOCE-
057500970418IF  1C     $STMV         IFEQ      '0'                                          *VOCE DA STAMPARE
057600970418     C     V1WSDV        ANDEQ     'S'                                          *RICHIESTA STAMP VOC
057700970117     C                   Z-ADD     RDLT(D)       DACRCT                         *CAMPI
057800970117     C                   Z-ADD     RICT(D)       DACRIC
057900970117     C                   Z-ADD     COMT(D)       DACCOM
058000970117     C                   Z-ADD     DELT(D)       DACDEL
058100970117     C                   MOVEL     *BLANKS       DADRCT                         *DECODIFICHE
058200970319IF  2C     RDLT(D)       IFGT      *ZEROS
058300970117     C                   Z-ADD     1             J
058400970117     C                   SETOFF                                           98
058500970117     C     RDLT(D)       LOOKUP    RDL(J)                                 98
058600970319IF  3C     *IN98         IFEQ      *ON
058700970117     C                   MOVEL     DRD(J)        DADRCT
058800970319E   3C                   ENDIF
058900970319X   2C                   ELSE
059000970117     C                   MOVEL(P)  'TOTALE'      DADRCT
059100970319E   2C                   ENDIF
059200970319X   1C                   ELSE                                                   *NO DETTAGLIO VOCI
059300970319     C                   Z-ADD     *ZEROS        DACRCT
059400970319     C                   MOVEL     *BLANKS       DADRCT                         *DECODIFICHE
059500970319     C                   Z-ADD     *ZEROS        DACRIC
059600970319     C                   Z-ADD     *ZEROS        DACCOM
059700970319     C                   Z-ADD     *ZEROS        DACDEL
059800970319E   1C                   ENDIF
059900970418     C*
060000970418IF  1C     $STMRD        IFEQ      '1'                                          *RIGA DA STAMPARE
060100970418     C     $STMRD        OREQ      '0'                                          *RIGA DA NON STAMPAR
060200970418     C     V1WSDV        ANDEQ     'S'                                          *MA RICH. STAMPA VOC
060300970117     C                   WRITE     TE21DA                                       *DETTAGLIO
060400970418E   1C                   ENDIF
060500970117     C*
060600970117     C                   ENDSR
060700970418     C*------------------------------------------------------------------------*
060800970418     C* IMPWD1 - IMPOSTAZIONI PRIMA FINESTRA
060900970418     C*------------------------------------------------------------------------*
061000970418     C     IMPWD1        BEGSR
061100970418     C*
061200970418     C* REIMPOSTA FORMATO VIDEO E INDICATORI
061300970418     C                   MOVEL     'N'           $FINV                          *FINE     FINESTRA
061400970418     C                   MOVEL     'S'           $CONF                          *CONFERMA FINESTRA
061500970512     C                   MOVEL     'N'           V1WSDV
061600970418     C                   EXSR      SETWD1
061700970418     C*
061800970418     C                   ENDSR
061900970418     C*------------------------------------------------------------------------*
062000970418     C* SETWD1 - IMPOSTAZIONI INDICATORI PRIMA FINESTRA
062100970418     C*------------------------------------------------------------------------*
062200970418     C     SETWD1        BEGSR
062300970418     C*
062400970418     C                   SETOFF                                       30         ERRORI
062500970418     C                   SETOFF                                       90         GENERICO DI ERRORE
062600970418     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
062700970418     C*
062800970418     C                   ENDSR
062900970418     C*------------------------------------------------------------------------*
063000970418     C* GESWD1 - GESTIONE PRIMA FINESTRA
063100970418     C*------------------------------------------------------------------------*
063200970418     C     GESWD1        BEGSR
063300970418     C*
063400970418     C* EMISSIONE VIDEATA
063500970418     C                   EXFMT     TE20W01
063600970418     C                   EXSR      SETWD1
063700970418     C* F12-RITORNO
063800970418IF  1C     *INKL         IFEQ      *ON
063900970418     C                   MOVEL     'S'           $FINV                          *FINE     FINESTRA
064000970418     C                   MOVEL     'N'           $CONF                          *CONFERMA FINESTRA
064100970418     C                   GOTO      FINWD1
064200970418E   1C                   ENDIF
064300970418     C* CONTROLLA VIDEO
064400970418     C                   EXSR      CTRWD1
064500970418IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
064600970418     C                   GOTO      FINWD1
064700970418E   1C                   ENDIF
064800970418     C* INVIO-CONFERMA
064900970418     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
065000970418     C*
065100970418     C     FINWD1        ENDSR
065200970418     C*------------------------------------------------------------------------*
065300970418     C* CTRWD1 - CONTROLLA PRIMA FINESTRA
065400970418     C*------------------------------------------------------------------------*
065500970418     C     CTRWD1        BEGSR
065600970418     C*--------------------
065700970418     C* STAMPA DETTAGLIO VOCI
065800970418     C*--------------------
065900970418IF  1C     V1WSDV        IFNE      'S'
066000970418     C     V1WSDV        ANDNE     'N'
066100970418     C                   SETON                                        3090
066200970418     C                   MOVEL     MSG(12)       $MSG
066300970418     C                   GOTO      FINCW1
066400970418E   1C                   ENDIF
066500970418     C*
066600970418     C     FINCW1        ENDSR
066700970117     C*------------------------------------------------------------------------*
066800970117     C* F22GES - GESTIONE TASTO PRIMO SFILE F22 ==> STAMPA DATI TIPO
066900970117     C*------------------------------------------------------------------------*
067000970117     C     F22GES        BEGSR
067100970117     C*
067200970117     C                   MOVEL     'F22'         D00OP1                         *LIVELLO PROGRAMMA
067300970117     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
067400970117     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
067500970117     C                   MOVEL     '0'           D00ERR                         *ERRORE
067600970117     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
067700970418     C*
067800970418     C                   MOVEL     *BLANKS       D00SCT                         *RAPPRESENTAZIONE CT
067900970418     C                   MOVEL     D00FIE        D00SIE                         *TARIFFA ITA/EST
068000980708     C                   MOVEL     D00DSF        D00SSF                         *TARIFFA ITA/EST
068100970117     C*
068200970117     C                   MOVEL     'TE43'        KCOAZ
068300970117     C                   MOVEL     DSTE00        KPJBU
068400970117     C                   CALL      'BCH10'
068500970117     C                   PARM                    KPJBA
068600970117     C*
068700970117     C                   ENDSR
068800970117     C*------------------------------------------------------------------------*
068900970117     C* IMPSF2 - IMPOSTAZIONI INIZIALI SECONDO SUBFILE
069000970117     C*------------------------------------------------------------------------*
069100970117     C     IMPSF2        BEGSR
069200970117     C*
069300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
069400970117     C                   EXSR      SETSF2                                       *SFILE
069500970117     C*
069600970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
069700970117     C                   EXSR      SETSC2                                       *SFILE CONTROL
069800970117     C* CARICA LE SCHIERE
069900970117     C     $DPC1         CHAIN     TE20S01                            98        *RILEGGE COD.TASS.
070000970117     C                   MOVEL     V1SCTS        WCTS
070100970117     C                   Z-ADD     V1SCOP        WCOP
070200970117     C                   Z-ADD     V1SRIP        WRIP
070300970117     C                   Z-ADD     V1SDEP        WDEP
070400970117     C                   Z-ADD     V1SCOT        WCOT
070500970117     C                   Z-ADD     V1SRIT        WRIT
070600970117     C                   Z-ADD     V1SDET        WDET
070700970117     C                   EXSR      MS2OUT
070800970117     C* CARICA IL SUBFILE
070900970117     C                   EXSR      POSSF2
071000970117     C*
071100970117     C                   ENDSR
071200970117     C*------------------------------------------------------------------------*
071300970117     C* SETSF2 - IMPOSTAZIONI INDICATORI SECONDO SUBFILE
071400970117     C*------------------------------------------------------------------------*
071500970117     C     SETSF2        BEGSR
071600970117     C*
071700970117     C                   MOVEL     '2'           TIPVID                         *VIDEO IN GESTIONE
071800970117     C                   MOVEL     '2'           DEPVID                         *VIDEO IN GESTIONE
071900970117     C                   SETOFF                                       22         ERRORI SFILE
072000970117     C                   SETOFF                                       90         GENERICO DI ERRORE
072100970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
072200970117     C*
072300970117     C                   ENDSR
072400970117     C*------------------------------------------------------------------------*
072500970117     C* SETSC2 - IMPOSTAZIONI INDICATORI E CAMPI SECONDO SUBFILE CONTROL
072600970117     C*------------------------------------------------------------------------*
072700970117     C     SETSC2        BEGSR
072800970117     C*
072900970117     C                   MOVEL     V1SCTS        V2CCTS
073000970117     C                   MOVEL     V1SDCT        V2CDCT
073100970117     C*
073200970117     C                   ENDSR
073300970117     C*------------------------------------------------------------------------*
073400970117     C* GESSF2 - GESTIONE SECONDO SUBFILE
073500970117     C*------------------------------------------------------------------------*
073600970117     C     GESSF2        BEGSR
073700970117     C*
073800970117     C* EMISSIONE VIDEATA
073900970117IF  1C     $TESVD        IFEQ      '1'
074000970117     C                   WRITE     TE20D00                                      *TESTATA
074100970117     C                   MOVEL     '0'           $TESVD
074200970117E   1C                   ENDIF
074300970117     C                   WRITE     TE20Z02                                      *PIEDI
074400970117     C                   EXFMT     TE20C02                                      *SFILE
074500970117     C                   EXSR      SETSF2
074600970117     C* F03-FINE
074700970117IF  1C     *INKC         IFEQ      *ON
074800970117     C                   EXSR      F03GES
074900970117     C                   GOTO      FINSF2
075000970117E   1C                   ENDIF
075100970117     C* F12-RITORNO
075200970117IF  1C     *INKL         IFEQ      *ON
075300970117     C                   MOVEL     '1'           TIPVID
075400970117     C                   GOTO      FINSF2
075500970117E   1C                   ENDIF
075600970117     C* CONTROLLA VIDEO
075700970117     C                   EXSR      CTRSF2
075800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
075900970117     C                   GOTO      FINSF2
076000970117E   1C                   ENDIF
076100970117     C* ENTER-RITORNA
076200970117     C                   MOVEL     '1'           TIPVID
076300970117IF  1C     DEPVID        IFEQ      '3'
076400970117     C                   MOVEL     '2'           TIPVID
076500970117E   1C                   ENDIF
076600970117     C*
076700970117     C     FINSF2        ENDSR
076800970117     C*------------------------------------------------------------------------*
076900970117     C* CTRSF2 - CONTROLLA SECONDO SUBFILE
077000970117     C*------------------------------------------------------------------------*
077100970117     C     CTRSF2        BEGSR
077200970117     C*
077300970117     C* REIMPOSTA VARIABILI DI LAVORO
077400970117     C                   MOVEL     '0'           WERR                           *ERRORE
077500970117     C*
077600970117     C* LEGGE TUTTI I RECORD DEL SFILE
077700970117     C                   Z-ADD     1             NR2
077800970117     C     NR2           CHAIN     TE20S02                            98
077900970117DO  1C     *IN98         DOWEQ     *OFF
078000970117     C                   SETOFF                                       70        *NO SFLNXTCHG
078100970117     C   11              MOVEL     DU2OPZ        V2SOPZ                         *DI DUPLICA
078200970117     C*--------------------
078300970117     C* OPZIONE
078400970117     C*--------------------
078500970117IF  2C     WERR          IFEQ      '0'
078600970117IF  3C     V2SOPZ        IFEQ      *BLANKS
078700970117     C     V2SOPZ        OREQ      '5 '
078800970117     C                   ELSE
078900970117     C                   MOVEL     '1'           WERR
079000970117     C                   SETON                                        2290
079100970117     C                   MOVEL     MSG(11)       $MSG
079200970117E   3C                   ENDIF
079300970117E   2C                   ENDIF
079400970117     C*
079500970117IF  2C     WERR          IFEQ      '1'
079600970117     C                   Z-ADD     NR2           $NR2                           *POSIZIONAMENTO
079700970117E   2C                   ENDIF
079800970117     C* GESTIONE OPZIONI
079900970117     C                   EXSR      GESOP2
080000970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
080100970117     C                   GOTO      FINCT2                                       *ESCE
080200970117X   2C                   ELSE                                                   *N O N  ERRORE
080300970117     C                   MOVEL     V2SOPZ        DU2OPZ                         *DI DUPLICA
080400970117     C                   ADD       1             NR2                            *LEGGE ALTRO RECORD
080500970117     C     NR2           CHAIN     TE20S02                            98
080600970117E   2C                   ENDIF
080700970117E   1C                   ENDDO                                                  *FINE SUBFILE
080800970117     C*
080900970117     C     FINCT2        ENDSR
081000970117     C*------------------------------------------------------------------------*
081100970117     C* GESOP2 - GESTIONE OPZIONI DEL SECONDO SUBFILE
081200970117     C*------------------------------------------------------------------------*
081300970117     C     GESOP2        BEGSR
081400970117     C*
081500970117SL  1C                   SELECT
081600970117     C     V2SOPZ        WHENEQ    '5 '
081700970117     C                   EXSR      O05GE2                                       *VISUALIZZA VOCI
081800970117     C                   OTHER
081900970117E   1C                   ENDSL
082000970117     C*
082100970117     C                   ENDSR
082200970117     C*------------------------------------------------------------------------*
082300970117     C* O05GE2 - GESTIONE OPZIONE SECONDO SFILE 5 ==> VISUALIZZA VOCI
082400970117     C*------------------------------------------------------------------------*
082500970117     C     O05GE2        BEGSR
082600970117     C*
082700970117     C* AGGIORNA 1° SFILE
082800970117     C                   EXSR      AGGSF2
082900970117     C*
083000970117     C* GESTIONE 3° SFILE
083100970117     C                   Z-ADD     NR2           $DPC2                          *DEPOSITO NRR
083200970117     C                   EXSR      IMPSF3                                       *IMPOSTAZIONI
083300970117     C                   EXSR      GESSF3                                       *GESTIONE
083400970117     C                   Z-ADD     $DPC2         NR2                            *DEPOSITO NRR
083500970117     C*
083600970117     C                   ENDSR
083700970117     C*------------------------------------------------------------------------*
083800970117     C* AGGSF2 - AGGIORNA RIGA DEL SECONDO SUBFILE
083900970117     C*------------------------------------------------------------------------*
084000970117     C     AGGSF2        BEGSR
084100970117     C*
084200970117     C                   MOVEL     *BLANKS       V2SOPZ
084300970117     C                   SETON                                        70        *SI SFLNXTCHG
084400970117     C                   UPDATE    TE20S02                                      *AGGIORNA SUBFILE
084500970117     C                   SETOFF                                       70        *NO SFLNXTCHG
084600970117     C*
084700970117     C                   ENDSR
084800970117     C*------------------------------------------------------------------------*
084900970117     C* IMPSF3 - IMPOSTAZIONI INIZIALI TERZO SUBFILE
085000970117     C*------------------------------------------------------------------------*
085100970117     C     IMPSF3        BEGSR
085200970117     C*
085300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
085400970117     C                   EXSR      SETSF3                                       *SFILE
085500970117     C*
085600970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
085700970117     C                   EXSR      SETSC3                                       *SFILE CONTROL
085800970117     C* CARICA LE SCHIERE
085900970117     C     $DPC1         CHAIN     TE20S01                            98        *RILEGGE COD.TASS.
086000970117     C     $DPC2         CHAIN     TE20S02                            98        *RILEGGE SCAGLIONE
086100970117     C                   MOVEL     V1SCTS        WCTS
086200970117     C                   Z-ADD     V2SSGL        WSGL
086300970117     C                   Z-ADD     V2SCOT        WCOM                           *COMPETENZA PORTO+AC
086400970117     C                   Z-ADD     V2SRIT        WRIC                           *RICAVO     PORTO+AC
086500970117     C                   Z-ADD     V2SDET        WDEL                           *DELTA      PORTO+AC
086600970117     C                   EXSR      MS3OUT
086700970117     C* CARICA IL SUBFILE
086800970117     C                   EXSR      POSSF3
086900970117     C*
087000970117     C                   ENDSR
087100970117     C*------------------------------------------------------------------------*
087200970117     C* SETSF3 - IMPOSTAZIONI INDICATORI TERZO SUBFILE
087300970117     C*------------------------------------------------------------------------*
087400970117     C     SETSF3        BEGSR
087500970117     C*
087600970117     C                   MOVEL     '3'           TIPVID                         *VIDEO IN GESTIONE
087700970117     C                   MOVEL     '3'           DEPVID                         *VIDEO IN GESTIONE
087800970117     C                   SETOFF                                       90         GENERICO DI ERRORE
087900970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
088000970117     C*
088100970117     C                   ENDSR
088200970117     C*------------------------------------------------------------------------*
088300970117     C* SETSC3 - IMPOSTAZIONI INDICATORI E CAMPI TERZO SUBFILE CONTROL
088400970117     C*------------------------------------------------------------------------*
088500970117     C     SETSC3        BEGSR
088600970117     C*
088700970117     C                   MOVEL     V1SCTS        V3CCTS
088800970117     C                   MOVEL     V1SDCT        V3CDCT
088900970117     C* EDITAZIONE SCAGLIONE
089000970117IF  1C     V2SSGL        IFGT      *ZEROS
089100970117     C                   Z-ADD     V2SSGL        $NUM             16 3          *NUMERO DA EDITARE
089200970117     C                   Z-ADD     3             $DEC              1 0          *NUMERO DECIMALI
089300020902    ? /COPY GAITRASRC/SRCTISE,TISE65T
089400970117     C                   MOVE(P)   $ALF          V3CDSG                         *NUMERO EDITATO
089500970117X   1C                   ELSE
089600970117     C                   MOVE(P)   'TOTALE'      V3CDSG
089700970117E   1C                   ENDIF
089800970117     C*
089900970117     C                   ENDSR
090000970117     C*------------------------------------------------------------------------*
090100970117     C* GESSF3 - GESTIONE TERZO SUBFILE
090200970117     C*------------------------------------------------------------------------*
090300970117     C     GESSF3        BEGSR
090400970117     C*
090500970117     C* EMISSIONE VIDEATA
090600970117IF  1C     $TESVD        IFEQ      '1'
090700970117     C                   WRITE     TE20D00                                      *TESTATA
090800970117     C                   MOVEL     '0'           $TESVD
090900970117E   1C                   ENDIF
091000970117     C                   WRITE     TE20Z03                                      *PIEDI
091100970117     C                   EXFMT     TE20C03                                      *SFILE
091200970117     C                   EXSR      SETSF3
091300970117     C* F03-FINE
091400970117IF  1C     *INKC         IFEQ      *ON
091500970117     C                   EXSR      F03GES
091600970117     C                   GOTO      FINSF3
091700970117E   1C                   ENDIF
091800970117     C* F12-RITORNO
091900970117IF  1C     *INKL         IFEQ      *ON
092000970117     C                   MOVEL     '2'           TIPVID
092100970117     C                   GOTO      FINSF3
092200970117E   1C                   ENDIF
092300970117     C* CONTROLLA VIDEO
092400970117     C                   EXSR      CTRSF3
092500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
092600970117     C                   GOTO      FINSF3
092700970117E   1C                   ENDIF
092800970117     C* ENTER-RITORNA
092900970117     C                   MOVEL     '2'           TIPVID
093000970117     C*
093100970117     C     FINSF3        ENDSR
093200970117     C*------------------------------------------------------------------------*
093300970117     C* CTRSF3 - CONTROLLA TERZO SUBFILE
093400970117     C*------------------------------------------------------------------------*
093500970117     C     CTRSF3        BEGSR
093600970117     C*
093700970117     C     FINCT3        ENDSR
093800970117     C*------------------------------------------------------------------------*
093900970117     C* POSSF1 - CARICA IL PRIMO SUBFILE
094000970117     C*------------------------------------------------------------------------*
094100970117     C     POSSF1        BEGSR
094200970117     C*
094300970117     C* ELIMINA IL CONTENUTO DEL SFILE
094400970117     C                   EXSR      CLRSF1
094500970117     C*
094600970117     C* SI POSIZIONA SUL PRIMO RECORD DA CARICARE
094700970117     C                   EXSR      SETFI1
094800970117     C* CARICA IL SUBFILE
094900970117     C                   Z-ADD     *ZEROS        NR1                            *DALL'INIZIO
095000970117     C                   EXSR      CARSF1
095100970117     C*
095200970117     C                   ENDSR
095300970117     C*------------------------------------------------------------------------*
095400970117     C* CLRSF1 - ELIMINA IL CONTENUTO DEL PRIMO SUBFILE
095500970117     C*------------------------------------------------------------------------*
095600970117     C     CLRSF1        BEGSR
095700970117     C*
095800970117     C                   Z-ADD     1             $NR1
095900970117     C                   SETOFF                                       81        *NO SFLDSP
096000970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
096100970117     C                   WRITE     TE20C01
096200970117     C                   SETON                                        81        *SI SFLDSP
096300970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
096400970117     C*
096500970117     C                   ENDSR
096600970117     C*------------------------------------------------------------------------*
096700970117     C* SETFI1 - POSIZIONAMENTO RECORD -PRIMO SUBFILE-
096800970117     C*------------------------------------------------------------------------*
096900970117     C     SETFI1        BEGSR
097000970117     C*
097100970117     C                   Z-ADD     *ZEROS        SF1EOF                         *0=NO FINE/1=FINE
097200970117     C                   Z-ADD     *ZEROS        V                              *INDICE SCHIERE VIDE
097300970117     C*
097400970117     C                   ENDSR
097500970117     C*------------------------------------------------------------------------*
097600970117     C* CARSF1 - CARICA TUTTO IL PRIMO SUBFILE
097700970117     C*------------------------------------------------------------------------*
097800970117     C     CARSF1        BEGSR
097900970117     C*
098000050103DO  1C     V             DOWLT     601
098100970117     C                   EXSR      LETFI1                                       *LEGGE RECORD
098200970117     C                   SETOFF                                       80        *NO SFLNXTCHG
098300970117     C                   ADD       1             NR1
098400970117     C                   EXSR      IRESF1                                       *IMPOSTA RECORD SFIL
098500970117     C                   WRITE     TE20S01                                      *SCRIVE RECORD SFILE
098600970117E   1C                   ENDDO
098700970117     C*
098800970117     C                   ENDSR
098900970117     C*------------------------------------------------------------------------*
099000970117     C* LETFI1 - LETTURA SUCESSIVA RECORD -PRIMO SUBFILE-
099100970117     C*------------------------------------------------------------------------*
099200970117     C     LETFI1        BEGSR
099300970117     C*
099400970117     C* LEGGE FINO A:
099500970117     C*     - FINE FILE
099600970117     C*     - TROVATO RECORD VALIDO
099700970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
099800050103DO  1C     V             DOWLT     601
099900970117     C     WRECOK        ANDEQ     'N'
100000970117     C                   ADD       1             V                              *INDICE SCHIERE VIDE
100100970117     C                   EXSR      CHKRE1                                       *CONTROLLA RECORD
100200970117E   1C                   ENDDO
100300970117     C*
100400970117     C                   ENDSR
100500970117     C*------------------------------------------------------------------------*
100600970117     C* CHKRE1 - CONTROLLA VALIDITA' ELEMENTO SCHIERA PRIMO SFILE
100700970117     C*------------------------------------------------------------------------*
100800970117     C     CHKRE1        BEGSR
100900970117     C*
101000970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
101100970117     C*
101200970117     C* CODICE VUOTO, ESCLUDE
101300970117IF  1C     CTSP(V)       IFEQ      *BLANKS
101400970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101500970117E   1C                   ENDIF
101600970117     C*
101700970117     C                   ENDSR
101800970117     C*------------------------------------------------------------------------*
101900970117     C* IRESF1 - IMPOSTA CAMPI RECORD PRIMO SUBFILE
102000970117     C*------------------------------------------------------------------------*
102100970117     C     IRESF1        BEGSR
102200970117     C*
102300970117     C                   MOVEL     *BLANKS       V1SOPZ                         *CAMPI
102400970117     C                   MOVEL     CTSP(V)       V1SCTS
102500970117     C                   MOVEL     DCTP(V)       V1SDCT
102600970117     C                   Z-ADD     RIPP(V)       V1SRIP
102700970117     C                   Z-ADD     COPP(V)       V1SCOP
102800970117     C                   Z-ADD     DEPP(V)       V1SDEP
102900970117     C                   Z-ADD     RITP(V)       V1SRIT
103000970117     C                   Z-ADD     COTP(V)       V1SCOT
103100970117     C                   Z-ADD     DETP(V)       V1SDET
103200970117     C*
103300970117     C                   MOVEL     V1SOPZ        DU1OPZ                         *TASTI DI DUPLICA
103400970117     C*
103500970117     C                   ENDSR
103600970117     C*------------------------------------------------------------------------*
103700970117     C* POSSF2 - CARICA IL SECONDO SUBFILE
103800970117     C*------------------------------------------------------------------------*
103900970117     C     POSSF2        BEGSR
104000970117     C*
104100970117     C* ELIMINA IL CONTENUTO DEL SFILE
104200970117     C                   EXSR      CLRSF2
104300970117     C*
104400970117     C* SI POSIZIONA SUL PRIMO RECORD DA CARICARE
104500970117     C                   EXSR      SETFI2
104600970117     C* CARICA IL SUBFILE
104700970117     C                   Z-ADD     *ZEROS        NR2                            *DALL'INIZIO
104800970117     C                   EXSR      CARSF2
104900970117     C*
105000970117     C                   ENDSR
105100970117     C*------------------------------------------------------------------------*
105200970117     C* CLRSF2 - ELIMINA IL CONTENUTO DEL SECONDO SUBFILE
105300970117     C*------------------------------------------------------------------------*
105400970117     C     CLRSF2        BEGSR
105500970117     C*
105600970117     C                   Z-ADD     1             $NR2
105700970117     C                   SETOFF                                       71        *NO SFLDSP
105800970117     C                   SETOFF                                       72        *NO DSPCTL/SI SFLCLR
105900970117     C                   WRITE     TE20C02
106000970117     C                   SETON                                        71        *SI SFLDSP
106100970117     C                   SETON                                        72        *SI DSPCTL/NO SFLCLR
106200970117     C*
106300970117     C                   ENDSR
106400970117     C*------------------------------------------------------------------------*
106500970117     C* SETFI2 - POSIZIONAMENTO RECORD -SECONDO SUBFILE-
106600970117     C*------------------------------------------------------------------------*
106700970117     C     SETFI2        BEGSR
106800970117     C*
106900970117     C                   Z-ADD     *ZEROS        SF2EOF                         *0=NO FINE/1=FINE
107000970117     C                   Z-ADD     *ZEROS        V                              *INDICE SCHIERE VIDE
107100970117     C*
107200970117     C                   ENDSR
107300970117     C*------------------------------------------------------------------------*
107400970117     C* CARSF2 - CARICA TUTTO IL SECONDO SUBFILE
107500970117     C*------------------------------------------------------------------------*
107600970117     C     CARSF2        BEGSR
107700970117     C*
107800970117DO  1C     V             DOWLT     11
107900970117     C                   EXSR      LETFI2                                       *LEGGE RECORD
108000970117     C                   SETOFF                                       70        *NO SFLNXTCHG
108100970117     C                   ADD       1             NR2
108200970117     C                   EXSR      IRESF2                                       *IMPOSTA RECORD SFIL
108300970117     C                   WRITE     TE20S02                                      *SCRIVE RECORD SFILE
108400970117E   1C                   ENDDO
108500970117     C*
108600970117     C                   ENDSR
108700970117     C*------------------------------------------------------------------------*
108800970117     C* LETFI2 - LETTURA SUCESSIVA RECORD -SECONDO SUBFILE-
108900970117     C*------------------------------------------------------------------------*
109000970117     C     LETFI2        BEGSR
109100970117     C*
109200970117     C* LEGGE FINO A:
109300970117     C*     - FINE FILE
109400970117     C*     - TROVATO RECORD VALIDO
109500970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
109600970117DO  1C     V             DOWLT     11
109700970117     C     WRECOK        ANDEQ     'N'
109800970117     C                   ADD       1             V                              *INDICE SCHIERE VIDE
109900970117     C                   EXSR      CHKRE2                                       *CONTROLLA RECORD
110000970117E   1C                   ENDDO
110100970117     C*
110200970117     C                   ENDSR
110300970117     C*------------------------------------------------------------------------*
110400970117     C* CHKRE2 - CONTROLLA VALIDITA' ELEMENTO SCHIERA SECONDO SFILE
110500970117     C*------------------------------------------------------------------------*
110600970117     C     CHKRE2        BEGSR
110700970117     C*
110800970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
110900970117     C*
111000970117     C* IMPORTI VUOTI, ESCLUDE
111100970117IF  1C     COPS(V)       IFEQ      *ZEROS
111200970117     C     RIPS(V)       ANDEQ     *ZEROS
111300970117     C     DEPS(V)       ANDEQ     *ZEROS
111400970117     C     COTS(V)       ANDEQ     *ZEROS
111500970117     C     RITS(V)       ANDEQ     *ZEROS
111600970117     C     DETS(V)       ANDEQ     *ZEROS
111700970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111800970117E   1C                   ENDIF
111900970117     C*
112000970117     C                   ENDSR
112100970117     C*------------------------------------------------------------------------*
112200970117     C* IRESF2 - IMPOSTA CAMPI RECORD SECONDO SUBFILE
112300970117     C*------------------------------------------------------------------------*
112400970117     C     IRESF2        BEGSR
112500970117     C*
112600970117     C                   MOVEL     *BLANKS       V2SOPZ                         *CAMPI
112700970117     C                   Z-ADD     SGLS(V)       V2SSGL
112800970117     C                   Z-ADD     RIPS(V)       V2SRIP
112900970117     C                   Z-ADD     COPS(V)       V2SCOP
113000970117     C                   Z-ADD     DEPS(V)       V2SDEP
113100970117     C                   Z-ADD     RITS(V)       V2SRIT
113200970117     C                   Z-ADD     COTS(V)       V2SCOT
113300970117     C                   Z-ADD     DETS(V)       V2SDET
113400970117     C*
113500970117     C                   MOVEL     V2SOPZ        DU2OPZ                         *TASTI DI DUPLICA
113600970117     C*
113700970117     C                   ENDSR
113800970117     C*------------------------------------------------------------------------*
113900970117     C* POSSF3 - CARICA IL TERZO SUBFILE
114000970117     C*------------------------------------------------------------------------*
114100970117     C     POSSF3        BEGSR
114200970117     C*
114300970117     C* ELIMINA IL CONTENUTO DEL SFILE
114400970117     C                   EXSR      CLRSF3
114500970117     C*
114600970117     C* SI POSIZIONA SUL PRIMO RECORD DA CARICARE
114700970117     C                   EXSR      SETFI3
114800970117     C* CARICA IL SUBFILE
114900970117     C                   Z-ADD     *ZEROS        NR3                            *DALL'INIZIO
115000970117     C                   EXSR      CARSF3
115100970117     C*
115200970117     C                   ENDSR
115300970117     C*------------------------------------------------------------------------*
115400970117     C* CLRSF3 - ELIMINA IL CONTENUTO DEL TERZO SUBFILE
115500970117     C*------------------------------------------------------------------------*
115600970117     C     CLRSF3        BEGSR
115700970117     C*
115800970117     C                   Z-ADD     1             $NR3
115900970117     C                   SETOFF                                       61        *NO SFLDSP
116000970117     C                   SETOFF                                       62        *NO DSPCTL/SI SFLCLR
116100970117     C                   WRITE     TE20C03
116200970117     C                   SETON                                        61        *SI SFLDSP
116300970117     C                   SETON                                        62        *SI DSPCTL/NO SFLCLR
116400970117     C*
116500970117     C                   ENDSR
116600970117     C*------------------------------------------------------------------------*
116700970117     C* SETFI3 - POSIZIONAMENTO RECORD -TERZO SUBFILE-
116800970117     C*------------------------------------------------------------------------*
116900970117     C     SETFI3        BEGSR
117000970117     C*
117100970117     C                   Z-ADD     *ZEROS        SF3EOF                         *0=NO FINE/1=FINE
117200970117     C                   Z-ADD     *ZEROS        V                              *INDICE SCHIERE VIDE
117300970117     C*
117400970117     C                   ENDSR
117500970117     C*------------------------------------------------------------------------*
117600970117     C* CARSF3 - CARICA TUTTO IL TERZO SUBFILE
117700970117     C*------------------------------------------------------------------------*
117800970117     C     CARSF3        BEGSR
117900970117     C*
118000970117DO  1C     V             DOWLT     31
118100970117     C                   EXSR      LETFI3                                       *LEGGE RECORD
118200970117     C                   SETOFF                                       60        *NO SFLNXTCHG
118300970117     C                   ADD       1             NR3
118400970117     C                   EXSR      IRESF3                                       *IMPOSTA RECORD SFIL
118500970117     C                   WRITE     TE20S03                                      *SCRIVE RECORD SFILE
118600970117E   1C                   ENDDO
118700970117     C*
118800970117     C                   ENDSR
118900970117     C*------------------------------------------------------------------------*
119000970117     C* LETFI3 - LETTURA SUCESSIVA RECORD -TERZO SUBFILE-
119100970117     C*------------------------------------------------------------------------*
119200970117     C     LETFI3        BEGSR
119300970117     C*
119400970117     C* LEGGE FINO A:
119500970117     C*     - FINE FILE
119600970117     C*     - TROVATO RECORD VALIDO
119700970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
119800970117DO  1C     V             DOWLT     31
119900970117     C     WRECOK        ANDEQ     'N'
120000970117     C                   ADD       1             V                              *INDICE SCHIERE VIDE
120100970117     C                   EXSR      CHKRE3                                       *CONTROLLA RECORD
120200970117E   1C                   ENDDO
120300970117     C*
120400970117     C                   ENDSR
120500970117     C*------------------------------------------------------------------------*
120600970117     C* CHKRE3 - CONTROLLA VALIDITA' ELEMENTO SCHIERA TERZO SFILE
120700970117     C*------------------------------------------------------------------------*
120800970117     C     CHKRE3        BEGSR
120900970117     C*
121000970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
121100970117     C*
121200970117     C* IMPORTI VUOTI, ESCLUDE
121300970117IF  1C     COMT(V)       IFEQ      *ZEROS
121400970117     C     RICT(V)       ANDEQ     *ZEROS
121500970117     C     DELT(V)       ANDEQ     *ZEROS
121600970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
121700970117E   1C                   ENDIF
121800970117     C*
121900970117     C                   ENDSR
122000970117     C*------------------------------------------------------------------------*
122100970117     C* IRESF3 - IMPOSTA CAMPI RECORD TERZO SUBFILE
122200970117     C*------------------------------------------------------------------------*
122300970117     C     IRESF3        BEGSR
122400970117     C*
122500970117     C                   Z-ADD     RDLT(V)       V3SRDL                         *CAMPI
122600970117     C                   Z-ADD     RICT(V)       V3SRIC
122700970117     C                   Z-ADD     COMT(V)       V3SCOM
122800970117     C                   Z-ADD     DELT(V)       V3SDEL
122900970117     C* DECODIFICHE
123000970117     C                   MOVEL     *BLANKS       V3SDRD                         *DESCR.RAGGRUP.DELTA
123100970117     C                   MOVEL     *BLANKS       V3STIP                         *TIPO RAGGRUPPAMENTO
123200970117IF  1C     RDLT(V)       IFGT      *ZEROS
123300970117     C                   Z-ADD     1             J
123400970117     C                   SETOFF                                           98
123500970117     C     RDLT(V)       LOOKUP    RDL(J)                                 98
123600970117IF  2C     *IN98         IFEQ      *ON
123700970117     C                   MOVEL     DRD(J)        V3SDRD
123800970117     C                   MOVEL     TIP(J)        V3STIP
123900970117E   2C                   ENDIF
124000970117X   1C                   ELSE
124100970117     C                   MOVEL(P)  'TOTALE'      V3SDRD
124200970117     C                   MOVEL(P)  *BLANKS       V3STIP
124300970117E   1C                   ENDIF
124400970117     C*
124500970117     C                   ENDSR
124600970117     C*------------------------------------------------------------------------*
124700970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
124800970117     C*------------------------------------------------------------------------*
124900970117     C     CARCC         BEGSR
125000970117     C*
125100970117     C* RECUPERA I DATI DELL'UTENTE
125200020906     C                   Z-ADD     1             CODUT
125300020830     C                   CALL      'X§PARUT'
125400970117     C                   PARM                    PARUT
125500970117     C*
125600970117     C                   Z-ADD     1             Z
125700970117DO  1C                   DO        50            X2                2 0
125800970117     C                   MOVE      TCU(X2)       CARCNT
125900970117IF  2C     TIPCNT        IFEQ      'CI'
126000970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
126100970117E   2C                   ENDIF
126200970117IF  2C     FL34CN        IFEQ      '01'
126300970117     C     TIPCNT        ANDEQ     'CG'
126400970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
126500970117E   2C                   ENDIF
126600970117IF  2C     FL34CN        IFEQ      '02'
126700970117     C     TIPCNT        ANDEQ     'CG'
126800970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
126900970117E   2C                   ENDIF
127000970117IF  2C     TIPCNT        IFEQ      'RI'
127100970117     C                   Z-ADD     KCU(X2)       RI(Z)
127200970117     C                   ADD       1             Z                 3 0
127300970117E   2C                   ENDIF
127400970117E   1C                   ENDDO
127500970117     C*
127600970117     C                   ENDSR
127700970117     C*------------------------------------------------------------------------*
127800970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
127900970117     C*------------------------------------------------------------------------*
128000970117     C     CARTAB        BEGSR
128100970117     C*--------------------
128200970117     C* RAGGRUPPAMENTI DELTA
128300970117     C*--------------------
128400970117     C                   Z-ADD     *ZEROS        I                 4 0
128500020906     C                   Z-ADD     1             KTBKUT
128600970117     C                   MOVEL     '9B'          KTBCOD
128700970117     C     KE2TAB        SETLL     TABEL00F
128800970117     C     KE2TAB        READE     TABEL00F                               99
128900970117DO  1C     *IN99         DOWEQ     *OFF
129000970117IF  2C     TBLFLG        IFEQ      *BLANKS
129100970117     C                   ADD       1             I
129200970117     C                   MOVEL     TBLUNI        DS9B
129300970117     C                   MOVEL     TBLKEY        RDL(I)                         *CODICE
129400970117     C                   MOVEL     §9BDES        DRD(I)                         *DESCRIZIONE
129500970117     C                   MOVEL     §9BTIP        TIP(I)                         *TIPO (P/A)
129600970117E   2C                   ENDIF
129700970117     C     KE2TAB        READE     TABEL00F                               99
129800970117E   1C                   ENDDO
129900970117     C*--------------------
130000970117     C* TIPI SERVIZIO
130100970117     C*--------------------
130200970117     C                   Z-ADD     *ZEROS        I                 4 0
130300020906     C                   Z-ADD     1             KTBKUT
130400970117     C                   MOVEL     '5E'          KTBCOD
130500970117     C     KE2TAB        SETLL     TABEL00F
130600970117     C     KE2TAB        READE     TABEL00F                               99
130700970117DO  1C     *IN99         DOWEQ     *OFF
130800970117IF  2C     TBLFLG        IFEQ      *BLANKS
130900970117     C                   ADD       1             I
131000970117     C                   MOVEL     TBLUNI        DS5E
131100970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
131200970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
131300970117E   2C                   ENDIF
131400970117     C     KE2TAB        READE     TABEL00F                               99
131500970117E   1C                   ENDDO
131600970117     C*--------------------
131700970117     C* TIPI TARIFFA
131800970117     C*--------------------
131900970117     C                   Z-ADD     *ZEROS        I                 4 0
132000020906     C                   Z-ADD     1             KTBKUT
132100970117     C                   MOVEL     'TR'          KTBCOD
132200970117     C     KE2TAB        SETLL     TABEL00F
132300970117     C     KE2TAB        READE     TABEL00F                               99
132400970117DO  1C     *IN99         DOWEQ     *OFF
132500970117IF  2C     TBLFLG        IFEQ      *BLANKS
132600970117     C                   MOVEL     TBLUNI        DSTR
132700970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
132800970117     C                   ADD       1             I
132900970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
133000970117     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
133100970117E   3C                   ENDIF
133200970117E   2C                   ENDIF
133300970117     C     KE2TAB        READE     TABEL00F                               99
133400970117E   1C                   ENDDO
133500040906     c* carico nazioni gestite dai network
133600040906     c                   if        d00fie = 'E' or d00dpd <> ' '
133700040906     c                   exsr      srnaz
133800040906     c                   end
133900970117     C*--------------------
134000970117     C* CODICI TASSAZIONE
134100970117     C*--------------------
134200970117     C                   Z-ADD     *ZEROS        I                 4 0
134300020906     C                   Z-ADD     1             KTBKUT
134400970117     C                   MOVEL     'CT'          KTBCOD
134500970117     C     KE2TAB        SETLL     TABEL00F
134600040906     c                   do        *hival
134700040906     C     KE2TAB        READE     TABEL00F
134800040906DO  1C                   if        %eof(tabel00f)
134900040906     c                   leave
135000040906     c                   end
135100040906IF  2C     TBLFLG        IFne      *BLANKS                                      *NON ANNULLATI
135200040906     c                   iter
135300040906     c                   end
135400970117     C                   MOVEL     TBLUNI        DSCT
135500040906     c* utilizzo in tariffa
135600040906IF  3C     §CTUTC        IFne      'S'                                          *DA CONSIDERARE
135700040906     c                   iter
135800040906     c                   end
135900040906     c* per fed solo tariffe fed e viceversa
136000040906IF  3C     §CTUTf        IFeq      'S'                                          *DA CONSIDERARE
136100040906IF  3C     d00dpd        andne     'F'                                          *DA CONSIDERARE
136200040906     c                   iter
136300040906     c                   end
136400040906IF  3C     §CTUTf        IFne      'S'                                          *DA CONSIDERARE
136500040906IF  3C     d00dpd        andeq     'F'                                          *DA CONSIDERARE
136600040906     c                   iter
136700040906     c                   end
136800040906     c* per dpd no cod tassazione EE
136900040906     c                   if        tblkey = 'EE' and d00dpd = 'D'
137000040906     c                   iter
137100040906     c                   end
137200040906     c* verifico tab. delle nazioni x vedere se paese servito dal network
137300040906     c                   if        d00fie = 'E' or d00dpd <> ' '
137400040906     c     §ctnar        lookup    nar                                    99
137500040906     c                   if        not *in99
137600040906     c                   iter
137700040906     c                   end
137800040906     c                   end
137900040906     c*
138000970117     C                   ADD       1             I
138100970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
138200970117     C                   MOVEL     §CTDES        DCT(I)                         *DESCRIZIONE
138300970117     C                   MOVEL     §CTCOR        COR(I)                         *ORDINAMENTO
138400970117     C                   MOVE      §CTCOR        WA1                            *TIPO CODICE
138500970312IF  4C     WA1           IFEQ      '9'
138600970312IF  5C     CTS(I)        IFEQ      'IT'
138700970117     C     CTS(I)        OREQ      'EE'
138800970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
138900970312X   5C                   ELSE
139000970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
139100040906     c* per fedex carico sia come regione che come codice elementare
139200040906     c                   if        d00dpd = 'F'
139300040906     C                   MOVEL     'E'           TCT(I)                         *TIPO "REGIONE"
139400040906     c                   end
139500970312E   5C                   ENDIF
139600970312X   4C                   ELSE
139700970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
139800970312E   4C                   ENDIF
139900970117E   1C                   ENDDO
140000970117     C*
140100970117     C                   ENDSR
140200040906     C*------------------------------------------------------------------------*
140300040906     C* leggo tabella delle nazioni x vedere se paese servito dal network
140400040906     C*------------------------------------------------------------------------*
140500040906     C     srnaz         BEGSR
140600040906     c                   clear                   nar
140700040906     c                   clear                   nz                3 0
140800040906     C                   Z-ADD     1             KTBKUT
140900040906     C                   MOVEL     '15'          KTBCOD
141000040906     C     KE2TAB        SETLL     TABEL00F
141100040906     c                   do        *hival
141200040906     C     KE2TAB        READE     TABEL00F
141300040906DO  1C                   if        %eof(tabel00f)
141400040906     c                   leave
141500040906     c                   end
141600040906IF  2C     TBLFLG        IFne      *BLANKS                                      *NON ANNULLATI
141700040906     c                   iter
141800040906     c                   end
141900040906     C                   MOVEL     TBLUNI        DS15
142000040906     c* EEX
142100040906     c                   if        d00fie = 'E' and d00dpd = *blanks
142200040906     c                             and §15eex = 'S'
142300040906     c                   add       1             nz
142400040906     c                   movel     tblkey        nar(nz)
142500040906     c                   iter
142600040906     c                   end
142700040906     c* FED
142800040906     c                   if        d00fie = 'E' and d00dpd = 'F'
142900040906     c                             and §15laf <> 0
143000040906     c                   add       1             nz
143100040906     c                   movel     tblkey        nar(nz)
143200040906     c                   iter
143300040906     c                   end
143400040906     c* FED
143500040906     c                   if        d00fie = 'I' and d00dpd = 'D'
143600040906     c                             and §15lad <> 0
143700040906     c                   add       1             nz
143800040906     c                   movel     tblkey        nar(nz)
143900040906     c                   iter
144000040906     c                   end
144100040906     c                   enddo
144200040906     C                   ENDSR
144300970117     C*------------------------------------------------------------------------*
144400970117     C* IMPVD0 - IMPOSTA LA TESTATA
144500970117     C*------------------------------------------------------------------------*
144600970117     C     IMPVD0        BEGSR
144700970117     C*
144800970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
144900970117     C*---
145000970117     C* TESTATA VIDEO
145100970117     C*---
145200970117IF  1C     D00OP0        IFEQ      'O02'
145300970117     C     D00OP0        OREQ      'O05'
145400970117     C                   MOVEL     'Visita '     V0DKOV                         *TARIFFA O OFFERTA
145500970117X   1C                   ELSE
145600970117     C                   MOVEL     'Cliente'     V0DKOV                         *TARIFFA O OFFERTA
145700970117E   1C                   ENDIF
145800970117     C* CAMPI
145900970117     C                   Z-ADD     D00KSC        V0CKSC
146000970117     C                   Z-ADD     D00CTR        V0CCTR
146100970117     C                   Z-ADD     D00PRG        V0CPRG
146200970117     C                   MOVEL     D00FIE        V0CFIE
146300980603     C                   MOVEL     D00DSF        V0CDSF
146400970117     C                   MOVEL     D00TSP        V0CTSP
146500970117     C                   Z-ADD     D00LNP        V0CLNP
146600011003     C                   MOVEL     D00DIV        V0CDIV
146700970117IF  1C     D00DTC        IFGT      *ZEROS
146800970117     C                   Z-ADD     D00DTC        G08INV
146900970117     C                   Z-ADD     *ZEROS        G08DAT
147000970117     C                   MOVEL     '3'           G08ERR
147100970117     C                   CALL      'XSRDA8'
147200970117     C                   PARM                    WLBDA8
147300970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
147400970117E   1C                   ENDIF
147500970117     C* DECODIFICHE
147600020906     C                   Z-ADD     1             KACKUT                         *ANAGRAFICA
147700970512     C                   Z-ADD     KCCCL         KACKCC
147800970512     C                   Z-ADD     D00KSC        KACKSC
147900970512IF  1C     D00CTO        IFEQ      'T'                                          *TARIFFA
148000970512     C     KEYACO        CHAIN     CNACO00F                           99
148100970512X   1C                   ELSE                                                   *OFFERTA
148200970512     C     KEYACO        CHAIN     TFACO00F                           99
148300970512IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
148400970512     C                   Z-ADD     D00KSC        KVINRV
148500970512     C     KEYVIS        CHAIN     TNVIS05L                           99        *VISITE
148600970512     C                   Z-ADD     VISKSC        KACKSC                         *CLIENTE DELLA VISIT
148700970512     C     KEYACO        CHAIN     CNACO00F                           99
148800970512E   2C                   ENDIF
148900970512E   1C                   ENDIF
149000970512     C                   MOVEL(P)  ACORAG        V0DKSC                         *CLIENTE
149100970117     C*
149200970117     C                   MOVEL     *BLANKS       V0DCTR
149300970117IF  1C     D00KSC        IFGT      *ZEROS
149400970117     C                   MOVEL     D00CTR        WA1               1
149500970117     C                   SETOFF                                           98
149600970117     C                   Z-ADD     1             I
149700970117     C     WA1           LOOKUP    TTR(I)                                 98
149800970117IF  2C     *IN98         IFEQ      *ON
149900970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
150000970117E   2C                   ENDIF
150100970117E   1C                   ENDIF
150200970117     C*
150300970117     C                   MOVEL     *BLANKS       V0DTSP
150400970117IF  1C     D00TSP        IFNE      *BLANKS
150500970117     C                   SETOFF                                           98
150600970117     C                   Z-ADD     1             I
150700970117     C     D00TSP        LOOKUP    TSP(I)                                 98
150800970117IF  2C     *IN98         IFEQ      *ON
150900970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
151000970117E   2C                   ENDIF
151100970117E   1C                   ENDIF
151200970117     C*
151300970117     C                   MOVEL     *BLANKS       V0DFIE
151400970117IF  1C     D00FIE        IFEQ      'I'
151500970117     C                   MOVEL     DVID(1)       V0DFIE                         *FLAG ITALIA/ESTERO
151600970117E   1C                   ENDIF
151700970117IF  1C     D00FIE        IFEQ      'E'
151800970117     C                   MOVEL     DVID(2)       V0DFIE                         *FLAG ITALIA/ESTERO
151900970117E   1C                   ENDIF
152000980603     C*
152100980603     C                   MOVEL     *BLANKS       V0DDSF
152200980603IF  1C     D00DSF        IFEQ      'S'
152300980603     C                   MOVEL     DVID(3)       V0DDSF                         *DATI SEDE O FILIALE
152400980603E   1C                   ENDIF
152500980603IF  1C     D00DSF        IFEQ      'F'
152600980603     C                   MOVEL     DVID(4)       V0DDSF                         *DATI SEDE O FILIALE
152700980603E   1C                   ENDIF
152800970117     C*
152900970117     C                   MOVEL     *BLANKS       V0DLNP
153000970117IF  1C     D00LNP        IFGT      *ZEROS
153100970117     C                   Z-ADD     D00LNP        KORFIL
153200970117     C     KEYORG        CHAIN     AZORG01L                           99
153300970117IF  2C     *IN99         IFEQ      *OFF
153400970117     C     ORGFVA        ANDEQ     *BLANKS
153500970117     C     ORGFAG        ANDNE     'V'
153600970117     C                   MOVEL     ORGDES        V0DLNP
153700970117E   2C                   ENDIF
153800970117E   1C                   ENDIF
153900970117     C*---
154000970117     C* TESTATA STAMPA
154100970117     C*---
154200970117     C                   MOVEL     V0DKOV        INCKOV
154300970117     C                   Z-ADD     V0CKSC        INCKSC
154400970117     C                   MOVEL     V0DKSC        INDKSC
154500970117     C                   Z-ADD     V0CCTR        INCCTR
154600970117     C                   MOVEL     V0DCTR        INDCTR
154700970117     C                   Z-ADD     V0CPRG        INCPRG
154800970117     C                   Z-ADD     V0CLNP        INCLNP
154900970117     C                   MOVEL     V0DLNP        INDLNP
155000011003     C                   MOVEL     V0CDIV        INCDIV
155100970117     C*
155200970117     C                   ENDSR
155300970117     C*------------------------------------------------------------------------*
155400970117     C* MEMETW - MEMORIZZA LE SCHIERE DAL WORKFILE
155500970117     C*------------------------------------------------------------------------*
155600970117     C     MEMETW        BEGSR
155700970117     C*
155800970117     C* LEGGE IL FILE DI LAVORO
155900980909     C******************** MOVELUSER      KTWUSR           *NUME UTENTE
156000980909     C                   MOVEL     JOB           KTWUSR                         *NUME UTENTE
156100980603     C                   MOVEL     D00DSF        KTWDSF                         *DATI SEDE O FILIALE
156200980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
156300970117     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
156400970117     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
156500970117     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
156600011001     C     KEYETW        SETLL     TEETW02L                               99
156700970117IF  1C     *IN99         IFEQ      *OFF                                         *DATI INESISTENTI
156800970117     C                   MOVEL     MSG(1)        D00MSG
156900970117     C                   MOVEL     '1'           D00ERR
157000970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
157100970117X   1C                   ELSE                                                   *DATI ESISTENTI
157200011001     C     KEYETW        READE     TEETW02L                               99
157300970117DO  2C     *IN99         DOWEQ     *OFF
157400970117     C*
157500970117     C* OPERAZIONI PER NUOVA NAZIONE DI TASSAZIONE
157600970117     C                   EXSR      NEWNAR
157700970117     C*
157800970117     C* CICLO FINO A ROTTURA NAZIONE
157900970117DO  3C     *IN99         DOWEQ     *OFF
158000970117     C     DEPNAR        ANDEQ     ETWNAR
158100970117     C*
158200970117     C* OPERAZIONI PER NUOVA REGIONE DI TASSAZIONE
158300970117     C                   EXSR      NEWRAP
158400970117     C*
158500970117     C* CICLO FINO A ROTTURA REGIONE
158600970117DO  4C     *IN99         DOWEQ     *OFF
158700970117     C     DEPNAR        ANDEQ     ETWNAR
158800970117     C     DEPRAP        ANDEQ     ETWRAP
158900970117     C*
159000970117     C* OPERAZIONI PER NUOVO CODICE TASSAZIONE
159100970117     C                   EXSR      NEWCTS
159200970117     C*
159300970117     C* CICLO FINO A ROTTURA CODICE TASSAZIONE
159400970117DO  5C     *IN99         DOWEQ     *OFF
159500970117     C     DEPNAR        ANDEQ     ETWNAR
159600970117     C     DEPRAP        ANDEQ     ETWRAP
159700970117     C     DEPCTS        ANDEQ     ETWCTS
159800970117     C*
159900970117     C* OPERAZIONI PER NUOVO SCAGLIONE
160000970117     C                   EXSR      NEWSGL
160100970117     C*
160200970117     C* CICLO FINO A ROTTURA SCAGLIONE
160300970117DO  6C     *IN99         DOWEQ     *OFF
160400970117     C     DEPNAR        ANDEQ     ETWNAR
160500970117     C     DEPRAP        ANDEQ     ETWRAP
160600970117     C     DEPCTS        ANDEQ     ETWCTS
160700970117     C     DEPSGL        ANDEQ     ETWSGL
160800970117     C*
160900970117     C* OPERAZIONI PER NUOVO RAGGRUPPAMENTO DELTA
161000970117     C                   EXSR      NEWRDL
161100970117     C*
161200970117     C* CICLO FINO A ROTTURA RAGGRUPPAMENTO DELTA
161300970117DO  7C     *IN99         DOWEQ     *OFF
161400970117     C     DEPNAR        ANDEQ     ETWNAR
161500970117     C     DEPRAP        ANDEQ     ETWRAP
161600970117     C     DEPCTS        ANDEQ     ETWCTS
161700970117     C     DEPSGL        ANDEQ     ETWSGL
161800970117     C     DEPRDL        ANDEQ     ETWRDL
161900970117     C*
162000970117     C* MEMORIZZA I DATI
162100970117     C                   EXSR      MEMDAT
162200970117     C*
162300970117     C* LETTURA SUCESSIVA WORKFILE
162400011001     C     KEYETW        READE     TEETW02L                               99
162500970117E   7C                   ENDDO                                                  *ROTTURA RAGGR.DELTA
162600970117E   6C                   ENDDO                                                  *ROTTURA SCAGLIONE
162700970117     C*
162800970117     C* MEMORIZZA I DATI PER SCAGLIONE
162900970117     C                   EXSR      MEMSGL
163000970117E   5C                   ENDDO                                                  *ROTTURA CODICE
163100970117E   4C                   ENDDO                                                  *ROTTURA REGIONE
163200970117E   3C                   ENDDO                                                  *ROTTURA NAZIONE
163300970117E   2C                   ENDDO                                                  *FINE FILE
163400970117E   1C                   ENDIF
163500970117     C*
163600970117     C                   ENDSR
163700970117     C*------------------------------------------------------------------------*
163800970117     C* MEMDAT - MEMORIZZA I DATI
163900970117     C*------------------------------------------------------------------------*
164000970117     C     MEMDAT        BEGSR
164100970117     C*
164200970117     C* DEPOSITA I DATI -PER SCAGLIONE-
164300970117     C                   Z-ADD     ETWPPE        SGLPPE                         *% PESO ASSOLUTA
164400970117     C                   Z-ADD     1             J
164500970117     C     ETWRDL        LOOKUP    RDL(J)                                 98
164600970117IF  1C     TIP(J)        IFEQ      'P'
164700970117     C     SGLCOP        ADD       ETWCOM        SGLCOP                         *COMPETENZE PORTO
164800970117     C     SGLRIP        ADD       ETWRIC        SGLRIP                         *RICAVI     PORTO
164900970117E   1C                   ENDIF
165000970117     C     SGLCOT        ADD       ETWCOM        SGLCOT                         *COMPETENZE PORTO+AC
165100970117     C     SGLRIT        ADD       ETWRIC        SGLRIT                         *RICAVI     PORTO+AC
165200970117     C*
165300970117     C* MEMORIZZA I DATI PER -CODICE TASSAZIONE-
165400970117     C     C             OCCUR     DRDL                                         V O C E
165500970117IF  1C     S             IFEQ      1                                            *1° SCAGLIONE
165600970117     C     TCO1(R)       ADD       ETWCOM        TCO1(R)                        *COMPETENZA
165700970117     C     TRI1(R)       ADD       ETWRIC        TRI1(R)                        *RICAVO
165800970117E   1C                   ENDIF
165900970117IF  1C     S             IFEQ      2                                            *2° SCAGLIONE
166000970117     C     TCO2(R)       ADD       ETWCOM        TCO2(R)                        *COMPETENZA
166100970117     C     TRI2(R)       ADD       ETWRIC        TRI2(R)                        *RICAVO
166200970117E   1C                   ENDIF
166300970117IF  1C     S             IFEQ      3                                            *3° SCAGLIONE
166400970117     C     TCO3(R)       ADD       ETWCOM        TCO3(R)                        *COMPETENZA
166500970117     C     TRI3(R)       ADD       ETWRIC        TRI3(R)                        *RICAVO
166600970117E   1C                   ENDIF
166700970117IF  1C     S             IFEQ      4                                            *4° SCAGLIONE
166800970117     C     TCO4(R)       ADD       ETWCOM        TCO4(R)                        *COMPETENZA
166900970117     C     TRI4(R)       ADD       ETWRIC        TRI4(R)                        *RICAVO
167000970117E   1C                   ENDIF
167100970117IF  1C     S             IFEQ      5                                            *5° SCAGLIONE
167200970117     C     TCO5(R)       ADD       ETWCOM        TCO5(R)                        *COMPETENZA
167300970117     C     TRI5(R)       ADD       ETWRIC        TRI5(R)                        *RICAVO
167400970117E   1C                   ENDIF
167500970117IF  1C     S             IFEQ      6                                            *6° SCAGLIONE
167600970117     C     TCO6(R)       ADD       ETWCOM        TCO6(R)                        *COMPETENZA
167700970117     C     TRI6(R)       ADD       ETWRIC        TRI6(R)                        *RICAVO
167800970117E   1C                   ENDIF
167900970117IF  1C     S             IFEQ      7                                            *7° SCAGLIONE
168000970117     C     TCO7(R)       ADD       ETWCOM        TCO7(R)                        *COMPETENZA
168100970117     C     TRI7(R)       ADD       ETWRIC        TRI7(R)                        *RICAVO
168200970117E   1C                   ENDIF
168300970117IF  1C     S             IFEQ      8                                            *8° SCAGLIONE
168400970117     C     TCO8(R)       ADD       ETWCOM        TCO8(R)                        *COMPETENZA
168500970117     C     TRI8(R)       ADD       ETWRIC        TRI8(R)                        *RICAVO
168600970117E   1C                   ENDIF
168700970117IF  1C     S             IFEQ      9                                            *9° SCAGLIONE
168800970117     C     TCO9(R)       ADD       ETWCOM        TCO9(R)                        *COMPETENZA
168900970117     C     TRI9(R)       ADD       ETWRIC        TRI9(R)                        *RICAVO
169000970117E   1C                   ENDIF
169100970117IF  1C     S             IFEQ      10                                           *10°SCAGLIONE
169200970117     C     TCO0(R)       ADD       ETWCOM        TCO0(R)                        *COMPETENZA
169300970117     C     TRI0(R)       ADD       ETWRIC        TRI0(R)                        *RICAVO
169400970117E   1C                   ENDIF
169500970117     C*
169600970117     C                   ENDSR
169700970117     C*------------------------------------------------------------------------*
169800970117     C* MEMSGL - MEMORIZZA I DATI PER SCAGLIONE
169900970117     C*------------------------------------------------------------------------*
170000970117     C     MEMSGL        BEGSR
170100970117     C*
170200970117     C     S             OCCUR     DSGL                                         S C A G L I O N E
170300970117     C                   Z-ADD     SGLPPE        TPPA(C)                        *% PESO
170400970117     C                   Z-ADD     SGLCOP        TCOP(C)                        *COMPETENZA PORTO
170500970117     C                   Z-ADD     SGLRIP        TRIP(C)                        *RICAVI     PORTO
170600970117     C                   Z-ADD     SGLCOT        TCOT(C)                        *COMPETENZA PORTO+AC
170700970117     C                   Z-ADD     SGLRIT        TRIT(C)                        *RICAVI     PORTO+AC
170800970117     C*
170900970117     C                   ENDSR
171000970117     C*------------------------------------------------------------------------*
171100970117     C* NEWNAR - OPERAZIONI PER NUOVA NAZIONE DI TASSAZIONE
171200970117     C*------------------------------------------------------------------------*
171300970117     C     NEWNAR        BEGSR
171400970117     C*
171500970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
171600970117     C                   MOVEL     ETWNAR        DEPNAR
171700970117     C*
171800970117     C                   ENDSR
171900970117     C*------------------------------------------------------------------------*
172000970117     C* NEWRAP - OPERAZIONI PER NUOVA REGIONE DI TASSAZIONE
172100970117     C*------------------------------------------------------------------------*
172200970117     C     NEWRAP        BEGSR
172300970117     C*
172400970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
172500970117     C                   MOVEL     ETWRAP        DEPRAP
172600970117     C*
172700970117     C                   ENDSR
172800970117     C*------------------------------------------------------------------------*
172900970117     C* NEWCTS - OPERAZIONI PER NUOVO CODICE TASSAZIONE
173000970117     C*------------------------------------------------------------------------*
173100970117     C     NEWCTS        BEGSR
173200970117     C*
173300970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
173400970117     C                   MOVEL     ETWCTS        DEPCTS
173500970117     C*
173600970117     C* MEMORIZZA IL CODICE TASSAZIONE "VERO" DA EMETTERE A VIDEO
173700970117IF  1C     ETWRCT        IFEQ      'T'
173800970117     C                   MOVEL     ETWCTS        VCTS              2
173900970117E   1C                   ENDIF
174000970117IF  1C     ETWRCT        IFEQ      'R'
174100970117     C                   MOVEL     ETWRAP        VCTS
174200970117E   1C                   ENDIF
174300970117IF  1C     ETWRCT        IFEQ      'N'
174400970117     C                   MOVEL     ETWNAR        VCTS
174500970117E   1C                   ENDIF
174600970117     C                   Z-ADD     1             J                 4 0
174700970117     C                   SETOFF                                           98
174800970117     C     VCTS          LOOKUP    CTSP(J)                                98
174900970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
175000970117     C                   ADD       1             V
175100970117     C                   MOVEL     VCTS          CTSP(V)                        *CODICE
175200970117     C                   Z-ADD     1             J                 4 0
175300970117     C                   SETOFF                                           98
175400970117     C     VCTS          LOOKUP    CTS(J)                                 98
175500970117IF  2C     *IN98         IFEQ      *ON
175600970117     C                   MOVEL     DCT(J)        DCTP(V)                        *DESCRIZIONE
175700970117     C                   MOVEL     TCT(J)        TCTP(V)                        *TIPO (E/R/N)
175800970117     C                   MOVEL     COR(J)        CORP(V)                        *ORDINAMENTO
175900970117E   2C                   ENDIF
176000970117E   1C                   ENDIF
176100970117     C*
176200970117     C* MEMORIZZA IL CODICE TASSAZIONE IN SCHIERA
176300970117     C                   SETOFF                                           98
176400970117     C                   Z-ADD     1             J
176500970117     C     ETWCTS        LOOKUP    TCTS(J)                                98
176600970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
176700970117     C                   ADD       1             CC                4 0
176800970117     C                   Z-ADD     CC            C                 4 0
176900970117     C                   MOVEL     ETWCTS        TCTS(C)                        *CODICE
177000970117     C                   MOVEL     ETWRAP        TRAP(C)                        *REGIONE
177100970117     C                   MOVEL     ETWNAR        TNAR(C)                        *NAZIONE
177200970117     C                   MOVEL     ETWRCT        TRCT(C)                        *RAGGRUPPAMENTO
177300970117X   1C                   ELSE
177400970117     C                   Z-ADD     J             C
177500970117E   1C                   ENDIF
177600970117     C*
177700970117     C                   ENDSR
177800970117     C*------------------------------------------------------------------------*
177900970117     C* NEWSGL - OPERAZIONI PER NUOVO SCAGLIONE
178000970117     C*------------------------------------------------------------------------*
178100970117     C     NEWSGL        BEGSR
178200970117     C*
178300970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
178400970117     C                   MOVEL     ETWSGL        DEPSGL
178500970117     C*
178600970117     C* AZZERA LE VARIABILI DI LAVORO
178700970129     C                   Z-ADD     *ZEROS        SGLPPE            7 4
178800011001     C                   Z-ADD     *ZEROS        SGLCOP           15 5
178900011001     C                   Z-ADD     *ZEROS        SGLRIP           15 5
179000011001     C                   Z-ADD     *ZEROS        SGLCOT           15 5
179100011001     C                   Z-ADD     *ZEROS        SGLRIT           15 5
179200970117     C*
179300970117     C* MEMORIZZA LO SCAGLIONE IN SCHIERA
179400970117     C                   SETOFF                                           98
179500970117     C                   Z-ADD     1             J
179600970117     C     ETWSGL        LOOKUP    TSGL(J)                                98
179700970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
179800970117     C                   ADD       1             SS                4 0
179900970117     C                   Z-ADD     SS            S                 4 0
180000970117     C                   Z-ADD     ETWSGL        TSGL(S)                        *SCAGLIONE
180100970117X   1C                   ELSE
180200970117     C                   Z-ADD     J             S
180300970117E   1C                   ENDIF
180400970117     C*
180500970117     C                   ENDSR
180600970117     C*------------------------------------------------------------------------*
180700970117     C* NEWRDL - OPERAZIONI PER NUOVA VOCE DI RAGGRUPPAMENTO DELTA
180800970117     C*------------------------------------------------------------------------*
180900970117     C     NEWRDL        BEGSR
181000970117     C*
181100970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
181200970117     C                   MOVEL     ETWRDL        DEPRDL
181300970117     C*
181400970117     C* MEMORIZZA IL RAGGRUPPAMENTO DELTA IN SCHIERA
181500970117     C                   SETOFF                                           98
181600970117     C                   Z-ADD     1             J
181700970117     C     ETWRDL        LOOKUP    TRDL(J)                                98
181800970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
181900970117     C                   ADD       1             RR                4 0
182000970117     C                   Z-ADD     RR            R                 4 0
182100970117     C                   MOVEL     ETWRDL        TRDL(R)                        *RAGGRUPPAMENTO
182200970117X   1C                   ELSE
182300970117     C                   Z-ADD     J             R
182400970117E   1C                   ENDIF
182500970117     C*
182600970117     C                   ENDSR
182700970117     C*------------------------------------------------------------------------*
182800970117     C* MS1OUT - MEMORIZZA LE SCHIERE DEL PRIMO SFILE DA EMETTERE
182900970117     C*------------------------------------------------------------------------*
183000970117     C     MS1OUT        BEGSR
183100970117     C*
183200970117     C* ORDINA I CODICI TASSAZIONE
183300970117     C                   EXSR      ORDCTS
183400970117     C*
183500970117     C* CALCOLA LE % DI PESO ASSOLUTE
183600970117     C                   EXSR      CALPPA
183700970117     C*
183800970117     C* AZZERA LE VARIABILI DI LAVORO
183900970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
184000970117     C                   CLEAR                   COPP                           *SCHIERE VIDEO
184100970117     C                   CLEAR                   RIPP
184200970117     C                   CLEAR                   DEPP
184300970117     C                   CLEAR                   COTP
184400970117     C                   CLEAR                   RITP
184500970117     C                   CLEAR                   DETP
184600970724     C                   Z-ADD     *ZEROS        LPER             17 7          *% DI LAVORO
184700970724     C                   Z-ADD     *ZEROS        LCOM             17 7          *IMPORTO DI COMODO
184800970724     C                   Z-ADD     *ZEROS        LIMP             17 7          *IMPORTO DI LAVORO
184900970117     C*---
185000970117     C* CALCOLA GLI IMPORTI -PER CODICE TASSAZIONE-
185100970117     C*---
185200050103DO  1C     1             DO        601           I                 4 0          *CICLO COD.VIDEO
185300970724     C                   Z-ADD     *ZEROS        TOTCOP           17 7          *COMP.PORTO TOT
185400970724     C                   Z-ADD     *ZEROS        TOTRIP           17 7          *RIC. PORTO TOT
185500970724     C                   Z-ADD     *ZEROS        TOTCOT           17 7          *COMP.PORTO+ACC TOT
185600970724     C                   Z-ADD     *ZEROS        TOTRIT           17 7          *RIC. PORTO+ACC TOT
185700050103DO  2C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
185800970117IF  3C     TCTS(J)       IFNE      *BLANKS
185900970117     C* CALCOLARE
186000970117IF  4C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
186100970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
186200970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
186300970117     C* CALCOLARE IN REGIONE
186400970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
186500970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
186600970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
186700970117     C* CALCOLARE IN NAZIONE
186800970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
186900970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
187000970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
187100970117     C* CALCOLARE TOTALE
187200050103     C     I             OREQ      601                                          *TOTALE
187300970117     C*
187400970117DO  5C     1             DO        10            K                 4 0          *CICLO SCAGLIONI
187500970117     C     K             OCCUR     DSGL                                         S C A G L I O N E
187600970117IF  6C     TPPA(J)       IFGT      *ZEROS
187700970117     C* PERCENTUALE
187800970117     C     TPPA(J)       MULT      100           LCOM
187900970117     C     LCOM          DIV       PPAP(I)       LPER
188000970117     C* COMPETENZE -PORTO-
188100970117     C     TCOP(J)       MULT      LPER          LCOM
188200970117     C     LCOM          DIV       100           LIMP
188300970117     C     TOTCOP        ADD       LIMP          TOTCOP
188400970117     C* RICAVI -PORTO-
188500970117     C     TRIP(J)       MULT      LPER          LCOM
188600970117     C     LCOM          DIV       100           LIMP
188700970117     C     TOTRIP        ADD       LIMP          TOTRIP
188800970117     C* COMPETENZE -PORTO + ACCESSORI-
188900970117     C     TCOT(J)       MULT      LPER          LCOM
189000970117     C     LCOM          DIV       100           LIMP
189100970117     C     TOTCOT        ADD       LIMP          TOTCOT
189200970117     C* RICAVI -PORTO + ACCESSORI-
189300970117     C     TRIT(J)       MULT      LPER          LCOM
189400970117     C     LCOM          DIV       100           LIMP
189500970117     C     TOTRIT        ADD       LIMP          TOTRIT
189600970117E   6C                   ENDIF
189700970117E   5C                   ENDDO                                                  *FINE SCAGLIONI
189800970117E   4C                   ENDIF
189900970117E   3C                   ENDIF
190000970117E   2C                   ENDDO                                                  *FINE COD.TASS.MEMOR
190100970117     C*
190200970725     C* MEMORIZZA I DATI NELL'ELEMENTO DI SCHIERA -CODICE TASSAZIONE-
190300970117IF  2C     TOTCOP        IFGT      *ZEROS                                       *SE MEMORIZZATO QC.
190400970117     C     TOTRIP        ORGT      *ZEROS
190500970117     C     TOTCOT        ORGT      *ZEROS
190600970117     C     TOTRIT        ORGT      *ZEROS
190700970117     C                   Z-ADD     I             V
190800970117     C                   EXSR      SS1OUT
190900970117E   2C                   ENDIF
191000970117E   1C                   ENDDO                                                  *FINE COD.TASS.VIDEO
191100970117     C*
191200970117     C                   ENDSR
191300970117     C*------------------------------------------------------------------------*
191400970117     C* SS1OUT - MEMORIZZA ELEMENTO SCHIERA DEL PRIMO SFILE DA EMETTERE
191500970117     C*------------------------------------------------------------------------*
191600970117     C     SS1OUT        BEGSR
191700970117     C*
191800050103IF  1C     V             IFEQ      601                                          *SE -TOTALE-
191900970117     C                   MOVEL     '**'          CTSP(V)                        *CODICE
192000970117     C                   MOVEL(P)  'TOTALE'      DCTP(V)                        *DESCRIZIONE
192100970117     C                   MOVEL     '*'           TCTP(V)                        *TIPO
192200970117     C                   MOVEL     *BLANKS       CORP(V)                        *ORDINAMENTO
192300970117E   1C                   ENDIF
192400970117     C* IMPORTI
192500970725     C                   Z-ADD(H)  TOTCOP        COPP(V)                        *COMPETENZA PORTO
192600970725     C                   Z-ADD(H)  TOTRIP        RIPP(V)                        *RICAVO     PORTO
192700970725     C                   Z-ADD(H)  TOTCOT        COTP(V)                        *COMPETENZA PORTO+AC
192800970725     C                   Z-ADD(H)  TOTRIT        RITP(V)                        *RICAVO     PORTO+AC
192900970117     C* DELTA
193000011024     C                   Z-ADD     TOTRIP        CONSUN           21 5
193100011024     C                   Z-ADD     TOTCOP        RIFERI           21 5
193200011024     C                   Z-ADD     *ZEROS        DELTA            21 1
193300970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
193400970117     C                   Z-ADD     DELTA         DEPP(V)                        *DELTA PORTO
193500011024     C                   Z-ADD     TOTRIT        CONSUN
193600011024     C                   Z-ADD     TOTCOT        RIFERI
193700011024     C                   Z-ADD     *ZEROS        DELTA
193800970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
193900970117     C                   Z-ADD     DELTA         DETP(V)                        *DELTA PORTO+ACC
194000970117     C*
194100970117     C                   ENDSR
194200970117     C*------------------------------------------------------------------------*
194300970117     C* ORDCTS - ORDINA I CODICI TASSAZIONE DA EMETTERE A VIDEO
194400970117     C*------------------------------------------------------------------------*
194500970117     C     ORDCTS        BEGSR
194600970117     C*
194700050103DO  1C     1             DO        600           I                 4 0
194800970117IF  2C     CTSP(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
194900970117     C     1             ADD       I             J                 4 0
195000050103DO  3C     J             DO        600           J
195100970117IF  4C     CTSP(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
195200970117IF  5C     CORP(I)       IFGT      CORP(J)
195300970117     C                   MOVEL     CTSP(I)       PCTS                           *CODICE
195400970117     C                   MOVEL     CTSP(J)       CTSP(I)
195500970117     C                   MOVEL     PCTS          CTSP(J)
195600970117     C                   MOVEL     DCTP(I)       PDCT                           *DESCRIZIONE
195700970117     C                   MOVEL     DCTP(J)       DCTP(I)
195800970117     C                   MOVEL     PDCT          DCTP(J)
195900970117     C                   MOVEL     TCTP(I)       PTCT                           *TIPO (E/R/N/*)
196000970117     C                   MOVEL     TCTP(J)       TCTP(I)
196100970117     C                   MOVEL     PTCT          TCTP(J)
196200970117     C                   MOVEL     CORP(I)       PCOR                           *ORDINAMENTO
196300970117     C                   MOVEL     CORP(J)       CORP(I)
196400970117     C                   MOVEL     PCOR          CORP(J)
196500970117E   5C                   ENDIF
196600970117E   4C                   ENDIF
196700970117E   3C                   ENDDO                                                  *FINE CICLO UNO
196800970117E   2C                   ENDIF
196900970117E   1C                   ENDDO                                                  *FINE CICLO DUE
197000970117     C*
197100970117     C                   ENDSR
197200970117     C*------------------------------------------------------------------------*
197300970117     C* CALPPA - CALCOLA LE % DI PESO ASSOLUTE
197400970117     C*------------------------------------------------------------------------*
197500970117     C     CALPPA        BEGSR
197600970117     C*
197700050103DO  1C     1             DO        601           I                 4 0          *CICLO COD.VIDEO
197800050103DO  2C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
197900970117IF  3C     TCTS(J)       IFNE      *BLANKS
198000970117     C* CALCOLARE
198100970117IF  4C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
198200970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
198300970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
198400970117     C* CALCOLARE IN REGIONE
198500970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
198600970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
198700970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
198800970117     C* CALCOLARE IN NAZIONE
198900970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
199000970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
199100970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
199200970117     C* CALCOLARE TOTALE
199300050103     C     I             OREQ      601                                          *TOTALE
199400970117     C*
199500970117DO  5C     1             DO        10            K                 4 0          *CICLO SCAGLIONI
199600970117     C     K             OCCUR     DSGL                                         S C A G L I O N E
199700970117     C                   ADD       TPPA(J)       PPAP(I)                        *%PESO ASS.TOTALE
199800970117     C     K             OCCUR     DLAV                                         L A V O R O
199900970117     C                   ADD       TPPA(J)       PPAS(I)                        *%PESO ASS. X SGL
200000970117E   5C                   ENDDO                                                  *FINE SCAGLIONI
200100970117     C*
200200970117E   4C                   ENDIF
200300970117E   3C                   ENDIF
200400970117E   2C                   ENDDO                                                  *FINE COD.TASS.MEMOR
200500970117E   1C                   ENDDO                                                  *FINE COD.TASS.VIDEO
200600970117     C*
200700970117     C                   ENDSR
200800970117     C*------------------------------------------------------------------------*
200900970117     C* MS2OUT - MEMORIZZA LE SCHIERE DEL SECONDO SFILE DA EMETTERE
201000970117     C*------------------------------------------------------------------------*
201100970117     C     MS2OUT        BEGSR
201200970117     C*
201300970117     C* AZZERA LE VARIABILI DI LAVORO
201400970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
201500970117     C                   CLEAR                   SGLS                           *SCHIERE VIDEO
201600970117     C                   CLEAR                   COPS
201700970117     C                   CLEAR                   RIPS
201800970117     C                   CLEAR                   DEPS
201900970117     C                   CLEAR                   COTS
202000970117     C                   CLEAR                   RITS
202100970117     C                   CLEAR                   DETS
202200970724     C                   Z-ADD     *ZEROS        LPER                           *% DI LAVORO
202300970724     C                   Z-ADD     *ZEROS        LCOM                           *IMPORTO DI COMODO
202400970724     C                   Z-ADD     *ZEROS        LIMP                           *IMPORTO DI LAVORO
202500970117     C*
202600970117     C* REPERISCE LA POSIZIONE IN SCHIERE DEL CODICE TASSAZIONE
202700970117     C                   Z-ADD     1             I
202800970117     C     WCTS          LOOKUP    CTSP(I)                                98
202900970117     C*---
203000970117     C* CALCOLA GLI IMPORTI -PER SCAGLIONE-
203100970117     C*---
203200970117DO  1C     1             DO        10            K                 4 0          *CICLO SCAGLIONI
203300970724     C                   Z-ADD     *ZEROS        TOTCOP                         *COMP.PORTO TOT
203400970724     C                   Z-ADD     *ZEROS        TOTRIP                         *RIC. PORTO TOT
203500970724     C                   Z-ADD     *ZEROS        TOTCOT                         *COMP.PORTO+ACC TOT
203600970724     C                   Z-ADD     *ZEROS        TOTRIT                         *RIC. PORTO+ACC TOT
203700970117IF  2C     TSGL(K)       IFGT      *ZEROS
203800050103DO  3C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
203900970117IF  4C     TCTS(J)       IFNE      *BLANKS
204000970117     C* CALCOLARE
204100970117IF  5C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
204200970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
204300970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
204400970117     C* CALCOLARE IN REGIONE
204500970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
204600970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
204700970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
204800970117     C* CALCOLARE IN NAZIONE
204900970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
205000970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
205100970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
205200970117     C* CALCOLARE TOTALE
205300970117     C     TCTP(I)       OREQ      '*'                                          *TOTALE
205400970117     C*
205500970117     C     K             OCCUR     DSGL                                         S C A G L I O N E
205600970117     C     K             OCCUR     DLAV                                         L A V O R O
205700970117     C* PERCENTUALE
205800970117IF  6C     TPPA(J)       IFGT      *ZEROS
205900970117     C     TPPA(J)       MULT      100           LCOM
206000970117     C     LCOM          DIV       PPAS(I)       LPER
206100970117     C* COMPETENZE -PORTO-
206200970117     C     TCOP(J)       MULT      LPER          LCOM
206300970117     C     LCOM          DIV       100           LIMP
206400970117     C     TOTCOP        ADD       LIMP          TOTCOP
206500970117     C* RICAVI -PORTO-
206600970117     C     TRIP(J)       MULT      LPER          LCOM
206700970117     C     LCOM          DIV       100           LIMP
206800970117     C     TOTRIP        ADD       LIMP          TOTRIP
206900970117     C* COMPETENZE -PORTO + ACCESSORI-
207000970117     C     TCOT(J)       MULT      LPER          LCOM
207100970117     C     LCOM          DIV       100           LIMP
207200970117     C     TOTCOT        ADD       LIMP          TOTCOT
207300970117     C* RICAVI -PORTO + ACCESSORI-
207400970117     C     TRIT(J)       MULT      LPER          LCOM
207500970117     C     LCOM          DIV       100           LIMP
207600970117     C     TOTRIT        ADD       LIMP          TOTRIT
207700970117E   6C                   ENDIF
207800970117E   5C                   ENDIF
207900970117E   4C                   ENDIF
208000970117E   3C                   ENDDO                                                  *FINE COD.TASS.MEMOR
208100970117     C*
208200970725     C* MEMORIZZA I DATI NELL'ELEMENTO DI SCHIERA -SCAGLIONE-
208300970117IF  3C     TOTCOP        IFGT      *ZEROS                                       *SE MEMORIZZATO QC.
208400970117     C     TOTRIP        ORGT      *ZEROS
208500970117     C     TOTCOT        ORGT      *ZEROS
208600970117     C     TOTRIT        ORGT      *ZEROS
208700970117     C                   Z-ADD     K             V
208800970117     C                   EXSR      SS2OUT
208900970117E   3C                   ENDIF
209000970117E   2C                   ENDIF
209100970117E   1C                   ENDDO                                                  *FINE SCAGLIONI
209200970117     C*
209300970117     C* MEMORIZZA I DATI DEL -TOTALE-
209400970117     C                   Z-ADD     11            V
209500970117     C                   Z-ADD     *ZEROS        SGLS(V)                        *SCAGLIONE
209600970117     C                   Z-ADD     WCOP          COPS(V)                        *COMPETENZA PORTO
209700970117     C                   Z-ADD     WRIP          RIPS(V)                        *RICAVO     PORTO
209800970117     C                   Z-ADD     WDEP          DEPS(V)                        *DELTA      PORTO
209900970117     C                   Z-ADD     WCOT          COTS(V)                        *COMPETENZA PORTO+AC
210000970130     C                   Z-ADD     WRIT          RITS(V)                        *RICAVO     PORTO+AC
210100970117     C                   Z-ADD     WDET          DETS(V)                        *DELTA      PORTO+AC
210200970117     C*
210300970117     C                   ENDSR
210400970117     C*------------------------------------------------------------------------*
210500970117     C* SS2OUT - MEMORIZZA ELEMENTO SCHIERA DEL SECONDO SFILE DA EMETTERE
210600970117     C*------------------------------------------------------------------------*
210700970117     C     SS2OUT        BEGSR
210800970117     C* IMPORTI
210900970117     C                   Z-ADD     TSGL(K)       SGLS(V)                        *SCAGLIONE
211000970725     C                   Z-ADD(H)  TOTCOP        COPS(V)                        *COMPETENZA PORTO
211100970725     C                   Z-ADD(H)  TOTRIP        RIPS(V)                        *RICAVO     PORTO
211200970725     C                   Z-ADD(H)  TOTCOT        COTS(V)                        *COMPETENZA PORTO+AC
211300970725     C                   Z-ADD(H)  TOTRIT        RITS(V)                        *RICAVO     PORTO+AC
211400970117     C* DELTA
211500011024     C                   Z-ADD     TOTRIP        CONSUN
211600011024     C                   Z-ADD     TOTCOP        RIFERI
211700011024     C                   Z-ADD     *ZEROS        DELTA
211800970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
211900970117     C                   Z-ADD     DELTA         DEPS(V)                        *DELTA PORTO
212000011024     C                   Z-ADD     TOTRIT        CONSUN
212100011024     C                   Z-ADD     TOTCOT        RIFERI
212200011024     C                   Z-ADD     *ZEROS        DELTA
212300970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
212400970117     C                   Z-ADD     DELTA         DETS(V)                        *DELTA PORTO+ACC
212500970117     C*
212600970117     C                   ENDSR
212700970117     C*------------------------------------------------------------------------*
212800970117     C* MS3OUT - MEMORIZZA LE SCHIERE DEL TERZO SFILE DA EMETTERE
212900970117     C*------------------------------------------------------------------------*
213000970117     C     MS3OUT        BEGSR
213100970117     C*
213200970117     C* AZZERA LE VARIABILI DI LAVORO
213300970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
213400970117     C                   CLEAR                   RDLT                           *SCHIERE VIDEO
213500970117     C                   CLEAR                   COMT
213600970117     C                   CLEAR                   RICT
213700970117     C                   CLEAR                   DELT
213800970724     C                   Z-ADD     *ZEROS        LPER                           *% DI LAVORO
213900970724     C                   Z-ADD     *ZEROS        LCOM                           *IMPORTO DI COMODO
214000970724     C                   Z-ADD     *ZEROS        LIMP                           *IMPORTO DI LAVORO
214100970117     C*
214200970117     C* REPERISCE LA POSIZIONE IN SCHIERE DEL CODICE TASSAZIONE
214300970117     C                   Z-ADD     1             I
214400970117     C     WCTS          LOOKUP    CTSP(I)                                98
214500970117     C*---
214600970117     C* CALCOLA GLI IMPORTI -PER RAGGRUPPAMENTO DELTA-
214700970117     C*---
214800970117     C*
214900970117DO  1C     1             DO        30            K                 4 0          *CICLO RAGGRUPPAMENT
215000970724     C                   Z-ADD     *ZEROS        TOTCOM           17 7          *COMPETENZA
215100970724     C                   Z-ADD     *ZEROS        TOTRIC           17 7          *RICAVO
215200970117IF  2C     TRDL(K)       IFGT      *ZEROS
215300050103DO  3C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
215400970117IF  4C     TCTS(J)       IFNE      *BLANKS
215500970117     C* CALCOLARE
215600970117IF  5C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
215700970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
215800970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
215900970117     C* CALCOLARE IN REGIONE
216000970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
216100970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
216200970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
216300970117     C* CALCOLARE IN NAZIONE
216400970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
216500970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
216600970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
216700970117     C* CALCOLARE TOTALE
216800970117     C     TCTP(I)       OREQ      '*'                                          *TOTALE
216900970117     C*
217000970117     C     J             OCCUR     DRDL                                         V O C E
217100970117     C*---
217200970117     C* 1° SCAGLIONE
217300970117     C*---
217400970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
217500970117     C     WSGL          ANDEQ     TSGL(1)
217600970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
217700970117IF  7C     TCO1(K)       IFGT      *ZEROS
217800970117     C     TRI1(K)       ORGT      *ZEROS
217900970117     C     1             OCCUR     DSGL                                         S C A G L I O N E
218000970117     C     1             OCCUR     DLAV                                         L A V O R O
218100970117     C* PERCENTUALE
218200970117     C                   Z-ADD     *ZEROS        LPER
218300970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
218400970117IF  9C     PPAS(I)       IFGT      *ZEROS
218500970117     C     TPPA(J)       MULT      100           LCOM
218600970117     C     LCOM          DIV       PPAS(I)       LPER
218700970117E   9C                   ENDIF
218800970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
218900970117IF  9C     PPAP(I)       IFGT      *ZEROS
219000970117     C     TPPA(J)       MULT      100           LCOM
219100970117     C     LCOM          DIV       PPAP(I)       LPER
219200970117E   9C                   ENDIF
219300970117E   8C                   ENDIF
219400970117     C* COMPETENZE
219500970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
219600970117     C     TCO1(K)       MULT      LPER          LCOM
219700970117     C     LCOM          DIV       100           LIMP
219800970117     C     TOTCOM        ADD       LIMP          TOTCOM
219900970117     C* RICAVI
220000970117     C     TRI1(K)       MULT      LPER          LCOM
220100970117     C     LCOM          DIV       100           LIMP
220200970117     C     TOTRIC        ADD       LIMP          TOTRIC
220300970117E   8C                   ENDIF
220400970117E   7C                   ENDIF
220500970117E   6C                   ENDIF
220600970117     C*---
220700970117     C* 2° SCAGLIONE
220800970117     C*---
220900970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
221000970117     C     WSGL          ANDEQ     TSGL(2)
221100970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
221200970117IF  7C     TCO2(K)       IFGT      *ZEROS
221300970117     C     TRI2(K)       ORGT      *ZEROS
221400970117     C     2             OCCUR     DSGL                                         S C A G L I O N E
221500970117     C     2             OCCUR     DLAV                                         L A V O R O
221600970117     C* PERCENTUALE
221700970117     C                   Z-ADD     *ZEROS        LPER
221800970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
221900970117IF  9C     PPAS(I)       IFGT      *ZEROS
222000970117     C     TPPA(J)       MULT      100           LCOM
222100970117     C     LCOM          DIV       PPAS(I)       LPER
222200970117E   9C                   ENDIF
222300970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
222400970117IF  9C     PPAP(I)       IFGT      *ZEROS
222500970117     C     TPPA(J)       MULT      100           LCOM
222600970117     C     LCOM          DIV       PPAP(I)       LPER
222700970117E   9C                   ENDIF
222800970117E   8C                   ENDIF
222900970117     C* COMPETENZE
223000970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
223100970117     C     TCO2(K)       MULT      LPER          LCOM
223200970117     C     LCOM          DIV       100           LIMP
223300970117     C     TOTCOM        ADD       LIMP          TOTCOM
223400970117     C* RICAVI
223500970117     C     TRI2(K)       MULT      LPER          LCOM
223600970117     C     LCOM          DIV       100           LIMP
223700970117     C     TOTRIC        ADD       LIMP          TOTRIC
223800970117E   8C                   ENDIF
223900970117E   7C                   ENDIF
224000970117E   6C                   ENDIF
224100970117     C*---
224200970117     C* 3° SCAGLIONE
224300970117     C*---
224400970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
224500970117     C     WSGL          ANDEQ     TSGL(3)
224600970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
224700970117IF  7C     TCO3(K)       IFGT      *ZEROS
224800970117     C     TRI3(K)       ORGT      *ZEROS
224900970117     C     3             OCCUR     DSGL                                         S C A G L I O N E
225000970117     C     3             OCCUR     DLAV                                         L A V O R O
225100970117     C* PERCENTUALE
225200970117     C                   Z-ADD     *ZEROS        LPER
225300970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
225400970117IF  9C     PPAS(I)       IFGT      *ZEROS
225500970117     C     TPPA(J)       MULT      100           LCOM
225600970117     C     LCOM          DIV       PPAS(I)       LPER
225700970117E   9C                   ENDIF
225800970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
225900970117IF  9C     PPAP(I)       IFGT      *ZEROS
226000970117     C     TPPA(J)       MULT      100           LCOM
226100970117     C     LCOM          DIV       PPAP(I)       LPER
226200970117E   9C                   ENDIF
226300970117E   8C                   ENDIF
226400970117     C* COMPETENZE
226500970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
226600970117     C     TCO3(K)       MULT      LPER          LCOM
226700970117     C     LCOM          DIV       100           LIMP
226800970117     C     TOTCOM        ADD       LIMP          TOTCOM
226900970117     C* RICAVI
227000970117     C     TRI3(K)       MULT      LPER          LCOM
227100970117     C     LCOM          DIV       100           LIMP
227200970117     C     TOTRIC        ADD       LIMP          TOTRIC
227300970117E   8C                   ENDIF
227400970117E   7C                   ENDIF
227500970117E   6C                   ENDIF
227600970117     C*---
227700970117     C* 4° SCAGLIONE
227800970117     C*---
227900970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
228000970117     C     WSGL          ANDEQ     TSGL(4)
228100970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
228200970117IF  7C     TCO4(K)       IFGT      *ZEROS
228300970117     C     TRI4(K)       ORGT      *ZEROS
228400970117     C     4             OCCUR     DSGL                                         S C A G L I O N E
228500970117     C     4             OCCUR     DLAV                                         L A V O R O
228600970117     C* PERCENTUALE
228700970117     C                   Z-ADD     *ZEROS        LPER
228800970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
228900970117IF  9C     PPAS(I)       IFGT      *ZEROS
229000970117     C     TPPA(J)       MULT      100           LCOM
229100970117     C     LCOM          DIV       PPAS(I)       LPER
229200970117E   9C                   ENDIF
229300970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
229400970117IF  9C     PPAP(I)       IFGT      *ZEROS
229500970117     C     TPPA(J)       MULT      100           LCOM
229600970117     C     LCOM          DIV       PPAP(I)       LPER
229700970117E   9C                   ENDIF
229800970117E   8C                   ENDIF
229900970117     C* COMPETENZE
230000970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
230100970117     C     TCO4(K)       MULT      LPER          LCOM
230200970117     C     LCOM          DIV       100           LIMP
230300970117     C     TOTCOM        ADD       LIMP          TOTCOM
230400970117     C* RICAVI
230500970117     C     TRI4(K)       MULT      LPER          LCOM
230600970117     C     LCOM          DIV       100           LIMP
230700970117     C     TOTRIC        ADD       LIMP          TOTRIC
230800970117E   8C                   ENDIF
230900970117E   7C                   ENDIF
231000970117E   6C                   ENDIF
231100970117     C*---
231200970117     C* 5° SCAGLIONE
231300970117     C*---
231400970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
231500970117     C     WSGL          ANDEQ     TSGL(5)
231600970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
231700970117IF  7C     TCO5(K)       IFGT      *ZEROS
231800970117     C     TRI5(K)       ORGT      *ZEROS
231900970117     C     5             OCCUR     DSGL                                         S C A G L I O N E
232000970117     C     5             OCCUR     DLAV                                         L A V O R O
232100970117     C* PERCENTUALE
232200970117     C                   Z-ADD     *ZEROS        LPER
232300970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
232400970117IF  9C     PPAS(I)       IFGT      *ZEROS
232500970117     C     TPPA(J)       MULT      100           LCOM
232600970117     C     LCOM          DIV       PPAS(I)       LPER
232700970117E   9C                   ENDIF
232800970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
232900970117IF  9C     PPAP(I)       IFGT      *ZEROS
233000970117     C     TPPA(J)       MULT      100           LCOM
233100970117     C     LCOM          DIV       PPAP(I)       LPER
233200970117E   9C                   ENDIF
233300970117E   8C                   ENDIF
233400970117     C* COMPETENZE
233500970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
233600970117     C     TCO5(K)       MULT      LPER          LCOM
233700970117     C     LCOM          DIV       100           LIMP
233800970117     C     TOTCOM        ADD       LIMP          TOTCOM
233900970117     C* RICAVI
234000970117     C     TRI5(K)       MULT      LPER          LCOM
234100970117     C     LCOM          DIV       100           LIMP
234200970117     C     TOTRIC        ADD       LIMP          TOTRIC
234300970117E   8C                   ENDIF
234400970117E   7C                   ENDIF
234500970117E   6C                   ENDIF
234600970117     C*---
234700970117     C* 6° SCAGLIONE
234800970117     C*---
234900970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
235000970117     C     WSGL          ANDEQ     TSGL(6)
235100970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
235200970117IF  7C     TCO6(K)       IFGT      *ZEROS
235300970117     C     TRI6(K)       ORGT      *ZEROS
235400970117     C     6             OCCUR     DSGL                                         S C A G L I O N E
235500970117     C     6             OCCUR     DLAV                                         L A V O R O
235600970117     C* PERCENTUALE
235700970117     C                   Z-ADD     *ZEROS        LPER
235800970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
235900970117IF  9C     PPAS(I)       IFGT      *ZEROS
236000970117     C     TPPA(J)       MULT      100           LCOM
236100970117     C     LCOM          DIV       PPAS(I)       LPER
236200970117E   9C                   ENDIF
236300970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
236400970117IF  9C     PPAP(I)       IFGT      *ZEROS
236500970117     C     TPPA(J)       MULT      100           LCOM
236600970117     C     LCOM          DIV       PPAP(I)       LPER
236700970117E   9C                   ENDIF
236800970117E   8C                   ENDIF
236900970117     C* COMPETENZE
237000970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
237100970117     C     TCO6(K)       MULT      LPER          LCOM
237200970117     C     LCOM          DIV       100           LIMP
237300970117     C     TOTCOM        ADD       LIMP          TOTCOM
237400970117     C* RICAVI
237500970117     C     TRI6(K)       MULT      LPER          LCOM
237600970117     C     LCOM          DIV       100           LIMP
237700970117     C     TOTRIC        ADD       LIMP          TOTRIC
237800970117E   8C                   ENDIF
237900970117E   7C                   ENDIF
238000970117E   6C                   ENDIF
238100970117     C*---
238200970117     C* 7° SCAGLIONE
238300970117     C*---
238400970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
238500970117     C     WSGL          ANDEQ     TSGL(7)
238600970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
238700970117IF  7C     TCO7(K)       IFGT      *ZEROS
238800970117     C     TRI7(K)       ORGT      *ZEROS
238900970117     C     7             OCCUR     DSGL                                         S C A G L I O N E
239000970117     C     7             OCCUR     DLAV                                         L A V O R O
239100970117     C* PERCENTUALE
239200970117     C                   Z-ADD     *ZEROS        LPER
239300970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
239400970117IF  9C     PPAS(I)       IFGT      *ZEROS
239500970117     C     TPPA(J)       MULT      100           LCOM
239600970117     C     LCOM          DIV       PPAS(I)       LPER
239700970117E   9C                   ENDIF
239800970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
239900970117IF  9C     PPAP(I)       IFGT      *ZEROS
240000970117     C     TPPA(J)       MULT      100           LCOM
240100970117     C     LCOM          DIV       PPAP(I)       LPER
240200970117E   9C                   ENDIF
240300970117E   8C                   ENDIF
240400970117     C* COMPETENZE
240500970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
240600970117     C     TCO7(K)       MULT      LPER          LCOM
240700970117     C     LCOM          DIV       100           LIMP
240800970117     C     TOTCOM        ADD       LIMP          TOTCOM
240900970117     C* RICAVI
241000970117     C     TRI7(K)       MULT      LPER          LCOM
241100970117     C     LCOM          DIV       100           LIMP
241200970117     C     TOTRIC        ADD       LIMP          TOTRIC
241300970117E   8C                   ENDIF
241400970117E   7C                   ENDIF
241500970117E   6C                   ENDIF
241600970117     C*---
241700970117     C* 8° SCAGLIONE
241800970117     C*---
241900970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
242000970117     C     WSGL          ANDEQ     TSGL(8)
242100970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
242200970117IF  7C     TCO8(K)       IFGT      *ZEROS
242300970117     C     TRI8(K)       ORGT      *ZEROS
242400970117     C     8             OCCUR     DSGL                                         S C A G L I O N E
242500970117     C     8             OCCUR     DLAV                                         L A V O R O
242600970117     C* PERCENTUALE
242700970117     C                   Z-ADD     *ZEROS        LPER
242800970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
242900970117IF  9C     PPAS(I)       IFGT      *ZEROS
243000970117     C     TPPA(J)       MULT      100           LCOM
243100970117     C     LCOM          DIV       PPAS(I)       LPER
243200970117E   9C                   ENDIF
243300970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
243400970117IF  9C     PPAP(I)       IFGT      *ZEROS
243500970117     C     TPPA(J)       MULT      100           LCOM
243600970117     C     LCOM          DIV       PPAP(I)       LPER
243700970117E   9C                   ENDIF
243800970117E   8C                   ENDIF
243900970117     C* COMPETENZE
244000970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
244100970117     C     TCO8(K)       MULT      LPER          LCOM
244200970117     C     LCOM          DIV       100           LIMP
244300970117     C     TOTCOM        ADD       LIMP          TOTCOM
244400970117     C* RICAVI
244500970117     C     TRI8(K)       MULT      LPER          LCOM
244600970117     C     LCOM          DIV       100           LIMP
244700970117     C     TOTRIC        ADD       LIMP          TOTRIC
244800970117E   8C                   ENDIF
244900970117E   7C                   ENDIF
245000970117E   6C                   ENDIF
245100970117     C*---
245200970117     C* 9° SCAGLIONE
245300970117     C*---
245400970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
245500970117     C     WSGL          ANDEQ     TSGL(9)
245600970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
245700970117IF  7C     TCO9(K)       IFGT      *ZEROS
245800970117     C     TRI9(K)       ORGT      *ZEROS
245900970117     C     9             OCCUR     DSGL                                         S C A G L I O N E
246000970117     C     9             OCCUR     DLAV                                         L A V O R O
246100970117     C* PERCENTUALE
246200970117     C                   Z-ADD     *ZEROS        LPER
246300970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
246400970117IF  9C     PPAS(I)       IFGT      *ZEROS
246500970117     C     TPPA(J)       MULT      100           LCOM
246600970117     C     LCOM          DIV       PPAS(I)       LPER
246700970117E   9C                   ENDIF
246800970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
246900970117IF  9C     PPAP(I)       IFGT      *ZEROS
247000970117     C     TPPA(J)       MULT      100           LCOM
247100970117     C     LCOM          DIV       PPAP(I)       LPER
247200970117E   9C                   ENDIF
247300970117E   8C                   ENDIF
247400970117     C* COMPETENZE
247500970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
247600970117     C     TCO9(K)       MULT      LPER          LCOM
247700970117     C     LCOM          DIV       100           LIMP
247800970117     C     TOTCOM        ADD       LIMP          TOTCOM
247900970117     C* RICAVI
248000970117     C     TRI9(K)       MULT      LPER          LCOM
248100970117     C     LCOM          DIV       100           LIMP
248200970117     C     TOTRIC        ADD       LIMP          TOTRIC
248300970117E   8C                   ENDIF
248400970117E   7C                   ENDIF
248500970117E   6C                   ENDIF
248600970117     C*---
248700970117     C* 10° SCAGLIONE
248800970117     C*---
248900970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
249000970117     C     WSGL          ANDEQ     TSGL(10)
249100970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
249200970117IF  7C     TCO0(K)       IFGT      *ZEROS
249300970117     C     TRI0(K)       ORGT      *ZEROS
249400970117     C     10            OCCUR     DSGL                                         S C A G L I O N E
249500970117     C     10            OCCUR     DLAV                                         L A V O R O
249600970117     C* PERCENTUALE
249700970117     C                   Z-ADD     *ZEROS        LPER
249800970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
249900970117IF  9C     PPAS(I)       IFGT      *ZEROS
250000970117     C     TPPA(J)       MULT      100           LCOM
250100970117     C     LCOM          DIV       PPAS(I)       LPER
250200970117E   9C                   ENDIF
250300970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
250400970117IF  9C     PPAP(I)       IFGT      *ZEROS
250500970117     C     TPPA(J)       MULT      100           LCOM
250600970117     C     LCOM          DIV       PPAP(I)       LPER
250700970117E   9C                   ENDIF
250800970117E   8C                   ENDIF
250900970117     C* COMPETENZE
251000970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
251100970117     C     TCO0(K)       MULT      LPER          LCOM
251200970117     C     LCOM          DIV       100           LIMP
251300970117     C     TOTCOM        ADD       LIMP          TOTCOM
251400970117     C* RICAVI
251500970117     C     TRI0(K)       MULT      LPER          LCOM
251600970117     C     LCOM          DIV       100           LIMP
251700970117     C     TOTRIC        ADD       LIMP          TOTRIC
251800970117E   8C                   ENDIF
251900970117E   7C                   ENDIF
252000970117E   6C                   ENDIF
252100970117     C*
252200970117E   5C                   ENDIF
252300970117E   4C                   ENDIF
252400970117E   3C                   ENDDO                                                  *FINE COD.TASS.MEMOR
252500970117     C*
252600970725     C* MEMORIZZA I DATI NELL'ELEMENTO DI SCHIERA -RAGGRUPPAMENTO DELTA-
252700970117IF  3C     TOTCOM        IFGT      *ZEROS                                       *SE MEMORIZZATO QC.
252800970117     C     TOTRIC        ORGT      *ZEROS
252900970117     C                   Z-ADD     K             V
253000970117     C                   EXSR      SS3OUT
253100970117E   3C                   ENDIF
253200970117E   2C                   ENDIF
253300970117E   1C                   ENDDO                                                  *FINE SCAGLIONI
253400970117     C*
253500970117     C* MEMORIZZA I DATI DEL -TOTALE-
253600970117     C                   Z-ADD     31            V
253700970117     C                   Z-ADD     *ZEROS        RDLT(V)                        *RAGGRUPPAMENTO
253800970130     C                   Z-ADD     WCOM          COMT(V)                        *COMPETENZA PORTO+AC
253900970130     C                   Z-ADD     WRIC          RICT(V)                        *RICAVO     PORTO+AC
254000970130     C                   Z-ADD     WDEL          DELT(V)                        *DELTA      PORTO+AC
254100970117     C*
254200970117     C                   ENDSR
254300970117     C*------------------------------------------------------------------------*
254400970117     C* SS3OUT - MEMORIZZA ELEMENTO SCHIERA DEL TERZO SFILE DA EMETTERE
254500970117     C*------------------------------------------------------------------------*
254600970117     C     SS3OUT        BEGSR
254700970117     C* IMPORTI
254800970117     C                   Z-ADD     TRDL(K)       RDLT(V)                        *RAGGRUPPAMENTO
254900970725     C                   Z-ADD(H)  TOTCOM        COMT(V)                        *COMPETENZA
255000970725     C                   Z-ADD(H)  TOTRIC        RICT(V)                        *RICAVO
255100970117     C* DELTA
255200011024     C                   Z-ADD     TOTRIC        CONSUN
255300011024     C                   Z-ADD     TOTCOM        RIFERI
255400011024     C                   Z-ADD     *ZEROS        DELTA
255500970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
255600970117     C                   Z-ADD     DELTA         DELT(V)                        *DELTA
255700970117     C*
255800970117     C                   ENDSR
255900970117     C*------------------------------------------------------------------------*
256000970117     C* CALDEL - CALCOLA IL DELTA
256100970117     C*------------------------------------------------------------------------*
256200970117     C     CALDEL        BEGSR
256300970117     C*---
256400970117     C* CONSUN -> VALORE A CONSUNTIVO
256500970117     C* RIFERI -> VALORE DI RIFERIMENTO
256600970117     C*
256700970117     C* (CONSUNTIVO - RIFERIMENTO) = GAP
256800970117     C* [(GAP x 100) : CONSUNTIVO] = DELTA
256900970117     C*---
257000011024     C*
257100011024     C* COME PRIMISSIMA COSA VERIFICO SE UNO DEI DUE FATTORI E' A 0
257200011024     C* ... SE SI' FORZO A +/- 999,9 IL DELTA.
257300011024     C*
257400011024     C                   IF        CONSUN = 0
257500011024     C                   Z-ADD     -999,9        DELTA
257600011024     C                   ENDIF
257700011024     C                   IF        RIFERI = 0
257800011024     C                   Z-ADD     +999,9        DELTA
257900011024     C                   ENDIF
258000011024     c
258100011024IF  1C                   IF        DELTA = 0
258200011024     C     CONSUN        SUB       RIFERI        GAP              21 5
258300970117     C     GAP           MULT      100           GAP
258400970117     C*
258500970117     C     GAP           DIV       CONSUN        GAP
258600970117     C                   Z-ADD(H)  GAP           DELTA
258700970117E   1C                   ENDIF
258800970117     C*
258900970117     C                   ENDSR
259000970318     C*------------------------------------------------------------------------*
259100970318     C* CARETC - LEGGE I DATI TIPO -GENERALI-
259200970318     C*------------------------------------------------------------------------*
259300040811     C*    CARETC        BEGSR
259400970318     C*
259500040811     C*                  CLEAR                   TEETC000
259600040811     C*                  MOVEL     D00DSF        KTCDSF                         *DATI SEDE O FILIALE
259700040811     C*                  MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
259800040811     C*                  Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
259900040811     C*                  Z-ADD     D00CTR        KTCCTR                         *TARIFFA
260000040811     C*                  Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
260100040811     C*                  Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
260200040811     C*    KEYETC        CHAIN     TEETC01L                           99
260300970318     C*
260400040811     C*                  ENDSR
260500970117     C*------------------------------------------------------------------------*
260600970117     C* *INZSR - OPERAZIONI INIZIALI
260700970117     C*------------------------------------------------------------------------*
260800970117     C     *INZSR        BEGSR
260900970117     C*--------------------
261000970117     C* RICEVIMENTO PARAMETRI
261100970117     C*--------------------
261200970117     C     *ENTRY        PLIST
261300970117     C                   PARM                    KPJBA
261400970117     C                   PARM                    DSTE00
261500041202     c* apertura controllata di alcuni file xchè solo di filiale
261600041202     c                   open      tfaco00f                             95
261700041202     c                   open      tnvis05l                             95
261800970117     C*--------------------
261900970117     C* VARIABILI RIFERITE AL DATABASE
262000970117     C*--------------------
262100970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
262200970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
262300970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
262400970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
262500970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
262600970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
262700970512     C     *LIKE         DEFINE    VISNRV        KVINRV                         *TNVIS00F
262800970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
262900040811     C*    *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
263000040811     C*    *LIKE         DEFINE    ETCCTO        KTCCTO
263100040811     C*    *LIKE         DEFINE    ETCKSC        KTCKSC
263200040811     C*    *LIKE         DEFINE    ETCCTR        KTCCTR
263300040811     C*    *LIKE         DEFINE    ETCPRG        KTCPRG
263400040811     C*    *LIKE         DEFINE    ETCLNP        KTCLNP
263500011001     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         *TEETW00F
263600980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
263700980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
263800970117     C     *LIKE         DEFINE    ETWKSC        KTWKSC
263900970117     C     *LIKE         DEFINE    ETWCTR        KTWCTR
264000970117     C     *LIKE         DEFINE    ETWPRG        KTWPRG
264100970117     C     *LIKE         DEFINE    ETWCTS        PCTS                           *DI PARCHEGGIO
264200970117     C     *LIKE         DEFINE    §CTDES        PDCT
264300970117     C     *LIKE         DEFINE    ETWRCT        PTCT
264400970117     C     *LIKE         DEFINE    §CTCOR        PCOR
264500970117     C     *LIKE         DEFINE    ETWCTS        WCTS                           *DI LAVORO
264600970117     C     *LIKE         DEFINE    ETWCOM        WCOP
264700970117     C     *LIKE         DEFINE    ETWRIC        WRIP
264800970117     C     *LIKE         DEFINE    V3SDEL        WDEP
264900970117     C     *LIKE         DEFINE    ETWCOM        WCOT
265000970117     C     *LIKE         DEFINE    ETWRIC        WRIT
265100970117     C     *LIKE         DEFINE    V3SDEL        WDET
265200970117     C     *LIKE         DEFINE    ETWSGL        WSGL
265300970117     C     *LIKE         DEFINE    ETWCOM        WCOM
265400970117     C     *LIKE         DEFINE    ETWRIC        WRIC
265500970117     C     *LIKE         DEFINE    V3SDEL        WDEL
265600970117     C     *LIKE         DEFINE    ETWNAR        DEPNAR                         *DI DEPOSITO
265700970117     C     *LIKE         DEFINE    ETWRAP        DEPRAP
265800970117     C     *LIKE         DEFINE    ETWCTS        DEPCTS
265900970117     C     *LIKE         DEFINE    ETWSGL        DEPSGL
266000970117     C     *LIKE         DEFINE    ETWRDL        DEPRDL
266100970117     C*--------------------
266200970117     C* CHIAVI DI LETTURA
266300970117     C*--------------------
266400970117     C* LETTURA TABEL00F - COMPLETA
266500970117     C     KEYTAB        KLIST
266600970117     C                   KFLD                    KTBKUT                         *UTENTE
266700970117     C                   KFLD                    KTBCOD                         *TABELLA
266800970117     C                   KFLD                    KTBKEY                         *CODICE
266900970117     C* LETTURA TABEL00F - PARZIALE
267000970117     C     KE2TAB        KLIST
267100970117     C                   KFLD                    KTBKUT                         *UTENTE
267200970117     C                   KFLD                    KTBCOD                         *TABELLA
267300970117     C* LETTURA CNACO01L - COMPLETA
267400970117     C     KEYACO        KLIST
267500970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
267600970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
267700970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
267800970512     C* LETTURA TNVIS05L - COMPLETA
267900970512     C     KEYVIS        KLIST
268000970512     C                   KFLD                    KVINRV                         *NUMERO VISITA
268100970117     C* LETTURA AZORG01L - COMPLETA
268200970117     C     KEYORG        KLIST
268300970117     C                   KFLD                    KORFIL                         *FILIALE
268400011001     C* LETTURA TEETC01L - COMPLETA
268500040811     C*    KEYETC        KLIST
268600040811     C*                  KFLD                    KTCDSF                         *DATI SEDE O FILIALE
268700040811     C*                  KFLD                    KTCCTO                         *TARIFFA O OFFERTA
268800040811     C*                  KFLD                    KTCKSC                         *CLIENTE O VISITA
268900040811     C*                  KFLD                    KTCCTR                         *TARIFFA
269000040811     C*                  KFLD                    KTCPRG                         *PROGRESSIVO
269100040811     C*                  KFLD                    KTCLNP                         *LINEA DI PARTENZA
269200011001     C* LETTURA TEETW02L - PARZIALE
269300970117     C     KEYETW        KLIST
269400970213     C                   KFLD                    KTWUSR                         *NOME UTENTE
269500980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
269600980603     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
269700970117     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
269800970117     C                   KFLD                    KTWCTR                         *TARIFFA
269900970117     C                   KFLD                    KTWPRG                         *PROGRESSIVO
270000970117     C*--------------------
270100970117     C* VARIABILI NON RIFERITE AL DATABASE
270200970117     C*--------------------
270300970117     C                   Z-ADD     *ZEROS        NR1               4 0          *SFLRCDNBR 1°
270400970117     C                   Z-ADD     *ZEROS        NR2               4 0          *SFLRCDNBR 2°
270500970117     C                   Z-ADD     *ZEROS        NR3               4 0          *SFLRCDNBR 3°
270600970117     C     *LIKE         DEFINE    NR1           SF1EOF                         *N°RECORD 1°SFILE
270700970117     C     *LIKE         DEFINE    NR2           SF2EOF                         *N°RECORD 2°SFILE
270800970117     C     *LIKE         DEFINE    NR3           SF3EOF                         *N°RECORD 3°SFILE
270900970117     C     *LIKE         DEFINE    V1SOPZ        DU1OPZ                         *DI DUPLICA 1°SFILE
271000970117     C     *LIKE         DEFINE    V1SOPZ        DU2OPZ                         *DI DUPLICA 2°SFILE
271100970117     C     *LIKE         DEFINE    $PC1          $DPC1                          *DEPOSITO NRR 1°SFIL
271200970117     C     *LIKE         DEFINE    $PC2          $DPC2                          *DEPOSITO NRR 1°SFIL
271300970117     C*--------------------
271400970117     C* VARIABILI DI CONTROLLO
271500970117     C*--------------------
271600970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
271700970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
271800970117     C                   MOVEL     '1'           DEPVID            1            *DEP.VIDEO DA GESTIR
271900970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
272000970117     C                   MOVEL     '0'           WERR              1            *ERRORE
272100970117     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
272200970319     C                   MOVEL     '0'           $STMC             1            *STAMPARE COD.TASS.
272300970117     C                   MOVEL     '0'           $STMS             1            *STAMPARE SCAGLIONE
272400970418     C                   MOVEL     '0'           $STMV             1            *STAMPARE VOCE
272500970418     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
272600970418     C                   MOVEL     'N'           $CONF             1            *CONFERMA VIDEATA
272700970117     C*
272800970117     C* REPERISCE LA DATA CORRENTE
272900970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
273000970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
273100970117     C                   Z-ADD     WN8           G08DAT
273200970117     C                   Z-ADD     *ZEROS        G08INV
273300970117     C                   MOVEL     '0'           G08ERR
273400970117     C                   CALL      'XSRDA8'
273500970117     C                   PARM                    WLBDA8
273600970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
273700970117     C*
273800970117     C* CARICA I CAPICONTO DI CONTABILITA'
273900970117     C                   EXSR      CARCC
274000970117     C*
274100970117     C* CARICA LE TABELLE OCCORRENTI
274200970117     C                   EXSR      CARTAB
274300970117     C*
274400970117     C* IMPOSTA LA TESTATA
274500970117     C                   EXSR      IMPVD0
274600970318     C*
274700970318     C* LEGG I DATI TIPO -GENERALI-
274800040811     C*                  EXSR      CARETC
274900970318     C*
275000970117     C* MEMORIZZA LE SCHIERE DATI DEL WORKFILE
275100970117     C                   EXSR      MEMETW
275200970117     C*
275300970117     C                   ENDSR
275400970117     O*------------------------------------------------------------------------*
275500970117** -DVID-
275600970117ITALIA              1
275700970117ESTERO              2
275800980603SEDE                3
275900980603FILIALE             4
2760009701175
276100970117** -MSG-
276200970117TNTE20R- Non esistono dati da visualizzare                                    1
276300970117TNTE20R- 2
276400970117TNTE20R- 3
276500970117TNTE20R- 4
276600970117TNTE20R- 5
276700970117TNTE20R- 6
276800970117TNTE20R- 7
276900970117TNTE20R- 8
277000970117TNTE20R- 9
277100970117TNTE20R- 10
277200970117TNTE20R- Opzione errata                                                       11
277300970418TNTE20R- Immettere S per stampare il dettaglio delta delle voci di tassazione 12
277400970117TNTE20R- 13
277500970117TNTE20R- 14
277600970117TNTE20R- 15
277700970117TNTE20R- 16
277800970117TNTE20R- 17
277900970117TNTE20R- 18
278000970117TNTE20R- 19
278100970117TNTE20R- 20
