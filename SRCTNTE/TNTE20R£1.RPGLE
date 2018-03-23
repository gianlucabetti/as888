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
005500050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
005600050103     D NAR             S              2    DIM(600)                             *CODICE NAZIONE
005700050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)
005800970117     D*-------------------
005900970117     D* SCHIERE DI MEMORIZZAZIONE
006000970117     D*-------------------
006100970117     D*---
006200970117     D* TOTALE
006300970117     D*---
006400050103     D TCTS            S              2    DIM(600)                             *CODICE TASSAZIONE
006500050103     D TRAP            S              2    DIM(600)                             *CODICE REGIONE
006600050103     D TNAR            S              2    DIM(600)                             *CODICE NAZIONE
006700050103     D TRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO (T/R
006800970117     D TSGL            S             13  3 DIM(10)                              *SCAGLIONI
006900970117     D TRDL            S              3  0 DIM(30)                              *RAGGRUPPAMENTO DELT
007000970117     D* MULTIPLE          -10 SCAGLIONI-
007100050103     D* MULTIPLE          -600 CODICI TASSAZIONE
007200970117     D*---
007300970117     D* DI LAVORO
007400970117     D*---
007500970117     D* MULTIPLE          -10 SCAGLIONI-
007600970117     D*---
007700970117     D* OUTPUT - 1° SFILE
007800970117     D*---
007900050103     D CTSP            S              2    DIM(601)                             *CODICE TASSAZIONE
008000050103     D PPAP            S              7  4 DIM(601)                             *% PESO ASSOLUTA TOT
008100050103     D DCTP            S             20    DIM(601)                             *DESCRIZIONE
008200050103     D TCTP            S              1    DIM(601)                             *TIPO (E/R/N/*)
008300050103     D CORP            S              3    DIM(601)                             *ORDINAMENTO
008400050103     D COPP            S             15  5 DIM(601)                             *COMPETENZA PORTO
008500050103     D RIPP            S             15  5 DIM(601)                             *RICAVI     PORTO
008600050103     D DEPP            S              4  1 DIM(601)                             *DELTA      PORTO
008700050103     D COTP            S             15  5 DIM(601)                             *COMPETENZA PORTO +
008800050103     D RITP            S             15  5 DIM(601)                             *RICAVI     PORTO +
008900050103     D DETP            S              4  1 DIM(601)                             *DELTA      PORTO +
009000970117     D*---
009100970117     D* OUTPUT - 2° SFILE
009200970117     D*---
009300970117     D SGLS            S             13  3 DIM(11)                              *SCAGLIONE
009400011018     D COPS            S             15  5 DIM(11)                              *COMPETENZA PORTO
009500011018     D RIPS            S             15  5 DIM(11)                              *RICAVI     PORTO
009600970117     D DEPS            S              4  1 DIM(11)                              *DELTA      PORTO
009700011018     D COTS            S             15  5 DIM(11)                              *COMPETENZA PORTO +
009800011018     D RITS            S             15  5 DIM(11)                              *RICAVI     PORTO +
009900970117     D DETS            S              4  1 DIM(11)                              *DELTA      PORTO +
010000970117     D*---
010100970117     D* OUTPUT - 2° SFILE
010200970117     D*---
010300970117     D RDLT            S              3  0 DIM(31)                              *RAGGRUPPAMENTO DELT
010400011018     D COMT            S             15  5 DIM(31)                              *COMPETENZE
010500011018     D RICT            S             15  5 DIM(31)                              *RICAVI
010600970117     D DELT            S              4  1 DIM(31)                              *DELTA
010700970117     D*-------------------
010800020902     D* TISE65T - EDITAZIONE CAMPI ALFANUMERICI
010900970117     D*-------------------
011000970117     D $N$             S              1    DIM(16)                              *NUMERO DA EDITARE
011100970117     D $A$             S              1    DIM(22)                              *NUMERO EDITATO
011200970117     D*------------------
011300970117     D* TABELLE CAPICONTO
011400970117     D*------------------
011500970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
011600970117     D*------------------------------------------------------------------------*
011700970117     D* INPUT
011800970117     D*------------------------------------------------------------------------*
011900970117     D*------------------
012000970117     D* DS SCHIERE MULTIPLE
012100970117     D*------------------
012200970117     D*---
012300970117     D* BYTE IN PACKED ->
012400050103     D* (7:2)+1=4 ; (4X600) =1200
012500050103     D* (15:2)+1=8 ; (8X600) =2400
012600011018     D* (15:2)+1=8 ; (8X 30) = 240
012700970117     D*---
012800970117     D* -10 SCAGLIONI-
012900970117     D DSGL            DS                  OCCURS(10) INZ
013000050103     D  TPPA                   1   2400P 4
013100050103     D                                     DIM(600)                             *% PESO ASSOLUTA
013200050103     D  TCOP                2401   7200P 5
013300050103     D                                     DIM(600)                             *COMPETENZA PORTO
013400050103     D  TRIP                7201  12000P 5
013500050103     D                                     DIM(600)                             *RICAVI     PORTO
013600050103     D  TCOT               12001  16800P 5
013700050103     D                                     DIM(600)                             *COMPETENZA PORTO+AC
013800050103     D  TRIT               16801  21600P 5
013900050103     D                                     DIM(600)                             *RICAVI     PORTO+AC
014000050103     D* -600 CODICI TASSAZIONE-
014100050103     D DRDL            DS                  OCCURS(600) INZ
014200011018     D  TCO1                   1    240P 5
014300011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 1
014400011018     D  TRI1                 241    480P 5
014500011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 1
014600011018     D  TCO2                 481    720P 5
014700011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 2
014800011018     D  TRI2                 721    960P 5
014900011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 2
015000011018     D  TCO3                 961   1200P 5
015100011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 3
015200011018     D  TRI3                1201   1440P 5
015300011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 3
015400011018     D  TCO4                1441   1680P 5
015500011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 4
015600011018     D  TRI4                1681   1920P 5
015700011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 4
015800011018     D  TCO5                1921   2160P 5
015900011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 5
016000011018     D  TRI5                2161   2400P 5
016100011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 5
016200011018     D  TCO6                2401   2640P 5
016300011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 6
016400011018     D  TRI6                2641   2880P 5
016500011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 6
016600011018     D  TCO7                2881   3120P 5
016700011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 7
016800011018     D  TRI7                3121   3360P 5
016900011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 7
017000011018     D  TCO8                3361   3600P 5
017100011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 8
017200011018     D  TRI8                3601   3840P 5
017300011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 8
017400011018     D  TCO9                3841   4080P 5
017500011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 9
017600011018     D  TRI9                4081   4320P 5
017700011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 9
017800011018     D  TCO0                4321   4560P 5
017900011001     D                                     DIM(30)                              *COMPET.SCAGLIONE 10
018000011018     D  TRI0                4561   4800P 5
018100011001     D                                     DIM(30)                              *RICAVO SCAGLIONE 10
018200970117     D* -10 SCAGLIONI-
018300970117     D DLAV            DS                  OCCURS(10) INZ
018400050103     D  PPAS                   1   2404P 4
018500050103     D                                     DIM(601)                             *% PESO ASSOLUTA TOT
018600970117     D*-------------------
018700970117     D* DS TIPI SERVIZIO
018800970117     D*-------------------
018900970117     D DS5E          E DS
019000970117     D*-------------------
019100970117     D* DS TIPI TARIFFA
019200970117     D*-------------------
019300970117     D DSTR          E DS
019400970117     D*-------------------
019500970117     D* DS CODICI TASSAZIONE
019600970117     D*-------------------
019700970117     D DSCT          E DS
019800970117     D*-------------------
019900970117     D* DS RAGGRUPPAMENTI DELTA
020000970117     D*-------------------
020100970117     D DS9B          E DS
020200970117     D*-------------------
020300970117     D* DS PASSAGGIO PARAMETRI
020400970117     D*-------------------
020500970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
020600970117     D*------------------
020700970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
020800970117     D*------------------
020900970117     D WLBDA8          DS
021000970117     D  G08DAT                 1      8  0
021100970117     D  G08INV                 9     16  0
021200970117     D  G08ERR                17     17
021300970117     D  G08TGI                18     22  0
021400970117     D*------------------
021500020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
021600970117     D*------------------
021700970117     D CARCNT          DS
021800970117     D  FL12CN                 1      2
021900970117     D  FL34CN                 3      4
022000970117     D  TIPCNT                 5      6
022100020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
022200970117     D  FLU                   66     97
022300970117     D                                     DIM(32)                              FLAG PAR.UTENT
022400970117     D  TCU                  398    697
022500970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
022600970117     D  KCU                  698    847P 0
022700970117     D                                     DIM(50)                              *TAB CAPICONTO
022800970117     D                                     PACKEVEN
022900970117     D TCUDS           DS
023000970117     D  E1                     1      1
023100970117     D  E3                     3      3
023200970117     D  E2                     2      2
023300970117     D  E4                     4      4
023400970117     D  E56                    5      6
023500970213     D*-------------------
023600970213     D* INFDS - INFORMAZIONI PROGRAMMA
023700970213     D*-------------------
023800970213     D                SDS
023900980909     D  JOB                  244    253
024000970213     D  USER                 254    263
024100970117     D*-------------------
024200970117     D* ARCHITETTURA
024300970117     D*-------------------
024400970117     D KPJBA         E DS
024500970117     C*------------------------------------------------------------------------*
024600970117     C* MAIN LINE
024700970117     C*------------------------------------------------------------------------*
024800970117     C*
024900970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
025000970117IF  1C     $FINE         IFNE      'S'
025100970117     C*
025200970117     C* IMPOSTAZIONI PRIMO SUBFILE
025300970117     C                   EXSR      IMPSF1
025400970117     C*
025500970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
025600970117DO  2C     $FINE         DOUEQ     'S'
025700970117     C     TIPVID        CASEQ     '1'           GESSF1                         *1° VIDEO
025800970117     C     TIPVID        CASEQ     '2'           GESSF2                         *2° VIDEO
025900970117     C     TIPVID        CASEQ     '3'           GESSF3                         *3° VIDEO
026000970117     C                   ENDCS
026100970117E   2C                   ENDDO
026200970117E   1C                   ENDIF
026300970117     C*
026400970117     C                   SETON                                        LR
026500970117     C*------------------------------------------------------------------------*
026600970117     C* IMPSF1 - IMPOSTAZIONI INIZIALI PRIMO SUBFILE
026700970117     C*------------------------------------------------------------------------*
026800970117     C     IMPSF1        BEGSR
026900970117     C*
027000970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
027100970117     C                   EXSR      SETSF1                                       *SFILE
027200970117     C* CARICA LE SCHIERE
027300970117     C                   EXSR      MS1OUT
027400970117     C* CARICA IL SUBFILE
027500970117     C                   EXSR      POSSF1
027600970117     C*
027700970117     C                   ENDSR
027800970117     C*------------------------------------------------------------------------*
027900970117     C* SETSF1 - IMPOSTAZIONI INDICATORI PRIMO SUBFILE
028000970117     C*------------------------------------------------------------------------*
028100970117     C     SETSF1        BEGSR
028200970117     C*
028300970117     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
028400970117     C                   MOVEL     '1'           DEPVID                         *VIDEO IN GESTIONE
028500970117     C                   SETOFF                                       21         ERRORI SFILE
028600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
028700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
028800970117     C*
028900970117     C                   ENDSR
029000970117     C*------------------------------------------------------------------------*
029100970117     C* GESSF1 - GESTIONE PRIMO SUBFILE
029200970117     C*------------------------------------------------------------------------*
029300970117     C     GESSF1        BEGSR
029400970117     C*
029500970117     C* EMISSIONE VIDEATA
029600970117IF  1C     $TESVD        IFEQ      '1'
029700970117     C                   WRITE     TE20D00                                      *TESTATA
029800970117     C                   MOVEL     '0'           $TESVD
029900970117E   1C                   ENDIF
030000970117     C                   WRITE     TE20Z01                                      *PIEDI
030100970117     C                   EXFMT     TE20C01                                      *SFILE
030200970117     C                   EXSR      SETSF1
030300970117     C* F03-FINE
030400970117IF  1C     *INKC         IFEQ      *ON
030500970117     C                   EXSR      F03GES
030600970117     C                   GOTO      FINSF1
030700970117E   1C                   ENDIF
030800970117     C* F12-RITORNO
030900970117IF  1C     *INKL         IFEQ      *ON
031000970117     C                   EXSR      F12GES
031100970117     C                   GOTO      FINSF1
031200970117E   1C                   ENDIF
031300970117     C* F21-STAMPA DELTA
031400970117IF  1C     *INKV         IFEQ      *ON
031500970117     C                   EXSR      F21GES
031600970117     C                   GOTO      FINSF1
031700970117E   1C                   ENDIF
031800970117     C* F22-STAMPA DATI TIPO
031900970117IF  1C     *INKW         IFEQ      *ON
032000970117     C                   EXSR      F22GES
032100970117     C                   GOTO      FINSF1
032200970117E   1C                   ENDIF
032300970117     C* CONTROLLA VIDEO
032400970117     C                   EXSR      CTRSF1
032500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
032600970117     C                   GOTO      FINSF1
032700970117E   1C                   ENDIF
032800970117     C* ENTER-RIMANE
032900970117IF  1C     $PC1          IFEQ      *ZEROS                                       *SE FUORI DAL SFILE
033000970117     C                   Z-ADD     1             $PC1                           *SI POSIZIONA AL 1°
033100970117X   1C                   ELSE
033200970117     C                   Z-ADD     $PC1          $NR1                           *SI POSIZIONA DOV'E'
033300970117E   1C                   ENDIF
033400970117     C*
033500970117     C     FINSF1        ENDSR
033600970117     C*------------------------------------------------------------------------*
033700970117     C* CTRSF1 - CONTROLLA PRIMO SUBFILE
033800970117     C*------------------------------------------------------------------------*
033900970117     C     CTRSF1        BEGSR
034000970117     C*
034100970117     C* REIMPOSTA VARIABILI DI LAVORO
034200970117     C                   MOVEL     '0'           WERR                           *ERRORE
034300970117     C*
034400970117     C* LEGGE TUTTI I RECORD DEL SFILE
034500970117     C                   Z-ADD     1             NR1
034600970117     C     NR1           CHAIN     TE20S01                            98
034700970117DO  1C     *IN98         DOWEQ     *OFF
034800970117     C                   SETOFF                                       80        *NO SFLNXTCHG
034900970117     C   10              MOVEL     DU1OPZ        V1SOPZ                         *DI DUPLICA
035000970117     C*--------------------
035100970117     C* OPZIONE
035200970117     C*--------------------
035300970117IF  2C     WERR          IFEQ      '0'
035400970117IF  3C     V1SOPZ        IFEQ      *BLANKS
035500970117     C     V1SOPZ        OREQ      '5 '
035600970117     C                   ELSE
035700970117     C                   MOVEL     '1'           WERR
035800970117     C                   SETON                                        2190
035900970117     C                   MOVEL     MSG(11)       $MSG
036000970117E   3C                   ENDIF
036100970117E   2C                   ENDIF
036200970117     C*
036300970117IF  2C     WERR          IFEQ      '1'
036400970117     C                   Z-ADD     NR1           $NR1                           *POSIZIONAMENTO
036500970117E   2C                   ENDIF
036600970117     C* GESTIONE OPZIONI
036700970117     C                   EXSR      GESOP1
036800970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
036900970117     C                   GOTO      FINCT1                                       *ESCE
037000970117X   2C                   ELSE                                                   *N O N  ERRORE
037100970117     C                   MOVEL     V1SOPZ        DU1OPZ                         *DI DUPLICA
037200970117     C                   ADD       1             NR1                            *LEGGE ALTRO RECORD
037300970117     C     NR1           CHAIN     TE20S01                            98
037400970117E   2C                   ENDIF
037500970117E   1C                   ENDDO                                                  *FINE SUBFILE
037600970117     C*
037700970117     C     FINCT1        ENDSR
037800970117     C*------------------------------------------------------------------------*
037900970117     C* GESOP1 - GESTIONE OPZIONI DEL PRIMO SUBFILE
038000970117     C*------------------------------------------------------------------------*
038100970117     C     GESOP1        BEGSR
038200970117     C*
038300970117SL  1C                   SELECT
038400970117     C     V1SOPZ        WHENEQ    '5 '
038500970117     C                   EXSR      O05GE1                                       *VISUALIZZA SCAGLION
038600970117     C                   OTHER
038700970117E   1C                   ENDSL
038800970117     C*
038900970117     C                   ENDSR
039000970117     C*------------------------------------------------------------------------*
039100970117     C* O05GE1 - GESTIONE OPZIONE PRIMO SFILE 5 ==> VISUALIZZA SCAGLIONI
039200970117     C*------------------------------------------------------------------------*
039300970117     C     O05GE1        BEGSR
039400970117     C*
039500970117     C* AGGIORNA 1° SFILE
039600970117     C                   EXSR      AGGSF1
039700970117     C*
039800970117     C* GESTIONE 2° SFILE
039900970117     C                   Z-ADD     NR1           $DPC1                          *DEPOSITO NRR
040000970117     C                   EXSR      IMPSF2                                       *IMPOSTAZIONI
040100970117     C                   EXSR      GESSF2                                       *GESTIONE
040200970117     C                   Z-ADD     $DPC1         NR1                            *DEPOSITO NRR
040300970117     C*
040400970117     C                   ENDSR
040500970117     C*------------------------------------------------------------------------*
040600970117     C* AGGSF1 - AGGIORNA RIGA DEL PRIMO SUBFILE
040700970117     C*------------------------------------------------------------------------*
040800970117     C     AGGSF1        BEGSR
040900970117     C*
041000970117     C                   MOVEL     *BLANKS       V1SOPZ
041100970117     C                   SETON                                        80        *SI SFLNXTCHG
041200970117     C                   UPDATE    TE20S01                                      *AGGIORNA SUBFILE
041300970117     C                   SETOFF                                       80        *NO SFLNXTCHG
041400970117     C*
041500970117     C                   ENDSR
041600970117     C*------------------------------------------------------------------------*
041700970117     C* F03GES - GESTIONE TASTO PRIMO SFILE F03 ==> FINE
041800970117     C*------------------------------------------------------------------------*
041900970117     C     F03GES        BEGSR
042000970117     C*
042100970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
042200970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
042300970117     C*
042400970117     C                   ENDSR
042500970117     C*------------------------------------------------------------------------*
042600970117     C* F12GES - GESTIONE TASTO PRIMO SFILE F12 ==> RITORNO
042700970117     C*------------------------------------------------------------------------*
042800970117     C     F12GES        BEGSR
042900970117     C*
043000970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
043100970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
043200970117     C*
043300970117     C                   ENDSR
043400970117     C*------------------------------------------------------------------------*
043500970117     C* F21GES - GESTIONE TASTO PRIMO SFILE F21 ==> STAMPA DELTA
043600970117     C*------------------------------------------------------------------------*
043700970418     C     F21GES        BEGSR
043800970418     C*
043900970418     C* RICHIESTA PARAMETRI DI LANCIO
044000970418     C                   EXSR      IMPWD1
044100970418DO  1C     $FINV         DOUEQ     'S'
044200970418     C                   EXSR      GESWD1                                       *GESTIONE FINESTRA
044300970418E   1C                   ENDDO
044400970418     C*
044500970117     C* IMPOSTA VARIABILI DI LAVORO
044600970418IF  1C     $CONF         IFEQ      'S'                                          *STAMPA CONFERMATA
044700970117     C                   OPEN      TNTE21P
044800970418     C                   MOVEL     '1'           *IN01                          *FINE PAGINA
044900970117     C*
045000970117     C* PER CIASCUN CODICE TASSAZIONE EMESSO A VIDEO
045100050103DO  2C     1             DO        601           A                 4 0
045200970418IF  3C     CTSP(A)       IFNE      *BLANKS
045300970117     C                   MOVEL     '0'           $STMC                          *STAMPARE COD.TASS.
045400970117     C*
045500970117     C* MEMORIZZA I DATI PER SCAGLIONE
045600970117     C                   MOVEL     CTSP(A)       WCTS
045700970117     C                   Z-ADD     RIPP(A)       WRIP
045800970117     C                   Z-ADD     COPP(A)       WCOP
045900970117     C                   Z-ADD     DEPP(A)       WDEP
046000970117     C                   Z-ADD     RITP(A)       WRIT
046100970117     C                   Z-ADD     COTP(A)       WCOT
046200970117     C                   Z-ADD     DETP(A)       WDET
046300970117     C                   EXSR      MS2OUT
046400970117     C*
046500970117     C* PER CIASCUN SCAGLIONE MEMORIZZATO
046600970418DO  4C     1             DO        11            B                 4 0
046700970418IF  5C     COTS(B)       IFGT      *ZEROS
046800970117     C     RITS(B)       ORGT      *ZEROS
046900970117     C                   MOVEL     '0'           $STMS                          *STAMPARE SCAGLIONE
047000970117     C*
047100970117     C* MEMORIZZA I DATI PER SCAGLIONE
047200970117     C                   MOVEL     CTSP(A)       WCTS
047300970117     C                   Z-ADD     SGLS(B)       WSGL
047400970117     C                   Z-ADD     COTS(B)       WCOM
047500970117     C                   Z-ADD     RITS(B)       WRIC
047600970117     C                   Z-ADD     DETS(B)       WDEL
047700970117     C                   EXSR      MS3OUT
047800970117     C*
047900970117     C* PER CIASCUNA VOCE MEMORIZZATA
048000970418DO  6C     1             DO        30            D                 4 0
048100970418     C                   MOVEL     '0'           $STMV                          *STAMPARE VOCE
048200970418IF  7C     COMT(D)       IFGT      *ZEROS
048300970117     C     RICT(D)       ORGT      *ZEROS
048400970117     C*
048500970319     C* STAMPA RIGA DI DETTAGLIO
048600970117     C                   EXSR      STADET
048700970117     C*
048800970418E   7C                   ENDIF
048900970418E   6C                   ENDDO                                                  *FINE VOCI
049000970418E   5C                   ENDIF
049100970418E   4C                   ENDDO                                                  *FINE SCAGLIONI
049200970418E   3C                   ENDIF
049300970418E   2C                   ENDDO                                                  *FINE COD.TASSAZIONE
049400970117     C*
049500970117     C* STAMPA RIGA DI FINE PROSPETTO
049600970117     C                   WRITE     TE21FI
049700970117     C                   CLOSE     TNTE21P
049800970418E   1C                   ENDIF
049900970117     C*
050000970117     C                   ENDSR
050100970117     C*------------------------------------------------------------------------*
050200970117     C* STADET - STAMPA RIGA DI DETTAGLIO
050300970117     C*------------------------------------------------------------------------*
050400970117     C     STADET        BEGSR
050500970418     C*
050600970418     C* REIMPOSTA VARIABILI DI LAVORO
050700970418     C                   MOVEL     '0'           $STMRD            1            *RIGA DA NON STAMPAR
050800970117     C*
050900970117     C* FINE PAGINA, STAMPA LA TESTATA
051000970117IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
051100970117     C                   WRITE     TE21TE                                       *TESTATA
051200970117     C                   WRITE     TE21IN                                       *INTESTAZIONE
051300970117     C                   WRITE     TE21I2                                       *INTESTAZIONE CAMPI
051400970117     C                   SETOFF                                       01
051500970418     C                   MOVEL     '0'           $STMC                          *COD.TASS. DA STAMP
051600970418     C                   MOVEL     '0'           $STMS                          *SCAGLIONE DA STAMP
051700970418     C                   MOVEL     '0'           $STMV                          *VOCE      DA STAMP
051800970117E   1C                   ENDIF
051900970117     C*
052000970117     C* IMPOSTA I CAMPI -CODICE TASSAZIONE-
052100970117IF  1C     $STMC         IFEQ      '0'
052200970117     C                   MOVEL     WCTS          DACCTS                         *CAMPI
052300970117     C                   MOVEL     DCTP(A)       DADCTS
052400970117     C                   Z-ADD     WRIP          DACCRP
052500970117     C                   Z-ADD     WCOP          DACCCP
052600970117     C                   Z-ADD     WDEP          DACCDP
052700970117     C                   Z-ADD     WRIT          DACCRT
052800970117     C                   Z-ADD     WCOT          DACCCT
052900970117     C                   Z-ADD     WDET          DACCDT
053000970418     C                   MOVEL     '1'           $STMC                          *NON STAMP.COD.TASS.
053100970418     C                   MOVEL     '1'           $STMRD                         *RIGA DA STAMPARE
053200970117X   1C                   ELSE
053300970117     C                   MOVEL     *BLANKS       DACCTS                         *CAMPI
053400970117     C                   MOVEL     *BLANKS       DADCTS
053500970117     C                   Z-ADD     *ZEROS        DACCRP
053600970117     C                   Z-ADD     *ZEROS        DACCCP
053700970117     C                   Z-ADD     *ZEROS        DACCDP
053800970117     C                   Z-ADD     *ZEROS        DACCRT
053900970117     C                   Z-ADD     *ZEROS        DACCCT
054000970117     C                   Z-ADD     *ZEROS        DACCDT
054100970117E   1C                   ENDIF
054200970117     C*
054300970117     C* IMPOSTA I CAMPI -SCAGLIONE-
054400970418IF  1C     $STMS         IFEQ      '0'                                          *SGL DA STAMPARE
054500970418IF  2C     SGLS(B)       IFGT      *ZEROS                                       *CAMPI
054600970117     C                   Z-ADD     SGLS(B)       $NUM             16 3          *NUMERO DA EDITARE
054700970117     C                   Z-ADD     3             $DEC              1 0          *NUMERO DECIMALI
054800020902    ? /COPY GAITRASRC/SRCTISE,TISE65T
054900970117     C                   MOVE(P)   $ALF          DACSGL                         *NUMERO EDITATO
055000970418X   2C                   ELSE
055100970117     C                   MOVE(P)   'TOTALE'      DACSGL
055200970418E   2C                   ENDIF
055300970117     C                   Z-ADD     RIPS(B)       DACSRP
055400970117     C                   Z-ADD     COPS(B)       DACSCP
055500970117     C                   Z-ADD     DEPS(B)       DACSDP
055600970117     C                   Z-ADD     RITS(B)       DACSRT
055700970117     C                   Z-ADD     COTS(B)       DACSCT
055800970117     C                   Z-ADD     DETS(B)       DACSDT
055900970418     C                   MOVEL     '1'           $STMS                          *NON STAMP.SCAGLIONE
056000970418     C                   MOVEL     '1'           $STMRD                         *RIGA DA STAMPARE
056100970418X   1C                   ELSE
056200970117     C                   MOVEL     *BLANKS       DACSGL                         *CAMPI
056300970117     C                   Z-ADD     *ZEROS        DACSRP
056400970117     C                   Z-ADD     *ZEROS        DACSCP
056500970117     C                   Z-ADD     *ZEROS        DACSDP
056600970117     C                   Z-ADD     *ZEROS        DACSRT
056700970117     C                   Z-ADD     *ZEROS        DACSCT
056800970117     C                   Z-ADD     *ZEROS        DACSDT
056900970418E   1C                   ENDIF
057000970117     C*
057100970117     C* IMPOSTA I CAMPI -VOCE-
057200970418IF  1C     $STMV         IFEQ      '0'                                          *VOCE DA STAMPARE
057300970418     C     V1WSDV        ANDEQ     'S'                                          *RICHIESTA STAMP VOC
057400970117     C                   Z-ADD     RDLT(D)       DACRCT                         *CAMPI
057500970117     C                   Z-ADD     RICT(D)       DACRIC
057600970117     C                   Z-ADD     COMT(D)       DACCOM
057700970117     C                   Z-ADD     DELT(D)       DACDEL
057800970117     C                   MOVEL     *BLANKS       DADRCT                         *DECODIFICHE
057900970319IF  2C     RDLT(D)       IFGT      *ZEROS
058000970117     C                   Z-ADD     1             J
058100970117     C                   SETOFF                                           98
058200970117     C     RDLT(D)       LOOKUP    RDL(J)                                 98
058300970319IF  3C     *IN98         IFEQ      *ON
058400970117     C                   MOVEL     DRD(J)        DADRCT
058500970319E   3C                   ENDIF
058600970319X   2C                   ELSE
058700970117     C                   MOVEL(P)  'TOTALE'      DADRCT
058800970319E   2C                   ENDIF
058900970319X   1C                   ELSE                                                   *NO DETTAGLIO VOCI
059000970319     C                   Z-ADD     *ZEROS        DACRCT
059100970319     C                   MOVEL     *BLANKS       DADRCT                         *DECODIFICHE
059200970319     C                   Z-ADD     *ZEROS        DACRIC
059300970319     C                   Z-ADD     *ZEROS        DACCOM
059400970319     C                   Z-ADD     *ZEROS        DACDEL
059500970319E   1C                   ENDIF
059600970418     C*
059700970418IF  1C     $STMRD        IFEQ      '1'                                          *RIGA DA STAMPARE
059800970418     C     $STMRD        OREQ      '0'                                          *RIGA DA NON STAMPAR
059900970418     C     V1WSDV        ANDEQ     'S'                                          *MA RICH. STAMPA VOC
060000970117     C                   WRITE     TE21DA                                       *DETTAGLIO
060100970418E   1C                   ENDIF
060200970117     C*
060300970117     C                   ENDSR
060400970418     C*------------------------------------------------------------------------*
060500970418     C* IMPWD1 - IMPOSTAZIONI PRIMA FINESTRA
060600970418     C*------------------------------------------------------------------------*
060700970418     C     IMPWD1        BEGSR
060800970418     C*
060900970418     C* REIMPOSTA FORMATO VIDEO E INDICATORI
061000970418     C                   MOVEL     'N'           $FINV                          *FINE     FINESTRA
061100970418     C                   MOVEL     'S'           $CONF                          *CONFERMA FINESTRA
061200970512     C                   MOVEL     'N'           V1WSDV
061300970418     C                   EXSR      SETWD1
061400970418     C*
061500970418     C                   ENDSR
061600970418     C*------------------------------------------------------------------------*
061700970418     C* SETWD1 - IMPOSTAZIONI INDICATORI PRIMA FINESTRA
061800970418     C*------------------------------------------------------------------------*
061900970418     C     SETWD1        BEGSR
062000970418     C*
062100970418     C                   SETOFF                                       30         ERRORI
062200970418     C                   SETOFF                                       90         GENERICO DI ERRORE
062300970418     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
062400970418     C*
062500970418     C                   ENDSR
062600970418     C*------------------------------------------------------------------------*
062700970418     C* GESWD1 - GESTIONE PRIMA FINESTRA
062800970418     C*------------------------------------------------------------------------*
062900970418     C     GESWD1        BEGSR
063000970418     C*
063100970418     C* EMISSIONE VIDEATA
063200970418     C                   EXFMT     TE20W01
063300970418     C                   EXSR      SETWD1
063400970418     C* F12-RITORNO
063500970418IF  1C     *INKL         IFEQ      *ON
063600970418     C                   MOVEL     'S'           $FINV                          *FINE     FINESTRA
063700970418     C                   MOVEL     'N'           $CONF                          *CONFERMA FINESTRA
063800970418     C                   GOTO      FINWD1
063900970418E   1C                   ENDIF
064000970418     C* CONTROLLA VIDEO
064100970418     C                   EXSR      CTRWD1
064200970418IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
064300970418     C                   GOTO      FINWD1
064400970418E   1C                   ENDIF
064500970418     C* INVIO-CONFERMA
064600970418     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
064700970418     C*
064800970418     C     FINWD1        ENDSR
064900970418     C*------------------------------------------------------------------------*
065000970418     C* CTRWD1 - CONTROLLA PRIMA FINESTRA
065100970418     C*------------------------------------------------------------------------*
065200970418     C     CTRWD1        BEGSR
065300970418     C*--------------------
065400970418     C* STAMPA DETTAGLIO VOCI
065500970418     C*--------------------
065600970418IF  1C     V1WSDV        IFNE      'S'
065700970418     C     V1WSDV        ANDNE     'N'
065800970418     C                   SETON                                        3090
065900970418     C                   MOVEL     MSG(12)       $MSG
066000970418     C                   GOTO      FINCW1
066100970418E   1C                   ENDIF
066200970418     C*
066300970418     C     FINCW1        ENDSR
066400970117     C*------------------------------------------------------------------------*
066500970117     C* F22GES - GESTIONE TASTO PRIMO SFILE F22 ==> STAMPA DATI TIPO
066600970117     C*------------------------------------------------------------------------*
066700970117     C     F22GES        BEGSR
066800970117     C*
066900970117     C                   MOVEL     'F22'         D00OP1                         *LIVELLO PROGRAMMA
067000970117     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
067100970117     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
067200970117     C                   MOVEL     '0'           D00ERR                         *ERRORE
067300970117     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
067400970418     C*
067500970418     C                   MOVEL     *BLANKS       D00SCT                         *RAPPRESENTAZIONE CT
067600970418     C                   MOVEL     D00FIE        D00SIE                         *TARIFFA ITA/EST
067700980708     C                   MOVEL     D00DSF        D00SSF                         *TARIFFA ITA/EST
067800970117     C*
067900970117     C                   MOVEL     'TE43'        KCOAZ
068000970117     C                   MOVEL     DSTE00        KPJBU
068100970117     C                   CALL      'BCH10'
068200970117     C                   PARM                    KPJBA
068300970117     C*
068400970117     C                   ENDSR
068500970117     C*------------------------------------------------------------------------*
068600970117     C* IMPSF2 - IMPOSTAZIONI INIZIALI SECONDO SUBFILE
068700970117     C*------------------------------------------------------------------------*
068800970117     C     IMPSF2        BEGSR
068900970117     C*
069000970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
069100970117     C                   EXSR      SETSF2                                       *SFILE
069200970117     C*
069300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
069400970117     C                   EXSR      SETSC2                                       *SFILE CONTROL
069500970117     C* CARICA LE SCHIERE
069600970117     C     $DPC1         CHAIN     TE20S01                            98        *RILEGGE COD.TASS.
069700970117     C                   MOVEL     V1SCTS        WCTS
069800970117     C                   Z-ADD     V1SCOP        WCOP
069900970117     C                   Z-ADD     V1SRIP        WRIP
070000970117     C                   Z-ADD     V1SDEP        WDEP
070100970117     C                   Z-ADD     V1SCOT        WCOT
070200970117     C                   Z-ADD     V1SRIT        WRIT
070300970117     C                   Z-ADD     V1SDET        WDET
070400970117     C                   EXSR      MS2OUT
070500970117     C* CARICA IL SUBFILE
070600970117     C                   EXSR      POSSF2
070700970117     C*
070800970117     C                   ENDSR
070900970117     C*------------------------------------------------------------------------*
071000970117     C* SETSF2 - IMPOSTAZIONI INDICATORI SECONDO SUBFILE
071100970117     C*------------------------------------------------------------------------*
071200970117     C     SETSF2        BEGSR
071300970117     C*
071400970117     C                   MOVEL     '2'           TIPVID                         *VIDEO IN GESTIONE
071500970117     C                   MOVEL     '2'           DEPVID                         *VIDEO IN GESTIONE
071600970117     C                   SETOFF                                       22         ERRORI SFILE
071700970117     C                   SETOFF                                       90         GENERICO DI ERRORE
071800970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
071900970117     C*
072000970117     C                   ENDSR
072100970117     C*------------------------------------------------------------------------*
072200970117     C* SETSC2 - IMPOSTAZIONI INDICATORI E CAMPI SECONDO SUBFILE CONTROL
072300970117     C*------------------------------------------------------------------------*
072400970117     C     SETSC2        BEGSR
072500970117     C*
072600970117     C                   MOVEL     V1SCTS        V2CCTS
072700970117     C                   MOVEL     V1SDCT        V2CDCT
072800970117     C*
072900970117     C                   ENDSR
073000970117     C*------------------------------------------------------------------------*
073100970117     C* GESSF2 - GESTIONE SECONDO SUBFILE
073200970117     C*------------------------------------------------------------------------*
073300970117     C     GESSF2        BEGSR
073400970117     C*
073500970117     C* EMISSIONE VIDEATA
073600970117IF  1C     $TESVD        IFEQ      '1'
073700970117     C                   WRITE     TE20D00                                      *TESTATA
073800970117     C                   MOVEL     '0'           $TESVD
073900970117E   1C                   ENDIF
074000970117     C                   WRITE     TE20Z02                                      *PIEDI
074100970117     C                   EXFMT     TE20C02                                      *SFILE
074200970117     C                   EXSR      SETSF2
074300970117     C* F03-FINE
074400970117IF  1C     *INKC         IFEQ      *ON
074500970117     C                   EXSR      F03GES
074600970117     C                   GOTO      FINSF2
074700970117E   1C                   ENDIF
074800970117     C* F12-RITORNO
074900970117IF  1C     *INKL         IFEQ      *ON
075000970117     C                   MOVEL     '1'           TIPVID
075100970117     C                   GOTO      FINSF2
075200970117E   1C                   ENDIF
075300970117     C* CONTROLLA VIDEO
075400970117     C                   EXSR      CTRSF2
075500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
075600970117     C                   GOTO      FINSF2
075700970117E   1C                   ENDIF
075800970117     C* ENTER-RITORNA
075900970117     C                   MOVEL     '1'           TIPVID
076000970117IF  1C     DEPVID        IFEQ      '3'
076100970117     C                   MOVEL     '2'           TIPVID
076200970117E   1C                   ENDIF
076300970117     C*
076400970117     C     FINSF2        ENDSR
076500970117     C*------------------------------------------------------------------------*
076600970117     C* CTRSF2 - CONTROLLA SECONDO SUBFILE
076700970117     C*------------------------------------------------------------------------*
076800970117     C     CTRSF2        BEGSR
076900970117     C*
077000970117     C* REIMPOSTA VARIABILI DI LAVORO
077100970117     C                   MOVEL     '0'           WERR                           *ERRORE
077200970117     C*
077300970117     C* LEGGE TUTTI I RECORD DEL SFILE
077400970117     C                   Z-ADD     1             NR2
077500970117     C     NR2           CHAIN     TE20S02                            98
077600970117DO  1C     *IN98         DOWEQ     *OFF
077700970117     C                   SETOFF                                       70        *NO SFLNXTCHG
077800970117     C   11              MOVEL     DU2OPZ        V2SOPZ                         *DI DUPLICA
077900970117     C*--------------------
078000970117     C* OPZIONE
078100970117     C*--------------------
078200970117IF  2C     WERR          IFEQ      '0'
078300970117IF  3C     V2SOPZ        IFEQ      *BLANKS
078400970117     C     V2SOPZ        OREQ      '5 '
078500970117     C                   ELSE
078600970117     C                   MOVEL     '1'           WERR
078700970117     C                   SETON                                        2290
078800970117     C                   MOVEL     MSG(11)       $MSG
078900970117E   3C                   ENDIF
079000970117E   2C                   ENDIF
079100970117     C*
079200970117IF  2C     WERR          IFEQ      '1'
079300970117     C                   Z-ADD     NR2           $NR2                           *POSIZIONAMENTO
079400970117E   2C                   ENDIF
079500970117     C* GESTIONE OPZIONI
079600970117     C                   EXSR      GESOP2
079700970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
079800970117     C                   GOTO      FINCT2                                       *ESCE
079900970117X   2C                   ELSE                                                   *N O N  ERRORE
080000970117     C                   MOVEL     V2SOPZ        DU2OPZ                         *DI DUPLICA
080100970117     C                   ADD       1             NR2                            *LEGGE ALTRO RECORD
080200970117     C     NR2           CHAIN     TE20S02                            98
080300970117E   2C                   ENDIF
080400970117E   1C                   ENDDO                                                  *FINE SUBFILE
080500970117     C*
080600970117     C     FINCT2        ENDSR
080700970117     C*------------------------------------------------------------------------*
080800970117     C* GESOP2 - GESTIONE OPZIONI DEL SECONDO SUBFILE
080900970117     C*------------------------------------------------------------------------*
081000970117     C     GESOP2        BEGSR
081100970117     C*
081200970117SL  1C                   SELECT
081300970117     C     V2SOPZ        WHENEQ    '5 '
081400970117     C                   EXSR      O05GE2                                       *VISUALIZZA VOCI
081500970117     C                   OTHER
081600970117E   1C                   ENDSL
081700970117     C*
081800970117     C                   ENDSR
081900970117     C*------------------------------------------------------------------------*
082000970117     C* O05GE2 - GESTIONE OPZIONE SECONDO SFILE 5 ==> VISUALIZZA VOCI
082100970117     C*------------------------------------------------------------------------*
082200970117     C     O05GE2        BEGSR
082300970117     C*
082400970117     C* AGGIORNA 1° SFILE
082500970117     C                   EXSR      AGGSF2
082600970117     C*
082700970117     C* GESTIONE 3° SFILE
082800970117     C                   Z-ADD     NR2           $DPC2                          *DEPOSITO NRR
082900970117     C                   EXSR      IMPSF3                                       *IMPOSTAZIONI
083000970117     C                   EXSR      GESSF3                                       *GESTIONE
083100970117     C                   Z-ADD     $DPC2         NR2                            *DEPOSITO NRR
083200970117     C*
083300970117     C                   ENDSR
083400970117     C*------------------------------------------------------------------------*
083500970117     C* AGGSF2 - AGGIORNA RIGA DEL SECONDO SUBFILE
083600970117     C*------------------------------------------------------------------------*
083700970117     C     AGGSF2        BEGSR
083800970117     C*
083900970117     C                   MOVEL     *BLANKS       V2SOPZ
084000970117     C                   SETON                                        70        *SI SFLNXTCHG
084100970117     C                   UPDATE    TE20S02                                      *AGGIORNA SUBFILE
084200970117     C                   SETOFF                                       70        *NO SFLNXTCHG
084300970117     C*
084400970117     C                   ENDSR
084500970117     C*------------------------------------------------------------------------*
084600970117     C* IMPSF3 - IMPOSTAZIONI INIZIALI TERZO SUBFILE
084700970117     C*------------------------------------------------------------------------*
084800970117     C     IMPSF3        BEGSR
084900970117     C*
085000970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
085100970117     C                   EXSR      SETSF3                                       *SFILE
085200970117     C*
085300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
085400970117     C                   EXSR      SETSC3                                       *SFILE CONTROL
085500970117     C* CARICA LE SCHIERE
085600970117     C     $DPC1         CHAIN     TE20S01                            98        *RILEGGE COD.TASS.
085700970117     C     $DPC2         CHAIN     TE20S02                            98        *RILEGGE SCAGLIONE
085800970117     C                   MOVEL     V1SCTS        WCTS
085900970117     C                   Z-ADD     V2SSGL        WSGL
086000970117     C                   Z-ADD     V2SCOT        WCOM                           *COMPETENZA PORTO+AC
086100970117     C                   Z-ADD     V2SRIT        WRIC                           *RICAVO     PORTO+AC
086200970117     C                   Z-ADD     V2SDET        WDEL                           *DELTA      PORTO+AC
086300970117     C                   EXSR      MS3OUT
086400970117     C* CARICA IL SUBFILE
086500970117     C                   EXSR      POSSF3
086600970117     C*
086700970117     C                   ENDSR
086800970117     C*------------------------------------------------------------------------*
086900970117     C* SETSF3 - IMPOSTAZIONI INDICATORI TERZO SUBFILE
087000970117     C*------------------------------------------------------------------------*
087100970117     C     SETSF3        BEGSR
087200970117     C*
087300970117     C                   MOVEL     '3'           TIPVID                         *VIDEO IN GESTIONE
087400970117     C                   MOVEL     '3'           DEPVID                         *VIDEO IN GESTIONE
087500970117     C                   SETOFF                                       90         GENERICO DI ERRORE
087600970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
087700970117     C*
087800970117     C                   ENDSR
087900970117     C*------------------------------------------------------------------------*
088000970117     C* SETSC3 - IMPOSTAZIONI INDICATORI E CAMPI TERZO SUBFILE CONTROL
088100970117     C*------------------------------------------------------------------------*
088200970117     C     SETSC3        BEGSR
088300970117     C*
088400970117     C                   MOVEL     V1SCTS        V3CCTS
088500970117     C                   MOVEL     V1SDCT        V3CDCT
088600970117     C* EDITAZIONE SCAGLIONE
088700970117IF  1C     V2SSGL        IFGT      *ZEROS
088800970117     C                   Z-ADD     V2SSGL        $NUM             16 3          *NUMERO DA EDITARE
088900970117     C                   Z-ADD     3             $DEC              1 0          *NUMERO DECIMALI
089000020902    ? /COPY GAITRASRC/SRCTISE,TISE65T
089100970117     C                   MOVE(P)   $ALF          V3CDSG                         *NUMERO EDITATO
089200970117X   1C                   ELSE
089300970117     C                   MOVE(P)   'TOTALE'      V3CDSG
089400970117E   1C                   ENDIF
089500970117     C*
089600970117     C                   ENDSR
089700970117     C*------------------------------------------------------------------------*
089800970117     C* GESSF3 - GESTIONE TERZO SUBFILE
089900970117     C*------------------------------------------------------------------------*
090000970117     C     GESSF3        BEGSR
090100970117     C*
090200970117     C* EMISSIONE VIDEATA
090300970117IF  1C     $TESVD        IFEQ      '1'
090400970117     C                   WRITE     TE20D00                                      *TESTATA
090500970117     C                   MOVEL     '0'           $TESVD
090600970117E   1C                   ENDIF
090700970117     C                   WRITE     TE20Z03                                      *PIEDI
090800970117     C                   EXFMT     TE20C03                                      *SFILE
090900970117     C                   EXSR      SETSF3
091000970117     C* F03-FINE
091100970117IF  1C     *INKC         IFEQ      *ON
091200970117     C                   EXSR      F03GES
091300970117     C                   GOTO      FINSF3
091400970117E   1C                   ENDIF
091500970117     C* F12-RITORNO
091600970117IF  1C     *INKL         IFEQ      *ON
091700970117     C                   MOVEL     '2'           TIPVID
091800970117     C                   GOTO      FINSF3
091900970117E   1C                   ENDIF
092000970117     C* CONTROLLA VIDEO
092100970117     C                   EXSR      CTRSF3
092200970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
092300970117     C                   GOTO      FINSF3
092400970117E   1C                   ENDIF
092500970117     C* ENTER-RITORNA
092600970117     C                   MOVEL     '2'           TIPVID
092700970117     C*
092800970117     C     FINSF3        ENDSR
092900970117     C*------------------------------------------------------------------------*
093000970117     C* CTRSF3 - CONTROLLA TERZO SUBFILE
093100970117     C*------------------------------------------------------------------------*
093200970117     C     CTRSF3        BEGSR
093300970117     C*
093400970117     C     FINCT3        ENDSR
093500970117     C*------------------------------------------------------------------------*
093600970117     C* POSSF1 - CARICA IL PRIMO SUBFILE
093700970117     C*------------------------------------------------------------------------*
093800970117     C     POSSF1        BEGSR
093900970117     C*
094000970117     C* ELIMINA IL CONTENUTO DEL SFILE
094100970117     C                   EXSR      CLRSF1
094200970117     C*
094300970117     C* SI POSIZIONA SUL PRIMO RECORD DA CARICARE
094400970117     C                   EXSR      SETFI1
094500970117     C* CARICA IL SUBFILE
094600970117     C                   Z-ADD     *ZEROS        NR1                            *DALL'INIZIO
094700970117     C                   EXSR      CARSF1
094800970117     C*
094900970117     C                   ENDSR
095000970117     C*------------------------------------------------------------------------*
095100970117     C* CLRSF1 - ELIMINA IL CONTENUTO DEL PRIMO SUBFILE
095200970117     C*------------------------------------------------------------------------*
095300970117     C     CLRSF1        BEGSR
095400970117     C*
095500970117     C                   Z-ADD     1             $NR1
095600970117     C                   SETOFF                                       81        *NO SFLDSP
095700970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
095800970117     C                   WRITE     TE20C01
095900970117     C                   SETON                                        81        *SI SFLDSP
096000970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
096100970117     C*
096200970117     C                   ENDSR
096300970117     C*------------------------------------------------------------------------*
096400970117     C* SETFI1 - POSIZIONAMENTO RECORD -PRIMO SUBFILE-
096500970117     C*------------------------------------------------------------------------*
096600970117     C     SETFI1        BEGSR
096700970117     C*
096800970117     C                   Z-ADD     *ZEROS        SF1EOF                         *0=NO FINE/1=FINE
096900970117     C                   Z-ADD     *ZEROS        V                              *INDICE SCHIERE VIDE
097000970117     C*
097100970117     C                   ENDSR
097200970117     C*------------------------------------------------------------------------*
097300970117     C* CARSF1 - CARICA TUTTO IL PRIMO SUBFILE
097400970117     C*------------------------------------------------------------------------*
097500970117     C     CARSF1        BEGSR
097600970117     C*
097700050103DO  1C     V             DOWLT     601
097800970117     C                   EXSR      LETFI1                                       *LEGGE RECORD
097900970117     C                   SETOFF                                       80        *NO SFLNXTCHG
098000970117     C                   ADD       1             NR1
098100970117     C                   EXSR      IRESF1                                       *IMPOSTA RECORD SFIL
098200970117     C                   WRITE     TE20S01                                      *SCRIVE RECORD SFILE
098300970117E   1C                   ENDDO
098400970117     C*
098500970117     C                   ENDSR
098600970117     C*------------------------------------------------------------------------*
098700970117     C* LETFI1 - LETTURA SUCESSIVA RECORD -PRIMO SUBFILE-
098800970117     C*------------------------------------------------------------------------*
098900970117     C     LETFI1        BEGSR
099000970117     C*
099100970117     C* LEGGE FINO A:
099200970117     C*     - FINE FILE
099300970117     C*     - TROVATO RECORD VALIDO
099400970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
099500050103DO  1C     V             DOWLT     601
099600970117     C     WRECOK        ANDEQ     'N'
099700970117     C                   ADD       1             V                              *INDICE SCHIERE VIDE
099800970117     C                   EXSR      CHKRE1                                       *CONTROLLA RECORD
099900970117E   1C                   ENDDO
100000970117     C*
100100970117     C                   ENDSR
100200970117     C*------------------------------------------------------------------------*
100300970117     C* CHKRE1 - CONTROLLA VALIDITA' ELEMENTO SCHIERA PRIMO SFILE
100400970117     C*------------------------------------------------------------------------*
100500970117     C     CHKRE1        BEGSR
100600970117     C*
100700970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
100800970117     C*
100900970117     C* CODICE VUOTO, ESCLUDE
101000970117IF  1C     CTSP(V)       IFEQ      *BLANKS
101100970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101200970117E   1C                   ENDIF
101300970117     C*
101400970117     C                   ENDSR
101500970117     C*------------------------------------------------------------------------*
101600970117     C* IRESF1 - IMPOSTA CAMPI RECORD PRIMO SUBFILE
101700970117     C*------------------------------------------------------------------------*
101800970117     C     IRESF1        BEGSR
101900970117     C*
102000970117     C                   MOVEL     *BLANKS       V1SOPZ                         *CAMPI
102100970117     C                   MOVEL     CTSP(V)       V1SCTS
102200970117     C                   MOVEL     DCTP(V)       V1SDCT
102300970117     C                   Z-ADD     RIPP(V)       V1SRIP
102400970117     C                   Z-ADD     COPP(V)       V1SCOP
102500970117     C                   Z-ADD     DEPP(V)       V1SDEP
102600970117     C                   Z-ADD     RITP(V)       V1SRIT
102700970117     C                   Z-ADD     COTP(V)       V1SCOT
102800970117     C                   Z-ADD     DETP(V)       V1SDET
102900970117     C*
103000970117     C                   MOVEL     V1SOPZ        DU1OPZ                         *TASTI DI DUPLICA
103100970117     C*
103200970117     C                   ENDSR
103300970117     C*------------------------------------------------------------------------*
103400970117     C* POSSF2 - CARICA IL SECONDO SUBFILE
103500970117     C*------------------------------------------------------------------------*
103600970117     C     POSSF2        BEGSR
103700970117     C*
103800970117     C* ELIMINA IL CONTENUTO DEL SFILE
103900970117     C                   EXSR      CLRSF2
104000970117     C*
104100970117     C* SI POSIZIONA SUL PRIMO RECORD DA CARICARE
104200970117     C                   EXSR      SETFI2
104300970117     C* CARICA IL SUBFILE
104400970117     C                   Z-ADD     *ZEROS        NR2                            *DALL'INIZIO
104500970117     C                   EXSR      CARSF2
104600970117     C*
104700970117     C                   ENDSR
104800970117     C*------------------------------------------------------------------------*
104900970117     C* CLRSF2 - ELIMINA IL CONTENUTO DEL SECONDO SUBFILE
105000970117     C*------------------------------------------------------------------------*
105100970117     C     CLRSF2        BEGSR
105200970117     C*
105300970117     C                   Z-ADD     1             $NR2
105400970117     C                   SETOFF                                       71        *NO SFLDSP
105500970117     C                   SETOFF                                       72        *NO DSPCTL/SI SFLCLR
105600970117     C                   WRITE     TE20C02
105700970117     C                   SETON                                        71        *SI SFLDSP
105800970117     C                   SETON                                        72        *SI DSPCTL/NO SFLCLR
105900970117     C*
106000970117     C                   ENDSR
106100970117     C*------------------------------------------------------------------------*
106200970117     C* SETFI2 - POSIZIONAMENTO RECORD -SECONDO SUBFILE-
106300970117     C*------------------------------------------------------------------------*
106400970117     C     SETFI2        BEGSR
106500970117     C*
106600970117     C                   Z-ADD     *ZEROS        SF2EOF                         *0=NO FINE/1=FINE
106700970117     C                   Z-ADD     *ZEROS        V                              *INDICE SCHIERE VIDE
106800970117     C*
106900970117     C                   ENDSR
107000970117     C*------------------------------------------------------------------------*
107100970117     C* CARSF2 - CARICA TUTTO IL SECONDO SUBFILE
107200970117     C*------------------------------------------------------------------------*
107300970117     C     CARSF2        BEGSR
107400970117     C*
107500970117DO  1C     V             DOWLT     11
107600970117     C                   EXSR      LETFI2                                       *LEGGE RECORD
107700970117     C                   SETOFF                                       70        *NO SFLNXTCHG
107800970117     C                   ADD       1             NR2
107900970117     C                   EXSR      IRESF2                                       *IMPOSTA RECORD SFIL
108000970117     C                   WRITE     TE20S02                                      *SCRIVE RECORD SFILE
108100970117E   1C                   ENDDO
108200970117     C*
108300970117     C                   ENDSR
108400970117     C*------------------------------------------------------------------------*
108500970117     C* LETFI2 - LETTURA SUCESSIVA RECORD -SECONDO SUBFILE-
108600970117     C*------------------------------------------------------------------------*
108700970117     C     LETFI2        BEGSR
108800970117     C*
108900970117     C* LEGGE FINO A:
109000970117     C*     - FINE FILE
109100970117     C*     - TROVATO RECORD VALIDO
109200970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
109300970117DO  1C     V             DOWLT     11
109400970117     C     WRECOK        ANDEQ     'N'
109500970117     C                   ADD       1             V                              *INDICE SCHIERE VIDE
109600970117     C                   EXSR      CHKRE2                                       *CONTROLLA RECORD
109700970117E   1C                   ENDDO
109800970117     C*
109900970117     C                   ENDSR
110000970117     C*------------------------------------------------------------------------*
110100970117     C* CHKRE2 - CONTROLLA VALIDITA' ELEMENTO SCHIERA SECONDO SFILE
110200970117     C*------------------------------------------------------------------------*
110300970117     C     CHKRE2        BEGSR
110400970117     C*
110500970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
110600970117     C*
110700970117     C* IMPORTI VUOTI, ESCLUDE
110800970117IF  1C     COPS(V)       IFEQ      *ZEROS
110900970117     C     RIPS(V)       ANDEQ     *ZEROS
111000970117     C     DEPS(V)       ANDEQ     *ZEROS
111100970117     C     COTS(V)       ANDEQ     *ZEROS
111200970117     C     RITS(V)       ANDEQ     *ZEROS
111300970117     C     DETS(V)       ANDEQ     *ZEROS
111400970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111500970117E   1C                   ENDIF
111600970117     C*
111700970117     C                   ENDSR
111800970117     C*------------------------------------------------------------------------*
111900970117     C* IRESF2 - IMPOSTA CAMPI RECORD SECONDO SUBFILE
112000970117     C*------------------------------------------------------------------------*
112100970117     C     IRESF2        BEGSR
112200970117     C*
112300970117     C                   MOVEL     *BLANKS       V2SOPZ                         *CAMPI
112400970117     C                   Z-ADD     SGLS(V)       V2SSGL
112500970117     C                   Z-ADD     RIPS(V)       V2SRIP
112600970117     C                   Z-ADD     COPS(V)       V2SCOP
112700970117     C                   Z-ADD     DEPS(V)       V2SDEP
112800970117     C                   Z-ADD     RITS(V)       V2SRIT
112900970117     C                   Z-ADD     COTS(V)       V2SCOT
113000970117     C                   Z-ADD     DETS(V)       V2SDET
113100970117     C*
113200970117     C                   MOVEL     V2SOPZ        DU2OPZ                         *TASTI DI DUPLICA
113300970117     C*
113400970117     C                   ENDSR
113500970117     C*------------------------------------------------------------------------*
113600970117     C* POSSF3 - CARICA IL TERZO SUBFILE
113700970117     C*------------------------------------------------------------------------*
113800970117     C     POSSF3        BEGSR
113900970117     C*
114000970117     C* ELIMINA IL CONTENUTO DEL SFILE
114100970117     C                   EXSR      CLRSF3
114200970117     C*
114300970117     C* SI POSIZIONA SUL PRIMO RECORD DA CARICARE
114400970117     C                   EXSR      SETFI3
114500970117     C* CARICA IL SUBFILE
114600970117     C                   Z-ADD     *ZEROS        NR3                            *DALL'INIZIO
114700970117     C                   EXSR      CARSF3
114800970117     C*
114900970117     C                   ENDSR
115000970117     C*------------------------------------------------------------------------*
115100970117     C* CLRSF3 - ELIMINA IL CONTENUTO DEL TERZO SUBFILE
115200970117     C*------------------------------------------------------------------------*
115300970117     C     CLRSF3        BEGSR
115400970117     C*
115500970117     C                   Z-ADD     1             $NR3
115600970117     C                   SETOFF                                       61        *NO SFLDSP
115700970117     C                   SETOFF                                       62        *NO DSPCTL/SI SFLCLR
115800970117     C                   WRITE     TE20C03
115900970117     C                   SETON                                        61        *SI SFLDSP
116000970117     C                   SETON                                        62        *SI DSPCTL/NO SFLCLR
116100970117     C*
116200970117     C                   ENDSR
116300970117     C*------------------------------------------------------------------------*
116400970117     C* SETFI3 - POSIZIONAMENTO RECORD -TERZO SUBFILE-
116500970117     C*------------------------------------------------------------------------*
116600970117     C     SETFI3        BEGSR
116700970117     C*
116800970117     C                   Z-ADD     *ZEROS        SF3EOF                         *0=NO FINE/1=FINE
116900970117     C                   Z-ADD     *ZEROS        V                              *INDICE SCHIERE VIDE
117000970117     C*
117100970117     C                   ENDSR
117200970117     C*------------------------------------------------------------------------*
117300970117     C* CARSF3 - CARICA TUTTO IL TERZO SUBFILE
117400970117     C*------------------------------------------------------------------------*
117500970117     C     CARSF3        BEGSR
117600970117     C*
117700970117DO  1C     V             DOWLT     31
117800970117     C                   EXSR      LETFI3                                       *LEGGE RECORD
117900970117     C                   SETOFF                                       60        *NO SFLNXTCHG
118000970117     C                   ADD       1             NR3
118100970117     C                   EXSR      IRESF3                                       *IMPOSTA RECORD SFIL
118200970117     C                   WRITE     TE20S03                                      *SCRIVE RECORD SFILE
118300970117E   1C                   ENDDO
118400970117     C*
118500970117     C                   ENDSR
118600970117     C*------------------------------------------------------------------------*
118700970117     C* LETFI3 - LETTURA SUCESSIVA RECORD -TERZO SUBFILE-
118800970117     C*------------------------------------------------------------------------*
118900970117     C     LETFI3        BEGSR
119000970117     C*
119100970117     C* LEGGE FINO A:
119200970117     C*     - FINE FILE
119300970117     C*     - TROVATO RECORD VALIDO
119400970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
119500970117DO  1C     V             DOWLT     31
119600970117     C     WRECOK        ANDEQ     'N'
119700970117     C                   ADD       1             V                              *INDICE SCHIERE VIDE
119800970117     C                   EXSR      CHKRE3                                       *CONTROLLA RECORD
119900970117E   1C                   ENDDO
120000970117     C*
120100970117     C                   ENDSR
120200970117     C*------------------------------------------------------------------------*
120300970117     C* CHKRE3 - CONTROLLA VALIDITA' ELEMENTO SCHIERA TERZO SFILE
120400970117     C*------------------------------------------------------------------------*
120500970117     C     CHKRE3        BEGSR
120600970117     C*
120700970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
120800970117     C*
120900970117     C* IMPORTI VUOTI, ESCLUDE
121000970117IF  1C     COMT(V)       IFEQ      *ZEROS
121100970117     C     RICT(V)       ANDEQ     *ZEROS
121200970117     C     DELT(V)       ANDEQ     *ZEROS
121300970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
121400970117E   1C                   ENDIF
121500970117     C*
121600970117     C                   ENDSR
121700970117     C*------------------------------------------------------------------------*
121800970117     C* IRESF3 - IMPOSTA CAMPI RECORD TERZO SUBFILE
121900970117     C*------------------------------------------------------------------------*
122000970117     C     IRESF3        BEGSR
122100970117     C*
122200970117     C                   Z-ADD     RDLT(V)       V3SRDL                         *CAMPI
122300970117     C                   Z-ADD     RICT(V)       V3SRIC
122400970117     C                   Z-ADD     COMT(V)       V3SCOM
122500970117     C                   Z-ADD     DELT(V)       V3SDEL
122600970117     C* DECODIFICHE
122700970117     C                   MOVEL     *BLANKS       V3SDRD                         *DESCR.RAGGRUP.DELTA
122800970117     C                   MOVEL     *BLANKS       V3STIP                         *TIPO RAGGRUPPAMENTO
122900970117IF  1C     RDLT(V)       IFGT      *ZEROS
123000970117     C                   Z-ADD     1             J
123100970117     C                   SETOFF                                           98
123200970117     C     RDLT(V)       LOOKUP    RDL(J)                                 98
123300970117IF  2C     *IN98         IFEQ      *ON
123400970117     C                   MOVEL     DRD(J)        V3SDRD
123500970117     C                   MOVEL     TIP(J)        V3STIP
123600970117E   2C                   ENDIF
123700970117X   1C                   ELSE
123800970117     C                   MOVEL(P)  'TOTALE'      V3SDRD
123900970117     C                   MOVEL(P)  *BLANKS       V3STIP
124000970117E   1C                   ENDIF
124100970117     C*
124200970117     C                   ENDSR
124300970117     C*------------------------------------------------------------------------*
124400970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
124500970117     C*------------------------------------------------------------------------*
124600970117     C     CARCC         BEGSR
124700970117     C*
124800970117     C* RECUPERA I DATI DELL'UTENTE
124900020906     C                   Z-ADD     1             CODUT
125000020830     C                   CALL      'X§PARUT'
125100970117     C                   PARM                    PARUT
125200970117     C*
125300970117     C                   Z-ADD     1             Z
125400970117DO  1C                   DO        50            X2                2 0
125500970117     C                   MOVE      TCU(X2)       CARCNT
125600970117IF  2C     TIPCNT        IFEQ      'CI'
125700970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
125800970117E   2C                   ENDIF
125900970117IF  2C     FL34CN        IFEQ      '01'
126000970117     C     TIPCNT        ANDEQ     'CG'
126100970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
126200970117E   2C                   ENDIF
126300970117IF  2C     FL34CN        IFEQ      '02'
126400970117     C     TIPCNT        ANDEQ     'CG'
126500970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
126600970117E   2C                   ENDIF
126700970117IF  2C     TIPCNT        IFEQ      'RI'
126800970117     C                   Z-ADD     KCU(X2)       RI(Z)
126900970117     C                   ADD       1             Z                 3 0
127000970117E   2C                   ENDIF
127100970117E   1C                   ENDDO
127200970117     C*
127300970117     C                   ENDSR
127400970117     C*------------------------------------------------------------------------*
127500970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
127600970117     C*------------------------------------------------------------------------*
127700970117     C     CARTAB        BEGSR
127800970117     C*--------------------
127900970117     C* RAGGRUPPAMENTI DELTA
128000970117     C*--------------------
128100970117     C                   Z-ADD     *ZEROS        I                 4 0
128200020906     C                   Z-ADD     1             KTBKUT
128300970117     C                   MOVEL     '9B'          KTBCOD
128400970117     C     KE2TAB        SETLL     TABEL00F
128500970117     C     KE2TAB        READE     TABEL00F                               99
128600970117DO  1C     *IN99         DOWEQ     *OFF
128700970117IF  2C     TBLFLG        IFEQ      *BLANKS
128800970117     C                   ADD       1             I
128900970117     C                   MOVEL     TBLUNI        DS9B
129000970117     C                   MOVEL     TBLKEY        RDL(I)                         *CODICE
129100970117     C                   MOVEL     §9BDES        DRD(I)                         *DESCRIZIONE
129200970117     C                   MOVEL     §9BTIP        TIP(I)                         *TIPO (P/A)
129300970117E   2C                   ENDIF
129400970117     C     KE2TAB        READE     TABEL00F                               99
129500970117E   1C                   ENDDO
129600970117     C*--------------------
129700970117     C* TIPI SERVIZIO
129800970117     C*--------------------
129900970117     C                   Z-ADD     *ZEROS        I                 4 0
130000020906     C                   Z-ADD     1             KTBKUT
130100970117     C                   MOVEL     '5E'          KTBCOD
130200970117     C     KE2TAB        SETLL     TABEL00F
130300970117     C     KE2TAB        READE     TABEL00F                               99
130400970117DO  1C     *IN99         DOWEQ     *OFF
130500970117IF  2C     TBLFLG        IFEQ      *BLANKS
130600970117     C                   ADD       1             I
130700970117     C                   MOVEL     TBLUNI        DS5E
130800970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
130900970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
131000970117E   2C                   ENDIF
131100970117     C     KE2TAB        READE     TABEL00F                               99
131200970117E   1C                   ENDDO
131300970117     C*--------------------
131400970117     C* TIPI TARIFFA
131500970117     C*--------------------
131600970117     C                   Z-ADD     *ZEROS        I                 4 0
131700020906     C                   Z-ADD     1             KTBKUT
131800970117     C                   MOVEL     'TR'          KTBCOD
131900970117     C     KE2TAB        SETLL     TABEL00F
132000970117     C     KE2TAB        READE     TABEL00F                               99
132100970117DO  1C     *IN99         DOWEQ     *OFF
132200970117IF  2C     TBLFLG        IFEQ      *BLANKS
132300970117     C                   MOVEL     TBLUNI        DSTR
132400970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
132500970117     C                   ADD       1             I
132600970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
132700970117     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
132800970117E   3C                   ENDIF
132900970117E   2C                   ENDIF
133000970117     C     KE2TAB        READE     TABEL00F                               99
133100970117E   1C                   ENDDO
133200970117     C*--------------------
133300970117     C* CODICI TASSAZIONE
133400970117     C*--------------------
133500970117     C                   Z-ADD     *ZEROS        I                 4 0
133600020906     C                   Z-ADD     1             KTBKUT
133700970117     C                   MOVEL     'CT'          KTBCOD
133800970117     C     KE2TAB        SETLL     TABEL00F
133900970117     C     KE2TAB        READE     TABEL00F                               99
134000970117DO  1C     *IN99         DOWEQ     *OFF
134100970117IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
134200970117     C                   MOVEL     TBLUNI        DSCT
134300970312IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
134400970117     C                   ADD       1             I
134500970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
134600970117     C                   MOVEL     §CTDES        DCT(I)                         *DESCRIZIONE
134700970117     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
134800970117     C                   MOVEL     §CTCOR        COR(I)                         *ORDINAMENTO
134900970117     C                   MOVE      §CTCOR        WA1                            *TIPO CODICE
135000970312IF  4C     WA1           IFEQ      '9'
135100970312IF  5C     CTS(I)        IFEQ      'IT'
135200970117     C     CTS(I)        OREQ      'EE'
135300970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
135400970312X   5C                   ELSE
135500970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
135600970312E   5C                   ENDIF
135700970312X   4C                   ELSE
135800970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
135900970312E   4C                   ENDIF
136000970312IF  4C     §CTNAR        IFEQ      *BLANKS
136100970117     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
136200970312X   4C                   ELSE
136300970117     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
136400970312E   4C                   ENDIF
136500970312E   3C                   ENDIF
136600970117E   2C                   ENDIF
136700970117     C     KE2TAB        READE     TABEL00F                               99
136800970117E   1C                   ENDDO
136900970117     C*
137000970117     C                   ENDSR
137100970117     C*------------------------------------------------------------------------*
137200970117     C* IMPVD0 - IMPOSTA LA TESTATA
137300970117     C*------------------------------------------------------------------------*
137400970117     C     IMPVD0        BEGSR
137500970117     C*
137600970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
137700970117     C*---
137800970117     C* TESTATA VIDEO
137900970117     C*---
138000970117IF  1C     D00OP0        IFEQ      'O02'
138100970117     C     D00OP0        OREQ      'O05'
138200970117     C                   MOVEL     'Visita '     V0DKOV                         *TARIFFA O OFFERTA
138300970117X   1C                   ELSE
138400970117     C                   MOVEL     'Cliente'     V0DKOV                         *TARIFFA O OFFERTA
138500970117E   1C                   ENDIF
138600970117     C* CAMPI
138700970117     C                   Z-ADD     D00KSC        V0CKSC
138800970117     C                   Z-ADD     D00CTR        V0CCTR
138900970117     C                   Z-ADD     D00PRG        V0CPRG
139000970117     C                   MOVEL     D00FIE        V0CFIE
139100980603     C                   MOVEL     D00DSF        V0CDSF
139200970117     C                   MOVEL     D00TSP        V0CTSP
139300970117     C                   Z-ADD     D00LNP        V0CLNP
139400011003     C                   MOVEL     D00DIV        V0CDIV
139500970117IF  1C     D00DTC        IFGT      *ZEROS
139600970117     C                   Z-ADD     D00DTC        G08INV
139700970117     C                   Z-ADD     *ZEROS        G08DAT
139800970117     C                   MOVEL     '3'           G08ERR
139900970117     C                   CALL      'XSRDA8'
140000970117     C                   PARM                    WLBDA8
140100970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
140200970117E   1C                   ENDIF
140300970117     C* DECODIFICHE
140400020906     C                   Z-ADD     1             KACKUT                         *ANAGRAFICA
140500970512     C                   Z-ADD     KCCCL         KACKCC
140600970512     C                   Z-ADD     D00KSC        KACKSC
140700970512IF  1C     D00CTO        IFEQ      'T'                                          *TARIFFA
140800970512     C     KEYACO        CHAIN     CNACO00F                           99
140900970512X   1C                   ELSE                                                   *OFFERTA
141000970512     C     KEYACO        CHAIN     TFACO00F                           99
141100970512IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
141200970512     C                   Z-ADD     D00KSC        KVINRV
141300970512     C     KEYVIS        CHAIN     TNVIS05L                           99        *VISITE
141400970512     C                   Z-ADD     VISKSC        KACKSC                         *CLIENTE DELLA VISIT
141500970512     C     KEYACO        CHAIN     CNACO00F                           99
141600970512E   2C                   ENDIF
141700970512E   1C                   ENDIF
141800970512     C                   MOVEL(P)  ACORAG        V0DKSC                         *CLIENTE
141900970117     C*
142000970117     C                   MOVEL     *BLANKS       V0DCTR
142100970117IF  1C     D00KSC        IFGT      *ZEROS
142200970117     C                   MOVEL     D00CTR        WA1               1
142300970117     C                   SETOFF                                           98
142400970117     C                   Z-ADD     1             I
142500970117     C     WA1           LOOKUP    TTR(I)                                 98
142600970117IF  2C     *IN98         IFEQ      *ON
142700970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
142800970117E   2C                   ENDIF
142900970117E   1C                   ENDIF
143000970117     C*
143100970117     C                   MOVEL     *BLANKS       V0DTSP
143200970117IF  1C     D00TSP        IFNE      *BLANKS
143300970117     C                   SETOFF                                           98
143400970117     C                   Z-ADD     1             I
143500970117     C     D00TSP        LOOKUP    TSP(I)                                 98
143600970117IF  2C     *IN98         IFEQ      *ON
143700970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
143800970117E   2C                   ENDIF
143900970117E   1C                   ENDIF
144000970117     C*
144100970117     C                   MOVEL     *BLANKS       V0DFIE
144200970117IF  1C     D00FIE        IFEQ      'I'
144300970117     C                   MOVEL     DVID(1)       V0DFIE                         *FLAG ITALIA/ESTERO
144400970117E   1C                   ENDIF
144500970117IF  1C     D00FIE        IFEQ      'E'
144600970117     C                   MOVEL     DVID(2)       V0DFIE                         *FLAG ITALIA/ESTERO
144700970117E   1C                   ENDIF
144800980603     C*
144900980603     C                   MOVEL     *BLANKS       V0DDSF
145000980603IF  1C     D00DSF        IFEQ      'S'
145100980603     C                   MOVEL     DVID(3)       V0DDSF                         *DATI SEDE O FILIALE
145200980603E   1C                   ENDIF
145300980603IF  1C     D00DSF        IFEQ      'F'
145400980603     C                   MOVEL     DVID(4)       V0DDSF                         *DATI SEDE O FILIALE
145500980603E   1C                   ENDIF
145600970117     C*
145700970117     C                   MOVEL     *BLANKS       V0DLNP
145800970117IF  1C     D00LNP        IFGT      *ZEROS
145900970117     C                   Z-ADD     D00LNP        KORFIL
146000970117     C     KEYORG        CHAIN     AZORG01L                           99
146100970117IF  2C     *IN99         IFEQ      *OFF
146200970117     C     ORGFVA        ANDEQ     *BLANKS
146300970117     C     ORGFAG        ANDNE     'V'
146400970117     C                   MOVEL     ORGDES        V0DLNP
146500970117E   2C                   ENDIF
146600970117E   1C                   ENDIF
146700970117     C*---
146800970117     C* TESTATA STAMPA
146900970117     C*---
147000970117     C                   MOVEL     V0DKOV        INCKOV
147100970117     C                   Z-ADD     V0CKSC        INCKSC
147200970117     C                   MOVEL     V0DKSC        INDKSC
147300970117     C                   Z-ADD     V0CCTR        INCCTR
147400970117     C                   MOVEL     V0DCTR        INDCTR
147500970117     C                   Z-ADD     V0CPRG        INCPRG
147600970117     C                   Z-ADD     V0CLNP        INCLNP
147700970117     C                   MOVEL     V0DLNP        INDLNP
147800011003     C                   MOVEL     V0CDIV        INCDIV
147900970117     C*
148000970117     C                   ENDSR
148100970117     C*------------------------------------------------------------------------*
148200970117     C* MEMETW - MEMORIZZA LE SCHIERE DAL WORKFILE
148300970117     C*------------------------------------------------------------------------*
148400970117     C     MEMETW        BEGSR
148500970117     C*
148600970117     C* LEGGE IL FILE DI LAVORO
148700980909     C******************** MOVELUSER      KTWUSR           *NUME UTENTE
148800980909     C                   MOVEL     JOB           KTWUSR                         *NUME UTENTE
148900980603     C                   MOVEL     D00DSF        KTWDSF                         *DATI SEDE O FILIALE
149000980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
149100970117     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
149200970117     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
149300970117     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
149400011001     C     KEYETW        SETLL     TEETW02L                               99
149500970117IF  1C     *IN99         IFEQ      *OFF                                         *DATI INESISTENTI
149600970117     C                   MOVEL     MSG(1)        D00MSG
149700970117     C                   MOVEL     '1'           D00ERR
149800970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
149900970117X   1C                   ELSE                                                   *DATI ESISTENTI
150000011001     C     KEYETW        READE     TEETW02L                               99
150100970117DO  2C     *IN99         DOWEQ     *OFF
150200970117     C*
150300970117     C* OPERAZIONI PER NUOVA NAZIONE DI TASSAZIONE
150400970117     C                   EXSR      NEWNAR
150500970117     C*
150600970117     C* CICLO FINO A ROTTURA NAZIONE
150700970117DO  3C     *IN99         DOWEQ     *OFF
150800970117     C     DEPNAR        ANDEQ     ETWNAR
150900970117     C*
151000970117     C* OPERAZIONI PER NUOVA REGIONE DI TASSAZIONE
151100970117     C                   EXSR      NEWRAP
151200970117     C*
151300970117     C* CICLO FINO A ROTTURA REGIONE
151400970117DO  4C     *IN99         DOWEQ     *OFF
151500970117     C     DEPNAR        ANDEQ     ETWNAR
151600970117     C     DEPRAP        ANDEQ     ETWRAP
151700970117     C*
151800970117     C* OPERAZIONI PER NUOVO CODICE TASSAZIONE
151900970117     C                   EXSR      NEWCTS
152000970117     C*
152100970117     C* CICLO FINO A ROTTURA CODICE TASSAZIONE
152200970117DO  5C     *IN99         DOWEQ     *OFF
152300970117     C     DEPNAR        ANDEQ     ETWNAR
152400970117     C     DEPRAP        ANDEQ     ETWRAP
152500970117     C     DEPCTS        ANDEQ     ETWCTS
152600970117     C*
152700970117     C* OPERAZIONI PER NUOVO SCAGLIONE
152800970117     C                   EXSR      NEWSGL
152900970117     C*
153000970117     C* CICLO FINO A ROTTURA SCAGLIONE
153100970117DO  6C     *IN99         DOWEQ     *OFF
153200970117     C     DEPNAR        ANDEQ     ETWNAR
153300970117     C     DEPRAP        ANDEQ     ETWRAP
153400970117     C     DEPCTS        ANDEQ     ETWCTS
153500970117     C     DEPSGL        ANDEQ     ETWSGL
153600970117     C*
153700970117     C* OPERAZIONI PER NUOVO RAGGRUPPAMENTO DELTA
153800970117     C                   EXSR      NEWRDL
153900970117     C*
154000970117     C* CICLO FINO A ROTTURA RAGGRUPPAMENTO DELTA
154100970117DO  7C     *IN99         DOWEQ     *OFF
154200970117     C     DEPNAR        ANDEQ     ETWNAR
154300970117     C     DEPRAP        ANDEQ     ETWRAP
154400970117     C     DEPCTS        ANDEQ     ETWCTS
154500970117     C     DEPSGL        ANDEQ     ETWSGL
154600970117     C     DEPRDL        ANDEQ     ETWRDL
154700970117     C*
154800970117     C* MEMORIZZA I DATI
154900970117     C                   EXSR      MEMDAT
155000970117     C*
155100970117     C* LETTURA SUCESSIVA WORKFILE
155200011001     C     KEYETW        READE     TEETW02L                               99
155300970117E   7C                   ENDDO                                                  *ROTTURA RAGGR.DELTA
155400970117E   6C                   ENDDO                                                  *ROTTURA SCAGLIONE
155500970117     C*
155600970117     C* MEMORIZZA I DATI PER SCAGLIONE
155700970117     C                   EXSR      MEMSGL
155800970117E   5C                   ENDDO                                                  *ROTTURA CODICE
155900970117E   4C                   ENDDO                                                  *ROTTURA REGIONE
156000970117E   3C                   ENDDO                                                  *ROTTURA NAZIONE
156100970117E   2C                   ENDDO                                                  *FINE FILE
156200970117E   1C                   ENDIF
156300970117     C*
156400970117     C                   ENDSR
156500970117     C*------------------------------------------------------------------------*
156600970117     C* MEMDAT - MEMORIZZA I DATI
156700970117     C*------------------------------------------------------------------------*
156800970117     C     MEMDAT        BEGSR
156900970117     C*
157000970117     C* DEPOSITA I DATI -PER SCAGLIONE-
157100970117     C                   Z-ADD     ETWPPE        SGLPPE                         *% PESO ASSOLUTA
157200970117     C                   Z-ADD     1             J
157300970117     C     ETWRDL        LOOKUP    RDL(J)                                 98
157400970117IF  1C     TIP(J)        IFEQ      'P'
157500970117     C     SGLCOP        ADD       ETWCOM        SGLCOP                         *COMPETENZE PORTO
157600970117     C     SGLRIP        ADD       ETWRIC        SGLRIP                         *RICAVI     PORTO
157700970117E   1C                   ENDIF
157800970117     C     SGLCOT        ADD       ETWCOM        SGLCOT                         *COMPETENZE PORTO+AC
157900970117     C     SGLRIT        ADD       ETWRIC        SGLRIT                         *RICAVI     PORTO+AC
158000970117     C*
158100970117     C* MEMORIZZA I DATI PER -CODICE TASSAZIONE-
158200970117     C     C             OCCUR     DRDL                                         V O C E
158300970117IF  1C     S             IFEQ      1                                            *1° SCAGLIONE
158400970117     C     TCO1(R)       ADD       ETWCOM        TCO1(R)                        *COMPETENZA
158500970117     C     TRI1(R)       ADD       ETWRIC        TRI1(R)                        *RICAVO
158600970117E   1C                   ENDIF
158700970117IF  1C     S             IFEQ      2                                            *2° SCAGLIONE
158800970117     C     TCO2(R)       ADD       ETWCOM        TCO2(R)                        *COMPETENZA
158900970117     C     TRI2(R)       ADD       ETWRIC        TRI2(R)                        *RICAVO
159000970117E   1C                   ENDIF
159100970117IF  1C     S             IFEQ      3                                            *3° SCAGLIONE
159200970117     C     TCO3(R)       ADD       ETWCOM        TCO3(R)                        *COMPETENZA
159300970117     C     TRI3(R)       ADD       ETWRIC        TRI3(R)                        *RICAVO
159400970117E   1C                   ENDIF
159500970117IF  1C     S             IFEQ      4                                            *4° SCAGLIONE
159600970117     C     TCO4(R)       ADD       ETWCOM        TCO4(R)                        *COMPETENZA
159700970117     C     TRI4(R)       ADD       ETWRIC        TRI4(R)                        *RICAVO
159800970117E   1C                   ENDIF
159900970117IF  1C     S             IFEQ      5                                            *5° SCAGLIONE
160000970117     C     TCO5(R)       ADD       ETWCOM        TCO5(R)                        *COMPETENZA
160100970117     C     TRI5(R)       ADD       ETWRIC        TRI5(R)                        *RICAVO
160200970117E   1C                   ENDIF
160300970117IF  1C     S             IFEQ      6                                            *6° SCAGLIONE
160400970117     C     TCO6(R)       ADD       ETWCOM        TCO6(R)                        *COMPETENZA
160500970117     C     TRI6(R)       ADD       ETWRIC        TRI6(R)                        *RICAVO
160600970117E   1C                   ENDIF
160700970117IF  1C     S             IFEQ      7                                            *7° SCAGLIONE
160800970117     C     TCO7(R)       ADD       ETWCOM        TCO7(R)                        *COMPETENZA
160900970117     C     TRI7(R)       ADD       ETWRIC        TRI7(R)                        *RICAVO
161000970117E   1C                   ENDIF
161100970117IF  1C     S             IFEQ      8                                            *8° SCAGLIONE
161200970117     C     TCO8(R)       ADD       ETWCOM        TCO8(R)                        *COMPETENZA
161300970117     C     TRI8(R)       ADD       ETWRIC        TRI8(R)                        *RICAVO
161400970117E   1C                   ENDIF
161500970117IF  1C     S             IFEQ      9                                            *9° SCAGLIONE
161600970117     C     TCO9(R)       ADD       ETWCOM        TCO9(R)                        *COMPETENZA
161700970117     C     TRI9(R)       ADD       ETWRIC        TRI9(R)                        *RICAVO
161800970117E   1C                   ENDIF
161900970117IF  1C     S             IFEQ      10                                           *10°SCAGLIONE
162000970117     C     TCO0(R)       ADD       ETWCOM        TCO0(R)                        *COMPETENZA
162100970117     C     TRI0(R)       ADD       ETWRIC        TRI0(R)                        *RICAVO
162200970117E   1C                   ENDIF
162300970117     C*
162400970117     C                   ENDSR
162500970117     C*------------------------------------------------------------------------*
162600970117     C* MEMSGL - MEMORIZZA I DATI PER SCAGLIONE
162700970117     C*------------------------------------------------------------------------*
162800970117     C     MEMSGL        BEGSR
162900970117     C*
163000970117     C     S             OCCUR     DSGL                                         S C A G L I O N E
163100970117     C                   Z-ADD     SGLPPE        TPPA(C)                        *% PESO
163200970117     C                   Z-ADD     SGLCOP        TCOP(C)                        *COMPETENZA PORTO
163300970117     C                   Z-ADD     SGLRIP        TRIP(C)                        *RICAVI     PORTO
163400970117     C                   Z-ADD     SGLCOT        TCOT(C)                        *COMPETENZA PORTO+AC
163500970117     C                   Z-ADD     SGLRIT        TRIT(C)                        *RICAVI     PORTO+AC
163600970117     C*
163700970117     C                   ENDSR
163800970117     C*------------------------------------------------------------------------*
163900970117     C* NEWNAR - OPERAZIONI PER NUOVA NAZIONE DI TASSAZIONE
164000970117     C*------------------------------------------------------------------------*
164100970117     C     NEWNAR        BEGSR
164200970117     C*
164300970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
164400970117     C                   MOVEL     ETWNAR        DEPNAR
164500970117     C*
164600970117     C                   ENDSR
164700970117     C*------------------------------------------------------------------------*
164800970117     C* NEWRAP - OPERAZIONI PER NUOVA REGIONE DI TASSAZIONE
164900970117     C*------------------------------------------------------------------------*
165000970117     C     NEWRAP        BEGSR
165100970117     C*
165200970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
165300970117     C                   MOVEL     ETWRAP        DEPRAP
165400970117     C*
165500970117     C                   ENDSR
165600970117     C*------------------------------------------------------------------------*
165700970117     C* NEWCTS - OPERAZIONI PER NUOVO CODICE TASSAZIONE
165800970117     C*------------------------------------------------------------------------*
165900970117     C     NEWCTS        BEGSR
166000970117     C*
166100970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
166200970117     C                   MOVEL     ETWCTS        DEPCTS
166300970117     C*
166400970117     C* MEMORIZZA IL CODICE TASSAZIONE "VERO" DA EMETTERE A VIDEO
166500970117IF  1C     ETWRCT        IFEQ      'T'
166600970117     C                   MOVEL     ETWCTS        VCTS              2
166700970117E   1C                   ENDIF
166800970117IF  1C     ETWRCT        IFEQ      'R'
166900970117     C                   MOVEL     ETWRAP        VCTS
167000970117E   1C                   ENDIF
167100970117IF  1C     ETWRCT        IFEQ      'N'
167200970117     C                   MOVEL     ETWNAR        VCTS
167300970117E   1C                   ENDIF
167400970117     C                   Z-ADD     1             J                 4 0
167500970117     C                   SETOFF                                           98
167600970117     C     VCTS          LOOKUP    CTSP(J)                                98
167700970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
167800970117     C                   ADD       1             V
167900970117     C                   MOVEL     VCTS          CTSP(V)                        *CODICE
168000970117     C                   Z-ADD     1             J                 4 0
168100970117     C                   SETOFF                                           98
168200970117     C     VCTS          LOOKUP    CTS(J)                                 98
168300970117IF  2C     *IN98         IFEQ      *ON
168400970117     C                   MOVEL     DCT(J)        DCTP(V)                        *DESCRIZIONE
168500970117     C                   MOVEL     TCT(J)        TCTP(V)                        *TIPO (E/R/N)
168600970117     C                   MOVEL     COR(J)        CORP(V)                        *ORDINAMENTO
168700970117E   2C                   ENDIF
168800970117E   1C                   ENDIF
168900970117     C*
169000970117     C* MEMORIZZA IL CODICE TASSAZIONE IN SCHIERA
169100970117     C                   SETOFF                                           98
169200970117     C                   Z-ADD     1             J
169300970117     C     ETWCTS        LOOKUP    TCTS(J)                                98
169400970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
169500970117     C                   ADD       1             CC                4 0
169600970117     C                   Z-ADD     CC            C                 4 0
169700970117     C                   MOVEL     ETWCTS        TCTS(C)                        *CODICE
169800970117     C                   MOVEL     ETWRAP        TRAP(C)                        *REGIONE
169900970117     C                   MOVEL     ETWNAR        TNAR(C)                        *NAZIONE
170000970117     C                   MOVEL     ETWRCT        TRCT(C)                        *RAGGRUPPAMENTO
170100970117X   1C                   ELSE
170200970117     C                   Z-ADD     J             C
170300970117E   1C                   ENDIF
170400970117     C*
170500970117     C                   ENDSR
170600970117     C*------------------------------------------------------------------------*
170700970117     C* NEWSGL - OPERAZIONI PER NUOVO SCAGLIONE
170800970117     C*------------------------------------------------------------------------*
170900970117     C     NEWSGL        BEGSR
171000970117     C*
171100970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
171200970117     C                   MOVEL     ETWSGL        DEPSGL
171300970117     C*
171400970117     C* AZZERA LE VARIABILI DI LAVORO
171500970129     C                   Z-ADD     *ZEROS        SGLPPE            7 4
171600011001     C                   Z-ADD     *ZEROS        SGLCOP           15 5
171700011001     C                   Z-ADD     *ZEROS        SGLRIP           15 5
171800011001     C                   Z-ADD     *ZEROS        SGLCOT           15 5
171900011001     C                   Z-ADD     *ZEROS        SGLRIT           15 5
172000970117     C*
172100970117     C* MEMORIZZA LO SCAGLIONE IN SCHIERA
172200970117     C                   SETOFF                                           98
172300970117     C                   Z-ADD     1             J
172400970117     C     ETWSGL        LOOKUP    TSGL(J)                                98
172500970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
172600970117     C                   ADD       1             SS                4 0
172700970117     C                   Z-ADD     SS            S                 4 0
172800970117     C                   Z-ADD     ETWSGL        TSGL(S)                        *SCAGLIONE
172900970117X   1C                   ELSE
173000970117     C                   Z-ADD     J             S
173100970117E   1C                   ENDIF
173200970117     C*
173300970117     C                   ENDSR
173400970117     C*------------------------------------------------------------------------*
173500970117     C* NEWRDL - OPERAZIONI PER NUOVA VOCE DI RAGGRUPPAMENTO DELTA
173600970117     C*------------------------------------------------------------------------*
173700970117     C     NEWRDL        BEGSR
173800970117     C*
173900970117     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
174000970117     C                   MOVEL     ETWRDL        DEPRDL
174100970117     C*
174200970117     C* MEMORIZZA IL RAGGRUPPAMENTO DELTA IN SCHIERA
174300970117     C                   SETOFF                                           98
174400970117     C                   Z-ADD     1             J
174500970117     C     ETWRDL        LOOKUP    TRDL(J)                                98
174600970117IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
174700970117     C                   ADD       1             RR                4 0
174800970117     C                   Z-ADD     RR            R                 4 0
174900970117     C                   MOVEL     ETWRDL        TRDL(R)                        *RAGGRUPPAMENTO
175000970117X   1C                   ELSE
175100970117     C                   Z-ADD     J             R
175200970117E   1C                   ENDIF
175300970117     C*
175400970117     C                   ENDSR
175500970117     C*------------------------------------------------------------------------*
175600970117     C* MS1OUT - MEMORIZZA LE SCHIERE DEL PRIMO SFILE DA EMETTERE
175700970117     C*------------------------------------------------------------------------*
175800970117     C     MS1OUT        BEGSR
175900970117     C*
176000970117     C* ORDINA I CODICI TASSAZIONE
176100970117     C                   EXSR      ORDCTS
176200970117     C*
176300970117     C* CALCOLA LE % DI PESO ASSOLUTE
176400970117     C                   EXSR      CALPPA
176500970117     C*
176600970117     C* AZZERA LE VARIABILI DI LAVORO
176700970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
176800970117     C                   CLEAR                   COPP                           *SCHIERE VIDEO
176900970117     C                   CLEAR                   RIPP
177000970117     C                   CLEAR                   DEPP
177100970117     C                   CLEAR                   COTP
177200970117     C                   CLEAR                   RITP
177300970117     C                   CLEAR                   DETP
177400970724     C                   Z-ADD     *ZEROS        LPER             17 7          *% DI LAVORO
177500970724     C                   Z-ADD     *ZEROS        LCOM             17 7          *IMPORTO DI COMODO
177600970724     C                   Z-ADD     *ZEROS        LIMP             17 7          *IMPORTO DI LAVORO
177700970117     C*---
177800970117     C* CALCOLA GLI IMPORTI -PER CODICE TASSAZIONE-
177900970117     C*---
178000050103DO  1C     1             DO        601           I                 4 0          *CICLO COD.VIDEO
178100970724     C                   Z-ADD     *ZEROS        TOTCOP           17 7          *COMP.PORTO TOT
178200970724     C                   Z-ADD     *ZEROS        TOTRIP           17 7          *RIC. PORTO TOT
178300970724     C                   Z-ADD     *ZEROS        TOTCOT           17 7          *COMP.PORTO+ACC TOT
178400970724     C                   Z-ADD     *ZEROS        TOTRIT           17 7          *RIC. PORTO+ACC TOT
178500050103DO  2C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
178600970117IF  3C     TCTS(J)       IFNE      *BLANKS
178700970117     C* CALCOLARE
178800970117IF  4C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
178900970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
179000970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
179100970117     C* CALCOLARE IN REGIONE
179200970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
179300970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
179400970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
179500970117     C* CALCOLARE IN NAZIONE
179600970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
179700970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
179800970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
179900970117     C* CALCOLARE TOTALE
180000050103     C     I             OREQ      601                                          *TOTALE
180100970117     C*
180200970117DO  5C     1             DO        10            K                 4 0          *CICLO SCAGLIONI
180300970117     C     K             OCCUR     DSGL                                         S C A G L I O N E
180400970117IF  6C     TPPA(J)       IFGT      *ZEROS
180500970117     C* PERCENTUALE
180600970117     C     TPPA(J)       MULT      100           LCOM
180700970117     C     LCOM          DIV       PPAP(I)       LPER
180800970117     C* COMPETENZE -PORTO-
180900970117     C     TCOP(J)       MULT      LPER          LCOM
181000970117     C     LCOM          DIV       100           LIMP
181100970117     C     TOTCOP        ADD       LIMP          TOTCOP
181200970117     C* RICAVI -PORTO-
181300970117     C     TRIP(J)       MULT      LPER          LCOM
181400970117     C     LCOM          DIV       100           LIMP
181500970117     C     TOTRIP        ADD       LIMP          TOTRIP
181600970117     C* COMPETENZE -PORTO + ACCESSORI-
181700970117     C     TCOT(J)       MULT      LPER          LCOM
181800970117     C     LCOM          DIV       100           LIMP
181900970117     C     TOTCOT        ADD       LIMP          TOTCOT
182000970117     C* RICAVI -PORTO + ACCESSORI-
182100970117     C     TRIT(J)       MULT      LPER          LCOM
182200970117     C     LCOM          DIV       100           LIMP
182300970117     C     TOTRIT        ADD       LIMP          TOTRIT
182400970117E   6C                   ENDIF
182500970117E   5C                   ENDDO                                                  *FINE SCAGLIONI
182600970117E   4C                   ENDIF
182700970117E   3C                   ENDIF
182800970117E   2C                   ENDDO                                                  *FINE COD.TASS.MEMOR
182900970117     C*
183000970725     C* MEMORIZZA I DATI NELL'ELEMENTO DI SCHIERA -CODICE TASSAZIONE-
183100970117IF  2C     TOTCOP        IFGT      *ZEROS                                       *SE MEMORIZZATO QC.
183200970117     C     TOTRIP        ORGT      *ZEROS
183300970117     C     TOTCOT        ORGT      *ZEROS
183400970117     C     TOTRIT        ORGT      *ZEROS
183500970117     C                   Z-ADD     I             V
183600970117     C                   EXSR      SS1OUT
183700970117E   2C                   ENDIF
183800970117E   1C                   ENDDO                                                  *FINE COD.TASS.VIDEO
183900970117     C*
184000970117     C                   ENDSR
184100970117     C*------------------------------------------------------------------------*
184200970117     C* SS1OUT - MEMORIZZA ELEMENTO SCHIERA DEL PRIMO SFILE DA EMETTERE
184300970117     C*------------------------------------------------------------------------*
184400970117     C     SS1OUT        BEGSR
184500970117     C*
184600050103IF  1C     V             IFEQ      601                                          *SE -TOTALE-
184700970117     C                   MOVEL     '**'          CTSP(V)                        *CODICE
184800970117     C                   MOVEL(P)  'TOTALE'      DCTP(V)                        *DESCRIZIONE
184900970117     C                   MOVEL     '*'           TCTP(V)                        *TIPO
185000970117     C                   MOVEL     *BLANKS       CORP(V)                        *ORDINAMENTO
185100970117E   1C                   ENDIF
185200970117     C* IMPORTI
185300970725     C                   Z-ADD(H)  TOTCOP        COPP(V)                        *COMPETENZA PORTO
185400970725     C                   Z-ADD(H)  TOTRIP        RIPP(V)                        *RICAVO     PORTO
185500970725     C                   Z-ADD(H)  TOTCOT        COTP(V)                        *COMPETENZA PORTO+AC
185600970725     C                   Z-ADD(H)  TOTRIT        RITP(V)                        *RICAVO     PORTO+AC
185700970117     C* DELTA
185800011024     C                   Z-ADD     TOTRIP        CONSUN           21 5
185900011024     C                   Z-ADD     TOTCOP        RIFERI           21 5
186000011024     C                   Z-ADD     *ZEROS        DELTA            21 1
186100970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
186200970117     C                   Z-ADD     DELTA         DEPP(V)                        *DELTA PORTO
186300011024     C                   Z-ADD     TOTRIT        CONSUN
186400011024     C                   Z-ADD     TOTCOT        RIFERI
186500011024     C                   Z-ADD     *ZEROS        DELTA
186600970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
186700970117     C                   Z-ADD     DELTA         DETP(V)                        *DELTA PORTO+ACC
186800970117     C*
186900970117     C                   ENDSR
187000970117     C*------------------------------------------------------------------------*
187100970117     C* ORDCTS - ORDINA I CODICI TASSAZIONE DA EMETTERE A VIDEO
187200970117     C*------------------------------------------------------------------------*
187300970117     C     ORDCTS        BEGSR
187400970117     C*
187500050103DO  1C     1             DO        600           I                 4 0
187600970117IF  2C     CTSP(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
187700970117     C     1             ADD       I             J                 4 0
187800050103DO  3C     J             DO        600           J
187900970117IF  4C     CTSP(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
188000970117IF  5C     CORP(I)       IFGT      CORP(J)
188100970117     C                   MOVEL     CTSP(I)       PCTS                           *CODICE
188200970117     C                   MOVEL     CTSP(J)       CTSP(I)
188300970117     C                   MOVEL     PCTS          CTSP(J)
188400970117     C                   MOVEL     DCTP(I)       PDCT                           *DESCRIZIONE
188500970117     C                   MOVEL     DCTP(J)       DCTP(I)
188600970117     C                   MOVEL     PDCT          DCTP(J)
188700970117     C                   MOVEL     TCTP(I)       PTCT                           *TIPO (E/R/N/*)
188800970117     C                   MOVEL     TCTP(J)       TCTP(I)
188900970117     C                   MOVEL     PTCT          TCTP(J)
189000970117     C                   MOVEL     CORP(I)       PCOR                           *ORDINAMENTO
189100970117     C                   MOVEL     CORP(J)       CORP(I)
189200970117     C                   MOVEL     PCOR          CORP(J)
189300970117E   5C                   ENDIF
189400970117E   4C                   ENDIF
189500970117E   3C                   ENDDO                                                  *FINE CICLO UNO
189600970117E   2C                   ENDIF
189700970117E   1C                   ENDDO                                                  *FINE CICLO DUE
189800970117     C*
189900970117     C                   ENDSR
190000970117     C*------------------------------------------------------------------------*
190100970117     C* CALPPA - CALCOLA LE % DI PESO ASSOLUTE
190200970117     C*------------------------------------------------------------------------*
190300970117     C     CALPPA        BEGSR
190400970117     C*
190500050103DO  1C     1             DO        601           I                 4 0          *CICLO COD.VIDEO
190600050103DO  2C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
190700970117IF  3C     TCTS(J)       IFNE      *BLANKS
190800970117     C* CALCOLARE
190900970117IF  4C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
191000970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
191100970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
191200970117     C* CALCOLARE IN REGIONE
191300970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
191400970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
191500970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
191600970117     C* CALCOLARE IN NAZIONE
191700970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
191800970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
191900970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
192000970117     C* CALCOLARE TOTALE
192100050103     C     I             OREQ      601                                          *TOTALE
192200970117     C*
192300970117DO  5C     1             DO        10            K                 4 0          *CICLO SCAGLIONI
192400970117     C     K             OCCUR     DSGL                                         S C A G L I O N E
192500970117     C                   ADD       TPPA(J)       PPAP(I)                        *%PESO ASS.TOTALE
192600970117     C     K             OCCUR     DLAV                                         L A V O R O
192700970117     C                   ADD       TPPA(J)       PPAS(I)                        *%PESO ASS. X SGL
192800970117E   5C                   ENDDO                                                  *FINE SCAGLIONI
192900970117     C*
193000970117E   4C                   ENDIF
193100970117E   3C                   ENDIF
193200970117E   2C                   ENDDO                                                  *FINE COD.TASS.MEMOR
193300970117E   1C                   ENDDO                                                  *FINE COD.TASS.VIDEO
193400970117     C*
193500970117     C                   ENDSR
193600970117     C*------------------------------------------------------------------------*
193700970117     C* MS2OUT - MEMORIZZA LE SCHIERE DEL SECONDO SFILE DA EMETTERE
193800970117     C*------------------------------------------------------------------------*
193900970117     C     MS2OUT        BEGSR
194000970117     C*
194100970117     C* AZZERA LE VARIABILI DI LAVORO
194200970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
194300970117     C                   CLEAR                   SGLS                           *SCHIERE VIDEO
194400970117     C                   CLEAR                   COPS
194500970117     C                   CLEAR                   RIPS
194600970117     C                   CLEAR                   DEPS
194700970117     C                   CLEAR                   COTS
194800970117     C                   CLEAR                   RITS
194900970117     C                   CLEAR                   DETS
195000970724     C                   Z-ADD     *ZEROS        LPER                           *% DI LAVORO
195100970724     C                   Z-ADD     *ZEROS        LCOM                           *IMPORTO DI COMODO
195200970724     C                   Z-ADD     *ZEROS        LIMP                           *IMPORTO DI LAVORO
195300970117     C*
195400970117     C* REPERISCE LA POSIZIONE IN SCHIERE DEL CODICE TASSAZIONE
195500970117     C                   Z-ADD     1             I
195600970117     C     WCTS          LOOKUP    CTSP(I)                                98
195700970117     C*---
195800970117     C* CALCOLA GLI IMPORTI -PER SCAGLIONE-
195900970117     C*---
196000970117DO  1C     1             DO        10            K                 4 0          *CICLO SCAGLIONI
196100970724     C                   Z-ADD     *ZEROS        TOTCOP                         *COMP.PORTO TOT
196200970724     C                   Z-ADD     *ZEROS        TOTRIP                         *RIC. PORTO TOT
196300970724     C                   Z-ADD     *ZEROS        TOTCOT                         *COMP.PORTO+ACC TOT
196400970724     C                   Z-ADD     *ZEROS        TOTRIT                         *RIC. PORTO+ACC TOT
196500970117IF  2C     TSGL(K)       IFGT      *ZEROS
196600050103DO  3C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
196700970117IF  4C     TCTS(J)       IFNE      *BLANKS
196800970117     C* CALCOLARE
196900970117IF  5C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
197000970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
197100970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
197200970117     C* CALCOLARE IN REGIONE
197300970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
197400970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
197500970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
197600970117     C* CALCOLARE IN NAZIONE
197700970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
197800970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
197900970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
198000970117     C* CALCOLARE TOTALE
198100970117     C     TCTP(I)       OREQ      '*'                                          *TOTALE
198200970117     C*
198300970117     C     K             OCCUR     DSGL                                         S C A G L I O N E
198400970117     C     K             OCCUR     DLAV                                         L A V O R O
198500970117     C* PERCENTUALE
198600970117IF  6C     TPPA(J)       IFGT      *ZEROS
198700970117     C     TPPA(J)       MULT      100           LCOM
198800970117     C     LCOM          DIV       PPAS(I)       LPER
198900970117     C* COMPETENZE -PORTO-
199000970117     C     TCOP(J)       MULT      LPER          LCOM
199100970117     C     LCOM          DIV       100           LIMP
199200970117     C     TOTCOP        ADD       LIMP          TOTCOP
199300970117     C* RICAVI -PORTO-
199400970117     C     TRIP(J)       MULT      LPER          LCOM
199500970117     C     LCOM          DIV       100           LIMP
199600970117     C     TOTRIP        ADD       LIMP          TOTRIP
199700970117     C* COMPETENZE -PORTO + ACCESSORI-
199800970117     C     TCOT(J)       MULT      LPER          LCOM
199900970117     C     LCOM          DIV       100           LIMP
200000970117     C     TOTCOT        ADD       LIMP          TOTCOT
200100970117     C* RICAVI -PORTO + ACCESSORI-
200200970117     C     TRIT(J)       MULT      LPER          LCOM
200300970117     C     LCOM          DIV       100           LIMP
200400970117     C     TOTRIT        ADD       LIMP          TOTRIT
200500970117E   6C                   ENDIF
200600970117E   5C                   ENDIF
200700970117E   4C                   ENDIF
200800970117E   3C                   ENDDO                                                  *FINE COD.TASS.MEMOR
200900970117     C*
201000970725     C* MEMORIZZA I DATI NELL'ELEMENTO DI SCHIERA -SCAGLIONE-
201100970117IF  3C     TOTCOP        IFGT      *ZEROS                                       *SE MEMORIZZATO QC.
201200970117     C     TOTRIP        ORGT      *ZEROS
201300970117     C     TOTCOT        ORGT      *ZEROS
201400970117     C     TOTRIT        ORGT      *ZEROS
201500970117     C                   Z-ADD     K             V
201600970117     C                   EXSR      SS2OUT
201700970117E   3C                   ENDIF
201800970117E   2C                   ENDIF
201900970117E   1C                   ENDDO                                                  *FINE SCAGLIONI
202000970117     C*
202100970117     C* MEMORIZZA I DATI DEL -TOTALE-
202200970117     C                   Z-ADD     11            V
202300970117     C                   Z-ADD     *ZEROS        SGLS(V)                        *SCAGLIONE
202400970117     C                   Z-ADD     WCOP          COPS(V)                        *COMPETENZA PORTO
202500970117     C                   Z-ADD     WRIP          RIPS(V)                        *RICAVO     PORTO
202600970117     C                   Z-ADD     WDEP          DEPS(V)                        *DELTA      PORTO
202700970117     C                   Z-ADD     WCOT          COTS(V)                        *COMPETENZA PORTO+AC
202800970130     C                   Z-ADD     WRIT          RITS(V)                        *RICAVO     PORTO+AC
202900970117     C                   Z-ADD     WDET          DETS(V)                        *DELTA      PORTO+AC
203000970117     C*
203100970117     C                   ENDSR
203200970117     C*------------------------------------------------------------------------*
203300970117     C* SS2OUT - MEMORIZZA ELEMENTO SCHIERA DEL SECONDO SFILE DA EMETTERE
203400970117     C*------------------------------------------------------------------------*
203500970117     C     SS2OUT        BEGSR
203600970117     C* IMPORTI
203700970117     C                   Z-ADD     TSGL(K)       SGLS(V)                        *SCAGLIONE
203800970725     C                   Z-ADD(H)  TOTCOP        COPS(V)                        *COMPETENZA PORTO
203900970725     C                   Z-ADD(H)  TOTRIP        RIPS(V)                        *RICAVO     PORTO
204000970725     C                   Z-ADD(H)  TOTCOT        COTS(V)                        *COMPETENZA PORTO+AC
204100970725     C                   Z-ADD(H)  TOTRIT        RITS(V)                        *RICAVO     PORTO+AC
204200970117     C* DELTA
204300011024     C                   Z-ADD     TOTRIP        CONSUN
204400011024     C                   Z-ADD     TOTCOP        RIFERI
204500011024     C                   Z-ADD     *ZEROS        DELTA
204600970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
204700970117     C                   Z-ADD     DELTA         DEPS(V)                        *DELTA PORTO
204800011024     C                   Z-ADD     TOTRIT        CONSUN
204900011024     C                   Z-ADD     TOTCOT        RIFERI
205000011024     C                   Z-ADD     *ZEROS        DELTA
205100970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
205200970117     C                   Z-ADD     DELTA         DETS(V)                        *DELTA PORTO+ACC
205300970117     C*
205400970117     C                   ENDSR
205500970117     C*------------------------------------------------------------------------*
205600970117     C* MS3OUT - MEMORIZZA LE SCHIERE DEL TERZO SFILE DA EMETTERE
205700970117     C*------------------------------------------------------------------------*
205800970117     C     MS3OUT        BEGSR
205900970117     C*
206000970117     C* AZZERA LE VARIABILI DI LAVORO
206100970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
206200970117     C                   CLEAR                   RDLT                           *SCHIERE VIDEO
206300970117     C                   CLEAR                   COMT
206400970117     C                   CLEAR                   RICT
206500970117     C                   CLEAR                   DELT
206600970724     C                   Z-ADD     *ZEROS        LPER                           *% DI LAVORO
206700970724     C                   Z-ADD     *ZEROS        LCOM                           *IMPORTO DI COMODO
206800970724     C                   Z-ADD     *ZEROS        LIMP                           *IMPORTO DI LAVORO
206900970117     C*
207000970117     C* REPERISCE LA POSIZIONE IN SCHIERE DEL CODICE TASSAZIONE
207100970117     C                   Z-ADD     1             I
207200970117     C     WCTS          LOOKUP    CTSP(I)                                98
207300970117     C*---
207400970117     C* CALCOLA GLI IMPORTI -PER RAGGRUPPAMENTO DELTA-
207500970117     C*---
207600970117     C*
207700970117DO  1C     1             DO        30            K                 4 0          *CICLO RAGGRUPPAMENT
207800970724     C                   Z-ADD     *ZEROS        TOTCOM           17 7          *COMPETENZA
207900970724     C                   Z-ADD     *ZEROS        TOTRIC           17 7          *RICAVO
208000970117IF  2C     TRDL(K)       IFGT      *ZEROS
208100050103DO  3C     1             DO        600           J                 4 0          *CICLO COD.MEMORIZZ.
208200970117IF  4C     TCTS(J)       IFNE      *BLANKS
208300970117     C* CALCOLARE
208400970117IF  5C     TCTP(I)       IFEQ      'E'                                          *ELEMENTARE
208500970117     C     CTSP(I)       ANDEQ     TCTS(J)                                      *CODICE
208600970117     C     TRCT(J)       ANDEQ     'T'                                          *DA CALCOLARE
208700970117     C* CALCOLARE IN REGIONE
208800970117     C     TCTP(I)       OREQ      'R'                                          *REGIONALE
208900970117     C     CTSP(I)       ANDEQ     TRAP(J)                                      *CODICE REGIONE
209000970117     C     TRCT(J)       ANDEQ     'R'                                          *DA CALC.IN REGIONE
209100970117     C* CALCOLARE IN NAZIONE
209200970117     C     TCTP(I)       OREQ      'N'                                          *NAZIONALE
209300970117     C     CTSP(I)       ANDEQ     TNAR(J)                                      *CODICE NAZIONE
209400970117     C     TRCT(J)       ANDEQ     'N'                                          *DA CALC.IN NAZIONE
209500970117     C* CALCOLARE TOTALE
209600970117     C     TCTP(I)       OREQ      '*'                                          *TOTALE
209700970117     C*
209800970117     C     J             OCCUR     DRDL                                         V O C E
209900970117     C*---
210000970117     C* 1° SCAGLIONE
210100970117     C*---
210200970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
210300970117     C     WSGL          ANDEQ     TSGL(1)
210400970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
210500970117IF  7C     TCO1(K)       IFGT      *ZEROS
210600970117     C     TRI1(K)       ORGT      *ZEROS
210700970117     C     1             OCCUR     DSGL                                         S C A G L I O N E
210800970117     C     1             OCCUR     DLAV                                         L A V O R O
210900970117     C* PERCENTUALE
211000970117     C                   Z-ADD     *ZEROS        LPER
211100970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
211200970117IF  9C     PPAS(I)       IFGT      *ZEROS
211300970117     C     TPPA(J)       MULT      100           LCOM
211400970117     C     LCOM          DIV       PPAS(I)       LPER
211500970117E   9C                   ENDIF
211600970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
211700970117IF  9C     PPAP(I)       IFGT      *ZEROS
211800970117     C     TPPA(J)       MULT      100           LCOM
211900970117     C     LCOM          DIV       PPAP(I)       LPER
212000970117E   9C                   ENDIF
212100970117E   8C                   ENDIF
212200970117     C* COMPETENZE
212300970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
212400970117     C     TCO1(K)       MULT      LPER          LCOM
212500970117     C     LCOM          DIV       100           LIMP
212600970117     C     TOTCOM        ADD       LIMP          TOTCOM
212700970117     C* RICAVI
212800970117     C     TRI1(K)       MULT      LPER          LCOM
212900970117     C     LCOM          DIV       100           LIMP
213000970117     C     TOTRIC        ADD       LIMP          TOTRIC
213100970117E   8C                   ENDIF
213200970117E   7C                   ENDIF
213300970117E   6C                   ENDIF
213400970117     C*---
213500970117     C* 2° SCAGLIONE
213600970117     C*---
213700970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
213800970117     C     WSGL          ANDEQ     TSGL(2)
213900970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
214000970117IF  7C     TCO2(K)       IFGT      *ZEROS
214100970117     C     TRI2(K)       ORGT      *ZEROS
214200970117     C     2             OCCUR     DSGL                                         S C A G L I O N E
214300970117     C     2             OCCUR     DLAV                                         L A V O R O
214400970117     C* PERCENTUALE
214500970117     C                   Z-ADD     *ZEROS        LPER
214600970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
214700970117IF  9C     PPAS(I)       IFGT      *ZEROS
214800970117     C     TPPA(J)       MULT      100           LCOM
214900970117     C     LCOM          DIV       PPAS(I)       LPER
215000970117E   9C                   ENDIF
215100970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
215200970117IF  9C     PPAP(I)       IFGT      *ZEROS
215300970117     C     TPPA(J)       MULT      100           LCOM
215400970117     C     LCOM          DIV       PPAP(I)       LPER
215500970117E   9C                   ENDIF
215600970117E   8C                   ENDIF
215700970117     C* COMPETENZE
215800970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
215900970117     C     TCO2(K)       MULT      LPER          LCOM
216000970117     C     LCOM          DIV       100           LIMP
216100970117     C     TOTCOM        ADD       LIMP          TOTCOM
216200970117     C* RICAVI
216300970117     C     TRI2(K)       MULT      LPER          LCOM
216400970117     C     LCOM          DIV       100           LIMP
216500970117     C     TOTRIC        ADD       LIMP          TOTRIC
216600970117E   8C                   ENDIF
216700970117E   7C                   ENDIF
216800970117E   6C                   ENDIF
216900970117     C*---
217000970117     C* 3° SCAGLIONE
217100970117     C*---
217200970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
217300970117     C     WSGL          ANDEQ     TSGL(3)
217400970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
217500970117IF  7C     TCO3(K)       IFGT      *ZEROS
217600970117     C     TRI3(K)       ORGT      *ZEROS
217700970117     C     3             OCCUR     DSGL                                         S C A G L I O N E
217800970117     C     3             OCCUR     DLAV                                         L A V O R O
217900970117     C* PERCENTUALE
218000970117     C                   Z-ADD     *ZEROS        LPER
218100970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
218200970117IF  9C     PPAS(I)       IFGT      *ZEROS
218300970117     C     TPPA(J)       MULT      100           LCOM
218400970117     C     LCOM          DIV       PPAS(I)       LPER
218500970117E   9C                   ENDIF
218600970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
218700970117IF  9C     PPAP(I)       IFGT      *ZEROS
218800970117     C     TPPA(J)       MULT      100           LCOM
218900970117     C     LCOM          DIV       PPAP(I)       LPER
219000970117E   9C                   ENDIF
219100970117E   8C                   ENDIF
219200970117     C* COMPETENZE
219300970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
219400970117     C     TCO3(K)       MULT      LPER          LCOM
219500970117     C     LCOM          DIV       100           LIMP
219600970117     C     TOTCOM        ADD       LIMP          TOTCOM
219700970117     C* RICAVI
219800970117     C     TRI3(K)       MULT      LPER          LCOM
219900970117     C     LCOM          DIV       100           LIMP
220000970117     C     TOTRIC        ADD       LIMP          TOTRIC
220100970117E   8C                   ENDIF
220200970117E   7C                   ENDIF
220300970117E   6C                   ENDIF
220400970117     C*---
220500970117     C* 4° SCAGLIONE
220600970117     C*---
220700970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
220800970117     C     WSGL          ANDEQ     TSGL(4)
220900970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
221000970117IF  7C     TCO4(K)       IFGT      *ZEROS
221100970117     C     TRI4(K)       ORGT      *ZEROS
221200970117     C     4             OCCUR     DSGL                                         S C A G L I O N E
221300970117     C     4             OCCUR     DLAV                                         L A V O R O
221400970117     C* PERCENTUALE
221500970117     C                   Z-ADD     *ZEROS        LPER
221600970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
221700970117IF  9C     PPAS(I)       IFGT      *ZEROS
221800970117     C     TPPA(J)       MULT      100           LCOM
221900970117     C     LCOM          DIV       PPAS(I)       LPER
222000970117E   9C                   ENDIF
222100970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
222200970117IF  9C     PPAP(I)       IFGT      *ZEROS
222300970117     C     TPPA(J)       MULT      100           LCOM
222400970117     C     LCOM          DIV       PPAP(I)       LPER
222500970117E   9C                   ENDIF
222600970117E   8C                   ENDIF
222700970117     C* COMPETENZE
222800970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
222900970117     C     TCO4(K)       MULT      LPER          LCOM
223000970117     C     LCOM          DIV       100           LIMP
223100970117     C     TOTCOM        ADD       LIMP          TOTCOM
223200970117     C* RICAVI
223300970117     C     TRI4(K)       MULT      LPER          LCOM
223400970117     C     LCOM          DIV       100           LIMP
223500970117     C     TOTRIC        ADD       LIMP          TOTRIC
223600970117E   8C                   ENDIF
223700970117E   7C                   ENDIF
223800970117E   6C                   ENDIF
223900970117     C*---
224000970117     C* 5° SCAGLIONE
224100970117     C*---
224200970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
224300970117     C     WSGL          ANDEQ     TSGL(5)
224400970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
224500970117IF  7C     TCO5(K)       IFGT      *ZEROS
224600970117     C     TRI5(K)       ORGT      *ZEROS
224700970117     C     5             OCCUR     DSGL                                         S C A G L I O N E
224800970117     C     5             OCCUR     DLAV                                         L A V O R O
224900970117     C* PERCENTUALE
225000970117     C                   Z-ADD     *ZEROS        LPER
225100970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
225200970117IF  9C     PPAS(I)       IFGT      *ZEROS
225300970117     C     TPPA(J)       MULT      100           LCOM
225400970117     C     LCOM          DIV       PPAS(I)       LPER
225500970117E   9C                   ENDIF
225600970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
225700970117IF  9C     PPAP(I)       IFGT      *ZEROS
225800970117     C     TPPA(J)       MULT      100           LCOM
225900970117     C     LCOM          DIV       PPAP(I)       LPER
226000970117E   9C                   ENDIF
226100970117E   8C                   ENDIF
226200970117     C* COMPETENZE
226300970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
226400970117     C     TCO5(K)       MULT      LPER          LCOM
226500970117     C     LCOM          DIV       100           LIMP
226600970117     C     TOTCOM        ADD       LIMP          TOTCOM
226700970117     C* RICAVI
226800970117     C     TRI5(K)       MULT      LPER          LCOM
226900970117     C     LCOM          DIV       100           LIMP
227000970117     C     TOTRIC        ADD       LIMP          TOTRIC
227100970117E   8C                   ENDIF
227200970117E   7C                   ENDIF
227300970117E   6C                   ENDIF
227400970117     C*---
227500970117     C* 6° SCAGLIONE
227600970117     C*---
227700970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
227800970117     C     WSGL          ANDEQ     TSGL(6)
227900970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
228000970117IF  7C     TCO6(K)       IFGT      *ZEROS
228100970117     C     TRI6(K)       ORGT      *ZEROS
228200970117     C     6             OCCUR     DSGL                                         S C A G L I O N E
228300970117     C     6             OCCUR     DLAV                                         L A V O R O
228400970117     C* PERCENTUALE
228500970117     C                   Z-ADD     *ZEROS        LPER
228600970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
228700970117IF  9C     PPAS(I)       IFGT      *ZEROS
228800970117     C     TPPA(J)       MULT      100           LCOM
228900970117     C     LCOM          DIV       PPAS(I)       LPER
229000970117E   9C                   ENDIF
229100970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
229200970117IF  9C     PPAP(I)       IFGT      *ZEROS
229300970117     C     TPPA(J)       MULT      100           LCOM
229400970117     C     LCOM          DIV       PPAP(I)       LPER
229500970117E   9C                   ENDIF
229600970117E   8C                   ENDIF
229700970117     C* COMPETENZE
229800970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
229900970117     C     TCO6(K)       MULT      LPER          LCOM
230000970117     C     LCOM          DIV       100           LIMP
230100970117     C     TOTCOM        ADD       LIMP          TOTCOM
230200970117     C* RICAVI
230300970117     C     TRI6(K)       MULT      LPER          LCOM
230400970117     C     LCOM          DIV       100           LIMP
230500970117     C     TOTRIC        ADD       LIMP          TOTRIC
230600970117E   8C                   ENDIF
230700970117E   7C                   ENDIF
230800970117E   6C                   ENDIF
230900970117     C*---
231000970117     C* 7° SCAGLIONE
231100970117     C*---
231200970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
231300970117     C     WSGL          ANDEQ     TSGL(7)
231400970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
231500970117IF  7C     TCO7(K)       IFGT      *ZEROS
231600970117     C     TRI7(K)       ORGT      *ZEROS
231700970117     C     7             OCCUR     DSGL                                         S C A G L I O N E
231800970117     C     7             OCCUR     DLAV                                         L A V O R O
231900970117     C* PERCENTUALE
232000970117     C                   Z-ADD     *ZEROS        LPER
232100970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
232200970117IF  9C     PPAS(I)       IFGT      *ZEROS
232300970117     C     TPPA(J)       MULT      100           LCOM
232400970117     C     LCOM          DIV       PPAS(I)       LPER
232500970117E   9C                   ENDIF
232600970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
232700970117IF  9C     PPAP(I)       IFGT      *ZEROS
232800970117     C     TPPA(J)       MULT      100           LCOM
232900970117     C     LCOM          DIV       PPAP(I)       LPER
233000970117E   9C                   ENDIF
233100970117E   8C                   ENDIF
233200970117     C* COMPETENZE
233300970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
233400970117     C     TCO7(K)       MULT      LPER          LCOM
233500970117     C     LCOM          DIV       100           LIMP
233600970117     C     TOTCOM        ADD       LIMP          TOTCOM
233700970117     C* RICAVI
233800970117     C     TRI7(K)       MULT      LPER          LCOM
233900970117     C     LCOM          DIV       100           LIMP
234000970117     C     TOTRIC        ADD       LIMP          TOTRIC
234100970117E   8C                   ENDIF
234200970117E   7C                   ENDIF
234300970117E   6C                   ENDIF
234400970117     C*---
234500970117     C* 8° SCAGLIONE
234600970117     C*---
234700970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
234800970117     C     WSGL          ANDEQ     TSGL(8)
234900970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
235000970117IF  7C     TCO8(K)       IFGT      *ZEROS
235100970117     C     TRI8(K)       ORGT      *ZEROS
235200970117     C     8             OCCUR     DSGL                                         S C A G L I O N E
235300970117     C     8             OCCUR     DLAV                                         L A V O R O
235400970117     C* PERCENTUALE
235500970117     C                   Z-ADD     *ZEROS        LPER
235600970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
235700970117IF  9C     PPAS(I)       IFGT      *ZEROS
235800970117     C     TPPA(J)       MULT      100           LCOM
235900970117     C     LCOM          DIV       PPAS(I)       LPER
236000970117E   9C                   ENDIF
236100970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
236200970117IF  9C     PPAP(I)       IFGT      *ZEROS
236300970117     C     TPPA(J)       MULT      100           LCOM
236400970117     C     LCOM          DIV       PPAP(I)       LPER
236500970117E   9C                   ENDIF
236600970117E   8C                   ENDIF
236700970117     C* COMPETENZE
236800970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
236900970117     C     TCO8(K)       MULT      LPER          LCOM
237000970117     C     LCOM          DIV       100           LIMP
237100970117     C     TOTCOM        ADD       LIMP          TOTCOM
237200970117     C* RICAVI
237300970117     C     TRI8(K)       MULT      LPER          LCOM
237400970117     C     LCOM          DIV       100           LIMP
237500970117     C     TOTRIC        ADD       LIMP          TOTRIC
237600970117E   8C                   ENDIF
237700970117E   7C                   ENDIF
237800970117E   6C                   ENDIF
237900970117     C*---
238000970117     C* 9° SCAGLIONE
238100970117     C*---
238200970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
238300970117     C     WSGL          ANDEQ     TSGL(9)
238400970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
238500970117IF  7C     TCO9(K)       IFGT      *ZEROS
238600970117     C     TRI9(K)       ORGT      *ZEROS
238700970117     C     9             OCCUR     DSGL                                         S C A G L I O N E
238800970117     C     9             OCCUR     DLAV                                         L A V O R O
238900970117     C* PERCENTUALE
239000970117     C                   Z-ADD     *ZEROS        LPER
239100970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
239200970117IF  9C     PPAS(I)       IFGT      *ZEROS
239300970117     C     TPPA(J)       MULT      100           LCOM
239400970117     C     LCOM          DIV       PPAS(I)       LPER
239500970117E   9C                   ENDIF
239600970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
239700970117IF  9C     PPAP(I)       IFGT      *ZEROS
239800970117     C     TPPA(J)       MULT      100           LCOM
239900970117     C     LCOM          DIV       PPAP(I)       LPER
240000970117E   9C                   ENDIF
240100970117E   8C                   ENDIF
240200970117     C* COMPETENZE
240300970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
240400970117     C     TCO9(K)       MULT      LPER          LCOM
240500970117     C     LCOM          DIV       100           LIMP
240600970117     C     TOTCOM        ADD       LIMP          TOTCOM
240700970117     C* RICAVI
240800970117     C     TRI9(K)       MULT      LPER          LCOM
240900970117     C     LCOM          DIV       100           LIMP
241000970117     C     TOTRIC        ADD       LIMP          TOTRIC
241100970117E   8C                   ENDIF
241200970117E   7C                   ENDIF
241300970117E   6C                   ENDIF
241400970117     C*---
241500970117     C* 10° SCAGLIONE
241600970117     C*---
241700970117IF  6C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
241800970117     C     WSGL          ANDEQ     TSGL(10)
241900970117     C     WSGL          OREQ      *ZEROS                                       *SCAGLIONE -TOTALE-
242000970117IF  7C     TCO0(K)       IFGT      *ZEROS
242100970117     C     TRI0(K)       ORGT      *ZEROS
242200970117     C     10            OCCUR     DSGL                                         S C A G L I O N E
242300970117     C     10            OCCUR     DLAV                                         L A V O R O
242400970117     C* PERCENTUALE
242500970117     C                   Z-ADD     *ZEROS        LPER
242600970117IF  8C     WSGL          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
242700970117IF  9C     PPAS(I)       IFGT      *ZEROS
242800970117     C     TPPA(J)       MULT      100           LCOM
242900970117     C     LCOM          DIV       PPAS(I)       LPER
243000970117E   9C                   ENDIF
243100970117X   8C                   ELSE                                                   *SCAGLIONE -TOTALE-
243200970117IF  9C     PPAP(I)       IFGT      *ZEROS
243300970117     C     TPPA(J)       MULT      100           LCOM
243400970117     C     LCOM          DIV       PPAP(I)       LPER
243500970117E   9C                   ENDIF
243600970117E   8C                   ENDIF
243700970117     C* COMPETENZE
243800970117IF  8C     LPER          IFGT      *ZEROS                                       *SCAGLIONE-SPECIFICO
243900970117     C     TCO0(K)       MULT      LPER          LCOM
244000970117     C     LCOM          DIV       100           LIMP
244100970117     C     TOTCOM        ADD       LIMP          TOTCOM
244200970117     C* RICAVI
244300970117     C     TRI0(K)       MULT      LPER          LCOM
244400970117     C     LCOM          DIV       100           LIMP
244500970117     C     TOTRIC        ADD       LIMP          TOTRIC
244600970117E   8C                   ENDIF
244700970117E   7C                   ENDIF
244800970117E   6C                   ENDIF
244900970117     C*
245000970117E   5C                   ENDIF
245100970117E   4C                   ENDIF
245200970117E   3C                   ENDDO                                                  *FINE COD.TASS.MEMOR
245300970117     C*
245400970725     C* MEMORIZZA I DATI NELL'ELEMENTO DI SCHIERA -RAGGRUPPAMENTO DELTA-
245500970117IF  3C     TOTCOM        IFGT      *ZEROS                                       *SE MEMORIZZATO QC.
245600970117     C     TOTRIC        ORGT      *ZEROS
245700970117     C                   Z-ADD     K             V
245800970117     C                   EXSR      SS3OUT
245900970117E   3C                   ENDIF
246000970117E   2C                   ENDIF
246100970117E   1C                   ENDDO                                                  *FINE SCAGLIONI
246200970117     C*
246300970117     C* MEMORIZZA I DATI DEL -TOTALE-
246400970117     C                   Z-ADD     31            V
246500970117     C                   Z-ADD     *ZEROS        RDLT(V)                        *RAGGRUPPAMENTO
246600970130     C                   Z-ADD     WCOM          COMT(V)                        *COMPETENZA PORTO+AC
246700970130     C                   Z-ADD     WRIC          RICT(V)                        *RICAVO     PORTO+AC
246800970130     C                   Z-ADD     WDEL          DELT(V)                        *DELTA      PORTO+AC
246900970117     C*
247000970117     C                   ENDSR
247100970117     C*------------------------------------------------------------------------*
247200970117     C* SS3OUT - MEMORIZZA ELEMENTO SCHIERA DEL TERZO SFILE DA EMETTERE
247300970117     C*------------------------------------------------------------------------*
247400970117     C     SS3OUT        BEGSR
247500970117     C* IMPORTI
247600970117     C                   Z-ADD     TRDL(K)       RDLT(V)                        *RAGGRUPPAMENTO
247700970725     C                   Z-ADD(H)  TOTCOM        COMT(V)                        *COMPETENZA
247800970725     C                   Z-ADD(H)  TOTRIC        RICT(V)                        *RICAVO
247900970117     C* DELTA
248000011024     C                   Z-ADD     TOTRIC        CONSUN
248100011024     C                   Z-ADD     TOTCOM        RIFERI
248200011024     C                   Z-ADD     *ZEROS        DELTA
248300970117     C                   EXSR      CALDEL                                       *CALCOLA DELTA
248400970117     C                   Z-ADD     DELTA         DELT(V)                        *DELTA
248500970117     C*
248600970117     C                   ENDSR
248700970117     C*------------------------------------------------------------------------*
248800970117     C* CALDEL - CALCOLA IL DELTA
248900970117     C*------------------------------------------------------------------------*
249000970117     C     CALDEL        BEGSR
249100970117     C*---
249200970117     C* CONSUN -> VALORE A CONSUNTIVO
249300970117     C* RIFERI -> VALORE DI RIFERIMENTO
249400970117     C*
249500970117     C* (CONSUNTIVO - RIFERIMENTO) = GAP
249600970117     C* [(GAP x 100) : CONSUNTIVO] = DELTA
249700970117     C*---
249800011024     C*
249900011024     C* COME PRIMISSIMA COSA VERIFICO SE UNO DEI DUE FATTORI E' A 0
250000011024     C* ... SE SI' FORZO A +/- 999,9 IL DELTA.
250100011024     C*
250200011024     C                   IF        CONSUN = 0
250300011024     C                   Z-ADD     -999,9        DELTA
250400011024     C                   ENDIF
250500011024     C                   IF        RIFERI = 0
250600011024     C                   Z-ADD     +999,9        DELTA
250700011024     C                   ENDIF
250800011024     c
250900011024IF  1C                   IF        DELTA = 0
251000011024     C     CONSUN        SUB       RIFERI        GAP              21 5
251100970117     C     GAP           MULT      100           GAP
251200970117     C*
251300970117     C     GAP           DIV       CONSUN        GAP
251400970117     C                   Z-ADD(H)  GAP           DELTA
251500970117E   1C                   ENDIF
251600970117     C*
251700970117     C                   ENDSR
251800970318     C*------------------------------------------------------------------------*
251900970318     C* CARETC - LEGGE I DATI TIPO -GENERALI-
252000970318     C*------------------------------------------------------------------------*
252100040811     C*    CARETC        BEGSR
252200970318     C*
252300040811     C*                  CLEAR                   TEETC000
252400040811     C*                  MOVEL     D00DSF        KTCDSF                         *DATI SEDE O FILIALE
252500040811     C*                  MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
252600040811     C*                  Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
252700040811     C*                  Z-ADD     D00CTR        KTCCTR                         *TARIFFA
252800040811     C*                  Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
252900040811     C*                  Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
253000040811     C*    KEYETC        CHAIN     TEETC01L                           99
253100970318     C*
253200040811     C*                  ENDSR
253300970117     C*------------------------------------------------------------------------*
253400970117     C* *INZSR - OPERAZIONI INIZIALI
253500970117     C*------------------------------------------------------------------------*
253600970117     C     *INZSR        BEGSR
253700970117     C*--------------------
253800970117     C* RICEVIMENTO PARAMETRI
253900970117     C*--------------------
254000970117     C     *ENTRY        PLIST
254100970117     C                   PARM                    KPJBA
254200970117     C                   PARM                    DSTE00
254300041202     c* apertura controllata di alcuni file xchè solo di filiale
254400041202     c                   open      tfaco00f                             95
254500041202     c                   open      tnvis05l                             95
254600970117     C*--------------------
254700970117     C* VARIABILI RIFERITE AL DATABASE
254800970117     C*--------------------
254900970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
255000970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
255100970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
255200970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
255300970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
255400970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
255500970512     C     *LIKE         DEFINE    VISNRV        KVINRV                         *TNVIS00F
255600970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
255700040811     C*    *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
255800040811     C*    *LIKE         DEFINE    ETCCTO        KTCCTO
255900040811     C*    *LIKE         DEFINE    ETCKSC        KTCKSC
256000040811     C*    *LIKE         DEFINE    ETCCTR        KTCCTR
256100040811     C*    *LIKE         DEFINE    ETCPRG        KTCPRG
256200040811     C*    *LIKE         DEFINE    ETCLNP        KTCLNP
256300011001     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         *TEETW00F
256400980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
256500980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
256600970117     C     *LIKE         DEFINE    ETWKSC        KTWKSC
256700970117     C     *LIKE         DEFINE    ETWCTR        KTWCTR
256800970117     C     *LIKE         DEFINE    ETWPRG        KTWPRG
256900970117     C     *LIKE         DEFINE    ETWCTS        PCTS                           *DI PARCHEGGIO
257000970117     C     *LIKE         DEFINE    §CTDES        PDCT
257100970117     C     *LIKE         DEFINE    ETWRCT        PTCT
257200970117     C     *LIKE         DEFINE    §CTCOR        PCOR
257300970117     C     *LIKE         DEFINE    ETWCTS        WCTS                           *DI LAVORO
257400970117     C     *LIKE         DEFINE    ETWCOM        WCOP
257500970117     C     *LIKE         DEFINE    ETWRIC        WRIP
257600970117     C     *LIKE         DEFINE    V3SDEL        WDEP
257700970117     C     *LIKE         DEFINE    ETWCOM        WCOT
257800970117     C     *LIKE         DEFINE    ETWRIC        WRIT
257900970117     C     *LIKE         DEFINE    V3SDEL        WDET
258000970117     C     *LIKE         DEFINE    ETWSGL        WSGL
258100970117     C     *LIKE         DEFINE    ETWCOM        WCOM
258200970117     C     *LIKE         DEFINE    ETWRIC        WRIC
258300970117     C     *LIKE         DEFINE    V3SDEL        WDEL
258400970117     C     *LIKE         DEFINE    ETWNAR        DEPNAR                         *DI DEPOSITO
258500970117     C     *LIKE         DEFINE    ETWRAP        DEPRAP
258600970117     C     *LIKE         DEFINE    ETWCTS        DEPCTS
258700970117     C     *LIKE         DEFINE    ETWSGL        DEPSGL
258800970117     C     *LIKE         DEFINE    ETWRDL        DEPRDL
258900970117     C*--------------------
259000970117     C* CHIAVI DI LETTURA
259100970117     C*--------------------
259200970117     C* LETTURA TABEL00F - COMPLETA
259300970117     C     KEYTAB        KLIST
259400970117     C                   KFLD                    KTBKUT                         *UTENTE
259500970117     C                   KFLD                    KTBCOD                         *TABELLA
259600970117     C                   KFLD                    KTBKEY                         *CODICE
259700970117     C* LETTURA TABEL00F - PARZIALE
259800970117     C     KE2TAB        KLIST
259900970117     C                   KFLD                    KTBKUT                         *UTENTE
260000970117     C                   KFLD                    KTBCOD                         *TABELLA
260100970117     C* LETTURA CNACO01L - COMPLETA
260200970117     C     KEYACO        KLIST
260300970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
260400970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
260500970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
260600970512     C* LETTURA TNVIS05L - COMPLETA
260700970512     C     KEYVIS        KLIST
260800970512     C                   KFLD                    KVINRV                         *NUMERO VISITA
260900970117     C* LETTURA AZORG01L - COMPLETA
261000970117     C     KEYORG        KLIST
261100970117     C                   KFLD                    KORFIL                         *FILIALE
261200011001     C* LETTURA TEETC01L - COMPLETA
261300040811     C*    KEYETC        KLIST
261400040811     C*                  KFLD                    KTCDSF                         *DATI SEDE O FILIALE
261500040811     C*                  KFLD                    KTCCTO                         *TARIFFA O OFFERTA
261600040811     C*                  KFLD                    KTCKSC                         *CLIENTE O VISITA
261700040811     C*                  KFLD                    KTCCTR                         *TARIFFA
261800040811     C*                  KFLD                    KTCPRG                         *PROGRESSIVO
261900040811     C*                  KFLD                    KTCLNP                         *LINEA DI PARTENZA
262000011001     C* LETTURA TEETW02L - PARZIALE
262100970117     C     KEYETW        KLIST
262200970213     C                   KFLD                    KTWUSR                         *NOME UTENTE
262300980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
262400980603     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
262500970117     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
262600970117     C                   KFLD                    KTWCTR                         *TARIFFA
262700970117     C                   KFLD                    KTWPRG                         *PROGRESSIVO
262800970117     C*--------------------
262900970117     C* VARIABILI NON RIFERITE AL DATABASE
263000970117     C*--------------------
263100970117     C                   Z-ADD     *ZEROS        NR1               4 0          *SFLRCDNBR 1°
263200970117     C                   Z-ADD     *ZEROS        NR2               4 0          *SFLRCDNBR 2°
263300970117     C                   Z-ADD     *ZEROS        NR3               4 0          *SFLRCDNBR 3°
263400970117     C     *LIKE         DEFINE    NR1           SF1EOF                         *N°RECORD 1°SFILE
263500970117     C     *LIKE         DEFINE    NR2           SF2EOF                         *N°RECORD 2°SFILE
263600970117     C     *LIKE         DEFINE    NR3           SF3EOF                         *N°RECORD 3°SFILE
263700970117     C     *LIKE         DEFINE    V1SOPZ        DU1OPZ                         *DI DUPLICA 1°SFILE
263800970117     C     *LIKE         DEFINE    V1SOPZ        DU2OPZ                         *DI DUPLICA 2°SFILE
263900970117     C     *LIKE         DEFINE    $PC1          $DPC1                          *DEPOSITO NRR 1°SFIL
264000970117     C     *LIKE         DEFINE    $PC2          $DPC2                          *DEPOSITO NRR 1°SFIL
264100970117     C*--------------------
264200970117     C* VARIABILI DI CONTROLLO
264300970117     C*--------------------
264400970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
264500970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
264600970117     C                   MOVEL     '1'           DEPVID            1            *DEP.VIDEO DA GESTIR
264700970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
264800970117     C                   MOVEL     '0'           WERR              1            *ERRORE
264900970117     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
265000970319     C                   MOVEL     '0'           $STMC             1            *STAMPARE COD.TASS.
265100970117     C                   MOVEL     '0'           $STMS             1            *STAMPARE SCAGLIONE
265200970418     C                   MOVEL     '0'           $STMV             1            *STAMPARE VOCE
265300970418     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
265400970418     C                   MOVEL     'N'           $CONF             1            *CONFERMA VIDEATA
265500970117     C*
265600970117     C* REPERISCE LA DATA CORRENTE
265700970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
265800970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
265900970117     C                   Z-ADD     WN8           G08DAT
266000970117     C                   Z-ADD     *ZEROS        G08INV
266100970117     C                   MOVEL     '0'           G08ERR
266200970117     C                   CALL      'XSRDA8'
266300970117     C                   PARM                    WLBDA8
266400970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
266500970117     C*
266600970117     C* CARICA I CAPICONTO DI CONTABILITA'
266700970117     C                   EXSR      CARCC
266800970117     C*
266900970117     C* CARICA LE TABELLE OCCORRENTI
267000970117     C                   EXSR      CARTAB
267100970117     C*
267200970117     C* IMPOSTA LA TESTATA
267300970117     C                   EXSR      IMPVD0
267400970318     C*
267500970318     C* LEGG I DATI TIPO -GENERALI-
267600040811     C*                  EXSR      CARETC
267700970318     C*
267800970117     C* MEMORIZZA LE SCHIERE DATI DEL WORKFILE
267900970117     C                   EXSR      MEMETW
268000970117     C*
268100970117     C                   ENDSR
268200970117     O*------------------------------------------------------------------------*
268300970117** -DVID-
268400970117ITALIA              1
268500970117ESTERO              2
268600980603SEDE                3
268700980603FILIALE             4
2688009701175
268900970117** -MSG-
269000970117TNTE20R- Non esistono dati da visualizzare                                    1
269100970117TNTE20R- 2
269200970117TNTE20R- 3
269300970117TNTE20R- 4
269400970117TNTE20R- 5
269500970117TNTE20R- 6
269600970117TNTE20R- 7
269700970117TNTE20R- 8
269800970117TNTE20R- 9
269900970117TNTE20R- 10
270000970117TNTE20R- Opzione errata                                                       11
270100970418TNTE20R- Immettere S per stampare il dettaglio delta delle voci di tassazione 12
270200970117TNTE20R- 13
270300970117TNTE20R- 14
270400970117TNTE20R- 15
270500970117TNTE20R- 16
270600970117TNTE20R- 17
270700970117TNTE20R- 18
270800970117TNTE20R- 19
270900970117TNTE20R- 20
