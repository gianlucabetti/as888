000100970117     H*------------------------------------------------------------------------*
000200970117     H* TARIFFE DA CONTO ECONOMICO - RICHIESTA CODICI TASSAZIONE               *
000300970117     H*------------------------------------------------------------------------*
000400970117     H DECEDIT('0,') DATEDIT(*DMY.)
000500970117     F*------------------------------------------------------------------------*
000600970117     F* DATA BASE
000700970117     F*------------------------------------------------------------------------*
000800970117     FTNTE03D   CF   E             WORKSTN
000900970117     F                                     SFILE(TE03S01:NR1)
001000970117     F                                     SFILE(TE03S02:NR2)
001100970117     F                                     SFILE(TE03S03:NR3)
001200020729     FTNTBE01L  IF   E           K DISK
001300970117     F*------------
001400970117     F* TABELLE
001500970117     F*------------
001600970117     FTABEL00F  IF   E           K DISK
001700970117     F*------------
001800970117     F* ORGANIGRAMMA
001900970117     F*------------
002000970117     FAZORG01L  IF   E           K DISK
002100970117     F*------------
002200970117     F* PIANO DEI CONTI
002300970117     F*------------
002400970117     FCNACO00F  IF   E           K DISK
002500041202     FTFACO00F  IF   E           K DISK    usropn
002600000211     F                                     RENAME(CNACO000:TFACO000)
002700970117     F*------------
002800970117     F* TARIFFE CLIENTE
002900970117     F*------------
003000970117     FTNTAM01L  IF   E           K DISK
003100041202     FTNOFM01L  IF   E           K DISK    usropn
003200970117     F                                     RENAME(TNTAM000:TNOFM000)
003300970117     F*------------
003400970117     F* DATI TIPO - CODICI TASSAZIONE
003500970117     F*------------
003600011001     FTEETT01L  UF A E           K DISK
003700970117     D*------------------------------------------------------------------------*
003800970117     D* SCHIERE
003900970117     D*------------------------------------------------------------------------*
004000970117     D*-------------------
004100970117     D* DESCRIZIONE VIDEO IN GESTIONE
004200970117     D*-------------------
004300970117     D DVID            S             20    DIM(5) CTDATA PERRCD(1)
004400970117     D*-------------------
004500970117     D* DECODIFICHE VARIE
004600970117     D*-------------------
004700970117     D DDEC            S             25    DIM(10) CTDATA PERRCD(1)
004800970117     D*-------------------
004900970117     D* DESCRIZIONE OPERAZIONE
005000970117     D*-------------------
005100970117     D DOPE            S             15    DIM(5) CTDATA PERRCD(1)
005200970117     D*-------------------
005300970117     D* MESSAGGI VIDEO
005400970117     D*-------------------
005500970117     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
005600970117     D*-------------------
005700970117     D* TIPO SERVIZIO
005800970117     D*-------------------
005900970117     D TSP             S              1    DIM(10)                              *CODICE
006000970117     D DTSP            S             20    DIM(10)                              *DESCRIZIONE
006100970117     D*-------------------
006200970117     D* TIPI TARIFFA
006300970117     D*-------------------
006400970117     D TTR             S              1    DIM(10)                              *CODICE
006500970117     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
006600970117     D*-------------------
006700970117     D* CODICI TASSAZIONE
006800970117     D*-------------------
006900970117     D*---
007000970117     D* BASE
007100970117     D*---
007200050103     D CTS             S              2    DIM(600)                             *CODICE
007300050103     D DCT             S             20    DIM(600)                             *DESCRIZIONE
007400050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
007500050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
007600050103     D COR             S              3    DIM(600)                             *ORDINAMENTO
007700050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
007800050103     D NAZ             S              2    DIM(600)                             *CODICE NAZIONE -> P
007900970117     D*---
008000970117     D* VIDEO
008100970117     D*---
008200970117     D* ELEMENTARI
008300050103     D VCTS            S              2    DIM(600)                             *CODICE
008400050103     D VFCR            S              1    DIM(600)                             *FLAG CAPOLUOGO
008500050103     D VCOR            S              3    DIM(600)                             *ORDINAMENTO
008600050103     D VRAP            S              2    DIM(600)                             *REGIONE
008700050103     D VRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
008800050103     D VPPE            S              4  1 DIM(600)                             *%INCIDENZA
008900970117     D* REGIONALI
009000050103     D RCTS            S              2    DIM(60)                              *CODICE
009100050103     D RCOR            S              3    DIM(60)                              *ORDINAMENTO
009200050103     D RRCT            S              1    DIM(60)                              *RAGGRUPPAMENTO
009300050103     D RPPE            S              4  1 DIM(60)                              *%INCIDENZA
009400050103     D RHRC            S              1    DIM(60)                              *RAGGRUPPAMENTO HIDD
009500050103     D RSCH            S             15    DIM(60)                              *CAMPONE RAGGRUPPAME
009600970117     D*---
009700970117     D* CARICAMENTO ALTRA TARIFFA
009800970117     D*---
009900050103     D CCTS            S              2    DIM(600)                             *CODICE
010000050103     D CRAP            S              2    DIM(600)                             *REGIONE
010100050103     D CRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
010200050103     D CPPE            S              4  1 DIM(600)                             *%INCIDENZA
010300970117     D*---
010400970117     D* RAPPRESENTAZIONE
010500970117     D*---
010600050103     D OCTS            S              2    DIM(600)                             *CODICE
010700050103     D OCOR            S              3    DIM(600)                             *ORDINAMENTO
010800050103     D OPPE            S              4  1 DIM(600)                             *%INCIDENZA
010900970129     D*---
011000970129     D* DI CARTELLO
011100970129     D*---
011200050103     D CTS8            S              2    DIM(600)                             *CODICE
011300050103     D RAP8            S              2    DIM(600)                             *REGIONE
011400050103     D RCT8            S              1    DIM(600)                             *RAGGRUPPAMENTO
011500050103     D PPE8            S              4  1 DIM(600)                             *%INCIDENZA
011600040901     D*---
011700040901     D* tabella nazioni per eex/fed/dpd
011800040901     D*---
011900050103     D nar             S                   DIM(600) inz like(§ctnar)            *CODICE
012000050103     D dfe             S              1    DIM(600) inz                         *REGIONE
012100970124     D*-------------------
012200970124     D* DI LAVORO
012300970124     D*-------------------
012400970124     D SCH             S              1    DIM(15)
012500970117     D*------------------
012600970117     D* TABELLE CAPICONTO
012700970117     D*------------------
012800970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
012900970117     D*------------------------------------------------------------------------*
013000970117     D* INPUT
013100970117     D*------------------------------------------------------------------------*
013200970117     D*-------------------
013300970117     D* DS TIPI SERVIZIO
013400970117     D*-------------------
013500970117     D DS5E          E DS
013600970117     D*-------------------
013700970117     D* DS TIPI TARIFFA
013800970117     D*-------------------
013900970117     D DSTR          E DS
014000021202     D*-------------------
014100021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
014200021202     D*-------------------
014300021202     D TRUL27DS      E DS
014400970117     D*-------------------
014500040901     D* DS nazioni
014600970117     D*-------------------
014700040901     D DS15          E DS
014800040901     D*-------------------
014900040901     D* DS CODICI TASSAZIONE
015000040901     D*-------------------
015100040901     D DSCT          E DS
015200970117     D*-------------------
015300970117     D* DS PASSAGGIO PARAMETRI
015400970117     D*-------------------
015500970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
015600980508     D*------------------
015700980508     D* DS RICERCA DATI TIPO
015800980508     D*------------------
015900050103     D DSTE30        E DS                  EXTNAME(TNTE30DS)
016000011003     D*-------------------
016100011003     D* DS REPERIMENTO DIVISA TARIFFE/OFFERTE
016200011003     D*-------------------
016300011003     D DSTA01        E DS
016400970117     D*------------------
016500970415     D* DS SCOMPOSIZIONE DESCRIZIONE VIDEO
016600970117     D*------------------
016700970117     D* DDEC,5
016800970117     D                 DS
016900970117     D  D05RCT                11     25
017000970117     D  DDEC05                 1     25
017100970415     D*------------------
017200970415     D* DS SCOMPOSIZIONE MESSAGGI
017300970415     D*------------------
017400970415     D* MSG,23
017500970415     D                 DS
017600970415     D  MSGDFI                51     53
017700970415     D  MSGDFD                55     55
017800970415     D  MSGM23                 1     78
017900970117     D*------------------
018000970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
018100970117     D*------------------
018200970117     D WLBDA8          DS
018300970117     D  G08DAT                 1      8  0
018400970117     D  G08INV                 9     16  0
018500970117     D  G08ERR                17     17
018600970117     D  G08TGI                18     22  0
018700970117     D*------------------
018800020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
018900970117     D*------------------
019000970117     D CARCNT          DS
019100970117     D  FL12CN                 1      2
019200970117     D  FL34CN                 3      4
019300970117     D  TIPCNT                 5      6
019400020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
019500970117     D  FLU                   66     97
019600970117     D                                     DIM(32)                              FLAG PAR.UTENT
019700970117     D  TCU                  398    697
019800970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
019900970117     D  KCU                  698    847P 0
020000970117     D                                     DIM(50)                              *TAB CAPICONTO
020100970117     D                                     PACKEVEN
020200970117     D TCUDS           DS
020300970117     D  E1                     1      1
020400970117     D  E3                     3      3
020500970117     D  E2                     2      2
020600970117     D  E4                     4      4
020700970117     D  E56                    5      6
020800970117     D*-------------------
020900970117     D* ARCHITETTURA
021000970117     D*-------------------
021100970117     D KPJBA         E DS
021200020729     D*-------------------
021300020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
021400020729     D*-------------------
021500020729     D OG143         E DS
021600020729     D*------------------
021700020729     D* DS PER LETTURA TABELLA "NTW"
021800020729     D*------------------
021900020729     D DNTW          E DS
022000970117     C*------------------------------------------------------------------------*
022100970117     C* MAIN LINE
022200970117     C*------------------------------------------------------------------------*
022300970117     C*
022400970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
022500970117IF  1C     $FINE         IFNE      'S'
022600970117     C*
022700970117     C* IMPOSTAZIONI PRIMO SUBFILE
022800970117     C                   EXSR      IMPSF1
022900970117     C*
023000970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
023100970117DO  2C     $FINE         DOUEQ     'S'
023200970117     C     TIPVID        CASEQ     '1'           GESSF1                         *1° VIDEO
023300970129     C     TIPVID        CASEQ     '2'           GESSF2                         *2° VIDEO
023400970129     C     TIPVID        CASEQ     '3'           GESSF3                         *3° VIDEO
023500970117     C                   ENDCS
023600970117E   2C                   ENDDO
023700970117E   1C                   ENDIF
023800970117     C*
023900970117     C                   SETON                                        LR
024000970117     C*------------------------------------------------------------------------*
024100970117     C* IMPSF1 - IMPOSTAZIONI INIZIALI PRIMO SUBFILE
024200970117     C*------------------------------------------------------------------------*
024300970117     C     IMPSF1        BEGSR
024400970117     C*
024500970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
024600970129     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
024700970117     C                   EXSR      SETSF1                                       *SFILE
024800970117     C* CARICA IL SUBFILE
024900970117     C                   EXSR      POSSF1
025000970117     C*
025100970117     C                   ENDSR
025200970117     C*------------------------------------------------------------------------*
025300970117     C* SETSF1 - IMPOSTAZIONI INDICATORI PRIMO SUBFILE
025400970117     C*------------------------------------------------------------------------*
025500970117     C     SETSF1        BEGSR
025600970117     C*
025700970117     C                   SETOFF                                       29         ERRORI CONTROL
025800970117     C                   SETOFF                                       222324     ERRORI SFILE
025900970117     C                   SETOFF                                       90         GENERICO DI ERRORE
026000970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
026100970117     C*
026200970117     C                   ENDSR
026300970117     C*------------------------------------------------------------------------*
026400970117     C* GESSF1 - GESTIONE PRIMO SUBFILE
026500970117     C*------------------------------------------------------------------------*
026600970117     C     GESSF1        BEGSR
026700970117     C*
026800970117     C* EMISSIONE VIDEATA
026900970117IF  1C     $TESVD        IFEQ      '1'
027000970117     C                   WRITE     TE03D00                                      *TESTATA
027100970117     C                   MOVEL     '0'           $TESVD
027200970117E   1C                   ENDIF
027300970117     C                   WRITE     TE03Z01                                      *PIEDI
027400970117     C                   EXFMT     TE03C01                                      *SFILE
027500970117     C                   EXSR      SETSF1
027600970117     C* F03-FINE
027700970117IF  1C     *INKC         IFEQ      *ON
027800970117     C                   EXSR      F03GES
027900970117     C                   GOTO      FINSF1
028000970117E   1C                   ENDIF
028100970117     C* F12-RITORNO
028200970117IF  1C     *INKL         IFEQ      *ON
028300970117     C                   EXSR      F12GES
028400970117     C                   GOTO      FINSF1
028500970117E   1C                   ENDIF
028600970117     C* F05-CARICA DA ALTRA TARIFFA
028700970117IF  1C     *INKE         IFEQ      *ON
028800970117     C                   EXSR      F05GES
028900970117     C                   GOTO      FINSF1
029000970117E   1C                   ENDIF
029100970117     C* F07-RAPPRESENTAZIONE
029200970117IF  1C     *INKG         IFEQ      *ON
029300970117     C                   EXSR      F07GES
029400970117     C                   GOTO      FINSF1
029500970117E   1C                   ENDIF
029600970117     C* CONTROLLA VIDEO
029700970117     C                   EXSR      CTRSF1
029800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
029900970117     C                   GOTO      FINSF1
030000970117E   1C                   ENDIF
030100970117     C* INVIO-AGGIORNA
030200970124     C                   EXSR      ENTSF1
030300970117     C* F06-AGGIORNA E PROSEGUE
030400970117IF  1C     *INKF         IFEQ      *ON
030500970117     C                   EXSR      F06GES
030600970117E   1C                   ENDIF
030700970117     C*
030800970117     C     FINSF1        ENDSR
030900970117     C*------------------------------------------------------------------------*
031000970117     C* CTRSF1 - CONTROLLA PRIMO SUBFILE
031100970117     C*------------------------------------------------------------------------*
031200970117     C     CTRSF1        BEGSR
031300970117     C*
031400970117     C* REIMPOSTA VARIABILI DI LAVORO
031500970117     C                   MOVEL     '0'           WERR              1            *ERRORE
031600970117     C                   Z-ADD     *ZEROS        V1CPPE                         *TOTALE PESI %
031700970117     C*
031800970117     C* LEGGE TUTTI I RECORD DEL SFILE
031900970117     C                   Z-ADD     1             NR1
032000970117     C     NR1           CHAIN     TE03S01                            98
032100970117DO  1C     *IN98         DOWEQ     *OFF
032200970117     C                   SETOFF                                       70        *NO SFLNXTCHG
032300970117     C*--------------------
032400970117     C* RAGGRUPPAMENTO
032500970117     C*--------------------
032600970117IF  2C     WERR          IFEQ      '0'
032700970117     C                   MOVEL     V1SCTS        WCTS
032800970117     C                   MOVEL     V1SRCT        WRCT
032900970117     C                   MOVEL     *BLANKS       WDRCT
033000970117     C                   EXSR      CHKRCT
033100970117     C                   MOVEL     WRCT          V1SRCT
033200970117     C                   MOVEL     WDRCT         V1SDRC
033300970117IF  3C     WERR          IFEQ      '1'
033400970117     C                   SETON                                        2290
033500970117     C                   MOVEL     MSG(2)        $MSG
033600970117E   3C                   ENDIF
033700970117E   2C                   ENDIF
033800970117     C*--------------------
033900970117     C* %INCIDENZA
034000970117     C*--------------------
034100970117IF  2C     WERR          IFEQ      '0'
034200970117IF  3C     V1SRCT        IFNE      *BLANKS                                      *DA CALCOLARE
034300970117IF  4C     V1SPPE        IFEQ      *ZEROS                                       *%VUOTA
034400970117     C                   SETON                                        2390
034500970117     C                   MOVEL     MSG(3)        $MSG
034600970117     C                   MOVEL     '1'           WERR
034700970117E   4C                   ENDIF
034800970117X   3C                   ELSE                                                   *DA NON CALCOLARE
034900970117IF  4C     V1SPPE        IFGT      *ZEROS                                       *%PIENA
035000970117     C                   SETON                                        2390
035100970117     C                   MOVEL     MSG(4)        $MSG
035200970117     C                   MOVEL     '1'           WERR
035300970117E   4C                   ENDIF
035400970117E   3C                   ENDIF
035500970117E   2C                   ENDIF
035600970117     C*--------------------
035700970117     C* OPZIONE
035800970117     C*--------------------
035900970117IF  2C     WERR          IFEQ      '0'
036000970117IF  3C     V1SOPZ        IFEQ      *BLANKS
036100970117     C     V1SOPZ        OREQ      '1 '
036200970117X   3C                   ELSE
036300970117     C                   MOVEL     '1'           WERR
036400970117     C                   SETON                                        2490
036500970117     C                   MOVEL     MSG(11)       $MSG
036600970117E   3C                   ENDIF
036700970117E   2C                   ENDIF
036800970117     C* NO ERRORE
036900970117IF  2C     WERR          IFEQ      '0'
037000970117     C                   EXSR      AGGVCT                                       AGGIORNA COD.TAS.REG
037100970117     C                   EXSR      CTRVCT                                       CONTROLLA COD.TAS.RE
037200970117     C                   EXSR      GESOP1                                       GESTISCE OPZIONI
037300970117E   2C                   ENDIF
037400970121     C*
037500970317     C     V1CPPE        ADD       V1SPPE        V1CPPE                         *TOTALE PESI %
037600970129     C     NR1           CHAIN     TE03S01                            98        *RILEGGE COD.TASS.
037700970129     C                   EXSR      AGGSF1                                       *AGGIORNA SFILE
037800970117     C*
037900970117IF  2C     WERR          IFEQ      '1'                                          E R R O R E
038000970117     C                   Z-ADD     NR1           $NR1                           *POSIZIONAMENTO
038100970117     C                   GOTO      FINCT1                                       *ESCE
038200970117X   2C                   ELSE                                                   N O   E R R O R E
038300970117     C                   ADD       1             NR1                            *LEGGE ALTRO RECORD
038400970117     C     NR1           CHAIN     TE03S01                            98
038500970117E   2C                   ENDIF
038600970117E   1C                   ENDDO                                                  *FINE SUBFILE
038700970117     C*
038800970117     C     FINCT1        ENDSR
038900970117     C*------------------------------------------------------------------------*
039000970117     C* AGGVCT - AGGIORNA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
039100970117     C*------------------------------------------------------------------------*
039200970117     C     AGGVCT        BEGSR
039300970415     C*
039400970415     C* REIMPOSTA LE VARIABILI DI LAVORO
039500970415     C                   MOVEL     '0'           WERR
039600970415     C                   Z-ADD     *ZEROS        TOTPPM            4 1          *% MINIMA IPOTETICA
039700970203     C*
039800970117     C* AGGIORNA IL RAGGRUPPAMENTO DEI CODICI TASSAZIONE DELLA REGIONE
039900970117IF  1C     V1SRCT        IFNE      'M'                                          *NO SITUAZIONE MISTA
040000970117DO  2C     1             DO        WUVCTS        I
040100970117IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
040200970117IF  4C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
040300970117     C                   MOVEL     V1SRCT        VRCT(I)                        *RAGGRUPPAMENTO
040400970117IF  5C     V1SRCT        IFEQ      *BLANKS                                      *NON CALCOLARE
040500970117     C                   Z-ADD     *ZEROS        VPPE(I)                        *%INCIDENZA=0
040600970415X   5C                   ELSE
040700970415     C                   ADD       0,1           TOTPPM                         *%INCIDENZA IPOTETIC
040800970117E   5C                   ENDIF
040900970117E   4C                   ENDIF
041000970117E   3C                   ENDIF
041100970117E   2C                   ENDDO
041200970128E   1C                   ENDIF
041300970415     C*---
041400970415     C* CONTROLLA LA %INCIDENZA MINIMA DA RICHIERE
041500970415     C*---
041600970415     C* TOTALE %REGIONE >= SOMMA % MINIMA (0,1) DEI COD.TASS.ELEM. DA CALCOLARE
041700970415IF  1C     V1SPPE        IFLT      TOTPPM
041800970415     C                   MOVEL     '1'           WERR
041900970415     C                   MOVEL     MSG(23)       MSGM23
042000970415     C                   MOVEL     TOTPPM        MSGDFI
042100970415     C                   MOVE      TOTPPM        MSGDFD
042200970415     C                   MOVEL     MSGM23        $MSG
042300970415     C                   SETON                                        2390
042400970415E   1C                   ENDIF
042500970415     C*---
042600970415     C* DALLA STESSA TARIFFA
042700970415     C*---
042800970124     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
042900970415IF  1C     WERR          IFEQ      '0'
043000970124     C                   Z-ADD     *ZEROS        TOTPPE            4 1
043100970415IF  2C     V1SRCT        IFNE      ' '                                          *NON CALCOLARE
043200050103DO  3C     1             DO        600           I
043300970415IF  4C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
043400970415IF  5C     V1SCTS        IFEQ      VRAP(I)                                      *STESSA REGIONE
043500970124     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
043600970415E   5C                   ENDIF
043700970415E   4C                   ENDIF
043800970415E   3C                   ENDDO
043900970124     C*
044000970415     C* CALCOLA LE % DI INCIDENZA DEI CODICI TASSAZIONE DELLA REGIONE
044100970415IF  3C     TOTPPE        IFGT      *ZEROS
044200970415DO  4C     1             DO        WUVCTS        I
044300970415IF  5C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
044400970415IF  6C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
044500970415IF  7C     VPPE(I)       IFGT      *ZEROS
044600970129     C     VPPE(I)       MULT      V1SPPE        WN156            15 6
044700970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
044800970415IF  8C     VPPE(I)       IFEQ      *ZEROS
044900970319     C                   Z-ADD     0,1           VPPE(I)
045000970415E   8C                   ENDIF
045100970415E   7C                   ENDIF
045200970415E   6C                   ENDIF
045300970415E   5C                   ENDIF
045400970415E   4C                   ENDDO
045500970415X   3C                   ELSE
045600970124     C*---
045700970203     C* DA ALTRA TARIFFA -SE VUOTA-
045800970124     C*---
045900970117     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
046000970124     C                   Z-ADD     *ZEROS        TOTPPE            4 1
046100050103DO  4C     1             DO        600           I
046200970415IF  5C     CCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
046300970415IF  6C     V1SCTS        IFEQ      CRAP(I)                                      *STESSA REGIONE
046400970117     C                   ADD       CPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
046500970415E   6C                   ENDIF
046600970415E   5C                   ENDIF
046700970415E   4C                   ENDDO
046800970129     C*
046900970117     C* CALCOLA LE % DI INCIDENZA DI CODICI TASSAZIONE DELLA REGIONE
047000970415IF  4C     TOTPPE        IFGT      *ZEROS
047100970415DO  5C     1             DO        WUVCTS        I
047200970415IF  6C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
047300970415IF  7C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
047400970117     C                   SETOFF                                           98
047500970117     C                   Z-ADD     1             J
047600970117     C     VCTS(I)       LOOKUP    CCTS(J)                                98
047700970415IF  8C     *IN98         IFEQ      *ON
047800970129     C     CPPE(J)       MULT      V1SPPE        WN156
047900970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
048000970415IF  9C     VPPE(I)       IFEQ      *ZEROS
048100970319     C                   Z-ADD     0,1           VPPE(I)
048200970415E   9C                   ENDIF
048300970415E   8C                   ENDIF
048400970415E   7C                   ENDIF
048500970415E   6C                   ENDIF
048600970415E   5C                   ENDDO
048700970415X   4C                   ELSE
048800970129     C*---
048900970415     C* DA CARTELLO -SE VUOTA LA TARIFFA-
049000970129     C*---
049100970129     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
049200970129     C                   Z-ADD     *ZEROS        TOTPPE            4 1
049300050103DO  5C     1             DO        600           I
049400970415IF  6C     CTS8(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
049500970415IF  7C     V1SCTS        IFEQ      RAP8(I)                                      *STESSA REGIONE
049600970415     C                   ADD       PPE8(I)       TOTPPE                         *%INCIDENZA REGIONAL
049700970415E   7C                   ENDIF
049800970415E   6C                   ENDIF
049900970415E   5C                   ENDDO
050000970129     C*
050100970129     C* CALCOLA LE % DI INCIDENZA DI CODICI TASSAZIONE DELLA REGIONE
050200970415IF  5C     TOTPPE        IFGT      *ZEROS
050300970415DO  6C     1             DO        WUVCTS        I
050400970415IF  7C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
050500970415IF  8C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
050600970129     C                   SETOFF                                           98
050700970129     C                   Z-ADD     1             J
050800970129     C     VCTS(I)       LOOKUP    CTS8(J)                                98
050900970415IF  9C     *IN98         IFEQ      *ON
051000970129     C     PPE8(J)       MULT      V1SPPE        WN156
051100970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
051200970415IF 10C     VPPE(I)       IFEQ      *ZEROS
051300970319     C                   Z-ADD     0,1           VPPE(I)
051400970415E  10C                   ENDIF
051500970415E   9C                   ENDIF
051600970415E   8C                   ENDIF
051700970415E   7C                   ENDIF
051800970415E   6C                   ENDDO
051900970415     C*---
052000970415     C* IMPOSTA 0,1 -SE VUOTA LA CARTELLO-
052100970415     C*---
052200970415X   5C                   ELSE
052300970415DO  6C     1             DO        WUVCTS        I
052400970415IF  7C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
052500970415IF  8C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
052600970415IF  9C     VRCT(I)       IFNE      *BLANKS                                      *DA CALCOLARE
052700970415     C                   Z-ADD     0,1           VPPE(I)                        *%INCIDENZA MINIMA
052800970415E   9C                   ENDIF
052900970415E   8C                   ENDIF
053000970415E   7C                   ENDIF
053100970415E   6C                   ENDDO
053200970415E   5C                   ENDIF
053300970129     C*
053400970415E   4C                   ENDIF
053500970415E   3C                   ENDIF
053600970128     C*---
053700970128     C* PAREGGIA LA %REGIONE CON LA %TOTALE DI TUTTI I CODICI TASSAZIONE
053800970319     C*---
053900970128     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
054000970409     C                   Z-ADD     *ZEROS        TOTPPE            4 1
054100050103DO  3C     1             DO        600           I
054200970415IF  4C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
054300970415IF  5C     V1SCTS        IFEQ      VRAP(I)                                      *STESSA REGIONE
054400970409     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
054500970415E   5C                   ENDIF
054600970415E   4C                   ENDIF
054700970415E   3C                   ENDDO
054800970414     C*
054900970319     C* CALCOLA LA DIFFERENZA FRA LA %REGIONALE E LA % DEI COD.TASSAZIONE (+/-)
055000970409     C                   Z-ADD     *ZEROS        WN41              4 1
055100970409     C     V1SPPE        SUB       TOTPPE        WN41
055200970415     C*
055300970415     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
055400970415IF  3C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
055500040902     c     wuvcts        andne     *zeros
055600970415DO  4C     WN41          DOWNE     *ZEROS
055700970415DO  5C     1             DO        WUVCTS        I
055800970415IF  6C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
055900970415IF  7C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
056000970415IF  8C     VRCT(I)       IFNE      *BLANKS                                      *DA CALCOLARE
056100970415     C* POSITIVA -> AGGIUNGERE
056200970415IF  9C     WN41          IFGT      *ZEROS
056300970415     C     VPPE(I)       ADD       0,1           VPPE(I)                        *AGGIUNGE %
056400970415     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
056500970415E   9C                   ENDIF
056600970415     C* NEGATIVA -> TOGLIERE
056700970415IF  9C     WN41          IFLT      *ZEROS
056800970415IF 10C     VPPE(I)       IFGT      0,1
056900970415     C     VPPE(I)       SUB       0,1           VPPE(I)                        *SOTTRAE %
057000970415     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
057100970415E  10C                   ENDIF
057200970415E   9C                   ENDIF
057300970415     C* UGUALE A ZERO -> USCIRE
057400970415IF  9C     WN41          IFEQ      *ZEROS
057500970415     C                   LEAVE
057600970415E   9C                   ENDIF
057700970415E   8C                   ENDIF
057800970415E   7C                   ENDIF
057900970415E   6C                   ENDIF
058000970415E   5C                   ENDDO                                                  *FINE COD.TASS.ELEM.
058100970415E   4C                   ENDDO                                                  *DIFFERENZA = 0
058200970415E   3C                   ENDIF
058300970414     C*
058400970415E   2C                   ENDIF
058500970319E   1C                   ENDIF
058600970128     C*
058700970117     C                   ENDSR
058800970117     C*------------------------------------------------------------------------*
058900970117     C* CTRVCT - CONTROLLA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
059000970117     C*------------------------------------------------------------------------*
059100970117     C     CTRVCT        BEGSR
059200970117     C*
059300970415IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
059400970415     C                   Z-ADD     *ZEROS        TOTPPE            4 1          *% TOTALE
059500970415DO  2C     1             DO        WUVCTS        I
059600970415IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
059700970415IF  4C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
059800970117     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA TOTALE
059900970129     C* DA NON CALCOLARE
060000970415IF  5C     V1SRCT        IFEQ      *BLANKS
060100970415IF  6C     WERR          IFEQ      '0'
060200970415IF  7C     VRCT(I)       IFNE      *BLANKS
060300970128     C                   MOVEL     '1'           WERR
060400970128     C                   MOVEL     MSG(6)        $MSG
060500970128     C                   SETON                                        2290
060600970415E   7C                   ENDIF
060700970415E   6C                   ENDIF
060800970128     C*
060900970415IF  6C     WERR          IFEQ      '0'
061000970415IF  7C     VPPE(I)       IFGT      *ZEROS
061100970117     C                   MOVEL     '1'           WERR
061200970128     C                   MOVEL     MSG(7)        $MSG
061300970117     C                   SETON                                        2290
061400970415E   7C                   ENDIF
061500970415E   6C                   ENDIF
061600970415X   5C                   ELSE
061700970129     C* DA CALCOLARE
061800970415IF  6C     V1SRCT        IFNE      'M'                                          *NO SITUAZIONE MISTA
061900970415IF  7C     WERR          IFEQ      '0'
062000970415IF  8C     VRCT(I)       IFNE      V1SRCT
062100970117     C                   MOVEL     '1'           WERR
062200970128     C                   MOVEL     MSG(8)        $MSG
062300970117     C                   SETON                                        2290
062400970415E   8C                   ENDIF
062500970415E   7C                   ENDIF
062600970415IF  7C     WERR          IFEQ      '0'
062700970415IF  8C     VPPE(I)       IFEQ      *ZEROS
062800970129     C                   MOVEL     '1'           WERR
062900970129     C                   MOVEL     MSG(9)        $MSG
063000970129     C                   SETON                                        2290
063100970415E   8C                   ENDIF
063200970415E   7C                   ENDIF
063300970129     C*
063400970415X   6C                   ELSE                                                   *SITUAZIONE MISTA
063500970129     C*
063600970415IF  7C     WERR          IFEQ      '0'
063700970415IF  8C     VRCT(I)       IFNE      *BLANKS
063800970129     C     VPPE(I)       ANDEQ     *ZEROS
063900970129     C                   MOVEL     '1'           WERR
064000970129     C                   MOVEL     MSG(21)       $MSG
064100970129     C                   SETON                                        2290
064200970415E   8C                   ENDIF
064300970415E   7C                   ENDIF
064400970128     C*
064500970415IF  7C     WERR          IFEQ      '0'
064600970415IF  8C     VRCT(I)       IFEQ      *BLANKS
064700970129     C     VPPE(I)       ANDGT     *ZEROS
064800970128     C                   MOVEL     '1'           WERR
064900970129     C                   MOVEL     MSG(22)       $MSG
065000970129     C                   SETON                                        2290
065100970415E   8C                   ENDIF
065200970415E   7C                   ENDIF
065300970415E   6C                   ENDIF
065400970415E   5C                   ENDIF
065500970117     C*
065600970415E   4C                   ENDIF
065700970415E   3C                   ENDIF
065800970415E   2C                   ENDDO
065900970117     C*
066000970117     C* TOTALE %INCIDENZA CODICI TASSAZIONE = %INCIDENZA REGIONE
066100970117IF  2C     V1SPPE        IFNE      TOTPPE
066200970117     C                   MOVEL     '1'           WERR
066300970128     C                   MOVEL     MSG(10)       $MSG
066400970117     C                   SETON                                        2390
066500970117E   2C                   ENDIF
066600970117E   1C                   ENDIF
066700970117     C*
066800970117     C                   ENDSR
066900970117     C*------------------------------------------------------------------------*
067000970117     C* GESOP1 - GESTIONE OPZIONI DEL PRIMO SUBFILE
067100970117     C*------------------------------------------------------------------------*
067200970117     C     GESOP1        BEGSR
067300970122     C*
067400970129     C                   MOVEL     V1SOPZ        DEPOPZ
067500970129     C                   EXSR      AGGSF1                                       *AGGIORNA SFILE
067600970117SL  1C                   SELECT
067700970129     C     DEPOPZ        WHENEQ    '1 '
067800970117     C                   EXSR      O01GE1                                       *CODICI TASSAZIONE
067900970117     C                   OTHER
068000970117E   1C                   ENDSL
068100970117     C*
068200970117     C                   ENDSR
068300970117     C*------------------------------------------------------------------------*
068400970117     C* O01GE1 - GESTIONE OPZIONE PRIMO SFILE 1 ==> CODICI TASSAZIONE ELEMENTARI
068500970117     C*------------------------------------------------------------------------*
068600970117     C     O01GE1        BEGSR
068700970117     C*
068800970117     C* GESTIONE 3° SFILE
068900970117     C                   Z-ADD     NR1           $DPC1                          *DEPOSITO NRR
069000970117     C                   EXSR      IMPSF3                                       *IMPOSTAZIONI
069100970129DO  1C     $FINV         DOUEQ     'S'
069200970117     C                   EXSR      GESSF3                                       *GESTIONE
069300970129E   1C                   ENDDO
069400970129     C                   Z-ADD     $DPC1         NR1                            *DEPOSITO NRR
069500970117     C*
069600970117     C                   ENDSR
069700970117     C*------------------------------------------------------------------------*
069800970117     C* AGGSF1 - AGGIORNA RIGA DEL PRIMO SUBFILE
069900970117     C*------------------------------------------------------------------------*
070000970117     C     AGGSF1        BEGSR
070100970117     C*
070200970124     C                   MOVEL     *BLANKS       V1SOPZ
070300970117     C                   SETON                                        70        *SI SFLNXTCHG
070400970117IF  1C     $TGES         IFEQ      'I'
070500970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
070600970117X   1C                   ELSE
070700970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
070800970117E   1C                   ENDIF
070900970117IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
071000970117     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
071100970127X   1C                   ELSE                                                   *DA PORTO
071200970117     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
071300970117E   1C                   ENDIF
071400970117     C                   UPDATE    TE03S01                                      *AGGIORNA SUBFILE
071500970117     C                   SETOFF                                       70        *NO SFLNXTCHG
071600970117     C*
071700970117     C                   ENDSR
071800970117     C*------------------------------------------------------------------------*
071900970124     C* ENTSF1 - GESTIONE TASTO FUNZIONALE INVIO 1°SFILE  ==> AGGIORNA
072000970117     C*------------------------------------------------------------------------*
072100970124     C     ENTSF1        BEGSR
072200970117     C*
072300970117IF  1C     $TGES         IFEQ      'G'                                          *SE GESTIONE
072400970117     C* AGGIORNA L'ARCHIVIO
072500980508     C*---                 EXSR AGGETT
072600970129     C*
072700970129     C* AGGIORNA LE SCHIERE VIDEO
072800970129     C                   EXSR      AGGRCT
072900970129     C*
073000970117     C* CARICA IL SUBFILE
073100970117     C                   EXSR      POSSF1
073200970117E   1C                   ENDIF
073300970117     C*
073400970117     C                   ENDSR
073500970117     C*------------------------------------------------------------------------*
073600970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
073700970117     C*------------------------------------------------------------------------*
073800970117     C     F03GES        BEGSR
073900970117     C*
074000970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
074100970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
074200011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
074300970117     C*
074400970117     C                   ENDSR
074500970117     C*------------------------------------------------------------------------*
074600970117     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
074700970117     C*------------------------------------------------------------------------*
074800970117     C     F05GES        BEGSR
074900970117     C*
075000970117     C* GESTIONE VIDETA
075100970117     C                   EXSR      IMPWD2
075200970117DO  1C     $FINV         DOUEQ     'S'
075300970117     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
075400970117E   1C                   ENDDO
075500970117     C*
075600970117     C* MEMORIZZA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA E CARICA IL SFILE
075700970117IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
075800980708     C                   MOVEL     V2WDSF        WDSF                           *IMPOSTA DATI CHIAVE
075900980708     C                   MOVEL     V2WCTO        WCTO
076000980708     C                   Z-ADD     V2WKSC        WKSC
076100980708     C                   Z-ADD     V2WCTR        WCTR
076200980708     C                   Z-ADD     V2WPRG        WPRG
076300980708     C                   Z-ADD     V2WLNP        WLNP
076400980708     C                   MOVEL     D00FIE        WFIE
076500040831     C                   MOVEL     D00dpd        Wdpd
076600011003     C                   MOVEL     V2WDIV        WDIV
076700970117     C                   EXSR      MEMETT                                       *MEMORIZZA
076800970117     C                   EXSR      POSSF1                                       *CARICA
076900980708     C                   MOVEL     V2WDSF        D0DDSF                         *DEPOSITA CHIAVE CAR
077000980708     C                   MOVEL     V2WCTO        D0DCTO
077100980708     C                   Z-ADD     V2WKSC        D0DKSC
077200980708     C                   Z-ADD     V2WCTR        D0DCTR
077300980708     C                   Z-ADD     V2WPRG        D0DPRG
077400980708     C                   Z-ADD     V2WLNP        D0DLNP
077500011003     C                   MOVEL     V2WDIV        D0DDIV
077600970117E   1C                   ENDIF
077700970117     C*
077800970117     C                   ENDSR
077900970117     C*------------------------------------------------------------------------*
078000970117     C* MEMETT - MEMORIZZA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
078100970117     C*------------------------------------------------------------------------*
078200970117     C     MEMETT        BEGSR
078300970117     C*
078400970117     C* CARICA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
078500970117     C                   EXSR      CARETT
078600980508     C*
078700980508     C* AGGIORNA LE SCHIERE VIDEO
078800980508     C                   EXSR      AGGSCV
078900980508     C*
079000980508     C                   ENDSR
079100980508     C*------------------------------------------------------------------------*
079200970117     C* CARETT - CARICA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
079300970117     C*------------------------------------------------------------------------*
079400970117     C     CARETT        BEGSR
079500980508     C*
079600980508     C* VALORIZZA I CODICI TASSAZIONE CON QUELLI CARICATI DA TABELLA "CT"
079700980508     C                   MOVEL     VCTS          CCTS
079800980514     C                   MOVEL     VRAP          CRAP
079900970117     C*
080000980508     C* PULISCE LE SCHIERE DI MEMORIZZAZIONE
080100980508     C                   CLEAR                   CRCT                           *SCHIERE CARICAMENTO
080200970117     C                   CLEAR                   CPPE
080300970117     C*
080400980508     C* CARICA DATI CODICI TASSAZIONE
080500980708     C                   MOVEL     WDSF          KTTDSF                         *DATI SEDE O FILIALE
080600980708     C                   MOVEL     WCTO          KTTCTO                         *TARIFFA O OFFERTA
080700980708     C                   Z-ADD     WKSC          KTTKSC                         *CLIENTE O VISITA
080800980708     C                   Z-ADD     WCTR          KTTCTR                         *TARIFFA
080900980708     C                   Z-ADD     WPRG          KTTPRG                         *PROGRESSIVO
081000980708     C                   Z-ADD     WLNP          KTTLNP                         *LINEA DI PARTENZA
081100980708     C                   MOVEL     WFIE          KTTFIE                         *ITALIA/ESTERO
081200040831     C                   MOVEL     Wdpd          KTTdpd                         *ITALIA/ESTERO
081300011001     C     KE2ETT        SETLL     TEETT01L                               99
081400011001     C     KE2ETT        READE(N)  TEETT01L                               99
081500970117DO  1C     *IN99         DOWEQ     *OFF
081600980508IF  2C     ETTATB        IFEQ      *BLANKS                                      *NON ANNULLATO
081700980508     C                   SETOFF                                           98
081800980508     C                   Z-ADD     1             J
081900980515     C     ETTCTS        LOOKUP    CCTS(J)                                98
082000980508IF  3C     *IN98         IFEQ      *ON                                          *CODICE TROVATO
082100980508     C                   MOVEL     ETTRCT        CRCT(J)                        *RAGGRUPPAMENTO
082200980508     C                   Z-ADD     ETTPPE        CPPE(J)                        *%INCIDENZA
082300980911     C     ETTRCT        IFEQ      'N'
082400980911     C     ETTPPE        ANDEQ     *ZEROS
082500980911     C                   MOVEL     *BLANKS       CRCT(J)
082600980911     C                   ENDIF
082700970129E   3C                   ENDIF
082800970129E   2C                   ENDIF
082900011001     C     KE2ETT        READE(N)  TEETT01L                               99
083000970117E   1C                   ENDDO
083100980508     C*
083200970117     C                   ENDSR
083300980508     C*------------------------------------------------------------------------*
083400980508     C* CARSTD - CARICA I CODICI TASSAZIONE DA TABELLA
083500980508     C*------------------------------------------------------------------------*
083600980508     C     CARSTD        BEGSR
083700980508     C*
083800980508     C* VALORIZZA I CODICI TASSAZIONE CON QUELLI CARICATI DA TABELLA "CT"
083900980708     C                   MOVEL     VCTS          CCTS
084000980708     C                   MOVEL     VRAP          CRAP
084100980514     C                   MOVEL     'T'           CRCT
084200980508     C                   Z-ADD     000,1         CPPE
084300980508     C*
084400980508     C                   ENDSR
084500980508     C*------------------------------------------------------------------------*
084600980508     C* AGGSCV - AGGIORNA LE SCHIERE VIDEO
084700980508     C*------------------------------------------------------------------------*
084800980508     C     AGGSCV        BEGSR
084900980508     C*
085000980508     C* PULISCE LE VARIABILI DI LAVORO
085100980902IF  1C*---       D00TLA    IFEQ 'P'                        *PORTO
085200980508     C                   CLEAR                   VRCT                           *SCHIERE VIDEO
085300980902     C                   CLEAR                   VPPE
085400980902E   1C*---                 ENDIF
085500980508     C*
085600980508     C* MEMORIZZA I DATI TIPO DELLA TARIFFA RICHIESTA NELLE SCHIERE VIDEO
085700980508DO  1C     1             DO        WUVCTS        J
085800980508IF  2C     CCTS(J)       IFNE      *BLANKS
085900980508     C                   SETOFF                                           98
086000980508     C                   Z-ADD     1             I
086100980508     C     CCTS(J)       LOOKUP    VCTS(I)                                98
086200980508IF  3C     *IN98         IFEQ      *ON
086300980508     C*---
086400980508     C* DELTA -> CARICA SOLO LE %INCIDENZA DEI CODICI DA CALCOLARE
086500980508     C*---
086600980902IF  4C*---       D00TLA    IFEQ 'D'
086700980902     C*---                 MOVELCRCT,J    VRCT,I           *RAGGRUPPAMENTO
086800980902     C*---                 Z-ADDCPPE,J    VPPE,I           *%INCIDENZA
086900980902X   4C*---                 ELSE
087000980508     C*---
087100980508     C* PORTO -> CARICA LE %INCIDENZA E I RAGGRUPPAMENTI DI TUTTI I CODICI
087200980508     C*---
087300980508     C                   MOVEL     CRCT(J)       VRCT(I)                        *RAGGRUPPAMENTO
087400980508     C                   Z-ADD     CPPE(J)       VPPE(I)                        *%INCIDENZA
087500980902E   4C*---                 ENDIF
087600980508E   3C                   ENDIF
087700980508E   2C                   ENDIF
087800980508E   1C                   ENDDO
087900980508     C*
088000980508     C* AGGIORNA I CODICI TASSAZIONE DELLA REGIONE
088100980508     C                   EXSR      AGGRCT
088200980508     C*
088300980508     C                   ENDSR
088400970117     C*------------------------------------------------------------------------*
088500970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
088600970117     C*------------------------------------------------------------------------*
088700970117     C     F06GES        BEGSR
088800970117     C*
088900970117IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
089000970117     C*---
089100970117     C* TOTALE %INCIDENZA = 100
089200970117     C*---
089300970117IF  2C     V1CPPE        IFNE      100,0
089400970117     C                   SETON                                        2990
089500970117     C                   MOVEL     MSG(5)        $MSG
089600970117     C                   MOVEL     '1'           WERR
089700970117     C                   Z-ADD     1             $NR1                           *INIZIO SFILE
089800970117E   2C                   ENDIF
089900970117     C* AGGIORNA ARCHIVIO
090000970117IF  2C     WERR          IFEQ      '0'                                          *NO ERRORI
090100970117     C                   EXSR      AGGETT
090200970129     C*
090300970129     C* AGGIORNA LE SCHIERE VIDEO
090400970129     C                   EXSR      AGGRCT
090500970117     C*---
090600970117     C* PORTO -> GESTIONE VIDETA DI CONFERMA
090700970117     C*---
090800970117IF  3C     D00TLA        IFEQ      'P'
090900970117     C                   EXSR      IMPWD3
091000970117DO  4C     $FINV         DOUEQ     'S'
091100970117     C                   EXSR      GESWD3                                       *GESTIONE FINESTRA
091200970117E   4C                   ENDDO
091300970117IF  4C     V3WRIS        IFEQ      'SI'                                         *SE CONFERMATO
091400970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
091500011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
091600970117E   4C                   ENDIF
091700970117     C*---
091800970117     C* DELTA -> ESCE E PROSEGUE
091900970117     C*---
092000970117X   3C                   ELSE
092100011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
092200970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
092300970117E   3C                   ENDIF
092400970117E   2C                   ENDIF
092500970117X   1C                   ELSE                                                   INTERROGAZIONE
092600011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
092700970130     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
092800970117E   1C                   ENDIF
092900970117     C*
093000970117     C                   ENDSR
093100970117     C*------------------------------------------------------------------------*
093200011001     C* AGGETT - AGGIORNAMENTO ARCHIVIO -TEETT-
093300970117     C*------------------------------------------------------------------------*
093400970124     C     AGGETT        BEGSR
093500980515     C*
093600980515     C* ANNULLA TUTTI I DATI DELLA TARIFFA IN LINEA
093700980515     C                   MOVEL     D00DSF        KTTDSF
093800980515     C                   MOVEL     D00CTO        KTTCTO
093900980515     C                   Z-ADD     D00KSC        KTTKSC
094000980515     C                   Z-ADD     D00CTR        KTTCTR
094100980515     C                   Z-ADD     D00PRG        KTTPRG
094200980515     C                   Z-ADD     D00LNP        KTTLNP
094300980515     C                   MOVEL     D00FIE        KTTFIE
094400040831     C                   MOVEL     D00dpd        KTTdpd
094500011001     C     KE2ETT        SETLL     TEETT01L                               99
094600011001     C     KE2ETT        READE     TEETT01L                               99
094700980515DO  1C     *IN99         DOWEQ     *OFF
094800980515IF  2C     ETTDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
094900980515     C                   MOVEL     'A'           ETTATB
095000980515     C                   MOVEL     *BLANKS       ETTFTR
095100011001     C                   UPDATE    TEETT000                                     *AGGIORNA
095200980515X   2C                   ELSE                                                   *MAI TRASMESSO
095300011001     C                   DELETE    TEETT000                                     *CANCELLA
095400980515E   2C                   ENDIF
095500011001     C     KE2ETT        READE     TEETT01L                               99
095600980515E   1C                   ENDDO
095700011001     C                   UNLOCK    TEETT01L
095800970117     C*
095900970117     C* LEGGE TUTTI I RECORD DEL SFILE
096000970117DO  2C     1             DO        WUVCTS        I
096100970117IF  2C     VCTS(I)       IFNE      *BLANKS                                      *SE IMMESSO QC.
096200970117     C*
096300970117     C* LEGGE IL RECORD IN ARCHIVIO
096400980508     C                   MOVEL     D00DSF        KTTDSF
096500980508     C                   MOVEL     D00CTO        KTTCTO
096600970117     C                   Z-ADD     D00KSC        KTTKSC
096700970117     C                   Z-ADD     D00CTR        KTTCTR
096800970117     C                   Z-ADD     D00PRG        KTTPRG
096900970117     C                   Z-ADD     D00LNP        KTTLNP
097000970117     C                   MOVEL     D00FIE        KTTFIE
097100040831     C                   MOVEL     D00dpd        KTTdpd
097200970117     C                   MOVEL     VCTS(I)       KTTCTS
097300011001     C     KEYETT        CHAIN     TEETT01L                           99
097400970117     C* INSERIMENTO
097500970117IF  3C     *IN99         IFEQ      *ON
097600970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
097700011001     C                   WRITE     TEETT000
097800970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZIONE
097900970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
098000970117X   3C                   ELSE
098100970117     C* VARIAZIONE
098200970117IF  4C     VRCT(I)       IFNE      ETTRCT                                       *SE MODIFICATO QC.
098300970117     C     VPPE(I)       ORNE      ETTPPE
098400970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
098500020715X   4C                   ELSE
098600020716     C                   MOVEL     *BLANKS       ETTFTR
098700020716     C                   Z-ADD     DATCOR        ETTDUV
098800020715     C                   MOVEL     *BLANKS       ETTATB
098900020715E   4C                   ENDIF
099000011001     C                   UPDATE    TEETT000
099100970117E   3C                   ENDIF
099200970117E   2C                   ENDIF
099300970117E   1C                   ENDDO                                                  *FINE SUBFILE
099400970117     C*
099500970117     C                   ENDSR
099600970117     C*------------------------------------------------------------------------*
099700011001     C* IMPETT - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETT-
099800970117     C*------------------------------------------------------------------------*
099900970117     C     IMPETT        BEGSR
100000970117     C*
100100970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
100200980508     C                   MOVEL     KTTDSF        ETTDSF
100300980508     C                   MOVEL     KTTCTO        ETTCTO
100400970117     C                   Z-ADD     KTTKSC        ETTKSC
100500970117     C                   Z-ADD     KTTCTR        ETTCTR
100600970117     C                   Z-ADD     KTTPRG        ETTPRG
100700970117     C                   Z-ADD     KTTLNP        ETTLNP
100800970117     C                   MOVEL     KTTFIE        ETTFIE
100900040831     C                   MOVEL     KTTdpd        ETTdpd
101000970117     C                   MOVEL     KTTCTS        ETTCTS
101100970117     C                   Z-ADD     DATCOR        ETTDIR
101200970117     C                   Z-ADD     *ZEROS        ETTDUV
101300970117     C                   MOVEL     *BLANKS       ETTFTR
101400970117     C                   Z-ADD     *ZEROS        ETTDTR
101500970117X   1C                   ELSE                                                   *VARIAZIONE
101600970117     C                   MOVEL     *BLANKS       ETTFTR
101700970117     C                   Z-ADD     DATCOR        ETTDUV
101800970117E   1C                   ENDIF
101900970117     C                   MOVEL     VRCT(I)       ETTRCT
102000970117     C                   Z-ADD     VPPE(I)       ETTPPE
102100970117     C                   MOVEL     *BLANKS       ETTATB
102200970117     C*
102300970117     C                   ENDSR
102400970117     C*------------------------------------------------------------------------*
102500970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
102600970117     C*------------------------------------------------------------------------*
102700970117     C     F12GES        BEGSR
102800970117     C*
102900970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
103000970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
103100011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
103200970117     C*
103300970117     C                   ENDSR
103400970117     C*------------------------------------------------------------------------*
103500970117     C* F07GES - GESTIONE TASTO FUNZIONALE F07 ==> RAPPRESENTAZIONE
103600970117     C*------------------------------------------------------------------------*
103700970117     C     F07GES        BEGSR
103800970117     C*
103900970117     C* SVILUPPA IL TIPO DI RAPPRESENTAZIONE
104000970117     C                   EXSR      SVIRAP
104100970117     C*
104200970117     C* GESTIONE VIDEATA
104300970117     C                   EXSR      IMPSF2
104400970117DO  1C     $FINV         DOUEQ     'S'
104500970117     C                   EXSR      GESSF2                                       *GESTIONE VIDEATA
104600970117E   1C                   ENDDO
104700970117     C*
104800970117     C                   ENDSR
104900970117     C*------------------------------------------------------------------------*
105000970117     C* SVIRAP - SVILUPPA IL TIPO DI RAPPRESENTAZIONE
105100970117     C*------------------------------------------------------------------------*
105200970117     C     SVIRAP        BEGSR
105300970117     C*
105400970117     C* PULISCE LE SCHIERE VIDEO DI OUTPUT
105500970117     C                   CLEAR                   OCTS                           *CODICE
105600970117     C                   CLEAR                   OPPE                           *%INCIDENZA
105700970117     C                   CLEAR                   OCOR                           *ORDINAMENTO
105800970117     C*
105900970117     C                   Z-ADD     *ZEROS        J                 4 0
106000970117DO  1C     1             DO        WUVCTS        I
106100970117IF  2C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
106200970117     C* DA CALCOLARE
106300970117IF  3C     VRCT(I)       IFEQ      'T'
106400970117     C                   ADD       1             J
106500970117     C                   MOVEL     VCTS(I)       OCTS(J)                        *CODICE
106600970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
106700970117     C                   MOVEL     VCOR(I)       OCOR(J)                        *ORDINAMENTO
106800970117X   3C                   ELSE
106900970117     C                   MOVEL     VCTS(I)       WCTS
107000970117     C                   MOVEL     *BLANKS       WRAP
107100970117     C                   MOVEL     *BLANKS       WNAZ
107200970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
107300970117     C* DA CALCOLARE IN REGIONE
107400970117IF  4C     VRCT(I)       IFEQ      'R'
107500970117     C                   SETOFF                                           98
107600970117     C                   Z-ADD     1             C                 4 0
107700970117     C     WRAP          LOOKUP    OCTS(C)                                98
107800970117IF  5C     *IN98         IFEQ      *OFF                                         *MAI  MEMORIZZATO
107900970117     C                   MOVEL     WRAP          WCTS
108000970117     C                   MOVEL     *BLANKS       WCOR
108100970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
108200970117     C                   ADD       1             J
108300970117     C                   MOVEL     WRAP          OCTS(J)                        *CODICE
108400970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
108500970117     C                   MOVEL     WCOR          OCOR(J)                        *ORDINAMENTO
108600970117X   5C                   ELSE                                                   *GIA' MEMORIZZATO
108700970117     C     VPPE(I)       ADD       OPPE(C)       OPPE(C)                        *SOMMA PESI
108800970117E   5C                   ENDIF
108900970117E   4C                   ENDIF
109000970117     C* DA CALCOLARE IN NAZIONE
109100970117IF  4C     VRCT(I)       IFEQ      'N'
109200970117     C                   SETOFF                                           98
109300970117     C                   Z-ADD     1             C                 4 0
109400970117     C     WNAZ          LOOKUP    OCTS(C)                                98
109500970117IF  5C     *IN98         IFEQ      *OFF                                         *MAI  MEMORIZZATO
109600970117     C                   MOVEL     WNAZ          WCTS
109700970117     C                   MOVEL     *BLANKS       WCOR
109800970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
109900970117     C                   ADD       1             J
110000970117     C                   MOVEL     WNAZ          OCTS(J)                        *CODICE
110100970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
110200970117     C                   MOVEL     WCOR          OCOR(J)                        *ORDINAMENTO
110300970117X   5C                   ELSE                                                   *GIA' MEMORIZZATO
110400970117     C     VPPE(I)       ADD       OPPE(C)       OPPE(C)                        *SOMMA PESI
110500970117E   5C                   ENDIF
110600970117E   4C                   ENDIF
110700970117E   3C                   ENDIF
110800970117E   2C                   ENDIF
110900970117E   1C                   ENDDO
111000970117     C                   Z-ADD     J             WUOCTS            4 0          *N°COD.TASS.OUTPUT
111100970117     C*
111200970117     C* ORDINA CODICI RAGGRUPPATI VIDEO
111300970117     C                   EXSR      ORDOCT
111400970117     C*
111500970117     C                   ENDSR
111600970117     C*------------------------------------------------------------------------*
111700970117     C* DATCTS - REPERISCE I DATI DEL CODICE TASSAZIONE
111800970117     C*------------------------------------------------------------------------*
111900970117     C     DATCTS        BEGSR
112000970117     C*
112100970117     C     *LIKE         DEFINE    ETTCTS        WCTS
112200970117     C     *LIKE         DEFINE    ETTCTS        WRAP
112300970117     C     *LIKE         DEFINE    ETTCTS        WNAZ
112400970117     C     *LIKE         DEFINE    §CTCOR        WCOR
112500970117     C*
112600970117     C                   SETOFF                                           98
112700970117     C                   Z-ADD     1             C                 4 0
112800970117     C     WCTS          LOOKUP    CTS(C)                                 98
112900970117IF  1C     *IN98         IFEQ      *ON
113000970117     C                   MOVEL     RAP(C)        WRAP                           *COD.TASS.REGIONE
113100970117     C                   MOVEL     NAZ(C)        WNAZ                           *COD.TASS.NAZIONE
113200970117     C                   MOVEL     COR(C)        WCOR                           *COD.ORDINAMENTO
113300970117E   1C                   ENDIF
113400970117     C*
113500970117     C                   ENDSR
113600970117     C*------------------------------------------------------------------------*
113700970117     C* ORDOCT - ORDINA I CODICI TASSAZIONE RAGGRUPPATI DA EMETTERE A VIDEO
113800970117     C*------------------------------------------------------------------------*
113900970117     C     ORDOCT        BEGSR
114000970117     C*
114100970117DO  1C     1             DO        WUOCTS        I
114200970117IF  2C     OCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
114300970117     C     1             ADD       I             J                 4 0
114400970117DO  3C     J             DO        WUOCTS        J
114500970117IF  4C     OCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
114600970117IF  5C     OCOR(I)       IFGT      OCOR(J)
114700970117     C                   MOVEL     OCTS(I)       PCTS                           *CODICE
114800970117     C                   MOVEL     OCTS(J)       OCTS(I)
114900970117     C                   MOVEL     PCTS          OCTS(J)
115000970117     C                   MOVEL     OPPE(I)       PPPE                           *%INCIDENZA
115100970117     C                   MOVEL     OPPE(J)       OPPE(I)
115200970117     C                   MOVEL     PPPE          OPPE(J)
115300970117     C                   MOVEL     OCOR(I)       PCOR                           *ORDINAMENTO
115400970117     C                   MOVEL     OCOR(J)       OCOR(I)
115500970117     C                   MOVEL     PCOR          OCOR(J)
115600970117E   5C                   ENDIF
115700970117E   4C                   ENDIF
115800970117E   3C                   ENDDO                                                  *FINE CICLO UNO
115900970117E   2C                   ENDIF
116000970117E   1C                   ENDDO                                                  *FINE CICLO DUE
116100970117     C*
116200970117     C                   ENDSR
116300970117     C*------------------------------------------------------------------------*
116400970117     C* IMPSF2 - IMPOSTAZIONI INIZIALI SECONDO SUBFILE
116500970117     C*------------------------------------------------------------------------*
116600970117     C     IMPSF2        BEGSR
116700970117     C*
116800970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
116900970129     C                   MOVEL     '2'           TIPVID                         *VIDEO IN GESTIONE
117000970117     C                   EXSR      SETSF2                                       *SFILE
117100970117     C                   MOVEL     'N'           $FINV                          *FINE VIDEATA
117200970117     C* CARICA IL SUBFILE
117300970117     C                   EXSR      POSSF2
117400970117     C*
117500970117     C                   ENDSR
117600970117     C*------------------------------------------------------------------------*
117700970117     C* SETSF2 - IMPOSTAZIONI INDICATORI SECONDO SUBFILE
117800970117     C*------------------------------------------------------------------------*
117900970117     C     SETSF2        BEGSR
118000970117     C*
118100970117     C                   SETOFF                                       90         GENERICO DI ERRORE
118200970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
118300970117     C*
118400970117     C                   ENDSR
118500970117     C*------------------------------------------------------------------------*
118600970117     C* GESSF2 - GESTIONE SECONDO SUBFILE
118700970117     C*------------------------------------------------------------------------*
118800970117     C     GESSF2        BEGSR
118900970117     C*
119000970117     C* EMISSIONE VIDEATA
119100970117     C                   WRITE     TE03D00                                      *TESTATA
119200970117     C                   WRITE     TE03Z02                                      *PIEDI
119300970117     C                   EXFMT     TE03C02                                      *SFILE
119400970117     C                   EXSR      SETSF2
119500970117     C* F12-RITORNO
119600970117IF  1C     *INKL         IFEQ      *ON
119700970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
119800970130     C                   MOVEL     '1'           TIPVID
119900970117     C                   GOTO      FINSF2
120000970117E   1C                   ENDIF
120100970117     C* INVIO-RITORNA
120200970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
120300970117     C*
120400970117     C     FINSF2        ENDSR
120500970117     C*------------------------------------------------------------------------*
120600970117     C* IMPSF3 - IMPOSTAZIONI INIZIALI TERZO SUBFILE
120700970117     C*------------------------------------------------------------------------*
120800970117     C     IMPSF3        BEGSR
120900970117     C*
121000970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
121100970129     C                   MOVEL     '3'           TIPVID                         *VIDEO IN GESTIONE
121200970117     C                   EXSR      SETSF3                                       *SFILE
121300970117     C*
121400970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
121500970117     C                   EXSR      SETSC3                                       *SFILE CONTROL
121600970117     C* CARICA IL SUBFILE
121700970129     C*---       $DPC1     CHAINTE03S01              98    *RILEGGE COD.TASS.
121800970117     C                   EXSR      POSSF3
121900970117     C*
122000970117     C                   ENDSR
122100970117     C*------------------------------------------------------------------------*
122200970117     C* SETSF3 - IMPOSTAZIONI INDICATORI TERZO SUBFILE
122300970117     C*------------------------------------------------------------------------*
122400970117     C     SETSF3        BEGSR
122500970117     C*
122600970117     C                   SETOFF                                       19         ERRORI CONTROL
122700970117     C                   SETOFF                                       2021       ERRORI SFILE
122800970117     C                   SETOFF                                       90         GENERICO DI ERRORE
122900970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
123000970129     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
123100970117     C*
123200970117     C                   ENDSR
123300970117     C*------------------------------------------------------------------------*
123400970117     C* SETSC3 - IMPOSTAZIONI INDICATORI E CAMPI TERZO SUBFILE CONTROL
123500970117     C*------------------------------------------------------------------------*
123600970117     C     SETSC3        BEGSR
123700970117     C*
123800970117     C                   MOVEL     V1SCTS        V3CRAP
123900970117     C                   MOVEL     V1SDCT        V3CDRA
124000970117     C                   MOVEL     V1SRCT        V3CRCT
124100970117     C                   MOVEL     V1SDRC        V3CDRC
124200970117     C                   Z-ADD     V1SPPE        V3CPPR
124300970117     C                   Z-ADD     *ZEROS        V3CPPE
124400970117     C*
124500970117     C                   ENDSR
124600970117     C*------------------------------------------------------------------------*
124700970117     C* GESSF3 - GESTIONE TERZO SUBFILE
124800970117     C*------------------------------------------------------------------------*
124900970117     C     GESSF3        BEGSR
125000970117     C*
125100970117     C* EMISSIONE VIDEATA
125200970117IF  1C     $TESVD        IFEQ      '1'
125300970117     C                   WRITE     TE03D00                                      *TESTATA
125400970117     C                   MOVEL     '0'           $TESVD
125500970117E   1C                   ENDIF
125600970117     C                   WRITE     TE03Z03                                      *PIEDI
125700970117     C                   EXFMT     TE03C03                                      *SFILE
125800970117     C                   EXSR      SETSF3
125900970117     C* F12-RITORNO
126000970117IF  1C     *INKL         IFEQ      *ON
126100970129     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
126200970117     C                   MOVEL     '1'           TIPVID
126300970117     C                   EXSR      SETSF1                                       *REIMPOSTA INDICATOR
126400970117     C                   GOTO      FINSF3
126500970117E   1C                   ENDIF
126600970117     C* CONTROLLA VIDEO
126700970117     C                   EXSR      CTRSF3
126800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
126900970117     C                   GOTO      FINSF3
127000970117E   1C                   ENDIF
127100970317     C* INVIO-AGGIORNA
127200970124     C                   EXSR      ENTSF3
127300970117     C*
127400970117     C     FINSF3        ENDSR
127500970117     C*------------------------------------------------------------------------*
127600970117     C* CTRSF3 - CONTROLLA TERZO SUBFILE
127700970117     C*------------------------------------------------------------------------*
127800970117     C     CTRSF3        BEGSR
127900970117     C*
128000970117     C* REIMPOSTA VARIABILI DI LAVORO
128100970117     C                   MOVEL     '0'           WERR              1            *ERRORE
128200970117     C                   Z-ADD     *ZEROS        V3CPPE                         *TOTALE PESI %
128300970117     C*
128400970117     C* LEGGE TUTTI I RECORD DEL SFILE
128500970117     C                   Z-ADD     1             NR3
128600970117     C     NR3           CHAIN     TE03S03                            98
128700970117DO  1C     *IN98         DOWEQ     *OFF
128800970117     C                   SETOFF                                       80        *NO SFLNXTCHG
128900970117     C*--------------------
129000970117     C* RAGGRUPPAMENTO
129100970117     C*--------------------
129200970117IF  2C     WERR          IFEQ      '0'
129300970117     C                   MOVEL     V3SCTS        WCTS
129400970117     C                   MOVEL     V3SRCT        WRCT
129500970117     C                   MOVEL     *BLANKS       WDRCT
129600970117     C                   EXSR      CHKRCT
129700970117     C                   MOVEL     WRCT          V3SRCT
129800970117     C                   MOVEL     WDRCT         V3SDRC
129900970117IF  3C     WERR          IFEQ      '1'
130000970117     C                   SETON                                        2090
130100970117     C                   MOVEL     MSG(2)        $MSG
130200970117E   3C                   ENDIF
130300970117E   2C                   ENDIF
130400970117     C*--------------------
130500970117     C* %INCIDENZA
130600970117     C*--------------------
130700970117IF  2C     WERR          IFEQ      '0'
130800970117IF  3C     V3SRCT        IFNE      *BLANKS                                      *DA CALCOLARE
130900970117IF  4C     V3SPPE        IFEQ      *ZEROS                                       *% VUOTA
131000970117     C                   SETON                                        2190
131100970117     C                   MOVEL     MSG(3)        $MSG
131200970117     C                   MOVEL     '1'           WERR
131300970117E   4C                   ENDIF
131400970117X   3C                   ELSE                                                   *DA NON CALCOLARE
131500970117IF  4C     V3SPPE        IFGT      *ZEROS                                       *% PIENA
131600970117     C                   SETON                                        2190
131700970117     C                   MOVEL     MSG(4)        $MSG
131800970117     C                   MOVEL     '1'           WERR
131900970117E   4C                   ENDIF
132000970117E   3C                   ENDIF
132100970117E   2C                   ENDIF
132200970117     C* AGGIORNA SFILE
132300970117IF  2C     WERR          IFEQ      '1'
132400970117     C                   Z-ADD     NR3           $NR3                           *POSIZIONAMENTO
132500970117E   2C                   ENDIF
132600970319     C*
132700970319     C     V3CPPE        ADD       V3SPPE        V3CPPE                         *TOTALE PESI %
132800970117     C                   EXSR      AGGSF3
132900970319     C*
133000970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
133100970117     C                   GOTO      FINCT3                                       *ESCE
133200970117X   2C                   ELSE                                                   *N O N  ERRORE
133300970117     C                   ADD       1             NR3                            *LEGGE ALTRO RECORD
133400970117     C     NR3           CHAIN     TE03S03                            98
133500970117E   2C                   ENDIF
133600970117E   1C                   ENDDO                                                  *FINE SUBFILE
133700970117     C*
133800970117     C     FINCT3        ENDSR
133900970117     C*------------------------------------------------------------------------*
134000970117     C* AGGSF3 - AGGIORNA RIGA DEL TERSO SUBFILE
134100970117     C*------------------------------------------------------------------------*
134200970117     C     AGGSF3        BEGSR
134300970117     C*
134400970117     C                   SETON                                        80        *SI SFLNXTCHG
134500970117IF  1C     $TGES         IFEQ      'I'
134600970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
134700970117X   1C                   ELSE
134800970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
134900970117E   1C                   ENDIF
135000970117IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
135100970117     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
135200970117X   1C                   ELSE
135300970117     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
135400970117E   1C                   ENDIF
135500970117     C                   UPDATE    TE03S03                                      *AGGIORNA SUBFILE
135600970117     C                   SETOFF                                       80        *NO SFLNXTCHG
135700970117     C*
135800970117     C                   ENDSR
135900970124     C*------------------------------------------------------------------------*
136000970124     C* ENTSF3 - GESTIONE TASTO FUNZIONALE INVIO 3°SFILE  ==> AGGIORNA
136100970124     C*------------------------------------------------------------------------*
136200970124     C     ENTSF3        BEGSR
136300970124     C*
136400970124     C                   EXSR      AGGCTS
136500970124     C                   EXSR      AGGRCT
136600970129     C* AGGIORNA PRIMO SFILE
136700970129     C                   Z-ADD     1             J
136800970129     C     V3CRAP        LOOKUP    RCTS(J)                                98
136900970129     C     $DPC1         CHAIN     TE03S01                            98        *RILEGGE COD.TASS.
137000970129     C                   MOVEL     RRCT(J)       V1SRCT                         *RAGGRUPPAMENTO
137100970129     C                   Z-ADD     RPPE(J)       V1SPPE                         *%INCIDENZA
137200970129     C                   MOVEL     RHRC(J)       V1HRCT                         *RAGGRUPP.TO HIDDEN
137300970129     C                   MOVEL     RSCH(J)       V1SCTE                         *CAMPONE RAGGRUPP.TI
137400970129     C                   EXSR      AGGSF1
137500970128     C*
137600970124     C                   ENDSR
137700970124     C*------------------------------------------------------------------------*
137800970124     C* AGGCTS - AGGIORNA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
137900970124     C*------------------------------------------------------------------------*
138000970124     C     AGGCTS        BEGSR
138100970124     C*
138200970124     C* LEGGE TUTTI I RECORD DEL SFILE
138300970124     C                   Z-ADD     1             NR3
138400970124     C     NR3           CHAIN     TE03S03                            98
138500970124DO  1C     *IN98         DOWEQ     *OFF
138600970124     C                   Z-ADD     1             J
138700970124     C     V3SCTS        LOOKUP    VCTS(J)                                98
138800970128     C                   MOVEL     V3SRCT        VRCT(J)                        *RAGGRUPPAMENTO
138900970128     C                   Z-ADD     V3SPPE        VPPE(J)                        *%INCIDENZA
139000970124     C                   ADD       1             NR3
139100970124     C     NR3           CHAIN     TE03S03                            98
139200970124E   1C                   ENDDO                                                  *FINE SUBFILE
139300970124     C*
139400970124     C                   ENDSR
139500970117     C*------------------------------------------------------------------------*
139600970124     C* AGGRTC - AGGIORNA I CODICI TASSAZIONE REGIONALI
139700970117     C*------------------------------------------------------------------------*
139800970117     C     AGGRCT        BEGSR
139900980708     C*
140000980708     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
140100980708     C                   CLEAR                   RRCT
140200980708     C                   CLEAR                   RHRC
140300980708     C                   CLEAR                   RPPE
140400970117     C*
140500970117     C* PER CIAUSCUNA REGIONE
140600970117DO  1C     1             DO        WURCTS        J
140700970117     C                   MOVEL     '0'           $RB               1            *RAGGRUPPAMENTI
140800970117     C                   MOVEL     '0'           $RN               1
140900970117     C                   MOVEL     '0'           $RR               1
141000970117     C                   MOVEL     '0'           $RT               1
141100970117     C                   Z-ADD     *ZEROS        TOTPPE
141200970117DO  2C     1             DO        WUVCTS        I
141300970117IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
141400970117IF  4C     VRAP(I)       IFEQ      RCTS(J)                                      *STESSA REGIONE
141500970117     C*
141600970117     C* CALCOLA IL RAGGRUPPAMENTO DELLA REGIONE
141700970117IF  5C     VRCT(I)       IFEQ      *BLANKS
141800970117     C                   MOVEL     '1'           $RB                            *RAGGRUP.NO CALCOLAR
141900970117E   5C                   ENDIF
142000970117IF  5C     VRCT(I)       IFEQ      'N'
142100970117     C                   MOVEL     '1'           $RN                            *RAGGRUP.NAZIONE
142200970117E   5C                   ENDIF
142300970117IF  5C     VRCT(I)       IFEQ      'R'
142400970117     C                   MOVEL     '1'           $RR                            *RAGGRUP.REGIONE
142500970117E   5C                   ENDIF
142600970117IF  5C     VRCT(I)       IFEQ      'T'
142700970117     C                   MOVEL     '1'           $RT                            *RAGGRUP.CALCOLARE
142800970117E   5C                   ENDIF
142900970117     C*
143000970117     C* CALCOLA LA %INCIDENZA
143100970128     C                   ADD       VPPE(I)       TOTPPE
143200970117E   4C                   ENDIF
143300970117E   3C                   ENDIF
143400970117E   2C                   ENDDO                                                  *FINE COD.TASS.ELEM.
143500970117     C*
143600970117     C* AGGIORNA IL RAGGRUPPAMENTO DELLA REGIONE
143700970117IF  2C     $RB           IFEQ      '1'
143800970117     C     $RN           ANDEQ     '0'
143900970117     C     $RR           ANDEQ     '0'
144000970117     C     $RT           ANDEQ     '0'
144100970117     C                   MOVEL     ' '           RRCT(J)                        *RAGGRUP.NO CALCOLAR
144200970128X   2C                   ELSE
144300970117IF  3C     $RN           IFEQ      '1'
144400970117     C     $RR           ANDEQ     '0'
144500970117     C     $RT           ANDEQ     '0'
144600970117     C     $RB           ANDEQ     '0'
144700970117     C                   MOVEL     'N'           RRCT(J)                        *RAGGRUP.NAZIONE
144800970117X   3C                   ELSE
144900970117IF  4C     $RR           IFEQ      '1'
145000970117     C     $RN           ANDEQ     '0'
145100970117     C     $RT           ANDEQ     '0'
145200970117     C     $RB           ANDEQ     '0'
145300970117     C                   MOVEL     'R'           RRCT(J)                        *RAGGRUP.REGIONE
145400970117X   4C                   ELSE
145500970117IF  5C     $RT           IFEQ      '1'
145600970117     C     $RN           ANDEQ     '0'
145700970117     C     $RR           ANDEQ     '0'
145800970117     C     $RB           ANDEQ     '0'
145900970117     C                   MOVEL     'T'           RRCT(J)                        *RAGGRUP.CALCOLARE
146000970117X   5C                   ELSE
146100970117     C                   MOVEL     'M'           RRCT(J)                        *RAGGRUPPAMENTO MIST
146200970117E   5C                   ENDIF
146300970117E   4C                   ENDIF
146400970117E   3C                   ENDIF
146500970117E   2C                   ENDIF
146600970124     C                   MOVEL     RRCT(J)       RHRC(J)                        *RAGGRUP.TO HIDDEN
146700970117     C*
146800970117     C* AGGIORNA LA %INCIDENZA
146900970117     C                   Z-ADD     TOTPPE        RPPE(J)                        *%INCIDENZA
147000970117E   1C                   ENDDO                                                  *FINE COD.TASS.REG.
147100970117     C*
147200970117     C                   ENDSR
147300970117     C*------------------------------------------------------------------------*
147400970117     C* IMPWD1 - IMPOSTAZIONI PRIMA FINESTRA
147500970117     C*------------------------------------------------------------------------*
147600970117     C     IMPWD1        BEGSR
147700970117     C*
147800970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
147900970117     C                   MOVEL     'N'           $FINV                          *FINE VIDEATA
148000970117     C                   CLEAR                   TE03W01
148100970117     C                   EXSR      SETWD1
148200970117     C*
148300970117     C                   ENDSR
148400970117     C*------------------------------------------------------------------------*
148500970117     C* SETWD1 - IMPOSTAZIONI INDICATORI PRIMA FINESTRA
148600970117     C*------------------------------------------------------------------------*
148700970117     C     SETWD1        BEGSR
148800970117     C*
148900970117     C                   SETOFF                                       252627     ERRORI
149000970117     C                   SETOFF                                       28
149100970117IF  1C     TIPVID        IFEQ      '1'
149200970117     C                   SETOFF                                       02        *SI VISUAL.RAGGRUP=M
149300970117X   1C                   ELSE
149400970117     C                   SETON                                        02        *NO VISUAL.RAGGRUP=M
149500970117E   1C                   ENDIF
149600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
149700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
149800970117     C*
149900970117     C                   ENDSR
150000970117     C*------------------------------------------------------------------------*
150100970117     C* GESWD1 - GESTIONE PRIMA FINESTRA
150200970117     C*------------------------------------------------------------------------*
150300970117     C     GESWD1        BEGSR
150400970117     C*
150500970117     C* EMISSIONE VIDEATA
150600970117     C                   EXFMT     TE03W01
150700970117     C                   EXSR      SETWD1
150800970117     C* F12-RITORNO
150900970117IF  1C     *INKL         IFEQ      *ON
151000970129     C                   MOVEL     *BLANKS       WRCT
151100970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
151200970117     C                   GOTO      FINWD1
151300970117E   1C                   ENDIF
151400970117     C* CONTROLLA VIDEO
151500970117     C                   EXSR      CTRWD1
151600970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
151700970117     C                   GOTO      FINWD1
151800970117E   1C                   ENDIF
151900970117     C* INVIO-SCELTA
152000970117     C                   MOVEL     WWRCT         WRCT
152100970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
152200970117     C*
152300970117     C     FINWD1        ENDSR
152400970117     C*------------------------------------------------------------------------*
152500970117     C* CTRWD1 - CONTROLLA PRIMA FINESTRA
152600970117     C*------------------------------------------------------------------------*
152700970117     C     CTRWD1        BEGSR
152800970117     C*
152900970117     C     *LIKE         DEFINE    V1WRCB        WWRCT
153000970117     C                   MOVEL     *BLANKS       WWRCT
153100970117     C*--------------------
153200970117     C* DA NON CALCOLARE
153300970117     C*--------------------
153400970117IF  1C     V1WRCB        IFNE      *BLANKS
153500970117     C     V1WRCB        ANDNE     '1'
153600970117     C                   SETON                                        2590
153700970117     C                   MOVEL     MSG(11)       $MSG
153800970117     C                   GOTO      FINCW1
153900970117X   1C                   ELSE
154000970117IF  2C     V1WRCB        IFEQ      '1'
154100970117     C                   MOVEL     ' '           WWRCT                          *SCELTA
154200970117     C                   GOTO      FINCW1
154300970117E   2C                   ENDIF
154400970117E   1C                   ENDIF
154500970117     C*--------------------
154600970117     C* DA CALCOLARE
154700970117     C*--------------------
154800970117IF  1C     V1WRCT        IFNE      *BLANKS
154900970117     C     V1WRCT        ANDNE     '1'
155000970117     C                   SETON                                        2690
155100970117     C                   MOVEL     MSG(11)       $MSG
155200970117     C                   GOTO      FINCW1
155300970117X   1C                   ELSE
155400970117IF  2C     V1WRCT        IFEQ      '1'
155500970117     C                   MOVEL     'T'           WWRCT                          *SCELTA
155600970117     C                   GOTO      FINCW1
155700970117E   2C                   ENDIF
155800970117E   1C                   ENDIF
155900970117     C*--------------------
156000970117     C* DA CALCOLARE NELLA REGIONE
156100970117     C*--------------------
156200970117IF  1C     V1WRCR        IFNE      *BLANKS
156300970117     C     V1WRCR        ANDNE     '1'
156400970117     C                   SETON                                        2790
156500970117     C                   MOVEL     MSG(11)       $MSG
156600970117     C                   GOTO      FINCW1
156700970117X   1C                   ELSE
156800970117IF  2C     V1WRCR        IFEQ      '1'
156900970117     C                   MOVEL     'R'           WWRCT                          *SCELTA
157000970117     C                   GOTO      FINCW1
157100970117E   2C                   ENDIF
157200970117E   1C                   ENDIF
157300970117     C*--------------------
157400970117     C* DA CALCOLARE NELLA NAZIONE
157500970117     C*--------------------
157600970117IF  1C     V1WRCN        IFNE      *BLANKS
157700970117     C     V1WRCN        ANDNE     '1'
157800970117     C                   SETON                                        2890
157900970117     C                   MOVEL     MSG(11)       $MSG
158000970117     C                   GOTO      FINCW1
158100970117X   1C                   ELSE
158200970117IF  2C     V1WRCN        IFEQ      '1'
158300970117     C                   MOVEL     'N'           WWRCT                          *SCELTA
158400970117     C                   GOTO      FINCW1
158500970117E   2C                   ENDIF
158600970117E   1C                   ENDIF
158700970117     C*
158800970117     C     FINCW1        ENDSR
158900970117     C*------------------------------------------------------------------------*
159000970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
159100970117     C*------------------------------------------------------------------------*
159200970117     C     IMPWD2        BEGSR
159300970117     C*
159400970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
159500970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
159600970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
159700980508     C                   MOVEL     D0DDSF        V2WDSF
159800980508     C                   MOVEL     D0DCTO        V2WCTO
159900970319     C                   Z-ADD     D0DKSC        V2WKSC
160000970319     C                   Z-ADD     D0DCTR        V2WCTR
160100970319     C                   Z-ADD     D0DPRG        V2WPRG
160200970319     C                   Z-ADD     D0DLNP        V2WLNP
160300011003     C                   MOVEL     D0DDIV        V2WDIV
160400970117     C                   EXSR      SETWD2
160500970117     C*
160600970117     C                   ENDSR
160700970117     C*------------------------------------------------------------------------*
160800970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
160900970117     C*------------------------------------------------------------------------*
161000970117     C     SETWD2        BEGSR
161100970117     C*
161200970117     C                   SETOFF                                       303132     ERRORI
161300980508     C                   SETOFF                                       333435
161400011003     C                   SETOFF                                       36
161500970117     C                   SETOFF                                       90         GENERICO DI ERRORE
161600970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
161700970117     C*
161800970117     C                   ENDSR
161900970117     C*------------------------------------------------------------------------*
162000970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
162100970117     C*------------------------------------------------------------------------*
162200970117     C     GESWD2        BEGSR
162300970117     C*
162400970117     C* EMISSIONE VIDEATA
162500970117     C                   EXFMT     TE03W02
162600970117     C                   EXSR      SETWD2
162700970317     C* F12 ABILITATO-RITORNO
162800970117IF  1C     *INKL         IFEQ      *ON
162900970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
163000970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
163100970117     C                   GOTO      FINWD2
163200970117E   1C                   ENDIF
163300980508     C* F11-RICERCA
163400980508IF  1C     *INKK         IFEQ      *ON
163500980508     C                   EXSR      GESRIC
163600980508     C                   GOTO      FINWD2
163700980508E   1C                   ENDIF
163800970117     C* CONTROLLA VIDEO
163900970117     C                   EXSR      CTRWD2
164000970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
164100970117     C                   GOTO      FINWD2
164200970117E   1C                   ENDIF
164300970117     C* F06-CONFERMA
164400970117IF  1C     *INKF         IFEQ      *ON
164500970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
164600970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
164700970117     C                   GOTO      FINWD2
164800970117E   1C                   ENDIF
164900970117     C*
165000970117     C     FINWD2        ENDSR
165100980508     C*------------------------------------------------------------------------*
165200980508     C* GESRIC - GESTIONE RICERCA DATI TIPO
165300980508     C*------------------------------------------------------------------------*
165400980508     C     GESRIC        BEGSR
165500980508     C*
165600980508     C                   CLEAR                   DSTE30
165700980508     C                   MOVEL     'C01'         D30OP0
165800980708     C                   MOVEL     'T'           D30ETX
165900980804     C                   Z-ADD     D00KSC        D30KSC
166000980904     C                   MOVEL     D00FIE        D30TFI
166100040901     C                   MOVEL     D00dpd        D30dpi
166200011011     C                   MOVEL     D00DIV        D30DIV
166300980508     C                   CALL      'TNTE32R'
166400980508     C                   PARM                    KPJBA
166500980508     C                   PARM                    DSTE30
166600980508IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
166700980508     C                   SETON                                        90
166800980508     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
166900980508X   1C                   ELSE                                                   *NO ERRORE
167000980508IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
167100980508     C     D30F12        OREQ      '1'
167200980508X   2C                   ELSE                                                   *SCELTA
167300980508     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
167400980508     C                   MOVEL     D30DSF        V2WDSF                         *CARICA CODICE
167500980508     C                   MOVEL     D30CTO        V2WCTO
167600980508     C                   Z-ADD     D30KSC        V2WKSC
167700980508     C                   Z-ADD     D30CTR        V2WCTR
167800980508     C                   Z-ADD     D30PRG        V2WPRG
167900980508     C                   Z-ADD     D30LNP        V2WLNP
168000011003     C                   MOVEL     D30DIV        V2WDIV
168100980508E   2C                   ENDIF
168200980508E   1C                   ENDIF
168300980508     C*
168400980508     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
168500980508     C*
168600980508     C                   ENDSR
168700970117     C*------------------------------------------------------------------------*
168800970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
168900970117     C*------------------------------------------------------------------------*
169000970117     C     CTRWD2        BEGSR
169100980508     C*--------------------
169200980508     C* DATI SEDE O FILIALE
169300980508     C*--------------------
169400980508IF  1C     V2WDSF        IFEQ      'S'
169500980508     C     V2WDSF        OREQ      'F'
169600980508X   1C                   ELSE
169700980508     C                   SETON                                        35
169800980508     C                   SETON                                        90
169900980508     C                   MOVEL     MSG(24)       $MSG
170000980508     C                   GOTO      FINCW2
170100980508E   1C                   ENDIF
170200970117     C*--------------------
170300970117     C* TARIFFA O OFFERTA
170400970117     C*--------------------
170500970117IF  1C     V2WCTO        IFEQ      'T'
170600970117     C     V2WCTO        OREQ      'O'
170700970117X   1C                   ELSE
170800970117     C                   SETON                                        3390
170900970117     C                   MOVEL     MSG(16)       $MSG
171000970117     C                   GOTO      FINCW2
171100970117E   1C                   ENDIF
171200970117     C*--------------------
171300970117     C* LINEA DI PARTENZA
171400970117     C*--------------------
171500970117     C                   MOVEL     V2WLNP        WLNP
171600970117     C                   MOVEL     *BLANKS       WDLNP
171700971204     C                   MOVEL     *BLANKS       WFL1
171800970117     C                   MOVEL     '0'           WERR
171900970117     C                   EXSR      CHKLNP
172000980818IF  1C     WERR          IFEQ      '1'
172100980818IF  2C     WLNP          IFEQ      888
172200980818     C     WLNP          OREQ      889
172300980818     C                   MOVEL     '0'           WERR
172400980818     C                   MOVEL     DDEC(10)      WDLNP
172500980818IF  3C     WLNP          IFEQ      888
172600980818     C                   MOVEL     *BLANKS       WFL1
172700980818E   3C                   ENDIF
172800980818IF  3C     WLNP          IFEQ      889
172900980818     C                   MOVEL     'E'           WFL1
173000980818E   3C                   ENDIF
173100980818X   2C                   ELSE
173200011003     C                   SETON                                        343690
173300970117     C                   MOVEL     MSG(15)       $MSG
173400970117     C                   GOTO      FINCW2
173500980818E   2C                   ENDIF
173600980818E   1C                   ENDIF
173700970117     C*
173800970117IF  1C     D00FIE        IFEQ      'I'
173900980114IF  2C     WFL1          IFNE      *BLANKS
174000011003     C                   SETON                                        343690
174100970117     C                   MOVEL     MSG(17)       $MSG
174200970117     C                   GOTO      FINCW2
174300980114E   2C                   ENDIF
174400980114X   1C                   ELSE
174500980114IF  2C     DLPFL1        IFEQ      *BLANKS
174600971204     C     WFL1          ANDNE     *BLANKS
174700011003     C                   SETON                                        343690
174800970117     C                   MOVEL     MSG(18)       $MSG
174900970117     C                   GOTO      FINCW2
175000980114E   2C                   ENDIF
175100980114IF  2C     DLPFL1        IFNE      *BLANKS
175200971204     C     WFL1          ANDEQ     *BLANKS
175300011003     C                   SETON                                        343690
175400970117     C                   MOVEL     MSG(19)       $MSG
175500970117     C                   GOTO      FINCW2
175600980114E   2C                   ENDIF
175700970117E   1C                   ENDIF
175800970117     C*--------------------
175900970117     C* DATI TIPO ESISTENTI
176000970117     C*--------------------
176100980508     C                   MOVEL     V2WDSF        KTTDSF                         *DATI SEDE O FILIALE
176200980508     C                   MOVEL     V2WCTO        KTTCTO                         *TARIFFA O OFFERTA
176300970117     C                   Z-ADD     V2WKSC        KTTKSC                         *CLIENTE O VISITA
176400970117     C                   Z-ADD     V2WCTR        KTTCTR                         *TARIFFA
176500970117     C                   Z-ADD     V2WPRG        KTTPRG                         *PROGRESSIVO
176600970117     C                   Z-ADD     V2WLNP        KTTLNP                         *LINEA DI PARTENZA
176700970117     C                   MOVEL     D00FIE        KTTFIE                         *FLAG ITALIA/ESTERO
176800040831     C                   MOVEL     D00dpd        KTTdpd                         *FLAG ITALIA/ESTERO
176900970619     C                   MOVEL     'N'           WEETT                          *ESISTENZA CTS
177000011001     C     KE2ETT        SETLL     TEETT01L
177100011001     C     KE2ETT        READE     TEETT01L                               99
177200970619DO  1C     *IN99         DOWEQ     *OFF
177300970619IF  2C     ETTATB        IFEQ      *BLANKS
177400980508     C                   MOVEL     'S'           WEETT                          *ESISTENZA CTS
177500970619E   2C                   ENDIF
177600011001     C     KE2ETT        READE     TEETT01L                               99
177700980508E   1C                   ENDDO                                                  *FINE CTS
177800970619IF  1C     WEETT         IFEQ      'N'                                          *NON TROVATI
177900970117     C                   SETON                                        303132
178000980508     C                   SETON                                        333435
178100011003     C                   SETON                                        36
178200980508     C                   SETON                                        90
178300970117     C                   MOVEL     MSG(13)       $MSG
178400970117E   1C                   ENDIF
178500970117     C*
178600970117     C     FINCW2        ENDSR
178700970117     C*------------------------------------------------------------------------*
178800970117     C* IMPWD3 - IMPOSTAZIONI TERZA FINESTRA
178900970117     C*------------------------------------------------------------------------*
179000970117     C     IMPWD3        BEGSR
179100970117     C*
179200970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
179300970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
179400970318     C                   MOVEL     'NO'          V3WRIS
179500970117     C                   EXSR      SETWD3
179600970117     C*
179700970117     C                   ENDSR
179800970117     C*------------------------------------------------------------------------*
179900970117     C* SETWD3 - IMPOSTAZIONI INDICATORI TERZA FINESTRA
180000970117     C*------------------------------------------------------------------------*
180100970117     C     SETWD3        BEGSR
180200970117     C*
180300970117     C                   SETOFF                                       40         ERRORI
180400970117     C                   SETOFF                                       90         GENERICO DI ERRORE
180500970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
180600970117     C*
180700970117     C                   ENDSR
180800970117     C*------------------------------------------------------------------------*
180900970117     C* GESWD3 - GESTIONE TERZA FINESTRA
181000970117     C*------------------------------------------------------------------------*
181100970117     C     GESWD3        BEGSR
181200970117     C*
181300970117     C* EMISSIONE VIDEATA
181400970117     C                   EXFMT     TE03W03
181500970117     C                   EXSR      SETWD3
181600970117     C* F12-RITORNO
181700970117IF  1C     *INKL         IFEQ      *ON
181800970314     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
181900970318     C                   MOVEL     'NO'          V3WRIS
182000970117     C                   GOTO      FINWD3
182100970117E   1C                   ENDIF
182200970117     C* CONTROLLA VIDEO
182300970117     C                   EXSR      CTRWD3
182400970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
182500970117     C                   GOTO      FINWD3
182600970117E   1C                   ENDIF
182700970117     C* INVIO-CONFERMA
182800970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
182900970117     C*
183000970117     C     FINWD3        ENDSR
183100970117     C*------------------------------------------------------------------------*
183200970117     C* CTRWD3 - CONTROLLA TERZA FINESTRA
183300970117     C*------------------------------------------------------------------------*
183400970117     C     CTRWD3        BEGSR
183500970117     C*--------------------
183600970117     C* CONFERMA SCRITTURA OFFERTE
183700970117     C*--------------------
183800970117IF  1C     V3WRIS        IFNE      'SI'
183900970117     C     V3WRIS        ANDNE     'NO'
184000970117     C                   SETON                                        4090
184100970117     C                   MOVEL     MSG(12)       $MSG
184200970117     C                   GOTO      FINCW3
184300970117E   1C                   ENDIF
184400970117     C*
184500970117     C     FINCW3        ENDSR
184600970117     C*------------------------------------------------------------------------*
184700970117     C* POSSF1 - CARICA IL PRIMO SUBFILE
184800970117     C*------------------------------------------------------------------------*
184900970117     C     POSSF1        BEGSR
185000970117     C*
185100970117     C* ELIMINA IL CONTENUTO DEL SFILE
185200970117     C                   EXSR      CLRSF1
185300970117     C*
185400970117     C* SI POSIZIONA SUL RECORD DA CARICARE
185500970117     C                   EXSR      SETFI1
185600970117     C* CARICA IL SUBFILE
185700970117     C                   Z-ADD     *ZEROS        NR1               4 0          *DALL'INIZIO
185800970117     C                   EXSR      CARSF1
185900970117     C*
186000970117     C                   ENDSR
186100970117     C*------------------------------------------------------------------------*
186200970117     C* CLRSF1 - ELIMINA IL CONTENUTO DEL PRIMO SUBFILE
186300970117     C*------------------------------------------------------------------------*
186400970117     C     CLRSF1        BEGSR
186500970117     C*
186600970117     C                   Z-ADD     1             $NR1
186700970117     C                   SETOFF                                       71        *NO SFLDSP
186800970117     C                   SETOFF                                       72        *NO DSPCTL/SI SFLCLR
186900970117     C                   WRITE     TE03C01
187000970117     C                   SETON                                        71        *SI SFLDSP
187100970117     C                   SETON                                        72        *SI DSPCTL/NO SFLCLR
187200970117     C*
187300970117     C                   ENDSR
187400970117     C*------------------------------------------------------------------------*
187500970117     C* SETFI1 - POSIZIONAMENTO RECORD -PRIMO SUBFILE-
187600970117     C*------------------------------------------------------------------------*
187700970117     C     SETFI1        BEGSR
187800970117     C*
187900970117     C                   Z-ADD     *ZEROS        SF1EOF                         *0=NO FINE/1=FINE
188000970117     C                   Z-ADD     *ZEROS        S                 4 0          *INDICE SCHIERE VIDE
188100970117     C*
188200970117     C                   ENDSR
188300970117     C*------------------------------------------------------------------------*
188400970117     C* CARSF1 - CARICA TUTTO IL PRIMO SUBFILE
188500970117     C*------------------------------------------------------------------------*
188600970117     C     CARSF1        BEGSR
188700970117     C*
188800970117DO  1C                   DO        WURCTS
188900970117     C                   EXSR      LETFI1                                       *LEGGE RECORD
189000970117IF  2C     SF1EOF        IFEQ      *ZEROS
189100970117     C                   ADD       1             NR1
189200970117     C                   EXSR      IRESF1                                       *IMPOSTA RECORD SFIL
189300970117     C                   WRITE     TE03S01                                      *SCRIVE RECORD SFILE
189400970117E   2C                   ENDIF
189500970117E   1C                   ENDDO
189600970117     C*
189700970117     C                   ENDSR
189800970117     C*------------------------------------------------------------------------*
189900970117     C* LETFI1 - LETTURA SUCESSIVA RECORD -PRIMO SUBFILE-
190000970117     C*------------------------------------------------------------------------*
190100970117     C     LETFI1        BEGSR
190200970117     C*
190300970117     C                   ADD       1             S                              *INDICE SCHIERE VIDE
190400970117     C*
190500970117     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
190600970117IF  1C     S             IFGT      WURCTS                                       *AMPIEZZA MASSIMA
190700970117     C     RCTS(S)       OREQ      *BLANKS                                      *SCHIERA VUOTA
190800970117     C                   Z-ADD     1             SF1EOF
190900970117E   1C                   ENDIF
191000970117     C*
191100970117     C                   ENDSR
191200970117     C*------------------------------------------------------------------------*
191300970117     C* IRESF1 - IMPOSTA CAMPI RECORD PRIMO SUBFILE
191400970117     C*------------------------------------------------------------------------*
191500970117     C     IRESF1        BEGSR
191600970117     C* CAMPI
191700970124     C                   MOVEL     RHRC(S)       V1HRCT                         *HIDDEN
191800970117     C                   MOVEL     RCTS(S)       V1SCTS
191900970117     C                   Z-ADD     RPPE(S)       V1SPPE
192000970117     C                   MOVEL     RRCT(S)       V1SRCT
192100970117     C* DECODIFICHE
192200970117     C                   SETOFF                                           98
192300970117     C                   Z-ADD     1             I
192400970117     C     RCTS(S)       LOOKUP    CTS(I)                                 98
192500970117     C                   MOVEL     DCT(I)        V1SDCT
192600970117     C                   MOVEL     V1SCTS        WCTS
192700970117     C                   MOVEL     V1SRCT        WRCT
192800970117     C                   MOVEL     *BLANKS       WDRCT
192900970117     C                   EXSR      DECRCT
193000970117     C                   MOVEL     WDRCT         V1SDRC                         *DESCR.RAGGRUPPAMENT
193100970117     C*
193200970128     C                   CLEAR                   SCH
193300970128     C                   Z-ADD     *ZEROS        J
193400970128DO  1C     1             DO        WUVCTS        I
193500970128IF  2C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
193600970128IF  3C     VRAP(I)       IFEQ      RCTS(S)                                      *STESSA REGIONE
193700970128     C                   ADD       1             J
193800970128     C                   MOVEL     VRCT(I)       SCH(J)
193900970128IF  4C     SCH(J)        IFEQ      *BLANKS
194000970128     C                   MOVEL     '*'           SCH(J)
194100970128E   4C                   ENDIF
194200970128E   3C                   ENDIF
194300970128E   2C                   ENDIF
194400970128E   1C                   ENDDO
194500970128     C                   MOVEA     SCH           RSCH(S)
194600970128     C                   MOVEL     RSCH(S)       V1SCTE                         *TUTTI RAGGR COD.TAS
194700970203     C* INDICATORI
194800970203     C                   SETOFF                                       70        *NO SFLNXTCHG
194900970203IF  1C     $TGES         IFEQ      'I'
195000970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
195100970203X   1C                   ELSE
195200970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
195300970203E   1C                   ENDIF
195400970203IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
195500970203     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
195600970203X   1C                   ELSE                                                   *DA PORTO
195700970203     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
195800970203E   1C                   ENDIF
195900970203     C*
196000970117     C                   ENDSR
196100970117     C*------------------------------------------------------------------------*
196200970117     C* POSSF2 - CARICA IL SECONDO SUBFILE
196300970117     C*------------------------------------------------------------------------*
196400970117     C     POSSF2        BEGSR
196500970117     C*
196600970117     C* ELIMINA IL CONTENUTO DEL SFILE
196700970117     C                   EXSR      CLRSF2
196800970117     C*
196900970117     C* SI POSIZIONA SUL TERZO RECORD DA CARICARE
197000970117     C                   EXSR      SETFI2
197100970117     C* CARICA IL SUBFILE
197200970117     C                   Z-ADD     *ZEROS        NR2               4 0          *DALL'INIZIO
197300970117     C                   EXSR      CARSF2
197400970117     C*
197500970117     C                   ENDSR
197600970117     C*------------------------------------------------------------------------*
197700970117     C* CLRSF2 - ELIMINA IL CONTENUTO DEL SECONDO SUBFILE
197800970117     C*------------------------------------------------------------------------*
197900980508     C     CLRSF2        BEGSR
198000970117     C*
198100970117     C                   Z-ADD     1             $NR2
198200970117     C                   SETOFF                                       91        *NO SFLDSP
198300970117     C                   SETOFF                                       92        *NO DSPCTL/SI SFLCLR
198400970117     C                   WRITE     TE03C02
198500970117     C                   SETON                                        91        *SI SFLDSP
198600970117     C                   SETON                                        92        *SI DSPCTL/NO SFLCLR
198700970117     C*
198800970117     C                   ENDSR
198900970117     C*------------------------------------------------------------------------*
199000970117     C* SETFI2 - POSIZIONAMENTO RECORD -SECONDO SUBFILE-
199100970117     C*------------------------------------------------------------------------*
199200970117     C     SETFI2        BEGSR
199300970117     C*
199400970117     C                   Z-ADD     *ZEROS        SF2EOF                         *0=NO FINE/1=FINE
199500970117     C                   Z-ADD     *ZEROS        O                 4 0          *INDICE SCHIERE VIDE
199600970117     C*
199700970117     C                   ENDSR
199800970117     C*------------------------------------------------------------------------*
199900970117     C* CARSF2 - CARICA TUTTO IL SECONDO SUBFILE
200000970117     C*------------------------------------------------------------------------*
200100970117     C     CARSF2        BEGSR
200200970117     C*
200300970117DO  1C     1             DO        WUOCTS
200400970117     C                   EXSR      LETFI2                                       *LEGGE RECORD
200500970117IF  2C     SF2EOF        IFEQ      *ZEROS
200600970117     C                   ADD       1             NR2
200700970117     C                   EXSR      IRESF2                                       *IMPOSTA RECORD SFIL
200800970117     C                   WRITE     TE03S02                                      *SCRIVE RECORD SFILE
200900970117E   2C                   ENDIF
201000970117E   1C                   ENDDO
201100970117     C*
201200970117     C                   ENDSR
201300970117     C*------------------------------------------------------------------------*
201400970117     C* LETFI2 - LETTURA SUCESSIVA RECORD -SECONDO SUBFILE-
201500970117     C*------------------------------------------------------------------------*
201600970117     C     LETFI2        BEGSR
201700970117     C*
201800970117     C                   ADD       1             O                              *INDICE SCHIERE VIDE
201900970117     C*
202000970117     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
202100970117IF  1C     O             IFGT      WUOCTS                                       *AMPIEZZA MASSIMA
202200970117     C     OCTS(O)       OREQ      *BLANKS                                      *SCHIERA VUOTA
202300970117     C                   Z-ADD     1             SF2EOF
202400970117E   1C                   ENDIF
202500970117     C*
202600970117     C                   ENDSR
202700970117     C*------------------------------------------------------------------------*
202800970117     C* IRESF2 - IMPOSTA CAMPI RECORD SECONDO SUBFILE
202900970117     C*------------------------------------------------------------------------*
203000970117     C     IRESF2        BEGSR
203100970117     C*
203200970117     C                   MOVEL     V1CPPE        V2CPPE                         *CONTROL
203300970117     C                   MOVEL     OCTS(O)       V2SCTS                         *SFILE
203400970117     C                   Z-ADD     OPPE(O)       V2SPPE
203500970117     C* DECODIFICHE
203600970117     C                   SETOFF                                           98
203700970117     C                   Z-ADD     1             I
203800970117     C     OCTS(O)       LOOKUP    CTS(I)                                 98
203900970117     C                   MOVEL     DCT(I)        V2SDCT
204000970117     C*
204100970117     C                   ENDSR
204200970117     C*------------------------------------------------------------------------*
204300970117     C* POSSF3 - CARICA IL TERZO SUBFILE
204400970117     C*------------------------------------------------------------------------*
204500970117     C     POSSF3        BEGSR
204600970117     C*
204700970117     C* ELIMINA IL CONTENUTO DEL SFILE
204800970117     C                   EXSR      CLRSF3
204900970117     C*
205000970117     C* SI POSIZIONA SUL RECORD DA CARICARE
205100970117     C                   EXSR      SETFI3
205200970117     C* CARICA IL SUBFILE
205300970117     C                   Z-ADD     *ZEROS        NR3               4 0          *DALL'INIZIO
205400970117     C                   EXSR      CARSF3
205500970117     C*
205600970117     C                   ENDSR
205700970117     C*------------------------------------------------------------------------*
205800970117     C* CLRSF3 - ELIMINA IL CONTENUTO DEL TERZO SUBFILE
205900970117     C*------------------------------------------------------------------------*
206000970117     C     CLRSF3        BEGSR
206100970117     C*
206200970117     C                   Z-ADD     1             $NR3
206300970117     C                   SETOFF                                       81        *NO SFLDSP
206400970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
206500970117     C                   WRITE     TE03C03
206600970117     C                   SETON                                        81        *SI SFLDSP
206700970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
206800970117     C*
206900970117     C                   ENDSR
207000970117     C*------------------------------------------------------------------------*
207100970117     C* SETFI3 - POSIZIONAMENTO RECORD -TERZO SUBFILE-
207200970117     C*------------------------------------------------------------------------*
207300970117     C     SETFI3        BEGSR
207400970117     C*
207500970117     C                   Z-ADD     *ZEROS        SF3EOF                         *0=NO FINE/1=FINE
207600970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
207700970117     C*
207800970117     C                   ENDSR
207900970117     C*------------------------------------------------------------------------*
208000970117     C* CARSF3 - CARICA TUTTO IL SECONDO SUBFILE
208100970117     C*------------------------------------------------------------------------*
208200970117     C     CARSF3        BEGSR
208300970117     C*
208400970117DO  1C                   DO        WUVCTS
208500970117     C                   EXSR      LETFI3                                       *LEGGE RECORD
208600970117IF  2C     SF3EOF        IFEQ      *ZEROS
208700970117     C                   ADD       1             NR3
208800970117     C                   EXSR      IRESF3                                       *IMPOSTA RECORD SFIL
208900970117     C                   WRITE     TE03S03                                      *SCRIVE RECORD SFILE
209000970117E   2C                   ENDIF
209100970117E   1C                   ENDDO
209200970117     C*
209300970117     C                   ENDSR
209400970117     C*------------------------------------------------------------------------*
209500970117     C* LETFI3 - LETTURA SUCESSIVA RECORD -TERZO SUBFILE-
209600970117     C*------------------------------------------------------------------------*
209700970117     C     LETFI3        BEGSR
209800970117     C*
209900970117     C* LEGGE FINO A:
210000970117     C*  - FINE FILE
210100970117     C*  - TROVATO RECORD VALIDO
210200970117     C                   MOVEL     'N'           WRECOK
210300970117DO  1C     SF3EOF        DOWEQ     *ZEROS
210400970117     C     WRECOK        ANDEQ     'N'
210500970117     C*
210600970117     C                   ADD       1             V
210700970117IF  2C     V             IFGT      WUVCTS                                       *AMPIEZZA MASSIMA
210800970117     C     VCTS(V)       OREQ      *BLANKS                                      *SCHIERA VUOTA
210900970117     C                   Z-ADD     1             SF3EOF
211000970117X   2C                   ELSE
211100970117     C                   EXSR      CHKSF3                                       *CONTROLLA RECORD
211200970117E   2C                   ENDIF
211300970117E   1C                   ENDDO
211400970117     C*
211500970117     C                   ENDSR
211600970117     C*------------------------------------------------------------------------*
211700970117     C* CHKSF3 - CONTROLLA RECORD DEL TERZO SUBFILE DA EMETTERE
211800970117     C*------------------------------------------------------------------------*
211900970117     C     CHKSF3        BEGSR
212000970117     C*
212100970117     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
212200970117     C*
212300970117     C* CODICE TASSAZIONE NON DELLA REGIONE, ESCLUDE
212400970117IF  1C     VRAP(V)       IFNE      V3CRAP
212500970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
212600970117E   1C                   ENDIF
212700970117     C*
212800970117     C                   ENDSR
212900970117     C*------------------------------------------------------------------------*
213000970117     C* IRESF3 - IMPOSTA CAMPI RECORD TERZO SUBFILE
213100970117     C*------------------------------------------------------------------------*
213200970117     C     IRESF3        BEGSR
213300970117     C* CAMPI
213400970117     C                   MOVEL     VCTS(V)       V3SCTS
213500970117     C                   Z-ADD     VPPE(V)       V3SPPE
213600970117     C                   MOVEL     VRCT(V)       V3SRCT
213700970128     C                   ADD       VPPE(V)       V3CPPE
213800970117     C* DECODIFICHE
213900970117     C                   SETOFF                                           98
214000970117     C                   Z-ADD     1             I
214100970117     C     VCTS(V)       LOOKUP    CTS(I)                                 98
214200970117     C                   MOVEL     DCT(I)        V3SDCT
214300970117     C                   MOVEL     FCR(I)        V3SFCR
214400970117     C                   MOVEL     V3SCTS        WCTS
214500970117     C                   MOVEL     V3SRCT        WRCT
214600970117     C                   MOVEL     *BLANKS       WDRCT
214700970117     C                   EXSR      DECRCT
214800970117     C                   MOVEL     WDRCT         V3SDRC                         *DESCR.RAGGRUPPAMENT
214900970203     C* INDICATORI
215000970203     C                   SETOFF                                       80        *NO SFLNXTCHG
215100970203IF  1C     $TGES         IFEQ      'I'
215200970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
215300970203X   1C                   ELSE
215400970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
215500970203E   1C                   ENDIF
215600970203IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
215700970203     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
215800970203X   1C                   ELSE
215900970203     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
216000970203E   1C                   ENDIF
216100970203     C*
216200970117     C                   ENDSR
216300970117     C*------------------------------------------------------------------------*
216400970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
216500970117     C*------------------------------------------------------------------------*
216600970117     C     CHKLNP        BEGSR
216700970117     C*
216800020729     C     *LIKE         DEFINE    §NTWFIE       WFL1
216900970117     C     *LIKE         DEFINE    V0DLNP        WDLNP
217000970117     C*
217100970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
217200970117     C                   MOVEL     WLNP          KORFIL
217300970117     C     KEYORG        CHAIN     AZORG01L                           99
217400970117IF  1C     *IN99         IFEQ      *OFF
217500970117     C     ORGFVA        ANDEQ     *BLANKS
217600970117     C     ORGFAG        ANDNE     'V'
217700970117     C                   MOVEL     ORGDES        WDLNP
217800020729     C                   MOVEL     ORGDE3        OG143
217900020729     C*
218000020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
218100020729     C                   CLEAR                   DNTW
218200020729     C*
218300020729     C                   MOVEL(P)  'NTW'         KTECOD
218400020729     C                   MOVEL(P)  §OGNTW        KTEKE1
218500020729     C     KEYTBE        CHAIN     TNTBE01L
218600020729     C                   IF        %found(TNTBE01L)
218700020729     C                   MOVEL     TBEUNI        DNTW
218800020729     C                   ENDIF
218900020729     C*
219000020729     C                   MOVEL     §NTWFIE       WFL1
219100970117X   1C                   ELSE
219200970117     C                   MOVEL     '1'           WERR
219300970117E   1C                   ENDIF
219400970117     C*
219500970117     C                   ENDSR
219600970117     C*------------------------------------------------------------------------*
219700970117     C* CHKRCT - CONTROLLA IL RAGGRUPPAMENTO CODICE TASSAZIONE
219800970117     C*------------------------------------------------------------------------*
219900970117     C     CHKRCT        BEGSR
220000970117     C*
220100970117     C     *LIKE         DEFINE    V1SRCT        WRCT
220200970117     C     *LIKE         DEFINE    V1SDRC        WDRCT
220300970117     C*
220400970117     C* RICHIESTO ELENCO CODICI
220500970117     C     '?'           SCAN      WRCT                                   98
220600970117IF  1C     *IN98         IFEQ      *ON
220700970117     C* GESTIONE VIDEATA
220800970117     C                   EXSR      IMPWD1
220900970117DO  2C     $FINV         DOUEQ     'S'
221000970117     C                   EXSR      GESWD1                                       *GESTIONE FINESTRA
221100970117E   2C                   ENDDO
221200970117E   1C                   ENDIF
221300970117     C*
221400970117     C* CONTROLLA ESATTEZZA CODICE
221500970117IF  1C     WRCT          IFNE      ' '
221600970117     C     WRCT          ANDNE     'T'
221700970117     C     WRCT          ANDNE     'R'
221800970117     C     WRCT          ANDNE     'N'
221900970117     C     WRCT          ANDNE     'M'
222000970117     C                   MOVEL     '1'           WERR
222100970117X   1C                   ELSE
222200970117     C                   EXSR      DECRCT                                       *DECODIFICA RAGGRUP.
222300970117IF  2C     WRCT          IFEQ      'M'                                          *SITUAZIONE "MISTA"
222400970117     C     WRCT          ANDNE     V1HRCT                                       *NON DA PRIMA
222500970117     C                   MOVEL     '1'           WERR
222600970117E   2C                   ENDIF
222700970117E   1C                   ENDIF
222800970117     C*
222900970117     C                   ENDSR
223000970117     C*------------------------------------------------------------------------*
223100970117     C* DECRCT - DECODIFICA IL RAGGRUPPAMENTO CODICE TASSAZIONE
223200970117     C*------------------------------------------------------------------------*
223300970117     C     DECRCT        BEGSR
223400970117     C*
223500970117IF  1C     WRCT          IFEQ      ' '                                          *NON CALCOLARE
223600970117     C                   MOVEL     DDEC(3)       WDRCT                          *DESCR.RAGGRUPPAMENT
223700970117E   1C                   ENDIF
223800970117IF  1C     WRCT          IFEQ      'T'                                          *CALCOLARE
223900970117     C                   MOVEL     DDEC(4)       WDRCT                          *DESCR.RAGGRUPPAMENT
224000970117E   1C                   ENDIF
224100970117IF  1C     WRCT          IFEQ      'R'                                          *CALCOLARE IN REGION
224200970117     C     WRCT          OREQ      'N'                                          *CALCOLARE IN NAZION
224300970117     C                   MOVEL     DDEC(5)       DDEC05
224400970117     C                   SETOFF                                           98
224500970117     C                   Z-ADD     1             I                 4 0
224600970117     C     WCTS          LOOKUP    CTS(I)                                 98
224700970117     C                   SETOFF                                           98
224800970117     C                   Z-ADD     1             J                 4 0
224900970117IF  2C     WRCT          IFEQ      'R'
225000970117     C     RAP(I)        LOOKUP    CTS(J)                                 98
225100970117X   2C                   ELSE
225200970117     C     NAZ(I)        LOOKUP    CTS(J)                                 98
225300970117E   2C                   ENDIF
225400970117     C                   MOVEL     DCT(J)        D05RCT
225500970117     C                   MOVEL     DDEC05        WDRCT                          *DESCR.RAGGRUPPAMENT
225600970117E   1C                   ENDIF
225700970117IF  1C     WRCT          IFEQ      'M'                                          *SITUAZIONE MISTA
225800970117     C                   MOVEL     DDEC(6)       WDRCT                          *DESCR.RAGGRUPPAMENT
225900970117E   1C                   ENDIF
226000970117     C*
226100970117     C                   ENDSR
226200970117     C*------------------------------------------------------------------------*
226300970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
226400970117     C*------------------------------------------------------------------------*
226500970117     C     CARCC         BEGSR
226600970117     C*
226700970117     C* RECUPERA I DATI DELL'UTENTE
226800020906     C                   Z-ADD     1             CODUT
226900020830     C                   CALL      'X§PARUT'
227000970117     C                   PARM                    PARUT
227100970117     C*
227200970117     C                   Z-ADD     1             Z
227300970117DO  1C                   DO        50            X2                2 0
227400970117     C                   MOVE      TCU(X2)       CARCNT
227500970117IF  2C     TIPCNT        IFEQ      'CI'
227600970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
227700970117E   2C                   ENDIF
227800970117IF  2C     FL34CN        IFEQ      '01'
227900970117     C     TIPCNT        ANDEQ     'CG'
228000970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
228100970117E   2C                   ENDIF
228200970117IF  2C     FL34CN        IFEQ      '02'
228300970117     C     TIPCNT        ANDEQ     'CG'
228400970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
228500970117E   2C                   ENDIF
228600970117IF  2C     TIPCNT        IFEQ      'RI'
228700970117     C                   Z-ADD     KCU(X2)       RI(Z)
228800970117     C                   ADD       1             Z                 3 0
228900970117E   2C                   ENDIF
229000970117E   1C                   ENDDO
229100970117     C*
229200970117     C                   ENDSR
229300970117     C*------------------------------------------------------------------------*
229400970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
229500970117     C*------------------------------------------------------------------------*
229600970117     C     CARTAB        BEGSR
229700970117     C*--------------------
229800970117     C* TIPI SERVIZIO
229900970117     C*--------------------
230000970117     C                   Z-ADD     *ZEROS        I                 4 0
230100020906     C                   Z-ADD     1             KTBKUT
230200970117     C                   MOVEL     '5E'          KTBCOD
230300970117     C     KE2TAB        SETLL     TABEL00F
230400970117     C     KE2TAB        READE     TABEL00F                               99
230500970117DO  1C     *IN99         DOWEQ     *OFF
230600970117IF  2C     TBLFLG        IFEQ      *BLANKS
230700970117     C                   ADD       1             I
230800970117     C                   MOVEL     TBLUNI        DS5E
230900970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
231000970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
231100970117E   2C                   ENDIF
231200970117     C     KE2TAB        READE     TABEL00F                               99
231300970117E   1C                   ENDDO
231400970117     C*--------------------
231500970117     C* TIPI TARIFFA
231600970117     C*--------------------
231700970117     C                   Z-ADD     *ZEROS        I                 4 0
231800020906     C                   Z-ADD     1             KTBKUT
231900970117     C                   MOVEL     'TR'          KTBCOD
232000970117     C     KE2TAB        SETLL     TABEL00F
232100970117     C     KE2TAB        READE     TABEL00F                               99
232200970117DO  1C     *IN99         DOWEQ     *OFF
232300970117IF  2C     TBLFLG        IFEQ      *BLANKS
232400970117     C                   MOVEL     TBLUNI        DSTR
232500970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
232600970117     C                   ADD       1             I
232700970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
232800040830     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
232900970117E   3C                   ENDIF
233000970117E   2C                   ENDIF
233100970117     C     KE2TAB        READE     TABEL00F                               99
233200970117E   1C                   ENDDO
233300970117     C*--------------------
233400970117     C* TARIFFA DI CARTELLO
233500970117     C*--------------------
233600970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
233700970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
233800970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
233900021202     C                   Z-ADD     *ZEROS        CARKSC
234000021202     C                   Z-ADD     *ZEROS        CARCTR
234100021202     C                   Z-ADD     *ZEROS        CARPRG
234200021202     C                   MOVEL     *BLANKS       CARDIV
234300021202     C                   CLEAR                   TRUL27DS
234400040901     C                   EVAL      I27TLn = d00dpd
234500040901     C                   EVAL      I27TLA = 'L'
234600021202     C                   EVAL      I27DTA = DATCOR
234700040831     C                   EVAL      I27tln = D00dpd
234800021202     C                   CALL      'TRUL27R'
234900021202     C                   PARM                    TRUL27DS
235000021202     C*
235100021202IF  1C                   IF        O27ERR <> 'E'
235200021202     C                   Z-ADD     O27KSC        KTMKSC
235300970117IF  2C     D00CTO        IFEQ      'O'
235400970117     C     KTMKSC        SETLL     TNOFM01L
235500970117     C     KTMKSC        READE     TNOFM01L                               99
235600970117X   2C                   ELSE
235700970117     C     KTMKSC        SETLL     TNTAM01L
235800970117     C     KTMKSC        READE     TNTAM01L                               99
235900970117E   2C                   ENDIF
236000970117DO  2C     *IN99         DOWEQ     *OFF
236100970117IF  3C     TAMATB        IFNE      'A'                                          *NO ANNULLATE
236200970117     C     TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
236300970117IF  4C     DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
236400970117     C     DATCOR        ANDLE     TAMDST
236500970117     C                   Z-ADD     TAMKSC        CARKSC
236600970117     C                   Z-ADD     TAMCTR        CARCTR
236700970117     C                   Z-ADD     TAMPRG        CARPRG
236800011003     C                   MOVEL     TAMFLO        DSTA01
236900011003     C                   MOVEL     §TADIV        CARDIV
237000970117     C                   LEAVE
237100970117E   4C                   ENDIF
237200970117E   3C                   ENDIF
237300970117IF  3C     D00CTO        IFEQ      'O'
237400970117     C     KTMKSC        READE     TNOFM01L                               99
237500970117X   3C                   ELSE
237600970117     C     KTMKSC        READE     TNTAM01L                               99
237700970117E   3C                   ENDIF
237800970117E   2C                   ENDDO
237900970117E   1C                   ENDIF
238000040831     c* carico nazioni gestite dai network
238100040831     c                   if        d00fie = 'E' or d00dpd <> ' '
238200040831     c                   exsr      srnaz
238300040831     c                   end
238400970117     C*--------------------
238500970117     C* CODICI TASSAZIONE
238600970117     C*--------------------
238700970117     C*---
238800970117     C* TARIFFA ITALIA: LNP ITALIA -> CARICA CODICI TASSAZIONE ITALIA
238900970117     C* TARIFFA ESTERO: LNP ITALIA -> CARICA CODICI TASSAZIONE ESTERO
239000970117     C*                 LNP ESTERO -> CARICA CODICI TASSAZIONE ITALIA + ESTERO
239100970117     C*---
239200970117     C                   Z-ADD     *ZEROS        I                 4 0
239300970117     C                   Z-ADD     *ZEROS        J                 4 0
239400970117     C                   Z-ADD     *ZEROS        K                 4 0
239500020906     C                   Z-ADD     1             KTBKUT
239600970117     C                   MOVEL     'CT'          KTBCOD
239700970117     C     KE2TAB        SETLL     TABEL00F
239800040831     c                   do        *hival
239900040831     C     KE2TAB        READE     TABEL00F
240000040831DO  1C                   if        %eof(tabel00f)
240100040831     c                   leave
240200040831     c                   end
240300040831IF  2C     TBLFLG        IFne      *BLANKS                                      *NON ANNULLATI
240400040831     c                   iter
240500040831     c                   end
240600970117     C                   MOVEL     TBLUNI        DSCT
240700040831     c* utilizzo in tariffa
240800040831IF  3C     §CTUTC        IFne      'S'                                          *DA CONSIDERARE
240900040831     c                   iter
241000040831     c                   end
241100040902     c* per fed solo tariffe fed e viceversa
241200040902IF  3C     §CTUTf        IFeq      'S'                                          *DA CONSIDERARE
241300040902IF  3C     d00dpd        andne     'F'                                          *DA CONSIDERARE
241400040902     c                   iter
241500040902     c                   end
241600040902IF  3C     §CTUTf        IFne      'S'                                          *DA CONSIDERARE
241700040902IF  3C     d00dpd        andeq     'F'                                          *DA CONSIDERARE
241800040902     c                   iter
241900040902     c                   end
242000040902     c* per dpd no cod tassazione EE
242100040902     c                   if        tblkey = 'EE' and d00dpd = 'D'
242200040902     c                   iter
242300040902     c                   end
242400040831     c* verifico tab. delle nazioni x vedere se paese servito dal network
242500040831     c                   if        d00fie = 'E' or d00dpd <> ' '
242600040901     c     §ctnar        lookup    nar                                    99
242700040831     c                   if        not *in99
242800040831     c                   iter
242900040831     c                   end
243000040831     c                   end
243100040831     c* Italia
243200970312IF  4C     D00FIE        IFEQ      'I'                                          *TARIFFA ITALIA
243300970117     C     §CTNAR        ANDEQ     *BLANKS                                      *CODICI ITALIA
243400040830     C     d00dpd        ANDeq     ' '                                          *CODICI ITALIA
243500040831     c* DPD
243600040830IF  4C     D00FIE        orEQ      'I'                                          *TARIFFA ITALIA
243700040831     C     d00dpd        ANDeq     'D'                                          *CODICI ITALIA
243800040830     C     §CTNAR        ANDne     *BLANKS                                      *CODICI ITALIA
243900040901     c*estero/FED
244000970117     C     D00FIE        OREQ      'E'                                          *TARIFFA ESTERO
244100971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
244200970117     C     §CTNAR        ANDNE     *BLANKS                                      *CODICI ESTERO
244300040901     c*
244400970117     C     D00FIE        OREQ      'E'                                          *TARIFFA ESTERO
244500971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ESTERO
244600970117     C                   ADD       1             I
244700970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
244800980514     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
244900970117     C                   MOVEL     §CTDES        DCT(I)                         *DESCRIZIONE
245000970117     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
245100970117     C                   MOVEL     §CTCOR        COR(I)                         *ORDINAMENTO
245200970117     C                   MOVE      §CTCOR        WA1                            *TIPO CODICE
245300970312IF  5C     WA1           IFEQ      '9'
245400970312IF  6C     CTS(I)        IFEQ      'IT'
245500970117     C     CTS(I)        OREQ      'EE'
245600970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
245700970312X   6C                   ELSE
245800970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
245900040903     c* per fedex carico sia come regione che come codice elementare
246000040903     c                   if        d00dpd = 'F'
246100040903     C                   MOVEL     'E'           TCT(I)                         *TIPO "REGIONE"
246200040903     c                   end
246300040830E   6C                   ENDIF
246400970312X   5C                   ELSE
246500970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
246600970312E   5C                   ENDIF
246700970312IF  5C     §CTNAR        IFEQ      *BLANKS
246800970117     C                   MOVEL     'IT'          NAZ(I)                         *NAZIONE ITALIA
246900970312X   5C                   ELSE
247000970117     C                   MOVEL     'EE'          NAZ(I)                         *NAZIONE ESTERO
247100970312E   5C                   ENDIF
247200970117     C* VIDEO->ELEMENTARI
247300970312IF  5C     TCT(I)        IFEQ      'E'
247400970117     C                   ADD       1             J
247500980508     C                   MOVEL     CTS(I)        VCTS(J)                        *CODICE
247600970117     C                   MOVEL     FCR(I)        VFCR(J)                        *FLAG CAPOLUOGO
247700970117     C                   MOVEL     COR(I)        VCOR(J)                        *ORDINAMENTO
247800970117     C                   MOVEL     RAP(I)        VRAP(J)                        *REGIONE
247900980508     C                   MOVEL     'T'           VRCT(J)                        *RAGGRUPPAMENTO
248000970312E   5C                   ENDIF
248100040903     c                   if        d00dpd = 'F'
248200040903     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
248300040903     c                   end
248400970117     C* VIDEO->REGIONALI
248500970312IF  5C     TCT(I)        IFEQ      'R'
248600970117     C                   ADD       1             K
248700970117     C                   MOVEL     CTS(I)        RCTS(K)                        *CODICE
248800970117     C                   MOVEL     COR(I)        RCOR(K)                        *ORDINAMENTO
248900970117E   2C                   ENDIF
249000040901E   2C                   ENDIF
249100970117E   1C                   ENDDO
249200970117     C                   Z-ADD     J             WUVCTS            4 0          *N°COD.TASS.ELEM.VID
249300970117     C                   Z-ADD     K             WURCTS            4 0          *N°COD.TASS.REG.VID.
249400970117     C*
249500970117     C* ORDINA CODICI ELEMENTARI VIDEO
249600970117     C                   EXSR      ORDVCT
249700970117     C*
249800970117     C* ORDINA CODICI REGIONALI VIDEO
249900970117     C                   EXSR      ORDRCT
250000970117     C*
250100970117     C                   ENDSR
250200970117     C*------------------------------------------------------------------------*
250300040831     C* leggo tabella delle nazioni x vedere se paese servito dal network
250400970117     C*------------------------------------------------------------------------*
250500040831     C     srnaz         BEGSR
250600040901     c                   clear                   nar
250700040901     c                   clear                   dfe
250800040901     c                   clear                   nz                3 0
250900040831     C                   Z-ADD     1             KTBKUT
251000040831     C                   MOVEL     '15'          KTBCOD
251100040831     C     KE2TAB        SETLL     TABEL00F
251200040831     c                   do        *hival
251300040831     C     KE2TAB        READE     TABEL00F
251400040831DO  1C                   if        %eof(tabel00f)
251500040831     c                   leave
251600040831     c                   end
251700040831IF  2C     TBLFLG        IFne      *BLANKS                                      *NON ANNULLATI
251800040831     c                   iter
251900040831     c                   end
252000040831     C                   MOVEL     TBLUNI        DS15
252100040901     c* EEX
252200040901     c                   if        d00fie = 'E' and d00dpd = *blanks
252300040901     c                             and §15eex = 'S'
252400040901     c                   add       1             nz
252500040902     c                   movel     'E'           dfe(nz)
252600040901     c                   movel     tblkey        nar(nz)
252700040901     c                   iter
252800040901     c                   end
252900040901     c* FED
253000040901     c                   if        d00fie = 'E' and d00dpd = 'F'
253100040901     c                             and §15laf <> 0
253200040901     c                   add       1             nz
253300040902     c                   movel     'F'           dfe(nz)
253400040901     c                   movel     tblkey        nar(nz)
253500040901     c                   iter
253600040901     c                   end
253700040901     c* FED
253800040901     c                   if        d00fie = 'I' and d00dpd = 'D'
253900040901     c                             and §15lad <> 0
254000040901     c                   add       1             nz
254100040902     c                   movel     'D'           dfe(nz)
254200040901     c                   movel     tblkey        nar(nz)
254300040901     c                   iter
254400040901     c                   end
254500040831     c                   enddo
254600040831     C                   ENDSR
254700040831     C*------------------------------------------------------------------------*
254800040831     C* ORDVCT - ORDINA I CODICI TASSAZIONE ELEMENTARI DA EMETTERE A VIDEO
254900040831     C*------------------------------------------------------------------------*
255000040831     C     ORDVCT        BEGSR
255100970117     C*
255200970117DO  1C     1             DO        WUVCTS        I
255300970117IF  2C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
255400970117     C     1             ADD       I             J
255500970117DO  3C     J             DO        WUVCTS        J
255600970117IF  4C     VCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
255700970117IF  5C     VCOR(I)       IFGT      VCOR(J)
255800970117     C                   MOVEL     VCTS(I)       PCTS                           *CODICE
255900970117     C                   MOVEL     VCTS(J)       VCTS(I)
256000970117     C                   MOVEL     PCTS          VCTS(J)
256100970117     C                   MOVEL     VFCR(I)       PFCR                           *FLAG CAPOLUOGO
256200970117     C                   MOVEL     VFCR(J)       VFCR(I)
256300970117     C                   MOVEL     PFCR          VFCR(J)
256400970117     C                   MOVEL     VCOR(I)       PCOR                           *ORDINAMENTO
256500970117     C                   MOVEL     VCOR(J)       VCOR(I)
256600970117     C                   MOVEL     PCOR          VCOR(J)
256700970117     C                   MOVEL     VRAP(I)       PRAP                           *REGIONE
256800970117     C                   MOVEL     VRAP(J)       VRAP(I)
256900970117     C                   MOVEL     PRAP          VRAP(J)
257000970117E   5C                   ENDIF
257100970117E   4C                   ENDIF
257200970117E   3C                   ENDDO                                                  *FINE CICLO UNO
257300970117E   2C                   ENDIF
257400970117E   1C                   ENDDO                                                  *FINE CICLO DUE
257500970117     C*
257600970117     C                   ENDSR
257700970117     C*------------------------------------------------------------------------*
257800970117     C* ORDRCT - ORDINA I CODICI TASSAZIONE REGIONALI DA EMETTERE A VIDEO
257900970117     C*------------------------------------------------------------------------*
258000970117     C     ORDRCT        BEGSR
258100970117     C*
258200970117DO  1C     1             DO        WURCTS        I
258300970117IF  2C     RCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
258400970117     C     1             ADD       I             J
258500970117DO  3C     J             DO        WURCTS        J
258600970117IF  4C     RCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
258700970117IF  5C     RCOR(I)       IFGT      RCOR(J)
258800970117     C                   MOVEL     RCTS(I)       PCTS                           *CODICE
258900970117     C                   MOVEL     RCTS(J)       RCTS(I)
259000970117     C                   MOVEL     PCTS          RCTS(J)
259100970117     C                   MOVEL     RCOR(I)       PCOR                           *ORDINAMENTO
259200970117     C                   MOVEL     RCOR(J)       RCOR(I)
259300981218     C                   MOVEL     PCOR          RCOR(J)
259400981218E   5C                   ENDIF
259500970117E   4C                   ENDIF
259600970117E   3C                   ENDDO                                                  *FINE CICLO UNO
259700970117E   2C                   ENDIF
259800970117E   1C                   ENDDO                                                  *FINE CICLO DUE
259900970117     C*
260000970117     C                   ENDSR
260100970117     C*------------------------------------------------------------------------*
260200970117     C* IMPVD0 - IMPOSTA LA TESTATA
260300970117     C*------------------------------------------------------------------------*
260400970117     C     IMPVD0        BEGSR
260500970117     C*
260600970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
260700970117     C*---
260800970117     C* TESTATA
260900970117     C*---
261000970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
261100970117IF  1C     D00TLA        IFEQ      'P'
261200970117     C                   MOVEL     DVID(1)       V0CTLA                         *LIVELLO PROCEDURA
261300970117X   1C                   ELSE
261400970117     C                   MOVEL     DVID(2)       V0CTLA                         *LIVELLO PROCEDURA
261500970117E   1C                   ENDIF
261600970117IF  1C     D00CTO        IFEQ      'O'
261700970117     C                   MOVEL     'Visita '     V0DKOV                         *TARIFFA O OFFERTA
261800970117X   1C                   ELSE
261900970117     C                   MOVEL     'Cliente'     V0DKOV                         *TARIFFA O OFFERTA
262000970117E   1C                   ENDIF
262100970117IF  1C     $TGES         IFEQ      'I'
262200970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
262300970122     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
262400970117X   1C                   ELSE
262500970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
262600970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
262700970117E   1C                   ENDIF
262800970117     C* CAMPI
262900970117     C                   Z-ADD     D00KSC        V0CKSC
263000970117     C                   Z-ADD     D00CTR        V0CCTR
263100970117     C                   Z-ADD     D00PRG        V0CPRG
263200970117     C                   MOVEL     D00FIE        V0CFIE
263300980508     C                   MOVEL     D00DSF        V0CDSF
263400970117     C                   MOVEL     D00TSP        V0CTSP
263500970117     C                   Z-ADD     D00LNP        V0CLNP
263600011003     C                   MOVEL     D00DIV        V0CDIV
263700970117IF  1C     D00DTC        IFGT      *ZEROS
263800970117     C                   Z-ADD     D00DTC        G08INV
263900970117     C                   Z-ADD     *ZEROS        G08DAT
264000970117     C                   MOVEL     '3'           G08ERR
264100970117     C                   CALL      'XSRDA8'
264200970117     C                   PARM                    WLBDA8
264300970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
264400970117E   1C                   ENDIF
264500970117     C* DECODIFICHE
264600970117     C                   MOVEL     *BLANKS       V0DKSC
264700970117IF  1C     D00KSC        IFGT      *ZEROS
264800020906     C                   Z-ADD     1             KACKUT
264900970117     C                   Z-ADD     KCCCL         KACKCC
265000970117     C                   Z-ADD     D00KSC        KACKSC
265100970117     C     KEYACO        CHAIN     CNACO00F                           99
265200000211IF  2C*---       *IN99     IFEQ *OFF
265300000211     C*---                 MOVELACORAG    V0DKSC           *CLIENTE
265400000211E   2C*---                 ENDIF
265500000211IF  2C     D00CTO        IFEQ      'T'                                          *TARIFFA
265600000211     C     KEYACO        CHAIN     CNACO00F                           99
265700000211X   2C                   ELSE                                                   *OFFERTA
265800000211     C     KEYACO        CHAIN     TFACO00F                           99
265900000211E   2C                   ENDIF
266000000211IF  2C     *IN99         IFEQ      *OFF
266100000211     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
266200000211E   2C                   ENDIF
266300970117E   1C                   ENDIF
266400970117     C*
266500970117     C                   MOVEL     *BLANKS       V0DCTR
266600970117IF  1C     D00KSC        IFGT      *ZEROS
266700970117     C                   MOVEL     D00CTR        WA1               1
266800970117     C                   SETOFF                                           98
266900970117     C                   Z-ADD     1             I
267000970117     C     WA1           LOOKUP    TTR(I)                                 98
267100970117IF  2C     *IN98         IFEQ      *ON
267200970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
267300970117E   2C                   ENDIF
267400970117E   1C                   ENDIF
267500970117     C*
267600970117     C                   MOVEL     *BLANKS       V0DTSP
267700970117IF  1C     D00TSP        IFNE      *BLANKS
267800970117     C                   SETOFF                                           98
267900970117     C                   Z-ADD     1             I
268000970117     C     D00TSP        LOOKUP    TSP(I)                                 98
268100970117IF  2C     *IN98         IFEQ      *ON
268200970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
268300970117E   2C                   ENDIF
268400970117E   1C                   ENDIF
268500970117     C*
268600970117     C                   MOVEL     *BLANKS       V0DFIE
268700970117IF  1C     D00FIE        IFEQ      'I'
268800970117     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
268900970117E   1C                   ENDIF
269000970117IF  1C     D00FIE        IFEQ      'E'
269100970117     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
269200970117E   1C                   ENDIF
269300980508     C*
269400980508     C                   MOVEL     *BLANKS       V0DDSF
269500980508IF  1C     D00DSF        IFEQ      'S'
269600980508     C                   MOVEL     DDEC(7)       V0DDSF                         *DATI SEDE/FILIALE
269700980508E   1C                   ENDIF
269800980508IF  1C     D00DSF        IFEQ      'F'
269900980508     C                   MOVEL     DDEC(8)       V0DDSF                         *DATI SEDE/FILIALE
270000980508E   1C                   ENDIF
270100970117     C*
270200970117     C                   MOVEL     *BLANKS       V0DLNP
270300970117IF  1C     D00LNP        IFGT      *ZEROS
270400970117     C                   Z-ADD     D00LNP        KORFIL
270500970117     C     KEYORG        CHAIN     AZORG01L                           99
270600970117IF  2C     *IN99         IFEQ      *OFF
270700970117     C     ORGFVA        ANDEQ     *BLANKS
270800970117     C     ORGFAG        ANDNE     'V'
270900970117     C                   MOVEL     ORGDES        V0DLNP
271000970117E   2C                   ENDIF
271100970117E   1C                   ENDIF
271200970117     C*---
271300970117     C* FINESTRA F05-CARICA DA ALTRA TARIFFA
271400970117     C*---
271500980508     C                   MOVEL     'S'           V2WDSF
271600980508     C                   MOVEL     'T'           V2WCTO
271700970117     C                   Z-ADD     CARKSC        V2WKSC
271800970117     C                   Z-ADD     CARCTR        V2WCTR
271900970117     C                   Z-ADD     CARPRG        V2WPRG
272000970117     C                   Z-ADD     D00LNP        V2WLNP
272100011003     C                   MOVEL     CARDIV        V2WDIV
272200970117     C*
272300970117     C                   ENDSR
272400970117     C*------------------------------------------------------------------------*
272500970117     C* MEMREC - MEMORIZZA IN SCHIERE I RECORD DA MANUTENZIONARE
272600970117     C*------------------------------------------------------------------------*
272700970117     C     MEMREC        BEGSR
272800970117     C*
272900980708     C* LEGGE I DATI TIPO DEL CLIENTE/DELLA VISITA
273000980508     C                   MOVEL     D00DSF        KTTDSF                         *DATI SEDE O FILIALE
273100980508     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
273200970122     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
273300970117     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
273400970117     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
273500970117     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
273600970117     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
273700040831     C                   MOVEL     D00dpd        KTTdpd                         *ITALIA/ESTERO
273800011001     C     KE2ETT        SETLL     TEETT01L                               99
273900970117     C*
274000970117     C* SE CODICI INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
274100970117IF  1C     *IN99         IFEQ      *OFF
274200970117     C                   MOVEL     DOPE(1)       V0COPE                         I N S E R I M E N T
274300970117     C                   WRITE     TE03D00                                      *TESTATA
274400970117     C                   WRITE     TE03D01                                      *MESSAGGIO SF VUOTO
274500970117     C                   EXSR      F05GES
274600980508     C*
274700980508     C* SE NON E' STATO CONFERMATO IL CARICAMENTO DA ALTRA TARIFFA, ALLORA
274800980508     C* CARICA I CODICI TASSAZIONE DA TABELLA
274900980508IF  2C     $CONF         IFEQ      'N'                                          *NESSUNA CONFERMA
275000980508     C                   EXSR      CARSTD                                       *CARICA DATI STANDAR
275100980508     C                   EXSR      AGGSCV                                       *AGGIORNA SCHIERE VI
275200980508E   2C                   ENDIF
275300970117X   1C                   ELSE
275400980708     C                   MOVEL     DOPE(2)       V0COPE                         *V A R I A Z I O N E
275500980708     C                   MOVEL     D00DSF        WDSF                           *IMPOSTA DATI CHIAVE
275600980708     C                   MOVEL     D00CTO        WCTO
275700980708     C                   Z-ADD     D00KSC        WKSC
275800980708     C                   Z-ADD     D00CTR        WCTR
275900980708     C                   Z-ADD     D00PRG        WPRG
276000980708     C                   Z-ADD     D00LNP        WLNP
276100980708     C                   MOVEL     D00FIE        WFIE
276200040831     C                   MOVEL     D00dpd        Wdpd
276300011003     C                   MOVEL     D00DIV        WDIV
276400980708     C                   EXSR      MEMETT                                       *MEM.DATI E AGG.VIDE
276500980708E   1C                   ENDIF
276600970117IF  1C     $TGES         IFEQ      'I'
276700970117     C                   MOVEL     *BLANKS       V0COPE
276800970117E   1C                   ENDIF
276900970117     C*
277000970117     C                   ENDSR
277100970129     C*------------------------------------------------------------------------*
277200970129     C* MEMCAR - MEMORIZZA I DATI DELLA TARIFFA DI CARTELLO
277300970129     C*------------------------------------------------------------------------*
277400970129     C     MEMCAR        BEGSR
277500980515     C*
277600980515     C* IMPOSTA I CODICI TASSAZIONE STANDARD
277700980708     C                   MOVEL     VCTS          CTS8
277800980708     C                   MOVEL     VRAP          RAP8
277900980515     C*
278000980515     C* PULISCE LE SCHIERE DI MEMORIZZAZIONE
278100980515     C                   CLEAR                   RCT8
278200980514     C                   CLEAR                   PPE8
278300970129     C* CARCAMENTO DATI
278400980515     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
278500980508     C                   MOVEL     'T'           KTTCTO                         *TARIFFA O OFFERTA
278600980508     C                   Z-ADD     CARKSC        KTTKSC                         *CLIENTE O VISITA
278700970129     C                   Z-ADD     CARCTR        KTTCTR                         *TARIFFA
278800970129     C                   Z-ADD     CARPRG        KTTPRG                         *PROGRESSIVO
278900970129     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
279000970129     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
279100040831     C                   MOVEL     D00dpd        KTTdpd                         *ITALIA/ESTERO
279200011001     C     KE2ETT        SETLL     TEETT01L                               99
279300011001     C     KE2ETT        READE(N)  TEETT01L                               99
279400970129DO  1C     *IN99         DOWEQ     *OFF
279500970129IF  2C     ETTATB        IFEQ      *BLANKS
279600970129     C                   SETOFF                                           98
279700970129     C                   Z-ADD     1             I
279800980515     C     ETTCTS        LOOKUP    CTS8(I)                                98
279900970129IF  3C     *IN98         IFEQ      *ON
280000970129     C                   MOVEL     ETTRCT        RCT8(I)                        *RAGGRUPPAMENTO
280100970129     C                   Z-ADD     ETTPPE        PPE8(I)                        *%INCIDENZA
280200980911     C     ETTRCT        IFEQ      'N'
280300980911     C     ETTPPE        ANDEQ     *ZEROS
280400980911     C                   MOVEL     *BLANKS       RCT8(I)
280500980911     C                   ENDIF
280600970129E   3C                   ENDIF
280700970129E   2C                   ENDIF
280800011001     C     KE2ETT        READE(N)  TEETT01L                               99
280900970129E   1C                   ENDDO
281000970129     C*
281100970129     C                   ENDSR
281200980508     C*------------------------------------------------------------------------*
281300980508     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
281400980508     C*------------------------------------------------------------------------*
281500980508     C     CTRENT        BEGSR
281600980508     C*--------------------
281700980508     C* INTERROGAZIONE -> DATI TIPO INESISTENTI
281800980508     C*--------------------
281900980508IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
282000980508     C                   MOVEL     '0'           WECTS             1            *NO ESISTENZA CTS
282100980508     C                   MOVEL     D00DSF        KTTDSF                         *DATI SEDE/FILIALE
282200980508     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
282300980508     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE P VISITA
282400980508     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
282500980508     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
282600980508     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
282700980508     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
282800040831     C                   MOVEL     D00dpd        KTTdpd                         *ITALIA/ESTERO
282900011001     C     KE2ETT        SETLL     TEETT01L                               99
283000011001     C     KE2ETT        READE(N)  TEETT01L                               99
283100980508DO  2C     *IN99         DOWEQ     *OFF                                         *NON TROVATO
283200980508IF  3C     ETTATB        IFEQ      *BLANKS                                      *NON ANNULLATI
283300980508     C                   MOVEL     '1'           WECTS                          *SI ESISTENZA CTS
283400980508     C                   LEAVE
283500980508E   3C                   ENDIF
283600011001     C     KE2ETT        READE(N)  TEETT01L                               99
283700980508E   2C                   ENDDO
283800980508IF  2C     WECTS         IFEQ      '0'
283900980508     C                   MOVEL     '1'           WERR                           *ERRORE
284000980508     C                   MOVEL     MSG(1)        D00MSG
284100980508     C                   MOVEL     '1'           D00ERR
284200980508     C                   MOVEL     'S'           $FINE                          *USCITA PGM
284300980508E   2C                   ENDIF
284400980508E   1C                   ENDIF
284500980508     C*
284600980508     C                   ENDSR
284700970117     C*------------------------------------------------------------------------*
284800970117     C* *INZSR - OPERAZIONI INIZIALI
284900970117     C*------------------------------------------------------------------------*
285000970117     C     *INZSR        BEGSR
285100970117     C*--------------------
285200970117     C* RICEVIMENTO PARAMETRI
285300970117     C*--------------------
285400970117     C     *ENTRY        PLIST
285500970117     C                   PARM                    KPJBA
285600970117     C                   PARM                    DSTE00
285700041202     c* apertura controllata di alcuni file xchè solo di filiale
285800041202     c                   open      tfaco00f                             95
285900041202     c                   open      tnofm01l                             95
286000970117     C*--------------------
286100970117     C* VARIABILI RIFERITE AL DATABASE
286200970117     C*--------------------
286300970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
286400970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
286500970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
286600970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
286700970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
286800970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
286900970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
287000970117     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
287100011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
287200980508     C     *LIKE         DEFINE    ETTCTO        KTTCTO
287300970117     C     *LIKE         DEFINE    ETTKSC        KTTKSC
287400970117     C     *LIKE         DEFINE    ETTCTR        KTTCTR
287500970117     C     *LIKE         DEFINE    ETTPRG        KTTPRG
287600970117     C     *LIKE         DEFINE    ETTLNP        KTTLNP
287700970117     C     *LIKE         DEFINE    ETTFIE        KTTFIE
287800040831     C     *LIKE         DEFINE    ETTdpd        KTTdpd
287900970117     C     *LIKE         DEFINE    ETTCTS        KTTCTS
288000970117     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
288100970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
288200970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
288300011003     C     *LIKE         DEFINE    §TADIV        CARDIV
288400970128     C     *LIKE         DEFINE    ETTCTS        DEPCTS
288500970128     C     *LIKE         DEFINE    ETTPPE        DEPPPE
288600020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
288700970117     C     *LIKE         DEFINE    ETTCTS        PCTS                           *DI PARCHEGGIO
288800970117     C     *LIKE         DEFINE    §CTFCR        PFCR
288900970117     C     *LIKE         DEFINE    §CTCOR        PCOR
289000970117     C     *LIKE         DEFINE    §CTRAP        PRAP
289100970117     C     *LIKE         DEFINE    ETTPPE        PPPE
289200980708     C     *LIKE         DEFINE    D00DSF        WDSF                           *DI LAVORO
289300980708     C     *LIKE         DEFINE    D00CTO        WCTO
289400980708     C     *LIKE         DEFINE    D00KSC        WKSC
289500980708     C     *LIKE         DEFINE    D00CTR        WCTR
289600980708     C     *LIKE         DEFINE    D00PRG        WPRG
289700980708     C     *LIKE         DEFINE    D00LNP        WLNP
289800980708     C     *LIKE         DEFINE    D00FIE        WFIE
289900040831     C     *LIKE         DEFINE    D00dpd        Wdpd
290000011003     C     *LIKE         DEFINE    D00DIV        WDIV
290100020729     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
290200020729     C     *LIKE         DEFINE    TBEKE1        KTEKE1
290300970117     C*--------------------
290400970117     C* CHIAVI DI LETTURA
290500970117     C*--------------------
290600970117     C* LETTURA TABEL00F - COMPLETA
290700970117     C     KEYTAB        KLIST
290800970117     C                   KFLD                    KTBKUT                         *UTENTE
290900970117     C                   KFLD                    KTBCOD                         *TABELLA
291000970117     C                   KFLD                    KTBKEY                         *CODICE
291100970117     C* LETTURA TABEL00F - PARZIALE
291200970117     C     KE2TAB        KLIST
291300970117     C                   KFLD                    KTBKUT                         *UTENTE
291400970117     C                   KFLD                    KTBCOD                         *TABELLA
291500970117     C* LETTURA AZORG01L - COMPLETA
291600970117     C     KEYORG        KLIST
291700970117     C                   KFLD                    KORFIL                         *FILIALE
291800970117     C* LETTURA CNACO01L - COMPLETA
291900970117     C     KEYACO        KLIST
292000970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
292100970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
292200970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
292300011001     C* LETTURA TEETT01L - PARZIALE
292400970117     C     KE2ETT        KLIST
292500980508     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
292600980508     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
292700970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
292800970117     C                   KFLD                    KTTCTR                         *TARIFFA
292900970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
293000970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
293100970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
293200040831     C                   KFLD                    KTTdpd                         *ITALIA/ESTERO
293300011001     C* LETTURA TEETT01L - COMPLETA
293400970117     C     KEYETT        KLIST
293500980508     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
293600970117     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
293700970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
293800970117     C                   KFLD                    KTTCTR                         *TARIFFA
293900970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
294000970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
294100970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
294200040831     C                   KFLD                    KTTdpd                         *ITALIA/ESTERO
294300970117     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
294400020729     C* LETTURA TNTBE01L - PARZIALE
294500020729     C     KEYTBE        KLIST
294600020729     C                   KFLD                    KTECOD                         *UTENTE
294700020729     C                   KFLD                    KTEKE1                         *CODICE
294800970117     C*--------------------
294900970117     C* VARIABILI NON RIFERITE AL DATABASE
295000970117     C*--------------------
295100970117     C     *LIKE         DEFINE    NR1           SF1EOF                         *DI DEPOSITO
295200970117     C     *LIKE         DEFINE    NR2           SF2EOF
295300970117     C     *LIKE         DEFINE    NR3           SF3EOF
295400970117     C     *LIKE         DEFINE    $PC1          $DPC1                          *DEPOSITO NRR 1°SFIL
295500970129     C     *LIKE         DEFINE    V1SOPZ        DEPOPZ
295600970117     C*--------------------
295700970117     C* VARIABILI DI CONTROLLO
295800970117     C*--------------------
295900970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
296000970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
296100970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
296200970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
296300970117     C                   MOVEL     '0'           WERR              1            *ERRORE
296400970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
296500970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
296600970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
296700970619     C                   MOVEL     'N'           WEETT             1            *ESISTENZA CTS
296800970117     C*
296900970117     C* REPERISCE LA DATA CORRENTE
297000970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
297100970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
297200970117     C                   Z-ADD     WN8           G08DAT
297300970117     C                   Z-ADD     *ZEROS        G08INV
297400970117     C                   MOVEL     '0'           G08ERR
297500970117     C                   CALL      'XSRDA8'
297600970117     C                   PARM                    WLBDA8
297700970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
297800970117     C*
297900970117     C* 02->GESTIONE, 05->INTERROGAZIONE
298000970117     C                   MOVE      D00OP0        WA2               2
298100970117IF  1C     WA2           IFEQ      '02'
298200970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
298300970117X   1C                   ELSE
298400970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
298500970117E   1C                   ENDIF
298600971204     C*
298700971204     C* REPERISCE IL FLAG ITALIA/ESTERO PER LA LNP DI ENTRATA
298800971204     C                   MOVEL     D00LNP        WLNP
298900971204     C                   MOVEL     *BLANKS       WFL1
299000971204     C                   EXSR      CHKLNP
299100971204     C                   MOVEL     WFL1          DLPFL1
299200970117     C*
299300970117     C* CARICA I CAPICONTO DI CONTABILITA'
299400970117     C                   EXSR      CARCC
299500980508     C*
299600980508     C* CONTROLLI INIZIALI DI ENTRATA
299700980508     C                   EXSR      CTRENT
299800970117     C*
299900970117     C* CARICA LE TABELLE OCCORRENTI
300000970117     C                   EXSR      CARTAB
300100970117     C*
300200970117     C* IMPOSTA LA TESTATA
300300970117     C                   EXSR      IMPVD0
300400970129     C*
300500970129     C* MEMORIZZA I DATI DELLA TARIFFA DI CARTELLO
300600970129     C                   EXSR      MEMCAR
300700970117     C*
300800970117     C* MEMORIZZA IN SCHIERE I RECORD DA MANUTENZIONARE
300900970117     C                   EXSR      MEMREC
301000970117     C*
301100970117     C                   ENDSR
301200970117     O*------------------------------------------------------------------------*
301300980508** -DVID-
301400970117PORTO               1
301500970117DELTA               2
3016009701173
3017009701174
3018009701175
301900970117** -DDEC-
302000970117ITALIA                   1
302100970117ESTERO                   2
302200970117NON APPARE IN TARIFFA    3
302300970117APPARE IN TARIFFA        4
302400970117APPARE IN XXXXXXXXXXXXXXX5
302500970117SITUAZIONE MISTA         6
302600980508SEDE                     7
302700980508FILIALE                  8
3028009701179
302900980818TUTTE LE FILIALI         10
303000970117** -DOPE-
303100970117* INSERIMENTO *1
303200970117* VARIAZIONE * 2
303300970122INTERROGAZIONE 3
303400970122 * GESTIONE *  4
3035009701175
303600970117** -MSG-
303700980508TNTE03R- Non esistono i dati tipo per la tariffa/visita richiesta             1
303800970117TNTE03R- Raggruppamento codice tassazione errato                              2
303900970117TNTE03R- Se il codice tass. è da esprimere immettere la %incidenza            3
304000970117TNTE03R- Se il codice tass. non è da esprimere lasciare vuota la %incidenza   4
304100970117TNTE03R- Il totale della %incidenza di tutti i codici tass. deve essere 100   5
304200970128TNTE03R- Togliere nei codici tassazione della regione il raggruppamento       6
304300970128TNTE03R- Togliere nei codici tassazione della regione la % di incidenza       7
304400970129TNTE03R- Inserire in tutti i codici tassazione il reggruppamento della regione8
304500970129TNTE03R- Inserire in tutti i codici tassazione la % di incidenza              9
304600970128TNTE03R- La % regione e la somma delle % dei codici tass. deve esser uguale   10
304700970117TNTE03R- Opzione errata                                                       11
304800970117TNTE03R- Immettere SI per confermare oppure NO                                12
304900970117TNTE03R- Dati tipo richiesti non trovati                                      13
305000970117TNTE03R- Caricare i codici tassazione da altra tariffa o da altra offerta     14
305100970117TNTE03R- Filiale errata                                                       15
305200970117TNTE03R- "T" per caricare i dati tariffa, "O" per caricare i dati offerta     16
305300970117TNTE03R- Tariffa Italia. Filiale errata                                       17
305400970117TNTE03R- Tariffa Estero con linea di partenza italia. Filiale errata          18
305500970117TNTE03R- Tariffa Estero con linea di partenza estero. Filiale errata          19
305600970117Scorrimento indietro o avanti oltre il primo o l'ultimo record del file.      20
305700970129TNTE03R- Inserire nei codici tassazione con raggruppamento la % di incidenza  21
305800970129TNTE03R- Inserire nei codici tassazione con % di incidenza il raggruppamento  22
305900970415TNTE03R- La % regione non puo' essere inferiore a xxx,x                       23
306000980508TNTE02R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    24
306100970117TNTE03R- 25
306200970117TNTE03R- 26
306300970117TNTE03R- 27
306400970117TNTE03R- 28
306500970117TNTE03R- 29
306600980508TNTE03R- 30
