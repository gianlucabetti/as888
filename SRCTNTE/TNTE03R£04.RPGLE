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
003901091203     D Tnte04DS      E DS                                                       *Profili utente
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
007801050304     D rct             S              1    DIM(600)                             *CODICE NAZIONE -> P
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
011600970124     D*-------------------
011700970124     D* DI LAVORO
011800970124     D*-------------------
011900970124     D SCH             S              1    DIM(15)
012000970117     D*------------------
012100970117     D* TABELLE CAPICONTO
012200970117     D*------------------
012300970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
012400970117     D*------------------------------------------------------------------------*
012500970117     D* INPUT
012600970117     D*------------------------------------------------------------------------*
012700970117     D*-------------------
012800970117     D* DS TIPI SERVIZIO
012900970117     D*-------------------
013000970117     D DS5E          E DS
013100970117     D*-------------------
013200970117     D* DS TIPI TARIFFA
013300970117     D*-------------------
013400970117     D DSTR          E DS
013500021202     D*-------------------
013600021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
013700021202     D*-------------------
013800021202     D TRUL27DS      E DS
013900970117     D*-------------------
014000970117     D* DS CODICI TASSAZIONE
014100970117     D*-------------------
014200970117     D DSCT          E DS
014300970117     D*-------------------
014400970117     D* DS PASSAGGIO PARAMETRI
014500970117     D*-------------------
014600970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
014700980508     D*------------------
014800980508     D* DS RICERCA DATI TIPO
014900980508     D*------------------
015000980508     D DSTE30        E DS                  EXTNAME(TNTE30DS)
015100011003     D*-------------------
015200011003     D* DS REPERIMENTO DIVISA TARIFFE/OFFERTE
015300011003     D*-------------------
015400011003     D DSTA01        E DS
015500970117     D*------------------
015600970415     D* DS SCOMPOSIZIONE DESCRIZIONE VIDEO
015700970117     D*------------------
015800970117     D* DDEC,5
015900970117     D                 DS
016000970117     D  D05RCT                11     25
016100970117     D  DDEC05                 1     25
016200970415     D*------------------
016300970415     D* DS SCOMPOSIZIONE MESSAGGI
016400970415     D*------------------
016500970415     D* MSG,23
016600970415     D                 DS
016700970415     D  MSGDFI                51     53
016800970415     D  MSGDFD                55     55
016900970415     D  MSGM23                 1     78
017000970117     D*------------------
017100970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
017200970117     D*------------------
017300970117     D WLBDA8          DS
017400970117     D  G08DAT                 1      8  0
017500970117     D  G08INV                 9     16  0
017600970117     D  G08ERR                17     17
017700970117     D  G08TGI                18     22  0
017800970117     D*------------------
017900020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
018000970117     D*------------------
018100970117     D CARCNT          DS
018200970117     D  FL12CN                 1      2
018300970117     D  FL34CN                 3      4
018400970117     D  TIPCNT                 5      6
018500020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
018600970117     D  FLU                   66     97
018700970117     D                                     DIM(32)                              FLAG PAR.UTENT
018800970117     D  TCU                  398    697
018900970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
019000970117     D  KCU                  698    847P 0
019100970117     D                                     DIM(50)                              *TAB CAPICONTO
019200970117     D                                     PACKEVEN
019300970117     D TCUDS           DS
019400970117     D  E1                     1      1
019500970117     D  E3                     3      3
019600970117     D  E2                     2      2
019700970117     D  E4                     4      4
019800970117     D  E56                    5      6
019900970117     D*-------------------
020000970117     D* ARCHITETTURA
020100970117     D*-------------------
020200970117     D KPJBA         E DS
020300020729     D*-------------------
020400020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
020500020729     D*-------------------
020600020729     D OG143         E DS
020700020729     D*------------------
020800020729     D* DS PER LETTURA TABELLA "NTW"
020900020729     D*------------------
021000020729     D DNTW          E DS
021100970117     C*------------------------------------------------------------------------*
021200970117     C* MAIN LINE
021300970117     C*------------------------------------------------------------------------*
021400970117     C*
021500970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
021600970117IF  1C     $FINE         IFNE      'S'
021700970117     C*
021800970117     C* IMPOSTAZIONI PRIMO SUBFILE
021900970117     C                   EXSR      IMPSF1
022000970117     C*
022100970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
022200970117DO  2C     $FINE         DOUEQ     'S'
022300970117     C     TIPVID        CASEQ     '1'           GESSF1                         *1° VIDEO
022400970129     C     TIPVID        CASEQ     '2'           GESSF2                         *2° VIDEO
022500970129     C     TIPVID        CASEQ     '3'           GESSF3                         *3° VIDEO
022600970117     C                   ENDCS
022700970117E   2C                   ENDDO
022800970117E   1C                   ENDIF
022900970117     C*
023000970117     C                   SETON                                        LR
023100970117     C*------------------------------------------------------------------------*
023200970117     C* IMPSF1 - IMPOSTAZIONI INIZIALI PRIMO SUBFILE
023300970117     C*------------------------------------------------------------------------*
023400970117     C     IMPSF1        BEGSR
023500970117     C*
023600970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
023700970129     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
023800970117     C                   EXSR      SETSF1                                       *SFILE
023900970117     C* CARICA IL SUBFILE
024000970117     C                   EXSR      POSSF1
024100970117     C*
024200970117     C                   ENDSR
024300970117     C*------------------------------------------------------------------------*
024400970117     C* SETSF1 - IMPOSTAZIONI INDICATORI PRIMO SUBFILE
024500970117     C*------------------------------------------------------------------------*
024600970117     C     SETSF1        BEGSR
024700970117     C*
024800970117     C                   SETOFF                                       29         ERRORI CONTROL
024900970117     C                   SETOFF                                       222324     ERRORI SFILE
025000970117     C                   SETOFF                                       90         GENERICO DI ERRORE
025100970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
025200970117     C*
025300970117     C                   ENDSR
025400970117     C*------------------------------------------------------------------------*
025500970117     C* GESSF1 - GESTIONE PRIMO SUBFILE
025600970117     C*------------------------------------------------------------------------*
025700970117     C     GESSF1        BEGSR
025800970117     C*
025900970117     C* EMISSIONE VIDEATA
026000970117IF  1C     $TESVD        IFEQ      '1'
026100970117     C                   WRITE     TE03D00                                      *TESTATA
026200970117     C                   MOVEL     '0'           $TESVD
026300970117E   1C                   ENDIF
026400970117     C                   WRITE     TE03Z01                                      *PIEDI
026500970117     C                   EXFMT     TE03C01                                      *SFILE
026600970117     C                   EXSR      SETSF1
026700970117     C* F03-FINE
026800970117IF  1C     *INKC         IFEQ      *ON
026900970117     C                   EXSR      F03GES
027000970117     C                   GOTO      FINSF1
027100970117E   1C                   ENDIF
027200970117     C* F12-RITORNO
027300970117IF  1C     *INKL         IFEQ      *ON
027400970117     C                   EXSR      F12GES
027500970117     C                   GOTO      FINSF1
027600970117E   1C                   ENDIF
027700970117     C* F05-CARICA DA ALTRA TARIFFA
027800970117IF  1C     *INKE         IFEQ      *ON
027900970117     C                   EXSR      F05GES
028000970117     C                   GOTO      FINSF1
028100970117E   1C                   ENDIF
028200970117     C* F07-RAPPRESENTAZIONE
028300970117IF  1C     *INKG         IFEQ      *ON
028400970117     C                   EXSR      F07GES
028500970117     C                   GOTO      FINSF1
028600970117E   1C                   ENDIF
028700970117     C* CONTROLLA VIDEO
028800970117     C                   EXSR      CTRSF1
028900970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
029000970117     C                   GOTO      FINSF1
029100970117E   1C                   ENDIF
029200970117     C* INVIO-AGGIORNA
029300970124     C                   EXSR      ENTSF1
029400970117     C* F06-AGGIORNA E PROSEGUE
029500970117IF  1C     *INKF         IFEQ      *ON
029600970117     C                   EXSR      F06GES
029700970117E   1C                   ENDIF
029800970117     C*
029900970117     C     FINSF1        ENDSR
030000970117     C*------------------------------------------------------------------------*
030100970117     C* CTRSF1 - CONTROLLA PRIMO SUBFILE
030200970117     C*------------------------------------------------------------------------*
030300970117     C     CTRSF1        BEGSR
030400970117     C*
030500970117     C* REIMPOSTA VARIABILI DI LAVORO
030600970117     C                   MOVEL     '0'           WERR              1            *ERRORE
030700970117     C                   Z-ADD     *ZEROS        V1CPPE                         *TOTALE PESI %
030800970117     C*
030900970117     C* LEGGE TUTTI I RECORD DEL SFILE
031000970117     C                   Z-ADD     1             NR1
031100970117     C     NR1           CHAIN     TE03S01                            98
031200970117DO  1C     *IN98         DOWEQ     *OFF
031300970117     C                   SETOFF                                       70        *NO SFLNXTCHG
031400970117     C*--------------------
031500970117     C* RAGGRUPPAMENTO
031600970117     C*--------------------
031700970117IF  2C     WERR          IFEQ      '0'
031800970117     C                   MOVEL     V1SCTS        WCTS
031900970117     C                   MOVEL     V1SRCT        WRCT
032000970117     C                   MOVEL     *BLANKS       WDRCT
032001050304     c                   clear                   $msg
032100970117     C                   EXSR      CHKRCT
032200970117     C                   MOVEL     WRCT          V1SRCT
032300970117     C                   MOVEL     WDRCT         V1SDRC
032400970117IF  3C     WERR          IFEQ      '1'
032500970117     C                   SETON                                        2290
032501050304     c                   if        $msg = *blanks
032600970117     C                   MOVEL     MSG(2)        $MSG
032700970117E   3C                   ENDIF
032701050304E   3C                   ENDIF
032800970117E   2C                   ENDIF
032900970117     C*--------------------
033000970117     C* %INCIDENZA
033100970117     C*--------------------
033200970117IF  2C     WERR          IFEQ      '0'
033300970117IF  3C     V1SRCT        IFNE      *BLANKS                                      *DA CALCOLARE
033400970117IF  4C     V1SPPE        IFEQ      *ZEROS                                       *%VUOTA
033500970117     C                   SETON                                        2390
033600970117     C                   MOVEL     MSG(3)        $MSG
033700970117     C                   MOVEL     '1'           WERR
033800970117E   4C                   ENDIF
033900970117X   3C                   ELSE                                                   *DA NON CALCOLARE
034000970117IF  4C     V1SPPE        IFGT      *ZEROS                                       *%PIENA
034100970117     C                   SETON                                        2390
034200970117     C                   MOVEL     MSG(4)        $MSG
034300970117     C                   MOVEL     '1'           WERR
034400970117E   4C                   ENDIF
034500970117E   3C                   ENDIF
034600970117E   2C                   ENDIF
034700970117     C*--------------------
034800970117     C* OPZIONE
034900970117     C*--------------------
035000970117IF  2C     WERR          IFEQ      '0'
035100970117IF  3C     V1SOPZ        IFEQ      *BLANKS
035200970117     C     V1SOPZ        OREQ      '1 '
035300970117X   3C                   ELSE
035400970117     C                   MOVEL     '1'           WERR
035500970117     C                   SETON                                        2490
035600970117     C                   MOVEL     MSG(11)       $MSG
035700970117E   3C                   ENDIF
035800970117E   2C                   ENDIF
035900970117     C* NO ERRORE
036000970117IF  2C     WERR          IFEQ      '0'
036100970117     C                   EXSR      AGGVCT                                       AGGIORNA COD.TAS.REG
036200970117     C                   EXSR      CTRVCT                                       CONTROLLA COD.TAS.RE
036300970117     C                   EXSR      GESOP1                                       GESTISCE OPZIONI
036400970117E   2C                   ENDIF
036500970121     C*
036600970317     C     V1CPPE        ADD       V1SPPE        V1CPPE                         *TOTALE PESI %
036700970129     C     NR1           CHAIN     TE03S01                            98        *RILEGGE COD.TASS.
036800970129     C                   EXSR      AGGSF1                                       *AGGIORNA SFILE
036900970117     C*
037000970117IF  2C     WERR          IFEQ      '1'                                          E R R O R E
037100970117     C                   Z-ADD     NR1           $NR1                           *POSIZIONAMENTO
037200970117     C                   GOTO      FINCT1                                       *ESCE
037300970117X   2C                   ELSE                                                   N O   E R R O R E
037400970117     C                   ADD       1             NR1                            *LEGGE ALTRO RECORD
037500970117     C     NR1           CHAIN     TE03S01                            98
037600970117E   2C                   ENDIF
037700970117E   1C                   ENDDO                                                  *FINE SUBFILE
037800970117     C*
037900970117     C     FINCT1        ENDSR
038000970117     C*------------------------------------------------------------------------*
038100970117     C* AGGVCT - AGGIORNA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
038200970117     C*------------------------------------------------------------------------*
038300970117     C     AGGVCT        BEGSR
038400970415     C*
038500970415     C* REIMPOSTA LE VARIABILI DI LAVORO
038600970415     C                   MOVEL     '0'           WERR
038700970415     C                   Z-ADD     *ZEROS        TOTPPM            4 1          *% MINIMA IPOTETICA
038800970203     C*
038900970117     C* AGGIORNA IL RAGGRUPPAMENTO DEI CODICI TASSAZIONE DELLA REGIONE
039000970117IF  1C     V1SRCT        IFNE      'M'                                          *NO SITUAZIONE MISTA
039100970117DO  2C     1             DO        WUVCTS        I
039200970117IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
039300970117IF  4C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
039400970117     C                   MOVEL     V1SRCT        VRCT(I)                        *RAGGRUPPAMENTO
039500970117IF  5C     V1SRCT        IFEQ      *BLANKS                                      *NON CALCOLARE
039600970117     C                   Z-ADD     *ZEROS        VPPE(I)                        *%INCIDENZA=0
039700970415X   5C                   ELSE
039800970415     C                   ADD       0,1           TOTPPM                         *%INCIDENZA IPOTETIC
039900970117E   5C                   ENDIF
040000970117E   4C                   ENDIF
040100970117E   3C                   ENDIF
040200970117E   2C                   ENDDO
040300970128E   1C                   ENDIF
040400970415     C*---
040500970415     C* CONTROLLA LA %INCIDENZA MINIMA DA RICHIERE
040600970415     C*---
040700970415     C* TOTALE %REGIONE >= SOMMA % MINIMA (0,1) DEI COD.TASS.ELEM. DA CALCOLARE
040800970415IF  1C     V1SPPE        IFLT      TOTPPM
040900970415     C                   MOVEL     '1'           WERR
041000970415     C                   MOVEL     MSG(23)       MSGM23
041100970415     C                   MOVEL     TOTPPM        MSGDFI
041200970415     C                   MOVE      TOTPPM        MSGDFD
041300970415     C                   MOVEL     MSGM23        $MSG
041400970415     C                   SETON                                        2390
041500970415E   1C                   ENDIF
041600970415     C*---
041700970415     C* DALLA STESSA TARIFFA
041800970415     C*---
041900970124     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
042000970415IF  1C     WERR          IFEQ      '0'
042100970124     C                   Z-ADD     *ZEROS        TOTPPE            4 1
042200970415IF  2C     V1SRCT        IFNE      ' '                                          *NON CALCOLARE
042300050103DO  3C     1             DO        600           I
042400970415IF  4C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
042500970415IF  5C     V1SCTS        IFEQ      VRAP(I)                                      *STESSA REGIONE
042600970124     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
042700970415E   5C                   ENDIF
042800970415E   4C                   ENDIF
042900970415E   3C                   ENDDO
043000970124     C*
043100970415     C* CALCOLA LE % DI INCIDENZA DEI CODICI TASSAZIONE DELLA REGIONE
043200970415IF  3C     TOTPPE        IFGT      *ZEROS
043300970415DO  4C     1             DO        WUVCTS        I
043400970415IF  5C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
043500970415IF  6C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
043600970415IF  7C     VPPE(I)       IFGT      *ZEROS
043700970129     C     VPPE(I)       MULT      V1SPPE        WN156            15 6
043800970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
043900970415IF  8C     VPPE(I)       IFEQ      *ZEROS
044000970319     C                   Z-ADD     0,1           VPPE(I)
044100970415E   8C                   ENDIF
044200970415E   7C                   ENDIF
044300970415E   6C                   ENDIF
044400970415E   5C                   ENDIF
044500970415E   4C                   ENDDO
044600970415X   3C                   ELSE
044700970124     C*---
044800970203     C* DA ALTRA TARIFFA -SE VUOTA-
044900970124     C*---
045000970117     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
045100970124     C                   Z-ADD     *ZEROS        TOTPPE            4 1
045200050103DO  4C     1             DO        600           I
045300970415IF  5C     CCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
045400970415IF  6C     V1SCTS        IFEQ      CRAP(I)                                      *STESSA REGIONE
045500970117     C                   ADD       CPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
045600970415E   6C                   ENDIF
045700970415E   5C                   ENDIF
045800970415E   4C                   ENDDO
045900970129     C*
046000970117     C* CALCOLA LE % DI INCIDENZA DI CODICI TASSAZIONE DELLA REGIONE
046100970415IF  4C     TOTPPE        IFGT      *ZEROS
046200970415DO  5C     1             DO        WUVCTS        I
046300970415IF  6C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
046400970415IF  7C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
046500970117     C                   SETOFF                                           98
046600970117     C                   Z-ADD     1             J
046700970117     C     VCTS(I)       LOOKUP    CCTS(J)                                98
046800970415IF  8C     *IN98         IFEQ      *ON
046900970129     C     CPPE(J)       MULT      V1SPPE        WN156
047000970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
047100970415IF  9C     VPPE(I)       IFEQ      *ZEROS
047200970319     C                   Z-ADD     0,1           VPPE(I)
047300970415E   9C                   ENDIF
047400970415E   8C                   ENDIF
047500970415E   7C                   ENDIF
047600970415E   6C                   ENDIF
047700970415E   5C                   ENDDO
047800970415X   4C                   ELSE
047900970129     C*---
048000970415     C* DA CARTELLO -SE VUOTA LA TARIFFA-
048100970129     C*---
048200970129     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
048300970129     C                   Z-ADD     *ZEROS        TOTPPE            4 1
048400050103DO  5C     1             DO        600           I
048500970415IF  6C     CTS8(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
048600970415IF  7C     V1SCTS        IFEQ      RAP8(I)                                      *STESSA REGIONE
048700970415     C                   ADD       PPE8(I)       TOTPPE                         *%INCIDENZA REGIONAL
048800970415E   7C                   ENDIF
048900970415E   6C                   ENDIF
049000970415E   5C                   ENDDO
049100970129     C*
049200970129     C* CALCOLA LE % DI INCIDENZA DI CODICI TASSAZIONE DELLA REGIONE
049300970415IF  5C     TOTPPE        IFGT      *ZEROS
049400970415DO  6C     1             DO        WUVCTS        I
049500970415IF  7C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
049600970415IF  8C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
049700970129     C                   SETOFF                                           98
049800970129     C                   Z-ADD     1             J
049900970129     C     VCTS(I)       LOOKUP    CTS8(J)                                98
050000970415IF  9C     *IN98         IFEQ      *ON
050100970129     C     PPE8(J)       MULT      V1SPPE        WN156
050200970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
050300970415IF 10C     VPPE(I)       IFEQ      *ZEROS
050400970319     C                   Z-ADD     0,1           VPPE(I)
050500970415E  10C                   ENDIF
050600970415E   9C                   ENDIF
050700970415E   8C                   ENDIF
050800970415E   7C                   ENDIF
050900970415E   6C                   ENDDO
051000970415     C*---
051100970415     C* IMPOSTA 0,1 -SE VUOTA LA CARTELLO-
051200970415     C*---
051300970415X   5C                   ELSE
051400970415DO  6C     1             DO        WUVCTS        I
051500970415IF  7C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
051600970415IF  8C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
051700970415IF  9C     VRCT(I)       IFNE      *BLANKS                                      *DA CALCOLARE
051800970415     C                   Z-ADD     0,1           VPPE(I)                        *%INCIDENZA MINIMA
051900970415E   9C                   ENDIF
052000970415E   8C                   ENDIF
052100970415E   7C                   ENDIF
052200970415E   6C                   ENDDO
052300970415E   5C                   ENDIF
052400970129     C*
052500970415E   4C                   ENDIF
052600970415E   3C                   ENDIF
052700970128     C*---
052800970128     C* PAREGGIA LA %REGIONE CON LA %TOTALE DI TUTTI I CODICI TASSAZIONE
052900970319     C*---
053000970128     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
053100970409     C                   Z-ADD     *ZEROS        TOTPPE            4 1
053200050103DO  3C     1             DO        600           I
053300970415IF  4C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
053400970415IF  5C     V1SCTS        IFEQ      VRAP(I)                                      *STESSA REGIONE
053500970409     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
053600970415E   5C                   ENDIF
053700970415E   4C                   ENDIF
053800970415E   3C                   ENDDO
053900970414     C*
054000970319     C* CALCOLA LA DIFFERENZA FRA LA %REGIONALE E LA % DEI COD.TASSAZIONE (+/-)
054100970409     C                   Z-ADD     *ZEROS        WN41              4 1
054200970409     C     V1SPPE        SUB       TOTPPE        WN41
054300970415     C*
054400970415     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
054500970415IF  3C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
054600970415DO  4C     WN41          DOWNE     *ZEROS
054700970415DO  5C     1             DO        WUVCTS        I
054800970415IF  6C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
054900970415IF  7C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
055000970415IF  8C     VRCT(I)       IFNE      *BLANKS                                      *DA CALCOLARE
055100970415     C* POSITIVA -> AGGIUNGERE
055200970415IF  9C     WN41          IFGT      *ZEROS
055300970415     C     VPPE(I)       ADD       0,1           VPPE(I)                        *AGGIUNGE %
055400970415     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
055500970415E   9C                   ENDIF
055600970415     C* NEGATIVA -> TOGLIERE
055700970415IF  9C     WN41          IFLT      *ZEROS
055800970415IF 10C     VPPE(I)       IFGT      0,1
055900970415     C     VPPE(I)       SUB       0,1           VPPE(I)                        *SOTTRAE %
056000970415     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
056100970415E  10C                   ENDIF
056200970415E   9C                   ENDIF
056300970415     C* UGUALE A ZERO -> USCIRE
056400970415IF  9C     WN41          IFEQ      *ZEROS
056500970415     C                   LEAVE
056600970415E   9C                   ENDIF
056700970415E   8C                   ENDIF
056800970415E   7C                   ENDIF
056900970415E   6C                   ENDIF
057000970415E   5C                   ENDDO                                                  *FINE COD.TASS.ELEM.
057100970415E   4C                   ENDDO                                                  *DIFFERENZA = 0
057200970415E   3C                   ENDIF
057300970414     C*
057400970415E   2C                   ENDIF
057500970319E   1C                   ENDIF
057600970128     C*
057700970117     C                   ENDSR
057800970117     C*------------------------------------------------------------------------*
057900970117     C* CTRVCT - CONTROLLA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
058000970117     C*------------------------------------------------------------------------*
058100970117     C     CTRVCT        BEGSR
058200970117     C*
058300970415IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
058400970415     C                   Z-ADD     *ZEROS        TOTPPE            4 1          *% TOTALE
058500970415DO  2C     1             DO        WUVCTS        I
058600970415IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
058700970415IF  4C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
058800970117     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA TOTALE
058900970129     C* DA NON CALCOLARE
059000970415IF  5C     V1SRCT        IFEQ      *BLANKS
059100970415IF  6C     WERR          IFEQ      '0'
059200970415IF  7C     VRCT(I)       IFNE      *BLANKS
059300970128     C                   MOVEL     '1'           WERR
059400970128     C                   MOVEL     MSG(6)        $MSG
059500970128     C                   SETON                                        2290
059600970415E   7C                   ENDIF
059700970415E   6C                   ENDIF
059800970128     C*
059900970415IF  6C     WERR          IFEQ      '0'
060000970415IF  7C     VPPE(I)       IFGT      *ZEROS
060100970117     C                   MOVEL     '1'           WERR
060200970128     C                   MOVEL     MSG(7)        $MSG
060300970117     C                   SETON                                        2290
060400970415E   7C                   ENDIF
060500970415E   6C                   ENDIF
060600970415X   5C                   ELSE
060700970129     C* DA CALCOLARE
060800970415IF  6C     V1SRCT        IFNE      'M'                                          *NO SITUAZIONE MISTA
060900970415IF  7C     WERR          IFEQ      '0'
061000970415IF  8C     VRCT(I)       IFNE      V1SRCT
061100970117     C                   MOVEL     '1'           WERR
061200970128     C                   MOVEL     MSG(8)        $MSG
061300970117     C                   SETON                                        2290
061400970415E   8C                   ENDIF
061500970415E   7C                   ENDIF
061600970415IF  7C     WERR          IFEQ      '0'
061700970415IF  8C     VPPE(I)       IFEQ      *ZEROS
061800970129     C                   MOVEL     '1'           WERR
061900970129     C                   MOVEL     MSG(9)        $MSG
062000970129     C                   SETON                                        2290
062100970415E   8C                   ENDIF
062200970415E   7C                   ENDIF
062300970129     C*
062400970415X   6C                   ELSE                                                   *SITUAZIONE MISTA
062500970129     C*
062600970415IF  7C     WERR          IFEQ      '0'
062700970415IF  8C     VRCT(I)       IFNE      *BLANKS
062800970129     C     VPPE(I)       ANDEQ     *ZEROS
062900970129     C                   MOVEL     '1'           WERR
063000970129     C                   MOVEL     MSG(21)       $MSG
063100970129     C                   SETON                                        2290
063200970415E   8C                   ENDIF
063300970415E   7C                   ENDIF
063400970128     C*
063500970415IF  7C     WERR          IFEQ      '0'
063600970415IF  8C     VRCT(I)       IFEQ      *BLANKS
063700970129     C     VPPE(I)       ANDGT     *ZEROS
063800970128     C                   MOVEL     '1'           WERR
063900970129     C                   MOVEL     MSG(22)       $MSG
064000970129     C                   SETON                                        2290
064100970415E   8C                   ENDIF
064200970415E   7C                   ENDIF
064300970415E   6C                   ENDIF
064400970415E   5C                   ENDIF
064500970117     C*
064600970415E   4C                   ENDIF
064700970415E   3C                   ENDIF
064800970415E   2C                   ENDDO
064900970117     C*
065000970117     C* TOTALE %INCIDENZA CODICI TASSAZIONE = %INCIDENZA REGIONE
065100970117IF  2C     V1SPPE        IFNE      TOTPPE
065200970117     C                   MOVEL     '1'           WERR
065300970128     C                   MOVEL     MSG(10)       $MSG
065400970117     C                   SETON                                        2390
065500970117E   2C                   ENDIF
065600970117E   1C                   ENDIF
065700970117     C*
065800970117     C                   ENDSR
065900970117     C*------------------------------------------------------------------------*
066000970117     C* GESOP1 - GESTIONE OPZIONI DEL PRIMO SUBFILE
066100970117     C*------------------------------------------------------------------------*
066200970117     C     GESOP1        BEGSR
066300970122     C*
066400970129     C                   MOVEL     V1SOPZ        DEPOPZ
066500970129     C                   EXSR      AGGSF1                                       *AGGIORNA SFILE
066600970117SL  1C                   SELECT
066700970129     C     DEPOPZ        WHENEQ    '1 '
066800970117     C                   EXSR      O01GE1                                       *CODICI TASSAZIONE
066900970117     C                   OTHER
067000970117E   1C                   ENDSL
067100970117     C*
067200970117     C                   ENDSR
067300970117     C*------------------------------------------------------------------------*
067400970117     C* O01GE1 - GESTIONE OPZIONE PRIMO SFILE 1 ==> CODICI TASSAZIONE ELEMENTARI
067500970117     C*------------------------------------------------------------------------*
067600970117     C     O01GE1        BEGSR
067700970117     C*
067800970117     C* GESTIONE 3° SFILE
067900970117     C                   Z-ADD     NR1           $DPC1                          *DEPOSITO NRR
068000970117     C                   EXSR      IMPSF3                                       *IMPOSTAZIONI
068100970129DO  1C     $FINV         DOUEQ     'S'
068200970117     C                   EXSR      GESSF3                                       *GESTIONE
068300970129E   1C                   ENDDO
068400970129     C                   Z-ADD     $DPC1         NR1                            *DEPOSITO NRR
068500970117     C*
068600970117     C                   ENDSR
068700970117     C*------------------------------------------------------------------------*
068800970117     C* AGGSF1 - AGGIORNA RIGA DEL PRIMO SUBFILE
068900970117     C*------------------------------------------------------------------------*
069000970117     C     AGGSF1        BEGSR
069100970117     C*
069200970124     C                   MOVEL     *BLANKS       V1SOPZ
069300970117     C                   SETON                                        70        *SI SFLNXTCHG
069400970117IF  1C     $TGES         IFEQ      'I'
069500970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
069600970117X   1C                   ELSE
069700970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
069800970117E   1C                   ENDIF
069900970117IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
070000970117     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
070100970127X   1C                   ELSE                                                   *DA PORTO
070200970117     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
070300970117E   1C                   ENDIF
070400970117     C                   UPDATE    TE03S01                                      *AGGIORNA SUBFILE
070500970117     C                   SETOFF                                       70        *NO SFLNXTCHG
070600970117     C*
070700970117     C                   ENDSR
070800970117     C*------------------------------------------------------------------------*
070900970124     C* ENTSF1 - GESTIONE TASTO FUNZIONALE INVIO 1°SFILE  ==> AGGIORNA
071000970117     C*------------------------------------------------------------------------*
071100970124     C     ENTSF1        BEGSR
071200970117     C*
071300970117IF  1C     $TGES         IFEQ      'G'                                          *SE GESTIONE
071400970117     C* AGGIORNA L'ARCHIVIO
071500980508     C*---                 EXSR AGGETT
071600970129     C*
071700970129     C* AGGIORNA LE SCHIERE VIDEO
071800970129     C                   EXSR      AGGRCT
071900970129     C*
072000970117     C* CARICA IL SUBFILE
072100970117     C                   EXSR      POSSF1
072200970117E   1C                   ENDIF
072300970117     C*
072400970117     C                   ENDSR
072500970117     C*------------------------------------------------------------------------*
072600970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
072700970117     C*------------------------------------------------------------------------*
072800970117     C     F03GES        BEGSR
072900970117     C*
073000970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
073100970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
073200011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
073300970117     C*
073400970117     C                   ENDSR
073500970117     C*------------------------------------------------------------------------*
073600970117     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
073700970117     C*------------------------------------------------------------------------*
073800970117     C     F05GES        BEGSR
073900970117     C*
074000970117     C* GESTIONE VIDETA
074100970117     C                   EXSR      IMPWD2
074200970117DO  1C     $FINV         DOUEQ     'S'
074300970117     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
074400970117E   1C                   ENDDO
074500970117     C*
074600970117     C* MEMORIZZA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA E CARICA IL SFILE
074700970117IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
074900980708     C                   MOVEL     V2WCTO        WCTO
075000980708     C                   Z-ADD     V2WKSC        WKSC
075100980708     C                   Z-ADD     V2WCTR        WCTR
075200980708     C                   Z-ADD     V2WPRG        WPRG
075300980708     C                   Z-ADD     V2WLNP        WLNP
075400980708     C                   MOVEL     D00FIE        WFIE
075500011003     C                   MOVEL     V2WDIV        WDIV
075600970117     C                   EXSR      MEMETT                                       *MEMORIZZA
075700970117     C                   EXSR      POSSF1                                       *CARICA
075800091123     C                   MOVEL     'S'           D0DDSF                         *DEPOSITA CHIAVE CAR
075900980708     C                   MOVEL     V2WCTO        D0DCTO
076000980708     C                   Z-ADD     V2WKSC        D0DKSC
076100980708     C                   Z-ADD     V2WCTR        D0DCTR
076200980708     C                   Z-ADD     V2WPRG        D0DPRG
076300980708     C                   Z-ADD     V2WLNP        D0DLNP
076400011003     C                   MOVEL     V2WDIV        D0DDIV
076500970117E   1C                   ENDIF
076600970117     C*
076700970117     C                   ENDSR
076800970117     C*------------------------------------------------------------------------*
076900970117     C* MEMETT - MEMORIZZA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
077000970117     C*------------------------------------------------------------------------*
077100970117     C     MEMETT        BEGSR
077200970117     C*
077300970117     C* CARICA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
077400970117     C                   EXSR      CARETT
077500980508     C*
077600980508     C* AGGIORNA LE SCHIERE VIDEO
077700980508     C                   EXSR      AGGSCV
077800980508     C*
077900980508     C                   ENDSR
078000980508     C*------------------------------------------------------------------------*
078100970117     C* CARETT - CARICA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
078200970117     C*------------------------------------------------------------------------*
078300970117     C     CARETT        BEGSR
078400980508     C*
078500980508     C* VALORIZZA I CODICI TASSAZIONE CON QUELLI CARICATI DA TABELLA "CT"
078600980508     C                   MOVEL     VCTS          CCTS
078700980514     C                   MOVEL     VRAP          CRAP
078800970117     C*
078900980508     C* PULISCE LE SCHIERE DI MEMORIZZAZIONE
079000980508     C                   CLEAR                   CRCT                           *SCHIERE CARICAMENTO
079100970117     C                   CLEAR                   CPPE
079200970117     C*
079300980508     C* CARICA DATI CODICI TASSAZIONE
079400091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
079500980708     C                   MOVEL     WCTO          KTTCTO                         *TARIFFA O OFFERTA
079600980708     C                   Z-ADD     WKSC          KTTKSC                         *CLIENTE O VISITA
079700980708     C                   Z-ADD     WCTR          KTTCTR                         *TARIFFA
079800980708     C                   Z-ADD     WPRG          KTTPRG                         *PROGRESSIVO
079900980708     C                   Z-ADD     WLNP          KTTLNP                         *LINEA DI PARTENZA
080000980708     C                   MOVEL     WFIE          KTTFIE                         *ITALIA/ESTERO
080100011001     C     KE2ETT        SETLL     TEETT01L                               99
080200011001     C     KE2ETT        READE(N)  TEETT01L                               99
080300970117DO  1C     *IN99         DOWEQ     *OFF
080400980508IF  2C     ETTATB        IFEQ      *BLANKS                                      *NON ANNULLATO
080500980508     C                   SETOFF                                           98
080600980508     C                   Z-ADD     1             J
080700980515     C     ETTCTS        LOOKUP    CCTS(J)                                98
080800980508IF  3C     *IN98         IFEQ      *ON                                          *CODICE TROVATO
080900980508     C                   MOVEL     ETTRCT        CRCT(J)                        *RAGGRUPPAMENTO
081000980508     C                   Z-ADD     ETTPPE        CPPE(J)                        *%INCIDENZA
081100980911     C     ETTRCT        IFEQ      'N'
081200980911     C     ETTPPE        ANDEQ     *ZEROS
081300980911     C                   MOVEL     *BLANKS       CRCT(J)
081400980911     C                   ENDIF
081500970129E   3C                   ENDIF
081600970129E   2C                   ENDIF
081700011001     C     KE2ETT        READE(N)  TEETT01L                               99
081800970117E   1C                   ENDDO
081900980508     C*
082000970117     C                   ENDSR
082100980508     C*------------------------------------------------------------------------*
082200980508     C* CARSTD - CARICA I CODICI TASSAZIONE DA TABELLA
082300980508     C*------------------------------------------------------------------------*
082400980508     C     CARSTD        BEGSR
082500980508     C*
082600980508     C* VALORIZZA I CODICI TASSAZIONE CON QUELLI CARICATI DA TABELLA "CT"
082700980708     C                   MOVEL     VCTS          CCTS
082800980708     C                   MOVEL     VRAP          CRAP
082900980514     C                   MOVEL     'T'           CRCT
083000980508     C                   Z-ADD     000,1         CPPE
083100980508     C*
083200980508     C                   ENDSR
083300980508     C*------------------------------------------------------------------------*
083400980508     C* AGGSCV - AGGIORNA LE SCHIERE VIDEO
083500980508     C*------------------------------------------------------------------------*
083600980508     C     AGGSCV        BEGSR
083700980508     C*
083800980508     C* PULISCE LE VARIABILI DI LAVORO
084000980508     C                   CLEAR                   VRCT                           *SCHIERE VIDEO
084100980902     C                   CLEAR                   VPPE
084300980508     C*
084400980508     C* MEMORIZZA I DATI TIPO DELLA TARIFFA RICHIESTA NELLE SCHIERE VIDEO
084500980508DO  1C     1             DO        WUVCTS        J
084600980508IF  2C     CCTS(J)       IFNE      *BLANKS
084700980508     C                   SETOFF                                           98
084800980508     C                   Z-ADD     1             I
084900980508     C     CCTS(J)       LOOKUP    VCTS(I)                                98
085000980508IF  3C     *IN98         IFEQ      *ON
085001050304     c* verifico se per il cod. tassazione esiste un raggruppam. imposto
085002050304     C                   MOVEL     CRCT(J)       VRCT(I)                        *RAGGRUPPAMENTO
085003050304     c                   if        crct(j) <> ' '
085004050304     c                   z-add     1             bb                3 0
085005050304     c     ccts(j)       lookup    cts(bb)                                51
085006050304     c                   if        *in51
085007050304     c                             and rct(bb) <> *blanks
085008050304     C                   MOVEL     rct(bb)       vrct(i)
086101050304     c                   end
086102050304     c                   end
086103050304     C* PORTO -> CARICA LE %INCIDENZA E I RAGGRUPPAMENTI DI TUTTI I CODICI
086200980508     C                   Z-ADD     CPPE(J)       VPPE(I)                        *%INCIDENZA
086400980508E   3C                   ENDIF
086500980508E   2C                   ENDIF
086600980508E   1C                   ENDDO
086700980508     C*
086800980508     C* AGGIORNA I CODICI TASSAZIONE DELLA REGIONE
086900980508     C                   EXSR      AGGRCT
087000980508     C*
087100980508     C                   ENDSR
087200970117     C*------------------------------------------------------------------------*
087300970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
087400970117     C*------------------------------------------------------------------------*
087500970117     C     F06GES        BEGSR
087600970117     C*
087700970117IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
087800970117     C*---
087900970117     C* TOTALE %INCIDENZA = 100
088000970117     C*---
088100970117IF  2C     V1CPPE        IFNE      100,0
088200970117     C                   SETON                                        2990
088300970117     C                   MOVEL     MSG(5)        $MSG
088400970117     C                   MOVEL     '1'           WERR
088500970117     C                   Z-ADD     1             $NR1                           *INIZIO SFILE
088600970117E   2C                   ENDIF
088700970117     C* AGGIORNA ARCHIVIO
088800970117IF  2C     WERR          IFEQ      '0'                                          *NO ERRORI
088900970117     C                   EXSR      AGGETT
089000970129     C*
089100970129     C* AGGIORNA LE SCHIERE VIDEO
089200970129     C                   EXSR      AGGRCT
089300970117     C*---
089400970117     C* PORTO -> GESTIONE VIDETA DI CONFERMA
089500970117     C*---
089600970117IF  3C     D00TLA        IFEQ      'P'
089700970117     C                   EXSR      IMPWD3
089800970117DO  4C     $FINV         DOUEQ     'S'
089900970117     C                   EXSR      GESWD3                                       *GESTIONE FINESTRA
090000970117E   4C                   ENDDO
090100970117IF  4C     V3WRIS        IFEQ      'SI'                                         *SE CONFERMATO
090200970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
090300011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
090400970117E   4C                   ENDIF
090500970117     C*---
090600970117     C* DELTA -> ESCE E PROSEGUE
090700970117     C*---
090800970117X   3C                   ELSE
090900011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
091000970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
091100970117E   3C                   ENDIF
091200970117E   2C                   ENDIF
091300970117X   1C                   ELSE                                                   INTERROGAZIONE
091400011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
091500970130     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
091600970117E   1C                   ENDIF
091700970117     C*
091800970117     C                   ENDSR
091900970117     C*------------------------------------------------------------------------*
092000011001     C* AGGETT - AGGIORNAMENTO ARCHIVIO -TEETT-
092100970117     C*------------------------------------------------------------------------*
092200970124     C     AGGETT        BEGSR
092300980515     C*
092400980515     C* ANNULLA TUTTI I DATI DELLA TARIFFA IN LINEA
092500091123     C                   MOVEL     'S'           KTTDSF
092600980515     C                   MOVEL     D00CTO        KTTCTO
092700980515     C                   Z-ADD     D00KSC        KTTKSC
092800980515     C                   Z-ADD     D00CTR        KTTCTR
092900980515     C                   Z-ADD     D00PRG        KTTPRG
093000980515     C                   Z-ADD     D00LNP        KTTLNP
093100980515     C                   MOVEL     D00FIE        KTTFIE
093200011001     C     KE2ETT        SETLL     TEETT01L                               99
093300011001     C     KE2ETT        READE     TEETT01L                               99
093400980515DO  1C     *IN99         DOWEQ     *OFF
093500980515IF  2C     ETTDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
093600980515     C                   MOVEL     'A'           ETTATB
093700980515     C                   MOVEL     *BLANKS       ETTFTR
093800011001     C                   UPDATE    TEETT000                                     *AGGIORNA
093900980515X   2C                   ELSE                                                   *MAI TRASMESSO
094000011001     C                   DELETE    TEETT000                                     *CANCELLA
094100980515E   2C                   ENDIF
094200011001     C     KE2ETT        READE     TEETT01L                               99
094300980515E   1C                   ENDDO
094400011001     C                   UNLOCK    TEETT01L
094500970117     C*
094600970117     C* LEGGE TUTTI I RECORD DEL SFILE
094700970117DO  2C     1             DO        WUVCTS        I
094800970117IF  2C     VCTS(I)       IFNE      *BLANKS                                      *SE IMMESSO QC.
094900970117     C*
095000970117     C* LEGGE IL RECORD IN ARCHIVIO
095100091123     C                   MOVEL     'S'           KTTDSF
095200980508     C                   MOVEL     D00CTO        KTTCTO
095300970117     C                   Z-ADD     D00KSC        KTTKSC
095400970117     C                   Z-ADD     D00CTR        KTTCTR
095500970117     C                   Z-ADD     D00PRG        KTTPRG
095600970117     C                   Z-ADD     D00LNP        KTTLNP
095700970117     C                   MOVEL     D00FIE        KTTFIE
095800970117     C                   MOVEL     VCTS(I)       KTTCTS
095900011001     C     KEYETT        CHAIN     TEETT01L                           99
096000970117     C* INSERIMENTO
096100970117IF  3C     *IN99         IFEQ      *ON
096200970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
096300011001     C                   WRITE     TEETT000
096400970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZIONE
096500970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
096600970117X   3C                   ELSE
096700970117     C* VARIAZIONE
096800970117IF  4C     VRCT(I)       IFNE      ETTRCT                                       *SE MODIFICATO QC.
096900970117     C     VPPE(I)       ORNE      ETTPPE
097000970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
097100020715X   4C                   ELSE
097200020716     C                   MOVEL     *BLANKS       ETTFTR
097300020716     C                   Z-ADD     DATCOR        ETTDUV
097400020715     C                   MOVEL     *BLANKS       ETTATB
097500020715E   4C                   ENDIF
097600011001     C                   UPDATE    TEETT000
097700970117E   3C                   ENDIF
097800970117E   2C                   ENDIF
097900970117E   1C                   ENDDO                                                  *FINE SUBFILE
098000970117     C*
098100970117     C                   ENDSR
098200970117     C*------------------------------------------------------------------------*
098300011001     C* IMPETT - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETT-
098400970117     C*------------------------------------------------------------------------*
098500970117     C     IMPETT        BEGSR
098600970117     C*
098700970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
098800980508     C                   MOVEL     KTTDSF        ETTDSF
098900980508     C                   MOVEL     KTTCTO        ETTCTO
099000970117     C                   Z-ADD     KTTKSC        ETTKSC
099100970117     C                   Z-ADD     KTTCTR        ETTCTR
099200970117     C                   Z-ADD     KTTPRG        ETTPRG
099300970117     C                   Z-ADD     KTTLNP        ETTLNP
099400970117     C                   MOVEL     KTTFIE        ETTFIE
099500970117     C                   MOVEL     KTTCTS        ETTCTS
099600970117     C                   Z-ADD     DATCOR        ETTDIR
099700970117     C                   Z-ADD     *ZEROS        ETTDUV
099800970117     C                   MOVEL     *BLANKS       ETTFTR
099900970117     C                   Z-ADD     *ZEROS        ETTDTR
100000970117X   1C                   ELSE                                                   *VARIAZIONE
100100970117     C                   MOVEL     *BLANKS       ETTFTR
100200970117     C                   Z-ADD     DATCOR        ETTDUV
100300970117E   1C                   ENDIF
100400970117     C                   MOVEL     VRCT(I)       ETTRCT
100500970117     C                   Z-ADD     VPPE(I)       ETTPPE
100600970117     C                   MOVEL     *BLANKS       ETTATB
100700970117     C*
100800970117     C                   ENDSR
100900970117     C*------------------------------------------------------------------------*
101000970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
101100970117     C*------------------------------------------------------------------------*
101200970117     C     F12GES        BEGSR
101300970117     C*
101400970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
101500970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
101600011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
101700970117     C*
101800970117     C                   ENDSR
101900970117     C*------------------------------------------------------------------------*
102000970117     C* F07GES - GESTIONE TASTO FUNZIONALE F07 ==> RAPPRESENTAZIONE
102100970117     C*------------------------------------------------------------------------*
102200970117     C     F07GES        BEGSR
102300970117     C*
102400970117     C* SVILUPPA IL TIPO DI RAPPRESENTAZIONE
102500970117     C                   EXSR      SVIRAP
102600970117     C*
102700970117     C* GESTIONE VIDEATA
102800970117     C                   EXSR      IMPSF2
102900970117DO  1C     $FINV         DOUEQ     'S'
103000970117     C                   EXSR      GESSF2                                       *GESTIONE VIDEATA
103100970117E   1C                   ENDDO
103200970117     C*
103300970117     C                   ENDSR
103400970117     C*------------------------------------------------------------------------*
103500970117     C* SVIRAP - SVILUPPA IL TIPO DI RAPPRESENTAZIONE
103600970117     C*------------------------------------------------------------------------*
103700970117     C     SVIRAP        BEGSR
103800970117     C*
103900970117     C* PULISCE LE SCHIERE VIDEO DI OUTPUT
104000970117     C                   CLEAR                   OCTS                           *CODICE
104100970117     C                   CLEAR                   OPPE                           *%INCIDENZA
104200970117     C                   CLEAR                   OCOR                           *ORDINAMENTO
104300970117     C*
104400970117     C                   Z-ADD     *ZEROS        J                 4 0
104500970117DO  1C     1             DO        WUVCTS        I
104600970117IF  2C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
104700970117     C* DA CALCOLARE
104800970117IF  3C     VRCT(I)       IFEQ      'T'
104900970117     C                   ADD       1             J
105000970117     C                   MOVEL     VCTS(I)       OCTS(J)                        *CODICE
105100970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
105200970117     C                   MOVEL     VCOR(I)       OCOR(J)                        *ORDINAMENTO
105300970117X   3C                   ELSE
105400970117     C                   MOVEL     VCTS(I)       WCTS
105500970117     C                   MOVEL     *BLANKS       WRAP
105600970117     C                   MOVEL     *BLANKS       WNAZ
105700970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
105800970117     C* DA CALCOLARE IN REGIONE
105900970117IF  4C     VRCT(I)       IFEQ      'R'
106000970117     C                   SETOFF                                           98
106100970117     C                   Z-ADD     1             C                 4 0
106200970117     C     WRAP          LOOKUP    OCTS(C)                                98
106300970117IF  5C     *IN98         IFEQ      *OFF                                         *MAI  MEMORIZZATO
106400970117     C                   MOVEL     WRAP          WCTS
106500970117     C                   MOVEL     *BLANKS       WCOR
106600970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
106700970117     C                   ADD       1             J
106800970117     C                   MOVEL     WRAP          OCTS(J)                        *CODICE
106900970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
107000970117     C                   MOVEL     WCOR          OCOR(J)                        *ORDINAMENTO
107100970117X   5C                   ELSE                                                   *GIA' MEMORIZZATO
107200970117     C     VPPE(I)       ADD       OPPE(C)       OPPE(C)                        *SOMMA PESI
107300970117E   5C                   ENDIF
107400970117E   4C                   ENDIF
107500970117     C* DA CALCOLARE IN NAZIONE
107600970117IF  4C     VRCT(I)       IFEQ      'N'
107700970117     C                   SETOFF                                           98
107800970117     C                   Z-ADD     1             C                 4 0
107900970117     C     WNAZ          LOOKUP    OCTS(C)                                98
108000970117IF  5C     *IN98         IFEQ      *OFF                                         *MAI  MEMORIZZATO
108100970117     C                   MOVEL     WNAZ          WCTS
108200970117     C                   MOVEL     *BLANKS       WCOR
108300970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
108400970117     C                   ADD       1             J
108500970117     C                   MOVEL     WNAZ          OCTS(J)                        *CODICE
108600970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
108700970117     C                   MOVEL     WCOR          OCOR(J)                        *ORDINAMENTO
108800970117X   5C                   ELSE                                                   *GIA' MEMORIZZATO
108900970117     C     VPPE(I)       ADD       OPPE(C)       OPPE(C)                        *SOMMA PESI
109000970117E   5C                   ENDIF
109100970117E   4C                   ENDIF
109200970117E   3C                   ENDIF
109300970117E   2C                   ENDIF
109400970117E   1C                   ENDDO
109500970117     C                   Z-ADD     J             WUOCTS            4 0          *N°COD.TASS.OUTPUT
109600970117     C*
109700970117     C* ORDINA CODICI RAGGRUPPATI VIDEO
109800970117     C                   EXSR      ORDOCT
109900970117     C*
110000970117     C                   ENDSR
110100970117     C*------------------------------------------------------------------------*
110200970117     C* DATCTS - REPERISCE I DATI DEL CODICE TASSAZIONE
110300970117     C*------------------------------------------------------------------------*
110400970117     C     DATCTS        BEGSR
110500970117     C*
110600970117     C     *LIKE         DEFINE    ETTCTS        WCTS
110700970117     C     *LIKE         DEFINE    ETTCTS        WRAP
110800970117     C     *LIKE         DEFINE    ETTCTS        WNAZ
110900970117     C     *LIKE         DEFINE    §CTCOR        WCOR
111000970117     C*
111100970117     C                   SETOFF                                           98
111200970117     C                   Z-ADD     1             C                 4 0
111300970117     C     WCTS          LOOKUP    CTS(C)                                 98
111400970117IF  1C     *IN98         IFEQ      *ON
111500970117     C                   MOVEL     RAP(C)        WRAP                           *COD.TASS.REGIONE
111600970117     C                   MOVEL     NAZ(C)        WNAZ                           *COD.TASS.NAZIONE
111700970117     C                   MOVEL     COR(C)        WCOR                           *COD.ORDINAMENTO
111800970117E   1C                   ENDIF
111900970117     C*
112000970117     C                   ENDSR
112100970117     C*------------------------------------------------------------------------*
112200970117     C* ORDOCT - ORDINA I CODICI TASSAZIONE RAGGRUPPATI DA EMETTERE A VIDEO
112300970117     C*------------------------------------------------------------------------*
112400970117     C     ORDOCT        BEGSR
112500970117     C*
112600970117DO  1C     1             DO        WUOCTS        I
112700970117IF  2C     OCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
112800970117     C     1             ADD       I             J                 4 0
112900970117DO  3C     J             DO        WUOCTS        J
113000970117IF  4C     OCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
113100970117IF  5C     OCOR(I)       IFGT      OCOR(J)
113200970117     C                   MOVEL     OCTS(I)       PCTS                           *CODICE
113300970117     C                   MOVEL     OCTS(J)       OCTS(I)
113400970117     C                   MOVEL     PCTS          OCTS(J)
113500970117     C                   MOVEL     OPPE(I)       PPPE                           *%INCIDENZA
113600970117     C                   MOVEL     OPPE(J)       OPPE(I)
113700970117     C                   MOVEL     PPPE          OPPE(J)
113800970117     C                   MOVEL     OCOR(I)       PCOR                           *ORDINAMENTO
113900970117     C                   MOVEL     OCOR(J)       OCOR(I)
114000970117     C                   MOVEL     PCOR          OCOR(J)
114100970117E   5C                   ENDIF
114200970117E   4C                   ENDIF
114300970117E   3C                   ENDDO                                                  *FINE CICLO UNO
114400970117E   2C                   ENDIF
114500970117E   1C                   ENDDO                                                  *FINE CICLO DUE
114600970117     C*
114700970117     C                   ENDSR
114800970117     C*------------------------------------------------------------------------*
114900970117     C* IMPSF2 - IMPOSTAZIONI INIZIALI SECONDO SUBFILE
115000970117     C*------------------------------------------------------------------------*
115100970117     C     IMPSF2        BEGSR
115200970117     C*
115300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
115400970129     C                   MOVEL     '2'           TIPVID                         *VIDEO IN GESTIONE
115500970117     C                   EXSR      SETSF2                                       *SFILE
115600970117     C                   MOVEL     'N'           $FINV                          *FINE VIDEATA
115700970117     C* CARICA IL SUBFILE
115800970117     C                   EXSR      POSSF2
115900970117     C*
116000970117     C                   ENDSR
116100970117     C*------------------------------------------------------------------------*
116200970117     C* SETSF2 - IMPOSTAZIONI INDICATORI SECONDO SUBFILE
116300970117     C*------------------------------------------------------------------------*
116400970117     C     SETSF2        BEGSR
116500970117     C*
116600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
116700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
116800970117     C*
116900970117     C                   ENDSR
117000970117     C*------------------------------------------------------------------------*
117100970117     C* GESSF2 - GESTIONE SECONDO SUBFILE
117200970117     C*------------------------------------------------------------------------*
117300970117     C     GESSF2        BEGSR
117400970117     C*
117500970117     C* EMISSIONE VIDEATA
117600970117     C                   WRITE     TE03D00                                      *TESTATA
117700970117     C                   WRITE     TE03Z02                                      *PIEDI
117800970117     C                   EXFMT     TE03C02                                      *SFILE
117900970117     C                   EXSR      SETSF2
118000970117     C* F12-RITORNO
118100970117IF  1C     *INKL         IFEQ      *ON
118200970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
118300970130     C                   MOVEL     '1'           TIPVID
118400970117     C                   GOTO      FINSF2
118500970117E   1C                   ENDIF
118600970117     C* INVIO-RITORNA
118700970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
118800970117     C*
118900970117     C     FINSF2        ENDSR
119000970117     C*------------------------------------------------------------------------*
119100970117     C* IMPSF3 - IMPOSTAZIONI INIZIALI TERZO SUBFILE
119200970117     C*------------------------------------------------------------------------*
119300970117     C     IMPSF3        BEGSR
119400970117     C*
119500970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
119600970129     C                   MOVEL     '3'           TIPVID                         *VIDEO IN GESTIONE
119700970117     C                   EXSR      SETSF3                                       *SFILE
119800970117     C*
119900970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
120000970117     C                   EXSR      SETSC3                                       *SFILE CONTROL
120100970117     C* CARICA IL SUBFILE
120200970129     C*---       $DPC1     CHAINTE03S01              98    *RILEGGE COD.TASS.
120300970117     C                   EXSR      POSSF3
120400970117     C*
120500970117     C                   ENDSR
120600970117     C*------------------------------------------------------------------------*
120700970117     C* SETSF3 - IMPOSTAZIONI INDICATORI TERZO SUBFILE
120800970117     C*------------------------------------------------------------------------*
120900970117     C     SETSF3        BEGSR
121000970117     C*
121100970117     C                   SETOFF                                       19         ERRORI CONTROL
121200970117     C                   SETOFF                                       2021       ERRORI SFILE
121300970117     C                   SETOFF                                       90         GENERICO DI ERRORE
121400970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
121500970129     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
121600970117     C*
121700970117     C                   ENDSR
121800970117     C*------------------------------------------------------------------------*
121900970117     C* SETSC3 - IMPOSTAZIONI INDICATORI E CAMPI TERZO SUBFILE CONTROL
122000970117     C*------------------------------------------------------------------------*
122100970117     C     SETSC3        BEGSR
122200970117     C*
122300970117     C                   MOVEL     V1SCTS        V3CRAP
122400970117     C                   MOVEL     V1SDCT        V3CDRA
122500970117     C                   MOVEL     V1SRCT        V3CRCT
122600970117     C                   MOVEL     V1SDRC        V3CDRC
122700970117     C                   Z-ADD     V1SPPE        V3CPPR
122800970117     C                   Z-ADD     *ZEROS        V3CPPE
122900970117     C*
123000970117     C                   ENDSR
123100970117     C*------------------------------------------------------------------------*
123200970117     C* GESSF3 - GESTIONE TERZO SUBFILE
123300970117     C*------------------------------------------------------------------------*
123400970117     C     GESSF3        BEGSR
123500970117     C*
123600970117     C* EMISSIONE VIDEATA
123700970117IF  1C     $TESVD        IFEQ      '1'
123800970117     C                   WRITE     TE03D00                                      *TESTATA
123900970117     C                   MOVEL     '0'           $TESVD
124000970117E   1C                   ENDIF
124100970117     C                   WRITE     TE03Z03                                      *PIEDI
124200970117     C                   EXFMT     TE03C03                                      *SFILE
124300970117     C                   EXSR      SETSF3
124400970117     C* F12-RITORNO
124500970117IF  1C     *INKL         IFEQ      *ON
124600970129     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
124700970117     C                   MOVEL     '1'           TIPVID
124800970117     C                   EXSR      SETSF1                                       *REIMPOSTA INDICATOR
124900970117     C                   GOTO      FINSF3
125000970117E   1C                   ENDIF
125100970117     C* CONTROLLA VIDEO
125200970117     C                   EXSR      CTRSF3
125300970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
125400970117     C                   GOTO      FINSF3
125500970117E   1C                   ENDIF
125600970317     C* INVIO-AGGIORNA
125700970124     C                   EXSR      ENTSF3
125800970117     C*
125900970117     C     FINSF3        ENDSR
126000970117     C*------------------------------------------------------------------------*
126100970117     C* CTRSF3 - CONTROLLA TERZO SUBFILE
126200970117     C*------------------------------------------------------------------------*
126300970117     C     CTRSF3        BEGSR
126400970117     C*
126500970117     C* REIMPOSTA VARIABILI DI LAVORO
126600970117     C                   MOVEL     '0'           WERR              1            *ERRORE
126700970117     C                   Z-ADD     *ZEROS        V3CPPE                         *TOTALE PESI %
126800970117     C*
126900970117     C* LEGGE TUTTI I RECORD DEL SFILE
127000970117     C                   Z-ADD     1             NR3
127100970117     C     NR3           CHAIN     TE03S03                            98
127200970117DO  1C     *IN98         DOWEQ     *OFF
127300970117     C                   SETOFF                                       80        *NO SFLNXTCHG
127400970117     C*--------------------
127500970117     C* RAGGRUPPAMENTO
127600970117     C*--------------------
127700970117IF  2C     WERR          IFEQ      '0'
127800970117     C                   MOVEL     V3SCTS        WCTS
127900970117     C                   MOVEL     V3SRCT        WRCT
128000970117     C                   MOVEL     *BLANKS       WDRCT
128001050304     c                   clear                   $msg
128100970117     C                   EXSR      CHKRCT
128200970117     C                   MOVEL     WRCT          V3SRCT
128300970117     C                   MOVEL     WDRCT         V3SDRC
128400970117IF  3C     WERR          IFEQ      '1'
128500970117     C                   SETON                                        2090
128501050304     c                   if        $msg = *blanks
128600970117     C                   MOVEL     MSG(2)        $MSG
128700970117E   3C                   ENDIF
128701050304E   3C                   ENDIF
128800970117E   2C                   ENDIF
128900970117     C*--------------------
129000970117     C* %INCIDENZA
129100970117     C*--------------------
129200970117IF  2C     WERR          IFEQ      '0'
129300970117IF  3C     V3SRCT        IFNE      *BLANKS                                      *DA CALCOLARE
129400970117IF  4C     V3SPPE        IFEQ      *ZEROS                                       *% VUOTA
129500970117     C                   SETON                                        2190
129600970117     C                   MOVEL     MSG(3)        $MSG
129700970117     C                   MOVEL     '1'           WERR
129800970117E   4C                   ENDIF
129900970117X   3C                   ELSE                                                   *DA NON CALCOLARE
130000970117IF  4C     V3SPPE        IFGT      *ZEROS                                       *% PIENA
130100970117     C                   SETON                                        2190
130200970117     C                   MOVEL     MSG(4)        $MSG
130300970117     C                   MOVEL     '1'           WERR
130400970117E   4C                   ENDIF
130500970117E   3C                   ENDIF
130600970117E   2C                   ENDIF
130700970117     C* AGGIORNA SFILE
130800970117IF  2C     WERR          IFEQ      '1'
130900970117     C                   Z-ADD     NR3           $NR3                           *POSIZIONAMENTO
131000970117E   2C                   ENDIF
131100970319     C*
131200970319     C     V3CPPE        ADD       V3SPPE        V3CPPE                         *TOTALE PESI %
131300970117     C                   EXSR      AGGSF3
131400970319     C*
131500970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
131600970117     C                   GOTO      FINCT3                                       *ESCE
131700970117X   2C                   ELSE                                                   *N O N  ERRORE
131800970117     C                   ADD       1             NR3                            *LEGGE ALTRO RECORD
131900970117     C     NR3           CHAIN     TE03S03                            98
132000970117E   2C                   ENDIF
132100970117E   1C                   ENDDO                                                  *FINE SUBFILE
132200970117     C*
132300970117     C     FINCT3        ENDSR
132400970117     C*------------------------------------------------------------------------*
132500970117     C* AGGSF3 - AGGIORNA RIGA DEL TERSO SUBFILE
132600970117     C*------------------------------------------------------------------------*
132700970117     C     AGGSF3        BEGSR
132800970117     C*
132900970117     C                   SETON                                        80        *SI SFLNXTCHG
133000970117IF  1C     $TGES         IFEQ      'I'
133100970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
133200970117X   1C                   ELSE
133300970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
133400970117E   1C                   ENDIF
133500970117IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
133600970117     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
133700970117X   1C                   ELSE
133800970117     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
133900970117E   1C                   ENDIF
134000970117     C                   UPDATE    TE03S03                                      *AGGIORNA SUBFILE
134100970117     C                   SETOFF                                       80        *NO SFLNXTCHG
134200970117     C*
134300970117     C                   ENDSR
134400970124     C*------------------------------------------------------------------------*
134500970124     C* ENTSF3 - GESTIONE TASTO FUNZIONALE INVIO 3°SFILE  ==> AGGIORNA
134600970124     C*------------------------------------------------------------------------*
134700970124     C     ENTSF3        BEGSR
134800970124     C*
134900970124     C                   EXSR      AGGCTS
135000970124     C                   EXSR      AGGRCT
135100970129     C* AGGIORNA PRIMO SFILE
135200970129     C                   Z-ADD     1             J
135300970129     C     V3CRAP        LOOKUP    RCTS(J)                                98
135400970129     C     $DPC1         CHAIN     TE03S01                            98        *RILEGGE COD.TASS.
135500970129     C                   MOVEL     RRCT(J)       V1SRCT                         *RAGGRUPPAMENTO
135600970129     C                   Z-ADD     RPPE(J)       V1SPPE                         *%INCIDENZA
135700970129     C                   MOVEL     RHRC(J)       V1HRCT                         *RAGGRUPP.TO HIDDEN
135800970129     C                   MOVEL     RSCH(J)       V1SCTE                         *CAMPONE RAGGRUPP.TI
135900970129     C                   EXSR      AGGSF1
136000970128     C*
136100970124     C                   ENDSR
136200970124     C*------------------------------------------------------------------------*
136300970124     C* AGGCTS - AGGIORNA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
136400970124     C*------------------------------------------------------------------------*
136500970124     C     AGGCTS        BEGSR
136600970124     C*
136700970124     C* LEGGE TUTTI I RECORD DEL SFILE
136800970124     C                   Z-ADD     1             NR3
136900970124     C     NR3           CHAIN     TE03S03                            98
137000970124DO  1C     *IN98         DOWEQ     *OFF
137100970124     C                   Z-ADD     1             J
137200970124     C     V3SCTS        LOOKUP    VCTS(J)                                98
137300970128     C                   MOVEL     V3SRCT        VRCT(J)                        *RAGGRUPPAMENTO
137400970128     C                   Z-ADD     V3SPPE        VPPE(J)                        *%INCIDENZA
137500970124     C                   ADD       1             NR3
137600970124     C     NR3           CHAIN     TE03S03                            98
137700970124E   1C                   ENDDO                                                  *FINE SUBFILE
137800970124     C*
137900970124     C                   ENDSR
138000970117     C*------------------------------------------------------------------------*
138100970124     C* AGGRTC - AGGIORNA I CODICI TASSAZIONE REGIONALI
138200970117     C*------------------------------------------------------------------------*
138300970117     C     AGGRCT        BEGSR
138400980708     C*
138500980708     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
138600980708     C                   CLEAR                   RRCT
138700980708     C                   CLEAR                   RHRC
138800980708     C                   CLEAR                   RPPE
138900970117     C*
139000970117     C* PER CIAUSCUNA REGIONE
139100970117DO  1C     1             DO        WURCTS        J
139200970117     C                   MOVEL     '0'           $RB               1            *RAGGRUPPAMENTI
139300970117     C                   MOVEL     '0'           $RN               1
139400970117     C                   MOVEL     '0'           $RR               1
139500970117     C                   MOVEL     '0'           $RT               1
139600970117     C                   Z-ADD     *ZEROS        TOTPPE
139700970117DO  2C     1             DO        WUVCTS        I
139800970117IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
139900970117IF  4C     VRAP(I)       IFEQ      RCTS(J)                                      *STESSA REGIONE
140000970117     C*
140100970117     C* CALCOLA IL RAGGRUPPAMENTO DELLA REGIONE
140200970117IF  5C     VRCT(I)       IFEQ      *BLANKS
140300970117     C                   MOVEL     '1'           $RB                            *RAGGRUP.NO CALCOLAR
140400970117E   5C                   ENDIF
140500970117IF  5C     VRCT(I)       IFEQ      'N'
140600970117     C                   MOVEL     '1'           $RN                            *RAGGRUP.NAZIONE
140700970117E   5C                   ENDIF
140800970117IF  5C     VRCT(I)       IFEQ      'R'
140900970117     C                   MOVEL     '1'           $RR                            *RAGGRUP.REGIONE
141000970117E   5C                   ENDIF
141100970117IF  5C     VRCT(I)       IFEQ      'T'
141200970117     C                   MOVEL     '1'           $RT                            *RAGGRUP.CALCOLARE
141300970117E   5C                   ENDIF
141400970117     C*
141500970117     C* CALCOLA LA %INCIDENZA
141600970128     C                   ADD       VPPE(I)       TOTPPE
141700970117E   4C                   ENDIF
141800970117E   3C                   ENDIF
141900970117E   2C                   ENDDO                                                  *FINE COD.TASS.ELEM.
142000970117     C*
142100970117     C* AGGIORNA IL RAGGRUPPAMENTO DELLA REGIONE
142200970117IF  2C     $RB           IFEQ      '1'
142300970117     C     $RN           ANDEQ     '0'
142400970117     C     $RR           ANDEQ     '0'
142500970117     C     $RT           ANDEQ     '0'
142600970117     C                   MOVEL     ' '           RRCT(J)                        *RAGGRUP.NO CALCOLAR
142700970128X   2C                   ELSE
142800970117IF  3C     $RN           IFEQ      '1'
142900970117     C     $RR           ANDEQ     '0'
143000970117     C     $RT           ANDEQ     '0'
143100970117     C     $RB           ANDEQ     '0'
143200970117     C                   MOVEL     'N'           RRCT(J)                        *RAGGRUP.NAZIONE
143300970117X   3C                   ELSE
143400970117IF  4C     $RR           IFEQ      '1'
143500970117     C     $RN           ANDEQ     '0'
143600970117     C     $RT           ANDEQ     '0'
143700970117     C     $RB           ANDEQ     '0'
143800970117     C                   MOVEL     'R'           RRCT(J)                        *RAGGRUP.REGIONE
143900970117X   4C                   ELSE
144000970117IF  5C     $RT           IFEQ      '1'
144100970117     C     $RN           ANDEQ     '0'
144200970117     C     $RR           ANDEQ     '0'
144300970117     C     $RB           ANDEQ     '0'
144400970117     C                   MOVEL     'T'           RRCT(J)                        *RAGGRUP.CALCOLARE
144500970117X   5C                   ELSE
144600970117     C                   MOVEL     'M'           RRCT(J)                        *RAGGRUPPAMENTO MIST
144700970117E   5C                   ENDIF
144800970117E   4C                   ENDIF
144900970117E   3C                   ENDIF
145000970117E   2C                   ENDIF
145100970124     C                   MOVEL     RRCT(J)       RHRC(J)                        *RAGGRUP.TO HIDDEN
145200970117     C*
145300970117     C* AGGIORNA LA %INCIDENZA
145400970117     C                   Z-ADD     TOTPPE        RPPE(J)                        *%INCIDENZA
145500970117E   1C                   ENDDO                                                  *FINE COD.TASS.REG.
145600970117     C*
145700970117     C                   ENDSR
145800970117     C*------------------------------------------------------------------------*
145900970117     C* IMPWD1 - IMPOSTAZIONI PRIMA FINESTRA
146000970117     C*------------------------------------------------------------------------*
146100970117     C     IMPWD1        BEGSR
146200970117     C*
146300970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
146400970117     C                   MOVEL     'N'           $FINV                          *FINE VIDEATA
146500970117     C                   CLEAR                   TE03W01
146600970117     C                   EXSR      SETWD1
146700970117     C*
146800970117     C                   ENDSR
146900970117     C*------------------------------------------------------------------------*
147000970117     C* SETWD1 - IMPOSTAZIONI INDICATORI PRIMA FINESTRA
147100970117     C*------------------------------------------------------------------------*
147200970117     C     SETWD1        BEGSR
147300970117     C*
147400970117     C                   SETOFF                                       252627     ERRORI
147500970117     C                   SETOFF                                       28
147600970117IF  1C     TIPVID        IFEQ      '1'
147700970117     C                   SETOFF                                       02        *SI VISUAL.RAGGRUP=M
147800970117X   1C                   ELSE
147900970117     C                   SETON                                        02        *NO VISUAL.RAGGRUP=M
148000970117E   1C                   ENDIF
148100970117     C                   SETOFF                                       90         GENERICO DI ERRORE
148200970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
148300970117     C*
148400970117     C                   ENDSR
148500970117     C*------------------------------------------------------------------------*
148600970117     C* GESWD1 - GESTIONE PRIMA FINESTRA
148700970117     C*------------------------------------------------------------------------*
148800970117     C     GESWD1        BEGSR
148900970117     C*
149000970117     C* EMISSIONE VIDEATA
149100970117     C                   EXFMT     TE03W01
149200970117     C                   EXSR      SETWD1
149300970117     C* F12-RITORNO
149400970117IF  1C     *INKL         IFEQ      *ON
149500970129     C                   MOVEL     *BLANKS       WRCT
149600970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
149700970117     C                   GOTO      FINWD1
149800970117E   1C                   ENDIF
149900970117     C* CONTROLLA VIDEO
150000970117     C                   EXSR      CTRWD1
150100970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
150200970117     C                   GOTO      FINWD1
150300970117E   1C                   ENDIF
150400970117     C* INVIO-SCELTA
150500970117     C                   MOVEL     WWRCT         WRCT
150600970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
150700970117     C*
150800970117     C     FINWD1        ENDSR
150900970117     C*------------------------------------------------------------------------*
151000970117     C* CTRWD1 - CONTROLLA PRIMA FINESTRA
151100970117     C*------------------------------------------------------------------------*
151200970117     C     CTRWD1        BEGSR
151300970117     C*
151400970117     C     *LIKE         DEFINE    V1WRCB        WWRCT
151500970117     C                   MOVEL     *BLANKS       WWRCT
151600970117     C*--------------------
151700970117     C* DA NON CALCOLARE
151800970117     C*--------------------
151900970117IF  1C     V1WRCB        IFNE      *BLANKS
152000970117     C     V1WRCB        ANDNE     '1'
152100970117     C                   SETON                                        2590
152200970117     C                   MOVEL     MSG(11)       $MSG
152300970117     C                   GOTO      FINCW1
152400970117X   1C                   ELSE
152500970117IF  2C     V1WRCB        IFEQ      '1'
152600970117     C                   MOVEL     ' '           WWRCT                          *SCELTA
152700970117     C                   GOTO      FINCW1
152800970117E   2C                   ENDIF
152900970117E   1C                   ENDIF
153000970117     C*--------------------
153100970117     C* DA CALCOLARE
153200970117     C*--------------------
153300970117IF  1C     V1WRCT        IFNE      *BLANKS
153400970117     C     V1WRCT        ANDNE     '1'
153500970117     C                   SETON                                        2690
153600970117     C                   MOVEL     MSG(11)       $MSG
153700970117     C                   GOTO      FINCW1
153800970117X   1C                   ELSE
153900970117IF  2C     V1WRCT        IFEQ      '1'
154000970117     C                   MOVEL     'T'           WWRCT                          *SCELTA
154100970117     C                   GOTO      FINCW1
154200970117E   2C                   ENDIF
154300970117E   1C                   ENDIF
154400970117     C*--------------------
154500970117     C* DA CALCOLARE NELLA REGIONE
154600970117     C*--------------------
154700970117IF  1C     V1WRCR        IFNE      *BLANKS
154800970117     C     V1WRCR        ANDNE     '1'
154900970117     C                   SETON                                        2790
155000970117     C                   MOVEL     MSG(11)       $MSG
155100970117     C                   GOTO      FINCW1
155200970117X   1C                   ELSE
155300970117IF  2C     V1WRCR        IFEQ      '1'
155400970117     C                   MOVEL     'R'           WWRCT                          *SCELTA
155500970117     C                   GOTO      FINCW1
155600970117E   2C                   ENDIF
155700970117E   1C                   ENDIF
155800970117     C*--------------------
155900970117     C* DA CALCOLARE NELLA NAZIONE
156000970117     C*--------------------
156100970117IF  1C     V1WRCN        IFNE      *BLANKS
156200970117     C     V1WRCN        ANDNE     '1'
156300970117     C                   SETON                                        2890
156400970117     C                   MOVEL     MSG(11)       $MSG
156500970117     C                   GOTO      FINCW1
156600970117X   1C                   ELSE
156700970117IF  2C     V1WRCN        IFEQ      '1'
156800970117     C                   MOVEL     'N'           WWRCT                          *SCELTA
156900970117     C                   GOTO      FINCW1
157000970117E   2C                   ENDIF
157100970117E   1C                   ENDIF
157200970117     C*
157300970117     C     FINCW1        ENDSR
157400970117     C*------------------------------------------------------------------------*
157500970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
157600970117     C*------------------------------------------------------------------------*
157700970117     C     IMPWD2        BEGSR
157800970117     C*
157801091123     c                   if        d00dsf = ' '
157802091123     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
157803091123     c                   eval      s2wcto = '(T/O)'
157804091123     c                   eval      d2wksc = 'Cliente o Visita ...........:'
157805091123     c                   eval      d2wprg = 'Progressivo ................:'
157806091123     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
157807091123     c                             Tariffa/Offerta'
157808091123     c                   else
157809091201     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
157810091130     c                   eval      s2wcto = '(T/O)'
157811091123     c                   eval      d2wksc = 'Cliente o Trattativa .......:'
157812091123     c                   eval      d2wprg = 'Versione ...................:'
157813091123     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
157814091123     c                             Tariffa/Trattativa'
157815091123     c                   end
157900970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
158000970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
158100970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
158300980508     C                   MOVEL     D0DCTO        V2WCTO
158400970319     C                   Z-ADD     D0DKSC        V2WKSC
158500970319     C                   Z-ADD     D0DCTR        V2WCTR
158600970319     C                   Z-ADD     D0DPRG        V2WPRG
158700970319     C                   Z-ADD     D0DLNP        V2WLNP
158800011003     C                   MOVEL     D0DDIV        V2WDIV
158900970117     C                   EXSR      SETWD2
159000970117     C*
159100970117     C                   ENDSR
159200970117     C*------------------------------------------------------------------------*
159300970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
159400970117     C*------------------------------------------------------------------------*
159500970117     C     SETWD2        BEGSR
159600970117     C*
159700970117     C                   SETOFF                                       303132     ERRORI
159800980508     C                   SETOFF                                       333435
159900011003     C                   SETOFF                                       36
160000970117     C                   SETOFF                                       90         GENERICO DI ERRORE
160100970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
160200970117     C*
160300970117     C                   ENDSR
160400970117     C*------------------------------------------------------------------------*
160500970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
160600970117     C*------------------------------------------------------------------------*
160700970117     C     GESWD2        BEGSR
160800970117     C*
160900970117     C* EMISSIONE VIDEATA
161000970117     C                   EXFMT     TE03W02
161100970117     C                   EXSR      SETWD2
161200970317     C* F12 ABILITATO-RITORNO
161300970117IF  1C     *INKL         IFEQ      *ON
161400970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
161500970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
161600970117     C                   GOTO      FINWD2
161700970117E   1C                   ENDIF
161800980508     C* F11-RICERCA
161900980508IF  1C     *INKK         IFEQ      *ON
162000980508     C                   EXSR      GESRIC
162100060825     C*                  GOTO      FINWD2
162200980508E   1C                   ENDIF
162300970117     C* CONTROLLA VIDEO
162400970117     C                   EXSR      CTRWD2
162500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
162600970117     C                   GOTO      FINWD2
162700970117E   1C                   ENDIF
162800970117     C* F06-CONFERMA
162900970117IF  1C     *INKF         IFEQ      *ON
163000970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
163100970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
163200970117     C                   GOTO      FINWD2
163300970117E   1C                   ENDIF
163400970117     C*
163500970117     C     FINWD2        ENDSR
163600980508     C*------------------------------------------------------------------------*
163700980508     C* GESRIC - GESTIONE RICERCA DATI TIPO
163800980508     C*------------------------------------------------------------------------*
163900980508     C     GESRIC        BEGSR
164000980508     C*
164100980508     C                   CLEAR                   DSTE30
164200980508     C                   MOVEL     'C01'         D30OP0
164300980708     C                   MOVEL     'T'           D30ETX
164400980804     C                   Z-ADD     D00KSC        D30KSC
164500980904     C                   MOVEL     D00FIE        D30TFI
164600011011     C                   MOVEL     D00DIV        D30DIV
164601091123     C                   MOVEL     D00Dsf        D30Dsf
164700980508     C                   CALL      'TNTE32R'
164800980508     C                   PARM                    KPJBA
164900980508     C                   PARM                    DSTE30
165000980508IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
165100980508     C                   SETON                                        90
165200980508     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
165300980508X   1C                   ELSE                                                   *NO ERRORE
165400980508IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
165500980508     C     D30F12        OREQ      '1'
165600980508X   2C                   ELSE                                                   *SCELTA
165700980508     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
165900980508     C                   MOVEL     D30CTO        V2WCTO
166000980508     C                   Z-ADD     D30KSC        V2WKSC
166100980508     C                   Z-ADD     D30CTR        V2WCTR
166200980508     C                   Z-ADD     D30PRG        V2WPRG
166300980508     C                   Z-ADD     D30LNP        V2WLNP
166400011003     C                   MOVEL     D30DIV        V2WDIV
166500980508E   2C                   ENDIF
166600980508E   1C                   ENDIF
166700980508     C*
166800060825     C*                  MOVEL     'S'           $FINV                          *FINE FINESTRA
166900980508     C*
167000980508     C                   ENDSR
167100970117     C*------------------------------------------------------------------------*
167200970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
167300970117     C*------------------------------------------------------------------------*
167400970117     C     CTRWD2        BEGSR
168600970117     C*--------------------
168701091123     C* TARIFFA / OFFERTA  / TRATTATIVA
168800970117     C*--------------------
168801091123     c                   if        (d00dsf = ' '
168802091123IF  1C                             and v2wcto <> 'T' and
168803091123IF  1C                             v2wcto <> 'O') or
168804091123     c                             (d00dsf = 'X'
168805091123IF  1C                             and v2wcto <> 'T' and
168806091130IF  1C                             v2wcto <> 'O')
168807091123     C                   SETON                                        90
168808091123     C                   MOVEL     MSG(16)       $MSG
168809091123     C                   GOTO      FINCW2
168810091123E   1C                   ENDIF
169501060825     C*--------------------
169502060825     C* stessa tipologia di codice tariffa
169503060825     C*--------------------
169504061128     c*                  movel     d00ctr        comd00            1 0
169505061128     c*                  movel     v2wctr        comv2w            1 0
169506061128     c*                  if        comd00 <> comv2w
169507061128     C*                  SETON                                        31
169508061128     C*                  SETON                                        90
169509061128     C*                  MOVEL     MSG(28)       $MSG
169510061128     C*                  GOTO      FINCW2
169511061128     c*                  end
169600970117     C*--------------------
169700970117     C* LINEA DI PARTENZA
169800970117     C*--------------------
169900970117     C                   MOVEL     V2WLNP        WLNP
170000970117     C                   MOVEL     *BLANKS       WDLNP
170100971204     C                   MOVEL     *BLANKS       WFL1
170200970117     C                   MOVEL     '0'           WERR
170300970117     C                   EXSR      CHKLNP
170400980818IF  1C     WERR          IFEQ      '1'
170500980818IF  2C     WLNP          IFEQ      888
170600980818     C     WLNP          OREQ      889
170700980818     C                   MOVEL     '0'           WERR
170800980818     C                   MOVEL     DDEC(10)      WDLNP
170900980818IF  3C     WLNP          IFEQ      888
171000980818     C                   MOVEL     *BLANKS       WFL1
171100980818E   3C                   ENDIF
171200980818IF  3C     WLNP          IFEQ      889
171300980818     C                   MOVEL     'E'           WFL1
171400980818E   3C                   ENDIF
171500980818X   2C                   ELSE
171600011003     C                   SETON                                        343690
171700970117     C                   MOVEL     MSG(15)       $MSG
171800970117     C                   GOTO      FINCW2
171900980818E   2C                   ENDIF
172000980818E   1C                   ENDIF
172100970117     C*
172200970117IF  1C     D00FIE        IFEQ      'I'
172300980114IF  2C     WFL1          IFNE      *BLANKS
172400011003     C                   SETON                                        343690
172500970117     C                   MOVEL     MSG(17)       $MSG
172600970117     C                   GOTO      FINCW2
172700980114E   2C                   ENDIF
172800980114X   1C                   ELSE
172900980114IF  2C     DLPFL1        IFEQ      *BLANKS
173000971204     C     WFL1          ANDNE     *BLANKS
173100011003     C                   SETON                                        343690
173200970117     C                   MOVEL     MSG(18)       $MSG
173300970117     C                   GOTO      FINCW2
173400980114E   2C                   ENDIF
173500980114IF  2C     DLPFL1        IFNE      *BLANKS
173600971204     C     WFL1          ANDEQ     *BLANKS
173700011003     C                   SETON                                        343690
173800970117     C                   MOVEL     MSG(19)       $MSG
173900970117     C                   GOTO      FINCW2
174000980114E   2C                   ENDIF
174100970117E   1C                   ENDIF
174200970117     C*--------------------
174300970117     C* DATI TIPO ESISTENTI
174400970117     C*--------------------
174500091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
174600980508     C                   MOVEL     V2WCTO        KTTCTO                         *TARIFFA O OFFERTA
174700970117     C                   Z-ADD     V2WKSC        KTTKSC                         *CLIENTE O VISITA
174800970117     C                   Z-ADD     V2WCTR        KTTCTR                         *TARIFFA
174900970117     C                   Z-ADD     V2WPRG        KTTPRG                         *PROGRESSIVO
175000970117     C                   Z-ADD     V2WLNP        KTTLNP                         *LINEA DI PARTENZA
175100970117     C                   MOVEL     D00FIE        KTTFIE                         *FLAG ITALIA/ESTERO
175200970619     C                   MOVEL     'N'           WEETT                          *ESISTENZA CTS
175300011001     C     KE2ETT        SETLL     TEETT01L
175400011001     C     KE2ETT        READE     TEETT01L                               99
175500970619DO  1C     *IN99         DOWEQ     *OFF
175600970619IF  2C     ETTATB        IFEQ      *BLANKS
175700980508     C                   MOVEL     'S'           WEETT                          *ESISTENZA CTS
175800970619E   2C                   ENDIF
175900011001     C     KE2ETT        READE     TEETT01L                               99
176000980508E   1C                   ENDDO                                                  *FINE CTS
176100970619IF  1C     WEETT         IFEQ      'N'                                          *NON TROVATI
176200970117     C                   SETON                                        303132
176300980508     C                   SETON                                        333435
176400011003     C                   SETON                                        36
176500980508     C                   SETON                                        90
176600970117     C                   MOVEL     MSG(13)       $MSG
176700970117E   1C                   ENDIF
176800970117     C*
176900970117     C     FINCW2        ENDSR
177000970117     C*------------------------------------------------------------------------*
177100970117     C* IMPWD3 - IMPOSTAZIONI TERZA FINESTRA
177200970117     C*------------------------------------------------------------------------*
177300970117     C     IMPWD3        BEGSR
177400970117     C*
177500970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
177600970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
177700970318     C                   MOVEL     'NO'          V3WRIS
177800970117     C                   EXSR      SETWD3
177900970117     C*
178000970117     C                   ENDSR
178100970117     C*------------------------------------------------------------------------*
178200970117     C* SETWD3 - IMPOSTAZIONI INDICATORI TERZA FINESTRA
178300970117     C*------------------------------------------------------------------------*
178400970117     C     SETWD3        BEGSR
178500970117     C*
178600970117     C                   SETOFF                                       40         ERRORI
178700970117     C                   SETOFF                                       90         GENERICO DI ERRORE
178800970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
178900970117     C*
179000970117     C                   ENDSR
179100970117     C*------------------------------------------------------------------------*
179200970117     C* GESWD3 - GESTIONE TERZA FINESTRA
179300970117     C*------------------------------------------------------------------------*
179400970117     C     GESWD3        BEGSR
179500970117     C*
179600970117     C* EMISSIONE VIDEATA
179700970117     C                   EXFMT     TE03W03
179800970117     C                   EXSR      SETWD3
179900970117     C* F12-RITORNO
180000970117IF  1C     *INKL         IFEQ      *ON
180100970314     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
180200970318     C                   MOVEL     'NO'          V3WRIS
180300970117     C                   GOTO      FINWD3
180400970117E   1C                   ENDIF
180500970117     C* CONTROLLA VIDEO
180600970117     C                   EXSR      CTRWD3
180700970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
180800970117     C                   GOTO      FINWD3
180900970117E   1C                   ENDIF
181000970117     C* INVIO-CONFERMA
181100970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
181200970117     C*
181300970117     C     FINWD3        ENDSR
181400970117     C*------------------------------------------------------------------------*
181500970117     C* CTRWD3 - CONTROLLA TERZA FINESTRA
181600970117     C*------------------------------------------------------------------------*
181700970117     C     CTRWD3        BEGSR
181800970117     C*--------------------
181900970117     C* CONFERMA SCRITTURA OFFERTE
182000970117     C*--------------------
182100970117IF  1C     V3WRIS        IFNE      'SI'
182200970117     C     V3WRIS        ANDNE     'NO'
182300970117     C                   SETON                                        4090
182400970117     C                   MOVEL     MSG(12)       $MSG
182500970117     C                   GOTO      FINCW3
182600970117E   1C                   ENDIF
182700970117     C*
182800970117     C     FINCW3        ENDSR
182900970117     C*------------------------------------------------------------------------*
183000970117     C* POSSF1 - CARICA IL PRIMO SUBFILE
183100970117     C*------------------------------------------------------------------------*
183200970117     C     POSSF1        BEGSR
183300970117     C*
183400970117     C* ELIMINA IL CONTENUTO DEL SFILE
183500970117     C                   EXSR      CLRSF1
183600970117     C*
183700970117     C* SI POSIZIONA SUL RECORD DA CARICARE
183800970117     C                   EXSR      SETFI1
183900970117     C* CARICA IL SUBFILE
184000970117     C                   Z-ADD     *ZEROS        NR1               4 0          *DALL'INIZIO
184100970117     C                   EXSR      CARSF1
184200970117     C*
184300970117     C                   ENDSR
184400970117     C*------------------------------------------------------------------------*
184500970117     C* CLRSF1 - ELIMINA IL CONTENUTO DEL PRIMO SUBFILE
184600970117     C*------------------------------------------------------------------------*
184700970117     C     CLRSF1        BEGSR
184800970117     C*
184900970117     C                   Z-ADD     1             $NR1
185000970117     C                   SETOFF                                       71        *NO SFLDSP
185100970117     C                   SETOFF                                       72        *NO DSPCTL/SI SFLCLR
185200970117     C                   WRITE     TE03C01
185300970117     C                   SETON                                        71        *SI SFLDSP
185400970117     C                   SETON                                        72        *SI DSPCTL/NO SFLCLR
185500970117     C*
185600970117     C                   ENDSR
185700970117     C*------------------------------------------------------------------------*
185800970117     C* SETFI1 - POSIZIONAMENTO RECORD -PRIMO SUBFILE-
185900970117     C*------------------------------------------------------------------------*
186000970117     C     SETFI1        BEGSR
186100970117     C*
186200970117     C                   Z-ADD     *ZEROS        SF1EOF                         *0=NO FINE/1=FINE
186300970117     C                   Z-ADD     *ZEROS        S                 4 0          *INDICE SCHIERE VIDE
186400970117     C*
186500970117     C                   ENDSR
186600970117     C*------------------------------------------------------------------------*
186700970117     C* CARSF1 - CARICA TUTTO IL PRIMO SUBFILE
186800970117     C*------------------------------------------------------------------------*
186900970117     C     CARSF1        BEGSR
187000970117     C*
187100970117DO  1C                   DO        WURCTS
187200970117     C                   EXSR      LETFI1                                       *LEGGE RECORD
187300970117IF  2C     SF1EOF        IFEQ      *ZEROS
187400970117     C                   ADD       1             NR1
187500970117     C                   EXSR      IRESF1                                       *IMPOSTA RECORD SFIL
187600970117     C                   WRITE     TE03S01                                      *SCRIVE RECORD SFILE
187700970117E   2C                   ENDIF
187800970117E   1C                   ENDDO
187900970117     C*
188000970117     C                   ENDSR
188100970117     C*------------------------------------------------------------------------*
188200970117     C* LETFI1 - LETTURA SUCESSIVA RECORD -PRIMO SUBFILE-
188300970117     C*------------------------------------------------------------------------*
188400970117     C     LETFI1        BEGSR
188500970117     C*
188600970117     C                   ADD       1             S                              *INDICE SCHIERE VIDE
188700970117     C*
188800970117     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
188900970117IF  1C     S             IFGT      WURCTS                                       *AMPIEZZA MASSIMA
189000970117     C     RCTS(S)       OREQ      *BLANKS                                      *SCHIERA VUOTA
189100970117     C                   Z-ADD     1             SF1EOF
189200970117E   1C                   ENDIF
189300970117     C*
189400970117     C                   ENDSR
189500970117     C*------------------------------------------------------------------------*
189600970117     C* IRESF1 - IMPOSTA CAMPI RECORD PRIMO SUBFILE
189700970117     C*------------------------------------------------------------------------*
189800970117     C     IRESF1        BEGSR
189900970117     C* CAMPI
190000970124     C                   MOVEL     RHRC(S)       V1HRCT                         *HIDDEN
190100970117     C                   MOVEL     RCTS(S)       V1SCTS
190200970117     C                   Z-ADD     RPPE(S)       V1SPPE
190300970117     C                   MOVEL     RRCT(S)       V1SRCT
190400970117     C* DECODIFICHE
190500970117     C                   SETOFF                                           98
190600970117     C                   Z-ADD     1             I
190700970117     C     RCTS(S)       LOOKUP    CTS(I)                                 98
190800970117     C                   MOVEL     DCT(I)        V1SDCT
190900970117     C                   MOVEL     V1SCTS        WCTS
191000970117     C                   MOVEL     V1SRCT        WRCT
191100970117     C                   MOVEL     *BLANKS       WDRCT
191200970117     C                   EXSR      DECRCT
191300970117     C                   MOVEL     WDRCT         V1SDRC                         *DESCR.RAGGRUPPAMENT
191400970117     C*
191500970128     C                   CLEAR                   SCH
191600970128     C                   Z-ADD     *ZEROS        J
191700970128DO  1C     1             DO        WUVCTS        I
191800970128IF  2C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
191900970128IF  3C     VRAP(I)       IFEQ      RCTS(S)                                      *STESSA REGIONE
192000970128     C                   ADD       1             J
192100970128     C                   MOVEL     VRCT(I)       SCH(J)
192200970128IF  4C     SCH(J)        IFEQ      *BLANKS
192300970128     C                   MOVEL     '*'           SCH(J)
192400970128E   4C                   ENDIF
192500970128E   3C                   ENDIF
192600970128E   2C                   ENDIF
192700970128E   1C                   ENDDO
192800970128     C                   MOVEA     SCH           RSCH(S)
192900970128     C                   MOVEL     RSCH(S)       V1SCTE                         *TUTTI RAGGR COD.TAS
192901050304     c* se x il cod. tassazione regione non trova i corrispondenti cod.
192902050304     c* tassazione elementari (errore nell'inserimento tab. CT) mando
192903050304     c* un msg d'errore AVVISARE CED
192904050304     c                   if        v1scte = ' '
192905050304     c                   seton                                        105090
192906050304     C                   MOVEL     MSG(25)       $MSG
192907050304     c                   eval      V1SDRC = *all'?'
192908050304     c                   clear                   v1srct
192909050304     c                   clear                   v1hrct
192910050304     C                   clear                   RHRC(S)                        *HIDDEN
192911050304     C                   clear                   RRCT(S)
192912050304     c                   else
193000970203     C* INDICATORI
193100050304     C                   SETOFF                                       7050      *NO SFLNXTCHG
193200970203IF  1C     $TGES         IFEQ      'I'
193300970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
193400970203X   1C                   ELSE
193500970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
193600970203E   1C                   ENDIF
193700970203IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
193800970203     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
193900970203X   1C                   ELSE                                                   *DA PORTO
194000970203     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
194100970203E   1C                   ENDIF
194101050304E   1C                   ENDIF
194200970203     C*
194300970117     C                   ENDSR
194400970117     C*------------------------------------------------------------------------*
194500970117     C* POSSF2 - CARICA IL SECONDO SUBFILE
194600970117     C*------------------------------------------------------------------------*
194700970117     C     POSSF2        BEGSR
194800970117     C*
194900970117     C* ELIMINA IL CONTENUTO DEL SFILE
195000970117     C                   EXSR      CLRSF2
195100970117     C*
195200970117     C* SI POSIZIONA SUL TERZO RECORD DA CARICARE
195300970117     C                   EXSR      SETFI2
195400970117     C* CARICA IL SUBFILE
195500970117     C                   Z-ADD     *ZEROS        NR2               4 0          *DALL'INIZIO
195600970117     C                   EXSR      CARSF2
195700970117     C*
195800970117     C                   ENDSR
195900970117     C*------------------------------------------------------------------------*
196000970117     C* CLRSF2 - ELIMINA IL CONTENUTO DEL SECONDO SUBFILE
196100970117     C*------------------------------------------------------------------------*
196200980508     C     CLRSF2        BEGSR
196300970117     C*
196400970117     C                   Z-ADD     1             $NR2
196500970117     C                   SETOFF                                       91        *NO SFLDSP
196600970117     C                   SETOFF                                       92        *NO DSPCTL/SI SFLCLR
196700970117     C                   WRITE     TE03C02
196800970117     C                   SETON                                        91        *SI SFLDSP
196900970117     C                   SETON                                        92        *SI DSPCTL/NO SFLCLR
197000970117     C*
197100970117     C                   ENDSR
197200970117     C*------------------------------------------------------------------------*
197300970117     C* SETFI2 - POSIZIONAMENTO RECORD -SECONDO SUBFILE-
197400970117     C*------------------------------------------------------------------------*
197500970117     C     SETFI2        BEGSR
197600970117     C*
197700970117     C                   Z-ADD     *ZEROS        SF2EOF                         *0=NO FINE/1=FINE
197800970117     C                   Z-ADD     *ZEROS        O                 4 0          *INDICE SCHIERE VIDE
197900970117     C*
198000970117     C                   ENDSR
198100970117     C*------------------------------------------------------------------------*
198200970117     C* CARSF2 - CARICA TUTTO IL SECONDO SUBFILE
198300970117     C*------------------------------------------------------------------------*
198400970117     C     CARSF2        BEGSR
198500970117     C*
198600970117DO  1C     1             DO        WUOCTS
198700970117     C                   EXSR      LETFI2                                       *LEGGE RECORD
198800970117IF  2C     SF2EOF        IFEQ      *ZEROS
198900970117     C                   ADD       1             NR2
199000970117     C                   EXSR      IRESF2                                       *IMPOSTA RECORD SFIL
199100970117     C                   WRITE     TE03S02                                      *SCRIVE RECORD SFILE
199200970117E   2C                   ENDIF
199300970117E   1C                   ENDDO
199400970117     C*
199500970117     C                   ENDSR
199600970117     C*------------------------------------------------------------------------*
199700970117     C* LETFI2 - LETTURA SUCESSIVA RECORD -SECONDO SUBFILE-
199800970117     C*------------------------------------------------------------------------*
199900970117     C     LETFI2        BEGSR
200000970117     C*
200100970117     C                   ADD       1             O                              *INDICE SCHIERE VIDE
200200970117     C*
200300970117     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
200400970117IF  1C     O             IFGT      WUOCTS                                       *AMPIEZZA MASSIMA
200500970117     C     OCTS(O)       OREQ      *BLANKS                                      *SCHIERA VUOTA
200600970117     C                   Z-ADD     1             SF2EOF
200700970117E   1C                   ENDIF
200800970117     C*
200900970117     C                   ENDSR
201000970117     C*------------------------------------------------------------------------*
201100970117     C* IRESF2 - IMPOSTA CAMPI RECORD SECONDO SUBFILE
201200970117     C*------------------------------------------------------------------------*
201300970117     C     IRESF2        BEGSR
201400970117     C*
201500970117     C                   MOVEL     V1CPPE        V2CPPE                         *CONTROL
201600970117     C                   MOVEL     OCTS(O)       V2SCTS                         *SFILE
201700970117     C                   Z-ADD     OPPE(O)       V2SPPE
201800970117     C* DECODIFICHE
201900970117     C                   SETOFF                                           98
202000970117     C                   Z-ADD     1             I
202100970117     C     OCTS(O)       LOOKUP    CTS(I)                                 98
202200970117     C                   MOVEL     DCT(I)        V2SDCT
202300970117     C*
202400970117     C                   ENDSR
202500970117     C*------------------------------------------------------------------------*
202600970117     C* POSSF3 - CARICA IL TERZO SUBFILE
202700970117     C*------------------------------------------------------------------------*
202800970117     C     POSSF3        BEGSR
202900970117     C*
203000970117     C* ELIMINA IL CONTENUTO DEL SFILE
203100970117     C                   EXSR      CLRSF3
203200970117     C*
203300970117     C* SI POSIZIONA SUL RECORD DA CARICARE
203400970117     C                   EXSR      SETFI3
203500970117     C* CARICA IL SUBFILE
203600970117     C                   Z-ADD     *ZEROS        NR3               4 0          *DALL'INIZIO
203700970117     C                   EXSR      CARSF3
203800970117     C*
203900970117     C                   ENDSR
204000970117     C*------------------------------------------------------------------------*
204100970117     C* CLRSF3 - ELIMINA IL CONTENUTO DEL TERZO SUBFILE
204200970117     C*------------------------------------------------------------------------*
204300970117     C     CLRSF3        BEGSR
204400970117     C*
204500970117     C                   Z-ADD     1             $NR3
204600970117     C                   SETOFF                                       81        *NO SFLDSP
204700970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
204800970117     C                   WRITE     TE03C03
204900970117     C                   SETON                                        81        *SI SFLDSP
205000970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
205100970117     C*
205200970117     C                   ENDSR
205300970117     C*------------------------------------------------------------------------*
205400970117     C* SETFI3 - POSIZIONAMENTO RECORD -TERZO SUBFILE-
205500970117     C*------------------------------------------------------------------------*
205600970117     C     SETFI3        BEGSR
205700970117     C*
205800970117     C                   Z-ADD     *ZEROS        SF3EOF                         *0=NO FINE/1=FINE
205900970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
206000970117     C*
206100970117     C                   ENDSR
206200970117     C*------------------------------------------------------------------------*
206300970117     C* CARSF3 - CARICA TUTTO IL SECONDO SUBFILE
206400970117     C*------------------------------------------------------------------------*
206500970117     C     CARSF3        BEGSR
206600970117     C*
206700970117DO  1C                   DO        WUVCTS
206800970117     C                   EXSR      LETFI3                                       *LEGGE RECORD
206900970117IF  2C     SF3EOF        IFEQ      *ZEROS
207000970117     C                   ADD       1             NR3
207100970117     C                   EXSR      IRESF3                                       *IMPOSTA RECORD SFIL
207200970117     C                   WRITE     TE03S03                                      *SCRIVE RECORD SFILE
207300970117E   2C                   ENDIF
207400970117E   1C                   ENDDO
207500970117     C*
207600970117     C                   ENDSR
207700970117     C*------------------------------------------------------------------------*
207800970117     C* LETFI3 - LETTURA SUCESSIVA RECORD -TERZO SUBFILE-
207900970117     C*------------------------------------------------------------------------*
208000970117     C     LETFI3        BEGSR
208100970117     C*
208200970117     C* LEGGE FINO A:
208300970117     C*  - FINE FILE
208400970117     C*  - TROVATO RECORD VALIDO
208500970117     C                   MOVEL     'N'           WRECOK
208600970117DO  1C     SF3EOF        DOWEQ     *ZEROS
208700970117     C     WRECOK        ANDEQ     'N'
208800970117     C*
208900970117     C                   ADD       1             V
209000970117IF  2C     V             IFGT      WUVCTS                                       *AMPIEZZA MASSIMA
209100970117     C     VCTS(V)       OREQ      *BLANKS                                      *SCHIERA VUOTA
209200970117     C                   Z-ADD     1             SF3EOF
209300970117X   2C                   ELSE
209400970117     C                   EXSR      CHKSF3                                       *CONTROLLA RECORD
209500970117E   2C                   ENDIF
209600970117E   1C                   ENDDO
209700970117     C*
209800970117     C                   ENDSR
209900970117     C*------------------------------------------------------------------------*
210000970117     C* CHKSF3 - CONTROLLA RECORD DEL TERZO SUBFILE DA EMETTERE
210100970117     C*------------------------------------------------------------------------*
210200970117     C     CHKSF3        BEGSR
210300970117     C*
210400970117     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
210500970117     C*
210600970117     C* CODICE TASSAZIONE NON DELLA REGIONE, ESCLUDE
210700970117IF  1C     VRAP(V)       IFNE      V3CRAP
210800970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
210900970117E   1C                   ENDIF
211000970117     C*
211100970117     C                   ENDSR
211200970117     C*------------------------------------------------------------------------*
211300970117     C* IRESF3 - IMPOSTA CAMPI RECORD TERZO SUBFILE
211400970117     C*------------------------------------------------------------------------*
211500970117     C     IRESF3        BEGSR
211600970117     C* CAMPI
211700970117     C                   MOVEL     VCTS(V)       V3SCTS
211800970117     C                   Z-ADD     VPPE(V)       V3SPPE
211900970117     C                   MOVEL     VRCT(V)       V3SRCT
212000970128     C                   ADD       VPPE(V)       V3CPPE
212100970117     C* DECODIFICHE
212200970117     C                   SETOFF                                           98
212300970117     C                   Z-ADD     1             I
212400970117     C     VCTS(V)       LOOKUP    CTS(I)                                 98
212500970117     C                   MOVEL     DCT(I)        V3SDCT
212600970117     C                   MOVEL     FCR(I)        V3SFCR
212700970117     C                   MOVEL     V3SCTS        WCTS
212800970117     C                   MOVEL     V3SRCT        WRCT
212900970117     C                   MOVEL     *BLANKS       WDRCT
213000970117     C                   EXSR      DECRCT
213100970117     C                   MOVEL     WDRCT         V3SDRC                         *DESCR.RAGGRUPPAMENT
213200970203     C* INDICATORI
213300970203     C                   SETOFF                                       80        *NO SFLNXTCHG
213400970203IF  1C     $TGES         IFEQ      'I'
213500970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
213600970203X   1C                   ELSE
213700970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
213800970203E   1C                   ENDIF
213900970203IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
214000970203     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
214100970203X   1C                   ELSE
214200970203     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
214300970203E   1C                   ENDIF
214400970203     C*
214500970117     C                   ENDSR
214600970117     C*------------------------------------------------------------------------*
214700970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
214800970117     C*------------------------------------------------------------------------*
214900970117     C     CHKLNP        BEGSR
215000970117     C*
215100020729     C     *LIKE         DEFINE    §NTWFIE       WFL1
215200970117     C     *LIKE         DEFINE    V0DLNP        WDLNP
215300970117     C*
215400970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
215500970117     C                   MOVEL     WLNP          KORFIL
215600970117     C     KEYORG        CHAIN     AZORG01L                           99
215700970117IF  1C     *IN99         IFEQ      *OFF
215800970117     C     ORGFVA        ANDEQ     *BLANKS
215900970117     C     ORGFAG        ANDNE     'V'
216000970117     C                   MOVEL     ORGDES        WDLNP
216100020729     C                   MOVEL     ORGDE3        OG143
216200020729     C*
216300020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
216400020729     C                   CLEAR                   DNTW
216500020729     C*
216600020729     C                   MOVEL(P)  'NTW'         KTECOD
216700020729     C                   MOVEL(P)  §OGNTW        KTEKE1
216800020729     C     KEYTBE        CHAIN     TNTBE01L
216900020729     C                   IF        %found(TNTBE01L)
217000020729     C                   MOVEL     TBEUNI        DNTW
217100020729     C                   ENDIF
217200020729     C*
217300020729     C                   MOVEL     §NTWFIE       WFL1
217400970117X   1C                   ELSE
217500970117     C                   MOVEL     '1'           WERR
217600970117E   1C                   ENDIF
217700970117     C*
217800970117     C                   ENDSR
217900970117     C*------------------------------------------------------------------------*
218000970117     C* CHKRCT - CONTROLLA IL RAGGRUPPAMENTO CODICE TASSAZIONE
218100970117     C*------------------------------------------------------------------------*
218200970117     C     CHKRCT        BEGSR
218300970117     C*
218400970117     C     *LIKE         DEFINE    V1SRCT        WRCT
218500970117     C     *LIKE         DEFINE    V1SDRC        WDRCT
218600970117     C*
218700970117     C* RICHIESTO ELENCO CODICI
218800970117     C     '?'           SCAN      WRCT                                   98
218900970117IF  1C     *IN98         IFEQ      *ON
219000970117     C* GESTIONE VIDEATA
219100970117     C                   EXSR      IMPWD1
219200970117DO  2C     $FINV         DOUEQ     'S'
219300970117     C                   EXSR      GESWD1                                       *GESTIONE FINESTRA
219400970117E   2C                   ENDDO
219500970117E   1C                   ENDIF
219600050304     C* se tariffa estero non do la possibilità di mettere il ragg. = Nazione
219601050304     c                   do
219602050304     c                   if        wrct = 'N' and v0cfie = 'E'
219603050304     C                   MOVEL     '1'           WERR
219604050304     C                   MOVEL     msg(26)       $msg
219605050304     c                   leave
219606050304     c                   end
219607050304     c* verifico se per il cod. tassazione esiste un raggruppam. imposto
219608050304     c                   if        wrct <> ' '
219609050304     c                   z-add     1             bb                3 0
219610050304     c     wcts          lookup    cts(bb)                                51
219611050304     c                   if        *in51 and rct(bb) <> wrct
219612050304     c                             and rct(bb) <> *blanks
219613050304     C                   MOVEL     '1'           WERR
219614050304     C                   MOVEL     msg(27)       $msg
219615050304     c                   eval      %subst($msg: 60: 1)=rct(bb)
219616050304     c                   leave
219617050304     c                   end
219618050304     c                   end
219700970117     C* CONTROLLA ESATTEZZA CODICE
219800970117IF  1C     WRCT          IFNE      ' '
219900970117     C     WRCT          ANDNE     'T'
220000970117     C     WRCT          ANDNE     'R'
220100970117     C     WRCT          ANDNE     'N'
220200970117     C     WRCT          ANDNE     'M'
220300970117     C                   MOVEL     '1'           WERR
220301050304     c                   leave
220400050304X   1C                   End
220500970117     C                   EXSR      DECRCT                                       *DECODIFICA RAGGRUP.
220600970117IF  2C     WRCT          IFEQ      'M'                                          *SITUAZIONE "MISTA"
220700970117     C     WRCT          ANDNE     V1HRCT                                       *NON DA PRIMA
220800970117     C                   MOVEL     '1'           WERR
221000970117E   1C                   ENDIF
221001050304     c                   enddo
221100970117     C*
221200970117     C                   ENDSR
221300970117     C*------------------------------------------------------------------------*
221400970117     C* DECRCT - DECODIFICA IL RAGGRUPPAMENTO CODICE TASSAZIONE
221500970117     C*------------------------------------------------------------------------*
221600970117     C     DECRCT        BEGSR
221700970117     C*
221800970117IF  1C     WRCT          IFEQ      ' '                                          *NON CALCOLARE
221900970117     C                   MOVEL     DDEC(3)       WDRCT                          *DESCR.RAGGRUPPAMENT
222000970117E   1C                   ENDIF
222100970117IF  1C     WRCT          IFEQ      'T'                                          *CALCOLARE
222200970117     C                   MOVEL     DDEC(4)       WDRCT                          *DESCR.RAGGRUPPAMENT
222300970117E   1C                   ENDIF
222400970117IF  1C     WRCT          IFEQ      'R'                                          *CALCOLARE IN REGION
222500970117     C     WRCT          OREQ      'N'                                          *CALCOLARE IN NAZION
222600970117     C                   MOVEL     DDEC(5)       DDEC05
222700970117     C                   SETOFF                                           98
222800970117     C                   Z-ADD     1             I                 4 0
222900970117     C     WCTS          LOOKUP    CTS(I)                                 98
223000970117     C                   SETOFF                                           98
223100970117     C                   Z-ADD     1             J                 4 0
223200970117IF  2C     WRCT          IFEQ      'R'
223300970117     C     RAP(I)        LOOKUP    CTS(J)                                 98
223400970117X   2C                   ELSE
223500970117     C     NAZ(I)        LOOKUP    CTS(J)                                 98
223600970117E   2C                   ENDIF
223700970117     C                   MOVEL     DCT(J)        D05RCT
223800970117     C                   MOVEL     DDEC05        WDRCT                          *DESCR.RAGGRUPPAMENT
223900970117E   1C                   ENDIF
224000970117IF  1C     WRCT          IFEQ      'M'                                          *SITUAZIONE MISTA
224100970117     C                   MOVEL     DDEC(6)       WDRCT                          *DESCR.RAGGRUPPAMENT
224200970117E   1C                   ENDIF
224300970117     C*
224400970117     C                   ENDSR
224500970117     C*------------------------------------------------------------------------*
224600970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
224700970117     C*------------------------------------------------------------------------*
224800970117     C     CARCC         BEGSR
224900970117     C*
225000970117     C* RECUPERA I DATI DELL'UTENTE
225100020906     C                   Z-ADD     1             CODUT
225200020830     C                   CALL      'X§PARUT'
225300970117     C                   PARM                    PARUT
225400970117     C*
225500970117     C                   Z-ADD     1             Z
225600970117DO  1C                   DO        50            X2                2 0
225700970117     C                   MOVE      TCU(X2)       CARCNT
225800970117IF  2C     TIPCNT        IFEQ      'CI'
225900970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
226000970117E   2C                   ENDIF
226100970117IF  2C     FL34CN        IFEQ      '01'
226200970117     C     TIPCNT        ANDEQ     'CG'
226300970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
226400970117E   2C                   ENDIF
226500970117IF  2C     FL34CN        IFEQ      '02'
226600970117     C     TIPCNT        ANDEQ     'CG'
226700970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
226800970117E   2C                   ENDIF
226900970117IF  2C     TIPCNT        IFEQ      'RI'
227000970117     C                   Z-ADD     KCU(X2)       RI(Z)
227100970117     C                   ADD       1             Z                 3 0
227200970117E   2C                   ENDIF
227300970117E   1C                   ENDDO
227400970117     C*
227500970117     C                   ENDSR
227600970117     C*------------------------------------------------------------------------*
227700970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
227800970117     C*------------------------------------------------------------------------*
227900970117     C     CARTAB        BEGSR
228000970117     C*--------------------
228100970117     C* TIPI SERVIZIO
228200970117     C*--------------------
228300970117     C                   Z-ADD     *ZEROS        I                 4 0
228400020906     C                   Z-ADD     1             KTBKUT
228500970117     C                   MOVEL     '5E'          KTBCOD
228600970117     C     KE2TAB        SETLL     TABEL00F
228700970117     C     KE2TAB        READE     TABEL00F                               99
228800970117DO  1C     *IN99         DOWEQ     *OFF
228900970117IF  2C     TBLFLG        IFEQ      *BLANKS
229000970117     C                   ADD       1             I
229100970117     C                   MOVEL     TBLUNI        DS5E
229200970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
229300970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
229400970117E   2C                   ENDIF
229500970117     C     KE2TAB        READE     TABEL00F                               99
229600970117E   1C                   ENDDO
229700970117     C*--------------------
229800970117     C* TIPI TARIFFA
229900970117     C*--------------------
230000970117     C                   Z-ADD     *ZEROS        I                 4 0
230100020906     C                   Z-ADD     1             KTBKUT
230200970117     C                   MOVEL     'TR'          KTBCOD
230300970117     C     KE2TAB        SETLL     TABEL00F
230400970117     C     KE2TAB        READE     TABEL00F                               99
230500970117DO  1C     *IN99         DOWEQ     *OFF
230600970117IF  2C     TBLFLG        IFEQ      *BLANKS
230700970117     C                   MOVEL     TBLUNI        DSTR
230800970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
230900970117     C                   ADD       1             I
231000970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
231100970117     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
231200970117E   3C                   ENDIF
231300970117E   2C                   ENDIF
231400970117     C     KE2TAB        READE     TABEL00F                               99
231500970117E   1C                   ENDDO
231600970117     C*--------------------
231700970117     C* TARIFFA DI CARTELLO
231800970117     C*--------------------
231900970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
232000970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
232100970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
232200021202     C                   Z-ADD     *ZEROS        CARKSC
232300021202     C                   Z-ADD     *ZEROS        CARCTR
232400021202     C                   Z-ADD     *ZEROS        CARPRG
232500021202     C                   MOVEL     *BLANKS       CARDIV
232600021202     C                   CLEAR                   TRUL27DS
232700021202     C                   EVAL      I27TLA = 'L'
232800021202     C                   EVAL      I27DTA = DATCOR
232900021202     C                   CALL      'TRUL27R'
233000021202     C                   PARM                    TRUL27DS
233100021202     C*
233200021202IF  1C                   IF        O27ERR <> 'E'
233300021202     C                   Z-ADD     O27KSC        KTMKSC
233401091123     c                   if        %subst(d00op0: 1: 1)='O'
233500970117     C     KTMKSC        SETLL     TNOFM01L
233600970117     C     KTMKSC        READE     TNOFM01L                               99
233700970117X   2C                   ELSE
233800970117     C     KTMKSC        SETLL     TNTAM01L
233900970117     C     KTMKSC        READE     TNTAM01L                               99
234000970117E   2C                   ENDIF
234100970117DO  2C     *IN99         DOWEQ     *OFF
234200970117IF  3C     TAMATB        IFNE      'A'                                          *NO ANNULLATE
234300970117     C     TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
234400970117IF  4C     DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
234500970117     C     DATCOR        ANDLE     TAMDST
234600970117     C                   Z-ADD     TAMKSC        CARKSC
234700970117     C                   Z-ADD     TAMCTR        CARCTR
234800970117     C                   Z-ADD     TAMPRG        CARPRG
234900011003     C                   MOVEL     TAMFLO        DSTA01
235000011003     C                   MOVEL     §TADIV        CARDIV
235100970117     C                   LEAVE
235200970117E   4C                   ENDIF
235300970117E   3C                   ENDIF
235301091123IF  2C                   IF        %subst(d00op0:1 :1)='O'
235500970117     C     KTMKSC        READE     TNOFM01L                               99
235600970117X   3C                   ELSE
235700970117     C     KTMKSC        READE     TNTAM01L                               99
235800970117E   3C                   ENDIF
235900970117E   2C                   ENDDO
236000970117E   1C                   ENDIF
236100970117     C*--------------------
236200970117     C* CODICI TASSAZIONE
236300970117     C*--------------------
236400970117     C*---
236500970117     C* TARIFFA ITALIA: LNP ITALIA -> CARICA CODICI TASSAZIONE ITALIA
236600970117     C* TARIFFA ESTERO: LNP ITALIA -> CARICA CODICI TASSAZIONE ESTERO
236700970117     C*                 LNP ESTERO -> CARICA CODICI TASSAZIONE ITALIA + ESTERO
236800970117     C*---
236900970117     C                   Z-ADD     *ZEROS        I                 4 0
237000970117     C                   Z-ADD     *ZEROS        J                 4 0
237100970117     C                   Z-ADD     *ZEROS        K                 4 0
237200020906     C                   Z-ADD     1             KTBKUT
237300970117     C                   MOVEL     'CT'          KTBCOD
237400970117     C     KE2TAB        SETLL     TABEL00F
237500970117     C     KE2TAB        READE     TABEL00F                               99
237600970117DO  1C     *IN99         DOWEQ     *OFF
237700970117IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
237800970117     C                   MOVEL     TBLUNI        DSCT
237900970312IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
238000970312IF  4C     D00FIE        IFEQ      'I'                                          *TARIFFA ITALIA
238100970117     C     §CTNAR        ANDEQ     *BLANKS                                      *CODICI ITALIA
238200970117     C     D00FIE        OREQ      'E'                                          *TARIFFA ESTERO
238300971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
238400970117     C     §CTNAR        ANDNE     *BLANKS                                      *CODICI ESTERO
238500970117     C     D00FIE        OREQ      'E'                                          *TARIFFA ESTERO
238600971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ESTERO
238700970117     C                   ADD       1             I
238800970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
238900980514     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
239000970117     C                   MOVEL     §CTDES        DCT(I)                         *DESCRIZIONE
239100970117     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
239200970117     C                   MOVEL     §CTCOR        COR(I)                         *ORDINAMENTO
239201050304     C                   MOVEL     §CTRCT        RCT(I)                         *ORDINAMENTO
239300970117     C                   MOVE      §CTCOR        WA1                            *TIPO CODICE
239400970312IF  5C     WA1           IFEQ      '9'
239500970312IF  6C     CTS(I)        IFEQ      'IT'
239600970117     C     CTS(I)        OREQ      'EE'
239700970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
239800970312X   6C                   ELSE
239900970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
240000970312E   6C                   ENDIF
240100970312X   5C                   ELSE
240200970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
240300970312E   5C                   ENDIF
240400970312IF  5C     §CTNAR        IFEQ      *BLANKS
240500970117     C                   MOVEL     'IT'          NAZ(I)                         *NAZIONE ITALIA
240600970312X   5C                   ELSE
240700970117     C                   MOVEL     'EE'          NAZ(I)                         *NAZIONE ESTERO
240800970312E   5C                   ENDIF
240900970117     C* VIDEO->ELEMENTARI
241000970312IF  5C     TCT(I)        IFEQ      'E'
241100970117     C                   ADD       1             J
241200980508     C                   MOVEL     CTS(I)        VCTS(J)                        *CODICE
241300970117     C                   MOVEL     FCR(I)        VFCR(J)                        *FLAG CAPOLUOGO
241400970117     C                   MOVEL     COR(I)        VCOR(J)                        *ORDINAMENTO
241500970117     C                   MOVEL     RAP(I)        VRAP(J)                        *REGIONE
241600980508     C                   MOVEL     'T'           VRCT(J)                        *RAGGRUPPAMENTO
241700970312E   5C                   ENDIF
241800970117     C* VIDEO->REGIONALI
241900970312IF  5C     TCT(I)        IFEQ      'R'
242000970117     C                   ADD       1             K
242100970117     C                   MOVEL     CTS(I)        RCTS(K)                        *CODICE
242200970117     C                   MOVEL     COR(I)        RCOR(K)                        *ORDINAMENTO
242300970312E   5C                   ENDIF
242400970312E   4C                   ENDIF
242500970117E   3C                   ENDIF
242600970117E   2C                   ENDIF
242700970117     C     KE2TAB        READE     TABEL00F                               99
242800970117E   1C                   ENDDO
242900970117     C                   Z-ADD     J             WUVCTS            4 0          *N°COD.TASS.ELEM.VID
243000970117     C                   Z-ADD     K             WURCTS            4 0          *N°COD.TASS.REG.VID.
243100970117     C*
243200970117     C* ORDINA CODICI ELEMENTARI VIDEO
243300970117     C                   EXSR      ORDVCT
243400970117     C*
243500970117     C* ORDINA CODICI REGIONALI VIDEO
243600970117     C                   EXSR      ORDRCT
243700970117     C*
243800970117     C                   ENDSR
243900970117     C*------------------------------------------------------------------------*
244000970117     C* ORDVCT - ORDINA I CODICI TASSAZIONE ELEMENTARI DA EMETTERE A VIDEO
244100970117     C*------------------------------------------------------------------------*
244200970117     C     ORDVCT        BEGSR
244300970117     C*
244400970117DO  1C     1             DO        WUVCTS        I
244500970117IF  2C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
244600970117     C     1             ADD       I             J
244700970117DO  3C     J             DO        WUVCTS        J
244800970117IF  4C     VCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
244900970117IF  5C     VCOR(I)       IFGT      VCOR(J)
245000970117     C                   MOVEL     VCTS(I)       PCTS                           *CODICE
245100970117     C                   MOVEL     VCTS(J)       VCTS(I)
245200970117     C                   MOVEL     PCTS          VCTS(J)
245300970117     C                   MOVEL     VFCR(I)       PFCR                           *FLAG CAPOLUOGO
245400970117     C                   MOVEL     VFCR(J)       VFCR(I)
245500970117     C                   MOVEL     PFCR          VFCR(J)
245600970117     C                   MOVEL     VCOR(I)       PCOR                           *ORDINAMENTO
245700970117     C                   MOVEL     VCOR(J)       VCOR(I)
245800970117     C                   MOVEL     PCOR          VCOR(J)
245900970117     C                   MOVEL     VRAP(I)       PRAP                           *REGIONE
246000970117     C                   MOVEL     VRAP(J)       VRAP(I)
246100970117     C                   MOVEL     PRAP          VRAP(J)
246200970117E   5C                   ENDIF
246300970117E   4C                   ENDIF
246400970117E   3C                   ENDDO                                                  *FINE CICLO UNO
246500970117E   2C                   ENDIF
246600970117E   1C                   ENDDO                                                  *FINE CICLO DUE
246700970117     C*
246800970117     C                   ENDSR
246900970117     C*------------------------------------------------------------------------*
247000970117     C* ORDRCT - ORDINA I CODICI TASSAZIONE REGIONALI DA EMETTERE A VIDEO
247100970117     C*------------------------------------------------------------------------*
247200970117     C     ORDRCT        BEGSR
247300970117     C*
247400970117DO  1C     1             DO        WURCTS        I
247500970117IF  2C     RCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
247600970117     C     1             ADD       I             J
247700970117DO  3C     J             DO        WURCTS        J
247800970117IF  4C     RCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
247900970117IF  5C     RCOR(I)       IFGT      RCOR(J)
248000970117     C                   MOVEL     RCTS(I)       PCTS                           *CODICE
248100970117     C                   MOVEL     RCTS(J)       RCTS(I)
248200970117     C                   MOVEL     PCTS          RCTS(J)
248300970117     C                   MOVEL     RCOR(I)       PCOR                           *ORDINAMENTO
248400970117     C                   MOVEL     RCOR(J)       RCOR(I)
248500981218     C                   MOVEL     PCOR          RCOR(J)
248600981218E   5C                   ENDIF
248700970117E   4C                   ENDIF
248800970117E   3C                   ENDDO                                                  *FINE CICLO UNO
248900970117E   2C                   ENDIF
249000970117E   1C                   ENDDO                                                  *FINE CICLO DUE
249100970117     C*
249200970117     C                   ENDSR
249300970117     C*------------------------------------------------------------------------*
249400970117     C* IMPVD0 - IMPOSTA LA TESTATA
249500970117     C*------------------------------------------------------------------------*
249600970117     C     IMPVD0        BEGSR
249700970117     C*
249800970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
249900970117     C*---
250000970117     C* TESTATA
250100970117     C*---
250200970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
250300970117IF  1C     D00TLA        IFEQ      'P'
250400970117     C                   MOVEL     DVID(1)       V0CTLA                         *LIVELLO PROCEDURA
250500970117X   1C                   ELSE
250600970117     C                   MOVEL     DVID(2)       V0CTLA                         *LIVELLO PROCEDURA
250700970117E   1C                   ENDIF
251201091123     C                   MOVEL     'Prg.'        d0cprg                         *TARIFFA O OFFERTA
251202091123     c                   if        %subst(d00op0: 1: 1) = 'O'
251203091123     c                   if        d00dsf =' '
251204091123     C                   MOVEL     'Visita '     d0cKsc                         *TARIFFA O OFFERTA
251205091123X   1C                   ELSE
251206091123     C                   MOVEL     'Tratt. '     d0cksc                         *TARIFFA O OFFERTA
251207091123     C                   MOVEL     'Ver.'        d0cprg                         *TARIFFA O OFFERTA
251208091123E   1C                   ENDIF
251209091123X   1C                   ELSE
251210091123     C                   MOVEL     'Cliente'     d0cksc                         *TARIFFA O OFFERTA
251211091123E   1C                   ENDIF
251300970117IF  1C     $TGES         IFEQ      'I'
251400970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
251500970122     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
251600970117X   1C                   ELSE
251700970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
251800970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
251900970117E   1C                   ENDIF
252000970117     C* CAMPI
252100970117     C                   Z-ADD     D00KSC        V0CKSC
252200970117     C                   Z-ADD     D00CTR        V0CCTR
252300970117     C                   Z-ADD     D00PRG        V0CPRG
252400970117     C                   MOVEL     D00FIE        V0CFIE
252600970117     C                   MOVEL     D00TSP        V0CTSP
252700970117     C                   Z-ADD     D00LNP        V0CLNP
252800011003     C                   MOVEL     D00DIV        V0CDIV
252900970117IF  1C     D00DTC        IFGT      *ZEROS
253000970117     C                   Z-ADD     D00DTC        G08INV
253100970117     C                   Z-ADD     *ZEROS        G08DAT
253200970117     C                   MOVEL     '3'           G08ERR
253300970117     C                   CALL      'XSRDA8'
253400970117     C                   PARM                    WLBDA8
253500970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
253600970117E   1C                   ENDIF
253700970117     C* DECODIFICHE
253800970117     C                   MOVEL     *BLANKS       V0DKSC
253900970117IF  1C     D00KSC        IFGT      *ZEROS
254000020906     C                   Z-ADD     1             KACKUT
254100970117     C                   Z-ADD     KCCCL         KACKCC
254200970117     C                   Z-ADD     D00KSC        KACKSC
254300091203     C*    KEYACO        CHAIN     CNACO00F                           99
254601091123IF  2C                   IF        %subst(d00op0: 1: 1) ='T'                    *TARIFFA
254800000211     C     KEYACO        CHAIN     CNACO00F                           99
254900000211X   2C                   ELSE                                                   *OFFERTA
255000000211     C     KEYACO        CHAIN     TFACO00F                           99
255100000211E   2C                   ENDIF
255200000211IF  2C     *IN99         IFEQ      *OFF
255300000211     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
255301091203     c                   else
255302091203     c                   clear                   tnte04ds
255303091203     C                   movel     D00dsf        d04dsf
255304091203     C                   Z-ADD     D00KSC        d04nrv
255305091203     c                   call      'TNTE04R'
255306091203     c                   parm                    kpjba
255307091203     c                   parm                    tnte04ds
255309091203     c                   movel     d04rag        v0dksc
255400000211E   2C                   ENDIF
255500970117E   1C                   ENDIF
255600970117     C*
255700970117     C                   MOVEL     *BLANKS       V0DCTR
255800970117IF  1C     D00KSC        IFGT      *ZEROS
255900970117     C                   MOVEL     D00CTR        WA1               1
256000970117     C                   SETOFF                                           98
256100970117     C                   Z-ADD     1             I
256200970117     C     WA1           LOOKUP    TTR(I)                                 98
256300970117IF  2C     *IN98         IFEQ      *ON
256400970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
256500970117E   2C                   ENDIF
256600970117E   1C                   ENDIF
256700970117     C*
256800970117     C                   MOVEL     *BLANKS       V0DTSP
256900970117IF  1C     D00TSP        IFNE      *BLANKS
257000970117     C                   SETOFF                                           98
257100970117     C                   Z-ADD     1             I
257200970117     C     D00TSP        LOOKUP    TSP(I)                                 98
257300970117IF  2C     *IN98         IFEQ      *ON
257400970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
257500970117E   2C                   ENDIF
257600970117E   1C                   ENDIF
257700970117     C*
257800970117     C                   MOVEL     *BLANKS       V0DFIE
257900970117IF  1C     D00FIE        IFEQ      'I'
258000970117     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
258100970117E   1C                   ENDIF
258200970117IF  1C     D00FIE        IFEQ      'E'
258300970117     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
258400970117E   1C                   ENDIF
259300970117     C*
259400970117     C                   MOVEL     *BLANKS       V0DLNP
259500970117IF  1C     D00LNP        IFGT      *ZEROS
259600970117     C                   Z-ADD     D00LNP        KORFIL
259700970117     C     KEYORG        CHAIN     AZORG01L                           99
259800970117IF  2C     *IN99         IFEQ      *OFF
259900970117     C     ORGFVA        ANDEQ     *BLANKS
260000970117     C     ORGFAG        ANDNE     'V'
260100970117     C                   MOVEL     ORGDES        V0DLNP
260200970117E   2C                   ENDIF
260300970117E   1C                   ENDIF
260400970117     C*---
260500970117     C* FINESTRA F05-CARICA DA ALTRA TARIFFA
260600970117     C*---
260800980508     C                   MOVEL     'T'           V2WCTO
260900970117     C                   Z-ADD     CARKSC        V2WKSC
261000970117     C                   Z-ADD     CARCTR        V2WCTR
261100970117     C                   Z-ADD     CARPRG        V2WPRG
261200970117     C                   Z-ADD     D00LNP        V2WLNP
261300011003     C                   MOVEL     CARDIV        V2WDIV
261400970117     C*
261500970117     C                   ENDSR
261600970117     C*------------------------------------------------------------------------*
261700970117     C* MEMREC - MEMORIZZA IN SCHIERE I RECORD DA MANUTENZIONARE
261800970117     C*------------------------------------------------------------------------*
261900970117     C     MEMREC        BEGSR
262000970117     C*
262100980708     C* LEGGE I DATI TIPO DEL CLIENTE/DELLA VISITA
262200091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
262300980508     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
262400970122     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
262500970117     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
262600970117     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
262700970117     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
262800970117     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
262900011001     C     KE2ETT        SETLL     TEETT01L                               99
263000970117     C*
263100970117     C* SE CODICI INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
263200970117IF  1C     *IN99         IFEQ      *OFF
263300970117     C                   MOVEL     DOPE(1)       V0COPE                         I N S E R I M E N T
263400970117     C                   WRITE     TE03D00                                      *TESTATA
263500970117     C                   WRITE     TE03D01                                      *MESSAGGIO SF VUOTO
263600970117     C                   EXSR      F05GES
263700980508     C*
263800980508     C* SE NON E' STATO CONFERMATO IL CARICAMENTO DA ALTRA TARIFFA, ALLORA
263900980508     C* CARICA I CODICI TASSAZIONE DA TABELLA
264000980508IF  2C     $CONF         IFEQ      'N'                                          *NESSUNA CONFERMA
264100980508     C                   EXSR      CARSTD                                       *CARICA DATI STANDAR
264200980508     C                   EXSR      AGGSCV                                       *AGGIORNA SCHIERE VI
264300980508E   2C                   ENDIF
264400970117X   1C                   ELSE
264500980708     C                   MOVEL     DOPE(2)       V0COPE                         *V A R I A Z I O N E
264700980708     C                   MOVEL     D00CTO        WCTO
264800980708     C                   Z-ADD     D00KSC        WKSC
264900980708     C                   Z-ADD     D00CTR        WCTR
265000980708     C                   Z-ADD     D00PRG        WPRG
265100980708     C                   Z-ADD     D00LNP        WLNP
265200980708     C                   MOVEL     D00FIE        WFIE
265300011003     C                   MOVEL     D00DIV        WDIV
265400980708     C                   EXSR      MEMETT                                       *MEM.DATI E AGG.VIDE
265500980708E   1C                   ENDIF
265600970117IF  1C     $TGES         IFEQ      'I'
265700970117     C                   MOVEL     *BLANKS       V0COPE
265800970117E   1C                   ENDIF
265900970117     C*
266000970117     C                   ENDSR
266100970129     C*------------------------------------------------------------------------*
266200970129     C* MEMCAR - MEMORIZZA I DATI DELLA TARIFFA DI CARTELLO
266300970129     C*------------------------------------------------------------------------*
266400970129     C     MEMCAR        BEGSR
266500980515     C*
266600980515     C* IMPOSTA I CODICI TASSAZIONE STANDARD
266700980708     C                   MOVEL     VCTS          CTS8
266800980708     C                   MOVEL     VRAP          RAP8
266900980515     C*
267000980515     C* PULISCE LE SCHIERE DI MEMORIZZAZIONE
267100980515     C                   CLEAR                   RCT8
267200980514     C                   CLEAR                   PPE8
267300970129     C* CARCAMENTO DATI
267400980515     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
267500980508     C                   MOVEL     'T'           KTTCTO                         *TARIFFA O OFFERTA
267600980508     C                   Z-ADD     CARKSC        KTTKSC                         *CLIENTE O VISITA
267700970129     C                   Z-ADD     CARCTR        KTTCTR                         *TARIFFA
267800970129     C                   Z-ADD     CARPRG        KTTPRG                         *PROGRESSIVO
267900970129     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
268000970129     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
268100011001     C     KE2ETT        SETLL     TEETT01L                               99
268200011001     C     KE2ETT        READE(N)  TEETT01L                               99
268300970129DO  1C     *IN99         DOWEQ     *OFF
268400970129IF  2C     ETTATB        IFEQ      *BLANKS
268500970129     C                   SETOFF                                           98
268600970129     C                   Z-ADD     1             I
268700980515     C     ETTCTS        LOOKUP    CTS8(I)                                98
268800970129IF  3C     *IN98         IFEQ      *ON
268900970129     C                   MOVEL     ETTRCT        RCT8(I)                        *RAGGRUPPAMENTO
269000970129     C                   Z-ADD     ETTPPE        PPE8(I)                        *%INCIDENZA
269100980911     C     ETTRCT        IFEQ      'N'
269200980911     C     ETTPPE        ANDEQ     *ZEROS
269300980911     C                   MOVEL     *BLANKS       RCT8(I)
269400980911     C                   ENDIF
269500970129E   3C                   ENDIF
269600970129E   2C                   ENDIF
269700011001     C     KE2ETT        READE(N)  TEETT01L                               99
269800970129E   1C                   ENDDO
269900970129     C*
270000970129     C                   ENDSR
270100980508     C*------------------------------------------------------------------------*
270200980508     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
270300980508     C*------------------------------------------------------------------------*
270400980508     C     CTRENT        BEGSR
270500980508     C*--------------------
270600980508     C* INTERROGAZIONE -> DATI TIPO INESISTENTI
270700980508     C*--------------------
270800980508IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
270900980508     C                   MOVEL     '0'           WECTS             1            *NO ESISTENZA CTS
271000091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE/FILIALE
271100980508     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
271200980508     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE P VISITA
271300980508     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
271400980508     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
271500980508     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
271600980508     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
271700011001     C     KE2ETT        SETLL     TEETT01L                               99
271800011001     C     KE2ETT        READE(N)  TEETT01L                               99
271900980508DO  2C     *IN99         DOWEQ     *OFF                                         *NON TROVATO
272000980508IF  3C     ETTATB        IFEQ      *BLANKS                                      *NON ANNULLATI
272100980508     C                   MOVEL     '1'           WECTS                          *SI ESISTENZA CTS
272200980508     C                   LEAVE
272300980508E   3C                   ENDIF
272400011001     C     KE2ETT        READE(N)  TEETT01L                               99
272500980508E   2C                   ENDDO
272600980508IF  2C     WECTS         IFEQ      '0'
272700980508     C                   MOVEL     '1'           WERR                           *ERRORE
272800980508     C                   MOVEL     MSG(1)        D00MSG
272900980508     C                   MOVEL     '1'           D00ERR
273000980508     C                   MOVEL     'S'           $FINE                          *USCITA PGM
273100980508E   2C                   ENDIF
273200980508E   1C                   ENDIF
273300980508     C*
273400980508     C                   ENDSR
273500970117     C*------------------------------------------------------------------------*
273600970117     C* *INZSR - OPERAZIONI INIZIALI
273700970117     C*------------------------------------------------------------------------*
273800970117     C     *INZSR        BEGSR
273900970117     C*--------------------
274000970117     C* RICEVIMENTO PARAMETRI
274100970117     C*--------------------
274200970117     C     *ENTRY        PLIST
274300970117     C                   PARM                    KPJBA
274400970117     C                   PARM                    DSTE00
274500041202     c* apertura controllata di alcuni file xchè solo di filiale
274600041202     c                   open      tfaco00f                             95
274700041202     c                   open      tnofm01l                             95
274800970117     C*--------------------
274900970117     C* VARIABILI RIFERITE AL DATABASE
275000970117     C*--------------------
275100970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
275200970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
275300970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
275400970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
275500970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
275600970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
275700970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
275800970117     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
275900011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
276000980508     C     *LIKE         DEFINE    ETTCTO        KTTCTO
276100970117     C     *LIKE         DEFINE    ETTKSC        KTTKSC
276200970117     C     *LIKE         DEFINE    ETTCTR        KTTCTR
276300970117     C     *LIKE         DEFINE    ETTPRG        KTTPRG
276400970117     C     *LIKE         DEFINE    ETTLNP        KTTLNP
276500970117     C     *LIKE         DEFINE    ETTFIE        KTTFIE
276600970117     C     *LIKE         DEFINE    ETTCTS        KTTCTS
276700970117     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
276800970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
276900970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
277000011003     C     *LIKE         DEFINE    §TADIV        CARDIV
277100970128     C     *LIKE         DEFINE    ETTCTS        DEPCTS
277200970128     C     *LIKE         DEFINE    ETTPPE        DEPPPE
277300020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
277400970117     C     *LIKE         DEFINE    ETTCTS        PCTS                           *DI PARCHEGGIO
277500970117     C     *LIKE         DEFINE    §CTFCR        PFCR
277600970117     C     *LIKE         DEFINE    §CTCOR        PCOR
277700970117     C     *LIKE         DEFINE    §CTRAP        PRAP
277800970117     C     *LIKE         DEFINE    ETTPPE        PPPE
278000980708     C     *LIKE         DEFINE    D00CTO        WCTO
278100980708     C     *LIKE         DEFINE    D00KSC        WKSC
278200980708     C     *LIKE         DEFINE    D00CTR        WCTR
278300980708     C     *LIKE         DEFINE    D00PRG        WPRG
278400980708     C     *LIKE         DEFINE    D00LNP        WLNP
278500980708     C     *LIKE         DEFINE    D00FIE        WFIE
278600011003     C     *LIKE         DEFINE    D00DIV        WDIV
278700020729     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
278800020729     C     *LIKE         DEFINE    TBEKE1        KTEKE1
278900970117     C*--------------------
279000970117     C* CHIAVI DI LETTURA
279100970117     C*--------------------
279200970117     C* LETTURA TABEL00F - COMPLETA
279300970117     C     KEYTAB        KLIST
279400970117     C                   KFLD                    KTBKUT                         *UTENTE
279500970117     C                   KFLD                    KTBCOD                         *TABELLA
279600970117     C                   KFLD                    KTBKEY                         *CODICE
279700970117     C* LETTURA TABEL00F - PARZIALE
279800970117     C     KE2TAB        KLIST
279900970117     C                   KFLD                    KTBKUT                         *UTENTE
280000970117     C                   KFLD                    KTBCOD                         *TABELLA
280100970117     C* LETTURA AZORG01L - COMPLETA
280200970117     C     KEYORG        KLIST
280300970117     C                   KFLD                    KORFIL                         *FILIALE
280400970117     C* LETTURA CNACO01L - COMPLETA
280500970117     C     KEYACO        KLIST
280600970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
280700970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
280800970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
280900011001     C* LETTURA TEETT01L - PARZIALE
281000970117     C     KE2ETT        KLIST
281100980508     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
281200980508     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
281300970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
281400970117     C                   KFLD                    KTTCTR                         *TARIFFA
281500970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
281600970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
281700970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
281800011001     C* LETTURA TEETT01L - COMPLETA
281900970117     C     KEYETT        KLIST
282000980508     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
282100970117     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
282200970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
282300970117     C                   KFLD                    KTTCTR                         *TARIFFA
282400970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
282500970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
282600970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
282700970117     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
282800020729     C* LETTURA TNTBE01L - PARZIALE
282900020729     C     KEYTBE        KLIST
283000020729     C                   KFLD                    KTECOD                         *UTENTE
283100020729     C                   KFLD                    KTEKE1                         *CODICE
283200970117     C*--------------------
283300970117     C* VARIABILI NON RIFERITE AL DATABASE
283400970117     C*--------------------
283500970117     C     *LIKE         DEFINE    NR1           SF1EOF                         *DI DEPOSITO
283600970117     C     *LIKE         DEFINE    NR2           SF2EOF
283700970117     C     *LIKE         DEFINE    NR3           SF3EOF
283800970117     C     *LIKE         DEFINE    $PC1          $DPC1                          *DEPOSITO NRR 1°SFIL
283900970129     C     *LIKE         DEFINE    V1SOPZ        DEPOPZ
284000970117     C*--------------------
284100970117     C* VARIABILI DI CONTROLLO
284200970117     C*--------------------
284300970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
284400970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
284500970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
284600970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
284700970117     C                   MOVEL     '0'           WERR              1            *ERRORE
284800970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
284900970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
285000970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
285100970619     C                   MOVEL     'N'           WEETT             1            *ESISTENZA CTS
285200970117     C*
285300970117     C* REPERISCE LA DATA CORRENTE
285400970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
285500970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
285600970117     C                   Z-ADD     WN8           G08DAT
285700970117     C                   Z-ADD     *ZEROS        G08INV
285800970117     C                   MOVEL     '0'           G08ERR
285900970117     C                   CALL      'XSRDA8'
286000970117     C                   PARM                    WLBDA8
286100970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
286200970117     C*
286300970117     C* 02->GESTIONE, 05->INTERROGAZIONE
286400970117     C                   MOVE      D00OP0        WA2               2
286500970117IF  1C     WA2           IFEQ      '02'
286600970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
286700970117X   1C                   ELSE
286800970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
286900970117E   1C                   ENDIF
287000971204     C*
287100971204     C* REPERISCE IL FLAG ITALIA/ESTERO PER LA LNP DI ENTRATA
287200971204     C                   MOVEL     D00LNP        WLNP
287300971204     C                   MOVEL     *BLANKS       WFL1
287400971204     C                   EXSR      CHKLNP
287500971204     C                   MOVEL     WFL1          DLPFL1
287600970117     C*
287700970117     C* CARICA I CAPICONTO DI CONTABILITA'
287800970117     C                   EXSR      CARCC
287900980508     C*
288000980508     C* CONTROLLI INIZIALI DI ENTRATA
288100980508     C                   EXSR      CTRENT
288200970117     C*
288300970117     C* CARICA LE TABELLE OCCORRENTI
288400970117     C                   EXSR      CARTAB
288500970117     C*
288600970117     C* IMPOSTA LA TESTATA
288700970117     C                   EXSR      IMPVD0
288800970129     C*
288900970129     C* MEMORIZZA I DATI DELLA TARIFFA DI CARTELLO
289000970129     C                   EXSR      MEMCAR
289100970117     C*
289200970117     C* MEMORIZZA IN SCHIERE I RECORD DA MANUTENZIONARE
289300970117     C                   EXSR      MEMREC
289400970117     C*
289500970117     C                   ENDSR
289600970117     O*------------------------------------------------------------------------*
289700980508** -DVID-
289800970117PORTO               1
289900970117DELTA               2
2900009701173
2901009701174
2902009701175
290300970117** -DDEC-
290400970117ITALIA                   1
290500970117ESTERO                   2
290600970117NON APPARE IN TARIFFA    3
290700970117APPARE IN TARIFFA        4
290800970117APPARE IN XXXXXXXXXXXXXXX5
290900970117SITUAZIONE MISTA         6
291000980508SEDE                     7
291100980508FILIALE                  8
2912009701179
291300980818TUTTE LE FILIALI         10
291400970117** -DOPE-
291500970117* INSERIMENTO *1
291600970117* VARIAZIONE * 2
291700970122INTERROGAZIONE 3
291800970122 * GESTIONE *  4
2919009701175
292000970117** -MSG-
292100980508TNTE03R- Non esistono i dati tipo per la tariffa/visita richiesta             1
292200970117TNTE03R- Raggruppamento codice tassazione errato                              2
292300970117TNTE03R- Se il codice tass. è da esprimere immettere la %incidenza            3
292400970117TNTE03R- Se il codice tass. non è da esprimere lasciare vuota la %incidenza   4
292500970117TNTE03R- Il totale della %incidenza di tutti i codici tass. deve essere 100   5
292600970128TNTE03R- Togliere nei codici tassazione della regione il raggruppamento       6
292700970128TNTE03R- Togliere nei codici tassazione della regione la % di incidenza       7
292800970129TNTE03R- Inserire in tutti i codici tassazione il reggruppamento della regione8
292900970129TNTE03R- Inserire in tutti i codici tassazione la % di incidenza              9
293000970128TNTE03R- La % regione e la somma delle % dei codici tass. deve esser uguale   10
293100970117TNTE03R- Opzione errata                                                       11
293200970117TNTE03R- Immettere SI per confermare oppure NO                                12
293300970117TNTE03R- Dati tipo richiesti non trovati                                      13
293400970117TNTE03R- Caricare i codici tassazione da altra tariffa o da altra offerta     14
293500970117TNTE03R- Filiale errata                                                       15
293600970117TNTE03R- "T" per caricare i dati tariffa, "O" per caricare i dati offerta     16
293700970117TNTE03R- Tariffa Italia. Filiale errata                                       17
293800970117TNTE03R- Tariffa Estero con linea di partenza italia. Filiale errata          18
293900970117TNTE03R- Tariffa Estero con linea di partenza estero. Filiale errata          19
294000970117Scorrimento indietro o avanti oltre il primo o l'ultimo record del file.      20
294100970129TNTE03R- Inserire nei codici tassazione con raggruppamento la % di incidenza  21
294200970129TNTE03R- Inserire nei codici tassazione con % di incidenza il raggruppamento  22
294300970415TNTE03R- La % regione non puo' essere inferiore a xxx,x                       23
294400980508TNTE02R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    24
294500050304ERRORE NEI CODICI TASSAZIONE AVVISARE CED
294600050304Per tariffe estero non è possibile inserire il raggruppamento N
294700050304Per questo cod.tassazione esiste il raggruppamento imposto x
294801060825Il codice tariffa scelto incongruente con quello originale                    28
294900970117TNTE03R- 29
295000980508TNTE03R- 30
