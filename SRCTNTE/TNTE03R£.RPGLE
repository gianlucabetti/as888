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
004000091203     D Tnte04DS      E DS                                                       *Profili utente
004100970117     D*-------------------
004200970117     D* DESCRIZIONE VIDEO IN GESTIONE
004300970117     D*-------------------
004400970117     D DVID            S             20    DIM(5) CTDATA PERRCD(1)
004500970117     D*-------------------
004600970117     D* DECODIFICHE VARIE
004700970117     D*-------------------
004800970117     D DDEC            S             25    DIM(10) CTDATA PERRCD(1)
004900970117     D*-------------------
005000970117     D* DESCRIZIONE OPERAZIONE
005100970117     D*-------------------
005200970117     D DOPE            S             15    DIM(5) CTDATA PERRCD(1)
005300970117     D*-------------------
005400970117     D* MESSAGGI VIDEO
005500970117     D*-------------------
005600970117     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
005700970117     D*-------------------
005800970117     D* TIPO SERVIZIO
005900970117     D*-------------------
006000970117     D TSP             S              1    DIM(10)                              *CODICE
006100970117     D DTSP            S             20    DIM(10)                              *DESCRIZIONE
006200970117     D*-------------------
006300970117     D* TIPI TARIFFA
006400970117     D*-------------------
006500970117     D TTR             S              1    DIM(10)                              *CODICE
006600970117     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
006700970117     D*-------------------
006800970117     D* CODICI TASSAZIONE
006900970117     D*-------------------
007000970117     D*---
007100970117     D* BASE
007200970117     D*---
007300050103     D CTS             S              2    DIM(600)                             *CODICE
007400050103     D DCT             S             20    DIM(600)                             *DESCRIZIONE
007500050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
007600050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
007700050103     D COR             S              3    DIM(600)                             *ORDINAMENTO
007800050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
007900050103     D NAZ             S              2    DIM(600)                             *CODICE NAZIONE -> P
008000050304     D rct             S              1    DIM(600)                             *CODICE NAZIONE -> P
008100970117     D*---
008200970117     D* VIDEO
008300970117     D*---
008400970117     D* ELEMENTARI
008500050103     D VCTS            S              2    DIM(600)                             *CODICE
008600050103     D VFCR            S              1    DIM(600)                             *FLAG CAPOLUOGO
008700050103     D VCOR            S              3    DIM(600)                             *ORDINAMENTO
008800050103     D VRAP            S              2    DIM(600)                             *REGIONE
008900050103     D VRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
009000050103     D VPPE            S              4  1 DIM(600)                             *%INCIDENZA
009100970117     D* REGIONALI
009200050103     D RCTS            S              2    DIM(60)                              *CODICE
009300050103     D RCOR            S              3    DIM(60)                              *ORDINAMENTO
009400050103     D RRCT            S              1    DIM(60)                              *RAGGRUPPAMENTO
009500050103     D RPPE            S              4  1 DIM(60)                              *%INCIDENZA
009600050103     D RHRC            S              1    DIM(60)                              *RAGGRUPPAMENTO HIDD
009700050103     D RSCH            S             15    DIM(60)                              *CAMPONE RAGGRUPPAME
009800970117     D*---
009900970117     D* CARICAMENTO ALTRA TARIFFA
010000970117     D*---
010100050103     D CCTS            S              2    DIM(600)                             *CODICE
010200050103     D CRAP            S              2    DIM(600)                             *REGIONE
010300050103     D CRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
010400050103     D CPPE            S              4  1 DIM(600)                             *%INCIDENZA
010500970117     D*---
010600970117     D* RAPPRESENTAZIONE
010700970117     D*---
010800050103     D OCTS            S              2    DIM(600)                             *CODICE
010900050103     D OCOR            S              3    DIM(600)                             *ORDINAMENTO
011000050103     D OPPE            S              4  1 DIM(600)                             *%INCIDENZA
011100970129     D*---
011200970129     D* DI CARTELLO
011300970129     D*---
011400050103     D CTS8            S              2    DIM(600)                             *CODICE
011500050103     D RAP8            S              2    DIM(600)                             *REGIONE
011600050103     D RCT8            S              1    DIM(600)                             *RAGGRUPPAMENTO
011700050103     D PPE8            S              4  1 DIM(600)                             *%INCIDENZA
011800970124     D*-------------------
011900970124     D* DI LAVORO
012000970124     D*-------------------
012100970124     D SCH             S              1    DIM(15)
012200970117     D*------------------
012300970117     D* TABELLE CAPICONTO
012400970117     D*------------------
012500970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
012600970117     D*------------------------------------------------------------------------*
012700970117     D* INPUT
012800970117     D*------------------------------------------------------------------------*
012900970117     D*-------------------
013000970117     D* DS TIPI SERVIZIO
013100970117     D*-------------------
013200970117     D DS5E          E DS
013300970117     D*-------------------
013400970117     D* DS TIPI TARIFFA
013500970117     D*-------------------
013600970117     D DSTR          E DS
013700021202     D*-------------------
013800021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
013900021202     D*-------------------
014000021202     D TRUL27DS      E DS
014100970117     D*-------------------
014200970117     D* DS CODICI TASSAZIONE
014300970117     D*-------------------
014400970117     D DSCT          E DS
014500970117     D*-------------------
014600970117     D* DS PASSAGGIO PARAMETRI
014700970117     D*-------------------
014800970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
014900980508     D*------------------
015000980508     D* DS RICERCA DATI TIPO
015100980508     D*------------------
015200980508     D DSTE30        E DS                  EXTNAME(TNTE30DS)
015300011003     D*-------------------
015400011003     D* DS REPERIMENTO DIVISA TARIFFE/OFFERTE
015500011003     D*-------------------
015600011003     D DSTA01        E DS
015700970117     D*------------------
015800970415     D* DS SCOMPOSIZIONE DESCRIZIONE VIDEO
015900970117     D*------------------
016000970117     D* DDEC,5
016100970117     D                 DS
016200970117     D  D05RCT                11     25
016300970117     D  DDEC05                 1     25
016400970415     D*------------------
016500970415     D* DS SCOMPOSIZIONE MESSAGGI
016600970415     D*------------------
016700970415     D* MSG,23
016800970415     D                 DS
016900970415     D  MSGDFI                51     53
017000970415     D  MSGDFD                55     55
017100970415     D  MSGM23                 1     78
017200970117     D*------------------
017300970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
017400970117     D*------------------
017500970117     D WLBDA8          DS
017600970117     D  G08DAT                 1      8  0
017700970117     D  G08INV                 9     16  0
017800970117     D  G08ERR                17     17
017900970117     D  G08TGI                18     22  0
018000970117     D*------------------
018100020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
018200970117     D*------------------
018300970117     D CARCNT          DS
018400970117     D  FL12CN                 1      2
018500970117     D  FL34CN                 3      4
018600970117     D  TIPCNT                 5      6
018700020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
018800970117     D  FLU                   66     97
018900970117     D                                     DIM(32)                              FLAG PAR.UTENT
019000970117     D  TCU                  398    697
019100970117     D                                     DIM(50)                              *TAB FLG 8  TP.CN
019200970117     D  KCU                  698    847P 0
019300970117     D                                     DIM(50)                              *TAB CAPICONTO
019400970117     D                                     PACKEVEN
019500970117     D TCUDS           DS
019600970117     D  E1                     1      1
019700970117     D  E3                     3      3
019800970117     D  E2                     2      2
019900970117     D  E4                     4      4
020000970117     D  E56                    5      6
020100970117     D*-------------------
020200970117     D* ARCHITETTURA
020300970117     D*-------------------
020400970117     D KPJBA         E DS
020500020729     D*-------------------
020600020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
020700020729     D*-------------------
020800020729     D OG143         E DS
020900020729     D*------------------
021000020729     D* DS PER LETTURA TABELLA "NTW"
021100020729     D*------------------
021200020729     D DNTW          E DS
021300970117     C*------------------------------------------------------------------------*
021400970117     C* MAIN LINE
021500970117     C*------------------------------------------------------------------------*
021600970117     C*
021700970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
021800970117IF  1C     $FINE         IFNE      'S'
021900970117     C*
022000970117     C* IMPOSTAZIONI PRIMO SUBFILE
022100970117     C                   EXSR      IMPSF1
022200970117     C*
022300970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
022400970117DO  2C     $FINE         DOUEQ     'S'
022500970117     C     TIPVID        CASEQ     '1'           GESSF1                         *1° VIDEO
022600970129     C     TIPVID        CASEQ     '2'           GESSF2                         *2° VIDEO
022700970129     C     TIPVID        CASEQ     '3'           GESSF3                         *3° VIDEO
022800970117     C                   ENDCS
022900970117E   2C                   ENDDO
023000970117E   1C                   ENDIF
023100970117     C*
023200970117     C                   SETON                                        LR
023300970117     C*------------------------------------------------------------------------*
023400970117     C* IMPSF1 - IMPOSTAZIONI INIZIALI PRIMO SUBFILE
023500970117     C*------------------------------------------------------------------------*
023600970117     C     IMPSF1        BEGSR
023700970117     C*
023800970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
023900970129     C                   MOVEL     '1'           TIPVID                         *VIDEO IN GESTIONE
024000970117     C                   EXSR      SETSF1                                       *SFILE
024100970117     C* CARICA IL SUBFILE
024200970117     C                   EXSR      POSSF1
024300970117     C*
024400970117     C                   ENDSR
024500970117     C*------------------------------------------------------------------------*
024600970117     C* SETSF1 - IMPOSTAZIONI INDICATORI PRIMO SUBFILE
024700970117     C*------------------------------------------------------------------------*
024800970117     C     SETSF1        BEGSR
024900970117     C*
025000970117     C                   SETOFF                                       29         ERRORI CONTROL
025100970117     C                   SETOFF                                       222324     ERRORI SFILE
025200970117     C                   SETOFF                                       90         GENERICO DI ERRORE
025300970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
025400970117     C*
025500970117     C                   ENDSR
025600970117     C*------------------------------------------------------------------------*
025700970117     C* GESSF1 - GESTIONE PRIMO SUBFILE
025800970117     C*------------------------------------------------------------------------*
025900970117     C     GESSF1        BEGSR
026000970117     C*
026100970117     C* EMISSIONE VIDEATA
026200970117IF  1C     $TESVD        IFEQ      '1'
026300970117     C                   WRITE     TE03D00                                      *TESTATA
026400970117     C                   MOVEL     '0'           $TESVD
026500970117E   1C                   ENDIF
026600970117     C                   WRITE     TE03Z01                                      *PIEDI
026700970117     C                   EXFMT     TE03C01                                      *SFILE
026800970117     C                   EXSR      SETSF1
026900970117     C* F03-FINE
027000970117IF  1C     *INKC         IFEQ      *ON
027100970117     C                   EXSR      F03GES
027200970117     C                   GOTO      FINSF1
027300970117E   1C                   ENDIF
027400970117     C* F12-RITORNO
027500970117IF  1C     *INKL         IFEQ      *ON
027600970117     C                   EXSR      F12GES
027700970117     C                   GOTO      FINSF1
027800970117E   1C                   ENDIF
027900970117     C* F05-CARICA DA ALTRA TARIFFA
028000970117IF  1C     *INKE         IFEQ      *ON
028100970117     C                   EXSR      F05GES
028200970117     C                   GOTO      FINSF1
028300970117E   1C                   ENDIF
028400970117     C* F07-RAPPRESENTAZIONE
028500970117IF  1C     *INKG         IFEQ      *ON
028600970117     C                   EXSR      F07GES
028700970117     C                   GOTO      FINSF1
028800970117E   1C                   ENDIF
028900970117     C* CONTROLLA VIDEO
029000970117     C                   EXSR      CTRSF1
029100970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
029200970117     C                   GOTO      FINSF1
029300970117E   1C                   ENDIF
029400970117     C* INVIO-AGGIORNA
029500970124     C                   EXSR      ENTSF1
029600970117     C* F06-AGGIORNA E PROSEGUE
029700970117IF  1C     *INKF         IFEQ      *ON
029800970117     C                   EXSR      F06GES
029900970117E   1C                   ENDIF
030000970117     C*
030100970117     C     FINSF1        ENDSR
030200970117     C*------------------------------------------------------------------------*
030300970117     C* CTRSF1 - CONTROLLA PRIMO SUBFILE
030400970117     C*------------------------------------------------------------------------*
030500970117     C     CTRSF1        BEGSR
030600970117     C*
030700970117     C* REIMPOSTA VARIABILI DI LAVORO
030800970117     C                   MOVEL     '0'           WERR              1            *ERRORE
030900970117     C                   Z-ADD     *ZEROS        V1CPPE                         *TOTALE PESI %
031000970117     C*
031100970117     C* LEGGE TUTTI I RECORD DEL SFILE
031200970117     C                   Z-ADD     1             NR1
031300970117     C     NR1           CHAIN     TE03S01                            98
031400970117DO  1C     *IN98         DOWEQ     *OFF
031500970117     C                   SETOFF                                       70        *NO SFLNXTCHG
031600970117     C*--------------------
031700970117     C* RAGGRUPPAMENTO
031800970117     C*--------------------
031900970117IF  2C     WERR          IFEQ      '0'
032000970117     C                   MOVEL     V1SCTS        WCTS
032100970117     C                   MOVEL     V1SRCT        WRCT
032200970117     C                   MOVEL     *BLANKS       WDRCT
032300050304     c                   clear                   $msg
032400970117     C                   EXSR      CHKRCT
032500970117     C                   MOVEL     WRCT          V1SRCT
032600970117     C                   MOVEL     WDRCT         V1SDRC
032700970117IF  3C     WERR          IFEQ      '1'
032800970117     C                   SETON                                        2290
032900050304     c                   if        $msg = *blanks
033000970117     C                   MOVEL     MSG(2)        $MSG
033100970117E   3C                   ENDIF
033200050304E   3C                   ENDIF
033300970117E   2C                   ENDIF
033400970117     C*--------------------
033500970117     C* %INCIDENZA
033600970117     C*--------------------
033700970117IF  2C     WERR          IFEQ      '0'
033800970117IF  3C     V1SRCT        IFNE      *BLANKS                                      *DA CALCOLARE
033900970117IF  4C     V1SPPE        IFEQ      *ZEROS                                       *%VUOTA
034000970117     C                   SETON                                        2390
034100970117     C                   MOVEL     MSG(3)        $MSG
034200970117     C                   MOVEL     '1'           WERR
034300970117E   4C                   ENDIF
034400970117X   3C                   ELSE                                                   *DA NON CALCOLARE
034500970117IF  4C     V1SPPE        IFGT      *ZEROS                                       *%PIENA
034600970117     C                   SETON                                        2390
034700970117     C                   MOVEL     MSG(4)        $MSG
034800970117     C                   MOVEL     '1'           WERR
034900970117E   4C                   ENDIF
035000970117E   3C                   ENDIF
035100970117E   2C                   ENDIF
035200970117     C*--------------------
035300970117     C* OPZIONE
035400970117     C*--------------------
035500970117IF  2C     WERR          IFEQ      '0'
035600970117IF  3C     V1SOPZ        IFEQ      *BLANKS
035700970117     C     V1SOPZ        OREQ      '1 '
035800970117X   3C                   ELSE
035900970117     C                   MOVEL     '1'           WERR
036000970117     C                   SETON                                        2490
036100970117     C                   MOVEL     MSG(11)       $MSG
036200970117E   3C                   ENDIF
036300970117E   2C                   ENDIF
036400970117     C* NO ERRORE
036500970117IF  2C     WERR          IFEQ      '0'
036600970117     C                   EXSR      AGGVCT                                       AGGIORNA COD.TAS.REG
036700970117     C                   EXSR      CTRVCT                                       CONTROLLA COD.TAS.RE
036800970117     C                   EXSR      GESOP1                                       GESTISCE OPZIONI
036900970117E   2C                   ENDIF
037000970121     C*
037100970317     C     V1CPPE        ADD       V1SPPE        V1CPPE                         *TOTALE PESI %
037200970129     C     NR1           CHAIN     TE03S01                            98        *RILEGGE COD.TASS.
037300970129     C                   EXSR      AGGSF1                                       *AGGIORNA SFILE
037400970117     C*
037500970117IF  2C     WERR          IFEQ      '1'                                          E R R O R E
037600970117     C                   Z-ADD     NR1           $NR1                           *POSIZIONAMENTO
037700970117     C                   GOTO      FINCT1                                       *ESCE
037800970117X   2C                   ELSE                                                   N O   E R R O R E
037900970117     C                   ADD       1             NR1                            *LEGGE ALTRO RECORD
038000970117     C     NR1           CHAIN     TE03S01                            98
038100970117E   2C                   ENDIF
038200970117E   1C                   ENDDO                                                  *FINE SUBFILE
038300970117     C*
038400970117     C     FINCT1        ENDSR
038500970117     C*------------------------------------------------------------------------*
038600970117     C* AGGVCT - AGGIORNA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
038700970117     C*------------------------------------------------------------------------*
038800970117     C     AGGVCT        BEGSR
038900970415     C*
039000970415     C* REIMPOSTA LE VARIABILI DI LAVORO
039100970415     C                   MOVEL     '0'           WERR
039200970415     C                   Z-ADD     *ZEROS        TOTPPM            4 1          *% MINIMA IPOTETICA
039300970203     C*
039400970117     C* AGGIORNA IL RAGGRUPPAMENTO DEI CODICI TASSAZIONE DELLA REGIONE
039500970117IF  1C     V1SRCT        IFNE      'M'                                          *NO SITUAZIONE MISTA
039600970117DO  2C     1             DO        WUVCTS        I
039700970117IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
039800970117IF  4C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
039900970117     C                   MOVEL     V1SRCT        VRCT(I)                        *RAGGRUPPAMENTO
040000970117IF  5C     V1SRCT        IFEQ      *BLANKS                                      *NON CALCOLARE
040100970117     C                   Z-ADD     *ZEROS        VPPE(I)                        *%INCIDENZA=0
040200970415X   5C                   ELSE
040300970415     C                   ADD       0,1           TOTPPM                         *%INCIDENZA IPOTETIC
040400970117E   5C                   ENDIF
040500970117E   4C                   ENDIF
040600970117E   3C                   ENDIF
040700970117E   2C                   ENDDO
040800970128E   1C                   ENDIF
040900970415     C*---
041000970415     C* CONTROLLA LA %INCIDENZA MINIMA DA RICHIERE
041100970415     C*---
041200970415     C* TOTALE %REGIONE >= SOMMA % MINIMA (0,1) DEI COD.TASS.ELEM. DA CALCOLARE
041300970415IF  1C     V1SPPE        IFLT      TOTPPM
041400970415     C                   MOVEL     '1'           WERR
041500970415     C                   MOVEL     MSG(23)       MSGM23
041600970415     C                   MOVEL     TOTPPM        MSGDFI
041700970415     C                   MOVE      TOTPPM        MSGDFD
041800970415     C                   MOVEL     MSGM23        $MSG
041900970415     C                   SETON                                        2390
042000970415E   1C                   ENDIF
042100970415     C*---
042200970415     C* DALLA STESSA TARIFFA
042300970415     C*---
042400970124     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
042500970415IF  1C     WERR          IFEQ      '0'
042600970124     C                   Z-ADD     *ZEROS        TOTPPE            4 1
042700970415IF  2C     V1SRCT        IFNE      ' '                                          *NON CALCOLARE
042800050103DO  3C     1             DO        600           I
042900970415IF  4C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
043000970415IF  5C     V1SCTS        IFEQ      VRAP(I)                                      *STESSA REGIONE
043100970124     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
043200970415E   5C                   ENDIF
043300970415E   4C                   ENDIF
043400970415E   3C                   ENDDO
043500970124     C*
043600970415     C* CALCOLA LE % DI INCIDENZA DEI CODICI TASSAZIONE DELLA REGIONE
043700970415IF  3C     TOTPPE        IFGT      *ZEROS
043800970415DO  4C     1             DO        WUVCTS        I
043900970415IF  5C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
044000970415IF  6C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
044100970415IF  7C     VPPE(I)       IFGT      *ZEROS
044200970129     C     VPPE(I)       MULT      V1SPPE        WN156            15 6
044300970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
044400970415IF  8C     VPPE(I)       IFEQ      *ZEROS
044500970319     C                   Z-ADD     0,1           VPPE(I)
044600970415E   8C                   ENDIF
044700970415E   7C                   ENDIF
044800970415E   6C                   ENDIF
044900970415E   5C                   ENDIF
045000970415E   4C                   ENDDO
045100970415X   3C                   ELSE
045200970124     C*---
045300970203     C* DA ALTRA TARIFFA -SE VUOTA-
045400970124     C*---
045500970117     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
045600970124     C                   Z-ADD     *ZEROS        TOTPPE            4 1
045700050103DO  4C     1             DO        600           I
045800970415IF  5C     CCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
045900970415IF  6C     V1SCTS        IFEQ      CRAP(I)                                      *STESSA REGIONE
046000970117     C                   ADD       CPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
046100970415E   6C                   ENDIF
046200970415E   5C                   ENDIF
046300970415E   4C                   ENDDO
046400970129     C*
046500970117     C* CALCOLA LE % DI INCIDENZA DI CODICI TASSAZIONE DELLA REGIONE
046600970415IF  4C     TOTPPE        IFGT      *ZEROS
046700970415DO  5C     1             DO        WUVCTS        I
046800970415IF  6C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
046900970415IF  7C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
047000970117     C                   SETOFF                                           98
047100970117     C                   Z-ADD     1             J
047200970117     C     VCTS(I)       LOOKUP    CCTS(J)                                98
047300970415IF  8C     *IN98         IFEQ      *ON
047400970129     C     CPPE(J)       MULT      V1SPPE        WN156
047500970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
047600970415IF  9C     VPPE(I)       IFEQ      *ZEROS
047700970319     C                   Z-ADD     0,1           VPPE(I)
047800970415E   9C                   ENDIF
047900970415E   8C                   ENDIF
048000970415E   7C                   ENDIF
048100970415E   6C                   ENDIF
048200970415E   5C                   ENDDO
048300970415X   4C                   ELSE
048400970129     C*---
048500970415     C* DA CARTELLO -SE VUOTA LA TARIFFA-
048600970129     C*---
048700970129     C* CALCOLA LA %TOTALE DI INCIDENZA -REGIONE- SULLA QUALE RIPROPORZIONARE
048800970129     C                   Z-ADD     *ZEROS        TOTPPE            4 1
048900050103DO  5C     1             DO        600           I
049000970415IF  6C     CTS8(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
049100970415IF  7C     V1SCTS        IFEQ      RAP8(I)                                      *STESSA REGIONE
049200970415     C                   ADD       PPE8(I)       TOTPPE                         *%INCIDENZA REGIONAL
049300970415E   7C                   ENDIF
049400970415E   6C                   ENDIF
049500970415E   5C                   ENDDO
049600970129     C*
049700970129     C* CALCOLA LE % DI INCIDENZA DI CODICI TASSAZIONE DELLA REGIONE
049800970415IF  5C     TOTPPE        IFGT      *ZEROS
049900970415DO  6C     1             DO        WUVCTS        I
050000970415IF  7C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
050100970415IF  8C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
050200970129     C                   SETOFF                                           98
050300970129     C                   Z-ADD     1             J
050400970129     C     VCTS(I)       LOOKUP    CTS8(J)                                98
050500970415IF  9C     *IN98         IFEQ      *ON
050600970129     C     PPE8(J)       MULT      V1SPPE        WN156
050700970409     C     WN156         DIV(H)    TOTPPE        VPPE(I)
050800970415IF 10C     VPPE(I)       IFEQ      *ZEROS
050900970319     C                   Z-ADD     0,1           VPPE(I)
051000970415E  10C                   ENDIF
051100970415E   9C                   ENDIF
051200970415E   8C                   ENDIF
051300970415E   7C                   ENDIF
051400970415E   6C                   ENDDO
051500970415     C*---
051600970415     C* IMPOSTA 0,1 -SE VUOTA LA CARTELLO-
051700970415     C*---
051800970415X   5C                   ELSE
051900970415DO  6C     1             DO        WUVCTS        I
052000970415IF  7C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
052100970415IF  8C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
052200970415IF  9C     VRCT(I)       IFNE      *BLANKS                                      *DA CALCOLARE
052300970415     C                   Z-ADD     0,1           VPPE(I)                        *%INCIDENZA MINIMA
052400970415E   9C                   ENDIF
052500970415E   8C                   ENDIF
052600970415E   7C                   ENDIF
052700970415E   6C                   ENDDO
052800970415E   5C                   ENDIF
052900970129     C*
053000970415E   4C                   ENDIF
053100970415E   3C                   ENDIF
053200970128     C*---
053300970128     C* PAREGGIA LA %REGIONE CON LA %TOTALE DI TUTTI I CODICI TASSAZIONE
053400970319     C*---
053500970128     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
053600970409     C                   Z-ADD     *ZEROS        TOTPPE            4 1
053700050103DO  3C     1             DO        600           I
053800970415IF  4C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
053900970415IF  5C     V1SCTS        IFEQ      VRAP(I)                                      *STESSA REGIONE
054000970409     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA REGIONAL
054100970415E   5C                   ENDIF
054200970415E   4C                   ENDIF
054300970415E   3C                   ENDDO
054400970414     C*
054500970319     C* CALCOLA LA DIFFERENZA FRA LA %REGIONALE E LA % DEI COD.TASSAZIONE (+/-)
054600970409     C                   Z-ADD     *ZEROS        WN41              4 1
054700970409     C     V1SPPE        SUB       TOTPPE        WN41
054800970415     C*
054900970415     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
055000970415IF  3C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
055100970415DO  4C     WN41          DOWNE     *ZEROS
055200970415DO  5C     1             DO        WUVCTS        I
055300970415IF  6C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
055400970415IF  7C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
055500970415IF  8C     VRCT(I)       IFNE      *BLANKS                                      *DA CALCOLARE
055600970415     C* POSITIVA -> AGGIUNGERE
055700970415IF  9C     WN41          IFGT      *ZEROS
055800970415     C     VPPE(I)       ADD       0,1           VPPE(I)                        *AGGIUNGE %
055900970415     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
056000970415E   9C                   ENDIF
056100970415     C* NEGATIVA -> TOGLIERE
056200970415IF  9C     WN41          IFLT      *ZEROS
056300100707IF 10C     VPPE(I)       IFGE      0,1
056400970415     C     VPPE(I)       SUB       0,1           VPPE(I)                        *SOTTRAE %
056500970415     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
056600970415E  10C                   ENDIF
056700970415E   9C                   ENDIF
056800970415     C* UGUALE A ZERO -> USCIRE
056900970415IF  9C     WN41          IFEQ      *ZEROS
057000970415     C                   LEAVE
057100970415E   9C                   ENDIF
057200970415E   8C                   ENDIF
057300970415E   7C                   ENDIF
057400970415E   6C                   ENDIF
057500970415E   5C                   ENDDO                                                  *FINE COD.TASS.ELEM.
057600970415E   4C                   ENDDO                                                  *DIFFERENZA = 0
057700970415E   3C                   ENDIF
057800970414     C*
057900970415E   2C                   ENDIF
058000970319E   1C                   ENDIF
058100970128     C*
058200970117     C                   ENDSR
058300970117     C*------------------------------------------------------------------------*
058400970117     C* CTRVCT - CONTROLLA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
058500970117     C*------------------------------------------------------------------------*
058600970117     C     CTRVCT        BEGSR
058700970117     C*
058800970415IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
058900970415     C                   Z-ADD     *ZEROS        TOTPPE            4 1          *% TOTALE
059000970415DO  2C     1             DO        WUVCTS        I
059100970415IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
059200970415IF  4C     VRAP(I)       IFEQ      V1SCTS                                       *STESSA REGIONE
059300970117     C                   ADD       VPPE(I)       TOTPPE                         *%INCIDENZA TOTALE
059400970129     C* DA NON CALCOLARE
059500970415IF  5C     V1SRCT        IFEQ      *BLANKS
059600970415IF  6C     WERR          IFEQ      '0'
059700970415IF  7C     VRCT(I)       IFNE      *BLANKS
059800970128     C                   MOVEL     '1'           WERR
059900970128     C                   MOVEL     MSG(6)        $MSG
060000970128     C                   SETON                                        2290
060100970415E   7C                   ENDIF
060200970415E   6C                   ENDIF
060300970128     C*
060400970415IF  6C     WERR          IFEQ      '0'
060500970415IF  7C     VPPE(I)       IFGT      *ZEROS
060600970117     C                   MOVEL     '1'           WERR
060700970128     C                   MOVEL     MSG(7)        $MSG
060800970117     C                   SETON                                        2290
060900970415E   7C                   ENDIF
061000970415E   6C                   ENDIF
061100970415X   5C                   ELSE
061200970129     C* DA CALCOLARE
061300970415IF  6C     V1SRCT        IFNE      'M'                                          *NO SITUAZIONE MISTA
061400970415IF  7C     WERR          IFEQ      '0'
061500970415IF  8C     VRCT(I)       IFNE      V1SRCT
061600970117     C                   MOVEL     '1'           WERR
061700970128     C                   MOVEL     MSG(8)        $MSG
061800970117     C                   SETON                                        2290
061900970415E   8C                   ENDIF
062000970415E   7C                   ENDIF
062100970415IF  7C     WERR          IFEQ      '0'
062200970415IF  8C     VPPE(I)       IFEQ      *ZEROS
062300970129     C                   MOVEL     '1'           WERR
062400970129     C                   MOVEL     MSG(9)        $MSG
062500970129     C                   SETON                                        2290
062600970415E   8C                   ENDIF
062700970415E   7C                   ENDIF
062800970129     C*
062900970415X   6C                   ELSE                                                   *SITUAZIONE MISTA
063000970129     C*
063100970415IF  7C     WERR          IFEQ      '0'
063200970415IF  8C     VRCT(I)       IFNE      *BLANKS
063300970129     C     VPPE(I)       ANDEQ     *ZEROS
063400970129     C                   MOVEL     '1'           WERR
063500970129     C                   MOVEL     MSG(21)       $MSG
063600970129     C                   SETON                                        2290
063700970415E   8C                   ENDIF
063800970415E   7C                   ENDIF
063900970128     C*
064000970415IF  7C     WERR          IFEQ      '0'
064100970415IF  8C     VRCT(I)       IFEQ      *BLANKS
064200970129     C     VPPE(I)       ANDGT     *ZEROS
064300970128     C                   MOVEL     '1'           WERR
064400970129     C                   MOVEL     MSG(22)       $MSG
064500970129     C                   SETON                                        2290
064600970415E   8C                   ENDIF
064700970415E   7C                   ENDIF
064800970415E   6C                   ENDIF
064900970415E   5C                   ENDIF
065000970117     C*
065100970415E   4C                   ENDIF
065200970415E   3C                   ENDIF
065300970415E   2C                   ENDDO
065400970117     C*
065500970117     C* TOTALE %INCIDENZA CODICI TASSAZIONE = %INCIDENZA REGIONE
065600970117IF  2C     V1SPPE        IFNE      TOTPPE
065700970117     C                   MOVEL     '1'           WERR
065800970128     C                   MOVEL     MSG(10)       $MSG
065900970117     C                   SETON                                        2390
066000970117E   2C                   ENDIF
066100970117E   1C                   ENDIF
066200970117     C*
066300970117     C                   ENDSR
066400970117     C*------------------------------------------------------------------------*
066500970117     C* GESOP1 - GESTIONE OPZIONI DEL PRIMO SUBFILE
066600970117     C*------------------------------------------------------------------------*
066700970117     C     GESOP1        BEGSR
066800970122     C*
066900970129     C                   MOVEL     V1SOPZ        DEPOPZ
067000970129     C                   EXSR      AGGSF1                                       *AGGIORNA SFILE
067100970117SL  1C                   SELECT
067200970129     C     DEPOPZ        WHENEQ    '1 '
067300970117     C                   EXSR      O01GE1                                       *CODICI TASSAZIONE
067400970117     C                   OTHER
067500970117E   1C                   ENDSL
067600970117     C*
067700970117     C                   ENDSR
067800970117     C*------------------------------------------------------------------------*
067900970117     C* O01GE1 - GESTIONE OPZIONE PRIMO SFILE 1 ==> CODICI TASSAZIONE ELEMENTARI
068000970117     C*------------------------------------------------------------------------*
068100970117     C     O01GE1        BEGSR
068200970117     C*
068300970117     C* GESTIONE 3° SFILE
068400970117     C                   Z-ADD     NR1           $DPC1                          *DEPOSITO NRR
068500970117     C                   EXSR      IMPSF3                                       *IMPOSTAZIONI
068600970129DO  1C     $FINV         DOUEQ     'S'
068700970117     C                   EXSR      GESSF3                                       *GESTIONE
068800970129E   1C                   ENDDO
068900970129     C                   Z-ADD     $DPC1         NR1                            *DEPOSITO NRR
069000970117     C*
069100970117     C                   ENDSR
069200970117     C*------------------------------------------------------------------------*
069300970117     C* AGGSF1 - AGGIORNA RIGA DEL PRIMO SUBFILE
069400970117     C*------------------------------------------------------------------------*
069500970117     C     AGGSF1        BEGSR
069600970117     C*
069700970124     C                   MOVEL     *BLANKS       V1SOPZ
069800970117     C                   SETON                                        70        *SI SFLNXTCHG
069900970117IF  1C     $TGES         IFEQ      'I'
070000970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
070100970117X   1C                   ELSE
070200970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
070300970117E   1C                   ENDIF
070400970117IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
070500970117     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
070600970127X   1C                   ELSE                                                   *DA PORTO
070700970117     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
070800970117E   1C                   ENDIF
070900970117     C                   UPDATE    TE03S01                                      *AGGIORNA SUBFILE
071000970117     C                   SETOFF                                       70        *NO SFLNXTCHG
071100970117     C*
071200970117     C                   ENDSR
071300970117     C*------------------------------------------------------------------------*
071400970124     C* ENTSF1 - GESTIONE TASTO FUNZIONALE INVIO 1°SFILE  ==> AGGIORNA
071500970117     C*------------------------------------------------------------------------*
071600970124     C     ENTSF1        BEGSR
071700970117     C*
071800970117IF  1C     $TGES         IFEQ      'G'                                          *SE GESTIONE
071900970117     C* AGGIORNA L'ARCHIVIO
072000980508     C*---                 EXSR AGGETT
072100970129     C*
072200970129     C* AGGIORNA LE SCHIERE VIDEO
072300970129     C                   EXSR      AGGRCT
072400970129     C*
072500970117     C* CARICA IL SUBFILE
072600970117     C                   EXSR      POSSF1
072700970117E   1C                   ENDIF
072800970117     C*
072900970117     C                   ENDSR
073000970117     C*------------------------------------------------------------------------*
073100970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
073200970117     C*------------------------------------------------------------------------*
073300970117     C     F03GES        BEGSR
073400970117     C*
073500970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
073600970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
073700011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
073800970117     C*
073900970117     C                   ENDSR
074000970117     C*------------------------------------------------------------------------*
074100970117     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
074200970117     C*------------------------------------------------------------------------*
074300970117     C     F05GES        BEGSR
074400970117     C*
074500970117     C* GESTIONE VIDETA
074600970117     C                   EXSR      IMPWD2
074700970117DO  1C     $FINV         DOUEQ     'S'
074800970117     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
074900970117E   1C                   ENDDO
075000970117     C*
075100970117     C* MEMORIZZA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA E CARICA IL SFILE
075200970117IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
075300980708     C                   MOVEL     V2WCTO        WCTO
075400980708     C                   Z-ADD     V2WKSC        WKSC
075500980708     C                   Z-ADD     V2WCTR        WCTR
075600980708     C                   Z-ADD     V2WPRG        WPRG
075700980708     C                   Z-ADD     V2WLNP        WLNP
075800980708     C                   MOVEL     D00FIE        WFIE
075900011003     C                   MOVEL     V2WDIV        WDIV
076000970117     C                   EXSR      MEMETT                                       *MEMORIZZA
076100970117     C                   EXSR      POSSF1                                       *CARICA
076200091123     C                   MOVEL     'S'           D0DDSF                         *DEPOSITA CHIAVE CAR
076300980708     C                   MOVEL     V2WCTO        D0DCTO
076400980708     C                   Z-ADD     V2WKSC        D0DKSC
076500980708     C                   Z-ADD     V2WCTR        D0DCTR
076600980708     C                   Z-ADD     V2WPRG        D0DPRG
076700980708     C                   Z-ADD     V2WLNP        D0DLNP
076800011003     C                   MOVEL     V2WDIV        D0DDIV
076900970117E   1C                   ENDIF
077000970117     C*
077100970117     C                   ENDSR
077200970117     C*------------------------------------------------------------------------*
077300970117     C* MEMETT - MEMORIZZA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
077400970117     C*------------------------------------------------------------------------*
077500970117     C     MEMETT        BEGSR
077600970117     C*
077700970117     C* CARICA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
077800970117     C                   EXSR      CARETT
077900980508     C*
078000980508     C* AGGIORNA LE SCHIERE VIDEO
078100980508     C                   EXSR      AGGSCV
078200980508     C*
078300980508     C                   ENDSR
078400980508     C*------------------------------------------------------------------------*
078500970117     C* CARETT - CARICA I CODICI TASSAZIONE DELLA TARIFFA RICHIESTA
078600970117     C*------------------------------------------------------------------------*
078700970117     C     CARETT        BEGSR
078800980508     C*
078900980508     C* VALORIZZA I CODICI TASSAZIONE CON QUELLI CARICATI DA TABELLA "CT"
079000980508     C                   MOVEL     VCTS          CCTS
079100980514     C                   MOVEL     VRAP          CRAP
079200970117     C*
079300980508     C* PULISCE LE SCHIERE DI MEMORIZZAZIONE
079400980508     C                   CLEAR                   CRCT                           *SCHIERE CARICAMENTO
079500970117     C                   CLEAR                   CPPE
079600970117     C*
079700980508     C* CARICA DATI CODICI TASSAZIONE
079800091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
079900980708     C                   MOVEL     WCTO          KTTCTO                         *TARIFFA O OFFERTA
080000980708     C                   Z-ADD     WKSC          KTTKSC                         *CLIENTE O VISITA
080100980708     C                   Z-ADD     WCTR          KTTCTR                         *TARIFFA
080200980708     C                   Z-ADD     WPRG          KTTPRG                         *PROGRESSIVO
080300980708     C                   Z-ADD     WLNP          KTTLNP                         *LINEA DI PARTENZA
080400980708     C                   MOVEL     WFIE          KTTFIE                         *ITALIA/ESTERO
080500011001     C     KE2ETT        SETLL     TEETT01L                               99
080600011001     C     KE2ETT        READE(N)  TEETT01L                               99
080700970117DO  1C     *IN99         DOWEQ     *OFF
080800980508IF  2C     ETTATB        IFEQ      *BLANKS                                      *NON ANNULLATO
080900980508     C                   SETOFF                                           98
081000980508     C                   Z-ADD     1             J
081100980515     C     ETTCTS        LOOKUP    CCTS(J)                                98
081200980508IF  3C     *IN98         IFEQ      *ON                                          *CODICE TROVATO
081300980508     C                   MOVEL     ETTRCT        CRCT(J)                        *RAGGRUPPAMENTO
081400980508     C                   Z-ADD     ETTPPE        CPPE(J)                        *%INCIDENZA
081500980911     C     ETTRCT        IFEQ      'N'
081600980911     C     ETTPPE        ANDEQ     *ZEROS
081700980911     C                   MOVEL     *BLANKS       CRCT(J)
081800980911     C                   ENDIF
081900970129E   3C                   ENDIF
082000970129E   2C                   ENDIF
082100011001     C     KE2ETT        READE(N)  TEETT01L                               99
082200970117E   1C                   ENDDO
082300980508     C*
082400970117     C                   ENDSR
082500980508     C*------------------------------------------------------------------------*
082600980508     C* CARSTD - CARICA I CODICI TASSAZIONE DA TABELLA
082700980508     C*------------------------------------------------------------------------*
082800980508     C     CARSTD        BEGSR
082900980508     C*
083000980508     C* VALORIZZA I CODICI TASSAZIONE CON QUELLI CARICATI DA TABELLA "CT"
083100980708     C                   MOVEL     VCTS          CCTS
083200980708     C                   MOVEL     VRAP          CRAP
083300980514     C                   MOVEL     'T'           CRCT
083400980508     C                   Z-ADD     000,1         CPPE
083500980508     C*
083600980508     C                   ENDSR
083700980508     C*------------------------------------------------------------------------*
083800980508     C* AGGSCV - AGGIORNA LE SCHIERE VIDEO
083900980508     C*------------------------------------------------------------------------*
084000980508     C     AGGSCV        BEGSR
084100980508     C*
084200980508     C* PULISCE LE VARIABILI DI LAVORO
084300980508     C                   CLEAR                   VRCT                           *SCHIERE VIDEO
084400980902     C                   CLEAR                   VPPE
084500980508     C*
084600980508     C* MEMORIZZA I DATI TIPO DELLA TARIFFA RICHIESTA NELLE SCHIERE VIDEO
084700980508DO  1C     1             DO        WUVCTS        J
084800980508IF  2C     CCTS(J)       IFNE      *BLANKS
084900980508     C                   SETOFF                                           98
085000980508     C                   Z-ADD     1             I
085100980508     C     CCTS(J)       LOOKUP    VCTS(I)                                98
085200980508IF  3C     *IN98         IFEQ      *ON
085300050304     c* verifico se per il cod. tassazione esiste un raggruppam. imposto
085400050304     C                   MOVEL     CRCT(J)       VRCT(I)                        *RAGGRUPPAMENTO
085500050304     c                   if        crct(j) <> ' '
085600050304     c                   z-add     1             bb                3 0
085700050304     c     ccts(j)       lookup    cts(bb)                                51
085800050304     c                   if        *in51
085900050304     c                             and rct(bb) <> *blanks
086000050304     C                   MOVEL     rct(bb)       vrct(i)
086100050304     c                   end
086200050304     c                   end
086300050304     C* PORTO -> CARICA LE %INCIDENZA E I RAGGRUPPAMENTI DI TUTTI I CODICI
086400980508     C                   Z-ADD     CPPE(J)       VPPE(I)                        *%INCIDENZA
086500980508E   3C                   ENDIF
086600980508E   2C                   ENDIF
086700980508E   1C                   ENDDO
086800980508     C*
086900980508     C* AGGIORNA I CODICI TASSAZIONE DELLA REGIONE
087000980508     C                   EXSR      AGGRCT
087100980508     C*
087200980508     C                   ENDSR
087300970117     C*------------------------------------------------------------------------*
087400970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
087500970117     C*------------------------------------------------------------------------*
087600970117     C     F06GES        BEGSR
087700970117     C*
087800970117IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
087900970117     C*---
088000970117     C* TOTALE %INCIDENZA = 100
088100970117     C*---
088200970117IF  2C     V1CPPE        IFNE      100,0
088300970117     C                   SETON                                        2990
088400970117     C                   MOVEL     MSG(5)        $MSG
088500970117     C                   MOVEL     '1'           WERR
088600970117     C                   Z-ADD     1             $NR1                           *INIZIO SFILE
088700970117E   2C                   ENDIF
088800970117     C* AGGIORNA ARCHIVIO
088900970117IF  2C     WERR          IFEQ      '0'                                          *NO ERRORI
089000970117     C                   EXSR      AGGETT
089100970129     C*
089200970129     C* AGGIORNA LE SCHIERE VIDEO
089300970129     C                   EXSR      AGGRCT
089400970117     C*---
089500970117     C* PORTO -> GESTIONE VIDETA DI CONFERMA
089600970117     C*---
089700970117IF  3C     D00TLA        IFEQ      'P'
089800970117     C                   EXSR      IMPWD3
089900970117DO  4C     $FINV         DOUEQ     'S'
090000970117     C                   EXSR      GESWD3                                       *GESTIONE FINESTRA
090100970117E   4C                   ENDDO
090200970117IF  4C     V3WRIS        IFEQ      'SI'                                         *SE CONFERMATO
090300970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
090400011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
090500970117E   4C                   ENDIF
090600970117     C*---
090700970117     C* DELTA -> ESCE E PROSEGUE
090800970117     C*---
090900970117X   3C                   ELSE
091000011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
091100970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
091200970117E   3C                   ENDIF
091300970117E   2C                   ENDIF
091400970117X   1C                   ELSE                                                   INTERROGAZIONE
091500011001     C                   MOVEL     'S'           D00ETT                         *S=TEETT CONTROLLATO
091600970130     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
091700970117E   1C                   ENDIF
091800970117     C*
091900970117     C                   ENDSR
092000970117     C*------------------------------------------------------------------------*
092100011001     C* AGGETT - AGGIORNAMENTO ARCHIVIO -TEETT-
092200970117     C*------------------------------------------------------------------------*
092300970124     C     AGGETT        BEGSR
092400980515     C*
092500980515     C* ANNULLA TUTTI I DATI DELLA TARIFFA IN LINEA
092600091123     C                   MOVEL     'S'           KTTDSF
092700980515     C                   MOVEL     D00CTO        KTTCTO
092800980515     C                   Z-ADD     D00KSC        KTTKSC
092900980515     C                   Z-ADD     D00CTR        KTTCTR
093000980515     C                   Z-ADD     D00PRG        KTTPRG
093100980515     C                   Z-ADD     D00LNP        KTTLNP
093200980515     C                   MOVEL     D00FIE        KTTFIE
093300011001     C     KE2ETT        SETLL     TEETT01L                               99
093400011001     C     KE2ETT        READE     TEETT01L                               99
093500980515DO  1C     *IN99         DOWEQ     *OFF
093600980515IF  2C     ETTDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
093700980515     C                   MOVEL     'A'           ETTATB
093800980515     C                   MOVEL     *BLANKS       ETTFTR
093900011001     C                   UPDATE    TEETT000                                     *AGGIORNA
094000980515X   2C                   ELSE                                                   *MAI TRASMESSO
094100011001     C                   DELETE    TEETT000                                     *CANCELLA
094200980515E   2C                   ENDIF
094300011001     C     KE2ETT        READE     TEETT01L                               99
094400980515E   1C                   ENDDO
094500011001     C                   UNLOCK    TEETT01L
094600970117     C*
094700970117     C* LEGGE TUTTI I RECORD DEL SFILE
094800970117DO  2C     1             DO        WUVCTS        I
094900970117IF  2C     VCTS(I)       IFNE      *BLANKS                                      *SE IMMESSO QC.
095000970117     C*
095100970117     C* LEGGE IL RECORD IN ARCHIVIO
095200091123     C                   MOVEL     'S'           KTTDSF
095300980508     C                   MOVEL     D00CTO        KTTCTO
095400970117     C                   Z-ADD     D00KSC        KTTKSC
095500970117     C                   Z-ADD     D00CTR        KTTCTR
095600970117     C                   Z-ADD     D00PRG        KTTPRG
095700970117     C                   Z-ADD     D00LNP        KTTLNP
095800970117     C                   MOVEL     D00FIE        KTTFIE
095900970117     C                   MOVEL     VCTS(I)       KTTCTS
096000011001     C     KEYETT        CHAIN     TEETT01L                           99
096100970117     C* INSERIMENTO
096200970117IF  3C     *IN99         IFEQ      *ON
096300970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
096400011001     C                   WRITE     TEETT000
096500970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZIONE
096600970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
096700970117X   3C                   ELSE
096800970117     C* VARIAZIONE
096900970117IF  4C     VRCT(I)       IFNE      ETTRCT                                       *SE MODIFICATO QC.
097000970117     C     VPPE(I)       ORNE      ETTPPE
097100970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
097200020715X   4C                   ELSE
097300020716     C                   MOVEL     *BLANKS       ETTFTR
097400020716     C                   Z-ADD     DATCOR        ETTDUV
097500020715     C                   MOVEL     *BLANKS       ETTATB
097600020715E   4C                   ENDIF
097700011001     C                   UPDATE    TEETT000
097800970117E   3C                   ENDIF
097900970117E   2C                   ENDIF
098000970117E   1C                   ENDDO                                                  *FINE SUBFILE
098100970117     C*
098200970117     C                   ENDSR
098300970117     C*------------------------------------------------------------------------*
098400011001     C* IMPETT - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETT-
098500970117     C*------------------------------------------------------------------------*
098600970117     C     IMPETT        BEGSR
098700970117     C*
098800970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
098900980508     C                   MOVEL     KTTDSF        ETTDSF
099000980508     C                   MOVEL     KTTCTO        ETTCTO
099100970117     C                   Z-ADD     KTTKSC        ETTKSC
099200970117     C                   Z-ADD     KTTCTR        ETTCTR
099300970117     C                   Z-ADD     KTTPRG        ETTPRG
099400970117     C                   Z-ADD     KTTLNP        ETTLNP
099500970117     C                   MOVEL     KTTFIE        ETTFIE
099600970117     C                   MOVEL     KTTCTS        ETTCTS
099700970117     C                   Z-ADD     DATCOR        ETTDIR
099800970117     C                   Z-ADD     *ZEROS        ETTDUV
099900970117     C                   MOVEL     *BLANKS       ETTFTR
100000970117     C                   Z-ADD     *ZEROS        ETTDTR
100100970117X   1C                   ELSE                                                   *VARIAZIONE
100200970117     C                   MOVEL     *BLANKS       ETTFTR
100300970117     C                   Z-ADD     DATCOR        ETTDUV
100400970117E   1C                   ENDIF
100500970117     C                   MOVEL     VRCT(I)       ETTRCT
100600970117     C                   Z-ADD     VPPE(I)       ETTPPE
100700970117     C                   MOVEL     *BLANKS       ETTATB
100800970117     C*
100900970117     C                   ENDSR
101000970117     C*------------------------------------------------------------------------*
101100970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
101200970117     C*------------------------------------------------------------------------*
101300970117     C     F12GES        BEGSR
101400970117     C*
101500970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
101600970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
101700011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
101800970117     C*
101900970117     C                   ENDSR
102000970117     C*------------------------------------------------------------------------*
102100970117     C* F07GES - GESTIONE TASTO FUNZIONALE F07 ==> RAPPRESENTAZIONE
102200970117     C*------------------------------------------------------------------------*
102300970117     C     F07GES        BEGSR
102400970117     C*
102500970117     C* SVILUPPA IL TIPO DI RAPPRESENTAZIONE
102600970117     C                   EXSR      SVIRAP
102700970117     C*
102800970117     C* GESTIONE VIDEATA
102900970117     C                   EXSR      IMPSF2
103000970117DO  1C     $FINV         DOUEQ     'S'
103100970117     C                   EXSR      GESSF2                                       *GESTIONE VIDEATA
103200970117E   1C                   ENDDO
103300970117     C*
103400970117     C                   ENDSR
103500970117     C*------------------------------------------------------------------------*
103600970117     C* SVIRAP - SVILUPPA IL TIPO DI RAPPRESENTAZIONE
103700970117     C*------------------------------------------------------------------------*
103800970117     C     SVIRAP        BEGSR
103900970117     C*
104000970117     C* PULISCE LE SCHIERE VIDEO DI OUTPUT
104100970117     C                   CLEAR                   OCTS                           *CODICE
104200970117     C                   CLEAR                   OPPE                           *%INCIDENZA
104300970117     C                   CLEAR                   OCOR                           *ORDINAMENTO
104400970117     C*
104500970117     C                   Z-ADD     *ZEROS        J                 4 0
104600970117DO  1C     1             DO        WUVCTS        I
104700970117IF  2C     VCTS(I)       IFNE      *BLANKS                                      *ELEMENTO PIENO
104800970117     C* DA CALCOLARE
104900970117IF  3C     VRCT(I)       IFEQ      'T'
105000970117     C                   ADD       1             J
105100970117     C                   MOVEL     VCTS(I)       OCTS(J)                        *CODICE
105200970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
105300970117     C                   MOVEL     VCOR(I)       OCOR(J)                        *ORDINAMENTO
105400970117X   3C                   ELSE
105500970117     C                   MOVEL     VCTS(I)       WCTS
105600970117     C                   MOVEL     *BLANKS       WRAP
105700970117     C                   MOVEL     *BLANKS       WNAZ
105800970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
105900970117     C* DA CALCOLARE IN REGIONE
106000970117IF  4C     VRCT(I)       IFEQ      'R'
106100970117     C                   SETOFF                                           98
106200970117     C                   Z-ADD     1             C                 4 0
106300970117     C     WRAP          LOOKUP    OCTS(C)                                98
106400970117IF  5C     *IN98         IFEQ      *OFF                                         *MAI  MEMORIZZATO
106500970117     C                   MOVEL     WRAP          WCTS
106600970117     C                   MOVEL     *BLANKS       WCOR
106700970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
106800970117     C                   ADD       1             J
106900970117     C                   MOVEL     WRAP          OCTS(J)                        *CODICE
107000970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
107100970117     C                   MOVEL     WCOR          OCOR(J)                        *ORDINAMENTO
107200970117X   5C                   ELSE                                                   *GIA' MEMORIZZATO
107300970117     C     VPPE(I)       ADD       OPPE(C)       OPPE(C)                        *SOMMA PESI
107400970117E   5C                   ENDIF
107500970117E   4C                   ENDIF
107600970117     C* DA CALCOLARE IN NAZIONE
107700970117IF  4C     VRCT(I)       IFEQ      'N'
107800970117     C                   SETOFF                                           98
107900970117     C                   Z-ADD     1             C                 4 0
108000970117     C     WNAZ          LOOKUP    OCTS(C)                                98
108100970117IF  5C     *IN98         IFEQ      *OFF                                         *MAI  MEMORIZZATO
108200970117     C                   MOVEL     WNAZ          WCTS
108300970117     C                   MOVEL     *BLANKS       WCOR
108400970117     C                   EXSR      DATCTS                                       *REPERISCE DATI CTS
108500970117     C                   ADD       1             J
108600970117     C                   MOVEL     WNAZ          OCTS(J)                        *CODICE
108700970117     C                   Z-ADD     VPPE(I)       OPPE(J)                        *%INCIDENZA
108800970117     C                   MOVEL     WCOR          OCOR(J)                        *ORDINAMENTO
108900970117X   5C                   ELSE                                                   *GIA' MEMORIZZATO
109000970117     C     VPPE(I)       ADD       OPPE(C)       OPPE(C)                        *SOMMA PESI
109100970117E   5C                   ENDIF
109200970117E   4C                   ENDIF
109300970117E   3C                   ENDIF
109400970117E   2C                   ENDIF
109500970117E   1C                   ENDDO
109600970117     C                   Z-ADD     J             WUOCTS            4 0          *N°COD.TASS.OUTPUT
109700970117     C*
109800970117     C* ORDINA CODICI RAGGRUPPATI VIDEO
109900970117     C                   EXSR      ORDOCT
110000970117     C*
110100970117     C                   ENDSR
110200970117     C*------------------------------------------------------------------------*
110300970117     C* DATCTS - REPERISCE I DATI DEL CODICE TASSAZIONE
110400970117     C*------------------------------------------------------------------------*
110500970117     C     DATCTS        BEGSR
110600970117     C*
110700970117     C     *LIKE         DEFINE    ETTCTS        WCTS
110800970117     C     *LIKE         DEFINE    ETTCTS        WRAP
110900970117     C     *LIKE         DEFINE    ETTCTS        WNAZ
111000970117     C     *LIKE         DEFINE    §CTCOR        WCOR
111100970117     C*
111200970117     C                   SETOFF                                           98
111300970117     C                   Z-ADD     1             C                 4 0
111400970117     C     WCTS          LOOKUP    CTS(C)                                 98
111500970117IF  1C     *IN98         IFEQ      *ON
111600970117     C                   MOVEL     RAP(C)        WRAP                           *COD.TASS.REGIONE
111700970117     C                   MOVEL     NAZ(C)        WNAZ                           *COD.TASS.NAZIONE
111800970117     C                   MOVEL     COR(C)        WCOR                           *COD.ORDINAMENTO
111900970117E   1C                   ENDIF
112000970117     C*
112100970117     C                   ENDSR
112200970117     C*------------------------------------------------------------------------*
112300970117     C* ORDOCT - ORDINA I CODICI TASSAZIONE RAGGRUPPATI DA EMETTERE A VIDEO
112400970117     C*------------------------------------------------------------------------*
112500970117     C     ORDOCT        BEGSR
112600970117     C*
112700970117DO  1C     1             DO        WUOCTS        I
112800970117IF  2C     OCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
112900970117     C     1             ADD       I             J                 4 0
113000970117DO  3C     J             DO        WUOCTS        J
113100970117IF  4C     OCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
113200970117IF  5C     OCOR(I)       IFGT      OCOR(J)
113300970117     C                   MOVEL     OCTS(I)       PCTS                           *CODICE
113400970117     C                   MOVEL     OCTS(J)       OCTS(I)
113500970117     C                   MOVEL     PCTS          OCTS(J)
113600970117     C                   MOVEL     OPPE(I)       PPPE                           *%INCIDENZA
113700970117     C                   MOVEL     OPPE(J)       OPPE(I)
113800970117     C                   MOVEL     PPPE          OPPE(J)
113900970117     C                   MOVEL     OCOR(I)       PCOR                           *ORDINAMENTO
114000970117     C                   MOVEL     OCOR(J)       OCOR(I)
114100970117     C                   MOVEL     PCOR          OCOR(J)
114200970117E   5C                   ENDIF
114300970117E   4C                   ENDIF
114400970117E   3C                   ENDDO                                                  *FINE CICLO UNO
114500970117E   2C                   ENDIF
114600970117E   1C                   ENDDO                                                  *FINE CICLO DUE
114700970117     C*
114800970117     C                   ENDSR
114900970117     C*------------------------------------------------------------------------*
115000970117     C* IMPSF2 - IMPOSTAZIONI INIZIALI SECONDO SUBFILE
115100970117     C*------------------------------------------------------------------------*
115200970117     C     IMPSF2        BEGSR
115300970117     C*
115400970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
115500970129     C                   MOVEL     '2'           TIPVID                         *VIDEO IN GESTIONE
115600970117     C                   EXSR      SETSF2                                       *SFILE
115700970117     C                   MOVEL     'N'           $FINV                          *FINE VIDEATA
115800970117     C* CARICA IL SUBFILE
115900970117     C                   EXSR      POSSF2
116000970117     C*
116100970117     C                   ENDSR
116200970117     C*------------------------------------------------------------------------*
116300970117     C* SETSF2 - IMPOSTAZIONI INDICATORI SECONDO SUBFILE
116400970117     C*------------------------------------------------------------------------*
116500970117     C     SETSF2        BEGSR
116600970117     C*
116700970117     C                   SETOFF                                       90         GENERICO DI ERRORE
116800970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
116900970117     C*
117000970117     C                   ENDSR
117100970117     C*------------------------------------------------------------------------*
117200970117     C* GESSF2 - GESTIONE SECONDO SUBFILE
117300970117     C*------------------------------------------------------------------------*
117400970117     C     GESSF2        BEGSR
117500970117     C*
117600970117     C* EMISSIONE VIDEATA
117700970117     C                   WRITE     TE03D00                                      *TESTATA
117800970117     C                   WRITE     TE03Z02                                      *PIEDI
117900970117     C                   EXFMT     TE03C02                                      *SFILE
118000970117     C                   EXSR      SETSF2
118100970117     C* F12-RITORNO
118200970117IF  1C     *INKL         IFEQ      *ON
118300970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
118400970130     C                   MOVEL     '1'           TIPVID
118500970117     C                   GOTO      FINSF2
118600970117E   1C                   ENDIF
118700970117     C* INVIO-RITORNA
118800970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
118900970117     C*
119000970117     C     FINSF2        ENDSR
119100970117     C*------------------------------------------------------------------------*
119200970117     C* IMPSF3 - IMPOSTAZIONI INIZIALI TERZO SUBFILE
119300970117     C*------------------------------------------------------------------------*
119400970117     C     IMPSF3        BEGSR
119500970117     C*
119600970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI SUBFILE
119700970129     C                   MOVEL     '3'           TIPVID                         *VIDEO IN GESTIONE
119800970117     C                   EXSR      SETSF3                                       *SFILE
119900970117     C*
120000970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI CONTROL
120100970117     C                   EXSR      SETSC3                                       *SFILE CONTROL
120200970117     C* CARICA IL SUBFILE
120300970129     C*---       $DPC1     CHAINTE03S01              98    *RILEGGE COD.TASS.
120400970117     C                   EXSR      POSSF3
120500970117     C*
120600970117     C                   ENDSR
120700970117     C*------------------------------------------------------------------------*
120800970117     C* SETSF3 - IMPOSTAZIONI INDICATORI TERZO SUBFILE
120900970117     C*------------------------------------------------------------------------*
121000970117     C     SETSF3        BEGSR
121100970117     C*
121200970117     C                   SETOFF                                       19         ERRORI CONTROL
121300970117     C                   SETOFF                                       2021       ERRORI SFILE
121400970117     C                   SETOFF                                       90         GENERICO DI ERRORE
121500970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
121600970129     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
121700970117     C*
121800970117     C                   ENDSR
121900970117     C*------------------------------------------------------------------------*
122000970117     C* SETSC3 - IMPOSTAZIONI INDICATORI E CAMPI TERZO SUBFILE CONTROL
122100970117     C*------------------------------------------------------------------------*
122200970117     C     SETSC3        BEGSR
122300970117     C*
122400970117     C                   MOVEL     V1SCTS        V3CRAP
122500970117     C                   MOVEL     V1SDCT        V3CDRA
122600970117     C                   MOVEL     V1SRCT        V3CRCT
122700970117     C                   MOVEL     V1SDRC        V3CDRC
122800970117     C                   Z-ADD     V1SPPE        V3CPPR
122900970117     C                   Z-ADD     *ZEROS        V3CPPE
123000970117     C*
123100970117     C                   ENDSR
123200970117     C*------------------------------------------------------------------------*
123300970117     C* GESSF3 - GESTIONE TERZO SUBFILE
123400970117     C*------------------------------------------------------------------------*
123500970117     C     GESSF3        BEGSR
123600970117     C*
123700970117     C* EMISSIONE VIDEATA
123800970117IF  1C     $TESVD        IFEQ      '1'
123900970117     C                   WRITE     TE03D00                                      *TESTATA
124000970117     C                   MOVEL     '0'           $TESVD
124100970117E   1C                   ENDIF
124200970117     C                   WRITE     TE03Z03                                      *PIEDI
124300970117     C                   EXFMT     TE03C03                                      *SFILE
124400970117     C                   EXSR      SETSF3
124500970117     C* F12-RITORNO
124600970117IF  1C     *INKL         IFEQ      *ON
124700970129     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
124800970117     C                   MOVEL     '1'           TIPVID
124900970117     C                   EXSR      SETSF1                                       *REIMPOSTA INDICATOR
125000970117     C                   GOTO      FINSF3
125100970117E   1C                   ENDIF
125200970117     C* CONTROLLA VIDEO
125300970117     C                   EXSR      CTRSF3
125400970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
125500970117     C                   GOTO      FINSF3
125600970117E   1C                   ENDIF
125700970317     C* INVIO-AGGIORNA
125800970124     C                   EXSR      ENTSF3
125900970117     C*
126000970117     C     FINSF3        ENDSR
126100970117     C*------------------------------------------------------------------------*
126200970117     C* CTRSF3 - CONTROLLA TERZO SUBFILE
126300970117     C*------------------------------------------------------------------------*
126400970117     C     CTRSF3        BEGSR
126500970117     C*
126600970117     C* REIMPOSTA VARIABILI DI LAVORO
126700970117     C                   MOVEL     '0'           WERR              1            *ERRORE
126800970117     C                   Z-ADD     *ZEROS        V3CPPE                         *TOTALE PESI %
126900970117     C*
127000970117     C* LEGGE TUTTI I RECORD DEL SFILE
127100970117     C                   Z-ADD     1             NR3
127200970117     C     NR3           CHAIN     TE03S03                            98
127300970117DO  1C     *IN98         DOWEQ     *OFF
127400970117     C                   SETOFF                                       80        *NO SFLNXTCHG
127500970117     C*--------------------
127600970117     C* RAGGRUPPAMENTO
127700970117     C*--------------------
127800970117IF  2C     WERR          IFEQ      '0'
127900970117     C                   MOVEL     V3SCTS        WCTS
128000970117     C                   MOVEL     V3SRCT        WRCT
128100970117     C                   MOVEL     *BLANKS       WDRCT
128200050304     c                   clear                   $msg
128300970117     C                   EXSR      CHKRCT
128400970117     C                   MOVEL     WRCT          V3SRCT
128500970117     C                   MOVEL     WDRCT         V3SDRC
128600970117IF  3C     WERR          IFEQ      '1'
128700970117     C                   SETON                                        2090
128800050304     c                   if        $msg = *blanks
128900970117     C                   MOVEL     MSG(2)        $MSG
129000970117E   3C                   ENDIF
129100050304E   3C                   ENDIF
129200970117E   2C                   ENDIF
129300970117     C*--------------------
129400970117     C* %INCIDENZA
129500970117     C*--------------------
129600970117IF  2C     WERR          IFEQ      '0'
129700970117IF  3C     V3SRCT        IFNE      *BLANKS                                      *DA CALCOLARE
129800970117IF  4C     V3SPPE        IFEQ      *ZEROS                                       *% VUOTA
129900970117     C                   SETON                                        2190
130000970117     C                   MOVEL     MSG(3)        $MSG
130100970117     C                   MOVEL     '1'           WERR
130200970117E   4C                   ENDIF
130300970117X   3C                   ELSE                                                   *DA NON CALCOLARE
130400970117IF  4C     V3SPPE        IFGT      *ZEROS                                       *% PIENA
130500970117     C                   SETON                                        2190
130600970117     C                   MOVEL     MSG(4)        $MSG
130700970117     C                   MOVEL     '1'           WERR
130800970117E   4C                   ENDIF
130900970117E   3C                   ENDIF
131000970117E   2C                   ENDIF
131100970117     C* AGGIORNA SFILE
131200970117IF  2C     WERR          IFEQ      '1'
131300970117     C                   Z-ADD     NR3           $NR3                           *POSIZIONAMENTO
131400970117E   2C                   ENDIF
131500970319     C*
131600970319     C     V3CPPE        ADD       V3SPPE        V3CPPE                         *TOTALE PESI %
131700970117     C                   EXSR      AGGSF3
131800970319     C*
131900970117IF  2C     WERR          IFEQ      *ON                                          * E R R O R E
132000970117     C                   GOTO      FINCT3                                       *ESCE
132100970117X   2C                   ELSE                                                   *N O N  ERRORE
132200970117     C                   ADD       1             NR3                            *LEGGE ALTRO RECORD
132300970117     C     NR3           CHAIN     TE03S03                            98
132400970117E   2C                   ENDIF
132500970117E   1C                   ENDDO                                                  *FINE SUBFILE
132600970117     C*
132700970117     C     FINCT3        ENDSR
132800970117     C*------------------------------------------------------------------------*
132900970117     C* AGGSF3 - AGGIORNA RIGA DEL TERSO SUBFILE
133000970117     C*------------------------------------------------------------------------*
133100970117     C     AGGSF3        BEGSR
133200970117     C*
133300970117     C                   SETON                                        80        *SI SFLNXTCHG
133400970117IF  1C     $TGES         IFEQ      'I'
133500970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
133600970117X   1C                   ELSE
133700970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
133800970117E   1C                   ENDIF
133900970117IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
134000970117     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
134100970117X   1C                   ELSE
134200970117     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
134300970117E   1C                   ENDIF
134400970117     C                   UPDATE    TE03S03                                      *AGGIORNA SUBFILE
134500970117     C                   SETOFF                                       80        *NO SFLNXTCHG
134600970117     C*
134700970117     C                   ENDSR
134800970124     C*------------------------------------------------------------------------*
134900970124     C* ENTSF3 - GESTIONE TASTO FUNZIONALE INVIO 3°SFILE  ==> AGGIORNA
135000970124     C*------------------------------------------------------------------------*
135100970124     C     ENTSF3        BEGSR
135200970124     C*
135300970124     C                   EXSR      AGGCTS
135400970124     C                   EXSR      AGGRCT
135500970129     C* AGGIORNA PRIMO SFILE
135600970129     C                   Z-ADD     1             J
135700970129     C     V3CRAP        LOOKUP    RCTS(J)                                98
135800970129     C     $DPC1         CHAIN     TE03S01                            98        *RILEGGE COD.TASS.
135900970129     C                   MOVEL     RRCT(J)       V1SRCT                         *RAGGRUPPAMENTO
136000970129     C                   Z-ADD     RPPE(J)       V1SPPE                         *%INCIDENZA
136100970129     C                   MOVEL     RHRC(J)       V1HRCT                         *RAGGRUPP.TO HIDDEN
136200970129     C                   MOVEL     RSCH(J)       V1SCTE                         *CAMPONE RAGGRUPP.TI
136300970129     C                   EXSR      AGGSF1
136400970128     C*
136500970124     C                   ENDSR
136600970124     C*------------------------------------------------------------------------*
136700970124     C* AGGCTS - AGGIORNA I CODICI TASSAZIONE ELEMENTARI DELLA REGIONE
136800970124     C*------------------------------------------------------------------------*
136900970124     C     AGGCTS        BEGSR
137000970124     C*
137100970124     C* LEGGE TUTTI I RECORD DEL SFILE
137200970124     C                   Z-ADD     1             NR3
137300970124     C     NR3           CHAIN     TE03S03                            98
137400970124DO  1C     *IN98         DOWEQ     *OFF
137500970124     C                   Z-ADD     1             J
137600970124     C     V3SCTS        LOOKUP    VCTS(J)                                98
137700970128     C                   MOVEL     V3SRCT        VRCT(J)                        *RAGGRUPPAMENTO
137800970128     C                   Z-ADD     V3SPPE        VPPE(J)                        *%INCIDENZA
137900970124     C                   ADD       1             NR3
138000970124     C     NR3           CHAIN     TE03S03                            98
138100970124E   1C                   ENDDO                                                  *FINE SUBFILE
138200970124     C*
138300970124     C                   ENDSR
138400970117     C*------------------------------------------------------------------------*
138500970124     C* AGGRTC - AGGIORNA I CODICI TASSAZIONE REGIONALI
138600970117     C*------------------------------------------------------------------------*
138700970117     C     AGGRCT        BEGSR
138800980708     C*
138900980708     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
139000980708     C                   CLEAR                   RRCT
139100980708     C                   CLEAR                   RHRC
139200980708     C                   CLEAR                   RPPE
139300970117     C*
139400970117     C* PER CIAUSCUNA REGIONE
139500970117DO  1C     1             DO        WURCTS        J
139600970117     C                   MOVEL     '0'           $RB               1            *RAGGRUPPAMENTI
139700970117     C                   MOVEL     '0'           $RN               1
139800970117     C                   MOVEL     '0'           $RR               1
139900970117     C                   MOVEL     '0'           $RT               1
140000970117     C                   Z-ADD     *ZEROS        TOTPPE
140100970117DO  2C     1             DO        WUVCTS        I
140200970117IF  3C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
140300970117IF  4C     VRAP(I)       IFEQ      RCTS(J)                                      *STESSA REGIONE
140400970117     C*
140500970117     C* CALCOLA IL RAGGRUPPAMENTO DELLA REGIONE
140600970117IF  5C     VRCT(I)       IFEQ      *BLANKS
140700970117     C                   MOVEL     '1'           $RB                            *RAGGRUP.NO CALCOLAR
140800970117E   5C                   ENDIF
140900970117IF  5C     VRCT(I)       IFEQ      'N'
141000970117     C                   MOVEL     '1'           $RN                            *RAGGRUP.NAZIONE
141100970117E   5C                   ENDIF
141200970117IF  5C     VRCT(I)       IFEQ      'R'
141300970117     C                   MOVEL     '1'           $RR                            *RAGGRUP.REGIONE
141400970117E   5C                   ENDIF
141500970117IF  5C     VRCT(I)       IFEQ      'T'
141600970117     C                   MOVEL     '1'           $RT                            *RAGGRUP.CALCOLARE
141700970117E   5C                   ENDIF
141800970117     C*
141900970117     C* CALCOLA LA %INCIDENZA
142000970128     C                   ADD       VPPE(I)       TOTPPE
142100970117E   4C                   ENDIF
142200970117E   3C                   ENDIF
142300970117E   2C                   ENDDO                                                  *FINE COD.TASS.ELEM.
142400970117     C*
142500970117     C* AGGIORNA IL RAGGRUPPAMENTO DELLA REGIONE
142600970117IF  2C     $RB           IFEQ      '1'
142700970117     C     $RN           ANDEQ     '0'
142800970117     C     $RR           ANDEQ     '0'
142900970117     C     $RT           ANDEQ     '0'
143000970117     C                   MOVEL     ' '           RRCT(J)                        *RAGGRUP.NO CALCOLAR
143100970128X   2C                   ELSE
143200970117IF  3C     $RN           IFEQ      '1'
143300970117     C     $RR           ANDEQ     '0'
143400970117     C     $RT           ANDEQ     '0'
143500970117     C     $RB           ANDEQ     '0'
143600970117     C                   MOVEL     'N'           RRCT(J)                        *RAGGRUP.NAZIONE
143700970117X   3C                   ELSE
143800970117IF  4C     $RR           IFEQ      '1'
143900970117     C     $RN           ANDEQ     '0'
144000970117     C     $RT           ANDEQ     '0'
144100970117     C     $RB           ANDEQ     '0'
144200970117     C                   MOVEL     'R'           RRCT(J)                        *RAGGRUP.REGIONE
144300970117X   4C                   ELSE
144400970117IF  5C     $RT           IFEQ      '1'
144500970117     C     $RN           ANDEQ     '0'
144600970117     C     $RR           ANDEQ     '0'
144700970117     C     $RB           ANDEQ     '0'
144800970117     C                   MOVEL     'T'           RRCT(J)                        *RAGGRUP.CALCOLARE
144900970117X   5C                   ELSE
145000970117     C                   MOVEL     'M'           RRCT(J)                        *RAGGRUPPAMENTO MIST
145100970117E   5C                   ENDIF
145200970117E   4C                   ENDIF
145300970117E   3C                   ENDIF
145400970117E   2C                   ENDIF
145500970124     C                   MOVEL     RRCT(J)       RHRC(J)                        *RAGGRUP.TO HIDDEN
145600970117     C*
145700970117     C* AGGIORNA LA %INCIDENZA
145800970117     C                   Z-ADD     TOTPPE        RPPE(J)                        *%INCIDENZA
145900970117E   1C                   ENDDO                                                  *FINE COD.TASS.REG.
146000970117     C*
146100970117     C                   ENDSR
146200970117     C*------------------------------------------------------------------------*
146300970117     C* IMPWD1 - IMPOSTAZIONI PRIMA FINESTRA
146400970117     C*------------------------------------------------------------------------*
146500970117     C     IMPWD1        BEGSR
146600970117     C*
146700970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
146800970117     C                   MOVEL     'N'           $FINV                          *FINE VIDEATA
146900970117     C                   CLEAR                   TE03W01
147000970117     C                   EXSR      SETWD1
147100970117     C*
147200970117     C                   ENDSR
147300970117     C*------------------------------------------------------------------------*
147400970117     C* SETWD1 - IMPOSTAZIONI INDICATORI PRIMA FINESTRA
147500970117     C*------------------------------------------------------------------------*
147600970117     C     SETWD1        BEGSR
147700970117     C*
147800970117     C                   SETOFF                                       252627     ERRORI
147900970117     C                   SETOFF                                       28
148000970117IF  1C     TIPVID        IFEQ      '1'
148100970117     C                   SETOFF                                       02        *SI VISUAL.RAGGRUP=M
148200970117X   1C                   ELSE
148300970117     C                   SETON                                        02        *NO VISUAL.RAGGRUP=M
148400970117E   1C                   ENDIF
148500970117     C                   SETOFF                                       90         GENERICO DI ERRORE
148600970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
148700970117     C*
148800970117     C                   ENDSR
148900970117     C*------------------------------------------------------------------------*
149000970117     C* GESWD1 - GESTIONE PRIMA FINESTRA
149100970117     C*------------------------------------------------------------------------*
149200970117     C     GESWD1        BEGSR
149300970117     C*
149400970117     C* EMISSIONE VIDEATA
149500970117     C                   EXFMT     TE03W01
149600970117     C                   EXSR      SETWD1
149700970117     C* F12-RITORNO
149800970117IF  1C     *INKL         IFEQ      *ON
149900970129     C                   MOVEL     *BLANKS       WRCT
150000970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
150100970117     C                   GOTO      FINWD1
150200970117E   1C                   ENDIF
150300970117     C* CONTROLLA VIDEO
150400970117     C                   EXSR      CTRWD1
150500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
150600970117     C                   GOTO      FINWD1
150700970117E   1C                   ENDIF
150800970117     C* INVIO-SCELTA
150900970117     C                   MOVEL     WWRCT         WRCT
151000970117     C                   MOVEL     'S'           $FINV                          *FINE VIDEATA
151100970117     C*
151200970117     C     FINWD1        ENDSR
151300970117     C*------------------------------------------------------------------------*
151400970117     C* CTRWD1 - CONTROLLA PRIMA FINESTRA
151500970117     C*------------------------------------------------------------------------*
151600970117     C     CTRWD1        BEGSR
151700970117     C*
151800970117     C     *LIKE         DEFINE    V1WRCB        WWRCT
151900970117     C                   MOVEL     *BLANKS       WWRCT
152000970117     C*--------------------
152100970117     C* DA NON CALCOLARE
152200970117     C*--------------------
152300970117IF  1C     V1WRCB        IFNE      *BLANKS
152400970117     C     V1WRCB        ANDNE     '1'
152500970117     C                   SETON                                        2590
152600970117     C                   MOVEL     MSG(11)       $MSG
152700970117     C                   GOTO      FINCW1
152800970117X   1C                   ELSE
152900970117IF  2C     V1WRCB        IFEQ      '1'
153000970117     C                   MOVEL     ' '           WWRCT                          *SCELTA
153100970117     C                   GOTO      FINCW1
153200970117E   2C                   ENDIF
153300970117E   1C                   ENDIF
153400970117     C*--------------------
153500970117     C* DA CALCOLARE
153600970117     C*--------------------
153700970117IF  1C     V1WRCT        IFNE      *BLANKS
153800970117     C     V1WRCT        ANDNE     '1'
153900970117     C                   SETON                                        2690
154000970117     C                   MOVEL     MSG(11)       $MSG
154100970117     C                   GOTO      FINCW1
154200970117X   1C                   ELSE
154300970117IF  2C     V1WRCT        IFEQ      '1'
154400970117     C                   MOVEL     'T'           WWRCT                          *SCELTA
154500970117     C                   GOTO      FINCW1
154600970117E   2C                   ENDIF
154700970117E   1C                   ENDIF
154800970117     C*--------------------
154900970117     C* DA CALCOLARE NELLA REGIONE
155000970117     C*--------------------
155100970117IF  1C     V1WRCR        IFNE      *BLANKS
155200970117     C     V1WRCR        ANDNE     '1'
155300970117     C                   SETON                                        2790
155400970117     C                   MOVEL     MSG(11)       $MSG
155500970117     C                   GOTO      FINCW1
155600970117X   1C                   ELSE
155700970117IF  2C     V1WRCR        IFEQ      '1'
155800970117     C                   MOVEL     'R'           WWRCT                          *SCELTA
155900970117     C                   GOTO      FINCW1
156000970117E   2C                   ENDIF
156100970117E   1C                   ENDIF
156200970117     C*--------------------
156300970117     C* DA CALCOLARE NELLA NAZIONE
156400970117     C*--------------------
156500970117IF  1C     V1WRCN        IFNE      *BLANKS
156600970117     C     V1WRCN        ANDNE     '1'
156700970117     C                   SETON                                        2890
156800970117     C                   MOVEL     MSG(11)       $MSG
156900970117     C                   GOTO      FINCW1
157000970117X   1C                   ELSE
157100970117IF  2C     V1WRCN        IFEQ      '1'
157200970117     C                   MOVEL     'N'           WWRCT                          *SCELTA
157300970117     C                   GOTO      FINCW1
157400970117E   2C                   ENDIF
157500970117E   1C                   ENDIF
157600970117     C*
157700970117     C     FINCW1        ENDSR
157800970117     C*------------------------------------------------------------------------*
157900970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
158000970117     C*------------------------------------------------------------------------*
158100970117     C     IMPWD2        BEGSR
158200970117     C*
158300091123     c                   if        d00dsf = ' '
158400091123     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
158500091123     c                   eval      s2wcto = '(T/O)'
158600091123     c                   eval      d2wksc = 'Cliente o Visita ...........:'
158700091123     c                   eval      d2wprg = 'Progressivo ................:'
158800091123     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
158900091123     c                             Tariffa/Offerta'
159000091123     c                   else
159100091201     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
159200091130     c                   eval      s2wcto = '(T/O)'
159300091123     c                   eval      d2wksc = 'Cliente o Trattativa .......:'
159400091123     c                   eval      d2wprg = 'Versione ...................:'
159500091123     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
159600091123     c                             Tariffa/Trattativa'
159700091123     c                   end
159800970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
159900970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
160000970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
160100980508     C                   MOVEL     D0DCTO        V2WCTO
160200970319     C                   Z-ADD     D0DKSC        V2WKSC
160300970319     C                   Z-ADD     D0DCTR        V2WCTR
160400970319     C                   Z-ADD     D0DPRG        V2WPRG
160500970319     C                   Z-ADD     D0DLNP        V2WLNP
160600011003     C                   MOVEL     D0DDIV        V2WDIV
160700970117     C                   EXSR      SETWD2
160800970117     C*
160900970117     C                   ENDSR
161000970117     C*------------------------------------------------------------------------*
161100970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
161200970117     C*------------------------------------------------------------------------*
161300970117     C     SETWD2        BEGSR
161400970117     C*
161500970117     C                   SETOFF                                       303132     ERRORI
161600980508     C                   SETOFF                                       333435
161700011003     C                   SETOFF                                       36
161800970117     C                   SETOFF                                       90         GENERICO DI ERRORE
161900970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
162000970117     C*
162100970117     C                   ENDSR
162200970117     C*------------------------------------------------------------------------*
162300970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
162400970117     C*------------------------------------------------------------------------*
162500970117     C     GESWD2        BEGSR
162600970117     C*
162700970117     C* EMISSIONE VIDEATA
162800970117     C                   EXFMT     TE03W02
162900970117     C                   EXSR      SETWD2
163000970317     C* F12 ABILITATO-RITORNO
163100970117IF  1C     *INKL         IFEQ      *ON
163200970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
163300970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
163400970117     C                   GOTO      FINWD2
163500970117E   1C                   ENDIF
163600980508     C* F11-RICERCA
163700980508IF  1C     *INKK         IFEQ      *ON
163800980508     C                   EXSR      GESRIC
163900060825     C*                  GOTO      FINWD2
164000980508E   1C                   ENDIF
164100970117     C* CONTROLLA VIDEO
164200970117     C                   EXSR      CTRWD2
164300970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
164400970117     C                   GOTO      FINWD2
164500970117E   1C                   ENDIF
164600970117     C* F06-CONFERMA
164700970117IF  1C     *INKF         IFEQ      *ON
164800970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
164900970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
165000970117     C                   GOTO      FINWD2
165100970117E   1C                   ENDIF
165200970117     C*
165300970117     C     FINWD2        ENDSR
165400980508     C*------------------------------------------------------------------------*
165500980508     C* GESRIC - GESTIONE RICERCA DATI TIPO
165600980508     C*------------------------------------------------------------------------*
165700980508     C     GESRIC        BEGSR
165800980508     C*
165900980508     C                   CLEAR                   DSTE30
166000980508     C                   MOVEL     'C01'         D30OP0
166100980708     C                   MOVEL     'T'           D30ETX
166200980804     C                   Z-ADD     D00KSC        D30KSC
166300980904     C                   MOVEL     D00FIE        D30TFI
166400011011     C                   MOVEL     D00DIV        D30DIV
166500091123     C                   MOVEL     D00Dsf        D30Dsf
166600980508     C                   CALL      'TNTE32R'
166700980508     C                   PARM                    KPJBA
166800980508     C                   PARM                    DSTE30
166900980508IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
167000980508     C                   SETON                                        90
167100980508     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
167200980508X   1C                   ELSE                                                   *NO ERRORE
167300980508IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
167400980508     C     D30F12        OREQ      '1'
167500980508X   2C                   ELSE                                                   *SCELTA
167600980508     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
167700980508     C                   MOVEL     D30CTO        V2WCTO
167800980508     C                   Z-ADD     D30KSC        V2WKSC
167900980508     C                   Z-ADD     D30CTR        V2WCTR
168000980508     C                   Z-ADD     D30PRG        V2WPRG
168100980508     C                   Z-ADD     D30LNP        V2WLNP
168200011003     C                   MOVEL     D30DIV        V2WDIV
168300980508E   2C                   ENDIF
168400980508E   1C                   ENDIF
168500980508     C*
168600060825     C*                  MOVEL     'S'           $FINV                          *FINE FINESTRA
168700980508     C*
168800980508     C                   ENDSR
168900970117     C*------------------------------------------------------------------------*
169000970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
169100970117     C*------------------------------------------------------------------------*
169200970117     C     CTRWD2        BEGSR
169300970117     C*--------------------
169400091123     C* TARIFFA / OFFERTA  / TRATTATIVA
169500970117     C*--------------------
169600091123     c                   if        (d00dsf = ' '
169700091123IF  1C                             and v2wcto <> 'T' and
169800091123IF  1C                             v2wcto <> 'O') or
169900091123     c                             (d00dsf = 'X'
170000091123IF  1C                             and v2wcto <> 'T' and
170100091130IF  1C                             v2wcto <> 'O')
170200091123     C                   SETON                                        90
170300091123     C                   MOVEL     MSG(16)       $MSG
170400091123     C                   GOTO      FINCW2
170500091123E   1C                   ENDIF
170600060825     C*--------------------
170700060825     C* stessa tipologia di codice tariffa
170800060825     C*--------------------
170900061128     c*                  movel     d00ctr        comd00            1 0
171000061128     c*                  movel     v2wctr        comv2w            1 0
171100061128     c*                  if        comd00 <> comv2w
171200061128     C*                  SETON                                        31
171300061128     C*                  SETON                                        90
171400061128     C*                  MOVEL     MSG(28)       $MSG
171500061128     C*                  GOTO      FINCW2
171600061128     c*                  end
171700970117     C*--------------------
171800970117     C* LINEA DI PARTENZA
171900970117     C*--------------------
172000970117     C                   MOVEL     V2WLNP        WLNP
172100970117     C                   MOVEL     *BLANKS       WDLNP
172200971204     C                   MOVEL     *BLANKS       WFL1
172300970117     C                   MOVEL     '0'           WERR
172400970117     C                   EXSR      CHKLNP
172500980818IF  1C     WERR          IFEQ      '1'
172600980818IF  2C     WLNP          IFEQ      888
172700980818     C     WLNP          OREQ      889
172800980818     C                   MOVEL     '0'           WERR
172900980818     C                   MOVEL     DDEC(10)      WDLNP
173000980818IF  3C     WLNP          IFEQ      888
173100980818     C                   MOVEL     *BLANKS       WFL1
173200980818E   3C                   ENDIF
173300980818IF  3C     WLNP          IFEQ      889
173400980818     C                   MOVEL     'E'           WFL1
173500980818E   3C                   ENDIF
173600980818X   2C                   ELSE
173700011003     C                   SETON                                        343690
173800970117     C                   MOVEL     MSG(15)       $MSG
173900970117     C                   GOTO      FINCW2
174000980818E   2C                   ENDIF
174100980818E   1C                   ENDIF
174200970117     C*
174300970117IF  1C     D00FIE        IFEQ      'I'
174400980114IF  2C     WFL1          IFNE      *BLANKS
174500011003     C                   SETON                                        343690
174600970117     C                   MOVEL     MSG(17)       $MSG
174700970117     C                   GOTO      FINCW2
174800980114E   2C                   ENDIF
174900980114X   1C                   ELSE
175000980114IF  2C     DLPFL1        IFEQ      *BLANKS
175100971204     C     WFL1          ANDNE     *BLANKS
175200011003     C                   SETON                                        343690
175300970117     C                   MOVEL     MSG(18)       $MSG
175400970117     C                   GOTO      FINCW2
175500980114E   2C                   ENDIF
175600980114IF  2C     DLPFL1        IFNE      *BLANKS
175700971204     C     WFL1          ANDEQ     *BLANKS
175800011003     C                   SETON                                        343690
175900970117     C                   MOVEL     MSG(19)       $MSG
176000970117     C                   GOTO      FINCW2
176100980114E   2C                   ENDIF
176200970117E   1C                   ENDIF
176300970117     C*--------------------
176400970117     C* DATI TIPO ESISTENTI
176500970117     C*--------------------
176600091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
176700980508     C                   MOVEL     V2WCTO        KTTCTO                         *TARIFFA O OFFERTA
176800970117     C                   Z-ADD     V2WKSC        KTTKSC                         *CLIENTE O VISITA
176900970117     C                   Z-ADD     V2WCTR        KTTCTR                         *TARIFFA
177000970117     C                   Z-ADD     V2WPRG        KTTPRG                         *PROGRESSIVO
177100970117     C                   Z-ADD     V2WLNP        KTTLNP                         *LINEA DI PARTENZA
177200970117     C                   MOVEL     D00FIE        KTTFIE                         *FLAG ITALIA/ESTERO
177300970619     C                   MOVEL     'N'           WEETT                          *ESISTENZA CTS
177400011001     C     KE2ETT        SETLL     TEETT01L
177500011001     C     KE2ETT        READE     TEETT01L                               99
177600970619DO  1C     *IN99         DOWEQ     *OFF
177700970619IF  2C     ETTATB        IFEQ      *BLANKS
177800980508     C                   MOVEL     'S'           WEETT                          *ESISTENZA CTS
177900970619E   2C                   ENDIF
178000011001     C     KE2ETT        READE     TEETT01L                               99
178100980508E   1C                   ENDDO                                                  *FINE CTS
178200970619IF  1C     WEETT         IFEQ      'N'                                          *NON TROVATI
178300970117     C                   SETON                                        303132
178400980508     C                   SETON                                        333435
178500011003     C                   SETON                                        36
178600980508     C                   SETON                                        90
178700970117     C                   MOVEL     MSG(13)       $MSG
178800970117E   1C                   ENDIF
178900970117     C*
179000970117     C     FINCW2        ENDSR
179100970117     C*------------------------------------------------------------------------*
179200970117     C* IMPWD3 - IMPOSTAZIONI TERZA FINESTRA
179300970117     C*------------------------------------------------------------------------*
179400970117     C     IMPWD3        BEGSR
179500970117     C*
179600970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
179700970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
179800970318     C                   MOVEL     'NO'          V3WRIS
179900970117     C                   EXSR      SETWD3
180000970117     C*
180100970117     C                   ENDSR
180200970117     C*------------------------------------------------------------------------*
180300970117     C* SETWD3 - IMPOSTAZIONI INDICATORI TERZA FINESTRA
180400970117     C*------------------------------------------------------------------------*
180500970117     C     SETWD3        BEGSR
180600970117     C*
180700970117     C                   SETOFF                                       40         ERRORI
180800970117     C                   SETOFF                                       90         GENERICO DI ERRORE
180900970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
181000970117     C*
181100970117     C                   ENDSR
181200970117     C*------------------------------------------------------------------------*
181300970117     C* GESWD3 - GESTIONE TERZA FINESTRA
181400970117     C*------------------------------------------------------------------------*
181500970117     C     GESWD3        BEGSR
181600970117     C*
181700970117     C* EMISSIONE VIDEATA
181800970117     C                   EXFMT     TE03W03
181900970117     C                   EXSR      SETWD3
182000970117     C* F12-RITORNO
182100970117IF  1C     *INKL         IFEQ      *ON
182200970314     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
182300970318     C                   MOVEL     'NO'          V3WRIS
182400970117     C                   GOTO      FINWD3
182500970117E   1C                   ENDIF
182600970117     C* CONTROLLA VIDEO
182700970117     C                   EXSR      CTRWD3
182800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
182900970117     C                   GOTO      FINWD3
183000970117E   1C                   ENDIF
183100970117     C* INVIO-CONFERMA
183200970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
183300970117     C*
183400970117     C     FINWD3        ENDSR
183500970117     C*------------------------------------------------------------------------*
183600970117     C* CTRWD3 - CONTROLLA TERZA FINESTRA
183700970117     C*------------------------------------------------------------------------*
183800970117     C     CTRWD3        BEGSR
183900970117     C*--------------------
184000970117     C* CONFERMA SCRITTURA OFFERTE
184100970117     C*--------------------
184200970117IF  1C     V3WRIS        IFNE      'SI'
184300970117     C     V3WRIS        ANDNE     'NO'
184400970117     C                   SETON                                        4090
184500970117     C                   MOVEL     MSG(12)       $MSG
184600970117     C                   GOTO      FINCW3
184700970117E   1C                   ENDIF
184800970117     C*
184900970117     C     FINCW3        ENDSR
185000970117     C*------------------------------------------------------------------------*
185100970117     C* POSSF1 - CARICA IL PRIMO SUBFILE
185200970117     C*------------------------------------------------------------------------*
185300970117     C     POSSF1        BEGSR
185400970117     C*
185500970117     C* ELIMINA IL CONTENUTO DEL SFILE
185600970117     C                   EXSR      CLRSF1
185700970117     C*
185800970117     C* SI POSIZIONA SUL RECORD DA CARICARE
185900970117     C                   EXSR      SETFI1
186000970117     C* CARICA IL SUBFILE
186100970117     C                   Z-ADD     *ZEROS        NR1               4 0          *DALL'INIZIO
186200970117     C                   EXSR      CARSF1
186300970117     C*
186400970117     C                   ENDSR
186500970117     C*------------------------------------------------------------------------*
186600970117     C* CLRSF1 - ELIMINA IL CONTENUTO DEL PRIMO SUBFILE
186700970117     C*------------------------------------------------------------------------*
186800970117     C     CLRSF1        BEGSR
186900970117     C*
187000970117     C                   Z-ADD     1             $NR1
187100970117     C                   SETOFF                                       71        *NO SFLDSP
187200970117     C                   SETOFF                                       72        *NO DSPCTL/SI SFLCLR
187300970117     C                   WRITE     TE03C01
187400970117     C                   SETON                                        71        *SI SFLDSP
187500970117     C                   SETON                                        72        *SI DSPCTL/NO SFLCLR
187600970117     C*
187700970117     C                   ENDSR
187800970117     C*------------------------------------------------------------------------*
187900970117     C* SETFI1 - POSIZIONAMENTO RECORD -PRIMO SUBFILE-
188000970117     C*------------------------------------------------------------------------*
188100970117     C     SETFI1        BEGSR
188200970117     C*
188300970117     C                   Z-ADD     *ZEROS        SF1EOF                         *0=NO FINE/1=FINE
188400970117     C                   Z-ADD     *ZEROS        S                 4 0          *INDICE SCHIERE VIDE
188500970117     C*
188600970117     C                   ENDSR
188700970117     C*------------------------------------------------------------------------*
188800970117     C* CARSF1 - CARICA TUTTO IL PRIMO SUBFILE
188900970117     C*------------------------------------------------------------------------*
189000970117     C     CARSF1        BEGSR
189100970117     C*
189200970117DO  1C                   DO        WURCTS
189300970117     C                   EXSR      LETFI1                                       *LEGGE RECORD
189400970117IF  2C     SF1EOF        IFEQ      *ZEROS
189500970117     C                   ADD       1             NR1
189600970117     C                   EXSR      IRESF1                                       *IMPOSTA RECORD SFIL
189700970117     C                   WRITE     TE03S01                                      *SCRIVE RECORD SFILE
189800970117E   2C                   ENDIF
189900970117E   1C                   ENDDO
190000970117     C*
190100970117     C                   ENDSR
190200970117     C*------------------------------------------------------------------------*
190300970117     C* LETFI1 - LETTURA SUCESSIVA RECORD -PRIMO SUBFILE-
190400970117     C*------------------------------------------------------------------------*
190500970117     C     LETFI1        BEGSR
190600970117     C*
190700970117     C                   ADD       1             S                              *INDICE SCHIERE VIDE
190800970117     C*
190900970117     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
191000970117IF  1C     S             IFGT      WURCTS                                       *AMPIEZZA MASSIMA
191100970117     C     RCTS(S)       OREQ      *BLANKS                                      *SCHIERA VUOTA
191200970117     C                   Z-ADD     1             SF1EOF
191300970117E   1C                   ENDIF
191400970117     C*
191500970117     C                   ENDSR
191600970117     C*------------------------------------------------------------------------*
191700970117     C* IRESF1 - IMPOSTA CAMPI RECORD PRIMO SUBFILE
191800970117     C*------------------------------------------------------------------------*
191900970117     C     IRESF1        BEGSR
192000970117     C* CAMPI
192100970124     C                   MOVEL     RHRC(S)       V1HRCT                         *HIDDEN
192200970117     C                   MOVEL     RCTS(S)       V1SCTS
192300970117     C                   Z-ADD     RPPE(S)       V1SPPE
192400970117     C                   MOVEL     RRCT(S)       V1SRCT
192500970117     C* DECODIFICHE
192600970117     C                   SETOFF                                           98
192700970117     C                   Z-ADD     1             I
192800970117     C     RCTS(S)       LOOKUP    CTS(I)                                 98
192900970117     C                   MOVEL     DCT(I)        V1SDCT
193000970117     C                   MOVEL     V1SCTS        WCTS
193100970117     C                   MOVEL     V1SRCT        WRCT
193200970117     C                   MOVEL     *BLANKS       WDRCT
193300970117     C                   EXSR      DECRCT
193400970117     C                   MOVEL     WDRCT         V1SDRC                         *DESCR.RAGGRUPPAMENT
193500970117     C*
193600970128     C                   CLEAR                   SCH
193700970128     C                   Z-ADD     *ZEROS        J
193800970128DO  1C     1             DO        WUVCTS        I
193900970128IF  2C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
194000970128IF  3C     VRAP(I)       IFEQ      RCTS(S)                                      *STESSA REGIONE
194100970128     C                   ADD       1             J
194200970128     C                   MOVEL     VRCT(I)       SCH(J)
194300970128IF  4C     SCH(J)        IFEQ      *BLANKS
194400970128     C                   MOVEL     '*'           SCH(J)
194500970128E   4C                   ENDIF
194600970128E   3C                   ENDIF
194700970128E   2C                   ENDIF
194800970128E   1C                   ENDDO
194900970128     C                   MOVEA     SCH           RSCH(S)
195000970128     C                   MOVEL     RSCH(S)       V1SCTE                         *TUTTI RAGGR COD.TAS
195100050304     c* se x il cod. tassazione regione non trova i corrispondenti cod.
195200050304     c* tassazione elementari (errore nell'inserimento tab. CT) mando
195300050304     c* un msg d'errore AVVISARE CED
195400050304     c                   if        v1scte = ' '
195500050304     c                   seton                                        105090
195600050304     C                   MOVEL     MSG(25)       $MSG
195700050304     c                   eval      V1SDRC = *all'?'
195800050304     c                   clear                   v1srct
195900050304     c                   clear                   v1hrct
196000050304     C                   clear                   RHRC(S)                        *HIDDEN
196100050304     C                   clear                   RRCT(S)
196200050304     c                   else
196300970203     C* INDICATORI
196400050304     C                   SETOFF                                       7050      *NO SFLNXTCHG
196500970203IF  1C     $TGES         IFEQ      'I'
196600970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
196700970203X   1C                   ELSE
196800970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
196900970203E   1C                   ENDIF
197000970203IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
197100970203     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
197200970203X   1C                   ELSE                                                   *DA PORTO
197300970203     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
197400970203E   1C                   ENDIF
197500050304E   1C                   ENDIF
197600970203     C*
197700970117     C                   ENDSR
197800970117     C*------------------------------------------------------------------------*
197900970117     C* POSSF2 - CARICA IL SECONDO SUBFILE
198000970117     C*------------------------------------------------------------------------*
198100970117     C     POSSF2        BEGSR
198200970117     C*
198300970117     C* ELIMINA IL CONTENUTO DEL SFILE
198400970117     C                   EXSR      CLRSF2
198500970117     C*
198600970117     C* SI POSIZIONA SUL TERZO RECORD DA CARICARE
198700970117     C                   EXSR      SETFI2
198800970117     C* CARICA IL SUBFILE
198900970117     C                   Z-ADD     *ZEROS        NR2               4 0          *DALL'INIZIO
199000970117     C                   EXSR      CARSF2
199100970117     C*
199200970117     C                   ENDSR
199300970117     C*------------------------------------------------------------------------*
199400970117     C* CLRSF2 - ELIMINA IL CONTENUTO DEL SECONDO SUBFILE
199500970117     C*------------------------------------------------------------------------*
199600980508     C     CLRSF2        BEGSR
199700970117     C*
199800970117     C                   Z-ADD     1             $NR2
199900970117     C                   SETOFF                                       91        *NO SFLDSP
200000970117     C                   SETOFF                                       92        *NO DSPCTL/SI SFLCLR
200100970117     C                   WRITE     TE03C02
200200970117     C                   SETON                                        91        *SI SFLDSP
200300970117     C                   SETON                                        92        *SI DSPCTL/NO SFLCLR
200400970117     C*
200500970117     C                   ENDSR
200600970117     C*------------------------------------------------------------------------*
200700970117     C* SETFI2 - POSIZIONAMENTO RECORD -SECONDO SUBFILE-
200800970117     C*------------------------------------------------------------------------*
200900970117     C     SETFI2        BEGSR
201000970117     C*
201100970117     C                   Z-ADD     *ZEROS        SF2EOF                         *0=NO FINE/1=FINE
201200970117     C                   Z-ADD     *ZEROS        O                 4 0          *INDICE SCHIERE VIDE
201300970117     C*
201400970117     C                   ENDSR
201500970117     C*------------------------------------------------------------------------*
201600970117     C* CARSF2 - CARICA TUTTO IL SECONDO SUBFILE
201700970117     C*------------------------------------------------------------------------*
201800970117     C     CARSF2        BEGSR
201900970117     C*
202000970117DO  1C     1             DO        WUOCTS
202100970117     C                   EXSR      LETFI2                                       *LEGGE RECORD
202200970117IF  2C     SF2EOF        IFEQ      *ZEROS
202300970117     C                   ADD       1             NR2
202400970117     C                   EXSR      IRESF2                                       *IMPOSTA RECORD SFIL
202500970117     C                   WRITE     TE03S02                                      *SCRIVE RECORD SFILE
202600970117E   2C                   ENDIF
202700970117E   1C                   ENDDO
202800970117     C*
202900970117     C                   ENDSR
203000970117     C*------------------------------------------------------------------------*
203100970117     C* LETFI2 - LETTURA SUCESSIVA RECORD -SECONDO SUBFILE-
203200970117     C*------------------------------------------------------------------------*
203300970117     C     LETFI2        BEGSR
203400970117     C*
203500970117     C                   ADD       1             O                              *INDICE SCHIERE VIDE
203600970117     C*
203700970117     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
203800970117IF  1C     O             IFGT      WUOCTS                                       *AMPIEZZA MASSIMA
203900970117     C     OCTS(O)       OREQ      *BLANKS                                      *SCHIERA VUOTA
204000970117     C                   Z-ADD     1             SF2EOF
204100970117E   1C                   ENDIF
204200970117     C*
204300970117     C                   ENDSR
204400970117     C*------------------------------------------------------------------------*
204500970117     C* IRESF2 - IMPOSTA CAMPI RECORD SECONDO SUBFILE
204600970117     C*------------------------------------------------------------------------*
204700970117     C     IRESF2        BEGSR
204800970117     C*
204900970117     C                   MOVEL     V1CPPE        V2CPPE                         *CONTROL
205000970117     C                   MOVEL     OCTS(O)       V2SCTS                         *SFILE
205100970117     C                   Z-ADD     OPPE(O)       V2SPPE
205200970117     C* DECODIFICHE
205300970117     C                   SETOFF                                           98
205400970117     C                   Z-ADD     1             I
205500970117     C     OCTS(O)       LOOKUP    CTS(I)                                 98
205600970117     C                   MOVEL     DCT(I)        V2SDCT
205700970117     C*
205800970117     C                   ENDSR
205900970117     C*------------------------------------------------------------------------*
206000970117     C* POSSF3 - CARICA IL TERZO SUBFILE
206100970117     C*------------------------------------------------------------------------*
206200970117     C     POSSF3        BEGSR
206300970117     C*
206400970117     C* ELIMINA IL CONTENUTO DEL SFILE
206500970117     C                   EXSR      CLRSF3
206600970117     C*
206700970117     C* SI POSIZIONA SUL RECORD DA CARICARE
206800970117     C                   EXSR      SETFI3
206900970117     C* CARICA IL SUBFILE
207000970117     C                   Z-ADD     *ZEROS        NR3               4 0          *DALL'INIZIO
207100970117     C                   EXSR      CARSF3
207200970117     C*
207300970117     C                   ENDSR
207400970117     C*------------------------------------------------------------------------*
207500970117     C* CLRSF3 - ELIMINA IL CONTENUTO DEL TERZO SUBFILE
207600970117     C*------------------------------------------------------------------------*
207700970117     C     CLRSF3        BEGSR
207800970117     C*
207900970117     C                   Z-ADD     1             $NR3
208000970117     C                   SETOFF                                       81        *NO SFLDSP
208100970117     C                   SETOFF                                       82        *NO DSPCTL/SI SFLCLR
208200970117     C                   WRITE     TE03C03
208300970117     C                   SETON                                        81        *SI SFLDSP
208400970117     C                   SETON                                        82        *SI DSPCTL/NO SFLCLR
208500970117     C*
208600970117     C                   ENDSR
208700970117     C*------------------------------------------------------------------------*
208800970117     C* SETFI3 - POSIZIONAMENTO RECORD -TERZO SUBFILE-
208900970117     C*------------------------------------------------------------------------*
209000970117     C     SETFI3        BEGSR
209100970117     C*
209200970117     C                   Z-ADD     *ZEROS        SF3EOF                         *0=NO FINE/1=FINE
209300970117     C                   Z-ADD     *ZEROS        V                 4 0          *INDICE SCHIERE VIDE
209400970117     C*
209500970117     C                   ENDSR
209600970117     C*------------------------------------------------------------------------*
209700970117     C* CARSF3 - CARICA TUTTO IL SECONDO SUBFILE
209800970117     C*------------------------------------------------------------------------*
209900970117     C     CARSF3        BEGSR
210000970117     C*
210100970117DO  1C                   DO        WUVCTS
210200970117     C                   EXSR      LETFI3                                       *LEGGE RECORD
210300970117IF  2C     SF3EOF        IFEQ      *ZEROS
210400970117     C                   ADD       1             NR3
210500970117     C                   EXSR      IRESF3                                       *IMPOSTA RECORD SFIL
210600970117     C                   WRITE     TE03S03                                      *SCRIVE RECORD SFILE
210700970117E   2C                   ENDIF
210800970117E   1C                   ENDDO
210900970117     C*
211000970117     C                   ENDSR
211100970117     C*------------------------------------------------------------------------*
211200970117     C* LETFI3 - LETTURA SUCESSIVA RECORD -TERZO SUBFILE-
211300970117     C*------------------------------------------------------------------------*
211400970117     C     LETFI3        BEGSR
211500970117     C*
211600970117     C* LEGGE FINO A:
211700970117     C*  - FINE FILE
211800970117     C*  - TROVATO RECORD VALIDO
211900970117     C                   MOVEL     'N'           WRECOK
212000970117DO  1C     SF3EOF        DOWEQ     *ZEROS
212100970117     C     WRECOK        ANDEQ     'N'
212200970117     C*
212300970117     C                   ADD       1             V
212400970117IF  2C     V             IFGT      WUVCTS                                       *AMPIEZZA MASSIMA
212500970117     C     VCTS(V)       OREQ      *BLANKS                                      *SCHIERA VUOTA
212600970117     C                   Z-ADD     1             SF3EOF
212700970117X   2C                   ELSE
212800970117     C                   EXSR      CHKSF3                                       *CONTROLLA RECORD
212900970117E   2C                   ENDIF
213000970117E   1C                   ENDDO
213100970117     C*
213200970117     C                   ENDSR
213300970117     C*------------------------------------------------------------------------*
213400970117     C* CHKSF3 - CONTROLLA RECORD DEL TERZO SUBFILE DA EMETTERE
213500970117     C*------------------------------------------------------------------------*
213600970117     C     CHKSF3        BEGSR
213700970117     C*
213800970117     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
213900970117     C*
214000970117     C* CODICE TASSAZIONE NON DELLA REGIONE, ESCLUDE
214100970117IF  1C     VRAP(V)       IFNE      V3CRAP
214200970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
214300970117E   1C                   ENDIF
214400970117     C*
214500970117     C                   ENDSR
214600970117     C*------------------------------------------------------------------------*
214700970117     C* IRESF3 - IMPOSTA CAMPI RECORD TERZO SUBFILE
214800970117     C*------------------------------------------------------------------------*
214900970117     C     IRESF3        BEGSR
215000970117     C* CAMPI
215100970117     C                   MOVEL     VCTS(V)       V3SCTS
215200970117     C                   Z-ADD     VPPE(V)       V3SPPE
215300970117     C                   MOVEL     VRCT(V)       V3SRCT
215400970128     C                   ADD       VPPE(V)       V3CPPE
215500970117     C* DECODIFICHE
215600970117     C                   SETOFF                                           98
215700970117     C                   Z-ADD     1             I
215800970117     C     VCTS(V)       LOOKUP    CTS(I)                                 98
215900970117     C                   MOVEL     DCT(I)        V3SDCT
216000970117     C                   MOVEL     FCR(I)        V3SFCR
216100970117     C                   MOVEL     V3SCTS        WCTS
216200970117     C                   MOVEL     V3SRCT        WRCT
216300970117     C                   MOVEL     *BLANKS       WDRCT
216400970117     C                   EXSR      DECRCT
216500970117     C                   MOVEL     WDRCT         V3SDRC                         *DESCR.RAGGRUPPAMENT
216600970203     C* INDICATORI
216700970203     C                   SETOFF                                       80        *NO SFLNXTCHG
216800970203IF  1C     $TGES         IFEQ      'I'
216900970203     C                   SETON                                        10        *SI PROTEZIONE CAMPI
217000970203X   1C                   ELSE
217100970203     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
217200970203E   1C                   ENDIF
217300970203IF  1C     D00TLA        IFEQ      'D'                                          *DA DELTA
217400970203     C                   SETON                                        01        *PROTEZIONE RAGGRUP.
217500970203X   1C                   ELSE
217600970203     C                   SETOFF                                       01        *NO PROTEZIONE RAGGR
217700970203E   1C                   ENDIF
217800970203     C*
217900970117     C                   ENDSR
218000970117     C*------------------------------------------------------------------------*
218100970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
218200970117     C*------------------------------------------------------------------------*
218300970117     C     CHKLNP        BEGSR
218400970117     C*
218500020729     C     *LIKE         DEFINE    §NTWFIE       WFL1
218600970117     C     *LIKE         DEFINE    V0DLNP        WDLNP
218700970117     C*
218800970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
218900970117     C                   MOVEL     WLNP          KORFIL
219000970117     C     KEYORG        CHAIN     AZORG01L                           99
219100970117IF  1C     *IN99         IFEQ      *OFF
219200970117     C     ORGFVA        ANDEQ     *BLANKS
219300970117     C     ORGFAG        ANDNE     'V'
219400970117     C                   MOVEL     ORGDES        WDLNP
219500020729     C                   MOVEL     ORGDE3        OG143
219600020729     C*
219700020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
219800020729     C                   CLEAR                   DNTW
219900020729     C*
220000020729     C                   MOVEL(P)  'NTW'         KTECOD
220100020729     C                   MOVEL(P)  §OGNTW        KTEKE1
220200020729     C     KEYTBE        CHAIN     TNTBE01L
220300020729     C                   IF        %found(TNTBE01L)
220400020729     C                   MOVEL     TBEUNI        DNTW
220500020729     C                   ENDIF
220600020729     C*
220700020729     C                   MOVEL     §NTWFIE       WFL1
220800970117X   1C                   ELSE
220900970117     C                   MOVEL     '1'           WERR
221000970117E   1C                   ENDIF
221100970117     C*
221200970117     C                   ENDSR
221300970117     C*------------------------------------------------------------------------*
221400970117     C* CHKRCT - CONTROLLA IL RAGGRUPPAMENTO CODICE TASSAZIONE
221500970117     C*------------------------------------------------------------------------*
221600970117     C     CHKRCT        BEGSR
221700970117     C*
221800970117     C     *LIKE         DEFINE    V1SRCT        WRCT
221900970117     C     *LIKE         DEFINE    V1SDRC        WDRCT
222000970117     C*
222100970117     C* RICHIESTO ELENCO CODICI
222200970117     C     '?'           SCAN      WRCT                                   98
222300970117IF  1C     *IN98         IFEQ      *ON
222400970117     C* GESTIONE VIDEATA
222500970117     C                   EXSR      IMPWD1
222600970117DO  2C     $FINV         DOUEQ     'S'
222700970117     C                   EXSR      GESWD1                                       *GESTIONE FINESTRA
222800970117E   2C                   ENDDO
222900970117E   1C                   ENDIF
223000050304     C* se tariffa estero non do la possibilità di mettere il ragg. = Nazione
223100050304     c                   do
223200050304     c                   if        wrct = 'N' and v0cfie = 'E'
223300050304     C                   MOVEL     '1'           WERR
223400050304     C                   MOVEL     msg(26)       $msg
223500050304     c                   leave
223600050304     c                   end
223700050304     c* verifico se per il cod. tassazione esiste un raggruppam. imposto
223800050304     c                   if        wrct <> ' '
223900050304     c                   z-add     1             bb                3 0
224000050304     c     wcts          lookup    cts(bb)                                51
224100050304     c                   if        *in51 and rct(bb) <> wrct
224200050304     c                             and rct(bb) <> *blanks
224300050304     C                   MOVEL     '1'           WERR
224400050304     C                   MOVEL     msg(27)       $msg
224500050304     c                   eval      %subst($msg: 60: 1)=rct(bb)
224600050304     c                   leave
224700050304     c                   end
224800050304     c                   end
224900970117     C* CONTROLLA ESATTEZZA CODICE
225000970117IF  1C     WRCT          IFNE      ' '
225100970117     C     WRCT          ANDNE     'T'
225200970117     C     WRCT          ANDNE     'R'
225300970117     C     WRCT          ANDNE     'N'
225400970117     C     WRCT          ANDNE     'M'
225500970117     C                   MOVEL     '1'           WERR
225600050304     c                   leave
225700050304X   1C                   End
225800970117     C                   EXSR      DECRCT                                       *DECODIFICA RAGGRUP.
225900970117IF  2C     WRCT          IFEQ      'M'                                          *SITUAZIONE "MISTA"
226000970117     C     WRCT          ANDNE     V1HRCT                                       *NON DA PRIMA
226100970117     C                   MOVEL     '1'           WERR
226200970117E   1C                   ENDIF
226300050304     c                   enddo
226400970117     C*
226500970117     C                   ENDSR
226600970117     C*------------------------------------------------------------------------*
226700970117     C* DECRCT - DECODIFICA IL RAGGRUPPAMENTO CODICE TASSAZIONE
226800970117     C*------------------------------------------------------------------------*
226900970117     C     DECRCT        BEGSR
227000970117     C*
227100970117IF  1C     WRCT          IFEQ      ' '                                          *NON CALCOLARE
227200970117     C                   MOVEL     DDEC(3)       WDRCT                          *DESCR.RAGGRUPPAMENT
227300970117E   1C                   ENDIF
227400970117IF  1C     WRCT          IFEQ      'T'                                          *CALCOLARE
227500970117     C                   MOVEL     DDEC(4)       WDRCT                          *DESCR.RAGGRUPPAMENT
227600970117E   1C                   ENDIF
227700970117IF  1C     WRCT          IFEQ      'R'                                          *CALCOLARE IN REGION
227800970117     C     WRCT          OREQ      'N'                                          *CALCOLARE IN NAZION
227900970117     C                   MOVEL     DDEC(5)       DDEC05
228000970117     C                   SETOFF                                           98
228100970117     C                   Z-ADD     1             I                 4 0
228200970117     C     WCTS          LOOKUP    CTS(I)                                 98
228300970117     C                   SETOFF                                           98
228400970117     C                   Z-ADD     1             J                 4 0
228500970117IF  2C     WRCT          IFEQ      'R'
228600970117     C     RAP(I)        LOOKUP    CTS(J)                                 98
228700970117X   2C                   ELSE
228800970117     C     NAZ(I)        LOOKUP    CTS(J)                                 98
228900970117E   2C                   ENDIF
229000970117     C                   MOVEL     DCT(J)        D05RCT
229100970117     C                   MOVEL     DDEC05        WDRCT                          *DESCR.RAGGRUPPAMENT
229200970117E   1C                   ENDIF
229300970117IF  1C     WRCT          IFEQ      'M'                                          *SITUAZIONE MISTA
229400970117     C                   MOVEL     DDEC(6)       WDRCT                          *DESCR.RAGGRUPPAMENT
229500970117E   1C                   ENDIF
229600970117     C*
229700970117     C                   ENDSR
229800970117     C*------------------------------------------------------------------------*
229900970117     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
230000970117     C*------------------------------------------------------------------------*
230100970117     C     CARCC         BEGSR
230200970117     C*
230300970117     C* RECUPERA I DATI DELL'UTENTE
230400020906     C                   Z-ADD     1             CODUT
230500020830     C                   CALL      'X§PARUT'
230600970117     C                   PARM                    PARUT
230700970117     C*
230800970117     C                   Z-ADD     1             Z
230900970117DO  1C                   DO        50            X2                2 0
231000970117     C                   MOVE      TCU(X2)       CARCNT
231100970117IF  2C     TIPCNT        IFEQ      'CI'
231200970117     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB.INDUSTRIALE
231300970117E   2C                   ENDIF
231400970117IF  2C     FL34CN        IFEQ      '01'
231500970117     C     TIPCNT        ANDEQ     'CG'
231600970117     C                   Z-ADD     KCU(X2)       KCCCL             4 0           *CLIENTI ITALIA
231700970117E   2C                   ENDIF
231800970117IF  2C     FL34CN        IFEQ      '02'
231900970117     C     TIPCNT        ANDEQ     'CG'
232000970117     C                   Z-ADD     KCU(X2)       KCCFO             4 0           *FORNITORI ITALIA
232100970117E   2C                   ENDIF
232200970117IF  2C     TIPCNT        IFEQ      'RI'
232300970117     C                   Z-ADD     KCU(X2)       RI(Z)
232400970117     C                   ADD       1             Z                 3 0
232500970117E   2C                   ENDIF
232600970117E   1C                   ENDDO
232700970117     C*
232800970117     C                   ENDSR
232900970117     C*------------------------------------------------------------------------*
233000970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
233100970117     C*------------------------------------------------------------------------*
233200970117     C     CARTAB        BEGSR
233300970117     C*--------------------
233400970117     C* TIPI SERVIZIO
233500970117     C*--------------------
233600970117     C                   Z-ADD     *ZEROS        I                 4 0
233700020906     C                   Z-ADD     1             KTBKUT
233800970117     C                   MOVEL     '5E'          KTBCOD
233900970117     C     KE2TAB        SETLL     TABEL00F
234000970117     C     KE2TAB        READE     TABEL00F                               99
234100970117DO  1C     *IN99         DOWEQ     *OFF
234200970117IF  2C     TBLFLG        IFEQ      *BLANKS
234300970117     C                   ADD       1             I
234400970117     C                   MOVEL     TBLUNI        DS5E
234500970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
234600970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
234700970117E   2C                   ENDIF
234800970117     C     KE2TAB        READE     TABEL00F                               99
234900970117E   1C                   ENDDO
235000970117     C*--------------------
235100970117     C* TIPI TARIFFA
235200970117     C*--------------------
235300970117     C                   Z-ADD     *ZEROS        I                 4 0
235400020906     C                   Z-ADD     1             KTBKUT
235500970117     C                   MOVEL     'TR'          KTBCOD
235600970117     C     KE2TAB        SETLL     TABEL00F
235700970117     C     KE2TAB        READE     TABEL00F                               99
235800970117DO  1C     *IN99         DOWEQ     *OFF
235900970117IF  2C     TBLFLG        IFEQ      *BLANKS
236000970117     C                   MOVEL     TBLUNI        DSTR
236100970117IF  3C     §TRUTE        IFEQ      'S'                                          *DA UTILIZZARE
236200970117     C                   ADD       1             I
236300970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
236400970117     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
236500970117E   3C                   ENDIF
236600970117E   2C                   ENDIF
236700970117     C     KE2TAB        READE     TABEL00F                               99
236800970117E   1C                   ENDDO
236900970117     C*--------------------
237000970117     C* TARIFFA DI CARTELLO
237100970117     C*--------------------
237200970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
237300970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
237400970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
237500021202     C                   Z-ADD     *ZEROS        CARKSC
237600021202     C                   Z-ADD     *ZEROS        CARCTR
237700021202     C                   Z-ADD     *ZEROS        CARPRG
237800021202     C                   MOVEL     *BLANKS       CARDIV
237900021202     C                   CLEAR                   TRUL27DS
238000021202     C                   EVAL      I27TLA = 'L'
238100021202     C                   EVAL      I27DTA = DATCOR
238200021202     C                   CALL      'TRUL27R'
238300021202     C                   PARM                    TRUL27DS
238400021202     C*
238500021202IF  1C                   IF        O27ERR <> 'E'
238600021202     C                   Z-ADD     O27KSC        KTMKSC
238700091123     c                   if        %subst(d00op0: 1: 1)='O'
238800970117     C     KTMKSC        SETLL     TNOFM01L
238900970117     C     KTMKSC        READE     TNOFM01L                               99
239000970117X   2C                   ELSE
239100970117     C     KTMKSC        SETLL     TNTAM01L
239200970117     C     KTMKSC        READE     TNTAM01L                               99
239300970117E   2C                   ENDIF
239400970117DO  2C     *IN99         DOWEQ     *OFF
239500970117IF  3C     TAMATB        IFNE      'A'                                          *NO ANNULLATE
239600970117     C     TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
239700970117IF  4C     DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
239800970117     C     DATCOR        ANDLE     TAMDST
239900970117     C                   Z-ADD     TAMKSC        CARKSC
240000970117     C                   Z-ADD     TAMCTR        CARCTR
240100970117     C                   Z-ADD     TAMPRG        CARPRG
240200011003     C                   MOVEL     TAMFLO        DSTA01
240300011003     C                   MOVEL     §TADIV        CARDIV
240400970117     C                   LEAVE
240500970117E   4C                   ENDIF
240600970117E   3C                   ENDIF
240700091123IF  2C                   IF        %subst(d00op0:1 :1)='O'
240800970117     C     KTMKSC        READE     TNOFM01L                               99
240900970117X   3C                   ELSE
241000970117     C     KTMKSC        READE     TNTAM01L                               99
241100970117E   3C                   ENDIF
241200970117E   2C                   ENDDO
241300970117E   1C                   ENDIF
241400970117     C*--------------------
241500970117     C* CODICI TASSAZIONE
241600970117     C*--------------------
241700970117     C*---
241800970117     C* TARIFFA ITALIA: LNP ITALIA -> CARICA CODICI TASSAZIONE ITALIA
241900970117     C* TARIFFA ESTERO: LNP ITALIA -> CARICA CODICI TASSAZIONE ESTERO
242000970117     C*                 LNP ESTERO -> CARICA CODICI TASSAZIONE ITALIA + ESTERO
242100970117     C*---
242200970117     C                   Z-ADD     *ZEROS        I                 4 0
242300970117     C                   Z-ADD     *ZEROS        J                 4 0
242400970117     C                   Z-ADD     *ZEROS        K                 4 0
242500020906     C                   Z-ADD     1             KTBKUT
242600970117     C                   MOVEL     'CT'          KTBCOD
242700970117     C     KE2TAB        SETLL     TABEL00F
242800970117     C     KE2TAB        READE     TABEL00F                               99
242900970117DO  1C     *IN99         DOWEQ     *OFF
243000970117IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
243100970117     C                   MOVEL     TBLUNI        DSCT
243200970312IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
243300970312IF  4C     D00FIE        IFEQ      'I'                                          *TARIFFA ITALIA
243400970117     C     §CTNAR        ANDEQ     *BLANKS                                      *CODICI ITALIA
243500970117     C     D00FIE        OREQ      'E'                                          *TARIFFA ESTERO
243600971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
243700970117     C     §CTNAR        ANDNE     *BLANKS                                      *CODICI ESTERO
243800970117     C     D00FIE        OREQ      'E'                                          *TARIFFA ESTERO
243900971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ESTERO
244000970117     C                   ADD       1             I
244100970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
244200980514     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
244300970117     C                   MOVEL     §CTDES        DCT(I)                         *DESCRIZIONE
244400970117     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
244500970117     C                   MOVEL     §CTCOR        COR(I)                         *ORDINAMENTO
244600050304     C                   MOVEL     §CTRCT        RCT(I)                         *ORDINAMENTO
244700970117     C                   MOVE      §CTCOR        WA1                            *TIPO CODICE
244800970312IF  5C     WA1           IFEQ      '9'
244900970312IF  6C     CTS(I)        IFEQ      'IT'
245000970117     C     CTS(I)        OREQ      'EE'
245100970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
245200970312X   6C                   ELSE
245300970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
245400970312E   6C                   ENDIF
245500970312X   5C                   ELSE
245600970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
245700970312E   5C                   ENDIF
245800970312IF  5C     §CTNAR        IFEQ      *BLANKS
245900970117     C                   MOVEL     'IT'          NAZ(I)                         *NAZIONE ITALIA
246000970312X   5C                   ELSE
246100970117     C                   MOVEL     'EE'          NAZ(I)                         *NAZIONE ESTERO
246200970312E   5C                   ENDIF
246300970117     C* VIDEO->ELEMENTARI
246400970312IF  5C     TCT(I)        IFEQ      'E'
246500970117     C                   ADD       1             J
246600980508     C                   MOVEL     CTS(I)        VCTS(J)                        *CODICE
246700970117     C                   MOVEL     FCR(I)        VFCR(J)                        *FLAG CAPOLUOGO
246800970117     C                   MOVEL     COR(I)        VCOR(J)                        *ORDINAMENTO
246900970117     C                   MOVEL     RAP(I)        VRAP(J)                        *REGIONE
247000980508     C                   MOVEL     'T'           VRCT(J)                        *RAGGRUPPAMENTO
247100970312E   5C                   ENDIF
247200970117     C* VIDEO->REGIONALI
247300970312IF  5C     TCT(I)        IFEQ      'R'
247400970117     C                   ADD       1             K
247500970117     C                   MOVEL     CTS(I)        RCTS(K)                        *CODICE
247600970117     C                   MOVEL     COR(I)        RCOR(K)                        *ORDINAMENTO
247700970312E   5C                   ENDIF
247800970312E   4C                   ENDIF
247900970117E   3C                   ENDIF
248000970117E   2C                   ENDIF
248100970117     C     KE2TAB        READE     TABEL00F                               99
248200970117E   1C                   ENDDO
248300970117     C                   Z-ADD     J             WUVCTS            4 0          *N°COD.TASS.ELEM.VID
248400970117     C                   Z-ADD     K             WURCTS            4 0          *N°COD.TASS.REG.VID.
248500970117     C*
248600970117     C* ORDINA CODICI ELEMENTARI VIDEO
248700970117     C                   EXSR      ORDVCT
248800970117     C*
248900970117     C* ORDINA CODICI REGIONALI VIDEO
249000970117     C                   EXSR      ORDRCT
249100970117     C*
249200970117     C                   ENDSR
249300970117     C*------------------------------------------------------------------------*
249400970117     C* ORDVCT - ORDINA I CODICI TASSAZIONE ELEMENTARI DA EMETTERE A VIDEO
249500970117     C*------------------------------------------------------------------------*
249600970117     C     ORDVCT        BEGSR
249700970117     C*
249800970117DO  1C     1             DO        WUVCTS        I
249900970117IF  2C     VCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
250000970117     C     1             ADD       I             J
250100970117DO  3C     J             DO        WUVCTS        J
250200970117IF  4C     VCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
250300970117IF  5C     VCOR(I)       IFGT      VCOR(J)
250400970117     C                   MOVEL     VCTS(I)       PCTS                           *CODICE
250500970117     C                   MOVEL     VCTS(J)       VCTS(I)
250600970117     C                   MOVEL     PCTS          VCTS(J)
250700970117     C                   MOVEL     VFCR(I)       PFCR                           *FLAG CAPOLUOGO
250800970117     C                   MOVEL     VFCR(J)       VFCR(I)
250900970117     C                   MOVEL     PFCR          VFCR(J)
251000970117     C                   MOVEL     VCOR(I)       PCOR                           *ORDINAMENTO
251100970117     C                   MOVEL     VCOR(J)       VCOR(I)
251200970117     C                   MOVEL     PCOR          VCOR(J)
251300970117     C                   MOVEL     VRAP(I)       PRAP                           *REGIONE
251400970117     C                   MOVEL     VRAP(J)       VRAP(I)
251500970117     C                   MOVEL     PRAP          VRAP(J)
251600970117E   5C                   ENDIF
251700970117E   4C                   ENDIF
251800970117E   3C                   ENDDO                                                  *FINE CICLO UNO
251900970117E   2C                   ENDIF
252000970117E   1C                   ENDDO                                                  *FINE CICLO DUE
252100970117     C*
252200970117     C                   ENDSR
252300970117     C*------------------------------------------------------------------------*
252400970117     C* ORDRCT - ORDINA I CODICI TASSAZIONE REGIONALI DA EMETTERE A VIDEO
252500970117     C*------------------------------------------------------------------------*
252600970117     C     ORDRCT        BEGSR
252700970117     C*
252800970117DO  1C     1             DO        WURCTS        I
252900970117IF  2C     RCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
253000970117     C     1             ADD       I             J
253100970117DO  3C     J             DO        WURCTS        J
253200970117IF  4C     RCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
253300970117IF  5C     RCOR(I)       IFGT      RCOR(J)
253400970117     C                   MOVEL     RCTS(I)       PCTS                           *CODICE
253500970117     C                   MOVEL     RCTS(J)       RCTS(I)
253600970117     C                   MOVEL     PCTS          RCTS(J)
253700970117     C                   MOVEL     RCOR(I)       PCOR                           *ORDINAMENTO
253800970117     C                   MOVEL     RCOR(J)       RCOR(I)
253900981218     C                   MOVEL     PCOR          RCOR(J)
254000981218E   5C                   ENDIF
254100970117E   4C                   ENDIF
254200970117E   3C                   ENDDO                                                  *FINE CICLO UNO
254300970117E   2C                   ENDIF
254400970117E   1C                   ENDDO                                                  *FINE CICLO DUE
254500970117     C*
254600970117     C                   ENDSR
254700970117     C*------------------------------------------------------------------------*
254800970117     C* IMPVD0 - IMPOSTA LA TESTATA
254900970117     C*------------------------------------------------------------------------*
255000970117     C     IMPVD0        BEGSR
255100970117     C*
255200970117     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
255300970117     C*---
255400970117     C* TESTATA
255500970117     C*---
255600970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
255700970117IF  1C     D00TLA        IFEQ      'P'
255800970117     C                   MOVEL     DVID(1)       V0CTLA                         *LIVELLO PROCEDURA
255900970117X   1C                   ELSE
256000970117     C                   MOVEL     DVID(2)       V0CTLA                         *LIVELLO PROCEDURA
256100970117E   1C                   ENDIF
256200091123     C                   MOVEL     'Prg.'        d0cprg                         *TARIFFA O OFFERTA
256300091123     c                   if        %subst(d00op0: 1: 1) = 'O'
256400091123     c                   if        d00dsf =' '
256500091123     C                   MOVEL     'Visita '     d0cKsc                         *TARIFFA O OFFERTA
256600091123X   1C                   ELSE
256700091123     C                   MOVEL     'Tratt. '     d0cksc                         *TARIFFA O OFFERTA
256800091123     C                   MOVEL     'Ver.'        d0cprg                         *TARIFFA O OFFERTA
256900091123E   1C                   ENDIF
257000091123X   1C                   ELSE
257100091123     C                   MOVEL     'Cliente'     d0cksc                         *TARIFFA O OFFERTA
257200091123E   1C                   ENDIF
257300970117IF  1C     $TGES         IFEQ      'I'
257400970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
257500970122     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
257600970117X   1C                   ELSE
257700970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
257800970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
257900970117E   1C                   ENDIF
258000970117     C* CAMPI
258100970117     C                   Z-ADD     D00KSC        V0CKSC
258200970117     C                   Z-ADD     D00CTR        V0CCTR
258300970117     C                   Z-ADD     D00PRG        V0CPRG
258400970117     C                   MOVEL     D00FIE        V0CFIE
258500970117     C                   MOVEL     D00TSP        V0CTSP
258600970117     C                   Z-ADD     D00LNP        V0CLNP
258700011003     C                   MOVEL     D00DIV        V0CDIV
258800970117IF  1C     D00DTC        IFGT      *ZEROS
258900970117     C                   Z-ADD     D00DTC        G08INV
259000970117     C                   Z-ADD     *ZEROS        G08DAT
259100970117     C                   MOVEL     '3'           G08ERR
259200970117     C                   CALL      'XSRDA8'
259300970117     C                   PARM                    WLBDA8
259400970117     C                   Z-ADD     G08DAT        V0CDTC                         *DATA PER CALC.COMP.
259500970117E   1C                   ENDIF
259600970117     C* DECODIFICHE
259700970117     C                   MOVEL     *BLANKS       V0DKSC
259800970117IF  1C     D00KSC        IFGT      *ZEROS
259900020906     C                   Z-ADD     1             KACKUT
260000970117     C                   Z-ADD     KCCCL         KACKCC
260100970117     C                   Z-ADD     D00KSC        KACKSC
260200091203     C*    KEYACO        CHAIN     CNACO00F                           99
260300091123IF  2C                   IF        %subst(d00op0: 1: 1) ='T'                    *TARIFFA
260400000211     C     KEYACO        CHAIN     CNACO00F                           99
260500000211X   2C                   ELSE                                                   *OFFERTA
260600000211     C     KEYACO        CHAIN     TFACO00F                           99
260700000211E   2C                   ENDIF
260800000211IF  2C     *IN99         IFEQ      *OFF
260900000211     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
261000091203     c                   else
261100091203     c                   clear                   tnte04ds
261200091203     C                   movel     D00dsf        d04dsf
261300091203     C                   Z-ADD     D00KSC        d04nrv
261400091203     c                   call      'TNTE04R'
261500091203     c                   parm                    kpjba
261600091203     c                   parm                    tnte04ds
261700091203     c                   movel     d04rag        v0dksc
261800000211E   2C                   ENDIF
261900970117E   1C                   ENDIF
262000970117     C*
262100970117     C                   MOVEL     *BLANKS       V0DCTR
262200970117IF  1C     D00KSC        IFGT      *ZEROS
262300970117     C                   MOVEL     D00CTR        WA1               1
262400970117     C                   SETOFF                                           98
262500970117     C                   Z-ADD     1             I
262600970117     C     WA1           LOOKUP    TTR(I)                                 98
262700970117IF  2C     *IN98         IFEQ      *ON
262800970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
262900970117E   2C                   ENDIF
263000970117E   1C                   ENDIF
263100970117     C*
263200970117     C                   MOVEL     *BLANKS       V0DTSP
263300970117IF  1C     D00TSP        IFNE      *BLANKS
263400970117     C                   SETOFF                                           98
263500970117     C                   Z-ADD     1             I
263600970117     C     D00TSP        LOOKUP    TSP(I)                                 98
263700970117IF  2C     *IN98         IFEQ      *ON
263800970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
263900970117E   2C                   ENDIF
264000970117E   1C                   ENDIF
264100970117     C*
264200970117     C                   MOVEL     *BLANKS       V0DFIE
264300970117IF  1C     D00FIE        IFEQ      'I'
264400970117     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
264500970117E   1C                   ENDIF
264600970117IF  1C     D00FIE        IFEQ      'E'
264700970117     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
264800970117E   1C                   ENDIF
264900970117     C*
265000970117     C                   MOVEL     *BLANKS       V0DLNP
265100970117IF  1C     D00LNP        IFGT      *ZEROS
265200970117     C                   Z-ADD     D00LNP        KORFIL
265300970117     C     KEYORG        CHAIN     AZORG01L                           99
265400970117IF  2C     *IN99         IFEQ      *OFF
265500970117     C     ORGFVA        ANDEQ     *BLANKS
265600970117     C     ORGFAG        ANDNE     'V'
265700970117     C                   MOVEL     ORGDES        V0DLNP
265800970117E   2C                   ENDIF
265900970117E   1C                   ENDIF
266000970117     C*---
266100970117     C* FINESTRA F05-CARICA DA ALTRA TARIFFA
266200970117     C*---
266300980508     C                   MOVEL     'T'           V2WCTO
266400970117     C                   Z-ADD     CARKSC        V2WKSC
266500970117     C                   Z-ADD     CARCTR        V2WCTR
266600970117     C                   Z-ADD     CARPRG        V2WPRG
266700970117     C                   Z-ADD     D00LNP        V2WLNP
266800011003     C                   MOVEL     CARDIV        V2WDIV
266900970117     C*
267000970117     C                   ENDSR
267100970117     C*------------------------------------------------------------------------*
267200970117     C* MEMREC - MEMORIZZA IN SCHIERE I RECORD DA MANUTENZIONARE
267300970117     C*------------------------------------------------------------------------*
267400970117     C     MEMREC        BEGSR
267500970117     C*
267600980708     C* LEGGE I DATI TIPO DEL CLIENTE/DELLA VISITA
267700091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
267800980508     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
267900970122     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
268000970117     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
268100970117     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
268200970117     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
268300970117     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
268400011001     C     KE2ETT        SETLL     TEETT01L                               99
268500970117     C*
268600970117     C* SE CODICI INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
268700970117IF  1C     *IN99         IFEQ      *OFF
268800970117     C                   MOVEL     DOPE(1)       V0COPE                         I N S E R I M E N T
268900970117     C                   WRITE     TE03D00                                      *TESTATA
269000970117     C                   WRITE     TE03D01                                      *MESSAGGIO SF VUOTO
269100970117     C                   EXSR      F05GES
269200980508     C*
269300980508     C* SE NON E' STATO CONFERMATO IL CARICAMENTO DA ALTRA TARIFFA, ALLORA
269400980508     C* CARICA I CODICI TASSAZIONE DA TABELLA
269500980508IF  2C     $CONF         IFEQ      'N'                                          *NESSUNA CONFERMA
269600980508     C                   EXSR      CARSTD                                       *CARICA DATI STANDAR
269700980508     C                   EXSR      AGGSCV                                       *AGGIORNA SCHIERE VI
269800980508E   2C                   ENDIF
269900970117X   1C                   ELSE
270000980708     C                   MOVEL     DOPE(2)       V0COPE                         *V A R I A Z I O N E
270100980708     C                   MOVEL     D00CTO        WCTO
270200980708     C                   Z-ADD     D00KSC        WKSC
270300980708     C                   Z-ADD     D00CTR        WCTR
270400980708     C                   Z-ADD     D00PRG        WPRG
270500980708     C                   Z-ADD     D00LNP        WLNP
270600980708     C                   MOVEL     D00FIE        WFIE
270700011003     C                   MOVEL     D00DIV        WDIV
270800980708     C                   EXSR      MEMETT                                       *MEM.DATI E AGG.VIDE
270900980708E   1C                   ENDIF
271000970117IF  1C     $TGES         IFEQ      'I'
271100970117     C                   MOVEL     *BLANKS       V0COPE
271200970117E   1C                   ENDIF
271300970117     C*
271400970117     C                   ENDSR
271500970129     C*------------------------------------------------------------------------*
271600970129     C* MEMCAR - MEMORIZZA I DATI DELLA TARIFFA DI CARTELLO
271700970129     C*------------------------------------------------------------------------*
271800970129     C     MEMCAR        BEGSR
271900980515     C*
272000980515     C* IMPOSTA I CODICI TASSAZIONE STANDARD
272100980708     C                   MOVEL     VCTS          CTS8
272200980708     C                   MOVEL     VRAP          RAP8
272300980515     C*
272400980515     C* PULISCE LE SCHIERE DI MEMORIZZAZIONE
272500980515     C                   CLEAR                   RCT8
272600980514     C                   CLEAR                   PPE8
272700970129     C* CARCAMENTO DATI
272800980515     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
272900980508     C                   MOVEL     'T'           KTTCTO                         *TARIFFA O OFFERTA
273000980508     C                   Z-ADD     CARKSC        KTTKSC                         *CLIENTE O VISITA
273100970129     C                   Z-ADD     CARCTR        KTTCTR                         *TARIFFA
273200970129     C                   Z-ADD     CARPRG        KTTPRG                         *PROGRESSIVO
273300970129     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
273400970129     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
273500011001     C     KE2ETT        SETLL     TEETT01L                               99
273600011001     C     KE2ETT        READE(N)  TEETT01L                               99
273700970129DO  1C     *IN99         DOWEQ     *OFF
273800970129IF  2C     ETTATB        IFEQ      *BLANKS
273900970129     C                   SETOFF                                           98
274000970129     C                   Z-ADD     1             I
274100980515     C     ETTCTS        LOOKUP    CTS8(I)                                98
274200970129IF  3C     *IN98         IFEQ      *ON
274300970129     C                   MOVEL     ETTRCT        RCT8(I)                        *RAGGRUPPAMENTO
274400970129     C                   Z-ADD     ETTPPE        PPE8(I)                        *%INCIDENZA
274500980911     C     ETTRCT        IFEQ      'N'
274600980911     C     ETTPPE        ANDEQ     *ZEROS
274700980911     C                   MOVEL     *BLANKS       RCT8(I)
274800980911     C                   ENDIF
274900970129E   3C                   ENDIF
275000970129E   2C                   ENDIF
275100011001     C     KE2ETT        READE(N)  TEETT01L                               99
275200970129E   1C                   ENDDO
275300970129     C*
275400970129     C                   ENDSR
275500980508     C*------------------------------------------------------------------------*
275600980508     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
275700980508     C*------------------------------------------------------------------------*
275800980508     C     CTRENT        BEGSR
275900980508     C*--------------------
276000980508     C* INTERROGAZIONE -> DATI TIPO INESISTENTI
276100980508     C*--------------------
276200980508IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
276300980508     C                   MOVEL     '0'           WECTS             1            *NO ESISTENZA CTS
276400091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE/FILIALE
276500980508     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
276600980508     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE P VISITA
276700980508     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
276800980508     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
276900980508     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
277000980508     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
277100011001     C     KE2ETT        SETLL     TEETT01L                               99
277200011001     C     KE2ETT        READE(N)  TEETT01L                               99
277300980508DO  2C     *IN99         DOWEQ     *OFF                                         *NON TROVATO
277400980508IF  3C     ETTATB        IFEQ      *BLANKS                                      *NON ANNULLATI
277500980508     C                   MOVEL     '1'           WECTS                          *SI ESISTENZA CTS
277600980508     C                   LEAVE
277700980508E   3C                   ENDIF
277800011001     C     KE2ETT        READE(N)  TEETT01L                               99
277900980508E   2C                   ENDDO
278000980508IF  2C     WECTS         IFEQ      '0'
278100980508     C                   MOVEL     '1'           WERR                           *ERRORE
278200980508     C                   MOVEL     MSG(1)        D00MSG
278300980508     C                   MOVEL     '1'           D00ERR
278400980508     C                   MOVEL     'S'           $FINE                          *USCITA PGM
278500980508E   2C                   ENDIF
278600980508E   1C                   ENDIF
278700980508     C*
278800980508     C                   ENDSR
278900970117     C*------------------------------------------------------------------------*
279000970117     C* *INZSR - OPERAZIONI INIZIALI
279100970117     C*------------------------------------------------------------------------*
279200970117     C     *INZSR        BEGSR
279300970117     C*--------------------
279400970117     C* RICEVIMENTO PARAMETRI
279500970117     C*--------------------
279600970117     C     *ENTRY        PLIST
279700970117     C                   PARM                    KPJBA
279800970117     C                   PARM                    DSTE00
279900041202     c* apertura controllata di alcuni file xchè solo di filiale
280000041202     c                   open      tfaco00f                             95
280100041202     c                   open      tnofm01l                             95
280200970117     C*--------------------
280300970117     C* VARIABILI RIFERITE AL DATABASE
280400970117     C*--------------------
280500970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
280600970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
280700970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
280800970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
280900970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
281000970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
281100970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
281200970117     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
281300011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
281400980508     C     *LIKE         DEFINE    ETTCTO        KTTCTO
281500970117     C     *LIKE         DEFINE    ETTKSC        KTTKSC
281600970117     C     *LIKE         DEFINE    ETTCTR        KTTCTR
281700970117     C     *LIKE         DEFINE    ETTPRG        KTTPRG
281800970117     C     *LIKE         DEFINE    ETTLNP        KTTLNP
281900970117     C     *LIKE         DEFINE    ETTFIE        KTTFIE
282000970117     C     *LIKE         DEFINE    ETTCTS        KTTCTS
282100970117     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
282200970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
282300970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
282400011003     C     *LIKE         DEFINE    §TADIV        CARDIV
282500970128     C     *LIKE         DEFINE    ETTCTS        DEPCTS
282600970128     C     *LIKE         DEFINE    ETTPPE        DEPPPE
282700020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
282800970117     C     *LIKE         DEFINE    ETTCTS        PCTS                           *DI PARCHEGGIO
282900970117     C     *LIKE         DEFINE    §CTFCR        PFCR
283000970117     C     *LIKE         DEFINE    §CTCOR        PCOR
283100970117     C     *LIKE         DEFINE    §CTRAP        PRAP
283200970117     C     *LIKE         DEFINE    ETTPPE        PPPE
283300980708     C     *LIKE         DEFINE    D00CTO        WCTO
283400980708     C     *LIKE         DEFINE    D00KSC        WKSC
283500980708     C     *LIKE         DEFINE    D00CTR        WCTR
283600980708     C     *LIKE         DEFINE    D00PRG        WPRG
283700980708     C     *LIKE         DEFINE    D00LNP        WLNP
283800980708     C     *LIKE         DEFINE    D00FIE        WFIE
283900011003     C     *LIKE         DEFINE    D00DIV        WDIV
284000020729     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
284100020729     C     *LIKE         DEFINE    TBEKE1        KTEKE1
284200970117     C*--------------------
284300970117     C* CHIAVI DI LETTURA
284400970117     C*--------------------
284500970117     C* LETTURA TABEL00F - COMPLETA
284600970117     C     KEYTAB        KLIST
284700970117     C                   KFLD                    KTBKUT                         *UTENTE
284800970117     C                   KFLD                    KTBCOD                         *TABELLA
284900970117     C                   KFLD                    KTBKEY                         *CODICE
285000970117     C* LETTURA TABEL00F - PARZIALE
285100970117     C     KE2TAB        KLIST
285200970117     C                   KFLD                    KTBKUT                         *UTENTE
285300970117     C                   KFLD                    KTBCOD                         *TABELLA
285400970117     C* LETTURA AZORG01L - COMPLETA
285500970117     C     KEYORG        KLIST
285600970117     C                   KFLD                    KORFIL                         *FILIALE
285700970117     C* LETTURA CNACO01L - COMPLETA
285800970117     C     KEYACO        KLIST
285900970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
286000970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
286100970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
286200011001     C* LETTURA TEETT01L - PARZIALE
286300970117     C     KE2ETT        KLIST
286400980508     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
286500980508     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
286600970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
286700970117     C                   KFLD                    KTTCTR                         *TARIFFA
286800970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
286900970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
287000970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
287100011001     C* LETTURA TEETT01L - COMPLETA
287200970117     C     KEYETT        KLIST
287300980508     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
287400970117     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
287500970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
287600970117     C                   KFLD                    KTTCTR                         *TARIFFA
287700970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
287800970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
287900970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
288000970117     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
288100020729     C* LETTURA TNTBE01L - PARZIALE
288200020729     C     KEYTBE        KLIST
288300020729     C                   KFLD                    KTECOD                         *UTENTE
288400020729     C                   KFLD                    KTEKE1                         *CODICE
288500970117     C*--------------------
288600970117     C* VARIABILI NON RIFERITE AL DATABASE
288700970117     C*--------------------
288800970117     C     *LIKE         DEFINE    NR1           SF1EOF                         *DI DEPOSITO
288900970117     C     *LIKE         DEFINE    NR2           SF2EOF
289000970117     C     *LIKE         DEFINE    NR3           SF3EOF
289100970117     C     *LIKE         DEFINE    $PC1          $DPC1                          *DEPOSITO NRR 1°SFIL
289200970129     C     *LIKE         DEFINE    V1SOPZ        DEPOPZ
289300970117     C*--------------------
289400970117     C* VARIABILI DI CONTROLLO
289500970117     C*--------------------
289600970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
289700970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
289800970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
289900970117     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
290000970117     C                   MOVEL     '0'           WERR              1            *ERRORE
290100970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
290200970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
290300970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
290400970619     C                   MOVEL     'N'           WEETT             1            *ESISTENZA CTS
290500970117     C*
290600970117     C* REPERISCE LA DATA CORRENTE
290700970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
290800970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
290900970117     C                   Z-ADD     WN8           G08DAT
291000970117     C                   Z-ADD     *ZEROS        G08INV
291100970117     C                   MOVEL     '0'           G08ERR
291200970117     C                   CALL      'XSRDA8'
291300970117     C                   PARM                    WLBDA8
291400970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
291500970117     C*
291600970117     C* 02->GESTIONE, 05->INTERROGAZIONE
291700970117     C                   MOVE      D00OP0        WA2               2
291800970117IF  1C     WA2           IFEQ      '02'
291900970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
292000970117X   1C                   ELSE
292100970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
292200970117E   1C                   ENDIF
292300971204     C*
292400971204     C* REPERISCE IL FLAG ITALIA/ESTERO PER LA LNP DI ENTRATA
292500971204     C                   MOVEL     D00LNP        WLNP
292600971204     C                   MOVEL     *BLANKS       WFL1
292700971204     C                   EXSR      CHKLNP
292800971204     C                   MOVEL     WFL1          DLPFL1
292900970117     C*
293000970117     C* CARICA I CAPICONTO DI CONTABILITA'
293100970117     C                   EXSR      CARCC
293200980508     C*
293300980508     C* CONTROLLI INIZIALI DI ENTRATA
293400980508     C                   EXSR      CTRENT
293500970117     C*
293600970117     C* CARICA LE TABELLE OCCORRENTI
293700970117     C                   EXSR      CARTAB
293800970117     C*
293900970117     C* IMPOSTA LA TESTATA
294000970117     C                   EXSR      IMPVD0
294100970129     C*
294200970129     C* MEMORIZZA I DATI DELLA TARIFFA DI CARTELLO
294300970129     C                   EXSR      MEMCAR
294400970117     C*
294500970117     C* MEMORIZZA IN SCHIERE I RECORD DA MANUTENZIONARE
294600970117     C                   EXSR      MEMREC
294700970117     C*
294800970117     C                   ENDSR
294900970117     O*------------------------------------------------------------------------*
295000980508** -DVID-
295100970117PORTO               1
295200970117DELTA               2
2953009701173
2954009701174
2955009701175
295600970117** -DDEC-
295700970117ITALIA                   1
295800970117ESTERO                   2
295900970117NON APPARE IN TARIFFA    3
296000970117APPARE IN TARIFFA        4
296100970117APPARE IN XXXXXXXXXXXXXXX5
296200970117SITUAZIONE MISTA         6
296300980508SEDE                     7
296400980508FILIALE                  8
2965009701179
296600980818TUTTE LE FILIALI         10
296700970117** -DOPE-
296800970117* INSERIMENTO *1
296900970117* VARIAZIONE * 2
297000970122INTERROGAZIONE 3
297100970122 * GESTIONE *  4
2972009701175
297300970117** -MSG-
297400980508TNTE03R- Non esistono i dati tipo per la tariffa/visita richiesta             1
297500970117TNTE03R- Raggruppamento codice tassazione errato                              2
297600970117TNTE03R- Se il codice tass. è da esprimere immettere la %incidenza            3
297700970117TNTE03R- Se il codice tass. non è da esprimere lasciare vuota la %incidenza   4
297800970117TNTE03R- Il totale della %incidenza di tutti i codici tass. deve essere 100   5
297900970128TNTE03R- Togliere nei codici tassazione della regione il raggruppamento       6
298000970128TNTE03R- Togliere nei codici tassazione della regione la % di incidenza       7
298100970129TNTE03R- Inserire in tutti i codici tassazione il reggruppamento della regione8
298200970129TNTE03R- Inserire in tutti i codici tassazione la % di incidenza              9
298300970128TNTE03R- La % regione e la somma delle % dei codici tass. deve esser uguale   10
298400970117TNTE03R- Opzione errata                                                       11
298500970117TNTE03R- Immettere SI per confermare oppure NO                                12
298600970117TNTE03R- Dati tipo richiesti non trovati                                      13
298700970117TNTE03R- Caricare i codici tassazione da altra tariffa o da altra offerta     14
298800970117TNTE03R- Filiale errata                                                       15
298900970117TNTE03R- "T" per caricare i dati tariffa, "O" per caricare i dati offerta     16
299000970117TNTE03R- Tariffa Italia. Filiale errata                                       17
299100970117TNTE03R- Tariffa Estero con linea di partenza italia. Filiale errata          18
299200970117TNTE03R- Tariffa Estero con linea di partenza estero. Filiale errata          19
299300970117Scorrimento indietro o avanti oltre il primo o l'ultimo record del file.      20
299400970129TNTE03R- Inserire nei codici tassazione con raggruppamento la % di incidenza  21
299500970129TNTE03R- Inserire nei codici tassazione con % di incidenza il raggruppamento  22
299600970415TNTE03R- La % regione non puo' essere inferiore a xxx,x                       23
299700980508TNTE02R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    24
299800050304ERRORE NEI CODICI TASSAZIONE AVVISARE CED
299900050304Per tariffe estero non è possibile inserire il raggruppamento N
300000050304Per questo cod.tassazione esiste il raggruppamento imposto x
300100060825Il codice tariffa scelto incongruente con quello originale                    28
300200970117TNTE03R- 29
300300980508TNTE03R- 30
