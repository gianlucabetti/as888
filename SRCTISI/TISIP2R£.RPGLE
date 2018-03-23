000100970910     H*------------------------------------------------------------------------*
000200970619     H* CAPPARIO - STAMPA TEMPI DI TRAZIONE - RICHIESTA PARAMETRI
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800970619     FTISI25D   CF   E             WORKSTN INFDS(DEVDS1)
000900970619     F                                     SFILE(SI25DMS:MSGULT)
001000970910     FAZORG01L  IF   E           K DISK
001100970910     D*------------------------------------------------------------------------*
001200970910     D* SCHIERE
001300970910     D*------------------------------------------------------------------------*
001400970910     D*------------
001500970910     D* MESSAGGI
001600970910     D*------------
001700970910     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001800970910     D*------------
001900970910     D* COMANDI
002000970910     D*------------
002100970910     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002200970910     D*------------
002300970910     D* DESCRIZIONI TASTI FUNZIONALI
002400970910     D*------------
002500970910     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970910     D*------------
002700970910     D* FUNZIONI DA CARICARE A VIDEO
002800970910     D*------------
002900970910     D FUV             S             19    DIM(24)
003000970910     D*------------------------------------------------------------------------*
003100970910     D* INPUT
003200970910     D*------------------------------------------------------------------------*
003300970910     D*------------
003400970910     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970910     D*------------
003600970910     D FUDDS           DS
003700970910     D  FUDDE1                 1      1
003800970910     D  FUDFUN                 2      3
003900970910     D  FUDDE2                 4     19
004000970910     D  FUDFI0                20     34
004100970910     D  FUDATT                35     35
004200970910     D  FUDFI1                36     80
004300970910     D  FUDDES                 1     19
004400970910     D  FUDRIG                 1     80
004500970910     D*------------
004600970910     D* DS GESTIONE TASTI FUNZIONALI
004700970910     D*------------
004800970910     D DSKEY         E DS
004900970910     D DEVDS1          DS
005000970910     D  KEY                  369    369
005100970910     D*------------
005200970910     D* DS "XMSG" - GESTIONE MESSAGGI
005300970910     D*------------
005400970910     D DSMSG         E DS
005500970910     D*------------
005600970910     D* DS DI PROCEDURA
005700970910     D*------------
005800970910     D DSSI00        E DS                  EXTNAME(TISI00DS)
005900970910     D*------------
006000970910     D* DS PARAMETRI DI LANCIO
006100970910     D*------------
006200970610     D PARAM           DS                  INZ
006300970910     D  PARTOR                 1      1
006400970619     D  PARPTA                 2      2
006500970619     D  PARLP1                 3      5  0
006600970619     D  PARLP2                 6      8  0
006700970619     D  PARLA1                 9     11  0
006800970619     D  PARLA2                12     14  0
006900970910     D*-------------
007000970910     D* ARCHITETTURA
007100970910     D*------------
007200970910     D KPJBA         E DS
007300970910     C*------------------------------------------------------------------------*
007400970910     C* MAIN LINES
007500970910     C*------------------------------------------------------------------------*
007600970910     C*
007700970910     C* NON CI SONO ERRORI IN ENTRATA
007800970910IF  1C     ERRL00        IFEQ      '0'
007900970910     C*
008000970910     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008100970910DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008200970910     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008300970910     C     FINL00        OREQ      '1'                                          *FINE PGM
008400970910     C*
008500970910     C* EMETTE FORMATO VIDEO
008600970910     C                   EXSR      BUTFMR
008700970910     C*
008800970910     C* GESTIONE TASTI FUNZIONALI
008900970910     C                   EXSR      FUNCON
009000970910E   2C                   ENDDO
009100970910     C*
009200970910     C* CI SONO ERRORI IN ENTRATA
009300970910X   1C                   ELSE
009400970910     C                   MOVEL     '1'           D00ERR
009500970910     C                   MOVEL     DSMSMS        D00MSG
009600970910E   1C                   ENDIF
009700970910     C* FINE PGM
009800970910     C                   SETON                                        LR
009900970910     C*------------------------------------------------------------------------*
010000970910     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010100970910     C*------------------------------------------------------------------------*
010200970910     C     BUTFMR        BEGSR
010300970910     C*
010400970910     C* EMETTE LA TESTATA
010500970619     C                   WRITE     SI25D01
010600970910     C*
010700970910     C* SPEDISCE EVENTUALE MESSAGGIO
010800970910     C                   EXSR      SNDMSG
010900970910     C*
011000970910     C* EMETTE LE FUNZIONI
011100970619     C                   WRITE     SI25D02
011200970910     C*
011300970910     C* EMETTE IL VIDEO
011400970619     C                   EXFMT     SI25D03
011500970910     C*
011600970910     C* REIMPOSTA FLAG DI ERRORE
011700970910     C                   MOVEL     '0'           ERRL00
011800970910     C*
011900970910     C* REIMPOSTA INDICATORI DI ERRORE
012000970910     C                   SETOFF                                         5859
012100970910     C                   SETOFF                                       606162
012200970910     C                   SETOFF                                       636465
012300970910     C                   SETOFF                                       666768
012400970910     C                   SETOFF                                       697071
012500970910     C                   SETOFF                                       727374
012600970910     C                   SETOFF                                       757677
012700970910     C                   SETOFF                                       787980
012800970910     C                   SETOFF                                       818283
012900970910     C                   SETOFF                                       848586
013000970910     C                   SETOFF                                       878889
013100970910     C*
013200970910     C                   ENDSR
013300970910     C*------------------------------------------------------------------------*
013400970910     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500970910     C*------------------------------------------------------------------------*
013600970910     C     SNDMSG        BEGSR
013700970910     C*
013800970910     C* PULISCE IL SFILE MESSAGGI
013900970910     C                   MOVEL     '1'           *IN40
014000970619     C                   WRITE     SI25DMC
014100970910     C                   MOVEL     '0'           *IN40
014200970910     C*
014300970910     C* VISUALIZZA IL MESSAGGIO
014400970910     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970910     C                   CALL      'XMSG'        XMSGPA
014600970910IF  1C     DSMSER        IFEQ      *ZEROS
014700970910     C                   MOVEL     DSMSKE        MSGKEY
014800970910E   1C                   ENDIF
014900970910     C                   Z-ADD     1             MSGULT
015000970619     C                   WRITE     SI25DMS
015100970619     C                   WRITE     SI25DMC
015200970910     C*
015300970910     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400970910     C                   MOVEL     *BLANKS       DSMSMS
015500970910     C*
015600970910     C                   ENDSR
015700970910     C*------------------------------------------------------------------------*
015800970910     C* ENTGES - GESTIONE ENTER -> INVIO
015900970910     C*------------------------------------------------------------------------*
016000970910     C     ENTGES        BEGSR
016100970910     C*
016200970910     C* CONTROLLA I CAMPI IMMESSI
016300970910     C                   EXSR      DECODI
016400970910     C*
016500970910     C                   ENDSR
016600970910     C*------------------------------------------------------------------------*
016700970910     C* F03GES - GESTIONE F03 -> FINE
016800970910     C*------------------------------------------------------------------------*
016900970910     C     F03GES        BEGSR
017000970910     C*
017100970910     C* IMPOSTA "PREMUTO F03"
017200970910     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017300970910     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
017400970910     C*
017500970910     C                   ENDSR
017600970910     C*------------------------------------------------------------------------*
017700970910     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
017800970910     C*------------------------------------------------------------------------*
017900970910     C     F06GES        BEGSR
018000970723     C*
018100970723     C* CONTROLLA I CAMPI IMMESSI
018200970723     C                   EXSR      DECODI
018300970910     C*
018400970910     C* LANCIA IL PROGRAMMA DI STAMPA
018500970723IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018600970910     C                   MOVEL     V1CTOR        PARTOR
018700970619     C                   MOVEL     V1CPTA        PARPTA
018800970619     C                   MOVEL     LP1V1C        PARLP1
018900970619     C                   MOVEL     LP2V1C        PARLP2
019000970619     C                   MOVEL     LA1V1C        PARLA1
019100970619     C                   MOVEL     LA2V1C        PARLA2
019200970910     C                   MOVEL     PARAM         KPJBU
019300970619     C                   MOVEL     'SI26'        KCOAZ
019400970612     C                   CALL      'BCH10'
019500970910     C                   PARM                    KPJBA
019600970619     C                   MOVEL     '1'           FINL00
019700970723E   1C                   ENDIF
019800970910     C*
019900970910     C                   ENDSR
020000970910     C*------------------------------------------------------------------------*
020100970910     C* F12GES - GESTIONE F12 -> RITORNO
020200970910     C*------------------------------------------------------------------------*
020300970910     C     F12GES        BEGSR
020400970910     C*
020500970910     C* IMPOSTA "PREMUTO F12"
020600970910     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020700970910     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
020800970910     C*
020900970910     C                   ENDSR
021000970910     C*------------------------------------------------------------------------*
021100970910     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
021200970910     C*------------------------------------------------------------------------*
021300970910     C     F22GES        BEGSR
021400970910     C*
021500970910     C                   Z-ADD     80            LENGH            15 5
021600970910     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
021700970910     C                   PARM                    CMD(1)
021800970910     C                   PARM                    LENGH
021900970910     C*
022000970910     C                   ENDSR
022100970910     C*------------------------------------------------------------------------*
022200970910     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022300970910     C*------------------------------------------------------------------------*
022400970910     C     F24GES        BEGSR
022500970910     C*
022600970910     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022700970910     C*
022800970910     C                   ENDSR
022900970910     C*------------------------------------------------------------------------*
023000970910     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023100970910     C*------------------------------------------------------------------------*
023200970910     C     DECODI        BEGSR
023300970910     C*--------------------
023400970910     C* ORDINAMENTO
023500970910     C*--------------------
023600970910IF  1C     ERRL00        IFEQ      '0'
023700970619IF  2C     V1CTOR        IFEQ      'P'
023800970619     C     V1CTOR        OREQ      'A'
023900970910X   2C                   ELSE
024000970619     C                   SETON                                        58
024100970910     C                   MOVEL     MSG(11)       DSMSMS
024200970910     C                   MOVEL     '1'           ERRL00
024300970910E   2C                   ENDIF
024400970910E   1C                   ENDIF
024500970619     C*--------------------
024600970619     C* TIPO TEMPO
024700970619     C*--------------------
024800970619IF  1C     ERRL00        IFEQ      '0'
024900970619IF  2C     V1CPTA        IFEQ      'P'
025000970619     C     V1CPTA        OREQ      'T'
025100970619     C     V1CPTA        OREQ      'A'
025200970619     C     V1CPTA        OREQ      *BLANKS
025300970619X   2C                   ELSE
025400970619     C                   SETON                                        59
025500970619     C                   MOVEL     MSG(11)       DSMSMS
025600970619     C                   MOVEL     '1'           ERRL00
025700970619E   2C                   ENDIF
025800970619E   1C                   ENDIF
025900970910     C*--------------------
026000970619     C* LINEA PARTENZA DAL
026100970910     C*--------------------
026200970619     C     *LIKE         DEFINE    V1CLP1        LP1V1C
026300970619     C                   MOVEL     *ZEROS        LP1V1C
026400970910IF  1C     ERRL00        IFEQ      '0'
026500970619     C                   MOVEL     *BLANKS       V1DLP1
026600970619IF  2C     V1CLP1        IFNE      *BLANKS
026700970619     C                   MOVEL     V1CLP1        WLIN
026800970910     C                   MOVEL     *BLANKS       WDLIN
026900970910     C                   EXSR      CHKLIN
027000970619     C                   MOVEL     WLIN          V1CLP1
027100970619     C                   MOVEL     WDLIN         V1DLP1
027200970619     C                   MOVEL     V1CLP1        LP1V1C
027300970910IF  3C     ERRL00        IFEQ      '1'
027400970619     C                   SETON                                        60
027500970619     C                   MOVEL     MSG(13)       DSMSMS
027600970910E   3C                   ENDIF
027700970910E   2C                   ENDIF
027800970910E   1C                   ENDIF
027900970910     C*--------------------
028000970619     C* LINEA PARTENZA AL
028100970910     C*--------------------
028200970619     C     *LIKE         DEFINE    V1CLP2        LP2V1C
028300970619     C                   MOVEL     *ZEROS        LP2V1C
028400970619IF  1C     ERRL00        IFEQ      '0'
028500970619     C                   MOVEL     *BLANKS       V1DLP2
028600970619IF  2C     V1CLP2        IFNE      *BLANKS
028700970619     C                   MOVEL     V1CLP2        WLIN
028800970619     C                   MOVEL     *BLANKS       WDLIN
028900970619     C                   EXSR      CHKLIN
029000970619     C                   MOVEL     WLIN          V1CLP2
029100970619     C                   MOVEL     WDLIN         V1DLP2
029200970619     C                   MOVEL     V1CLP2        LP2V1C
029300970619IF  3C     ERRL00        IFEQ      '1'
029400970619     C                   SETON                                        61
029500970619     C                   MOVEL     MSG(13)       DSMSMS
029600970619E   3C                   ENDIF
029700970619E   2C                   ENDIF
029800970619E   1C                   ENDIF
029900970910     C*--------------------
030000970619     C* LINEA PARTENZA DAL+AL
030100970910     C*--------------------
030200970910     C* SE DAL ANCHE AL E VICEVERSA
030300970910IF  1C     ERRL00        IFEQ      '0'
030400970619IF  2C     V1CLP1        IFNE      *BLANKS
030500970619     C     V1CLP2        ANDEQ     *BLANKS
030600970619     C     V1CLP2        ORNE      *BLANKS
030700970619     C     V1CLP1        ANDEQ     *BLANKS
030800970619     C                   SETON                                        6061
030900970910     C                   MOVEL     MSG(14)       DSMSMS
031000970910     C                   MOVEL     '1'           ERRL00
031100970910E   2C                   ENDIF
031200970910E   1C                   ENDIF
031300970910     C* DAL <= AL
031400970910IF  1C     ERRL00        IFEQ      '0'
031500970619IF  2C     V1CLP1        IFNE      *BLANKS
031600970619IF  3C     V1CLP1        IFGT      V1CLP2
031700970619     C                   SETON                                        6061
031800970910     C                   MOVEL     MSG(15)       DSMSMS
031900970910     C                   MOVEL     '1'           ERRL00
032000970910E   3C                   ENDIF
032100970910E   2C                   ENDIF
032200970910E   1C                   ENDIF
032300970619     C*--------------------
032400970619     C* LINEA ARRIVO DAL
032500970619     C*--------------------
032600970619     C     *LIKE         DEFINE    V1CLA1        LA1V1C
032700970619     C                   MOVEL     *ZEROS        LA1V1C
032800970619IF  1C     ERRL00        IFEQ      '0'
032900970619     C                   MOVEL     *BLANKS       V1DLA1
033000970619IF  2C     V1CLA1        IFNE      *BLANKS
033100970619     C                   MOVEL     V1CLA1        WLIN
033200970619     C                   MOVEL     *BLANKS       WDLIN
033300970619     C                   EXSR      CHKLIN
033400970619     C                   MOVEL     WLIN          V1CLA1
033500970619     C                   MOVEL     WDLIN         V1DLA1
033600970619     C                   MOVEL     V1CLA1        LA1V1C
033700970619IF  3C     ERRL00        IFEQ      '1'
033800970619     C                   SETON                                        62
033900970619     C                   MOVEL     MSG(13)       DSMSMS
034000970619E   3C                   ENDIF
034100970619E   2C                   ENDIF
034200970619E   1C                   ENDIF
034300970619     C*--------------------
034400970619     C* LINEA PARTENZA AL
034500970619     C*--------------------
034600970619     C     *LIKE         DEFINE    V1CLA2        LA2V1C
034700970619     C                   MOVEL     *ZEROS        LA2V1C
034800970619IF  1C     ERRL00        IFEQ      '0'
034900970619     C                   MOVEL     *BLANKS       V1DLA2
035000970619IF  2C     V1CLA2        IFNE      *BLANKS
035100970619     C                   MOVEL     V1CLA2        WLIN
035200970619     C                   MOVEL     *BLANKS       WDLIN
035300970619     C                   EXSR      CHKLIN
035400970619     C                   MOVEL     WLIN          V1CLA2
035500970619     C                   MOVEL     WDLIN         V1DLA2
035600970619     C                   MOVEL     V1CLA2        LA2V1C
035700970619IF  3C     ERRL00        IFEQ      '1'
035800970619     C                   SETON                                        63
035900970619     C                   MOVEL     MSG(13)       DSMSMS
036000970619E   3C                   ENDIF
036100970619E   2C                   ENDIF
036200970619E   1C                   ENDIF
036300970619     C*--------------------
036400970619     C* LINEA PARTENZA DAL+AL
036500970619     C*--------------------
036600970619     C* SE DAL ANCHE AL E VICEVERSA
036700970619IF  1C     ERRL00        IFEQ      '0'
036800970619IF  2C     V1CLA1        IFNE      *BLANKS
036900970619     C     V1CLA2        ANDEQ     *BLANKS
037000970619     C     V1CLA2        ORNE      *BLANKS
037100970619     C     V1CLA1        ANDEQ     *BLANKS
037200970619     C                   SETON                                        6263
037300970619     C                   MOVEL     MSG(14)       DSMSMS
037400970619     C                   MOVEL     '1'           ERRL00
037500970619E   2C                   ENDIF
037600970619E   1C                   ENDIF
037700970619     C* DAL <= AL
037800970619IF  1C     ERRL00        IFEQ      '0'
037900970619IF  2C     V1CLA1        IFNE      *BLANKS
038000970619IF  3C     V1CLA1        IFGT      V1CLA2
038100970619     C                   SETON                                        6263
038200970619     C                   MOVEL     MSG(15)       DSMSMS
038300970619     C                   MOVEL     '1'           ERRL00
038400970619E   3C                   ENDIF
038500970619E   2C                   ENDIF
038600970619E   1C                   ENDIF
038700970910     C*
038800970910     C                   ENDSR
038900970910     C*------------------------------------------------------------------------*
039000970910     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
039100970910     C*------------------------------------------------------------------------*
039200970910     C     CHKLIN        BEGSR
039300970910     C*
039400970619     C     *LIKE         DEFINE    V1CLP1        WLIN
039500970619     C     *LIKE         DEFINE    V1DLP1        WDLIN
039600970910     C*
039700970910     C* RICHIESTO ELENCO
039800970910     C                   SETOFF                                           99
039900970910     C     '?'           SCAN      WLIN                                   99
040000970910IF  1C     *IN99         IFEQ      *ON
040100970910     C                   MOVEL     *BLANKS       §COD1             3
040200970910     C                   MOVEL     *BLANKS       §TIP              1
040300970910     C                   MOVEL     *BLANKS       §DES1            25
040400971203     C                   MOVEL     *BLANKS       §PRA              3
040500971203     C                   CALL      'TNSD24R'
040600970910     C                   PARM                    §COD1
040700970910     C                   PARM                    §TIP
040800970910     C                   PARM                    §DES1
040900971203     C                   PARM                    §PRA
041000970910     C                   MOVEL     §COD1         WLIN
041100970910     C                   MOVEL     §DES1         WDLIN
041200970910E   1C                   ENDIF
041300970910     C*
041400970910     C* SOLO CARATTERI NUMERICI
041500970910     C                   SETOFF                                       99
041600970910     C                   TESTN                   WLIN                 99
041700970910IF  1C     *IN99         IFEQ      *OFF                                         *NON E'NUMERO
041800970910     C                   MOVEL     '1'           ERRL00
041900970910X   1C                   ELSE                                                   *E'NUMERO DA TESTN
042000970910     C                   MOVE      WLIN          W001              1
042100970910IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
042200970910     C                   MOVEL     '1'           ERRL00
042300970910X   2C                   ELSE                                                   *NUMERO
042400970910     C*
042500970910     C* CONTROLLA ESISTENZA CODICE
042600970910     C                   MOVEL     WLIN          KORFIL
042700970910     C     KEYORG        CHAIN     AZORG01L                           99
042800970910IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
042900970910     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
043000970910     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
043100970910     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
043200970910X   3C                   ELSE
043300970910     C                   MOVEL     '1'           ERRL00
043400970910E   3C                   ENDIF
043500970910E   2C                   ENDIF
043600970910E   1C                   ENDIF
043700970910     C*
043800970910     C                   ENDSR
043900970910     C*------------------------------------------------------------------------*
044000970910     C* AZZVID - AZZERA I CAMPI DEL VIDEO
044100970910     C*------------------------------------------------------------------------*
044200970910     C     AZZVID        BEGSR
044300970910     C* CAMPI
044400970619     C                   MOVEL     'P'           V1CTOR
044500970619     C                   MOVEL     'P'           V1CPTA
044600970619     C                   MOVEL     *BLANKS       V1CLP1
044700970619     C                   MOVEL     *BLANKS       V1CLP2
044800970619     C                   MOVEL     *BLANKS       V1CLA1
044900970619     C                   MOVEL     *BLANKS       V1CLA2
045000970910     C* DECODIFICHE
045100970619     C                   MOVEL     *BLANKS       V1DLP1
045200970619     C                   MOVEL     *BLANKS       V1DLP2
045300970619     C                   MOVEL     *BLANKS       V1DLA1
045400970619     C                   MOVEL     *BLANKS       V1DLA2
045500970910     C*
045600970910     C                   ENDSR
045700970910     C*------------------------------------------------------------------------*
045800970910     C* *INZSR - OPERAZIONI INIZIALI
045900970910     C*------------------------------------------------------------------------*
046000970910     C     *INZSR        BEGSR
046100970910     C*---
046200970910     C* RICEVIMENTO PARAMETRI
046300970910     C*---
046400970910     C     *ENTRY        PLIST
046500970612     C                   PARM                    KPJBA
046600970612     C                   PARM                    DSSI00
046700970910     C*
046800970910     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
046900970910     C     XMSGPA        PLIST
047000970910     C                   PARM                    DSMSMS
047100970910     C                   PARM                    DSMSPG
047200970910     C                   PARM                    DSMSKE
047300970910     C                   PARM                    DSMSER
047400970910     C*
047500970910     C* IMPOSTA LA DS -TASTI PREMUTI-
047600970910     C                   CALL      'XKEY'
047700970910     C                   PARM                    DSKEY
047800970910     C*---
047900970910     C* VARIABILI NON RIFERITE AL DATA BASE
048000970910     C*---
048100970910     C     *LIKE         DEFINE    FUDFUN        FUNKEY
048200970910     C     *LIKE         DEFINE    D00OP0        DEPOP0
048300970910     C     *LIKE         DEFINE    D00OP1        DEPOP1
048400970910     C     *LIKE         DEFINE    D00OP0        WOP0
048500970910     C     *LIKE         DEFINE    D00OP1        WOP1
048600970910     C*---
048700970910     C* VARIABILI DI CONTROLLO
048800970910     C*---
048900970910     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
049000970910     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
049100970910     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
049200970910     C                   MOVEL     '0'           ERRL00            1            *ERRORE
049300970910     C*
049400970910     C* IMPOSTA LA DS -INVIO MESSAGGI-
049500970619     C                   MOVEL     'TISI25R'     DSMSPG
049600970619     C                   MOVEL     'TISI25R'     MSGQUE
049700970910     C                   MOVEL     *BLANKS       MSGKEY
049800970910     C                   MOVEL     *BLANKS       DSMSMS
049900970910     C*---
050000970910     C* VARIABILI RIFERITE AL DATA BASE
050100970910     C*---
050200970910     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
050300970910     C*---
050400970910     C* CHIAVI DI LETTURA
050500970910     C*---
050600970910     C* LETTURA AZORG01L - COMPLETA
050700970910     C     KEYORG        KLIST
050800970910     C                   KFLD                    KORFIL                         *FILIALE
050900970910     C*
051000970910     C* DEPOSITA LE OPZIONI DI ENTRATA
051100970910     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
051200970910     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
051300970619     C*
051400970619     C* CALCOLA LA DATA CORRENTE
051500970619     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
051600970619     C                   MOVE      WN14          CORDAT            8 0          *DATA (8) IN G/M/AA
051700970910     C*
051800970910     C* GESTIONE TASTI FUNZIONALI A VIDEO
051900970910     C                   Z-ADD     *ZEROS        FUNGRU            5 0
052000970910     C                   EXSR      FUNGES
052100970910     C*
052200970910     C* IMPOSTO LA VIDEATA
052300970910     C                   EXSR      AZZVID
052400970910     C*
052500970910     C                   ENDSR
052600970910     C*------------------------------------------------------------------------*
052700970910     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
052800970910     C*------------------------------------------------------------------------*
052900970910     C     FUNGES        BEGSR
053000970910     C*
053100970910     C* PULISCO CAMPI DI LAVORO
053200970910     C                   Z-ADD     0             W                 5 0
053300970910     C*
053400970910     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
053500970910     C* E CARICO TABELLA FUNZIONI ATTIVE
053600970910     C     1             DO        24            I
053700970910     C                   MOVEL     FUD(I)        FUDRIG
053800970910     C*
053900970910     C* PULIZIA FUNZIONI PRECEDENTI
054000970910     C                   MOVEL     *BLANKS       FUV(I)
054100970910     C*
054200970910     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
054300970910     C* (SALTA F24)
054400970910     C     FUDATT        IFEQ      'S'
054500970910     C     FUDFUN        ANDNE     '24'
054600970910     C                   ADD       1             W
054700970910     C                   MOVEL     FUDDES        FUV(W)
054800970910     C                   END
054900970910     C                   END                                                    DO WHILE
055000970910     C*
055100970910     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
055200970910     C* ALTRIMENTI DISATTIVO F24
055300970910     C     1             DO        24            I
055400970910     C                   MOVEL     FUD(I)        FUDRIG
055500970910     C*
055600970910     C     FUDFUN        IFEQ      '24'
055700970910     C     W             IFGT      6
055800970910     C                   MOVEL     'S'           FUDATT
055900970910     C                   ELSE
056000970910     C                   MOVEL     *BLANKS       FUDATT
056100970910     C                   END                                                    W GT 8
056200970910     C                   MOVEL     FUDRIG        FUD(I)
056300970910     C                   END
056400970910     C                   END                                                    DO WHILE
056500970910     C*
056600970910     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
056700970910     C     W             DIV       6             I
056800970910     C                   MVR                     Z
056900970910     C*
057000970910     C     Z             IFNE      0
057100970910     C     I             ADD       1             Z
057200970910     C                   ELSE
057300970910     C                   Z-ADD     I             Z                 5 0
057400970910     C                   END
057500970910     C*
057600970910     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
057700970910     C                   ADD       1             FUNGRU
057800970910     C     FUNGRU        IFGT      Z
057900970910     C     FUNGRU        ORLT      1
058000970910     C                   Z-ADD     1             FUNGRU
058100970910     C                   END
058200970910     C*
058300970910     C* PRIMO GRUPPO DI 6 FUNZIONI
058400970910     C     FUNGRU        IFEQ      1
058500970910     C                   MOVEL     FUV(1)        FUN001
058600970910     C                   MOVEL     FUV(2)        FUN002
058700970910     C                   MOVEL     FUV(3)        FUN003
058800970910     C                   MOVEL     FUV(4)        FUN004
058900970910     C                   MOVEL     FUV(5)        FUN005
059000970910     C                   MOVEL     FUV(6)        FUN006
059100970910     C                   END
059200970910     C*
059300970910     C* SECONDO GRUPPO DI 6 FUNZIONI
059400970910     C     FUNGRU        IFEQ      2
059500970910     C                   MOVEL     FUV(7)        FUN001
059600970910     C                   MOVEL     FUV(8)        FUN002
059700970910     C                   MOVEL     FUV(9)        FUN003
059800970910     C                   MOVEL     FUV(10)       FUN004
059900970910     C                   MOVEL     FUV(11)       FUN005
060000970910     C                   MOVEL     FUV(12)       FUN006
060100970910     C                   END
060200970910     C*
060300970910     C* TERZO GRUPPO DI 6 FUNZIONI
060400970910     C     FUNGRU        IFEQ      3
060500970910     C                   MOVEL     FUV(13)       FUN001
060600970910     C                   MOVEL     FUV(14)       FUN002
060700970910     C                   MOVEL     FUV(15)       FUN003
060800970910     C                   MOVEL     FUV(16)       FUN004
060900970910     C                   MOVEL     FUV(17)       FUN005
061000970910     C                   MOVEL     FUV(18)       FUN006
061100970910     C                   END
061200970910     C*
061300970910     C* QUARTO GRUPPO DI 6 FUNZIONI
061400970910     C     FUNGRU        IFEQ      4
061500970910     C                   MOVEL     FUV(19)       FUN001
061600970910     C                   MOVEL     FUV(20)       FUN002
061700970910     C                   MOVEL     FUV(21)       FUN003
061800970910     C                   MOVEL     FUV(22)       FUN004
061900970910     C                   MOVEL     *BLANKS       FUN005
062000970910     C                   MOVEL     *BLANKS       FUN006
062100970910     C                   END
062200970910     C*
062300970910     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
062400970910     C                   MOVEL     *BLANKS       FUN007
062500970910     C                   MOVEL     *BLANKS       FUN008
062600970910     C*
062700970910     C     1             DO        24            I
062800970910     C                   MOVEL     FUD(I)        FUDRIG
062900970910     C     FUDFUN        IFEQ      '24'
063000970910     C     FUDATT        ANDEQ     'S'
063100970910     C                   MOVEL     FUDDES        FUN008
063200970910     C                   END
063300970910     C                   END                                                    DO
063400970910     C*
063500970910     C                   ENDSR
063600970910     C*------------------------------------------------------------------------*
063700970910     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
063800970910     C*------------------------------------------------------------------------*
063900970910     C     FUNCON        BEGSR
064000970910     C*
064100970910     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
064200970910     C                   EXSR      FUNVAR
064300970910     C*
064400970910     C* PULISCO CAMPI DI LAVORO
064500970910     C                   Z-ADD     0             I                 5 0
064600970910     C                   Z-ADD     0             W                 5 0
064700970910     C*
064800970910     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
064900970910     C     FUNKEY        IFNE      *BLANKS
065000970910     C     1             DO        24            I
065100970910     C                   MOVEL     FUD(I)        FUDRIG
065200970910     C*
065300970910     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
065400970910     C     FUNKEY        IFEQ      FUDFUN
065500970910     C     FUDATT        ANDEQ     'S'
065600970910     C                   Z-ADD     I             W
065700970910     C                   END
065800970910     C                   END                                                    DO WHILE
065900970910     C*
066000970910     C* FUNZIONE NON ATTIVA (ERRORE)
066100970910     C     W             IFEQ      0
066200970910     C*
066300970910     C* EMISSIONE MESSAGGIO DI ERRORE
066400970910     C                   MOVEL     MSG(1)        DSMSMS
066500970910     C*
066600970910     C                   END                                                    W=0
066700970910     C                   END                                                    FUNKEY NE *BLANKS
066800970910     C*
066900970910     C* FUNZIONE ATTIVA (PROCEDI ...)
067000970910     C     W             IFNE      0
067100970910     C     FUNKEY        OREQ      *BLANKS
067200970910     C     FUNKEY        IFEQ      *BLANKS
067300970910     C                   EXSR      ENTGES
067400970910     C                   END
067500970910     C     FUNKEY        IFEQ      '03'
067600970910     C                   EXSR      F03GES
067700970910     C                   END
067800970910     C     FUNKEY        IFEQ      '06'
067900970910     C                   EXSR      F06GES
068000970910     C                   END
068100970910     C     FUNKEY        IFEQ      '12'
068200970910     C                   EXSR      F12GES
068300970910     C                   END
068400970910     C     FUNKEY        IFEQ      '24'
068500970910     C                   EXSR      F24GES
068600970910     C                   END
068700970910     C                   END
068800970910     C*
068900970910     C                   ENDSR
069000970910     C*------------------------------------------------------------------------*
069100970910     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
069200970910     C*------------------------------------------------------------------------*
069300970910     C     FUNVAR        BEGSR
069400970910     C*
069500970910     C* INIZIALIZZA VARIABILE -FUNKEY-
069600970910     C*
069700970910     C* FUNKEY=*BLANKS SIGNIFICA ENTER
069800970910     C     KEY           IFEQ      ENTER
069900970910     C                   MOVEL     *BLANKS       FUNKEY
070000970910     C                   END
070100970910     C     KEY           IFEQ      F1
070200970910     C                   MOVEL     '01'          FUNKEY
070300970910     C                   END
070400970910     C     KEY           IFEQ      F2
070500970910     C                   MOVEL     '02'          FUNKEY
070600970910     C                   END
070700970910     C     KEY           IFEQ      F3
070800970910     C                   MOVEL     '03'          FUNKEY
070900970910     C                   END
071000970910     C     KEY           IFEQ      F4
071100970910     C                   MOVEL     '04'          FUNKEY
071200970910     C                   END
071300970910     C     KEY           IFEQ      F5
071400970910     C                   MOVEL     '05'          FUNKEY
071500970910     C                   END
071600970910     C     KEY           IFEQ      F6
071700970910     C                   MOVEL     '06'          FUNKEY
071800970910     C                   END
071900970910     C     KEY           IFEQ      F7
072000970910     C                   MOVEL     '07'          FUNKEY
072100970910     C                   END
072200970910     C     KEY           IFEQ      F8
072300970910     C                   MOVEL     '08'          FUNKEY
072400970910     C                   END
072500970910     C     KEY           IFEQ      F9
072600970910     C                   MOVEL     '09'          FUNKEY
072700970910     C                   END
072800970910     C     KEY           IFEQ      F10
072900970910     C                   MOVEL     '10'          FUNKEY
073000970910     C                   END
073100970910     C     KEY           IFEQ      F11
073200970910     C                   MOVEL     '11'          FUNKEY
073300970910     C                   END
073400970910     C     KEY           IFEQ      F12
073500970910     C                   MOVEL     '12'          FUNKEY
073600970910     C                   END
073700970910     C     KEY           IFEQ      F13
073800970910     C                   MOVEL     '13'          FUNKEY
073900970910     C                   END
074000970910     C     KEY           IFEQ      F14
074100970910     C                   MOVEL     '14'          FUNKEY
074200970910     C                   END
074300970910     C     KEY           IFEQ      F15
074400970910     C                   MOVEL     '15'          FUNKEY
074500970910     C                   END
074600970910     C     KEY           IFEQ      F16
074700970910     C                   MOVEL     '16'          FUNKEY
074800970910     C                   END
074900970910     C     KEY           IFEQ      F17
075000970910     C                   MOVEL     '17'          FUNKEY
075100970910     C                   END
075200970910     C     KEY           IFEQ      F18
075300970910     C                   MOVEL     '18'          FUNKEY
075400970910     C                   END
075500970910     C     KEY           IFEQ      F19
075600970910     C                   MOVEL     '19'          FUNKEY
075700970910     C                   END
075800970910     C     KEY           IFEQ      F20
075900970910     C                   MOVEL     '20'          FUNKEY
076000970910     C                   END
076100970910     C     KEY           IFEQ      F21
076200970910     C                   MOVEL     '21'          FUNKEY
076300970910     C                   END
076400970910     C     KEY           IFEQ      F22
076500970910     C                   MOVEL     '22'          FUNKEY
076600970910     C                   END
076700970910     C     KEY           IFEQ      F23
076800970910     C                   MOVEL     '23'          FUNKEY
076900970910     C                   END
077000970910     C     KEY           IFEQ      F24
077100970910     C                   MOVEL     '24'          FUNKEY
077200970910     C                   END
077300970910     C     KEY           IFEQ      ROLLUP
077400970910     C                   MOVEL     'RU'          FUNKEY
077500970910     C                   END
077600970910     C     KEY           IFEQ      HELP
077700970910     C                   MOVEL     'HP'          FUNKEY
077800970910     C                   END
077900970910     C                   ENDSR
078000970910     C*------------------------------------------------------------------------*
078100970910     C* NOTE COSTRUZIONE SCHIERE
078200970910     C*------------------------------------------------------------------------*
078300970910     C*
078400970910     C* -MSG-MESSAGGI
078500970910     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
078600970910     C* (NON TOCCARE MSG,1 E MSG,2)
078700970910     C*
078800970910     C* -CMD-COMANDI
078900970910     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
079000970910     C*
079100970910     C* -OPD-OPZIONI
079200970910     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
079300970910     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
079400970910     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
079500970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
079600970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
079700970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
079800970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
079900970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
080000970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
080100970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
080200970910     C*
080300970910     C* -FUD-FUNZIONI
080400970910     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
080500970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
080600970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
080700970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
080800970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
080900970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
081000970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
081100970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
081200970910     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
081300970910     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
081400970910     O*------------------------------------------------------------------------*
081500970910** MSG - MESSAGGI
081600970619TISI25R- Opzione non gestita                                                   1
081700970619TISI25R- Tasto funzionale non gestito                                          2
081800970619TISI25R- 3
081900970619TISI25R- 4
082000970619TISI25R- 5
082100970619TISI25R- 6
082200970619TISI25R- 7
082300970619TISI25R- 8
082400970619TISI25R- 9
082500970619TISI25R- 10
082600970619TISI25R- Immettere uno dei valori possibili                                   11
082700970619TISI25R- Data errata                                                          12
082800970619TISI25R- Linea errata                                                         13
082900970619TISI25R- Se richiesto il Dal immettere anche l'AL e viceversa                 14
083000970619TISI25R- Il Dal deve essere inferiore o uguale all'AL                         15
083100970619TISI25R- 16
083200970619TISI25R- 17
083300970619TISI25R- 18
083400970619TISI25R- 19
083500970619TISI25R- Stampa lanciata ...                                                  20
083600970610** CMD - COMANDI CL
083700970610WRKSPLF
083800970610
083900970610
084000970610
084100970610
084200970910** FUD - FUNZIONI
084300970910                   -        (S/N)=N                        funz.01¬
084400970910                   -        (S/N)=N                        funz.02¬
084500970910F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
084600970910                   -        (S/N)=N                        funz.04¬
084700970910                   -        (S/N)=N                        funz.05¬
084800970610F06=Conferma       -        (S/N)=S                        funz.06¬
084900970910                   -        (S/N)=N                        funz.07¬
085000970910                   -        (S/N)=N                        funz.08¬
085100970910                   -        (S/N)=N                        funz.09¬
085200970910                   -        (S/N)=N                        funz.10¬
085300970910                   -        (S/N)=N                        funz.11¬
085400970910F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
085500970910                   -        (S/N)=N                        funz.13¬
085600970910                   -        (S/N)=N                        funz.14¬
085700970910                   -        (S/N)=N                        funz.15¬
085800970910                   -        (S/N)=N                        funz.16¬
085900970910                   -        (S/N)=N                        funz.17¬
086000970910                   -        (S/N)=N                        funz.18¬
086100970910                   -        (S/N)=N                        funz.19¬
086200970910                   -        (S/N)=N                        funz.20¬
086300970910                   -        (S/N)=N                        funz.21¬
086400970910                   -        (S/N)=N                        funz.22¬
086500970910                   -        (S/N)=N                        funz.23¬
086600970910F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
