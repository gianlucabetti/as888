000100970910     H*------------------------------------------------------------------------*
000200970610     H* CAPPARIO - STAMPA CAPPARIO - RICHIESTA PARAMETRI
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800000221     FTISI08D   CF   E             WORKSTN INFDS(DEVDS1)
000900000221     F                                     SFILE(SI08DMS:MSGULT)
001000970910     FTABEL00F  IF   E           K DISK
001100970910     FAZORG01L  IF   E           K DISK
001200970613     FAZCPT01L  IF   E           K DISK
001300970910     D*------------------------------------------------------------------------*
001400970910     D* SCHIERE
001500970910     D*------------------------------------------------------------------------*
001600970910     D*------------
001700970910     D* MESSAGGI
001800970910     D*------------
001900970910     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000970910     D*------------
002100970910     D* COMANDI
002200970910     D*------------
002300970910     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002400970910     D*------------
002500970910     D* DESCRIZIONI TASTI FUNZIONALI
002600970910     D*------------
002700970910     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002800970910     D*------------
002900970910     D* FUNZIONI DA CARICARE A VIDEO
003000970910     D*------------
003100970910     D FUV             S             19    DIM(24)
003200970910     D*------------------------------------------------------------------------*
003300970910     D* INPUT
003400970910     D*------------------------------------------------------------------------*
003500970910     D*------------
003600970910     D* RIDEFINIZIONE FUNZIONI VIDEO
003700970910     D*------------
003800970910     D FUDDS           DS
003900970910     D  FUDDE1                 1      1
004000970910     D  FUDFUN                 2      3
004100970910     D  FUDDE2                 4     19
004200970910     D  FUDFI0                20     34
004300970910     D  FUDATT                35     35
004400970910     D  FUDFI1                36     80
004500970910     D  FUDDES                 1     19
004600970910     D  FUDRIG                 1     80
004700970910     D*------------
004800970910     D* DS GESTIONE TASTI FUNZIONALI
004900970910     D*------------
005000970910     D DSKEY         E DS
005100970910     D DEVDS1          DS
005200970910     D  KEY                  369    369
005300970910     D*------------
005400970910     D* DS "XMSG" - GESTIONE MESSAGGI
005500970910     D*------------
005600970910     D DSMSG         E DS
005700970611     D*------------------
005800970611     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900970611     D*------------------
006000970611     D WLBDA8          DS                  INZ
006100970611     D  G08DAT                 1      8  0
006200970611     D  G08INV                 9     16  0
006300970611     D  G08ERR                17     17
006400970611     D  G08TGI                18     22  0
006500970910     D*------------------
006600970910     D* DS NAZIONI
006700970910     D*------------------
006800970910     D DS15          E DS
006900970910     D*------------------
007000970910     D* DS PROVINCIE
007100970910     D*------------------
007200970910     D DSPR          E DS
007300970612     D*------------------
007400970612     D* DS TIPI INOLTRO
007500970612     D*------------------
007600970612     D DS9A          E DS
007700970910     D*------------
007800970910     D* DS DI PROCEDURA
007900970910     D*------------
008000970910     D DSSI00        E DS                  EXTNAME(TISI00DS)
008100970910     D*------------
008200970910     D* DS PARAMETRI DI LANCIO
008300970910     D*------------
008400970610     D PARAM           DS                  INZ
008500000221     D  PARDAT                 1      8  0
008600000221     D  PARVER                 9     13  0
008700000221     D  PARPRI                14     14
008800000221     D  PARLDI                15     15
008900000221     D  PARISO                16     16
009000000221     D  PARCAD                17     25
009100000221     D  PARCAA                26     34
009200000221     D  PARPRD                35     36
009300000221     D  PARPRA                37     38
009400000221     D  PARLAD                39     41  0
009500000221     D  PARLAA                42     44  0
009600000223     D*-------------
009700000223     D* ARCHITETTURA
009800000223     D*------------
009900000223     D KPJBA         E DS
010000970910     C*------------------------------------------------------------------------*
010100970910     C* MAIN LINES
010200970910     C*------------------------------------------------------------------------*
010300970910     C*
010400970910     C* NON CI SONO ERRORI IN ENTRATA
010500970910IF  1C     ERRL00        IFEQ      '0'
010600970910     C*
010700970910     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010800970910DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010900970910     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011000970910     C     FINL00        OREQ      '1'                                          *FINE PGM
011100970910     C*
011200970910     C* EMETTE FORMATO VIDEO
011300970910     C                   EXSR      BUTFMR
011400970910     C*
011500970910     C* GESTIONE TASTI FUNZIONALI
011600970910     C                   EXSR      FUNCON
011700970910E   2C                   ENDDO
011800970910     C*
011900970910     C* CI SONO ERRORI IN ENTRATA
012000970910X   1C                   ELSE
012100970910     C                   MOVEL     '1'           D00ERR
012200970910     C                   MOVEL     DSMSMS        D00MSG
012300970910E   1C                   ENDIF
012400970910     C* FINE PGM
012500970910     C                   SETON                                        LR
012600970910     C*------------------------------------------------------------------------*
012700970910     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012800970910     C*------------------------------------------------------------------------*
012900970910     C     BUTFMR        BEGSR
013000970910     C*
013100970910     C* EMETTE LA TESTATA
013200000221     C                   WRITE     SI08D01
013300970910     C*
013400970910     C* SPEDISCE EVENTUALE MESSAGGIO
013500970910     C                   EXSR      SNDMSG
013600970910     C*
013700970910     C* EMETTE LE FUNZIONI
013800000221     C                   WRITE     SI08D02
013900970910     C*
014000970910     C* EMETTE IL VIDEO
014100000221     C                   EXFMT     SI08D03
014200970910     C*
014300970910     C* REIMPOSTA FLAG DI ERRORE
014400970910     C                   MOVEL     '0'           ERRL00
014500970910     C*
014600000221 VED C* REIMPOSTA INDICATORI DI ERRORE
014700000221  "  C                   SETOFF                                         5859
014800970910     C                   SETOFF                                       606162
014900970910     C                   SETOFF                                       636465
015000970910     C                   SETOFF                                       666768
015100970910     C                   SETOFF                                       697071
015200970910     C                   SETOFF                                       727374
015300000221  "  C                   SETOFF                                       7586
015400000221 VED C                   SETOFF                                       878889
015500970910     C*
015600970910     C                   ENDSR
015700970910     C*------------------------------------------------------------------------*
015800970910     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015900970910     C*------------------------------------------------------------------------*
016000970910     C     SNDMSG        BEGSR
016100970910     C*
016200970910     C* PULISCE IL SFILE MESSAGGI
016300970910     C                   MOVEL     '1'           *IN40
016400000221     C                   WRITE     SI08DMC
016500970910     C                   MOVEL     '0'           *IN40
016600970910     C*
016700970910     C* VISUALIZZA IL MESSAGGIO
016800970910     C                   Z-ADD     *ZEROS        MSGULT            4 0
016900970910     C                   CALL      'XMSG'        XMSGPA
017000970910IF  1C     DSMSER        IFEQ      *ZEROS
017100970910     C                   MOVEL     DSMSKE        MSGKEY
017200970910E   1C                   ENDIF
017300970910     C                   Z-ADD     1             MSGULT
017400000221     C                   WRITE     SI08DMS
017500000221     C                   WRITE     SI08DMC
017600970910     C*
017700970910     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017800970910     C                   MOVEL     *BLANKS       DSMSMS
017900970910     C*
018000970910     C                   ENDSR
018100970910     C*------------------------------------------------------------------------*
018200970910     C* ENTGES - GESTIONE ENTER -> INVIO
018300970910     C*------------------------------------------------------------------------*
018400970910     C     ENTGES        BEGSR
018500970910     C*
018600970910     C* CONTROLLA I CAMPI IMMESSI
018700970910     C                   EXSR      DECODI
018800970910     C*
018900970910     C                   ENDSR
019000970910     C*------------------------------------------------------------------------*
019100970910     C* F03GES - GESTIONE F03 -> FINE
019200970910     C*------------------------------------------------------------------------*
019300970910     C     F03GES        BEGSR
019400970910     C*
019500970910     C* IMPOSTA "PREMUTO F03"
019600970910     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019700970910     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
019800970910     C*
019900970910     C                   ENDSR
020000970910     C*------------------------------------------------------------------------*
020100970623     C* F06GES - GESTIONE F06 -> CONFERMA
020200970910     C*------------------------------------------------------------------------*
020300970723     C     F06GES        BEGSR
020400970723     C*
020500970723     C* CONTROLLA I CAMPI IMMESSI
020600970723     C                   EXSR      DECODI
020700970910     C*
020800000221     C* LANCIA IL PROGRAMMA DI STAMPA
020900000221IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021000000221     C                   Z-ADD     V1CDRI        PARDAT
021100000221     C                   Z-ADD     D00VER        PARVER
021200000221     C                   MOVEL     V1CPRI        PARPRI
021300000221     C                   MOVEL     V1CLDI        PARLDI
021400000221     C                   MOVEL     V1CISO        PARISO
021500000221     C                   MOVEL     V1CCA1        PARCAD
021600000221     C                   MOVEL     V1CCA2        PARCAA
021700000221     C                   MOVEL     V1CPR1        PARPRD
021800000221     C                   MOVEL     V1CPR2        PARPRA
021900000221     C                   MOVE      V1CLN1        PARLAD
022000000221     C                   MOVE      V1CLN2        PARLAA
022100000223     C                   MOVEL     PARAM         KPJBU
022200000223     C                   MOVEL     'SI09'        KCOAZ
022300010119     C                   CALL      'BCH10'
022400010119 MOM C*                  CALL      'TISI09R'
022500000223     C                   PARM                    KPJBA
022600000223     C*                  MOVEL     '1'           FINL00
022700970723E   1C                   ENDIF
022800000221     C*
022900000221     C                   ENDSR
023000970910     C*------------------------------------------------------------------------*
023100970910     C* F12GES - GESTIONE F12 -> RITORNO
023200970910     C*------------------------------------------------------------------------*
023300970910     C     F12GES        BEGSR
023400970910     C*
023500970910     C* IMPOSTA "PREMUTO F12"
023600970910     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023700970910     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
023800970910     C*
023900970910     C                   ENDSR
024000970910     C*------------------------------------------------------------------------*
024100970910     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
024200970910     C*------------------------------------------------------------------------*
024300970910     C     F22GES        BEGSR
024400000221     C*
024500970910     C                   Z-ADD     80            LENGH            15 5
024600970910     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
024700970910     C                   PARM                    CMD(1)
024800970910     C                   PARM                    LENGH
024900970910     C*
025000970910     C                   ENDSR
025100970910     C*------------------------------------------------------------------------*
025200970910     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025300970910     C*------------------------------------------------------------------------*
025400970910     C     F24GES        BEGSR
025500970910     C*
025600970910     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025700970910     C*
025800970910     C                   ENDSR
025900970910     C*------------------------------------------------------------------------*
026000000221     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026100000221     C*------------------------------------------------------------------------*
026200970910     C     DECODI        BEGSR
026300970910     C*--------------------
026400970910     C* CAP DAL+AL
026500970910     C*--------------------
026600970910     C* SE DAL ANCHE AL E VICEVERSA
026700970910IF  1C     ERRL00        IFEQ      '0'
026800970910IF  2C     V1CCA1        IFNE      *BLANKS
026900970910     C     V1CCA2        ANDEQ     *BLANKS
027000970910     C     V1CCA2        ORNE      *BLANKS
027100970910     C     V1CCA1        ANDEQ     *BLANKS
027200970910     C                   SETON                                        7071
027300970910     C                   MOVEL     MSG(14)       DSMSMS
027400970910     C                   MOVEL     '1'           ERRL00
027500970910E   2C                   ENDIF
027600970910E   1C                   ENDIF
027700970910     C* DAL <= AL
027800970910IF  1C     ERRL00        IFEQ      '0'
027900970910IF  2C     V1CCA1        IFNE      *BLANKS
028000970910IF  3C     V1CCA1        IFGT      V1CCA2
028100970910     C                   SETON                                        7071
028200970910     C                   MOVEL     MSG(15)       DSMSMS
028300970910     C                   MOVEL     '1'           ERRL00
028400970910E   3C                   ENDIF
028500970910E   2C                   ENDIF
028600970910E   1C                   ENDIF
028700970910     C*--------------------
028800970910     C* PROVINCIA DAL
028900970910     C*--------------------
029000970910IF  1C     ERRL00        IFEQ      '0'
029100970910     C                   MOVEL     *BLANKS       V1DPR1
029200970910IF  2C     V1CPR1        IFNE      *BLANKS
029300970910     C                   MOVEL     V1CPR1        WPRV
029400970910     C                   MOVEL     *BLANKS       WDPRV
029500970910     C                   EXSR      CHKPRV
029600970910     C                   MOVEL     WPRV          V1CPR1
029700970910     C                   MOVEL     WDPRV         V1DPR1
029800970910IF  3C     ERRL00        IFEQ      '1'
029900970910     C                   SETON                                        72
030000970910     C                   MOVEL     MSG(16)       DSMSMS
030100970910E   3C                   ENDIF
030200970910E   2C                   ENDIF
030300970910E   1C                   ENDIF
030400970910     C*--------------------
030500970910     C* PROVINCIA AL
030600970910     C*--------------------
030700970910IF  1C     ERRL00        IFEQ      '0'
030800970910     C                   MOVEL     *BLANKS       V1DPR2
030900970910IF  2C     V1CPR2        IFNE      *BLANKS
031000970910     C                   MOVEL     V1CPR2        WPRV
031100970910     C                   MOVEL     *BLANKS       WDPRV
031200970910     C                   EXSR      CHKPRV
031300970910     C                   MOVEL     WPRV          V1CPR2
031400970910     C                   MOVEL     WDPRV         V1DPR2
031500970910IF  3C     ERRL00        IFEQ      '1'
031600970910     C                   SETON                                        73
031700970910     C                   MOVEL     MSG(16)       DSMSMS
031800970910E   3C                   ENDIF
031900970910E   2C                   ENDIF
032000970910E   1C                   ENDIF
032100970910     C*--------------------
032200970910     C* PROVINCIA DAL+AL
032300970910     C*--------------------
032400970910     C* SE DAL ANCHE AL E VICEVERSA
032500970910IF  1C     ERRL00        IFEQ      '0'
032600970910IF  2C     V1CPR1        IFNE      *BLANKS
032700970910     C     V1CPR2        ANDEQ     *BLANKS
032800970910     C     V1CPR2        ORNE      *BLANKS
032900970910     C     V1CPR1        ANDEQ     *BLANKS
033000970910     C                   SETON                                        7273
033100970910     C                   MOVEL     MSG(14)       DSMSMS
033200970910     C                   MOVEL     '1'           ERRL00
033300970910E   2C                   ENDIF
033400970910E   1C                   ENDIF
033500970910     C* DAL <= AL
033600970910IF  1C     ERRL00        IFEQ      '0'
033700970910IF  2C     V1CPR1        IFNE      *BLANKS
033800970910IF  3C     V1CPR1        IFGT      V1CPR2
033900970910     C                   SETON                                        7273
034000970910     C                   MOVEL     MSG(15)       DSMSMS
034100970910     C                   MOVEL     '1'           ERRL00
034200970910E   3C                   ENDIF
034300970910E   2C                   ENDIF
034400970910E   1C                   ENDIF
034500970910     C*--------------------
034600970910     C* LINEA ARRIVO DAL
034700970910     C*--------------------
034800970910IF  1C     ERRL00        IFEQ      '0'
034900970910     C                   MOVEL     *BLANKS       V1DLN1
035000970910IF  2C     V1CLN1        IFNE      *BLANKS
035100970910     C                   MOVEL     V1CLN1        WLIN
035200970910     C                   MOVEL     *BLANKS       WDLIN
035300970910     C                   EXSR      CHKLIN
035400970910     C                   MOVEL     WLIN          V1CLN1
035500970910     C                   MOVEL     WDLIN         V1DLN1
035600970910IF  3C     ERRL00        IFEQ      '1'
035700970910     C                   SETON                                        74
035800970910     C                   MOVEL     MSG(17)       DSMSMS
035900970910E   3C                   ENDIF
036000970910E   2C                   ENDIF
036100970910E   1C                   ENDIF
036200970910     C*--------------------
036300970910     C* LINEA ARRIVO AL
036400970910     C*--------------------
036500970910IF  1C     ERRL00        IFEQ      '0'
036600970910     C                   MOVEL     *BLANKS       V1DLN2
036700970910IF  2C     V1CLN2        IFNE      *BLANKS
036800970910     C                   MOVEL     V1CLN2        WLIN
036900970910     C                   MOVEL     *BLANKS       WDLIN
037000970910     C                   EXSR      CHKLIN
037100970910     C                   MOVEL     WLIN          V1CLN2
037200970910     C                   MOVEL     WDLIN         V1DLN2
037300970910IF  3C     ERRL00        IFEQ      '1'
037400970910     C                   SETON                                        75
037500970910     C                   MOVEL     MSG(17)       DSMSMS
037600970910E   3C                   ENDIF
037700970910E   2C                   ENDIF
037800970910E   1C                   ENDIF
037900970910     C*--------------------
038000970910     C* LINEA ARRIVO DAL+AL
038100970910     C*--------------------
038200970910     C* SE DAL ANCHE AL E VICEVERSA
038300970910IF  1C     ERRL00        IFEQ      '0'
038400970910IF  2C     V1CLN1        IFNE      *BLANKS
038500970910     C     V1CLN2        ANDEQ     *BLANKS
038600970910     C     V1CLN2        ORNE      *BLANKS
038700970910     C     V1CLN1        ANDEQ     *BLANKS
038800970910     C                   SETON                                        7475
038900970910     C                   MOVEL     MSG(14)       DSMSMS
039000970910     C                   MOVEL     '1'           ERRL00
039100970910E   2C                   ENDIF
039200970910E   1C                   ENDIF
039300970910     C* DAL <= AL
039400970910IF  1C     ERRL00        IFEQ      '0'
039500970910IF  2C     V1CLN1        IFNE      *BLANKS
039600970910IF  3C     V1CLN1        IFGT      V1CLN2
039700970910     C                   SETON                                        7475
039800970910     C                   MOVEL     MSG(15)       DSMSMS
039900970910     C                   MOVEL     '1'           ERRL00
040000970910E   3C                   ENDIF
040100970910E   2C                   ENDIF
040200970910E   1C                   ENDIF
040300000221     C*
040400000221     C                   ENDSR
040500970910     C*------------------------------------------------------------------------*
040600970910     C* CHKPRV - CONTROLLA LA PROVINCIA
040700970910     C*------------------------------------------------------------------------*
040800970910     C     CHKPRV        BEGSR
040900970910     C*
041000970910     C     *LIKE         DEFINE    V1CPR1        WPRV
041100970910     C     *LIKE         DEFINE    V1DPR1        WDPRV
041200970910     C*
041300970910     C* RICHIESTO ELENCO
041400970910     C                   SETOFF                                           99
041500970910     C     '?'           SCAN      WPRV                                   99
041600970910IF  1C     *IN99         IFEQ      *ON
041700020906     C                   Z-ADD     1             §KUT              1 0
041800970910     C                   MOVEL     'PR'          §COD              2
041900970910     C                   MOVEL     *BLANKS       §KEY              8
042000970910     C                   MOVEL     *BLANKS       §DES             30
042100970910     C                   CALL      'X§TABER'
042200970910     C                   PARM                    §KUT
042300970910     C                   PARM                    §COD
042400970910     C                   PARM                    §KEY
042500970910     C                   PARM                    §DES
042600970910     C                   MOVEL     §KEY          WPRV
042700970910     C                   MOVEL     §DES          WDPRV
042800970910E   1C                   ENDIF
042900970910     C*
043000970910     C* CONTROLLA ESISTENZA CODICE
043100020906     C                   MOVEL     1             KTBKUT
043200970910     C                   MOVEL     'PR'          KTBCOD
043300970910     C                   MOVEL(P)  WPRV          KTBKEY
043400970910     C     KEYTAB        CHAIN     TABEL00F                           99
043500970910IF  1C     *IN99         IFEQ      *OFF
043600970910     C     TBLFLG        ANDEQ     *BLANKS
043700970910     C                   MOVEL     TBLUNI        DSPR
043800970910     C                   MOVEL     §PRDES        WDPRV
043900970910X   1C                   ELSE
044000970910     C                   MOVEL     '1'           ERRL00
044100970910E   1C                   ENDIF
044200970910     C*
044300970910     C                   ENDSR
044400970910     C*------------------------------------------------------------------------*
044500970910     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
044600970910     C*------------------------------------------------------------------------*
044700970910     C     CHKLIN        BEGSR
044800970910     C*
044900970610     C     *LIKE         DEFINE    V1CLN1        WLIN
045000970610     C     *LIKE         DEFINE    V1DLN1        WDLIN
045100970910     C*
045200970910     C* RICHIESTO ELENCO
045300970910     C                   SETOFF                                           99
045400970910     C     '?'           SCAN      WLIN                                   99
045500970910IF  1C     *IN99         IFEQ      *ON
045600970910     C                   MOVEL     *BLANKS       §COD1             3
045700970910     C                   MOVEL     *BLANKS       §TIP              1
045800970910     C                   MOVEL     *BLANKS       §DES1            25
045900971203     C                   MOVEL     *BLANKS       §PRA              3
046000971203     C                   CALL      'TNSD24R'
046100970910     C                   PARM                    §COD1
046200970910     C                   PARM                    §TIP
046300970910     C                   PARM                    §DES1
046400971203     C                   PARM                    §PRA
046500970910     C                   MOVEL     §COD1         WLIN
046600970910     C                   MOVEL     §DES1         WDLIN
046700970910E   1C                   ENDIF
046800970910     C*
046900970910     C* SOLO CARATTERI NUMERICI
047000970910     C                   SETOFF                                       99
047100970910     C                   TESTN                   WLIN                 99
047200970910IF  1C     *IN99         IFEQ      *OFF                                         *NON E'NUMERO
047300970910     C                   MOVEL     '1'           ERRL00
047400970910X   1C                   ELSE                                                   *E'NUMERO DA TESTN
047500970910     C                   MOVE      WLIN          W001              1
047600970910IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
047700970910     C                   MOVEL     '1'           ERRL00
047800970910X   2C                   ELSE                                                   *NUMERO
047900970910     C*
048000970910     C* CONTROLLA ESISTENZA CODICE
048100970910     C                   MOVEL     WLIN          KORFIL
048200970910     C     KEYORG        CHAIN     AZORG01L                           99
048300970910IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
048400970910     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
048500970910     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
048600970910     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
048700970910X   3C                   ELSE
048800970910     C                   MOVEL     '1'           ERRL00
048900970910E   3C                   ENDIF
049000970910E   2C                   ENDIF
049100970910E   1C                   ENDIF
049200970910     C*
049300970910     C                   ENDSR
049400000221     C*------------------------------------------------------------------------*
049500000221     C* AZZVID - AZZERA I CAMPI DEL VIDEO
049600970910     C*------------------------------------------------------------------------*
049700970910     C     AZZVID        BEGSR
049800970910     C* CAMPI
049900970611     C                   Z-ADD     D00DRI        G08INV
050000970611     C                   Z-ADD     *ZEROS        G08DAT
050100970611     C                   MOVEL     '3'           G08ERR
050200970611     C                   CALL      'XSRDA8'
050300970611     C                   PARM                    WLBDA8
050400970611     C                   Z-ADD     G08DAT        V1CDRI
050500000221     C                   MOVEL     'N'           V1CPRI
050600000221     C                   MOVEL     'S'           V1CLDI
050700000221     C                   MOVEL     'S'           V1CISO
050800970910     C                   MOVEL     *BLANKS       V1CCA1
050900970910     C                   MOVEL     *BLANKS       V1CCA2
051000970910     C                   MOVEL     *BLANKS       V1CPR1
051100970910     C                   MOVEL     *BLANKS       V1CPR2
051200970910     C                   MOVEL     *BLANKS       V1CLN1
051300970910     C                   MOVEL     *BLANKS       V1CLN2
051400970910     C* DECODIFICHE
051500970910     C                   MOVEL     *BLANKS       V1DPR1
051600970910     C                   MOVEL     *BLANKS       V1DPR2
051700970910     C                   MOVEL     *BLANKS       V1DLN1
051800970910     C                   MOVEL     *BLANKS       V1DLN2
051900000221     C*
052000000221     C                   ENDSR
052100970910     C*------------------------------------------------------------------------*
052200970910     C* *INZSR - OPERAZIONI INIZIALI
052300970910     C*------------------------------------------------------------------------*
052400970910     C     *INZSR        BEGSR
052500970910     C*---
052600970910     C* RICEVIMENTO PARAMETRI
052700970910     C*---
052800970910     C     *ENTRY        PLIST
052900000223     C                   PARM                    KPJBA
053000970612     C                   PARM                    DSSI00
053100970910     C*
053200970910     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
053300970910     C     XMSGPA        PLIST
053400970910     C                   PARM                    DSMSMS
053500970910     C                   PARM                    DSMSPG
053600970910     C                   PARM                    DSMSKE
053700970910     C                   PARM                    DSMSER
053800970910     C*
053900970910     C* IMPOSTA LA DS -TASTI PREMUTI-
054000970910     C                   CALL      'XKEY'
054100970910     C                   PARM                    DSKEY
054200970910     C*---
054300970910     C* VARIABILI NON RIFERITE AL DATA BASE
054400970910     C*---
054500970910     C     *LIKE         DEFINE    FUDFUN        FUNKEY
054600970910     C     *LIKE         DEFINE    D00OP0        DEPOP0
054700970910     C     *LIKE         DEFINE    D00OP1        DEPOP1
054800970910     C     *LIKE         DEFINE    D00OP0        WOP0
054900970910     C     *LIKE         DEFINE    D00OP1        WOP1
055000970910     C*---
055100970910     C* VARIABILI DI CONTROLLO
055200970910     C*---
055300970910     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
055400970910     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055500970910     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055600970910     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055700970910     C*
055800970910     C* IMPOSTA LA DS -INVIO MESSAGGI-
055900000221     C                   MOVEL     'TISI08R'     DSMSPG
056000000221     C                   MOVEL     'TISI08R'     MSGQUE
056100970910     C                   MOVEL     *BLANKS       MSGKEY
056200970910     C                   MOVEL     *BLANKS       DSMSMS
056300970910     C*---
056400970910     C* VARIABILI RIFERITE AL DATA BASE
056500970910     C*---
056600970910     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
056700970910     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
056800970910     C     *LIKE         DEFINE    TBLCOD        KTBCOD
056900970910     C     *LIKE         DEFINE    TBLKEY        KTBKEY
057000970910     C*---
057100970910     C* CHIAVI DI LETTURA
057200970910     C*---
057300970910     C* LETTURA AZORG01L - COMPLETA
057400000221     C     KEYORG        KLIST
057500970910     C                   KFLD                    KORFIL                         *FILIALE
057600970910     C* LETTURA TABEL00F - COMPLETA
057700970910     C     KEYTAB        KLIST
057800970910     C                   KFLD                    KTBKUT                         *CODICE UTENTE
057900970910     C                   KFLD                    KTBCOD                         *TABELLA
058000970910     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
058100970910     C*
058200970910     C* DEPOSITA LE OPZIONI DI ENTRATA
058300970910     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
058400970910     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
058500970703     C*
058600970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
058700970703IF  1C     D00VER        IFEQ      *ZEROS
058800970707     C                   Z-ADD     D00DRI        P96DRI            8 0
058900970707     C                   Z-ADD     *ZEROS        P96VER            5 0
059000970707     C                   CALL      'TISI96R'
059100970707     C                   PARM                    P96DRI
059200970707     C                   PARM                    P96VER
059300970707     C                   Z-ADD     P96VER        D00VER
059400970703E   1C                   ENDIF
059500970910     C*
059600970910     C* GESTIONE TASTI FUNZIONALI A VIDEO
059700970910     C                   Z-ADD     *ZEROS        FUNGRU            5 0
059800970910     C                   EXSR      FUNGES
059900970910     C*
060000970910     C* IMPOSTO LA VIDEATA
060100970910     C                   EXSR      AZZVID
060200970910     C*
060300970910     C                   ENDSR
060400970910     C*------------------------------------------------------------------------*
060500970910     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
060600970910     C*------------------------------------------------------------------------*
060700970910     C     FUNGES        BEGSR
060800970910     C*
060900970910     C* PULISCO CAMPI DI LAVORO
061000970910     C                   Z-ADD     0             W                 5 0
061100970910     C*
061200970910     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
061300970910     C* E CARICO TABELLA FUNZIONI ATTIVE
061400970910     C     1             DO        24            I
061500970910     C                   MOVEL     FUD(I)        FUDRIG
061600970910     C*
061700970910     C* PULIZIA FUNZIONI PRECEDENTI
061800970910     C                   MOVEL     *BLANKS       FUV(I)
061900970910     C*
062000970910     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
062100970910     C* (SALTA F24)
062200970910     C     FUDATT        IFEQ      'S'
062300970910     C     FUDFUN        ANDNE     '24'
062400970910     C                   ADD       1             W
062500970910     C                   MOVEL     FUDDES        FUV(W)
062600970910     C                   END
062700970910     C                   END                                                    DO WHILE
062800970910     C*
062900970910     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
063000970910     C* ALTRIMENTI DISATTIVO F24
063100970910     C     1             DO        24            I
063200970910     C                   MOVEL     FUD(I)        FUDRIG
063300970910     C*
063400970910     C     FUDFUN        IFEQ      '24'
063500970910     C     W             IFGT      6
063600970910     C                   MOVEL     'S'           FUDATT
063700970910     C                   ELSE
063800970910     C                   MOVEL     *BLANKS       FUDATT
063900970910     C                   END                                                    W GT 8
064000970910     C                   MOVEL     FUDRIG        FUD(I)
064100970910     C                   END
064200970910     C                   END                                                    DO WHILE
064300970910     C*
064400970910     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
064500970910     C     W             DIV       6             I
064600970910     C                   MVR                     Z
064700970910     C*
064800970910     C     Z             IFNE      0
064900970910     C     I             ADD       1             Z
065000970910     C                   ELSE
065100970910     C                   Z-ADD     I             Z                 5 0
065200970910     C                   END
065300970910     C*
065400970910     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
065500970910     C                   ADD       1             FUNGRU
065600970910     C     FUNGRU        IFGT      Z
065700970910     C     FUNGRU        ORLT      1
065800970910     C                   Z-ADD     1             FUNGRU
065900970910     C                   END
066000970910     C*
066100970910     C* PRIMO GRUPPO DI 6 FUNZIONI
066200970910     C     FUNGRU        IFEQ      1
066300970910     C                   MOVEL     FUV(1)        FUN001
066400970910     C                   MOVEL     FUV(2)        FUN002
066500970910     C                   MOVEL     FUV(3)        FUN003
066600970910     C                   MOVEL     FUV(4)        FUN004
066700970910     C                   MOVEL     FUV(5)        FUN005
066800970910     C                   MOVEL     FUV(6)        FUN006
066900970910     C                   END
067000970910     C*
067100970910     C* SECONDO GRUPPO DI 6 FUNZIONI
067200970910     C     FUNGRU        IFEQ      2
067300970910     C                   MOVEL     FUV(7)        FUN001
067400970910     C                   MOVEL     FUV(8)        FUN002
067500970910     C                   MOVEL     FUV(9)        FUN003
067600970910     C                   MOVEL     FUV(10)       FUN004
067700970910     C                   MOVEL     FUV(11)       FUN005
067800970910     C                   MOVEL     FUV(12)       FUN006
067900970910     C                   END
068000970910     C*
068100970910     C* TERZO GRUPPO DI 6 FUNZIONI
068200970910     C     FUNGRU        IFEQ      3
068300970910     C                   MOVEL     FUV(13)       FUN001
068400970910     C                   MOVEL     FUV(14)       FUN002
068500970910     C                   MOVEL     FUV(15)       FUN003
068600970910     C                   MOVEL     FUV(16)       FUN004
068700970910     C                   MOVEL     FUV(17)       FUN005
068800970910     C                   MOVEL     FUV(18)       FUN006
068900970910     C                   END
069000970910     C*
069100970910     C* QUARTO GRUPPO DI 6 FUNZIONI
069200970910     C     FUNGRU        IFEQ      4
069300970910     C                   MOVEL     FUV(19)       FUN001
069400970910     C                   MOVEL     FUV(20)       FUN002
069500970910     C                   MOVEL     FUV(21)       FUN003
069600970910     C                   MOVEL     FUV(22)       FUN004
069700970910     C                   MOVEL     *BLANKS       FUN005
069800970910     C                   MOVEL     *BLANKS       FUN006
069900970910     C                   END
070000970910     C*
070100970910     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
070200970910     C                   MOVEL     *BLANKS       FUN007
070300970910     C                   MOVEL     *BLANKS       FUN008
070400970910     C*
070500970910     C     1             DO        24            I
070600970910     C                   MOVEL     FUD(I)        FUDRIG
070700970910     C     FUDFUN        IFEQ      '24'
070800970910     C     FUDATT        ANDEQ     'S'
070900970910     C                   MOVEL     FUDDES        FUN008
071000970910     C                   END
071100970910     C                   END                                                    DO
071200970910     C*
071300970910     C                   ENDSR
071400970910     C*------------------------------------------------------------------------*
071500970910     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
071600970910     C*------------------------------------------------------------------------*
071700970910     C     FUNCON        BEGSR
071800970910     C*
071900970910     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
072000970910     C                   EXSR      FUNVAR
072100970910     C*
072200970910     C* PULISCO CAMPI DI LAVORO
072300970910     C                   Z-ADD     0             I                 5 0
072400970910     C                   Z-ADD     0             W                 5 0
072500970910     C*
072600970910     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
072700970910     C     FUNKEY        IFNE      *BLANKS
072800970910     C     1             DO        24            I
072900970910     C                   MOVEL     FUD(I)        FUDRIG
073000970910     C*
073100970910     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
073200970910     C     FUNKEY        IFEQ      FUDFUN
073300970910     C     FUDATT        ANDEQ     'S'
073400970910     C                   Z-ADD     I             W
073500970910     C                   END
073600970910     C                   END                                                    DO WHILE
073700970910     C*
073800970910     C* FUNZIONE NON ATTIVA (ERRORE)
073900970910     C     W             IFEQ      0
074000970910     C*
074100970910     C* EMISSIONE MESSAGGIO DI ERRORE
074200970910     C                   MOVEL     MSG(1)        DSMSMS
074300970910     C*
074400970910     C                   END                                                    W=0
074500970910     C                   END                                                    FUNKEY NE *BLANKS
074600970910     C*
074700970910     C* FUNZIONE ATTIVA (PROCEDI ...)
074800970910     C     W             IFNE      0
074900970910     C     FUNKEY        OREQ      *BLANKS
075000970910     C     FUNKEY        IFEQ      *BLANKS
075100970910     C                   EXSR      ENTGES
075200970910     C                   END
075300970910     C     FUNKEY        IFEQ      '03'
075400970910     C                   EXSR      F03GES
075500970910     C                   END
075600970910     C     FUNKEY        IFEQ      '06'
075700970910     C                   EXSR      F06GES
075800970910     C                   END
075900970910     C     FUNKEY        IFEQ      '12'
076000970910     C                   EXSR      F12GES
076100970910     C                   END
076200970910     C     FUNKEY        IFEQ      '24'
076300970910     C                   EXSR      F24GES
076400970910     C                   END
076500970910     C                   END
076600970910     C*
076700970910     C                   ENDSR
076800970910     C*------------------------------------------------------------------------*
076900970910     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
077000970910     C*------------------------------------------------------------------------*
077100970910     C     FUNVAR        BEGSR
077200970910     C*
077300970910     C* INIZIALIZZA VARIABILE -FUNKEY-
077400970910     C*
077500970910     C* FUNKEY=*BLANKS SIGNIFICA ENTER
077600970910     C     KEY           IFEQ      ENTER
077700970910     C                   MOVEL     *BLANKS       FUNKEY
077800970910     C                   END
077900970910     C     KEY           IFEQ      F1
078000970910     C                   MOVEL     '01'          FUNKEY
078100970910     C                   END
078200970910     C     KEY           IFEQ      F2
078300970910     C                   MOVEL     '02'          FUNKEY
078400970910     C                   END
078500970910     C     KEY           IFEQ      F3
078600970910     C                   MOVEL     '03'          FUNKEY
078700970910     C                   END
078800970910     C     KEY           IFEQ      F4
078900970910     C                   MOVEL     '04'          FUNKEY
079000970910     C                   END
079100970910     C     KEY           IFEQ      F5
079200970910     C                   MOVEL     '05'          FUNKEY
079300970910     C                   END
079400970910     C     KEY           IFEQ      F6
079500970910     C                   MOVEL     '06'          FUNKEY
079600970910     C                   END
079700970910     C     KEY           IFEQ      F7
079800970910     C                   MOVEL     '07'          FUNKEY
079900970910     C                   END
080000970910     C     KEY           IFEQ      F8
080100970910     C                   MOVEL     '08'          FUNKEY
080200970910     C                   END
080300970910     C     KEY           IFEQ      F9
080400970910     C                   MOVEL     '09'          FUNKEY
080500970910     C                   END
080600970910     C     KEY           IFEQ      F10
080700970910     C                   MOVEL     '10'          FUNKEY
080800970910     C                   END
080900970910     C     KEY           IFEQ      F11
081000970910     C                   MOVEL     '11'          FUNKEY
081100970910     C                   END
081200970910     C     KEY           IFEQ      F12
081300970910     C                   MOVEL     '12'          FUNKEY
081400970910     C                   END
081500970910     C     KEY           IFEQ      F13
081600970910     C                   MOVEL     '13'          FUNKEY
081700970910     C                   END
081800970910     C     KEY           IFEQ      F14
081900970910     C                   MOVEL     '14'          FUNKEY
082000970910     C                   END
082100970910     C     KEY           IFEQ      F15
082200970910     C                   MOVEL     '15'          FUNKEY
082300970910     C                   END
082400970910     C     KEY           IFEQ      F16
082500970910     C                   MOVEL     '16'          FUNKEY
082600970910     C                   END
082700970910     C     KEY           IFEQ      F17
082800970910     C                   MOVEL     '17'          FUNKEY
082900970910     C                   END
083000970910     C     KEY           IFEQ      F18
083100970910     C                   MOVEL     '18'          FUNKEY
083200970910     C                   END
083300970910     C     KEY           IFEQ      F19
083400970910     C                   MOVEL     '19'          FUNKEY
083500970910     C                   END
083600970910     C     KEY           IFEQ      F20
083700970910     C                   MOVEL     '20'          FUNKEY
083800970910     C                   END
083900970910     C     KEY           IFEQ      F21
084000970910     C                   MOVEL     '21'          FUNKEY
084100970910     C                   END
084200970910     C     KEY           IFEQ      F22
084300970910     C                   MOVEL     '22'          FUNKEY
084400970910     C                   END
084500970910     C     KEY           IFEQ      F23
084600970910     C                   MOVEL     '23'          FUNKEY
084700970910     C                   END
084800970910     C     KEY           IFEQ      F24
084900970910     C                   MOVEL     '24'          FUNKEY
085000970910     C                   END
085100970910     C     KEY           IFEQ      ROLLUP
085200970910     C                   MOVEL     'RU'          FUNKEY
085300970910     C                   END
085400970910     C     KEY           IFEQ      HELP
085500970910     C                   MOVEL     'HP'          FUNKEY
085600970910     C                   END
085700970910     C                   ENDSR
085800970910     C*------------------------------------------------------------------------*
085900970910     C* NOTE COSTRUZIONE SCHIERE
086000970910     C*------------------------------------------------------------------------*
086100970910     C*
086200970910     C* -MSG-MESSAGGI
086300970910     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
086400970910     C* (NON TOCCARE MSG,1 E MSG,2)
086500970910     C*
086600970910     C* -CMD-COMANDI
086700970910     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
086800970910     C*
086900970910     C* -OPD-OPZIONI
087000970910     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
087100970910     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
087200970910     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
087300970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
087400970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
087500970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
087600970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
087700970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
087800970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
087900970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
088000970910     C*
088100970910     C* -FUD-FUNZIONI
088200970910     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
088300970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088400970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088500970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
088600970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088700970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
088800970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088900970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
089000970910     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
089100970910     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
089200970910     O*------------------------------------------------------------------------*
089300970910** MSG - MESSAGGI
089400000221TISI08R- Opzione non gestita                                                   1
089500000221TISI08R- Tasto funzionale non gestito                                          2
089600000221TISI08R- 3
089700000221TISI08R- 4
089800000221TISI08R- 5
089900000221TISI08R- 6
090000000221TISI08R- 7
090100000221TISI08R- 8
090200000221TISI08R- 9
090300000221TISI08R- 10
090400000221TISI08R- Immettere uno dei valori possibili                                   11
090500000221TISI08R- Tempo consegna errato                                                12
090600000221TISI08R- Nazione errata                                                       13
090700000221TISI08R- Se richiesto il Dal immettere anche l'AL e viceversa                 14
090800000221TISI08R- Il Dal deve essere inferiore o uguale all'AL                         15
090900000221TISI08R- Provincia errata                                                     16
091000000221TISI08R- Linea di arrivo errata                                               17
091100000221TISI08R- Tipo inoltro errato                                                  18
091200000221TISI08R- Immettere 'S' per stampare le località gestite a livello di località 19
091300000221TISI08R- Stampa lanciata ...                                                  20
091400970610** CMD - COMANDI CL
091500970610WRKSPLF
091600970610
091700970610
091800970610
091900970610
092000970910** FUD - FUNZIONI
092100970910                   -        (S/N)=N                        funz.01¬
092200970910                   -        (S/N)=N                        funz.02¬
092300970910F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
092400970910                   -        (S/N)=N                        funz.04¬
092500970910                   -        (S/N)=N                        funz.05¬
092600970610F06=Conferma       -        (S/N)=S                        funz.06¬
092700970910                   -        (S/N)=N                        funz.07¬
092800970910                   -        (S/N)=N                        funz.08¬
092900970910                   -        (S/N)=N                        funz.09¬
093000970910                   -        (S/N)=N                        funz.10¬
093100970910                   -        (S/N)=N                        funz.11¬
093200970910F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
093300970910                   -        (S/N)=N                        funz.13¬
093400970910                   -        (S/N)=N                        funz.14¬
093500970910                   -        (S/N)=N                        funz.15¬
093600970910                   -        (S/N)=N                        funz.16¬
093700970910                   -        (S/N)=N                        funz.17¬
093800970910                   -        (S/N)=N                        funz.18¬
093900970910                   -        (S/N)=N                        funz.19¬
094000970910                   -        (S/N)=N                        funz.20¬
094100970910                   -        (S/N)=N                        funz.21¬
094200970910                   -        (S/N)=N                        funz.22¬
094300970910                   -        (S/N)=N                        funz.23¬
094400970910F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
