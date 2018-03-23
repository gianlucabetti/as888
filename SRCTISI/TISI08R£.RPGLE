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
005700970910     D*------------
005800970910     D* DS "XFIL" - REPERIMENTO DATI UTENTE
005900970910     D*------------
006000970910     D DSFIL         E DS
006100970611     D*------------------
006200970611     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300970611     D*------------------
006400970611     D WLBDA8          DS                  INZ
006500970611     D  G08DAT                 1      8  0
006600970611     D  G08INV                 9     16  0
006700970611     D  G08ERR                17     17
006800970611     D  G08TGI                18     22  0
006900970910     D*------------------
007000970910     D* DS NAZIONI
007100970910     D*------------------
007200970910     D DS15          E DS
007300970910     D*------------------
007400970910     D* DS PROVINCIE
007500970910     D*------------------
007600970910     D DSPR          E DS
007700970612     D*------------------
007800970612     D* DS TIPI INOLTRO
007900970612     D*------------------
008000970612     D DS9A          E DS
008100970910     D*------------
008200970910     D* DS DI PROCEDURA
008300970910     D*------------
008400970910     D DSSI00        E DS                  EXTNAME(TISI00DS)
008500970910     D*------------
008600970910     D* DS PARAMETRI DI LANCIO
008700970910     D*------------
008800970610     D PARAM           DS                  INZ
008900000221     D  PARDAT                 1      8  0
009000000221     D  PARVER                 9     13  0
009100000221     D  PARPRI                14     14
009200000221     D  PARLDI                15     15
009300000221     D  PARISO                16     16
009400000221     D  PARCAD                17     25
009500000221     D  PARCAA                26     34
009600000221     D  PARPRD                35     36
009700000221     D  PARPRA                37     38
009800000221     D  PARLAD                39     41  0
009900000221     D  PARLAA                42     44  0
010000000223     D*-------------
010100000223     D* ARCHITETTURA
010200000223     D*------------
010300000223     D KPJBA         E DS
010400970910     C*------------------------------------------------------------------------*
010500970910     C* MAIN LINES
010600970910     C*------------------------------------------------------------------------*
010700970910     C*
010800970910     C* NON CI SONO ERRORI IN ENTRATA
010900970910IF  1C     ERRL00        IFEQ      '0'
011000970910     C*
011100970910     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011200970910DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011300970910     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011400970910     C     FINL00        OREQ      '1'                                          *FINE PGM
011500970910     C*
011600970910     C* EMETTE FORMATO VIDEO
011700970910     C                   EXSR      BUTFMR
011800970910     C*
011900970910     C* GESTIONE TASTI FUNZIONALI
012000970910     C                   EXSR      FUNCON
012100970910E   2C                   ENDDO
012200970910     C*
012300970910     C* CI SONO ERRORI IN ENTRATA
012400970910X   1C                   ELSE
012500970910     C                   MOVEL     '1'           D00ERR
012600970910     C                   MOVEL     DSMSMS        D00MSG
012700970910E   1C                   ENDIF
012800970910     C* FINE PGM
012900970910     C                   SETON                                        LR
013000970910     C*------------------------------------------------------------------------*
013100970910     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013200970910     C*------------------------------------------------------------------------*
013300970910     C     BUTFMR        BEGSR
013400970910     C*
013500970910     C* EMETTE LA TESTATA
013600000221     C                   WRITE     SI08D01
013700970910     C*
013800970910     C* SPEDISCE EVENTUALE MESSAGGIO
013900970910     C                   EXSR      SNDMSG
014000970910     C*
014100970910     C* EMETTE LE FUNZIONI
014200000221     C                   WRITE     SI08D02
014300970910     C*
014400970910     C* EMETTE IL VIDEO
014500000221     C                   EXFMT     SI08D03
014600970910     C*
014700970910     C* REIMPOSTA FLAG DI ERRORE
014800970910     C                   MOVEL     '0'           ERRL00
014900970910     C*
015000000221 VED C* REIMPOSTA INDICATORI DI ERRORE
015100000221  "  C                   SETOFF                                         5859
015200970910     C                   SETOFF                                       606162
015300970910     C                   SETOFF                                       636465
015400970910     C                   SETOFF                                       666768
015500970910     C                   SETOFF                                       697071
015600970910     C                   SETOFF                                       727374
015700000221  "  C                   SETOFF                                       7586
015800000221 VED C                   SETOFF                                       878889
015900970910     C*
016000970910     C                   ENDSR
016100970910     C*------------------------------------------------------------------------*
016200970910     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016300970910     C*------------------------------------------------------------------------*
016400970910     C     SNDMSG        BEGSR
016500970910     C*
016600970910     C* PULISCE IL SFILE MESSAGGI
016700970910     C                   MOVEL     '1'           *IN40
016800000221     C                   WRITE     SI08DMC
016900970910     C                   MOVEL     '0'           *IN40
017000970910     C*
017100970910     C* VISUALIZZA IL MESSAGGIO
017200970910     C                   Z-ADD     *ZEROS        MSGULT            4 0
017300970910     C                   CALL      'XMSG'        XMSGPA
017400970910IF  1C     DSMSER        IFEQ      *ZEROS
017500970910     C                   MOVEL     DSMSKE        MSGKEY
017600970910E   1C                   ENDIF
017700970910     C                   Z-ADD     1             MSGULT
017800000221     C                   WRITE     SI08DMS
017900000221     C                   WRITE     SI08DMC
018000970910     C*
018100970910     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018200970910     C                   MOVEL     *BLANKS       DSMSMS
018300970910     C*
018400970910     C                   ENDSR
018500970910     C*------------------------------------------------------------------------*
018600970910     C* ENTGES - GESTIONE ENTER -> INVIO
018700970910     C*------------------------------------------------------------------------*
018800970910     C     ENTGES        BEGSR
018900970910     C*
019000970910     C* CONTROLLA I CAMPI IMMESSI
019100970910     C                   EXSR      DECODI
019200970910     C*
019300970910     C                   ENDSR
019400970910     C*------------------------------------------------------------------------*
019500970910     C* F03GES - GESTIONE F03 -> FINE
019600970910     C*------------------------------------------------------------------------*
019700970910     C     F03GES        BEGSR
019800970910     C*
019900970910     C* IMPOSTA "PREMUTO F03"
020000970910     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020100970910     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
020200970910     C*
020300970910     C                   ENDSR
020400970910     C*------------------------------------------------------------------------*
020500970623     C* F06GES - GESTIONE F06 -> CONFERMA
020600970910     C*------------------------------------------------------------------------*
020700970723     C     F06GES        BEGSR
020800970723     C*
020900970723     C* CONTROLLA I CAMPI IMMESSI
021000970723     C                   EXSR      DECODI
021100970910     C*
021200000221     C* LANCIA IL PROGRAMMA DI STAMPA
021300000221IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021400000221     C                   Z-ADD     V1CDRI        PARDAT
021500000221     C                   Z-ADD     D00VER        PARVER
021600000221     C                   MOVEL     V1CPRI        PARPRI
021700000221     C                   MOVEL     V1CLDI        PARLDI
021800000221     C                   MOVEL     V1CISO        PARISO
021900000221     C                   MOVEL     V1CCA1        PARCAD
022000000221     C                   MOVEL     V1CCA2        PARCAA
022100000221     C                   MOVEL     V1CPR1        PARPRD
022200000221     C                   MOVEL     V1CPR2        PARPRA
022300000221     C                   MOVE      V1CLN1        PARLAD
022400000221     C                   MOVE      V1CLN2        PARLAA
022500000223     C                   MOVEL     PARAM         KPJBU
022600000223     C                   MOVEL     'SI09'        KCOAZ
022700010119     C                   CALL      'BCH10'
022800010119 MOM C*                  CALL      'TISI09R'
022900000223     C                   PARM                    KPJBA
023000000223     C*                  MOVEL     '1'           FINL00
023100970723E   1C                   ENDIF
023200000221     C*
023300000221     C                   ENDSR
023400970910     C*------------------------------------------------------------------------*
023500970910     C* F12GES - GESTIONE F12 -> RITORNO
023600970910     C*------------------------------------------------------------------------*
023700970910     C     F12GES        BEGSR
023800970910     C*
023900970910     C* IMPOSTA "PREMUTO F12"
024000970910     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024100970910     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
024200970910     C*
024300970910     C                   ENDSR
024400970910     C*------------------------------------------------------------------------*
024500970910     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
024600970910     C*------------------------------------------------------------------------*
024700970910     C     F22GES        BEGSR
024800000221     C*
024900970910     C                   Z-ADD     80            LENGH            15 5
025000970910     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
025100970910     C                   PARM                    CMD(1)
025200970910     C                   PARM                    LENGH
025300970910     C*
025400970910     C                   ENDSR
025500970910     C*------------------------------------------------------------------------*
025600970910     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025700970910     C*------------------------------------------------------------------------*
025800970910     C     F24GES        BEGSR
025900970910     C*
026000970910     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026100970910     C*
026200970910     C                   ENDSR
026300970910     C*------------------------------------------------------------------------*
026400000221     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026500000221     C*------------------------------------------------------------------------*
026600970910     C     DECODI        BEGSR
026700970910     C*--------------------
026800970910     C* CAP DAL+AL
026900970910     C*--------------------
027000970910     C* SE DAL ANCHE AL E VICEVERSA
027100970910IF  1C     ERRL00        IFEQ      '0'
027200970910IF  2C     V1CCA1        IFNE      *BLANKS
027300970910     C     V1CCA2        ANDEQ     *BLANKS
027400970910     C     V1CCA2        ORNE      *BLANKS
027500970910     C     V1CCA1        ANDEQ     *BLANKS
027600970910     C                   SETON                                        7071
027700970910     C                   MOVEL     MSG(14)       DSMSMS
027800970910     C                   MOVEL     '1'           ERRL00
027900970910E   2C                   ENDIF
028000970910E   1C                   ENDIF
028100970910     C* DAL <= AL
028200970910IF  1C     ERRL00        IFEQ      '0'
028300970910IF  2C     V1CCA1        IFNE      *BLANKS
028400970910IF  3C     V1CCA1        IFGT      V1CCA2
028500970910     C                   SETON                                        7071
028600970910     C                   MOVEL     MSG(15)       DSMSMS
028700970910     C                   MOVEL     '1'           ERRL00
028800970910E   3C                   ENDIF
028900970910E   2C                   ENDIF
029000970910E   1C                   ENDIF
029100970910     C*--------------------
029200970910     C* PROVINCIA DAL
029300970910     C*--------------------
029400970910IF  1C     ERRL00        IFEQ      '0'
029500970910     C                   MOVEL     *BLANKS       V1DPR1
029600970910IF  2C     V1CPR1        IFNE      *BLANKS
029700970910     C                   MOVEL     V1CPR1        WPRV
029800970910     C                   MOVEL     *BLANKS       WDPRV
029900970910     C                   EXSR      CHKPRV
030000970910     C                   MOVEL     WPRV          V1CPR1
030100970910     C                   MOVEL     WDPRV         V1DPR1
030200970910IF  3C     ERRL00        IFEQ      '1'
030300970910     C                   SETON                                        72
030400970910     C                   MOVEL     MSG(16)       DSMSMS
030500970910E   3C                   ENDIF
030600970910E   2C                   ENDIF
030700970910E   1C                   ENDIF
030800970910     C*--------------------
030900970910     C* PROVINCIA AL
031000970910     C*--------------------
031100970910IF  1C     ERRL00        IFEQ      '0'
031200970910     C                   MOVEL     *BLANKS       V1DPR2
031300970910IF  2C     V1CPR2        IFNE      *BLANKS
031400970910     C                   MOVEL     V1CPR2        WPRV
031500970910     C                   MOVEL     *BLANKS       WDPRV
031600970910     C                   EXSR      CHKPRV
031700970910     C                   MOVEL     WPRV          V1CPR2
031800970910     C                   MOVEL     WDPRV         V1DPR2
031900970910IF  3C     ERRL00        IFEQ      '1'
032000970910     C                   SETON                                        73
032100970910     C                   MOVEL     MSG(16)       DSMSMS
032200970910E   3C                   ENDIF
032300970910E   2C                   ENDIF
032400970910E   1C                   ENDIF
032500970910     C*--------------------
032600970910     C* PROVINCIA DAL+AL
032700970910     C*--------------------
032800970910     C* SE DAL ANCHE AL E VICEVERSA
032900970910IF  1C     ERRL00        IFEQ      '0'
033000970910IF  2C     V1CPR1        IFNE      *BLANKS
033100970910     C     V1CPR2        ANDEQ     *BLANKS
033200970910     C     V1CPR2        ORNE      *BLANKS
033300970910     C     V1CPR1        ANDEQ     *BLANKS
033400970910     C                   SETON                                        7273
033500970910     C                   MOVEL     MSG(14)       DSMSMS
033600970910     C                   MOVEL     '1'           ERRL00
033700970910E   2C                   ENDIF
033800970910E   1C                   ENDIF
033900970910     C* DAL <= AL
034000970910IF  1C     ERRL00        IFEQ      '0'
034100970910IF  2C     V1CPR1        IFNE      *BLANKS
034200970910IF  3C     V1CPR1        IFGT      V1CPR2
034300970910     C                   SETON                                        7273
034400970910     C                   MOVEL     MSG(15)       DSMSMS
034500970910     C                   MOVEL     '1'           ERRL00
034600970910E   3C                   ENDIF
034700970910E   2C                   ENDIF
034800970910E   1C                   ENDIF
034900970910     C*--------------------
035000970910     C* LINEA ARRIVO DAL
035100970910     C*--------------------
035200970910IF  1C     ERRL00        IFEQ      '0'
035300970910     C                   MOVEL     *BLANKS       V1DLN1
035400970910IF  2C     V1CLN1        IFNE      *BLANKS
035500970910     C                   MOVEL     V1CLN1        WLIN
035600970910     C                   MOVEL     *BLANKS       WDLIN
035700970910     C                   EXSR      CHKLIN
035800970910     C                   MOVEL     WLIN          V1CLN1
035900970910     C                   MOVEL     WDLIN         V1DLN1
036000970910IF  3C     ERRL00        IFEQ      '1'
036100970910     C                   SETON                                        74
036200970910     C                   MOVEL     MSG(17)       DSMSMS
036300970910E   3C                   ENDIF
036400970910E   2C                   ENDIF
036500970910E   1C                   ENDIF
036600970910     C*--------------------
036700970910     C* LINEA ARRIVO AL
036800970910     C*--------------------
036900970910IF  1C     ERRL00        IFEQ      '0'
037000970910     C                   MOVEL     *BLANKS       V1DLN2
037100970910IF  2C     V1CLN2        IFNE      *BLANKS
037200970910     C                   MOVEL     V1CLN2        WLIN
037300970910     C                   MOVEL     *BLANKS       WDLIN
037400970910     C                   EXSR      CHKLIN
037500970910     C                   MOVEL     WLIN          V1CLN2
037600970910     C                   MOVEL     WDLIN         V1DLN2
037700970910IF  3C     ERRL00        IFEQ      '1'
037800970910     C                   SETON                                        75
037900970910     C                   MOVEL     MSG(17)       DSMSMS
038000970910E   3C                   ENDIF
038100970910E   2C                   ENDIF
038200970910E   1C                   ENDIF
038300970910     C*--------------------
038400970910     C* LINEA ARRIVO DAL+AL
038500970910     C*--------------------
038600970910     C* SE DAL ANCHE AL E VICEVERSA
038700970910IF  1C     ERRL00        IFEQ      '0'
038800970910IF  2C     V1CLN1        IFNE      *BLANKS
038900970910     C     V1CLN2        ANDEQ     *BLANKS
039000970910     C     V1CLN2        ORNE      *BLANKS
039100970910     C     V1CLN1        ANDEQ     *BLANKS
039200970910     C                   SETON                                        7475
039300970910     C                   MOVEL     MSG(14)       DSMSMS
039400970910     C                   MOVEL     '1'           ERRL00
039500970910E   2C                   ENDIF
039600970910E   1C                   ENDIF
039700970910     C* DAL <= AL
039800970910IF  1C     ERRL00        IFEQ      '0'
039900970910IF  2C     V1CLN1        IFNE      *BLANKS
040000970910IF  3C     V1CLN1        IFGT      V1CLN2
040100970910     C                   SETON                                        7475
040200970910     C                   MOVEL     MSG(15)       DSMSMS
040300970910     C                   MOVEL     '1'           ERRL00
040400970910E   3C                   ENDIF
040500970910E   2C                   ENDIF
040600970910E   1C                   ENDIF
040700000221     C*
040800000221     C                   ENDSR
040900970910     C*------------------------------------------------------------------------*
041000970910     C* CHKPRV - CONTROLLA LA PROVINCIA
041100970910     C*------------------------------------------------------------------------*
041200970910     C     CHKPRV        BEGSR
041300970910     C*
041400970910     C     *LIKE         DEFINE    V1CPR1        WPRV
041500970910     C     *LIKE         DEFINE    V1DPR1        WDPRV
041600970910     C*
041700970910     C* RICHIESTO ELENCO
041800970910     C                   SETOFF                                           99
041900970910     C     '?'           SCAN      WPRV                                   99
042000970910IF  1C     *IN99         IFEQ      *ON
042100970910     C                   Z-ADD     DSFICD        §KUT              1 0
042200970910     C                   MOVEL     'PR'          §COD              2
042300970910     C                   MOVEL     *BLANKS       §KEY              8
042400970910     C                   MOVEL     *BLANKS       §DES             30
042500970910     C                   CALL      'X§TABER'
042600970910     C                   PARM                    §KUT
042700970910     C                   PARM                    §COD
042800970910     C                   PARM                    §KEY
042900970910     C                   PARM                    §DES
043000970910     C                   MOVEL     §KEY          WPRV
043100970910     C                   MOVEL     §DES          WDPRV
043200970910E   1C                   ENDIF
043300970910     C*
043400970910     C* CONTROLLA ESISTENZA CODICE
043500970910     C                   MOVEL     DSFICD        KTBKUT
043600970910     C                   MOVEL     'PR'          KTBCOD
043700970910     C                   MOVEL(P)  WPRV          KTBKEY
043800970910     C     KEYTAB        CHAIN     TABEL00F                           99
043900970910IF  1C     *IN99         IFEQ      *OFF
044000970910     C     TBLFLG        ANDEQ     *BLANKS
044100970910     C                   MOVEL     TBLUNI        DSPR
044200970910     C                   MOVEL     §PRDES        WDPRV
044300970910X   1C                   ELSE
044400970910     C                   MOVEL     '1'           ERRL00
044500970910E   1C                   ENDIF
044600970910     C*
044700970910     C                   ENDSR
044800970910     C*------------------------------------------------------------------------*
044900970910     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
045000970910     C*------------------------------------------------------------------------*
045100970910     C     CHKLIN        BEGSR
045200970910     C*
045300970610     C     *LIKE         DEFINE    V1CLN1        WLIN
045400970610     C     *LIKE         DEFINE    V1DLN1        WDLIN
045500970910     C*
045600970910     C* RICHIESTO ELENCO
045700970910     C                   SETOFF                                           99
045800970910     C     '?'           SCAN      WLIN                                   99
045900970910IF  1C     *IN99         IFEQ      *ON
046000970910     C                   MOVEL     *BLANKS       §COD1             3
046100970910     C                   MOVEL     *BLANKS       §TIP              1
046200970910     C                   MOVEL     *BLANKS       §DES1            25
046300971203     C                   MOVEL     *BLANKS       §PRA              3
046400971203     C                   CALL      'TNSD24R'
046500970910     C                   PARM                    §COD1
046600970910     C                   PARM                    §TIP
046700970910     C                   PARM                    §DES1
046800971203     C                   PARM                    §PRA
046900970910     C                   MOVEL     §COD1         WLIN
047000970910     C                   MOVEL     §DES1         WDLIN
047100970910E   1C                   ENDIF
047200970910     C*
047300970910     C* SOLO CARATTERI NUMERICI
047400970910     C                   SETOFF                                       99
047500970910     C                   TESTN                   WLIN                 99
047600970910IF  1C     *IN99         IFEQ      *OFF                                         *NON E'NUMERO
047700970910     C                   MOVEL     '1'           ERRL00
047800970910X   1C                   ELSE                                                   *E'NUMERO DA TESTN
047900970910     C                   MOVE      WLIN          W001              1
048000970910IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
048100970910     C                   MOVEL     '1'           ERRL00
048200970910X   2C                   ELSE                                                   *NUMERO
048300970910     C*
048400970910     C* CONTROLLA ESISTENZA CODICE
048500970910     C                   MOVEL     WLIN          KORFIL
048600970910     C     KEYORG        CHAIN     AZORG01L                           99
048700970910IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
048800970910     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
048900970910     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
049000970910     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
049100970910X   3C                   ELSE
049200970910     C                   MOVEL     '1'           ERRL00
049300970910E   3C                   ENDIF
049400970910E   2C                   ENDIF
049500970910E   1C                   ENDIF
049600970910     C*
049700970910     C                   ENDSR
049800000221     C*------------------------------------------------------------------------*
049900000221     C* AZZVID - AZZERA I CAMPI DEL VIDEO
050000970910     C*------------------------------------------------------------------------*
050100970910     C     AZZVID        BEGSR
050200970910     C* CAMPI
050300970611     C                   Z-ADD     D00DRI        G08INV
050400970611     C                   Z-ADD     *ZEROS        G08DAT
050500970611     C                   MOVEL     '3'           G08ERR
050600970611     C                   CALL      'XSRDA8'
050700970611     C                   PARM                    WLBDA8
050800970611     C                   Z-ADD     G08DAT        V1CDRI
050900000221     C                   MOVEL     'N'           V1CPRI
051000000221     C                   MOVEL     'S'           V1CLDI
051100000221     C                   MOVEL     'S'           V1CISO
051200970910     C                   MOVEL     *BLANKS       V1CCA1
051300970910     C                   MOVEL     *BLANKS       V1CCA2
051400970910     C                   MOVEL     *BLANKS       V1CPR1
051500970910     C                   MOVEL     *BLANKS       V1CPR2
051600970910     C                   MOVEL     *BLANKS       V1CLN1
051700970910     C                   MOVEL     *BLANKS       V1CLN2
051800970910     C* DECODIFICHE
051900970910     C                   MOVEL     *BLANKS       V1DPR1
052000970910     C                   MOVEL     *BLANKS       V1DPR2
052100970910     C                   MOVEL     *BLANKS       V1DLN1
052200970910     C                   MOVEL     *BLANKS       V1DLN2
052300000221     C*
052400000221     C                   ENDSR
052500970910     C*------------------------------------------------------------------------*
052600970910     C* *INZSR - OPERAZIONI INIZIALI
052700970910     C*------------------------------------------------------------------------*
052800970910     C     *INZSR        BEGSR
052900970910     C*---
053000970910     C* RICEVIMENTO PARAMETRI
053100970910     C*---
053200970910     C     *ENTRY        PLIST
053300000223     C                   PARM                    KPJBA
053400970612     C                   PARM                    DSSI00
053500970910     C*
053600970910     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
053700970910     C     XMSGPA        PLIST
053800970910     C                   PARM                    DSMSMS
053900970910     C                   PARM                    DSMSPG
054000970910     C                   PARM                    DSMSKE
054100970910     C                   PARM                    DSMSER
054200970910     C*
054300970910     C* IMPOSTA LA DS -TASTI PREMUTI-
054400970910     C                   CALL      'XKEY'
054500970910     C                   PARM                    DSKEY
054600970910     C*---
054700970910     C* VARIABILI NON RIFERITE AL DATA BASE
054800970910     C*---
054900970910     C     *LIKE         DEFINE    FUDFUN        FUNKEY
055000970910     C     *LIKE         DEFINE    D00OP0        DEPOP0
055100970910     C     *LIKE         DEFINE    D00OP1        DEPOP1
055200970910     C     *LIKE         DEFINE    D00OP0        WOP0
055300970910     C     *LIKE         DEFINE    D00OP1        WOP1
055400970910     C*---
055500970910     C* VARIABILI DI CONTROLLO
055600970910     C*---
055700970910     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
055800970910     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055900970910     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
056000970910     C                   MOVEL     '0'           ERRL00            1            *ERRORE
056100970910     C*
056200970910     C* IMPOSTA LA DS -INVIO MESSAGGI-
056300000221     C                   MOVEL     'TISI08R'     DSMSPG
056400000221     C                   MOVEL     'TISI08R'     MSGQUE
056500970910     C                   MOVEL     *BLANKS       MSGKEY
056600970910     C                   MOVEL     *BLANKS       DSMSMS
056700970910     C*---
056800970910     C* VARIABILI RIFERITE AL DATA BASE
056900970910     C*---
057000970910     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
057100970910     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
057200970910     C     *LIKE         DEFINE    TBLCOD        KTBCOD
057300970910     C     *LIKE         DEFINE    TBLKEY        KTBKEY
057400970910     C*---
057500970910     C* CHIAVI DI LETTURA
057600970910     C*---
057700970910     C* LETTURA AZORG01L - COMPLETA
057800000221     C     KEYORG        KLIST
057900970910     C                   KFLD                    KORFIL                         *FILIALE
058000970910     C* LETTURA TABEL00F - COMPLETA
058100970910     C     KEYTAB        KLIST
058200970910     C                   KFLD                    KTBKUT                         *CODICE UTENTE
058300970910     C                   KFLD                    KTBCOD                         *TABELLA
058400970910     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
058500970613     C*
058600970910     C* REPERIMENTO DATI UTENTE
058700970910     C                   CALL      'XFIL'
058800970910     C                   PARM                    DSFIL
058900970910     C*
059000970910     C* DEPOSITA LE OPZIONI DI ENTRATA
059100970910     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
059200970910     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
059300970703     C*
059400970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
059500970703IF  1C     D00VER        IFEQ      *ZEROS
059600970707     C                   Z-ADD     D00DRI        P96DRI            8 0
059700970707     C                   Z-ADD     *ZEROS        P96VER            5 0
059800970707     C                   CALL      'TISI96R'
059900970707     C                   PARM                    P96DRI
060000970707     C                   PARM                    P96VER
060100970707     C                   Z-ADD     P96VER        D00VER
060200970703E   1C                   ENDIF
060300970910     C*
060400970910     C* GESTIONE TASTI FUNZIONALI A VIDEO
060500970910     C                   Z-ADD     *ZEROS        FUNGRU            5 0
060600970910     C                   EXSR      FUNGES
060700970910     C*
060800970910     C* IMPOSTO LA VIDEATA
060900970910     C                   EXSR      AZZVID
061000970910     C*
061100970910     C                   ENDSR
061200970910     C*------------------------------------------------------------------------*
061300970910     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
061400970910     C*------------------------------------------------------------------------*
061500970910     C     FUNGES        BEGSR
061600970910     C*
061700970910     C* PULISCO CAMPI DI LAVORO
061800970910     C                   Z-ADD     0             W                 5 0
061900970910     C*
062000970910     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
062100970910     C* E CARICO TABELLA FUNZIONI ATTIVE
062200970910     C     1             DO        24            I
062300970910     C                   MOVEL     FUD(I)        FUDRIG
062400970910     C*
062500970910     C* PULIZIA FUNZIONI PRECEDENTI
062600970910     C                   MOVEL     *BLANKS       FUV(I)
062700970910     C*
062800970910     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
062900970910     C* (SALTA F24)
063000970910     C     FUDATT        IFEQ      'S'
063100970910     C     FUDFUN        ANDNE     '24'
063200970910     C                   ADD       1             W
063300970910     C                   MOVEL     FUDDES        FUV(W)
063400970910     C                   END
063500970910     C                   END                                                    DO WHILE
063600970910     C*
063700970910     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
063800970910     C* ALTRIMENTI DISATTIVO F24
063900970910     C     1             DO        24            I
064000970910     C                   MOVEL     FUD(I)        FUDRIG
064100970910     C*
064200970910     C     FUDFUN        IFEQ      '24'
064300970910     C     W             IFGT      6
064400970910     C                   MOVEL     'S'           FUDATT
064500970910     C                   ELSE
064600970910     C                   MOVEL     *BLANKS       FUDATT
064700970910     C                   END                                                    W GT 8
064800970910     C                   MOVEL     FUDRIG        FUD(I)
064900970910     C                   END
065000970910     C                   END                                                    DO WHILE
065100970910     C*
065200970910     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
065300970910     C     W             DIV       6             I
065400970910     C                   MVR                     Z
065500970910     C*
065600970910     C     Z             IFNE      0
065700970910     C     I             ADD       1             Z
065800970910     C                   ELSE
065900970910     C                   Z-ADD     I             Z                 5 0
066000970910     C                   END
066100970910     C*
066200970910     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
066300970910     C                   ADD       1             FUNGRU
066400970910     C     FUNGRU        IFGT      Z
066500970910     C     FUNGRU        ORLT      1
066600970910     C                   Z-ADD     1             FUNGRU
066700970910     C                   END
066800970910     C*
066900970910     C* PRIMO GRUPPO DI 6 FUNZIONI
067000970910     C     FUNGRU        IFEQ      1
067100970910     C                   MOVEL     FUV(1)        FUN001
067200970910     C                   MOVEL     FUV(2)        FUN002
067300970910     C                   MOVEL     FUV(3)        FUN003
067400970910     C                   MOVEL     FUV(4)        FUN004
067500970910     C                   MOVEL     FUV(5)        FUN005
067600970910     C                   MOVEL     FUV(6)        FUN006
067700970910     C                   END
067800970910     C*
067900970910     C* SECONDO GRUPPO DI 6 FUNZIONI
068000970910     C     FUNGRU        IFEQ      2
068100970910     C                   MOVEL     FUV(7)        FUN001
068200970910     C                   MOVEL     FUV(8)        FUN002
068300970910     C                   MOVEL     FUV(9)        FUN003
068400970910     C                   MOVEL     FUV(10)       FUN004
068500970910     C                   MOVEL     FUV(11)       FUN005
068600970910     C                   MOVEL     FUV(12)       FUN006
068700970910     C                   END
068800970910     C*
068900970910     C* TERZO GRUPPO DI 6 FUNZIONI
069000970910     C     FUNGRU        IFEQ      3
069100970910     C                   MOVEL     FUV(13)       FUN001
069200970910     C                   MOVEL     FUV(14)       FUN002
069300970910     C                   MOVEL     FUV(15)       FUN003
069400970910     C                   MOVEL     FUV(16)       FUN004
069500970910     C                   MOVEL     FUV(17)       FUN005
069600970910     C                   MOVEL     FUV(18)       FUN006
069700970910     C                   END
069800970910     C*
069900970910     C* QUARTO GRUPPO DI 6 FUNZIONI
070000970910     C     FUNGRU        IFEQ      4
070100970910     C                   MOVEL     FUV(19)       FUN001
070200970910     C                   MOVEL     FUV(20)       FUN002
070300970910     C                   MOVEL     FUV(21)       FUN003
070400970910     C                   MOVEL     FUV(22)       FUN004
070500970910     C                   MOVEL     *BLANKS       FUN005
070600970910     C                   MOVEL     *BLANKS       FUN006
070700970910     C                   END
070800970910     C*
070900970910     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
071000970910     C                   MOVEL     *BLANKS       FUN007
071100970910     C                   MOVEL     *BLANKS       FUN008
071200970910     C*
071300970910     C     1             DO        24            I
071400970910     C                   MOVEL     FUD(I)        FUDRIG
071500970910     C     FUDFUN        IFEQ      '24'
071600970910     C     FUDATT        ANDEQ     'S'
071700970910     C                   MOVEL     FUDDES        FUN008
071800970910     C                   END
071900970910     C                   END                                                    DO
072000970910     C*
072100970910     C                   ENDSR
072200970910     C*------------------------------------------------------------------------*
072300970910     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
072400970910     C*------------------------------------------------------------------------*
072500970910     C     FUNCON        BEGSR
072600970910     C*
072700970910     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
072800970910     C                   EXSR      FUNVAR
072900970910     C*
073000970910     C* PULISCO CAMPI DI LAVORO
073100970910     C                   Z-ADD     0             I                 5 0
073200970910     C                   Z-ADD     0             W                 5 0
073300970910     C*
073400970910     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
073500970910     C     FUNKEY        IFNE      *BLANKS
073600970910     C     1             DO        24            I
073700970910     C                   MOVEL     FUD(I)        FUDRIG
073800970910     C*
073900970910     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
074000970910     C     FUNKEY        IFEQ      FUDFUN
074100970910     C     FUDATT        ANDEQ     'S'
074200970910     C                   Z-ADD     I             W
074300970910     C                   END
074400970910     C                   END                                                    DO WHILE
074500970910     C*
074600970910     C* FUNZIONE NON ATTIVA (ERRORE)
074700970910     C     W             IFEQ      0
074800970910     C*
074900970910     C* EMISSIONE MESSAGGIO DI ERRORE
075000970910     C                   MOVEL     MSG(1)        DSMSMS
075100970910     C*
075200970910     C                   END                                                    W=0
075300970910     C                   END                                                    FUNKEY NE *BLANKS
075400970910     C*
075500970910     C* FUNZIONE ATTIVA (PROCEDI ...)
075600970910     C     W             IFNE      0
075700970910     C     FUNKEY        OREQ      *BLANKS
075800970910     C     FUNKEY        IFEQ      *BLANKS
075900970910     C                   EXSR      ENTGES
076000970910     C                   END
076100970910     C     FUNKEY        IFEQ      '03'
076200970910     C                   EXSR      F03GES
076300970910     C                   END
076400970910     C     FUNKEY        IFEQ      '06'
076500970910     C                   EXSR      F06GES
076600970910     C                   END
076700970910     C     FUNKEY        IFEQ      '12'
076800970910     C                   EXSR      F12GES
076900970910     C                   END
077000970910     C     FUNKEY        IFEQ      '24'
077100970910     C                   EXSR      F24GES
077200970910     C                   END
077300970910     C                   END
077400970910     C*
077500970910     C                   ENDSR
077600970910     C*------------------------------------------------------------------------*
077700970910     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
077800970910     C*------------------------------------------------------------------------*
077900970910     C     FUNVAR        BEGSR
078000970910     C*
078100970910     C* INIZIALIZZA VARIABILE -FUNKEY-
078200970910     C*
078300970910     C* FUNKEY=*BLANKS SIGNIFICA ENTER
078400970910     C     KEY           IFEQ      ENTER
078500970910     C                   MOVEL     *BLANKS       FUNKEY
078600970910     C                   END
078700970910     C     KEY           IFEQ      F1
078800970910     C                   MOVEL     '01'          FUNKEY
078900970910     C                   END
079000970910     C     KEY           IFEQ      F2
079100970910     C                   MOVEL     '02'          FUNKEY
079200970910     C                   END
079300970910     C     KEY           IFEQ      F3
079400970910     C                   MOVEL     '03'          FUNKEY
079500970910     C                   END
079600970910     C     KEY           IFEQ      F4
079700970910     C                   MOVEL     '04'          FUNKEY
079800970910     C                   END
079900970910     C     KEY           IFEQ      F5
080000970910     C                   MOVEL     '05'          FUNKEY
080100970910     C                   END
080200970910     C     KEY           IFEQ      F6
080300970910     C                   MOVEL     '06'          FUNKEY
080400970910     C                   END
080500970910     C     KEY           IFEQ      F7
080600970910     C                   MOVEL     '07'          FUNKEY
080700970910     C                   END
080800970910     C     KEY           IFEQ      F8
080900970910     C                   MOVEL     '08'          FUNKEY
081000970910     C                   END
081100970910     C     KEY           IFEQ      F9
081200970910     C                   MOVEL     '09'          FUNKEY
081300970910     C                   END
081400970910     C     KEY           IFEQ      F10
081500970910     C                   MOVEL     '10'          FUNKEY
081600970910     C                   END
081700970910     C     KEY           IFEQ      F11
081800970910     C                   MOVEL     '11'          FUNKEY
081900970910     C                   END
082000970910     C     KEY           IFEQ      F12
082100970910     C                   MOVEL     '12'          FUNKEY
082200970910     C                   END
082300970910     C     KEY           IFEQ      F13
082400970910     C                   MOVEL     '13'          FUNKEY
082500970910     C                   END
082600970910     C     KEY           IFEQ      F14
082700970910     C                   MOVEL     '14'          FUNKEY
082800970910     C                   END
082900970910     C     KEY           IFEQ      F15
083000970910     C                   MOVEL     '15'          FUNKEY
083100970910     C                   END
083200970910     C     KEY           IFEQ      F16
083300970910     C                   MOVEL     '16'          FUNKEY
083400970910     C                   END
083500970910     C     KEY           IFEQ      F17
083600970910     C                   MOVEL     '17'          FUNKEY
083700970910     C                   END
083800970910     C     KEY           IFEQ      F18
083900970910     C                   MOVEL     '18'          FUNKEY
084000970910     C                   END
084100970910     C     KEY           IFEQ      F19
084200970910     C                   MOVEL     '19'          FUNKEY
084300970910     C                   END
084400970910     C     KEY           IFEQ      F20
084500970910     C                   MOVEL     '20'          FUNKEY
084600970910     C                   END
084700970910     C     KEY           IFEQ      F21
084800970910     C                   MOVEL     '21'          FUNKEY
084900970910     C                   END
085000970910     C     KEY           IFEQ      F22
085100970910     C                   MOVEL     '22'          FUNKEY
085200970910     C                   END
085300970910     C     KEY           IFEQ      F23
085400970910     C                   MOVEL     '23'          FUNKEY
085500970910     C                   END
085600970910     C     KEY           IFEQ      F24
085700970910     C                   MOVEL     '24'          FUNKEY
085800970910     C                   END
085900970910     C     KEY           IFEQ      ROLLUP
086000970910     C                   MOVEL     'RU'          FUNKEY
086100970910     C                   END
086200970910     C     KEY           IFEQ      HELP
086300970910     C                   MOVEL     'HP'          FUNKEY
086400970910     C                   END
086500970910     C                   ENDSR
086600970910     C*------------------------------------------------------------------------*
086700970910     C* NOTE COSTRUZIONE SCHIERE
086800970910     C*------------------------------------------------------------------------*
086900970910     C*
087000970910     C* -MSG-MESSAGGI
087100970910     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
087200970910     C* (NON TOCCARE MSG,1 E MSG,2)
087300970910     C*
087400970910     C* -CMD-COMANDI
087500970910     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
087600970910     C*
087700970910     C* -OPD-OPZIONI
087800970910     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
087900970910     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
088000970910     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
088100970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088200970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088300970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
088400970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088500970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
088600970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088700970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
088800970910     C*
088900970910     C* -FUD-FUNZIONI
089000970910     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
089100970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
089200970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
089300970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
089400970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
089500970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
089600970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
089700970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
089800970910     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
089900970910     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
090000970910     O*------------------------------------------------------------------------*
090100970910** MSG - MESSAGGI
090200000221TISI08R- Opzione non gestita                                                   1
090300000221TISI08R- Tasto funzionale non gestito                                          2
090400000221TISI08R- 3
090500000221TISI08R- 4
090600000221TISI08R- 5
090700000221TISI08R- 6
090800000221TISI08R- 7
090900000221TISI08R- 8
091000000221TISI08R- 9
091100000221TISI08R- 10
091200000221TISI08R- Immettere uno dei valori possibili                                   11
091300000221TISI08R- Tempo consegna errato                                                12
091400000221TISI08R- Nazione errata                                                       13
091500000221TISI08R- Se richiesto il Dal immettere anche l'AL e viceversa                 14
091600000221TISI08R- Il Dal deve essere inferiore o uguale all'AL                         15
091700000221TISI08R- Provincia errata                                                     16
091800000221TISI08R- Linea di arrivo errata                                               17
091900000221TISI08R- Tipo inoltro errato                                                  18
092000000221TISI08R- Immettere 'S' per stampare le località gestite a livello di località 19
092100000221TISI08R- Stampa lanciata ...                                                  20
092200970610** CMD - COMANDI CL
092300970610WRKSPLF
092400970610
092500970610
092600970610
092700970610
092800970910** FUD - FUNZIONI
092900970910                   -        (S/N)=N                        funz.01¬
093000970910                   -        (S/N)=N                        funz.02¬
093100970910F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
093200970910                   -        (S/N)=N                        funz.04¬
093300970910                   -        (S/N)=N                        funz.05¬
093400970610F06=Conferma       -        (S/N)=S                        funz.06¬
093500970910                   -        (S/N)=N                        funz.07¬
093600970910                   -        (S/N)=N                        funz.08¬
093700970910                   -        (S/N)=N                        funz.09¬
093800970910                   -        (S/N)=N                        funz.10¬
093900970910                   -        (S/N)=N                        funz.11¬
094000970910F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
094100970910                   -        (S/N)=N                        funz.13¬
094200970910                   -        (S/N)=N                        funz.14¬
094300970910                   -        (S/N)=N                        funz.15¬
094400970910                   -        (S/N)=N                        funz.16¬
094500970910                   -        (S/N)=N                        funz.17¬
094600970910                   -        (S/N)=N                        funz.18¬
094700970910                   -        (S/N)=N                        funz.19¬
094800970910                   -        (S/N)=N                        funz.20¬
094900970910                   -        (S/N)=N                        funz.21¬
095000970910                   -        (S/N)=N                        funz.22¬
095100970910                   -        (S/N)=N                        funz.23¬
095200970910F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
