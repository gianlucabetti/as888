000100970910     H*------------------------------------------------------------------------*
000200970610     H* CAPPARIO - STAMPA CAPPARIO - RICHIESTA PARAMETRI
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800970910     FTISI04D   CF   E             WORKSTN INFDS(DEVDS1)
000900970910     F                                     SFILE(SI04DMS:MSGULT)
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
008500970910     D  PARTOR                 1      1
008600970611     D  PARCA1                 2     10
008700970611     D  PARCA2                11     19
008800970611     D  PARNA1                20     22
008900970611     D  PARNA2                23     25
009000970611     D  PARPR1                26     27
009100970611     D  PARPR2                28     29
009200970611     D  PARLN1                30     32  0
009300970611     D  PARLN2                33     35  0
009400970612     D  PARISO                36     36
009500970613     D  PARTTC                37     41  0
009600970613     D  PARTEM                42     42
009700970613     D  PARTIC                43     43
009800970613     D  PARLOC                44     44
009900970613     D  PARLIV                45     45
010000970613     D  PARVER                46     50  0
010100970613     D  PARDRI                51     58  0
010200971003     D  PAROBS                59     59
010300970910     D*-------------
010400970910     D* ARCHITETTURA
010500970910     D*------------
010600970910     D KPJBA         E DS
010700061017      * - Parametri x Controllo profilo utenti
010800061017     d TIBS34ds      e ds
010900061017      * - Ds di riferimento al file esterno AZUTE00F
011000061017     d AZUTEds       e ds                  extname(AZUTE00F)
011100061017      * - Ds per dati organigramma
011200061017     d dDATIUTE      e ds
011300970910     C*------------------------------------------------------------------------*
011400970910     C* MAIN LINES
011500970910     C*------------------------------------------------------------------------*
011600970910     C*
011700970910     C* NON CI SONO ERRORI IN ENTRATA
011800970910IF  1C     ERRL00        IFEQ      '0'
011900970910     C*
012000970910     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012100970910DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012200970910     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012300970910     C     FINL00        OREQ      '1'                                          *FINE PGM
012400970910     C*
012500970910     C* EMETTE FORMATO VIDEO
012600970910     C                   EXSR      BUTFMR
012700970910     C*
012800970910     C* GESTIONE TASTI FUNZIONALI
012900970910     C                   EXSR      FUNCON
013000970910E   2C                   ENDDO
013100970910     C*
013200970910     C* CI SONO ERRORI IN ENTRATA
013300970910X   1C                   ELSE
013400970910     C                   MOVEL     '1'           D00ERR
013500970910     C                   MOVEL     DSMSMS        D00MSG
013600970910E   1C                   ENDIF
013700970910     C* FINE PGM
013800970910     C                   SETON                                        LR
013900970910     C*------------------------------------------------------------------------*
014000970910     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014100970910     C*------------------------------------------------------------------------*
014200970910     C     BUTFMR        BEGSR
014300970910     C*
014400970910     C* EMETTE LA TESTATA
014500970910     C                   WRITE     SI04D01
014600970910     C*
014700970910     C* SPEDISCE EVENTUALE MESSAGGIO
014800970910     C                   EXSR      SNDMSG
014900970910     C*
015000970910     C* EMETTE LE FUNZIONI
015100970910     C                   WRITE     SI04D02
015200970910     C*
015300970910     C* EMETTE IL VIDEO
015400970910     C                   EXFMT     SI04D03
015500970910     C*
015600970910     C* REIMPOSTA FLAG DI ERRORE
015700970910     C                   MOVEL     '0'           ERRL00
015800970910     C*
015900970910     C* REIMPOSTA INDICATORI DI ERRORE
016000970910     C                   SETOFF                                         5859
016100970910     C                   SETOFF                                       606162
016200970910     C                   SETOFF                                       636465
016300970910     C                   SETOFF                                       666768
016400970910     C                   SETOFF                                       697071
016500970910     C                   SETOFF                                       727374
016600970910     C                   SETOFF                                       757677
016700970910     C                   SETOFF                                       787980
016800970910     C                   SETOFF                                       818283
016900970910     C                   SETOFF                                       848586
017000970910     C                   SETOFF                                       878889
017100970910     C*
017200970910     C                   ENDSR
017300970910     C*------------------------------------------------------------------------*
017400970910     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017500970910     C*------------------------------------------------------------------------*
017600970910     C     SNDMSG        BEGSR
017700970910     C*
017800970910     C* PULISCE IL SFILE MESSAGGI
017900970910     C                   MOVEL     '1'           *IN40
018000970910     C                   WRITE     SI04DMC
018100970910     C                   MOVEL     '0'           *IN40
018200970910     C*
018300970910     C* VISUALIZZA IL MESSAGGIO
018400970910     C                   Z-ADD     *ZEROS        MSGULT            4 0
018500970910     C                   CALL      'XMSG'        XMSGPA
018600970910IF  1C     DSMSER        IFEQ      *ZEROS
018700970910     C                   MOVEL     DSMSKE        MSGKEY
018800970910E   1C                   ENDIF
018900970910     C                   Z-ADD     1             MSGULT
019000970910     C                   WRITE     SI04DMS
019100970910     C                   WRITE     SI04DMC
019200970910     C*
019300970910     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019400970910     C                   MOVEL     *BLANKS       DSMSMS
019500970910     C*
019600970910     C                   ENDSR
019700970910     C*------------------------------------------------------------------------*
019800970910     C* ENTGES - GESTIONE ENTER -> INVIO
019900970910     C*------------------------------------------------------------------------*
020000970910     C     ENTGES        BEGSR
020100970910     C*
020200970910     C* CONTROLLA I CAMPI IMMESSI
020300970910     C                   EXSR      DECODI
020400970910     C*
020500970910     C                   ENDSR
020600970910     C*------------------------------------------------------------------------*
020700970910     C* F03GES - GESTIONE F03 -> FINE
020800970910     C*------------------------------------------------------------------------*
020900970910     C     F03GES        BEGSR
021000970910     C*
021100970910     C* IMPOSTA "PREMUTO F03"
021200970910     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021300970910     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
021400970910     C*
021500970910     C                   ENDSR
021600970910     C*------------------------------------------------------------------------*
021700970623     C* F06GES - GESTIONE F06 -> CONFERMA
021800970910     C*------------------------------------------------------------------------*
021900970723     C     F06GES        BEGSR
022000970723     C*
022100970723     C* CONTROLLA I CAMPI IMMESSI
022200970723     C                   EXSR      DECODI
022300970910     C*
022400970910     C* LANCIA IL PROGRAMMA DI STAMPA
022500970723IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022600970910     C                   MOVEL     V1CTOR        PARTOR
022700970910     C                   MOVEL     V1CNA1        PARNA1
022800970910     C                   MOVEL     V1CNA2        PARNA2
022900970910     C                   MOVEL     V1CCA1        PARCA1
023000970910     C                   MOVEL     V1CCA2        PARCA2
023100970910     C                   MOVEL     V1CPR1        PARPR1
023200970910     C                   MOVEL     V1CPR2        PARPR2
023300970910     C                   MOVEL     V1CLN1        PARLN1
023400970910     C                   MOVEL     V1CLN2        PARLN2
023500970612     C                   MOVEL     V1CISO        PARISO
023600970613     C                   Z-ADD     V1CTTC        PARTTC
023700970612     C                   MOVEL     V1CTEM        PARTEM
023800970910     C                   MOVEL     V1CTIC        PARTIC
023900970910     C                   MOVEL     V1CLOC        PARLOC
024000970612     C                   MOVEL     V1CLIV        PARLIV
024100971003     C                   MOVEL     V1COBS        PAROBS
024200970611     C                   Z-ADD     D00VER        PARVER
024300970611     C                   Z-ADD     D00DRI        PARDRI
024400970910     C                   MOVEL     PARAM         KPJBU
024500970910     C                   MOVEL     'SI05'        KCOAZ
024600061016     c                   if        KNMUS = *all'1'
024700061016 MOM c                   call      'TISI05R'
024800061016     c                   parm                    KPJBA
024900061016     c                   else
025000010723     C                   CALL      'BCH10'
025100970910     C                   PARM                    KPJBA
025200061016     c                   endif
025300970619     C                   MOVEL     '1'           FINL00
025400970723E   1C                   ENDIF
025500970910     C*
025600970910     C                   ENDSR
025700970910     C*------------------------------------------------------------------------*
025800970910     C* F12GES - GESTIONE F12 -> RITORNO
025900970910     C*------------------------------------------------------------------------*
026000970910     C     F12GES        BEGSR
026100970910     C*
026200970910     C* IMPOSTA "PREMUTO F12"
026300970910     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026400970910     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
026500970910     C*
026600970910     C                   ENDSR
026700970910     C*------------------------------------------------------------------------*
026800970910     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
026900970910     C*------------------------------------------------------------------------*
027000970910     C     F22GES        BEGSR
027100970910     C*
027200970910     C                   Z-ADD     80            LENGH            15 5
027300970910     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
027400970910     C                   PARM                    CMD(1)
027500970910     C                   PARM                    LENGH
027600970910     C*
027700970910     C                   ENDSR
027800970910     C*------------------------------------------------------------------------*
027900970910     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028000970910     C*------------------------------------------------------------------------*
028100970910     C     F24GES        BEGSR
028200970910     C*
028300970910     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028400970910     C*
028500970910     C                   ENDSR
028600970910     C*------------------------------------------------------------------------*
028700970910     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028800970910     C*------------------------------------------------------------------------*
028900970910     C     DECODI        BEGSR
029000970910     C*--------------------
029100970910     C* ORDINAMENTO
029200970910     C*--------------------
029300970910IF  1C     ERRL00        IFEQ      '0'
029400970910IF  2C     V1CTOR        IFEQ      'C'
029500970910     C     V1CTOR        OREQ      'P'
029600970910     C     V1CTOR        OREQ      'L'
029700970910X   2C                   ELSE
029800970910     C                   SETON                                        81
029900970910     C                   MOVEL     MSG(11)       DSMSMS
030000970910     C                   MOVEL     '1'           ERRL00
030100970910E   2C                   ENDIF
030200970910E   1C                   ENDIF
030300970910     C*--------------------
030400970910     C* NAZIONE DAL
030500970910     C*--------------------
030600970910IF  1C     ERRL00        IFEQ      '0'
030700970910     C                   MOVEL     *BLANKS       V1DNA1
030800970910IF  2C     V1CNA1        IFNE      *BLANKS
030900970910     C                   MOVEL     V1CNA1        WNAR
031000970910     C                   MOVEL     *BLANKS       WDNAR
031100970910     C                   EXSR      CHKNAR
031200970910     C                   MOVEL     WNAR          V1CNA1
031300970910     C                   MOVEL     WDNAR         V1DNA1
031400970610IF  3C     ERRL00        IFEQ      '1'
031500970910     C                   SETON                                        76
031600970910     C                   MOVEL     MSG(13)       DSMSMS
031700970610E   3C                   ENDIF
031800970610E   2C                   ENDIF
031900970910E   1C                   ENDIF
032000970910     C*--------------------
032100970910     C* NAZIONE AL
032200970910     C*--------------------
032300970910IF  1C     ERRL00        IFEQ      '0'
032400970910     C                   MOVEL     *BLANKS       V1DNA2
032500970910IF  2C     V1CNA2        IFNE      *BLANKS
032600970910     C                   MOVEL     V1CNA2        WNAR
032700970910     C                   MOVEL     *BLANKS       WDNAR
032800970910     C                   EXSR      CHKNAR
032900970910     C                   MOVEL     WNAR          V1CNA2
033000970910     C                   MOVEL     WDNAR         V1DNA2
033100970610IF  3C     ERRL00        IFEQ      '1'
033200970910     C                   SETON                                        77
033300970910     C                   MOVEL     MSG(13)       DSMSMS
033400970610E   3C                   ENDIF
033500970610E   2C                   ENDIF
033600970910E   1C                   ENDIF
033700970910     C*--------------------
033800970910     C* NAZIONE DAL+AL
033900970910     C*--------------------
034000970910     C* SE DAL ANCHE AL E VICEVERSA
034100970910IF  1C     ERRL00        IFEQ      '0'
034200970910IF  2C     V1CNA1        IFNE      *BLANKS
034300970910     C     V1CNA2        ANDEQ     *BLANKS
034400970910     C     V1CNA2        ORNE      *BLANKS
034500970910     C     V1CNA1        ANDEQ     *BLANKS
034600970910     C                   SETON                                        7677
034700970910     C                   MOVEL     MSG(14)       DSMSMS
034800970910     C                   MOVEL     '1'           ERRL00
034900970910E   2C                   ENDIF
035000970910E   1C                   ENDIF
035100970910     C* DAL <= AL
035200970910IF  1C     ERRL00        IFEQ      '0'
035300970910IF  2C     V1CNA1        IFNE      *BLANKS
035400970910IF  3C     V1CNA1        IFGT      V1CNA2
035500970910     C                   SETON                                        7677
035600970910     C                   MOVEL     MSG(15)       DSMSMS
035700970910     C                   MOVEL     '1'           ERRL00
035800970910E   3C                   ENDIF
035900970910E   2C                   ENDIF
036000970910E   1C                   ENDIF
036100970910     C*--------------------
036200970910     C* CAP DAL+AL
036300970910     C*--------------------
036400970910     C* SE DAL ANCHE AL E VICEVERSA
036500970910IF  1C     ERRL00        IFEQ      '0'
036600970910IF  2C     V1CCA1        IFNE      *BLANKS
036700970910     C     V1CCA2        ANDEQ     *BLANKS
036800970910     C     V1CCA2        ORNE      *BLANKS
036900970910     C     V1CCA1        ANDEQ     *BLANKS
037000970910     C                   SETON                                        7071
037100970910     C                   MOVEL     MSG(14)       DSMSMS
037200970910     C                   MOVEL     '1'           ERRL00
037300970910E   2C                   ENDIF
037400970910E   1C                   ENDIF
037500970910     C* DAL <= AL
037600970910IF  1C     ERRL00        IFEQ      '0'
037700970910IF  2C     V1CCA1        IFNE      *BLANKS
037800970910IF  3C     V1CCA1        IFGT      V1CCA2
037900970910     C                   SETON                                        7071
038000970910     C                   MOVEL     MSG(15)       DSMSMS
038100970910     C                   MOVEL     '1'           ERRL00
038200970910E   3C                   ENDIF
038300970910E   2C                   ENDIF
038400970910E   1C                   ENDIF
038500970910     C*--------------------
038600970910     C* PROVINCIA DAL
038700970910     C*--------------------
038800970910IF  1C     ERRL00        IFEQ      '0'
038900970910     C                   MOVEL     *BLANKS       V1DPR1
039000970910IF  2C     V1CPR1        IFNE      *BLANKS
039100970910     C                   MOVEL     V1CPR1        WPRV
039200970910     C                   MOVEL     *BLANKS       WDPRV
039300970910     C                   EXSR      CHKPRV
039400970910     C                   MOVEL     WPRV          V1CPR1
039500970910     C                   MOVEL     WDPRV         V1DPR1
039600970910IF  3C     ERRL00        IFEQ      '1'
039700970910     C                   SETON                                        72
039800970910     C                   MOVEL     MSG(16)       DSMSMS
039900970910E   3C                   ENDIF
040000970910E   2C                   ENDIF
040100970910E   1C                   ENDIF
040200970910     C*--------------------
040300970910     C* PROVINCIA AL
040400970910     C*--------------------
040500970910IF  1C     ERRL00        IFEQ      '0'
040600970910     C                   MOVEL     *BLANKS       V1DPR2
040700970910IF  2C     V1CPR2        IFNE      *BLANKS
040800970910     C                   MOVEL     V1CPR2        WPRV
040900970910     C                   MOVEL     *BLANKS       WDPRV
041000970910     C                   EXSR      CHKPRV
041100970910     C                   MOVEL     WPRV          V1CPR2
041200970910     C                   MOVEL     WDPRV         V1DPR2
041300970910IF  3C     ERRL00        IFEQ      '1'
041400970910     C                   SETON                                        73
041500970910     C                   MOVEL     MSG(16)       DSMSMS
041600970910E   3C                   ENDIF
041700970910E   2C                   ENDIF
041800970910E   1C                   ENDIF
041900970910     C*--------------------
042000970910     C* PROVINCIA DAL+AL
042100970910     C*--------------------
042200970910     C* SE DAL ANCHE AL E VICEVERSA
042300970910IF  1C     ERRL00        IFEQ      '0'
042400970910IF  2C     V1CPR1        IFNE      *BLANKS
042500970910     C     V1CPR2        ANDEQ     *BLANKS
042600970910     C     V1CPR2        ORNE      *BLANKS
042700970910     C     V1CPR1        ANDEQ     *BLANKS
042800970910     C                   SETON                                        7273
042900970910     C                   MOVEL     MSG(14)       DSMSMS
043000970910     C                   MOVEL     '1'           ERRL00
043100970910E   2C                   ENDIF
043200970910E   1C                   ENDIF
043300970910     C* DAL <= AL
043400970910IF  1C     ERRL00        IFEQ      '0'
043500970910IF  2C     V1CPR1        IFNE      *BLANKS
043600970910IF  3C     V1CPR1        IFGT      V1CPR2
043700970910     C                   SETON                                        7273
043800970910     C                   MOVEL     MSG(15)       DSMSMS
043900970910     C                   MOVEL     '1'           ERRL00
044000970910E   3C                   ENDIF
044100970910E   2C                   ENDIF
044200970910E   1C                   ENDIF
044300970910     C*--------------------
044400970910     C* LINEA ARRIVO DAL
044500970910     C*--------------------
044600970910IF  1C     ERRL00        IFEQ      '0'
044700970910     C                   MOVEL     *BLANKS       V1DLN1
044800970910IF  2C     V1CLN1        IFNE      *BLANKS
044900970910     C                   MOVEL     V1CLN1        WLIN
045000970910     C                   MOVEL     *BLANKS       WDLIN
045100970910     C                   EXSR      CHKLIN
045200970910     C                   MOVEL     WLIN          V1CLN1
045300970910     C                   MOVEL     WDLIN         V1DLN1
045400970910IF  3C     ERRL00        IFEQ      '1'
045500970910     C                   SETON                                        74
045600970910     C                   MOVEL     MSG(17)       DSMSMS
045700970910E   3C                   ENDIF
045800970910E   2C                   ENDIF
045900970910E   1C                   ENDIF
046000970910     C*--------------------
046100970910     C* LINEA ARRIVO AL
046200970910     C*--------------------
046300970910IF  1C     ERRL00        IFEQ      '0'
046400970910     C                   MOVEL     *BLANKS       V1DLN2
046500970910IF  2C     V1CLN2        IFNE      *BLANKS
046600970910     C                   MOVEL     V1CLN2        WLIN
046700970910     C                   MOVEL     *BLANKS       WDLIN
046800970910     C                   EXSR      CHKLIN
046900970910     C                   MOVEL     WLIN          V1CLN2
047000970910     C                   MOVEL     WDLIN         V1DLN2
047100970910IF  3C     ERRL00        IFEQ      '1'
047200970910     C                   SETON                                        75
047300970910     C                   MOVEL     MSG(17)       DSMSMS
047400970910E   3C                   ENDIF
047500970910E   2C                   ENDIF
047600970910E   1C                   ENDIF
047700970910     C*--------------------
047800970910     C* LINEA ARRIVO DAL+AL
047900970910     C*--------------------
048000970910     C* SE DAL ANCHE AL E VICEVERSA
048100970910IF  1C     ERRL00        IFEQ      '0'
048200970910IF  2C     V1CLN1        IFNE      *BLANKS
048300970910     C     V1CLN2        ANDEQ     *BLANKS
048400970910     C     V1CLN2        ORNE      *BLANKS
048500970910     C     V1CLN1        ANDEQ     *BLANKS
048600970910     C                   SETON                                        7475
048700970910     C                   MOVEL     MSG(14)       DSMSMS
048800970910     C                   MOVEL     '1'           ERRL00
048900970910E   2C                   ENDIF
049000970910E   1C                   ENDIF
049100970910     C* DAL <= AL
049200970910IF  1C     ERRL00        IFEQ      '0'
049300970910IF  2C     V1CLN1        IFNE      *BLANKS
049400970910IF  3C     V1CLN1        IFGT      V1CLN2
049500970910     C                   SETON                                        7475
049600970910     C                   MOVEL     MSG(15)       DSMSMS
049700970910     C                   MOVEL     '1'           ERRL00
049800970910E   3C                   ENDIF
049900970910E   2C                   ENDIF
050000970910E   1C                   ENDIF
050100970612     C*--------------------
050200970612     C* TIPO INOLTRO
050300970612     C*--------------------
050400970612IF  1C     ERRL00        IFEQ      '0'
050500970612     C                   MOVEL     *BLANKS       V1DISO
050600970612IF  2C     V1CISO        IFNE      *BLANKS
050700970612     C                   MOVEL     V1CISO        WINL
050800970612     C                   MOVEL     *BLANKS       WDINL
050900970612     C                   EXSR      CHKINL
051000970612     C                   MOVEL     WINL          V1CISO
051100970612     C                   MOVEL     WDINL         V1DISO
051200970612IF  3C     ERRL00        IFEQ      '1'
051300970612     C                   SETON                                        82
051400970613     C                   MOVEL     MSG(18)       DSMSMS
051500970612E   3C                   ENDIF
051600970612E   2C                   ENDIF
051700970612E   1C                   ENDIF
051800970613     C*--------------------
051900970613     C* TIPO TEMPO CONSEGNA
052000970613     C*--------------------
052100970613IF  1C     ERRL00        IFEQ      '0'
052200970613IF  2C     V1CTTC        IFGT      *ZEROS
052300970613     C                   Z-ADD     D00VER        KCTVER
052400970613     C                   Z-ADD     V1CTTC        KCTTTC
052500970613     C     KEYCPT        CHAIN     AZCPT01L                           99
052600970613IF  3C     *IN99         IFEQ      *OFF
052700970613     C     CPTATB        ANDEQ     *BLANKS
052800970613X   3C                   ELSE
052900970613     C                   SETON                                        84
053000970613     C                   MOVEL     MSG(12)       DSMSMS
053100970613     C                   MOVEL     '1'           ERRL00
053200970613E   3C                   ENDIF
053300970613E   2C                   ENDIF
053400970613E   1C                   ENDIF
053500970910     C*--------------------
053600970613     C* STAMPA TEMPI
053700970910     C*--------------------
053800970910IF  1C     ERRL00        IFEQ      '0'
053900970910IF  2C     V1CTEM        IFEQ      'S'
054000970910     C     V1CTEM        OREQ      'N'
054100970910X   2C                   ELSE
054200970910     C                   SETON                                        78
054300970910     C                   MOVEL     MSG(11)       DSMSMS
054400970910     C                   MOVEL     '1'           ERRL00
054500970910E   2C                   ENDIF
054600970910E   1C                   ENDIF
054700970910     C*--------------------
054800970910     C* TEMPI INTERNI O CONSEGNA
054900970910     C*--------------------
055000970910IF  1C     ERRL00        IFEQ      '0'
055100970910IF  2C     V1CTIC        IFEQ      'I'
055200970910     C     V1CTIC        OREQ      'C'
055300970910     C     V1CTIC        OREQ      'T'
055400970910X   2C                   ELSE
055500970910     C                   SETON                                        79
055600970910     C                   MOVEL     MSG(11)       DSMSMS
055700970910     C                   MOVEL     '1'           ERRL00
055800970910E   2C                   ENDIF
055900970910E   1C                   ENDIF
056000970910     C*--------------------
056100970613     C* STAMPA LOCALITA'
056200970910     C*--------------------
056300970910IF  1C     ERRL00        IFEQ      '0'
056400970910IF  2C     V1CLOC        IFEQ      'S'
056500970910     C     V1CLOC        OREQ      'N'
056600970611     C     V1CLOC        OREQ      'D'
056700970910X   2C                   ELSE
056800970910     C                   SETON                                        80
056900970910     C                   MOVEL     MSG(11)       DSMSMS
057000970910     C                   MOVEL     '1'           ERRL00
057100970910E   2C                   ENDIF
057200970910E   1C                   ENDIF
057300970612     C*--------------------
057400970613     C* SOLO LOCALITA' DIVERSE DAL CAP
057500970612     C*--------------------
057600970612IF  1C     ERRL00        IFEQ      '0'
057700970612IF  2C     V1CLIV        IFEQ      'S'
057800970612     C     V1CLIV        OREQ      'N'
057900970612X   2C                   ELSE
058000970612     C                   SETON                                        83
058100970612     C                   MOVEL     MSG(19)       DSMSMS
058200970612     C                   MOVEL     '1'           ERRL00
058300970612E   2C                   ENDIF
058400970612E   1C                   ENDIF
058500971003     C*--------------------
058600971003     C* STAMPA DATI OBSOLETI
058700971003     C*--------------------
058800971003IF  1C     ERRL00        IFEQ      '0'
058900971003IF  2C     V1COBS        IFEQ      'S'
059000971003     C     V1COBS        OREQ      'N'
059100971003X   2C                   ELSE
059200971003     C                   SETON                                        85
059300971003     C                   MOVEL     MSG(11)       DSMSMS
059400971003     C                   MOVEL     '1'           ERRL00
059500971003E   2C                   ENDIF
059600971003E   1C                   ENDIF
059700970910     C*
059800970910     C                   ENDSR
059900970910     C*------------------------------------------------------------------------*
060000970910     C* CHKNAR - CONTROLLA LA NAZIONE
060100970910     C*------------------------------------------------------------------------*
060200970910     C     CHKNAR        BEGSR
060300970910     C*
060400970910     C     *LIKE         DEFINE    V1CNA1        WNAR
060500970910     C     *LIKE         DEFINE    V1DNA1        WDNAR
060600970910     C*
060700970910     C* RICHIESTO ELENCO
060800970910     C                   SETOFF                                           99
060900970910     C     '?'           SCAN      WNAR                                   99
061000970910IF  1C     *IN99         IFEQ      *ON
061100020906     C                   Z-ADD     1             §KUT              1 0
061200970910     C                   MOVEL     '15'          §COD              2
061300970910     C                   MOVEL     *BLANKS       §KEY              8
061400970910     C                   MOVEL     *BLANKS       §DES             30
061500970910     C                   CALL      'X§TABER'
061600970910     C                   PARM                    §KUT
061700970910     C                   PARM                    §COD
061800970910     C                   PARM                    §KEY
061900970910     C                   PARM                    §DES
062000970910     C                   MOVEL     §KEY          WNAR
062100970910     C                   MOVEL     §DES          WDNAR
062200970910E   1C                   ENDIF
062300970910     C*
062400970910     C* CONTROLLA ESISTENZA CODICE
062500020906     C                   MOVEL     1             KTBKUT
062600970910     C                   MOVEL     '15'          KTBCOD
062700970910     C                   MOVEL(P)  WNAR          KTBKEY
062800970910     C     KEYTAB        CHAIN     TABEL00F                           99
062900970910IF  1C     *IN99         IFEQ      *OFF
063000970910     C     TBLFLG        ANDEQ     *BLANKS
063100970910     C                   MOVEL     TBLUNI        DS15
063200970910     C                   MOVEL     §15DES        WDNAR
063300970910X   1C                   ELSE
063400970611IF  2C     WNAR          IFEQ      'I  '
063500970611     C                   MOVEL(P)  'ITALIA'      WDNAR
063600970611X   2C                   ELSE
063700970910     C                   MOVEL     '1'           ERRL00
063800970611E   2C                   ENDIF
063900970611E   1C                   ENDIF
064000970910     C*
064100970910     C                   ENDSR
064200970910     C*------------------------------------------------------------------------*
064300970910     C* CHKPRV - CONTROLLA LA PROVINCIA
064400970910     C*------------------------------------------------------------------------*
064500970910     C     CHKPRV        BEGSR
064600970910     C*
064700970910     C     *LIKE         DEFINE    V1CPR1        WPRV
064800970910     C     *LIKE         DEFINE    V1DPR1        WDPRV
064900970910     C*
065000970910     C* RICHIESTO ELENCO
065100970910     C                   SETOFF                                           99
065200970910     C     '?'           SCAN      WPRV                                   99
065300970910IF  1C     *IN99         IFEQ      *ON
065400020906     C                   Z-ADD     1             §KUT              1 0
065500970910     C                   MOVEL     'PR'          §COD              2
065600970910     C                   MOVEL     *BLANKS       §KEY              8
065700970910     C                   MOVEL     *BLANKS       §DES             30
065800970910     C                   CALL      'X§TABER'
065900970910     C                   PARM                    §KUT
066000970910     C                   PARM                    §COD
066100970910     C                   PARM                    §KEY
066200970910     C                   PARM                    §DES
066300970910     C                   MOVEL     §KEY          WPRV
066400970910     C                   MOVEL     §DES          WDPRV
066500970910E   1C                   ENDIF
066600970910     C*
066700970910     C* CONTROLLA ESISTENZA CODICE
066800020906     C                   MOVEL     1             KTBKUT
066900970910     C                   MOVEL     'PR'          KTBCOD
067000970910     C                   MOVEL(P)  WPRV          KTBKEY
067100970910     C     KEYTAB        CHAIN     TABEL00F                           99
067200970910IF  1C     *IN99         IFEQ      *OFF
067300970910     C     TBLFLG        ANDEQ     *BLANKS
067400970910     C                   MOVEL     TBLUNI        DSPR
067500970910     C                   MOVEL     §PRDES        WDPRV
067600970910X   1C                   ELSE
067700970910     C                   MOVEL     '1'           ERRL00
067800970910E   1C                   ENDIF
067900970910     C*
068000970910     C                   ENDSR
068100970910     C*------------------------------------------------------------------------*
068200970910     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
068300970910     C*------------------------------------------------------------------------*
068400970910     C     CHKLIN        BEGSR
068500970910     C*
068600970610     C     *LIKE         DEFINE    V1CLN1        WLIN
068700970610     C     *LIKE         DEFINE    V1DLN1        WDLIN
068800970910     C*
068900970910     C* RICHIESTO ELENCO
069000970910     C                   SETOFF                                           99
069100970910     C     '?'           SCAN      WLIN                                   99
069200970910IF  1C     *IN99         IFEQ      *ON
069300970910     C                   MOVEL     *BLANKS       §COD1             3
069400970910     C                   MOVEL     *BLANKS       §TIP              1
069500970910     C                   MOVEL     *BLANKS       §DES1            25
069600971203     C                   MOVEL     *BLANKS       §PRA              3
069700971203     C                   CALL      'TNSD24R'
069800970910     C                   PARM                    §COD1
069900970910     C                   PARM                    §TIP
070000970910     C                   PARM                    §DES1
070100971203     C                   PARM                    §PRA
070200970910     C                   MOVEL     §COD1         WLIN
070300970910     C                   MOVEL     §DES1         WDLIN
070400970910E   1C                   ENDIF
070500970910     C*
070600970910     C* SOLO CARATTERI NUMERICI
070700970910     C                   SETOFF                                       99
070800970910     C                   TESTN                   WLIN                 99
070900970910IF  1C     *IN99         IFEQ      *OFF                                         *NON E'NUMERO
071000970910     C                   MOVEL     '1'           ERRL00
071100970910X   1C                   ELSE                                                   *E'NUMERO DA TESTN
071200970910     C                   MOVE      WLIN          W001              1
071300970910IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
071400970910     C                   MOVEL     '1'           ERRL00
071500970910X   2C                   ELSE                                                   *NUMERO
071600970910     C*
071700970910     C* CONTROLLA ESISTENZA CODICE
071800970910     C                   MOVEL     WLIN          KORFIL
071900970910     C     KEYORG        CHAIN     AZORG01L                           99
072000970910IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
072100970910     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
072200970910     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
072300970910     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
072400970910X   3C                   ELSE
072500970910     C                   MOVEL     '1'           ERRL00
072600970910E   3C                   ENDIF
072700970910E   2C                   ENDIF
072800970910E   1C                   ENDIF
072900970910     C*
073000970910     C                   ENDSR
073100970612     C*------------------------------------------------------------------------*
073200970612     C* CHKINL - CONTROLLA L'INOLTRO
073300970612     C*------------------------------------------------------------------------*
073400970612     C     CHKINL        BEGSR
073500970612     C*
073600970612     C     *LIKE         DEFINE    V1CISO        WINL
073700970612     C     *LIKE         DEFINE    V1DISO        WDINL
073800970612     C*
073900970612     C* RICHIESTO ELENCO
074000970612     C                   SETOFF                                           99
074100970612     C     '?'           SCAN      WINL                                   99
074200970612IF  1C     *IN99         IFEQ      *ON
074300020906     C                   Z-ADD     1             §KUT              1 0
074400970612     C                   MOVEL     '9A'          §COD              2
074500970612     C                   MOVEL     *BLANKS       §KEY              8
074600970612     C                   MOVEL     *BLANKS       §DES             30
074700970612     C                   CALL      'X§TABER'
074800970612     C                   PARM                    §KUT
074900970612     C                   PARM                    §COD
075000970612     C                   PARM                    §KEY
075100970612     C                   PARM                    §DES
075200970612     C                   MOVEL     §KEY          WINL
075300970612     C                   MOVEL     §DES          WDINL
075400970612E   1C                   ENDIF
075500970612     C*
075600970612     C* CONTROLLA ESISTENZA CODICE
075700020906     C                   MOVEL     1             KTBKUT
075800970612     C                   MOVEL     '9A'          KTBCOD
075900970612     C                   MOVEL(P)  WINL          KTBKEY
076000970612     C     KEYTAB        CHAIN     TABEL00F                           99
076100970612IF  1C     *IN99         IFEQ      *OFF
076200970612     C     TBLFLG        ANDEQ     *BLANKS
076300970612     C                   MOVEL     TBLUNI        DS9A
076400970612     C                   MOVEL     §9ADES        WDINL
076500970612X   1C                   ELSE
076600970612     C                   MOVEL     '1'           ERRL00
076700970612E   1C                   ENDIF
076800110623     c* Se non utilizzabile  nel cappario errore
076900110623     c                   if        §9auticpc='N' and errl00='0'
077000110623     C                   MOVEL     '1'           ERRL00
077100110623E   1C                   ENDIF
077200110623     C*
077300970612     C*
077400970612     C                   ENDSR
077500970910     C*------------------------------------------------------------------------*
077600970910     C* AZZVID - AZZERA I CAMPI DEL VIDEO
077700970910     C*------------------------------------------------------------------------*
077800970910     C     AZZVID        BEGSR
077900970910     C* CAMPI
078000970611     C                   Z-ADD     D00DRI        G08INV
078100970611     C                   Z-ADD     *ZEROS        G08DAT
078200970611     C                   MOVEL     '3'           G08ERR
078300970611     C                   CALL      'XSRDA8'
078400970611     C                   PARM                    WLBDA8
078500970611     C                   Z-ADD     G08DAT        V1CDRI
078600970910     C                   MOVEL     'C'           V1CTOR
078700970910     C                   MOVEL     *BLANKS       V1CNA1
078800970910     C                   MOVEL     *BLANKS       V1CNA2
078900970910     C                   MOVEL     *BLANKS       V1CCA1
079000970910     C                   MOVEL     *BLANKS       V1CCA2
079100970910     C                   MOVEL     *BLANKS       V1CPR1
079200970910     C                   MOVEL     *BLANKS       V1CPR2
079300970910     C                   MOVEL     *BLANKS       V1CLN1
079400970910     C                   MOVEL     *BLANKS       V1CLN2
079500970612     C                   MOVEL     *BLANKS       V1CISO
079600970613     C                   Z-ADD     *ZEROS        V1CTTC
079700970613     C                   MOVEL     'S'           V1CTEM
079800970910     C                   MOVEL     'T'           V1CTIC
079900970613     C                   MOVEL     'S'           V1CLOC
080000970612     C                   MOVEL     'N'           V1CLIV
080100971003     C                   MOVEL     'N'           V1COBS
080200970910     C* DECODIFICHE
080300970910     C                   MOVEL     *BLANKS       V1DNA1
080400970910     C                   MOVEL     *BLANKS       V1DNA2
080500970910     C                   MOVEL     *BLANKS       V1DPR1
080600970910     C                   MOVEL     *BLANKS       V1DPR2
080700970910     C                   MOVEL     *BLANKS       V1DLN1
080800970910     C                   MOVEL     *BLANKS       V1DLN2
080900970612     C                   MOVEL     *BLANKS       V1DISO
081000970910     C*
081100970910     C                   ENDSR
081200970910     C*------------------------------------------------------------------------*
081300970910     C* *INZSR - OPERAZIONI INIZIALI
081400970910     C*------------------------------------------------------------------------*
081500970910     C     *INZSR        BEGSR
081600970910     C*---
081700970910     C* RICEVIMENTO PARAMETRI
081800970910     C*---
081900970910     C     *ENTRY        PLIST
082000970612     C                   PARM                    KPJBA
082100970612     C                   PARM                    DSSI00
082200970910     C*
082300970910     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
082400970910     C     XMSGPA        PLIST
082500970910     C                   PARM                    DSMSMS
082600970910     C                   PARM                    DSMSPG
082700970910     C                   PARM                    DSMSKE
082800970910     C                   PARM                    DSMSER
082900061017      *
083000061017      * Reperimento dati aziendali
083100061017     c     *dtaara       define    §azute        AZUTEds
083200061017     c     *dtaara       define    §datiute      dDATIUTE
083300061017     c                   in(E)     *dtaara
083400061017     c                   if        %ERROR or RSUT = *blanks
083500061017     c                   clear                   Tibs34Ds
083600061017     c                   call      'TIBS34R'
083700061017     c                   parm                    Tibs34Ds
083800061017     c                   in        *dtaara
083900061017     c                   endif
084000061017     c                   movel     RSut          DSFirs
084100970910     C*
084200970910     C* IMPOSTA LA DS -TASTI PREMUTI-
084300970910     C                   CALL      'XKEY'
084400970910     C                   PARM                    DSKEY
084500970910     C*---
084600970910     C* VARIABILI NON RIFERITE AL DATA BASE
084700970910     C*---
084800970910     C     *LIKE         DEFINE    FUDFUN        FUNKEY
084900970910     C     *LIKE         DEFINE    D00OP0        DEPOP0
085000970910     C     *LIKE         DEFINE    D00OP1        DEPOP1
085100970910     C     *LIKE         DEFINE    D00OP0        WOP0
085200970910     C     *LIKE         DEFINE    D00OP1        WOP1
085300970910     C*---
085400970910     C* VARIABILI DI CONTROLLO
085500970910     C*---
085600970910     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
085700970910     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
085800970910     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
085900970910     C                   MOVEL     '0'           ERRL00            1            *ERRORE
086000970910     C*
086100970910     C* IMPOSTA LA DS -INVIO MESSAGGI-
086200970910     C                   MOVEL     'TISI04R'     DSMSPG
086300970910     C                   MOVEL     'TISI04R'     MSGQUE
086400970910     C                   MOVEL     *BLANKS       MSGKEY
086500970910     C                   MOVEL     *BLANKS       DSMSMS
086600970910     C*---
086700970910     C* VARIABILI RIFERITE AL DATA BASE
086800970910     C*---
086900970910     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
087000970910     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
087100970910     C     *LIKE         DEFINE    TBLCOD        KTBCOD
087200970910     C     *LIKE         DEFINE    TBLKEY        KTBKEY
087300970613     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
087400970613     C     *LIKE         DEFINE    CPTTTC        KCTTTC
087500970910     C*---
087600970910     C* CHIAVI DI LETTURA
087700970910     C*---
087800970910     C* LETTURA AZORG01L - COMPLETA
087900970910     C     KEYORG        KLIST
088000970910     C                   KFLD                    KORFIL                         *FILIALE
088100970910     C* LETTURA TABEL00F - COMPLETA
088200970910     C     KEYTAB        KLIST
088300970910     C                   KFLD                    KTBKUT                         *CODICE UTENTE
088400970910     C                   KFLD                    KTBCOD                         *TABELLA
088500970910     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
088600970613     C* LETTURA AZCPT01L - PARZIALE
088700970613     C     KEYCPT        KLIST
088800970613     C                   KFLD                    KCTVER                         *VERSIONE
088900970613     C                   KFLD                    KCTTTC                         *CODICE TEMPO
089000970910     C*
089100970910     C* DEPOSITA LE OPZIONI DI ENTRATA
089200970910     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
089300970910     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
089400970703     C*
089500970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
089600970703IF  1C     D00VER        IFEQ      *ZEROS
089700970707     C                   Z-ADD     D00DRI        P96DRI            8 0
089800970707     C                   Z-ADD     *ZEROS        P96VER            5 0
089900970707     C                   CALL      'TISI96R'
090000970707     C                   PARM                    P96DRI
090100970707     C                   PARM                    P96VER
090200970707     C                   Z-ADD     P96VER        D00VER
090300970703E   1C                   ENDIF
090400970910     C*
090500970910     C* GESTIONE TASTI FUNZIONALI A VIDEO
090600970910     C                   Z-ADD     *ZEROS        FUNGRU            5 0
090700970910     C                   EXSR      FUNGES
090800970910     C*
090900970910     C* IMPOSTO LA VIDEATA
091000970910     C                   EXSR      AZZVID
091100970910     C*
091200970910     C                   ENDSR
091300970910     C*------------------------------------------------------------------------*
091400970910     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
091500970910     C*------------------------------------------------------------------------*
091600970910     C     FUNGES        BEGSR
091700970910     C*
091800970910     C* PULISCO CAMPI DI LAVORO
091900970910     C                   Z-ADD     0             W                 5 0
092000970910     C*
092100970910     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
092200970910     C* E CARICO TABELLA FUNZIONI ATTIVE
092300970910     C     1             DO        24            I
092400970910     C                   MOVEL     FUD(I)        FUDRIG
092500970910     C*
092600970910     C* PULIZIA FUNZIONI PRECEDENTI
092700970910     C                   MOVEL     *BLANKS       FUV(I)
092800970910     C*
092900970910     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
093000970910     C* (SALTA F24)
093100970910     C     FUDATT        IFEQ      'S'
093200970910     C     FUDFUN        ANDNE     '24'
093300970910     C                   ADD       1             W
093400970910     C                   MOVEL     FUDDES        FUV(W)
093500970910     C                   END
093600970910     C                   END                                                    DO WHILE
093700970910     C*
093800970910     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
093900970910     C* ALTRIMENTI DISATTIVO F24
094000970910     C     1             DO        24            I
094100970910     C                   MOVEL     FUD(I)        FUDRIG
094200970910     C*
094300970910     C     FUDFUN        IFEQ      '24'
094400970910     C     W             IFGT      6
094500970910     C                   MOVEL     'S'           FUDATT
094600970910     C                   ELSE
094700970910     C                   MOVEL     *BLANKS       FUDATT
094800970910     C                   END                                                    W GT 8
094900970910     C                   MOVEL     FUDRIG        FUD(I)
095000970910     C                   END
095100970910     C                   END                                                    DO WHILE
095200970910     C*
095300970910     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
095400970910     C     W             DIV       6             I
095500970910     C                   MVR                     Z
095600970910     C*
095700970910     C     Z             IFNE      0
095800970910     C     I             ADD       1             Z
095900970910     C                   ELSE
096000970910     C                   Z-ADD     I             Z                 5 0
096100970910     C                   END
096200970910     C*
096300970910     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
096400970910     C                   ADD       1             FUNGRU
096500970910     C     FUNGRU        IFGT      Z
096600970910     C     FUNGRU        ORLT      1
096700970910     C                   Z-ADD     1             FUNGRU
096800970910     C                   END
096900970910     C*
097000970910     C* PRIMO GRUPPO DI 6 FUNZIONI
097100970910     C     FUNGRU        IFEQ      1
097200970910     C                   MOVEL     FUV(1)        FUN001
097300970910     C                   MOVEL     FUV(2)        FUN002
097400970910     C                   MOVEL     FUV(3)        FUN003
097500970910     C                   MOVEL     FUV(4)        FUN004
097600970910     C                   MOVEL     FUV(5)        FUN005
097700970910     C                   MOVEL     FUV(6)        FUN006
097800970910     C                   END
097900970910     C*
098000970910     C* SECONDO GRUPPO DI 6 FUNZIONI
098100970910     C     FUNGRU        IFEQ      2
098200970910     C                   MOVEL     FUV(7)        FUN001
098300970910     C                   MOVEL     FUV(8)        FUN002
098400970910     C                   MOVEL     FUV(9)        FUN003
098500970910     C                   MOVEL     FUV(10)       FUN004
098600970910     C                   MOVEL     FUV(11)       FUN005
098700970910     C                   MOVEL     FUV(12)       FUN006
098800970910     C                   END
098900970910     C*
099000970910     C* TERZO GRUPPO DI 6 FUNZIONI
099100970910     C     FUNGRU        IFEQ      3
099200970910     C                   MOVEL     FUV(13)       FUN001
099300970910     C                   MOVEL     FUV(14)       FUN002
099400970910     C                   MOVEL     FUV(15)       FUN003
099500970910     C                   MOVEL     FUV(16)       FUN004
099600970910     C                   MOVEL     FUV(17)       FUN005
099700970910     C                   MOVEL     FUV(18)       FUN006
099800970910     C                   END
099900970910     C*
100000970910     C* QUARTO GRUPPO DI 6 FUNZIONI
100100970910     C     FUNGRU        IFEQ      4
100200970910     C                   MOVEL     FUV(19)       FUN001
100300970910     C                   MOVEL     FUV(20)       FUN002
100400970910     C                   MOVEL     FUV(21)       FUN003
100500970910     C                   MOVEL     FUV(22)       FUN004
100600970910     C                   MOVEL     *BLANKS       FUN005
100700970910     C                   MOVEL     *BLANKS       FUN006
100800970910     C                   END
100900970910     C*
101000970910     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
101100970910     C                   MOVEL     *BLANKS       FUN007
101200970910     C                   MOVEL     *BLANKS       FUN008
101300970910     C*
101400970910     C     1             DO        24            I
101500970910     C                   MOVEL     FUD(I)        FUDRIG
101600970910     C     FUDFUN        IFEQ      '24'
101700970910     C     FUDATT        ANDEQ     'S'
101800970910     C                   MOVEL     FUDDES        FUN008
101900970910     C                   END
102000970910     C                   END                                                    DO
102100970910     C*
102200970910     C                   ENDSR
102300970910     C*------------------------------------------------------------------------*
102400970910     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
102500970910     C*------------------------------------------------------------------------*
102600970910     C     FUNCON        BEGSR
102700970910     C*
102800970910     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
102900970910     C                   EXSR      FUNVAR
103000970910     C*
103100970910     C* PULISCO CAMPI DI LAVORO
103200970910     C                   Z-ADD     0             I                 5 0
103300970910     C                   Z-ADD     0             W                 5 0
103400970910     C*
103500970910     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
103600970910     C     FUNKEY        IFNE      *BLANKS
103700970910     C     1             DO        24            I
103800970910     C                   MOVEL     FUD(I)        FUDRIG
103900970910     C*
104000970910     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
104100970910     C     FUNKEY        IFEQ      FUDFUN
104200970910     C     FUDATT        ANDEQ     'S'
104300970910     C                   Z-ADD     I             W
104400970910     C                   END
104500970910     C                   END                                                    DO WHILE
104600970910     C*
104700970910     C* FUNZIONE NON ATTIVA (ERRORE)
104800970910     C     W             IFEQ      0
104900970910     C*
105000970910     C* EMISSIONE MESSAGGIO DI ERRORE
105100970910     C                   MOVEL     MSG(1)        DSMSMS
105200970910     C*
105300970910     C                   END                                                    W=0
105400970910     C                   END                                                    FUNKEY NE *BLANKS
105500970910     C*
105600970910     C* FUNZIONE ATTIVA (PROCEDI ...)
105700970910     C     W             IFNE      0
105800970910     C     FUNKEY        OREQ      *BLANKS
105900970910     C     FUNKEY        IFEQ      *BLANKS
106000970910     C                   EXSR      ENTGES
106100970910     C                   END
106200970910     C     FUNKEY        IFEQ      '03'
106300970910     C                   EXSR      F03GES
106400970910     C                   END
106500970910     C     FUNKEY        IFEQ      '06'
106600970910     C                   EXSR      F06GES
106700970910     C                   END
106800970910     C     FUNKEY        IFEQ      '12'
106900970910     C                   EXSR      F12GES
107000970910     C                   END
107100970910     C     FUNKEY        IFEQ      '24'
107200970910     C                   EXSR      F24GES
107300970910     C                   END
107400970910     C                   END
107500970910     C*
107600970910     C                   ENDSR
107700970910     C*------------------------------------------------------------------------*
107800970910     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
107900970910     C*------------------------------------------------------------------------*
108000970910     C     FUNVAR        BEGSR
108100970910     C*
108200970910     C* INIZIALIZZA VARIABILE -FUNKEY-
108300970910     C*
108400970910     C* FUNKEY=*BLANKS SIGNIFICA ENTER
108500970910     C     KEY           IFEQ      ENTER
108600970910     C                   MOVEL     *BLANKS       FUNKEY
108700970910     C                   END
108800970910     C     KEY           IFEQ      F1
108900970910     C                   MOVEL     '01'          FUNKEY
109000970910     C                   END
109100970910     C     KEY           IFEQ      F2
109200970910     C                   MOVEL     '02'          FUNKEY
109300970910     C                   END
109400970910     C     KEY           IFEQ      F3
109500970910     C                   MOVEL     '03'          FUNKEY
109600970910     C                   END
109700970910     C     KEY           IFEQ      F4
109800970910     C                   MOVEL     '04'          FUNKEY
109900970910     C                   END
110000970910     C     KEY           IFEQ      F5
110100970910     C                   MOVEL     '05'          FUNKEY
110200970910     C                   END
110300970910     C     KEY           IFEQ      F6
110400970910     C                   MOVEL     '06'          FUNKEY
110500970910     C                   END
110600970910     C     KEY           IFEQ      F7
110700970910     C                   MOVEL     '07'          FUNKEY
110800970910     C                   END
110900970910     C     KEY           IFEQ      F8
111000970910     C                   MOVEL     '08'          FUNKEY
111100970910     C                   END
111200970910     C     KEY           IFEQ      F9
111300970910     C                   MOVEL     '09'          FUNKEY
111400970910     C                   END
111500970910     C     KEY           IFEQ      F10
111600970910     C                   MOVEL     '10'          FUNKEY
111700970910     C                   END
111800970910     C     KEY           IFEQ      F11
111900970910     C                   MOVEL     '11'          FUNKEY
112000970910     C                   END
112100970910     C     KEY           IFEQ      F12
112200970910     C                   MOVEL     '12'          FUNKEY
112300970910     C                   END
112400970910     C     KEY           IFEQ      F13
112500970910     C                   MOVEL     '13'          FUNKEY
112600970910     C                   END
112700970910     C     KEY           IFEQ      F14
112800970910     C                   MOVEL     '14'          FUNKEY
112900970910     C                   END
113000970910     C     KEY           IFEQ      F15
113100970910     C                   MOVEL     '15'          FUNKEY
113200970910     C                   END
113300970910     C     KEY           IFEQ      F16
113400970910     C                   MOVEL     '16'          FUNKEY
113500970910     C                   END
113600970910     C     KEY           IFEQ      F17
113700970910     C                   MOVEL     '17'          FUNKEY
113800970910     C                   END
113900970910     C     KEY           IFEQ      F18
114000970910     C                   MOVEL     '18'          FUNKEY
114100970910     C                   END
114200970910     C     KEY           IFEQ      F19
114300970910     C                   MOVEL     '19'          FUNKEY
114400970910     C                   END
114500970910     C     KEY           IFEQ      F20
114600970910     C                   MOVEL     '20'          FUNKEY
114700970910     C                   END
114800970910     C     KEY           IFEQ      F21
114900970910     C                   MOVEL     '21'          FUNKEY
115000970910     C                   END
115100970910     C     KEY           IFEQ      F22
115200970910     C                   MOVEL     '22'          FUNKEY
115300970910     C                   END
115400970910     C     KEY           IFEQ      F23
115500970910     C                   MOVEL     '23'          FUNKEY
115600970910     C                   END
115700970910     C     KEY           IFEQ      F24
115800970910     C                   MOVEL     '24'          FUNKEY
115900970910     C                   END
116000970910     C     KEY           IFEQ      ROLLUP
116100970910     C                   MOVEL     'RU'          FUNKEY
116200970910     C                   END
116300970910     C     KEY           IFEQ      HELP
116400970910     C                   MOVEL     'HP'          FUNKEY
116500970910     C                   END
116600970910     C                   ENDSR
116700970910     C*------------------------------------------------------------------------*
116800970910     C* NOTE COSTRUZIONE SCHIERE
116900970910     C*------------------------------------------------------------------------*
117000970910     C*
117100970910     C* -MSG-MESSAGGI
117200970910     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
117300970910     C* (NON TOCCARE MSG,1 E MSG,2)
117400970910     C*
117500970910     C* -CMD-COMANDI
117600970910     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
117700970910     C*
117800970910     C* -OPD-OPZIONI
117900970910     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
118000970910     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
118100970910     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
118200970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
118300970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
118400970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
118500970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
118600970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
118700970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
118800970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
118900970910     C*
119000970910     C* -FUD-FUNZIONI
119100970910     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
119200970910     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
119300970910     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
119400970910     C*  . POSSONO ESSERE IN ORDINE SPARSO
119500970910     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
119600970910     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
119700970910     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
119800970910     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
119900970910     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
120000970910     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
120100970910     O*------------------------------------------------------------------------*
120200970910** MSG - MESSAGGI
120300970619TISI04R- Opzione non gestita                                                   1
120400970619TISI04R- Tasto funzionale non gestito                                          2
120500970910TISI04R- 3
120600970910TISI04R- 4
120700970910TISI04R- 5
120800970910TISI04R- 6
120900970910TISI04R- 7
121000970910TISI04R- 8
121100970910TISI04R- 9
121200970910TISI04R- 10
121300970910TISI04R- Immettere uno dei valori possibili                                   11
121400970613TISI04R- Tempo consegna errato                                                12
121500970910TISI04R- Nazione errata                                                       13
121600970910TISI04R- Se richiesto il Dal immettere anche l'AL e viceversa                 14
121700970910TISI04R- Il Dal deve essere inferiore o uguale all'AL                         15
121800970910TISI04R- Provincia errata                                                     16
121900970910TISI04R- Linea di arrivo errata                                               17
122000110623TISI04R- Tipo inoltro errato o non utilizzabile nel cappario                  18
122100970612TISI04R- Immettere 'S' per stampare le località gestite a livello di località 19
122200970910TISI04R- Stampa lanciata ...                                                  20
122300970610** CMD - COMANDI CL
122400970610WRKSPLF
122500970610
122600970610
122700970610
122800970610
122900970910** FUD - FUNZIONI
123000970910                   -        (S/N)=N                        funz.01¬
123100970910                   -        (S/N)=N                        funz.02¬
123200970910F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
123300970910                   -        (S/N)=N                        funz.04¬
123400970910                   -        (S/N)=N                        funz.05¬
123500970610F06=Conferma       -        (S/N)=S                        funz.06¬
123600970910                   -        (S/N)=N                        funz.07¬
123700970910                   -        (S/N)=N                        funz.08¬
123800970910                   -        (S/N)=N                        funz.09¬
123900970910                   -        (S/N)=N                        funz.10¬
124000970910                   -        (S/N)=N                        funz.11¬
124100970910F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
124200970910                   -        (S/N)=N                        funz.13¬
124300970910                   -        (S/N)=N                        funz.14¬
124400970910                   -        (S/N)=N                        funz.15¬
124500970910                   -        (S/N)=N                        funz.16¬
124600970910                   -        (S/N)=N                        funz.17¬
124700970910                   -        (S/N)=N                        funz.18¬
124800970910                   -        (S/N)=N                        funz.19¬
124900970910                   -        (S/N)=N                        funz.20¬
125000970910                   -        (S/N)=N                        funz.21¬
125100970910                   -        (S/N)=N                        funz.22¬
125200970910                   -        (S/N)=N                        funz.23¬
125300970910F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
