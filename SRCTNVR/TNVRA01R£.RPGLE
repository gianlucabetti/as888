000100120413       // gestione del sfl errori, indicatori dal 50 al 89 per singoli errori, 99 per cumulativo
000200120413
000300111222     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000400120413     fxxxx001d  cf   e             workstn INFDS(DEVDS1)
000500120416     F                                     SFILE(UBLSDMS:MSGULT)
000600111227       // - Status ds
000700111227     d Status         sds
000800111227     d  SDSpgm           *proc
000900111227     d  SDSprm           *parms
001000111227     d  SDSusr               254    263
001100111227       // - InfDS
001200111227     d InfDspF         ds
001300111227     d  dsp_aid              369    369a
001400111227
001500111227       // - KPJBA
001600111227     d kpjba         e ds
001700111227
001800111227       // - Parametri x Controllo profilo utenti
001900111227     d TIBS34ds      e ds
002000111227
002100111227     d §AzUte        e ds                  extname(AZUTE00F)
002200111227     d                                     dtaara
002300111227     d §DatiUte      e ds                  extname(dDatiUte)
002400111227     d                                     dtaara
002500120413
002600120413     D*------------
002700120413     D* DESCRIZIONI TASTI FUNZIONALI
002800120413     D*------------
002900120413     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003000120413     D*------------
003100120413     D* FUNZIONI DA CARICARE A VIDEO
003200120413     D*------------
003300120413     D FUV             S             19    DIM(24)
003400120413     D*------------
003500120413     D* RIDEFINIZIONE FUNZIONI VIDEO
003600120413     D*------------
003700120413     D FUDDS           DS
003800120413     D  FUDDE1                 1      1
003900120413     D  FUDFUN                 2      3
004000120413     D  FUDDE2                 4     19
004100120413     D  FUDFI0                20     34
004200120413     D  FUDATT                35     35
004300120413     D  FUDFI1                36     80
004400120413     D  FUDDES                 1     19
004500120413     D  FUDRIG                 1     80
004600120413     D*------------
004700120413     D* DS GESTIONE TASTI FUNZIONALI
004800120413     D*------------
004900120413     D DSKEY         E DS
005000120413     D DEVDS1          DS
005100120413     D  KEY                  369    369
005200120413     D*------------
005300120413     D* DS "XMSG" - GESTIONE MESSAGGI
005400120413     D*------------
005500120413     D DSMSG         E DS
005600120413
005700120413       // Variabili di appoggio
005800120413     D DataISO         s               d
005900120413
006000111227       //--------------------------------------------------------------
006100120413       // Definizione prototipi procedure usate.                       ?
006200111227       //--------------------------------------------------------------
006300111227       // - Reperimento dati utente
006400111227      /copy gaitrasrc/srcProtoPr,TIBS34R
006500111227     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006600120413     C*
006700120413     C* GESTIONE TASTI FUNZIONALI A VIDEO
006800120413     C                   Z-ADD     *ZEROS        FUNGRU            5 0
006900120413     C                   EXSR      FUNGES
007000120413     C*
007100111227
007200111227
007300111227      /free
007400111227
007500111227         in(E) §AzUte;
007600111227         if NOT %error;
007700111227           in(E) §DatiUte;
007800111227         endif;
007900111227         if %error or RSut = *blanks;
008000111227           clear TIBS34ds;
008100111227           tibs34r(tibs34ds);
008200111227           in §AzUte;
008300111227           in §DatiUte;
008400111227         endif;
008500111227
008600111227      /end-free
008700111227
008800120413     C*
008900120413     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009000120413DO  1C     F03L00        DOUEQ     '1'
009100120413     C     F12L00        OREQ      '1'
009200120413     C*
009300120413     C* EMETTE FORMATO VIDEO
009400120413     C                   EXSR      BUTFMR
009500120413     C*
009600120413     C* GESTIONE TASTI FUNZIONALI
009700120413     C                   EXSR      FUNCON
009800120413     C*
009900120413     C* se non ci sono errori eseguo l'elaborazione
010000120413     C                   IF        *in99 = *off
010100120413     C                   EXSR      Esegui
010200120413     C                   ENDIF
010300120413     C*
010400120413E   1C                   ENDDO
010500120413     C*
010600120413     C                   SETON                                        LR
010700120413     C*------------------------------------------------------------------------*
010800120413     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010900120413     C*------------------------------------------------------------------------*
011000120413     C     BUTFMR        BEGSR
011100120413     C*
011200120413     C* IMPOSTA FLAG DI ERRORE
011300120413     C                   SETOFF                                       99
011400120413     C*
011500120413     C* EMETTE LA TESTATA
011600120413     C                   WRITE     T01
011700120413     C*
011800120413     C* EMETTE LE FUNZIONI
011900120413     C                   WRITE     Z01
012000120413     C*
012100120413     C* EMETTE IL VIDEO
012200120413     C                   EXFMT     D01
012300120413     C*
012400120413     C                   ENDSR
012500120413     C*------------------------------------------------------------------------*
012600120413     C* Esegui elaborazione
012700120413     C*------------------------------------------------------------------------*
012800120413     C     Esegui        BEGSR
012900120413     C*
013000120413     C*
013100120413     C                   ENDSR
013200120413     C*------------------------------------------------------------------------*
013300120413     C* *INZSR - OPERAZIONI INIZIALI
013400120413     C*------------------------------------------------------------------------*
013500120413     C     *INZSR        BEGSR
013600120413     C*
013700120413     C* RICEVIMENTO PARAMETRI
013800120413     C*
013900120413     C     *ENTRY        PLIST
014000120413     C                   PARM                    KPJBA
014100120413     C*
014200120413     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
014300120413     C     XMSGPA        PLIST
014400120413     C                   PARM                    DSMSMS
014500120413     C                   PARM                    DSMSPG
014600120413     C                   PARM                    DSMSKE
014700120413     C                   PARM                    DSMSER
014800120413     C*
014900120413     C* IMPOSTA LA DS -TASTI PREMUTI-
015000120413     C                   CALL      'XKEY'
015100120413     C                   PARM                    DSKEY
015200120413     C*---
015300120413     C* VARIABILI NON RIFERITE AL DATA BASE
015400120413     C*---
015500120413     C     *LIKE         DEFINE    FUDFUN        FUNKEY
015600120413     C*---
015700120413     C* VARIABILI DI CONTROLLO
015800120413     C*---
015900120413     C                   MOVEL     '0'           F03L00            1
016000120413     C                   MOVEL     '0'           F12L00            1
016100120413     C*
016200120413     C* GESTIONE TASTI FUNZIONALI A VIDEO
016300120413     C                   Z-ADD     *ZEROS        FUNGRU            5 0
016400120413     C                   EXSR      FUNGES
016500120413     C*
016600120413     C                   ENDSR
016700120413     C*------------------------------------------------------------------------*
016800120413     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
016900120413     C*------------------------------------------------------------------------*
017000120413     C     FUNGES        BEGSR
017100120413     C*
017200120413     C* PULISCO CAMPI DI LAVORO
017300120413     C                   Z-ADD     0             W                 5 0
017400120413     C*
017500120413     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
017600120413     C* E CARICO TABELLA FUNZIONI ATTIVE
017700120413     C     1             DO        24            I
017800120413     C                   MOVEL     FUD(I)        FUDRIG
017900120413     C*
018000120413     C* PULIZIA FUNZIONI PRECEDENTI
018100120413     C                   MOVEL     *BLANKS       FUV(I)
018200120413     C*
018300120413     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
018400120413     C* (SALTA F24)
018500120413     C     FUDATT        IFEQ      'S'
018600120413     C     FUDFUN        ANDNE     '24'
018700120413     C                   ADD       1             W
018800120413     C                   MOVEL     FUDDES        FUV(W)
018900120413     C                   END
019000120413     C                   END
019100120413     C*
019200120413     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
019300120413     C* ALTRIMENTI DISATTIVO F24
019400120413     C     1             DO        24            I
019500120413     C                   MOVEL     FUD(I)        FUDRIG
019600120413     C*
019700120413     C     FUDFUN        IFEQ      '24'
019800120413     C     W             IFGT      6
019900120413     C                   MOVEL     'S'           FUDATT
020000120413     C                   ELSE
020100120413     C                   MOVEL     *BLANKS       FUDATT
020200120413     C                   END
020300120413     C                   MOVEL     FUDRIG        FUD(I)
020400120413     C                   END
020500120413     C                   END
020600120413     C*
020700120413     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
020800120413     C     W             DIV       6             I
020900120413     C                   MVR                     Z
021000120413     C*
021100120413     C     Z             IFNE      0
021200120413     C     I             ADD       1             Z
021300120413     C                   ELSE
021400120413     C                   Z-ADD     I             Z                 5 0
021500120413     C                   END
021600120413     C*
021700120413     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
021800120413     C                   ADD       1             FUNGRU
021900120413     C     FUNGRU        IFGT      Z
022000120413     C     FUNGRU        ORLT      1
022100120413     C                   Z-ADD     1             FUNGRU
022200120413     C                   END
022300120413     C*
022400120413     C* PRIMO GRUPPO DI 6 FUNZIONI
022500120413     C     FUNGRU        IFEQ      1
022600120413     C                   MOVEL     FUV(1)        FUN001
022700120413     C                   MOVEL     FUV(2)        FUN002
022800120413     C                   MOVEL     FUV(3)        FUN003
022900120413     C                   MOVEL     FUV(4)        FUN004
023000120413     C                   MOVEL     FUV(5)        FUN005
023100120413     C                   MOVEL     FUV(6)        FUN006
023200120413     C                   END
023300120413     C*
023400120413     C* SECONDO GRUPPO DI 6 FUNZIONI
023500120413     C     FUNGRU        IFEQ      2
023600120413     C                   MOVEL     FUV(7)        FUN001
023700120413     C                   MOVEL     FUV(8)        FUN002
023800120413     C                   MOVEL     FUV(9)        FUN003
023900120413     C                   MOVEL     FUV(10)       FUN004
024000120413     C                   MOVEL     FUV(11)       FUN005
024100120413     C                   MOVEL     FUV(12)       FUN006
024200120413     C                   END
024300120413     C*
024400120413     C* TERZO GRUPPO DI 6 FUNZIONI
024500120413     C     FUNGRU        IFEQ      3
024600120413     C                   MOVEL     FUV(13)       FUN001
024700120413     C                   MOVEL     FUV(14)       FUN002
024800120413     C                   MOVEL     FUV(15)       FUN003
024900120413     C                   MOVEL     FUV(16)       FUN004
025000120413     C                   MOVEL     FUV(17)       FUN005
025100120413     C                   MOVEL     FUV(18)       FUN006
025200120413     C                   END
025300120413     C*
025400120413     C* QUARTO GRUPPO DI 6 FUNZIONI
025500120413     C     FUNGRU        IFEQ      4
025600120413     C                   MOVEL     FUV(19)       FUN001
025700120413     C                   MOVEL     FUV(20)       FUN002
025800120413     C                   MOVEL     FUV(21)       FUN003
025900120413     C                   MOVEL     FUV(22)       FUN004
026000120413     C                   MOVEL     *BLANKS       FUN005
026100120413     C                   MOVEL     *BLANKS       FUN006
026200120413     C                   END
026300120413     C*
026400120413     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
026500120413     C                   MOVEL     *BLANKS       FUN007
026600120413     C                   MOVEL     *BLANKS       FUN008
026700120413     C*
026800120413     C     1             DO        24            I
026900120413     C                   MOVEL     FUD(I)        FUDRIG
027000120413     C     FUDFUN        IFEQ      '24'
027100120413     C     FUDATT        ANDEQ     'S'
027200120413     C                   MOVEL     FUDDES        FUN008
027300120413     C                   END
027400120413     C                   END
027500120413     C*
027600120413     C                   ENDSR
027700120413     C*------------------------------------------------------------------------*
027800120413     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
027900120413     C*------------------------------------------------------------------------*
028000120413     C     FUNCON        BEGSR
028100120413     C*
028200120413     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
028300120413     C                   EXSR      FUNVAR
028400120413     C*
028500120413     C* PULISCO CAMPI DI LAVORO
028600120413     C                   Z-ADD     0             I                 5 0
028700120413     C                   Z-ADD     0             W                 5 0
028800120413     C*
028900120413     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
029000120413     C     FUNKEY        IFNE      *BLANKS
029100120413     C     1             DO        24            I
029200120413     C                   MOVEL     FUD(I)        FUDRIG
029300120413     C*
029400120413     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
029500120413     C     FUNKEY        IFEQ      FUDFUN
029600120413     C     FUDATT        ANDEQ     'S'
029700120413     C                   Z-ADD     I             W
029800120413     C                   END
029900120413     C                   END
030000120413     C*
030100120413     C* FUNZIONE NON ATTIVA (ERRORE)
030200120413     C     W             IFEQ      0
030300120413     C*
030400120413     C* EMISSIONE MESSAGGIO DI ERRORE
030500120413     C                   SETON                                        5099
030600120413     C*
030700120413     C                   END
030800120413     C                   END
030900120413     C*
031000120413     C* FUNZIONE ATTIVA (PROCEDI ...)
031100120413     C     W             IFNE      0
031200120413     C     FUNKEY        OREQ      *BLANKS
031300120413     C     FUNKEY        IFEQ      *BLANKS
031400120413     C                   EXSR      ENTGES
031500120413     C                   END
031600120413     C     FUNKEY        IFEQ      '03'
031700120413     C                   EXSR      F03GES
031800120413     C                   END
031900120413     C     FUNKEY        IFEQ      '12'
032000120413     C                   EXSR      F12GES
032100120413     C                   END
032200120413     C                   END
032300120413     C*
032400120413     C                   ENDSR
032500120413     C*------------------------------------------------------------------------*
032600120413     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
032700120413     C*------------------------------------------------------------------------*
032800120413     C     FUNVAR        BEGSR
032900120413     C*
033000120413     C* INIZIALIZZA VARIABILE -FUNKEY-
033100120413     C*
033200120413     C* FUNKEY=*BLANKS SIGNIFICA ENTER
033300120413     C     KEY           IFEQ      ENTER
033400120413     C                   MOVEL     *BLANKS       FUNKEY
033500120413     C                   END
033600120413     C     KEY           IFEQ      F1
033700120413     C                   MOVEL     '01'          FUNKEY
033800120413     C                   END
033900120413     C     KEY           IFEQ      F2
034000120413     C                   MOVEL     '02'          FUNKEY
034100120413     C                   END
034200120413     C     KEY           IFEQ      F3
034300120413     C                   MOVEL     '03'          FUNKEY
034400120413     C                   END
034500120413     C     KEY           IFEQ      F4
034600120413     C                   MOVEL     '04'          FUNKEY
034700120413     C                   END
034800120413     C     KEY           IFEQ      F5
034900120413     C                   MOVEL     '05'          FUNKEY
035000120413     C                   END
035100120413     C     KEY           IFEQ      F6
035200120413     C                   MOVEL     '06'          FUNKEY
035300120413     C                   END
035400120413     C     KEY           IFEQ      F7
035500120413     C                   MOVEL     '07'          FUNKEY
035600120413     C                   END
035700120413     C     KEY           IFEQ      F8
035800120413     C                   MOVEL     '08'          FUNKEY
035900120413     C                   END
036000120413     C     KEY           IFEQ      F9
036100120413     C                   MOVEL     '09'          FUNKEY
036200120413     C                   END
036300120413     C     KEY           IFEQ      F10
036400120413     C                   MOVEL     '10'          FUNKEY
036500120413     C                   END
036600120413     C     KEY           IFEQ      F11
036700120413     C                   MOVEL     '11'          FUNKEY
036800120413     C                   END
036900120413     C     KEY           IFEQ      F12
037000120413     C                   MOVEL     '12'          FUNKEY
037100120413     C                   END
037200120413     C     KEY           IFEQ      F13
037300120413     C                   MOVEL     '13'          FUNKEY
037400120413     C                   END
037500120413     C     KEY           IFEQ      F14
037600120413     C                   MOVEL     '14'          FUNKEY
037700120413     C                   END
037800120413     C     KEY           IFEQ      F15
037900120413     C                   MOVEL     '15'          FUNKEY
038000120413     C                   END
038100120413     C     KEY           IFEQ      F16
038200120413     C                   MOVEL     '16'          FUNKEY
038300120413     C                   END
038400120413     C     KEY           IFEQ      F17
038500120413     C                   MOVEL     '17'          FUNKEY
038600120413     C                   END
038700120413     C     KEY           IFEQ      F18
038800120413     C                   MOVEL     '18'          FUNKEY
038900120413     C                   END
039000120413     C     KEY           IFEQ      F19
039100120413     C                   MOVEL     '19'          FUNKEY
039200120413     C                   END
039300120413     C     KEY           IFEQ      F20
039400120413     C                   MOVEL     '20'          FUNKEY
039500120413     C                   END
039600120413     C     KEY           IFEQ      F21
039700120413     C                   MOVEL     '21'          FUNKEY
039800120413     C                   END
039900120413     C     KEY           IFEQ      F22
040000120413     C                   MOVEL     '22'          FUNKEY
040100120413     C                   END
040200120413     C     KEY           IFEQ      F23
040300120413     C                   MOVEL     '23'          FUNKEY
040400120413     C                   END
040500120413     C     KEY           IFEQ      F24
040600120413     C                   MOVEL     '24'          FUNKEY
040700120413     C                   END
040800120413     C     KEY           IFEQ      ROLLUP
040900120413     C                   MOVEL     'RU'          FUNKEY
041000120413     C                   END
041100120413     C     KEY           IFEQ      HELP
041200120413     C                   MOVEL     'HP'          FUNKEY
041300120413     C                   END
041400120413     C                   ENDSR
041500120413     C*------------------------------------------------------------------------*
041600120413     C* ENTGES - GESTIONE ENTER -> INVIO
041700120413     C*------------------------------------------------------------------------*
041800120413     C     ENTGES        BEGSR
041900120413     C*
042000120413     C* controllo formato data
042100120413     C                   IF        D01DtRif <> 0
042200120413     C     *dmy          TEST(D)                 D01DtRif               51
042300120413     C                   IF        *in51 = *on
042400120413     C                   SETON                                        99
042500120413     C                   ELSE
042600120413     C     *dmy          MOVEL     D01DtRif      DataISO
042700120413     C                   ENDIF
042800120413     C                   ENDIF
042900120413     C*
043000120413     C                   ENDSR
043100120413     C*------------------------------------------------------------------------*
043200120413     C* F03GES - GESTIONE F03 -> FINE
043300120413     C*------------------------------------------------------------------------*
043400120413     C     F03GES        BEGSR
043500120413     C*
043600120413     C* IMPOSTA "PREMUTO F03"
043700120413     C                   MOVEL     '1'           F03L00
043800120413     C*
043900120413     C                   ENDSR
044000120413     C*------------------------------------------------------------------------*
044100120413     C* F12GES - GESTIONE F12 -> RITORNO
044200120413     C*------------------------------------------------------------------------*
044300120413     C     F12GES        BEGSR
044400120413     C*
044500120413     C* IMPOSTA "PREMUTO F12"
044600120413     C                   MOVEL     '1'           F12L00
044700120413     C*
044800120413     C                   ENDSR
044900120413     C*------------------------------------------------------------------------*
045000120413     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
045100120413     C*------------------------------------------------------------------------*
045200120413     C     F24GES        BEGSR
045300120413     C*
045400120413     C                   EXSR      FUNGES
045500120413     C*
045600120413     C                   ENDSR
045700120413** FUD - FUNZIONI
045800120413                   -        (S/N)=N                        funz.01¬
045900120413                   -        (S/N)=N                        funz.02¬
046000120413F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
046100120413                   -        (S/N)=N                        funz.04¬
046200120413                   -        (S/N)=N                        funz.05¬
046300120413                   -        (S/N)=N                        funz.06¬
046400120413                   -        (S/N)=N                        funz.07¬
046500120413                   -        (S/N)=N                        funz.08¬
046600120413                   -        (S/N)=N                        funz.09¬
046700120413                   -        (S/N)=N                        funz.10¬
046800120413                   -        (S/N)=N                        funz.11¬
046900120413F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
047000120413                   -        (S/N)=N                        funz.13¬
047100120413                   -        (S/N)=N                        funz.14¬
047200120413                   -        (S/N)=N                        funz.15¬
047300120413                   -        (S/N)=N                        funz.16¬
047400120413                   -        (S/N)=N                        funz.17¬
047500120413                   -        (S/N)=N                        funz.18¬
047600120413                   -        (S/N)=N                        funz.19¬
047700120413                   -        (S/N)=N                        funz.20¬
047800120413                   -        (S/N)=N                        funz.21¬
047900120413                   -        (S/N)=N                        funz.22¬
048000120413                   -        (S/N)=N                        funz.23¬
048100120413F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
