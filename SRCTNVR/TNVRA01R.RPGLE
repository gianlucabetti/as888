000100120413       // gestione del sfl errori, indicatori dal 50 al 89 per singoli errori, 99 per cumulativo
000200120413
000300111222     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000400120416     fTNVRA01D  cf   e             workstn INFDS(DEVDS1)
000500120416     F                                     SFILE(DMS:MSGULT)
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
002600120416     D*------------------------------------------------------------------------*
002700120416     D* SCHIERE
002800120416     D*------------------------------------------------------------------------*
002900120416     d MSG             S             80    DIM(10) CTDATA PERRCD(1)
003000120413     D*------------
003100120413     D* DESCRIZIONI TASTI FUNZIONALI
003200120413     D*------------
003300120413     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003400120413     D*------------
003500120413     D* FUNZIONI DA CARICARE A VIDEO
003600120413     D*------------
003700120413     D FUV             S             19    DIM(24)
003800120413     D*------------
003900120413     D* RIDEFINIZIONE FUNZIONI VIDEO
004000120413     D*------------
004100120413     D FUDDS           DS
004200120413     D  FUDDE1                 1      1
004300120413     D  FUDFUN                 2      3
004400120413     D  FUDDE2                 4     19
004500120413     D  FUDFI0                20     34
004600120413     D  FUDATT                35     35
004700120413     D  FUDFI1                36     80
004800120413     D  FUDDES                 1     19
004900120413     D  FUDRIG                 1     80
005000120413     D*------------
005100120413     D* DS GESTIONE TASTI FUNZIONALI
005200120413     D*------------
005300120413     D DSKEY         E DS
005400120413     D DEVDS1          DS
005500120413     D  KEY                  369    369
005600120413     D*------------
005700120413     D* DS "XMSG" - GESTIONE MESSAGGI
005800120413     D*------------
005900120413     D DSMSG         E DS
006000120413
006100120413       // Variabili di appoggio
006200120413     D DataISO         s               d
006300120416     d in_DtRif        s              8s 0
006400120416     d out_Errore      s              1
006500120413
006600111227       //--------------------------------------------------------------
006700120413       // Definizione prototipi procedure usate.                       ?
006800111227       //--------------------------------------------------------------
006900111227       // - Reperimento dati utente
007000111227      /copy gaitrasrc/srcProtoPr,TIBS34R
007100111227     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
007200120416
007300120416     d* gestione statistica SQL
007400120416     d TNVRA02R        pr                  extpgm('TNVRA02R')
007500120416     d  in_DtRif                      8s 0
007600120417     d  in_kpjba                           like(kpjba)
007700120416     d  out_Errore                    1
007800120416
007900120413     C*
008000120413     C* GESTIONE TASTI FUNZIONALI A VIDEO
008100120413     C                   Z-ADD     *ZEROS        FUNGRU            5 0
008200120413     C                   EXSR      FUNGES
008300120413     C*
008400111227
008500111227
008600111227      /free
008700111227
008800111227         in(E) §AzUte;
008900111227         if NOT %error;
009000111227           in(E) §DatiUte;
009100111227         endif;
009200111227         if %error or RSut = *blanks;
009300111227           clear TIBS34ds;
009400111227           tibs34r(tibs34ds);
009500111227           in §AzUte;
009600111227           in §DatiUte;
009700111227         endif;
009800111227
009900111227      /end-free
010000111227
010100120413     C*
010200120413     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010300120413DO  1C     F03L00        DOUEQ     '1'
010400120413     C     F12L00        OREQ      '1'
010500120413     C*
010600120413     C* EMETTE FORMATO VIDEO
010700120413     C                   EXSR      BUTFMR
010800120413     C*
010900120413     C* GESTIONE TASTI FUNZIONALI
011000120413     C                   EXSR      FUNCON
011100120413     C*
011200120413E   1C                   ENDDO
011300120413     C*
011400120413     C                   SETON                                        LR
011500120413     C*------------------------------------------------------------------------*
011600120413     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011700120413     C*------------------------------------------------------------------------*
011800120413     C     BUTFMR        BEGSR
011900120413     C*
012000120413     C* EMETTE LA TESTATA
012100120413     C                   WRITE     T01
012200120416     C*
012300120416     C* SPEDISCE EVENTUALE MESSAGGIO
012400120416     C                   EXSR      SNDMSG
012500120413     C*
012600120413     C* EMETTE LE FUNZIONI
012700120413     C                   WRITE     Z01
012800120413     C*
012900120413     C* EMETTE IL VIDEO
013000120413     C                   EXFMT     D01
013100120413     C*
013200120413     C                   ENDSR
013300120416     C*------------------------------------------------------------------------*
013400120416     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500120416     C*------------------------------------------------------------------------*
013600120416     C     SNDMSG        BEGSR
013700120416     C*
013800120416     C* PULISCE IL SFILE MESSAGGI
013900120416     C                   MOVEL     '1'           *IN40
014000120416     C                   WRITE     DMC
014100120416     C                   MOVEL     '0'           *IN40
014200120416     C*
014300120416     C* VISUALIZZA IL MESSAGGIO
014400120416     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500120416     C                   CALL      'XMSG'        XMSGPA
014600120416IF  1C     DSMSER        IFEQ      *ZEROS
014700120416     C                   MOVEL     DSMSKE        MSGKEY
014800120416E   1C                   ENDIF
014900120416     C                   Z-ADD     1             MSGULT
015000120416     C                   WRITE     DMS
015100120416     C                   WRITE     DMC
015200120416     C*
015300120416     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400120416     C                   MOVEL     *BLANKS       DSMSMS
015500120416     C*
015600120416     C                   ENDSR
015700120413     C*------------------------------------------------------------------------*
015800120413     C* Esegui elaborazione
015900120413     C*------------------------------------------------------------------------*
016000120413     C     Esegui        BEGSR
016100120413     C*
016200120413     C*
016300120413     C                   ENDSR
016400120413     C*------------------------------------------------------------------------*
016500120413     C* *INZSR - OPERAZIONI INIZIALI
016600120413     C*------------------------------------------------------------------------*
016700120413     C     *INZSR        BEGSR
016800120413     C*
016900120413     C* RICEVIMENTO PARAMETRI
017000120413     C*
017100120413     C     *ENTRY        PLIST
017200120413     C                   PARM                    KPJBA
017300120413     C*
017400120413     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
017500120413     C     XMSGPA        PLIST
017600120413     C                   PARM                    DSMSMS
017700120413     C                   PARM                    DSMSPG
017800120413     C                   PARM                    DSMSKE
017900120413     C                   PARM                    DSMSER
018000120413     C*
018100120413     C* IMPOSTA LA DS -TASTI PREMUTI-
018200120413     C                   CALL      'XKEY'
018300120413     C                   PARM                    DSKEY
018400120413     C*---
018500120413     C* VARIABILI NON RIFERITE AL DATA BASE
018600120413     C*---
018700120413     C     *LIKE         DEFINE    FUDFUN        FUNKEY
018800120413     C*---
018900120413     C* VARIABILI DI CONTROLLO
019000120413     C*---
019100120413     C                   MOVEL     '0'           F03L00            1
019200120413     C                   MOVEL     '0'           F12L00            1
019300120416     C*
019400120416     C* IMPOSTA LA DS -INVIO MESSAGGI-
019500120416     C                   MOVEL     'UBLBLSPE'    DSMSPG
019600120416     C                   MOVEL     'UBLBLSPE'    MSGQUE
019700120416     C                   MOVEL     *BLANKS       MSGKEY
019800120416     C                   MOVEL     *BLANKS       DSMSMS
019900120413     C*
020000120413     C* GESTIONE TASTI FUNZIONALI A VIDEO
020100120413     C                   Z-ADD     *ZEROS        FUNGRU            5 0
020200120413     C                   EXSR      FUNGES
020300120413     C*
020400120413     C                   ENDSR
020500120413     C*------------------------------------------------------------------------*
020600120413     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
020700120413     C*------------------------------------------------------------------------*
020800120413     C     FUNGES        BEGSR
020900120413     C*
021000120413     C* PULISCO CAMPI DI LAVORO
021100120413     C                   Z-ADD     0             W                 5 0
021200120413     C*
021300120413     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
021400120413     C* E CARICO TABELLA FUNZIONI ATTIVE
021500120413     C     1             DO        24            I
021600120413     C                   MOVEL     FUD(I)        FUDRIG
021700120413     C*
021800120413     C* PULIZIA FUNZIONI PRECEDENTI
021900120413     C                   MOVEL     *BLANKS       FUV(I)
022000120413     C*
022100120413     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
022200120413     C* (SALTA F24)
022300120413     C     FUDATT        IFEQ      'S'
022400120413     C     FUDFUN        ANDNE     '24'
022500120413     C                   ADD       1             W
022600120413     C                   MOVEL     FUDDES        FUV(W)
022700120413     C                   END
022800120413     C                   END
022900120413     C*
023000120413     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
023100120413     C* ALTRIMENTI DISATTIVO F24
023200120413     C     1             DO        24            I
023300120413     C                   MOVEL     FUD(I)        FUDRIG
023400120413     C*
023500120413     C     FUDFUN        IFEQ      '24'
023600120413     C     W             IFGT      6
023700120413     C                   MOVEL     'S'           FUDATT
023800120413     C                   ELSE
023900120413     C                   MOVEL     *BLANKS       FUDATT
024000120413     C                   END
024100120413     C                   MOVEL     FUDRIG        FUD(I)
024200120413     C                   END
024300120413     C                   END
024400120413     C*
024500120413     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
024600120413     C     W             DIV       6             I
024700120413     C                   MVR                     Z
024800120413     C*
024900120413     C     Z             IFNE      0
025000120413     C     I             ADD       1             Z
025100120413     C                   ELSE
025200120413     C                   Z-ADD     I             Z                 5 0
025300120413     C                   END
025400120413     C*
025500120413     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
025600120413     C                   ADD       1             FUNGRU
025700120413     C     FUNGRU        IFGT      Z
025800120413     C     FUNGRU        ORLT      1
025900120413     C                   Z-ADD     1             FUNGRU
026000120413     C                   END
026100120413     C*
026200120413     C* PRIMO GRUPPO DI 6 FUNZIONI
026300120413     C     FUNGRU        IFEQ      1
026400120413     C                   MOVEL     FUV(1)        FUN001
026500120413     C                   MOVEL     FUV(2)        FUN002
026600120413     C                   MOVEL     FUV(3)        FUN003
026700120413     C                   MOVEL     FUV(4)        FUN004
026800120413     C                   MOVEL     FUV(5)        FUN005
026900120413     C                   MOVEL     FUV(6)        FUN006
027000120413     C                   END
027100120413     C*
027200120413     C* SECONDO GRUPPO DI 6 FUNZIONI
027300120413     C     FUNGRU        IFEQ      2
027400120413     C                   MOVEL     FUV(7)        FUN001
027500120413     C                   MOVEL     FUV(8)        FUN002
027600120413     C                   MOVEL     FUV(9)        FUN003
027700120413     C                   MOVEL     FUV(10)       FUN004
027800120413     C                   MOVEL     FUV(11)       FUN005
027900120413     C                   MOVEL     FUV(12)       FUN006
028000120413     C                   END
028100120413     C*
028200120413     C* TERZO GRUPPO DI 6 FUNZIONI
028300120413     C     FUNGRU        IFEQ      3
028400120413     C                   MOVEL     FUV(13)       FUN001
028500120413     C                   MOVEL     FUV(14)       FUN002
028600120413     C                   MOVEL     FUV(15)       FUN003
028700120413     C                   MOVEL     FUV(16)       FUN004
028800120413     C                   MOVEL     FUV(17)       FUN005
028900120413     C                   MOVEL     FUV(18)       FUN006
029000120413     C                   END
029100120413     C*
029200120413     C* QUARTO GRUPPO DI 6 FUNZIONI
029300120413     C     FUNGRU        IFEQ      4
029400120413     C                   MOVEL     FUV(19)       FUN001
029500120413     C                   MOVEL     FUV(20)       FUN002
029600120413     C                   MOVEL     FUV(21)       FUN003
029700120413     C                   MOVEL     FUV(22)       FUN004
029800120413     C                   MOVEL     *BLANKS       FUN005
029900120413     C                   MOVEL     *BLANKS       FUN006
030000120413     C                   END
030100120413     C*
030200120413     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
030300120413     C                   MOVEL     *BLANKS       FUN007
030400120413     C                   MOVEL     *BLANKS       FUN008
030500120413     C*
030600120413     C     1             DO        24            I
030700120413     C                   MOVEL     FUD(I)        FUDRIG
030800120413     C     FUDFUN        IFEQ      '24'
030900120413     C     FUDATT        ANDEQ     'S'
031000120413     C                   MOVEL     FUDDES        FUN008
031100120413     C                   END
031200120413     C                   END
031300120413     C*
031400120413     C                   ENDSR
031500120413     C*------------------------------------------------------------------------*
031600120413     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
031700120413     C*------------------------------------------------------------------------*
031800120413     C     FUNCON        BEGSR
031900120413     C*
032000120413     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
032100120413     C                   EXSR      FUNVAR
032200120413     C*
032300120413     C* PULISCO CAMPI DI LAVORO
032400120413     C                   Z-ADD     0             I                 5 0
032500120413     C                   Z-ADD     0             W                 5 0
032600120413     C*
032700120413     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
032800120413     C     FUNKEY        IFNE      *BLANKS
032900120413     C     1             DO        24            I
033000120413     C                   MOVEL     FUD(I)        FUDRIG
033100120413     C*
033200120413     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
033300120413     C     FUNKEY        IFEQ      FUDFUN
033400120413     C     FUDATT        ANDEQ     'S'
033500120413     C                   Z-ADD     I             W
033600120413     C                   END
033700120413     C                   END
033800120413     C*
033900120413     C* FUNZIONE NON ATTIVA (ERRORE)
034000120413     C     W             IFEQ      0
034100120413     C*
034200120413     C* EMISSIONE MESSAGGIO DI ERRORE
034300120416     C                   MOVEL     MSG(1)        DSMSMS
034400120413     C*
034500120413     C                   END
034600120413     C                   END
034700120413     C*
034800120413     C* FUNZIONE ATTIVA (PROCEDI ...)
034900120413     C     W             IFNE      0
035000120413     C     FUNKEY        OREQ      *BLANKS
035100120413     C     FUNKEY        IFEQ      *BLANKS
035200120413     C                   EXSR      ENTGES
035300120413     C                   END
035400120413     C     FUNKEY        IFEQ      '03'
035500120413     C                   EXSR      F03GES
035600120413     C                   END
035700120413     C     FUNKEY        IFEQ      '12'
035800120413     C                   EXSR      F12GES
035900120413     C                   END
036000120413     C                   END
036100120413     C*
036200120413     C                   ENDSR
036300120413     C*------------------------------------------------------------------------*
036400120413     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
036500120413     C*------------------------------------------------------------------------*
036600120413     C     FUNVAR        BEGSR
036700120413     C*
036800120413     C* INIZIALIZZA VARIABILE -FUNKEY-
036900120413     C*
037000120413     C* FUNKEY=*BLANKS SIGNIFICA ENTER
037100120413     C     KEY           IFEQ      ENTER
037200120413     C                   MOVEL     *BLANKS       FUNKEY
037300120413     C                   END
037400120413     C     KEY           IFEQ      F1
037500120413     C                   MOVEL     '01'          FUNKEY
037600120413     C                   END
037700120413     C     KEY           IFEQ      F2
037800120413     C                   MOVEL     '02'          FUNKEY
037900120413     C                   END
038000120413     C     KEY           IFEQ      F3
038100120413     C                   MOVEL     '03'          FUNKEY
038200120413     C                   END
038300120413     C     KEY           IFEQ      F4
038400120413     C                   MOVEL     '04'          FUNKEY
038500120413     C                   END
038600120413     C     KEY           IFEQ      F5
038700120413     C                   MOVEL     '05'          FUNKEY
038800120413     C                   END
038900120413     C     KEY           IFEQ      F6
039000120413     C                   MOVEL     '06'          FUNKEY
039100120413     C                   END
039200120413     C     KEY           IFEQ      F7
039300120413     C                   MOVEL     '07'          FUNKEY
039400120413     C                   END
039500120413     C     KEY           IFEQ      F8
039600120413     C                   MOVEL     '08'          FUNKEY
039700120413     C                   END
039800120413     C     KEY           IFEQ      F9
039900120413     C                   MOVEL     '09'          FUNKEY
040000120413     C                   END
040100120413     C     KEY           IFEQ      F10
040200120413     C                   MOVEL     '10'          FUNKEY
040300120413     C                   END
040400120413     C     KEY           IFEQ      F11
040500120413     C                   MOVEL     '11'          FUNKEY
040600120413     C                   END
040700120413     C     KEY           IFEQ      F12
040800120413     C                   MOVEL     '12'          FUNKEY
040900120413     C                   END
041000120413     C     KEY           IFEQ      F13
041100120413     C                   MOVEL     '13'          FUNKEY
041200120413     C                   END
041300120413     C     KEY           IFEQ      F14
041400120413     C                   MOVEL     '14'          FUNKEY
041500120413     C                   END
041600120413     C     KEY           IFEQ      F15
041700120413     C                   MOVEL     '15'          FUNKEY
041800120413     C                   END
041900120413     C     KEY           IFEQ      F16
042000120413     C                   MOVEL     '16'          FUNKEY
042100120413     C                   END
042200120413     C     KEY           IFEQ      F17
042300120413     C                   MOVEL     '17'          FUNKEY
042400120413     C                   END
042500120413     C     KEY           IFEQ      F18
042600120413     C                   MOVEL     '18'          FUNKEY
042700120413     C                   END
042800120413     C     KEY           IFEQ      F19
042900120413     C                   MOVEL     '19'          FUNKEY
043000120413     C                   END
043100120413     C     KEY           IFEQ      F20
043200120413     C                   MOVEL     '20'          FUNKEY
043300120413     C                   END
043400120413     C     KEY           IFEQ      F21
043500120413     C                   MOVEL     '21'          FUNKEY
043600120413     C                   END
043700120413     C     KEY           IFEQ      F22
043800120413     C                   MOVEL     '22'          FUNKEY
043900120413     C                   END
044000120413     C     KEY           IFEQ      F23
044100120413     C                   MOVEL     '23'          FUNKEY
044200120413     C                   END
044300120413     C     KEY           IFEQ      F24
044400120413     C                   MOVEL     '24'          FUNKEY
044500120413     C                   END
044600120413     C     KEY           IFEQ      ROLLUP
044700120413     C                   MOVEL     'RU'          FUNKEY
044800120413     C                   END
044900120413     C     KEY           IFEQ      HELP
045000120413     C                   MOVEL     'HP'          FUNKEY
045100120413     C                   END
045200120413     C                   ENDSR
045300120413     C*------------------------------------------------------------------------*
045400120413     C* ENTGES - GESTIONE ENTER -> INVIO
045500120413     C*------------------------------------------------------------------------*
045600120413     C     ENTGES        BEGSR
045700120413     C*
045800120413     C* controllo formato data
045900120413     C                   IF        D01DtRif <> 0
046000120416     C     *eur          TEST(D)                 D01DtRif               99
046100120416     C                   IF        *in99 = *on
046200120416     C                   MOVEL     MSG(3)        DSMSMS
046300120413     C                   ELSE
046400120416     C     *eur          MOVEL     D01DtRif      DataISO
046500120416     C     *iso          MOVEL     DataISO       in_DtRif
046600120413     C                   ENDIF
046700120417     C                   ELSE
046800120417     C                   SETON                                        99
046900120417     C                   MOVEL     MSG(3)        DSMSMS
047000120413     C                   ENDIF
047100120413     C*
047200120416     C* se non si sono errori chiamo il pgm che esegue la statistica passando la data di riferimento
047300120416     C                   IF        *in99 = *off
047400120416      /free
047500120417         TNVRA02R(in_DtRif:kpjba:out_Errore);
047600120416         // se ho ricevuto un msg di errore, attivo il flag
047700120416         select;
047800120417         when out_Errore = *blank;
047900120417           DSMSMS = MSG(5);
048000120417         when out_Errore = '4';
048100120417           DSMSMS = MSG(4);
048200120417         when out_Errore = '6';
048300120417           DSMSMS = MSG(6);
048400120417         when out_Errore = '7';
048500120417           DSMSMS = MSG(7);
048600120416         endsl;
048700120416      /end-free
048800120416     C*
048900120416     C                   ENDIF
049000120416     C*
049100120416     C                   ENDSR
049200120413     C*------------------------------------------------------------------------*
049300120413     C* F03GES - GESTIONE F03 -> FINE
049400120413     C*------------------------------------------------------------------------*
049500120413     C     F03GES        BEGSR
049600120413     C*
049700120413     C* IMPOSTA "PREMUTO F03"
049800120413     C                   MOVEL     '1'           F03L00
049900120413     C*
050000120413     C                   ENDSR
050100120413     C*------------------------------------------------------------------------*
050200120413     C* F12GES - GESTIONE F12 -> RITORNO
050300120413     C*------------------------------------------------------------------------*
050400120413     C     F12GES        BEGSR
050500120413     C*
050600120413     C* IMPOSTA "PREMUTO F12"
050700120413     C                   MOVEL     '1'           F12L00
050800120413     C*
050900120413     C                   ENDSR
051000120413     C*------------------------------------------------------------------------*
051100120413     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
051200120413     C*------------------------------------------------------------------------*
051300120413     C     F24GES        BEGSR
051400120413     C*
051500120413     C                   EXSR      FUNGES
051600120413     C*
051700120413     C                   ENDSR
051800120416** msg - messaggi emessi dal pgm (non toccare 1 e 2)
051900120416TNVRA01R Funzione non attiva
052000120416TNVRA01R 2
052100120416TNVRA01R Data errata
052200120416TNVRA01R Nessuna registrazione con questa data
052300120417TNVRA01R Stampa effettuata
052400120417TNVRA01R Errore in stampa
052500120417TNVRA01R Errore SQL: guarda il DUMP
052600120416TNVRA01R 8
052700120416TNVRA01R 9
052800120416TNVRA01R 10
052900120416** FUD - FUNZIONI
053000120413                   -        (S/N)=N                        funz.01¬
053100120413                   -        (S/N)=N                        funz.02¬
053200120413F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
053300120413                   -        (S/N)=N                        funz.04¬
053400120413                   -        (S/N)=N                        funz.05¬
053500120413                   -        (S/N)=N                        funz.06¬
053600120413                   -        (S/N)=N                        funz.07¬
053700120413                   -        (S/N)=N                        funz.08¬
053800120413                   -        (S/N)=N                        funz.09¬
053900120413                   -        (S/N)=N                        funz.10¬
054000120413                   -        (S/N)=N                        funz.11¬
054100120413F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
054200120413                   -        (S/N)=N                        funz.13¬
054300120413                   -        (S/N)=N                        funz.14¬
054400120413                   -        (S/N)=N                        funz.15¬
054500120413                   -        (S/N)=N                        funz.16¬
054600120413                   -        (S/N)=N                        funz.17¬
054700120413                   -        (S/N)=N                        funz.18¬
054800120413                   -        (S/N)=N                        funz.19¬
054900120413                   -        (S/N)=N                        funz.20¬
055000120413                   -        (S/N)=N                        funz.21¬
055100120413                   -        (S/N)=N                        funz.22¬
055200120413                   -        (S/N)=N                        funz.23¬
055300120413F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
