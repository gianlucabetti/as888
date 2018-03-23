000100970526     H*------------------------------------------------------------------------*
000200070416     H* TEST - REPERIMENTO STATO TELLUS BOLLA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000900070416     FFIDG34D1  CF   E             WORKSTN INFDS(DEVDS1)
001000070416     F                                     SFILE(DG34DMS:MSGULT)
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------
001500970526     D* DECODIFICHE
001600970526     D*------------
001700020415     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* DESCRIZIONI TASTI FUNZIONALI
002400970526     D*------------
002500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* FUNZIONI DA CARICARE A VIDEO
002800970526     D*------------
002900970526     D FUV             S             19    DIM(24)
003000970526     D*------------------------------------------------------------------------*
003100970526     D* INPUT
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970526     D*------------
003600970526     D FUDDS           DS
003700970526     D  FUDDE1                 1      1
003800970526     D  FUDFUN                 2      3
003900970526     D  FUDDE2                 4     19
004000970526     D  FUDFI0                20     34
004100970526     D  FUDATT                35     35
004200970526     D  FUDFI1                36     80
004300970526     D  FUDDES                 1     19
004400970526     D  FUDRIG                 1     80
004500970526     D*------------
004600970526     D* DS GESTIONE TASTI FUNZIONALI
004700970526     D*------------
004800970526     D DSKEY         E DS
004900970526     D DEVDS1          DS
005000970526     D  KEY                  369    369
005100970526     D*------------
005200970526     D* DS "XMSG" - GESTIONE MESSAGGI
005300970526     D*------------
005400970526     D DSMSG         E DS
005900020415     D*------------
006000970526     D* ARCHITETTURA
006100970526     D*------------
006200970526     D KPJBA         E DS
006300000407     D*------------------
006400020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
006500020412     D*------------------
006600070416     D FIDG34DS      E DS
007200970526     C*------------------------------------------------------------------------*
007300970526     C* MAIN LINES
007400970526     C*------------------------------------------------------------------------*
007500970526     C*
007600970526     C* NON CI SONO ERRORI IN ENTRATA
007700970526IF  1C     ERRL00        IFEQ      '0'
007800970526     C*
007900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008300970526     C*
008400970526     C* EMETTE FORMATO VIDEO
008500970526     C                   EXSR      BUTFMR
008600970526     C*
008700970526     C* GESTIONE TASTI FUNZIONALI
008800970526     C                   EXSR      FUNCON
008900970526E   2C                   ENDDO
009000970526     C*
009100970526E   1C                   ENDIF
009200970526     C* FINE PGM
009300970526     C                   SETON                                        LR
009400970526     C*------------------------------------------------------------------------*
009500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009600970526     C*------------------------------------------------------------------------*
009700970526     C     BUTFMR        BEGSR
009800970526     C*
009900970526     C* EMETTE LA TESTATA
010000070416     C                   WRITE     DG34D01
010100970526     C*
010200970526     C* SPEDISCE EVENTUALE MESSAGGIO
010300970526     C                   EXSR      SNDMSG
010400970526     C*
010500970526     C* EMETTE LE FUNZIONI
010600070416     C                   WRITE     DG34D02
010700970526     C*
010800970526     C* EMETTE IL VIDEO
010900070416     C                   EXFMT     DG34D03
011000970526     C*
011100970526     C* REIMPOSTA FLAG DI ERRORE
011200970526     C                   MOVEL     '0'           ERRL00
011300970526     C*
011400970526     C                   ENDSR
011500970526     C*------------------------------------------------------------------------*
011600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011700970526     C*------------------------------------------------------------------------*
011800970526     C     SNDMSG        BEGSR
011900970526     C*
012000970526     C* PULISCE IL SFILE MESSAGGI
012100970526     C                   MOVEL     '1'           *IN40
012200070416     C                   WRITE     DG34DMC
012300970526     C                   MOVEL     '0'           *IN40
012400970526     C*
012500970526     C* VISUALIZZA IL MESSAGGIO
012600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
012700970526     C                   CALL      'XMSG'        XMSGPA
012800970526IF  1C     DSMSER        IFEQ      *ZEROS
012900970526     C                   MOVEL     DSMSKE        MSGKEY
013000970526E   1C                   ENDIF
013100970526     C                   Z-ADD     1             MSGULT
013200070416     C                   WRITE     DG34DMS
013300070416     C                   WRITE     DG34DMC
013400970526     C*
013500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013600970526     C                   MOVEL     *BLANKS       DSMSMS
013700970526     C*
013800970526     C                   ENDSR
013900970526     C*------------------------------------------------------------------------*
014000970526     C* ENTGES - GESTIONE ENTER -> INVIO
014100970526     C*------------------------------------------------------------------------*
014200970526     C     ENTGES        BEGSR
014300970526     C*
014400020415     C                   EXSR      TEST
014500970526     C*
014600970526     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F03GES - GESTIONE F03 -> FINE
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F03GES        BEGSR
015100970526     C*
015200970526     C* IMPOSTA "PREMUTO F03"
015300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F06GES        BEGSR
016000970526     C*
016100020412     C*
016200020412     C                   ENDSR
016300970526     C*------------------------------------------------------------------------*
016400970526     C* F12GES - GESTIONE F12 -> RITORNO
016500970526     C*------------------------------------------------------------------------*
016600970526     C     F12GES        BEGSR
016700970526     C*
016800970526     C* IMPOSTA "PREMUTO F12"
016900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017400970526     C*------------------------------------------------------------------------*
017500970526     C     F24GES        BEGSR
017600970526     C*
017700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
017800970526     C*
017900970526     C                   ENDSR
018000970526     C*------------------------------------------------------------------------*
018100051227     C* TEST - OPERAZIONI DI TEST X REPERIMENTO DESTINATARIO PARTICOLARE
018200970526     C*------------------------------------------------------------------------*
018300020415     C     TEST          BEGSR
018400970526     C*
018500020415     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
018600020415     C                   EXSR      IMPCAM
019000970526     C*
019100020415     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
019101070416     C                   EVAL      ID34SIF = KNSIF
019300070416     C                   CALL      'FIDG34C'
019400070416     C                   PARM                    FIDG34DS
023000020412     C*
023100020412     C                   ENDSR
023200000407     C*------------------------------------------------------------------------*
023300020415     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
023400000407     C*------------------------------------------------------------------------*
023500000407     C     IMPCAM        BEGSR
023600000407     C*
024300000407     C*
024400000407     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
024700970526     C*------------------------------------------------------------------------*
024800970526     C     IMPFUN        BEGSR
024900970526     C*
025000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
025100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
025200970526     C                   EXSR      FUNGES
025300970526     C*
025400970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
025500970526     C                   MOVEL     DEC(1)        VIDFUN
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* *INZSR - OPERAZIONI INIZIALI
026000970526     C*------------------------------------------------------------------------*
026100970526     C     *INZSR        BEGSR
026200970526     C*---
026300970526     C* RICEVIMENTO PARAMETRI
026400970526     C*---
026500970526     C     *ENTRY        PLIST
026600970612     C                   PARM                    KPJBA
026601070416     C*
026602070416     C                   CLEAR                   FIDG34DS
027200970526     C*
027300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
027400970526     C     XMSGPA        PLIST
027500970526     C                   PARM                    DSMSMS
027600970526     C                   PARM                    DSMSPG
027700970526     C                   PARM                    DSMSKE
027800970526     C                   PARM                    DSMSER
027900970526     C*
028000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
028100970526     C                   CALL      'XKEY'
028200970526     C                   PARM                    DSKEY
028300970526     C*---
028400970526     C* VARIABILI NON RIFERITE AL DATA BASE
028500970526     C*---
028600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
028700970526     C*---
028800970526     C* VARIABILI DI CONTROLLO
028900970526     C*---
029000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
029100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
029200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
029300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
029400970526     C*
029500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
029600070416     C                   MOVEL     'FIDG34R'     DSMSPG
029700070416     C                   MOVEL     'FIDG34R'     MSGQUE
029800970526     C                   MOVEL     *BLANKS       MSGKEY
029900970526     C                   MOVEL     *BLANKS       DSMSMS
030300970526     C*
030400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
030500970526     C                   EXSR      IMPFUN
030600970526     C*
030700970526     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
031000970526     C*------------------------------------------------------------------------*
031100970526     C     FUNGES        BEGSR
031200970526     C*
031300970526     C* PULISCO CAMPI DI LAVORO
031400970526     C                   Z-ADD     0             W                 5 0
031500970526     C*
031600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
031700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
031800970526     C     1             DO        24            I
031900970526     C                   MOVEL     FUD(I)        FUDRIG
032000970526     C*
032100970526     C* PULIZIA FUNZIONI PRECEDENTI
032200970526     C                   MOVEL     *BLANKS       FUV(I)
032300970526     C*
032400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
032500970526     C* (SALTA F24)
032600970526     C     FUDATT        IFEQ      'S'
032700970526     C     FUDFUN        ANDNE     '24'
032800970526     C                   ADD       1             W
032900970526     C                   MOVEL     FUDDES        FUV(W)
033000970526     C                   END
033100970526     C                   END                                                    DO WHILE
033200970526     C*
033300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
033400970526     C* ALTRIMENTI DISATTIVO F24
033500970526     C     1             DO        24            I
033600970526     C                   MOVEL     FUD(I)        FUDRIG
033700970526     C*
033800970526     C     FUDFUN        IFEQ      '24'
033900970526     C     W             IFGT      6
034000970526     C                   MOVEL     'S'           FUDATT
034100970526     C                   ELSE
034200970526     C                   MOVEL     *BLANKS       FUDATT
034300970526     C                   END                                                    W GT 8
034400970526     C                   MOVEL     FUDRIG        FUD(I)
034500970526     C                   END
034600970526     C                   END                                                    DO WHILE
034700970526     C*
034800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
034900970526     C     W             DIV       6             I
035000970526     C                   MVR                     Z
035100970526     C*
035200970526     C     Z             IFNE      0
035300970526     C     I             ADD       1             Z
035400970526     C                   ELSE
035500970526     C                   Z-ADD     I             Z                 5 0
035600970526     C                   END
035700970526     C*
035800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
035900970526     C                   ADD       1             FUNGRU
036000970526     C     FUNGRU        IFGT      Z
036100970526     C     FUNGRU        ORLT      1
036200970526     C                   Z-ADD     1             FUNGRU
036300970526     C                   END
036400970526     C*
036500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
036600970526     C     FUNGRU        IFEQ      1
036700970526     C                   MOVEL     FUV(1)        FUN001
036800970526     C                   MOVEL     FUV(2)        FUN002
036900970526     C                   MOVEL     FUV(3)        FUN003
037000970526     C                   MOVEL     FUV(4)        FUN004
037100970526     C                   MOVEL     FUV(5)        FUN005
037200970526     C                   MOVEL     FUV(6)        FUN006
037300970526     C                   END
037400970526     C*
037500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
037600970526     C     FUNGRU        IFEQ      2
037700970526     C                   MOVEL     FUV(7)        FUN001
037800970526     C                   MOVEL     FUV(8)        FUN002
037900970526     C                   MOVEL     FUV(9)        FUN003
038000970526     C                   MOVEL     FUV(10)       FUN004
038100970526     C                   MOVEL     FUV(11)       FUN005
038200970526     C                   MOVEL     FUV(12)       FUN006
038300970526     C                   END
038400970526     C*
038500970526     C* TERZO GRUPPO DI 6 FUNZIONI
038600970526     C     FUNGRU        IFEQ      3
038700970526     C                   MOVEL     FUV(13)       FUN001
038800970526     C                   MOVEL     FUV(14)       FUN002
038900970526     C                   MOVEL     FUV(15)       FUN003
039000970526     C                   MOVEL     FUV(16)       FUN004
039100970526     C                   MOVEL     FUV(17)       FUN005
039200970526     C                   MOVEL     FUV(18)       FUN006
039300970526     C                   END
039400970526     C*
039500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
039600970526     C     FUNGRU        IFEQ      4
039700970526     C                   MOVEL     FUV(19)       FUN001
039800970526     C                   MOVEL     FUV(20)       FUN002
039900970526     C                   MOVEL     FUV(21)       FUN003
040000970526     C                   MOVEL     FUV(22)       FUN004
040100970526     C                   MOVEL     *BLANKS       FUN005
040200970526     C                   MOVEL     *BLANKS       FUN006
040300970526     C                   END
040400970526     C*
040500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
040600970526     C                   MOVEL     *BLANKS       FUN007
040700970526     C                   MOVEL     *BLANKS       FUN008
040800970526     C*
040900970526     C     1             DO        24            I
041000970526     C                   MOVEL     FUD(I)        FUDRIG
041100970526     C     FUDFUN        IFEQ      '24'
041200970526     C     FUDATT        ANDEQ     'S'
041300970526     C                   MOVEL     FUDDES        FUN008
041400970526     C                   END
041500970526     C                   END                                                    DO
041600970526     C*
041700970526     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
042000970526     C*------------------------------------------------------------------------*
042100970526     C     FUNCON        BEGSR
042200970526     C*
042300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
042400970526     C                   EXSR      FUNVAR
042500970526     C*
042600970526     C* PULISCO CAMPI DI LAVORO
042700970526     C                   Z-ADD     0             I                 5 0
042800970526     C                   Z-ADD     0             W                 5 0
042900970526     C*
043000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
043100970526     C     FUNKEY        IFNE      *BLANKS
043200970526     C     1             DO        24            I
043300970526     C                   MOVEL     FUD(I)        FUDRIG
043400970526     C*
043500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
043600970526     C     FUNKEY        IFEQ      FUDFUN
043700970526     C     FUDATT        ANDEQ     'S'
043800970526     C                   Z-ADD     I             W
043900970526     C                   END
044000970526     C                   END                                                    DO WHILE
044100970526     C*
044200970526     C* FUNZIONE NON ATTIVA (ERRORE)
044300970526     C     W             IFEQ      0
044400970526     C*
044500970526     C* EMISSIONE MESSAGGIO DI ERRORE
044600970526     C                   MOVEL     MSG(1)        DSMSMS
044700970526     C*
044800970526     C                   END                                                    W=0
044900970526     C                   END                                                    FUNKEY NE *BLANKS
045000970526     C*
045100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
045200970526     C     W             IFNE      0
045300970526     C     FUNKEY        OREQ      *BLANKS
045400970526     C     FUNKEY        IFEQ      *BLANKS
045500970526     C                   EXSR      ENTGES
045600970526     C                   END
045700970526     C     FUNKEY        IFEQ      '03'
045800970526     C                   EXSR      F03GES
045900970526     C                   END
046000970526     C     FUNKEY        IFEQ      '06'
046100970526     C                   EXSR      F06GES
046200970526     C                   END
046300970526     C     FUNKEY        IFEQ      '12'
046400970526     C                   EXSR      F12GES
046500970526     C                   END
046600970526     C     FUNKEY        IFEQ      '24'
046700970526     C                   EXSR      F24GES
046800970526     C                   END
046900970526     C                   END
047000970526     C*
047100970526     C                   ENDSR
047200970526     C*------------------------------------------------------------------------*
047300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
047400970526     C*------------------------------------------------------------------------*
047500970526     C     FUNVAR        BEGSR
047600970526     C*
047700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
047800970526     C*
047900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
048000970526     C     KEY           IFEQ      ENTER
048100970526     C                   MOVEL     *BLANKS       FUNKEY
048200970526     C                   END
048300970526     C     KEY           IFEQ      F1
048400970526     C                   MOVEL     '01'          FUNKEY
048500970526     C                   END
048600970526     C     KEY           IFEQ      F2
048700970526     C                   MOVEL     '02'          FUNKEY
048800970526     C                   END
048900970526     C     KEY           IFEQ      F3
049000970526     C                   MOVEL     '03'          FUNKEY
049100970526     C                   END
049200970526     C     KEY           IFEQ      F4
049300970526     C                   MOVEL     '04'          FUNKEY
049400970526     C                   END
049500970526     C     KEY           IFEQ      F5
049600970526     C                   MOVEL     '05'          FUNKEY
049700970526     C                   END
049800970526     C     KEY           IFEQ      F6
049900970526     C                   MOVEL     '06'          FUNKEY
050000970526     C                   END
050100970526     C     KEY           IFEQ      F7
050200970526     C                   MOVEL     '07'          FUNKEY
050300970526     C                   END
050400970526     C     KEY           IFEQ      F8
050500970526     C                   MOVEL     '08'          FUNKEY
050600970526     C                   END
050700970526     C     KEY           IFEQ      F9
050800970526     C                   MOVEL     '09'          FUNKEY
050900970526     C                   END
051000970526     C     KEY           IFEQ      F10
051100970526     C                   MOVEL     '10'          FUNKEY
051200970526     C                   END
051300970526     C     KEY           IFEQ      F11
051400970526     C                   MOVEL     '11'          FUNKEY
051500970526     C                   END
051600970526     C     KEY           IFEQ      F12
051700970526     C                   MOVEL     '12'          FUNKEY
051800970526     C                   END
051900970526     C     KEY           IFEQ      F13
052000970526     C                   MOVEL     '13'          FUNKEY
052100970526     C                   END
052200970526     C     KEY           IFEQ      F14
052300970526     C                   MOVEL     '14'          FUNKEY
052400970526     C                   END
052500970526     C     KEY           IFEQ      F15
052600970526     C                   MOVEL     '15'          FUNKEY
052700970526     C                   END
052800970526     C     KEY           IFEQ      F16
052900970526     C                   MOVEL     '16'          FUNKEY
053000970526     C                   END
053100970526     C     KEY           IFEQ      F17
053200970526     C                   MOVEL     '17'          FUNKEY
053300970526     C                   END
053400970526     C     KEY           IFEQ      F18
053500970526     C                   MOVEL     '18'          FUNKEY
053600970526     C                   END
053700970526     C     KEY           IFEQ      F19
053800970526     C                   MOVEL     '19'          FUNKEY
053900970526     C                   END
054000970526     C     KEY           IFEQ      F20
054100970526     C                   MOVEL     '20'          FUNKEY
054200970526     C                   END
054300970526     C     KEY           IFEQ      F21
054400970526     C                   MOVEL     '21'          FUNKEY
054500970526     C                   END
054600970526     C     KEY           IFEQ      F22
054700970526     C                   MOVEL     '22'          FUNKEY
054800970526     C                   END
054900970526     C     KEY           IFEQ      F23
055000970526     C                   MOVEL     '23'          FUNKEY
055100970526     C                   END
055200970526     C     KEY           IFEQ      F24
055300970526     C                   MOVEL     '24'          FUNKEY
055400970526     C                   END
055500970526     C     KEY           IFEQ      ROLLUP
055600970526     C                   MOVEL     'RU'          FUNKEY
055700970526     C                   END
055800970526     C     KEY           IFEQ      HELP
055900970526     C                   MOVEL     'HP'          FUNKEY
056000970526     C                   END
056100970526     C                   ENDSR
056200970526     C*------------------------------------------------------------------------*
056300970526     C* NOTE COSTRUZIONE SCHIERE
056400970526     C*------------------------------------------------------------------------*
056500970526     C*
056600970526     C* -MSG-MESSAGGI
056700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
056800970526     C* (NON TOCCARE MSG,1 E MSG,2)
056900970526     C*
057000970526     C* -CMD-COMANDI
057100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
057200970526     C*
057300970526     C* -OPD-OPZIONI
057400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
057500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
057600970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
057700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
057800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
057900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
058000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
058100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
058200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
058300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
058400970526     C*
058500970526     C* -FUD-FUNZIONI
058600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
058700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
058800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
058900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
059000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
059100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
059200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
059300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
059400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
059500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
059600970526     O*------------------------------------------------------------------------*
059700970526** DEC - DECODIFICHE
059800020415     TEST                                                         1
059900020415                                                                  2
060000020415                                                                  3
060100020415                                                                  4
060200020415                                                                  5
060300020415                                                                  6
060400970528** MSG - MESSAGGI
060500070416FIDG34R- Tasto funzionale non gestito                                          1
060600070416FIDG34R- Inserimento effettuato                                                2
060700070416FIDG34R- Modifica effettuata                                                   3
060800070416FIDG34R- Annullamento effettuato                                               4
060900070416FIDG34R- Ripristino effettuato                                                 5
061000070416FIDG34R- Richiesta funzione per record inesistente.                            6
061100070416FIDG34R- Impossibile inserire, record già esistente.                           7
061200070416FIDG34R- Richiesta funzione per record esistente                               8
061300070416FIDG34R-                                                                       9
061400070416FIDG34R-                                                                       10
061500070416FIDG34R-                                                                       11
061600070416FIDG34R- Tutti i campi sono obbligatori                                        12
061700070416FIDG34R-                                                                       13
061800070416FIDG34R-                                                                       14
061900070416FIDG34R- Copia effettuata                                                      15
062000070416FIDG34R-                                                                       16
062100070416FIDG34R-                                                                       17
062200020415                                                                               18
062300020415                                                                               19
062400020415                                                                               20
062500970526** FUD - FUNZIONI
062600970526                   -        (S/N)=N                        funz.01¬
062700970526                   -        (S/N)=N                        funz.02¬
062800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
062900970526                   -        (S/N)=N                        funz.04¬
063000970526                   -        (S/N)=N                        funz.05¬
063100070416                   -        (S/N)=N                        funz.06¬
063200970527                   -        (S/N)=N                        funz.07¬
063300970527                   -        (S/N)=N                        funz.08¬
063400970527                   -        (S/N)=N                        funz.09¬
063500970527                   -        (S/N)=N                        funz.10¬
063600970526                   -        (S/N)=N                        funz.11¬
063700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
063800970526                   -        (S/N)=N                        funz.13¬
063900970526                   -        (S/N)=N                        funz.14¬
064000970526                   -        (S/N)=N                        funz.15¬
064100970526                   -        (S/N)=N                        funz.16¬
064200970526                   -        (S/N)=N                        funz.17¬
064300970526                   -        (S/N)=N                        funz.18¬
064400970526                   -        (S/N)=N                        funz.19¬
064500970526                   -        (S/N)=N                        funz.20¬
064600970526                   -        (S/N)=N                        funz.21¬
064700970526                   -        (S/N)=N                        funz.22¬
064800970526                   -        (S/N)=N                        funz.23¬
064900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
