000100970526     H*------------------------------------------------------------------------*
000200020415     H* TEST - DESTINATARI DISAGIATI/SUPERMERCATI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030626     FXSRLAV8TD CF   E             WORKSTN INFDS(DEVDS1)
000900030626     F                                     SFILE(LAV8DMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D*------------
001400970526     D* MESSAGGI
001500970526     D*------------
001600970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* DESCRIZIONI TASTI FUNZIONALI
001900970526     D*------------
002000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* FUNZIONI DA CARICARE A VIDEO
002300970526     D*------------
002400970526     D FUV             S             19    DIM(24)
002500970526     D*------------------------------------------------------------------------*
002600970526     D* INPUT
002700970526     D*------------------------------------------------------------------------*
002800970526     D*------------
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000970526     D*------------
005100020415     D*------------
005200970526     D* ARCHITETTURA
005300970526     D*------------
005400970526     D KPJBA         E DS
005500000407     D*------------------
005600030626     D* DS X PASSAGGIO PARAMETRI
005700020412     D*------------------
005800030626     D WDAT8           DS
005900030626     D  DATADA                        8  0
006000030626     D  DATAA                         8  0
006100030626     D  GGLAV                         5  0
006200970526     C*------------------------------------------------------------------------*
006300970526     C* MAIN LINES
006400970526     C*------------------------------------------------------------------------*
006500970526     C*
006600970526     C* NON CI SONO ERRORI IN ENTRATA
006700970526IF  1C     ERRL00        IFEQ      '0'
006800970526     C*
006900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
007100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
007200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
007300970526     C*
007400970526     C* EMETTE FORMATO VIDEO
007500970526     C                   EXSR      BUTFMR
007600970526     C*
007700970526     C* GESTIONE TASTI FUNZIONALI
007800970526     C                   EXSR      FUNCON
007900970526E   2C                   ENDDO
008000970526     C*
008100970526E   1C                   ENDIF
008200970526     C* FINE PGM
008300970526     C                   SETON                                        LR
008400970526     C*------------------------------------------------------------------------*
008500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
008600970526     C*------------------------------------------------------------------------*
008700970526     C     BUTFMR        BEGSR
008800970526     C*
008900970526     C* EMETTE LA TESTATA
009000030626     C                   WRITE     LAV8D01
009100970526     C*
009200970526     C* SPEDISCE EVENTUALE MESSAGGIO
009300970526     C                   EXSR      SNDMSG
009400970526     C*
009500970526     C* EMETTE LE FUNZIONI
009600030626     C                   WRITE     LAV8D02
009700970526     C*
009800970526     C* EMETTE IL VIDEO
009900030626     C                   EXFMT     LAV8D03
010000970526     C*
010100970526     C* REIMPOSTA FLAG DI ERRORE
010200970526     C                   MOVEL     '0'           ERRL00
010300970526     C*
010400970526     C                   ENDSR
010500970526     C*------------------------------------------------------------------------*
010600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
010700970526     C*------------------------------------------------------------------------*
010800970526     C     SNDMSG        BEGSR
010900970526     C*
011000970526     C* PULISCE IL SFILE MESSAGGI
011100970526     C                   MOVEL     '1'           *IN40
011200030626     C                   WRITE     LAV8DMC
011300970526     C                   MOVEL     '0'           *IN40
011400970526     C*
011500970526     C* VISUALIZZA IL MESSAGGIO
011600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
011700970526     C                   CALL      'XMSG'        XMSGPA
011800970526IF  1C     DSMSER        IFEQ      *ZEROS
011900970526     C                   MOVEL     DSMSKE        MSGKEY
012000970526E   1C                   ENDIF
012100970526     C                   Z-ADD     1             MSGULT
012200030626     C                   WRITE     LAV8DMS
012300030626     C                   WRITE     LAV8DMC
012400970526     C*
012500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
012600970526     C                   MOVEL     *BLANKS       DSMSMS
012700970526     C*
012800970526     C                   ENDSR
012900970526     C*------------------------------------------------------------------------*
013000970526     C* ENTGES - GESTIONE ENTER -> INVIO
013100970526     C*------------------------------------------------------------------------*
013200970526     C     ENTGES        BEGSR
013300970526     C*
013400020415     C                   EXSR      TEST
013500970526     C*
013600970526     C                   ENDSR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* F03GES - GESTIONE F03 -> FINE
013900970526     C*------------------------------------------------------------------------*
014000970526     C     F03GES        BEGSR
014100970526     C*
014200970526     C* IMPOSTA "PREMUTO F03"
014300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014400970526     C*
014500970526     C                   ENDSR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* F12GES - GESTIONE F12 -> RITORNO
014800970526     C*------------------------------------------------------------------------*
014900970526     C     F12GES        BEGSR
015000970526     C*
015100970526     C* IMPOSTA "PREMUTO F12"
015200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
015300970526     C*
015400970526     C                   ENDSR
015500970526     C*------------------------------------------------------------------------*
015600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
015700970526     C*------------------------------------------------------------------------*
015800970526     C     F24GES        BEGSR
015900970526     C*
016000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
016100970526     C*
016200970526     C                   ENDSR
016300970526     C*------------------------------------------------------------------------*
016400020415     C* TEST - OPERAZIONI DI TEST X REPERIMENTO DESTINATARIO DISAG./SUPERMERC.
016500970526     C*------------------------------------------------------------------------*
016600020415     C     TEST          BEGSR
016700970526     C*
016800020415     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
016900020415     C                   EXSR      IMPCAM
017000970526     C*
017100020415     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
017200030626     C                   CALL      'XSRLAV8'
017300030626     C                   PARM                    WDAT8
017400020415     C*
017500020415     C* IMPOSTO A VIDEO I CAMPI RESTITUITI NELLA DS DI PASSAGGIO DI PROCEDURA
017600030626     C                   EVAL      VIDGGL = GGLAV
017700020412     C*
017800020412     C                   ENDSR
017900000407     C*------------------------------------------------------------------------*
018000030626     C* IMPCAM - IMPOSTA I CAMPI DELLA DS DAL VIDEO
018100000407     C*------------------------------------------------------------------------*
018200000407     C     IMPCAM        BEGSR
018300000407     C*
018400030626     C                   Z-ADD     VIDDAD        DATADA
018500030626     C                   Z-ADD     VIDDAA        DATAA
018600030626     C                   Z-ADD     *ZEROS        GGLAV
018700000407     C*
018800000407     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
019100970526     C*------------------------------------------------------------------------*
019200970526     C     IMPFUN        BEGSR
019300970526     C*
019400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
019500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
019600970526     C                   EXSR      FUNGES
019700970526     C*
019800970526     C                   ENDSR
019900970526     C*------------------------------------------------------------------------*
020000030626     C* *INZSR - OPERAZIONI INIZIALI
020100970526     C*------------------------------------------------------------------------*
020200970526     C     *INZSR        BEGSR
020300970526     C*---
020400970526     C* RICEVIMENTO PARAMETRI
020500970526     C*---
020600970526     C     *ENTRY        PLIST
020700970612     C                   PARM                    KPJBA
020800970526     C*
020900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
021000970526     C     XMSGPA        PLIST
021100970526     C                   PARM                    DSMSMS
021200970526     C                   PARM                    DSMSPG
021300970526     C                   PARM                    DSMSKE
021400970526     C                   PARM                    DSMSER
021500970526     C*
021600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
021700970526     C                   CALL      'XKEY'
021800970526     C                   PARM                    DSKEY
021900970526     C*---
022000970526     C* VARIABILI NON RIFERITE AL DATA BASE
022100970526     C*---
022200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
022300970526     C*---
022400970526     C* VARIABILI DI CONTROLLO
022500970526     C*---
022600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
022700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
022800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
022900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
023000970526     C*
023100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
023200030626     C                   MOVEL     'XSRLAV8TR'   DSMSPG
023300030626     C                   MOVEL     'XSRLAV8TR'   MSGQUE
023400970526     C                   MOVEL     *BLANKS       MSGKEY
023500970526     C                   MOVEL     *BLANKS       DSMSMS
023600970526     C*
023700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
023800970526     C                   EXSR      IMPFUN
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
024300970526     C*------------------------------------------------------------------------*
024400970526     C     FUNGES        BEGSR
024500970526     C*
024600970526     C* PULISCO CAMPI DI LAVORO
024700970526     C                   Z-ADD     0             W                 5 0
024800970526     C*
024900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
025000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
025100970526     C     1             DO        24            I
025200970526     C                   MOVEL     FUD(I)        FUDRIG
025300970526     C*
025400970526     C* PULIZIA FUNZIONI PRECEDENTI
025500970526     C                   MOVEL     *BLANKS       FUV(I)
025600970526     C*
025700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
025800970526     C* (SALTA F24)
025900970526     C     FUDATT        IFEQ      'S'
026000970526     C     FUDFUN        ANDNE     '24'
026100970526     C                   ADD       1             W
026200970526     C                   MOVEL     FUDDES        FUV(W)
026300970526     C                   END
026400970526     C                   END                                                    DO WHILE
026500970526     C*
026600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
026700970526     C* ALTRIMENTI DISATTIVO F24
026800970526     C     1             DO        24            I
026900970526     C                   MOVEL     FUD(I)        FUDRIG
027000970526     C*
027100970526     C     FUDFUN        IFEQ      '24'
027200970526     C     W             IFGT      6
027300970526     C                   MOVEL     'S'           FUDATT
027400970526     C                   ELSE
027500970526     C                   MOVEL     *BLANKS       FUDATT
027600970526     C                   END                                                    W GT 8
027700970526     C                   MOVEL     FUDRIG        FUD(I)
027800970526     C                   END
027900970526     C                   END                                                    DO WHILE
028000970526     C*
028100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
028200970526     C     W             DIV       6             I
028300970526     C                   MVR                     Z
028400970526     C*
028500970526     C     Z             IFNE      0
028600970526     C     I             ADD       1             Z
028700970526     C                   ELSE
028800970526     C                   Z-ADD     I             Z                 5 0
028900970526     C                   END
029000970526     C*
029100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
029200970526     C                   ADD       1             FUNGRU
029300970526     C     FUNGRU        IFGT      Z
029400970526     C     FUNGRU        ORLT      1
029500970526     C                   Z-ADD     1             FUNGRU
029600970526     C                   END
029700970526     C*
029800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
029900970526     C     FUNGRU        IFEQ      1
030000970526     C                   MOVEL     FUV(1)        FUN001
030100970526     C                   MOVEL     FUV(2)        FUN002
030200970526     C                   MOVEL     FUV(3)        FUN003
030300970526     C                   MOVEL     FUV(4)        FUN004
030400970526     C                   MOVEL     FUV(5)        FUN005
030500970526     C                   MOVEL     FUV(6)        FUN006
030600970526     C                   END
030700970526     C*
030800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
030900970526     C     FUNGRU        IFEQ      2
031000970526     C                   MOVEL     FUV(7)        FUN001
031100970526     C                   MOVEL     FUV(8)        FUN002
031200970526     C                   MOVEL     FUV(9)        FUN003
031300970526     C                   MOVEL     FUV(10)       FUN004
031400970526     C                   MOVEL     FUV(11)       FUN005
031500970526     C                   MOVEL     FUV(12)       FUN006
031600970526     C                   END
031700970526     C*
031800970526     C* TERZO GRUPPO DI 6 FUNZIONI
031900970526     C     FUNGRU        IFEQ      3
032000970526     C                   MOVEL     FUV(13)       FUN001
032100970526     C                   MOVEL     FUV(14)       FUN002
032200970526     C                   MOVEL     FUV(15)       FUN003
032300970526     C                   MOVEL     FUV(16)       FUN004
032400970526     C                   MOVEL     FUV(17)       FUN005
032500970526     C                   MOVEL     FUV(18)       FUN006
032600970526     C                   END
032700970526     C*
032800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
032900970526     C     FUNGRU        IFEQ      4
033000970526     C                   MOVEL     FUV(19)       FUN001
033100970526     C                   MOVEL     FUV(20)       FUN002
033200970526     C                   MOVEL     FUV(21)       FUN003
033300970526     C                   MOVEL     FUV(22)       FUN004
033400970526     C                   MOVEL     *BLANKS       FUN005
033500970526     C                   MOVEL     *BLANKS       FUN006
033600970526     C                   END
033700970526     C*
033800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
033900970526     C                   MOVEL     *BLANKS       FUN007
034000970526     C                   MOVEL     *BLANKS       FUN008
034100970526     C*
034200970526     C     1             DO        24            I
034300970526     C                   MOVEL     FUD(I)        FUDRIG
034400970526     C     FUDFUN        IFEQ      '24'
034500970526     C     FUDATT        ANDEQ     'S'
034600970526     C                   MOVEL     FUDDES        FUN008
034700970526     C                   END
034800970526     C                   END                                                    DO
034900970526     C*
035000970526     C                   ENDSR
035100970526     C*------------------------------------------------------------------------*
035200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
035300970526     C*------------------------------------------------------------------------*
035400970526     C     FUNCON        BEGSR
035500970526     C*
035600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
035700970526     C                   EXSR      FUNVAR
035800970526     C*
035900970526     C* PULISCO CAMPI DI LAVORO
036000970526     C                   Z-ADD     0             I                 5 0
036100970526     C                   Z-ADD     0             W                 5 0
036200970526     C*
036300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
036400970526     C     FUNKEY        IFNE      *BLANKS
036500970526     C     1             DO        24            I
036600970526     C                   MOVEL     FUD(I)        FUDRIG
036700970526     C*
036800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
036900970526     C     FUNKEY        IFEQ      FUDFUN
037000970526     C     FUDATT        ANDEQ     'S'
037100970526     C                   Z-ADD     I             W
037200970526     C                   END
037300970526     C                   END                                                    DO WHILE
037400970526     C*
037500970526     C* FUNZIONE NON ATTIVA (ERRORE)
037600970526     C     W             IFEQ      0
037700970526     C*
037800970526     C* EMISSIONE MESSAGGIO DI ERRORE
037900970526     C                   MOVEL     MSG(1)        DSMSMS
038000970526     C*
038100970526     C                   END                                                    W=0
038200970526     C                   END                                                    FUNKEY NE *BLANKS
038300970526     C*
038400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
038500970526     C     W             IFNE      0
038600970526     C     FUNKEY        OREQ      *BLANKS
038700970526     C     FUNKEY        IFEQ      *BLANKS
038800970526     C                   EXSR      ENTGES
038900970526     C                   END
039000970526     C     FUNKEY        IFEQ      '03'
039100970526     C                   EXSR      F03GES
039200970526     C                   END
039300970526     C     FUNKEY        IFEQ      '12'
039400970526     C                   EXSR      F12GES
039500970526     C                   END
039600970526     C     FUNKEY        IFEQ      '24'
039700970526     C                   EXSR      F24GES
039800970526     C                   END
039900970526     C                   END
040000970526     C*
040100970526     C                   ENDSR
040200970526     C*------------------------------------------------------------------------*
040300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
040400970526     C*------------------------------------------------------------------------*
040500970526     C     FUNVAR        BEGSR
040600970526     C*
040700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
040800970526     C*
040900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
041000970526     C     KEY           IFEQ      ENTER
041100970526     C                   MOVEL     *BLANKS       FUNKEY
041200970526     C                   END
041300970526     C     KEY           IFEQ      F1
041400970526     C                   MOVEL     '01'          FUNKEY
041500970526     C                   END
041600970526     C     KEY           IFEQ      F2
041700970526     C                   MOVEL     '02'          FUNKEY
041800970526     C                   END
041900970526     C     KEY           IFEQ      F3
042000970526     C                   MOVEL     '03'          FUNKEY
042100970526     C                   END
042200970526     C     KEY           IFEQ      F4
042300970526     C                   MOVEL     '04'          FUNKEY
042400970526     C                   END
042500970526     C     KEY           IFEQ      F5
042600970526     C                   MOVEL     '05'          FUNKEY
042700970526     C                   END
042800970526     C     KEY           IFEQ      F6
042900970526     C                   MOVEL     '06'          FUNKEY
043000970526     C                   END
043100970526     C     KEY           IFEQ      F7
043200970526     C                   MOVEL     '07'          FUNKEY
043300970526     C                   END
043400970526     C     KEY           IFEQ      F8
043500970526     C                   MOVEL     '08'          FUNKEY
043600970526     C                   END
043700970526     C     KEY           IFEQ      F9
043800970526     C                   MOVEL     '09'          FUNKEY
043900970526     C                   END
044000970526     C     KEY           IFEQ      F10
044100970526     C                   MOVEL     '10'          FUNKEY
044200970526     C                   END
044300970526     C     KEY           IFEQ      F11
044400970526     C                   MOVEL     '11'          FUNKEY
044500970526     C                   END
044600970526     C     KEY           IFEQ      F12
044700970526     C                   MOVEL     '12'          FUNKEY
044800970526     C                   END
044900970526     C     KEY           IFEQ      F13
045000970526     C                   MOVEL     '13'          FUNKEY
045100970526     C                   END
045200970526     C     KEY           IFEQ      F14
045300970526     C                   MOVEL     '14'          FUNKEY
045400970526     C                   END
045500970526     C     KEY           IFEQ      F15
045600970526     C                   MOVEL     '15'          FUNKEY
045700970526     C                   END
045800970526     C     KEY           IFEQ      F16
045900970526     C                   MOVEL     '16'          FUNKEY
046000970526     C                   END
046100970526     C     KEY           IFEQ      F17
046200970526     C                   MOVEL     '17'          FUNKEY
046300970526     C                   END
046400970526     C     KEY           IFEQ      F18
046500970526     C                   MOVEL     '18'          FUNKEY
046600970526     C                   END
046700970526     C     KEY           IFEQ      F19
046800970526     C                   MOVEL     '19'          FUNKEY
046900970526     C                   END
047000970526     C     KEY           IFEQ      F20
047100970526     C                   MOVEL     '20'          FUNKEY
047200970526     C                   END
047300970526     C     KEY           IFEQ      F21
047400970526     C                   MOVEL     '21'          FUNKEY
047500970526     C                   END
047600970526     C     KEY           IFEQ      F22
047700970526     C                   MOVEL     '22'          FUNKEY
047800970526     C                   END
047900970526     C     KEY           IFEQ      F23
048000970526     C                   MOVEL     '23'          FUNKEY
048100970526     C                   END
048200970526     C     KEY           IFEQ      F24
048300970526     C                   MOVEL     '24'          FUNKEY
048400970526     C                   END
048500970526     C     KEY           IFEQ      ROLLUP
048600970526     C                   MOVEL     'RU'          FUNKEY
048700970526     C                   END
048800970526     C     KEY           IFEQ      HELP
048900970526     C                   MOVEL     'HP'          FUNKEY
049000970526     C                   END
049100970526     C                   ENDSR
049200970526     C*------------------------------------------------------------------------*
049300970526     C* NOTE COSTRUZIONE SCHIERE
049400970526     C*------------------------------------------------------------------------*
049500970526     C*
049600970526     C* -MSG-MESSAGGI
049700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
049800970526     C* (NON TOCCARE MSG,1 E MSG,2)
049900970526     C*
050000970526     C* -CMD-COMANDI
050100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
050200970526     C*
050300970526     C* -OPD-OPZIONI
050400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
050500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
050600970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
050700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
050800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
050900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
051000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
051100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
051200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
051300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
051400970526     C*
051500970526     C* -FUD-FUNZIONI
051600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
051700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
051800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
051900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
052000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
052100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
052200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
052300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
052400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
052500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
052600970526     O*------------------------------------------------------------------------*
052700970528** MSG - MESSAGGI
052800030626XSRLAV8- Tasto funzionale non gestito                                          1
052900030626                                                                               2
053000030626                                                                               3
053100030626                                                                               4
053200030626                                                                               5
053300030626                                                                               6
053400030626                                                                               7
053500030626                                                                               8
053600030626                                                                               9
053700030626                                                                               10
053800030626                                                                               11
053900030626                                                                               12
054000030626                                                                               13
054100030626                                                                               14
054200030626                                                                               15
054300030626                                                                               16
054400030626                                                                               17
054500030626                                                                               18
054600020415                                                                               19
054700020415                                                                               20
054800970526** FUD - FUNZIONI
054900970526                   -        (S/N)=N                        funz.01¬
055000970526                   -        (S/N)=N                        funz.02¬
055100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
055200970526                   -        (S/N)=N                        funz.04¬
055300970526                   -        (S/N)=N                        funz.05¬
055400000407F06=Conferma       -        (S/N)=S                        funz.06¬
055500970527                   -        (S/N)=N                        funz.07¬
055600970527                   -        (S/N)=N                        funz.08¬
055700970527                   -        (S/N)=N                        funz.09¬
055800970527                   -        (S/N)=N                        funz.10¬
055900970526                   -        (S/N)=N                        funz.11¬
056000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
056100970526                   -        (S/N)=N                        funz.13¬
056200970526                   -        (S/N)=N                        funz.14¬
056300970526                   -        (S/N)=N                        funz.15¬
056400970526                   -        (S/N)=N                        funz.16¬
056500970526                   -        (S/N)=N                        funz.17¬
056600970526                   -        (S/N)=N                        funz.18¬
056700970526                   -        (S/N)=N                        funz.19¬
056800970526                   -        (S/N)=N                        funz.20¬
056900970526                   -        (S/N)=N                        funz.21¬
057000970526                   -        (S/N)=N                        funz.22¬
057100970526                   -        (S/N)=N                        funz.23¬
057200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
