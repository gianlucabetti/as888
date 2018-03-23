000100970526     H*------------------------------------------------------------------------*
000200030929     H* TEST - CALCOLO CHKDGT FORMATO "INTERLEAVE 2/5"
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030929     FTRULIL25D CF   E             WORKSTN INFDS(DEVDS1)
000900030929     F                                     SFILE(IL25DMS:MSGULT)
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
005000020415     D*------------
005100970526     D* ARCHITETTURA
005200970526     D*------------
005300970526     D KPJBA         E DS
005400970526     C*------------------------------------------------------------------------*
005500970526     C* MAIN LINES
005600970526     C*------------------------------------------------------------------------*
005700970526     C*
005800970526     C* NON CI SONO ERRORI IN ENTRATA
005900970526IF  1C     ERRL00        IFEQ      '0'
006000970526     C*
006100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
006300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
006400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
006500970526     C*
006600970526     C* EMETTE FORMATO VIDEO
006700970526     C                   EXSR      BUTFMR
006800970526     C*
006900970526     C* GESTIONE TASTI FUNZIONALI
007000970526     C                   EXSR      FUNCON
007100970526E   2C                   ENDDO
007200970526     C*
007300970526E   1C                   ENDIF
007400970526     C* FINE PGM
007500970526     C                   SETON                                        LR
007600970526     C*------------------------------------------------------------------------*
007700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007800970526     C*------------------------------------------------------------------------*
007900970526     C     BUTFMR        BEGSR
008000970526     C*
008100970526     C* EMETTE LA TESTATA
008200030929     C                   WRITE     IL25D01
008300970526     C*
008400970526     C* SPEDISCE EVENTUALE MESSAGGIO
008500970526     C                   EXSR      SNDMSG
008600970526     C*
008700970526     C* EMETTE LE FUNZIONI
008800030929     C                   WRITE     IL25D02
008900970526     C*
009000970526     C* EMETTE IL VIDEO
009100030929     C                   EXFMT     IL25D03
009200970526     C*
009300970526     C* REIMPOSTA FLAG DI ERRORE
009400970526     C                   MOVEL     '0'           ERRL00
009500970526     C*
009600970526     C                   ENDSR
009700970526     C*------------------------------------------------------------------------*
009800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
009900970526     C*------------------------------------------------------------------------*
010000970526     C     SNDMSG        BEGSR
010100970526     C*
010200970526     C* PULISCE IL SFILE MESSAGGI
010300970526     C                   MOVEL     '1'           *IN40
010400030929     C                   WRITE     IL25DMC
010500970526     C                   MOVEL     '0'           *IN40
010600970526     C*
010700970526     C* VISUALIZZA IL MESSAGGIO
010800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
010900970526     C                   CALL      'XMSG'        XMSGPA
011000970526IF  1C     DSMSER        IFEQ      *ZEROS
011100970526     C                   MOVEL     DSMSKE        MSGKEY
011200970526E   1C                   ENDIF
011300970526     C                   Z-ADD     1             MSGULT
011400030929     C                   WRITE     IL25DMS
011500030929     C                   WRITE     IL25DMC
011600970526     C*
011700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011800970526     C                   MOVEL     *BLANKS       DSMSMS
011900970526     C*
012000970526     C                   ENDSR
012100970526     C*------------------------------------------------------------------------*
012200970526     C* ENTGES - GESTIONE ENTER -> INVIO
012300970526     C*------------------------------------------------------------------------*
012400970526     C     ENTGES        BEGSR
012500970526     C*
012600020415     C                   EXSR      TEST
012700970526     C*
012800970526     C                   ENDSR
012900970526     C*------------------------------------------------------------------------*
013000970526     C* F03GES - GESTIONE F03 -> FINE
013100970526     C*------------------------------------------------------------------------*
013200970526     C     F03GES        BEGSR
013300970526     C*
013400970526     C* IMPOSTA "PREMUTO F03"
013500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
013600970526     C*
013700970526     C                   ENDSR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* F12GES - GESTIONE F12 -> RITORNO
014000970526     C*------------------------------------------------------------------------*
014100970526     C     F12GES        BEGSR
014200970526     C*
014300970526     C* IMPOSTA "PREMUTO F12"
014400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
014500970526     C*
014600970526     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F24GES        BEGSR
015100970526     C*
015200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
015300970526     C*
015400970526     C                   ENDSR
015500970526     C*------------------------------------------------------------------------*
015600020415     C* TEST - OPERAZIONI DI TEST X REPERIMENTO DESTINATARIO DISAG./SUPERMERC.
015700970526     C*------------------------------------------------------------------------*
015800020415     C     TEST          BEGSR
015900970526     C*
016000020415     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
016100030929     C                   CALL      'TRULIL25R'
016200030929     C                   PARM                    VIDCOD
016300030929     C                   PARM                    VIDBCD
016400020412     C*
016500020412     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     IMPFUN        BEGSR
017000970526     C*
017100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
017200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
017300970526     C                   EXSR      FUNGES
017400970526     C*
017500970526     C                   ENDSR
017600970526     C*------------------------------------------------------------------------*
017700030626     C* *INZSR - OPERAZIONI INIZIALI
017800970526     C*------------------------------------------------------------------------*
017900970526     C     *INZSR        BEGSR
018000970526     C*---
018100970526     C* RICEVIMENTO PARAMETRI
018200970526     C*---
018300970526     C     *ENTRY        PLIST
018400970612     C                   PARM                    KPJBA
018500970526     C*
018600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
018700970526     C     XMSGPA        PLIST
018800970526     C                   PARM                    DSMSMS
018900970526     C                   PARM                    DSMSPG
019000970526     C                   PARM                    DSMSKE
019100970526     C                   PARM                    DSMSER
019200970526     C*
019300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
019400970526     C                   CALL      'XKEY'
019500970526     C                   PARM                    DSKEY
019600970526     C*---
019700970526     C* VARIABILI NON RIFERITE AL DATA BASE
019800970526     C*---
019900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
020000970526     C*---
020100970526     C* VARIABILI DI CONTROLLO
020200970526     C*---
020300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
020400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
020500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
020600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
020700970526     C*
020800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
020900030929     C                   MOVEL     'TRULIL25R'   DSMSPG
021000030929     C                   MOVEL     'TRULIL25R'   MSGQUE
021100970526     C                   MOVEL     *BLANKS       MSGKEY
021200970526     C                   MOVEL     *BLANKS       DSMSMS
021300970526     C*
021400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
021500970526     C                   EXSR      IMPFUN
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
022000970526     C*------------------------------------------------------------------------*
022100970526     C     FUNGES        BEGSR
022200970526     C*
022300970526     C* PULISCO CAMPI DI LAVORO
022400970526     C                   Z-ADD     0             W                 5 0
022500970526     C*
022600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
022700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
022800970526     C     1             DO        24            I
022900970526     C                   MOVEL     FUD(I)        FUDRIG
023000970526     C*
023100970526     C* PULIZIA FUNZIONI PRECEDENTI
023200970526     C                   MOVEL     *BLANKS       FUV(I)
023300970526     C*
023400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
023500970526     C* (SALTA F24)
023600970526     C     FUDATT        IFEQ      'S'
023700970526     C     FUDFUN        ANDNE     '24'
023800970526     C                   ADD       1             W
023900970526     C                   MOVEL     FUDDES        FUV(W)
024000970526     C                   END
024100970526     C                   END                                                    DO WHILE
024200970526     C*
024300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
024400970526     C* ALTRIMENTI DISATTIVO F24
024500970526     C     1             DO        24            I
024600970526     C                   MOVEL     FUD(I)        FUDRIG
024700970526     C*
024800970526     C     FUDFUN        IFEQ      '24'
024900970526     C     W             IFGT      6
025000970526     C                   MOVEL     'S'           FUDATT
025100970526     C                   ELSE
025200970526     C                   MOVEL     *BLANKS       FUDATT
025300970526     C                   END                                                    W GT 8
025400970526     C                   MOVEL     FUDRIG        FUD(I)
025500970526     C                   END
025600970526     C                   END                                                    DO WHILE
025700970526     C*
025800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
025900970526     C     W             DIV       6             I
026000970526     C                   MVR                     Z
026100970526     C*
026200970526     C     Z             IFNE      0
026300970526     C     I             ADD       1             Z
026400970526     C                   ELSE
026500970526     C                   Z-ADD     I             Z                 5 0
026600970526     C                   END
026700970526     C*
026800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
026900970526     C                   ADD       1             FUNGRU
027000970526     C     FUNGRU        IFGT      Z
027100970526     C     FUNGRU        ORLT      1
027200970526     C                   Z-ADD     1             FUNGRU
027300970526     C                   END
027400970526     C*
027500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
027600970526     C     FUNGRU        IFEQ      1
027700970526     C                   MOVEL     FUV(1)        FUN001
027800970526     C                   MOVEL     FUV(2)        FUN002
027900970526     C                   MOVEL     FUV(3)        FUN003
028000970526     C                   MOVEL     FUV(4)        FUN004
028100970526     C                   MOVEL     FUV(5)        FUN005
028200970526     C                   MOVEL     FUV(6)        FUN006
028300970526     C                   END
028400970526     C*
028500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
028600970526     C     FUNGRU        IFEQ      2
028700970526     C                   MOVEL     FUV(7)        FUN001
028800970526     C                   MOVEL     FUV(8)        FUN002
028900970526     C                   MOVEL     FUV(9)        FUN003
029000970526     C                   MOVEL     FUV(10)       FUN004
029100970526     C                   MOVEL     FUV(11)       FUN005
029200970526     C                   MOVEL     FUV(12)       FUN006
029300970526     C                   END
029400970526     C*
029500970526     C* TERZO GRUPPO DI 6 FUNZIONI
029600970526     C     FUNGRU        IFEQ      3
029700970526     C                   MOVEL     FUV(13)       FUN001
029800970526     C                   MOVEL     FUV(14)       FUN002
029900970526     C                   MOVEL     FUV(15)       FUN003
030000970526     C                   MOVEL     FUV(16)       FUN004
030100970526     C                   MOVEL     FUV(17)       FUN005
030200970526     C                   MOVEL     FUV(18)       FUN006
030300970526     C                   END
030400970526     C*
030500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
030600970526     C     FUNGRU        IFEQ      4
030700970526     C                   MOVEL     FUV(19)       FUN001
030800970526     C                   MOVEL     FUV(20)       FUN002
030900970526     C                   MOVEL     FUV(21)       FUN003
031000970526     C                   MOVEL     FUV(22)       FUN004
031100970526     C                   MOVEL     *BLANKS       FUN005
031200970526     C                   MOVEL     *BLANKS       FUN006
031300970526     C                   END
031400970526     C*
031500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
031600970526     C                   MOVEL     *BLANKS       FUN007
031700970526     C                   MOVEL     *BLANKS       FUN008
031800970526     C*
031900970526     C     1             DO        24            I
032000970526     C                   MOVEL     FUD(I)        FUDRIG
032100970526     C     FUDFUN        IFEQ      '24'
032200970526     C     FUDATT        ANDEQ     'S'
032300970526     C                   MOVEL     FUDDES        FUN008
032400970526     C                   END
032500970526     C                   END                                                    DO
032600970526     C*
032700970526     C                   ENDSR
032800970526     C*------------------------------------------------------------------------*
032900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
033000970526     C*------------------------------------------------------------------------*
033100970526     C     FUNCON        BEGSR
033200970526     C*
033300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
033400970526     C                   EXSR      FUNVAR
033500970526     C*
033600970526     C* PULISCO CAMPI DI LAVORO
033700970526     C                   Z-ADD     0             I                 5 0
033800970526     C                   Z-ADD     0             W                 5 0
033900970526     C*
034000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
034100970526     C     FUNKEY        IFNE      *BLANKS
034200970526     C     1             DO        24            I
034300970526     C                   MOVEL     FUD(I)        FUDRIG
034400970526     C*
034500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
034600970526     C     FUNKEY        IFEQ      FUDFUN
034700970526     C     FUDATT        ANDEQ     'S'
034800970526     C                   Z-ADD     I             W
034900970526     C                   END
035000970526     C                   END                                                    DO WHILE
035100970526     C*
035200970526     C* FUNZIONE NON ATTIVA (ERRORE)
035300970526     C     W             IFEQ      0
035400970526     C*
035500970526     C* EMISSIONE MESSAGGIO DI ERRORE
035600970526     C                   MOVEL     MSG(1)        DSMSMS
035700970526     C*
035800970526     C                   END                                                    W=0
035900970526     C                   END                                                    FUNKEY NE *BLANKS
036000970526     C*
036100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
036200970526     C     W             IFNE      0
036300970526     C     FUNKEY        OREQ      *BLANKS
036400970526     C     FUNKEY        IFEQ      *BLANKS
036500970526     C                   EXSR      ENTGES
036600970526     C                   END
036700970526     C     FUNKEY        IFEQ      '03'
036800970526     C                   EXSR      F03GES
036900970526     C                   END
037000970526     C     FUNKEY        IFEQ      '12'
037100970526     C                   EXSR      F12GES
037200970526     C                   END
037300970526     C     FUNKEY        IFEQ      '24'
037400970526     C                   EXSR      F24GES
037500970526     C                   END
037600970526     C                   END
037700970526     C*
037800970526     C                   ENDSR
037900970526     C*------------------------------------------------------------------------*
038000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
038100970526     C*------------------------------------------------------------------------*
038200970526     C     FUNVAR        BEGSR
038300970526     C*
038400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
038500970526     C*
038600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
038700970526     C     KEY           IFEQ      ENTER
038800970526     C                   MOVEL     *BLANKS       FUNKEY
038900970526     C                   END
039000970526     C     KEY           IFEQ      F1
039100970526     C                   MOVEL     '01'          FUNKEY
039200970526     C                   END
039300970526     C     KEY           IFEQ      F2
039400970526     C                   MOVEL     '02'          FUNKEY
039500970526     C                   END
039600970526     C     KEY           IFEQ      F3
039700970526     C                   MOVEL     '03'          FUNKEY
039800970526     C                   END
039900970526     C     KEY           IFEQ      F4
040000970526     C                   MOVEL     '04'          FUNKEY
040100970526     C                   END
040200970526     C     KEY           IFEQ      F5
040300970526     C                   MOVEL     '05'          FUNKEY
040400970526     C                   END
040500970526     C     KEY           IFEQ      F6
040600970526     C                   MOVEL     '06'          FUNKEY
040700970526     C                   END
040800970526     C     KEY           IFEQ      F7
040900970526     C                   MOVEL     '07'          FUNKEY
041000970526     C                   END
041100970526     C     KEY           IFEQ      F8
041200970526     C                   MOVEL     '08'          FUNKEY
041300970526     C                   END
041400970526     C     KEY           IFEQ      F9
041500970526     C                   MOVEL     '09'          FUNKEY
041600970526     C                   END
041700970526     C     KEY           IFEQ      F10
041800970526     C                   MOVEL     '10'          FUNKEY
041900970526     C                   END
042000970526     C     KEY           IFEQ      F11
042100970526     C                   MOVEL     '11'          FUNKEY
042200970526     C                   END
042300970526     C     KEY           IFEQ      F12
042400970526     C                   MOVEL     '12'          FUNKEY
042500970526     C                   END
042600970526     C     KEY           IFEQ      F13
042700970526     C                   MOVEL     '13'          FUNKEY
042800970526     C                   END
042900970526     C     KEY           IFEQ      F14
043000970526     C                   MOVEL     '14'          FUNKEY
043100970526     C                   END
043200970526     C     KEY           IFEQ      F15
043300970526     C                   MOVEL     '15'          FUNKEY
043400970526     C                   END
043500970526     C     KEY           IFEQ      F16
043600970526     C                   MOVEL     '16'          FUNKEY
043700970526     C                   END
043800970526     C     KEY           IFEQ      F17
043900970526     C                   MOVEL     '17'          FUNKEY
044000970526     C                   END
044100970526     C     KEY           IFEQ      F18
044200970526     C                   MOVEL     '18'          FUNKEY
044300970526     C                   END
044400970526     C     KEY           IFEQ      F19
044500970526     C                   MOVEL     '19'          FUNKEY
044600970526     C                   END
044700970526     C     KEY           IFEQ      F20
044800970526     C                   MOVEL     '20'          FUNKEY
044900970526     C                   END
045000970526     C     KEY           IFEQ      F21
045100970526     C                   MOVEL     '21'          FUNKEY
045200970526     C                   END
045300970526     C     KEY           IFEQ      F22
045400970526     C                   MOVEL     '22'          FUNKEY
045500970526     C                   END
045600970526     C     KEY           IFEQ      F23
045700970526     C                   MOVEL     '23'          FUNKEY
045800970526     C                   END
045900970526     C     KEY           IFEQ      F24
046000970526     C                   MOVEL     '24'          FUNKEY
046100970526     C                   END
046200970526     C     KEY           IFEQ      ROLLUP
046300970526     C                   MOVEL     'RU'          FUNKEY
046400970526     C                   END
046500970526     C     KEY           IFEQ      HELP
046600970526     C                   MOVEL     'HP'          FUNKEY
046700970526     C                   END
046800970526     C                   ENDSR
046900970526     C*------------------------------------------------------------------------*
047000970526     C* NOTE COSTRUZIONE SCHIERE
047100970526     C*------------------------------------------------------------------------*
047200970526     C*
047300970526     C* -MSG-MESSAGGI
047400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
047500970526     C* (NON TOCCARE MSG,1 E MSG,2)
047600970526     C*
047700970526     C* -CMD-COMANDI
047800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
047900970526     C*
048000970526     C* -OPD-OPZIONI
048100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
048200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
048300970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
048400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
048500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
048600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
048700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
048800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
048900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
049000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
049100970526     C*
049200970526     C* -FUD-FUNZIONI
049300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
049400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
049500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
049600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
049700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
049800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
049900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
050000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
050100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
050200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
050300970526     O*------------------------------------------------------------------------*
050400970528** MSG - MESSAGGI
050500030929TRULIL25R - Tasto funzionale non gestito                                       1
050600030626                                                                               2
050700030626                                                                               3
050800030626                                                                               4
050900030626                                                                               5
051000030626                                                                               6
051100030626                                                                               7
051200030626                                                                               8
051300030626                                                                               9
051400030626                                                                               10
051500030626                                                                               11
051600030626                                                                               12
051700030626                                                                               13
051800030626                                                                               14
051900030626                                                                               15
052000030626                                                                               16
052100030626                                                                               17
052200030626                                                                               18
052300020415                                                                               19
052400020415                                                                               20
052500970526** FUD - FUNZIONI
052600970526                   -        (S/N)=N                        funz.01¬
052700970526                   -        (S/N)=N                        funz.02¬
052800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
052900970526                   -        (S/N)=N                        funz.04¬
053000970526                   -        (S/N)=N                        funz.05¬
053100000407F06=Conferma       -        (S/N)=S                        funz.06¬
053200970527                   -        (S/N)=N                        funz.07¬
053300970527                   -        (S/N)=N                        funz.08¬
053400970527                   -        (S/N)=N                        funz.09¬
053500970527                   -        (S/N)=N                        funz.10¬
053600970526                   -        (S/N)=N                        funz.11¬
053700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
053800970526                   -        (S/N)=N                        funz.13¬
053900970526                   -        (S/N)=N                        funz.14¬
054000970526                   -        (S/N)=N                        funz.15¬
054100970526                   -        (S/N)=N                        funz.16¬
054200970526                   -        (S/N)=N                        funz.17¬
054300970526                   -        (S/N)=N                        funz.18¬
054400970526                   -        (S/N)=N                        funz.19¬
054500970526                   -        (S/N)=N                        funz.20¬
054600970526                   -        (S/N)=N                        funz.21¬
054700970526                   -        (S/N)=N                        funz.22¬
054800970526                   -        (S/N)=N                        funz.23¬
054900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
