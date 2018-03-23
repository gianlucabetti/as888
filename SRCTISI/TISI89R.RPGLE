000100000000      ****************************************************************
000200970619      * GESTISCE PER PROVA TUTTE LA FUNZIONI ROUTINE 'TISI88R'       *
000300970619      *      CHE GENERA TUTTI I SINONIMI DI UNA LOCALITA'            *
000400000000      ****************************************************************
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F* FILE VIDEO (SUBFILE DATI)
000700970619     FTISI89D   CF   E             WORKSTN INFDS(DEVDS1)
000800970619     F                                     SFILE(SI89DMS:MSGULT)
000900000000     D*****************************************************************
001000000000     D* MESSAGGI
001100930208     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001200000000     D* DESCRIZIONI TASTI FUNZIONALI
001300000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
001500000000     D FUV             S             19    DIM(24)
001600970620     D*****************************************************************
001700970623     D sin             S             35    DIM(60)
001800000000     D*****************************************************************
001900000000     D* RIDEFINIZIONE SCHIERA -FUD-
002000000000     D FUDDS           DS
002100000000     D*                -DESCRIZIONE PARZIALE
002200000000     D  FUDDE1                 1      1
002300000000     D*                -FUNZIONE
002400000000     D  FUDFUN                 2      3
002500000000     D*                -DESCRIZIONE PARZIALE
002600000000     D  FUDDE2                 4     19
002700000000     D*                -FILLER
002800000000     D  FUDFI0                20     34
002900000000     D*                -FLAG FUNZ.ATTIVA (S/N)
003000000000     D  FUDATT                35     35
003100000000     D*                -FILLER
003200000000     D  FUDFI1                36     80
003300000000     D*                -DESCRIZIONE COMPLETA
003400000000     D  FUDDES                 1     19
003500000000     D*                -RIDEFINIZIONE RIGA
003600000000     D  FUDRIG                 1     80
003700930201     D*****************************************************************
003800930201     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003900930201     D DSMSG         E DS
004000930305     D*****************************************************************
004100970619     D* PARAMETRI CHIAMATA PGM TISI88R (LOC->SIN)
004200970619     D SI88          E DS                  EXTNAME(TISI88DS)
004300930201     D*****************************************************************
004400930201     D* DS PER GESTIONE TASTI FUNZIONALI
004500930201     D DSKEY         E DS
004600930201     D DEVDS1          DS
004700930201     D  KEY                  369    369
004800950407     D***************************************************************
004900950407     D* DS PER LANCIO PGM DATA->N.GIORNI (XSRDA8)
005000950407     D G02PAR          DS
005100950407     D  G02DAT                 1      8  0
005200950407     D  G02INV                 9     16  0
005300950407     D  G02ERR                17     17
005400950407     D  G02TGI                18     22  0
005500930201     D***************************************************************
005600930201     D* ARCHITETTURA
005700930201     D KPJBA         E DS
005800000000     C*================================================================
005900000000     C* MAIN DEL PGM
006000000000     C                   EXSR      RUTINZ
006100000000     C*
006200000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
006300000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
006400000000     C*
006500000000     C* CICLO FINO A CHE:
006600000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
006700000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
006800930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
006900000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
007000000000     C     F12L00        OREQ      '1'
007100930202     C     FLFINE        OREQ      '1'
007200000000     C*
007300000000     C* EMETTE FORMATO VIDEO
007400000000     C                   EXSR      BUTFMR
007500000000     C*
007600000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
007700000000     C                   EXSR      FUNCON
007800000000     C*
007900000000     C                   END                                                    <-- 2 --
008000000000     C*
008100000000     C                   END                                                    <-- 1 --
008200000000     C*
008300000000     C                   SETON                                        LR
008400000000     C*FINE MAIN DEL PGM
008500000000     C*================================================================
008600000000     C* EMISSIONE FORMATO A VIDEO
008700000000     C*================================================================
008800000000     C     BUTFMR        BEGSR
008900930204     C*
009000930204     C* WRITE RECORD TESTATA
009100970619     C                   WRITE     SI89D01
009200930205     C*
009300930205     C* VISUALIZZAZIONE MESSAGGIO
009400930205     C                   EXSR      SNDMSG
009500930204     C*
009600930204     C* WRITE RECORD FUNZIONI
009700970619     C                   WRITE     SI89D02
009800000000     C*
009900930204     C* EXFMT RECORD DETTAGLIO
010000970619     C                   EXFMT     SI89D03
010100000000     C*
010200000000     C* AZZERO FLAG DI ERRORE LIVELLO
010300000000     C                   MOVEL     '0'           ERRL00
010400000000     C*
010500000000     C                   ENDSR
010600000000     C*================================================================
010700000000     C* SNDMSG - MANDA MESSAGGIO
010800000000     C*================================================================
010900000000     C     SNDMSG        BEGSR
011000000000     C*
011100000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
011200000000     C                   MOVEL     '1'           *IN40
011300970619     C                   WRITE     SI89DMC
011400000000     C                   MOVEL     '0'           *IN40
011500000000     C*
011600000000     C* VISUALIZZAZIONE MESSAGGIO
011700000000     C                   Z-ADD     0             MSGULT            4 0
011800930202     C                   CALL      'XMSG'        XMSGPA
011900930201     C     DSMSER        IFEQ      0
012000930201     C                   MOVEL     DSMSKE        MSGKEY
012100000000     C                   END
012200000000     C                   Z-ADD     1             MSGULT
012300970619     C                   WRITE     SI89DMS                                      SFL MESS
012400970619     C                   WRITE     SI89DMC                                      CONTR MESS
012500000000     C*
012600000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
012700930201     C                   MOVEL     *BLANKS       DSMSMS
012800000000     C*
012900000000     C                   ENDSR
013000000000     C*================================================================
013100000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
013200000000     C*================================================================
013300000000     C     ENTGES        BEGSR
013400950407     C*
013500930201     C*
013600930326     C* LANCIA ROUTINE
013700950407     C     ERRL00        IFEQ      '0'                                          --- 1 -->
013800970619     C                   CALL      'TISI88R'
013900970619     C                   PARM                    SI88
014000950407     C                   ENDIF                                                  <-- 1 ---
014100970620     c*
014200970620     c                   MOVEA     o88sin        sin
014300970620     c                   MOVEL     sin(1)        o88s01
014400970620     c                   MOVEL     sin(2)        o88s02
014500970620     c                   MOVEL     sin(3)        o88s03
014600970620     c                   MOVEL     sin(4)        o88s04
014700970620     c                   MOVEL     sin(5)        o88s05
014800970620     c                   MOVEL     sin(6)        o88s06
014900970620     c                   MOVEL     sin(7)        o88s07
015000970620     c                   MOVEL     sin(8)        o88s08
015100970620     c                   MOVEL     sin(9)        o88s09
015200970620     c                   MOVEL     sin(10)       o88s10
015300970620     c                   MOVEL     sin(11)       o88s11
015400970620     c                   MOVEL     sin(12)       o88s12
015500970620     c                   MOVEL     sin(13)       o88s13
015600970620     c                   MOVEL     sin(14)       o88s14
015700970620     c                   MOVEL     sin(15)       o88s15
015800970620     c                   MOVEL     sin(16)       o88s16
015900970620     c*                  MOVEL     sin(17)       o88s17
016000970620     c*                  MOVEL     sin(18)       o88s18
016100970620     c*                  MOVEL     sin(19)       o88s19
016200970620     c*                  MOVEL     sin(20)       o88s20
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*================================================================
016600930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
016700000000     C*================================================================
016800000000     C     F01GES        BEGSR
016900000000     C                   ENDSR
017000000000     C*================================================================
017100930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017200000000     C*================================================================
017300000000     C     F03GES        BEGSR
017400000000     C*
017500930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017600000000     C                   MOVEL     '1'           F03L00
017700000000     C*
017800000000     C                   ENDSR
017900930201     C*================================================================
018000930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018100930201     C*================================================================
018200930201     C     F06GES        BEGSR
018300930201     C                   ENDSR
018400000000     C*================================================================
018500930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
018600000000     C*================================================================
018700000000     C     F07GES        BEGSR
018800000000     C                   ENDSR
018900000000     C*================================================================
019000930210     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
019100000000     C*================================================================
019200000000     C     F08GES        BEGSR
019300000000     C                   ENDSR
019400000000     C*================================================================
019500930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
019600000000     C*================================================================
019700000000     C     F12GES        BEGSR
019800000000     C*
019900000000     C* IMPOSTO FLAG DI LIVELLO
020000000000     C                   MOVEL     '1'           F12L00
020100000000     C*
020200000000     C                   ENDSR
020300930201     C*================================================================
020400930210     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ELENCO
020500930201     C*================================================================
020600930201     C     F21GES        BEGSR
020700930201     C                   ENDSR
020800000000     C*================================================================
020900930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
021000000000     C*================================================================
021100000000     C     F22GES        BEGSR
021200000000     C                   ENDSR
021300000000     C*================================================================
021400930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
021500000000     C*================================================================
021600000000     C     F24GES        BEGSR
021700000000     C*
021800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
021900000000     C                   EXSR      FUNGES
022000000000     C*
022100000000     C                   ENDSR
022200000000     C*================================================================
022300000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
022400000000     C*================================================================
022500000000     C     HELGES        BEGSR
022600000000     C                   ENDSR
022700000000     C*================================================================
022800000000     C*  RUTINZ - ROUTINE INIZIALE
022900000000     C*================================================================
023000000000     C     RUTINZ        BEGSR
023100930401     C*
023200930401     C* RICEVIMENTO PARAMETRI
023300930401     C     *ENTRY        PLIST
023400930401     C                   PARM                    KPJBA
023500930201     C*
023600930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
023700930201     C     XMSGPA        PLIST
023800930201     C                   PARM                    DSMSMS
023900930201     C                   PARM                    DSMSPG
024000930201     C                   PARM                    DSMSKE
024100930201     C                   PARM                    DSMSER
024200930201     C*
024300930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
024400930201     C                   CALL      'XKEY'
024500930201     C                   PARM                    DSKEY
024600930201     C*
024700930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
024800930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
024900930201     C*
025000930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
025100930201     C*   -NOME PGM CHIAMANTE
025200930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
025300970619     C                   MOVEL     'TISI89R'     DSMSPG
025400970619     C                   MOVEL     'TISI89R'     MSGQUE
025500930201     C                   MOVEL     *BLANKS       MSGKEY
025600930201     C                   MOVEL     *BLANKS       DSMSMS
025700930201     C*
025800930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
025900930201     C                   Z-ADD     0             FUNGRU            5 0
026000930201     C*
026100930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
026200930201     C                   EXSR      FUNGES
026300930201     C*
026400930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
026500930201     C                   MOVEL     '0'           F12L00            1
026600930201     C                   MOVEL     '0'           F03L00            1
026700930201     C                   MOVEL     '0'           FINL00            1
026800930201     C                   MOVEL     '0'           ERRL00            1
026900930201     C                   MOVEL     '0'           FLFINE            1
027000930421     C*
027100930421     C* AZZERA CAMPI VIDEO PACKED
027200930421     C                   EXSR      AZZERA
027300930421     C*
027400930421     C                   ENDSR
027500930421     C*================================================================
027600930421     C* AZZERA - AZZERA CAMPI PACKED
027700930421     C*================================================================
027800930421     C     AZZERA        BEGSR
027900930305     C*
028000930305     C* INIZIALIZZO CAMPI PACKED
028100000000     C*
028200000000     C                   ENDSR
028300000000     C*================================================================
028400000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
028500000000     C*================================================================
028600000000     C     FUNGES        BEGSR
028700000000     C*
028800000000     C* PULISCO CAMPI DI LAVORO
028900000000     C                   Z-ADD     0             W                 5 0
029000000000     C*
029100000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
029200000000     C* E CARICO TABELLA FUNZIONI ATTIVE
029300000000     C     1             DO        24            I
029400000000     C                   MOVEL     FUD(I)        FUDRIG
029500000000     C*
029600000000     C* PULIZIA FUNZIONI PRECEDENTI
029700000000     C                   MOVEL     *BLANKS       FUV(I)
029800000000     C*
029900000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
030000000000     C* (SALTA F24)
030100000000     C     FUDATT        IFEQ      'S'
030200000000     C     FUDFUN        ANDNE     '24'
030300000000     C                   ADD       1             W
030400000000     C                   MOVEL     FUDDES        FUV(W)
030500000000     C                   END
030600000000     C                   END                                                    DO WHILE
030700000000     C*
030800000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
030900000000     C* ALTRIMENTI DISATTIVO F24
031000000000     C     1             DO        24            I
031100000000     C                   MOVEL     FUD(I)        FUDRIG
031200000000     C*
031300000000     C     FUDFUN        IFEQ      '24'
031400000000     C     W             IFGT      6
031500000000     C                   MOVEL     'S'           FUDATT
031600000000     C                   ELSE
031700000000     C                   MOVEL     *BLANKS       FUDATT
031800000000     C                   END                                                    W GT 8
031900000000     C                   MOVEL     FUDRIG        FUD(I)
032000000000     C                   END
032100000000     C                   END                                                    DO WHILE
032200000000     C*
032300000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
032400000000     C     W             DIV       6             I
032500000000     C                   MVR                     Z
032600000000     C*
032700000000     C     Z             IFNE      0
032800000000     C     I             ADD       1             Z
032900000000     C                   ELSE
033000000000     C                   Z-ADD     I             Z                 5 0
033100000000     C                   END
033200000000     C*
033300000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
033400000000     C                   ADD       1             FUNGRU
033500000000     C     FUNGRU        IFGT      Z
033600000000     C     FUNGRU        ORLT      1
033700000000     C                   Z-ADD     1             FUNGRU
033800000000     C                   END
033900000000     C*
034000000000     C* PRIMO GRUPPO DI 6 FUNZIONI
034100000000     C     FUNGRU        IFEQ      1
034200000000     C                   MOVEL     FUV(1)        FUN001
034300000000     C                   MOVEL     FUV(2)        FUN002
034400000000     C                   MOVEL     FUV(3)        FUN003
034500000000     C                   MOVEL     FUV(4)        FUN004
034600000000     C                   MOVEL     FUV(5)        FUN005
034700000000     C                   MOVEL     FUV(6)        FUN006
034800000000     C                   END
034900000000     C*
035000000000     C* SECONDO GRUPPO DI 6 FUNZIONI
035100000000     C     FUNGRU        IFEQ      2
035200000000     C                   MOVEL     FUV(7)        FUN001
035300000000     C                   MOVEL     FUV(8)        FUN002
035400000000     C                   MOVEL     FUV(9)        FUN003
035500000000     C                   MOVEL     FUV(10)       FUN004
035600000000     C                   MOVEL     FUV(11)       FUN005
035700000000     C                   MOVEL     FUV(12)       FUN006
035800000000     C                   END
035900000000     C*
036000000000     C* TERZO GRUPPO DI 6 FUNZIONI
036100000000     C     FUNGRU        IFEQ      3
036200000000     C                   MOVEL     FUV(13)       FUN001
036300000000     C                   MOVEL     FUV(14)       FUN002
036400000000     C                   MOVEL     FUV(15)       FUN003
036500000000     C                   MOVEL     FUV(16)       FUN004
036600000000     C                   MOVEL     FUV(17)       FUN005
036700000000     C                   MOVEL     FUV(18)       FUN006
036800000000     C                   END
036900000000     C*
037000000000     C* QUARTO GRUPPO DI 6 FUNZIONI
037100000000     C     FUNGRU        IFEQ      4
037200000000     C                   MOVEL     FUV(19)       FUN001
037300000000     C                   MOVEL     FUV(20)       FUN002
037400000000     C                   MOVEL     FUV(21)       FUN003
037500000000     C                   MOVEL     FUV(22)       FUN004
037600000000     C                   MOVEL     *BLANKS       FUN005
037700000000     C                   MOVEL     *BLANKS       FUN006
037800000000     C                   END
037900000000     C*
038000000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
038100000000     C                   MOVEL     *BLANKS       FUN007
038200000000     C                   MOVEL     *BLANKS       FUN008
038300000000     C*
038400000000     C     1             DO        24            I
038500000000     C                   MOVEL     FUD(I)        FUDRIG
038600000000     C     FUDFUN        IFEQ      '24'
038700000000     C     FUDATT        ANDEQ     'S'
038800000000     C                   MOVEL     FUDDES        FUN008
038900000000     C                   END
039000000000     C                   END                                                    DO
039100000000     C                   ENDSR
039200000000     C*================================================================
039300000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
039400000000     C*================================================================
039500000000     C     FUNCON        BEGSR
039600000000     C*
039700000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
039800000000     C                   EXSR      FUNVAR
039900000000     C*
040000000000     C* PULISCO CAMPI DI LAVORO
040100000000     C                   Z-ADD     0             I                 5 0
040200000000     C                   Z-ADD     0             W                 5 0
040300000000     C*
040400000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
040500000000     C     FUNKEY        IFNE      *BLANKS
040600000000     C     1             DO        24            I
040700000000     C                   MOVEL     FUD(I)        FUDRIG
040800000000     C*
040900000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
041000000000     C     FUNKEY        IFEQ      FUDFUN
041100000000     C     FUDATT        ANDEQ     'S'
041200000000     C                   Z-ADD     I             W
041300000000     C                   END
041400000000     C                   END                                                    DO WHILE
041500000000     C*
041600000000     C* FUNZIONE NON ATTIVA (ERRORE)
041700000000     C     W             IFEQ      0
041800000000     C*
041900000000     C* EMISSIONE MESSAGGIO DI ERRORE
042000930201     C                   MOVEL     MSG(1)        DSMSMS
042100930201     C*
042200930201     C                   END                                                    W=0
042300000000     C                   END                                                    FUNKEY NE *BLANKS
042400000000     C*
042500000000     C* FUNZIONE ATTIVA (PROCEDI ...)
042600000000     C     W             IFNE      0
042700000000     C     FUNKEY        OREQ      *BLANKS
042800000000     C*
042900000000     C     FUNKEY        IFEQ      *BLANKS
043000000000     C                   EXSR      ENTGES
043100000000     C                   END
043200000000     C     FUNKEY        IFEQ      '01'
043300000000     C                   EXSR      F01GES
043400000000     C                   END
043500000000     C     FUNKEY        IFEQ      '03'
043600000000     C                   EXSR      F03GES
043700000000     C                   END
043800930202     C     FUNKEY        IFEQ      '06'
043900930202     C                   EXSR      F06GES
044000930202     C                   END
044100000000     C     FUNKEY        IFEQ      '07'
044200000000     C                   EXSR      F07GES
044300000000     C                   END
044400000000     C     FUNKEY        IFEQ      '08'
044500000000     C                   EXSR      F08GES
044600000000     C                   END
044700000000     C     FUNKEY        IFEQ      '12'
044800000000     C                   EXSR      F12GES
044900000000     C                   END
045000930201     C     FUNKEY        IFEQ      '21'
045100930201     C                   EXSR      F21GES
045200930201     C                   END
045300000000     C     FUNKEY        IFEQ      '22'
045400000000     C                   EXSR      F22GES
045500000000     C                   END
045600000000     C     FUNKEY        IFEQ      '24'
045700000000     C                   EXSR      F24GES
045800000000     C                   END
045900000000     C     FUNKEY        IFEQ      'HP'
046000000000     C                   EXSR      HELGES
046100000000     C                   END
046200000000     C*
046300000000     C                   END
046400000000     C*
046500000000     C                   ENDSR
046600000000     C*================================================================
046700000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
046800000000     C*================================================================
046900000000     C     FUNVAR        BEGSR
047000000000     C*
047100000000     C* INIZIALIZZA VARIABILE -FUNKEY-
047200000000     C*
047300000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
047400000000     C     KEY           IFEQ      ENTER
047500000000     C                   MOVEL     *BLANKS       FUNKEY
047600000000     C                   END
047700000000     C     KEY           IFEQ      F1
047800000000     C                   MOVEL     '01'          FUNKEY
047900000000     C                   END
048000000000     C     KEY           IFEQ      F2
048100000000     C                   MOVEL     '02'          FUNKEY
048200000000     C                   END
048300000000     C     KEY           IFEQ      F3
048400000000     C                   MOVEL     '03'          FUNKEY
048500000000     C                   END
048600000000     C     KEY           IFEQ      F4
048700000000     C                   MOVEL     '04'          FUNKEY
048800000000     C                   END
048900000000     C     KEY           IFEQ      F5
049000000000     C                   MOVEL     '05'          FUNKEY
049100000000     C                   END
049200000000     C     KEY           IFEQ      F6
049300000000     C                   MOVEL     '06'          FUNKEY
049400000000     C                   END
049500000000     C     KEY           IFEQ      F7
049600000000     C                   MOVEL     '07'          FUNKEY
049700000000     C                   END
049800000000     C     KEY           IFEQ      F8
049900000000     C                   MOVEL     '08'          FUNKEY
050000000000     C                   END
050100000000     C     KEY           IFEQ      F9
050200000000     C                   MOVEL     '09'          FUNKEY
050300000000     C                   END
050400000000     C     KEY           IFEQ      F10
050500000000     C                   MOVEL     '10'          FUNKEY
050600000000     C                   END
050700000000     C     KEY           IFEQ      F11
050800000000     C                   MOVEL     '11'          FUNKEY
050900000000     C                   END
051000000000     C     KEY           IFEQ      F12
051100000000     C                   MOVEL     '12'          FUNKEY
051200000000     C                   END
051300000000     C     KEY           IFEQ      F13
051400000000     C                   MOVEL     '13'          FUNKEY
051500000000     C                   END
051600000000     C     KEY           IFEQ      F14
051700000000     C                   MOVEL     '14'          FUNKEY
051800000000     C                   END
051900000000     C     KEY           IFEQ      F15
052000000000     C                   MOVEL     '15'          FUNKEY
052100000000     C                   END
052200000000     C     KEY           IFEQ      F16
052300000000     C                   MOVEL     '16'          FUNKEY
052400000000     C                   END
052500000000     C     KEY           IFEQ      F17
052600000000     C                   MOVEL     '17'          FUNKEY
052700000000     C                   END
052800000000     C     KEY           IFEQ      F18
052900000000     C                   MOVEL     '18'          FUNKEY
053000000000     C                   END
053100000000     C     KEY           IFEQ      F19
053200000000     C                   MOVEL     '19'          FUNKEY
053300000000     C                   END
053400000000     C     KEY           IFEQ      F20
053500000000     C                   MOVEL     '20'          FUNKEY
053600000000     C                   END
053700000000     C     KEY           IFEQ      F21
053800000000     C                   MOVEL     '21'          FUNKEY
053900000000     C                   END
054000000000     C     KEY           IFEQ      F22
054100000000     C                   MOVEL     '22'          FUNKEY
054200000000     C                   END
054300000000     C     KEY           IFEQ      F23
054400000000     C                   MOVEL     '23'          FUNKEY
054500000000     C                   END
054600000000     C     KEY           IFEQ      F24
054700000000     C                   MOVEL     '24'          FUNKEY
054800000000     C                   END
054900000000     C     KEY           IFEQ      ROLLUP
055000000000     C                   MOVEL     'RU'          FUNKEY
055100000000     C                   END
055200000000     C     KEY           IFEQ      HELP
055300000000     C                   MOVEL     'HP'          FUNKEY
055400000000     C                   END
055500000000     C                   ENDSR
055600000000     C*================================================================
055700000000     C* NOTE PER COMPILAZIONE SCHIERE
055800000000     C*================================================================
055900000000     C* -MSG-
056000000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
056100000000     C* (NON TOCCARE MSG,1)
056200000000     C*
056300000000     C* -CMD-
056400000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
056500000000     C*
056600000000     C* -FUD-
056700000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
056800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
056900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
057000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
057100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
057200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
057300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
057400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
057500000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
057600000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
057700000000     C*
057800000000     C*================================================================
057900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
058000970619TISI89R- Tasto funzionale non gestito                                          1
058100970619TISI89R- 2                                                                     2
058200970619TISI89R- 3                                                                     3
058300970619TISI89R- 4                                                                     4
058400970619TISI89R- 5                                                                     5
058500970619TISI89R- 6                                                                     6
058600970619TISI89R- 7                                                                     7
058700970619TISI89R- 8                                                                     8
058800970619TISI89R- 9                                                                     9
058900970619TISI89R- 10                                                                   10
059000970619TISI89R- 11                                                                   11
059100970619TISI89R- 12                                                                   12
059200970619TISI89R- 13                                                                   13
059300970619TISI89R- 14                                                                   14
059400970619TISI89R- 15                                                                   15
059500970619TISI89R- 16                                                                   16
059600970619TISI89R- 17                                                                   17
059700970619TISI89R- 18                                                                   18
059800970619TISI89R- 19                                                                   19
059900970619TISI89R- 20                                                                   20
060000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
060100930201                   -        (S/N)=N                        funz.01¬
060200000000                   -        (S/N)=N                        funz.02¬
060300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
060400000000                   -        (S/N)=N                        funz.04¬
060500000000                   -        (S/N)=N                        funz.05¬
060600930305                   -        (S/N)=N                        funz.06¬
060700930201                   -        (S/N)=N                        funz.07¬
060800930210                   -        (S/N)=N                        funz.08¬
060900000000                   -        (S/N)=N                        funz.09¬
061000000000                   -        (S/N)=N                        funz.10¬
061100000000                   -        (S/N)=N                        funz.11¬
061200930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
061300000000                   -        (S/N)=N                        funz.13¬
061400000000                   -        (S/N)=N                        funz.14¬
061500000000                   -        (S/N)=N                        funz.15¬
061600000000                   -        (S/N)=N                        funz.16¬
061700000000                   -        (S/N)=N                        funz.17¬
061800000000                   -        (S/N)=N                        funz.18¬
061900000000                   -        (S/N)=N                        funz.19¬
062000000000                   -        (S/N)=N                        funz.20¬
062100930305                   -        (S/N)=N                        funz.21¬
062200930305                   -        (S/N)=N                        funz.22¬
062300000000                   -        (S/N)=N                        funz.23¬
062400000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
