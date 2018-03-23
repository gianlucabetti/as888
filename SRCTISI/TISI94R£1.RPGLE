000100000000      ****************************************************************
000200970516      * GESTISCE PER PROVA TUTTE LA FUNZIONI ROUTINE 'TISI95R'       *
000300000000      ****************************************************************
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F* FILE VIDEO (SUBFILE DATI)
000600970516     FTISI94D   CF   E             WORKSTN INFDS(DEVDS1)
000700970516     F                                     SFILE(SI94DMS:MSGULT)
000800970618     FAZCPR02L  IF   E           K DISK
000900000000     D*****************************************************************
001000000000     D* MESSAGGI
001100930208     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001200000000     D* DESCRIZIONI TASTI FUNZIONALI
001300000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
001500000000     D FUV             S             19    DIM(24)
001600000000     D*****************************************************************
001700000000     D* RIDEFINIZIONE SCHIERA -FUD-
001800000000     D FUDDS           DS
001900000000     D*                -DESCRIZIONE PARZIALE
002000000000     D  FUDDE1                 1      1
002100000000     D*                -FUNZIONE
002200000000     D  FUDFUN                 2      3
002300000000     D*                -DESCRIZIONE PARZIALE
002400000000     D  FUDDE2                 4     19
002500000000     D*                -FILLER
002600000000     D  FUDFI0                20     34
002700000000     D*                -FLAG FUNZ.ATTIVA (S/N)
002800000000     D  FUDATT                35     35
002900000000     D*                -FILLER
003000000000     D  FUDFI1                36     80
003100000000     D*                -DESCRIZIONE COMPLETA
003200000000     D  FUDDES                 1     19
003300000000     D*                -RIDEFINIZIONE RIGA
003400000000     D  FUDRIG                 1     80
003500930201     D*****************************************************************
003600930201     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003700930201     D DSMSG         E DS
003800930305     D*****************************************************************
003900970516     D* PARAMETRI CHIAMATA PGM TISI95R (CONTROLLO CAP)
004000970516     D SI95          E DS                  EXTNAME(TISI95DS)
004100060921     D tisi97ds      E DS
004200930201     D*****************************************************************
004300930201     D* DS PER GESTIONE TASTI FUNZIONALI
004400930201     D DSKEY         E DS
004500930201     D DEVDS1          DS
004600930201     D  KEY                  369    369
004700950407     D***************************************************************
004800950407     D* DS PER LANCIO PGM DATA->N.GIORNI (XSRDA8)
004900950407     D G02PAR          DS
005000950407     D  G02DAT                 1      8  0
005100950407     D  G02INV                 9     16  0
005200950407     D  G02ERR                17     17
005300950407     D  G02TGI                18     22  0
005400000421     D*-------------------
005500000421     D* DS LETTURA O95RDS -DATI INSTRADAMENTO POSTE-
005600000421     D*-------------------
005700000421     D DRDS          E DS
005800930201     D***************************************************************
005900930201     D* ARCHITETTURA
006000930201     D KPJBA         E DS
006100000000     C*================================================================
006200000000     C* MAIN DEL PGM
006300000000     C                   EXSR      RUTINZ
006400000000     C*
006500000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
006600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
006700000000     C*
006800000000     C* CICLO FINO A CHE:
006900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
007000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
007100930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
007200000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
007300000000     C     F12L00        OREQ      '1'
007400930202     C     FLFINE        OREQ      '1'
007500000000     C*
007600000000     C* EMETTE FORMATO VIDEO
007700000000     C                   EXSR      BUTFMR
007800000000     C*
007900000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
008000000000     C                   EXSR      FUNCON
008100000000     C*
008200000000     C                   END                                                    <-- 2 --
008300000000     C*
008400000000     C                   END                                                    <-- 1 --
008500000000     C*
008600000000     C                   SETON                                        LR
008700000000     C*FINE MAIN DEL PGM
008800000000     C*================================================================
008900000000     C* EMISSIONE FORMATO A VIDEO
009000000000     C*================================================================
009100000000     C     BUTFMR        BEGSR
009200930204     C*
009300930204     C* WRITE RECORD TESTATA
009400970516     C                   WRITE     SI94D01
009500930205     C*
009600930205     C* VISUALIZZAZIONE MESSAGGIO
009700930205     C                   EXSR      SNDMSG
009800930204     C*
009900930204     C* WRITE RECORD FUNZIONI
010000970516     C                   WRITE     SI94D02
010100000000     C*
010200930204     C* EXFMT RECORD DETTAGLIO
010300970516     C                   EXFMT     SI94D03
010400000000     C*
010500000000     C* AZZERO FLAG DI ERRORE LIVELLO
010600000000     C                   MOVEL     '0'           ERRL00
010700000000     C*
010800000000     C                   ENDSR
010900000000     C*================================================================
011000000000     C* SNDMSG - MANDA MESSAGGIO
011100000000     C*================================================================
011200000000     C     SNDMSG        BEGSR
011300000000     C*
011400000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
011500000000     C                   MOVEL     '1'           *IN40
011600970516     C                   WRITE     SI94DMC
011700000000     C                   MOVEL     '0'           *IN40
011800000000     C*
011900000000     C* VISUALIZZAZIONE MESSAGGIO
012000000000     C                   Z-ADD     0             MSGULT            4 0
012100930202     C                   CALL      'XMSG'        XMSGPA
012200930201     C     DSMSER        IFEQ      0
012300930201     C                   MOVEL     DSMSKE        MSGKEY
012400000000     C                   END
012500000000     C                   Z-ADD     1             MSGULT
012600970516     C                   WRITE     SI94DMS                                      SFL MESS
012700970516     C                   WRITE     SI94DMC                                      CONTR MESS
012800000000     C*
012900000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
013000930201     C                   MOVEL     *BLANKS       DSMSMS
013100000000     C*
013200000000     C                   ENDSR
013300000000     C*================================================================
013400000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
013500000000     C*================================================================
013600000000     C     ENTGES        BEGSR
013700060922     c                   clear                   o97uti
013800060922     c                   clear                   o97err
013900060922     c                   clear                   o97fit
014000060922     c                   clear                   o97esi
014100930201     C*
014200930326     C* LANCIA ROUTINE
014300000629     C                   CLEAR                   DRDS
014400000629IF  1C     ERRL00        IFEQ      '0'                                          --- 1 -->
014500970516     C                   CALL      'TISI95R'
014600970516     C                   PARM                    SI95
014700060921     c
014800060921     c* solo se tipo controllo '1'
014900060921     c                   if        i95tcn>'1' and i97ntw<>*blanks
015000060921     C                   CALL      'TISI97R'
015100060921     C                   PARM                    SI95
015200060921     C                   PARM                    tiSI97ds
015300060921     c                   if        o97err=' ' and o97esi=' '
015400060921     c                   eval      o97esi='N'
015500060921     c                   endif
015600060921     c                   endif
015700060921     c
015800000421IF  2C     I95TSP        IFEQ      'P'                                          SERVIZIO POSTE
015900000629     C                   MOVEL     O95RDS        A14              14
016000000629     C                   MOVE      A14           A1                1
016100000629     C                   TESTN                   A14                  28
016200000629IF  3C     *IN28         IFEQ      *OFF                                         *DA TESTN NON E' NUM
016300000629     C     *IN28         OREQ      *ON                                          *DA TESTN E'NUMERO M
016400000629     C     A1            ANDLT     '0'                                          *TROVATA UN LETTERA
016500000629X   3C                   ELSE                                                   *TUTTI NUMERI
016600000629     C                   MOVEL     O95RDS        DRDS
016700000629E   3C                   ENDIF
016800000629E   2C                   ENDIF
016900000629E   1C                   ENDIF                                                  <-- 1 ---
017000000000     C*
017100000000     C                   ENDSR
017200000000     C*================================================================
017300930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
017400000000     C*================================================================
017500000000     C     F01GES        BEGSR
017600000000     C                   ENDSR
017700000000     C*================================================================
017800930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017900000000     C*================================================================
018000000000     C     F03GES        BEGSR
018100000000     C*
018200930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
018300000000     C                   MOVEL     '1'           F03L00
018400000000     C*
018500000000     C                   ENDSR
018600930201     C*================================================================
018700930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018800930201     C*================================================================
018900930201     C     F06GES        BEGSR
019000930201     C                   ENDSR
019100000000     C*================================================================
019200930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
019300000000     C*================================================================
019400000000     C     F07GES        BEGSR
019500000000     C                   ENDSR
019600000000     C*================================================================
019700930210     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
019800000000     C*================================================================
019900000000     C     F08GES        BEGSR
020000000000     C                   ENDSR
020100000000     C*================================================================
020200930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
020300000000     C*================================================================
020400000000     C     F12GES        BEGSR
020500000000     C*
020600000000     C* IMPOSTO FLAG DI LIVELLO
020700000000     C                   MOVEL     '1'           F12L00
020800000000     C*
020900000000     C                   ENDSR
021000930201     C*================================================================
021100930210     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ELENCO
021200930201     C*================================================================
021300930201     C     F21GES        BEGSR
021400930201     C                   ENDSR
021500000000     C*================================================================
021600930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
021700000000     C*================================================================
021800000000     C     F22GES        BEGSR
021900000000     C                   ENDSR
022000000000     C*================================================================
022100930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
022200000000     C*================================================================
022300000000     C     F24GES        BEGSR
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*================================================================
022700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
022800000000     C*================================================================
022900000000     C     HELGES        BEGSR
023000000000     C                   ENDSR
023100000000     C*================================================================
023200000000     C*  RUTINZ - ROUTINE INIZIALE
023300000000     C*================================================================
023400000000     C     RUTINZ        BEGSR
023500930401     C*
023600930401     C* RICEVIMENTO PARAMETRI
023700930401     C     *ENTRY        PLIST
023800930401     C                   PARM                    KPJBA
023900930201     C*
024000930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
024100930201     C     XMSGPA        PLIST
024200930201     C                   PARM                    DSMSMS
024300930201     C                   PARM                    DSMSPG
024400930201     C                   PARM                    DSMSKE
024500930201     C                   PARM                    DSMSER
024600930201     C*
024700930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
024800930201     C                   CALL      'XKEY'
024900930201     C                   PARM                    DSKEY
025000930201     C*
025100930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
025200930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
025300930201     C*
025400930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
025500930201     C*   -NOME PGM CHIAMANTE
025600930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
025700970516     C                   MOVEL     'TISI94R'     DSMSPG
025800970516     C                   MOVEL     'TISI94R'     MSGQUE
025900930201     C                   MOVEL     *BLANKS       MSGKEY
026000930201     C                   MOVEL     *BLANKS       DSMSMS
026100930201     C*
026200930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
026300930201     C                   Z-ADD     0             FUNGRU            5 0
026400930201     C*
026500930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
026600060921     C                   MOVEL     FUD(3)        fun001
026700930201     C*
026800930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
026900930201     C                   MOVEL     '0'           F12L00            1
027000930201     C                   MOVEL     '0'           F03L00            1
027100930201     C                   MOVEL     '0'           FINL00            1
027200930201     C                   MOVEL     '0'           ERRL00            1
027300930201     C                   MOVEL     '0'           FLFINE            1
027400930421     C*
027500930421     C* AZZERA CAMPI VIDEO PACKED
027600930421     C                   EXSR      AZZERA
027700970530     C                   MOVEL     '3'           I95TCN
027800970618     C* RECUPERA DATA DECORR. ULTIMA VERSIONE
027900970618     C     99999999      SETGT     AZCPR02L
028000970618     C                   READP     AZCPR02L                               99
028100970618     C                   Z-ADD     CPRDDE        I95DAT
028200930421     C*
028300930421     C                   ENDSR
028400930421     C*================================================================
028500930421     C* AZZERA - AZZERA CAMPI PACKED
028600930421     C*================================================================
028700930421     C     AZZERA        BEGSR
028800930305     C*
028900930305     C* INIZIALIZZO CAMPI PACKED
029000970516     C                   Z-ADD     *ZEROS        I95DAT
029100970703     C                   Z-ADD     *ZEROS        I95LKG
029200970516     C                   Z-ADD     *ZEROS        I95LMC
029300970516     C                   Z-ADD     *ZEROS        I95TFP
029400980602     C                   Z-ADD     *ZEROS        I95FI3
029500980602     C                   Z-ADD     *ZEROS        O95GF3
029600980602     C                   Z-ADD     *ZEROS        O95GF4
029700970530     C                   Z-ADD     *ZEROS        O95VER
029800970530     C                   Z-ADD     *ZEROS        O95RDE
029900970530     C                   Z-ADD     *ZEROS        O95RSC
030000970516     C                   Z-ADD     *ZEROS        O95LNA
030100970516     C                   Z-ADD     *ZEROS        O95ZNC
030200970516     C                   Z-ADD     *ZEROS        O95TTC
030300970516     C                   Z-ADD     *ZEROS        O950IN
030400970516     C                   Z-ADD     *ZEROS        O950CN
030500970516     C                   Z-ADD     *ZEROS        O951IN
030600970516     C                   Z-ADD     *ZEROS        O951CN
030700970516     C                   Z-ADD     *ZEROS        O95TIS
030800970516     C                   Z-ADD     *ZEROS        O95TCS
030900970516     C                   Z-ADD     *ZEROS        O95TLD
031000970516     C                   Z-ADD     *ZEROS        O95TTC
031100970519     C                   Z-ADD     *ZEROS        O95ELA
031200970519     C                   Z-ADD     *ZEROS        O95EZC
031300970606     C                   Z-ADD     *ZEROS        O95ELO
031400970606     C                   Z-ADD     *ZEROS        O95EZO
031500970606     C                   Z-ADD     *ZEROS        O95ELS
031600970606     C                   Z-ADD     *ZEROS        O95EZS
031700970521     C                   Z-ADD     *ZEROS        O95CLA
031800970521     C                   Z-ADD     *ZEROS        O95CLK
031900970521     C                   Z-ADD     *ZEROS        O95CLM
032000970521     C                   Z-ADD     *ZEROS        O95CLO
032100970605     C                   Z-ADD     *ZEROS        O95CZO
032200970605     C                   Z-ADD     *ZEROS        O95CKS
032300970605     C                   Z-ADD     *ZEROS        O95CMS
032400970605     C                   Z-ADD     *ZEROS        O95CLS
032500970605     C                   Z-ADD     *ZEROS        O95CZS
032600970605     C                   Z-ADD     *ZEROS        O95CK2
032700970605     C                   Z-ADD     *ZEROS        O95CM2
032800970605     C                   Z-ADD     *ZEROS        O95CL2
032900970605     C                   Z-ADD     *ZEROS        O95CZ2
033000970521     C                   Z-ADD     *ZEROS        O95CTC
033100970521     C                   Z-ADD     *ZEROS        O95CZC
033200970521     C                   Z-ADD     *ZEROS        O95CZF
033300970521     C                   Z-ADD     *ZEROS        O95LLA
033400970521     C                   Z-ADD     *ZEROS        O95LLK
033500040528     C                   Z-ADD     *ZEROS        O95LKS
033600040528     C                   Z-ADD     *ZEROS        O95LMS
033700970521     C                   Z-ADD     *ZEROS        O95LLM
033800970521     C                   Z-ADD     *ZEROS        O95LLO
033900040528     C                   Z-ADD     *ZEROS        O95LLS
034000970521     C                   Z-ADD     *ZEROS        O95LTC
034100970521     C                   Z-ADD     *ZEROS        O95LZC
034200970521     C                   Z-ADD     *ZEROS        O95LZF
034300970521     C                   Z-ADD     *ZEROS        O95LZO
034400040528     C                   Z-ADD     *ZEROS        O95LZS
034500970521     C                   Z-ADD     *ZEROS        O95TFP
034600970521     C                   Z-ADD     *ZEROS        O95TF1
034700970521     C                   Z-ADD     *ZEROS        O95TF2
034800000421     C                   CLEAR                   DRDS
034900000000     C*
035000000000     C                   ENDSR
035100000000     C*================================================================
035200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
035300000000     C*================================================================
035400000000     C     FUNCON        BEGSR
035500000000     C*
035600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
035700000000     C                   EXSR      FUNVAR
035800000000     C*
035900000000     C* PULISCO CAMPI DI LAVORO
036000000000     C                   Z-ADD     0             I                 5 0
036100000000     C                   Z-ADD     0             W                 5 0
036200000000     C*
036300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
036400000000     C     FUNKEY        IFNE      *BLANKS
036500000000     C     1             DO        24            I
036600000000     C                   MOVEL     FUD(I)        FUDRIG
036700000000     C*
036800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
036900000000     C     FUNKEY        IFEQ      FUDFUN
037000000000     C     FUDATT        ANDEQ     'S'
037100000000     C                   Z-ADD     I             W
037200000000     C                   END
037300000000     C                   END                                                    DO WHILE
037400000000     C*
037500000000     C* FUNZIONE NON ATTIVA (ERRORE)
037600000000     C     W             IFEQ      0
037700000000     C*
037800000000     C* EMISSIONE MESSAGGIO DI ERRORE
037900930201     C                   MOVEL     MSG(1)        DSMSMS
038000930201     C*
038100930201     C                   END                                                    W=0
038200000000     C                   END                                                    FUNKEY NE *BLANKS
038300000000     C*
038400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
038500000000     C     W             IFNE      0
038600000000     C     FUNKEY        OREQ      *BLANKS
038700000000     C*
038800000000     C     FUNKEY        IFEQ      *BLANKS
038900000000     C                   EXSR      ENTGES
039000000000     C                   END
039100000000     C     FUNKEY        IFEQ      '01'
039200000000     C                   EXSR      F01GES
039300000000     C                   END
039400000000     C     FUNKEY        IFEQ      '03'
039500000000     C                   EXSR      F03GES
039600000000     C                   END
039700930202     C     FUNKEY        IFEQ      '06'
039800930202     C                   EXSR      F06GES
039900930202     C                   END
040000000000     C     FUNKEY        IFEQ      '07'
040100000000     C                   EXSR      F07GES
040200000000     C                   END
040300000000     C     FUNKEY        IFEQ      '08'
040400000000     C                   EXSR      F08GES
040500000000     C                   END
040600000000     C     FUNKEY        IFEQ      '12'
040700000000     C                   EXSR      F12GES
040800000000     C                   END
040900930201     C     FUNKEY        IFEQ      '21'
041000930201     C                   EXSR      F21GES
041100930201     C                   END
041200000000     C     FUNKEY        IFEQ      '22'
041300000000     C                   EXSR      F22GES
041400000000     C                   END
041500000000     C     FUNKEY        IFEQ      '24'
041600000000     C                   EXSR      F24GES
041700000000     C                   END
041800000000     C     FUNKEY        IFEQ      'HP'
041900000000     C                   EXSR      HELGES
042000000000     C                   END
042100000000     C*
042200000000     C                   END
042300000000     C*
042400000000     C                   ENDSR
042500000000     C*================================================================
042600000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
042700000000     C*================================================================
042800000000     C     FUNVAR        BEGSR
042900000000     C*
043000000000     C* INIZIALIZZA VARIABILE -FUNKEY-
043100000000     C*
043200000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
043300000000     C     KEY           IFEQ      ENTER
043400000000     C                   MOVEL     *BLANKS       FUNKEY
043500000000     C                   END
043600000000     C     KEY           IFEQ      F1
043700000000     C                   MOVEL     '01'          FUNKEY
043800000000     C                   END
043900000000     C     KEY           IFEQ      F2
044000000000     C                   MOVEL     '02'          FUNKEY
044100000000     C                   END
044200000000     C     KEY           IFEQ      F3
044300000000     C                   MOVEL     '03'          FUNKEY
044400000000     C                   END
044500000000     C     KEY           IFEQ      F4
044600000000     C                   MOVEL     '04'          FUNKEY
044700000000     C                   END
044800000000     C     KEY           IFEQ      F5
044900000000     C                   MOVEL     '05'          FUNKEY
045000000000     C                   END
045100000000     C     KEY           IFEQ      F6
045200000000     C                   MOVEL     '06'          FUNKEY
045300000000     C                   END
045400000000     C     KEY           IFEQ      F7
045500000000     C                   MOVEL     '07'          FUNKEY
045600000000     C                   END
045700000000     C     KEY           IFEQ      F8
045800000000     C                   MOVEL     '08'          FUNKEY
045900000000     C                   END
046000000000     C     KEY           IFEQ      F9
046100000000     C                   MOVEL     '09'          FUNKEY
046200000000     C                   END
046300000000     C     KEY           IFEQ      F10
046400000000     C                   MOVEL     '10'          FUNKEY
046500000000     C                   END
046600000000     C     KEY           IFEQ      F11
046700000000     C                   MOVEL     '11'          FUNKEY
046800000000     C                   END
046900000000     C     KEY           IFEQ      F12
047000000000     C                   MOVEL     '12'          FUNKEY
047100000000     C                   END
047200000000     C     KEY           IFEQ      F13
047300000000     C                   MOVEL     '13'          FUNKEY
047400000000     C                   END
047500000000     C     KEY           IFEQ      F14
047600000000     C                   MOVEL     '14'          FUNKEY
047700000000     C                   END
047800000000     C     KEY           IFEQ      F15
047900000000     C                   MOVEL     '15'          FUNKEY
048000000000     C                   END
048100000000     C     KEY           IFEQ      F16
048200000000     C                   MOVEL     '16'          FUNKEY
048300000000     C                   END
048400000000     C     KEY           IFEQ      F17
048500000000     C                   MOVEL     '17'          FUNKEY
048600000000     C                   END
048700000000     C     KEY           IFEQ      F18
048800000000     C                   MOVEL     '18'          FUNKEY
048900000000     C                   END
049000000000     C     KEY           IFEQ      F19
049100000000     C                   MOVEL     '19'          FUNKEY
049200000000     C                   END
049300000000     C     KEY           IFEQ      F20
049400000000     C                   MOVEL     '20'          FUNKEY
049500000000     C                   END
049600000000     C     KEY           IFEQ      F21
049700000000     C                   MOVEL     '21'          FUNKEY
049800000000     C                   END
049900000000     C     KEY           IFEQ      F22
050000000000     C                   MOVEL     '22'          FUNKEY
050100000000     C                   END
050200000000     C     KEY           IFEQ      F23
050300000000     C                   MOVEL     '23'          FUNKEY
050400000000     C                   END
050500000000     C     KEY           IFEQ      F24
050600000000     C                   MOVEL     '24'          FUNKEY
050700000000     C                   END
050800000000     C     KEY           IFEQ      ROLLUP
050900000000     C                   MOVEL     'RU'          FUNKEY
051000000000     C                   END
051100000000     C     KEY           IFEQ      HELP
051200000000     C                   MOVEL     'HP'          FUNKEY
051300000000     C                   END
051400000000     C                   ENDSR
051500000000     C*================================================================
051600000000     C* NOTE PER COMPILAZIONE SCHIERE
051700000000     C*================================================================
051800000000     C* -MSG-
051900000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
052000000000     C* (NON TOCCARE MSG,1)
052100000000     C*
052200000000     C* -CMD-
052300000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
052400000000     C*
052500000000     C* -FUD-
052600000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
052700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
052800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
052900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
053000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
053100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
053200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
053300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
053400000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
053500000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
053600000000     C*
053700000000     C*================================================================
053800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
053900970516TISI94R- Tasto funzionale non gestito                                          1
054000970516TISI94R- 2                                                                     2
054100970516TISI94R- 3                                                                     3
054200970516TISI94R- 4                                                                     4
054300970516TISI94R- 5                                                                     5
054400970516TISI94R- 6                                                                     6
054500970516TISI94R- 7                                                                     7
054600970516TISI94R- 8                                                                     8
054700970516TISI94R- 9                                                                     9
054800970516TISI94R- 10                                                                   10
054900970516TISI94R- 11                                                                   11
055000970516TISI94R- 12                                                                   12
055100970516TISI94R- 13                                                                   13
055200970516TISI95R- 14                                                                   14
055300970516TISI94R- 15                                                                   15
055400970516TISI94R- 16                                                                   16
055500970516TISI94R- 17                                                                   17
055600970516TISI94R- 18                                                                   18
055700970516TISI94R- 19                                                                   19
055800970516TISI94R- 20                                                                   20
055900000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
056000930201                   -        (S/N)=N                        funz.01¬
056100000000                   -        (S/N)=N                        funz.02¬
056200000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
056300000000                   -        (S/N)=N                        funz.04¬
056400000000                   -        (S/N)=N                        funz.05¬
056500930305                   -        (S/N)=N                        funz.06¬
056600930201                   -        (S/N)=N                        funz.07¬
056700930210                   -        (S/N)=N                        funz.08¬
056800000000                   -        (S/N)=N                        funz.09¬
056900000000                   -        (S/N)=N                        funz.10¬
057000000000                   -        (S/N)=N                        funz.11¬
057100930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
057200000000                   -        (S/N)=N                        funz.13¬
057300000000                   -        (S/N)=N                        funz.14¬
057400000000                   -        (S/N)=N                        funz.15¬
057500000000                   -        (S/N)=N                        funz.16¬
057600000000                   -        (S/N)=N                        funz.17¬
057700000000                   -        (S/N)=N                        funz.18¬
057800000000                   -        (S/N)=N                        funz.19¬
057900000000                   -        (S/N)=N                        funz.20¬
058000930305                   -        (S/N)=N                        funz.21¬
058100930305                   -        (S/N)=N                        funz.22¬
058200000000                   -        (S/N)=N                        funz.23¬
058300000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
