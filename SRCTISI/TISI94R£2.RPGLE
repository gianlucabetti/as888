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
004200061030     D dcppflo       E DS
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
005500000421     D*-------------------
005600000421     D* DS LETTURA O95RDS -DATI INSTRADAMENTO POSTE-
005700000421     D*-------------------
005800000421     D DRDS          E DS
005900930201     D***************************************************************
006000930201     D* ARCHITETTURA
006100930201     D KPJBA         E DS
006200000000     C*================================================================
006300000000     C* MAIN DEL PGM
006400000000     C                   EXSR      RUTINZ
006500000000     C*
006600000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
006700000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
006800000000     C*
006900000000     C* CICLO FINO A CHE:
007000000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
007100000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
007200930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
007300000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
007400000000     C     F12L00        OREQ      '1'
007500930202     C     FLFINE        OREQ      '1'
007600000000     C*
007700000000     C* EMETTE FORMATO VIDEO
007800000000     C                   EXSR      BUTFMR
007900000000     C*
008000000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
008100000000     C                   EXSR      FUNCON
008200000000     C*
008300000000     C                   END                                                    <-- 2 --
008400000000     C*
008500000000     C                   END                                                    <-- 1 --
008600000000     C*
008700000000     C                   SETON                                        LR
008800000000     C*FINE MAIN DEL PGM
008900000000     C*================================================================
009000000000     C* EMISSIONE FORMATO A VIDEO
009100000000     C*================================================================
009200000000     C     BUTFMR        BEGSR
009300930204     C*
009400930204     C* WRITE RECORD TESTATA
009500970516     C                   WRITE     SI94D01
009600930205     C*
009700930205     C* VISUALIZZAZIONE MESSAGGIO
009800930205     C                   EXSR      SNDMSG
009900930204     C*
010000930204     C* WRITE RECORD FUNZIONI
010100970516     C                   WRITE     SI94D02
010200000000     C*
010300930204     C* EXFMT RECORD DETTAGLIO
010400970516     C                   EXFMT     SI94D03
010500000000     C*
010600000000     C* AZZERO FLAG DI ERRORE LIVELLO
010700000000     C                   MOVEL     '0'           ERRL00
010800000000     C*
010900000000     C                   ENDSR
011000000000     C*================================================================
011100000000     C* SNDMSG - MANDA MESSAGGIO
011200000000     C*================================================================
011300000000     C     SNDMSG        BEGSR
011400000000     C*
011500000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
011600000000     C                   MOVEL     '1'           *IN40
011700970516     C                   WRITE     SI94DMC
011800000000     C                   MOVEL     '0'           *IN40
011900000000     C*
012000000000     C* VISUALIZZAZIONE MESSAGGIO
012100000000     C                   Z-ADD     0             MSGULT            4 0
012200930202     C                   CALL      'XMSG'        XMSGPA
012300930201     C     DSMSER        IFEQ      0
012400930201     C                   MOVEL     DSMSKE        MSGKEY
012500000000     C                   END
012600000000     C                   Z-ADD     1             MSGULT
012700970516     C                   WRITE     SI94DMS                                      SFL MESS
012800970516     C                   WRITE     SI94DMC                                      CONTR MESS
012900000000     C*
013000000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
013100930201     C                   MOVEL     *BLANKS       DSMSMS
013200000000     C*
013300000000     C                   ENDSR
013400000000     C*================================================================
013500000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
013600000000     C*================================================================
013700000000     C     ENTGES        BEGSR
013800060922     c                   clear                   o97uti
013900060922     c                   clear                   o97err
014000060922     c                   clear                   o97fit
014100060922     c                   clear                   o97esi
014200061030     c                   clear                   vidtut
014300061030     c                   clear                   vidcas
014400930201     C*
014500930326     C* LANCIA ROUTINE
014600000629     C                   CLEAR                   DRDS
014700000629IF  1C     ERRL00        IFEQ      '0'                                          --- 1 -->
014800970516     C                   CALL      'TISI95R'
014900970516     C                   PARM                    SI95
015000060921     c
015100060921     c* solo se tipo controllo '1'
015200060921     c                   if        i95tcn>'1' and i97ntw<>*blanks
015300060921     C                   CALL      'TISI97R'
015400060921     C                   PARM                    SI95
015500060921     C                   PARM                    tiSI97ds
015600060921     c                   if        o97err=' ' and o97esi=' '
015700060921     c                   eval      o97esi='N'
015800060921     c                   endif
015900060921     c                   endif
016000061030     c* Imposto
016100061030     c                   if        o97err=' '
016200061030     c                   if        o97flo=*blanks
016300061030     c                   movel     'S'           vidtut
016400061030     c                   else
016500061030     c                   movel     o97flo        dcppflo
016600061030     c                   movel     §cppcas       vidcas
016700061030     c                   endif
016800061030     c                   endif
016900060921     c
017000000421IF  2C     I95TSP        IFEQ      'P'                                          SERVIZIO POSTE
017100000629     C                   MOVEL     O95RDS        A14              14
017200000629     C                   MOVE      A14           A1                1
017300000629     C                   TESTN                   A14                  28
017400000629IF  3C     *IN28         IFEQ      *OFF                                         *DA TESTN NON E' NUM
017500000629     C     *IN28         OREQ      *ON                                          *DA TESTN E'NUMERO M
017600000629     C     A1            ANDLT     '0'                                          *TROVATA UN LETTERA
017700000629X   3C                   ELSE                                                   *TUTTI NUMERI
017800000629     C                   MOVEL     O95RDS        DRDS
017900000629E   3C                   ENDIF
018000000629E   2C                   ENDIF
018100000629E   1C                   ENDIF                                                  <-- 1 ---
018200000000     C*
018300000000     C                   ENDSR
018400000000     C*================================================================
018500930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
018600000000     C*================================================================
018700000000     C     F01GES        BEGSR
018800000000     C                   ENDSR
018900000000     C*================================================================
019000930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
019100000000     C*================================================================
019200000000     C     F03GES        BEGSR
019300000000     C*
019400930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
019500000000     C                   MOVEL     '1'           F03L00
019600000000     C*
019700000000     C                   ENDSR
019800930201     C*================================================================
019900930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
020000930201     C*================================================================
020100930201     C     F06GES        BEGSR
020200930201     C                   ENDSR
020300000000     C*================================================================
020400930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
020500000000     C*================================================================
020600000000     C     F07GES        BEGSR
020700000000     C                   ENDSR
020800000000     C*================================================================
020900930210     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
021000000000     C*================================================================
021100000000     C     F08GES        BEGSR
021200000000     C                   ENDSR
021300000000     C*================================================================
021400930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
021500000000     C*================================================================
021600000000     C     F12GES        BEGSR
021700000000     C*
021800000000     C* IMPOSTO FLAG DI LIVELLO
021900000000     C                   MOVEL     '1'           F12L00
022000000000     C*
022100000000     C                   ENDSR
022200930201     C*================================================================
022300930210     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ELENCO
022400930201     C*================================================================
022500930201     C     F21GES        BEGSR
022600930201     C                   ENDSR
022700000000     C*================================================================
022800930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
022900000000     C*================================================================
023000000000     C     F22GES        BEGSR
023100000000     C                   ENDSR
023200000000     C*================================================================
023300930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
023400000000     C*================================================================
023500000000     C     F24GES        BEGSR
023600000000     C*
023700000000     C                   ENDSR
023800000000     C*================================================================
023900000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
024000000000     C*================================================================
024100000000     C     HELGES        BEGSR
024200000000     C                   ENDSR
024300000000     C*================================================================
024400000000     C*  RUTINZ - ROUTINE INIZIALE
024500000000     C*================================================================
024600000000     C     RUTINZ        BEGSR
024700930401     C*
024800930401     C* RICEVIMENTO PARAMETRI
024900930401     C     *ENTRY        PLIST
025000930401     C                   PARM                    KPJBA
025100930201     C*
025200930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
025300930201     C     XMSGPA        PLIST
025400930201     C                   PARM                    DSMSMS
025500930201     C                   PARM                    DSMSPG
025600930201     C                   PARM                    DSMSKE
025700930201     C                   PARM                    DSMSER
025800930201     C*
025900930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
026000930201     C                   CALL      'XKEY'
026100930201     C                   PARM                    DSKEY
026200930201     C*
026300930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
026400930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
026500930201     C*
026600930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
026700930201     C*   -NOME PGM CHIAMANTE
026800930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
026900970516     C                   MOVEL     'TISI94R'     DSMSPG
027000970516     C                   MOVEL     'TISI94R'     MSGQUE
027100930201     C                   MOVEL     *BLANKS       MSGKEY
027200930201     C                   MOVEL     *BLANKS       DSMSMS
027300930201     C*
027400930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
027500930201     C                   Z-ADD     0             FUNGRU            5 0
027600930201     C*
027700930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
027800060921     C                   MOVEL     FUD(3)        fun001
027900930201     C*
028000930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
028100930201     C                   MOVEL     '0'           F12L00            1
028200930201     C                   MOVEL     '0'           F03L00            1
028300930201     C                   MOVEL     '0'           FINL00            1
028400930201     C                   MOVEL     '0'           ERRL00            1
028500930201     C                   MOVEL     '0'           FLFINE            1
028600930421     C*
028700930421     C* AZZERA CAMPI VIDEO PACKED
028800930421     C                   EXSR      AZZERA
028900970530     C                   MOVEL     '3'           I95TCN
029000970618     C* RECUPERA DATA DECORR. ULTIMA VERSIONE
029100970618     C     99999999      SETGT     AZCPR02L
029200970618     C                   READP     AZCPR02L                               99
029300970618     C                   Z-ADD     CPRDDE        I95DAT
029400930421     C*
029500930421     C                   ENDSR
029600930421     C*================================================================
029700930421     C* AZZERA - AZZERA CAMPI PACKED
029800930421     C*================================================================
029900930421     C     AZZERA        BEGSR
030000930305     C*
030100930305     C* INIZIALIZZO CAMPI PACKED
030200970516     C                   Z-ADD     *ZEROS        I95DAT
030300970703     C                   Z-ADD     *ZEROS        I95LKG
030400970516     C                   Z-ADD     *ZEROS        I95LMC
030500970516     C                   Z-ADD     *ZEROS        I95TFP
030600980602     C                   Z-ADD     *ZEROS        I95FI3
030700980602     C                   Z-ADD     *ZEROS        O95GF3
030800980602     C                   Z-ADD     *ZEROS        O95GF4
030900970530     C                   Z-ADD     *ZEROS        O95VER
031000970530     C                   Z-ADD     *ZEROS        O95RDE
031100970530     C                   Z-ADD     *ZEROS        O95RSC
031200970516     C                   Z-ADD     *ZEROS        O95LNA
031300970516     C                   Z-ADD     *ZEROS        O95ZNC
031400970516     C                   Z-ADD     *ZEROS        O95TTC
031500970516     C                   Z-ADD     *ZEROS        O950IN
031600970516     C                   Z-ADD     *ZEROS        O950CN
031700970516     C                   Z-ADD     *ZEROS        O951IN
031800970516     C                   Z-ADD     *ZEROS        O951CN
031900970516     C                   Z-ADD     *ZEROS        O95TIS
032000970516     C                   Z-ADD     *ZEROS        O95TCS
032100970516     C                   Z-ADD     *ZEROS        O95TLD
032200970516     C                   Z-ADD     *ZEROS        O95TTC
032300970519     C                   Z-ADD     *ZEROS        O95ELA
032400970519     C                   Z-ADD     *ZEROS        O95EZC
032500970606     C                   Z-ADD     *ZEROS        O95ELO
032600970606     C                   Z-ADD     *ZEROS        O95EZO
032700970606     C                   Z-ADD     *ZEROS        O95ELS
032800970606     C                   Z-ADD     *ZEROS        O95EZS
032900970521     C                   Z-ADD     *ZEROS        O95CLA
033000970521     C                   Z-ADD     *ZEROS        O95CLK
033100970521     C                   Z-ADD     *ZEROS        O95CLM
033200970521     C                   Z-ADD     *ZEROS        O95CLO
033300970605     C                   Z-ADD     *ZEROS        O95CZO
033400970605     C                   Z-ADD     *ZEROS        O95CKS
033500970605     C                   Z-ADD     *ZEROS        O95CMS
033600970605     C                   Z-ADD     *ZEROS        O95CLS
033700970605     C                   Z-ADD     *ZEROS        O95CZS
033800970605     C                   Z-ADD     *ZEROS        O95CK2
033900970605     C                   Z-ADD     *ZEROS        O95CM2
034000970605     C                   Z-ADD     *ZEROS        O95CL2
034100970605     C                   Z-ADD     *ZEROS        O95CZ2
034200970521     C                   Z-ADD     *ZEROS        O95CTC
034300970521     C                   Z-ADD     *ZEROS        O95CZC
034400970521     C                   Z-ADD     *ZEROS        O95CZF
034500970521     C                   Z-ADD     *ZEROS        O95LLA
034600970521     C                   Z-ADD     *ZEROS        O95LLK
034700040528     C                   Z-ADD     *ZEROS        O95LKS
034800040528     C                   Z-ADD     *ZEROS        O95LMS
034900970521     C                   Z-ADD     *ZEROS        O95LLM
035000970521     C                   Z-ADD     *ZEROS        O95LLO
035100040528     C                   Z-ADD     *ZEROS        O95LLS
035200970521     C                   Z-ADD     *ZEROS        O95LTC
035300970521     C                   Z-ADD     *ZEROS        O95LZC
035400970521     C                   Z-ADD     *ZEROS        O95LZF
035500970521     C                   Z-ADD     *ZEROS        O95LZO
035600040528     C                   Z-ADD     *ZEROS        O95LZS
035700970521     C                   Z-ADD     *ZEROS        O95TFP
035800970521     C                   Z-ADD     *ZEROS        O95TF1
035900970521     C                   Z-ADD     *ZEROS        O95TF2
036000000421     C                   CLEAR                   DRDS
036100000000     C*
036200000000     C                   ENDSR
036300000000     C*================================================================
036400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
036500000000     C*================================================================
036600000000     C     FUNCON        BEGSR
036700000000     C*
036800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
036900000000     C                   EXSR      FUNVAR
037000000000     C*
037100000000     C* PULISCO CAMPI DI LAVORO
037200000000     C                   Z-ADD     0             I                 5 0
037300000000     C                   Z-ADD     0             W                 5 0
037400000000     C*
037500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
037600000000     C     FUNKEY        IFNE      *BLANKS
037700000000     C     1             DO        24            I
037800000000     C                   MOVEL     FUD(I)        FUDRIG
037900000000     C*
038000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
038100000000     C     FUNKEY        IFEQ      FUDFUN
038200000000     C     FUDATT        ANDEQ     'S'
038300000000     C                   Z-ADD     I             W
038400000000     C                   END
038500000000     C                   END                                                    DO WHILE
038600000000     C*
038700000000     C* FUNZIONE NON ATTIVA (ERRORE)
038800000000     C     W             IFEQ      0
038900000000     C*
039000000000     C* EMISSIONE MESSAGGIO DI ERRORE
039100930201     C                   MOVEL     MSG(1)        DSMSMS
039200930201     C*
039300930201     C                   END                                                    W=0
039400000000     C                   END                                                    FUNKEY NE *BLANKS
039500000000     C*
039600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
039700000000     C     W             IFNE      0
039800000000     C     FUNKEY        OREQ      *BLANKS
039900000000     C*
040000000000     C     FUNKEY        IFEQ      *BLANKS
040100000000     C                   EXSR      ENTGES
040200000000     C                   END
040300000000     C     FUNKEY        IFEQ      '01'
040400000000     C                   EXSR      F01GES
040500000000     C                   END
040600000000     C     FUNKEY        IFEQ      '03'
040700000000     C                   EXSR      F03GES
040800000000     C                   END
040900930202     C     FUNKEY        IFEQ      '06'
041000930202     C                   EXSR      F06GES
041100930202     C                   END
041200000000     C     FUNKEY        IFEQ      '07'
041300000000     C                   EXSR      F07GES
041400000000     C                   END
041500000000     C     FUNKEY        IFEQ      '08'
041600000000     C                   EXSR      F08GES
041700000000     C                   END
041800000000     C     FUNKEY        IFEQ      '12'
041900000000     C                   EXSR      F12GES
042000000000     C                   END
042100930201     C     FUNKEY        IFEQ      '21'
042200930201     C                   EXSR      F21GES
042300930201     C                   END
042400000000     C     FUNKEY        IFEQ      '22'
042500000000     C                   EXSR      F22GES
042600000000     C                   END
042700000000     C     FUNKEY        IFEQ      '24'
042800000000     C                   EXSR      F24GES
042900000000     C                   END
043000000000     C     FUNKEY        IFEQ      'HP'
043100000000     C                   EXSR      HELGES
043200000000     C                   END
043300000000     C*
043400000000     C                   END
043500000000     C*
043600000000     C                   ENDSR
043700000000     C*================================================================
043800000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
043900000000     C*================================================================
044000000000     C     FUNVAR        BEGSR
044100000000     C*
044200000000     C* INIZIALIZZA VARIABILE -FUNKEY-
044300000000     C*
044400000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
044500000000     C     KEY           IFEQ      ENTER
044600000000     C                   MOVEL     *BLANKS       FUNKEY
044700000000     C                   END
044800000000     C     KEY           IFEQ      F1
044900000000     C                   MOVEL     '01'          FUNKEY
045000000000     C                   END
045100000000     C     KEY           IFEQ      F2
045200000000     C                   MOVEL     '02'          FUNKEY
045300000000     C                   END
045400000000     C     KEY           IFEQ      F3
045500000000     C                   MOVEL     '03'          FUNKEY
045600000000     C                   END
045700000000     C     KEY           IFEQ      F4
045800000000     C                   MOVEL     '04'          FUNKEY
045900000000     C                   END
046000000000     C     KEY           IFEQ      F5
046100000000     C                   MOVEL     '05'          FUNKEY
046200000000     C                   END
046300000000     C     KEY           IFEQ      F6
046400000000     C                   MOVEL     '06'          FUNKEY
046500000000     C                   END
046600000000     C     KEY           IFEQ      F7
046700000000     C                   MOVEL     '07'          FUNKEY
046800000000     C                   END
046900000000     C     KEY           IFEQ      F8
047000000000     C                   MOVEL     '08'          FUNKEY
047100000000     C                   END
047200000000     C     KEY           IFEQ      F9
047300000000     C                   MOVEL     '09'          FUNKEY
047400000000     C                   END
047500000000     C     KEY           IFEQ      F10
047600000000     C                   MOVEL     '10'          FUNKEY
047700000000     C                   END
047800000000     C     KEY           IFEQ      F11
047900000000     C                   MOVEL     '11'          FUNKEY
048000000000     C                   END
048100000000     C     KEY           IFEQ      F12
048200000000     C                   MOVEL     '12'          FUNKEY
048300000000     C                   END
048400000000     C     KEY           IFEQ      F13
048500000000     C                   MOVEL     '13'          FUNKEY
048600000000     C                   END
048700000000     C     KEY           IFEQ      F14
048800000000     C                   MOVEL     '14'          FUNKEY
048900000000     C                   END
049000000000     C     KEY           IFEQ      F15
049100000000     C                   MOVEL     '15'          FUNKEY
049200000000     C                   END
049300000000     C     KEY           IFEQ      F16
049400000000     C                   MOVEL     '16'          FUNKEY
049500000000     C                   END
049600000000     C     KEY           IFEQ      F17
049700000000     C                   MOVEL     '17'          FUNKEY
049800000000     C                   END
049900000000     C     KEY           IFEQ      F18
050000000000     C                   MOVEL     '18'          FUNKEY
050100000000     C                   END
050200000000     C     KEY           IFEQ      F19
050300000000     C                   MOVEL     '19'          FUNKEY
050400000000     C                   END
050500000000     C     KEY           IFEQ      F20
050600000000     C                   MOVEL     '20'          FUNKEY
050700000000     C                   END
050800000000     C     KEY           IFEQ      F21
050900000000     C                   MOVEL     '21'          FUNKEY
051000000000     C                   END
051100000000     C     KEY           IFEQ      F22
051200000000     C                   MOVEL     '22'          FUNKEY
051300000000     C                   END
051400000000     C     KEY           IFEQ      F23
051500000000     C                   MOVEL     '23'          FUNKEY
051600000000     C                   END
051700000000     C     KEY           IFEQ      F24
051800000000     C                   MOVEL     '24'          FUNKEY
051900000000     C                   END
052000000000     C     KEY           IFEQ      ROLLUP
052100000000     C                   MOVEL     'RU'          FUNKEY
052200000000     C                   END
052300000000     C     KEY           IFEQ      HELP
052400000000     C                   MOVEL     'HP'          FUNKEY
052500000000     C                   END
052600000000     C                   ENDSR
052700000000     C*================================================================
052800000000     C* NOTE PER COMPILAZIONE SCHIERE
052900000000     C*================================================================
053000000000     C* -MSG-
053100000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
053200000000     C* (NON TOCCARE MSG,1)
053300000000     C*
053400000000     C* -CMD-
053500000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
053600000000     C*
053700000000     C* -FUD-
053800000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
053900000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
054000000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
054100000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
054200000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
054300000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
054400000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
054500000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
054600000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
054700000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
054800000000     C*
054900000000     C*================================================================
055000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
055100970516TISI94R- Tasto funzionale non gestito                                          1
055200970516TISI94R- 2                                                                     2
055300970516TISI94R- 3                                                                     3
055400970516TISI94R- 4                                                                     4
055500970516TISI94R- 5                                                                     5
055600970516TISI94R- 6                                                                     6
055700970516TISI94R- 7                                                                     7
055800970516TISI94R- 8                                                                     8
055900970516TISI94R- 9                                                                     9
056000970516TISI94R- 10                                                                   10
056100970516TISI94R- 11                                                                   11
056200970516TISI94R- 12                                                                   12
056300970516TISI94R- 13                                                                   13
056400970516TISI95R- 14                                                                   14
056500970516TISI94R- 15                                                                   15
056600970516TISI94R- 16                                                                   16
056700970516TISI94R- 17                                                                   17
056800970516TISI94R- 18                                                                   18
056900970516TISI94R- 19                                                                   19
057000970516TISI94R- 20                                                                   20
057100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
057200930201                   -        (S/N)=N                        funz.01¬
057300000000                   -        (S/N)=N                        funz.02¬
057400000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
057500000000                   -        (S/N)=N                        funz.04¬
057600000000                   -        (S/N)=N                        funz.05¬
057700930305                   -        (S/N)=N                        funz.06¬
057800930201                   -        (S/N)=N                        funz.07¬
057900930210                   -        (S/N)=N                        funz.08¬
058000000000                   -        (S/N)=N                        funz.09¬
058100000000                   -        (S/N)=N                        funz.10¬
058200000000                   -        (S/N)=N                        funz.11¬
058300930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
058400000000                   -        (S/N)=N                        funz.13¬
058500000000                   -        (S/N)=N                        funz.14¬
058600000000                   -        (S/N)=N                        funz.15¬
058700000000                   -        (S/N)=N                        funz.16¬
058800000000                   -        (S/N)=N                        funz.17¬
058900000000                   -        (S/N)=N                        funz.18¬
059000000000                   -        (S/N)=N                        funz.19¬
059100000000                   -        (S/N)=N                        funz.20¬
059200930305                   -        (S/N)=N                        funz.21¬
059300930305                   -        (S/N)=N                        funz.22¬
059400000000                   -        (S/N)=N                        funz.23¬
059500000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
