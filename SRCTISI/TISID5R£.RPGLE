000100000000     H*------------------------------------------------------------------------*
000200060520     H* CALCOLO INSTRADAMENTO CAPPARIO DPD GeoPost - TEST
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800170628     FTISID5D   CF   E             WORKSTN INFDS(DEVDS1)
000900170628     F                                     SFILE(SID5DMS:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D*-------------------
001800000000     D* DESCRIZIONI TASTI FUNZIONALI
001900000000     D*-------------------
002000000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002300000000     D*-------------------
002400000000     D FUV             S             19    DIM(24)
002500000000     D*------------------------------------------------------------------------*
002600000000     D* INPUT
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* RIDEFINIZIONE SCHIERA -FUD-
003000000000     D*-------------------
003100000000     D FUDDS           DS
003200000000     D*                -DESCRIZIONE PARZIALE
003300000000     D  FUDDE1                 1      1
003400000000     D*                -FUNZIONE
003500000000     D  FUDFUN                 2      3
003600000000     D*                -DESCRIZIONE PARZIALE
003700000000     D  FUDDE2                 4     19
003800000000     D*                -FILLER
003900000000     D  FUDFI0                20     34
004000000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004100000000     D  FUDATT                35     35
004200000000     D*                -FILLER
004300000000     D  FUDFI1                36     80
004400000000     D*                -DESCRIZIONE COMPLETA
004500000000     D  FUDDES                 1     19
004600000000     D*                -RIDEFINIZIONE RIGA
004700000000     D  FUDRIG                 1     80
004800000000     D*-------------------
004900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005000000000     D*-------------------
005100000000     D DSMSG         E DS
005200000000     D*-------------------
005300020919     D* DS REPERIMENTO DATI UTENTE
005400020919     D*------------
005500020919     D TIBS34DS      E DS                                                       *Profili utente
005600020919     D DDATIUTE      E DS                                                       *Dati utente
005700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005800060520     D*--------------------
005900060520     D* DS "XSRDAT" - CONTROLLO DATA (8)
006000060520     D*--------------------
006100060520     DWLBDA8           DS                  INZ
006200060520     D  G08DAT                 1      8  0
006300060520     D  G08INV                 9     16  0
006400060520     D  G08ERR                17     17
006500060520     D  G08TGI                18     22  0
006600000104     D*--------------------
006700060520     D* DS D PROCEDURA
006800000104     D*--------------------
006900170628     D TISID4DS      E DS                  INZ
007000000000     D*-------------------
007100000000     D* DS PER GESTIONE TASTI FUNZIONALI
007200000000     D*-------------------
007300000000     D DSKEY         E DS
007400000000     D DEVDS1          DS
007500000000     D  KEY                  369    369
007600000000     D*-------------------
007700000000     D* ARCHITETTURA
007800000000     D*-------------------
007900000000     D KPJBA         E DS
008000060721     D*-------------------
008100060721     D* VARIABILI D WRK
008200060721     D*-------------------
008300060721     D wTimeStr        s               z   inz
008400060721     D wTimeEnd        s               z   inz
008500000000     C*------------------------------------------------------------------------*
008600000000     C* MAIN LINE
008700000000     C*------------------------------------------------------------------------*
008800080124      *
008900080124     c                   eval      *in41  = (%subst(KNMUS:1:3) = 'EDP')
009000000000     C*
009100000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009200000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
009300000000     C*
009400000000     C* CICLO FINO A CHE:
009500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009700000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009800000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009900000000     C     F12L00        OREQ      '1'
010000000000     C     FLFINE        OREQ      '1'
010100000000     C*
010200000000     C* EMETTE FORMATO VIDEO
010300000000     C                   EXSR      BUTFMR
010400000000     C*
010500000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010600000000     C                   EXSR      FUNCON
010700000000     C*
010800000000     C                   END                                                    <-- 2 --
010900000000     C*
011000000000     C                   END                                                    <-- 1 --
011100000000     C*
011200000000     C                   SETON                                        LR
011300000000     C*------------------------------------------------------------------------*
011400000000     C* EMISSIONE FORMATO A VIDEO
011500000000     C*------------------------------------------------------------------------*
011600000000     C     BUTFMR        BEGSR
011700000000     C*
011800000000     C* WRITE RECORD TESTATA
011900170628     C                   WRITE     SID5D01
012000000000     C*
012100000000     C* VISUALIZZAZIONE MESSAGGIO
012200000000     C                   EXSR      SNDMSG
012300000000     C*
012400000000     C* WRITE RECORD FUNZIONI
012500170628     C                   WRITE     SID5D02
012600991019     C*
012700991019     C* EXFMT RECORD DETTAGLIO
012800170628     C                   EXFMT     SID5D03
012900991019     C*
013000000000     C* AZZERO FLAG DI ERRORE LIVELLO
013100000000     C                   MOVEL     '0'           ERRL00
013200000000     C*
013300000000     C* AZZERO FLAG DI ERRORE
013400060520     C                   SETOFF                                       70
013500000000     C*
013600000000     C                   ENDSR
013700000000     C*------------------------------------------------------------------------*
013800000000     C* SNDMSG - MANDA MESSAGGIO
013900000000     C*------------------------------------------------------------------------*
014000000000     C     SNDMSG        BEGSR
014100000000     C*
014200000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014300000000     C                   MOVEL     '1'           *IN40
014400170628     C                   WRITE     SID5DMC
014500000000     C                   MOVEL     '0'           *IN40
014600000000     C*
014700000000     C* VISUALIZZAZIONE MESSAGGIO
014800000000     C                   Z-ADD     0             MSGULT            4 0
014900000000     C                   CALL      'XMSG'        XMSGPA
015000000000     C     DSMSER        IFEQ      0
015100000000     C                   MOVEL     DSMSKE        MSGKEY
015200000000     C                   END
015300000000     C                   Z-ADD     1             MSGULT
015400170628     C                   WRITE     SID5DMS                                      SFL MESS
015500170628     C                   WRITE     SID5DMC                                      CONTR MESS
015600000000     C*
015700000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015800000000     C                   MOVEL     *BLANKS       DSMSMS
015900000000     C*
016000000000     C                   ENDSR
016100000000     C*------------------------------------------------------------------------*
016200000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016300000000     C*------------------------------------------------------------------------*
016400000000     C     ENTGES        BEGSR
016500000000     C*
016600000000     C* CONTROLLA I CAMPI IMMESSI
016700060520     C                   EXSR      DECODI
016800060520     C*
016900000216     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
017000000216     C     ERRL00        IFEQ      '0'
017100060721     C*
017200060721     C* Salvo il timestamp prima d eseguire la ricerca
017300060721     C                   TIME                    wTimeStr
017400000216     C*
017500000216     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD
017600170628     C                   CALL      'TISID4R'
017700170628     C                   PARM                    TISID4DS
017800060721     C*
017900060721     C* Salvo il timestamp dopo aver eseguito la ricerca
018000060721     C                   TIME                    wTimeEnd
018100060520     C*
018200060520     C* Se errore evidenzio bene a video
018300170628     C                   IF        OSID4ERR <> *blanks
018400060520     C                   SETON                                        70
018500060520     C                   ENDIF
018600000216     C                   ENDIF
018700060721     C*
018800060721     C* Se richiesto debug performance visualizzo i risultati
018900060721     C                   IF        VIDDBG = 'S'
019000060721     C     wTimeStr      DSPLY
019100060721     C     wTimeEnd      DSPLY
019200060721     C                   ENDIF
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*------------------------------------------------------------------------*
019600000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
019700000000     C*------------------------------------------------------------------------*
019800000000     C     F03GES        BEGSR
019900000000     C*
020000000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020100000000     C                   MOVEL     '1'           F03L00
020200000000     C*
020300000000     C                   ENDSR
020400060520     C*------------------------------------------------------------------------*
020500060520     C* F05GES -GESTIONE TASTO FUNZIONALE F05 - INIZIALIZZA VIDEATA
020600060520     C*------------------------------------------------------------------------*
020700060520     C     F05GES        BEGSR
020800060520     C*
020900170628     C                   CLEAR                   TISID4DS
021000170628     C                   Z-ADD     DATCOR        ISID4DRI
021100060721     C                   EVAL      VIDDBG = 'N'
021200060520     C*
021300060520     C                   ENDSR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
021600000000     C*------------------------------------------------------------------------*
021700000000     C     F12GES        BEGSR
021800000000     C*
021900000000     C* IMPOSTO FLAG DI LIVELLO
022000991103     C                   MOVEL     '1'           F12L00
022100000000     C*
022200000000     C                   ENDSR
022300000000     C*------------------------------------------------------------------------*
022400991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
022500000000     C*------------------------------------------------------------------------*
022600000000     C     F24GES        BEGSR
022700000000     C*
022800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
022900000000     C                   EXSR      FUNGES
023000000000     C*
023100000000     C                   ENDSR
023200991020     C*------------------------------------------------------------------------*
023300000104     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
023400000000     C*------------------------------------------------------------------------*
023500000000     C     DECODI        BEGSR
023600000216     C*
023700060520     C                   SETOFF                                       70
023800000216     C                   MOVEL     '0'           ERRL00
023900000000     C*
024000000000     C                   ENDSR
024100020919     C*--------------------------------------------------------------------------------------------*
024200020919     C* REPERISCE I DATI UTENTE
024300020919     C*--------------------------------------------------------------------------------------------*
024400020919     C     REPDATIUTE    BEGSR
024500020919     C*
024600020919     C* INIZIALIZZA VARIABILI DI WRK
024700020919     C                   CLEAR                   TIBS34DS
024800020919     C                   CLEAR                   AZUTEDS
024900020919     C                   CLEAR                   DDATIUTE
025000020919     C*
025100020919     C     *DTAARA       DEFINE    �azute        azuteds
025200020919     C     *DTAARA       DEFINE    �datiute      ddatiute
025300020919     C                   IN(E)     *DTAARA
025400060520     C                   IF        %Error
025500060520     C                   EVAL      I34Tla = 'L'
025600020919     C                   CALL      'TIBS34R'
025700020919     C                   PARM                    Tibs34Ds
025800020919     C                   IN        *DTAARA
025900060520     C                   ENDIF
026000020919     C*
026100020919     C                   ENDSR
026200000000     C*------------------------------------------------------------------------*
026300980403     C* *INZSR - OPERAZIONI INIZIALI
026400000000     C*------------------------------------------------------------------------*
026500980403     C     *INZSR        BEGSR
026600000000     C*
026700000000     C* RICEVIMENTO PARAMETRI
026800000000     C     *ENTRY        PLIST
026900000000     C                   PARM                    KPJBA
027000000000     C*
027100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
027200000000     C     XMSGPA        PLIST
027300000000     C                   PARM                    DSMSMS
027400000000     C                   PARM                    DSMSPG
027500000000     C                   PARM                    DSMSKE
027600000000     C                   PARM                    DSMSER
027700000000     C*
027800000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
027900000000     C                   CALL      'XKEY'
028000000000     C                   PARM                    DSKEY
028100000000     C*
028200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
028300000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
028400000000     C*
028500000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
028600000000     C*   -NOME PGM CHIAMANTE
028700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
028800170628     C                   MOVEL     'TISID5R'     DSMSPG
028900170628     C                   MOVEL     'TISID5R'     MSGQUE
029000000000     C                   MOVEL     *BLANKS       MSGKEY
029100000000     C                   MOVEL     *BLANKS       DSMSMS
029200000000     C*
029300000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
029400000000     C                   Z-ADD     0             FUNGRU            5 0
029500000000     C*
029600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
029700000000     C                   EXSR      FUNGES
029800000000     C*
029900000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
030000000000     C                   MOVEL     '0'           F12L00            1
030100000000     C                   MOVEL     '0'           F03L00            1
030200000000     C                   MOVEL     '0'           FINL00            1
030300000000     C                   MOVEL     '0'           ERRL00            1
030400000000     C                   MOVEL     '0'           FLFINE            1
030500000000     C*
030600000000     C* LANCIO ROUTINE PER VERIFICARE SE
030700000000     C* SONO IN SEDE, IN FILIALE DI 1� LIVELLO O IN FILIALE DI 2� LIVELLO
030800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
030900020919     C                   EXSR      REPDATIUTE
031000060520     C*
031100060520     C* IMPOSTA LA DATA CORRENTE
031200060520     C                   TIME                    N14              14 0
031300060520     C                   MOVE      N14           N8                8 0
031400060520     C                   Z-ADD     N8            G08DAT
031500060520     C                   Z-ADD     *ZEROS        G08INV
031600060520     C                   MOVEL     '0'           G08ERR
031700060520     C                   CALL      'XSRDA8'
031800060520     C                   PARM                    WLBDA8
031900060520     C                   Z-ADD     G08INV        DATCOR            8 0
032000060520     C*
032100060520     C* INIZIALIZZAZIONE VIDEO
032200170628     C                   CLEAR                   TISID4DS
032300170628     C                   Z-ADD     DATCOR        ISID4DRI
032400060721     C                   EVAL      VIDDBG = 'N'
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
032900000000     C*------------------------------------------------------------------------*
033000000000     C     FUNGES        BEGSR
033100000000     C*
033200000000     C* PULISCO CAMPI DI LAVORO
033300000000     C                   Z-ADD     0             W                 5 0
033400000000     C*
033500000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
033600000000     C* E CARICO TABELLA FUNZIONI ATTIVE
033700000000     C     1             DO        24            I
033800000000     C                   MOVEL     FUD(I)        FUDRIG
033900000000     C*
034000000000     C* PULIZIA FUNZIONI PRECEDENTI
034100000000     C                   MOVEL     *BLANKS       FUV(I)
034200000000     C*
034300000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
034400000000     C* (SALTA F24)
034500000000     C     FUDATT        IFEQ      'S'
034600000000     C     FUDFUN        ANDNE     '24'
034700000000     C                   ADD       1             W
034800000000     C                   MOVEL     FUDDES        FUV(W)
034900000000     C                   END
035000000000     C                   END                                                    DO WHILE
035100000000     C*
035200000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
035300000000     C* ALTRIMENTI DISATTIVO F24
035400000000     C     1             DO        24            I
035500000000     C                   MOVEL     FUD(I)        FUDRIG
035600000000     C*
035700000000     C     FUDFUN        IFEQ      '24'
035800000000     C     W             IFGT      6
035900000000     C                   MOVEL     'S'           FUDATT
036000000000     C                   ELSE
036100000000     C                   MOVEL     *BLANKS       FUDATT
036200000000     C                   END                                                    W GT 8
036300000000     C                   MOVEL     FUDRIG        FUD(I)
036400000000     C                   END
036500000000     C                   END                                                    DO WHILE
036600000000     C*
036700000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
036800000000     C     W             DIV       6             I
036900000000     C                   MVR                     Z
037000000000     C*
037100000000     C     Z             IFNE      0
037200000000     C     I             ADD       1             Z
037300000000     C                   ELSE
037400000000     C                   Z-ADD     I             Z                 5 0
037500000000     C                   END
037600000000     C*
037700000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
037800000000     C                   ADD       1             FUNGRU
037900000000     C     FUNGRU        IFGT      Z
038000000000     C     FUNGRU        ORLT      1
038100000000     C                   Z-ADD     1             FUNGRU
038200000000     C                   END
038300000000     C*
038400000000     C* PRIMO GRUPPO DI 6 FUNZIONI
038500000000     C     FUNGRU        IFEQ      1
038600000000     C                   MOVEL     FUV(1)        FUN001
038700000000     C                   MOVEL     FUV(2)        FUN002
038800000000     C                   MOVEL     FUV(3)        FUN003
038900000000     C                   MOVEL     FUV(4)        FUN004
039000000000     C                   MOVEL     FUV(5)        FUN005
039100000000     C                   MOVEL     FUV(6)        FUN006
039200000000     C                   END
039300000000     C*
039400000000     C* SECONDO GRUPPO DI 6 FUNZIONI
039500000000     C     FUNGRU        IFEQ      2
039600000000     C                   MOVEL     FUV(7)        FUN001
039700000000     C                   MOVEL     FUV(8)        FUN002
039800000000     C                   MOVEL     FUV(9)        FUN003
039900000000     C                   MOVEL     FUV(10)       FUN004
040000000000     C                   MOVEL     FUV(11)       FUN005
040100000000     C                   MOVEL     FUV(12)       FUN006
040200000000     C                   END
040300000000     C*
040400000000     C* TERZO GRUPPO DI 6 FUNZIONI
040500000000     C     FUNGRU        IFEQ      3
040600000000     C                   MOVEL     FUV(13)       FUN001
040700000000     C                   MOVEL     FUV(14)       FUN002
040800000000     C                   MOVEL     FUV(15)       FUN003
040900000000     C                   MOVEL     FUV(16)       FUN004
041000000000     C                   MOVEL     FUV(17)       FUN005
041100000000     C                   MOVEL     FUV(18)       FUN006
041200000000     C                   END
041300000000     C*
041400000000     C* QUARTO GRUPPO DI 6 FUNZIONI
041500000000     C     FUNGRU        IFEQ      4
041600000000     C                   MOVEL     FUV(19)       FUN001
041700000000     C                   MOVEL     FUV(20)       FUN002
041800000000     C                   MOVEL     FUV(21)       FUN003
041900000000     C                   MOVEL     FUV(22)       FUN004
042000000000     C                   MOVEL     *BLANKS       FUN005
042100000000     C                   MOVEL     *BLANKS       FUN006
042200000000     C                   END
042300000000     C*
042400000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
042500000000     C                   MOVEL     *BLANKS       FUN007
042600000000     C                   MOVEL     *BLANKS       FUN008
042700000000     C*
042800000000     C     1             DO        24            I
042900000000     C                   MOVEL     FUD(I)        FUDRIG
043000000000     C     FUDFUN        IFEQ      '24'
043100000000     C     FUDATT        ANDEQ     'S'
043200000000     C                   MOVEL     FUDDES        FUN008
043300000000     C                   END
043400000000     C                   END                                                    DO
043500000000     C                   ENDSR
043600000000     C*------------------------------------------------------------------------*
043700000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
043800000000     C*------------------------------------------------------------------------*
043900000000     C     FUNCON        BEGSR
044000000000     C*
044100000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
044200000000     C                   EXSR      FUNVAR
044300000000     C*
044400000000     C* PULISCO CAMPI DI LAVORO
044500000000     C                   Z-ADD     0             I                 5 0
044600000000     C                   Z-ADD     0             W                 5 0
044700000000     C*
044800000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
044900000000     C     FUNKEY        IFNE      *BLANKS
045000000000     C     1             DO        24            I
045100000000     C                   MOVEL     FUD(I)        FUDRIG
045200000000     C*
045300000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
045400000000     C     FUNKEY        IFEQ      FUDFUN
045500000000     C     FUDATT        ANDEQ     'S'
045600000000     C                   Z-ADD     I             W
045700000000     C                   END
045800000000     C                   END                                                    DO WHILE
045900000000     C*
046000000000     C* FUNZIONE NON ATTIVA (ERRORE)
046100000000     C     W             IFEQ      0
046200000000     C*
046300000000     C* EMISSIONE MESSAGGIO DI ERRORE
046400000000     C                   MOVEL     MSG(1)        DSMSMS
046500000000     C*
046600000000     C                   END                                                    W=0
046700000000     C                   END                                                    FUNKEY NE *BLANKS
046800000000     C*
046900000000     C* FUNZIONE ATTIVA (PROCEDI ...)
047000000000     C     W             IFNE      0
047100000000     C     FUNKEY        OREQ      *BLANKS
047200000000     C*
047300000000     C     FUNKEY        IFEQ      *BLANKS
047400000000     C                   EXSR      ENTGES
047500000000     C                   END
047600000000     C     FUNKEY        IFEQ      '01'
047700000000     C                   END
047800000000     C     FUNKEY        IFEQ      '03'
047900000000     C                   EXSR      F03GES
048000000000     C                   END
048100000000     C     FUNKEY        IFEQ      '06'
048200000000     C                   END
048300000000     C     FUNKEY        IFEQ      '07'
048400000000     C                   END
048500000000     C     FUNKEY        IFEQ      '08'
048600000000     C                   END
048700000000     C     FUNKEY        IFEQ      '12'
048800000000     C                   EXSR      F12GES
048900000000     C                   END
049000000000     C     FUNKEY        IFEQ      '21'
049100000000     C                   END
049200000000     C     FUNKEY        IFEQ      '22'
049300000000     C                   END
049400000000     C     FUNKEY        IFEQ      '24'
049500000000     C                   EXSR      F24GES
049600000000     C                   END
049700000000     C     FUNKEY        IFEQ      'HP'
049800000000     C                   END
049900000000     C*
050000000000     C                   END
050100000000     C*
050200000000     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
050500000000     C*------------------------------------------------------------------------*
050600000000     C     FUNVAR        BEGSR
050700000000     C*
050800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
050900000000     C*
051000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
051100000000     C     KEY           IFEQ      ENTER
051200000000     C                   MOVEL     *BLANKS       FUNKEY
051300000000     C                   END
051400000000     C     KEY           IFEQ      F1
051500000000     C                   MOVEL     '01'          FUNKEY
051600000000     C                   END
051700000000     C     KEY           IFEQ      F2
051800000000     C                   MOVEL     '02'          FUNKEY
051900000000     C                   END
052000000000     C     KEY           IFEQ      F3
052100000000     C                   MOVEL     '03'          FUNKEY
052200000000     C                   END
052300000000     C     KEY           IFEQ      F4
052400000000     C                   MOVEL     '04'          FUNKEY
052500000000     C                   END
052600000000     C     KEY           IFEQ      F5
052700000000     C                   MOVEL     '05'          FUNKEY
052800000000     C                   END
052900991103     C     KEY           IFEQ      F6
053000000000     C                   MOVEL     '06'          FUNKEY
053100000000     C                   END
053200000000     C     KEY           IFEQ      F7
053300000000     C                   MOVEL     '07'          FUNKEY
053400000000     C                   END
053500000000     C     KEY           IFEQ      F8
053600000000     C                   MOVEL     '08'          FUNKEY
053700000000     C                   END
053800000000     C     KEY           IFEQ      F9
053900000000     C                   MOVEL     '09'          FUNKEY
054000000000     C                   END
054100000000     C     KEY           IFEQ      F10
054200000000     C                   MOVEL     '10'          FUNKEY
054300000000     C                   END
054400000000     C     KEY           IFEQ      F11
054500000000     C                   MOVEL     '11'          FUNKEY
054600000000     C                   END
054700000000     C     KEY           IFEQ      F12
054800000000     C                   MOVEL     '12'          FUNKEY
054900000000     C                   END
055000000000     C     KEY           IFEQ      F13
055100000000     C                   MOVEL     '13'          FUNKEY
055200000000     C                   END
055300000000     C     KEY           IFEQ      F14
055400000000     C                   MOVEL     '14'          FUNKEY
055500000000     C                   END
055600000000     C     KEY           IFEQ      F15
055700000000     C                   MOVEL     '15'          FUNKEY
055800000000     C                   END
055900000000     C     KEY           IFEQ      F16
056000000000     C                   MOVEL     '16'          FUNKEY
056100000000     C                   END
056200000000     C     KEY           IFEQ      F17
056300000000     C                   MOVEL     '17'          FUNKEY
056400000000     C                   END
056500000000     C     KEY           IFEQ      F18
056600000000     C                   MOVEL     '18'          FUNKEY
056700000000     C                   END
056800000000     C     KEY           IFEQ      F19
056900000000     C                   MOVEL     '19'          FUNKEY
057000000000     C                   END
057100000000     C     KEY           IFEQ      F20
057200000000     C                   MOVEL     '20'          FUNKEY
057300000000     C                   END
057400000000     C     KEY           IFEQ      F21
057500000000     C                   MOVEL     '21'          FUNKEY
057600000000     C                   END
057700000000     C     KEY           IFEQ      F22
057800000000     C                   MOVEL     '22'          FUNKEY
057900000000     C                   END
058000000000     C     KEY           IFEQ      F23
058100000000     C                   MOVEL     '23'          FUNKEY
058200000000     C                   END
058300000000     C     KEY           IFEQ      F24
058400000000     C                   MOVEL     '24'          FUNKEY
058500000000     C                   END
058600000000     C     KEY           IFEQ      ROLLUP
058700000000     C                   MOVEL     'RU'          FUNKEY
058800000000     C                   END
058900000000     C     KEY           IFEQ      HELP
059000000000     C                   MOVEL     'HP'          FUNKEY
059100000000     C                   END
059200000000     C                   ENDSR
059300000000     C*------------------------------------------------------------------------*
059400000000     C* NOTE PER COMPILAZIONE SCHIERE
059500000000     C*------------------------------------------------------------------------*
059600000000     C* -MSG-
059700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
059800000000     C* (NON TOCCARE MSG,1)
059900000000     C*
060000000000     C* -CMD-
060100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
060200000000     C*
060300000000     C* -FUD-
060400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
060500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
060600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
060700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
060800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
060900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
061000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
061100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
061200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
061300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
061400000000     C*
061500000000     C*------------------------------------------------------------------------*
061600991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
061700170628TISID5R- Tasto funzionale non gestito.                                         1
061800170628TISID5R- File Versioni "CAPPARIO DPD" vuoto o data non coperta da versione.    2
061900170628TISID5R- Campo obbligatorio, inserire una data.                                3
062000170628TISID5R- Data errata.                                                          4
062100170628TISID5R- Campo obbligatorio, inserire il CAP                                   5
062200170628TISID5R-                                                                       6
062300170628TISID5R-                                                                       7
062400170628TISID5R-                                                                       8
062500170628TISID5R-                                                                       9
062600170628TISID5R-                                                                      10
062700170628TISID5R-                                                                      11
062800170628TISID5R-                                                                      12
062900170628TISID5R-                                                                      13
063000170628TISID5R-                                                                      14
063100170628TISID5R-                                                                      15
063200170628TISID5R-                                                                      16
063300170628TISID5R-                                                                      17
063400170628TISID5R-                                                                      18
063500170628TISID5R-                                                                      19
063600170628TISID5R-                                                                      20
063700000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
063800000000                   -        (S/N)=N                        funz.01�
063900000000                   -        (S/N)=N                        funz.02�
064000000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03�
064100000000                   -        (S/N)=N                        funz.04�
064200000000                   -        (S/N)=N                        funz.05�
064300000216F06=Importazione   -        (S/N)=N                        funz.06�
064400000104                   -        (S/N)=N                        funz.07�
064500000105                   -        (S/N)=N                        funz.08�
064600000000                   -        (S/N)=N                        funz.09�
064700000000                   -        (S/N)=N                        funz.10�
064800000000                   -        (S/N)=N                        funz.11�
064900991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12�
065000000000                   -        (S/N)=N                        funz.13�
065100000000                   -        (S/N)=N                        funz.14�
065200000000                   -        (S/N)=N                        funz.15�
065300000000                   -        (S/N)=N                        funz.16�
065400000000                   -        (S/N)=N                        funz.17�
065500000000                   -        (S/N)=N                        funz.18�
065600000000                   -        (S/N)=N                        funz.19�
065700000000                   -        (S/N)=N                        funz.20�
065800000000                   -        (S/N)=N                        funz.21�
065900000000F22=Gestione stampe-        (S/N)=N                        funz.22�
066000000000                   -        (S/N)=N                        funz.23�
066100991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24�
