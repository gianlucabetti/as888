000100000000     H*------------------------------------------------------------------------*
000200160726     H* Tester verifica instradabilità da EEX a DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800160726     Ffnlvd2D   CF   E             WORKSTN INFDS(DEVDS1)
000900160726     F                                     SFILE(lvd2DMS:MSGULT)
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
005500020919     D TIBS34DS      E DS
005600020919     D DDATIUTE      E DS
005700020919     D AZUTEDS       E DS                  extname(AZUTE00F)
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
006900160726     D fnlvd1ds      E DS                  INZ
006901160726     D tisi95ds      E DS                  INZ
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
009200000000     C     ERRL00        IFEQ      '0'
009300000000     C*
009400000000     C* CICLO FINO A CHE:
009500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009700000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009800000000     C     F03L00        DOUEQ     '1'
009900000000     C     F12L00        OREQ      '1'
010000000000     C     FLFINE        OREQ      '1'
010100000000     C*
010200000000     C* EMETTE FORMATO VIDEO
010300000000     C                   EXSR      BUTFMR
010400000000     C*
010500000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010600000000     C                   EXSR      FUNCON
010700000000     C*
010800000000     C                   END
010900000000     C*
011000000000     C                   END
011100060608     C*
011200060608     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD SOLO X CHIUSURA
011300160726     C                   CLEAR                   fnlvd1ds
011400160726     C                   EVAL      ilvd1TLA = 'C'
011500160726     C                   CALL      'FNLVD1R'
011600160726     C                   PARM                    FNLVD1DS
011601160726     C                   PARM                    TISI95ds
011700000000     C*
011800000000     C                   SETON                                        LR
011900000000     C*------------------------------------------------------------------------*
012000000000     C* EMISSIONE FORMATO A VIDEO
012100000000     C*------------------------------------------------------------------------*
012200000000     C     BUTFMR        BEGSR
012300000000     C*
012400000000     C* WRITE RECORD TESTATA
012500160726     C                   WRITE     lvd2D01
012600000000     C*
012700000000     C* VISUALIZZAZIONE MESSAGGIO
012800000000     C                   EXSR      SNDMSG
012900000000     C*
013000000000     C* WRITE RECORD FUNZIONI
013100160726     C                   WRITE     lvd2D02
013200991019     C*
013300991019     C* EXFMT RECORD DETTAGLIO
013400160726     C                   EXFMT     lvd2D03
013500991019     C*
013600000000     C* AZZERO FLAG DI ERRORE LIVELLO
013700000000     C                   MOVEL     '0'           ERRL00
013800000000     C*
013900000000     C* AZZERO FLAG DI ERRORE
014000060520     C                   SETOFF                                       70
014100000000     C*
014200000000     C                   ENDSR
014300000000     C*------------------------------------------------------------------------*
014400000000     C* SNDMSG - MANDA MESSAGGIO
014500000000     C*------------------------------------------------------------------------*
014600000000     C     SNDMSG        BEGSR
014700000000     C*
014800000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014900000000     C                   MOVEL     '1'           *IN40
015000160726     C                   WRITE     lvd2DMC
015100000000     C                   MOVEL     '0'           *IN40
015200000000     C*
015300000000     C* VISUALIZZAZIONE MESSAGGIO
015400000000     C                   Z-ADD     0             MSGULT            4 0
015500000000     C                   CALL      'XMSG'        XMSGPA
015600000000     C     DSMSER        IFEQ      0
015700000000     C                   MOVEL     DSMSKE        MSGKEY
015800000000     C                   END
015900000000     C                   Z-ADD     1             MSGULT
016000160726     C                   WRITE     lvd2DMS
016100160726     C                   WRITE     lvd2DMC
016200000000     C*
016300000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
016400000000     C                   MOVEL     *BLANKS       DSMSMS
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016900000000     C*------------------------------------------------------------------------*
017000000000     C     ENTGES        BEGSR
017100000000     C*
017200000000     C* CONTROLLA I CAMPI IMMESSI
017300060520     C                   EXSR      DECODI
017400060520     C*
017500160726     C* VEDO SE A VIDEO TUTTO OK ALTRIMEnTI LA CONFERMA NON E' VALIDA
017600000216     C     ERRL00        IFEQ      '0'
017700060721     C*
017800060721     C* Salvo il timestamp prima d eseguire la ricerca
017900060721     C                   TIME                    wTimeStr
018000000216     C*
018001160726     c                   eval      i95tcn='3'
018002160726     c                   call      'TISI95R'
018003160726     c                   parm                    tisi95ds
018100160726     C* LANCIO IL PROGRAMMA
018300160726     C                   CALL      'FNLVD1R'
018400160726     C                   PARM                    FNLVD1DS
018401160726     C                   PARM                    TISI95DS
018500060721     C*
018800060520     C*
018900160726     C* Se errore o  non instradabile a dpd evidenzio bene a video
019000160726     C                   IF        olvd1esi <> 'S'
019100060520     C                   SETON                                        70
019200060520     C                   ENDIF
019300000216     C                   ENDIF
019400060721     C*
020000000000     C*
020100000000     C                   ENDSR
020200000000     C*------------------------------------------------------------------------*
020300000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
020400000000     C*------------------------------------------------------------------------*
020500000000     C     F03GES        BEGSR
020600000000     C*
020700000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020800000000     C                   MOVEL     '1'           F03L00
020900000000     C*
021000000000     C                   ENDSR
021100060520     C*------------------------------------------------------------------------*
021200060520     C* F05GES -GESTIONE TASTO FUNZIONALE F05 - INIZIALIZZA VIDEATA
021300060520     C*------------------------------------------------------------------------*
021400060520     C     F05GES        BEGSR
021500060520     C*
021600160726     C                   CLEAR                   fnlvd1ds
021601160726     C                   CLEAR                   tisi95ds
021800160726     C                   Z-ADD     DATCOR        i95dat
022000060520     C*
022100060520     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
022400000000     C*------------------------------------------------------------------------*
022500000000     C     F12GES        BEGSR
022600000000     C*
022700000000     C* IMPOSTO FLAG DI LIVELLO
022800991103     C                   MOVEL     '1'           F12L00
022900000000     C*
023000000000     C                   ENDSR
023100000000     C*------------------------------------------------------------------------*
023200991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
023300000000     C*------------------------------------------------------------------------*
023400000000     C     F24GES        BEGSR
023500000000     C*
023600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
023700000000     C                   EXSR      FUNGES
023800000000     C*
023900000000     C                   ENDSR
024000991020     C*------------------------------------------------------------------------*
024100000104     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
024200000000     C*------------------------------------------------------------------------*
024300000000     C     DECODI        BEGSR
024400000216     C*
024500060520     C                   SETOFF                                       70
024600000216     C                   MOVEL     '0'           ERRL00
024700000000     C*
024800000000     C                   ENDSR
024900020919     C*-------------------------------------------------------------------------
025000020919     C* REPERISCE I DATI UTENTE
025100020919     C*-------------------------------------------------------------------------
025200020919     C     REPDATIUTE    BEGSR
025300020919     C*
025400020919     C* INIZIALIZZA VARIABILI DI WRK
025500020919     C                   CLEAR                   TIBS34DS
025600020919     C                   CLEAR                   AZUTEDS
025700020919     C                   CLEAR                   DDATIUTE
025800020919     C*
025900020919     C     *DTAARA       DEFINE    §azute        azuteds
026000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
026100020919     C                   IN(E)     *DTAARA
026200060520     C                   IF        %Error
026300060520     C                   EVAL      I34Tla = 'L'
026400020919     C                   CALL      'TIBS34R'
026500020919     C                   PARM                    Tibs34Ds
026600020919     C                   IN        *DTAARA
026700060520     C                   ENDIF
026800020919     C*
026900020919     C                   ENDSR
027000000000     C*------------------------------------------------------------------------*
027100980403     C* *INZSR - OPERAZIONI INIZIALI
027200000000     C*------------------------------------------------------------------------*
027300980403     C     *INZSR        BEGSR
027400000000     C*
027500000000     C* RICEVIMENTO PARAMETRI
027600000000     C     *ENTRY        PLIST
027700000000     C                   PARM                    KPJBA
027800000000     C*
027900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
028000000000     C     XMSGPA        PLIST
028100000000     C                   PARM                    DSMSMS
028200000000     C                   PARM                    DSMSPG
028300000000     C                   PARM                    DSMSKE
028400000000     C                   PARM                    DSMSER
028500000000     C*
028600000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
028700000000     C                   CALL      'XKEY'
028800000000     C                   PARM                    DSKEY
028900000000     C*
029000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
029100000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
029200000000     C*
029300000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
029400000000     C*   -NOME PGM CHIAMANTE
029500000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
029600160726     C                   MOVEL     'FNLVD2R'     DSMSPG
029700160726     C                   MOVEL     'FNLVD2R'     MSGQUE
029800000000     C                   MOVEL     *BLANKS       MSGKEY
029900000000     C                   MOVEL     *BLANKS       DSMSMS
030000000000     C*
030100000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
030200000000     C                   Z-ADD     0             FUNGRU            5 0
030300000000     C*
030400000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
030500000000     C                   EXSR      FUNGES
030600000000     C*
030700000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
030800000000     C                   MOVEL     '0'           F12L00            1
030900000000     C                   MOVEL     '0'           F03L00            1
031000000000     C                   MOVEL     '0'           FINL00            1
031100000000     C                   MOVEL     '0'           ERRL00            1
031200000000     C                   MOVEL     '0'           FLFINE            1
031300000000     C*
031400000000     C* LANCIO ROUTINE PER VERIFICARE SE
031500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
031600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
031700020919     C                   EXSR      REPDATIUTE
031800060520     C*
031900060520     C* IMPOSTA LA DATA CORRENTE
032000060520     C                   TIME                    N14              14 0
032100060520     C                   MOVE      N14           N8                8 0
032200060520     C                   Z-ADD     N8            G08DAT
032300060520     C                   Z-ADD     *ZEROS        G08INV
032400060520     C                   MOVEL     '0'           G08ERR
032500060520     C                   CALL      'XSRDA8'
032600060520     C                   PARM                    WLBDA8
032700060520     C                   Z-ADD     G08INV        DATCOR            8 0
032800060520     C*
032900060520     C* INIZIALIZZAZIONE VIDEO
033000160726     C                   CLEAR                   FNLVD1ds
033001160726     C                   CLEAR                   tisi95ds
033200160726     C                   Z-ADD     DATCOR        i95dat
033400000000     C*
033500000000     C                   ENDSR
033600000000     C*------------------------------------------------------------------------*
033700000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
033800000000     C*------------------------------------------------------------------------*
033900000000     C     FUNGES        BEGSR
034000000000     C*
034100000000     C* PULISCO CAMPI DI LAVORO
034200000000     C                   Z-ADD     0             W                 5 0
034300000000     C*
034400000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
034500000000     C* E CARICO TABELLA FUNZIONI ATTIVE
034600000000     C     1             DO        24            I
034700000000     C                   MOVEL     FUD(I)        FUDRIG
034800000000     C*
034900000000     C* PULIZIA FUNZIONI PRECEDENTI
035000000000     C                   MOVEL     *BLANKS       FUV(I)
035100000000     C*
035200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
035300000000     C* (SALTA F24)
035400000000     C     FUDATT        IFEQ      'S'
035500000000     C     FUDFUN        ANDNE     '24'
035600000000     C                   ADD       1             W
035700000000     C                   MOVEL     FUDDES        FUV(W)
035800000000     C                   END
035900000000     C                   END
036000000000     C*
036100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
036200000000     C* ALTRIMENTI DISATTIVO F24
036300000000     C     1             DO        24            I
036400000000     C                   MOVEL     FUD(I)        FUDRIG
036500000000     C*
036600000000     C     FUDFUN        IFEQ      '24'
036700000000     C     W             IFGT      6
036800000000     C                   MOVEL     'S'           FUDATT
036900000000     C                   ELSE
037000000000     C                   MOVEL     *BLANKS       FUDATT
037100000000     C                   END
037200000000     C                   MOVEL     FUDRIG        FUD(I)
037300000000     C                   END
037400000000     C                   END
037500000000     C*
037600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
037700000000     C     W             DIV       6             I
037800000000     C                   MVR                     Z
037900000000     C*
038000000000     C     Z             IFNE      0
038100000000     C     I             ADD       1             Z
038200000000     C                   ELSE
038300000000     C                   Z-ADD     I             Z                 5 0
038400000000     C                   END
038500000000     C*
038600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
038700000000     C                   ADD       1             FUNGRU
038800000000     C     FUNGRU        IFGT      Z
038900000000     C     FUNGRU        ORLT      1
039000000000     C                   Z-ADD     1             FUNGRU
039100000000     C                   END
039200000000     C*
039300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
039400000000     C     FUNGRU        IFEQ      1
039500000000     C                   MOVEL     FUV(1)        FUN001
039600000000     C                   MOVEL     FUV(2)        FUN002
039700000000     C                   MOVEL     FUV(3)        FUN003
039800000000     C                   MOVEL     FUV(4)        FUN004
039900000000     C                   MOVEL     FUV(5)        FUN005
040000000000     C                   MOVEL     FUV(6)        FUN006
040100000000     C                   END
040200000000     C*
040300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
040400000000     C     FUNGRU        IFEQ      2
040500000000     C                   MOVEL     FUV(7)        FUN001
040600000000     C                   MOVEL     FUV(8)        FUN002
040700000000     C                   MOVEL     FUV(9)        FUN003
040800000000     C                   MOVEL     FUV(10)       FUN004
040900000000     C                   MOVEL     FUV(11)       FUN005
041000000000     C                   MOVEL     FUV(12)       FUN006
041100000000     C                   END
041200000000     C*
041300000000     C* TERZO GRUPPO DI 6 FUNZIONI
041400000000     C     FUNGRU        IFEQ      3
041500000000     C                   MOVEL     FUV(13)       FUN001
041600000000     C                   MOVEL     FUV(14)       FUN002
041700000000     C                   MOVEL     FUV(15)       FUN003
041800000000     C                   MOVEL     FUV(16)       FUN004
041900000000     C                   MOVEL     FUV(17)       FUN005
042000000000     C                   MOVEL     FUV(18)       FUN006
042100000000     C                   END
042200000000     C*
042300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
042400000000     C     FUNGRU        IFEQ      4
042500000000     C                   MOVEL     FUV(19)       FUN001
042600000000     C                   MOVEL     FUV(20)       FUN002
042700000000     C                   MOVEL     FUV(21)       FUN003
042800000000     C                   MOVEL     FUV(22)       FUN004
042900000000     C                   MOVEL     *BLANKS       FUN005
043000000000     C                   MOVEL     *BLANKS       FUN006
043100000000     C                   END
043200000000     C*
043300000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
043400000000     C                   MOVEL     *BLANKS       FUN007
043500000000     C                   MOVEL     *BLANKS       FUN008
043600000000     C*
043700000000     C     1             DO        24            I
043800000000     C                   MOVEL     FUD(I)        FUDRIG
043900000000     C     FUDFUN        IFEQ      '24'
044000000000     C     FUDATT        ANDEQ     'S'
044100000000     C                   MOVEL     FUDDES        FUN008
044200000000     C                   END
044300000000     C                   END
044400000000     C                   ENDSR
044500000000     C*------------------------------------------------------------------------*
044600000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
044700000000     C*------------------------------------------------------------------------*
044800000000     C     FUNCON        BEGSR
044900000000     C*
045000000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
045100000000     C                   EXSR      FUNVAR
045200000000     C*
045300000000     C* PULISCO CAMPI DI LAVORO
045400000000     C                   Z-ADD     0             I                 5 0
045500000000     C                   Z-ADD     0             W                 5 0
045600000000     C*
045700000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
045800000000     C     FUNKEY        IFNE      *BLANKS
045900000000     C     1             DO        24            I
046000000000     C                   MOVEL     FUD(I)        FUDRIG
046100000000     C*
046200000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
046300000000     C     FUNKEY        IFEQ      FUDFUN
046400000000     C     FUDATT        ANDEQ     'S'
046500000000     C                   Z-ADD     I             W
046600000000     C                   END
046700000000     C                   END
046800000000     C*
046900000000     C* FUNZIONE NON ATTIVA (ERRORE)
047000000000     C     W             IFEQ      0
047100000000     C*
047200000000     C* EMISSIONE MESSAGGIO DI ERRORE
047300000000     C                   MOVEL     MSG(1)        DSMSMS
047400000000     C*
047500000000     C                   END
047600000000     C                   END
047700000000     C*
047800000000     C* FUNZIONE ATTIVA (PROCEDI ...)
047900000000     C     W             IFNE      0
048000000000     C     FUNKEY        OREQ      *BLANKS
048100000000     C*
048200000000     C     FUNKEY        IFEQ      *BLANKS
048300000000     C                   EXSR      ENTGES
048400000000     C                   END
048500000000     C     FUNKEY        IFEQ      '01'
048600000000     C                   END
048700000000     C     FUNKEY        IFEQ      '03'
048800000000     C                   EXSR      F03GES
048900000000     C                   END
049000000000     C     FUNKEY        IFEQ      '06'
049100000000     C                   END
049200000000     C     FUNKEY        IFEQ      '07'
049300000000     C                   END
049400000000     C     FUNKEY        IFEQ      '08'
049500000000     C                   END
049600000000     C     FUNKEY        IFEQ      '12'
049700000000     C                   EXSR      F12GES
049800000000     C                   END
049900000000     C     FUNKEY        IFEQ      '21'
050000000000     C                   END
050100000000     C     FUNKEY        IFEQ      '22'
050200000000     C                   END
050300000000     C     FUNKEY        IFEQ      '24'
050400000000     C                   EXSR      F24GES
050500000000     C                   END
050600000000     C     FUNKEY        IFEQ      'HP'
050700000000     C                   END
050800000000     C*
050900000000     C                   END
051000000000     C*
051100000000     C                   ENDSR
051200000000     C*------------------------------------------------------------------------*
051300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051400000000     C*------------------------------------------------------------------------*
051500000000     C     FUNVAR        BEGSR
051600000000     C*
051700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
051800000000     C*
051900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
052000000000     C     KEY           IFEQ      ENTER
052100000000     C                   MOVEL     *BLANKS       FUNKEY
052200000000     C                   END
052300000000     C     KEY           IFEQ      F1
052400000000     C                   MOVEL     '01'          FUNKEY
052500000000     C                   END
052600000000     C     KEY           IFEQ      F2
052700000000     C                   MOVEL     '02'          FUNKEY
052800000000     C                   END
052900000000     C     KEY           IFEQ      F3
053000000000     C                   MOVEL     '03'          FUNKEY
053100000000     C                   END
053200000000     C     KEY           IFEQ      F4
053300000000     C                   MOVEL     '04'          FUNKEY
053400000000     C                   END
053500000000     C     KEY           IFEQ      F5
053600000000     C                   MOVEL     '05'          FUNKEY
053700000000     C                   END
053800991103     C     KEY           IFEQ      F6
053900000000     C                   MOVEL     '06'          FUNKEY
054000000000     C                   END
054100000000     C     KEY           IFEQ      F7
054200000000     C                   MOVEL     '07'          FUNKEY
054300000000     C                   END
054400000000     C     KEY           IFEQ      F8
054500000000     C                   MOVEL     '08'          FUNKEY
054600000000     C                   END
054700000000     C     KEY           IFEQ      F9
054800000000     C                   MOVEL     '09'          FUNKEY
054900000000     C                   END
055000000000     C     KEY           IFEQ      F10
055100000000     C                   MOVEL     '10'          FUNKEY
055200000000     C                   END
055300000000     C     KEY           IFEQ      F11
055400000000     C                   MOVEL     '11'          FUNKEY
055500000000     C                   END
055600000000     C     KEY           IFEQ      F12
055700000000     C                   MOVEL     '12'          FUNKEY
055800000000     C                   END
055900000000     C     KEY           IFEQ      F13
056000000000     C                   MOVEL     '13'          FUNKEY
056100000000     C                   END
056200000000     C     KEY           IFEQ      F14
056300000000     C                   MOVEL     '14'          FUNKEY
056400000000     C                   END
056500000000     C     KEY           IFEQ      F15
056600000000     C                   MOVEL     '15'          FUNKEY
056700000000     C                   END
056800000000     C     KEY           IFEQ      F16
056900000000     C                   MOVEL     '16'          FUNKEY
057000000000     C                   END
057100000000     C     KEY           IFEQ      F17
057200000000     C                   MOVEL     '17'          FUNKEY
057300000000     C                   END
057400000000     C     KEY           IFEQ      F18
057500000000     C                   MOVEL     '18'          FUNKEY
057600000000     C                   END
057700000000     C     KEY           IFEQ      F19
057800000000     C                   MOVEL     '19'          FUNKEY
057900000000     C                   END
058000000000     C     KEY           IFEQ      F20
058100000000     C                   MOVEL     '20'          FUNKEY
058200000000     C                   END
058300000000     C     KEY           IFEQ      F21
058400000000     C                   MOVEL     '21'          FUNKEY
058500000000     C                   END
058600000000     C     KEY           IFEQ      F22
058700000000     C                   MOVEL     '22'          FUNKEY
058800000000     C                   END
058900000000     C     KEY           IFEQ      F23
059000000000     C                   MOVEL     '23'          FUNKEY
059100000000     C                   END
059200000000     C     KEY           IFEQ      F24
059300000000     C                   MOVEL     '24'          FUNKEY
059400000000     C                   END
059500000000     C     KEY           IFEQ      ROLLUP
059600000000     C                   MOVEL     'RU'          FUNKEY
059700000000     C                   END
059800000000     C     KEY           IFEQ      HELP
059900000000     C                   MOVEL     'HP'          FUNKEY
060000000000     C                   END
060100000000     C                   ENDSR
060200000000     C*------------------------------------------------------------------------*
060300000000     C* NOTE PER COMPILAZIONE SCHIERE
060400000000     C*------------------------------------------------------------------------*
060500000000     C* -MSG-
060600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060700000000     C* (NON TOCCARE MSG,1)
060800000000     C*
060900000000     C* -CMD-
061000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061100000000     C*
061200000000     C* -FUD-
061300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
061400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
061700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
061800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
061900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
062000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
062100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
062200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
062300000000     C*
062400000000     C*------------------------------------------------------------------------*
062500991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062600060520TISIE4R- Tasto funzionale non gestito.                                         1
062700060520TISIE4R- File Versioni "CAPPARIO DPD" vuoto o data non coperta da versione.    2
062800060520TISIE4R- Campo obbligatorio, inserire una data.                                3
062900060520TISIE4R- Data errata.                                                          4
063000060520TISIE4R- Campo obbligatorio, inserire il CAP                                   5
063100060520TISIE4R-                                                                       6
063200060520TISIE4R-                                                                       7
063300060520TISIE4R-                                                                       8
063400060520TISIE4R-                                                                       9
063500060520TISIE4R-                                                                      10
063600060520TISIE4R-                                                                      11
063700060520TISIE4R-                                                                      12
063800060520TISIE4R-                                                                      13
063900060520TISIE4R-                                                                      14
064000060520TISIE4R-                                                                      15
064100060520TISIE4R-                                                                      16
064200060520TISIE4R-                                                                      17
064300060520TISIE4R-                                                                      18
064400060520TISIE4R-                                                                      19
064500060520TISIE4R-                                                                      20
064600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
064700000000                   -        (S/N)=N                        funz.01¬
064800000000                   -        (S/N)=N                        funz.02¬
064900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
065000000000                   -        (S/N)=N                        funz.04¬
065100000000                   -        (S/N)=N                        funz.05¬
065200000216F06=Importazione   -        (S/N)=N                        funz.06¬
065300000104                   -        (S/N)=N                        funz.07¬
065400000105                   -        (S/N)=N                        funz.08¬
065500000000                   -        (S/N)=N                        funz.09¬
065600000000                   -        (S/N)=N                        funz.10¬
065700000000                   -        (S/N)=N                        funz.11¬
065800991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
065900000000                   -        (S/N)=N                        funz.13¬
066000000000                   -        (S/N)=N                        funz.14¬
066100000000                   -        (S/N)=N                        funz.15¬
066200000000                   -        (S/N)=N                        funz.16¬
066300000000                   -        (S/N)=N                        funz.17¬
066400000000                   -        (S/N)=N                        funz.18¬
066500000000                   -        (S/N)=N                        funz.19¬
066600000000                   -        (S/N)=N                        funz.20¬
066700000000                   -        (S/N)=N                        funz.21¬
066800000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
066900000000                   -        (S/N)=N                        funz.23¬
067000991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
