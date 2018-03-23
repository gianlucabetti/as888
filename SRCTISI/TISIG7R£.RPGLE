000100000000     H*------------------------------------------------------------------------*
000200030109     H* SALDIFICAZIONE MOVIMENTI GABBIE/BANCALI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800030109     FTISIG7D   CF   E             WORKSTN INFDS(DEVDS1)
000900030109     F                                     SFILE(SIG7DMS:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D*-------------------
001800000000     D* COMANDI
001900000000     D*-------------------
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* DESCRIZIONI TASTI FUNZIONALI
002300000000     D*-------------------
002400000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002500000000     D*-------------------
002600000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002700000000     D*-------------------
002800000000     D FUV             S             19    DIM(24)
002900000914     D*------------------
003000000914     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000914     D*------------------
003200000914     D WLBDA8          DS                  INZ
003300000914     D  G08DAT                 1      8  0
003400000914     D  G08INV                 9     16  0
003500000914     D  G08ERR                17     17
003600000914     D  G08TGI                18     22  0
003700000000     D*------------------------------------------------------------------------*
003800000000     D* INPUT
003900000000     D*------------------------------------------------------------------------*
004000000000     D*-------------------
004100000000     D* RIDEFINIZIONE SCHIERA -FUD-
004200000000     D*-------------------
004300000000     D FUDDS           DS
004400000000     D*                -DESCRIZIONE PARZIALE
004500000000     D  FUDDE1                 1      1
004600000000     D*                -FUNZIONE
004700000000     D  FUDFUN                 2      3
004800000000     D*                -DESCRIZIONE PARZIALE
004900000000     D  FUDDE2                 4     19
005000000000     D*                -FILLER
005100000000     D  FUDFI0                20     34
005200000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005300000000     D  FUDATT                35     35
005400000000     D*                -FILLER
005500000000     D  FUDFI1                36     80
005600000000     D*                -DESCRIZIONE COMPLETA
005700000000     D  FUDDES                 1     19
005800000000     D*                -RIDEFINIZIONE RIGA
005900000000     D  FUDRIG                 1     80
006000000000     D*-------------------
006100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006200000000     D*-------------------
006300000000     D DSMSG         E DS
006400000000     D*-------------------
006500000000     D* DS PARAMETRI DI LANCIO PGM
006600000000     D*-------------------
006700950703     D PARAM           DS
006800030109     D  PARDAF                 1      8  0
006900000000     D*-------------------
007000000000     D* DS PER GESTIONE TASTI FUNZIONALI
007100000000     D*-------------------
007200000000     D DSKEY         E DS
007300000000     D DEVDS1          DS
007400000000     D  KEY                  369    369
007500030123     D*------------
007600030123     D* DS REPERIMENTO DATI UTENTE
007700030123     D*------------
007800030123     D TIBS34DS      E DS                                                       *Profili utente
007900030123     D DDATIUTE      E DS                                                       *Dati utente
008000030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008100000000     D*-------------------
008200000000     D* ARCHITETTURA
008300000000     D*-------------------
008400000000     D KPJBA         E DS
008500000000     C*------------------------------------------------------------------------*
008600000000     C* MAIN LINE
008700000000     C*------------------------------------------------------------------------*
008800000000     C*
008900000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
009100000000     C*
009200000000     C* CICLO FINO A CHE:
009300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009500000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009700000000     C     F12L00        OREQ      '1'
009800000000     C     FLFINE        OREQ      '1'
009900000000     C*
010000000000     C* EMETTE FORMATO VIDEO
010100000000     C                   EXSR      BUTFMR
010200000000     C*
010300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010400000000     C                   EXSR      FUNCON
010500000000     C*
010600000914     C                   ENDDO                                                  <-- 2 --
010700000000     C*
010800000914     C                   ENDIF                                                  <-- 1 --
010900991118     C*
011000000000     C                   SETON                                        LR
011100000000     C*------------------------------------------------------------------------*
011200000000     C* EMISSIONE FORMATO A VIDEO
011300000000     C*------------------------------------------------------------------------*
011400000000     C     BUTFMR        BEGSR
011500000000     C*
011600000000     C* WRITE RECORD TESTATA
011700030109     C                   WRITE     SIG7D01
011800000000     C*
011900000000     C* VISUALIZZAZIONE MESSAGGIO
012000000000     C                   EXSR      SNDMSG
012100000000     C*
012200000000     C* WRITE RECORD FUNZIONI
012300030109     C                   WRITE     SIG7D02
012400991019     C*
012500991019     C* EXFMT RECORD DETTAGLIO
012600030109     C                   EXFMT     SIG7D03
012700991019     C*
012800000000     C* AZZERO FLAG DI ERRORE LIVELLO
012900000000     C                   MOVEL     '0'           ERRL00
013000000000     C*
013100000000     C* AZZERO FLAG DI ERRORE
013200991025     C                   SETOFF                                         7071
013300000000     C*
013400000000     C                   ENDSR
013500000000     C*------------------------------------------------------------------------*
013600000000     C* SNDMSG - MANDA MESSAGGIO
013700000000     C*------------------------------------------------------------------------*
013800000000     C     SNDMSG        BEGSR
013900000000     C*
014000000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014100000000     C                   MOVEL     '1'           *IN40
014200030109     C                   WRITE     SIG7DMC
014300000000     C                   MOVEL     '0'           *IN40
014400000000     C*
014500000000     C* VISUALIZZAZIONE MESSAGGIO
014600000000     C                   Z-ADD     0             MSGULT            4 0
014700000000     C                   CALL      'XMSG'        XMSGPA
014800000000     C     DSMSER        IFEQ      0
014900000000     C                   MOVEL     DSMSKE        MSGKEY
015000000000     C                   END
015100000000     C                   Z-ADD     1             MSGULT
015200030109     C                   WRITE     SIG7DMS                                      SFL MESS
015300030109     C                   WRITE     SIG7DMC                                      CONTR MESS
015400000000     C*
015500000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015600000000     C                   MOVEL     *BLANKS       DSMSMS
015700000000     C*
015800000000     C                   ENDSR
015900000000     C*------------------------------------------------------------------------*
016000000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016100000000     C*------------------------------------------------------------------------*
016200000000     C     ENTGES        BEGSR
016300000000     C*
016400000000     C* CONTROLLA I CAMPI IMMESSI
016500000000     C                   EXSR      DECODI
016600000000     C*
016700000000     C                   ENDSR
016800000000     C*------------------------------------------------------------------------*
016900000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017000000000     C*------------------------------------------------------------------------*
017100000000     C     F03GES        BEGSR
017200000000     C*
017300000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017400000000     C                   MOVEL     '1'           F03L00
017500000000     C*
017600000000     C                   ENDSR
017700000000     C*------------------------------------------------------------------------*
017800000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
017900000000     C*------------------------------------------------------------------------*
018000991020     C     F06GES        BEGSR
018100000000     C*
018200991105     C                   EXSR      DECODI
018300991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
018400991105     C     ERRL00        IFEQ      '0'
018500991105     C*
018600000914     C* LANCIO IL PROGRAMMA DI GENERAZIONE ARCHIVIO
018700030109     C                   MOVEL     'SIG8'        KCOAZ
018800000000     C                   EXSR      VIDPAR
018900950703     C                   MOVEL     PARAM         KPJBU
019000030122     C                   CALL      'BCH10'
019100030122 MOM C***                CALL      'TISIG8C'
019200030109     C                   PARM                    KPJBA
019300000927     C*
019400000927     C* IMPOSTO FLAG F03 DI LIVELLO PGM
019500000927     C                   MOVEL     '1'           F03L00
019600000914     C*
019700991105     C                   ENDIF
019800991105     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
020200000000     C*------------------------------------------------------------------------*
020300000000     C     F12GES        BEGSR
020400000000     C*
020500000000     C* IMPOSTO FLAG DI LIVELLO
020600000000     C                   MOVEL     '1'           F12L00
020700000000     C*
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
021100000000     C*------------------------------------------------------------------------*
021200000000     C     F24GES        BEGSR
021300000000     C*
021400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
021500000000     C                   EXSR      FUNGES
021600000000     C*
021700000000     C                   ENDSR
021800000914     C*------------------------------------------------------------------------*
021900000914     C* CHKDA8 - CONTROLLA LA DATA (8)
022000000914     C*------------------------------------------------------------------------*
022100000914     C     CHKDA8        BEGSR
022200030109     C*
022300030109     C     *LIKE         DEFINE    VIDDAF        WDA8                           gg/mm/aaaa
022400030109     C     *LIKE         DEFINE    VIDDAF        DA8W                           aaaa/mm/gg
022500000914     C*
022600030109     C                   Z-ADD     VIDDAF        G08DAT
022700000914     C                   Z-ADD     *ZEROS        G08INV
022800000914     C                   MOVEL     '0'           G08ERR
022900000914     C                   CALL      'XSRDA8'
023000000914     C                   PARM                    WLBDA8
023100030109     C                   Z-ADD     G08DAT        WDA8
023200030109     C                   Z-ADD     G08INV        DA8W
023300030109     C     G08ERR        IFEQ      '1'
023400000914     C                   MOVEL     '1'           ERRL00
023500030109     C                   ENDIF
023600000914     C*
023700000914     C                   ENDSR
023800991020     C*------------------------------------------------------------------------*
023900000914     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
024000000000     C*------------------------------------------------------------------------*
024100000000     C     DECODI        BEGSR
024200000914     C*
024300000914     C                   MOVEL     '0'           ERRL00
024400000914     C*--------------------
024500000914     C* DATA FINALE
024600000914     C*--------------------
024700030109     C     *LIKE         DEFINE    VIDDAF        DAFVID
024800030109     C     ERRL00        IFEQ      '0'
024900000914     C                   Z-ADD     VIDDAF        WDA8
025000000914     C                   Z-ADD     *ZEROS        DA8W
025100000914     C                   EXSR      CHKDA8
025200000914     C                   Z-ADD     WDA8          VIDDAF
025300030109     C                   Z-ADD     DA8W          DAFVID
025400030109     C     ERRL00        IFEQ      '1'
025500000914     C                   SETON                                        71
025600000914     C                   MOVEL     MSG(16)       DSMSMS
025700030109     C                   ENDIF
025800030109     C                   ENDIF
025900000000     C*
026000000000     C                   ENDSR
026100000000     C*------------------------------------------------------------------------*
026200000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
026300000000     C*------------------------------------------------------------------------*
026400000000     C     AZZVID        BEGSR
026500000000     C*
026600030109     C                   Z-ADD     *ZEROS        VIDDAF
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
027100000000     C*------------------------------------------------------------------------*
027200000000     C     VIDPAR        BEGSR
027300000000     C*
027400000914     C                   Z-ADD     DAFVID        PARDAF
027500000000     C*
027600000000     C                   ENDSR
027700030123     C*--------------------------------------------------------------------------------------------*
027800030123     C* REPERISCE I DATI UTENTE
027900030123     C*--------------------------------------------------------------------------------------------*
028000030123     C     REPDATIUTE    BEGSR
028100030123     C*
028200030123     C* INIZIALIZZA VARIABILI DI WRK
028300030123     C                   CLEAR                   TIBS34DS
028400030123     C                   CLEAR                   AZUTEDS
028500030123     C                   CLEAR                   DDATIUTE
028600030123     C*
028700030123     C     *DTAARA       DEFINE    §azute        azuteds
028800030123     C     *DTAARA       DEFINE    §datiute      ddatiute
028900030123     C                   IN(E)     *DTAARA
029000030123     C                   IF        %Error
029100030123     C                   EVAL      I34Tla = 'L'
029200030123     C                   CALL      'TIBS34R'
029300030123     C                   PARM                    Tibs34Ds
029400030123     C                   IN        *DTAARA
029500030123     C                   ENDIF
029600030123     C*
029700030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
029800030123     C                   MOVEL(P)  RSUT          DSFIRS           20
029900030123     C*
030000030123     C                   ENDSR
030100000000     C*------------------------------------------------------------------------*
030200980403     C* *INZSR - OPERAZIONI INIZIALI
030300000000     C*------------------------------------------------------------------------*
030400980403     C     *INZSR        BEGSR
030500000000     C*
030600000000     C* RICEVIMENTO PARAMETRI
030700000000     C     *ENTRY        PLIST
030800000000     C                   PARM                    KPJBA
030900030123     C*
031000030123     C* REPERIMENTO DATI UTENTE
031100030123     C                   EXSR      REPDATIUTE
031200000000     C*
031300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
031400000000     C     XMSGPA        PLIST
031500000000     C                   PARM                    DSMSMS
031600000000     C                   PARM                    DSMSPG
031700000000     C                   PARM                    DSMSKE
031800000000     C                   PARM                    DSMSER
031900000000     C*
032000000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
032100000000     C                   CALL      'XKEY'
032200000000     C                   PARM                    DSKEY
032300000000     C*
032400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
032500000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
032600000000     C*
032700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
032800000000     C*   -NOME PGM CHIAMANTE
032900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
033000030109     C                   MOVEL     'TISIG7R'     DSMSPG
033100030109     C                   MOVEL     'TISIG7R'     MSGQUE
033200000000     C                   MOVEL     *BLANKS       MSGKEY
033300000000     C                   MOVEL     *BLANKS       DSMSMS
033400000000     C*
033500000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
033600000000     C                   Z-ADD     0             FUNGRU            5 0
033700000000     C*
033800000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
033900000000     C                   EXSR      FUNGES
034000000000     C*
034100010208     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
034200000000     C                   MOVEL     '0'           F12L00            1
034300000000     C                   MOVEL     '0'           F03L00            1
034400000000     C                   MOVEL     '0'           FINL00            1
034500000000     C                   MOVEL     '0'           ERRL00            1
034600000000     C                   MOVEL     '0'           FLFINE            1
034700000000     C*
034800000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
034900000000     C                   EXSR      AZZVID
035000010208     C*
035100000000     C                   ENDSR
035200000000     C*------------------------------------------------------------------------*
035300000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
035400000000     C*------------------------------------------------------------------------*
035500000000     C     FUNGES        BEGSR
035600000000     C*
035700000000     C* PULISCO CAMPI DI LAVORO
035800000000     C                   Z-ADD     0             W                 5 0
035900000000     C*
036000000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
036100000000     C* E CARICO TABELLA FUNZIONI ATTIVE
036200000000     C     1             DO        24            I
036300000000     C                   MOVEL     FUD(I)        FUDRIG
036400000000     C*
036500000000     C* PULIZIA FUNZIONI PRECEDENTI
036600000000     C                   MOVEL     *BLANKS       FUV(I)
036700000000     C*
036800000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
036900000000     C* (SALTA F24)
037000000000     C     FUDATT        IFEQ      'S'
037100000000     C     FUDFUN        ANDNE     '24'
037200000000     C                   ADD       1             W
037300000000     C                   MOVEL     FUDDES        FUV(W)
037400000000     C                   END
037500000000     C                   END                                                    DO WHILE
037600000000     C*
037700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
037800000000     C* ALTRIMENTI DISATTIVO F24
037900000000     C     1             DO        24            I
038000000000     C                   MOVEL     FUD(I)        FUDRIG
038100000000     C*
038200000000     C     FUDFUN        IFEQ      '24'
038300000000     C     W             IFGT      6
038400000000     C                   MOVEL     'S'           FUDATT
038500000000     C                   ELSE
038600000000     C                   MOVEL     *BLANKS       FUDATT
038700000000     C                   END                                                    W GT 8
038800000000     C                   MOVEL     FUDRIG        FUD(I)
038900000000     C                   END
039000000000     C                   END                                                    DO WHILE
039100000000     C*
039200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
039300000000     C     W             DIV       6             I
039400000000     C                   MVR                     Z
039500000000     C*
039600000000     C     Z             IFNE      0
039700000000     C     I             ADD       1             Z
039800000000     C                   ELSE
039900000000     C                   Z-ADD     I             Z                 5 0
040000000000     C                   END
040100000000     C*
040200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
040300000000     C                   ADD       1             FUNGRU
040400000000     C     FUNGRU        IFGT      Z
040500000000     C     FUNGRU        ORLT      1
040600000000     C                   Z-ADD     1             FUNGRU
040700000000     C                   END
040800000000     C*
040900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
041000000000     C     FUNGRU        IFEQ      1
041100000000     C                   MOVEL     FUV(1)        FUN001
041200000000     C                   MOVEL     FUV(2)        FUN002
041300000000     C                   MOVEL     FUV(3)        FUN003
041400000000     C                   MOVEL     FUV(4)        FUN004
041500000000     C                   MOVEL     FUV(5)        FUN005
041600000000     C                   MOVEL     FUV(6)        FUN006
041700000000     C                   END
041800000000     C*
041900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
042000000000     C     FUNGRU        IFEQ      2
042100000000     C                   MOVEL     FUV(7)        FUN001
042200000000     C                   MOVEL     FUV(8)        FUN002
042300000000     C                   MOVEL     FUV(9)        FUN003
042400000000     C                   MOVEL     FUV(10)       FUN004
042500000000     C                   MOVEL     FUV(11)       FUN005
042600000000     C                   MOVEL     FUV(12)       FUN006
042700000000     C                   END
042800000000     C*
042900000000     C* TERZO GRUPPO DI 6 FUNZIONI
043000000000     C     FUNGRU        IFEQ      3
043100000000     C                   MOVEL     FUV(13)       FUN001
043200000000     C                   MOVEL     FUV(14)       FUN002
043300000000     C                   MOVEL     FUV(15)       FUN003
043400000000     C                   MOVEL     FUV(16)       FUN004
043500000000     C                   MOVEL     FUV(17)       FUN005
043600000000     C                   MOVEL     FUV(18)       FUN006
043700000000     C                   END
043800000000     C*
043900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
044000000000     C     FUNGRU        IFEQ      4
044100000000     C                   MOVEL     FUV(19)       FUN001
044200000000     C                   MOVEL     FUV(20)       FUN002
044300000000     C                   MOVEL     FUV(21)       FUN003
044400000000     C                   MOVEL     FUV(22)       FUN004
044500000000     C                   MOVEL     *BLANKS       FUN005
044600000000     C                   MOVEL     *BLANKS       FUN006
044700000000     C                   END
044800000000     C*
044900000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
045000000000     C                   MOVEL     *BLANKS       FUN007
045100000000     C                   MOVEL     *BLANKS       FUN008
045200000000     C*
045300000000     C     1             DO        24            I
045400000000     C                   MOVEL     FUD(I)        FUDRIG
045500000000     C     FUDFUN        IFEQ      '24'
045600000000     C     FUDATT        ANDEQ     'S'
045700000000     C                   MOVEL     FUDDES        FUN008
045800000000     C                   END
045900000000     C                   END                                                    DO
046000000000     C                   ENDSR
046100000000     C*------------------------------------------------------------------------*
046200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
046300000000     C*------------------------------------------------------------------------*
046400000000     C     FUNCON        BEGSR
046500000000     C*
046600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
046700000000     C                   EXSR      FUNVAR
046800000000     C*
046900000000     C* PULISCO CAMPI DI LAVORO
047000000000     C                   Z-ADD     0             I                 5 0
047100000000     C                   Z-ADD     0             W                 5 0
047200000000     C*
047300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
047400000000     C     FUNKEY        IFNE      *BLANKS
047500000000     C     1             DO        24            I
047600000000     C                   MOVEL     FUD(I)        FUDRIG
047700000000     C*
047800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
047900000000     C     FUNKEY        IFEQ      FUDFUN
048000000000     C     FUDATT        ANDEQ     'S'
048100000000     C                   Z-ADD     I             W
048200000000     C                   END
048300000000     C                   END                                                    DO WHILE
048400000000     C*
048500000000     C* FUNZIONE NON ATTIVA (ERRORE)
048600000000     C     W             IFEQ      0
048700000000     C*
048800000000     C* EMISSIONE MESSAGGIO DI ERRORE
048900000000     C                   MOVEL     MSG(1)        DSMSMS
049000000000     C*
049100000000     C                   END                                                    W=0
049200000000     C                   END                                                    FUNKEY NE *BLANKS
049300000000     C*
049400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
049500000000     C     W             IFNE      0
049600000000     C     FUNKEY        OREQ      *BLANKS
049700000000     C*
049800000000     C     FUNKEY        IFEQ      *BLANKS
049900000000     C                   EXSR      ENTGES
050000000000     C                   END
050100000000     C     FUNKEY        IFEQ      '03'
050200000000     C                   EXSR      F03GES
050300000000     C                   END
050400000000     C     FUNKEY        IFEQ      '06'
050500000000     C                   EXSR      F06GES
050600000000     C                   END
050700000000     C     FUNKEY        IFEQ      '12'
050800000000     C                   EXSR      F12GES
050900000000     C                   END
051000000000     C     FUNKEY        IFEQ      '24'
051100000000     C                   EXSR      F24GES
051200000000     C                   END
051300000000     C*
051400000000     C                   END
051500000000     C*
051600000000     C                   ENDSR
051700000000     C*------------------------------------------------------------------------*
051800000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051900000000     C*------------------------------------------------------------------------*
052000000000     C     FUNVAR        BEGSR
052100000000     C*
052200000000     C* INIZIALIZZA VARIABILE -FUNKEY-
052300000000     C*
052400000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
052500000000     C     KEY           IFEQ      ENTER
052600000000     C                   MOVEL     *BLANKS       FUNKEY
052700000000     C                   END
052800000000     C     KEY           IFEQ      F1
052900000000     C                   MOVEL     '01'          FUNKEY
053000000000     C                   END
053100000000     C     KEY           IFEQ      F2
053200000000     C                   MOVEL     '02'          FUNKEY
053300000000     C                   END
053400000000     C     KEY           IFEQ      F3
053500000000     C                   MOVEL     '03'          FUNKEY
053600000000     C                   END
053700000000     C     KEY           IFEQ      F4
053800000000     C                   MOVEL     '04'          FUNKEY
053900000000     C                   END
054000000000     C     KEY           IFEQ      F5
054100000000     C                   MOVEL     '05'          FUNKEY
054200000000     C                   END
054300000000     C     KEY           IFEQ      F6
054400000000     C                   MOVEL     '06'          FUNKEY
054500000000     C                   END
054600000000     C     KEY           IFEQ      F7
054700000000     C                   MOVEL     '07'          FUNKEY
054800000000     C                   END
054900000000     C     KEY           IFEQ      F8
055000000000     C                   MOVEL     '08'          FUNKEY
055100000000     C                   END
055200000000     C     KEY           IFEQ      F9
055300000000     C                   MOVEL     '09'          FUNKEY
055400000000     C                   END
055500000000     C     KEY           IFEQ      F10
055600000000     C                   MOVEL     '10'          FUNKEY
055700000000     C                   END
055800000000     C     KEY           IFEQ      F11
055900000000     C                   MOVEL     '11'          FUNKEY
056000000000     C                   END
056100000000     C     KEY           IFEQ      F12
056200000000     C                   MOVEL     '12'          FUNKEY
056300000000     C                   END
056400000000     C     KEY           IFEQ      F13
056500000000     C                   MOVEL     '13'          FUNKEY
056600000000     C                   END
056700000000     C     KEY           IFEQ      F14
056800000000     C                   MOVEL     '14'          FUNKEY
056900000000     C                   END
057000000000     C     KEY           IFEQ      F15
057100000000     C                   MOVEL     '15'          FUNKEY
057200000000     C                   END
057300000000     C     KEY           IFEQ      F16
057400000000     C                   MOVEL     '16'          FUNKEY
057500000000     C                   END
057600000000     C     KEY           IFEQ      F17
057700000000     C                   MOVEL     '17'          FUNKEY
057800000000     C                   END
057900000000     C     KEY           IFEQ      F18
058000000000     C                   MOVEL     '18'          FUNKEY
058100000000     C                   END
058200000000     C     KEY           IFEQ      F19
058300000000     C                   MOVEL     '19'          FUNKEY
058400000000     C                   END
058500000000     C     KEY           IFEQ      F20
058600000000     C                   MOVEL     '20'          FUNKEY
058700000000     C                   END
058800000000     C     KEY           IFEQ      F21
058900000000     C                   MOVEL     '21'          FUNKEY
059000000000     C                   END
059100000000     C     KEY           IFEQ      F22
059200000000     C                   MOVEL     '22'          FUNKEY
059300000000     C                   END
059400000000     C     KEY           IFEQ      F23
059500000000     C                   MOVEL     '23'          FUNKEY
059600000000     C                   END
059700000000     C     KEY           IFEQ      F24
059800000000     C                   MOVEL     '24'          FUNKEY
059900000000     C                   END
060000000000     C     KEY           IFEQ      ROLLUP
060100000000     C                   MOVEL     'RU'          FUNKEY
060200000000     C                   END
060300000000     C     KEY           IFEQ      HELP
060400000000     C                   MOVEL     'HP'          FUNKEY
060500000000     C                   END
060600000000     C                   ENDSR
060700000000     C*------------------------------------------------------------------------*
060800000000     C* NOTE PER COMPILAZIONE SCHIERE
060900000000     C*------------------------------------------------------------------------*
061000000000     C* -MSG-
061100000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
061200000000     C* (NON TOCCARE MSG,1)
061300000000     C*
061400000000     C* -CMD-
061500000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061600000000     C*
061700000000     C* -FUD-
061800000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
061900000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062000000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062100000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
062200000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062300000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
062400000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
062500000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
062600000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
062700000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
062800000000     C*
062900000000     C*------------------------------------------------------------------------*
063000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
063100030109TISIG7R- Tasto funzionale non gestito.                                         1
063200030109TISIG7R-
063300030109TISIG7R- Data non accettabile.                                                 3
063400030109TISIG7R-
063500030109TISIG7R-
063600030109TISIG7R- Data inizio maggiore data fine.                                       6
063700030109TISIG7R- Codice CONSEGNA ANOMALE non valido per POSTE.                         7
063800030109TISIG7R-
063900030109TISIG7R-
064000030109TISIG7R-
064100030109TISIG7R-
064200030109TISIG7R-
064300030109TISIG7R-
064400030109TISIG7R-
064500030109TISIG7R-
064600030109TISIG7R- Data non valida.                                                     16
064700030109TISIG7R-
064800030109TISIG7R-
064900030109TISIG7R-
065000030109TISIG7R-
065100000000** ======== SCHIERA: CMD  (COMANDI)                        ================
065200000000WRKSPLF
065300000000
065400000000
065500000000
065600000000
065700000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
065800000000                   -        (S/N)=N                        funz.01¬
065900000000                   -        (S/N)=N                        funz.02¬
066000000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
066100000000                   -        (S/N)=N                        funz.04¬
066200000000                   -        (S/N)=N                        funz.05¬
066300000000F06=Lancio         -        (S/N)=S                        funz.06¬
066400000000                   -        (S/N)=N                        funz.07¬
066500000000                   -        (S/N)=N                        funz.08¬
066600000000                   -        (S/N)=N                        funz.09¬
066700000000                   -        (S/N)=N                        funz.10¬
066800000000                   -        (S/N)=N                        funz.11¬
066900000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
067000000000                   -        (S/N)=N                        funz.13¬
067100000000                   -        (S/N)=N                        funz.14¬
067200000000                   -        (S/N)=N                        funz.15¬
067300000000                   -        (S/N)=N                        funz.16¬
067400000000                   -        (S/N)=N                        funz.17¬
067500000000                   -        (S/N)=N                        funz.18¬
067600000000                   -        (S/N)=N                        funz.19¬
067700000000                   -        (S/N)=N                        funz.20¬
067800000000                   -        (S/N)=N                        funz.21¬
067900000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
068000000000                   -        (S/N)=N                        funz.23¬
068100000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
