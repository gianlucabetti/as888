000100000000     H*------------------------------------------------------------------------*
000200000225     H* STATISTICA DESTINATARI - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000225     FTIST27D   CF   E             WORKSTN INFDS(DEVDS1)
000900000225     F                                     SFILE(ST27DMS:MSGULT)
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
002000000000     D* DESCRIZIONI TASTI FUNZIONALI
002100000000     D*-------------------
002200000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002500000000     D*-------------------
002600000000     D FUV             S             19    DIM(24)
002700991124     D*------------------
002800991117     D*------------------
002900000225     D* DS - X CONTROLLO E X INVERSIONE VELOCE DATA (8 NUM)
003000000225     D*------------------
003100000225     D WLBDA8          DS                  INZ
003200000225     D  G08DAT                 1      8  0
003300000225     D  G08INV                 9     16  0
003400000225     D  G08ERR                17     17
003500000225     D  G08TGI                18     22  0
003600020917     D*-------------------
003700020917     D* REPERIMENTO DATI UTENTE
003800020917     D*-------------------
003900020917     D TIBS34DS      E DS                                                       *Profili utente
004000020917     D DDATIUTE      E DS                                                       *Dati utente
004100020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004200000000     D*------------------------------------------------------------------------*
004300000000     D* INPUT
004400000000     D*------------------------------------------------------------------------*
004500000000     D*-------------------
004600000000     D* RIDEFINIZIONE SCHIERA -FUD-
004700000000     D*-------------------
004800000000     D FUDDS           DS
004900000000     D*                -DESCRIZIONE PARZIALE
005000000000     D  FUDDE1                 1      1
005100000000     D*                -FUNZIONE
005200000000     D  FUDFUN                 2      3
005300000000     D*                -DESCRIZIONE PARZIALE
005400000000     D  FUDDE2                 4     19
005500000000     D*                -FILLER
005600000000     D  FUDFI0                20     34
005700000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005800000000     D  FUDATT                35     35
005900000000     D*                -FILLER
006000000000     D  FUDFI1                36     80
006100000000     D*                -DESCRIZIONE COMPLETA
006200000000     D  FUDDES                 1     19
006300000000     D*                -RIDEFINIZIONE RIGA
006400000000     D  FUDRIG                 1     80
006500000000     D*-------------------
006600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006700000000     D*-------------------
006800000000     D DSMSG         E DS
006900000000     D*-------------------
007000000000     D* DS PARAMETRI DI LANCIO PGM
007100000000     D*-------------------
007200950703     D PARAM           DS
007300991122     D  PARDAI                 1      8  0
007400991122     D  PARDAF                 9     16  0
007500000000     D*-------------------
007600000000     D* DS PER GESTIONE TASTI FUNZIONALI
007700000000     D*-------------------
007800000000     D DSKEY         E DS
007900000000     D DEVDS1          DS
008000000000     D  KEY                  369    369
008100000000     D*-------------------
008200000000     D* ARCHITETTURA
008300000000     D*-------------------
008400000000     D KPJBA         E DS
008500991117     D*-------------------
008600000000     C*------------------------------------------------------------------------*
008700000000     C* MAIN LINE
008800000000     C*------------------------------------------------------------------------*
008900000000     C*
009000000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009100000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
009200000000     C*
009300000000     C* CICLO FINO A CHE:
009400000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009500000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009600000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009700000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009800000000     C     F12L00        OREQ      '1'
009900000000     C     FLFINE        OREQ      '1'
010000000000     C*
010100000000     C* EMETTE FORMATO VIDEO
010200000000     C                   EXSR      BUTFMR
010300000000     C*
010400000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010500000000     C                   EXSR      FUNCON
010600000000     C*
010700000000     C                   END                                                    <-- 2 --
010800000000     C*
010900000000     C                   END                                                    <-- 1 --
011000991118     C*
011100000000     C                   SETON                                        LR
011200000000     C*------------------------------------------------------------------------*
011300000000     C* EMISSIONE FORMATO A VIDEO
011400000000     C*------------------------------------------------------------------------*
011500000000     C     BUTFMR        BEGSR
011600000000     C*
011700000000     C* WRITE RECORD TESTATA
011800000225     C                   WRITE     ST27D01
011900000000     C*
012000000000     C* VISUALIZZAZIONE MESSAGGIO
012100000000     C                   EXSR      SNDMSG
012200000000     C*
012300000000     C* WRITE RECORD FUNZIONI
012400000225     C                   WRITE     ST27D02
012500991019     C*
012600991019     C* EXFMT RECORD DETTAGLIO
012700000225     C                   EXFMT     ST27D03
012800991019     C*
012900000000     C* AZZERO FLAG DI ERRORE LIVELLO
013000000000     C                   MOVEL     '0'           ERRL00
013100000000     C*
013200000000     C* AZZERO FLAG DI ERRORE
013300000301     C                   SETOFF                                       707172
013400000301     C                   SETOFF                                       738283
013500000301     C                   SETOFF                                       848586
013600000000     C*
013700000000     C                   ENDSR
013800000000     C*------------------------------------------------------------------------*
013900000000     C* SNDMSG - MANDA MESSAGGIO
014000000000     C*------------------------------------------------------------------------*
014100000000     C     SNDMSG        BEGSR
014200000000     C*
014300000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014400000000     C                   MOVEL     '1'           *IN40
014500000225     C                   WRITE     ST27DMC
014600000000     C                   MOVEL     '0'           *IN40
014700000000     C*
014800000000     C* VISUALIZZAZIONE MESSAGGIO
014900000000     C                   Z-ADD     0             MSGULT            4 0
015000000000     C                   CALL      'XMSG'        XMSGPA
015100000000     C     DSMSER        IFEQ      0
015200000000     C                   MOVEL     DSMSKE        MSGKEY
015300000000     C                   END
015400000000     C                   Z-ADD     1             MSGULT
015500000225     C                   WRITE     ST27DMS                                      SFL MESS
015600000225     C                   WRITE     ST27DMC                                      CONTR MESS
015700000000     C*
015800000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015900000000     C                   MOVEL     *BLANKS       DSMSMS
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016400000000     C*------------------------------------------------------------------------*
016500000000     C     ENTGES        BEGSR
016600000000     C*
016700000000     C* CONTROLLA I CAMPI IMMESSI
016800000000     C                   EXSR      DECODI
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
017300000000     C*------------------------------------------------------------------------*
017400000000     C     F01GES        BEGSR
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017800000000     C*------------------------------------------------------------------------*
017900000000     C     F03GES        BEGSR
018000000000     C*
018100000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
018200000000     C                   MOVEL     '1'           F03L00
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018700000000     C*------------------------------------------------------------------------*
018800991020     C     F06GES        BEGSR
018900000000     C*
019000991105     C                   EXSR      DECODI
019100991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
019200991105     C     ERRL00        IFEQ      '0'
019300991105     C*
019400000000     C* LANCIO IL PROGRAMMA DI STAMPA
019500000225     C                   MOVEL     'ST28'        KCOAZ
019600950703     C                   MOVEL     PARAM         KPJBU
019700000225     C                   CALL      'BCH10'
019800000225 MOM C*                  CALL      'TIST28R'
019900991021     C                   PARM                    KPJBA
020000991124     C*
020100991105     C                   ENDIF
020200991105     C*
020300000000     C                   ENDSR
020400000000     C*------------------------------------------------------------------------*
020500000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
020600000000     C*------------------------------------------------------------------------*
020700000000     C     F07GES        BEGSR
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
021100000000     C*------------------------------------------------------------------------*
021200000000     C     F08GES        BEGSR
021300000000     C                   ENDSR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
021600000000     C*------------------------------------------------------------------------*
021700000000     C     F12GES        BEGSR
021800000000     C*
021900000000     C* IMPOSTO FLAG DI LIVELLO
022000000000     C                   MOVEL     '1'           F12L00
022100000000     C*
022200000000     C                   ENDSR
022300000000     C*------------------------------------------------------------------------*
022400000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
022500000000     C*------------------------------------------------------------------------*
022600000000     C     F21GES        BEGSR
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
023000000000     C*------------------------------------------------------------------------*
023100000000     C     F22GES        BEGSR
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
023500000000     C*------------------------------------------------------------------------*
023600000000     C     F24GES        BEGSR
023700000000     C*
023800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
023900000000     C                   EXSR      FUNGES
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*------------------------------------------------------------------------*
024300000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
024400000000     C*------------------------------------------------------------------------*
024500000000     C     HELGES        BEGSR
024600000000     C                   ENDSR
024700991020     C*------------------------------------------------------------------------*
024800991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
024900000000     C*------------------------------------------------------------------------*
025000000000     C     DECODI        BEGSR
025100991117     C*
025200000225     C* CONTROLLO DATA INIZIALE
025300000225     C                   Z-ADD     VIDDAI        $DAT              8 0
025400000225     C                   EXSR      CHKDAT
025500000225     C     ERRL00        IFEQ      '1'
025600000225     C                   SETON                                        70
025700000225     C                   MOVEL     MSG(16)       DSMSMS
025800000225     C                   ELSE
025900000225     C                   Z-ADD     G08INV        PARDAI
026000000225     C                   ENDIF
026100000225     C*
026200000225     C* CONTROLLO DATA FINALE
026300000225     C                   Z-ADD     VIDDAF        $DAT
026400000225     C                   EXSR      CHKDAT
026500000225     C     ERRL00        IFEQ      '1'
026600000225     C                   SETON                                        71
026700000225     C                   MOVEL     MSG(16)       DSMSMS
026800000225     C                   ELSE
026900000225     C                   Z-ADD     G08INV        PARDAF
027000000225     C                   ENDIF
027100000225     C*
027200000225     C* CONTROLLO DATA FINALE > DATA INIZIALE
027300000225     C     ERRL00        IFEQ      '0'
027400000301     C     PARDAI        IFGT      PARDAF
027500000225     C                   SETON                                        7071
027600000225     C                   MOVEL     '1'           ERRL00
027700000225     C                   MOVEL     MSG(6)        DSMSMS
027800000225     C                   ENDIF
027900000225     C                   ENDIF
028000000225     C*
028100000000     C                   ENDSR
028200000225     C*------------------------------------------------------------------------*
028300000225     C* CHKDAT - CONTROLLA LA DATA
028400000225     C*          INPUT --> $DAT:  DATA
028500000225     C*------------------------------------------------------------------------*
028600000225     C     CHKDAT        BEGSR
028700000225     C*
028800000225     C* OBBLIGATORIA
028900000225IF  1C     $DAT          IFEQ      *ZEROS
029000000225     C                   MOVEL     '1'           ERRL00
029100000225X   1C                   ELSE
029200000225     C* CORRETTA
029300000225     C                   Z-ADD     $DAT          G08DAT
029400000225     C                   Z-ADD     *ZEROS        G08INV
029500000225     C                   MOVEL     '0'           G08ERR
029600000225     C                   CALL      'XSRDA8'
029700000225     C                   PARM                    WLBDA8
029800000308     C     G08ERR        IFEQ      '1'
029900000308     C                   MOVEL     '1'           ERRL00
030000000308     C                   ENDIF
030100000225     C*
030200000225E   1C                   ENDIF
030300000225     C*
030400000225     C                   ENDSR
030500000000     C*------------------------------------------------------------------------*
030600000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
030700000000     C*------------------------------------------------------------------------*
030800000000     C     AZZVID        BEGSR
030900000000     C*
031000000225     C                   Z-ADD     *zeros        VIDDAI
031100000225     C                   Z-ADD     *zeros        VIDDAF
031200000000     C*
031300000000     C                   ENDSR
031400020917     C*--------------------------------------------------------------------------------------------*
031500020917     C* REPERISCE I DATI UTENTE
031600020917     C*--------------------------------------------------------------------------------------------*
031700020917     C     REPDATIUTE    BEGSR
031800020917     C*
031900020917     C* INIZIALIZZA VARIABILI DI WRK
032000020917     C                   CLEAR                   TIBS34DS
032100020917     C                   CLEAR                   AZUTEDS
032200020917     C                   CLEAR                   DDATIUTE
032300020917     C*
032400020917     C     *DTAARA       DEFINE    §azute        azuteds
032500020917     C     *DTAARA       DEFINE    §datiute      ddatiute
032600020917     C                   IN(E)     *DTAARA
032700020917if  1C                   IF        %Error
032800020917     c                   EVAL      I34Tla = 'L'
032900020917     C                   CALL      'TIBS34R'
033000020917     C                   PARM                    Tibs34Ds
033100020917     C                   IN        *DTAARA
033200020917e   1C                   ENDIF
033300020917     C*
033400020917     C                   ENDSR
033500000000     C*------------------------------------------------------------------------*
033600980403     C* *INZSR - OPERAZIONI INIZIALI
033700000000     C*------------------------------------------------------------------------*
033800980403     C     *INZSR        BEGSR
033900000000     C*
034000000000     C* RICEVIMENTO PARAMETRI
034100000000     C     *ENTRY        PLIST
034200000000     C                   PARM                    KPJBA
034300020917     C*
034400020917     C* LANCIO ROUTINE PER VERIFICARE SE
034500020917     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
034600020917     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
034700020917     C                   EXSR      REPDATIUTE
034800000000     C*
034900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
035000000000     C     XMSGPA        PLIST
035100000000     C                   PARM                    DSMSMS
035200000000     C                   PARM                    DSMSPG
035300000000     C                   PARM                    DSMSKE
035400000000     C                   PARM                    DSMSER
035500000000     C*
035600000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
035700000000     C                   CALL      'XKEY'
035800000000     C                   PARM                    DSKEY
035900000000     C*
036000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
036100000225     C     *LIKE         DEFINE    FUDFUN        FUNKEY
036200000000     C*
036300000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
036400000000     C*   -NOME PGM CHIAMANTE
036500000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
036600000225     C                   MOVEL     'TIST27R'     DSMSPG
036700000225     C                   MOVEL     'TIST27R'     MSGQUE
036800000000     C                   MOVEL     *BLANKS       MSGKEY
036900000000     C                   MOVEL     *BLANKS       DSMSMS
037000000000     C*
037100000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
037200000000     C                   Z-ADD     0             FUNGRU            5 0
037300000000     C*
037400000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
037500000000     C                   EXSR      FUNGES
037600000000     C*
037700000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
037800000000     C                   MOVEL     '0'           F12L00            1
037900000000     C                   MOVEL     '0'           F03L00            1
038000000000     C                   MOVEL     '0'           FINL00            1
038100000000     C                   MOVEL     '0'           ERRL00            1
038200000000     C                   MOVEL     '0'           FLFINE            1
038300000000     C*
038400000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
038500000000     C                   EXSR      AZZVID
038600000000     C*
038700000000     C                   ENDSR
038800000000     C*------------------------------------------------------------------------*
038900000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
039000000000     C*------------------------------------------------------------------------*
039100000000     C     FUNGES        BEGSR
039200000000     C*
039300000000     C* PULISCO CAMPI DI LAVORO
039400000000     C                   Z-ADD     0             W                 5 0
039500000000     C*
039600000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
039700000000     C* E CARICO TABELLA FUNZIONI ATTIVE
039800000000     C     1             DO        24            I
039900000000     C                   MOVEL     FUD(I)        FUDRIG
040000000000     C*
040100000000     C* PULIZIA FUNZIONI PRECEDENTI
040200000000     C                   MOVEL     *BLANKS       FUV(I)
040300000000     C*
040400000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
040500000000     C* (SALTA F24)
040600000000     C     FUDATT        IFEQ      'S'
040700000000     C     FUDFUN        ANDNE     '24'
040800000000     C                   ADD       1             W
040900000000     C                   MOVEL     FUDDES        FUV(W)
041000000000     C                   END
041100000000     C                   END                                                    DO WHILE
041200000000     C*
041300000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
041400000000     C* ALTRIMENTI DISATTIVO F24
041500000000     C     1             DO        24            I
041600000000     C                   MOVEL     FUD(I)        FUDRIG
041700000000     C*
041800000000     C     FUDFUN        IFEQ      '24'
041900000000     C     W             IFGT      6
042000000000     C                   MOVEL     'S'           FUDATT
042100000000     C                   ELSE
042200000000     C                   MOVEL     *BLANKS       FUDATT
042300000000     C                   END                                                    W GT 8
042400000000     C                   MOVEL     FUDRIG        FUD(I)
042500000000     C                   END
042600000000     C                   END                                                    DO WHILE
042700000000     C*
042800000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
042900000000     C     W             DIV       6             I
043000000000     C                   MVR                     Z
043100000000     C*
043200000000     C     Z             IFNE      0
043300000000     C     I             ADD       1             Z
043400000000     C                   ELSE
043500000000     C                   Z-ADD     I             Z                 5 0
043600000000     C                   END
043700000000     C*
043800000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
043900000000     C                   ADD       1             FUNGRU
044000000000     C     FUNGRU        IFGT      Z
044100000000     C     FUNGRU        ORLT      1
044200000000     C                   Z-ADD     1             FUNGRU
044300000000     C                   END
044400000000     C*
044500000000     C* PRIMO GRUPPO DI 6 FUNZIONI
044600000000     C     FUNGRU        IFEQ      1
044700000000     C                   MOVEL     FUV(1)        FUN001
044800000000     C                   MOVEL     FUV(2)        FUN002
044900000000     C                   MOVEL     FUV(3)        FUN003
045000000000     C                   MOVEL     FUV(4)        FUN004
045100000000     C                   MOVEL     FUV(5)        FUN005
045200000000     C                   MOVEL     FUV(6)        FUN006
045300000000     C                   END
045400000000     C*
045500000000     C* SECONDO GRUPPO DI 6 FUNZIONI
045600000000     C     FUNGRU        IFEQ      2
045700000000     C                   MOVEL     FUV(7)        FUN001
045800000000     C                   MOVEL     FUV(8)        FUN002
045900000000     C                   MOVEL     FUV(9)        FUN003
046000000000     C                   MOVEL     FUV(10)       FUN004
046100000000     C                   MOVEL     FUV(11)       FUN005
046200000000     C                   MOVEL     FUV(12)       FUN006
046300000000     C                   END
046400000000     C*
046500000000     C* TERZO GRUPPO DI 6 FUNZIONI
046600000000     C     FUNGRU        IFEQ      3
046700000000     C                   MOVEL     FUV(13)       FUN001
046800000000     C                   MOVEL     FUV(14)       FUN002
046900000000     C                   MOVEL     FUV(15)       FUN003
047000000000     C                   MOVEL     FUV(16)       FUN004
047100000000     C                   MOVEL     FUV(17)       FUN005
047200000000     C                   MOVEL     FUV(18)       FUN006
047300000000     C                   END
047400000000     C*
047500000000     C* QUARTO GRUPPO DI 6 FUNZIONI
047600000000     C     FUNGRU        IFEQ      4
047700000000     C                   MOVEL     FUV(19)       FUN001
047800000000     C                   MOVEL     FUV(20)       FUN002
047900000000     C                   MOVEL     FUV(21)       FUN003
048000000000     C                   MOVEL     FUV(22)       FUN004
048100000000     C                   MOVEL     *BLANKS       FUN005
048200000000     C                   MOVEL     *BLANKS       FUN006
048300000000     C                   END
048400000000     C*
048500000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
048600000000     C                   MOVEL     *BLANKS       FUN007
048700000000     C                   MOVEL     *BLANKS       FUN008
048800000000     C*
048900000000     C     1             DO        24            I
049000000000     C                   MOVEL     FUD(I)        FUDRIG
049100000000     C     FUDFUN        IFEQ      '24'
049200000000     C     FUDATT        ANDEQ     'S'
049300000000     C                   MOVEL     FUDDES        FUN008
049400000000     C                   END
049500000000     C                   END                                                    DO
049600000000     C                   ENDSR
049700000000     C*------------------------------------------------------------------------*
049800000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
049900000000     C*------------------------------------------------------------------------*
050000000000     C     FUNCON        BEGSR
050100000000     C*
050200000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
050300000000     C                   EXSR      FUNVAR
050400000000     C*
050500000000     C* PULISCO CAMPI DI LAVORO
050600000000     C                   Z-ADD     0             I                 5 0
050700000000     C                   Z-ADD     0             W                 5 0
050800000000     C*
050900000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
051000000000     C     FUNKEY        IFNE      *BLANKS
051100000000     C     1             DO        24            I
051200000000     C                   MOVEL     FUD(I)        FUDRIG
051300000000     C*
051400000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
051500000000     C     FUNKEY        IFEQ      FUDFUN
051600000000     C     FUDATT        ANDEQ     'S'
051700000000     C                   Z-ADD     I             W
051800000000     C                   END
051900000000     C                   END                                                    DO WHILE
052000000000     C*
052100000000     C* FUNZIONE NON ATTIVA (ERRORE)
052200000000     C     W             IFEQ      0
052300000000     C*
052400000000     C* EMISSIONE MESSAGGIO DI ERRORE
052500000000     C                   MOVEL     MSG(1)        DSMSMS
052600000000     C*
052700000000     C                   END                                                    W=0
052800000000     C                   END                                                    FUNKEY NE *BLANKS
052900000000     C*
053000000000     C* FUNZIONE ATTIVA (PROCEDI ...)
053100000000     C     W             IFNE      0
053200000000     C     FUNKEY        OREQ      *BLANKS
053300000000     C*
053400000000     C     FUNKEY        IFEQ      *BLANKS
053500000000     C                   EXSR      ENTGES
053600000000     C                   END
053700000000     C     FUNKEY        IFEQ      '01'
053800000000     C                   EXSR      F01GES
053900000000     C                   END
054000000000     C     FUNKEY        IFEQ      '03'
054100000000     C                   EXSR      F03GES
054200000000     C                   END
054300000000     C     FUNKEY        IFEQ      '06'
054400000000     C                   EXSR      F06GES
054500000000     C                   END
054600000000     C     FUNKEY        IFEQ      '07'
054700000000     C                   EXSR      F07GES
054800000000     C                   END
054900000000     C     FUNKEY        IFEQ      '08'
055000000000     C                   EXSR      F08GES
055100000000     C                   END
055200000000     C     FUNKEY        IFEQ      '12'
055300000000     C                   EXSR      F12GES
055400000000     C                   END
055500000000     C     FUNKEY        IFEQ      '21'
055600000000     C                   EXSR      F21GES
055700000000     C                   END
055800000000     C     FUNKEY        IFEQ      '22'
055900000000     C                   EXSR      F22GES
056000000000     C                   END
056100000000     C     FUNKEY        IFEQ      '24'
056200000000     C                   EXSR      F24GES
056300000000     C                   END
056400000000     C     FUNKEY        IFEQ      'HP'
056500000000     C                   EXSR      HELGES
056600000000     C                   END
056700000000     C*
056800000000     C                   END
056900000000     C*
057000000000     C                   ENDSR
057100000000     C*------------------------------------------------------------------------*
057200000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
057300000000     C*------------------------------------------------------------------------*
057400000000     C     FUNVAR        BEGSR
057500000000     C*
057600000000     C* INIZIALIZZA VARIABILE -FUNKEY-
057700000000     C*
057800000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
057900000000     C     KEY           IFEQ      ENTER
058000000000     C                   MOVEL     *BLANKS       FUNKEY
058100000000     C                   END
058200000000     C     KEY           IFEQ      F1
058300000000     C                   MOVEL     '01'          FUNKEY
058400000000     C                   END
058500000000     C     KEY           IFEQ      F2
058600000000     C                   MOVEL     '02'          FUNKEY
058700000000     C                   END
058800000000     C     KEY           IFEQ      F3
058900000000     C                   MOVEL     '03'          FUNKEY
059000000000     C                   END
059100000000     C     KEY           IFEQ      F4
059200000000     C                   MOVEL     '04'          FUNKEY
059300000000     C                   END
059400000000     C     KEY           IFEQ      F5
059500000000     C                   MOVEL     '05'          FUNKEY
059600000000     C                   END
059700000000     C     KEY           IFEQ      F6
059800000000     C                   MOVEL     '06'          FUNKEY
059900000000     C                   END
060000000000     C     KEY           IFEQ      F7
060100000000     C                   MOVEL     '07'          FUNKEY
060200000000     C                   END
060300000000     C     KEY           IFEQ      F8
060400000000     C                   MOVEL     '08'          FUNKEY
060500000000     C                   END
060600000000     C     KEY           IFEQ      F9
060700000000     C                   MOVEL     '09'          FUNKEY
060800000000     C                   END
060900000000     C     KEY           IFEQ      F10
061000000000     C                   MOVEL     '10'          FUNKEY
061100000000     C                   END
061200000000     C     KEY           IFEQ      F11
061300000000     C                   MOVEL     '11'          FUNKEY
061400000000     C                   END
061500000000     C     KEY           IFEQ      F12
061600000000     C                   MOVEL     '12'          FUNKEY
061700000000     C                   END
061800000000     C     KEY           IFEQ      F13
061900000000     C                   MOVEL     '13'          FUNKEY
062000000000     C                   END
062100000000     C     KEY           IFEQ      F14
062200000000     C                   MOVEL     '14'          FUNKEY
062300000000     C                   END
062400000000     C     KEY           IFEQ      F15
062500000000     C                   MOVEL     '15'          FUNKEY
062600000000     C                   END
062700000000     C     KEY           IFEQ      F16
062800000000     C                   MOVEL     '16'          FUNKEY
062900000000     C                   END
063000000000     C     KEY           IFEQ      F17
063100000000     C                   MOVEL     '17'          FUNKEY
063200000000     C                   END
063300000000     C     KEY           IFEQ      F18
063400000000     C                   MOVEL     '18'          FUNKEY
063500000000     C                   END
063600000000     C     KEY           IFEQ      F19
063700000000     C                   MOVEL     '19'          FUNKEY
063800000000     C                   END
063900000000     C     KEY           IFEQ      F20
064000000000     C                   MOVEL     '20'          FUNKEY
064100000000     C                   END
064200000000     C     KEY           IFEQ      F21
064300000000     C                   MOVEL     '21'          FUNKEY
064400000000     C                   END
064500000000     C     KEY           IFEQ      F22
064600000000     C                   MOVEL     '22'          FUNKEY
064700000000     C                   END
064800000000     C     KEY           IFEQ      F23
064900000000     C                   MOVEL     '23'          FUNKEY
065000000000     C                   END
065100000000     C     KEY           IFEQ      F24
065200000000     C                   MOVEL     '24'          FUNKEY
065300000000     C                   END
065400000000     C     KEY           IFEQ      ROLLUP
065500000000     C                   MOVEL     'RU'          FUNKEY
065600000000     C                   END
065700000000     C     KEY           IFEQ      HELP
065800000000     C                   MOVEL     'HP'          FUNKEY
065900000000     C                   END
066000000000     C                   ENDSR
066100000000     C*------------------------------------------------------------------------*
066200000000     C* NOTE PER COMPILAZIONE SCHIERE
066300000000     C*------------------------------------------------------------------------*
066400000000     C* -MSG-
066500000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
066600000000     C* (NON TOCCARE MSG,1)
066700000000     C*
066800000000     C* -FUD-
066900000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
067000000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067100000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067200000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
067300000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067400000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
067500000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
067600000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
067700000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
067800000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
067900000000     C*
068000000000     C*------------------------------------------------------------------------*
068100000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
068200000225TIST27R- Tasto funzionale non gestito.                                         1
068300000225TIST27R-                                                                       2
068400000225TIST27R- Data non accettabile.                                                 3
068500000225TIST27R-                                                                       4
068600000225TIST27R-                                                                       5
068700000225TIST27R- Data inizio maggiore data fine.                                       6
068800000225TIST27R-                                                                       7
068900000225TIST27R- Data obbligatoria.                                                    8
069000000225TIST27R-                                                                       9
069100000225TIST27R-                                                                      10
069200000225TIST27R-                                                                      11
069300000225TIST27R-                                                                      12
069400000225TIST27R-                                                                      13
069500000225TIST27R-                                                                      14
069600000225TIST27R-                                                                      15
069700000225TIST27R- Data non valida.                                                     16
069800000225TIST27R-                                                                      17
069900000225TIST27R-                                                                      18
070000000225TIST27R-                                                                      19
070100000225TIST27R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
070200000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
070300000000                   -        (S/N)=N                        funz.01¬
070400000000                   -        (S/N)=N                        funz.02¬
070500000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
070600000000                   -        (S/N)=N                        funz.04¬
070700000000                   -        (S/N)=N                        funz.05¬
070800000000F06=Lancio         -        (S/N)=S                        funz.06¬
070900000000                   -        (S/N)=N                        funz.07¬
071000000000                   -        (S/N)=N                        funz.08¬
071100000000                   -        (S/N)=N                        funz.09¬
071200000000                   -        (S/N)=N                        funz.10¬
071300000000                   -        (S/N)=N                        funz.11¬
071400000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
071500000000                   -        (S/N)=N                        funz.13¬
071600000000                   -        (S/N)=N                        funz.14¬
071700000000                   -        (S/N)=N                        funz.15¬
071800000000                   -        (S/N)=N                        funz.16¬
071900000000                   -        (S/N)=N                        funz.17¬
072000000000                   -        (S/N)=N                        funz.18¬
072100000000                   -        (S/N)=N                        funz.19¬
072200000000                   -        (S/N)=N                        funz.20¬
072300000000                   -        (S/N)=N                        funz.21¬
072400000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
072500000000                   -        (S/N)=N                        funz.23¬
072600000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
