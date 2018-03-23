000100000000     H*------------------------------------------------------------------------*
000200000225     H* STATISTICA DESTINATARI - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000301     FTIST29D   CF   E             WORKSTN INFDS(DEVDS1)
000900000301     F                                     SFILE(ST29DMS:MSGULT)
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
002800000000     D*------------------------------------------------------------------------*
002900000000     D* INPUT
003000000000     D*------------------------------------------------------------------------*
003100000000     D*-------------------
003200000000     D* RIDEFINIZIONE SCHIERA -FUD-
003300000000     D*-------------------
003400000000     D FUDDS           DS
003500000000     D*                -DESCRIZIONE PARZIALE
003600000000     D  FUDDE1                 1      1
003700000000     D*                -FUNZIONE
003800000000     D  FUDFUN                 2      3
003900000000     D*                -DESCRIZIONE PARZIALE
004000000000     D  FUDDE2                 4     19
004100000000     D*                -FILLER
004200000000     D  FUDFI0                20     34
004300000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004400000000     D  FUDATT                35     35
004500000000     D*                -FILLER
004600000000     D  FUDFI1                36     80
004700000000     D*                -DESCRIZIONE COMPLETA
004800000000     D  FUDDES                 1     19
004900000000     D*                -RIDEFINIZIONE RIGA
005000000000     D  FUDRIG                 1     80
005100000000     D*-------------------
005200000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005300000000     D*-------------------
005400000000     D DSMSG         E DS
005500000301     D*------------------
005600000301     D* DS - X CONTROLLO E X INVERSIONE VELOCE DATA (8 NUM)
005700000301     D*------------------
005800000301     D WLBDA8          DS                  INZ
005900000301     D  G08DAT                 1      8  0
006000000301     D  G08INV                 9     16  0
006100000301     D  G08ERR                17     17
006200000301     D  G08TGI                18     22  0
006300020917     D*-------------------
006400020917     D* REPERIMENTO DATI UTENTE
006500020917     D*-------------------
006600020917     D TIBS34DS      E DS                                                       *Profili utente
006700020917     D DDATIUTE      E DS                                                       *Dati utente
006800020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900970401     D*----------------
007000000000     D* DS PARAMETRI DI LANCIO PGM
007100000000     D*-------------------
007200950703     D PARAM           DS
007300000301     D  PARDAI                 1      6  0
007400000301     D  PARDAF                 7     12  0
007500000613     D  PARPT                 13     13
007600000000     D*-------------------
007700000000     D* DS PER GESTIONE TASTI FUNZIONALI
007800000000     D*-------------------
007900000000     D DSKEY         E DS
008000000000     D DEVDS1          DS
008100000000     D  KEY                  369    369
008200000000     D*-------------------
008300000000     D* ARCHITETTURA
008400000000     D*-------------------
008500000000     D KPJBA         E DS
008600991117     D*-------------------
008700000000     C*------------------------------------------------------------------------*
008800000000     C* MAIN LINE
008900000000     C*------------------------------------------------------------------------*
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
011100991118     C*
011200000000     C                   SETON                                        LR
011300000000     C*------------------------------------------------------------------------*
011400000000     C* EMISSIONE FORMATO A VIDEO
011500000000     C*------------------------------------------------------------------------*
011600000000     C     BUTFMR        BEGSR
011700000000     C*
011800000000     C* WRITE RECORD TESTATA
011900000301     C                   WRITE     ST29D01
012000000000     C*
012100000000     C* VISUALIZZAZIONE MESSAGGIO
012200000000     C                   EXSR      SNDMSG
012300000000     C*
012400000000     C* WRITE RECORD FUNZIONI
012500000301     C                   WRITE     ST29D02
012600991019     C*
012700991019     C* EXFMT RECORD DETTAGLIO
012800000301     C                   EXFMT     ST29D03
012900991019     C*
013000000000     C* AZZERO FLAG DI ERRORE LIVELLO
013100000000     C                   MOVEL     '0'           ERRL00
013200000000     C*
013300000000     C* AZZERO FLAG DI ERRORE
013400000613     C                   SETOFF                                       707172
013500000000     C*
013600000000     C                   ENDSR
013700000000     C*------------------------------------------------------------------------*
013800000000     C* SNDMSG - MANDA MESSAGGIO
013900000000     C*------------------------------------------------------------------------*
014000000000     C     SNDMSG        BEGSR
014100000000     C*
014200000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014300000000     C                   MOVEL     '1'           *IN40
014400000301     C                   WRITE     ST29DMC
014500000000     C                   MOVEL     '0'           *IN40
014600000000     C*
014700000000     C* VISUALIZZAZIONE MESSAGGIO
014800000000     C                   Z-ADD     0             MSGULT            4 0
014900000000     C                   CALL      'XMSG'        XMSGPA
015000000000     C     DSMSER        IFEQ      0
015100000000     C                   MOVEL     DSMSKE        MSGKEY
015200000000     C                   END
015300000000     C                   Z-ADD     1             MSGULT
015400000301     C                   WRITE     ST29DMS                                      SFL MESS
015500000301     C                   WRITE     ST29DMC                                      CONTR MESS
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
016700000000     C                   EXSR      DECODI
016800000000     C*
016900000000     C                   ENDSR
017000000000     C*------------------------------------------------------------------------*
017100000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
017200000000     C*------------------------------------------------------------------------*
017300000000     C     F01GES        BEGSR
017400000000     C                   ENDSR
017500000000     C*------------------------------------------------------------------------*
017600000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017700000000     C*------------------------------------------------------------------------*
017800000000     C     F03GES        BEGSR
017900000000     C*
018000000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
018100000000     C                   MOVEL     '1'           F03L00
018200000000     C*
018300000000     C                   ENDSR
018400000000     C*------------------------------------------------------------------------*
018500000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018600000000     C*------------------------------------------------------------------------*
018700991020     C     F06GES        BEGSR
018800000000     C*
018900991105     C                   EXSR      DECODI
019000991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
019100991105     C     ERRL00        IFEQ      '0'
019200991105     C*
019300000000     C* LANCIO IL PROGRAMMA DI STAMPA
019400000613     C                   MOVEL     VIDPT         PARPT
019500000301     C                   MOVEL     'ST30'        KCOAZ
019600950703     C                   MOVEL     PARAM         KPJBU
019700000303     C                   CALL      'BCH10'
019800000303 MOM C*                  CALL      'TIST30R'
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
025300000301     C                   MOVE      VIDDAI        $DAT              8 0
025400000301     C                   MOVEL     '01'          $DAT
025500000225     C                   EXSR      CHKDAT
025600000301     C     G08ERR        IFEQ      '1'
025700000225     C                   SETON                                        70
025800000225     C                   MOVEL     MSG(16)       DSMSMS
025900000301     C                   MOVEL     '1'           ERRL00
026000000225     C                   ELSE
026100000301     C                   MOVEL     G08INV        PARDAI
026200000225     C                   ENDIF
026300000225     C*
026400000301     C* CONTROLLO DATA FINALE
026500000301     C                   MOVE      VIDDAF        $DAT              8 0
026600000301     C                   MOVEL     '01'          $DAT
026700000225     C                   EXSR      CHKDAT
026800000301     C     G08ERR        IFEQ      '1'
026900000225     C                   SETON                                        71
027000000225     C                   MOVEL     MSG(16)       DSMSMS
027100000301     C                   MOVEL     '1'           ERRL00
027200000225     C                   ELSE
027300000301     C                   MOVEL     G08INV        PARDAF
027400000225     C                   ENDIF
027500000225     C*
027600000225     C* CONTROLLO DATA FINALE > DATA INIZIALE
027700000225     C     ERRL00        IFEQ      '0'
027800000301     C     PARDAI        IFGT      PARDAF
027900000225     C                   SETON                                        7071
028000000225     C                   MOVEL     '1'           ERRL00
028100000225     C                   MOVEL     MSG(6)        DSMSMS
028200000225     C                   ENDIF
028300000225     C                   ENDIF
028400020326     C*
028500020326     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'A
028600020326IF  1C     ERRL00        IFEQ      '0'
028700020326     C                   MOVEL     PARDAI        ANNODA            4 0
028800020326     C                   MOVEL     PARDAF        ANNOA             4 0
028900020326IF  2C     ANNODA        IFNE      ANNOA
029000020326     C                   MOVEL     '1'           ERRL00
029100020326     C                   MOVEL     MSG(19)       DSMSMS
029200020326     C                   SETON                                        7071
029300020326E   2C                   ENDIF
029400020326E   1C                   ENDIF
029500000613     C*
029600000613     C* CONTROLLO INSERIMENTO OPZIONE X POSTE
029700000613     C                   IF        VIDPT<>'S' AND VIDPT<>'N'                    *BARTOLINI
029800000613     C                   SETON                                        72
029900000613     C                   MOVEL     '1'           ERRL00
030000000613     C                   MOVEL     MSG(4)        DSMSMS
030100000613     C                   ENDIF
030200000225     C*
030300000000     C                   ENDSR
030400000225     C*------------------------------------------------------------------------*
030500000225     C* CHKDAT - CONTROLLA LA DATA
030600000225     C*          INPUT --> $DAT:  DATA
030700000225     C*------------------------------------------------------------------------*
030800000225     C     CHKDAT        BEGSR
030900000225     C*
031000000225     C* OBBLIGATORIA
031100000225IF  1C     $DAT          IFEQ      *ZEROS
031200000225     C                   MOVEL     '1'           ERRL00
031300000225X   1C                   ELSE
031400000225     C* CORRETTA
031500000301     C                   Z-ADD     $DAT          G08DAT
031600000301     C                   Z-ADD     *ZEROS        G08INV
031700000301     C                   MOVEL     '0'           G08ERR
031800000225     C                   CALL      'XSRDA8'
031900000225     C                   PARM                    WLBDA8
032000000225     C*
032100000225E   1C                   ENDIF
032200000225     C*
032300000225     C                   ENDSR
032400000000     C*------------------------------------------------------------------------*
032500000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
032600000000     C*------------------------------------------------------------------------*
032700000000     C     AZZVID        BEGSR
032800000000     C*
032900000225     C                   Z-ADD     *zeros        VIDDAI
033000000225     C                   Z-ADD     *zeros        VIDDAF
033100000000     C*
033200000000     C                   ENDSR
033300020917     C*--------------------------------------------------------------------------------------------*
033400020917     C* REPERISCE I DATI UTENTE
033500020917     C*--------------------------------------------------------------------------------------------*
033600020917     C     REPDATIUTE    BEGSR
033700020917     C*
033800020917     C* INIZIALIZZA VARIABILI DI WRK
033900020917     C                   CLEAR                   TIBS34DS
034000020917     C                   CLEAR                   AZUTEDS
034100020917     C                   CLEAR                   DDATIUTE
034200020917     C*
034300020917     C     *DTAARA       DEFINE    §azute        azuteds
034400020917     C     *DTAARA       DEFINE    §datiute      ddatiute
034500020917     C                   IN(E)     *DTAARA
034600020917if  1C                   IF        %Error
034700020917     c                   EVAL      I34Tla = 'L'
034800020917     C                   CALL      'TIBS34R'
034900020917     C                   PARM                    Tibs34Ds
035000020917     C                   IN        *DTAARA
035100020917e   1C                   ENDIF
035200020917     C*
035300020917     C                   ENDSR
035400000000     C*------------------------------------------------------------------------*
035500980403     C* *INZSR - OPERAZIONI INIZIALI
035600000000     C*------------------------------------------------------------------------*
035700980403     C     *INZSR        BEGSR
035800000000     C*
035900000000     C* RICEVIMENTO PARAMETRI
036000000000     C     *ENTRY        PLIST
036100000000     C                   PARM                    KPJBA
036200020917     C*
036300020917     C* LANCIO ROUTINE PER VERIFICARE SE
036400020917     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
036500020917     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
036600020917     C                   EXSR      REPDATIUTE
036700000614     C*
036800000614     C* IMPOSTO I DEFAULT DEL VIDEO
036900000614     C                   MOVEL     'N'           VIDPT
037000000000     C*
037100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
037200000000     C     XMSGPA        PLIST
037300000000     C                   PARM                    DSMSMS
037400000000     C                   PARM                    DSMSPG
037500000000     C                   PARM                    DSMSKE
037600000000     C                   PARM                    DSMSER
037700000000     C*
037800000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
037900000000     C                   CALL      'XKEY'
038000000000     C                   PARM                    DSKEY
038100000000     C*
038200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
038300000225     C     *LIKE         DEFINE    FUDFUN        FUNKEY
038400000000     C*
038500000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
038600000000     C*   -NOME PGM CHIAMANTE
038700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
038800000301     C                   MOVEL     'TIST29R'     DSMSPG
038900000301     C                   MOVEL     'TIST29R'     MSGQUE
039000000000     C                   MOVEL     *BLANKS       MSGKEY
039100000000     C                   MOVEL     *BLANKS       DSMSMS
039200000000     C*
039300000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
039400000000     C                   Z-ADD     0             FUNGRU            5 0
039500000000     C*
039600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
039700000000     C                   EXSR      FUNGES
039800000000     C*
039900000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
040000000000     C                   MOVEL     '0'           F12L00            1
040100000000     C                   MOVEL     '0'           F03L00            1
040200000000     C                   MOVEL     '0'           FINL00            1
040300000000     C                   MOVEL     '0'           ERRL00            1
040400000000     C                   MOVEL     '0'           FLFINE            1
040500000000     C*
040600000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
040700000000     C                   EXSR      AZZVID
040800000000     C*
040900000000     C                   ENDSR
041000000000     C*------------------------------------------------------------------------*
041100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
041200000000     C*------------------------------------------------------------------------*
041300000000     C     FUNGES        BEGSR
041400000000     C*
041500000000     C* PULISCO CAMPI DI LAVORO
041600000000     C                   Z-ADD     0             W                 5 0
041700000000     C*
041800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
041900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
042000000000     C     1             DO        24            I
042100000000     C                   MOVEL     FUD(I)        FUDRIG
042200000000     C*
042300000000     C* PULIZIA FUNZIONI PRECEDENTI
042400000000     C                   MOVEL     *BLANKS       FUV(I)
042500000000     C*
042600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
042700000000     C* (SALTA F24)
042800000000     C     FUDATT        IFEQ      'S'
042900000000     C     FUDFUN        ANDNE     '24'
043000000000     C                   ADD       1             W
043100000000     C                   MOVEL     FUDDES        FUV(W)
043200000000     C                   END
043300000000     C                   END                                                    DO WHILE
043400000000     C*
043500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
043600000000     C* ALTRIMENTI DISATTIVO F24
043700000000     C     1             DO        24            I
043800000000     C                   MOVEL     FUD(I)        FUDRIG
043900000000     C*
044000000000     C     FUDFUN        IFEQ      '24'
044100000000     C     W             IFGT      6
044200000000     C                   MOVEL     'S'           FUDATT
044300000000     C                   ELSE
044400000000     C                   MOVEL     *BLANKS       FUDATT
044500000000     C                   END                                                    W GT 8
044600000000     C                   MOVEL     FUDRIG        FUD(I)
044700000000     C                   END
044800000000     C                   END                                                    DO WHILE
044900000000     C*
045000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
045100000000     C     W             DIV       6             I
045200000000     C                   MVR                     Z
045300000000     C*
045400000000     C     Z             IFNE      0
045500000000     C     I             ADD       1             Z
045600000000     C                   ELSE
045700000000     C                   Z-ADD     I             Z                 5 0
045800000000     C                   END
045900000000     C*
046000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
046100000000     C                   ADD       1             FUNGRU
046200000000     C     FUNGRU        IFGT      Z
046300000000     C     FUNGRU        ORLT      1
046400000000     C                   Z-ADD     1             FUNGRU
046500000000     C                   END
046600000000     C*
046700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
046800000000     C     FUNGRU        IFEQ      1
046900000000     C                   MOVEL     FUV(1)        FUN001
047000000000     C                   MOVEL     FUV(2)        FUN002
047100000000     C                   MOVEL     FUV(3)        FUN003
047200000000     C                   MOVEL     FUV(4)        FUN004
047300000000     C                   MOVEL     FUV(5)        FUN005
047400000000     C                   MOVEL     FUV(6)        FUN006
047500000000     C                   END
047600000000     C*
047700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
047800000000     C     FUNGRU        IFEQ      2
047900000000     C                   MOVEL     FUV(7)        FUN001
048000000000     C                   MOVEL     FUV(8)        FUN002
048100000000     C                   MOVEL     FUV(9)        FUN003
048200000000     C                   MOVEL     FUV(10)       FUN004
048300000000     C                   MOVEL     FUV(11)       FUN005
048400000000     C                   MOVEL     FUV(12)       FUN006
048500000000     C                   END
048600000000     C*
048700000000     C* TERZO GRUPPO DI 6 FUNZIONI
048800000000     C     FUNGRU        IFEQ      3
048900000000     C                   MOVEL     FUV(13)       FUN001
049000000000     C                   MOVEL     FUV(14)       FUN002
049100000000     C                   MOVEL     FUV(15)       FUN003
049200000000     C                   MOVEL     FUV(16)       FUN004
049300000000     C                   MOVEL     FUV(17)       FUN005
049400000000     C                   MOVEL     FUV(18)       FUN006
049500000000     C                   END
049600000000     C*
049700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
049800000000     C     FUNGRU        IFEQ      4
049900000000     C                   MOVEL     FUV(19)       FUN001
050000000000     C                   MOVEL     FUV(20)       FUN002
050100000000     C                   MOVEL     FUV(21)       FUN003
050200000000     C                   MOVEL     FUV(22)       FUN004
050300000000     C                   MOVEL     *BLANKS       FUN005
050400000000     C                   MOVEL     *BLANKS       FUN006
050500000000     C                   END
050600000000     C*
050700000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
050800000000     C                   MOVEL     *BLANKS       FUN007
050900000000     C                   MOVEL     *BLANKS       FUN008
051000000000     C*
051100000000     C     1             DO        24            I
051200000000     C                   MOVEL     FUD(I)        FUDRIG
051300000000     C     FUDFUN        IFEQ      '24'
051400000000     C     FUDATT        ANDEQ     'S'
051500000000     C                   MOVEL     FUDDES        FUN008
051600000000     C                   END
051700000000     C                   END                                                    DO
051800000000     C                   ENDSR
051900000000     C*------------------------------------------------------------------------*
052000000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
052100000000     C*------------------------------------------------------------------------*
052200000000     C     FUNCON        BEGSR
052300000000     C*
052400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
052500000000     C                   EXSR      FUNVAR
052600000000     C*
052700000000     C* PULISCO CAMPI DI LAVORO
052800000000     C                   Z-ADD     0             I                 5 0
052900000000     C                   Z-ADD     0             W                 5 0
053000000000     C*
053100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
053200000000     C     FUNKEY        IFNE      *BLANKS
053300000000     C     1             DO        24            I
053400000000     C                   MOVEL     FUD(I)        FUDRIG
053500000000     C*
053600000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
053700000000     C     FUNKEY        IFEQ      FUDFUN
053800000000     C     FUDATT        ANDEQ     'S'
053900000000     C                   Z-ADD     I             W
054000000000     C                   END
054100000000     C                   END                                                    DO WHILE
054200000000     C*
054300000000     C* FUNZIONE NON ATTIVA (ERRORE)
054400000000     C     W             IFEQ      0
054500000000     C*
054600000000     C* EMISSIONE MESSAGGIO DI ERRORE
054700000000     C                   MOVEL     MSG(1)        DSMSMS
054800000000     C*
054900000000     C                   END                                                    W=0
055000000000     C                   END                                                    FUNKEY NE *BLANKS
055100000000     C*
055200000000     C* FUNZIONE ATTIVA (PROCEDI ...)
055300000000     C     W             IFNE      0
055400000000     C     FUNKEY        OREQ      *BLANKS
055500000000     C*
055600000000     C     FUNKEY        IFEQ      *BLANKS
055700000000     C                   EXSR      ENTGES
055800000000     C                   END
055900000000     C     FUNKEY        IFEQ      '01'
056000000000     C                   EXSR      F01GES
056100000000     C                   END
056200000000     C     FUNKEY        IFEQ      '03'
056300000000     C                   EXSR      F03GES
056400000000     C                   END
056500000000     C     FUNKEY        IFEQ      '06'
056600000000     C                   EXSR      F06GES
056700000000     C                   END
056800000000     C     FUNKEY        IFEQ      '07'
056900000000     C                   EXSR      F07GES
057000000000     C                   END
057100000000     C     FUNKEY        IFEQ      '08'
057200000000     C                   EXSR      F08GES
057300000000     C                   END
057400000000     C     FUNKEY        IFEQ      '12'
057500000000     C                   EXSR      F12GES
057600000000     C                   END
057700000000     C     FUNKEY        IFEQ      '21'
057800000000     C                   EXSR      F21GES
057900000000     C                   END
058000000000     C     FUNKEY        IFEQ      '22'
058100000000     C                   EXSR      F22GES
058200000000     C                   END
058300000000     C     FUNKEY        IFEQ      '24'
058400000000     C                   EXSR      F24GES
058500000000     C                   END
058600000000     C     FUNKEY        IFEQ      'HP'
058700000000     C                   EXSR      HELGES
058800000000     C                   END
058900000000     C*
059000000000     C                   END
059100000000     C*
059200000000     C                   ENDSR
059300000000     C*------------------------------------------------------------------------*
059400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
059500000000     C*------------------------------------------------------------------------*
059600000000     C     FUNVAR        BEGSR
059700000000     C*
059800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
059900000000     C*
060000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
060100000000     C     KEY           IFEQ      ENTER
060200000000     C                   MOVEL     *BLANKS       FUNKEY
060300000000     C                   END
060400000000     C     KEY           IFEQ      F1
060500000000     C                   MOVEL     '01'          FUNKEY
060600000000     C                   END
060700000000     C     KEY           IFEQ      F2
060800000000     C                   MOVEL     '02'          FUNKEY
060900000000     C                   END
061000000000     C     KEY           IFEQ      F3
061100000000     C                   MOVEL     '03'          FUNKEY
061200000000     C                   END
061300000000     C     KEY           IFEQ      F4
061400000000     C                   MOVEL     '04'          FUNKEY
061500000000     C                   END
061600000000     C     KEY           IFEQ      F5
061700000000     C                   MOVEL     '05'          FUNKEY
061800000000     C                   END
061900000000     C     KEY           IFEQ      F6
062000000000     C                   MOVEL     '06'          FUNKEY
062100000000     C                   END
062200000000     C     KEY           IFEQ      F7
062300000000     C                   MOVEL     '07'          FUNKEY
062400000000     C                   END
062500000000     C     KEY           IFEQ      F8
062600000000     C                   MOVEL     '08'          FUNKEY
062700000000     C                   END
062800000000     C     KEY           IFEQ      F9
062900000000     C                   MOVEL     '09'          FUNKEY
063000000000     C                   END
063100000000     C     KEY           IFEQ      F10
063200000000     C                   MOVEL     '10'          FUNKEY
063300000000     C                   END
063400000000     C     KEY           IFEQ      F11
063500000000     C                   MOVEL     '11'          FUNKEY
063600000000     C                   END
063700000000     C     KEY           IFEQ      F12
063800000000     C                   MOVEL     '12'          FUNKEY
063900000000     C                   END
064000000000     C     KEY           IFEQ      F13
064100000000     C                   MOVEL     '13'          FUNKEY
064200000000     C                   END
064300000000     C     KEY           IFEQ      F14
064400000000     C                   MOVEL     '14'          FUNKEY
064500000000     C                   END
064600000000     C     KEY           IFEQ      F15
064700000000     C                   MOVEL     '15'          FUNKEY
064800000000     C                   END
064900000000     C     KEY           IFEQ      F16
065000000000     C                   MOVEL     '16'          FUNKEY
065100000000     C                   END
065200000000     C     KEY           IFEQ      F17
065300000000     C                   MOVEL     '17'          FUNKEY
065400000000     C                   END
065500000000     C     KEY           IFEQ      F18
065600000000     C                   MOVEL     '18'          FUNKEY
065700000000     C                   END
065800000000     C     KEY           IFEQ      F19
065900000000     C                   MOVEL     '19'          FUNKEY
066000000000     C                   END
066100000000     C     KEY           IFEQ      F20
066200000000     C                   MOVEL     '20'          FUNKEY
066300000000     C                   END
066400000000     C     KEY           IFEQ      F21
066500000000     C                   MOVEL     '21'          FUNKEY
066600000000     C                   END
066700000000     C     KEY           IFEQ      F22
066800000000     C                   MOVEL     '22'          FUNKEY
066900000000     C                   END
067000000000     C     KEY           IFEQ      F23
067100000000     C                   MOVEL     '23'          FUNKEY
067200000000     C                   END
067300000000     C     KEY           IFEQ      F24
067400000000     C                   MOVEL     '24'          FUNKEY
067500000000     C                   END
067600000000     C     KEY           IFEQ      ROLLUP
067700000000     C                   MOVEL     'RU'          FUNKEY
067800000000     C                   END
067900000000     C     KEY           IFEQ      HELP
068000000000     C                   MOVEL     'HP'          FUNKEY
068100000000     C                   END
068200000000     C                   ENDSR
068300000000     C*------------------------------------------------------------------------*
068400000000     C* NOTE PER COMPILAZIONE SCHIERE
068500000000     C*------------------------------------------------------------------------*
068600000000     C* -MSG-
068700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
068800000000     C* (NON TOCCARE MSG,1)
068900000000     C*
069000000000     C* -FUD-
069100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
069200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
069300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
069400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
069500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
069700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
069800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
069900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
070000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
070100000000     C*
070200000000     C*------------------------------------------------------------------------*
070300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
070400000301TIST29R- Tasto funzionale non gestito.                                         1
070500000301TIST29R-                                                                       2
070600000301TIST29R- Data non accettabile.                                                 3
070700000613TIST29R- Opzione obbligatoria.                                                 4
070800000301TIST29R-                                                                       5
070900000301TIST29R- Data inizio maggiore data fine.                                       6
071000000301TIST29R-                                                                       7
071100000301TIST29R- Data obbligatoria.                                                    8
071200000301TIST29R-                                                                       9
071300000301TIST29R-                                                                      10
071400000301TIST29R-                                                                      11
071500000301TIST29R-                                                                      12
071600000301TIST29R-                                                                      13
071700000301TIST29R-                                                                      14
071800000301TIST29R-                                                                      15
071900000301TIST29R- Data non valida.                                                     16
072000000301TIST29R-                                                                      17
072100000301TIST29R-                                                                      18
072200020326TIST29R- Impossibile lanciare la statistica su anni differenti                19
072300000301TIST29R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
072400000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
072500000000                   -        (S/N)=N                        funz.01¬
072600000000                   -        (S/N)=N                        funz.02¬
072700000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
072800000000                   -        (S/N)=N                        funz.04¬
072900000000                   -        (S/N)=N                        funz.05¬
073000000000F06=Lancio         -        (S/N)=S                        funz.06¬
073100000000                   -        (S/N)=N                        funz.07¬
073200000000                   -        (S/N)=N                        funz.08¬
073300000000                   -        (S/N)=N                        funz.09¬
073400000000                   -        (S/N)=N                        funz.10¬
073500000000                   -        (S/N)=N                        funz.11¬
073600000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
073700000000                   -        (S/N)=N                        funz.13¬
073800000000                   -        (S/N)=N                        funz.14¬
073900000000                   -        (S/N)=N                        funz.15¬
074000000000                   -        (S/N)=N                        funz.16¬
074100000000                   -        (S/N)=N                        funz.17¬
074200000000                   -        (S/N)=N                        funz.18¬
074300000000                   -        (S/N)=N                        funz.19¬
074400000000                   -        (S/N)=N                        funz.20¬
074500000000                   -        (S/N)=N                        funz.21¬
074600000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
074700000000                   -        (S/N)=N                        funz.23¬
074800000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
