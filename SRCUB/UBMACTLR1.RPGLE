000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800141216     FUBMACTLD1 cf   e             WORKSTN INFDS(DEVDS1)
000900141216     F                                     SFILE(MACTLDMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D* MESSAGGI
001400970526     D*------------
001500000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* DESCRIZIONI TASTI FUNZIONALI
001800970526     D*------------
001900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* FUNZIONI DA CARICARE A VIDEO
002200970526     D*------------
002300970526     D FUV             S             19    DIM(24)
002400970526     D*------------------------------------------------------------------------*
002500970526     D* INPUT
002600970526     D*------------------------------------------------------------------------*
002700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002800970526     D*------------
002900970526     D FUDDS           DS
003000970526     D  FUDDE1                 1      1
003100970526     D  FUDFUN                 2      3
003200970526     D  FUDDE2                 4     19
003300970526     D  FUDFI0                20     34
003400970526     D  FUDATT                35     35
003500970526     D  FUDFI1                36     80
003600970526     D  FUDDES                 1     19
003700970526     D  FUDRIG                 1     80
003800970526     D*------------
003900970526     D* DS GESTIONE TASTI FUNZIONALI
004000970526     D*------------
004100970526     D DSKEY         E DS
004200970526     D DEVDS1          DS
004300970526     D  KEY                  369    369
004400970526     D*------------
004500970526     D* DS "XMSG" - GESTIONE MESSAGGI
004600970526     D*------------
004700970526     D DSMSG         E DS
004800970528     D*------------------
004900970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000970528     D*------------------
005100970528     D WLBDA8          DS                  INZ
005200970528     D  G08DAT                 1      8  0
005300970528     D  G08INV                 9     16  0
005400970528     D  G08ERR                17     17
005500970528     D  G08TGI                18     22  0
005600970526     D*------------
005700970526     D* ARCHITETTURA
005800970526     D*------------
005900970526     D KPJBA         E DS
006000020923     D*-------------------
006100020923     D* DS REPERIMENTO DATI UTENTE
006200020923     D*-------------------
006300020923     D TIBS34DS      E DS                                                       *Profili utente
006400020923     D DDATIUTE      E DS                                                       *Dati utente
006500020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006600120927     D* DS REPERIMENTO PADRE/FIGLI
006700120927     D TIBS10DS      E DS
006800141027     D WLBDAT          DS
006900141027     D  G02DAT                 1      8  0
007000141027     D  G02INV                 9     16  0
007100141027     D  G02ERR                17     17
007200141027     D  G02TGI                18     22  0
007300141027     D wDataDa         s              8s 0
007400141027     D wDataA          s              8s 0
007500141028     D*-------------------
007600141216     D* parametri UBMACTLR
007700141028     D*-------------------
007800141216     D UBMACTLDS       DS                  inz
007900141217     D  NrGg                          3s 0
008000141218     D  IndEMail                    126a
008100120928     D*-------------------
008200120928
008300121003     d Status         sds
008400121003     d  SDSpgm           *proc
008500121003     d  SDSprm           *parms
008600121003     d  SDSuser              254    263
008700130116
008800130116     d ora             s               t
008900140117     d wX              s              2s 0
009000141217     d $Debug          s              1a
009100120928
009200970526     C*------------------------------------------------------------------------*
009300000303     C* MAIN LINES
009400970526     C*------------------------------------------------------------------------*
009500970526     C*
009600120927     C* metto i default su D03
009700120927     C                   EXSR      InzD03
009800120927     C*
009900120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010000991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010300970526     C*
010400970526     C* EMETTE FORMATO VIDEO
010500970526     C                   EXSR      BUTFMR
010600970526     C*
010700970526     C* GESTIONE TASTI FUNZIONALI
010800970526     C                   EXSR      FUNCON
010900970526E   2C                   ENDDO
011000970526     C*
011100970526     C* FINE PGM
011200970526     C                   SETON                                        LR
011300970526     C*------------------------------------------------------------------------*
011400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011500970526     C*------------------------------------------------------------------------*
011600970526     C     BUTFMR        BEGSR
011700970526     C*
011800970526     C* EMETTE LA TESTATA
011900141216     C                   WRITE     MACTLD01
012000970526     C*
012100970526     C* SPEDISCE EVENTUALE MESSAGGIO
012200970526     C                   EXSR      SNDMSG
012300970526     C*
012400970526     C* EMETTE LE FUNZIONI
012500141216     C                   WRITE     MACTLD02
012600970526     C*
012700970526     C* EMETTE IL VIDEO
012800141216     C                   EXFMT     MACTLD03
012900970526     C*
013000991214     C* REIMPOSTA FLAG DI ERRORE
013100991214     C                   MOVEL     '0'           ERRL00
013200970526     C*
013300970526     C* REIMPOSTA INDICATORI DI ERRORE
013400020611     C                   SETOFF                                       555556
013500970526     C*
013600970526     C                   ENDSR
013700970526     C*------------------------------------------------------------------------*
013800120927     C* InzD03 - Inizializzo D03
013900970526     C*------------------------------------------------------------------------*
014000120927     C     InzD03        BEGSR
014100120927     C*
014200141216     C                   EVAL      D03NrGG = 2
014300130108     C*
014400120927     C                   ENDSR
014500120927     C*------------------------------------------------------------------------*
014600120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014700120927     C*------------------------------------------------------------------------*
014800120927     C     SNDMSG        BEGSR
014900120927     C*
015000970526     C* PULISCE IL SFILE MESSAGGI
015100970526     C                   MOVEL     '1'           *IN40
015200141216     C                   WRITE     MACTLDMC
015300970526     C                   MOVEL     '0'           *IN40
015400970526     C*
015500970526     C* VISUALIZZA IL MESSAGGIO
015600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015700970526     C                   CALL      'XMSG'        XMSGPA
015800020611     C     DSMSER        IFEQ      *ZEROS
015900970526     C                   MOVEL     DSMSKE        MSGKEY
016000020611     C                   ENDIF
016100970526     C                   Z-ADD     1             MSGULT
016200141216     C                   WRITE     MACTLDMS
016300141216     C                   WRITE     MACTLDMC
016400970526     C*
016500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016600970526     C                   MOVEL     *BLANKS       DSMSMS
016700970526     C*
016800970526     C                   ENDSR
016900970526     C*------------------------------------------------------------------------*
017000970526     C* ENTGES - GESTIONE ENTER -> INVIO
017100970526     C*------------------------------------------------------------------------*
017200970526     C     ENTGES        BEGSR
017300970526     C*
017400970526     C* CONTROLLA I CAMPI IMMESSI
017500970526     C                   EXSR      DECODI
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900970526     C* F03GES - GESTIONE F03 -> FINE
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F03GES        BEGSR
018200970526     C*
018300970526     C* IMPOSTA "PREMUTO F03"
018400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018500970526     C*
018600970526     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800991214     C* F06GES - GESTIONE F06 -> CONFERMA
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F06GES        BEGSR
019100970526     C*
019200991214     C* CONTROLLA I CAMPI IMMESSI
019300991214     C                   EXSR      DECODI
019400020611     C*
019500991214     C* ESEGUE ROUTINE DI CONFERMA
019600020611     C     ERRL00        IFEQ      '0'
019700991214     C                   EXSR      ESEGUI
019800130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
019900121109     C                   MOVEL     MSG(20)       DSMSMS
020000121109     C* non esco mai dal pgm
020100121109     C***                MOVEL     '1'           FINL00
020200121108     C                   ENDIF
020300970526     C*
020400991214     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600130109     C* F07GES - GESTIONE F07 -> STAMPA
020700970526     C*------------------------------------------------------------------------*
020800130109     C     F07GES        BEGSR
020900970526     C*
021000140818     C                   EXSR      F06GES
021100130110     C*
021200970526     C                   ENDSR
021300130109     C*------------------------------------------------------------------------*
021400130109     C* F12GES - GESTIONE F12 -> RITORNO
021500130109     C*------------------------------------------------------------------------*
021600130109     C     F12GES        BEGSR
021700130109     C*
021800130109     C* IMPOSTA "PREMUTO F12"
021900130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022000130109     C*
022100130109     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022400970526     C*------------------------------------------------------------------------*
022500970526     C     F24GES        BEGSR
022600970526     C*
022700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022800970526     C*
022900970526     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
023200970526     C*------------------------------------------------------------------------*
023300991214     C     ESEGUI        BEGSR
023400141027     C*
023500141216     C* da questo pgm si passa un indirizzo email perché il risultato arriva via email
023600141216     C                   RESET                   UBMACTLDS
023700141217     C                   EVAL      IndEMail = D03Dest
023800141217     C                   EVAL      NrGg     = D03NrGg
023900141217     C* Sottometto l'azione di elaborazione
024000141217     C                   MOVEL     'UBMA'        KCOAZ
024100141216     C                   EVAL      KPJBU = UBMACTLDS
024200141027     C*
024300121108     C* chiamo
024400141217     C                   IF        $Debug = *blank
024500141217     C                   CALL      'BCH10'
024600141217     c                   parm                    Kpjba
024700141217     C                   ELSE
024800141217     C                   CALL      'UBMACTLR'
024900141217     c                   parm                    Kpjba
025000141217     C                   ENDIF
025100140120     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025500970526     C*------------------------------------------------------------------------*
025600970526     C     DECODI        BEGSR
025700121019     C*
025800120927     C                   SETOFF                                       505152
025900140120     c                   EVAL      VIDMSG    = *blank
026000120927     C*
026100141216     C* il nr. gg è obbligatorio
026200141216     C*
026300141216     C     ERRL00        IFEQ      '0'
026400141216     C                   IF        D03NrGG = 0
026500141216     C                   MOVEL     '1'           ERRL00
026600141216     C                   SETON                                        50
026700141216     C                   MOVEL     MSG(2)        DSMSMS
026800141216     C                   ENDIF
026900141216     C                   ENDIF
027000141216     C*
027100970528     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
027400970526     C*------------------------------------------------------------------------*
027500970526     C     AZZVID        BEGSR
027600141027     C*
027700141217     C                   EVAL      D03NrGg = 2
027800141217     C                   EVAL      D03Dest = *blank
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
028300970526     C*------------------------------------------------------------------------*
028400970526     C     IMPFUN        BEGSR
028500970526     C*
028600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
028700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
028800970526     C                   EXSR      FUNGES
028900970526     C*
029000970526     C                   ENDSR
029100020923     C*--------------------------------------------------------------------------------------------*
029200020923     C* REPERISCE I DATI UTENTE
029300020923     C*--------------------------------------------------------------------------------------------*
029400020923     C     REPDATIUTE    BEGSR
029500020923     C*
029600020923     C* INIZIALIZZA VARIABILI DI WRK
029700020923     C                   CLEAR                   TIBS34DS
029800020923     C                   CLEAR                   AZUTEDS
029900020923     C                   CLEAR                   DDATIUTE
030000020923     C*
030100020923     C     *DTAARA       DEFINE    §azute        azuteds
030200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
030300020923     C                   IN(E)     *DTAARA
030400020923if  1C                   IF        %Error
030500020923     c                   EVAL      I34Tla = 'L'
030600020923     C                   CALL      'TIBS34R'
030700020923     C                   PARM                    Tibs34Ds
030800020923     C                   IN        *DTAARA
030900020923e   1C                   ENDIF
031000020923     C*
031100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
031200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
031300020923     C*
031400020923     C                   ENDSR
031500970526     C*------------------------------------------------------------------------*
031600970526     C* *INZSR - OPERAZIONI INIZIALI
031700970526     C*------------------------------------------------------------------------*
031800970526     C     *INZSR        BEGSR
031900991217     C*
032000991217     C     *ENTRY        PLIST
032100991217     C                   PARM                    KPJBA
032200020923     C*
032300020923     C* REPERISCE I DATI DELL'UTENTE
032400020923     C                   EXSR      REPDATIUTE
032500970526     C*
032600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
032700970526     C     XMSGPA        PLIST
032800970526     C                   PARM                    DSMSMS
032900970526     C                   PARM                    DSMSPG
033000970526     C                   PARM                    DSMSKE
033100970526     C                   PARM                    DSMSER
033200970526     C*
033300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
033400970526     C                   CALL      'XKEY'
033500970526     C                   PARM                    DSKEY
033600970526     C*---
033700970526     C* VARIABILI NON RIFERITE AL DATA BASE
033800970526     C*---
033900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
034000991214     C                   MOVEL     *BLANKS       DEPOP1            3
034100970526     C*---
034200970526     C* VARIABILI DI CONTROLLO
034300970526     C*---
034400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
034500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
034600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
034700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
034800970526     C*
034900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
035000141216     C                   MOVEL     'UBMACTLR1'   DSMSPG
035100141216     C                   MOVEL     'UBMACTLR1'   MSGQUE
035200970526     C                   MOVEL     *BLANKS       MSGKEY
035300970526     C                   MOVEL     *BLANKS       DSMSMS
035400970528     C*
035500970528     C* CALCOLA LA DATA CORRENTE
035600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
035700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
035800970528     C                   Z-ADD     WN8           G08DAT
035900970528     C                   Z-ADD     *ZEROS        G08INV
036000970528     C                   MOVEL     '0'           G08ERR
036100970528     C                   CALL      'XSRDA8'
036200970528     C                   PARM                    WLBDA8
036300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
036400000310     C*
036500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
036600000310     C                   EXSR      AZZVID
036700970526     C*
036800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
036900970526     C                   EXSR      IMPFUN
037000970526     C*
037100970526     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
037400970526     C*------------------------------------------------------------------------*
037500970526     C     FUNGES        BEGSR
037600970526     C*
037700970526     C* PULISCO CAMPI DI LAVORO
037800970526     C                   Z-ADD     0             W                 5 0
037900970526     C*
038000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
038100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
038200970526     C     1             DO        24            I
038300970526     C                   MOVEL     FUD(I)        FUDRIG
038400970526     C*
038500970526     C* PULIZIA FUNZIONI PRECEDENTI
038600970526     C                   MOVEL     *BLANKS       FUV(I)
038700970526     C*
038800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
038900970526     C* (SALTA F24)
039000970526     C     FUDATT        IFEQ      'S'
039100970526     C     FUDFUN        ANDNE     '24'
039200970526     C                   ADD       1             W
039300970526     C                   MOVEL     FUDDES        FUV(W)
039400970526     C                   END
039500970526     C                   END                                                    DO WHILE
039600970526     C*
039700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
039800970526     C* ALTRIMENTI DISATTIVO F24
039900970526     C     1             DO        24            I
040000970526     C                   MOVEL     FUD(I)        FUDRIG
040100970526     C*
040200970526     C     FUDFUN        IFEQ      '24'
040300970526     C     W             IFGT      6
040400970526     C                   MOVEL     'S'           FUDATT
040500970526     C                   ELSE
040600970526     C                   MOVEL     *BLANKS       FUDATT
040700970526     C                   END                                                    W GT 8
040800970526     C                   MOVEL     FUDRIG        FUD(I)
040900970526     C                   END
041000970526     C                   END                                                    DO WHILE
041100970526     C*
041200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
041300970526     C     W             DIV       6             I
041400970526     C                   MVR                     Z
041500970526     C*
041600970526     C     Z             IFNE      0
041700970526     C     I             ADD       1             Z
041800970526     C                   ELSE
041900970526     C                   Z-ADD     I             Z                 5 0
042000970526     C                   END
042100970526     C*
042200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
042300970526     C                   ADD       1             FUNGRU
042400970526     C     FUNGRU        IFGT      Z
042500970526     C     FUNGRU        ORLT      1
042600970526     C                   Z-ADD     1             FUNGRU
042700970526     C                   END
042800970526     C*
042900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
043000970526     C     FUNGRU        IFEQ      1
043100970526     C                   MOVEL     FUV(1)        FUN001
043200970526     C                   MOVEL     FUV(2)        FUN002
043300970526     C                   MOVEL     FUV(3)        FUN003
043400970526     C                   MOVEL     FUV(4)        FUN004
043500970526     C                   MOVEL     FUV(5)        FUN005
043600970526     C                   MOVEL     FUV(6)        FUN006
043700970526     C                   END
043800970526     C*
043900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
044000970526     C     FUNGRU        IFEQ      2
044100970526     C                   MOVEL     FUV(7)        FUN001
044200970526     C                   MOVEL     FUV(8)        FUN002
044300970526     C                   MOVEL     FUV(9)        FUN003
044400970526     C                   MOVEL     FUV(10)       FUN004
044500970526     C                   MOVEL     FUV(11)       FUN005
044600970526     C                   MOVEL     FUV(12)       FUN006
044700970526     C                   END
044800970526     C*
044900970526     C* TERZO GRUPPO DI 6 FUNZIONI
045000970526     C     FUNGRU        IFEQ      3
045100970526     C                   MOVEL     FUV(13)       FUN001
045200970526     C                   MOVEL     FUV(14)       FUN002
045300970526     C                   MOVEL     FUV(15)       FUN003
045400970526     C                   MOVEL     FUV(16)       FUN004
045500970526     C                   MOVEL     FUV(17)       FUN005
045600970526     C                   MOVEL     FUV(18)       FUN006
045700970526     C                   END
045800970526     C*
045900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
046000970526     C     FUNGRU        IFEQ      4
046100970526     C                   MOVEL     FUV(19)       FUN001
046200970526     C                   MOVEL     FUV(20)       FUN002
046300970526     C                   MOVEL     FUV(21)       FUN003
046400970526     C                   MOVEL     FUV(22)       FUN004
046500970526     C                   MOVEL     *BLANKS       FUN005
046600970526     C                   MOVEL     *BLANKS       FUN006
046700970526     C                   END
046800970526     C*
046900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
047000970526     C                   MOVEL     *BLANKS       FUN007
047100970526     C                   MOVEL     *BLANKS       FUN008
047200970526     C*
047300970526     C     1             DO        24            I
047400970526     C                   MOVEL     FUD(I)        FUDRIG
047500970526     C     FUDFUN        IFEQ      '24'
047600970526     C     FUDATT        ANDEQ     'S'
047700970526     C                   MOVEL     FUDDES        FUN008
047800970526     C                   END
047900970526     C                   END                                                    DO
048000970526     C*
048100970526     C                   ENDSR
048200970526     C*------------------------------------------------------------------------*
048300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
048400970526     C*------------------------------------------------------------------------*
048500970526     C     FUNCON        BEGSR
048600970526     C*
048700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
048800970526     C                   EXSR      FUNVAR
048900970526     C*
049000970526     C* PULISCO CAMPI DI LAVORO
049100970526     C                   Z-ADD     0             I                 5 0
049200970526     C                   Z-ADD     0             W                 5 0
049300970526     C*
049400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
049500970526     C     FUNKEY        IFNE      *BLANKS
049600970526     C     1             DO        24            I
049700970526     C                   MOVEL     FUD(I)        FUDRIG
049800970526     C*
049900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
050000970526     C     FUNKEY        IFEQ      FUDFUN
050100970526     C     FUDATT        ANDEQ     'S'
050200970526     C                   Z-ADD     I             W
050300970526     C                   END
050400970526     C                   END                                                    DO WHILE
050500970526     C*
050600970526     C* FUNZIONE NON ATTIVA (ERRORE)
050700970526     C     W             IFEQ      0
050800970526     C*
050900970526     C* EMISSIONE MESSAGGIO DI ERRORE
051000970526     C                   MOVEL     MSG(1)        DSMSMS
051100970526     C*
051200970526     C                   END                                                    W=0
051300970526     C                   END                                                    FUNKEY NE *BLANKS
051400970526     C*
051500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
051600970526     C     W             IFNE      0
051700970526     C     FUNKEY        OREQ      *BLANKS
051800970526     C     FUNKEY        IFEQ      *BLANKS
051900970526     C                   EXSR      ENTGES
052000970526     C                   END
052100970526     C     FUNKEY        IFEQ      '03'
052200970526     C                   EXSR      F03GES
052300970526     C                   END
052400970526     C     FUNKEY        IFEQ      '06'
052500970526     C                   EXSR      F06GES
052600970526     C                   END
052700130109     C     FUNKEY        IFEQ      '07'
052800130109     C                   EXSR      F07GES
052900130109     C                   END
053000970526     C     FUNKEY        IFEQ      '12'
053100970526     C                   EXSR      F12GES
053200970526     C                   END
053300970526     C     FUNKEY        IFEQ      '24'
053400970526     C                   EXSR      F24GES
053500970526     C                   END
053600970526     C                   END
053700970526     C*
053800970526     C                   ENDSR
053900970526     C*------------------------------------------------------------------------*
054000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
054100970526     C*------------------------------------------------------------------------*
054200970526     C     FUNVAR        BEGSR
054300970526     C*
054400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
054500970526     C*
054600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
054700970526     C     KEY           IFEQ      ENTER
054800970526     C                   MOVEL     *BLANKS       FUNKEY
054900970526     C                   END
055000970526     C     KEY           IFEQ      F1
055100970526     C                   MOVEL     '01'          FUNKEY
055200970526     C                   END
055300970526     C     KEY           IFEQ      F2
055400970526     C                   MOVEL     '02'          FUNKEY
055500970526     C                   END
055600970526     C     KEY           IFEQ      F3
055700970526     C                   MOVEL     '03'          FUNKEY
055800970526     C                   END
055900970526     C     KEY           IFEQ      F4
056000970526     C                   MOVEL     '04'          FUNKEY
056100970526     C                   END
056200970526     C     KEY           IFEQ      F5
056300970526     C                   MOVEL     '05'          FUNKEY
056400970526     C                   END
056500970526     C     KEY           IFEQ      F6
056600970526     C                   MOVEL     '06'          FUNKEY
056700970526     C                   END
056800970526     C     KEY           IFEQ      F7
056900970526     C                   MOVEL     '07'          FUNKEY
057000970526     C                   END
057100970526     C     KEY           IFEQ      F8
057200970526     C                   MOVEL     '08'          FUNKEY
057300970526     C                   END
057400970526     C     KEY           IFEQ      F9
057500970526     C                   MOVEL     '09'          FUNKEY
057600970526     C                   END
057700970526     C     KEY           IFEQ      F10
057800970526     C                   MOVEL     '10'          FUNKEY
057900970526     C                   END
058000970526     C     KEY           IFEQ      F11
058100970526     C                   MOVEL     '11'          FUNKEY
058200970526     C                   END
058300970526     C     KEY           IFEQ      F12
058400970526     C                   MOVEL     '12'          FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F13
058700970526     C                   MOVEL     '13'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      F14
059000970526     C                   MOVEL     '14'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      F15
059300970526     C                   MOVEL     '15'          FUNKEY
059400970526     C                   END
059500970526     C     KEY           IFEQ      F16
059600970526     C                   MOVEL     '16'          FUNKEY
059700970526     C                   END
059800970526     C     KEY           IFEQ      F17
059900970526     C                   MOVEL     '17'          FUNKEY
060000970526     C                   END
060100970526     C     KEY           IFEQ      F18
060200970526     C                   MOVEL     '18'          FUNKEY
060300970526     C                   END
060400970526     C     KEY           IFEQ      F19
060500970526     C                   MOVEL     '19'          FUNKEY
060600970526     C                   END
060700970526     C     KEY           IFEQ      F20
060800970526     C                   MOVEL     '20'          FUNKEY
060900970526     C                   END
061000970526     C     KEY           IFEQ      F21
061100970526     C                   MOVEL     '21'          FUNKEY
061200970526     C                   END
061300970526     C     KEY           IFEQ      F22
061400970526     C                   MOVEL     '22'          FUNKEY
061500970526     C                   END
061600970526     C     KEY           IFEQ      F23
061700970526     C                   MOVEL     '23'          FUNKEY
061800970526     C                   END
061900970526     C     KEY           IFEQ      F24
062000970526     C                   MOVEL     '24'          FUNKEY
062100970526     C                   END
062200970526     C     KEY           IFEQ      ROLLUP
062300970526     C                   MOVEL     'RU'          FUNKEY
062400970526     C                   END
062500970526     C     KEY           IFEQ      HELP
062600970526     C                   MOVEL     'HP'          FUNKEY
062700970526     C                   END
062800970526     C                   ENDSR
062900970526     C*------------------------------------------------------------------------*
063000970526     C* NOTE COSTRUZIONE SCHIERE
063100970526     C*------------------------------------------------------------------------*
063200970526     C*
063300970526     C* -MSG-MESSAGGI
063400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
063500970526     C* (NON TOCCARE MSG,1 E MSG,2)
063600970526     C*
063700970526     C* -CMD-COMANDI
063800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
063900970526     C*
064000970526     C* -OPD-OPZIONI
064100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
064200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
064300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
064400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
064500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
064600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
064700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
064800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
064900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
065000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
065100970526     C*
065200970526     C* -FUD-FUNZIONI
065300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
065400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
065500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
065600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
065700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
065800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
065900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
066100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
066200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
066300970526** MSG - MESSAGGI
066400120927Tasto funzionale non gestito                                                   1
066500141217Numero giorni di ritardo obbligatorio                                          2
066600141216                                                                               3
066700141216                                                                               4
066800130108                                                                               5
066900130108Campo obbligatorio                                                             6
067000130108                                                                               7
067100130108                                                                               8
067200120927                                                                               9
067300120927                                                                               10
067400120927                                                                               11
067500120927                                                                               12
067600120927                                                                               13
067700141027                                                                               14
067800140116                                                                               15
067900120927                                                                               16
068000120927                                                                               17
068100120927                                                                               18
068200120927                                                                               19
068300141027Elaborazione conclusa                                                          20
068400141027                                                                               21
068500141027                                                                               22
068600121108                                                                               23
068700121108                                                                               24
068800970526** FUD - FUNZIONI
068900970526                   -        (S/N)=N                        funz.01¬
069000970526                   -        (S/N)=N                        funz.02¬
069100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
069200970526                   -        (S/N)=N                        funz.04¬
069300970526                   -        (S/N)=N                        funz.05¬
069400991214F06=Conferma       -        (S/N)=S                        funz.06¬
069500140819                   -        (S/N)=N                        funz.07¬
069600970527                   -        (S/N)=N                        funz.08¬
069700970527                   -        (S/N)=N                        funz.09¬
069800970527                   -        (S/N)=N                        funz.10¬
069900970526                   -        (S/N)=N                        funz.11¬
070000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
070100970526                   -        (S/N)=N                        funz.13¬
070200970526                   -        (S/N)=N                        funz.14¬
070300970526                   -        (S/N)=N                        funz.15¬
070400970526                   -        (S/N)=N                        funz.16¬
070500970526                   -        (S/N)=N                        funz.17¬
070600970526                   -        (S/N)=N                        funz.18¬
070700970526                   -        (S/N)=N                        funz.19¬
070800970526                   -        (S/N)=N                        funz.20¬
070900970526                   -        (S/N)=N                        funz.21¬
071000970526                   -        (S/N)=N                        funz.22¬
071100970526                   -        (S/N)=N                        funz.23¬
071200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
