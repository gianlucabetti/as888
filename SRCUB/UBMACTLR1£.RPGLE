000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800141027     FUBSMSSTSD1cf   e             WORKSTN INFDS(DEVDS1)
000900141027     F                                     SFILE(SMSSTSDMS:MSGULT)
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
007600141028     D* parametri UBSMSSTSR
007700141028     D*-------------------
007800141029     D UBSMSSTSDS      DS                  inz
007900141027     D  IndEMail                     50a
008000141027     D  DataDa                        8s 0
008100141027     D  DataA                         8s 0
008200141028     D  GgDa                          9
008300141028     D  GgA                           9
008400141027     D  NoInv                         7s 0
008500141027     D  Inv                           7s 0
008600141027     D  InvNP                         7s 0
008700141027     D  InvUnk                        7s 0
008800141027     D  InvFail                       7s 0
008900141027     D  InvNoEx                       7s 0
009000141027     D  InvOK                         7s 0
009100141027     D  InvMore                       7s 0
009200141028     D  InvIeri                       7s 0
009300141029     D  MedIeri                       6s 1
009400141028     D  InvOggi                       7s 0
009500141029     D  MedOggi                       6s 1
009600141030     D  InvOggi2Ore                   7s 0
009700120928     D*-------------------
009800120928
009900121003     d Status         sds
010000121003     d  SDSpgm           *proc
010100121003     d  SDSprm           *parms
010200121003     d  SDSuser              254    263
010300121003
010400140116     d pIn_Lib         s             10a
010500140819     d pIn_CLR         s              1a
010600140819     d pOut_RcvVar     s            500a
010700140819     d pOut_Esito      s              2  0
010800130116
010900130116     d ora             s               t
011000140117     d wX              s              2s 0
011100120928
011200970526     C*------------------------------------------------------------------------*
011300000303     C* MAIN LINES
011400970526     C*------------------------------------------------------------------------*
011500970526     C*
011600120927     C* metto i default su D03
011700120927     C                   EXSR      InzD03
011800120927     C*
011900120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012000991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012300970526     C*
012400970526     C* EMETTE FORMATO VIDEO
012500970526     C                   EXSR      BUTFMR
012600970526     C*
012700970526     C* GESTIONE TASTI FUNZIONALI
012800970526     C                   EXSR      FUNCON
012900970526E   2C                   ENDDO
013000970526     C*
013100970526     C* FINE PGM
013200970526     C                   SETON                                        LR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013500970526     C*------------------------------------------------------------------------*
013600970526     C     BUTFMR        BEGSR
013700970526     C*
013800970526     C* EMETTE LA TESTATA
013900141027     C                   WRITE     SMSSTSD01
014000970526     C*
014100970526     C* SPEDISCE EVENTUALE MESSAGGIO
014200970526     C                   EXSR      SNDMSG
014300970526     C*
014400970526     C* EMETTE LE FUNZIONI
014500141027     C                   WRITE     SMSSTSD02
014600970526     C*
014700970526     C* EMETTE IL VIDEO
014800141027     C                   EXFMT     SMSSTSD03
014900970526     C*
015000991214     C* REIMPOSTA FLAG DI ERRORE
015100991214     C                   MOVEL     '0'           ERRL00
015200970526     C*
015300970526     C* REIMPOSTA INDICATORI DI ERRORE
015400020611     C                   SETOFF                                       555556
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800120927     C* InzD03 - Inizializzo D03
015900970526     C*------------------------------------------------------------------------*
016000120927     C     InzD03        BEGSR
016100120927     C*
016200141027     C                   EVAL      D03DataA = %dec(%date() : *eur)
016300130108     C*
016400120927     C                   ENDSR
016500120927     C*------------------------------------------------------------------------*
016600120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016700120927     C*------------------------------------------------------------------------*
016800120927     C     SNDMSG        BEGSR
016900120927     C*
017000970526     C* PULISCE IL SFILE MESSAGGI
017100970526     C                   MOVEL     '1'           *IN40
017200141027     C                   WRITE     SMSSTSDMC
017300970526     C                   MOVEL     '0'           *IN40
017400970526     C*
017500970526     C* VISUALIZZA IL MESSAGGIO
017600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017700970526     C                   CALL      'XMSG'        XMSGPA
017800020611     C     DSMSER        IFEQ      *ZEROS
017900970526     C                   MOVEL     DSMSKE        MSGKEY
018000020611     C                   ENDIF
018100970526     C                   Z-ADD     1             MSGULT
018200141027     C                   WRITE     SMSSTSDMS
018300141027     C                   WRITE     SMSSTSDMC
018400970526     C*
018500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018600970526     C                   MOVEL     *BLANKS       DSMSMS
018700970526     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* ENTGES - GESTIONE ENTER -> INVIO
019100970526     C*------------------------------------------------------------------------*
019200970526     C     ENTGES        BEGSR
019300970526     C*
019400970526     C* CONTROLLA I CAMPI IMMESSI
019500970526     C                   EXSR      DECODI
019600970526     C*
019700970526     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* F03GES - GESTIONE F03 -> FINE
020000970526     C*------------------------------------------------------------------------*
020100970526     C     F03GES        BEGSR
020200970526     C*
020300970526     C* IMPOSTA "PREMUTO F03"
020400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020500970526     C*
020600970526     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800991214     C* F06GES - GESTIONE F06 -> CONFERMA
020900970526     C*------------------------------------------------------------------------*
021000970526     C     F06GES        BEGSR
021100970526     C*
021200991214     C* CONTROLLA I CAMPI IMMESSI
021300991214     C                   EXSR      DECODI
021400020611     C*
021500991214     C* ESEGUE ROUTINE DI CONFERMA
021600020611     C     ERRL00        IFEQ      '0'
021700991214     C                   EXSR      ESEGUI
021800130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
021900121109     C                   MOVEL     MSG(20)       DSMSMS
022000121109     C* non esco mai dal pgm
022100121109     C***                MOVEL     '1'           FINL00
022200121108     C                   ENDIF
022300970526     C*
022400991214     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600130109     C* F07GES - GESTIONE F07 -> STAMPA
022700970526     C*------------------------------------------------------------------------*
022800130109     C     F07GES        BEGSR
022900970526     C*
023000140818     C                   EXSR      F06GES
023100130110     C*
023200970526     C                   ENDSR
023300130109     C*------------------------------------------------------------------------*
023400130109     C* F12GES - GESTIONE F12 -> RITORNO
023500130109     C*------------------------------------------------------------------------*
023600130109     C     F12GES        BEGSR
023700130109     C*
023800130109     C* IMPOSTA "PREMUTO F12"
023900130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024000130109     C*
024100130109     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024400970526     C*------------------------------------------------------------------------*
024500970526     C     F24GES        BEGSR
024600970526     C*
024700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
025200970526     C*------------------------------------------------------------------------*
025300991214     C     ESEGUI        BEGSR
025400141027     C*
025500141027     C* da questo pgm non si passa un indirizzo email perché mostra a video il risultato
025600141029     C                   RESET                   UBSMSSTSDS
025700141027     C                   EVAL      IndEMail = *blank
025800141027     C*
025900141027     C                   EVAL      DataDa = wDataDa
026000141027     C                   EVAL      DataA  = wDataA
026100141027     C                   EVAL      KPJBU = UBSMSSTSDS
026200141027     C*
026300121108     C* chiamo
026400141027     C                   CALL      'UBSMSSTSR'
026500141027     c                   parm                    Kpjba
026600141027     C*
026700141027     C                   EVAL      UBSMSSTSDS = KPJBU
026800141028     C                   EVAL      wDataDa = DataDa
026900141028     C                   Z-ADD     wDataDa       G02INV
027000141028     C                   Z-ADD     *ZERO         G02DAT
027100141028     C                   MOVEL     '3'           G02ERR
027200141028     C                   CALL      'XSRDA8'
027300141028     C                   PARM                    WLBDAT
027400141028     C                   Z-ADD     G02DAT        D03DataDaO
027500141028     C                   EVAL      wDataA  = DataA
027600141028     C                   Z-ADD     wDataA        G02INV
027700141028     C                   Z-ADD     *ZERO         G02DAT
027800141028     C                   MOVEL     '3'           G02ERR
027900141028     C                   CALL      'XSRDA8'
028000141028     C                   PARM                    WLBDAT
028100141028     C                   Z-ADD     G02DAT        D03DataAO
028200141028     C                   EVAL      D03GgDa    = GgDa
028300141028     C                   EVAL      D03GgA     = GgA
028400141028     C                   EVAL      D03NOINV   = NoInv
028500141027     C                   EVAL      D03INV     = Inv
028600141027     C                   EVAL      D03INVNP   = InvNP
028700141027     C                   EVAL      D03INVUNK  = InvUnk
028800141027     C                   EVAL      D03INVFAIL = InvFail
028900141027     C                   EVAL      D03INVNOEX = InvNoEx
029000141027     C                   EVAL      D03INVOK   = InvOK
029100141027     C                   EVAL      D03INVMORE = InvMore
029200141121     C                   IF        Inv <> 0
029300141121     C                   EVAL      D03MedNP   = InvNP*100/Inv
029400141121     C                   EVAL      D03MedUnk  = InvUnk*100/Inv
029500141121     C                   EVAL      D03MedFail = InvFail*100/Inv
029600141121     C                   EVAL      D03MedNoEx = InvNoEx*100/Inv
029700141121     C                   EVAL      D03MedOK   = InvOK*100/Inv
029800141121     C                   EVAL      D03MedMore = InvMore*100/Inv
029900141121     C                   ENDIF
030000141028     C                   EVAL      D03InvIeri = InvIeri
030100141029     C                   EVAL      D03MedIeri = MedIeri
030200141028     C                   EVAL      D03InvOggi = InvOggi
030300141029     C                   EVAL      D03MedOggi = MedOggi
030400141028     C                   EVAL      D03Oggi = %dec(%date() : *eur)
030500141028     C                   EVAL      D03Ieri = %dec(%date() - %days(1) : *eur)
030600141030     C                   EVAL      D03Inv2Ore = InvOggi2Ore
030700140120     C*
030800970526     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
031100970526     C*------------------------------------------------------------------------*
031200970526     C     DECODI        BEGSR
031300121019     C*
031400120927     C                   SETOFF                                       505152
031500140120     c                   EVAL      VIDMSG    = *blank
031600141029     C                   Z-ADD     0             WDataA
031700141029     C                   Z-ADD     0             WDataDa
031800141027      *
031900141027      * GIRO E CONTROLLO DATE
032000141027      *
032100141027     C     ERRL00        IFEQ      '0'
032200141027     C     D03DataDa     IFNE      *ZEROS
032300141027     C                   Z-ADD     D03DataDa     G02DAT
032400141027     C                   Z-ADD     *ZERO         G02INV
032500141027     C                   MOVEL     *BLANKS       G02ERR
032600141027     C                   CALL      'XSRDA8'
032700141027     C                   PARM                    WLBDAT
032800141027     C                   Z-ADD     G02DAT        D03DataDa
032900141027     C     G02ERR        IFEQ      '1'
033000141027     C                   MOVEL     '1'           ERRL00
033100141027     C                   SETON                                        50        Formato data errato
033200141027     C                   MOVEL     MSG(3)        DSMSMS
033300141027     C                   MOVE      *ZERO         wDataDa
033400141027     C                   ELSE
033500141027     C                   Z-ADD     G02INV        WDataDa
033600141027     C                   ENDIF
033700141027     C                   ENDIF
033800141027     C                   ENDIF
033900141027
034000141027     C     ERRL00        IFEQ      '0'
034100141027     C     D03DataA      IFNE      *ZEROS
034200141027     C                   Z-ADD     D03DataA      G02DAT
034300141027     C                   Z-ADD     *ZERO         G02INV
034400141027     C                   MOVEL     *BLANKS       G02ERR
034500141027     C                   CALL      'XSRDA8'
034600141027     C                   PARM                    WLBDAT
034700141027     C                   Z-ADD     G02DAT        D03DataA
034800141027     C     G02ERR        IFEQ      '1'
034900141027     C                   MOVEL     '1'           ERRL00
035000141027     C                   SETON                                        51        Formato data errato
035100141027     C                   MOVEL     MSG(3)        DSMSMS
035200141027     C                   MOVE      *ZERO         wDataA
035300141027     C                   ELSE
035400141027     C                   Z-ADD     G02INV        WDataA
035500141027     C                   ENDIF
035600141027     C                   ELSE
035700141027     C                   SETON                                        51        Dato obbligatorio
035800141027     C                   MOVEL     MSG(6)        DSMSMS
035900141027     C                   MOVE      *ZERO         wDataA
036000141027     C                   ENDIF
036100141027     C                   ENDIF
036200121001     C*
036300141027     C* Data A < Data Da
036400121001     C     ERRL00        IFEQ      '0'
036500141027     C                   IF        wDataA < wDataDa
036600141027     C                   MOVEL     '1'           ERRL00
036700141027     C                   SETON                                        5051
036800141027     C                   MOVEL     MSG(4)        DSMSMS
036900120927     C                   ENDIF
037000140116     C                   ENDIF
037100120927     C*
037200970528     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
037500970526     C*------------------------------------------------------------------------*
037600970526     C     AZZVID        BEGSR
037700141027     C*
037800141027     C                   EVAL      D03DataDa = 0
037900141027     C                   EVAL      D03DataA = %dec(%date())
038000970526     C*
038100970526     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
038400970526     C*------------------------------------------------------------------------*
038500970526     C     IMPFUN        BEGSR
038600970526     C*
038700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
038800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
038900970526     C                   EXSR      FUNGES
039000970526     C*
039100970526     C                   ENDSR
039200020923     C*--------------------------------------------------------------------------------------------*
039300020923     C* REPERISCE I DATI UTENTE
039400020923     C*--------------------------------------------------------------------------------------------*
039500020923     C     REPDATIUTE    BEGSR
039600020923     C*
039700020923     C* INIZIALIZZA VARIABILI DI WRK
039800020923     C                   CLEAR                   TIBS34DS
039900020923     C                   CLEAR                   AZUTEDS
040000020923     C                   CLEAR                   DDATIUTE
040100020923     C*
040200020923     C     *DTAARA       DEFINE    §azute        azuteds
040300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
040400020923     C                   IN(E)     *DTAARA
040500020923if  1C                   IF        %Error
040600020923     c                   EVAL      I34Tla = 'L'
040700020923     C                   CALL      'TIBS34R'
040800020923     C                   PARM                    Tibs34Ds
040900020923     C                   IN        *DTAARA
041000020923e   1C                   ENDIF
041100020923     C*
041200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
041300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
041400020923     C*
041500020923     C                   ENDSR
041600970526     C*------------------------------------------------------------------------*
041700970526     C* *INZSR - OPERAZIONI INIZIALI
041800970526     C*------------------------------------------------------------------------*
041900970526     C     *INZSR        BEGSR
042000991217     C*
042100991217     C     *ENTRY        PLIST
042200991217     C                   PARM                    KPJBA
042300020923     C*
042400020923     C* REPERISCE I DATI DELL'UTENTE
042500020923     C                   EXSR      REPDATIUTE
042600970526     C*
042700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
042800970526     C     XMSGPA        PLIST
042900970526     C                   PARM                    DSMSMS
043000970526     C                   PARM                    DSMSPG
043100970526     C                   PARM                    DSMSKE
043200970526     C                   PARM                    DSMSER
043300970526     C*
043400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
043500970526     C                   CALL      'XKEY'
043600970526     C                   PARM                    DSKEY
043700970526     C*---
043800970526     C* VARIABILI NON RIFERITE AL DATA BASE
043900970526     C*---
044000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
044100991214     C                   MOVEL     *BLANKS       DEPOP1            3
044200970526     C*---
044300970526     C* VARIABILI DI CONTROLLO
044400970526     C*---
044500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
044600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
044700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
044800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
044900970526     C*
045000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
045100141027     C                   MOVEL     'UBSMSSTSR1'  DSMSPG
045200141027     C                   MOVEL     'UBSMSSTSR1'  MSGQUE
045300970526     C                   MOVEL     *BLANKS       MSGKEY
045400970526     C                   MOVEL     *BLANKS       DSMSMS
045500970528     C*
045600970528     C* CALCOLA LA DATA CORRENTE
045700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
045800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
045900970528     C                   Z-ADD     WN8           G08DAT
046000970528     C                   Z-ADD     *ZEROS        G08INV
046100970528     C                   MOVEL     '0'           G08ERR
046200970528     C                   CALL      'XSRDA8'
046300970528     C                   PARM                    WLBDA8
046400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
046500000310     C*
046600000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
046700000310     C                   EXSR      AZZVID
046800970526     C*
046900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
047000970526     C                   EXSR      IMPFUN
047100970526     C*
047200970526     C                   ENDSR
047300970526     C*------------------------------------------------------------------------*
047400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
047500970526     C*------------------------------------------------------------------------*
047600970526     C     FUNGES        BEGSR
047700970526     C*
047800970526     C* PULISCO CAMPI DI LAVORO
047900970526     C                   Z-ADD     0             W                 5 0
048000970526     C*
048100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
048200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
048300970526     C     1             DO        24            I
048400970526     C                   MOVEL     FUD(I)        FUDRIG
048500970526     C*
048600970526     C* PULIZIA FUNZIONI PRECEDENTI
048700970526     C                   MOVEL     *BLANKS       FUV(I)
048800970526     C*
048900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
049000970526     C* (SALTA F24)
049100970526     C     FUDATT        IFEQ      'S'
049200970526     C     FUDFUN        ANDNE     '24'
049300970526     C                   ADD       1             W
049400970526     C                   MOVEL     FUDDES        FUV(W)
049500970526     C                   END
049600970526     C                   END                                                    DO WHILE
049700970526     C*
049800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
049900970526     C* ALTRIMENTI DISATTIVO F24
050000970526     C     1             DO        24            I
050100970526     C                   MOVEL     FUD(I)        FUDRIG
050200970526     C*
050300970526     C     FUDFUN        IFEQ      '24'
050400970526     C     W             IFGT      6
050500970526     C                   MOVEL     'S'           FUDATT
050600970526     C                   ELSE
050700970526     C                   MOVEL     *BLANKS       FUDATT
050800970526     C                   END                                                    W GT 8
050900970526     C                   MOVEL     FUDRIG        FUD(I)
051000970526     C                   END
051100970526     C                   END                                                    DO WHILE
051200970526     C*
051300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
051400970526     C     W             DIV       6             I
051500970526     C                   MVR                     Z
051600970526     C*
051700970526     C     Z             IFNE      0
051800970526     C     I             ADD       1             Z
051900970526     C                   ELSE
052000970526     C                   Z-ADD     I             Z                 5 0
052100970526     C                   END
052200970526     C*
052300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
052400970526     C                   ADD       1             FUNGRU
052500970526     C     FUNGRU        IFGT      Z
052600970526     C     FUNGRU        ORLT      1
052700970526     C                   Z-ADD     1             FUNGRU
052800970526     C                   END
052900970526     C*
053000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
053100970526     C     FUNGRU        IFEQ      1
053200970526     C                   MOVEL     FUV(1)        FUN001
053300970526     C                   MOVEL     FUV(2)        FUN002
053400970526     C                   MOVEL     FUV(3)        FUN003
053500970526     C                   MOVEL     FUV(4)        FUN004
053600970526     C                   MOVEL     FUV(5)        FUN005
053700970526     C                   MOVEL     FUV(6)        FUN006
053800970526     C                   END
053900970526     C*
054000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
054100970526     C     FUNGRU        IFEQ      2
054200970526     C                   MOVEL     FUV(7)        FUN001
054300970526     C                   MOVEL     FUV(8)        FUN002
054400970526     C                   MOVEL     FUV(9)        FUN003
054500970526     C                   MOVEL     FUV(10)       FUN004
054600970526     C                   MOVEL     FUV(11)       FUN005
054700970526     C                   MOVEL     FUV(12)       FUN006
054800970526     C                   END
054900970526     C*
055000970526     C* TERZO GRUPPO DI 6 FUNZIONI
055100970526     C     FUNGRU        IFEQ      3
055200970526     C                   MOVEL     FUV(13)       FUN001
055300970526     C                   MOVEL     FUV(14)       FUN002
055400970526     C                   MOVEL     FUV(15)       FUN003
055500970526     C                   MOVEL     FUV(16)       FUN004
055600970526     C                   MOVEL     FUV(17)       FUN005
055700970526     C                   MOVEL     FUV(18)       FUN006
055800970526     C                   END
055900970526     C*
056000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
056100970526     C     FUNGRU        IFEQ      4
056200970526     C                   MOVEL     FUV(19)       FUN001
056300970526     C                   MOVEL     FUV(20)       FUN002
056400970526     C                   MOVEL     FUV(21)       FUN003
056500970526     C                   MOVEL     FUV(22)       FUN004
056600970526     C                   MOVEL     *BLANKS       FUN005
056700970526     C                   MOVEL     *BLANKS       FUN006
056800970526     C                   END
056900970526     C*
057000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
057100970526     C                   MOVEL     *BLANKS       FUN007
057200970526     C                   MOVEL     *BLANKS       FUN008
057300970526     C*
057400970526     C     1             DO        24            I
057500970526     C                   MOVEL     FUD(I)        FUDRIG
057600970526     C     FUDFUN        IFEQ      '24'
057700970526     C     FUDATT        ANDEQ     'S'
057800970526     C                   MOVEL     FUDDES        FUN008
057900970526     C                   END
058000970526     C                   END                                                    DO
058100970526     C*
058200970526     C                   ENDSR
058300970526     C*------------------------------------------------------------------------*
058400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
058500970526     C*------------------------------------------------------------------------*
058600970526     C     FUNCON        BEGSR
058700970526     C*
058800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
058900970526     C                   EXSR      FUNVAR
059000970526     C*
059100970526     C* PULISCO CAMPI DI LAVORO
059200970526     C                   Z-ADD     0             I                 5 0
059300970526     C                   Z-ADD     0             W                 5 0
059400970526     C*
059500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
059600970526     C     FUNKEY        IFNE      *BLANKS
059700970526     C     1             DO        24            I
059800970526     C                   MOVEL     FUD(I)        FUDRIG
059900970526     C*
060000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
060100970526     C     FUNKEY        IFEQ      FUDFUN
060200970526     C     FUDATT        ANDEQ     'S'
060300970526     C                   Z-ADD     I             W
060400970526     C                   END
060500970526     C                   END                                                    DO WHILE
060600970526     C*
060700970526     C* FUNZIONE NON ATTIVA (ERRORE)
060800970526     C     W             IFEQ      0
060900970526     C*
061000970526     C* EMISSIONE MESSAGGIO DI ERRORE
061100970526     C                   MOVEL     MSG(1)        DSMSMS
061200970526     C*
061300970526     C                   END                                                    W=0
061400970526     C                   END                                                    FUNKEY NE *BLANKS
061500970526     C*
061600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
061700970526     C     W             IFNE      0
061800970526     C     FUNKEY        OREQ      *BLANKS
061900970526     C     FUNKEY        IFEQ      *BLANKS
062000970526     C                   EXSR      ENTGES
062100970526     C                   END
062200970526     C     FUNKEY        IFEQ      '03'
062300970526     C                   EXSR      F03GES
062400970526     C                   END
062500970526     C     FUNKEY        IFEQ      '06'
062600970526     C                   EXSR      F06GES
062700970526     C                   END
062800130109     C     FUNKEY        IFEQ      '07'
062900130109     C                   EXSR      F07GES
063000130109     C                   END
063100970526     C     FUNKEY        IFEQ      '12'
063200970526     C                   EXSR      F12GES
063300970526     C                   END
063400970526     C     FUNKEY        IFEQ      '24'
063500970526     C                   EXSR      F24GES
063600970526     C                   END
063700970526     C                   END
063800970526     C*
063900970526     C                   ENDSR
064000970526     C*------------------------------------------------------------------------*
064100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
064200970526     C*------------------------------------------------------------------------*
064300970526     C     FUNVAR        BEGSR
064400970526     C*
064500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
064600970526     C*
064700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
064800970526     C     KEY           IFEQ      ENTER
064900970526     C                   MOVEL     *BLANKS       FUNKEY
065000970526     C                   END
065100970526     C     KEY           IFEQ      F1
065200970526     C                   MOVEL     '01'          FUNKEY
065300970526     C                   END
065400970526     C     KEY           IFEQ      F2
065500970526     C                   MOVEL     '02'          FUNKEY
065600970526     C                   END
065700970526     C     KEY           IFEQ      F3
065800970526     C                   MOVEL     '03'          FUNKEY
065900970526     C                   END
066000970526     C     KEY           IFEQ      F4
066100970526     C                   MOVEL     '04'          FUNKEY
066200970526     C                   END
066300970526     C     KEY           IFEQ      F5
066400970526     C                   MOVEL     '05'          FUNKEY
066500970526     C                   END
066600970526     C     KEY           IFEQ      F6
066700970526     C                   MOVEL     '06'          FUNKEY
066800970526     C                   END
066900970526     C     KEY           IFEQ      F7
067000970526     C                   MOVEL     '07'          FUNKEY
067100970526     C                   END
067200970526     C     KEY           IFEQ      F8
067300970526     C                   MOVEL     '08'          FUNKEY
067400970526     C                   END
067500970526     C     KEY           IFEQ      F9
067600970526     C                   MOVEL     '09'          FUNKEY
067700970526     C                   END
067800970526     C     KEY           IFEQ      F10
067900970526     C                   MOVEL     '10'          FUNKEY
068000970526     C                   END
068100970526     C     KEY           IFEQ      F11
068200970526     C                   MOVEL     '11'          FUNKEY
068300970526     C                   END
068400970526     C     KEY           IFEQ      F12
068500970526     C                   MOVEL     '12'          FUNKEY
068600970526     C                   END
068700970526     C     KEY           IFEQ      F13
068800970526     C                   MOVEL     '13'          FUNKEY
068900970526     C                   END
069000970526     C     KEY           IFEQ      F14
069100970526     C                   MOVEL     '14'          FUNKEY
069200970526     C                   END
069300970526     C     KEY           IFEQ      F15
069400970526     C                   MOVEL     '15'          FUNKEY
069500970526     C                   END
069600970526     C     KEY           IFEQ      F16
069700970526     C                   MOVEL     '16'          FUNKEY
069800970526     C                   END
069900970526     C     KEY           IFEQ      F17
070000970526     C                   MOVEL     '17'          FUNKEY
070100970526     C                   END
070200970526     C     KEY           IFEQ      F18
070300970526     C                   MOVEL     '18'          FUNKEY
070400970526     C                   END
070500970526     C     KEY           IFEQ      F19
070600970526     C                   MOVEL     '19'          FUNKEY
070700970526     C                   END
070800970526     C     KEY           IFEQ      F20
070900970526     C                   MOVEL     '20'          FUNKEY
071000970526     C                   END
071100970526     C     KEY           IFEQ      F21
071200970526     C                   MOVEL     '21'          FUNKEY
071300970526     C                   END
071400970526     C     KEY           IFEQ      F22
071500970526     C                   MOVEL     '22'          FUNKEY
071600970526     C                   END
071700970526     C     KEY           IFEQ      F23
071800970526     C                   MOVEL     '23'          FUNKEY
071900970526     C                   END
072000970526     C     KEY           IFEQ      F24
072100970526     C                   MOVEL     '24'          FUNKEY
072200970526     C                   END
072300970526     C     KEY           IFEQ      ROLLUP
072400970526     C                   MOVEL     'RU'          FUNKEY
072500970526     C                   END
072600970526     C     KEY           IFEQ      HELP
072700970526     C                   MOVEL     'HP'          FUNKEY
072800970526     C                   END
072900970526     C                   ENDSR
073000970526     C*------------------------------------------------------------------------*
073100970526     C* NOTE COSTRUZIONE SCHIERE
073200970526     C*------------------------------------------------------------------------*
073300970526     C*
073400970526     C* -MSG-MESSAGGI
073500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
073600970526     C* (NON TOCCARE MSG,1 E MSG,2)
073700970526     C*
073800970526     C* -CMD-COMANDI
073900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
074000970526     C*
074100970526     C* -OPD-OPZIONI
074200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
074300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
074400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
074500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
074600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
074700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
074800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
074900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
075000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
075100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
075200970526     C*
075300970526     C* -FUD-FUNZIONI
075400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
075500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
075600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
075700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
075800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
075900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
076000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
076100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
076200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
076300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
076400970526** MSG - MESSAGGI
076500120927Tasto funzionale non gestito                                                   1
076600120927Numero chiamate obbligatorio                                                   2
076700141027Data errata                                                                    3
076800141027Data A < Data DA                                                               4
076900130108                                                                               5
077000130108Campo obbligatorio                                                             6
077100130108                                                                               7
077200130108                                                                               8
077300120927                                                                               9
077400120927                                                                               10
077500120927                                                                               11
077600120927                                                                               12
077700120927                                                                               13
077800141027                                                                               14
077900140116                                                                               15
078000120927                                                                               16
078100120927                                                                               17
078200120927                                                                               18
078300120927                                                                               19
078400141027Elaborazione conclusa                                                          20
078500141027                                                                               21
078600141027                                                                               22
078700121108                                                                               23
078800121108                                                                               24
078900970526** FUD - FUNZIONI
079000970526                   -        (S/N)=N                        funz.01¬
079100970526                   -        (S/N)=N                        funz.02¬
079200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
079300970526                   -        (S/N)=N                        funz.04¬
079400970526                   -        (S/N)=N                        funz.05¬
079500991214F06=Conferma       -        (S/N)=S                        funz.06¬
079600140819                   -        (S/N)=N                        funz.07¬
079700970527                   -        (S/N)=N                        funz.08¬
079800970527                   -        (S/N)=N                        funz.09¬
079900970527                   -        (S/N)=N                        funz.10¬
080000970526                   -        (S/N)=N                        funz.11¬
080100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
080200970526                   -        (S/N)=N                        funz.13¬
080300970526                   -        (S/N)=N                        funz.14¬
080400970526                   -        (S/N)=N                        funz.15¬
080500970526                   -        (S/N)=N                        funz.16¬
080600970526                   -        (S/N)=N                        funz.17¬
080700970526                   -        (S/N)=N                        funz.18¬
080800970526                   -        (S/N)=N                        funz.19¬
080900970526                   -        (S/N)=N                        funz.20¬
081000970526                   -        (S/N)=N                        funz.21¬
081100970526                   -        (S/N)=N                        funz.22¬
081200970526                   -        (S/N)=N                        funz.23¬
081300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
