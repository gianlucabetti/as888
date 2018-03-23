000100970526     H*------------------------------------------------------------------------*
000200140214     H* Scrittura FISIA00F per test
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800140214     FUBTSTSIAD cf   e             WORKSTN INFDS(DEVDS1)
000900140214     F                                     SFILE(TSTSIADMS:MSGULT)
001000140214     FFISIA00F  if A e             DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D* MESSAGGI
001500970526     D*------------
001600000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* DESCRIZIONI TASTI FUNZIONALI
001900970526     D*------------
002000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* FUNZIONI DA CARICARE A VIDEO
002300970526     D*------------
002400970526     D FUV             S             19    DIM(24)
002500130920     D*------------
002600140214     D* DS di suddivisione campi FISIA
002700130920     D*------------
002800140214     D DSIAMSG       E DS
002900140214     D DSIAFLO       E DS
003000970526     D*------------------------------------------------------------------------*
003100970526     D* INPUT
003200970526     D*------------------------------------------------------------------------*
003300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003400970526     D*------------
003500970526     D FUDDS           DS
003600970526     D  FUDDE1                 1      1
003700970526     D  FUDFUN                 2      3
003800970526     D  FUDDE2                 4     19
003900970526     D  FUDFI0                20     34
004000970526     D  FUDATT                35     35
004100970526     D  FUDFI1                36     80
004200970526     D  FUDDES                 1     19
004300970526     D  FUDRIG                 1     80
004400970526     D*------------
004500970526     D* DS GESTIONE TASTI FUNZIONALI
004600970526     D*------------
004700970526     D DSKEY         E DS
004800970526     D DEVDS1          DS
004900970526     D  KEY                  369    369
005000970526     D*------------
005100970526     D* DS "XMSG" - GESTIONE MESSAGGI
005200970526     D*------------
005300970526     D DSMSG         E DS
005400970526     D*------------
005500970526     D* ARCHITETTURA
005600970526     D*------------
005700970526     D KPJBA         E DS
005800020923     D*-------------------
005900020923     D* DS REPERIMENTO DATI UTENTE
006000020923     D*-------------------
006100020923     D TIBS34DS      E DS                                                       *Profili utente
006200141009     D DSIARIC       E DS                                                       *Profili utente
006300020923     D DDATIUTE      E DS                                                       *Dati utente
006400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006500120928     D*-------------------
006600120928
006700120928
006800970526     C*------------------------------------------------------------------------*
006900000303     C* MAIN LINES
007000970526     C*------------------------------------------------------------------------*
007100970526     C*
007200120927     C* metto i default su D03
007300120927     C                   EXSR      InzD03
007400120927     C*
007500120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007600991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
007700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
007800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
007900970526     C*
008000970526     C* EMETTE FORMATO VIDEO
008100970526     C                   EXSR      BUTFMR
008200970526     C*
008300970526     C* GESTIONE TASTI FUNZIONALI
008400970526     C                   EXSR      FUNCON
008500970526E   2C                   ENDDO
008600970526     C*
008700970526     C* FINE PGM
008800970526     C                   SETON                                        LR
008900970526     C*------------------------------------------------------------------------*
009000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009100970526     C*------------------------------------------------------------------------*
009200970526     C     BUTFMR        BEGSR
009300970526     C*
009400970526     C* EMETTE LA TESTATA
009500140214     C                   WRITE     TSTSIAD01
009600970526     C*
009700970526     C* SPEDISCE EVENTUALE MESSAGGIO
009800970526     C                   EXSR      SNDMSG
009900970526     C*
010000970526     C* EMETTE LE FUNZIONI
010100140214     C                   WRITE     TSTSIAD02
010200970526     C*
010300970526     C* EMETTE IL VIDEO
010400140214     C                   EXFMT     TSTSIAD03
010500970526     C*
010600991214     C* REIMPOSTA FLAG DI ERRORE
010700991214     C                   MOVEL     '0'           ERRL00
010800970526     C*
010900970526     C* REIMPOSTA INDICATORI DI ERRORE
011000020611     C                   SETOFF                                       555556
011100970526     C*
011200970526     C                   ENDSR
011300970526     C*------------------------------------------------------------------------*
011400120927     C* InzD03 - Inizializzo D03
011500970526     C*------------------------------------------------------------------------*
011600120927     C     InzD03        BEGSR
011700120927     C*
011800130108     C*
011900120927     C                   ENDSR
012000120927     C*------------------------------------------------------------------------*
012100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012200120927     C*------------------------------------------------------------------------*
012300120927     C     SNDMSG        BEGSR
012400120927     C*
012500970526     C* PULISCE IL SFILE MESSAGGI
012600970526     C                   MOVEL     '1'           *IN40
012700140214     C                   WRITE     TSTSIADMC
012800970526     C                   MOVEL     '0'           *IN40
012900970526     C*
013000970526     C* VISUALIZZA IL MESSAGGIO
013100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
013200970526     C                   CALL      'XMSG'        XMSGPA
013300020611     C     DSMSER        IFEQ      *ZEROS
013400970526     C                   MOVEL     DSMSKE        MSGKEY
013500020611     C                   ENDIF
013600970526     C                   Z-ADD     1             MSGULT
013700140214     C                   WRITE     TSTSIADMS
013800140214     C                   WRITE     TSTSIADMC
013900970526     C*
014000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014100970526     C                   MOVEL     *BLANKS       DSMSMS
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500970526     C* ENTGES - GESTIONE ENTER -> INVIO
014600970526     C*------------------------------------------------------------------------*
014700970526     C     ENTGES        BEGSR
014800970526     C*
014900970526     C* CONTROLLA I CAMPI IMMESSI
015000970526     C                   EXSR      DECODI
015100970526     C*
015200970526     C                   ENDSR
015300970526     C*------------------------------------------------------------------------*
015400970526     C* F03GES - GESTIONE F03 -> FINE
015500970526     C*------------------------------------------------------------------------*
015600970526     C     F03GES        BEGSR
015700970526     C*
015800970526     C* IMPOSTA "PREMUTO F03"
015900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016000970526     C*
016100970526     C                   ENDSR
016200140214     C*------------------------------------------------------------------------*
016300140214     C* F05GES - GESTIONE F05 -> Pulizia video
016400140214     C*------------------------------------------------------------------------*
016500140214     C     F05GES        BEGSR
016600140214     C*
016700140214     C* pulisco i campi di input
016800140214     C                   EVAL      VIDAAS = 0
016900140214     C                   EVAL      VIDLNP = 0
017000140214     C                   EVAL      VIDNRS = 0
017100140214     C                   EVAL      VIDNSP = 0
017200140214     C                   EVAL      VIDTipA = *blank
017300140214     C                   EVAL      VIDEmail = *blank
017400140214     C                   EVAL      VIDCel = *blank
017500140214     C                   EVAL      VIDNOTE = *blank
017600140214     C*
017700140214     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900991214     C* F06GES - GESTIONE F06 -> CONFERMA
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F06GES        BEGSR
018200970526     C*
018300991214     C* CONTROLLA I CAMPI IMMESSI
018400991214     C                   EXSR      DECODI
018500020611     C*
018600991214     C* ESEGUE ROUTINE DI CONFERMA
018700020611     C     ERRL00        IFEQ      '0'
018800991214     C                   EXSR      ESEGUI
018900140214     C* il messaggio di file lo do sempre
019000121109     C                   MOVEL     MSG(20)       DSMSMS
019100121109     C* non esco mai dal pgm
019200121109     C***                MOVEL     '1'           FINL00
019300121108     C                   ENDIF
019400970526     C*
019500991214     C                   ENDSR
019600130109     C*------------------------------------------------------------------------*
019700130109     C* F12GES - GESTIONE F12 -> RITORNO
019800130109     C*------------------------------------------------------------------------*
019900130109     C     F12GES        BEGSR
020000130109     C*
020100130109     C* IMPOSTA "PREMUTO F12"
020200130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020300130109     C*
020400130109     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F24GES        BEGSR
020900970526     C*
021000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400140214     C* ESEGUI - scrittura FISIA00F
021500970526     C*------------------------------------------------------------------------*
021600991214     C     ESEGUI        BEGSR
021700141009     C*
021800141009     C                   IF        VIDTipA = 'RIC'
021900141009     C                   CLEAR                   DSIARIC
022000141009     C                   EVAL      §SIACHI = 'P'
022100141009     C                   EVAL      §SIADEV = DATCOR
022200141009     C                   EVAL      §SIAHEV = ORACOR
022300141009     C                   IF        VIDNote = *blanks
022400141009     C                   EVAL      VIDNote = DSIARIC
022500141009     C                   ENDIF
022600141009     C                   ENDIF
022700121108     C*
022800140214     C* preparo DS
022900140214     C                   EVAL      §SIAMSGAAS = VIDAAS
023000140214     C                   EVAL      §SIAMSGSMS = VIDCel
023100140214     C                   EVAL      §SIAMSGEML = VIDEmail
023200141009     C*
023300141009     C                   EVAL      §SIAFLOTIP = VIDFLO01
023400140214     C*
023500140214     C* scrivo FISIA00F
023600140214     C                   EVAL      SIADAORI   = %timestamp()
023700140214     C                   EVAL      SIAAAS     = 9999
023800140214     C                   EVAL      SIALNP     = VIDLNP
023900140214     C                   EVAL      SIANRS     = VIDNRS
024000140214     C                   EVAL      SIANSP     = VIDNSP
024100140214     C                   EVAL      SIATipA    = VIDTipA
024200140214     C                   EVAL      SIASTS     = '0'
024300140214     C                   EVAL      SIAMSG     = DSIAMSG
024400140214     C                   EVAL      SIAFLO     = DSIAFLO
024500140214     C                   EVAL      SIANOTE    = VIDNote
024600991214     C*
024700140214     C                   WRITE     FISIA000
024800140214     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025200970526     C*------------------------------------------------------------------------*
025300970526     C     DECODI        BEGSR
025400121019     C*
025500120927     C                   SETOFF                                       505152
025600120927     C                   SETOFF                                       535455
025700130123     C                   SETOFF                                       56
025800000306     C*
025900141015     C* anno/linea/spedizione/tipo sono obbligatori
026000141015     C                   IF        VIDAAS  = 0 or
026100141015     C                             VIDLNP  = 0 or
026200141015     C                             VIDNSP  = 0 or
026300141015     C                             VIDTipA = *blank
026400140214     C                   MOVEL     '1'           ERRL00
026500140214     C                   SETON                                        50
026600140214     C                   MOVEL     MSG(6)        DSMSMS
026700140214     C                   ENDIF
026800141015     C*
026900141015     C* email o cellulare sono obbligatori
027000141015     C                   IF        VIDEmail = *blank and
027100141015     C                             VIDCel   = *blank
027200141015     C                   MOVEL     '1'           ERRL00
027300141015     C                   SETON                                        50
027400141015     C                   MOVEL     MSG(6)        DSMSMS
027500141015     C                   ENDIF
027600120927     C*
027700970528     C                   ENDSR
027800970526     C*------------------------------------------------------------------------*
027900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
028000970526     C*------------------------------------------------------------------------*
028100970526     C     AZZVID        BEGSR
028200970526     C* CAMPI
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
028700970526     C*------------------------------------------------------------------------*
028800970526     C     IMPFUN        BEGSR
028900970526     C*
029000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
029100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
029200970526     C                   EXSR      FUNGES
029300970526     C*
029400970526     C                   ENDSR
029500020923     C*--------------------------------------------------------------------------------------------*
029600020923     C* REPERISCE I DATI UTENTE
029700020923     C*--------------------------------------------------------------------------------------------*
029800020923     C     REPDATIUTE    BEGSR
029900020923     C*
030000020923     C* INIZIALIZZA VARIABILI DI WRK
030100020923     C                   CLEAR                   TIBS34DS
030200020923     C                   CLEAR                   AZUTEDS
030300020923     C                   CLEAR                   DDATIUTE
030400020923     C*
030500020923     C     *DTAARA       DEFINE    §azute        azuteds
030600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
030700020923     C                   IN(E)     *DTAARA
030800020923if  1C                   IF        %Error
030900020923     c                   EVAL      I34Tla = 'L'
031000020923     C                   CALL      'TIBS34R'
031100020923     C                   PARM                    Tibs34Ds
031200020923     C                   IN        *DTAARA
031300020923e   1C                   ENDIF
031400020923     C*
031500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
031600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
031700020923     C*
031800020923     C                   ENDSR
031900970526     C*------------------------------------------------------------------------*
032000970526     C* *INZSR - OPERAZIONI INIZIALI
032100970526     C*------------------------------------------------------------------------*
032200970526     C     *INZSR        BEGSR
032300991217     C*
032400991217     C     *ENTRY        PLIST
032500991217     C                   PARM                    KPJBA
032600020923     C*
032700020923     C* REPERISCE I DATI DELL'UTENTE
032800020923     C                   EXSR      REPDATIUTE
032900970526     C*
033000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
033100970526     C     XMSGPA        PLIST
033200970526     C                   PARM                    DSMSMS
033300970526     C                   PARM                    DSMSPG
033400970526     C                   PARM                    DSMSKE
033500970526     C                   PARM                    DSMSER
033600970526     C*
033700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
033800970526     C                   CALL      'XKEY'
033900970526     C                   PARM                    DSKEY
034000970526     C*---
034100970526     C* VARIABILI NON RIFERITE AL DATA BASE
034200970526     C*---
034300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
034400991214     C                   MOVEL     *BLANKS       DEPOP1            3
034500970526     C*---
034600970526     C* VARIABILI DI CONTROLLO
034700970526     C*---
034800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
034900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
035000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
035100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
035200970526     C*
035300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
035400140214     C                   MOVEL     'UBTSTSIAR'   DSMSPG
035500140214     C                   MOVEL     'UBTSTSIAR'   MSGQUE
035600970526     C                   MOVEL     *BLANKS       MSGKEY
035700970526     C                   MOVEL     *BLANKS       DSMSMS
035800970528     C*
035900970528     C* CALCOLA LA DATA CORRENTE
036000141009     C                   time                    wn14             14 0
036100141009     C                   movel     wn14          ORACOR            4 0
036200140416     C                   Z-ADD     *zeros        DATCOR            8 0          *DATA CORRENTE AA/M/
036300140416     C                   EVAL      DATCOR = %dec(%date() : *ISO)
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
052700140214     C     FUNKEY        IFEQ      '05'
052800140214     C                   EXSR      F05GES
052900140214     C                   END
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
066500140214                                                                               2
066600130920                                                                               3
066700130108                                                                               4
066800130108                                                                               5
066900130108Campo obbligatorio                                                             6
067000130108                                                                               7
067100130108                                                                               8
067200120927                                                                               9
067300120927                                                                               10
067400120927                                                                               11
067500120927                                                                               12
067600120927                                                                               13
067700140214Data errata                                                                    14
067800140214                                                                               15
067900120927                                                                               16
068000120927                                                                               17
068100120927                                                                               18
068200120927                                                                               19
068300140214Scrittura effettuata                                                           20
068400120927Scelta obbligatoria                                                            21
068500140214                                                                               22
068600121108                                                                               23
068700121108                                                                               24
068800970526** FUD - FUNZIONI
068900970526                   -        (S/N)=N                        funz.01¬
069000970526                   -        (S/N)=N                        funz.02¬
069100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
069200970526                   -        (S/N)=N                        funz.04¬
069300140214F05=Pulizia video  -        (S/N)=S                        funz.05¬
069400991214F06=Conferma       -        (S/N)=S                        funz.06¬
069500130920                   -        (S/N)=N                        funz.07¬
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
