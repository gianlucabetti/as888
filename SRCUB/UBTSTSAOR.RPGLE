000100970526     H*------------------------------------------------------------------------*
000200140416     H* Scrittura FISAO00F per test
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800140416     FUBTSTSAOD cf   e             WORKSTN INFDS(DEVDS1)
000900140416     F                                     SFILE(TSTSAODMS:MSGULT)
001000140416     FFISAO00F  if A e             DISK
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
002600140416     D* DS di suddivisione campi FISAO
002700130920     D*------------
002800140416     D DSAOMSG       E DS
002900140416     D DSAOFLO       E DS
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
006200020923     D DDATIUTE      E DS                                                       *Dati utente
006300020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006400120928     D*-------------------
006500120928
006600140416
006700970526     C*------------------------------------------------------------------------*
006800000303     C* MAIN LINES
006900970526     C*------------------------------------------------------------------------*
007000970526     C*
007100120927     C* metto i default su D03
007200120927     C                   EXSR      InzD03
007300120927     C*
007400120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007500991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
007600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
007700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
007800970526     C*
007900970526     C* EMETTE FORMATO VIDEO
008000970526     C                   EXSR      BUTFMR
008100970526     C*
008200970526     C* GESTIONE TASTI FUNZIONALI
008300970526     C                   EXSR      FUNCON
008400970526E   2C                   ENDDO
008500970526     C*
008600970526     C* FINE PGM
008700970526     C                   SETON                                        LR
008800970526     C*------------------------------------------------------------------------*
008900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009000970526     C*------------------------------------------------------------------------*
009100970526     C     BUTFMR        BEGSR
009200970526     C*
009300970526     C* EMETTE LA TESTATA
009400140416     C                   WRITE     TSTSAOD01
009500970526     C*
009600970526     C* SPEDISCE EVENTUALE MESSAGGIO
009700970526     C                   EXSR      SNDMSG
009800970526     C*
009900970526     C* EMETTE LE FUNZIONI
010000140416     C                   WRITE     TSTSAOD02
010100970526     C*
010200970526     C* EMETTE IL VIDEO
010300140416     C                   EXFMT     TSTSAOD03
010400970526     C*
010500991214     C* REIMPOSTA FLAG DI ERRORE
010600991214     C                   MOVEL     '0'           ERRL00
010700970526     C*
010800970526     C* REIMPOSTA INDICATORI DI ERRORE
010900020611     C                   SETOFF                                       555556
011000970526     C*
011100970526     C                   ENDSR
011200970526     C*------------------------------------------------------------------------*
011300120927     C* InzD03 - Inizializzo D03
011400970526     C*------------------------------------------------------------------------*
011500120927     C     InzD03        BEGSR
011600120927     C*
011700130108     C*
011800120927     C                   ENDSR
011900120927     C*------------------------------------------------------------------------*
012000120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012100120927     C*------------------------------------------------------------------------*
012200120927     C     SNDMSG        BEGSR
012300120927     C*
012400970526     C* PULISCE IL SFILE MESSAGGI
012500970526     C                   MOVEL     '1'           *IN40
012600140416     C                   WRITE     TSTSAODMC
012700970526     C                   MOVEL     '0'           *IN40
012800970526     C*
012900970526     C* VISUALIZZA IL MESSAGGIO
013000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
013100970526     C                   CALL      'XMSG'        XMSGPA
013200020611     C     DSMSER        IFEQ      *ZEROS
013300970526     C                   MOVEL     DSMSKE        MSGKEY
013400020611     C                   ENDIF
013500970526     C                   Z-ADD     1             MSGULT
013600140416     C                   WRITE     TSTSAODMS
013700140416     C                   WRITE     TSTSAODMC
013800970526     C*
013900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014000970526     C                   MOVEL     *BLANKS       DSMSMS
014100970526     C*
014200970526     C                   ENDSR
014300970526     C*------------------------------------------------------------------------*
014400970526     C* ENTGES - GESTIONE ENTER -> INVIO
014500970526     C*------------------------------------------------------------------------*
014600970526     C     ENTGES        BEGSR
014700970526     C*
014800970526     C* CONTROLLA I CAMPI IMMESSI
014900970526     C                   EXSR      DECODI
015000970526     C*
015100970526     C                   ENDSR
015200970526     C*------------------------------------------------------------------------*
015300970526     C* F03GES - GESTIONE F03 -> FINE
015400970526     C*------------------------------------------------------------------------*
015500970526     C     F03GES        BEGSR
015600970526     C*
015700970526     C* IMPOSTA "PREMUTO F03"
015800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015900970526     C*
016000970526     C                   ENDSR
016100140214     C*------------------------------------------------------------------------*
016200140214     C* F05GES - GESTIONE F05 -> Pulizia video
016300140214     C*------------------------------------------------------------------------*
016400140214     C     F05GES        BEGSR
016500140214     C*
016600140214     C* pulisco i campi di input
016700140416     C                   EVAL      VIDPOE = 0
016800140416     C                   EVAL      VIDNSR = 0
016900140416     C                   EVAL      VIDNOR = 0
017000140416     C                   EVAL      VIDNRV = 0
017100140214     C                   EVAL      VIDTipA = *blank
017200140214     C                   EVAL      VIDEmail = *blank
017300140214     C                   EVAL      VIDCel = *blank
017400140214     C                   EVAL      VIDNOTE = *blank
017500140214     C*
017600140214     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800991214     C* F06GES - GESTIONE F06 -> CONFERMA
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F06GES        BEGSR
018100970526     C*
018200991214     C* CONTROLLA I CAMPI IMMESSI
018300991214     C                   EXSR      DECODI
018400020611     C*
018500991214     C* ESEGUE ROUTINE DI CONFERMA
018600020611     C     ERRL00        IFEQ      '0'
018700991214     C                   EXSR      ESEGUI
018800140214     C* il messaggio di file lo do sempre
018900121109     C                   MOVEL     MSG(20)       DSMSMS
019000121109     C* non esco mai dal pgm
019100121109     C***                MOVEL     '1'           FINL00
019200121108     C                   ENDIF
019300970526     C*
019400991214     C                   ENDSR
019500130109     C*------------------------------------------------------------------------*
019600130109     C* F12GES - GESTIONE F12 -> RITORNO
019700130109     C*------------------------------------------------------------------------*
019800130109     C     F12GES        BEGSR
019900130109     C*
020000130109     C* IMPOSTA "PREMUTO F12"
020100130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020200130109     C*
020300130109     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F24GES        BEGSR
020800970526     C*
020900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300140416     C* ESEGUI - scrittura FISAO00F
021400970526     C*------------------------------------------------------------------------*
021500991214     C     ESEGUI        BEGSR
021600121108     C*
021700140214     C* preparo DS
021800140416     C                   EVAL      §SAOMSGPOE = VIDPOE
021900140416     C                   EVAL      §SAOMSGSMS = VIDCel
022000140416     C                   EVAL      §SAOMSGEML = VIDEmail
022100140214
022200140416     C                   EVAL      §SAOFLOTIP = 'T'
022300140214     C*
022400140416     C* scrivo FISAO00F
022500140416     C                   EVAL      SAODAORI   = %timestamp()
022600140416     C                   EVAL      SAOPOE     = 999
022700140416     C                   EVAL      SAONSR     = VIDNSR
022800140416     C                   EVAL      SAONOR     = VIDNOR
022900140416     C                   EVAL      SAONRV     = VIDNRV
023000140416     C                   EVAL      SAOTipA    = VIDTipA
023100140416     C                   EVAL      SAOSTS     = '0'
023200140416     C                   EVAL      SAOMSG     = DSAOMSG
023300140416     C                   EVAL      SAOFLO     = DSAOFLO
023400140416     C                   EVAL      SAONOTE    = VIDNote
023500991214     C*
023600140416     C                   WRITE     FISAO000
023700140214     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024100970526     C*------------------------------------------------------------------------*
024200970526     C     DECODI        BEGSR
024300121019     C*
024400120927     C                   SETOFF                                       505152
024500120927     C                   SETOFF                                       535455
024600130123     C                   SETOFF                                       56
024700000306     C*
024800140214     C* anno/linea/spedizione/email/cellulare sono obbligatori
024900140416     C                   IF        VIDPOE = 0 or
025000140416     C                             VIDNOR = 0 or
025100140214     C                             VIDTipA = *blank or
025200140418     C                             (VIDEmail = *blank and
025300140418     C                             VIDCel = *blank)
025400140214     C                   MOVEL     '1'           ERRL00
025500140214     C                   SETON                                        50
025600140214     C                   MOVEL     MSG(6)        DSMSMS
025700140214     C                   ENDIF
025800120927     C*
025900970528     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
026200970526     C*------------------------------------------------------------------------*
026300970526     C     AZZVID        BEGSR
026400970526     C* CAMPI
026500970526     C*
026600970526     C                   ENDSR
026700970526     C*------------------------------------------------------------------------*
026800020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
026900970526     C*------------------------------------------------------------------------*
027000970526     C     IMPFUN        BEGSR
027100970526     C*
027200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
027300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
027400970526     C                   EXSR      FUNGES
027500970526     C*
027600970526     C                   ENDSR
027700020923     C*--------------------------------------------------------------------------------------------*
027800020923     C* REPERISCE I DATI UTENTE
027900020923     C*--------------------------------------------------------------------------------------------*
028000020923     C     REPDATIUTE    BEGSR
028100020923     C*
028200020923     C* INIZIALIZZA VARIABILI DI WRK
028300020923     C                   CLEAR                   TIBS34DS
028400020923     C                   CLEAR                   AZUTEDS
028500020923     C                   CLEAR                   DDATIUTE
028600020923     C*
028700020923     C     *DTAARA       DEFINE    §azute        azuteds
028800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
028900020923     C                   IN(E)     *DTAARA
029000020923if  1C                   IF        %Error
029100020923     c                   EVAL      I34Tla = 'L'
029200020923     C                   CALL      'TIBS34R'
029300020923     C                   PARM                    Tibs34Ds
029400020923     C                   IN        *DTAARA
029500020923e   1C                   ENDIF
029600020923     C*
029700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
029800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
029900020923     C*
030000020923     C                   ENDSR
030100970526     C*------------------------------------------------------------------------*
030200970526     C* *INZSR - OPERAZIONI INIZIALI
030300970526     C*------------------------------------------------------------------------*
030400970526     C     *INZSR        BEGSR
030500991217     C*
030600991217     C     *ENTRY        PLIST
030700991217     C                   PARM                    KPJBA
030800020923     C*
030900020923     C* REPERISCE I DATI DELL'UTENTE
031000020923     C                   EXSR      REPDATIUTE
031100970526     C*
031200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
031300970526     C     XMSGPA        PLIST
031400970526     C                   PARM                    DSMSMS
031500970526     C                   PARM                    DSMSPG
031600970526     C                   PARM                    DSMSKE
031700970526     C                   PARM                    DSMSER
031800970526     C*
031900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
032000970526     C                   CALL      'XKEY'
032100970526     C                   PARM                    DSKEY
032200970526     C*---
032300970526     C* VARIABILI NON RIFERITE AL DATA BASE
032400970526     C*---
032500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
032600991214     C                   MOVEL     *BLANKS       DEPOP1            3
032700970526     C*---
032800970526     C* VARIABILI DI CONTROLLO
032900970526     C*---
033000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
033100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
033200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
033300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
033400970526     C*
033500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
033600140416     C                   MOVEL     'UBTSTSAOR'   DSMSPG
033700140416     C                   MOVEL     'UBTSTSAOR'   MSGQUE
033800970526     C                   MOVEL     *BLANKS       MSGKEY
033900970526     C                   MOVEL     *BLANKS       DSMSMS
034000970528     C*
034100970528     C* CALCOLA LA DATA CORRENTE
034200140416     C                   Z-ADD     *zeros        DATCOR            8 0          *DATA CORRENTE AA/M/
034300140416     C                   EVAL      DATCOR = %dec(%date() : *ISO)
034400000310     C*
034500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
034600000310     C                   EXSR      AZZVID
034700970526     C*
034800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
034900970526     C                   EXSR      IMPFUN
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
035400970526     C*------------------------------------------------------------------------*
035500970526     C     FUNGES        BEGSR
035600970526     C*
035700970526     C* PULISCO CAMPI DI LAVORO
035800970526     C                   Z-ADD     0             W                 5 0
035900970526     C*
036000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
036100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
036200970526     C     1             DO        24            I
036300970526     C                   MOVEL     FUD(I)        FUDRIG
036400970526     C*
036500970526     C* PULIZIA FUNZIONI PRECEDENTI
036600970526     C                   MOVEL     *BLANKS       FUV(I)
036700970526     C*
036800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
036900970526     C* (SALTA F24)
037000970526     C     FUDATT        IFEQ      'S'
037100970526     C     FUDFUN        ANDNE     '24'
037200970526     C                   ADD       1             W
037300970526     C                   MOVEL     FUDDES        FUV(W)
037400970526     C                   END
037500970526     C                   END                                                    DO WHILE
037600970526     C*
037700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
037800970526     C* ALTRIMENTI DISATTIVO F24
037900970526     C     1             DO        24            I
038000970526     C                   MOVEL     FUD(I)        FUDRIG
038100970526     C*
038200970526     C     FUDFUN        IFEQ      '24'
038300970526     C     W             IFGT      6
038400970526     C                   MOVEL     'S'           FUDATT
038500970526     C                   ELSE
038600970526     C                   MOVEL     *BLANKS       FUDATT
038700970526     C                   END                                                    W GT 8
038800970526     C                   MOVEL     FUDRIG        FUD(I)
038900970526     C                   END
039000970526     C                   END                                                    DO WHILE
039100970526     C*
039200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
039300970526     C     W             DIV       6             I
039400970526     C                   MVR                     Z
039500970526     C*
039600970526     C     Z             IFNE      0
039700970526     C     I             ADD       1             Z
039800970526     C                   ELSE
039900970526     C                   Z-ADD     I             Z                 5 0
040000970526     C                   END
040100970526     C*
040200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
040300970526     C                   ADD       1             FUNGRU
040400970526     C     FUNGRU        IFGT      Z
040500970526     C     FUNGRU        ORLT      1
040600970526     C                   Z-ADD     1             FUNGRU
040700970526     C                   END
040800970526     C*
040900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
041000970526     C     FUNGRU        IFEQ      1
041100970526     C                   MOVEL     FUV(1)        FUN001
041200970526     C                   MOVEL     FUV(2)        FUN002
041300970526     C                   MOVEL     FUV(3)        FUN003
041400970526     C                   MOVEL     FUV(4)        FUN004
041500970526     C                   MOVEL     FUV(5)        FUN005
041600970526     C                   MOVEL     FUV(6)        FUN006
041700970526     C                   END
041800970526     C*
041900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
042000970526     C     FUNGRU        IFEQ      2
042100970526     C                   MOVEL     FUV(7)        FUN001
042200970526     C                   MOVEL     FUV(8)        FUN002
042300970526     C                   MOVEL     FUV(9)        FUN003
042400970526     C                   MOVEL     FUV(10)       FUN004
042500970526     C                   MOVEL     FUV(11)       FUN005
042600970526     C                   MOVEL     FUV(12)       FUN006
042700970526     C                   END
042800970526     C*
042900970526     C* TERZO GRUPPO DI 6 FUNZIONI
043000970526     C     FUNGRU        IFEQ      3
043100970526     C                   MOVEL     FUV(13)       FUN001
043200970526     C                   MOVEL     FUV(14)       FUN002
043300970526     C                   MOVEL     FUV(15)       FUN003
043400970526     C                   MOVEL     FUV(16)       FUN004
043500970526     C                   MOVEL     FUV(17)       FUN005
043600970526     C                   MOVEL     FUV(18)       FUN006
043700970526     C                   END
043800970526     C*
043900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
044000970526     C     FUNGRU        IFEQ      4
044100970526     C                   MOVEL     FUV(19)       FUN001
044200970526     C                   MOVEL     FUV(20)       FUN002
044300970526     C                   MOVEL     FUV(21)       FUN003
044400970526     C                   MOVEL     FUV(22)       FUN004
044500970526     C                   MOVEL     *BLANKS       FUN005
044600970526     C                   MOVEL     *BLANKS       FUN006
044700970526     C                   END
044800970526     C*
044900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
045000970526     C                   MOVEL     *BLANKS       FUN007
045100970526     C                   MOVEL     *BLANKS       FUN008
045200970526     C*
045300970526     C     1             DO        24            I
045400970526     C                   MOVEL     FUD(I)        FUDRIG
045500970526     C     FUDFUN        IFEQ      '24'
045600970526     C     FUDATT        ANDEQ     'S'
045700970526     C                   MOVEL     FUDDES        FUN008
045800970526     C                   END
045900970526     C                   END                                                    DO
046000970526     C*
046100970526     C                   ENDSR
046200970526     C*------------------------------------------------------------------------*
046300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
046400970526     C*------------------------------------------------------------------------*
046500970526     C     FUNCON        BEGSR
046600970526     C*
046700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
046800970526     C                   EXSR      FUNVAR
046900970526     C*
047000970526     C* PULISCO CAMPI DI LAVORO
047100970526     C                   Z-ADD     0             I                 5 0
047200970526     C                   Z-ADD     0             W                 5 0
047300970526     C*
047400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
047500970526     C     FUNKEY        IFNE      *BLANKS
047600970526     C     1             DO        24            I
047700970526     C                   MOVEL     FUD(I)        FUDRIG
047800970526     C*
047900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
048000970526     C     FUNKEY        IFEQ      FUDFUN
048100970526     C     FUDATT        ANDEQ     'S'
048200970526     C                   Z-ADD     I             W
048300970526     C                   END
048400970526     C                   END                                                    DO WHILE
048500970526     C*
048600970526     C* FUNZIONE NON ATTIVA (ERRORE)
048700970526     C     W             IFEQ      0
048800970526     C*
048900970526     C* EMISSIONE MESSAGGIO DI ERRORE
049000970526     C                   MOVEL     MSG(1)        DSMSMS
049100970526     C*
049200970526     C                   END                                                    W=0
049300970526     C                   END                                                    FUNKEY NE *BLANKS
049400970526     C*
049500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
049600970526     C     W             IFNE      0
049700970526     C     FUNKEY        OREQ      *BLANKS
049800970526     C     FUNKEY        IFEQ      *BLANKS
049900970526     C                   EXSR      ENTGES
050000970526     C                   END
050100970526     C     FUNKEY        IFEQ      '03'
050200970526     C                   EXSR      F03GES
050300970526     C                   END
050400970526     C     FUNKEY        IFEQ      '06'
050500970526     C                   EXSR      F06GES
050600970526     C                   END
050700140214     C     FUNKEY        IFEQ      '05'
050800140214     C                   EXSR      F05GES
050900140214     C                   END
051000970526     C     FUNKEY        IFEQ      '12'
051100970526     C                   EXSR      F12GES
051200970526     C                   END
051300970526     C     FUNKEY        IFEQ      '24'
051400970526     C                   EXSR      F24GES
051500970526     C                   END
051600970526     C                   END
051700970526     C*
051800970526     C                   ENDSR
051900970526     C*------------------------------------------------------------------------*
052000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
052100970526     C*------------------------------------------------------------------------*
052200970526     C     FUNVAR        BEGSR
052300970526     C*
052400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
052500970526     C*
052600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
052700970526     C     KEY           IFEQ      ENTER
052800970526     C                   MOVEL     *BLANKS       FUNKEY
052900970526     C                   END
053000970526     C     KEY           IFEQ      F1
053100970526     C                   MOVEL     '01'          FUNKEY
053200970526     C                   END
053300970526     C     KEY           IFEQ      F2
053400970526     C                   MOVEL     '02'          FUNKEY
053500970526     C                   END
053600970526     C     KEY           IFEQ      F3
053700970526     C                   MOVEL     '03'          FUNKEY
053800970526     C                   END
053900970526     C     KEY           IFEQ      F4
054000970526     C                   MOVEL     '04'          FUNKEY
054100970526     C                   END
054200970526     C     KEY           IFEQ      F5
054300970526     C                   MOVEL     '05'          FUNKEY
054400970526     C                   END
054500970526     C     KEY           IFEQ      F6
054600970526     C                   MOVEL     '06'          FUNKEY
054700970526     C                   END
054800970526     C     KEY           IFEQ      F7
054900970526     C                   MOVEL     '07'          FUNKEY
055000970526     C                   END
055100970526     C     KEY           IFEQ      F8
055200970526     C                   MOVEL     '08'          FUNKEY
055300970526     C                   END
055400970526     C     KEY           IFEQ      F9
055500970526     C                   MOVEL     '09'          FUNKEY
055600970526     C                   END
055700970526     C     KEY           IFEQ      F10
055800970526     C                   MOVEL     '10'          FUNKEY
055900970526     C                   END
056000970526     C     KEY           IFEQ      F11
056100970526     C                   MOVEL     '11'          FUNKEY
056200970526     C                   END
056300970526     C     KEY           IFEQ      F12
056400970526     C                   MOVEL     '12'          FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      F13
056700970526     C                   MOVEL     '13'          FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F14
057000970526     C                   MOVEL     '14'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F15
057300970526     C                   MOVEL     '15'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F16
057600970526     C                   MOVEL     '16'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F17
057900970526     C                   MOVEL     '17'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F18
058200970526     C                   MOVEL     '18'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F19
058500970526     C                   MOVEL     '19'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F20
058800970526     C                   MOVEL     '20'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F21
059100970526     C                   MOVEL     '21'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F22
059400970526     C                   MOVEL     '22'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F23
059700970526     C                   MOVEL     '23'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F24
060000970526     C                   MOVEL     '24'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      ROLLUP
060300970526     C                   MOVEL     'RU'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      HELP
060600970526     C                   MOVEL     'HP'          FUNKEY
060700970526     C                   END
060800970526     C                   ENDSR
060900970526     C*------------------------------------------------------------------------*
061000970526     C* NOTE COSTRUZIONE SCHIERE
061100970526     C*------------------------------------------------------------------------*
061200970526     C*
061300970526     C* -MSG-MESSAGGI
061400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
061500970526     C* (NON TOCCARE MSG,1 E MSG,2)
061600970526     C*
061700970526     C* -CMD-COMANDI
061800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061900970526     C*
062000970526     C* -OPD-OPZIONI
062100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
062200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
062300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
062400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
062700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
062900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
063000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
063100970526     C*
063200970526     C* -FUD-FUNZIONI
063300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
063400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
063500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
063600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
063700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
063800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
063900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
064000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
064100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
064200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
064300970526** MSG - MESSAGGI
064400120927Tasto funzionale non gestito                                                   1
064500140214                                                                               2
064600130920                                                                               3
064700130108                                                                               4
064800130108                                                                               5
064900130108Campo obbligatorio                                                             6
065000130108                                                                               7
065100130108                                                                               8
065200120927                                                                               9
065300120927                                                                               10
065400120927                                                                               11
065500120927                                                                               12
065600120927                                                                               13
065700140214Data errata                                                                    14
065800140214                                                                               15
065900120927                                                                               16
066000120927                                                                               17
066100120927                                                                               18
066200120927                                                                               19
066300140214Scrittura effettuata                                                           20
066400120927Scelta obbligatoria                                                            21
066500140214                                                                               22
066600121108                                                                               23
066700121108                                                                               24
066800970526** FUD - FUNZIONI
066900970526                   -        (S/N)=N                        funz.01¬
067000970526                   -        (S/N)=N                        funz.02¬
067100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
067200970526                   -        (S/N)=N                        funz.04¬
067300140214F05=Pulizia video  -        (S/N)=S                        funz.05¬
067400991214F06=Conferma       -        (S/N)=S                        funz.06¬
067500130920                   -        (S/N)=N                        funz.07¬
067600970527                   -        (S/N)=N                        funz.08¬
067700970527                   -        (S/N)=N                        funz.09¬
067800970527                   -        (S/N)=N                        funz.10¬
067900970526                   -        (S/N)=N                        funz.11¬
068000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
068100970526                   -        (S/N)=N                        funz.13¬
068200970526                   -        (S/N)=N                        funz.14¬
068300970526                   -        (S/N)=N                        funz.15¬
068400970526                   -        (S/N)=N                        funz.16¬
068500970526                   -        (S/N)=N                        funz.17¬
068600970526                   -        (S/N)=N                        funz.18¬
068700970526                   -        (S/N)=N                        funz.19¬
068800970526                   -        (S/N)=N                        funz.20¬
068900970526                   -        (S/N)=N                        funz.21¬
069000970526                   -        (S/N)=N                        funz.22¬
069100970526                   -        (S/N)=N                        funz.23¬
069200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
