000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150121     FUBTL05D1  cf   e             WORKSTN INFDS(DEVDS1)
000900150121     F                                     SFILE(TL05S01:SFLULT)
001000150121     F                                     SFILE(TL05DMS:MSGULT)
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
002500970526     D*------------------------------------------------------------------------*
002600970526     D* INPUT
002700970526     D*------------------------------------------------------------------------*
002800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002900970526     D*------------
003000970526     D FUDDS           DS
003100970526     D  FUDDE1                 1      1
003200970526     D  FUDFUN                 2      3
003300970526     D  FUDDE2                 4     19
003400970526     D  FUDFI0                20     34
003500970526     D  FUDATT                35     35
003600970526     D  FUDFI1                36     80
003700970526     D  FUDDES                 1     19
003800970526     D  FUDRIG                 1     80
003900970526     D*------------
004000970526     D* DS GESTIONE TASTI FUNZIONALI
004100970526     D*------------
004200970526     D DSKEY         E DS
004300970526     D DEVDS1          DS
004400970526     D  KEY                  369    369
004500970526     D*------------
004600970526     D* DS "XMSG" - GESTIONE MESSAGGI
004700970526     D*------------
004800970526     D DSMSG         E DS
004900970528     D*------------------
005000970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100970528     D*------------------
005200970528     D WLBDA8          DS                  INZ
005300970528     D  G08DAT                 1      8  0
005400970528     D  G08INV                 9     16  0
005500970528     D  G08ERR                17     17
005600970528     D  G08TGI                18     22  0
005700970526     D*------------
005800970526     D* ARCHITETTURA
005900970526     D*------------
006000970526     D KPJBA         E DS
006100020923     D*-------------------
006200020923     D* DS REPERIMENTO DATI UTENTE
006300020923     D*-------------------
006400020923     D TIBS34DS      E DS                                                       *Profili utente
006500020923     D DDATIUTE      E DS                                                       *Dati utente
006600020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700120927     D* DS REPERIMENTO PADRE/FIGLI
006800120927     D TIBS10DS      E DS
006900120928     D*-------------------
007000120928
007100121003     d Status         sds
007200121003     d  SDSpgm           *proc
007300121003     d  SDSprm           *parms
007400121003     d  SDSuser              254    263
007500121003
007600150121     D*
007700150121     D   pInNazione    S              3A
007800150121     D   pInProvincia  S              4A
007900150121     D   pInComune     S            128A
008000150121     D   pInLocalita   S            128A
008100150121     D   pInCAP        S              9A
008200150121     D   pInVia        S            128A
008300150121     D   pInCivico     S             10A
008400150121     D   pInScore      S             10A
008500150121     D   pOutEsito     S              1A
008600150121     D   pOutEsitoDes  S            128A
008700150121     D   pOutTotFound  S              4S 0
008800150121     D   pOutResults   DS                  dim(10) likeds(dsOutUBTL05R02)       10 * 630 della DS
008900150121
009000150121     D* campi appoggio SFL
009100150121     D SFLULT          S                   like(SFLNBR)
009200130116
009300130116     d ora             s               t
009400140117     d wX              s              2s 0
009500150121     d wI              s              3s 0
009600150121
009700150121 CPY  /COPY GAITRASRC/SRCUB,UBTL05D02
009800120928
009900970526     C*------------------------------------------------------------------------*
010000000303     C* MAIN LINES
010100970526     C*------------------------------------------------------------------------*
010200970526     C*
010300150121     C* metto i default su C01
010400150121     C                   EXSR      InzC01
010500120927     C*
010600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011000970526     C*
011100970526     C* EMETTE FORMATO VIDEO
011200970526     C                   EXSR      BUTFMR
011300970526     C*
011400970526     C* GESTIONE TASTI FUNZIONALI
011500970526     C                   EXSR      FUNCON
011600150121     C                   ENDDO
011700970526     C*
011800970526     C* FINE PGM
011900970526     C                   SETON                                        LR
012000970526     C*------------------------------------------------------------------------*
012100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012200970526     C*------------------------------------------------------------------------*
012300970526     C     BUTFMR        BEGSR
012400970526     C*
012500970526     C* EMETTE LA TESTATA
012600150121     C                   WRITE     TL05D01
012700970526     C*
012800970526     C* SPEDISCE EVENTUALE MESSAGGIO
012900970526     C                   EXSR      SNDMSG
013000970526     C*
013100970526     C* EMETTE LE FUNZIONI
013200150121     C                   WRITE     TL05D02
013300970526     C*
013400150122     C* EMETTE IL SFLCTL
013500150122     C                   EXFMT     TL05C01
013600970526     C*
013700991214     C* REIMPOSTA FLAG DI ERRORE
013800991214     C                   MOVEL     '0'           ERRL00
013900970526     C*
014000970526     C* REIMPOSTA INDICATORI DI ERRORE
014100020611     C                   SETOFF                                       555556
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500150121     C* InzC01 - Inizializzo C01
014600970526     C*------------------------------------------------------------------------*
014700150121     C     InzC01        BEGSR
014800120927     C*
014900150122     C*   tolgo il SFL (N30) e lo pulisco (N31)
015000150122     C                   SETOFF                                         3031
015100150122     C                   WRITE     TL05C01
015200150122     C*   lascio tolto il SFL ma riattivo il SFLCTL (31)
015300150122     C                   SETON                                          31
015400130108     C*
015500120927     C                   ENDSR
015600120927     C*------------------------------------------------------------------------*
015700120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015800120927     C*------------------------------------------------------------------------*
015900120927     C     SNDMSG        BEGSR
016000120927     C*
016100970526     C* PULISCE IL SFILE MESSAGGI
016200970526     C                   MOVEL     '1'           *IN40
016300150121     C                   WRITE     TL05DMC
016400970526     C                   MOVEL     '0'           *IN40
016500970526     C*
016600970526     C* VISUALIZZA IL MESSAGGIO
016700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016800970526     C                   CALL      'XMSG'        XMSGPA
016900020611     C     DSMSER        IFEQ      *ZEROS
017000970526     C                   MOVEL     DSMSKE        MSGKEY
017100020611     C                   ENDIF
017200970526     C                   Z-ADD     1             MSGULT
017300150121     C                   WRITE     TL05DMS
017400150121     C                   WRITE     TL05DMC
017500970526     C*
017600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017700970526     C                   MOVEL     *BLANKS       DSMSMS
017800970526     C*
017900970526     C                   ENDSR
018000970526     C*------------------------------------------------------------------------*
018100970526     C* ENTGES - GESTIONE ENTER -> INVIO
018200970526     C*------------------------------------------------------------------------*
018300970526     C     ENTGES        BEGSR
018400970526     C*
018500970526     C* CONTROLLA I CAMPI IMMESSI
018600970526     C                   EXSR      DECODI
018700970526     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* F03GES - GESTIONE F03 -> FINE
019100970526     C*------------------------------------------------------------------------*
019200970526     C     F03GES        BEGSR
019300970526     C*
019400970526     C* IMPOSTA "PREMUTO F03"
019500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019600970526     C*
019700970526     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900991214     C* F06GES - GESTIONE F06 -> CONFERMA
020000970526     C*------------------------------------------------------------------------*
020100970526     C     F06GES        BEGSR
020200970526     C*
020300991214     C* CONTROLLA I CAMPI IMMESSI
020400991214     C                   EXSR      DECODI
020500020611     C*
020600991214     C* ESEGUE ROUTINE DI CONFERMA
020700020611     C     ERRL00        IFEQ      '0'
020800991214     C                   EXSR      ESEGUI
020900150121     C* il messaggio di elaborazione conclusa lo do sempre, a prescindere dagli esiti
021000121109     C                   MOVEL     MSG(20)       DSMSMS
021100121109     C* non esco mai dal pgm
021200121109     C***                MOVEL     '1'           FINL00
021300121108     C                   ENDIF
021400970526     C*
021500991214     C                   ENDSR
021600130109     C*------------------------------------------------------------------------*
021700130109     C* F12GES - GESTIONE F12 -> RITORNO
021800130109     C*------------------------------------------------------------------------*
021900130109     C     F12GES        BEGSR
022000130109     C*
022100130109     C* IMPOSTA "PREMUTO F12"
022200130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022300130109     C*
022400130109     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F24GES        BEGSR
022900970526     C*
023000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
023500970526     C*------------------------------------------------------------------------*
023600991214     C     ESEGUI        BEGSR
023700140120     C*
023800150121     C* valorizzo i dati per chiamare il pgm da testare
023900150121     C                   EVAL      pInNazione   = 'it'
024000150121     C                   EVAL      pInProvincia = C01Prv
024100150121     C                   EVAL      pInComune    = C01Com
024200150121     C                   EVAL      pInLocalita  = *blanks
024300150121     C                   EVAL      pInCAP       = C01CAP
024400150121     C                   EVAL      pInVia       = C01Ind
024500150121     C                   EVAL      pInCivico    = *blanks
024600150121     C                   EVAL      pInScore     = C01Sco
024700121108     C* chiamo
024800150121     C                   CALL      'UBTL05R02'
024900150121     C                   PARM                    pInNazione
025000150121     C                   PARM                    pInProvincia
025100150121     C                   PARM                    pInComune
025200150121     C                   PARM                    pInLocalita
025300150121     C                   PARM                    pInCAP
025400150121     C                   PARM                    pInVia
025500150121     C                   PARM                    pInCivico
025600150121     C                   PARM                    pInScore
025700150121     C                   PARM                    pOutEsito
025800150121     C                   PARM                    pOutEsitoDes
025900150121     C                   PARM                    pOutTotFound
026000150121     C                   PARM                    pOutResults
026100141113
026200150121     C* carico il SFL con i dati ricevuti
026300150121     C                   EXSR      CarSFL
026400150121
026500970526     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026800970526     C*------------------------------------------------------------------------*
026900970526     C     DECODI        BEGSR
027000121019     C*
027100120927     C                   SETOFF                                       505152
027200121001     C*
027300150127     C* Provincia e comune obbligatori
027400150127     C     ERRL00        IFEQ      '0'
027500150127     C                   IF        C01Prv = *blank or
027600150127     C                             C01Com = *blank
027700150127     C                   MOVEL     '1'           ERRL00
027800150127     C                   SETON                                        51
027900150127     C                   MOVEL     MSG(3)        DSMSMS
028000150127     C                   ENDIF
028100150127     C                   ENDIF
028200120927     C*
028300970528     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
028600970526     C*------------------------------------------------------------------------*
028700970526     C     AZZVID        BEGSR
028800970526     C* CAMPI
028900970526     C*
029000970526     C                   ENDSR
029100150121     C*------------------------------------------------------------------------*
029200150121     C* CARSFL - CARICAMENTO SFL
029300150121     C*------------------------------------------------------------------------*
029400150121     C     CARSFL        BEGSR
029500150121     C*
029600150121     C* pulizia SFL
029700150121     C                   SETOFF                                         3031
029800150121     C                   WRITE     TL05C01
029900150121     C                   SETON                                          31
030000150122     C                   EVAL      SFLULT = 0
030100150122     C                   SETOFF                                       33
030200150121     C*
030300150121     C* CICLO per caricare tutte le righe che mi sono arrivate (al max sono 10)
030400150122     C                   FOR       wI = 1 to pOutTotFound
030500150121     C*
030600150122     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' pOutTotFound > 0
030700150121     C                   EVAL      S01Prv = pOutResults(wI).Prov
030800150121     C                   EVAL      S01Com = pOutResults(wI).Comune
030900150121     C                   EVAL      S01Loc = pOutResults(wI).Localit
031000150121     C                   EVAL      S01CAP = pOutResults(wI).CAP
031100150121     C                   EVAL      S01Ind = pOutResults(wI).Via
031200150121     C                   EVAL      S01Civ = pOutResults(wI).Civico
031300150121     C                   EVAL      S01Reg = pOutResults(wI).Regione
031400150121     C                   EVAL      S01Liv = pOutResults(wI).LivNorm
031500150121     C                   EVAL      S01Sco = pOutResults(wI).Score
031600150121     C*
031700150121     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
031800150121     C                   ADD       1             SFLULT
031900150121     C*
032000150121     C* SCRITTURA RECORD SFL
032100150121     C                   WRITE     TL05S01
032200150122     C* attivo SFL (30)
032300150122     C                   SETON                                        30
032400150122     C                   EVAL      SFLNBR = 1
032500150121
032600150121     C                   ENDFOR
032700150122     C*
032800150122     C* se ho meno o uguale a 8 righe sono rimasto nella prima pagina
032900150122     C                   IF        SflUlt < 9
033000150122     C                   SETON                                        33
033100150122     C                   ENDIF
033200150121     C*
033300150121     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
033600970526     C*------------------------------------------------------------------------*
033700970526     C     IMPFUN        BEGSR
033800970526     C*
033900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
034000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
034100970526     C                   EXSR      FUNGES
034200970526     C*
034300970526     C                   ENDSR
034400020923     C*--------------------------------------------------------------------------------------------*
034500020923     C* REPERISCE I DATI UTENTE
034600020923     C*--------------------------------------------------------------------------------------------*
034700020923     C     REPDATIUTE    BEGSR
034800020923     C*
034900020923     C* INIZIALIZZA VARIABILI DI WRK
035000020923     C                   CLEAR                   TIBS34DS
035100020923     C                   CLEAR                   AZUTEDS
035200020923     C                   CLEAR                   DDATIUTE
035300020923     C*
035400020923     C     *DTAARA       DEFINE    §azute        azuteds
035500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
035600020923     C                   IN(E)     *DTAARA
035700020923if  1C                   IF        %Error
035800020923     c                   EVAL      I34Tla = 'L'
035900020923     C                   CALL      'TIBS34R'
036000020923     C                   PARM                    Tibs34Ds
036100020923     C                   IN        *DTAARA
036200020923e   1C                   ENDIF
036300020923     C*
036400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
036500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
036600020923     C*
036700020923     C                   ENDSR
036800970526     C*------------------------------------------------------------------------*
036900970526     C* *INZSR - OPERAZIONI INIZIALI
037000970526     C*------------------------------------------------------------------------*
037100970526     C     *INZSR        BEGSR
037200991217     C*
037300991217     C     *ENTRY        PLIST
037400991217     C                   PARM                    KPJBA
037500020923     C*
037600020923     C* REPERISCE I DATI DELL'UTENTE
037700020923     C                   EXSR      REPDATIUTE
037800970526     C*
037900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
038000970526     C     XMSGPA        PLIST
038100970526     C                   PARM                    DSMSMS
038200970526     C                   PARM                    DSMSPG
038300970526     C                   PARM                    DSMSKE
038400970526     C                   PARM                    DSMSER
038500970526     C*
038600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
038700970526     C                   CALL      'XKEY'
038800970526     C                   PARM                    DSKEY
038900970526     C*---
039000970526     C* VARIABILI NON RIFERITE AL DATA BASE
039100970526     C*---
039200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
039300991214     C                   MOVEL     *BLANKS       DEPOP1            3
039400970526     C*---
039500970526     C* VARIABILI DI CONTROLLO
039600970526     C*---
039700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
039800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
039900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
040000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
040100970526     C*
040200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
040300150121     C                   MOVEL     'UBTL05R1'    DSMSPG
040400150121     C                   MOVEL     'UBTL05R1'    MSGQUE
040500970526     C                   MOVEL     *BLANKS       MSGKEY
040600970526     C                   MOVEL     *BLANKS       DSMSMS
040700970528     C*
040800970528     C* CALCOLA LA DATA CORRENTE
040900970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
041000970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
041100970528     C                   Z-ADD     WN8           G08DAT
041200970528     C                   Z-ADD     *ZEROS        G08INV
041300970528     C                   MOVEL     '0'           G08ERR
041400970528     C                   CALL      'XSRDA8'
041500970528     C                   PARM                    WLBDA8
041600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
041700000310     C*
041800000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
041900000310     C                   EXSR      AZZVID
042000970526     C*
042100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
042200970526     C                   EXSR      IMPFUN
042300970526     C*
042400970526     C                   ENDSR
042500970526     C*------------------------------------------------------------------------*
042600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
042700970526     C*------------------------------------------------------------------------*
042800970526     C     FUNGES        BEGSR
042900970526     C*
043000970526     C* PULISCO CAMPI DI LAVORO
043100970526     C                   Z-ADD     0             W                 5 0
043200970526     C*
043300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
043400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
043500970526     C     1             DO        24            I
043600970526     C                   MOVEL     FUD(I)        FUDRIG
043700970526     C*
043800970526     C* PULIZIA FUNZIONI PRECEDENTI
043900970526     C                   MOVEL     *BLANKS       FUV(I)
044000970526     C*
044100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
044200970526     C* (SALTA F24)
044300970526     C     FUDATT        IFEQ      'S'
044400970526     C     FUDFUN        ANDNE     '24'
044500970526     C                   ADD       1             W
044600970526     C                   MOVEL     FUDDES        FUV(W)
044700970526     C                   END
044800970526     C                   END                                                    DO WHILE
044900970526     C*
045000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
045100970526     C* ALTRIMENTI DISATTIVO F24
045200970526     C     1             DO        24            I
045300970526     C                   MOVEL     FUD(I)        FUDRIG
045400970526     C*
045500970526     C     FUDFUN        IFEQ      '24'
045600970526     C     W             IFGT      6
045700970526     C                   MOVEL     'S'           FUDATT
045800970526     C                   ELSE
045900970526     C                   MOVEL     *BLANKS       FUDATT
046000970526     C                   END                                                    W GT 8
046100970526     C                   MOVEL     FUDRIG        FUD(I)
046200970526     C                   END
046300970526     C                   END                                                    DO WHILE
046400970526     C*
046500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
046600970526     C     W             DIV       6             I
046700970526     C                   MVR                     Z
046800970526     C*
046900970526     C     Z             IFNE      0
047000970526     C     I             ADD       1             Z
047100970526     C                   ELSE
047200970526     C                   Z-ADD     I             Z                 5 0
047300970526     C                   END
047400970526     C*
047500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
047600970526     C                   ADD       1             FUNGRU
047700970526     C     FUNGRU        IFGT      Z
047800970526     C     FUNGRU        ORLT      1
047900970526     C                   Z-ADD     1             FUNGRU
048000970526     C                   END
048100970526     C*
048200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
048300970526     C     FUNGRU        IFEQ      1
048400970526     C                   MOVEL     FUV(1)        FUN001
048500970526     C                   MOVEL     FUV(2)        FUN002
048600970526     C                   MOVEL     FUV(3)        FUN003
048700970526     C                   MOVEL     FUV(4)        FUN004
048800970526     C                   MOVEL     FUV(5)        FUN005
048900970526     C                   MOVEL     FUV(6)        FUN006
049000970526     C                   END
049100970526     C*
049200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
049300970526     C     FUNGRU        IFEQ      2
049400970526     C                   MOVEL     FUV(7)        FUN001
049500970526     C                   MOVEL     FUV(8)        FUN002
049600970526     C                   MOVEL     FUV(9)        FUN003
049700970526     C                   MOVEL     FUV(10)       FUN004
049800970526     C                   MOVEL     FUV(11)       FUN005
049900970526     C                   MOVEL     FUV(12)       FUN006
050000970526     C                   END
050100970526     C*
050200970526     C* TERZO GRUPPO DI 6 FUNZIONI
050300970526     C     FUNGRU        IFEQ      3
050400970526     C                   MOVEL     FUV(13)       FUN001
050500970526     C                   MOVEL     FUV(14)       FUN002
050600970526     C                   MOVEL     FUV(15)       FUN003
050700970526     C                   MOVEL     FUV(16)       FUN004
050800970526     C                   MOVEL     FUV(17)       FUN005
050900970526     C                   MOVEL     FUV(18)       FUN006
051000970526     C                   END
051100970526     C*
051200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
051300970526     C     FUNGRU        IFEQ      4
051400970526     C                   MOVEL     FUV(19)       FUN001
051500970526     C                   MOVEL     FUV(20)       FUN002
051600970526     C                   MOVEL     FUV(21)       FUN003
051700970526     C                   MOVEL     FUV(22)       FUN004
051800970526     C                   MOVEL     *BLANKS       FUN005
051900970526     C                   MOVEL     *BLANKS       FUN006
052000970526     C                   END
052100970526     C*
052200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
052300970526     C                   MOVEL     *BLANKS       FUN007
052400970526     C                   MOVEL     *BLANKS       FUN008
052500970526     C*
052600970526     C     1             DO        24            I
052700970526     C                   MOVEL     FUD(I)        FUDRIG
052800970526     C     FUDFUN        IFEQ      '24'
052900970526     C     FUDATT        ANDEQ     'S'
053000970526     C                   MOVEL     FUDDES        FUN008
053100970526     C                   END
053200970526     C                   END                                                    DO
053300970526     C*
053400970526     C                   ENDSR
053500970526     C*------------------------------------------------------------------------*
053600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
053700970526     C*------------------------------------------------------------------------*
053800970526     C     FUNCON        BEGSR
053900970526     C*
054000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
054100970526     C                   EXSR      FUNVAR
054200970526     C*
054300970526     C* PULISCO CAMPI DI LAVORO
054400970526     C                   Z-ADD     0             I                 5 0
054500970526     C                   Z-ADD     0             W                 5 0
054600970526     C*
054700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
054800970526     C     FUNKEY        IFNE      *BLANKS
054900970526     C     1             DO        24            I
055000970526     C                   MOVEL     FUD(I)        FUDRIG
055100970526     C*
055200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
055300970526     C     FUNKEY        IFEQ      FUDFUN
055400970526     C     FUDATT        ANDEQ     'S'
055500970526     C                   Z-ADD     I             W
055600970526     C                   END
055700970526     C                   END                                                    DO WHILE
055800970526     C*
055900970526     C* FUNZIONE NON ATTIVA (ERRORE)
056000970526     C     W             IFEQ      0
056100970526     C*
056200970526     C* EMISSIONE MESSAGGIO DI ERRORE
056300970526     C                   MOVEL     MSG(1)        DSMSMS
056400970526     C*
056500970526     C                   END                                                    W=0
056600970526     C                   END                                                    FUNKEY NE *BLANKS
056700970526     C*
056800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
056900970526     C     W             IFNE      0
057000970526     C     FUNKEY        OREQ      *BLANKS
057100970526     C     FUNKEY        IFEQ      *BLANKS
057200970526     C                   EXSR      ENTGES
057300970526     C                   END
057400970526     C     FUNKEY        IFEQ      '03'
057500970526     C                   EXSR      F03GES
057600970526     C                   END
057700970526     C     FUNKEY        IFEQ      '06'
057800970526     C                   EXSR      F06GES
057900970526     C                   END
058000970526     C     FUNKEY        IFEQ      '12'
058100970526     C                   EXSR      F12GES
058200970526     C                   END
058300970526     C     FUNKEY        IFEQ      '24'
058400970526     C                   EXSR      F24GES
058500970526     C                   END
058600970526     C                   END
058700970526     C*
058800970526     C                   ENDSR
058900970526     C*------------------------------------------------------------------------*
059000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
059100970526     C*------------------------------------------------------------------------*
059200970526     C     FUNVAR        BEGSR
059300970526     C*
059400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
059500970526     C*
059600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
059700970526     C     KEY           IFEQ      ENTER
059800970526     C                   MOVEL     *BLANKS       FUNKEY
059900970526     C                   END
060000970526     C     KEY           IFEQ      F1
060100970526     C                   MOVEL     '01'          FUNKEY
060200970526     C                   END
060300970526     C     KEY           IFEQ      F2
060400970526     C                   MOVEL     '02'          FUNKEY
060500970526     C                   END
060600970526     C     KEY           IFEQ      F3
060700970526     C                   MOVEL     '03'          FUNKEY
060800970526     C                   END
060900970526     C     KEY           IFEQ      F4
061000970526     C                   MOVEL     '04'          FUNKEY
061100970526     C                   END
061200970526     C     KEY           IFEQ      F5
061300970526     C                   MOVEL     '05'          FUNKEY
061400970526     C                   END
061500970526     C     KEY           IFEQ      F6
061600970526     C                   MOVEL     '06'          FUNKEY
061700970526     C                   END
061800970526     C     KEY           IFEQ      F7
061900970526     C                   MOVEL     '07'          FUNKEY
062000970526     C                   END
062100970526     C     KEY           IFEQ      F8
062200970526     C                   MOVEL     '08'          FUNKEY
062300970526     C                   END
062400970526     C     KEY           IFEQ      F9
062500970526     C                   MOVEL     '09'          FUNKEY
062600970526     C                   END
062700970526     C     KEY           IFEQ      F10
062800970526     C                   MOVEL     '10'          FUNKEY
062900970526     C                   END
063000970526     C     KEY           IFEQ      F11
063100970526     C                   MOVEL     '11'          FUNKEY
063200970526     C                   END
063300970526     C     KEY           IFEQ      F12
063400970526     C                   MOVEL     '12'          FUNKEY
063500970526     C                   END
063600970526     C     KEY           IFEQ      F13
063700970526     C                   MOVEL     '13'          FUNKEY
063800970526     C                   END
063900970526     C     KEY           IFEQ      F14
064000970526     C                   MOVEL     '14'          FUNKEY
064100970526     C                   END
064200970526     C     KEY           IFEQ      F15
064300970526     C                   MOVEL     '15'          FUNKEY
064400970526     C                   END
064500970526     C     KEY           IFEQ      F16
064600970526     C                   MOVEL     '16'          FUNKEY
064700970526     C                   END
064800970526     C     KEY           IFEQ      F17
064900970526     C                   MOVEL     '17'          FUNKEY
065000970526     C                   END
065100970526     C     KEY           IFEQ      F18
065200970526     C                   MOVEL     '18'          FUNKEY
065300970526     C                   END
065400970526     C     KEY           IFEQ      F19
065500970526     C                   MOVEL     '19'          FUNKEY
065600970526     C                   END
065700970526     C     KEY           IFEQ      F20
065800970526     C                   MOVEL     '20'          FUNKEY
065900970526     C                   END
066000970526     C     KEY           IFEQ      F21
066100970526     C                   MOVEL     '21'          FUNKEY
066200970526     C                   END
066300970526     C     KEY           IFEQ      F22
066400970526     C                   MOVEL     '22'          FUNKEY
066500970526     C                   END
066600970526     C     KEY           IFEQ      F23
066700970526     C                   MOVEL     '23'          FUNKEY
066800970526     C                   END
066900970526     C     KEY           IFEQ      F24
067000970526     C                   MOVEL     '24'          FUNKEY
067100970526     C                   END
067200970526     C     KEY           IFEQ      ROLLUP
067300970526     C                   MOVEL     'RU'          FUNKEY
067400970526     C                   END
067500970526     C     KEY           IFEQ      HELP
067600970526     C                   MOVEL     'HP'          FUNKEY
067700970526     C                   END
067800970526     C                   ENDSR
067900970526     C*------------------------------------------------------------------------*
068000970526     C* NOTE COSTRUZIONE SCHIERE
068100970526     C*------------------------------------------------------------------------*
068200970526     C*
068300970526     C* -MSG-MESSAGGI
068400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
068500970526     C* (NON TOCCARE MSG,1 E MSG,2)
068600970526     C*
068700970526     C* -CMD-COMANDI
068800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
068900970526     C*
069000970526     C* -OPD-OPZIONI
069100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
069200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
069300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
069400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
069500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
069600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
069700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
069900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
070000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
070100970526     C*
070200970526     C* -FUD-FUNZIONI
070300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
070400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
070700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
070800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
070900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
071000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
071100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
071200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
071300970526** MSG - MESSAGGI
071400120927Tasto funzionale non gestito                                                   1
071500150121                                                                               2
071600150127Provincia e località obbligatori                                               3
071700140819                                                                               4
071800130108                                                                               5
071900150121                                                                               6
072000130108                                                                               7
072100130108                                                                               8
072200120927                                                                               9
072300120927                                                                               10
072400120927                                                                               11
072500120927                                                                               12
072600120927                                                                               13
072700120927Data errata                                                                    14
072800140116                                                                               15
072900120927                                                                               16
073000120927                                                                               17
073100120927                                                                               18
073200120927                                                                               19
073300150121Test concluso - vedi SFL                                                       20
073400150121                                                                               21
073500150121                                                                               22
073600121108                                                                               23
073700121108                                                                               24
073800970526** FUD - FUNZIONI
073900970526                   -        (S/N)=N                        funz.01¬
074000970526                   -        (S/N)=N                        funz.02¬
074100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
074200970526                   -        (S/N)=N                        funz.04¬
074300970526                   -        (S/N)=N                        funz.05¬
074400991214F06=Conferma       -        (S/N)=S                        funz.06¬
074500140819                   -        (S/N)=N                        funz.07¬
074600970527                   -        (S/N)=N                        funz.08¬
074700970527                   -        (S/N)=N                        funz.09¬
074800970527                   -        (S/N)=N                        funz.10¬
074900970526                   -        (S/N)=N                        funz.11¬
075000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
075100970526                   -        (S/N)=N                        funz.13¬
075200970526                   -        (S/N)=N                        funz.14¬
075300970526                   -        (S/N)=N                        funz.15¬
075400970526                   -        (S/N)=N                        funz.16¬
075500970526                   -        (S/N)=N                        funz.17¬
075600970526                   -        (S/N)=N                        funz.18¬
075700970526                   -        (S/N)=N                        funz.19¬
075800970526                   -        (S/N)=N                        funz.20¬
075900970526                   -        (S/N)=N                        funz.21¬
076000970526                   -        (S/N)=N                        funz.22¬
076100970526                   -        (S/N)=N                        funz.23¬
076200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
