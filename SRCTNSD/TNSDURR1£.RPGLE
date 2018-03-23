000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150424     FTNSDURD1  cf   e             WORKSTN INFDS(DEVDS1)
000900150424     F                                     SFILE(SDURDMS:MSGULT)
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
006800120928     D*-------------------
006900120928
007000121003     d Status         sds
007100121003     d  SDSpgm           *proc
007200121003     d  SDSprm           *parms
007300121003     d  SDSuser              254    263
007400130116
007500150505     d Order           s              1
007600150505     d ora             s               t
007700140117     d wX              s              2s 0
007800120928
007900970526     C*------------------------------------------------------------------------*
008000000303     C* MAIN LINES
008100970526     C*------------------------------------------------------------------------*
008200970526     C*
008300120927     C* metto i default su D03
008400120927     C                   EXSR      InzD03
008500120927     C*
008600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009000970526     C*
009100970526     C* EMETTE FORMATO VIDEO
009200970526     C                   EXSR      BUTFMR
009300970526     C*
009400970526     C* GESTIONE TASTI FUNZIONALI
009500970526     C                   EXSR      FUNCON
009600970526E   2C                   ENDDO
009700970526     C*
009800970526     C* FINE PGM
009900970526     C                   SETON                                        LR
010000970526     C*------------------------------------------------------------------------*
010100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010200970526     C*------------------------------------------------------------------------*
010300970526     C     BUTFMR        BEGSR
010400970526     C*
010500970526     C* EMETTE LA TESTATA
010600150424     C                   WRITE     SDURD01
010700970526     C*
010800970526     C* SPEDISCE EVENTUALE MESSAGGIO
010900970526     C                   EXSR      SNDMSG
011000970526     C*
011100970526     C* EMETTE LE FUNZIONI
011200150424     C                   WRITE     SDURD02
011300970526     C*
011400970526     C* EMETTE IL VIDEO
011500150424     C                   EXFMT     SDURD03
011600970526     C*
011700991214     C* REIMPOSTA FLAG DI ERRORE
011800991214     C                   MOVEL     '0'           ERRL00
011900970526     C*
012000970526     C* REIMPOSTA INDICATORI DI ERRORE
012100020611     C                   SETOFF                                       555556
012200970526     C*
012300970526     C                   ENDSR
012400970526     C*------------------------------------------------------------------------*
012500120927     C* InzD03 - Inizializzo D03
012600970526     C*------------------------------------------------------------------------*
012700120927     C     InzD03        BEGSR
012800120927     C*
012900130108     C*
013000120927     C                   ENDSR
013100120927     C*------------------------------------------------------------------------*
013200120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013300120927     C*------------------------------------------------------------------------*
013400120927     C     SNDMSG        BEGSR
013500120927     C*
013600970526     C* PULISCE IL SFILE MESSAGGI
013700970526     C                   MOVEL     '1'           *IN40
013800150424     C                   WRITE     SDURDMC
013900970526     C                   MOVEL     '0'           *IN40
014000970526     C*
014100970526     C* VISUALIZZA IL MESSAGGIO
014200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014300970526     C                   CALL      'XMSG'        XMSGPA
014400020611     C     DSMSER        IFEQ      *ZEROS
014500970526     C                   MOVEL     DSMSKE        MSGKEY
014600020611     C                   ENDIF
014700970526     C                   Z-ADD     1             MSGULT
014800150424     C                   WRITE     SDURDMS
014900150424     C                   WRITE     SDURDMC
015000970526     C*
015100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015200970526     C                   MOVEL     *BLANKS       DSMSMS
015300970526     C*
015400970526     C                   ENDSR
015500970526     C*------------------------------------------------------------------------*
015600970526     C* ENTGES - GESTIONE ENTER -> INVIO
015700970526     C*------------------------------------------------------------------------*
015800970526     C     ENTGES        BEGSR
015900970526     C*
016000970526     C* CONTROLLA I CAMPI IMMESSI
016100970526     C                   EXSR      DECODI
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F03GES - GESTIONE F03 -> FINE
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F03GES        BEGSR
016800970526     C*
016900970526     C* IMPOSTA "PREMUTO F03"
017000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400150505     C* F06GES - GESTIONE F06 -> Conferma con Brand in ordinamento
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F06GES        BEGSR
017700970526     C*
017800991214     C* CONTROLLA I CAMPI IMMESSI
017900991214     C                   EXSR      DECODI
018000020611     C*
018100991214     C* ESEGUE ROUTINE DI CONFERMA
018200020611     C     ERRL00        IFEQ      '0'
018300150505     C                   EVAL      Order = '1'
018400150505     C                   EXSR      ESEGUI
018500130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
018600121109     C                   MOVEL     MSG(20)       DSMSMS
018700121109     C* non esco mai dal pgm
018800121109     C***                MOVEL     '1'           FINL00
018900121108     C                   ENDIF
019000970526     C*
019100991214     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300150505     C* F07GES - GESTIONE F07 -> Conferma con User in ordinamento
019400970526     C*------------------------------------------------------------------------*
019500130109     C     F07GES        BEGSR
019600970526     C*
019700150505     C* CONTROLLA I CAMPI IMMESSI
019800150505     C                   EXSR      DECODI
019900150505     C*
020000150505     C* ESEGUE ROUTINE DI CONFERMA
020100150505     C     ERRL00        IFEQ      '0'
020200150505     C                   EVAL      Order = '2'
020300150505     C                   EXSR      ESEGUI
020400150505     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
020500150505     C                   MOVEL     MSG(20)       DSMSMS
020600150505     C* non esco mai dal pgm
020700150505     C***                MOVEL     '1'           FINL00
020800150505     C                   ENDIF
020900130110     C*
021000970526     C                   ENDSR
021100130109     C*------------------------------------------------------------------------*
021200130109     C* F12GES - GESTIONE F12 -> RITORNO
021300130109     C*------------------------------------------------------------------------*
021400130109     C     F12GES        BEGSR
021500130109     C*
021600130109     C* IMPOSTA "PREMUTO F12"
021700130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021800130109     C*
021900130109     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F24GES        BEGSR
022400970526     C*
022500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022600970526     C*
022700970526     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
023000970526     C*------------------------------------------------------------------------*
023100991214     C     ESEGUI        BEGSR
023200140120     C*
023300121108     C* chiamo
023400150424     C                   CALL      'TNSDURR'
023500150505     C                   PARM                    Order
023600140120     C*
023700970526     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024000970526     C*------------------------------------------------------------------------*
024100970526     C     DECODI        BEGSR
024200121019     C*
024300120927     C                   SETOFF                                       505152
024400140120     c                   EVAL      VIDMSG    = *blank
024500120927     C*
024600970528     C                   ENDSR
024700970526     C*------------------------------------------------------------------------*
024800970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
024900970526     C*------------------------------------------------------------------------*
025000970526     C     AZZVID        BEGSR
025100970526     C* CAMPI
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
025600970526     C*------------------------------------------------------------------------*
025700970526     C     IMPFUN        BEGSR
025800970526     C*
025900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
026000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
026100970526     C                   EXSR      FUNGES
026200970526     C*
026300970526     C                   ENDSR
026400020923     C*--------------------------------------------------------------------------------------------*
026500020923     C* REPERISCE I DATI UTENTE
026600020923     C*--------------------------------------------------------------------------------------------*
026700020923     C     REPDATIUTE    BEGSR
026800020923     C*
026900020923     C* INIZIALIZZA VARIABILI DI WRK
027000020923     C                   CLEAR                   TIBS34DS
027100020923     C                   CLEAR                   AZUTEDS
027200020923     C                   CLEAR                   DDATIUTE
027300020923     C*
027400020923     C     *DTAARA       DEFINE    §azute        azuteds
027500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
027600020923     C                   IN(E)     *DTAARA
027700020923if  1C                   IF        %Error
027800020923     c                   EVAL      I34Tla = 'L'
027900020923     C                   CALL      'TIBS34R'
028000020923     C                   PARM                    Tibs34Ds
028100020923     C                   IN        *DTAARA
028200020923e   1C                   ENDIF
028300020923     C*
028400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
028500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
028600020923     C*
028700020923     C                   ENDSR
028800970526     C*------------------------------------------------------------------------*
028900970526     C* *INZSR - OPERAZIONI INIZIALI
029000970526     C*------------------------------------------------------------------------*
029100970526     C     *INZSR        BEGSR
029200991217     C*
029300991217     C     *ENTRY        PLIST
029400991217     C                   PARM                    KPJBA
029500020923     C*
029600020923     C* REPERISCE I DATI DELL'UTENTE
029700020923     C                   EXSR      REPDATIUTE
029800970526     C*
029900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
030000970526     C     XMSGPA        PLIST
030100970526     C                   PARM                    DSMSMS
030200970526     C                   PARM                    DSMSPG
030300970526     C                   PARM                    DSMSKE
030400970526     C                   PARM                    DSMSER
030500970526     C*
030600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
030700970526     C                   CALL      'XKEY'
030800970526     C                   PARM                    DSKEY
030900970526     C*---
031000970526     C* VARIABILI NON RIFERITE AL DATA BASE
031100970526     C*---
031200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
031300991214     C                   MOVEL     *BLANKS       DEPOP1            3
031400970526     C*---
031500970526     C* VARIABILI DI CONTROLLO
031600970526     C*---
031700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
031800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
031900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
032000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
032100970526     C*
032200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
032300150424     C                   MOVEL     'TNSDURR1'    DSMSPG
032400150424     C                   MOVEL     'TNSDURR1'    MSGQUE
032500970526     C                   MOVEL     *BLANKS       MSGKEY
032600970526     C                   MOVEL     *BLANKS       DSMSMS
032700970528     C*
032800970528     C* CALCOLA LA DATA CORRENTE
032900970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
033000970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
033100970528     C                   Z-ADD     WN8           G08DAT
033200970528     C                   Z-ADD     *ZEROS        G08INV
033300970528     C                   MOVEL     '0'           G08ERR
033400970528     C                   CALL      'XSRDA8'
033500970528     C                   PARM                    WLBDA8
033600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
033700000310     C*
033800000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
033900000310     C                   EXSR      AZZVID
034000970526     C*
034100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
034200970526     C                   EXSR      IMPFUN
034300970526     C*
034400970526     C                   ENDSR
034500970526     C*------------------------------------------------------------------------*
034600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
034700970526     C*------------------------------------------------------------------------*
034800970526     C     FUNGES        BEGSR
034900970526     C*
035000970526     C* PULISCO CAMPI DI LAVORO
035100970526     C                   Z-ADD     0             W                 5 0
035200970526     C*
035300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
035400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
035500970526     C     1             DO        24            I
035600970526     C                   MOVEL     FUD(I)        FUDRIG
035700970526     C*
035800970526     C* PULIZIA FUNZIONI PRECEDENTI
035900970526     C                   MOVEL     *BLANKS       FUV(I)
036000970526     C*
036100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
036200970526     C* (SALTA F24)
036300970526     C     FUDATT        IFEQ      'S'
036400970526     C     FUDFUN        ANDNE     '24'
036500970526     C                   ADD       1             W
036600970526     C                   MOVEL     FUDDES        FUV(W)
036700970526     C                   END
036800970526     C                   END                                                    DO WHILE
036900970526     C*
037000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
037100970526     C* ALTRIMENTI DISATTIVO F24
037200970526     C     1             DO        24            I
037300970526     C                   MOVEL     FUD(I)        FUDRIG
037400970526     C*
037500970526     C     FUDFUN        IFEQ      '24'
037600970526     C     W             IFGT      6
037700970526     C                   MOVEL     'S'           FUDATT
037800970526     C                   ELSE
037900970526     C                   MOVEL     *BLANKS       FUDATT
038000970526     C                   END                                                    W GT 8
038100970526     C                   MOVEL     FUDRIG        FUD(I)
038200970526     C                   END
038300970526     C                   END                                                    DO WHILE
038400970526     C*
038500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
038600970526     C     W             DIV       6             I
038700970526     C                   MVR                     Z
038800970526     C*
038900970526     C     Z             IFNE      0
039000970526     C     I             ADD       1             Z
039100970526     C                   ELSE
039200970526     C                   Z-ADD     I             Z                 5 0
039300970526     C                   END
039400970526     C*
039500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
039600970526     C                   ADD       1             FUNGRU
039700970526     C     FUNGRU        IFGT      Z
039800970526     C     FUNGRU        ORLT      1
039900970526     C                   Z-ADD     1             FUNGRU
040000970526     C                   END
040100970526     C*
040200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
040300970526     C     FUNGRU        IFEQ      1
040400970526     C                   MOVEL     FUV(1)        FUN001
040500970526     C                   MOVEL     FUV(2)        FUN002
040600970526     C                   MOVEL     FUV(3)        FUN003
040700970526     C                   MOVEL     FUV(4)        FUN004
040800970526     C                   MOVEL     FUV(5)        FUN005
040900970526     C                   MOVEL     FUV(6)        FUN006
041000970526     C                   END
041100970526     C*
041200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
041300970526     C     FUNGRU        IFEQ      2
041400970526     C                   MOVEL     FUV(7)        FUN001
041500970526     C                   MOVEL     FUV(8)        FUN002
041600970526     C                   MOVEL     FUV(9)        FUN003
041700970526     C                   MOVEL     FUV(10)       FUN004
041800970526     C                   MOVEL     FUV(11)       FUN005
041900970526     C                   MOVEL     FUV(12)       FUN006
042000970526     C                   END
042100970526     C*
042200970526     C* TERZO GRUPPO DI 6 FUNZIONI
042300970526     C     FUNGRU        IFEQ      3
042400970526     C                   MOVEL     FUV(13)       FUN001
042500970526     C                   MOVEL     FUV(14)       FUN002
042600970526     C                   MOVEL     FUV(15)       FUN003
042700970526     C                   MOVEL     FUV(16)       FUN004
042800970526     C                   MOVEL     FUV(17)       FUN005
042900970526     C                   MOVEL     FUV(18)       FUN006
043000970526     C                   END
043100970526     C*
043200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
043300970526     C     FUNGRU        IFEQ      4
043400970526     C                   MOVEL     FUV(19)       FUN001
043500970526     C                   MOVEL     FUV(20)       FUN002
043600970526     C                   MOVEL     FUV(21)       FUN003
043700970526     C                   MOVEL     FUV(22)       FUN004
043800970526     C                   MOVEL     *BLANKS       FUN005
043900970526     C                   MOVEL     *BLANKS       FUN006
044000970526     C                   END
044100970526     C*
044200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
044300970526     C                   MOVEL     *BLANKS       FUN007
044400970526     C                   MOVEL     *BLANKS       FUN008
044500970526     C*
044600970526     C     1             DO        24            I
044700970526     C                   MOVEL     FUD(I)        FUDRIG
044800970526     C     FUDFUN        IFEQ      '24'
044900970526     C     FUDATT        ANDEQ     'S'
045000970526     C                   MOVEL     FUDDES        FUN008
045100970526     C                   END
045200970526     C                   END                                                    DO
045300970526     C*
045400970526     C                   ENDSR
045500970526     C*------------------------------------------------------------------------*
045600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
045700970526     C*------------------------------------------------------------------------*
045800970526     C     FUNCON        BEGSR
045900970526     C*
046000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
046100970526     C                   EXSR      FUNVAR
046200970526     C*
046300970526     C* PULISCO CAMPI DI LAVORO
046400970526     C                   Z-ADD     0             I                 5 0
046500970526     C                   Z-ADD     0             W                 5 0
046600970526     C*
046700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
046800970526     C     FUNKEY        IFNE      *BLANKS
046900970526     C     1             DO        24            I
047000970526     C                   MOVEL     FUD(I)        FUDRIG
047100970526     C*
047200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
047300970526     C     FUNKEY        IFEQ      FUDFUN
047400970526     C     FUDATT        ANDEQ     'S'
047500970526     C                   Z-ADD     I             W
047600970526     C                   END
047700970526     C                   END                                                    DO WHILE
047800970526     C*
047900970526     C* FUNZIONE NON ATTIVA (ERRORE)
048000970526     C     W             IFEQ      0
048100970526     C*
048200970526     C* EMISSIONE MESSAGGIO DI ERRORE
048300970526     C                   MOVEL     MSG(1)        DSMSMS
048400970526     C*
048500970526     C                   END                                                    W=0
048600970526     C                   END                                                    FUNKEY NE *BLANKS
048700970526     C*
048800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
048900970526     C     W             IFNE      0
049000970526     C     FUNKEY        OREQ      *BLANKS
049100970526     C     FUNKEY        IFEQ      *BLANKS
049200970526     C                   EXSR      ENTGES
049300970526     C                   END
049400970526     C     FUNKEY        IFEQ      '03'
049500970526     C                   EXSR      F03GES
049600970526     C                   END
049700970526     C     FUNKEY        IFEQ      '06'
049800970526     C                   EXSR      F06GES
049900970526     C                   END
050000130109     C     FUNKEY        IFEQ      '07'
050100130109     C                   EXSR      F07GES
050200130109     C                   END
050300970526     C     FUNKEY        IFEQ      '12'
050400970526     C                   EXSR      F12GES
050500970526     C                   END
050600970526     C     FUNKEY        IFEQ      '24'
050700970526     C                   EXSR      F24GES
050800970526     C                   END
050900970526     C                   END
051000970526     C*
051100970526     C                   ENDSR
051200970526     C*------------------------------------------------------------------------*
051300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051400970526     C*------------------------------------------------------------------------*
051500970526     C     FUNVAR        BEGSR
051600970526     C*
051700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
051800970526     C*
051900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
052000970526     C     KEY           IFEQ      ENTER
052100970526     C                   MOVEL     *BLANKS       FUNKEY
052200970526     C                   END
052300970526     C     KEY           IFEQ      F1
052400970526     C                   MOVEL     '01'          FUNKEY
052500970526     C                   END
052600970526     C     KEY           IFEQ      F2
052700970526     C                   MOVEL     '02'          FUNKEY
052800970526     C                   END
052900970526     C     KEY           IFEQ      F3
053000970526     C                   MOVEL     '03'          FUNKEY
053100970526     C                   END
053200970526     C     KEY           IFEQ      F4
053300970526     C                   MOVEL     '04'          FUNKEY
053400970526     C                   END
053500970526     C     KEY           IFEQ      F5
053600970526     C                   MOVEL     '05'          FUNKEY
053700970526     C                   END
053800970526     C     KEY           IFEQ      F6
053900970526     C                   MOVEL     '06'          FUNKEY
054000970526     C                   END
054100970526     C     KEY           IFEQ      F7
054200970526     C                   MOVEL     '07'          FUNKEY
054300970526     C                   END
054400970526     C     KEY           IFEQ      F8
054500970526     C                   MOVEL     '08'          FUNKEY
054600970526     C                   END
054700970526     C     KEY           IFEQ      F9
054800970526     C                   MOVEL     '09'          FUNKEY
054900970526     C                   END
055000970526     C     KEY           IFEQ      F10
055100970526     C                   MOVEL     '10'          FUNKEY
055200970526     C                   END
055300970526     C     KEY           IFEQ      F11
055400970526     C                   MOVEL     '11'          FUNKEY
055500970526     C                   END
055600970526     C     KEY           IFEQ      F12
055700970526     C                   MOVEL     '12'          FUNKEY
055800970526     C                   END
055900970526     C     KEY           IFEQ      F13
056000970526     C                   MOVEL     '13'          FUNKEY
056100970526     C                   END
056200970526     C     KEY           IFEQ      F14
056300970526     C                   MOVEL     '14'          FUNKEY
056400970526     C                   END
056500970526     C     KEY           IFEQ      F15
056600970526     C                   MOVEL     '15'          FUNKEY
056700970526     C                   END
056800970526     C     KEY           IFEQ      F16
056900970526     C                   MOVEL     '16'          FUNKEY
057000970526     C                   END
057100970526     C     KEY           IFEQ      F17
057200970526     C                   MOVEL     '17'          FUNKEY
057300970526     C                   END
057400970526     C     KEY           IFEQ      F18
057500970526     C                   MOVEL     '18'          FUNKEY
057600970526     C                   END
057700970526     C     KEY           IFEQ      F19
057800970526     C                   MOVEL     '19'          FUNKEY
057900970526     C                   END
058000970526     C     KEY           IFEQ      F20
058100970526     C                   MOVEL     '20'          FUNKEY
058200970526     C                   END
058300970526     C     KEY           IFEQ      F21
058400970526     C                   MOVEL     '21'          FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F22
058700970526     C                   MOVEL     '22'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      F23
059000970526     C                   MOVEL     '23'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      F24
059300970526     C                   MOVEL     '24'          FUNKEY
059400970526     C                   END
059500970526     C     KEY           IFEQ      ROLLUP
059600970526     C                   MOVEL     'RU'          FUNKEY
059700970526     C                   END
059800970526     C     KEY           IFEQ      HELP
059900970526     C                   MOVEL     'HP'          FUNKEY
060000970526     C                   END
060100970526     C                   ENDSR
060200970526     C*------------------------------------------------------------------------*
060300970526     C* NOTE COSTRUZIONE SCHIERE
060400970526     C*------------------------------------------------------------------------*
060500970526     C*
060600970526     C* -MSG-MESSAGGI
060700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060800970526     C* (NON TOCCARE MSG,1 E MSG,2)
060900970526     C*
061000970526     C* -CMD-COMANDI
061100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061200970526     C*
061300970526     C* -OPD-OPZIONI
061400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
061500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
061600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
061700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
062000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
062200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
062300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
062400970526     C*
062500970526     C* -FUD-FUNZIONI
062600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
062700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
063000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
063100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
063200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
063300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
063400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
063500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
063600970526** MSG - MESSAGGI
063700120927Tasto funzionale non gestito                                                   1
063800150424                                                                               2
063900150424                                                                               3
064000140819                                                                               4
064100130108                                                                               5
064200130108Campo obbligatorio                                                             6
064300130108                                                                               7
064400130108                                                                               8
064500120927                                                                               9
064600120927                                                                               10
064700120927                                                                               11
064800120927                                                                               12
064900120927                                                                               13
065000120927Data errata                                                                    14
065100140116                                                                               15
065200120927                                                                               16
065300120927                                                                               17
065400120927                                                                               18
065500120927                                                                               19
065600150424Aggiornamento concluso                                                         20
065700150424                                                                               21
065800150424                                                                               22
065900121108                                                                               23
066000121108                                                                               24
066100970526** FUD - FUNZIONI
066200970526                   -        (S/N)=N                        funz.01¬
066300970526                   -        (S/N)=N                        funz.02¬
066400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
066500970526                   -        (S/N)=N                        funz.04¬
066600970526                   -        (S/N)=N                        funz.05¬
066700150505F06=Conf. + Brand  -        (S/N)=S                        funz.06¬
066800150505F07=Conf. + User   -        (S/N)=S                        funz.07¬
066900970527                   -        (S/N)=N                        funz.08¬
067000970527                   -        (S/N)=N                        funz.09¬
067100970527                   -        (S/N)=N                        funz.10¬
067200970526                   -        (S/N)=N                        funz.11¬
067300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
067400970526                   -        (S/N)=N                        funz.13¬
067500970526                   -        (S/N)=N                        funz.14¬
067600970526                   -        (S/N)=N                        funz.15¬
067700970526                   -        (S/N)=N                        funz.16¬
067800970526                   -        (S/N)=N                        funz.17¬
067900970526                   -        (S/N)=N                        funz.18¬
068000970526                   -        (S/N)=N                        funz.19¬
068100970526                   -        (S/N)=N                        funz.20¬
068200970526                   -        (S/N)=N                        funz.21¬
068300970526                   -        (S/N)=N                        funz.22¬
068400970526                   -        (S/N)=N                        funz.23¬
068500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
