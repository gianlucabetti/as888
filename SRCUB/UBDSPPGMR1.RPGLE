000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800140819     FUBDSPPGMD1cf   e             WORKSTN INFDS(DEVDS1)
000900140819     F                                     SFILE(DSPPGMDMS:MSGULT)
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
007400121003
007500140116     d pIn_Lib         s             10a
007600140819     d pIn_CLR         s              1a
007700140819     d pOut_RcvVar     s            500a
007800140819     d pOut_Esito      s              2  0
007900130116
008000130116     d ora             s               t
008100140117     d wX              s              2s 0
008200120928
008300970526     C*------------------------------------------------------------------------*
008400000303     C* MAIN LINES
008500970526     C*------------------------------------------------------------------------*
008600970526     C*
008700120927     C* metto i default su D03
008800120927     C                   EXSR      InzD03
008900120927     C*
009000120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009100991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009400970526     C*
009500970526     C* EMETTE FORMATO VIDEO
009600970526     C                   EXSR      BUTFMR
009700970526     C*
009800970526     C* GESTIONE TASTI FUNZIONALI
009900970526     C                   EXSR      FUNCON
010000970526E   2C                   ENDDO
010100970526     C*
010200970526     C* FINE PGM
010300970526     C                   SETON                                        LR
010400970526     C*------------------------------------------------------------------------*
010500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010600970526     C*------------------------------------------------------------------------*
010700970526     C     BUTFMR        BEGSR
010800970526     C*
010900970526     C* EMETTE LA TESTATA
011000140819     C                   WRITE     DSPPGMD01
011100970526     C*
011200970526     C* SPEDISCE EVENTUALE MESSAGGIO
011300970526     C                   EXSR      SNDMSG
011400970526     C*
011500970526     C* EMETTE LE FUNZIONI
011600140819     C                   WRITE     DSPPGMD02
011700970526     C*
011800970526     C* EMETTE IL VIDEO
011900140819     C                   EXFMT     DSPPGMD03
012000970526     C*
012100991214     C* REIMPOSTA FLAG DI ERRORE
012200991214     C                   MOVEL     '0'           ERRL00
012300970526     C*
012400970526     C* REIMPOSTA INDICATORI DI ERRORE
012500020611     C                   SETOFF                                       555556
012600970526     C*
012700970526     C                   ENDSR
012800970526     C*------------------------------------------------------------------------*
012900120927     C* InzD03 - Inizializzo D03
013000970526     C*------------------------------------------------------------------------*
013100120927     C     InzD03        BEGSR
013200120927     C*
013300130108     C*
013400120927     C                   ENDSR
013500120927     C*------------------------------------------------------------------------*
013600120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013700120927     C*------------------------------------------------------------------------*
013800120927     C     SNDMSG        BEGSR
013900120927     C*
014000970526     C* PULISCE IL SFILE MESSAGGI
014100970526     C                   MOVEL     '1'           *IN40
014200140819     C                   WRITE     DSPPGMDMC
014300970526     C                   MOVEL     '0'           *IN40
014400970526     C*
014500970526     C* VISUALIZZA IL MESSAGGIO
014600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014700970526     C                   CALL      'XMSG'        XMSGPA
014800020611     C     DSMSER        IFEQ      *ZEROS
014900970526     C                   MOVEL     DSMSKE        MSGKEY
015000020611     C                   ENDIF
015100970526     C                   Z-ADD     1             MSGULT
015200140819     C                   WRITE     DSPPGMDMS
015300140819     C                   WRITE     DSPPGMDMC
015400970526     C*
015500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015600970526     C                   MOVEL     *BLANKS       DSMSMS
015700970526     C*
015800970526     C                   ENDSR
015900970526     C*------------------------------------------------------------------------*
016000970526     C* ENTGES - GESTIONE ENTER -> INVIO
016100970526     C*------------------------------------------------------------------------*
016200970526     C     ENTGES        BEGSR
016300970526     C*
016400970526     C* CONTROLLA I CAMPI IMMESSI
016500970526     C                   EXSR      DECODI
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F03GES - GESTIONE F03 -> FINE
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F03GES        BEGSR
017200970526     C*
017300970526     C* IMPOSTA "PREMUTO F03"
017400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500970526     C*
017600970526     C                   ENDSR
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
018800130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
018900121109     C                   MOVEL     MSG(20)       DSMSMS
019000121109     C* non esco mai dal pgm
019100121109     C***                MOVEL     '1'           FINL00
019200121108     C                   ENDIF
019300970526     C*
019400991214     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600130109     C* F07GES - GESTIONE F07 -> STAMPA
019700970526     C*------------------------------------------------------------------------*
019800130109     C     F07GES        BEGSR
019900970526     C*
020000140818     C                   EXSR      F06GES
020100130110     C*
020200970526     C                   ENDSR
020300130109     C*------------------------------------------------------------------------*
020400130109     C* F12GES - GESTIONE F12 -> RITORNO
020500130109     C*------------------------------------------------------------------------*
020600130109     C     F12GES        BEGSR
020700130109     C*
020800130109     C* IMPOSTA "PREMUTO F12"
020900130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021000130109     C*
021100130109     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F24GES        BEGSR
021600970526     C*
021700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
022200970526     C*------------------------------------------------------------------------*
022300991214     C     ESEGUI        BEGSR
022400140120     C*
022500140120     C* creo il file temporaneo con elenco dei membri
022600140116     C                   EVAL      pIn_Lib     = VIDLib
022700140819     C                   EVAL      pIn_CLR     = VIDCLR
022800140819     c                   clear                   pOut_RcvVar
022900140819     c                   clear                   pOut_Esito
023000121108     C* chiamo
023100140819     C                   CALL      'UBDSPPGMC2'
023200140117     c                   parm                    pIn_Lib
023300140819     c                   parm                    pIn_CLR
023400140819     c                   parm                    pOut_RcvVar
023500140819     c                   parm                    pOut_Esito
023600121108     C*
023700140109     c                   EVAL      VIDEsito = pOut_Esito
023800140120     C*
023900970526     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024200970526     C*------------------------------------------------------------------------*
024300970526     C     DECODI        BEGSR
024400121019     C*
024500120927     C                   SETOFF                                       505152
024600140120     c                   EVAL      VIDMSG    = *blank
024700121001     C*
024800140819     C* Libreria obbligatoria
024900121001     C     ERRL00        IFEQ      '0'
025000140819     C                   IF        VIDLib = *blank
025100120927     C                   MOVEL     '1'           ERRL00
025200130108     C                   SETON                                        51
025300120927     C                   MOVEL     MSG(3)        DSMSMS
025400120927     C                   ENDIF
025500140116     C                   ENDIF
025600120927     C*
025700970528     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
026000970526     C*------------------------------------------------------------------------*
026100970526     C     AZZVID        BEGSR
026200970526     C* CAMPI
026300970526     C*
026400970526     C                   ENDSR
026500970526     C*------------------------------------------------------------------------*
026600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
026700970526     C*------------------------------------------------------------------------*
026800970526     C     IMPFUN        BEGSR
026900970526     C*
027000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
027100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
027200970526     C                   EXSR      FUNGES
027300970526     C*
027400970526     C                   ENDSR
027500020923     C*--------------------------------------------------------------------------------------------*
027600020923     C* REPERISCE I DATI UTENTE
027700020923     C*--------------------------------------------------------------------------------------------*
027800020923     C     REPDATIUTE    BEGSR
027900020923     C*
028000020923     C* INIZIALIZZA VARIABILI DI WRK
028100020923     C                   CLEAR                   TIBS34DS
028200020923     C                   CLEAR                   AZUTEDS
028300020923     C                   CLEAR                   DDATIUTE
028400020923     C*
028500020923     C     *DTAARA       DEFINE    §azute        azuteds
028600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
028700020923     C                   IN(E)     *DTAARA
028800020923if  1C                   IF        %Error
028900020923     c                   EVAL      I34Tla = 'L'
029000020923     C                   CALL      'TIBS34R'
029100020923     C                   PARM                    Tibs34Ds
029200020923     C                   IN        *DTAARA
029300020923e   1C                   ENDIF
029400020923     C*
029500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
029600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
029700020923     C*
029800020923     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000970526     C* *INZSR - OPERAZIONI INIZIALI
030100970526     C*------------------------------------------------------------------------*
030200970526     C     *INZSR        BEGSR
030300991217     C*
030400991217     C     *ENTRY        PLIST
030500991217     C                   PARM                    KPJBA
030600020923     C*
030700020923     C* REPERISCE I DATI DELL'UTENTE
030800020923     C                   EXSR      REPDATIUTE
030900970526     C*
031000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
031100970526     C     XMSGPA        PLIST
031200970526     C                   PARM                    DSMSMS
031300970526     C                   PARM                    DSMSPG
031400970526     C                   PARM                    DSMSKE
031500970526     C                   PARM                    DSMSER
031600970526     C*
031700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
031800970526     C                   CALL      'XKEY'
031900970526     C                   PARM                    DSKEY
032000970526     C*---
032100970526     C* VARIABILI NON RIFERITE AL DATA BASE
032200970526     C*---
032300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
032400991214     C                   MOVEL     *BLANKS       DEPOP1            3
032500970526     C*---
032600970526     C* VARIABILI DI CONTROLLO
032700970526     C*---
032800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
032900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
033000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
033100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
033200970526     C*
033300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
033400140819     C                   MOVEL     'UBDSPPGMR1'  DSMSPG
033500140819     C                   MOVEL     'UBDSPPGMR1'  MSGQUE
033600970526     C                   MOVEL     *BLANKS       MSGKEY
033700970526     C                   MOVEL     *BLANKS       DSMSMS
033800970528     C*
033900970528     C* CALCOLA LA DATA CORRENTE
034000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
034100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
034200970528     C                   Z-ADD     WN8           G08DAT
034300970528     C                   Z-ADD     *ZEROS        G08INV
034400970528     C                   MOVEL     '0'           G08ERR
034500970528     C                   CALL      'XSRDA8'
034600970528     C                   PARM                    WLBDA8
034700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
034800000310     C*
034900000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
035000000310     C                   EXSR      AZZVID
035100970526     C*
035200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
035300970526     C                   EXSR      IMPFUN
035400970526     C*
035500970526     C                   ENDSR
035600970526     C*------------------------------------------------------------------------*
035700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
035800970526     C*------------------------------------------------------------------------*
035900970526     C     FUNGES        BEGSR
036000970526     C*
036100970526     C* PULISCO CAMPI DI LAVORO
036200970526     C                   Z-ADD     0             W                 5 0
036300970526     C*
036400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
036500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
036600970526     C     1             DO        24            I
036700970526     C                   MOVEL     FUD(I)        FUDRIG
036800970526     C*
036900970526     C* PULIZIA FUNZIONI PRECEDENTI
037000970526     C                   MOVEL     *BLANKS       FUV(I)
037100970526     C*
037200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
037300970526     C* (SALTA F24)
037400970526     C     FUDATT        IFEQ      'S'
037500970526     C     FUDFUN        ANDNE     '24'
037600970526     C                   ADD       1             W
037700970526     C                   MOVEL     FUDDES        FUV(W)
037800970526     C                   END
037900970526     C                   END                                                    DO WHILE
038000970526     C*
038100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
038200970526     C* ALTRIMENTI DISATTIVO F24
038300970526     C     1             DO        24            I
038400970526     C                   MOVEL     FUD(I)        FUDRIG
038500970526     C*
038600970526     C     FUDFUN        IFEQ      '24'
038700970526     C     W             IFGT      6
038800970526     C                   MOVEL     'S'           FUDATT
038900970526     C                   ELSE
039000970526     C                   MOVEL     *BLANKS       FUDATT
039100970526     C                   END                                                    W GT 8
039200970526     C                   MOVEL     FUDRIG        FUD(I)
039300970526     C                   END
039400970526     C                   END                                                    DO WHILE
039500970526     C*
039600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
039700970526     C     W             DIV       6             I
039800970526     C                   MVR                     Z
039900970526     C*
040000970526     C     Z             IFNE      0
040100970526     C     I             ADD       1             Z
040200970526     C                   ELSE
040300970526     C                   Z-ADD     I             Z                 5 0
040400970526     C                   END
040500970526     C*
040600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
040700970526     C                   ADD       1             FUNGRU
040800970526     C     FUNGRU        IFGT      Z
040900970526     C     FUNGRU        ORLT      1
041000970526     C                   Z-ADD     1             FUNGRU
041100970526     C                   END
041200970526     C*
041300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
041400970526     C     FUNGRU        IFEQ      1
041500970526     C                   MOVEL     FUV(1)        FUN001
041600970526     C                   MOVEL     FUV(2)        FUN002
041700970526     C                   MOVEL     FUV(3)        FUN003
041800970526     C                   MOVEL     FUV(4)        FUN004
041900970526     C                   MOVEL     FUV(5)        FUN005
042000970526     C                   MOVEL     FUV(6)        FUN006
042100970526     C                   END
042200970526     C*
042300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
042400970526     C     FUNGRU        IFEQ      2
042500970526     C                   MOVEL     FUV(7)        FUN001
042600970526     C                   MOVEL     FUV(8)        FUN002
042700970526     C                   MOVEL     FUV(9)        FUN003
042800970526     C                   MOVEL     FUV(10)       FUN004
042900970526     C                   MOVEL     FUV(11)       FUN005
043000970526     C                   MOVEL     FUV(12)       FUN006
043100970526     C                   END
043200970526     C*
043300970526     C* TERZO GRUPPO DI 6 FUNZIONI
043400970526     C     FUNGRU        IFEQ      3
043500970526     C                   MOVEL     FUV(13)       FUN001
043600970526     C                   MOVEL     FUV(14)       FUN002
043700970526     C                   MOVEL     FUV(15)       FUN003
043800970526     C                   MOVEL     FUV(16)       FUN004
043900970526     C                   MOVEL     FUV(17)       FUN005
044000970526     C                   MOVEL     FUV(18)       FUN006
044100970526     C                   END
044200970526     C*
044300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
044400970526     C     FUNGRU        IFEQ      4
044500970526     C                   MOVEL     FUV(19)       FUN001
044600970526     C                   MOVEL     FUV(20)       FUN002
044700970526     C                   MOVEL     FUV(21)       FUN003
044800970526     C                   MOVEL     FUV(22)       FUN004
044900970526     C                   MOVEL     *BLANKS       FUN005
045000970526     C                   MOVEL     *BLANKS       FUN006
045100970526     C                   END
045200970526     C*
045300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
045400970526     C                   MOVEL     *BLANKS       FUN007
045500970526     C                   MOVEL     *BLANKS       FUN008
045600970526     C*
045700970526     C     1             DO        24            I
045800970526     C                   MOVEL     FUD(I)        FUDRIG
045900970526     C     FUDFUN        IFEQ      '24'
046000970526     C     FUDATT        ANDEQ     'S'
046100970526     C                   MOVEL     FUDDES        FUN008
046200970526     C                   END
046300970526     C                   END                                                    DO
046400970526     C*
046500970526     C                   ENDSR
046600970526     C*------------------------------------------------------------------------*
046700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
046800970526     C*------------------------------------------------------------------------*
046900970526     C     FUNCON        BEGSR
047000970526     C*
047100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
047200970526     C                   EXSR      FUNVAR
047300970526     C*
047400970526     C* PULISCO CAMPI DI LAVORO
047500970526     C                   Z-ADD     0             I                 5 0
047600970526     C                   Z-ADD     0             W                 5 0
047700970526     C*
047800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
047900970526     C     FUNKEY        IFNE      *BLANKS
048000970526     C     1             DO        24            I
048100970526     C                   MOVEL     FUD(I)        FUDRIG
048200970526     C*
048300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
048400970526     C     FUNKEY        IFEQ      FUDFUN
048500970526     C     FUDATT        ANDEQ     'S'
048600970526     C                   Z-ADD     I             W
048700970526     C                   END
048800970526     C                   END                                                    DO WHILE
048900970526     C*
049000970526     C* FUNZIONE NON ATTIVA (ERRORE)
049100970526     C     W             IFEQ      0
049200970526     C*
049300970526     C* EMISSIONE MESSAGGIO DI ERRORE
049400970526     C                   MOVEL     MSG(1)        DSMSMS
049500970526     C*
049600970526     C                   END                                                    W=0
049700970526     C                   END                                                    FUNKEY NE *BLANKS
049800970526     C*
049900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
050000970526     C     W             IFNE      0
050100970526     C     FUNKEY        OREQ      *BLANKS
050200970526     C     FUNKEY        IFEQ      *BLANKS
050300970526     C                   EXSR      ENTGES
050400970526     C                   END
050500970526     C     FUNKEY        IFEQ      '03'
050600970526     C                   EXSR      F03GES
050700970526     C                   END
050800970526     C     FUNKEY        IFEQ      '06'
050900970526     C                   EXSR      F06GES
051000970526     C                   END
051100130109     C     FUNKEY        IFEQ      '07'
051200130109     C                   EXSR      F07GES
051300130109     C                   END
051400970526     C     FUNKEY        IFEQ      '12'
051500970526     C                   EXSR      F12GES
051600970526     C                   END
051700970526     C     FUNKEY        IFEQ      '24'
051800970526     C                   EXSR      F24GES
051900970526     C                   END
052000970526     C                   END
052100970526     C*
052200970526     C                   ENDSR
052300970526     C*------------------------------------------------------------------------*
052400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
052500970526     C*------------------------------------------------------------------------*
052600970526     C     FUNVAR        BEGSR
052700970526     C*
052800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
052900970526     C*
053000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
053100970526     C     KEY           IFEQ      ENTER
053200970526     C                   MOVEL     *BLANKS       FUNKEY
053300970526     C                   END
053400970526     C     KEY           IFEQ      F1
053500970526     C                   MOVEL     '01'          FUNKEY
053600970526     C                   END
053700970526     C     KEY           IFEQ      F2
053800970526     C                   MOVEL     '02'          FUNKEY
053900970526     C                   END
054000970526     C     KEY           IFEQ      F3
054100970526     C                   MOVEL     '03'          FUNKEY
054200970526     C                   END
054300970526     C     KEY           IFEQ      F4
054400970526     C                   MOVEL     '04'          FUNKEY
054500970526     C                   END
054600970526     C     KEY           IFEQ      F5
054700970526     C                   MOVEL     '05'          FUNKEY
054800970526     C                   END
054900970526     C     KEY           IFEQ      F6
055000970526     C                   MOVEL     '06'          FUNKEY
055100970526     C                   END
055200970526     C     KEY           IFEQ      F7
055300970526     C                   MOVEL     '07'          FUNKEY
055400970526     C                   END
055500970526     C     KEY           IFEQ      F8
055600970526     C                   MOVEL     '08'          FUNKEY
055700970526     C                   END
055800970526     C     KEY           IFEQ      F9
055900970526     C                   MOVEL     '09'          FUNKEY
056000970526     C                   END
056100970526     C     KEY           IFEQ      F10
056200970526     C                   MOVEL     '10'          FUNKEY
056300970526     C                   END
056400970526     C     KEY           IFEQ      F11
056500970526     C                   MOVEL     '11'          FUNKEY
056600970526     C                   END
056700970526     C     KEY           IFEQ      F12
056800970526     C                   MOVEL     '12'          FUNKEY
056900970526     C                   END
057000970526     C     KEY           IFEQ      F13
057100970526     C                   MOVEL     '13'          FUNKEY
057200970526     C                   END
057300970526     C     KEY           IFEQ      F14
057400970526     C                   MOVEL     '14'          FUNKEY
057500970526     C                   END
057600970526     C     KEY           IFEQ      F15
057700970526     C                   MOVEL     '15'          FUNKEY
057800970526     C                   END
057900970526     C     KEY           IFEQ      F16
058000970526     C                   MOVEL     '16'          FUNKEY
058100970526     C                   END
058200970526     C     KEY           IFEQ      F17
058300970526     C                   MOVEL     '17'          FUNKEY
058400970526     C                   END
058500970526     C     KEY           IFEQ      F18
058600970526     C                   MOVEL     '18'          FUNKEY
058700970526     C                   END
058800970526     C     KEY           IFEQ      F19
058900970526     C                   MOVEL     '19'          FUNKEY
059000970526     C                   END
059100970526     C     KEY           IFEQ      F20
059200970526     C                   MOVEL     '20'          FUNKEY
059300970526     C                   END
059400970526     C     KEY           IFEQ      F21
059500970526     C                   MOVEL     '21'          FUNKEY
059600970526     C                   END
059700970526     C     KEY           IFEQ      F22
059800970526     C                   MOVEL     '22'          FUNKEY
059900970526     C                   END
060000970526     C     KEY           IFEQ      F23
060100970526     C                   MOVEL     '23'          FUNKEY
060200970526     C                   END
060300970526     C     KEY           IFEQ      F24
060400970526     C                   MOVEL     '24'          FUNKEY
060500970526     C                   END
060600970526     C     KEY           IFEQ      ROLLUP
060700970526     C                   MOVEL     'RU'          FUNKEY
060800970526     C                   END
060900970526     C     KEY           IFEQ      HELP
061000970526     C                   MOVEL     'HP'          FUNKEY
061100970526     C                   END
061200970526     C                   ENDSR
061300970526     C*------------------------------------------------------------------------*
061400970526     C* NOTE COSTRUZIONE SCHIERE
061500970526     C*------------------------------------------------------------------------*
061600970526     C*
061700970526     C* -MSG-MESSAGGI
061800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
061900970526     C* (NON TOCCARE MSG,1 E MSG,2)
062000970526     C*
062100970526     C* -CMD-COMANDI
062200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
062300970526     C*
062400970526     C* -OPD-OPZIONI
062500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
062600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
062700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
062800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
063000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
063100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
063200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
063300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
063400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
063500970526     C*
063600970526     C* -FUD-FUNZIONI
063700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
063800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
063900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
064000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
064100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
064200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
064300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
064400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
064500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
064600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
064700970526** MSG - MESSAGGI
064800120927Tasto funzionale non gestito                                                   1
064900120927Numero chiamate obbligatorio                                                   2
065000140819Libreria obbligatoria                                                          3
065100140819                                                                               4
065200130108                                                                               5
065300130108Campo obbligatorio                                                             6
065400130108                                                                               7
065500130108                                                                               8
065600120927                                                                               9
065700120927                                                                               10
065800120927                                                                               11
065900120927                                                                               12
066000120927                                                                               13
066100120927Data errata                                                                    14
066200140116                                                                               15
066300120927                                                                               16
066400120927                                                                               17
066500120927                                                                               18
066600120927                                                                               19
066700140821Reperimento concluso - se l'esito è 0, i dati sono sul file WFACTGR0F          20
066800120927Scelta obbligatoria                                                            21
066900130110Eseguita la stampa                                                             22
067000121108                                                                               23
067100121108                                                                               24
067200970526** FUD - FUNZIONI
067300970526                   -        (S/N)=N                        funz.01¬
067400970526                   -        (S/N)=N                        funz.02¬
067500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
067600970526                   -        (S/N)=N                        funz.04¬
067700970526                   -        (S/N)=N                        funz.05¬
067800991214F06=Conferma       -        (S/N)=S                        funz.06¬
067900140819                   -        (S/N)=N                        funz.07¬
068000970527                   -        (S/N)=N                        funz.08¬
068100970527                   -        (S/N)=N                        funz.09¬
068200970527                   -        (S/N)=N                        funz.10¬
068300970526                   -        (S/N)=N                        funz.11¬
068400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
068500970526                   -        (S/N)=N                        funz.13¬
068600970526                   -        (S/N)=N                        funz.14¬
068700970526                   -        (S/N)=N                        funz.15¬
068800970526                   -        (S/N)=N                        funz.16¬
068900970526                   -        (S/N)=N                        funz.17¬
069000970526                   -        (S/N)=N                        funz.18¬
069100970526                   -        (S/N)=N                        funz.19¬
069200970526                   -        (S/N)=N                        funz.20¬
069300970526                   -        (S/N)=N                        funz.21¬
069400970526                   -        (S/N)=N                        funz.22¬
069500970526                   -        (S/N)=N                        funz.23¬
069600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
