000100970526     H*------------------------------------------------------------------------*
000200140109     H* Lancio Test - Reperimento unificazioni file fatture
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800120927     Fcnaco00f  IF   E           K DISK
000900140109     FUBrepcliuDcf   e             WORKSTN INFDS(DEVDS1)
001000140109     F                                     SFILE(REPCLUDMS:MSGULT)
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
007600140109     d pIn_CodCli      s              7s 0
007700140109     d pOut_Esito      s              2s 0
007800140109      // legenda flag:
007900140109      //               01 = Cliente non passato
008000140109      //               02 = Cliente passato non trovato in tab. 4C
008100140109      //               03 = Cliente passato trovato in tab. 4C ma annullato
008200140109      //               04 = Cliente passato non unificato
008300140109      //               05 = Errore SQL
008400140109     d pOut_CliUni     s              7s 0
008500140109     d pOut_TotUti     s              5s 0
008600140109     d pOut_ListUti    s              7s 0 dim(500)
008700130116
008800130116     d ora             s               t
008900120928
009000970526     C*------------------------------------------------------------------------*
009100000303     C* MAIN LINES
009200970526     C*------------------------------------------------------------------------*
009300970526     C*
009400120927     C* metto i default su D03
009500120927     C                   EXSR      InzD03
009600120927     C*
009700120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010100970526     C*
010200970526     C* EMETTE FORMATO VIDEO
010300970526     C                   EXSR      BUTFMR
010400970526     C*
010500970526     C* GESTIONE TASTI FUNZIONALI
010600970526     C                   EXSR      FUNCON
010700970526E   2C                   ENDDO
010800970526     C*
010900970526     C* FINE PGM
011000970526     C                   SETON                                        LR
011100970526     C*------------------------------------------------------------------------*
011200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011300970526     C*------------------------------------------------------------------------*
011400970526     C     BUTFMR        BEGSR
011500970526     C*
011600970526     C* EMETTE LA TESTATA
011700140109     C                   WRITE     REPCLUD01
011800970526     C*
011900970526     C* SPEDISCE EVENTUALE MESSAGGIO
012000970526     C                   EXSR      SNDMSG
012100970526     C*
012200970526     C* EMETTE LE FUNZIONI
012300140109     C                   WRITE     REPCLUD02
012400970526     C*
012500970526     C* EMETTE IL VIDEO
012600140109     C                   EXFMT     REPCLUD03
012700970526     C*
012800991214     C* REIMPOSTA FLAG DI ERRORE
012900991214     C                   MOVEL     '0'           ERRL00
013000970526     C*
013100970526     C* REIMPOSTA INDICATORI DI ERRORE
013200020611     C                   SETOFF                                       555556
013300970526     C*
013400970526     C                   ENDSR
013500970526     C*------------------------------------------------------------------------*
013600120927     C* InzD03 - Inizializzo D03
013700970526     C*------------------------------------------------------------------------*
013800120927     C     InzD03        BEGSR
013900120927     C*
014000130108     C*
014100120927     C                   ENDSR
014200120927     C*------------------------------------------------------------------------*
014300120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014400120927     C*------------------------------------------------------------------------*
014500120927     C     SNDMSG        BEGSR
014600120927     C*
014700970526     C* PULISCE IL SFILE MESSAGGI
014800970526     C                   MOVEL     '1'           *IN40
014900140109     C                   WRITE     REPCLUDMC
015000970526     C                   MOVEL     '0'           *IN40
015100970526     C*
015200970526     C* VISUALIZZA IL MESSAGGIO
015300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015400970526     C                   CALL      'XMSG'        XMSGPA
015500020611     C     DSMSER        IFEQ      *ZEROS
015600970526     C                   MOVEL     DSMSKE        MSGKEY
015700020611     C                   ENDIF
015800970526     C                   Z-ADD     1             MSGULT
015900140109     C                   WRITE     REPCLUDMS
016000140109     C                   WRITE     REPCLUDMC
016100970526     C*
016200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016300970526     C                   MOVEL     *BLANKS       DSMSMS
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* ENTGES - GESTIONE ENTER -> INVIO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     ENTGES        BEGSR
017000970526     C*
017100970526     C* CONTROLLA I CAMPI IMMESSI
017200970526     C                   EXSR      DECODI
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600970526     C* F03GES - GESTIONE F03 -> FINE
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F03GES        BEGSR
017900970526     C*
018000970526     C* IMPOSTA "PREMUTO F03"
018100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500991214     C* F06GES - GESTIONE F06 -> CONFERMA
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F06GES        BEGSR
018800970526     C*
018900991214     C* CONTROLLA I CAMPI IMMESSI
019000991214     C                   EXSR      DECODI
019100020611     C*
019200991214     C* ESEGUE ROUTINE DI CONFERMA
019300020611     C     ERRL00        IFEQ      '0'
019400991214     C                   EXSR      ESEGUI
019500130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
019600121109     C                   MOVEL     MSG(20)       DSMSMS
019700121109     C* non esco mai dal pgm
019800121109     C***                MOVEL     '1'           FINL00
019900121108     C                   ENDIF
020000970526     C*
020100991214     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300130109     C* F07GES - GESTIONE F07 -> STAMPA
020400970526     C*------------------------------------------------------------------------*
020500130109     C     F07GES        BEGSR
020600970526     C*
020700130110     C*
020800970526     C                   ENDSR
020900130109     C*------------------------------------------------------------------------*
021000130109     C* F12GES - GESTIONE F12 -> RITORNO
021100130109     C*------------------------------------------------------------------------*
021200130109     C     F12GES        BEGSR
021300130109     C*
021400130109     C* IMPOSTA "PREMUTO F12"
021500130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021600130109     C*
021700130109     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022000970526     C*------------------------------------------------------------------------*
022100970526     C     F24GES        BEGSR
022200970526     C*
022300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022800970526     C*------------------------------------------------------------------------*
022900991214     C     ESEGUI        BEGSR
023000121108     C*
023100140109     C                   EVAL      pIn_CodCli  = VIDCodCli
023200140109     c                   clear                   pOut_Esito
023300140109     c                   clear                   pOut_CliUni
023400140109     c                   clear                   pOut_TotUti
023500140109     c                   clear                   pOut_ListUti
023600121108     C* chiamo
023700140109     C                   CALL      'UBREPCLIU'
023800140109     c                   parm                    pIn_CodCli
023900140109     c                   parm                    pOut_Esito
024000140109     c                   parm                    pOut_CliUni
024100140109     c                   parm                    pOut_TotUti
024200140109     c                   parm                    pOut_ListUti
024300121108     C*
024400140109     c                   EVAL      VIDEsito = pOut_Esito
024500140109     C                   IF        pOut_CliUni > 0
024600140109     c                   EVAL      VIDCliUni = %editc(pOut_CliUni:'X')
024700140109     C                   Z-ADD     1             ACOkut
024800140109     C                   Z-ADD     DUTKCI        ACOkcc
024900140109     C                   Z-ADD     pOut_CliUni   ACOksc
025000140109     C     K03ACO00      CHAIN     cnaco00f
025100140109     C                   IF        %found(cnaco00f)
025200140109     C                   MOVEL     ACOrag        VDDCliUni
025300140109     C                   ELSE
025400140109     C* non faccio nulla
025500140109     C                   MOVEL     *BLANKS       VDDCliUni
025600140109     C                   ENDIF
025700140109     C                   ENDIF
025800140109     c                   EVAL      VIDTotUti = pOut_TotUti
025900140109     c                   IF        pOut_ListUti(01) > 0
026000140109     c                   EVAL      VIDUTI01  = %editc(pOut_ListUti(01):'X')
026100140109     c                   ENDIF
026200140109     c                   IF        pOut_ListUti(02) > 0
026300140109     c                   EVAL      VIDUTI02  = %editc(pOut_ListUti(02):'X')
026400140109     c                   ENDIF
026500140109     c                   IF        pOut_ListUti(03) > 0
026600140109     c                   EVAL      VIDUTI03  = %editc(pOut_ListUti(03):'X')
026700140109     c                   ENDIF
026800140109     c                   IF        pOut_ListUti(04) > 0
026900140109     c                   EVAL      VIDUTI04  = %editc(pOut_ListUti(04):'X')
027000140109     c                   ENDIF
027100140109     c                   IF        pOut_ListUti(05) > 0
027200140109     c                   EVAL      VIDUTI05  = %editc(pOut_ListUti(05):'X')
027300140109     c                   ENDIF
027400140109     c                   IF        pOut_ListUti(06) > 0
027500140109     c                   EVAL      VIDUTI06  = %editc(pOut_ListUti(06):'X')
027600140109     c                   ENDIF
027700140109     c                   IF        pOut_ListUti(07) > 0
027800140109     c                   EVAL      VIDUTI07  = %editc(pOut_ListUti(07):'X')
027900140109     c                   ENDIF
028000140109     c                   IF        pOut_ListUti(08) > 0
028100140109     c                   EVAL      VIDUTI08  = %editc(pOut_ListUti(08):'X')
028200140109     c                   ENDIF
028300140109     c                   IF        pOut_ListUti(09) > 0
028400140109     c                   EVAL      VIDUTI09  = %editc(pOut_ListUti(09):'X')
028500140109     c                   ENDIF
028600140109     c                   IF        pOut_ListUti(10) > 0
028700140109     c                   EVAL      VIDUTI10  = %editc(pOut_ListUti(10):'X')
028800140109     c                   ENDIF
028900991214     C*
029000970526     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029300970526     C*------------------------------------------------------------------------*
029400970526     C     DECODI        BEGSR
029500121019     C*
029600120927     C                   SETOFF                                       505152
029700140109     C                   MOVEL     *blanks       VDDCodCli
029800140109     C                   MOVEL     *blanks       VIDCliUni
029900140109     C                   MOVEL     *blanks       VDDCliUni
030000140109     C                   MOVEL     *zeros        VIDEsito
030100140109     C                   MOVEL     *zeros        VIDTotUti
030200140109     c                   EVAL      VIDUTI01  = *blank
030300140109     c                   EVAL      VIDUTI02  = *blank
030400140109     c                   EVAL      VIDUTI03  = *blank
030500140109     c                   EVAL      VIDUTI04  = *blank
030600140109     c                   EVAL      VIDUTI05  = *blank
030700140109     c                   EVAL      VIDUTI06  = *blank
030800140109     c                   EVAL      VIDUTI07  = *blank
030900140109     c                   EVAL      VIDUTI08  = *blank
031000140109     c                   EVAL      VIDUTI09  = *blank
031100140109     c                   EVAL      VIDUTI10  = *blank
031200121001     C*
031300140109     C* Cod. cliente esistente su CNACO
031400121001     C     ERRL00        IFEQ      '0'
031500140109     C                   IF        VIDCodCli > 0
031600120927     C                   Z-ADD     1             ACOkut
031700120927     C                   Z-ADD     DUTKCI        ACOkcc
031800140109     C                   Z-ADD     VIDCodCli     ACOksc
031900120927     C     K03ACO00      CHAIN     cnaco00f
032000120927     C                   IF        %found(cnaco00f)
032100140109     C                   MOVEL     ACOrag        VDDCodCli
032200120927     C                   ELSE
032300120927     C                   MOVEL     '1'           ERRL00
032400130108     C                   SETON                                        51
032500120927     C                   MOVEL     MSG(3)        DSMSMS
032600140109     C                   MOVEL     *BLANKS       VDDCodCli
032700120927     C                   ENDIF
032800130123     C                   ENDIF
032900120927     C                   ENDIF
033000120927     C*
033100970528     C                   ENDSR
033200970526     C*------------------------------------------------------------------------*
033300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
033400970526     C*------------------------------------------------------------------------*
033500970526     C     AZZVID        BEGSR
033600970526     C* CAMPI
033700970526     C*
033800970526     C                   ENDSR
033900970526     C*------------------------------------------------------------------------*
034000020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
034100970526     C*------------------------------------------------------------------------*
034200970526     C     IMPFUN        BEGSR
034300970526     C*
034400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
034500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
034600970526     C                   EXSR      FUNGES
034700970526     C*
034800970526     C                   ENDSR
034900020923     C*--------------------------------------------------------------------------------------------*
035000020923     C* REPERISCE I DATI UTENTE
035100020923     C*--------------------------------------------------------------------------------------------*
035200020923     C     REPDATIUTE    BEGSR
035300020923     C*
035400020923     C* INIZIALIZZA VARIABILI DI WRK
035500020923     C                   CLEAR                   TIBS34DS
035600020923     C                   CLEAR                   AZUTEDS
035700020923     C                   CLEAR                   DDATIUTE
035800020923     C*
035900020923     C     *DTAARA       DEFINE    §azute        azuteds
036000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
036100020923     C                   IN(E)     *DTAARA
036200020923if  1C                   IF        %Error
036300020923     c                   EVAL      I34Tla = 'L'
036400020923     C                   CALL      'TIBS34R'
036500020923     C                   PARM                    Tibs34Ds
036600020923     C                   IN        *DTAARA
036700020923e   1C                   ENDIF
036800020923     C*
036900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
037000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
037100020923     C*
037200020923     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* *INZSR - OPERAZIONI INIZIALI
037500970526     C*------------------------------------------------------------------------*
037600970526     C     *INZSR        BEGSR
037700991217     C*
037800991217     C     *ENTRY        PLIST
037900991217     C                   PARM                    KPJBA
038000120927     C*
038100120927     C* LETTURA cnaco00f -
038200120927     C     K03ACO00      KLIST
038300120927     C                   KFLD                    ACOkut
038400120927     C                   KFLD                    ACOkcc
038500120927     C                   KFLD                    ACOksc
038600020923     C*
038700020923     C* REPERISCE I DATI DELL'UTENTE
038800020923     C                   EXSR      REPDATIUTE
038900970526     C*
039000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
039100970526     C     XMSGPA        PLIST
039200970526     C                   PARM                    DSMSMS
039300970526     C                   PARM                    DSMSPG
039400970526     C                   PARM                    DSMSKE
039500970526     C                   PARM                    DSMSER
039600970526     C*
039700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
039800970526     C                   CALL      'XKEY'
039900970526     C                   PARM                    DSKEY
040000970526     C*---
040100970526     C* VARIABILI NON RIFERITE AL DATA BASE
040200970526     C*---
040300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
040400991214     C                   MOVEL     *BLANKS       DEPOP1            3
040500970526     C*---
040600970526     C* VARIABILI DI CONTROLLO
040700970526     C*---
040800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
040900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
041000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
041100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
041200970526     C*
041300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
041400140109     C                   MOVEL     'UBREPCLIUR'  DSMSPG
041500140109     C                   MOVEL     'UBREPCLIUR'  MSGQUE
041600970526     C                   MOVEL     *BLANKS       MSGKEY
041700970526     C                   MOVEL     *BLANKS       DSMSMS
041800970528     C*
041900970528     C* CALCOLA LA DATA CORRENTE
042000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
042100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
042200970528     C                   Z-ADD     WN8           G08DAT
042300970528     C                   Z-ADD     *ZEROS        G08INV
042400970528     C                   MOVEL     '0'           G08ERR
042500970528     C                   CALL      'XSRDA8'
042600970528     C                   PARM                    WLBDA8
042700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
042800000310     C*
042900000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
043000000310     C                   EXSR      AZZVID
043100970526     C*
043200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
043300970526     C                   EXSR      IMPFUN
043400970526     C*
043500970526     C                   ENDSR
043600970526     C*------------------------------------------------------------------------*
043700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
043800970526     C*------------------------------------------------------------------------*
043900970526     C     FUNGES        BEGSR
044000970526     C*
044100970526     C* PULISCO CAMPI DI LAVORO
044200970526     C                   Z-ADD     0             W                 5 0
044300970526     C*
044400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
044500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
044600970526     C     1             DO        24            I
044700970526     C                   MOVEL     FUD(I)        FUDRIG
044800970526     C*
044900970526     C* PULIZIA FUNZIONI PRECEDENTI
045000970526     C                   MOVEL     *BLANKS       FUV(I)
045100970526     C*
045200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
045300970526     C* (SALTA F24)
045400970526     C     FUDATT        IFEQ      'S'
045500970526     C     FUDFUN        ANDNE     '24'
045600970526     C                   ADD       1             W
045700970526     C                   MOVEL     FUDDES        FUV(W)
045800970526     C                   END
045900970526     C                   END                                                    DO WHILE
046000970526     C*
046100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
046200970526     C* ALTRIMENTI DISATTIVO F24
046300970526     C     1             DO        24            I
046400970526     C                   MOVEL     FUD(I)        FUDRIG
046500970526     C*
046600970526     C     FUDFUN        IFEQ      '24'
046700970526     C     W             IFGT      6
046800970526     C                   MOVEL     'S'           FUDATT
046900970526     C                   ELSE
047000970526     C                   MOVEL     *BLANKS       FUDATT
047100970526     C                   END                                                    W GT 8
047200970526     C                   MOVEL     FUDRIG        FUD(I)
047300970526     C                   END
047400970526     C                   END                                                    DO WHILE
047500970526     C*
047600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
047700970526     C     W             DIV       6             I
047800970526     C                   MVR                     Z
047900970526     C*
048000970526     C     Z             IFNE      0
048100970526     C     I             ADD       1             Z
048200970526     C                   ELSE
048300970526     C                   Z-ADD     I             Z                 5 0
048400970526     C                   END
048500970526     C*
048600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
048700970526     C                   ADD       1             FUNGRU
048800970526     C     FUNGRU        IFGT      Z
048900970526     C     FUNGRU        ORLT      1
049000970526     C                   Z-ADD     1             FUNGRU
049100970526     C                   END
049200970526     C*
049300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
049400970526     C     FUNGRU        IFEQ      1
049500970526     C                   MOVEL     FUV(1)        FUN001
049600970526     C                   MOVEL     FUV(2)        FUN002
049700970526     C                   MOVEL     FUV(3)        FUN003
049800970526     C                   MOVEL     FUV(4)        FUN004
049900970526     C                   MOVEL     FUV(5)        FUN005
050000970526     C                   MOVEL     FUV(6)        FUN006
050100970526     C                   END
050200970526     C*
050300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
050400970526     C     FUNGRU        IFEQ      2
050500970526     C                   MOVEL     FUV(7)        FUN001
050600970526     C                   MOVEL     FUV(8)        FUN002
050700970526     C                   MOVEL     FUV(9)        FUN003
050800970526     C                   MOVEL     FUV(10)       FUN004
050900970526     C                   MOVEL     FUV(11)       FUN005
051000970526     C                   MOVEL     FUV(12)       FUN006
051100970526     C                   END
051200970526     C*
051300970526     C* TERZO GRUPPO DI 6 FUNZIONI
051400970526     C     FUNGRU        IFEQ      3
051500970526     C                   MOVEL     FUV(13)       FUN001
051600970526     C                   MOVEL     FUV(14)       FUN002
051700970526     C                   MOVEL     FUV(15)       FUN003
051800970526     C                   MOVEL     FUV(16)       FUN004
051900970526     C                   MOVEL     FUV(17)       FUN005
052000970526     C                   MOVEL     FUV(18)       FUN006
052100970526     C                   END
052200970526     C*
052300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
052400970526     C     FUNGRU        IFEQ      4
052500970526     C                   MOVEL     FUV(19)       FUN001
052600970526     C                   MOVEL     FUV(20)       FUN002
052700970526     C                   MOVEL     FUV(21)       FUN003
052800970526     C                   MOVEL     FUV(22)       FUN004
052900970526     C                   MOVEL     *BLANKS       FUN005
053000970526     C                   MOVEL     *BLANKS       FUN006
053100970526     C                   END
053200970526     C*
053300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
053400970526     C                   MOVEL     *BLANKS       FUN007
053500970526     C                   MOVEL     *BLANKS       FUN008
053600970526     C*
053700970526     C     1             DO        24            I
053800970526     C                   MOVEL     FUD(I)        FUDRIG
053900970526     C     FUDFUN        IFEQ      '24'
054000970526     C     FUDATT        ANDEQ     'S'
054100970526     C                   MOVEL     FUDDES        FUN008
054200970526     C                   END
054300970526     C                   END                                                    DO
054400970526     C*
054500970526     C                   ENDSR
054600970526     C*------------------------------------------------------------------------*
054700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
054800970526     C*------------------------------------------------------------------------*
054900970526     C     FUNCON        BEGSR
055000970526     C*
055100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
055200970526     C                   EXSR      FUNVAR
055300970526     C*
055400970526     C* PULISCO CAMPI DI LAVORO
055500970526     C                   Z-ADD     0             I                 5 0
055600970526     C                   Z-ADD     0             W                 5 0
055700970526     C*
055800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
055900970526     C     FUNKEY        IFNE      *BLANKS
056000970526     C     1             DO        24            I
056100970526     C                   MOVEL     FUD(I)        FUDRIG
056200970526     C*
056300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
056400970526     C     FUNKEY        IFEQ      FUDFUN
056500970526     C     FUDATT        ANDEQ     'S'
056600970526     C                   Z-ADD     I             W
056700970526     C                   END
056800970526     C                   END                                                    DO WHILE
056900970526     C*
057000970526     C* FUNZIONE NON ATTIVA (ERRORE)
057100970526     C     W             IFEQ      0
057200970526     C*
057300970526     C* EMISSIONE MESSAGGIO DI ERRORE
057400970526     C                   MOVEL     MSG(1)        DSMSMS
057500970526     C*
057600970526     C                   END                                                    W=0
057700970526     C                   END                                                    FUNKEY NE *BLANKS
057800970526     C*
057900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
058000970526     C     W             IFNE      0
058100970526     C     FUNKEY        OREQ      *BLANKS
058200970526     C     FUNKEY        IFEQ      *BLANKS
058300970526     C                   EXSR      ENTGES
058400970526     C                   END
058500970526     C     FUNKEY        IFEQ      '03'
058600970526     C                   EXSR      F03GES
058700970526     C                   END
058800970526     C     FUNKEY        IFEQ      '06'
058900970526     C                   EXSR      F06GES
059000970526     C                   END
059100130109     C     FUNKEY        IFEQ      '07'
059200130109     C                   EXSR      F07GES
059300130109     C                   END
059400970526     C     FUNKEY        IFEQ      '12'
059500970526     C                   EXSR      F12GES
059600970526     C                   END
059700970526     C     FUNKEY        IFEQ      '24'
059800970526     C                   EXSR      F24GES
059900970526     C                   END
060000970526     C                   END
060100970526     C*
060200970526     C                   ENDSR
060300970526     C*------------------------------------------------------------------------*
060400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
060500970526     C*------------------------------------------------------------------------*
060600970526     C     FUNVAR        BEGSR
060700970526     C*
060800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
060900970526     C*
061000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
061100970526     C     KEY           IFEQ      ENTER
061200970526     C                   MOVEL     *BLANKS       FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F1
061500970526     C                   MOVEL     '01'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F2
061800970526     C                   MOVEL     '02'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F3
062100970526     C                   MOVEL     '03'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F4
062400970526     C                   MOVEL     '04'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F5
062700970526     C                   MOVEL     '05'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F6
063000970526     C                   MOVEL     '06'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F7
063300970526     C                   MOVEL     '07'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F8
063600970526     C                   MOVEL     '08'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F9
063900970526     C                   MOVEL     '09'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F10
064200970526     C                   MOVEL     '10'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F11
064500970526     C                   MOVEL     '11'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F12
064800970526     C                   MOVEL     '12'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F13
065100970526     C                   MOVEL     '13'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F14
065400970526     C                   MOVEL     '14'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F15
065700970526     C                   MOVEL     '15'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F16
066000970526     C                   MOVEL     '16'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F17
066300970526     C                   MOVEL     '17'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F18
066600970526     C                   MOVEL     '18'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F19
066900970526     C                   MOVEL     '19'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F20
067200970526     C                   MOVEL     '20'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F21
067500970526     C                   MOVEL     '21'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F22
067800970526     C                   MOVEL     '22'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F23
068100970526     C                   MOVEL     '23'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F24
068400970526     C                   MOVEL     '24'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      ROLLUP
068700970526     C                   MOVEL     'RU'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      HELP
069000970526     C                   MOVEL     'HP'          FUNKEY
069100970526     C                   END
069200970526     C                   ENDSR
069300970526     C*------------------------------------------------------------------------*
069400970526     C* NOTE COSTRUZIONE SCHIERE
069500970526     C*------------------------------------------------------------------------*
069600970526     C*
069700970526     C* -MSG-MESSAGGI
069800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
069900970526     C* (NON TOCCARE MSG,1 E MSG,2)
070000970526     C*
070100970526     C* -CMD-COMANDI
070200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
070300970526     C*
070400970526     C* -OPD-OPZIONI
070500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
070600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
070700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
070800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
071000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
071100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
071300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
071400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
071500970526     C*
071600970526     C* -FUD-FUNZIONI
071700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
071800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
071900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
072000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
072100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
072200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
072300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
072400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
072500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
072600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
072700970526** MSG - MESSAGGI
072800120927Tasto funzionale non gestito                                                   1
072900120927Numero chiamate obbligatorio                                                   2
073000120927Codice cliente non esiste su CNACO00F                                          3
073100130108                                                                               4
073200130108                                                                               5
073300130108Campo obbligatorio                                                             6
073400130108                                                                               7
073500130108                                                                               8
073600120927                                                                               9
073700120927                                                                               10
073800120927                                                                               11
073900120927                                                                               12
074000120927                                                                               13
074100120927Data errata                                                                    14
074200130108La data inizio deve essere > o = della data fine                               15
074300120927                                                                               16
074400120927                                                                               17
074500120927                                                                               18
074600120927                                                                               19
074700140109Reperimento concluso - controllare l'esito                                     20
074800120927Scelta obbligatoria                                                            21
074900130110Eseguita la stampa                                                             22
075000121108                                                                               23
075100121108                                                                               24
075200970526** FUD - FUNZIONI
075300970526                   -        (S/N)=N                        funz.01¬
075400970526                   -        (S/N)=N                        funz.02¬
075500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
075600970526                   -        (S/N)=N                        funz.04¬
075700970526                   -        (S/N)=N                        funz.05¬
075800991214F06=Conferma       -        (S/N)=S                        funz.06¬
075900140109                   -        (S/N)=N                        funz.07¬
076000970527                   -        (S/N)=N                        funz.08¬
076100970527                   -        (S/N)=N                        funz.09¬
076200970527                   -        (S/N)=N                        funz.10¬
076300970526                   -        (S/N)=N                        funz.11¬
076400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
076500970526                   -        (S/N)=N                        funz.13¬
076600970526                   -        (S/N)=N                        funz.14¬
076700970526                   -        (S/N)=N                        funz.15¬
076800970526                   -        (S/N)=N                        funz.16¬
076900970526                   -        (S/N)=N                        funz.17¬
077000970526                   -        (S/N)=N                        funz.18¬
077100970526                   -        (S/N)=N                        funz.19¬
077200970526                   -        (S/N)=N                        funz.20¬
077300970526                   -        (S/N)=N                        funz.21¬
077400970526                   -        (S/N)=N                        funz.22¬
077500970526                   -        (S/N)=N                        funz.23¬
077600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
