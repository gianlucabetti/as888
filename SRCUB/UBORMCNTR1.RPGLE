000100970526     H*------------------------------------------------------------------------*
000200160601     H* Lancio
000300970526     H*------------------------------------------------------------------------*
000400160817     H dftactgrp(*no) bnddir('UBBNDDIR') actgrp('QILE')
000500160817     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900160601     FUBORMCNTD1cf   e             WORKSTN INFDS(DEVDS1)
001000160601     F                                     SFILE(ORMCNDMS:MSGULT)
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
007500160601
007600160601       // - Parametri ricevuti:
007700160601       // opz: 1=elabora  C=chiudi
007800160818     d*pIn_Opz         s              1a
007900160818     d*pIn_DS_FNVAO  e ds                  extname(FNVAOEIIR:FNVAO000)
008000160818     d*                                    qualified
008100160601       // - Parametri in uscita:
008200160818     d*pOut_Count      s              3s 0
008300160601       // esito: 1=OK  E=Err input  2=KO
008400160818     d*pOut_Esito      s              1a
008500130116
008600130116     d ora             s               t
008700140117     d wX              s              2s 0
008800150407     d wVAB            s               n
008900150407     d wVAO            s               n
009000160223
009100160223       //--------------------------------------------------------------
009200160223       // Definizione prototipi procedure.
009300160223       //--------------------------------------------------------------
009400120928
009500160818     D/COPY GAITRASRC/SRCPROTOPI,UBORMCNTR
009600160818     D/COPY GAITRASRC/SRCPROTOPR,UBORMCNTR
009700160817
009800970526     C*------------------------------------------------------------------------*
009900000303     C* MAIN LINES
010000970526     C*------------------------------------------------------------------------*
010100970526     C*
010200120927     C* metto i default su D03
010300120927     C                   EXSR      InzD03
010400120927     C*
010500120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010600991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010900970526     C*
011000970526     C* EMETTE FORMATO VIDEO
011100970526     C                   EXSR      BUTFMR
011200970526     C*
011300970526     C* GESTIONE TASTI FUNZIONALI
011400970526     C                   EXSR      FUNCON
011500970526E   2C                   ENDDO
011600970526     C*
011700970526     C* FINE PGM
011800970526     C                   SETON                                        LR
011900970526     C*------------------------------------------------------------------------*
012000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012100970526     C*------------------------------------------------------------------------*
012200970526     C     BUTFMR        BEGSR
012300970526     C*
012400970526     C* EMETTE LA TESTATA
012500160601     C                   WRITE     ORMCND01
012600970526     C*
012700970526     C* SPEDISCE EVENTUALE MESSAGGIO
012800970526     C                   EXSR      SNDMSG
012900970526     C*
013000970526     C* EMETTE LE FUNZIONI
013100160601     C                   WRITE     ORMCND02
013200970526     C*
013300970526     C* EMETTE IL VIDEO
013400160601     C                   EXFMT     ORMCND03
013500970526     C*
013600991214     C* REIMPOSTA FLAG DI ERRORE
013700991214     C                   MOVEL     '0'           ERRL00
013800970526     C*
013900970526     C* REIMPOSTA INDICATORI DI ERRORE
014000020611     C                   SETOFF                                       555556
014100970526     C*
014200970526     C                   ENDSR
014300970526     C*------------------------------------------------------------------------*
014400120927     C* InzD03 - Inizializzo D03
014500970526     C*------------------------------------------------------------------------*
014600120927     C     InzD03        BEGSR
014700120927     C*
014800130108     C*
014900120927     C                   ENDSR
015000120927     C*------------------------------------------------------------------------*
015100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015200120927     C*------------------------------------------------------------------------*
015300120927     C     SNDMSG        BEGSR
015400120927     C*
015500970526     C* PULISCE IL SFILE MESSAGGI
015600970526     C                   MOVEL     '1'           *IN40
015700160601     C                   WRITE     ORMCNDMC
015800970526     C                   MOVEL     '0'           *IN40
015900970526     C*
016000970526     C* VISUALIZZA IL MESSAGGIO
016100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016200970526     C                   CALL      'XMSG'        XMSGPA
016300020611     C     DSMSER        IFEQ      *ZEROS
016400970526     C                   MOVEL     DSMSKE        MSGKEY
016500020611     C                   ENDIF
016600970526     C                   Z-ADD     1             MSGULT
016700160601     C                   WRITE     ORMCNDMS
016800160601     C                   WRITE     ORMCNDMC
016900970526     C*
017000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017100970526     C                   MOVEL     *BLANKS       DSMSMS
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* ENTGES - GESTIONE ENTER -> INVIO
017600970526     C*------------------------------------------------------------------------*
017700970526     C     ENTGES        BEGSR
017800970526     C*
017900970526     C* CONTROLLA I CAMPI IMMESSI
018000970526     C                   EXSR      DECODI
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* F03GES - GESTIONE F03 -> FINE
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F03GES        BEGSR
018700970526     C*
018800970526     C* IMPOSTA "PREMUTO F03"
018900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300991214     C* F06GES - GESTIONE F06 -> CONFERMA
019400970526     C*------------------------------------------------------------------------*
019500970526     C     F06GES        BEGSR
019600970526     C*
019700991214     C* CONTROLLA I CAMPI IMMESSI
019800991214     C                   EXSR      DECODI
019900020611     C*
020000991214     C* ESEGUE ROUTINE DI CONFERMA
020100020611     C     ERRL00        IFEQ      '0'
020200991214     C                   EXSR      ESEGUI
020300130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
020400121109     C                   MOVEL     MSG(20)       DSMSMS
020500121109     C* non esco mai dal pgm
020600121109     C***                MOVEL     '1'           FINL00
020700121108     C                   ENDIF
020800970526     C*
020900991214     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100130109     C* F07GES - GESTIONE F07 -> STAMPA
021200970526     C*------------------------------------------------------------------------*
021300130109     C     F07GES        BEGSR
021400970526     C*
021500140818     C                   EXSR      F06GES
021600130110     C*
021700970526     C                   ENDSR
021800130109     C*------------------------------------------------------------------------*
021900130109     C* F12GES - GESTIONE F12 -> RITORNO
022000130109     C*------------------------------------------------------------------------*
022100130109     C     F12GES        BEGSR
022200130109     C*
022300130109     C* IMPOSTA "PREMUTO F12"
022400130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022500130109     C*
022600130109     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022900970526     C*------------------------------------------------------------------------*
023000970526     C     F24GES        BEGSR
023100970526     C*
023200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023300970526     C*
023400970526     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
023700970526     C*------------------------------------------------------------------------*
023800991214     C     ESEGUI        BEGSR
023900140120     C*
024000160217     C* richiamo il driver
024100160601     C                   EVAL      pIn_Opz     = d03Opz
024200160608     C                   EVAL      pIn_DS_FNVAO.VAOPOE = D03POE
024300160608     C                   EVAL      pIn_DS_FNVAO.VAOTOR = D03TOR
024400160608     C                   EVAL      pIn_DS_FNVAO.VAOPKG = D03PKG
024500160610     C                   EVAL      pIn_DS_FNVAO.vaodao = D03DAO
024600160610     C                   EVAL      pIn_DS_FNVAO.vaooao = D03OAO
024700160601     C                   EVAL      pIn_DS_FNVAO.vaocor = *all'0'
024800160601     C                   EVAL      pIn_DS_FNVAO.vaocra = *all'0'
024900160601     C                   EVAL      pIn_DS_FNVAO.vaodar = *all'0'
025000160601     C                   EVAL      pIn_DS_FNVAO.vaoorr = *all'0'
025100160601     C                   EVAL      pIn_DS_FNVAO.vaoncl = *all'0'
025200160601     C                   EVAL      pIn_DS_FNVAO.vaoksc = *all'0'
025300160601     c                   clear                   pOut_ESITO
025400160601     c                   clear                   pOut_Count
025500121108     C* chiamo
025600160817     C***                CALL      'UBORMCNTR'
025700160819     c*                  CALLB     'UBORMCNTR'
025800160819     c*                  parm                    pIn_Opz
025900160819     c*                  parm                    pIn_DS_FNVAO
026000160819     c*                  parm                    pOut_Count
026100160819     c*                  parm                    pOut_Esito
026200160819     C                   CALLP     UBORMCNTR
026300160819     c                                          (pIn_Opz:
026400160819     c                                           pIn_DS_FNVAO:
026500160819     c                                           pOut_Count:
026600160819     c                                           pOut_Esito)
026700121108     C*
026800160601     c                   EVAL      D03Esito = pOut_Esito
026900160217     C* valorizzo l'output solo se l'esito è OK
027000160601     c                   IF        D03Esito='1'
027100160601     C                   eval      d03Count =  pOut_Count
027200160223     C                   ENDIF
027300991214     C*
027400970526     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
027700970526     C*------------------------------------------------------------------------*
027800970526     C     DECODI        BEGSR
027900121019     C*
028000160217     C                   SETOFF                                       505152
028100160601     C                   clear                   pOut_Esito
028200160601     C                   clear                   pOut_Count
028300120927     C*
028400970528     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
028700970526     C*------------------------------------------------------------------------*
028800970526     C     AZZVID        BEGSR
028900970526     C* CAMPI
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
029400970526     C*------------------------------------------------------------------------*
029500970526     C     IMPFUN        BEGSR
029600970526     C*
029700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
029800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
029900970526     C                   EXSR      FUNGES
030000970526     C*
030100970526     C                   ENDSR
030200020923     C*--------------------------------------------------------------------------------------------*
030300020923     C* REPERISCE I DATI UTENTE
030400020923     C*--------------------------------------------------------------------------------------------*
030500020923     C     REPDATIUTE    BEGSR
030600020923     C*
030700020923     C* INIZIALIZZA VARIABILI DI WRK
030800020923     C                   CLEAR                   TIBS34DS
030900020923     C                   CLEAR                   AZUTEDS
031000020923     C                   CLEAR                   DDATIUTE
031100020923     C*
031200020923     C     *DTAARA       DEFINE    §azute        azuteds
031300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
031400020923     C                   IN(E)     *DTAARA
031500020923if  1C                   IF        %Error
031600020923     c                   EVAL      I34Tla = 'L'
031700020923     C                   CALL      'TIBS34R'
031800020923     C                   PARM                    Tibs34Ds
031900020923     C                   IN        *DTAARA
032000020923e   1C                   ENDIF
032100020923     C*
032200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
032300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
032400020923     C*
032500020923     C                   ENDSR
032600970526     C*------------------------------------------------------------------------*
032700970526     C* *INZSR - OPERAZIONI INIZIALI
032800970526     C*------------------------------------------------------------------------*
032900970526     C     *INZSR        BEGSR
033000991217     C*
033100991217     C     *ENTRY        PLIST
033200991217     C                   PARM                    KPJBA
033300020923     C*
033400020923     C* REPERISCE I DATI DELL'UTENTE
033500020923     C                   EXSR      REPDATIUTE
033600970526     C*
033700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
033800970526     C     XMSGPA        PLIST
033900970526     C                   PARM                    DSMSMS
034000970526     C                   PARM                    DSMSPG
034100970526     C                   PARM                    DSMSKE
034200970526     C                   PARM                    DSMSER
034300970526     C*
034400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
034500970526     C                   CALL      'XKEY'
034600970526     C                   PARM                    DSKEY
034700970526     C*---
034800970526     C* VARIABILI NON RIFERITE AL DATA BASE
034900970526     C*---
035000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
035100991214     C                   MOVEL     *BLANKS       DEPOP1            3
035200970526     C*---
035300970526     C* VARIABILI DI CONTROLLO
035400970526     C*---
035500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
035600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
035700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
035800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
035900970526     C*
036000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
036100160601     C                   MOVEL     'UBORMCNR1'   DSMSPG
036200160601     C                   MOVEL     'UBORMCNR1'   MSGQUE
036300970526     C                   MOVEL     *BLANKS       MSGKEY
036400970526     C                   MOVEL     *BLANKS       DSMSMS
036500970528     C*
036600970528     C* CALCOLA LA DATA CORRENTE
036700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
036800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
036900970528     C                   Z-ADD     WN8           G08DAT
037000970528     C                   Z-ADD     *ZEROS        G08INV
037100970528     C                   MOVEL     '0'           G08ERR
037200970528     C                   CALL      'XSRDA8'
037300970528     C                   PARM                    WLBDA8
037400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
037500000310     C*
037600000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
037700000310     C                   EXSR      AZZVID
037800970526     C*
037900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
038000970526     C                   EXSR      IMPFUN
038100970526     C*
038200970526     C                   ENDSR
038300970526     C*------------------------------------------------------------------------*
038400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
038500970526     C*------------------------------------------------------------------------*
038600970526     C     FUNGES        BEGSR
038700970526     C*
038800970526     C* PULISCO CAMPI DI LAVORO
038900970526     C                   Z-ADD     0             W                 5 0
039000970526     C*
039100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
039200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
039300970526     C     1             DO        24            I
039400970526     C                   MOVEL     FUD(I)        FUDRIG
039500970526     C*
039600970526     C* PULIZIA FUNZIONI PRECEDENTI
039700970526     C                   MOVEL     *BLANKS       FUV(I)
039800970526     C*
039900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
040000970526     C* (SALTA F24)
040100970526     C     FUDATT        IFEQ      'S'
040200970526     C     FUDFUN        ANDNE     '24'
040300970526     C                   ADD       1             W
040400970526     C                   MOVEL     FUDDES        FUV(W)
040500970526     C                   END
040600970526     C                   END                                                    DO WHILE
040700970526     C*
040800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
040900970526     C* ALTRIMENTI DISATTIVO F24
041000970526     C     1             DO        24            I
041100970526     C                   MOVEL     FUD(I)        FUDRIG
041200970526     C*
041300970526     C     FUDFUN        IFEQ      '24'
041400970526     C     W             IFGT      6
041500970526     C                   MOVEL     'S'           FUDATT
041600970526     C                   ELSE
041700970526     C                   MOVEL     *BLANKS       FUDATT
041800970526     C                   END                                                    W GT 8
041900970526     C                   MOVEL     FUDRIG        FUD(I)
042000970526     C                   END
042100970526     C                   END                                                    DO WHILE
042200970526     C*
042300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
042400970526     C     W             DIV       6             I
042500970526     C                   MVR                     Z
042600970526     C*
042700970526     C     Z             IFNE      0
042800970526     C     I             ADD       1             Z
042900970526     C                   ELSE
043000970526     C                   Z-ADD     I             Z                 5 0
043100970526     C                   END
043200970526     C*
043300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
043400970526     C                   ADD       1             FUNGRU
043500970526     C     FUNGRU        IFGT      Z
043600970526     C     FUNGRU        ORLT      1
043700970526     C                   Z-ADD     1             FUNGRU
043800970526     C                   END
043900970526     C*
044000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
044100970526     C     FUNGRU        IFEQ      1
044200970526     C                   MOVEL     FUV(1)        FUN001
044300970526     C                   MOVEL     FUV(2)        FUN002
044400970526     C                   MOVEL     FUV(3)        FUN003
044500970526     C                   MOVEL     FUV(4)        FUN004
044600970526     C                   MOVEL     FUV(5)        FUN005
044700970526     C                   MOVEL     FUV(6)        FUN006
044800970526     C                   END
044900970526     C*
045000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
045100970526     C     FUNGRU        IFEQ      2
045200970526     C                   MOVEL     FUV(7)        FUN001
045300970526     C                   MOVEL     FUV(8)        FUN002
045400970526     C                   MOVEL     FUV(9)        FUN003
045500970526     C                   MOVEL     FUV(10)       FUN004
045600970526     C                   MOVEL     FUV(11)       FUN005
045700970526     C                   MOVEL     FUV(12)       FUN006
045800970526     C                   END
045900970526     C*
046000970526     C* TERZO GRUPPO DI 6 FUNZIONI
046100970526     C     FUNGRU        IFEQ      3
046200970526     C                   MOVEL     FUV(13)       FUN001
046300970526     C                   MOVEL     FUV(14)       FUN002
046400970526     C                   MOVEL     FUV(15)       FUN003
046500970526     C                   MOVEL     FUV(16)       FUN004
046600970526     C                   MOVEL     FUV(17)       FUN005
046700970526     C                   MOVEL     FUV(18)       FUN006
046800970526     C                   END
046900970526     C*
047000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
047100970526     C     FUNGRU        IFEQ      4
047200970526     C                   MOVEL     FUV(19)       FUN001
047300970526     C                   MOVEL     FUV(20)       FUN002
047400970526     C                   MOVEL     FUV(21)       FUN003
047500970526     C                   MOVEL     FUV(22)       FUN004
047600970526     C                   MOVEL     *BLANKS       FUN005
047700970526     C                   MOVEL     *BLANKS       FUN006
047800970526     C                   END
047900970526     C*
048000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
048100970526     C                   MOVEL     *BLANKS       FUN007
048200970526     C                   MOVEL     *BLANKS       FUN008
048300970526     C*
048400970526     C     1             DO        24            I
048500970526     C                   MOVEL     FUD(I)        FUDRIG
048600970526     C     FUDFUN        IFEQ      '24'
048700970526     C     FUDATT        ANDEQ     'S'
048800970526     C                   MOVEL     FUDDES        FUN008
048900970526     C                   END
049000970526     C                   END                                                    DO
049100970526     C*
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
049500970526     C*------------------------------------------------------------------------*
049600970526     C     FUNCON        BEGSR
049700970526     C*
049800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
049900970526     C                   EXSR      FUNVAR
050000970526     C*
050100970526     C* PULISCO CAMPI DI LAVORO
050200970526     C                   Z-ADD     0             I                 5 0
050300970526     C                   Z-ADD     0             W                 5 0
050400970526     C*
050500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
050600970526     C     FUNKEY        IFNE      *BLANKS
050700970526     C     1             DO        24            I
050800970526     C                   MOVEL     FUD(I)        FUDRIG
050900970526     C*
051000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
051100970526     C     FUNKEY        IFEQ      FUDFUN
051200970526     C     FUDATT        ANDEQ     'S'
051300970526     C                   Z-ADD     I             W
051400970526     C                   END
051500970526     C                   END                                                    DO WHILE
051600970526     C*
051700970526     C* FUNZIONE NON ATTIVA (ERRORE)
051800970526     C     W             IFEQ      0
051900970526     C*
052000970526     C* EMISSIONE MESSAGGIO DI ERRORE
052100970526     C                   MOVEL     MSG(1)        DSMSMS
052200970526     C*
052300970526     C                   END                                                    W=0
052400970526     C                   END                                                    FUNKEY NE *BLANKS
052500970526     C*
052600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
052700970526     C     W             IFNE      0
052800970526     C     FUNKEY        OREQ      *BLANKS
052900970526     C     FUNKEY        IFEQ      *BLANKS
053000970526     C                   EXSR      ENTGES
053100970526     C                   END
053200970526     C     FUNKEY        IFEQ      '03'
053300970526     C                   EXSR      F03GES
053400970526     C                   END
053500970526     C     FUNKEY        IFEQ      '06'
053600970526     C                   EXSR      F06GES
053700970526     C                   END
053800130109     C     FUNKEY        IFEQ      '07'
053900130109     C                   EXSR      F07GES
054000130109     C                   END
054100970526     C     FUNKEY        IFEQ      '12'
054200970526     C                   EXSR      F12GES
054300970526     C                   END
054400970526     C     FUNKEY        IFEQ      '24'
054500970526     C                   EXSR      F24GES
054600970526     C                   END
054700970526     C                   END
054800970526     C*
054900970526     C                   ENDSR
055000970526     C*------------------------------------------------------------------------*
055100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
055200970526     C*------------------------------------------------------------------------*
055300970526     C     FUNVAR        BEGSR
055400970526     C*
055500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
055600970526     C*
055700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
055800970526     C     KEY           IFEQ      ENTER
055900970526     C                   MOVEL     *BLANKS       FUNKEY
056000970526     C                   END
056100970526     C     KEY           IFEQ      F1
056200970526     C                   MOVEL     '01'          FUNKEY
056300970526     C                   END
056400970526     C     KEY           IFEQ      F2
056500970526     C                   MOVEL     '02'          FUNKEY
056600970526     C                   END
056700970526     C     KEY           IFEQ      F3
056800970526     C                   MOVEL     '03'          FUNKEY
056900970526     C                   END
057000970526     C     KEY           IFEQ      F4
057100970526     C                   MOVEL     '04'          FUNKEY
057200970526     C                   END
057300970526     C     KEY           IFEQ      F5
057400970526     C                   MOVEL     '05'          FUNKEY
057500970526     C                   END
057600970526     C     KEY           IFEQ      F6
057700970526     C                   MOVEL     '06'          FUNKEY
057800970526     C                   END
057900970526     C     KEY           IFEQ      F7
058000970526     C                   MOVEL     '07'          FUNKEY
058100970526     C                   END
058200970526     C     KEY           IFEQ      F8
058300970526     C                   MOVEL     '08'          FUNKEY
058400970526     C                   END
058500970526     C     KEY           IFEQ      F9
058600970526     C                   MOVEL     '09'          FUNKEY
058700970526     C                   END
058800970526     C     KEY           IFEQ      F10
058900970526     C                   MOVEL     '10'          FUNKEY
059000970526     C                   END
059100970526     C     KEY           IFEQ      F11
059200970526     C                   MOVEL     '11'          FUNKEY
059300970526     C                   END
059400970526     C     KEY           IFEQ      F12
059500970526     C                   MOVEL     '12'          FUNKEY
059600970526     C                   END
059700970526     C     KEY           IFEQ      F13
059800970526     C                   MOVEL     '13'          FUNKEY
059900970526     C                   END
060000970526     C     KEY           IFEQ      F14
060100970526     C                   MOVEL     '14'          FUNKEY
060200970526     C                   END
060300970526     C     KEY           IFEQ      F15
060400970526     C                   MOVEL     '15'          FUNKEY
060500970526     C                   END
060600970526     C     KEY           IFEQ      F16
060700970526     C                   MOVEL     '16'          FUNKEY
060800970526     C                   END
060900970526     C     KEY           IFEQ      F17
061000970526     C                   MOVEL     '17'          FUNKEY
061100970526     C                   END
061200970526     C     KEY           IFEQ      F18
061300970526     C                   MOVEL     '18'          FUNKEY
061400970526     C                   END
061500970526     C     KEY           IFEQ      F19
061600970526     C                   MOVEL     '19'          FUNKEY
061700970526     C                   END
061800970526     C     KEY           IFEQ      F20
061900970526     C                   MOVEL     '20'          FUNKEY
062000970526     C                   END
062100970526     C     KEY           IFEQ      F21
062200970526     C                   MOVEL     '21'          FUNKEY
062300970526     C                   END
062400970526     C     KEY           IFEQ      F22
062500970526     C                   MOVEL     '22'          FUNKEY
062600970526     C                   END
062700970526     C     KEY           IFEQ      F23
062800970526     C                   MOVEL     '23'          FUNKEY
062900970526     C                   END
063000970526     C     KEY           IFEQ      F24
063100970526     C                   MOVEL     '24'          FUNKEY
063200970526     C                   END
063300970526     C     KEY           IFEQ      ROLLUP
063400970526     C                   MOVEL     'RU'          FUNKEY
063500970526     C                   END
063600970526     C     KEY           IFEQ      HELP
063700970526     C                   MOVEL     'HP'          FUNKEY
063800970526     C                   END
063900970526     C                   ENDSR
064000970526     C*------------------------------------------------------------------------*
064100970526     C* NOTE COSTRUZIONE SCHIERE
064200970526     C*------------------------------------------------------------------------*
064300970526     C*
064400970526     C* -MSG-MESSAGGI
064500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
064600970526     C* (NON TOCCARE MSG,1 E MSG,2)
064700970526     C*
064800970526     C* -CMD-COMANDI
064900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
065000970526     C*
065100970526     C* -OPD-OPZIONI
065200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
065300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
065400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
065500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
065600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
065700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
065800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
065900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
066000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
066200970526     C*
066300970526     C* -FUD-FUNZIONI
066400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
066500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
067000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
067100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
067200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
067300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
067400970526** MSG - MESSAGGI
067500120927Tasto funzionale non gestito                                                   1
067600160217                                                                               2
067700160217                                                                               3
067800160217                                                                               4
067900160217                                                                               5
068000160217                                                                               6
068100160217                                                                               7
068200130108                                                                               8
068300120927                                                                               9
068400120927                                                                               10
068500120927                                                                               11
068600120927                                                                               12
068700120927                                                                               13
068800120927Data errata                                                                    14
068900140116                                                                               15
069000120927                                                                               16
069100120927                                                                               17
069200120927                                                                               18
069300120927                                                                               19
069400160217Esecuzione  conclusa - controllare l'esito                                     20
069500160217                                                                               21
069600160217                                                                               22
069700121108                                                                               23
069800121108                                                                               24
069900970526** FUD - FUNZIONI
070000970526                   -        (S/N)=N                        funz.01¬
070100970526                   -        (S/N)=N                        funz.02¬
070200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
070300970526                   -        (S/N)=N                        funz.04¬
070400970526                   -        (S/N)=N                        funz.05¬
070500991214F06=Conferma       -        (S/N)=S                        funz.06¬
070600160217                   -        (S/N)=S                        funz.07¬
070700970527                   -        (S/N)=N                        funz.08¬
070800970527                   -        (S/N)=N                        funz.09¬
070900970527                   -        (S/N)=N                        funz.10¬
071000970526                   -        (S/N)=N                        funz.11¬
071100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
071200970526                   -        (S/N)=N                        funz.13¬
071300970526                   -        (S/N)=N                        funz.14¬
071400970526                   -        (S/N)=N                        funz.15¬
071500970526                   -        (S/N)=N                        funz.16¬
071600970526                   -        (S/N)=N                        funz.17¬
071700970526                   -        (S/N)=N                        funz.18¬
071800970526                   -        (S/N)=N                        funz.19¬
071900970526                   -        (S/N)=N                        funz.20¬
072000970526                   -        (S/N)=N                        funz.21¬
072100970526                   -        (S/N)=N                        funz.22¬
072200970526                   -        (S/N)=N                        funz.23¬
072300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
