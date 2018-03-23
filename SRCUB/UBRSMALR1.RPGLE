000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400170216     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000500170216     H BNDDIR('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900170216     FUBRSMALD1 cf   e             WORKSTN INFDS(DEVDS1)
001000170216     F                                     SFILE(RSMALDMS:MSGULT)
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
006900170216     D UBRSMALDS     E DS                  QUALIFIED
007000120928     D*-------------------
007100120928
007200121003     d Status         sds
007300121003     d  SDSpgm           *proc
007400121003     d  SDSprm           *parms
007500121003     d  SDSuser              254    263
007600130116
007700130116     d ora             s               t
007800140117     d wX              s              2s 0
007900150407     d wVAB            s               n
008000150407     d wVAO            s               n
008100160223
008200160223       //--------------------------------------------------------------
008300160223       // Definizione prototipi procedure.
008400160223       //--------------------------------------------------------------
008500160223
008600170216     d/copy gaitrasrc/srcProtoPr,UBRSMALR
008700120928
008800970526     C*------------------------------------------------------------------------*
008900000303     C* MAIN LINES
009000970526     C*------------------------------------------------------------------------*
009100970526     C*
009200120927     C* metto i default su D03
009300120927     C                   EXSR      InzD03
009400120927     C*
009500120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009600991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009900970526     C*
010000970526     C* EMETTE FORMATO VIDEO
010100970526     C                   EXSR      BUTFMR
010200970526     C*
010300970526     C* GESTIONE TASTI FUNZIONALI
010400970526     C                   EXSR      FUNCON
010500970526E   2C                   ENDDO
010600970526     C*
010700970526     C* FINE PGM
010800970526     C                   SETON                                        LR
010900970526     C*------------------------------------------------------------------------*
011000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011100970526     C*------------------------------------------------------------------------*
011200970526     C     BUTFMR        BEGSR
011300970526     C*
011400970526     C* EMETTE LA TESTATA
011500170216     C                   WRITE     RSMALD01
011600970526     C*
011700970526     C* SPEDISCE EVENTUALE MESSAGGIO
011800970526     C                   EXSR      SNDMSG
011900970526     C*
012000970526     C* EMETTE LE FUNZIONI
012100170216     C                   WRITE     RSMALD02
012200970526     C*
012300970526     C* EMETTE IL VIDEO
012400170216     C                   EXFMT     RSMALD03
012500970526     C*
012600991214     C* REIMPOSTA FLAG DI ERRORE
012700991214     C                   MOVEL     '0'           ERRL00
012800970526     C*
012900970526     C* REIMPOSTA INDICATORI DI ERRORE
013000020611     C                   SETOFF                                       555556
013100970526     C*
013200970526     C                   ENDSR
013300970526     C*------------------------------------------------------------------------*
013400120927     C* InzD03 - Inizializzo D03
013500970526     C*------------------------------------------------------------------------*
013600120927     C     InzD03        BEGSR
013700120927     C*
013800130108     C*
013900120927     C                   ENDSR
014000120927     C*------------------------------------------------------------------------*
014100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014200120927     C*------------------------------------------------------------------------*
014300120927     C     SNDMSG        BEGSR
014400120927     C*
014500970526     C* PULISCE IL SFILE MESSAGGI
014600970526     C                   MOVEL     '1'           *IN40
014700170216     C                   WRITE     RSMALDMC
014800970526     C                   MOVEL     '0'           *IN40
014900970526     C*
015000970526     C* VISUALIZZA IL MESSAGGIO
015100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015200970526     C                   CALL      'XMSG'        XMSGPA
015300020611     C     DSMSER        IFEQ      *ZEROS
015400970526     C                   MOVEL     DSMSKE        MSGKEY
015500020611     C                   ENDIF
015600970526     C                   Z-ADD     1             MSGULT
015700170216     C                   WRITE     RSMALDMS
015800170216     C                   WRITE     RSMALDMC
015900970526     C*
016000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016100970526     C                   MOVEL     *BLANKS       DSMSMS
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* ENTGES - GESTIONE ENTER -> INVIO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     ENTGES        BEGSR
016800970526     C*
016900970526     C* CONTROLLA I CAMPI IMMESSI
017000970526     C                   EXSR      DECODI
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300991214     C* F06GES - GESTIONE F06 -> CONFERMA
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F06GES        BEGSR
018600970526     C*
018700991214     C* CONTROLLA I CAMPI IMMESSI
018800991214     C                   EXSR      DECODI
018900020611     C*
019000991214     C* ESEGUE ROUTINE DI CONFERMA
019100020611     C     ERRL00        IFEQ      '0'
019200991214     C                   EXSR      ESEGUI
019300130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
019400121109     C                   MOVEL     MSG(20)       DSMSMS
019500121109     C* non esco mai dal pgm
019600121109     C***                MOVEL     '1'           FINL00
019700121108     C                   ENDIF
019800970526     C*
019900991214     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100130109     C* F07GES - GESTIONE F07 -> STAMPA
020200970526     C*------------------------------------------------------------------------*
020300130109     C     F07GES        BEGSR
020400970526     C*
020500140818     C                   EXSR      F06GES
020600130110     C*
020700970526     C                   ENDSR
020800130109     C*------------------------------------------------------------------------*
020900130109     C* F12GES - GESTIONE F12 -> RITORNO
021000130109     C*------------------------------------------------------------------------*
021100130109     C     F12GES        BEGSR
021200130109     C*
021300130109     C* IMPOSTA "PREMUTO F12"
021400130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021500130109     C*
021600130109     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021900970526     C*------------------------------------------------------------------------*
022000970526     C     F24GES        BEGSR
022100970526     C*
022200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
022700970526     C*------------------------------------------------------------------------*
022800991214     C     ESEGUI        BEGSR
022900140120     C*
023000160217     C* richiamo il driver
023100170216     C                   EVAL      UBRSMALDS.iTpRic     = d03TpRic
023200170216     C                   EVAL      UBRSMALDS.iOrigine   = d03Origine
023300170216     C                   EVAL      UBRSMALDS.iAAS       = d03AAS
023400170216     C                   EVAL      UBRSMALDS.iLNP       = d03LNP
023500170216     C                   EVAL      UBRSMALDS.iNRS       = d03NRS
023600170216     C                   EVAL      UBRSMALDS.iNSP       = d03NSP
023700170216        clear     UBRSMALDS.oEsito;
023800170216        clear     UBRSMALDS.oRagSoc;
023900170517        clear     UBRSMALDS.oINM   ;
024000170517        clear     UBRSMALDS.oCAM   ;
024100170517        clear     UBRSMALDS.oLOM   ;
024200170517        clear     UBRSMALDS.oPRM   ;
024300170517        clear     UBRSMALDS.oNZM   ;
024400121108     C* chiamo
024500170216         UBRSMALR_S ( UBRSMALDS );
024600121108     C*
024700170216     c                   EVAL      D03Esito = UBRSMALDS.oEsito
024800160217     C* valorizzo l'output solo se l'esito è OK
024900170216     c                   IF        D03Esito=0
025000170216     C                   eval      d03RagSoc = UBRSMALDS.oRagSoc
025100170517     C                   eval      d03INM    = UBRSMALDS.oINM
025200170517     C                   eval      d03CAM    = UBRSMALDS.oCAM
025300170517     C                   eval      d03LOM    = UBRSMALDS.oLOM
025400170517     C                   eval      d03PRM    = UBRSMALDS.oPRM
025500170517     C                   eval      d03NZM    = UBRSMALDS.oNZM
025600160223     C                   ELSE
025700170216     C                   eval      d03RagSoc = *blank
025800170517        clear     UBRSMALDS.oINM   ;
025900170517        clear     UBRSMALDS.oCAM   ;
026000170517        clear     UBRSMALDS.oLOM   ;
026100170517        clear     UBRSMALDS.oPRM   ;
026200170517        clear     UBRSMALDS.oNZM   ;
026300160223     C                   ENDIF
026400991214     C*
026500970526     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026800970526     C*------------------------------------------------------------------------*
026900970526     C     DECODI        BEGSR
027000121019     C*
027100160217     C                   SETOFF                                       505152
027200170216        clear UBRSMALDS.oRagSoc;
027300170517        clear     UBRSMALDS.oINM   ;
027400170517        clear     UBRSMALDS.oCAM   ;
027500170517        clear     UBRSMALDS.oLOM   ;
027600170517        clear     UBRSMALDS.oPRM   ;
027700170517        clear     UBRSMALDS.oNZM   ;
027800120927     C*
027900970528     C                   ENDSR
028000970526     C*------------------------------------------------------------------------*
028100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
028200970526     C*------------------------------------------------------------------------*
028300970526     C     AZZVID        BEGSR
028400970526     C* CAMPI
028500970526     C*
028600970526     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
028900970526     C*------------------------------------------------------------------------*
029000970526     C     IMPFUN        BEGSR
029100970526     C*
029200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
029300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
029400970526     C                   EXSR      FUNGES
029500970526     C*
029600970526     C                   ENDSR
029700020923     C*--------------------------------------------------------------------------------------------*
029800020923     C* REPERISCE I DATI UTENTE
029900020923     C*--------------------------------------------------------------------------------------------*
030000020923     C     REPDATIUTE    BEGSR
030100020923     C*
030200020923     C* INIZIALIZZA VARIABILI DI WRK
030300020923     C                   CLEAR                   TIBS34DS
030400020923     C                   CLEAR                   AZUTEDS
030500020923     C                   CLEAR                   DDATIUTE
030600020923     C*
030700020923     C     *DTAARA       DEFINE    §azute        azuteds
030800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
030900020923     C                   IN(E)     *DTAARA
031000020923if  1C                   IF        %Error
031100020923     c                   EVAL      I34Tla = 'L'
031200020923     C                   CALL      'TIBS34R'
031300020923     C                   PARM                    Tibs34Ds
031400020923     C                   IN        *DTAARA
031500020923e   1C                   ENDIF
031600020923     C*
031700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
031800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
031900020923     C*
032000020923     C                   ENDSR
032100970526     C*------------------------------------------------------------------------*
032200970526     C* *INZSR - OPERAZIONI INIZIALI
032300970526     C*------------------------------------------------------------------------*
032400970526     C     *INZSR        BEGSR
032500991217     C*
032600991217     C     *ENTRY        PLIST
032700991217     C                   PARM                    KPJBA
032800020923     C*
032900020923     C* REPERISCE I DATI DELL'UTENTE
033000020923     C                   EXSR      REPDATIUTE
033100970526     C*
033200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
033300970526     C     XMSGPA        PLIST
033400970526     C                   PARM                    DSMSMS
033500970526     C                   PARM                    DSMSPG
033600970526     C                   PARM                    DSMSKE
033700970526     C                   PARM                    DSMSER
033800970526     C*
033900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
034000970526     C                   CALL      'XKEY'
034100970526     C                   PARM                    DSKEY
034200970526     C*---
034300970526     C* VARIABILI NON RIFERITE AL DATA BASE
034400970526     C*---
034500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
034600991214     C                   MOVEL     *BLANKS       DEPOP1            3
034700970526     C*---
034800970526     C* VARIABILI DI CONTROLLO
034900970526     C*---
035000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
035100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
035200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
035300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
035400970526     C*
035500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
035600170216     C                   MOVEL     'UBRSMALR1'   DSMSPG
035700170216     C                   MOVEL     'UBRSMALR1'   MSGQUE
035800970526     C                   MOVEL     *BLANKS       MSGKEY
035900970526     C                   MOVEL     *BLANKS       DSMSMS
036000970528     C*
036100970528     C* CALCOLA LA DATA CORRENTE
036200970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
036300970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
036400970528     C                   Z-ADD     WN8           G08DAT
036500970528     C                   Z-ADD     *ZEROS        G08INV
036600970528     C                   MOVEL     '0'           G08ERR
036700970528     C                   CALL      'XSRDA8'
036800970528     C                   PARM                    WLBDA8
036900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
037000000310     C*
037100000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
037200000310     C                   EXSR      AZZVID
037300970526     C*
037400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
037500970526     C                   EXSR      IMPFUN
037600970526     C*
037700970526     C                   ENDSR
037800970526     C*------------------------------------------------------------------------*
037900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
038000970526     C*------------------------------------------------------------------------*
038100970526     C     FUNGES        BEGSR
038200970526     C*
038300970526     C* PULISCO CAMPI DI LAVORO
038400970526     C                   Z-ADD     0             W                 5 0
038500970526     C*
038600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
038700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
038800970526     C     1             DO        24            I
038900970526     C                   MOVEL     FUD(I)        FUDRIG
039000970526     C*
039100970526     C* PULIZIA FUNZIONI PRECEDENTI
039200970526     C                   MOVEL     *BLANKS       FUV(I)
039300970526     C*
039400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
039500970526     C* (SALTA F24)
039600970526     C     FUDATT        IFEQ      'S'
039700970526     C     FUDFUN        ANDNE     '24'
039800970526     C                   ADD       1             W
039900970526     C                   MOVEL     FUDDES        FUV(W)
040000970526     C                   END
040100970526     C                   END                                                    DO WHILE
040200970526     C*
040300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
040400970526     C* ALTRIMENTI DISATTIVO F24
040500970526     C     1             DO        24            I
040600970526     C                   MOVEL     FUD(I)        FUDRIG
040700970526     C*
040800970526     C     FUDFUN        IFEQ      '24'
040900970526     C     W             IFGT      6
041000970526     C                   MOVEL     'S'           FUDATT
041100970526     C                   ELSE
041200970526     C                   MOVEL     *BLANKS       FUDATT
041300970526     C                   END                                                    W GT 8
041400970526     C                   MOVEL     FUDRIG        FUD(I)
041500970526     C                   END
041600970526     C                   END                                                    DO WHILE
041700970526     C*
041800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
041900970526     C     W             DIV       6             I
042000970526     C                   MVR                     Z
042100970526     C*
042200970526     C     Z             IFNE      0
042300970526     C     I             ADD       1             Z
042400970526     C                   ELSE
042500970526     C                   Z-ADD     I             Z                 5 0
042600970526     C                   END
042700970526     C*
042800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
042900970526     C                   ADD       1             FUNGRU
043000970526     C     FUNGRU        IFGT      Z
043100970526     C     FUNGRU        ORLT      1
043200970526     C                   Z-ADD     1             FUNGRU
043300970526     C                   END
043400970526     C*
043500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
043600970526     C     FUNGRU        IFEQ      1
043700970526     C                   MOVEL     FUV(1)        FUN001
043800970526     C                   MOVEL     FUV(2)        FUN002
043900970526     C                   MOVEL     FUV(3)        FUN003
044000970526     C                   MOVEL     FUV(4)        FUN004
044100970526     C                   MOVEL     FUV(5)        FUN005
044200970526     C                   MOVEL     FUV(6)        FUN006
044300970526     C                   END
044400970526     C*
044500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
044600970526     C     FUNGRU        IFEQ      2
044700970526     C                   MOVEL     FUV(7)        FUN001
044800970526     C                   MOVEL     FUV(8)        FUN002
044900970526     C                   MOVEL     FUV(9)        FUN003
045000970526     C                   MOVEL     FUV(10)       FUN004
045100970526     C                   MOVEL     FUV(11)       FUN005
045200970526     C                   MOVEL     FUV(12)       FUN006
045300970526     C                   END
045400970526     C*
045500970526     C* TERZO GRUPPO DI 6 FUNZIONI
045600970526     C     FUNGRU        IFEQ      3
045700970526     C                   MOVEL     FUV(13)       FUN001
045800970526     C                   MOVEL     FUV(14)       FUN002
045900970526     C                   MOVEL     FUV(15)       FUN003
046000970526     C                   MOVEL     FUV(16)       FUN004
046100970526     C                   MOVEL     FUV(17)       FUN005
046200970526     C                   MOVEL     FUV(18)       FUN006
046300970526     C                   END
046400970526     C*
046500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
046600970526     C     FUNGRU        IFEQ      4
046700970526     C                   MOVEL     FUV(19)       FUN001
046800970526     C                   MOVEL     FUV(20)       FUN002
046900970526     C                   MOVEL     FUV(21)       FUN003
047000970526     C                   MOVEL     FUV(22)       FUN004
047100970526     C                   MOVEL     *BLANKS       FUN005
047200970526     C                   MOVEL     *BLANKS       FUN006
047300970526     C                   END
047400970526     C*
047500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
047600970526     C                   MOVEL     *BLANKS       FUN007
047700970526     C                   MOVEL     *BLANKS       FUN008
047800970526     C*
047900970526     C     1             DO        24            I
048000970526     C                   MOVEL     FUD(I)        FUDRIG
048100970526     C     FUDFUN        IFEQ      '24'
048200970526     C     FUDATT        ANDEQ     'S'
048300970526     C                   MOVEL     FUDDES        FUN008
048400970526     C                   END
048500970526     C                   END                                                    DO
048600970526     C*
048700970526     C                   ENDSR
048800970526     C*------------------------------------------------------------------------*
048900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
049000970526     C*------------------------------------------------------------------------*
049100970526     C     FUNCON        BEGSR
049200970526     C*
049300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
049400970526     C                   EXSR      FUNVAR
049500970526     C*
049600970526     C* PULISCO CAMPI DI LAVORO
049700970526     C                   Z-ADD     0             I                 5 0
049800970526     C                   Z-ADD     0             W                 5 0
049900970526     C*
050000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
050100970526     C     FUNKEY        IFNE      *BLANKS
050200970526     C     1             DO        24            I
050300970526     C                   MOVEL     FUD(I)        FUDRIG
050400970526     C*
050500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
050600970526     C     FUNKEY        IFEQ      FUDFUN
050700970526     C     FUDATT        ANDEQ     'S'
050800970526     C                   Z-ADD     I             W
050900970526     C                   END
051000970526     C                   END                                                    DO WHILE
051100970526     C*
051200970526     C* FUNZIONE NON ATTIVA (ERRORE)
051300970526     C     W             IFEQ      0
051400970526     C*
051500970526     C* EMISSIONE MESSAGGIO DI ERRORE
051600970526     C                   MOVEL     MSG(1)        DSMSMS
051700970526     C*
051800970526     C                   END                                                    W=0
051900970526     C                   END                                                    FUNKEY NE *BLANKS
052000970526     C*
052100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
052200970526     C     W             IFNE      0
052300970526     C     FUNKEY        OREQ      *BLANKS
052400970526     C     FUNKEY        IFEQ      *BLANKS
052500970526     C                   EXSR      ENTGES
052600970526     C                   END
052700970526     C     FUNKEY        IFEQ      '03'
052800970526     C                   EXSR      F03GES
052900970526     C                   END
053000970526     C     FUNKEY        IFEQ      '06'
053100970526     C                   EXSR      F06GES
053200970526     C                   END
053300130109     C     FUNKEY        IFEQ      '07'
053400130109     C                   EXSR      F07GES
053500130109     C                   END
053600970526     C     FUNKEY        IFEQ      '12'
053700970526     C                   EXSR      F12GES
053800970526     C                   END
053900970526     C     FUNKEY        IFEQ      '24'
054000970526     C                   EXSR      F24GES
054100970526     C                   END
054200970526     C                   END
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
054700970526     C*------------------------------------------------------------------------*
054800970526     C     FUNVAR        BEGSR
054900970526     C*
055000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
055100970526     C*
055200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
055300970526     C     KEY           IFEQ      ENTER
055400970526     C                   MOVEL     *BLANKS       FUNKEY
055500970526     C                   END
055600970526     C     KEY           IFEQ      F1
055700970526     C                   MOVEL     '01'          FUNKEY
055800970526     C                   END
055900970526     C     KEY           IFEQ      F2
056000970526     C                   MOVEL     '02'          FUNKEY
056100970526     C                   END
056200970526     C     KEY           IFEQ      F3
056300970526     C                   MOVEL     '03'          FUNKEY
056400970526     C                   END
056500970526     C     KEY           IFEQ      F4
056600970526     C                   MOVEL     '04'          FUNKEY
056700970526     C                   END
056800970526     C     KEY           IFEQ      F5
056900970526     C                   MOVEL     '05'          FUNKEY
057000970526     C                   END
057100970526     C     KEY           IFEQ      F6
057200970526     C                   MOVEL     '06'          FUNKEY
057300970526     C                   END
057400970526     C     KEY           IFEQ      F7
057500970526     C                   MOVEL     '07'          FUNKEY
057600970526     C                   END
057700970526     C     KEY           IFEQ      F8
057800970526     C                   MOVEL     '08'          FUNKEY
057900970526     C                   END
058000970526     C     KEY           IFEQ      F9
058100970526     C                   MOVEL     '09'          FUNKEY
058200970526     C                   END
058300970526     C     KEY           IFEQ      F10
058400970526     C                   MOVEL     '10'          FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F11
058700970526     C                   MOVEL     '11'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      F12
059000970526     C                   MOVEL     '12'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      F13
059300970526     C                   MOVEL     '13'          FUNKEY
059400970526     C                   END
059500970526     C     KEY           IFEQ      F14
059600970526     C                   MOVEL     '14'          FUNKEY
059700970526     C                   END
059800970526     C     KEY           IFEQ      F15
059900970526     C                   MOVEL     '15'          FUNKEY
060000970526     C                   END
060100970526     C     KEY           IFEQ      F16
060200970526     C                   MOVEL     '16'          FUNKEY
060300970526     C                   END
060400970526     C     KEY           IFEQ      F17
060500970526     C                   MOVEL     '17'          FUNKEY
060600970526     C                   END
060700970526     C     KEY           IFEQ      F18
060800970526     C                   MOVEL     '18'          FUNKEY
060900970526     C                   END
061000970526     C     KEY           IFEQ      F19
061100970526     C                   MOVEL     '19'          FUNKEY
061200970526     C                   END
061300970526     C     KEY           IFEQ      F20
061400970526     C                   MOVEL     '20'          FUNKEY
061500970526     C                   END
061600970526     C     KEY           IFEQ      F21
061700970526     C                   MOVEL     '21'          FUNKEY
061800970526     C                   END
061900970526     C     KEY           IFEQ      F22
062000970526     C                   MOVEL     '22'          FUNKEY
062100970526     C                   END
062200970526     C     KEY           IFEQ      F23
062300970526     C                   MOVEL     '23'          FUNKEY
062400970526     C                   END
062500970526     C     KEY           IFEQ      F24
062600970526     C                   MOVEL     '24'          FUNKEY
062700970526     C                   END
062800970526     C     KEY           IFEQ      ROLLUP
062900970526     C                   MOVEL     'RU'          FUNKEY
063000970526     C                   END
063100970526     C     KEY           IFEQ      HELP
063200970526     C                   MOVEL     'HP'          FUNKEY
063300970526     C                   END
063400970526     C                   ENDSR
063500970526     C*------------------------------------------------------------------------*
063600970526     C* NOTE COSTRUZIONE SCHIERE
063700970526     C*------------------------------------------------------------------------*
063800970526     C*
063900970526     C* -MSG-MESSAGGI
064000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
064100970526     C* (NON TOCCARE MSG,1 E MSG,2)
064200970526     C*
064300970526     C* -CMD-COMANDI
064400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
064500970526     C*
064600970526     C* -OPD-OPZIONI
064700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
064800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
064900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
065000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
065100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
065200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
065300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
065400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
065500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
065600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
065700970526     C*
065800970526     C* -FUD-FUNZIONI
065900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
066000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
066500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
066700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
066800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
066900970526** MSG - MESSAGGI
067000120927Tasto funzionale non gestito                                                   1
067100160217                                                                               2
067200160217                                                                               3
067300160217                                                                               4
067400160217                                                                               5
067500160217                                                                               6
067600160217                                                                               7
067700130108                                                                               8
067800120927                                                                               9
067900120927                                                                               10
068000120927                                                                               11
068100120927                                                                               12
068200120927                                                                               13
068300120927Data errata                                                                    14
068400140116                                                                               15
068500120927                                                                               16
068600120927                                                                               17
068700120927                                                                               18
068800120927                                                                               19
068900160217Esecuzione  conclusa - controllare l'esito                                     20
069000160217                                                                               21
069100160217                                                                               22
069200121108                                                                               23
069300121108                                                                               24
069400970526** FUD - FUNZIONI
069500970526                   -        (S/N)=N                        funz.01¬
069600970526                   -        (S/N)=N                        funz.02¬
069700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
069800970526                   -        (S/N)=N                        funz.04¬
069900970526                   -        (S/N)=N                        funz.05¬
070000991214F06=Conferma       -        (S/N)=S                        funz.06¬
070100160217                   -        (S/N)=S                        funz.07¬
070200970527                   -        (S/N)=N                        funz.08¬
070300970527                   -        (S/N)=N                        funz.09¬
070400970527                   -        (S/N)=N                        funz.10¬
070500970526                   -        (S/N)=N                        funz.11¬
070600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
070700970526                   -        (S/N)=N                        funz.13¬
070800970526                   -        (S/N)=N                        funz.14¬
070900970526                   -        (S/N)=N                        funz.15¬
071000970526                   -        (S/N)=N                        funz.16¬
071100970526                   -        (S/N)=N                        funz.17¬
071200970526                   -        (S/N)=N                        funz.18¬
071300970526                   -        (S/N)=N                        funz.19¬
071400970526                   -        (S/N)=N                        funz.20¬
071500970526                   -        (S/N)=N                        funz.21¬
071600970526                   -        (S/N)=N                        funz.22¬
071700970526                   -        (S/N)=N                        funz.23¬
071800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
