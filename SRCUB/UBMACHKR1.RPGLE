000100970526     H*------------------------------------------------------------------------*
000200161104     H* Lancio Agg. file TIMAC00F
000300970526     H*------------------------------------------------------------------------*
000400161104     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000500161104     H BNDDIR('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900161104     FUBMACHKD1 cf   e             WORKSTN INFDS(DEVDS1)
001000161104     F                                     SFILE(MACHKDMS:MSGULT)
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
006900141027     D WLBDAT          DS
007000141027     D  G02DAT                 1      8  0
007100141027     D  G02INV                 9     16  0
007200141027     D  G02ERR                17     17
007300141027     D  G02TGI                18     22  0
007400141027     D wDataDa         s              8s 0
007500141027     D wDataA          s              8s 0
007600141028     D*-------------------
007700161104     D* parametri UBMACHKR
007800141028     D*-------------------
007900161110     D*/copy gaitrasrc/srcProtoPR,UBMACHK1R
008000161110     D*/copy gaitrasrc/srcProtoPR,UBMASTO1R
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
009100161107       // Periodo DA
009200161107     d MACHKTSDa       s               z
009300161107       // Periodo AL
009400161107     d MACHKTSAl       s               z
009500161107       // Esito
009600161107     d MACHKEsito      s              1a
009700161104     D*------------
009800161104     D* campi tipo data per le date a video
009900161104     D DtDaISO         s               d
010000161104     D DtAlISO         s               d
010100120928
010200970526     C*------------------------------------------------------------------------*
010300000303     C* MAIN LINES
010400970526     C*------------------------------------------------------------------------*
010500970526     C*
010600120927     C* metto i default su D03
010700120927     C                   EXSR      InzD03
010800120927     C*
010900120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011000991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011300970526     C*
011400970526     C* EMETTE FORMATO VIDEO
011500970526     C                   EXSR      BUTFMR
011600970526     C*
011700970526     C* GESTIONE TASTI FUNZIONALI
011800970526     C                   EXSR      FUNCON
011900970526E   2C                   ENDDO
012000970526     C*
012100970526     C* FINE PGM
012200970526     C                   SETON                                        LR
012300970526     C*------------------------------------------------------------------------*
012400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012500970526     C*------------------------------------------------------------------------*
012600970526     C     BUTFMR        BEGSR
012700970526     C*
012800970526     C* EMETTE LA TESTATA
012900161104     C                   WRITE     MACHKD01
013000970526     C*
013100970526     C* SPEDISCE EVENTUALE MESSAGGIO
013200970526     C                   EXSR      SNDMSG
013300970526     C*
013400970526     C* EMETTE LE FUNZIONI
013500161104     C                   WRITE     MACHKD02
013600970526     C*
013700970526     C* EMETTE IL VIDEO
013800161104     C                   EXFMT     MACHKD03
013900970526     C*
014000991214     C* REIMPOSTA FLAG DI ERRORE
014100991214     C                   MOVEL     '0'           ERRL00
014200970526     C*
014300970526     C* REIMPOSTA INDICATORI DI ERRORE
014400020611     C                   SETOFF                                       555556
014500970526     C*
014600970526     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800120927     C* InzD03 - Inizializzo D03
014900970526     C*------------------------------------------------------------------------*
015000120927     C     InzD03        BEGSR
015100120927     C*
015200161104     C                   clear                   D03DtDa
015300161104     C                   clear                   D03DtDa
015400161111     C                   clear                   D03Dlt
015500161117     C                   clear                   D03Tst
015600130108     C*
015700120927     C                   ENDSR
015800120927     C*------------------------------------------------------------------------*
015900120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016000120927     C*------------------------------------------------------------------------*
016100120927     C     SNDMSG        BEGSR
016200120927     C*
016300970526     C* PULISCE IL SFILE MESSAGGI
016400970526     C                   MOVEL     '1'           *IN40
016500161104     C                   WRITE     MACHKDMC
016600970526     C                   MOVEL     '0'           *IN40
016700970526     C*
016800970526     C* VISUALIZZA IL MESSAGGIO
016900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017000970526     C                   CALL      'XMSG'        XMSGPA
017100020611     C     DSMSER        IFEQ      *ZEROS
017200970526     C                   MOVEL     DSMSKE        MSGKEY
017300020611     C                   ENDIF
017400970526     C                   Z-ADD     1             MSGULT
017500161104     C                   WRITE     MACHKDMS
017600161104     C                   WRITE     MACHKDMC
017700970526     C*
017800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017900970526     C                   MOVEL     *BLANKS       DSMSMS
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* ENTGES - GESTIONE ENTER -> INVIO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     ENTGES        BEGSR
018600970526     C*
018700970526     C* CONTROLLA I CAMPI IMMESSI
018800970526     C                   EXSR      DECODI
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200970526     C* F03GES - GESTIONE F03 -> FINE
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F03GES        BEGSR
019500970526     C*
019600970526     C* IMPOSTA "PREMUTO F03"
019700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100991214     C* F06GES - GESTIONE F06 -> CONFERMA
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F06GES        BEGSR
020400970526     C*
020500991214     C* CONTROLLA I CAMPI IMMESSI
020600991214     C                   EXSR      DECODI
020700020611     C*
020800991214     C* ESEGUE ROUTINE DI CONFERMA
020900020611     C     ERRL00        IFEQ      '0'
021000991214     C                   EXSR      ESEGUI
021100130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
021200121109     C                   MOVEL     MSG(20)       DSMSMS
021300121109     C* non esco mai dal pgm
021400121109     C***                MOVEL     '1'           FINL00
021500121108     C                   ENDIF
021600970526     C*
021700991214     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900130109     C* F07GES - GESTIONE F07 -> STAMPA
022000970526     C*------------------------------------------------------------------------*
022100130109     C     F07GES        BEGSR
022200970526     C*
022300140818     C                   EXSR      F06GES
022400130110     C*
022500970526     C                   ENDSR
022600130109     C*------------------------------------------------------------------------*
022700130109     C* F12GES - GESTIONE F12 -> RITORNO
022800130109     C*------------------------------------------------------------------------*
022900130109     C     F12GES        BEGSR
023000130109     C*
023100130109     C* IMPOSTA "PREMUTO F12"
023200130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023300130109     C*
023400130109     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023700970526     C*------------------------------------------------------------------------*
023800970526     C     F24GES        BEGSR
023900970526     C*
024000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
024500970526     C*------------------------------------------------------------------------*
024600991214     C     ESEGUI        BEGSR
024700161111     C*
024800161111     C                   CLEAR                   kpjbu
024900141027     C*
025000161108     C* passo il periodo dalla data DA che parte dalle 00, alla data AL che finisce alle 23.59.59
025100161108     C                   IF        DtDaISO <> *loval
025200161110     C***                EVAL      MACHKTSDa = %timestamp(DtDaISO)
025300161110     C                   EVAL      %subst(kpjbu : 1 :10) =
025400161110     C                             %char(DtDaISO)
025500161108     C                   ENDIF
025600161108     C                   IF        DtAlISO <> *loval
025700161110     C***                EVAL      MACHKTSAl = %timestamp(%char(DtAlISO) +
025800161110     C***                                      '-23.59.59.999999')
025900161110     C                   EVAL      %subst(kpjbu : 11 :10) =
026000161110     C                             %char(DtAlISO)
026100161108     C                   ENDIF
026200161111     C* opzione
026300161111     C                   EVAL      %subst(kpjbu : 21 :1) = *blank
026400161111     C                   IF        D03Dlt = 'S'
026500161111     C                   EVAL      %subst(kpjbu : 21 :1) = 'D'
026600161111     C                   ENDIF
026700161117     C                   IF        D03Tst = 'S'
026800161117     C                   EVAL      %subst(kpjbu : 22 :1) = 'S'
026900161117     C                   ENDIF
027000141027     C*
027100161110     C* chiamo per creare TIMAC
027200161110     C                   CALL      'UBMACKS1R'
027300161110     C                   PARM                    kpjba
027400161110     C*
027500161110        //               UBMACHK1R_S( *blank :
027600161110        //                            MACHKTSDa :
027700161110        //                            MACHKTSAl :
027800161110        //                            MACHKEsito );
027900161110        //               UBMASTO1R_S( *blank :
028000161110        //                            MACHKTSDa :
028100161110        //                            MACHKTSAl :
028200161110        //                            MACHKEsito );
028300140120     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028700970526     C*------------------------------------------------------------------------*
028800970526     C     DECODI        BEGSR
028900121019     C*
029000120927     C                   SETOFF                                       505152
029100140120     c                   EVAL      VIDMSG    = *blank
029200120927     C*
029300161104     C* Data DA
029400161104     C*
029500141216     C     ERRL00        IFEQ      '0'
029600161104     C* se la data è valorizzata, controllo sia corretta
029700161104IF  2C                   IF        D03DtDa > 0
029800161104     C                   MONITOR
029900161104     C* se il valore della data è > 6 cifre
030000161104     C                   IF        %len(%trim(%char(D03DtDa))) > 6
030100161104     C* presumo sia scritta in ggmmaaaa
030200161104IF  5C                   EVAL      DtDaISO = %date(D03DtDa:*eur)
030300161104     C* se il valore della data è <= 6 cifre
030400161104     C                   ELSE
030500161104     C* presumo sia scritta in ggmmaa
030600161104IF  5C                   EVAL      DtDaISO = %date(D03DtDa:*dmy)
030700161104     C                   ENDIF
030800161104IF  2C                   ON-ERROR
030900161104     C                   MOVEL     '1'           ERRL00
031000161104     C                   SETON                                        51
031100161104     C                   MOVEL     MSG(2)        DSMSMS
031200161104     C                   LEAVESR
031300161104     C                   ENDMON
031400161104     C* rivalorizzo il video con la data in ggmmaaaa
031500161104     C                   EVAL      D03DtDa = %dec(DtDaISO:*eur)
031600161104E   2C                   ENDIF
031700141216     C                   ENDIF
031800161104     C*
031900161104     C* Data AL
032000161104     C*
032100161104     C     ERRL00        IFEQ      '0'
032200161104     C* se la data è valorizzata, controllo sia corretta
032300161104IF  2C                   IF        D03DtAl > 0
032400161104     C                   MONITOR
032500161104     C* se il valore della data è > 6 cifre
032600161104     C                   IF        %len(%trim(%char(D03DtAl))) > 6
032700161104     C* presumo sia scritta in ggmmaaaa
032800161104IF  5C                   EVAL      DtAlISO = %date(D03DtAl:*eur)
032900161104     C* se il valore della data è <= 6 cifre
033000161104     C                   ELSE
033100161104     C* presumo sia scritta in ggmmaa
033200161104IF  5C                   EVAL      DtAlISO = %date(D03DtAl:*dmy)
033300161104     C                   ENDIF
033400161104IF  2C                   ON-ERROR
033500161104     C                   MOVEL     '1'           ERRL00
033600161104     C                   SETON                                        52
033700161104     C                   MOVEL     MSG(2)        DSMSMS
033800161104     C                   LEAVESR
033900161104     C                   ENDMON
034000161104     C* rivalorizzo il video con la data in ggmmaaaa
034100161104     C                   EVAL      D03DtAl = %dec(DtAlISO:*eur)
034200161104E   2C                   ENDIF
034300161104     C                   ENDIF
034400141216     C*
034500970528     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     AZZVID        BEGSR
035000141027     C*
035100161104     C                   clear                   D03DtDa
035200161104     C                   clear                   D03DtAl
035300161111     C                   clear                   D03Dlt
035400161117     C                   clear                   D03Tst
035500970526     C*
035600970526     C                   ENDSR
035700970526     C*------------------------------------------------------------------------*
035800020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
035900970526     C*------------------------------------------------------------------------*
036000970526     C     IMPFUN        BEGSR
036100970526     C*
036200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
036300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
036400970526     C                   EXSR      FUNGES
036500970526     C*
036600970526     C                   ENDSR
036700020923     C*--------------------------------------------------------------------------------------------*
036800020923     C* REPERISCE I DATI UTENTE
036900020923     C*--------------------------------------------------------------------------------------------*
037000020923     C     REPDATIUTE    BEGSR
037100020923     C*
037200020923     C* INIZIALIZZA VARIABILI DI WRK
037300020923     C                   CLEAR                   TIBS34DS
037400020923     C                   CLEAR                   AZUTEDS
037500020923     C                   CLEAR                   DDATIUTE
037600020923     C*
037700020923     C     *DTAARA       DEFINE    §azute        azuteds
037800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
037900020923     C                   IN(E)     *DTAARA
038000020923if  1C                   IF        %Error
038100020923     c                   EVAL      I34Tla = 'L'
038200020923     C                   CALL      'TIBS34R'
038300020923     C                   PARM                    Tibs34Ds
038400020923     C                   IN        *DTAARA
038500020923e   1C                   ENDIF
038600020923     C*
038700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
038800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
038900020923     C*
039000020923     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* *INZSR - OPERAZIONI INIZIALI
039300970526     C*------------------------------------------------------------------------*
039400970526     C     *INZSR        BEGSR
039500991217     C*
039600991217     C     *ENTRY        PLIST
039700991217     C                   PARM                    KPJBA
039800020923     C*
039900020923     C* REPERISCE I DATI DELL'UTENTE
040000020923     C                   EXSR      REPDATIUTE
040100970526     C*
040200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
040300970526     C     XMSGPA        PLIST
040400970526     C                   PARM                    DSMSMS
040500970526     C                   PARM                    DSMSPG
040600970526     C                   PARM                    DSMSKE
040700970526     C                   PARM                    DSMSER
040800970526     C*
040900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
041000970526     C                   CALL      'XKEY'
041100970526     C                   PARM                    DSKEY
041200970526     C*---
041300970526     C* VARIABILI NON RIFERITE AL DATA BASE
041400970526     C*---
041500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
041600991214     C                   MOVEL     *BLANKS       DEPOP1            3
041700970526     C*---
041800970526     C* VARIABILI DI CONTROLLO
041900970526     C*---
042000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
042100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
042200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
042300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
042400970526     C*
042500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
042600161104     C                   MOVEL     'UBMACHKR1'   DSMSPG
042700161104     C                   MOVEL     'UBMACHKR1'   MSGQUE
042800970526     C                   MOVEL     *BLANKS       MSGKEY
042900970526     C                   MOVEL     *BLANKS       DSMSMS
043000970528     C*
043100970528     C* CALCOLA LA DATA CORRENTE
043200970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
043300970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
043400970528     C                   Z-ADD     WN8           G08DAT
043500970528     C                   Z-ADD     *ZEROS        G08INV
043600970528     C                   MOVEL     '0'           G08ERR
043700970528     C                   CALL      'XSRDA8'
043800970528     C                   PARM                    WLBDA8
043900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
044000000310     C*
044100000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
044200000310     C                   EXSR      AZZVID
044300970526     C*
044400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
044500970526     C                   EXSR      IMPFUN
044600970526     C*
044700970526     C                   ENDSR
044800970526     C*------------------------------------------------------------------------*
044900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
045000970526     C*------------------------------------------------------------------------*
045100970526     C     FUNGES        BEGSR
045200970526     C*
045300970526     C* PULISCO CAMPI DI LAVORO
045400970526     C                   Z-ADD     0             W                 5 0
045500970526     C*
045600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
045700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
045800970526     C     1             DO        24            I
045900970526     C                   MOVEL     FUD(I)        FUDRIG
046000970526     C*
046100970526     C* PULIZIA FUNZIONI PRECEDENTI
046200970526     C                   MOVEL     *BLANKS       FUV(I)
046300970526     C*
046400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
046500970526     C* (SALTA F24)
046600970526     C     FUDATT        IFEQ      'S'
046700970526     C     FUDFUN        ANDNE     '24'
046800970526     C                   ADD       1             W
046900970526     C                   MOVEL     FUDDES        FUV(W)
047000970526     C                   END
047100970526     C                   END                                                    DO WHILE
047200970526     C*
047300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
047400970526     C* ALTRIMENTI DISATTIVO F24
047500970526     C     1             DO        24            I
047600970526     C                   MOVEL     FUD(I)        FUDRIG
047700970526     C*
047800970526     C     FUDFUN        IFEQ      '24'
047900970526     C     W             IFGT      6
048000970526     C                   MOVEL     'S'           FUDATT
048100970526     C                   ELSE
048200970526     C                   MOVEL     *BLANKS       FUDATT
048300970526     C                   END                                                    W GT 8
048400970526     C                   MOVEL     FUDRIG        FUD(I)
048500970526     C                   END
048600970526     C                   END                                                    DO WHILE
048700970526     C*
048800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
048900970526     C     W             DIV       6             I
049000970526     C                   MVR                     Z
049100970526     C*
049200970526     C     Z             IFNE      0
049300970526     C     I             ADD       1             Z
049400970526     C                   ELSE
049500970526     C                   Z-ADD     I             Z                 5 0
049600970526     C                   END
049700970526     C*
049800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
049900970526     C                   ADD       1             FUNGRU
050000970526     C     FUNGRU        IFGT      Z
050100970526     C     FUNGRU        ORLT      1
050200970526     C                   Z-ADD     1             FUNGRU
050300970526     C                   END
050400970526     C*
050500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
050600970526     C     FUNGRU        IFEQ      1
050700970526     C                   MOVEL     FUV(1)        FUN001
050800970526     C                   MOVEL     FUV(2)        FUN002
050900970526     C                   MOVEL     FUV(3)        FUN003
051000970526     C                   MOVEL     FUV(4)        FUN004
051100970526     C                   MOVEL     FUV(5)        FUN005
051200970526     C                   MOVEL     FUV(6)        FUN006
051300970526     C                   END
051400970526     C*
051500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
051600970526     C     FUNGRU        IFEQ      2
051700970526     C                   MOVEL     FUV(7)        FUN001
051800970526     C                   MOVEL     FUV(8)        FUN002
051900970526     C                   MOVEL     FUV(9)        FUN003
052000970526     C                   MOVEL     FUV(10)       FUN004
052100970526     C                   MOVEL     FUV(11)       FUN005
052200970526     C                   MOVEL     FUV(12)       FUN006
052300970526     C                   END
052400970526     C*
052500970526     C* TERZO GRUPPO DI 6 FUNZIONI
052600970526     C     FUNGRU        IFEQ      3
052700970526     C                   MOVEL     FUV(13)       FUN001
052800970526     C                   MOVEL     FUV(14)       FUN002
052900970526     C                   MOVEL     FUV(15)       FUN003
053000970526     C                   MOVEL     FUV(16)       FUN004
053100970526     C                   MOVEL     FUV(17)       FUN005
053200970526     C                   MOVEL     FUV(18)       FUN006
053300970526     C                   END
053400970526     C*
053500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
053600970526     C     FUNGRU        IFEQ      4
053700970526     C                   MOVEL     FUV(19)       FUN001
053800970526     C                   MOVEL     FUV(20)       FUN002
053900970526     C                   MOVEL     FUV(21)       FUN003
054000970526     C                   MOVEL     FUV(22)       FUN004
054100970526     C                   MOVEL     *BLANKS       FUN005
054200970526     C                   MOVEL     *BLANKS       FUN006
054300970526     C                   END
054400970526     C*
054500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
054600970526     C                   MOVEL     *BLANKS       FUN007
054700970526     C                   MOVEL     *BLANKS       FUN008
054800970526     C*
054900970526     C     1             DO        24            I
055000970526     C                   MOVEL     FUD(I)        FUDRIG
055100970526     C     FUDFUN        IFEQ      '24'
055200970526     C     FUDATT        ANDEQ     'S'
055300970526     C                   MOVEL     FUDDES        FUN008
055400970526     C                   END
055500970526     C                   END                                                    DO
055600970526     C*
055700970526     C                   ENDSR
055800970526     C*------------------------------------------------------------------------*
055900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
056000970526     C*------------------------------------------------------------------------*
056100970526     C     FUNCON        BEGSR
056200970526     C*
056300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
056400970526     C                   EXSR      FUNVAR
056500970526     C*
056600970526     C* PULISCO CAMPI DI LAVORO
056700970526     C                   Z-ADD     0             I                 5 0
056800970526     C                   Z-ADD     0             W                 5 0
056900970526     C*
057000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
057100970526     C     FUNKEY        IFNE      *BLANKS
057200970526     C     1             DO        24            I
057300970526     C                   MOVEL     FUD(I)        FUDRIG
057400970526     C*
057500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
057600970526     C     FUNKEY        IFEQ      FUDFUN
057700970526     C     FUDATT        ANDEQ     'S'
057800970526     C                   Z-ADD     I             W
057900970526     C                   END
058000970526     C                   END                                                    DO WHILE
058100970526     C*
058200970526     C* FUNZIONE NON ATTIVA (ERRORE)
058300970526     C     W             IFEQ      0
058400970526     C*
058500970526     C* EMISSIONE MESSAGGIO DI ERRORE
058600970526     C                   MOVEL     MSG(1)        DSMSMS
058700970526     C*
058800970526     C                   END                                                    W=0
058900970526     C                   END                                                    FUNKEY NE *BLANKS
059000970526     C*
059100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
059200970526     C     W             IFNE      0
059300970526     C     FUNKEY        OREQ      *BLANKS
059400970526     C     FUNKEY        IFEQ      *BLANKS
059500970526     C                   EXSR      ENTGES
059600970526     C                   END
059700970526     C     FUNKEY        IFEQ      '03'
059800970526     C                   EXSR      F03GES
059900970526     C                   END
060000970526     C     FUNKEY        IFEQ      '06'
060100970526     C                   EXSR      F06GES
060200970526     C                   END
060300130109     C     FUNKEY        IFEQ      '07'
060400130109     C                   EXSR      F07GES
060500130109     C                   END
060600970526     C     FUNKEY        IFEQ      '12'
060700970526     C                   EXSR      F12GES
060800970526     C                   END
060900970526     C     FUNKEY        IFEQ      '24'
061000970526     C                   EXSR      F24GES
061100970526     C                   END
061200970526     C                   END
061300970526     C*
061400970526     C                   ENDSR
061500970526     C*------------------------------------------------------------------------*
061600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
061700970526     C*------------------------------------------------------------------------*
061800970526     C     FUNVAR        BEGSR
061900970526     C*
062000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
062100970526     C*
062200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
062300970526     C     KEY           IFEQ      ENTER
062400970526     C                   MOVEL     *BLANKS       FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F1
062700970526     C                   MOVEL     '01'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F2
063000970526     C                   MOVEL     '02'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F3
063300970526     C                   MOVEL     '03'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F4
063600970526     C                   MOVEL     '04'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F5
063900970526     C                   MOVEL     '05'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F6
064200970526     C                   MOVEL     '06'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F7
064500970526     C                   MOVEL     '07'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F8
064800970526     C                   MOVEL     '08'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F9
065100970526     C                   MOVEL     '09'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F10
065400970526     C                   MOVEL     '10'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F11
065700970526     C                   MOVEL     '11'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F12
066000970526     C                   MOVEL     '12'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F13
066300970526     C                   MOVEL     '13'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F14
066600970526     C                   MOVEL     '14'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F15
066900970526     C                   MOVEL     '15'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F16
067200970526     C                   MOVEL     '16'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F17
067500970526     C                   MOVEL     '17'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F18
067800970526     C                   MOVEL     '18'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F19
068100970526     C                   MOVEL     '19'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F20
068400970526     C                   MOVEL     '20'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      F21
068700970526     C                   MOVEL     '21'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      F22
069000970526     C                   MOVEL     '22'          FUNKEY
069100970526     C                   END
069200970526     C     KEY           IFEQ      F23
069300970526     C                   MOVEL     '23'          FUNKEY
069400970526     C                   END
069500970526     C     KEY           IFEQ      F24
069600970526     C                   MOVEL     '24'          FUNKEY
069700970526     C                   END
069800970526     C     KEY           IFEQ      ROLLUP
069900970526     C                   MOVEL     'RU'          FUNKEY
070000970526     C                   END
070100970526     C     KEY           IFEQ      HELP
070200970526     C                   MOVEL     'HP'          FUNKEY
070300970526     C                   END
070400970526     C                   ENDSR
070500970526     C*------------------------------------------------------------------------*
070600970526     C* NOTE COSTRUZIONE SCHIERE
070700970526     C*------------------------------------------------------------------------*
070800970526     C*
070900970526     C* -MSG-MESSAGGI
071000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
071100970526     C* (NON TOCCARE MSG,1 E MSG,2)
071200970526     C*
071300970526     C* -CMD-COMANDI
071400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
071500970526     C*
071600970526     C* -OPD-OPZIONI
071700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
071800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
071900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
072000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
072100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
072200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
072300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
072400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
072500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
072600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
072700970526     C*
072800970526     C* -FUD-FUNZIONI
072900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
073000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
073100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
073200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
073300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
073400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
073500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
073600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
073700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
073800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
073900970526** MSG - MESSAGGI
074000120927Tasto funzionale non gestito                                                   1
074100161104Data errata                                                                    2
074200141216                                                                               3
074300141216                                                                               4
074400130108                                                                               5
074500130108Campo obbligatorio                                                             6
074600130108                                                                               7
074700130108                                                                               8
074800120927                                                                               9
074900120927                                                                               10
075000120927                                                                               11
075100120927                                                                               12
075200120927                                                                               13
075300141027                                                                               14
075400140116                                                                               15
075500120927                                                                               16
075600120927                                                                               17
075700120927                                                                               18
075800120927                                                                               19
075900141027Elaborazione conclusa                                                          20
076000141027                                                                               21
076100141027                                                                               22
076200121108                                                                               23
076300121108                                                                               24
076400970526** FUD - FUNZIONI
076500970526                   -        (S/N)=N                        funz.01¬
076600970526                   -        (S/N)=N                        funz.02¬
076700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
076800970526                   -        (S/N)=N                        funz.04¬
076900970526                   -        (S/N)=N                        funz.05¬
077000991214F06=Conferma       -        (S/N)=S                        funz.06¬
077100140819                   -        (S/N)=N                        funz.07¬
077200970527                   -        (S/N)=N                        funz.08¬
077300970527                   -        (S/N)=N                        funz.09¬
077400970527                   -        (S/N)=N                        funz.10¬
077500970526                   -        (S/N)=N                        funz.11¬
077600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
077700970526                   -        (S/N)=N                        funz.13¬
077800970526                   -        (S/N)=N                        funz.14¬
077900970526                   -        (S/N)=N                        funz.15¬
078000970526                   -        (S/N)=N                        funz.16¬
078100970526                   -        (S/N)=N                        funz.17¬
078200970526                   -        (S/N)=N                        funz.18¬
078300970526                   -        (S/N)=N                        funz.19¬
078400970526                   -        (S/N)=N                        funz.20¬
078500970526                   -        (S/N)=N                        funz.21¬
078600970526                   -        (S/N)=N                        funz.22¬
078700970526                   -        (S/N)=N                        funz.23¬
078800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
