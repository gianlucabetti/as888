000100970526     H*------------------------------------------------------------------------*
000200170629     H* Lancio Confronto calcolo instradamento DPD TISIE3R/TISID4R
000300970526     H*------------------------------------------------------------------------*
000400161104     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000500161104     H BNDDIR('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900170629     FTNVRVAS01Dcf   e             WORKSTN INFDS(DEVDS1)
001000170629     F                                     SFILE(AS01DMS:MSGULT)
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
002100170202     D*------------
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
006300170306     D*-------------------
006400170306
006500170306       // Ricerca/Controllo tabelle (TNTBE)?
006600170306     d TIBS02ds      e ds                  inz
006700170306     d   T02mod      e                     inz('C')
006800170306     d   T02cod      e                     inz('VPR')
006900170323     d   T02ke1      e                     inz
007000170306     d   T02ke2      e                     inz
007100170306
007200020923     D TIBS34DS      E DS                                                       *Profili utente
007300020923     D DDATIUTE      E DS                                                       *Dati utente
007400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007500141027     D WLBDAT          DS
007600141027     D  G02DAT                 1      8  0
007700141027     D  G02INV                 9     16  0
007800141027     D  G02ERR                17     17
007900141027     D  G02TGI                18     22  0
008000141027     D wDataDa         s              8s 0
008100141027     D wDataA          s              8s 0
008200120928
008300121003     d Status         sds
008400121003     d  SDSpgm           *proc
008500121003     d  SDSprm           *parms
008600121003     d  SDSuser              254    263
008700170202
008800130116     d ora             s               t
008900140117     d wX              s              2s 0
009000170629     d* per ora è sempre interattivo
009100170629     d $Interatt       s               n   inz(*on)
009200161104     D*------------
009300170629     D* parametri 02R
009400170629     D DtSpeD          s              8
009500170629     D DtSpeA          s              8
009600170629     D LNA             s              3
009700170629     D SRVC            s              3
009800170629     D ERR             s              1      inz
009900170629     D* campi tipo data per le date a video
010000170629     D DtSpeDISO       s               d
010100170629     D DtSpeAISO       s               d
010200170306
010300170306      /copy gaitrasrc/srcProtoPR,TIBS02R
010400120928
010500970526     C*------------------------------------------------------------------------*
010600000303     C* MAIN LINES
010700970526     C*------------------------------------------------------------------------*
010800970526     C*
010900120927     C* metto i default su D03
011000120927     C                   EXSR      InzD03
011100120927     C*
011200120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011300991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011600970526     C*
011700970526     C* EMETTE FORMATO VIDEO
011800970526     C                   EXSR      BUTFMR
011900970526     C*
012000970526     C* GESTIONE TASTI FUNZIONALI
012100970526     C                   EXSR      FUNCON
012200970526E   2C                   ENDDO
012300970526     C*
012400970526     C* FINE PGM
012500970526     C                   SETON                                        LR
012600970526     C*------------------------------------------------------------------------*
012700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012800970526     C*------------------------------------------------------------------------*
012900970526     C     BUTFMR        BEGSR
013000970526     C*
013100970526     C* EMETTE LA TESTATA
013200170629     C                   WRITE     AS01D01
013300970526     C*
013400970526     C* SPEDISCE EVENTUALE MESSAGGIO
013500970526     C                   EXSR      SNDMSG
013600970526     C*
013700970526     C* EMETTE LE FUNZIONI
013800170629     C                   WRITE     AS01D02
013900970526     C*
014000970526     C* EMETTE IL VIDEO
014100170629     C                   EXFMT     AS01D03
014200970526     C*
014300991214     C* REIMPOSTA FLAG DI ERRORE
014400991214     C                   MOVEL     '0'           ERRL00
014500970526     C*
014600970526     C* REIMPOSTA INDICATORI DI ERRORE
014700020611     C                   SETOFF                                       555556
014800970526     C*
014900970526     C                   ENDSR
015000970526     C*------------------------------------------------------------------------*
015100120927     C* InzD03 - Inizializzo D03
015200970526     C*------------------------------------------------------------------------*
015300120927     C     InzD03        BEGSR
015400120927     C*
015500170629     C                   clear                   D03DtSpeD
015600170629     C                   clear                   D03DtSpeA
015700170629     C                   clear                   D03LNA
015800170629     C                   clear                   D03SRVC
015900130108     C*
016000120927     C                   ENDSR
016100120927     C*------------------------------------------------------------------------*
016200120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016300120927     C*------------------------------------------------------------------------*
016400120927     C     SNDMSG        BEGSR
016500120927     C*
016600970526     C* PULISCE IL SFILE MESSAGGI
016700970526     C                   MOVEL     '1'           *IN40
016800170629     C                   WRITE     AS01DMC
016900970526     C                   MOVEL     '0'           *IN40
017000970526     C*
017100970526     C* VISUALIZZA IL MESSAGGIO
017200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017300970526     C                   CALL      'XMSG'        XMSGPA
017400020611     C     DSMSER        IFEQ      *ZEROS
017500970526     C                   MOVEL     DSMSKE        MSGKEY
017600020611     C                   ENDIF
017700970526     C                   Z-ADD     1             MSGULT
017800170629     C                   WRITE     AS01DMS
017900170629     C                   WRITE     AS01DMC
018000970526     C*
018100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018200970526     C                   MOVEL     *BLANKS       DSMSMS
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* ENTGES - GESTIONE ENTER -> INVIO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     ENTGES        BEGSR
018900970526     C*
019000970526     C* CONTROLLA I CAMPI IMMESSI
019100970526     C                   EXSR      DECODI
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500970526     C* F03GES - GESTIONE F03 -> FINE
019600970526     C*------------------------------------------------------------------------*
019700970526     C     F03GES        BEGSR
019800970526     C*
019900970526     C* IMPOSTA "PREMUTO F03"
020000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020100970526     C*
020200970526     C                   ENDSR
020300970526     C*------------------------------------------------------------------------*
020400991214     C* F06GES - GESTIONE F06 -> CONFERMA
020500970526     C*------------------------------------------------------------------------*
020600970526     C     F06GES        BEGSR
020700970526     C*
020800991214     C* CONTROLLA I CAMPI IMMESSI
020900991214     C                   EXSR      DECODI
021000020611     C*
021100991214     C* ESEGUE ROUTINE DI CONFERMA
021200020611     C     ERRL00        IFEQ      '0'
021300991214     C                   EXSR      ESEGUI
021400170306     C* messaggio di messa in coda del lavoro
021500121109     C                   MOVEL     MSG(20)       DSMSMS
021600121109     C* non esco mai dal pgm
021700121109     C***                MOVEL     '1'           FINL00
021800121108     C                   ENDIF
021900970526     C*
022000991214     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200130109     C* F07GES - GESTIONE F07 -> STAMPA
022300970526     C*------------------------------------------------------------------------*
022400130109     C     F07GES        BEGSR
022500970526     C*
022600140818     C                   EXSR      F06GES
022700130110     C*
022800970526     C                   ENDSR
022900130109     C*------------------------------------------------------------------------*
023000130109     C* F12GES - GESTIONE F12 -> RITORNO
023100130109     C*------------------------------------------------------------------------*
023200130109     C     F12GES        BEGSR
023300130109     C*
023400130109     C* IMPOSTA "PREMUTO F12"
023500130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023600130109     C*
023700130109     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024000970526     C*------------------------------------------------------------------------*
024100970526     C     F24GES        BEGSR
024200970526     C*
024300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024400970526     C*
024500970526     C                   ENDSR
024600170202     C*------------------------------------------------------------------------*
024700170202     C* F21GES - GESTIONE F21 -> BCH09
024800170202     C*------------------------------------------------------------------------*
024900170202     C     F21GES        BEGSR
025000170202     C*
025100170202     C                   MOVE      'GOB2'        KCOAZ
025200170202     C                   CALL      'BCH09'
025300170202     C                   PARM                    KPJBA
025400170202     C*
025500170202     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
025800970526     C*------------------------------------------------------------------------*
025900991214     C     ESEGUI        BEGSR
026000161111     C*
026100161111     C                   CLEAR                   kpjbu
026200141027     C*
026300170629     C* preparo parametri
026400170629     C                   EVAL      DtSpeD = %char(%dec(DtSpeDISO:*iso))
026500170629     C                   EVAL      DtSpeA = %char(%dec(DtSpeAISO:*iso))
026600170629     C                   EVAL      LNA    = %editc(D03LNA:'X')
026700170629     C                   EVAL      SRVC   = %editc(D03SRVC:'X')
026800170629     C                   EVAL      ERR    = *blank
026900141027     C*
027000170629     C* chiamo per lanciare in batch
027100170202     C                   MOVE      'GOB2'        KCOAZ
027200170202     C                   IF        $interatt = *off
027300170202     C                   CALL      'BCH10'
027400170202     C                   PARM                    KPJBA
027500170202     c                   else
027600170629     C                   CALL      'TNVRVAS02R'
027700170629     C                   PARM                    DtSpeD
027800170629     C                   PARM                    DtSpeA
027900170629     C                   PARM                    LNA
028000170629     C                   PARM                    SRVC
028100170629     C                   PARM                    ERR
028200170202     C                   ENDIF
028300140120     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028700970526     C*------------------------------------------------------------------------*
028800970526     C     DECODI        BEGSR
028900121019     C*
029000120927     C                   SETOFF                                       505152
029100140120     c                   EVAL      VIDMSG    = *blank
029200141216     C*
029300170202     C* dati obbligatori
029400170629     C*
029500170629     C*  data spedizione DA obbligatoria e valida
029600170629        if D03DtSpeD = 0;
029700170202     C                   MOVEL     '1'           ERRL00
029800170202     C                   SETON                                        50
029900170202     C                   MOVEL     MSG(6)        DSMSMS
030000170202     C                   LEAVESR
030100170629        else;
030200170629     C                   MONITOR
030300170629     C* se il valore della data è > 6 cifre
030400170629     C                   IF        %len(%trim(%char(D03DtSpeD))
030500170629     C                             ) > 6
030600170629     C* presumo sia scritta in ggmmaaaa
030700170629     C                   EVAL      DtSpeDISO = %date(D03DtSpeD:*eur)
030800170629     C* se il valore della data è <= 6 cifre
030900170629     C                   ELSE
031000170629     C* presumo sia scritta in ggmmaa
031100170629     C                   EVAL      DtSpeDISO = %date(D03DtSpeD:*dmy)
031200170629     C                   ENDIF
031300170629     C                   ON-ERROR
031400170629     C                   MOVEL     '1'           ERRL00
031500170629     C                   SETON                                        50
031600170629     C                   MOVEL     MSG(2)        DSMSMS
031700170629     C                   LEAVESR
031800170629     C                   ENDMON
031900170629     C* rivalorizzo il video con la data in ggmmaaaa
032000170629     C                   EVAL      D03DtSpeD = %dec(DtSpeDISO:*eur)
032100170202        endif;
032200170629     C*
032300170629     C*  data spedizione AL obbligatoria e valida
032400170629        if D03DtSpeA = 0;
032500170629     C                   MOVEL     '1'           ERRL00
032600170629     C                   SETON                                        51
032700170629     C                   MOVEL     MSG(6)        DSMSMS
032800170629     C                   LEAVESR
032900170629        else;
033000170629     C                   MONITOR
033100170629     C* se il valore della data è > 6 cifre
033200170629     C                   IF        %len(%trim(%char(D03DtSpeA))
033300170629     C                             ) > 6
033400170629     C* presumo sia scritta in ggmmaaaa
033500170629     C                   EVAL      DtSpeAISO = %date(D03DtSpeA:*eur)
033600170629     C* se il valore della data è <= 6 cifre
033700170629     C                   ELSE
033800170629     C* presumo sia scritta in ggmmaa
033900170629     C                   EVAL      DtSpeAISO = %date(D03DtSpeA:*dmy)
034000170629     C                   ENDIF
034100170629     C                   ON-ERROR
034200170629     C                   MOVEL     '1'           ERRL00
034300170629     C                   SETON                                        51
034400170629     C                   MOVEL     MSG(2)        DSMSMS
034500170629     C                   LEAVESR
034600170629     C                   ENDMON
034700170629     C* rivalorizzo il video con la data in ggmmaaaa
034800170629     C                   EVAL      D03DtSpeA = %dec(DtSpeAISO:*eur)
034900170629        endif;
035000170629     C*
035100170629     C*  servizio DPD obbligatorio (i valori validi sono a video)
035200170629        if D03SRVC = 0;
035300170629     C                   MOVEL     '1'           ERRL00
035400170629     C                   SETON                                        52
035500170629     C                   MOVEL     MSG(6)        DSMSMS
035600170629     C                   LEAVESR
035700170629        endif;
035800170202     C*
035900170629     C*
036000170629     C*  data spedizione DA > AL
036100170629        if DtSpeDISO > DtSpeAISO;
036200170629     C                   MOVEL     '1'           ERRL00
036300170629     C                   SETON                                        5051
036400170629     C                   MOVEL     MSG(3)        DSMSMS
036500170629     C                   LEAVESR
036600170629        endif;
036700170629
036800970528     C                   ENDSR
036900970526     C*------------------------------------------------------------------------*
037000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
037100970526     C*------------------------------------------------------------------------*
037200970526     C     AZZVID        BEGSR
037300141027     C*
037400170629     C                   clear                   D03DtSpeD
037500170629     C                   clear                   D03DtSpeA
037600170629     C                   clear                   D03LNA
037700170629     C                   clear                   D03SRVC
037800970526     C*
037900970526     C                   ENDSR
038000970526     C*------------------------------------------------------------------------*
038100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
038200970526     C*------------------------------------------------------------------------*
038300970526     C     IMPFUN        BEGSR
038400970526     C*
038500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
038600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
038700970526     C                   EXSR      FUNGES
038800970526     C*
038900970526     C                   ENDSR
039000020923     C*--------------------------------------------------------------------------------------------*
039100020923     C* REPERISCE I DATI UTENTE
039200020923     C*--------------------------------------------------------------------------------------------*
039300020923     C     REPDATIUTE    BEGSR
039400020923     C*
039500020923     C* INIZIALIZZA VARIABILI DI WRK
039600020923     C                   CLEAR                   TIBS34DS
039700020923     C                   CLEAR                   AZUTEDS
039800020923     C                   CLEAR                   DDATIUTE
039900020923     C*
040000020923     C     *DTAARA       DEFINE    §azute        azuteds
040100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
040200020923     C                   IN(E)     *DTAARA
040300020923if  1C                   IF        %Error
040400020923     c                   EVAL      I34Tla = 'L'
040500020923     C                   CALL      'TIBS34R'
040600020923     C                   PARM                    Tibs34Ds
040700020923     C                   IN        *DTAARA
040800020923e   1C                   ENDIF
040900020923     C*
041000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
041100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
041200020923     C*
041300020923     C                   ENDSR
041400970526     C*------------------------------------------------------------------------*
041500970526     C* *INZSR - OPERAZIONI INIZIALI
041600970526     C*------------------------------------------------------------------------*
041700970526     C     *INZSR        BEGSR
041800991217     C*
041900991217     C     *ENTRY        PLIST
042000991217     C                   PARM                    KPJBA
042100020923     C*
042200020923     C* REPERISCE I DATI DELL'UTENTE
042300020923     C                   EXSR      REPDATIUTE
042400970526     C*
042500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
042600970526     C     XMSGPA        PLIST
042700970526     C                   PARM                    DSMSMS
042800970526     C                   PARM                    DSMSPG
042900970526     C                   PARM                    DSMSKE
043000970526     C                   PARM                    DSMSER
043100970526     C*
043200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
043300970526     C                   CALL      'XKEY'
043400970526     C                   PARM                    DSKEY
043500970526     C*---
043600970526     C* VARIABILI NON RIFERITE AL DATA BASE
043700970526     C*---
043800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
043900991214     C                   MOVEL     *BLANKS       DEPOP1            3
044000970526     C*---
044100970526     C* VARIABILI DI CONTROLLO
044200970526     C*---
044300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
044400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
044500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
044600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
044700970526     C*
044800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
044900170629     C                   MOVEL     'TNVRVAS01R'  DSMSPG
045000170629     C                   MOVEL     'TNVRVAS01R'  MSGQUE
045100970526     C                   MOVEL     *BLANKS       MSGKEY
045200970526     C                   MOVEL     *BLANKS       DSMSMS
045300970528     C*
045400970528     C* CALCOLA LA DATA CORRENTE
045500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
045600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
045700970528     C                   Z-ADD     WN8           G08DAT
045800970528     C                   Z-ADD     *ZEROS        G08INV
045900970528     C                   MOVEL     '0'           G08ERR
046000970528     C                   CALL      'XSRDA8'
046100970528     C                   PARM                    WLBDA8
046200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
046300000310     C*
046400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
046500000310     C                   EXSR      AZZVID
046600970526     C*
046700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
046800970526     C                   EXSR      IMPFUN
046900970526     C*
047000970526     C                   ENDSR
047100970526     C*------------------------------------------------------------------------*
047200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
047300970526     C*------------------------------------------------------------------------*
047400970526     C     FUNGES        BEGSR
047500970526     C*
047600970526     C* PULISCO CAMPI DI LAVORO
047700970526     C                   Z-ADD     0             W                 5 0
047800970526     C*
047900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
048000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
048100970526     C     1             DO        24            I
048200970526     C                   MOVEL     FUD(I)        FUDRIG
048300970526     C*
048400970526     C* PULIZIA FUNZIONI PRECEDENTI
048500970526     C                   MOVEL     *BLANKS       FUV(I)
048600970526     C*
048700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
048800970526     C* (SALTA F24)
048900970526     C     FUDATT        IFEQ      'S'
049000970526     C     FUDFUN        ANDNE     '24'
049100970526     C                   ADD       1             W
049200970526     C                   MOVEL     FUDDES        FUV(W)
049300970526     C                   END
049400970526     C                   END                                                    DO WHILE
049500970526     C*
049600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
049700970526     C* ALTRIMENTI DISATTIVO F24
049800970526     C     1             DO        24            I
049900970526     C                   MOVEL     FUD(I)        FUDRIG
050000970526     C*
050100970526     C     FUDFUN        IFEQ      '24'
050200970526     C     W             IFGT      6
050300970526     C                   MOVEL     'S'           FUDATT
050400970526     C                   ELSE
050500970526     C                   MOVEL     *BLANKS       FUDATT
050600970526     C                   END                                                    W GT 8
050700970526     C                   MOVEL     FUDRIG        FUD(I)
050800970526     C                   END
050900970526     C                   END                                                    DO WHILE
051000970526     C*
051100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
051200970526     C     W             DIV       6             I
051300970526     C                   MVR                     Z
051400970526     C*
051500970526     C     Z             IFNE      0
051600970526     C     I             ADD       1             Z
051700970526     C                   ELSE
051800970526     C                   Z-ADD     I             Z                 5 0
051900970526     C                   END
052000970526     C*
052100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
052200970526     C                   ADD       1             FUNGRU
052300970526     C     FUNGRU        IFGT      Z
052400970526     C     FUNGRU        ORLT      1
052500970526     C                   Z-ADD     1             FUNGRU
052600970526     C                   END
052700970526     C*
052800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
052900970526     C     FUNGRU        IFEQ      1
053000970526     C                   MOVEL     FUV(1)        FUN001
053100970526     C                   MOVEL     FUV(2)        FUN002
053200970526     C                   MOVEL     FUV(3)        FUN003
053300970526     C                   MOVEL     FUV(4)        FUN004
053400970526     C                   MOVEL     FUV(5)        FUN005
053500970526     C                   MOVEL     FUV(6)        FUN006
053600970526     C                   END
053700970526     C*
053800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
053900970526     C     FUNGRU        IFEQ      2
054000970526     C                   MOVEL     FUV(7)        FUN001
054100970526     C                   MOVEL     FUV(8)        FUN002
054200970526     C                   MOVEL     FUV(9)        FUN003
054300970526     C                   MOVEL     FUV(10)       FUN004
054400970526     C                   MOVEL     FUV(11)       FUN005
054500970526     C                   MOVEL     FUV(12)       FUN006
054600970526     C                   END
054700970526     C*
054800970526     C* TERZO GRUPPO DI 6 FUNZIONI
054900970526     C     FUNGRU        IFEQ      3
055000970526     C                   MOVEL     FUV(13)       FUN001
055100970526     C                   MOVEL     FUV(14)       FUN002
055200970526     C                   MOVEL     FUV(15)       FUN003
055300970526     C                   MOVEL     FUV(16)       FUN004
055400970526     C                   MOVEL     FUV(17)       FUN005
055500970526     C                   MOVEL     FUV(18)       FUN006
055600970526     C                   END
055700970526     C*
055800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
055900970526     C     FUNGRU        IFEQ      4
056000970526     C                   MOVEL     FUV(19)       FUN001
056100970526     C                   MOVEL     FUV(20)       FUN002
056200970526     C                   MOVEL     FUV(21)       FUN003
056300970526     C                   MOVEL     FUV(22)       FUN004
056400970526     C                   MOVEL     *BLANKS       FUN005
056500970526     C                   MOVEL     *BLANKS       FUN006
056600970526     C                   END
056700970526     C*
056800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
056900970526     C                   MOVEL     *BLANKS       FUN007
057000970526     C                   MOVEL     *BLANKS       FUN008
057100970526     C*
057200970526     C     1             DO        24            I
057300970526     C                   MOVEL     FUD(I)        FUDRIG
057400970526     C     FUDFUN        IFEQ      '24'
057500970526     C     FUDATT        ANDEQ     'S'
057600970526     C                   MOVEL     FUDDES        FUN008
057700970526     C                   END
057800970526     C                   END                                                    DO
057900970526     C*
058000970526     C                   ENDSR
058100970526     C*------------------------------------------------------------------------*
058200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
058300970526     C*------------------------------------------------------------------------*
058400970526     C     FUNCON        BEGSR
058500970526     C*
058600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
058700970526     C                   EXSR      FUNVAR
058800970526     C*
058900970526     C* PULISCO CAMPI DI LAVORO
059000970526     C                   Z-ADD     0             I                 5 0
059100970526     C                   Z-ADD     0             W                 5 0
059200970526     C*
059300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
059400970526     C     FUNKEY        IFNE      *BLANKS
059500970526     C     1             DO        24            I
059600970526     C                   MOVEL     FUD(I)        FUDRIG
059700970526     C*
059800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
059900970526     C     FUNKEY        IFEQ      FUDFUN
060000970526     C     FUDATT        ANDEQ     'S'
060100970526     C                   Z-ADD     I             W
060200970526     C                   END
060300970526     C                   END                                                    DO WHILE
060400970526     C*
060500970526     C* FUNZIONE NON ATTIVA (ERRORE)
060600970526     C     W             IFEQ      0
060700970526     C*
060800970526     C* EMISSIONE MESSAGGIO DI ERRORE
060900970526     C                   MOVEL     MSG(1)        DSMSMS
061000970526     C*
061100970526     C                   END                                                    W=0
061200970526     C                   END                                                    FUNKEY NE *BLANKS
061300970526     C*
061400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
061500970526     C     W             IFNE      0
061600970526     C     FUNKEY        OREQ      *BLANKS
061700970526     C     FUNKEY        IFEQ      *BLANKS
061800970526     C                   EXSR      ENTGES
061900970526     C                   END
062000970526     C     FUNKEY        IFEQ      '03'
062100970526     C                   EXSR      F03GES
062200970526     C                   END
062300970526     C     FUNKEY        IFEQ      '06'
062400970526     C                   EXSR      F06GES
062500970526     C                   END
062600130109     C     FUNKEY        IFEQ      '07'
062700130109     C                   EXSR      F07GES
062800130109     C                   END
062900970526     C     FUNKEY        IFEQ      '12'
063000970526     C                   EXSR      F12GES
063100970526     C                   END
063200170202     C     FUNKEY        IFEQ      '21'
063300170202     C                   EXSR      F21GES
063400170202     C                   END
063500970526     C     FUNKEY        IFEQ      '24'
063600970526     C                   EXSR      F24GES
063700970526     C                   END
063800970526     C                   END
063900970526     C*
064000970526     C                   ENDSR
064100970526     C*------------------------------------------------------------------------*
064200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
064300970526     C*------------------------------------------------------------------------*
064400970526     C     FUNVAR        BEGSR
064500970526     C*
064600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
064700970526     C*
064800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
064900970526     C     KEY           IFEQ      ENTER
065000970526     C                   MOVEL     *BLANKS       FUNKEY
065100970526     C                   END
065200970526     C     KEY           IFEQ      F1
065300970526     C                   MOVEL     '01'          FUNKEY
065400970526     C                   END
065500970526     C     KEY           IFEQ      F2
065600970526     C                   MOVEL     '02'          FUNKEY
065700970526     C                   END
065800970526     C     KEY           IFEQ      F3
065900970526     C                   MOVEL     '03'          FUNKEY
066000970526     C                   END
066100970526     C     KEY           IFEQ      F4
066200970526     C                   MOVEL     '04'          FUNKEY
066300970526     C                   END
066400970526     C     KEY           IFEQ      F5
066500970526     C                   MOVEL     '05'          FUNKEY
066600970526     C                   END
066700970526     C     KEY           IFEQ      F6
066800970526     C                   MOVEL     '06'          FUNKEY
066900970526     C                   END
067000970526     C     KEY           IFEQ      F7
067100970526     C                   MOVEL     '07'          FUNKEY
067200970526     C                   END
067300970526     C     KEY           IFEQ      F8
067400970526     C                   MOVEL     '08'          FUNKEY
067500970526     C                   END
067600970526     C     KEY           IFEQ      F9
067700970526     C                   MOVEL     '09'          FUNKEY
067800970526     C                   END
067900970526     C     KEY           IFEQ      F10
068000970526     C                   MOVEL     '10'          FUNKEY
068100970526     C                   END
068200970526     C     KEY           IFEQ      F11
068300970526     C                   MOVEL     '11'          FUNKEY
068400970526     C                   END
068500970526     C     KEY           IFEQ      F12
068600970526     C                   MOVEL     '12'          FUNKEY
068700970526     C                   END
068800970526     C     KEY           IFEQ      F13
068900970526     C                   MOVEL     '13'          FUNKEY
069000970526     C                   END
069100970526     C     KEY           IFEQ      F14
069200970526     C                   MOVEL     '14'          FUNKEY
069300970526     C                   END
069400970526     C     KEY           IFEQ      F15
069500970526     C                   MOVEL     '15'          FUNKEY
069600970526     C                   END
069700970526     C     KEY           IFEQ      F16
069800970526     C                   MOVEL     '16'          FUNKEY
069900970526     C                   END
070000970526     C     KEY           IFEQ      F17
070100970526     C                   MOVEL     '17'          FUNKEY
070200970526     C                   END
070300970526     C     KEY           IFEQ      F18
070400970526     C                   MOVEL     '18'          FUNKEY
070500970526     C                   END
070600970526     C     KEY           IFEQ      F19
070700970526     C                   MOVEL     '19'          FUNKEY
070800970526     C                   END
070900970526     C     KEY           IFEQ      F20
071000970526     C                   MOVEL     '20'          FUNKEY
071100970526     C                   END
071200970526     C     KEY           IFEQ      F21
071300970526     C                   MOVEL     '21'          FUNKEY
071400970526     C                   END
071500970526     C     KEY           IFEQ      F22
071600970526     C                   MOVEL     '22'          FUNKEY
071700970526     C                   END
071800970526     C     KEY           IFEQ      F23
071900970526     C                   MOVEL     '23'          FUNKEY
072000970526     C                   END
072100970526     C     KEY           IFEQ      F24
072200970526     C                   MOVEL     '24'          FUNKEY
072300970526     C                   END
072400970526     C     KEY           IFEQ      ROLLUP
072500970526     C                   MOVEL     'RU'          FUNKEY
072600970526     C                   END
072700970526     C     KEY           IFEQ      HELP
072800970526     C                   MOVEL     'HP'          FUNKEY
072900970526     C                   END
073000970526     C                   ENDSR
073100970526     C*------------------------------------------------------------------------*
073200970526     C* NOTE COSTRUZIONE SCHIERE
073300970526     C*------------------------------------------------------------------------*
073400970526     C*
073500970526     C* -MSG-MESSAGGI
073600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
073700970526     C* (NON TOCCARE MSG,1 E MSG,2)
073800970526     C*
073900970526     C* -CMD-COMANDI
074000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
074100970526     C*
074200970526     C* -OPD-OPZIONI
074300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
074400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
074500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
074600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
074700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
074800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
074900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
075000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
075100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
075200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
075300970526     C*
075400970526     C* -FUD-FUNZIONI
075500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
075600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
075700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
075800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
075900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
076000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
076100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
076200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
076300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
076400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
076500970526** MSG - MESSAGGI
076600120927Tasto funzionale non gestito                                                   1
076700161104Data errata                                                                    2
076800170629Data DA > Data Al                                                              3
076900141216                                                                               4
077000130108                                                                               5
077100130108Campo obbligatorio                                                             6
077200130108                                                                               7
077300130108                                                                               8
077400120927                                                                               9
077500120927                                                                               10
077600120927                                                                               11
077700120927                                                                               12
077800120927                                                                               13
077900141027                                                                               14
078000140116                                                                               15
078100120927                                                                               16
078200120927                                                                               17
078300120927                                                                               18
078400120927                                                                               19
078500170306Job sottomesso                                                                 20
078600141027                                                                               21
078700141027                                                                               22
078800121108                                                                               23
078900121108                                                                               24
079000970526** FUD - FUNZIONI
079100970526                   -        (S/N)=N                        funz.01¬
079200970526                   -        (S/N)=N                        funz.02¬
079300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
079400970526                   -        (S/N)=N                        funz.04¬
079500970526                   -        (S/N)=N                        funz.05¬
079600991214F06=Conferma       -        (S/N)=S                        funz.06¬
079700140819                   -        (S/N)=N                        funz.07¬
079800970527                   -        (S/N)=N                        funz.08¬
079900970527                   -        (S/N)=N                        funz.09¬
080000970527                   -        (S/N)=N                        funz.10¬
080100970526                   -        (S/N)=N                        funz.11¬
080200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
080300970526                   -        (S/N)=N                        funz.13¬
080400970526                   -        (S/N)=N                        funz.14¬
080500970526                   -        (S/N)=N                        funz.15¬
080600970526                   -        (S/N)=N                        funz.16¬
080700970526                   -        (S/N)=N                        funz.17¬
080800970526                   -        (S/N)=N                        funz.18¬
080900970526                   -        (S/N)=N                        funz.19¬
081000970526                   -        (S/N)=N                        funz.20¬
081100170629F21=Param.batch    -        (S/N)=N                        funz.21¬
081200970526                   -        (S/N)=N                        funz.22¬
081300970526                   -        (S/N)=N                        funz.23¬
081400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
