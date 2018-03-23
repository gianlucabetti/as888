000100970526     H*------------------------------------------------------------------------*
000200170426     H* Test Agg. file TIVCP00F
000300970526     H*------------------------------------------------------------------------*
000400161104     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000500161104     H BNDDIR('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900170426     FUBVCP0D1  cf   e             WORKSTN INFDS(DEVDS1)
001000170426     F                                     SFILE(VCP0DMS:MSGULT)
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
008200141028     D*-------------------
008300170426     D* prototipi UBVCP0R
008400141028     D*-------------------
008500170426      /copy gaitrasrc/srcProtoPR,UBVCP0R_02
008600170426      /copy gaitrasrc/srcProtoPR,UBVCP0R_03
008700171019      /copy gaitrasrc/srcProtoPR,UBVCP0R_05
008800120928     D*-------------------
008900120928
009000121003     d Status         sds
009100121003     d  SDSpgm           *proc
009200121003     d  SDSprm           *parms
009300121003     d  SDSuser              254    263
009400170202
009500130116     d ora             s               t
009600140117     d wX              s              2s 0
009700170426     d Esito           s              2s 0
009800161104     D*------------
009900171019     d w03Anno         s                   like(D03Anno) based(p03Anno)
010000171019     d w03From         s                   like(D03From) based(p03From)
010100171019     d w03To           s                   like(D03To  ) based(p03To  )
010200171019     d w03Incr         s                   like(D03Incr) based(p03Incr)
010300171019     d w03MaxPrg       s                   like(D03MaxPrg) based(p03MaxPrg)
010400171019     D*------------
010500161104     D* campi tipo data per le date a video
010600161104     D DtDaISO         s               d
010700161104     D DtAlISO         s               d
010800120928
010900970526     C*------------------------------------------------------------------------*
011000000303     C* MAIN LINES
011100970526     C*------------------------------------------------------------------------*
011200970526     C*
011300120927     C* metto i default su D03
011400120927     C                   EXSR      InzD03
011500120927     C*
011600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012000970526     C*
012100970526     C* EMETTE FORMATO VIDEO
012200970526     C                   EXSR      BUTFMR
012300970526     C*
012400970526     C* GESTIONE TASTI FUNZIONALI
012500970526     C                   EXSR      FUNCON
012600970526E   2C                   ENDDO
012700970526     C*
012800970526     C* FINE PGM
012900970526     C                   SETON                                        LR
013000970526     C*------------------------------------------------------------------------*
013100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013200970526     C*------------------------------------------------------------------------*
013300970526     C     BUTFMR        BEGSR
013400970526     C*
013500970526     C* EMETTE LA TESTATA
013600170426     C                   WRITE     VCP0D01
013700970526     C*
013800970526     C* SPEDISCE EVENTUALE MESSAGGIO
013900970526     C                   EXSR      SNDMSG
014000970526     C*
014100970526     C* EMETTE LE FUNZIONI
014200170426     C                   WRITE     VCP0D02
014300970526     C*
014400970526     C* EMETTE IL VIDEO
014500170426     C                   EXFMT     VCP0D03
014600970526     C*
014700991214     C* REIMPOSTA FLAG DI ERRORE
014800991214     C                   MOVEL     '0'           ERRL00
014900970526     C*
015000970526     C* REIMPOSTA INDICATORI DI ERRORE
015100020611     C                   SETOFF                                       555556
015200970526     C*
015300970526     C                   ENDSR
015400970526     C*------------------------------------------------------------------------*
015500120927     C* InzD03 - Inizializzo D03
015600970526     C*------------------------------------------------------------------------*
015700120927     C     InzD03        BEGSR
015800120927     C*
015900170426     C                   clear                   D03TpID
016000170426     C                   clear                   D03ID
016100170426     C                   clear                   D03Anno
016200170426     C                   clear                   D03From
016300170426     C                   clear                   D03To
016400170426     C                   clear                   D03Incr
016500170426     C                   clear                   D03MaxPrg
016600171019         eval p03Anno = *null;
016700171019         eval p03From = *null;
016800171019         eval p03To   = *null;
016900171019         eval p03Incr = *null;
017000171019         eval p03MaxPrg = *null;
017100170426     C                   eval      D03IA ='I'
017200130108     C*
017300120927     C                   ENDSR
017400120927     C*------------------------------------------------------------------------*
017500120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017600120927     C*------------------------------------------------------------------------*
017700120927     C     SNDMSG        BEGSR
017800120927     C*
017900970526     C* PULISCE IL SFILE MESSAGGI
018000970526     C                   MOVEL     '1'           *IN40
018100170426     C                   WRITE     VCP0DMC
018200970526     C                   MOVEL     '0'           *IN40
018300970526     C*
018400970526     C* VISUALIZZA IL MESSAGGIO
018500970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018600970526     C                   CALL      'XMSG'        XMSGPA
018700020611     C     DSMSER        IFEQ      *ZEROS
018800970526     C                   MOVEL     DSMSKE        MSGKEY
018900020611     C                   ENDIF
019000970526     C                   Z-ADD     1             MSGULT
019100170426     C                   WRITE     VCP0DMS
019200170426     C                   WRITE     VCP0DMC
019300970526     C*
019400970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019500970526     C                   MOVEL     *BLANKS       DSMSMS
019600970526     C*
019700970526     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* ENTGES - GESTIONE ENTER -> INVIO
020000970526     C*------------------------------------------------------------------------*
020100970526     C     ENTGES        BEGSR
020200970526     C*
020300970526     C* CONTROLLA I CAMPI IMMESSI
020400970526     C                   EXSR      DECODI
020500970526     C*
020600970526     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800970526     C* F03GES - GESTIONE F03 -> FINE
020900970526     C*------------------------------------------------------------------------*
021000970526     C     F03GES        BEGSR
021100970526     C*
021200970526     C* IMPOSTA "PREMUTO F03"
021300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021400970526     C*
021500970526     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700991214     C* F06GES - GESTIONE F06 -> CONFERMA
021800970526     C*------------------------------------------------------------------------*
021900970526     C     F06GES        BEGSR
022000970526     C*
022100991214     C* CONTROLLA I CAMPI IMMESSI
022200991214     C                   EXSR      DECODI
022300020611     C*
022400991214     C* ESEGUE ROUTINE DI CONFERMA
022500020611     C     ERRL00        IFEQ      '0'
022600991214     C                   EXSR      ESEGUI
022700170306     C* messaggio di messa in coda del lavoro
022800170426     C                   MOVEL     MSG(20)       DSMSMS
022900121109     C* non esco mai dal pgm
023000121109     C***                MOVEL     '1'           FINL00
023100121108     C                   ENDIF
023200970526     C*
023300991214     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500130109     C* F07GES - GESTIONE F07 -> STAMPA
023600970526     C*------------------------------------------------------------------------*
023700130109     C     F07GES        BEGSR
023800970526     C*
023900140818     C                   EXSR      F06GES
024000130110     C*
024100970526     C                   ENDSR
024200130109     C*------------------------------------------------------------------------*
024300130109     C* F12GES - GESTIONE F12 -> RITORNO
024400130109     C*------------------------------------------------------------------------*
024500130109     C     F12GES        BEGSR
024600130109     C*
024700130109     C* IMPOSTA "PREMUTO F12"
024800130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024900130109     C*
025000130109     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025300970526     C*------------------------------------------------------------------------*
025400970526     C     F24GES        BEGSR
025500970526     C*
025600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025700970526     C*
025800970526     C                   ENDSR
025900970526     C*------------------------------------------------------------------------*
026000140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
026100970526     C*------------------------------------------------------------------------*
026200991214     C     ESEGUI        BEGSR
026300161111     C*
026400170426          if d03IA = 'I';
026500170426            ubvcp0r_crt(d03TpID:d03ID:d03Anno:d03From:d03To:d03Incr:
026600170426                        d03MaxPrg:esito);
026700170426          endif;
026800170426          if d03IA = 'A';
026900170426            ubvcp0r_ann(d03TpID:d03ID:
027000170426                                  esito);
027100170426          endif;
027200171019          if d03IA = 'M';
027300171019            if D03Anno <> 0;
027400171019              eval p03Anno = %addr(D03Anno);
027500171019            endif;
027600171019            if D03From <> 0;
027700171019              eval p03From = %addr(D03From);
027800171019            endif;
027900171019            if D03To   <> 0;
028000171019              eval p03To   = %addr(D03To  );
028100171019            endif;
028200171019            if D03Incr <> 0;
028300171019              eval p03Incr = %addr(D03Incr);
028400171019            endif;
028500171019            if D03MaxPrg <> 0;
028600171019              eval p03MaxPrg = %addr(D03MaxPrg);
028700171019            endif;
028800171019     C***                CALLP     UBVCP0R.UPD(
028900171019     C***                           d03TpID:d03ID:w03Anno:w03From:w03To:w03Incr:
029000171019     C***                           w03MaxPrg:esito)
029100171019            ubvcp0r_upd(d03TpID:d03ID:*omit  :w03From:w03To:*omit  :
029200171019                        *omit    :esito);
029300171019          endif;
029400170426          D03Esito = Esito;
029500140120     C*
029600970526     C                   ENDSR
029700970526     C*------------------------------------------------------------------------*
029800991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029900970526     C*------------------------------------------------------------------------*
030000970526     C     DECODI        BEGSR
030100121019     C*
030200120927     C                   SETOFF                                       505152
030300140120     c                   EVAL      VIDMSG    = *blank
030400170426
030500170426         // se a video l'incremento non è stato indicato, lo considero =1
030600170426         if D03Incr = 0;
030700170426           D03Incr = 1;
030800170426         endif;
030900170427         // se a video il nr. max stacchi non è stato indicato, lo considero =1
031000170427         if D03MaxPrg = 0;
031100170427           D03MaxPrg = 1;
031200170427         endif;
031300141216     C*
031400170202     C*
031500970528     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
031800970526     C*------------------------------------------------------------------------*
031900970526     C     AZZVID        BEGSR
032000141027     C*
032100170426     C                   clear                   D03TpID
032200170426     C                   clear                   D03ID
032300170426     C                   clear                   D03Anno
032400170426     C                   clear                   D03From
032500170426     C                   clear                   D03To
032600170426     C                   clear                   D03Incr
032700170426     C                   clear                   D03MaxPrg
032800170426     C                   eval      D03IA ='I'
032900970526     C*
033000970526     C                   ENDSR
033100970526     C*------------------------------------------------------------------------*
033200020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
033300970526     C*------------------------------------------------------------------------*
033400970526     C     IMPFUN        BEGSR
033500970526     C*
033600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
033700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
033800970526     C                   EXSR      FUNGES
033900970526     C*
034000970526     C                   ENDSR
034100020923     C*--------------------------------------------------------------------------------------------*
034200020923     C* REPERISCE I DATI UTENTE
034300020923     C*--------------------------------------------------------------------------------------------*
034400020923     C     REPDATIUTE    BEGSR
034500020923     C*
034600020923     C* INIZIALIZZA VARIABILI DI WRK
034700020923     C                   CLEAR                   TIBS34DS
034800020923     C                   CLEAR                   AZUTEDS
034900020923     C                   CLEAR                   DDATIUTE
035000020923     C*
035100020923     C     *DTAARA       DEFINE    §azute        azuteds
035200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
035300020923     C                   IN(E)     *DTAARA
035400020923if  1C                   IF        %Error
035500020923     c                   EVAL      I34Tla = 'L'
035600020923     C                   CALL      'TIBS34R'
035700020923     C                   PARM                    Tibs34Ds
035800020923     C                   IN        *DTAARA
035900020923e   1C                   ENDIF
036000020923     C*
036100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
036200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
036300020923     C*
036400020923     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* *INZSR - OPERAZIONI INIZIALI
036700970526     C*------------------------------------------------------------------------*
036800970526     C     *INZSR        BEGSR
036900991217     C*
037000991217     C     *ENTRY        PLIST
037100991217     C                   PARM                    KPJBA
037200020923     C*
037300020923     C* REPERISCE I DATI DELL'UTENTE
037400020923     C                   EXSR      REPDATIUTE
037500970526     C*
037600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
037700970526     C     XMSGPA        PLIST
037800970526     C                   PARM                    DSMSMS
037900970526     C                   PARM                    DSMSPG
038000970526     C                   PARM                    DSMSKE
038100970526     C                   PARM                    DSMSER
038200970526     C*
038300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
038400970526     C                   CALL      'XKEY'
038500970526     C                   PARM                    DSKEY
038600970526     C*---
038700970526     C* VARIABILI NON RIFERITE AL DATA BASE
038800970526     C*---
038900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
039000991214     C                   MOVEL     *BLANKS       DEPOP1            3
039100970526     C*---
039200970526     C* VARIABILI DI CONTROLLO
039300970526     C*---
039400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
039500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
039600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
039700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
039800970526     C*
039900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
040000170426     C                   MOVEL     'UBVCP0R1'    DSMSPG
040100170426     C                   MOVEL     'UBVCP0R1'    MSGQUE
040200970526     C                   MOVEL     *BLANKS       MSGKEY
040300970526     C                   MOVEL     *BLANKS       DSMSMS
040400970528     C*
040500970528     C* CALCOLA LA DATA CORRENTE
040600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
040700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
040800970528     C                   Z-ADD     WN8           G08DAT
040900970528     C                   Z-ADD     *ZEROS        G08INV
041000970528     C                   MOVEL     '0'           G08ERR
041100970528     C                   CALL      'XSRDA8'
041200970528     C                   PARM                    WLBDA8
041300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
041400000310     C*
041500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
041600000310     C                   EXSR      AZZVID
041700970526     C*
041800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
041900970526     C                   EXSR      IMPFUN
042000970526     C*
042100970526     C                   ENDSR
042200970526     C*------------------------------------------------------------------------*
042300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
042400970526     C*------------------------------------------------------------------------*
042500970526     C     FUNGES        BEGSR
042600970526     C*
042700970526     C* PULISCO CAMPI DI LAVORO
042800970526     C                   Z-ADD     0             W                 5 0
042900970526     C*
043000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
043100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
043200970526     C     1             DO        24            I
043300970526     C                   MOVEL     FUD(I)        FUDRIG
043400970526     C*
043500970526     C* PULIZIA FUNZIONI PRECEDENTI
043600970526     C                   MOVEL     *BLANKS       FUV(I)
043700970526     C*
043800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
043900970526     C* (SALTA F24)
044000970526     C     FUDATT        IFEQ      'S'
044100970526     C     FUDFUN        ANDNE     '24'
044200970526     C                   ADD       1             W
044300970526     C                   MOVEL     FUDDES        FUV(W)
044400970526     C                   END
044500970526     C                   END                                                    DO WHILE
044600970526     C*
044700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
044800970526     C* ALTRIMENTI DISATTIVO F24
044900970526     C     1             DO        24            I
045000970526     C                   MOVEL     FUD(I)        FUDRIG
045100970526     C*
045200970526     C     FUDFUN        IFEQ      '24'
045300970526     C     W             IFGT      6
045400970526     C                   MOVEL     'S'           FUDATT
045500970526     C                   ELSE
045600970526     C                   MOVEL     *BLANKS       FUDATT
045700970526     C                   END                                                    W GT 8
045800970526     C                   MOVEL     FUDRIG        FUD(I)
045900970526     C                   END
046000970526     C                   END                                                    DO WHILE
046100970526     C*
046200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
046300970526     C     W             DIV       6             I
046400970526     C                   MVR                     Z
046500970526     C*
046600970526     C     Z             IFNE      0
046700970526     C     I             ADD       1             Z
046800970526     C                   ELSE
046900970526     C                   Z-ADD     I             Z                 5 0
047000970526     C                   END
047100970526     C*
047200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
047300970526     C                   ADD       1             FUNGRU
047400970526     C     FUNGRU        IFGT      Z
047500970526     C     FUNGRU        ORLT      1
047600970526     C                   Z-ADD     1             FUNGRU
047700970526     C                   END
047800970526     C*
047900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
048000970526     C     FUNGRU        IFEQ      1
048100970526     C                   MOVEL     FUV(1)        FUN001
048200970526     C                   MOVEL     FUV(2)        FUN002
048300970526     C                   MOVEL     FUV(3)        FUN003
048400970526     C                   MOVEL     FUV(4)        FUN004
048500970526     C                   MOVEL     FUV(5)        FUN005
048600970526     C                   MOVEL     FUV(6)        FUN006
048700970526     C                   END
048800970526     C*
048900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
049000970526     C     FUNGRU        IFEQ      2
049100970526     C                   MOVEL     FUV(7)        FUN001
049200970526     C                   MOVEL     FUV(8)        FUN002
049300970526     C                   MOVEL     FUV(9)        FUN003
049400970526     C                   MOVEL     FUV(10)       FUN004
049500970526     C                   MOVEL     FUV(11)       FUN005
049600970526     C                   MOVEL     FUV(12)       FUN006
049700970526     C                   END
049800970526     C*
049900970526     C* TERZO GRUPPO DI 6 FUNZIONI
050000970526     C     FUNGRU        IFEQ      3
050100970526     C                   MOVEL     FUV(13)       FUN001
050200970526     C                   MOVEL     FUV(14)       FUN002
050300970526     C                   MOVEL     FUV(15)       FUN003
050400970526     C                   MOVEL     FUV(16)       FUN004
050500970526     C                   MOVEL     FUV(17)       FUN005
050600970526     C                   MOVEL     FUV(18)       FUN006
050700970526     C                   END
050800970526     C*
050900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
051000970526     C     FUNGRU        IFEQ      4
051100970526     C                   MOVEL     FUV(19)       FUN001
051200970526     C                   MOVEL     FUV(20)       FUN002
051300970526     C                   MOVEL     FUV(21)       FUN003
051400970526     C                   MOVEL     FUV(22)       FUN004
051500970526     C                   MOVEL     *BLANKS       FUN005
051600970526     C                   MOVEL     *BLANKS       FUN006
051700970526     C                   END
051800970526     C*
051900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
052000970526     C                   MOVEL     *BLANKS       FUN007
052100970526     C                   MOVEL     *BLANKS       FUN008
052200970526     C*
052300970526     C     1             DO        24            I
052400970526     C                   MOVEL     FUD(I)        FUDRIG
052500970526     C     FUDFUN        IFEQ      '24'
052600970526     C     FUDATT        ANDEQ     'S'
052700970526     C                   MOVEL     FUDDES        FUN008
052800970526     C                   END
052900970526     C                   END                                                    DO
053000970526     C*
053100970526     C                   ENDSR
053200970526     C*------------------------------------------------------------------------*
053300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
053400970526     C*------------------------------------------------------------------------*
053500970526     C     FUNCON        BEGSR
053600970526     C*
053700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
053800970526     C                   EXSR      FUNVAR
053900970526     C*
054000970526     C* PULISCO CAMPI DI LAVORO
054100970526     C                   Z-ADD     0             I                 5 0
054200970526     C                   Z-ADD     0             W                 5 0
054300970526     C*
054400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
054500970526     C     FUNKEY        IFNE      *BLANKS
054600970526     C     1             DO        24            I
054700970526     C                   MOVEL     FUD(I)        FUDRIG
054800970526     C*
054900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
055000970526     C     FUNKEY        IFEQ      FUDFUN
055100970526     C     FUDATT        ANDEQ     'S'
055200970526     C                   Z-ADD     I             W
055300970526     C                   END
055400970526     C                   END                                                    DO WHILE
055500970526     C*
055600970526     C* FUNZIONE NON ATTIVA (ERRORE)
055700970526     C     W             IFEQ      0
055800970526     C*
055900970526     C* EMISSIONE MESSAGGIO DI ERRORE
056000970526     C                   MOVEL     MSG(1)        DSMSMS
056100970526     C*
056200970526     C                   END                                                    W=0
056300970526     C                   END                                                    FUNKEY NE *BLANKS
056400970526     C*
056500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
056600970526     C     W             IFNE      0
056700970526     C     FUNKEY        OREQ      *BLANKS
056800970526     C     FUNKEY        IFEQ      *BLANKS
056900970526     C                   EXSR      ENTGES
057000970526     C                   END
057100970526     C     FUNKEY        IFEQ      '03'
057200970526     C                   EXSR      F03GES
057300970526     C                   END
057400970526     C     FUNKEY        IFEQ      '06'
057500970526     C                   EXSR      F06GES
057600970526     C                   END
057700130109     C     FUNKEY        IFEQ      '07'
057800130109     C                   EXSR      F07GES
057900130109     C                   END
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
071500161104Data errata                                                                    2
071600141216                                                                               3
071700141216                                                                               4
071800130108                                                                               5
071900130108Campo obbligatorio                                                             6
072000130108                                                                               7
072100130108                                                                               8
072200120927                                                                               9
072300120927                                                                               10
072400120927                                                                               11
072500120927                                                                               12
072600120927                                                                               13
072700141027                                                                               14
072800140116                                                                               15
072900120927                                                                               16
073000120927                                                                               17
073100120927                                                                               18
073200120927                                                                               19
073300170426Elaborazione eseguita: controllare l'esito                                     20
073400141027                                                                               21
073500141027                                                                               22
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
075900170426                   -        (S/N)=N                        funz.21¬
076000970526                   -        (S/N)=N                        funz.22¬
076100970526                   -        (S/N)=N                        funz.23¬
076200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
