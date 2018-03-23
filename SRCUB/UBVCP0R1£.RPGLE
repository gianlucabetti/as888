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
008700120928     D*-------------------
008800120928
008900121003     d Status         sds
009000121003     d  SDSpgm           *proc
009100121003     d  SDSprm           *parms
009200121003     d  SDSuser              254    263
009300170202
009400130116     d ora             s               t
009500140117     d wX              s              2s 0
009600170426     d Esito           s              2s 0
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
012900170426     C                   WRITE     VCP0D01
013000970526     C*
013100970526     C* SPEDISCE EVENTUALE MESSAGGIO
013200970526     C                   EXSR      SNDMSG
013300970526     C*
013400970526     C* EMETTE LE FUNZIONI
013500170426     C                   WRITE     VCP0D02
013600970526     C*
013700970526     C* EMETTE IL VIDEO
013800170426     C                   EXFMT     VCP0D03
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
015200170426     C                   clear                   D03TpID
015300170426     C                   clear                   D03ID
015400170426     C                   clear                   D03Anno
015500170426     C                   clear                   D03From
015600170426     C                   clear                   D03To
015700170426     C                   clear                   D03Incr
015800170426     C                   clear                   D03MaxPrg
015900170426     C                   eval      D03IA ='I'
016000130108     C*
016100120927     C                   ENDSR
016200120927     C*------------------------------------------------------------------------*
016300120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016400120927     C*------------------------------------------------------------------------*
016500120927     C     SNDMSG        BEGSR
016600120927     C*
016700970526     C* PULISCE IL SFILE MESSAGGI
016800970526     C                   MOVEL     '1'           *IN40
016900170426     C                   WRITE     VCP0DMC
017000970526     C                   MOVEL     '0'           *IN40
017100970526     C*
017200970526     C* VISUALIZZA IL MESSAGGIO
017300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017400970526     C                   CALL      'XMSG'        XMSGPA
017500020611     C     DSMSER        IFEQ      *ZEROS
017600970526     C                   MOVEL     DSMSKE        MSGKEY
017700020611     C                   ENDIF
017800970526     C                   Z-ADD     1             MSGULT
017900170426     C                   WRITE     VCP0DMS
018000170426     C                   WRITE     VCP0DMC
018100970526     C*
018200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018300970526     C                   MOVEL     *BLANKS       DSMSMS
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* ENTGES - GESTIONE ENTER -> INVIO
018800970526     C*------------------------------------------------------------------------*
018900970526     C     ENTGES        BEGSR
019000970526     C*
019100970526     C* CONTROLLA I CAMPI IMMESSI
019200970526     C                   EXSR      DECODI
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* F03GES - GESTIONE F03 -> FINE
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F03GES        BEGSR
019900970526     C*
020000970526     C* IMPOSTA "PREMUTO F03"
020100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500991214     C* F06GES - GESTIONE F06 -> CONFERMA
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F06GES        BEGSR
020800970526     C*
020900991214     C* CONTROLLA I CAMPI IMMESSI
021000991214     C                   EXSR      DECODI
021100020611     C*
021200991214     C* ESEGUE ROUTINE DI CONFERMA
021300020611     C     ERRL00        IFEQ      '0'
021400991214     C                   EXSR      ESEGUI
021500170306     C* messaggio di messa in coda del lavoro
021600170426     C                   MOVEL     MSG(20)       DSMSMS
021700121109     C* non esco mai dal pgm
021800121109     C***                MOVEL     '1'           FINL00
021900121108     C                   ENDIF
022000970526     C*
022100991214     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300130109     C* F07GES - GESTIONE F07 -> STAMPA
022400970526     C*------------------------------------------------------------------------*
022500130109     C     F07GES        BEGSR
022600970526     C*
022700140818     C                   EXSR      F06GES
022800130110     C*
022900970526     C                   ENDSR
023000130109     C*------------------------------------------------------------------------*
023100130109     C* F12GES - GESTIONE F12 -> RITORNO
023200130109     C*------------------------------------------------------------------------*
023300130109     C     F12GES        BEGSR
023400130109     C*
023500130109     C* IMPOSTA "PREMUTO F12"
023600130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023700130109     C*
023800130109     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024100970526     C*------------------------------------------------------------------------*
024200970526     C     F24GES        BEGSR
024300970526     C*
024400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024500970526     C*
024600970526     C                   ENDSR
024700970526     C*------------------------------------------------------------------------*
024800140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
024900970526     C*------------------------------------------------------------------------*
025000991214     C     ESEGUI        BEGSR
025100161111     C*
025200170426          if d03IA = 'I';
025300170426            ubvcp0r_crt(d03TpID:d03ID:d03Anno:d03From:d03To:d03Incr:
025400170426                        d03MaxPrg:esito);
025500170426          endif;
025600170426          if d03IA = 'A';
025700170426            ubvcp0r_ann(d03TpID:d03ID:
025800170426                                  esito);
025900170426          endif;
026000170426          D03Esito = Esito;
026100140120     C*
026200970526     C                   ENDSR
026300970526     C*------------------------------------------------------------------------*
026400991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026500970526     C*------------------------------------------------------------------------*
026600970526     C     DECODI        BEGSR
026700121019     C*
026800120927     C                   SETOFF                                       505152
026900140120     c                   EVAL      VIDMSG    = *blank
027000170426
027100170426         // se a video l'incremento non è stato indicato, lo considero =1
027200170426         if D03Incr = 0;
027300170426           D03Incr = 1;
027400170426         endif;
027500170427         // se a video il nr. max stacchi non è stato indicato, lo considero =1
027600170427         if D03MaxPrg = 0;
027700170427           D03MaxPrg = 1;
027800170427         endif;
027900141216     C*
028000170202     C*
028100970528     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
028400970526     C*------------------------------------------------------------------------*
028500970526     C     AZZVID        BEGSR
028600141027     C*
028700170426     C                   clear                   D03TpID
028800170426     C                   clear                   D03ID
028900170426     C                   clear                   D03Anno
029000170426     C                   clear                   D03From
029100170426     C                   clear                   D03To
029200170426     C                   clear                   D03Incr
029300170426     C                   clear                   D03MaxPrg
029400170426     C                   eval      D03IA ='I'
029500970526     C*
029600970526     C                   ENDSR
029700970526     C*------------------------------------------------------------------------*
029800020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
029900970526     C*------------------------------------------------------------------------*
030000970526     C     IMPFUN        BEGSR
030100970526     C*
030200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
030300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
030400970526     C                   EXSR      FUNGES
030500970526     C*
030600970526     C                   ENDSR
030700020923     C*--------------------------------------------------------------------------------------------*
030800020923     C* REPERISCE I DATI UTENTE
030900020923     C*--------------------------------------------------------------------------------------------*
031000020923     C     REPDATIUTE    BEGSR
031100020923     C*
031200020923     C* INIZIALIZZA VARIABILI DI WRK
031300020923     C                   CLEAR                   TIBS34DS
031400020923     C                   CLEAR                   AZUTEDS
031500020923     C                   CLEAR                   DDATIUTE
031600020923     C*
031700020923     C     *DTAARA       DEFINE    §azute        azuteds
031800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
031900020923     C                   IN(E)     *DTAARA
032000020923if  1C                   IF        %Error
032100020923     c                   EVAL      I34Tla = 'L'
032200020923     C                   CALL      'TIBS34R'
032300020923     C                   PARM                    Tibs34Ds
032400020923     C                   IN        *DTAARA
032500020923e   1C                   ENDIF
032600020923     C*
032700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
032800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
032900020923     C*
033000020923     C                   ENDSR
033100970526     C*------------------------------------------------------------------------*
033200970526     C* *INZSR - OPERAZIONI INIZIALI
033300970526     C*------------------------------------------------------------------------*
033400970526     C     *INZSR        BEGSR
033500991217     C*
033600991217     C     *ENTRY        PLIST
033700991217     C                   PARM                    KPJBA
033800020923     C*
033900020923     C* REPERISCE I DATI DELL'UTENTE
034000020923     C                   EXSR      REPDATIUTE
034100970526     C*
034200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
034300970526     C     XMSGPA        PLIST
034400970526     C                   PARM                    DSMSMS
034500970526     C                   PARM                    DSMSPG
034600970526     C                   PARM                    DSMSKE
034700970526     C                   PARM                    DSMSER
034800970526     C*
034900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
035000970526     C                   CALL      'XKEY'
035100970526     C                   PARM                    DSKEY
035200970526     C*---
035300970526     C* VARIABILI NON RIFERITE AL DATA BASE
035400970526     C*---
035500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
035600991214     C                   MOVEL     *BLANKS       DEPOP1            3
035700970526     C*---
035800970526     C* VARIABILI DI CONTROLLO
035900970526     C*---
036000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
036100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
036200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
036300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
036400970526     C*
036500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
036600170426     C                   MOVEL     'UBVCP0R1'    DSMSPG
036700170426     C                   MOVEL     'UBVCP0R1'    MSGQUE
036800970526     C                   MOVEL     *BLANKS       MSGKEY
036900970526     C                   MOVEL     *BLANKS       DSMSMS
037000970528     C*
037100970528     C* CALCOLA LA DATA CORRENTE
037200970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
037300970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
037400970528     C                   Z-ADD     WN8           G08DAT
037500970528     C                   Z-ADD     *ZEROS        G08INV
037600970528     C                   MOVEL     '0'           G08ERR
037700970528     C                   CALL      'XSRDA8'
037800970528     C                   PARM                    WLBDA8
037900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
038000000310     C*
038100000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
038200000310     C                   EXSR      AZZVID
038300970526     C*
038400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
038500970526     C                   EXSR      IMPFUN
038600970526     C*
038700970526     C                   ENDSR
038800970526     C*------------------------------------------------------------------------*
038900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
039000970526     C*------------------------------------------------------------------------*
039100970526     C     FUNGES        BEGSR
039200970526     C*
039300970526     C* PULISCO CAMPI DI LAVORO
039400970526     C                   Z-ADD     0             W                 5 0
039500970526     C*
039600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
039700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
039800970526     C     1             DO        24            I
039900970526     C                   MOVEL     FUD(I)        FUDRIG
040000970526     C*
040100970526     C* PULIZIA FUNZIONI PRECEDENTI
040200970526     C                   MOVEL     *BLANKS       FUV(I)
040300970526     C*
040400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
040500970526     C* (SALTA F24)
040600970526     C     FUDATT        IFEQ      'S'
040700970526     C     FUDFUN        ANDNE     '24'
040800970526     C                   ADD       1             W
040900970526     C                   MOVEL     FUDDES        FUV(W)
041000970526     C                   END
041100970526     C                   END                                                    DO WHILE
041200970526     C*
041300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
041400970526     C* ALTRIMENTI DISATTIVO F24
041500970526     C     1             DO        24            I
041600970526     C                   MOVEL     FUD(I)        FUDRIG
041700970526     C*
041800970526     C     FUDFUN        IFEQ      '24'
041900970526     C     W             IFGT      6
042000970526     C                   MOVEL     'S'           FUDATT
042100970526     C                   ELSE
042200970526     C                   MOVEL     *BLANKS       FUDATT
042300970526     C                   END                                                    W GT 8
042400970526     C                   MOVEL     FUDRIG        FUD(I)
042500970526     C                   END
042600970526     C                   END                                                    DO WHILE
042700970526     C*
042800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
042900970526     C     W             DIV       6             I
043000970526     C                   MVR                     Z
043100970526     C*
043200970526     C     Z             IFNE      0
043300970526     C     I             ADD       1             Z
043400970526     C                   ELSE
043500970526     C                   Z-ADD     I             Z                 5 0
043600970526     C                   END
043700970526     C*
043800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
043900970526     C                   ADD       1             FUNGRU
044000970526     C     FUNGRU        IFGT      Z
044100970526     C     FUNGRU        ORLT      1
044200970526     C                   Z-ADD     1             FUNGRU
044300970526     C                   END
044400970526     C*
044500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
044600970526     C     FUNGRU        IFEQ      1
044700970526     C                   MOVEL     FUV(1)        FUN001
044800970526     C                   MOVEL     FUV(2)        FUN002
044900970526     C                   MOVEL     FUV(3)        FUN003
045000970526     C                   MOVEL     FUV(4)        FUN004
045100970526     C                   MOVEL     FUV(5)        FUN005
045200970526     C                   MOVEL     FUV(6)        FUN006
045300970526     C                   END
045400970526     C*
045500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
045600970526     C     FUNGRU        IFEQ      2
045700970526     C                   MOVEL     FUV(7)        FUN001
045800970526     C                   MOVEL     FUV(8)        FUN002
045900970526     C                   MOVEL     FUV(9)        FUN003
046000970526     C                   MOVEL     FUV(10)       FUN004
046100970526     C                   MOVEL     FUV(11)       FUN005
046200970526     C                   MOVEL     FUV(12)       FUN006
046300970526     C                   END
046400970526     C*
046500970526     C* TERZO GRUPPO DI 6 FUNZIONI
046600970526     C     FUNGRU        IFEQ      3
046700970526     C                   MOVEL     FUV(13)       FUN001
046800970526     C                   MOVEL     FUV(14)       FUN002
046900970526     C                   MOVEL     FUV(15)       FUN003
047000970526     C                   MOVEL     FUV(16)       FUN004
047100970526     C                   MOVEL     FUV(17)       FUN005
047200970526     C                   MOVEL     FUV(18)       FUN006
047300970526     C                   END
047400970526     C*
047500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
047600970526     C     FUNGRU        IFEQ      4
047700970526     C                   MOVEL     FUV(19)       FUN001
047800970526     C                   MOVEL     FUV(20)       FUN002
047900970526     C                   MOVEL     FUV(21)       FUN003
048000970526     C                   MOVEL     FUV(22)       FUN004
048100970526     C                   MOVEL     *BLANKS       FUN005
048200970526     C                   MOVEL     *BLANKS       FUN006
048300970526     C                   END
048400970526     C*
048500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
048600970526     C                   MOVEL     *BLANKS       FUN007
048700970526     C                   MOVEL     *BLANKS       FUN008
048800970526     C*
048900970526     C     1             DO        24            I
049000970526     C                   MOVEL     FUD(I)        FUDRIG
049100970526     C     FUDFUN        IFEQ      '24'
049200970526     C     FUDATT        ANDEQ     'S'
049300970526     C                   MOVEL     FUDDES        FUN008
049400970526     C                   END
049500970526     C                   END                                                    DO
049600970526     C*
049700970526     C                   ENDSR
049800970526     C*------------------------------------------------------------------------*
049900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
050000970526     C*------------------------------------------------------------------------*
050100970526     C     FUNCON        BEGSR
050200970526     C*
050300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
050400970526     C                   EXSR      FUNVAR
050500970526     C*
050600970526     C* PULISCO CAMPI DI LAVORO
050700970526     C                   Z-ADD     0             I                 5 0
050800970526     C                   Z-ADD     0             W                 5 0
050900970526     C*
051000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
051100970526     C     FUNKEY        IFNE      *BLANKS
051200970526     C     1             DO        24            I
051300970526     C                   MOVEL     FUD(I)        FUDRIG
051400970526     C*
051500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
051600970526     C     FUNKEY        IFEQ      FUDFUN
051700970526     C     FUDATT        ANDEQ     'S'
051800970526     C                   Z-ADD     I             W
051900970526     C                   END
052000970526     C                   END                                                    DO WHILE
052100970526     C*
052200970526     C* FUNZIONE NON ATTIVA (ERRORE)
052300970526     C     W             IFEQ      0
052400970526     C*
052500970526     C* EMISSIONE MESSAGGIO DI ERRORE
052600970526     C                   MOVEL     MSG(1)        DSMSMS
052700970526     C*
052800970526     C                   END                                                    W=0
052900970526     C                   END                                                    FUNKEY NE *BLANKS
053000970526     C*
053100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
053200970526     C     W             IFNE      0
053300970526     C     FUNKEY        OREQ      *BLANKS
053400970526     C     FUNKEY        IFEQ      *BLANKS
053500970526     C                   EXSR      ENTGES
053600970526     C                   END
053700970526     C     FUNKEY        IFEQ      '03'
053800970526     C                   EXSR      F03GES
053900970526     C                   END
054000970526     C     FUNKEY        IFEQ      '06'
054100970526     C                   EXSR      F06GES
054200970526     C                   END
054300130109     C     FUNKEY        IFEQ      '07'
054400130109     C                   EXSR      F07GES
054500130109     C                   END
054600970526     C     FUNKEY        IFEQ      '12'
054700970526     C                   EXSR      F12GES
054800970526     C                   END
054900970526     C     FUNKEY        IFEQ      '24'
055000970526     C                   EXSR      F24GES
055100970526     C                   END
055200970526     C                   END
055300970526     C*
055400970526     C                   ENDSR
055500970526     C*------------------------------------------------------------------------*
055600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
055700970526     C*------------------------------------------------------------------------*
055800970526     C     FUNVAR        BEGSR
055900970526     C*
056000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
056100970526     C*
056200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
056300970526     C     KEY           IFEQ      ENTER
056400970526     C                   MOVEL     *BLANKS       FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      F1
056700970526     C                   MOVEL     '01'          FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F2
057000970526     C                   MOVEL     '02'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F3
057300970526     C                   MOVEL     '03'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F4
057600970526     C                   MOVEL     '04'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F5
057900970526     C                   MOVEL     '05'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F6
058200970526     C                   MOVEL     '06'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F7
058500970526     C                   MOVEL     '07'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F8
058800970526     C                   MOVEL     '08'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F9
059100970526     C                   MOVEL     '09'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F10
059400970526     C                   MOVEL     '10'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F11
059700970526     C                   MOVEL     '11'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F12
060000970526     C                   MOVEL     '12'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F13
060300970526     C                   MOVEL     '13'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F14
060600970526     C                   MOVEL     '14'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F15
060900970526     C                   MOVEL     '15'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F16
061200970526     C                   MOVEL     '16'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F17
061500970526     C                   MOVEL     '17'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F18
061800970526     C                   MOVEL     '18'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F19
062100970526     C                   MOVEL     '19'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F20
062400970526     C                   MOVEL     '20'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F21
062700970526     C                   MOVEL     '21'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F22
063000970526     C                   MOVEL     '22'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F23
063300970526     C                   MOVEL     '23'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F24
063600970526     C                   MOVEL     '24'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      ROLLUP
063900970526     C                   MOVEL     'RU'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      HELP
064200970526     C                   MOVEL     'HP'          FUNKEY
064300970526     C                   END
064400970526     C                   ENDSR
064500970526     C*------------------------------------------------------------------------*
064600970526     C* NOTE COSTRUZIONE SCHIERE
064700970526     C*------------------------------------------------------------------------*
064800970526     C*
064900970526     C* -MSG-MESSAGGI
065000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
065100970526     C* (NON TOCCARE MSG,1 E MSG,2)
065200970526     C*
065300970526     C* -CMD-COMANDI
065400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
065500970526     C*
065600970526     C* -OPD-OPZIONI
065700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
065800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
065900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
066000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
066500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
066700970526     C*
066800970526     C* -FUD-FUNZIONI
066900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
067000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
067300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
067500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
067600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
067700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
067800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
067900970526** MSG - MESSAGGI
068000120927Tasto funzionale non gestito                                                   1
068100161104Data errata                                                                    2
068200141216                                                                               3
068300141216                                                                               4
068400130108                                                                               5
068500130108Campo obbligatorio                                                             6
068600130108                                                                               7
068700130108                                                                               8
068800120927                                                                               9
068900120927                                                                               10
069000120927                                                                               11
069100120927                                                                               12
069200120927                                                                               13
069300141027                                                                               14
069400140116                                                                               15
069500120927                                                                               16
069600120927                                                                               17
069700120927                                                                               18
069800120927                                                                               19
069900170426Elaborazione eseguita: controllare l'esito                                     20
070000141027                                                                               21
070100141027                                                                               22
070200121108                                                                               23
070300121108                                                                               24
070400970526** FUD - FUNZIONI
070500970526                   -        (S/N)=N                        funz.01¬
070600970526                   -        (S/N)=N                        funz.02¬
070700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
070800970526                   -        (S/N)=N                        funz.04¬
070900970526                   -        (S/N)=N                        funz.05¬
071000991214F06=Conferma       -        (S/N)=S                        funz.06¬
071100140819                   -        (S/N)=N                        funz.07¬
071200970527                   -        (S/N)=N                        funz.08¬
071300970527                   -        (S/N)=N                        funz.09¬
071400970527                   -        (S/N)=N                        funz.10¬
071500970526                   -        (S/N)=N                        funz.11¬
071600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
071700970526                   -        (S/N)=N                        funz.13¬
071800970526                   -        (S/N)=N                        funz.14¬
071900970526                   -        (S/N)=N                        funz.15¬
072000970526                   -        (S/N)=N                        funz.16¬
072100970526                   -        (S/N)=N                        funz.17¬
072200970526                   -        (S/N)=N                        funz.18¬
072300970526                   -        (S/N)=N                        funz.19¬
072400970526                   -        (S/N)=N                        funz.20¬
072500170426                   -        (S/N)=N                        funz.21¬
072600970526                   -        (S/N)=N                        funz.22¬
072700970526                   -        (S/N)=N                        funz.23¬
072800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
