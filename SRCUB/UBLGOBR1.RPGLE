000100970526     H*------------------------------------------------------------------------*
000200161104     H* Lancio Agg. file TIMAC00F
000300970526     H*------------------------------------------------------------------------*
000400161104     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000500161104     H BNDDIR('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900170202     FUBLGOBD1  cf   e             WORKSTN INFDS(DEVDS1)
001000170202     F                                     SFILE(LGOBDMS:MSGULT)
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
008300170202     D* parametri UBLGOBR
008400141028     D*-------------------
008500170202     D*/copy gaitrasrc/srcProtoPR,UBLGOBR
008600120928     D*-------------------
008700120928
008800121003     d Status         sds
008900121003     d  SDSpgm           *proc
009000121003     d  SDSprm           *parms
009100121003     d  SDSuser              254    263
009200170202
009300130116     d ora             s               t
009400140117     d wX              s              2s 0
009500170202     d $Interatt       s               n   inz(*off)
009600161104     D*------------
009700161104     D* campi tipo data per le date a video
009800161104     D DtDaISO         s               d
009900161104     D DtAlISO         s               d
010000170202     D*------------
010100170202     d D03Lib          ds
010200170202     d  D03LibDim                    10    dim(12)
010300170202     d  D03Lib01                     10    overlay(D03Lib : 01)
010400170202     d  D03Lib02                     10    overlay(D03Lib : 11)
010500170202     d  D03Lib03                     10    overlay(D03Lib : 21)
010600170202     d  D03Lib04                     10    overlay(D03Lib : 31)
010700170202     d  D03Lib05                     10    overlay(D03Lib : 41)
010800170202     d  D03Lib06                     10    overlay(D03Lib : 51)
010900170202     d  D03Lib07                     10    overlay(D03Lib : 61)
011000170202     d  D03Lib08                     10    overlay(D03Lib : 71)
011100170202     d  D03Lib09                     10    overlay(D03Lib : 81)
011200170202     d  D03Lib10                     10    overlay(D03Lib : 91)
011300170202     d  D03Lib11                     10    overlay(D03Lib :101)
011400170202     d  D03Lib12                     10    overlay(D03Lib :111)
011500170306
011600170306      /copy gaitrasrc/srcProtoPR,TIBS02R
011700120928
011800970526     C*------------------------------------------------------------------------*
011900000303     C* MAIN LINES
012000970526     C*------------------------------------------------------------------------*
012100970526     C*
012200120927     C* metto i default su D03
012300120927     C                   EXSR      InzD03
012400120927     C*
012500120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012600991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012900970526     C*
013000970526     C* EMETTE FORMATO VIDEO
013100970526     C                   EXSR      BUTFMR
013200970526     C*
013300970526     C* GESTIONE TASTI FUNZIONALI
013400970526     C                   EXSR      FUNCON
013500970526E   2C                   ENDDO
013600970526     C*
013700970526     C* FINE PGM
013800970526     C                   SETON                                        LR
013900970526     C*------------------------------------------------------------------------*
014000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014100970526     C*------------------------------------------------------------------------*
014200970526     C     BUTFMR        BEGSR
014300970526     C*
014400970526     C* EMETTE LA TESTATA
014500170202     C                   WRITE     LGOBD01
014600970526     C*
014700970526     C* SPEDISCE EVENTUALE MESSAGGIO
014800970526     C                   EXSR      SNDMSG
014900970526     C*
015000970526     C* EMETTE LE FUNZIONI
015100170202     C                   WRITE     LGOBD02
015200970526     C*
015300970526     C* EMETTE IL VIDEO
015400170202     C                   EXFMT     LGOBD03
015500970526     C*
015600991214     C* REIMPOSTA FLAG DI ERRORE
015700991214     C                   MOVEL     '0'           ERRL00
015800970526     C*
015900970526     C* REIMPOSTA INDICATORI DI ERRORE
016000020611     C                   SETOFF                                       555556
016100970526     C*
016200970526     C                   ENDSR
016300970526     C*------------------------------------------------------------------------*
016400120927     C* InzD03 - Inizializzo D03
016500970526     C*------------------------------------------------------------------------*
016600120927     C     InzD03        BEGSR
016700120927     C*
016800170202     C                   clear                   D03Logica
016900170202     C                   clear                   D03DltLg
017000170202     C                   clear                   D03Lib
017100170202        D03ObjT = '*ALL';
017200170202        D03Obj  = '*ALL';
017300130108     C*
017400120927     C                   ENDSR
017500120927     C*------------------------------------------------------------------------*
017600120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017700120927     C*------------------------------------------------------------------------*
017800120927     C     SNDMSG        BEGSR
017900120927     C*
018000970526     C* PULISCE IL SFILE MESSAGGI
018100970526     C                   MOVEL     '1'           *IN40
018200170202     C                   WRITE     LGOBDMC
018300970526     C                   MOVEL     '0'           *IN40
018400970526     C*
018500970526     C* VISUALIZZA IL MESSAGGIO
018600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018700970526     C                   CALL      'XMSG'        XMSGPA
018800020611     C     DSMSER        IFEQ      *ZEROS
018900970526     C                   MOVEL     DSMSKE        MSGKEY
019000020611     C                   ENDIF
019100970526     C                   Z-ADD     1             MSGULT
019200170202     C                   WRITE     LGOBDMS
019300170202     C                   WRITE     LGOBDMC
019400970526     C*
019500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019600970526     C                   MOVEL     *BLANKS       DSMSMS
019700970526     C*
019800970526     C                   ENDSR
019900970526     C*------------------------------------------------------------------------*
020000970526     C* ENTGES - GESTIONE ENTER -> INVIO
020100970526     C*------------------------------------------------------------------------*
020200970526     C     ENTGES        BEGSR
020300970526     C*
020400970526     C* CONTROLLA I CAMPI IMMESSI
020500970526     C                   EXSR      DECODI
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* F03GES - GESTIONE F03 -> FINE
021000970526     C*------------------------------------------------------------------------*
021100970526     C     F03GES        BEGSR
021200970526     C*
021300970526     C* IMPOSTA "PREMUTO F03"
021400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021500970526     C*
021600970526     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800991214     C* F06GES - GESTIONE F06 -> CONFERMA
021900970526     C*------------------------------------------------------------------------*
022000970526     C     F06GES        BEGSR
022100970526     C*
022200991214     C* CONTROLLA I CAMPI IMMESSI
022300991214     C                   EXSR      DECODI
022400020611     C*
022500991214     C* ESEGUE ROUTINE DI CONFERMA
022600020611     C     ERRL00        IFEQ      '0'
022700991214     C                   EXSR      ESEGUI
022800170306     C* messaggio di messa in coda del lavoro
022900121109     C                   MOVEL     MSG(20)       DSMSMS
023000121109     C* non esco mai dal pgm
023100121109     C***                MOVEL     '1'           FINL00
023200121108     C                   ENDIF
023300970526     C*
023400991214     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600130109     C* F07GES - GESTIONE F07 -> STAMPA
023700970526     C*------------------------------------------------------------------------*
023800130109     C     F07GES        BEGSR
023900970526     C*
024000140818     C                   EXSR      F06GES
024100130110     C*
024200970526     C                   ENDSR
024300130109     C*------------------------------------------------------------------------*
024400130109     C* F12GES - GESTIONE F12 -> RITORNO
024500130109     C*------------------------------------------------------------------------*
024600130109     C     F12GES        BEGSR
024700130109     C*
024800130109     C* IMPOSTA "PREMUTO F12"
024900130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025000130109     C*
025100130109     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025400970526     C*------------------------------------------------------------------------*
025500970526     C     F24GES        BEGSR
025600970526     C*
025700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025800970526     C*
025900970526     C                   ENDSR
026000170202     C*------------------------------------------------------------------------*
026100170202     C* F21GES - GESTIONE F21 -> BCH09
026200170202     C*------------------------------------------------------------------------*
026300170202     C     F21GES        BEGSR
026400170202     C*
026500170202     C                   MOVE      'GOB2'        KCOAZ
026600170202     C                   CALL      'BCH09'
026700170202     C                   PARM                    KPJBA
026800170202     C*
026900170202     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
027200970526     C*------------------------------------------------------------------------*
027300991214     C     ESEGUI        BEGSR
027400161111     C*
027500161111     C                   CLEAR                   kpjbu
027600141027     C*
027700161111     C* opzione
027800170202     C                   EVAL      %subst(kpjbu : 01 :2) = D03Logica
027900170202     C                   EVAL      %subst(kpjbu : 03 :1) = D03DltLg
028000170202     C                   EVAL      %subst(kpjbu : 04 :120) = D03Lib
028100170202     C                   EVAL      %subst(kpjbu :124 :7) = D03ObjT
028200170202     C                   EVAL      %subst(kpjbu :131 :10) = D03Obj
028300170504     C                   EVAL      %subst(kpjbu :141 :10) = D03UsrCrt
028400141027     C*
028500161110     C* chiamo per creare TIMAC
028600170202     C                   MOVE      'GOB2'        KCOAZ
028700170202     C                   IF        $interatt = *off
028800170202     C                   CALL      'BCH10'
028900170202     C                   PARM                    KPJBA
029000170202     c                   else
029100170202     C                   CALL      'UBLGOBR2'
029200170202     C                   PARM                    KPJBA
029300170202     C                   ENDIF
029400140120     C*
029500970526     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029800970526     C*------------------------------------------------------------------------*
029900970526     C     DECODI        BEGSR
030000121019     C*
030100120927     C                   SETOFF                                       505152
030200140120     c                   EVAL      VIDMSG    = *blank
030300141216     C*
030400170202     C* dati obbligatori
030500170202     C*  logica
030600170202        if D03Logica = *blank;
030700170202     C                   MOVEL     '1'           ERRL00
030800170202     C                   SETON                                        50
030900170202     C                   MOVEL     MSG(6)        DSMSMS
031000170202     C                   LEAVESR
031100170202        endif;
031200170323
031300170323        // recupero dati da tab. VPR  a seconda della logica
031400170323        // lista librerie (se non già valorizzata)
031500170323        if D03Lib = *blank;
031600170323          clear kpjbu;
031700170323          //T02mod = 'C';              (già così)?
031800170323          //T02cod = 'VPR';            (già così)?
031900170323          // la lista librerie la reperisco a seconda della logica
032000170323          select;
032100170323           when D03Logica = '01';
032200170323            T02ke1 = 'LIBLIST_LOG01';
032300170323           when D03Logica = '02';
032400170323            T02ke1 = 'LIBLIST_LOG02';
032500170323           other;
032600170323            T02ke1 = *blank         ;
032700170323          endsl;
032800170323          //T02sif = knsif;            (non serve perché quasta è una tab. solo sede)
032900170323          TNTBE_RicercaControllo ( kpjba : tibs02ds );
033000170323          if T02err = *blank;
033100170323            D03Lib = %subst(T02uni:1:120);
033200170323          else;
033300170323            D03Lib = *blank;
033400170323          endif;
033500170323        endif;
033600170202     C*
033700970528     C                   ENDSR
033800970526     C*------------------------------------------------------------------------*
033900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
034000970526     C*------------------------------------------------------------------------*
034100970526     C     AZZVID        BEGSR
034200141027     C*
034300170202     C                   clear                   D03Logica
034400170202     C                   clear                   D03Lib
034500170202     C                   clear                   D03DltLg
034600970526     C*
034700970526     C                   ENDSR
034800970526     C*------------------------------------------------------------------------*
034900020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
035000970526     C*------------------------------------------------------------------------*
035100970526     C     IMPFUN        BEGSR
035200970526     C*
035300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
035400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
035500970526     C                   EXSR      FUNGES
035600970526     C*
035700970526     C                   ENDSR
035800020923     C*--------------------------------------------------------------------------------------------*
035900020923     C* REPERISCE I DATI UTENTE
036000020923     C*--------------------------------------------------------------------------------------------*
036100020923     C     REPDATIUTE    BEGSR
036200020923     C*
036300020923     C* INIZIALIZZA VARIABILI DI WRK
036400020923     C                   CLEAR                   TIBS34DS
036500020923     C                   CLEAR                   AZUTEDS
036600020923     C                   CLEAR                   DDATIUTE
036700020923     C*
036800020923     C     *DTAARA       DEFINE    §azute        azuteds
036900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
037000020923     C                   IN(E)     *DTAARA
037100020923if  1C                   IF        %Error
037200020923     c                   EVAL      I34Tla = 'L'
037300020923     C                   CALL      'TIBS34R'
037400020923     C                   PARM                    Tibs34Ds
037500020923     C                   IN        *DTAARA
037600020923e   1C                   ENDIF
037700020923     C*
037800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
037900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
038000020923     C*
038100020923     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* *INZSR - OPERAZIONI INIZIALI
038400970526     C*------------------------------------------------------------------------*
038500970526     C     *INZSR        BEGSR
038600991217     C*
038700991217     C     *ENTRY        PLIST
038800991217     C                   PARM                    KPJBA
038900020923     C*
039000020923     C* REPERISCE I DATI DELL'UTENTE
039100020923     C                   EXSR      REPDATIUTE
039200970526     C*
039300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
039400970526     C     XMSGPA        PLIST
039500970526     C                   PARM                    DSMSMS
039600970526     C                   PARM                    DSMSPG
039700970526     C                   PARM                    DSMSKE
039800970526     C                   PARM                    DSMSER
039900970526     C*
040000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
040100970526     C                   CALL      'XKEY'
040200970526     C                   PARM                    DSKEY
040300970526     C*---
040400970526     C* VARIABILI NON RIFERITE AL DATA BASE
040500970526     C*---
040600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
040700991214     C                   MOVEL     *BLANKS       DEPOP1            3
040800970526     C*---
040900970526     C* VARIABILI DI CONTROLLO
041000970526     C*---
041100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
041200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
041300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
041400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
041500970526     C*
041600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
041700170202     C                   MOVEL     'UBLGOBR1'    DSMSPG
041800170202     C                   MOVEL     'UBLGOBR1'    MSGQUE
041900970526     C                   MOVEL     *BLANKS       MSGKEY
042000970526     C                   MOVEL     *BLANKS       DSMSMS
042100970528     C*
042200970528     C* CALCOLA LA DATA CORRENTE
042300970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
042400970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
042500970528     C                   Z-ADD     WN8           G08DAT
042600970528     C                   Z-ADD     *ZEROS        G08INV
042700970528     C                   MOVEL     '0'           G08ERR
042800970528     C                   CALL      'XSRDA8'
042900970528     C                   PARM                    WLBDA8
043000970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043100000310     C*
043200000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
043300000310     C                   EXSR      AZZVID
043400970526     C*
043500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
043600970526     C                   EXSR      IMPFUN
043700970526     C*
043800970526     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
044100970526     C*------------------------------------------------------------------------*
044200970526     C     FUNGES        BEGSR
044300970526     C*
044400970526     C* PULISCO CAMPI DI LAVORO
044500970526     C                   Z-ADD     0             W                 5 0
044600970526     C*
044700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
044800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
044900970526     C     1             DO        24            I
045000970526     C                   MOVEL     FUD(I)        FUDRIG
045100970526     C*
045200970526     C* PULIZIA FUNZIONI PRECEDENTI
045300970526     C                   MOVEL     *BLANKS       FUV(I)
045400970526     C*
045500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
045600970526     C* (SALTA F24)
045700970526     C     FUDATT        IFEQ      'S'
045800970526     C     FUDFUN        ANDNE     '24'
045900970526     C                   ADD       1             W
046000970526     C                   MOVEL     FUDDES        FUV(W)
046100970526     C                   END
046200970526     C                   END                                                    DO WHILE
046300970526     C*
046400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
046500970526     C* ALTRIMENTI DISATTIVO F24
046600970526     C     1             DO        24            I
046700970526     C                   MOVEL     FUD(I)        FUDRIG
046800970526     C*
046900970526     C     FUDFUN        IFEQ      '24'
047000970526     C     W             IFGT      6
047100970526     C                   MOVEL     'S'           FUDATT
047200970526     C                   ELSE
047300970526     C                   MOVEL     *BLANKS       FUDATT
047400970526     C                   END                                                    W GT 8
047500970526     C                   MOVEL     FUDRIG        FUD(I)
047600970526     C                   END
047700970526     C                   END                                                    DO WHILE
047800970526     C*
047900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
048000970526     C     W             DIV       6             I
048100970526     C                   MVR                     Z
048200970526     C*
048300970526     C     Z             IFNE      0
048400970526     C     I             ADD       1             Z
048500970526     C                   ELSE
048600970526     C                   Z-ADD     I             Z                 5 0
048700970526     C                   END
048800970526     C*
048900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
049000970526     C                   ADD       1             FUNGRU
049100970526     C     FUNGRU        IFGT      Z
049200970526     C     FUNGRU        ORLT      1
049300970526     C                   Z-ADD     1             FUNGRU
049400970526     C                   END
049500970526     C*
049600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
049700970526     C     FUNGRU        IFEQ      1
049800970526     C                   MOVEL     FUV(1)        FUN001
049900970526     C                   MOVEL     FUV(2)        FUN002
050000970526     C                   MOVEL     FUV(3)        FUN003
050100970526     C                   MOVEL     FUV(4)        FUN004
050200970526     C                   MOVEL     FUV(5)        FUN005
050300970526     C                   MOVEL     FUV(6)        FUN006
050400970526     C                   END
050500970526     C*
050600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
050700970526     C     FUNGRU        IFEQ      2
050800970526     C                   MOVEL     FUV(7)        FUN001
050900970526     C                   MOVEL     FUV(8)        FUN002
051000970526     C                   MOVEL     FUV(9)        FUN003
051100970526     C                   MOVEL     FUV(10)       FUN004
051200970526     C                   MOVEL     FUV(11)       FUN005
051300970526     C                   MOVEL     FUV(12)       FUN006
051400970526     C                   END
051500970526     C*
051600970526     C* TERZO GRUPPO DI 6 FUNZIONI
051700970526     C     FUNGRU        IFEQ      3
051800970526     C                   MOVEL     FUV(13)       FUN001
051900970526     C                   MOVEL     FUV(14)       FUN002
052000970526     C                   MOVEL     FUV(15)       FUN003
052100970526     C                   MOVEL     FUV(16)       FUN004
052200970526     C                   MOVEL     FUV(17)       FUN005
052300970526     C                   MOVEL     FUV(18)       FUN006
052400970526     C                   END
052500970526     C*
052600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
052700970526     C     FUNGRU        IFEQ      4
052800970526     C                   MOVEL     FUV(19)       FUN001
052900970526     C                   MOVEL     FUV(20)       FUN002
053000970526     C                   MOVEL     FUV(21)       FUN003
053100970526     C                   MOVEL     FUV(22)       FUN004
053200970526     C                   MOVEL     *BLANKS       FUN005
053300970526     C                   MOVEL     *BLANKS       FUN006
053400970526     C                   END
053500970526     C*
053600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
053700970526     C                   MOVEL     *BLANKS       FUN007
053800970526     C                   MOVEL     *BLANKS       FUN008
053900970526     C*
054000970526     C     1             DO        24            I
054100970526     C                   MOVEL     FUD(I)        FUDRIG
054200970526     C     FUDFUN        IFEQ      '24'
054300970526     C     FUDATT        ANDEQ     'S'
054400970526     C                   MOVEL     FUDDES        FUN008
054500970526     C                   END
054600970526     C                   END                                                    DO
054700970526     C*
054800970526     C                   ENDSR
054900970526     C*------------------------------------------------------------------------*
055000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
055100970526     C*------------------------------------------------------------------------*
055200970526     C     FUNCON        BEGSR
055300970526     C*
055400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
055500970526     C                   EXSR      FUNVAR
055600970526     C*
055700970526     C* PULISCO CAMPI DI LAVORO
055800970526     C                   Z-ADD     0             I                 5 0
055900970526     C                   Z-ADD     0             W                 5 0
056000970526     C*
056100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
056200970526     C     FUNKEY        IFNE      *BLANKS
056300970526     C     1             DO        24            I
056400970526     C                   MOVEL     FUD(I)        FUDRIG
056500970526     C*
056600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
056700970526     C     FUNKEY        IFEQ      FUDFUN
056800970526     C     FUDATT        ANDEQ     'S'
056900970526     C                   Z-ADD     I             W
057000970526     C                   END
057100970526     C                   END                                                    DO WHILE
057200970526     C*
057300970526     C* FUNZIONE NON ATTIVA (ERRORE)
057400970526     C     W             IFEQ      0
057500970526     C*
057600970526     C* EMISSIONE MESSAGGIO DI ERRORE
057700970526     C                   MOVEL     MSG(1)        DSMSMS
057800970526     C*
057900970526     C                   END                                                    W=0
058000970526     C                   END                                                    FUNKEY NE *BLANKS
058100970526     C*
058200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
058300970526     C     W             IFNE      0
058400970526     C     FUNKEY        OREQ      *BLANKS
058500970526     C     FUNKEY        IFEQ      *BLANKS
058600970526     C                   EXSR      ENTGES
058700970526     C                   END
058800970526     C     FUNKEY        IFEQ      '03'
058900970526     C                   EXSR      F03GES
059000970526     C                   END
059100970526     C     FUNKEY        IFEQ      '06'
059200970526     C                   EXSR      F06GES
059300970526     C                   END
059400130109     C     FUNKEY        IFEQ      '07'
059500130109     C                   EXSR      F07GES
059600130109     C                   END
059700970526     C     FUNKEY        IFEQ      '12'
059800970526     C                   EXSR      F12GES
059900970526     C                   END
060000170202     C     FUNKEY        IFEQ      '21'
060100170202     C                   EXSR      F21GES
060200170202     C                   END
060300970526     C     FUNKEY        IFEQ      '24'
060400970526     C                   EXSR      F24GES
060500970526     C                   END
060600970526     C                   END
060700970526     C*
060800970526     C                   ENDSR
060900970526     C*------------------------------------------------------------------------*
061000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
061100970526     C*------------------------------------------------------------------------*
061200970526     C     FUNVAR        BEGSR
061300970526     C*
061400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
061500970526     C*
061600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
061700970526     C     KEY           IFEQ      ENTER
061800970526     C                   MOVEL     *BLANKS       FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F1
062100970526     C                   MOVEL     '01'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F2
062400970526     C                   MOVEL     '02'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F3
062700970526     C                   MOVEL     '03'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F4
063000970526     C                   MOVEL     '04'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F5
063300970526     C                   MOVEL     '05'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F6
063600970526     C                   MOVEL     '06'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F7
063900970526     C                   MOVEL     '07'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F8
064200970526     C                   MOVEL     '08'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F9
064500970526     C                   MOVEL     '09'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F10
064800970526     C                   MOVEL     '10'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F11
065100970526     C                   MOVEL     '11'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F12
065400970526     C                   MOVEL     '12'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F13
065700970526     C                   MOVEL     '13'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F14
066000970526     C                   MOVEL     '14'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F15
066300970526     C                   MOVEL     '15'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F16
066600970526     C                   MOVEL     '16'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F17
066900970526     C                   MOVEL     '17'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F18
067200970526     C                   MOVEL     '18'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F19
067500970526     C                   MOVEL     '19'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F20
067800970526     C                   MOVEL     '20'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F21
068100970526     C                   MOVEL     '21'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F22
068400970526     C                   MOVEL     '22'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      F23
068700970526     C                   MOVEL     '23'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      F24
069000970526     C                   MOVEL     '24'          FUNKEY
069100970526     C                   END
069200970526     C     KEY           IFEQ      ROLLUP
069300970526     C                   MOVEL     'RU'          FUNKEY
069400970526     C                   END
069500970526     C     KEY           IFEQ      HELP
069600970526     C                   MOVEL     'HP'          FUNKEY
069700970526     C                   END
069800970526     C                   ENDSR
069900970526     C*------------------------------------------------------------------------*
070000970526     C* NOTE COSTRUZIONE SCHIERE
070100970526     C*------------------------------------------------------------------------*
070200970526     C*
070300970526     C* -MSG-MESSAGGI
070400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
070500970526     C* (NON TOCCARE MSG,1 E MSG,2)
070600970526     C*
070700970526     C* -CMD-COMANDI
070800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
070900970526     C*
071000970526     C* -OPD-OPZIONI
071100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
071200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
071300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
071400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
071500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
071600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
071700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
071900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
072000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
072100970526     C*
072200970526     C* -FUD-FUNZIONI
072300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
072400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
072500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
072600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
072700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
072800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
072900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
073000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
073100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
073200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
073300970526** MSG - MESSAGGI
073400120927Tasto funzionale non gestito                                                   1
073500161104Data errata                                                                    2
073600141216                                                                               3
073700141216                                                                               4
073800130108                                                                               5
073900130108Campo obbligatorio                                                             6
074000130108                                                                               7
074100130108                                                                               8
074200120927                                                                               9
074300120927                                                                               10
074400120927                                                                               11
074500120927                                                                               12
074600120927                                                                               13
074700141027                                                                               14
074800140116                                                                               15
074900120927                                                                               16
075000120927                                                                               17
075100120927                                                                               18
075200120927                                                                               19
075300170306Job sottomesso                                                                 20
075400141027                                                                               21
075500141027                                                                               22
075600121108                                                                               23
075700121108                                                                               24
075800970526** FUD - FUNZIONI
075900970526                   -        (S/N)=N                        funz.01¬
076000970526                   -        (S/N)=N                        funz.02¬
076100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
076200970526                   -        (S/N)=N                        funz.04¬
076300970526                   -        (S/N)=N                        funz.05¬
076400991214F06=Conferma       -        (S/N)=S                        funz.06¬
076500140819                   -        (S/N)=N                        funz.07¬
076600970527                   -        (S/N)=N                        funz.08¬
076700970527                   -        (S/N)=N                        funz.09¬
076800970527                   -        (S/N)=N                        funz.10¬
076900970526                   -        (S/N)=N                        funz.11¬
077000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
077100970526                   -        (S/N)=N                        funz.13¬
077200970526                   -        (S/N)=N                        funz.14¬
077300970526                   -        (S/N)=N                        funz.15¬
077400970526                   -        (S/N)=N                        funz.16¬
077500970526                   -        (S/N)=N                        funz.17¬
077600970526                   -        (S/N)=N                        funz.18¬
077700970526                   -        (S/N)=N                        funz.19¬
077800970526                   -        (S/N)=N                        funz.20¬
077900170202F21=Param.batch    -        (S/N)=S                        funz.21¬
078000970526                   -        (S/N)=N                        funz.22¬
078100970526                   -        (S/N)=N                        funz.23¬
078200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
