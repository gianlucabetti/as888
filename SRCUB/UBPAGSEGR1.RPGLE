000100150904     h dftactgrp(*no) actgrp('QILE') bnddir('UBRTVNETA')
000200970526     H*------------------------------------------------------------------------*
000300150904     H* Lancio Stampa Logo in PAGSEG
000400970526     H*------------------------------------------------------------------------*
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900150904     fTNTBE01L  if   e           k disk    extfile(wLibFile)  usropn
001000150904     FUBPAGSEGD1cf   e             WORKSTN INFDS(DEVDS1)
001100150904     F                                     SFILE(PAGSEGDMS:MSGULT)
001200150904       // -?Printer File?
001300150904     fUBPAGSEGP o    e             printer usropn
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D* MESSAGGI
001800970526     D*------------
001900000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* DESCRIZIONI TASTI FUNZIONALI
002200970526     D*------------
002300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* FUNZIONI DA CARICARE A VIDEO
002600970526     D*------------
002700970526     D FUV             S             19    DIM(24)
002800970526     D*------------------------------------------------------------------------*
002900970526     D* INPUT
003000970526     D*------------------------------------------------------------------------*
003100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003200970526     D*------------
003300970526     D FUDDS           DS
003400970526     D  FUDDE1                 1      1
003500970526     D  FUDFUN                 2      3
003600970526     D  FUDDE2                 4     19
003700970526     D  FUDFI0                20     34
003800970526     D  FUDATT                35     35
003900970526     D  FUDFI1                36     80
004000970526     D  FUDDES                 1     19
004100970526     D  FUDRIG                 1     80
004200970526     D*------------
004300970526     D* DS GESTIONE TASTI FUNZIONALI
004400970526     D*------------
004500970526     D DSKEY         E DS
004600970526     D DEVDS1          DS
004700970526     D  KEY                  369    369
004800970526     D*------------
004900970526     D* DS "XMSG" - GESTIONE MESSAGGI
005000970526     D*------------
005100970526     D DSMSG         E DS
005200970528     D*------------------
005300970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400970528     D*------------------
005500970528     D WLBDA8          DS                  INZ
005600970528     D  G08DAT                 1      8  0
005700970528     D  G08INV                 9     16  0
005800970528     D  G08ERR                17     17
005900970528     D  G08TGI                18     22  0
006000970526     D*------------
006100970526     D* ARCHITETTURA
006200970526     D*------------
006300970526     D KPJBA         E DS
006400020923     D*-------------------
006500020923     D* DS REPERIMENTO DATI UTENTE
006600020923     D*-------------------
006700020923     D TIBS34DS      E DS                                                       *Profili utente
006800020923     D DDATIUTE      E DS                                                       *Dati utente
006900020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000120927     D* DS REPERIMENTO PADRE/FIGLI
007100120927     D TIBS10DS      E DS
007200150904     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
007300150904     d                                     prefix(k_)  inz
007400150904     D DAR8          E DS                  qualified inz
007500141027     D WLBDAT          DS
007600141027     D  G02DAT                 1      8  0
007700141027     D  G02INV                 9     16  0
007800141027     D  G02ERR                17     17
007900141027     D  G02TGI                18     22  0
008000141027     D wDataDa         s              8s 0
008100141027     D wDataA          s              8s 0
008200150904       // -?Nome della libreria in cui è il file?
008300150904     d wLibFile        s             21a   inz
008400120928     D*-------------------
008500120928
008600121003     d Status         sds
008700121003     d  SDSpgm           *proc
008800121003     d  SDSprm           *parms
008900121003     d  SDSuser              254    263
009000130116
009100130116     d ora             s               t
009200140117     d wX              s              2s 0
009300141217     d $Debug          s              1a
009400150904       // -?Valori di ritorno?
009500150904     d rtnEsito        s             10i 0 inz
009600150904       // -?Nome del sistema?
009700150904     d currSysNeta     s              8a   inz
009800150904       // -?Reperimento NETA sistema AS/400 corrente?
009900150904      /copy gaitrasrc/srcProtoPR,UBRTVNETA
010000150904       // -?Errori gestiti?
010100150904     d c_ErrRepCurrSysNetA...
010200150904     d                 c                   const(-1)
010300120928
010400970526     C*------------------------------------------------------------------------*
010500000303     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800120927     C* metto i default su D03
010900120927     C                   EXSR      InzD03
011000120927     C*
011100120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011200991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011500970526     C*
011600970526     C* EMETTE FORMATO VIDEO
011700970526     C                   EXSR      BUTFMR
011800970526     C*
011900970526     C* GESTIONE TASTI FUNZIONALI
012000970526     C                   EXSR      FUNCON
012100970526E   2C                   ENDDO
012200970526     C*
012300970526     C* FINE PGM
012400150904     C*
012500150904     C                   if        %open(UBPAGSEGP)
012600150904     C                   close     UBPAGSEGP
012700150904     C                   endif
012800970526     C                   SETON                                        LR
012900970526     C*------------------------------------------------------------------------*
013000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013100970526     C*------------------------------------------------------------------------*
013200970526     C     BUTFMR        BEGSR
013300970526     C*
013400970526     C* EMETTE LA TESTATA
013500150904     C                   WRITE     PAGSEGD01
013600970526     C*
013700970526     C* SPEDISCE EVENTUALE MESSAGGIO
013800970526     C                   EXSR      SNDMSG
013900970526     C*
014000970526     C* EMETTE LE FUNZIONI
014100150904     C                   WRITE     PAGSEGD02
014200970526     C*
014300970526     C* EMETTE IL VIDEO
014400150904     C                   EXFMT     PAGSEGD03
014500970526     C*
014600991214     C* REIMPOSTA FLAG DI ERRORE
014700991214     C                   MOVEL     '0'           ERRL00
014800970526     C*
014900970526     C* REIMPOSTA INDICATORI DI ERRORE
015000020611     C                   SETOFF                                       555556
015100970526     C*
015200970526     C                   ENDSR
015300970526     C*------------------------------------------------------------------------*
015400120927     C* InzD03 - Inizializzo D03
015500970526     C*------------------------------------------------------------------------*
015600120927     C     InzD03        BEGSR
015700120927     C*
015800130108     C*
015900120927     C                   ENDSR
016000120927     C*------------------------------------------------------------------------*
016100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016200120927     C*------------------------------------------------------------------------*
016300120927     C     SNDMSG        BEGSR
016400120927     C*
016500970526     C* PULISCE IL SFILE MESSAGGI
016600970526     C                   MOVEL     '1'           *IN40
016700150904     C                   WRITE     PAGSEGDMC
016800970526     C                   MOVEL     '0'           *IN40
016900970526     C*
017000970526     C* VISUALIZZA IL MESSAGGIO
017100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017200970526     C                   CALL      'XMSG'        XMSGPA
017300020611     C     DSMSER        IFEQ      *ZEROS
017400970526     C                   MOVEL     DSMSKE        MSGKEY
017500020611     C                   ENDIF
017600970526     C                   Z-ADD     1             MSGULT
017700150904     C                   WRITE     PAGSEGDMS
017800150904     C                   WRITE     PAGSEGDMC
017900970526     C*
018000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018100970526     C                   MOVEL     *BLANKS       DSMSMS
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* ENTGES - GESTIONE ENTER -> INVIO
018600970526     C*------------------------------------------------------------------------*
018700970526     C     ENTGES        BEGSR
018800970526     C*
018900970526     C* CONTROLLA I CAMPI IMMESSI
019000970526     C                   EXSR      DECODI
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* F03GES - GESTIONE F03 -> FINE
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F03GES        BEGSR
019700970526     C*
019800970526     C* IMPOSTA "PREMUTO F03"
019900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020000970526     C*
020100970526     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300991214     C* F06GES - GESTIONE F06 -> CONFERMA
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F06GES        BEGSR
020600970526     C*
020700991214     C* CONTROLLA I CAMPI IMMESSI
020800991214     C                   EXSR      DECODI
020900020611     C*
021000991214     C* ESEGUE ROUTINE DI CONFERMA
021100020611     C     ERRL00        IFEQ      '0'
021200991214     C                   EXSR      ESEGUI
021300130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
021400121109     C                   MOVEL     MSG(20)       DSMSMS
021500150904     C*     esco     dal pgm
021600150904     C                   MOVEL     '1'           FINL00
021700121108     C                   ENDIF
021800970526     C*
021900991214     C                   ENDSR
022000130109     C*------------------------------------------------------------------------*
022100130109     C* F12GES - GESTIONE F12 -> RITORNO
022200130109     C*------------------------------------------------------------------------*
022300130109     C     F12GES        BEGSR
022400130109     C*
022500130109     C* IMPOSTA "PREMUTO F12"
022600130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022700130109     C*
022800130109     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023100970526     C*------------------------------------------------------------------------*
023200970526     C     F24GES        BEGSR
023300970526     C*
023400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
023900970526     C*------------------------------------------------------------------------*
024000991214     C     ESEGUI        BEGSR
024100141027     C*
024200150904     C                   if        not %open(UBPAGSEGP)
024300150904     C                   open      UBPAGSEGP
024400150904     C                   endif
024500150904     C                   EVAL      P0LogoF = D03LogoF
024600150904     C                   EVAL      §Logo = 'LOGO' + D03LogoF
024700150904     C                   EVAL      P0Des   = D03Des
024800150904     C                   WRITE     PAGSEGP0
024900150904     C                   WRITE     PAGSEGP1
025000140120     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025400970526     C*------------------------------------------------------------------------*
025500970526     C     DECODI        BEGSR
025600121019     C*
025700120927     C                   SETOFF                                       505152
025800140120     c                   EVAL      VIDMSG    = *blank
025900120927     C*
026000150904     C* il cod. è obbligatorio
026100141216     C*
026200141216     C     ERRL00        IFEQ      '0'
026300150904     C                   IF        D03LogoF = *blank
026400141216     C                   MOVEL     '1'           ERRL00
026500141216     C                   SETON                                        50
026600141216     C                   MOVEL     MSG(2)        DSMSMS
026700141216     C                   ENDIF
026800141216     C                   ENDIF
026900141216     C*
027000150904     C     ERRL00        IFEQ      '0'
027100150904
027200150904      /free
027300150904
027400150904           k_TBEcod = 'AR8';
027500150904           k_TBEke1 = D03LogoF;
027600150904           clear  k_TBEke2;
027700150904           clear  k_TBElin;
027800150904           clear  k_TBEsif;
027900150904           setll  %kds( k05tntbe01 )  TNTBE000;
028000150904           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
028100150904           //se c'è, ce n'è 1
028200150904           If Not %eof(TNTBE01L);
028300150904             dAR8 = TBEuni;
028400150904             D03Des = dAR8.§AR8Des;
028500150904           Else;
028600150904               ErrL00 = '1';
028700150904               *in51 = *on;
028800150904               DSMSMS = MSG(3);
028900150904           EndIf;
029000150904
029100150904      /end-free
029200150904
029300150904     C                   ENDIF
029400150904     C*
029500970528     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
029800970526     C*------------------------------------------------------------------------*
029900970526     C     AZZVID        BEGSR
030000141027     C*
030100150904     C                   EVAL      D03LogoF = *blank
030200150904     C                   EVAL      D03Des   = *blank
030300970526     C*
030400970526     C                   ENDSR
030500970526     C*------------------------------------------------------------------------*
030600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
030700970526     C*------------------------------------------------------------------------*
030800970526     C     IMPFUN        BEGSR
030900970526     C*
031000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
031100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
031200970526     C                   EXSR      FUNGES
031300970526     C*
031400970526     C                   ENDSR
031500020923     C*--------------------------------------------------------------------------------------------*
031600020923     C* REPERISCE I DATI UTENTE
031700020923     C*--------------------------------------------------------------------------------------------*
031800020923     C     REPDATIUTE    BEGSR
031900020923     C*
032000020923     C* INIZIALIZZA VARIABILI DI WRK
032100020923     C                   CLEAR                   TIBS34DS
032200020923     C                   CLEAR                   AZUTEDS
032300020923     C                   CLEAR                   DDATIUTE
032400020923     C*
032500020923     C     *DTAARA       DEFINE    §azute        azuteds
032600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
032700020923     C                   IN(E)     *DTAARA
032800020923if  1C                   IF        %Error
032900020923     c                   EVAL      I34Tla = 'L'
033000020923     C                   CALL      'TIBS34R'
033100020923     C                   PARM                    Tibs34Ds
033200020923     C                   IN        *DTAARA
033300020923e   1C                   ENDIF
033400020923     C*
033500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
033700020923     C*
033800020923     C                   ENDSR
033900970526     C*------------------------------------------------------------------------*
034000970526     C* *INZSR - OPERAZIONI INIZIALI
034100970526     C*------------------------------------------------------------------------*
034200970526     C     *INZSR        BEGSR
034300991217     C*
034400991217     C     *ENTRY        PLIST
034500991217     C                   PARM                    KPJBA
034600020923     C*
034700020923     C* REPERISCE I DATI DELL'UTENTE
034800020923     C                   EXSR      REPDATIUTE
034900970526     C*
035000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
035100970526     C     XMSGPA        PLIST
035200970526     C                   PARM                    DSMSMS
035300970526     C                   PARM                    DSMSPG
035400970526     C                   PARM                    DSMSKE
035500970526     C                   PARM                    DSMSER
035600150904     C
035700150904
035800150904      /free
035900150904
036000150904       // -?Verifica del sistema AS/400 corrente?
036100150904       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
036200150904         //rtnEsito = c_ErrRepCurrSysNetA;
036300150904         //return  rtnEsito;
036400150904         *inlr = *on;
036500150904         return;
036600150904       endif;
036700150904
036800150904       // -?Apertura archivi?
036900150904       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
037000150904         wLibFile = 'GAITRAGRU/TNTBE01L';
037100150904       else;
037200150904         wLibFile = 'GAITRAGRPS/TNTBE01L';
037300150904       endif;
037400150904
037500150904       if  Not %open(TNTBE01L);
037600150904         open  TNTBE01L;
037700150904       endif;
037800150904
037900150904      /end-free
038000150904
038100970526     C*
038200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
038300970526     C                   CALL      'XKEY'
038400970526     C                   PARM                    DSKEY
038500970526     C*---
038600970526     C* VARIABILI NON RIFERITE AL DATA BASE
038700970526     C*---
038800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
038900991214     C                   MOVEL     *BLANKS       DEPOP1            3
039000970526     C*---
039100970526     C* VARIABILI DI CONTROLLO
039200970526     C*---
039300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
039400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
039500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
039600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
039700970526     C*
039800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
039900150904     C                   MOVEL     'UBPAGSEGR1'  DSMSPG
040000150904     C                   MOVEL     'UBPAGSEGR1'  MSGQUE
040100970526     C                   MOVEL     *BLANKS       MSGKEY
040200970526     C                   MOVEL     *BLANKS       DSMSMS
040300970528     C*
040400970528     C* CALCOLA LA DATA CORRENTE
040500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
040600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
040700970528     C                   Z-ADD     WN8           G08DAT
040800970528     C                   Z-ADD     *ZEROS        G08INV
040900970528     C                   MOVEL     '0'           G08ERR
041000970528     C                   CALL      'XSRDA8'
041100970528     C                   PARM                    WLBDA8
041200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
041300000310     C*
041400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
041500000310     C                   EXSR      AZZVID
041600970526     C*
041700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
041800970526     C                   EXSR      IMPFUN
041900970526     C*
042000970526     C                   ENDSR
042100970526     C*------------------------------------------------------------------------*
042200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
042300970526     C*------------------------------------------------------------------------*
042400970526     C     FUNGES        BEGSR
042500970526     C*
042600970526     C* PULISCO CAMPI DI LAVORO
042700970526     C                   Z-ADD     0             W                 5 0
042800970526     C*
042900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
043000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
043100970526     C     1             DO        24            I
043200970526     C                   MOVEL     FUD(I)        FUDRIG
043300970526     C*
043400970526     C* PULIZIA FUNZIONI PRECEDENTI
043500970526     C                   MOVEL     *BLANKS       FUV(I)
043600970526     C*
043700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
043800970526     C* (SALTA F24)
043900970526     C     FUDATT        IFEQ      'S'
044000970526     C     FUDFUN        ANDNE     '24'
044100970526     C                   ADD       1             W
044200970526     C                   MOVEL     FUDDES        FUV(W)
044300970526     C                   END
044400970526     C                   END                                                    DO WHILE
044500970526     C*
044600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
044700970526     C* ALTRIMENTI DISATTIVO F24
044800970526     C     1             DO        24            I
044900970526     C                   MOVEL     FUD(I)        FUDRIG
045000970526     C*
045100970526     C     FUDFUN        IFEQ      '24'
045200970526     C     W             IFGT      6
045300970526     C                   MOVEL     'S'           FUDATT
045400970526     C                   ELSE
045500970526     C                   MOVEL     *BLANKS       FUDATT
045600970526     C                   END                                                    W GT 8
045700970526     C                   MOVEL     FUDRIG        FUD(I)
045800970526     C                   END
045900970526     C                   END                                                    DO WHILE
046000970526     C*
046100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
046200970526     C     W             DIV       6             I
046300970526     C                   MVR                     Z
046400970526     C*
046500970526     C     Z             IFNE      0
046600970526     C     I             ADD       1             Z
046700970526     C                   ELSE
046800970526     C                   Z-ADD     I             Z                 5 0
046900970526     C                   END
047000970526     C*
047100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
047200970526     C                   ADD       1             FUNGRU
047300970526     C     FUNGRU        IFGT      Z
047400970526     C     FUNGRU        ORLT      1
047500970526     C                   Z-ADD     1             FUNGRU
047600970526     C                   END
047700970526     C*
047800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
047900970526     C     FUNGRU        IFEQ      1
048000970526     C                   MOVEL     FUV(1)        FUN001
048100970526     C                   MOVEL     FUV(2)        FUN002
048200970526     C                   MOVEL     FUV(3)        FUN003
048300970526     C                   MOVEL     FUV(4)        FUN004
048400970526     C                   MOVEL     FUV(5)        FUN005
048500970526     C                   MOVEL     FUV(6)        FUN006
048600970526     C                   END
048700970526     C*
048800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
048900970526     C     FUNGRU        IFEQ      2
049000970526     C                   MOVEL     FUV(7)        FUN001
049100970526     C                   MOVEL     FUV(8)        FUN002
049200970526     C                   MOVEL     FUV(9)        FUN003
049300970526     C                   MOVEL     FUV(10)       FUN004
049400970526     C                   MOVEL     FUV(11)       FUN005
049500970526     C                   MOVEL     FUV(12)       FUN006
049600970526     C                   END
049700970526     C*
049800970526     C* TERZO GRUPPO DI 6 FUNZIONI
049900970526     C     FUNGRU        IFEQ      3
050000970526     C                   MOVEL     FUV(13)       FUN001
050100970526     C                   MOVEL     FUV(14)       FUN002
050200970526     C                   MOVEL     FUV(15)       FUN003
050300970526     C                   MOVEL     FUV(16)       FUN004
050400970526     C                   MOVEL     FUV(17)       FUN005
050500970526     C                   MOVEL     FUV(18)       FUN006
050600970526     C                   END
050700970526     C*
050800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
050900970526     C     FUNGRU        IFEQ      4
051000970526     C                   MOVEL     FUV(19)       FUN001
051100970526     C                   MOVEL     FUV(20)       FUN002
051200970526     C                   MOVEL     FUV(21)       FUN003
051300970526     C                   MOVEL     FUV(22)       FUN004
051400970526     C                   MOVEL     *BLANKS       FUN005
051500970526     C                   MOVEL     *BLANKS       FUN006
051600970526     C                   END
051700970526     C*
051800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
051900970526     C                   MOVEL     *BLANKS       FUN007
052000970526     C                   MOVEL     *BLANKS       FUN008
052100970526     C*
052200970526     C     1             DO        24            I
052300970526     C                   MOVEL     FUD(I)        FUDRIG
052400970526     C     FUDFUN        IFEQ      '24'
052500970526     C     FUDATT        ANDEQ     'S'
052600970526     C                   MOVEL     FUDDES        FUN008
052700970526     C                   END
052800970526     C                   END                                                    DO
052900970526     C*
053000970526     C                   ENDSR
053100970526     C*------------------------------------------------------------------------*
053200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
053300970526     C*------------------------------------------------------------------------*
053400970526     C     FUNCON        BEGSR
053500970526     C*
053600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
053700970526     C                   EXSR      FUNVAR
053800970526     C*
053900970526     C* PULISCO CAMPI DI LAVORO
054000970526     C                   Z-ADD     0             I                 5 0
054100970526     C                   Z-ADD     0             W                 5 0
054200970526     C*
054300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
054400970526     C     FUNKEY        IFNE      *BLANKS
054500970526     C     1             DO        24            I
054600970526     C                   MOVEL     FUD(I)        FUDRIG
054700970526     C*
054800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
054900970526     C     FUNKEY        IFEQ      FUDFUN
055000970526     C     FUDATT        ANDEQ     'S'
055100970526     C                   Z-ADD     I             W
055200970526     C                   END
055300970526     C                   END                                                    DO WHILE
055400970526     C*
055500970526     C* FUNZIONE NON ATTIVA (ERRORE)
055600970526     C     W             IFEQ      0
055700970526     C*
055800970526     C* EMISSIONE MESSAGGIO DI ERRORE
055900970526     C                   MOVEL     MSG(1)        DSMSMS
056000970526     C*
056100970526     C                   END                                                    W=0
056200970526     C                   END                                                    FUNKEY NE *BLANKS
056300970526     C*
056400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
056500970526     C     W             IFNE      0
056600970526     C     FUNKEY        OREQ      *BLANKS
056700970526     C     FUNKEY        IFEQ      *BLANKS
056800970526     C                   EXSR      ENTGES
056900970526     C                   END
057000970526     C     FUNKEY        IFEQ      '03'
057100970526     C                   EXSR      F03GES
057200970526     C                   END
057300970526     C     FUNKEY        IFEQ      '06'
057400970526     C                   EXSR      F06GES
057500970526     C                   END
057600970526     C     FUNKEY        IFEQ      '12'
057700970526     C                   EXSR      F12GES
057800970526     C                   END
057900970526     C     FUNKEY        IFEQ      '24'
058000970526     C                   EXSR      F24GES
058100970526     C                   END
058200970526     C                   END
058300970526     C*
058400970526     C                   ENDSR
058500970526     C*------------------------------------------------------------------------*
058600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
058700970526     C*------------------------------------------------------------------------*
058800970526     C     FUNVAR        BEGSR
058900970526     C*
059000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
059100970526     C*
059200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
059300970526     C     KEY           IFEQ      ENTER
059400970526     C                   MOVEL     *BLANKS       FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F1
059700970526     C                   MOVEL     '01'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F2
060000970526     C                   MOVEL     '02'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F3
060300970526     C                   MOVEL     '03'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F4
060600970526     C                   MOVEL     '04'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F5
060900970526     C                   MOVEL     '05'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F6
061200970526     C                   MOVEL     '06'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F7
061500970526     C                   MOVEL     '07'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F8
061800970526     C                   MOVEL     '08'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F9
062100970526     C                   MOVEL     '09'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F10
062400970526     C                   MOVEL     '10'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F11
062700970526     C                   MOVEL     '11'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F12
063000970526     C                   MOVEL     '12'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F13
063300970526     C                   MOVEL     '13'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F14
063600970526     C                   MOVEL     '14'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F15
063900970526     C                   MOVEL     '15'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F16
064200970526     C                   MOVEL     '16'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F17
064500970526     C                   MOVEL     '17'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F18
064800970526     C                   MOVEL     '18'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F19
065100970526     C                   MOVEL     '19'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F20
065400970526     C                   MOVEL     '20'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F21
065700970526     C                   MOVEL     '21'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F22
066000970526     C                   MOVEL     '22'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F23
066300970526     C                   MOVEL     '23'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F24
066600970526     C                   MOVEL     '24'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      ROLLUP
066900970526     C                   MOVEL     'RU'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      HELP
067200970526     C                   MOVEL     'HP'          FUNKEY
067300970526     C                   END
067400970526     C                   ENDSR
067500970526     C*------------------------------------------------------------------------*
067600970526     C* NOTE COSTRUZIONE SCHIERE
067700970526     C*------------------------------------------------------------------------*
067800970526     C*
067900970526     C* -MSG-MESSAGGI
068000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
068100970526     C* (NON TOCCARE MSG,1 E MSG,2)
068200970526     C*
068300970526     C* -CMD-COMANDI
068400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
068500970526     C*
068600970526     C* -OPD-OPZIONI
068700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
068800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
068900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
069000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
069100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
069200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
069300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
069500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
069600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
069700970526     C*
069800970526     C* -FUD-FUNZIONI
069900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
070000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
070300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
070400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
070500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
070600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
070700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
070800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
070900970526** MSG - MESSAGGI
071000120927Tasto funzionale non gestito                                                   1
071100150904Codice Logo obbligatorio                                                       2
071200150904Codice Logo inesistente in tab.AR8                                             3
071300141216                                                                               4
071400130108                                                                               5
071500150904                                                                               6
071600130108                                                                               7
071700130108                                                                               8
071800120927                                                                               9
071900120927                                                                               10
072000120927                                                                               11
072100120927                                                                               12
072200120927                                                                               13
072300141027                                                                               14
072400140116                                                                               15
072500120927                                                                               16
072600120927                                                                               17
072700120927                                                                               18
072800120927                                                                               19
072900150904Stampa effettuata                                                              20
073000141027                                                                               21
073100141027                                                                               22
073200121108                                                                               23
073300121108                                                                               24
073400970526** FUD - FUNZIONI
073500970526                   -        (S/N)=N                        funz.01¬
073600970526                   -        (S/N)=N                        funz.02¬
073700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
073800970526                   -        (S/N)=N                        funz.04¬
073900970526                   -        (S/N)=N                        funz.05¬
074000991214F06=Conferma       -        (S/N)=S                        funz.06¬
074100140819                   -        (S/N)=N                        funz.07¬
074200970527                   -        (S/N)=N                        funz.08¬
074300970527                   -        (S/N)=N                        funz.09¬
074400970527                   -        (S/N)=N                        funz.10¬
074500970526                   -        (S/N)=N                        funz.11¬
074600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
074700970526                   -        (S/N)=N                        funz.13¬
074800970526                   -        (S/N)=N                        funz.14¬
074900970526                   -        (S/N)=N                        funz.15¬
075000970526                   -        (S/N)=N                        funz.16¬
075100970526                   -        (S/N)=N                        funz.17¬
075200970526                   -        (S/N)=N                        funz.18¬
075300970526                   -        (S/N)=N                        funz.19¬
075400970526                   -        (S/N)=N                        funz.20¬
075500970526                   -        (S/N)=N                        funz.21¬
075600970526                   -        (S/N)=N                        funz.22¬
075700970526                   -        (S/N)=N                        funz.23¬
075800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
