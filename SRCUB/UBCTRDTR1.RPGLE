000100150916     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200150916     h dftactgrp(*no)
000300150916     h bnddir('UBBNDDIR')
000400970526     H*------------------------------------------------------------------------*
000500150915     H* Lancio Reperimento Tariffa decorrente alla data
000600970526     H*------------------------------------------------------------------------*
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000150915     FUBCTRDTD1 cf   e             WORKSTN INFDS(DEVDS1)
001100150915     F                                     SFILE(CTRDTDMS:MSGULT)
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D* MESSAGGI
001600970526     D*------------
001700000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* DESCRIZIONI TASTI FUNZIONALI
002000970526     D*------------
002100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* FUNZIONI DA CARICARE A VIDEO
002400970526     D*------------
002500970526     D FUV             S             19    DIM(24)
002600970526     D*------------------------------------------------------------------------*
002700970526     D* INPUT
002800970526     D*------------------------------------------------------------------------*
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000970528     D*------------------
005100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200970528     D*------------------
005300970528     D WLBDA8          DS                  INZ
005400970528     D  G08DAT                 1      8  0
005500970528     D  G08INV                 9     16  0
005600970528     D  G08ERR                17     17
005700970528     D  G08TGI                18     22  0
005800970526     D*------------
005900970526     D* ARCHITETTURA
006000970526     D*------------
006100970526     D KPJBA         E DS
006200020923     D*-------------------
006300020923     D* DS REPERIMENTO DATI UTENTE
006400020923     D*-------------------
006500020923     D TIBS34DS      E DS                                                       *Profili utente
006600020923     D DDATIUTE      E DS                                                       *Dati utente
006700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800150915     D TNTAM00F      E DS                                                       *Tariffe
006900141028     D*-------------------
007000150915     D* parametri UBCTRDTR
007100141028     D*-------------------
007200150915     D pIn_CTR         s              3s 0
007300150915     D pIn_KSC         s              7s 0
007400150915     D pIn_DtRif       s              8s 0
007500150915     D pOut_Esito      s              2s 0
007600150915     D pOut_EsitoDesc  s             50
007700150915     D pOut_Rcd        s            256
007800120928     D*-------------------
007900120928
008000121003     d Status         sds
008100121003     d  SDSpgm           *proc
008200121003     d  SDSprm           *parms
008300121003     d  SDSuser              254    263
008400130116
008500130116     d ora             s               t
008600140117     d wX              s              2s 0
008700141217     d $Debug          s              1a
008800120928
008900970526     C*------------------------------------------------------------------------*
009000000303     C* MAIN LINES
009100970526     C*------------------------------------------------------------------------*
009200970526     C*
009300120927     C* metto i default su D03
009400120927     C                   EXSR      InzD03
009500120927     C*
009600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010000970526     C*
010100970526     C* EMETTE FORMATO VIDEO
010200970526     C                   EXSR      BUTFMR
010300970526     C*
010400970526     C* GESTIONE TASTI FUNZIONALI
010500970526     C                   EXSR      FUNCON
010600970526E   2C                   ENDDO
010700970526     C*
010800970526     C* FINE PGM
010900970526     C                   SETON                                        LR
011000970526     C*------------------------------------------------------------------------*
011100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011200970526     C*------------------------------------------------------------------------*
011300970526     C     BUTFMR        BEGSR
011400970526     C*
011500970526     C* EMETTE LA TESTATA
011600150915     C                   WRITE     CTRDTD01
011700970526     C*
011800970526     C* SPEDISCE EVENTUALE MESSAGGIO
011900970526     C                   EXSR      SNDMSG
012000970526     C*
012100970526     C* EMETTE LE FUNZIONI
012200150915     C                   WRITE     CTRDTD02
012300970526     C*
012400970526     C* EMETTE IL VIDEO
012500150915     C                   EXFMT     CTRDTD03
012600970526     C*
012700991214     C* REIMPOSTA FLAG DI ERRORE
012800991214     C                   MOVEL     '0'           ERRL00
012900970526     C*
013000970526     C* REIMPOSTA INDICATORI DI ERRORE
013100020611     C                   SETOFF                                       555556
013200970526     C*
013300970526     C                   ENDSR
013400970526     C*------------------------------------------------------------------------*
013500120927     C* InzD03 - Inizializzo D03
013600970526     C*------------------------------------------------------------------------*
013700120927     C     InzD03        BEGSR
013800120927     C*
013900130108     C*
014000120927     C                   ENDSR
014100120927     C*------------------------------------------------------------------------*
014200120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014300120927     C*------------------------------------------------------------------------*
014400120927     C     SNDMSG        BEGSR
014500120927     C*
014600970526     C* PULISCE IL SFILE MESSAGGI
014700970526     C                   MOVEL     '1'           *IN40
014800150915     C                   WRITE     CTRDTDMC
014900970526     C                   MOVEL     '0'           *IN40
015000970526     C*
015100970526     C* VISUALIZZA IL MESSAGGIO
015200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015300970526     C                   CALL      'XMSG'        XMSGPA
015400020611     C     DSMSER        IFEQ      *ZEROS
015500970526     C                   MOVEL     DSMSKE        MSGKEY
015600020611     C                   ENDIF
015700970526     C                   Z-ADD     1             MSGULT
015800150915     C                   WRITE     CTRDTDMS
015900150915     C                   WRITE     CTRDTDMC
016000970526     C*
016100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016200970526     C                   MOVEL     *BLANKS       DSMSMS
016300970526     C*
016400970526     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* ENTGES - GESTIONE ENTER -> INVIO
016700970526     C*------------------------------------------------------------------------*
016800970526     C     ENTGES        BEGSR
016900970526     C*
017000970526     C* CONTROLLA I CAMPI IMMESSI
017100970526     C                   EXSR      DECODI
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* F03GES - GESTIONE F03 -> FINE
017600970526     C*------------------------------------------------------------------------*
017700970526     C     F03GES        BEGSR
017800970526     C*
017900970526     C* IMPOSTA "PREMUTO F03"
018000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400991214     C* F06GES - GESTIONE F06 -> CONFERMA
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F06GES        BEGSR
018700970526     C*
018800991214     C* CONTROLLA I CAMPI IMMESSI
018900991214     C                   EXSR      DECODI
019000020611     C*
019100991214     C* ESEGUE ROUTINE DI CONFERMA
019200020611     C     ERRL00        IFEQ      '0'
019300991214     C                   EXSR      ESEGUI
019400130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
019500121109     C                   MOVEL     MSG(20)       DSMSMS
019600121109     C* non esco mai dal pgm
019700121109     C***                MOVEL     '1'           FINL00
019800121108     C                   ENDIF
019900970526     C*
020000991214     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200130109     C* F07GES - GESTIONE F07 -> STAMPA
020300970526     C*------------------------------------------------------------------------*
020400130109     C     F07GES        BEGSR
020500970526     C*
020600140818     C                   EXSR      F06GES
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
022700140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
022800970526     C*------------------------------------------------------------------------*
022900991214     C     ESEGUI        BEGSR
023000141027     C*
023100150915     C                   EVAL      pIn_KSC  = D03Ksc
023200150915     C                   EVAL      pIn_CTR  = D03CTr
023300150915     C                   EVAL      pIn_DTRif= D03DtR
023400150915     C*
023500150915     C                   CLEAR                   D03Esito
023600150915     C                   CLEAR                   D03EsitoD
023700150915     C                   CLEAR                   D03Prg
023800150915     C                   CLEAR                   D03DDT
023900150915     C                   CLEAR                   D03DST
024000150915     C                   CLEAR                   D03TSP
024100150915     C                   CLEAR                   D03GCA
024200150915     C                   CLEAR                   D03GGA
024300150915     C                   CLEAR                   D03GMA
024400150915     C                   CLEAR                   D03GVA
024500150915     C                   CLEAR                   D03FIE
024600150915     C                   CLEAR                   D03TIN
024700150915     C                   CLEAR                   D03BAP
024800150915     C                   CLEAR                   D03NAS
024900150915     C                   CLEAR                   D03NOT
025000141027     C*
025100121108     C* chiamo
025200150916     C                   CALLB     'UBCTRDTR'
025300150915     c                   parm                    pIn_CTR
025400150915     c                   parm                    pIn_KSC
025500150915     c                   parm                    pIn_DtRif
025600150915     c                   parm                    pOut_Esito
025700150915     c                   parm                    pOut_EsitoDesc
025800150915     c                   parm                    pOut_Rcd
025900150915     C*
026000150915     C                   EVAL      D03Esito = pOut_Esito
026100150915     C                   EVAL      D03EsitoD= pOut_EsitoDesc
026200150915     C* se l'esito è buono ci saranno anche il progressivo e i dati
026300150915     C                   IF        pOut_Esito = 0
026400150915     C* prendo i dati
026500150915     C                   EVAL      TNTAM00F = pOut_Rcd
026600150915     C                   EVAL      D03Prg   = TAMPRG
026700150915     C                   EVAL      D03DDT   = TAMDDT
026800150915     C                   EVAL      D03DST   = TAMDST
026900150915     C                   EVAL      D03TSP   = TAMTSP
027000150915     C                   EVAL      D03GCA   = TAMGCA
027100150915     C                   EVAL      D03GGA   = TAMGGA
027200150915     C                   EVAL      D03GMA   = TAMGMA
027300150915     C                   EVAL      D03GVA   = TAMGVA
027400150915     C                   EVAL      D03FIE   = TAMFIE
027500150915     C                   EVAL      D03TIN   = TAMTIN
027600150915     C                   EVAL      D03BAP   = TAMBAP
027700150915     C                   EVAL      D03NAS   = TAMNAS
027800150915     C                   EVAL      D03NOT   = TAMNOT
027900150915     C                   ENDIF
028000140120     C*
028100970526     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028400970526     C*------------------------------------------------------------------------*
028500970526     C     DECODI        BEGSR
028600121019     C*
028700120927     C                   SETOFF                                       505152
028800140120     c                   EVAL      VIDMSG    = *blank
028900120927     C*
029000150915     C* il cliente è obbligatorio
029100141216     C*
029200141216     C     ERRL00        IFEQ      '0'
029300150915     C                   IF        D03KSC = 0
029400141216     C                   MOVEL     '1'           ERRL00
029500141216     C                   SETON                                        50
029600141216     C                   MOVEL     MSG(2)        DSMSMS
029700141216     C                   ENDIF
029800141216     C                   ENDIF
029900150915     C*
030000150915     C* la tariffa è obbligatorio però 000 è una tariffa
030100150915     C*
030200150915     C*    ERRL00        IFEQ      '0'
030300150915     C*                  IF        D03CTR = 0
030400150915     C*                  MOVEL     '1'           ERRL00
030500150915     C*                  SETON                                        51
030600150915     C*                  MOVEL     MSG(3)        DSMSMS
030700150915     C*                  ENDIF
030800150915     C*                  ENDIF
030900150915     C*
031000150915     C* la data di riferimento è obbligatoria
031100150915     C*
031200150915     C     ERRL00        IFEQ      '0'
031300150916     C* se data=0 imposto oggi
031400150915     C                   IF        D03DTR = 0
031500150916     C                   EVAL      D03DTR = %dec(%date() : *ISO)
031600150915     C                   ENDIF
031700150916     C                   ENDIF
031800141216     C*
031900970528     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
032200970526     C*------------------------------------------------------------------------*
032300970526     C     AZZVID        BEGSR
032400141027     C*
032500150915     C                   EVAL      D03KSC = 0
032600150915     C                   EVAL      D03CTR = 0
032700150915     C                   EVAL      D03DTR = 0
032800970526     C*
032900970526     C                   ENDSR
033000970526     C*------------------------------------------------------------------------*
033100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
033200970526     C*------------------------------------------------------------------------*
033300970526     C     IMPFUN        BEGSR
033400970526     C*
033500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
033600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
033700970526     C                   EXSR      FUNGES
033800970526     C*
033900970526     C                   ENDSR
034000020923     C*--------------------------------------------------------------------------------------------*
034100020923     C* REPERISCE I DATI UTENTE
034200020923     C*--------------------------------------------------------------------------------------------*
034300020923     C     REPDATIUTE    BEGSR
034400020923     C*
034500020923     C* INIZIALIZZA VARIABILI DI WRK
034600020923     C                   CLEAR                   TIBS34DS
034700020923     C                   CLEAR                   AZUTEDS
034800020923     C                   CLEAR                   DDATIUTE
034900020923     C*
035000020923     C     *DTAARA       DEFINE    §azute        azuteds
035100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
035200020923     C                   IN(E)     *DTAARA
035300020923if  1C                   IF        %Error
035400020923     c                   EVAL      I34Tla = 'L'
035500020923     C                   CALL      'TIBS34R'
035600020923     C                   PARM                    Tibs34Ds
035700020923     C                   IN        *DTAARA
035800020923e   1C                   ENDIF
035900020923     C*
036000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
036100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
036200020923     C*
036300020923     C                   ENDSR
036400970526     C*------------------------------------------------------------------------*
036500970526     C* *INZSR - OPERAZIONI INIZIALI
036600970526     C*------------------------------------------------------------------------*
036700970526     C     *INZSR        BEGSR
036800991217     C*
036900991217     C     *ENTRY        PLIST
037000991217     C                   PARM                    KPJBA
037100020923     C*
037200020923     C* REPERISCE I DATI DELL'UTENTE
037300020923     C                   EXSR      REPDATIUTE
037400970526     C*
037500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
037600970526     C     XMSGPA        PLIST
037700970526     C                   PARM                    DSMSMS
037800970526     C                   PARM                    DSMSPG
037900970526     C                   PARM                    DSMSKE
038000970526     C                   PARM                    DSMSER
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
039900150915     C                   MOVEL     'UBCTRDTR1'   DSMSPG
040000150915     C                   MOVEL     'UBCTRDTR1'   MSGQUE
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
057600130109     C     FUNKEY        IFEQ      '07'
057700130109     C                   EXSR      F07GES
057800130109     C                   END
057900970526     C     FUNKEY        IFEQ      '12'
058000970526     C                   EXSR      F12GES
058100970526     C                   END
058200970526     C     FUNKEY        IFEQ      '24'
058300970526     C                   EXSR      F24GES
058400970526     C                   END
058500970526     C                   END
058600970526     C*
058700970526     C                   ENDSR
058800970526     C*------------------------------------------------------------------------*
058900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
059000970526     C*------------------------------------------------------------------------*
059100970526     C     FUNVAR        BEGSR
059200970526     C*
059300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
059400970526     C*
059500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
059600970526     C     KEY           IFEQ      ENTER
059700970526     C                   MOVEL     *BLANKS       FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F1
060000970526     C                   MOVEL     '01'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F2
060300970526     C                   MOVEL     '02'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F3
060600970526     C                   MOVEL     '03'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F4
060900970526     C                   MOVEL     '04'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F5
061200970526     C                   MOVEL     '05'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F6
061500970526     C                   MOVEL     '06'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F7
061800970526     C                   MOVEL     '07'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F8
062100970526     C                   MOVEL     '08'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F9
062400970526     C                   MOVEL     '09'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F10
062700970526     C                   MOVEL     '10'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F11
063000970526     C                   MOVEL     '11'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F12
063300970526     C                   MOVEL     '12'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F13
063600970526     C                   MOVEL     '13'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F14
063900970526     C                   MOVEL     '14'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F15
064200970526     C                   MOVEL     '15'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F16
064500970526     C                   MOVEL     '16'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F17
064800970526     C                   MOVEL     '17'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F18
065100970526     C                   MOVEL     '18'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F19
065400970526     C                   MOVEL     '19'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F20
065700970526     C                   MOVEL     '20'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F21
066000970526     C                   MOVEL     '21'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F22
066300970526     C                   MOVEL     '22'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F23
066600970526     C                   MOVEL     '23'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F24
066900970526     C                   MOVEL     '24'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      ROLLUP
067200970526     C                   MOVEL     'RU'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      HELP
067500970526     C                   MOVEL     'HP'          FUNKEY
067600970526     C                   END
067700970526     C                   ENDSR
067800970526     C*------------------------------------------------------------------------*
067900970526     C* NOTE COSTRUZIONE SCHIERE
068000970526     C*------------------------------------------------------------------------*
068100970526     C*
068200970526     C* -MSG-MESSAGGI
068300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
068400970526     C* (NON TOCCARE MSG,1 E MSG,2)
068500970526     C*
068600970526     C* -CMD-COMANDI
068700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
068800970526     C*
068900970526     C* -OPD-OPZIONI
069000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
069100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
069200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
069300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
069400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
069500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
069600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
069800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
069900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
070000970526     C*
070100970526     C* -FUD-FUNZIONI
070200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
070300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
070600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
070700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
070800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
070900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
071000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
071100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
071200970526** MSG - MESSAGGI
071300120927Tasto funzionale non gestito                                                   1
071400150915Codice cliente obbligatorio                                                    2
071500150915Codice tariffa obbligatoria                                                    3
071600150915Data di riferimento obbligatoria                                               4
071700130108                                                                               5
071800130108Campo obbligatorio                                                             6
071900130108                                                                               7
072000130108                                                                               8
072100120927                                                                               9
072200120927                                                                               10
072300120927                                                                               11
072400120927                                                                               12
072500120927                                                                               13
072600141027                                                                               14
072700140116                                                                               15
072800120927                                                                               16
072900120927                                                                               17
073000120927                                                                               18
073100120927                                                                               19
073200141027Elaborazione conclusa                                                          20
073300141027                                                                               21
073400141027                                                                               22
073500121108                                                                               23
073600121108                                                                               24
073700970526** FUD - FUNZIONI
073800970526                   -        (S/N)=N                        funz.01¬
073900970526                   -        (S/N)=N                        funz.02¬
074000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
074100970526                   -        (S/N)=N                        funz.04¬
074200970526                   -        (S/N)=N                        funz.05¬
074300991214F06=Conferma       -        (S/N)=S                        funz.06¬
074400140819                   -        (S/N)=N                        funz.07¬
074500970527                   -        (S/N)=N                        funz.08¬
074600970527                   -        (S/N)=N                        funz.09¬
074700970527                   -        (S/N)=N                        funz.10¬
074800970526                   -        (S/N)=N                        funz.11¬
074900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
075000970526                   -        (S/N)=N                        funz.13¬
075100970526                   -        (S/N)=N                        funz.14¬
075200970526                   -        (S/N)=N                        funz.15¬
075300970526                   -        (S/N)=N                        funz.16¬
075400970526                   -        (S/N)=N                        funz.17¬
075500970526                   -        (S/N)=N                        funz.18¬
075600970526                   -        (S/N)=N                        funz.19¬
075700970526                   -        (S/N)=N                        funz.20¬
075800970526                   -        (S/N)=N                        funz.21¬
075900970526                   -        (S/N)=N                        funz.22¬
076000970526                   -        (S/N)=N                        funz.23¬
076100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
