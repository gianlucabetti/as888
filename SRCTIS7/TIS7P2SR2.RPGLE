000100970526     H*------------------------------------------------------------------------*
000200130314     H* Sostituzione maschera
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800120927     Fcnaco00f  IF   E           K DISK
000900130318     FTIS7P2SD2 cf   e             WORKSTN INFDS(DEVDS1)
001000130314     F                                     SFILE(S7TP2SDMS:MSGULT)
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
006400130314     D FIVAB00F      e ds
006500020923     D TIBS34DS      E DS                                                       *Profili utente
006600020923     D DDATIUTE      E DS                                                       *Dati utente
006700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800120927     D* DS REPERIMENTO PADRE/FIGLI
006900120927     D TIBS10DS      E DS
007000120928     D*-------------------
007100120928
007200121003     d Status         sds
007300121003     d  SDSpgm           *proc
007400121003     d  SDSprm           *parms
007500121003     d  SDSuser              254    263
007600121003
007700140114     d wDate           s              8  0 inz
007800130314     d wEsito          s              1a
007900130318     d TIS7P2SR1       pr                  extpgm('TIS7P2SR1')
008000130314     d  FIVAB                              like(FIVAB00F)
008100130314     d  In_MaskPDF                         like(VIDMask)
008200130314     d  Out_MaskPDF                        like(VIDMaskO)
008300130314     d  wEsito                             like(VIDEsito)
008400130116
008500130116     d ora             s               t
008600120928
008700970526     C*------------------------------------------------------------------------*
008800000303     C* MAIN LINES
008900970526     C*------------------------------------------------------------------------*
009000970526     C*
009100120927     C* metto i default su D03
009200120927     C                   EXSR      InzD03
009300120927     C*
009400120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009500991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009800970526     C*
009900970526     C* EMETTE FORMATO VIDEO
010000970526     C                   EXSR      BUTFMR
010100970526     C*
010200970526     C* GESTIONE TASTI FUNZIONALI
010300970526     C                   EXSR      FUNCON
010400970526E   2C                   ENDDO
010500970526     C*
010600970526     C* FINE PGM
010700970526     C                   SETON                                        LR
010800970526     C*------------------------------------------------------------------------*
010900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011000970526     C*------------------------------------------------------------------------*
011100970526     C     BUTFMR        BEGSR
011200970526     C*
011300970526     C* EMETTE LA TESTATA
011400130314     C                   WRITE     S7TP2SD01
011500970526     C*
011600970526     C* SPEDISCE EVENTUALE MESSAGGIO
011700970526     C                   EXSR      SNDMSG
011800970526     C*
011900970526     C* EMETTE LE FUNZIONI
012000130314     C                   WRITE     S7TP2SD02
012100970526     C*
012200970526     C* EMETTE IL VIDEO
012300130314     C                   EXFMT     S7TP2SD03
012400970526     C*
012500991214     C* REIMPOSTA FLAG DI ERRORE
012600991214     C                   MOVEL     '0'           ERRL00
012700970526     C*
012800970526     C* REIMPOSTA INDICATORI DI ERRORE
012900020611     C                   SETOFF                                       555556
013000970526     C*
013100970526     C                   ENDSR
013200970526     C*------------------------------------------------------------------------*
013300120927     C* InzD03 - Inizializzo D03
013400970526     C*------------------------------------------------------------------------*
013500120927     C     InzD03        BEGSR
013600120927     C*
013700140114     C* Reperimento data odierna (fmt aaaa/mm/gg)
013800140114     C                   EVAL      wDate = %dec( %date() )
013900140114     C
014000140114     C                   EVAL      VIDAAC = %dec(%subst(%editc(wDate:'X'):1:4)
014100140114     C                                           : 4 : 0)
014200130108     C*
014300120927     C                   ENDSR
014400120927     C*------------------------------------------------------------------------*
014500120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014600120927     C*------------------------------------------------------------------------*
014700120927     C     SNDMSG        BEGSR
014800120927     C*
014900970526     C* PULISCE IL SFILE MESSAGGI
015000970526     C                   MOVEL     '1'           *IN40
015100130314     C                   WRITE     S7TP2SDMC
015200970526     C                   MOVEL     '0'           *IN40
015300970526     C*
015400970526     C* VISUALIZZA IL MESSAGGIO
015500970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015600970526     C                   CALL      'XMSG'        XMSGPA
015700020611     C     DSMSER        IFEQ      *ZEROS
015800970526     C                   MOVEL     DSMSKE        MSGKEY
015900020611     C                   ENDIF
016000970526     C                   Z-ADD     1             MSGULT
016100130314     C                   WRITE     S7TP2SDMS
016200130314     C                   WRITE     S7TP2SDMC
016300970526     C*
016400970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016500970526     C                   MOVEL     *BLANKS       DSMSMS
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* ENTGES - GESTIONE ENTER -> INVIO
017000970526     C*------------------------------------------------------------------------*
017100970526     C     ENTGES        BEGSR
017200970526     C*
017300970526     C* CONTROLLA I CAMPI IMMESSI
017400970526     C                   EXSR      DECODI
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* F03GES - GESTIONE F03 -> FINE
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F03GES        BEGSR
018100970526     C*
018200970526     C* IMPOSTA "PREMUTO F03"
018300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700991214     C* F06GES - GESTIONE F06 -> CONFERMA
018800970526     C*------------------------------------------------------------------------*
018900970526     C     F06GES        BEGSR
019000970526     C*
019100991214     C* CONTROLLA I CAMPI IMMESSI
019200991214     C                   EXSR      DECODI
019300020611     C*
019400991214     C* ESEGUE ROUTINE DI CONFERMA
019500020611     C     ERRL00        IFEQ      '0'
019600991214     C                   EXSR      ESEGUI
019700130314     C* il messaggio di sostituzione conclusa lo do sempre, a prescindere dagli esiti
019800121109     C                   MOVEL     MSG(20)       DSMSMS
019900121109     C* non esco mai dal pgm
020000121109     C***                MOVEL     '1'           FINL00
020100121108     C                   ENDIF
020200970526     C*
020300991214     C                   ENDSR
020400130109     C*------------------------------------------------------------------------*
020500130109     C* F12GES - GESTIONE F12 -> RITORNO
020600130109     C*------------------------------------------------------------------------*
020700130109     C     F12GES        BEGSR
020800130109     C*
020900130109     C* IMPOSTA "PREMUTO F12"
021000130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100130109     C*
021200130109     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F24GES        BEGSR
021700970526     C*
021800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022300970526     C*------------------------------------------------------------------------*
022400991214     C     ESEGUI        BEGSR
022500121108     C*
022600130314     c                   CLEAR                   VIDMaskO
022700130314     c                   CLEAR                   VIDEsito
022800130314     c                   CLEAR                   FIVAB00F
022900130314     C* compongo FIVAB
023000130314     C                   EVAL                    VABRMN = VIDRMN
023100130314     C                   EVAL                    VABRMA = VIDRMA
023200130314     C                   EVAL                    VABNAS = VIDNAS
023300130314     C                   EVAL                    VABCCM = VIDCCM
023400140114     C                   EVAL                    VABAAS = VIDAAS
023500130314     C* chiamo
023600130318     C                   CALL      'TIS7P2SR1'
023700130314     C                   PARM                    FIVAB00F
023800130314     C                   PARM                    VIDMask
023900130314     C                   PARM                    VIDMaskO
024000130314     C                   PARM                    VIDEsito
024100121108     C*
024200991214     C*
024300970526     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024600970526     C*------------------------------------------------------------------------*
024700970526     C     DECODI        BEGSR
024800121019     C*
024900120927     C                   SETOFF                                       505152
025000120927     C                   SETOFF                                       535455
025100130123     C                   SETOFF                                       56
025200000306     C*
025300120927     C* Ctrl i campi obbligatori
025400120927     C     ERRL00        IFEQ      '0'
025500120927     C                   SELECT
025600130314     C* maschera
025700130314     C                   WHEN      VIDMask = *blank
025800130314     C                   MOVEL     '1'           ERRL00
025900130314     C                   SETON                                        50
026000130314     C                   MOVEL     MSG(6)        DSMSMS
026100130314     C* almeno un dato di FIVAB
026200130314     C                   WHEN      VIDRMN  = 0 and
026300130314     C                             VIDRMA  = *blank and
026400130314     C                             VIDNAS  = *blank and
026500140114     C                             VIDCCM  = 0 and
026600140114     C                             VIDAAS  = 0
026700121108     C                   MOVEL     '1'           ERRL00
026800130123     C                   SETON                                        515253
026900130123     C                   SETON                                        5455
027000130314     C                   MOVEL     MSG(5)        DSMSMS
027100120927     C                   ENDSL
027200120927     C                   ENDIF
027300120927     C*
027400970528     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
027700970526     C*------------------------------------------------------------------------*
027800970526     C     AZZVID        BEGSR
027900970526     C* CAMPI
028000970526     C*
028100970526     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
028400970526     C*------------------------------------------------------------------------*
028500970526     C     IMPFUN        BEGSR
028600970526     C*
028700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
028800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
028900970526     C                   EXSR      FUNGES
029000970526     C*
029100970526     C                   ENDSR
029200020923     C*--------------------------------------------------------------------------------------------*
029300020923     C* REPERISCE I DATI UTENTE
029400020923     C*--------------------------------------------------------------------------------------------*
029500020923     C     REPDATIUTE    BEGSR
029600020923     C*
029700020923     C* INIZIALIZZA VARIABILI DI WRK
029800020923     C                   CLEAR                   TIBS34DS
029900020923     C                   CLEAR                   AZUTEDS
030000020923     C                   CLEAR                   DDATIUTE
030100020923     C*
030200020923     C     *DTAARA       DEFINE    §azute        azuteds
030300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
030400020923     C                   IN(E)     *DTAARA
030500020923if  1C                   IF        %Error
030600020923     c                   EVAL      I34Tla = 'L'
030700020923     C                   CALL      'TIBS34R'
030800020923     C                   PARM                    Tibs34Ds
030900020923     C                   IN        *DTAARA
031000020923e   1C                   ENDIF
031100020923     C*
031200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
031300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
031400020923     C*
031500020923     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* *INZSR - OPERAZIONI INIZIALI
031800970526     C*------------------------------------------------------------------------*
031900970526     C     *INZSR        BEGSR
032000991217     C*
032100991217     C     *ENTRY        PLIST
032200991217     C                   PARM                    KPJBA
032300120927     C*
032400120927     C* LETTURA cnaco00f -
032500120927     C     K03ACO00      KLIST
032600120927     C                   KFLD                    ACOkut
032700120927     C                   KFLD                    ACOkcc
032800120927     C                   KFLD                    ACOksc
032900020923     C*
033000020923     C* REPERISCE I DATI DELL'UTENTE
033100020923     C                   EXSR      REPDATIUTE
033200970526     C*
033300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
033400970526     C     XMSGPA        PLIST
033500970526     C                   PARM                    DSMSMS
033600970526     C                   PARM                    DSMSPG
033700970526     C                   PARM                    DSMSKE
033800970526     C                   PARM                    DSMSER
033900970526     C*
034000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
034100970526     C                   CALL      'XKEY'
034200970526     C                   PARM                    DSKEY
034300970526     C*---
034400970526     C* VARIABILI NON RIFERITE AL DATA BASE
034500970526     C*---
034600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
034700991214     C                   MOVEL     *BLANKS       DEPOP1            3
034800970526     C*---
034900970526     C* VARIABILI DI CONTROLLO
035000970526     C*---
035100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
035200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
035300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
035400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
035500970526     C*
035600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
035700130318     C                   MOVEL     'TIS7P2SR'    DSMSPG
035800130318     C                   MOVEL     'TIS7P2SR'    MSGQUE
035900970526     C                   MOVEL     *BLANKS       MSGKEY
036000970526     C                   MOVEL     *BLANKS       DSMSMS
036100970528     C*
036200970528     C* CALCOLA LA DATA CORRENTE
036300970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
036400970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
036500970528     C                   Z-ADD     WN8           G08DAT
036600970528     C                   Z-ADD     *ZEROS        G08INV
036700970528     C                   MOVEL     '0'           G08ERR
036800970528     C                   CALL      'XSRDA8'
036900970528     C                   PARM                    WLBDA8
037000970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
037100000310     C*
037200000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
037300000310     C                   EXSR      AZZVID
037400970526     C*
037500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
037600970526     C                   EXSR      IMPFUN
037700970526     C*
037800970526     C                   ENDSR
037900970526     C*------------------------------------------------------------------------*
038000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
038100970526     C*------------------------------------------------------------------------*
038200970526     C     FUNGES        BEGSR
038300970526     C*
038400970526     C* PULISCO CAMPI DI LAVORO
038500970526     C                   Z-ADD     0             W                 5 0
038600970526     C*
038700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
038800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
038900970526     C     1             DO        24            I
039000970526     C                   MOVEL     FUD(I)        FUDRIG
039100970526     C*
039200970526     C* PULIZIA FUNZIONI PRECEDENTI
039300970526     C                   MOVEL     *BLANKS       FUV(I)
039400970526     C*
039500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
039600970526     C* (SALTA F24)
039700970526     C     FUDATT        IFEQ      'S'
039800970526     C     FUDFUN        ANDNE     '24'
039900970526     C                   ADD       1             W
040000970526     C                   MOVEL     FUDDES        FUV(W)
040100970526     C                   END
040200970526     C                   END                                                    DO WHILE
040300970526     C*
040400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
040500970526     C* ALTRIMENTI DISATTIVO F24
040600970526     C     1             DO        24            I
040700970526     C                   MOVEL     FUD(I)        FUDRIG
040800970526     C*
040900970526     C     FUDFUN        IFEQ      '24'
041000970526     C     W             IFGT      6
041100970526     C                   MOVEL     'S'           FUDATT
041200970526     C                   ELSE
041300970526     C                   MOVEL     *BLANKS       FUDATT
041400970526     C                   END                                                    W GT 8
041500970526     C                   MOVEL     FUDRIG        FUD(I)
041600970526     C                   END
041700970526     C                   END                                                    DO WHILE
041800970526     C*
041900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
042000970526     C     W             DIV       6             I
042100970526     C                   MVR                     Z
042200970526     C*
042300970526     C     Z             IFNE      0
042400970526     C     I             ADD       1             Z
042500970526     C                   ELSE
042600970526     C                   Z-ADD     I             Z                 5 0
042700970526     C                   END
042800970526     C*
042900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
043000970526     C                   ADD       1             FUNGRU
043100970526     C     FUNGRU        IFGT      Z
043200970526     C     FUNGRU        ORLT      1
043300970526     C                   Z-ADD     1             FUNGRU
043400970526     C                   END
043500970526     C*
043600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
043700970526     C     FUNGRU        IFEQ      1
043800970526     C                   MOVEL     FUV(1)        FUN001
043900970526     C                   MOVEL     FUV(2)        FUN002
044000970526     C                   MOVEL     FUV(3)        FUN003
044100970526     C                   MOVEL     FUV(4)        FUN004
044200970526     C                   MOVEL     FUV(5)        FUN005
044300970526     C                   MOVEL     FUV(6)        FUN006
044400970526     C                   END
044500970526     C*
044600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
044700970526     C     FUNGRU        IFEQ      2
044800970526     C                   MOVEL     FUV(7)        FUN001
044900970526     C                   MOVEL     FUV(8)        FUN002
045000970526     C                   MOVEL     FUV(9)        FUN003
045100970526     C                   MOVEL     FUV(10)       FUN004
045200970526     C                   MOVEL     FUV(11)       FUN005
045300970526     C                   MOVEL     FUV(12)       FUN006
045400970526     C                   END
045500970526     C*
045600970526     C* TERZO GRUPPO DI 6 FUNZIONI
045700970526     C     FUNGRU        IFEQ      3
045800970526     C                   MOVEL     FUV(13)       FUN001
045900970526     C                   MOVEL     FUV(14)       FUN002
046000970526     C                   MOVEL     FUV(15)       FUN003
046100970526     C                   MOVEL     FUV(16)       FUN004
046200970526     C                   MOVEL     FUV(17)       FUN005
046300970526     C                   MOVEL     FUV(18)       FUN006
046400970526     C                   END
046500970526     C*
046600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
046700970526     C     FUNGRU        IFEQ      4
046800970526     C                   MOVEL     FUV(19)       FUN001
046900970526     C                   MOVEL     FUV(20)       FUN002
047000970526     C                   MOVEL     FUV(21)       FUN003
047100970526     C                   MOVEL     FUV(22)       FUN004
047200970526     C                   MOVEL     *BLANKS       FUN005
047300970526     C                   MOVEL     *BLANKS       FUN006
047400970526     C                   END
047500970526     C*
047600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
047700970526     C                   MOVEL     *BLANKS       FUN007
047800970526     C                   MOVEL     *BLANKS       FUN008
047900970526     C*
048000970526     C     1             DO        24            I
048100970526     C                   MOVEL     FUD(I)        FUDRIG
048200970526     C     FUDFUN        IFEQ      '24'
048300970526     C     FUDATT        ANDEQ     'S'
048400970526     C                   MOVEL     FUDDES        FUN008
048500970526     C                   END
048600970526     C                   END                                                    DO
048700970526     C*
048800970526     C                   ENDSR
048900970526     C*------------------------------------------------------------------------*
049000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
049100970526     C*------------------------------------------------------------------------*
049200970526     C     FUNCON        BEGSR
049300970526     C*
049400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
049500970526     C                   EXSR      FUNVAR
049600970526     C*
049700970526     C* PULISCO CAMPI DI LAVORO
049800970526     C                   Z-ADD     0             I                 5 0
049900970526     C                   Z-ADD     0             W                 5 0
050000970526     C*
050100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
050200970526     C     FUNKEY        IFNE      *BLANKS
050300970526     C     1             DO        24            I
050400970526     C                   MOVEL     FUD(I)        FUDRIG
050500970526     C*
050600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
050700970526     C     FUNKEY        IFEQ      FUDFUN
050800970526     C     FUDATT        ANDEQ     'S'
050900970526     C                   Z-ADD     I             W
051000970526     C                   END
051100970526     C                   END                                                    DO WHILE
051200970526     C*
051300970526     C* FUNZIONE NON ATTIVA (ERRORE)
051400970526     C     W             IFEQ      0
051500970526     C*
051600970526     C* EMISSIONE MESSAGGIO DI ERRORE
051700970526     C                   MOVEL     MSG(1)        DSMSMS
051800970526     C*
051900970526     C                   END                                                    W=0
052000970526     C                   END                                                    FUNKEY NE *BLANKS
052100970526     C*
052200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
052300970526     C     W             IFNE      0
052400970526     C     FUNKEY        OREQ      *BLANKS
052500970526     C     FUNKEY        IFEQ      *BLANKS
052600970526     C                   EXSR      ENTGES
052700970526     C                   END
052800970526     C     FUNKEY        IFEQ      '03'
052900970526     C                   EXSR      F03GES
053000970526     C                   END
053100970526     C     FUNKEY        IFEQ      '06'
053200970526     C                   EXSR      F06GES
053300970526     C                   END
053400970526     C     FUNKEY        IFEQ      '12'
053500970526     C                   EXSR      F12GES
053600970526     C                   END
053700970526     C     FUNKEY        IFEQ      '24'
053800970526     C                   EXSR      F24GES
053900970526     C                   END
054000970526     C                   END
054100970526     C*
054200970526     C                   ENDSR
054300970526     C*------------------------------------------------------------------------*
054400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
054500970526     C*------------------------------------------------------------------------*
054600970526     C     FUNVAR        BEGSR
054700970526     C*
054800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
054900970526     C*
055000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
055100970526     C     KEY           IFEQ      ENTER
055200970526     C                   MOVEL     *BLANKS       FUNKEY
055300970526     C                   END
055400970526     C     KEY           IFEQ      F1
055500970526     C                   MOVEL     '01'          FUNKEY
055600970526     C                   END
055700970526     C     KEY           IFEQ      F2
055800970526     C                   MOVEL     '02'          FUNKEY
055900970526     C                   END
056000970526     C     KEY           IFEQ      F3
056100970526     C                   MOVEL     '03'          FUNKEY
056200970526     C                   END
056300970526     C     KEY           IFEQ      F4
056400970526     C                   MOVEL     '04'          FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      F5
056700970526     C                   MOVEL     '05'          FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F6
057000970526     C                   MOVEL     '06'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F7
057300970526     C                   MOVEL     '07'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F8
057600970526     C                   MOVEL     '08'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F9
057900970526     C                   MOVEL     '09'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F10
058200970526     C                   MOVEL     '10'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F11
058500970526     C                   MOVEL     '11'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F12
058800970526     C                   MOVEL     '12'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F13
059100970526     C                   MOVEL     '13'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F14
059400970526     C                   MOVEL     '14'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F15
059700970526     C                   MOVEL     '15'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F16
060000970526     C                   MOVEL     '16'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F17
060300970526     C                   MOVEL     '17'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F18
060600970526     C                   MOVEL     '18'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F19
060900970526     C                   MOVEL     '19'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F20
061200970526     C                   MOVEL     '20'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F21
061500970526     C                   MOVEL     '21'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F22
061800970526     C                   MOVEL     '22'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F23
062100970526     C                   MOVEL     '23'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F24
062400970526     C                   MOVEL     '24'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      ROLLUP
062700970526     C                   MOVEL     'RU'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      HELP
063000970526     C                   MOVEL     'HP'          FUNKEY
063100970526     C                   END
063200970526     C                   ENDSR
063300970526     C*------------------------------------------------------------------------*
063400970526     C* NOTE COSTRUZIONE SCHIERE
063500970526     C*------------------------------------------------------------------------*
063600970526     C*
063700970526     C* -MSG-MESSAGGI
063800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
063900970526     C* (NON TOCCARE MSG,1 E MSG,2)
064000970526     C*
064100970526     C* -CMD-COMANDI
064200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
064300970526     C*
064400970526     C* -OPD-OPZIONI
064500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
064600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
064700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
064800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
064900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
065000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
065100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
065200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
065300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
065400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
065500970526     C*
065600970526     C* -FUD-FUNZIONI
065700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
065800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
065900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
066300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
066500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
066600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
066700970526     O*------------------------------------------------------------------------*
066800970526** MSG - MESSAGGI
066900120927Tasto funzionale non gestito                                                   1
067000130314                                                                               2
067100130314                                                                               3
067200130108                                                                               4
067300130314Immettere almeno un valore                                                     5
067400130108Campo obbligatorio                                                             6
067500130108                                                                               7
067600130108                                                                               8
067700120927                                                                               9
067800120927                                                                               10
067900120927                                                                               11
068000120927                                                                               12
068100120927                                                                               13
068200120927Data errata                                                                    14
068300130314                                                                               15
068400120927                                                                               16
068500120927                                                                               17
068600120927                                                                               18
068700120927                                                                               19
068800130314Sostituzione conclusa - controllare gli esiti                                  20
068900120927Scelta obbligatoria                                                            21
069000130314                                                                               22
069100121108                                                                               23
069200121108                                                                               24
069300970526** FUD - FUNZIONI
069400970526                   -        (S/N)=N                        funz.01¬
069500970526                   -        (S/N)=N                        funz.02¬
069600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
069700970526                   -        (S/N)=N                        funz.04¬
069800970526                   -        (S/N)=N                        funz.05¬
069900991214F06=Conferma       -        (S/N)=S                        funz.06¬
070000130314                   -        (S/N)=N                        funz.07¬
070100970527                   -        (S/N)=N                        funz.08¬
070200970527                   -        (S/N)=N                        funz.09¬
070300970527                   -        (S/N)=N                        funz.10¬
070400970526                   -        (S/N)=N                        funz.11¬
070500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
070600970526                   -        (S/N)=N                        funz.13¬
070700970526                   -        (S/N)=N                        funz.14¬
070800970526                   -        (S/N)=N                        funz.15¬
070900970526                   -        (S/N)=N                        funz.16¬
071000970526                   -        (S/N)=N                        funz.17¬
071100970526                   -        (S/N)=N                        funz.18¬
071200970526                   -        (S/N)=N                        funz.19¬
071300970526                   -        (S/N)=N                        funz.20¬
071400970526                   -        (S/N)=N                        funz.21¬
071500970526                   -        (S/N)=N                        funz.22¬
071600970526                   -        (S/N)=N                        funz.23¬
071700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
