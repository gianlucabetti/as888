000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800120927     Fcnaco00f  IF   E           K DISK
000900121108     FUBDSTVASD1cf   e             WORKSTN INFDS(DEVDS1)
001000121108     F                                     SFILE(DSTVASDMS:MSGULT)
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
007500121003
007600121108     d pIn_FGS         s              3s 0
007700121108     d pIn_CCM         s              7s 0
007800121108     d pIn_CMR         s             35
007900121108     d pIn_CNT         s              5s 0
008000121108     d pIn_Flags       s              4
008100121108     d pIn_Lib         s             10
008200121108     d pIn_TpFile      s              5
008300121108     d pOut_Esito      s              2s 0
008400121108     d pOut_MotivoErr  s              5s 0
008500121108     d UBDSTVASR       pr                  extpgm('UBDSTVASR')
008600121108     d pIn_FGS                        3s 0
008700121108     d pIn_CCM                        7s 0
008800121108     d pIn_CMR                       35
008900121108     d pIn_CNT                        5s 0
009000121108     d pIn_Flags                      4
009100121108     d pIn_Lib                       10
009200121108     d pIn_TpFile                     5
009300121108     d pOut_Esito                     2s 0
009400121108     d pOut_MotivoErr                 5s 0
009500120928
009600970526     C*------------------------------------------------------------------------*
009700000303     C* MAIN LINES
009800970526     C*------------------------------------------------------------------------*
009900970526     C*
010000120927     C* metto i default su D03
010100120927     C                   EXSR      InzD03
010200120927     C*
010300120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010400991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010700970526     C*
010800970526     C* EMETTE FORMATO VIDEO
010900970526     C                   EXSR      BUTFMR
011000970526     C*
011100970526     C* GESTIONE TASTI FUNZIONALI
011200970526     C                   EXSR      FUNCON
011300970526E   2C                   ENDDO
011400970526     C*
011500970526     C* FINE PGM
011600970526     C                   SETON                                        LR
011700970526     C*------------------------------------------------------------------------*
011800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011900970526     C*------------------------------------------------------------------------*
012000970526     C     BUTFMR        BEGSR
012100970526     C*
012200970526     C* EMETTE LA TESTATA
012300121108     C                   WRITE     DSTVASD01
012400970526     C*
012500970526     C* SPEDISCE EVENTUALE MESSAGGIO
012600970526     C                   EXSR      SNDMSG
012700970526     C*
012800970526     C* EMETTE LE FUNZIONI
012900121108     C                   WRITE     DSTVASD02
013000970526     C*
013100970526     C* EMETTE IL VIDEO
013200121108     C                   EXFMT     DSTVASD03
013300970526     C*
013400991214     C* REIMPOSTA FLAG DI ERRORE
013500991214     C                   MOVEL     '0'           ERRL00
013600970526     C*
013700970526     C* REIMPOSTA INDICATORI DI ERRORE
013800020611     C                   SETOFF                                       555556
013900970526     C*
014000970526     C                   ENDSR
014100970526     C*------------------------------------------------------------------------*
014200120927     C* InzD03 - Inizializzo D03
014300970526     C*------------------------------------------------------------------------*
014400120927     C     InzD03        BEGSR
014500970526     C*
014600121108     C                   EVAL      VidTPFILE = 'FIVA '
014700120927     C*
014800120927     C                   ENDSR
014900120927     C*------------------------------------------------------------------------*
015000120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015100120927     C*------------------------------------------------------------------------*
015200120927     C     SNDMSG        BEGSR
015300120927     C*
015400970526     C* PULISCE IL SFILE MESSAGGI
015500970526     C                   MOVEL     '1'           *IN40
015600121108     C                   WRITE     DSTVASDMC
015700970526     C                   MOVEL     '0'           *IN40
015800970526     C*
015900970526     C* VISUALIZZA IL MESSAGGIO
016000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016100970526     C                   CALL      'XMSG'        XMSGPA
016200020611     C     DSMSER        IFEQ      *ZEROS
016300970526     C                   MOVEL     DSMSKE        MSGKEY
016400020611     C                   ENDIF
016500970526     C                   Z-ADD     1             MSGULT
016600121108     C                   WRITE     DSTVASDMS
016700121108     C                   WRITE     DSTVASDMC
016800970526     C*
016900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017000970526     C                   MOVEL     *BLANKS       DSMSMS
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* ENTGES - GESTIONE ENTER -> INVIO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     ENTGES        BEGSR
017700970526     C*
017800970526     C* CONTROLLA I CAMPI IMMESSI
017900970526     C                   EXSR      DECODI
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F03GES - GESTIONE F03 -> FINE
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F03GES        BEGSR
018600970526     C*
018700970526     C* IMPOSTA "PREMUTO F03"
018800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200991214     C* F06GES - GESTIONE F06 -> CONFERMA
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F06GES        BEGSR
019500970526     C*
019600991214     C* CONTROLLA I CAMPI IMMESSI
019700991214     C                   EXSR      DECODI
019800020611     C*
019900991214     C* ESEGUE ROUTINE DI CONFERMA
020000020611     C     ERRL00        IFEQ      '0'
020100991214     C                   EXSR      ESEGUI
020200121109     C* se la sincolarizzazione è andata a buon fine emetto messaggio
020300121109     C                   IF        pOut_Esito = 0
020400121109     C                   MOVEL     MSG(20)       DSMSMS
020500121109     C                   ENDIF
020600121109     C* non esco mai dal pgm
020700121109     C***                MOVEL     '1'           FINL00
020800121108     C                   ENDIF
020900970526     C*
021000991214     C                   ENDSR
021100970526     C*------------------------------------------------------------------------*
021200970526     C* F12GES - GESTIONE F12 -> RITORNO
021300970526     C*------------------------------------------------------------------------*
021400970526     C     F12GES        BEGSR
021500970526     C*
021600970526     C* IMPOSTA "PREMUTO F12"
021700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F24GES        BEGSR
022400970526     C*
022500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022600970526     C*
022700970526     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
023000970526     C*------------------------------------------------------------------------*
023100991214     C     ESEGUI        BEGSR
023200121108     C*
023300121108     C                   EVAL      pIn_FGS     = VIDFGS
023400121108     C                   EVAL      pIn_CCM     = VIDCCM
023500121108     C                   EVAL      pIn_CMR     = VIDCMR
023600121108     C                   EVAL      pIn_CNT     = VIDCNT
023700121108     c                   EVAL      pIn_Flags   = VIDFlags
023800121108     c                   EVAL      pIn_Lib     = VIDLib
023900121108     c                   EVAL      pIn_TpFile  = VIDTpFile
024000121108     C* chiamo
024100121108     C                   CALL      'UBDSTVASR'
024200121108     C                   PARM                    pIn_FGS
024300121108     C                   PARM                    pIn_CCM
024400121108     C                   PARM                    pIn_CMR
024500121108     C                   PARM                    pIn_CNT
024600121108     c                   PARM                    pIn_Flags
024700121108     c                   PARM                    pIn_Lib
024800121108     c                   PARM                    pIn_TpFile
024900121108     c                   parm                    pOut_Esito
025000121108     c                   parm                    pOut_MotivoErr
025100121108     C*
025200121108     c                   EVAL      VIDEsito   = pOut_Esito
025300121108     c                   EVAL      VIDMotivo  = pOut_MotivoErr
025400991214     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025800970526     C*------------------------------------------------------------------------*
025900970526     C     DECODI        BEGSR
026000121019     C*
026100120927     C                   SETOFF                                       505152
026200120927     C                   SETOFF                                       535455
026300120927     C                   SETOFF                                       565758
026400121019     C                   SETOFF                                       596061
026500000306     C*
026600120927     C* Ctrl i campi obbligatori
026700120927     C     ERRL00        IFEQ      '0'
026800120927     C                   SELECT
026900121108     C* Tipi files (deve esserci qualunque tipo file)
027000121108     C                   WHEN      VIDTPFILE = *blank
027100121019     C                   MOVEL     '1'           ERRL00
027200121108     C                   SETON                                        55
027300121019     C                   MOVEL     MSG(6)        DSMSMS
027400121019     C                   MOVEL     *BLANKS       VDDCCM
027500121108     C* P.O. gestione (deve esserci qualunque tipo file)
027600121108     C                   WHEN      VIDFGS = 0
027700121108     C                   MOVEL     '1'           ERRL00
027800121108     C                   SETON                                        50
027900121108     C                   MOVEL     MSG(6)        DSMSMS
028000121108     C                   MOVEL     *BLANKS       VDDCCM
028100121108     C* cod.cliente (deve esserci qualunque tipo file)
028200121108     C                   WHEN      VIDCCM = 0
028300121108     C                   MOVEL     '1'           ERRL00
028400121108     C                   SETON                                        51
028500121108     C                   MOVEL     MSG(6)        DSMSMS
028600121108     C                   MOVEL     *BLANKS       VDDCCM
028700121108     C* CMR (deve esserci solo per EDIVA)
028800130107     C***                WHEN      VIDCMR = *blank and VIDTpFile = 'EDIVA'
028900130107     C***                MOVEL     '1'           ERRL00
029000130107     C***                SETON                                        52
029100130107     C***                MOVEL     MSG(6)        DSMSMS
029200130107     C***                MOVEL     *BLANKS       VDDCCM
029300121108     C* contatore (deve esserci solo per EDIVA)
029400121108     C                   WHEN      VIDCNT = 0 and VIDTpFile = 'EDIVA'
029500121108     C                   MOVEL     '1'           ERRL00
029600121108     C                   SETON                                        53
029700121108     C                   MOVEL     MSG(6)        DSMSMS
029800121108     C                   MOVEL     *BLANKS       VDDCCM
029900120927     C                   ENDSL
030000120927     C                   ENDIF
030100121108     C*
030200121108     C* Flags (deve essere indicato almeno 1 per qualunque tipo file, e si deve fare con la X)
030300121108     C                   IF        VIDFLAGS = *blank
030400121108     C                             or %subst(VIDFLAGS : 1 : 1) <> 'X'
030500121108     C                                and %subst(VIDFLAGS : 1 : 1) <> *blank
030600121108     C                             or %subst(VIDFLAGS : 2 : 1) <> 'X'
030700121108     C                                and %subst(VIDFLAGS : 2 : 1) <> *blank
030800121108     C                             or %subst(VIDFLAGS : 3 : 1) <> 'X'
030900121108     C                                and %subst(VIDFLAGS : 3 : 1) <> *blank
031000121108     C                             or %subst(VIDFLAGS : 4 : 1) <> 'X'
031100121108     C                                and %subst(VIDFLAGS : 4 : 1) <> *blank
031200121108     C                   MOVEL     '1'           ERRL00
031300121108     C                   SETON                                        54
031400121108     C                   MOVEL     MSG(8)        DSMSMS
031500121108     C                   MOVEL     *BLANKS       VDDCCM
031600121108     C                   ENDIF
031700121108     C*
031800121108     C* Ctrl i campi che non devono esserci se scelto FIVA
031900121108     C     ERRL00        IFEQ      '0'
032000121108     C                   SELECT
032100121108     C* CMR (deve esserci solo per EDIVA)
032200121108     C                   WHEN      VIDCMR <> *blank and VIDTpFile = 'FIVA'
032300121108     C                   MOVEL     '1'           ERRL00
032400121108     C                   SETON                                        52
032500121108     C                   MOVEL     MSG(7)        DSMSMS
032600121108     C                   MOVEL     *BLANKS       VDDCCM
032700121108     C* contatore (deve esserci solo per EDIVA)
032800121108     C                   WHEN      VIDCNT > 0 and VIDTpFile = 'FIVA'
032900121108     C                   MOVEL     '1'           ERRL00
033000121108     C                   SETON                                        53
033100121108     C                   MOVEL     MSG(7)        DSMSMS
033200121108     C                   MOVEL     *BLANKS       VDDCCM
033300121108     C                   ENDSL
033400121108     C                   ENDIF
033500121001     C*
033600121001     C* Cod. cliente mittente esistente su CNACO
033700121001     C     ERRL00        IFEQ      '0'
033800120927     C                   Z-ADD     1             ACOkut
033900120927     C                   Z-ADD     DUTKCI        ACOkcc
034000120927     C                   Z-ADD     VIDCCM        ACOksc
034100120927     C     K03ACO00      CHAIN     cnaco00f
034200120927     C                   IF        %found(cnaco00f)
034300120927     C                   MOVEL     ACOrag        VDDCCM
034400120927     C                   ELSE
034500120927     C                   MOVEL     '1'           ERRL00
034600120927     C                   SETON                                        51
034700120927     C                   MOVEL     MSG(3)        DSMSMS
034800120927     C                   MOVEL     *BLANKS       VDDCCM
034900120927     C                   ENDIF
035000120927     C                   ENDIF
035100120927     C*
035200970528     C                   ENDSR
035300970526     C*------------------------------------------------------------------------*
035400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
035500970526     C*------------------------------------------------------------------------*
035600970526     C     AZZVID        BEGSR
035700970526     C* CAMPI
035800970526     C*
035900970526     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
036200970526     C*------------------------------------------------------------------------*
036300970526     C     IMPFUN        BEGSR
036400970526     C*
036500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
036600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
036700970526     C                   EXSR      FUNGES
036800970526     C*
036900970526     C                   ENDSR
037000020923     C*--------------------------------------------------------------------------------------------*
037100020923     C* REPERISCE I DATI UTENTE
037200020923     C*--------------------------------------------------------------------------------------------*
037300020923     C     REPDATIUTE    BEGSR
037400020923     C*
037500020923     C* INIZIALIZZA VARIABILI DI WRK
037600020923     C                   CLEAR                   TIBS34DS
037700020923     C                   CLEAR                   AZUTEDS
037800020923     C                   CLEAR                   DDATIUTE
037900020923     C*
038000020923     C     *DTAARA       DEFINE    §azute        azuteds
038100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
038200020923     C                   IN(E)     *DTAARA
038300020923if  1C                   IF        %Error
038400020923     c                   EVAL      I34Tla = 'L'
038500020923     C                   CALL      'TIBS34R'
038600020923     C                   PARM                    Tibs34Ds
038700020923     C                   IN        *DTAARA
038800020923e   1C                   ENDIF
038900020923     C*
039000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
039100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
039200020923     C*
039300020923     C                   ENDSR
039400970526     C*------------------------------------------------------------------------*
039500970526     C* *INZSR - OPERAZIONI INIZIALI
039600970526     C*------------------------------------------------------------------------*
039700970526     C     *INZSR        BEGSR
039800991217     C*
039900991217     C     *ENTRY        PLIST
040000991217     C                   PARM                    KPJBA
040100120927     C*
040200120927     C* LETTURA cnaco00f -
040300120927     C     K03ACO00      KLIST
040400120927     C                   KFLD                    ACOkut
040500120927     C                   KFLD                    ACOkcc
040600120927     C                   KFLD                    ACOksc
040700020923     C*
040800020923     C* REPERISCE I DATI DELL'UTENTE
040900020923     C                   EXSR      REPDATIUTE
041000970526     C*
041100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
041200970526     C     XMSGPA        PLIST
041300970526     C                   PARM                    DSMSMS
041400970526     C                   PARM                    DSMSPG
041500970526     C                   PARM                    DSMSKE
041600970526     C                   PARM                    DSMSER
041700970526     C*
041800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
041900970526     C                   CALL      'XKEY'
042000970526     C                   PARM                    DSKEY
042100970526     C*---
042200970526     C* VARIABILI NON RIFERITE AL DATA BASE
042300970526     C*---
042400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
042500991214     C                   MOVEL     *BLANKS       DEPOP1            3
042600970526     C*---
042700970526     C* VARIABILI DI CONTROLLO
042800970526     C*---
042900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
043000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
043100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
043200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
043300970526     C*
043400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
043500121108     C                   MOVEL     'UBDSTVASR'   DSMSPG
043600121108     C                   MOVEL     'UBDSTVASR'   MSGQUE
043700970526     C                   MOVEL     *BLANKS       MSGKEY
043800970526     C                   MOVEL     *BLANKS       DSMSMS
043900970528     C*
044000970528     C* CALCOLA LA DATA CORRENTE
044100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
044200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
044300970528     C                   Z-ADD     WN8           G08DAT
044400970528     C                   Z-ADD     *ZEROS        G08INV
044500970528     C                   MOVEL     '0'           G08ERR
044600970528     C                   CALL      'XSRDA8'
044700970528     C                   PARM                    WLBDA8
044800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
044900000310     C*
045000000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
045100000310     C                   EXSR      AZZVID
045200970526     C*
045300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
045400970526     C                   EXSR      IMPFUN
045500970526     C*
045600970526     C                   ENDSR
045700970526     C*------------------------------------------------------------------------*
045800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
045900970526     C*------------------------------------------------------------------------*
046000970526     C     FUNGES        BEGSR
046100970526     C*
046200970526     C* PULISCO CAMPI DI LAVORO
046300970526     C                   Z-ADD     0             W                 5 0
046400970526     C*
046500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
046600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
046700970526     C     1             DO        24            I
046800970526     C                   MOVEL     FUD(I)        FUDRIG
046900970526     C*
047000970526     C* PULIZIA FUNZIONI PRECEDENTI
047100970526     C                   MOVEL     *BLANKS       FUV(I)
047200970526     C*
047300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
047400970526     C* (SALTA F24)
047500970526     C     FUDATT        IFEQ      'S'
047600970526     C     FUDFUN        ANDNE     '24'
047700970526     C                   ADD       1             W
047800970526     C                   MOVEL     FUDDES        FUV(W)
047900970526     C                   END
048000970526     C                   END                                                    DO WHILE
048100970526     C*
048200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
048300970526     C* ALTRIMENTI DISATTIVO F24
048400970526     C     1             DO        24            I
048500970526     C                   MOVEL     FUD(I)        FUDRIG
048600970526     C*
048700970526     C     FUDFUN        IFEQ      '24'
048800970526     C     W             IFGT      6
048900970526     C                   MOVEL     'S'           FUDATT
049000970526     C                   ELSE
049100970526     C                   MOVEL     *BLANKS       FUDATT
049200970526     C                   END                                                    W GT 8
049300970526     C                   MOVEL     FUDRIG        FUD(I)
049400970526     C                   END
049500970526     C                   END                                                    DO WHILE
049600970526     C*
049700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
049800970526     C     W             DIV       6             I
049900970526     C                   MVR                     Z
050000970526     C*
050100970526     C     Z             IFNE      0
050200970526     C     I             ADD       1             Z
050300970526     C                   ELSE
050400970526     C                   Z-ADD     I             Z                 5 0
050500970526     C                   END
050600970526     C*
050700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
050800970526     C                   ADD       1             FUNGRU
050900970526     C     FUNGRU        IFGT      Z
051000970526     C     FUNGRU        ORLT      1
051100970526     C                   Z-ADD     1             FUNGRU
051200970526     C                   END
051300970526     C*
051400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
051500970526     C     FUNGRU        IFEQ      1
051600970526     C                   MOVEL     FUV(1)        FUN001
051700970526     C                   MOVEL     FUV(2)        FUN002
051800970526     C                   MOVEL     FUV(3)        FUN003
051900970526     C                   MOVEL     FUV(4)        FUN004
052000970526     C                   MOVEL     FUV(5)        FUN005
052100970526     C                   MOVEL     FUV(6)        FUN006
052200970526     C                   END
052300970526     C*
052400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
052500970526     C     FUNGRU        IFEQ      2
052600970526     C                   MOVEL     FUV(7)        FUN001
052700970526     C                   MOVEL     FUV(8)        FUN002
052800970526     C                   MOVEL     FUV(9)        FUN003
052900970526     C                   MOVEL     FUV(10)       FUN004
053000970526     C                   MOVEL     FUV(11)       FUN005
053100970526     C                   MOVEL     FUV(12)       FUN006
053200970526     C                   END
053300970526     C*
053400970526     C* TERZO GRUPPO DI 6 FUNZIONI
053500970526     C     FUNGRU        IFEQ      3
053600970526     C                   MOVEL     FUV(13)       FUN001
053700970526     C                   MOVEL     FUV(14)       FUN002
053800970526     C                   MOVEL     FUV(15)       FUN003
053900970526     C                   MOVEL     FUV(16)       FUN004
054000970526     C                   MOVEL     FUV(17)       FUN005
054100970526     C                   MOVEL     FUV(18)       FUN006
054200970526     C                   END
054300970526     C*
054400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
054500970526     C     FUNGRU        IFEQ      4
054600970526     C                   MOVEL     FUV(19)       FUN001
054700970526     C                   MOVEL     FUV(20)       FUN002
054800970526     C                   MOVEL     FUV(21)       FUN003
054900970526     C                   MOVEL     FUV(22)       FUN004
055000970526     C                   MOVEL     *BLANKS       FUN005
055100970526     C                   MOVEL     *BLANKS       FUN006
055200970526     C                   END
055300970526     C*
055400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
055500970526     C                   MOVEL     *BLANKS       FUN007
055600970526     C                   MOVEL     *BLANKS       FUN008
055700970526     C*
055800970526     C     1             DO        24            I
055900970526     C                   MOVEL     FUD(I)        FUDRIG
056000970526     C     FUDFUN        IFEQ      '24'
056100970526     C     FUDATT        ANDEQ     'S'
056200970526     C                   MOVEL     FUDDES        FUN008
056300970526     C                   END
056400970526     C                   END                                                    DO
056500970526     C*
056600970526     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
056900970526     C*------------------------------------------------------------------------*
057000970526     C     FUNCON        BEGSR
057100970526     C*
057200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
057300970526     C                   EXSR      FUNVAR
057400970526     C*
057500970526     C* PULISCO CAMPI DI LAVORO
057600970526     C                   Z-ADD     0             I                 5 0
057700970526     C                   Z-ADD     0             W                 5 0
057800970526     C*
057900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
058000970526     C     FUNKEY        IFNE      *BLANKS
058100970526     C     1             DO        24            I
058200970526     C                   MOVEL     FUD(I)        FUDRIG
058300970526     C*
058400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
058500970526     C     FUNKEY        IFEQ      FUDFUN
058600970526     C     FUDATT        ANDEQ     'S'
058700970526     C                   Z-ADD     I             W
058800970526     C                   END
058900970526     C                   END                                                    DO WHILE
059000970526     C*
059100970526     C* FUNZIONE NON ATTIVA (ERRORE)
059200970526     C     W             IFEQ      0
059300970526     C*
059400970526     C* EMISSIONE MESSAGGIO DI ERRORE
059500970526     C                   MOVEL     MSG(1)        DSMSMS
059600970526     C*
059700970526     C                   END                                                    W=0
059800970526     C                   END                                                    FUNKEY NE *BLANKS
059900970526     C*
060000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
060100970526     C     W             IFNE      0
060200970526     C     FUNKEY        OREQ      *BLANKS
060300970526     C     FUNKEY        IFEQ      *BLANKS
060400970526     C                   EXSR      ENTGES
060500970526     C                   END
060600970526     C     FUNKEY        IFEQ      '03'
060700970526     C                   EXSR      F03GES
060800970526     C                   END
060900970526     C     FUNKEY        IFEQ      '06'
061000970526     C                   EXSR      F06GES
061100970526     C                   END
061200970526     C     FUNKEY        IFEQ      '12'
061300970526     C                   EXSR      F12GES
061400970526     C                   END
061500970526     C     FUNKEY        IFEQ      '24'
061600970526     C                   EXSR      F24GES
061700970526     C                   END
061800970526     C                   END
061900970526     C*
062000970526     C                   ENDSR
062100970526     C*------------------------------------------------------------------------*
062200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
062300970526     C*------------------------------------------------------------------------*
062400970526     C     FUNVAR        BEGSR
062500970526     C*
062600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
062700970526     C*
062800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
062900970526     C     KEY           IFEQ      ENTER
063000970526     C                   MOVEL     *BLANKS       FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F1
063300970526     C                   MOVEL     '01'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F2
063600970526     C                   MOVEL     '02'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F3
063900970526     C                   MOVEL     '03'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F4
064200970526     C                   MOVEL     '04'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F5
064500970526     C                   MOVEL     '05'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F6
064800970526     C                   MOVEL     '06'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F7
065100970526     C                   MOVEL     '07'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F8
065400970526     C                   MOVEL     '08'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F9
065700970526     C                   MOVEL     '09'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F10
066000970526     C                   MOVEL     '10'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F11
066300970526     C                   MOVEL     '11'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F12
066600970526     C                   MOVEL     '12'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F13
066900970526     C                   MOVEL     '13'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F14
067200970526     C                   MOVEL     '14'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F15
067500970526     C                   MOVEL     '15'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F16
067800970526     C                   MOVEL     '16'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F17
068100970526     C                   MOVEL     '17'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F18
068400970526     C                   MOVEL     '18'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      F19
068700970526     C                   MOVEL     '19'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      F20
069000970526     C                   MOVEL     '20'          FUNKEY
069100970526     C                   END
069200970526     C     KEY           IFEQ      F21
069300970526     C                   MOVEL     '21'          FUNKEY
069400970526     C                   END
069500970526     C     KEY           IFEQ      F22
069600970526     C                   MOVEL     '22'          FUNKEY
069700970526     C                   END
069800970526     C     KEY           IFEQ      F23
069900970526     C                   MOVEL     '23'          FUNKEY
070000970526     C                   END
070100970526     C     KEY           IFEQ      F24
070200970526     C                   MOVEL     '24'          FUNKEY
070300970526     C                   END
070400970526     C     KEY           IFEQ      ROLLUP
070500970526     C                   MOVEL     'RU'          FUNKEY
070600970526     C                   END
070700970526     C     KEY           IFEQ      HELP
070800970526     C                   MOVEL     'HP'          FUNKEY
070900970526     C                   END
071000970526     C                   ENDSR
071100970526     C*------------------------------------------------------------------------*
071200970526     C* NOTE COSTRUZIONE SCHIERE
071300970526     C*------------------------------------------------------------------------*
071400970526     C*
071500970526     C* -MSG-MESSAGGI
071600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
071700970526     C* (NON TOCCARE MSG,1 E MSG,2)
071800970526     C*
071900970526     C* -CMD-COMANDI
072000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
072100970526     C*
072200970526     C* -OPD-OPZIONI
072300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
072400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
072500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
072600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
072700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
072800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
072900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
073000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
073100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
073200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
073300970526     C*
073400970526     C* -FUD-FUNZIONI
073500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
073600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
073700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
073800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
073900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
074000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
074100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
074200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
074300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
074400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
074500970526     O*------------------------------------------------------------------------*
074600970526** MSG - MESSAGGI
074700120927Tasto funzionale non gestito                                                   1
074800120927Numero chiamate obbligatorio                                                   2
074900120927Codice cliente non esiste su CNACO00F                                          3
075000121108                                                                               4
075100121108                                                                               5
075200120927Campo obbligatorio                                                             6
075300121108Il campo non deve essere valorizzato se tipi file FIVA*                        7
075400121108I flags sono obbligatori e vanno indicati con X                                8
075500120927                                                                               9
075600120927                                                                               10
075700120927                                                                               11
075800120927                                                                               12
075900120927                                                                               13
076000120927Data errata                                                                    14
076100120927La data inizio deve essere maggiore della data fine                            15
076200120927                                                                               16
076300120927                                                                               17
076400120927                                                                               18
076500120927                                                                               19
076600121109L'operazione si è conclusa positivamente                                       20
076700120927Scelta obbligatoria                                                            21
076800120927                                                                               22
076900121108                                                                               23
077000121108                                                                               24
077100970526** FUD - FUNZIONI
077200970526                   -        (S/N)=N                        funz.01¬
077300970526                   -        (S/N)=N                        funz.02¬
077400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
077500970526                   -        (S/N)=N                        funz.04¬
077600970526                   -        (S/N)=N                        funz.05¬
077700991214F06=Conferma       -        (S/N)=S                        funz.06¬
077800121108                   -        (S/N)=N                        funz.07¬
077900970527                   -        (S/N)=N                        funz.08¬
078000970527                   -        (S/N)=N                        funz.09¬
078100970527                   -        (S/N)=N                        funz.10¬
078200970526                   -        (S/N)=N                        funz.11¬
078300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
078400970526                   -        (S/N)=N                        funz.13¬
078500970526                   -        (S/N)=N                        funz.14¬
078600970526                   -        (S/N)=N                        funz.15¬
078700970526                   -        (S/N)=N                        funz.16¬
078800970526                   -        (S/N)=N                        funz.17¬
078900970526                   -        (S/N)=N                        funz.18¬
079000970526                   -        (S/N)=N                        funz.19¬
079100970526                   -        (S/N)=N                        funz.20¬
079200970526                   -        (S/N)=N                        funz.21¬
079300970526                   -        (S/N)=N                        funz.22¬
079400970526                   -        (S/N)=N                        funz.23¬
079500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
