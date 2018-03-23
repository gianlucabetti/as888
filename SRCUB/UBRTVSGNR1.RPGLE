000100970526     H*------------------------------------------------------------------------*
000200130110     H* Simulazione accorpamento bolle
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800130920     FUBRTVSGND1cf   e             WORKSTN INFDS(DEVDS1)
000900130920     F                                     SFILE(RTVSGNDMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D* MESSAGGI
001400970526     D*------------
001500000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* DESCRIZIONI TASTI FUNZIONALI
001800970526     D*------------
001900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* FUNZIONI DA CARICARE A VIDEO
002200970526     D*------------
002300970526     D FUV             S             19    DIM(24)
002400130920     D*------------
002500130920     D* primi 24 segnacolli
002600130920     D*------------
002700130920     D SGNDS           DS
002800130920     D  SGN                          18    DIM(24)
002900130920     D  VIDSGN01               1     18
003000130920     D  VIDSGN02              19     36
003100130920     D  VIDSGN03              37     54
003200130920     D  VIDSGN04              55     72
003300130920     D  VIDSGN05              73     90
003400130920     D  VIDSGN06              91    108
003500130920     D  VIDSGN07             109    126
003600130920     D  VIDSGN08             127    144
003700130920     D  VIDSGN09             145    162
003800130920     D  VIDSGN10             163    180
003900130920     D  VIDSGN11             181    198
004000130920     D  VIDSGN12             199    216
004100130920     D  VIDSGN13             217    234
004200130920     D  VIDSGN14             235    252
004300130920     D  VIDSGN15             253    270
004400130920     D  VIDSGN16             271    288
004500130920     D  VIDSGN17             289    306
004600130920     D  VIDSGN18             307    324
004700130920     D  VIDSGN19             325    342
004800130920     D  VIDSGN20             343    360
004900130920     D  VIDSGN21             361    378
005000130920     D  VIDSGN22             379    396
005100130920     D  VIDSGN23             397    414
005200130920     D  VIDSGN24             415    432
005300970526     D*------------------------------------------------------------------------*
005400970526     D* INPUT
005500970526     D*------------------------------------------------------------------------*
005600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005700970526     D*------------
005800970526     D FUDDS           DS
005900970526     D  FUDDE1                 1      1
006000970526     D  FUDFUN                 2      3
006100970526     D  FUDDE2                 4     19
006200970526     D  FUDFI0                20     34
006300970526     D  FUDATT                35     35
006400970526     D  FUDFI1                36     80
006500970526     D  FUDDES                 1     19
006600970526     D  FUDRIG                 1     80
006700970526     D*------------
006800970526     D* DS GESTIONE TASTI FUNZIONALI
006900970526     D*------------
007000970526     D DSKEY         E DS
007100970526     D DEVDS1          DS
007200970526     D  KEY                  369    369
007300970526     D*------------
007400970526     D* DS "XMSG" - GESTIONE MESSAGGI
007500970526     D*------------
007600970526     D DSMSG         E DS
007700970528     D*------------------
007800970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
007900970528     D*------------------
008000970528     D WLBDA8          DS                  INZ
008100970528     D  G08DAT                 1      8  0
008200970528     D  G08INV                 9     16  0
008300970528     D  G08ERR                17     17
008400970528     D  G08TGI                18     22  0
008500970526     D*------------
008600970526     D* ARCHITETTURA
008700970526     D*------------
008800970526     D KPJBA         E DS
008900020923     D*-------------------
009000020923     D* DS REPERIMENTO DATI UTENTE
009100020923     D*-------------------
009200020923     D TIBS34DS      E DS                                                       *Profili utente
009300020923     D DDATIUTE      E DS                                                       *Dati utente
009400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009500120927     D* DS REPERIMENTO PADRE/FIGLI
009600120927     D TIBS10DS      E DS
009700120928     D*-------------------
009800120928
009900121003     d Status         sds
010000121003     d  SDSpgm           *proc
010100121003     d  SDSprm           *parms
010200121003     d  SDSuser              254    263
010300121003
010400130920     d pIn_AAS         s              4s 0
010500130920     d pIn_LNP         s              3s 0
010600130920     d pIn_NRS         s              2s 0
010700130920     d pIn_NSP         s              7s 0
010800130920     d pIn_NCL         s              5s 0
010900130920     d pOut_SGN        s             18    dim(1000)
011000130920     d pOut_NrSGN      s              3s 0
011100130920     d pOut_Esito      s              1
011200130920
011300130920     d UBRTVSGNR       pr                  extpgm('UBRTVSGNR')
011400130920     d pIn_AAS                        4s 0
011500130920     d pIn_LNP                        3s 0
011600130920     d pIn_NRS                        2s 0
011700130920     d pIn_NSP                        7s 0
011800130920     d pIn_NCL                        5s 0
011900130920     d pOut_SGN                      18    dim(1000)
012000130920     d pOut_NrSGN                     3s 0
012100130920     d pOut_Esito                     1
012200130116
012300130116     d ora             s               t
012400120928
012500970526     C*------------------------------------------------------------------------*
012600000303     C* MAIN LINES
012700970526     C*------------------------------------------------------------------------*
012800970526     C*
012900120927     C* metto i default su D03
013000120927     C                   EXSR      InzD03
013100120927     C*
013200120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013300991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013600970526     C*
013700970526     C* EMETTE FORMATO VIDEO
013800970526     C                   EXSR      BUTFMR
013900970526     C*
014000970526     C* GESTIONE TASTI FUNZIONALI
014100970526     C                   EXSR      FUNCON
014200970526E   2C                   ENDDO
014300970526     C*
014400970526     C* FINE PGM
014500970526     C                   SETON                                        LR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014800970526     C*------------------------------------------------------------------------*
014900970526     C     BUTFMR        BEGSR
015000970526     C*
015100970526     C* EMETTE LA TESTATA
015200130920     C                   WRITE     RTVSGND01
015300970526     C*
015400970526     C* SPEDISCE EVENTUALE MESSAGGIO
015500970526     C                   EXSR      SNDMSG
015600970526     C*
015700970526     C* EMETTE LE FUNZIONI
015800130920     C                   WRITE     RTVSGND02
015900970526     C*
016000970526     C* EMETTE IL VIDEO
016100130920     C                   EXFMT     RTVSGND03
016200970526     C*
016300991214     C* REIMPOSTA FLAG DI ERRORE
016400991214     C                   MOVEL     '0'           ERRL00
016500970526     C*
016600970526     C* REIMPOSTA INDICATORI DI ERRORE
016700020611     C                   SETOFF                                       555556
016800970526     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100120927     C* InzD03 - Inizializzo D03
017200970526     C*------------------------------------------------------------------------*
017300120927     C     InzD03        BEGSR
017400120927     C*
017500130108     C*
017600120927     C                   ENDSR
017700120927     C*------------------------------------------------------------------------*
017800120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017900120927     C*------------------------------------------------------------------------*
018000120927     C     SNDMSG        BEGSR
018100120927     C*
018200970526     C* PULISCE IL SFILE MESSAGGI
018300970526     C                   MOVEL     '1'           *IN40
018400130920     C                   WRITE     RTVSGNDMC
018500970526     C                   MOVEL     '0'           *IN40
018600970526     C*
018700970526     C* VISUALIZZA IL MESSAGGIO
018800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018900970526     C                   CALL      'XMSG'        XMSGPA
019000020611     C     DSMSER        IFEQ      *ZEROS
019100970526     C                   MOVEL     DSMSKE        MSGKEY
019200020611     C                   ENDIF
019300970526     C                   Z-ADD     1             MSGULT
019400130920     C                   WRITE     RTVSGNDMS
019500130920     C                   WRITE     RTVSGNDMC
019600970526     C*
019700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019800970526     C                   MOVEL     *BLANKS       DSMSMS
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* ENTGES - GESTIONE ENTER -> INVIO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     ENTGES        BEGSR
020500970526     C*
020600970526     C* CONTROLLA I CAMPI IMMESSI
020700970526     C                   EXSR      DECODI
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F03GES - GESTIONE F03 -> FINE
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F03GES        BEGSR
021400970526     C*
021500970526     C* IMPOSTA "PREMUTO F03"
021600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021700970526     C*
021800970526     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000991214     C* F06GES - GESTIONE F06 -> CONFERMA
022100970526     C*------------------------------------------------------------------------*
022200970526     C     F06GES        BEGSR
022300970526     C*
022400991214     C* CONTROLLA I CAMPI IMMESSI
022500991214     C                   EXSR      DECODI
022600020611     C*
022700991214     C* ESEGUE ROUTINE DI CONFERMA
022800020611     C     ERRL00        IFEQ      '0'
022900991214     C                   EXSR      ESEGUI
023000130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
023100121109     C                   MOVEL     MSG(20)       DSMSMS
023200121109     C* non esco mai dal pgm
023300121109     C***                MOVEL     '1'           FINL00
023400121108     C                   ENDIF
023500970526     C*
023600991214     C                   ENDSR
023700130109     C*------------------------------------------------------------------------*
023800130109     C* F12GES - GESTIONE F12 -> RITORNO
023900130109     C*------------------------------------------------------------------------*
024000130109     C     F12GES        BEGSR
024100130109     C*
024200130109     C* IMPOSTA "PREMUTO F12"
024300130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024400130109     C*
024500130109     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024800970526     C*------------------------------------------------------------------------*
024900970526     C     F24GES        BEGSR
025000970526     C*
025100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
025600970526     C*------------------------------------------------------------------------*
025700991214     C     ESEGUI        BEGSR
025800121108     C*
025900130920     C                   EVAL      pIn_AAS     = VIDAAS
026000130920     C                   EVAL      pIn_LNP     = VIDLNP
026100130920     C                   EVAL      pIn_NRS     = VIDNRS
026200130920     C                   EVAL      pIn_NSP     = VIDNSP
026300130920     C                   EVAL      pIn_NCL     = VIDNCL
026400121108     C* chiamo
026500130920     C                   CALL      'UBRTVSGNR'
026600130920     C                   PARM                    pIn_AAS
026700130920     C                   PARM                    pIn_LNP
026800130920     C                   PARM                    pIn_NRS
026900130920     C                   PARM                    pIn_NSP
027000130920     C                   PARM                    pIn_NCL
027100130920     C                   PARM                    pOut_SGN
027200130920     c                   PARM                    pOut_NrSGN
027300130920     c                   parm                    pOut_Esito
027400121108     C*
027500130920     c                   CLEAR                   VIDEsito
027600130920     c                   CLEAR                   VIDNrSGN
027700130920     c                   CLEAR                   SGN
027800130920
027900130920     c                   EVAL      VIDEsito   = pOut_Esito
028000130920     C                   IF        VIDEsito <> '1'
028100130920     c                   EVAL      VIDNrSGN   = pOut_NrSGN
028200130920     C     1             DO        24            wX                3 0
028300130920     C                   EVAL      SGN(wx) = pOut_SGN(wX)
028400130920     C                   ENDDO
028500130920     c                   ENDIF
028600991214     C*
028700970526     C                   ENDSR
028800970526     C*------------------------------------------------------------------------*
028900991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029000970526     C*------------------------------------------------------------------------*
029100970526     C     DECODI        BEGSR
029200121019     C*
029300120927     C                   SETOFF                                       505152
029400120927     C                   SETOFF                                       535455
029500130123     C                   SETOFF                                       56
029600000306     C*
029700120927     C*
029800970528     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
030100970526     C*------------------------------------------------------------------------*
030200970526     C     AZZVID        BEGSR
030300970526     C* CAMPI
030400970526     C*
030500970526     C                   ENDSR
030600970526     C*------------------------------------------------------------------------*
030700020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
030800970526     C*------------------------------------------------------------------------*
030900970526     C     IMPFUN        BEGSR
031000970526     C*
031100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
031200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
031300970526     C                   EXSR      FUNGES
031400970526     C*
031500970526     C                   ENDSR
031600020923     C*--------------------------------------------------------------------------------------------*
031700020923     C* REPERISCE I DATI UTENTE
031800020923     C*--------------------------------------------------------------------------------------------*
031900020923     C     REPDATIUTE    BEGSR
032000020923     C*
032100020923     C* INIZIALIZZA VARIABILI DI WRK
032200020923     C                   CLEAR                   TIBS34DS
032300020923     C                   CLEAR                   AZUTEDS
032400020923     C                   CLEAR                   DDATIUTE
032500020923     C*
032600020923     C     *DTAARA       DEFINE    §azute        azuteds
032700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
032800020923     C                   IN(E)     *DTAARA
032900020923if  1C                   IF        %Error
033000020923     c                   EVAL      I34Tla = 'L'
033100020923     C                   CALL      'TIBS34R'
033200020923     C                   PARM                    Tibs34Ds
033300020923     C                   IN        *DTAARA
033400020923e   1C                   ENDIF
033500020923     C*
033600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
033800020923     C*
033900020923     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100970526     C* *INZSR - OPERAZIONI INIZIALI
034200970526     C*------------------------------------------------------------------------*
034300970526     C     *INZSR        BEGSR
034400991217     C*
034500991217     C     *ENTRY        PLIST
034600991217     C                   PARM                    KPJBA
034700020923     C*
034800020923     C* REPERISCE I DATI DELL'UTENTE
034900020923     C                   EXSR      REPDATIUTE
035000970526     C*
035100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
035200970526     C     XMSGPA        PLIST
035300970526     C                   PARM                    DSMSMS
035400970526     C                   PARM                    DSMSPG
035500970526     C                   PARM                    DSMSKE
035600970526     C                   PARM                    DSMSER
035700970526     C*
035800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
035900970526     C                   CALL      'XKEY'
036000970526     C                   PARM                    DSKEY
036100970526     C*---
036200970526     C* VARIABILI NON RIFERITE AL DATA BASE
036300970526     C*---
036400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
036500991214     C                   MOVEL     *BLANKS       DEPOP1            3
036600970526     C*---
036700970526     C* VARIABILI DI CONTROLLO
036800970526     C*---
036900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
037000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
037100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
037200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
037300970526     C*
037400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
037500130920     C                   MOVEL     'UBRTVSGNR'   DSMSPG
037600130920     C                   MOVEL     'UBRTVSGNR'   MSGQUE
037700970526     C                   MOVEL     *BLANKS       MSGKEY
037800970526     C                   MOVEL     *BLANKS       DSMSMS
037900970528     C*
038000970528     C* CALCOLA LA DATA CORRENTE
038100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
038200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
038300970528     C                   Z-ADD     WN8           G08DAT
038400970528     C                   Z-ADD     *ZEROS        G08INV
038500970528     C                   MOVEL     '0'           G08ERR
038600970528     C                   CALL      'XSRDA8'
038700970528     C                   PARM                    WLBDA8
038800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
038900000310     C*
039000000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
039100000310     C                   EXSR      AZZVID
039200970526     C*
039300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
039400970526     C                   EXSR      IMPFUN
039500970526     C*
039600970526     C                   ENDSR
039700970526     C*------------------------------------------------------------------------*
039800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
039900970526     C*------------------------------------------------------------------------*
040000970526     C     FUNGES        BEGSR
040100970526     C*
040200970526     C* PULISCO CAMPI DI LAVORO
040300970526     C                   Z-ADD     0             W                 5 0
040400970526     C*
040500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
040600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
040700970526     C     1             DO        24            I
040800970526     C                   MOVEL     FUD(I)        FUDRIG
040900970526     C*
041000970526     C* PULIZIA FUNZIONI PRECEDENTI
041100970526     C                   MOVEL     *BLANKS       FUV(I)
041200970526     C*
041300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
041400970526     C* (SALTA F24)
041500970526     C     FUDATT        IFEQ      'S'
041600970526     C     FUDFUN        ANDNE     '24'
041700970526     C                   ADD       1             W
041800970526     C                   MOVEL     FUDDES        FUV(W)
041900970526     C                   END
042000970526     C                   END                                                    DO WHILE
042100970526     C*
042200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
042300970526     C* ALTRIMENTI DISATTIVO F24
042400970526     C     1             DO        24            I
042500970526     C                   MOVEL     FUD(I)        FUDRIG
042600970526     C*
042700970526     C     FUDFUN        IFEQ      '24'
042800970526     C     W             IFGT      6
042900970526     C                   MOVEL     'S'           FUDATT
043000970526     C                   ELSE
043100970526     C                   MOVEL     *BLANKS       FUDATT
043200970526     C                   END                                                    W GT 8
043300970526     C                   MOVEL     FUDRIG        FUD(I)
043400970526     C                   END
043500970526     C                   END                                                    DO WHILE
043600970526     C*
043700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
043800970526     C     W             DIV       6             I
043900970526     C                   MVR                     Z
044000970526     C*
044100970526     C     Z             IFNE      0
044200970526     C     I             ADD       1             Z
044300970526     C                   ELSE
044400970526     C                   Z-ADD     I             Z                 5 0
044500970526     C                   END
044600970526     C*
044700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
044800970526     C                   ADD       1             FUNGRU
044900970526     C     FUNGRU        IFGT      Z
045000970526     C     FUNGRU        ORLT      1
045100970526     C                   Z-ADD     1             FUNGRU
045200970526     C                   END
045300970526     C*
045400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
045500970526     C     FUNGRU        IFEQ      1
045600970526     C                   MOVEL     FUV(1)        FUN001
045700970526     C                   MOVEL     FUV(2)        FUN002
045800970526     C                   MOVEL     FUV(3)        FUN003
045900970526     C                   MOVEL     FUV(4)        FUN004
046000970526     C                   MOVEL     FUV(5)        FUN005
046100970526     C                   MOVEL     FUV(6)        FUN006
046200970526     C                   END
046300970526     C*
046400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
046500970526     C     FUNGRU        IFEQ      2
046600970526     C                   MOVEL     FUV(7)        FUN001
046700970526     C                   MOVEL     FUV(8)        FUN002
046800970526     C                   MOVEL     FUV(9)        FUN003
046900970526     C                   MOVEL     FUV(10)       FUN004
047000970526     C                   MOVEL     FUV(11)       FUN005
047100970526     C                   MOVEL     FUV(12)       FUN006
047200970526     C                   END
047300970526     C*
047400970526     C* TERZO GRUPPO DI 6 FUNZIONI
047500970526     C     FUNGRU        IFEQ      3
047600970526     C                   MOVEL     FUV(13)       FUN001
047700970526     C                   MOVEL     FUV(14)       FUN002
047800970526     C                   MOVEL     FUV(15)       FUN003
047900970526     C                   MOVEL     FUV(16)       FUN004
048000970526     C                   MOVEL     FUV(17)       FUN005
048100970526     C                   MOVEL     FUV(18)       FUN006
048200970526     C                   END
048300970526     C*
048400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
048500970526     C     FUNGRU        IFEQ      4
048600970526     C                   MOVEL     FUV(19)       FUN001
048700970526     C                   MOVEL     FUV(20)       FUN002
048800970526     C                   MOVEL     FUV(21)       FUN003
048900970526     C                   MOVEL     FUV(22)       FUN004
049000970526     C                   MOVEL     *BLANKS       FUN005
049100970526     C                   MOVEL     *BLANKS       FUN006
049200970526     C                   END
049300970526     C*
049400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
049500970526     C                   MOVEL     *BLANKS       FUN007
049600970526     C                   MOVEL     *BLANKS       FUN008
049700970526     C*
049800970526     C     1             DO        24            I
049900970526     C                   MOVEL     FUD(I)        FUDRIG
050000970526     C     FUDFUN        IFEQ      '24'
050100970526     C     FUDATT        ANDEQ     'S'
050200970526     C                   MOVEL     FUDDES        FUN008
050300970526     C                   END
050400970526     C                   END                                                    DO
050500970526     C*
050600970526     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
050900970526     C*------------------------------------------------------------------------*
051000970526     C     FUNCON        BEGSR
051100970526     C*
051200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
051300970526     C                   EXSR      FUNVAR
051400970526     C*
051500970526     C* PULISCO CAMPI DI LAVORO
051600970526     C                   Z-ADD     0             I                 5 0
051700970526     C                   Z-ADD     0             W                 5 0
051800970526     C*
051900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
052000970526     C     FUNKEY        IFNE      *BLANKS
052100970526     C     1             DO        24            I
052200970526     C                   MOVEL     FUD(I)        FUDRIG
052300970526     C*
052400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
052500970526     C     FUNKEY        IFEQ      FUDFUN
052600970526     C     FUDATT        ANDEQ     'S'
052700970526     C                   Z-ADD     I             W
052800970526     C                   END
052900970526     C                   END                                                    DO WHILE
053000970526     C*
053100970526     C* FUNZIONE NON ATTIVA (ERRORE)
053200970526     C     W             IFEQ      0
053300970526     C*
053400970526     C* EMISSIONE MESSAGGIO DI ERRORE
053500970526     C                   MOVEL     MSG(1)        DSMSMS
053600970526     C*
053700970526     C                   END                                                    W=0
053800970526     C                   END                                                    FUNKEY NE *BLANKS
053900970526     C*
054000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
054100970526     C     W             IFNE      0
054200970526     C     FUNKEY        OREQ      *BLANKS
054300970526     C     FUNKEY        IFEQ      *BLANKS
054400970526     C                   EXSR      ENTGES
054500970526     C                   END
054600970526     C     FUNKEY        IFEQ      '03'
054700970526     C                   EXSR      F03GES
054800970526     C                   END
054900970526     C     FUNKEY        IFEQ      '06'
055000970526     C                   EXSR      F06GES
055100970526     C                   END
055200970526     C     FUNKEY        IFEQ      '12'
055300970526     C                   EXSR      F12GES
055400970526     C                   END
055500970526     C     FUNKEY        IFEQ      '24'
055600970526     C                   EXSR      F24GES
055700970526     C                   END
055800970526     C                   END
055900970526     C*
056000970526     C                   ENDSR
056100970526     C*------------------------------------------------------------------------*
056200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
056300970526     C*------------------------------------------------------------------------*
056400970526     C     FUNVAR        BEGSR
056500970526     C*
056600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
056700970526     C*
056800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
056900970526     C     KEY           IFEQ      ENTER
057000970526     C                   MOVEL     *BLANKS       FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F1
057300970526     C                   MOVEL     '01'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F2
057600970526     C                   MOVEL     '02'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F3
057900970526     C                   MOVEL     '03'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F4
058200970526     C                   MOVEL     '04'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F5
058500970526     C                   MOVEL     '05'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F6
058800970526     C                   MOVEL     '06'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F7
059100970526     C                   MOVEL     '07'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F8
059400970526     C                   MOVEL     '08'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F9
059700970526     C                   MOVEL     '09'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F10
060000970526     C                   MOVEL     '10'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F11
060300970526     C                   MOVEL     '11'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F12
060600970526     C                   MOVEL     '12'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F13
060900970526     C                   MOVEL     '13'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F14
061200970526     C                   MOVEL     '14'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F15
061500970526     C                   MOVEL     '15'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F16
061800970526     C                   MOVEL     '16'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F17
062100970526     C                   MOVEL     '17'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F18
062400970526     C                   MOVEL     '18'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F19
062700970526     C                   MOVEL     '19'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F20
063000970526     C                   MOVEL     '20'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F21
063300970526     C                   MOVEL     '21'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F22
063600970526     C                   MOVEL     '22'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F23
063900970526     C                   MOVEL     '23'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F24
064200970526     C                   MOVEL     '24'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      ROLLUP
064500970526     C                   MOVEL     'RU'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      HELP
064800970526     C                   MOVEL     'HP'          FUNKEY
064900970526     C                   END
065000970526     C                   ENDSR
065100970526     C*------------------------------------------------------------------------*
065200970526     C* NOTE COSTRUZIONE SCHIERE
065300970526     C*------------------------------------------------------------------------*
065400970526     C*
065500970526     C* -MSG-MESSAGGI
065600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
065700970526     C* (NON TOCCARE MSG,1 E MSG,2)
065800970526     C*
065900970526     C* -CMD-COMANDI
066000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
066100970526     C*
066200970526     C* -OPD-OPZIONI
066300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
066400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
066500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
066600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
067100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
067200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
067300970526     C*
067400970526     C* -FUD-FUNZIONI
067500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
067600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
067900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
068000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
068100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
068200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
068300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
068400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
068500970526** MSG - MESSAGGI
068600120927Tasto funzionale non gestito                                                   1
068700120927Numero chiamate obbligatorio                                                   2
068800130920                                                                               3
068900130108                                                                               4
069000130108                                                                               5
069100130108Campo obbligatorio                                                             6
069200130108                                                                               7
069300130108                                                                               8
069400120927                                                                               9
069500120927                                                                               10
069600120927                                                                               11
069700120927                                                                               12
069800120927                                                                               13
069900120927Data errata                                                                    14
070000130108La data inizio deve essere > o = della data fine                               15
070100120927                                                                               16
070200120927                                                                               17
070300120927                                                                               18
070400120927                                                                               19
070500130920Reperimento concluso - controllare l'esito                                     20
070600120927Scelta obbligatoria                                                            21
070700130110Eseguita la stampa                                                             22
070800121108                                                                               23
070900121108                                                                               24
071000970526** FUD - FUNZIONI
071100970526                   -        (S/N)=N                        funz.01¬
071200970526                   -        (S/N)=N                        funz.02¬
071300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
071400970526                   -        (S/N)=N                        funz.04¬
071500970526                   -        (S/N)=N                        funz.05¬
071600991214F06=Conferma       -        (S/N)=S                        funz.06¬
071700130920                   -        (S/N)=N                        funz.07¬
071800970527                   -        (S/N)=N                        funz.08¬
071900970527                   -        (S/N)=N                        funz.09¬
072000970527                   -        (S/N)=N                        funz.10¬
072100970526                   -        (S/N)=N                        funz.11¬
072200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
072300970526                   -        (S/N)=N                        funz.13¬
072400970526                   -        (S/N)=N                        funz.14¬
072500970526                   -        (S/N)=N                        funz.15¬
072600970526                   -        (S/N)=N                        funz.16¬
072700970526                   -        (S/N)=N                        funz.17¬
072800970526                   -        (S/N)=N                        funz.18¬
072900970526                   -        (S/N)=N                        funz.19¬
073000970526                   -        (S/N)=N                        funz.20¬
073100970526                   -        (S/N)=N                        funz.21¬
073200970526                   -        (S/N)=N                        funz.22¬
073300970526                   -        (S/N)=N                        funz.23¬
073400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
