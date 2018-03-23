000100000000     H*------------------------------------------------------------------------*
000200071001     H* CONTROLLO ASSORBIMENTI X GIRI O DISTINTE/GIRI RITIRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800071001     FFIOR79D   CF   E             WORKSTN INFDS(DEVDS1)
000900071001     F                                     SFILE(OR79DMS:MSGULT)
001000060830     FTNTBE01L  IF   E           K DISK
001100070105     FAZORG01L  IF   E           K DISK
001200070105     FFNFVV01L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000000000     D*-------------------
002100000000     D* DESCRIZIONI TASTI FUNZIONALI
002200000000     D*-------------------
002300000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400000000     D*-------------------
002500000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002600000000     D*-------------------
002700000000     D FUV             S             19    DIM(24)
002800000000     D*-------------------
002900000000     D*------------------------------------------------------------------------*
003000000000     D* INPUT
003100000000     D*------------------------------------------------------------------------*
003200000000     D*-------------------
003300000000     D* RIDEFINIZIONE SCHIERA -FUD-
003400000000     D*-------------------
003500000000     D FUDDS           DS
003600000000     D*                -DESCRIZIONE PARZIALE
003700000000     D  FUDDE1                 1      1
003800000000     D*                -FUNZIONE
003900000000     D  FUDFUN                 2      3
004000000000     D*                -DESCRIZIONE PARZIALE
004100000000     D  FUDDE2                 4     19
004200000000     D*                -FILLER
004300000000     D  FUDFI0                20     34
004400000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004500000000     D  FUDATT                35     35
004600000000     D*                -FILLER
004700000000     D  FUDFI1                36     80
004800000000     D*                -DESCRIZIONE COMPLETA
004900000000     D  FUDDES                 1     19
005000000000     D*                -RIDEFINIZIONE RIGA
005100000000     D  FUDRIG                 1     80
005200000000     D*-------------------
005300000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005400000000     D*-------------------
005500000000     D DSMSG         E DS
005600000000     D*-------------------
005700000000     D* DS GESTIONE TASTI FUNZIONALI
005800000000     D*-------------------
005900000000     D DSKEY         E DS
006000000000     D DEVDS1          DS
006100000000     D  KEY                  369    369
006200000000     D*-------------------
006300000000     D* DS "XSRDA8" - CONTROLLO DATA(8)
006400000000     D*-------------------
006500971128     D WLBDA8          DS
006600000000     D  G08DAT                 1      8  0
006700000000     D  G08INV                 9     16  0
006800000000     D  G08ERR                17     17
006900000000     D  G08TGI                18     22  0
007000060830     D*-------------------
007100000000     D* ARCHITETTURA
007200000000     D*-------------------
007300000000     D KPJBA         E DS
007400070223     D*-------------------
007500070223     D* DESCRIZIONI ORGANIGRAMMA
007600070223     D*-------------------
007700071001     D OG147         E DS
007800070105     D*-------------------
007900070105     D* DS REPERIMENTO DATI UTENTE
008000070105     D*-------------------
008100070105     D TIBS34DS      E DS                                                       *Profili utente
008200070105     D DDATIUTE      E DS                                                       *Dati utente
008300070105     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008400070105     D*-------------------
008500070202     D* DS PER TRUL06R - CARICAMENTO £1/£6
008600070105     D*-------------------
008700070105     D TRUL06DS      E DS
008800070105     D  Lin                    1     90  0
008900070105     D                                     DIM(30)                              SKI COMODO
009000070105     D TIBS36DS      E DS
009100070105     D DSLR01        E DS
009200001016     D*-------------------
009300001016     D* DS PARAMETRI DI LANCIO PGM
009400001016     D*-------------------
009500060829     D PARAM           DS                  INZ
009600070105     D  In_FGS                        3  0
009700070105     D  In_DFV                        8  0
009800070105     D  In_NFV                        7  0
009900070129     D  In_LIV                        1
010000070105     D  In_TIP                        2
010100070223     D  In_PIG                        1
010200000000     C*------------------------------------------------------------------------*
010300000000     C* MAIN LINE
010400000000     C*------------------------------------------------------------------------*
010500000000     C*
010600000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
010700000000IF  1C     ERRL00        IFEQ      '0'
010800000000     C*
010900000000     C* CICLO FINO A CHE:
011000000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
011100000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
011200000000DO  2C     F03L00        DOUEQ     '1'
011300000000     C     F12L00        OREQ      '1'
011400000000     C*
011500000000     C* EMETTE FORMATO VIDEO
011600000000     C                   EXSR      BUTFMR
011700000000     C*
011800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
011900000000     C                   EXSR      FUNCON
012000000000     C*
012100000000E   2C                   ENDDO
012200000000E   1C                   ENDIF
012300000000     C*
012400000000     C                   SETON                                        LR
012500000000     C*------------------------------------------------------------------------*
012600000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
012700000000     C*------------------------------------------------------------------------*
012800000000     C     BUTFMR        BEGSR
012900000000     C*
013000000000     C* EMETTE TESTATA
013100071001     C                   WRITE     or79D01
013200000000     C*
013300000000     C* VISUALIZZA MESSAGGIO
013400000000     C                   EXSR      SNDMSG
013500000000     C*
013600000000     C* EMETTE FUNZIONI
013700071001     C                   WRITE     or79D02
013800000000     C*
013900000000     C* EMETTE FORMATO VIDEO
014000071001     C                   EXFMT     or79D03
014100000000     C*
014200000000     C* AZZERA FLAG DI ERRORE LIVELLO
014300000000     C                   MOVEL     '0'           ERRL00
014400000000     C*
014500000000     C* AZZERA INDICATORI DI ERRORE
014600070105     C                   SETOFF                                       707172
014700070202     C                   SETOFF                                       73
014800000000     C*
014900000000     C                   ENDSR
015000000000     C*------------------------------------------------------------------------*
015100000000     C* SNDMSG - MANDA MESSAGGIO
015200000000     C*------------------------------------------------------------------------*
015300000000     C     SNDMSG        BEGSR
015400000000     C*
015500000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
015600000000     C                   MOVEL     '1'           *IN40
015700071001     C                   WRITE     or79DMC
015800000000     C                   MOVEL     '0'           *IN40
015900000000     C*
016000000000     C* VISUALIZZAZIONE MESSAGGIO
016100000000     C                   Z-ADD     0             MSGULT            4 0
016200000000     C                   CALL      'XMSG'        XMSGPA
016300000000     C     DSMSER        IFEQ      0
016400000000     C                   MOVEL     DSMSKE        MSGKEY
016500000000     C                   ENDIF
016600000000     C                   Z-ADD     1             MSGULT
016700071001     C                   WRITE     or79DMS                                      SFL MESS
016800071001     C                   WRITE     or79DMC                                      CONTR MESS
016900000000     C*
017000000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
017100000000     C                   MOVEL     *BLANKS       DSMSMS
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
017600000000     C*------------------------------------------------------------------------*
017700000000     C     ENTGES        BEGSR
017800000000     C*
017900000000     C* CONTROLLA I CAMPI IMMESSI
018000000000     C                   EXSR      DECODI
018100000000     C*
018200000000     C                   ENDSR
018300000000     C*------------------------------------------------------------------------*
018400000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
018500000000     C*------------------------------------------------------------------------*
018600000000     C     F03GES        BEGSR
018700000000     C*
018800000000     C* IMPOSTA FLAG F03
018900000000     C                   MOVEL     '1'           F03L00
019000000000     C*
019100000000     C                   ENDSR
019200000000     C*------------------------------------------------------------------------*
019300000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=LANCIO
019400000000     C*------------------------------------------------------------------------*
019500000000     C     F06GES        BEGSR
019600000000     C*
019700000000     C* CONTROLLA I CAMPI IMMESSI
019800000000     C                   EXSR      DECODI
019900000000     C*
020000000000     C* LANCIA IL PROGRAMMA DI STAMPA
020100000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORE
020200000000     C*
020300000000     C                   EXSR      VIDPAR
020400010417     C                   MOVEL(p)  PARAM         KPJBU
020500071001     C                   MOVEL     'OR80'        KCOAZ
020600071022     C                   CALL      'BCH10'
020700071022 MOM C****               CALL      'FIOR80R'
020800000000     C                   PARM                    KPJBA
020900060829     C*
021000060829     C* QUINDI ESCO DAL PROGRAMMA
021100060829     C                   MOVEL     '1'           F03L00
021200000000E   1C                   ENDIF
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
021700000000     C*------------------------------------------------------------------------*
021800000000     C     F12GES        BEGSR
021900000000     C*
022000000000     C* IMPOSTO FLAG F12
022100000000     C                   MOVEL     '1'           F12L00
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*------------------------------------------------------------------------*
022500000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRE FUNZIONI
022600000000     C*------------------------------------------------------------------------*
022700000000     C     F24GES        BEGSR
022800000000     C*
022900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
023000000000     C                   EXSR      FUNGES
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400070105     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023500000000     C*------------------------------------------------------------------------*
023600000000     C     DECODI        BEGSR
023700060830     C*
023800060830     C* AZZERA INDICATORI DI ERRORE
023900070105     C                   SETOFF                                       707172
024000070202     C                   SETOFF                                       73
024100070105     C*--------------------
024200070105     C* P.O. GESTIONE
024300070105     C*--------------------
024400070105     C     ERRL00        IFEQ      '0'
024500070105     C     VIDFGS        CHAIN     AZORG01L
024600070105     C                   IF        %found(AZORG01L)
024700071001     C                   MOVEL     ORGDE7        OG147
024800070105     C                   ELSE
024900070105     C                   MOVEL     '1'           ERRL00
025000070105     C                   MOVEL     MSG(02)       DSMSMS
025100070105     C                   SETON                                        70
025200070105     C                   ENDIF
025300070105     C                   ENDIF
025400070105     C*
025500070105     C     ERRL00        IFEQ      '0'
025600070605     C                   IF        %subst(KNMUS:1:3) <> 'EDP' AND
025700070605     C                             %subst(KNMUS:1:5) <> 'ISP99'
025800070105     C     WO36TFP       IFNE      *zeros
025900070105     C     VIDFGS        LOOKUP    Lin                                    99
026000070105     C                   IF        *IN99 = *on
026100070105     C                   ELSE
026200070105     C                   MOVEL     '1'           ERRL00
026300070105     C                   MOVEL     MSG(03)       DSMSMS
026400070105     C                   SETON                                        70
026500070105     C                   ENDIF
026600070105     C                   ENDIF
026700070604     C                   ENDIF
026800070105     C                   ENDIF
026900070105     C*
027000070105     C     ERRL00        IFEQ      '0'
027100070105     C     VIDFGS        IFEQ      *zeros
027200070105     C                   MOVEL     '1'           ERRL00
027300070105     C                   MOVEL     MSG(04)       DSMSMS
027400070105     C                   SETON                                        70
027500070105     C                   ENDIF
027600070105     C                   ENDIF
027700070223     C*--------------------
027800071001     C* VERIFICA PIANIFICAZIONE GIRI RITIR    "RISPETTO A"
027900070223     C*--------------------
028000070223     C     ERRL00        IFEQ      '0'
028100071001     C                   IF        §OGDDAO = *blanks
028200071009     C                   MOVEL     '1'           ERRL00
028300071009     C                   MOVEL     MSG(09)       DSMSMS
028400071009     C                   SETON                                        70
028500070223     C                   ENDIF
028600070223     C                   ENDIF
028700000000     C*--------------------
028800070105     C* DATA DISTINTE
028900000000     C*--------------------
029000001016     C     ERRL00        IFEQ      '0'
029100070105     C     VIDDTA        IFNE      *zeros
029200070105     C                   Z-ADD     VIDDTA        G08DAT
029300000000     C                   Z-ADD     *ZEROS        G08INV
029400000000     C                   MOVEL     '0'           G08ERR
029500000000     C                   CALL      'XSRDA8'
029600971128     C                   PARM                    WLBDA8
029700070105     C                   Z-ADD     G08DAT        VIDDTA
029800001016     C     G08ERR        IFEQ      '0'
029900070105     C                   Z-ADD     G08INV        WDTA              8 0
030000071001     C                   ELSE
030100070125     C                   MOVEL     '1'           ERRL00
030200000000     C                   MOVEL     MSG(16)       DSMSMS
030300070105     C                   SETON                                        71
030400070105     C                   ENDIF
030500071001     C                   ELSE
030600071001     C                   MOVEL     '1'           ERRL00
030700071001     C                   MOVEL     MSG(16)       DSMSMS
030800071001     C                   SETON                                        71
030900001016     C                   ENDIF
031000001016     C                   ENDIF
031100001110     C*--------------------
031200070105     C* NUMERO DISTINTA
031300001110     C*--------------------
031400001110     C     ERRL00        IFEQ      '0'
031500070105     C     VIDNDC        IFNE      *zeros
031600070105     C     VIDNDC        ANDNE     *blanks
031700070105     C     '?'           scan      VIDNDC                                 86
031800070105     C     *in86         ifeq      *on
031900070105     C                   clear                   dslr01
032000070105     C                   movel     'TIST93R'     dlrpgm
032100070105     C                   movel     *blanks       dlrtfv                         *sia open che close
032200070105     C                   z-add     4             dlrnpg
032300070105     C                   z-add     VIDFGS        dlrfgs
032400070220     C                   movel     'S'           dlrric
032500070222     C*
032600070222     C                   if        viddta = *zeros
032700070222     C                   eval      dlrgda = datcorinv - 10000
032800070222     C                   else
032900070222     C                   eval      dlrgda = viddta
033000070222     C                   eval      dlrgal = viddta
033100070222     C                   endif
033200070222     C*
033300070105     C                   movel(p)  dslr01        kpjbu
033400070105     C                   call      'FNLR02R'
033500070105     C                   parm                    kpjba
033600070105     C                   movel     kpjbu         dslr01
033700070105     C                   move      dlrnfv        VIDNDC
033800070105     C                   endif
033900070105     C                   endif
034000070105     C*
034100070105     C                   testn                   VIDNDC               86
034200070105     C   86              move      VIDNDC        vidndcn           7 0
034300070105     C* Se immesso il Nr distinta verifica che sia disponibile
034400070105     C     vidndcn       ifgt      *zero
034500070105     C                   z-add     vidndcn       dist5
034600070105     C     KEYfvv01_C    chain     fnfvv01l                           88
034700070105     C                   if        *in88 or fvvfcf<>*blanks or fvvatb<>*blanks
034800070105     C                   MOVEL     '1'           ERRL00
034900070105     C                   MOVEL     MSG(15)       DSMSMS
035000070105     C                   SETON                                        72
035100070105     C                   endif
035200070105     C                   endif
035300070105     C                   endif
035400070105     C*--------------------
035500070105     C* OBBLIGATORIETA' SELEZIONI
035600070105     C*--------------------
035700070105     C     ERRL00        IFEQ      '0'
035800070105     C                   IF        VIDDTA  = *zeros AND
035900070125     C                             (VIDNDC = *zeros OR VIDNDC = *blanks) AND
036000070125     C                             VIDTIP <> 'G'
036100070129     C                   MOVEL     '1'           ERRL00
036200070129     C                   MOVEL     MSG(07)       DSMSMS
036300070129     C                   SETON                                        7173
036400070105     C                   ENDIF
036500070129     C*
036600070129     C                   IF        VIDLIV  = 'T' AND
036700070129     C                             VIDTIP <> 'DG'
036800070129     C                   MOVEL     '1'           ERRL00
036900070129     C                   MOVEL     MSG(08)       DSMSMS
037000070129     C                   SETON                                          73
037100070129     C                   ENDIF
037200070105     C*
037300070105     C                   IF        VIDDTA  <> *zeros AND
037400070105     C                             (VIDNDC <>  *zeros AND VIDNDC <> *blanks)
037500070105     C                   MOVEL     '1'           ERRL00
037600070105     C                   MOVEL     MSG(05)       DSMSMS
037700070105     C                   SETON                                        7172
037800070105     C                   ENDIF
037900070105     C                   ENDIF
038000000000     C*
038100000000     C                   ENDSR
038200000000     C*------------------------------------------------------------------------*
038300000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
038400000000     C*------------------------------------------------------------------------*
038500000000     C     AZZVID        BEGSR
038600010417     C*
038700010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
038800071001     C                   CLEAR                   or79D03
038900070105     C*
039000070105     C                   EVAL      VIDFGS = O36POU
039100070129     C                   EVAL      VIDLIV = 'S'
039200071001     C                   EVAL      VIDTIP = 'DG'
039300070223     C                   EVAL      VIDPIG = 'D'
039400000000     C*
039500000000     C                   ENDSR
039600000000     C*------------------------------------------------------------------------*
039700001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
039800000000     C*------------------------------------------------------------------------*
039900000000     C     VIDPAR        BEGSR
040000000000     C*
040100070105     C                   Z-ADD     VIDFGS        In_FGS
040200070105     C                   Z-ADD     WDTA          In_DFV
040300070105     C                   MOVE(P)   VIDNDC        In_NFV
040400070129     C                   MOVEL(P)  VIDLIV        In_LIV
040500070105     C                   MOVEL(P)  VIDTIP        In_TIP
040600070223     C                   MOVEL(P)  VIDPIG        In_PIG
040700001016     C*
040800000000     C                   ENDSR
040900070105     C*--------------------------------------------------------------------------------------------*
041000070105     C* REPERISCE I DATI UTENTE
041100070105     C*--------------------------------------------------------------------------------------------*
041200070105     C     REPDATIUTE    BEGSR
041300070105     C*
041400070105     C* INIZIALIZZA VARIABILI DI WRK
041500070105     C                   CLEAR                   TIBS34DS
041600070105     C                   CLEAR                   AZUTEDS
041700070105     C                   CLEAR                   DDATIUTE
041800070105     C*
041900070105     C     *DTAARA       DEFINE    §azute        azuteds
042000070105     C     *DTAARA       DEFINE    §datiute      ddatiute
042100070105     C                   IN(E)     *DTAARA
042200070105     C                   IF        %Error
042300070105     c                   EVAL      I34Tla = 'L'
042400070105     C                   CALL      'TIBS34R'
042500070105     C                   PARM                    Tibs34Ds
042600070105     C                   IN        *DTAARA
042700070105     C                   ENDIF
042800070105     C*
042900070105     C                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100000000     C* *INZSR - OOPERAZIONI INIZIALI
043200000000     C*------------------------------------------------------------------------*
043300000000     C     *INZSR        BEGSR
043400000000     C*--------------------
043500000000     C* RICEVIMENTO PARAMETRI
043600000000     C*--------------------
043700000000     C     *ENTRY        PLIST
043800000000     C                   PARM                    KPJBA
043900070105     C*
044000070105     C* REPERIMENTO DATI UTENTE
044100070105     C                   EXSR      REPDATIUTE
044200070220     C*
044300070220     C* CALCOLA LA DATA CORRENTE
044400070220     C                   time                    wn14             14 0
044500070220     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044600070220     C                   z-add     wn8           g08dat
044700070220     C                   z-add     *zeros        g08inv
044800070220     C                   movel     '0'           g08err
044900070220     C                   call      'XSRDA8'
045000070220     C                   parm                    wlbda8
045100070220     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
045200070220     C                   z-add     g08dat        datcorinv         8 0          *DATA CORRENTE GG/M/
045300070105     C*
045400070105     C* Eseguo considerazioni sul profilo utente corrente
045500070105     C                   CLEAR                   TIBS36DS
045600070105     C                   EVAL      I36UTE = KNMUS
045700070105     C                   CALL      'TIBS36R'
045800070105     C                   PARM                    TIBS36DS
045900070105     C*
046000070202     C* CARICO TABELLA FILIALI GESTITE £1
046100070105     C                   clear                   trul06ds
046200070202     C                   move      '£1'          d06cod
046300070105     C                   movel     o36tfp        d06key
046400070105     C                   movel     trul06ds      kpjbu
046500070105     C                   call      'TRUL06R'
046600070105     C                   parm                    kpjba
046700070105     C                   movel     kpjbu         trul06ds
046800070105     C*
046900070105     C                   move      o36tfp        wo36tfp           3 0
047000070105     C*
047100070105     C* DEFINIZIONE CHIAVI DI ACCESSO AI FILE
047200070105     C*
047300070105     C* FNFVV01L - Completa
047400070105     C     KEYfvv01_C    KLIST
047500070105     C                   KFLD                    categ             1 0
047600070105     C                   KFLD                    dist5             5 0
047700070105     C                   KFLD                    VIDFGS
047800070105     C                   z-add     4             categ
047900070105     C*
048000000000     C*--------------------
048100060829     C* VARIABILI NN RIFERITE AL DATABASE
048200000000     C*--------------------
048300000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
048400000000     C*
048500000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
048600000000     C     XMSGPA        PLIST
048700000000     C                   PARM                    DSMSMS
048800000000     C                   PARM                    DSMSPG
048900000000     C                   PARM                    DSMSKE
049000000000     C                   PARM                    DSMSER
049100000000     C*
049200000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
049300000000     C                   CALL      'XKEY'
049400000000     C                   PARM                    DSKEY
049500000000     C*
049600000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
049700000000     C*   -NOME PGM CHIAMANTE
049800000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
049900071001     C                   MOVEL     'FIOR79R'     DSMSPG
050000071001     C                   MOVEL     'FIOR79R'     MSGQUE
050100000000     C                   MOVEL     *BLANKS       MSGKEY
050200000000     C                   MOVEL     *BLANKS       DSMSMS
050300000000     C*
050400000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
050500000000     C                   Z-ADD     0             FUNGRU            5 0
050600000000     C*
050700000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
050800000000     C                   EXSR      FUNGES
050900000000     C*--------------------
051000000000     C* VARIABILI DI CONTROLLO
051100000000     C*--------------------
051200000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
051300000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
051400000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
051500000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
051600000000     C*
051700000000     C* PULISCE IL VIDEO DA EMETTERE
051800000000     C                   EXSR      AZZVID
051900000000     C*
052000000000     C                   ENDSR
052100000000     C*------------------------------------------------------------------------*
052200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
052300000000     C*------------------------------------------------------------------------*
052400000000     C     FUNGES        BEGSR
052500000000     C*
052600000000     C* PULISCO CAMPI DI LAVORO
052700000000     C                   Z-ADD     0             W                 5 0
052800000000     C*
052900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
053000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
053100000000     C     1             DO        24            I
053200000000     C                   MOVEL     FUD(I)        FUDRIG
053300000000     C*
053400000000     C* PULIZIA FUNZIONI PRECEDENTI
053500000000     C                   MOVEL     *BLANKS       FUV(I)
053600000000     C*
053700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
053800000000     C* (SALTA F24)
053900000000     C     FUDATT        IFEQ      'S'
054000000000     C     FUDFUN        ANDNE     '24'
054100000000     C                   ADD       1             W
054200000000     C                   MOVEL     FUDDES        FUV(W)
054300000000     C                   END
054400000000     C                   END                                                    DO WHILE
054500000000     C*
054600000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
054700000000     C* ALTRIMENTI DISATTIVO F24
054800000000     C     1             DO        24            I
054900000000     C                   MOVEL     FUD(I)        FUDRIG
055000000000     C*
055100000000     C     FUDFUN        IFEQ      '24'
055200000000     C     W             IFGT      6
055300000000     C                   MOVEL     'S'           FUDATT
055400000000     C                   ELSE
055500000000     C                   MOVEL     *BLANKS       FUDATT
055600000000     C                   END                                                    W GT 8
055700000000     C                   MOVEL     FUDRIG        FUD(I)
055800000000     C                   END
055900000000     C                   END                                                    DO WHILE
056000000000     C*
056100000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
056200000000     C     W             DIV       6             I
056300000000     C                   MVR                     Z
056400000000     C*
056500000000     C     Z             IFNE      0
056600000000     C     I             ADD       1             Z
056700000000     C                   ELSE
056800000000     C                   Z-ADD     I             Z                 5 0
056900000000     C                   END
057000000000     C*
057100000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
057200000000     C                   ADD       1             FUNGRU
057300000000     C     FUNGRU        IFGT      Z
057400000000     C     FUNGRU        ORLT      1
057500000000     C                   Z-ADD     1             FUNGRU
057600000000     C                   END
057700000000     C*
057800000000     C* PRIMO GRUPPO DI 6 FUNZIONI
057900000000     C     FUNGRU        IFEQ      1
058000000000     C                   MOVEL     FUV(1)        FUN001
058100000000     C                   MOVEL     FUV(2)        FUN002
058200000000     C                   MOVEL     FUV(3)        FUN003
058300000000     C                   MOVEL     FUV(4)        FUN004
058400000000     C                   MOVEL     FUV(5)        FUN005
058500000000     C                   MOVEL     FUV(6)        FUN006
058600000000     C                   END
058700000000     C*
058800000000     C* SECONDO GRUPPO DI 6 FUNZIONI
058900000000     C     FUNGRU        IFEQ      2
059000000000     C                   MOVEL     FUV(7)        FUN001
059100000000     C                   MOVEL     FUV(8)        FUN002
059200000000     C                   MOVEL     FUV(9)        FUN003
059300000000     C                   MOVEL     FUV(10)       FUN004
059400000000     C                   MOVEL     FUV(11)       FUN005
059500000000     C                   MOVEL     FUV(12)       FUN006
059600000000     C                   END
059700000000     C*
059800000000     C* TERZO GRUPPO DI 6 FUNZIONI
059900000000     C     FUNGRU        IFEQ      3
060000000000     C                   MOVEL     FUV(13)       FUN001
060100000000     C                   MOVEL     FUV(14)       FUN002
060200000000     C                   MOVEL     FUV(15)       FUN003
060300000000     C                   MOVEL     FUV(16)       FUN004
060400000000     C                   MOVEL     FUV(17)       FUN005
060500000000     C                   MOVEL     FUV(18)       FUN006
060600000000     C                   END
060700000000     C*
060800000000     C* QUARTO GRUPPO DI 6 FUNZIONI
060900000000     C     FUNGRU        IFEQ      4
061000000000     C                   MOVEL     FUV(19)       FUN001
061100000000     C                   MOVEL     FUV(20)       FUN002
061200000000     C                   MOVEL     FUV(21)       FUN003
061300000000     C                   MOVEL     FUV(22)       FUN004
061400000000     C                   MOVEL     *BLANKS       FUN005
061500000000     C                   MOVEL     *BLANKS       FUN006
061600000000     C                   END
061700000000     C*
061800000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
061900000000     C                   MOVEL     *BLANKS       FUN007
062000000000     C                   MOVEL     *BLANKS       FUN008
062100000000     C*
062200000000     C     1             DO        24            I
062300000000     C                   MOVEL     FUD(I)        FUDRIG
062400000000     C     FUDFUN        IFEQ      '24'
062500000000     C     FUDATT        ANDEQ     'S'
062600000000     C                   MOVEL     FUDDES        FUN008
062700000000     C                   END
062800000000     C                   END                                                    DO
062900000000     C*
063000000000     C                   ENDSR
063100000000     C*------------------------------------------------------------------------*
063200000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
063300000000     C*------------------------------------------------------------------------*
063400000000     C     FUNCON        BEGSR
063500000000     C*
063600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
063700000000     C                   EXSR      FUNVAR
063800000000     C*
063900000000     C* PULISCE CAMPI DI LAVORO
064000000000     C                   Z-ADD     0             I                 5 0
064100000000     C                   Z-ADD     0             W                 5 0
064200000000     C*
064300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
064400000000IF  1C     FUNKEY        IFNE      *BLANKS
064500000000DO  2C     1             DO        24            I
064600000000     C                   MOVEL     FUD(I)        FUDRIG
064700000000     C*
064800000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
064900000000IF  3C     FUNKEY        IFEQ      FUDFUN
065000000000     C     FUDATT        ANDEQ     'S'
065100000000     C                   Z-ADD     I             W
065200000000E   3C                   ENDIF
065300000000E   2C                   ENDDO
065400000000     C*
065500000000     C* FUNZIONE NON ATTIVA, ERRORE
065600000000IF  2C     W             IFEQ      0
065700000000     C                   MOVEL     MSG(1)        DSMSMS
065800000000E   2C                   ENDIF
065900000000E   1C                   ENDIF
066000000000     C*
066100000000     C* FUNZIONE ATTIVA, PROCEDE
066200000000IF  1C     W             IFNE      0
066300000000     C     FUNKEY        OREQ      *BLANKS
066400000000IF  2C     FUNKEY        IFEQ      *BLANKS
066500000000     C                   EXSR      ENTGES
066600000000E   2C                   ENDIF
066700000000IF  2C     FUNKEY        IFEQ      '03'
066800000000     C                   EXSR      F03GES                                       *FINE LAVORO
066900000000E   2C                   ENDIF
067000000000IF  2C     FUNKEY        IFEQ      '06'
067100000000     C                   EXSR      F06GES                                       *LANCIO
067200000000E   2C                   ENDIF
067300000000IF  2C     FUNKEY        IFEQ      '12'
067400000000     C                   EXSR      F12GES                                       *RITORNO
067500000000E   2C                   ENDIF
067600000000IF  2C     FUNKEY        IFEQ      '24'
067700000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
067800000000E   2C                   ENDIF
067900000000E   1C                   ENDIF
068000000000     C*
068100000000     C                   ENDSR
068200000000     C*------------------------------------------------------------------------*
068300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
068400000000     C*------------------------------------------------------------------------*
068500000000     C     FUNVAR        BEGSR
068600000000     C*
068700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
068800000000     C*
068900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
069000000000     C     KEY           IFEQ      ENTER
069100000000     C                   MOVEL     *BLANKS       FUNKEY
069200000000     C                   END
069300000000     C     KEY           IFEQ      F1
069400000000     C                   MOVEL     '01'          FUNKEY
069500000000     C                   END
069600000000     C     KEY           IFEQ      F2
069700000000     C                   MOVEL     '02'          FUNKEY
069800000000     C                   END
069900000000     C     KEY           IFEQ      F3
070000000000     C                   MOVEL     '03'          FUNKEY
070100000000     C                   END
070200000000     C     KEY           IFEQ      F4
070300000000     C                   MOVEL     '04'          FUNKEY
070400000000     C                   END
070500000000     C     KEY           IFEQ      F5
070600000000     C                   MOVEL     '05'          FUNKEY
070700000000     C                   END
070800000000     C     KEY           IFEQ      F6
070900000000     C                   MOVEL     '06'          FUNKEY
071000000000     C                   END
071100000000     C     KEY           IFEQ      F7
071200000000     C                   MOVEL     '07'          FUNKEY
071300000000     C                   END
071400000000     C     KEY           IFEQ      F8
071500000000     C                   MOVEL     '08'          FUNKEY
071600000000     C                   END
071700000000     C     KEY           IFEQ      F9
071800000000     C                   MOVEL     '09'          FUNKEY
071900000000     C                   END
072000000000     C     KEY           IFEQ      F10
072100000000     C                   MOVEL     '10'          FUNKEY
072200000000     C                   END
072300000000     C     KEY           IFEQ      F11
072400000000     C                   MOVEL     '11'          FUNKEY
072500000000     C                   END
072600000000     C     KEY           IFEQ      F12
072700000000     C                   MOVEL     '12'          FUNKEY
072800000000     C                   END
072900000000     C     KEY           IFEQ      F13
073000000000     C                   MOVEL     '13'          FUNKEY
073100000000     C                   END
073200000000     C     KEY           IFEQ      F14
073300000000     C                   MOVEL     '14'          FUNKEY
073400000000     C                   END
073500000000     C     KEY           IFEQ      F15
073600000000     C                   MOVEL     '15'          FUNKEY
073700000000     C                   END
073800000000     C     KEY           IFEQ      F16
073900000000     C                   MOVEL     '16'          FUNKEY
074000000000     C                   END
074100000000     C     KEY           IFEQ      F17
074200000000     C                   MOVEL     '17'          FUNKEY
074300000000     C                   END
074400000000     C     KEY           IFEQ      F18
074500000000     C                   MOVEL     '18'          FUNKEY
074600000000     C                   END
074700000000     C     KEY           IFEQ      F19
074800000000     C                   MOVEL     '19'          FUNKEY
074900000000     C                   END
075000000000     C     KEY           IFEQ      F20
075100000000     C                   MOVEL     '20'          FUNKEY
075200000000     C                   END
075300000000     C     KEY           IFEQ      F21
075400000000     C                   MOVEL     '21'          FUNKEY
075500000000     C                   END
075600000000     C     KEY           IFEQ      F22
075700000000     C                   MOVEL     '22'          FUNKEY
075800000000     C                   END
075900000000     C     KEY           IFEQ      F23
076000000000     C                   MOVEL     '23'          FUNKEY
076100000000     C                   END
076200000000     C     KEY           IFEQ      F24
076300000000     C                   MOVEL     '24'          FUNKEY
076400000000     C                   END
076500000000     C     KEY           IFEQ      ROLLUP
076600000000     C                   MOVEL     'RU'          FUNKEY
076700000000     C                   END
076800000000     C     KEY           IFEQ      HELP
076900000000     C                   MOVEL     'HP'          FUNKEY
077000000000     C                   END
077100000000     C                   ENDSR
077200000000     C*------------------------------------------------------------------------*
077300000000     C* NOTE PER COMPILAZIONE SCHIERE
077400000000     C*------------------------------------------------------------------------*
077500000000     C* -MSG-
077600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
077700000000     C* (NON TOCCARE MSG,1)
077800000000     C*
077900000000     C* -CMD-
078000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
078100000000     C*
078200000000     C* -FUD-
078300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
078400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
078500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
078600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
078700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
078800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
078900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
079000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
079100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
079200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
079300000000     C*
079400000000     C*------------------------------------------------------------------------*
079500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
079600071001FIOR79R- Tasto funzionale non gestito                                          1
079700071001FIOR79R- P.O. inesistente                                                      2
079800071001FIOR79R- P.O. non in gestione                                                  3
079900071001FIOR79R- P.O. obbligatorio                                                     4
080000071001FIOR79R- Indicare o data distinte o numero distinta                            5
080100071001FIOR79R- Distinta errata, chiusa o non disponibile                             6
080200071001FIOR79R- Per bolle non in distinta possibile solo layout "G"                   7
080300071001FIOR79R- Livello di dettaglio previsto solo per layout "DG"                    8
080400071009FIOR79R- P.O. non abilitato alla funzione                                      9
080500071001FIOR79R-                                                                      10
080600071001FIOR79R-                                                                      11
080700071001FIOR79R-                                                                      12
080800071001FIOR79R-                                                                      13
080900071001FIOR79R-                                                                      14
081000071001FIOR79R- Numero distinta non valido                                           15
081100071001FIOR79R- Data non valida                                                      16
081200071001FIOR79R-                                                                      17
081300071001FIOR79R-                                                                      18
081400071001FIOR79R-                                                                      19
081500071001FIOR79R-                                                                      20
081600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
081700070605F01=Aiuto          -        (S/N)=N                        funz.01¬
081800000000                   -        (S/N)=N                        funz.02¬
081900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
082000000000                   -        (S/N)=N                        funz.04¬
082100000000                   -        (S/N)=N                        funz.05¬
082200060829F06=Lancio         -        (S/N)=S  <== S-FISSO           funz.06¬
082300000000                   -        (S/N)=N                        funz.07¬
082400000000                   -        (S/N)=N                        funz.08¬
082500000000                   -        (S/N)=N                        funz.09¬
082600060829                   -        (S/N)=N                        funz.10¬
082700000000                   -        (S/N)=N                        funz.11¬
082800060830                   -        (S/N)=N                        funz.12¬
082900000000                   -        (S/N)=N                        funz.13¬
083000000000                   -        (S/N)=N                        funz.14¬
083100000000                   -        (S/N)=N                        funz.15¬
083200000000                   -        (S/N)=N                        funz.16¬
083300000000                   -        (S/N)=N                        funz.17¬
083400000000                   -        (S/N)=N                        funz.18¬
083500000000                   -        (S/N)=N                        funz.19¬
083600000000                   -        (S/N)=N                        funz.20¬
083700000000                   -        (S/N)=N                        funz.21¬
083800060829                   -        (S/N)=N                        funz.22¬
083900000000                   -        (S/N)=N                        funz.23¬
084000060830                   -        (S/N)=N                        funz.24¬
