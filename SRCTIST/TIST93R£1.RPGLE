000100000000     H*------------------------------------------------------------------------*
000200070105     H* CONTROLLO ASSORBIMENTI X GIRI O DISTINTE/GIRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800070105     FTIST93D   CF   E             WORKSTN INFDS(DEVDS1)
000900070105     F                                     SFILE(ST93DMS:MSGULT)
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
007301070223     D*-------------------
007302070223     D* DESCRIZIONI ORGANIGRAMMA
007303070223     D*-------------------
007304070223     D OG146         E DS
007400070105     D*-------------------
007500070105     D* DS REPERIMENTO DATI UTENTE
007600070105     D*-------------------
007700070105     D TIBS34DS      E DS                                                       *Profili utente
007800070105     D DDATIUTE      E DS                                                       *Dati utente
007900070105     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008000070105     D*-------------------
008100070202     D* DS PER TRUL06R - CARICAMENTO £1/£6
008200070105     D*-------------------
008300070105     D TRUL06DS      E DS
008400070105     D  Lin                    1     90  0
008500070105     D                                     DIM(30)                              SKI COMODO
008600070105     D TIBS36DS      E DS
008700070105     D DSLR01        E DS
008800001016     D*-------------------
008900001016     D* DS PARAMETRI DI LANCIO PGM
009000001016     D*-------------------
009100060829     D PARAM           DS                  INZ
009200070105     D  In_FGS                        3  0
009300070105     D  In_DFV                        8  0
009400070105     D  In_NFV                        7  0
009500070129     D  In_LIV                        1
009600070105     D  In_TIP                        2
009601070223     D  In_PIG                        1
009700000000     C*------------------------------------------------------------------------*
009800000000     C* MAIN LINE
009900000000     C*------------------------------------------------------------------------*
010000000000     C*
010100000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
010200000000IF  1C     ERRL00        IFEQ      '0'
010300000000     C*
010400000000     C* CICLO FINO A CHE:
010500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
010600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
010700000000DO  2C     F03L00        DOUEQ     '1'
010800000000     C     F12L00        OREQ      '1'
010900000000     C*
011000000000     C* EMETTE FORMATO VIDEO
011100000000     C                   EXSR      BUTFMR
011200000000     C*
011300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
011400000000     C                   EXSR      FUNCON
011500000000     C*
011600000000E   2C                   ENDDO
011700000000E   1C                   ENDIF
011800000000     C*
011900000000     C                   SETON                                        LR
012000000000     C*------------------------------------------------------------------------*
012100000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
012200000000     C*------------------------------------------------------------------------*
012300000000     C     BUTFMR        BEGSR
012400000000     C*
012500000000     C* EMETTE TESTATA
012600070105     C                   WRITE     ST93D01
012700000000     C*
012800000000     C* VISUALIZZA MESSAGGIO
012900000000     C                   EXSR      SNDMSG
013000000000     C*
013100000000     C* EMETTE FUNZIONI
013200070105     C                   WRITE     ST93D02
013300000000     C*
013400000000     C* EMETTE FORMATO VIDEO
013500070105     C                   EXFMT     ST93D03
013600000000     C*
013700000000     C* AZZERA FLAG DI ERRORE LIVELLO
013800000000     C                   MOVEL     '0'           ERRL00
013900000000     C*
014000000000     C* AZZERA INDICATORI DI ERRORE
014100070105     C                   SETOFF                                       707172
014200070202     C                   SETOFF                                       73
014300000000     C*
014400000000     C                   ENDSR
014500000000     C*------------------------------------------------------------------------*
014600000000     C* SNDMSG - MANDA MESSAGGIO
014700000000     C*------------------------------------------------------------------------*
014800000000     C     SNDMSG        BEGSR
014900000000     C*
015000000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
015100000000     C                   MOVEL     '1'           *IN40
015200070105     C                   WRITE     ST93DMC
015300000000     C                   MOVEL     '0'           *IN40
015400000000     C*
015500000000     C* VISUALIZZAZIONE MESSAGGIO
015600000000     C                   Z-ADD     0             MSGULT            4 0
015700000000     C                   CALL      'XMSG'        XMSGPA
015800000000     C     DSMSER        IFEQ      0
015900000000     C                   MOVEL     DSMSKE        MSGKEY
016000000000     C                   ENDIF
016100000000     C                   Z-ADD     1             MSGULT
016200070105     C                   WRITE     ST93DMS                                      SFL MESS
016300070105     C                   WRITE     ST93DMC                                      CONTR MESS
016400000000     C*
016500000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
016600000000     C                   MOVEL     *BLANKS       DSMSMS
016700000000     C*
016800000000     C                   ENDSR
016900000000     C*------------------------------------------------------------------------*
017000000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
017100000000     C*------------------------------------------------------------------------*
017200000000     C     ENTGES        BEGSR
017300000000     C*
017400000000     C* CONTROLLA I CAMPI IMMESSI
017500000000     C                   EXSR      DECODI
017600000000     C*
017700000000     C                   ENDSR
017800000000     C*------------------------------------------------------------------------*
017900000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
018000000000     C*------------------------------------------------------------------------*
018100000000     C     F03GES        BEGSR
018200000000     C*
018300000000     C* IMPOSTA FLAG F03
018400000000     C                   MOVEL     '1'           F03L00
018500000000     C*
018600000000     C                   ENDSR
018700000000     C*------------------------------------------------------------------------*
018800000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=LANCIO
018900000000     C*------------------------------------------------------------------------*
019000000000     C     F06GES        BEGSR
019100000000     C*
019200000000     C* CONTROLLA I CAMPI IMMESSI
019300000000     C                   EXSR      DECODI
019400000000     C*
019500000000     C* LANCIA IL PROGRAMMA DI STAMPA
019600000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORE
019700000000     C*
019800000000     C                   EXSR      VIDPAR
019900010417     C                   MOVEL(p)  PARAM         KPJBU
020000070105     C                   MOVEL     'ST94'        KCOAZ
020100070215     C                   CALL      'BCH10'
020200070215 MOM C***                CALL      'TIST94R'
020300000000     C                   PARM                    KPJBA
020400060829     C*
020500060829     C* QUINDI ESCO DAL PROGRAMMA
020600060829     C                   MOVEL     '1'           F03L00
020700000000E   1C                   ENDIF
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*------------------------------------------------------------------------*
021100000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
021200000000     C*------------------------------------------------------------------------*
021300000000     C     F12GES        BEGSR
021400000000     C*
021500000000     C* IMPOSTO FLAG F12
021600000000     C                   MOVEL     '1'           F12L00
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRE FUNZIONI
022100000000     C*------------------------------------------------------------------------*
022200000000     C     F24GES        BEGSR
022300000000     C*
022400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
022500000000     C                   EXSR      FUNGES
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900070105     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023000000000     C*------------------------------------------------------------------------*
023100000000     C     DECODI        BEGSR
023200060830     C*
023300060830     C* AZZERA INDICATORI DI ERRORE
023400070105     C                   SETOFF                                       707172
023500070202     C                   SETOFF                                       73
023600070105     C*--------------------
023700070105     C* P.O. GESTIONE
023800070105     C*--------------------
023900070105     C     ERRL00        IFEQ      '0'
024000070105     C     VIDFGS        CHAIN     AZORG01L
024100070105     C                   IF        %found(AZORG01L)
024101070223     C                   MOVEL     ORGDE6        OG146
024200070105     C                   ELSE
024300070105     C                   MOVEL     '1'           ERRL00
024400070105     C                   MOVEL     MSG(02)       DSMSMS
024500070105     C                   SETON                                        70
024600070105     C                   ENDIF
024700070105     C                   ENDIF
024800070105     C*
024900070105     C     ERRL00        IFEQ      '0'
025000070105     C     WO36TFP       IFNE      *zeros
025100070105     C     VIDFGS        LOOKUP    Lin                                    99
025200070105     C                   IF        *IN99 = *on
025300070105     C                   ELSE
025400070105     C                   MOVEL     '1'           ERRL00
025500070105     C                   MOVEL     MSG(03)       DSMSMS
025600070105     C                   SETON                                        70
025700070105     C                   ENDIF
025800070105     C                   ENDIF
025900070105     C                   ENDIF
026000070105     C*
026100070105     C     ERRL00        IFEQ      '0'
026200070105     C     VIDFGS        IFEQ      *zeros
026300070105     C                   MOVEL     '1'           ERRL00
026400070105     C                   MOVEL     MSG(04)       DSMSMS
026500070105     C                   SETON                                        70
026600070105     C                   ENDIF
026700070105     C                   ENDIF
026701070223     C*--------------------
026702070223     C* VERIFICA PIANIFICAZIONE GIRI CONSEGNA "RISPETTO A"
026703070223     C*--------------------
026704070223     C     ERRL00        IFEQ      '0'
026705070223     C                   IF        §OGDDA = *blanks
026706070223     C                   EVAL      VIDPIG = 'A'
026707070223     C                   ENDIF
026709070223     C                   ENDIF
026800000000     C*--------------------
026900070105     C* DATA DISTINTE
027000000000     C*--------------------
027100001016     C     ERRL00        IFEQ      '0'
027200070105     C     VIDDTA        IFNE      *zeros
027300070105     C                   Z-ADD     VIDDTA        G08DAT
027400000000     C                   Z-ADD     *ZEROS        G08INV
027500000000     C                   MOVEL     '0'           G08ERR
027600000000     C                   CALL      'XSRDA8'
027700971128     C                   PARM                    WLBDA8
027800070105     C                   Z-ADD     G08DAT        VIDDTA
027900001016     C     G08ERR        IFEQ      '0'
028000070105     C                   Z-ADD     G08INV        WDTA              8 0
028100001016     C                   ELSE
028200070125     C                   MOVEL     '1'           ERRL00
028300000000     C                   MOVEL     MSG(16)       DSMSMS
028400070105     C                   SETON                                        71
028500070105     C                   ENDIF
028600001016     C                   ENDIF
028700001016     C                   ENDIF
028800001110     C*--------------------
028900070105     C* NUMERO DISTINTA
029000001110     C*--------------------
029100001110     C     ERRL00        IFEQ      '0'
029200070105     C     VIDNDC        IFNE      *zeros
029300070105     C     VIDNDC        ANDNE     *blanks
029400070105     C     '?'           scan      VIDNDC                                 86
029500070105     C     *in86         ifeq      *on
029600070105     C                   clear                   dslr01
029700070105     C                   movel     'TIST93R'     dlrpgm
029800070105     C                   movel     *blanks       dlrtfv                         *sia open che close
029900070105     C                   z-add     4             dlrnpg
030000070105     C                   z-add     VIDFGS        dlrfgs
030100070220     C                   movel     'S'           dlrric
030200070222     C*
030300070222     C                   if        viddta = *zeros
030400070222     C                   eval      dlrgda = datcorinv - 10000
030500070222     C                   else
030600070222     C                   eval      dlrgda = viddta
030700070222     C                   eval      dlrgal = viddta
030800070222     C                   endif
030900070222     C*
031000070105     C                   movel(p)  dslr01        kpjbu
031100070105     C                   call      'FNLR02R'
031200070105     C                   parm                    kpjba
031300070105     C                   movel     kpjbu         dslr01
031400070105     C                   move      dlrnfv        VIDNDC
031500070105     C                   endif
031600070105     C                   endif
031700070105     C*
031800070105     C                   testn                   VIDNDC               86
031900070105     C   86              move      VIDNDC        vidndcn           7 0
032000070105     C* Se immesso il Nr distinta verifica che sia disponibile
032100070105     C     vidndcn       ifgt      *zero
032200070105     C                   z-add     vidndcn       dist5
032300070105     C     KEYfvv01_C    chain     fnfvv01l                           88
032400070105     C                   if        *in88 or fvvfcf<>*blanks or fvvatb<>*blanks
032500070105     C                   MOVEL     '1'           ERRL00
032600070105     C                   MOVEL     MSG(15)       DSMSMS
032700070105     C                   SETON                                        72
032800070105     C                   endif
032900070105     C                   endif
033000070105     C                   endif
033100070105     C*--------------------
033200070105     C* OBBLIGATORIETA' SELEZIONI
033300070105     C*--------------------
033400070105     C     ERRL00        IFEQ      '0'
033500070105     C                   IF        VIDDTA  = *zeros AND
033600070125     C                             (VIDNDC = *zeros OR VIDNDC = *blanks) AND
033700070125     C                             VIDTIP <> 'G'
033800070129     C                   MOVEL     '1'           ERRL00
033900070129     C                   MOVEL     MSG(07)       DSMSMS
034000070129     C                   SETON                                        7173
034100070105     C                   ENDIF
034200070129     C*
034300070129     C                   IF        VIDLIV  = 'T' AND
034400070129     C                             VIDTIP <> 'DG'
034500070129     C                   MOVEL     '1'           ERRL00
034600070129     C                   MOVEL     MSG(08)       DSMSMS
034700070129     C                   SETON                                          73
034800070129     C                   ENDIF
034900070105     C*
035000070105     C                   IF        VIDDTA  <> *zeros AND
035100070105     C                             (VIDNDC <>  *zeros AND VIDNDC <> *blanks)
035200070105     C                   MOVEL     '1'           ERRL00
035300070105     C                   MOVEL     MSG(05)       DSMSMS
035400070105     C                   SETON                                        7172
035500070105     C                   ENDIF
035600070105     C                   ENDIF
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
036100000000     C*------------------------------------------------------------------------*
036200000000     C     AZZVID        BEGSR
036300010417     C*
036400010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
036500070105     C                   CLEAR                   ST93D03
036600070105     C*
036700070105     C                   EVAL      VIDFGS = O36POU
036800070129     C                   EVAL      VIDLIV = 'S'
036900070105     C                   EVAL      VIDTIP = 'G'
036901070223     C                   EVAL      VIDPIG = 'D'
037000000000     C*
037100000000     C                   ENDSR
037200000000     C*------------------------------------------------------------------------*
037300001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
037400000000     C*------------------------------------------------------------------------*
037500000000     C     VIDPAR        BEGSR
037600000000     C*
037700070105     C                   Z-ADD     VIDFGS        In_FGS
037800070105     C                   Z-ADD     WDTA          In_DFV
037900070105     C                   MOVE(P)   VIDNDC        In_NFV
038000070129     C                   MOVEL(P)  VIDLIV        In_LIV
038100070105     C                   MOVEL(P)  VIDTIP        In_TIP
038101070223     C                   MOVEL(P)  VIDPIG        In_PIG
038200001016     C*
038300000000     C                   ENDSR
038400070105     C*--------------------------------------------------------------------------------------------*
038500070105     C* REPERISCE I DATI UTENTE
038600070105     C*--------------------------------------------------------------------------------------------*
038700070105     C     REPDATIUTE    BEGSR
038800070105     C*
038900070105     C* INIZIALIZZA VARIABILI DI WRK
039000070105     C                   CLEAR                   TIBS34DS
039100070105     C                   CLEAR                   AZUTEDS
039200070105     C                   CLEAR                   DDATIUTE
039300070105     C*
039400070105     C     *DTAARA       DEFINE    §azute        azuteds
039500070105     C     *DTAARA       DEFINE    §datiute      ddatiute
039600070105     C                   IN(E)     *DTAARA
039700070105     C                   IF        %Error
039800070105     c                   EVAL      I34Tla = 'L'
039900070105     C                   CALL      'TIBS34R'
040000070105     C                   PARM                    Tibs34Ds
040100070105     C                   IN        *DTAARA
040200070105     C                   ENDIF
040300070105     C*
040400070105     C                   ENDSR
040500000000     C*------------------------------------------------------------------------*
040600000000     C* *INZSR - OOPERAZIONI INIZIALI
040700000000     C*------------------------------------------------------------------------*
040800000000     C     *INZSR        BEGSR
040900000000     C*--------------------
041000000000     C* RICEVIMENTO PARAMETRI
041100000000     C*--------------------
041200000000     C     *ENTRY        PLIST
041300000000     C                   PARM                    KPJBA
041400070105     C*
041500070105     C* REPERIMENTO DATI UTENTE
041600070105     C                   EXSR      REPDATIUTE
041700070220     C*
041800070220     C* CALCOLA LA DATA CORRENTE
041900070220     C                   time                    wn14             14 0
042000070220     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
042100070220     C                   z-add     wn8           g08dat
042200070220     C                   z-add     *zeros        g08inv
042300070220     C                   movel     '0'           g08err
042400070220     C                   call      'XSRDA8'
042500070220     C                   parm                    wlbda8
042600070220     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
042700070220     C                   z-add     g08dat        datcorinv         8 0          *DATA CORRENTE GG/M/
042800070105     C*
042900070105     C* Eseguo considerazioni sul profilo utente corrente
043000070105     C                   CLEAR                   TIBS36DS
043100070105     C                   EVAL      I36UTE = KNMUS
043200070105     C                   CALL      'TIBS36R'
043300070105     C                   PARM                    TIBS36DS
043400070105     C*
043500070202     C* CARICO TABELLA FILIALI GESTITE £1
043600070105     C                   clear                   trul06ds
043700070202     C                   move      '£1'          d06cod
043800070105     C                   movel     o36tfp        d06key
043900070105     C                   movel     trul06ds      kpjbu
044000070105     C                   call      'TRUL06R'
044100070105     C                   parm                    kpjba
044200070105     C                   movel     kpjbu         trul06ds
044300070105     C*
044400070105     C                   move      o36tfp        wo36tfp           3 0
044500070105     C*
044600070105     C* DEFINIZIONE CHIAVI DI ACCESSO AI FILE
044700070105     C*
044800070105     C* FNFVV01L - Completa
044900070105     C     KEYfvv01_C    KLIST
045000070105     C                   KFLD                    categ             1 0
045100070105     C                   KFLD                    dist5             5 0
045200070105     C                   KFLD                    VIDFGS
045300070105     C                   z-add     4             categ
045400070105     C*
045500000000     C*--------------------
045600060829     C* VARIABILI NN RIFERITE AL DATABASE
045700000000     C*--------------------
045800000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
045900000000     C*
046000000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
046100000000     C     XMSGPA        PLIST
046200000000     C                   PARM                    DSMSMS
046300000000     C                   PARM                    DSMSPG
046400000000     C                   PARM                    DSMSKE
046500000000     C                   PARM                    DSMSER
046600000000     C*
046700000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
046800000000     C                   CALL      'XKEY'
046900000000     C                   PARM                    DSKEY
047000000000     C*
047100000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
047200000000     C*   -NOME PGM CHIAMANTE
047300000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
047400070105     C                   MOVEL     'TIST93R'     DSMSPG
047500070105     C                   MOVEL     'TIST93R'     MSGQUE
047600000000     C                   MOVEL     *BLANKS       MSGKEY
047700000000     C                   MOVEL     *BLANKS       DSMSMS
047800000000     C*
047900000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
048000000000     C                   Z-ADD     0             FUNGRU            5 0
048100000000     C*
048200000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
048300000000     C                   EXSR      FUNGES
048400000000     C*--------------------
048500000000     C* VARIABILI DI CONTROLLO
048600000000     C*--------------------
048700000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
048800000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
048900000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
049000000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
049100000000     C*
049200000000     C* PULISCE IL VIDEO DA EMETTERE
049300000000     C                   EXSR      AZZVID
049400000000     C*
049500000000     C                   ENDSR
049600000000     C*------------------------------------------------------------------------*
049700000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
049800000000     C*------------------------------------------------------------------------*
049900000000     C     FUNGES        BEGSR
050000000000     C*
050100000000     C* PULISCO CAMPI DI LAVORO
050200000000     C                   Z-ADD     0             W                 5 0
050300000000     C*
050400000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
050500000000     C* E CARICO TABELLA FUNZIONI ATTIVE
050600000000     C     1             DO        24            I
050700000000     C                   MOVEL     FUD(I)        FUDRIG
050800000000     C*
050900000000     C* PULIZIA FUNZIONI PRECEDENTI
051000000000     C                   MOVEL     *BLANKS       FUV(I)
051100000000     C*
051200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
051300000000     C* (SALTA F24)
051400000000     C     FUDATT        IFEQ      'S'
051500000000     C     FUDFUN        ANDNE     '24'
051600000000     C                   ADD       1             W
051700000000     C                   MOVEL     FUDDES        FUV(W)
051800000000     C                   END
051900000000     C                   END                                                    DO WHILE
052000000000     C*
052100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
052200000000     C* ALTRIMENTI DISATTIVO F24
052300000000     C     1             DO        24            I
052400000000     C                   MOVEL     FUD(I)        FUDRIG
052500000000     C*
052600000000     C     FUDFUN        IFEQ      '24'
052700000000     C     W             IFGT      6
052800000000     C                   MOVEL     'S'           FUDATT
052900000000     C                   ELSE
053000000000     C                   MOVEL     *BLANKS       FUDATT
053100000000     C                   END                                                    W GT 8
053200000000     C                   MOVEL     FUDRIG        FUD(I)
053300000000     C                   END
053400000000     C                   END                                                    DO WHILE
053500000000     C*
053600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
053700000000     C     W             DIV       6             I
053800000000     C                   MVR                     Z
053900000000     C*
054000000000     C     Z             IFNE      0
054100000000     C     I             ADD       1             Z
054200000000     C                   ELSE
054300000000     C                   Z-ADD     I             Z                 5 0
054400000000     C                   END
054500000000     C*
054600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
054700000000     C                   ADD       1             FUNGRU
054800000000     C     FUNGRU        IFGT      Z
054900000000     C     FUNGRU        ORLT      1
055000000000     C                   Z-ADD     1             FUNGRU
055100000000     C                   END
055200000000     C*
055300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
055400000000     C     FUNGRU        IFEQ      1
055500000000     C                   MOVEL     FUV(1)        FUN001
055600000000     C                   MOVEL     FUV(2)        FUN002
055700000000     C                   MOVEL     FUV(3)        FUN003
055800000000     C                   MOVEL     FUV(4)        FUN004
055900000000     C                   MOVEL     FUV(5)        FUN005
056000000000     C                   MOVEL     FUV(6)        FUN006
056100000000     C                   END
056200000000     C*
056300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
056400000000     C     FUNGRU        IFEQ      2
056500000000     C                   MOVEL     FUV(7)        FUN001
056600000000     C                   MOVEL     FUV(8)        FUN002
056700000000     C                   MOVEL     FUV(9)        FUN003
056800000000     C                   MOVEL     FUV(10)       FUN004
056900000000     C                   MOVEL     FUV(11)       FUN005
057000000000     C                   MOVEL     FUV(12)       FUN006
057100000000     C                   END
057200000000     C*
057300000000     C* TERZO GRUPPO DI 6 FUNZIONI
057400000000     C     FUNGRU        IFEQ      3
057500000000     C                   MOVEL     FUV(13)       FUN001
057600000000     C                   MOVEL     FUV(14)       FUN002
057700000000     C                   MOVEL     FUV(15)       FUN003
057800000000     C                   MOVEL     FUV(16)       FUN004
057900000000     C                   MOVEL     FUV(17)       FUN005
058000000000     C                   MOVEL     FUV(18)       FUN006
058100000000     C                   END
058200000000     C*
058300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
058400000000     C     FUNGRU        IFEQ      4
058500000000     C                   MOVEL     FUV(19)       FUN001
058600000000     C                   MOVEL     FUV(20)       FUN002
058700000000     C                   MOVEL     FUV(21)       FUN003
058800000000     C                   MOVEL     FUV(22)       FUN004
058900000000     C                   MOVEL     *BLANKS       FUN005
059000000000     C                   MOVEL     *BLANKS       FUN006
059100000000     C                   END
059200000000     C*
059300000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
059400000000     C                   MOVEL     *BLANKS       FUN007
059500000000     C                   MOVEL     *BLANKS       FUN008
059600000000     C*
059700000000     C     1             DO        24            I
059800000000     C                   MOVEL     FUD(I)        FUDRIG
059900000000     C     FUDFUN        IFEQ      '24'
060000000000     C     FUDATT        ANDEQ     'S'
060100000000     C                   MOVEL     FUDDES        FUN008
060200000000     C                   END
060300000000     C                   END                                                    DO
060400000000     C*
060500000000     C                   ENDSR
060600000000     C*------------------------------------------------------------------------*
060700000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
060800000000     C*------------------------------------------------------------------------*
060900000000     C     FUNCON        BEGSR
061000000000     C*
061100000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
061200000000     C                   EXSR      FUNVAR
061300000000     C*
061400000000     C* PULISCE CAMPI DI LAVORO
061500000000     C                   Z-ADD     0             I                 5 0
061600000000     C                   Z-ADD     0             W                 5 0
061700000000     C*
061800000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
061900000000IF  1C     FUNKEY        IFNE      *BLANKS
062000000000DO  2C     1             DO        24            I
062100000000     C                   MOVEL     FUD(I)        FUDRIG
062200000000     C*
062300000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
062400000000IF  3C     FUNKEY        IFEQ      FUDFUN
062500000000     C     FUDATT        ANDEQ     'S'
062600000000     C                   Z-ADD     I             W
062700000000E   3C                   ENDIF
062800000000E   2C                   ENDDO
062900000000     C*
063000000000     C* FUNZIONE NON ATTIVA, ERRORE
063100000000IF  2C     W             IFEQ      0
063200000000     C                   MOVEL     MSG(1)        DSMSMS
063300000000E   2C                   ENDIF
063400000000E   1C                   ENDIF
063500000000     C*
063600000000     C* FUNZIONE ATTIVA, PROCEDE
063700000000IF  1C     W             IFNE      0
063800000000     C     FUNKEY        OREQ      *BLANKS
063900000000IF  2C     FUNKEY        IFEQ      *BLANKS
064000000000     C                   EXSR      ENTGES
064100000000E   2C                   ENDIF
064200000000IF  2C     FUNKEY        IFEQ      '03'
064300000000     C                   EXSR      F03GES                                       *FINE LAVORO
064400000000E   2C                   ENDIF
064500000000IF  2C     FUNKEY        IFEQ      '06'
064600000000     C                   EXSR      F06GES                                       *LANCIO
064700000000E   2C                   ENDIF
064800000000IF  2C     FUNKEY        IFEQ      '12'
064900000000     C                   EXSR      F12GES                                       *RITORNO
065000000000E   2C                   ENDIF
065100000000IF  2C     FUNKEY        IFEQ      '24'
065200000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
065300000000E   2C                   ENDIF
065400000000E   1C                   ENDIF
065500000000     C*
065600000000     C                   ENDSR
065700000000     C*------------------------------------------------------------------------*
065800000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
065900000000     C*------------------------------------------------------------------------*
066000000000     C     FUNVAR        BEGSR
066100000000     C*
066200000000     C* INIZIALIZZA VARIABILE -FUNKEY-
066300000000     C*
066400000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
066500000000     C     KEY           IFEQ      ENTER
066600000000     C                   MOVEL     *BLANKS       FUNKEY
066700000000     C                   END
066800000000     C     KEY           IFEQ      F1
066900000000     C                   MOVEL     '01'          FUNKEY
067000000000     C                   END
067100000000     C     KEY           IFEQ      F2
067200000000     C                   MOVEL     '02'          FUNKEY
067300000000     C                   END
067400000000     C     KEY           IFEQ      F3
067500000000     C                   MOVEL     '03'          FUNKEY
067600000000     C                   END
067700000000     C     KEY           IFEQ      F4
067800000000     C                   MOVEL     '04'          FUNKEY
067900000000     C                   END
068000000000     C     KEY           IFEQ      F5
068100000000     C                   MOVEL     '05'          FUNKEY
068200000000     C                   END
068300000000     C     KEY           IFEQ      F6
068400000000     C                   MOVEL     '06'          FUNKEY
068500000000     C                   END
068600000000     C     KEY           IFEQ      F7
068700000000     C                   MOVEL     '07'          FUNKEY
068800000000     C                   END
068900000000     C     KEY           IFEQ      F8
069000000000     C                   MOVEL     '08'          FUNKEY
069100000000     C                   END
069200000000     C     KEY           IFEQ      F9
069300000000     C                   MOVEL     '09'          FUNKEY
069400000000     C                   END
069500000000     C     KEY           IFEQ      F10
069600000000     C                   MOVEL     '10'          FUNKEY
069700000000     C                   END
069800000000     C     KEY           IFEQ      F11
069900000000     C                   MOVEL     '11'          FUNKEY
070000000000     C                   END
070100000000     C     KEY           IFEQ      F12
070200000000     C                   MOVEL     '12'          FUNKEY
070300000000     C                   END
070400000000     C     KEY           IFEQ      F13
070500000000     C                   MOVEL     '13'          FUNKEY
070600000000     C                   END
070700000000     C     KEY           IFEQ      F14
070800000000     C                   MOVEL     '14'          FUNKEY
070900000000     C                   END
071000000000     C     KEY           IFEQ      F15
071100000000     C                   MOVEL     '15'          FUNKEY
071200000000     C                   END
071300000000     C     KEY           IFEQ      F16
071400000000     C                   MOVEL     '16'          FUNKEY
071500000000     C                   END
071600000000     C     KEY           IFEQ      F17
071700000000     C                   MOVEL     '17'          FUNKEY
071800000000     C                   END
071900000000     C     KEY           IFEQ      F18
072000000000     C                   MOVEL     '18'          FUNKEY
072100000000     C                   END
072200000000     C     KEY           IFEQ      F19
072300000000     C                   MOVEL     '19'          FUNKEY
072400000000     C                   END
072500000000     C     KEY           IFEQ      F20
072600000000     C                   MOVEL     '20'          FUNKEY
072700000000     C                   END
072800000000     C     KEY           IFEQ      F21
072900000000     C                   MOVEL     '21'          FUNKEY
073000000000     C                   END
073100000000     C     KEY           IFEQ      F22
073200000000     C                   MOVEL     '22'          FUNKEY
073300000000     C                   END
073400000000     C     KEY           IFEQ      F23
073500000000     C                   MOVEL     '23'          FUNKEY
073600000000     C                   END
073700000000     C     KEY           IFEQ      F24
073800000000     C                   MOVEL     '24'          FUNKEY
073900000000     C                   END
074000000000     C     KEY           IFEQ      ROLLUP
074100000000     C                   MOVEL     'RU'          FUNKEY
074200000000     C                   END
074300000000     C     KEY           IFEQ      HELP
074400000000     C                   MOVEL     'HP'          FUNKEY
074500000000     C                   END
074600000000     C                   ENDSR
074700000000     C*------------------------------------------------------------------------*
074800000000     C* NOTE PER COMPILAZIONE SCHIERE
074900000000     C*------------------------------------------------------------------------*
075000000000     C* -MSG-
075100000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
075200000000     C* (NON TOCCARE MSG,1)
075300000000     C*
075400000000     C* -CMD-
075500000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
075600000000     C*
075700000000     C* -FUD-
075800000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
075900000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
076000000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
076100000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
076200000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
076300000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
076400000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
076500000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
076600000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
076700000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
076800000000     C*
076900000000     C*------------------------------------------------------------------------*
077000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
077100070105TIST93R- Tasto funzionale non gestito                                          1
077200070105TIST93R- P.O. inesistente                                                      2
077300070105TIST93R- P.O. non in gestione                                                  3
077400070105TIST93R- P.O. obbligatorio                                                     4
077500070105TIST93R- Indicare o data distinte o numero distinta                            5
077600070105TIST93R- Distinta errata, chiusa o non disponibile                             6
077700070201TIST93R- Per bolle non in distinta possibile solo layout "G"                   7
077800070604TIST93R- Livello di dettaglio previsto solo per layout "DG"                    8
077900070105TIST93R-                                                                       9
078000070105TIST93R-                                                                      10
078100070105TIST93R-                                                                      11
078200070105TIST93R-                                                                      12
078300070105TIST93R-                                                                      13
078400070105TIST93R-                                                                      14
078500070105TIST93R- Numero distinta non valido                                           15
078600070105TIST93R- Data non valida                                                      16
078700070105TIST93R-                                                                      17
078800070105TIST93R-                                                                      18
078900070105TIST93R-                                                                      19
079000070105TIST93R-                                                                      20
079100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
079200060829F01=Aiuto          -        (S/N)=S  <== S-FISSO           funz.01¬
079300000000                   -        (S/N)=N                        funz.02¬
079400000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
079500000000                   -        (S/N)=N                        funz.04¬
079600000000                   -        (S/N)=N                        funz.05¬
079700060829F06=Lancio         -        (S/N)=S  <== S-FISSO           funz.06¬
079800000000                   -        (S/N)=N                        funz.07¬
079900000000                   -        (S/N)=N                        funz.08¬
080000000000                   -        (S/N)=N                        funz.09¬
080100060829                   -        (S/N)=N                        funz.10¬
080200000000                   -        (S/N)=N                        funz.11¬
080300060830                   -        (S/N)=N                        funz.12¬
080400000000                   -        (S/N)=N                        funz.13¬
080500000000                   -        (S/N)=N                        funz.14¬
080600000000                   -        (S/N)=N                        funz.15¬
080700000000                   -        (S/N)=N                        funz.16¬
080800000000                   -        (S/N)=N                        funz.17¬
080900000000                   -        (S/N)=N                        funz.18¬
081000000000                   -        (S/N)=N                        funz.19¬
081100000000                   -        (S/N)=N                        funz.20¬
081200000000                   -        (S/N)=N                        funz.21¬
081300060829                   -        (S/N)=N                        funz.22¬
081400000000                   -        (S/N)=N                        funz.23¬
081500060830                   -        (S/N)=N                        funz.24¬
