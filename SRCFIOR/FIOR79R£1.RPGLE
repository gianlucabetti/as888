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
007400070223     D*-------------------
007500070223     D* DESCRIZIONI ORGANIGRAMMA
007600070223     D*-------------------
007700070223     D OG146         E DS
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
013100070105     C                   WRITE     ST93D01
013200000000     C*
013300000000     C* VISUALIZZA MESSAGGIO
013400000000     C                   EXSR      SNDMSG
013500000000     C*
013600000000     C* EMETTE FUNZIONI
013700070105     C                   WRITE     ST93D02
013800000000     C*
013900000000     C* EMETTE FORMATO VIDEO
014000070105     C                   EXFMT     ST93D03
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
015700070105     C                   WRITE     ST93DMC
015800000000     C                   MOVEL     '0'           *IN40
015900000000     C*
016000000000     C* VISUALIZZAZIONE MESSAGGIO
016100000000     C                   Z-ADD     0             MSGULT            4 0
016200000000     C                   CALL      'XMSG'        XMSGPA
016300000000     C     DSMSER        IFEQ      0
016400000000     C                   MOVEL     DSMSKE        MSGKEY
016500000000     C                   ENDIF
016600000000     C                   Z-ADD     1             MSGULT
016700070105     C                   WRITE     ST93DMS                                      SFL MESS
016800070105     C                   WRITE     ST93DMC                                      CONTR MESS
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
020500070105     C                   MOVEL     'ST94'        KCOAZ
020600070215     C                   CALL      'BCH10'
020700070215 MOM C***                CALL      'TIST94R'
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
024700070223     C                   MOVEL     ORGDE6        OG146
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
027800070223     C* VERIFICA PIANIFICAZIONE GIRI CONSEGNA "RISPETTO A"
027900070223     C*--------------------
028000070223     C     ERRL00        IFEQ      '0'
028100070223     C                   IF        §OGDDA = *blanks
028200070223     C                   EVAL      VIDPIG = 'A'
028300070223     C                   ENDIF
028400070223     C                   ENDIF
028500000000     C*--------------------
028600070105     C* DATA DISTINTE
028700000000     C*--------------------
028800001016     C     ERRL00        IFEQ      '0'
028900070105     C     VIDDTA        IFNE      *zeros
029000070105     C                   Z-ADD     VIDDTA        G08DAT
029100000000     C                   Z-ADD     *ZEROS        G08INV
029200000000     C                   MOVEL     '0'           G08ERR
029300000000     C                   CALL      'XSRDA8'
029400971128     C                   PARM                    WLBDA8
029500070105     C                   Z-ADD     G08DAT        VIDDTA
029600001016     C     G08ERR        IFEQ      '0'
029700070105     C                   Z-ADD     G08INV        WDTA              8 0
029800001016     C                   ELSE
029900070125     C                   MOVEL     '1'           ERRL00
030000000000     C                   MOVEL     MSG(16)       DSMSMS
030100070105     C                   SETON                                        71
030200070105     C                   ENDIF
030300001016     C                   ENDIF
030400001016     C                   ENDIF
030500001110     C*--------------------
030600070105     C* NUMERO DISTINTA
030700001110     C*--------------------
030800001110     C     ERRL00        IFEQ      '0'
030900070105     C     VIDNDC        IFNE      *zeros
031000070105     C     VIDNDC        ANDNE     *blanks
031100070105     C     '?'           scan      VIDNDC                                 86
031200070105     C     *in86         ifeq      *on
031300070105     C                   clear                   dslr01
031400070105     C                   movel     'TIST93R'     dlrpgm
031500070105     C                   movel     *blanks       dlrtfv                         *sia open che close
031600070105     C                   z-add     4             dlrnpg
031700070105     C                   z-add     VIDFGS        dlrfgs
031800070220     C                   movel     'S'           dlrric
031900070222     C*
032000070222     C                   if        viddta = *zeros
032100070222     C                   eval      dlrgda = datcorinv - 10000
032200070222     C                   else
032300070222     C                   eval      dlrgda = viddta
032400070222     C                   eval      dlrgal = viddta
032500070222     C                   endif
032600070222     C*
032700070105     C                   movel(p)  dslr01        kpjbu
032800070105     C                   call      'FNLR02R'
032900070105     C                   parm                    kpjba
033000070105     C                   movel     kpjbu         dslr01
033100070105     C                   move      dlrnfv        VIDNDC
033200070105     C                   endif
033300070105     C                   endif
033400070105     C*
033500070105     C                   testn                   VIDNDC               86
033600070105     C   86              move      VIDNDC        vidndcn           7 0
033700070105     C* Se immesso il Nr distinta verifica che sia disponibile
033800070105     C     vidndcn       ifgt      *zero
033900070105     C                   z-add     vidndcn       dist5
034000070105     C     KEYfvv01_C    chain     fnfvv01l                           88
034100070105     C                   if        *in88 or fvvfcf<>*blanks or fvvatb<>*blanks
034200070105     C                   MOVEL     '1'           ERRL00
034300070105     C                   MOVEL     MSG(15)       DSMSMS
034400070105     C                   SETON                                        72
034500070105     C                   endif
034600070105     C                   endif
034700070105     C                   endif
034800070105     C*--------------------
034900070105     C* OBBLIGATORIETA' SELEZIONI
035000070105     C*--------------------
035100070105     C     ERRL00        IFEQ      '0'
035200070105     C                   IF        VIDDTA  = *zeros AND
035300070125     C                             (VIDNDC = *zeros OR VIDNDC = *blanks) AND
035400070125     C                             VIDTIP <> 'G'
035500070129     C                   MOVEL     '1'           ERRL00
035600070129     C                   MOVEL     MSG(07)       DSMSMS
035700070129     C                   SETON                                        7173
035800070105     C                   ENDIF
035900070129     C*
036000070129     C                   IF        VIDLIV  = 'T' AND
036100070129     C                             VIDTIP <> 'DG'
036200070129     C                   MOVEL     '1'           ERRL00
036300070129     C                   MOVEL     MSG(08)       DSMSMS
036400070129     C                   SETON                                          73
036500070129     C                   ENDIF
036600070105     C*
036700070105     C                   IF        VIDDTA  <> *zeros AND
036800070105     C                             (VIDNDC <>  *zeros AND VIDNDC <> *blanks)
036900070105     C                   MOVEL     '1'           ERRL00
037000070105     C                   MOVEL     MSG(05)       DSMSMS
037100070105     C                   SETON                                        7172
037200070105     C                   ENDIF
037300070105     C                   ENDIF
037400000000     C*
037500000000     C                   ENDSR
037600000000     C*------------------------------------------------------------------------*
037700000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
037800000000     C*------------------------------------------------------------------------*
037900000000     C     AZZVID        BEGSR
038000010417     C*
038100010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
038200070105     C                   CLEAR                   ST93D03
038300070105     C*
038400070105     C                   EVAL      VIDFGS = O36POU
038500070129     C                   EVAL      VIDLIV = 'S'
038600070105     C                   EVAL      VIDTIP = 'G'
038700070223     C                   EVAL      VIDPIG = 'D'
038800000000     C*
038900000000     C                   ENDSR
039000000000     C*------------------------------------------------------------------------*
039100001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
039200000000     C*------------------------------------------------------------------------*
039300000000     C     VIDPAR        BEGSR
039400000000     C*
039500070105     C                   Z-ADD     VIDFGS        In_FGS
039600070105     C                   Z-ADD     WDTA          In_DFV
039700070105     C                   MOVE(P)   VIDNDC        In_NFV
039800070129     C                   MOVEL(P)  VIDLIV        In_LIV
039900070105     C                   MOVEL(P)  VIDTIP        In_TIP
040000070223     C                   MOVEL(P)  VIDPIG        In_PIG
040100001016     C*
040200000000     C                   ENDSR
040300070105     C*--------------------------------------------------------------------------------------------*
040400070105     C* REPERISCE I DATI UTENTE
040500070105     C*--------------------------------------------------------------------------------------------*
040600070105     C     REPDATIUTE    BEGSR
040700070105     C*
040800070105     C* INIZIALIZZA VARIABILI DI WRK
040900070105     C                   CLEAR                   TIBS34DS
041000070105     C                   CLEAR                   AZUTEDS
041100070105     C                   CLEAR                   DDATIUTE
041200070105     C*
041300070105     C     *DTAARA       DEFINE    §azute        azuteds
041400070105     C     *DTAARA       DEFINE    §datiute      ddatiute
041500070105     C                   IN(E)     *DTAARA
041600070105     C                   IF        %Error
041700070105     c                   EVAL      I34Tla = 'L'
041800070105     C                   CALL      'TIBS34R'
041900070105     C                   PARM                    Tibs34Ds
042000070105     C                   IN        *DTAARA
042100070105     C                   ENDIF
042200070105     C*
042300070105     C                   ENDSR
042400000000     C*------------------------------------------------------------------------*
042500000000     C* *INZSR - OOPERAZIONI INIZIALI
042600000000     C*------------------------------------------------------------------------*
042700000000     C     *INZSR        BEGSR
042800000000     C*--------------------
042900000000     C* RICEVIMENTO PARAMETRI
043000000000     C*--------------------
043100000000     C     *ENTRY        PLIST
043200000000     C                   PARM                    KPJBA
043300070105     C*
043400070105     C* REPERIMENTO DATI UTENTE
043500070105     C                   EXSR      REPDATIUTE
043600070220     C*
043700070220     C* CALCOLA LA DATA CORRENTE
043800070220     C                   time                    wn14             14 0
043900070220     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044000070220     C                   z-add     wn8           g08dat
044100070220     C                   z-add     *zeros        g08inv
044200070220     C                   movel     '0'           g08err
044300070220     C                   call      'XSRDA8'
044400070220     C                   parm                    wlbda8
044500070220     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
044600070220     C                   z-add     g08dat        datcorinv         8 0          *DATA CORRENTE GG/M/
044700070105     C*
044800070105     C* Eseguo considerazioni sul profilo utente corrente
044900070105     C                   CLEAR                   TIBS36DS
045000070105     C                   EVAL      I36UTE = KNMUS
045100070105     C                   CALL      'TIBS36R'
045200070105     C                   PARM                    TIBS36DS
045300070105     C*
045400070202     C* CARICO TABELLA FILIALI GESTITE £1
045500070105     C                   clear                   trul06ds
045600070202     C                   move      '£1'          d06cod
045700070105     C                   movel     o36tfp        d06key
045800070105     C                   movel     trul06ds      kpjbu
045900070105     C                   call      'TRUL06R'
046000070105     C                   parm                    kpjba
046100070105     C                   movel     kpjbu         trul06ds
046200070105     C*
046300070105     C                   move      o36tfp        wo36tfp           3 0
046400070105     C*
046500070105     C* DEFINIZIONE CHIAVI DI ACCESSO AI FILE
046600070105     C*
046700070105     C* FNFVV01L - Completa
046800070105     C     KEYfvv01_C    KLIST
046900070105     C                   KFLD                    categ             1 0
047000070105     C                   KFLD                    dist5             5 0
047100070105     C                   KFLD                    VIDFGS
047200070105     C                   z-add     4             categ
047300070105     C*
047400000000     C*--------------------
047500060829     C* VARIABILI NN RIFERITE AL DATABASE
047600000000     C*--------------------
047700000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
047800000000     C*
047900000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
048000000000     C     XMSGPA        PLIST
048100000000     C                   PARM                    DSMSMS
048200000000     C                   PARM                    DSMSPG
048300000000     C                   PARM                    DSMSKE
048400000000     C                   PARM                    DSMSER
048500000000     C*
048600000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
048700000000     C                   CALL      'XKEY'
048800000000     C                   PARM                    DSKEY
048900000000     C*
049000000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
049100000000     C*   -NOME PGM CHIAMANTE
049200000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
049300070105     C                   MOVEL     'TIST93R'     DSMSPG
049400070105     C                   MOVEL     'TIST93R'     MSGQUE
049500000000     C                   MOVEL     *BLANKS       MSGKEY
049600000000     C                   MOVEL     *BLANKS       DSMSMS
049700000000     C*
049800000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
049900000000     C                   Z-ADD     0             FUNGRU            5 0
050000000000     C*
050100000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
050200000000     C                   EXSR      FUNGES
050300000000     C*--------------------
050400000000     C* VARIABILI DI CONTROLLO
050500000000     C*--------------------
050600000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
050700000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
050800000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
050900000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
051000000000     C*
051100000000     C* PULISCE IL VIDEO DA EMETTERE
051200000000     C                   EXSR      AZZVID
051300000000     C*
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
051700000000     C*------------------------------------------------------------------------*
051800000000     C     FUNGES        BEGSR
051900000000     C*
052000000000     C* PULISCO CAMPI DI LAVORO
052100000000     C                   Z-ADD     0             W                 5 0
052200000000     C*
052300000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
052400000000     C* E CARICO TABELLA FUNZIONI ATTIVE
052500000000     C     1             DO        24            I
052600000000     C                   MOVEL     FUD(I)        FUDRIG
052700000000     C*
052800000000     C* PULIZIA FUNZIONI PRECEDENTI
052900000000     C                   MOVEL     *BLANKS       FUV(I)
053000000000     C*
053100000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
053200000000     C* (SALTA F24)
053300000000     C     FUDATT        IFEQ      'S'
053400000000     C     FUDFUN        ANDNE     '24'
053500000000     C                   ADD       1             W
053600000000     C                   MOVEL     FUDDES        FUV(W)
053700000000     C                   END
053800000000     C                   END                                                    DO WHILE
053900000000     C*
054000000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
054100000000     C* ALTRIMENTI DISATTIVO F24
054200000000     C     1             DO        24            I
054300000000     C                   MOVEL     FUD(I)        FUDRIG
054400000000     C*
054500000000     C     FUDFUN        IFEQ      '24'
054600000000     C     W             IFGT      6
054700000000     C                   MOVEL     'S'           FUDATT
054800000000     C                   ELSE
054900000000     C                   MOVEL     *BLANKS       FUDATT
055000000000     C                   END                                                    W GT 8
055100000000     C                   MOVEL     FUDRIG        FUD(I)
055200000000     C                   END
055300000000     C                   END                                                    DO WHILE
055400000000     C*
055500000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
055600000000     C     W             DIV       6             I
055700000000     C                   MVR                     Z
055800000000     C*
055900000000     C     Z             IFNE      0
056000000000     C     I             ADD       1             Z
056100000000     C                   ELSE
056200000000     C                   Z-ADD     I             Z                 5 0
056300000000     C                   END
056400000000     C*
056500000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
056600000000     C                   ADD       1             FUNGRU
056700000000     C     FUNGRU        IFGT      Z
056800000000     C     FUNGRU        ORLT      1
056900000000     C                   Z-ADD     1             FUNGRU
057000000000     C                   END
057100000000     C*
057200000000     C* PRIMO GRUPPO DI 6 FUNZIONI
057300000000     C     FUNGRU        IFEQ      1
057400000000     C                   MOVEL     FUV(1)        FUN001
057500000000     C                   MOVEL     FUV(2)        FUN002
057600000000     C                   MOVEL     FUV(3)        FUN003
057700000000     C                   MOVEL     FUV(4)        FUN004
057800000000     C                   MOVEL     FUV(5)        FUN005
057900000000     C                   MOVEL     FUV(6)        FUN006
058000000000     C                   END
058100000000     C*
058200000000     C* SECONDO GRUPPO DI 6 FUNZIONI
058300000000     C     FUNGRU        IFEQ      2
058400000000     C                   MOVEL     FUV(7)        FUN001
058500000000     C                   MOVEL     FUV(8)        FUN002
058600000000     C                   MOVEL     FUV(9)        FUN003
058700000000     C                   MOVEL     FUV(10)       FUN004
058800000000     C                   MOVEL     FUV(11)       FUN005
058900000000     C                   MOVEL     FUV(12)       FUN006
059000000000     C                   END
059100000000     C*
059200000000     C* TERZO GRUPPO DI 6 FUNZIONI
059300000000     C     FUNGRU        IFEQ      3
059400000000     C                   MOVEL     FUV(13)       FUN001
059500000000     C                   MOVEL     FUV(14)       FUN002
059600000000     C                   MOVEL     FUV(15)       FUN003
059700000000     C                   MOVEL     FUV(16)       FUN004
059800000000     C                   MOVEL     FUV(17)       FUN005
059900000000     C                   MOVEL     FUV(18)       FUN006
060000000000     C                   END
060100000000     C*
060200000000     C* QUARTO GRUPPO DI 6 FUNZIONI
060300000000     C     FUNGRU        IFEQ      4
060400000000     C                   MOVEL     FUV(19)       FUN001
060500000000     C                   MOVEL     FUV(20)       FUN002
060600000000     C                   MOVEL     FUV(21)       FUN003
060700000000     C                   MOVEL     FUV(22)       FUN004
060800000000     C                   MOVEL     *BLANKS       FUN005
060900000000     C                   MOVEL     *BLANKS       FUN006
061000000000     C                   END
061100000000     C*
061200000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
061300000000     C                   MOVEL     *BLANKS       FUN007
061400000000     C                   MOVEL     *BLANKS       FUN008
061500000000     C*
061600000000     C     1             DO        24            I
061700000000     C                   MOVEL     FUD(I)        FUDRIG
061800000000     C     FUDFUN        IFEQ      '24'
061900000000     C     FUDATT        ANDEQ     'S'
062000000000     C                   MOVEL     FUDDES        FUN008
062100000000     C                   END
062200000000     C                   END                                                    DO
062300000000     C*
062400000000     C                   ENDSR
062500000000     C*------------------------------------------------------------------------*
062600000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
062700000000     C*------------------------------------------------------------------------*
062800000000     C     FUNCON        BEGSR
062900000000     C*
063000000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
063100000000     C                   EXSR      FUNVAR
063200000000     C*
063300000000     C* PULISCE CAMPI DI LAVORO
063400000000     C                   Z-ADD     0             I                 5 0
063500000000     C                   Z-ADD     0             W                 5 0
063600000000     C*
063700000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
063800000000IF  1C     FUNKEY        IFNE      *BLANKS
063900000000DO  2C     1             DO        24            I
064000000000     C                   MOVEL     FUD(I)        FUDRIG
064100000000     C*
064200000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
064300000000IF  3C     FUNKEY        IFEQ      FUDFUN
064400000000     C     FUDATT        ANDEQ     'S'
064500000000     C                   Z-ADD     I             W
064600000000E   3C                   ENDIF
064700000000E   2C                   ENDDO
064800000000     C*
064900000000     C* FUNZIONE NON ATTIVA, ERRORE
065000000000IF  2C     W             IFEQ      0
065100000000     C                   MOVEL     MSG(1)        DSMSMS
065200000000E   2C                   ENDIF
065300000000E   1C                   ENDIF
065400000000     C*
065500000000     C* FUNZIONE ATTIVA, PROCEDE
065600000000IF  1C     W             IFNE      0
065700000000     C     FUNKEY        OREQ      *BLANKS
065800000000IF  2C     FUNKEY        IFEQ      *BLANKS
065900000000     C                   EXSR      ENTGES
066000000000E   2C                   ENDIF
066100000000IF  2C     FUNKEY        IFEQ      '03'
066200000000     C                   EXSR      F03GES                                       *FINE LAVORO
066300000000E   2C                   ENDIF
066400000000IF  2C     FUNKEY        IFEQ      '06'
066500000000     C                   EXSR      F06GES                                       *LANCIO
066600000000E   2C                   ENDIF
066700000000IF  2C     FUNKEY        IFEQ      '12'
066800000000     C                   EXSR      F12GES                                       *RITORNO
066900000000E   2C                   ENDIF
067000000000IF  2C     FUNKEY        IFEQ      '24'
067100000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
067200000000E   2C                   ENDIF
067300000000E   1C                   ENDIF
067400000000     C*
067500000000     C                   ENDSR
067600000000     C*------------------------------------------------------------------------*
067700000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
067800000000     C*------------------------------------------------------------------------*
067900000000     C     FUNVAR        BEGSR
068000000000     C*
068100000000     C* INIZIALIZZA VARIABILE -FUNKEY-
068200000000     C*
068300000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
068400000000     C     KEY           IFEQ      ENTER
068500000000     C                   MOVEL     *BLANKS       FUNKEY
068600000000     C                   END
068700000000     C     KEY           IFEQ      F1
068800000000     C                   MOVEL     '01'          FUNKEY
068900000000     C                   END
069000000000     C     KEY           IFEQ      F2
069100000000     C                   MOVEL     '02'          FUNKEY
069200000000     C                   END
069300000000     C     KEY           IFEQ      F3
069400000000     C                   MOVEL     '03'          FUNKEY
069500000000     C                   END
069600000000     C     KEY           IFEQ      F4
069700000000     C                   MOVEL     '04'          FUNKEY
069800000000     C                   END
069900000000     C     KEY           IFEQ      F5
070000000000     C                   MOVEL     '05'          FUNKEY
070100000000     C                   END
070200000000     C     KEY           IFEQ      F6
070300000000     C                   MOVEL     '06'          FUNKEY
070400000000     C                   END
070500000000     C     KEY           IFEQ      F7
070600000000     C                   MOVEL     '07'          FUNKEY
070700000000     C                   END
070800000000     C     KEY           IFEQ      F8
070900000000     C                   MOVEL     '08'          FUNKEY
071000000000     C                   END
071100000000     C     KEY           IFEQ      F9
071200000000     C                   MOVEL     '09'          FUNKEY
071300000000     C                   END
071400000000     C     KEY           IFEQ      F10
071500000000     C                   MOVEL     '10'          FUNKEY
071600000000     C                   END
071700000000     C     KEY           IFEQ      F11
071800000000     C                   MOVEL     '11'          FUNKEY
071900000000     C                   END
072000000000     C     KEY           IFEQ      F12
072100000000     C                   MOVEL     '12'          FUNKEY
072200000000     C                   END
072300000000     C     KEY           IFEQ      F13
072400000000     C                   MOVEL     '13'          FUNKEY
072500000000     C                   END
072600000000     C     KEY           IFEQ      F14
072700000000     C                   MOVEL     '14'          FUNKEY
072800000000     C                   END
072900000000     C     KEY           IFEQ      F15
073000000000     C                   MOVEL     '15'          FUNKEY
073100000000     C                   END
073200000000     C     KEY           IFEQ      F16
073300000000     C                   MOVEL     '16'          FUNKEY
073400000000     C                   END
073500000000     C     KEY           IFEQ      F17
073600000000     C                   MOVEL     '17'          FUNKEY
073700000000     C                   END
073800000000     C     KEY           IFEQ      F18
073900000000     C                   MOVEL     '18'          FUNKEY
074000000000     C                   END
074100000000     C     KEY           IFEQ      F19
074200000000     C                   MOVEL     '19'          FUNKEY
074300000000     C                   END
074400000000     C     KEY           IFEQ      F20
074500000000     C                   MOVEL     '20'          FUNKEY
074600000000     C                   END
074700000000     C     KEY           IFEQ      F21
074800000000     C                   MOVEL     '21'          FUNKEY
074900000000     C                   END
075000000000     C     KEY           IFEQ      F22
075100000000     C                   MOVEL     '22'          FUNKEY
075200000000     C                   END
075300000000     C     KEY           IFEQ      F23
075400000000     C                   MOVEL     '23'          FUNKEY
075500000000     C                   END
075600000000     C     KEY           IFEQ      F24
075700000000     C                   MOVEL     '24'          FUNKEY
075800000000     C                   END
075900000000     C     KEY           IFEQ      ROLLUP
076000000000     C                   MOVEL     'RU'          FUNKEY
076100000000     C                   END
076200000000     C     KEY           IFEQ      HELP
076300000000     C                   MOVEL     'HP'          FUNKEY
076400000000     C                   END
076500000000     C                   ENDSR
076600000000     C*------------------------------------------------------------------------*
076700000000     C* NOTE PER COMPILAZIONE SCHIERE
076800000000     C*------------------------------------------------------------------------*
076900000000     C* -MSG-
077000000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
077100000000     C* (NON TOCCARE MSG,1)
077200000000     C*
077300000000     C* -CMD-
077400000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
077500000000     C*
077600000000     C* -FUD-
077700000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
077800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
077900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
078000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
078100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
078200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
078300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
078400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
078500000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
078600000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
078700000000     C*
078800000000     C*------------------------------------------------------------------------*
078900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
079000070105TIST93R- Tasto funzionale non gestito                                          1
079100070105TIST93R- P.O. inesistente                                                      2
079200070105TIST93R- P.O. non in gestione                                                  3
079300070105TIST93R- P.O. obbligatorio                                                     4
079400070105TIST93R- Indicare o data distinte o numero distinta                            5
079500070105TIST93R- Distinta errata, chiusa o non disponibile                             6
079600070201TIST93R- Per bolle non in distinta possibile solo layout "G"                   7
079700070604TIST93R- Livello di dettaglio previsto solo per layout "DG"                    8
079800070105TIST93R-                                                                       9
079900070105TIST93R-                                                                      10
080000070105TIST93R-                                                                      11
080100070105TIST93R-                                                                      12
080200070105TIST93R-                                                                      13
080300070105TIST93R-                                                                      14
080400070105TIST93R- Numero distinta non valido                                           15
080500070105TIST93R- Data non valida                                                      16
080600070105TIST93R-                                                                      17
080700070105TIST93R-                                                                      18
080800070105TIST93R-                                                                      19
080900070105TIST93R-                                                                      20
081000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
081100070605F01=Aiuto          -        (S/N)=N                        funz.01¬
081200000000                   -        (S/N)=N                        funz.02¬
081300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
081400000000                   -        (S/N)=N                        funz.04¬
081500000000                   -        (S/N)=N                        funz.05¬
081600060829F06=Lancio         -        (S/N)=S  <== S-FISSO           funz.06¬
081700000000                   -        (S/N)=N                        funz.07¬
081800000000                   -        (S/N)=N                        funz.08¬
081900000000                   -        (S/N)=N                        funz.09¬
082000060829                   -        (S/N)=N                        funz.10¬
082100000000                   -        (S/N)=N                        funz.11¬
082200060830                   -        (S/N)=N                        funz.12¬
082300000000                   -        (S/N)=N                        funz.13¬
082400000000                   -        (S/N)=N                        funz.14¬
082500000000                   -        (S/N)=N                        funz.15¬
082600000000                   -        (S/N)=N                        funz.16¬
082700000000                   -        (S/N)=N                        funz.17¬
082800000000                   -        (S/N)=N                        funz.18¬
082900000000                   -        (S/N)=N                        funz.19¬
083000000000                   -        (S/N)=N                        funz.20¬
083100000000                   -        (S/N)=N                        funz.21¬
083200060829                   -        (S/N)=N                        funz.22¬
083300000000                   -        (S/N)=N                        funz.23¬
083400060830                   -        (S/N)=N                        funz.24¬
