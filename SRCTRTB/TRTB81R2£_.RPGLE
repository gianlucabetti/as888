000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
002100940223      *
002200940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002300940620      *  03           PROTECT TUTTI I CAMPI
002400940223      *  21           GENERICO OPERAZIONI I/O
002500940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002600940223      *  30           SFLDSP
002700940223      * N31           SFLCLR
002800940128      *  31           SFLDSPCTL
002900940128      *  32           SFLNXTCHG
003000940128      *  33           SFLEND
003100940128      *  39           OF PRTF
003200940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003300940317    > *  Specificare l'uso dei singoli indicatori
003400940315      *  50 <---> 98  ERRORI SU VIDEO
003500940317    > *  Specificare l'uso dei singoli indicatori
003600940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003900940128      *  99           INDIC. GENERALE DI ERRORE
004000940128     F*----------------------------------------------------*
004100030122$001 FTRTB81D2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300030113     FTABEL00F  UF A E           K DISK
004400940128     D*----------------------------------------------------*
006000030122$003 D EXTDS         E DS                  EXTNAME(DS8D) INZ
006001030113      *-------------
006100940127     D* Passaggio Parametri
006200940127     D KPJBA         E DS
006201030113      *-------------
006202030113     D* Parametri in ricezione
006203030113     D  TABDS          DS
006204030113     D  XTAOPZ                 1      2
006205030113     D  XTARET                 3      3
006206030113     D  XTAOPR                 4      4
006207030113     D  XTAERR                 5      5
006208030113     D  XTAKEY                 6     13
012000940201     D*-------------
012100940201     D DSFMT           DS
012200940506     D  $TASTO               369    369
012300940201     D  NRG                  370    370
012400940201     D  NCL                  371    371
012500940201     D*-------------
012600940201     D* posizione cursore
012700940201     D CURSOR          DS
012800940223     D  RIRI                   1      2B 0 INZ
012900940201     D  R$$                    2      2
013000940223     D  COCO                   3      4B 0 INZ
013100940201     D  C$$                    4      4
013200940207     D*-------------
013300940207     D* Reperimento nome PGM
013400940207     D STATUS         SDS           333
013500940207     D  DSPGM            *PROC
013600940225     D*-------------
013700940225     D* COSTANTI
013800940225     D*-------------
014000940506     D* Tasti di funzione
014100940506     D F01             C                   CONST(X'31')
014200940506     D F02             C                   CONST(X'32')
014300940506     D F03             C                   CONST(X'33')
014400940506     D F04             C                   CONST(X'34')
014500940506     D F05             C                   CONST(X'35')
014600940506     D F06             C                   CONST(X'36')
014700940506     D F07             C                   CONST(X'37')
014800940506     D F08             C                   CONST(X'38')
014900940506     D F09             C                   CONST(X'39')
015000940506     D F10             C                   CONST(X'3A')
015100940506     D F11             C                   CONST(X'3B')
015200940506     D F12             C                   CONST(X'3C')
015300940506     D F13             C                   CONST(X'B1')
015400940506     D F14             C                   CONST(X'B2')
015500940506     D F15             C                   CONST(X'B3')
015600940506     D F16             C                   CONST(X'B4')
015700940506     D F17             C                   CONST(X'B5')
015800940506     D F18             C                   CONST(X'B6')
015900940506     D F19             C                   CONST(X'B7')
016000940506     D F20             C                   CONST(X'B8')
016100940506     D F21             C                   CONST(X'B9')
016200940506     D F22             C                   CONST(X'BA')
016300940506     D F23             C                   CONST(X'BB')
016400940506     D F24             C                   CONST(X'BC')
016500940506     D ENTER           C                   CONST(X'F1')
016600940506     D ROLDWN          C                   CONST(X'F4')
016700940506     D ROLLUP          C                   CONST(X'F5')
016800940506     D*-------------
017000941108     D DATA            C                   CONST('0001-01-01')
017001030114     D Annull          C                   CONST('  ANNULLATO  ')
017002030114     D OPz_Imm         C                   CONST('IMMISSIONE')
017003030114     D OPz_Var         C                   CONST('VARIAZIONE')
017004030114     D OPz_Del         C                   CONST(' ANNULLA  ')
017005030114     D OPz_Vis         C                   CONST('VISUALIZZA')
019000940127     C*----------------------------------------------------*
019100940127     C*                MAIN LINE PROGRAM
019200940127     C*----------------------------------------------------*
019600940223     C*
019700940223     C     $FINE         DOWEQ     *OFF
019800940202     C     $GEST         CASEQ     'D1'          GESD1
019900940117     C                   END
020000940117     C                   END
020200940325     C* fine programma
020201030113     c                   movel     tabds         kpjbu
020300030113     C                   Seton                                        LR
031300940131     C************************************************************
031400940510     C* GESTIONE VIDEO RECORD D1
031500940131     C************************************************************
031600940127     C     GESD1         BEGSR
031601030113      *
031700940225     C* inizializzazione videata
031800940223     C     $INZD1        IFEQ      *ON
031900940127     C                   EXSR      INZD1
032000940223     C                   MOVE      *OFF          $INZD1
032100940117     C                   END
032101030113      *
032200940225     C* emissione piede videata se proveniente da altra
032300940225     C* salvataggio valore formato di provenienza
032400940207     C     $LASTV        IFNE      'D1'
032500940614     C                   WRITE     T1
032700940202     C                   MOVE      $LASTV        $LASTG
032800940202     C                   MOVE      'D1'          $LASTV
032900940202     C                   END
033000940131     C*
033100940203     C                   SELECT
033200940203     C* Annullamento
033201030114     C* Visualizzazione
033300030114    >C     xtaopz        WHENEQ    '01'
033301030114     c                   movel     Opz_Imm       D1Fun
033302030114    >C     xtaopz        WHENEQ    '05'
033303030114     c                   movel     Opz_Vis       D1Fun
033304030114     C                   SETON                                        020306
033305030114    >C     xtaopz        WHENEQ    '04'
033306030114     c                   movel     Opz_Del       D1Fun
033600030114     C                   SETON                                        0203
033601030114     C                   SETOFF                                         06
033700940620     C* Manutenzione
033800030113    >C     xtaopz        WHENEQ    '02'
033801030114     c                   movel     Opz_Var       D1Fun
033900030114     C                   SETON                                        02
034000030114     C                   SETOFF                                       0306
034100940620     C*
034200940620     C                   OTHER
034300940620     C*
034400030114     C                   SETOFF                                       020306
034500940203     C                   ENDSL
034501030114     C*
034502030114     c                   movel     D1Fun         D2Fun
034503030113     C*
034504030114     c                   SETOFF                                         99
034600940620     C* Immissione/Modifica/Copia
034601030113     C*               *----------------*
034700940620     C                   EXFMT     D1
034701030113     C*               *----------------*
034702030114     C*
034703030114     C     *IN99         IFEQ      *OFF
034704030114     C*
0347050301141    C                   SELECT
034706030114     C* F3=Fine
034707030114     C     $TASTO        WHENEQ    F03
034708030114     C                   EXSR      F03D1
034709030114     C* F12=Ritorno
034710030114     C     $TASTO        WHENEQ    F12
034711030114     C                   EXSR      F12D1
034712030114     C*
0347130301141O   C                   OTHER
034714030114     C*
034715030114     C                   EXSR      CTRD1
034901030114     C* Video 2
035200940506     C     *IN99         IFEQ      *OFF
035201030122     c                   goto      no_Vid2
035202030114     C*               *----------------*
035203030122     C                   EXFMT     D2
035204030114     C*               *----------------*
035205030122     c     no_Vid2       tag
035300940131     C*
0354009402031    C                   SELECT
035600940131     C* F3=Fine
035700940506     C     $TASTO        WHENEQ    F03
035800940309     C                   EXSR      F03D1
036800940131     C* F12=Ritorno
036900940506     C     $TASTO        WHENEQ    F12
037000940309     C                   EXSR      F12D1
037400940131     C*
0375009402031O   C                   OTHER
037501030113     C*
037600030114     C                   EXSR      CTRD2
037601030113     C*
037700940616     C     *IN99         IFEQ      *OFF
038200940616     C* F6
0383000301142    C     $TASTO        andeq     F06
0383010301152    C     $TASTO        oreq      F23
038400940224     C* eseguo aggiornamento
038500940315    >C                   EXSR      AGGANA
038600940224     C* eseguo operazioni del dopo-aggiornamento
038800030114     C  N99              EXSR      GESAGG
038901030114     C*
039201030114     C                   ENDIF
039202030114     C*
0392030301142-   C                   ENDSL
039204030114     C*
0392050301142-   C                   ENDIF
039400940224     C*
0395000301142-   C                   ENDSL
039501030114     C*
0395020301142-   C                   END
039503030114     C*
039600940117     C                   ENDSR
040900940117      ************************************************************
041000940207      * INIZIALIZZAZIONE VIDEATA DATI
041100940117      ************************************************************
041200940127     C     INZD1         BEGSR
041300940131     C*
041400940615     C                   CLEAR                   D1
041401030113     C                   CLEAR                   D2
041600030113     C                   MOVEl     xtakey        D1COD
041601030113     C                   MOVEl     xtakey        D2COD
041602030113     C                   MOVEl     xtakey        TBLkey
042400940207     C*
042500940224     C* valorizzo la chiave univoca per reperire i dati
042600940224     C* se non immissione
0427000301141   >C     xtaopz        IFNE      '01'
042800940315    >C*-------------------------------------------------------------
042900940315    >C* impostare i campi chiave ricevuti
043000940315    >C*-------------------------------------------------------------
043300940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0434000301132   >C     xtaopz        IFEQ      '02'
043500030113    >C     xtaopz        OREQ      '04'
043600030113     C     KTBL          chain     TABEL00F
043700940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0438009406142E   C                   ELSE
043802030113     C     KTBL          chain(N)  TABEL00F
0440009406142-   C                   ENDIF
044100940614     C*
044600030113     C                   IF        %Found(Tabel00F)
044700030113     C                   MOVEL     TBLuni        EXTDS
044701030114     c                   if        TblFLG = '*'
044703030115     c                   seton                                        08
044706030114     C                   MOVEl     Annull        D1Ann
044707030114     C                   MOVEl     D1Ann         D2Ann
044708030114     C                   end
044709030113     C*
047300940224     C* riempio la videata
047400940224     C                   EXSR      RIED1
047401030113      *
047500940224     C* richiamo routine dei ctrl per decodificarla
047600940614    >C                   EXSR      CTRD1
047601030113      *
047700960228     C                   MOVEL     *IN99         SAV99             1
047701030113      *
047800960228     C* Se la gestione è più videate
047900960305    >C                   EXSR      CTRD2
047901030113      *
048000960305    >C     *IN99         IFEQ      *ON
048100960305    >C                   MOVEL     *IN99         SAV99             1
048200960305    >C                   ENDIF
048201030113      *
048300940224     C                   MOVE      *ALL'0'       IN5098
048400940224     C                   MOVEA     IN5098        *IN(50)
048401030113      *
048500940224     C* definizione indicatori su video
048600940224     C                   SETOFF                                       02
0487009402243    C                   SELECT
048800030114    >C     xtaopz        WHENEQ    '02'
048900940224     C                   SETON                                        02
049000030114    >C     xtaopz        WHENEQ    '05'
0495009402243-   C                   ENDSL
049600940224     C*
0497009402242-   C                   ENDIF
049701030114     C*
0497020301141   >C                   Else
049703030114     C* Se in Immissione nuovo record
049704030114     c                   exsr      NewD1
049800940223     C*
0499009402241-   C                   ENDIF
050000940224     C*
050100940117     C                   ENDSR
050200940207     C/EJECT
050300940207     C************************************************************
050400940510     C* RIEMPIMENTO VIDEATA  D1
050500940207     C************************************************************
050600030114     C     NEWD1         BEGSR
050700940207     C*
050701030114     c                   clear                   d1
050702030114     c                   clear                   d2
050714030114     C*
050715030114     C                   ENDSR
050716030114     C************************************************************
050717030114     C* RIEMPIMENTO VIDEATA  D1
050718030114     C************************************************************
050719030114     C     RIED1         BEGSR
050720030114     C*
050721030113     C* descrizione
050722030122     c                   eval      d1DES = §8DDES
050723030122      * Partenza/Arrivo
050725030122     c                   eval      d1PA  = §8DPA
050844940207     C*
050900940207     C                   ENDSR
051100940309     C************************************************************
051200940309     C* GESTIONE F03 VIDEO D1
051300940309     C************************************************************
051400940309     C     F03D1         BEGSR
051500940309     C*
051600940309     C                   MOVE      *ON           $FINE
051700030113    >C                   MOVE      '1'           xtaret
052000940325     C*
052100940325     C                   ENDSR
054200940309     C************************************************************
054300940309     C* GESTIONE F12 VIDEO D1
054400940309     C************************************************************
054500940309     C     F12D1         BEGSR
054600940309     C*
054700940309     C                   MOVE      *ON           $FINE
054900940309     C*
055000940309     C                   ENDSR
057100940131     C************************************************************
057200940207     C* CONTROLLO VIDEATA
057300940131     C************************************************************
057400940127     C     CTRD1         BEGSR
057500940131     C*
057600940127     C                   SETOFF                                       99
057700940127     C* non imposto pos. del cursore
057800940127     C                   Z-ADD     0             H1RIGA
057900940127     C                   Z-ADD     0             H1COLO
058000940621     C*
059600940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0598000301141   >C     xtaopz        IFNE      '04'
059900971110     C*
060000971110     C* controllo codice vuoto
060100971110$017 C     D1COD         IFEQ      *ALL' '
060200971110     C                   SETON                                        5099
060201030114     C                   Else
060202030114     c                   eval      d2cod = d1cod
060300971110     C                   ENDIF
060301030114     C*
060302030114     C* controllo codice vuoto
060303030114$017 C     D1DES         IFEQ      *ALL' '
060304030114     C                   SETON                                        5199
060305030114     C                   Else
060306030114     c                   eval      d2des = d1des
060307030114     C                   ENDIF
060500940207     C*
060600940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
060700030114    >C     xtaopz        IFEQ      '01'
061101030114    >C     xtaopz        oreq      '03'
061102030114     c                   movel(p)  d1cod         tblkey
061103030114     C     KTBL          chain(N)  TABEL00F
061104030114     c                   if        %Found(TABEL00F)
061105030114     C                   SETON                                        5299
065600030114     C                   EndIF
065601030114     C                   ENDIF
065750030114      *
0658009402241-   C                   END
065900940131     C*
066000940302     C     *IN99         IFEQ      *ON
066100940510     C     $GEST         ANDEQ     'D1'
066300940117     C                   MOVEA     *IN           WIN
066400940128     C                   MOVE      *ALL'0'       IN5098           49
066500940128     C                   MOVEA     IN5098        *IN(50)
066600940614     C                   WRITE     D1
066700940117     C                   MOVEA     WIN           *IN
066800940302     C                   ENDIF
066900940127     C*
067000940117     C                   ENDSR
067100960305     C/EJECT
067200960305     C************************************************************
067300960305     C* CONTROLLO VIDEATA D2
067400960305     C************************************************************
067500960305     C     CTRD2         BEGSR
067600960305     C*
067700960305     C                   ENDSR
067800940207     C/EJECT
067900940224     C************************************************************
068000940315    >C* AGGIORNAMENTO ANAGRAFICA
068100940224     C************************************************************
068200940315    >C     AGGANA        BEGSR
068300971110     C*
068400030114$007 C                   MOVE      '1'           tblKUT
068401030122$007 C                   MOVE      '8D'          tblCOD
068500030114     C                   movel     *blanks       tblKEY
068600030114     C                   MOVEl     D1COD         tblKEY
068601030114     c                   movel     '1'           tblFTT
068602030114     c                   move      *zeros        tblDTR
068603030114     c                   move      *zeros        tblFLT
068604030114     c                   move      *blanks       tblFTR
068605030114     c                   move      ' '           TBLFLG
068800940207     C*
068900940207     C                   SELECT
069000030114    >C     xtaopz        WHENEQ    '01'
069100030114    >C     xtaopz        OREQ      '03'
069200940315    >C                   EXSR      RIEANA
069201030114     c                   write     TABEL                                59
069700940318     C* se rcd non scrivibile attivo errore
069900030114    >C   59              SETON                                          99
069901030114      *
070100030114    >C     xtaopz        WHENEQ    '02'
070600940622    >C                   EXSR      RIEANA
070601030114     c                   update    TABEL                                59
070602030114    >C   59              SETON                                          99
071100030114    >C     xtaopz        WHENEQ    '04'
071101030114     c                   move      '*'           TBLFLG
071102030115     c                   if        $tasto = F23
071103030115     c                   delete    TABEL                                59
071104030115     c                   else
071105030114     c                   update    TABEL                                59
071106030115     c                   end
071107030114    >C   59              SETON                                          99
072300940621     C*
072400940207     C                   ENDSL
072500940203     C*
072600940203     C                   ENDSR
072800940224     C************************************************************
072900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
073000940224     C************************************************************
073100940616     C     RIEANA        BEGSR
073501030114     C*
073502030114     C                   CLEAR                   tblUNI
073503030114     C* descrizione
073504030122     c                   movel     d1DES         §8DDES
073505030122      * Partenza Arrivo
073507030122     c                   movel     d1PA          §8DPA
073641030114     C*
073642030114     C                   MOVEL     EXTDS         TBLUNI
073643030114     C*
073644940207     C                   ENDSR
073800940224     C************************************************************
073900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
074000940224     C************************************************************
074100940224     C     GESAGG        BEGSR
074200940224     C*
074300940224     C* segnalo al pgm chiamante l'avvenuta conferma
074400030114    >C                   MOVE      *ON           xtaopr
074500940224     C* segnalo al pgm chiamante l'aver premuto l'F6
074600030114    >C                   MOVE      '0'           xtaret
074700940224     C*
0748009402241    C                   SELECT
074900940224     C* nel caso di immissione, ripristino la videata iniziale
075000030114    >C     xtaopz        WHENEQ    '01'
075100940224     C                   MOVE      *ON           $INZD1
075101030114     C                   MOVE      *ON           $FINE
075200940224     C* altrimenti ritorno al pgm chiamante
075300940224     C                   OTHER
075400940224     C                   MOVE      *ON           $FINE
0755009402241-   C                   ENDSL
075600940224     C*
075700940224     C                   ENDSR
077300940131     C************************************************************
077400940131     C* OPERAZIONI INIZIALI
077500940131     C************************************************************
077600940131     C     *INZSR        BEGSR
077700940131     C*
077800940127     C* Reperimento parametri
077900940127     C*
078000940117     C     *ENTRY        PLIST
078100940117     C                   PARM                    KPJBA
078101030113     C                   movel     kpjbu         tabds
078102030115     c                   setoff                                       08
078103030115     C*
087400940223     C* Variabili per gestione videate
087600940223     C                   MOVE      'D1'          $GEST
087700940223     C                   MOVE      *OFF          $FINE
087800940223     C                   MOVE      *ON           $INZD1
087900940224     C                   MOVE      *BLANK        $LASTG
088000940224     C                   MOVE      *BLANK        $LASTV
088100940506     C                   Z-ADD     0             $ULKD1            3 0
088200940127     C*
088300940223     C* Variabili appoggio
088500940224     C                   Z-ADD     0             CURR
088600940224     C                   Z-ADD     0             CURC
088700940207     C*
088800940207     C* Valorizzazione campi univoci testate
089000940614     C                   CLEAR                   T1
089100940614     C                   MOVEL     KNSIF         NOMSIF
089200030113     C******             MOVEL     XSCDSI        NOMDIT
089300941027     C                   MOVEL     DSPGM         NOMPGM
089301030113     C*
089302030113     C                   z-add     1             TBLkut
089303030122     C                   move      '8D'          TBLcod
089304030114     C                   movel     xtakey        TBLkey
091000940224     C*
091100940224     C* Inizializzazione/Decodifica prima videata
091300940224     C                   EXSR      INZD1
091400940224     C                   MOVE      *OFF          $INZD1
092100940127     C*
092200940117     C                   ENDSR
092300940128     C************************************************************
092400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
092500940223     C************************************************************
092600940223     C     DEFCAM        BEGSR
092700940223     C*
092800940223     C* klist
092803030113     C     KTBL          KLIST
092804030113     C                   KFLD                    TBLkut
092805030113     C                   KFLD                    TBlCOD
092806030113     C                   KFLD                    TBlKEY
092900940207     C*
093100940223     C* Variabili per gestione videate
093200940223     C*
093300940223     C                   MOVE      *BLANK        $GEST             2
093400940223     C                   MOVE      *BLANK        $FINE             1
093500940223     C                   MOVE      *BLANK        $INZD1            1
093600940223     C                   MOVE      *BLANK        $LASTG            2
093700940223     C                   MOVE      *BLANK        $LASTV            2
093800940223     C*
093900940223     C* Variabili appoggio
094100940223     C                   Z-ADD     0             CURR              2 0
094200940223     C                   Z-ADD     0             CURC              2 0
094300940223     C                   MOVE      *ZEROS        WIN              99
094400940224     C                   MOVE      *BLANK        WTBERR            1
094500940223     C*
094600940223     C* Indici
094800940223     C                   Z-ADD     0             X                 3 0
094900940223     C                   Z-ADD     0             Y                 3 0
095000940127     C*
095100940117     C                   ENDSR
095200940128     C************************************************************
