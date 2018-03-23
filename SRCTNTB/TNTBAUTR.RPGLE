000100141127     H*
000200141127     H*
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000600940307      *  21           GENERICO OPERAZIONI I/O
000700940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940224      *  30           SFLDSP
000900940224      * N31           SFLCLR
001000940224      *  31           SFLDSPCTL
001100940224      *  32           SFLNXTCHG
001200940224      *  33           SFLEND
001300940224      *  39           OF PRTF
001400940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940608      *  Specificare l'uso dei singoli indicatori
001600940224      *  50 <---> 98  ERRORI SU VIDEO
001700940608      *  Specificare l'uso dei singoli indicatori
001800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001900940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002000940224      *  99           INDIC. GENERALE DI ERRORE
002100940128     F*----------------------------------------------------*
002200100119     Ftntbe01L  IF   E           K DISK
002300100119     Fazorg01L  IF   E           K DISK
002400100119     FTNTBAUTD  CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600090225     F                                     SFILE(S0:S0NRR)
002700940201     F                                     INFDS(DSFMT)
002800940128     D*----------------------------------------------------*
002900940211     D* Passaggio Parametri
003000940211     D KPJBA         E DS
003100100407     D TNTBautDS     E DS
003200100322     D tbeUNI_dAUT   E DS                  EXTNAME(DAUT)
003300100322     D                                     PREFIX(tbeUNI_)
003400030113      *-------------
003500940325     D* Parametri in ricezione
003600030113     D  TABDS          DS
003700030113     D  XTAOPZ                 1      2
003800030113     D  XTARET                 3      3
003900030113     D  XTAOPR                 4      4
004000030113     D  XTAERR                 5      5
004100100409     D  XTAKEY1                6     20
004200100409     D  XTAKEY2               21     30
004300100409     D  XTACOD                31     33
004400100322      *
004500100322      * La local Data Area sostituisce i parametri passati fra programmi
004600100322      * *LDA Local Data Area per passare i totali da mettere in testata
004700100322     D LDA_Selez       DS
004800100322     D  lda_AUT                1      1
004900100322     D  lda_COOP               2      2
005000100322     D  lda_AFDE               3      3
005100100324     D  lda_AUTMTC             4      4
005200100324     D  lda_AUTSDT             5      5
005300100324     D  lda_AUTRET             6      6
005400100324     D  lda_AUTCON             7      7
005500100324     D  lda_AUTCRS             8      8
005600100324     D  lda_AUTVTCC            9      9
005700100324     D  lda_AUTMTCC           10     10
005800100324     D  lda_AUTSDTC           11     11
005900100324     D  lda_AUTRETC           12     12
006000100324     D  lda_AUTCRSC           13     13
006100100324     D  lda_AUTCONC           14     14
006200100324     D  lda_RIEABI            15     15
006300100324     D  lda_AFFMTC            16     16
006400100324     D  lda_AFFSDT            17     17
006500100324     D  lda_AFFRET            18     18
006600100324     D  lda_AFFCON            19     19
006700100324     D  lda_AFFCRS            20     20
006800100324     D  lda_AFFGVSTD          21     21
006900100324     D  lda_AFFAGV            22     22
007000100324     D  lda_AFFCPSTD          23     23
007100100324     D  lda_AFFARRM           24     24
007200100324     D  lda_AUTCPRRE          25     25
007300100324     D  lda_AUTGSTM           26     26
007400100324     D  lda_AUTSTDCC          27     27
007500100324     D  lda_lda                1     27    DIM(27)
007600940211     D*-------------
007700940211     D DSFMT           DS           512
007800940506     D  $TASTO               369    369
007900940211     D  NRG                  370    370
008000940211     D  NCL                  371    371
008100940211     D  SFLNRR               378    379B 0
008200940207     D*-------------
008300940207     D* Nome PGM a video
008400940207     D                 DS
008500940207     D  PROGR                  1     10
008600940207     D  ASTER1                 1      1    INZ('*')
008700940207     D  SIGLA                  2      9
008800940207     D  ASTER2                10     10    INZ('*')
008900940127     D*-------------
009000940127     D* Reperimento nome PGM
009100940127     D STATUS         SDS           333
009200940127     D  DSPGM            *PROC
009300030113     D*-------------
009400030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
009500090225$003 D S0NRR           S                   Like(C1rcd)
009600090225$003 D S1NRR           S                   Like(C1rcd)
009700100319$003 D S2NRR           S                   Like(C1rcd)
009800030113$003 D WSfl            S                   Like(C1nrr)
009900030113$003 D Wmax            S                   Like(C1rcd)
010000030113$003 D Wpag            S                   Like(C1rcd)
010100090225$003 D Winzs0          S                   Like($inzs0)
010200090225$003 D Winzs1          S                   Like($inzs1)
010300100319$003 D Winzs2          S                   Like($inzs2)
010400100324$003 D x1cod           S                   Like(h1cod)
010500090225$003 D x0cod           S                   Like(s0cod)
010600100326$003 D savC0pos        S                   Like(C0POS)
010700090225     D*-------------
010800100119     d dataiso         s               d   datfmt(*iso)
010900100119     d dataeur         s               d   datfmt(*eur)
011000100119     d sav_cod_FIL     s              3
011100940207     D*-------------
011200940211     D* COSTANTI
011300940211     D*-------------
011400090225     D SFLPAG0         C                   CONST(11)
011500100121     D SFLPAG1         C                   CONST(05)
011600100319     D SFLPAG2         C                   CONST(05)
011700940314     D* dimensione della schiera $MS1
011800940506     D*
011900940506     D* Tasti di funzione
012000940506     D F01             C                   CONST(X'31')
012100940506     D F02             C                   CONST(X'32')
012200940506     D F03             C                   CONST(X'33')
012300940506     D F04             C                   CONST(X'34')
012400940506     D F05             C                   CONST(X'35')
012500940506     D F06             C                   CONST(X'36')
012600940506     D F07             C                   CONST(X'37')
012700940506     D F08             C                   CONST(X'38')
012800940506     D F09             C                   CONST(X'39')
012900940506     D F10             C                   CONST(X'3A')
013000940506     D F11             C                   CONST(X'3B')
013100940506     D F12             C                   CONST(X'3C')
013200940506     D F13             C                   CONST(X'B1')
013300940506     D F14             C                   CONST(X'B2')
013400940506     D F15             C                   CONST(X'B3')
013500940506     D F16             C                   CONST(X'B4')
013600940506     D F17             C                   CONST(X'B5')
013700940506     D F18             C                   CONST(X'B6')
013800940506     D F19             C                   CONST(X'B7')
013900940506     D F20             C                   CONST(X'B8')
014000940506     D F21             C                   CONST(X'B9')
014100940506     D F22             C                   CONST(X'BA')
014200940506     D F23             C                   CONST(X'BB')
014300940506     D F24             C                   CONST(X'BC')
014400940506     D ENTER           C                   CONST(X'F1')
014500940506     D ROLDWN          C                   CONST(X'F4')
014600940506     D ROLLUP          C                   CONST(X'F5')
014700940127     C*----------------------------------------------------*
014800940127     C*                MAIN LINE PROGRAM
014900940127     C*----------------------------------------------------*
015000940223     C* inizializzazione variabili
015100940223     C                   EXSR      INZVAR
015200940223     C*
015300940223     C     $FINE         DOWEQ     *OFF
015400090225     C     $GEST         CASEQ     'S0'          GESS0
015500940131     C     $GEST         CASEQ     'S1'          GESS1
015600940117     C                   END
015700940117     C                   END
015800940325     C* fine programma
015900940325     C                   SETON                                            LR
016000030113     C************************************************************
016100030113     C* INIZIALIZZAZIONE VARIABILI
016200030113     C************************************************************
016300030113     C     INZVAR        BEGSR
016400030113     C*
016500030113     C* Pulizia campi e indicatori
016600030113     C                   MOVE      *ALL'0'       IN4049           10
016700030113     C                   MOVEA     IN4049        *IN(40)
016800030113     C                   CLEAR                   S1OPZ
016900090225     C                   CLEAR                   S0OPZ
017000030113     C*
017100090225     C* Variabili per gestione videate
017200030113     C                   MOVE      *OFF          $FINE
017300030113     C                   MOVE      *OFF          $EFILE
017400030113     C                   MOVE      *OFF          $ESCI
017500030113     C                   MOVE      *OFF          $RCDOK
017600030113     C                   Z-ADD     0             $ULKS1            3 0
017700030113     C*
017800100319     C                   MOVE      *ON           $INZS2
017900030113     C                   MOVE      *ON           $INZS1
018000090225     C                   MOVE      *ON           $INZS0
018100100407     C*
018200100322     c                   move      *all'S'       lda_lda
018300100324     c                   out       lda_selez
018400030113     C* Variabili appoggio
018500030114     C                   Z-ADD     1             WPAG
018600030113     c*
018700100407     c* Video da cui partire
018800100407     c                   movel(p)  'AUT'         tbe_AUT           3
018900100407     C                   MOVE      'S0'          $GEST
019000100407     c                   if        autSelez = 'R' and autFILIA <> *blank and
019100100407     c                             autProf <> *blank
019200100407     C                   MOVE      'S1'          $GEST
019300100407     c                   eval      CodiceFIL = autFILIA
019400100407     c                   end
019500100407     c*
019600030113     C                   ENDSR
019700090225     C************************************************************
019800090225     C* GESTIONE LISTA
019900090225     C************************************************************
020000090225     C     GESS0         BEGSR
020100090225     C*
020200090225     C* inizializzazione videata
020300090225     C     $INZS0        IFEQ      *ON
020400090225     C                   EXSR      INZS0
020500090225     C                   MOVE      *OFF          $INZS0
020600090225     C                   ENDIF
020700100325     C*
020800100325     C* tutte le selezioni
020900100325     c                   move      *all'S'       lda_lda
021000100325     c                   out       lda_selez
021100100325     C*
021200090225     C* emissione piede videata
021300090225     C                   WRITE     Z0
021400090225     C* Non ci sono records
021500090225     C     WMAX          IFEQ      0
021600090225     C                   WRITE     D0
021700090225     C                   Else
021800090225     C     Wsfl          IFgt      0
021900090225     C                   Z-ADD     wsfl          C0RCD
022000090225     C                   Else
022100090225     C     Wpag          IFgt      0
022200090225     C                   Z-ADD     wpag          C0RCD
022300090225     C                   EndIF
022400090225     C                   EndIF
022500090225     C                   ENDIF
022600090225     C*
022700090225     C*              *------------------*
022800090225     C                   EXFMT     C0
022900090225     C*              *------------------*
023000090225     C*
023100090225     C     C0NRR         IFNE      0
023200090225     C                   Z-ADD     C0NRR         WSFL
023300090225     C                   ENDIF
023400090225     C                   Z-ADD     SFLNRR        C0RCD
023500090225     C* Selezioni
0236000902251    C                   SELECT
023700090225     C* F3=Fine
023800090225     C     $TASTO        WHENEQ    F03
023900090225     C                   EXSR      F03S1
024000100121     C* F10=Immissione
024100100121     C     $TASTO        WHENEQ    F10
024200100121     C                   EXSR      F10S1
024300100325     C                   MOVE      'S0'          $GEST
024400100322     C* F17=Selezioni
024500100322     C     $TASTO        WHENEQ    F17
024600100322     C                   EXSR      F17S1
0247000902251O   C                   OTHER
024800090225     C* CONTROLLO DATI
024900090225     C                   EXSR      CTRC0
025000090225     C     *IN99         IFEQ      *OFF
025100090225     C                   EXSR      CTRS0
025200090225     C                   END
0253000902251-   C                   ENDSL
025400090225     C*
025500090225     C                   ENDSR
025600090225     C/EJECT
025700090225     C************************************************************
025800090225     C* INIZIALIZZAZIONE LISTA
025900090225     C************************************************************
026000090225     C     INZS0         BEGSR
026100100407     C*
026200090225     C* pulizia SFL
026300090225     C                   SETOFF                                         3031
026400090225     C                   WRITE     C0
026500090225     C                   SETON                                          31
026600090225     C*
026700100325     C*
026800090225     C* CARICAMENTO SFL totale
026900090225     C                   Z-ADD     0             S0NRR
027000090225     C                   Z-ADD     1             C0RCD
027100090225     C                   Z-ADD     0             WMAX
027200090225     C                   Z-ADD     0             wsfl
027300090225     C*
027400090225     C*  Posizionamento all'inizio
027500100326     c                   movel(p)  c0pos         tbe_FIL          15
027600100326     C                   IF        Posiziona = 'S'
027700100326     c     tbe_AUT_Fil   setll     tnTBE01L
027800100326     C                   eval      Posiziona = *blank
027900100326     c                   else
028000100119     c     tbe_AUT       setll     tnTBE01L
028100100326     c                   end
028200100119    >C                   EXSR      READ_TBE
028300090225     C* Carico il SFL
028400090225     C                   EXSR      ROLS0
028500090225     C*
028600090225     c                   if        xtaopr <> '1'
028700090225     C                   Z-ADD     1             WPAG
028800090225     c                   end
028900090225     C*
029000090225     C                   ENDSR
029100090225     C************************************************************
029200090225     C* CARICAMENTO PAGINA LISTA
029300090225     C************************************************************
029400090225     C     ROLS0         BEGSR
029500090225     C*
029600090225     C                   SETOFF                                       32
029700090225     C                   Z-ADD     0             Y
029800090225     C                   Z-ADD     WMAX          S0NRR
029900090225     C*
030000100119     C*  Selezionarle Tutte
030100100119     c                   eval      s0cod =  '999'
030200100119     c                   eval      s0des =  'TUTTE'
030300100119     C                   ADD       1             S0NRR
030400100119     C                   ADD       1             Y
030500100119     C                   WRITE     S0
030600100119     C*
030700090225     C* Leggo dal file i segmenti presenti nell'archivio
0308000902251    C     $EFILE        DOWEQ     *OFF
030900090225     C*
031000100119     c                   movel     tbeKe1        cod_FIL           3
031100100119     C*
031200100119     c                   if        cod_FIL <> sav_cod_FIL
031300090225     c*
031400100119     c                   clear                   s0opz
031500090225     c* Prendo tipo segmento generale
031600100119     c                   eval      s0cod =  cod_FIL
031700100119     c                   movel     cod_FIL       num_FIL           3 0
031800100119     c     num_FIL       chain     azorg01l
031900100119     c                   if        %Found(azorg01l)
032000100119     c                   eval      s0des =  orgDES
032100100119     c                   end
032200090225     C                   ADD       1             S0NRR
032300090225     C                   ADD       1             Y
032400090225     C                   WRITE     S0
032500100119     C*
032600100119     c                   eval       sav_cod_FIL = cod_FIL
032700100119     c                   end
032800100119     C*
032900100119    >C                   EXSR      READ_TBE
033000090225     C*
0331000902251-   C                   ENDDO
033200090225     C*
033300090225     C                   Z-ADD     S0NRR         WMAX                 30
033400090225     C*
033500090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
033600090225     C*
033700090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
033800090225     C                   MVR                     RESTO             3 0
033900090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0340000902251    C     RESTO         IFGT      0
034100090225     C                   ADD       1             C0RCD
0342000902251E   C                   ELSE
034300090225     C                   SUB       SFLPAG0       C0RCD
034400090225     C                   ADD       1             C0RCD
0345000902251-   C                   ENDIF
034600090225     C*
034700090225     C                   ENDSR
034800090225     C************************************************************
034900090225     C* LETTURA RCD
035000090225     C************************************************************
035100100119     C     READ_TBE      BEGSR
035200090225     C*
035300090225     C                   MOVEL     *OFF          $EFILE
035400090225     C                   MOVEL     *OFF          $RCDOK
035500090225     C*
0356000902251    C     $EFILE        DOUEQ     *ON
035700090225     C     $RCDOK        OREQ      *ON
035800090225     C*
035900100119     c     tbe_AUT       Reade     tntbe01L
036000100119     c                   if        %eof(tntbe01L)
036100090225     C                   MOVEL     *ON           $EFILE
036200090225     C                   MOVE      $EFILE        *IN33
036300090225     c                   else
036400100322      **
036500100322     c                   eXSr      Selez_RECord
036600100119     c                   end
036700090225     C*
0368000902251-   C                   ENDDO
036900090225     C*
037000090225     C                   ENDSR
037100090225     C************************************************************
037200090225     C* CONTROLLO TESTATA LISTA
037300090225     C************************************************************
037400100322     C     Selez_RECord  BEGSR
037500100407      *
037600100407      *  Se richiamato con Filiale e Profilo deve selezionare solo i records
037700100407      *    appartenenti alla Filiale/Profilo
037800100407     c                   if        autSelez = 'R' and autFILIA <> *blank and
037900100407     c                             autProf <> *blank
038000100407     c                   movel(p)  autFILIA      Fil_Profilo      13
038100100407     c                   move      autPROF       Fil_Profilo
038200100407     c                   movel(p)  Fil_Profilo   Fil_Prof_blk     15
038300100407      * se il profilo non corrisponde Scarta il record
038400100407     c                   If        Fil_Prof_blk <> tbeKE1
038500100407     c                   goto      End_filtro
038600100407     c                   end
038700100407     c                   end
038800100407     c*
038900100322     C*
039000100322     C*  imposta i flags appena letti nella DS
039100100322     c                   movel     tbeUNI        tbeUNI_daut
039200100322     C                   MOVEL     *Off          $RCDOK
039300100322     C*
039400100322     C*  SELEZIONI SUGLI AUTISTI
039500100322     C*  se c'è una selezione specifica sugli AUT
039600100322     c                   if            lda_AUTMTC     = 'S'   or
039700100322     c                                 lda_AUTSDT     = 'S'   or
039800100322     c                                 lda_AUTRET     = 'S'   or
039900100322     c                                 lda_AUTCON     = 'S'   or
040000100322     c                                 lda_AUTCRS     = 'S'   or
040100100322     c                                 lda_AUTSTDCC   = 'S'
040200100322      *
040300100322      *    verifica almeno una valida se "S" abilitato il profilo
040400100322     c                   if        lda_AUTMTC   ='S' and tbeUNI_§AUTMTC   ='S'
040500100322     c                               or
040600100322     c                             lda_AUTSDT   ='S' and tbeUNI_§AUTSDT   ='S'
040700100322     c                               or
040800100322     c                             lda_AUTRET   ='S' and tbeUNI_§AUTRET   ='S'
040900100322     c                               or
041000100322     c                             lda_AUTCON   ='S' and tbeUNI_§AUTCON   ='S'
041100100322     c                               or
041200100322     c                             lda_AUTCRS   ='S' and tbeUNI_§AUTCRS   ='S'
041300100322     c                               or
041400100322     c                             lda_AUTSTDCC ='S' and tbeUNI_§AUTSTDCC ='S'
041500100322     C                   MOVEL     *ON           $RCDOK
041600100322     c                   goto      End_filtro
041700100322     c                   end
041800100322      *
041900100322     c                   else
042000100322     C*  se non è stato selezionato nemmeno uno specifico
042100100322     C*   ma si vuole vedere chi ha autorizzazioni o negazioni sugli AUT
042200100322     c                   if            lda_AUT        = 'S'
042300100322      *  Chi tratta gli AUT
042400100322     c                   if        tbeUNI_§AUTMTC   = 'S'
042500100322     c                               or
042600100322     c                             tbeUNI_§AUTSDT   = 'S'
042700100322     c                               or
042800100322     c                             tbeUNI_§AUTRET   = 'S'
042900100322     c                               or
043000100322     c                             tbeUNI_§AUTCON   = 'S'
043100100322     c                               or
043200100322     c                             tbeUNI_§AUTCRS   = 'S'
043300100322     c                               or
043400100322     c                             tbeUNI_§AUTSTDCC = 'S'
043500100322     C                   MOVEL     *ON           $RCDOK
043600100322     c                   goto      End_filtro
043700100322     c                   end
043800100322     c                   endif
043900100322      *
044000100322     c                   end
044100100322     C*-----------
044200100322     C*  SELEZIONI SULLE COOP
044300100322     c                   if            lda_AUTVTCC   = 'S'   or
044400100322     c                                 lda_AUTMTCC   = 'S'   or
044500100322     c                                 lda_AUTSDTC   = 'S'   or
044600100322     c                                 lda_AUTRETC   = 'S'   or
044700100322     c                                 lda_AUTCRSC   = 'S'   or
044800100322     c                                 lda_AUTCONC   = 'S'   or
044900100322     c                                 lda_AUTGSTM   = 'S'
045000100322      *
045100100322      *    verifica almeno una valida se "S" abilitato il profilo
045200100322     c                   if        lda_AUTVTCC  ='S' and tbeUNI_§AUTVTCC  ='S'
045300100322     c                               or
045400100322     c                             lda_AUTMTCC  ='S' and tbeUNI_§AUTMTCC  ='S'
045500100322     c                               or
045600100322     c                             lda_AUTSDTC  ='S' and tbeUNI_§AUTSDTC  ='S'
045700100322     c                               or
045800100322     c                             lda_AUTRETC  ='S' and tbeUNI_§AUTRETC  ='S'
045900100322     c                               or
046000100322     c                             lda_AUTCRSC  ='S' and tbeUNI_§AUTCRSC  ='S'
046100100322     c                               or
046200100322     c                             lda_AUTCONC  ='S' and tbeUNI_§AUTCONC  ='S'
046300100322     c                               or
046400100322     c                             lda_AUTGSTM  ='S' and tbeUNI_§AUTGSTM  ='S'
046500100322     C                   MOVEL     *ON           $RCDOK
046600100322     c                   goto      End_filtro
046700100322     c                   end
046800100322      *
046900100322     C*  se non è stato selezionato nemmeno uno specifico
047000100322     C*   ma si vuole vedere chi ha autorizzazioni o negazioni sulle COOP
047100100322     c                   else
047200100322      *
047300100322     c                   if            lda_COOP      = 'S'
047400100322      *  Chi tratta gli AUT
047500100322     c                   if        tbeUNI_§AUTVTCC  = 'S'
047600100322     c                               or
047700100322     c                             tbeUNI_§AUTMTCC  = 'S'
047800100322     c                               or
047900100322     c                             tbeUNI_§AUTSDTC  = 'S'
048000100322     c                               or
048100100322     c                             tbeUNI_§AUTRETC  = 'S'
048200100322     c                               or
048300100322     c                             tbeUNI_§AUTCRSC  = 'S'
048400100322     c                               or
048500100322     c                             tbeUNI_§AUTCONC  = 'S'
048600100322     c                               or
048700100322     c                             tbeUNI_§AUTGSTM  = 'S'
048800100322     C                   MOVEL     *ON           $RCDOK
048900100322     c                   goto      End_filtro
049000100322     c                   end
049100100322     c                   end
049200100322      *
049300100322     c                   end
049400100322     C*-----------
049500100322     C*  SELEZIONI SULLE AFFLUENZE/DEFLUENZE
049600100322     c                   if            lda_AFFMTC    = 'S'   or
049700100322     c                                 lda_AFFSDT    = 'S'   or
049800100322     c                                 lda_AFFRET    = 'S'   or
049900100322     c                                 lda_AFFCON    = 'S'   or
050000100322     c                                 lda_AFFCRS    = 'S'   or
050100100322     c                                 lda_AFFGVSTD  = 'S'   or
050200100322     c                                 lda_AFFAGV    = 'S'   or
050300100322     c                                 lda_AFFCPSTD  = 'S'   or
050400100322     c                                 lda_AFFARRM   = 'S'
050500100322      *
050600100322      *    verifica almeno una valida se "S" abilitato il profilo
050700100322     c                   if        lda_AFFMTC   ='S' and tbeUNI_§AFFMTC   ='S'
050800100322     c                               or
050900100322     c                             lda_AFFSDT   ='S' and tbeUNI_§AFFSDT   ='S'
051000100322     c                               or
051100100322     c                             lda_AFFRET   ='S' and tbeUNI_§AFFRET   ='S'
051200100322     c                               or
051300100322     c                             lda_AFFCON   ='S' and tbeUNI_§AFFCON   ='S'
051400100322     c                               or
051500100322     c                             lda_AFFCRS   ='S' and tbeUNI_§AFFCRS   ='S'
051600100322     c                               or
051700100322     c                             lda_AFFGVSTD ='S' and tbeUNI_§AFFGVSTD ='S'
051800100322     c                               or
051900100322     c                             lda_AFFAGV   ='S' and tbeUNI_§AFFAGV   ='S'
052000100322     c                               or
052100100322     c                             lda_AFFCPSTD ='S' and tbeUNI_§AFFCPSTD ='S'
052200100322     c                               or
052300100322     c                             lda_AFFARRM  ='S' and tbeUNI_§AFFARRM  ='S'
052400100322     C                   MOVEL     *ON           $RCDOK
052500100322     c                   goto      End_filtro
052600100322     c                   end
052700100322      *
052800100322     C*  se non è stato selezionato nemmeno uno specifico
052900100322     C*   ma si vuole vedere chi ha autorizzazioni o negazioni sulle AFFL/DEFL
053000100322     c                   else
053100100322     C*
053200100322     c                   if            lda_AFDE      = 'S'
053300100322      *  Chi tratta le Affluenze/Defluenze
053400100322     c                   if        tbeUNI_§AFFMTC   = 'S'
053500100322     c                               or
053600100322     c                             tbeUNI_§AFFSDT   = 'S'
053700100322     c                               or
053800100322     c                             tbeUNI_§AFFRET   = 'S'
053900100322     c                               or
054000100322     c                             tbeUNI_§AFFCON   = 'S'
054100100322     c                               or
054200100322     c                             tbeUNI_§AFFCRS   = 'S'
054300100322     c                               or
054400100322     c                             tbeUNI_§AFFGVSTD = 'S'
054500100322     c                               or
054600100322     c                             tbeUNI_§AFFAGV   = 'S'
054700100322     c                               or
054800100322     c                             tbeUNI_§AFFCPSTD = 'S'
054900100322     c                               or
055000100322     c                             tbeUNI_§AFFARRM  = 'S'
055100100322     C                   MOVEL     *ON           $RCDOK
055200100322     c                   goto      End_filtro
055300100322     c                   end
055400100322     c                   end
055500100322     C*
055600100322     c                   end
055700100322     C*
055800100322     C*-----------
055900100322     C*  SELEZIONI SU ALTRO
056000100322     C*
056100100322     c                   if            lda_RIEABI    = 'S'   or
056200100322     c                                 lda_AUTCPRRE  = 'S'
056300100322      *
056400100322      *    verifica almeno una valida se "S" abilitato il profilo
056500100322     c                   if        lda_RIEABI   ='S' and tbeUNI_§RIEABI   ='S'
056600100322     c                               or
056700100322     c                             lda_AUTCPRRE ='S' and tbeUNI_§AUTCPRRE ='S'
056800100322     C                   MOVEL     *ON           $RCDOK
056900100322     c                   goto      End_filtro
057000100322     c                   end
057100100322     C*
057200100324     c                   end
057300100322     C*
057400100322     C     End_filtro    ENDSR
057500100322     C************************************************************
057600100322     C* CONTROLLO TESTATA LISTA
057700100322     C************************************************************
057800100322     C     CTRC0         BEGSR
057900100322     C*
058000100326     C                   MOVE      *OFF          *IN99
058100100326     C*
058200100326     C                   clear                   posiziona         1
058300100326     c                   if        c0pos <> SAVc0pos
058400100326     C                   move      'S'           posiziona         1
058500100326     c                   eval      SAVc0pos = c0pos
058600100326     c                   move      *ON           $INZS0
058700100326     c                   end
058800090225     C*
058900090225     C                   ENDSR
059000090225     C************************************************************
059100090225     C* CONTROLLO OPZIONI LISTA
059200090225     C************************************************************
059300090225     C     CTRS0         BEGSR
059400090225     C*
059500090225     C                   MOVEL     *OFF          $ESCI
059600090225     C                   SETOFF                                       99
059700090225     C                   clear                   S0OPZ
059800090225     c                   clear                   x0cod
059900090225     C*
060000090225     C* Leggo il sfl solo se ci sono rcd
0601000902251    C     WMAX          IFGT      0
060200090225     C                   READC     S0                                     21
060300090225     C*
060400090225     C* esce se fine sfl o errore che richiede l'uscita
0605000902252    C     *IN21         DOWEQ     *OFF
060600090225     C     $ESCI         ANDEQ     *OFF
060700090225     C                   Z-ADD     S0NRR         C0RCD
060800090225     C* ctrl su riga
060900090225     C                   EXSR      RECS0
061000090225     C* gestione opzioni
0611000902253    C     S0OPZ         IFNE      *blank
061200090225     C     *IN99         ANDEQ     *OFF
061300090225     C                   EXSR      OPZS0
0614000902253-   C                   ENDIF
061500090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0616000902253    C     *IN99         IFEQ      *ON
061700090225     C     $ESCI         OREQ      *ON
061800090225     C                   MOVE      *ON           *IN32
061900090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
062000090225     C* la ripristinerò a conclusione del ciclo di READC
062100090225     C                   MOVE      *OFF          $INZS0
0622000902253-   C                   ENDIF
062300090225     C*
062400090225     C                   clear                   S0OPZ
062500090225     C*
062600090225     C                   UPDATE    S0
062700090225     C*
062800090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0629000902253    C     $ESCI         IFEQ      *OFF
063000090225     C                   READC     S0                                     21
063100090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0632000902254    C     *IN21         IFEQ      *ON
063300090225     C                   MOVE      WINZS0        $INZS0
063400090225     C* calcolo pagina a cui deve posizionarsi
063500090225     C                   EXSR      CLCPAG0
0636000902254-   C                   ENDIF
0637000902253-   C                   ENDIF
063800090225     C*
0639000902252-   C                   ENDDO
064000090225     C*
0641000902251-   C                   ENDIF
064200090225     C*
064300090225     C                   ENDSR
064400090225     C/EJECT
064500090225     C************************************************************
064600090225     C* CONTROLLO CAMPI I/O RIGA LISTA
064700090225     C************************************************************
064800090225     C     RECS0         BEGSR
064900090225     C*
065000090225     C* reset indicatori DSPATR
065100090225     C                   MOVE      *ALL'0'       IN4049           10
065200090225     C                   MOVEA     IN4049        *IN(40)
065300090225     C*
065400090225     C                   ENDSR
065500090225     C************************************************************
065600090225     C* GESTIONE OPZIONI LISTA
065700090225     C************************************************************
065800090225     C     OPZS0         BEGSR
065900090225     C*
066000090225      * selezionato un codice da restituire al chiamante
0661000902253    C     S0OPZ         IFeq      '1'
066200100119     c                   move      s0cod         CodiceFIL         3
066300090225     C                   MOVE      'S1'          $GEST
066400090225     C                   MOVEL     *ON           $ESCI
066500090225     C                   END
066600090225     C*
066700090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0668000902252    C     *IN99         IFEQ      *ON
066900090225     C                   SETON                                        40
0670000902252-   C                   ENDIF
067100090225     C*
067200090225     C                   ENDSR
067300090225     C/EJECT
067400940127     C************************************************************
067500940131     C* GESTIONE LISTA
067600940127     C************************************************************
067700940127     C     GESS1         BEGSR
067800030113     C*
067900940223     C* inizializzazione videata
068000940223     C     $INZS1        IFEQ      *ON
068100940127     C                   EXSR      INZS1
068200940223     C                   MOVE      *OFF          $INZS1
068300940127     C                   ENDIF
068400030113     C*
068500030113     C* emissione piede videata
068600030113     C                   WRITE     Z1
068700030113     C* Non ci sono records
068800940223     C     WMAX          IFEQ      0
068900940607     C                   WRITE     D1
069000030114     C                   Else
069100030114     C     Wsfl          IFgt      0
069200030114     C                   Z-ADD     wsfl          C1RCD
069300030114     C                   Else
069400030114     C     Wpag          IFgt      0
069500030114     C                   Z-ADD     wpag          C1RCD
069600030114     C                   EndIF
069700030114     C                   EndIF
069800030114     C                   ENDIF
069900940127     C*
070000030113     C*              *------------------*
070100940607     C                   EXFMT     C1
070200030113     C*              *------------------*
070300030113     C*
070400940204     C     C1NRR         IFNE      0
070500940204     C                   Z-ADD     C1NRR         WSFL
070600940204     C                   ENDIF
070700940127     C                   Z-ADD     SFLNRR        C1RCD
070800030113     C* Selezioni
0709009401271    C                   SELECT
071000940127     C* F3=Fine
071100940506     C     $TASTO        WHENEQ    F03
071200940309     C                   EXSR      F03S1
071300940131     C* F10=Immissione
071400940506     C     $TASTO        WHENEQ    F10
071500940309     C                   EXSR      F10S1
071600100325     C                   MOVE      'S1'          $GEST
071700090225     C* F12=Ritorno
071800090225     C     $TASTO        WHENEQ    F12
071900090225     C                   eval      $Gest='S0'
072000090225     C                   MOVE      *ON           $INZS1
072100090225     C                   MOVE      *ON           $INZS0
072200100923      *  se richiamato da pgm autorizzazioni
072300100923     c                   if        KCDAZ ='BS30'
072400100923     C                   MOVE      *ON           $FINE
072500100923     c                   end
072600100407      *
072700100326     C* se c'era un posizionamento
072800100326     c                   if        c0POS <> *blank
072900100326     c                   eval      SAVc0pos = c0pos
073000100326     c                   eval      posiziona = 'S'
073100100326     c                   end
073200100407      *
073300100407     c                   eval      autFILIA = *blank
073400100407     c                   eval      autPROF  = *blank
073500100407      *
073600100322     C* F17=Selezioni
073700100322     C     $TASTO        WHENEQ    F17
073800100322     C                   EXSR      F17S1
0739009401271O   C                   OTHER
074000940127     C* CONTROLLO DATI
074100940131     C                   EXSR      CTRC1
074200940201     C     *IN99         IFEQ      *OFF
074300940131     C                   EXSR      CTRS1
074400940131     C                   END
0745009401271-   C                   ENDSL
074600940127     C*
074700940127     C                   ENDSR
074800940224     C/EJECT
074900940127     C************************************************************
075000940131     C* INIZIALIZZAZIONE LISTA
075100940127     C************************************************************
075200940127     C     INZS1         BEGSR
075300100322     C*
075400100322     C*  Legge l'Area dati e poi la rilascia
075500100322     c                   in        LDA_Selez
075600100322     C*
075700940302     C* pulizia SFL
075800100121     C                   SETOFF                                       423031
075900940607     C                   WRITE     C1
076000940128     C                   SETON                                          31
076100940128     C*
076200030113     C* CARICAMENTO SFL totale
076300940201     C                   Z-ADD     0             S1NRR
076400030113     C                   Z-ADD     1             C1RCD
076500940128     C                   Z-ADD     0             WMAX
076600090225     C                   Z-ADD     0             wsfl
076700100121     c                   if        CodiceFIL = *blank
076800100121     c                   eval      CodiceFIL = '999'
076900100121     c                   end
077000100119     C                   move      CodiceFIL     c1FIL
077100940224     C*
077200940224     C* Posizionamento su file pilota
077300100119     c     tbe_AUT       setll     tntbe01L
077400100119    >C                   EXSR      READ_tbe
077500030113     C* Carico il SFL
077600940127     C                   EXSR      ROLS1
077700030113     C*
077800030114     c                   if        xtaopr <> '1'
077900030114     C                   Z-ADD     1             WPAG
078000030114     c                   end
078100940127     C*
078200940127     C                   ENDSR
078300940127     C************************************************************
078400940131     C* CARICAMENTO PAGINA LISTA
078500940127     C************************************************************
078600940127     C     ROLS1         BEGSR
078700940127     C*
078800940128     C                   SETOFF                                       32
078900940223     C                   Z-ADD     0             Y
079000940127     C                   Z-ADD     WMAX          S1NRR
079100940127     C*
079200940127     C* Leggo dal file anagrafico per caricare la lista
0793009401311    C     $EFILE        DOWEQ     *OFF
079400100119     C*
079500100119     c                   movel     tbeKe1        cod_FIL           3
079600100119     C*
079700100119     c                   if        cod_FIL = c1FIL or c1FIL = '999'
079800100317     c*
079900030113     c                   clear                   s1opz
080000100324     c                   eval      h1cod = tbeKE1
080100100324     c                   movel(p)  tbeke1        s1cod
080200100324     c                   eval      s1cod1 = %subst(tbeKE1:4:10)
080300100317     c                   movel     tbeKE2        s1cod2
080400100119     c                   clear                   s1des
080500100324     c                   clear                   s1rcv
080600100317     c                   clear                   num_FIL           3 0
080700100317     c                   movel     s1cod2        num_FIL
080800100317     c                   if        num_FIL <> 0
080900100119     c     num_FIL       chain     azorg01l
081000100119     c                   if        %Found(azorg01l)
081100100119     c                   eval      s1des =  orgDES
081200100119     c                   end
081300100119     c                   end
081400100119     c                   if        tbeDTR <> 0
081500100119     c                   move      tbeDTR        dataiso
081600100119     c                   move      dataiso       dataeur
081700100317     c                   moveL     dataeur       s1rcv
081800100119     c                   end
081900100119     c                   eval      s1dati = tbeUNI
082000940127     C*
082100100323     c                   eval      h1ANNulla = *off
082200100121     c                   if        tbeATB <> *blank
082300100323     c                   move(p)   'ANNULLATO'   s1des
082400100323     c                   eval      h1ANNulla = *on
082500100121     c                   seton                                        42
082600100121     c                   end
082700100121     C*
082800940127     C                   ADD       1             S1NRR
082900940127     C                   ADD       1             Y
083000940607     C                   WRITE     S1
083100100121     c                   setoff                                       42
083200100119     c                   end
083300940131     C*
083400100119    >C                   EXSR      READ_tbe
0835009401271-   C                   ENDDO
083600940127     C*
083700940223     C                   Z-ADD     S1NRR         WMAX                 30
083800940127     C*
083900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
084000090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
084100940127     C                   MVR                     RESTO             3 0
084200090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0843000301141    C     RESTO         IFGT      0
084400030114     C                   ADD       1             C1RCD
0845000301141E   C                   ELSE
084600090225     C                   SUB       SFLPAG1       C1RCD
084700030114     C                   ADD       1             C1RCD
0848000301141-   C                   ENDIF
084900940128     C*
085000940127     C                   ENDSR
085100940224     C************************************************************
085200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
085300940224     C************************************************************
085400090225     C     CLCPAG0       BEGSR
085500940224     C* Input :
085600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
085700940224     C*          cursore
085800940224     C* - SFLPAG = numero rcd per pagina sfl
085900940224     C* Output :
086000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
086100940224     C*
086200090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
086300940224     C                   MVR                     RESTO             3 0
086400940224     C     RESTO         IFGT      0
086500940224     C                   ADD       1             PAGINE
086600940224     C                   ENDIF
086700090226     c                   if        pagine > 1
086800090225     C     PAGINE        MULT      SFLPAG0       WPAG
086900090226     C                   END
087000940224     C*
087100940224     C                   ENDSR
087200090225     C************************************************************
087300090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
087400090225     C************************************************************
087500090225     C     CLCPAG1       BEGSR
087600090225     C* Input :
087700090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
087800090225     C*          cursore
087900090225     C* - SFLPAG = numero rcd per pagina sfl
088000090225     C* Output :
088100090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
088200090225     C*
088300090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
088400090225     C                   MVR                     RESTO             3 0
088500090225     C     RESTO         IFGT      0
088600090225     C                   ADD       1             PAGINE
088700090225     C                   ENDIF
088800090226     c                   if        pagine > 1
088900090225     C     PAGINE        MULT      SFLPAG1       WPAG
089000090226     C                   ENDIF
089100090225     C*
089200090225     C                   ENDSR
089300940309     C************************************************************
089400940309     C* GESTIONE F03 VIDEO S1
089500940309     C************************************************************
089600940309     C     F03S1         BEGSR
089700940309     C*
089800940309     C                   MOVE      *ON           $FINE
089900940325     C* fine programma
090000940309     C                   ENDSR
090100940309     C/EJECT
090200940309     C************************************************************
090300940309     C* GESTIONE F10 VIDEO S1
090400940314     c* AGGIUNTA RECORD
090500940309     C************************************************************
090600940309     C     F10S1         BEGSR
090700940309     C*
090800030113     C                   RESET                   tabds
090900030113     C                   MOVEL     '01'          xtaopz
091000030113     C                   MOVE      *ZERO         xtaret
091100030113     C                   MOVE      *ZERO         xtaopr
091200100409     C                   MOVEl     *blank        xtakey1
091300100726     C                   if        AUTSELEZ ='R'
091400100726     C                   eval       xtakey1 = AUTFILIA + AUTPROF
091500100726     c                   end
091600100409     C                   MOVEl     *blank        xtakey2
091700100409     C                   MOVEl     'AUT'         xtacod
091800030113     C                   MOVE      *BLANKS       KPJBU
091900030113     C                   MOVEL     tabds         KPJBU
092000100119$004 C                   CALL      'TNTBAUTR2'
092100030113     C                   PARM                    KPJBA
092200090225      *
092300030114     C                   MOVEL     KPJBU         tabds
092400030113      *
092500940309     C* ritorno da PGM gestione
092600940309     C                   EXSR      GESRET
092700090226      *
092800940309     C     WINZS1        IFEQ      *ON
092900940309     C                   MOVE      *ON           $INZS1
093000090226     C* carico sempre la 1a pagina
093100090226     C                   Z-ADD     1             WPAG
093200940309     C                   ENDIF
093300940309     C*
093400940309     C                   ENDSR
093500940128     C************************************************************
093600100322     C* Ha richiamato il programma per scegliere particolari profili
093700940128     C************************************************************
093800100322     C     F17S1         BEGSR
093900100322     C*
094000100322     C* reinizializza il SFL poichè lo deve ricaricare
094100100322     C                   MOVE      *ON           $INZS1
094200940128     C*
094300100322      *  I parametri di selezione sono nella Local Data Area condivisa dai 2 pgm
094400100322     c                   call      'TNTBAUTR1'
094500100322     c                   parm                    kpjba
094600100322      *
094700100322     C*
094800100322     C                   ENDSR
094900100322     C************************************************************
095000100322     C* CONTROLLO TESTATA LISTA
095100100322     C************************************************************
095200100322     C     CTRC1         BEGSR
095300100322     C*
095400940201     C                   MOVE      *OFF          *IN99
095500940131     C*
095600940202     C                   ENDSR
095700940131     C************************************************************
095800940131     C* CONTROLLO OPZIONI LISTA
095900940131     C************************************************************
096000940131     C     CTRS1         BEGSR
096100940131     C*
096200940202     C                   MOVEL     *OFF          $ESCI
096300940201     C                   SETOFF                                       99
096400940131     C                   Z-ADD     0             S1OPZ
096500040930     c                   clear                   x1cod
096600940131     C*
096700940127     C* Leggo il sfl solo se ci sono rcd
0968009401311    C     WMAX          IFGT      0
096900940607     C                   READC     S1                                     21
097000940127     C*
097100940131     C* esce se fine sfl o errore che richiede l'uscita
0972009401312    C     *IN21         DOWEQ     *OFF
097300940131     C     $ESCI         ANDEQ     *OFF
097400940201     C                   Z-ADD     S1NRR         C1RCD
097500940131     C* ctrl su riga
097600940131     C                   EXSR      RECS1
097700940131     C* gestione opzioni
0978009401313    C     S1OPZ         IFNE      0
097900940201     C     *IN99         ANDEQ     *OFF
098000940131     C                   EXSR      OPZS1
0981009401273-   C                   ENDIF
098200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0983009402013    C     *IN99         IFEQ      *ON
098400940201     C     $ESCI         OREQ      *ON
098500940131     C                   MOVE      *ON           *IN32
098600940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
098700940204     C* la ripristinerò a conclusione del ciclo di READC
098800940223     C                   MOVE      *OFF          $INZS1
0989009402233-   C                   ENDIF
099000940223     C*
099100100323     C  n99              Z-ADD     0             S1OPZ
099200100121      ****
099300100323     c     h1annulla     comp      *on                                    42
099400940223     C*
099500940607     C                   UPDATE    S1
099600940223     C*
099700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0998009401313    C     $ESCI         IFEQ      *OFF
099900940607     C                   READC     S1                                     21
100000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1001009402014    C     *IN21         IFEQ      *ON
100200940201     C                   MOVE      WINZS1        $INZS1
100300940204     C* calcolo pagina a cui deve posizionarsi
100400090225     C                   EXSR      CLCPAG1
1005009402014-   C                   ENDIF
1006009402013-   C                   ENDIF
100700940131     C*
1008009401272-   C                   ENDDO
100900940127     C*
1010009401311-   C                   ENDIF
101100940131     C*
101200940127     C                   ENDSR
101300940127     C/EJECT
101400940127     C************************************************************
101500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
101600940127     C************************************************************
101700940131     C     RECS1         BEGSR
101800940131     C*
101900940201     C* reset indicatori DSPATR
102000940201     C                   MOVE      *ALL'0'       IN4049           10
102100940201     C                   MOVEA     IN4049        *IN(40)
102200100323     C*
102300100323     C*  se si è selezionato di ripristinare un record NON annullato
1024001003233    C     S1OPZ         IFeq      8
1025001003233    C     H1ANNulla     andeq     *off
102600100323     c                   seton                                        41  99
102700100323     C                   END
102800100323     C*
102900100323     C*  se invece il record era annullato si può solo vedere o ripristinare
1030001003233    C     H1ANNulla     ifeq      *on
1031001003233    C     S1OPZ         andne     8
1032001003233    C     S1OPZ         andne     5
103300100323     c                   seton                                        41  99
103400100323     C                   END
103500100323     C*
103600100323     C*  se richiamato x selezionare un codice, --> 11 = *on
103700100323     C*   non deve utilizzare le opzioni di modifica e viceversa
103800040930     c     *in11         ifeq      *on
1039000409303    C     S1OPZ         IFeq      2
1040000409303    C     S1OPZ         OReq      3
1041000409303    C     S1OPZ         OReq      4
1042001003233    C     S1OPZ         OReq      8
104300040930     c                   seton                                        41  99
104400040930     C                   END
104500100323     C*
104600040930      * selezionato un codice da restituire al chiamante
1047000409303    C     S1OPZ         IFeq      1
104800100324     c                   move      h1cod         x1cod
104900040930     c                   clear                   kpjbu
105000040930     c                   movel     x1cod         kpjbu
105100040930     C                   MOVEL     *ON           $ESCI
105200040930     C                   MOVEL     *ON           $fine
105300040930     C                   END
105400040930     C                   END
105500040930     C*
105600040930     c     *in11         ifeq      *off
1057000409303    C     S1OPZ         ANDeq     1
105800040930     c                   seton                                        41  99
105900040930     C                   END
106000040930     C*
106100940131     C                   ENDSR
106200940131     C************************************************************
106300940131     C* GESTIONE OPZIONI LISTA
106400940131     C************************************************************
106500940131     C     OPZS1         BEGSR
106600940201     C*
1067000409302    C     *IN11         IFEQ      *Off
1068000409302    C     *IN11         oreq      *On
1069000409302    C     s1opz         andeq     05
107000040930     C*
107100030113     C                   RESET                   tabds
107200100323     c                   move      *zero         xtaopz
107300100323     C                   MOVE      S1OPZ         xtaopz
107400030113     C                   MOVE      *ZERO         xtaret
107500030113     C                   MOVE      *ZERO         xtaopr
107600100409     C                   MOVEl(p)  h1COD         xtakey1
107700100409     C                   MOVEl(p)  s1COD2        xtakey2
107800100409     C                   MOVEl     'AUT'         xtacod
107900940715     C                   MOVE      *BLANKS       KPJBU
108000030113     C                   MOVEL     tabds         KPJBU
108100100119$004 C                   CALL      'TNTBAUTR2'
108200940607     C                   PARM                    KPJBA
108300090225      *
108400030114     C                   MOVEL     KPJBU         tabds
108500940201     C*
108600940223     C* ritorno da PGM gestione
108700940223     C                   EXSR      GESRET
108800040930     C*
108900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1090009402252    C     *IN99         IFEQ      *ON
109100940223     C                   SETON                                        40
1092009402252-   C                   ENDIF
109300940225     C*
1094000409302-   C                   ENDIF
109500040930     C*
109600940131     C                   ENDSR
109700940223     C/EJECT
109800940223     C************************************************************
109900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
110000940223     C************************************************************
110100940223     C     GESRET        BEGSR
110200940223     C*
110300940223     C* modo di ritorno
110400940223     C*
1105009402231    C                   SELECT
110600100923     C*  per il filtro delle filiali al rientro.
110700100923    >C     kcdaz         WHENEQ    'BS30'
110800100923     C                   MOVE      *ON           $ESCI
110900100923     C*******            MOVE      *ON           $FINE
111000100923     C                   MOVE      'S1'          $GEST
111100100923     C*
111200940314    >C* << questi modi di utilizzo dei valori di ritorno dal
111300940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
111400940314    >C*    proposte, normalmente sempre valide, ma modificabili
111500940314    >C*    per situazioni particolari >>
111600940223     C* 1 = F3
111700030113    >C     xtaret        WHENEQ    '1'
111800940224     C                   MOVE      *ON           $ESCI
111900940223     C                   MOVE      *ON           $FINE
112000940223     C* 2 = F12
112100030113    >C     xtaret        WHENEQ    '2'
112200940223     C                   MOVE      *ON           $ESCI
112300100923     C*
112400100923     C* Se richiamato dal programma delle autorizzazioni non deve passare
1125009402231-   C                   ENDSL
112600940223     C*
112700940223     C* operazione eseguite dal pgm chiamato
112800940223     C*
1129009402231    C                   SELECT
113000940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
113100030113    >C     xtaopr        WHENEQ    '1'
113200940223     C                   MOVE      *ON           WINZS1
113300940223     C*
1134009402231-   C                   ENDSL
113500940223     C*
113600940223     C* funzione non eseguibile per errore :
113700940223     C*
1138009402231    C                   SELECT
113900940223     C* 1 = funzione richiamata chiusa in errore
114000940316    >C*  eventualmente gestire altri codici di errore
114100030113    >C     xtaerr        WHENEQ    '1'
114200940223     C                   MOVE      *ON           $ESCI
114300940223     C                   SETON                                        5299
114400940223     C*
1145009402231-   C                   ENDSL
114600940223     C*
114700940223     C                   ENDSR
114800940223     C/EJECT
114900940131     C************************************************************
115000940131     C* OPERAZIONI INIZIALI
115100940131     C************************************************************
115200940131     C     *INZSR        BEGSR
115300030113     C*
115400030113     C* Reperimento parametri
115500030113     C     *ENTRY        PLIST
115600030113     C                   PARM                    KPJBA
115700100407     C                   movel     kpjbu         TNTBautDS
115800040930     C*
115900100326     c     tbe_AUT_Fil   Klist
116000100326     c                   kfld                    tbe_AUT
116100100326     c                   kfld                    tbe_FIL          15
116200100326     C*
116300100322     C* definisce la Local Data Area per selezionare i profili
116400100322     C     *DTAARA       DEFINE    *LDA          LDA_Selez
116500100322     C*
116600040930     C* Se chiamato x selezionare un codice
116700040930     c                   clear                   x1cod
116800100407     c                   if        autSELEZ = '?'
116900040930     c                   seton                                        11
117000040930     c                   end
117100030113     C*
117200030113     C* Variabili per gestione videate
117300030113     C                   MOVE      *BLANK        $GEST             2
117400030113     C                   MOVE      *BLANK        $FINE             1
117500100319     C                   MOVE      *BLANK        $INZS2            1
117600030113     C                   MOVE      *BLANK        $INZS1            1
117700090225     C                   MOVE      *BLANK        $INZS0            1
117800030113     C                   MOVE      *BLANK        $EFILE            1
117900030113     C                   MOVE      *BLANK        $ESCI             1
118000030113     C                   MOVE      *BLANK        $RCDOK            1
118100030113     C* Indici
118200030113     C                   Z-ADD     0             X                 3 0
118300030113     C                   Z-ADD     0             Y                 3 0
118400940506     C*
118500940506     C* Reperimento tasti di funzione
118600100326     C*
118700100326     C                   clear                   c0pos
118800100326     C                   clear                   SAVc0pos
118900940127     C*
119000940117     C                   ENDSR
119100100319     C************************************************************
