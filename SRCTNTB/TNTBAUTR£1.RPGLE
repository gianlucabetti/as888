000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
072200100407      *
072300100326     C* se c'era un posizionamento
072400100326     c                   if        c0POS <> *blank
072500100326     c                   eval      SAVc0pos = c0pos
072600100326     c                   eval      posiziona = 'S'
072700100326     c                   end
072800100407      *
072900100407     c                   eval      autFILIA = *blank
073000100407     c                   eval      autPROF  = *blank
073100100407      *
073200100322     C* F17=Selezioni
073300100322     C     $TASTO        WHENEQ    F17
073400100322     C                   EXSR      F17S1
0735009401271O   C                   OTHER
073600940127     C* CONTROLLO DATI
073700940131     C                   EXSR      CTRC1
073800940201     C     *IN99         IFEQ      *OFF
073900940131     C                   EXSR      CTRS1
074000940131     C                   END
0741009401271-   C                   ENDSL
074200940127     C*
074300940127     C                   ENDSR
074400940224     C/EJECT
074500940127     C************************************************************
074600940131     C* INIZIALIZZAZIONE LISTA
074700940127     C************************************************************
074800940127     C     INZS1         BEGSR
074900100322     C*
075000100322     C*  Legge l'Area dati e poi la rilascia
075100100322     c                   in        LDA_Selez
075200100322     C*
075300940302     C* pulizia SFL
075400100121     C                   SETOFF                                       423031
075500940607     C                   WRITE     C1
075600940128     C                   SETON                                          31
075700940128     C*
075800030113     C* CARICAMENTO SFL totale
075900940201     C                   Z-ADD     0             S1NRR
076000030113     C                   Z-ADD     1             C1RCD
076100940128     C                   Z-ADD     0             WMAX
076200090225     C                   Z-ADD     0             wsfl
076300100121     c                   if        CodiceFIL = *blank
076400100121     c                   eval      CodiceFIL = '999'
076500100121     c                   end
076600100119     C                   move      CodiceFIL     c1FIL
076700940224     C*
076800940224     C* Posizionamento su file pilota
076900100119     c     tbe_AUT       setll     tntbe01L
077000100119    >C                   EXSR      READ_tbe
077100030113     C* Carico il SFL
077200940127     C                   EXSR      ROLS1
077300030113     C*
077400030114     c                   if        xtaopr <> '1'
077500030114     C                   Z-ADD     1             WPAG
077600030114     c                   end
077700940127     C*
077800940127     C                   ENDSR
077900940127     C************************************************************
078000940131     C* CARICAMENTO PAGINA LISTA
078100940127     C************************************************************
078200940127     C     ROLS1         BEGSR
078300940127     C*
078400940128     C                   SETOFF                                       32
078500940223     C                   Z-ADD     0             Y
078600940127     C                   Z-ADD     WMAX          S1NRR
078700940127     C*
078800940127     C* Leggo dal file anagrafico per caricare la lista
0789009401311    C     $EFILE        DOWEQ     *OFF
079000100119     C*
079100100119     c                   movel     tbeKe1        cod_FIL           3
079200100119     C*
079300100119     c                   if        cod_FIL = c1FIL or c1FIL = '999'
079400100317     c*
079500030113     c                   clear                   s1opz
079600100324     c                   eval      h1cod = tbeKE1
079700100324     c                   movel(p)  tbeke1        s1cod
079800100324     c                   eval      s1cod1 = %subst(tbeKE1:4:10)
079900100317     c                   movel     tbeKE2        s1cod2
080000100119     c                   clear                   s1des
080100100324     c                   clear                   s1rcv
080200100317     c                   clear                   num_FIL           3 0
080300100317     c                   movel     s1cod2        num_FIL
080400100317     c                   if        num_FIL <> 0
080500100119     c     num_FIL       chain     azorg01l
080600100119     c                   if        %Found(azorg01l)
080700100119     c                   eval      s1des =  orgDES
080800100119     c                   end
080900100119     c                   end
081000100119     c                   if        tbeDTR <> 0
081100100119     c                   move      tbeDTR        dataiso
081200100119     c                   move      dataiso       dataeur
081300100317     c                   moveL     dataeur       s1rcv
081400100119     c                   end
081500100119     c                   eval      s1dati = tbeUNI
081600940127     C*
081700100323     c                   eval      h1ANNulla = *off
081800100121     c                   if        tbeATB <> *blank
081900100323     c                   move(p)   'ANNULLATO'   s1des
082000100323     c                   eval      h1ANNulla = *on
082100100121     c                   seton                                        42
082200100121     c                   end
082300100121     C*
082400940127     C                   ADD       1             S1NRR
082500940127     C                   ADD       1             Y
082600940607     C                   WRITE     S1
082700100121     c                   setoff                                       42
082800100119     c                   end
082900940131     C*
083000100119    >C                   EXSR      READ_tbe
0831009401271-   C                   ENDDO
083200940127     C*
083300940223     C                   Z-ADD     S1NRR         WMAX                 30
083400940127     C*
083500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
083600090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
083700940127     C                   MVR                     RESTO             3 0
083800090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0839000301141    C     RESTO         IFGT      0
084000030114     C                   ADD       1             C1RCD
0841000301141E   C                   ELSE
084200090225     C                   SUB       SFLPAG1       C1RCD
084300030114     C                   ADD       1             C1RCD
0844000301141-   C                   ENDIF
084500940128     C*
084600940127     C                   ENDSR
084700940224     C************************************************************
084800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
084900940224     C************************************************************
085000090225     C     CLCPAG0       BEGSR
085100940224     C* Input :
085200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
085300940224     C*          cursore
085400940224     C* - SFLPAG = numero rcd per pagina sfl
085500940224     C* Output :
085600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
085700940224     C*
085800090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
085900940224     C                   MVR                     RESTO             3 0
086000940224     C     RESTO         IFGT      0
086100940224     C                   ADD       1             PAGINE
086200940224     C                   ENDIF
086300090226     c                   if        pagine > 1
086400090225     C     PAGINE        MULT      SFLPAG0       WPAG
086500090226     C                   END
086600940224     C*
086700940224     C                   ENDSR
086800090225     C************************************************************
086900090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
087000090225     C************************************************************
087100090225     C     CLCPAG1       BEGSR
087200090225     C* Input :
087300090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
087400090225     C*          cursore
087500090225     C* - SFLPAG = numero rcd per pagina sfl
087600090225     C* Output :
087700090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
087800090225     C*
087900090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
088000090225     C                   MVR                     RESTO             3 0
088100090225     C     RESTO         IFGT      0
088200090225     C                   ADD       1             PAGINE
088300090225     C                   ENDIF
088400090226     c                   if        pagine > 1
088500090225     C     PAGINE        MULT      SFLPAG1       WPAG
088600090226     C                   ENDIF
088700090225     C*
088800090225     C                   ENDSR
088900940309     C************************************************************
089000940309     C* GESTIONE F03 VIDEO S1
089100940309     C************************************************************
089200940309     C     F03S1         BEGSR
089300940309     C*
089400940309     C                   MOVE      *ON           $FINE
089500940325     C* fine programma
089600940309     C                   ENDSR
089700940309     C/EJECT
089800940309     C************************************************************
089900940309     C* GESTIONE F10 VIDEO S1
090000940314     c* AGGIUNTA RECORD
090100940309     C************************************************************
090200940309     C     F10S1         BEGSR
090300940309     C*
090400030113     C                   RESET                   tabds
090500030113     C                   MOVEL     '01'          xtaopz
090600030113     C                   MOVE      *ZERO         xtaret
090700030113     C                   MOVE      *ZERO         xtaopr
090800100409     C                   MOVEl     *blank        xtakey1
090900100409     C                   MOVEl     *blank        xtakey2
091000100409     C                   MOVEl     'AUT'         xtacod
091100030113     C                   MOVE      *BLANKS       KPJBU
091200030113     C                   MOVEL     tabds         KPJBU
091300100119$004 C                   CALL      'TNTBAUTR2'
091400030113     C                   PARM                    KPJBA
091500090225      *
091600030114     C                   MOVEL     KPJBU         tabds
091700030113      *
091800940309     C* ritorno da PGM gestione
091900940309     C                   EXSR      GESRET
092000090226      *
092100940309     C     WINZS1        IFEQ      *ON
092200940309     C                   MOVE      *ON           $INZS1
092300090226     C* carico sempre la 1a pagina
092400090226     C                   Z-ADD     1             WPAG
092500940309     C                   ENDIF
092600940309     C*
092700940309     C                   ENDSR
092800940128     C************************************************************
092900100322     C* Ha richiamato il programma per scegliere particolari profili
093000940128     C************************************************************
093100100322     C     F17S1         BEGSR
093200100322     C*
093300100322     C* reinizializza il SFL poichè lo deve ricaricare
093400100322     C                   MOVE      *ON           $INZS1
093500940128     C*
093600100322      *  I parametri di selezione sono nella Local Data Area condivisa dai 2 pgm
093700100322     c                   call      'TNTBAUTR1'
093800100322     c                   parm                    kpjba
093900100322      *
094000100322     C*
094100100322     C                   ENDSR
094200100322     C************************************************************
094300100322     C* CONTROLLO TESTATA LISTA
094400100322     C************************************************************
094500100322     C     CTRC1         BEGSR
094600100322     C*
094700940201     C                   MOVE      *OFF          *IN99
094800940131     C*
094900940202     C                   ENDSR
095000940131     C************************************************************
095100940131     C* CONTROLLO OPZIONI LISTA
095200940131     C************************************************************
095300940131     C     CTRS1         BEGSR
095400940131     C*
095500940202     C                   MOVEL     *OFF          $ESCI
095600940201     C                   SETOFF                                       99
095700940131     C                   Z-ADD     0             S1OPZ
095800040930     c                   clear                   x1cod
095900940131     C*
096000940127     C* Leggo il sfl solo se ci sono rcd
0961009401311    C     WMAX          IFGT      0
096200940607     C                   READC     S1                                     21
096300940127     C*
096400940131     C* esce se fine sfl o errore che richiede l'uscita
0965009401312    C     *IN21         DOWEQ     *OFF
096600940131     C     $ESCI         ANDEQ     *OFF
096700940201     C                   Z-ADD     S1NRR         C1RCD
096800940131     C* ctrl su riga
096900940131     C                   EXSR      RECS1
097000940131     C* gestione opzioni
0971009401313    C     S1OPZ         IFNE      0
097200940201     C     *IN99         ANDEQ     *OFF
097300940131     C                   EXSR      OPZS1
0974009401273-   C                   ENDIF
097500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0976009402013    C     *IN99         IFEQ      *ON
097700940201     C     $ESCI         OREQ      *ON
097800940131     C                   MOVE      *ON           *IN32
097900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
098000940204     C* la ripristinerò a conclusione del ciclo di READC
098100940223     C                   MOVE      *OFF          $INZS1
0982009402233-   C                   ENDIF
098300940223     C*
098400100323     C  n99              Z-ADD     0             S1OPZ
098500100121      ****
098600100323     c     h1annulla     comp      *on                                    42
098700940223     C*
098800940607     C                   UPDATE    S1
098900940223     C*
099000940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0991009401313    C     $ESCI         IFEQ      *OFF
099200940607     C                   READC     S1                                     21
099300940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0994009402014    C     *IN21         IFEQ      *ON
099500940201     C                   MOVE      WINZS1        $INZS1
099600940204     C* calcolo pagina a cui deve posizionarsi
099700090225     C                   EXSR      CLCPAG1
0998009402014-   C                   ENDIF
0999009402013-   C                   ENDIF
100000940131     C*
1001009401272-   C                   ENDDO
100200940127     C*
1003009401311-   C                   ENDIF
100400940131     C*
100500940127     C                   ENDSR
100600940127     C/EJECT
100700940127     C************************************************************
100800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
100900940127     C************************************************************
101000940131     C     RECS1         BEGSR
101100940131     C*
101200940201     C* reset indicatori DSPATR
101300940201     C                   MOVE      *ALL'0'       IN4049           10
101400940201     C                   MOVEA     IN4049        *IN(40)
101500100323     C*
101600100323     C*  se si è selezionato di ripristinare un record NON annullato
1017001003233    C     S1OPZ         IFeq      8
1018001003233    C     H1ANNulla     andeq     *off
101900100323     c                   seton                                        41  99
102000100323     C                   END
102100100323     C*
102200100323     C*  se invece il record era annullato si può solo vedere o ripristinare
1023001003233    C     H1ANNulla     ifeq      *on
1024001003233    C     S1OPZ         andne     8
1025001003233    C     S1OPZ         andne     5
102600100323     c                   seton                                        41  99
102700100323     C                   END
102800100323     C*
102900100323     C*  se richiamato x selezionare un codice, --> 11 = *on
103000100323     C*   non deve utilizzare le opzioni di modifica e viceversa
103100040930     c     *in11         ifeq      *on
1032000409303    C     S1OPZ         IFeq      2
1033000409303    C     S1OPZ         OReq      3
1034000409303    C     S1OPZ         OReq      4
1035001003233    C     S1OPZ         OReq      8
103600040930     c                   seton                                        41  99
103700040930     C                   END
103800100323     C*
103900040930      * selezionato un codice da restituire al chiamante
1040000409303    C     S1OPZ         IFeq      1
104100100324     c                   move      h1cod         x1cod
104200040930     c                   clear                   kpjbu
104300040930     c                   movel     x1cod         kpjbu
104400040930     C                   MOVEL     *ON           $ESCI
104500040930     C                   MOVEL     *ON           $fine
104600040930     C                   END
104700040930     C                   END
104800040930     C*
104900040930     c     *in11         ifeq      *off
1050000409303    C     S1OPZ         ANDeq     1
105100040930     c                   seton                                        41  99
105200040930     C                   END
105300040930     C*
105400940131     C                   ENDSR
105500940131     C************************************************************
105600940131     C* GESTIONE OPZIONI LISTA
105700940131     C************************************************************
105800940131     C     OPZS1         BEGSR
105900940201     C*
1060000409302    C     *IN11         IFEQ      *Off
1061000409302    C     *IN11         oreq      *On
1062000409302    C     s1opz         andeq     05
106300040930     C*
106400030113     C                   RESET                   tabds
106500100323     c                   move      *zero         xtaopz
106600100323     C                   MOVE      S1OPZ         xtaopz
106700030113     C                   MOVE      *ZERO         xtaret
106800030113     C                   MOVE      *ZERO         xtaopr
106900100409     C                   MOVEl(p)  h1COD         xtakey1
107000100409     C                   MOVEl(p)  s1COD2        xtakey2
107100100409     C                   MOVEl     'AUT'         xtacod
107200940715     C                   MOVE      *BLANKS       KPJBU
107300030113     C                   MOVEL     tabds         KPJBU
107400100119$004 C                   CALL      'TNTBAUTR2'
107500940607     C                   PARM                    KPJBA
107600090225      *
107700030114     C                   MOVEL     KPJBU         tabds
107800940201     C*
107900940223     C* ritorno da PGM gestione
108000940223     C                   EXSR      GESRET
108100040930     C*
108200940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1083009402252    C     *IN99         IFEQ      *ON
108400940223     C                   SETON                                        40
1085009402252-   C                   ENDIF
108600940225     C*
1087000409302-   C                   ENDIF
108800040930     C*
108900940131     C                   ENDSR
109000940223     C/EJECT
109100940223     C************************************************************
109200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
109300940223     C************************************************************
109400940223     C     GESRET        BEGSR
109500940223     C*
109600940223     C* modo di ritorno
109700940223     C*
1098009402231    C                   SELECT
109900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
110000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
110100940314    >C*    proposte, normalmente sempre valide, ma modificabili
110200940314    >C*    per situazioni particolari >>
110300940223     C* 1 = F3
110400030113    >C     xtaret        WHENEQ    '1'
110500940224     C                   MOVE      *ON           $ESCI
110600940223     C                   MOVE      *ON           $FINE
110700940223     C* 2 = F12
110800030113    >C     xtaret        WHENEQ    '2'
110900940223     C                   MOVE      *ON           $ESCI
111000940223     C*
1111009402231-   C                   ENDSL
111200940223     C*
111300940223     C* operazione eseguite dal pgm chiamato
111400940223     C*
1115009402231    C                   SELECT
111600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
111700030113    >C     xtaopr        WHENEQ    '1'
111800940223     C                   MOVE      *ON           WINZS1
111900940223     C*
1120009402231-   C                   ENDSL
112100940223     C*
112200940223     C* funzione non eseguibile per errore :
112300940223     C*
1124009402231    C                   SELECT
112500940223     C* 1 = funzione richiamata chiusa in errore
112600940316    >C*  eventualmente gestire altri codici di errore
112700030113    >C     xtaerr        WHENEQ    '1'
112800940223     C                   MOVE      *ON           $ESCI
112900940223     C                   SETON                                        5299
113000940223     C*
1131009402231-   C                   ENDSL
113200940223     C*
113300940223     C                   ENDSR
113400940223     C/EJECT
113500940131     C************************************************************
113600940131     C* OPERAZIONI INIZIALI
113700940131     C************************************************************
113800940131     C     *INZSR        BEGSR
113900030113     C*
114000030113     C* Reperimento parametri
114100030113     C     *ENTRY        PLIST
114200030113     C                   PARM                    KPJBA
114300100407     C                   movel     kpjbu         TNTBautDS
114400040930     C*
114500100326     c     tbe_AUT_Fil   Klist
114600100326     c                   kfld                    tbe_AUT
114700100326     c                   kfld                    tbe_FIL          15
114800100326     C*
114900100322     C* definisce la Local Data Area per selezionare i profili
115000100322     C     *DTAARA       DEFINE    *LDA          LDA_Selez
115100100322     C*
115200040930     C* Se chiamato x selezionare un codice
115300040930     c                   clear                   x1cod
115400100407     c                   if        autSELEZ = '?'
115500040930     c                   seton                                        11
115600040930     c                   end
115700030113     C*
115800030113     C* Variabili per gestione videate
115900030113     C                   MOVE      *BLANK        $GEST             2
116000030113     C                   MOVE      *BLANK        $FINE             1
116100100319     C                   MOVE      *BLANK        $INZS2            1
116200030113     C                   MOVE      *BLANK        $INZS1            1
116300090225     C                   MOVE      *BLANK        $INZS0            1
116400030113     C                   MOVE      *BLANK        $EFILE            1
116500030113     C                   MOVE      *BLANK        $ESCI             1
116600030113     C                   MOVE      *BLANK        $RCDOK            1
116700030113     C* Indici
116800030113     C                   Z-ADD     0             X                 3 0
116900030113     C                   Z-ADD     0             Y                 3 0
117000940506     C*
117100940506     C* Reperimento tasti di funzione
117200100326     C*
117300100326     C                   clear                   c0pos
117400100326     C                   clear                   SAVc0pos
117500940127     C*
117600940117     C                   ENDSR
117700100319     C************************************************************
