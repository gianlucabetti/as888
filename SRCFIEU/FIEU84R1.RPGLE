000100141127     H*
000200141127     H*
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400030729      *
000500940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000600940620      *  03           PROTECT TUTTI I CAMPI
000700030123      *  09           PROTECT dei campi voce C/E
000800940223      *  21           GENERICO OPERAZIONI I/O
000900940223      *  22           GENERICO ERRORE OPERAZIONI I/O
001000940223      *  30           SFLDSP
001100940223      * N31           SFLCLR
001200940128      *  31           SFLDSPCTL
001300940128      *  32           SFLNXTCHG
001400940128      *  33           SFLEND
001500940128      *  39           OF PRTF
001600940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940315      *  50 <---> 98  ERRORI SU VIDEO
001900940317    > *  Specificare l'uso dei singoli indicatori
002000940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002300940128      *  99           INDIC. GENERALE DI ERRORE
002400940128     F*----------------------------------------------------*
002500040826$001 FFIEU84D1  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700040826     FFIEU84F   UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900030113      *-------------
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200030113      *-------------
003300030113     D* Parametri in ricezione
003400030113     D  TABDS          DS
003500030729     D  XTAOPZ                 1      1
003600030729     D  XTARET                 2      2
003700030729     D  XTAOPR                 3      3
003800030729     D  XTAERR                 4      4
003900040826     D  XTACOD                 5      7
004000940201     D*-------------
004100030204     D  S_TASTO        S                   like($Tasto)
004200940201     D DSFMT           DS
004300940506     D  $TASTO               369    369
004400940201     D  NRG                  370    370
004500940201     D  NCL                  371    371
004600940201     D*-------------
004700940201     D* posizione cursore
004800940201     D CURSOR          DS
004900940223     D  RIRI                   1      2B 0 INZ
005000940201     D  R$$                    2      2
005100940223     D  COCO                   3      4B 0 INZ
005200940201     D  C$$                    4      4
005300030728      *
005400030728     Ddataiso          s               d   datfmt(*iso)
005500030728     Ddataeur          s               d   datfmt(*eur)
005600940207     D*-------------
005700940207     D* Reperimento nome PGM
005800940207     D STATUS         SDS           333
005900940207     D  DSPGM            *PROC
006000940225     D*-------------
006100940225     D* COSTANTI
006200940225     D*-------------
006300940506     D* Tasti di funzione
006400940506     D F01             C                   CONST(X'31')
006500940506     D F02             C                   CONST(X'32')
006600940506     D F03             C                   CONST(X'33')
006700940506     D F04             C                   CONST(X'34')
006800940506     D F05             C                   CONST(X'35')
006900940506     D F06             C                   CONST(X'36')
007000940506     D F07             C                   CONST(X'37')
007100940506     D F08             C                   CONST(X'38')
007200940506     D F09             C                   CONST(X'39')
007300940506     D F10             C                   CONST(X'3A')
007400940506     D F11             C                   CONST(X'3B')
007500940506     D F12             C                   CONST(X'3C')
007600940506     D F13             C                   CONST(X'B1')
007700940506     D F14             C                   CONST(X'B2')
007800940506     D F15             C                   CONST(X'B3')
007900940506     D F16             C                   CONST(X'B4')
008000940506     D F17             C                   CONST(X'B5')
008100940506     D F18             C                   CONST(X'B6')
008200940506     D F19             C                   CONST(X'B7')
008300940506     D F20             C                   CONST(X'B8')
008400940506     D F21             C                   CONST(X'B9')
008500940506     D F22             C                   CONST(X'BA')
008600940506     D F23             C                   CONST(X'BB')
008700940506     D F24             C                   CONST(X'BC')
008800940506     D ENTER           C                   CONST(X'F1')
008900940506     D ROLDWN          C                   CONST(X'F4')
009000940506     D ROLLUP          C                   CONST(X'F5')
009100940506     D*-------------
009200941108     D DATA            C                   CONST('0001-01-01')
009300030114     D Annull          C                   CONST('  ANNULLATO  ')
009400030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009500030114     D OPz_Var         C                   CONST('VARIAZIONE')
009600030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009700030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009800940127     C*----------------------------------------------------*
009900940127     C*                MAIN LINE PROGRAM
010000940127     C*----------------------------------------------------*
010100940223     C*
010200940223     C     $FINE         DOWEQ     *OFF
010300940202     C     $GEST         CASEQ     'D1'          GESD1
010400940117     C                   END
010500940117     C                   END
010600940325     C* fine programma
010700030113     c                   movel     tabds         kpjbu
010800030113     C                   Seton                                        LR
010900940131     C************************************************************
011000940510     C* GESTIONE VIDEO RECORD D1
011100940131     C************************************************************
011200940127     C     GESD1         BEGSR
011300030113      *
011400940225     C* inizializzazione videata
011500940223     C     $INZD1        IFEQ      *ON
011600940127     C                   EXSR      INZD1
011700940223     C                   MOVE      *OFF          $INZD1
011800940117     C                   END
011900030113      *
012000940225     C* emissione piede videata se proveniente da altra
012100940225     C* salvataggio valore formato di provenienza
012200940207     C     $LASTV        IFNE      'D1'
012300940614     C                   WRITE     T1
012400940202     C                   MOVE      $LASTV        $LASTG
012500940202     C                   MOVE      'D1'          $LASTV
012600940202     C                   END
012700940131     C*
012800940203     C                   SELECT
012900940203     C* Annullamento
013000030114     C* Visualizzazione
013100030729    >C     xtaopz        WHENEQ    '1'
013200030114     c                   movel     Opz_Imm       D1Fun
013300030729    >C     xtaopz        WHENEQ    '5'
013400030114     c                   movel     Opz_Vis       D1Fun
013500030114     C                   SETON                                        020306
013600030729    >C     xtaopz        WHENEQ    '4'
013700030114     c                   movel     Opz_Del       D1Fun
013800030114     C                   SETON                                        0203
013900030114     C                   SETOFF                                         06
014000940620     C* Manutenzione
014100030729    >C     xtaopz        WHENEQ    '2'
014200030114     c                   movel     Opz_Var       D1Fun
014300030114     C                   SETON                                        02
014400030114     C                   SETOFF                                       0306
014500940620     C*
014600940620     C                   OTHER
014700940620     C*
014800030114     C                   SETOFF                                       020306
014900940203     C                   ENDSL
015000030113     C*
015100030728     c     video1        tag
015200030114     c                   SETOFF                                         99
015300940620     C* Immissione/Modifica/Copia
015400030113     C*               *----------------*
015500940620     C                   EXFMT     D1
015600030113     C*               *----------------*
015700030114     C*
015800030114     C     *IN99         IFEQ      *OFF
015900030114     C*
0160000301141    C                   SELECT
016100030114     C* F3=Fine
016200030114     C     $TASTO        WHENEQ    F03
016300030114     C                   EXSR      F03D1
016400030114     C* F12=Ritorno
016500030114     C     $TASTO        WHENEQ    F12
016600030114     C                   EXSR      F12D1
016700030114     C*
0168000301141O   C                   OTHER
016900030114     C*
017000030114     C                   EXSR      CTRD1
017100030728     c   99              goto      video1
017200030113     C*
017300940616     C     *IN99         IFEQ      *OFF
017400940616     C* F6
0175000302052    C     $TASTO        ifeq      F06
0176000301152    C     $TASTO        oreq      F23
017700940224     C* eseguo aggiornamento
017800940315    >C                   EXSR      AGGANA
017900940224     C* eseguo operazioni del dopo-aggiornamento
018000030114     C  N99              EXSR      GESAGG
018100030205     c                   end
018200030114     C*
018300030114     C                   ENDIF
018400030114     C*
0185000301142-   C                   ENDSL
018600030114     C*
0187000301142-   C                   ENDIF
018800940224     C*
018900940117     C                   ENDSR
019000940117      ************************************************************
019100940207      * INIZIALIZZAZIONE VIDEATA DATI
019200940117      ************************************************************
019300940127     C     INZD1         BEGSR
019400940131     C*
019500940615     C                   CLEAR                   D1
019600030729     C                   MOVEl     xtaCOD        D1COD
019700940207     C*
019800940224     C* valorizzo la chiave univoca per reperire i dati
019900940224     C* se non immissione
0200000307291   >C     xtaopz        IFNE      '1'
020100030204      *
020200940315    >C*-------------------------------------------------------------
020300940315    >C* impostare i campi chiave ricevuti
020400940315    >C*-------------------------------------------------------------
020500040826     C     Keva          chain(N)  fieu84f
020600940614     C*
020700040826     C                   IF        %Found(fieu84f)
020800040826     c                   eval        d1cod =  t84ntw
020900040826     c                   eval        d1des =  t84des
021000040826     c                   eval        d1ifc =  t84ifc
021100040826     c                   eval        d1ifr =  t84ifr
021200040826     c                   eval        d1iac =  t84iac
021300040826     c                   eval        d1iar =  t84iar
021400040826     c                   eval        d1efc =  t84efc
021500040826     c                   eval        d1efr =  t84efr
021600040826     c                   eval        d1eac =  t84eac
021700040826     c                   eval        d1ear =  t84ear
021800040826      **
021900040826     c**                 if        t84ANN = 'A'  and xtaOPZ <>'3'
022000040826     c**                 seton                                        08
022100040826     C**                 MOVEl     Annull        D1Ann
022200040826     C**                 end
022300030113      *
022400030729     C* richiamo routine dei ctrl per decodificarla
022500940614    >C                   EXSR      CTRD1
022600030113      *
022700960305    >C     *IN99         IFEQ      *ON
022800960305    >C                   MOVEL     *IN99         SAV99             1
022900960305    >C                   ENDIF
023000030113      *
023100940224     C                   MOVE      *ALL'0'       IN5098
023200940224     C                   MOVEA     IN5098        *IN(50)
023300030113      *
023400940224     C* definizione indicatori su video
023500940224     C                   SETOFF                                       02
0236009402243    C                   SELECT
023700030729    >C     xtaopz        WHENEQ    '2'
023800940224     C                   SETON                                        02
023900030729    >C     xtaopz        WHENEQ    '5'
0240009402243-   C                   ENDSL
024100940224     C*
0242009402242-   C                   ENDIF
024300030114     C*
0244000301141   >C                   Else
024500030114     C* Se in Immissione nuovo record
024600030728     c                   clear                   d1
024700940223     C*
0248009402241-   C                   ENDIF
024900940224     C*
025000940117     C                   ENDSR
025100940309     C************************************************************
025200940309     C* GESTIONE F03 VIDEO D1
025300940309     C************************************************************
025400940309     C     F03D1         BEGSR
025500940309     C*
025600940309     C                   MOVE      *ON           $FINE
025700030113    >C                   MOVE      '1'           xtaret
025800940325     C*
025900940325     C                   ENDSR
026000940309     C************************************************************
026100940309     C* GESTIONE F12 VIDEO D1
026200940309     C************************************************************
026300940309     C     F12D1         BEGSR
026400940309     C*
026500940309     C                   MOVE      *ON           $FINE
026600940309     C*
026700940309     C                   ENDSR
026800940131     C************************************************************
026900940207     C* CONTROLLO VIDEATA
027000940131     C************************************************************
027100940127     C     CTRD1         BEGSR
027200940131     C*
027300940127     C                   SETOFF                                       99
027400030123      *
027500940127     C* non imposto pos. del cursore
027600940127     C                   Z-ADD     0             H1RIGA
027700940127     C                   Z-ADD     0             H1COLO
027800940621     C*
027900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0280000307291   >C     xtaopz        IFNE      '4'
028100040826     c* selezioni
028200040826     c                   if        d1ifc = 'X' and d1ifr = 'X' or
028300040826     c                             d1ifc = ' ' and d1ifr = ' '
028400040826     c                   seton                                        5199
028500030728     c                   end
028600030728     c*
028700040826     c                   if        d1iac = 'X' and d1iar = 'X' or
028800040826     c                             d1iac = ' ' and d1iar = ' '
028900040826     c                   seton                                        5299
029000040826     c                   end
029100040826     c*
029200040826     c                   if        d1efc = 'X' and d1efr = 'X' or
029300040826     c                             d1efc = ' ' and d1efr = ' '
029400040826     c                   seton                                        5399
029500040826     c                   end
029600040826     c*
029700040826     c                   if        d1eac = 'X' and d1ear = 'X' or
029800040826     c                             d1eac = ' ' and d1ear = ' '
029900040826     c                   seton                                        5499
030000040826     c                   end
030100040826     c*
030200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
030300030729    >C     xtaopz        IFEQ      '1'
030400030729    >C     xtaopz        oreq      '3'
030500030728     C*
030600040826     c* controllo di non sovrapposizione
030700040826     C     Kev           chain     fieu84f
030800040826     c                   if        %found(fieu84f)
030900030728     c                   seton                                        5599
031000030728     c                   end
031100030114     C                   ENDIF
031200030114     C*
031300030114     C* controllo codice vuoto
031400030729$017 C     D1cod         IFEQ      *blank
031500040826     C                   SETON                                        5699
031600030114     C                   End
031700030114      *
0318009402241-   C                   END
031900940131     C*
032000940302     C     *IN99         IFEQ      *ON
032100940510     C     $GEST         ANDEQ     'D1'
032200940117     C                   MOVEA     *IN           WIN
032300940128     C                   MOVE      *ALL'0'       IN5098           49
032400940128     C                   MOVEA     IN5098        *IN(50)
032500940614     C                   WRITE     D1
032600940117     C                   MOVEA     WIN           *IN
032700940302     C                   ENDIF
032800940127     C*
032900940117     C                   ENDSR
033000940207     C/EJECT
033100940224     C************************************************************
033200940315    >C* AGGIORNAMENTO ANAGRAFICA
033300940224     C************************************************************
033400940315    >C     AGGANA        BEGSR
033500940207     C*
033600940207     C                   SELECT
033700030204     C*
033800030729    >C     xtaopz        WHENEQ    '1'
033900030729    >C     xtaopz        OREQ      '3'
034000030204     C*
034100030728    >C                   EXSR      RIEANA
034200040826     c                   write     fieu084                              59
034300940318     C* se rcd non scrivibile attivo errore
034400030114    >C   59              SETON                                          99
034500030204      *
034600030729    >C     xtaopz        WHENEQ    '2'
034700030204      *
034800040826     C     Keva          chain     fieu84f
034900030728    >C                   EXSR      RIEANA
035000040826     c                   update    fieu084                              59
035100030114    >C   59              SETON                                          99
035200030204      *
035300030729    >C     xtaopz        WHENEQ    '4'
035400040826     C     Keva          chain     fieu84f
035500030115     c                   if        $tasto = F23
035600040826     c                   delete    fieu084                              59
035700030115     c                   else
035800040826     c                   delete    fieu084                              59
035900030115     c                   end
036000030114    >C   59              SETON                                          99
036100030204      *
036200940207     C                   ENDSL
036300940203     C*
036400940203     C                   ENDSR
036500940224     C************************************************************
036600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
036700940224     C************************************************************
036800030728     C     RIEANA        BEGSR
036900030114     C*
037000040826     c                   move      d1cod         t84ntw
037100040826     c                   movel     d1des         t84des
037200040826     c                   movel     d1ifc         t84ifc
037300040826     c                   movel     d1ifr         t84ifr
037400040826     c                   movel     d1iac         t84iac
037500040826     c                   movel     d1iar         t84iar
037600040826     c                   movel     d1efc         t84efc
037700040826     c                   movel     d1efr         t84efr
037800040826     c                   movel     d1eac         t84eac
037900040826     c                   movel     d1ear         t84ear
038000030114     C*
038100940207     C                   ENDSR
038200940224     C************************************************************
038300940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
038400940224     C************************************************************
038500940224     C     GESAGG        BEGSR
038600940224     C*
038700940224     C* segnalo al pgm chiamante l'avvenuta conferma
038800030114    >C                   MOVE      *ON           xtaopr
038900940224     C* segnalo al pgm chiamante l'aver premuto l'F6
039000030114    >C                   MOVE      '0'           xtaret
039100940224     C*
0392009402241    C                   SELECT
039300940224     C* nel caso di immissione, ripristino la videata iniziale
039400030729    >C     xtaopz        WHENEQ    '1'
039500940224     C                   MOVE      *ON           $INZD1
039600030114     C                   MOVE      *ON           $FINE
039700940224     C* altrimenti ritorno al pgm chiamante
039800940224     C                   OTHER
039900940224     C                   MOVE      *ON           $FINE
0400009402241-   C                   ENDSL
040100940224     C*
040200940224     C                   ENDSR
040300940131     C************************************************************
040400940131     C* OPERAZIONI INIZIALI
040500940131     C************************************************************
040600940131     C     *INZSR        BEGSR
040700940131     C*
040800940127     C* Reperimento parametri
040900940127     C*
041000940117     C     *ENTRY        PLIST
041100940117     C                   PARM                    KPJBA
041200030113     C                   movel     kpjbu         tabds
041300030115     c                   setoff                                       08
041400030115     C*
041500940223     C* Variabili per gestione videate
041600940223     C                   MOVE      'D1'          $GEST
041700940223     C                   MOVE      *OFF          $FINE
041800940223     C                   MOVE      *ON           $INZD1
041900940224     C                   MOVE      *BLANK        $LASTG
042000940224     C                   MOVE      *BLANK        $LASTV
042100940506     C                   Z-ADD     0             $ULKD1            3 0
042200940127     C*
042300940223     C* Variabili appoggio
042400940224     C                   Z-ADD     0             CURR
042500940224     C                   Z-ADD     0             CURC
042600940207     C*
042700940207     C* Valorizzazione campi univoci testate
042800940614     C                   CLEAR                   T1
042900940614     C                   MOVEL     KNSIF         NOMSIF
043000941027     C                   MOVEL     DSPGM         NOMPGM
043100030113     C*
043200040826     C                   movel     XTACOD        t84ntw
043300940224     C*
043400940224     C* Inizializzazione/Decodifica prima videata
043500940224     C                   EXSR      INZD1
043600940224     C                   MOVE      *OFF          $INZD1
043700940127     C*
043800940117     C                   ENDSR
043900940128     C************************************************************
044000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
044100030729     c************************************************************
044200940223     C     DEFCAM        BEGSR
044300940223     C*
044400940223     C* klist
044500030729     C     Keva          KLIST
044600030728     C                   KFLD                    XTACOD
044700030728      *
044800030728     C     Kev           KLIST
044900030729     C                   KFLD                    D1COD
045000940207     C*
045100940223     C* Variabili per gestione videate
045200940223     C*
045300940223     C                   MOVE      *BLANK        $GEST             2
045400940223     C                   MOVE      *BLANK        $FINE             1
045500940223     C                   MOVE      *BLANK        $INZD1            1
045600940223     C                   MOVE      *BLANK        $LASTG            2
045700940223     C                   MOVE      *BLANK        $LASTV            2
045800940223     C*
045900940223     C* Variabili appoggio
046000940223     C                   Z-ADD     0             CURR              2 0
046100940223     C                   Z-ADD     0             CURC              2 0
046200940223     C                   MOVE      *ZEROS        WIN              99
046300940224     C                   MOVE      *BLANK        WTBERR            1
046400940223     C*
046500940223     C* Indici
046600940223     C                   Z-ADD     0             X                 3 0
046700940223     C                   Z-ADD     0             Y                 3 0
046800940127     C*
046900940117     C                   ENDSR
047000940128     C************************************************************
