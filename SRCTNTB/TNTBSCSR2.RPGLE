000100141127     H*
000200141127     H*
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
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
002500160412$001 FTNTBSCSD2 CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700110927     Ftntbe01l  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900160412$003 D EXTDS         E DS                  EXTNAME(tntbSCSds)
003000030113      *-------------
003100940127     D* Passaggio Parametri
003200940127     D KPJBA         E DS
003300030113      *-------------
003400030113     D* Parametri in ricezione
003500030113     D  TABDS          DS
003600030113     D  XTAOPZ                 1      2
003700030113     D  XTARET                 3      3
003800030113     D  XTAOPR                 4      4
003900030113     D  XTAERR                 5      5
004000160412     D  XTAKEY                 6      7
004100160412     D  XTAKE2                 8     22
004200940201     D*-------------
004300030204     D  S_TASTO        S                   like($Tasto)
004400940201     D DSFMT           DS
004500940506     D  $TASTO               369    369
004600940201     D  NRG                  370    370
004700940201     D  NCL                  371    371
004800940201     D*-------------
004900940201     D* posizione cursore
005000940201     D CURSOR          DS
005100940223     D  RIRI                   1      2B 0 INZ
005200940201     D  R$$                    2      2
005300940223     D  COCO                   3      4B 0 INZ
005400940201     D  C$$                    4      4
005500940207     D*-------------
005600940207     D* Reperimento nome PGM
005700940207     D STATUS         SDS           333
005800940207     D  DSPGM            *PROC
005900940225     D*-------------
006000940225     D* COSTANTI
006100940225     D*-------------
006200940506     D* Tasti di funzione
006300940506     D F01             C                   CONST(X'31')
006400940506     D F02             C                   CONST(X'32')
006500940506     D F03             C                   CONST(X'33')
006600940506     D F04             C                   CONST(X'34')
006700940506     D F05             C                   CONST(X'35')
006800940506     D F06             C                   CONST(X'36')
006900940506     D F07             C                   CONST(X'37')
007000940506     D F08             C                   CONST(X'38')
007100940506     D F09             C                   CONST(X'39')
007200940506     D F10             C                   CONST(X'3A')
007300940506     D F11             C                   CONST(X'3B')
007400940506     D F12             C                   CONST(X'3C')
007500940506     D F13             C                   CONST(X'B1')
007600940506     D F14             C                   CONST(X'B2')
007700940506     D F15             C                   CONST(X'B3')
007800940506     D F16             C                   CONST(X'B4')
007900940506     D F17             C                   CONST(X'B5')
008000940506     D F18             C                   CONST(X'B6')
008100940506     D F19             C                   CONST(X'B7')
008200940506     D F20             C                   CONST(X'B8')
008300940506     D F21             C                   CONST(X'B9')
008400940506     D F22             C                   CONST(X'BA')
008500940506     D F23             C                   CONST(X'BB')
008600940506     D F24             C                   CONST(X'BC')
008700940506     D ENTER           C                   CONST(X'F1')
008800940506     D ROLDWN          C                   CONST(X'F4')
008900940506     D ROLLUP          C                   CONST(X'F5')
009000160421     d digits          c                   '0123456789'
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
013100030114    >C     xtaopz        WHENEQ    '01'
013200030114     c                   movel     Opz_Imm       D1Fun
013300030114    >C     xtaopz        WHENEQ    '05'
013400030114     c                   movel     Opz_Vis       D1Fun
013500030114     C                   SETON                                        020306
013600030114    >C     xtaopz        WHENEQ    '04'
013700030114     c                   movel     Opz_Del       D1Fun
013800110902     C                   SETON                                        02
013900030114     C                   SETOFF                                         06
014000940620     C* Manutenzione
014100030113    >C     xtaopz        WHENEQ    '02'
014200030114     c                   movel     Opz_Var       D1Fun
014300030114     C                   SETON                                        02
014400030114     C                   SETOFF                                       0306
014500940620     C*
014600940620     C                   OTHER
014700940620     C*
014800030114     C                   SETOFF                                       020306
014900940203     C                   ENDSL
015000030114     C*
015100030114     c                   SETOFF                                         99
015200940620     C* Immissione/Modifica/Copia
015300030113     C*               *----------------*
015400940620     C                   EXFMT     D1
015500030113     C*               *----------------*
015600030114     C*
015700030114     C     *IN99         IFEQ      *OFF
015800030114     C*
0159000301141    C                   SELECT
016000030114     C* F3=Fine
016100030114     C     $TASTO        WHENEQ    F03
016200030114     C                   EXSR      F03D1
016300030114     C* F12=Ritorno
016400030114     C     $TASTO        WHENEQ    F12
016500030114     C                   EXSR      F12D1
016600030114     C*
0167000301141O   C                   OTHER
016800030114     C*
016900030114     C                   EXSR      CTRD1
017000030113     C*
017100940616     C     *IN99         IFEQ      *OFF
017200940616     C* F6
0173000302052    C     $TASTO        ifeq      F06
0174000301152    C     $TASTO        oreq      F23
017500030204     C* A chi Trasmette la Tabella ?
017600130301     c                   Exsr      ACHI
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
018800030114     C*
018900940117     C                   ENDSR
019000940117      ************************************************************
019100940207      * INIZIALIZZAZIONE VIDEATA DATI
019200940117      ************************************************************
019300940127     C     INZD1         BEGSR
019400940131     C*
019500940615     C                   CLEAR                   D1
019600160412     C                   move      'SCS'         tbeCOD
019700030113     C                   MOVEl     xtakey        D1COD
019800160412     C                   MOVEl     xtake2        D1SUBSC
019900110927     C                   clear                   tbeKE1
020000110927     C                   MOVEl     xtakey        tbeKE1
020100160412     C                   clear                   tbeKE2
020200160412     C                   MOVEl     xtake2        tbeKE2
020300940207     C*
020400940224     C* valorizzo la chiave univoca per reperire i dati
020500940224     C* se non immissione
0206000301141   >C     xtaopz        IFNE      '01'
020700030204      *
020800940315    >C*-------------------------------------------------------------
020900940315    >C* impostare i campi chiave ricevuti
021000940315    >C*-------------------------------------------------------------
021100110927     C     KTBL          chain(N)  tntbe01L
021200940614     C*
021300110927     C                   IF        %Found(tntbe01L)
021400110927      *
021500110927     C                   MOVEL     tbeUNI        EXTDS
021600110927      *
021700110901     c*******            if        TblFLG = '*'
021800110901     c*******            seton                                        08
021900110901     C*******            MOVEl     Annull        D1Ann
022000110901     C*******            end
022100030113     C*
022200940224     C* riempio la videata
022300940224     C                   EXSR      RIED1
022400030113      *
022500940224     C* richiamo routine dei ctrl per decodificarla
022600940614    >C                   EXSR      CTRD1
022700030113      *
022800940224     C                   MOVE      *ALL'0'       IN5098
022900940224     C                   MOVEA     IN5098        *IN(50)
023000030113      *
023100940224     C* definizione indicatori su video
023200940224     C                   SETOFF                                       02
0233009402243    C                   SELECT
023400030114    >C     xtaopz        WHENEQ    '02'
023500940224     C                   SETON                                        02
023600030114    >C     xtaopz        WHENEQ    '05'
0237009402243-   C                   ENDSL
023800940224     C*
0239009402242-   C                   ENDIF
024000030114     C*
0241000301141   >C                   Else
024200030114     C* Se in Immissione nuovo record
024300030114     c                   exsr      NewD1
024400940223     C*
0245009402241-   C                   ENDIF
024600940224     C*
024700940117     C                   ENDSR
024800940207     C/EJECT
024900940207     C************************************************************
025000940510     C* RIEMPIMENTO VIDEATA  D1
025100940207     C************************************************************
025200030114     C     NEWD1         BEGSR
025300940207     C*
025400030114     c                   clear                   d1
025500030114     C*
025600030114     C                   ENDSR
025700030114     C************************************************************
025800030114     C* RIEMPIMENTO VIDEATA  D1
025900030114     C************************************************************
026000030114     C     RIED1         BEGSR
026100030114     C*
026200160413     c                   clear                   d1sost
026300160413     c                   clear                   d1desc
026400030204     C*
026500160413     c                   eval        d1sost = SCSSOST
026600160413     c                   eval        d1desc = SCSDESC
026700120412     C*
026800940207     C                   ENDSR
026900940309     C************************************************************
027000940309     C* GESTIONE F03 VIDEO D1
027100940309     C************************************************************
027200940309     C     F03D1         BEGSR
027300940309     C*
027400940309     C                   MOVE      *ON           $FINE
027500030113    >C                   MOVE      '1'           xtaret
027600940325     C*
027700940325     C                   ENDSR
027800940309     C************************************************************
027900940309     C* GESTIONE F12 VIDEO D1
028000940309     C************************************************************
028100940309     C     F12D1         BEGSR
028200940309     C*
028300940309     C                   MOVE      *ON           $FINE
028400940309     C*
028500940309     C                   ENDSR
028600940131     C************************************************************
028700940207     C* CONTROLLO VIDEATA
028800940131     C************************************************************
028900940127     C     CTRD1         BEGSR
029000940131     C*
029100940127     C                   SETOFF                                       99
029200030123      *
029300940127     C* non imposto pos. del cursore
029400940127     C                   Z-ADD     0             H1RIGA
029500940127     C                   Z-ADD     0             H1COLO
029600940621     C*
029700940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0298000301141   >C     xtaopz        IFNE      '04'
029900971110     C*
030000160413     C* controllo codice SCAN vuoto
030100971110$017 C     D1COD         IFEQ      *ALL' '
030200971110     C                   SETON                                        5099
030300120412     C                   Else
030400971110     C                   ENDIF
030500110908     C*
030600160413     C* controllo stringa vuota
030700160413$017 C     D1subSC       IFEQ      *ALL' '
030800160413     C                   SETON                                        5199
030900160413     C                   Else
031000160413     C                   ENDIF
031100160413     C*
031200160413     C* controllo codice sostituivo dello SCAN vuoto
031300160413$017 C     D1sost        IFEQ      *ALL' '
031400160413     C                   SETON                                        5499
031500160413     C                   Else
031600160421      *  controllo validità distinta
031700160421     c     digits        check     d1sost
031800160421     c                   if        %Found
031900160421     C                   SETON                                        5499
032000160421     c                   end
032100160421      *
032200160413     C                   ENDIF
032300160413     C*
032400160413     C* controllo descrizione vuota
032500160413$017 C     D1desc        IFEQ      *ALL' '
032600160413     C                   SETON                                        5599
032700160413     C                   Else
032800160413     C                   ENDIF
032900160413     C*
033000940207     C*
033100940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
033200030114    >C     xtaopz        IFEQ      '01'
033300030114    >C     xtaopz        oreq      '03'
033400110927     c                   clear                   tbeKE1
033500160413     c                   clear                   tbeKE2
033600110927     c                   movel     d1cod         tbeKE1
033700160412     c                   movel     d1subSC       tbeKE2
033800110927     C     KTBL          chain(N)  tntbe01L
033900110927     c                   if        %Found(tntbe01L)
034000110908     C                   SETON                                        5399
034100030114     C                   EndIF
034200030114     C                   ENDIF
034300030114      *
0344009402241-   C                   END
034500940131     C*
034600940302     C     *IN99         IFEQ      *ON
034700940510     C     $GEST         ANDEQ     'D1'
034800940117     C                   MOVEA     *IN           WIN
034900940128     C                   MOVE      *ALL'0'       IN5098           49
035000940128     C                   MOVEA     IN5098        *IN(50)
035100940614     C                   WRITE     D1
035200940117     C                   MOVEA     WIN           *IN
035300940302     C                   ENDIF
035400940127     C*
035500940117     C                   ENDSR
035600960305     C/EJECT
035700030204     C************************************************************
035800030204     C* A chi deve trasmettere
035900030204     C************************************************************
036000030204     C     ACHI          BEGSR
036100030204     C*
036200030204     c                   eval      S_Tasto = $Tasto
036300030204     C*
036400030204     c                   move      '1'           t1trs
036500030204     c                   clear                   t1fil
036600030204     c     su_win        tag
036700030204     C                   Exfmt     W1Tras
036800030204     c                   if        t1trs = '2' and
036900030204     c                             t1fil = 0
037000030204     c                   seton                                        77
037100030204     c                   goto      su_win
037200030204     c                   end
037300030204     C*
037400030204     c                   eval      $Tasto = S_Tasto
037500030204     C*
037600030204     C                   ENDSR
037700030204     C/EJECT
037800030204     C************************************************************
037900030204    >C* AGGIORNAMENTO ANAGRAFICA
038000030204     C************************************************************
038100030204    >C     FLDAGG        BEGSR
038200030204     C*
038300030204     C*  campi chiave da scrivere e aggiornare
038400110927     C                   clear                   tbeKE1
038500160412     C                   clear                   tbeKE2
038600110927     C                   MOVEl     D1COD         tbeKE1
038700160412     C                   MOVEl     D1subSC       tbeKE2
038800030204     C*
038900030204     C                   ENDSR
039000940207     C/EJECT
039100940224     C************************************************************
039200940315    >C* AGGIORNAMENTO ANAGRAFICA
039300940224     C************************************************************
039400940315    >C     AGGANA        BEGSR
039500940207     C*
039600030204     C                   exsr      FLDAGG
039700030204     C*
039800940207     C                   SELECT
039900030204     C*
040000030114    >C     xtaopz        WHENEQ    '01'
040100030114    >C     xtaopz        OREQ      '03'
040200030204     C*
040300160412    >C                   EXSR      RIEANA_SCS
040400110927     c                   write     tntbe000                             59
040500940318     C* se rcd non scrivibile attivo errore
040600030114    >C   59              SETON                                          99
040700030204      *
040800030114    >C     xtaopz        WHENEQ    '02'
040900030204      *
041000110927     C     KTBL          chain     tntbe01L
041100030204     C                   exsr      FLDAGG
041200160412    >C                   EXSR      RIEANA_SCS
041300110927     c                   update    tntbe000                             59
041400030114    >C   59              SETON                                          99
041500030204      *
041600030114    >C     xtaopz        WHENEQ    '04'
041700110927     C     KTBL          chain     tntbe01L
041800030115     c                   if        $tasto = F23
041900110927     c                   delete    tntbe000                             59
042000030115     c                   else
042100110927     c                   delete    tntbe000                             59
042200030115     c                   end
042300030114    >C   59              SETON                                          99
042400940621     C*
042500940207     C                   ENDSL
042600940203     C*
042700940203     C                   ENDSR
042800940224     C************************************************************
042900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
043000940224     C************************************************************
043100160412     C     RIEANA_SCS    BEGSR
043200030114     C*
043300110927     C                   CLEAR                   tbeuni
043400160412     C*
043500160413     c                   movel     d1sost        SCSSOST
043600160413     c                   movel     d1desc        SCSDESC
043700110927     C                   MOVEL     EXTDS         tbeuni
043800130301     C*
043900130301     c                   if        t1trs = '1'
044000130301     c                   eval      tbeFTT='S'
044100130301     c                   elseif    t1trs = '2'
044200130301     c                   eval      tbeFTT='S'
044300130301     c                   eval      tbeFLT=t1fil
044400130301     c                   end
044500130301     C*
044600030114     C*
044700940207     C                   ENDSR
044800940224     C************************************************************
044900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
045000940224     C************************************************************
045100940224     C     GESAGG        BEGSR
045200940224     C*
045300940224     C* segnalo al pgm chiamante l'avvenuta conferma
045400030114    >C                   MOVE      *ON           xtaopr
045500940224     C* segnalo al pgm chiamante l'aver premuto l'F6
045600030114    >C                   MOVE      '0'           xtaret
045700940224     C*
0458009402241    C                   SELECT
045900940224     C* nel caso di immissione, ripristino la videata iniziale
046000030114    >C     xtaopz        WHENEQ    '01'
046100940224     C                   MOVE      *ON           $INZD1
046200030114     C                   MOVE      *ON           $FINE
046300940224     C* altrimenti ritorno al pgm chiamante
046400940224     C                   OTHER
046500940224     C                   MOVE      *ON           $FINE
0466009402241-   C                   ENDSL
046700940224     C*
046800940224     C                   ENDSR
046900940131     C************************************************************
047000940131     C* OPERAZIONI INIZIALI
047100940131     C************************************************************
047200940131     C     *INZSR        BEGSR
047300940131     C*
047400940127     C* Reperimento parametri
047500940127     C*
047600940117     C     *ENTRY        PLIST
047700940117     C                   PARM                    KPJBA
047800030113     C                   movel     kpjbu         tabds
047900030115     c                   setoff                                       08
048000030115     C*
048100940223     C* Variabili per gestione videate
048200940223     C                   MOVE      'D1'          $GEST
048300940223     C                   MOVE      *OFF          $FINE
048400940223     C                   MOVE      *ON           $INZD1
048500940224     C                   MOVE      *BLANK        $LASTG
048600940224     C                   MOVE      *BLANK        $LASTV
048700940506     C                   Z-ADD     0             $ULKD1            3 0
048800940127     C*
048900940223     C* Variabili appoggio
049000940224     C                   Z-ADD     0             CURR
049100940224     C                   Z-ADD     0             CURC
049200940207     C*
049300940207     C* Valorizzazione campi univoci testate
049400940614     C                   CLEAR                   T1
049500940614     C                   MOVEL     KNSIF         NOMSIF
049600030113     C******             MOVEL     XSCDSI        NOMDIT
049700941027     C                   MOVEL     DSPGM         NOMPGM
049800030113     C*
049900160412     C                   move      'SCS'         tbeCOD
050000110927     C                   clear                   tbeKE1
050100110927     C                   moveL     xtakey        tbeKE1
050200160413     C                   clear                   tbeKE2
050300160413     C                   MOVEl     xtake2        tbeKE2
050400940224     C*
050500940224     C* Inizializzazione/Decodifica prima videata
050600940224     C                   EXSR      INZD1
050700940224     C                   MOVE      *OFF          $INZD1
050800940127     C*
050900940117     C                   ENDSR
051000940128     C************************************************************
051100940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
051200940223     C************************************************************
051300940223     C     DEFCAM        BEGSR
051400940223     C*
051500940223     C* klist
051600030113     C     KTBL          KLIST
051700110927     C                   KFLD                    tbeCOD
051800110927     C                   KFLD                    tbeKE1
051900160412     C                   KFLD                    tbeKE2
052000940207     C*
052100940223     C* Variabili per gestione videate
052200940223     C                   MOVE      *BLANK        $GEST             2
052300940223     C                   MOVE      *BLANK        $FINE             1
052400940223     C                   MOVE      *BLANK        $INZD1            1
052500940223     C                   MOVE      *BLANK        $LASTG            2
052600940223     C                   MOVE      *BLANK        $LASTV            2
052700940223     C* Variabili appoggio
052800940223     C                   Z-ADD     0             CURR              2 0
052900940223     C                   Z-ADD     0             CURC              2 0
053000940223     C                   MOVE      *ZEROS        WIN              99
053100940224     C                   MOVE      *BLANK        WTBERR            1
053200940223     C* Indici
053300940223     C                   Z-ADD     0             X                 3 0
053400940223     C                   Z-ADD     0             Y                 3 0
053500940127     C*
053600940117     C                   ENDSR
053700940128     C************************************************************
