000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
002500120413     fAzorg01l  if   e           K Disk
002600120412$001 FTNTB93D2  CF   E             WORKSTN
002700940201     F                                     INFDS(DSFMT)
002800110927     Ftntbe01l  UF A E           K DISK
002900940128     D*----------------------------------------------------*
003000120412$003 D EXTDS         E DS                  EXTNAME(tntb93ds)
003100030113      *-------------
003200940127     D* Passaggio Parametri
003300940127     D KPJBA         E DS
003400030113      *-------------
003500030113     D* Parametri in ricezione
003600030113     D  TABDS          DS
003700030113     D  XTAOPZ                 1      2
003800030113     D  XTARET                 3      3
003900030113     D  XTAOPR                 4      4
004000030113     D  XTAERR                 5      5
004100110926     D  XTAKEY                 6      8
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
009000940506     D*-------------
009100941108     D DATA            C                   CONST('0001-01-01')
009200030114     D Annull          C                   CONST('  ANNULLATO  ')
009300030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009400030114     D OPz_Var         C                   CONST('VARIAZIONE')
009500030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009600030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009700940127     C*----------------------------------------------------*
009800940127     C*                MAIN LINE PROGRAM
009900940127     C*----------------------------------------------------*
010000940223     C*
010100940223     C     $FINE         DOWEQ     *OFF
010200940202     C     $GEST         CASEQ     'D1'          GESD1
010300940117     C                   END
010400940117     C                   END
010500940325     C* fine programma
010600030113     c                   movel     tabds         kpjbu
010700030113     C                   Seton                                        LR
010800940131     C************************************************************
010900940510     C* GESTIONE VIDEO RECORD D1
011000940131     C************************************************************
011100940127     C     GESD1         BEGSR
011200030113      *
011300940225     C* inizializzazione videata
011400940223     C     $INZD1        IFEQ      *ON
011500940127     C                   EXSR      INZD1
011600940223     C                   MOVE      *OFF          $INZD1
011700940117     C                   END
011800030113      *
011900940225     C* emissione piede videata se proveniente da altra
012000940225     C* salvataggio valore formato di provenienza
012100940207     C     $LASTV        IFNE      'D1'
012200940614     C                   WRITE     T1
012300940202     C                   MOVE      $LASTV        $LASTG
012400940202     C                   MOVE      'D1'          $LASTV
012500940202     C                   END
012600940131     C*
012700940203     C                   SELECT
012800940203     C* Annullamento
012900030114     C* Visualizzazione
013000030114    >C     xtaopz        WHENEQ    '01'
013100030114     c                   movel     Opz_Imm       D1Fun
013200030114    >C     xtaopz        WHENEQ    '05'
013300030114     c                   movel     Opz_Vis       D1Fun
013400030114     C                   SETON                                        020306
013500030114    >C     xtaopz        WHENEQ    '04'
013600030114     c                   movel     Opz_Del       D1Fun
013700110902     C                   SETON                                        02
013800030114     C                   SETOFF                                         06
013900940620     C* Manutenzione
014000030113    >C     xtaopz        WHENEQ    '02'
014100030114     c                   movel     Opz_Var       D1Fun
014200030114     C                   SETON                                        02
014300030114     C                   SETOFF                                       0306
014400940620     C*
014500940620     C                   OTHER
014600940620     C*
014700030114     C                   SETOFF                                       020306
014800940203     C                   ENDSL
014900030114     C*
015000030114     c                   SETOFF                                         99
015100940620     C* Immissione/Modifica/Copia
015200030113     C*               *----------------*
015300940620     C                   EXFMT     D1
015400030113     C*               *----------------*
015500030114     C*
015600030114     C     *IN99         IFEQ      *OFF
015700030114     C*
0158000301141    C                   SELECT
015900030114     C* F3=Fine
016000030114     C     $TASTO        WHENEQ    F03
016100030114     C                   EXSR      F03D1
016200030114     C* F12=Ritorno
016300030114     C     $TASTO        WHENEQ    F12
016400030114     C                   EXSR      F12D1
016500030114     C*
0166000301141O   C                   OTHER
016700030114     C*
016800030114     C                   EXSR      CTRD1
016900030113     C*
017000940616     C     *IN99         IFEQ      *OFF
017100940616     C* F6
0172000302052    C     $TASTO        ifeq      F06
0173000301152    C     $TASTO        oreq      F23
017400030204     C* A chi Trasmette la Tabella ?
017500130301     c                   Exsr      ACHI
017600940224     C* eseguo aggiornamento
017700940315    >C                   EXSR      AGGANA
017800940224     C* eseguo operazioni del dopo-aggiornamento
017900030114     C  N99              EXSR      GESAGG
018000030205     c                   end
018100030114     C*
018200030114     C                   ENDIF
018300030114     C*
0184000301142-   C                   ENDSL
018500030114     C*
0186000301142-   C                   ENDIF
018700030114     C*
018800940117     C                   ENDSR
018900940117      ************************************************************
019000940207      * INIZIALIZZAZIONE VIDEATA DATI
019100940117      ************************************************************
019200940127     C     INZD1         BEGSR
019300940131     C*
019400940615     C                   CLEAR                   D1
019500120412     C                   move      'MIC'         tbeCOD
019600030113     C                   MOVEl     xtakey        D1COD
019700110927     C                   clear                   tbeKE1
019800110927     C                   MOVEl     xtakey        tbeKE1
019900940207     C*
020000940224     C* valorizzo la chiave univoca per reperire i dati
020100940224     C* se non immissione
0202000301141   >C     xtaopz        IFNE      '01'
020300030204      *
020400940315    >C*-------------------------------------------------------------
020500940315    >C* impostare i campi chiave ricevuti
020600940315    >C*-------------------------------------------------------------
020700110927     C     KTBL          chain(N)  tntbe01L
020800940614     C*
020900110927     C                   IF        %Found(tntbe01L)
021000110927      *
021100110927     C                   MOVEL     tbeUNI        EXTDS
021200110927      *
021300110901     c*******            if        TblFLG = '*'
021400110901     c*******            seton                                        08
021500110901     C*******            MOVEl     Annull        D1Ann
021600110901     C*******            end
021700030113     C*
021800940224     C* riempio la videata
021900940224     C                   EXSR      RIED1
022000030113      *
022100940224     C* richiamo routine dei ctrl per decodificarla
022200940614    >C                   EXSR      CTRD1
022300030113      *
022400940224     C                   MOVE      *ALL'0'       IN5098
022500940224     C                   MOVEA     IN5098        *IN(50)
022600030113      *
022700940224     C* definizione indicatori su video
022800940224     C                   SETOFF                                       02
0229009402243    C                   SELECT
023000030114    >C     xtaopz        WHENEQ    '02'
023100940224     C                   SETON                                        02
023200030114    >C     xtaopz        WHENEQ    '05'
0233009402243-   C                   ENDSL
023400940224     C*
0235009402242-   C                   ENDIF
023600030114     C*
0237000301141   >C                   Else
023800030114     C* Se in Immissione nuovo record
023900030114     c                   exsr      NewD1
024000940223     C*
0241009402241-   C                   ENDIF
024200940224     C*
024300940117     C                   ENDSR
024400940207     C/EJECT
024500940207     C************************************************************
024600940510     C* RIEMPIMENTO VIDEATA  D1
024700940207     C************************************************************
024800030114     C     NEWD1         BEGSR
024900940207     C*
025000030114     c                   clear                   d1
025100030114     C*
025200030114     C                   ENDSR
025300030114     C************************************************************
025400030114     C* RIEMPIMENTO VIDEATA  D1
025500030114     C************************************************************
025600030114     C     RIED1         BEGSR
025700030114     C*
025800120412     c                   clear                   d1des
025900110927     C* campi delle 45 settimane possibili
026000120412     c                   if         B93MMIC <> *BLANK
026100120412     c                   move      B93MMIC       d1MMIC
026200120412     C     D1MMIC        CHAIN     AZORG01L
026300120412     c                   if        %Found(AZORG01L)
026400120412     c                   movel     orgdes        d1des
026500120412     c                   end
026600120412     c                   else
026700120412     c                   eval      d1MMIC  =     0
026800120412     c                   end
026900030204     C*
027000120412     C*
027100940207     C                   ENDSR
027200940309     C************************************************************
027300940309     C* GESTIONE F03 VIDEO D1
027400940309     C************************************************************
027500940309     C     F03D1         BEGSR
027600940309     C*
027700940309     C                   MOVE      *ON           $FINE
027800030113    >C                   MOVE      '1'           xtaret
027900940325     C*
028000940325     C                   ENDSR
028100940309     C************************************************************
028200940309     C* GESTIONE F12 VIDEO D1
028300940309     C************************************************************
028400940309     C     F12D1         BEGSR
028500940309     C*
028600940309     C                   MOVE      *ON           $FINE
028700940309     C*
028800940309     C                   ENDSR
028900940131     C************************************************************
029000940207     C* CONTROLLO VIDEATA
029100940131     C************************************************************
029200940127     C     CTRD1         BEGSR
029300940131     C*
029400940127     C                   SETOFF                                       99
029500030123      *
029600940127     C* non imposto pos. del cursore
029700940127     C                   Z-ADD     0             H1RIGA
029800940127     C                   Z-ADD     0             H1COLO
029900940621     C*
030000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0301000301141   >C     xtaopz        IFNE      '04'
030200971110     C*
030300971110     C* controllo codice vuoto
030400971110$017 C     D1COD         IFEQ      *ALL' '
030500971110     C                   SETON                                        5099
030600120412     C                   Else
030700120412     c                   move      d1cod         d1fil             3 0
030800120412     C     D1fil         CHAIN     AZORG01L
030900120412     c                   if        %Found(AZORG01L) and orgfag ='F'
031000120412     c                   movel     orgdes        d1des
031100120412     c                   else
031200120412     C                   SETON                                        5099
031300120412     c                   end
031400971110     C                   ENDIF
031500110908     C*
031600940207     C*
031700940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
031800030114    >C     xtaopz        IFEQ      '01'
031900030114    >C     xtaopz        oreq      '03'
032000110927     c                   clear                   tbeKE1
032100110927     c                   movel     d1cod         tbeKE1
032200110927     C     KTBL          chain(N)  tntbe01L
032300110927     c                   if        %Found(tntbe01L)
032400110908     C                   SETON                                        5399
032500030114     C                   EndIF
032600030114     C                   ENDIF
032700030114      *
0328009402241-   C                   END
032900940131     C*
033000940302     C     *IN99         IFEQ      *ON
033100940510     C     $GEST         ANDEQ     'D1'
033200940117     C                   MOVEA     *IN           WIN
033300940128     C                   MOVE      *ALL'0'       IN5098           49
033400940128     C                   MOVEA     IN5098        *IN(50)
033500940614     C                   WRITE     D1
033600940117     C                   MOVEA     WIN           *IN
033700940302     C                   ENDIF
033800940127     C*
033900940117     C                   ENDSR
034000960305     C/EJECT
034100030204     C************************************************************
034200030204     C* A chi deve trasmettere
034300030204     C************************************************************
034400030204     C     ACHI          BEGSR
034500030204     C*
034600030204     c                   eval      S_Tasto = $Tasto
034700030204     C*
034800030204     c                   move      '1'           t1trs
034900030204     c                   clear                   t1fil
035000030204     c     su_win        tag
035100030204     C                   Exfmt     W1Tras
035200030204     c                   if        t1trs = '2' and
035300030204     c                             t1fil = 0
035400030204     c                   seton                                        77
035500030204     c                   goto      su_win
035600030204     c                   end
035700030204     C*
035800030204     c                   eval      $Tasto = S_Tasto
035900030204     C*
036000030204     C                   ENDSR
036100030204     C/EJECT
036200030204     C************************************************************
036300030204    >C* AGGIORNAMENTO ANAGRAFICA
036400030204     C************************************************************
036500030204    >C     FLDAGG        BEGSR
036600030204     C*
036700030204     C*  campi chiave da scrivere e aggiornare
036800110927     C                   clear                   tbeKE1
036900110927     C                   MOVEl     D1COD         tbeKE1
037000030204     C*
037100030204     C                   ENDSR
037200940207     C/EJECT
037300940224     C************************************************************
037400940315    >C* AGGIORNAMENTO ANAGRAFICA
037500940224     C************************************************************
037600940315    >C     AGGANA        BEGSR
037700940207     C*
037800030204     C                   exsr      FLDAGG
037900030204     C*
038000940207     C                   SELECT
038100030204     C*
038200030114    >C     xtaopz        WHENEQ    '01'
038300030114    >C     xtaopz        OREQ      '03'
038400030204     C*
038500120412     C                   move      'MIC'         tbeCOD
038600120412    >C                   EXSR      RIEANA_MIC
038700110927     c                   write     tntbe000                             59
038800940318     C* se rcd non scrivibile attivo errore
038900030114    >C   59              SETON                                          99
039000030204      *
039100030114    >C     xtaopz        WHENEQ    '02'
039200030204      *
039300120412     C                   move      'MIC'         tbeCOD
039400110927     C     KTBL          chain     tntbe01L
039500030204     C                   exsr      FLDAGG
039600120412    >C                   EXSR      RIEANA_MIC
039700110927     c                   update    tntbe000                             59
039800030114    >C   59              SETON                                          99
039900030204      *
040000030114    >C     xtaopz        WHENEQ    '04'
040100120412     C                   move      'MIC'         tbeCOD
040200110927     C     KTBL          chain     tntbe01L
040300030115     c                   if        $tasto = F23
040400110927     c                   delete    tntbe000                             59
040500030115     c                   else
040600110927     c                   delete    tntbe000                             59
040700030115     c                   end
040800030114    >C   59              SETON                                          99
040900940621     C*
041000940207     C                   ENDSL
041100940203     C*
041200940203     C                   ENDSR
041300940224     C************************************************************
041400940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
041500940224     C************************************************************
041600120412     C     RIEANA_MIC    BEGSR
041700030114     C*
041800110927     C                   CLEAR                   tbeuni
041900110927     C                   movel     *all'0'       tbeuni
042000110928     C                   MOVEL     *all'0'       EXTDS
042100110927     C* campi delle 45 settimane possibili
042200120412     c                   if            d1MMIC >  0
042300120412     c                   move      d1MMIC        B93MMIC
042400110928     c                   end
042500110927     C                   MOVEL     EXTDS         tbeuni
042600130301     C*
042700130301     c                   if        t1trs = '1'
042800130301     c                   eval      tbeFTT='S'
042900130301     c                   elseif    t1trs = '2'
043000130301     c                   eval      tbeFTT='S'
043100130301     c                   eval      tbeFLT=t1fil
043200130301     c                   end
043300130301     C*
043400030114     C*
043500940207     C                   ENDSR
043600940224     C************************************************************
043700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
043800940224     C************************************************************
043900940224     C     GESAGG        BEGSR
044000940224     C*
044100940224     C* segnalo al pgm chiamante l'avvenuta conferma
044200030114    >C                   MOVE      *ON           xtaopr
044300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
044400030114    >C                   MOVE      '0'           xtaret
044500940224     C*
0446009402241    C                   SELECT
044700940224     C* nel caso di immissione, ripristino la videata iniziale
044800030114    >C     xtaopz        WHENEQ    '01'
044900940224     C                   MOVE      *ON           $INZD1
045000030114     C                   MOVE      *ON           $FINE
045100940224     C* altrimenti ritorno al pgm chiamante
045200940224     C                   OTHER
045300940224     C                   MOVE      *ON           $FINE
0454009402241-   C                   ENDSL
045500940224     C*
045600940224     C                   ENDSR
045700940131     C************************************************************
045800940131     C* OPERAZIONI INIZIALI
045900940131     C************************************************************
046000940131     C     *INZSR        BEGSR
046100940131     C*
046200940127     C* Reperimento parametri
046300940127     C*
046400940117     C     *ENTRY        PLIST
046500940117     C                   PARM                    KPJBA
046600030113     C                   movel     kpjbu         tabds
046700030115     c                   setoff                                       08
046800030115     C*
046900940223     C* Variabili per gestione videate
047000940223     C                   MOVE      'D1'          $GEST
047100940223     C                   MOVE      *OFF          $FINE
047200940223     C                   MOVE      *ON           $INZD1
047300940224     C                   MOVE      *BLANK        $LASTG
047400940224     C                   MOVE      *BLANK        $LASTV
047500940506     C                   Z-ADD     0             $ULKD1            3 0
047600940127     C*
047700940223     C* Variabili appoggio
047800940224     C                   Z-ADD     0             CURR
047900940224     C                   Z-ADD     0             CURC
048000940207     C*
048100940207     C* Valorizzazione campi univoci testate
048200940614     C                   CLEAR                   T1
048300940614     C                   MOVEL     KNSIF         NOMSIF
048400030113     C******             MOVEL     XSCDSI        NOMDIT
048500941027     C                   MOVEL     DSPGM         NOMPGM
048600030113     C*
048700120412     C                   move      'MIC'         tbeCOD
048800110927     C                   clear                   tbeKE1
048900110927     C                   moveL     xtakey        tbeKE1
049000940224     C*
049100940224     C* Inizializzazione/Decodifica prima videata
049200940224     C                   EXSR      INZD1
049300940224     C                   MOVE      *OFF          $INZD1
049400940127     C*
049500940117     C                   ENDSR
049600940128     C************************************************************
049700940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
049800940223     C************************************************************
049900940223     C     DEFCAM        BEGSR
050000940223     C*
050100940223     C* klist
050200030113     C     KTBL          KLIST
050300110927     C                   KFLD                    tbeCOD
050400110927     C                   KFLD                    tbeKE1
050500940207     C*
050600940223     C* Variabili per gestione videate
050700940223     C*
050800940223     C                   MOVE      *BLANK        $GEST             2
050900940223     C                   MOVE      *BLANK        $FINE             1
051000940223     C                   MOVE      *BLANK        $INZD1            1
051100940223     C                   MOVE      *BLANK        $LASTG            2
051200940223     C                   MOVE      *BLANK        $LASTV            2
051300940223     C*
051400940223     C* Variabili appoggio
051500940223     C                   Z-ADD     0             CURR              2 0
051600940223     C                   Z-ADD     0             CURC              2 0
051700940223     C                   MOVE      *ZEROS        WIN              99
051800940224     C                   MOVE      *BLANK        WTBERR            1
051900940223     C*
052000940223     C* Indici
052100940223     C                   Z-ADD     0             X                 3 0
052200940223     C                   Z-ADD     0             Y                 3 0
052300940127     C*
052400940117     C                   ENDSR
052500940128     C************************************************************
