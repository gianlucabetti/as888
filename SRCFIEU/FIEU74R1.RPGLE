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
002500040115$001 FFIEU74D1  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700040115     FTncfp02L  UF A E           K DISK
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
003900040115     D  XTACOD                 5      7
004000040115     D  XTANTW                 8     10
004100040115     D  XTAFIL                11     13  0
004200040115     D  XTADDA                14     21  0
004300040115     D  XTAADA                22     29  0
004400940201     D*-------------
004500030204     D  S_TASTO        S                   like($Tasto)
004600940201     D DSFMT           DS
004700940506     D  $TASTO               369    369
004800940201     D  NRG                  370    370
004900940201     D  NCL                  371    371
005000940201     D*-------------
005100940201     D* posizione cursore
005200940201     D CURSOR          DS
005300940223     D  RIRI                   1      2B 0 INZ
005400940201     D  R$$                    2      2
005500940223     D  COCO                   3      4B 0 INZ
005600940201     D  C$$                    4      4
005700030728      *
005800030728     Ddataiso          s               d   datfmt(*iso)
005900030728     Ddataeur          s               d   datfmt(*eur)
006000940207     D*-------------
006100940207     D* Reperimento nome PGM
006200940207     D STATUS         SDS           333
006300940207     D  DSPGM            *PROC
006400940225     D*-------------
006500940225     D* COSTANTI
006600940225     D*-------------
006700940506     D* Tasti di funzione
006800940506     D F01             C                   CONST(X'31')
006900940506     D F02             C                   CONST(X'32')
007000940506     D F03             C                   CONST(X'33')
007100940506     D F04             C                   CONST(X'34')
007200940506     D F05             C                   CONST(X'35')
007300940506     D F06             C                   CONST(X'36')
007400940506     D F07             C                   CONST(X'37')
007500940506     D F08             C                   CONST(X'38')
007600940506     D F09             C                   CONST(X'39')
007700940506     D F10             C                   CONST(X'3A')
007800940506     D F11             C                   CONST(X'3B')
007900940506     D F12             C                   CONST(X'3C')
008000940506     D F13             C                   CONST(X'B1')
008100940506     D F14             C                   CONST(X'B2')
008200940506     D F15             C                   CONST(X'B3')
008300940506     D F16             C                   CONST(X'B4')
008400940506     D F17             C                   CONST(X'B5')
008500940506     D F18             C                   CONST(X'B6')
008600940506     D F19             C                   CONST(X'B7')
008700940506     D F20             C                   CONST(X'B8')
008800940506     D F21             C                   CONST(X'B9')
008900940506     D F22             C                   CONST(X'BA')
009000940506     D F23             C                   CONST(X'BB')
009100940506     D F24             C                   CONST(X'BC')
009200940506     D ENTER           C                   CONST(X'F1')
009300940506     D ROLDWN          C                   CONST(X'F4')
009400940506     D ROLLUP          C                   CONST(X'F5')
009500940506     D*-------------
009600941108     D DATA            C                   CONST('0001-01-01')
009700030114     D Annull          C                   CONST('  ANNULLATO  ')
009800030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009900030114     D OPz_Var         C                   CONST('VARIAZIONE')
010000030114     D OPz_Del         C                   CONST(' ANNULLA  ')
010100030114     D OPz_Vis         C                   CONST('VISUALIZZA')
010200940127     C*----------------------------------------------------*
010300940127     C*                MAIN LINE PROGRAM
010400940127     C*----------------------------------------------------*
010500940223     C*
010600940223     C     $FINE         DOWEQ     *OFF
010700940202     C     $GEST         CASEQ     'D1'          GESD1
010800940117     C                   END
010900940117     C                   END
011000940325     C* fine programma
011100030113     c                   movel     tabds         kpjbu
011200030113     C                   Seton                                        LR
011300940131     C************************************************************
011400940510     C* GESTIONE VIDEO RECORD D1
011500940131     C************************************************************
011600940127     C     GESD1         BEGSR
011700030113      *
011800940225     C* inizializzazione videata
011900940223     C     $INZD1        IFEQ      *ON
012000940127     C                   EXSR      INZD1
012100940223     C                   MOVE      *OFF          $INZD1
012200940117     C                   END
012300030113      *
012400940225     C* emissione piede videata se proveniente da altra
012500940225     C* salvataggio valore formato di provenienza
012600940207     C     $LASTV        IFNE      'D1'
012700940614     C                   WRITE     T1
012800940202     C                   MOVE      $LASTV        $LASTG
012900940202     C                   MOVE      'D1'          $LASTV
013000940202     C                   END
013100940131     C*
013200940203     C                   SELECT
013300940203     C* Annullamento
013400030114     C* Visualizzazione
013500030729    >C     xtaopz        WHENEQ    '1'
013600030114     c                   movel     Opz_Imm       D1Fun
013700030729    >C     xtaopz        WHENEQ    '5'
013800030114     c                   movel     Opz_Vis       D1Fun
013900030114     C                   SETON                                        020306
014000030729    >C     xtaopz        WHENEQ    '4'
014100030114     c                   movel     Opz_Del       D1Fun
014200030114     C                   SETON                                        0203
014300030114     C                   SETOFF                                         06
014400940620     C* Manutenzione
014500030729    >C     xtaopz        WHENEQ    '2'
014600030114     c                   movel     Opz_Var       D1Fun
014700030114     C                   SETON                                        02
014800030114     C                   SETOFF                                       0306
014900940620     C*
015000940620     C                   OTHER
015100940620     C*
015200030114     C                   SETOFF                                       020306
015300940203     C                   ENDSL
015400030113     C*
015500030728     c     video1        tag
015600030114     c                   SETOFF                                         99
015700940620     C* Immissione/Modifica/Copia
015800030113     C*               *----------------*
015900940620     C                   EXFMT     D1
016000030113     C*               *----------------*
016100030114     C*
016200030114     C     *IN99         IFEQ      *OFF
016300030114     C*
0164000301141    C                   SELECT
016500030114     C* F3=Fine
016600030114     C     $TASTO        WHENEQ    F03
016700030114     C                   EXSR      F03D1
016800030114     C* F12=Ritorno
016900030114     C     $TASTO        WHENEQ    F12
017000030114     C                   EXSR      F12D1
017100030114     C*
0172000301141O   C                   OTHER
017300030114     C*
017400030114     C                   EXSR      CTRD1
017500030728     c   99              goto      video1
017600030113     C*
017700940616     C     *IN99         IFEQ      *OFF
017800940616     C* F6
0179000302052    C     $TASTO        ifeq      F06
0180000301152    C     $TASTO        oreq      F23
018100940224     C* eseguo aggiornamento
018200940315    >C                   EXSR      AGGANA
018300940224     C* eseguo operazioni del dopo-aggiornamento
018400030114     C  N99              EXSR      GESAGG
018500030205     c                   end
018600030114     C*
018700030114     C                   ENDIF
018800030114     C*
0189000301142-   C                   ENDSL
019000030114     C*
0191000301142-   C                   ENDIF
019200940224     C*
019300940117     C                   ENDSR
019400940117      ************************************************************
019500940207      * INIZIALIZZAZIONE VIDEATA DATI
019600940117      ************************************************************
019700940127     C     INZD1         BEGSR
019800940131     C*
019900940615     C                   CLEAR                   D1
020000030729     C                   MOVEl     xtaCOD        D1COD
020100040115     C                   movel(P)  xtaCOD        X15COD
020200940207     C*
020300940224     C* valorizzo la chiave univoca per reperire i dati
020400940224     C* se non immissione
0205000307291   >C     xtaopz        IFNE      '1'
020600030204      *
020700940315    >C*-------------------------------------------------------------
020800940315    >C* impostare i campi chiave ricevuti
020900940315    >C*-------------------------------------------------------------
021000040115     C     Kcfp          chain(N)  TNcfp02L
021100940614     C*
021200040115     C                   IF        %Found(TNcfp02L)
021300040115     c                   movel     cfpCAU        d1cod
021400040115     c                   eval        d1fil =  cfpfil
021500040115     c                   eval        d1ntw =  cfpntw
021600040115     c                   eval        h1dda =  cfpdda
021700040115     c                   eval        h1ADA =  cfpada
021800040115     c                   movel     cfpdda        dataiso
021900030728     c                   movel     dataiso       dataeur
022000030729     c                   movel     dataeur       d1dda
022100040115     c                   movel     cfpada        dataiso
022200030728     c                   movel     dataiso       dataeur
022300030729     c                   movel     dataeur       d1ADA
022400040115     c                   movel     cfpdes        d1des
022500040115     c                   movel     cfpgrp        d1grp
022600040115     c                   if        cfpANN = 'A'  and xtaOPZ <>'3'
022700030115     c                   seton                                        08
022800030114     C                   MOVEl     Annull        D1Ann
022900030114     C                   end
023000030113      *
023100030729     C* richiamo routine dei ctrl per decodificarla
023200940614    >C                   EXSR      CTRD1
023300030113      *
023400960305    >C     *IN99         IFEQ      *ON
023500960305    >C                   MOVEL     *IN99         SAV99             1
023600960305    >C                   ENDIF
023700030113      *
023800940224     C                   MOVE      *ALL'0'       IN5098
023900940224     C                   MOVEA     IN5098        *IN(50)
024000030113      *
024100940224     C* definizione indicatori su video
024200940224     C                   SETOFF                                       02
0243009402243    C                   SELECT
024400030729    >C     xtaopz        WHENEQ    '2'
024500940224     C                   SETON                                        02
024600030729    >C     xtaopz        WHENEQ    '5'
0247009402243-   C                   ENDSL
024800940224     C*
0249009402242-   C                   ENDIF
025000030114     C*
0251000301141   >C                   Else
025200030114     C* Se in Immissione nuovo record
025300030728     c                   clear                   d1
025400940223     C*
0255009402241-   C                   ENDIF
025600940224     C*
025700940117     C                   ENDSR
025800940309     C************************************************************
025900940309     C* GESTIONE F03 VIDEO D1
026000940309     C************************************************************
026100940309     C     F03D1         BEGSR
026200940309     C*
026300940309     C                   MOVE      *ON           $FINE
026400030113    >C                   MOVE      '1'           xtaret
026500940325     C*
026600940325     C                   ENDSR
026700940309     C************************************************************
026800940309     C* GESTIONE F12 VIDEO D1
026900940309     C************************************************************
027000940309     C     F12D1         BEGSR
027100940309     C*
027200940309     C                   MOVE      *ON           $FINE
027300940309     C*
027400940309     C                   ENDSR
027500940131     C************************************************************
027600940207     C* CONTROLLO VIDEATA
027700940131     C************************************************************
027800940127     C     CTRD1         BEGSR
027900940131     C*
028000940127     C                   SETOFF                                       99
028100030123      *
028200940127     C* non imposto pos. del cursore
028300940127     C                   Z-ADD     0             H1RIGA
028400940127     C                   Z-ADD     0             H1COLO
028500940621     C*
028600940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0287000307291   >C     xtaopz        IFNE      '4'
028800030728     c* data decorrenza
028900030729     c                   if        d1dda  <> 0
029000030729     c     *eur          test(d)                 d1dda                  50
029100030728     c                   if        *in50
029200030729     c     *dmy          test(d)                 d1dda                  50
029300030729     c  n50*dmy          move      d1dda         dataeur
029400030729     c  n50              move      dataeur       d1dda
029500030728     c  n50              move      dataeur       dataiso
029600030729     c  n50              move      dataiso       h1dda
029700030728     c                   else
029800030729     c                   move      d1dda         dataeur
029900030729     c                   move      dataeur       d1dda
030000030728     c                   move      dataeur       dataiso
030100030729     c                   move      dataiso       h1dda
030200030728     c                   end
030300030728     c                   else
030400030728     c                   seton                                        50
030500030728     c                   end
030600030728     c   50              seton                                        99
030700030728     c* data scadenza
030800030729     c                   if        d1ADA  <> 0
030900030729     c     *eur          test(d)                 d1ADA                  51
031000030728     c                   if        *in51
031100030729     c     *dmy          test(d)                 d1ADA                  51
031200030729     c  n51*dmy          move      d1ADA         dataeur
031300030729     c  n51              move      dataeur       d1ADA
031400030728     c  n51              move      dataeur       dataiso
031500030729     c  n51              move      dataiso       h1ADA
031600030728     c                   else
031700030729     c                   move      d1ADA         dataeur
031800030729     c                   move      dataeur       d1ADA
031900030728     c                   move      dataeur       dataiso
032000030729     c                   move      dataiso       h1ADA
032100030728     c                   end
032200030728     c                   else
032300030728     c                   seton                                          51
032400030728     c                   end
032500030728     c   51              seton                                        99
032600030728     c*
032700030728     c*  sequenza date
032800030729     c                   if        h1ADA < h1dda and
032900030728     c                             *in51 = *Off and *in52 = *Off
033000030728     c                   seton                                        5299
033100030728     c                   end
033200030728     c*
033300030728     c* gruppo appartenenza
033400040607     c                   if        d1grp =  *Blank
033500030728     c                   seton                                        5399
033600030728     c                   end
033700030728     c*
033800030728     c* Codice Voce vuoto
033900030729     c                   if        d1cod =  *blank
034000030728     c                   seton                                        5499
034100030728     c                   end
034200030728     c*
034300940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
034400030729    >C     xtaopz        IFEQ      '1'
034500030729    >C     xtaopz        oreq      '3'
034600030728     C*
034700030728     c* controllo di non sovrapposizione periodi
034800040115     C                   movel(P)  D1COD         D1COD15
034900040115     C     Kev           setll     tncfp02l
035000040115     C     Kev           reade     tncfp02l
035100040115     c                   dow       not %eof(tncfp02l)
035200040115     c                   if        cfpdda <= h1dda and
035300040115     c                             cfpada >= h1dda or
035400040115     c                             cfpdda <= h1ADA and
035500040115     c                             cfpada >= h1ADA
035600030728     c                   seton                                        5599
035700030728     c                   leave
035800030728     c                   end
035900040115     C     Kev           reade     tncfp02l
036000030728     c                   end
036100030114     C                   ENDIF
036200030114     C*
036300030114     C* controllo codice vuoto
036400030729$017 C     D1cod         IFEQ      *blank
036500030728     C                   SETON                                        5499
036600030114     C                   End
036700030114      *
0368009402241-   C                   END
036900940131     C*
037000940302     C     *IN99         IFEQ      *ON
037100940510     C     $GEST         ANDEQ     'D1'
037200940117     C                   MOVEA     *IN           WIN
037300940128     C                   MOVE      *ALL'0'       IN5098           49
037400940128     C                   MOVEA     IN5098        *IN(50)
037500940614     C                   WRITE     D1
037600940117     C                   MOVEA     WIN           *IN
037700940302     C                   ENDIF
037800940127     C*
037900940117     C                   ENDSR
038000940207     C/EJECT
038100940224     C************************************************************
038200940315    >C* AGGIORNAMENTO ANAGRAFICA
038300940224     C************************************************************
038400940315    >C     AGGANA        BEGSR
038500940207     C*
038600940207     C                   SELECT
038700030204     C*
038800030729    >C     xtaopz        WHENEQ    '1'
038900030729    >C     xtaopz        OREQ      '3'
039000030204     C*
039100030728    >C                   EXSR      RIEANA
039200040115     c                   write     Tncfp000                             59
039300940318     C* se rcd non scrivibile attivo errore
039400030114    >C   59              SETON                                          99
039500030204      *
039600030729    >C     xtaopz        WHENEQ    '2'
039700030204      *
039800040115     C     Kcfp          chain     Tncfp02L
039900030728    >C                   EXSR      RIEANA
040000040115     c                   update    Tncfp000                             59
040100030114    >C   59              SETON                                          99
040200030204      *
040300030729    >C     xtaopz        WHENEQ    '4'
040400040115     C     Kcfp          chain     Tncfp02L
040500040115     c                   move      'A'           cfpANN
040600030115     c                   if        $tasto = F23
040700040115     c                   delete    Tncfp000                             59
040800030115     c                   else
040900040115     c                   update    Tncfp000                             59
041000030115     c                   end
041100030114    >C   59              SETON                                          99
041200030204      *
041300940207     C                   ENDSL
041400940203     C*
041500940203     C                   ENDSR
041600940224     C************************************************************
041700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
041800940224     C************************************************************
041900030728     C     RIEANA        BEGSR
042000030114     C*
042100040115     c                   movel     d1DES         cfpDES
042200040115     c                   movel     d1cod         cfpCAU
042300040115     c                   movel     d1GRP         cfpGRP
042400040115     c                   movel     d1NTW         cfpNTW
042500040115     c                   movel     d1FIL         cfpFIL
042600030728     c                   movel     d1DDA         dataeur
042700030728     c                   movel     dataeur       dataiso
042800040115     c                   movel     dataiso       cfpDDA
042900030728     c                   movel     d1ADA         dataeur
043000030728     c                   movel     dataeur       dataiso
043100040115     c                   movel     dataiso       cfpADA
043200040115     c                   movel     ' '           cfpANN
043300030114     C*
043400940207     C                   ENDSR
043500940224     C************************************************************
043600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
043700940224     C************************************************************
043800940224     C     GESAGG        BEGSR
043900940224     C*
044000940224     C* segnalo al pgm chiamante l'avvenuta conferma
044100030114    >C                   MOVE      *ON           xtaopr
044200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
044300030114    >C                   MOVE      '0'           xtaret
044400940224     C*
0445009402241    C                   SELECT
044600940224     C* nel caso di immissione, ripristino la videata iniziale
044700030729    >C     xtaopz        WHENEQ    '1'
044800940224     C                   MOVE      *ON           $INZD1
044900030114     C                   MOVE      *ON           $FINE
045000940224     C* altrimenti ritorno al pgm chiamante
045100940224     C                   OTHER
045200940224     C                   MOVE      *ON           $FINE
0453009402241-   C                   ENDSL
045400940224     C*
045500940224     C                   ENDSR
045600940131     C************************************************************
045700940131     C* OPERAZIONI INIZIALI
045800940131     C************************************************************
045900940131     C     *INZSR        BEGSR
046000940131     C*
046100940127     C* Reperimento parametri
046200940127     C*
046300940117     C     *ENTRY        PLIST
046400940117     C                   PARM                    KPJBA
046500030113     C                   movel     kpjbu         tabds
046600030115     c                   setoff                                       08
046700030115     C*
046800940223     C* Variabili per gestione videate
046900940223     C                   MOVE      'D1'          $GEST
047000940223     C                   MOVE      *OFF          $FINE
047100940223     C                   MOVE      *ON           $INZD1
047200940224     C                   MOVE      *BLANK        $LASTG
047300940224     C                   MOVE      *BLANK        $LASTV
047400940506     C                   Z-ADD     0             $ULKD1            3 0
047500940127     C*
047600940223     C* Variabili appoggio
047700940224     C                   Z-ADD     0             CURR
047800940224     C                   Z-ADD     0             CURC
047900940207     C*
048000940207     C* Valorizzazione campi univoci testate
048100940614     C                   CLEAR                   T1
048200940614     C                   MOVEL     KNSIF         NOMSIF
048300941027     C                   MOVEL     DSPGM         NOMPGM
048400030113     C*
048500040115     C                   movel(P)  XTACOD        cfpCAU
048600040115     C                   movel     XTANTW        cfpNTW
048700040115     C                   z-add     XTAFIL        cfpFIL
048800040115     c                   z-add     XTADDA        cfpDDA
048900940224     C*
049000940224     C* Inizializzazione/Decodifica prima videata
049100940224     C                   EXSR      INZD1
049200940224     C                   MOVE      *OFF          $INZD1
049300940127     C*
049400940117     C                   ENDSR
049500940128     C************************************************************
049600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
049700030729     c************************************************************
049800940223     C     DEFCAM        BEGSR
049900940223     C*
050000940223     C* klist
050100040115     C     Kcfp          KLIST
050200030728     C                   KFLD                    XTANTW
050300040115     C                   KFLD                    X15COD           15
050400030728     C                   KFLD                    XTADDA
050500030728      *
050600030728     C     Kev           KLIST
050700030729     C                   KFLD                    D1NTW
050800040115     C                   KFLD                    D1COD15          15
050900940207     C*
051000940223     C* Variabili per gestione videate
051100940223     C*
051200940223     C                   MOVE      *BLANK        $GEST             2
051300940223     C                   MOVE      *BLANK        $FINE             1
051400940223     C                   MOVE      *BLANK        $INZD1            1
051500940223     C                   MOVE      *BLANK        $LASTG            2
051600940223     C                   MOVE      *BLANK        $LASTV            2
051700940223     C*
051800940223     C* Variabili appoggio
051900940223     C                   Z-ADD     0             CURR              2 0
052000940223     C                   Z-ADD     0             CURC              2 0
052100940223     C                   MOVE      *ZEROS        WIN              99
052200940224     C                   MOVE      *BLANK        WTBERR            1
052300940223     C*
052400940223     C* Indici
052500940223     C                   Z-ADD     0             X                 3 0
052600940223     C                   Z-ADD     0             Y                 3 0
052700940127     C*
052800940117     C                   ENDSR
052900940128     C************************************************************
