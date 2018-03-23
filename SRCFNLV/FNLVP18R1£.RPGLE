000100141127      *        tab CTL.Fatt.PTNR
000200141127      *
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
002500170705$001 FFNLVP18D1 CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700030729     FTneva02L  UF A E           K DISK
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
003900030729     D  XTACOD                 5      6
004000030729     D  XTANTW                 7      9
004100030729     D  XTAFIL                10     12  0
004200030729     D  XTADDA                13     20  0
004300030729     D  XTAADA                21     28  0
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
020100940207     C*
020200940224     C* valorizzo la chiave univoca per reperire i dati
020300940224     C* se non immissione
0204000307291   >C     xtaopz        IFNE      '1'
020500030204      *
020600940315    >C*-------------------------------------------------------------
020700940315    >C* impostare i campi chiave ricevuti
020800940315    >C*-------------------------------------------------------------
020900030729     C     Keva          chain(N)  TNeva02L
021000940614     C*
021100030729     C                   IF        %Found(TNeva02L)
021200030729     c                   eval        d1cod =  evaVAR
021300030729     c                   eval        d1fil =  evafil
021400030729     c                   eval        d1ntw =  evantw
021500030729     c                   eval        h1dda =  evadda
021600030729     c                   eval        h1ADA =  evaada
021700030729     c                   movel     evadda        dataiso
021800030728     c                   movel     dataiso       dataeur
021900030729     c                   movel     dataeur       d1dda
022000030729     c                   movel     evaada        dataiso
022100030728     c                   movel     dataiso       dataeur
022200030729     c                   movel     dataeur       d1ADA
022300030729     c                   movel     evades        d1des
022400030729     c                   movel     evagrp        d1grp
022500030729     c                   if        evaANN = 'A'  and xtaOPZ <>'3'
022600030115     c                   seton                                        08
022700030114     C                   MOVEl     Annull        D1Ann
022800030114     C                   end
022900030113      *
023000030729     C* richiamo routine dei ctrl per decodificarla
023100940614    >C                   EXSR      CTRD1
023200030113      *
023300960305    >C     *IN99         IFEQ      *ON
023400960305    >C                   MOVEL     *IN99         SAV99             1
023500960305    >C                   ENDIF
023600030113      *
023700940224     C                   MOVE      *ALL'0'       IN5098
023800940224     C                   MOVEA     IN5098        *IN(50)
023900030113      *
024000940224     C* definizione indicatori su video
024100940224     C                   SETOFF                                       02
0242009402243    C                   SELECT
024300030729    >C     xtaopz        WHENEQ    '2'
024400940224     C                   SETON                                        02
024500030729    >C     xtaopz        WHENEQ    '5'
0246009402243-   C                   ENDSL
024700940224     C*
0248009402242-   C                   ENDIF
024900030114     C*
0250000301141   >C                   Else
025100030114     C* Se in Immissione nuovo record
025200030728     c                   clear                   d1
025300940223     C*
0254009402241-   C                   ENDIF
025500940224     C*
025600940117     C                   ENDSR
025700940309     C************************************************************
025800940309     C* GESTIONE F03 VIDEO D1
025900940309     C************************************************************
026000940309     C     F03D1         BEGSR
026100940309     C*
026200940309     C                   MOVE      *ON           $FINE
026300030113    >C                   MOVE      '1'           xtaret
026400940325     C*
026500940325     C                   ENDSR
026600940309     C************************************************************
026700940309     C* GESTIONE F12 VIDEO D1
026800940309     C************************************************************
026900940309     C     F12D1         BEGSR
027000940309     C*
027100940309     C                   MOVE      *ON           $FINE
027200940309     C*
027300940309     C                   ENDSR
027400940131     C************************************************************
027500940207     C* CONTROLLO VIDEATA
027600940131     C************************************************************
027700940127     C     CTRD1         BEGSR
027800940131     C*
027900940127     C                   SETOFF                                       99
028000030123      *
028100940127     C* non imposto pos. del cursore
028200940127     C                   Z-ADD     0             H1RIGA
028300940127     C                   Z-ADD     0             H1COLO
028400940621     C*
028500940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0286000307291   >C     xtaopz        IFNE      '4'
028700030728     c* data decorrenza
028800030729     c                   if        d1dda  <> 0
028900030729     c     *eur          test(d)                 d1dda                  50
029000030728     c                   if        *in50
029100030729     c     *dmy          test(d)                 d1dda                  50
029200030729     c  n50*dmy          move      d1dda         dataeur
029300030729     c  n50              move      dataeur       d1dda
029400030728     c  n50              move      dataeur       dataiso
029500030729     c  n50              move      dataiso       h1dda
029600030728     c                   else
029700030729     c                   move      d1dda         dataeur
029800030729     c                   move      dataeur       d1dda
029900030728     c                   move      dataeur       dataiso
030000030729     c                   move      dataiso       h1dda
030100030728     c                   end
030200030728     c                   else
030300030728     c                   seton                                        50
030400030728     c                   end
030500030728     c   50              seton                                        99
030600030728     c* data scadenza
030700030729     c                   if        d1ADA  <> 0
030800030729     c     *eur          test(d)                 d1ADA                  51
030900030728     c                   if        *in51
031000030729     c     *dmy          test(d)                 d1ADA                  51
031100030729     c  n51*dmy          move      d1ADA         dataeur
031200030729     c  n51              move      dataeur       d1ADA
031300030728     c  n51              move      dataeur       dataiso
031400030729     c  n51              move      dataiso       h1ADA
031500030728     c                   else
031600030729     c                   move      d1ADA         dataeur
031700030729     c                   move      dataeur       d1ADA
031800030728     c                   move      dataeur       dataiso
031900030729     c                   move      dataiso       h1ADA
032000030728     c                   end
032100030728     c                   else
032200030728     c                   seton                                          51
032300030728     c                   end
032400030728     c   51              seton                                        99
032500030728     c*
032600030728     c*  sequenza date
032700030729     c                   if        h1ADA < h1dda and
032800030728     c                             *in51 = *Off and *in52 = *Off
032900030728     c                   seton                                        5299
033000030728     c                   end
033100030728     c*
033200030728     c* gruppo appartenenza
033300040607     c                   if        d1grp =  *blank
033400030728     c                   seton                                        5399
033500030728     c                   end
033600030728     c*
033700030728     c* Codice Voce vuoto
033800030729     c                   if        d1cod =  *blank
033900030728     c                   seton                                        5499
034000030728     c                   end
034100030728     c*
034200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
034300030729    >C     xtaopz        IFEQ      '1'
034400030729    >C     xtaopz        oreq      '3'
034500030728     C*
034600030728     c* controllo di non sovrapposizione periodi
034700030729     C     Kev           setll     tneva02l
034800030729     C     Kev           reade     tneva02l
034900030729     c                   dow       not %eof(tneva02l)
035000030729     c                   if        evadda <= h1dda and
035100030729     c                             evaada >= h1dda or
035200030729     c                             evadda <= h1ADA and
035300030729     c                             evaada >= h1ADA
035400030728     c                   seton                                        5599
035500030728     c                   leave
035600030728     c                   end
035700030729     C     Kev           reade     tneva02l
035800030728     c                   end
035900030114     C                   ENDIF
036000030114     C*
036100030114     C* controllo codice vuoto
036200030729$017 C     D1cod         IFEQ      *blank
036300030728     C                   SETON                                        5499
036400030114     C                   End
036500030114      *
0366009402241-   C                   END
036700940131     C*
036800940302     C     *IN99         IFEQ      *ON
036900940510     C     $GEST         ANDEQ     'D1'
037000940117     C                   MOVEA     *IN           WIN
037100940128     C                   MOVE      *ALL'0'       IN5098           49
037200940128     C                   MOVEA     IN5098        *IN(50)
037300940614     C                   WRITE     D1
037400940117     C                   MOVEA     WIN           *IN
037500940302     C                   ENDIF
037600940127     C*
037700940117     C                   ENDSR
037800940207     C/EJECT
037900940224     C************************************************************
038000940315    >C* AGGIORNAMENTO ANAGRAFICA
038100940224     C************************************************************
038200940315    >C     AGGANA        BEGSR
038300940207     C*
038400940207     C                   SELECT
038500030204     C*
038600030729    >C     xtaopz        WHENEQ    '1'
038700030729    >C     xtaopz        OREQ      '3'
038800030204     C*
038900030728    >C                   EXSR      RIEANA
039000030729     c                   write     Tneva000                             59
039100940318     C* se rcd non scrivibile attivo errore
039200030114    >C   59              SETON                                          99
039300030204      *
039400030729    >C     xtaopz        WHENEQ    '2'
039500030204      *
039600030729     C     Keva          chain     Tneva02L
039700030728    >C                   EXSR      RIEANA
039800030729     c                   update    Tneva000                             59
039900030114    >C   59              SETON                                          99
040000030204      *
040100030729    >C     xtaopz        WHENEQ    '4'
040200030729     C     Keva          chain     Tneva02L
040300030729     c                   move      'A'           evaANN
040400030115     c                   if        $tasto = F23
040500030729     c                   delete    Tneva000                             59
040600030115     c                   else
040700030729     c                   update    Tneva000                             59
040800030115     c                   end
040900030114    >C   59              SETON                                          99
041000030204      *
041100940207     C                   ENDSL
041200940203     C*
041300940203     C                   ENDSR
041400940224     C************************************************************
041500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
041600940224     C************************************************************
041700030728     C     RIEANA        BEGSR
041800030114     C*
041900030729     c                   movel     d1DES         evaDES
042000030729     c                   movel     d1cod         evaVAR
042100030729     c                   movel     d1GRP         evaGRP
042200030729     c                   movel     d1NTW         evaNTW
042300030729     c                   movel     d1FIL         evaFIL
042400030728     c                   movel     d1DDA         dataeur
042500030728     c                   movel     dataeur       dataiso
042600030729     c                   movel     dataiso       evaDDA
042700030728     c                   movel     d1ADA         dataeur
042800030728     c                   movel     dataeur       dataiso
042900030729     c                   movel     dataiso       evaADA
043000030729     c                   movel     ' '           evaANN
043100030114     C*
043200940207     C                   ENDSR
043300940224     C************************************************************
043400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
043500940224     C************************************************************
043600940224     C     GESAGG        BEGSR
043700940224     C*
043800940224     C* segnalo al pgm chiamante l'avvenuta conferma
043900030114    >C                   MOVE      *ON           xtaopr
044000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
044100030114    >C                   MOVE      '0'           xtaret
044200940224     C*
0443009402241    C                   SELECT
044400940224     C* nel caso di immissione, ripristino la videata iniziale
044500030729    >C     xtaopz        WHENEQ    '1'
044600940224     C                   MOVE      *ON           $INZD1
044700030114     C                   MOVE      *ON           $FINE
044800940224     C* altrimenti ritorno al pgm chiamante
044900940224     C                   OTHER
045000940224     C                   MOVE      *ON           $FINE
0451009402241-   C                   ENDSL
045200940224     C*
045300940224     C                   ENDSR
045400940131     C************************************************************
045500940131     C* OPERAZIONI INIZIALI
045600940131     C************************************************************
045700940131     C     *INZSR        BEGSR
045800940131     C*
045900940127     C* Reperimento parametri
046000940127     C*
046100940117     C     *ENTRY        PLIST
046200940117     C                   PARM                    KPJBA
046300030113     C                   movel     kpjbu         tabds
046400030115     c                   setoff                                       08
046500030115     C*
046600940223     C* Variabili per gestione videate
046700940223     C                   MOVE      'D1'          $GEST
046800940223     C                   MOVE      *OFF          $FINE
046900940223     C                   MOVE      *ON           $INZD1
047000940224     C                   MOVE      *BLANK        $LASTG
047100940224     C                   MOVE      *BLANK        $LASTV
047200940506     C                   Z-ADD     0             $ULKD1            3 0
047300940127     C*
047400940223     C* Variabili appoggio
047500940224     C                   Z-ADD     0             CURR
047600940224     C                   Z-ADD     0             CURC
047700940207     C*
047800940207     C* Valorizzazione campi univoci testate
047900940614     C                   CLEAR                   T1
048000940614     C                   MOVEL     KNSIF         NOMSIF
048100941027     C                   MOVEL     DSPGM         NOMPGM
048200030113     C*
048300030729     C                   movel     XTACOD        evaVAR
048400030729     C                   movel     XTANTW        evaNTW
048500030729     C                   z-add     XTAFIL        evaFIL
048600030729     c                   z-add     XTADDA        evaDDA
048700940224     C*
048800940224     C* Inizializzazione/Decodifica prima videata
048900940224     C                   EXSR      INZD1
049000940224     C                   MOVE      *OFF          $INZD1
049100940127     C*
049200940117     C                   ENDSR
049300940128     C************************************************************
049400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
049500030729     c************************************************************
049600940223     C     DEFCAM        BEGSR
049700940223     C*
049800940223     C* klist
049900030729     C     Keva          KLIST
050000030728     C                   KFLD                    XTACOD
050100030728     C                   KFLD                    XTANTW
050200030728     C                   KFLD                    XTAFIL
050300030728     C                   KFLD                    XTADDA
050400030728      *
050500030728     C     Kev           KLIST
050600030729     C                   KFLD                    D1COD
050700030729     C                   KFLD                    D1NTW
050800030729     C                   KFLD                    D1FIL
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
