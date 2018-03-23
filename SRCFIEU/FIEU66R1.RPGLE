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
002500150428$001 FFIEU66D1  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700150428     FTnCLD00F  UF A E           K DISK
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
003900150428     D  XTACOD                 5     14
004000150428     D  XTADEP                15     18  0
004100150428     D  XTADDA                19     26  0
004200150428     D  XTAADA                27     34  0
004300940201     D*-------------
004400030204     D  S_TASTO        S                   like($Tasto)
004500940201     D DSFMT           DS
004600940506     D  $TASTO               369    369
004700940201     D  NRG                  370    370
004800940201     D  NCL                  371    371
004900940201     D*-------------
005000940201     D* posizione cursore
005100940201     D CURSOR          DS
005200940223     D  RIRI                   1      2B 0 INZ
005300940201     D  R$$                    2      2
005400940223     D  COCO                   3      4B 0 INZ
005500940201     D  C$$                    4      4
005600030728      *
005700030728     Ddataiso          s               d   datfmt(*iso)
005800030728     Ddataeur          s               d   datfmt(*eur)
005900940207     D*-------------
006000940207     D* Reperimento nome PGM
006100940207     D STATUS         SDS           333
006200940207     D  DSPGM            *PROC
006300940225     D*-------------
006400940225     D* COSTANTI
006500940225     D*-------------
006600940506     D* Tasti di funzione
006700940506     D F01             C                   CONST(X'31')
006800940506     D F02             C                   CONST(X'32')
006900940506     D F03             C                   CONST(X'33')
007000940506     D F04             C                   CONST(X'34')
007100940506     D F05             C                   CONST(X'35')
007200940506     D F06             C                   CONST(X'36')
007300940506     D F07             C                   CONST(X'37')
007400940506     D F08             C                   CONST(X'38')
007500940506     D F09             C                   CONST(X'39')
007600940506     D F10             C                   CONST(X'3A')
007700940506     D F11             C                   CONST(X'3B')
007800940506     D F12             C                   CONST(X'3C')
007900940506     D F13             C                   CONST(X'B1')
008000940506     D F14             C                   CONST(X'B2')
008100940506     D F15             C                   CONST(X'B3')
008200940506     D F16             C                   CONST(X'B4')
008300940506     D F17             C                   CONST(X'B5')
008400940506     D F18             C                   CONST(X'B6')
008500940506     D F19             C                   CONST(X'B7')
008600940506     D F20             C                   CONST(X'B8')
008700940506     D F21             C                   CONST(X'B9')
008800940506     D F22             C                   CONST(X'BA')
008900940506     D F23             C                   CONST(X'BB')
009000940506     D F24             C                   CONST(X'BC')
009100940506     D ENTER           C                   CONST(X'F1')
009200940506     D ROLDWN          C                   CONST(X'F4')
009300940506     D ROLLUP          C                   CONST(X'F5')
009400940506     D*-------------
009500941108     D DATA            C                   CONST('0001-01-01')
009600030114     D Annull          C                   CONST('  ANNULLATO  ')
009700030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009800030114     D OPz_Var         C                   CONST('VARIAZIONE')
009900030114     D OPz_Del         C                   CONST(' ANNULLA  ')
010000030114     D OPz_Vis         C                   CONST('VISUALIZZA')
010100940127     C*----------------------------------------------------*
010200940127     C*                MAIN LINE PROGRAM
010300940127     C*----------------------------------------------------*
010400940223     C*
010500940223     C     $FINE         DOWEQ     *OFF
010600940202     C     $GEST         CASEQ     'D1'          GESD1
010700940117     C                   END
010800940117     C                   END
010900940325     C* fine programma
011000030113     c                   movel     tabds         kpjbu
011100030113     C                   Seton                                        LR
011200940131     C************************************************************
011300940510     C* GESTIONE VIDEO RECORD D1
011400940131     C************************************************************
011500940127     C     GESD1         BEGSR
011600030113      *
011700940225     C* inizializzazione videata
011800940223     C     $INZD1        IFEQ      *ON
011900940127     C                   EXSR      INZD1
012000940223     C                   MOVE      *OFF          $INZD1
012100940117     C                   END
012200030113      *
012300940225     C* emissione piede videata se proveniente da altra
012400940225     C* salvataggio valore formato di provenienza
012500940207     C     $LASTV        IFNE      'D1'
012600940614     C                   WRITE     T1
012700940202     C                   MOVE      $LASTV        $LASTG
012800940202     C                   MOVE      'D1'          $LASTV
012900940202     C                   END
013000940131     C*
013100940203     C                   SELECT
013200940203     C* Annullamento
013300030114     C* Visualizzazione
013400030729    >C     xtaopz        WHENEQ    '1'
013500030114     c                   movel     Opz_Imm       D1Fun
013600030729    >C     xtaopz        WHENEQ    '5'
013700030114     c                   movel     Opz_Vis       D1Fun
013800030114     C                   SETON                                        020306
013900030729    >C     xtaopz        WHENEQ    '4'
014000030114     c                   movel     Opz_Del       D1Fun
014100030114     C                   SETON                                        0203
014200030114     C                   SETOFF                                         06
014300940620     C* Manutenzione
014400030729    >C     xtaopz        WHENEQ    '2'
014500030114     c                   movel     Opz_Var       D1Fun
014600030114     C                   SETON                                        02
014700030114     C                   SETOFF                                       0306
014800940620     C*
014900940620     C                   OTHER
015000940620     C*
015100030114     C                   SETOFF                                       020306
015200940203     C                   ENDSL
015300030113     C*
015400030728     c     video1        tag
015500030114     c                   SETOFF                                         99
015600940620     C* Immissione/Modifica/Copia
015700030113     C*               *----------------*
015800940620     C                   EXFMT     D1
015900030113     C*               *----------------*
016000030114     C*
016100030114     C     *IN99         IFEQ      *OFF
016200030114     C*
0163000301141    C                   SELECT
016400030114     C* F3=Fine
016500030114     C     $TASTO        WHENEQ    F03
016600030114     C                   EXSR      F03D1
016700030114     C* F12=Ritorno
016800030114     C     $TASTO        WHENEQ    F12
016900030114     C                   EXSR      F12D1
017000030114     C*
0171000301141O   C                   OTHER
017200030114     C*
017300030114     C                   EXSR      CTRD1
017400030728     c   99              goto      video1
017500030113     C*
017600940616     C     *IN99         IFEQ      *OFF
017700940616     C* F6
0178000302052    C     $TASTO        ifeq      F06
0179000301152    C     $TASTO        oreq      F23
018000940224     C* eseguo aggiornamento
018100940315    >C                   EXSR      AGGANA
018200940224     C* eseguo operazioni del dopo-aggiornamento
018300030114     C  N99              EXSR      GESAGG
018400030205     c                   end
018500030114     C*
018600030114     C                   ENDIF
018700030114     C*
0188000301142-   C                   ENDSL
018900030114     C*
0190000301142-   C                   ENDIF
019100940224     C*
019200940117     C                   ENDSR
019300940117      ************************************************************
019400940207      * INIZIALIZZAZIONE VIDEATA DATI
019500940117      ************************************************************
019600940127     C     INZD1         BEGSR
019700940131     C*
019800940615     C                   CLEAR                   D1
019900030729     C                   MOVEl     xtaCOD        D1cod
020000940207     C*
020100940224     C* valorizzo la chiave univoca per reperire i dati
020200940224     C* se non immissione
0203000307291   >C     xtaopz        IFNE      '1'
020400030204      *
020500940315    >C*-------------------------------------------------------------
020600940315    >C* impostare i campi chiave ricevuti
020700940315    >C*-------------------------------------------------------------
020800150428     C     KCLD          chain(N)  TNCLD00F
020900940614     C*
021000150428     C                   IF        %Found(TNCLD00F)
021100150428     c                   eval        d1cod =  cldCOD
021200150428     c                   eval        d1dep =  cldDEP
021300150428     c                   eval        h1dda =  clddda
021400150428     c                   eval        h1ADA =  cldada
021500150428     c                   movel     clddda        dataiso
021600030728     c                   movel     dataiso       dataeur
021700030729     c                   movel     dataeur       d1dda
021800150428     c                   movel     cldada        dataiso
021900030728     c                   movel     dataiso       dataeur
022000030729     c                   movel     dataeur       d1ADA
022100150428     c                   movel     clddes        d1des
022200150428     c                   if        cldANN = 'A'  and xtaOPZ <>'3'
022300030115     c                   seton                                        08
022400030114     C                   MOVEl     Annull        D1Ann
022500030114     C                   end
022600030113      *
022700030729     C* richiamo routine dei ctrl per decodificarla
022800940614    >C                   EXSR      CTRD1
022900030113      *
023000960305    >C     *IN99         IFEQ      *ON
023100960305    >C                   MOVEL     *IN99         SAV99             1
023200960305    >C                   ENDIF
023300030113      *
023400940224     C                   MOVE      *ALL'0'       IN5098
023500940224     C                   MOVEA     IN5098        *IN(50)
023600030113      *
023700940224     C* definizione indicatori su video
023800940224     C                   SETOFF                                       02
0239009402243    C                   SELECT
024000030729    >C     xtaopz        WHENEQ    '2'
024100940224     C                   SETON                                        02
024200030729    >C     xtaopz        WHENEQ    '5'
0243009402243-   C                   ENDSL
024400940224     C*
0245009402242-   C                   ENDIF
024600030114     C*
0247000301141   >C                   Else
024800030114     C* Se in Immissione nuovo record
024900030728     c                   clear                   d1
025000940223     C*
0251009402241-   C                   ENDIF
025200940224     C*
025300940117     C                   ENDSR
025400940309     C************************************************************
025500940309     C* GESTIONE F03 VIDEO D1
025600940309     C************************************************************
025700940309     C     F03D1         BEGSR
025800940309     C*
025900940309     C                   MOVE      *ON           $FINE
026000030113    >C                   MOVE      '1'           xtaret
026100940325     C*
026200940325     C                   ENDSR
026300940309     C************************************************************
026400940309     C* GESTIONE F12 VIDEO D1
026500940309     C************************************************************
026600940309     C     F12D1         BEGSR
026700940309     C*
026800940309     C                   MOVE      *ON           $FINE
026900940309     C*
027000940309     C                   ENDSR
027100940131     C************************************************************
027200940207     C* CONTROLLO VIDEATA
027300940131     C************************************************************
027400940127     C     CTRD1         BEGSR
027500940131     C*
027600940127     C                   SETOFF                                       99
027700030123      *
027800940127     C* non imposto pos. del cursore
027900940127     C                   Z-ADD     0             H1RIGA
028000940127     C                   Z-ADD     0             H1COLO
028100940621     C*
028200940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0283000307291   >C     xtaopz        IFNE      '4'
028400030728     c* data decorrenza
028500030729     c                   if        d1dda  <> 0
028600030729     c     *eur          test(d)                 d1dda                  50
028700030728     c                   if        *in50
028800030729     c     *dmy          test(d)                 d1dda                  50
028900030729     c  n50*dmy          move      d1dda         dataeur
029000030729     c  n50              move      dataeur       d1dda
029100030728     c  n50              move      dataeur       dataiso
029200030729     c  n50              move      dataiso       h1dda
029300030728     c                   else
029400030729     c                   move      d1dda         dataeur
029500030729     c                   move      dataeur       d1dda
029600030728     c                   move      dataeur       dataiso
029700030729     c                   move      dataiso       h1dda
029800030728     c                   end
029900030728     c                   else
030000030728     c                   seton                                        50
030100030728     c                   end
030200030728     c   50              seton                                        99
030300030728     c* data scadenza
030400030729     c                   if        d1ADA  <> 0
030500030729     c     *eur          test(d)                 d1ADA                  51
030600030728     c                   if        *in51
030700030729     c     *dmy          test(d)                 d1ADA                  51
030800030729     c  n51*dmy          move      d1ADA         dataeur
030900030729     c  n51              move      dataeur       d1ADA
031000030728     c  n51              move      dataeur       dataiso
031100030729     c  n51              move      dataiso       h1ADA
031200030728     c                   else
031300030729     c                   move      d1ADA         dataeur
031400030729     c                   move      dataeur       d1ADA
031500030728     c                   move      dataeur       dataiso
031600030729     c                   move      dataiso       h1ADA
031700030728     c                   end
031800030728     c                   else
031900030728     c                   seton                                          51
032000030728     c                   end
032100030728     c   51              seton                                        99
032200030728     c*
032300030728     c*  sequenza date
032400030729     c                   if        h1ADA < h1dda and
032500030728     c                             *in51 = *Off and *in52 = *Off
032600030728     c                   seton                                        5299
032700030728     c                   end
032800030728     c*
032900150428     c* Codice cliente vuoto (CHIAVE principale)
033000150428     c                   if        d1cod =  *blank
033100030728     c                   seton                                        5499
033200030728     c                   end
033300150428     c*
033400150428     c* Codice Depot   vuoto (CHIAVE principale)
033500150428     c                   if        d1cod =  *zeros
033600150428     c                   seton                                        5699
033700150428     c                   end
033800030728     c*
033900940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
034000030729    >C     xtaopz        IFEQ      '1'
034100030729    >C     xtaopz        oreq      '3'
034200030728     C*
034300030728     c* controllo di non sovrapposizione periodi
034400150428     C     Kcl           setll     tncld00f
034500150428     C     Kcl           reade     tncld00f
034600150428     c                   dow       not %eof(tncld00f)
034700150428     c                   if        clddda <= h1dda and
034800150428     c                             cldada >= h1dda or
034900150428     c                             clddda <= h1ADA and
035000150428     c                             cldada >= h1ADA
035100030728     c                   seton                                        5599
035200030728     c                   leave
035300030728     c                   end
035400150428     C     Kcl           reade     tncld00f
035500030728     c                   end
035600030114     C                   ENDIF
035700030114     C*
035800030114     C* controllo codice vuoto
035900150428$017 C     D1cod         IFEQ      *blank
036000030728     C                   SETON                                        5499
036100030114     C                   End
036200030114      *
036300150428$017 C     D1dep         IFEQ      *zeros
036400150428     C                   SETON                                        5699
036500150428     C                   End
036600150428      *
0367009402241-   C                   END
036800940131     C*
036900940302     C     *IN99         IFEQ      *ON
037000940510     C     $GEST         ANDEQ     'D1'
037100940117     C                   MOVEA     *IN           WIN
037200940128     C                   MOVE      *ALL'0'       IN5098           49
037300940128     C                   MOVEA     IN5098        *IN(50)
037400940614     C                   WRITE     D1
037500940117     C                   MOVEA     WIN           *IN
037600940302     C                   ENDIF
037700940127     C*
037800940117     C                   ENDSR
037900940207     C/EJECT
038000940224     C************************************************************
038100940315    >C* AGGIORNAMENTO ANAGRAFICA
038200940224     C************************************************************
038300940315    >C     AGGANA        BEGSR
038400940207     C*
038500940207     C                   SELECT
038600030204     C*
038700030729    >C     xtaopz        WHENEQ    '1'
038800030729    >C     xtaopz        OREQ      '3'
038900030204     C*
039000030728    >C                   EXSR      RIEANA
039100150428     c                   write     Tncld000                             59
039200940318     C* se rcd non scrivibile attivo errore
039300030114    >C   59              SETON                                          99
039400030204      *
039500030729    >C     xtaopz        WHENEQ    '2'
039600030204      *
039700150428     C     KCLD          chain     Tncld00f
039800030728    >C                   EXSR      RIEANA
039900150428     c                   update    Tncld000                             59
040000030114    >C   59              SETON                                          99
040100030204      *
040200030729    >C     xtaopz        WHENEQ    '4'
040300150428     C     KCLD          chain     Tncld00f
040400150428     c                   move      'A'           cldANN
040500030115     c                   if        $tasto = F23
040600150428     c                   delete    Tncld000                             59
040700030115     c                   else
040800150428     c                   update    Tncld000                             59
040900030115     c                   end
041000030114    >C   59              SETON                                          99
041100030204      *
041200940207     C                   ENDSL
041300940203     C*
041400940203     C                   ENDSR
041500940224     C************************************************************
041600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
041700940224     C************************************************************
041800030728     C     RIEANA        BEGSR
041900030114     C*
042000150428     c                   movel     d1DES         cldDES
042100150428     c                   movel     d1cod         cldCOD
042200150428     c                   movel     d1DEP         cldDEP
042300030728     c                   movel     d1DDA         dataeur
042400030728     c                   movel     dataeur       dataiso
042500150428     c                   movel     dataiso       cldDDA
042600030728     c                   movel     d1ADA         dataeur
042700030728     c                   movel     dataeur       dataiso
042800150428     c                   movel     dataiso       cldADA
042900150428     c                   movel     ' '           cldANN
043000030114     C*
043100940207     C                   ENDSR
043200940224     C************************************************************
043300940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
043400940224     C************************************************************
043500940224     C     GESAGG        BEGSR
043600940224     C*
043700940224     C* segnalo al pgm chiamante l'avvenuta conferma
043800030114    >C                   MOVE      *ON           xtaopr
043900940224     C* segnalo al pgm chiamante l'aver premuto l'F6
044000030114    >C                   MOVE      '0'           xtaret
044100940224     C*
0442009402241    C                   SELECT
044300940224     C* nel caso di immissione, ripristino la videata iniziale
044400030729    >C     xtaopz        WHENEQ    '1'
044500940224     C                   MOVE      *ON           $INZD1
044600030114     C                   MOVE      *ON           $FINE
044700940224     C* altrimenti ritorno al pgm chiamante
044800940224     C                   OTHER
044900940224     C                   MOVE      *ON           $FINE
0450009402241-   C                   ENDSL
045100940224     C*
045200940224     C                   ENDSR
045300940131     C************************************************************
045400940131     C* OPERAZIONI INIZIALI
045500940131     C************************************************************
045600940131     C     *INZSR        BEGSR
045700940131     C*
045800940127     C* Reperimento parametri
045900940127     C*
046000940117     C     *ENTRY        PLIST
046100940117     C                   PARM                    KPJBA
046200030113     C                   movel     kpjbu         tabds
046300030115     c                   setoff                                       08
046400030115     C*
046500940223     C* Variabili per gestione videate
046600940223     C                   MOVE      'D1'          $GEST
046700940223     C                   MOVE      *OFF          $FINE
046800940223     C                   MOVE      *ON           $INZD1
046900940224     C                   MOVE      *BLANK        $LASTG
047000940224     C                   MOVE      *BLANK        $LASTV
047100940506     C                   Z-ADD     0             $ULKD1            3 0
047200940127     C*
047300940223     C* Variabili appoggio
047400940224     C                   Z-ADD     0             CURR
047500940224     C                   Z-ADD     0             CURC
047600940207     C*
047700940207     C* Valorizzazione campi univoci testate
047800940614     C                   CLEAR                   T1
047900940614     C                   MOVEL     KNSIF         NOMSIF
048000941027     C                   MOVEL     DSPGM         NOMPGM
048100030113     C*
048200150428     C                   movel     XTACOD        cldCOD
048300150428     C                   z-add     XTADEP        cldDEP
048400150428     c                   z-add     XTADDA        cldDDA
048500940224     C*
048600940224     C* Inizializzazione/Decodifica prima videata
048700940224     C                   EXSR      INZD1
048800940224     C                   MOVE      *OFF          $INZD1
048900940127     C*
049000940117     C                   ENDSR
049100940128     C************************************************************
049200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
049300940223     C************************************************************
049400940223     C     DEFCAM        BEGSR
049500940223     C*
049600150428
049700150428     C     Kcld          KLIST
049800030728     C                   KFLD                    XTACOD
049900150428     C                   KFLD                    XTADEP
050000030728      *
050100150428     C     Kcl           KLIST
050200030729     C                   KFLD                    D1COD
050300150428     C                   KFLD                    D1DEP
050400940207     C*
050500940223     C* Variabili per gestione videate
050600940223     C*
050700940223     C                   MOVE      *BLANK        $GEST             2
050800940223     C                   MOVE      *BLANK        $FINE             1
050900940223     C                   MOVE      *BLANK        $INZD1            1
051000940223     C                   MOVE      *BLANK        $LASTG            2
051100940223     C                   MOVE      *BLANK        $LASTV            2
051200940223     C*
051300940223     C* Variabili appoggio
051400940223     C                   Z-ADD     0             CURR              2 0
051500940223     C                   Z-ADD     0             CURC              2 0
051600940223     C                   MOVE      *ZEROS        WIN              99
051700940224     C                   MOVE      *BLANK        WTBERR            1
051800940223     C*
051900940223     C* Indici
052000940223     C                   Z-ADD     0             X                 3 0
052100940223     C                   Z-ADD     0             Y                 3 0
052200940127     C*
052300940117     C                   ENDSR
052400940128     C************************************************************
