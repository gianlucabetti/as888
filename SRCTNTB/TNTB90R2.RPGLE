000100970214     H DECEDIT('0,') DATEDIT(*DMY.)
000200940223      *
000300940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000400940620      *  03           PROTECT TUTTI I CAMPI
000500030123      *  09           PROTECT dei campi voce C/E
000600940223      *  21           GENERICO OPERAZIONI I/O
000700940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940223      *  30           SFLDSP
000900940223      * N31           SFLCLR
001000940128      *  31           SFLDSPCTL
001100940128      *  32           SFLNXTCHG
001200940128      *  33           SFLEND
001300940128      *  39           OF PRTF
001400940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940317    > *  Specificare l'uso dei singoli indicatori
001600940315      *  50 <---> 98  ERRORI SU VIDEO
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
001900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002100940128      *  99           INDIC. GENERALE DI ERRORE
002200940128     F*----------------------------------------------------*
002300110927$001 FTNTB90D2  CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500110927     Ftntbe01l  UF A E           K DISK
002600940128     D*----------------------------------------------------*
002700110926$003 D EXTDS         E DS                  EXTNAME(tntb90ds)
002800030113      *-------------
002900940127     D* Passaggio Parametri
003000940127     D KPJBA         E DS
003100030113      *-------------
003200030113     D* Parametri in ricezione
003300030113     D  TABDS          DS
003400030113     D  XTAOPZ                 1      2
003500030113     D  XTARET                 3      3
003600030113     D  XTAOPR                 4      4
003700030113     D  XTAERR                 5      5
003800110926     D  XTAKEY                 6      8
003900940201     D*-------------
004000030204     D  S_TASTO        S                   like($Tasto)
004100940201     D DSFMT           DS
004200940506     D  $TASTO               369    369
004300940201     D  NRG                  370    370
004400940201     D  NCL                  371    371
004500940201     D*-------------
004600940201     D* posizione cursore
004700940201     D CURSOR          DS
004800940223     D  RIRI                   1      2B 0 INZ
004900940201     D  R$$                    2      2
005000940223     D  COCO                   3      4B 0 INZ
005100940201     D  C$$                    4      4
005200940207     D*-------------
005300940207     D* Reperimento nome PGM
005400940207     D STATUS         SDS           333
005500940207     D  DSPGM            *PROC
005600940225     D*-------------
005700940225     D* COSTANTI
005800940225     D*-------------
005900940506     D* Tasti di funzione
006000940506     D F01             C                   CONST(X'31')
006100940506     D F02             C                   CONST(X'32')
006200940506     D F03             C                   CONST(X'33')
006300940506     D F04             C                   CONST(X'34')
006400940506     D F05             C                   CONST(X'35')
006500940506     D F06             C                   CONST(X'36')
006600940506     D F07             C                   CONST(X'37')
006700940506     D F08             C                   CONST(X'38')
006800940506     D F09             C                   CONST(X'39')
006900940506     D F10             C                   CONST(X'3A')
007000940506     D F11             C                   CONST(X'3B')
007100940506     D F12             C                   CONST(X'3C')
007200940506     D F13             C                   CONST(X'B1')
007300940506     D F14             C                   CONST(X'B2')
007400940506     D F15             C                   CONST(X'B3')
007500940506     D F16             C                   CONST(X'B4')
007600940506     D F17             C                   CONST(X'B5')
007700940506     D F18             C                   CONST(X'B6')
007800940506     D F19             C                   CONST(X'B7')
007900940506     D F20             C                   CONST(X'B8')
008000940506     D F21             C                   CONST(X'B9')
008100940506     D F22             C                   CONST(X'BA')
008200940506     D F23             C                   CONST(X'BB')
008300940506     D F24             C                   CONST(X'BC')
008400940506     D ENTER           C                   CONST(X'F1')
008500940506     D ROLDWN          C                   CONST(X'F4')
008600940506     D ROLLUP          C                   CONST(X'F5')
008700940506     D*-------------
008800941108     D DATA            C                   CONST('0001-01-01')
008900030114     D Annull          C                   CONST('  ANNULLATO  ')
009000030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009100030114     D OPz_Var         C                   CONST('VARIAZIONE')
009200030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009300030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009400940127     C*----------------------------------------------------*
009500940127     C*                MAIN LINE PROGRAM
009600940127     C*----------------------------------------------------*
009700940223     C*
009800940223     C     $FINE         DOWEQ     *OFF
009900940202     C     $GEST         CASEQ     'D1'          GESD1
010000940117     C                   END
010100940117     C                   END
010200940325     C* fine programma
010300030113     c                   movel     tabds         kpjbu
010400030113     C                   Seton                                        LR
010500940131     C************************************************************
010600940510     C* GESTIONE VIDEO RECORD D1
010700940131     C************************************************************
010800940127     C     GESD1         BEGSR
010900030113      *
011000940225     C* inizializzazione videata
011100940223     C     $INZD1        IFEQ      *ON
011200940127     C                   EXSR      INZD1
011300940223     C                   MOVE      *OFF          $INZD1
011400940117     C                   END
011500030113      *
011600940225     C* emissione piede videata se proveniente da altra
011700940225     C* salvataggio valore formato di provenienza
011800940207     C     $LASTV        IFNE      'D1'
011900940614     C                   WRITE     T1
012000940202     C                   MOVE      $LASTV        $LASTG
012100940202     C                   MOVE      'D1'          $LASTV
012200940202     C                   END
012300940131     C*
012400940203     C                   SELECT
012500940203     C* Annullamento
012600030114     C* Visualizzazione
012700030114    >C     xtaopz        WHENEQ    '01'
012800030114     c                   movel     Opz_Imm       D1Fun
012900030114    >C     xtaopz        WHENEQ    '05'
013000030114     c                   movel     Opz_Vis       D1Fun
013100030114     C                   SETON                                        020306
013200030114    >C     xtaopz        WHENEQ    '04'
013300030114     c                   movel     Opz_Del       D1Fun
013400110902     C                   SETON                                        02
013500030114     C                   SETOFF                                         06
013600940620     C* Manutenzione
013700030113    >C     xtaopz        WHENEQ    '02'
013800030114     c                   movel     Opz_Var       D1Fun
013900030114     C                   SETON                                        02
014000030114     C                   SETOFF                                       0306
014100940620     C*
014200940620     C                   OTHER
014300940620     C*
014400030114     C                   SETOFF                                       020306
014500940203     C                   ENDSL
014600030114     C*
014700030114     c                   SETOFF                                         99
014800940620     C* Immissione/Modifica/Copia
014900030113     C*               *----------------*
015000940620     C                   EXFMT     D1
015100030113     C*               *----------------*
015200030114     C*
015300030114     C     *IN99         IFEQ      *OFF
015400030114     C*
0155000301141    C                   SELECT
015600030114     C* F3=Fine
015700030114     C     $TASTO        WHENEQ    F03
015800030114     C                   EXSR      F03D1
015900030114     C* F12=Ritorno
016000030114     C     $TASTO        WHENEQ    F12
016100030114     C                   EXSR      F12D1
016200030114     C*
0163000301141O   C                   OTHER
016400030114     C*
016500030114     C                   EXSR      CTRD1
016600030113     C*
016700940616     C     *IN99         IFEQ      *OFF
016800940616     C* F6
0169000302052    C     $TASTO        ifeq      F06
0170000301152    C     $TASTO        oreq      F23
017100030204     C* A chi Trasmette la Tabella ?
017200110901     c****               Exsr      ACHI
017300940224     C* eseguo aggiornamento
017400940315    >C                   EXSR      AGGANA
017500940224     C* eseguo operazioni del dopo-aggiornamento
017600030114     C  N99              EXSR      GESAGG
017700030205     c                   end
017800030114     C*
017900030114     C                   ENDIF
018000030114     C*
0181000301142-   C                   ENDSL
018200030114     C*
0183000301142-   C                   ENDIF
018400030114     C*
018500940117     C                   ENDSR
018600940117      ************************************************************
018700940207      * INIZIALIZZAZIONE VIDEATA DATI
018800940117      ************************************************************
018900940127     C     INZD1         BEGSR
019000940131     C*
019100940615     C                   CLEAR                   D1
019200110927     C                   move      'WEK'         tbeCOD
019300030113     C                   MOVEl     xtakey        D1COD
019400110927     C                   clear                   tbeKE1
019500110927     C                   MOVEl     xtakey        tbeKE1
019600940207     C*
019700940224     C* valorizzo la chiave univoca per reperire i dati
019800940224     C* se non immissione
0199000301141   >C     xtaopz        IFNE      '01'
020000030204      *
020100940315    >C*-------------------------------------------------------------
020200940315    >C* impostare i campi chiave ricevuti
020300940315    >C*-------------------------------------------------------------
020400110927     C     KTBL          chain(N)  tntbe01L
020500940614     C*
020600110927     C                   IF        %Found(tntbe01L)
020700110927      *
020800110927     C                   MOVEL     tbeUNI        EXTDS
020900110927      *
021000110901     c*******            if        TblFLG = '*'
021100110901     c*******            seton                                        08
021200110901     C*******            MOVEl     Annull        D1Ann
021300110901     C*******            end
021400030113     C*
021500940224     C* riempio la videata
021600940224     C                   EXSR      RIED1
021700030113      *
021800940224     C* richiamo routine dei ctrl per decodificarla
021900940614    >C                   EXSR      CTRD1
022000030113      *
022100940224     C                   MOVE      *ALL'0'       IN5098
022200940224     C                   MOVEA     IN5098        *IN(50)
022300030113      *
022400940224     C* definizione indicatori su video
022500940224     C                   SETOFF                                       02
0226009402243    C                   SELECT
022700030114    >C     xtaopz        WHENEQ    '02'
022800940224     C                   SETON                                        02
022900030114    >C     xtaopz        WHENEQ    '05'
0230009402243-   C                   ENDSL
023100940224     C*
0232009402242-   C                   ENDIF
023300030114     C*
0234000301141   >C                   Else
023500030114     C* Se in Immissione nuovo record
023600030114     c                   exsr      NewD1
023700940223     C*
0238009402241-   C                   ENDIF
023900940224     C*
024000940117     C                   ENDSR
024100940207     C/EJECT
024200940207     C************************************************************
024300940510     C* RIEMPIMENTO VIDEATA  D1
024400940207     C************************************************************
024500030114     C     NEWD1         BEGSR
024600940207     C*
024700030114     c                   clear                   d1
024800030114     C*
024900030114     C                   ENDSR
025000030114     C************************************************************
025100030114     C* RIEMPIMENTO VIDEATA  D1
025200030114     C************************************************************
025300030114     C     RIED1         BEGSR
025400030114     C*
025500110927     C* campi delle 45 settimane possibili
025600110928     c                   if         B90WK01 <> *zeros
025700110927     c                   eval      d1001   =     B90WK01
025800110928     c                   end
025900110928     c                   if         B90WK02 <> *zeros
026000110927     c                   eval      d1002   =     B90WK02
026100110928     c                   end
026200110928     c                   if         B90WK03 <> *zeros
026300110927     c                   eval      d1003   =     B90WK03
026400110928     c                   end
026500110928     c                   if         B90WK04 <> *zeros
026600110927     c                   eval      d1004   =     B90WK04
026700110928     c                   end
026800110928     c                   if         B90WK05 <> *zeros
026900110927     c                   eval      d1005   =     B90WK05
027000110928     c                   end
027100110928     c                   if         B90WK06 <> *zeros
027200110927     c                   eval      d1006   =     B90WK06
027300110928     c                   end
027400110928     c                   if         B90WK07 <> *zeros
027500110927     c                   eval      d1007   =     B90WK07
027600110928     c                   end
027700110928     c                   if         B90WK08 <> *zeros
027800110927     c                   eval      d1008   =     B90WK08
027900110928     c                   end
028000110928     c                   if         B90WK09 <> *zeros
028100110927     c                   eval      d1009   =     B90WK09
028200110928     c                   end
028300110928     c                   if         B90WK10 <> *zeros
028400110927     c                   eval      d1010   =     B90WK10
028500110928     c                   end
028600110928     c                   if         B90WK11 <> *zeros
028700110928     c                   eval      d1011   =     B90WK11
028800110928     c                   end
028900110928     c                   if         B90WK12 <> *zeros
029000110928     c                   eval      d1012   =     B90WK12
029100110928     c                   end
029200110928     c                   if         B90WK13 <> *zeros
029300110928     c                   eval      d1013   =     B90WK13
029400110928     c                   end
029500110928     c                   if         B90WK14 <> *zeros
029600110928     c                   eval      d1014   =     B90WK14
029700110928     c                   end
029800110928     c                   if         B90WK15 <> *zeros
029900110928     c                   eval      d1015   =     B90WK15
030000110928     c                   end
030100110928     c                   if         B90WK16 <> *zeros
030200110928     c                   eval      d1016   =     B90WK16
030300110928     c                   end
030400110928     c                   if         B90WK17 <> *zeros
030500110928     c                   eval      d1017   =     B90WK17
030600110928     c                   end
030700110928     c                   if         B90WK18 <> *zeros
030800110928     c                   eval      d1018   =     B90WK18
030900110928     c                   end
031000110928     c                   if         B90WK19 <> *zeros
031100110928     c                   eval      d1019   =     B90WK19
031200110928     c                   end
031300110928     c                   if         B90WK20 <> *zeros
031400110927     c                   eval      d1020   =     B90WK20
031500110928     c                   end
031600110928     c                   if         B90WK21 <> *zeros
031700110928     c                   eval      d1021   =     B90WK21
031800110928     c                   end
031900110928     c                   if         B90WK22 <> *zeros
032000110928     c                   eval      d1022   =     B90WK22
032100110928     c                   end
032200110928     c                   if         B90WK23 <> *zeros
032300110928     c                   eval      d1023   =     B90WK23
032400110928     c                   end
032500110928     c                   if         B90WK24 <> *zeros
032600110928     c                   eval      d1024   =     B90WK24
032700110928     c                   end
032800110928     c                   if         B90WK25 <> *zeros
032900110928     c                   eval      d1025   =     B90WK25
033000110928     c                   end
033100110928     c                   if         B90WK26 <> *zeros
033200110928     c                   eval      d1026   =     B90WK26
033300110928     c                   end
033400110928     c                   if         B90WK27 <> *zeros
033500110928     c                   eval      d1027   =     B90WK27
033600110928     c                   end
033700110928     c                   if         B90WK28 <> *zeros
033800110928     c                   eval      d1028   =     B90WK28
033900110928     c                   end
034000110928     c                   if         B90WK29 <> *zeros
034100110928     c                   eval      d1029   =     B90WK29
034200110928     c                   end
034300110928     c                   if         B90WK30 <> *zeros
034400110927     c                   eval      d1030   =     B90WK30
034500110928     c                   end
034600110928     c                   if         B90WK31 <> *zeros
034700110928     c                   eval      d1031   =     B90WK31
034800110928     c                   end
034900110928     c                   if         B90WK32 <> *zeros
035000110928     c                   eval      d1032   =     B90WK32
035100110928     c                   end
035200110928     c                   if         B90WK33 <> *zeros
035300110928     c                   eval      d1033   =     B90WK33
035400110928     c                   end
035500110928     c                   if         B90WK34 <> *zeros
035600110928     c                   eval      d1034   =     B90WK34
035700110928     c                   end
035800110928     c                   if         B90WK35 <> *zeros
035900110928     c                   eval      d1035   =     B90WK35
036000110928     c                   end
036100110928     c                   if         B90WK36 <> *zeros
036200110928     c                   eval      d1036   =     B90WK36
036300110928     c                   end
036400110928     c                   if         B90WK37 <> *zeros
036500110928     c                   eval      d1037   =     B90WK37
036600110928     c                   end
036700110928     c                   if         B90WK38 <> *zeros
036800110928     c                   eval      d1038   =     B90WK38
036900110928     c                   end
037000110928     c                   if         B90WK39 <> *zeros
037100110928     c                   eval      d1039   =     B90WK39
037200110928     c                   end
037300110928     c                   if         B90WK40 <> *zeros
037400110927     c                   eval      d1040   =     B90WK40
037500110928     c                   end
037600110928     c                   if         B90WK41 <> *zeros
037700110927     c                   eval      d1041   =     B90WK41
037800110928     c                   end
037900110928     c                   if         B90WK42 <> *zeros
038000110927     c                   eval      d1042   =     B90WK42
038100110928     c                   end
038200110928     c                   if         B90WK43 <> *zeros
038300110927     c                   eval      d1043   =     B90WK43
038400110928     c                   end
038500110928     c                   if         B90WK44 <> *zeros
038600110927     c                   eval      d1044   =     B90WK44
038700110928     c                   end
038800110928     c                   if         B90WK45 <> *zeros
038900110927     c                   eval      d1045   =     B90WK45
039000110928     c                   end
039100030204     C*
039200940207     C                   ENDSR
039300940309     C************************************************************
039400940309     C* GESTIONE F03 VIDEO D1
039500940309     C************************************************************
039600940309     C     F03D1         BEGSR
039700940309     C*
039800940309     C                   MOVE      *ON           $FINE
039900030113    >C                   MOVE      '1'           xtaret
040000940325     C*
040100940325     C                   ENDSR
040200940309     C************************************************************
040300940309     C* GESTIONE F12 VIDEO D1
040400940309     C************************************************************
040500940309     C     F12D1         BEGSR
040600940309     C*
040700940309     C                   MOVE      *ON           $FINE
040800940309     C*
040900940309     C                   ENDSR
041000940131     C************************************************************
041100940207     C* CONTROLLO VIDEATA
041200940131     C************************************************************
041300940127     C     CTRD1         BEGSR
041400940131     C*
041500940127     C                   SETOFF                                       99
041600030123      *
041700940127     C* non imposto pos. del cursore
041800940127     C                   Z-ADD     0             H1RIGA
041900940127     C                   Z-ADD     0             H1COLO
042000940621     C*
042100940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0422000301141   >C     xtaopz        IFNE      '04'
042300971110     C*
042400971110     C* controllo codice vuoto
042500971110$017 C     D1COD         IFEQ      *ALL' '
042600971110     C                   SETON                                        5099
042700971110     C                   ENDIF
042800110908     C*
042900940207     C*
043000940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
043100030114    >C     xtaopz        IFEQ      '01'
043200030114    >C     xtaopz        oreq      '03'
043300110927     c                   clear                   tbeKE1
043400110927     c                   movel     d1cod         tbeKE1
043500110927     C     KTBL          chain(N)  tntbe01L
043600110927     c                   if        %Found(tntbe01L)
043700110908     C                   SETON                                        5399
043800030114     C                   EndIF
043900030114     C                   ENDIF
044000030114      *
0441009402241-   C                   END
044200940131     C*
044300940302     C     *IN99         IFEQ      *ON
044400940510     C     $GEST         ANDEQ     'D1'
044500940117     C                   MOVEA     *IN           WIN
044600940128     C                   MOVE      *ALL'0'       IN5098           49
044700940128     C                   MOVEA     IN5098        *IN(50)
044800940614     C                   WRITE     D1
044900940117     C                   MOVEA     WIN           *IN
045000940302     C                   ENDIF
045100940127     C*
045200940117     C                   ENDSR
045300960305     C/EJECT
045400030204     C************************************************************
045500030204     C* A chi deve trasmettere
045600030204     C************************************************************
045700030204     C     ACHI          BEGSR
045800030204     C*
045900030204     c                   eval      S_Tasto = $Tasto
046000030204     C*
046100030204     c                   move      '1'           t1trs
046200030204     c                   clear                   t1fil
046300030204     c     su_win        tag
046400030204     C                   Exfmt     W1Tras
046500030204     c                   if        t1trs = '2' and
046600030204     c                             t1fil = 0
046700030204     c                   seton                                        77
046800030204     c                   goto      su_win
046900030204     c                   end
047000030204     C*
047100030204     c                   eval      $Tasto = S_Tasto
047200030204     C*
047300030204     C                   ENDSR
047400030204     C/EJECT
047500030204     C************************************************************
047600030204    >C* AGGIORNAMENTO ANAGRAFICA
047700030204     C************************************************************
047800030204    >C     FLDAGG        BEGSR
047900030204     C*
048000030204     C*  campi chiave da scrivere e aggiornare
048100110927     C                   clear                   tbeKE1
048200110927     C                   MOVEl     D1COD         tbeKE1
048300030204     C*
048400030204     C                   ENDSR
048500940207     C/EJECT
048600940224     C************************************************************
048700940315    >C* AGGIORNAMENTO ANAGRAFICA
048800940224     C************************************************************
048900940315    >C     AGGANA        BEGSR
049000940207     C*
049100030204     C                   exsr      FLDAGG
049200030204     C*
049300940207     C                   SELECT
049400030204     C*
049500030114    >C     xtaopz        WHENEQ    '01'
049600030114    >C     xtaopz        OREQ      '03'
049700030204     C*
049800110927     C                   move      'WEK'         tbeCOD
049900110927    >C                   EXSR      RIEANA_WEK
050000110927     c                   write     tntbe000                             59
050100940318     C* se rcd non scrivibile attivo errore
050200030114    >C   59              SETON                                          99
050300030204      *
050400030114    >C     xtaopz        WHENEQ    '02'
050500030204      *
050600110927     C                   move      'WEK'         tbeCOD
050700110927     C     KTBL          chain     tntbe01L
050800030204     C                   exsr      FLDAGG
050900110927    >C                   EXSR      RIEANA_WEK
051000110927     c                   update    tntbe000                             59
051100030114    >C   59              SETON                                          99
051200030204      *
051300030114    >C     xtaopz        WHENEQ    '04'
051400110927     C                   move      'WEK'         tbeCOD
051500110927     C     KTBL          chain     tntbe01L
051600030115     c                   if        $tasto = F23
051700110927     c                   delete    tntbe000                             59
051800030115     c                   else
051900110927     c                   delete    tntbe000                             59
052000030115     c                   end
052100030114    >C   59              SETON                                          99
052200940621     C*
052300940207     C                   ENDSL
052400940203     C*
052500940203     C                   ENDSR
052600940224     C************************************************************
052700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
052800940224     C************************************************************
052900110927     C     RIEANA_WEK    BEGSR
053000030114     C*
053100110927     C                   CLEAR                   tbeuni
053200110927     C                   movel     *all'0'       tbeuni
053300110928     C                   MOVEL     *all'0'       EXTDS
053400110927     C* campi delle 45 settimane possibili
053500110927     c                   if            d1001 <> *blank
053600110927     c                   movel     d1001         B90WK01
053700110928     c                   end
053800110927     c                   if            d1002 <> *blank
053900110927     c                   movel     d1002         B90WK02
054000110928     c                   end
054100110927     c                   if            d1003 <> *blank
054200110927     c                   movel     d1003         B90WK03
054300110928     c                   end
054400110927     c                   if            d1004 <> *blank
054500110927     c                   movel     d1004         B90WK04
054600110928     c                   end
054700110927     c                   if            d1005 <> *blank
054800110927     c                   movel     d1005         B90WK05
054900110928     c                   end
055000110927     c                   if            d1006 <> *blank
055100110927     c                   movel     d1006         B90WK06
055200110928     c                   end
055300110927     c                   if            d1007 <> *blank
055400110927     c                   movel     d1007         B90WK07
055500110928     c                   end
055600110927     c                   if            d1008 <> *blank
055700110927     c                   movel     d1008         B90WK08
055800110928     c                   end
055900110927     c                   if            d1009 <> *blank
056000110927     c                   movel     d1009         B90WK09
056100110928     c                   end
056200110927     c                   if            d1010 <> *blank
056300110927     c                   movel     d1010         B90WK10
056400110928     c                   end
056500110927     c                   if            d1011 <> *blank
056600110927     c                   movel     d1011         B90WK11
056700110928     c                   end
056800110927     c                   if            d1012 <> *blank
056900110927     c                   movel     d1012         B90WK12
057000110928     c                   end
057100110927     c                   if            d1013 <> *blank
057200110927     c                   movel     d1013         B90WK13
057300110928     c                   end
057400110927     c                   if            d1014 <> *blank
057500110927     c                   movel     d1014         B90WK14
057600110928     c                   end
057700110927     c                   if            d1015 <> *blank
057800110927     c                   movel     d1015         B90WK15
057900110928     c                   end
058000110927     c                   if            d1016 <> *blank
058100110927     c                   movel     d1016         B90WK16
058200110928     c                   end
058300110927     c                   if            d1017 <> *blank
058400110927     c                   movel     d1017         B90WK17
058500110928     c                   end
058600110927     c                   if            d1018 <> *blank
058700110927     c                   movel     d1018         B90WK18
058800110928     c                   end
058900110927     c                   if            d1019 <> *blank
059000110927     c                   movel     d1019         B90WK19
059100110928     c                   end
059200110927     c                   if            d1020 <> *blank
059300110927     c                   movel     d1020         B90WK20
059400110928     c                   end
059500110927     c                   if            d1021 <> *blank
059600110927     c                   movel     d1021         B90WK21
059700110928     c                   end
059800110927     c                   if            d1022 <> *blank
059900110927     c                   movel     d1022         B90WK22
060000110928     c                   end
060100110927     c                   if            d1023 <> *blank
060200110927     c                   movel     d1023         B90WK23
060300110928     c                   end
060400110927     c                   if            d1024 <> *blank
060500110927     c                   movel     d1024         B90WK24
060600110928     c                   end
060700110928     c                   if            d1025 <> *blank
060800110927     c                   movel     d1025         B90WK25
060900110928     c                   end
061000110928     c                   if            d1026 <> *blank
061100110927     c                   movel     d1026         B90WK26
061200110928     c                   end
061300110928     c                   if            d1027 <> *blank
061400110927     c                   movel     d1027         B90WK27
061500110928     c                   end
061600110928     c                   if            d1028 <> *blank
061700110927     c                   movel     d1028         B90WK28
061800110928     c                   end
061900110928     c                   if            d1029 <> *blank
062000110927     c                   movel     d1029         B90WK29
062100110928     c                   end
062200110928     c                   if            d1030 <> *blank
062300110927     c                   movel     d1030         B90WK30
062400110928     c                   end
062500110928     c                   if            d1031 <> *blank
062600110927     c                   movel     d1031         B90WK31
062700110928     c                   end
062800110928     c                   if            d1032 <> *blank
062900110927     c                   movel     d1032         B90WK32
063000110928     c                   end
063100110928     c                   if            d1033 <> *blank
063200110927     c                   movel     d1033         B90WK33
063300110928     c                   end
063400110928     c                   if            d1034 <> *blank
063500110927     c                   movel     d1034         B90WK34
063600110928     c                   end
063700110928     c                   if            d1035 <> *blank
063800110927     c                   movel     d1035         B90WK35
063900110928     c                   end
064000110928     c                   if            d1036 <> *blank
064100110927     c                   movel     d1036         B90WK36
064200110928     c                   end
064300110928     c                   if            d1037 <> *blank
064400110927     c                   movel     d1037         B90WK37
064500110928     c                   end
064600110928     c                   if            d1038 <> *blank
064700110927     c                   movel     d1038         B90WK38
064800110928     c                   end
064900110928     c                   if            d1039 <> *blank
065000110927     c                   movel     d1039         B90WK39
065100110928     c                   end
065200110928     c                   if            d1040 <> *blank
065300110927     c                   movel     d1040         B90WK40
065400110928     c                   end
065500110928     c                   if            d1041 <> *blank
065600110927     c                   movel     d1041         B90WK41
065700110928     c                   end
065800110928     c                   if            d1042 <> *blank
065900110927     c                   movel     d1042         B90WK42
066000110928     c                   end
066100110928     c                   if            d1043 <> *blank
066200110927     c                   movel     d1043         B90WK43
066300110928     c                   end
066400110928     c                   if            d1044 <> *blank
066500110927     c                   movel     d1044         B90WK44
066600110928     c                   end
066700110928     c                   if            d1045 <> *blank
066800110927     c                   movel     d1045         B90WK45
066900110928     c                   end
067000110927     C                   MOVEL     EXTDS         tbeuni
067100030114     C*
067200940207     C                   ENDSR
067300940224     C************************************************************
067400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
067500940224     C************************************************************
067600940224     C     GESAGG        BEGSR
067700940224     C*
067800940224     C* segnalo al pgm chiamante l'avvenuta conferma
067900030114    >C                   MOVE      *ON           xtaopr
068000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
068100030114    >C                   MOVE      '0'           xtaret
068200940224     C*
0683009402241    C                   SELECT
068400940224     C* nel caso di immissione, ripristino la videata iniziale
068500030114    >C     xtaopz        WHENEQ    '01'
068600940224     C                   MOVE      *ON           $INZD1
068700030114     C                   MOVE      *ON           $FINE
068800940224     C* altrimenti ritorno al pgm chiamante
068900940224     C                   OTHER
069000940224     C                   MOVE      *ON           $FINE
0691009402241-   C                   ENDSL
069200940224     C*
069300940224     C                   ENDSR
069400940131     C************************************************************
069500940131     C* OPERAZIONI INIZIALI
069600940131     C************************************************************
069700940131     C     *INZSR        BEGSR
069800940131     C*
069900940127     C* Reperimento parametri
070000940127     C*
070100940117     C     *ENTRY        PLIST
070200940117     C                   PARM                    KPJBA
070300030113     C                   movel     kpjbu         tabds
070400030115     c                   setoff                                       08
070500030115     C*
070600940223     C* Variabili per gestione videate
070700940223     C                   MOVE      'D1'          $GEST
070800940223     C                   MOVE      *OFF          $FINE
070900940223     C                   MOVE      *ON           $INZD1
071000940224     C                   MOVE      *BLANK        $LASTG
071100940224     C                   MOVE      *BLANK        $LASTV
071200940506     C                   Z-ADD     0             $ULKD1            3 0
071300940127     C*
071400940223     C* Variabili appoggio
071500940224     C                   Z-ADD     0             CURR
071600940224     C                   Z-ADD     0             CURC
071700940207     C*
071800940207     C* Valorizzazione campi univoci testate
071900940614     C                   CLEAR                   T1
072000940614     C                   MOVEL     KNSIF         NOMSIF
072100030113     C******             MOVEL     XSCDSI        NOMDIT
072200941027     C                   MOVEL     DSPGM         NOMPGM
072300030113     C*
072400110927     C                   move      'WEK'         tbeCOD
072500110927     C                   clear                   tbeKE1
072600110927     C                   moveL     xtakey        tbeKE1
072700940224     C*
072800940224     C* Inizializzazione/Decodifica prima videata
072900940224     C                   EXSR      INZD1
073000940224     C                   MOVE      *OFF          $INZD1
073100940127     C*
073200940117     C                   ENDSR
073300940128     C************************************************************
073400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
073500940223     C************************************************************
073600940223     C     DEFCAM        BEGSR
073700940223     C*
073800940223     C* klist
073900030113     C     KTBL          KLIST
074000110927     C                   KFLD                    tbeCOD
074100110927     C                   KFLD                    tbeKE1
074200940207     C*
074300940223     C* Variabili per gestione videate
074400940223     C*
074500940223     C                   MOVE      *BLANK        $GEST             2
074600940223     C                   MOVE      *BLANK        $FINE             1
074700940223     C                   MOVE      *BLANK        $INZD1            1
074800940223     C                   MOVE      *BLANK        $LASTG            2
074900940223     C                   MOVE      *BLANK        $LASTV            2
075000940223     C*
075100940223     C* Variabili appoggio
075200940223     C                   Z-ADD     0             CURR              2 0
075300940223     C                   Z-ADD     0             CURC              2 0
075400940223     C                   MOVE      *ZEROS        WIN              99
075500940224     C                   MOVE      *BLANK        WTBERR            1
075600940223     C*
075700940223     C* Indici
075800940223     C                   Z-ADD     0             X                 3 0
075900940223     C                   Z-ADD     0             Y                 3 0
076000940127     C*
076100940117     C                   ENDSR
076200940128     C************************************************************
