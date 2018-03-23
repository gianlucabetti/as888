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
002500110927$001 FTNTB90D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700110927     Ftntbe01l  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900110926$003 D EXTDS         E DS                  EXTNAME(tntb90ds)
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
004000110926     D  XTAKEY                 6      8
004100940201     D*-------------
004200030204     D  S_TASTO        S                   like($Tasto)
004300940201     D DSFMT           DS
004400940506     D  $TASTO               369    369
004500940201     D  NRG                  370    370
004600940201     D  NCL                  371    371
004700940201     D*-------------
004800940201     D* posizione cursore
004900940201     D CURSOR          DS
005000940223     D  RIRI                   1      2B 0 INZ
005100940201     D  R$$                    2      2
005200940223     D  COCO                   3      4B 0 INZ
005300940201     D  C$$                    4      4
005400940207     D*-------------
005500940207     D* Reperimento nome PGM
005600940207     D STATUS         SDS           333
005700940207     D  DSPGM            *PROC
005800940225     D*-------------
005900940225     D* COSTANTI
006000940225     D*-------------
006100940506     D* Tasti di funzione
006200940506     D F01             C                   CONST(X'31')
006300940506     D F02             C                   CONST(X'32')
006400940506     D F03             C                   CONST(X'33')
006500940506     D F04             C                   CONST(X'34')
006600940506     D F05             C                   CONST(X'35')
006700940506     D F06             C                   CONST(X'36')
006800940506     D F07             C                   CONST(X'37')
006900940506     D F08             C                   CONST(X'38')
007000940506     D F09             C                   CONST(X'39')
007100940506     D F10             C                   CONST(X'3A')
007200940506     D F11             C                   CONST(X'3B')
007300940506     D F12             C                   CONST(X'3C')
007400940506     D F13             C                   CONST(X'B1')
007500940506     D F14             C                   CONST(X'B2')
007600940506     D F15             C                   CONST(X'B3')
007700940506     D F16             C                   CONST(X'B4')
007800940506     D F17             C                   CONST(X'B5')
007900940506     D F18             C                   CONST(X'B6')
008000940506     D F19             C                   CONST(X'B7')
008100940506     D F20             C                   CONST(X'B8')
008200940506     D F21             C                   CONST(X'B9')
008300940506     D F22             C                   CONST(X'BA')
008400940506     D F23             C                   CONST(X'BB')
008500940506     D F24             C                   CONST(X'BC')
008600940506     D ENTER           C                   CONST(X'F1')
008700940506     D ROLDWN          C                   CONST(X'F4')
008800940506     D ROLLUP          C                   CONST(X'F5')
008900940506     D*-------------
009000941108     D DATA            C                   CONST('0001-01-01')
009100030114     D Annull          C                   CONST('  ANNULLATO  ')
009200030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009300030114     D OPz_Var         C                   CONST('VARIAZIONE')
009400030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009500030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009600940127     C*----------------------------------------------------*
009700940127     C*                MAIN LINE PROGRAM
009800940127     C*----------------------------------------------------*
009900940223     C*
010000940223     C     $FINE         DOWEQ     *OFF
010100940202     C     $GEST         CASEQ     'D1'          GESD1
010200940117     C                   END
010300940117     C                   END
010400940325     C* fine programma
010500030113     c                   movel     tabds         kpjbu
010600030113     C                   Seton                                        LR
010700940131     C************************************************************
010800940510     C* GESTIONE VIDEO RECORD D1
010900940131     C************************************************************
011000940127     C     GESD1         BEGSR
011100030113      *
011200940225     C* inizializzazione videata
011300940223     C     $INZD1        IFEQ      *ON
011400940127     C                   EXSR      INZD1
011500940223     C                   MOVE      *OFF          $INZD1
011600940117     C                   END
011700030113      *
011800940225     C* emissione piede videata se proveniente da altra
011900940225     C* salvataggio valore formato di provenienza
012000940207     C     $LASTV        IFNE      'D1'
012100940614     C                   WRITE     T1
012200940202     C                   MOVE      $LASTV        $LASTG
012300940202     C                   MOVE      'D1'          $LASTV
012400940202     C                   END
012500940131     C*
012600940203     C                   SELECT
012700940203     C* Annullamento
012800030114     C* Visualizzazione
012900030114    >C     xtaopz        WHENEQ    '01'
013000030114     c                   movel     Opz_Imm       D1Fun
013100030114    >C     xtaopz        WHENEQ    '05'
013200030114     c                   movel     Opz_Vis       D1Fun
013300030114     C                   SETON                                        020306
013400030114    >C     xtaopz        WHENEQ    '04'
013500030114     c                   movel     Opz_Del       D1Fun
013600110902     C                   SETON                                        02
013700030114     C                   SETOFF                                         06
013800940620     C* Manutenzione
013900030113    >C     xtaopz        WHENEQ    '02'
014000030114     c                   movel     Opz_Var       D1Fun
014100030114     C                   SETON                                        02
014200030114     C                   SETOFF                                       0306
014300940620     C*
014400940620     C                   OTHER
014500940620     C*
014600030114     C                   SETOFF                                       020306
014700940203     C                   ENDSL
014800030114     C*
014900030114     c                   SETOFF                                         99
015000940620     C* Immissione/Modifica/Copia
015100030113     C*               *----------------*
015200940620     C                   EXFMT     D1
015300030113     C*               *----------------*
015400030114     C*
015500030114     C     *IN99         IFEQ      *OFF
015600030114     C*
0157000301141    C                   SELECT
015800030114     C* F3=Fine
015900030114     C     $TASTO        WHENEQ    F03
016000030114     C                   EXSR      F03D1
016100030114     C* F12=Ritorno
016200030114     C     $TASTO        WHENEQ    F12
016300030114     C                   EXSR      F12D1
016400030114     C*
0165000301141O   C                   OTHER
016600030114     C*
016700030114     C                   EXSR      CTRD1
016800030113     C*
016900940616     C     *IN99         IFEQ      *OFF
017000940616     C* F6
0171000302052    C     $TASTO        ifeq      F06
0172000301152    C     $TASTO        oreq      F23
017300030204     C* A chi Trasmette la Tabella ?
017400110901     c****               Exsr      ACHI
017500940224     C* eseguo aggiornamento
017600940315    >C                   EXSR      AGGANA
017700940224     C* eseguo operazioni del dopo-aggiornamento
017800030114     C  N99              EXSR      GESAGG
017900030205     c                   end
018000030114     C*
018100030114     C                   ENDIF
018200030114     C*
0183000301142-   C                   ENDSL
018400030114     C*
0185000301142-   C                   ENDIF
018600030114     C*
018700940117     C                   ENDSR
018800940117      ************************************************************
018900940207      * INIZIALIZZAZIONE VIDEATA DATI
019000940117      ************************************************************
019100940127     C     INZD1         BEGSR
019200940131     C*
019300940615     C                   CLEAR                   D1
019400110927     C                   move      'WEK'         tbeCOD
019500030113     C                   MOVEl     xtakey        D1COD
019600110927     C                   clear                   tbeKE1
019700110927     C                   MOVEl     xtakey        tbeKE1
019800940207     C*
019900940224     C* valorizzo la chiave univoca per reperire i dati
020000940224     C* se non immissione
0201000301141   >C     xtaopz        IFNE      '01'
020200030204      *
020300940315    >C*-------------------------------------------------------------
020400940315    >C* impostare i campi chiave ricevuti
020500940315    >C*-------------------------------------------------------------
020600110927     C     KTBL          chain(N)  tntbe01L
020700940614     C*
020800110927     C                   IF        %Found(tntbe01L)
020900110927      *
021000110927     C                   MOVEL     tbeUNI        EXTDS
021100110927      *
021200110901     c*******            if        TblFLG = '*'
021300110901     c*******            seton                                        08
021400110901     C*******            MOVEl     Annull        D1Ann
021500110901     C*******            end
021600030113     C*
021700940224     C* riempio la videata
021800940224     C                   EXSR      RIED1
021900030113      *
022000940224     C* richiamo routine dei ctrl per decodificarla
022100940614    >C                   EXSR      CTRD1
022200030113      *
022300940224     C                   MOVE      *ALL'0'       IN5098
022400940224     C                   MOVEA     IN5098        *IN(50)
022500030113      *
022600940224     C* definizione indicatori su video
022700940224     C                   SETOFF                                       02
0228009402243    C                   SELECT
022900030114    >C     xtaopz        WHENEQ    '02'
023000940224     C                   SETON                                        02
023100030114    >C     xtaopz        WHENEQ    '05'
0232009402243-   C                   ENDSL
023300940224     C*
0234009402242-   C                   ENDIF
023500030114     C*
0236000301141   >C                   Else
023700030114     C* Se in Immissione nuovo record
023800030114     c                   exsr      NewD1
023900940223     C*
0240009402241-   C                   ENDIF
024100940224     C*
024200940117     C                   ENDSR
024300940207     C/EJECT
024400940207     C************************************************************
024500940510     C* RIEMPIMENTO VIDEATA  D1
024600940207     C************************************************************
024700030114     C     NEWD1         BEGSR
024800940207     C*
024900030114     c                   clear                   d1
025000030114     C*
025100030114     C                   ENDSR
025200030114     C************************************************************
025300030114     C* RIEMPIMENTO VIDEATA  D1
025400030114     C************************************************************
025500030114     C     RIED1         BEGSR
025600030114     C*
025700110927     C* campi delle 45 settimane possibili
025800110928     c                   if         B90WK01 <> *zeros
025900110927     c                   eval      d1001   =     B90WK01
026000110928     c                   end
026100110928     c                   if         B90WK02 <> *zeros
026200110927     c                   eval      d1002   =     B90WK02
026300110928     c                   end
026400110928     c                   if         B90WK03 <> *zeros
026500110927     c                   eval      d1003   =     B90WK03
026600110928     c                   end
026700110928     c                   if         B90WK04 <> *zeros
026800110927     c                   eval      d1004   =     B90WK04
026900110928     c                   end
027000110928     c                   if         B90WK05 <> *zeros
027100110927     c                   eval      d1005   =     B90WK05
027200110928     c                   end
027300110928     c                   if         B90WK06 <> *zeros
027400110927     c                   eval      d1006   =     B90WK06
027500110928     c                   end
027600110928     c                   if         B90WK07 <> *zeros
027700110927     c                   eval      d1007   =     B90WK07
027800110928     c                   end
027900110928     c                   if         B90WK08 <> *zeros
028000110927     c                   eval      d1008   =     B90WK08
028100110928     c                   end
028200110928     c                   if         B90WK09 <> *zeros
028300110927     c                   eval      d1009   =     B90WK09
028400110928     c                   end
028500110928     c                   if         B90WK10 <> *zeros
028600110927     c                   eval      d1010   =     B90WK10
028700110928     c                   end
028800110928     c                   if         B90WK11 <> *zeros
028900110928     c                   eval      d1011   =     B90WK11
029000110928     c                   end
029100110928     c                   if         B90WK12 <> *zeros
029200110928     c                   eval      d1012   =     B90WK12
029300110928     c                   end
029400110928     c                   if         B90WK13 <> *zeros
029500110928     c                   eval      d1013   =     B90WK13
029600110928     c                   end
029700110928     c                   if         B90WK14 <> *zeros
029800110928     c                   eval      d1014   =     B90WK14
029900110928     c                   end
030000110928     c                   if         B90WK15 <> *zeros
030100110928     c                   eval      d1015   =     B90WK15
030200110928     c                   end
030300110928     c                   if         B90WK16 <> *zeros
030400110928     c                   eval      d1016   =     B90WK16
030500110928     c                   end
030600110928     c                   if         B90WK17 <> *zeros
030700110928     c                   eval      d1017   =     B90WK17
030800110928     c                   end
030900110928     c                   if         B90WK18 <> *zeros
031000110928     c                   eval      d1018   =     B90WK18
031100110928     c                   end
031200110928     c                   if         B90WK19 <> *zeros
031300110928     c                   eval      d1019   =     B90WK19
031400110928     c                   end
031500110928     c                   if         B90WK20 <> *zeros
031600110927     c                   eval      d1020   =     B90WK20
031700110928     c                   end
031800110928     c                   if         B90WK21 <> *zeros
031900110928     c                   eval      d1021   =     B90WK21
032000110928     c                   end
032100110928     c                   if         B90WK22 <> *zeros
032200110928     c                   eval      d1022   =     B90WK22
032300110928     c                   end
032400110928     c                   if         B90WK23 <> *zeros
032500110928     c                   eval      d1023   =     B90WK23
032600110928     c                   end
032700110928     c                   if         B90WK24 <> *zeros
032800110928     c                   eval      d1024   =     B90WK24
032900110928     c                   end
033000110928     c                   if         B90WK25 <> *zeros
033100110928     c                   eval      d1025   =     B90WK25
033200110928     c                   end
033300110928     c                   if         B90WK26 <> *zeros
033400110928     c                   eval      d1026   =     B90WK26
033500110928     c                   end
033600110928     c                   if         B90WK27 <> *zeros
033700110928     c                   eval      d1027   =     B90WK27
033800110928     c                   end
033900110928     c                   if         B90WK28 <> *zeros
034000110928     c                   eval      d1028   =     B90WK28
034100110928     c                   end
034200110928     c                   if         B90WK29 <> *zeros
034300110928     c                   eval      d1029   =     B90WK29
034400110928     c                   end
034500110928     c                   if         B90WK30 <> *zeros
034600110927     c                   eval      d1030   =     B90WK30
034700110928     c                   end
034800110928     c                   if         B90WK31 <> *zeros
034900110928     c                   eval      d1031   =     B90WK31
035000110928     c                   end
035100110928     c                   if         B90WK32 <> *zeros
035200110928     c                   eval      d1032   =     B90WK32
035300110928     c                   end
035400110928     c                   if         B90WK33 <> *zeros
035500110928     c                   eval      d1033   =     B90WK33
035600110928     c                   end
035700110928     c                   if         B90WK34 <> *zeros
035800110928     c                   eval      d1034   =     B90WK34
035900110928     c                   end
036000110928     c                   if         B90WK35 <> *zeros
036100110928     c                   eval      d1035   =     B90WK35
036200110928     c                   end
036300110928     c                   if         B90WK36 <> *zeros
036400110928     c                   eval      d1036   =     B90WK36
036500110928     c                   end
036600110928     c                   if         B90WK37 <> *zeros
036700110928     c                   eval      d1037   =     B90WK37
036800110928     c                   end
036900110928     c                   if         B90WK38 <> *zeros
037000110928     c                   eval      d1038   =     B90WK38
037100110928     c                   end
037200110928     c                   if         B90WK39 <> *zeros
037300110928     c                   eval      d1039   =     B90WK39
037400110928     c                   end
037500110928     c                   if         B90WK40 <> *zeros
037600110927     c                   eval      d1040   =     B90WK40
037700110928     c                   end
037800110928     c                   if         B90WK41 <> *zeros
037900110927     c                   eval      d1041   =     B90WK41
038000110928     c                   end
038100110928     c                   if         B90WK42 <> *zeros
038200110927     c                   eval      d1042   =     B90WK42
038300110928     c                   end
038400110928     c                   if         B90WK43 <> *zeros
038500110927     c                   eval      d1043   =     B90WK43
038600110928     c                   end
038700110928     c                   if         B90WK44 <> *zeros
038800110927     c                   eval      d1044   =     B90WK44
038900110928     c                   end
039000110928     c                   if         B90WK45 <> *zeros
039100110927     c                   eval      d1045   =     B90WK45
039200110928     c                   end
039300030204     C*
039400940207     C                   ENDSR
039500940309     C************************************************************
039600940309     C* GESTIONE F03 VIDEO D1
039700940309     C************************************************************
039800940309     C     F03D1         BEGSR
039900940309     C*
040000940309     C                   MOVE      *ON           $FINE
040100030113    >C                   MOVE      '1'           xtaret
040200940325     C*
040300940325     C                   ENDSR
040400940309     C************************************************************
040500940309     C* GESTIONE F12 VIDEO D1
040600940309     C************************************************************
040700940309     C     F12D1         BEGSR
040800940309     C*
040900940309     C                   MOVE      *ON           $FINE
041000940309     C*
041100940309     C                   ENDSR
041200940131     C************************************************************
041300940207     C* CONTROLLO VIDEATA
041400940131     C************************************************************
041500940127     C     CTRD1         BEGSR
041600940131     C*
041700940127     C                   SETOFF                                       99
041800030123      *
041900940127     C* non imposto pos. del cursore
042000940127     C                   Z-ADD     0             H1RIGA
042100940127     C                   Z-ADD     0             H1COLO
042200940621     C*
042300940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0424000301141   >C     xtaopz        IFNE      '04'
042500971110     C*
042600971110     C* controllo codice vuoto
042700971110$017 C     D1COD         IFEQ      *ALL' '
042800971110     C                   SETON                                        5099
042900971110     C                   ENDIF
043000110908     C*
043100940207     C*
043200940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
043300030114    >C     xtaopz        IFEQ      '01'
043400030114    >C     xtaopz        oreq      '03'
043500110927     c                   clear                   tbeKE1
043600110927     c                   movel     d1cod         tbeKE1
043700110927     C     KTBL          chain(N)  tntbe01L
043800110927     c                   if        %Found(tntbe01L)
043900110908     C                   SETON                                        5399
044000030114     C                   EndIF
044100030114     C                   ENDIF
044200030114      *
0443009402241-   C                   END
044400940131     C*
044500940302     C     *IN99         IFEQ      *ON
044600940510     C     $GEST         ANDEQ     'D1'
044700940117     C                   MOVEA     *IN           WIN
044800940128     C                   MOVE      *ALL'0'       IN5098           49
044900940128     C                   MOVEA     IN5098        *IN(50)
045000940614     C                   WRITE     D1
045100940117     C                   MOVEA     WIN           *IN
045200940302     C                   ENDIF
045300940127     C*
045400940117     C                   ENDSR
045500960305     C/EJECT
045600030204     C************************************************************
045700030204     C* A chi deve trasmettere
045800030204     C************************************************************
045900030204     C     ACHI          BEGSR
046000030204     C*
046100030204     c                   eval      S_Tasto = $Tasto
046200030204     C*
046300030204     c                   move      '1'           t1trs
046400030204     c                   clear                   t1fil
046500030204     c     su_win        tag
046600030204     C                   Exfmt     W1Tras
046700030204     c                   if        t1trs = '2' and
046800030204     c                             t1fil = 0
046900030204     c                   seton                                        77
047000030204     c                   goto      su_win
047100030204     c                   end
047200030204     C*
047300030204     c                   eval      $Tasto = S_Tasto
047400030204     C*
047500030204     C                   ENDSR
047600030204     C/EJECT
047700030204     C************************************************************
047800030204    >C* AGGIORNAMENTO ANAGRAFICA
047900030204     C************************************************************
048000030204    >C     FLDAGG        BEGSR
048100030204     C*
048200030204     C*  campi chiave da scrivere e aggiornare
048300110927     C                   clear                   tbeKE1
048400110927     C                   MOVEl     D1COD         tbeKE1
048500030204     C*
048600030204     C                   ENDSR
048700940207     C/EJECT
048800940224     C************************************************************
048900940315    >C* AGGIORNAMENTO ANAGRAFICA
049000940224     C************************************************************
049100940315    >C     AGGANA        BEGSR
049200940207     C*
049300030204     C                   exsr      FLDAGG
049400030204     C*
049500940207     C                   SELECT
049600030204     C*
049700030114    >C     xtaopz        WHENEQ    '01'
049800030114    >C     xtaopz        OREQ      '03'
049900030204     C*
050000110927     C                   move      'WEK'         tbeCOD
050100110927    >C                   EXSR      RIEANA_WEK
050200110927     c                   write     tntbe000                             59
050300940318     C* se rcd non scrivibile attivo errore
050400030114    >C   59              SETON                                          99
050500030204      *
050600030114    >C     xtaopz        WHENEQ    '02'
050700030204      *
050800110927     C                   move      'WEK'         tbeCOD
050900110927     C     KTBL          chain     tntbe01L
051000030204     C                   exsr      FLDAGG
051100110927    >C                   EXSR      RIEANA_WEK
051200110927     c                   update    tntbe000                             59
051300030114    >C   59              SETON                                          99
051400030204      *
051500030114    >C     xtaopz        WHENEQ    '04'
051600110927     C                   move      'WEK'         tbeCOD
051700110927     C     KTBL          chain     tntbe01L
051800030115     c                   if        $tasto = F23
051900110927     c                   delete    tntbe000                             59
052000030115     c                   else
052100110927     c                   delete    tntbe000                             59
052200030115     c                   end
052300030114    >C   59              SETON                                          99
052400940621     C*
052500940207     C                   ENDSL
052600940203     C*
052700940203     C                   ENDSR
052800940224     C************************************************************
052900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
053000940224     C************************************************************
053100110927     C     RIEANA_WEK    BEGSR
053200030114     C*
053300110927     C                   CLEAR                   tbeuni
053400110927     C                   movel     *all'0'       tbeuni
053500110928     C                   MOVEL     *all'0'       EXTDS
053600110927     C* campi delle 45 settimane possibili
053700110927     c                   if            d1001 <> *blank
053800110927     c                   movel     d1001         B90WK01
053900110928     c                   end
054000110927     c                   if            d1002 <> *blank
054100110927     c                   movel     d1002         B90WK02
054200110928     c                   end
054300110927     c                   if            d1003 <> *blank
054400110927     c                   movel     d1003         B90WK03
054500110928     c                   end
054600110927     c                   if            d1004 <> *blank
054700110927     c                   movel     d1004         B90WK04
054800110928     c                   end
054900110927     c                   if            d1005 <> *blank
055000110927     c                   movel     d1005         B90WK05
055100110928     c                   end
055200110927     c                   if            d1006 <> *blank
055300110927     c                   movel     d1006         B90WK06
055400110928     c                   end
055500110927     c                   if            d1007 <> *blank
055600110927     c                   movel     d1007         B90WK07
055700110928     c                   end
055800110927     c                   if            d1008 <> *blank
055900110927     c                   movel     d1008         B90WK08
056000110928     c                   end
056100110927     c                   if            d1009 <> *blank
056200110927     c                   movel     d1009         B90WK09
056300110928     c                   end
056400110927     c                   if            d1010 <> *blank
056500110927     c                   movel     d1010         B90WK10
056600110928     c                   end
056700110927     c                   if            d1011 <> *blank
056800110927     c                   movel     d1011         B90WK11
056900110928     c                   end
057000110927     c                   if            d1012 <> *blank
057100110927     c                   movel     d1012         B90WK12
057200110928     c                   end
057300110927     c                   if            d1013 <> *blank
057400110927     c                   movel     d1013         B90WK13
057500110928     c                   end
057600110927     c                   if            d1014 <> *blank
057700110927     c                   movel     d1014         B90WK14
057800110928     c                   end
057900110927     c                   if            d1015 <> *blank
058000110927     c                   movel     d1015         B90WK15
058100110928     c                   end
058200110927     c                   if            d1016 <> *blank
058300110927     c                   movel     d1016         B90WK16
058400110928     c                   end
058500110927     c                   if            d1017 <> *blank
058600110927     c                   movel     d1017         B90WK17
058700110928     c                   end
058800110927     c                   if            d1018 <> *blank
058900110927     c                   movel     d1018         B90WK18
059000110928     c                   end
059100110927     c                   if            d1019 <> *blank
059200110927     c                   movel     d1019         B90WK19
059300110928     c                   end
059400110927     c                   if            d1020 <> *blank
059500110927     c                   movel     d1020         B90WK20
059600110928     c                   end
059700110927     c                   if            d1021 <> *blank
059800110927     c                   movel     d1021         B90WK21
059900110928     c                   end
060000110927     c                   if            d1022 <> *blank
060100110927     c                   movel     d1022         B90WK22
060200110928     c                   end
060300110927     c                   if            d1023 <> *blank
060400110927     c                   movel     d1023         B90WK23
060500110928     c                   end
060600110927     c                   if            d1024 <> *blank
060700110927     c                   movel     d1024         B90WK24
060800110928     c                   end
060900110928     c                   if            d1025 <> *blank
061000110927     c                   movel     d1025         B90WK25
061100110928     c                   end
061200110928     c                   if            d1026 <> *blank
061300110927     c                   movel     d1026         B90WK26
061400110928     c                   end
061500110928     c                   if            d1027 <> *blank
061600110927     c                   movel     d1027         B90WK27
061700110928     c                   end
061800110928     c                   if            d1028 <> *blank
061900110927     c                   movel     d1028         B90WK28
062000110928     c                   end
062100110928     c                   if            d1029 <> *blank
062200110927     c                   movel     d1029         B90WK29
062300110928     c                   end
062400110928     c                   if            d1030 <> *blank
062500110927     c                   movel     d1030         B90WK30
062600110928     c                   end
062700110928     c                   if            d1031 <> *blank
062800110927     c                   movel     d1031         B90WK31
062900110928     c                   end
063000110928     c                   if            d1032 <> *blank
063100110927     c                   movel     d1032         B90WK32
063200110928     c                   end
063300110928     c                   if            d1033 <> *blank
063400110927     c                   movel     d1033         B90WK33
063500110928     c                   end
063600110928     c                   if            d1034 <> *blank
063700110927     c                   movel     d1034         B90WK34
063800110928     c                   end
063900110928     c                   if            d1035 <> *blank
064000110927     c                   movel     d1035         B90WK35
064100110928     c                   end
064200110928     c                   if            d1036 <> *blank
064300110927     c                   movel     d1036         B90WK36
064400110928     c                   end
064500110928     c                   if            d1037 <> *blank
064600110927     c                   movel     d1037         B90WK37
064700110928     c                   end
064800110928     c                   if            d1038 <> *blank
064900110927     c                   movel     d1038         B90WK38
065000110928     c                   end
065100110928     c                   if            d1039 <> *blank
065200110927     c                   movel     d1039         B90WK39
065300110928     c                   end
065400110928     c                   if            d1040 <> *blank
065500110927     c                   movel     d1040         B90WK40
065600110928     c                   end
065700110928     c                   if            d1041 <> *blank
065800110927     c                   movel     d1041         B90WK41
065900110928     c                   end
066000110928     c                   if            d1042 <> *blank
066100110927     c                   movel     d1042         B90WK42
066200110928     c                   end
066300110928     c                   if            d1043 <> *blank
066400110927     c                   movel     d1043         B90WK43
066500110928     c                   end
066600110928     c                   if            d1044 <> *blank
066700110927     c                   movel     d1044         B90WK44
066800110928     c                   end
066900110928     c                   if            d1045 <> *blank
067000110927     c                   movel     d1045         B90WK45
067100110928     c                   end
067200110927     C                   MOVEL     EXTDS         tbeuni
067300030114     C*
067400940207     C                   ENDSR
067500940224     C************************************************************
067600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
067700940224     C************************************************************
067800940224     C     GESAGG        BEGSR
067900940224     C*
068000940224     C* segnalo al pgm chiamante l'avvenuta conferma
068100030114    >C                   MOVE      *ON           xtaopr
068200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
068300030114    >C                   MOVE      '0'           xtaret
068400940224     C*
0685009402241    C                   SELECT
068600940224     C* nel caso di immissione, ripristino la videata iniziale
068700030114    >C     xtaopz        WHENEQ    '01'
068800940224     C                   MOVE      *ON           $INZD1
068900030114     C                   MOVE      *ON           $FINE
069000940224     C* altrimenti ritorno al pgm chiamante
069100940224     C                   OTHER
069200940224     C                   MOVE      *ON           $FINE
0693009402241-   C                   ENDSL
069400940224     C*
069500940224     C                   ENDSR
069600940131     C************************************************************
069700940131     C* OPERAZIONI INIZIALI
069800940131     C************************************************************
069900940131     C     *INZSR        BEGSR
070000940131     C*
070100940127     C* Reperimento parametri
070200940127     C*
070300940117     C     *ENTRY        PLIST
070400940117     C                   PARM                    KPJBA
070500030113     C                   movel     kpjbu         tabds
070600030115     c                   setoff                                       08
070700030115     C*
070800940223     C* Variabili per gestione videate
070900940223     C                   MOVE      'D1'          $GEST
071000940223     C                   MOVE      *OFF          $FINE
071100940223     C                   MOVE      *ON           $INZD1
071200940224     C                   MOVE      *BLANK        $LASTG
071300940224     C                   MOVE      *BLANK        $LASTV
071400940506     C                   Z-ADD     0             $ULKD1            3 0
071500940127     C*
071600940223     C* Variabili appoggio
071700940224     C                   Z-ADD     0             CURR
071800940224     C                   Z-ADD     0             CURC
071900940207     C*
072000940207     C* Valorizzazione campi univoci testate
072100940614     C                   CLEAR                   T1
072200940614     C                   MOVEL     KNSIF         NOMSIF
072300030113     C******             MOVEL     XSCDSI        NOMDIT
072400941027     C                   MOVEL     DSPGM         NOMPGM
072500030113     C*
072600110927     C                   move      'WEK'         tbeCOD
072700110927     C                   clear                   tbeKE1
072800110927     C                   moveL     xtakey        tbeKE1
072900940224     C*
073000940224     C* Inizializzazione/Decodifica prima videata
073100940224     C                   EXSR      INZD1
073200940224     C                   MOVE      *OFF          $INZD1
073300940127     C*
073400940117     C                   ENDSR
073500940128     C************************************************************
073600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
073700940223     C************************************************************
073800940223     C     DEFCAM        BEGSR
073900940223     C*
074000940223     C* klist
074100030113     C     KTBL          KLIST
074200110927     C                   KFLD                    tbeCOD
074300110927     C                   KFLD                    tbeKE1
074400940207     C*
074500940223     C* Variabili per gestione videate
074600940223     C*
074700940223     C                   MOVE      *BLANK        $GEST             2
074800940223     C                   MOVE      *BLANK        $FINE             1
074900940223     C                   MOVE      *BLANK        $INZD1            1
075000940223     C                   MOVE      *BLANK        $LASTG            2
075100940223     C                   MOVE      *BLANK        $LASTV            2
075200940223     C*
075300940223     C* Variabili appoggio
075400940223     C                   Z-ADD     0             CURR              2 0
075500940223     C                   Z-ADD     0             CURC              2 0
075600940223     C                   MOVE      *ZEROS        WIN              99
075700940224     C                   MOVE      *BLANK        WTBERR            1
075800940223     C*
075900940223     C* Indici
076000940223     C                   Z-ADD     0             X                 3 0
076100940223     C                   Z-ADD     0             Y                 3 0
076200940127     C*
076300940117     C                   ENDSR
076400940128     C************************************************************
