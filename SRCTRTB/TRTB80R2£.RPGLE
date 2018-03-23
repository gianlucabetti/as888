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
002500030113$001 FTRTB80D2  CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700030113     FTABEL00F  UF A E           K DISK
002800940128     D*----------------------------------------------------*
002900030204$003 D EXTDS8A       E DS                  EXTNAME(DS8A) INZ
003000030204$003 D EXTDS8F       E DS                  EXTNAME(DS8F) INZ
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
004100030113     D  XTAKEY                 6     13
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
013700030114     C                   SETON                                        0203
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
015000030114     c                   movel     D1Fun         D2Fun
015100030113     C*
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
017100030114     C* Video 2
017200940506     C     *IN99         IFEQ      *OFF
017300030204     c     Video2        tag
017400030205     c                   setoff                                       99
017500030114     C*               *----------------*
017600030114     C                   EXFMT     D2
017700030114     C*               *----------------*
017800940131     C*
0179009402031    C                   SELECT
018000940131     C* F3=Fine
018100940506     C     $TASTO        WHENEQ    F03
018200940309     C                   EXSR      F03D1
018300030204     C* F4=Fine
018400030204     C     $TASTO        WHENEQ    F04
018500030204     C                   EXSR      F04D2
018600030204     c                   goto      Video2
018700940131     C* F12=Ritorno
018800940506     C     $TASTO        WHENEQ    F12
018900940309     C                   EXSR      F12D1
019000940131     C*
0191009402031O   C                   OTHER
019200030113     C*
019300030114     C                   EXSR      CTRD2
019400030205     c   99              goto      video2
019500030113     C*
019600940616     C     *IN99         IFEQ      *OFF
019700940616     C* F6
0198000302052    C     $TASTO        ifeq      F06
0199000301152    C     $TASTO        oreq      F23
020000030204     C* A chi Trasmette la Tabella ?
020100030204     c                   Exsr      ACHI
020200940224     C* eseguo aggiornamento
020300940315    >C                   EXSR      AGGANA
020400940224     C* eseguo operazioni del dopo-aggiornamento
020500030114     C  N99              EXSR      GESAGG
020600030205     c                   end
020700030114     C*
020800030114     C                   ENDIF
020900030114     C*
0210000301142-   C                   ENDSL
021100030114     C*
0212000301142-   C                   ENDIF
021300940224     C*
0214000301142-   C                   ENDSL
021500030114     C*
0216000301142-   C                   END
021700030114     C*
021800940117     C                   ENDSR
021900940117      ************************************************************
022000940207      * INIZIALIZZAZIONE VIDEATA DATI
022100940117      ************************************************************
022200940127     C     INZD1         BEGSR
022300940131     C*
022400940615     C                   CLEAR                   D1
022500030113     C                   CLEAR                   D2
022600030204     C                   move      '8A'          TBLcod
022700030113     C                   MOVEl     xtakey        D1COD
022800030113     C                   MOVEl     xtakey        D2COD
022900030113     C                   MOVEl     xtakey        TBLkey
023000940207     C*
023100940224     C* valorizzo la chiave univoca per reperire i dati
023200940224     C* se non immissione
0233000301141   >C     xtaopz        IFNE      '01'
023400030204      *
023500940315    >C*-------------------------------------------------------------
023600940315    >C* impostare i campi chiave ricevuti
023700940315    >C*-------------------------------------------------------------
023800030204     C                   move      '8A'          TBLcod
023900030204     C     KTBL          chain(N)  TABEL00F
024000940614     C*
024100030113     C                   IF        %Found(Tabel00F)
024200030204     C                   MOVEL     TBLuni        EXTDS8A
024300030114     c                   if        TblFLG = '*'
024400030115     c                   seton                                        08
024500030114     C                   MOVEl     Annull        D1Ann
024600030114     C                   MOVEl     D1Ann         D2Ann
024700030114     C                   end
024800030204     C*
024900030204     C*  Aggancia la tabella correlata: (8F)
025000030204     C                   move      '8F'          TBLcod
025100030204     C     KTBL          chain(N)  TABEL00F
025200030204     C                   IF        %Found(Tabel00F)
025300030204     C                   MOVEL     TBLuni        EXTDS8F
025400030204     C                   end
025500030113     C*
025600940224     C* riempio la videata
025700940224     C                   EXSR      RIED1
025800030113      *
025900940224     C* richiamo routine dei ctrl per decodificarla
026000940614    >C                   EXSR      CTRD1
026100030113      *
026200960228     C                   MOVEL     *IN99         SAV99             1
026300030113      *
026400960228     C* Se la gestione è più videate
026500960305    >C                   EXSR      CTRD2
026600030113      *
026700960305    >C     *IN99         IFEQ      *ON
026800960305    >C                   MOVEL     *IN99         SAV99             1
026900960305    >C                   ENDIF
027000030113      *
027100940224     C                   MOVE      *ALL'0'       IN5098
027200940224     C                   MOVEA     IN5098        *IN(50)
027300030113      *
027400940224     C* definizione indicatori su video
027500940224     C                   SETOFF                                       02
0276009402243    C                   SELECT
027700030114    >C     xtaopz        WHENEQ    '02'
027800940224     C                   SETON                                        02
027900030114    >C     xtaopz        WHENEQ    '05'
0280009402243-   C                   ENDSL
028100940224     C*
0282009402242-   C                   ENDIF
028300030114     C*
0284000301141   >C                   Else
028500030114     C* Se in Immissione nuovo record
028600030114     c                   exsr      NewD1
028700940223     C*
0288009402241-   C                   ENDIF
028900940224     C*
029000940117     C                   ENDSR
029100940207     C/EJECT
029200940207     C************************************************************
029300940510     C* RIEMPIMENTO VIDEATA  D1
029400940207     C************************************************************
029500030114     C     NEWD1         BEGSR
029600940207     C*
029700030114     c                   clear                   d1
029800030114     c                   clear                   d2
029900030114     c                   move      '+'           D1SPM
030000030114     c                   move      'N'           D1SDT
030100030123     c                   move      'E'           D112E
030200030115     c                   move      *blanks       D2CE1
030300030115     c                   move      *blanks       D2CE2
030400030115     c                   move      *blanks       D2CE3
030500030115     c                   move      *blanks       D2CE4
030600030114     c                   move      'N'           D2fc1
030700030114     c                   move      'N'           D2fc2
030800030114     c                   move      'N'           D2fc3
030900030114     c                   move      'N'           D2fc4
031000030204     c                   move      *blanks       D2prt
031100030204     c                   move      '00'          D2or01
031200030204     c                   move      '00'          D2or02
031300030204     c                   move      '00'          D2or03
031400030114     C*
031500030114     C                   ENDSR
031600030114     C************************************************************
031700030114     C* RIEMPIMENTO VIDEATA  D1
031800030114     C************************************************************
031900030114     C     RIED1         BEGSR
032000030114     C*
032100030113     C* descrizione
032200030113     c                   eval      d1DES = §8ADES
032300030113      * Autotr./Coop
032400030114     c                   if        §8AFAC ='C'
032500030113     c                   eval      d1FAC = §8AFAC
032600030114     c                   else
032700030114     c                   eval      d1FAC = 'A'
032800030114     c                   end
032900030113      * Tipo Servizio
033000030113     c                   select
033100030113     c                   when      §8ausc = 'S' and §8ausr = ' '
033200030113     c                   eval      d1TSR = 'C'
033300030113     c                   when      §8ausr = 'S' and §8ausc = ' '
033400030113     c                   eval      d1TSR = 'R'
033500030113     c                   when      §8ausr = 'S' and §8ausc = 'S'
033600030113     c                   eval      d1TSR = 'E'
033700030113     c                   when      §8ausc = 'X' and §8ausr = ' '
033800030113     c                   eval      d1TSR = 'X'
033900030113     c                   when      §8ausc = 'T' and §8ausr = ' '
034000030113     c                   eval      d1TSR = 'T'
034100030113     c                   when      §8ausc = 'P' and §8ausr = ' '
034200030113     c                   eval      d1TSR = 'P'
034300030113     c                   endsl
034400030113      * Poste/Non poste/Entrambe
034500030113     c                   if        §8afpt = *blank
034600030113     c                   eval      d1FPT = 'E'
034700030113     c                   endIf
034800030113     c                   if        §8afpt = 'N'
034900030113     c                   eval      d1FPT = 'N'
035000030113     c                   endIf
035100030113     c                   if        §8afpt = 'P'
035200030113     c                   eval      d1FPT = 'P'
035300030113     c                   endIf
035400030113      * Base/Accessori
035500030113     c                   if        §8atba = 'B'
035600030113     c                   eval      d1TBA = 'B'
035700030113     c                   else
035800030113     c                   eval      d1TBA = 'A'
035900030113     c                   endIf
036000030113      * Segno +/-
036100030113     c                   if        §8aspm = '-'
036200030113     c                   eval      d1SPM = '-'
036300030113     c                   else
036400030113     c                   eval      d1SPM = '+'
036500030113     c                   endIf
036600030113      * Sostituzione Dettaglio tariffa
036700030113     c                   eval      d1SDT = §8ASDT
036800030113      * Livello a Dettaglio
036900030113     c                   eval      d1FDE = §8AFDE
037000030113      * Livello a Prestazione
037100030113     c                   eval      d1FPR = §8AFPR
037200030113      * Livello a Giornata
037300030113     c                   eval      d1FGI = §8AFGI
037400100419      * obbligatorietà del minimo e del massimo sui dettagli
037500100419     c                   eval      d1obbMIMA = §8aOBBmima
037600030113      *Tipi Tariffe
037700030113      *   Dettaglio
037800030113     c                   eval      d1TT0 = §8ATT0
037900030113     c                   eval      d1TT1 = §8ATT1
038000030113     c                   eval      d1TT2 = §8ATT2
038100030113     c                   eval      d1TT3 = §8ATT3
038200030113     c                   eval      d1TT4 = §8ATT4
038300030113     c                   eval      d1TT5 = §8ATT5
038400030113      *   Prestazione
038500030113     c                   eval      d1TT6 = §8ATT6
038600030113     c                   eval      d1TT7 = §8ATT7
038700030113     c                   eval      d1TT8 = §8ATT8
038800030113     c                   eval      d1TT9 = §8ATT9
038900030113     c                   eval      d1TTV = §8ATTV
039000030113      *   Giornata
039100030113     c                   eval      d1TB1 = §8ATB1
039200030113     c                   eval      d1TB2 = §8ATB2
039300030113     c                   eval      d1TB3 = §8ATB3
039400030113     c                   eval      d1TB4 = §8ATB4
039500030113     c                   eval      d1TB5 = §8ATB5
039600030113      *   Priorità di lettura
039700030113     c                   eval      d1LVL = §8ALVL
039800030113      *   Tipi Voce
039900030123     c                   if        §8A12E = *blank
040000030123     c                   eval      d112E = 'E'
040100030123     c                   else
040200030113     c                   eval      d112E = §8A12E
040300030123     c                   end
040400030113      *   C/E
040500030115     c                   if        §8ACE1 <> '000'
040600030114     c                   eval      d2CE1 = §8ACE1
040700030115     c                   else
040800030115     c                   eval      d2CE1 = *blank
040900030115     c                   end
041000030115     c                   if        §8ACE2 <> '000'
041100030114     c                   eval      d2CE2 = §8ACE2
041200030115     c                   else
041300030115     c                   eval      d2CE2 = *blank
041400030115     c                   end
041500030115     c                   if        §8ACE3 <> '000'
041600030114     c                   eval      d2CE3 = §8ACE3
041700030115     c                   else
041800030115     c                   eval      d2CE3 = *blank
041900030115     c                   end
042000030115     c                   if        §8ACE4 <> '000'
042100030114     c                   eval      d2CE4 = §8ACE4
042200030115     c                   else
042300030115     c                   eval      d2CE4 = *blank
042400030115     c                   end
042500030114      * forzature
042600030114     c                   if        d2FC1 <> *blank
042700030114     c                   eval      d2FC1 = §8AFC1
042800030114     c                   else
042900030114     c                   eval      d2FC1 = 'N'
043000030114     c                   end
043100030114     c                   if        d2FC2 <> *blank
043200030114     c                   eval      d2FC2 = §8AFC2
043300030114     c                   else
043400030114     c                   eval      d2FC2 = 'N'
043500030114     c                   end
043600030114     c                   if        d2FC3 <> *blank
043700030114     c                   eval      d2FC3 = §8AFC3
043800030114     c                   else
043900030114     c                   eval      d2FC3 = 'N'
044000030114     c                   end
044100030114     c                   if        d2FC4 <> *blank
044200030114     c                   eval      d2FC4 = §8AFC4
044300030114     c                   else
044400030114     c                   eval      d2FC4 = 'N'
044500030114     c                   end
044600030204     C*
044700030204     C*  Tabella 8F  correlata
044800030204     c                   eval      d2prt  = §8Fdes
044900030204     C*
045000030204     c                   if        §8For1 <> *blank
045100030204     c                   eval      d2or01 = §8For1
045200030204     c                   else
045300030204     c                   eval      d2or01 = '00'
045400030204     c                   end
045500940207     C*
045600030204     c                   if        §8For2 <> *blank
045700030204     c                   eval      d2or02 = §8For2
045800030204     c                   else
045900030204     c                   eval      d2or02 = '00'
046000030204     c                   end
046100030204     C*
046200030204     c                   if        §8For3 <> *blank
046300030204     c                   eval      d2or03 = §8For3
046400030204     c                   else
046500030204     c                   eval      d2or03 = '00'
046600030204     c                   end
046700030204     C*
046800090902      * Autotr./Coop
046900090902     c                   if        §8Aobb <> *blank
047000090902     c                   eval      d1obbl = §8Aobb
047100090902     c                   else
047200090902     c                   eval      d1obbl = 'N'
047300090902     c                   end
047400090903     c                   eval      d1vocr = §8Avocr
047500090902     C*
047600090902     C*
047700940207     C                   ENDSR
047800940309     C************************************************************
047900940309     C* GESTIONE F03 VIDEO D1
048000940309     C************************************************************
048100940309     C     F03D1         BEGSR
048200940309     C*
048300940309     C                   MOVE      *ON           $FINE
048400030113    >C                   MOVE      '1'           xtaret
048500940325     C*
048600940325     C                   ENDSR
048700940309     C************************************************************
048800940309     C* GESTIONE F12 VIDEO D1
048900940309     C************************************************************
049000940309     C     F12D1         BEGSR
049100940309     C*
049200940309     C                   MOVE      *ON           $FINE
049300940309     C*
049400940309     C                   ENDSR
049500030204     C************************************************************
049600030204     C* GESTIONE F04 VIDEO D2
049700030204     C************************************************************
049800030204     C     F04D2         BEGSR
049900030204     C*
050000030204     c                   exfmt     D2F4
050100030204     C*
050200030204     C                   ENDSR
050300940131     C************************************************************
050400940207     C* CONTROLLO VIDEATA
050500940131     C************************************************************
050600940127     C     CTRD1         BEGSR
050700940131     C*
050800940127     C                   SETOFF                                       99
050900030123     C*
051000030123     C                   SETOFF                                       09
051100030123     C* SE COOPERATIVA
051200030123$017 C     D1FAC         IFEQ      'C'
051300030123     C                   SETON                                        09
051400030123     C                   End
051500030123      *
051600940127     C* non imposto pos. del cursore
051700940127     C                   Z-ADD     0             H1RIGA
051800940127     C                   Z-ADD     0             H1COLO
051900940621     C*
052000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0521000301141   >C     xtaopz        IFNE      '04'
052200971110     C*
052300971110     C* controllo codice vuoto
052400971110$017 C     D1COD         IFEQ      *ALL' '
052500971110     C                   SETON                                        5099
052600030114     C                   Else
052700030114     c                   eval      d2cod = d1cod
052800971110     C                   ENDIF
052900030114     C*
053000030114     C* controllo codice vuoto
053100030114$017 C     D1DES         IFEQ      *ALL' '
053200030114     C                   SETON                                        5199
053300030114     C                   Else
053400030114     c                   eval      d2des = d1des
053500030114     C                   ENDIF
053600940207     C*
053700940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
053800030114    >C     xtaopz        IFEQ      '01'
053900030114    >C     xtaopz        oreq      '03'
054000030114     c                   movel(p)  d1cod         tblkey
054100030114     C     KTBL          chain(N)  TABEL00F
054200030114     c                   if        %Found(TABEL00F)
054300030114     C                   SETON                                        5299
054400030114     C                   EndIF
054500030114     C                   ENDIF
054600030114     C*
054700030114     C* controllo codice vuoto
054800030114$017 C     D1FAC         IFEQ      *ALL' '
054900030114     C                   SETON                                        5399
055000030114     C                   End
055100030114      *
055200030114     C* controllo codice vuoto
055300070212$017 C**** D1TSR         IFEQ      *ALL' '
055400070212     C****               SETON                                        5499
055500070212     C****               End
055600030114      *
055700030115$017 C     D1FAC         IFEQ      'A'
055800030115$017 C     D1TSR         IFEQ      'X'
055900030115$017 C     D1TSR         OREQ      'T'
056000030115$017 C     D1TSR         OREQ      'P'
056100030115     C                   SETON                                        6599
056200030115     C                   End
056300030115     C                   End
056400030115      *
056500030115$017 C     D1FAC         IFEQ      'C'
056600030115$017 C     D1TSR         IFEQ      'C'
056700030115$017 C     D1TSR         OREQ      'R'
056800030115$017 C     D1TSR         OREQ      'E'
056900030115     C                   SETON                                        6599
057000030115     C                   End
057100030115     C                   End
057200030115      *
057300030114     C* controllo codice vuoto
057400030114$017 C     D1FPT         IFEQ      *ALL' '
057500030114     C                   SETON                                        5599
057600030114     C                   End
057700030114      *
057800030114     C* controllo codice vuoto
057900030114$017 C     D1TBA         IFEQ      *ALL' '
058000030114     C                   SETON                                        5699
058100030114     C                   End
058200030114      *
058300030114     C* controllo codice vuoto
058400030114$017 C     D1SPM         IFEQ      *ALL' '
058500030114     C                   SETON                                        5799
058600030114     C                   End
058700030114      *
058800030114     C* controllo codice vuoto
058900030114$017 C     D1SDT         IFEQ      *ALL' '
059000030114     C                   SETON                                        5899
059100030114     C                   End
059200030114      *
059300030114     C* controllo codice vuoto
059400030114$017 C     D1FDE         IFEQ      *ALL' '
059500030114$017 C     D1FPR         andeq     *ALL' '
059600030114$017 C     D1FGI         andeq     *ALL' '
059700030114     C                   SETON                                        5999
059800030114     C                   End
059900030114      *
060000030114     C* controllo codice vuoto
060100030114$017 C     D1FDE         IFne      *ALL' '
060200030114$017 C     D1TT0         andeq     *ALL' '
060300030114     C                   SETON                                        6099
060400030114     C                   End
060500030114      *
060600030114     C* controllo codice vuoto
060700030114$017 C     D1FPR         IFne      *ALL' '
060800030114$017 C     D1TT6         andeq     *ALL' '
060900030114     C                   SETON                                        6199
061000030114     C                   End
061100030114      *
061200030114     C* controllo codice vuoto
061300030114$017 C     D1FGI         IFne      *ALL' '
061400030114$017 C     D1TB1         andeq     *ALL' '
061500030114     C                   SETON                                        6299
061600030114     C                   End
061700030114      *
061800030114     C* controllo codice vuoto
061900030114$017 C     D1LVL         IFEQ      *ALL' '
062000030114     C                   SETON                                        6399
062100030114     C                   End
062200030114      *
062300030114     C* controllo codice vuoto
062400030123$017 C     D112E         IFEQ      *ALL' '
062500030123     c                   SETON                                        6499
062600030123     C                   End
062700090908     C* controllo codice vuoto solo x le Coop
062800090908     c                   if        §8AFAC ='C'
062900090903$017 C     D1vocr        IFEQ      *blank
063000090903$017 C     D1obbl        andeq     'S'
063100090903     c                   SETON                                        6999
063200090903     C                   End
063300090908     C                   End
063400030114      *
0635009402241-   C                   END
063600940131     C*
063700940302     C     *IN99         IFEQ      *ON
063800940510     C     $GEST         ANDEQ     'D1'
063900940117     C                   MOVEA     *IN           WIN
064000940128     C                   MOVE      *ALL'0'       IN5098           49
064100940128     C                   MOVEA     IN5098        *IN(50)
064200940614     C                   WRITE     D1
064300940117     C                   MOVEA     WIN           *IN
064400940302     C                   ENDIF
064500940127     C*
064600940117     C                   ENDSR
064700960305     C/EJECT
064800960305     C************************************************************
064900960305     C* CONTROLLO VIDEATA D2
065000960305     C************************************************************
065100960305     C     CTRD2         BEGSR
065200030205     C*
065300030205     C* controllo codice vuoto ordinamento in stampa
065400030205$017 C     d2or01        IFEQ      *ALL' '
065500030205$017 C     d2or01        oreq      *ALL'0'
065600030205$017 C                   if        d2or02 <> '00' and d2or02 <> '  ' or
065700030205$017 C                             d2or03 <> '00' and d2or03 <> '  '
065800030205     C                   SETON                                        6699
065900030205     C                   End
066000030205     C                   End
066100960305     C*
066200030205$017 C     d2or02        IFEQ      *ALL' '
066300030205$017 C     d2or02        oreq      *ALL'0'
066400030205$017 C                   if        d2or01 <> '00' and d2or01 <> '  ' or
066500030205$017 C                             d2or03 <> '00' and d2or03 <> '  '
066600030205     C                   SETON                                        6799
066700030205     C                   End
066800030205     C                   End
066900030205     C*
067000030205$017 C     d2or03        IFEQ      *ALL' '
067100030205$017 C     d2or03        oreq      *ALL'0'
067200030205$017 C                   if        d2or01 <> '00' and d2or01 <> '  ' or
067300030205$017 C                             d2or02 <> '00' and d2or02 <> '  '
067400030205     C                   SETON                                        6899
067500030205     C                   End
067600030205     C                   End
067700030205     C*
067800960305     C                   ENDSR
067900030204     C/EJECT
068000030204     C************************************************************
068100030204     C* A chi deve trasmettere
068200030204     C************************************************************
068300030204     C     ACHI          BEGSR
068400030204     C*
068500030204     c                   eval      S_Tasto = $Tasto
068600030204     C*
068700030204     c                   move      '1'           t1trs
068800030204     c                   clear                   t1fil
068900030204     c     su_win        tag
069000030204     C                   Exfmt     W1Tras
069100030204     c                   if        t1trs = '2' and
069200030204     c                             t1fil = 0
069300030204     c                   seton                                        77
069400030204     c                   goto      su_win
069500030204     c                   end
069600030204     C*
069700030204     c                   eval      $Tasto = S_Tasto
069800030204     C*
069900030204     C                   ENDSR
070000030204     C/EJECT
070100030204     C************************************************************
070200030204    >C* AGGIORNAMENTO ANAGRAFICA
070300030204     C************************************************************
070400030204    >C     FLDAGG        BEGSR
070500030204     C*
070600030204     C*  campi chiave da scrivere e aggiornare
070700030204$007 C                   MOVE      '1'           tblKUT
070800030204     C                   movel     *blanks       tblKEY
070900030204     C                   MOVEl     D1COD         tblKEY
071000030204     c                   movel     T1trs         tblFTT
071100030204     c                   move      T1fil         tblFLT
071200030204     c                   move      *zeros        tblDTR
071300030204     c                   move      *blanks       tblFTR
071400030204     c                   move      ' '           TBLFLG
071500030204     C*
071600030204     C                   ENDSR
071700940207     C/EJECT
071800940224     C************************************************************
071900940315    >C* AGGIORNAMENTO ANAGRAFICA
072000940224     C************************************************************
072100940315    >C     AGGANA        BEGSR
072200940207     C*
072300030204     C                   exsr      FLDAGG
072400030204     C*
072500940207     C                   SELECT
072600030204     C*
072700030114    >C     xtaopz        WHENEQ    '01'
072800030114    >C     xtaopz        OREQ      '03'
072900030204     C*
073000030204     C                   move      '8A'          TBLcod
073100030204    >C                   EXSR      RIEANA_8A
073200030114     c                   write     TABEL                                59
073300940318     C* se rcd non scrivibile attivo errore
073400030114    >C   59              SETON                                          99
073500030204     C*   correlata 8F
073600030204     c                   if        *in99 = *Off
073700030204     C                   move      '8F'          TBLcod
073800030204    >C                   EXSR      RIEANA_8F
073900030204     c                   write     TABEL                                59
074000030204     C* se rcd non scrivibile attivo errore
074100030204    >C   59              SETON                                          99
074200030204     c                   end
074300030204      *
074400030114    >C     xtaopz        WHENEQ    '02'
074500030204      *
074600030204     C                   move      '8A'          TBLcod
074700030204     C     KTBL          chain     TABEL00F
074800030204     C                   exsr      FLDAGG
074900030204    >C                   EXSR      RIEANA_8A
075000030114     c                   update    TABEL                                59
075100030114    >C   59              SETON                                          99
075200030204      *
075300030204     c                   if        *in99 = *Off
075400030204     C                   move      '8F'          TBLcod
075500030204     C     KTBL          chain     TABEL00F
075600030204     c                   if        %Found(TABEL00F)
075700030204     C                   exsr      FLDAGG
075800030204    >C                   EXSR      RIEANA_8F
075900030204     c                   update    TABEL                                59
076000030204    >C   59              SETON                                          99
076100030204     c                   else
076200030204    >C                   EXSR      RIEANA_8F
076300030204     c                   write     TABEL                                59
076400030204    >C   59              SETON                                          99
076500030204     c                   end
076600030204     c                   end
076700030204      *
076800030114    >C     xtaopz        WHENEQ    '04'
076900030204     C                   move      '8A'          TBLcod
077000030204     C     KTBL          chain     TABEL00F
077100030114     c                   move      '*'           TBLFLG
077200030115     c                   if        $tasto = F23
077300030115     c                   delete    TABEL                                59
077400030115     c                   else
077500030114     c                   update    TABEL                                59
077600030115     c                   end
077700030114    >C   59              SETON                                          99
077800030204      *
077900030204     c                   if        *in99 = *Off
078000030204     C                   move      '8F'          TBLcod
078100030204     C     KTBL          chain     TABEL00F
078200030204     c                   if        %Found(TABEL00F)
078300030204     c                   move      '*'           TBLFLG
078400030204     c                   if        $tasto = F23
078500030204     c                   delete    TABEL                                59
078600030204     c                   else
078700030204     c                   update    TABEL                                59
078800030204     c                   end
078900030204    >C   59              SETON                                          99
079000030204     c                   end
079100030204     c                   else
079200030204     c                   if        $tasto = F06
079300030204     c                   move      '*'           TBLFLG
079400030204     C                   exsr      FLDAGG
079500030204    >C                   EXSR      RIEANA_8F
079600030204     c                   write     TABEL                                59
079700030204    >C   59              SETON                                          99
079800030204     c                   end
079900030204     c                   end
080000940621     C*
080100940207     C                   ENDSL
080200940203     C*
080300940203     C                   ENDSR
080400940224     C************************************************************
080500940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
080600940224     C************************************************************
080700030204     C     RIEANA_8A     BEGSR
080800030114     C*
080900030114     C                   CLEAR                   tblUNI
081000030114     C* descrizione
081100030114     c                   movel     d1DES         §8ADES
081200030114      * Autotr./Coop
081300030114     c                   if        d1fac = 'C'
081400030114     c                   movel     d1FAC         §8AFAC
081500030114     c                   else
081600030114     c                   movel     *blank        §8AFAC
081700030114     c                   end
081800030114      * Tipo Servizio
081900030114     c                   select
082000030114     c                   when      d1TSR = 'C'
082100030114     c                   eval         §8ausc = 'S'
082200030114     c                   eval         §8ausr = ' '
082300030114     c                   when      d1TSR = 'R'
082400030114     c                   eval         §8ausr = 'S'
082500030114     c                   eval         §8ausc = ' '
082600030114     c                   when      d1TSR = 'E'
082700030114     c                   eval         §8ausr = 'S'
082800030114     c                   eval         §8ausc = 'S'
082900030114     c                   when      d1TSR = 'X' or d1TSR = 'T' or
083000030114     c                             d1TSR = 'P'
083100030114     c                   eval         §8ausc = d1TSR
083200030114     c                   eval         §8ausr = ' '
083300030114     c                   endsl
083400030114      *....
083500030114      * Poste/Non poste/Entrambe
083600030114     c                   if        d1FPT = 'E'
083700030114     c                   eval         §8afpt = *blank
083800030114     c                   endIf
083900030114     c                   if        d1FPT = 'N'
084000030114     c                   eval         §8afpt = 'N'
084100030114     c                   endIf
084200030114     c                   if        d1FPT = 'P'
084300030114     c                   eval         §8afpt = 'P'
084400030114     c                   endIf
084500030114      * Base/Accessori
084600030114     c                   if        d1TBA = 'B'
084700030114     c                   eval         §8atba = 'B'
084800030114     c                   endIf
084900030114     c                   if        d1TBA = 'A'
085000030114     c                   eval         §8atba = ' '
085100030114     c                   endIf
085200030114      * Segno +/-
085300030114     c                   if        d1SPM = '-'
085400030114     c                   eval        §8aspm = '-'
085500030114     c                   endIf
085600030114     c                   if        d1SPM = '+'
085700030114     c                   eval        §8aspm = ' '
085800030114     c                   endIf
085900030114      * Sostituzione Dettaglio tariffa
086000030114     c                   eval        §8ASDT = d1SDT
086100030114      * Livello a Dettaglio
086200030114     c                   eval        §8AFDE = d1FDE
086300030114      * Livello a Prestazione
086400030114     c                   eval        §8AFPR = d1FPR
086500030114      * Livello a Giornata
086600030114     c                   eval        §8AFGI = d1FGI
086700100419      * obbligatorietà del minimo e del massimo
086800100419     c                   eval        §8aOBBmima = d1obbMIMA
086900030114      *Tipi Tariffe
087000030114      *   Dettaglio
087100030114     c                   eval        §8ATT0 = d1TT0
087200030114     c                   eval        §8ATT1 = d1TT1
087300030114     c                   eval        §8ATT2 = d1TT2
087400030114     c                   eval        §8ATT3 = d1TT3
087500030114     c                   eval        §8ATT4 = d1TT4
087600030114     c                   eval        §8ATT5 = d1TT5
087700030114      *   Prestazione
087800030114     c                   eval        §8ATT6 = d1TT6
087900030114     c                   eval        §8ATT7 = d1TT7
088000030114     c                   eval        §8ATT8 = d1TT8
088100030114     c                   eval        §8ATT9 = d1TT9
088200030114     c                   eval        §8ATTV = d1TTV
088300030114      *   Giornata
088400030114     c                   eval        §8ATB1 = d1TB1
088500030114     c                   eval        §8ATB2 = d1TB2
088600030114     c                   eval        §8ATB3 = d1TB3
088700030114     c                   eval        §8ATB4 = d1TB4
088800030114     c                   eval        §8ATB5 = d1TB5
088900030114      *   Priorità di lettura
089000030114     c                   eval        §8ALVL = d1LVL
089100030114      *   Tipi Voce
089200030114     c                   eval        §8A12E = d112E
089300030123      *   C/E   Voci x Coop  solo la 4° è valorizzata
089400030123     c                   if        d1FAC ='C'
089500030123     c                   eval      d2CE1 = *blank
089600030123     c                   eval      d2CE2 = *blank
089700030123     c                   eval      d2CE3 = *blank
089800030123     c                   end
089900030123      *   C/E
090000030115     c                   if        d2CE1 <> *blank
090100030114     c                   eval        §8ACE1 = d2CE1
090200030115     c                   else
090300030115     c                   move      *zeros        §8ACE1
090400030115     c                   end
090500030115     c                   if        d2CE2 <> *blank
090600030114     c                   eval        §8ACE2 = d2CE2
090700030115     c                   else
090800030115     c                   move      *zeros        §8ACE2
090900030115     c                   end
091000030115     c                   if        d2CE3 <> *blank
091100030114     c                   eval        §8ACE3 = d2CE3
091200030115     c                   else
091300030115     c                   move      *zeros        §8ACE3
091400030115     c                   end
091500030115     c                   if        d2CE4 <> *blank
091600030114     c                   eval        §8ACE4 = d2CE4
091700030115     c                   else
091800030115     c                   move      *zeros        §8ACE4
091900030115     c                   end
092000030114      * Forzature
092100030114     c                   if        d2FC1 ='S'
092200030114     c                   eval        §8AFC1 = d2FC1
092300030114     c                   else
092400030114     c                   eval        §8AFC1 = *blank
092500030114     c                   end
092600030114     c                   if        d2FC2 ='S'
092700030114     c                   eval        §8AFC2 = d2FC2
092800030114     c                   else
092900030114     c                   eval        §8AFC2 = *blank
093000030114     c                   end
093100030114     c                   if        d2FC3 ='S'
093200030114     c                   eval        §8AFC3 = d2FC3
093300030114     c                   else
093400030114     c                   eval        §8AFC3 = *blank
093500030114     c                   end
093600030114     c                   if        d2FC4 ='S'
093700030114     c                   eval        §8AFC4 = d2FC4
093800030114     c                   else
093900030114     c                   eval        §8AFC4 = *blank
094000030114     c                   end
094100090902      *
094200090902      * Autotr./Coop  (il No è equivqlente a Blank)
094300090902     c                   if        d1obbl = 'N'
094400090902     c                   eval      d1obbl = *blank
094500090902     c                   end
094600090902     c                   eval      §8Aobb  =  d1obbl
094700090903     c                   eval      §8Avocr =  d1vocr
094800030114     C*
094900030204     C                   MOVEL     EXTDS8A       TBLUNI
095000030114     C*
095100940207     C                   ENDSR
095200030204     C************************************************************
095300030204     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
095400030204     C************************************************************
095500030204     C     RIEANA_8F     BEGSR
095600030204     C*
095700030204     C                   CLEAR                   tblUNI
095800030204     C*
095900030204     C*  Tabella 8F  correlata
096000030204     c                   eval      §8Fdes  =  d2prt
096100030204     c                   eval      §8For1  =  d2or01
096200030204     c                   eval      §8For2  =  d2or02
096300030204     c                   eval      §8For3  =  d2or03
096400030204     C*
096500090902     C*
096600030204     C                   MOVEL     EXTDS8F       TBLUNI
096700030204     C*
096800030204     C                   ENDSR
096900940224     C************************************************************
097000940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
097100940224     C************************************************************
097200940224     C     GESAGG        BEGSR
097300940224     C*
097400940224     C* segnalo al pgm chiamante l'avvenuta conferma
097500030114    >C                   MOVE      *ON           xtaopr
097600940224     C* segnalo al pgm chiamante l'aver premuto l'F6
097700030114    >C                   MOVE      '0'           xtaret
097800940224     C*
0979009402241    C                   SELECT
098000940224     C* nel caso di immissione, ripristino la videata iniziale
098100030114    >C     xtaopz        WHENEQ    '01'
098200940224     C                   MOVE      *ON           $INZD1
098300030114     C                   MOVE      *ON           $FINE
098400940224     C* altrimenti ritorno al pgm chiamante
098500940224     C                   OTHER
098600940224     C                   MOVE      *ON           $FINE
0987009402241-   C                   ENDSL
098800940224     C*
098900940224     C                   ENDSR
099000940131     C************************************************************
099100940131     C* OPERAZIONI INIZIALI
099200940131     C************************************************************
099300940131     C     *INZSR        BEGSR
099400940131     C*
099500940127     C* Reperimento parametri
099600940127     C*
099700940117     C     *ENTRY        PLIST
099800940117     C                   PARM                    KPJBA
099900030113     C                   movel     kpjbu         tabds
100000030115     c                   setoff                                       08
100100030115     C*
100200940223     C* Variabili per gestione videate
100300940223     C                   MOVE      'D1'          $GEST
100400940223     C                   MOVE      *OFF          $FINE
100500940223     C                   MOVE      *ON           $INZD1
100600940224     C                   MOVE      *BLANK        $LASTG
100700940224     C                   MOVE      *BLANK        $LASTV
100800940506     C                   Z-ADD     0             $ULKD1            3 0
100900940127     C*
101000940223     C* Variabili appoggio
101100940224     C                   Z-ADD     0             CURR
101200940224     C                   Z-ADD     0             CURC
101300940207     C*
101400940207     C* Valorizzazione campi univoci testate
101500940614     C                   CLEAR                   T1
101600940614     C                   MOVEL     KNSIF         NOMSIF
101700030113     C******             MOVEL     XSCDSI        NOMDIT
101800941027     C                   MOVEL     DSPGM         NOMPGM
101900030113     C*
102000030113     C                   z-add     1             TBLkut
102100030113     C                   move      '8A'          TBLcod
102200030114     C                   movel     xtakey        TBLkey
102300940224     C*
102400940224     C* Inizializzazione/Decodifica prima videata
102500940224     C                   EXSR      INZD1
102600940224     C                   MOVE      *OFF          $INZD1
102700940127     C*
102800940117     C                   ENDSR
102900940128     C************************************************************
103000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
103100940223     C************************************************************
103200940223     C     DEFCAM        BEGSR
103300940223     C*
103400940223     C* klist
103500030113     C     KTBL          KLIST
103600030113     C                   KFLD                    TBLkut
103700030113     C                   KFLD                    TBlCOD
103800030113     C                   KFLD                    TBlKEY
103900940207     C*
104000940223     C* Variabili per gestione videate
104100940223     C*
104200940223     C                   MOVE      *BLANK        $GEST             2
104300940223     C                   MOVE      *BLANK        $FINE             1
104400940223     C                   MOVE      *BLANK        $INZD1            1
104500940223     C                   MOVE      *BLANK        $LASTG            2
104600940223     C                   MOVE      *BLANK        $LASTV            2
104700940223     C*
104800940223     C* Variabili appoggio
104900940223     C                   Z-ADD     0             CURR              2 0
105000940223     C                   Z-ADD     0             CURC              2 0
105100940223     C                   MOVE      *ZEROS        WIN              99
105200940224     C                   MOVE      *BLANK        WTBERR            1
105300940223     C*
105400940223     C* Indici
105500940223     C                   Z-ADD     0             X                 3 0
105600940223     C                   Z-ADD     0             Y                 3 0
105700940127     C*
105800940117     C                   ENDSR
105900940128     C************************************************************
