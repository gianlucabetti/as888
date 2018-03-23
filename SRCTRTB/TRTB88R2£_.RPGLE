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
037400030113      *Tipi Tariffe
037500030113      *   Dettaglio
037600030113     c                   eval      d1TT0 = §8ATT0
037700030113     c                   eval      d1TT1 = §8ATT1
037800030113     c                   eval      d1TT2 = §8ATT2
037900030113     c                   eval      d1TT3 = §8ATT3
038000030113     c                   eval      d1TT4 = §8ATT4
038100030113     c                   eval      d1TT5 = §8ATT5
038200030113      *   Prestazione
038300030113     c                   eval      d1TT6 = §8ATT6
038400030113     c                   eval      d1TT7 = §8ATT7
038500030113     c                   eval      d1TT8 = §8ATT8
038600030113     c                   eval      d1TT9 = §8ATT9
038700030113     c                   eval      d1TTV = §8ATTV
038800030113      *   Giornata
038900030113     c                   eval      d1TB1 = §8ATB1
039000030113     c                   eval      d1TB2 = §8ATB2
039100030113     c                   eval      d1TB3 = §8ATB3
039200030113     c                   eval      d1TB4 = §8ATB4
039300030113     c                   eval      d1TB5 = §8ATB5
039400030113      *   Priorità di lettura
039500030113     c                   eval      d1LVL = §8ALVL
039600030113      *   Tipi Voce
039700030123     c                   if        §8A12E = *blank
039800030123     c                   eval      d112E = 'E'
039900030123     c                   else
040000030113     c                   eval      d112E = §8A12E
040100030123     c                   end
040200030113      *   C/E
040300030115     c                   if        §8ACE1 <> '000'
040400030114     c                   eval      d2CE1 = §8ACE1
040500030115     c                   else
040600030115     c                   eval      d2CE1 = *blank
040700030115     c                   end
040800030115     c                   if        §8ACE2 <> '000'
040900030114     c                   eval      d2CE2 = §8ACE2
041000030115     c                   else
041100030115     c                   eval      d2CE2 = *blank
041200030115     c                   end
041300030115     c                   if        §8ACE3 <> '000'
041400030114     c                   eval      d2CE3 = §8ACE3
041500030115     c                   else
041600030115     c                   eval      d2CE3 = *blank
041700030115     c                   end
041800030115     c                   if        §8ACE4 <> '000'
041900030114     c                   eval      d2CE4 = §8ACE4
042000030115     c                   else
042100030115     c                   eval      d2CE4 = *blank
042200030115     c                   end
042300030114      * forzature
042400030114     c                   if        d2FC1 <> *blank
042500030114     c                   eval      d2FC1 = §8AFC1
042600030114     c                   else
042700030114     c                   eval      d2FC1 = 'N'
042800030114     c                   end
042900030114     c                   if        d2FC2 <> *blank
043000030114     c                   eval      d2FC2 = §8AFC2
043100030114     c                   else
043200030114     c                   eval      d2FC2 = 'N'
043300030114     c                   end
043400030114     c                   if        d2FC3 <> *blank
043500030114     c                   eval      d2FC3 = §8AFC3
043600030114     c                   else
043700030114     c                   eval      d2FC3 = 'N'
043800030114     c                   end
043900030114     c                   if        d2FC4 <> *blank
044000030114     c                   eval      d2FC4 = §8AFC4
044100030114     c                   else
044200030114     c                   eval      d2FC4 = 'N'
044300030114     c                   end
044400030204     C*
044500030204     C*  Tabella 8F  correlata
044600030204     c                   eval      d2prt  = §8Fdes
044700030204     C*
044800030204     c                   if        §8For1 <> *blank
044900030204     c                   eval      d2or01 = §8For1
045000030204     c                   else
045100030204     c                   eval      d2or01 = '00'
045200030204     c                   end
045300940207     C*
045400030204     c                   if        §8For2 <> *blank
045500030204     c                   eval      d2or02 = §8For2
045600030204     c                   else
045700030204     c                   eval      d2or02 = '00'
045800030204     c                   end
045900030204     C*
046000030204     c                   if        §8For3 <> *blank
046100030204     c                   eval      d2or03 = §8For3
046200030204     c                   else
046300030204     c                   eval      d2or03 = '00'
046400030204     c                   end
046500030204     C*
046600940207     C                   ENDSR
046700940309     C************************************************************
046800940309     C* GESTIONE F03 VIDEO D1
046900940309     C************************************************************
047000940309     C     F03D1         BEGSR
047100940309     C*
047200940309     C                   MOVE      *ON           $FINE
047300030113    >C                   MOVE      '1'           xtaret
047400940325     C*
047500940325     C                   ENDSR
047600940309     C************************************************************
047700940309     C* GESTIONE F12 VIDEO D1
047800940309     C************************************************************
047900940309     C     F12D1         BEGSR
048000940309     C*
048100940309     C                   MOVE      *ON           $FINE
048200940309     C*
048300940309     C                   ENDSR
048400030204     C************************************************************
048500030204     C* GESTIONE F04 VIDEO D2
048600030204     C************************************************************
048700030204     C     F04D2         BEGSR
048800030204     C*
048900030204     c                   exfmt     D2F4
049000030204     C*
049100030204     C                   ENDSR
049200940131     C************************************************************
049300940207     C* CONTROLLO VIDEATA
049400940131     C************************************************************
049500940127     C     CTRD1         BEGSR
049600940131     C*
049700940127     C                   SETOFF                                       99
049800030123     C*
049900030123     C                   SETOFF                                       09
050000030123     C* SE COOPERATIVA
050100030123$017 C     D1FAC         IFEQ      'C'
050200030123     C                   SETON                                        09
050300030123     C                   End
050400030123      *
050500940127     C* non imposto pos. del cursore
050600940127     C                   Z-ADD     0             H1RIGA
050700940127     C                   Z-ADD     0             H1COLO
050800940621     C*
050900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0510000301141   >C     xtaopz        IFNE      '04'
051100971110     C*
051200971110     C* controllo codice vuoto
051300971110$017 C     D1COD         IFEQ      *ALL' '
051400971110     C                   SETON                                        5099
051500030114     C                   Else
051600030114     c                   eval      d2cod = d1cod
051700971110     C                   ENDIF
051800030114     C*
051900030114     C* controllo codice vuoto
052000030114$017 C     D1DES         IFEQ      *ALL' '
052100030114     C                   SETON                                        5199
052200030114     C                   Else
052300030114     c                   eval      d2des = d1des
052400030114     C                   ENDIF
052500940207     C*
052600940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
052700030114    >C     xtaopz        IFEQ      '01'
052800030114    >C     xtaopz        oreq      '03'
052900030114     c                   movel(p)  d1cod         tblkey
053000030114     C     KTBL          chain(N)  TABEL00F
053100030114     c                   if        %Found(TABEL00F)
053200030114     C                   SETON                                        5299
053300030114     C                   EndIF
053400030114     C                   ENDIF
053500030114     C*
053600030114     C* controllo codice vuoto
053700030114$017 C     D1FAC         IFEQ      *ALL' '
053800030114     C                   SETON                                        5399
053900030114     C                   End
054000030114      *
054100030114     C* controllo codice vuoto
054200070212$017 C**** D1TSR         IFEQ      *ALL' '
054300070212     C****               SETON                                        5499
054400070212     C****               End
054500030114      *
054600030115$017 C     D1FAC         IFEQ      'A'
054700030115$017 C     D1TSR         IFEQ      'X'
054800030115$017 C     D1TSR         OREQ      'T'
054900030115$017 C     D1TSR         OREQ      'P'
055000030115     C                   SETON                                        6599
055100030115     C                   End
055200030115     C                   End
055300030115      *
055400030115$017 C     D1FAC         IFEQ      'C'
055500030115$017 C     D1TSR         IFEQ      'C'
055600030115$017 C     D1TSR         OREQ      'R'
055700030115$017 C     D1TSR         OREQ      'E'
055800030115     C                   SETON                                        6599
055900030115     C                   End
056000030115     C                   End
056100030115      *
056200030114     C* controllo codice vuoto
056300030114$017 C     D1FPT         IFEQ      *ALL' '
056400030114     C                   SETON                                        5599
056500030114     C                   End
056600030114      *
056700030114     C* controllo codice vuoto
056800030114$017 C     D1TBA         IFEQ      *ALL' '
056900030114     C                   SETON                                        5699
057000030114     C                   End
057100030114      *
057200030114     C* controllo codice vuoto
057300030114$017 C     D1SPM         IFEQ      *ALL' '
057400030114     C                   SETON                                        5799
057500030114     C                   End
057600030114      *
057700030114     C* controllo codice vuoto
057800030114$017 C     D1SDT         IFEQ      *ALL' '
057900030114     C                   SETON                                        5899
058000030114     C                   End
058100030114      *
058200030114     C* controllo codice vuoto
058300030114$017 C     D1FDE         IFEQ      *ALL' '
058400030114$017 C     D1FPR         andeq     *ALL' '
058500030114$017 C     D1FGI         andeq     *ALL' '
058600030114     C                   SETON                                        5999
058700030114     C                   End
058800030114      *
058900030114     C* controllo codice vuoto
059000030114$017 C     D1FDE         IFne      *ALL' '
059100030114$017 C     D1TT0         andeq     *ALL' '
059200030114     C                   SETON                                        6099
059300030114     C                   End
059400030114      *
059500030114     C* controllo codice vuoto
059600030114$017 C     D1FPR         IFne      *ALL' '
059700030114$017 C     D1TT6         andeq     *ALL' '
059800030114     C                   SETON                                        6199
059900030114     C                   End
060000030114      *
060100030114     C* controllo codice vuoto
060200030114$017 C     D1FGI         IFne      *ALL' '
060300030114$017 C     D1TB1         andeq     *ALL' '
060400030114     C                   SETON                                        6299
060500030114     C                   End
060600030114      *
060700030114     C* controllo codice vuoto
060800030114$017 C     D1LVL         IFEQ      *ALL' '
060900030114     C                   SETON                                        6399
061000030114     C                   End
061100030114      *
061200030114     C* controllo codice vuoto
061300030123$017 C     D112E         IFEQ      *ALL' '
061400030123     c                   SETON                                        6499
061500030123     C                   End
061600030114      *
0617009402241-   C                   END
061800940131     C*
061900940302     C     *IN99         IFEQ      *ON
062000940510     C     $GEST         ANDEQ     'D1'
062100940117     C                   MOVEA     *IN           WIN
062200940128     C                   MOVE      *ALL'0'       IN5098           49
062300940128     C                   MOVEA     IN5098        *IN(50)
062400940614     C                   WRITE     D1
062500940117     C                   MOVEA     WIN           *IN
062600940302     C                   ENDIF
062700940127     C*
062800940117     C                   ENDSR
062900960305     C/EJECT
063000960305     C************************************************************
063100960305     C* CONTROLLO VIDEATA D2
063200960305     C************************************************************
063300960305     C     CTRD2         BEGSR
063400030205     C*
063500030205     C* controllo codice vuoto ordinamento in stampa
063600030205$017 C     d2or01        IFEQ      *ALL' '
063700030205$017 C     d2or01        oreq      *ALL'0'
063800030205$017 C                   if        d2or02 <> '00' and d2or02 <> '  ' or
063900030205$017 C                             d2or03 <> '00' and d2or03 <> '  '
064000030205     C                   SETON                                        6699
064100030205     C                   End
064200030205     C                   End
064300960305     C*
064400030205$017 C     d2or02        IFEQ      *ALL' '
064500030205$017 C     d2or02        oreq      *ALL'0'
064600030205$017 C                   if        d2or01 <> '00' and d2or01 <> '  ' or
064700030205$017 C                             d2or03 <> '00' and d2or03 <> '  '
064800030205     C                   SETON                                        6799
064900030205     C                   End
065000030205     C                   End
065100030205     C*
065200030205$017 C     d2or03        IFEQ      *ALL' '
065300030205$017 C     d2or03        oreq      *ALL'0'
065400030205$017 C                   if        d2or01 <> '00' and d2or01 <> '  ' or
065500030205$017 C                             d2or02 <> '00' and d2or02 <> '  '
065600030205     C                   SETON                                        6899
065700030205     C                   End
065800030205     C                   End
065900030205     C*
066000960305     C                   ENDSR
066100030204     C/EJECT
066200030204     C************************************************************
066300030204     C* A chi deve trasmettere
066400030204     C************************************************************
066500030204     C     ACHI          BEGSR
066600030204     C*
066700030204     c                   eval      S_Tasto = $Tasto
066800030204     C*
066900030204     c                   move      '1'           t1trs
067000030204     c                   clear                   t1fil
067100030204     c     su_win        tag
067200030204     C                   Exfmt     W1Tras
067300030204     c                   if        t1trs = '2' and
067400030204     c                             t1fil = 0
067500030204     c                   seton                                        77
067600030204     c                   goto      su_win
067700030204     c                   end
067800030204     C*
067900030204     c                   eval      $Tasto = S_Tasto
068000030204     C*
068100030204     C                   ENDSR
068200030204     C/EJECT
068300030204     C************************************************************
068400030204    >C* AGGIORNAMENTO ANAGRAFICA
068500030204     C************************************************************
068600030204    >C     FLDAGG        BEGSR
068700030204     C*
068800030204     C*  campi chiave da scrivere e aggiornare
068900030204$007 C                   MOVE      '1'           tblKUT
069000030204     C                   movel     *blanks       tblKEY
069100030204     C                   MOVEl     D1COD         tblKEY
069200030204     c                   movel     T1trs         tblFTT
069300030204     c                   move      T1fil         tblFLT
069400030204     c                   move      *zeros        tblDTR
069500030204     c                   move      *blanks       tblFTR
069600030204     c                   move      ' '           TBLFLG
069700030204     C*
069800030204     C                   ENDSR
069900940207     C/EJECT
070000940224     C************************************************************
070100940315    >C* AGGIORNAMENTO ANAGRAFICA
070200940224     C************************************************************
070300940315    >C     AGGANA        BEGSR
070400940207     C*
070500030204     C                   exsr      FLDAGG
070600030204     C*
070700940207     C                   SELECT
070800030204     C*
070900030114    >C     xtaopz        WHENEQ    '01'
071000030114    >C     xtaopz        OREQ      '03'
071100030204     C*
071200030204     C                   move      '8A'          TBLcod
071300030204    >C                   EXSR      RIEANA_8A
071400030114     c                   write     TABEL                                59
071500940318     C* se rcd non scrivibile attivo errore
071600030114    >C   59              SETON                                          99
071700030204     C*   correlata 8F
071800030204     c                   if        *in99 = *Off
071900030204     C                   move      '8F'          TBLcod
072000030204    >C                   EXSR      RIEANA_8F
072100030204     c                   write     TABEL                                59
072200030204     C* se rcd non scrivibile attivo errore
072300030204    >C   59              SETON                                          99
072400030204     c                   end
072500030204      *
072600030114    >C     xtaopz        WHENEQ    '02'
072700030204      *
072800030204     C                   move      '8A'          TBLcod
072900030204     C     KTBL          chain     TABEL00F
073000030204     C                   exsr      FLDAGG
073100030204    >C                   EXSR      RIEANA_8A
073200030114     c                   update    TABEL                                59
073300030114    >C   59              SETON                                          99
073400030204      *
073500030204     c                   if        *in99 = *Off
073600030204     C                   move      '8F'          TBLcod
073700030204     C     KTBL          chain     TABEL00F
073800030204     c                   if        %Found(TABEL00F)
073900030204     C                   exsr      FLDAGG
074000030204    >C                   EXSR      RIEANA_8F
074100030204     c                   update    TABEL                                59
074200030204    >C   59              SETON                                          99
074300030204     c                   else
074400030204    >C                   EXSR      RIEANA_8F
074500030204     c                   write     TABEL                                59
074600030204    >C   59              SETON                                          99
074700030204     c                   end
074800030204     c                   end
074900030204      *
075000030114    >C     xtaopz        WHENEQ    '04'
075100030204     C                   move      '8A'          TBLcod
075200030204     C     KTBL          chain     TABEL00F
075300030114     c                   move      '*'           TBLFLG
075400030115     c                   if        $tasto = F23
075500030115     c                   delete    TABEL                                59
075600030115     c                   else
075700030114     c                   update    TABEL                                59
075800030115     c                   end
075900030114    >C   59              SETON                                          99
076000030204      *
076100030204     c                   if        *in99 = *Off
076200030204     C                   move      '8F'          TBLcod
076300030204     C     KTBL          chain     TABEL00F
076400030204     c                   if        %Found(TABEL00F)
076500030204     c                   move      '*'           TBLFLG
076600030204     c                   if        $tasto = F23
076700030204     c                   delete    TABEL                                59
076800030204     c                   else
076900030204     c                   update    TABEL                                59
077000030204     c                   end
077100030204    >C   59              SETON                                          99
077200030204     c                   end
077300030204     c                   else
077400030204     c                   if        $tasto = F06
077500030204     c                   move      '*'           TBLFLG
077600030204     C                   exsr      FLDAGG
077700030204    >C                   EXSR      RIEANA_8F
077800030204     c                   write     TABEL                                59
077900030204    >C   59              SETON                                          99
078000030204     c                   end
078100030204     c                   end
078200940621     C*
078300940207     C                   ENDSL
078400940203     C*
078500940203     C                   ENDSR
078600940224     C************************************************************
078700940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
078800940224     C************************************************************
078900030204     C     RIEANA_8A     BEGSR
079000030114     C*
079100030114     C                   CLEAR                   tblUNI
079200030114     C* descrizione
079300030114     c                   movel     d1DES         §8ADES
079400030114      * Autotr./Coop
079500030114     c                   if        d1fac = 'C'
079600030114     c                   movel     d1FAC         §8AFAC
079700030114     c                   else
079800030114     c                   movel     *blank        §8AFAC
079900030114     c                   end
080000030114      * Tipo Servizio
080100030114     c                   select
080200030114     c                   when      d1TSR = 'C'
080300030114     c                   eval         §8ausc = 'S'
080400030114     c                   eval         §8ausr = ' '
080500030114     c                   when      d1TSR = 'R'
080600030114     c                   eval         §8ausr = 'S'
080700030114     c                   eval         §8ausc = ' '
080800030114     c                   when      d1TSR = 'E'
080900030114     c                   eval         §8ausr = 'S'
081000030114     c                   eval         §8ausc = 'S'
081100030114     c                   when      d1TSR = 'X' or d1TSR = 'T' or
081200030114     c                             d1TSR = 'P'
081300030114     c                   eval         §8ausc = d1TSR
081400030114     c                   eval         §8ausr = ' '
081500030114     c                   endsl
081600030114      *....
081700030114      * Poste/Non poste/Entrambe
081800030114     c                   if        d1FPT = 'E'
081900030114     c                   eval         §8afpt = *blank
082000030114     c                   endIf
082100030114     c                   if        d1FPT = 'N'
082200030114     c                   eval         §8afpt = 'N'
082300030114     c                   endIf
082400030114     c                   if        d1FPT = 'P'
082500030114     c                   eval         §8afpt = 'P'
082600030114     c                   endIf
082700030114      * Base/Accessori
082800030114     c                   if        d1TBA = 'B'
082900030114     c                   eval         §8atba = 'B'
083000030114     c                   endIf
083100030114     c                   if        d1TBA = 'A'
083200030114     c                   eval         §8atba = ' '
083300030114     c                   endIf
083400030114      * Segno +/-
083500030114     c                   if        d1SPM = '-'
083600030114     c                   eval        §8aspm = '-'
083700030114     c                   endIf
083800030114     c                   if        d1SPM = '+'
083900030114     c                   eval        §8aspm = ' '
084000030114     c                   endIf
084100030114      * Sostituzione Dettaglio tariffa
084200030114     c                   eval        §8ASDT = d1SDT
084300030114      * Livello a Dettaglio
084400030114     c                   eval        §8AFDE = d1FDE
084500030114      * Livello a Prestazione
084600030114     c                   eval        §8AFPR = d1FPR
084700030114      * Livello a Giornata
084800030114     c                   eval        §8AFGI = d1FGI
084900030114      *Tipi Tariffe
085000030114      *   Dettaglio
085100030114     c                   eval        §8ATT0 = d1TT0
085200030114     c                   eval        §8ATT1 = d1TT1
085300030114     c                   eval        §8ATT2 = d1TT2
085400030114     c                   eval        §8ATT3 = d1TT3
085500030114     c                   eval        §8ATT4 = d1TT4
085600030114     c                   eval        §8ATT5 = d1TT5
085700030114      *   Prestazione
085800030114     c                   eval        §8ATT6 = d1TT6
085900030114     c                   eval        §8ATT7 = d1TT7
086000030114     c                   eval        §8ATT8 = d1TT8
086100030114     c                   eval        §8ATT9 = d1TT9
086200030114     c                   eval        §8ATTV = d1TTV
086300030114      *   Giornata
086400030114     c                   eval        §8ATB1 = d1TB1
086500030114     c                   eval        §8ATB2 = d1TB2
086600030114     c                   eval        §8ATB3 = d1TB3
086700030114     c                   eval        §8ATB4 = d1TB4
086800030114     c                   eval        §8ATB5 = d1TB5
086900030114      *   Priorità di lettura
087000030114     c                   eval        §8ALVL = d1LVL
087100030114      *   Tipi Voce
087200030114     c                   eval        §8A12E = d112E
087300030123      *   C/E   Voci x Coop  solo la 4° è valorizzata
087400030123     c                   if        d1FAC ='C'
087500030123     c                   eval      d2CE1 = *blank
087600030123     c                   eval      d2CE2 = *blank
087700030123     c                   eval      d2CE3 = *blank
087800030123     c                   end
087900030123      *   C/E
088000030115     c                   if        d2CE1 <> *blank
088100030114     c                   eval        §8ACE1 = d2CE1
088200030115     c                   else
088300030115     c                   move      *zeros        §8ACE1
088400030115     c                   end
088500030115     c                   if        d2CE2 <> *blank
088600030114     c                   eval        §8ACE2 = d2CE2
088700030115     c                   else
088800030115     c                   move      *zeros        §8ACE2
088900030115     c                   end
089000030115     c                   if        d2CE3 <> *blank
089100030114     c                   eval        §8ACE3 = d2CE3
089200030115     c                   else
089300030115     c                   move      *zeros        §8ACE3
089400030115     c                   end
089500030115     c                   if        d2CE4 <> *blank
089600030114     c                   eval        §8ACE4 = d2CE4
089700030115     c                   else
089800030115     c                   move      *zeros        §8ACE4
089900030115     c                   end
090000030114      * Forzature
090100030114     c                   if        d2FC1 ='S'
090200030114     c                   eval        §8AFC1 = d2FC1
090300030114     c                   else
090400030114     c                   eval        §8AFC1 = *blank
090500030114     c                   end
090600030114     c                   if        d2FC2 ='S'
090700030114     c                   eval        §8AFC2 = d2FC2
090800030114     c                   else
090900030114     c                   eval        §8AFC2 = *blank
091000030114     c                   end
091100030114     c                   if        d2FC3 ='S'
091200030114     c                   eval        §8AFC3 = d2FC3
091300030114     c                   else
091400030114     c                   eval        §8AFC3 = *blank
091500030114     c                   end
091600030114     c                   if        d2FC4 ='S'
091700030114     c                   eval        §8AFC4 = d2FC4
091800030114     c                   else
091900030114     c                   eval        §8AFC4 = *blank
092000030114     c                   end
092100030114     C*
092200030204     C                   MOVEL     EXTDS8A       TBLUNI
092300030114     C*
092400940207     C                   ENDSR
092500030204     C************************************************************
092600030204     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
092700030204     C************************************************************
092800030204     C     RIEANA_8F     BEGSR
092900030204     C*
093000030204     C                   CLEAR                   tblUNI
093100030204     C*
093200030204     C*  Tabella 8F  correlata
093300030204     c                   eval      §8Fdes  =  d2prt
093400030204     c                   eval      §8For1  =  d2or01
093500030204     c                   eval      §8For2  =  d2or02
093600030204     c                   eval      §8For3  =  d2or03
093700030204     C*
093800030204     C                   MOVEL     EXTDS8F       TBLUNI
093900030204     C*
094000030204     C                   ENDSR
094100940224     C************************************************************
094200940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
094300940224     C************************************************************
094400940224     C     GESAGG        BEGSR
094500940224     C*
094600940224     C* segnalo al pgm chiamante l'avvenuta conferma
094700030114    >C                   MOVE      *ON           xtaopr
094800940224     C* segnalo al pgm chiamante l'aver premuto l'F6
094900030114    >C                   MOVE      '0'           xtaret
095000940224     C*
0951009402241    C                   SELECT
095200940224     C* nel caso di immissione, ripristino la videata iniziale
095300030114    >C     xtaopz        WHENEQ    '01'
095400940224     C                   MOVE      *ON           $INZD1
095500030114     C                   MOVE      *ON           $FINE
095600940224     C* altrimenti ritorno al pgm chiamante
095700940224     C                   OTHER
095800940224     C                   MOVE      *ON           $FINE
0959009402241-   C                   ENDSL
096000940224     C*
096100940224     C                   ENDSR
096200940131     C************************************************************
096300940131     C* OPERAZIONI INIZIALI
096400940131     C************************************************************
096500940131     C     *INZSR        BEGSR
096600940131     C*
096700940127     C* Reperimento parametri
096800940127     C*
096900940117     C     *ENTRY        PLIST
097000940117     C                   PARM                    KPJBA
097100030113     C                   movel     kpjbu         tabds
097200030115     c                   setoff                                       08
097300030115     C*
097400940223     C* Variabili per gestione videate
097500940223     C                   MOVE      'D1'          $GEST
097600940223     C                   MOVE      *OFF          $FINE
097700940223     C                   MOVE      *ON           $INZD1
097800940224     C                   MOVE      *BLANK        $LASTG
097900940224     C                   MOVE      *BLANK        $LASTV
098000940506     C                   Z-ADD     0             $ULKD1            3 0
098100940127     C*
098200940223     C* Variabili appoggio
098300940224     C                   Z-ADD     0             CURR
098400940224     C                   Z-ADD     0             CURC
098500940207     C*
098600940207     C* Valorizzazione campi univoci testate
098700940614     C                   CLEAR                   T1
098800940614     C                   MOVEL     KNSIF         NOMSIF
098900030113     C******             MOVEL     XSCDSI        NOMDIT
099000941027     C                   MOVEL     DSPGM         NOMPGM
099100030113     C*
099200030113     C                   z-add     1             TBLkut
099300030113     C                   move      '8A'          TBLcod
099400030114     C                   movel     xtakey        TBLkey
099500940224     C*
099600940224     C* Inizializzazione/Decodifica prima videata
099700940224     C                   EXSR      INZD1
099800940224     C                   MOVE      *OFF          $INZD1
099900940127     C*
100000940117     C                   ENDSR
100100940128     C************************************************************
100200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
100300940223     C************************************************************
100400940223     C     DEFCAM        BEGSR
100500940223     C*
100600940223     C* klist
100700030113     C     KTBL          KLIST
100800030113     C                   KFLD                    TBLkut
100900030113     C                   KFLD                    TBlCOD
101000030113     C                   KFLD                    TBlKEY
101100940207     C*
101200940223     C* Variabili per gestione videate
101300940223     C*
101400940223     C                   MOVE      *BLANK        $GEST             2
101500940223     C                   MOVE      *BLANK        $FINE             1
101600940223     C                   MOVE      *BLANK        $INZD1            1
101700940223     C                   MOVE      *BLANK        $LASTG            2
101800940223     C                   MOVE      *BLANK        $LASTV            2
101900940223     C*
102000940223     C* Variabili appoggio
102100940223     C                   Z-ADD     0             CURR              2 0
102200940223     C                   Z-ADD     0             CURC              2 0
102300940223     C                   MOVE      *ZEROS        WIN              99
102400940224     C                   MOVE      *BLANK        WTBERR            1
102500940223     C*
102600940223     C* Indici
102700940223     C                   Z-ADD     0             X                 3 0
102800940223     C                   Z-ADD     0             Y                 3 0
102900940127     C*
103000940117     C                   ENDSR
103100940128     C************************************************************
