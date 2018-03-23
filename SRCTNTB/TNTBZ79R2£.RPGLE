000100130117     H DFTACTGRP(*NO) BNDDIR('PRNPGM') ACTGRP('QILE')
000200130117     H DECEDIT('0,') DATEDIT(*DMY.)
000300940223      *
000400940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000500940620      *  03           PROTECT TUTTI I CAMPI
000600030123      *  09           PROTECT dei campi voce C/E
000700940223      *  21           GENERICO OPERAZIONI I/O
000800940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000900940223      *  30           SFLDSP
001000940223      * N31           SFLCLR
001100940128      *  31           SFLDSPCTL
001200940128      *  32           SFLNXTCHG
001300940128      *  33           SFLEND
001400940128      *  39           OF PRTF
001500940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001600940317    > *  Specificare l'uso dei singoli indicatori
001700940315      *  50 <---> 98  ERRORI SU VIDEO
001800940317    > *  Specificare l'uso dei singoli indicatori
001900940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002100940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002200940128      *  99           INDIC. GENERALE DI ERRORE
002300940128     F*----------------------------------------------------*
002400130115$001 FTNTBZ79D2 CF   E             WORKSTN
002500940201     F                                     INFDS(DSFMT)
002600110927     Ftntbe01l  UF A E           K DISK
002700130117     FazORG01l  iF   E           K DISK
002800940128     D*----------------------------------------------------*
002900130115$003 D EXTDS         E DS                  EXTNAME(tntbZ79ds)
003000030113      *-------------
003100940127     D* Passaggio Parametri
003200940127     D KPJBA         E DS
003300130116     D*-------------
003400130116     d dataiso         s               d   datfmt(*iso)
003500130116     d udateiso        s               d   datfmt(*iso)
003600130116     d dataeur         s               d   datfmt(*eur)
003700130117     d oggi            s              8  0
003800130117     d wdat            s              8  0
003900130117     d wSOC            s              3A
004000130117     d wErrore         s              6A
004100130116      *
004200130116     D WLBDA8          DS
004300130116     D  G02DAT                 1      8  0
004400130116     D  G02INV                 9     16  0
004500130116     D  G02ERR                17     17
004600130116     D  G02TGI                18     22  0
004700130116      *
004800030113      *-------------
004900030113     D* Parametri in ricezione
005000030113     D  TABDS          DS
005100030113     D  XTAOPZ                 1      2
005200030113     D  XTARET                 3      3
005300030113     D  XTAOPR                 4      4
005400030113     D  XTAERR                 5      5
005500110926     D  XTAKEY                 6      8
005600130115     D  XTAKEY2                9     11
005700940201     D*-------------
005800030204     D  S_TASTO        S                   like($Tasto)
005900940201     D DSFMT           DS
006000940506     D  $TASTO               369    369
006100940201     D  NRG                  370    370
006200940201     D  NCL                  371    371
006300940201     D*-------------
006400940201     D* posizione cursore
006500940201     D CURSOR          DS
006600940223     D  RIRI                   1      2B 0 INZ
006700940201     D  R$$                    2      2
006800940223     D  COCO                   3      4B 0 INZ
006900940201     D  C$$                    4      4
007000940207     D*-------------
007100940207     D* Reperimento nome PGM
007200940207     D STATUS         SDS           333
007300940207     D  DSPGM            *PROC
007400940225     D*-------------
007500940225     D* COSTANTI
007600940225     D*-------------
007700940506     D* Tasti di funzione
007800940506     D F01             C                   CONST(X'31')
007900940506     D F02             C                   CONST(X'32')
008000940506     D F03             C                   CONST(X'33')
008100940506     D F04             C                   CONST(X'34')
008200940506     D F05             C                   CONST(X'35')
008300940506     D F06             C                   CONST(X'36')
008400940506     D F07             C                   CONST(X'37')
008500940506     D F08             C                   CONST(X'38')
008600940506     D F09             C                   CONST(X'39')
008700940506     D F10             C                   CONST(X'3A')
008800940506     D F11             C                   CONST(X'3B')
008900940506     D F12             C                   CONST(X'3C')
009000940506     D F13             C                   CONST(X'B1')
009100940506     D F14             C                   CONST(X'B2')
009200940506     D F15             C                   CONST(X'B3')
009300940506     D F16             C                   CONST(X'B4')
009400940506     D F17             C                   CONST(X'B5')
009500940506     D F18             C                   CONST(X'B6')
009600940506     D F19             C                   CONST(X'B7')
009700940506     D F20             C                   CONST(X'B8')
009800940506     D F21             C                   CONST(X'B9')
009900940506     D F22             C                   CONST(X'BA')
010000940506     D F23             C                   CONST(X'BB')
010100940506     D F24             C                   CONST(X'BC')
010200940506     D ENTER           C                   CONST(X'F1')
010300940506     D ROLDWN          C                   CONST(X'F4')
010400940506     D ROLLUP          C                   CONST(X'F5')
010500130117     D digits          C                   CONST('0123456789')
010600940506     D*-------------
010700941108     D DATA            C                   CONST('0001-01-01')
010800030114     D Annull          C                   CONST('  ANNULLATO  ')
010900030114     D OPz_Imm         C                   CONST('IMMISSIONE')
011000030114     D OPz_Var         C                   CONST('VARIAZIONE')
011100030114     D OPz_Del         C                   CONST(' ANNULLA  ')
011200030114     D OPz_Vis         C                   CONST('VISUALIZZA')
011300130117     ***************************************************************************
011400130117     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
011500130117     D ESITO_OK...
011600130117     D                 C                   0
011700130117     ***************************************************************************
011800130117     **
011900130117     ** Dichiarazione prototipi procedure esterne.
012000130117     **
012100130117     ***************************************************************************
012200130117      /DEFINE DFTACTGRP_YES
012300130117     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
012400130117      /UNDEFINE DFTACTGRP_YES
012500130117     ***************************************************************************
012600130117     **
012700130117     ** Definizione strutture dati.
012800130117     **
012900130117      **************************************************************************
013000130117     D tibsSocI0...
013100130117     D               E DS                  QUALIFIED
013200130117     D                                     INZ
013300130117     D tibsSocO0...
013400130117     D               E DS                  QUALIFIED
013500130117     D                                     INZ
013600130117     ***************************************************************************
013700130117     **
013800130117     ** Definizione variabili modulo/programma.
013900130117     **
014000130117     ***************************************************************************
014100130117     D prmRqsOpCode...
014200130117     D                 S             10A
014300130117     D prmRpyOpCode...
014400130117     D                 S             10A
014500130117     D prmRpyIdMsg...
014600130117     D                 S             10I 0
014700130117     D prmRqsFormato...
014800130117     D                 S             10A
014900130117     D prmRqsDataSize...
015000130117     D                 S             10I 0
015100130117     D prmRpyFormato...
015200130117     D                 S             10A
015300130117     D prmRpyDataSize...
015400130117     D                 S             10I 0
015500130117
015600940127     C*----------------------------------------------------*
015700940127     C*                MAIN LINE PROGRAM
015800940127     C*----------------------------------------------------*
015900130117     C*
016000940223     C*
016100940223     C     $FINE         DOWEQ     *OFF
016200940202     C     $GEST         CASEQ     'D1'          GESD1
016300940117     C                   END
016400940117     C                   END
016500130117     C*
016600940325     C* fine programma
016700030113     c                   movel     tabds         kpjbu
016800030113     C                   Seton                                        LR
016900940131     C************************************************************
017000940510     C* GESTIONE VIDEO RECORD D1
017100940131     C************************************************************
017200940127     C     GESD1         BEGSR
017300030113      *
017400940225     C* inizializzazione videata
017500940223     C     $INZD1        IFEQ      *ON
017600940127     C                   EXSR      INZD1
017700940223     C                   MOVE      *OFF          $INZD1
017800940117     C                   END
017900030113      *
018000940225     C* emissione piede videata se proveniente da altra
018100940225     C* salvataggio valore formato di provenienza
018200940207     C     $LASTV        IFNE      'D1'
018300940614     C                   WRITE     T1
018400940202     C                   MOVE      $LASTV        $LASTG
018500940202     C                   MOVE      'D1'          $LASTV
018600940202     C                   END
018700940131     C*
018800940203     C                   SELECT
018900940203     C* Annullamento
019000030114     C* Visualizzazione
019100030114    >C     xtaopz        WHENEQ    '01'
019200030114     c                   movel     Opz_Imm       D1Fun
019300030114    >C     xtaopz        WHENEQ    '05'
019400030114     c                   movel     Opz_Vis       D1Fun
019500030114     C                   SETON                                        020306
019600030114    >C     xtaopz        WHENEQ    '04'
019700030114     c                   movel     Opz_Del       D1Fun
019800110902     C                   SETON                                        02
019900030114     C                   SETOFF                                         06
020000940620     C* Manutenzione
020100030113    >C     xtaopz        WHENEQ    '02'
020200030114     c                   movel     Opz_Var       D1Fun
020300030114     C                   SETON                                        02
020400030114     C                   SETOFF                                       0306
020500940620     C*
020600940620     C                   OTHER
020700940620     C*
020800030114     C                   SETOFF                                       020306
020900940203     C                   ENDSL
021000030114     C*
021100030114     c                   SETOFF                                         99
021200940620     C* Immissione/Modifica/Copia
021300030113     C*               *----------------*
021400940620     C                   EXFMT     D1
021500030113     C*               *----------------*
021600030114     C*
021700030114     C     *IN99         IFEQ      *OFF
021800030114     C*
0219000301141    C                   SELECT
022000030114     C* F3=Fine
022100030114     C     $TASTO        WHENEQ    F03
022200030114     C                   EXSR      F03D1
022300030114     C* F12=Ritorno
022400030114     C     $TASTO        WHENEQ    F12
022500030114     C                   EXSR      F12D1
022600030114     C*
0227000301141O   C                   OTHER
022800030114     C*
022900030114     C                   EXSR      CTRD1
023000030113     C*
023100940616     C     *IN99         IFEQ      *OFF
023200940616     C* F6
0233000302052    C     $TASTO        ifeq      F06
0234000301152    C     $TASTO        oreq      F23
023500030204     C* A chi Trasmette la Tabella ?
023600110901     c****               Exsr      ACHI
023700940224     C* eseguo aggiornamento
023800940315    >C                   EXSR      AGGANA
023900940224     C* eseguo operazioni del dopo-aggiornamento
024000030114     C  N99              EXSR      GESAGG
024100030205     c                   end
024200030114     C*
024300030114     C                   ENDIF
024400030114     C*
0245000301142-   C                   ENDSL
024600030114     C*
0247000301142-   C                   ENDIF
024800030114     C*
024900940117     C                   ENDSR
025000940117      ************************************************************
025100940207      * INIZIALIZZAZIONE VIDEATA DATI
025200940117      ************************************************************
025300940127     C     INZD1         BEGSR
025400940131     C*
025500940615     C                   CLEAR                   D1
025600130115     C                   move      'Z79'         tbeCOD
025700030113     C                   MOVEl     xtakey        D1COD
025800130115     C                   MOVEl     xtakey2       D1COD2
025900110927     C                   clear                   tbeKE1
026000110927     C                   MOVEl     xtakey        tbeKE1
026100130115     C                   clear                   tbeKE2
026200130115     C                   MOVEl     xtakey2       tbeKE2
026300940207     C*
026400940224     C* valorizzo la chiave univoca per reperire i dati
026500940224     C* se non immissione
0266000301141   >C     xtaopz        IFNE      '01'
026700030204      *
026800940315    >C*-------------------------------------------------------------
026900940315    >C* impostare i campi chiave ricevuti
027000940315    >C*-------------------------------------------------------------
027100110927     C     KTBL          chain(N)  tntbe01L
027200940614     C*
027300110927     C                   IF        %Found(tntbe01L)
027400110927      *
027500110927     C                   MOVEL     tbeUNI        EXTDS
027600110927      *
027700110901     c*******            if        TblFLG = '*'
027800110901     c*******            seton                                        08
027900110901     C*******            MOVEl     Annull        D1Ann
028000110901     C*******            end
028100030113     C*
028200940224     C* riempio la videata
028300940224     C                   EXSR      RIED1
028400030113      *
028500940224     C* richiamo routine dei ctrl per decodificarla
028600940614    >C                   EXSR      CTRD1
028700030113      *
028800940224     C                   MOVE      *ALL'0'       IN5098
028900940224     C                   MOVEA     IN5098        *IN(50)
029000030113      *
029100940224     C* definizione indicatori su video
029200940224     C                   SETOFF                                       02
0293009402243    C                   SELECT
029400030114    >C     xtaopz        WHENEQ    '02'
029500940224     C                   SETON                                        02
029600030114    >C     xtaopz        WHENEQ    '05'
0297009402243-   C                   ENDSL
029800940224     C*
0299009402242-   C                   ENDIF
030000030114     C*
0301000301141   >C                   Else
030200030114     C* Se in Immissione nuovo record
030300030114     c                   exsr      NewD1
030400940223     C*
0305009402241-   C                   ENDIF
030600940224     C*
030700940117     C                   ENDSR
030800940207     C/EJECT
030900940207     C************************************************************
031000940510     C* RIEMPIMENTO VIDEATA  D1
031100940207     C************************************************************
031200030114     C     NEWD1         BEGSR
031300940207     C*
031400030114     c                   clear                   d1
031500030114     C*
031600030114     C                   ENDSR
031700030114     C************************************************************
031800030114     C* RIEMPIMENTO VIDEATA  D1
031900030114     C************************************************************
032000030114     C     RIED1         BEGSR
032100030114     C*
032200110927     C* campi delle 45 settimane possibili
032300130115     c                   if         Z79newSOC <> *blank
032400130115     c                   eval      d1SOCNEW =    Z79newSOC
032500110928     c                   end
032600130115     c                   if         Z79DADATA <> *blank
032700130116     c                   move      Z79dadata     campo8            8 0
032800130116     c     *iso          move      campo8        dataiso
032900130116     c                   move      dataiso       dataeur
033000130116     c                   move      dataEUR       d1daDATA
033100110928     c                   end
033200030204     C*
033300940207     C                   ENDSR
033400940309     C************************************************************
033500940309     C* GESTIONE F03 VIDEO D1
033600940309     C************************************************************
033700940309     C     F03D1         BEGSR
033800940309     C*
033900940309     C                   MOVE      *ON           $FINE
034000030113    >C                   MOVE      '1'           xtaret
034100940325     C*
034200940325     C                   ENDSR
034300940309     C************************************************************
034400940309     C* GESTIONE F12 VIDEO D1
034500940309     C************************************************************
034600940309     C     F12D1         BEGSR
034700940309     C*
034800940309     C                   MOVE      *ON           $FINE
034900940309     C*
035000940309     C                   ENDSR
035100940131     C************************************************************
035200940207     C* CONTROLLO VIDEATA
035300940131     C************************************************************
035400940127     C     CTRD1         BEGSR
035500940131     C*
035600940127     C                   SETOFF                                       99
035700030123      *
035800940127     C* non imposto pos. del cursore
035900940127     C                   Z-ADD     0             H1RIGA
036000940127     C                   Z-ADD     0             H1COLO
036100940621     C*
036200940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0363000301141   >C     xtaopz        IFNE      '04'
036400971110     C*
036500971110     C* controllo codice vuoto
036600971110$017 C     D1COD         IFEQ      *ALL' '
036700971110     C                   SETON                                        5099
036800130117     C                   Else
036900130117     c     digits        check     d1cod
037000130117     c                   if        %Found
037100130117     C                   SETON                                        5099
037200130117     c                   else
037300130117     c                   move      d1cod         Filiale           3 0
037400130117     c     Filiale       chain     azORG01l
037500130117     c                   if        not %Found(azORG01L)
037600130117     C                   SETON                                        5099
037700130117     c                   else
037800130117     c                   if        ORGFag <> 'F' and ORGFag <> 'A'
037900130117     C                   SETON                                        5099
038000130117     c                   end
038100130117     c                   end
038200130117     c                   end
038300130117     C                   ENDIF
038400110908     C*
038500130118     c                   clear                   d1des
038600130118     C*
038700130115     C* controllo codice vuoto
038800130115$017 C     D1COD2        IFEQ      *ALL' '
038900130115     C                   SETON                                        5199
039000130117     C                   Else
039100130117     c                   eval      wsoc = D1cod2
039200130117     c                   exsr      decod_societa
039300130117$017 C     wErrore       IFne      *ALL' '
039400130117     C                   SETON                                        5199
039500130118     c                   else
039600130118     c                   eval        d1des = tibsSocO0.RAGSOCIALE
039700130118     c                   end
039800130115     C                   ENDIF
039900130118     C*
040000130118     c                   clear                   d1des2
040100940207     C*
040200130116     C* controllo codice vuoto
040300130116$017 C     D1socNEW      IFEQ      *ALL' '
040400130116     C                   SETON                                        5299
040500130117     C                   Else
040600130117     c                   eval      wsoc = D1socNEW
040700130117     c                   exsr      decod_societa
040800130117$017 C     wErrore       IFne      *ALL' '
040900130117     C                   SETON                                        5299
041000130118     c                   else
041100130118     c                   eval        d1des2 = tibsSocO0.RAGSOCIALE
041200130117     c                   end
041300130116     C                   ENDIF
041400130116     C*
041500130116     C* controllo data errata
041600130116$017 C     D1daDATA      IFEQ      *ALL'0'
041700130116     C                   SETON                                        5499
041800130116     C                   ElsE
041900130116     C*
042000130116     C                   MOVE      d1daDATA      G02DAT
042100130116     C                   MOVEL     *BLANK        G02ERR
042200130116     C                   CALL      'XSRDA8'
042300130116     C                   PARM                    WLBDA8
042400130116     C     G02ERR        IFEQ      '1'
042500130117     C                   SETON                                        5499
042600130116     C                   END
042700130116     c                   move      g02dat        d1daDATA
042800130116     c                   move      g02inv        h1daDATA          8 0
042900130117     C*
043000130117     C*  Non può essere inferiore a oggi
043100130117     c                   if        g02inv < oggi
043200130117     C                   SETON                                        5499
043300130117     C                   END
043400130117     C*
043500130116     C                   ENDIF
043600130116     C*
043700940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
043800030114    >C     xtaopz        IFEQ      '01'
043900030114    >C     xtaopz        oreq      '03'
044000110927     c                   clear                   tbeKE1
044100110927     c                   movel     d1cod         tbeKE1
044200130116     C     KTBL1         chain(N)  tntbe01L
044300110927     c                   if        %Found(tntbe01L)
044400110908     C                   SETON                                        5399
044500030114     C                   EndIF
044600030114     C                   ENDIF
044700030114      *
0448009402241-   C                   END
044900940131     C*
045000940302     C     *IN99         IFEQ      *ON
045100940510     C     $GEST         ANDEQ     'D1'
045200940117     C                   MOVEA     *IN           WIN
045300940128     C                   MOVE      *ALL'0'       IN5098           49
045400940128     C                   MOVEA     IN5098        *IN(50)
045500940614     C                   WRITE     D1
045600940117     C                   MOVEA     WIN           *IN
045700940302     C                   ENDIF
045800940127     C*
045900940117     C                   ENDSR
046000960305     C/EJECT
046100030204     C************************************************************
046200030204     C* A chi deve trasmettere
046300030204     C************************************************************
046400030204     C     ACHI          BEGSR
046500030204     C*
046600030204     c                   eval      S_Tasto = $Tasto
046700030204     C*
046800030204     c                   move      '1'           t1trs
046900030204     c                   clear                   t1fil
047000030204     c     su_win        tag
047100030204     C                   Exfmt     W1Tras
047200030204     c                   if        t1trs = '2' and
047300030204     c                             t1fil = 0
047400030204     c                   seton                                        77
047500030204     c                   goto      su_win
047600030204     c                   end
047700030204     C*
047800030204     c                   eval      $Tasto = S_Tasto
047900030204     C*
048000030204     C                   ENDSR
048100030204     C/EJECT
048200030204     C************************************************************
048300030204    >C* AGGIORNAMENTO ANAGRAFICA
048400030204     C************************************************************
048500030204    >C     FLDAGG        BEGSR
048600030204     C*
048700030204     C*  campi chiave da scrivere e aggiornare
048800110927     C                   clear                   tbeKE1
048900110927     C                   MOVEl     D1COD         tbeKE1
049000130115     C                   clear                   tbeKE2
049100130115     C                   MOVEl     D1COD2        tbeKE2
049200030204     C*
049300030204     C                   ENDSR
049400940207     C/EJECT
049500940224     C************************************************************
049600940315    >C* AGGIORNAMENTO ANAGRAFICA
049700940224     C************************************************************
049800940315    >C     AGGANA        BEGSR
049900940207     C*
050000030204     C                   exsr      FLDAGG
050100030204     C*
050200940207     C                   SELECT
050300030204     C*
050400030114    >C     xtaopz        WHENEQ    '01'
050500030114    >C     xtaopz        OREQ      '03'
050600030204     C*
050700130115     C                   move      'Z79'         tbeCOD
050800130115    >C                   EXSR      RIEANA_Z79
050900110927     c                   write     tntbe000                             59
051000940318     C* se rcd non scrivibile attivo errore
051100030114    >C   59              SETON                                          99
051200030204      *
051300030114    >C     xtaopz        WHENEQ    '02'
051400030204      *
051500130115     C                   move      'Z79'         tbeCOD
051600110927     C     KTBL          chain     tntbe01L
051700030204     C                   exsr      FLDAGG
051800130115    >C                   EXSR      RIEANA_Z79
051900110927     c                   update    tntbe000                             59
052000030114    >C   59              SETON                                          99
052100030204      *
052200030114    >C     xtaopz        WHENEQ    '04'
052300130115     C                   move      'Z79'         tbeCOD
052400110927     C     KTBL          chain     tntbe01L
052500030115     c                   if        $tasto = F23
052600110927     c                   delete    tntbe000                             59
052700030115     c                   else
052800110927     c                   delete    tntbe000                             59
052900030115     c                   end
053000030114    >C   59              SETON                                          99
053100940621     C*
053200940207     C                   ENDSL
053300940203     C*
053400940203     C                   ENDSR
053500940224     C************************************************************
053600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
053700940224     C************************************************************
053800130115     C     RIEANA_Z79    BEGSR
053900030114     C*
054000110927     C                   CLEAR                   tbeuni
054100130115     C                   movel     *all' '       tbeuni
054200110928     C                   MOVEL     *all'0'       EXTDS
054300130115     C*
054400130115     C*  Nuova Soceità e DATA inizio Validità
054500130115     c                   if            d1socNEW <> *blank
054600130115     c                   movel     d1socNEW      Z79newSOC
054700110928     c                   end
054800130115     C*
054900130116     c                   if            h1daDATA <> *zeros
055000130116     c                   movel     h1daDATA      Z79daDATA
055100110928     c                   end
055200110927     C                   MOVEL     EXTDS         tbeuni
055300030114     C*
055400940207     C                   ENDSR
055500940224     C************************************************************
055600940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
055700940224     C************************************************************
055800940224     C     GESAGG        BEGSR
055900940224     C*
056000940224     C* segnalo al pgm chiamante l'avvenuta conferma
056100030114    >C                   MOVE      *ON           xtaopr
056200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
056300030114    >C                   MOVE      '0'           xtaret
056400940224     C*
0565009402241    C                   SELECT
056600940224     C* nel caso di immissione, ripristino la videata iniziale
056700030114    >C     xtaopz        WHENEQ    '01'
056800940224     C                   MOVE      *ON           $INZD1
056900030114     C                   MOVE      *ON           $FINE
057000940224     C* altrimenti ritorno al pgm chiamante
057100940224     C                   OTHER
057200940224     C                   MOVE      *ON           $FINE
0573009402241-   C                   ENDSL
057400940224     C*
057500940224     C                   ENDSR
057600940131     C************************************************************
057700940131     C* OPERAZIONI INIZIALI
057800940131     C************************************************************
057900940131     C     *INZSR        BEGSR
058000940131     C*
058100940127     C* Reperimento parametri
058200940127     C*
058300940117     C     *ENTRY        PLIST
058400940117     C                   PARM                    KPJBA
058500030113     C                   movel     kpjbu         tabds
058600030115     c                   setoff                                       08
058700130117     C*
058800130117     ** Inizializzo il programma.
058900130117     C                   CALL      'TIBSSOCR'
059000130117     C                   PARM      'INIT'        prmRqsOpCode
059100130117     C                   PARM                    prmRpyOpCode
059200130117     C                   PARM                    prmRpyIdMsg
059300030115     C*
059400940223     C* Variabili per gestione videate
059500940223     C                   MOVE      'D1'          $GEST
059600940223     C                   MOVE      *OFF          $FINE
059700940223     C                   MOVE      *ON           $INZD1
059800940224     C                   MOVE      *BLANK        $LASTG
059900940224     C                   MOVE      *BLANK        $LASTV
060000940506     C                   Z-ADD     0             $ULKD1            3 0
060100940127     C*
060200940223     C* Variabili appoggio
060300940224     C                   Z-ADD     0             CURR
060400940224     C                   Z-ADD     0             CURC
060500940207     C*
060600940207     C* Valorizzazione campi univoci testate
060700940614     C                   CLEAR                   T1
060800940614     C                   MOVEL     KNSIF         NOMSIF
060900030113     C******             MOVEL     XSCDSI        NOMDIT
061000941027     C                   MOVEL     DSPGM         NOMPGM
061100030113     C*
061200130115     C                   move      'Z79'         tbeCOD
061300110927     C                   clear                   tbeKE1
061400110927     C                   moveL     xtakey        tbeKE1
061500130115     C                   clear                   tbeKE2
061600130115     C                   moveL     xtakey2       tbeKE2
061700940224     C*
061800940224     C* Inizializzazione/Decodifica prima videata
061900940224     C                   EXSR      INZD1
062000940224     C                   MOVE      *OFF          $INZD1
062100940127     C*
062200130117     C                   time                    w0120            14 0
062300130117     C                   movel     w0120         times             6 0
062400130117     C                   move      w0120         wdat
062500130117      * data da impostare su data stampa e convalida se lancio = 'C'
062600130117     c                   move      wdat          dataeur
062700130117     c                   move      dataeur       dataiso
062800130117     c                   move      dataiso       oggi
062900130117     C*
063000940117     C                   ENDSR
063100940128     C************************************************************
063200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
063300940223     C************************************************************
063400940223     C     DEFCAM        BEGSR
063500940223     C*
063600940223     C* klist
063700030113     C     KTBL          KLIST
063800110927     C                   KFLD                    tbeCOD
063900110927     C                   KFLD                    tbeKE1
064000130115     C                   KFLD                    tbeKE2
064100130116     C* klist
064200130116     C     KTBL1         KLIST
064300130116     C                   KFLD                    tbeCOD
064400130116     C                   KFLD                    tbeKE1
064500940207     C*
064600940223     C* Variabili per gestione videate
064700940223     C*
064800940223     C                   MOVE      *BLANK        $GEST             2
064900940223     C                   MOVE      *BLANK        $FINE             1
065000940223     C                   MOVE      *BLANK        $INZD1            1
065100940223     C                   MOVE      *BLANK        $LASTG            2
065200940223     C                   MOVE      *BLANK        $LASTV            2
065300940223     C*
065400940223     C* Variabili appoggio
065500940223     C                   Z-ADD     0             CURR              2 0
065600940223     C                   Z-ADD     0             CURC              2 0
065700940223     C                   MOVE      *ZEROS        WIN              99
065800940224     C                   MOVE      *BLANK        WTBERR            1
065900940223     C*
066000940223     C* Indici
066100940223     C                   Z-ADD     0             X                 3 0
066200940223     C                   Z-ADD     0             Y                 3 0
066300940127     C*
066400940117     C                   ENDSR
066500940128     C************************************************************
066600130117     C*
066700130117     C************************************************************
066800130117     C     Decod_societa begSR
066900130117      *
067000130117     c                   eval      wErrore = *blank
067100130117     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
067200130117     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
067300130118     C                   clear                   tibsSocI0
067400130117     C                   EVAL      tibsSocI0.idSocieta = wSOC
067500130117     C
067600130117     C                   CALL      'TIBSSOCR'
067700130117     C                   PARM      'GETANAGRAF'  prmRqsOpCode
067800130117     C                   PARM      *BLANK        prmRpyOpCode
067900130117     C                   PARM      *ZERO         prmRpyIdMsg
068000130117     C                   PARM      'TIBSSOCI0'   prmRqsFormato
068100130117     C                   PARM                    tibsSocI0
068200130117     C                   PARM                    prmRqsDataSize
068300130117     C                   PARM      'TIBSSOCO0'   prmRpyFormato
068400130117     C                   PARM                    tibsSocO0
068500130117     C                   PARM                    prmRpyDataSize
068600130117     c                   if         PRMRPYIDMSG >= 0
068700130117     c                              and TIBSSOCO0.IDSOCIETA <> *blank
068800130117     c                   eval      wErrore = *blank
068900130117     c                   else
069000130117     c                   eval      wErrore = 'Errore'
069100130117     c                   end
069200130117      *
069300130117     C                   ENDSR
069400130117     C************************************************************
