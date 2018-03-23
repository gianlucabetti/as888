000100130204     h dftactgrp(*no) actgrp(*Caller)  BNDDIR('TIBS')
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
015500130204ab    **
015600130204ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
015700130204ab    /DEFINE DFTACTGRP_NO
015800130204ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
015900130204ab    /UNDEFINE DFTACTGRP_NO
016000130204      *
016100130204ab   D TibsSof_esito...
016200130204ab   D                 S             10I 0 IMPORT
016300130204ab   D idSocieta...
016400130204EDPDCD                 S              3A
016500130204ab   **
016600130204EDPDCd Filiale         s              3S 0
016700130204EDPDCd dtVALIDITA      s               D
016800130204EDPDCd VALdatFINE      s               D
016900130204EDPDCd VALdatINIZ      s               D
017000130204EDPDCd SOC_errata      s              1A
017100130117
017200940127     C*----------------------------------------------------*
017300940127     C*                MAIN LINE PROGRAM
017400940127     C*----------------------------------------------------*
017500130204ab   C*     Inizializza
017600130204ab    /FREE
017700130204ab     TibsSof_Init();
017800130204ab    /END-FREE
017900130117     C*
018000940223     C*
018100940223     C     $FINE         DOWEQ     *OFF
018200940202     C     $GEST         CASEQ     'D1'          GESD1
018300940117     C                   END
018400940117     C                   END
018500130117     C*
018600940325     C* fine programma
018700030113     c                   movel     tabds         kpjbu
018800030113     C                   Seton                                        LR
018900940131     C************************************************************
019000940510     C* GESTIONE VIDEO RECORD D1
019100940131     C************************************************************
019200940127     C     GESD1         BEGSR
019300030113      *
019400940225     C* inizializzazione videata
019500940223     C     $INZD1        IFEQ      *ON
019600940127     C                   EXSR      INZD1
019700940223     C                   MOVE      *OFF          $INZD1
019800940117     C                   END
019900030113      *
020000940225     C* emissione piede videata se proveniente da altra
020100940225     C* salvataggio valore formato di provenienza
020200940207     C     $LASTV        IFNE      'D1'
020300940614     C                   WRITE     T1
020400940202     C                   MOVE      $LASTV        $LASTG
020500940202     C                   MOVE      'D1'          $LASTV
020600940202     C                   END
020700940131     C*
020800940203     C                   SELECT
020900940203     C* Annullamento
021000030114     C* Visualizzazione
021100030114    >C     xtaopz        WHENEQ    '01'
021200030114     c                   movel     Opz_Imm       D1Fun
021300030114    >C     xtaopz        WHENEQ    '05'
021400030114     c                   movel     Opz_Vis       D1Fun
021500030114     C                   SETON                                        020306
021600030114    >C     xtaopz        WHENEQ    '04'
021700030114     c                   movel     Opz_Del       D1Fun
021800110902     C                   SETON                                        02
021900030114     C                   SETOFF                                         06
022000940620     C* Manutenzione
022100030113    >C     xtaopz        WHENEQ    '02'
022200030114     c                   movel     Opz_Var       D1Fun
022300030114     C                   SETON                                        02
022400030114     C                   SETOFF                                       0306
022500940620     C*
022600940620     C                   OTHER
022700940620     C*
022800030114     C                   SETOFF                                       020306
022900940203     C                   ENDSL
023000030114     C*
023100030114     c                   SETOFF                                         99
023200940620     C* Immissione/Modifica/Copia
023300030113     C*               *----------------*
023400940620     C                   EXFMT     D1
023500030113     C*               *----------------*
023600030114     C*
023700030114     C     *IN99         IFEQ      *OFF
023800030114     C*
0239000301141    C                   SELECT
024000030114     C* F3=Fine
024100030114     C     $TASTO        WHENEQ    F03
024200030114     C                   EXSR      F03D1
024300030114     C* F12=Ritorno
024400030114     C     $TASTO        WHENEQ    F12
024500030114     C                   EXSR      F12D1
024600030114     C*
0247000301141O   C                   OTHER
024800030114     C*
024900030114     C                   EXSR      CTRD1
025000030113     C*
025100940616     C     *IN99         IFEQ      *OFF
025200940616     C* F6
0253000302052    C     $TASTO        ifeq      F06
0254000301152    C     $TASTO        oreq      F23
025500030204     C* A chi Trasmette la Tabella ?
025600110901     c****               Exsr      ACHI
025700940224     C* eseguo aggiornamento
025800940315    >C                   EXSR      AGGANA
025900940224     C* eseguo operazioni del dopo-aggiornamento
026000030114     C  N99              EXSR      GESAGG
026100030205     c                   end
026200030114     C*
026300030114     C                   ENDIF
026400030114     C*
0265000301142-   C                   ENDSL
026600030114     C*
0267000301142-   C                   ENDIF
026800030114     C*
026900940117     C                   ENDSR
027000940117      ************************************************************
027100940207      * INIZIALIZZAZIONE VIDEATA DATI
027200940117      ************************************************************
027300940127     C     INZD1         BEGSR
027400940131     C*
027500940615     C                   CLEAR                   D1
027600130115     C                   move      'Z79'         tbeCOD
027700030113     C                   MOVEl     xtakey        D1COD
027800130115     C                   MOVEl     xtakey2       D1COD2
027900110927     C                   clear                   tbeKE1
028000110927     C                   MOVEl     xtakey        tbeKE1
028100130115     C                   clear                   tbeKE2
028200130115     C                   MOVEl     xtakey2       tbeKE2
028300940207     C*
028400940224     C* valorizzo la chiave univoca per reperire i dati
028500940224     C* se non immissione
0286000301141   >C     xtaopz        IFNE      '01'
028700030204      *
028800940315    >C*-------------------------------------------------------------
028900940315    >C* impostare i campi chiave ricevuti
029000940315    >C*-------------------------------------------------------------
029100110927     C     KTBL          chain(N)  tntbe01L
029200940614     C*
029300110927     C                   IF        %Found(tntbe01L)
029400110927      *
029500110927     C                   MOVEL     tbeUNI        EXTDS
029600110927      *
029700110901     c*******            if        TblFLG = '*'
029800110901     c*******            seton                                        08
029900110901     C*******            MOVEl     Annull        D1Ann
030000110901     C*******            end
030100030113     C*
030200940224     C* riempio la videata
030300940224     C                   EXSR      RIED1
030400030113      *
030500940224     C* richiamo routine dei ctrl per decodificarla
030600940614    >C                   EXSR      CTRD1
030700030113      *
030800940224     C                   MOVE      *ALL'0'       IN5098
030900940224     C                   MOVEA     IN5098        *IN(50)
031000030113      *
031100940224     C* definizione indicatori su video
031200940224     C                   SETOFF                                       02
0313009402243    C                   SELECT
031400030114    >C     xtaopz        WHENEQ    '02'
031500940224     C                   SETON                                        02
031600030114    >C     xtaopz        WHENEQ    '05'
0317009402243-   C                   ENDSL
031800940224     C*
0319009402242-   C                   ENDIF
032000030114     C*
0321000301141   >C                   Else
032200030114     C* Se in Immissione nuovo record
032300030114     c                   exsr      NewD1
032400940223     C*
0325009402241-   C                   ENDIF
032600940224     C*
032700940117     C                   ENDSR
032800940207     C/EJECT
032900940207     C************************************************************
033000940510     C* RIEMPIMENTO VIDEATA  D1
033100940207     C************************************************************
033200030114     C     NEWD1         BEGSR
033300940207     C*
033400030114     c                   clear                   d1
033500030114     C*
033600030114     C                   ENDSR
033700030114     C************************************************************
033800030114     C* RIEMPIMENTO VIDEATA  D1
033900030114     C************************************************************
034000030114     C     RIED1         BEGSR
034100030114     C*
034200110927     C* campi delle 45 settimane possibili
034300130115     c                   if         Z79newSOC <> *blank
034400130115     c                   eval      d1SOCNEW =    Z79newSOC
034500110928     c                   end
034600130115     c                   if         Z79DADATA <> *blank
034700130116     c                   move      Z79dadata     campo8            8 0
034800130116     c     *iso          move      campo8        dataiso
034900130116     c                   move      dataiso       dataeur
035000130116     c                   move      dataEUR       d1daDATA
035100110928     c                   end
035200030204     C*
035300940207     C                   ENDSR
035400940309     C************************************************************
035500940309     C* GESTIONE F03 VIDEO D1
035600940309     C************************************************************
035700940309     C     F03D1         BEGSR
035800940309     C*
035900940309     C                   MOVE      *ON           $FINE
036000030113    >C                   MOVE      '1'           xtaret
036100940325     C*
036200940325     C                   ENDSR
036300940309     C************************************************************
036400940309     C* GESTIONE F12 VIDEO D1
036500940309     C************************************************************
036600940309     C     F12D1         BEGSR
036700940309     C*
036800940309     C                   MOVE      *ON           $FINE
036900940309     C*
037000940309     C                   ENDSR
037100940131     C************************************************************
037200940207     C* CONTROLLO VIDEATA
037300940131     C************************************************************
037400940127     C     CTRD1         BEGSR
037500940131     C*
037600940127     C                   SETOFF                                       99
037700030123      *
037800940127     C* non imposto pos. del cursore
037900940127     C                   Z-ADD     0             H1RIGA
038000940127     C                   Z-ADD     0             H1COLO
038100940621     C*
038200940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0383000301141   >C     xtaopz        IFNE      '04'
038400971110     C*
038500971110     C* controllo codice vuoto
038600971110$017 C     D1COD         IFEQ      *ALL' '
038700971110     C                   SETON                                        5099
038800130117     C                   Else
038900130117     c     digits        check     d1cod
039000130117     c                   if        %Found
039100130117     C                   SETON                                        5099
039200130117     c                   else
039300130117     c                   move      d1cod         Filiale           3 0
039400130117     c     Filiale       chain     azORG01l
039500130117     c                   if        not %Found(azORG01L)
039600130117     C                   SETON                                        5099
039700130117     c                   else
039800130117     c                   if        ORGFag <> 'F' and ORGFag <> 'A'
039900130117     C                   SETON                                        5099
040000130117     c                   end
040100130117     c                   end
040200130117     c                   end
040300130117     C                   ENDIF
040400110908     C*
040500130118     c                   clear                   d1des
040600130118     C*
040700130115     C* controllo codice vuoto
040800130115$017 C     D1COD2        IFEQ      *ALL' '
040900130115     C                   SETON                                        5199
041000130117     C                   Else
041100130117     c                   eval      wsoc = D1cod2
041200130117     c                   exsr      decod_societa
041300130204$017 c     wErrore       IFne      *ALL' '
041400130117     C                   SETON                                        5199
041500130118     c                   else
041600130118     c                   eval        d1des = tibsSocO0.RAGSOCIALE
041700130118     c                   end
041800130115     C                   ENDIF
041900130118     C*
042000130118     c                   clear                   d1des2
042100940207     C*
042200130116     C* controllo codice vuoto
042300130116$017 C     D1socNEW      IFEQ      *ALL' '
042400130116     C                   SETON                                        5299
042500130117     C                   Else
042600130117     c                   eval      wsoc = D1socNEW
042700130117     c                   exsr      decod_societa
042800130117$017 C     wErrore       IFne      *ALL' '
042900130117     C                   SETON                                        5299
043000130118     c                   else
043100130118     c                   eval        d1des2 = tibsSocO0.RAGSOCIALE
043200130117     c                   end
043300130116     C                   ENDIF
043400130204      *
043500130204      *  La società è uguale fra vecchia e nuova
043600130204$017 C                   IF        D1COD2 = D1socNEW
043700130204     C                   SETON                                        5599
043800130204     C                   ENDIF
043900130204      *  Se la Filiale e la società inserita non si appartengono
044000130204     c                   move      udateiso      dtvalidita
044100130204ab   C                   exsr      TEST_societa
044200130204     C                   if        soc_errata ='S'  or
044300130204     C                             idSocieta <> D1COD2
044400130204     C                   SETON                                        5699
044500130204     C                   ENDIF
044600130204     C*
044700130116     C* controllo data errata
044800130116$017 C     D1daDATA      IFEQ      *ALL'0'
044900130116     C                   SETON                                        5499
045000130116     C                   ElsE
045100130116     C*
045200130116     C                   MOVE      d1daDATA      G02DAT
045300130116     C                   MOVEL     *BLANK        G02ERR
045400130116     C                   CALL      'XSRDA8'
045500130116     C                   PARM                    WLBDA8
045600130116     C     G02ERR        IFEQ      '1'
045700130117     C                   SETON                                        5499
045800130116     C                   END
045900130116     c                   move      g02dat        d1daDATA
046000130116     c                   move      g02inv        h1daDATA          8 0
046100130117     C*
046200130117     C*  Non può essere inferiore a oggi
046300130117     c                   if        g02inv < oggi
046400130117     C                   SETON                                        5499
046500130117     C                   END
046600130117     C*
046700130116     C                   ENDIF
046800130116     C*
046900940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
047000030114    >C     xtaopz        IFEQ      '01'
047100030114    >C     xtaopz        oreq      '03'
047200110927     c                   clear                   tbeKE1
047300110927     c                   movel     d1cod         tbeKE1
047400130116     C     KTBL1         chain(N)  tntbe01L
047500110927     c                   if        %Found(tntbe01L)
047600110908     C                   SETON                                        5399
047700030114     C                   EndIF
047800030114     C                   ENDIF
047900030114      *
0480009402241-   C                   END
048100940131     C*
048200940302     C     *IN99         IFEQ      *ON
048300940510     C     $GEST         ANDEQ     'D1'
048400940117     C                   MOVEA     *IN           WIN
048500940128     C                   MOVE      *ALL'0'       IN5098           49
048600940128     C                   MOVEA     IN5098        *IN(50)
048700940614     C                   WRITE     D1
048800940117     C                   MOVEA     WIN           *IN
048900940302     C                   ENDIF
049000940127     C*
049100940117     C                   ENDSR
049200960305     C/EJECT
049300030204     C************************************************************
049400030204     C* A chi deve trasmettere
049500030204     C************************************************************
049600030204     C     ACHI          BEGSR
049700030204     C*
049800030204     c                   eval      S_Tasto = $Tasto
049900030204     C*
050000030204     c                   move      '1'           t1trs
050100030204     c                   clear                   t1fil
050200030204     c     su_win        tag
050300030204     C                   Exfmt     W1Tras
050400030204     c                   if        t1trs = '2' and
050500030204     c                             t1fil = 0
050600030204     c                   seton                                        77
050700030204     c                   goto      su_win
050800030204     c                   end
050900030204     C*
051000030204     c                   eval      $Tasto = S_Tasto
051100030204     C*
051200030204     C                   ENDSR
051300030204     C/EJECT
051400030204     C************************************************************
051500030204    >C* AGGIORNAMENTO ANAGRAFICA
051600030204     C************************************************************
051700030204    >C     FLDAGG        BEGSR
051800030204     C*
051900030204     C*  campi chiave da scrivere e aggiornare
052000110927     C                   clear                   tbeKE1
052100110927     C                   MOVEl     D1COD         tbeKE1
052200130115     C                   clear                   tbeKE2
052300130115     C                   MOVEl     D1COD2        tbeKE2
052400030204     C*
052500030204     C                   ENDSR
052600940207     C/EJECT
052700940224     C************************************************************
052800940315    >C* AGGIORNAMENTO ANAGRAFICA
052900940224     C************************************************************
053000940315    >C     AGGANA        BEGSR
053100940207     C*
053200030204     C                   exsr      FLDAGG
053300030204     C*
053400940207     C                   SELECT
053500030204     C*
053600030114    >C     xtaopz        WHENEQ    '01'
053700030114    >C     xtaopz        OREQ      '03'
053800030204     C*
053900130115     C                   move      'Z79'         tbeCOD
054000130115    >C                   EXSR      RIEANA_Z79
054100110927     c                   write     tntbe000                             59
054200940318     C* se rcd non scrivibile attivo errore
054300030114    >C   59              SETON                                          99
054400030204      *
054500030114    >C     xtaopz        WHENEQ    '02'
054600030204      *
054700130115     C                   move      'Z79'         tbeCOD
054800110927     C     KTBL          chain     tntbe01L
054900030204     C                   exsr      FLDAGG
055000130115    >C                   EXSR      RIEANA_Z79
055100110927     c                   update    tntbe000                             59
055200030114    >C   59              SETON                                          99
055300030204      *
055400030114    >C     xtaopz        WHENEQ    '04'
055500130115     C                   move      'Z79'         tbeCOD
055600110927     C     KTBL          chain     tntbe01L
055700030115     c                   if        $tasto = F23
055800110927     c                   delete    tntbe000                             59
055900030115     c                   else
056000110927     c                   delete    tntbe000                             59
056100030115     c                   end
056200030114    >C   59              SETON                                          99
056300940621     C*
056400940207     C                   ENDSL
056500940203     C*
056600940203     C                   ENDSR
056700940224     C************************************************************
056800940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
056900940224     C************************************************************
057000130115     C     RIEANA_Z79    BEGSR
057100030114     C*
057200110927     C                   CLEAR                   tbeuni
057300130115     C                   movel     *all' '       tbeuni
057400110928     C                   MOVEL     *all'0'       EXTDS
057500130115     C*
057600130115     C*  Nuova Soceità e DATA inizio Validità
057700130115     c                   if            d1socNEW <> *blank
057800130115     c                   movel     d1socNEW      Z79newSOC
057900110928     c                   end
058000130115     C*
058100130116     c                   if            h1daDATA <> *zeros
058200130116     c                   movel     h1daDATA      Z79daDATA
058300110928     c                   end
058400110927     C                   MOVEL     EXTDS         tbeuni
058500030114     C*
058600940207     C                   ENDSR
058700940224     C************************************************************
058800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
058900940224     C************************************************************
059000940224     C     GESAGG        BEGSR
059100940224     C*
059200940224     C* segnalo al pgm chiamante l'avvenuta conferma
059300030114    >C                   MOVE      *ON           xtaopr
059400940224     C* segnalo al pgm chiamante l'aver premuto l'F6
059500030114    >C                   MOVE      '0'           xtaret
059600940224     C*
0597009402241    C                   SELECT
059800940224     C* nel caso di immissione, ripristino la videata iniziale
059900030114    >C     xtaopz        WHENEQ    '01'
060000940224     C                   MOVE      *ON           $INZD1
060100030114     C                   MOVE      *ON           $FINE
060200940224     C* altrimenti ritorno al pgm chiamante
060300940224     C                   OTHER
060400940224     C                   MOVE      *ON           $FINE
0605009402241-   C                   ENDSL
060600940224     C*
060700940224     C                   ENDSR
060800940131     C************************************************************
060900940131     C* OPERAZIONI INIZIALI
061000940131     C************************************************************
061100940131     C     *INZSR        BEGSR
061200940131     C*
061300940127     C* Reperimento parametri
061400940127     C*
061500940117     C     *ENTRY        PLIST
061600940117     C                   PARM                    KPJBA
061700030113     C                   movel     kpjbu         tabds
061800030115     c                   setoff                                       08
061900130117     C*
062000130117     ** Inizializzo il programma.
062100130117     C                   CALL      'TIBSSOCR'
062200130117     C                   PARM      'INIT'        prmRqsOpCode
062300130117     C                   PARM                    prmRpyOpCode
062400130117     C                   PARM                    prmRpyIdMsg
062500030115     C*
062600940223     C* Variabili per gestione videate
062700940223     C                   MOVE      'D1'          $GEST
062800940223     C                   MOVE      *OFF          $FINE
062900940223     C                   MOVE      *ON           $INZD1
063000940224     C                   MOVE      *BLANK        $LASTG
063100940224     C                   MOVE      *BLANK        $LASTV
063200940506     C                   Z-ADD     0             $ULKD1            3 0
063300940127     C*
063400940223     C* Variabili appoggio
063500940224     C                   Z-ADD     0             CURR
063600940224     C                   Z-ADD     0             CURC
063700940207     C*
063800940207     C* Valorizzazione campi univoci testate
063900940614     C                   CLEAR                   T1
064000940614     C                   MOVEL     KNSIF         NOMSIF
064100030113     C******             MOVEL     XSCDSI        NOMDIT
064200941027     C                   MOVEL     DSPGM         NOMPGM
064300030113     C*
064400130115     C                   move      'Z79'         tbeCOD
064500110927     C                   clear                   tbeKE1
064600110927     C                   moveL     xtakey        tbeKE1
064700130115     C                   clear                   tbeKE2
064800130115     C                   moveL     xtakey2       tbeKE2
064900940224     C*
065000940224     C* Inizializzazione/Decodifica prima videata
065100940224     C                   EXSR      INZD1
065200940224     C                   MOVE      *OFF          $INZD1
065300940127     C*
065400130117     C                   time                    w0120            14 0
065500130117     C                   movel     w0120         times             6 0
065600130117     C                   move      w0120         wdat
065700130117      * data da impostare su data stampa e convalida se lancio = 'C'
065800130117     c                   move      wdat          dataeur
065900130117     c                   move      dataeur       dataiso
066000130117     c                   move      dataiso       oggi
066100130204     c                   move      dataiso       udateiso
066200130117     C*
066300940117     C                   ENDSR
066400940128     C************************************************************
066500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
066600940223     C************************************************************
066700940223     C     DEFCAM        BEGSR
066800940223     C*
066900940223     C* klist
067000030113     C     KTBL          KLIST
067100110927     C                   KFLD                    tbeCOD
067200110927     C                   KFLD                    tbeKE1
067300130115     C                   KFLD                    tbeKE2
067400130116     C* klist
067500130116     C     KTBL1         KLIST
067600130116     C                   KFLD                    tbeCOD
067700130116     C                   KFLD                    tbeKE1
067800940207     C*
067900940223     C* Variabili per gestione videate
068000940223     C*
068100940223     C                   MOVE      *BLANK        $GEST             2
068200940223     C                   MOVE      *BLANK        $FINE             1
068300940223     C                   MOVE      *BLANK        $INZD1            1
068400940223     C                   MOVE      *BLANK        $LASTG            2
068500940223     C                   MOVE      *BLANK        $LASTV            2
068600940223     C*
068700940223     C* Variabili appoggio
068800940223     C                   Z-ADD     0             CURR              2 0
068900940223     C                   Z-ADD     0             CURC              2 0
069000940223     C                   MOVE      *ZEROS        WIN              99
069100940224     C                   MOVE      *BLANK        WTBERR            1
069200940223     C*
069300940223     C* Indici
069400940223     C                   Z-ADD     0             X                 3 0
069500940223     C                   Z-ADD     0             Y                 3 0
069600940127     C*
069700940117     C                   ENDSR
069800940128     C************************************************************
069900130117     C*
070000130117     C************************************************************
070100130117     C     Decod_societa begSR
070200130117      *
070300130117     c                   eval      wErrore = *blank
070400130117     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
070500130117     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
070600130118     C                   clear                   tibsSocI0
070700130117     C                   EVAL      tibsSocI0.idSocieta = wSOC
070800130117     C
070900130117     C                   CALL      'TIBSSOCR'
071000130117     C                   PARM      'GETANAGRAF'  prmRqsOpCode
071100130117     C                   PARM      *BLANK        prmRpyOpCode
071200130117     C                   PARM      *ZERO         prmRpyIdMsg
071300130117     C                   PARM      'TIBSSOCI0'   prmRqsFormato
071400130117     C                   PARM                    tibsSocI0
071500130117     C                   PARM                    prmRqsDataSize
071600130117     C                   PARM      'TIBSSOCO0'   prmRpyFormato
071700130117     C                   PARM                    tibsSocO0
071800130117     C                   PARM                    prmRpyDataSize
071900130117     c                   if         PRMRPYIDMSG >= 0
072000130117     c                              and TIBSSOCO0.IDSOCIETA <> *blank
072100130117     c                   eval      wErrore = *blank
072200130117     c                   else
072300130117     c                   eval      wErrore = 'Errore'
072400130117     c                   end
072500130117      *
072600130117     C                   ENDSR
072700130117     C************************************************************
072800130204ab    * Controlla se nel periodo successivo è prevista un'altra società
072900130204ab   C**************************************************************************
073000130204ab   C     TEST_SOCieta  begSR
073100130204ab    *
073200130204     c                   eval      soc_errata ='N'
073300130204ab    /FREE
073400130204ab
073500130204EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
073600130204EDPDC                                             : dtVALIDITA
073700130204EDPDC                                             : 'O'
073800130204EDPDC                                             : valDatIniz
073900130204EDPDC                                             : valDatFine
074000130204ab                                                );
074100130204ab
074200130204EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
074300130204EDPDC    // Gestire l'errore.
074400130204
074500130204         eval  soc_errata ='S';
074600130204ab     ENDIF;
074700130204ab
074800130204ab    /END-FREE
074900130204ab   C                   ENDSR
075000130204     c**********************************************************************
