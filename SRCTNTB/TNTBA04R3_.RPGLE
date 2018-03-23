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
002300161031$001 FTNTBA04D3 CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500161103     Ftntbe00F  UF A E           K DISK
002600161031     fTNTBE02L  if   e           k disk    rename(TNTBE000:TNTBE2)
002700161031     f                                     prefix(T2_)
002800161031     fTNTBEx1L  Uf A e           k disk    rename(tntbe000:tntbex) prefix(x)
002900161031      *
003000161031      *
003100161031     fazorg01l  if   e           k disk
003200940128     D*----------------------------------------------------*
003300161031$003 D EXTDS         E DS                  EXTNAME(DAIG)
003400030113      *-------------
003500940127     D* Passaggio Parametri
003600940127     D KPJBA         E DS
003700030113      *-------------
003800030113     D* Parametri in ricezione
003900030113     D  TABDS          DS
004000030113     D  XTAOPZ                 1      2
004100030113     D  XTARET                 3      3
004200030113     D  XTAOPR                 4      4
004300030113     D  XTAERR                 5      5
004400161031     D  XTAKE1                 6     20
004500161031     D  XTAKE2                21     35
004600161103     D  XTAtberec             36     44
004700161031      *---------------------------------------------------------------*
004800161031     d wDate           s               d   datfmt(*iso)  inz(*loval)
004900161031     D DATA_eur        S               D   DATFMT(*eur)
005000161031     D DATA_amg        S               D   DATFMT(*iso)
005100161031     D DATA_iso        S               D   DATFMT(*iso)
005200161031     D*-------------
005300161031     D WLBDA8          DS
005400161031     D  G02DAT                 1      8  0
005500161031     D  G02INV                 9     16  0
005600161031     D  G02ERR                17     17
005700161031     D  G02TGI                18     22  0
005800161031      *
005900161031      *
006000161031     d TIBS34ds      e ds                  inz
006100161031     d dDatiUte      e ds                  inz
006200161031     d AZUTEds       e ds                  extname(AZUTE00F) inz
006300161031      *
006400161031     d TNTBEds       e ds                  extname(TNTBE00F) inz
006500161031     d xTNTBEds      e ds                  extname(TNTBEx1l) inz
006600161031     d                                     prefix(x)
006700161031      *
006800161031     d dschiave        ds            15
006900161031     d  d1fil
007000161031     d  d1asp
007100161031     d  d1ter
007200161031     d  d1mas
007300161031     d  d1att
007400161031     d  h1all
007500161031      *
007600161031     D tibs02ds      E DS
007700161031      *
007800161031      * Tabella
007900161031     D dTAI          E DS
008000161031     D dATS          E DS
008100161031     D dMAS          E DS
008200161031     D dAAT          E DS
008300161031     D dALL          E DS
008400940201     D*-------------
008500030204     D  S_TASTO        S                   like($Tasto)
008600940201     D DSFMT           DS
008700940506     D  $TASTO               369    369
008800940201     D  NRG                  370    370
008900940201     D  NCL                  371    371
009000940201     D*-------------
009100940201     D* posizione cursore
009200940201     D CURSOR          DS
009300940223     D  RIRI                   1      2B 0 INZ
009400940201     D  R$$                    2      2
009500940223     D  COCO                   3      4B 0 INZ
009600940201     D  C$$                    4      4
009700940207     D*-------------
009800940207     D* Reperimento nome PGM
009900940207     D STATUS         SDS           333
010000940207     D  DSPGM            *PROC
010100940225     D*-------------
010200940225     D* COSTANTI
010300940225     D*-------------
010400940506     D* Tasti di funzione
010500940506     D F01             C                   CONST(X'31')
010600940506     D F02             C                   CONST(X'32')
010700940506     D F03             C                   CONST(X'33')
010800940506     D F04             C                   CONST(X'34')
010900940506     D F05             C                   CONST(X'35')
011000940506     D F06             C                   CONST(X'36')
011100940506     D F07             C                   CONST(X'37')
011200940506     D F08             C                   CONST(X'38')
011300940506     D F09             C                   CONST(X'39')
011400940506     D F10             C                   CONST(X'3A')
011500940506     D F11             C                   CONST(X'3B')
011600940506     D F12             C                   CONST(X'3C')
011700940506     D F13             C                   CONST(X'B1')
011800940506     D F14             C                   CONST(X'B2')
011900940506     D F15             C                   CONST(X'B3')
012000940506     D F16             C                   CONST(X'B4')
012100940506     D F17             C                   CONST(X'B5')
012200940506     D F18             C                   CONST(X'B6')
012300940506     D F19             C                   CONST(X'B7')
012400940506     D F20             C                   CONST(X'B8')
012500940506     D F21             C                   CONST(X'B9')
012600940506     D F22             C                   CONST(X'BA')
012700940506     D F23             C                   CONST(X'BB')
012800940506     D F24             C                   CONST(X'BC')
012900940506     D ENTER           C                   CONST(X'F1')
013000940506     D ROLDWN          C                   CONST(X'F4')
013100940506     D ROLLUP          C                   CONST(X'F5')
013200160421     d digits          c                   '0123456789'
013300940506     D*-------------
013400941108     D DATA            C                   CONST('0001-01-01')
013500030114     D Annull          C                   CONST('  ANNULLATO  ')
013600030114     D OPz_Imm         C                   CONST('IMMISSIONE')
013700030114     D OPz_Var         C                   CONST('VARIAZIONE')
013800030114     D OPz_Del         C                   CONST(' ANNULLA  ')
013900030114     D OPz_Vis         C                   CONST('VISUALIZZA')
014000940127     C*----------------------------------------------------*
014100940127     C*                MAIN LINE PROGRAM
014200940127     C*----------------------------------------------------*
014300940223     C     $FINE         DOWEQ     *OFF
014400940202     C     $GEST         CASEQ     'D1'          GESD1
014500940117     C                   END
014600940117     C                   END
014700940325     C* fine programma
014800030113     c                   movel     tabds         kpjbu
014900030113     C                   Seton                                        LR
015000940131     C************************************************************
015100940510     C* GESTIONE VIDEO RECORD D1
015200940131     C************************************************************
015300940127     C     GESD1         BEGSR
015400030113      *
015500940225     C* inizializzazione videata
015600940223     C     $INZD1        IFEQ      *ON
015700940127     C                   EXSR      INZD1
015800940223     C                   MOVE      *OFF          $INZD1
015900940117     C                   END
016000030113      *
016100940225     C* emissione piede videata se proveniente da altra
016200940225     C* salvataggio valore formato di provenienza
016300940207     C     $LASTV        IFNE      'D1'
016400940614     C                   WRITE     T1
016500940202     C                   MOVE      $LASTV        $LASTG
016600940202     C                   MOVE      'D1'          $LASTV
016700940202     C                   END
016800940131     C*
016900940203     C                   SELECT
017000940203     C* Annullamento
017100030114     C* Visualizzazione
017200030114    >C     xtaopz        WHENEQ    '01'
017300030114     c                   movel     Opz_Imm       D1Fun
017400030114    >C     xtaopz        WHENEQ    '05'
017500030114     c                   movel     Opz_Vis       D1Fun
017600030114     C                   SETON                                        020306
017700030114    >C     xtaopz        WHENEQ    '04'
017800030114     c                   movel     Opz_Del       D1Fun
017900110902     C                   SETON                                        02
018000030114     C                   SETOFF                                         06
018100940620     C* Manutenzione
018200030113    >C     xtaopz        WHENEQ    '02'
018300030114     c                   movel     Opz_Var       D1Fun
018400030114     C                   SETON                                        02
018500030114     C                   SETOFF                                       0306
018600940620     C*
018700940620     C                   OTHER
018800940620     C*
018900030114     C                   SETOFF                                       020306
019000940203     C                   ENDSL
019100030114     C*
019200030114     c                   SETOFF                                         99
019300940620     C* Immissione/Modifica/Copia
019400030113     C*               *----------------*
019500940620     C                   EXFMT     D1
019600030113     C*               *----------------*
019700030114     C     *IN99         IFEQ      *OFF
019800030114     C*
0199000301141    C                   SELECT
020000030114     C* F3=Fine
020100030114     C     $TASTO        WHENEQ    F03
020200030114     C                   EXSR      F03D1
020300030114     C* F12=Ritorno
020400030114     C     $TASTO        WHENEQ    F12
020500030114     C                   EXSR      F12D1
020600030114     C*
0207000301141O   C                   OTHER
020800030114     C*
020900030114     C                   EXSR      CTRD1
021000030113     C*
021100940616     C     *IN99         IFEQ      *OFF
021200940616     C* F6
0213000302052    C     $TASTO        ifeq      F06
0214000301152    C     $TASTO        oreq      F23
021500161031      **
021600940224     C* eseguo aggiornamento
021700940315    >C                   EXSR      AGGANA
021800940224     C* eseguo operazioni del dopo-aggiornamento
021900030114     C  N99              EXSR      GESAGG
022000030205     c                   end
022100030114     C*
022200030114     C                   ENDIF
022300030114     C*
0224000301142-   C                   ENDSL
022500030114     C*
0226000301142-   C                   ENDIF
022700030114     C*
022800940117     C                   ENDSR
022900940117      ************************************************************
023000940207      * INIZIALIZZAZIONE VIDEATA DATI
023100940117      ************************************************************
023200940127     C     INZD1         BEGSR
023300940131     C*
023400940615     C                   CLEAR                   D1
023500161031     C                   move      'AIG'         tbeCOD
023600161031     C*
023700161031     C*  in immissione i campi chiave sono vuoti
023800161031     c                   if        xtake1 =*blank
023900161031     c                   movel     '000     '    xtake1
024000161031     c                   end
024100161031     c                   if        xtake2 =*blank
024200161031     c                   movel     '00000000'    xtake2
024300161031     c                   end
024400161031     C*
024500161031     C* Se in immissione pulisco il video
0246001610311   >C     xtaopz        IFEQ      '01'
024700161031      *
024800161031     C* Se in Immissione nuovo record
024900161031     c                   exsr      NewD1
025000161031     C*
0251001610311   >C                   Else
025200940207     C*
025300940224     C* valorizzo la chiave univoca per reperire i dati
025400940224     C* se non immissione
025500161031     C*
025600161031     C                   clear                   tbeKE1
025700161031     C                   MOVEl     xtake1        tbeKE1
025800161031     C                   clear                   tbeKE2
025900161031     C                   MOVEl     xtake2        tbeKE2
026000161031     C*
026100161031     C                   MOVEl     xtake1        dschiave
026200161031     C                   MOVEl     xtake2        h1scad
026300161031     c                   if        h1scad <> *zeros
026400161031     c                   move      h1scad        data_iso
026500161031     c                   move      data_iso      data_eur
026600161031     c                   move      data_eur      d1scad
026700161031     c                   end
026800161031     C*
026900161031     C                   EXSR      CTRD1
027000030204      *
027100940315    >C*-------------------------------------------------------------
027200940315    >C* impostare i campi chiave ricevuti
027300940315    >C*-------------------------------------------------------------
027400110927     C     KTBL          chain(N)  tntbe01L
027500940614     C*
027600110927     C                   IF        %Found(tntbe01L)
027700110927      *
027800110927     C                   MOVEL     tbeUNI        EXTDS
027900110927      *
028000110901     c*******            if        TblFLG = '*'
028100110901     c*******            seton                                        08
028200110901     C*******            MOVEl     Annull        D1Ann
028300110901     C*******            end
028400030113     C*
028500940224     C* riempio la videata
028600940224     C                   EXSR      RIED1
028700030113      *
028800940224     C* richiamo routine dei ctrl per decodificarla
028900940614    >C                   EXSR      CTRD1
029000030113      *
029100940224     C                   MOVE      *ALL'0'       IN5098
029200940224     C                   MOVEA     IN5098        *IN(50)
029300030113      *
029400940224     C* definizione indicatori su video
029500940224     C                   SETOFF                                       02
0296009402243    C                   SELECT
029700030114    >C     xtaopz        WHENEQ    '02'
029800940224     C                   SETON                                        02
029900030114    >C     xtaopz        WHENEQ    '05'
0300009402243-   C                   ENDSL
030100940224     C*
0302009402242-   C                   ENDIF
030300030114     C*
0304009402241-   C                   ENDIF
030500940224     C*
030600940117     C                   ENDSR
030700940207     C/EJECT
030800940207     C************************************************************
030900940510     C* RIEMPIMENTO VIDEATA  D1
031000940207     C************************************************************
031100030114     C     NEWD1         BEGSR
031200940207     C*
031300030114     c                   clear                   d1
031400030114     C*
031500030114     C                   ENDSR
031600030114     C************************************************************
031700030114     C* RIEMPIMENTO VIDEATA  D1
031800030114     C************************************************************
031900030114     C     RIED1         BEGSR
032000030114     C*
032100161031     c                   clear                   d1imp
032200161031     c                   clear                   d1qta
032300030204     C*
032400161031     c                   eval        d1imp  = D§AIGIMP
032500161031     c                   eval        d1qta  = D§AIGQTA
032600120412     C*
032700940207     C                   ENDSR
032800940309     C************************************************************
032900940309     C* GESTIONE F03 VIDEO D1
033000940309     C************************************************************
033100940309     C     F03D1         BEGSR
033200940309     C*
033300940309     C                   MOVE      *ON           $FINE
033400030113    >C                   MOVE      '1'           xtaret
033500940325     C*
033600940325     C                   ENDSR
033700940309     C************************************************************
033800940309     C* GESTIONE F12 VIDEO D1
033900940309     C************************************************************
034000940309     C     F12D1         BEGSR
034100940309     C*
034200940309     C                   MOVE      *ON           $FINE
034300940309     C*
034400940309     C                   ENDSR
034500940131     C************************************************************
034600940207     C* CONTROLLO VIDEATA
034700940131     C************************************************************
034800940127     C     CTRD1         BEGSR
034900940131     C*
035000940127     C                   SETOFF                                       99
035100030123      *
035200940127     C* non imposto pos. del cursore
035300940127     C                   Z-ADD     0             H1RIGA
035400940127     C                   Z-ADD     0             H1COLO
035500940621     C*
035600940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0357000301141   >C     xtaopz        IFNE      '04'
035800971110     C*
035900161031     C* controllo codice Filiale
036000161031$017 C                   IF        d1FIL = 0
036100161031     c                   clear                   d1fild
036200161031     C                   SETON                                        6199
036300161031     C                   elseIF    d1fil = 999
036400161031     c                   movel     'Azienda'     d1fild
036500161031     C                   Else
036600161031     c                   clear                   d1fild
036700161031     c     d1fil         chain     azorg01l
036800161031     c                   if        %found(azorg01l)
036900161031     c                   movel     orgdes        d1fild
037000161031     c                   else
037100161031     c                   seton                                        6199
037200161031     c                   endif
037300971110     C                   ENDIF
037400161031      **
037500161031      **  Tipo aspettativa
037600161031     C                   CLEAR                   tibs02ds
037700161031     C                   MOVEL     KNSIF         T02SIF
037800161031     C                   MOVEL     'TAI'         T02COD
037900161031      **
038000161031     c                   if        d1asp  = '?'
038100161031     C                   MOVEL     'R'           T02MOD
038200161031     c                   seton                                        99
038300161031     c                   else
038400161031     C                   MOVEL     'C'           T02MOD
038500161031     C                   movel(p)  d1asp         T02KE1
038600161031     c                   endif
038700161031      *
038800161031     c                   if        d1asp  = *blank
038900161031     c                   seton                                        6299
039000161031     c                   else
039100161031     C                   CALL      'TIBS02R'
039200161031     C                   PARM                    KPJBA
039300161031     C                   PARM                    tibs02ds
039400161031     C     T02ERR        IFEQ      *BLANKS
039500161031     C                   MOVEL     T02UNI        dtai
039600161031     c                   movel     d§taides      d1aspD
039700161031     c                   movel     t02ke1        d1asp
039800161031     C                   ELSE
039900161031     c                   clear                   d1aspD
040000161031     C                   CLEAR                   d1asp
040100161031     c                   seton                                        6299
040200161031     C                   ENDIF
040300161031     c                   end
040400161031      *
040500161031      **  Territorio
040600161031     C                   CLEAR                   tibs02ds
040700161031     C                   MOVEL     KNSIF         T02SIF
040800161031     C                   MOVEL     'ATS'         T02COD
040900161031      **
041000161031     c                   if        d1ter  = *blank
041100161031     c                   seton                                        6399
041200161031     c                   else
041300161031      **
041400161031     c                   if        d1ter  = '?'
041500161031     C                   MOVEL     'R'           T02MOD
041600161031     c                   seton                                        99
041700161031     c                   else
041800161031     C                   MOVEL     'C'           T02MOD
041900161031     C                   movel(p)  d1ter         T02KE1
042000161031     c                   endif
042100161031      *
042200161031     C                   CALL      'TIBS02R'
042300161031     C                   PARM                    KPJBA
042400161031     C                   PARM                    tibs02ds
042500161031     C     T02ERR        IFEQ      *BLANKS
042600161031     C                   MOVEL     T02UNI        dats
042700161031     c                   movel     d§atsdes      d1terd
042800161031     c                   movel     t02ke1        d1ter
042900161031     C                   ELSE
043000161031     c                   clear                   d1terd
043100161031     C                   CLEAR                   d1ter
043200161031     c                   seton                                        6399
043300161031     C                   ENDIF
043400161031     c                   end
043500161031      *
043600161031      **  Massa
043700161031     C                   CLEAR                   tibs02ds
043800161031     C                   MOVEL     KNSIF         T02SIF
043900161031     C                   MOVEL     'MAS'         T02COD
044000161031     c                   if        d1mas  = *blank
044100161031     c                   seton                                        6499
044200161031     c                   else
044300161031      **
044400161031     c                   if        d1mas  = '?'
044500161031     C                   MOVEL     'R'           T02MOD
044600161031     c                   seton                                        99
044700161031     C                   CALL      'TIBS02R'
044800161031     C                   PARM                    KPJBA
044900161031     C                   PARM                    tibs02ds
045000161031     C     T02ERR        IFEQ      *BLANKS
045100161031     C                   MOVEL     T02UNI        Dmas
045200161031     c                   movel     d§masdes      d1masd
045300161031     c                   movel     t02ke2        d1mas
045400161031     C                   ELSE
045500161031     c                   clear                   d1masd
045600161031     C                   CLEAR                   d1mas
045700161031     c                   seton                                        6499
045800161031     C                   ENDIF
045900161031     c                   else
046000161031     c                   seton                                        9964
046100161031     c     'MAS'         setll     tntbe02l
046200161031     c     'MAS'         reade     tntbe02l
046300161031     c                   dow       not %EoF(tntbe02l)
046400161031     c                   if        d1mas = %subst(T2_tbeKE2:1:1)
046500161031     C                   MOVEL     T2_tbeUNI     Dmas
046600161031     c                   movel     d§masdes      d1masD
046700161031     c                   setoff                                       9964
046800161031     c                   leave
046900161031     c                   end
047000161031     c     'MAS'         reade     tntbe02l
047100161031     C                   ENDdo
047200161031     c                   endif
047300161031     c                   end
047400161031      *
047500161031      **  Tipo Attività
047600161031     C                   CLEAR                   tibs02ds
047700161031     C                   MOVEL     KNSIF         T02SIF
047800161031     C                   MOVEL     'AAT'         T02COD
047900161031     c                   if        d1att  = *blank
048000161031     c                   seton                                        9965
048100161031     c                   else
048200161031      **
048300161031     c                   if        d1att  = '?'
048400161031     C                   MOVEL     'R'           T02MOD
048500161031     c                   seton                                        99
048600161031     c                   else
048700161031     C                   MOVEL     'C'           T02MOD
048800161031     C                   movel(p)  d1att         T02KE1
048900161031     c                   endif
049000161031      *
049100161031     C                   CALL      'TIBS02R'
049200161031     C                   PARM                    KPJBA
049300161031     C                   PARM                    tibs02ds
049400161031     C     T02ERR        IFEQ      *BLANKS
049500161031     C                   MOVEL     T02UNI        Daat
049600161031     c                   movel     d§aatdes      d1attd
049700161031     c                   movel     t02ke1        d1att
049800161031     C                   ELSE
049900161031     c                   clear                   d1attd
050000161031     C                   CLEAR                   D1att
050100161031     c                   seton                                        9965
050200161031     C                   ENDIF
050300161031     c                   end
050400161031      *
050500161031      **  Tipo Allestimento
050600161031     c                   goto      no_allesti
050700161031     C                   CLEAR                   tibs02ds
050800161031     C                   MOVEL     KNSIF         T02SIF
050900161031     C                   MOVEL     'ALL'         T02COD
051000161031     c                   if        h1all  = *blank
051100161031     c                   seton                                        9966
051200161031     c                   else
051300161031      **
051400161031     c                   if        h1all  = '?'
051500161031     C                   MOVEL     'R'           T02MOD
051600161031     c                   seton                                        99
051700161031     c                   else
051800161031     C                   MOVEL     'C'           T02MOD
051900161031     C                   movel(p)  h1all         T02KE1
052000161031     c                   endif
052100161031      *
052200161031     C                   CALL      'TIBS02R'
052300161031     C                   PARM                    KPJBA
052400161031     C                   PARM                    tibs02ds
052500161031     C     T02ERR        IFEQ      *BLANKS
052600161031     C                   MOVEL     T02UNI        Dall
052700161031     c                   movel     d§alldes      h1alld
052800161031     c                   movel     t02ke1        h1all
052900161031     C                   ELSE
053000161031     c                   clear                   h1alld
053100161031     C                   CLEAR                   h1all
053200161031     c                   seton                                        9966
053300161031     C                   ENDIF
053400161031     c                   end
053500161031     c     no_allesti    tag
053600161031      **
053700161031     C* controllo data zero
053800161031$017 C     D1scad        IFEQ      0
053900161031     C                   SETON                                        6699
054000160413     C                   Else
054100161031     C*
054200161031     C                   MOVE      D1scad        G02DAT
054300161031     C                   MOVEL     *BLANK        G02ERR
054400161031     C                   CALL      'XSRDA8'
054500161031     C                   PARM                    WLBDA8
054600161031     C*
054700161031     C     G02ERR        IFEQ      '1'
054800161031     C                   SETON                                        6699
054900161031     C                   ElsE
055000161031     c                   move      g02dat        D1scad
055100161103     c                   z-add     g02INV        h1scad
055200161031     C                   END
055300161031     C*
055400160413     C                   ENDIF
055500161031     C*
055600161031      *--------------------------------
055700161031     C* controllo
055800161031$017 C     D1qta         IFEQ      0
055900161031     C                   SETON                                        6899
056000161031     C                   ENDIF
056100161031     C* controllo
056200161031$017 C     D1imp         IFEQ      0
056300161031     C                   SETON                                        6999
056400161031     C                   ENDIF
056500161031      *--------------------------------
056600160413     C*
056700940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
056800030114    >C     xtaopz        IFEQ      '01'
056900030114    >C     xtaopz        oreq      '03'
057000110927     c                   clear                   tbeKE1
057100160413     c                   clear                   tbeKE2
057200161031     c                   movel     dschiave      tbeKE1
057300161031     c                   movel     h1scad        tbeKE2
057400110927     C     KTBL          chain(N)  tntbe01L
057500110927     c                   if        %Found(tntbe01L)
057600110908     C                   SETON                                        5399
057700030114     C                   EndIF
057800030114     C                   ENDIF
057900030114      *
0580009402241-   C                   END
058100940131     C*
058200940302     C     *IN99         IFEQ      *ON
058300940510     C     $GEST         ANDEQ     'D1'
058400940117     C                   MOVEA     *IN           WIN
058500940128     C                   MOVE      *ALL'0'       IN5098           49
058600940128     C                   MOVEA     IN5098        *IN(50)
058700940614     C                   WRITE     D1
058800940117     C                   MOVEA     WIN           *IN
058900940302     C                   ENDIF
059000940127     C*
059100940117     C                   ENDSR
059200030204     C/EJECT
059300030204     C************************************************************
059400030204    >C* AGGIORNAMENTO ANAGRAFICA
059500030204     C************************************************************
059600030204    >C     FLDAGG        BEGSR
059700030204     C*
059800030204     C*  campi chiave da scrivere e aggiornare
059900110927     C                   clear                   tbeKE1
060000160412     C                   clear                   tbeKE2
060100161031     C                   MOVEl     DsChiave      tbeKE1
060200161031     C                   MOVEl     h1scad        tbeKE2
060300030204     C*
060400030204     C                   ENDSR
060500940207     C/EJECT
060600940224     C************************************************************
060700940315    >C* AGGIORNAMENTO ANAGRAFICA
060800940224     C************************************************************
060900940315    >C     AGGANA        BEGSR
061000940207     C*
061100030204     C                   exsr      FLDAGG
061200030204     C*
061300940207     C                   SELECT
061400030204     C*
061500030114    >C     xtaopz        WHENEQ    '01'
061600030114    >C     xtaopz        OREQ      '03'
061700030204     C*
061800161031    >C                   EXSR      RIEANA_IAG
061900110927     c                   write     tntbe000                             59
062000161031      *
062100161031     c                   movel     TNTBEds       xTNTBEds
062200161031     c                   write     tntbex
062300161031      *
062400940318     C* se rcd non scrivibile attivo errore
062500030114    >C   59              SETON                                          99
062600030204      *
062700030114    >C     xtaopz        WHENEQ    '02'
062800030204      *
062900110927     C     KTBL          chain     tntbe01L
063000030204     C                   exsr      FLDAGG
063100161031    >C                   EXSR      RIEANA_IAG
063200110927     c                   update    tntbe000                             59
063300161031      *
063400161031     c     K05TBE01      chain     tntbex1l
063500161031     c                   if        %found(tntbex1l)
063600161031     c                   movel     TNTBEds       xTNTBEds
063700161031     c                   update    tntbex
063800161103     c                   else
063900161103     c                   write     tntbex
064000161103     c                   end
064100161031      *
064200030114    >C   59              SETON                                          99
064300030204      *
064400030114    >C     xtaopz        WHENEQ    '04'
064500110927     C     KTBL          chain     tntbe01L
064600030115     c                   if        $tasto = F23
064700110927     c                   delete    tntbe000                             59
064800161031      *
064900161031     c     K05TBE01      chain     tntbex1l
065000161031     c                   if        %found(tntbex1l)
065100161031     c                   delete    tntbex
065200161031     c                   end
065300161031      *
065400030115     c                   else
065500110927     c                   delete    tntbe000                             59
065600161031      *
065700161031     c     K05TBE01      chain     tntbex1l
065800161031     c                   if        %found(tntbex1l)
065900161031     c                   delete    tntbex
066000161031     c                   end
066100161031      *
066200030115     c                   end
066300030114    >C   59              SETON                                          99
066400940621     C*
066500940207     C                   ENDSL
066600940203     C*
066700940203     C                   ENDSR
066800940224     C************************************************************
066900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
067000940224     C************************************************************
067100161031     C     RIEANA_IAG    BEGSR
067200030114     C*
067300110927     C                   CLEAR                   tbeuni
067400160412     C*
067500161031     c                   z-add     d1imp         D§AIGIMP
067600161031     c                   z-add     d1qta         D§AIGQTA
067700110927     C                   MOVEL     EXTDS         tbeuni
067800130301     C*
067900130301     c                   eval      tbeFTT='S'
068000161031     c                   eval      tbeFLT=0
068100030114     C*
068200940207     C                   ENDSR
068300940224     C************************************************************
068400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
068500940224     C************************************************************
068600940224     C     GESAGG        BEGSR
068700940224     C*
068800940224     C* segnalo al pgm chiamante l'avvenuta conferma
068900030114    >C                   MOVE      *ON           xtaopr
069000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
069100030114    >C                   MOVE      '0'           xtaret
069200940224     C*
0693009402241    C                   SELECT
069400940224     C* nel caso di immissione, ripristino la videata iniziale
069500030114    >C     xtaopz        WHENEQ    '01'
069600940224     C                   MOVE      *ON           $INZD1
069700030114     C                   MOVE      *ON           $FINE
069800940224     C* altrimenti ritorno al pgm chiamante
069900940224     C                   OTHER
070000940224     C                   MOVE      *ON           $FINE
0701009402241-   C                   ENDSL
070200940224     C*
070300940224     C                   ENDSR
070400940131     C************************************************************
070500940131     C* OPERAZIONI INIZIALI
070600940131     C************************************************************
070700940131     C     *INZSR        BEGSR
070800940131     C*
070900940127     C* Reperimento parametri
071000940127     C*
071100940117     C     *ENTRY        PLIST
071200940117     C                   PARM                    KPJBA
071300030113     C                   movel     kpjbu         tabds
071400161103     C                   move      XTAtbeREC     nrec_tbe          9 0
071500030115     c                   setoff                                       08
071600030115     C*
071700940223     C* Variabili per gestione videate
071800940223     C                   MOVE      'D1'          $GEST
071900940223     C                   MOVE      *OFF          $FINE
072000940223     C                   MOVE      *ON           $INZD1
072100940224     C                   MOVE      *BLANK        $LASTG
072200940224     C                   MOVE      *BLANK        $LASTV
072300940506     C                   Z-ADD     0             $ULKD1            3 0
072400940127     C*
072500940223     C* Variabili appoggio
072600940224     C                   Z-ADD     0             CURR
072700940224     C                   Z-ADD     0             CURC
072800161031     c                   movel     *date         Wdate
072900161031      *
073000161031      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
073100161031     c     *dtaara       define    §azute        AZUTEds
073200161031     c     *dtaara       define    §datiute      dDatiUte
073300161031      *
073400161031     c                   clear                   AZUTEds
073500161031     c                   clear                   dDatiUte
073600161031     c                   clear                   TIBS34ds
073700161031     c                   in(E)     *dtaara
073800161031if  1c                   if        %Error  or  RSUT = *blanks
073900161031     c                   call      'TIBS34R'
074000161031     c                   parm                    TIBS34ds
074100161031     c                   in        *dtaara
074200161031e   1c                   endif
074300940207     C*
074400940207     C* Valorizzazione campi univoci testate
074500940614     C                   CLEAR                   T1
074600940614     C                   MOVEL     KNSIF         NOMSIF
074700161031     C                   MOVEL     rsut          NOMDIT
074800941027     C                   MOVEL     DSPGM         NOMPGM
074900030113     C*
075000161031     C                   move      'AIG'         tbeCOD
075100110927     C                   clear                   tbeKE1
075200161031     C                   moveL     xtake1        tbeKE1
075300160413     C                   clear                   tbeKE2
075400160413     C                   MOVEl     xtake2        tbeKE2
075500940224     C*
075600940224     C* Inizializzazione/Decodifica prima videata
075700940224     C                   EXSR      INZD1
075800940224     C                   MOVE      *OFF          $INZD1
075900940127     C*
076000940117     C                   ENDSR
076100940128     C************************************************************
076200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
076300940223     C************************************************************
076400940223     C     DEFCAM        BEGSR
076500940223     C*
076600940223     C* klist
076700030113     C     KTBL          KLIST
076800110927     C                   KFLD                    tbeCOD
076900110927     C                   KFLD                    tbeKE1
077000160412     C                   KFLD                    tbeKE2
077100161031     C*
077200161031      * Definizioni chiavi di accesso
077300161031     c     K05TBE01      klist                                                  *tntbe01l
077400161031     c                   kfld                    TBEcod                         -tabella
077500161031     c                   kfld                    TBEke1                         -chiave uno
077600161031     c                   kfld                    TBEke2                         -chiave due
077700161031     c                   kfld                    TBElin                         -lingua
077800161031     c                   kfld                    TBEsif                         -s.informativo
077900940207     C*
078000940223     C* Variabili per gestione videate
078100940223     C                   MOVE      *BLANK        $GEST             2
078200940223     C                   MOVE      *BLANK        $FINE             1
078300940223     C                   MOVE      *BLANK        $INZD1            1
078400940223     C                   MOVE      *BLANK        $LASTG            2
078500940223     C                   MOVE      *BLANK        $LASTV            2
078600940223     C* Variabili appoggio
078700940223     C                   Z-ADD     0             CURR              2 0
078800940223     C                   Z-ADD     0             CURC              2 0
078900940223     C                   MOVE      *ZEROS        WIN              99
079000940224     C                   MOVE      *BLANK        WTBERR            1
079100940223     C* Indici
079200940223     C                   Z-ADD     0             X                 3 0
079300940223     C                   Z-ADD     0             Y                 3 0
079400940127     C*
079500940117     C                   ENDSR
079600940128     C************************************************************
