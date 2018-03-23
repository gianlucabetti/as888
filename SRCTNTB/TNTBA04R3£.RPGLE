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
002500161103     Ftntbe00F  UF A E             DISK
002600161103     fTNTBE01L  if   e           k disk    rename(TNTBE000:TNTBE1)
002700161031     fTNTBE02L  if   e           k disk    rename(TNTBE000:TNTBE2)
002800161031     f                                     prefix(T2_)
002900161031     fTNTBEx1L  Uf A e           k disk    rename(tntbe000:tntbex) prefix(x)
003000161031      *
003100161031      *
003200161031     fazorg01l  if   e           k disk
003300940128     D*----------------------------------------------------*
003400161031$003 D EXTDS         E DS                  EXTNAME(DAIG)
003500030113      *-------------
003600940127     D* Passaggio Parametri
003700940127     D KPJBA         E DS
003800030113      *-------------
003900030113     D* Parametri in ricezione
004000030113     D  TABDS          DS
004100030113     D  XTAOPZ                 1      2
004200030113     D  XTARET                 3      3
004300030113     D  XTAOPR                 4      4
004400030113     D  XTAERR                 5      5
004500161031     D  XTAKE1                 6     20
004600161031     D  XTAKE2                21     35
004700161103     D  XTAtberec             36     44
004800161031      *---------------------------------------------------------------*
004900161031     d wDate           s               d   datfmt(*iso)  inz(*loval)
005000161031     D DATA_eur        S               D   DATFMT(*eur)
005100161031     D DATA_amg        S               D   DATFMT(*iso)
005200161031     D DATA_iso        S               D   DATFMT(*iso)
005300161031     D*-------------
005400161031     D WLBDA8          DS
005500161031     D  G02DAT                 1      8  0
005600161031     D  G02INV                 9     16  0
005700161031     D  G02ERR                17     17
005800161031     D  G02TGI                18     22  0
005900161031      *
006000161031      *
006100161031     d TIBS34ds      e ds                  inz
006200161031     d dDatiUte      e ds                  inz
006300161031     d AZUTEds       e ds                  extname(AZUTE00F) inz
006400161031      *
006500161031     d TNTBEds       e ds                  extname(TNTBE00F) inz
006600161031     d xTNTBEds      e ds                  extname(TNTBEx1l) inz
006700161031     d                                     prefix(x)
006800161031      *
006900161031     d dschiave        ds            15
007000161031     d  d1fil
007100161031     d  d1asp
007200161031     d  d1ter
007300161031     d  d1mas
007400161031     d  d1att
007500161031     d  h1all
007600161031      *
007700161031     D tibs02ds      E DS
007800161031      *
007900161031      * Tabella
008000161031     D dTAI          E DS
008100161031     D dATS          E DS
008200161031     D dMAS          E DS
008300161031     D dAAT          E DS
008400161031     D dALL          E DS
008500940201     D*-------------
008600030204     D  S_TASTO        S                   like($Tasto)
008700940201     D DSFMT           DS
008800940506     D  $TASTO               369    369
008900940201     D  NRG                  370    370
009000940201     D  NCL                  371    371
009100940201     D*-------------
009200940201     D* posizione cursore
009300940201     D CURSOR          DS
009400940223     D  RIRI                   1      2B 0 INZ
009500940201     D  R$$                    2      2
009600940223     D  COCO                   3      4B 0 INZ
009700940201     D  C$$                    4      4
009800940207     D*-------------
009900940207     D* Reperimento nome PGM
010000940207     D STATUS         SDS           333
010100940207     D  DSPGM            *PROC
010200940225     D*-------------
010300940225     D* COSTANTI
010400940225     D*-------------
010500940506     D* Tasti di funzione
010600940506     D F01             C                   CONST(X'31')
010700940506     D F02             C                   CONST(X'32')
010800940506     D F03             C                   CONST(X'33')
010900940506     D F04             C                   CONST(X'34')
011000940506     D F05             C                   CONST(X'35')
011100940506     D F06             C                   CONST(X'36')
011200940506     D F07             C                   CONST(X'37')
011300940506     D F08             C                   CONST(X'38')
011400940506     D F09             C                   CONST(X'39')
011500940506     D F10             C                   CONST(X'3A')
011600940506     D F11             C                   CONST(X'3B')
011700940506     D F12             C                   CONST(X'3C')
011800940506     D F13             C                   CONST(X'B1')
011900940506     D F14             C                   CONST(X'B2')
012000940506     D F15             C                   CONST(X'B3')
012100940506     D F16             C                   CONST(X'B4')
012200940506     D F17             C                   CONST(X'B5')
012300940506     D F18             C                   CONST(X'B6')
012400940506     D F19             C                   CONST(X'B7')
012500940506     D F20             C                   CONST(X'B8')
012600940506     D F21             C                   CONST(X'B9')
012700940506     D F22             C                   CONST(X'BA')
012800940506     D F23             C                   CONST(X'BB')
012900940506     D F24             C                   CONST(X'BC')
013000940506     D ENTER           C                   CONST(X'F1')
013100940506     D ROLDWN          C                   CONST(X'F4')
013200940506     D ROLLUP          C                   CONST(X'F5')
013300160421     d digits          c                   '0123456789'
013400940506     D*-------------
013500941108     D DATA            C                   CONST('0001-01-01')
013600030114     D Annull          C                   CONST('  ANNULLATO  ')
013700030114     D OPz_Imm         C                   CONST('IMMISSIONE')
013800030114     D OPz_Var         C                   CONST('VARIAZIONE')
013900030114     D OPz_Del         C                   CONST(' ANNULLA  ')
014000030114     D OPz_Vis         C                   CONST('VISUALIZZA')
014100940127     C*----------------------------------------------------*
014200940127     C*                MAIN LINE PROGRAM
014300940127     C*----------------------------------------------------*
014400940223     C     $FINE         DOWEQ     *OFF
014500940202     C     $GEST         CASEQ     'D1'          GESD1
014600940117     C                   END
014700940117     C                   END
014800940325     C* fine programma
014900030113     c                   movel     tabds         kpjbu
015000030113     C                   Seton                                        LR
015100940131     C************************************************************
015200940510     C* GESTIONE VIDEO RECORD D1
015300940131     C************************************************************
015400940127     C     GESD1         BEGSR
015500030113      *
015600940225     C* inizializzazione videata
015700940223     C     $INZD1        IFEQ      *ON
015800940127     C                   EXSR      INZD1
015900940223     C                   MOVE      *OFF          $INZD1
016000940117     C                   END
016100030113      *
016200940225     C* emissione piede videata se proveniente da altra
016300940225     C* salvataggio valore formato di provenienza
016400940207     C     $LASTV        IFNE      'D1'
016500940614     C                   WRITE     T1
016600940202     C                   MOVE      $LASTV        $LASTG
016700940202     C                   MOVE      'D1'          $LASTV
016800940202     C                   END
016900940131     C*
017000940203     C                   SELECT
017100940203     C* Annullamento
017200030114     C* Visualizzazione
017300030114    >C     xtaopz        WHENEQ    '01'
017400030114     c                   movel     Opz_Imm       D1Fun
017500030114    >C     xtaopz        WHENEQ    '05'
017600030114     c                   movel     Opz_Vis       D1Fun
017700030114     C                   SETON                                        020306
017800030114    >C     xtaopz        WHENEQ    '04'
017900030114     c                   movel     Opz_Del       D1Fun
018000110902     C                   SETON                                        02
018100030114     C                   SETOFF                                         06
018200940620     C* Manutenzione
018300030113    >C     xtaopz        WHENEQ    '02'
018400030114     c                   movel     Opz_Var       D1Fun
018500030114     C                   SETON                                        02
018600030114     C                   SETOFF                                       0306
018700940620     C*
018800940620     C                   OTHER
018900940620     C*
019000030114     C                   SETOFF                                       020306
019100940203     C                   ENDSL
019200030114     C*
019300030114     c                   SETOFF                                         99
019400940620     C* Immissione/Modifica/Copia
019500030113     C*               *----------------*
019600940620     C                   EXFMT     D1
019700030113     C*               *----------------*
019800030114     C     *IN99         IFEQ      *OFF
019900030114     C*
0200000301141    C                   SELECT
020100030114     C* F3=Fine
020200030114     C     $TASTO        WHENEQ    F03
020300030114     C                   EXSR      F03D1
020400030114     C* F12=Ritorno
020500030114     C     $TASTO        WHENEQ    F12
020600030114     C                   EXSR      F12D1
020700030114     C*
0208000301141O   C                   OTHER
020900030114     C*
021000030114     C                   EXSR      CTRD1
021100030113     C*
021200940616     C     *IN99         IFEQ      *OFF
021300940616     C* F6
0214000302052    C     $TASTO        ifeq      F06
0215000301152    C     $TASTO        oreq      F23
021600161031      **
021700940224     C* eseguo aggiornamento
021800940315    >C                   EXSR      AGGANA
021900940224     C* eseguo operazioni del dopo-aggiornamento
022000030114     C  N99              EXSR      GESAGG
022100030205     c                   end
022200030114     C*
022300030114     C                   ENDIF
022400030114     C*
0225000301142-   C                   ENDSL
022600030114     C*
0227000301142-   C                   ENDIF
022800030114     C*
022900940117     C                   ENDSR
023000940117      ************************************************************
023100940207      * INIZIALIZZAZIONE VIDEATA DATI
023200940117      ************************************************************
023300940127     C     INZD1         BEGSR
023400940131     C*
023500940615     C                   CLEAR                   D1
023600161031     C                   move      'AIG'         tbeCOD
023700161031     C*
023800161031     C*  in immissione i campi chiave sono vuoti
023900161031     c                   if        xtake1 =*blank
024000161031     c                   movel     '000     '    xtake1
024100161031     c                   end
024200161031     c                   if        xtake2 =*blank
024300161031     c                   movel     '00000000'    xtake2
024400161031     c                   end
024500161031     C*
024600161031     C* Se in immissione pulisco il video
0247001610311   >C     xtaopz        IFEQ      '01'
024800161031      *
024900161031     C* Se in Immissione nuovo record
025000161031     c                   exsr      NewD1
025100161031     C*
0252001610311   >C                   Else
025300940207     C*
025400940224     C* valorizzo la chiave univoca per reperire i dati
025500940224     C* se non immissione
025600161031     C*
025700161031     C                   clear                   tbeKE1
025800161031     C                   MOVEl     xtake1        tbeKE1
025900161031     C                   clear                   tbeKE2
026000161031     C                   MOVEl     xtake2        tbeKE2
026100161031     C*
026200161031     C                   MOVEl     xtake1        dschiave
026300161031     C                   MOVEl     xtake2        h1scad
026400161103     C                   MOVEl     xtake2        h1oldscad
026500161031     c                   if        h1scad <> *zeros
026600161031     c                   move      h1scad        data_iso
026700161031     c                   move      data_iso      data_eur
026800161031     c                   move      data_eur      d1scad
026900161031     c                   end
027000161031     C*
027100161031     C                   EXSR      CTRD1
027200030204      *
027300940315    >C*-------------------------------------------------------------
027400940315    >C* impostare i campi chiave ricevuti
027500940315    >C*-------------------------------------------------------------
027600161103     C     tbe_rec       chain(N)  tntbe00F
027700940614     C*
027800161103     C                   IF        %Found(tntbe00F)
027900110927      *
028000110927     C                   MOVEL     tbeUNI        EXTDS
028100110927      *
028200110901     c*******            if        TblFLG = '*'
028300110901     c*******            seton                                        08
028400110901     C*******            MOVEl     Annull        D1Ann
028500110901     C*******            end
028600030113     C*
028700940224     C* riempio la videata
028800940224     C                   EXSR      RIED1
028900030113      *
029000940224     C* richiamo routine dei ctrl per decodificarla
029100940614    >C                   EXSR      CTRD1
029200030113      *
029300940224     C                   MOVE      *ALL'0'       IN5098
029400940224     C                   MOVEA     IN5098        *IN(50)
029500030113      *
029600940224     C* definizione indicatori su video
029700940224     C                   SETOFF                                       02
0298009402243    C                   SELECT
029900030114    >C     xtaopz        WHENEQ    '02'
030000940224     C                   SETON                                        02
030100030114    >C     xtaopz        WHENEQ    '05'
0302009402243-   C                   ENDSL
030300940224     C*
0304009402242-   C                   ENDIF
030500030114     C*
0306009402241-   C                   ENDIF
030700940224     C*
030800940117     C                   ENDSR
030900940207     C/EJECT
031000940207     C************************************************************
031100940510     C* RIEMPIMENTO VIDEATA  D1
031200940207     C************************************************************
031300030114     C     NEWD1         BEGSR
031400940207     C*
031500030114     c                   clear                   d1
031600161103     c                   z-add     31122039      d1scad
031700030114     C*
031800030114     C                   ENDSR
031900030114     C************************************************************
032000030114     C* RIEMPIMENTO VIDEATA  D1
032100030114     C************************************************************
032200030114     C     RIED1         BEGSR
032300030114     C*
032400161031     c                   clear                   d1imp
032500161031     c                   clear                   d1qta
032600030204     C*
032700161031     c                   eval        d1imp  = D§AIGIMP
032800161031     c                   eval        d1qta  = D§AIGQTA
032900120412     C*
033000940207     C                   ENDSR
033100940309     C************************************************************
033200940309     C* GESTIONE F03 VIDEO D1
033300940309     C************************************************************
033400940309     C     F03D1         BEGSR
033500940309     C*
033600940309     C                   MOVE      *ON           $FINE
033700030113    >C                   MOVE      '1'           xtaret
033800940325     C*
033900940325     C                   ENDSR
034000940309     C************************************************************
034100940309     C* GESTIONE F12 VIDEO D1
034200940309     C************************************************************
034300940309     C     F12D1         BEGSR
034400940309     C*
034500940309     C                   MOVE      *ON           $FINE
034600940309     C*
034700940309     C                   ENDSR
034800940131     C************************************************************
034900940207     C* CONTROLLO VIDEATA
035000940131     C************************************************************
035100940127     C     CTRD1         BEGSR
035200940131     C*
035300940127     C                   SETOFF                                       99
035400030123      *
035500940127     C* non imposto pos. del cursore
035600940127     C                   Z-ADD     0             H1RIGA
035700940127     C                   Z-ADD     0             H1COLO
035800940621     C*
035900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0360000301141   >C     xtaopz        IFNE      '04'
036100971110     C*
036200161031     C* controllo codice Filiale
036300161109     c                   clear                   d1fild
036400161031$017 C                   IF        d1FIL = 0
036500161031     C                   SETON                                        6199
036600161109     c                   goto      errd1
036700161031     C                   elseIF    d1fil = 999
036800161031     c                   movel     'Azienda'     d1fild
036900161031     C                   Else
037000161031     c     d1fil         chain     azorg01l
037100161031     c                   if        %found(azorg01l)
037200161031     c                   movel     orgdes        d1fild
037300161031     c                   else
037400161031     c                   seton                                        6199
037500161109     c                   goto      errd1
037600161109     c                   endif
037700971110     C                   ENDIF
037800161031      **
037900161031      **  Tipo aspettativa
038000161031     C                   CLEAR                   tibs02ds
038100161031     C                   MOVEL     KNSIF         T02SIF
038200161031     C                   MOVEL     'TAI'         T02COD
038300161031      **
038400161031     c                   if        d1asp  = '?'
038500161031     C                   MOVEL     'R'           T02MOD
038600161031     c                   seton                                        99
038700161031     c                   else
038800161031     C                   MOVEL     'C'           T02MOD
038900161031     C                   movel(p)  d1asp         T02KE1
039000161031     c                   endif
039100161031      *
039200161109     c                   clear                   d1aspD
039300161031     c                   if        d1asp  = *blank
039400161031     c                   seton                                        6299
039500161109     c                   goto      errd1
039600161031     c                   else
039700161031     C                   CALL      'TIBS02R'
039800161031     C                   PARM                    KPJBA
039900161031     C                   PARM                    tibs02ds
040000161031     C     T02ERR        IFEQ      *BLANKS
040100161031     C                   MOVEL     T02UNI        dtai
040200161031     c                   movel     d§taides      d1aspD
040300161031     c                   movel     t02ke1        d1asp
040400161031     C                   ELSE
040500161031     C                   CLEAR                   d1asp
040600161031     c                   seton                                        6299
040700161109     c                   goto      errd1
040800161031     C                   ENDIF
040900161031     c                   end
041000161031      *
041100161031      **  Territorio
041200161031     C                   CLEAR                   tibs02ds
041300161031     C                   MOVEL     KNSIF         T02SIF
041400161031     C                   MOVEL     'ATS'         T02COD
041500161031      **
041600161031     c                   if        d1ter  = *blank
041700161031     c                   seton                                        6399
041800161109     c                   goto      errd1
041900161031     c                   else
042000161031      **
042100161109     c                   clear                   d1terd
042200161031     c                   if        d1ter  = '?'
042300161031     C                   MOVEL     'R'           T02MOD
042400161031     c                   seton                                        99
042500161031     c                   else
042600161031     C                   MOVEL     'C'           T02MOD
042700161031     C                   movel(p)  d1ter         T02KE1
042800161031     c                   endif
042900161031      *
043000161031     C                   CALL      'TIBS02R'
043100161031     C                   PARM                    KPJBA
043200161031     C                   PARM                    tibs02ds
043300161031     C     T02ERR        IFEQ      *BLANKS
043400161031     C                   MOVEL     T02UNI        dats
043500161031     c                   movel     d§atsdes      d1terd
043600161031     c                   movel     t02ke1        d1ter
043700161031     C                   ELSE
043800161031     C                   CLEAR                   d1ter
043900161031     c                   seton                                        6399
044000161109     c                   goto      errd1
044100161031     C                   ENDIF
044200161031     c                   end
044300161031      *
044400161031      **  Massa
044500161031     C                   CLEAR                   tibs02ds
044600161031     C                   MOVEL     KNSIF         T02SIF
044700161031     C                   MOVEL     'MAS'         T02COD
044800161109     C                   CLEAR                   d1masD
044900161109      **
045000161031     c                   if        d1mas  = *blank
045100161031     c                   seton                                        6499
045200161109     c                   goto      errd1
045300161031     c                   else
045400161031      **
045500161031     c                   if        d1mas  = '?'
045600161031     C                   MOVEL     'R'           T02MOD
045700161031     c                   seton                                        99
045800161031     C                   CALL      'TIBS02R'
045900161031     C                   PARM                    KPJBA
046000161031     C                   PARM                    tibs02ds
046100161031     C     T02ERR        IFEQ      *BLANKS
046200161031     C                   MOVEL     T02UNI        Dmas
046300161031     c                   movel     d§masdes      d1masd
046400161031     c                   movel     t02ke2        d1mas
046500161031     C                   ELSE
046600161109     c                   clear                   d1mas
046700161031     c                   seton                                        6499
046800161109     c                   goto      errd1
046900161031     C                   ENDIF
047000161031     c                   else
047100161031     c                   seton                                        9964
047200161031     c     'MAS'         setll     tntbe02l
047300161031     c     'MAS'         reade     tntbe02l
047400161031     c                   dow       not %EoF(tntbe02l)
047500161031     c                   if        d1mas = %subst(T2_tbeKE2:1:1)
047600161031     C                   MOVEL     T2_tbeUNI     Dmas
047700161031     c                   movel     d§masdes      d1masD
047800161031     c                   setoff                                       9964
047900161031     c                   leave
048000161031     c                   end
048100161031     c     'MAS'         reade     tntbe02l
048200161031     C                   ENDdo
048300161109     c   99              goto      errd1
048400161031     c                   endif
048500161031     c                   end
048600161031      *
048700161031      **  Tipo Attività
048800161031     C                   CLEAR                   tibs02ds
048900161031     C                   MOVEL     KNSIF         T02SIF
049000161031     C                   MOVEL     'AAT'         T02COD
049100161109     c                   clear                   d1attd
049200161031     c                   if        d1att  = *blank
049300161031     c                   seton                                        9965
049400161109     c                   goto      errd1
049500161031     c                   else
049600161031      **
049700161031     c                   if        d1att  = '?'
049800161031     C                   MOVEL     'R'           T02MOD
049900161031     c                   seton                                        99
050000161031     c                   else
050100161031     C                   MOVEL     'C'           T02MOD
050200161031     C                   movel(p)  d1att         T02KE1
050300161031     c                   endif
050400161031      *
050500161031     C                   CALL      'TIBS02R'
050600161031     C                   PARM                    KPJBA
050700161031     C                   PARM                    tibs02ds
050800161031     C     T02ERR        IFEQ      *BLANKS
050900161031     C                   MOVEL     T02UNI        Daat
051000161031     c                   movel     d§aatdes      d1attd
051100161031     c                   movel     t02ke1        d1att
051200161031     C                   ELSE
051300161031     C                   CLEAR                   D1att
051400161031     c                   seton                                        9965
051500161109     c                   goto      errd1
051600161031     C                   ENDIF
051700161031     c                   end
051800161031      *
051900161031      **  Tipo Allestimento
052000161031     c                   goto      no_allesti
052100161031     C                   CLEAR                   tibs02ds
052200161031     C                   MOVEL     KNSIF         T02SIF
052300161031     C                   MOVEL     'ALL'         T02COD
052400161109     c                   clear                   h1alld
052500161031     c                   if        h1all  = *blank
052600161031     c                   seton                                        9966
052700161109     c                   goto      errd1
052800161031     c                   else
052900161031      **
053000161031     c                   if        h1all  = '?'
053100161031     C                   MOVEL     'R'           T02MOD
053200161031     c                   seton                                        99
053300161031     c                   else
053400161031     C                   MOVEL     'C'           T02MOD
053500161031     C                   movel(p)  h1all         T02KE1
053600161031     c                   endif
053700161031      *
053800161031     C                   CALL      'TIBS02R'
053900161031     C                   PARM                    KPJBA
054000161031     C                   PARM                    tibs02ds
054100161031     C     T02ERR        IFEQ      *BLANKS
054200161031     C                   MOVEL     T02UNI        Dall
054300161031     c                   movel     d§alldes      h1alld
054400161031     c                   movel     t02ke1        h1all
054500161031     C                   ELSE
054600161031     C                   CLEAR                   h1all
054700161031     c                   seton                                        9966
054800161109     c                   goto      errd1
054900161031     C                   ENDIF
055000161031     c                   end
055100161031     c     no_allesti    tag
055200161031      **
055300161031     C* controllo data zero
055400161031$017 C     D1scad        IFEQ      0
055500161031     C                   SETON                                        6699
055600161109     c                   goto      errd1
055700160413     C                   Else
055800161031     C*
055900161031     C                   MOVE      D1scad        G02DAT
056000161031     C                   MOVEL     *BLANK        G02ERR
056100161031     C                   CALL      'XSRDA8'
056200161031     C                   PARM                    WLBDA8
056300161031     C*
056400161031     C     G02ERR        IFEQ      '1'
056500161031     C                   SETON                                        6699
056600161109     c                   goto      errd1
056700161031     C                   ElsE
056800161031     c                   move      g02dat        D1scad
056900161103     c                   z-add     g02INV        h1scad
057000161031     C                   END
057100161031     C*
057200161103     c* non deve essere retroattiva e deve essere fine mese
057300161104     C*     NON PIU' il FINE MESE  ma qualsiasi data
057400161103     c                   if        not *in66
057500161103     c                   move      D1scad        data_eur
057600161103     c                   move      data_eur      data_iso
057700161103     C*
057800161103    4c                   if        data_iso < wdate
057900161103     c                   seton                                        6799
058000161104    4c******             else
058100161104     c******             extrct    data_iso:*m   mesep             2 0
058200161104     c******             adddur    1:*d          data_iso
058300161104     c******             extrct    data_iso:*m   mesed             2 0
058400161104    5c******             if        mesep = mesed
058500161104     c******             seton                                        6799
058600161104    5C******             end
058700161103    4C                   end
058800161103    4C                   end
058900161103     C*
059000160413     C                   ENDIF
059100161031     C*
059200161031      *--------------------------------
059300161031     C* controllo
059400161031$017 C     D1qta         IFEQ      0
059500161031     C                   SETON                                        6899
059600161109     c                   goto      errd1
059700161031     C                   ENDIF
059800161031     C* controllo
059900161031$017 C     D1imp         IFEQ      0
060000161031     C                   SETON                                        6999
060100161109     c                   goto      errd1
060200161031     C                   ENDIF
060300161031      *--------------------------------
060400160413     C*
060500940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
060600030114    >C     xtaopz        IFEQ      '01'
060700030114    >C     xtaopz        oreq      '03'
060800110927     c                   clear                   tbeKE1
060900160413     c                   clear                   tbeKE2
061000161031     c                   movel     dschiave      tbeKE1
061100161031     c                   movel     h1scad        tbeKE2
061200161103     C     KTBE          chain     tntbe01L
061300161103     c                   if        %Found(tntbe01L)
061400110908     C                   SETON                                        5399
061500161109     c                   goto      errd1
061600030114     C                   EndIF
061700030114     C                   ENDIF
061800030114      *
0619009402241-   C                   END
062000940131     C*
0621001611091-   C     errD1         tag
062200161109     C*
062300940302     C     *IN99         IFEQ      *ON
062400940510     C     $GEST         ANDEQ     'D1'
062500940117     C                   MOVEA     *IN           WIN
062600940128     C                   MOVE      *ALL'0'       IN5098           49
062700940128     C                   MOVEA     IN5098        *IN(50)
062800940614     C                   WRITE     D1
062900940117     C                   MOVEA     WIN           *IN
063000940302     C                   ENDIF
063100940127     C*
063200940117     C                   ENDSR
063300030204     C/EJECT
063400030204     C************************************************************
063500030204    >C* AGGIORNAMENTO ANAGRAFICA
063600030204     C************************************************************
063700030204    >C     FLDAGG        BEGSR
063800030204     C*
063900030204     C*  campi chiave da scrivere e aggiornare
064000110927     C                   clear                   tbeKE1
064100160412     C                   clear                   tbeKE2
064200161031     C                   MOVEl     DsChiave      tbeKE1
064300161031     C                   MOVEl     h1scad        tbeKE2
064400030204     C*
064500030204     C                   ENDSR
064600940207     C/EJECT
064700940224     C************************************************************
064800940315    >C* AGGIORNAMENTO ANAGRAFICA
064900940224     C************************************************************
065000940315    >C     AGGANA        BEGSR
065100940207     C*
065200030204     C                   exsr      FLDAGG
065300030204     C*
065400940207     C                   SELECT
065500030204     C*
065600030114    >C     xtaopz        WHENEQ    '01'
065700030114    >C     xtaopz        OREQ      '03'
065800030204     C*
065900161031    >C                   EXSR      RIEANA_IAG
066000110927     c                   write     tntbe000                             59
066100161031      *
066200161103     c  n59              movel     TNTBEds       xTNTBEds
066300161103     c  n59              write     tntbex                               59
066400161031      *
066500940318     C* se rcd non scrivibile attivo errore
066600030114    >C   59              SETON                                          99
066700030204      *
066800030114    >C     xtaopz        WHENEQ    '02'
066900030204      *
067000161103     C     tbe_rec       chain     tntbe00F
067100030204     C                   exsr      FLDAGG
067200161031    >C                   EXSR      RIEANA_IAG
067300110927     c                   update    tntbe000                             59
067400161031      *
067500161103     c                   if        not *in59
067600161103     c                   movel(p)  H1OLDSCAD     chiave2
067700161031     c     K05TBE01      chain     tntbex1l
067800161031     c                   if        %found(tntbex1l)
067900161031     c                   movel     TNTBEds       xTNTBEds
068000161103     c                   update    tntbex                               59
068100161103     c                   else
068200161103     c                   movel     TNTBEds       xTNTBEds
068300161103     c                   write     tntbex                               59
068400161103     c                   end
068500161103     c                   end
068600161031      *
068700030114    >C   59              SETON                                          99
068800030204      *
068900030114    >C     xtaopz        WHENEQ    '04'
069000161103     C     tbe_rec       chain     tntbe00F
069100030115     c                   if        $tasto = F23
069200110927     c                   delete    tntbe000                             59
069300161031      *
069400161103     c                   movel(p)  H1OLDSCAD     chiave2
069500161031     c     K05TBE01      chain     tntbex1l
069600161031     c                   if        %found(tntbex1l)
069700161031     c                   delete    tntbex
069800161031     c                   end
069900161031      *
070000030115     c                   else
070100110927     c                   delete    tntbe000                             59
070200161031      *
070300161103     c                   movel(p)  H1OLDSCAD     chiave2
070400161031     c     K05TBE01      chain     tntbex1l
070500161031     c                   if        %found(tntbex1l)
070600161031     c                   delete    tntbex
070700161031     c                   end
070800161031      *
070900030115     c                   end
071000030114    >C   59              SETON                                          99
071100940621     C*
071200940207     C                   ENDSL
071300940203     C*
071400940203     C                   ENDSR
071500940224     C************************************************************
071600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
071700940224     C************************************************************
071800161031     C     RIEANA_IAG    BEGSR
071900030114     C*
072000110927     C                   CLEAR                   tbeuni
072100160412     C*
072200161031     c                   z-add     d1imp         D§AIGIMP
072300161031     c                   z-add     d1qta         D§AIGQTA
072400110927     C                   MOVEL     EXTDS         tbeuni
072500130301     C*
072600161104     c                   eval      TBEAPL='BI'
072700130301     c                   eval      tbeFTT='S'
072800161031     c                   eval      tbeFLT=0
072900030114     C*
073000940207     C                   ENDSR
073100940224     C************************************************************
073200940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
073300940224     C************************************************************
073400940224     C     GESAGG        BEGSR
073500940224     C*
073600940224     C* segnalo al pgm chiamante l'avvenuta conferma
073700030114    >C                   MOVE      *ON           xtaopr
073800940224     C* segnalo al pgm chiamante l'aver premuto l'F6
073900030114    >C                   MOVE      '0'           xtaret
074000940224     C*
0741009402241    C                   SELECT
074200940224     C* nel caso di immissione, ripristino la videata iniziale
074300030114    >C     xtaopz        WHENEQ    '01'
074400940224     C                   MOVE      *ON           $INZD1
074500030114     C                   MOVE      *ON           $FINE
074600940224     C* altrimenti ritorno al pgm chiamante
074700940224     C                   OTHER
074800940224     C                   MOVE      *ON           $FINE
0749009402241-   C                   ENDSL
075000940224     C*
075100940224     C                   ENDSR
075200940131     C************************************************************
075300940131     C* OPERAZIONI INIZIALI
075400940131     C************************************************************
075500940131     C     *INZSR        BEGSR
075600940131     C*
075700940127     C* Reperimento parametri
075800940127     C*
075900940117     C     *ENTRY        PLIST
076000940117     C                   PARM                    KPJBA
076100030113     C                   movel     kpjbu         tabds
076200161103     C                   move      XTAtbeREC     tbe_rec           9 0
076300030115     c                   setoff                                       08
076400030115     C*
076500940223     C* Variabili per gestione videate
076600940223     C                   MOVE      'D1'          $GEST
076700940223     C                   MOVE      *OFF          $FINE
076800940223     C                   MOVE      *ON           $INZD1
076900940224     C                   MOVE      *BLANK        $LASTG
077000940224     C                   MOVE      *BLANK        $LASTV
077100940506     C                   Z-ADD     0             $ULKD1            3 0
077200940127     C*
077300940223     C* Variabili appoggio
077400940224     C                   Z-ADD     0             CURR
077500940224     C                   Z-ADD     0             CURC
077600161031     c                   movel     *date         Wdate
077700161031      *
077800161031      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
077900161031     c     *dtaara       define    §azute        AZUTEds
078000161031     c     *dtaara       define    §datiute      dDatiUte
078100161031      *
078200161031     c                   clear                   AZUTEds
078300161031     c                   clear                   dDatiUte
078400161031     c                   clear                   TIBS34ds
078500161031     c                   in(E)     *dtaara
078600161031if  1c                   if        %Error  or  RSUT = *blanks
078700161031     c                   call      'TIBS34R'
078800161031     c                   parm                    TIBS34ds
078900161031     c                   in        *dtaara
079000161031e   1c                   endif
079100940207     C*
079200940207     C* Valorizzazione campi univoci testate
079300940614     C                   CLEAR                   T1
079400940614     C                   MOVEL     KNSIF         NOMSIF
079500161031     C                   MOVEL     rsut          NOMDIT
079600941027     C                   MOVEL     DSPGM         NOMPGM
079700030113     C*
079800161031     C                   move      'AIG'         tbeCOD
079900110927     C                   clear                   tbeKE1
080000161031     C                   moveL     xtake1        tbeKE1
080100160413     C                   clear                   tbeKE2
080200160413     C                   MOVEl     xtake2        tbeKE2
080300940224     C*
080400940224     C* Inizializzazione/Decodifica prima videata
080500940224     C                   EXSR      INZD1
080600940224     C                   MOVE      *OFF          $INZD1
080700940127     C*
080800940117     C                   ENDSR
080900940128     C************************************************************
081000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
081100940223     C************************************************************
081200940223     C     DEFCAM        BEGSR
081300940223     C*
081400940223     C* klist
081500161103     C     KTBE          KLIST
081600110927     C                   KFLD                    tbeCOD
081700110927     C                   KFLD                    tbeKE1
081800160412     C                   KFLD                    tbeKE2
081900161031     C*
082000161031      * Definizioni chiavi di accesso
082100161031     c     K05TBE01      klist                                                  *tntbe01l
082200161031     c                   kfld                    TBEcod                         -tabella
082300161031     c                   kfld                    TBEke1                         -chiave uno
082400161103     c                   kfld                    chiave2          15            -chiave due
082500161031     c                   kfld                    TBElin                         -lingua
082600161031     c                   kfld                    TBEsif                         -s.informativo
082700940207     C*
082800940223     C* Variabili per gestione videate
082900940223     C                   MOVE      *BLANK        $GEST             2
083000940223     C                   MOVE      *BLANK        $FINE             1
083100940223     C                   MOVE      *BLANK        $INZD1            1
083200940223     C                   MOVE      *BLANK        $LASTG            2
083300940223     C                   MOVE      *BLANK        $LASTV            2
083400940223     C* Variabili appoggio
083500940223     C                   Z-ADD     0             CURR              2 0
083600940223     C                   Z-ADD     0             CURC              2 0
083700940223     C                   MOVE      *ZEROS        WIN              99
083800940224     C                   MOVE      *BLANK        WTBERR            1
083900940223     C* Indici
084000940223     C                   Z-ADD     0             X                 3 0
084100940223     C                   Z-ADD     0             Y                 3 0
084200940127     C*
084300940117     C                   ENDSR
084400940128     C************************************************************
