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
032500170313     c                   clear                   d1qtas
032600170313     c                   clear                   d1qtav
032700030204     C*
032800161031     c                   eval        d1imp  = D§AIGIMP
032900170313     c                   eval        d1qtas = D§AIGQTAs
033000170313     c                   eval        d1qtav = D§AIGQTAv
033100120412     C*
033200940207     C                   ENDSR
033300940309     C************************************************************
033400940309     C* GESTIONE F03 VIDEO D1
033500940309     C************************************************************
033600940309     C     F03D1         BEGSR
033700940309     C*
033800940309     C                   MOVE      *ON           $FINE
033900030113    >C                   MOVE      '1'           xtaret
034000940325     C*
034100940325     C                   ENDSR
034200940309     C************************************************************
034300940309     C* GESTIONE F12 VIDEO D1
034400940309     C************************************************************
034500940309     C     F12D1         BEGSR
034600940309     C*
034700940309     C                   MOVE      *ON           $FINE
034800940309     C*
034900940309     C                   ENDSR
035000940131     C************************************************************
035100940207     C* CONTROLLO VIDEATA
035200940131     C************************************************************
035300940127     C     CTRD1         BEGSR
035400940131     C*
035500940127     C                   SETOFF                                       99
035600030123      *
035700940127     C* non imposto pos. del cursore
035800940127     C                   Z-ADD     0             H1RIGA
035900940127     C                   Z-ADD     0             H1COLO
036000940621     C*
036100940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0362000301141   >C     xtaopz        IFNE      '04'
036300971110     C*
036400161031     C* controllo codice Filiale
036500161109     c                   clear                   d1fild
036600161031$017 C                   IF        d1FIL = 0
036700161031     C                   SETON                                        6199
036800161109     c                   goto      errd1
036900161031     C                   elseIF    d1fil = 999
037000161031     c                   movel     'Azienda'     d1fild
037100161031     C                   Else
037200161031     c     d1fil         chain     azorg01l
037300161031     c                   if        %found(azorg01l)
037400161031     c                   movel     orgdes        d1fild
037500161031     c                   else
037600161031     c                   seton                                        6199
037700161109     c                   goto      errd1
037800161109     c                   endif
037900971110     C                   ENDIF
038000161031      **
038100161031      **  Tipo aspettativa
038200161031     C                   CLEAR                   tibs02ds
038300161031     C                   MOVEL     KNSIF         T02SIF
038400161031     C                   MOVEL     'TAI'         T02COD
038500161031      **
038600161031     c                   if        d1asp  = '?'
038700161031     C                   MOVEL     'R'           T02MOD
038800161031     c                   seton                                        99
038900161031     c                   else
039000161031     C                   MOVEL     'C'           T02MOD
039100161031     C                   movel(p)  d1asp         T02KE1
039200161031     c                   endif
039300161031      *
039400161109     c                   clear                   d1aspD
039500161031     c                   if        d1asp  = *blank
039600161031     c                   seton                                        6299
039700161109     c                   goto      errd1
039800161031     c                   else
039900161031     C                   CALL      'TIBS02R'
040000161031     C                   PARM                    KPJBA
040100161031     C                   PARM                    tibs02ds
040200161031     C     T02ERR        IFEQ      *BLANKS
040300161031     C                   MOVEL     T02UNI        dtai
040400161031     c                   movel     d§taides      d1aspD
040500161031     c                   movel     t02ke1        d1asp
040600161031     C                   ELSE
040700161031     C                   CLEAR                   d1asp
040800161031     c                   seton                                        6299
040900161109     c                   goto      errd1
041000161031     C                   ENDIF
041100161031     c                   end
041200161031      *
041300161031      **  Territorio
041400161031     C                   CLEAR                   tibs02ds
041500161031     C                   MOVEL     KNSIF         T02SIF
041600161031     C                   MOVEL     'ATS'         T02COD
041700161031      **
041800161031     c                   if        d1ter  = *blank
041900161031     c                   seton                                        6399
042000161109     c                   goto      errd1
042100161031     c                   else
042200161031      **
042300161109     c                   clear                   d1terd
042400161031     c                   if        d1ter  = '?'
042500161031     C                   MOVEL     'R'           T02MOD
042600161031     c                   seton                                        99
042700161031     c                   else
042800161031     C                   MOVEL     'C'           T02MOD
042900161031     C                   movel(p)  d1ter         T02KE1
043000161031     c                   endif
043100161031      *
043200161031     C                   CALL      'TIBS02R'
043300161031     C                   PARM                    KPJBA
043400161031     C                   PARM                    tibs02ds
043500161031     C     T02ERR        IFEQ      *BLANKS
043600161031     C                   MOVEL     T02UNI        dats
043700161031     c                   movel     d§atsdes      d1terd
043800161031     c                   movel     t02ke1        d1ter
043900161031     C                   ELSE
044000161031     C                   CLEAR                   d1ter
044100161031     c                   seton                                        6399
044200161109     c                   goto      errd1
044300161031     C                   ENDIF
044400161031     c                   end
044500161031      *
044600161031      **  Massa
044700161031     C                   CLEAR                   tibs02ds
044800161031     C                   MOVEL     KNSIF         T02SIF
044900161031     C                   MOVEL     'MAS'         T02COD
045000161109     C                   CLEAR                   d1masD
045100161109      **
045200161031     c                   if        d1mas  = *blank
045300161031     c                   seton                                        6499
045400161109     c                   goto      errd1
045500161031     c                   else
045600161031      **
045700161031     c                   if        d1mas  = '?'
045800161031     C                   MOVEL     'R'           T02MOD
045900161031     c                   seton                                        99
046000161031     C                   CALL      'TIBS02R'
046100161031     C                   PARM                    KPJBA
046200161031     C                   PARM                    tibs02ds
046300161031     C     T02ERR        IFEQ      *BLANKS
046400161031     C                   MOVEL     T02UNI        Dmas
046500161031     c                   movel     d§masdes      d1masd
046600161031     c                   movel     t02ke2        d1mas
046700161031     C                   ELSE
046800161109     c                   clear                   d1mas
046900161031     c                   seton                                        6499
047000161109     c                   goto      errd1
047100161031     C                   ENDIF
047200161031     c                   else
047300161031     c                   seton                                        9964
047400161031     c     'MAS'         setll     tntbe02l
047500161031     c     'MAS'         reade     tntbe02l
047600161031     c                   dow       not %EoF(tntbe02l)
047700161031     c                   if        d1mas = %subst(T2_tbeKE2:1:1)
047800161031     C                   MOVEL     T2_tbeUNI     Dmas
047900161031     c                   movel     d§masdes      d1masD
048000161031     c                   setoff                                       9964
048100161031     c                   leave
048200161031     c                   end
048300161031     c     'MAS'         reade     tntbe02l
048400161031     C                   ENDdo
048500161109     c   99              goto      errd1
048600161031     c                   endif
048700161031     c                   end
048800161031      *
048900161031      **  Tipo Attività
049000161031     C                   CLEAR                   tibs02ds
049100161031     C                   MOVEL     KNSIF         T02SIF
049200161031     C                   MOVEL     'AAT'         T02COD
049300161109     c                   clear                   d1attd
049400161031     c                   if        d1att  = *blank
049500161031     c                   seton                                        9965
049600161109     c                   goto      errd1
049700161031     c                   else
049800161031      **
049900161031     c                   if        d1att  = '?'
050000161031     C                   MOVEL     'R'           T02MOD
050100161031     c                   seton                                        99
050200161031     c                   else
050300161031     C                   MOVEL     'C'           T02MOD
050400161031     C                   movel(p)  d1att         T02KE1
050500161031     c                   endif
050600161031      *
050700161031     C                   CALL      'TIBS02R'
050800161031     C                   PARM                    KPJBA
050900161031     C                   PARM                    tibs02ds
051000161031     C     T02ERR        IFEQ      *BLANKS
051100161031     C                   MOVEL     T02UNI        Daat
051200161031     c                   movel     d§aatdes      d1attd
051300161031     c                   movel     t02ke1        d1att
051400161031     C                   ELSE
051500161031     C                   CLEAR                   D1att
051600161031     c                   seton                                        9965
051700161109     c                   goto      errd1
051800161031     C                   ENDIF
051900161031     c                   end
052000161031      *
052100161031      **  Tipo Allestimento
052200161031     c                   goto      no_allesti
052300161031     C                   CLEAR                   tibs02ds
052400161031     C                   MOVEL     KNSIF         T02SIF
052500161031     C                   MOVEL     'ALL'         T02COD
052600161109     c                   clear                   h1alld
052700161031     c                   if        h1all  = *blank
052800161031     c                   seton                                        9966
052900161109     c                   goto      errd1
053000161031     c                   else
053100161031      **
053200161031     c                   if        h1all  = '?'
053300161031     C                   MOVEL     'R'           T02MOD
053400161031     c                   seton                                        99
053500161031     c                   else
053600161031     C                   MOVEL     'C'           T02MOD
053700161031     C                   movel(p)  h1all         T02KE1
053800161031     c                   endif
053900161031      *
054000161031     C                   CALL      'TIBS02R'
054100161031     C                   PARM                    KPJBA
054200161031     C                   PARM                    tibs02ds
054300161031     C     T02ERR        IFEQ      *BLANKS
054400161031     C                   MOVEL     T02UNI        Dall
054500161031     c                   movel     d§alldes      h1alld
054600161031     c                   movel     t02ke1        h1all
054700161031     C                   ELSE
054800161031     C                   CLEAR                   h1all
054900161031     c                   seton                                        9966
055000161109     c                   goto      errd1
055100161031     C                   ENDIF
055200161031     c                   end
055300161031     c     no_allesti    tag
055400161031      **
055500161031     C* controllo data zero
055600161031$017 C     D1scad        IFEQ      0
055700161031     C                   SETON                                        6699
055800161109     c                   goto      errd1
055900160413     C                   Else
056000161031     C*
056100161031     C                   MOVE      D1scad        G02DAT
056200161031     C                   MOVEL     *BLANK        G02ERR
056300161031     C                   CALL      'XSRDA8'
056400161031     C                   PARM                    WLBDA8
056500161031     C*
056600161031     C     G02ERR        IFEQ      '1'
056700161031     C                   SETON                                        6699
056800161109     c                   goto      errd1
056900161031     C                   ElsE
057000161031     c                   move      g02dat        D1scad
057100161103     c                   z-add     g02INV        h1scad
057200161031     C                   END
057300161031     C*
057400161103     c* non deve essere retroattiva e deve essere fine mese
057500161104     C*     NON PIU' il FINE MESE  ma qualsiasi data
057600161103     c                   if        not *in66
057700161103     c                   move      D1scad        data_eur
057800161103     c                   move      data_eur      data_iso
057900161103     C*
058000161103    4c                   if        data_iso < wdate
058100161103     c                   seton                                        6799
058200161104    4c******             else
058300161104     c******             extrct    data_iso:*m   mesep             2 0
058400161104     c******             adddur    1:*d          data_iso
058500161104     c******             extrct    data_iso:*m   mesed             2 0
058600161104    5c******             if        mesep = mesed
058700161104     c******             seton                                        6799
058800161104    5C******             end
058900161103    4C                   end
059000161103    4C                   end
059100161103     C*
059200160413     C                   ENDIF
059300161031     C*
059400161031      *--------------------------------
059500170313     C* controllo - almeno una quantità deve essere > 0
059600170313$017 C     D1qtas        IFEQ      0
059700170313$017 C     D1qtav        andeq     0
059800170313     C                   SETON                                        687099
059900161109     c                   goto      errd1
060000161031     C                   ENDIF
060100170313     C*
060200161031     C* controllo
060300161031$017 C     D1imp         IFEQ      0
060400170313     C***                SETON                                        6999
060500170313     c***                goto      errd1
060600161031     C                   ENDIF
060700161031      *--------------------------------
060800160413     C*
060900940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
061000030114    >C     xtaopz        IFEQ      '01'
061100030114    >C     xtaopz        oreq      '03'
061200110927     c                   clear                   tbeKE1
061300160413     c                   clear                   tbeKE2
061400161031     c                   movel     dschiave      tbeKE1
061500161031     c                   movel     h1scad        tbeKE2
061600161103     C     KTBE          chain     tntbe01L
061700161103     c                   if        %Found(tntbe01L)
061800110908     C                   SETON                                        5399
061900161109     c                   goto      errd1
062000030114     C                   EndIF
062100030114     C                   ENDIF
062200030114      *
0623009402241-   C                   END
062400940131     C*
0625001611091-   C     errD1         tag
062600161109     C*
062700940302     C     *IN99         IFEQ      *ON
062800940510     C     $GEST         ANDEQ     'D1'
062900940117     C                   MOVEA     *IN           WIN
063000940128     C                   MOVE      *ALL'0'       IN5098           49
063100940128     C                   MOVEA     IN5098        *IN(50)
063200940614     C                   WRITE     D1
063300940117     C                   MOVEA     WIN           *IN
063400940302     C                   ENDIF
063500940127     C*
063600940117     C                   ENDSR
063700030204     C/EJECT
063800030204     C************************************************************
063900030204    >C* AGGIORNAMENTO ANAGRAFICA
064000030204     C************************************************************
064100030204    >C     FLDAGG        BEGSR
064200030204     C*
064300030204     C*  campi chiave da scrivere e aggiornare
064400110927     C                   clear                   tbeKE1
064500160412     C                   clear                   tbeKE2
064600161031     C                   MOVEl     DsChiave      tbeKE1
064700161031     C                   MOVEl     h1scad        tbeKE2
064800030204     C*
064900030204     C                   ENDSR
065000940207     C/EJECT
065100940224     C************************************************************
065200940315    >C* AGGIORNAMENTO ANAGRAFICA
065300940224     C************************************************************
065400940315    >C     AGGANA        BEGSR
065500940207     C*
065600030204     C                   exsr      FLDAGG
065700030204     C*
065800940207     C                   SELECT
065900030204     C*
066000030114    >C     xtaopz        WHENEQ    '01'
066100030114    >C     xtaopz        OREQ      '03'
066200030204     C*
066300161031    >C                   EXSR      RIEANA_IAG
066400110927     c                   write     tntbe000                             59
066500161031      *
066600161103     c  n59              movel     TNTBEds       xTNTBEds
066700161103     c  n59              write     tntbex                               59
066800161031      *
066900940318     C* se rcd non scrivibile attivo errore
067000030114    >C   59              SETON                                          99
067100030204      *
067200030114    >C     xtaopz        WHENEQ    '02'
067300030204      *
067400161103     C     tbe_rec       chain     tntbe00F
067500030204     C                   exsr      FLDAGG
067600161031    >C                   EXSR      RIEANA_IAG
067700110927     c                   update    tntbe000                             59
067800161031      *
067900161103     c                   if        not *in59
068000161103     c                   movel(p)  H1OLDSCAD     chiave2
068100161031     c     K05TBE01      chain     tntbex1l
068200161031     c                   if        %found(tntbex1l)
068300161031     c                   movel     TNTBEds       xTNTBEds
068400161103     c                   update    tntbex                               59
068500161103     c                   else
068600161103     c                   movel     TNTBEds       xTNTBEds
068700161103     c                   write     tntbex                               59
068800161103     c                   end
068900161103     c                   end
069000161031      *
069100030114    >C   59              SETON                                          99
069200030204      *
069300030114    >C     xtaopz        WHENEQ    '04'
069400161103     C     tbe_rec       chain     tntbe00F
069500030115     c                   if        $tasto = F23
069600110927     c                   delete    tntbe000                             59
069700161031      *
069800161103     c                   movel(p)  H1OLDSCAD     chiave2
069900161031     c     K05TBE01      chain     tntbex1l
070000161031     c                   if        %found(tntbex1l)
070100161031     c                   delete    tntbex
070200161031     c                   end
070300161031      *
070400030115     c                   else
070500110927     c                   delete    tntbe000                             59
070600161031      *
070700161103     c                   movel(p)  H1OLDSCAD     chiave2
070800161031     c     K05TBE01      chain     tntbex1l
070900161031     c                   if        %found(tntbex1l)
071000161031     c                   delete    tntbex
071100161031     c                   end
071200161031      *
071300030115     c                   end
071400030114    >C   59              SETON                                          99
071500940621     C*
071600940207     C                   ENDSL
071700940203     C*
071800940203     C                   ENDSR
071900940224     C************************************************************
072000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
072100940224     C************************************************************
072200161031     C     RIEANA_IAG    BEGSR
072300030114     C*
072400110927     C                   CLEAR                   tbeuni
072500160412     C*
072600161031     c                   z-add     d1imp         D§AIGIMP
072700170313     c                   z-add     d1qtas        D§AIGQTAs
072800170313     c                   z-add     d1qtav        D§AIGQTAv
072900110927     C                   MOVEL     EXTDS         tbeuni
073000130301     C*
073100161104     c                   eval      TBEAPL='BI'
073200130301     c                   eval      tbeFTT='S'
073300161031     c                   eval      tbeFLT=0
073400030114     C*
073500940207     C                   ENDSR
073600940224     C************************************************************
073700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
073800940224     C************************************************************
073900940224     C     GESAGG        BEGSR
074000940224     C*
074100940224     C* segnalo al pgm chiamante l'avvenuta conferma
074200030114    >C                   MOVE      *ON           xtaopr
074300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
074400030114    >C                   MOVE      '0'           xtaret
074500940224     C*
0746009402241    C                   SELECT
074700940224     C* nel caso di immissione, ripristino la videata iniziale
074800030114    >C     xtaopz        WHENEQ    '01'
074900940224     C                   MOVE      *ON           $INZD1
075000030114     C                   MOVE      *ON           $FINE
075100940224     C* altrimenti ritorno al pgm chiamante
075200940224     C                   OTHER
075300940224     C                   MOVE      *ON           $FINE
0754009402241-   C                   ENDSL
075500940224     C*
075600940224     C                   ENDSR
075700940131     C************************************************************
075800940131     C* OPERAZIONI INIZIALI
075900940131     C************************************************************
076000940131     C     *INZSR        BEGSR
076100940131     C*
076200940127     C* Reperimento parametri
076300940127     C*
076400940117     C     *ENTRY        PLIST
076500940117     C                   PARM                    KPJBA
076600030113     C                   movel     kpjbu         tabds
076700161103     C                   move      XTAtbeREC     tbe_rec           9 0
076800030115     c                   setoff                                       08
076900030115     C*
077000940223     C* Variabili per gestione videate
077100940223     C                   MOVE      'D1'          $GEST
077200940223     C                   MOVE      *OFF          $FINE
077300940223     C                   MOVE      *ON           $INZD1
077400940224     C                   MOVE      *BLANK        $LASTG
077500940224     C                   MOVE      *BLANK        $LASTV
077600940506     C                   Z-ADD     0             $ULKD1            3 0
077700940127     C*
077800940223     C* Variabili appoggio
077900940224     C                   Z-ADD     0             CURR
078000940224     C                   Z-ADD     0             CURC
078100161031     c                   movel     *date         Wdate
078200161031      *
078300161031      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
078400161031     c     *dtaara       define    §azute        AZUTEds
078500161031     c     *dtaara       define    §datiute      dDatiUte
078600161031      *
078700161031     c                   clear                   AZUTEds
078800161031     c                   clear                   dDatiUte
078900161031     c                   clear                   TIBS34ds
079000161031     c                   in(E)     *dtaara
079100161031if  1c                   if        %Error  or  RSUT = *blanks
079200161031     c                   call      'TIBS34R'
079300161031     c                   parm                    TIBS34ds
079400161031     c                   in        *dtaara
079500161031e   1c                   endif
079600940207     C*
079700940207     C* Valorizzazione campi univoci testate
079800940614     C                   CLEAR                   T1
079900940614     C                   MOVEL     KNSIF         NOMSIF
080000161031     C                   MOVEL     rsut          NOMDIT
080100941027     C                   MOVEL     DSPGM         NOMPGM
080200030113     C*
080300161031     C                   move      'AIG'         tbeCOD
080400110927     C                   clear                   tbeKE1
080500161031     C                   moveL     xtake1        tbeKE1
080600160413     C                   clear                   tbeKE2
080700160413     C                   MOVEl     xtake2        tbeKE2
080800940224     C*
080900940224     C* Inizializzazione/Decodifica prima videata
081000940224     C                   EXSR      INZD1
081100940224     C                   MOVE      *OFF          $INZD1
081200940127     C*
081300940117     C                   ENDSR
081400940128     C************************************************************
081500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
081600940223     C************************************************************
081700940223     C     DEFCAM        BEGSR
081800940223     C*
081900940223     C* klist
082000161103     C     KTBE          KLIST
082100110927     C                   KFLD                    tbeCOD
082200110927     C                   KFLD                    tbeKE1
082300160412     C                   KFLD                    tbeKE2
082400161031     C*
082500161031      * Definizioni chiavi di accesso
082600161031     c     K05TBE01      klist                                                  *tntbe01l
082700161031     c                   kfld                    TBEcod                         -tabella
082800161031     c                   kfld                    TBEke1                         -chiave uno
082900161103     c                   kfld                    chiave2          15            -chiave due
083000161031     c                   kfld                    TBElin                         -lingua
083100161031     c                   kfld                    TBEsif                         -s.informativo
083200940207     C*
083300940223     C* Variabili per gestione videate
083400940223     C                   MOVE      *BLANK        $GEST             2
083500940223     C                   MOVE      *BLANK        $FINE             1
083600940223     C                   MOVE      *BLANK        $INZD1            1
083700940223     C                   MOVE      *BLANK        $LASTG            2
083800940223     C                   MOVE      *BLANK        $LASTV            2
083900940223     C* Variabili appoggio
084000940223     C                   Z-ADD     0             CURR              2 0
084100940223     C                   Z-ADD     0             CURC              2 0
084200940223     C                   MOVE      *ZEROS        WIN              99
084300940224     C                   MOVE      *BLANK        WTBERR            1
084400940223     C* Indici
084500940223     C                   Z-ADD     0             X                 3 0
084600940223     C                   Z-ADD     0             Y                 3 0
084700940127     C*
084800940117     C                   ENDSR
084900940128     C************************************************************
