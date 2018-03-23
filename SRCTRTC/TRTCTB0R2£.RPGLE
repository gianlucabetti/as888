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
002500100803$001 FTRTCTB0D2 CF   E             WORKSTN
002600100803     F                                     SFILE(S1:S1NRR)
002700940201     F                                     INFDS(DSFMT)
002800100806     Fedmap01l  uF a E           K DISK
002900100806     Fedmap02l  iF   E           K DISK    rename(EDMAP000:EDMAP2)
003000940128     D*----------------------------------------------------*
003100940127     D* Passaggio Parametri
003200940127     D KPJBA         E DS
003300030113      *-------------
003400030113     D* Parametri in ricezione
003500030113     D  TABDS          DS
003600030113     D  XTAOPZ                 1      2
003700030113     D  XTARET                 3      3
003800030113     D  XTAOPR                 4      4
003900030113     D  XTAERR                 5      5
004000100805     D  XTAKEY                 6     20
004100100805$003 D Kcodice                             Like(MAPcodice)
004200100805$003 D Kunbcli                             Like(MAPUNBCLI)
004300100805$003 D Ktmsg                               Like(MAPTIPOMSG)
004400100805$003 D Kvers                               Like(MAPVERSION)
004500100805$003 D Krels                               Like(MAPRELEASE)
004600100805$003 D Kagenz                              Like(MAPAGENCY)
004700100805$003 D Kassoc                              Like(MAPASSOCIA)
004800100805$003 D KObblig                             Like(MAPOBBLIG)
004900100805$003 D KDescr                              Like(MAPDESCR)
005000100805$003 D KDesci                              Like(MAPDESCI)
005100100805$003 D OLDCodice                           Like(MAPcodice)
005200100805$003 D OLDunbcli                           Like(MAPUNBCLI)
005300100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
005400100805$003 D OLDvers                             Like(MAPVERSION)
005500100805$003 D OLDrels                             Like(MAPRELEASE)
005600100805$003 D OLDagenz                            Like(MAPAGENCY)
005700100805$003 D OLDassoc                            Like(MAPASSOCIA)
005800100809     d  k_unbcli                           Like(MAPUNBCLI)
005900100809     d  k_tmsg                             Like(MAPTIPOMSG)
006000100809     d  k_vers                             Like(MAPVERSION)
006100100809     d  k_rels                             Like(MAPRELEASE)
006200100809     d  k_agenz                            Like(MAPAGENCY)
006300100809     d  k_assoc                            Like(MAPASSOCIA)
006400940201     D*-------------
006500030204     D  S_TASTO        S                   like($Tasto)
006600940201     D DSFMT           DS
006700940506     D  $TASTO               369    369
006800940201     D  NRG                  370    370
006900940201     D  NCL                  371    371
007000940201     D*-------------
007100940201     D* posizione cursore
007200940201     D CURSOR          DS
007300940223     D  RIRI                   1      2B 0 INZ
007400940201     D  R$$                    2      2
007500940223     D  COCO                   3      4B 0 INZ
007600940201     D  C$$                    4      4
007700940207     D*-------------
007800940207     D* Reperimento nome PGM
007900940207     D STATUS         SDS           333
008000940207     D  DSPGM            *PROC
008100100806     D*-------------
008200100806     D rigaSFL         DS
008300100806     D  S1GRUPPO
008400100806     D  S1CAMPO
008500100806     D  S1OBBL
008600100806     D  S1TIPO
008700100806     D  S1DESCR
008800100806     D  S1DESCRI
008900100804     D*-------------
009000100805     D RigPag_SFl1     S              3  0 INZ(16)
009100100804$003 D S1NRR           S                   Like(C1rcd)
009200100804$003 D Wpagine         S                   Like(C1rcd)
009300100804     D S1pag           S                   Like(C1rcd)
009400100804     D WSfl            S                   Like(C1nrr)
009500100804     D Wmax            S                   Like(C1rcd)
009600100804     D Wpag            S                   Like(C1rcd)
009700100805     D savGRUPPO       S                   Like(s1GRUPPO)
009800940225     D*-------------
009900940225     D* COSTANTI
010000940225     D*-------------
010100940506     D* Tasti di funzione
010200940506     D F01             C                   CONST(X'31')
010300940506     D F02             C                   CONST(X'32')
010400940506     D F03             C                   CONST(X'33')
010500940506     D F04             C                   CONST(X'34')
010600940506     D F05             C                   CONST(X'35')
010700940506     D F06             C                   CONST(X'36')
010800940506     D F07             C                   CONST(X'37')
010900940506     D F08             C                   CONST(X'38')
011000940506     D F09             C                   CONST(X'39')
011100940506     D F10             C                   CONST(X'3A')
011200940506     D F11             C                   CONST(X'3B')
011300940506     D F12             C                   CONST(X'3C')
011400940506     D F13             C                   CONST(X'B1')
011500940506     D F14             C                   CONST(X'B2')
011600940506     D F15             C                   CONST(X'B3')
011700940506     D F16             C                   CONST(X'B4')
011800940506     D F17             C                   CONST(X'B5')
011900940506     D F18             C                   CONST(X'B6')
012000940506     D F19             C                   CONST(X'B7')
012100940506     D F20             C                   CONST(X'B8')
012200940506     D F21             C                   CONST(X'B9')
012300940506     D F22             C                   CONST(X'BA')
012400940506     D F23             C                   CONST(X'BB')
012500940506     D F24             C                   CONST(X'BC')
012600940506     D ENTER           C                   CONST(X'F1')
012700940506     D ROLDWN          C                   CONST(X'F4')
012800940506     D ROLLUP          C                   CONST(X'F5')
012900940506     D*-------------
013000941108     D DATA            C                   CONST('0001-01-01')
013100030114     D Annull          C                   CONST('  ANNULLATO  ')
013200030114     D OPz_Imm         C                   CONST('IMMISSIONE')
013300030114     D OPz_Var         C                   CONST('VARIAZIONE')
013400030114     D OPz_Del         C                   CONST(' ANNULLA  ')
013500030114     D OPz_Vis         C                   CONST('VISUALIZZA')
013600940127     C*----------------------------------------------------*
013700940127     C*                MAIN LINE PROGRAM
013800940127     C*----------------------------------------------------*
013900940223     C*
014000940223     C     $FINE         DOWEQ     *OFF
014100100803     C     $GEST         CASEQ     'S1'          GESS1
014200940117     C                   END
014300940117     C                   END
014400940325     C* fine programma
014500030113     c                   movel     tabds         kpjbu
014600030113     C                   Seton                                        LR
014700940131     C************************************************************
014800100804     C* GESTIONE VIDEO RECORD S1
014900940131     C************************************************************
015000100804     C     GESS1         BEGSR
015100030113      *
015200940225     C* inizializzazione videata
015300100804     C     $INZS1        IFEQ      *ON
015400100804     C                   EXSR      INZS1
015500100810     C                   z-add     1             riga_posiz
015600100804     C                   MOVE      *OFF          $INZS1
015700940117     C                   END
015800030113      *
015900100804     C                   WRITE     T1
016000100804     C                   WRITE     Z1
016100030114     C*
016200100810     c                   if         riga_posiz > 0
016300100810     C                   z-add     riga_posiz    C1RCD
016400100810     c                   else
016500100810     C*
016600100810     C                   z-add     c1nrr         C1RCD
016700100810     c                   end
016800030114     c                   SETOFF                                         99
016900940620     C* Immissione/Modifica/Copia
017000030113     C*               *----------------*
017100100804     C                   EXFMT     c1
017200030113     C*               *----------------*
017300100810     c                   clear                   riga_posiz        5 0
017400030114     C*
017500030114     C     *IN99         IFEQ      *OFF
017600030114     C*
0177000301141    C                   SELECT
017800030114     C* F3=Fine
017900030114     C     $TASTO        WHENEQ    F03
018000100804     C                   EXSR      F03S1
018100030114     C* F12=Ritorno
018200030114     C     $TASTO        WHENEQ    F12
018300100804     C                   EXSR      F12S1
018400030114     C*
0185000301141O   C                   OTHER
018600030114     C*
018700100804     C                   EXSR      CTRC1
018800100804     C  n99              EXSR      CTRS1
018900030113     C*
019000940616     C     *IN99         IFEQ      *OFF
019100100804    >C     xtaopz        andNE     '05'
019200940616     C* F6
0193000302052    C     $TASTO        ifeq      F06
0194000301152    C     $TASTO        oreq      F23
019500100804     C*
019600100805     c                   exfmt     ConfWind
019700100805     C     $TASTO        Ifeq      F06
019800100805     C*
019900100804     C* eseguo sempre la cancellazione
020000100805    >C     xtaopz        ifEQ      '02'
020100100805    >C     xtaopz        oreq      '04'
020200100804    >C                   EXSR      DELTAB
020300100805     c                   end
020400100804     C* eseguo scrittura
020500100804    >C     xtaopz        ifnE      '04'
020600100804    >C                   EXSR      WRITAB
020700100804     c                   end
020800100804     C*
020900100804    >C                   EXSR      GESAGG
021000030205     c                   end
021100030114     C*
021200100805     c                   else
021300100805     C*
021400100805     c                   end
021500100805     C*
021600030114     C                   ENDIF
021700030114     C*
0218000301142-   C                   ENDSL
021900030114     C*
0220000301142-   C                   ENDIF
022100030114     C*
022200940117     C                   ENDSR
022300940117      ************************************************************
022400940207      * INIZIALIZZAZIONE VIDEATA DATI
022500940117      ************************************************************
022600100804     C     INZS1         BEGSR
022700100804     C*
022800100804     C* reset indicatori DSPATR
022900100804     c                   exsr      INZ_indERR
023000100804     C* pulizia SFL
023100100804     C                   SETOFF                                         3031
023200100804     C                   WRITE     C1
023300100804     C                   SETON                                          31
023400100804     C*
023500100804     C* Righe x Pagina del 1° SFL
023600100805     c                   z-add     RigPag_SFl1   s1pag
023700100805     C                   MOVEL     Kcodice       C1codice
023800100804     C                   MOVEL     Kunbcli       C1UNBCLI
023900100804     C                   MOVEL     Ktmsg         C1TMSG
024000100804     C                   MOVEL     Kvers         C1VERS
024100100804     C                   MOVEL     Krels         C1RELS
024200100804     C                   MOVEL     Kagenz        C1AGEN
024300100804     C                   MOVEL     Kassoc        C1ASSOC
024400100804     C                   MOVEL     KDescr        C1DESCR
024500100804     C                   MOVEL     KObblig       H1OBBL
024600100804     C                   MOVEL     KDesci        H1DESCI
024700100804      *
024800100804     C* CARICAMENTO SFL totale
024900100804     C                   Z-ADD     0             S1NRR
025000100804     C                   Z-ADD     1             C1RCD
025100100804     C                   Z-ADD     0             WMAX
025200100804     C                   Z-ADD     0             Wsfl
025300100804     C* Posizionamento su file
025400100804     C*
025500100804     C* valorizzo la chiave univoca per reperire i dati
025600100804     C* se non immissione
0257001008041   >C     xtaopz        IFNE      '01'
025800100804      *
025900100804    >C*------------------------------------*
026000100804    >C* impostare i campi chiave ricevuti
026100100804    >C*------------------------------------*
026200100804     C     K_map01       setll     edMAP01l
026300100804     C*
026400100804     C* riempio la videata
026500100804     C                   IF        %Found(edmap01l)
026600100804     C* riempio la videata
026700100804     C                   EXSR      RolS1
026800100804      *
026900100804     C* richiamo routine dei ctrl per decodificarla
027000100804    >C                   EXSR      CTRS1
027100100804      *
027200100805     C                   MOVE      *ALL'0'       IN5098           49
027300100804     C                   MOVEA     IN5098        *IN(50)
027400100804      *
027500100804     C* definizione indicatori su video
027600100806     C                   SETOFF                                       02  06
027700100805      *
0278001008043    C                   SELECT
027900100804    >C     xtaopz        WHENEQ    '02'
028000100805    >C     xtaopz        WHENEQ    '04'
028100100805     C                   SETON                                        02
028200100804    >C     xtaopz        WHENEQ    '05'
028300100805     C                   SETON                                        02  06
0284001008043-   C                   ENDSL
028500100804     C*
0286001008042-   C                   ENDIF
028700100804     C*
0288001008041   >C                   Else
028900100805      *
029000100805     C* riempio la videata di righe vuote da riempire
029100100805     C                   EXSR      RolS1
029200100805     C*
0293001008041-   C                   ENDIF
029400100804     C*
029500940117     C                   ENDSR
029600940207     C/EJECT
029700100804      *---------------------------------------------------------------*
029800100804      *? CARICAMENTO PAGINA LISTA                                    ?
029900100804      *---------------------------------------------------------------*
030000100804     C     ROLS1         BEGSR
030100100804     C*
030200100804     C                   SETOFF                                       32
030300100804     C                   Z-ADD     WMAX          S1NRR
030400100804     C*
030500100804     C* Caricamento del Sfl x Giro
030600100804     c                   Exsr      Write_S1
030700100804     C*
030800100804     C                   Z-ADD     S1NRR         WMAX                 30
030900100804     C*
031000100804     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
031100100804     C     S1NRR         DIV       S1pag         PAGINE            4 0
031200100804     C                   MVR                     RESTO             3 0
031300100804     C     PAGINE        MULT      S1pag         C1RCD
0314001008041    C     RESTO         IFGT      0
031500100804     C                   ADD       1             C1RCD
0316001008041E   C                   ELSE
031700100804     C                   SUB       S1pag         C1RCD
031800100804     C                   ADD       1             C1RCD
0319001008041-   C                   ENDIF
032000100804     C*
032100100804     C                   ENDSR
032200100804      *---------------------------------------------------------------*
032300100804      *?  Scrive SFL                                                 ?
032400100804      *---------------------------------------------------------------*
032500100805     C     Write_S1      BEGSR
032600100805      *
032700100805     c                   clear                   pos_cursor        1
032800100805      *
032900100805      *  Se NON inserimento
033000100805    >C                   if        xtaopz <> '01'
033100100805      *
033200100804     C*  Esegue una lettura di posizionamento GIRO fuori
033300100804      *   per poi poter totalizzare x Giro all'interno della Rout.RIGA_Giro
033400100804      *   richiamata anche in seguito.
033500100804     c                   exsr      Legge_rec
033600100804     C*
033700100804      * Finchè il giro è lo stesso
0338001008041    C     $EFILE        DowEQ     *OFF
033900100804     C*
034000100804     C* ---------
034100100804     C* si ricorda il gruppo precedente per calcolare in seguito la DS
034200100804     C*   da scrivere
034300100804     C* ---------
034400100804     C                   clear                   H1GRU_PRE
034500100804     C                   clear                   H1lun_PRE
034600100804     C*
034700100804     c                   if        s1gruppo <> mapcatego
034800100804     C*   a rottura di gruppo ossia di categoria
034900100804     C*
035000100804     C                   MOVEL     S1GRUPPO      H1GRU_PRE
035100100804     C                   z-add     lung_GRUPPO   H1lun_PRE
035200100804     c                   clear                   lung_GRUPPO       5 0
035300100804     C*
035400100804     C*   quando è uguale somma
035500100804     c                   else
035600100804     c                   if        mapCATEGO <> *blank
035700100804     c                   add       mapLUNGH      lung_GRUPPO
035800100804     c                   endif
035900100804     c                   end
036000100804     C* ---------
036100100804     C                   MOVEL     MAPCATEGO     S1GRUPPO
036200100804     C                   MOVEL     MAPNCAMPO     S1CAMPO
036300100804     C                   if          mapOBBLIG = 'M'
036400100804     C                   MOVEL     'S'           S1obbl
036500100804     c                   else
036600100804     C                   MOVEL     'N'           S1obbl
036700100804     c                   end
036800100804     C                   MOVEL     MAPTIPOD      S1TIPO
036900100804     C                   z-add     MAPLUNGH      S1LUNGO
037000100804     C                   z-add     MAPDECIM      S1DECIM
037100100804     C                   MOVEL     mapDESCR      S1descr
037200100804     C                   MOVEL     mapDESCI      S1descri
037300100805      *
037400100805     c                   setoff                                       11
037500100805     c                   if        pos_cursor <> 'S'
037600100805     c                   setoN                                        11
037700100805     c                   eval      pos_cursor  = 'S'
037800100805     c                   end
037900100804      *---------
038000100804      *  Scrive la riga di SFL a rottura di giro
038100100804     C                   ADD       1             S1NRR
038200100804     C                   WRITE     S1
038300100804      *---------
038400100804     c                   exsr      Legge_rec
038500100804     c                   EndDO
038600100804     C*
038700100805     c                   End
038800100805     C*
038900100805    >C                   if        xtaopz<>'04' and xtaopz<>'05'
039000100805     C*
039100100805     C     S1NRR         DIV       S1pag         PAGINE            4 0
039200100805     C                   MVR                     RESTO             3 0
039300100805     C*
039400100804     C*  deve scrivere tot pagine vuote da scrivere
039500100805      *  Se INSerimento
039600100805     c     RigPag_SFl1   mult      10            volte             5 0
039700100805     c                   sub       resto         volte
039800100805     C*
039900100805     c                   do        volte         contariga         5 0
040000100805     C*
040100100805      *---------
040200100805     c                   clear                   H1LUN_PRE
040300100805     c                   clear                   H1GRU_PRE
040400100805     c                   clear                   S1GRUPPO
040500100805     c                   clear                   S1CAMPO
040600100805     c                   clear                   S1OBBL
040700100805     c                   clear                   S1TIPO
040800100805     c                   clear                   S1LUNGO
040900100805     c                   clear                   S1DECIM
041000100805     c                   clear                   S1DESCR
041100100805     c                   clear                   S1DESCRI
041200100805     C                   SETOFF                                       0203
041300100805     c                   setoff                                       11
041400100805     c                   if        pos_cursor <> 'S'
041500100805     c                   setoN                                        11
041600100805     c                   eval      pos_cursor  = 'S'
041700100805     c                   end
041800100805      * indicatore (+) spento
041900100805     C                   SETOFF                                       33
042000100805     C                   IF        volte = contariga
042100100805     C                   SETON                                        33
042200100805     c                   end
042300100805      *  Scrive la riga di SFL a rottura di giro
042400100805     C                   ADD       1             S1NRR
042500100805     C                   WRITE     S1
042600100805      *---------
042700100805     c                   EndDo
042800100805     c                   End
042900100804     C*
043000100804     C                   ENDSR
043100100804      *---------------------------------------------------------------*
043200100804      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
043300100804      *---------------------------------------------------------------*
043400100804     C     LEGGE_REC     BEGSR
043500100804     C*
043600100804     C*  Per EoF deve uscire
043700100804     C                   MOVEL     *OFF          $EFILE
043800100804     C*  Per Record Ok deve uscire riportando i dati
043900100804     C                   MOVEL     *OFF          $RCDOK
044000100804     C*
0441001008041    C     $EFILE        DOUEQ     *ON
044200100804     C     $RCDOK        OREQ      *ON
044300100804     C*
044400100804     c     k_map01       reade     edMAP01L
044500100804      * Fine File
044600100804     C                   IF        %Eof(edMAP01l)
044700100804     C                   MOVEL     *ON           $EFILE
044800100804     C                   MOVE      $EFILE        *IN33
044900100804     C                   else
045000100804     c                   if        mapNCAMPO <> *blank
045100100804     C                   MOVE      *ON           $RCDOK
045200100804     C                   end
045300100804     C                   end
045400100804      *
0455001008041-   C                   ENDDO
045600100804     C*
045700100804     C                   ENDSR
045800100804     C/EJECT
045900100804      *---------------------------------------------------------------*
046000100804      *?   CONTROLLO TESTATA LISTA                                   ?
046100100804      *---------------------------------------------------------------*
046200100804     C     CTRC1         BEGSR
046300100804     C                   MOVE      *OFF          *IN99
046400100804     C                   ENDSR
046500100804     C/EJECT
046600100804      *---------------------------------------------------------------*
046700100804      *?   CONTROLLO OPZIONI LISTA                                   ?
046800100804      *---------------------------------------------------------------*
046900100804     C     CTRS1         BEGSR
047000100804     C*
047100100804     C                   MOVEL     *OFF          $ESCI
047200100804     C                   SETOFF                                       99
047300100804     c                   clear                   tot_lunghezza     5 0
047400100805     c                   clear                   resto
047500100805     c                   clear                   riga_vuota        1
047600100806     c                   clear                   solo_gruppo       1
047700100804     C*
047800100804     C* Leggo il sfl solo se ci sono rcd
0479001008041    C     WMAX          IFGT      0
048000100806     c                   z-add     wmax          wmaxrighe         5 0
048100100806     C*
048200100806     c                   do        wmaxrighe     s1nRR
048300100806     C     s1nrr         chain     S1                                 21
048400100804     C*
048500100804     C* esce se fine sfl o errore che richiede l'uscita
0486001008062    C     *IN21         ifEQ      *OFF
048700100804     C     $ESCI         ANDEQ     *OFF
048800100804     C                   Z-ADD     S1NRR         C1RCD
048900100804     C* ctrl su riga
049000100805     C                   EXSR      RECS1
049100100804      *
049200100804      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
049300100806     c                   add       s1lungo       tot_lunghezza
049400100804     C                   MOVE      *ON           *IN32
049500100804     C                   UPDATE    S1
049600100806     c                   if        *in99
049700100806     c                   leave
049800100806     c                   endIf
049900100806     C*
0500001008043-   C                   ENDIF
050100100804     C*
0502001008042-   C                   ENDDO
050300100804     C*
0504001008041-   C                   ENDIF
050500100806     C*
050600100806     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
050700100806     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0508001008063    C     $ESCI         IFEQ      *OFF
0509001008064    C     *IN21         andEQ     *ON
051000100806     C* calcolo pagina a cui deve posizionarsi
051100100806     c                   z-add     s1pag         Wpagine
051200100806     C                   EXSR      CLCPAG
0513001008064-   C                   ENDIF
051400100804     C*
051500100804     C                   ENDSR
051600100804     C/EJECT
051700100804      *---------------------------------------------------------------*
051800100804      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
051900100804      *---------------------------------------------------------------*
052000100804     C     CLCPAG        BEGSR
052100100804     C* Input :
052200100804     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
052300100804     C*          cursore
052400100804     C* - Wpagine= numero rcd per pagina sfl
052500100804     C* Output :
052600100804     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
052700100804     C*
052800100804     C     WSFL          DIV       Wpagine       PAGINE            4 0
052900100804     C                   MVR                     RESTO             3 0
053000100804     C     RESTO         IFGT      0
053100100804     C                   ADD       1             PAGINE
053200100804     C                   ENDIF
053300100804     C     PAGINE        MULT      Wpagine       WPAG
053400100804     C*
053500100804     C                   ENDSR
053600100804     C/EJECT
053700100803     C************************************************************
053800100803     C* CONTROLLO VIDEATA
053900100803     C************************************************************
054000100804     C     RECS1         BEGSR
054100100803     C*
054200100803     C                   SETOFF                                       99
054300100804     C*
054400100804     C* reset indicatori DSPATR
054500100804     c                   exsr      INZ_indERR
054600100805      *
054700100806      * A riga vuota se lo memorizza x dopo
054800100806$017 C     RIGAsfl       IFEQ      *blank
054900100804$017 C     S1lungo       andEQ     *zero
055000100806$017 C     S1decim       andEQ     *zero
055100100806     c                   move      'S'           riga_vuota        1
055200100806     c                   clear                   solo_GRUPPO       1
055300100806      *
055400100804     C                   Else
055500100806      *
055600100805     C*  Non deve esistere una riga vuota fra righe valorizzate.
055700100805     c                   if          riga_vuota = 'S'
055800100805     C                   SETON                                        5999
055900100810     C                   goto      priority
056000100810     C                   END
056100100806     C*
056200100806     C*  Se voglio copiare un gruppo di righe già presenti imposto solo il Gruppo
056300100806     C*   e premo invio. e automaticamente copierà il gruppo già presente
056400100806     C*   in altri segmenti su quello che sto immettendo.
056500100810     c                   move      'N'           Gruppo_trovato    1
056600100806$017 C                   IF        S1gruppo <> *blank
056700100806$017 C     S1campo       IFEQ      *blank
056800100806$017 C     S1tipo        andEQ     *blank
056900100806$017 C     S1obbl        andEQ     *blank
057000100806$017 C     S1lungo       andEQ     *zero
057100100806$017 C     S1decim       andEQ     *zero
057200100806$017 C     S1descr       andEQ     *blank
057300100806$017 C     S1descri      andEQ     *blank
057400100806     C                   Exsr      Decod_GRUPPO
057500100806     c                   move      'S'           solo_GRUPPO       1
057600100806     C                   END
057700100806     C                   END
057800100806     C*
057900100810     c                   move      'N'           Campo_trovato     1
058000100810     C* Se non ha trovato il gruppo tenta semplicemente con il solo campo
058100100810$017 C                   IF        S1gruppo <> *blank and s1campo <> *blank
058200100810     c                             and Gruppo_trovato = 'N'
058300100810$017 C     S1tipo        IFEQ      *blank
058400100810$017 C     S1obbl        andEQ     *blank
058500100810$017 C     S1lungo       andEQ     *zero
058600100810$017 C     S1decim       andEQ     *zero
058700100810$017 C     S1descr       andEQ     *blank
058800100810$017 C     S1descri      andEQ     *blank
058900100810     C                   Exsr      Decod_campo
059000100810     C                   END
059100100810     C                   END
059200100810     C*
059300100806     C*  Se non c'è solo il gruppo
059400100806     c                   IF         solo_GRUPPO = *Blank
059500100804     C*
059600100804$017 C     S1campo       IFEQ      *blank
059700100804     C                   SETON                                        5199
059800100804     C                   END
059900100804     C*
060000100804$017 C     S1tipo        IFEQ      *blank
060100100804     C                   SETON                                        5299
060200100804     C                   END
060300100804     C*
060400100804$017 C     S1obbl        IFEQ      *blank
060500100804     C                   SETON                                        5499
060600100804     C                   END
060700100804     C*
060800100804$017 C     S1lungo       IFEQ      *zero
060900100804     C                   SETON                                        5399
061000100804     C                   END
061100100804     C*
061200100806$017 C     S1decim       IFgt      *zero
061300100806$017 C     S1tipo        andne     'N'
061400100806     C                   SETON                                        5699
061500100806     C                   END
061600100806     C*
061700100804$017 C     S1descr       IFEQ      *blank
061800100804     C                   SETON                                        5599
061900100804     C                   END
062000100803     C*
062100100806     C                   END
062200100806     C*
062300100804     C                   ENDif
062400100806     C*---------------------------------------------------------------------------
062500100810     c     priority      tag
0626001008063    C     *IN99         IFEQ      *ON
062700100806     C     $ESCI         OREQ      *ON
062800100806     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
062900100806     C* e la ripristinerò a conclusione del ciclo di READC
063000100806     C                   MOVE      *OFF          $INZS1
063100100810     C                   z-add     s1nrr         riga_posiz
0632001008063-   C                   ENDIF
063300100803     C*
063400100803     C                   ENDSR
063500100803     C/EJECT
063600940309     C************************************************************
063700100806     C* decodifica il gruppo di campi
063800940309     C************************************************************
063900100806     C     Decod_GRUPPO  BEGSR
064000940309     C*
064100100809     c                   move      'N'           Clear_key         1
064200100806     c                   clear                   gia_zero          1
064300100806     C                   movel     s1gruppo      gruppo            4
064400100809     c                   eval        k_unbcli  =  oldunbcli
064500100809     c                   eval        k_tmsg    =  oldtmsg
064600100809     c                   eval        k_vers    =  oldvers
064700100809     c                   eval        k_rels    =  oldrels
064800100809     c                   eval        k_agenz   =  oldagenz
064900100809     c                   eval        k_assoc   =  oldassoc
065000100810     c     di_nuovo      tag
065100100809     c*
065200100806     C     k_map02       setll     edmap02l
065300100806     C     k_map02       reade     edmap02l
065400100806     C*
065500100806     c                   dow       not %Eof(edmap02l)
065600100806     C*
065700100806     c                   if        mapnCAMPO <> *blank
065800100806     c                   eval        gia_zero = 'S'
065900100809     c                   eval        gruppo_trovato = 'S'
066000100806     C*
066100100806     c                   eval      s1gruppo = mapCATEGO
066200100806     c                   eval      s1campo  = mapNCAMPO
066300100806     c                   if        mapOBBLIG = 'M'
066400100806     c                   eval      s1obbl   = 'S'
066500100806     c                   else
066600100806     c                   eval      s1obbl   = 'N'
066700100806     c                   end
066800100806     c                   eval      S1TIPO   = mapTIPOD
066900100806     c                   eval      S1LUNGO  = mapLUNGH
067000100806     c                   eval      S1DECIM  = mapDECIM
067100100806     c                   eval      S1DESCR  = mapDESCR
067200100806     c                   eval      S1DESCRI = mapDESCI
067300100806     c                   update    s1
067400100806      *
067500100806      * aggancia la successiva rige di sfl
067600100806     c                   add       1             s1nrr
067700100806     c     s1nrr         chain     s1
067800100806      *
067900100806     c                   else
068000100806      *  se è uno zero successivo ha finito la copiatura dei records
068100100806     c                   if          gia_zero = 'S'
068200100806     c                   leave
068300100806     c                   end
068400100806     C*
068500100806     c                   end
068600100806     C*
068700100806     C     k_map02       reade     edmap02l
068800100806     c                   endDo
068900100806     C*
069000100809     c                   if        gruppo_trovato = 'N' and Clear_key='N'
069100100809     c                   eval        Clear_key =  'S'
069200100809     c                   eval        k_unbcli  =  *blank
069300100809     c                   eval        k_tmsg    =  *blank
069400100809     c                   eval        k_vers    =  *blank
069500100809     c                   eval        k_rels    =  *blank
069600100809     c                   eval        k_agenz   =  *blank
069700100809     c                   eval        k_assoc   =  *blank
069800100810     c                   goto      di_nuovo
069900100809     c                   end
070000100809     C*
070100100806     C                   ENDSR
070200100810     C/EJECT
070300100810     C************************************************************
070400100810     C* decodifica il semplice campo
070500100810     C************************************************************
070600100810     C     Decod_CAMPO   BEGSR
070700100810     C*
070800100810     c                   move      'N'           Clear_key         1
070900100810     c                   eval        k_unbcli  =  oldunbcli
071000100810     c                   eval        k_tmsg    =  oldtmsg
071100100810     c                   eval        k_vers    =  oldvers
071200100810     c                   eval        k_rels    =  oldrels
071300100810     c                   eval        k_agenz   =  oldagenz
071400100810     c                   eval        k_assoc   =  oldassoc
071500100810     c     x_campo       tag
071600100810     c*
071700100810     C     k_map02_1     setll     edmap02l
071800100810     C     k_map02_1     reade     edmap02l
071900100810     C*
072000100810     c                   dow       not %Eof(edmap02l)
072100100810     C*
072200100810     c                   if        mapnCAMPO <> *blank and mapnCAMPO = s1Campo
072300100810     c                   eval        campo_trovato = 'S'
072400100810     C*
072500100810     c                   if        mapOBBLIG = 'M'
072600100810     c                   eval      s1obbl   = 'S'
072700100810     c                   else
072800100810     c                   eval      s1obbl   = 'N'
072900100810     c                   end
073000100810     c                   eval      S1TIPO   = mapTIPOD
073100100810     c                   eval      S1LUNGO  = mapLUNGH
073200100810     c                   eval      S1DECIM  = mapDECIM
073300100810     c                   eval      S1DESCR  = mapDESCR
073400100810     c                   eval      S1DESCRI = mapDESCI
073500100810     c                   end
073600100810     C*
073700100810     C     k_map02_1     reade     edmap02l
073800100810     c                   endDo
073900100810     C*
074000100810     c                   if        campo_trovato = 'N' and Clear_key='N'
074100100810     c                   eval        Clear_key =  'S'
074200100810     c                   eval        k_unbcli  =  *blank
074300100810     c                   eval        k_tmsg    =  *blank
074400100810     c                   eval        k_vers    =  *blank
074500100810     c                   eval        k_rels    =  *blank
074600100810     c                   eval        k_agenz   =  *blank
074700100810     c                   eval        k_assoc   =  *blank
074800100810     c                   goto      x_campo
074900100810     c                   end
075000100810     C*
075100100810     C                   ENDSR
075200100806     C/EJECT
075300100806     C************************************************************
075400100806     C* GESTIONE F03 VIDEO S1
075500100806     C************************************************************
075600100806     C     F03S1         BEGSR
075700100806     C*
075800940309     C                   MOVE      *ON           $FINE
075900030113    >C                   MOVE      '1'           xtaret
076000940325     C*
076100940325     C                   ENDSR
076200940309     C************************************************************
076300100804     C* GESTIONE F12 VIDEO S1
076400940309     C************************************************************
076500100804     C     F12S1         BEGSR
076600940309     C*
076700940309     C                   MOVE      *ON           $FINE
076800100805    >C                   MOVE      '2'           xtaret
076900940309     C*
077000940309     C                   ENDSR
077100940224     C************************************************************
077200940315    >C* AGGIORNAMENTO ANAGRAFICA
077300940224     C************************************************************
077400100804    >C     DELTAB        BEGSR
077500940207     C*
077600100805     C     k_map01       setll     edmap01l
077700100805     C     k_map01       reade     edmap01l
077800100805     c                   dow       not %EoF(edmap01l)
077900100805     C                   delete    edmap000
078000100805     C     k_map01       reade     edmap01l
078100100805     C                   END
078200940203     C*
078300940203     C                   ENDSR
078400100804     C************************************************************
078500100804    >C* AGGIORNAMENTO ANAGRAFICA
078600100804     C************************************************************
078700100804    >C     WRITAB        BEGSR
078800100805     C*
078900100805     C                   clear                   savGRUPPO
079000100805     c                   z-add     0             sequenza          5 0
079100100805     c                   z-add     0             progress          5 0
079200100805     C                   SETOFF                                       99
079300100805     C*
079400100805     C* Scrittura record Segmento Totale
079500100805     c                   exsr      Rcd_Testata
079600100805     C*
079700100806     C* Leggo il sfl solo se ci sono rcd
0798001008061    C     WMAX          IFGT      0
079900100806     C*
080000100806     c                   do        wmax          s1nRR
080100100806     C     s1nrr         chain     S1                                 21
080200100806     C*
080300100806      *
080400100806     C* esce se fine sfl o errore che richiede l'uscita
0805001008062    C     *IN21         ifEQ      *OFF
080600100806     C     $ESCI         ANDEQ     *OFF
080700100805      *
080800100805      * A riga vuota esce dal ciclo di aggiornamento
080900100806$017 C     RIGAsfl       IFEQ      *blank
081000100806$017 C     S1lungo       andEQ     *zero
081100100806$017 C     S1decim       andEQ     *zero
081200100805     c                   LEAVE
081300100805     c                   end
081400100805      *
081500100805      * a rottura di gruppo di campi
081600100805     c                   if        S1GRUPPO <> SAVgruppo or S1GRUPPO = *blank
081700100806     c                   z-add     0             progress
081800100806      *
081900100806      *   scrive solo se il gruppo non è blank
082000100806     c                   if        SAVgruppo <> *blank
082100100805     c                   exsr      Rcd_Gruppo
082200100806     c                   end
082300100806      *
082400100806     c                   z-add     0             Lung_Gruppo
082500100805     c                   add       10            sequenza
082600100805     c                   eval        SAVgruppo = S1GRUPPO
082700100805     c                   end
082800100805      *
082900100805     c                   add       1             progress
083000100806     c                   add       s1LUNGO       Lung_Gruppo
083100100805     c                   exsr      Rcd_campo
083200100805      *
083300100805      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
083400100805     C                   MOVE      *ON           *IN32
083500100805     C                   UPDATE    S1
083600100806     c                   end
083700100805      *
0838001008052-   C                   ENDDO
083900100806      *
084000100806      *   scrive solo se il gruppo non è blank
084100100806     c                   if        SAVgruppo <> *blank
084200100806     c                   exsr      Rcd_Gruppo
084300100806     c                   end
084400100804     C*
084500100806     c                   end
084600100806      *
084700100805     C* segnalo al pgm chiamante l'aver premuto l'F6
084800100805    >C                   MOVE      '0'           xtaret
084900100804     C*
085000100804     C                   ENDSR
085100940224     C************************************************************
085200100805     C* Record di testata del segmento
085300940224     C************************************************************
085400100805     C     Rcd_Testata   BEGSR
085500940224     C*
085600100805     c                   clear                   edmap000
085700100805     c                   eval      MAPUNBCLI   = C1UNBCLI
085800100805     c                   eval      MAPTIPOMSG  = C1TMSG
085900100805     c                   eval      MAPVERSION  = C1VERS
086000100805     c                   eval      MAPRELEASE  = C1RELS
086100100805     c                   eval      MAPAGENCY   = C1AGEN
086200100805     c                   eval      MAPASSOCIA  = C1ASSOC
086300100805     c                   eval      MAPCODICE   = C1CODICE
086400100805     c                   eval      MAPOBBLIG   = H1OBBL
086500100805     c                   eval      MAPLUNGH    = tot_lunghezza
086600100805     c                   eval      MAPDESCR    = C1DESCR
086700100805     c                   eval      MAPDESCI    = H1DESCI
086800100805     c                   write     edmap000                             99
086900100805     C*
087000100805     C                   ENDSR
087100100805     C************************************************************
087200100805     C* Record di Raggruppamento
087300100805     C************************************************************
087400100805     C     Rcd_Gruppo    BEGSR
087500100805     C*
087600100805     c                   clear                   edmap000
087700100805     c                   eval      MAPUNBCLI   = C1UNBCLI
087800100805     c                   eval      MAPTIPOMSG  = C1TMSG
087900100805     c                   eval      MAPVERSION  = C1VERS
088000100805     c                   eval      MAPRELEASE  = C1RELS
088100100805     c                   eval      MAPAGENCY   = C1AGEN
088200100805     c                   eval      MAPASSOCIA  = C1ASSOC
088300100805      *
088400100806     c                   eval      MAPCATEGO   = SAVgruppo
088500100806     c                   eval      MAPCODICE   = C1CODICE
088600100806     c                   eval      MAPLUNGH    = Lung_Gruppo
088700100806     c                   eval      MAPSEQUEN   = Sequenza
088800100806     c                   eval      MAPDESCR    = *blank
088900100805     c                   eval      MAPDESCI    = *blank
089000100805     c                   eval      MAPOBBLIG   = *blank
089100100806     c                   eval      MAPTIPOD    = 'D'
089200100806      *
089300100805     c                   write     edmap000                             99
089400100805     C*
089500100805     C                   ENDSR
089600100805     C************************************************************
089700100805     C* Record di Dettaglio del campo
089800100805     C************************************************************
089900100805     C     Rcd_Campo     BEGSR
090000100805     C*
090100100805     c                   clear                   edmap000
090200100805     c                   eval      MAPUNBCLI   = C1UNBCLI
090300100805     c                   eval      MAPTIPOMSG  = C1TMSG
090400100805     c                   eval      MAPVERSION  = C1VERS
090500100805     c                   eval      MAPRELEASE  = C1RELS
090600100805     c                   eval      MAPAGENCY   = C1AGEN
090700100805     c                   eval      MAPASSOCIA  = C1ASSOC
090800100805     c                   eval      MAPCODICE   = C1CODICE
090900100805      *
091000100805     c                   eval      MAPSEQUEN   = Sequenza
091100100805     c                   eval      MAPPROGR    = Progress
091200100805     c                   eval      MAPCATEGO   = S1GRUPPO
091300100805     c                   eval      MAPNCAMPO   = S1CAMPO
091400100805      *
091500100805     c                   eval      MAPTIPOD    = S1TIPO
091600100805     c                   eval      MAPLUNGH    = S1LUNGO
091700100806     c                   eval      MAPDECIM    = S1DECIM
091800100805     c                   eval      MAPDESCR    = S1DESCR
091900100805     c                   eval      MAPDESCI    = S1DESCRI
092000100806     c                   If        S1OBBL  = 'S'
092100100806     c                   eval      MAPOBBLIG = 'M'
092200100806     c                   else
092300100806     c                   eval      MAPOBBLIG = 'C'
092400100806     c                   end
092500100805      *
092600100805     c                   write     edmap000                             99
092700100805     C*
092800100805     C                   ENDSR
092900100805     C************************************************************
093000100805     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
093100100805     C************************************************************
093200100805     C     GESAGG        BEGSR
093300100805     C*
093400940224     C* segnalo al pgm chiamante l'avvenuta conferma
093500030114    >C                   MOVE      *ON           xtaopr
0936009402241    C                   SELECT
093700940224     C* nel caso di immissione, ripristino la videata iniziale
093800030114    >C     xtaopz        WHENEQ    '01'
093900100804     C                   MOVE      *ON           $INZS1
094000030114     C                   MOVE      *ON           $FINE
094100940224     C* altrimenti ritorno al pgm chiamante
094200940224     C                   OTHER
094300940224     C                   MOVE      *ON           $FINE
0944009402241-   C                   ENDSL
094500940224     C*
094600940224     C                   ENDSR
094700940131     C************************************************************
094800940131     C* OPERAZIONI INIZIALI
094900940131     C************************************************************
095000940131     C     *INZSR        BEGSR
095100940131     C*
095200940127     C* Reperimento parametri
095300940117     C     *ENTRY        PLIST
095400940117     C                   PARM                    KPJBA
095500030113     C                   movel     kpjbu         tabds
095600100803      *
095700940223     C* Variabili per gestione videate
095800100803     c                   if        XTAOPZ ='01' or XTAOPZ ='03'
095900100804     C                   MOVE      'S1'          $GEST
096000100803     c                   else
096100100803     C                   MOVE      'S1'          $GEST
096200100803     c                   end
096300100803      *
096400100804     C                   MOVE      *BLANK        $EFILE            1
096500100804     C                   MOVE      *BLANK        $ESCI             1
096600100804     C                   MOVE      *BLANK        $RCDOK            1
096700940223     C                   MOVE      *OFF          $FINE
096800100804     C                   MOVE      *ON           $INZS1
096900940224     C                   MOVE      *BLANK        $LASTV
097000940127     C*
097100940223     C* Variabili appoggio
097200940224     C                   Z-ADD     0             CURR
097300940224     C                   Z-ADD     0             CURC
097400940207     C*
097500940207     C* Valorizzazione campi univoci testate
097600100804     C                   MOVEL     KNSIF         NOMSIF
097700100804     C                   MOVEL     DSPGM         NOMPGM
097800100804     C                   MOVEL     KNMUS         NOMDIT
097900100806     C                   SETOFF                                       020306
098000100805     C                   SELECT
098100100805     C* Annullamento
098200100805     C* Visualizzazione
098300100805    >C     xtaopz        WHENEQ    '01'
098400100805     c                   movel     Opz_Imm       C1Fun
098500100809    >C     xtaopz        WHENEQ    '03'
098600100809     c                   movel     Opz_Imm       C1Fun
098700100805    >C     xtaopz        WHENEQ    '05'
098800100805     c                   movel     Opz_Vis       C1Fun
098900100806     C                   SETON                                        02  06
099000100805    >C     xtaopz        WHENEQ    '04'
099100100805     c                   movel     Opz_Del       C1Fun
099200100805     C                   SETON                                        02
099300100805     C* Manutenzione
099400100805    >C     xtaopz        WHENEQ    '02'
099500100805     c                   movel     Opz_Var       C1Fun
099600100805     C                   OTHER
099700100805     C                   ENDSL
099800100805     C*
099900940127     C*
100000940117     C                   ENDSR
100100940128     C************************************************************
100200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
100300940223     C************************************************************
100400940223     C     DEFCAM        BEGSR
100500090226      *
100600100803     C     k_map01       kLIST
100700100805     C                   kfld                    oldunbcli
100800100805     C                   kfld                    oldtmsg
100900100805     C                   kfld                    oldvers
101000100805     C                   kfld                    oldrels
101100100805     C                   kfld                    oldagenz
101200100805     C                   kfld                    oldassoc
101300100805     C                   kfld                    oldcodice
101400100803     C*
101500100806     C     k_map02       kLIST
101600100806     C                   kfld                    oldunbcli
101700100806     C                   kfld                    oldtmsg
101800100806     C                   kfld                    oldvers
101900100806     C                   kfld                    oldrels
102000100806     C                   kfld                    oldagenz
102100100806     C                   kfld                    oldassoc
102200100806     C                   kfld                    gruppo            4
102300100810     C*
102400100810     C     k_map02_1     kLIST
102500100810     C                   kfld                    oldunbcli
102600100810     C                   kfld                    oldtmsg
102700100810     C                   kfld                    oldvers
102800100810     C                   kfld                    oldrels
102900100810     C                   kfld                    oldagenz
103000100810     C                   kfld                    oldassoc
103100100806     C*
103200940223     C* Variabili per gestione videate
103300940223     C                   MOVE      *BLANK        $GEST             2
103400940223     C                   MOVE      *BLANK        $FINE             1
103500100804     C                   MOVE      *BLANK        $INZS1            1
103600940223     C                   MOVE      *BLANK        $LASTG            2
103700940223     C                   MOVE      *BLANK        $LASTV            2
103800940223     C*
103900940223     C* Variabili appoggio
104000940223     C                   Z-ADD     0             CURR              2 0
104100940223     C                   Z-ADD     0             CURC              2 0
104200940223     C                   MOVE      *ZEROS        WIN              99
104300940224     C                   MOVE      *BLANK        WTBERR            1
104400940223     C*
104500940223     C* Indici
104600940223     C                   Z-ADD     0             X                 3 0
104700940127     C*
104800940117     C                   ENDSR
104900100804     C/EJECT
105000100804     C************************************************************
105100100804      *?  INIZIALIZZA indicatori di errore    ?
105200100804     C************************************************************
105300100804     C     INZ_indERR    BEGSR
105400100804      *
105500100804     C                   move      *ALL'0'       IN5098
105600100804     C                   moveA     IN5098        *IN(50)
105700100804      *
105800100804     C                   ENDSR
105900100804     C/EJECT
106000100804      *---------------------------------------------------------------*
