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
002300100803$001 FTRTCTB0D2 CF   E             WORKSTN
002400100803     F                                     SFILE(S1:S1NRR)
002500940201     F                                     INFDS(DSFMT)
002600100806     Fedmap01l  uF a E           K DISK
002700100806     Fedmap02l  iF   E           K DISK    rename(EDMAP000:EDMAP2)
002800940128     D*----------------------------------------------------*
002900940127     D* Passaggio Parametri
003000940127     D KPJBA         E DS
003100030113      *-------------
003200030113     D* Parametri in ricezione
003300030113     D  TABDS          DS
003400030113     D  XTAOPZ                 1      2
003500030113     D  XTARET                 3      3
003600030113     D  XTAOPR                 4      4
003700030113     D  XTAERR                 5      5
003800100805     D  XTAKEY                 6     20
003900100805$003 D Kcodice                             Like(MAPcodice)
004000100805$003 D Kunbcli                             Like(MAPUNBCLI)
004100100805$003 D Ktmsg                               Like(MAPTIPOMSG)
004200100805$003 D Kvers                               Like(MAPVERSION)
004300100805$003 D Krels                               Like(MAPRELEASE)
004400100805$003 D Kagenz                              Like(MAPAGENCY)
004500100805$003 D Kassoc                              Like(MAPASSOCIA)
004600100805$003 D KObblig                             Like(MAPOBBLIG)
004700100805$003 D KDescr                              Like(MAPDESCR)
004800100805$003 D KDesci                              Like(MAPDESCI)
004900100805$003 D OLDCodice                           Like(MAPcodice)
005000100805$003 D OLDunbcli                           Like(MAPUNBCLI)
005100100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
005200100805$003 D OLDvers                             Like(MAPVERSION)
005300100805$003 D OLDrels                             Like(MAPRELEASE)
005400100805$003 D OLDagenz                            Like(MAPAGENCY)
005500100805$003 D OLDassoc                            Like(MAPASSOCIA)
005600100809     d  k_unbcli                           Like(MAPUNBCLI)
005700100809     d  k_tmsg                             Like(MAPTIPOMSG)
005800100809     d  k_vers                             Like(MAPVERSION)
005900100809     d  k_rels                             Like(MAPRELEASE)
006000100809     d  k_agenz                            Like(MAPAGENCY)
006100100809     d  k_assoc                            Like(MAPASSOCIA)
006200940201     D*-------------
006300030204     D  S_TASTO        S                   like($Tasto)
006400940201     D DSFMT           DS
006500940506     D  $TASTO               369    369
006600940201     D  NRG                  370    370
006700940201     D  NCL                  371    371
006800940201     D*-------------
006900940201     D* posizione cursore
007000940201     D CURSOR          DS
007100940223     D  RIRI                   1      2B 0 INZ
007200940201     D  R$$                    2      2
007300940223     D  COCO                   3      4B 0 INZ
007400940201     D  C$$                    4      4
007500940207     D*-------------
007600940207     D* Reperimento nome PGM
007700940207     D STATUS         SDS           333
007800940207     D  DSPGM            *PROC
007900100806     D*-------------
008000100806     D rigaSFL         DS
008100100806     D  S1GRUPPO
008200100806     D  S1CAMPO
008300100806     D  S1OBBL
008400100806     D  S1TIPO
008500100806     D  S1DESCR
008600100806     D  S1DESCRI
008700100804     D*-------------
008800100805     D RigPag_SFl1     S              3  0 INZ(16)
008900100804$003 D S1NRR           S                   Like(C1rcd)
009000100804$003 D Wpagine         S                   Like(C1rcd)
009100100804     D S1pag           S                   Like(C1rcd)
009200100804     D WSfl            S                   Like(C1nrr)
009300100804     D Wmax            S                   Like(C1rcd)
009400100804     D Wpag            S                   Like(C1rcd)
009500100805     D savGRUPPO       S                   Like(s1GRUPPO)
009600940225     D*-------------
009700940225     D* COSTANTI
009800940225     D*-------------
009900940506     D* Tasti di funzione
010000940506     D F01             C                   CONST(X'31')
010100940506     D F02             C                   CONST(X'32')
010200940506     D F03             C                   CONST(X'33')
010300940506     D F04             C                   CONST(X'34')
010400940506     D F05             C                   CONST(X'35')
010500940506     D F06             C                   CONST(X'36')
010600940506     D F07             C                   CONST(X'37')
010700940506     D F08             C                   CONST(X'38')
010800940506     D F09             C                   CONST(X'39')
010900940506     D F10             C                   CONST(X'3A')
011000940506     D F11             C                   CONST(X'3B')
011100940506     D F12             C                   CONST(X'3C')
011200940506     D F13             C                   CONST(X'B1')
011300940506     D F14             C                   CONST(X'B2')
011400940506     D F15             C                   CONST(X'B3')
011500940506     D F16             C                   CONST(X'B4')
011600940506     D F17             C                   CONST(X'B5')
011700940506     D F18             C                   CONST(X'B6')
011800940506     D F19             C                   CONST(X'B7')
011900940506     D F20             C                   CONST(X'B8')
012000940506     D F21             C                   CONST(X'B9')
012100940506     D F22             C                   CONST(X'BA')
012200940506     D F23             C                   CONST(X'BB')
012300940506     D F24             C                   CONST(X'BC')
012400940506     D ENTER           C                   CONST(X'F1')
012500940506     D ROLDWN          C                   CONST(X'F4')
012600940506     D ROLLUP          C                   CONST(X'F5')
012700940506     D*-------------
012800941108     D DATA            C                   CONST('0001-01-01')
012900030114     D Annull          C                   CONST('  ANNULLATO  ')
013000030114     D OPz_Imm         C                   CONST('IMMISSIONE')
013100030114     D OPz_Var         C                   CONST('VARIAZIONE')
013200030114     D OPz_Del         C                   CONST(' ANNULLA  ')
013300030114     D OPz_Vis         C                   CONST('VISUALIZZA')
013400940127     C*----------------------------------------------------*
013500940127     C*                MAIN LINE PROGRAM
013600940127     C*----------------------------------------------------*
013700940223     C*
013800940223     C     $FINE         DOWEQ     *OFF
013900100803     C     $GEST         CASEQ     'S1'          GESS1
014000940117     C                   END
014100940117     C                   END
014200940325     C* fine programma
014300030113     c                   movel     tabds         kpjbu
014400030113     C                   Seton                                        LR
014500940131     C************************************************************
014600100804     C* GESTIONE VIDEO RECORD S1
014700940131     C************************************************************
014800100804     C     GESS1         BEGSR
014900030113      *
015000940225     C* inizializzazione videata
015100100804     C     $INZS1        IFEQ      *ON
015200100804     C                   EXSR      INZS1
015300100810     C                   z-add     1             riga_posiz
015400100804     C                   MOVE      *OFF          $INZS1
015500940117     C                   END
015600030113      *
015700100804     C                   WRITE     T1
015800100804     C                   WRITE     Z1
015900030114     C*
016000100810     c                   if         riga_posiz > 0
016100100810     C                   z-add     riga_posiz    C1RCD
016200100810     c                   else
016300100810     C*
016400100810     C                   z-add     c1nrr         C1RCD
016500100810     c                   end
016600030114     c                   SETOFF                                         99
016700940620     C* Immissione/Modifica/Copia
016800030113     C*               *----------------*
016900100804     C                   EXFMT     c1
017000030113     C*               *----------------*
017100100810     c                   clear                   riga_posiz        5 0
017200030114     C*
017300030114     C     *IN99         IFEQ      *OFF
017400030114     C*
0175000301141    C                   SELECT
017600030114     C* F3=Fine
017700030114     C     $TASTO        WHENEQ    F03
017800100804     C                   EXSR      F03S1
017900030114     C* F12=Ritorno
018000030114     C     $TASTO        WHENEQ    F12
018100100804     C                   EXSR      F12S1
018200030114     C*
0183000301141O   C                   OTHER
018400030114     C*
018500131018     C  n02              EXSR      CTRC1
018600131018     c  n02
018700131018     Cann99              EXSR      CTRS1
018800030113     C*
018900940616     C     *IN99         IFEQ      *OFF
019000100804    >C     xtaopz        andNE     '05'
019100940616     C* F6
0192000302052    C     $TASTO        ifeq      F06
0193000301152    C     $TASTO        oreq      F23
019400100804     C*
019500100805     c                   exfmt     ConfWind
019600100805     C     $TASTO        Ifeq      F06
019700100805     C*
019800100804     C* eseguo sempre la cancellazione
019900100805    >C     xtaopz        ifEQ      '02'
020000100805    >C     xtaopz        oreq      '04'
020100100804    >C                   EXSR      DELTAB
020200100805     c                   end
020300100804     C* eseguo scrittura
020400100804    >C     xtaopz        ifnE      '04'
020500100804    >C                   EXSR      WRITAB
020600100804     c                   end
020700100804     C*
020800100804    >C                   EXSR      GESAGG
020900030205     c                   end
021000030114     C*
021100100805     c                   else
021200100805     C*
021300100805     c                   end
021400100805     C*
021500030114     C                   ENDIF
021600030114     C*
0217000301142-   C                   ENDSL
021800030114     C*
0219000301142-   C                   ENDIF
022000030114     C*
022100940117     C                   ENDSR
022200940117      ************************************************************
022300940207      * INIZIALIZZAZIONE VIDEATA DATI
022400940117      ************************************************************
022500100804     C     INZS1         BEGSR
022600100804     C*
022700100804     C* reset indicatori DSPATR
022800100804     c                   exsr      INZ_indERR
022900100804     C* pulizia SFL
023000100804     C                   SETOFF                                         3031
023100100804     C                   WRITE     C1
023200100804     C                   SETON                                          31
023300100804     C*
023400100804     C* Righe x Pagina del 1° SFL
023500100805     c                   z-add     RigPag_SFl1   s1pag
023600100805     C                   MOVEL     Kcodice       C1codice
023700100804     C                   MOVEL     Kunbcli       C1UNBCLI
023800100804     C                   MOVEL     Ktmsg         C1TMSG
023900100804     C                   MOVEL     Kvers         C1VERS
024000100804     C                   MOVEL     Krels         C1RELS
024100100804     C                   MOVEL     Kagenz        C1AGEN
024200100804     C                   MOVEL     Kassoc        C1ASSOC
024300100804     C                   MOVEL     KDescr        C1DESCR
024400100804     C                   MOVEL     KObblig       H1OBBL
024500100804     C                   MOVEL     KDesci        H1DESCI
024600100804      *
024700100804     C* CARICAMENTO SFL totale
024800100804     C                   Z-ADD     0             S1NRR
024900100804     C                   Z-ADD     1             C1RCD
025000100804     C                   Z-ADD     0             WMAX
025100100804     C                   Z-ADD     0             Wsfl
025200100804     C* Posizionamento su file
025300100804     C*
025400100804     C* valorizzo la chiave univoca per reperire i dati
025500100804     C* se non immissione
0256001008041   >C     xtaopz        IFNE      '01'
025700100804      *
025800100804    >C*------------------------------------*
025900100804    >C* impostare i campi chiave ricevuti
026000100804    >C*------------------------------------*
026100100804     C     K_map01       setll     edMAP01l
026200100804     C*
026300100804     C* riempio la videata
026400100804     C                   IF        %Found(edmap01l)
026500100804     C* riempio la videata
026600100804     C                   EXSR      RolS1
026700100804      *
026800100804     C* richiamo routine dei ctrl per decodificarla
026900131018    >C  n02              EXSR      CTRS1
027000100804      *
027100100805     C                   MOVE      *ALL'0'       IN5098           49
027200100804     C                   MOVEA     IN5098        *IN(50)
027300100804      *
027400100804     C* definizione indicatori su video
027500100806     C                   SETOFF                                       02  06
027600100805      *
0277001008043    C                   SELECT
027800100804    >C     xtaopz        WHENEQ    '02'
027900100805    >C     xtaopz        WHENEQ    '04'
028000100805     C                   SETON                                        02
028100100804    >C     xtaopz        WHENEQ    '05'
028200100805     C                   SETON                                        02  06
0283001008043-   C                   ENDSL
028400100804     C*
0285001008042-   C                   ENDIF
028600100804     C*
0287001008041   >C                   Else
028800100805      *
028900100805     C* riempio la videata di righe vuote da riempire
029000100805     C                   EXSR      RolS1
029100100805     C*
0292001008041-   C                   ENDIF
029300100804     C*
029400940117     C                   ENDSR
029500940207     C/EJECT
029600100804      *---------------------------------------------------------------*
029700100804      *? CARICAMENTO PAGINA LISTA                                    ?
029800100804      *---------------------------------------------------------------*
029900100804     C     ROLS1         BEGSR
030000100804     C*
030100100804     C                   SETOFF                                       32
030200100804     C                   Z-ADD     WMAX          S1NRR
030300100804     C*
030400100804     C* Caricamento del Sfl x Giro
030500100804     c                   Exsr      Write_S1
030600100804     C*
030700100804     C                   Z-ADD     S1NRR         WMAX                 30
030800100804     C*
030900100804     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
031000100804     C     S1NRR         DIV       S1pag         PAGINE            4 0
031100100804     C                   MVR                     RESTO             3 0
031200100804     C     PAGINE        MULT      S1pag         C1RCD
0313001008041    C     RESTO         IFGT      0
031400100804     C                   ADD       1             C1RCD
0315001008041E   C                   ELSE
031600100804     C                   SUB       S1pag         C1RCD
031700100804     C                   ADD       1             C1RCD
0318001008041-   C                   ENDIF
031900100804     C*
032000100804     C                   ENDSR
032100100804      *---------------------------------------------------------------*
032200100804      *?  Scrive SFL                                                 ?
032300100804      *---------------------------------------------------------------*
032400100805     C     Write_S1      BEGSR
032500100805      *
032600100805     c                   clear                   pos_cursor        1
032700100805      *
032800100805      *  Se NON inserimento
032900100805    >C                   if        xtaopz <> '01'
033000100805      *
033100100804     C*  Esegue una lettura di posizionamento GIRO fuori
033200100804      *   per poi poter totalizzare x Giro all'interno della Rout.RIGA_Giro
033300100804      *   richiamata anche in seguito.
033400100804     c                   exsr      Legge_rec
033500100804     C*
033600100804      * Finchè il giro è lo stesso
0337001008041    C     $EFILE        DowEQ     *OFF
033800100804     C*
033900100804     C* ---------
034000100804     C* si ricorda il gruppo precedente per calcolare in seguito la DS
034100100804     C*   da scrivere
034200100804     C* ---------
034300100804     C                   clear                   H1GRU_PRE
034400100804     C                   clear                   H1lun_PRE
034500100804     C*
034600100804     c                   if        s1gruppo <> mapcatego
034700131018      *---------
034800131018      *  Scrive la riga di SFL a rottura di giro
034900131018     C   02              exsr      RIGA_blank
035000100804     C*   a rottura di gruppo ossia di categoria
035100100804     C*
035200100804     C                   MOVEL     S1GRUPPO      H1GRU_PRE
035300100804     C                   z-add     lung_GRUPPO   H1lun_PRE
035400100804     c                   clear                   lung_GRUPPO       5 0
035500100804     C*
035600100804     C*   quando è uguale somma
035700100804     c                   else
035800100804     c                   if        mapCATEGO <> *blank
035900100804     c                   add       mapLUNGH      lung_GRUPPO
036000100804     c                   endif
036100100804     c                   end
036200100804     C* ---------
036300100804     C                   MOVEL     MAPCATEGO     S1GRUPPO
036400100804     C                   MOVEL     MAPNCAMPO     S1CAMPO
036500100804     C                   if          mapOBBLIG = 'M'
036600100804     C                   MOVEL     'S'           S1obbl
036700100804     c                   else
036800100804     C                   MOVEL     'N'           S1obbl
036900100804     c                   end
037000100804     C                   MOVEL     MAPTIPOD      S1TIPO
037100100804     C                   z-add     MAPLUNGH      S1LUNGO
037200100804     C                   z-add     MAPDECIM      S1DECIM
037300100804     C                   MOVEL     mapDESCR      S1descr
037400100804     C                   MOVEL     mapDESCI      S1descri
037500100805      *
037600100805     c                   setoff                                       11
037700100805     c                   if        pos_cursor <> 'S'
037800100805     c                   setoN                                        11
037900100805     c                   eval      pos_cursor  = 'S'
038000100805     c                   end
038100100804      *---------
038200100804      *  Scrive la riga di SFL a rottura di giro
038300100804     C                   ADD       1             S1NRR
038400100804     C                   WRITE     S1
038500100804      *---------
038600100804     c                   exsr      Legge_rec
038700100804     c                   EndDO
038800100804     C*
038900100805     c                   End
039000100805     C*
039100100805    >C                   if        xtaopz<>'04' and xtaopz<>'05'
039200100805     C*
039300100805     C     S1NRR         DIV       S1pag         PAGINE            4 0
039400100805     C                   MVR                     RESTO             3 0
039500100805     C*
039600100804     C*  deve scrivere tot pagine vuote da scrivere
039700100805      *  Se INSerimento
039800100805     c     RigPag_SFl1   mult      10            volte             5 0
039900100805     c                   sub       resto         volte
040000100805     C*
040100100805     c                   do        volte         contariga         5 0
040200100805     C*
040300100805      *---------
040400100805     c                   clear                   H1LUN_PRE
040500100805     c                   clear                   H1GRU_PRE
040600100805     c                   clear                   S1GRUPPO
040700100805     c                   clear                   S1CAMPO
040800100805     c                   clear                   S1OBBL
040900100805     c                   clear                   S1TIPO
041000100805     c                   clear                   S1LUNGO
041100100805     c                   clear                   S1DECIM
041200100805     c                   clear                   S1DESCR
041300100805     c                   clear                   S1DESCRI
041400100805     C                   SETOFF                                       0203
041500100805     c                   setoff                                       11
041600100805     c                   if        pos_cursor <> 'S'
041700100805     c                   setoN                                        11
041800100805     c                   eval      pos_cursor  = 'S'
041900100805     c                   end
042000100805      * indicatore (+) spento
042100100805     C                   SETOFF                                       33
042200100805     C                   IF        volte = contariga
042300100805     C                   SETON                                        33
042400100805     c                   end
042500100805      *  Scrive la riga di SFL a rottura di giro
042600100805     C                   ADD       1             S1NRR
042700100805     C                   WRITE     S1
042800100805      *---------
042900100805     c                   EndDo
043000100805     c                   End
043100100804     C*
043200100804     C                   ENDSR
043300131018      *---------------------------------------------------------------*
043400131018      *?  INSERISCE UNA RIGA IN PIÙ VUOTA                            ?
043500131018      *---------------------------------------------------------------*
043600131018     C     RIGA_blank    BEGSR
043700131018     C*
043800131018     C                   CLEAR                   s1gruppo
043900131018     C                   CLEAR                   s1campo
044000131018     C                   CLEAR                   s1obbl
044100131018     C                   CLEAR                   s1tipo
044200131018     C                   CLEAR                   s1lungo
044300131018     C                   CLEAR                   s1decim
044400131018     C                   CLEAR                   s1descr
044500131018     C                   CLEAR                   s1descri
044600131018     C                   ADD       1             S1NRR
044700131018     C                   WRITE     S1
044800131018     C                   ENDSR
044900100804      *---------------------------------------------------------------*
045000100804      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
045100100804      *---------------------------------------------------------------*
045200100804     C     LEGGE_REC     BEGSR
045300100804     C*
045400100804     C*  Per EoF deve uscire
045500100804     C                   MOVEL     *OFF          $EFILE
045600100804     C*  Per Record Ok deve uscire riportando i dati
045700100804     C                   MOVEL     *OFF          $RCDOK
045800100804     C*
0459001008041    C     $EFILE        DOUEQ     *ON
046000100804     C     $RCDOK        OREQ      *ON
046100100804     C*
046200100804     c     k_map01       reade     edMAP01L
046300100804      * Fine File
046400100804     C                   IF        %Eof(edMAP01l)
046500100804     C                   MOVEL     *ON           $EFILE
046600100804     C                   MOVE      $EFILE        *IN33
046700100804     C                   else
046800100804     c                   if        mapNCAMPO <> *blank
046900100804     C                   MOVE      *ON           $RCDOK
047000100804     C                   end
047100100804     C                   end
047200100804      *
0473001008041-   C                   ENDDO
047400100804     C*
047500100804     C                   ENDSR
047600100804     C/EJECT
047700100804      *---------------------------------------------------------------*
047800100804      *?   CONTROLLO TESTATA LISTA                                   ?
047900100804      *---------------------------------------------------------------*
048000100804     C     CTRC1         BEGSR
048100100804     C                   MOVE      *OFF          *IN99
048200100804     C                   ENDSR
048300100804     C/EJECT
048400100804      *---------------------------------------------------------------*
048500100804      *?   CONTROLLO OPZIONI LISTA                                   ?
048600100804      *---------------------------------------------------------------*
048700100804     C     CTRS1         BEGSR
048800100804     C*
048900100804     C                   MOVEL     *OFF          $ESCI
049000100804     C                   SETOFF                                       99
049100100804     c                   clear                   tot_lunghezza     5 0
049200100805     c                   clear                   resto
049300100805     c                   clear                   riga_vuota        1
049400100806     c                   clear                   solo_gruppo       1
049500100804     C*
049600100804     C* Leggo il sfl solo se ci sono rcd
0497001008041    C     WMAX          IFGT      0
049800100806     c                   z-add     wmax          wmaxrighe         5 0
049900100806     C*
050000100806     c                   do        wmaxrighe     s1nRR
050100100806     C     s1nrr         chain     S1                                 21
050200100804     C*
050300100804     C* esce se fine sfl o errore che richiede l'uscita
0504001008062    C     *IN21         ifEQ      *OFF
050500100804     C     $ESCI         ANDEQ     *OFF
050600100804     C                   Z-ADD     S1NRR         C1RCD
050700100804     C* ctrl su riga
050800100805     C                   EXSR      RECS1
050900100804      *
051000100804      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
051100100806     c                   add       s1lungo       tot_lunghezza
051200100804     C                   MOVE      *ON           *IN32
051300100804     C                   UPDATE    S1
051400100806     c                   if        *in99
051500100806     c                   leave
051600100806     c                   endIf
051700100806     C*
0518001008043-   C                   ENDIF
051900100804     C*
0520001008042-   C                   ENDDO
052100100804     C*
0522001008041-   C                   ENDIF
052300100806     C*
052400100806     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
052500100806     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0526001008063    C     $ESCI         IFEQ      *OFF
0527001008064    C     *IN21         andEQ     *ON
052800100806     C* calcolo pagina a cui deve posizionarsi
052900100806     c                   z-add     s1pag         Wpagine
053000100806     C                   EXSR      CLCPAG
0531001008064-   C                   ENDIF
053200100804     C*
053300100804     C                   ENDSR
053400100804     C/EJECT
053500100804      *---------------------------------------------------------------*
053600100804      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
053700100804      *---------------------------------------------------------------*
053800100804     C     CLCPAG        BEGSR
053900100804     C* Input :
054000100804     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
054100100804     C*          cursore
054200100804     C* - Wpagine= numero rcd per pagina sfl
054300100804     C* Output :
054400100804     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
054500100804     C*
054600100804     C     WSFL          DIV       Wpagine       PAGINE            4 0
054700100804     C                   MVR                     RESTO             3 0
054800100804     C     RESTO         IFGT      0
054900100804     C                   ADD       1             PAGINE
055000100804     C                   ENDIF
055100100804     C     PAGINE        MULT      Wpagine       WPAG
055200100804     C*
055300100804     C                   ENDSR
055400100804     C/EJECT
055500100803     C************************************************************
055600100803     C* CONTROLLO VIDEATA
055700100803     C************************************************************
055800100804     C     RECS1         BEGSR
055900100803     C*
056000100803     C                   SETOFF                                       99
056100100804     C*
056200100804     C* reset indicatori DSPATR
056300100804     c                   exsr      INZ_indERR
056400100805      *
056500100806      * A riga vuota se lo memorizza x dopo
056600100806$017 C     RIGAsfl       IFEQ      *blank
056700100804$017 C     S1lungo       andEQ     *zero
056800100806$017 C     S1decim       andEQ     *zero
056900100806     c                   move      'S'           riga_vuota        1
057000100806     c                   clear                   solo_GRUPPO       1
057100100806      *
057200100804     C                   Else
057300100806      *
057400100805     C*  Non deve esistere una riga vuota fra righe valorizzate.
057500100805     c                   if          riga_vuota = 'S'
057600100805     C                   SETON                                        5999
057700100810     C                   goto      priority
057800100810     C                   END
057900100806     C*
058000100806     C*  Se voglio copiare un gruppo di righe già presenti imposto solo il Gruppo
058100100806     C*   e premo invio. e automaticamente copierà il gruppo già presente
058200100806     C*   in altri segmenti su quello che sto immettendo.
058300100810     c                   move      'N'           Gruppo_trovato    1
058400100806$017 C                   IF        S1gruppo <> *blank
058500100806$017 C     S1campo       IFEQ      *blank
058600100806$017 C     S1tipo        andEQ     *blank
058700100806$017 C     S1obbl        andEQ     *blank
058800100806$017 C     S1lungo       andEQ     *zero
058900100806$017 C     S1decim       andEQ     *zero
059000100806$017 C     S1descr       andEQ     *blank
059100100806$017 C     S1descri      andEQ     *blank
059200100806     C                   Exsr      Decod_GRUPPO
059300100806     c                   move      'S'           solo_GRUPPO       1
059400100806     C                   END
059500100806     C                   END
059600100806     C*
059700100810     c                   move      'N'           Campo_trovato     1
059800100810     C* Se non ha trovato il gruppo tenta semplicemente con il solo campo
059900100810$017 C                   IF        S1gruppo <> *blank and s1campo <> *blank
060000100810     c                             and Gruppo_trovato = 'N'
060100100810$017 C     S1tipo        IFEQ      *blank
060200100810$017 C     S1obbl        andEQ     *blank
060300100810$017 C     S1lungo       andEQ     *zero
060400100810$017 C     S1decim       andEQ     *zero
060500100810$017 C     S1descr       andEQ     *blank
060600100810$017 C     S1descri      andEQ     *blank
060700100810     C                   Exsr      Decod_campo
060800100810     C                   END
060900100810     C                   END
061000100810     C*
061100100806     C*  Se non c'è solo il gruppo
061200100806     c                   IF         solo_GRUPPO = *Blank
061300100804     C*
061400100804$017 C     S1campo       IFEQ      *blank
061500100804     C                   SETON                                        5199
061600100804     C                   END
061700100804     C*
061800100804$017 C     S1tipo        IFEQ      *blank
061900100804     C                   SETON                                        5299
062000100804     C                   END
062100100804     C*
062200100804$017 C     S1obbl        IFEQ      *blank
062300100804     C                   SETON                                        5499
062400100804     C                   END
062500100804     C*
062600100804$017 C     S1lungo       IFEQ      *zero
062700100804     C                   SETON                                        5399
062800100804     C                   END
062900100804     C*
063000100806$017 C     S1decim       IFgt      *zero
063100100806$017 C     S1tipo        andne     'N'
063200100806     C                   SETON                                        5699
063300100806     C                   END
063400100806     C*
063500100804$017 C     S1descr       IFEQ      *blank
063600100804     C                   SETON                                        5599
063700100804     C                   END
063800100803     C*
063900100806     C                   END
064000100806     C*
064100100804     C                   ENDif
064200100806     C*---------------------------------------------------------------------------
064300100810     c     priority      tag
0644001008063    C     *IN99         IFEQ      *ON
064500100806     C     $ESCI         OREQ      *ON
064600100806     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
064700100806     C* e la ripristinerò a conclusione del ciclo di READC
064800100806     C                   MOVE      *OFF          $INZS1
064900100810     C                   z-add     s1nrr         riga_posiz
0650001008063-   C                   ENDIF
065100100803     C*
065200100803     C                   ENDSR
065300100803     C/EJECT
065400940309     C************************************************************
065500100806     C* decodifica il gruppo di campi
065600940309     C************************************************************
065700100806     C     Decod_GRUPPO  BEGSR
065800940309     C*
065900100809     c                   move      'N'           Clear_key         1
066000100806     c                   clear                   gia_zero          1
066100100806     C                   movel     s1gruppo      gruppo            4
066200100809     c                   eval        k_unbcli  =  oldunbcli
066300100809     c                   eval        k_tmsg    =  oldtmsg
066400100809     c                   eval        k_vers    =  oldvers
066500100809     c                   eval        k_rels    =  oldrels
066600100809     c                   eval        k_agenz   =  oldagenz
066700100809     c                   eval        k_assoc   =  oldassoc
066800100810     c     di_nuovo      tag
066900100809     c*
067000100806     C     k_map02       setll     edmap02l
067100100806     C     k_map02       reade     edmap02l
067200100806     C*
067300100806     c                   dow       not %Eof(edmap02l)
067400100806     C*
067500100806     c                   if        mapnCAMPO <> *blank
067600100806     c                   eval        gia_zero = 'S'
067700100809     c                   eval        gruppo_trovato = 'S'
067800100806     C*
067900100806     c                   eval      s1gruppo = mapCATEGO
068000100806     c                   eval      s1campo  = mapNCAMPO
068100100806     c                   if        mapOBBLIG = 'M'
068200100806     c                   eval      s1obbl   = 'S'
068300100806     c                   else
068400100806     c                   eval      s1obbl   = 'N'
068500100806     c                   end
068600100806     c                   eval      S1TIPO   = mapTIPOD
068700100806     c                   eval      S1LUNGO  = mapLUNGH
068800100806     c                   eval      S1DECIM  = mapDECIM
068900100806     c                   eval      S1DESCR  = mapDESCR
069000100806     c                   eval      S1DESCRI = mapDESCI
069100100806     c                   update    s1
069200100806      *
069300100806      * aggancia la successiva rige di sfl
069400100806     c                   add       1             s1nrr
069500100806     c     s1nrr         chain     s1
069600100806      *
069700100806     c                   else
069800100806      *  se è uno zero successivo ha finito la copiatura dei records
069900100806     c                   if          gia_zero = 'S'
070000100806     c                   leave
070100100806     c                   end
070200100806     C*
070300100806     c                   end
070400100806     C*
070500100806     C     k_map02       reade     edmap02l
070600100806     c                   endDo
070700100806     C*
070800100809     c                   if        gruppo_trovato = 'N' and Clear_key='N'
070900100809     c                   eval        Clear_key =  'S'
071000100809     c                   eval        k_unbcli  =  *blank
071100100809     c                   eval        k_tmsg    =  *blank
071200100809     c                   eval        k_vers    =  *blank
071300100809     c                   eval        k_rels    =  *blank
071400100809     c                   eval        k_agenz   =  *blank
071500100809     c                   eval        k_assoc   =  *blank
071600100810     c                   goto      di_nuovo
071700100809     c                   end
071800100809     C*
071900100806     C                   ENDSR
072000100810     C/EJECT
072100100810     C************************************************************
072200100810     C* decodifica il semplice campo
072300100810     C************************************************************
072400100810     C     Decod_CAMPO   BEGSR
072500100810     C*
072600100810     c                   move      'N'           Clear_key         1
072700100810     c                   eval        k_unbcli  =  oldunbcli
072800100810     c                   eval        k_tmsg    =  oldtmsg
072900100810     c                   eval        k_vers    =  oldvers
073000100810     c                   eval        k_rels    =  oldrels
073100100810     c                   eval        k_agenz   =  oldagenz
073200100810     c                   eval        k_assoc   =  oldassoc
073300100810     c     x_campo       tag
073400100810     c*
073500100810     C     k_map02_1     setll     edmap02l
073600100810     C     k_map02_1     reade     edmap02l
073700100810     C*
073800100810     c                   dow       not %Eof(edmap02l)
073900100810     C*
074000100810     c                   if        mapnCAMPO <> *blank and mapnCAMPO = s1Campo
074100100810     c                   eval        campo_trovato = 'S'
074200100810     C*
074300100810     c                   if        mapOBBLIG = 'M'
074400100810     c                   eval      s1obbl   = 'S'
074500100810     c                   else
074600100810     c                   eval      s1obbl   = 'N'
074700100810     c                   end
074800100810     c                   eval      S1TIPO   = mapTIPOD
074900100810     c                   eval      S1LUNGO  = mapLUNGH
075000100810     c                   eval      S1DECIM  = mapDECIM
075100100810     c                   eval      S1DESCR  = mapDESCR
075200100810     c                   eval      S1DESCRI = mapDESCI
075300100810     c                   end
075400100810     C*
075500100810     C     k_map02_1     reade     edmap02l
075600100810     c                   endDo
075700100810     C*
075800100810     c                   if        campo_trovato = 'N' and Clear_key='N'
075900100810     c                   eval        Clear_key =  'S'
076000100810     c                   eval        k_unbcli  =  *blank
076100100810     c                   eval        k_tmsg    =  *blank
076200100810     c                   eval        k_vers    =  *blank
076300100810     c                   eval        k_rels    =  *blank
076400100810     c                   eval        k_agenz   =  *blank
076500100810     c                   eval        k_assoc   =  *blank
076600100810     c                   goto      x_campo
076700100810     c                   end
076800100810     C*
076900100810     C                   ENDSR
077000100806     C/EJECT
077100100806     C************************************************************
077200100806     C* GESTIONE F03 VIDEO S1
077300100806     C************************************************************
077400100806     C     F03S1         BEGSR
077500100806     C*
077600940309     C                   MOVE      *ON           $FINE
077700030113    >C                   MOVE      '1'           xtaret
077800940325     C*
077900940325     C                   ENDSR
078000940309     C************************************************************
078100100804     C* GESTIONE F12 VIDEO S1
078200940309     C************************************************************
078300100804     C     F12S1         BEGSR
078400940309     C*
078500940309     C                   MOVE      *ON           $FINE
078600100805    >C                   MOVE      '2'           xtaret
078700940309     C*
078800940309     C                   ENDSR
078900940224     C************************************************************
079000940315    >C* AGGIORNAMENTO ANAGRAFICA
079100940224     C************************************************************
079200100804    >C     DELTAB        BEGSR
079300940207     C*
079400100805     C     k_map01       setll     edmap01l
079500100805     C     k_map01       reade     edmap01l
079600100805     c                   dow       not %EoF(edmap01l)
079700100805     C                   delete    edmap000
079800100805     C     k_map01       reade     edmap01l
079900100805     C                   END
080000940203     C*
080100940203     C                   ENDSR
080200100804     C************************************************************
080300100804    >C* AGGIORNAMENTO ANAGRAFICA
080400100804     C************************************************************
080500100804    >C     WRITAB        BEGSR
080600100805     C*
080700100805     C                   clear                   savGRUPPO
080800100805     c                   z-add     0             sequenza          5 0
080900100805     c                   z-add     0             progress          5 0
081000100805     C                   SETOFF                                       99
081100100805     C*
081200100805     C* Scrittura record Segmento Totale
081300100805     c                   exsr      Rcd_Testata
081400100805     C*
081500100806     C* Leggo il sfl solo se ci sono rcd
0816001008061    C     WMAX          IFGT      0
081700100806     C*
081800100806     c                   do        wmax          s1nRR
081900100806     C     s1nrr         chain     S1                                 21
082000100806     C*
082100100806      *
082200100806     C* esce se fine sfl o errore che richiede l'uscita
0823001008062    C     *IN21         ifEQ      *OFF
082400100806     C     $ESCI         ANDEQ     *OFF
082500100805      *
082600100805      * A riga vuota esce dal ciclo di aggiornamento
082700100806$017 C     RIGAsfl       IFEQ      *blank
082800100806$017 C     S1lungo       andEQ     *zero
082900100806$017 C     S1decim       andEQ     *zero
083000100805     c                   LEAVE
083100100805     c                   end
083200100805      *
083300100805      * a rottura di gruppo di campi
083400100805     c                   if        S1GRUPPO <> SAVgruppo or S1GRUPPO = *blank
083500100806     c                   z-add     0             progress
083600100806      *
083700100806      *   scrive solo se il gruppo non è blank
083800100806     c                   if        SAVgruppo <> *blank
083900100805     c                   exsr      Rcd_Gruppo
084000100806     c                   end
084100100806      *
084200100806     c                   z-add     0             Lung_Gruppo
084300100805     c                   add       10            sequenza
084400100805     c                   eval        SAVgruppo = S1GRUPPO
084500100805     c                   end
084600100805      *
084700100805     c                   add       1             progress
084800100806     c                   add       s1LUNGO       Lung_Gruppo
084900100805     c                   exsr      Rcd_campo
085000100805      *
085100100805      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
085200100805     C                   MOVE      *ON           *IN32
085300100805     C                   UPDATE    S1
085400100806     c                   end
085500100805      *
0856001008052-   C                   ENDDO
085700100806      *
085800100806      *   scrive solo se il gruppo non è blank
085900100806     c                   if        SAVgruppo <> *blank
086000100806     c                   exsr      Rcd_Gruppo
086100100806     c                   end
086200100804     C*
086300100806     c                   end
086400100806      *
086500100805     C* segnalo al pgm chiamante l'aver premuto l'F6
086600100805    >C                   MOVE      '0'           xtaret
086700100804     C*
086800100804     C                   ENDSR
086900940224     C************************************************************
087000100805     C* Record di testata del segmento
087100940224     C************************************************************
087200100805     C     Rcd_Testata   BEGSR
087300940224     C*
087400100805     c                   clear                   edmap000
087500100805     c                   eval      MAPUNBCLI   = C1UNBCLI
087600100805     c                   eval      MAPTIPOMSG  = C1TMSG
087700100805     c                   eval      MAPVERSION  = C1VERS
087800100805     c                   eval      MAPRELEASE  = C1RELS
087900100805     c                   eval      MAPAGENCY   = C1AGEN
088000100805     c                   eval      MAPASSOCIA  = C1ASSOC
088100100805     c                   eval      MAPCODICE   = C1CODICE
088200100805     c                   eval      MAPOBBLIG   = H1OBBL
088300100805     c                   eval      MAPLUNGH    = tot_lunghezza
088400100805     c                   eval      MAPDESCR    = C1DESCR
088500100805     c                   eval      MAPDESCI    = H1DESCI
088600100805     c                   write     edmap000                             99
088700100805     C*
088800100805     C                   ENDSR
088900100805     C************************************************************
089000100805     C* Record di Raggruppamento
089100100805     C************************************************************
089200100805     C     Rcd_Gruppo    BEGSR
089300100805     C*
089400100805     c                   clear                   edmap000
089500100805     c                   eval      MAPUNBCLI   = C1UNBCLI
089600100805     c                   eval      MAPTIPOMSG  = C1TMSG
089700100805     c                   eval      MAPVERSION  = C1VERS
089800100805     c                   eval      MAPRELEASE  = C1RELS
089900100805     c                   eval      MAPAGENCY   = C1AGEN
090000100805     c                   eval      MAPASSOCIA  = C1ASSOC
090100100805      *
090200100806     c                   eval      MAPCATEGO   = SAVgruppo
090300100806     c                   eval      MAPCODICE   = C1CODICE
090400100806     c                   eval      MAPLUNGH    = Lung_Gruppo
090500100806     c                   eval      MAPSEQUEN   = Sequenza
090600100806     c                   eval      MAPDESCR    = *blank
090700100805     c                   eval      MAPDESCI    = *blank
090800100805     c                   eval      MAPOBBLIG   = *blank
090900100806     c                   eval      MAPTIPOD    = 'D'
091000100806      *
091100100805     c                   write     edmap000                             99
091200100805     C*
091300100805     C                   ENDSR
091400100805     C************************************************************
091500100805     C* Record di Dettaglio del campo
091600100805     C************************************************************
091700100805     C     Rcd_Campo     BEGSR
091800100805     C*
091900100805     c                   clear                   edmap000
092000100805     c                   eval      MAPUNBCLI   = C1UNBCLI
092100100805     c                   eval      MAPTIPOMSG  = C1TMSG
092200100805     c                   eval      MAPVERSION  = C1VERS
092300100805     c                   eval      MAPRELEASE  = C1RELS
092400100805     c                   eval      MAPAGENCY   = C1AGEN
092500100805     c                   eval      MAPASSOCIA  = C1ASSOC
092600100805     c                   eval      MAPCODICE   = C1CODICE
092700100805      *
092800100805     c                   eval      MAPSEQUEN   = Sequenza
092900100805     c                   eval      MAPPROGR    = Progress
093000100805     c                   eval      MAPCATEGO   = S1GRUPPO
093100100805     c                   eval      MAPNCAMPO   = S1CAMPO
093200100805      *
093300100805     c                   eval      MAPTIPOD    = S1TIPO
093400100805     c                   eval      MAPLUNGH    = S1LUNGO
093500100806     c                   eval      MAPDECIM    = S1DECIM
093600100805     c                   eval      MAPDESCR    = S1DESCR
093700100805     c                   eval      MAPDESCI    = S1DESCRI
093800100806     c                   If        S1OBBL  = 'S'
093900100806     c                   eval      MAPOBBLIG = 'M'
094000100806     c                   else
094100100806     c                   eval      MAPOBBLIG = 'C'
094200100806     c                   end
094300100805      *
094400100805     c                   write     edmap000                             99
094500100805     C*
094600100805     C                   ENDSR
094700100805     C************************************************************
094800100805     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
094900100805     C************************************************************
095000100805     C     GESAGG        BEGSR
095100100805     C*
095200940224     C* segnalo al pgm chiamante l'avvenuta conferma
095300030114    >C                   MOVE      *ON           xtaopr
0954009402241    C                   SELECT
095500940224     C* nel caso di immissione, ripristino la videata iniziale
095600030114    >C     xtaopz        WHENEQ    '01'
095700100804     C                   MOVE      *ON           $INZS1
095800030114     C                   MOVE      *ON           $FINE
095900940224     C* altrimenti ritorno al pgm chiamante
096000940224     C                   OTHER
096100940224     C                   MOVE      *ON           $FINE
0962009402241-   C                   ENDSL
096300940224     C*
096400940224     C                   ENDSR
096500940131     C************************************************************
096600940131     C* OPERAZIONI INIZIALI
096700940131     C************************************************************
096800940131     C     *INZSR        BEGSR
096900940131     C*
097000940127     C* Reperimento parametri
097100940117     C     *ENTRY        PLIST
097200940117     C                   PARM                    KPJBA
097300030113     C                   movel     kpjbu         tabds
097400100803      *
097500940223     C* Variabili per gestione videate
097600100803     c                   if        XTAOPZ ='01' or XTAOPZ ='03'
097700100804     C                   MOVE      'S1'          $GEST
097800100803     c                   else
097900100803     C                   MOVE      'S1'          $GEST
098000100803     c                   end
098100100803      *
098200100804     C                   MOVE      *BLANK        $EFILE            1
098300100804     C                   MOVE      *BLANK        $ESCI             1
098400100804     C                   MOVE      *BLANK        $RCDOK            1
098500940223     C                   MOVE      *OFF          $FINE
098600100804     C                   MOVE      *ON           $INZS1
098700940224     C                   MOVE      *BLANK        $LASTV
098800940127     C*
098900940223     C* Variabili appoggio
099000940224     C                   Z-ADD     0             CURR
099100940224     C                   Z-ADD     0             CURC
099200940207     C*
099300940207     C* Valorizzazione campi univoci testate
099400100804     C                   MOVEL     KNSIF         NOMSIF
099500100804     C                   MOVEL     DSPGM         NOMPGM
099600100804     C                   MOVEL     KNMUS         NOMDIT
099700100806     C                   SETOFF                                       020306
099800100805     C                   SELECT
099900100805     C* Annullamento
100000100805     C* Visualizzazione
100100100805    >C     xtaopz        WHENEQ    '01'
100200100805     c                   movel     Opz_Imm       C1Fun
100300100809    >C     xtaopz        WHENEQ    '03'
100400100809     c                   movel     Opz_Imm       C1Fun
100500100805    >C     xtaopz        WHENEQ    '05'
100600100805     c                   movel     Opz_Vis       C1Fun
100700100806     C                   SETON                                        02  06
100800100805    >C     xtaopz        WHENEQ    '04'
100900100805     c                   movel     Opz_Del       C1Fun
101000100805     C                   SETON                                        02
101100100805     C* Manutenzione
101200100805    >C     xtaopz        WHENEQ    '02'
101300100805     c                   movel     Opz_Var       C1Fun
101400100805     C                   OTHER
101500100805     C                   ENDSL
101600100805     C*
101700940127     C*
101800940117     C                   ENDSR
101900940128     C************************************************************
102000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
102100940223     C************************************************************
102200940223     C     DEFCAM        BEGSR
102300090226      *
102400100803     C     k_map01       kLIST
102500100805     C                   kfld                    oldunbcli
102600100805     C                   kfld                    oldtmsg
102700100805     C                   kfld                    oldvers
102800100805     C                   kfld                    oldrels
102900100805     C                   kfld                    oldagenz
103000100805     C                   kfld                    oldassoc
103100100805     C                   kfld                    oldcodice
103200100803     C*
103300100806     C     k_map02       kLIST
103400100806     C                   kfld                    oldunbcli
103500100806     C                   kfld                    oldtmsg
103600100806     C                   kfld                    oldvers
103700100806     C                   kfld                    oldrels
103800100806     C                   kfld                    oldagenz
103900100806     C                   kfld                    oldassoc
104000100806     C                   kfld                    gruppo            4
104100100810     C*
104200100810     C     k_map02_1     kLIST
104300100810     C                   kfld                    oldunbcli
104400100810     C                   kfld                    oldtmsg
104500100810     C                   kfld                    oldvers
104600100810     C                   kfld                    oldrels
104700100810     C                   kfld                    oldagenz
104800100810     C                   kfld                    oldassoc
104900100806     C*
105000940223     C* Variabili per gestione videate
105100940223     C                   MOVE      *BLANK        $GEST             2
105200940223     C                   MOVE      *BLANK        $FINE             1
105300100804     C                   MOVE      *BLANK        $INZS1            1
105400940223     C                   MOVE      *BLANK        $LASTG            2
105500940223     C                   MOVE      *BLANK        $LASTV            2
105600940223     C*
105700940223     C* Variabili appoggio
105800940223     C                   Z-ADD     0             CURR              2 0
105900940223     C                   Z-ADD     0             CURC              2 0
106000940223     C                   MOVE      *ZEROS        WIN              99
106100940224     C                   MOVE      *BLANK        WTBERR            1
106200940223     C*
106300940223     C* Indici
106400940223     C                   Z-ADD     0             X                 3 0
106500940127     C*
106600940117     C                   ENDSR
106700100804     C/EJECT
106800100804     C************************************************************
106900100804      *?  INIZIALIZZA indicatori di errore    ?
107000100804     C************************************************************
107100100804     C     INZ_indERR    BEGSR
107200100804      *
107300100804     C                   move      *ALL'0'       IN5098
107400100804     C                   moveA     IN5098        *IN(50)
107500100804      *
107600100804     C                   ENDSR
107700100804     C/EJECT
107800100804      *---------------------------------------------------------------*
