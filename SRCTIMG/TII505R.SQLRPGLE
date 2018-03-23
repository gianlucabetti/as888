000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300110826      *
000400110913      * TII505R *----------------------------------------------------*
000500110913      *     ?   - Conferma ORDINI MAGAZZINO -    ?
000600070403      *--------------------------------------------------------------*
000700110913      * in SEDE vengono prese le proposte di Ordine, inserite dalla Filiale,
000800110913      * e confermate generando veri ORDINI di acquisto per il Magazzino.
000900110913      * Dal file TMORP00F i dati vengono confermati nel TMMOC00F.
001000070227      *--------------------------------------------------------------*
001100110725      * Il n°d'ordine è univoco.
001200110725      *--------------------------------------------------------------*
001300940307      *  21           GENERICO OPERAZIONI I/O
001400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001500070301      *  23           GENERICO Operazioni varie
001600940224      *  30           SFLDSP
001700940224      * N31           SFLCLR
001800940224      *  31           SFLDSPCTL
001900940224      *  32           SFLNXTCHG
002000940224      *  33           SFLEND
002100940224      *  39           OF PRTF
002200070302      *  40 <---> 49  DSPATR SU video
002300940608      *  Specificare l'uso dei singoli indicatori
002400070227      *  50           Emissione della window totale/parziale
002500070227      *  51 <---> 90  ERRORI SU VIDEO
002600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002700070829      *  98           ERRORE SPECIALE :
002800940224      *  99           INDIC. GENERALE DI ERRORE
002900940128     F*----------------------------------------------------*
003000070404      *  Output
003100110906     FTMorp01L  iF   E           K DISK    rename(tmORP000:tmorp01)
003200110916     FTMorp02L  iF   E           K DISK    infds(orpds)
003300080609      *
003400070404      *  Input
003500110830     FazORG01l  if   e           k disk
003600110916     Ftii505d   CF   E             WORKSTN
003700110826     F                                     SFILE(TII5S00:S0NRR)
003800940201     F                                     INFDS(DSFMT)
003900070305     D*----------------------------------------------------*
004000070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004100070326     D*----------------------------------------------------*
004200940211     D* Passaggio Parametri
004300940211     D KPJBA         E DS
004400070227     D KPJBUs          s                   like(KPJBU)
004500110726     D*-------------
004600110726     d ficn70ds      e ds
004700110901     d  og143        e ds
004800110830     d dAUT          e ds
004900110916     d Tii502DS      e ds
005000110915     d Tii506DS      e ds
005100110830     D*-------------
005200940211     D DSFMT           DS           512
005300940506     D  $TASTO               369    369
005400940211     D  NRG                  370    370
005500940211     D  NCL                  371    371
005600940211     D  SFLNRR               378    379B 0
005700940127     D*-------------
005800940127     D* Reperimento nome PGM
005900940127     D STATUS         SDS           333
006000940127     D  DSPGM            *PROC
006100110830     D*-------------
006200110830     d orpds           ds
006300110830     d  orp_nrr              397    400b 0
006400110725     D*-------------
006500070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
006600110826$003 D S0NRR           S                   Like(C0rcd)
006700110826$003 D S0pag           S                   Like(C0rcd)
006800110916$003 D WSfl            S                   Like(C0nrr)
006900110916$003 D Wmax            S                   Like(C0rcd)
007000110916$003 D Wpag            S                   Like(C0rcd)
007100070330$003 D sav_WSfl        S                   Like(WSfl)
007200070330$003 D sav_Wmax        S                   Like(Wmax)
007300070330$003 D sav_Wpag        S                   Like(Wpag)
007400110916$003 D Wpagine         S                   Like(C0rcd)
007500110824      *--------------------------
007600110826     D RigPag_SFl0     S              3  0 INZ(18)
007700070227      *--------------------------
007800070418     D sav_d1cfgs      S                   Like(d1cfgs) inz(0)
007900110826$003 D sav_S0NRR       S                   Like(s0nrr)
008000101129      *--------------------------
008100110726     d dataiso         s               d   datfmt(*iso)
008200110825     d oggiiso         s               d   datfmt(*iso)
008300101129     d dataeur         s               d   datfmt(*eur)
008400070320      *--------------------------
008500110916     d Filiale         s                   like(d1cFGS)
008600070305      *--------------------------
008700110909     D Cambia_Video...
008800110909     D                 S              1A
008900110906     D dopo_alert_window...
009000110906     D                 S              1A
009100110830      *--------------------------
009200070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
009300070305     D WLBDAT          DS                  INZ
009400070305     D  G02DAT                 1      8  0
009500070227     D  G02INV                 9     16  0
009600070227     D  G02ERR                17     17
009700070227     D  G02TGI                18     22  0
009800070307      *
009900070307     D DATPAR          DS
010000070307     D  GIODAT                 1      8  0
010100070307     D  GIOINV                 9     16  0
010200070307     D  GIOTGI                17     21  0
010300070307      *
010400070307     d Wdata8          DS
010500070307     d  dadata                 1      8  0
010600070307     d  adata                  9     16  0
010700070307      *--------------------------------
010800070227     d AZUTEds       e ds                  extname(AZUTE00F)
010900070227     d DDatiUte      e ds
011000110725      *--------------------------
011100070320     d* Ds per dati organigramma
011200070404      *------
011300100505     d DTAISO          s               D
011400070320      *--------------------------
011500070320     d* Parametri x Controllo profilo utenti
011600070320     d TIBS34ds      e ds
011700070227      *--------------------------
011800070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
011900070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
012000070227      *--------------------------
012100070320      *?  COSTANTI                                      ?
012200070320      *--------------------------
012300940506     D* Tasti di funzione
012400940506     D F01             C                   CONST(X'31')
012500940506     D F02             C                   CONST(X'32')
012600940506     D F03             C                   CONST(X'33')
012700940506     D F04             C                   CONST(X'34')
012800940506     D F05             C                   CONST(X'35')
012900940506     D F06             C                   CONST(X'36')
013000940506     D F07             C                   CONST(X'37')
013100940506     D F08             C                   CONST(X'38')
013200940506     D F09             C                   CONST(X'39')
013300940506     D F10             C                   CONST(X'3A')
013400940506     D F11             C                   CONST(X'3B')
013500070315     D F12             C                   CONST(X'3C')
013600940506     D F13             C                   CONST(X'B1')
013700940506     D F14             C                   CONST(X'B2')
013800940506     D F15             C                   CONST(X'B3')
013900940506     D F16             C                   CONST(X'B4')
014000940506     D F17             C                   CONST(X'B5')
014100940506     D F18             C                   CONST(X'B6')
014200940506     D F19             C                   CONST(X'B7')
014300940506     D F20             C                   CONST(X'B8')
014400940506     D F21             C                   CONST(X'B9')
014500940506     D F22             C                   CONST(X'BA')
014600940506     D F23             C                   CONST(X'BB')
014700940506     D F24             C                   CONST(X'BC')
014800940506     D ENTER           C                   CONST(X'F1')
014900940506     D ROLDWN          C                   CONST(X'F4')
015000940506     D ROLLUP          C                   CONST(X'F5')
015100070301     d digits          c                   '0123456789'
015200110826     d Pianificato     c                   'P'
015300110831     d aConsumo        c                   'C'
015400110831     d Acquisti        c                   'A'
015500110831     d Marketing       c                   'M'
015600110914     d Tutto           c                   'T'
015700940127     C*----------------------------------------------------*
015800070419      *?     MAIN LINE PROGRAM                             ?
015900940127     C*----------------------------------------------------*
016000940223     C* inizializzazione variabili
016100940223     C                   EXSR      INZVAR
016200940223     C*
016300940223     C     $FINE         DOWEQ     *OFF
016400110909     C*
016500110915     c                   eval      cambia_video = *blank
016600110909     C*  re-imposta il passaggio fra video e video
016700110909     c                   if        $GEST <> sav$GEST and $GEST <> 'W1'
016800110909     c                   eval      cambia_video = 'S'
016900110909     C                   end
017000110823     C*
017100110906     C* Controllo Autorizzazione PROFILO
017200110906     C*  altrimenti non Entra a programma
017300110823     C     $GEST         CASEQ     'AU'          GESAU                          Controllo DAUT
017400110826     C*
017500110826     C* Finestra x ALLARME messaggi importanti
017600110826     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
017700110906     C*
017800110826     C* 1° Video
017900070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
018000070413     C*
018100110906     C* 2° Video SFL Sottoclassi da scegliere
018200110906     C     $GEST         CASEQ     'S0'          GESS0                          Lista SottoClassi
018300110906      *
018400940117     C                   END
018500110909      *
018600110909     C                   ENDdo
018700940325     C* fine programma
018800110901      **
018900940325     C                   SETON                                            LR
019000030113     C************************************************************
019100070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
019200030113     C************************************************************
019300030113     C     INZVAR        BEGSR
019400030113     C*
019500030113     C* Variabili per gestione videate
019600030113     C                   MOVE      *OFF          $FINE
019700110824     C                   MOVE      *OFF          dopo_in_$FINE
019800110824     C                   MOVE      *ON           $INZD1
019900110826     C                   MOVE      *OFF          $INZS0
020000030113     C                   MOVE      *OFF          $EFILE
020100030113     C                   MOVE      *OFF          $ESCI
020200030113     C                   MOVE      *OFF          $RCDOK
020300110915      **
020400110823     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
020500110823     C                   MOVE      'AU'          $GEST
020600030113     C* Variabili appoggio
020700030114     C                   Z-ADD     1             WPAG
020800030113     c*
020900030113     C                   ENDSR
021000110823     C************************************************************
021100110823      *?  Emissione primo video di SCELTA                        ?
021200110823     C************************************************************
021300110823     C     GESD1         BEGSR
021400110823     C*
021500070227     C* inizializzazione videata
021600070227     C     $INZD1        IFEQ      *ON
021700070227     C                   EXSR      INZD1
021800070227     C                   MOVE      *OFF          $INZD1
021900070227     C                   ENDIF
022000070227     C*
022100070228     c     Video_1       tag
022200070302     C*  Emissione Video
022300070228      * ?            *-----------------------*
022400110725     C                   EXFMT     TII5D01
022500070228      * ?            *-----------------------*
022600070326     c                   setoff                                       99
022700070305     C                   movel     'D1'          sav$GEST
022800070227     C* Selezioni
0229000702271    C                   SELECT
023000070227     C* F3=Fine
023100070227     C                   WHEN      $TASTO   =   F03
023200070227     C                   EXSR      F03_exit
023300070227      *
0234000702271O   C                   OTHER
023500070227     C* CONTROLLO DATI
023600070227     C                   EXSR      CTRD1
023700080626      *
023800110916      * ? Conferma gli Ordini selezionati
023900110916     C     *IN99         IFEQ      *OFF
024000110916     C     $TASTO        andeq     F06
024100110916      *
024200110916     C                   MOVE      *ON           $INZS0
024300110916     C                   movel     'S0'          $GEST
024400110916      *
024500110916     c                   end
024600110916      *
0247001109161-   C                   ENDSL
024800110902     C*
024900080606     C     Fine_GESD1    ENDSR
025000070306     C/EJECT
025100110726      *---------------------------------------------------------------*
025200070320      *?  INIZIALIZZAZIONE Primo video                           ?
025300110726      *---------------------------------------------------------------*
025400070227     C     INZD1         BEGSR
025500070227     C*
025600070227     C* Pulizia campi e indicatori
025700070308     c                   exsr      INZ_indERR
025800110726     C*
025900070227     C* Pulizia formato record
026000110725     c                   clear                   TII5D01
026100070227     C* campi titolo
026200070302     C                   MOVEL     dsPGM         D1CPGM
026300070227     C                   MOVEL     knsif         D1nsif
026400070227     C                   MOVEL     knmus         D1nmus
026500070227     C                   MOVEL     RSUT          D1crsu
026600110914     C                   MOVEL     Tutto         D1Torp
026700110926     C                   MOVEL     Acquisti      D1UFFc
026800110824     C                   z-add     settimana     D1week
026900070227     C* campi video
027000110914     C                   MOVEL     *zeros        D1CFGS
027100110830     C                   z-add     woggi         D1cDDC
027200110915     C                   z-add     0             Num_Ordine        5 0
027300110915     C                   z-add     0             Data_Ordine       8 0
027400070418     C*
027500080606     C     END_INZ1      ENDSR
027600940127     C************************************************************
027700070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
027800940127     C************************************************************
027900070227     C     CTRD1         BEGSR
028000070606     C*
028100070227     C*  Spegne indicatori di errore
028200070308     c                   exsr      INZ_indERR
028300110915     c                   setoff                                       99
028400110902     C* --------------
028500110914     d* ?- Controlla la filiale in gestione
028600110726     C*  Controlla il P.O.
028700110916     C     d1cFGS        ifGT      0
028800070227     C     d1cFGS        CHAIN     AZORG01l
028900110725      * Non presente in organigramma
029000110725    1C                   IF        not %found(azorg01l)
029100070326     C                   SETON                                        51  99
029200110914     c                   movel     ErrMsg(2)     d1msg
029300070227     C                   goto      ENDCTR1
029400110901    1C                   else
029500110901     c                   eval      og143 = orgDE3
029600110915    1C                   ENDIF
029700110916    1C                   ENDIF
029800110902      * >>>>>>>>>>>>>
029900110902      ***
030000110916      * Salva la filiale su campo di work x tutto il pgm
030100110916     c                   z-add     d1cfgs        Filiale
030200110916     C                   Z-ADD     D1cFGS        sav_d1cFGS
030300070307     C*---------------
030400070227     C     ENDCTR1       ENDSR
030500070319      *---------------------------------------------------------------*
030600110726      *  ?  GESTIONE SFL di immissione righe di ordine               ?
030700070319      *---------------------------------------------------------------*
030800110826     C     GESS0         BEGSR
030900070319     C*
031000940223     C* inizializzazione videata
031100110826     C     $INZS0        IFEQ      *ON
031200110826     C                   EXSR      INZS0
031300110826     C                   MOVE      *OFF          $INZS0
031400940127     C                   ENDIF
031500070327     C*
031600070306     C* Inizializza e imposta piede videata
031700110826     C                   EXSR      INZZ0
031800070306     C*
031900030113     C* emissione piede videata
032000070302      * ?            *-----------------------*
032100110826     C                   WRITE     TII5Z00
032200070302      * ?            *-----------------------*
032300070228     C*
032400940223     C     WMAX          IFEQ      0
032500070228     C* Non ci sono records
032600070302      * ?            *-----------------------*
032700110826     C                   WRITE     TII5V00
032800070302      * ?            *-----------------------*
032900070301     c                   setoff                                       30
033000030114     C                   Else
033100070301     c                   seton                                        30
033200070330     c                   eval       sav_WSfl =  WSfl
033300070330     c                   eval       sav_Wpag =  Wpag
033400070301      *
033500030114     C     Wsfl          IFgt      0
033600110826     C                   Z-ADD     wsfl          C0RCD
033700030114     C                   Else
033800030114     C     Wpag          IFgt      0
033900110826     C                   Z-ADD     wpag          C0RCD
034000030114     C                   EndIF
034100030114     C                   EndIF
034200030114     C                   ENDIF
034300070228      *
034400070228      *  Emissione SFL dei GIRI
034500070228      * ?            *-----------------------*
034600110826     C                   EXFMT     TII5C00
034700070228      * ?            *-----------------------*
034800070326     c                   setoff                                       99
034900110826     C                   movel     'S0'          sav$GEST
035000070228      *
035100110826     C     C0NRR         IFNE      0
035200110826     C                   Z-ADD     C0NRR         WSFL
035300940204     C                   ENDIF
035400110826     C                   Z-ADD     SFLNRR        C0RCD
035500030113     C* Selezioni
0356009401271    C                   SELECT
035700070328     C* F3=Fine
035800070328     C     $TASTO        WHENEQ    F03
035900070328     C                   EXSR      F03_exit
036000070328     C* F12=Ritorno
036100070328     C     $TASTO        WHENEQ    F12
036200070328     c                   move      'D1'          $GEST
036300070328     C                   MOVE      *ON           $INZD1
036400070328      *
0365009401271O   C                   OTHER
036600070228     C* CONTROLLO testata
036700110826     C                   EXSR      CTRC0
036800110915      *
036900110915     C* CONTROLLO subfile sulle scelte eseguite
037000940201     C     *IN99         IFEQ      *OFF
037100110915     C     $TASTO        andeq     F06
037200110826     C                   EXSR      CTRS0
037300940131     C                   END
037400070327      *
037500110915      * ? Conferma gli Ordini selezionati
037600110830     C     *IN99         IFEQ      *OFF
037700110915     C     $TASTO        andeq     F06
037800110830     c     ERRmsg80      andEQ     *blank
037900110830     C     WMAX          andGT     0
038000110915     C                   MOVE      *ON           $INZS0
038100110915     C                   movel     'S0'          $GEST
038200070314     c                   end
038300070228      *
0384009401271-   C                   ENDSL
038500940127     C*
038600940127     C                   ENDSR
038700070328     C/EJECT
038800070328      *---------------------------------------------------------------*
038900070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
039000070328      *---------------------------------------------------------------*
039100110826     C     INZZ0         BEGSR
039200070328     C*
039300070306     C* Messaggio o tasti funzionali
039400110826     c                   clear                   z0dmsg
039500110826     c                   clear                   ERRmsg80         80
039600070307     C*
039700070328      * imposta eventuali errori
039800110826     c                   eval      z0dmsg = ERRmsg80
039900110906     C*
040000070306     C                   ENDSR
040100070306     C/EJECT
040200070306      *---------------------------------------------------------------*
040300070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
040400070306      *---------------------------------------------------------------*
040500110826     C     INZS0         BEGSR
040600070306     C*
040700070228     C* reset indicatori DSPATR
040800070308     c                   exsr      INZ_indERR
040900940302     C* pulizia SFL
041000940128     C                   SETOFF                                         3031
041100110826     C                   WRITE     TII5C00
041200940128     C                   SETON                                          31
041300940128     C*
041400110826     C* Righe x Pagina del 0° SFL
041500110826     c                   z-add     RigPag_SFl0   s0pag
041600070228      *
041700030113     C* CARICAMENTO SFL totale
041800110826     C                   Z-ADD     0             S0NRR
041900110826     C                   Z-ADD     1             C0RCD
042000940128     C                   Z-ADD     0             WMAX
042100070302     C                   Z-ADD     0             Wsfl
042200070302     C* campi titolo
042300110826     C                   MOVEL     dsPGM         c0CPGM
042400110826     C                   MOVEL     knsif         c0nsif
042500110826     C                   MOVEL     knmus         c0nmus
042600110826     C                   MOVEL     RSUT          c0crsu
042700110826     C                   z-add     settimana     c0week
042800110830     C*
042900110725      **----------------
043000110725     C* Posizionamento su file pilota
043100110825     C                   eval        $EFILE    = *OFF
043200110915     C     d1cfgs        setll     tmORP01L
043300110725      **----------------
043400030113     C* Carico il SFL
043500110826     C                   EXSR      ROLS0
043600030113     C*
043700030114     C                   Z-ADD     1             WPAG
043800110726     c                   eval       sav_Wmax =  Wmax
043900110725     C*
044000110725     C                   ENDSR
044100110725     C/EJECT
044200110725      *---------------------------------------------------------------*
044300070320      *? CARICAMENTO PAGINA LISTA                                    ?
044400070305      *---------------------------------------------------------------*
044500110826     C     ROLS0         BEGSR
044600070305     C*
044700070305     C                   SETOFF                                       32
044800110831     C                   Z-ADD     1             Y
044900110826     C                   Z-ADD     WMAX          S0NRR
045000070305     C*
045100070305     C* Caricamento del Sfl x Giro
045200110826     c                   Exsr      Write_S0
045300070305     C*
045400110826     C                   Z-ADD     S0NRR         WMAX                 30
045500070305     C*
045600110826     C* POSIZIONAMENTO AL 0° RCD DELLA PAGINA
045700110826     C     S0NRR         DIV       S0pag         PAGINE            4 0
045800070305     C                   MVR                     RESTO             3 0
045900110826     C     PAGINE        MULT      S0pag         C0RCD
0460000703051    C     RESTO         IFGT      0
046100110826     C                   ADD       1             C0RCD
0462000703051E   C                   ELSE
046300110826     C                   SUB       S0pag         C0RCD
046400110826     C                   ADD       1             C0RCD
0465000703051-   C                   ENDIF
046600070305     C*
046700070305     C                   ENDSR
046800070301      *---------------------------------------------------------------*
046900070320      *?  Scrive x Ogni rottura di GIRO                              ?
047000070301      *---------------------------------------------------------------*
047100110826     C     Write_S0      BEGSR
047200070305      *
047300110826      *  Precarica da Ordine Pianificato
047400110826     c                   exsr      Legge_rec0
047500070228      *
047600070228      * Finchè il giro è lo stesso
0477001108311    C     $EFILE        DowEQ     *OFF
047800110826     C                   clear                   TII5S00
047900070228      *
048000110830      * Solo x ordini mensili deve impostare le sottoclassi
048100110830      *  degli articoli pianificati preselezionate con "1" e proteggere
048200110830      *   altre scelte.
048300110826     c                   exsr      carica_campi0
048400070305      *---------
048500110826     C                   ADD       1             S0NRR
048600070305     C                   ADD       1             Y
048700110826     C                   WRITE     TII5S00
048800070305      *---------
048900110826     c                   exsr      Legge_rec0
049000110825     C*
049100070228     c                   EndDO
049200070228     C*
049300070228     C                   ENDSR
049400110825      *---------------------------------------------------------------*
049500110825      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
049600110825      *---------------------------------------------------------------*
049700110826     C     LEGGE_REC0    BEGSR
049800110825     C*
049900110825     C*  Per EoF deve uscire
050000110825     C*  Per Record Ok deve uscire riportando i dati
050100110825     C                   MOVEL     *OFF          $EFILE
050200110825     C                   MOVEL     *OFF          $RCDOK
050300110825     C*
0504001108251    C     $EFILE        DOUEQ     *ON
050500110825     C     $RCDOK        OREQ      *ON
050600110825     C*
050700110826      * ? Se Ordine Pianificato deve caricare da file precaricato
050800110825     c****   LETTURA RECORD x Filiale
050900110915     c                   read      tmORP01L
051000110825      * Fine File
051100110915     C                   IF        %Eof(tmORP01L)
051200110825     C                   MOVEL     *ON           $EFILE
051300110915     C                   MOVE      *ON           *IN33
051400110825     C                   Else
051500110825      * ?Controlla se la Spedizione è da considerare
051600110825     C                   MOVEL     *ON           $RCDOK
051700110826      *
051800110826     C                   exsr      Check_RECord0
051900110825     C                   END
052000110825     C*
052100110915     C* va al nuovo ordine  se ha scartato il precedente
052200110915     c     kTmORP        setgt     tmORP01L
052300110915     C*
0524001108251-   C                   ENDDO
052500110825     C*
052600110825     C                   ENDSR
052700110825      *---------------------------------------------------------------*
052800110825      *?   CONTROLLO il record letto se va bene                      ?
052900110825      *---------------------------------------------------------------*
053000110826     C     Check_Record0 BEGSR
053100110825     C*
053200110915     C*  Controlla il tipo di ordine
053300110915     c                   if        D1TORP <> Tutto and
053400110915     c                             D1TORP <> orpPIAN
053500110830     C                   MOVEL     *OFF          $RCDOK
053600110830     C                   END
053700110825     C*
053800110915     C*  Controlla chi ha fatto l'ordine
053900110915     c                   if        D1uffC <> Tutto and
054000110915     c                             D1uffC <> orpUFF
054100110915     C                   MOVEL     *OFF          $RCDOK
054200110915     C                   END
054300110915     C*
054400110915     C*  se non è stato confermato o è stato sospeso
054500111019     c                   if        orpCONF <> '1'     or
054600110915     c                             orpATT  <> *blank
054700110915     C                   MOVEL     *OFF          $RCDOK
054800110915     C                   END
054900110915     C*
055000110915     C*  Se richiesta una filiale occorre verificare che il record letto
055100110915     C*   non sia già sulla filiale successiva
055200110915     C*  Come se fosse arrivato a Fine File
055300110915     c                   if        D1cfgs  > 0     and
055400110915     c                             D1cfgs <  orpFIL
055500110915     C                   MOVEL     *ON           $EFILE
055600110915     C                   MOVE      *ON           *IN33
055700110915     C                   END
055800110915     C*
055900110901     C*
056000110826     C     end_Check0    ENDSR
056100070301      *---------------------------------------------------------------*
056200110915      *?  Carica gli Ordini da Confermare ordinati per Filiale       ?
056300070301      *---------------------------------------------------------------*
056400110826     C     Carica_campi0 BEGSR
056500110726     C*
056600110915     c                   move      *blank        s0sce
056700110915     c                   eval      s0NOR    =  orpNOR
056800110915     c                   eval      h0DOR    =  orpDOR
056900110916     C*
057000110916     C                   Z-ADD     h0DOR         G02INV                         *AAAA/MM/GG
057100110916     C                   MOVEL     '3'           G02ERR
057200110916     C                   CALL      'XSRDA8'
057300110916     C                   PARM                    WLBDAT
057400110916     c                   Z-ADD     G02DAT        Data_Ordine
057500110916     c                   Z-ADD     G02DAT        s0DOR
057600110916      **
057700110915     c                   eval      s0PIAN   =  orpPIAN
057800110915     c                   eval      s0FIL    =  orpFIL
057900110830     C*
058000110726     C                   ENDSR
058100110726      *---------------------------------------------------------------*
058200110726      *?   CONTROLLO TESTATA LISTA                                   ?
058300110726      *---------------------------------------------------------------*
058400110826     C     CTRC0         BEGSR
058500110726     C*
058600070301     C                   MOVE      *OFF          *IN99
058700110726     C*
058800070301     C                   ENDSR
058900070301      *---------------------------------------------------------------*
059000070320      *?   CONTROLLO OPZIONI LISTA                                   ?
059100070301      *---------------------------------------------------------------*
059200110826     C     CTRS0         BEGSR
059300070301     C*
059400070301     C                   MOVEL     *OFF          $ESCI
059500110830     c                   clear                   ERRmsg80
059600070301     C                   SETOFF                                       99
059700070301     C*
059800070301     C* Leggo il sfl solo se ci sono rcd
0599000703011    C     WMAX          IFGT      0
060000110826     C                   READC     TII5S00                                21
060100070301     C*
060200070301     C* esce se fine sfl o errore che richiede l'uscita
0603000703012    C     *IN21         DOWEQ     *OFF
060400070301     C     $ESCI         ANDEQ     *OFF
060500110826     C                   Z-ADD     S0NRR         C0RCD
060600070301     C* ctrl su riga
060700110826     C                   EXSR      RECS0
060800070301      *
060900070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0610000703013    C     *IN99         IFEQ      *ON
061100070301     C     $ESCI         OREQ      *ON
061200070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
061300070301     C* la ripristinerò a conclusione del ciclo di READC
061400110826     C                   MOVE      *OFF          $INZS0
0615000703013-   C                   ENDIF
061600070301     C*
061700070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
061800070301     C                   MOVE      *ON           *IN32
061900110826     C                   UPDATE    TII5S00
062000070301     C*
062100070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0622000703013    C     $ESCI         IFEQ      *OFF
062300110826     C                   READC     TII5S00                                21
062400070301      *
062500070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0626000703014    C     *IN21         IFEQ      *ON
062700070301     C* calcolo pagina a cui deve posizionarsi
062800110826     c                   z-add     s0pag         Wpagine
062900070301     C                   EXSR      CLCPAG
0630000703014-   C                   ENDIF
0631000703013-   C                   ENDIF
063200070301     C*
0633000703012-   C                   ENDDO
063400070301     C*
0635000703011-   C                   ENDIF
063600110726     C* __________________________________________________
063700110826     C*  errori da evidenziare tramite finestra di ALLERTA
063800110826     c                   exsr      Errori
063900110726     C* __________________________________________________
064000070328     C*
064100070301     C                   ENDSR
064200070301     C/EJECT
064300070301      *---------------------------------------------------------------*
064400070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
064500070301      *---------------------------------------------------------------*
064600110826     C     RECS0         BEGSR
064700070301     C*
064800070301     C* reset indicatori DSPATR
064900070308     c                   exsr      INZ_indERR
065000070326     C*
065100110830     c                   setON                                        32
065200110826     C* gestione opzioni
0653001108263    C     S0sce         IFNE      *blank
065400110826     C     *IN99         ANDEQ     *OFF
065500110826      *
065600110826     C                   EXSR      SCE_S0
065700110915      *
065800110830     c                   setOFF                                       32
065900110830     c                   clear                   s0sce
066000110826      *
0661001108263-   C                   ENDIF
066200110826     C*
066300110826     C     endSF0        ENDSR
066400110826      *---------------------------------------------------------------*
066500110826      *?   GESTIONE OPZIONI LISTA                                    ?
066600110826      *---------------------------------------------------------------*
066700110826     C     SCE_S0        BEGSR
066800110826     C*
066900110919     C*  Selezionata la riga
067000110919     c                   If        s0SCE <> *blank
067100110916     c                   eval        num_ordine  = s0NOR
067200110916     c                   eval        Data_Ordine = s0dor
067300110916     c                   eval        filiale    = s0fil
067400110919     c                   exsr      Chiama_Ordine
067500110915     c                   endIf
067600110826     C*
067700110919     c                   eval        s0sce = *blank
067800110919     C*
067900110826     C                   ENDSR
068000110826     C/EJECT
068100110826      *---------------------------------------------------------------*
068200110826      *?  GESTIONE F03 x uscire LR                                   ?
068300110826      *---------------------------------------------------------------*
068400110826     C     F03_exit      BEGSR
068500110909      **
068600110906     C                   MOVE      *ON           $FINE
068700110826      *
068800110826     C                   ENDSR
068900110826     C/EJECT
069000110825      *---------------------------------------------------------------*
069100110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
069200110825      *---------------------------------------------------------------*
069300110825     C     CLCPAG        BEGSR
069400110825     C* Input :
069500110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
069600110825     C*          cursore
069700110825     C* - Wpagine= numero rcd per pagina sfl
069800110825     C* Output :
069900110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
070000110825     C*
070100110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
070200110825     C                   MVR                     RESTO             3 0
070300110825     C     RESTO         IFGT      0
070400110825     C                   ADD       1             PAGINE
070500110825     C                   ENDIF
070600110825     C     PAGINE        MULT      Wpagine       WPAG
070700110825     C*
070800110825     C                   ENDSR
070900110825     C************************************************************
071000110825      *?  INIZIALIZZA indicatori di errore                           ?
071100110825     C************************************************************
071200110825     C     INZ_indERR    BEGSR
071300110825      *
071400110825     C                   move      *ALL'0'       IN5190           40
071500110825     C                   moveA     IN5190        *IN(51)
071600110825      *
071700110825     C                   ENDSR
071800070301     C/EJECT
071900070301      *---------------------------------------------------------------*
072000070316      *  ?  OPERAZIONI INIZIALI      ?
072100070301      *---------------------------------------------------------------*
072200940131     C     *INZSR        BEGSR
072300110726     C*
072400110908     C*  chiavi x files.
072500110908     C*
072600110906     c     kTmORP        klist
072700110824     C                   KFLD                    orpFIL
072800110905     C                   KFLD                    orpUFF
072900110824     C                   KFLD                    orpANNO
073000110824     C                   KFLD                    orpWEEK
073100110905     C                   KFLD                    orpPIAN
073200110915     C                   KFLD                    orpNOR
073300110830     C*
073400030113     C* Reperimento parametri
073500030113     C     *ENTRY        PLIST
073600030113     C                   PARM                    KPJBA
073700110726      *  Se è EDP
073800070308     c                   CLEAR                   se_EDP            1
073900070308     c                   if        %subst(Knmus:1:3) = 'EDP'
074000070308     c                   MOVE      'S'           se_EDP
074100070307     c                   end
074200070305     C*
074300070227      * Reperisco dati job
074400070227     c                   exsr      DatiJob
074500110901     C*  Controlla il P.O.
074600110901     C     d1cFGS        CHAIN     AZORG01l
074700110901      * Non presente in organigramma
074800110901    1C                   IF        %found(azorg01l)
074900110901     c                   eval      og143 = orgDE3
075000110901     c                   if        §ogNTW = 'LOG'
075100110901     c                   end
075200110901    1C                   ENDIF
075300110901     C*
075400030113     C* Variabili per gestione videate
075500030113     C                   MOVE      *BLANK        $GEST             2
075600070305     C                   MOVE      *BLANK        sav$GEST          2
075700110906     C                   MOVE      *off          $FINE             1
075800110906     C                   MOVE      *off          dopo_in_$FINE     1
075900110906     C                   MOVE      *off          $INZD1            1
076000110906     C                   MOVE      *off          $INZS0            1
076100110906     C                   MOVE      *off          $EFILE            1
076200110906     C                   MOVE      *off          $ESCI             1
076300110906     C                   MOVE      *off          $RCDOK            1
076400030113     C* Indici
076500030113     C                   Z-ADD     0             X                 3 0
076600030113     C                   Z-ADD     0             Y                 3 0
076700070227     C***
076800070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
076900070227     C                   TIME                    W0140            14 0
077000110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
077100110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
077200070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
077300070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
077400070227     C*
077500110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
077600070227     C                   MOVEL     *BLANK        G02ERR
077700070227     C                   CALL      'XSRDA8'
077800070227     C                   PARM                    WLBDAT
077900110825     c                   Z-ADD     G02INV        WoggiAMG          8 0          *AAAA/MM/GG
078000110825     c                   eval      dataiso = %date(WoggiAMG:*iso)
078100110824     c                   eval      dataeur = dataiso
078200110825     c                   eval      oggiiso = dataiso
078300110824     c                   clear                   settimana         2 0
078400110824     C*
078500110824     C* NR.Settimana in corso
078600110824     C/EXEC SQL
078700110831     C+ SELECT week_iso(:OGGIISO) INTO :settimana FROM tmAMP00F
078800110824     C/END-EXEC
078900100505     C*
079000940117     C                   ENDSR
079100070227      *---------------------------------------------------------------*
079200070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
079300070227      *---------------------------------------------------------------*
079400070228     c     DatiJOB       BEGSR
079500070227      *
079600070227     c     *dtaara       define    §azute        azuteds
079700070227     c     *dtaara       define    §datiute      ddatiute
079800070227      *
079900070227     c                   in(E)     *dtaara
080000070227     c                   IF        %ERROR or RSUT = *blanks
080100070227     c                   clear                   Tibs34Ds
080200070227     c                   call      'TIBS34R'
080300070227     c                   parm                    Tibs34Ds
080400070227     c                   in        *dtaara
080500070227     c                   ENDIF
080600080606      *
080700070227     c                   ENDSR
080800110825     C/EJECT
080900110826      * ?-------------------------------------------------------------*?
081000110826      *?   SUBROUTINE Autorizzazione                                 ?
081100110826      * ?-------------------------------------------------------------*?
081200110826     C     GESAU         BEGSR
081300110826      *
081400110826     C* Se sarà un profilo Abilitato, imposta l'Entrata dal Primo Video
081500110826     C                   MOVE      'D1'          $GEST
081600110826     C*
081700110826      * ?- Controlla autorizzazione sull'utente
081800110826     c                   movel     dutpou        i70pog
081900110826     C* Il p.o.deve essere impostato in base al profilo
082000110826     C     dutlpo        IFEQ      '2'
082100110826     C                   MOVEL     dutpou        i70pge                         *FIL.GESTIONE
082200110826     C                   ELSE
082300110826     C                   MOVEL     SIMFEL        i70pge
082400110826     C                   END
082500110826     c                   movel     knmus         i70prf
082600110826     c                   call      'FICN70R'
082700110826     c                   parm                    ficn70ds
082800110826      *
082900110826     c                   movel     o70uni        daut
083000110826      *
083100110916     c* utente NON abilitato alla gestione magazzino OPPURE IN Filiale
083200110921     c                   if        §autmag <>'S'
083300110826      *
083400110906     c                   eval      Err001 = ' '
083500110906     c                   eval      Err002 = ErrMsg(3)
083600110826     c                   eval      Err003 = ' '
083700110826      *   Messaggio errore a Finestra
083800110906     C                   MOVE      *on           dopo_in_$FINE
083900110826     C                   MOVE      'W1'          $GEST
084000110826      *
084100110826     c                   end
084200110826     c*
084300110826     C     endAU         ENDSR
084400110906     C************************************************************
084500110906      *?  Emissione Finestra Errori                              ?
084600110906     C************************************************************
084700110906     C     GESW1         BEGSR
084800110906     C*
084900110826     C                   eval      $FINE = dopo_in_$FINE
085000110826     C                   eval      $GEST = sav$GEST
085100110906     c                   eval      dopo_alert_window    = *blank
085200110826     c*
085300110826     c                   exfmt     TII5WER
085400110906     c*
085500110906     c                   eval         Err001 = *blank
085600110906     c                   eval         Err002 = *blank
085700110906     c                   eval         Err003 = *blank
085800110906     c                   eval       dopo_alert_window    ='S'
085900110906     c*
086000110906     C* F3=Fine - Uscita Forzata
086100110906     C                   if        $TASTO = F03 and $FINE = *off
086200110906     C                   exsr      F03_exit
086300110906     c                   endIF
086400110826     c*
086500110826     c* emesso l'errore torna al video che lo aveva generato
086600110826     c*  oppure va fuori dal programma  in base a $FINE e $GEST
086700110826     c*
086800110826     C                   ENDSR
086900110916      * ?-------------------------------------------------------------*?
087000110916     C*   Routine di Revisione dell'ordine
087100110916      * ?-------------------------------------------------------------*?
087200110919     C     Chiama_Ordine BEGSR
087300110916     C*
087400110916     c                   eval      kpjbus =  kpjbu
087500110919     c                   clear                   tii506ds
087600110919     c***
087700110919     c                   eval      I506CONFER  = 'C'
087800110919     c***
087900110916     c                   eval      I506FILIAL  = Filiale
088000110916     c                   eval      I506NUMORD  = num_ordine
088100110916     c                   eval      I506DATAOR  = Data_Ordine
088200110916     c                   eval      I506WEEK    =settimana
088300110916     c                   eval      I506ANNO    =anno
088400110926     c                   eval      I506PIAN    = s0pian
088500110916     c                   eval      kpjbu  =  tii506ds
088600110916     c                   call      'TII506R'
088700110916     c                   parm                    kpjba
088800110916     c                   eval      tii506ds = kpjbu
088900110916     c                   eval      kpjbu  =  kpjbus
089000110916     c*
089100110916     C                   ENDSR
089200110916      * ?-------------------------------------------------------------*?
089300110916     C*   Routine degli errori da visualizzare sui SFL
089400110916      * ?-------------------------------------------------------------*?
089500110916     C     Errori        BEGSR
089600110916     C*
089700110906     C*  Messaggio di avviso    SE deve emettere un messaggio
089800110906     c                   if        err001 <> *blank or
089900110906     c                             err002 <> *blank or
090000110906     c                             err003 <> *blank
090100110826     C*
090200110826     c                   seton                                        99
090300110826      * ?            *-----------------------*
090400110826     C                   exfmt     TII5WER
090500110826      * ?            *-----------------------*
090600110826     C*
090700110909     c                   clear                   err001
090800110909     c                   clear                   err002
090900110909     c                   clear                   err003
091000110909     C*
091100110826     C*  Imposta per il dopo emissione errore
091200110826     C                   eval      $FINE = dopo_in_$FINE
091300110826     C                   eval      $GEST = sav$GEST
091400110906     C*
091500110906     c                   end
091600110826     C*
091700110826     c                   ENDSR
091800110826      *---------------------------------------------------------------------------------
091900080609      *
092000070326** ErrMsg
092100110914L'interrogazione Ordini è possibile nell'ambito di una filiale.                01
092200110914Filiale errata o inesistente.                                                  02
092300110901L'Utente non è autorizzato alla procedura di Filiale.                          03
092400110726Scelta obbligatoria                                                            04
092500110916                                                                               05
092600110916                                                                               06
092700110902L' Articolo non è presente in anagrafica                                       07
092800110902L' Articolo non appartiene all'ufficio di competenza                           08
092900110916                                                                               09
093000110914L'Ordine non è disponibile in quanto NON confermato.                           10
093100110916                                                                               11
093200110916                                                                               12
093300110916                                                                               13
093400110916                                                                               14
093500110916                                                                               15
093600110725                                                                               16
093700110725                                                                               17
093800110725                                                                               18
093900110726                                                                               19
094000110726                                                                               20
094100110726                                                                               21
094200110726                                                                               22
094300110726                                                                               23
094400110726                                                                               24
094500110726                                                                               25
094600110726                                                                               26
094700110726                                                                               27
094800110726                                                                               28
094900110726                                                                               29
095000110726                                                                               30
095100110726                                                                               31
095200110725                                                                               32
095300110726                                                                               33
095400110725                                                                               34
095500070829                                                                               35
095600070829                                                                               36
095700070829                                                                               37
095800070829                                                                               38
095900070829                                                                               39
096000110726Il valore immesso per il campo non è valido.                                   40
