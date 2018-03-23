000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400080819      * Fidg311R *---------------------------------------------------*
000500080820      *     ?   - DETTAGLIO Distinta - CONSEGNE/RITIRI     ?
000600070403      *--------------------------------------------------------------*
000700940224      *
000800940307      *  21           GENERICO OPERAZIONI I/O
000900940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001000070301      *  23           GENERICO Operazioni varie
001100940224      *  30           SFLDSP
001200940224      * N31           SFLCLR
001300940224      *  31           SFLDSPCTL
001400940224      *  32           SFLNXTCHG
001500940224      *  33           SFLEND
001600940224      *  39           OF PRTF
001700070302      *  40 <---> 49  DSPATR SU video
001800940608      *  Specificare l'uso dei singoli indicatori
001900070227      *  50           Emissione della window totale/parziale
002000070227      *  51 <---> 90  ERRORI SU VIDEO
002100940608      *  Specificare l'uso dei singoli indicatori
002200940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002300070829      *  98           ERRORE SPECIALE :
002400940224      *  99           INDIC. GENERALE DI ERRORE
002500940128     F*----------------------------------------------------*
002600080820     Ffnarb70l  iF   E           K DISK
002700080826     Ffnorm06l  iF   E           K DISK
002800080826     Ffnorg01l  iF   E           K DISK
002900080901      *
003000080901     Ffidst01l  iF   E           K DISK
003100080901     Ffiapd01l  iF   E           K DISK
003200070227      *
003300080819     Ffidg311d  CF   E             WORKSTN
003400070227     F                                     SFILE(FDG0S02:S2NRR)
003500940201     F                                     INFDS(DSFMT)
003600070305     D*----------------------------------------------------*
003700080819     D Errmsg          S             78    DIM(10) CTDATA PERRCD(1)             MSG DI ERRORE
003800070326     D*----------------------------------------------------*
003900080819     D fidg311ds     E DS
004000080819
004100940211     D* Passaggio Parametri
004200940211     D KPJBA         E DS
004300070227     D KPJBUs          s                   like(KPJBU)
004400940211     D*-------------
004500940211     D DSFMT           DS           512
004600940506     D  $TASTO               369    369
004700940211     D  NRG                  370    370
004800940211     D  NCL                  371    371
004900940211     D  SFLNRR               378    379B 0
005000940127     D*-------------
005100940127     D* Reperimento nome PGM
005200940127     D STATUS         SDS           333
005300940127     D  DSPGM            *PROC
005400030113     D*-------------
005500070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
005600080819     D S2NRR           S                   Like(C2rcd)
005700080819     D S2pag           S                   Like(C2rcd)
005800080819     D WSfl            S                   Like(C2nrr)
005900080819     D Wmax            S                   Like(C2rcd)
006000080819     D Wpag            S                   Like(C2rcd)
006100080819     D Wpagine         S                   Like(C2rcd)
006200080819     D Wrigsfl2        S              3  0 inz(16)
006300080820     D Otto_righe      S              3  0 INZ(8)
006400080901     D dst_Distinta    S                   Like(dstNFV)
006500070320      *--------------------------
006600070404     d wfgs_ifp        s                   like(ARBifp)
006700080820      *--------------------------
006800080820     d stringasql      s           1100    varying
006900080820     d Filiale...
007000080820     d                 s              3s 0
007100080820     d Num_Distinta...
007200080820     d                 s              7s 0
007300080820     d Data_Distinta...
007400080820     d                 s              8s 0
007500080820     d Dati_da_Visual...
007600080820     d                 s              1a
007700080820     d Priorita_dati...
007800080820     d                 s              1a
007900080821     d Categ_Foglio...
008000080821     d                 s              1s 0
008100080820      *
008200080820     D*VISUALIZZAZIONE ORM - FIOR07R
008300080820     D Parm01          DS
008400080820     D  ppoe                          3  0
008500080820     D  pnor                          7  0
008600080820     D  pnsr                          2  0
008700080820     D  pnrv                          2  0
008800080820     D  psce                          1
008900080820     D  pfgs                          3  0
009000080820     D  ppor                          3  0
009100080820     D  pdtr                          8  0
009200080820     D  pmdb                         10
009300080820     D  pprb                         10
009400080820     D  pdts                          8  0
009500080820     D  prmp                          1
009600080820     D  pbrc                          1
009700080820     D  pref                          2
009800080820     D  pmio                          1
009900080820
010000070228      *--------------------------
010100070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010200070305     D WLBDAT          DS                  INZ
010300070305     D  G02DAT                 1      8  0
010400070227     D  G02INV                 9     16  0
010500070227     D  G02ERR                17     17
010600070227     D  G02TGI                18     22  0
010700070307      *
010800070307      *--------------------------------
010900070307      * DS per richiamo pgm FNLR36R
011000070307     D DSLR36          DS
011100070307     D  P36AAS                14     17  0
011200070307     D  P36LNP                18     20  0
011300070307     D  P36NRS                21     22  0
011400070307     D  P36NSP                23     29  0
011500070307     D  P36F03                30     30
011600070307     D  P36FLG                31     31
011700080819
011800070320      *--------------------------
011900070227     d AZUTEds       e ds                  extname(AZUTE00F)
012000070227     d DDatiUte      e ds
012100080819
012200070320      *--------------------------
012300070320     d* Parametri x Controllo profilo utenti
012400070320     d TIBS34ds      e ds
012500080819
012600070227      *--------------------------
012700080821     D* DS FAsi passate da PGM chiamanti
012800080821     D FIDG35DS      E DS
012900080821
013000080821     D*------------
013100070302     D* DS PER - calcolo pesi volumi colli
013200070302     D fidg27ds      E DS
013300080820
013400080923     D*------------
013500080923     D* DS  Data Consegna Richiesta
013600080923     D fnlv80ds      E DS
013700080923
013800071008      *--------------------------
013900070228     D fnarbds       e ds                  extname(fnarb00f)
014000080820     d fnorgds       e ds                  extname(fnorg00f)
014100080820     d fnormds       e ds                  extname(fnorm00f)
014200080820
014300070227      *--------------------------
014400070320      *?  COSTANTI                                      ?
014500070320      *--------------------------
014600940506     D* Tasti di funzione
014700940506     D F01             C                   CONST(X'31')
014800940506     D F02             C                   CONST(X'32')
014900940506     D F03             C                   CONST(X'33')
015000940506     D F04             C                   CONST(X'34')
015100940506     D F05             C                   CONST(X'35')
015200940506     D F06             C                   CONST(X'36')
015300940506     D F07             C                   CONST(X'37')
015400940506     D F08             C                   CONST(X'38')
015500940506     D F09             C                   CONST(X'39')
015600940506     D F10             C                   CONST(X'3A')
015700940506     D F11             C                   CONST(X'3B')
015800070315     D F12             C                   CONST(X'3C')
015900940506     D F13             C                   CONST(X'B1')
016000940506     D F14             C                   CONST(X'B2')
016100940506     D F15             C                   CONST(X'B3')
016200940506     D F16             C                   CONST(X'B4')
016300940506     D F17             C                   CONST(X'B5')
016400940506     D F18             C                   CONST(X'B6')
016500940506     D F19             C                   CONST(X'B7')
016600940506     D F20             C                   CONST(X'B8')
016700940506     D F21             C                   CONST(X'B9')
016800940506     D F22             C                   CONST(X'BA')
016900940506     D F23             C                   CONST(X'BB')
017000940506     D F24             C                   CONST(X'BC')
017100940506     D ENTER           C                   CONST(X'F1')
017200940506     D ROLDWN          C                   CONST(X'F4')
017300940506     D ROLLUP          C                   CONST(X'F5')
017400070301     d digits          c                   '0123456789'
017500070419
017600070419      **********************************************************************
017700070419      * Constants
017800070419      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
017900070419      **********************************************************************
018000080820$xxx D Ord_XXX         C                   1
018100070427$xxx D Ord_DESTINAT    C                   2
018200070427$xxx D Ord_INDIRIZZO   C                   3
018300070427$xxx D Ord_LOCALITA    C                   4
018400070427$xxx D Ord_VOLUME      C                   5
018500070427$xxx D Ord_PESO        C                   6
018600080820
018700070427      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
018800070427     D MaxKey          C                   9
018900070419     D Ascendente      C                   1
019000070419     D Discendente     C                   2
019100070419     D Carattere       C                   6
019200070419     D Put             C                   1
019300070419     D EndPut          C                   2
019400070419     D Get             C                   3
019500070419     D Numerico        C                   8
019600070419      **********************************************************************
019700070419      * Campi utili:
019800070419      *     RRN1       - Numero relativo di record del Subfile
019900070419      *     SizeList   - Dimensione della lista
020000070419      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
020100070419      **********************************************************************
020200070419     D Rrn1            S              5I 0
020300070419     D NotUsed         S             16A
020400070419     D ReturnSize      S              9B 0
020500070419     D SizeList        S              9B 0
020600070419     D RrnLast         S              5I 0
020700070427     D WrkSort         S              1  0 inz(0)
020800070427     D WindSort        S              3I 0 inz(0)
020900070419      **********************************************************************
021000070419      * Data Structures
021100070419      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
021200070419      *     QLGSCB     - The sort request block for the QLGSORT API
021300070419      *     QLGSCB00   - The sort request block for the QLGSRTIO API
021400070419      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
021500070419      *     QUSEC      - Error structure for the QLGSORT API
021600070419      **********************************************************************
021700070419     D SflRcd          DS
021800080819     D  S2CR
021900080819     D  S2DESM
022000080819     D  S2IND
022100080819     D  S2LOD
022200080819     D  S2PESO
022300080819     D  S2VOL
022400080819     D  S2BANC
022500080820     D  S2SPO
022600080819     D  S2COLL
022700080819     D  S2TSER
022800080819     D  S2BOLORM
022900080819     D  S2CMIT
023000080819     D  S2DCR
023100080819     D  S2HCR
023200070419     D  S2SCE
023300080820     D  h2aas
023400080820     D  h2lnp
023500080820     D  h2nrs
023600080820     D  h2nsp
023700080820     D  h2POE
023800080820     D  h2NOR
023900080820     D  h2NSR
024000080820     D  h2NRV
024100080923     D  S2when
024200070419     D  Selected                      1A
024300070419
024400070419      /COPY QSYSINC/QRPGLESRC,QLGSORT
024500070419     D QLGKL                         16    DIM(MaxKey)
024600070419     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
024700070419     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
024800070419     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
024900070419     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
025000070419
025100070419      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
025200070419      /COPY QSYSINC/QRPGLESRC,QUSEC
025300080820     C*----------------------------------------------------*
025400080820      *?     MAIN LINE PROGRAM     1 solo video            ?
025500080820     C*----------------------------------------------------*
025600080820     C* inizializzazione variabili
025700080820     C                   EXSR      INZVAR
025800080820     C*
025900080820     C     $FINE         DOWEQ     *OFF
026000080820     C*
026100080820     C* Lista di dettaglio delle Bolle x selezione e guida aggiornamento Data Base
026200080820     C     $GEST         CASEQ     'S2'          GESS2                          Lista delle Bolle
026300080820     C                   END
026400080820      *
026500080820     C                   END
026600080820     C* fine programma
026700080821     C                   eval      KPJBU = FIDG311ds
026800080820     C                   SETON                                            LR
026900080820     C/EJECT
027000080819      *---------------------------------------------------------------*
027100080819      *  ?  OPERAZIONI INIZIALI      ?
027200080819      *---------------------------------------------------------------*
027300080819     C     *INZSR        BEGSR
027400080819     C*
027500080819     C* Reperimento parametri
027600080819     C     *ENTRY        PLIST
027700080819     C                   PARM                    KPJBA
027800080819     C                   movel     KPJBU         FIDG311ds
027900080820      *
028000080820      *  Chiavi
028100080820      *
028200080820     c     karb70        klist
028300080820     c                   kfld                    wfgs_ifp
028400080820     c                   kfld                    Num_Distinta
028500080820      *
028600080826      *
028700080826     c     korm06        klist
028800080826     c                   kfld                    Filiale
028900080826     c                   kfld                    ORmNDC
029000080826      *
029100080826     c     korg01        klist
029200080826     c                   kfld                    ORmPOE
029300080826     c                   kfld                    ORmNSR
029400080826     c                   kfld                    ORmNOR
029500080826     c                   kfld                    ORmNRV
029600080901      *
029700080901     c     Kdst01        klist
029800080901     c                   kfld                    Categ_Foglio
029900080901     c                   kfld                    dst_Distinta
030000080901     c                   kfld                    filiale
030100080901      *
030200080901     c     Kapd01        klist
030300080901     c                   kfld                    tipo_AUT          1
030400080901     c                   kfld                    dstPDR
030500080901     c                   move      'A'           tipo_AUT
030600080826      *
030700080820      *  Imposta i Parametri
030800080820     c                   eval      Filiale        = F31FGS
030900080820     c                   eval      Priorita_Dati  = F31PriDati
031000080820     c                   eval      Num_Distinta   = F31NumDis
031100080820     c                   eval      Data_Distinta  = F31DatDis
031200080820     c                   eval      Dati_da_Visual = F31DatiVis
031300080821     c                   eval      Categ_Foglio   = F31Npg
031400080820      *
031500080820      * Se non richiesto esplicitamente i Ritiri impostare Consegne di Default
031600080820     c                   if        Priorita_Dati  <> 'R'
031700080820     c                   eval      Priorita_Dati  =  'C'
031800080820     c                   end
031900080819      *
032000080819      * Reperisco dati job
032100080819     c                   exsr      DatiJob
032200080819
032300080819     C* Variabili per gestione videate
032400080820     C                   MOVE      *BLANK        $ESCI             1
032500080819     C                   MOVE      *BLANK        $GEST             2
032600080819     C                   MOVE      *BLANK        $FINE             1
032700080819     C                   MOVE      *BLANK        $INZS2            1
032800080819     C                   MOVE      *BLANK        $EFILE            1
032900080820     C                   MOVE      *OFF          $RCDOK            1
033000080819     C* Indici
033100080819     C                   Z-ADD     0             X                 3 0
033200080819     C                   Z-ADD     0             Y                 3 0
033300080820      *
033400080819      * GIRO DATA DEL GIORNO: LA PRENDO DA TIME
033500080819     C                   TIME                    W0140            14 0
033600080819     C                   MOVE      W0140         UDATE8            8 0
033700080819     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
033800080819     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
033900080819     C*
034000080819     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
034100080819     C                   MOVEL     *BLANK        G02ERR
034200080819     C                   CALL      'XSRDA8'
034300080819     C                   PARM                    WLBDAT
034400080819     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
034500080819      *
034600080819     C                   ENDSR
034700080819     C/EJECT
034800030113     C************************************************************
034900070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
035000030113     C************************************************************
035100030113     C     INZVAR        BEGSR
035200030113     C*
035300030113     C* Variabili per gestione videate
035400080820     C                   MOVE      'S2'          $GEST
035500030113     C                   MOVE      *OFF          $FINE
035600030113     C                   MOVE      *OFF          $ESCI
035700080820     C                   MOVE      *OFF          $EFILE
035800030113     C                   MOVE      *OFF          $RCDOK
035900080819     C                   MOVE      *ON           $INZS2
036000030114     C                   Z-ADD     1             WPAG
036100030113     c*
036200030113     C                   ENDSR
036300070227     C/EJECT
036400070301      *---------------------------------------------------------------*
036500070316      *  ?  GESTIONE SFL Bolle da selezionare                        ?
036600070301      *---------------------------------------------------------------*
036700070301     C     GESS2         BEGSR
036800070301     C*
036900070301     C* inizializzazione videata
037000070301     C     $INZS2        IFEQ      *ON
037100070301     C                   EXSR      INZS2
037200070301     C                   MOVE      *OFF          $INZS2
037300070301     C                   ENDIF
037400070306     C*
037500070306     C* Inizializza e imposta piede videata
037600070306     C                   EXSR      INZZ2
037700070305     C*
037800070301     C* emissione piede videata
037900070302      * ?            *-----------------------*
038000070301     C                   WRITE     FDG0Z02
038100070302      * ?            *-----------------------*
038200070301     C*
038300070301     C     WMAX          IFEQ      0
038400080820     C*
038500080820     C* Non ci sono records  Videata di Avviso
038600070302      * ?            *-----------------------*
038700070301     C                   WRITE     FDG0V02
038800070302      * ?            *-----------------------*
038900070301     c                   setoff                                       30
039000080820      *
039100080820     C                   ELSE
039200080820      *
039300070301     c                   seton                                        30
039400070301      *
039500080820      * Reimposta il posizionamento della pagina
039600080820     c                   exsr      Posiz_Pagina
039700080820      *
039800070301     C                   ENDIF
039900070301      *
040000070301      *  Emissione SFL delle BOLLE
040100080820      * ?            *************************
040200070301     C                   EXFMT     FDG0C02
040300080820      * ?            *************************
040400070326     c                   setoff                                       99
040500070301      *
040600070301     C     C2NRR         IFNE      0
040700070301     C                   Z-ADD     C2NRR         WSFL
040800070301     C                   ENDIF
040900070301     C                   Z-ADD     SFLNRR        C2RCD
041000070301     C* Selezioni
0411000703011    C                   SELECT
041200070301     C* F3=Fine
041300070301     C     $TASTO        WHENEQ    F03
041400080821     c                   move      '03'          F31TFunZ
041500070301     C                   EXSR      F03_exit
041600070307      *
041700080819     C* F4=Giri Ass.
041800080819     C     $TASTO        WHENEQ    F04
041900080819     C                   EXSR      F04_GiriAss
042000070426      *
042100070427     C* F07= Ordinamento x Peso
042200070427     C     $TASTO        WHENEQ    F07
042300070427     c                   z-add     6             WrkSort
042400070427     C                   EXSR      Ordina_SFL2
042500070301      *
042600070427     C* F08= Ordinamento x Volume
042700070427     C     $TASTO        WHENEQ    F08
042800070427     c                   z-add     5             WrkSort
042900070427     C                   EXSR      Ordina_SFL2
043000070427      *
043100070427     C* F10= Ordinamento x Località
043200070427     C     $TASTO        WHENEQ    F10
043300070427     c                   z-add     4             WrkSort
043400070427     C                   EXSR      Ordina_SFL2
043500070427      *
043600070427     C* F11= Ordinamento x Indirizzo
043700070427     C     $TASTO        WHENEQ    F11
043800070427     c                   z-add     3             WrkSort
043900070427     C                   EXSR      Ordina_SFL2
044000070427      *
044100080820     C* F13= Ordinamento x Destinatario / Mittente
044200070427     C     $TASTO        WHENEQ    F13
044300070427     c                   z-add     2             WrkSort
044400070427     C                   EXSR      Ordina_SFL2
044500070427      *
044600070301     C* F12=Ritorno
044700070301     C     $TASTO        WHENEQ    F12
044800080819     C                   EXSR      F03_exit
044900070301      *
0450000703011O   C                   OTHER
045100070301     C* CONTROLLO testata
045200070301     C                   EXSR      CTRC2
045300080820      *
045400070301     C* CONTROLLO subfile
045500070301     C     *IN99         IFEQ      *OFF
045600070301     C                   EXSR      CTRS2
045700070301     C                   END
045800070301      *
0459000703011-   C                   ENDSL
046000070301     C*
046100070301     C                   ENDSR
046200070306     C/EJECT
046300070306      *---------------------------------------------------------------*
046400080820      *?   Riposizionamento su Pagina SFL                            ?
046500070306      *---------------------------------------------------------------*
046600080820     C     Posiz_Pagina  BEGSR
046700070306     C*
046800080820     C     Wsfl          IFgt      0
046900080820     C*
047000080820      * essendo il posizionamento *TOP
047100080820      *  non è + corretto impostare direttamente il record
047200080820      *   ma occorre ricavare il primo record della pagina.
047300080820     c                   z-add     0             c2rcd
047400080820      *
047500080820     c                   if        wrigsfl2 > 0
047600080820     c     wsfl          div       wrigsfl2      C2RCD
047700080820     c                   mvr                     resto
047800080820     c                   if        c2rcd > 0
047900080820     c                   mult      wrigsfl2      C2RCD
048000080820     c                   if        resto > 0
048100080820     c                   add       1             C2RCD
048200080820     c                   end
048300080820     c                   else
048400080820     c                   z-add     1             C2RCD
048500080820     c                   end
048600080820     c                   end
048700080820      *
048800080820      * attenzione all'incremento di 1 può far schiantare il sfl
048900080820      * poichè può superare il nr.max delle righe presenti.
049000080820      * esempio se wsfl = 16 o un suo multiplo essendo wrigsfl2 = 16
049100080820      * c2rcd alla fine equivale a wsfl ossia 16 ....e se in seguito
049200080820      * si aggiunge 1 al sflrcdnbr andiamo fuori limite max record
049300080820      * caricati e si schianta in emissione del sfl.
049400080820     c                   if        resto = 0
049500080820     c                   eval      C2RCD = c2rcd - (wrigsfl2 - 1)
049600080820     c                   else
049700080820     c                   if        c2rcd = 0
049800080820     c                   add       1             c2rcd
049900080820     c                   end
050000080820     c                   end
050100080820      *
050200080820     C                   Else
050300080820     C     Wpag          IFgt      0
050400080820     C                   Z-ADD     wpag          C2RCD
050500080820     C                   EndIF
050600080820     C                   EndIF
050700080820     C*
050800080820     C                   ENDSR
050900080820     C/EJECT
051000080820      *---------------------------------------------------------------*
051100080820      *?   INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL               ?
051200080820      *---------------------------------------------------------------*
051300080820     C     INZZ2         BEGSR
051400080820     C*
051500070306     C* Messaggio o tasti funzionali
051600070306     c                   clear                   z2dmsg
051700070328     c                   clear                   ErrmsgZ          80
051800070328      *
051900070328      * imposta eventuali errori
052000070328     c                   eval      z2dmsg = ErrMsgZ
052100080820      *
052200070307      * tasti funzionali se non è stato emesso alcun messaggio
052300070328     c                   if        ErrMsgZ = *blank
052400070307     c                   setoff                                       99
052500070427     c                   end
052600070426      *
052700070306     C                   ENDSR
052800070301     C/EJECT
052900070301      *---------------------------------------------------------------*
053000070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
053100070301      *---------------------------------------------------------------*
053200070301     C     INZS2         BEGSR
053300070301     C*
053400070301     C* reset indicatori DSPATR
053500070308     c                   exsr      INZ_indERR
053600070301     C* pulizia SFL
053700070301     C                   SETOFF                                         3031
053800070301     C                   WRITE     FDG0C02
053900070301     C                   SETON                                          31
054000070301     C*
054100080820     C* Righe x Pagina su SFL
054200080820     c                   z-add     Otto_righe    s2pag
054300070420      *
054400080820     C* Default di Ordinamento
054500080820     C                   EVAL      WrkSort  = Ord_DESTINAT
054600080820     c                   eval      wINDsort = Ord_DESTINAT
054700070301      *
054800070301     C* CARICAMENTO SFL totale
054900070301     C                   Z-ADD     0             S2NRR
055000070301     C                   Z-ADD     1             C2RCD
055100070301     C                   Z-ADD     0             WMAX
055200070302     C                   Z-ADD     0             Wsfl
055300070302     C* campi titolo
055400080820     C                   MOVEL     RSUT          c2crsu
055500070302     C                   MOVEL     dsPGM         c2CPGM
055600070302     C                   MOVEL     knsif         c2nsif
055700070302     C                   MOVEL     knmus         c2nmus
055800080820     C                   eval      c2ndc = Num_Distinta
055900080901      *
056000080901      * Decodifica l'Autista
056100080901     C                   eval      Dst_Distinta = Num_Distinta
056200080901     c     Kdst01        chain     fiDST01l
056300080901     c                   if        %Found(FiDST01L)
056400080901     c     Kapd01        chain     fiAPD01l
056500080901     c                   if        %Found(fiAPD01l)
056600080901     c                   eval      c2daut = APDRSC
056700080901     c                   end
056800080901     c                   end
056900080901      *
057000080820      * Se non richiesto esplicitamente i Ritiri impostare Consegne di Default
057100080820     c                   if        Priorita_Dati  <> 'R'
057200080820     c                   eval      c2P1 =  'C'
057300080820     c                   eval      c2P2 =  'R'
057400080820     c                   else
057500080820     c                   eval      c2P1 =  'R'
057600080820     c                   eval      c2P2 =  'C'
057700080820     c                   end
057800080820      *
057900080820      * Carica il SFL
058000070301     c                   EXSR      ROLS2
058100070301     C*
058200070301     C                   Z-ADD     1             WPAG
058300070301     C*
058400070301     C                   ENDSR
058500080819     C************************************************************
058600080819      *?  INIZIALIZZA indicatori di errore                           ?
058700080819      *---------------------------------------------------------------*
058800080819     C     INZ_indERR    BEGSR
058900080819      *
059000080819     C                   move      *ALL'0'       IN5190           40
059100080819     C                   moveA     IN5190        *IN(51)
059200080819      *
059300080819     C                   ENDSR
059400080819     C/EJECT
059500070301      *---------------------------------------------------------------*
059600080826      *?  CARICAMENTO PAGINA SFL                                  ?
059700070301      *---------------------------------------------------------------*
059800070301     C     ROLS2         BEGSR
059900070301     C*
060000070301     C                   SETOFF                                       32
060100080820     C*
060200070301     C                   Z-ADD     0             Y
060300070301     C                   Z-ADD     WMAX          S2NRR
060400070301     C*
060500080820      * ?      Carica x Numero Distinta
060600080820     c                   if        Priorita_Dati  <> 'R'
060700080821     C* Consegne + Ritiri
060800080820     c                   exsr      CARica_Bolle
060900080820     c                   exsr      CARica_ORM
061000080820     c                   else
061100080821     C* Ritiri + Consegne
061200080820     c                   exsr      CARica_ORM
061300080820     c                   exsr      CARica_Bolle
061400080820     c                   end
061500070301     C* Fine Caricamento
061600070301     C                   Z-ADD     S2NRR         WMAX                 30
061700070419     C*
061800070419     C* se ci sono almeno 2 records da ordinare fra loro
061900070419     c                   if        wmax > 1
062000070419      * ?   Esegue l'Ordinamento x Giro/Spedizione
062100070419     c                   exsr      Ordina_SFL2
062200070419     c                   end
062300070301     C*
062400070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
062500070301     C     S2NRR         DIV       S2pag         PAGINE            4 0
062600070301     C                   MVR                     RESTO             3 0
062700070301     C     PAGINE        MULT      S2pag         C2RCD
0628000703011    C     RESTO         IFGT      0
062900070301     C                   ADD       1             C2RCD
0630000703011E   C                   ELSE
063100070301     C                   SUB       S2pag         C2RCD
063200070301     C                   ADD       1             C2RCD
0633000703011-   C                   ENDIF
063400070301     C*
063500070301     C                   ENDSR
063600070302      *---------------------------------------------------------------*
063700080820      *  ?  Carica --> I RITIRI      x Numero Distinta                ?
063800080820      *---------------------------------------------------------------*
063900080820     C     CARica_ORM    BEGSR
064000080821
064100080821     C*   Se richiesta la visualizzazione dei Ritiri o di Entrambe
064200081111     c******             if        Dati_da_Visual = 'O' or
064300081111     c****                         Dati_da_Visual = 'E'
064400080826      *
064500080826      * leggo Ritiri x Filiale/Distinta
064600080826     c                   z-add     Num_Distinta  ORmNDC
064700080826     c     korm06        setll     fnorm06l
064800080826
064900080820      * leggo gli ORM
065000080820     c                   exsr      LEGGE_ORM
065100080820
065200080820     C* Leggo dal file per caricare la lista
0653000808201    C     $EFILE        DOWEQ     *OFF
065400080820     C                   clear                   FDG0S02
065500080820      *
065600080820     c                   exsr      RIGA_ORM
065700080820      *
065800080820     C                   ADD       1             S2NRR
065900080820     C                   EVAL      RrnLast = S2nrr
066000080820     C                   ADD       1             Y
066100080820     C                   WRITE     FDG0S02
066200080820     C*
066300080820      * Lettura di un record
066400080820     c                   exsr      LEGGE_ORM
0665000808201-   C                   ENDDO
066600080821      *
066700081111     c*******            end
066800080820     C*
066900080820     C                   ENDSR
067000080820      *------------------------------------------------------------------------*
067100080826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
067200080820      *------------------------------------------------------------------------*
067300080820     c     LEGGE_ORM     begsr
067400080820
067500080820     C*  Per EoF deve uscire
067600080820      * per rcd ok deve uscire riportando i dati
067700080820     C                   MOVEL     *OFF          $EFILE
067800080820     C                   MOVE      *OFF          $RCDOK
067900080820     C*
0680000808201    C     $EFILE        DOUEQ     *ON
068100080820     C     $RCDOK        OREQ      *ON
068200080820
068300080826     c     korm06        reade     fnORM06l
068400080826      * Fine File
068500080826     c                   if        %Eof(fnORM06l)
068600080826     C                   MOVEL     *ON           $EFILE
068700080826     C                   MOVE      $EFILE        *IN33
068800080826     c                   else
068900080826     c     korg01        chain     fnORG01l
069000080826     c                   if        not %found(fnORg01l)
069100080826     c                   clear                   fnORGds
069200080826     c                   end
069300080826     c                   end
069400080826      *
069500080820     c                   eval      $rcdok = *on
069600080820     c                   enddo
069700080820
069800080820     c                   endsr
069900080820
070000080821      *---------------------------------------------------------------*
070100080821      *?  Imposta Campi del 2°SFL x ORM                              ?
070200080821      *---------------------------------------------------------------*
070300080821     C     RIGA_ORM      BEGSR
070400080821     C*
070500080821     C* Dati Ritiro
070600080821      *  Salva la chiave anche per riposizionarsi in seguito
070700080821     c                   z-add     ormPOE        H2POE
070800080821     c                   z-add     ormNOR        H2NOR
070900080821     c                   z-add     ormNSR        H2NSR
071000080821     c                   z-add     ormNRV        H2NRV
071100080821      *  Ritiro
071200080821     c                   movel     'R'           S2cr
071300080821      *
071400080821      *  Riferimento ORM
071500080821     c                   eval        %subst(S2bolOrm:1:3)  = %editc(ormPOE:'X')
071600080821     c                   eval        %subst(S2bolOrm:5:2)  = %editc(ormNSR:'Z')
071700080821     c                   eval        %subst(S2bolOrm:8:7) = %char(ormNOR)
071800080821     c                   eval        %subst(S2bolOrm:16:2) = %editc(ormNRV:'Z')
071900080821      *
072000080821     c                   movel     ormRSR        S2desM
072100080821     c                   movel     ormINR        S2ind
072200080821     c                   movel     ormLOR        S2lod
072300080821     c                   z-add     ormBNC        S2Banc
072400080821     c                   if        ormSPI ='S'
072500080821     c                   move      'Sp'          S2Spo
072600080821     c                   end
072700080821     c                   movel     ormRSO        S2cMit
072800080821      *
072900080821      * Data Ora di ritiro
073000080821     C                   Z-ADD     ormDAR        G02inv
073100080821     C                   MOVEL     '3'           G02ERR
073200080821     C                   CALL      'XSRDA8'
073300080821     C                   PARM                    WLBDAT
073400080821     C                   Z-ADD     G02DAT        S2dcr
073500080821     c                   z-add     ormORR        S2hcr
073600080821      *
073700080821      *  Pesi Volumi Colli
073800080821     c                   z-add     ormNCL        S2coll
073900080821     c                   eval      S2vol  = orgvlm
074000080821      *
074100080821     c                   if        orgpkg > *zeros
074200080821     c                   if        orgpkg > 9999
074300080821     c                   eval      S2peso = 9999
074400080821     c                   else
074500080821     c                   eval      S2peso = %dech(orgpkg:5:0)
074600080821     c                   endif
074700080821     c                   if        S2peso = *zeros
074800080821     c                   eval      S2peso = 1
074900080821     c                   endif
075000080821     c                   else
075100080821     c                   clear                   S2peso
075200080821     c                   endif
075300080821      *
075400080821     C                   ENDSR
075500080826      *---------------------------------------------------------------*
075600080826      *  ?  Carica --> LE BOLLE      x Numero Distinta                ?
075700080826      *---------------------------------------------------------------*
075800080826     C     CARica_Bolle  BEGSR
075900080826     C*
076000080826     C*   Se richiesta la visualizzazione delle Consegne o di Entrambe
076100081111     c******             if        Dati_da_Visual = 'C' or
076200081111     c******                       Dati_da_Visual = 'E'
076300080826      *
076400080826      * leggo Bolle x Filiale/Distinta
076500080826     c                   z-add     Filiale       wfgs_ifp
076600080826     c     karb70        setll     fnarb70l
076700080826
076800080826      * ? ----------------------------
076900080826      * ? Carico il SFL
077000080826     c                   exsr      LEGGE_Bolle
077100080826     C*
077200080826     C* Leggo dal file per caricare la lista
0773000808261    C     $EFILE        DOWEQ     *OFF
077400080826     C                   clear                   FDG0S02
077500080826      *
077600080826     c                   exsr      RIGA_Bolla
077700080826      *
077800080826     C                   ADD       1             S2NRR
077900080826     C                   EVAL      RrnLast = S2nrr
078000080826     C                   ADD       1             Y
078100080826     C                   WRITE     FDG0S02
078200080826     C*
078300080826      * Lettura di un record
078400080826     c                   exsr      LEGGE_Bolle
0785000808261-   C                   ENDDO
078600080826      *
078700081111     c*******            end
078800080826     C*
078900080826     C                   ENDSR
079000080820      *---------------------------------------------------------------*
079100070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
079200070302      *---------------------------------------------------------------*
079300070302     C     LEGGE_Bolle   BEGSR
079400070302     C*
079500070302     C*  Per EoF deve uscire
079600070302     C                   MOVEL     *OFF          $EFILE
079700080820     C                   MOVE      *OFF          $RCDOK
079800070302     C*
0799000703021    C     $EFILE        DOUEQ     *ON
080000070302     C     $RCDOK        OREQ      *ON
080100080821      *
080200080821      * ?LETTURE  FILE
080300080821     C*
080400080821     c     karb70        reade     fnarb70l
080500080821      * Fine File
080600080820     c                   if        %Eof(fnarb70l)
080700080820     C                   MOVEL     *ON           $EFILE
080800080820     C                   MOVE      $EFILE        *IN33
080900070417     c                   end
081000080821      *
081100080820     C                   MOVE      *ON           $RCDOK
0812000703021-   C                   ENDDO
081300070302     C*
081400070302     C                   ENDSR
081500070301      *---------------------------------------------------------------*
081600080820      *?  Imposta Campi del 2°SFL x Bolla di consegna                ?
081700070301      *---------------------------------------------------------------*
081800080820     C     RIGA_Bolla    BEGSR
081900070301     C*
082000070301     C* Dati Bolla
082100070301      *  Salva la chiave anche per riposizionarsi in seguito
082200080819     c                   z-add     ARBaas        H2aas
082300080819     c                   z-add     ARBlnp        H2lnp
082400080819     c                   z-add     ARBnrs        H2nrs
082500080819     c                   z-add     ARBnsp        H2nsp
082600080820      *  Consegna
082700080820     c                   movel     'C'           S2cr
082800080820     c                   movel     Arbtsp        S2tSer
082900080820      *
083000080820      *  Riferimento Bolla
083100080820     c                   eval        %subst(S2bolOrm:1:4)  = %char(Arbaas)
083200080821     c                   eval        %subst(S2bolOrm:6:3)  = %editc(ArbLnp:'X')
083300080821     c                   eval        %subst(S2bolOrm:10:2) = %editc(ArbNrs:'Z')
083400080820     c                   eval        %subst(S2bolOrm:13:7) = %char(ArbNsp)
083500080820      *
083600080820     c                   movel     ARBrsD        S2desM
083700080819     c                   movel     ARBind        S2ind
083800080819     c                   movel     ARBlod        S2lod
083900080820     c                   movel     ARBrsM        S2cMit
084000080820      *
084100080820      * Data Ora richiesta consegna
084200080923     C*****              Z-ADD     ARBdcr        G02inv
084300080923     C*****              MOVEL     '3'           G02ERR
084400080923     C*****              CALL      'XSRDA8'
084500080923     C*****              PARM                    WLBDAT
084600080923     C*****              Z-ADD     G02DAT        S2dcr
084700080923     c*****              z-add     ARBhcr        S2hcr
084800080820      *
084900080923      *  Decodifica quando deve essere riferita la data di richiesta Consegna
085000080923     c                   if        arbdcr>0 or arbhcr>0
085100080923     c                   clear                   fnlv80ds
085200080923     c                   eval      ilv80aas=arbaas
085300080923     c                   eval      ilv80lnp=arblnp
085400080923     c                   eval      ilv80nrs=arbnrs
085500080923     c                   eval      ilv80nsp=arbnsp
085600080923     c                   eval      ilv80mgs=arbmgs
085700080923     c                   eval      ilv80tc1=arbtc1
085800080923     c                   eval      ilv80tc2=arbtc2
085900080923     c                   eval      ilv80dcr=arbdcr
086000080923     c                   eval      ilv80hcr=arbhcr
086100080923     c                   eval      ilv80tcr=arbtcr
086200080923     c                   call      'FNLV80R'
086300080923     c                   parm                    kpjba
086400080923     c                   parm                    fnlv80ds
086500080923     c
086600080923     c                   eval      s2dcr  = olv80dcr8
086700080923     c                   eval      s2when = olv80tcrd
086800080923     c                   z-add     ARBhcr        S2hcr
086900080923     c                   endif
087000080923     c
087100080923      *
087200080923      *
087300070302      *  Pesi Volumi Colli da consegnare
087400070305     c                   exsr      Cal_PesVol
087500080820      *
087600070302      *   i pesi/volumi/Colli Reali da consegnare
087700080819     c                   z-add     D27PKLDCO     S2peso
087800070502     c                   move      D27PKLDCO     Decimal_uno       1 0
087900070502     c                   if        Decimal_uno > 0
088000080819     c                   add       1             S2peso
088100070502     c                   end
088200080819     c                   z-add     D27VLUDCO     S2vol
088300080820     c                   z-add     D27NCLDCO     S2coll
088400070301      *
088500070301     C                   ENDSR
088600070301      *---------------------------------------------------------------*
088700070320      *?  CONTROLLO TESTATA LISTA                                    ?
088800070301      *---------------------------------------------------------------*
088900070301     C     CTRC2         BEGSR
089000070301     C                   MOVE      *OFF          *IN99
089100070301     C                   ENDSR
089200070514      *---------------------------------------------------------------*
089300070514      *?  CONTROLLO OPZIONI LISTA                                    ?
089400070514      *---------------------------------------------------------------*
089500070514     C     CTRS2         BEGSR
089600070301     C*
089700070301     C                   MOVEL     *OFF          $ESCI
089800070301     C                   SETOFF                                       99
089900070301     C*
090000070301     C* Leggo il sfl solo se ci sono rcd
0901000703011    C     WMAX          IFGT      0
090200070302     C                   READC     FDG0S02                                21
090300070301     C*
090400070301     C* esce se fine sfl o errore che richiede l'uscita
0905000703012    C     *IN21         DOWEQ     *OFF
090600070301     C     $ESCI         ANDEQ     *OFF
090700070302     C                   Z-ADD     S2NRR         C2RCD
090800070301     C* ctrl su riga
090900070302     C                   EXSR      RECS2
091000070301      *
091100070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0912000703013    C     *IN99         IFEQ      *ON
091300070301     C     $ESCI         OREQ      *ON
091400070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
091500070301     C* la ripristinerò a conclusione del ciclo di READC
091600070302     C                   MOVE      *OFF          $INZS2
0917000703013-   C                   ENDIF
091800070301     C*
091900070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
092000080820     C                   SetOFF                                       32
092100080819     c                   if        s2sce = '5'
092200070301     C                   MOVE      *ON           *IN32
092300070307     c                   end
092400080820      *
092500070302     C                   UPDATE    FDG0S02
092600070301     C*
092700070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0928000703013    C     $ESCI         IFEQ      *OFF
092900070302     C                   READC     FDG0S02                                21
093000070301      *
093100070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0932000703014    C     *IN21         IFEQ      *ON
093300070301     C* calcolo pagina a cui deve posizionarsi
093400070302     c                   z-add     s2pag         Wpagine
093500070301     C                   EXSR      CLCPAG
0936000703014-   C                   ENDIF
0937000703013-   C                   ENDIF
093800070301     C*
0939000703012-   C                   ENDDO
094000070301     C*
0941000703011-   C                   ENDIF
094200070301     C*
094300070301     C                   ENDSR
094400070301     C/EJECT
094500070301      *---------------------------------------------------------------*
094600070320      *?  CONTROLLO CAMPI I/O RIGA LISTA                             ?
094700070301      *---------------------------------------------------------------*
094800070302     C     RECS2         BEGSR
094900070301     C*
095000070301     C* reset indicatori DSPATR
095100070308     c                   exsr      INZ_indERR
095200070327     C*
095300070327     C* gestione opzioni
0954000703273    C     S2sce         IFNE      *blank
095500070327     C     *IN99         ANDEQ     *OFF
095600070327     C                   EXSR      SCE_S2
0957000703273-   C                   ENDIF
095800070301     C*
095900070301     C                   ENDSR
096000070301      *---------------------------------------------------------------*
096100070320      *?  GESTIONE OPZIONI LISTA                                     ?
096200070301      *---------------------------------------------------------------*
096300070309     C     SCE_S2        BEGSR
096400070307     C*
096500070307     C*  Interrogazione bolla
096600080819     c                   If        s2SCE = '5' and s2CR = 'C'
096700070307     c                   eval      kpjbus = kpjbu
096800070307     C                   clear                   dslr36
096900080820     C                   z-add     h2AAS         P36AAS
097000080820     C                   z-add     h2LNP         P36LNP
097100080820     C                   z-add     h2NRS         P36NRS
097200080820     C                   z-add     h2NSP         P36NSP
097300070307     C                   MOVEL     '1'           P36FLG
097400070307     C                   MOVEL     ' '           P36F03
097500070307     C                   movel(p)  dslr36        KPJBU
097600070307     C                   call      'FNLR36R'                            99
097700070307     C                   parm                    KPJBA
097800070307     c                   movel     kpjbus        kpjbu
097900070307     c                   clear                   s2SCE
098000070307     c                   seton                                        99
098100080819      *
098200080820     C*  Interrogazione ORM
098300080819     c                   ElseIF    s2SCE = '5' and s2CR = 'R'
098400080820     c                   movel     kpjbu         kpjbus
098500080820     C                   clear                   PARM01
098600080820     C                   z-add     h2poe         ppoe
098700080820     C                   z-add     h2nor         pnor
098800080820     C                   z-add     h2nsr         pnsr
098900080820     C                   z-add     h2nrv         pnrv
099000080820     C                   move      'D'           psce
099100080820     C                   move      Filiale       pfgs
099200080820     C                   movel(p)  PARM01        KPJBU
099300080820     C                   call      'FIOR07R'                            99
099400080820     C                   parm                    KPJBA
099500080820     c                   movel     kpjbus        kpjbu
099600080821     c                   clear                   s2SCE
099700080821      *
099800070307     c                   endIf
099900070301     C*
100000070301     C                   ENDSR
100100070302     C/EJECT
100200070301      *---------------------------------------------------------------*
100300070316      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
100400070301      *---------------------------------------------------------------*
100500070228     C     CLCPAG        BEGSR
100600940224     C* Input :
100700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
100800940224     C*          cursore
100900070301     C* - Wpagine= numero rcd per pagina sfl
101000940224     C* Output :
101100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
101200940224     C*
101300070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
101400940224     C                   MVR                     RESTO             3 0
101500940224     C     RESTO         IFGT      0
101600940224     C                   ADD       1             PAGINE
101700940224     C                   ENDIF
101800070301     C     PAGINE        MULT      Wpagine       WPAG
101900940224     C*
102000940224     C                   ENDSR
102100080819     C/EJECT
102200080820      *?  Visualizza Giri Assegnati                                  ?
102300070301      *---------------------------------------------------------------*
102400080820     C     F04_GiriAss   BEGSR
102500080820     C*
102600080821     c                   clear                   fidg35ds
102700080821     c                   eval      D35FGSi = Filiale
102800080821     c                   eval      D35DDCi = Data_Distinta
102900080821     c                   eval      D35NDCi = Num_Distinta
103000080821     c                   eval      D35NPGi = Categ_Foglio
103100080821     c                   eval      D35PGMi = c2CPGM
103200080821      * si vuole fare vedere tutto
103300080901     c*********          eval      D35opzi = 'S'
103400080821     c                   eval      kpjbus = kpjbu
103500080821     c                   movel(p)  fidg35ds      kpjbu
103600080821     c                   call      'FIDG35R'
103700080821     c                   parm                    kpjba
103800080821     c                   movel     kpjbu         fidg35ds
103900080821     c                   eval      kpjbu = kpjbus
104000080821     c                   if        d35FunO = '03'
104100080821     c                   move      *on           $FINE
104200080821     c                   end
104300080820     C*
104400080820     C                   ENDSR
104500080820     C/EJECT
104600080820      *?  GESTIONE F03 x uscire LR                                   ?
104700080820      *---------------------------------------------------------------*
104800080820     C     F03_exit      BEGSR
104900070227     C* fine programma
105000940309     C                   MOVE      *ON           $FINE
105100070302      *
105200940309     C                   ENDSR
105300070227     C/EJECT
105400070227      *---------------------------------------------------------------*
105500070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
105600070227      *---------------------------------------------------------------*
105700070228     c     DatiJOB       BEGSR
105800070227      *
105900070227     c     *dtaara       define    §azute        azuteds
106000070227     c     *dtaara       define    §datiute      ddatiute
106100070227      *
106200070227     c                   in(E)     *dtaara
106300070227     c                   IF        %ERROR or RSUT = *blanks
106400070227     c                   clear                   Tibs34Ds
106500070227     c                   call      'TIBS34R'
106600070227     c                   parm                    Tibs34Ds
106700070227     c                   in        *dtaara
106800070227     c                   ENDIF
106900080606      *
107000070227     c                   ENDSR
107100070305     C*---------------------------------------------------------------*
107200070320      *?  Calcola Pesi Volumi e Colli da consegnare                  ?
107300070305     C*---------------------------------------------------------------*
107400070305     C     Cal_PesVol    BEGSR
107500070305     C*
107600070305     c                   eval      kpjbus = kpjbu
107700070323     c                   clear                   fidg27ds
107800080820     c                   z-add     Data_Distinta D27DFVI
107900070323     c                   z-add     arbAAS        d27aasI
108000070305     c                   z-add     arbLNP        d27lnpI
108100070305     c                   z-add     arbNRS        d27nrsI
108200070305     c                   z-add     arbNSP        d27nspI
108300070305     c                   eval      kpjbu = fidg27ds
108400070305     c                   call      'FIDG27R'
108500070305     c                   parm                    kpjba
108600080820     c                   parm                    fnarbds
108700070305     c                   eval      fidg27ds = kpjbu
108800070305     c                   eval      kpjbu = kpjbus
108900070305     C*
109000080820     C*  attenzione se il Da Consegnare è nullo poichè
109100080820     C*    è stato consegnato, sostituisco con il consegnato
109200080820     c                   if        D27NCLDCO = 0 and D27NCLCO > 0
109300080820     c                   z-add     D27NCLCO      D27NCLDCO
109400080820     c                   z-add     D27PKLCO      D27PKLDCO
109500080820     c                   z-add     D27VLUCO      D27VLUDCO
109600080820     c                   end
109700080820     C*
109800070305     C                   ENDSR
109900070319     C/EJECT
110000070515      * ?-------------------------------------------------------------*?
110100070515      *? Riordina comunque il SFL                                     ?
110200070515      * ?-------------------------------------------------------------*?
110300070515     C     Ordina_SFL2   BEGSR
110400070514     C*
110500070514     C*  Totale Records Caricati nel 2°SFL
110600070427     c                   eval      wsfl = 1
110700070427     C*
110800070427     C*  se richiesto Ordinamento sul SFL da operatore --> F11
110900070427     c                   setoff                                       010203
111000070427     c                   setoff                                       040506
111100070427     c                   setoff                                       07
111200070427     C*
111300070427      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
111400070427      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
111500070427      * Il campo è usato per selezionare i records dando un ordine a quelli
111600070427      * selezionati davanti ai non selezionati.
111700070427     C                   CLEAR                   QLGSCB
111800070427     C                   CLEAR                   QLGSCB00
111900070427      *
112000070427     c                   select
112100070427      *
112200070427      *?  Ordinamento per Destinatario    ?
112300070427     C                   when      WrkSort = Ord_DESTINAT
112400070427     c                   move      Ord_DESTINAT  wINDsort
112500080610      *
112600080820     ** Tenendo conto della visualizzazione prioritaria C/R
112700070511     C                   EVAL      QLGNBRK    = 2
112800070427
112900080820     ** La Priorità fra Consegne e Ritiri
113000080820     C                   EVAL      QLGSP   = 1
113100080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
113200080820     C                   EVAL      QLGDT    = Carattere
113300080820      * Priorità dei dati
113400080820     c                   If        Priorita_dati = 'R'
113500070511     C                   EVAL      QLGSO    = Discendente
113600080820     c                   else
113700080820     C                   EVAL      QLGSO    = Ascendente
113800080820     c                   end
113900080820      *
114000070427     C                   EVAL      QLGKL(1) = QLGSKL
114100070511
114200080820     ** Il DEST è in 2° posizione
114300080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2cr)
114400080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
114500070511     C                   EVAL      QLGDT    = Carattere
114600070511     C                   EVAL      QLGSO    = Ascendente
114700070511     C                   EVAL      QLGKL(2) = QLGSKL
114800070427      *
114900070427      *?  Ordinamento per Indirizzo/Destinatario    ?
115000070427     C                   when      WrkSort = Ord_INDIRIZZO
115100070427     c                   move      Ord_INDIRIZZO wINDsort
115200080610      *
115300070511     ** 3 campi chiave.   x INDIRIZZO/DESTINATARIO
115400070511     C                   EVAL      QLGNBRK    = 3
115500080820
115600080820     ** La Priorità fra Consegne e Ritiri
115700080820     C                   EVAL      QLGSP   = 1
115800080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
115900080820     C                   EVAL      QLGDT    = Carattere
116000080820      * Priorità dei dati
116100080820     c                   If        Priorita_dati = 'R'
116200080820     C                   EVAL      QLGSO    = Discendente
116300080820     c                   else
116400080820     C                   EVAL      QLGSO    = Ascendente
116500080820     c                   end
116600080820      *
116700080820     C                   EVAL      QLGKL(1) = QLGSKL
116800070427
116900080820     ** Il INDIRIZZO è in 3°posizione
117000080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
117100080820     c                                           %SIZE(S2DesM)
117200080820     C                   EVAL      QLGSS    = %SIZE(S2ind)
117300070427     C                   EVAL      QLGDT    = Carattere
117400070427     C                   EVAL      QLGSO    = Ascendente
117500070511     C                   EVAL      QLGKL(2) = QLGSKL
117600070427
117700080820     ** Il DEST è in 2°posizione
117800080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
117900080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
118000070427     C                   EVAL      QLGDT    = Carattere
118100070427     C                   EVAL      QLGSO    = Ascendente
118200070511     C                   EVAL      QLGKL(3) = QLGSKL
118300070427
118400070427      *?  Ordinamento per Località/Destinatario    ?
118500070427     C                   when      WrkSort = Ord_LOCALITA
118600070427     c                   move      Ord_LOCALITA  wINDsort
118700080610      *
118800070511     ** 3 campi chiave.   x LOCALITA'/DESTINATARIO
118900070511     C                   EVAL      QLGNBRK    = 3
119000080820
119100080820     ** La Priorità fra Consegne e Ritiri
119200080820     C                   EVAL      QLGSP   = 1
119300080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
119400080820     C                   EVAL      QLGDT    = Carattere
119500080820      * Priorità dei dati
119600080820     c                   If        Priorita_dati = 'R'
119700080820     C                   EVAL      QLGSO    = Discendente
119800080820     c                   else
119900080820     C                   EVAL      QLGSO    = Ascendente
120000080820     c                   end
120100080820      *
120200080820     C                   EVAL      QLGKL(1) = QLGSKL
120300070427
120400070427     ** La LOCALITA' è in posizione (xx) 20 Bytes char ascending
120500080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
120600080820     c                                           %SIZE(S2DesM) +
120700080820     c                                           %SIZE(S2IND)
120800080820     C                   EVAL      QLGSS    = %SIZE(S2lod)
120900070427     C                   EVAL      QLGDT    = Carattere
121000070427     C                   EVAL      QLGSO    = Ascendente
121100070511     C                   EVAL      QLGKL(2) = QLGSKL
121200070427
121300070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
121400080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
121500080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
121600070427     C                   EVAL      QLGDT    = Carattere
121700070427     C                   EVAL      QLGSO    = Ascendente
121800070511     C                   EVAL      QLGKL(3) = QLGSKL
121900080610      *
122000070427      *?  Ordinamento per Peso/Destinatario    ?
122100070427     C                   when      WrkSort = Ord_PESO
122200070427     c                   move      Ord_PESO      wINDsort
122300080610      *
122400080820     ** 3 campi chiave.   x PESO/DESTINATARIO
122500080820     C                   EVAL      QLGNBRK    = 3
122600080820
122700080820     ** La Priorità fra Consegne e Ritiri
122800080820     C                   EVAL      QLGSP   = 1
122900080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
123000080820     C                   EVAL      QLGDT    = Carattere
123100080820      * Priorità dei dati
123200080820     c                   If        Priorita_dati = 'R'
123300080820     C                   EVAL      QLGSO    = Discendente
123400080820     c                   else
123500080820     C                   EVAL      QLGSO    = Ascendente
123600080820     c                   end
123700080820      *
123800080820     C                   EVAL      QLGKL(1) = QLGSKL
123900070427
124000070427     ** Il PESO è in posizione (xx) 7 Digits Num descending
124100080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
124200080820     c                                           %SIZE(S2DesM) +
124300080820     c                                           %SIZE(S2IND) +
124400080820     c                                           %SIZE(S2LOD)
124500080820     C                   EVAL      QLGSS    = %SIZE(S2Peso)
124600070427     C                   EVAL      QLGDT    = Numerico
124700070427     C                   EVAL      QLGSO    = Discendente
124800080820     C                   EVAL      QLGKL(2) = QLGSKL
124900070427
125000070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
125100080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
125200080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
125300070427     C                   EVAL      QLGDT    = Carattere
125400070427     C                   EVAL      QLGSO    = Ascendente
125500080820     C                   EVAL      QLGKL(3) = QLGSKL
125600070427      *
125700070427      *?  Ordinamento per Volume/Destinatario    ?
125800070427     C                   when      WrkSort = Ord_VOLUME
125900070427     c                   move      Ord_VOLUME    wINDsort
126000070427      *
126100080820     ** 3 campi chiave.   x VOLUME/DESTINATARIO
126200080820     C                   EVAL      QLGNBRK    = 3
126300080820
126400080820     ** La Priorità fra Consegne e Ritiri
126500080820     C                   EVAL      QLGSP   = 1
126600080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
126700080820     C                   EVAL      QLGDT    = Carattere
126800080820      * Priorità dei dati
126900080820     c                   If        Priorita_dati = 'R'
127000080820     C                   EVAL      QLGSO    = Discendente
127100080820     c                   else
127200080820     C                   EVAL      QLGSO    = Ascendente
127300080820     c                   end
127400080820      *
127500080820     C                   EVAL      QLGKL(1) = QLGSKL
127600070427
127700080820     ** Il VOLUME
127800080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
127900080820     c                                           %SIZE(S2DesM) +
128000080820     c                                           %SIZE(S2IND) +
128100080820     c                                           %SIZE(S2LOD) +
128200080820     c                                           %SIZE(S2Peso)
128300080820     C                   EVAL      QLGSS    = %SIZE(S2Vol)
128400070427     C                   EVAL      QLGDT    = Numerico
128500070427     C                   EVAL      QLGSO    = Discendente
128600080820     C                   EVAL      QLGKL(2) = QLGSKL
128700070427
128800080820     ** Il DEST
128900080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
129000080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
129100070427     C                   EVAL      QLGDT    = Carattere
129200070427     C                   EVAL      QLGSO    = Ascendente
129300080820     C                   EVAL      QLGKL(3) = QLGSKL
129400080610      *
129500070427     c                   endSL
129600070427      *------------
129700070427      * Load other sort parameters.
129800070427     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
129900070427     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
130000070427     C                   EVAL      QLGRT     = 8
130100070427     C                   EVAL      QLGOKL    = 80
130200070427     C                   EVAL      QLGLKE    = 16
130300070427     C                   EVAL      QLGLSS    = 290
130400070427
130500070427      * Initialize Sort I/O API fields.
130600070427     C                   EVAL      QLGRL00  = QLGRL
130700070427     C                   EVAL      QLGRC00  = 1
130800070427     C                   CLEAR                   QUSEI
130900070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
131000070427
131100070427      * First step - Initialize the sort routine.
131200070427     C                   CALL      'QLGSORT'
131300070427     C                   PARM                    QLGSCB
131400070427     C                   PARM                    NotUsed
131500070427     C                   PARM                    NotUsed
131600070427     C                   PARM                    SizeList
131700070427     C                   PARM                    ReturnSize
131800070427     C                   PARM                    QUSEC
131900070427
132000070427      * Next step - Write records to I/O routine.
132100070427     C                   EVAL      QLGRT00 = Put
132200070427
132300070503     C                   DO        RrnLast       S2nrr_xx          5 0
132400070503     C     S2nrr_xx      CHAIN     FDG0S02
132500070427
132600070427     ** Solo le righe con Selected = 'Y' sono riordinate,
132700070427     ** quindi per fare un ordinamento di tutte le righe
132800070427     ** metto 'Y' sempre.
132900070427     C                   EVAL      Selected  = 'Y'
133000070427
133100070427     C                   CLEAR                   QUSEI
133200070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
133300070427      *
133400070427     C                   CALL      'QLGSRTIO'
133500070427     C                   PARM                    QLGSCB00
133600070427     C                   PARM                    SflRcd
133700070427     C                   PARM                    NotUsed
133800070427     C                   PARM                    SizeList
133900070427     C                   PARM                    NotUsed
134000070427     C                   PARM                    QUSEC
134100070427
134200070427     C                   ENDDO
134300070427
134400070427      * Next step - Signal end of input, clear subfile for reload.
134500070427     C                   EVAL      QLGRT00 = EndPut
134600070427     C                   CLEAR                   QUSEI
134700070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
134800070427
134900070427     C                   CALL      'QLGSRTIO'
135000070427     C                   PARM                    QLGSCB00
135100070427     C                   PARM                    SflRcd
135200070427     C                   PARM                    NotUsed
135300070427     C                   PARM                    SizeList
135400070427     C                   PARM                    NotUsed
135500070427     C                   PARM                    QUSEC
135600070427      *
  pulizia SFL 2  ?
135700070427     C                   SETOFF                                         3031
135800070427     C                   WRITE     FDG0C02
135900070427     C                   SETON                                          31
136000070427      *
136100070427
136200070427      *
  Campo di Ordinamento in REVERSE ?
136300070427     c                   if        wINDsort  > 0
136400070427     C                   move      *on           *in(wINDsort)
136500070427     c                   end
136600070427
136700070427      * Final step - Write the records back to the subfile.
136800070427     C                   EVAL      QLGRT00 = Get
136900070427
137000070503     C                   DO        RrnLast       S2nrr_xx
137100070503     c                   eval      S2nrr = S2nrr_xx
137200070427     C                   CLEAR                   QUSEI
137300070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
137400070427     C                   CALL      'QLGSRTIO'
137500070427     C                   PARM                    QLGSCB00
137600070427     C                   PARM                    NotUsed
137700070427     C                   PARM                    SflRcd
137800070427     C                   PARM                    QLGRL00
137900070427     C                   PARM                    NotUsed
138000070427     C                   PARM                    QUSEC
138100070503      * SFLnxtCHG
138200080820     C                   SetOFF                                       32
138300070503     c                   if        s2sce <> *blank
138400070503     c                   seton                                        32
138500070503     c                   end
138600080820      **
138700070427     C                   WRITE     FDG0S02
138800080820      **
138900070427     C                   ENDDO
139000070427      *
139100070503      *  All'uscita di queste chiamate a routine di sistema il SFL record
139200070503      *   si incrementa stranamente quindi lo reimposto correttamente
139300070503      *    prima di lasciare la routine.
139400070503     C                   EVAL      S2nrr = RrnLast
139500070514     C*
139600070427     C                   ENDSR
139700070427     C/EJECT
139800080609      * ?-------------------------------------------------------------*?
139900080609      *
140000070326** ErrMsg
140100080819                                                                               01
140200080819                                                                               02
140300080819                                                                               03
140400080819                                                                               04
140500080819                                                                               05
140600080819                                                                               06
140700080819                                                                               07
140800080819                                                                               08
140900080819                                                                               09
141000080819                                                                               10
