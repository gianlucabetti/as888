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
024100111014     D  h2xco
024200080923     D  S2when
024300070419     D  Selected                      1A
024400070419
024500070419      /COPY QSYSINC/QRPGLESRC,QLGSORT
024600070419     D QLGKL                         16    DIM(MaxKey)
024700070419     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
024800070419     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
024900070419     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
025000070419     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
025100070419
025200070419      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
025300070419      /COPY QSYSINC/QRPGLESRC,QUSEC
025400080820     C*----------------------------------------------------*
025500080820      *?     MAIN LINE PROGRAM     1 solo video            ?
025600080820     C*----------------------------------------------------*
025700080820     C* inizializzazione variabili
025800080820     C                   EXSR      INZVAR
025900080820     C*
026000080820     C     $FINE         DOWEQ     *OFF
026100080820     C*
026200080820     C* Lista di dettaglio delle Bolle x selezione e guida aggiornamento Data Base
026300080820     C     $GEST         CASEQ     'S2'          GESS2                          Lista delle Bolle
026400080820     C                   END
026500080820      *
026600080820     C                   END
026700080820     C* fine programma
026800080821     C                   eval      KPJBU = FIDG311ds
026900080820     C                   SETON                                            LR
027000080820     C/EJECT
027100080819      *---------------------------------------------------------------*
027200080819      *  ?  OPERAZIONI INIZIALI      ?
027300080819      *---------------------------------------------------------------*
027400080819     C     *INZSR        BEGSR
027500080819     C*
027600080819     C* Reperimento parametri
027700080819     C     *ENTRY        PLIST
027800080819     C                   PARM                    KPJBA
027900080819     C                   movel     KPJBU         FIDG311ds
028000080820      *
028100080820      *  Chiavi
028200080820      *
028300080820     c     karb70        klist
028400080820     c                   kfld                    wfgs_ifp
028500080820     c                   kfld                    Num_Distinta
028600080820      *
028700080826      *
028800080826     c     korm06        klist
028900080826     c                   kfld                    Filiale
029000080826     c                   kfld                    ORmNDC
029100080826      *
029200080826     c     korg01        klist
029300080826     c                   kfld                    ORmPOE
029400080826     c                   kfld                    ORmNSR
029500080826     c                   kfld                    ORmNOR
029600080826     c                   kfld                    ORmNRV
029700080901      *
029800080901     c     Kdst01        klist
029900080901     c                   kfld                    Categ_Foglio
030000080901     c                   kfld                    dst_Distinta
030100080901     c                   kfld                    filiale
030200080901      *
030300080901     c     Kapd01        klist
030400080901     c                   kfld                    tipo_AUT          1
030500080901     c                   kfld                    dstPDR
030600080901     c                   move      'A'           tipo_AUT
030700080826      *
030800080820      *  Imposta i Parametri
030900080820     c                   eval      Filiale        = F31FGS
031000080820     c                   eval      Priorita_Dati  = F31PriDati
031100080820     c                   eval      Num_Distinta   = F31NumDis
031200080820     c                   eval      Data_Distinta  = F31DatDis
031300080820     c                   eval      Dati_da_Visual = F31DatiVis
031400080821     c                   eval      Categ_Foglio   = F31Npg
031500080820      *
031600080820      * Se non richiesto esplicitamente i Ritiri impostare Consegne di Default
031700080820     c                   if        Priorita_Dati  <> 'R'
031800080820     c                   eval      Priorita_Dati  =  'C'
031900080820     c                   end
032000080819      *
032100080819      * Reperisco dati job
032200080819     c                   exsr      DatiJob
032300080819
032400080819     C* Variabili per gestione videate
032500080820     C                   MOVE      *BLANK        $ESCI             1
032600080819     C                   MOVE      *BLANK        $GEST             2
032700080819     C                   MOVE      *BLANK        $FINE             1
032800080819     C                   MOVE      *BLANK        $INZS2            1
032900080819     C                   MOVE      *BLANK        $EFILE            1
033000080820     C                   MOVE      *OFF          $RCDOK            1
033100080819     C* Indici
033200080819     C                   Z-ADD     0             X                 3 0
033300080819     C                   Z-ADD     0             Y                 3 0
033400080820      *
033500080819      * GIRO DATA DEL GIORNO: LA PRENDO DA TIME
033600080819     C                   TIME                    W0140            14 0
033700080819     C                   MOVE      W0140         UDATE8            8 0
033800080819     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
033900080819     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
034000080819     C*
034100080819     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
034200080819     C                   MOVEL     *BLANK        G02ERR
034300080819     C                   CALL      'XSRDA8'
034400080819     C                   PARM                    WLBDAT
034500080819     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
034600080819      *
034700080819     C                   ENDSR
034800080819     C/EJECT
034900030113     C************************************************************
035000070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
035100030113     C************************************************************
035200030113     C     INZVAR        BEGSR
035300030113     C*
035400030113     C* Variabili per gestione videate
035500080820     C                   MOVE      'S2'          $GEST
035600030113     C                   MOVE      *OFF          $FINE
035700030113     C                   MOVE      *OFF          $ESCI
035800080820     C                   MOVE      *OFF          $EFILE
035900030113     C                   MOVE      *OFF          $RCDOK
036000080819     C                   MOVE      *ON           $INZS2
036100030114     C                   Z-ADD     1             WPAG
036200030113     c*
036300030113     C                   ENDSR
036400070227     C/EJECT
036500070301      *---------------------------------------------------------------*
036600070316      *  ?  GESTIONE SFL Bolle da selezionare                        ?
036700070301      *---------------------------------------------------------------*
036800070301     C     GESS2         BEGSR
036900070301     C*
037000070301     C* inizializzazione videata
037100070301     C     $INZS2        IFEQ      *ON
037200070301     C                   EXSR      INZS2
037300070301     C                   MOVE      *OFF          $INZS2
037400070301     C                   ENDIF
037500070306     C*
037600070306     C* Inizializza e imposta piede videata
037700070306     C                   EXSR      INZZ2
037800070305     C*
037900070301     C* emissione piede videata
038000070302      * ?            *-----------------------*
038100070301     C                   WRITE     FDG0Z02
038200070302      * ?            *-----------------------*
038300070301     C*
038400070301     C     WMAX          IFEQ      0
038500080820     C*
038600080820     C* Non ci sono records  Videata di Avviso
038700070302      * ?            *-----------------------*
038800070301     C                   WRITE     FDG0V02
038900070302      * ?            *-----------------------*
039000070301     c                   setoff                                       30
039100080820      *
039200080820     C                   ELSE
039300080820      *
039400070301     c                   seton                                        30
039500070301      *
039600080820      * Reimposta il posizionamento della pagina
039700080820     c                   exsr      Posiz_Pagina
039800080820      *
039900070301     C                   ENDIF
040000070301      *
040100070301      *  Emissione SFL delle BOLLE
040200080820      * ?            *************************
040300070301     C                   EXFMT     FDG0C02
040400080820      * ?            *************************
040500070326     c                   setoff                                       99
040600070301      *
040700070301     C     C2NRR         IFNE      0
040800070301     C                   Z-ADD     C2NRR         WSFL
040900070301     C                   ENDIF
041000070301     C                   Z-ADD     SFLNRR        C2RCD
041100070301     C* Selezioni
0412000703011    C                   SELECT
041300070301     C* F3=Fine
041400070301     C     $TASTO        WHENEQ    F03
041500080821     c                   move      '03'          F31TFunZ
041600070301     C                   EXSR      F03_exit
041700070307      *
041800080819     C* F4=Giri Ass.
041900080819     C     $TASTO        WHENEQ    F04
042000080819     C                   EXSR      F04_GiriAss
042100070426      *
042200070427     C* F07= Ordinamento x Peso
042300070427     C     $TASTO        WHENEQ    F07
042400070427     c                   z-add     6             WrkSort
042500070427     C                   EXSR      Ordina_SFL2
042600070301      *
042700070427     C* F08= Ordinamento x Volume
042800070427     C     $TASTO        WHENEQ    F08
042900070427     c                   z-add     5             WrkSort
043000070427     C                   EXSR      Ordina_SFL2
043100070427      *
043200070427     C* F10= Ordinamento x Località
043300070427     C     $TASTO        WHENEQ    F10
043400070427     c                   z-add     4             WrkSort
043500070427     C                   EXSR      Ordina_SFL2
043600070427      *
043700070427     C* F11= Ordinamento x Indirizzo
043800070427     C     $TASTO        WHENEQ    F11
043900070427     c                   z-add     3             WrkSort
044000070427     C                   EXSR      Ordina_SFL2
044100070427      *
044200080820     C* F13= Ordinamento x Destinatario / Mittente
044300070427     C     $TASTO        WHENEQ    F13
044400070427     c                   z-add     2             WrkSort
044500070427     C                   EXSR      Ordina_SFL2
044600070427      *
044700070301     C* F12=Ritorno
044800070301     C     $TASTO        WHENEQ    F12
044900080819     C                   EXSR      F03_exit
045000070301      *
0451000703011O   C                   OTHER
045200070301     C* CONTROLLO testata
045300070301     C                   EXSR      CTRC2
045400080820      *
045500070301     C* CONTROLLO subfile
045600070301     C     *IN99         IFEQ      *OFF
045700070301     C                   EXSR      CTRS2
045800070301     C                   END
045900070301      *
0460000703011-   C                   ENDSL
046100070301     C*
046200070301     C                   ENDSR
046300070306     C/EJECT
046400070306      *---------------------------------------------------------------*
046500080820      *?   Riposizionamento su Pagina SFL                            ?
046600070306      *---------------------------------------------------------------*
046700080820     C     Posiz_Pagina  BEGSR
046800070306     C*
046900080820     C     Wsfl          IFgt      0
047000080820     C*
047100080820      * essendo il posizionamento *TOP
047200080820      *  non è + corretto impostare direttamente il record
047300080820      *   ma occorre ricavare il primo record della pagina.
047400080820     c                   z-add     0             c2rcd
047500080820      *
047600080820     c                   if        wrigsfl2 > 0
047700080820     c     wsfl          div       wrigsfl2      C2RCD
047800080820     c                   mvr                     resto
047900080820     c                   if        c2rcd > 0
048000080820     c                   mult      wrigsfl2      C2RCD
048100080820     c                   if        resto > 0
048200080820     c                   add       1             C2RCD
048300080820     c                   end
048400080820     c                   else
048500080820     c                   z-add     1             C2RCD
048600080820     c                   end
048700080820     c                   end
048800080820      *
048900080820      * attenzione all'incremento di 1 può far schiantare il sfl
049000080820      * poichè può superare il nr.max delle righe presenti.
049100080820      * esempio se wsfl = 16 o un suo multiplo essendo wrigsfl2 = 16
049200080820      * c2rcd alla fine equivale a wsfl ossia 16 ....e se in seguito
049300080820      * si aggiunge 1 al sflrcdnbr andiamo fuori limite max record
049400080820      * caricati e si schianta in emissione del sfl.
049500080820     c                   if        resto = 0
049600080820     c                   eval      C2RCD = c2rcd - (wrigsfl2 - 1)
049700080820     c                   else
049800080820     c                   if        c2rcd = 0
049900080820     c                   add       1             c2rcd
050000080820     c                   end
050100080820     c                   end
050200080820      *
050300080820     C                   Else
050400080820     C     Wpag          IFgt      0
050500080820     C                   Z-ADD     wpag          C2RCD
050600080820     C                   EndIF
050700080820     C                   EndIF
050800080820     C*
050900080820     C                   ENDSR
051000080820     C/EJECT
051100080820      *---------------------------------------------------------------*
051200080820      *?   INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL               ?
051300080820      *---------------------------------------------------------------*
051400080820     C     INZZ2         BEGSR
051500080820     C*
051600070306     C* Messaggio o tasti funzionali
051700070306     c                   clear                   z2dmsg
051800070328     c                   clear                   ErrmsgZ          80
051900070328      *
052000070328      * imposta eventuali errori
052100070328     c                   eval      z2dmsg = ErrMsgZ
052200080820      *
052300070307      * tasti funzionali se non è stato emesso alcun messaggio
052400070328     c                   if        ErrMsgZ = *blank
052500070307     c                   setoff                                       99
052600070427     c                   end
052700070426      *
052800070306     C                   ENDSR
052900070301     C/EJECT
053000070301      *---------------------------------------------------------------*
053100070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
053200070301      *---------------------------------------------------------------*
053300070301     C     INZS2         BEGSR
053400070301     C*
053500070301     C* reset indicatori DSPATR
053600070308     c                   exsr      INZ_indERR
053700070301     C* pulizia SFL
053800070301     C                   SETOFF                                         3031
053900070301     C                   WRITE     FDG0C02
054000070301     C                   SETON                                          31
054100070301     C*
054200080820     C* Righe x Pagina su SFL
054300080820     c                   z-add     Otto_righe    s2pag
054400070420      *
054500080820     C* Default di Ordinamento
054600080820     C                   EVAL      WrkSort  = Ord_DESTINAT
054700080820     c                   eval      wINDsort = Ord_DESTINAT
054800070301      *
054900070301     C* CARICAMENTO SFL totale
055000070301     C                   Z-ADD     0             S2NRR
055100070301     C                   Z-ADD     1             C2RCD
055200070301     C                   Z-ADD     0             WMAX
055300070302     C                   Z-ADD     0             Wsfl
055400070302     C* campi titolo
055500080820     C                   MOVEL     RSUT          c2crsu
055600070302     C                   MOVEL     dsPGM         c2CPGM
055700070302     C                   MOVEL     knsif         c2nsif
055800070302     C                   MOVEL     knmus         c2nmus
055900080820     C                   eval      c2ndc = Num_Distinta
056000080901      *
056100080901      * Decodifica l'Autista
056200080901     C                   eval      Dst_Distinta = Num_Distinta
056300080901     c     Kdst01        chain     fiDST01l
056400080901     c                   if        %Found(FiDST01L)
056500080901     c     Kapd01        chain     fiAPD01l
056600080901     c                   if        %Found(fiAPD01l)
056700080901     c                   eval      c2daut = APDRSC
056800080901     c                   end
056900080901     c                   end
057000080901      *
057100080820      * Se non richiesto esplicitamente i Ritiri impostare Consegne di Default
057200080820     c                   if        Priorita_Dati  <> 'R'
057300080820     c                   eval      c2P1 =  'C'
057400080820     c                   eval      c2P2 =  'R'
057500080820     c                   else
057600080820     c                   eval      c2P1 =  'R'
057700080820     c                   eval      c2P2 =  'C'
057800080820     c                   end
057900080820      *
058000080820      * Carica il SFL
058100070301     c                   EXSR      ROLS2
058200070301     C*
058300070301     C                   Z-ADD     1             WPAG
058400070301     C*
058500070301     C                   ENDSR
058600080819     C************************************************************
058700080819      *?  INIZIALIZZA indicatori di errore                           ?
058800080819      *---------------------------------------------------------------*
058900080819     C     INZ_indERR    BEGSR
059000080819      *
059100080819     C                   move      *ALL'0'       IN5190           40
059200080819     C                   moveA     IN5190        *IN(51)
059300080819      *
059400080819     C                   ENDSR
059500080819     C/EJECT
059600070301      *---------------------------------------------------------------*
059700080826      *?  CARICAMENTO PAGINA SFL                                  ?
059800070301      *---------------------------------------------------------------*
059900070301     C     ROLS2         BEGSR
060000070301     C*
060100070301     C                   SETOFF                                       32
060200080820     C*
060300070301     C                   Z-ADD     0             Y
060400070301     C                   Z-ADD     WMAX          S2NRR
060500070301     C*
060600080820      * ?      Carica x Numero Distinta
060700080820     c                   if        Priorita_Dati  <> 'R'
060800080821     C* Consegne + Ritiri
060900080820     c                   exsr      CARica_Bolle
061000080820     c                   exsr      CARica_ORM
061100080820     c                   else
061200080821     C* Ritiri + Consegne
061300080820     c                   exsr      CARica_ORM
061400080820     c                   exsr      CARica_Bolle
061500080820     c                   end
061600070301     C* Fine Caricamento
061700070301     C                   Z-ADD     S2NRR         WMAX                 30
061800070419     C*
061900070419     C* se ci sono almeno 2 records da ordinare fra loro
062000070419     c                   if        wmax > 1
062100070419      * ?   Esegue l'Ordinamento x Giro/Spedizione
062200070419     c                   exsr      Ordina_SFL2
062300070419     c                   end
062400070301     C*
062500070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
062600070301     C     S2NRR         DIV       S2pag         PAGINE            4 0
062700070301     C                   MVR                     RESTO             3 0
062800070301     C     PAGINE        MULT      S2pag         C2RCD
0629000703011    C     RESTO         IFGT      0
063000070301     C                   ADD       1             C2RCD
0631000703011E   C                   ELSE
063200070301     C                   SUB       S2pag         C2RCD
063300070301     C                   ADD       1             C2RCD
0634000703011-   C                   ENDIF
063500070301     C*
063600070301     C                   ENDSR
063700070302      *---------------------------------------------------------------*
063800080820      *  ?  Carica --> I RITIRI      x Numero Distinta                ?
063900080820      *---------------------------------------------------------------*
064000080820     C     CARica_ORM    BEGSR
064100080821
064200080821     C*   Se richiesta la visualizzazione dei Ritiri o di Entrambe
064300081111     c******             if        Dati_da_Visual = 'O' or
064400081111     c****                         Dati_da_Visual = 'E'
064500080826      *
064600080826      * leggo Ritiri x Filiale/Distinta
064700080826     c                   z-add     Num_Distinta  ORmNDC
064800080826     c     korm06        setll     fnorm06l
064900080826
065000080820      * leggo gli ORM
065100080820     c                   exsr      LEGGE_ORM
065200080820
065300080820     C* Leggo dal file per caricare la lista
0654000808201    C     $EFILE        DOWEQ     *OFF
065500080820     C                   clear                   FDG0S02
065600080820      *
065700080820     c                   exsr      RIGA_ORM
065800080820      *
065900080820     C                   ADD       1             S2NRR
066000080820     C                   EVAL      RrnLast = S2nrr
066100080820     C                   ADD       1             Y
066200130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
066300130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
066400130221     c*****h2xco         comp      ' '                                8888
066500130221     c     h2xco         comp      '0'                                88
066600080820     C                   WRITE     FDG0S02
066700080820     C*
066800080820      * Lettura di un record
066900080820     c                   exsr      LEGGE_ORM
0670000808201-   C                   ENDDO
067100080821      *
067200081111     c*******            end
067300080820     C*
067400080820     C                   ENDSR
067500080820      *------------------------------------------------------------------------*
067600080826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
067700080820      *------------------------------------------------------------------------*
067800080820     c     LEGGE_ORM     begsr
067900080820
068000080820     C*  Per EoF deve uscire
068100080820      * per rcd ok deve uscire riportando i dati
068200080820     C                   MOVEL     *OFF          $EFILE
068300080820     C                   MOVE      *OFF          $RCDOK
068400080820     C*
0685000808201    C     $EFILE        DOUEQ     *ON
068600080820     C     $RCDOK        OREQ      *ON
068700080820
068800080826     c     korm06        reade     fnORM06l
068900080826      * Fine File
069000080826     c                   if        %Eof(fnORM06l)
069100080826     C                   MOVEL     *ON           $EFILE
069200080826     C                   MOVE      $EFILE        *IN33
069300080826     c                   else
069400080826     c     korg01        chain     fnORG01l
069500080826     c                   if        not %found(fnORg01l)
069600080826     c                   clear                   fnORGds
069700080826     c                   end
069800080826     c                   end
069900080826      *
070000080820     c                   eval      $rcdok = *on
070100080820     c                   enddo
070200080820
070300080820     c                   endsr
070400080820
070500080821      *---------------------------------------------------------------*
070600080821      *?  Imposta Campi del 2°SFL x ORM                              ?
070700080821      *---------------------------------------------------------------*
070800080821     C     RIGA_ORM      BEGSR
070900080821     C*
071000080821     C* Dati Ritiro
071100080821      *  Salva la chiave anche per riposizionarsi in seguito
071200080821     c                   z-add     ormPOE        H2POE
071300080821     c                   z-add     ormNOR        H2NOR
071400080821     c                   z-add     ormNSR        H2NSR
071500080821     c                   z-add     ormNRV        H2NRV
071600111014     c                   clear                   H2xco
071700080821      *  Ritiro
071800080821     c                   movel     'R'           S2cr
071900080821      *
072000080821      *  Riferimento ORM
072100080821     c                   eval        %subst(S2bolOrm:1:3)  = %editc(ormPOE:'X')
072200080821     c                   eval        %subst(S2bolOrm:5:2)  = %editc(ormNSR:'Z')
072300080821     c                   eval        %subst(S2bolOrm:8:7) = %char(ormNOR)
072400080821     c                   eval        %subst(S2bolOrm:16:2) = %editc(ormNRV:'Z')
072500080821      *
072600080821     c                   movel     ormRSR        S2desM
072700080821     c                   movel     ormINR        S2ind
072800080821     c                   movel     ormLOR        S2lod
072900080821     c                   z-add     ormBNC        S2Banc
073000080821     c                   if        ormSPI ='S'
073100080821     c                   move      'Sp'          S2Spo
073200080821     c                   end
073300080821     c                   movel     ormRSO        S2cMit
073400080821      *
073500080821      * Data Ora di ritiro
073600080821     C                   Z-ADD     ormDAR        G02inv
073700080821     C                   MOVEL     '3'           G02ERR
073800080821     C                   CALL      'XSRDA8'
073900080821     C                   PARM                    WLBDAT
074000080821     C                   Z-ADD     G02DAT        S2dcr
074100080821     c                   z-add     ormORR        S2hcr
074200080821      *
074300080821      *  Pesi Volumi Colli
074400080821     c                   z-add     ormNCL        S2coll
074500080821     c                   eval      S2vol  = orgvlm
074600080821      *
074700080821     c                   if        orgpkg > *zeros
074800080821     c                   if        orgpkg > 9999
074900080821     c                   eval      S2peso = 9999
075000080821     c                   else
075100080821     c                   eval      S2peso = %dech(orgpkg:5:0)
075200080821     c                   endif
075300080821     c                   if        S2peso = *zeros
075400080821     c                   eval      S2peso = 1
075500080821     c                   endif
075600080821     c                   else
075700080821     c                   clear                   S2peso
075800080821     c                   endif
075900080821      *
076000080821     C                   ENDSR
076100080826      *---------------------------------------------------------------*
076200080826      *  ?  Carica --> LE BOLLE      x Numero Distinta                ?
076300080826      *---------------------------------------------------------------*
076400080826     C     CARica_Bolle  BEGSR
076500080826     C*
076600080826     C*   Se richiesta la visualizzazione delle Consegne o di Entrambe
076700081111     c******             if        Dati_da_Visual = 'C' or
076800081111     c******                       Dati_da_Visual = 'E'
076900080826      *
077000080826      * leggo Bolle x Filiale/Distinta
077100080826     c                   z-add     Filiale       wfgs_ifp
077200080826     c     karb70        setll     fnarb70l
077300080826
077400080826      * ? ----------------------------
077500080826      * ? Carico il SFL
077600080826     c                   exsr      LEGGE_Bolle
077700080826     C*
077800080826     C* Leggo dal file per caricare la lista
0779000808261    C     $EFILE        DOWEQ     *OFF
078000080826     C                   clear                   FDG0S02
078100080826      *
078200080826     c                   exsr      RIGA_Bolla
078300080826      *
078400080826     C                   ADD       1             S2NRR
078500080826     C                   EVAL      RrnLast = S2nrr
078600080826     C                   ADD       1             Y
078700130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
078800130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
078900130221     c*****h2xco         comp      ' '                                8888
079000130221     c     h2xco         comp      '0'                                88
079100080826     C                   WRITE     FDG0S02
079200080826     C*
079300080826      * Lettura di un record
079400080826     c                   exsr      LEGGE_Bolle
0795000808261-   C                   ENDDO
079600080826      *
079700081111     c*******            end
079800080826     C*
079900080826     C                   ENDSR
080000080820      *---------------------------------------------------------------*
080100070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
080200070302      *---------------------------------------------------------------*
080300070302     C     LEGGE_Bolle   BEGSR
080400070302     C*
080500070302     C*  Per EoF deve uscire
080600070302     C                   MOVEL     *OFF          $EFILE
080700080820     C                   MOVE      *OFF          $RCDOK
080800070302     C*
0809000703021    C     $EFILE        DOUEQ     *ON
081000070302     C     $RCDOK        OREQ      *ON
081100080821      *
081200080821      * ?LETTURE  FILE
081300080821     C*
081400080821     c     karb70        reade     fnarb70l
081500080821      * Fine File
081600080820     c                   if        %Eof(fnarb70l)
081700080820     C                   MOVEL     *ON           $EFILE
081800080820     C                   MOVE      $EFILE        *IN33
081900070417     c                   end
082000080821      *
082100080820     C                   MOVE      *ON           $RCDOK
0822000703021-   C                   ENDDO
082300070302     C*
082400070302     C                   ENDSR
082500070301      *---------------------------------------------------------------*
082600080820      *?  Imposta Campi del 2°SFL x Bolla di consegna                ?
082700070301      *---------------------------------------------------------------*
082800080820     C     RIGA_Bolla    BEGSR
082900070301     C*
083000070301     C* Dati Bolla
083100070301      *  Salva la chiave anche per riposizionarsi in seguito
083200080819     c                   z-add     ARBaas        H2aas
083300080819     c                   z-add     ARBlnp        H2lnp
083400080819     c                   z-add     ARBnrs        H2nrs
083500080819     c                   z-add     ARBnsp        H2nsp
083600111014     c                   movel     ARBxco        H2xco
083700080820      *  Consegna
083800080820     c                   movel     'C'           S2cr
083900080820     c                   movel     Arbtsp        S2tSer
084000080820      *
084100080820      *  Riferimento Bolla
084200080820     c                   eval        %subst(S2bolOrm:1:4)  = %char(Arbaas)
084300080821     c                   eval        %subst(S2bolOrm:6:3)  = %editc(ArbLnp:'X')
084400080821     c                   eval        %subst(S2bolOrm:10:2) = %editc(ArbNrs:'Z')
084500080820     c                   eval        %subst(S2bolOrm:13:7) = %char(ArbNsp)
084600080820      *
084700080820     c                   movel     ARBrsD        S2desM
084800080819     c                   movel     ARBind        S2ind
084900080819     c                   movel     ARBlod        S2lod
085000080820     c                   movel     ARBrsM        S2cMit
085100080820      *
085200080820      * Data Ora richiesta consegna
085300080923     C*****              Z-ADD     ARBdcr        G02inv
085400080923     C*****              MOVEL     '3'           G02ERR
085500080923     C*****              CALL      'XSRDA8'
085600080923     C*****              PARM                    WLBDAT
085700080923     C*****              Z-ADD     G02DAT        S2dcr
085800080923     c*****              z-add     ARBhcr        S2hcr
085900080820      *
086000080923      *  Decodifica quando deve essere riferita la data di richiesta Consegna
086100080923     c                   if        arbdcr>0 or arbhcr>0
086200080923     c                   clear                   fnlv80ds
086300080923     c                   eval      ilv80aas=arbaas
086400080923     c                   eval      ilv80lnp=arblnp
086500080923     c                   eval      ilv80nrs=arbnrs
086600080923     c                   eval      ilv80nsp=arbnsp
086700080923     c                   eval      ilv80mgs=arbmgs
086800080923     c                   eval      ilv80tc1=arbtc1
086900080923     c                   eval      ilv80tc2=arbtc2
087000080923     c                   eval      ilv80dcr=arbdcr
087100080923     c                   eval      ilv80hcr=arbhcr
087200080923     c                   eval      ilv80tcr=arbtcr
087300080923     c                   call      'FNLV80R'
087400080923     c                   parm                    kpjba
087500080923     c                   parm                    fnlv80ds
087600080923     c
087700080923     c                   eval      s2dcr  = olv80dcr8
087800080923     c                   eval      s2when = olv80tcrd
087900080923     c                   z-add     ARBhcr        S2hcr
088000080923     c                   endif
088100080923     c
088200080923      *
088300080923      *
088400070302      *  Pesi Volumi Colli da consegnare
088500070305     c                   exsr      Cal_PesVol
088600080820      *
088700070302      *   i pesi/volumi/Colli Reali da consegnare
088800080819     c                   z-add     D27PKLDCO     S2peso
088900070502     c                   move      D27PKLDCO     Decimal_uno       1 0
089000070502     c                   if        Decimal_uno > 0
089100080819     c                   add       1             S2peso
089200070502     c                   end
089300080819     c                   z-add     D27VLUDCO     S2vol
089400080820     c                   z-add     D27NCLDCO     S2coll
089500070301      *
089600070301     C                   ENDSR
089700070301      *---------------------------------------------------------------*
089800070320      *?  CONTROLLO TESTATA LISTA                                    ?
089900070301      *---------------------------------------------------------------*
090000070301     C     CTRC2         BEGSR
090100070301     C                   MOVE      *OFF          *IN99
090200070301     C                   ENDSR
090300070514      *---------------------------------------------------------------*
090400070514      *?  CONTROLLO OPZIONI LISTA                                    ?
090500070514      *---------------------------------------------------------------*
090600070514     C     CTRS2         BEGSR
090700070301     C*
090800070301     C                   MOVEL     *OFF          $ESCI
090900070301     C                   SETOFF                                       99
091000070301     C*
091100070301     C* Leggo il sfl solo se ci sono rcd
0912000703011    C     WMAX          IFGT      0
091300070302     C                   READC     FDG0S02                                21
091400070301     C*
091500070301     C* esce se fine sfl o errore che richiede l'uscita
0916000703012    C     *IN21         DOWEQ     *OFF
091700070301     C     $ESCI         ANDEQ     *OFF
091800070302     C                   Z-ADD     S2NRR         C2RCD
091900070301     C* ctrl su riga
092000070302     C                   EXSR      RECS2
092100070301      *
092200070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0923000703013    C     *IN99         IFEQ      *ON
092400070301     C     $ESCI         OREQ      *ON
092500070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
092600070301     C* la ripristinerò a conclusione del ciclo di READC
092700070302     C                   MOVE      *OFF          $INZS2
0928000703013-   C                   ENDIF
092900070301     C*
093000070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
093100080820     C                   SetOFF                                       32
093200080819     c                   if        s2sce = '5'
093300070301     C                   MOVE      *ON           *IN32
093400070307     c                   end
093500080820      *
093600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
093700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
093800130221     c*****h2xco         comp      ' '                                8888
093900130221     c     h2xco         comp      '0'                                88
094000070302     C                   UPDATE    FDG0S02
094100070301     C*
094200070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0943000703013    C     $ESCI         IFEQ      *OFF
094400070302     C                   READC     FDG0S02                                21
094500070301      *
094600070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0947000703014    C     *IN21         IFEQ      *ON
094800070301     C* calcolo pagina a cui deve posizionarsi
094900070302     c                   z-add     s2pag         Wpagine
095000070301     C                   EXSR      CLCPAG
0951000703014-   C                   ENDIF
0952000703013-   C                   ENDIF
095300070301     C*
0954000703012-   C                   ENDDO
095500070301     C*
0956000703011-   C                   ENDIF
095700070301     C*
095800070301     C                   ENDSR
095900070301     C/EJECT
096000070301      *---------------------------------------------------------------*
096100070320      *?  CONTROLLO CAMPI I/O RIGA LISTA                             ?
096200070301      *---------------------------------------------------------------*
096300070302     C     RECS2         BEGSR
096400070301     C*
096500070301     C* reset indicatori DSPATR
096600070308     c                   exsr      INZ_indERR
096700070327     C*
096800070327     C* gestione opzioni
0969000703273    C     S2sce         IFNE      *blank
097000070327     C     *IN99         ANDEQ     *OFF
097100070327     C                   EXSR      SCE_S2
0972000703273-   C                   ENDIF
097300070301     C*
097400070301     C                   ENDSR
097500070301      *---------------------------------------------------------------*
097600070320      *?  GESTIONE OPZIONI LISTA                                     ?
097700070301      *---------------------------------------------------------------*
097800070309     C     SCE_S2        BEGSR
097900070307     C*
098000070307     C*  Interrogazione bolla
098100080819     c                   If        s2SCE = '5' and s2CR = 'C'
098200070307     c                   eval      kpjbus = kpjbu
098300070307     C                   clear                   dslr36
098400080820     C                   z-add     h2AAS         P36AAS
098500080820     C                   z-add     h2LNP         P36LNP
098600080820     C                   z-add     h2NRS         P36NRS
098700080820     C                   z-add     h2NSP         P36NSP
098800070307     C                   MOVEL     '1'           P36FLG
098900070307     C                   MOVEL     ' '           P36F03
099000070307     C                   movel(p)  dslr36        KPJBU
099100070307     C                   call      'FNLR36R'                            99
099200070307     C                   parm                    KPJBA
099300070307     c                   movel     kpjbus        kpjbu
099400070307     c                   clear                   s2SCE
099500070307     c                   seton                                        99
099600080819      *
099700080820     C*  Interrogazione ORM
099800080819     c                   ElseIF    s2SCE = '5' and s2CR = 'R'
099900080820     c                   movel     kpjbu         kpjbus
100000080820     C                   clear                   PARM01
100100080820     C                   z-add     h2poe         ppoe
100200080820     C                   z-add     h2nor         pnor
100300080820     C                   z-add     h2nsr         pnsr
100400080820     C                   z-add     h2nrv         pnrv
100500080820     C                   move      'D'           psce
100600080820     C                   move      Filiale       pfgs
100700080820     C                   movel(p)  PARM01        KPJBU
100800080820     C                   call      'FIOR07R'                            99
100900080820     C                   parm                    KPJBA
101000080820     c                   movel     kpjbus        kpjbu
101100080821     c                   clear                   s2SCE
101200080821      *
101300070307     c                   endIf
101400070301     C*
101500070301     C                   ENDSR
101600070302     C/EJECT
101700070301      *---------------------------------------------------------------*
101800070316      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
101900070301      *---------------------------------------------------------------*
102000070228     C     CLCPAG        BEGSR
102100940224     C* Input :
102200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
102300940224     C*          cursore
102400070301     C* - Wpagine= numero rcd per pagina sfl
102500940224     C* Output :
102600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
102700940224     C*
102800070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
102900940224     C                   MVR                     RESTO             3 0
103000940224     C     RESTO         IFGT      0
103100940224     C                   ADD       1             PAGINE
103200940224     C                   ENDIF
103300070301     C     PAGINE        MULT      Wpagine       WPAG
103400940224     C*
103500940224     C                   ENDSR
103600080819     C/EJECT
103700080820      *?  Visualizza Giri Assegnati                                  ?
103800070301      *---------------------------------------------------------------*
103900080820     C     F04_GiriAss   BEGSR
104000080820     C*
104100080821     c                   clear                   fidg35ds
104200080821     c                   eval      D35FGSi = Filiale
104300080821     c                   eval      D35DDCi = Data_Distinta
104400080821     c                   eval      D35NDCi = Num_Distinta
104500080821     c                   eval      D35NPGi = Categ_Foglio
104600080821     c                   eval      D35PGMi = c2CPGM
104700080821      * si vuole fare vedere tutto
104800080901     c*********          eval      D35opzi = 'S'
104900080821     c                   eval      kpjbus = kpjbu
105000080821     c                   movel(p)  fidg35ds      kpjbu
105100080821     c                   call      'FIDG35R'
105200080821     c                   parm                    kpjba
105300080821     c                   movel     kpjbu         fidg35ds
105400080821     c                   eval      kpjbu = kpjbus
105500080821     c                   if        d35FunO = '03'
105600080821     c                   move      *on           $FINE
105700080821     c                   end
105800080820     C*
105900080820     C                   ENDSR
106000080820     C/EJECT
106100080820      *?  GESTIONE F03 x uscire LR                                   ?
106200080820      *---------------------------------------------------------------*
106300080820     C     F03_exit      BEGSR
106400070227     C* fine programma
106500940309     C                   MOVE      *ON           $FINE
106600070302      *
106700940309     C                   ENDSR
106800070227     C/EJECT
106900070227      *---------------------------------------------------------------*
107000070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
107100070227      *---------------------------------------------------------------*
107200070228     c     DatiJOB       BEGSR
107300070227      *
107400070227     c     *dtaara       define    §azute        azuteds
107500070227     c     *dtaara       define    §datiute      ddatiute
107600070227      *
107700070227     c                   in(E)     *dtaara
107800070227     c                   IF        %ERROR or RSUT = *blanks
107900070227     c                   clear                   Tibs34Ds
108000070227     c                   call      'TIBS34R'
108100070227     c                   parm                    Tibs34Ds
108200070227     c                   in        *dtaara
108300070227     c                   ENDIF
108400080606      *
108500070227     c                   ENDSR
108600070305     C*---------------------------------------------------------------*
108700070320      *?  Calcola Pesi Volumi e Colli da consegnare                  ?
108800070305     C*---------------------------------------------------------------*
108900070305     C     Cal_PesVol    BEGSR
109000070305     C*
109100070305     c                   eval      kpjbus = kpjbu
109200070323     c                   clear                   fidg27ds
109300080820     c                   z-add     Data_Distinta D27DFVI
109400070323     c                   z-add     arbAAS        d27aasI
109500070305     c                   z-add     arbLNP        d27lnpI
109600070305     c                   z-add     arbNRS        d27nrsI
109700070305     c                   z-add     arbNSP        d27nspI
109800070305     c                   eval      kpjbu = fidg27ds
109900070305     c                   call      'FIDG27R'
110000070305     c                   parm                    kpjba
110100080820     c                   parm                    fnarbds
110200070305     c                   eval      fidg27ds = kpjbu
110300070305     c                   eval      kpjbu = kpjbus
110400070305     C*
110500080820     C*  attenzione se il Da Consegnare è nullo poichè
110600080820     C*    è stato consegnato, sostituisco con il consegnato
110700080820     c                   if        D27NCLDCO = 0 and D27NCLCO > 0
110800080820     c                   z-add     D27NCLCO      D27NCLDCO
110900080820     c                   z-add     D27PKLCO      D27PKLDCO
111000080820     c                   z-add     D27VLUCO      D27VLUDCO
111100080820     c                   end
111200080820     C*
111300070305     C                   ENDSR
111400070319     C/EJECT
111500070515      * ?-------------------------------------------------------------*?
111600070515      *? Riordina comunque il SFL                                     ?
111700070515      * ?-------------------------------------------------------------*?
111800070515     C     Ordina_SFL2   BEGSR
111900070514     C*
112000070514     C*  Totale Records Caricati nel 2°SFL
112100070427     c                   eval      wsfl = 1
112200070427     C*
112300070427     C*  se richiesto Ordinamento sul SFL da operatore --> F11
112400070427     c                   setoff                                       010203
112500070427     c                   setoff                                       040506
112600070427     c                   setoff                                       07
112700070427     C*
112800070427      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
112900070427      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
113000070427      * Il campo è usato per selezionare i records dando un ordine a quelli
113100070427      * selezionati davanti ai non selezionati.
113200070427     C                   CLEAR                   QLGSCB
113300070427     C                   CLEAR                   QLGSCB00
113400070427      *
113500070427     c                   select
113600070427      *
113700070427      *?  Ordinamento per Destinatario    ?
113800070427     C                   when      WrkSort = Ord_DESTINAT
113900070427     c                   move      Ord_DESTINAT  wINDsort
114000080610      *
114100080820     ** Tenendo conto della visualizzazione prioritaria C/R
114200070511     C                   EVAL      QLGNBRK    = 2
114300070427
114400080820     ** La Priorità fra Consegne e Ritiri
114500080820     C                   EVAL      QLGSP   = 1
114600080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
114700080820     C                   EVAL      QLGDT    = Carattere
114800080820      * Priorità dei dati
114900080820     c                   If        Priorita_dati = 'R'
115000070511     C                   EVAL      QLGSO    = Discendente
115100080820     c                   else
115200080820     C                   EVAL      QLGSO    = Ascendente
115300080820     c                   end
115400080820      *
115500070427     C                   EVAL      QLGKL(1) = QLGSKL
115600070511
115700080820     ** Il DEST è in 2° posizione
115800080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2cr)
115900080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
116000070511     C                   EVAL      QLGDT    = Carattere
116100070511     C                   EVAL      QLGSO    = Ascendente
116200070511     C                   EVAL      QLGKL(2) = QLGSKL
116300070427      *
116400070427      *?  Ordinamento per Indirizzo/Destinatario    ?
116500070427     C                   when      WrkSort = Ord_INDIRIZZO
116600070427     c                   move      Ord_INDIRIZZO wINDsort
116700080610      *
116800070511     ** 3 campi chiave.   x INDIRIZZO/DESTINATARIO
116900070511     C                   EVAL      QLGNBRK    = 3
117000080820
117100080820     ** La Priorità fra Consegne e Ritiri
117200080820     C                   EVAL      QLGSP   = 1
117300080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
117400080820     C                   EVAL      QLGDT    = Carattere
117500080820      * Priorità dei dati
117600080820     c                   If        Priorita_dati = 'R'
117700080820     C                   EVAL      QLGSO    = Discendente
117800080820     c                   else
117900080820     C                   EVAL      QLGSO    = Ascendente
118000080820     c                   end
118100080820      *
118200080820     C                   EVAL      QLGKL(1) = QLGSKL
118300070427
118400080820     ** Il INDIRIZZO è in 3°posizione
118500080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
118600080820     c                                           %SIZE(S2DesM)
118700080820     C                   EVAL      QLGSS    = %SIZE(S2ind)
118800070427     C                   EVAL      QLGDT    = Carattere
118900070427     C                   EVAL      QLGSO    = Ascendente
119000070511     C                   EVAL      QLGKL(2) = QLGSKL
119100070427
119200080820     ** Il DEST è in 2°posizione
119300080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
119400080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
119500070427     C                   EVAL      QLGDT    = Carattere
119600070427     C                   EVAL      QLGSO    = Ascendente
119700070511     C                   EVAL      QLGKL(3) = QLGSKL
119800070427
119900070427      *?  Ordinamento per Località/Destinatario    ?
120000070427     C                   when      WrkSort = Ord_LOCALITA
120100070427     c                   move      Ord_LOCALITA  wINDsort
120200080610      *
120300070511     ** 3 campi chiave.   x LOCALITA'/DESTINATARIO
120400070511     C                   EVAL      QLGNBRK    = 3
120500080820
120600080820     ** La Priorità fra Consegne e Ritiri
120700080820     C                   EVAL      QLGSP   = 1
120800080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
120900080820     C                   EVAL      QLGDT    = Carattere
121000080820      * Priorità dei dati
121100080820     c                   If        Priorita_dati = 'R'
121200080820     C                   EVAL      QLGSO    = Discendente
121300080820     c                   else
121400080820     C                   EVAL      QLGSO    = Ascendente
121500080820     c                   end
121600080820      *
121700080820     C                   EVAL      QLGKL(1) = QLGSKL
121800070427
121900070427     ** La LOCALITA' è in posizione (xx) 20 Bytes char ascending
122000080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
122100080820     c                                           %SIZE(S2DesM) +
122200080820     c                                           %SIZE(S2IND)
122300080820     C                   EVAL      QLGSS    = %SIZE(S2lod)
122400070427     C                   EVAL      QLGDT    = Carattere
122500070427     C                   EVAL      QLGSO    = Ascendente
122600070511     C                   EVAL      QLGKL(2) = QLGSKL
122700070427
122800070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
122900080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
123000080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
123100070427     C                   EVAL      QLGDT    = Carattere
123200070427     C                   EVAL      QLGSO    = Ascendente
123300070511     C                   EVAL      QLGKL(3) = QLGSKL
123400080610      *
123500070427      *?  Ordinamento per Peso/Destinatario    ?
123600070427     C                   when      WrkSort = Ord_PESO
123700070427     c                   move      Ord_PESO      wINDsort
123800080610      *
123900080820     ** 3 campi chiave.   x PESO/DESTINATARIO
124000080820     C                   EVAL      QLGNBRK    = 3
124100080820
124200080820     ** La Priorità fra Consegne e Ritiri
124300080820     C                   EVAL      QLGSP   = 1
124400080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
124500080820     C                   EVAL      QLGDT    = Carattere
124600080820      * Priorità dei dati
124700080820     c                   If        Priorita_dati = 'R'
124800080820     C                   EVAL      QLGSO    = Discendente
124900080820     c                   else
125000080820     C                   EVAL      QLGSO    = Ascendente
125100080820     c                   end
125200080820      *
125300080820     C                   EVAL      QLGKL(1) = QLGSKL
125400070427
125500070427     ** Il PESO è in posizione (xx) 7 Digits Num descending
125600080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
125700080820     c                                           %SIZE(S2DesM) +
125800080820     c                                           %SIZE(S2IND) +
125900080820     c                                           %SIZE(S2LOD)
126000080820     C                   EVAL      QLGSS    = %SIZE(S2Peso)
126100070427     C                   EVAL      QLGDT    = Numerico
126200070427     C                   EVAL      QLGSO    = Discendente
126300080820     C                   EVAL      QLGKL(2) = QLGSKL
126400070427
126500070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
126600080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
126700080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
126800070427     C                   EVAL      QLGDT    = Carattere
126900070427     C                   EVAL      QLGSO    = Ascendente
127000080820     C                   EVAL      QLGKL(3) = QLGSKL
127100070427      *
127200070427      *?  Ordinamento per Volume/Destinatario    ?
127300070427     C                   when      WrkSort = Ord_VOLUME
127400070427     c                   move      Ord_VOLUME    wINDsort
127500070427      *
127600080820     ** 3 campi chiave.   x VOLUME/DESTINATARIO
127700080820     C                   EVAL      QLGNBRK    = 3
127800080820
127900080820     ** La Priorità fra Consegne e Ritiri
128000080820     C                   EVAL      QLGSP   = 1
128100080820     C                   EVAL      QLGSS    = %SIZE(S2CR)
128200080820     C                   EVAL      QLGDT    = Carattere
128300080820      * Priorità dei dati
128400080820     c                   If        Priorita_dati = 'R'
128500080820     C                   EVAL      QLGSO    = Discendente
128600080820     c                   else
128700080820     C                   EVAL      QLGSO    = Ascendente
128800080820     c                   end
128900080820      *
129000080820     C                   EVAL      QLGKL(1) = QLGSKL
129100070427
129200080820     ** Il VOLUME
129300080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)  +
129400080820     c                                           %SIZE(S2DesM) +
129500080820     c                                           %SIZE(S2IND) +
129600080820     c                                           %SIZE(S2LOD) +
129700080820     c                                           %SIZE(S2Peso)
129800080820     C                   EVAL      QLGSS    = %SIZE(S2Vol)
129900070427     C                   EVAL      QLGDT    = Numerico
130000070427     C                   EVAL      QLGSO    = Discendente
130100080820     C                   EVAL      QLGKL(2) = QLGSKL
130200070427
130300080820     ** Il DEST
130400080820     C                   EVAL      QLGSP   = 1 + %SIZE(S2CR)
130500080820     C                   EVAL      QLGSS    = %SIZE(S2DesM)
130600070427     C                   EVAL      QLGDT    = Carattere
130700070427     C                   EVAL      QLGSO    = Ascendente
130800080820     C                   EVAL      QLGKL(3) = QLGSKL
130900080610      *
131000070427     c                   endSL
131100070427      *------------
131200070427      * Load other sort parameters.
131300070427     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
131400070427     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
131500070427     C                   EVAL      QLGRT     = 8
131600070427     C                   EVAL      QLGOKL    = 80
131700070427     C                   EVAL      QLGLKE    = 16
131800070427     C                   EVAL      QLGLSS    = 290
131900070427
132000070427      * Initialize Sort I/O API fields.
132100070427     C                   EVAL      QLGRL00  = QLGRL
132200070427     C                   EVAL      QLGRC00  = 1
132300070427     C                   CLEAR                   QUSEI
132400070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
132500070427
132600070427      * First step - Initialize the sort routine.
132700070427     C                   CALL      'QLGSORT'
132800070427     C                   PARM                    QLGSCB
132900070427     C                   PARM                    NotUsed
133000070427     C                   PARM                    NotUsed
133100070427     C                   PARM                    SizeList
133200070427     C                   PARM                    ReturnSize
133300070427     C                   PARM                    QUSEC
133400070427
133500070427      * Next step - Write records to I/O routine.
133600070427     C                   EVAL      QLGRT00 = Put
133700070427
133800070503     C                   DO        RrnLast       S2nrr_xx          5 0
133900070503     C     S2nrr_xx      CHAIN     FDG0S02
134000070427
134100070427     ** Solo le righe con Selected = 'Y' sono riordinate,
134200070427     ** quindi per fare un ordinamento di tutte le righe
134300070427     ** metto 'Y' sempre.
134400070427     C                   EVAL      Selected  = 'Y'
134500070427
134600070427     C                   CLEAR                   QUSEI
134700070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
134800070427      *
134900070427     C                   CALL      'QLGSRTIO'
135000070427     C                   PARM                    QLGSCB00
135100070427     C                   PARM                    SflRcd
135200070427     C                   PARM                    NotUsed
135300070427     C                   PARM                    SizeList
135400070427     C                   PARM                    NotUsed
135500070427     C                   PARM                    QUSEC
135600070427
135700070427     C                   ENDDO
135800070427
135900070427      * Next step - Signal end of input, clear subfile for reload.
136000070427     C                   EVAL      QLGRT00 = EndPut
136100070427     C                   CLEAR                   QUSEI
136200070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
136300070427
136400070427     C                   CALL      'QLGSRTIO'
136500070427     C                   PARM                    QLGSCB00
136600070427     C                   PARM                    SflRcd
136700070427     C                   PARM                    NotUsed
136800070427     C                   PARM                    SizeList
136900070427     C                   PARM                    NotUsed
137000070427     C                   PARM                    QUSEC
137100070427      *
  pulizia SFL 2  ?
137200070427     C                   SETOFF                                         3031
137300070427     C                   WRITE     FDG0C02
137400070427     C                   SETON                                          31
137500070427      *
137600070427
137700070427      *
  Campo di Ordinamento in REVERSE ?
137800070427     c                   if        wINDsort  > 0
137900070427     C                   move      *on           *in(wINDsort)
138000070427     c                   end
138100070427
138200070427      * Final step - Write the records back to the subfile.
138300070427     C                   EVAL      QLGRT00 = Get
138400070427
138500070503     C                   DO        RrnLast       S2nrr_xx
138600070503     c                   eval      S2nrr = S2nrr_xx
138700070427     C                   CLEAR                   QUSEI
138800070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
138900070427     C                   CALL      'QLGSRTIO'
139000070427     C                   PARM                    QLGSCB00
139100070427     C                   PARM                    NotUsed
139200070427     C                   PARM                    SflRcd
139300070427     C                   PARM                    QLGRL00
139400070427     C                   PARM                    NotUsed
139500070427     C                   PARM                    QUSEC
139600070503      * SFLnxtCHG
139700080820     C                   SetOFF                                       32
139800070503     c                   if        s2sce <> *blank
139900070503     c                   seton                                        32
140000070503     c                   end
140100080820      **
140200130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
140300130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
140400130221     c*****h2xco         comp      ' '                                8888
140500130221     c     h2xco         comp      '0'                                88
140600070427     C                   WRITE     FDG0S02
140700080820      **
140800070427     C                   ENDDO
140900070427      *
141000070503      *  All'uscita di queste chiamate a routine di sistema il SFL record
141100070503      *   si incrementa stranamente quindi lo reimposto correttamente
141200070503      *    prima di lasciare la routine.
141300070503     C                   EVAL      S2nrr = RrnLast
141400070514     C*
141500070427     C                   ENDSR
141600070427     C/EJECT
141700080609      * ?-------------------------------------------------------------*?
141800080609      *
141900070326** ErrMsg
142000080819                                                                               01
142100080819                                                                               02
142200080819                                                                               03
142300080819                                                                               04
142400080819                                                                               05
142500080819                                                                               06
142600080819                                                                               07
142700080819                                                                               08
142800080819                                                                               09
142900080819                                                                               10
