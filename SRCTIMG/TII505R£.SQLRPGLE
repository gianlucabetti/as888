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
024500110916     C*   se è stato richiesto un numero d'ordine specifico
024600110916     C*   immesso nel sistema
024700110916     c                   if        d1nor > 0
024800110916     c                   eval      num_ordine = d1nor
024900110916     c                   eval      filiale    = d1cfgs
025000110919     c                   exsr      Chiama_Ordine
025100110916     c                   end
025200110916      *
025300110916     c                   end
025400110916      *
0255001109161-   C                   ENDSL
025600110902     C*
025700080606     C     Fine_GESD1    ENDSR
025800070306     C/EJECT
025900110726      *---------------------------------------------------------------*
026000070320      *?  INIZIALIZZAZIONE Primo video                           ?
026100110726      *---------------------------------------------------------------*
026200070227     C     INZD1         BEGSR
026300070227     C*
026400070227     C* Pulizia campi e indicatori
026500070308     c                   exsr      INZ_indERR
026600110726     C*
026700070227     C* Pulizia formato record
026800110725     c                   clear                   TII5D01
026900070227     C* campi titolo
027000070302     C                   MOVEL     dsPGM         D1CPGM
027100070227     C                   MOVEL     knsif         D1nsif
027200070227     C                   MOVEL     knmus         D1nmus
027300070227     C                   MOVEL     RSUT          D1crsu
027400110914     C                   MOVEL     Tutto         D1Torp
027500110914     C                   MOVEL     Tutto         D1UFFc
027600110824     C                   z-add     settimana     D1week
027700070227     C* campi video
027800110914     C                   MOVEL     *zeros        D1CFGS
027900110830     C                   z-add     woggi         D1cDDC
028000110915     C                   z-add     0             Num_Ordine        5 0
028100110915     C                   z-add     0             Data_Ordine       8 0
028200070418     C*
028300080606     C     END_INZ1      ENDSR
028400940127     C************************************************************
028500070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
028600940127     C************************************************************
028700070227     C     CTRD1         BEGSR
028800070606     C*
028900070227     C*  Spegne indicatori di errore
029000070308     c                   exsr      INZ_indERR
029100110915     c                   setoff                                       99
029200110902     C* --------------
029300110914     d* ?- Controlla la filiale in gestione
029400110726     C*  Controlla il P.O.
029500110916     C     d1cFGS        ifGT      0
029600070227     C     d1cFGS        CHAIN     AZORG01l
029700110725      * Non presente in organigramma
029800110725    1C                   IF        not %found(azorg01l)
029900070326     C                   SETON                                        51  99
030000110914     c                   movel     ErrMsg(2)     d1msg
030100070227     C                   goto      ENDCTR1
030200110901    1C                   else
030300110901     c                   eval      og143 = orgDE3
030400110915    1C                   ENDIF
030500110916    1C                   ENDIF
030600110902      * >>>>>>>>>>>>>
030700110902      ***  interroga quali Ordini sono stati immessi per la filiale
030800110916     c                   if        d1int = '?' and d1cfgs  = 0
030900110914     C                   SETON                                        515599
031000110914     c                   movel     ErrMsg(1)     d1msg
031100110914     C                   goto      ENDCTR1
031200110914     c                   end
031300110914     c                   if        d1int = '?' and d1cfgs <> 0
031400110902     c                   movel     kpjbu         kpjbus
031500110916     c                   clear                   tii502ds
031600110916     c                   movel     '1'           I502CONFER
031700110916     c                   z-add     d1cfgs        I502FILIAL
031800110916     c                   movel     d1torp        I502TIPORD
031900110916     c                   movel     d1uffc        I502UFFCOM
032000110916     c                   movel     *all'9'       I502WEEKAA
032100110916     c                   move      anno          I502WEEKAA
032200110916     c                   movel     tii502ds      kpjbu
032300110916     c                   call      'TII502R'
032400110916     c                   parm                    kpjba
032500110916     c                   movel     kpjbu         tii502ds
032600110916     c                   eval      d1int = *blank
032700110916     c                   move      o502numord    d1nor
032800110908     c                   if        d1nor = 0
032900110908     c                   SETON                                            99
033000110908     c                   end
033100110902     c                   movel     kpjbus        kpjbu
033200110902     c                   end
033300110902      ***
033400110908     c                   IF        d1nor > 0
033500110916     c     d1nor         chain     tmORP02L
033600110906     c                   if        not %Found(TmOrp02L)
033700110902     C                   SETON                                        54  99
033800110902     c                   movel     ErrMsg(9)     d1msg
033900110902     C                   goto      ENDCTR1
034000110902     c                   else
034100110914     C* se l'ordine non è stato chiuso
034200110914     c                   if        orpCONF = *blank
034300110902     C                   SETON                                        54  99
034400110902     c                   movel     ErrMsg(10)    d1msg
034500110902     C                   goto      ENDCTR1
034600110902     c                   else
034700110902     C*
034800110916     C                   Z-ADD     orpFIL        d1cfgs
034900110902     C                   Z-ADD     orpDOR        G02INV                         *AAAA/MM/GG
035000110902     C                   MOVEL     '3'           G02ERR
035100110902     C                   CALL      'XSRDA8'
035200110902     C                   PARM                    WLBDAT
035300110902     c                   Z-ADD     G02DAT        Data_Ordine
035400110902     C*
035500110902     c                   endif
035600110902     c                   endif
035700110902     c                   endif
035800110916     C*
035900110916      * Salva la filiale su campo di work x tutto il pgm
036000110916     c                   z-add     d1cfgs        Filiale
036100110916     C                   Z-ADD     D1cFGS        sav_d1cFGS
036200070307     C*---------------
036300070227     C     ENDCTR1       ENDSR
036400070319      *---------------------------------------------------------------*
036500110726      *  ?  GESTIONE SFL di immissione righe di ordine               ?
036600070319      *---------------------------------------------------------------*
036700110826     C     GESS0         BEGSR
036800070319     C*
036900940223     C* inizializzazione videata
037000110826     C     $INZS0        IFEQ      *ON
037100110826     C                   EXSR      INZS0
037200110826     C                   MOVE      *OFF          $INZS0
037300940127     C                   ENDIF
037400070327     C*
037500070306     C* Inizializza e imposta piede videata
037600110826     C                   EXSR      INZZ0
037700070306     C*
037800030113     C* emissione piede videata
037900070302      * ?            *-----------------------*
038000110826     C                   WRITE     TII5Z00
038100070302      * ?            *-----------------------*
038200070228     C*
038300940223     C     WMAX          IFEQ      0
038400070228     C* Non ci sono records
038500070302      * ?            *-----------------------*
038600110826     C                   WRITE     TII5V00
038700070302      * ?            *-----------------------*
038800070301     c                   setoff                                       30
038900030114     C                   Else
039000070301     c                   seton                                        30
039100070330     c                   eval       sav_WSfl =  WSfl
039200070330     c                   eval       sav_Wpag =  Wpag
039300070301      *
039400030114     C     Wsfl          IFgt      0
039500110826     C                   Z-ADD     wsfl          C0RCD
039600030114     C                   Else
039700030114     C     Wpag          IFgt      0
039800110826     C                   Z-ADD     wpag          C0RCD
039900030114     C                   EndIF
040000030114     C                   EndIF
040100030114     C                   ENDIF
040200070228      *
040300070228      *  Emissione SFL dei GIRI
040400070228      * ?            *-----------------------*
040500110826     C                   EXFMT     TII5C00
040600070228      * ?            *-----------------------*
040700070326     c                   setoff                                       99
040800110826     C                   movel     'S0'          sav$GEST
040900070228      *
041000110826     C     C0NRR         IFNE      0
041100110826     C                   Z-ADD     C0NRR         WSFL
041200940204     C                   ENDIF
041300110826     C                   Z-ADD     SFLNRR        C0RCD
041400030113     C* Selezioni
0415009401271    C                   SELECT
041600070328     C* F3=Fine
041700070328     C     $TASTO        WHENEQ    F03
041800070328     C                   EXSR      F03_exit
041900070328     C* F12=Ritorno
042000070328     C     $TASTO        WHENEQ    F12
042100070328     c                   move      'D1'          $GEST
042200070328     C                   MOVE      *ON           $INZD1
042300070328      *
0424009401271O   C                   OTHER
042500070228     C* CONTROLLO testata
042600110826     C                   EXSR      CTRC0
042700110915      *
042800110915     C* CONTROLLO subfile sulle scelte eseguite
042900940201     C     *IN99         IFEQ      *OFF
043000110915     C     $TASTO        andeq     F06
043100110826     C                   EXSR      CTRS0
043200940131     C                   END
043300070327      *
043400110915      * ? Conferma gli Ordini selezionati
043500110830     C     *IN99         IFEQ      *OFF
043600110915     C     $TASTO        andeq     F06
043700110830     c     ERRmsg80      andEQ     *blank
043800110830     C     WMAX          andGT     0
043900110915     C                   MOVE      *ON           $INZS0
044000110915     C                   movel     'S0'          $GEST
044100070314     c                   end
044200070228      *
0443009401271-   C                   ENDSL
044400940127     C*
044500940127     C                   ENDSR
044600070328     C/EJECT
044700070328      *---------------------------------------------------------------*
044800070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
044900070328      *---------------------------------------------------------------*
045000110826     C     INZZ0         BEGSR
045100070328     C*
045200070306     C* Messaggio o tasti funzionali
045300110826     c                   clear                   z0dmsg
045400110826     c                   clear                   ERRmsg80         80
045500070307     C*
045600070328      * imposta eventuali errori
045700110826     c                   eval      z0dmsg = ERRmsg80
045800110906     C*
045900070306     C                   ENDSR
046000070306     C/EJECT
046100070306      *---------------------------------------------------------------*
046200070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
046300070306      *---------------------------------------------------------------*
046400110826     C     INZS0         BEGSR
046500070306     C*
046600070228     C* reset indicatori DSPATR
046700070308     c                   exsr      INZ_indERR
046800940302     C* pulizia SFL
046900940128     C                   SETOFF                                         3031
047000110826     C                   WRITE     TII5C00
047100940128     C                   SETON                                          31
047200940128     C*
047300110826     C* Righe x Pagina del 0° SFL
047400110826     c                   z-add     RigPag_SFl0   s0pag
047500070228      *
047600030113     C* CARICAMENTO SFL totale
047700110826     C                   Z-ADD     0             S0NRR
047800110826     C                   Z-ADD     1             C0RCD
047900940128     C                   Z-ADD     0             WMAX
048000070302     C                   Z-ADD     0             Wsfl
048100070302     C* campi titolo
048200110826     C                   MOVEL     dsPGM         c0CPGM
048300110826     C                   MOVEL     knsif         c0nsif
048400110826     C                   MOVEL     knmus         c0nmus
048500110826     C                   MOVEL     RSUT          c0crsu
048600110826     C                   z-add     settimana     c0week
048700110830     C*
048800110725      **----------------
048900110725     C* Posizionamento su file pilota
049000110825     C                   eval        $EFILE    = *OFF
049100110915     C     d1cfgs        setll     tmORP01L
049200110725      **----------------
049300030113     C* Carico il SFL
049400110826     C                   EXSR      ROLS0
049500030113     C*
049600030114     C                   Z-ADD     1             WPAG
049700110726     c                   eval       sav_Wmax =  Wmax
049800110725     C*
049900110725     C                   ENDSR
050000110725     C/EJECT
050100110725      *---------------------------------------------------------------*
050200070320      *? CARICAMENTO PAGINA LISTA                                    ?
050300070305      *---------------------------------------------------------------*
050400110826     C     ROLS0         BEGSR
050500070305     C*
050600070305     C                   SETOFF                                       32
050700110831     C                   Z-ADD     1             Y
050800110826     C                   Z-ADD     WMAX          S0NRR
050900070305     C*
051000070305     C* Caricamento del Sfl x Giro
051100110826     c                   Exsr      Write_S0
051200070305     C*
051300110826     C                   Z-ADD     S0NRR         WMAX                 30
051400070305     C*
051500110826     C* POSIZIONAMENTO AL 0° RCD DELLA PAGINA
051600110826     C     S0NRR         DIV       S0pag         PAGINE            4 0
051700070305     C                   MVR                     RESTO             3 0
051800110826     C     PAGINE        MULT      S0pag         C0RCD
0519000703051    C     RESTO         IFGT      0
052000110826     C                   ADD       1             C0RCD
0521000703051E   C                   ELSE
052200110826     C                   SUB       S0pag         C0RCD
052300110826     C                   ADD       1             C0RCD
0524000703051-   C                   ENDIF
052500070305     C*
052600070305     C                   ENDSR
052700070301      *---------------------------------------------------------------*
052800070320      *?  Scrive x Ogni rottura di GIRO                              ?
052900070301      *---------------------------------------------------------------*
053000110826     C     Write_S0      BEGSR
053100070305      *
053200110826      *  Precarica da Ordine Pianificato
053300110826     c                   exsr      Legge_rec0
053400070228      *
053500070228      * Finchè il giro è lo stesso
0536001108311    C     $EFILE        DowEQ     *OFF
053700110826     C                   clear                   TII5S00
053800070228      *
053900110830      * Solo x ordini mensili deve impostare le sottoclassi
054000110830      *  degli articoli pianificati preselezionate con "1" e proteggere
054100110830      *   altre scelte.
054200110826     c                   exsr      carica_campi0
054300070305      *---------
054400110826     C                   ADD       1             S0NRR
054500070305     C                   ADD       1             Y
054600110826     C                   WRITE     TII5S00
054700070305      *---------
054800110826     c                   exsr      Legge_rec0
054900110825     C*
055000070228     c                   EndDO
055100070228     C*
055200070228     C                   ENDSR
055300110825      *---------------------------------------------------------------*
055400110825      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
055500110825      *---------------------------------------------------------------*
055600110826     C     LEGGE_REC0    BEGSR
055700110825     C*
055800110825     C*  Per EoF deve uscire
055900110825     C*  Per Record Ok deve uscire riportando i dati
056000110825     C                   MOVEL     *OFF          $EFILE
056100110825     C                   MOVEL     *OFF          $RCDOK
056200110825     C*
0563001108251    C     $EFILE        DOUEQ     *ON
056400110825     C     $RCDOK        OREQ      *ON
056500110825     C*
056600110826      * ? Se Ordine Pianificato deve caricare da file precaricato
056700110825     c****   LETTURA RECORD x Filiale
056800110915     c                   read      tmORP01L
056900110825      * Fine File
057000110915     C                   IF        %Eof(tmORP01L)
057100110825     C                   MOVEL     *ON           $EFILE
057200110915     C                   MOVE      *ON           *IN33
057300110825     C                   Else
057400110825      * ?Controlla se la Spedizione è da considerare
057500110825     C                   MOVEL     *ON           $RCDOK
057600110826      *
057700110826     C                   exsr      Check_RECord0
057800110825     C                   END
057900110825     C*
058000110915     C* va al nuovo ordine  se ha scartato il precedente
058100110915     c     kTmORP        setgt     tmORP01L
058200110915     C*
0583001108251-   C                   ENDDO
058400110825     C*
058500110825     C                   ENDSR
058600110825      *---------------------------------------------------------------*
058700110825      *?   CONTROLLO il record letto se va bene                      ?
058800110825      *---------------------------------------------------------------*
058900110826     C     Check_Record0 BEGSR
059000110825     C*
059100110915     C*  Controlla il tipo di ordine
059200110915     c                   if        D1TORP <> Tutto and
059300110915     c                             D1TORP <> orpPIAN
059400110830     C                   MOVEL     *OFF          $RCDOK
059500110830     C                   END
059600110825     C*
059700110915     C*  Controlla chi ha fatto l'ordine
059800110915     c                   if        D1uffC <> Tutto and
059900110915     c                             D1uffC <> orpUFF
060000110915     C                   MOVEL     *OFF          $RCDOK
060100110915     C                   END
060200110915     C*
060300110915     C*  se non è stato confermato o è stato sospeso
060400110916     c                   if        orpCONF <>'1'     or
060500110915     c                             orpATT  <> *blank
060600110915     C                   MOVEL     *OFF          $RCDOK
060700110915     C                   END
060800110915     C*
060900110915     C*  Se richiesta una filiale occorre verificare che il record letto
061000110915     C*   non sia già sulla filiale successiva
061100110915     C*  Come se fosse arrivato a Fine File
061200110915     c                   if        D1cfgs  > 0     and
061300110915     c                             D1cfgs <  orpFIL
061400110915     C                   MOVEL     *ON           $EFILE
061500110915     C                   MOVE      *ON           *IN33
061600110915     C                   END
061700110915     C*
061800110901     C*
061900110826     C     end_Check0    ENDSR
062000070301      *---------------------------------------------------------------*
062100110915      *?  Carica gli Ordini da Confermare ordinati per Filiale       ?
062200070301      *---------------------------------------------------------------*
062300110826     C     Carica_campi0 BEGSR
062400110726     C*
062500110915     c                   move      *blank        s0sce
062600110915     c                   eval      s0NOR    =  orpNOR
062700110915     c                   eval      h0DOR    =  orpDOR
062800110916     C*
062900110916     C                   Z-ADD     h0DOR         G02INV                         *AAAA/MM/GG
063000110916     C                   MOVEL     '3'           G02ERR
063100110916     C                   CALL      'XSRDA8'
063200110916     C                   PARM                    WLBDAT
063300110916     c                   Z-ADD     G02DAT        Data_Ordine
063400110916     c                   Z-ADD     G02DAT        s0DOR
063500110916      **
063600110915     c                   eval      s0PIAN   =  orpPIAN
063700110915     c                   eval      s0FIL    =  orpFIL
063800110830     C*
063900110726     C                   ENDSR
064000110726      *---------------------------------------------------------------*
064100110726      *?   CONTROLLO TESTATA LISTA                                   ?
064200110726      *---------------------------------------------------------------*
064300110826     C     CTRC0         BEGSR
064400110726     C*
064500070301     C                   MOVE      *OFF          *IN99
064600110726     C*
064700070301     C                   ENDSR
064800070301      *---------------------------------------------------------------*
064900070320      *?   CONTROLLO OPZIONI LISTA                                   ?
065000070301      *---------------------------------------------------------------*
065100110826     C     CTRS0         BEGSR
065200070301     C*
065300070301     C                   MOVEL     *OFF          $ESCI
065400110830     c                   clear                   ERRmsg80
065500070301     C                   SETOFF                                       99
065600070301     C*
065700070301     C* Leggo il sfl solo se ci sono rcd
0658000703011    C     WMAX          IFGT      0
065900110826     C                   READC     TII5S00                                21
066000070301     C*
066100070301     C* esce se fine sfl o errore che richiede l'uscita
0662000703012    C     *IN21         DOWEQ     *OFF
066300070301     C     $ESCI         ANDEQ     *OFF
066400110826     C                   Z-ADD     S0NRR         C0RCD
066500070301     C* ctrl su riga
066600110826     C                   EXSR      RECS0
066700070301      *
066800070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0669000703013    C     *IN99         IFEQ      *ON
067000070301     C     $ESCI         OREQ      *ON
067100070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
067200070301     C* la ripristinerò a conclusione del ciclo di READC
067300110826     C                   MOVE      *OFF          $INZS0
0674000703013-   C                   ENDIF
067500070301     C*
067600070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
067700070301     C                   MOVE      *ON           *IN32
067800110826     C                   UPDATE    TII5S00
067900070301     C*
068000070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0681000703013    C     $ESCI         IFEQ      *OFF
068200110826     C                   READC     TII5S00                                21
068300070301      *
068400070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0685000703014    C     *IN21         IFEQ      *ON
068600070301     C* calcolo pagina a cui deve posizionarsi
068700110826     c                   z-add     s0pag         Wpagine
068800070301     C                   EXSR      CLCPAG
0689000703014-   C                   ENDIF
0690000703013-   C                   ENDIF
069100070301     C*
0692000703012-   C                   ENDDO
069300070301     C*
0694000703011-   C                   ENDIF
069500110726     C* __________________________________________________
069600110826     C*  errori da evidenziare tramite finestra di ALLERTA
069700110826     c                   exsr      Errori
069800110726     C* __________________________________________________
069900070328     C*
070000070301     C                   ENDSR
070100070301     C/EJECT
070200070301      *---------------------------------------------------------------*
070300070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
070400070301      *---------------------------------------------------------------*
070500110826     C     RECS0         BEGSR
070600070301     C*
070700070301     C* reset indicatori DSPATR
070800070308     c                   exsr      INZ_indERR
070900070326     C*
071000110830     c                   setON                                        32
071100110826     C* gestione opzioni
0712001108263    C     S0sce         IFNE      *blank
071300110826     C     *IN99         ANDEQ     *OFF
071400110826      *
071500110826     C                   EXSR      SCE_S0
071600110915      *
071700110830     c                   setOFF                                       32
071800110830     c                   clear                   s0sce
071900110826      *
0720001108263-   C                   ENDIF
072100110826     C*
072200110826     C     endSF0        ENDSR
072300110826      *---------------------------------------------------------------*
072400110826      *?   GESTIONE OPZIONI LISTA                                    ?
072500110826      *---------------------------------------------------------------*
072600110826     C     SCE_S0        BEGSR
072700110826     C*
072800110919     C*  Selezionata la riga
072900110919     c                   If        s0SCE <> *blank
073000110916     c                   eval        num_ordine  = s0NOR
073100110916     c                   eval        Data_Ordine = s0dor
073200110916     c                   eval        filiale    = s0fil
073300110919     c                   exsr      Chiama_Ordine
073400110915     c                   endIf
073500110826     C*
073600110919     c                   eval        s0sce = *blank
073700110919     C*
073800110826     C                   ENDSR
073900110826     C/EJECT
074000110826      *---------------------------------------------------------------*
074100110826      *?  GESTIONE F03 x uscire LR                                   ?
074200110826      *---------------------------------------------------------------*
074300110826     C     F03_exit      BEGSR
074400110909      **
074500110906     C                   MOVE      *ON           $FINE
074600110826      *
074700110826     C                   ENDSR
074800110826     C/EJECT
074900110825      *---------------------------------------------------------------*
075000110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
075100110825      *---------------------------------------------------------------*
075200110825     C     CLCPAG        BEGSR
075300110825     C* Input :
075400110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
075500110825     C*          cursore
075600110825     C* - Wpagine= numero rcd per pagina sfl
075700110825     C* Output :
075800110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
075900110825     C*
076000110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
076100110825     C                   MVR                     RESTO             3 0
076200110825     C     RESTO         IFGT      0
076300110825     C                   ADD       1             PAGINE
076400110825     C                   ENDIF
076500110825     C     PAGINE        MULT      Wpagine       WPAG
076600110825     C*
076700110825     C                   ENDSR
076800110825     C************************************************************
076900110825      *?  INIZIALIZZA indicatori di errore                           ?
077000110825     C************************************************************
077100110825     C     INZ_indERR    BEGSR
077200110825      *
077300110825     C                   move      *ALL'0'       IN5190           40
077400110825     C                   moveA     IN5190        *IN(51)
077500110825      *
077600110825     C                   ENDSR
077700070301     C/EJECT
077800070301      *---------------------------------------------------------------*
077900070316      *  ?  OPERAZIONI INIZIALI      ?
078000070301      *---------------------------------------------------------------*
078100940131     C     *INZSR        BEGSR
078200110726     C*
078300110908     C*  chiavi x files.
078400110908     C*
078500110906     c     kTmORP        klist
078600110824     C                   KFLD                    orpFIL
078700110905     C                   KFLD                    orpUFF
078800110824     C                   KFLD                    orpANNO
078900110824     C                   KFLD                    orpWEEK
079000110905     C                   KFLD                    orpPIAN
079100110915     C                   KFLD                    orpNOR
079200110830     C*
079300030113     C* Reperimento parametri
079400030113     C     *ENTRY        PLIST
079500030113     C                   PARM                    KPJBA
079600070228      *
079700110726      *  Se è EDP
079800070308     c                   CLEAR                   se_EDP            1
079900070308     c                   if        %subst(Knmus:1:3) = 'EDP'
080000070308     c                   MOVE      'S'           se_EDP
080100070307     c                   end
080200070305     C*
080300070227      * Reperisco dati job
080400070227     c                   exsr      DatiJob
080500110901     C*  Controlla il P.O.
080600110901     C     d1cFGS        CHAIN     AZORG01l
080700110901      * Non presente in organigramma
080800110901    1C                   IF        %found(azorg01l)
080900110901     c                   eval      og143 = orgDE3
081000110901     c                   if        §ogNTW = 'LOG'
081100110901     c                   end
081200110901    1C                   ENDIF
081300110901     C*
081400030113     C* Variabili per gestione videate
081500030113     C                   MOVE      *BLANK        $GEST             2
081600070305     C                   MOVE      *BLANK        sav$GEST          2
081700110906     C                   MOVE      *off          $FINE             1
081800110906     C                   MOVE      *off          dopo_in_$FINE     1
081900110906     C                   MOVE      *off          $INZD1            1
082000110906     C                   MOVE      *off          $INZS0            1
082100110906     C                   MOVE      *off          $EFILE            1
082200110906     C                   MOVE      *off          $ESCI             1
082300110906     C                   MOVE      *off          $RCDOK            1
082400030113     C* Indici
082500030113     C                   Z-ADD     0             X                 3 0
082600030113     C                   Z-ADD     0             Y                 3 0
082700070227     C***
082800070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
082900070227     C                   TIME                    W0140            14 0
083000110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
083100110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
083200070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
083300070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
083400070227     C*
083500110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
083600070227     C                   MOVEL     *BLANK        G02ERR
083700070227     C                   CALL      'XSRDA8'
083800070227     C                   PARM                    WLBDAT
083900110825     c                   Z-ADD     G02INV        WoggiAMG          8 0          *AAAA/MM/GG
084000110825     c                   eval      dataiso = %date(WoggiAMG:*iso)
084100110824     c                   eval      dataeur = dataiso
084200110825     c                   eval      oggiiso = dataiso
084300110824     c                   clear                   settimana         2 0
084400110824     C*
084500110824     C* NR.Settimana in corso
084600110824     C/EXEC SQL
084700110831     C+ SELECT week_iso(:OGGIISO) INTO :settimana FROM tmAMP00F
084800110824     C/END-EXEC
084900100505     C*
085000940117     C                   ENDSR
085100070227      *---------------------------------------------------------------*
085200070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
085300070227      *---------------------------------------------------------------*
085400070228     c     DatiJOB       BEGSR
085500070227      *
085600070227     c     *dtaara       define    §azute        azuteds
085700070227     c     *dtaara       define    §datiute      ddatiute
085800070227      *
085900070227     c                   in(E)     *dtaara
086000070227     c                   IF        %ERROR or RSUT = *blanks
086100070227     c                   clear                   Tibs34Ds
086200070227     c                   call      'TIBS34R'
086300070227     c                   parm                    Tibs34Ds
086400070227     c                   in        *dtaara
086500070227     c                   ENDIF
086600080606      *
086700070227     c                   ENDSR
086800110825     C/EJECT
086900110826      * ?-------------------------------------------------------------*?
087000110826      *?   SUBROUTINE Autorizzazione                                 ?
087100110826      * ?-------------------------------------------------------------*?
087200110826     C     GESAU         BEGSR
087300110826      *
087400110826     C* Se sarà un profilo Abilitato, imposta l'Entrata dal Primo Video
087500110826     C                   MOVE      'D1'          $GEST
087600110826     C*
087700110826      * ?- Controlla autorizzazione sull'utente
087800110826     c                   movel     dutpou        i70pog
087900110826     C* Il p.o.deve essere impostato in base al profilo
088000110826     C     dutlpo        IFEQ      '2'
088100110826     C                   MOVEL     dutpou        i70pge                         *FIL.GESTIONE
088200110826     C                   ELSE
088300110826     C                   MOVEL     SIMFEL        i70pge
088400110826     C                   END
088500110826     c                   movel     knmus         i70prf
088600110826     c                   call      'FICN70R'
088700110826     c                   parm                    ficn70ds
088800110826      *
088900110826     c                   movel     o70uni        daut
089000110826      *
089100110916     c* utente NON abilitato alla gestione magazzino OPPURE IN Filiale
089200110916     c                   if        §autmag <>'S' or dutLPO<>'S'
089300110826      *
089400110906     c                   eval      Err001 = ' '
089500110906     c                   eval      Err002 = ErrMsg(3)
089600110826     c                   eval      Err003 = ' '
089700110826      *   Messaggio errore a Finestra
089800110906     C                   MOVE      *on           dopo_in_$FINE
089900110826     C                   MOVE      'W1'          $GEST
090000110826      *
090100110826     c                   end
090200110826     c*
090300110826     C     endAU         ENDSR
090400110906     C************************************************************
090500110906      *?  Emissione Finestra Errori                              ?
090600110906     C************************************************************
090700110906     C     GESW1         BEGSR
090800110906     C*
090900110826     C                   eval      $FINE = dopo_in_$FINE
091000110826     C                   eval      $GEST = sav$GEST
091100110906     c                   eval      dopo_alert_window    = *blank
091200110826     c*
091300110826     c                   exfmt     TII5WER
091400110906     c*
091500110906     c                   eval         Err001 = *blank
091600110906     c                   eval         Err002 = *blank
091700110906     c                   eval         Err003 = *blank
091800110906     c                   eval       dopo_alert_window    ='S'
091900110906     c*
092000110906     C* F3=Fine - Uscita Forzata
092100110906     C                   if        $TASTO = F03 and $FINE = *off
092200110906     C                   exsr      F03_exit
092300110906     c                   endIF
092400110826     c*
092500110826     c* emesso l'errore torna al video che lo aveva generato
092600110826     c*  oppure va fuori dal programma  in base a $FINE e $GEST
092700110826     c*
092800110826     C                   ENDSR
092900110916      * ?-------------------------------------------------------------*?
093000110916     C*   Routine di Revisione dell'ordine
093100110916      * ?-------------------------------------------------------------*?
093200110919     C     Chiama_Ordine BEGSR
093300110916     C*
093400110916     c                   eval      kpjbus =  kpjbu
093500110919     c                   clear                   tii506ds
093600110919     c***
093700110919     C*  Selezionata la riga
093800110919     c                   If        s0SCE = '1'
093900110919     c                   eval      I506CONFER  = 'C'
094000110919     C*  Revisione dell'ordine
094100110919     c                   elseIF    s0SCE = '2'
094200110919     c***  invia il flag I506CONFER a Blank per revisionare l'ordine
094300110919     c                   end
094400110919     c***
094500110916     c                   eval      I506FILIAL  = Filiale
094600110916     c                   eval      I506NUMORD  = num_ordine
094700110916     c                   eval      I506DATAOR  = Data_Ordine
094800110916     c                   eval      I506WEEK    =settimana
094900110916     c                   eval      I506ANNO    =anno
095000110916     c                   eval      kpjbu  =  tii506ds
095100110916     c                   call      'TII506R'
095200110916     c                   parm                    kpjba
095300110916     c                   eval      tii506ds = kpjbu
095400110916     c                   eval      kpjbu  =  kpjbus
095500110916     c*
095600110916     C                   ENDSR
095700110916      * ?-------------------------------------------------------------*?
095800110916     C*   Routine degli errori da visualizzare sui SFL
095900110916      * ?-------------------------------------------------------------*?
096000110916     C     Errori        BEGSR
096100110916     C*
096200110906     C*  Messaggio di avviso    SE deve emettere un messaggio
096300110906     c                   if        err001 <> *blank or
096400110906     c                             err002 <> *blank or
096500110906     c                             err003 <> *blank
096600110826     C*
096700110826     c                   seton                                        99
096800110826      * ?            *-----------------------*
096900110826     C                   exfmt     TII5WER
097000110826      * ?            *-----------------------*
097100110826     C*
097200110909     c                   clear                   err001
097300110909     c                   clear                   err002
097400110909     c                   clear                   err003
097500110909     C*
097600110826     C*  Imposta per il dopo emissione errore
097700110826     C                   eval      $FINE = dopo_in_$FINE
097800110826     C                   eval      $GEST = sav$GEST
097900110906     C*
098000110906     c                   end
098100110826     C*
098200110826     c                   ENDSR
098300110826      *---------------------------------------------------------------------------------
098400080609      *
098500070326** ErrMsg
098600110914L'interrogazione Ordini è possibile nell'ambito di una filiale.                01
098700110914Filiale errata o inesistente.                                                  02
098800110901L'Utente non è autorizzato alla procedura di Filiale.                          03
098900110726Scelta obbligatoria                                                            04
099000110916                                                                               05
099100110916                                                                               06
099200110902L' Articolo non è presente in anagrafica                                       07
099300110902L' Articolo non appartiene all'ufficio di competenza                           08
099400110916                                                                               09
099500110914L'Ordine non è disponibile in quanto NON confermato.                           10
099600110916                                                                               11
099700110916                                                                               12
099800110916                                                                               13
099900110916                                                                               14
100000110916                                                                               15
100100110725                                                                               16
100200110725                                                                               17
100300110725                                                                               18
100400110726                                                                               19
100500110726                                                                               20
100600110726                                                                               21
100700110726                                                                               22
100800110726                                                                               23
100900110726                                                                               24
101000110726                                                                               25
101100110726                                                                               26
101200110726                                                                               27
101300110726                                                                               28
101400110726                                                                               29
101500110726                                                                               30
101600110726                                                                               31
101700110725                                                                               32
101800110726                                                                               33
101900110725                                                                               34
102000070829                                                                               35
102100070829                                                                               36
102200070829                                                                               37
102300070829                                                                               38
102400070829                                                                               39
102500110726Il valore immesso per il campo non è valido.                                   40
