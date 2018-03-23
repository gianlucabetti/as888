000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400110725      * TII500R *----------------------------------------------------*
000500110725      *     ?   - GESTIONE ORDINI MAGAZZIONO -    ?
000600070403      *--------------------------------------------------------------*
000700110725      * Occorre verificare che l'utente sia abilitato ad utilizzare il pgm.
000800110725      * Il programma può caricare ordini già preconfezionati archiviati
000900110725      * su un file appositamente preparato oppure può inserire righe d'ordine
001000110725      * per soddisfare altre esigenze delle filiali nel riapprovigionamento
001100110725      * dei materiali dalla Logistica.
001200070227      *--------------------------------------------------------------*
001300110725      * Il n°d'ordine è univoco.
001400110725      *--------------------------------------------------------------*
001500940307      *  21           GENERICO OPERAZIONI I/O
001600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001700070301      *  23           GENERICO Operazioni varie
001800940224      *  30           SFLDSP
001900940224      * N31           SFLCLR
002000940224      *  31           SFLDSPCTL
002100940224      *  32           SFLNXTCHG
002200940224      *  33           SFLEND
002300940224      *  39           OF PRTF
002400070302      *  40 <---> 49  DSPATR SU video
002500940608      *  Specificare l'uso dei singoli indicatori
002600070227      *  50           Emissione della window totale/parziale
002700070227      *  51 <---> 90  ERRORI SU VIDEO
002800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002900070829      *  98           ERRORE SPECIALE :
003000940224      *  99           INDIC. GENERALE DI ERRORE
003100940128     F*----------------------------------------------------*
003200070404      *  Output
003300110725     FTIorp01L  uF a E           K DISK    commit infds(orpds)
003400080609      *
003500070404      *  Input
003600110824     FtnTBE01l  if   e           k disk
003700110826     FanTAB01l  if   e           k disk
003800110726     FtMAMP01l  if   e           k disk
003900110725     Ftii500d   CF   E             WORKSTN
004000110725     F                                     SFILE(TII5S01:S1NRR)
004100940201     F                                     INFDS(DSFMT)
004200070305     D*----------------------------------------------------*
004300070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004400070326     D*----------------------------------------------------*
004500940211     D* Passaggio Parametri
004600940211     D KPJBA         E DS
004700070227     D KPJBUs          s                   like(KPJBU)
004800110726     D*-------------
004900110726     d ficn70ds      e ds
005000110826     d antab155      e ds
005100110826     d antab156      e ds
005200940211     D*-------------
005300110824     d dsWEK         e ds
005400110825     D  weekWEK                       2  0 DIM(54)
005500110824     D*-------------
005600940211     D DSFMT           DS           512
005700940506     D  $TASTO               369    369
005800940211     D  NRG                  370    370
005900940211     D  NCL                  371    371
006000940211     D  SFLNRR               378    379B 0
006100940127     D*-------------
006200940127     D* Reperimento nome PGM
006300940127     D STATUS         SDS           333
006400940127     D  DSPGM            *PROC
006500030113     D*-------------
006600110725     d orpds           ds
006700110725     d  orp_nrr              397    400b 0
006800110725     D*-------------
006900110725     D*--- DATA AREA NUMERAZIONE ORDINI :
007000110725     D DTAORC          DS
007100110725     D  ORDNUO                 1      5  0
007200070320     D*-------------
007300070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
007400110826$003 D S0NRR           S                   Like(C0rcd)
007500110826$003 D S0pag           S                   Like(C0rcd)
007600070227$003 D S1NRR           S                   Like(C1rcd)
007700070228$003 D S1pag           S                   Like(C1rcd)
007800070302$003 D WSfl            S                   Like(C1nrr)
007900030113$003 D Wmax            S                   Like(C1rcd)
008000030113$003 D Wpag            S                   Like(C1rcd)
008100070330$003 D sav_WSfl        S                   Like(WSfl)
008200070330$003 D sav_Wmax        S                   Like(Wmax)
008300070330$003 D sav_Wpag        S                   Like(Wpag)
008400070301$003 D Wpagine         S                   Like(C1rcd)
008500110824      *--------------------------
008600110826     D RigPag_SFl0     S              3  0 INZ(18)
008700110826     D RigPag_SFl1     S              3  0 INZ(18)
008800070227      *--------------------------
008900110826     D sav_d0cfgs      S                   Like(d0cfgs) inz(0)
009000070418     D sav_d1cfgs      S                   Like(d1cfgs) inz(0)
009100110826$003 D sav_S0NRR       S                   Like(s0nrr)
009200110826$003 D sav_S1NRR       S                   Like(s1nrr)
009300101129      *--------------------------
009400110726     d dataiso         s               d   datfmt(*iso)
009500110825     d oggiiso         s               d   datfmt(*iso)
009600101129     d dataeur         s               d   datfmt(*eur)
009700070320      *--------------------------
009800110824     d wFGS            s                   like(d1cFGS)
009900070307     d kCOD1           s                   like(TBEcod)
010000070307     d kKEY1           s                   like(TBEke1)
010100070305      *--------------------------
010200070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010300070305     D WLBDAT          DS                  INZ
010400070305     D  G02DAT                 1      8  0
010500070227     D  G02INV                 9     16  0
010600070227     D  G02ERR                17     17
010700070227     D  G02TGI                18     22  0
010800070307      *
010900070307     D DATPAR          DS
011000070307     D  GIODAT                 1      8  0
011100070307     D  GIOINV                 9     16  0
011200070307     D  GIOTGI                17     21  0
011300070307      *
011400070307     d Wdata8          DS
011500070307     d  dadata                 1      8  0
011600070307     d  adata                  9     16  0
011700070307      *--------------------------------
011800070227     d AZUTEds       e ds                  extname(AZUTE00F)
011900070227     d DDatiUte      e ds
012000110725      *--------------------------
012100070320     d* Ds per dati organigramma
012200070404      *------
012300100505     d DTAISO          s               D
012400070320      *--------------------------
012500070320     d* Parametri x Controllo profilo utenti
012600070320     d TIBS34ds      e ds
012700070227      *--------------------------
012800070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
012900070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
013000070227      *--------------------------
013100070320      *?  COSTANTI                                      ?
013200070320      *--------------------------
013300940506     D* Tasti di funzione
013400940506     D F01             C                   CONST(X'31')
013500940506     D F02             C                   CONST(X'32')
013600940506     D F03             C                   CONST(X'33')
013700940506     D F04             C                   CONST(X'34')
013800940506     D F05             C                   CONST(X'35')
013900940506     D F06             C                   CONST(X'36')
014000940506     D F07             C                   CONST(X'37')
014100940506     D F08             C                   CONST(X'38')
014200940506     D F09             C                   CONST(X'39')
014300940506     D F10             C                   CONST(X'3A')
014400940506     D F11             C                   CONST(X'3B')
014500070315     D F12             C                   CONST(X'3C')
014600940506     D F13             C                   CONST(X'B1')
014700940506     D F14             C                   CONST(X'B2')
014800940506     D F15             C                   CONST(X'B3')
014900940506     D F16             C                   CONST(X'B4')
015000940506     D F17             C                   CONST(X'B5')
015100940506     D F18             C                   CONST(X'B6')
015200940506     D F19             C                   CONST(X'B7')
015300940506     D F20             C                   CONST(X'B8')
015400940506     D F21             C                   CONST(X'B9')
015500940506     D F22             C                   CONST(X'BA')
015600940506     D F23             C                   CONST(X'BB')
015700940506     D F24             C                   CONST(X'BC')
015800940506     D ENTER           C                   CONST(X'F1')
015900940506     D ROLDWN          C                   CONST(X'F4')
016000940506     D ROLLUP          C                   CONST(X'F5')
016100070301     d digits          c                   '0123456789'
016200110825     d Periodico       c                   'P'
016300110825     d Continuativo    c                   'C'
016400940127     C*----------------------------------------------------*
016500070419      *?     MAIN LINE PROGRAM                             ?
016600940127     C*----------------------------------------------------*
016700940223     C* inizializzazione variabili
016800940223     C                   EXSR      INZVAR
016900940223     C*
017000940223     C     $FINE         DOWEQ     *OFF
017100110823     C*
017200110823     C* Controllo Autorizzazione PROFILO altrimenti non Entra a programma
017300110823     C     $GEST         CASEQ     'AU'          GESAU                          Controllo DAUT
017400110826     C*
017500110826     C* 1° Video
017600070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
017700070413     C*
017800110826     C* Elenco Classi da scegliere
017900110826     C     $GEST         CASEQ     'S0'          GESS0                          Lista Classi
018000110826     C*
018100110826     C* Elenco Articoli da selezionare
018200110826     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli
018300110725     C*
018400110725     C* Finestra x errori
018500110725     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
018600110726      *                                                                         errori su bolle
018700110726     C* Aggiorna il DataBase
018800110726     C     $GEST         CASEQ     'DB'          GESDB                          Aggiorna FILE
018900110726     C*
019000940117     C                   END
019100940117     C                   END
019200940325     C* fine programma
019300940325     C                   SETON                                            LR
019400030113     C************************************************************
019500070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
019600030113     C************************************************************
019700030113     C     INZVAR        BEGSR
019800030113     C*
019900030113     C* Variabili per gestione videate
020000030113     C                   MOVE      *OFF          $FINE
020100110824     C                   MOVE      *OFF          dopo_in_$FINE
020200110726     C                   MOVE      *OFF          $wrtDB
020300110824     C                   MOVE      *ON           $INZD1
020400070227     C                   MOVE      *OFF          $INZW1
020500110826     C                   MOVE      *OFF          $INZS0
020600110826     C                   MOVE      *OFF          $INZS1
020700030113     C                   MOVE      *OFF          $EFILE
020800030113     C                   MOVE      *OFF          $ESCI
020900030113     C                   MOVE      *OFF          $RCDOK
021000110823     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
021100110823     C                   MOVE      'AU'          $GEST
021200030113     C* Variabili appoggio
021300030114     C                   Z-ADD     1             WPAG
021400030113     c*
021500030113     C                   ENDSR
021600110823      * ?-------------------------------------------------------------*?
021700110823      *?   SUBROUTINE Autorizzazione                                 ?
021800110823      * ?-------------------------------------------------------------*?
021900110823     C     GESAU         BEGSR
022000070227     C*
022100110823      * ?- Controlla autorizzazione sull'utente
022200110823     c                   movel     dutpou        i70pog
022300110824     C* Il p.o.deve essere impostato in base al profilo
022400110824     C     dutlpo        IFEQ      '2'
022500110824     C                   MOVEL     dutpou        i70pge                         *FIL.GESTIONE
022600110824     C                   ELSE
022700110824     C                   MOVEL     SIMFEL        i70pge
022800110824     C                   END
022900110823     c                   movel     knmus         i70prf
023000110823     c                   call      'FICN70R'
023100110823     c                   parm                    ficn70ds
023200110823      *
023300110823     c                   movel     o70uni        daut
023400110823     c* utente non abilitato alla gestione magazzino
023500110823     c                   if        §autmag <>'S'
023600110824     c                   eval      Err001 = ' '
023700110825     c                   eval      Err002 = ErrMsg(3)
023800110824     c                   eval      Err003 = ' '
023900110824     C                   MOVE      'S'           dopo_in_$FINE
024000110824     C                   MOVE      'W1'          $GEST
024100110823     C                   goto      ENDAU
024200110823     c                   end
024300110823      *
024400110823     C* Se Abilitato imposta l'Entrata dal Primo Video
024500110823     C                   MOVE      'D1'          $GEST
024600110824     c*
024700110824     C     endAU         ENDSR
024800110824     C************************************************************
024900110824      *?  Emissione Finestra Errori                              ?
025000110824     C************************************************************
025100110824     C     GESW1         BEGSR
025200110824     C*
025300110824     C                   eval      $FINE = dopo_in_$FINE
025400110824     C                   eval      $GEST = sav$GEST
025500110824     c*
025600110824     c                   exfmt     TII5WER
025700110823     c*
025800110824     c* emesso l'errore torna al video che lo aveva generato
025900110824     c*  oppure va fuori dal programma  in base a $FINE e $GEST
026000110824     c*
026100110824     C                   ENDSR
026200110823     C************************************************************
026300110823      *?  Emissione primo video di SCELTA                        ?
026400110823     C************************************************************
026500110823     C     GESD1         BEGSR
026600110823     C*
026700070227     C* inizializzazione videata
026800070227     C     $INZD1        IFEQ      *ON
026900070227     C                   EXSR      INZD1
027000070227     C                   MOVE      *OFF          $INZD1
027100070227     C                   ENDIF
027200070227     C*
027300070228     c     Video_1       tag
027400070302     C*  Emissione Video
027500070228      * ?            *-----------------------*
027600110725     C                   EXFMT     TII5D01
027700070228      * ?            *-----------------------*
027800070326     c                   setoff                                       99
027900070305     C                   movel     'D1'          sav$GEST
028000070227     C* Selezioni
0281000702271    C                   SELECT
028200070413     C*  Cambio filiale
028300070413     C                   WHEN      $TASTO   =   F18
028400070418     C                   SETON                                        07
028500070227     C* F3=Fine
028600070227     C                   WHEN      $TASTO   =   F03
028700070227     C                   EXSR      F03_exit
028800070227      *
0289000702271O   C                   OTHER
029000070227     C* CONTROLLO DATI
029100070227     C                   EXSR      CTRD1
029200080626      *
0293000702271-   C                   ENDSL
029400070227     C*
029500070418     C* Per errori o decodifiche o tasto di cambio P.O.
029600070418     C                   if        $TASTO   =   F18 or *in99
029700070418     c                   goto      Video_1
029800070418     c                   end
029900070228     C*
030000110725     C* Se non ci sono errori passa alle operazioni sul SFL
030100110725     C*   Reperisce il nuovo numero Ordine
030200110725     c                   clear                   ordine_OK         1
030300110725     c                   exsr      new_ordine
030400110725     C*
030500110725     c                   if        ordine_OK ='S'
030600110824     C*
030700110824     C*  Emissione SFL per ordini
030800110826     c                   eval      $GEST ='S0'
030900110826     C                   MOVE      *ON           $INZS0
031000110826     C                   MOVE      *ON           $INZW0
031100110824     C*
031200110725     c                   end
031300070228      *
031400080606     C     Fine_GESD1    ENDSR
031500070306     C/EJECT
031600110726      *---------------------------------------------------------------*
031700070320      *?  INIZIALIZZAZIONE Primo video                           ?
031800110726      *---------------------------------------------------------------*
031900070227     C     INZD1         BEGSR
032000070227     C*
032100070227     C* Pulizia campi e indicatori
032200070308     c                   exsr      INZ_indERR
032300110726     C*
032400070227     C* Pulizia formato record
032500110725     c                   clear                   TII5D01
032600070227     C* campi titolo
032700070302     C                   MOVEL     dsPGM         D1CPGM
032800070227     C                   MOVEL     knsif         D1nsif
032900070227     C                   MOVEL     knmus         D1nmus
033000070227     C                   MOVEL     RSUT          D1crsu
033100110825     C                   MOVEL     Periodico     D1Torp
033200110824     C                   z-add     settimana     D1week
033300070227     C* campi video
033400070227     C* Il p.o.deve essere impostato in base al profilo
033500070413     C     dutlpo        IFEQ      '2'
033600070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
033700070413     C                   ELSE
033800070413     c                   seton                                        08
033900070413     C                   MOVEL     SIMFEL        D1CFGS
034000070413     C                   END
034100080606      *
034200110824     C                   z-add     woggi8        D1cDDC
034300080606      *
034400110824      *  reimposta la filiale
034500070418     c                   if        sav_d1cfgs > 0
034600070418     C                   z-add     sav_d1cfgs    D1cfgs
034700070418     c                   end
034800070418     C*
034900080606     C     END_INZ1      ENDSR
035000940127     C************************************************************
035100070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
035200940127     C************************************************************
035300070227     C     CTRD1         BEGSR
035400070606     C*
035500070227     C*  Spegne indicatori di errore
035600070308     c                   exsr      INZ_indERR
035700070227     c                   setoff                                       99
035800110726     C* --------------
035900070227      * ?- Controlla la filiale in gestione
036000110725     c                   clear                   wfgs
036100110725     C                   clear                   sav_d1cFGS
036200070227     C                   CLEAR                   DSLV50
036300070227     C                   MOVEL     KNMUS         D50PRU
036400070227     C                   z-add     d1cFGS        D50FGS
036500070227     C                   CALL      'FNLV50R'
036600070227     C                   PARM                    DSLV50
036700070227    1C     D50ERR        IFNE      ' '
036800070326     c                   movel     ErrMsg(1)     d1msg
036900070326     C                   SETON                                        51  99
037000070227     C                   goto      ENDCTR1
037100070227    1C                   ENDIF
037200070227      *
037300110726     C*  Controlla il P.O.
037400070227     C     d1cFGS        CHAIN     AZORG01l
037500110725      * Non presente in organigramma
037600110725    1C                   IF        not %found(azorg01l)
037700070326     C                   SETON                                        51  99
037800070326     c                   movel     ErrMsg(1)     d1msg
037900070227     C                   goto      ENDCTR1
038000070227    1C                   ENDIF
038100070227     C*
038200070316      * Salva la filiale su campo di work x tutto il pgm
038300070316     c                   z-add     d1cfgs        wfgs
038400070418     C                   Z-ADD     D1cFGS        sav_d1cFGS
038500070307      *
038600110824      * > TABELLA WEK x le settimane degli ordini mensili
038700110825     c                   if        D1TORP = Periodico
038800110824      **
038900110824     C                   eval        tbeCOD = 'WEK'
039000110825     C                   movel(p)  wfgs          tbeKE1
039100110824     c     kTnTBE        chain     tntbe01l
039200110825    1c                   if        not %Found(tntbe01l)
039300110725     C                   SETON                                        52  99
039400110825     c                   movel     ErrMsg(2)     d1msg
039500110725     C                   goto      ENDCTR1
039600110825      * >>>>>>>>>>>>>
039700110725    1C                   Else
039800110726      * >>>>>>>>>>>>>
039900110825      *  Controlla se è la settimana corretta per l'inserimento del Periodico
040000110825     c     settimana     lookup    weekWEK                                21
040100110824     c                   if        not *in21
040200110824     C                   SETON                                        52  99
040300110825     c                   movel     ErrMsg(5)     d1msg
040400110824     C                   goto      ENDCTR1
040500110824     c                   end
040600110824      ***
040700110824     C                   eval       orpFIL  = d1cfgs
040800110824     C                   eval       orpANNO = Anno
040900110824     C                   eval       orpWEEK = settimana
041000110824     c     kTiORP        setll     tiORP01L
041100110824     c                   if        %Equal
041200110824     C                   SETON                                        52  99
041300110825     c                   movel     ErrMsg(6)     d1msg
041400110824     C                   goto      ENDCTR1
041500110824     c                   endif
041600110824      ***
041700110825    1c                   endif
041800110726      * >>>>>>>>>>>>>
041900110825     C                   ENDIF
042000070307     C*---------------
042100070227     C     ENDCTR1       ENDSR
042200070319      *---------------------------------------------------------------*
042300110726      *  ?  GESTIONE SFL di immissione righe di ordine               ?
042400070319      *---------------------------------------------------------------*
042500110826     C     GESS0         BEGSR
042600070319     C*
042700940223     C* inizializzazione videata
042800110826     C     $INZS0        IFEQ      *ON
042900110826     C                   EXSR      INZS0
043000110826     C                   MOVE      *OFF          $INZS0
043100940127     C                   ENDIF
043200070327     C*
043300070306     C* Inizializza e imposta piede videata
043400110826     C                   EXSR      INZZ0
043500070306     C*
043600030113     C* emissione piede videata
043700070302      * ?            *-----------------------*
043800110826     C                   WRITE     TII5Z00
043900070302      * ?            *-----------------------*
044000070228     C*
044100940223     C     WMAX          IFEQ      0
044200070228     C* Non ci sono records
044300070302      * ?            *-----------------------*
044400110826     C                   WRITE     TII5V00
044500070302      * ?            *-----------------------*
044600070301     c                   setoff                                       30
044700030114     C                   Else
044800070301     c                   seton                                        30
044900070330     c                   eval       sav_WSfl =  WSfl
045000070330     c                   eval       sav_Wpag =  Wpag
045100070301      *
045200030114     C     Wsfl          IFgt      0
045300110826     C                   Z-ADD     wsfl          C0RCD
045400030114     C                   Else
045500030114     C     Wpag          IFgt      0
045600110826     C                   Z-ADD     wpag          C0RCD
045700030114     C                   EndIF
045800030114     C                   EndIF
045900030114     C                   ENDIF
046000070228      *
046100070228      *  Emissione SFL dei GIRI
046200070228      * ?            *-----------------------*
046300110826     C                   EXFMT     TII5C00
046400070228      * ?            *-----------------------*
046500070326     c                   setoff                                       99
046600110826     C                   movel     'S0'          sav$GEST
046700070228      *
046800110826     C     C0NRR         IFNE      0
046900110826     C                   Z-ADD     C0NRR         WSFL
047000940204     C                   ENDIF
047100110826     C                   Z-ADD     SFLNRR        C0RCD
047200030113     C* Selezioni
0473009401271    C                   SELECT
047400070328     C* F3=Fine
047500070328     C     $TASTO        WHENEQ    F03
047600070328     C                   EXSR      F03_exit
047700110726      *
047800070328     C* F12=Ritorno
047900070328     C     $TASTO        WHENEQ    F12
048000070328     c                   move      'D1'          $GEST
048100070328     C                   MOVE      *ON           $INZD1
048200070328      *
0483009401271O   C                   OTHER
048400070228     C* CONTROLLO testata
048500110826     C                   EXSR      CTRC0
048600070228     C* CONTROLLO subfile
048700940201     C     *IN99         IFEQ      *OFF
048800110826     C                   EXSR      CTRS0
048900940131     C                   END
049000070327      *
049100110726      * ? Scrive   l'ordine
049200070228     C     *IN99         IFEQ      *OFF
049300110726     C     $TASTO        andeq     F06
049400070328     c     ErrMsgZ       andEQ     *blank
049500070328     C     WMAX          andGT     0
049600110826     C                   MOVE      *ON           $INZS1
049700110826     C                   movel     'S1'          $GEST
049800070314     c                   end
049900070228      *
0500009401271-   C                   ENDSL
050100940127     C*
050200940127     C                   ENDSR
050300070328     C/EJECT
050400070328      *---------------------------------------------------------------*
050500070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
050600070328      *---------------------------------------------------------------*
050700110826     C     INZZ0         BEGSR
050800070328     C*
050900070306     C* Messaggio o tasti funzionali
051000110826     c                   clear                   z0dmsg
051100070328     c                   clear                   ErrmsgZ          80
051200070307     C*
051300070328      * imposta eventuali errori
051400110826     c                   eval      z0dmsg = ErrmsgZ
051500070306     C*
051600070306     C                   ENDSR
051700070306     C/EJECT
051800070306      *---------------------------------------------------------------*
051900070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
052000070306      *---------------------------------------------------------------*
052100110826     C     INZS0         BEGSR
052200070306     C*
052300070228     C* reset indicatori DSPATR
052400070308     c                   exsr      INZ_indERR
052500940302     C* pulizia SFL
052600940128     C                   SETOFF                                         3031
052700110826     C                   WRITE     TII5C00
052800940128     C                   SETON                                          31
052900940128     C*
053000110826     C* Righe x Pagina del 0° SFL
053100110826     c                   z-add     RigPag_SFl0   s0pag
053200070228      *
053300030113     C* CARICAMENTO SFL totale
053400110826     C                   Z-ADD     0             S0NRR
053500110826     C                   Z-ADD     1             C0RCD
053600940128     C                   Z-ADD     0             WMAX
053700070302     C                   Z-ADD     0             Wsfl
053800070302     C* campi titolo
053900110826     C                   MOVEL     dsPGM         c0CPGM
054000110826     C                   MOVEL     knsif         c0nsif
054100110826     C                   MOVEL     knmus         c0nmus
054200110826     C                   MOVEL     RSUT          c0crsu
054300110826     C                   z-add     settimana     c0week
054400110725      **----------------
054500110725     C* Posizionamento su file pilota
054600110826     C                   eval        tabGRU    = '155'
054700110825     C                   eval        $EFILE    = *OFF
054800110826     C     tabGRU        setll     Antab01L
054900110725      **----------------
055000030113     C* Carico il SFL
055100110826     C                   EXSR      ROLS0
055200030113     C*
055300030114     C                   Z-ADD     1             WPAG
055400110726     c                   eval       sav_Wmax =  Wmax
055500110725     C*
055600110725     C                   ENDSR
055700110725     C/EJECT
055800110725      *---------------------------------------------------------------*
055900070320      *? CARICAMENTO PAGINA LISTA                                    ?
056000070305      *---------------------------------------------------------------*
056100110826     C     ROLS0         BEGSR
056200070305     C*
056300070305     C                   SETOFF                                       32
056400070305     C                   Z-ADD     0             Y
056500110826     C                   Z-ADD     WMAX          S0NRR
056600070305     C*
056700070305     C* Caricamento del Sfl x Giro
056800110826     c                   Exsr      Write_S0
056900070305     C*
057000110826     C                   Z-ADD     S0NRR         WMAX                 30
057100070305     C*
057200110826     C* POSIZIONAMENTO AL 0° RCD DELLA PAGINA
057300110826     C     S0NRR         DIV       S0pag         PAGINE            4 0
057400070305     C                   MVR                     RESTO             3 0
057500110826     C     PAGINE        MULT      S0pag         C0RCD
0576000703051    C     RESTO         IFGT      0
057700110826     C                   ADD       1             C0RCD
0578000703051E   C                   ELSE
057900110826     C                   SUB       S0pag         C0RCD
058000110826     C                   ADD       1             C0RCD
0581000703051-   C                   ENDIF
058200070305     C*
058300070305     C                   ENDSR
058400070301      *---------------------------------------------------------------*
058500070320      *?  Scrive x Ogni rottura di GIRO                              ?
058600070301      *---------------------------------------------------------------*
058700110826     C     Write_S0      BEGSR
058800070305      *
058900110825      *  Precarica da Ordine Periodico
059000110826     c                   exsr      Legge_rec0
059100070228      *
059200070228      * Finchè il giro è lo stesso
059300110826     c     Y             Dowge     RigPag_SFl0
059400110826     C                   clear                   TII5S00
059500070228      *
059600110726      * Solo x ordini mensili
059700110826     c                   exsr      carica_campi0
059800070305      *---------
059900110826     C                   ADD       1             S0NRR
060000070305     C                   ADD       1             Y
060100110826     C                   WRITE     TII5S00
060200070305      *---------
060300110826     c                   exsr      Legge_rec0
060400110825     C*
060500070228     c                   EndDO
060600070228     C*
060700070228     C                   ENDSR
060800110825      *---------------------------------------------------------------*
060900110825      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
061000110825      *---------------------------------------------------------------*
061100110826     C     LEGGE_REC0    BEGSR
061200110825     C*
061300110825     C*  Per EoF deve uscire
061400110825     C*  Per Record Ok deve uscire riportando i dati
061500110825     C                   MOVEL     *OFF          $EFILE
061600110825     C                   MOVEL     *OFF          $RCDOK
061700110825     C*
0618001108251    C     $EFILE        DOUEQ     *ON
061900110825     C     $RCDOK        OREQ      *ON
062000110825     C*
062100110825      * ? Se Ordine Periodico deve caricare da file precaricato
062200110825     c****   LETTURA RECORD x Filiale
062300110826     c     tabGRU        reade     antab01l
062400110825      * Fine File
062500110826     C                   IF        %Eof(antab01l)
062600110825     C                   MOVEL     *ON           $EFILE
062700110825     C                   MOVE      $EFILE        *IN33
062800110825     C                   Else
062900110825      * ?Controlla se la Spedizione è da considerare
063000110825     C                   MOVEL     *ON           $RCDOK
063100110826     C                   exsr      Check_RECord0
063200110825     C                   END
063300110825     C*
0634001108251-   C                   ENDDO
063500110825     C*
063600110825     C                   ENDSR
063700110825      *---------------------------------------------------------------*
063800110825      *?   CONTROLLO il record letto se va bene                      ?
063900110825      *---------------------------------------------------------------*
064000110826     C     Check_Record0 BEGSR
064100110825     C*
064200110825     C*
064300110826     C     end_Check0    ENDSR
064400070301      *---------------------------------------------------------------*
064500110825      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
064600070301      *---------------------------------------------------------------*
064700110826     C     Carica_campi0 BEGSR
064800110726     C*
064900110826     c                   eval      s1DES =  tabDES
065000110826     c                   eval      s1COD =  tabCOD
065100110825     C*
065200110726     C                   ENDSR
065300110726      *---------------------------------------------------------------*
065400110726      *?   CONTROLLO TESTATA LISTA                                   ?
065500110726      *---------------------------------------------------------------*
065600110826     C     CTRC0         BEGSR
065700110726     C*
065800070301     C                   MOVE      *OFF          *IN99
065900110726     C*
066000070301     C                   ENDSR
066100070301      *---------------------------------------------------------------*
066200070320      *?   CONTROLLO OPZIONI LISTA                                   ?
066300070301      *---------------------------------------------------------------*
066400110826     C     CTRS0         BEGSR
066500070301     C*
066600070301     C                   MOVEL     *OFF          $ESCI
066700071129     c                   clear                   ErrMsgZ
066800070301     C                   SETOFF                                       99
066900070301     C*
067000070301     C* Leggo il sfl solo se ci sono rcd
0671000703011    C     WMAX          IFGT      0
067200110826     C                   READC     TII5S00                                21
067300070301     C*
067400070301     C* esce se fine sfl o errore che richiede l'uscita
0675000703012    C     *IN21         DOWEQ     *OFF
067600070301     C     $ESCI         ANDEQ     *OFF
067700110826     C                   Z-ADD     S0NRR         C0RCD
067800070301     C* ctrl su riga
067900110826     C                   EXSR      RECS0
068000070301      *
068100070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0682000703013    C     *IN99         IFEQ      *ON
068300070301     C     $ESCI         OREQ      *ON
068400070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
068500070301     C* la ripristinerò a conclusione del ciclo di READC
068600110826     C                   MOVE      *OFF          $INZS0
0687000703013-   C                   ENDIF
068800070301     C*
068900070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
069000070301     C                   MOVE      *ON           *IN32
069100110826     C                   UPDATE    TII5S00
069200070301     C*
069300070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0694000703013    C     $ESCI         IFEQ      *OFF
069500110826     C                   READC     TII5S00                                21
069600070301      *
069700070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0698000703014    C     *IN21         IFEQ      *ON
069900070301     C* calcolo pagina a cui deve posizionarsi
070000110826     c                   z-add     s0pag         Wpagine
070100070301     C                   EXSR      CLCPAG
0702000703014-   C                   ENDIF
0703000703013-   C                   ENDIF
070400070301     C*
0705000703012-   C                   ENDDO
070600070301     C*
0707000703011-   C                   ENDIF
070800110726     C* __________________________________________________
070900070327     C*  Controlli generali sulle scelte fatte
071000110826     c   99              exsr      Errori
071100110726     C* __________________________________________________
071200070328     C*
071300070301     C                   ENDSR
071400070301     C/EJECT
071500070301      *---------------------------------------------------------------*
071600070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
071700070301      *---------------------------------------------------------------*
071800110826     C     RECS0         BEGSR
071900070301     C*
072000070301     C* reset indicatori DSPATR
072100070308     c                   exsr      INZ_indERR
072200070326     C*
072300110826     C* gestione opzioni
0724001108263    C     S0sce         IFNE      *blank
072500110826     C     *IN99         ANDEQ     *OFF
072600110826     C                   EXSR      SCE_S0
0727001108263-   C                   ENDIF
072800110826     C*
072900070327     C*
073000110826     C     endSF0        ENDSR
073100110826      *---------------------------------------------------------------*
073200110826      *?   GESTIONE OPZIONI LISTA                                    ?
073300110826      *---------------------------------------------------------------*
073400110826     C     SCE_S0        BEGSR
073500110826     C*
073600110826     C*  Selezionata la riga
073700110826     c                   If        s0SCE = '1'
073800110826     c                   eval      kpjbus = kpjbu
073900110826     C                   clear                   Tii501ds
074000110826     C                   move      dutCOU        i501codute
074100110826     C                   move      d0DOR         i501weekAA
074200110826     C                   movel     d1week        i501weekAA
074300110826     C                   move      d1cfgs        i501filial
074400110826     C                   move      d1tORP        i501tipord
074500110826     C                   move      d1uffC        i501uffcom
074600110826     C                   move      s0cod         i501classe
074700110826     C                   move      c0NOR         i501numord
074800110826     C                   move      h0DOR         i501dataor
074900110826     C                   move      *blank        i501cpyMOF
075000110826     C                   movel(p)  Tii501ds      KPJBU
075100110826     C                   call      'TII501R'                            99
075200110826     C                   parm                    KPJBA
075300110826     c                   movel     kpjbus        kpjbu
075400110826     c                   endIf
075500110826     C*
075600110826     C                   ENDSR
075700110826     C/EJECT
075800110826      *---------------------------------------------------------------*
075900110826      *  ?  GESTIONE SFL di immissione righe di ordine               ?
076000110826      *---------------------------------------------------------------*
076100110826     C     GESS1         BEGSR
076200110826     C*
076300110826     C* inizializzazione videata
076400110826     C     $INZS1        IFEQ      *ON
076500110826     C                   EXSR      INZS1
076600110826     C                   MOVE      *OFF          $INZS1
076700110826     C                   ENDIF
076800110826     C*
076900110826     C* Inizializza e imposta piede videata
077000110826     C                   EXSR      INZZ1
077100110826     C*
077200110826     C* emissione piede videata
077300110826      * ?            *-----------------------*
077400110826     C                   WRITE     TII5Z01
077500110826      * ?            *-----------------------*
077600110826     C*
077700110826     C     WMAX          IFEQ      0
077800110826     C* Non ci sono records
077900110826      * ?            *-----------------------*
078000110826     C                   WRITE     TII5V01
078100110826      * ?            *-----------------------*
078200110826     c                   setoff                                       30
078300110826     C                   Else
078400110826     c                   seton                                        30
078500110826     c                   eval       sav_WSfl =  WSfl
078600110826     c                   eval       sav_Wpag =  Wpag
078700110826      *
078800110826     C     Wsfl          IFgt      0
078900110826     C                   Z-ADD     wsfl          C1RCD
079000110826     C                   Else
079100110826     C     Wpag          IFgt      0
079200110826     C                   Z-ADD     wpag          C1RCD
079300110826     C                   EndIF
079400110826     C                   EndIF
079500110826     C                   ENDIF
079600110826      *
079700110826      *  Emissione SFL dei GIRI
079800110826      * ?            *-----------------------*
079900110826     C                   EXFMT     TII5C01
080000110826      * ?            *-----------------------*
080100110826     c                   setoff                                       99
080200110826     C                   movel     'S1'          sav$GEST
080300110826      *
080400110826     C     C1NRR         IFNE      0
080500110826     C                   Z-ADD     C1NRR         WSFL
080600110826     C                   ENDIF
080700110826     C                   Z-ADD     SFLNRR        C1RCD
080800110826     C* Selezioni
0809001108261    C                   SELECT
081000110826     C* F3=Fine
081100110826     C     $TASTO        WHENEQ    F03
081200110826     C                   EXSR      F03_exit
081300110826      *
081400110826     C* F10=Immissione righe Ordine
081500110826     C     $TASTO        WHENEQ    F10
081600110826     C                   EXSR      F10_Immissione
081700110826      *
081800110826     C* F12=Ritorno
081900110826     C     $TASTO        WHENEQ    F12
082000110826     c                   move      'D1'          $GEST
082100110826     C                   MOVE      *ON           $INZD1
082200110826      *
0823001108261O   C                   OTHER
082400110826     C* CONTROLLO testata
082500110826     C                   EXSR      CTRC1
082600110826     C* CONTROLLO subfile
082700110826     C     *IN99         IFEQ      *OFF
082800110826     C                   EXSR      CTRS1
082900110826     C                   END
083000110826      *
083100110826      * ? Scrive   l'ordine
083200110826     C     *IN99         IFEQ      *OFF
083300110826     C     $TASTO        andeq     F06
083400110826     c     ErrMsgZ       andEQ     *blank
083500110826     C     WMAX          andGT     0
083600110826     C                   MOVE      *ON           $WRTDB
083700110826     C                   movel     'DB'          $GEST
083800110826     c                   end
083900110826      *
0840001108261-   C                   ENDSL
084100110826     C*
084200110826     C                   ENDSR
084300110826     C/EJECT
084400110826      *---------------------------------------------------------------*
084500110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
084600110826      *---------------------------------------------------------------*
084700110826     C     INZZ1         BEGSR
084800110826     C*
084900110826     C* Messaggio o tasti funzionali
085000110826     c                   clear                   z1dmsg
085100110826     c                   clear                   ErrmsgZ          80
085200110826     C*
085300110826      * imposta eventuali errori
085400110826     c                   eval      z1dmsg = ErrmsgZ
085500110826     C*
085600110826     C                   ENDSR
085700110826     C/EJECT
085800110826      *---------------------------------------------------------------*
085900110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
086000110826      *---------------------------------------------------------------*
086100110826     C     INZS1         BEGSR
086200110826     C*
086300110826     C* reset indicatori DSPATR
086400110826     c                   exsr      INZ_indERR
086500110826     C* pulizia SFL
086600110826     C                   SETOFF                                         3031
086700110826     C                   WRITE     TII5C01
086800110826     C                   SETON                                          31
086900110826     C*
087000110826     C* Righe x Pagina del 1° SFL
087100110826     c                   z-add     RigPag_SFl1   s1pag
087200110826      *
087300110826     C* CARICAMENTO SFL totale
087400110826     C                   Z-ADD     0             S1NRR
087500110826     C                   Z-ADD     1             C1RCD
087600110826     C                   Z-ADD     0             WMAX
087700110826     C                   Z-ADD     0             Wsfl
087800110826     C* campi titolo
087900110826     C                   MOVEL     dsPGM         c1CPGM
088000110826     C                   MOVEL     knsif         c1nsif
088100110826     C                   MOVEL     knmus         c1nmus
088200110826     C                   MOVEL     RSUT          c1crsu
088300110826     C                   z-add     settimana     c1week
088400110826      **----------------
088500110826     C* Posizionamento su file pilota
088600110826      **** se richiesto il Periodico x precaricamento righe
088700110826     c                   if        D1TORP = Periodico
088800110826      ***   SETLL x FILIALE e periodo di attività e gruppo
088900110826     C                   eval        ormFIL    = d1cfgs
089000110826     C                   eval        $EFILE    = *OFF
089100110826     C     d1cFGS        setll     tiORM01L
089200110826     c                   end
089300110826      **----------------
089400110826     C* Carico il SFL
089500110826     C                   EXSR      ROLS1
089600110826     C*
089700110826     C                   Z-ADD     1             WPAG
089800110826     c                   eval       sav_Wmax =  Wmax
089900110826     C*
090000110826     C                   ENDSR
090100110826     C/EJECT
090200110826      *---------------------------------------------------------------*
090300110826      *? CARICAMENTO PAGINA LISTA                                    ?
090400110826      *---------------------------------------------------------------*
090500110826     C     ROLS1         BEGSR
090600110826     C*
090700110826     C                   SETOFF                                       32
090800110826     C                   Z-ADD     0             Y
090900110826     C                   Z-ADD     WMAX          S1NRR
091000110826     C*
091100110826     C* Caricamento del Sfl x Giro
091200110826     c                   Exsr      Write_S1
091300110826     C*
091400110826     C                   Z-ADD     S1NRR         WMAX                 30
091500110826     C*
091600110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
091700110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
091800110826     C                   MVR                     RESTO             3 0
091900110826     C     PAGINE        MULT      S1pag         C1RCD
0920001108261    C     RESTO         IFGT      0
092100110826     C                   ADD       1             C1RCD
0922001108261E   C                   ELSE
092300110826     C                   SUB       S1pag         C1RCD
092400110826     C                   ADD       1             C1RCD
0925001108261-   C                   ENDIF
092600110826     C*
092700110826     C                   ENDSR
092800110826      *---------------------------------------------------------------*
092900110826      *?  Scrive x Ogni rottura di GIRO                              ?
093000110826      *---------------------------------------------------------------*
093100110826     C     Write_S1      BEGSR
093200110826      *
093300110826      *  Precarica da Ordine Periodico
093400110826     c                   if        d1torp = Periodico
093500110826     c                   exsr      Legge_rec1
093600110826     c                   end
093700110826      *
093800110826      * Finchè il giro è lo stesso
093900110826     c     Y             Dowge     RigPag_SFl1
094000110826     C                   clear                   TII5S01
094100110826      *
094200110826      * Solo x ordini mensili
094300110826     c                   if        d1torp =  Periodico and $EFILE = *OFF
094400110826     c                   exsr      carica_campi1
094500110826     c                   end
094600110826      *---------
094700110826     C                   ADD       1             S1NRR
094800110826     C                   ADD       1             Y
094900110826     C                   WRITE     TII5S01
095000110826      *---------
095100110826     c                   if        d1torp = Periodico and $EFILE = *OFF
095200110826     c                   exsr      Legge_rec1
095300110826     c                   end
095400110826     C*
095500110826     c                   EndDO
095600110826     C*
095700110826     C                   ENDSR
095800110826      *---------------------------------------------------------------*
095900110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
096000110826      *---------------------------------------------------------------*
096100110826     C     LEGGE_REC1    BEGSR
096200110826     C*
096300110826     C*  Per EoF deve uscire
096400110826     C*  Per Record Ok deve uscire riportando i dati
096500110826     C                   MOVEL     *OFF          $EFILE
096600110826     C                   MOVEL     *OFF          $RCDOK
096700110826     C*
0968001108261    C     $EFILE        DOUEQ     *ON
096900110826     C     $RCDOK        OREQ      *ON
097000110826     C*
097100110826      * ? Se Ordine Periodico deve caricare da file precaricato
097200110826     c****   LETTURA RECORD x Filiale
097300110826     c     D1Cfgs        reade     tiORM01l
097400110826      * Fine File
097500110826     C                   IF        %Eof(tiORM01l)
097600110826     C                   MOVEL     *ON           $EFILE
097700110826     C                   MOVE      $EFILE        *IN33
097800110826     C                   Else
097900110826      * ?Controlla se la Spedizione è da considerare
098000110826     C                   MOVEL     *ON           $RCDOK
098100110826     C                   exsr      Check_RECord1
098200110826     C                   END
098300110826     C*
0984001108261-   C                   ENDDO
098500110826     C*
098600110826     C                   ENDSR
098700110826      *---------------------------------------------------------------*
098800110826      *?   CONTROLLO il record letto se va bene                      ?
098900110826      *---------------------------------------------------------------*
099000110826     C     Check_Record1 BEGSR
099100110826     C*
099200110826     C*  se Scaduto o Sospeso ---- SALTA --------
099300110826     c                   if        moffinoAL < woggi8AMG  or
099400110826     c                             mofsosp   <> *blank
099500110826     C                   MOVEL     *OFF          $RCDOK
099600110826     c                   goto      end_check1
099700110826     c                   end
099800110826     C*
099900110826     C     end_check1    ENDSR
100000110826      *---------------------------------------------------------------*
100100110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
100200110826      *---------------------------------------------------------------*
100300110826     C     Carica_campi1 BEGSR
100400110826     C*
100500110826     C*  carica i campi e li protegge
100600110826     c                   seton                                        22
100700110826     c                   eval      h1PROTECT = '1'
100800110826     c                   eval      h1NOTE    = '0'
100900110826     c                   eval      s1MAT =  MOFMAT
101000110826     c                   eval      s1QTA =  MOFQTA
101100110826     C*
101200110826     C* decodifiche Anagrafica articoli
101300110826     c     s1mat         chain     tmAMP01l
101400110826     c                   if        %Found(tmAMP01L)
101500110826     c                   eval      s1DES =  AMPDMT
101600110826     c                   eval      s1UMA =  AMPUMA
101700110826     c                   else
101800110826     c                   eval      s1DES =  *all'*'
101900110826     c                   eval      s1UMA =  *all'*'
102000110826     c                   end
102100110826     C*
102200110826     C                   ENDSR
102300110826      *---------------------------------------------------------------*
102400110826      *?   CONTROLLO TESTATA LISTA                                   ?
102500110826      *---------------------------------------------------------------*
102600110826     C     CTRC1         BEGSR
102700110826     C*
102800110826     C                   MOVE      *OFF          *IN99
102900110826     C*
103000110826     C                   ENDSR
103100110826      *---------------------------------------------------------------*
103200110826      *?   CONTROLLO OPZIONI LISTA                                   ?
103300110826      *---------------------------------------------------------------*
103400110826     C     CTRS1         BEGSR
103500110826     C*
103600110826     C                   MOVEL     *OFF          $ESCI
103700110826     c                   clear                   ErrMsgZ
103800110826     C                   SETOFF                                       99
103900110826     C*
104000110826     C* Leggo il sfl solo se ci sono rcd
1041001108261    C     WMAX          IFGT      0
104200110826     C                   READC     TII5S01                                21
104300110826     C*
104400110826     C* esce se fine sfl o errore che richiede l'uscita
1045001108262    C     *IN21         DOWEQ     *OFF
104600110826     C     $ESCI         ANDEQ     *OFF
104700110826     C                   Z-ADD     S1NRR         C1RCD
104800110826     C* ctrl su riga
104900110826     C                   EXSR      RECS1
105000110826      *
105100110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1052001108263    C     *IN99         IFEQ      *ON
105300110826     C     $ESCI         OREQ      *ON
105400110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
105500110826     C* la ripristinerò a conclusione del ciclo di READC
105600110826     C                   MOVE      *OFF          $INZS1
1057001108263-   C                   ENDIF
105800110826     C*
105900110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
106000110826     C                   MOVE      *ON           *IN32
106100110826     C                   UPDATE    TII5S01
106200110826     C*
106300110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1064001108263    C     $ESCI         IFEQ      *OFF
106500110826     C                   READC     TII5S01                                21
106600110826      *
106700110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1068001108264    C     *IN21         IFEQ      *ON
106900110826     C* calcolo pagina a cui deve posizionarsi
107000110826     c                   z-add     s1pag         Wpagine
107100110826     C                   EXSR      CLCPAG
1072001108264-   C                   ENDIF
1073001108263-   C                   ENDIF
107400110826     C*
1075001108262-   C                   ENDDO
107600110826     C*
1077001108261-   C                   ENDIF
107800110826     C* __________________________________________________
107900110826     C*  Controlli generali sulle scelte fatte
108000110826     c   99              exsr      errori
108100110826     C* __________________________________________________
108200110826     C*
108300110826     C                   ENDSR
108400110826     C/EJECT
108500110826      *---------------------------------------------------------------*
108600110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
108700110826      *---------------------------------------------------------------*
108800110826     C     RECS1         BEGSR
108900110826     C*
109000110826     C* reset indicatori DSPATR
109100110826     c                   exsr      INZ_indERR
109200110826     C*
109300110826     c* se NON è un articolo presente in anagrafica
109400110826     C*  segnala l'errore
109500110826     c                   clear                   s1dES
109600110826     c                   if        s1MAT <> *blank
109700110826     c     S1MAT         chain     TMamp01L
109800110826     c                   if        not %Found(TMamp01L)
109900110826     C                   seton                                        9956
110000110826     c                   eval        err002 = ErrMsg(7)
110100110826     c                   goTO      endSF1
110200110826     c                   else
110300110826     C*
110400110826     c                   eval       s1dES = AmpDMT
110500110826     C*
110600110826     C*    controllare tramite sottoCLASSE merceologica (tabella)
110700110826     C*          l'ufficio di Competenza
110800110826     c                   if        GRU156V <> *blank and GRU156V <> d1uffC
110900110826     C                   seton                                        9956
111000110826     c                   eval        err002 = ErrMsg(8)
111100110826     c                   goTO      endSF1
111200110826     c                   end
111300110826     C*
111400110826     c                   end
111500110826     c                   end
111600110826     C*
111700110826     C     endSF1        ENDSR
111800070301      *---------------------------------------------------------------*
111900110726      *?  GESTIONE F10 x imissione Multipla                          ?
112000070301      *---------------------------------------------------------------*
112100110726     C     F10_immissioneBEGSR
112200070302      *
112300110726      *    far vedere una finestra con le classi merceologiche
112400110726      *      da selezionare
112500110726      *    quindi aprire un'altra finestra di selezione dei relativi articoli
112600110726      *     appartenenti alle classi selezionate
112700110726      *    Tutti gli articoli scelti dovranno essere riportati sulle righe
112800110726      *     in immissione
112900110726      *
113000940309     C                   ENDSR
113100070227     C/EJECT
113200110726      *---------------------------------------------------------------*
113300110726      *?  GESTIONE F03 x uscire LR                                   ?
113400110726      *---------------------------------------------------------------*
113500110726     C     F03_exit      BEGSR
113600110726     C* fine programma
113700110726     C                   MOVE      *ON           $FINE
113800110726      *
113900110726     C                   ENDSR
114000110726     C/EJECT
114100070302      *---------------------------------------------------------------*
114200070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
114300070302      *---------------------------------------------------------------*
114400070302     C     F06_Aggiorna  BEGSR
114500070306      **
114600070309      **
114700110725     C*  Esegue l'aggiornamento
114800110725      **
114900110725      **
115000070305     C*
115100070302     C                   ENDSR
115200110825      *---------------------------------------------------------------*
115300110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
115400110825      *---------------------------------------------------------------*
115500110825     C     CLCPAG        BEGSR
115600110825     C* Input :
115700110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
115800110825     C*          cursore
115900110825     C* - Wpagine= numero rcd per pagina sfl
116000110825     C* Output :
116100110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
116200110825     C*
116300110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
116400110825     C                   MVR                     RESTO             3 0
116500110825     C     RESTO         IFGT      0
116600110825     C                   ADD       1             PAGINE
116700110825     C                   ENDIF
116800110825     C     PAGINE        MULT      Wpagine       WPAG
116900110825     C*
117000110825     C                   ENDSR
117100110825     C************************************************************
117200110825      *?  INIZIALIZZA indicatori di errore                           ?
117300110825     C************************************************************
117400110825     C     INZ_indERR    BEGSR
117500110825      *
117600110825     C                   move      *ALL'0'       IN5190           40
117700110825     C                   moveA     IN5190        *IN(51)
117800110825      *
117900110825     C                   ENDSR
118000070301     C/EJECT
118100070301      *---------------------------------------------------------------*
118200070316      *  ?  OPERAZIONI INIZIALI      ?
118300070301      *---------------------------------------------------------------*
118400940131     C     *INZSR        BEGSR
118500110726     C*
118600110824     C*    chiave per il TNTBE
118700110824     c     kTnTBE        klist
118800110824     C                   KFLD                    tbeCOD
118900110824     C                   KFLD                    tbeKE1
119000110824     C*
119100110824     c     kTiORP        klist
119200110824     C                   KFLD                    orpFIL
119300110824     C                   KFLD                    orpANNO
119400110824     C                   KFLD                    orpWEEK
119500110825     C*
119600110825     c     kTiORM        klist
119700110825     C                   KFLD                    ormFIL
119800110825     C                   KFLD                    ormFINOAL
119900110824     C*
120000030113     C* Reperimento parametri
120100030113     C     *ENTRY        PLIST
120200030113     C                   PARM                    KPJBA
120300070228      *
120400110726      *  Se è EDP
120500070308     c                   CLEAR                   se_EDP            1
120600070308     c                   if        %subst(Knmus:1:3) = 'EDP'
120700070308     c                   MOVE      'S'           se_EDP
120800070307     c                   end
120900070301      *
121000110725     C     *DTAARA       DEFINE                  DTAORC                         *
121100110725     C*
121200070305     C*
121300070227      * Reperisco dati job
121400070227     c                   exsr      DatiJob
121500070227
121600030113     C* Variabili per gestione videate
121700030113     C                   MOVE      *BLANK        $GEST             2
121800070305     C                   MOVE      *BLANK        sav$GEST          2
121900030113     C                   MOVE      *BLANK        $FINE             1
122000110824     C                   MOVE      *BLANK        dopo_in_$FINE     1
122100070227     C                   MOVE      *BLANK        $INZD1            1
122200070227     C                   MOVE      *BLANK        $INZW1            1
122300070227     C                   MOVE      *BLANK        $INZS1            1
122400110726     C                   MOVE      *BLANK        $WRTdb            1
122500030113     C                   MOVE      *BLANK        $EFILE            1
122600030113     C                   MOVE      *BLANK        $ESCI             1
122700030113     C                   MOVE      *BLANK        $RCDOK            1
122800030113     C* Indici
122900030113     C                   Z-ADD     0             X                 3 0
123000030113     C                   Z-ADD     0             Y                 3 0
123100070227     C***
123200070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
123300070227     C                   TIME                    W0140            14 0
123400110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
123500110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
123600070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
123700070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
123800070227     C*
123900110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
124000070227     C                   MOVEL     *BLANK        G02ERR
124100070227     C                   CALL      'XSRDA8'
124200070227     C                   PARM                    WLBDAT
124300110825     c                   Z-ADD     G02INV        WoggiAMG          8 0          *AAAA/MM/GG
124400110825     c                   eval      dataiso = %date(WoggiAMG:*iso)
124500110824     c                   eval      dataeur = dataiso
124600110825     c                   eval      oggiiso = dataiso
124700110824     c                   clear                   settimana         2 0
124800110824     C*
124900110824     C* NR.Settimana in corso
125000110824     C/EXEC SQL
125100110825     C+ SELECT week_iso(OGGIISO) INTO :settimana FROM tiORP00F
125200110824     C/END-EXEC
125300100505     C*
125400940117     C                   ENDSR
125500070227      *---------------------------------------------------------------*
125600070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
125700070227      *---------------------------------------------------------------*
125800070228     c     DatiJOB       BEGSR
125900070227      *
126000070227     c     *dtaara       define    §azute        azuteds
126100070227     c     *dtaara       define    §datiute      ddatiute
126200070227      *
126300070227     c                   in(E)     *dtaara
126400070227     c                   IF        %ERROR or RSUT = *blanks
126500070227     c                   clear                   Tibs34Ds
126600070227     c                   call      'TIBS34R'
126700070227     c                   parm                    Tibs34Ds
126800070227     c                   in        *dtaara
126900070227     c                   ENDIF
127000080606      *
127100070227     c                   ENDSR
127200110825      *---------------------------------------------------------------*
127300110825      *? CARICAMENTO PAGINA LISTA                                    ?
127400110825      *---------------------------------------------------------------*
127500110825     c     New_Ordine    begSR
127600110825     C*
127700110825     C                   z-add     0             Conta_Tenta       3 0
127800110825     C*
127900110825     C*- LETTURA NUMERO ORDINE NUOVO DA AREA DATI
128000110825     C     SULOCK        TAG                                                    *
128100110825     C*          -------------                             *
128200110825     C     *LOCK         IN        DTAORC                               46      *
128300110825     C   46              eval      Conta_Tenta = Conta_Tenta + 1
128400110825     C   46              if        Conta_Tenta = 999
128500110825     C                   GOTO      No_numero                                    *
128600110825     c                   end
128700110825     C   46              GOTO      SULOCK                                       *
128800110825     C                   Z-ADD     ORDNUO        c1nor                          *
128900110825     C                   ADD       1             ORDNUO                         *
129000110825     C                   OUT       DTAORC                                       *
129100110825     c                   move      'S'           ordine_OK
129200110825     C                   MOVE      woggi         c1dor
129300110825     C*
129400110825     C     No_numero     ENDSR
129500110825     C/EJECT
129600110726      * ?-------------------------------------------------------------*?
129700110826     C*   Routine degli errori da visualizzare sui SFL
129800110826      * ?-------------------------------------------------------------*?
129900110826     C     Errori        BEGSR
130000110826     C*
130100110826     c   99              if        ErrMsgZ = *blank
130200110826     C*
130300110826     C* Se non è stata scelto  nemmeno un giro... errore
130400110826     c                   clear                   err001
130500110826     c                   clear                   err002
130600110826     c                   clear                   err003
130700110826      *
130800110826     C*  Messaggio di avviso
130900110826     C*  Messaggio di avviso    SE deve emettere un messaggio
131000110826     C*  Messaggio di avviso
131100110826     C*
131200110826     c                   if        err001 <> *blank or
131300110826     c                             err002 <> *blank or
131400110826     c                             err003 <> *blank
131500110826     c                   seton                                        99
131600110826      * ?            *-----------------------*
131700110826     C                   exfmt     TII5WER
131800110826      * ?            *-----------------------*
131900110826     c                   end
132000110826     C*
132100110826     c                   end
132200110826     C*
132300110826     c                   ENDSR
132400110826      *---------------------------------------------------------------------------------
132500080609      *
132600070326** ErrMsg
132700110825La Filiale non è in gestione                                                   01
132800110825Non è possibile effettuare l''ordine Periodico nella settimana.                02
132900110824L''Utente non è autorizzato alla gestione della procedura.                     03
133000110726Scelta obbligatoria                                                            04
133100110825Manca la tab.WEK -> Settimane x eseguire gli Ordini Periodici.                 05
133200110825E''già stato fatto un ordine Periodico nei giorni precedenti.                  06
133300110825L''Articolo non è presente in anagrafica                                       07
133400110825L''Articolo non appartiene all'ufficio di competenza                           08
133500110725                                                                               09
133600110725                                                                               10
133700110725                                                                               11
133800110725                                                                               12
133900110725                                                                               13
134000110725                                                                               14
134100110725                                                                               15
134200110725                                                                               16
134300110725                                                                               17
134400110725                                                                               18
134500110726                                                                               19
134600110726                                                                               20
134700110726                                                                               21
134800110726                                                                               22
134900110726                                                                               23
135000110726                                                                               24
135100110726                                                                               25
135200110726                                                                               26
135300110726                                                                               27
135400110726                                                                               28
135500110726                                                                               29
135600110726                                                                               30
135700110726                                                                               31
135800110725                                                                               32
135900110726                                                                               33
136000110725                                                                               34
136100070829                                                                               35
136200070829                                                                               36
136300070829                                                                               37
136400070829                                                                               38
136500070829                                                                               39
136600110726Il valore immesso per il campo non è valido.                                   40
