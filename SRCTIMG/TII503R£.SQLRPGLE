000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300110826      *
000400110725      * TII500R *----------------------------------------------------*
000500110826      *     ?   - GESTIONE ORDINI MAGAZZINO -    ?
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
003300110906     FTMorp01L  iF   E           K DISK    rename(tmORP000:tmorp01)
003400110906     FTMorp02L  uF   E           K DISK    infds(orpds)
003500110906     FTMorp00f  uF   E             DISK    rename(tmORP000:tmorpfis) usropn
003600110906     FTmOPP01L  iF   E           K DISK    infds(oppds)
003700080609      *
003800070404      *  Input
003900110830     FazORG01l  if   e           k disk
004000110830     FtnTBE01l  if   e           k disk
004100110826     FanTAB01l  if   e           k disk
004200110726     FtMAMP01l  if   e           k disk
004300110725     Ftii500d   CF   E             WORKSTN
004400110826     F                                     SFILE(TII5S00:S0NRR)
004500110826     F                                     SFILE(TII5S01:S1NRR)
004600940201     F                                     INFDS(DSFMT)
004700070305     D*----------------------------------------------------*
004800070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004900070326     D*----------------------------------------------------*
005000940211     D* Passaggio Parametri
005100940211     D KPJBA         E DS
005200070227     D KPJBUs          s                   like(KPJBU)
005300110726     D*-------------
005400110726     d ficn70ds      e ds
005500110826     d antab155      e ds
005600110826     d antab156      e ds
005700110901     d  og143        e ds
005800110830     d dAUT          e ds
005900110830     d Tii501DS      e ds
006000110909     d Tii502DS      e ds
006100940211     D*-------------
006200110824     d dsWEK         e ds
006300110905     D  weekWEK                       2    DIM(54)
006400110824     D*-------------
006500110830     D  SubClassi                     5    DIM(500)
006600110830     D*-------------
006700940211     D DSFMT           DS           512
006800940506     D  $TASTO               369    369
006900940211     D  NRG                  370    370
007000940211     D  NCL                  371    371
007100940211     D  SFLNRR               378    379B 0
007200940127     D*-------------
007300940127     D* Reperimento nome PGM
007400940127     D STATUS         SDS           333
007500940127     D  DSPGM            *PROC
007600030113     D*-------------
007700110906     d oppds           ds
007800110906     d  opp_nrr              397    400b 0
007900110830     D*-------------
008000110830     d orpds           ds
008100110830     d  orp_nrr              397    400b 0
008200110725     D*-------------
008300110725     D*--- DATA AREA NUMERAZIONE ORDINI :
008400110725     D DTAORC          DS
008500110725     D  ORDNUO                 1      5  0
008600070320     D*-------------
008700070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
008800110826$003 D S0NRR           S                   Like(C0rcd)
008900110826$003 D S0pag           S                   Like(C0rcd)
009000070227$003 D S1NRR           S                   Like(C1rcd)
009100070228$003 D S1pag           S                   Like(C1rcd)
009200070302$003 D WSfl            S                   Like(C1nrr)
009300030113$003 D Wmax            S                   Like(C1rcd)
009400030113$003 D Wpag            S                   Like(C1rcd)
009500070330$003 D sav_WSfl        S                   Like(WSfl)
009600070330$003 D sav_Wmax        S                   Like(Wmax)
009700070330$003 D sav_Wpag        S                   Like(Wpag)
009800070301$003 D Wpagine         S                   Like(C1rcd)
009900110824      *--------------------------
010000110826     D RigPag_SFl0     S              3  0 INZ(18)
010100110826     D RigPag_SFl1     S              3  0 INZ(18)
010200070227      *--------------------------
010300070418     D sav_d1cfgs      S                   Like(d1cfgs) inz(0)
010400110826$003 D sav_S0NRR       S                   Like(s0nrr)
010500110826$003 D sav_S1NRR       S                   Like(s1nrr)
010600101129      *--------------------------
010700110726     d dataiso         s               d   datfmt(*iso)
010800110825     d oggiiso         s               d   datfmt(*iso)
010900101129     d dataeur         s               d   datfmt(*eur)
011000070320      *--------------------------
011100110824     d wFGS            s                   like(d1cFGS)
011200110830     d kCOD1           s                   like(TBEcod)
011300070307     d kKEY1           s                   like(TBEke1)
011400070305      *--------------------------
011500110906     D scritto_almeno_una_riga_ordine...
011600110906     D                 S              1A
011700110906     D premuto_F10...
011800110830     D                 S              1A
011900110909     D Cambia_Video...
012000110909     D                 S              1A
012100110906     D dopo_alert_window...
012200110906     D                 S              1A
012300110901     D Tipo_Filiale    S              1A
012400110905     D rev_Ordine      S                   like(*in12)
012500110830      *--------------------------
012600070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
012700070305     D WLBDAT          DS                  INZ
012800070305     D  G02DAT                 1      8  0
012900070227     D  G02INV                 9     16  0
013000070227     D  G02ERR                17     17
013100070227     D  G02TGI                18     22  0
013200070307      *
013300070307     D DATPAR          DS
013400070307     D  GIODAT                 1      8  0
013500070307     D  GIOINV                 9     16  0
013600070307     D  GIOTGI                17     21  0
013700070307      *
013800070307     d Wdata8          DS
013900070307     d  dadata                 1      8  0
014000070307     d  adata                  9     16  0
014100070307      *--------------------------------
014200070227     d AZUTEds       e ds                  extname(AZUTE00F)
014300070227     d DDatiUte      e ds
014400110725      *--------------------------
014500070320     d* Ds per dati organigramma
014600070404      *------
014700100505     d DTAISO          s               D
014800070320      *--------------------------
014900070320     d* Parametri x Controllo profilo utenti
015000070320     d TIBS34ds      e ds
015100070227      *--------------------------
015200070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
015300070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
015400070227      *--------------------------
015500070320      *?  COSTANTI                                      ?
015600070320      *--------------------------
015700940506     D* Tasti di funzione
015800940506     D F01             C                   CONST(X'31')
015900940506     D F02             C                   CONST(X'32')
016000940506     D F03             C                   CONST(X'33')
016100940506     D F04             C                   CONST(X'34')
016200940506     D F05             C                   CONST(X'35')
016300940506     D F06             C                   CONST(X'36')
016400940506     D F07             C                   CONST(X'37')
016500940506     D F08             C                   CONST(X'38')
016600940506     D F09             C                   CONST(X'39')
016700940506     D F10             C                   CONST(X'3A')
016800940506     D F11             C                   CONST(X'3B')
016900070315     D F12             C                   CONST(X'3C')
017000940506     D F13             C                   CONST(X'B1')
017100940506     D F14             C                   CONST(X'B2')
017200940506     D F15             C                   CONST(X'B3')
017300940506     D F16             C                   CONST(X'B4')
017400940506     D F17             C                   CONST(X'B5')
017500940506     D F18             C                   CONST(X'B6')
017600940506     D F19             C                   CONST(X'B7')
017700940506     D F20             C                   CONST(X'B8')
017800940506     D F21             C                   CONST(X'B9')
017900940506     D F22             C                   CONST(X'BA')
018000940506     D F23             C                   CONST(X'BB')
018100940506     D F24             C                   CONST(X'BC')
018200940506     D ENTER           C                   CONST(X'F1')
018300940506     D ROLDWN          C                   CONST(X'F4')
018400940506     D ROLLUP          C                   CONST(X'F5')
018500070301     d digits          c                   '0123456789'
018600110826     d Pianificato     c                   'P'
018700110831     d aConsumo        c                   'C'
018800110829     d Classe_155      c                   '155'
018900110829     d SottoClas_156   c                   '156'
019000110831     d Acquisti        c                   'A'
019100110831     d Marketing       c                   'M'
019200940127     C*----------------------------------------------------*
019300070419      *?     MAIN LINE PROGRAM                             ?
019400940127     C*----------------------------------------------------*
019500940223     C* inizializzazione variabili
019600940223     C                   EXSR      INZVAR
019700940223     C*
019800940223     C     $FINE         DOWEQ     *OFF
019900110909     C*
020000110909     C*  re-imposta il passaggio fra video e video
020100110909     c                   eval      cambia_video = *blank
020200110909     c                   if        $GEST <> sav$GEST and $GEST <> 'W1'
020300110909     c                   eval      cambia_video = 'S'
020400110909     C                   end
020500110823     C*
020600110906     C* Controllo Autorizzazione PROFILO
020700110906     C*  altrimenti non Entra a programma
020800110823     C     $GEST         CASEQ     'AU'          GESAU                          Controllo DAUT
020900110826     C*
021000110826     C* Finestra x ALLARME messaggi importanti
021100110826     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
021200110906     C*
021300110826     C* 1° Video
021400070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
021500070413     C*
021600110906     C* 2° Video SFL Sottoclassi da scegliere
021700110906     C     $GEST         CASEQ     'S0'          GESS0                          Lista SottoClassi
021800110826     C*
021900110906     C* 3° Video SFL riepilogativo dell'ordine
022000110906     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli Immes
022100110906     C*
022200110906     C* Uscita forzata - Cancella Ordine immesso
022300110906     C     $GEST         CASEQ     'CO'          GESCO                          Cancella Ordine
022400110906      *
022500940117     C                   END
022600110909      *
022700110909     C                   ENDdo
022800940325     C* fine programma
022900110901     C                   clear                   Tii501ds
023000110901     C                   move      Tipo_Filiale  i501livLPO
023100110901     C                   move      dutCOU        i501codute
023200110901     C                   move      knmus         i501knmus
023300110901     C                   move      anno          i501weekAA
023400110901     C                   movel     settimana     i501weekAA
023500110901     C                   move      dutpou        i501filial
023600110901     C                   movel(p)  Tii501ds      KPJBU
023700110901      **
023800940325     C                   SETON                                            LR
023900030113     C************************************************************
024000070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
024100030113     C************************************************************
024200030113     C     INZVAR        BEGSR
024300030113     C*
024400030113     C* Variabili per gestione videate
024500030113     C                   MOVE      *OFF          $FINE
024600110824     C                   MOVE      *OFF          dopo_in_$FINE
024700110824     C                   MOVE      *ON           $INZD1
024800110826     C                   MOVE      *OFF          $INZS0
024900110826     C                   MOVE      *OFF          $INZS1
025000030113     C                   MOVE      *OFF          $EFILE
025100030113     C                   MOVE      *OFF          $ESCI
025200030113     C                   MOVE      *OFF          $RCDOK
025300110823     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
025400110823     C                   MOVE      'AU'          $GEST
025500030113     C* Variabili appoggio
025600030114     C                   Z-ADD     1             WPAG
025700030113     c*
025800030113     C                   ENDSR
025900110823     C************************************************************
026000110823      *?  Emissione primo video di SCELTA                        ?
026100110823     C************************************************************
026200110823     C     GESD1         BEGSR
026300110823     C*
026400070227     C* inizializzazione videata
026500070227     C     $INZD1        IFEQ      *ON
026600070227     C                   EXSR      INZD1
026700070227     C                   MOVE      *OFF          $INZD1
026800070227     C                   ENDIF
026900070227     C*
027000070228     c     Video_1       tag
027100070302     C*  Emissione Video
027200070228      * ?            *-----------------------*
027300110725     C                   EXFMT     TII5D01
027400070228      * ?            *-----------------------*
027500070326     c                   setoff                                       99
027600070305     C                   movel     'D1'          sav$GEST
027700070227     C* Selezioni
0278000702271    C                   SELECT
027900070413     C*  Cambio filiale
028000070413     C                   WHEN      $TASTO   =   F18
028100070418     C                   SETON                                        07
028200070227     C* F3=Fine
028300070227     C                   WHEN      $TASTO   =   F03
028400070227     C                   EXSR      F03_exit
028500070227      *
0286000702271O   C                   OTHER
028700070227     C* CONTROLLO DATI
028800070227     C                   EXSR      CTRD1
028900080626      *
0290000702271-   C                   ENDSL
029100070227     C*
029200070418     C* Per errori o decodifiche o tasto di cambio P.O.
029300070418     C                   if        $TASTO   =   F18 or *in99
029400070418     c                   goto      Video_1
029500070418     c                   end
029600070228     C*
029700110902     C*   se è stato richiesto un numero d'ordine specifico
029800110902     C*    rimanda al Riepilogo per rivisualizzare ciò che è stato
029900110902     C*   immesso nel sistema
030000110902     c                   if        d1nor > 0
030100110905     c                   seton                                        12
030200110905     c                   eval      rev_ordine = *in12
030300110902     c                   eval      num_ordine = d1nor
030400110902     C*  Emissione SFL per ordini di riepilogo
030500110902     c                   eval      $GEST ='S1'
030600110902     C                   MOVE      *ON           $INZS1
030700110902     c                   else
030800110902     C*
030900110725     C* Se non ci sono errori passa alle operazioni sul SFL
031000110725     C*   Reperisce il nuovo numero Ordine
031100110725     c                   clear                   ordine_OK         1
031200110906     c                   eval      scritto_almeno_una_riga_ordine = ' '
031300110906     c                   eval      premuto_F10 = ' '
031400110906     C*
031500110906     C*  preleva un nuovo numero ordine
031600110725     c                   exsr      new_ordine
031700110725     C*
031800110725     c                   if        ordine_OK ='S'
031900110824     C*  Emissione SFL per ordini
032000110826     c                   eval      $GEST ='S0'
032100110826     C                   MOVE      *ON           $INZS0
032200110725     c                   end
032300110902     C*
032400110902     c                   endIF
032500070228      *
032600080606     C     Fine_GESD1    ENDSR
032700070306     C/EJECT
032800110726      *---------------------------------------------------------------*
032900070320      *?  INIZIALIZZAZIONE Primo video                           ?
033000110726      *---------------------------------------------------------------*
033100070227     C     INZD1         BEGSR
033200070227     C*
033300070227     C* Pulizia campi e indicatori
033400070308     c                   exsr      INZ_indERR
033500110726     C*
033600070227     C* Pulizia formato record
033700110725     c                   clear                   TII5D01
033800070227     C* campi titolo
033900070302     C                   MOVEL     dsPGM         D1CPGM
034000070227     C                   MOVEL     knsif         D1nsif
034100070227     C                   MOVEL     knmus         D1nmus
034200070227     C                   MOVEL     RSUT          D1crsu
034300110831     C                   MOVEL     aConsumo      D1Torp
034400110831     C                   MOVEL     Acquisti      D1UFFc
034500110824     C                   z-add     settimana     D1week
034600070227     C* campi video
034700070227     C* Il p.o.deve essere impostato in base al profilo
034800070413     C     dutlpo        IFEQ      '2'
034900070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
035000070413     C                   ELSE
035100070413     c                   seton                                        08
035200070413     C                   MOVEL     SIMFEL        D1CFGS
035300070413     C                   END
035400080606      *
035500110830     C                   z-add     woggi         D1cDDC
035600080606      *
035700110824      *  reimposta la filiale
035800070418     c                   if        sav_d1cfgs > 0
035900070418     C                   z-add     sav_d1cfgs    D1cfgs
036000070418     c                   end
036100070418     C*
036200080606     C     END_INZ1      ENDSR
036300940127     C************************************************************
036400070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
036500940127     C************************************************************
036600070227     C     CTRD1         BEGSR
036700070606     C*
036800070227     C*  Spegne indicatori di errore
036900070308     c                   exsr      INZ_indERR
037000110905     c                   setoff                                       99  12
037100110905     c                   eval      rev_ordine = *in12
037200110726     C* --------------
037300110902     c
037400110902     C* --------------
037500070227      * ?- Controlla la filiale in gestione
037600110725     c                   clear                   wfgs
037700110725     C                   clear                   sav_d1cFGS
037800070227     C                   CLEAR                   DSLV50
037900070227     C                   MOVEL     KNMUS         D50PRU
038000070227     C                   z-add     d1cFGS        D50FGS
038100070227     C                   CALL      'FNLV50R'
038200070227     C                   PARM                    DSLV50
038300070227    1C     D50ERR        IFNE      ' '
038400070326     c                   movel     ErrMsg(1)     d1msg
038500070326     C                   SETON                                        51  99
038600070227     C                   goto      ENDCTR1
038700070227    1C                   ENDIF
038800070227      *
038900110726     C*  Controlla il P.O.
039000110901     c                   movel     dutLPO        Tipo_Filiale
039100070227     C     d1cFGS        CHAIN     AZORG01l
039200110725      * Non presente in organigramma
039300110725    1C                   IF        not %found(azorg01l)
039400070326     C                   SETON                                        51  99
039500070326     c                   movel     ErrMsg(1)     d1msg
039600070227     C                   goto      ENDCTR1
039700110901    1C                   else
039800110901     c                   eval      og143 = orgDE3
039900110901     c                   if        §ogNTW = 'LOG'
040000110901     c                   movel     'L'           Tipo_Filiale
040100110901     c                   end
040200110901    1C                   ENDIF
040300070227     C*
040400070316      * Salva la filiale su campo di work x tutto il pgm
040500070316     c                   z-add     d1cfgs        wfgs
040600070418     C                   Z-ADD     D1cFGS        sav_d1cFGS
040700070307      *
040800110824      * > TABELLA WEK x le settimane degli ordini mensili
040900110826     c                   if        D1TORP = Pianificato
041000110824      **
041100110824     C                   eval        tbeCOD = 'WEK'
041200110825     C                   movel(p)  wfgs          tbeKE1
041300110824     c     kTnTBE        chain     tntbe01l
041400110825    1c                   if        not %Found(tntbe01l)
041500110725     C                   SETON                                        52  99
041600110825     c                   movel     ErrMsg(2)     d1msg
041700110725     C                   goto      ENDCTR1
041800110825      * >>>>>>>>>>>>>
041900110725    1C                   Else
042000110905     c                   movea     tbeUNI        weekWEK
042100110726      * >>>>>>>>>>>>>
042200110826      *  Controlla se è la settimana corretta per l'inserimento del Pianificato
042300110905     c                   move      settimana     week2A            2
042400110905     c     week2a        lookup    weekWEK                                21
042500110824     c                   if        not *in21
042600110824     C                   SETON                                        52  99
042700110825     c                   movel     ErrMsg(5)     d1msg
042800110824     C                   goto      ENDCTR1
042900110824     c                   end
043000110824      ***
043100110824     C                   eval       orpFIL  = d1cfgs
043200110905     C                   eval       orpUFF  = d1uffC
043300110824     C                   eval       orpANNO = Anno
043400110824     C                   eval       orpWEEK = settimana
043500110905     C                   eval       orpPIAN = d1torp
043600110908     c     kTmORP        chain     tmORP01L
043700110908     c                   if        %Found(tmORP01L)
043800110912      **
043900110912      ** se già esiste la ggancia da confermare
044000110912     c                   z-add     orpNOR        num_Ordine
044100110912     c                   z-add     orpNOR        d1NOR
044200110912     c                   z-add     orpDOR        h0DOR
044300110912     C*
044400110912     C                   Z-ADD     h0DOR         G02INV                         *AAAA/MM/GG
044500110912     C                   MOVEL     '3'           G02ERR
044600110912     C                   CALL      'XSRDA8'
044700110912     C                   PARM                    WLBDAT
044800110912     c                   Z-ADD     G02DAT        Data_Ordine
044900110912     c                   move      'S1'          $GEST
045000110912     c                   move      *ON           $INZS1
045100110912     c                   goto      ENDCTR1
045200110912      **
045300110912      ** se già esiste emette l'errore.
045400110908     c                   if          orpconf = '1'
045500110908     c                                or
045600110908     c                               orpconf <> '1' and
045700110908     c                               d1int <> '?'   and
045800110908     c                               d1nor <> orpNOR
045900110824     C                   SETON                                        52  99
046000110825     c                   movel     ErrMsg(6)     d1msg
046100110824     C                   goto      ENDCTR1
046200110824     c                   endif
046300110908     c                   end
046400110824      ***
046500110902      ***
046600110912     c     dopo          tag
046700110825    1c                   endif
046800110726      * >>>>>>>>>>>>>
046900110825     C                   ENDIF
047000110902      * >>>>>>>>>>>>>
047100110902      ***  interroga quali Ordini sono stati immessi per la filiale
047200110902     c                   if        d1int = '?'
047300110902     c                   movel     kpjbu         kpjbus
047400110909     c                   clear                   tii502ds
047500110916     c                   move      '0'           I502confer
047600110916     c                   z-add     d1cfgs        I502FILIAL
047700110909     c                   movel     d1torp        I502TIPORD
047800110909     c                   movel     d1uffc        I502UFFCOM
047900110909     c                   movel     settimana     I502WEEKAA
048000110909     c                   move      anno          I502WEEKAA
048100110909     c                   movel     tii502ds      kpjbu
048200110902     c                   call      'TII502R'
048300110902     c                   parm                    kpjba
048400110909     c                   movel     kpjbu         tii502ds
048500110909     c                   move      o502numord    d1nor
048600110916     c                   eval      d1int = *blank
048700110908     c                   if        d1nor = 0
048800110908     c                   SETON                                            99
048900110908     c                   end
049000110902     c                   movel     kpjbus        kpjbu
049100110902     c                   end
049200110902      ***
049300110908     c                   IF        d1nor > 0
049400110906     c     d1nor         chain     tmORP02L
049500110906     c                   if        not %Found(TmOrp02L)
049600110902     C                   SETON                                        54  99
049700110902     c                   movel     ErrMsg(9)     d1msg
049800110902     C                   goto      ENDCTR1
049900110902     c                   else
050000110902     C* se l'ordine è stato chiuso
050100110902     c                   if        orpCONF <> *blank
050200110902     C                   SETON                                        54  99
050300110902     c                   movel     ErrMsg(10)    d1msg
050400110902     C                   goto      ENDCTR1
050500110902     c                   else
050600110902     C*
050700110902     C                   Z-ADD     orpDOR        G02INV                         *AAAA/MM/GG
050800110902     C                   MOVEL     '3'           G02ERR
050900110902     C                   CALL      'XSRDA8'
051000110902     C                   PARM                    WLBDAT
051100110902     c                   Z-ADD     G02DAT        Data_Ordine
051200110902     C*
051300110902     c                   endif
051400110902     c                   endif
051500110902     c                   endif
051600070307     C*---------------
051700070227     C     ENDCTR1       ENDSR
051800070319      *---------------------------------------------------------------*
051900110726      *  ?  GESTIONE SFL di immissione righe di ordine               ?
052000070319      *---------------------------------------------------------------*
052100110826     C     GESS0         BEGSR
052200070319     C*
052300940223     C* inizializzazione videata
052400110826     C     $INZS0        IFEQ      *ON
052500110826     C                   EXSR      INZS0
052600110826     C                   MOVE      *OFF          $INZS0
052700940127     C                   ENDIF
052800070327     C*
052900070306     C* Inizializza e imposta piede videata
053000110826     C                   EXSR      INZZ0
053100070306     C*
053200030113     C* emissione piede videata
053300070302      * ?            *-----------------------*
053400110826     C                   WRITE     TII5Z00
053500070302      * ?            *-----------------------*
053600070228     C*
053700940223     C     WMAX          IFEQ      0
053800070228     C* Non ci sono records
053900070302      * ?            *-----------------------*
054000110826     C                   WRITE     TII5V00
054100070302      * ?            *-----------------------*
054200070301     c                   setoff                                       30
054300030114     C                   Else
054400070301     c                   seton                                        30
054500070330     c                   eval       sav_WSfl =  WSfl
054600070330     c                   eval       sav_Wpag =  Wpag
054700070301      *
054800030114     C     Wsfl          IFgt      0
054900110826     C                   Z-ADD     wsfl          C0RCD
055000030114     C                   Else
055100030114     C     Wpag          IFgt      0
055200110826     C                   Z-ADD     wpag          C0RCD
055300030114     C                   EndIF
055400030114     C                   EndIF
055500030114     C                   ENDIF
055600070228      *
055700070228      *  Emissione SFL dei GIRI
055800070228      * ?            *-----------------------*
055900110826     C                   EXFMT     TII5C00
056000070228      * ?            *-----------------------*
056100070326     c                   setoff                                       99
056200110908     c                   clear                   AlmenoUnaSCl      1
056300110826     C                   movel     'S0'          sav$GEST
056400070228      *
056500110826     C     C0NRR         IFNE      0
056600110826     C                   Z-ADD     C0NRR         WSFL
056700940204     C                   ENDIF
056800110826     C                   Z-ADD     SFLNRR        C0RCD
056900030113     C* Selezioni
0570009401271    C                   SELECT
057100070328     C* F3=Fine
057200070328     C     $TASTO        WHENEQ    F03
057300070328     C                   EXSR      F03_exit
057400110726      *
057500110906     C* F10=Avanza
057600110906     C     $TASTO        WHENEQ    F10
057700110906     c                   move      'S1'          $GEST
057800110906     C                   MOVE      *ON           $INZS1
057900110906      *
058000070328     C* F12=Ritorno
058100070328     C     $TASTO        WHENEQ    F12
058200070328     c                   move      'D1'          $GEST
058300070328     C                   MOVE      *ON           $INZD1
058400070328      *
0585009401271O   C                   OTHER
058600070228     C* CONTROLLO testata
058700110826     C                   EXSR      CTRC0
058800070228     C* CONTROLLO subfile
058900940201     C     *IN99         IFEQ      *OFF
059000110826     C                   EXSR      CTRS0
059100940131     C                   END
059200070327      *
059300110830      * ? Visualizza l'ordine se scritto almeno un record di ordine
059400110830      *    dal programma chiamato.
059500110830     C     *IN99         IFEQ      *OFF
059600110902     C**** $TASTO        andeq     F06
059700110908     c     AlmenoUnaSCl  andeq     'S'
059800110830     c     ERRmsg80      andEQ     *blank
059900110830     C     WMAX          andGT     0
060000110909     c     i501keyRET    andNE     F12
060100110826     C                   MOVE      *ON           $INZS1
060200110826     C                   movel     'S1'          $GEST
060300070314     c                   end
060400070228      *
0605009401271-   C                   ENDSL
060600940127     C*
060700940127     C                   ENDSR
060800070328     C/EJECT
060900070328      *---------------------------------------------------------------*
061000070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
061100070328      *---------------------------------------------------------------*
061200110826     C     INZZ0         BEGSR
061300070328     C*
061400070306     C* Messaggio o tasti funzionali
061500110826     c                   clear                   z0dmsg
061600110826     c                   clear                   ERRmsg80         80
061700070307     C*
061800070328      * imposta eventuali errori
061900110826     c                   eval      z0dmsg = ERRmsg80
062000070306     C*
062100110906     C*   Attenzione se ho scritto delle righe di Ordine
062200110906     C*     NON deve permettere di tornare alla videata iniziale
062300110906     c                   setOFF                                       12
062400110906     c                   if        scritto_almeno_una_riga_ordine ='S'
062500110906     c                               and
062600110906     c                             premuto_F10 = *blank
062700110906     c                   setON                                        12
062800110906     c                   end
062900110906     C*
063000070306     C                   ENDSR
063100070306     C/EJECT
063200070306      *---------------------------------------------------------------*
063300070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
063400070306      *---------------------------------------------------------------*
063500110826     C     INZS0         BEGSR
063600070306     C*
063700070228     C* reset indicatori DSPATR
063800070308     c                   exsr      INZ_indERR
063900940302     C* pulizia SFL
064000940128     C                   SETOFF                                         3031
064100110826     C                   WRITE     TII5C00
064200940128     C                   SETON                                          31
064300940128     C*
064400110826     C* Righe x Pagina del 0° SFL
064500110826     c                   z-add     RigPag_SFl0   s0pag
064600070228      *
064700030113     C* CARICAMENTO SFL totale
064800110826     C                   Z-ADD     0             S0NRR
064900110826     C                   Z-ADD     1             C0RCD
065000940128     C                   Z-ADD     0             WMAX
065100070302     C                   Z-ADD     0             Wsfl
065200070302     C* campi titolo
065300110826     C                   MOVEL     dsPGM         c0CPGM
065400110826     C                   MOVEL     knsif         c0nsif
065500110826     C                   MOVEL     knmus         c0nmus
065600110826     C                   MOVEL     RSUT          c0crsu
065700110826     C                   z-add     settimana     c0week
065800110826     C                   z-add     num_ordine    c0NOR
065900110826     C                   z-add     data_ordine   c0DOR
066000110830     C*
066100110830     C*  Se richiesto un ordine pianificato x Filiale
066200110830     C*    occorre precaricarsi le sottoclassi presenti sugli articoli
066300110830     C*  dell'ordine pianificato.
066400110830     c                   if        D1TORP = Pianificato
066500110830     C                   exsr      carica_subclas
066600110830     C                   END
066700110830     C*
066800110725      **----------------
066900110725     C* Posizionamento su file pilota
067000110825     C                   eval        $EFILE    = *OFF
067100110829     C     SottoClas_156 setll     Antab01L
067200110725      **----------------
067300030113     C* Carico il SFL
067400110826     C                   EXSR      ROLS0
067500030113     C*
067600030114     C                   Z-ADD     1             WPAG
067700110726     c                   eval       sav_Wmax =  Wmax
067800110725     C*
067900110725     C                   ENDSR
068000110725     C/EJECT
068100110725      *---------------------------------------------------------------*
068200070320      *? CARICAMENTO PAGINA LISTA                                    ?
068300070305      *---------------------------------------------------------------*
068400110826     C     ROLS0         BEGSR
068500070305     C*
068600070305     C                   SETOFF                                       32
068700110831     C                   Z-ADD     1             Y
068800110826     C                   Z-ADD     WMAX          S0NRR
068900070305     C*
069000070305     C* Caricamento del Sfl x Giro
069100110826     c                   Exsr      Write_S0
069200070305     C*
069300110826     C                   Z-ADD     S0NRR         WMAX                 30
069400070305     C*
069500110826     C* POSIZIONAMENTO AL 0° RCD DELLA PAGINA
069600110826     C     S0NRR         DIV       S0pag         PAGINE            4 0
069700070305     C                   MVR                     RESTO             3 0
069800110826     C     PAGINE        MULT      S0pag         C0RCD
0699000703051    C     RESTO         IFGT      0
070000110826     C                   ADD       1             C0RCD
0701000703051E   C                   ELSE
070200110826     C                   SUB       S0pag         C0RCD
070300110826     C                   ADD       1             C0RCD
0704000703051-   C                   ENDIF
070500070305     C*
070600070305     C                   ENDSR
070700070301      *---------------------------------------------------------------*
070800070320      *?  Scrive x Ogni rottura di GIRO                              ?
070900070301      *---------------------------------------------------------------*
071000110826     C     Write_S0      BEGSR
071100070305      *
071200110826      *  Precarica da Ordine Pianificato
071300110826     c                   exsr      Legge_rec0
071400070228      *
071500070228      * Finchè il giro è lo stesso
0716001108311    C     $EFILE        DowEQ     *OFF
071700110826     C                   clear                   TII5S00
071800070228      *
071900110830      * Solo x ordini mensili deve impostare le sottoclassi
072000110830      *  degli articoli pianificati preselezionate con "1" e proteggere
072100110830      *   altre scelte.
072200110826     c                   exsr      carica_campi0
072300070305      *---------
072400110826     C                   ADD       1             S0NRR
072500070305     C                   ADD       1             Y
072600110826     C                   WRITE     TII5S00
072700070305      *---------
072800110826     c                   exsr      Legge_rec0
072900110825     C*
073000070228     c                   EndDO
073100070228     C*
073200070228     C                   ENDSR
073300110825      *---------------------------------------------------------------*
073400110825      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
073500110825      *---------------------------------------------------------------*
073600110826     C     LEGGE_REC0    BEGSR
073700110825     C*
073800110825     C*  Per EoF deve uscire
073900110825     C*  Per Record Ok deve uscire riportando i dati
074000110825     C                   MOVEL     *OFF          $EFILE
074100110825     C                   MOVEL     *OFF          $RCDOK
074200110825     C*
0743001108251    C     $EFILE        DOUEQ     *ON
074400110825     C     $RCDOK        OREQ      *ON
074500110825     C*
074600110826      * ? Se Ordine Pianificato deve caricare da file precaricato
074700110825     c****   LETTURA RECORD x Filiale
074800110829     c     SottoClas_156 reade     antab01l
074900110825      * Fine File
075000110826     C                   IF        %Eof(antab01l)
075100110825     C                   MOVEL     *ON           $EFILE
075200110825     C                   MOVE      $EFILE        *IN33
075300110825     C                   Else
075400110825      * ?Controlla se la Spedizione è da considerare
075500110825     C                   MOVEL     *ON           $RCDOK
075600110826      *
075700110826     C                   exsr      Check_RECord0
075800110826      *
075900110825     C                   END
076000110825     C*
0761001108251-   C                   ENDDO
076200110825     C*
076300110825     C                   ENDSR
076400110825      *---------------------------------------------------------------*
076500110825      *?   CONTROLLO il record letto se va bene                      ?
076600110825      *---------------------------------------------------------------*
076700110826     C     Check_Record0 BEGSR
076800110825     C*
076900110830     C*  Controlla se la sottoclasse è presente negli articoli pianificati
077000110830     C*   alotrimenti non visualizza la tabella
077100110830     c                   if        D1TORP = Pianificato
077200110830     C                   MOVE      TABCOD        SCLASSE           5
077300110830     C     SCLASSE       lookup    subClassi                              22
077400110830     c                   if        not *in22
077500110830     C                   MOVEL     *OFF          $RCDOK
077600110830     C                   End
077700110830     C                   END
077800110825     C*
077900110901     C*  Sottoclasse autorizzata alla Filiale 1°/2°Livello o x Logistica
078000110901     C                   MOVEl     TABdes        ANTAB156
078100110901     C*
078200110901     c                   if        FI1156V <> *blank and dutlpo = '1'
078300110901     c                               and §ogNTW <>'LOG'
078400110901     c                                or
078500110901     c                             FI2156V <> *blank and dutlpo = '2'
078600110901     c                               and §ogNTW <>'LOG'
078700110901     c                                or
078800110901     c                             dutlpo = 'S' and §ogNTW <>'LOG'
078900110901     c                                or
079000110901     c                             FI3156V <> *blank and §ogNTW = 'LOG'
079100110901     c                   MOVEL     *OFF          $RCDOK
079200110901     c                   end
079300110901     C*
079400110826     C     end_Check0    ENDSR
079500070301      *---------------------------------------------------------------*
079600110825      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
079700070301      *---------------------------------------------------------------*
079800110826     C     Carica_campi0 BEGSR
079900110726     C*
080000110830     c                   eval      s0DES =  tabDES
080100110830     c                   move      tabCOD        s0COD
080200110830     C*
080300110907     C                   SETOFF                                       34  32
080400110830     c                   if        D1TORP = Pianificato
080500110830     c                   move      '1'           s0sce
080600110907     C                   SETON                                        34  32
080700110830     C                   END
080800110825     C*
080900110726     C                   ENDSR
081000110726      *---------------------------------------------------------------*
081100110726      *?   CONTROLLO TESTATA LISTA                                   ?
081200110726      *---------------------------------------------------------------*
081300110826     C     CTRC0         BEGSR
081400110726     C*
081500070301     C                   MOVE      *OFF          *IN99
081600110726     C*
081700070301     C                   ENDSR
081800070301      *---------------------------------------------------------------*
081900070320      *?   CONTROLLO OPZIONI LISTA                                   ?
082000070301      *---------------------------------------------------------------*
082100110826     C     CTRS0         BEGSR
082200070301     C*
082300070301     C                   MOVEL     *OFF          $ESCI
082400110830     c                   clear                   ERRmsg80
082500070301     C                   SETOFF                                       99
082600070301     C*
082700070301     C* Leggo il sfl solo se ci sono rcd
0828000703011    C     WMAX          IFGT      0
082900110826     C                   READC     TII5S00                                21
083000070301     C*
083100070301     C* esce se fine sfl o errore che richiede l'uscita
0832000703012    C     *IN21         DOWEQ     *OFF
083300070301     C     $ESCI         ANDEQ     *OFF
083400110826     C                   Z-ADD     S0NRR         C0RCD
083500070301     C* ctrl su riga
083600110826     C                   EXSR      RECS0
083700070301      *
083800070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0839000703013    C     *IN99         IFEQ      *ON
084000070301     C     $ESCI         OREQ      *ON
084100070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
084200070301     C* la ripristinerò a conclusione del ciclo di READC
084300110826     C                   MOVE      *OFF          $INZS0
0844000703013-   C                   ENDIF
084500070301     C*
084600070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
084700070301     C                   MOVE      *ON           *IN32
084800110826     C                   UPDATE    TII5S00
084900070301     C*
085000070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0851000703013    C     $ESCI         IFEQ      *OFF
085200110826     C                   READC     TII5S00                                21
085300070301      *
085400070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0855000703014    C     *IN21         IFEQ      *ON
085600070301     C* calcolo pagina a cui deve posizionarsi
085700110826     c                   z-add     s0pag         Wpagine
085800070301     C                   EXSR      CLCPAG
0859000703014-   C                   ENDIF
0860000703013-   C                   ENDIF
086100070301     C*
0862000703012-   C                   ENDDO
086300070301     C*
0864000703011-   C                   ENDIF
086500110726     C* __________________________________________________
086600110826     C*  errori da evidenziare tramite finestra di ALLERTA
086700110826     c                   exsr      Errori
086800110726     C* __________________________________________________
086900070328     C*
087000070301     C                   ENDSR
087100070301     C/EJECT
087200070301      *---------------------------------------------------------------*
087300070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
087400070301      *---------------------------------------------------------------*
087500110826     C     RECS0         BEGSR
087600070301     C*
087700070301     C* reset indicatori DSPATR
087800070308     c                   exsr      INZ_indERR
087900070326     C*
088000110830     c                   setON                                        32
088100110826     C* gestione opzioni
0882001108263    C     S0sce         IFNE      *blank
088300110826     C     *IN99         ANDEQ     *OFF
088400110902     C**** $TASTO        ANDEQ     F06
088500110826      *
088600110908     c                   eval      AlmenoUnaSCl ='S'
088700110826     C                   EXSR      SCE_S0
088800110830     c                   setOFF                                       32
088900110830     c                   clear                   s0sce
089000110826      *
0891001108263-   C                   ENDIF
089200110826     C*
089300070327     C*
089400110826     C     endSF0        ENDSR
089500110826      *---------------------------------------------------------------*
089600110826      *?   GESTIONE OPZIONI LISTA                                    ?
089700110826      *---------------------------------------------------------------*
089800110826     C     SCE_S0        BEGSR
089900110826     C*
090000110826     C*  Selezionata la riga
090100110826     c                   If        s0SCE = '1'
090200110826     c                   eval      kpjbus = kpjbu
090300110826     C                   clear                   Tii501ds
090400110901     C                   move      Tipo_Filiale  i501livLPO
090500110826     C                   move      dutCOU        i501codute
090600110826     C                   move      knmus         i501knmus
090700110830     C                   move      c0DOR         i501weekAA
090800110826     C                   movel     d1week        i501weekAA
090900110826     C                   move      d1cfgs        i501filial
091000110826     C                   move      d1tORP        i501tipord
091100110826     C                   move      d1uffC        i501uffcom
091200110831     C                   move      *all'0'       i501sclass
091300110831     C                   move      s0cod         i501sclass
091400110826     C                   move      num_ordine    i501numord
091500110826     C                   move      data_ordine   i501dataor
091600110906     C                   move      *blank        i501cpyOPP
091700110830     C                   move      *blank        i501keyRET
091800110830     C                   move      *blank        i501wriUNO
091900110830      * se ordine PIanificato
092000110830     c                   if        D1TORP = Pianificato
092100110906     C                   move      'S'           i501cpyOPP
092200110830     c                   end
092300110826     C                   movel(p)  Tii501ds      KPJBU
092400110831     C                   call      'TII501R'
092500110826     C                   parm                    KPJBA
092600110830     C                   movel(p)  KPJBU         Tii501ds
092700110826     c                   movel     kpjbus        kpjbu
092800110830      * se è stata immessa almeno una riga di ordine
092900110830      ** per visualizzare in riepilogo.
093000110906     c                   If        i501wriUNO = 'S'
093100110906     c                   eval      scritto_almeno_una_riga_ordine = 'S'
093200110830     c                   end
093300110908      * deve uscire
093400110908     c                   If        i501keyRET = F03
093500110901     C                   EXSR      F03_exit
093600110901     c                   end
093700110908      * deve tornare alla scelta sottoclassi
093800110908     c                   If        i501keyRET = F12
093900110908     C                   eval      $ESCI = *on
094000110909     c                   eval      $GEST = 'S0'
094100110909     c******             seton                                        99
094200110908     c                   end
094300110826     c                   endIf
094400110826     C*
094500110826     C                   ENDSR
094600110826     C/EJECT
094700110826      *---------------------------------------------------------------*
094800110830      *  ?  Carica le Sottoclassi presenti su articoli PIANIFICATI   ?
094900110826      *---------------------------------------------------------------*
095000110830     C     carica_subclasBEGSR
095100110830     C*
095200110830     c                   clear                   SubClassi
095300110906     C                   eval        oppFIL = d1cfgs
095400110906     C                   eval        oppUFF = d1uffc
095500110830     C*
095600110906     c     kTMopp        setll     tMopp01l
095700110906     c     kTMopp        reade     tMopp01l
095800110906     c                   dow       not %EoF(tMopp01l)
095900110830     c*
096000110830     c* cerca decodifiche su file articoli
096100110906     c     oppMAT        chain     tmAMP01L
096200110830     c                   if        %Found(tmAMP01L)
096300110830     c*
096400110830     C                   Z-ADD     1             SCL               3 0
096500110830     C     ampSCM        LOOKUP    SubClassi(SCL)                         22
096600110830     c*
096700110830      * Solo se non c'è la imposta in schiera.
096800110830     C                   If        *IN22 = *oFF
096900110830     C                   Z-ADD     1             SCL               3 0
097000110830     C     *blank        LOOKUP    SubClassi(SCL)                         22
097100110830     c* lo aggiunge
097200110830     C                   If        *IN22 = *oN
097300110830     C                   eval      SubClassi(SCL) = ampSCM
097400110830     c                   end
097500110830     c                   end
097600110830     c*
097700110830     c                   end
097800110830     c*
097900110906     c     kTMopp        reade     tMopp01l
098000110830     c                   enddo
098100110830     C*
098200110830     C                   ENDSR
098300110830     C/EJECT
098400110830      *---------------------------------------------------------------*
098500110830      *  ?  GESTIONE SFL di immissione righe di ordine               ?
098600110830      *---------------------------------------------------------------*
098700110830     C     GESS1         BEGSR
098800110830     C*
098900110826     C* inizializzazione videata
099000110826     C     $INZS1        IFEQ      *ON
099100110826     C                   EXSR      INZS1
099200110826     C                   MOVE      *OFF          $INZS1
099300110826     C                   ENDIF
099400110826     C*
099500110826     C* Inizializza e imposta piede videata
099600110826     C                   EXSR      INZZ1
099700110826     C*
099800110826     C* emissione piede videata
099900110826      * ?            *-----------------------*
100000110826     C                   WRITE     TII5Z01
100100110826      * ?            *-----------------------*
100200110826     C*
100300110826     C     WMAX          IFEQ      0
100400110826     C* Non ci sono records
100500110826      * ?            *-----------------------*
100600110826     C                   WRITE     TII5V01
100700110826      * ?            *-----------------------*
100800110826     c                   setoff                                       30
100900110826     C                   Else
101000110826     c                   seton                                        30
101100110826     c                   eval       sav_WSfl =  WSfl
101200110826     c                   eval       sav_Wpag =  Wpag
101300110826      *
101400110826     C     Wsfl          IFgt      0
101500110826     C                   Z-ADD     wsfl          C1RCD
101600110826     C                   Else
101700110826     C     Wpag          IFgt      0
101800110826     C                   Z-ADD     wpag          C1RCD
101900110826     C                   EndIF
102000110826     C                   EndIF
102100110826     C                   ENDIF
102200110826      *
102300110908      *    se pianificato 35-ON
102400110908     c                   setoFF                                       35
102500110908     c                   if        D1TORP = Pianificato
102600110908     c                   setoN                                        35
102700110908     C                   EndIF
102800110908      *
102900110905      * se deve rivedere un ordine già immesso
103000110905     c                   move      rev_ordine    *in12
103100110908      *
103200110826      *  Emissione SFL dei GIRI
103300110826      * ?            *-----------------------*
103400110826     C                   EXFMT     TII5C01
103500110826      * ?            *-----------------------*
103600110826     c                   setoff                                       99
103700110826     C                   movel     'S1'          sav$GEST
103800110826      *
103900110826     C     C1NRR         IFNE      0
104000110826     C                   Z-ADD     C1NRR         WSFL
104100110826     C                   ENDIF
104200110826     C                   Z-ADD     SFLNRR        C1RCD
104300110826     C* Selezioni
1044001108261    C                   SELECT
104500110826     C* F3=Fine
104600110826     C     $TASTO        WHENEQ    F03
104700110826     C                   EXSR      F03_exit
104800110826      *
104900110826     C* F12=Ritorno
105000110826     C     $TASTO        WHENEQ    F12
105100110826     c                   move      'S0'          $GEST
105200110826     C                   MOVE      *ON           $INZS0
105300110826      *
1054001108261O   C                   OTHER
105500110826     C* CONTROLLO testata
105600110826     C                   EXSR      CTRC1
105700110826     C* CONTROLLO subfile
105800110826     C     *IN99         IFEQ      *OFF
105900110826     C                   EXSR      CTRS1
106000110826     C                   END
106100110826      *
106200110826      * ? Scrive   l'ordine
106300110826     C     *IN99         IFEQ      *OFF
106400110826     c     ERRmsg80      andEQ     *blank
106500110826     C     WMAX          andGT     0
106600110905     C     $TASTO        ifeq      F10
106700110905     C     $TASTO        oreq      F08
106800110905      **
106900110902     C* F10=Valida l'Ordine come NON PIU' manutenibile
107000110905     C     $TASTO        ifeq      F10
107100110906     C                   EXSR      F10_Chiude_ORD
107200110905     c                   end
107300110905      **
107400110826     C                   MOVE      *ON           $INZD1
107500110826     C                   movel     'D1'          $GEST
107600110826     c                   end
107700110905     c                   end
107800110826      *
1079001108261-   C                   ENDSL
108000110826     C*
108100110826     C                   ENDSR
108200110826     C/EJECT
108300110826      *---------------------------------------------------------------*
108400110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
108500110826      *---------------------------------------------------------------*
108600110826     C     INZZ1         BEGSR
108700110826     C*
108800110826     C* Messaggio o tasti funzionali
108900110826     c                   clear                   z1dmsg
109000110826     c                   clear                   ERRmsg80         80
109100110826     C*
109200110826      * imposta eventuali errori
109300110826     c                   eval      z1dmsg = ERRmsg80
109400110826     C*
109500110826     C                   ENDSR
109600110826     C/EJECT
109700110826      *---------------------------------------------------------------*
109800110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
109900110826      *---------------------------------------------------------------*
110000110826     C     INZS1         BEGSR
110100110826     C*
110200110826     C* reset indicatori DSPATR
110300110826     c                   exsr      INZ_indERR
110400110826     C* pulizia SFL
110500110908     C                   SETOFF                                       3031
110600110826     C                   WRITE     TII5C01
110700110826     C                   SETON                                          31
110800110826     C*
110900110826     C* Righe x Pagina del 1° SFL
111000110826     c                   z-add     RigPag_SFl1   s1pag
111100110826      *
111200110826     C* CARICAMENTO SFL totale
111300110826     C                   Z-ADD     0             S1NRR
111400110826     C                   Z-ADD     1             C1RCD
111500110826     C                   Z-ADD     0             WMAX
111600110826     C                   Z-ADD     0             Wsfl
111700110826     C* campi titolo
111800110826     C                   MOVEL     dsPGM         c1CPGM
111900110826     C                   MOVEL     knsif         c1nsif
112000110826     C                   MOVEL     knmus         c1nmus
112100110826     C                   MOVEL     RSUT          c1crsu
112200110826     C                   z-add     settimana     c1week
112300110830     C                   z-add     num_ordine    c1NOR
112400110830     C                   z-add     data_ordine   c1DOR
112500110826      **----------------
112600110826     C* Posizionamento su file pilota
112700110830     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
112800110826     C                   eval        $EFILE    = *OFF
112900110906     C     c1NOR         setll     tmORP02L
113000110826      **----------------
113100110826     C* Carico il SFL
113200110826     C                   EXSR      ROLS1
113300110826     C*
113400110826     C                   Z-ADD     1             WPAG
113500110826     c                   eval       sav_Wmax =  Wmax
113600110826     C*
113700110826     C                   ENDSR
113800110826     C/EJECT
113900110826      *---------------------------------------------------------------*
114000110826      *? CARICAMENTO PAGINA LISTA                                    ?
114100110826      *---------------------------------------------------------------*
114200110826     C     ROLS1         BEGSR
114300110826     C*
114400110826     C                   SETOFF                                       32
114500110831     C                   Z-ADD     1             Y
114600110826     C                   Z-ADD     WMAX          S1NRR
114700110826     C*
114800110826     C* Caricamento del Sfl x Giro
114900110826     c                   Exsr      Write_S1
115000110826     C*
115100110826     C                   Z-ADD     S1NRR         WMAX                 30
115200110826     C*
115300110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
115400110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
115500110826     C                   MVR                     RESTO             3 0
115600110826     C     PAGINE        MULT      S1pag         C1RCD
1157001108261    C     RESTO         IFGT      0
115800110826     C                   ADD       1             C1RCD
1159001108261E   C                   ELSE
116000110826     C                   SUB       S1pag         C1RCD
116100110826     C                   ADD       1             C1RCD
1162001108261-   C                   ENDIF
116300110826     C*
116400110826     C                   ENDSR
116500110826      *---------------------------------------------------------------*
116600110826      *?  Scrive x Ogni rottura di GIRO                              ?
116700110826      *---------------------------------------------------------------*
116800110826     C     Write_S1      BEGSR
116900110826      *
117000110826      *  Precarica da Ordine Pianificato
117100110826     c                   exsr      Legge_rec1
117200110826      *
117300110826      * Finchè il giro è lo stesso
1174001108311    C     $EFILE        DowEQ     *OFF
117500110826     C                   clear                   TII5S01
117600110826      *
117700110826      * Solo x ordini mensili
117800110826     c                   exsr      carica_campi1
117900110826      *---------
118000110826     C                   ADD       1             S1NRR
118100110826     C                   ADD       1             Y
118200110826     C                   WRITE     TII5S01
118300110826      *---------
118400110826     c                   exsr      Legge_rec1
118500110826     c                   EndDO
118600110826     C*
118700110826     C                   ENDSR
118800110826      *---------------------------------------------------------------*
118900110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
119000110826      *---------------------------------------------------------------*
119100110826     C     LEGGE_REC1    BEGSR
119200110826     C*
119300110826     C*  Per EoF deve uscire
119400110826     C*  Per Record Ok deve uscire riportando i dati
119500110826     C                   MOVEL     *OFF          $EFILE
119600110826     C                   MOVEL     *OFF          $RCDOK
119700110826     C*
1198001108261    C     $EFILE        DOUEQ     *ON
119900110826     C     $RCDOK        OREQ      *ON
120000110826     C*
120100110830      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
120200110830     c****   LETTURA RECORD x N°ORDINE
120300110906     C     c1NOR         reade     tmORP02L
120400110826      * Fine File
120500110906     C                   IF        %Eof(tmORP02l)
120600110826     C                   MOVEL     *ON           $EFILE
120700110826     C                   MOVE      $EFILE        *IN33
120800110826     C                   Else
120900110826      * ?Controlla se la Spedizione è da considerare
121000110826     C                   MOVEL     *ON           $RCDOK
121100110826     C                   exsr      Check_RECord1
121200110826     C                   END
121300110826     C*
1214001108261-   C                   ENDDO
121500110826     C*
121600110826     C                   ENDSR
121700110826      *---------------------------------------------------------------*
121800110826      *?   CONTROLLO il record letto se va bene                      ?
121900110826      *---------------------------------------------------------------*
122000110826     C     Check_Record1 BEGSR
122100110826     C*
122200110826     C*
122300110826     C     end_check1    ENDSR
122400110826      *---------------------------------------------------------------*
122500110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
122600110826      *---------------------------------------------------------------*
122700110826     C     Carica_campi1 BEGSR
122800110826     C*
122900110908     c                   setoff                                       34
123000110908     c                   eval      scritto_almeno_una_riga_ordine ='S'
123100110908     C*
123200110826     C*  carica i campi e li protegge
123300110902     c                   z-add     orp_nrr       h1nrrorp
123400110830     c                   eval      s1MAT =  orpMAT
123500110908     C*
123600110908     C* i Sospesi possono essere solamente per Ordini pianificati e di
123700110908     C*   articoli Obbligatori.
123800110906     c                   if        orpATT = 'S'
123900110906     c                   eval      s1selSOS = orpATT
124000110908     c                   eval      h1selSOS = orpATT
124100110906     c                   move(p)   'SOSP'        s1sospeso
124200110906     c                   end
124300110908      **
124400110906     c                   if        orpATT = 'A'
124500110908     c                   eval      h1sosp = 'A'
124600110908     c                   eval      h1selSOS = orpATT
124700110906     c                   eval      s1selSOS = *blank
124800110906     c                   move(p)   '*SOSP'       s1sospeso
124900110906     c                   end
125000110908      **
125100110830     c                   eval      s1QTA =  orpQTA
125200110830     c                   if        orpNOTE <> *blank
125300110830     c                   move      '*'           s1not
125400110908     c                   move      '*'           h1not
125500110830     c                   eval      h1NOTA1 = %Subst(orpNOTE:01:60)
125600110830     c                   eval      h1NOTA2 = %Subst(orpNOTE:61:60)
125700110830     c                   eval      h1NOTA3 = %Subst(orpNOTE:121:60)
125800110830     c                   eval      h1NOTA4 = %Subst(orpNOTE:181:60)
125900110830     c                   end
126000110826     C*
126100110826     C* decodifiche Anagrafica articoli
126200110826     c     s1mat         chain     tmAMP01l
126300110826     c                   if        %Found(tmAMP01L)
126400110826     c                   eval      s1DES =  AMPDMT
126500110826     c                   eval      s1UMA =  AMPUMA
126600110826     c                   else
126700110826     c                   eval      s1DES =  *all'*'
126800110826     c                   eval      s1UMA =  *all'*'
126900110826     c                   end
127000110908     C*
127100110908     C* SOLO PER ORDINI PIANIFICATI su articoli obbligatori
127200110908     C*   SPROTEGGO il campo sospensione
127300110908     c                   if           d1TORP = Pianificato
127400110908     C*      Classe Merceologica
127500110908     c                   clear                   obbligatorio      1
127600110908     c                   clear                   facoltativo       1
127700110908     c                   clear                   a_consumo         1
127800110908     C*
127900110908     c                   move      Classe_155    tabGRU
128000110908     c                   move      *all'0'       tabCOD
128100110908     c                   move      ampCLM        tabCOD
128200110908     c     kAnTAB        chain     antab01l
128300110908     c                   if          %Found(AnTAB01l)
128400110908     c                   eval      antab155 = tabDES
128500110908     c                   eval         obbligatorio = PE1155v
128600110908     c                   eval         facoltativo  = PE2155v
128700110908     c                   eval         a_consumo    = PE3155v
128800110908      ****
128900110908     c                   if          obbligatorio <> *blank and h1sosp <>'A'
129000110908     C                   setON                                        34
129100110908     c                   end
129200110908     c                   end
129300110908     c                   end
129400110908     C*
129500110826     C                   ENDSR
129600110826      *---------------------------------------------------------------*
129700110826      *?   CONTROLLO TESTATA LISTA                                   ?
129800110826      *---------------------------------------------------------------*
129900110826     C     CTRC1         BEGSR
130000110826     C*
130100110826     C                   MOVE      *OFF          *IN99
130200110826     C*
130300110826     C                   ENDSR
130400110826      *---------------------------------------------------------------*
130500110826      *?   CONTROLLO OPZIONI LISTA                                   ?
130600110826      *---------------------------------------------------------------*
130700110826     C     CTRS1         BEGSR
130800110826     C*
130900110826     C                   MOVEL     *OFF          $ESCI
131000110826     c                   clear                   ERRmsg80
131100110826     C                   SETOFF                                       99
131200110826     C*
131300110826     C* Leggo il sfl solo se ci sono rcd
1314001108261    C     WMAX          IFGT      0
131500110826     C                   READC     TII5S01                                21
131600110826     C*
131700110826     C* esce se fine sfl o errore che richiede l'uscita
1318001108262    C     *IN21         DOWEQ     *OFF
131900110826     C     $ESCI         ANDEQ     *OFF
132000110826     C                   Z-ADD     S1NRR         C1RCD
132100110826     C* ctrl su riga
132200110826     C                   EXSR      RECS1
132300110826      *
132400110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1325001108263    C     *IN99         IFEQ      *ON
132600110826     C     $ESCI         OREQ      *ON
132700110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
132800110826     C* la ripristinerò a conclusione del ciclo di READC
132900110826     C                   MOVE      *OFF          $INZS1
1330001108263-   C                   ENDIF
133100110826     C*
133200110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
133300110826     C                   MOVE      *ON           *IN32
133400110826     C                   UPDATE    TII5S01
133500110826     C*
133600110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1337001108263    C     $ESCI         IFEQ      *OFF
133800110826     C                   READC     TII5S01                                21
133900110826      *
134000110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1341001108264    C     *IN21         IFEQ      *ON
134200110826     C* calcolo pagina a cui deve posizionarsi
134300110826     c                   z-add     s1pag         Wpagine
134400110826     C                   EXSR      CLCPAG
1345001108264-   C                   ENDIF
1346001108263-   C                   ENDIF
134700110826     C*
1348001108262-   C                   ENDDO
134900110826     C*
1350001108261-   C                   ENDIF
135100110826     C* __________________________________________________
135200110826     C*  errori da evidenziare tramite finestra di ALLERTA
135300110826     c                   exsr      errori
135400110826     C* __________________________________________________
135500110826     C*
135600110826     C                   ENDSR
135700110826     C/EJECT
135800110826      *---------------------------------------------------------------*
135900110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
136000110826      *---------------------------------------------------------------*
136100110826     C     RECS1         BEGSR
136200110826     C*
136300110826     C* reset indicatori DSPATR
136400110826     c                   exsr      INZ_indERR
136500110826     C*
136600110830     c* se è richiesta manutenzione NOTE
136700110830     C* gestione opzioni
1368001109063    C     s1selNOT      IFNE      *blank
136900110830     C     *IN99         ANDEQ     *OFF
137000110830     C                   EXSR      SCE_S1
137100110906     c                   clear                   s1selNOT
1372001108303-   C                   ENDIF
137300110826     C*
137400110906     c                   clear                   s1sospeso
1375001109083    C     s1selSOS      IFNE      h1selsos
137600110906     C     *IN99         ANDEQ     *OFF
137700110906     C                   EXSR      SCE_S1
1378001109063-   C                   ENDIF
137900110906     C*
138000110826     C     endSF1        ENDSR
138100110830      *---------------------------------------------------------------*
138200110830      *?   GESTIONE OPZIONE NOTE                                     ?
138300110830      *---------------------------------------------------------------*
138400110830     C     SCE_S1        BEGSR
138500110830     C*
138600110908     c                   clear                   cambia_ORP        1
138700110909     C*
138800110909     C*  INSERIMENTO NOTE:
138900110909     C*  ----------------
139000110908     C*
1391001109063    C     s1selNOT      IFNE      *blank
139200110909     C*
139300110908     c                   clear                   s1selNOT
139400110909     c                   clear                   NOTfree
139500110909     C*
139600110830     c                   eval      notMAT = s1MAT
139700110830     c                   eval      notDES = s1DES
139800110830     c                   eval      notQTA = s1QTA
139900110830     c                   eval      not001 = h1nota1
140000110830     c                   eval      not002 = h1nota2
140100110830     c                   eval      not003 = h1nota3
140200110830     c                   eval      not004 = h1nota4
140300110830     C*
140400110830     C*  Selezionata la riga per visualizzare le note
140500110830     C                   exfmt     TII5NOT
140600110908     C*
140700110908     c                   clear                   s1not
140800110908     c                   if             not001 <> *blank or
140900110908     c                                  not002 <> *blank or
141000110908     c                                  not003 <> *blank or
141100110908     c                                  not004 <> *blank
141200110908     c                   movel     '*'           s1not
141300110908     c                   end
141400110908     C*
141500110902     c                   eval       h1nota1 = not001
141600110902     c                   eval       h1nota2 = not002
141700110902     c                   eval       h1nota3 = not003
141800110902     c                   eval       h1nota4 = not004
141900110906     C*
142000110907     c                   end
142100110908     C*
142200110909     C*  SOSPENSIONE ARTICOLO:
142300110909     C*  --------------------
142400110909     C*
142500110907     C* Deve comunque aggiornare x NOTE oppure se sospeso
142600110908     c                   if          s1not <> h1not
142700110907     c                                  or
142800110908     c                               s1SELsos <> h1SELsos
142900110909      **
143000110909     C*  se rilevata variazione sulla sospensione di qualche articolo
143100110909     c                   if          s1SELsos <> h1SELsos
143200110909     c                   eval         Err001 = errmsg(13)
143300110909     c                   eval         Err002 = errmsg(14)
143400110909     c                   eval         Err003 = *blank
143500110909     c                   end
143600110909     C*
143700110908     c                   eval      h1not = s1not
143800110908     C*
143900110906     c                   open      tmorp00f
144000110906     c     h1nrrorp      chain     tmorp00f
144100110906     c                   if        %Found(tmorp00f)
144200110906      *
144300110902     c                   eval      orpNOTE = h1nota1 + h1nota2 + h1nota3 +
144400110902     c                                       h1nota4
144500110906      *
144600110908     c                   if        s1SELsos <> h1SELsos  and
144700110906     c                               h1sosp = *blank
144800110906      * sospeso dalla Filiale --> "S"
144900110908     c                   eval      orpATT = s1SELsos
145000110908     c                   clear                   s1sospeso
145100110908     c                   if        orpatt <> *blank
145200110908     c                   move      'SOSP'        s1sospeso
145300110906     c                   end
145400110908     c                   end
145500110906      *  invece
145600110906      * sospeso da Sede --> "A"
145700110906     c                   if          h1sosp = 'A'
145800110908     c                   move      '*SOSP'       s1sospeso
145900110906     c                   end
146000110906      *
146100110906     c                   update    tmorpfis
146200110906     c                   close     tmorp00f
146300110902     c                   end
146400110907     C*
146500110907     c                   end
146600110830     C*
146700110830     C                   ENDSR
146800110830     C/EJECT
146900070302      *---------------------------------------------------------------*
147000070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
147100070302      *---------------------------------------------------------------*
147200070302     C     F06_Aggiorna  BEGSR
147300070306      **
147400070305     C*
147500070302     C                   ENDSR
147600110826     C/EJECT
147700110826      *---------------------------------------------------------------*
147800110826      *?  GESTIONE F10 x imissione Multipla                          ?
147900110826      *---------------------------------------------------------------*
148000110906     C     F10_chiude_ORDBEGSR
148100110826      *
148200110906     c                   eval      premuto_F10 = 'S'
148300110906      *
148400110830      *    imposta il flag di fine manutenzioni Ordine per essere poi confermato
148500110902      **
148600110902      **----------------
148700110902     C* Posizionamento su file pilota
148800110902     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
148900110902     C                   eval        $EFILE    = *OFF
149000110906     C     c1NOR         setll     tmORP02L
149100110902     C                   MOVEL     *OFF          $EFILE
149200110902     C                   MOVEL     *OFF          $RCDOK
149300110902     C*
1494001109021    C     $EFILE        DOUEQ     *ON
149500110902     C*
149600110902      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
149700110906     C     c1NOR         reade     tmORP02L
149800110902      * Fine File
149900110906     C                   IF        %Eof(tmORP02l)
150000110902     C                   MOVEL     *ON           $EFILE
150100110902     C                   Else
150200110902      * ?aggiorna
150300110902     C                   MOVEL     *ON           orpCONF
150400110906     C                   update    tmORP000
150500110902     C                   END
150600110902     C*
1507001109021-   C                   ENDDO
150800110902     C*
150900110902      **----------------
151000110902      **
151100110826     C                   ENDSR
151200110826     C/EJECT
151300110826      *---------------------------------------------------------------*
151400110826      *?  GESTIONE F03 x uscire LR                                   ?
151500110826      *---------------------------------------------------------------*
151600110826     C     F03_exit      BEGSR
151700110909      **
151800110909     C* se si è su un altro video rispetto a prima occorre resettare
151900110909      **  la finestra con il messaggio di avviso.
152000110909     c                   if        cambia_video = 'S'
152100110909     c                   eval      dopo_alert_window    = *blank
152200110909     c                   end
152300110906     C* fine programma
152400110906     c****
152500110906      *  se ha emesso la finestra di allerta per un ordine non CONFERMATO
152600110906      *   in DEFINITIVA deve eseguire la cancellazione dell'ordine per poi
152700110906      *    uscire forzatamente.
152800110906     c                   if           dopo_alert_window ='S' and
152900110906     c                             scritto_almeno_una_riga_ordine = 'S'
153000110906     c                                and premuto_F10 = *blank
153100110906     c                                and   D1TORP = aConsumo
153200110906     C                   MOVE      'CO'          $GEST
153300110906     c                   end
153400110906     c****
153500110906      *   deve emettere la prima volta la Window di avviso per
153600110906      *     Ordine NON CONFERMATO DEFINITIVAMENTE
153700110906     c                   if        scritto_almeno_una_riga_ordine = 'S'
153800110906     c                                and premuto_F10 = *blank  and
153900110906     c                                dopo_alert_window = *blank
154000110906     c                                and   D1TORP = aConsumo
154100110906      *
154200110906     c                   eval      Err001 = ErrMsg(11)
154300110909     c                   eval      Err002 = ErrMsg(15)
154400110906     c                   eval      Err003 = ErrMsg(12)
154500110906      *   Messaggio errore a Finestra
154600110906     C                   MOVE      *off          dopo_in_$FINE
154700110906     C                   MOVE      'W1'          $GEST
154800110906     c                   end
154900110906      *
155000110906      * Se ha scritto DEFINITIVAMENTE L'Ordine può uscire dal programma
155100110906      **  oppure
155200110906      * Se non ha fatto nulla può uscire dal programma
155300110906     c                   if        scritto_almeno_una_riga_ordine = 'S'
155400110906     c                                and premuto_F10 = 'S'
155500110906     c                                       or
155600110906     c                             scritto_almeno_una_riga_ordine = *blank
155700110908     c                                       or
155800110908     c                             D1TORP = Pianificato
155900110906     C                   MOVE      *ON           $FINE
156000110906     c                   end
156100110826      *
156200110826     C                   ENDSR
156300110826     C/EJECT
156400110825      *---------------------------------------------------------------*
156500110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
156600110825      *---------------------------------------------------------------*
156700110825     C     CLCPAG        BEGSR
156800110825     C* Input :
156900110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
157000110825     C*          cursore
157100110825     C* - Wpagine= numero rcd per pagina sfl
157200110825     C* Output :
157300110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
157400110825     C*
157500110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
157600110825     C                   MVR                     RESTO             3 0
157700110825     C     RESTO         IFGT      0
157800110825     C                   ADD       1             PAGINE
157900110825     C                   ENDIF
158000110825     C     PAGINE        MULT      Wpagine       WPAG
158100110825     C*
158200110825     C                   ENDSR
158300110825     C************************************************************
158400110825      *?  INIZIALIZZA indicatori di errore                           ?
158500110825     C************************************************************
158600110825     C     INZ_indERR    BEGSR
158700110825      *
158800110825     C                   move      *ALL'0'       IN5190           40
158900110825     C                   moveA     IN5190        *IN(51)
159000110825      *
159100110825     C                   ENDSR
159200070301     C/EJECT
159300070301      *---------------------------------------------------------------*
159400070316      *  ?  OPERAZIONI INIZIALI      ?
159500070301      *---------------------------------------------------------------*
159600940131     C     *INZSR        BEGSR
159700110726     C*
159800110908     C*  chiavi x files.
159900110824     c     kTnTBE        klist
160000110824     C                   KFLD                    tbeCOD
160100110824     C                   KFLD                    tbeKE1
160200110824     C*
160300110908     c     kAnTAB        klist
160400110908     C                   KFLD                    tabGRU
160500110908     C                   KFLD                    tabCOD
160600110908     C*
160700110906     c     kTmORP        klist
160800110824     C                   KFLD                    orpFIL
160900110905     C                   KFLD                    orpUFF
161000110824     C                   KFLD                    orpANNO
161100110824     C                   KFLD                    orpWEEK
161200110905     C                   KFLD                    orpPIAN
161300110824     C*
161400110906     c     kTMopp        klist
161500110906     C                   KFLD                    oppFIL
161600110906     C                   KFLD                    oppUFF
161700110830     C*
161800030113     C* Reperimento parametri
161900030113     C     *ENTRY        PLIST
162000030113     C                   PARM                    KPJBA
162100070228      *
162200110726      *  Se è EDP
162300070308     c                   CLEAR                   se_EDP            1
162400070308     c                   if        %subst(Knmus:1:3) = 'EDP'
162500070308     c                   MOVE      'S'           se_EDP
162600070307     c                   end
162700070301      *
162800110725     C     *DTAARA       DEFINE                  DTAORC                         *
162900110725     C*
163000070305     C*
163100070227      * Reperisco dati job
163200070227     c                   exsr      DatiJob
163300110901     C*  Controlla il P.O.
163400110901     c                   movel     dutLPO        Tipo_Filiale
163500110901     C     d1cFGS        CHAIN     AZORG01l
163600110901      * Non presente in organigramma
163700110901    1C                   IF        %found(azorg01l)
163800110901     c                   eval      og143 = orgDE3
163900110901     c                   if        §ogNTW = 'LOG'
164000110901     c                   movel     'L'           Tipo_Filiale
164100110901     c                   end
164200110901    1C                   ENDIF
164300110901     C*
164400070227
164500030113     C* Variabili per gestione videate
164600030113     C                   MOVE      *BLANK        $GEST             2
164700070305     C                   MOVE      *BLANK        sav$GEST          2
164800110906     C                   MOVE      *off          $FINE             1
164900110906     C                   MOVE      *off          dopo_in_$FINE     1
165000110906     C                   MOVE      *off          $INZD1            1
165100110906     C                   MOVE      *off          $INZS1            1
165200110906     C                   MOVE      *off          $INZS0            1
165300110906     C                   MOVE      *off          $EFILE            1
165400110906     C                   MOVE      *off          $ESCI             1
165500110906     C                   MOVE      *off          $RCDOK            1
165600030113     C* Indici
165700030113     C                   Z-ADD     0             X                 3 0
165800030113     C                   Z-ADD     0             Y                 3 0
165900070227     C***
166000070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
166100070227     C                   TIME                    W0140            14 0
166200110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
166300110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
166400070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
166500070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
166600070227     C*
166700110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
166800070227     C                   MOVEL     *BLANK        G02ERR
166900070227     C                   CALL      'XSRDA8'
167000070227     C                   PARM                    WLBDAT
167100110825     c                   Z-ADD     G02INV        WoggiAMG          8 0          *AAAA/MM/GG
167200110825     c                   eval      dataiso = %date(WoggiAMG:*iso)
167300110824     c                   eval      dataeur = dataiso
167400110825     c                   eval      oggiiso = dataiso
167500110824     c                   clear                   settimana         2 0
167600110824     C*
167700110824     C* NR.Settimana in corso
167800110824     C/EXEC SQL
167900110831     C+ SELECT week_iso(:OGGIISO) INTO :settimana FROM tmAMP00F
168000110824     C/END-EXEC
168100100505     C*
168200940117     C                   ENDSR
168300070227      *---------------------------------------------------------------*
168400070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
168500070227      *---------------------------------------------------------------*
168600070228     c     DatiJOB       BEGSR
168700070227      *
168800070227     c     *dtaara       define    §azute        azuteds
168900070227     c     *dtaara       define    §datiute      ddatiute
169000070227      *
169100070227     c                   in(E)     *dtaara
169200070227     c                   IF        %ERROR or RSUT = *blanks
169300070227     c                   clear                   Tibs34Ds
169400070227     c                   call      'TIBS34R'
169500070227     c                   parm                    Tibs34Ds
169600070227     c                   in        *dtaara
169700070227     c                   ENDIF
169800080606      *
169900070227     c                   ENDSR
170000110825      *---------------------------------------------------------------*
170100110825      *? CARICAMENTO PAGINA LISTA                                    ?
170200110825      *---------------------------------------------------------------*
170300110825     c     New_Ordine    begSR
170400110825     C*
170500110825     C                   z-add     0             Conta_Tenta       3 0
170600110826     C                   z-add     0             Num_Ordine        5 0
170700110826     C                   z-add     0             Data_Ordine       8 0
170800110825     C*
170900110825     C*- LETTURA NUMERO ORDINE NUOVO DA AREA DATI
171000110825     C     SULOCK        TAG                                                    *
171100110825     C*          -------------                             *
171200110825     C     *LOCK         IN        DTAORC                               46      *
171300110825     C   46              eval      Conta_Tenta = Conta_Tenta + 1
171400110825     C   46              if        Conta_Tenta = 999
171500110825     C                   GOTO      No_numero                                    *
171600110825     c                   end
171700110825     C   46              GOTO      SULOCK                                       *
171800110826     C                   Z-ADD     ORDNUO        Num_Ordine                     *
171900110825     C                   ADD       1             ORDNUO                         *
172000110825     C                   OUT       DTAORC                                       *
172100110825     c                   move      'S'           ordine_OK
172200110826     C                   MOVE      woggi         Data_Ordine
172300110825     C*
172400110825     C     No_numero     ENDSR
172500110825     C/EJECT
172600110826      * ?-------------------------------------------------------------*?
172700110826      *?   SUBROUTINE Autorizzazione                                 ?
172800110826      * ?-------------------------------------------------------------*?
172900110826     C     GESAU         BEGSR
173000110826      *
173100110826     C* Se sarà un profilo Abilitato, imposta l'Entrata dal Primo Video
173200110826     C                   MOVE      'D1'          $GEST
173300110826     C*
173400110826      * ?- Controlla autorizzazione sull'utente
173500110826     c                   movel     dutpou        i70pog
173600110826     C* Il p.o.deve essere impostato in base al profilo
173700110826     C     dutlpo        IFEQ      '2'
173800110826     C                   MOVEL     dutpou        i70pge                         *FIL.GESTIONE
173900110826     C                   ELSE
174000110826     C                   MOVEL     SIMFEL        i70pge
174100110826     C                   END
174200110826     c                   movel     knmus         i70prf
174300110826     c                   call      'FICN70R'
174400110826     c                   parm                    ficn70ds
174500110826      *
174600110826     c                   movel     o70uni        daut
174700110826      *
174800110901     c* utente NON abilitato alla gestione magazzino OPPURE IN SEDE
174900110901     c                   if        §autmag <>'S' or dutLPO ='S'
175000110826      *
175100110906     c                   eval      Err001 = ' '
175200110906     c                   eval      Err002 = ErrMsg(3)
175300110826     c                   eval      Err003 = ' '
175400110826      *   Messaggio errore a Finestra
175500110906     C                   MOVE      *on           dopo_in_$FINE
175600110826     C                   MOVE      'W1'          $GEST
175700110826      *
175800110826     c                   end
175900110826     c*
176000110826     C     endAU         ENDSR
176100110826     C************************************************************
176200110906      *?  Gestione Fine programma                                ?
176300110826     C************************************************************
176400110906     C     GESCO         BEGSR
176500110826     C*
176600110906     C*  Cancella l'ordine immesso ed esce
176700110906      **----------------
176800110906     C* Posizionamento su file pilota
176900110906     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
177000110906     C                   eval        $EFILE    = *OFF
177100110906     C     c1NOR         setll     tmORP02L
177200110906     C                   MOVEL     *OFF          $EFILE
177300110906     C                   MOVEL     *OFF          $RCDOK
177400110906     C*
1775001109061    C     $EFILE        DOUEQ     *ON
177600110906     C*
177700110906      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
177800110906     C     c1NOR         reade     tmORP02L
177900110906      * Fine File
178000110906     C                   IF        %Eof(tmORP02l)
178100110906     C                   MOVEL     *ON           $EFILE
178200110906     C                   Else
178300110906      * ?cancella x uscita forzata da pgm senza convalidare
178400110906     C                   delete    tmORP000
178500110906     C                   END
178600110906     C*
1787001109061-   C                   ENDDO
178800110906     C*
178900110906      **----------------
179000110906     C*
179100110906     C*   quindi esce dal programma
179200110906     c                   eval      $FINE = *ON
179300110906     c*
179400110906     C     endEP         ENDSR
179500110906     C************************************************************
179600110906      *?  Emissione Finestra Errori                              ?
179700110906     C************************************************************
179800110906     C     GESW1         BEGSR
179900110906     C*
180000110826     C                   eval      $FINE = dopo_in_$FINE
180100110826     C                   eval      $GEST = sav$GEST
180200110906     c                   eval      dopo_alert_window    = *blank
180300110826     c*
180400110826     c                   exfmt     TII5WER
180500110906     c*
180600110906     c                   eval         Err001 = *blank
180700110906     c                   eval         Err002 = *blank
180800110906     c                   eval         Err003 = *blank
180900110906     c                   eval       dopo_alert_window    ='S'
181000110906     c*
181100110906     C* F3=Fine - Uscita Forzata
181200110906     C                   if        $TASTO = F03 and $FINE = *off
181300110906     C                   exsr      F03_exit
181400110906     c                   endIF
181500110826     c*
181600110826     c* emesso l'errore torna al video che lo aveva generato
181700110826     c*  oppure va fuori dal programma  in base a $FINE e $GEST
181800110826     c*
181900110826     C                   ENDSR
182000110726      * ?-------------------------------------------------------------*?
182100110826     C*   Routine degli errori da visualizzare sui SFL
182200110826      * ?-------------------------------------------------------------*?
182300110826     C     Errori        BEGSR
182400110906     C*
182500110906     C*  Messaggio di avviso    SE deve emettere un messaggio
182600110906     c                   if        err001 <> *blank or
182700110906     c                             err002 <> *blank or
182800110906     c                             err003 <> *blank
182900110826     C*
183000110826     c                   seton                                        99
183100110826      * ?            *-----------------------*
183200110826     C                   exfmt     TII5WER
183300110826      * ?            *-----------------------*
183400110826     C*
183500110909     c                   clear                   err001
183600110909     c                   clear                   err002
183700110909     c                   clear                   err003
183800110909     C*
183900110826     C*  Imposta per il dopo emissione errore
184000110826     C                   eval      $FINE = dopo_in_$FINE
184100110826     C                   eval      $GEST = sav$GEST
184200110906     C*
184300110906     c                   end
184400110826     C*
184500110826     c                   ENDSR
184600110826      *---------------------------------------------------------------------------------
184700080609      *
184800070326** ErrMsg
184900110825La Filiale non è in gestione                                                   01
185000110912Non è possibile effettuare l'ordine Pianificato nella settimana.               02
185100110901L'Utente non è autorizzato alla procedura di Filiale.                          03
185200110726Scelta obbligatoria                                                            04
185300110912La settimana non è pianificata per eseguire l'Ordine Periodico.                05
185400110902E' già stato fatto un ordine Pianificato nei giorni precedenti.                06
185500110902L' Articolo non è presente in anagrafica                                       07
185600110902L' Articolo non appartiene all'ufficio di competenza                           08
185700110902Numero Ordine inesistente                                                      09
185800110902L'Ordine non è più disponibile                                                 10
185900110913ATTENZIONE : premendo F3 i dati inseriti andranno Perduti !!!                  11
186000110913    F3=Fine                           Enter x Continuare                       12
186100110909Se Sospeso o Tolto dalla Sospensione, spiegare nelle Note                      13
186200110909perchè l'articolo è sospeso o reintegrato nell'ordine.                         14
186300110913      Per confermare i dati inseriti premere F10                               15
186400110725                                                                               16
186500110725                                                                               17
186600110725                                                                               18
186700110726                                                                               19
186800110726                                                                               20
186900110726                                                                               21
187000110726                                                                               22
187100110726                                                                               23
187200110726                                                                               24
187300110726                                                                               25
187400110726                                                                               26
187500110726                                                                               27
187600110726                                                                               28
187700110726                                                                               29
187800110726                                                                               30
187900110726                                                                               31
188000110725                                                                               32
188100110726                                                                               33
188200110725                                                                               34
188300070829                                                                               35
188400070829                                                                               36
188500070829                                                                               37
188600070829                                                                               38
188700070829                                                                               39
188800110726Il valore immesso per il campo non è valido.                                   40
