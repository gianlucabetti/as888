000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300110826      *
000400110928      * TII503R *----------------------------------------------------*
000500110928      *     ?   - INTERROG.ORDINI MAGAZZINO -    ?
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
002300111010      *  36           scelta NOTE
002400940224      *  39           OF PRTF
002500070302      *  40 <---> 49  DSPATR SU video
002600940608      *  Specificare l'uso dei singoli indicatori
002700070227      *  50           Emissione della window totale/parziale
002800070227      *  51 <---> 90  ERRORI SU VIDEO
002900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003000070829      *  98           ERRORE SPECIALE :
003100940224      *  99           INDIC. GENERALE DI ERRORE
003200940128     F*----------------------------------------------------*
003300070404      *  Output
003400110906     FTMorp01L  iF   E           K DISK    rename(tmORP000:tmorp01)
003500110929     FTMorp02L  iF   E           K DISK    infds(orpds)
003600080609      *
003700070404      *  Input
003800110830     FazORG01l  if   e           k disk
003900110726     FtMAMP01l  if   e           k disk
004000110928     Ftii503d   CF   E             WORKSTN
004100110826     F                                     SFILE(TII5S01:S1NRR)
004200940201     F                                     INFDS(DSFMT)
004300070305     D*----------------------------------------------------*
004400070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004500070326     D*----------------------------------------------------*
004600940211     D* Passaggio Parametri
004700940211     D KPJBA         E DS
004800070227     D KPJBUs          s                   like(KPJBU)
004900110726     D*-------------
005000110726     d ficn70ds      e ds
005100110901     d  og143        e ds
005200110830     d dAUT          e ds
005300110909     d Tii502DS      e ds
005400940211     D*-------------
005500110824     d dsWEK         e ds
005600110905     D  weekWEK                       2    DIM(54)
005700110824     D*-------------
005800110830     D  SubClassi                     5    DIM(500)
005900110830     D*-------------
006000940211     D DSFMT           DS           512
006100940506     D  $TASTO               369    369
006200940211     D  NRG                  370    370
006300940211     D  NCL                  371    371
006400940211     D  SFLNRR               378    379B 0
006500940127     D*-------------
006600940127     D* Reperimento nome PGM
006700940127     D STATUS         SDS           333
006800940127     D  DSPGM            *PROC
006900030113     D*-------------
007000110928     d oppds           ds
007100110928     d  opp_nrr              397    400b 0
007200110928     D*-------------
007300110830     d orpds           ds
007400110830     d  orp_nrr              397    400b 0
007500110725     D*-------------
007600110725     D*--- DATA AREA NUMERAZIONE ORDINI :
007700070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
007800070227$003 D S1NRR           S                   Like(C1rcd)
007900070228$003 D S1pag           S                   Like(C1rcd)
008000070302$003 D WSfl            S                   Like(C1nrr)
008100030113$003 D Wmax            S                   Like(C1rcd)
008200030113$003 D Wpag            S                   Like(C1rcd)
008300070330$003 D sav_WSfl        S                   Like(WSfl)
008400070330$003 D sav_Wmax        S                   Like(Wmax)
008500070330$003 D sav_Wpag        S                   Like(Wpag)
008600070301$003 D Wpagine         S                   Like(C1rcd)
008700110824      *--------------------------
008800110826     D RigPag_SFl1     S              3  0 INZ(18)
008900070227      *--------------------------
009000070418     D sav_d1cfgs      S                   Like(d1cfgs) inz(0)
009100110826$003 D sav_S1NRR       S                   Like(s1nrr)
009200101129      *--------------------------
009300110726     d dataiso         s               d   datfmt(*iso)
009400110825     d oggiiso         s               d   datfmt(*iso)
009500101129     d dataeur         s               d   datfmt(*eur)
009600070320      *--------------------------
009700110824     d wFGS            s                   like(d1cFGS)
009800070305      *--------------------------
009900110909     D Cambia_Video...
010000110909     D                 S              1A
010100110906     D dopo_alert_window...
010200110906     D                 S              1A
010300110901     D Tipo_Filiale    S              1A
010400110830      *--------------------------
010500070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010600070305     D WLBDAT          DS                  INZ
010700070305     D  G02DAT                 1      8  0
010800070227     D  G02INV                 9     16  0
010900070227     D  G02ERR                17     17
011000070227     D  G02TGI                18     22  0
011100070307      *
011200070307     D DATPAR          DS
011300070307     D  GIODAT                 1      8  0
011400070307     D  GIOINV                 9     16  0
011500070307     D  GIOTGI                17     21  0
011600070307      *
011700070307     d Wdata8          DS
011800070307     d  dadata                 1      8  0
011900070307     d  adata                  9     16  0
012000070307      *--------------------------------
012100070227     d AZUTEds       e ds                  extname(AZUTE00F)
012200070227     d DDatiUte      e ds
012300110725      *--------------------------
012400070320     d* Ds per dati organigramma
012500070404      *------
012600100505     d DTAISO          s               D
012700070320      *--------------------------
012800070320     d* Parametri x Controllo profilo utenti
012900070320     d TIBS34ds      e ds
013000070227      *--------------------------
013100070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
013200070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
013300070227      *--------------------------
013400070320      *?  COSTANTI                                      ?
013500070320      *--------------------------
013600940506     D* Tasti di funzione
013700940506     D F01             C                   CONST(X'31')
013800940506     D F02             C                   CONST(X'32')
013900940506     D F03             C                   CONST(X'33')
014000940506     D F04             C                   CONST(X'34')
014100940506     D F05             C                   CONST(X'35')
014200940506     D F06             C                   CONST(X'36')
014300940506     D F07             C                   CONST(X'37')
014400940506     D F08             C                   CONST(X'38')
014500940506     D F09             C                   CONST(X'39')
014600940506     D F10             C                   CONST(X'3A')
014700940506     D F11             C                   CONST(X'3B')
014800070315     D F12             C                   CONST(X'3C')
014900940506     D F13             C                   CONST(X'B1')
015000940506     D F14             C                   CONST(X'B2')
015100940506     D F15             C                   CONST(X'B3')
015200940506     D F16             C                   CONST(X'B4')
015300940506     D F17             C                   CONST(X'B5')
015400940506     D F18             C                   CONST(X'B6')
015500940506     D F19             C                   CONST(X'B7')
015600940506     D F20             C                   CONST(X'B8')
015700940506     D F21             C                   CONST(X'B9')
015800940506     D F22             C                   CONST(X'BA')
015900940506     D F23             C                   CONST(X'BB')
016000940506     D F24             C                   CONST(X'BC')
016100940506     D ENTER           C                   CONST(X'F1')
016200940506     D ROLDWN          C                   CONST(X'F4')
016300940506     D ROLLUP          C                   CONST(X'F5')
016400070301     d digits          c                   '0123456789'
016500110826     d Pianificato     c                   'P'
016600110831     d aConsumo        c                   'C'
016700110829     d Classe_155      c                   '155'
016800110829     d SottoClas_156   c                   '156'
016900110831     d Acquisti        c                   'A'
017000110831     d Marketing       c                   'M'
017100940127     C*----------------------------------------------------*
017200070419      *?     MAIN LINE PROGRAM                             ?
017300940127     C*----------------------------------------------------*
017400940223     C* inizializzazione variabili
017500940223     C                   EXSR      INZVAR
017600940223     C*
017700940223     C     $FINE         DOWEQ     *OFF
017800110909     C*
017900110909     C*  re-imposta il passaggio fra video e video
018000110909     c                   eval      cambia_video = *blank
018100110909     c                   if        $GEST <> sav$GEST and $GEST <> 'W1'
018200110909     c                   eval      cambia_video = 'S'
018300110909     C                   end
018400110823     C*
018500110906     C* Controllo Autorizzazione PROFILO
018600110906     C*  altrimenti non Entra a programma
018700110823     C     $GEST         CASEQ     'AU'          GESAU                          Controllo DAUT
018800110826     C*
018900110826     C* Finestra x ALLARME messaggi importanti
019000110826     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
019100110826     C* 1° Video
019200070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
019300110826     C*
019400110906     C* 3° Video SFL riepilogativo dell'ordine
019500110906     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli Immes
019600110906     C*
019700940117     C                   END
019800110909      *
019900110909     C                   ENDdo
020000940325     C* fine programma
020100940325     C                   SETON                                            LR
020200030113     C************************************************************
020300070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
020400030113     C************************************************************
020500030113     C     INZVAR        BEGSR
020600030113     C*
020700030113     C* Variabili per gestione videate
020800030113     C                   MOVE      *OFF          $FINE
020900110824     C                   MOVE      *OFF          dopo_in_$FINE
021000110824     C                   MOVE      *ON           $INZD1
021100110826     C                   MOVE      *OFF          $INZS1
021200030113     C                   MOVE      *OFF          $EFILE
021300030113     C                   MOVE      *OFF          $ESCI
021400030113     C                   MOVE      *OFF          $RCDOK
021500110823     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
021600110823     C                   MOVE      'AU'          $GEST
021700030113     C* Variabili appoggio
021800030114     C                   Z-ADD     1             WPAG
021900030113     c*
022000030113     C                   ENDSR
022100110823     C************************************************************
022200110823      *?  Emissione primo video di SCELTA                        ?
022300110823     C************************************************************
022400110823     C     GESD1         BEGSR
022500110823     C*
022600070227     C* inizializzazione videata
022700070227     C     $INZD1        IFEQ      *ON
022800070227     C                   EXSR      INZD1
022900070227     C                   MOVE      *OFF          $INZD1
023000070227     C                   ENDIF
023100070227     C*
023200070228     c     Video_1       tag
023300070302     C*  Emissione Video
023400070228      * ?            *-----------------------*
023500110725     C                   EXFMT     TII5D01
023600070228      * ?            *-----------------------*
023700070326     c                   setoff                                       99
023800070305     C                   movel     'D1'          sav$GEST
023900070227     C* Selezioni
0240000702271    C                   SELECT
024100070413     C*  Cambio filiale
024200070413     C                   WHEN      $TASTO   =   F18
024300070418     C                   SETON                                        07
024400070227     C* F3=Fine
024500070227     C                   WHEN      $TASTO   =   F03
024600070227     C                   EXSR      F03_exit
024700070227      *
0248000702271O   C                   OTHER
024900070227     C* CONTROLLO DATI
025000070227     C                   EXSR      CTRD1
025100080626      *
0252000702271-   C                   ENDSL
025300070227     C*
025400070418     C* Per errori o decodifiche o tasto di cambio P.O.
025500070418     C                   if        $TASTO   =   F18 or *in99
025600070418     c                   goto      Video_1
025700070418     c                   end
025800111010     C*
025900111010     C*  Emissione SFL per ordini di riepilogo
026000111010     c                   eval      $GEST ='S1'
026100111010     C                   MOVE      *ON           $INZS1
026200070228     C*
026300110902     C*   se è stato richiesto un numero d'ordine specifico
026400110902     C*    rimanda al Riepilogo per rivisualizzare ciò che è stato
026500110902     C*   immesso nel sistema
026600110902     c                   if        d1nor > 0
026700110905     c                   seton                                        12
026800110902     c                   eval      num_ordine = d1nor
026900110902     c                   endIF
027000070228      *
027100080606     C     Fine_GESD1    ENDSR
027200070306     C/EJECT
027300110726      *---------------------------------------------------------------*
027400070320      *?  INIZIALIZZAZIONE Primo video                           ?
027500110726      *---------------------------------------------------------------*
027600070227     C     INZD1         BEGSR
027700070227     C*
027800070227     C* Pulizia campi e indicatori
027900070308     c                   exsr      INZ_indERR
028000110726     C*
028100070227     C* Pulizia formato record
028200110725     c                   clear                   TII5D01
028300070227     C* campi titolo
028400070302     C                   MOVEL     dsPGM         D1CPGM
028500070227     C                   MOVEL     knsif         D1nsif
028600070227     C                   MOVEL     knmus         D1nmus
028700070227     C                   MOVEL     RSUT          D1crsu
028800110831     C                   MOVEL     aConsumo      D1Torp
028900111020     C                   MOVEL     'N'           D1Conf
029000110831     C                   MOVEL     Acquisti      D1UFFc
029100110824     C                   z-add     settimana     D1week
029200111020     C                   z-add     0             D1dada
029300111020     C                   z-add     99999999      D1ada
029400070227     C* campi video
029500070227     C* Il p.o.deve essere impostato in base al profilo
029600070413     C     dutlpo        IFEQ      '2'
029700070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
029800070413     C                   ELSE
029900070413     c                   seton                                        08
030000070413     C                   MOVEL     SIMFEL        D1CFGS
030100070413     C                   END
030200080606      *
030300110830     C                   z-add     woggi         D1cDDC
030400080606      *
030500110824      *  reimposta la filiale
030600070418     c                   if        sav_d1cfgs > 0
030700070418     C                   z-add     sav_d1cfgs    D1cfgs
030800070418     c                   end
030900070418     C*
031000080606     C     END_INZ1      ENDSR
031100940127     C************************************************************
031200070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
031300940127     C************************************************************
031400070227     C     CTRD1         BEGSR
031500070606     C*
031600070227     C*  Spegne indicatori di errore
031700070308     c                   exsr      INZ_indERR
031800110905     c                   setoff                                       99  12
031900110902     C* --------------
032000070227      * ?- Controlla la filiale in gestione
032100110725     c                   clear                   wfgs
032200110725     C                   clear                   sav_d1cFGS
032300070227     C                   CLEAR                   DSLV50
032400070227     C                   MOVEL     KNMUS         D50PRU
032500070227     C                   z-add     d1cFGS        D50FGS
032600070227     C                   CALL      'FNLV50R'
032700070227     C                   PARM                    DSLV50
032800070227    1C     D50ERR        IFNE      ' '
032900111011    1C     dutlpo        andne     'S'
033000070326     c                   movel     ErrMsg(1)     d1msg
033100111011     c                   if        dutlpo ='S'
033200111011      * sblocca il campo
033300111011     C                   SETON                                        07
033400111011     c                   end
033500070326     C                   SETON                                        51  99
033600070227     C                   goto      ENDCTR1
033700070227    1C                   ENDIF
033800070227      *
033900110726     C*  Controlla il P.O.
034000111020     c                   if        d1cfgs > 0
034100110901     c                   movel     dutLPO        Tipo_Filiale
034200070227     C     d1cFGS        CHAIN     AZORG01l
034300110725      * Non presente in organigramma
034400110725    1C                   IF        not %found(azorg01l)
034500070326     C                   SETON                                        51  99
034600111011     c                   if        dutlpo ='S' and d1cfgs = 0
034700111011      * sblocca il campo
034800111011     C                   SETON                                        07
034900111011     c                   end
035000070326     c                   movel     ErrMsg(1)     d1msg
035100070227     C                   goto      ENDCTR1
035200110901    1C                   else
035300110901     c                   eval      og143 = orgDE3
035400110901     c                   if        §ogNTW = 'LOG'
035500110901     c                   movel     'L'           Tipo_Filiale
035600110901     c                   end
035700110901    1C                   ENDIF
035800111020     c                   end
035900070227     C*
036000111020      * --------------
036100111020     C*  se tutto il periodo
036200111020     c                   if        d1dada = 0  and
036300111020     c                             (d1ada = 0 or d1ada = 99999999)
036400111020     C                   Z-ADD     0             d1DADA
036500111020     C                   Z-ADD     99999999      d1ADA
036600111020     C                   Z-ADD     0             h1DADA
036700111020     C                   Z-ADD     99999999      h1ADA
036800111020     C*
036900111020     c                   else
037000111020     C*
037100111020      * controllo immissione delle date
037200111020     C                   Z-ADD     0             h1DADA
037300111020     c                   if        d1dada > 0
037400111020     C                   Z-ADD     d1dada        G02DAT
037500111020     C                   MOVEL     *BLANK        G02ERR
037600111020     C                   CALL      'XSRDA8'
037700111020     C                   PARM                    WLBDAT
037800111020     C     G02ERR        IFEQ      '1'
037900111020     c                   movel     ErrMsg(5)     d1msg
038000111020     C                   SETON                                        55  99
038100111020     C                   GOTO      endctr1
038200111020     C                   ENDIF
038300111020     C                   Z-ADD     G02INV        h1DADA
038400111020     C                   Z-ADD     G02dat        D1dada
038500111020      *
038600111020      * Controllo data finale
038700111020     C     d1Ada         ifeq      0
038800111020     C                   Z-ADD     d1DaDA        d1Ada
038900111020     C                   End
039000111020     c                   end
039100111020      *
039200111020     C                   Z-ADD     0             h1ADA
039300111020     c                   if        d1ada > 0  and
039400111020     c                             d1ada < 99999999
039500111020     C                   Z-ADD     d1aDA         G02DAT
039600111020     C                   MOVEL     *BLANK        G02ERR
039700111020     C                   CALL      'XSRDA8'
039800111020     C                   PARM                    WLBDAT
039900111020     C     G02ERR        IFEQ      '1'
040000111020     c                   movel     ErrMsg(5)     d1msg
040100111020     C                   SETON                                        56  99
040200111020     C                   GOTO      endctr1
040300111020     C                   ENDIF
040400111020     c*
040500111020     C                   Z-ADD     G02INV        h1ADA
040600111020     C                   Z-ADD     G02dat        d1Ada
040700111020     c                   else
040800111020     c                   eval        h1ada = 99999999
040900111020     c                   end
041000111020     c*
041100111020     C     h1DAda        ifgt      h1Ada
041200111020     c                   movel     ErrMsg(6)     d1msg
041300111020     C                   SETON                                        555699
041400111020     C                   GOTO      endctr1
041500111020     C                   END
041600111020      *
041700111020     c                   End
041800111020      * --------------
041900070316      * Salva la filiale su campo di work x tutto il pgm
042000070316     c                   z-add     d1cfgs        wfgs
042100070418     C                   Z-ADD     D1cFGS        sav_d1cFGS
042200110824      ***
042300110824     C                   eval       orpFIL  = d1cfgs
042400110905     C                   eval       orpUFF  = d1uffC
042500110824     C                   eval       orpANNO = Anno
042600110824     C                   eval       orpWEEK = settimana
042700110905     C                   eval       orpPIAN = d1torp
042800110908     c     kTmORP        chain     tmORP01L
042900110908     c                   if        %Found(tmORP01L)
043000110912      **
043100111011      ** se già esiste l'aggancia da confermare
043200111011     c                   z-add     orpNOR        num_Ordine        5 0
043300110912     c                   z-add     orpNOR        d1NOR
043400111010  104C                   Z-ADD     orpDOR        G02INV                         *AAAA/MM/GG
043500111010  104C                   MOVEL     '3'           G02ERR
043600111010  104C                   CALL      'XSRDA8'
043700111010  104C                   PARM                    WLBDAT
043800111010  104c                   Z-ADD     G02DAT        Data_Ordine       8 0
043900111010  104c                   move      'S1'          $GEST
044000111010  104c                   move      *ON           $INZS1
044100111010  104c                   goto      ENDCTR1
044200111010  104 **
044300110908     c                   end
044400110902      ***
044500110912     c     dopo          tag
044600110902      * >>>>>>>>>>>>>
044700110902      ***  interroga quali Ordini sono stati immessi per la filiale
044800111010     c                   IF        d1nor = 0
044900110902     c                   movel     kpjbu         kpjbus
045000110909     c                   clear                   tii502ds
045100110916     c                   z-add     d1cfgs        I502FILIAL
045200110909     c                   movel     d1torp        I502TIPORD
045300110909     c                   movel     d1uffc        I502UFFCOM
045400111020     c                   z-add     h1dada        I502dadata
045500111020     c                   z-add     h1ada         I502adata
045600111019      *-
045700111020     c                   movel     ' '           I502confer
045800111010     c                   if        d1conf = 'S'
045900111019     c                   movel     'C'           I502confer
046000111010     c                   end
046100111019      *-
046200110909     c                   movel     settimana     I502WEEKAA
046300110909     c                   move      anno          I502WEEKAA
046400111020      *-
046500110909     c                   movel     tii502ds      kpjbu
046600110902     c                   call      'TII502R'
046700110902     c                   parm                    kpjba
046800110909     c                   movel     kpjbu         tii502ds
046900111020      *-
047000110909     c                   move      o502numord    d1nor
047100110908     c                   if        d1nor = 0
047200110908     c                   SETON                                            99
047300110908     c                   end
047400111019      *-
047500110902     c                   movel     kpjbus        kpjbu
047600111010     c                   end
047700110902      ***
047800111010     c                   IF        d1nor > 0
047900110906     c     d1nor         chain     tmORP02L
048000110906     c                   if        not %Found(TmOrp02L)
048100110902     C                   SETON                                        54  99
048200110902     c                   movel     ErrMsg(9)     d1msg
048300110902     C                   goto      ENDCTR1
048400110902     c                   else
048500110902     C*
048600110902     C                   Z-ADD     orpDOR        G02INV                         *AAAA/MM/GG
048700110902     C                   MOVEL     '3'           G02ERR
048800110902     C                   CALL      'XSRDA8'
048900110902     C                   PARM                    WLBDAT
049000110902     c                   Z-ADD     G02DAT        Data_Ordine
049100110902     C*
049200110902     c                   endif
049300110902     c                   endif
049400070307     C*---------------
049500070227     C     ENDCTR1       ENDSR
049600110830     C/EJECT
049700110830      *---------------------------------------------------------------*
049800110830      *  ?  GESTIONE SFL di immissione righe di ordine               ?
049900110830      *---------------------------------------------------------------*
050000110830     C     GESS1         BEGSR
050100110830     C*
050200110826     C* inizializzazione videata
050300110826     C     $INZS1        IFEQ      *ON
050400110826     C                   EXSR      INZS1
050500110826     C                   MOVE      *OFF          $INZS1
050600110826     C                   ENDIF
050700110826     C*
050800110826     C* Inizializza e imposta piede videata
050900110826     C                   EXSR      INZZ1
051000110826     C*
051100110826     C* emissione piede videata
051200110826      * ?            *-----------------------*
051300110826     C                   WRITE     TII5Z01
051400110826      * ?            *-----------------------*
051500110826     C*
051600110826     C     WMAX          IFEQ      0
051700110826     C* Non ci sono records
051800110826      * ?            *-----------------------*
051900111010     C                   WRITE     TII5C01
052000110826      * ?            *-----------------------*
052100110826     c                   setoff                                       30
052200110826     C                   Else
052300110826     c                   seton                                        30
052400110826     c                   eval       sav_WSfl =  WSfl
052500110826     c                   eval       sav_Wpag =  Wpag
052600110826      *
052700110826     C     Wsfl          IFgt      0
052800110826     C                   Z-ADD     wsfl          C1RCD
052900110826     C                   Else
053000110826     C     Wpag          IFgt      0
053100110826     C                   Z-ADD     wpag          C1RCD
053200110826     C                   EndIF
053300110826     C                   EndIF
053400110826     C                   ENDIF
053500110826      *
053600110908      *    se pianificato 35-ON
053700110908     c                   setoFF                                       35
053800110908     c                   if        D1TORP = Pianificato
053900110908     c                   setoN                                        35
054000110908     C                   EndIF
054100110908      *
054200110908      *
054300110826      *  Emissione SFL dei GIRI
054400110826      * ?            *-----------------------*
054500110826     C                   EXFMT     TII5C01
054600110826      * ?            *-----------------------*
054700110826     c                   setoff                                       99
054800110826     C                   movel     'S1'          sav$GEST
054900110826      *
055000110826     C     C1NRR         IFNE      0
055100110826     C                   Z-ADD     C1NRR         WSFL
055200110826     C                   ENDIF
055300110826     C                   Z-ADD     SFLNRR        C1RCD
055400110826     C* Selezioni
0555001108261    C                   SELECT
055600110826     C* F3=Fine
055700110826     C     $TASTO        WHENEQ    F03
055800110826     C                   EXSR      F03_exit
055900110826      *
056000110826     C* F12=Ritorno
056100110826     C     $TASTO        WHENEQ    F12
056200111010     c                   move      'D1'          $GEST
056300111010     C                   MOVE      *ON           $INZD1
056400110826      *
0565001108261O   C                   OTHER
056600110826     C* CONTROLLO testata
056700110826     C                   EXSR      CTRC1
056800110826     C* CONTROLLO subfile
056900110826     C     *IN99         IFEQ      *OFF
057000110826     C                   EXSR      CTRS1
057100110826     C                   END
057200110826      *
0573001108261-   C                   ENDSL
057400110826     C*
057500110826     C                   ENDSR
057600110826     C/EJECT
057700110826      *---------------------------------------------------------------*
057800110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
057900110826      *---------------------------------------------------------------*
058000110826     C     INZZ1         BEGSR
058100110826     C*
058200110826     C* Messaggio o tasti funzionali
058300110826     c                   clear                   z1dmsg
058400110826     c                   clear                   ERRmsg80         80
058500110826     C*
058600110826      * imposta eventuali errori
058700110826     c                   eval      z1dmsg = ERRmsg80
058800110826     C*
058900110826     C                   ENDSR
059000110826     C/EJECT
059100110826      *---------------------------------------------------------------*
059200110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
059300110826      *---------------------------------------------------------------*
059400110826     C     INZS1         BEGSR
059500110826     C*
059600110826     C* reset indicatori DSPATR
059700110826     c                   exsr      INZ_indERR
059800110826     C* pulizia SFL
059900110908     C                   SETOFF                                       3031
060000110826     C                   WRITE     TII5C01
060100110826     C                   SETON                                          31
060200110826     C*
060300110826     C* Righe x Pagina del 1° SFL
060400110826     c                   z-add     RigPag_SFl1   s1pag
060500110826      *
060600110826     C* CARICAMENTO SFL totale
060700110826     C                   Z-ADD     0             S1NRR
060800110826     C                   Z-ADD     1             C1RCD
060900110826     C                   Z-ADD     0             WMAX
061000110826     C                   Z-ADD     0             Wsfl
061100110826     C* campi titolo
061200110826     C                   MOVEL     dsPGM         c1CPGM
061300110826     C                   MOVEL     knsif         c1nsif
061400110826     C                   MOVEL     knmus         c1nmus
061500110826     C                   MOVEL     RSUT          c1crsu
061600110826     C                   z-add     settimana     c1week
061700110830     C                   z-add     num_ordine    c1NOR
061800110830     C                   z-add     data_ordine   c1DOR
061900110826      **----------------
062000110826     C* Posizionamento su file pilota
062100110830     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
062200110826     C                   eval        $EFILE    = *OFF
062300110906     C     c1NOR         setll     tmORP02L
062400110826      **----------------
062500110826     C* Carico il SFL
062600110826     C                   EXSR      ROLS1
062700110826     C*
062800110826     C                   Z-ADD     1             WPAG
062900110826     c                   eval       sav_Wmax =  Wmax
063000110826     C*
063100110826     C                   ENDSR
063200110826     C/EJECT
063300110826      *---------------------------------------------------------------*
063400110826      *? CARICAMENTO PAGINA LISTA                                    ?
063500110826      *---------------------------------------------------------------*
063600110826     C     ROLS1         BEGSR
063700110826     C*
063800110826     C                   SETOFF                                       32
063900110831     C                   Z-ADD     1             Y
064000110826     C                   Z-ADD     WMAX          S1NRR
064100110826     C*
064200110826     C* Caricamento del Sfl x Giro
064300110826     c                   Exsr      Write_S1
064400110826     C*
064500110826     C                   Z-ADD     S1NRR         WMAX                 30
064600110826     C*
064700110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
064800110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
064900110826     C                   MVR                     RESTO             3 0
065000110826     C     PAGINE        MULT      S1pag         C1RCD
0651001108261    C     RESTO         IFGT      0
065200110826     C                   ADD       1             C1RCD
0653001108261E   C                   ELSE
065400110826     C                   SUB       S1pag         C1RCD
065500110826     C                   ADD       1             C1RCD
0656001108261-   C                   ENDIF
065700110826     C*
065800110826     C                   ENDSR
065900110826      *---------------------------------------------------------------*
066000110826      *?  Scrive x Ogni rottura di GIRO                              ?
066100110826      *---------------------------------------------------------------*
066200110826     C     Write_S1      BEGSR
066300110826      *
066400110826      *  Precarica da Ordine Pianificato
066500110826     c                   exsr      Legge_rec1
066600110826      *
066700110826      * Finchè il giro è lo stesso
0668001108311    C     $EFILE        DowEQ     *OFF
066900110826     C                   clear                   TII5S01
067000110826      *
067100110826      * Solo x ordini mensili
067200110826     c                   exsr      carica_campi1
067300110826      *---------
067400110826     C                   ADD       1             S1NRR
067500110826     C                   ADD       1             Y
067600110826     C                   WRITE     TII5S01
067700110826      *---------
067800110826     c                   exsr      Legge_rec1
067900110826     c                   EndDO
068000110826     C*
068100110826     C                   ENDSR
068200110826      *---------------------------------------------------------------*
068300110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
068400110826      *---------------------------------------------------------------*
068500110826     C     LEGGE_REC1    BEGSR
068600110826     C*
068700110826     C*  Per EoF deve uscire
068800110826     C*  Per Record Ok deve uscire riportando i dati
068900110826     C                   MOVEL     *OFF          $EFILE
069000110826     C                   MOVEL     *OFF          $RCDOK
069100110826     C*
0692001108261    C     $EFILE        DOUEQ     *ON
069300110826     C     $RCDOK        OREQ      *ON
069400110826     C*
069500110830      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
069600110830     c****   LETTURA RECORD x N°ORDINE
069700110906     C     c1NOR         reade     tmORP02L
069800110826      * Fine File
069900110906     C                   IF        %Eof(tmORP02l)
070000110826     C                   MOVEL     *ON           $EFILE
070100110826     C                   MOVE      $EFILE        *IN33
070200110826     C                   Else
070300110826      * ?Controlla se la Spedizione è da considerare
070400110826     C                   MOVEL     *ON           $RCDOK
070500110826     C                   exsr      Check_RECord1
070600110826     C                   END
070700110826     C*
0708001108261-   C                   ENDDO
070900110826     C*
071000110826     C                   ENDSR
071100110826      *---------------------------------------------------------------*
071200110826      *?   CONTROLLO il record letto se va bene                      ?
071300110826      *---------------------------------------------------------------*
071400110826     C     Check_Record1 BEGSR
071500110826     C*
071600110826     C*
071700110826     C     end_check1    ENDSR
071800110826      *---------------------------------------------------------------*
071900110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
072000110826      *---------------------------------------------------------------*
072100110826     C     Carica_campi1 BEGSR
072200110826     C*
072300111020     c                   setoff                                       3440
072400111020     c                   z-add     orpfil        c1fil
072500111020     c                   if        orpconf = 'C'
072600111020     c                   seton                                          40
072700111020     c                   end
072800110908     C*
072900110826     C*  carica i campi e li protegge
073000110902     c                   z-add     orp_nrr       h1nrrorp
073100110830     c                   eval      s1MAT =  orpMAT
073200110908     C*
073300110908     C* i Sospesi possono essere solamente per Ordini pianificati e di
073400110908     C*   articoli Obbligatori.
073500110906     c                   if        orpATT = 'S'
073600110906     c                   move(p)   'SOSP'        s1sospeso
073700110906     c                   end
073800110908      **
073900110906     c                   if        orpATT = 'A'
074000110908     c                   eval      h1sosp = 'A'
074100110906     c                   move(p)   '*SOSP'       s1sospeso
074200110906     c                   end
074300110908      **
074400111010     c                   eval      *in36 = '0'
074500110830     c                   eval      s1QTA =  orpQTA
074600110830     c                   if        orpNOTE <> *blank
074700110830     c                   move      '*'           s1not
074800110908     c                   move      '*'           h1not
074900111010     c                   eval      *in36 = '1'
075000110830     c                   eval      h1NOTA1 = %Subst(orpNOTE:01:60)
075100110830     c                   eval      h1NOTA2 = %Subst(orpNOTE:61:60)
075200110830     c                   eval      h1NOTA3 = %Subst(orpNOTE:121:60)
075300110830     c                   eval      h1NOTA4 = %Subst(orpNOTE:181:60)
075400110830     c                   end
075500110826     C*
075600110826     C* decodifiche Anagrafica articoli
075700110826     c     s1mat         chain     tmAMP01l
075800110826     c                   if        %Found(tmAMP01L)
075900110826     c                   eval      s1DES =  AMPDMT
076000110826     c                   eval      s1UMA =  AMPUMA
076100110826     c                   else
076200110826     c                   eval      s1DES =  *all'*'
076300110826     c                   eval      s1UMA =  *all'*'
076400110826     c                   end
076500110908     C*
076600110908     C*
076700110826     C                   ENDSR
076800110826      *---------------------------------------------------------------*
076900110826      *?   CONTROLLO TESTATA LISTA                                   ?
077000110826      *---------------------------------------------------------------*
077100110826     C     CTRC1         BEGSR
077200110826     C*
077300110826     C                   MOVE      *OFF          *IN99
077400110826     C*
077500110826     C                   ENDSR
077600110826      *---------------------------------------------------------------*
077700110826      *?   CONTROLLO OPZIONI LISTA                                   ?
077800110826      *---------------------------------------------------------------*
077900110826     C     CTRS1         BEGSR
078000110826     C*
078100110826     C                   MOVEL     *OFF          $ESCI
078200110826     c                   clear                   ERRmsg80
078300110826     C                   SETOFF                                       99
078400110826     C*
078500110826     C* Leggo il sfl solo se ci sono rcd
0786001108261    C     WMAX          IFGT      0
078700110826     C                   READC     TII5S01                                21
078800110826     C*
078900110826     C* esce se fine sfl o errore che richiede l'uscita
0790001108262    C     *IN21         DOWEQ     *OFF
079100110826     C     $ESCI         ANDEQ     *OFF
079200110826     C                   Z-ADD     S1NRR         C1RCD
079300110826     C* ctrl su riga
079400110826     C                   EXSR      RECS1
079500110826      *
079600110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0797001108263    C     *IN99         IFEQ      *ON
079800110826     C     $ESCI         OREQ      *ON
079900110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
080000110826     C* la ripristinerò a conclusione del ciclo di READC
080100110826     C                   MOVE      *OFF          $INZS1
0802001108263-   C                   ENDIF
080300111010     C*
080400111010     c                   eval      *in36 = '0'
080500111010     c                   if          s1not = '*'
080600111010     c                   eval      *in36 = '1'
080700111010     c                   end
080800110826     C*
080900110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
081000110826     C                   MOVE      *ON           *IN32
081100110826     C                   UPDATE    TII5S01
081200110826     C*
081300110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0814001108263    C     $ESCI         IFEQ      *OFF
081500110826     C                   READC     TII5S01                                21
081600110826      *
081700110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0818001108264    C     *IN21         IFEQ      *ON
081900110826     C* calcolo pagina a cui deve posizionarsi
082000110826     c                   z-add     s1pag         Wpagine
082100110826     C                   EXSR      CLCPAG
0822001108264-   C                   ENDIF
0823001108263-   C                   ENDIF
082400110826     C*
0825001108262-   C                   ENDDO
082600110826     C*
0827001108261-   C                   ENDIF
082800110826     C* __________________________________________________
082900110826     C*  errori da evidenziare tramite finestra di ALLERTA
083000110826     c                   exsr      errori
083100110826     C* __________________________________________________
083200110826     C*
083300110826     C                   ENDSR
083400110826     C/EJECT
083500110826      *---------------------------------------------------------------*
083600110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
083700110826      *---------------------------------------------------------------*
083800110826     C     RECS1         BEGSR
083900110826     C*
084000110826     C* reset indicatori DSPATR
084100110826     c                   exsr      INZ_indERR
084200110826     C*
084300110830     c* se è richiesta manutenzione NOTE
084400110830     C* gestione opzioni
0845001109063    C     s1selNOT      IFNE      *blank
084600110830     C     *IN99         ANDEQ     *OFF
084700110830     C                   EXSR      SCE_S1
084800110906     c                   clear                   s1selNOT
0849001108303-   C                   ENDIF
085000110906     C*
085100110826     C     endSF1        ENDSR
085200110830      *---------------------------------------------------------------*
085300110830      *?   GESTIONE OPZIONE NOTE                                     ?
085400110830      *---------------------------------------------------------------*
085500110830     C     SCE_S1        BEGSR
085600110830     C*
085700110908     c                   clear                   cambia_ORP        1
085800110909     C*  INSERIMENTO NOTE:
085900110909     C*  ----------------
086000110908     c                   clear                   s1selNOT
086100110909     c                   clear                   NOTfree
086200110909     C*
086300110830     c                   eval      notMAT = s1MAT
086400110830     c                   eval      notDES = s1DES
086500110830     c                   eval      notQTA = s1QTA
086600110830     c                   eval      not001 = h1nota1
086700110830     c                   eval      not002 = h1nota2
086800110830     c                   eval      not003 = h1nota3
086900110830     c                   eval      not004 = h1nota4
087000110830     C*
087100110830     C*  Selezionata la riga per visualizzare le note
087200110830     C                   exfmt     TII5NOT
087300110908     C*
087400110830     C                   ENDSR
087500110826     C/EJECT
087600110826      *---------------------------------------------------------------*
087700110826      *?  GESTIONE F03 x uscire LR                                   ?
087800110826      *---------------------------------------------------------------*
087900110826     C     F03_exit      BEGSR
088000111010      **
088100111010     C* se si è su un altro video rispetto a prima occorre resettare
088200111010      **  la finestra con il messaggio di avviso.
088300111010     c                   if        cambia_video = 'S'
088400111010     c                   eval      dopo_alert_window    = *blank
088500111010     c                   end
088600111010      **
088700111010     C                   MOVE      *ON           $FINE
088800110906     C* fine programma
088900110826      *
089000110826     C                   ENDSR
089100110826     C/EJECT
089200110825      *---------------------------------------------------------------*
089300110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
089400110825      *---------------------------------------------------------------*
089500110825     C     CLCPAG        BEGSR
089600110825     C* Input :
089700110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
089800110825     C*          cursore
089900110825     C* - Wpagine= numero rcd per pagina sfl
090000110825     C* Output :
090100110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
090200110825     C*
090300110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
090400110825     C                   MVR                     RESTO             3 0
090500110825     C     RESTO         IFGT      0
090600110825     C                   ADD       1             PAGINE
090700110825     C                   ENDIF
090800110825     C     PAGINE        MULT      Wpagine       WPAG
090900110825     C*
091000110825     C                   ENDSR
091100110825     C************************************************************
091200110825      *?  INIZIALIZZA indicatori di errore                           ?
091300110825     C************************************************************
091400110825     C     INZ_indERR    BEGSR
091500110825      *
091600110825     C                   move      *ALL'0'       IN5190           40
091700110825     C                   moveA     IN5190        *IN(51)
091800110825      *
091900110825     C                   ENDSR
092000070301     C/EJECT
092100070301      *---------------------------------------------------------------*
092200070316      *  ?  OPERAZIONI INIZIALI      ?
092300070301      *---------------------------------------------------------------*
092400940131     C     *INZSR        BEGSR
092500110726     C*
092600110908     C*  chiavi x files.
092700110824     C*
092800110908     C*
092900110906     c     kTmORP        klist
093000110824     C                   KFLD                    orpFIL
093100110905     C                   KFLD                    orpUFF
093200110824     C                   KFLD                    orpANNO
093300110824     C                   KFLD                    orpWEEK
093400110905     C                   KFLD                    orpPIAN
093500110824     C*
093600030113     C* Reperimento parametri
093700030113     C     *ENTRY        PLIST
093800030113     C                   PARM                    KPJBA
093900070228      *
094000110726      *  Se è EDP
094100070308     c                   CLEAR                   se_EDP            1
094200070308     c                   if        %subst(Knmus:1:3) = 'EDP'
094300070308     c                   MOVE      'S'           se_EDP
094400070307     c                   end
094500070305     C*
094600070227      * Reperisco dati job
094700070227     c                   exsr      DatiJob
094800110901     C*  Controlla il P.O.
094900110901     c                   movel     dutLPO        Tipo_Filiale
095000110901     C     d1cFGS        CHAIN     AZORG01l
095100110901      * Non presente in organigramma
095200110901    1C                   IF        %found(azorg01l)
095300110901     c                   eval      og143 = orgDE3
095400110901     c                   if        §ogNTW = 'LOG'
095500110901     c                   movel     'L'           Tipo_Filiale
095600110901     c                   end
095700110901    1C                   ENDIF
095800110901     C*
095900070227
096000030113     C* Variabili per gestione videate
096100030113     C                   MOVE      *BLANK        $GEST             2
096200070305     C                   MOVE      *BLANK        sav$GEST          2
096300110906     C                   MOVE      *off          $FINE             1
096400110906     C                   MOVE      *off          dopo_in_$FINE     1
096500110906     C                   MOVE      *off          $INZD1            1
096600110906     C                   MOVE      *off          $INZS1            1
096700110906     C                   MOVE      *off          $EFILE            1
096800110906     C                   MOVE      *off          $ESCI             1
096900110906     C                   MOVE      *off          $RCDOK            1
097000030113     C* Indici
097100030113     C                   Z-ADD     0             X                 3 0
097200030113     C                   Z-ADD     0             Y                 3 0
097300070227     C***
097400070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
097500070227     C                   TIME                    W0140            14 0
097600110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
097700110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
097800070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
097900070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
098000070227     C*
098100110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
098200070227     C                   MOVEL     *BLANK        G02ERR
098300070227     C                   CALL      'XSRDA8'
098400070227     C                   PARM                    WLBDAT
098500110825     c                   Z-ADD     G02INV        WoggiAMG          8 0          *AAAA/MM/GG
098600110825     c                   eval      dataiso = %date(WoggiAMG:*iso)
098700110824     c                   eval      dataeur = dataiso
098800110825     c                   eval      oggiiso = dataiso
098900110824     c                   clear                   settimana         2 0
099000110824     C*
099100110824     C* NR.Settimana in corso
099200110824     C/EXEC SQL
099300110831     C+ SELECT week_iso(:OGGIISO) INTO :settimana FROM tmAMP00F
099400110824     C/END-EXEC
099500100505     C*
099600940117     C                   ENDSR
099700070227      *---------------------------------------------------------------*
099800070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
099900070227      *---------------------------------------------------------------*
100000070228     c     DatiJOB       BEGSR
100100070227      *
100200070227     c     *dtaara       define    §azute        azuteds
100300070227     c     *dtaara       define    §datiute      ddatiute
100400070227      *
100500070227     c                   in(E)     *dtaara
100600070227     c                   IF        %ERROR or RSUT = *blanks
100700070227     c                   clear                   Tibs34Ds
100800070227     c                   call      'TIBS34R'
100900070227     c                   parm                    Tibs34Ds
101000070227     c                   in        *dtaara
101100070227     c                   ENDIF
101200080606      *
101300070227     c                   ENDSR
101400110826      * ?-------------------------------------------------------------*?
101500110826      *?   SUBROUTINE Autorizzazione                                 ?
101600110826      * ?-------------------------------------------------------------*?
101700110826     C     GESAU         BEGSR
101800110826      *
101900110826     C* Se sarà un profilo Abilitato, imposta l'Entrata dal Primo Video
102000110826     C                   MOVE      'D1'          $GEST
102100110826     C*
102200110826      * ?- Controlla autorizzazione sull'utente
102300110826     c                   movel     dutpou        i70pog
102400110826     C* Il p.o.deve essere impostato in base al profilo
102500110826     C     dutlpo        IFEQ      '2'
102600110826     C                   MOVEL     dutpou        i70pge                         *FIL.GESTIONE
102700110826     C                   ELSE
102800110826     C                   MOVEL     SIMFEL        i70pge
102900110826     C                   END
103000110826     c                   movel     knmus         i70prf
103100110826     c                   call      'FICN70R'
103200110826     c                   parm                    ficn70ds
103300110826      *
103400110826     c                   movel     o70uni        daut
103500110826      *
103600110901     c* utente NON abilitato alla gestione magazzino OPPURE IN SEDE
103700111011     c                   if        §autmag <>'S'
103800110826      *
103900110906     c                   eval      Err001 = ' '
104000110906     c                   eval      Err002 = ErrMsg(3)
104100110826     c                   eval      Err003 = ' '
104200110826      *   Messaggio errore a Finestra
104300110906     C                   MOVE      *on           dopo_in_$FINE
104400110826     C                   MOVE      'W1'          $GEST
104500110826      *
104600110826     c                   end
104700110826     c*
104800110826     C     endAU         ENDSR
104900110906     C************************************************************
105000110906      *?  Emissione Finestra Errori                              ?
105100110906     C************************************************************
105200110906     C     GESW1         BEGSR
105300110906     C*
105400110826     C                   eval      $FINE = dopo_in_$FINE
105500110826     C                   eval      $GEST = sav$GEST
105600110906     c                   eval      dopo_alert_window    = *blank
105700110826     c*
105800110826     c                   exfmt     TII5WER
105900110906     c*
106000110906     c                   eval         Err001 = *blank
106100110906     c                   eval         Err002 = *blank
106200110906     c                   eval         Err003 = *blank
106300110906     c                   eval       dopo_alert_window    ='S'
106400110906     c*
106500110906     C* F3=Fine - Uscita Forzata
106600110906     C                   if        $TASTO = F03 and $FINE = *off
106700110906     C                   exsr      F03_exit
106800110906     c                   endIF
106900110826     c*
107000110826     c* emesso l'errore torna al video che lo aveva generato
107100110826     c*  oppure va fuori dal programma  in base a $FINE e $GEST
107200110826     c*
107300110826     C                   ENDSR
107400110726      * ?-------------------------------------------------------------*?
107500110826     C*   Routine degli errori da visualizzare sui SFL
107600110826      * ?-------------------------------------------------------------*?
107700110826     C     Errori        BEGSR
107800110906     C*
107900110906     C*  Messaggio di avviso    SE deve emettere un messaggio
108000110906     c                   if        err001 <> *blank or
108100110906     c                             err002 <> *blank or
108200110906     c                             err003 <> *blank
108300110826     C*
108400110826     c                   seton                                        99
108500110826      * ?            *-----------------------*
108600110826     C                   exfmt     TII5WER
108700110826      * ?            *-----------------------*
108800110826     C*
108900110909     c                   clear                   err001
109000110909     c                   clear                   err002
109100110909     c                   clear                   err003
109200110909     C*
109300110826     C*  Imposta per il dopo emissione errore
109400110826     C                   eval      $FINE = dopo_in_$FINE
109500110826     C                   eval      $GEST = sav$GEST
109600110906     C*
109700110906     c                   end
109800110826     C*
109900110826     c                   ENDSR
110000110826      *---------------------------------------------------------------------------------
110100080609      *
110200070326** ErrMsg
110300110825La Filiale non è in gestione                                                   01
110400111010                                                                               02
110500110901L'Utente non è autorizzato alla procedura di Filiale.                          03
110600110726Scelta obbligatoria                                                            04
110700111020Data errata                                                                    05
110800111020Range Date non nell'ordine corretto                                            06
110900111010                                                                               07
111000111010                                                                               08
111100110902Numero Ordine inesistente                                                      09
111200111010                                                                               10
111300111010                                                                                 
111400111010
111500111010                                                                               13
111600111010                                                                               14
111700111010                                                                               15
111800110725                                                                               16
111900110725                                                                               17
112000110725                                                                               18
112100110726                                                                               19
112200110726                                                                               20
112300110726                                                                               21
112400110726                                                                               22
112500110726                                                                               23
112600110726                                                                               24
112700110726                                                                               25
112800110726                                                                               26
112900110726                                                                               27
113000110726                                                                               28
113100110726                                                                               29
113200110726                                                                               30
113300110726                                                                               31
113400110725                                                                               32
113500110726                                                                               33
113600110725                                                                               34
113700070829                                                                               35
113800070829                                                                               36
113900070829                                                                               37
114000070829                                                                               38
114100070829                                                                               39
114200110726Il valore immesso per il campo non è valido.                                   40
