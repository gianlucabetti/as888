000100100803       //==============================================================
000200101214       //?Gestione Data affidamento presunto traffico                  ?
000300100803       //==============================================================
000400100803
000500100803       //--------------------------------------------------------------
000600100803       //?Specifiche di controllo.                                     ?
000700100803       //--------------------------------------------------------------
000800100803
000900100803     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000100809     h dftactgrp(*no)
001100101115       // - Deve essere specificato DFTACTGRP(*NO) per un prototipo che
001200101115       //   non ha la parola chiave EXTPGM.
001300101115       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
001400101115
001500100803
001600100803       //--------------------------------------------------------------
001700100803       //?Dichiarazione file.
001800100803       //--------------------------------------------------------------
001900100803
002000101215     fTivof01L  uf a e           k disk
002100100803
002200100803       // -?Video parametri di lancio?
002300101214     fTNTA63D   cf   e             workstn usropn
002400100803     f                                     indds(IndDspF)
002500100803     f                                     infds(InfDspF)
002600100803
002700100803       //--------------------------------------------------------------
002800100803       //?Definizione costanti.                                        ?
002900100803       //--------------------------------------------------------------
003000100803
003100100803       // -?Tasti funzionali a video?
003200100803     d c_F01           c                   const(x'31')
003300100803     d c_F02           c                   const(x'32')
003400100803     d c_F03           c                   const(x'33')
003500100803     d c_F04           c                   const(x'34')
003600100803     d c_F05           c                   const(x'35')
003700100803     d c_F06           c                   const(x'36')
003800100803     d c_F07           c                   const(x'37')
003900100803     d c_F08           c                   const(x'38')
004000100803     d c_F09           c                   const(x'39')
004100100803     d c_F10           c                   const(x'3A')
004200100803     d c_F11           c                   const(x'3B')
004300100803     d c_F12           c                   const(x'3C')
004400100803     d c_F13           c                   const(x'B1')
004500100803     d c_F14           c                   const(x'B2')
004600100803     d c_F15           c                   const(x'B3')
004700100803     d c_F16           c                   const(x'B4')
004800100803     d c_F17           c                   const(x'B5')
004900100803     d c_F18           c                   const(x'B6')
005000100803     d c_F19           c                   const(x'B7')
005100100803     d c_F20           c                   const(x'B8')
005200100803     d c_F21           c                   const(x'B9')
005300100803     d c_F22           c                   const(x'BA')
005400100803     d c_F23           c                   const(x'BB')
005500100803     d c_F24           c                   const(x'BC')
005600100803     d c_Enter         c                   const(x'F1')
005700100803     d c_RollDown      c                   const(x'F4')
005800100803     d c_RollUp        c                   const(x'F5')
005900100803
006000100803       //--------------------------------------------------------------
006100100803       //?Definizione schiere.                                         ?
006200100803       //--------------------------------------------------------------
006300101214      // - Messaggi di errore
006400101214     d $Msg            s             78    dim(10) ctdata perrcd(1)
006500100803
006600100803       //--------------------------------------------------------------
006700100803       //?Definizione aree dati.                                       ?
006800100803       //--------------------------------------------------------------
006900100803
007000100803       // -?Dati utente?
007100100803     d §AzUte        e ds                  extname(AZUTE00F)
007200100803     d                                     dtaara
007300100803     d §DatiUte      e ds                  extname(dDatiUte)
007400100803     d                                     dtaara
007500100803
007600100803       //--------------------------------------------------------------
007700100803       //?Definizione strutture dati.                                  ?
007800100803       //--------------------------------------------------------------
007900100803
008000100803       // -?Status ds?
008100100803     d Status         sds
008200100806     d   SDSpgm          *proc
008300100803
008400100803       // -?InfDS?
008500100803     d InfDspF         ds
008600100806     d   dsp_aid             369    369a
008700100803
008800101110      // - Indicatori su DspF
008900101110     d IndDspF         ds
009000101110         // -?Condzionamento tipo di emissione?
009100101110     d   xDs4                          n   overlay(IndDspF : 34)
009200101110        // - Indicatori di visualizzazione
009300101110     d  errMessage                    1n   overlay(IndDspF : 28)
009400101214        // - Indicatori di errore window accettazione
009500101214     d  PosCurDpa                     1n   overlay(IndDspF : 50)
009600101214        // - Indicatori di errore generico
009700101214     d  errGenerico                   1n   overlay(IndDspF : 99)
009800101214
009900101214     d WindDspF        ds                  inz
010000101214     d   WindDspFa            50     90    inz(*zero)
010100101214
010200101214     d wlbdat          ds                  inz
010300101214     d  g02dat                 1      8  0
010400101214     d  g02inv                 9     16  0
010500101214     d  g02err                17     17
010600101214     d  g02tgi                18     22  0
010700101214
010800101110
010900100803       // -?Parametri ricevuti?
011000100803     d KPJBA         e ds
011100101214     d TNTA63DS      e ds
011200100803
011300100803       // -?Parametri per Reperimento dati utente?
011400100803     d TIBS34ds      e ds                  inz
011500101028
011600100803       //--------------------------------------------------------------
011700100803       //?Definizione variabili.                                       ?
011800100803       //--------------------------------------------------------------
011900100803
012000100803       // -?Flags booleani?
012100100803     d $Fine           s               n   inz
012200100803     d $InzD01         s               n   inz(*on)
012300100803
012400101028       // -?Campi di comodo
012500101215     d wrkdpa          s              8  0
012600101215     d  data_eur       s               d   Datfmt(*eur)
012700101215     d  data_iso       s               d   Datfmt(*iso)
012800101214     d  data_dmy       s               d   Datfmt(*dmy)
012900101214     d  data_ymd       s               d   datfmt(*ymd)
013000101214     d  datacor8       s              8  0
013100101112
013200100803       // -?Variabili per la gestione del video?
013300100803     d $Video          s              2    inz('D1')
013400100803
013500100803       //--------------------------------------------------------------
013600100908       //?Definizione prototipi & relativi parametri                   ?
013700100803       //--------------------------------------------------------------
013800100803
013900101105       // -?API QSNDRTVMOD (Retrieve Display Mode)?
014000101105     d QdspMode        s              1    inz
014100101105      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
014200101105
014300100803      /copy gaitrasrc/srcProtoPR,TIBS34R
014400101214      /copy gaitrasrc/srcprotopr,xsrda8
014500100803
014600100803       //--------------------------------------------------------------
014700100803       //?Definizione key-list.                                        ?
014800100803       //--------------------------------------------------------------
014900100803
015000100803       //--------------------------------------------------------------
015100100803       //?M A I N - L I N E                                            ?
015200100803       //--------------------------------------------------------------
015300100803
015400100803     c     *Entry        plist
015500100803     c                   parm                    KPJBA
015600101214     c                   parm                    TNTA63DS
015700100803
015800100803      /free
015900100803
016000100803       // -?Operazioni iniziali?
016100100803       exsr  sr_RoutInz;
016200100803
016300100803       // -?Gestione window?
016400100803       DoW  Not $Fine;
016500100803         select;
016600101214
016700101214           when  $Video = 'D1';
016800101214             exsr  sr_GesD01;
016900101214
017000101214         endsl;
017100101214       EndDo;
017200100803
017300100803       // -?Operazioni finali?
017400100803       exsr  sr_RoutEnd;
017500100803
017600100803       //--------------------------------------------------------------
017700100803       //?Operazioni iniziali.                                         ?
017800100803       //--------------------------------------------------------------
017900100803       BEGSR  sr_RoutInz;
018000100803
018100100803         *inLR = *on;
018200100803
018300100803         // -?Impostazione campi "fissi"?
018400100803
018500100803         // -?Reperimento dati del lavoro?
018600100803         exsr  sr_DatiJob;
018700100809
018800100809         // -?Reperimento formato attuale display?
018900101214         clear  TA63P01;
019000100809         RetrieveDisplayMode ( QdspMode : *omit : *omit );
019100100809         xDs4 = (QdspMode = '4');
019200101214         open  TNTA63D;
019300100804
019400100804         // -?Posizionamento window?
019500100809           if  Not xDs4;
019600100927             W1rig = 11;
019700100809           else;
019800100927             W1rig = 14;
019900100809           endif;
020000100809           W1col = 2;
020100101214
020200101214       // Reperisco data corrente
020300101214             data_iso = (%date());
020400101214             datacor8 = %dec(data_iso);
020500100803
020600100803       ENDSR;
020700100803
020800100803       //--------------------------------------------------------------
020900100803       //?Reperimento Dati del job (Utente/Operativi).                 ?
021000100803       //--------------------------------------------------------------
021100100803       BEGSR  sr_DatiJob;
021200100803
021300100803         in(E) §AzUte;
021400100803         if NOT %error;
021500100803           in(E) §DatiUte;
021600100803         endif;
021700100803         if %error or RSut = *blanks;
021800100803           clear TIBS34ds;
021900100803           tibs34r(tibs34ds);
022000100803           in §AzUte;
022100100803           in §DatiUte;
022200100803         endif;
022300100803
022400100803       ENDSR;
022500100803
022600100803       //--------------------------------------------------------------
022700100803       //?Gestione videata D01.                                        ?
022800100803       //--------------------------------------------------------------
022900100803       BEGSR  sr_GesD01;
023000100803
023100100803         // -?Inizializzazione videata?
023200100803         if  $InzD01;
023300101214             exsr  sr_InzD01;
023400101214             $InzD01 = *off ;
023500100803         endif;
023600100803
023700101214         // Gestione video
023800101214         DOW $Fine = *off;
023900101214
024000101214         // -?Emissione Piede (window) e Testata?
024100101214            write  TA63P01;
024200101214            write  TA63T01;
024300101214
024400101214         // -?Emissione videata?
024500101214            exfmt  TA63D01;
024600101214
024700101214            errMessage = *off ;
024800101214            errGenerico = *off ;
024900101214            clear d01msg;
025000101214
025100101214         SELECT;
025200101214
025300101214         // - F12=Ritorno
025400101214           WHEN  dsp_aid = c_F12;
025500101216             OTA63f12 = 'S';
025600101214             $Fine  = *on ;
025700101214
025800101214         // - F06=Accetta
025900101214           WHEN  dsp_aid = c_F06  ;
026000101214             Exsr SR_ctrd01 ;
026100101214             if  errGenerico = *on;
026200101214                 iter ;
026300101214             endif;
026400101214
026500101214         // aggiorno i record per l'affidamento presunto
026600101214             exsr Sr_AffPre ;
026700101214
026800101216             OTA63dpa = VOFdpa;
026900101214             $Fine = *on ;
027000101214
027100101214           OTHER ;
027200101214
027300101214             Exsr SR_ctrd01 ;
027400101214             if  errGenerico = *on;
027500101214                 iter   ;
027600101214             endif;
027700101214
027800101214         ENDSL;
027900101214
028000101214         ENDDO;
028100101214
028200101214       ENDSR;
028300100803
028400100803       //--------------------------------------------------------------
028500100803       //?Inizializzazione videata D01.                                ?
028600100803       //--------------------------------------------------------------
028700100803       BEGSR  sr_InzD01;
028800100803
028900101215         clear  TA63D01;
029000100803
029100100805         // -?Impostazione campi a video con i parametri ricevuti?
029200101028         // ·?
029300101110
029400101214         Chain(n) (Ita63Nrv: 999) Tivof01l ;
029500101214         If  %found(Tivof01l);
029600101214             If vofdpa <> *zeros ;
029700101214                data_dmy= %date(vofdpa:*iso) ;
029800101214                d01dpa  = %dec(data_dmy)  ;
029900101214                data_eur= %date(vofdpa:*iso) ;
030000101215                d01dpa = %dec(data_eur) ;
030100101214             endif ;
030200101214         Endif ;
030300100803
030400100803       ENDSR;
030500100803
030600101214       //--------------------------------------------------------------
030700101214       //?Controllo videata
030800101214       //--------------------------------------------------------------
030900101214       BEGSR Sr_CtrD01 ;
031000101214
031100101214            WindDspF = IndDspF;
031200101214            reset WindDspFa;
031300101214            IndDspF  = WindDspF;
031400101214
031500101214        // Controllo data presunto affidamento
031600101214           If    d01Dpa  =  0 and ita63fle = 'I';
031700101214                 errMessage  = *on;
031800101214                 errGenerico = *on;
031900101214                 PosCurDpa   = *on;
032000101214                 d01msg = $Msg(03);
032100101214                 leavesr;
032200101214           endif;
032300101214
032400101214           If    d01Dpa  <> 0;
032500101214             clear wlbdat;
032600101214             g02dat = D01Dpa;
032700101214             xsrda8(wlbdat);
032800101214             if g02err = '1';
032900101214               errMessage  = *on;
033000101214               errGenerico = *on;
033100101214               PosCurDpa   = *on;
033200101214               d01msg = $Msg(01);
033300101214               leavesr;
033400101214             endif;
033500101214
033600101214        // Verfiico che non sia inferiore all'anno 2000 e superiore al 2039
033700101214           If    G02Inv < 20000101 or G02Inv > 20391231 ;
033800101214               errMessage  = *on;
033900101214               errGenerico = *on;
034000101214               PosCurDpa   = *on;
034100101214               d01msg = $Msg(01);
034200101214               leavesr;
034300101214             endif;
034400101214
034500101214             d01dpa = g02dat;
034600101214             wrkdpa = g02inv;
034700101214
034800101214        // Verifico che non sia inferiore ad oggi
034900101214           If    G02Inv < datacor8 ;
035000101214               errMessage  = *on;
035100101214               errGenerico = *on;
035200101214               PosCurDpa   = *on;
035300101214               d01msg = $Msg(02) ;
035400101214               leavesr;
035500101214             endif;
035600101214
035700110118           endif;
035800101214       ENDSR;
035900101214
036000101214       //--------------------------------------------------------------
036100101214       //?Conferma data affidamento presunto
036200101214       //--------------------------------------------------------------
036300101214       BEGSR sr_AffPre ;
036400101214
036500101214       // aggiorno l'affidamento presunto su TIVOF
036600101214
036700101214         Chain    (Ita63Nrv: 999) Tivof01l ;
036800101214         If  %found(Tivof01l);
036900110112             If wrkdpa > 0 ;
037000110112               vofdpa = wrkdpa ;
037100110112               vofeso = 'A ' ;
037200110112               update tivof000 ;
037300110112             else ;
037400110112               delete tivof000 ;
037500110112             endif ;
037600101214         endif ;
037700101214         If  not %found(Tivof01l);
037800101214             vofnrv = Ita63Nrv ;
037900101214             vofctr = 999      ;
038000110208             vofdpo = datacor8 ;
038100101214             vofdpa = wrkdpa ;
038200101214             vofeso = 'A ' ;
038300101214             write  tivof000 ;
038400101214         endif ;
038500101214
038600101214       ENDSR ;
038700101214
038800100803       //--------------------------------------------------------------
038900100803       //?Operazioni finali.                                           ?
039000100803       //--------------------------------------------------------------
039100100803       BEGSR  sr_RoutEnd;
039200100803
039300100803
039400100803         // -?Uscita?
039500100803         return;
039600100803
039700100803       ENDSR;
039800100803
039900100803      /end-free
040000100804
040100100804       //--------------------------------------------------------------
040200100804       //?Messaggi di errore.                                          ?
040300100804       //--------------------------------------------------------------
040400101214** - $MSG -------------------------------------------------------------------*
040500101214Data Errata                                                                    1
040600101214Data errata minore della data odierna                                          2
040700101214La Data non può essere uguale a zero                                           3
