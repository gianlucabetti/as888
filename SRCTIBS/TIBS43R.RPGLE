000100161005       //==============================================================
000200161005       //?Gestione *file NOTE per i Profili Utente Aziendali (AZUTE00F)?
000300161005       //==============================================================
000400161005
000500161005       //--------------------------------------------------------------
000600161005       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700161005       //--------------------------------------------------------------
000800161005
000900161005     /*PRM dbgview(*source)
001000161005     /*END
001100161005
001200161005       //--------------------------------------------------------------
001300161005       //?Specifiche di controllo.                                     ?
001400161005       //--------------------------------------------------------------
001500161005
001600161005     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700161005     h dftactgrp(*no)
001800161005
001900161005       //--------------------------------------------------------------
002000161005       //?Dichiarazione file.                                          ?
002100161005       //--------------------------------------------------------------
002200161005
002300161005       // -?Profili Utente?
002400161005     fKPPRF01L  if   e           k disk
002500161005
002600161005       // -?Utenti Aziendali?
002700161005     fAZUTE01L  if   e           k disk
002800161006
002900161006       // -?Organigramma Filiali/Aziendale?
003000161006     fAZORG01L  if   e           k disk
003100161005
003200161006       // -?NOTE Utenti Aziendali?
003300161005     fAZUTN01L  Uf A e           k disk
003400161005
003500161005       // -?Video Gestione?
003600161006     fTIBS43D   cf   e             workstn
003700161005     f                                     sfile( BS43S01 : S01nrr )
003800161005     f                                     indds( IndDspF )
003900161005     f                                     infds( InfDspF )
004000161005
004100161005       //--------------------------------------------------------------
004200161005       //?Definizione costanti.                                        ?
004300161005       //--------------------------------------------------------------
004400161005
004500161005       // -?Numero di righe nel subfile (SFLPAG)?
004600161005     d c_SflPag        c                   const(17)
004700161005
004800161005       // -?Numero massimo di righe di testo elaborabili?
004900161005     d c_MaxRec        c                   const(9999)
005000161005
005100161005       // -?Costante per controllo "caratteri solo numerici"?
005200161005     d c_Digits        c                   const('0123456789')
005300161005
005400161005       // -?Tasti funzionali a video?
005500161005     d c_F01           c                   const(x'31')
005600161005     d c_F02           c                   const(x'32')
005700161005     d c_F03           c                   const(x'33')
005800161005     d c_F04           c                   const(x'34')
005900161005     d c_F05           c                   const(x'35')
006000161005     d c_F06           c                   const(x'36')
006100161005     d c_F07           c                   const(x'37')
006200161005     d c_F08           c                   const(x'38')
006300161005     d c_F09           c                   const(x'39')
006400161005     d c_F10           c                   const(x'3A')
006500161005     d c_F11           c                   const(x'3B')
006600161005     d c_F12           c                   const(x'3C')
006700161005     d c_F13           c                   const(x'B1')
006800161005     d c_F14           c                   const(x'B2')
006900161005     d c_F15           c                   const(x'B3')
007000161005     d c_F16           c                   const(x'B4')
007100161005     d c_F17           c                   const(x'B5')
007200161005     d c_F18           c                   const(x'B6')
007300161005     d c_F19           c                   const(x'B7')
007400161005     d c_F20           c                   const(x'B8')
007500161005     d c_F21           c                   const(x'B9')
007600161005     d c_F22           c                   const(x'BA')
007700161005     d c_F23           c                   const(x'BB')
007800161005     d c_F24           c                   const(x'BC')
007900161005     d c_Enter         c                   const(x'F1')
008000161005     d c_RollDown      c                   const(x'F4')
008100161005     d c_RollUp        c                   const(x'F5')
008200161005
008300161005       //--------------------------------------------------------------
008400161005       //?Definizione schiere.                                         ?
008500161005       //--------------------------------------------------------------
008600161005
008700161005       // -?Messaggi di errore?
008800161007     d sk_Msg          s             78    dim( 3)  ctdata  perrcd(01)
008900161005
009000161005       //--------------------------------------------------------------
009100161005       //?Definizione aree dati.                                       ?
009200161005       //--------------------------------------------------------------
009300161005
009400161005       // -?Dati utente?
009500161005     d §AzUte        e ds                  extname(AZUTE00F)
009600161005     d                                     dtaara
009700161005     d §DatiUte      e ds                  extname(dDatiUte)
009800161005     d                                     dtaara
009900161005
010000161005       //--------------------------------------------------------------
010100161005       //?Definizione strutture dati.                                  ?
010200161005       //--------------------------------------------------------------
010300161005
010400161005       // -?Status ds?
010500161006     d Status         sds                  qualified
010600161005     d   SDSpgm          *proc
010700161006     d   SDSjobName          244    253
010800161006     d   SDSjobUser          254    263
010900161006     d   SDSjobNbr           264    269s 0
011000161005
011100161005       // -?InfDS?
011200161006     d InfDspF         ds                  qualified
011300161005     d   dsp_aid             369    369a
011400161005     d   sfl_rrn             376    377i 0
011500161005     d   min_nrr             378    379i 0
011600161005     d   num_rcds            380    381i 0
011700161005
011800161005       // -?Indicatori su DspF?
011900161005     d IndDspF         ds                  inz
012000161006         // -?Abilitazione tasti funzionali?
012100161006     d   $F3Attivo                     n   overlay( IndDspF : 03 )
012200161006     d   $F6Attivo                     n   overlay( IndDspF : 06 )
012300161006     d   $F12Attivo                    n   overlay( IndDspF : 12 )
012400161006     d   $RlUpAttivo                   n   overlay( IndDspF : 25 )
012500161006         // -?Emissione messaggio di errore?
012600161006     d   $ErrMessage                   n   overlay( IndDspF : 28 )
012700161006         // -?Indicatori di gestione del subfile?
012800161006     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
012900161006     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
013000161006     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
013100161006     d   $SflEnd                       n   overlay( IndDspF : 33 )
013200161006         // -?Indicatori per Attibuti di visualizzazione?
013300161006     d   $InsertNOT                    n   overlay( IndDspF : 41 )
013400161006         // -?Posizionamento cursore & Segnalazione errore?
013500161006     d   $PosCurNOT                    n   overlay( IndDspF : 51 )
013600161006         // -?Errore di paginazione?
013700161006     d   $ErrRoll                      n   overlay( IndDspF : 98 )
013800161006         // -?Riemissione videata?
013900161006     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
014000161005
014100161005       // -?Parametri ricevuti?
014200161005     d KPJBA         e ds
014300161006     d TIBS43ds      e ds                  qualified      inz
014400161006
014500161006       // -?Data/Ora attuali?
014600161006     d wTime_ds        ds                  qualified      inz
014700161006     d   wDate                        8s 0 inz
014800161006     d   wTime                        6s 0 inz
014900161006
015000161006       // -?Dati a video?
015100161006     d Note_ds         ds                  inz
015200161006     d   V1Hute
015300161006     d   V1Htrc
015400161006     d*//V1Hprg
015500161006     d   V1Hdin
015600161006     d   V1Hhin
015700161006     d   V1Huin
015800161006     d   V1Hjin
015900161006     d SaveNote_ds     ds                  qualified      inz
016000161007     d   W1Hute                            like( V1Hute ) inz(*hival)
016100161007     d   W1Htrc                            like( V1Htrc ) inz(*hival)
016200161007     d*//W1Hprg                            like( V1Hprg ) inz(*hival)
016300161007     d   W1Hdin                            like( V1Hdin ) inz(*hival)
016400161007     d   W1Hhin                            like( V1Hhin ) inz(*hival)
016500161007     d   W1Huin                            like( V1Huin ) inz(*hival)
016600161007     d   W1Hjin                            like( V1Hjin ) inz(*hival)
016700161005
016800161005       //--------------------------------------------------------------
016900161005       //?Definizione variabili globali.                               ?
017000161005       //--------------------------------------------------------------
017100161005
017200161005       // -?Flags booleani?
017300161005     d $Fine           s               n   inz
017400161005     d $InzS01         s               n   inz(*on)
017500161006     d $Note           s               n   inz
017600161007     d $Avviso         s               n   inz
017700161005
017800161005       // -?Indici di schiera?
017900161005     d xx              s              3  0 inz
018000161005
018100161005       // -?Variabili per la gestione del video?
018200161005     d $Video          s              2    inz('S1')
018300161005     d S01nrr          s                   like(C1RcdNbr) inz
018400161006     d wNrr1Free       s                   like(C1RcdNbr) inz
018500161006     d w_SflPag        s              3  0 inz
018600161006     d wPag            s              5  0 inz
018700161006     d wRig            s              3  0 inz
018800161005
018900161005       // -?Campi di comodo?
019000161005     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
019100161007     d wLastPRG        s                   like(UTNprg)   inz
019200161005
019300161005       //--------------------------------------------------------------
019400161005       //?Definizione prototipi procedure.                             ?
019500161005       //--------------------------------------------------------------
019600161005
019700161005       // -?Reperimento dati utente?
019800161005     d TIBS34ds      e ds                  inz
019900161005      /copy gaitrasrc/srcProtoPR,TIBS34R
020000161006
020100161006       // -?"Bonifica" una stringa da caratteri indesiderati?
020200161006      /copy gaitrasrc/srcProtoPI,XCHKCHAR
020300161006      /copy gaitrasrc/srcProtoPR,XCHKCHAR
020400161005
020500161005       //--------------------------------------------------------------
020600161005       //?Definizione key-list.                                        ?
020700161005       //--------------------------------------------------------------
020800161005
020900161005       // -?File KPPRF01L?
021000161005     d keyKPPRF01    e ds                  extname( KPPRF01L : *key )
021100161005     d                                     prefix( k_ )   inz
021200161005
021300161005       // -?File AZUTE01L?
021400161005     d keyAZUTE01    e ds                  extname( AZUTE01L : *key )
021500161005     d                                     prefix( k_ )   inz
021600161006
021700161006       // -?File AZUTN01L?
021800161006     d keyAZUTN01    e ds                  extname( AZUTN01L : *key )
021900161006     d                                     prefix( k_ )   inz
022000161005
022100161005       //--------------------------------------------------------------
022200161005       //?Riepilogo indicatori utilizzati.                             ?
022300161005       //--------------------------------------------------------------
022400161005       //--------------------------------------------------------------
022500161005
022600161005       //--------------------------------------------------------------
022700161005       //?M A I N - L I N E                                            ?
022800161005       //--------------------------------------------------------------
022900161005
023000161005     c     *Entry        plist
023100161005     c                   parm                    KPJBA
023200161005
023300161005      /free
023400161005
023500161005       // -?Operazioni iniziali?
023600161005       exsr  sr_RoutInz;
023700161005
023800161005       // -?Gestione videate?
023900161006       DoW  $Fine = *off;
024000161005
024100161005         select;
024200161005
024300161005           // -?Gestione subfile S1?
024400161005           when $Video = 'S1';
024500161005             exsr  sr_GesS01;
024600161005
024700161005           // -?Gestione videata ????
024800161005           other;
024900161005             $Fine = *on;
025000161005
025100161005         endsl;
025200161005
025300161006       EndDo;
025400161005
025500161005       // -?Operazioni finali?
025600161005       exsr  sr_RoutEnd;
025700161005
025800161005       //--------------------------------------------------------------
025900161005       //?Operazioni iniziali.                                         ?
026000161005       //--------------------------------------------------------------
026100161005       BEGSR  sr_RoutInz;
026200161005
026300161005         // -?Ricezione parametri?
026400161005         TIBS43ds = KPJBU;
026500161006         TIBS43ds.oBS43err = *off;
026600161006         clear  TIBS43ds.oBS43msg;
026700161005
026800161005         // -?Impostazione chiusura *pgm?
026900161005         select;
027000161006           when  TIBS43ds.iBS43tla = *blank;
027100161005             *inRT = *on;
027200161006           when  TIBS43ds.iBS43tla = 'C';
027300161005             *inLR = *on;
027400161005             $Fine = *on;
027500161005             leavesr;
027600161006           //*·when  TIBS43ds.iBS43tla = 'L';
027700161005           other;
027800161005             *inLR = *on;
027900161005         endsl;
028000161007
028100161007         // -?Controllo parametri ricevuti?
028200161007         if  ( TIBS43ds.iBS43opz <> *blank  and
028300161007               TIBS43ds.iBS43opz <> '4'     and
028400161007               TIBS43ds.iBS43opz <> '5' )   OR
028500161007             TIBS43ds.iBS43ute   =  *blank;
028600161007           TIBS43ds.oBS43err = *on;
028700161007           TIBS43ds.oBS43msg = 'Ricevuti parametri errati';
028800161007           $Fine = *on;
028900161007           leavesr;
029000161007         endif;
029100161005
029200161005         // -?Reperimento dati job?
029300161005         exsr  sr_DatiJob;
029400161005
029500161005         // -?Impostazione nome programma a video?
029600161006         VTCpgm = Status.SDSpgm;
029700161005
029800161005         // -?Re-impostazione flags booleani (fosse mai il 2° richiamo)?
029900161005         reset  $Fine;
030000161005         reset  $InzS01;
030100161005         reset  $Video;
030200161006
030300161007         // -?Richiesta la Cancellazione Note Profilo Utente?
030400161007         if  TIBS43ds.iBS43opz = '4';
030500161006           exsr  sr_DltAZUTN;
030600161006           $Fine = *on;
030700161006         endif;
030800161005
030900161005       ENDSR;
031000161005
031100161005       //--------------------------------------------------------------
031200161005       //?Reperimento Dati del job (Utente/Operativi).                 ?
031300161005       //--------------------------------------------------------------
031400161005       BEGSR  sr_DatiJob;
031500161005
031600161005         in(E) §AzUte;
031700161005         if NOT %error;
031800161005           in(E) §DatiUte;
031900161005         endif;
032000161005         if %error or RSut = *blank;
032100161005           clear TIBS34ds;
032200161005           tibs34r ( tibs34ds );
032300161005           in §AzUte;
032400161005           in §DatiUte;
032500161005         endif;
032600161005
032700161005       ENDSR;
032800161005
032900161005       //--------------------------------------------------------------
033000161007       //?Gestione SubFile S01.                                        ?
033100161005       //--------------------------------------------------------------
033200161006       BEGSR  sr_GesS01;
033300161005
033400161006         // -?Inizializzazione subfile?
033500161006         if  $InzS01 = *on;
033600161006           exsr  sr_InzS01;
033700161006           $InzS01 = *off;
033800161006           // -?Verifica raggiungimento del limite di records?
033900161006           //  ?registrabili nel subfile (9999)?
034000161006           if  S01nrr >= c_MaxRec;
034100161006             $ErrGenerico = *on;
034200161006             $ErrMessage  = *on;
034300161006             VIDmsg = sk_Msg(01);
034400161006           endif;
034500161005         endif;
034600161005
034700161006         // -?Emissione Testata & Piede con tasti funzionali abilitati?
034800161005         write  BS43T01;
034900161005         write  BS43Z01;
035000161006
035100161006         // -?Posizionamento cursore?
035200161007         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035300161006         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035400161006         //  ?si visualizza la pagina che era visualizzata quando?
035500161006         //  ?l'utente ha premuto l'ultimo tasto.?
035600161006         if  C1CsrRrn > *zero;
035700161006           C1RcdNbr = C1CsrRrn;
035800161006         else;
035900161006           C1RcdNbr = 1;
036000161006           write  BS43S00;          //(no rec.)?
036100161006         endif;
036200161005
036300161007         // -?Emissione subfile?
036400161005         exfmt  BS43C01;
036500161005
036600161006         clear  VIDmsg;
036700161006         clear  $ErrMessage;
036800161006         clear  $ErrGenerico;
036900161005
037000161006         Select;
037100161005
037200161005           // -?F3=Fine?
037300161006           When  InfDspF.dsp_aid = c_F03;
037400161007             if  TIBS43ds.iBS43opz <> '5';
037500161007               exsr  sr_CtrS01;
037600161007             endif;
037700161007             if  NOT $ErrGenerico;
037800161007               TIBS43ds.oBS43fxx = '03';
037900161007               $Fine = *on;
038000161007             endif;
038100161005
038200161005           // -?F12=Ritorno?
038300161006           WHEN  InfDspF.dsp_aid = c_F12;
038400161007             if  TIBS43ds.iBS43opz <> '5';
038500161007               exsr  sr_CtrS01;
038600161007             endif;
038700161007             if  NOT $ErrGenerico;
038800161007               TIBS43ds.oBS43fxx = '12';
038900161007               $Fine = *on;
039000161007             endif;
039100161006
039200161007           // -?Subfile vuoto?
039300161006           WHEN  S01nrr  = *zero;
039400161006
039500161006           // -?Roll-Up?
039600161006           WHEN  InfDspF.dsp_aid = c_RollUp;
039700161006             // -?Verifica raggiungimento del limite di records?
039800161006             //  ?registrabili nel subfile (9999)?
039900161006             if  S01nrr >= c_MaxRec;
040000161006               $ErrMessage  = *on;
040100161006               $ErrGenerico = *on;
040200161006               VIDmsg = sk_Msg(01);
040300161006               leavesr;
040400161006             endif;
040500161006             exsr sr_RollUpS01;
040600161005
040700161005           // -?Invio / F6=Conferma?
040800161005           OTHER;
040900161007             // -?Controllo dati (se NON visualizzazione)?
041000161007             if  TIBS43ds.iBS43opz <> '5';
041100161007               exsr  sr_CtrS01;
041200161007             endif;
041300161006             select;
041400161006               // -?Rilevato errore?
041500161006               when  $ErrGenerico = *on;
041600161006                 leavesr;
041700161006               // -?F6=Conferma?
041800161006               when  InfDspF.dsp_aid = c_F06;
041900161006                 exsr  sr_WrtAZUTN;
042000161006                 TIBS43ds.oBS43fxx = '06';
042100161006                 $Fine = *on;
042200161006             endsl;
042300161005
042400161005         ENDSL;
042500161005
042600161005       ENDSR;
042700161005
042800161005       //--------------------------------------------------------------
042900161007       //?Inizializzazione SubFile S01.                                ?
043000161005       //--------------------------------------------------------------
043100161006       BEGSR  sr_InzS01;
043200161006
043300161006         // -?Pulizia subfile?
043400161006         $SflDsp_N    = *on;
043500161006         $SflDspCtl_N = *on;
043600161006         write  BS43C01;
043700161006         $SflDspCtl_N = *off;
043800161006         $SflEnd      = *off;
043900161006
044000161006         $SflNxtChg   = *off;
044100161006
044200161006         clear  w_SflPag;
044300161006         clear  C1RcdNbr;
044400161006         clear  C1CsrRrn;
044500161006         clear  S01nrr;
044600161006         clear  VIDmsg;
044700161006         $ErrMessage  = *off;
044800161006         $ErrGenerico = *off;
044900161007         clear  wNrr1Free;
045000161007         clear  wLastPRG;
045100161006
045200161006         // -?Spegnimento degli indicatori di errore?
045300161006         %subst( IndDspF : 50 ) = *off;
045400161006
045500161006         // -?Impostazione dati nel subfile-control?
045600161006         C1Cute = TIBS43ds.iBS43ute;
045700161006         clear  C1Dute;
045800161006         clear  C1Cfil;
045900161006         clear  C1Dfil;
046000161006         chain  ( TIBS43ds.iBS43ute )  KPPRF000;
046100161006         if  %found( KPPRF01L );
046200161006           C1Dute = PRFtxt;
046300161006         endif;
046400161006         chain  ( TIBS43ds.iBS43ute )  AZUTE000;
046500161006         if  %found( AZUTE01L );
046600161006           C1Cfil = UTEfil;
046700161006           chain  ( UTEfil )  AZORG01L;
046800161006           if  %found( AZORG01L );
046900161006             C1Dfil = ORGdes;
047000161006           endif;
047100161006         endif;
047200161006
047300161006         // -?Caricamento della 1ª pagina di dati del subfile?
047400161006         clear  keyAZUTN01;
047500161006         k_UTNute = TIBS43ds.iBS43ute;
047600161006         k_UTNtrc = TIBS43ds.iBS43trc;
047700161006         setll     %kds( keyAZUTN01 )  AZUTN000;
047800161006         reade(N)  %kds( keyAZUTN01 : 2 )  AZUTN000;
047900161006         exsr  sr_RollUpS01;
048000161006
048100161006         // -?(Dis)Attivazione tasti funzionali?
048200161018         $F3Attivo   = *off;
048300161007         $F6Attivo   = ( TIBS43ds.iBS43opz = *blank );
048400161006         $F12Attivo  = *on;
048500161007         $RlUpAttivo = ( TIBS43ds.iBS43opz = *blank  or
048600161007                         NOT %eof( AZUTN01L ) );
048700161006
048800161006         // -?Impaginazione subfile?
048900161006         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
049000161006         if  S01nrr > *zero;
049100161006           C1RcdNbr = 1;
049200161006           C1CsrRrn = 1;
049300161006         else;
049400161006           clear  C1RcdNbr;
049500161006         endif;
049600161005
049700161005       ENDSR;
049800161006
049900161006       //--------------------------------------------------------------
050000161006       //?Caricamento singola pagine SubFile S01.                      ?
050100161006       //--------------------------------------------------------------
050200161006       BEGSR  sr_RollUpS01;
050300161006
050400161006         S01nrr     = ( w_SflPag * c_SflPag );
050500161006         w_SflPag  += 1;
050600161006         $SflNxtChg = *off;
050700161006
050800161007
050900161006         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
051000161006         DoW  not %eof( AZUTN01L )              and
051100161006              S01nrr < ( w_SflPag * c_SflPag )  and
051200161006              S01nrr < c_MaxRec;
051300161006
051400161006           // -?Caricamento dati nel record del subfile?
051500161006           exsr  sr_RieRecS01;
051600161007
051700161006           S01nrr += 1;
051800161006           write  BS43S01;
051900161006
052000161006           // -?Lettura record successivo nell'archivio?
052100161006           reade(N)  %kds( keyAZUTN01 : 2 )  AZUTN000;
052200161006
052300161006         EndDo;
052400161006
052500161007
052600161006         // -?Eventuale aggiunta di record vuoti (nuove note)?
052700161006         //  ?SE richiesto l'Inserimento (non la Visualizzazione)?
052800161007         If  TIBS43ds.iBS43opz = *blank;
052900161007
053000161007           clear  Note_ds;
053100161007
053200161007           DoW  S01nrr < ( w_SflPag * c_SflPag )   and
053300161007                S01nrr < c_MaxRec;
053400161007
053500161007             clear  BS43S01;
053600161007
053700161007             if  Note_ds <> SaveNote_ds;
053800161007               wDate_EUR = %date();
053900161007               V1Odin = %dec( wDate_EUR );
054000161007               V1Ohin = %dec( %time() );
054100161007               V1Ouin = Status.SDSjobUser;
054200161007               V1Ojin = Status.SDSjobName;
054300161007               SaveNote_ds = Note_ds;
054400161007             endif;
054500161007
054600161007             $InsertNOT = *on;
054700161007             $PosCurNOT = *on;
054800161007
054900161007             S01nrr    += 1;
055000161007             write  BS43S01;
055100161007
055200161007             // -?Memorizzaz. del nrr del primo record vuoto (inseribile)?
055300161007             if  wNrr1Free = *zero;
055400161007               wNrr1Free = S01nrr;
055500161007             endif;
055600161007
055700161007           EndDo;
055800161007
055900161007         EndIf;
056000161007
056100161006
056200161006         // -?Visualizzazione del SFL (se ci sono dati)?
056300161006         $SflDsp_N   = ( S01nrr <= *zero );
056400161006
056500161006         // -?Attivazione (eventuale) del SFLEND?
056600161007         if  TIBS43ds.iBS43opz = '5';
056700161006           $SflEnd   = ( %eof( AZUTN01L ) );
056800161006         else;
056900161006           $SflEnd   = ( S01nrr >= c_MaxRec );
057000161006         endif;
057100161006         $RlUpAttivo = ( Not $SflEnd );
057200161006
057300161006         // -?Posizionamento cursore al 1° rcd della pagina?
057400161006         if  S01nrr > *zero;
057500161006           wPag     = S01nrr / c_SflPag;
057600161006           wRig     = S01nrr - ( c_SflPag * wPag );
057700161006           C1RcdNbr = wPag * c_SflPag;
057800161006           if  wRig > *zeros;
057900161006             C1RcdNbr += 1;
058000161006           else;
058100161006             C1RcdNbr  = C1RcdNbr - c_SflPag + 1;
058200161006           endif;
058300161006         else;
058400161006           clear  C1RcdNbr;
058500161006         endif;
058600161006
058700161006         C1CsrRrn = C1RcdNbr;
058800161006
058900161006       ENDSR;
059000161006
059100161006       //--------------------------------------------------------------
059200161006       //?Caricamento singolo record nel SubFile S01.                  ?
059300161006       //--------------------------------------------------------------
059400161006       BEGSR  sr_RieRecS01;
059500161006
059600161006         clear  BS43S01;
059700161006
059800161006         if  Not %eof( AZUTN01L );
059900161007
060000161007           // -?Memorizzazione dell'ultimo progressivo già utilizzato?
060100161007           //  ?in AZUTN00F?
060200161007           wLastPRG  = UTNprg;
060300161006
060400161006           // -?Record annullato: da NON riportare?
060500161006           if  UTNatb <> *blank;
060600161006             leavesr;
060700161006           endif;
060800161006
060900161006           $InsertNOT = *off;
061000161006
061100161006           // -?Impostazione campi Hidden?
061200161006           V1Hute = UTNute;
061300161006           V1Htrc = UTNtrc;
061400161006           V1Hprg = UTNprg;
061500161006           V1Hdin = UTNdin;
061600161006           V1Hhin = UTNhin;
061700161006           V1Hjin = UTNjin;
061800161006
061900161006           // -?Impostazione campi Output?
062000161006           if  Note_ds <> SaveNote_ds;
062100161006             wDate_EUR = %date( UTNdin : *iso );
062200161006             V1Odin = %dec( wDate_EUR );
062300161006             V1Ohin = UTNhin;
062400161006             V1Ouin = UTNuin;
062500161006             V1Ojin = UTNjin;
062600161006           endif;
062700161006
062800161006           SaveNote_ds = Note_ds;
062900161006
063000161006           // -?Impostazione campi Output (reperiti) o Input (vuoti)?
063100161006           V1Inot = UTNnot;
063200161006
063300161006         endif;
063400161006
063500161006       ENDSR;
063600161006
063700161006       //--------------------------------------------------------------
063800161007       //?Controllo SubFile S01.                                       ?
063900161006       //--------------------------------------------------------------
064000161006       BEGSR  sr_CtrS01;
064100161006
064200161006         %subst( IndDspF : 50 ) = *off;
064300161006         $SflNxtChg = *off;
064400161006
064500161006         $Note      = *off;
064600161006
064700161006         // -?Ciclo di lettura/controllo record inseriti?
064800161006         readc  BS43S01;
064900161006
065000161006         DoW  not  %eof( TIBS43D );
065100161006
065200161007           // -?Verifica SE inserite nuove note?
065300161006           if  V1Hute = *blank  and  V1Inot <> *blank;
065400161007             $Note      = *on;
065500161007             $SflNxtChg = *on;
065600161006           endif;
065700161006
065800161006           $InsertNOT = ( V1Hute = *blank  and
065900161006                          V1Hprg = *zero   and
066000161006                          V1Hdin = *zero   and
066100161006                          V1Huin = *blank  and
066200161006                          V1Hjin = *blank );
066300161007
066400161007           // -?Aggiornamento orario di inserimento?
066500161007           //  ?(nel primo rec. di I/O)?
066600161007           if  $InsertNOT  and  V1Ohin <> *zero;
066700161007             V1Ohin = %dec( %time() );
066800161007           endif;
066900161007
067000161007           C1CsrRrn   = S01nrr;
067100161007           C1RcdNbr   = C1CsrRrn;
067200161006
067300161006           update  BS43S01;
067400161006
067500161006           readc  BS43S01;
067600161006
067700161006         EndDo;
067800161006
067900161007         Select;
068000161007
068100161007           // -?SE NON trovate nuove note e premuto Invio o F6:?
068200161007           //  ?posizionamento cursore sul primo rec. di I/O?
068300161007           When  Not $Note  and  ( InfDspF.dsp_aid = c_Enter  or
068400161007                                   InfDspF.dsp_aid = c_F06 );
068500161007
068600161007             doW  wNrr1Free = *zero;
068700161007               exsr  sr_RollUpS01;
068800161007             enddo;
068900161007
069000161007             chain  wNrr1Free  BS43S01;
069100161007             C1CsrRrn   = wNrr1Free;
069200161007             C1RcdNbr   = C1CsrRrn;
069300161007             $PosCurNOT = *on;
069400161007             update  BS43S01;
069500161007
069600161018             //*·// -?Emissione errore solo SE premuto F6?
069700161018             //*·if  InfDspF.dsp_aid = c_F06;
069800161018             //*·  $ErrGenerico = *on;
069900161018             //*·  $ErrMessage  = *on;
070000161018             //*·  VIDmsg = sk_Msg(02);
070100161018             //*·endif;
070200161007
070300161007           // -?SE trovate nuove note e premuto F3 o F12:?
070400161007           //  ?avviso della perdita?
070500161007           When  $Note  and  ( InfDspF.dsp_aid = c_F12  or
070600161007                               InfDspF.dsp_aid = c_F03 );
070700161007
070800161007             if  Not $Avviso;
070900161007               $ErrGenerico = *on;
071000161007               $ErrMessage  = *on;
071100161007               VIDmsg = sk_Msg(03);
071200161007             endif;
071300161007             $Avviso  = *on;
071400161007
071500161007           // -?Altrimenti: si resetta il flag di avviso della perdita?
071600161007           Other;
071700161007             $Avviso  = *off;
071800161007
071900161007         EndSl;
072000161006
072100161006       ENDSR;
072200161005
072300161005       //--------------------------------------------------------------
072400161006       //?Scrittura records *file AZUTN00F.                            ?
072500161005       //--------------------------------------------------------------
072600161006       BEGSR  sr_WrtAZUTN;
072700161006
072800161006         // -?Reperimento data e ora attuali?
072900161006         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
073000161005
073100161005         // -?Ciclo di lettura del subfile?
073200161007         readc  BS43S01;
073300161007
073400161007         DoW  not  %eof( TIBS43D );
073500161006
073600161007           If  V1Inot <> *blank  and  V1Hute = *blank
073700161007                                 and  V1Hprg = *zero
073800161007                                 and  V1Hdin = *zero
073900161007                                 and  V1Huin = *blank
074000161007                                 and  V1Hjin = *blank;
074100161006
074200161006             // -?Copio i nuovi records dal video al file?
074300161006             //  ?(proseguendo dall'ultimo progressivo note reperito in fase di?
074400161006             //  ?caricamento del subfile)?
074500161007             wLastPrg += 1;
074600161006
074700161006             clear  AZUTN000;
074800161006
074900161006             //*·UTNatb = *blank;
075000161006             UTNute = TIBS43ds.iBS43ute;
075100161006             UTNtrc = TIBS43ds.iBS43trc;
075200161007             UTNprg = wLastPrg;
075300161006             UTNnot = V1Inot;
075400161006             UTNdin = wTime_ds.wDate;
075500161006             UTNhin = wTime_ds.wTime;
075600161006             UTNuin = Status.SDSjobUser;
075700161006             UTNjin = Status.SDSjobName;
075800161006
075900161006             // -?Controllo il campo nota che non contenga?
076000161006             //  ?caratteri particolari?
076100161006             exsr  sr_ChkNota;
076200161006
076300161006             //______________
076400161006             write  AZUTN000;
076500161006             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076600161006
076700161006           EndIf;
076800161007
076900161007           readc  BS43S01;
077000161007
077100161007         EndDo;
077200161005
077300161005       ENDSR;
077400161006
077500161006       //--------------------------------------------------------------
077600161006       //?Cancellazione record *file AZUTN00F.                         ?
077700161006       //--------------------------------------------------------------
077800161006       BEGSR  sr_DltAZUTN;
077900161006
078000161006         clear  keyAZUTN01;
078100161006         k_UTNute = TIBS43ds.iBS43ute;
078200161006         k_UTNtrc = TIBS43ds.iBS43trc;
078300161006         k_UTNprg = TIBS43ds.iBS43prg;
078400161006
078500161006         setll  %kds( keyAZUTN01 )  AZUTN000;
078600161006
078700161006         if  TIBS43ds.iBS43prg = *zero;
078800161006           reade  %kds( keyAZUTN01 : 2 )  AZUTN000;
078900161006         else;
079000161006           reade  %kds( keyAZUTN01 )  AZUTN000;
079100161006         endif;
079200161006
079300161006         DoW  Not %eof( AZUTN01L );
079400161006
079500161006           //_______________
079600161006           delete  AZUTN000;
079700161006           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079800161006
079900161006           if  TIBS43ds.iBS43prg = *zero;
080000161006             reade  %kds( keyAZUTN01 : 2 )  AZUTN000;
080100161006           else;
080200161006             reade  %kds( keyAZUTN01 )  AZUTN000;
080300161006           endif;
080400161006
080500161006         EndDo;
080600161006
080700161006       ENDSR;
080800161006
080900161006       //--------------------------------------------------------------
081000161006       //?Controllo caratteri nella singola nota.                      ?
081100161006       //--------------------------------------------------------------
081200161006       BEGSR  sr_ChkNota;
081300161006
081400161006         clear  TxtInOut;
081500161006         TxtInOut = UTNnot;
081600161006
081700161006         xChkChar ( TxtInOut : ElencoChar : TipoElenco :
081800161006                    CharSost : UpperCase  : ChkNull :
081900161006                    CharNull : Esito );
082000161006
082100161006         if  Esito = '1';
082200161006           UTNnot = TxtInOut;
082300161006         endif;
082400161006
082500161006       ENDSR;
082600161005
082700161005       //--------------------------------------------------------------
082800161005       //?Operazioni finali.                                           ?
082900161005       //--------------------------------------------------------------
083000161005       BEGSR  sr_RoutEnd;
083100161005
083200161005         // -?Uscita dal *pgm?
083300161005         return;
083400161005
083500161005       ENDSR;
083600161005
083700161005      /end-free
083800161005
083900161005       //--------------------------------------------------------------
084000161005       //?Definizione schiere a tempo di compilazione                  ?
084100161005       //--------------------------------------------------------------
084200161005
084300161006** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
084400161006Rilevate note oltre il numero massimo di quelle visualizzabili.                 1
084500161006Inserire almeno una nota o uscire SENZA confermare (con F3 o F12)               2
084600161007Avviso: uscendo senza confermare si perderanno le note appena inserite          3
