000100141024       //==============================================================
000200150112       //?TRKC10R * Gestione Note per Campagna Cliente                 ?
000300141024       //==============================================================
000400141024
000500141024       //--------------------------------------------------------------
000600141024       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700141024       //--------------------------------------------------------------
000800141024
000900141024     /*PRM  dbgview(*source)
001000141024     /*END
001100141024
001200141024       //--------------------------------------------------------------
001300141024       //?Specifiche di controllo.                                     ?
001400141024       //--------------------------------------------------------------
001500141024
001600141024     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700141024     h dftactgrp(*no) actgrp(*caller)
001800141024
001900141024       //--------------------------------------------------------------
002000141024       //?Dichiarazione file.                                          ?
002100141024       //--------------------------------------------------------------
002200141024
002300141024       // -?Anagrafica Clienti Potenziali?
002400141024     fTNCPO01L  if   e           k disk
002500141028
002600141028       // -?Campagne Commerciali?
002700141028     fTICMP01L  if   e           k disk
002800141024
002900141028       // -?Note Campagne Commerciali?
003000141024     fTICMN01L  Uf A e           k disk    usropn
003100141024     f                                     commit($FlgCmt)
003200141024
003300141024       // -?Video Gestione?
003400150112     fTRKC10D   cf   e             workstn
003500150112     f                                     sfile(KC10S01 : S01nrr)
003600141024     f                                     indds(IndDspF)
003700141024     f                                     infds(InfDspF)
003800141024
003900141024       //--------------------------------------------------------------
004000141024       //?Definizione costanti.                                        ?
004100141024       //--------------------------------------------------------------
004200141024
004300141024       // -?Numero di record in ciascuna videata di subfile?
004400141024     d c_SflPag        c                   const(19)
004500141024
004600141031       // -?Numero massimo di record gestibili in un SubFile?
004700141024     d c_MaxRec        c                   const(9999)
004800141024
004900141024       // -?Tasti funzionali a video?
005000141024     d c_F01           c                   const(x'31')
005100141024     d c_F02           c                   const(x'32')
005200141024     d c_F03           c                   const(x'33')
005300141024     d c_F04           c                   const(x'34')
005400141024     d c_F05           c                   const(x'35')
005500141024     d c_F06           c                   const(x'36')
005600141024     d c_F07           c                   const(x'37')
005700141024     d c_F08           c                   const(x'38')
005800141024     d c_F09           c                   const(x'39')
005900141024     d c_F10           c                   const(x'3A')
006000141024     d c_F11           c                   const(x'3B')
006100141024     d c_F12           c                   const(x'3C')
006200141024     d c_F13           c                   const(x'B1')
006300141024     d c_F14           c                   const(x'B2')
006400141024     d c_F15           c                   const(x'B3')
006500141024     d c_F16           c                   const(x'B4')
006600141024     d c_F17           c                   const(x'B5')
006700141024     d c_F18           c                   const(x'B6')
006800141024     d c_F19           c                   const(x'B7')
006900141024     d c_F20           c                   const(x'B8')
007000141024     d c_F21           c                   const(x'B9')
007100141024     d c_F22           c                   const(x'BA')
007200141024     d c_F23           c                   const(x'BB')
007300141024     d c_F24           c                   const(x'BC')
007400141024     d c_Enter         c                   const(x'F1')
007500141024     d c_RollDown      c                   const(x'F4')
007600141024     d c_RollUp        c                   const(x'F5')
007700141024
007800141024       //--------------------------------------------------------------
007900141024       //?Definizione schiere.                                         ?
008000141024       //--------------------------------------------------------------
008100141024
008200141028       // -?Messaggi di errore?
008300141028     d sk_Msg          s             78    dim( 1)  ctdata  perrcd( 1)
008400141024
008500141024       //--------------------------------------------------------------
008600141024       //?Definizione aree dati.                                       ?
008700141024       //--------------------------------------------------------------
008800141024
008900141024       // -?Dati utente?
009000141024     d §AzUte        e ds                  extname(AZUTE00F)
009100141024     d                                     dtaara
009200141024     d §DatiUte      e ds                  extname(dDatiUte)
009300141024     d                                     dtaara
009400141024
009500141024       //--------------------------------------------------------------
009600141024       //?Definizione strutture dati.                                  ?
009700141024       //--------------------------------------------------------------
009800141024
009900141024       // -?Status ds?
010000141024     d Status         sds
010100141024     d   SDSpgm          *proc
010200141024     d*//SDSprm          *parms
010300141024     d*//SDSusr              254    263
010400141024
010500141024       // -?InfDS?
010600141024     d InfDspF         ds
010700141024     d   dsp_aid             369    369a
010800141024     d*//sfl_rrn             376    377i 0
010900141024     d*//min_nrr             378    379i 0
011000141024     d*//num_rcds            380    381i 0
011100141024
011200141024       // -?Indicatori su DspF?
011300141024     d IndDspF         ds                  inz
011400141024         // -?Abilitazione tasti funzionali?
011500141024     d   F3Attivo                      n   overlay(IndDspF : 03)
011600141024     d   F6Attivo                      n   overlay(IndDspF : 06)
011700141031     d   F10Attivo                     n   overlay(IndDspF : 10)
011800141024     d   F12Attivo                     n   overlay(IndDspF : 12)
011900141030     d*//DescrINVIO                    n   overlay(IndDspF : 25)
012000141024         // -?Emissione messaggio di errore?
012100141024     d   ErrMessage                    n   overlay(IndDspF : 28)
012200141024         // -?Indicatori di gestione del subfile?
012300141024     d   SflDsp_N                      n   overlay(IndDspF : 30)
012400141024     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012500141024     d   SflNxtChg                     n   overlay(IndDspF : 32)
012600141024     d   SflEnd                        n   overlay(IndDspF : 33)
012700141024         // -?Indicatori per Attributi di visualizzazione?
012800141024     d   DisplayKSU                    n   overlay(IndDspF : 41)
012900141024     d   DisplayKSC                    n   overlay(IndDspF : 42)
013000141024     d   DisplayCPO                    n   overlay(IndDspF : 43)
013100141118     d   EvidenzFCM                    n   overlay(IndDspF : 44)
013200141203     d   EvidenzRIS                    n   overlay(IndDspF : 45)
013300141024     d   ProtectNOT                    n   overlay(IndDspF : 49)
013400141024         // -?Posizionamento cursore & segnalazione errore?
013500141024     d   PosCurNOT                     n   overlay(IndDspF : 50)
013600141024         // -?Riemissione videata?
013700141024     d   ErrGenerico                   n   overlay(IndDspF : 99)
013800141028
013900141028       // -?Data/Ora immissione Nota?
014000141028     d wTime_ds        ds                  inz
014100141028     d   CMNdim                            inz
014200141028     d   CMNhim                            inz
014300141024
014400141024       // -?Parametri ricevuti?
014500141024     d KPJBA         e ds
014600150112     d TRKC10ds      e ds
014700141024
014800150112     d SaveTRKC10ds  e ds                  extname(TRKC10ds)
014900141024     d                                     inz   qualified
015000141118
015100141118       // -?Tabella "FCM" = Fasi Campagne Commerciali?
015200141118     d dFCM          e ds                  inz   qualified
015300141024
015400141024       //--------------------------------------------------------------
015500141024       //?Definizione variabili globali.                               ?
015600141024       //--------------------------------------------------------------
015700141024
015800141024       // -?Flags booleani?
015900141030     d $FlgCmt         s               n   inz
016000141024     d $EoF            s               n   inz
016100141024     d $Fine           s               n   inz
016200141024     d $InzS01         s               n   inz(*on)
016300141031     d $InzS01_B       s               n   inz
016400141031     d $UpdS01         s               n   inz
016500141024
016600141024       // -?Variabili per la gestione del video?
016700141024     d $Video          s              2    inz('S1')
016800141027     d wPag            s              4  0 inz
016900141027     d wRig            s              3  0 inz
017000141024     d S01nrr          s                   like(C1RcdNbr)  inz
017100141027     d wNrr1Free       s                   like(C1RcdNbr)  inz
017200141027     d wNrr9Free       s                   like(C1RcdNbr)  inz
017300141027     d wNrr1FreePag    s                   like(C1RcdNbr)  inz
017400141028     d SavS01nrr       s                   like(C1RcdNbr)  inz
017500141031     d w_MaxRec        s                   like(CMNpno)    inz(*hival)
017600141030     d wSaveCMNpru     s                   like(CMNpru)    inz
017700141030     d wSaveCMNdim     s                   like(CMNdim)    inz
017800141030     d wSaveCMNhim     s                   like(CMNhim)    inz
017900141024
018000141024       // -?Campi di comodo?
018100141202     d wCMNpno         s                   like(CMNpno)    inz
018200141028     d wCountNote      s                   like(CMNpno)    inz
018300141024
018400141024       //--------------------------------------------------------------
018500141024       //?Definizione prototipi procedure usate.                       ?
018600141024       //--------------------------------------------------------------
018700141024
018800141024       // -?Reperimento dati utente?
018900141024     d TIBS34ds      e ds                  inz
019000141024      /copy gaitrasrc/srcProtoPR,TIBS34R
019100141024
019200141024       // -?Controllo formale / Inversione data?
019300141024     d WLBdat          ds                  inz
019400141024     d   G02dat                1      8  0 inz
019500141024     d   G02inv                9     16  0 inz
019600141024     d   G02err               17     17    inz('3')
019700141024     d   G02tgi               18     22  0 inz
019800141024      /copy gaitrasrc/srcProtoPR,XSRDA8
019900141024
020000141024       // -?"Bonifica" stringa da caratteri indesiderati?
020100141024      /copy gaitrasrc/srcProtoPI,XCHKCHAR
020200141024      /copy gaitrasrc/srcProtoPR,XCHKCHAR
020300141024
020400141024       // -?Controllo/Decodifica cliente?
020500141024      /copy gaitrasrc/srcProtoPI,TIBS69R
020600141024      /copy gaitrasrc/srcProtoPR,TIBS69R
020700141118
020800141118       // -?Ricerca/Controllo tabelle (TNTBE)?
020900141118     d TIBS02ds      e ds                  inz
021000141118     d   T02mod      e                     inz('C')
021100141118     d   T02cod      e                     inz('FCM')
021200141118      /copy gaitrasrc/srcProtoPR,TIBS02R
021300141024
021400141024      *// // -?API QsnQryModSup - Query Display Mode Support?
021500141024      *///copy gaitrasrc/srcProtoPR,TIBS34R
021600141024
021700141024       //--------------------------------------------------------------
021800141024       //?Definizione key-list.                                        ?
021900141024       //--------------------------------------------------------------
022000141028
022100141028       // -?File TNCPO01L?
022200141028     d keyTNCPO01    e ds                  extname(TNCPO01L : *key)
022300141028     d                                     prefix(k_)   inz
022400141028
022500141028       // -?File TICMP01L?
022600141028     d keyTICMP01    e ds                  extname(TICMP01L : *key)
022700141028     d                                     prefix(k_)   inz
022800141024
022900141024       // -?File TICMN01L?
023000141024     d keyTICMN01    e ds                  extname(TICMN01L : *key)
023100141024     d                                     prefix(k_)   inz
023200141024
023300141024       //--------------------------------------------------------------
023400141024       //?Riepilogo indicatori utilizzati.                             ?
023500141024       //--------------------------------------------------------------
023600141024       //--------------------------------------------------------------
023700141024
023800141024       //--------------------------------------------------------------
023900141024       //?M A I N - L I N E                                            ?
024000141024       //--------------------------------------------------------------
024100141024
024200141024     c     *Entry        plist
024300141024     c                   parm                    KPJBA
024400150112     c                   parm                    TRKC10ds
024500141024
024600141024      /free
024700141024
024800141024       // -?Operazioni iniziali?
024900141104       exsr  sr_RoutInz;
025000141024
025100141024       Select;
025200141103
025300141103         // -?Già rilevato un Errore: Uscita?
025400141103         when  $Fine = *on;
025500141024
025600141024         // -?Richiesta CANCELLAZIONE note (senza video)?
025700150112         when  iKC10flm = 'D';
025800141024           exsr  sr_Delete;
025900141024
026000141024         // -?Richiesto RECUPERO note      (senza video)?
026100150112         when  iKC10flm = 'R';
026200141024           exsr  sr_Recupero;
026300141024
026400141024         // -?Richiesta CONFERMA note      (senza video)?
026500141103         //  ?precedentemente immesse nel SFL?
026600150112         when  iKC10flm = 'C';
026700141024           exsr  sr_WriteTICMN;
026800141024
026900141024         // -?Ciclo di gestione del file video?
027000141024         other;
027100141024           DoW  $Fine = *off;
027200141024
027300141024             select;
027400141024               // -?Gestione videata S1 (subfile)?
027500141104               when  $Video = 'S1';
027600141104                 exsr  sr_GesS01;
027700141024               // -?? ? ??
027800141024               other;
027900141024                 $Fine = *on;
028000141024             endsl;
028100141024
028200141024           EndDo;
028300141024
028400141024       EndSl;
028500141024
028600141024       // -?Operazioni finali?
028700141104       exsr  sr_RoutEnd;
028800141024
028900141024       //--------------------------------------------------------------
029000141024       //?Operazioni iniziali.                                         ?
029100141024       //--------------------------------------------------------------
029200141024       BEGSR  sr_RoutInz;
029300141024
029400141024         // -?Impostazione chiusura?
029500150112         //  ?TIPO LANCIO IKC10TLA = "C" -> Solo Chiusura in LR?
029600141024         //                         ?"L" -> Elaboraz.+Chiusura in LR?
029700141024         //                         ?" " -> Elaboraz.+Chiusura in RT?
029800150112         *inLR = ( iKC10tla = 'C'  or  iKC10tla = 'L' );
029900141024
030000141031         // -?Pulizia parametri di solo Output?
030100150112         clear  oKC10piu;
030200150112         clear  oKC10fxx;
030300150112         clear  oKC10err;
030400150112         clear  oKC10msg;
030500141031
030600141031         // -?Verifica SE richiesta la sola chiusura del *pgm?
030700150112         $Fine = ( iKC10tla = 'C' );
030800141031         if  $Fine;
030900141103           leavesr;
031000141031         endif;
031100141024
031200141031         // -?Impostazione Commit?
031300141028         if  Not %open(TICMN01L);
031400150112           $FlgCmt = ( iKC10cmt = *on );
031500141028           open  TICMN01L;
031600141024         endif;
031700141024
031800141024         // -?Reperimento dati job?
031900141024         exsr  sr_DatiJob;
032000150123
032100150123         // -?Pulizia campi di comodo?
032200150123         clear  wCountNote;
032300141024
032400141024         // -?NON richiesta la gestione del video?
032500150112         if  iKC10flm = 'C'  or
032600150112             iKC10flm = 'D'  or
032700150112             iKC10flm = 'R';
032800141024           leavesr;
032900141024         endif;
033000141024
033100141031         // -?Verifica SE richiesta l'inizializzazione del video?
033200141031         //  ?(NON se 2° richiamo per la stessa Campagna)?
033300150112         if  iKC10tla <> SaveTRKC10ds.iKC10tla  or
033400150112             iKC10flm <> SaveTRKC10ds.iKC10flm  or
033500150112             iKC10ncm <> SaveTRKC10ds.iKC10ncm  or
033600150112             iKC10ksu <> SaveTRKC10ds.iKC10ksu  or
033700150112             iKC10ksc <> SaveTRKC10ds.iKC10ksc  or
033800150112             iKC10cpo <> SaveTRKC10ds.iKC10cpo  or
033900150112             iKC10acm <> SaveTRKC10ds.iKC10acm;
034000141031           $InzS01 = *on;
034100141024         endif;
034200141031         // -?Verifica SE ricevute Note da proporre a video?
034300150112         //  ?(NON qui per IKC10FLM = "C" e Note <> *blank)?
034400150112         if  iKC10flm =  'M';
034500141031           $InzS01_B = *on;
034600141031         endif;
034700141031
034800150112         SaveTRKC10ds = TRKC10ds;
034900141024
035000141024         // -?Impostazione nome programma a video?
035100141024         VTCpgm = SDSpgm;
035200141024
035300141024         $Video = 'S1';
035400141024         clear  VTCmod;
035500141024         select;
035600150112           when  iKC10flm = *blank  or  iKC10flm = 'M';
035700141024             VTCmod = '  IMMISSIONE  ';
035800150112           when  iKC10flm = 'I';
035900141024             VTCmod = 'INTERROGAZIONE';
036000141024         endsl;
036100141024
036200141024         // -?(Dis)Abilitazione tasti funzionali?
036300141024         F3Attivo  = *off;
036400150112         F6Attivo  = ( iKC10flm =  *blank );
036500150112         F10Attivo = ( iKC10flm =  'I'  and  iKC10f10 = 'S' );
036600150112         F12Attivo = ( iKC10flm <> 'M' );
036700141030
036800150112         //DescrINVIO = ( iKC10flm = 'M' );   //?(già condizionato da "Not F12Attivo")?
036900141024
037000141024         // -?Impostazione parzializzazioni richieste?
037100150112         clear  KC10C01;
037200150112         C01ncm   = iKC10ncm;
037300150112         C01des   = iKC10des;
037400150112         C01ksu   = iKC10ksu;
037500150112         C01ksc   = iKC10ksc;
037600150112         C01cpo   = iKC10cpo;
037700141031         select;
037800150112           when  iKC10cpo > *zero;
037900150112             C01cpoD  = iKC10rsc;
038000150112           when  iKC10ksc > *zero;
038100150112             C01kscD  = iKC10rsc;
038200150112           when  iKC10ksu > *zero;
038300150112             C01ksuD  = iKC10rsc;
038400141031         endsl;
038500141024
038600141024         // -?Decodifica campagna - se non ricevuta?
038700150112         If  iKC10des = *blank;
038800150112           if  iKC10ncm <> k_CMPncm;
038900141031             clear  keyTICMP01;
039000150112             k_CMPncm = iKC10ncm;
039100141031             chain  %kds( keyTICMP01 )  TICMP000;
039200141031             if  Not %found(TICMP01L);
039300141031               CMPdes = *all'? ';
039400141031             endif;
039500141024           endif;
039600141031           C01des = CMPdes;
039700141024         EndIf;
039800141024
039900141024         // -?Decodifica cliente  - se non ricevuta?
040000150112         If  iKC10rsc =  *blank;
040100141024
040200141024           Select;
040300141024
040400150112             When  iKC10ksu <> *zero  or  iKC10ksc <> *zero;
040500150112               if  ( iKC10ksu <> *zero  and  iKC10ksu <> I69kac )  or
040600150112                   ( iKC10ksc <> *zero  and  iKC10ksc <> I69kac );
040700141031                 clear  TIBS69ds;
040800141031                 I69sif = knsif;
040900141031                 I69kcc = DUTkci;
041000150112                 if  iKC10ksu <> *zero;
041100150112                   I69kac = iKC10ksu;
041200141031                 else;
041300150112                   I69kac = iKC10ksc;
041400141031                 endif;
041500141031                 tibs69r( tibs69ds :
041600141031                          ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
041700141031                 if  O69err = *on;
041800141031                   ACOrag = *all'? ';
041900141031                 endif;
042000141031               endif;
042100150112               if  iKC10ksu <> *zero;
042200141031                 C01ksuD = ACOrag;
042300141031               else;
042400141031                 C01kscD = ACOrag;
042500141031               endif;
042600141024
042700150112             When  iKC10cpo <> *zero;
042800150112               if  iKC10cpo <> k_CPOcpo;
042900141031                 if  not %open(TNCPO01L);
043000141031                   open  TNCPO01L;
043100141031                 endif;
043200141031                 clear  keyTNCPO01;
043300150112                 k_CPOcpo = iKC10cpo;
043400141031                 chain  %kds( keyTNCPO01 )  TNCPO000;
043500141031                 if  Not %found(TNCPO01L);
043600141031                   CPOrag = *all'? ';
043700141031                 endif;
043800141024               endif;
043900141031               C01cpoD = CPOrag;
044000141024
044100141024           EndSl;
044200141024
044300141024         EndIf;
044400141024
044500141024         // -?Impostazione attributi di visualizzazione in testata?
044600141031         DisplayKSU = *off;
044700141031         DisplayKSC = *off;
044800141031         DisplayCPO = *off;
044900141031         select;
045000150112           when  iKC10cpo <> *zero;
045100141031             DisplayCPO = *on;
045200150112           when  iKC10ksc <> *zero;
045300141031             DisplayKSC = *on;
045400150112           when  iKC10ksu <> *zero;
045500141031             DisplayKSU = *on;
045600141031         endsl;
045700141024
045800141024       ENDSR;
045900141024
046000141024       //--------------------------------------------------------------
046100141024       //?Reperimento Dati del job (Utente/Operativi).                 ?
046200141024       //--------------------------------------------------------------
046300141024       BEGSR  sr_DatiJob;
046400141024
046500141024         in(e) §AzUte;
046600141024         if NOT %error;
046700141024           in(e) §DatiUte;
046800141024         endif;
046900141024         if %error or RSut = *blank;
047000141024           tibs34r ( tibs34ds );
047100141024           in §AzUte;
047200141024           in §DatiUte;
047300141024         endif;
047400141024
047500141024       ENDSR;
047600141024
047700141024       //--------------------------------------------------------------
047800141027       //?Gestione SubFile S01.                                        ?
047900141024       //--------------------------------------------------------------
048000141024       BEGSR  sr_GesS01;
048100141024
048200141024         // -?Inizializzazione videata?
048300141024         if  $InzS01 = *on;
048400141024           exsr  sr_InzS01;
048500141024           $InzS01 = *off;
048600141024         endif;
048700141027
048800141027         // -?Impostazione Note eventualmente ricevute?
048900141031         if  $InzS01_B = *on;
049000141031           $UpdS01   = *on;
049100141027           exsr  sr_UpdS01;
049200141031           $InzS01_B = *off;
049300141027         endif;
049400141024
049500141024         // -?Emissione Testata & Piede con tasti funzionali abilitati?
049600150112         write  KC10T01;
049700150112         write  KC10P01;
049800141027
049900141024         // -?Posizionamento cursore?
050000141027         //  ?C1CsrRrn contiene il numero di riga del SubFile su cui?
050100141024         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
050200141024         //  ?si visualizza la pagina che vedeva l'utente quando ha?
050300141024         //  ?premuto l'ultimo tasto.?
050400141024         if  C1CsrRrn > *zero;
050500141024           C1RcdNbr = C1CsrRrn;
050600141024         else;
050700141024           C1RcdNbr = 1;
050800150112           write  KC10S00;          //?(no rec. - SOLO visualizzazione)?
050900141024         endif;
051000141024
051100141024         // -?Emissione videata?
051200150112         exfmt  KC10C01;
051300141024
051400141024         clear  V1Dmsg;
051500141024         reset  ErrMessage;
051600141024         reset  ErrGenerico;
051700141024
051800141024         SELECT;
051900141024
052000141024           // -?F3=Fine?
052100141024           WHEN  dsp_aid = c_F03;
052200150112             oKC10fxx = '03';
052300141031             $InzS01  = *on;
052400141031             $Fine    = *on;
052500141031
052600141031           // -?F10=Inserimento (da visualizzazione)?
052700141031           WHEN  dsp_aid = c_F10;
052800141031             $InzS01   = *on;
052900141031             F10Attivo = *off;
053000141031             F6Attivo  = *on;
053100141024
053200141024           // -?F12=Ritorno?
053300141024           WHEN  dsp_aid = c_F12;
053400150112             oKC10fxx = '12';
053500141031             $InzS01  = *on;
053600141031             $Fine    = *on;
053700141027
053800141027           // -?RollUp=Caricamento righe vuote di I/O?
053900141027           WHEN  dsp_aid = c_RollUp;
054000141027             exsr  sr_RollUpS01;
054100141024
054200141027           // -?Invio o F6=Conferma?
054300141024           OTHER;
054400141103             // -?Aggiornamento posizionamento nel subfile?
054500141031             Select;
054600141030               // -?Invio=Prosegui?
054700141031               //when  DescrINVIO     and  dsp_aid = c_Enter;
054800141031               When  Not F12Attivo  and  dsp_aid = c_Enter;
054900150112                 exsr  sr_SetEKC10___1;
055000141031                 if  NOT ErrGenerico;
055100150112                   //clear  oKC10fxx;
055200141031                   $Fine = *on;
055300141031                 endif;
055400141030               // -?F6=Conferma?
055500141031               When  dsp_aid = c_F06;
055600141030                 exsr  sr_WriteTICMN;
055700141030                 if  NOT ErrGenerico;
055800150112                   oKC10fxx = '06';
055900141030                   $Fine = *on;
056000141030                 endif;
056100141031               // -?Invio?
056200141031               //  ?(Posizionamento cursore sul 1° rec. vuoto)?
056300141031               Other;
056400141031                 $UpdS01 = *off;
056500141031                 exsr  sr_UpdS01;
056600141031             EndSl;
056700141027             // -?Errore?
056800141027             if  ErrGenerico;
056900141027               leavesr;
057000141027             endif;
057100141024
057200141024         ENDSL;
057300141024
057400141024       ENDSR;
057500141024
057600141024       //--------------------------------------------------------------
057700141027       //?Inizializzazione SubFile S01.                                ?
057800141024       //--------------------------------------------------------------
057900141024       BEGSR  sr_InzS01;
058000141024
058100141024         // -?Spegnimento degli indicatori di errore?
058200141024         %subst(IndDspF : 50) = *off;
058300141024
058400141103         // -?Pulizia subfile?
058500141024         SflDsp_N    = *on;
058600141024         SflDspCtl_N = *on;
058700150112         write  KC10C01;
058800141024         SflDspCtl_N = *off;
058900141024         SflEnd      = *off;
059000141024
059100141024         clear  C1RcdNbr;
059200141024         clear  C1CsrRrn;
059300141024         clear  S01nrr;
059400141024         clear  V1Dmsg;
059500141024         ErrMessage  = *off;
059600141024         ErrGenerico = *off;
059700141202         clear  wCMNpno;
059800141027         clear  wNrr1Free;
059900141027         clear  wNrr9Free;
060000141030         //clear  wCountNote;
060100141031         clear  wSaveCMNpru;
060200141031         clear  wSaveCMNdim;
060300141031         clear  wSaveCMNhim;
060400141024
060500141103         // -?Caricamento - completo - delle note nel subfile?
060600141024         clear  keyTICMN01;
060700150112         k_CMNncm = iKC10ncm;
060800150112         k_CMNksu = iKC10ksu;
060900150112         k_CMNksc = iKC10ksc;
061000150112         k_CMNcpo = iKC10cpo;
061100141118         setll  %kds( keyTICMN01 : 4 )  TICMN000;
061200141118         reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
061300141024
061400141024         DoW  Not  %eof(TICMN01L);
061500141024
061600141204           // -?Emissione messaggio di segnalazione raggiungimento limite?
061700141204           if  S01nrr >= c_MaxRec;
061800141204             ErrGenerico = *on;
061900141204             ErrMessage  = *on;
062000141204             V1Dmsg = sk_Msg(01);
062100141204             leave;
062200141204           endif;
062300141024
062400141203           if  CMNacm <> *blank  or
062500141203               kNmUs  =  CMNpru  or
062600141203               DUTlpo =  'S';
062700141202             exsr  sr_RieRecS01;
062800141202           endif;
062900141024
063000141118           reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
063100141024
063200141024         EndDo;
063300141024
063400141103         // -?Caricamento record vuoti - almeno 2 - nel subfile?
063500141024         //  ?(SE NON interrogazione  e?
063600141103         //  ? SE NON raggiunto il n° Max di rec caricabili nel SFL)?
063700150112         IF  ( iKC10flm <> 'I'  or
063800150112             ( iKC10flm =  'I'  and  iKC10f10 = 'S'
063900141202                                and  dsp_aid  = c_F10 ) )  and
064000141204             S01nrr < c_MaxRec;
064100141024
064200141204           //// -?Azzeramento nuovo Progressivo Note?
064300141204           //clear  wCMNpno;  ?(già fatto)?
064400141027
064500141103           // -?Memorizzazione 1° Nrr libero nel subfile?
064600141027           wNrr1Free = S01nrr + 1;
064700141024
064800141103           // -?Caricamento record vuoti - almeno 2 - nel subfile?
064900141024           //  ?(fino al riempimento della pagina)?
065000141027           exsr  sr_RollUpS01;
065100141024
065200141027         ENDIF;
065300141103
065400141204         // -?Raggiunto il n° Max di rec. gestibili nel subfile (9.999)?
065500141204         //  ?"solo" per visualizzare le note già inserite:?
065600141103         // ·?DISattivazione F6=Conferma?
065700141103         if  F6Attivo  and  wNrr1Free = *zero;
065800141103           F6Attivo = *off;
065900141103         endif;
066000141103         // ·?DISattivazione F10=Inserimento?
066100141204         if  F10Attivo  and  S01nrr >= c_MaxRec;
066200141103           F10Attivo = *off;
066300141103         endif;
066400141103
066500141103         // -?Emissione messaggio di segnalazione raggiungimento limite?
066600141204         //  ?- già visualizzati 9.999 rec. nel subfile o 999 rec. I/O -?
066700141204         //  ?N.B. - CMNPRG è 3/0?
066800150112         if  ( iKC10flm  <> 'I'  or  ( iKC10flm = 'I'      and
066900150112                                       iKC10f10 = 'S' ) )  AND
067000141204             ( S01nrr    >= c_MaxRec  or
067100141204               wNrr9Free >= wNrr1Free + w_MaxRec );
067200141103           ErrGenerico = *on;
067300141103           ErrMessage  = *on;
067400141103           V1Dmsg = sk_Msg(01);
067500141103         endif;
067600141024
067700141024         // -?Visualizzazione del SFL (se ci sono dati)?
067800141024         SflDsp_N  = (S01nrr <= *zero);
067900141024
068000141024         // -?Attivazione del SFLEND?
068100150112         SflEnd = ( ( %eof(TICMN01L)  and  iKC10flm = 'I'
068200141031                                      and  NOT F10Attivo  )  or
068300141204                    S01nrr    >= c_MaxRec                    or
068400141204                    wNrr9Free >= wNrr1Free + w_MaxRec );
068500141024
068600141103         // -?Posizionamento subfile?
068700141027         If  S01nrr > *zero;
068800141027
068900141027           select;
069000141027             // -?Se visualizzazione: nel 1° rec.?
069100150112             when    iKC10flm = 'I'    and
069200150112                   ( iKC10f10 = *blank  or  F10Attivo );
069300141027               C1RcdNbr = 1;
069400141027             // -?Se in pagina nuova / tutta libera (RollUp): nel 1° rec.?
069500141027             //  ?della pagina (tutti liberi)?
069600141027             when  wNrr1FreePag > wNrr1Free;
069700141027               C1RcdNbr = wNrr1FreePag;
069800141027             // -?Altrimenti: nel 1° rec. libero della pagina?
069900141027             other;
070000141027               C1RcdNbr = wNrr1Free;
070100141027           endsl;
070200141027
070300141024           C1CsrRrn  = C1RcdNbr;
070400141027
070500141027         Else;
070600141027
070700141024           clear  C1RcdNbr;
070800141027
070900141027         EndIf;
071000141024
071100141024       ENDSR;
071200141024
071300141024       //--------------------------------------------------------------
071400141027       //?Caricamento singolo record del SubFile S01.                  ?
071500141024       //--------------------------------------------------------------
071600141024       BEGSR  sr_RieRecS01;
071700141024
071800150112         clear  KC10S01;
071900141024
072000141103         // -?Impostazione campi nel record del subfile?
072100141024         // ·?Campi Hidden?
072200141203         HS1acm  = CMNacm;               // ·?Fase campagna?
072300141203         HS1pno  = CMNpno;               // ·?Progressivo Nota?
072400141203         HS1dim  = CMNdim;               // ·?Data inserimento?
072500141203         HS1him  = CMNhim;               // ·?Ora  inserimento?
072600141203         HS1fim  = CMNfim;               // ·?Fil. inserimento?
072700141203         HS1flo  = CMNflo;               // ·?Flag Operativi?
072800141024         // ·?Campi di Input/Output?
072900141203         S01not  = CMNnot;               // ·?Note?
073000141024         // ·?Campi di Output?
073100141202         if  CMNpru <> wSaveCMNpru  or
073200141030             CMNdim <> wSaveCMNdim  or
073300141030             CMNhim <> wSaveCMNhim;
073400141202           if  CMNacm <> *blank;
073500141202             exsr  sr_DecodFaseCamp;
073600141202             S01fcm = dFCM.§FCMdes;
073700141202           else;
073800141202             S01fcm = 'RISERVATA';
073900141202           endif;
074000141030           S01pru = CMNpru;
074100141030           reset  WLBdat;
074200141030           G02inv = CMNdim;
074300141030           xsrda8 ( WLBdat );
074400141030           if  G02err = *off;
074500141030             S01dim = G02dat;
074600141030           endif;
074700141030           S01him = ( CMNhim / 100 );
074800141118           wSaveCMNpru = CMNpru;
074900141118           wSaveCMNdim = CMNdim;
075000141118           wSaveCMNhim = CMNhim;
075100141030         endif;
075200141024
075300141024         // -?Impostazione attributi di visualizzazione?
075400141203         // ·?Evidenziazione Fase (se attuale)?
075500150112         EvidenzFCM = ( HS1acm <> *blank  and  HS1acm = iKC10acm );
075600141203         // ·?Evidenziazione Fase/Note (se Riservate)?
075700141203         EvidenzRIS = ( HS1acm = *blank );
075800141024         // ·?Protezione delle Note già inserite?
075900141024         ProtectNOT = *on;
076000141027         HS1in49    = ProtectNOT;
076100141024
076200141103         // -?Registrazione singolo record nel subfile?
076300141024         S01nrr += 1;
076400150112         write  KC10S01;
076500141024
076600141024       ENDSR;
076700141118
076800141118       //--------------------------------------------------------------
076900141118       //?Decodifica Fase della Campagna Commerciale.                  ?
077000141118       //--------------------------------------------------------------
077100141118       BEGSR  sr_DecodFaseCamp;
077200141118
077300141118         clear  dFCM;
077400141118         reset  TIBS02ds;
077500141118         //T02mod = 'C';        ?(già così)?
077600141118         //T02cod = 'FCM';      ?(già così)?
077700141118         T02ke1 = CMNacm;
077800141118         T02sif = KNSIF;
077900141118
078000141118         TNTBE_RicercaControllo ( kpjba : tibs02ds );
078100141118
078200141118         if  T02err = *blank;
078300141118           dFCM = T02uni;
078400141118         endif;
078500141118
078600141118       ENDSR;
078700141027
078800141027       //--------------------------------------------------------------
078900141028       //?Gestione Roll-Up nel SubFile S01.                            ?
079000141027       //--------------------------------------------------------------
079100141027       BEGSR  sr_RollUpS01;
079200141027
079300141027         clear  wNrr1FreePag;
079400150112         clear  KC10S01;
079500141027         SflNxtChg = *off;
079600141027
079700141028         wPag = ( S01nrr / c_SflPag ) + 1;
079800141028
079900141204         DoW  S01nrr < ( wPag * c_SflPag )  and
080000141204              S01nrr < c_MaxRec             and
080100141204              S01nrr < wNrr1Free + w_MaxRec - 1;
080200141027
080300141118           // -?Impostazione campi ricevuti nel record del subfile?
080400141027           // ·?Campi Hidden?
080500150112           HS1acm  = iKC10acm;           // ·?Fase campagna?
080600150112           HS1dim  = eKC10dim;           // ·?Data inserimento?
080700150112           HS1him  = eKC10him;           // ·?Ora  inserimento?
080800150112           HS1fim  = eKC10fim;           // ·?Fil. inserimento?
080900150112           HS1flo  = eKC10flo;           // ·?Flag operativi?
081000141027           // ·?Campi di Input/Output?
081100141120           //clear  S01not;              // ·?Note (VUOTE)?
081200141027
081300141027           // -?Impostazione attributi di visualizzazione?
081400141203           // ·?Evidenziazione Fase (se attuale) - NON in inserimento?
081500141202           EvidenzFCM = *off;
081600141203           // ·?Evidenziazione Fase/Note (se Riservata)?
081700141203           EvidenzRIS = ( HS1acm = *blank );
081800141117           // ·?Protezione delle Note già inserite?
081900141027           ProtectNOT = *off;
082000141027           HS1in49    = ProtectNOT;
082100141027
082200141103           // -?Posizionamento cursore (sul 1° record libero del subfile)?
082300141027           PosCurNOT  = ( wNrr1Free = S01nrr + 1   or
082400141027                        ( S01nrr    > wNrr1Free   and
082500141027                          %rem( S01nrr : c_SflPag ) = *zero  ) );
082600141027
082700141103           // -?Registrazione singolo record VUOTO nel subfile?
082800141027           S01nrr += 1;
082900150112           write  KC10S01;
083000141027
083100141027           if  wNrr1FreePag = *zero;
083200141027             wNrr1FreePag = S01nrr;
083300141027           endif;
083400141027
083500141027         EndDo;
083600141027
083700141103         // -?Salvataggio ultimo Nrr registrato nel subfile?
083800141027         wNrr9Free = S01nrr;
083900141027
084000141027         // -?Visualizzazione del SFL (rec. - anche vuoti - ce ne sono)?
084100141027         SflDsp_N  = *off;
084200141027
084300141027         // -?Attivazione del SFLEND?
084400141204         SflEnd = ( S01nrr    >= c_MaxRec  or
084500141204                    wNrr9Free >= wNrr1Free + w_MaxRec );
084600141027
084700141103         // -?Posizionamento subfile?
084800141027         Select;
084900141027           // -?Se visualizzazione: nel 1° rec. della pagina?
085000150112           When  iKC10flm = 'I';
085100141027             wPag     = S01nrr / c_SflPag;
085200141027             wRig     = S01nrr - (c_SflPag * wPag);
085300141027             C1RcdNbr = wPag * c_SflPag;
085400141027             if  wRig > *zero;
085500141027               C1RcdNbr += 1;
085600141027             else;
085700141027               C1RcdNbr = C1RcdNbr - c_SflPag + 1;
085800141027             endif;
085900141027           // -?Se in pagina nuova / tutta libera (RollUp): nel 1° rec.?
086000141027           //  ?della pagina (tutti liberi)?
086100141027           When  wNrr1FreePag > wNrr1Free;
086200141027             C1RcdNbr = wNrr1FreePag;
086300141028           // -?Altrimenti: nel 1° rec. libero della pagina (ultima,?
086400141028           //  ?libera in parte)?
086500141027           Other;
086600141027             C1RcdNbr = wNrr1Free;
086700141027         EndSl;
086800141027
086900141027         C1CsrRrn  = C1RcdNbr;
087000141027
087100141027       ENDSR;
087200141028
087300141028       //--------------------------------------------------------------
087400141028       //?Controllo dati in S01.                                     ?
087500141028       //--------------------------------------------------------------
087600141030       BEGSR  sr_CtrS01;
087700141028
087800141028         // -?Spegnimento degli indicatori di errore?
087900141028         %subst(IndDspF : 50) = *off;
088000141028
088100141028       ENDSR;
088200141024
088300141024       //--------------------------------------------------------------
088400141028       //?Scrittura nuove Note in TICMN00F.                          ?
088500141024       //--------------------------------------------------------------
088600141030       BEGSR  sr_WriteTICMN;
088700141024
088800141031         clear  TICMN000;
088900141030         wTime_ds = %editc( %dec( %timestamp() ) : 'X' );
089000141028
089100141202         SELECT;
089200141202
089300141202           // -?SE il subfile è caricato: si scrive in base al contenuto?
089400141202           //  ?del subfile?
089500141202           WHEN  wNrr1Free > *zero;
089600141202
089700141202             // ·?Richiamato con "C"=Conferma dati precedentemente?
089800141202             //  ?immessi: prima si aggiornano le prime 2 righe libere?
089900141202             //  ?nel SFL?
090000150112             if  iKC10flm = 'C'  and  iKC10fno = 'S';
090100141202               $UpdS01 = *on;
090200141202               exsr  sr_UpdS01;
090300141202             endif;
090400141202
090500141202             // ·?Comunque si scrivono le note del subfile?
090600150112             clear  eKC10no1;
090700150112             clear  eKC10no2;
090800150121             clear  oKC10piu;
090900141202             clear  wCountNote;
091000141202             For  S01nrr = wNrr1Free  To  wNrr9Free;
091100141202
091200150112               chain  S01nrr  KC10S01;
091300141202
091400150112               if  %found(TRKC10D)  and  S01not <> *blank;
091500141202                 exsr  sr_WrtRecTICMN;
091600150112                 exsr  sr_SetEKC10___2;
091700150112                 exsr  sr_SetEKC10___3;
091800141202               endif;
091900141202
092000141202             EndFor;
092100141202
092200141202           // -?Se il subfile NON è caricato: si scrive in base ai parametri?
092300141202           //  ?ricevuti (...SE ricevute note)?
092400150112           WHEN  eKC10no1 <> *blank  or  eKC10no2 <> *blank;
092500141103
092600141202             // ·?Ricerca del 1° progressivo libero (DOVREBBE essere 1...)?
092700141203             clear  wCMNpno;        //?...Meglio farlo SEMPRE!?
092800141203             //If  wCMNpno = *zero;
092900141103
093000141103               clear  keyTICMN01;
093100150112               k_CMNncm = iKC10ncm;
093200150112               k_CMNksu = iKC10ksu;
093300150112               k_CMNksc = iKC10ksc;
093400150112               k_CMNcpo = iKC10cpo;
093500141202               k_CMNdim = CMNdim;
093600141202               k_CMNhim = CMNhim;
093700141204               k_CMNpno = w_MaxRec;
093800141103               setGT  %kds( keyTICMN01 )  TICMN000;
093900141202               readPe(N)  %kds( keyTICMN01 : 6 )  TICMN000;
094000141103               if  Not  %eof(TICMN01L);
094100141202                 wCMNpno = CMNpno;
094200141103               endif;
094300141103
094400141204               if  wCMNpno >= w_MaxRec;  //?*ERR?
094500141103                 ErrGenerico = *on;
094600141103                 $Fine    = *on;
094700150112                 oKC10err = 'S';
094800150112                 oKC10msg = sk_Msg(01);
094900141103                 leavesr;
095000141103               endif;
095100141103
095200141203             //EndIf;
095300141028
095400141202             // ·?Impostazione dei campi nel subfile?
095500150112             C01ncm  = iKC10ncm;         // ·?Num. Campagna Comm.le?
095600150112             C01ksu  = iKC10ksu;         // ·?Cliente Unitario?
095700150112             C01ksc  = iKC10ksc;         // ·?Cod. Cliente?
095800150112             C01cpo  = iKC10cpo;         // ·?Cod. Potenziale?
095900150112             HS1acm  = iKC10acm;         // ·?Fase campagna?
096000150112             HS1dim  = eKC10dim;         // ·?Data inserimento?
096100150112             HS1him  = eKC10him;         // ·?Ora  inserimento?
096200150112             HS1fim  = eKC10fim;         // ·?Fil. inserimento?
096300150112             HS1flo  = eKC10flo;         // ·?Flag operativi?
096400141028
096500141202             // ·?Scrittura delle note ricevute?
096600141028             For  wRig = 1  To  2;
096700141028
096800141028               clear  S01not;
096900141028               select;
097000150112                 when  wRig = 1  and  eKC10no1 <> *blank;
097100150112                   S01not = eKC10no1;
097200150112                 when  wRig = 2  and  eKC10no2 <> *blank;
097300150112                   S01not = eKC10no2;
097400141028               endsl;
097500141028
097600141202               // ·?Scrittura singola nota del subfile?
097700141028               if  S01not <> *blank;
097800141028                 exsr  sr_WrtRecTICMN;
097900150112                 exsr  sr_SetEKC10___2;
098000150112                 exsr  sr_SetEKC10___3;
098100141028               endif;
098200141028
098300141028             EndFor;
098400141028
098500141202         ENDSL;
098600141028
098700141031         $InzS01   = *on;
098800141031         $InzS01_B = *on;
098900150112         clear  SaveTRKC10ds;
099000141028
099100141028       ENDSR;
099200141031
099300141031       //--------------------------------------------------------------
099400141031       //?Aggiornamento SubFile S01.                                   ?
099500141031       //--------------------------------------------------------------
099600141031       BEGSR  sr_UpdS01;
099700141031
099800141117         // -?SubFile SENZA rec. I/O (impossibile, ma...)?
099900141031         if  wNrr1Free <= *zero;
100000141031           leavesr;
100100141031         endif;
100200141031
100300141031         clear  wCountNote;
100400141031         SavS01nrr = S01nrr;
100500141031
100600141031         FOR  S01nrr = wNrr1Free  to  wNrr9Free;
100700141031
100800150112           chain  S01nrr  KC10S01;
100900141031
101000150112           If  %found(TRKC10D)  and  HS1in49 = *off;
101100141204
101200141204             if  wCountNote >= w_MaxRec;      //?*ERR?
101300141204               ErrGenerico = *on;
101400141204               ErrMessage  = *on;
101500141204               V1Dmsg = sk_Msg(01);
101600141204               leave;
101700141204             endif;
101800141031
101900141031             wCountNote += 1;
102000141031
102100141204             if  $UpdS01  and  wCountNote <= 2;
102200141031               select;
102300141031                 when  wCountNote = 1;
102400150112                   S01not = eKC10no1;
102500141031                 when  wCountNote = 2;
102600150112                   S01not = eKC10no2;
102700141031               endsl;
102800141031             endif;
102900141031
103000141031             // -?Impostazione attributi di visualizzazione?
103100141203             // ·?Evidenziazione Fase (se attuale) - NON in inserimento?
103200141118             EvidenzFCM = *off;
103300141203             // ·?Evidenziazione Fase/Note (se Riservata)?
103400141203             EvidenzRIS = ( HS1acm = *blank );
103500141117             // ·?Protezione delle Note già inserite?
103600141031             ProtectNOT = *off;
103700141031             HS1in49    = ProtectNOT;
103800141031
103900141103             // -?Posizionamento cursore (sul 1° record libero del subfile)?
104000141031             PosCurNOT  = ( S01not = *blank );
104100141031             C1RcdNbr   = S01nrr;
104200141031             C1CsrRrn   = C1RcdNbr;
104300141031
104400141103             // -?Aggiornamento record del subfile?
104500141031             SflNxtChg  = ( S01not <> *blank );
104600150112             update  KC10S01;
104700141031
104800141031             // -?Uscita DOPO aver posizionato il cursore nel 1° rec.?
104900141103             //  ?libero nel subfile?
105000141031             if  wCountNote > 2  and  S01not = *blank;
105100141031               leave;
105200141031             endif;
105300141031
105400141031           EndIf;
105500141031
105600141031         ENDFOR;
105700141031
105800141031         S01nrr = SavS01nrr;
105900141031
106000141031       ENDSR;
106100141031
106200141031       //--------------------------------------------------------------
106300141031       //?Lettura SubFile S01 per Impostazione parametri di Output.  ?
106400141031       //--------------------------------------------------------------
106500150112       BEGSR  sr_SetEKC10___1;
106600141031
106700141031         // -?Aggiornamento parametri Note?
106800150121         clear  eKC10no1;
106900150121         clear  eKC10no2;
107000150121         clear  oKC10piu;
107100141031         clear  wCountNote;
107200141031         SavS01nrr = S01nrr;
107300141031
107400141031         FOR  S01nrr = wNrr1Free  to  wNrr9Free;
107500141031
107600150112           chain  S01nrr  KC10S01;
107700141031
107800150112           If  %found(TRKC10D)  and  HS1in49 = *off;
107900141031
108000141031             // -?Impostazione singolo parametro di Output?
108100141031             if  S01not <> *blank;
108200150112               exsr  sr_SetEKC10___2;
108300141031             endif;
108400141031
108500141031             // -?Impostazione attributi di visualizzazione?
108600141203             // ·?Evidenziazione Fase (se attuale) - NON in inserimento?
108700141118             EvidenzFCM = *off;
108800141203             // ·?Evidenziazione Fase/Note (se Riservata)?
108900141203             EvidenzRIS = ( HS1acm = *blank );
109000141117             // ·?Protezione delle Note già inserite?
109100141031             ProtectNOT = *off;
109200141031             HS1in49    = ProtectNOT;
109300141031
109400141031             // -?Posizionamento cursore (sul 1° record libero)?
109500141031             PosCurNOT  = ( S01not = *blank );
109600141031
109700141103             // -?Aggiornamento record del subfile?
109800141031             SflNxtChg  = ( S01not <> *blank );
109900150112             update  KC10S01;
110000141031
110100141031             // -?Uscita DOPO aver letto le prime 3 note inserite?
110200141031             if  wCountNote > 2;
110300141031               leave;
110400141031             endif;
110500141031
110600141031           EndIf;
110700141031
110800141031         ENDFOR;
110900141031
111000141031         S01nrr = SavS01nrr;
111100141031
111200141031       ENDSR;
111300141030
111400141030       //--------------------------------------------------------------
111500141204       //?Impostazione singola Nota di (Input/)Output.                 ?
111600141030       //--------------------------------------------------------------
111700150112       BEGSR  sr_SetEKC10___2;
111800141030
111900141030         wCountNote += 1;
112000141030
112100141030         select;
112200141030           when  wCountNote = 1;
112300150112             eKC10no1 = S01not;
112400141030           when  wCountNote = 2;
112500150112             eKC10no2 = S01not;
112600141030           when  wCountNote > 2;
112700150112             oKC10piu = '+';
112800141030         endsl;
112900141030
113000141030       ENDSR;
113100141204
113200141204       //--------------------------------------------------------------
113300141204       //?Impostazione altri parametri di (Input/)Output.              ?
113400141204       //--------------------------------------------------------------
113500150112       BEGSR  sr_SetEKC10___3;
113600141204
113700141204         // -?Data inserimento?
113800150112         if  eKC10dim = *zero;
113900150112           eKC10dim = CMNdim;
114000141204         endif;
114100141204         // -?Ora inserimento?
114200150112         if  eKC10him = *zero;
114300150112           eKC10him = CMNhim;
114400141204         endif;
114500141204         // -?Filiale inserimento?
114600150112         if  eKC10fim = *zero;
114700150112           eKC10fim = DUTpou;
114800141204         endif;
114900141204         // -?Profilo Utente inserimento?
115000150112         if  eKC10pru = *blank;
115100150112           eKC10pru = kNmUs;
115200141204         endif;
115300141204
115400141204       ENDSR;
115500141028
115600141028       //--------------------------------------------------------------
115700141028       //?Scrittura singolo record in TICMN00F.                        ?
115800141028       //--------------------------------------------------------------
115900141028       BEGSR  sr_WrtRecTICMN;
116000141028
116100141202         wCMNpno += 1;
116200141024
116300141028         CMNncm = C01ncm;
116400141028         CMNksu = C01ksu;
116500141028         CMNksc = C01ksc;
116600141028         CMNcpo = C01cpo;
116700141028         CMNacm = HS1acm;
116800141202         CMNpno = wCMNpno;
116900141028
117000141028         // -?Data/Ora immissione: aggiorno SEMPRE con le correnti?
117100141030         //  ?(già impostate per mantenerle univoche)?
117200141028         //CMNdim = %int( %subst( %char( %dec( %timestamp() ) )
117300141028         //                       : 1 : 8 ) );
117400141028         //CMNhim = %int( %subst( %char( %dec( %timestamp() ) )
117500141028         //                       : 9 : 6 ) );
117600141028
117700141028         CMNfim = DUTpou;
117800141028         CMNpru = kNmUs;
117900141028
118000141028         //CMNflo = HS1flo;
118100141028
118200141028         CMNnot = S01not;
118300141028         // -?Note: controllo che NON contenga caratteri particolari?
118400141028         exsr sr_ChkChar;
118500141028         //if  CMNnot = *blank;
118600141028         //  leavesr;
118700141028         //endif;
118800141028
118900141028         //_____________
119000141028         write TICMN000;
119100141028         //¯¯¯¯¯¯¯¯¯¯¯¯¯
119200150123
119300150123         // -?Impostazione Progressivo Note in elaborazione?
119400150123         //  ?(serve all subr. "sr_SetEKC10___2", che lo incrementa)?
119500150123         wCountNote = wCMNpno - 1;
119600141028
119700141028       ENDSR;
119800141024
119900141028       //--------------------------------------------------------------
120000141028       //?Recupero Note da restituire al chiamante.                    ?
120100141028       //--------------------------------------------------------------
120200141028       BEGSR  sr_Recupero;
120300141028
120400150112         clear  eKC10no1;
120500150112         clear  eKC10no2;
120600150112         //clear  eKC10dim;
120700150112         //clear  eKC10him;
120800150112         clear  eKC10fim;
120900150112         clear  eKC10pru;
121000150112         clear  eKC10flo;
121100141203
121200141028         // -?Il recupero delle Note è gestibile solo per chiave completa?
121300141204         //  ?(SE non ricevuti data e ora immissione: vengono recuperate?
121400150122         //  ?dalle ultime Note inserite, con o senza - se "999" - la?
121500150122         //  ?fase ricevuta)?
121600141028         clear  wCountNote;
121700141028         clear  keyTICMN01;
121800150112         k_CMNncm = iKC10ncm;
121900150112         k_CMNksu = iKC10ksu;
122000150112         k_CMNksc = iKC10ksc;
122100150112         k_CMNcpo = iKC10cpo;
122200150112         k_CMNdim = eKC10dim;
122300150112         k_CMNhim = eKC10him;
122400141028         //k_CMNpno = ? ? ? ? ?;
122500150121
122600150122         // ·?Non ricevuti data e ora immissione: vengono recuperate?
122700150122         //  ?dalle ultime Note inserite, con o senza (se "999") la?
122800150122         //  ?fase ricevuta?
122900150121         If  k_CMNhim = *zero  and  k_CMNdim = *zero;
123000150121
123100150121           setGt  %kds( keyTICMN01 : 4 )  TICMN000;
123200150121           readPe(N)  %kds( keyTICMN01 : 4 )  TICMN000;
123300150121
123400150121           DoW  Not  %eof( TICMN01L );
123500150121
123600150121             if  ( CMNacm <> *blank   or
123700150121                   kNmUs  =  CMNpru   or
123800150121                   DUTlpo =  'S' )    and
123900150121                 ( iKC10acm = *all'9'  or  iKC10acm = CMNacm );
124000150121               k_CMNdim = CMNdim;
124100150121               k_CMNhim = CMNhim;
124200150121               leave;
124300150121             endif;
124400150121
124500150121             readPe(N)  %kds( keyTICMN01 : 4 )  TICMN000;
124600150121
124700150121           EndDo;
124800150121
124900150121         EndIf;
125000141028
125100150122         // ·?Recupero delle ultime Note inserite?
125200150122         //  ?con o senza (se "999") la fase ricevuta?
125300141202         setll  %kds( keyTICMN01 : 6 )  TICMN000;
125400141203         select;
125500141203           when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
125600141203             reade(N)  %kds( keyTICMN01 : 6 )  TICMN000;
125700141203           when  k_CMNdim <> *zero;
125800141203             reade(N)  %kds( keyTICMN01 : 5 )  TICMN000;
125900141203           other;
126000141203             reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
126100141203         endsl;
126200141028
126300141028         DoW  Not  %eof( TICMN01L );
126400141028
126500141203           If  CMNacm <> *blank  or
126600141203               kNmUs  =  CMNpru  or
126700141203               DUTlpo =  'S';
126800141202
126900141202             wCountNote += 1;
127000141202             select;
127100150112               when  iKC10acm <> *all'9'  and  iKC10acm <> CMNacm;
127200141203                 wCountNote -= 1;
127300141202               when  wCountNote = 1;
127400150112                 eKC10no1 = CMNnot;
127500150112                 eKC10dim = CMNdim;
127600150112                 eKC10him = CMNhim;
127700150112                 eKC10fim = CMNfim;
127800150112                 eKC10pru = CMNpru;
127900150112                 eKC10flo = CMNflo;
128000141203                 if  k_CMNdim = *zero  or  k_CMNhim = *zero;
128100141203                   k_CMNhim = CMNhim;
128200141203                 endif;
128300141203                 if  k_CMNdim = *zero;
128400141203                   k_CMNdim = CMNdim;
128500141203                 endif;
128600141202               when  wCountNote = 2;
128700150112                 eKC10no2 = CMNnot;
128800141202               when  wCountNote > 2;
128900150112                 oKC10piu = '+';
129000141202                 leave;
129100141202             endsl;
129200141202
129300141203           EndIf;
129400141028
129500141203           select;
129600141203             when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
129700141203               reade(N)  %kds( keyTICMN01 : 6 )  TICMN000;
129800141203             when  k_CMNdim <> *zero;
129900141203               reade(N)  %kds( keyTICMN01 : 5 )  TICMN000;
130000141203             other;
130100141203               reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
130200141203           endsl;
130300141028
130400141028         EndDo;
130500141024
130600141028       ENDSR;
130700141024
130800141028       //--------------------------------------------------------------
130900141028       //?Cancellazione Note indicate dal chiamante.                   ?
131000141028       //--------------------------------------------------------------
131100141028       BEGSR  sr_Delete;
131200141204
131300150112         clear  eKC10no1;
131400150112         clear  eKC10no2;
131500150112         //clear  eKC10dim;
131600150112         //clear  eKC10him;
131700150112         clear  eKC10fim;
131800150112         clear  eKC10pru;
131900150112         clear  eKC10flo;
132000141024
132100141204         // -?La cancellazione delle Note è gestibile anche per chiave parziale?
132200141204         //  ?(SE non ricevuti data e ora immissione: vengono cancellate?
132300141204         //  ?tutte le note inserite per la fase ricevuta)?
132400141028         clear  keyTICMN01;
132500150112         k_CMNncm = iKC10ncm;
132600150112         k_CMNksu = iKC10ksu;
132700150112         k_CMNksc = iKC10ksc;
132800150112         k_CMNcpo = iKC10cpo;
132900150112         k_CMNdim = eKC10dim;
133000150112         k_CMNhim = eKC10him;
133100141028         //k_CMNpno = ? ? ? ? ?;
133200141028
133300141202         setll  %kds( keyTICMN01 : 6 )  TICMN000;
133400141202         select;
133500141204           when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
133600141202             reade  %kds( keyTICMN01 : 6 )  TICMN000;
133700141204           when  k_CMNdim <> *zero;
133800141202             reade  %kds( keyTICMN01 : 5 )  TICMN000;
133900141202           other;
134000141202             reade  %kds( keyTICMN01 : 4 )  TICMN000;
134100141202         endsl;
134200141028
134300141028         DoW  Not  %eof( TICMN01L );
134400141204
134500150112           if  iKC10acm = *all'9'  or  iKC10acm = CMNacm;
134600141028
134700141204             //_______________
134800141204             delete  TICMN000;
134900141204             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
135000141204
135100141204           endif;
135200141028
135300141202           select;
135400141204             when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
135500141202               reade  %kds( keyTICMN01 : 6 )  TICMN000;
135600141204             when  k_CMNdim <> *zero;
135700141202               reade  %kds( keyTICMN01 : 5 )  TICMN000;
135800141202             other;
135900141202               reade  %kds( keyTICMN01 : 4 )  TICMN000;
136000141202           endsl;
136100141028
136200141028         EndDo;
136300141024
136400141028       ENDSR;
136500141028
136600141028       //--------------------------------------------------------------
136700141028       //?Controllo caratteri in una singola Nota.                   ?
136800141028       //--------------------------------------------------------------
136900141028       BEGSR  sr_ChkChar;
137000141028
137100141028         clear TxtInOut;
137200141028         TxtInOut = CMNnot;
137300141028         ChkNull  = '1';
137400141028
137500141028         xchkchar ( TxtInOut :
137600141028                    ElencoChar :
137700141028                    TipoElenco :
137800141028                    CharSost :
137900141028                    UpperCase :
138000141028                    ChkNull :
138100141028                    CharNull :
138200141028                    Esito );
138300141028
138400141028         select;
138500141028           // -?Nessuna modifica effettuata?
138600141028           when  Esito = *off;
138700141028           // -?Modifica effettuata?
138800141028           when  Esito = *on;
138900141028             CMNnot = TxtInOut;
139000141028           // -?Errore nei parametri di input?
139100141028           when  Esito = 'I';
139200141028             clear  CMNnot;
139300141028           // -?Errore di elaborazione?
139400141028           //when  Esito = 'E';
139500141028           //  clear  CMNnot;
139600141028         endsl;
139700141028
139800141028       ENDSR;
139900141024
140000141024       //--------------------------------------------------------------
140100141024       //?Operazioni finali.                                         ?
140200141024       //--------------------------------------------------------------
140300141024       BEGSR  sr_RoutEnd;
140400141028
140500141028         // -?Chiusura applicazioni precedentemente aperte?
140600150112         If  iKC10tla = 'C'  or  iKC10tla = 'L';
140700141028
140800141028           if  I69kac <> *zero;
140900141028             clear  TIBS69ds;
141000141028             I69tla = 'C';
141100141028             tibs69r( tibs69ds :
141200141028                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
141300141028           endif;
141400141028
141500141028         EndIf;
141600141024
141700141024         // -?Chiusura *pgm?
141800141024         return;
141900141024
142000141024       ENDSR;
142100141024
142200141024      /end-free
142300141028
142400141028** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
142500141104Reperite TROPPE note per la Campagna Commerciale (già 999)                      1
