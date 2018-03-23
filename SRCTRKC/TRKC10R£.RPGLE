000100141024       //==============================================================
000200141024       //?MKCM10R * Gestione Note per Campagna Cliente                 ?
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
003400141024     fMKCM10D   cf   e             workstn
003500141024     f                                     sfile(CM10S01 : S01nrr)
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
014600141028     d MKCM10ds      e ds
014700141024
014800141024     d SaveMKCM10ds  e ds                  extname(MKCM10ds)
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
024400141024     c                   parm                    MKCM10ds
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
025700141024         when  iCM10flm = 'D';
025800141024           exsr  sr_Delete;
025900141024
026000141024         // -?Richiesto RECUPERO note      (senza video)?
026100141024         when  iCM10flm = 'R';
026200141024           exsr  sr_Recupero;
026300141024
026400141024         // -?Richiesta CONFERMA note      (senza video)?
026500141103         //  ?precedentemente immesse nel SFL?
026600141024         when  iCM10flm = 'C';
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
029500141024         //  ?TIPO LANCIO ICM10TLA = "C" -> Solo Chiusura in LR?
029600141024         //                         ?"L" -> Elaboraz.+Chiusura in LR?
029700141024         //                         ?" " -> Elaboraz.+Chiusura in RT?
029800141024         *inLR = ( iCM10tla = 'C'  or  iCM10tla = 'L' );
029900141024
030000141031         // -?Pulizia parametri di solo Output?
030100141024         clear  oCM10piu;
030200141024         clear  oCM10fxx;
030300141024         clear  oCM10err;
030400141024         clear  oCM10msg;
030500141031
030600141031         // -?Verifica SE richiesta la sola chiusura del *pgm?
030700141031         $Fine = ( iCM10tla = 'C' );
030800141031         if  $Fine;
030900141103           leavesr;
031000141031         endif;
031100141024
031200141031         // -?Impostazione Commit?
031300141028         if  Not %open(TICMN01L);
031400141024           $FlgCmt = ( iCM10cmt = *on );
031500141028           open  TICMN01L;
031600141024         endif;
031700141024
031800141024         // -?Reperimento dati job?
031900141024         exsr  sr_DatiJob;
032000141024
032100141024         // -?NON richiesta la gestione del video?
032200141024         if  iCM10flm = 'C'  or
032300141024             iCM10flm = 'D'  or
032400141024             iCM10flm = 'R';
032500141024           leavesr;
032600141024         endif;
032700141024
032800141031         // -?Verifica SE richiesta l'inizializzazione del video?
032900141031         //  ?(NON se 2° richiamo per la stessa Campagna)?
033000141024         if  iCM10tla <> SaveMKCM10ds.iCM10tla  or
033100141024             iCM10flm <> SaveMKCM10ds.iCM10flm  or
033200141024             iCM10ncm <> SaveMKCM10ds.iCM10ncm  or
033300141024             iCM10ksu <> SaveMKCM10ds.iCM10ksu  or
033400141024             iCM10ksc <> SaveMKCM10ds.iCM10ksc  or
033500141024             iCM10cpo <> SaveMKCM10ds.iCM10cpo  or
033600141024             iCM10acm <> SaveMKCM10ds.iCM10acm;
033700141031           $InzS01 = *on;
033800141024         endif;
033900141031         // -?Verifica SE ricevute Note da proporre a video?
034000141031         //  ?(NON qui per ICM10FLM = "C" e Note <> *blank)?
034100141031         if  iCM10flm =  'M';
034200141031           $InzS01_B = *on;
034300141031         endif;
034400141031
034500141031         SaveMKCM10ds = MKCM10ds;
034600141024
034700141024         // -?Impostazione nome programma a video?
034800141024         VTCpgm = SDSpgm;
034900141024
035000141024         $Video = 'S1';
035100141024         clear  VTCmod;
035200141024         select;
035300141030           when  iCM10flm = *blank  or  iCM10flm = 'M';
035400141024             VTCmod = '  IMMISSIONE  ';
035500141024           when  iCM10flm = 'I';
035600141024             VTCmod = 'INTERROGAZIONE';
035700141024         endsl;
035800141024
035900141024         // -?(Dis)Abilitazione tasti funzionali?
036000141024         F3Attivo  = *off;
036100141031         F6Attivo  = ( iCM10flm =  *blank );
036200141031         F10Attivo = ( iCM10flm =  'I'  and  iCM10f10 = 'S' );
036300141030         F12Attivo = ( iCM10flm <> 'M' );
036400141030
036500141030         //DescrINVIO = ( iCM10flm = 'M' );   //?(già condizionato da "Not F12Attivo")?
036600141024
036700141024         // -?Impostazione parzializzazioni richieste?
036800141024         clear  CM10C01;
036900141024         C01ncm   = iCM10ncm;
037000141024         C01des   = iCM10des;
037100141024         C01ksu   = iCM10ksu;
037200141031         C01ksc   = iCM10ksc;
037300141031         C01cpo   = iCM10cpo;
037400141031         select;
037500141031           when  iCM10cpo > *zero;
037600141031             C01cpoD  = iCM10rsc;
037700141031           when  iCM10ksc > *zero;
037800141031             C01kscD  = iCM10rsc;
037900141031           when  iCM10ksu > *zero;
038000141031             C01ksuD  = iCM10rsc;
038100141031         endsl;
038200141024
038300141024         // -?Decodifica campagna - se non ricevuta?
038400141031         If  iCM10des = *blank;
038500141031           if  iCM10ncm <> k_CMPncm;
038600141031             clear  keyTICMP01;
038700141031             k_CMPncm = iCM10ncm;
038800141031             chain  %kds( keyTICMP01 )  TICMP000;
038900141031             if  Not %found(TICMP01L);
039000141031               CMPdes = *all'? ';
039100141031             endif;
039200141024           endif;
039300141031           C01des = CMPdes;
039400141024         EndIf;
039500141024
039600141024         // -?Decodifica cliente  - se non ricevuta?
039700141024         If  iCM10rsc =  *blank;
039800141024
039900141024           Select;
040000141024
040100141031             When  iCM10ksu <> *zero  or  iCM10ksc <> *zero;
040200141031               if  ( iCM10ksu <> *zero  and  iCM10ksu <> I69kac )  or
040300141031                   ( iCM10ksc <> *zero  and  iCM10ksc <> I69kac );
040400141031                 clear  TIBS69ds;
040500141031                 I69sif = knsif;
040600141031                 I69kcc = DUTkci;
040700141031                 if  iCM10ksu <> *zero;
040800141031                   I69kac = iCM10ksu;
040900141031                 else;
041000141031                   I69kac = iCM10ksc;
041100141031                 endif;
041200141031                 tibs69r( tibs69ds :
041300141031                          ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
041400141031                 if  O69err = *on;
041500141031                   ACOrag = *all'? ';
041600141031                 endif;
041700141031               endif;
041800141031               if  iCM10ksu <> *zero;
041900141031                 C01ksuD = ACOrag;
042000141031               else;
042100141031                 C01kscD = ACOrag;
042200141031               endif;
042300141024
042400141031             When  iCM10cpo <> *zero;
042500141031               if  iCM10cpo <> k_CPOcpo;
042600141031                 if  not %open(TNCPO01L);
042700141031                   open  TNCPO01L;
042800141031                 endif;
042900141031                 clear  keyTNCPO01;
043000141031                 k_CPOcpo = iCM10cpo;
043100141031                 chain  %kds( keyTNCPO01 )  TNCPO000;
043200141031                 if  Not %found(TNCPO01L);
043300141031                   CPOrag = *all'? ';
043400141031                 endif;
043500141024               endif;
043600141031               C01cpoD = CPOrag;
043700141024
043800141024           EndSl;
043900141024
044000141024         EndIf;
044100141024
044200141024         // -?Impostazione attributi di visualizzazione in testata?
044300141031         DisplayKSU = *off;
044400141031         DisplayKSC = *off;
044500141031         DisplayCPO = *off;
044600141031         select;
044700141031           when  iCM10cpo <> *zero;
044800141031             DisplayCPO = *on;
044900141031           when  iCM10ksc <> *zero;
045000141031             DisplayKSC = *on;
045100141031           when  iCM10ksu <> *zero;
045200141031             DisplayKSU = *on;
045300141031         endsl;
045400141024
045500141024       ENDSR;
045600141024
045700141024       //--------------------------------------------------------------
045800141024       //?Reperimento Dati del job (Utente/Operativi).                 ?
045900141024       //--------------------------------------------------------------
046000141024       BEGSR  sr_DatiJob;
046100141024
046200141024         in(e) §AzUte;
046300141024         if NOT %error;
046400141024           in(e) §DatiUte;
046500141024         endif;
046600141024         if %error or RSut = *blank;
046700141024           tibs34r ( tibs34ds );
046800141024           in §AzUte;
046900141024           in §DatiUte;
047000141024         endif;
047100141024
047200141024       ENDSR;
047300141024
047400141024       //--------------------------------------------------------------
047500141027       //?Gestione SubFile S01.                                        ?
047600141024       //--------------------------------------------------------------
047700141024       BEGSR  sr_GesS01;
047800141024
047900141024         // -?Inizializzazione videata?
048000141024         if  $InzS01 = *on;
048100141024           exsr  sr_InzS01;
048200141024           $InzS01 = *off;
048300141024         endif;
048400141027
048500141027         // -?Impostazione Note eventualmente ricevute?
048600141031         if  $InzS01_B = *on;
048700141031           $UpdS01   = *on;
048800141027           exsr  sr_UpdS01;
048900141031           $InzS01_B = *off;
049000141027         endif;
049100141024
049200141024         // -?Emissione Testata & Piede con tasti funzionali abilitati?
049300141024         write  CM10T01;
049400141024         write  CM10P01;
049500141027
049600141024         // -?Posizionamento cursore?
049700141027         //  ?C1CsrRrn contiene il numero di riga del SubFile su cui?
049800141024         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
049900141024         //  ?si visualizza la pagina che vedeva l'utente quando ha?
050000141024         //  ?premuto l'ultimo tasto.?
050100141024         if  C1CsrRrn > *zero;
050200141024           C1RcdNbr = C1CsrRrn;
050300141024         else;
050400141024           C1RcdNbr = 1;
050500141027           write  CM10S00;          //?(no rec. - SOLO visualizzazione)?
050600141024         endif;
050700141024
050800141024         // -?Emissione videata?
050900141024         exfmt  CM10C01;
051000141024
051100141024         clear  V1Dmsg;
051200141024         reset  ErrMessage;
051300141024         reset  ErrGenerico;
051400141024
051500141024         SELECT;
051600141024
051700141024           // -?F3=Fine?
051800141024           WHEN  dsp_aid = c_F03;
051900141024             oCM10fxx = '03';
052000141031             $InzS01  = *on;
052100141031             $Fine    = *on;
052200141031
052300141031           // -?F10=Inserimento (da visualizzazione)?
052400141031           WHEN  dsp_aid = c_F10;
052500141031             $InzS01   = *on;
052600141031             F10Attivo = *off;
052700141031             F6Attivo  = *on;
052800141024
052900141024           // -?F12=Ritorno?
053000141024           WHEN  dsp_aid = c_F12;
053100141024             oCM10fxx = '12';
053200141031             $InzS01  = *on;
053300141031             $Fine    = *on;
053400141027
053500141027           // -?RollUp=Caricamento righe vuote di I/O?
053600141027           WHEN  dsp_aid = c_RollUp;
053700141027             exsr  sr_RollUpS01;
053800141024
053900141027           // -?Invio o F6=Conferma?
054000141024           OTHER;
054100141103             // -?Aggiornamento posizionamento nel subfile?
054200141031             Select;
054300141030               // -?Invio=Prosegui?
054400141031               //when  DescrINVIO     and  dsp_aid = c_Enter;
054500141031               When  Not F12Attivo  and  dsp_aid = c_Enter;
054600141030                 exsr  sr_SetECM10___1;
054700141031                 if  NOT ErrGenerico;
054800141031                   //clear  oCM10fxx;
054900141031                   $Fine = *on;
055000141031                 endif;
055100141030               // -?F6=Conferma?
055200141031               When  dsp_aid = c_F06;
055300141030                 exsr  sr_WriteTICMN;
055400141030                 if  NOT ErrGenerico;
055500141030                   oCM10fxx = '06';
055600141030                   $Fine = *on;
055700141030                 endif;
055800141031               // -?Invio?
055900141031               //  ?(Posizionamento cursore sul 1° rec. vuoto)?
056000141031               Other;
056100141031                 $UpdS01 = *off;
056200141031                 exsr  sr_UpdS01;
056300141031             EndSl;
056400141027             // -?Errore?
056500141027             if  ErrGenerico;
056600141027               leavesr;
056700141027             endif;
056800141024
056900141024         ENDSL;
057000141024
057100141024       ENDSR;
057200141024
057300141024       //--------------------------------------------------------------
057400141027       //?Inizializzazione SubFile S01.                                ?
057500141024       //--------------------------------------------------------------
057600141024       BEGSR  sr_InzS01;
057700141024
057800141024         // -?Spegnimento degli indicatori di errore?
057900141024         %subst(IndDspF : 50) = *off;
058000141024
058100141103         // -?Pulizia subfile?
058200141024         SflDsp_N    = *on;
058300141024         SflDspCtl_N = *on;
058400141024         write  CM10C01;
058500141024         SflDspCtl_N = *off;
058600141024         SflEnd      = *off;
058700141024
058800141024         clear  C1RcdNbr;
058900141024         clear  C1CsrRrn;
059000141024         clear  S01nrr;
059100141024         clear  V1Dmsg;
059200141024         ErrMessage  = *off;
059300141024         ErrGenerico = *off;
059400141202         clear  wCMNpno;
059500141027         clear  wNrr1Free;
059600141027         clear  wNrr9Free;
059700141030         //clear  wCountNote;
059800141031         clear  wSaveCMNpru;
059900141031         clear  wSaveCMNdim;
060000141031         clear  wSaveCMNhim;
060100141024
060200141103         // -?Caricamento - completo - delle note nel subfile?
060300141024         clear  keyTICMN01;
060400141024         k_CMNncm = iCM10ncm;
060500141024         k_CMNksu = iCM10ksu;
060600141024         k_CMNksc = iCM10ksc;
060700141024         k_CMNcpo = iCM10cpo;
060800141118         setll  %kds( keyTICMN01 : 4 )  TICMN000;
060900141118         reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
061000141024
061100141024         DoW  Not  %eof(TICMN01L);
061200141024
061300141204           // -?Emissione messaggio di segnalazione raggiungimento limite?
061400141204           if  S01nrr >= c_MaxRec;
061500141204             ErrGenerico = *on;
061600141204             ErrMessage  = *on;
061700141204             V1Dmsg = sk_Msg(01);
061800141204             leave;
061900141204           endif;
062000141024
062100141203           if  CMNacm <> *blank  or
062200141203               kNmUs  =  CMNpru  or
062300141203               DUTlpo =  'S';
062400141202             exsr  sr_RieRecS01;
062500141202           endif;
062600141024
062700141118           reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
062800141024
062900141024         EndDo;
063000141024
063100141103         // -?Caricamento record vuoti - almeno 2 - nel subfile?
063200141024         //  ?(SE NON interrogazione  e?
063300141103         //  ? SE NON raggiunto il n° Max di rec caricabili nel SFL)?
063400141031         IF  ( iCM10flm <> 'I'  or
063500141031             ( iCM10flm =  'I'  and  iCM10f10 = 'S'
063600141202                                and  dsp_aid  = c_F10 ) )  and
063700141204             S01nrr < c_MaxRec;
063800141024
063900141204           //// -?Azzeramento nuovo Progressivo Note?
064000141204           //clear  wCMNpno;  ?(già fatto)?
064100141027
064200141103           // -?Memorizzazione 1° Nrr libero nel subfile?
064300141027           wNrr1Free = S01nrr + 1;
064400141024
064500141103           // -?Caricamento record vuoti - almeno 2 - nel subfile?
064600141024           //  ?(fino al riempimento della pagina)?
064700141027           exsr  sr_RollUpS01;
064800141024
064900141027         ENDIF;
065000141103
065100141204         // -?Raggiunto il n° Max di rec. gestibili nel subfile (9.999)?
065200141204         //  ?"solo" per visualizzare le note già inserite:?
065300141103         // ·?DISattivazione F6=Conferma?
065400141103         if  F6Attivo  and  wNrr1Free = *zero;
065500141103           F6Attivo = *off;
065600141103         endif;
065700141103         // ·?DISattivazione F10=Inserimento?
065800141204         if  F10Attivo  and  S01nrr >= c_MaxRec;
065900141103           F10Attivo = *off;
066000141103         endif;
066100141103
066200141103         // -?Emissione messaggio di segnalazione raggiungimento limite?
066300141204         //  ?- già visualizzati 9.999 rec. nel subfile o 999 rec. I/O -?
066400141204         //  ?N.B. - CMNPRG è 3/0?
066500141204         if  ( iCM10flm  <> 'I'  or  ( iCM10flm = 'I'      and
066600141204                                       iCM10f10 = 'S' ) )  AND
066700141204             ( S01nrr    >= c_MaxRec  or
066800141204               wNrr9Free >= wNrr1Free + w_MaxRec );
066900141103           ErrGenerico = *on;
067000141103           ErrMessage  = *on;
067100141103           V1Dmsg = sk_Msg(01);
067200141103         endif;
067300141024
067400141024         // -?Visualizzazione del SFL (se ci sono dati)?
067500141024         SflDsp_N  = (S01nrr <= *zero);
067600141024
067700141024         // -?Attivazione del SFLEND?
067800141031         SflEnd = ( ( %eof(TICMN01L)  and  iCM10flm = 'I'
067900141031                                      and  NOT F10Attivo  )  or
068000141204                    S01nrr    >= c_MaxRec                    or
068100141204                    wNrr9Free >= wNrr1Free + w_MaxRec );
068200141024
068300141103         // -?Posizionamento subfile?
068400141027         If  S01nrr > *zero;
068500141027
068600141027           select;
068700141027             // -?Se visualizzazione: nel 1° rec.?
068800141031             when    iCM10flm = 'I'    and
068900141031                   ( iCM10f10 = *blank  or  F10Attivo );
069000141027               C1RcdNbr = 1;
069100141027             // -?Se in pagina nuova / tutta libera (RollUp): nel 1° rec.?
069200141027             //  ?della pagina (tutti liberi)?
069300141027             when  wNrr1FreePag > wNrr1Free;
069400141027               C1RcdNbr = wNrr1FreePag;
069500141027             // -?Altrimenti: nel 1° rec. libero della pagina?
069600141027             other;
069700141027               C1RcdNbr = wNrr1Free;
069800141027           endsl;
069900141027
070000141024           C1CsrRrn  = C1RcdNbr;
070100141027
070200141027         Else;
070300141027
070400141024           clear  C1RcdNbr;
070500141027
070600141027         EndIf;
070700141024
070800141024       ENDSR;
070900141024
071000141024       //--------------------------------------------------------------
071100141027       //?Caricamento singolo record del SubFile S01.                  ?
071200141024       //--------------------------------------------------------------
071300141024       BEGSR  sr_RieRecS01;
071400141024
071500141024         clear  CM10S01;
071600141024
071700141103         // -?Impostazione campi nel record del subfile?
071800141024         // ·?Campi Hidden?
071900141203         HS1acm  = CMNacm;               // ·?Fase campagna?
072000141203         HS1pno  = CMNpno;               // ·?Progressivo Nota?
072100141203         HS1dim  = CMNdim;               // ·?Data inserimento?
072200141203         HS1him  = CMNhim;               // ·?Ora  inserimento?
072300141203         HS1fim  = CMNfim;               // ·?Fil. inserimento?
072400141203         HS1flo  = CMNflo;               // ·?Flag Operativi?
072500141024         // ·?Campi di Input/Output?
072600141203         S01not  = CMNnot;               // ·?Note?
072700141024         // ·?Campi di Output?
072800141202         if  CMNpru <> wSaveCMNpru  or
072900141030             CMNdim <> wSaveCMNdim  or
073000141030             CMNhim <> wSaveCMNhim;
073100141202           if  CMNacm <> *blank;
073200141202             exsr  sr_DecodFaseCamp;
073300141202             S01fcm = dFCM.§FCMdes;
073400141202           else;
073500141202             S01fcm = 'RISERVATA';
073600141202           endif;
073700141030           S01pru = CMNpru;
073800141030           reset  WLBdat;
073900141030           G02inv = CMNdim;
074000141030           xsrda8 ( WLBdat );
074100141030           if  G02err = *off;
074200141030             S01dim = G02dat;
074300141030           endif;
074400141030           S01him = ( CMNhim / 100 );
074500141118           wSaveCMNpru = CMNpru;
074600141118           wSaveCMNdim = CMNdim;
074700141118           wSaveCMNhim = CMNhim;
074800141030         endif;
074900141024
075000141024         // -?Impostazione attributi di visualizzazione?
075100141203         // ·?Evidenziazione Fase (se attuale)?
075200141203         EvidenzFCM = ( HS1acm <> *blank  and  HS1acm = iCM10acm );
075300141203         // ·?Evidenziazione Fase/Note (se Riservate)?
075400141203         EvidenzRIS = ( HS1acm = *blank );
075500141024         // ·?Protezione delle Note già inserite?
075600141024         ProtectNOT = *on;
075700141027         HS1in49    = ProtectNOT;
075800141024
075900141103         // -?Registrazione singolo record nel subfile?
076000141024         S01nrr += 1;
076100141024         write  CM10S01;
076200141024
076300141024       ENDSR;
076400141118
076500141118       //--------------------------------------------------------------
076600141118       //?Decodifica Fase della Campagna Commerciale.                  ?
076700141118       //--------------------------------------------------------------
076800141118       BEGSR  sr_DecodFaseCamp;
076900141118
077000141118         clear  dFCM;
077100141118         reset  TIBS02ds;
077200141118         //T02mod = 'C';        ?(già così)?
077300141118         //T02cod = 'FCM';      ?(già così)?
077400141118         T02ke1 = CMNacm;
077500141118         T02sif = KNSIF;
077600141118
077700141118         TNTBE_RicercaControllo ( kpjba : tibs02ds );
077800141118
077900141118         if  T02err = *blank;
078000141118           dFCM = T02uni;
078100141118         endif;
078200141118
078300141118       ENDSR;
078400141027
078500141027       //--------------------------------------------------------------
078600141028       //?Gestione Roll-Up nel SubFile S01.                            ?
078700141027       //--------------------------------------------------------------
078800141027       BEGSR  sr_RollUpS01;
078900141027
079000141027         clear  wNrr1FreePag;
079100141027         clear  CM10S01;
079200141027         SflNxtChg = *off;
079300141027
079400141028         wPag = ( S01nrr / c_SflPag ) + 1;
079500141028
079600141204         DoW  S01nrr < ( wPag * c_SflPag )  and
079700141204              S01nrr < c_MaxRec             and
079800141204              S01nrr < wNrr1Free + w_MaxRec - 1;
079900141027
080000141118           // -?Impostazione campi ricevuti nel record del subfile?
080100141027           // ·?Campi Hidden?
080200141203           HS1acm  = iCM10acm;           // ·?Fase campagna?
080300141202           HS1dim  = eCM10dim;           // ·?Data inserimento?
080400141203           HS1him  = eCM10him;           // ·?Ora  inserimento?
080500141203           HS1fim  = eCM10fim;           // ·?Fil. inserimento?
080600141202           HS1flo  = eCM10flo;           // ·?Flag operativi?
080700141027           // ·?Campi di Input/Output?
080800141120           //clear  S01not;              // ·?Note (VUOTE)?
080900141027
081000141027           // -?Impostazione attributi di visualizzazione?
081100141203           // ·?Evidenziazione Fase (se attuale) - NON in inserimento?
081200141202           EvidenzFCM = *off;
081300141203           // ·?Evidenziazione Fase/Note (se Riservata)?
081400141203           EvidenzRIS = ( HS1acm = *blank );
081500141117           // ·?Protezione delle Note già inserite?
081600141027           ProtectNOT = *off;
081700141027           HS1in49    = ProtectNOT;
081800141027
081900141103           // -?Posizionamento cursore (sul 1° record libero del subfile)?
082000141027           PosCurNOT  = ( wNrr1Free = S01nrr + 1   or
082100141027                        ( S01nrr    > wNrr1Free   and
082200141027                          %rem( S01nrr : c_SflPag ) = *zero  ) );
082300141027
082400141103           // -?Registrazione singolo record VUOTO nel subfile?
082500141027           S01nrr += 1;
082600141027           write  CM10S01;
082700141027
082800141027           if  wNrr1FreePag = *zero;
082900141027             wNrr1FreePag = S01nrr;
083000141027           endif;
083100141027
083200141027         EndDo;
083300141027
083400141103         // -?Salvataggio ultimo Nrr registrato nel subfile?
083500141027         wNrr9Free = S01nrr;
083600141027
083700141027         // -?Visualizzazione del SFL (rec. - anche vuoti - ce ne sono)?
083800141027         SflDsp_N  = *off;
083900141027
084000141027         // -?Attivazione del SFLEND?
084100141204         SflEnd = ( S01nrr    >= c_MaxRec  or
084200141204                    wNrr9Free >= wNrr1Free + w_MaxRec );
084300141027
084400141103         // -?Posizionamento subfile?
084500141027         Select;
084600141027           // -?Se visualizzazione: nel 1° rec. della pagina?
084700141027           When  iCM10flm = 'I';
084800141027             wPag     = S01nrr / c_SflPag;
084900141027             wRig     = S01nrr - (c_SflPag * wPag);
085000141027             C1RcdNbr = wPag * c_SflPag;
085100141027             if  wRig > *zero;
085200141027               C1RcdNbr += 1;
085300141027             else;
085400141027               C1RcdNbr = C1RcdNbr - c_SflPag + 1;
085500141027             endif;
085600141027           // -?Se in pagina nuova / tutta libera (RollUp): nel 1° rec.?
085700141027           //  ?della pagina (tutti liberi)?
085800141027           When  wNrr1FreePag > wNrr1Free;
085900141027             C1RcdNbr = wNrr1FreePag;
086000141028           // -?Altrimenti: nel 1° rec. libero della pagina (ultima,?
086100141028           //  ?libera in parte)?
086200141027           Other;
086300141027             C1RcdNbr = wNrr1Free;
086400141027         EndSl;
086500141027
086600141027         C1CsrRrn  = C1RcdNbr;
086700141027
086800141027       ENDSR;
086900141028
087000141028       //--------------------------------------------------------------
087100141028       //?Controllo dati in S01.                                     ?
087200141028       //--------------------------------------------------------------
087300141030       BEGSR  sr_CtrS01;
087400141028
087500141028         // -?Spegnimento degli indicatori di errore?
087600141028         %subst(IndDspF : 50) = *off;
087700141028
087800141028       ENDSR;
087900141024
088000141024       //--------------------------------------------------------------
088100141028       //?Scrittura nuove Note in TICMN00F.                          ?
088200141024       //--------------------------------------------------------------
088300141030       BEGSR  sr_WriteTICMN;
088400141024
088500141031         clear  TICMN000;
088600141030         wTime_ds = %editc( %dec( %timestamp() ) : 'X' );
088700141028
088800141202         SELECT;
088900141202
089000141202           // -?SE il subfile è caricato: si scrive in base al contenuto?
089100141202           //  ?del subfile?
089200141202           WHEN  wNrr1Free > *zero;
089300141202
089400141202             // ·?Richiamato con "C"=Conferma dati precedentemente?
089500141202             //  ?immessi: prima si aggiornano le prime 2 righe libere?
089600141202             //  ?nel SFL?
089700141202             if  iCM10flm = 'C'  and  iCM10fno = 'S';
089800141202               $UpdS01 = *on;
089900141202               exsr  sr_UpdS01;
090000141202             endif;
090100141202
090200141202             // ·?Comunque si scrivono le note del subfile?
090300141202             clear  eCM10no1;
090400141202             clear  eCM10no2;
090500141202             clear  wCountNote;
090600141202             For  S01nrr = wNrr1Free  To  wNrr9Free;
090700141202
090800141202               chain  S01nrr  CM10S01;
090900141202
091000141202               if  %found(MKCM10D)  and  S01not <> *blank;
091100141202                 exsr  sr_WrtRecTICMN;
091200141202                 exsr  sr_SetECM10___2;
091300141204                 exsr  sr_SetECM10___3;
091400141202               endif;
091500141202
091600141202             EndFor;
091700141202
091800141202           // -?Se il subfile NON è caricato: si scrive in base ai parametri?
091900141202           //  ?ricevuti (...SE ricevute note)?
092000141202           WHEN  eCM10no1 <> *blank  or  eCM10no2 <> *blank;
092100141103
092200141202             // ·?Ricerca del 1° progressivo libero (DOVREBBE essere 1...)?
092300141203             clear  wCMNpno;        //?...Meglio farlo SEMPRE!?
092400141203             //If  wCMNpno = *zero;
092500141103
092600141103               clear  keyTICMN01;
092700141103               k_CMNncm = iCM10ncm;
092800141103               k_CMNksu = iCM10ksu;
092900141103               k_CMNksc = iCM10ksc;
093000141103               k_CMNcpo = iCM10cpo;
093100141202               k_CMNdim = CMNdim;
093200141202               k_CMNhim = CMNhim;
093300141204               k_CMNpno = w_MaxRec;
093400141103               setGT  %kds( keyTICMN01 )  TICMN000;
093500141202               readPe(N)  %kds( keyTICMN01 : 6 )  TICMN000;
093600141103               if  Not  %eof(TICMN01L);
093700141202                 wCMNpno = CMNpno;
093800141103               endif;
093900141103
094000141204               if  wCMNpno >= w_MaxRec;  //?*ERR?
094100141103                 ErrGenerico = *on;
094200141103                 $Fine    = *on;
094300141103                 oCM10err = 'S';
094400141103                 oCM10msg = sk_Msg(01);
094500141103                 leavesr;
094600141103               endif;
094700141103
094800141203             //EndIf;
094900141028
095000141202             // ·?Impostazione dei campi nel subfile?
095100141203             C01ncm  = iCM10ncm;         // ·?Num. Campagna Comm.le?
095200141203             C01ksu  = iCM10ksu;         // ·?Cliente Unitario?
095300141203             C01ksc  = iCM10ksc;         // ·?Cod. Cliente?
095400141203             C01cpo  = iCM10cpo;         // ·?Cod. Potenziale?
095500141203             HS1acm  = iCM10acm;         // ·?Fase campagna?
095600141203             HS1dim  = eCM10dim;         // ·?Data inserimento?
095700141203             HS1him  = eCM10him;         // ·?Ora  inserimento?
095800141203             HS1fim  = eCM10fim;         // ·?Fil. inserimento?
095900141203             HS1flo  = eCM10flo;         // ·?Flag operativi?
096000141028
096100141202             // ·?Scrittura delle note ricevute?
096200141028             For  wRig = 1  To  2;
096300141028
096400141028               clear  S01not;
096500141028               select;
096600141028                 when  wRig = 1  and  eCM10no1 <> *blank;
096700141028                   S01not = eCM10no1;
096800141028                 when  wRig = 2  and  eCM10no2 <> *blank;
096900141028                   S01not = eCM10no2;
097000141028               endsl;
097100141028
097200141202               // ·?Scrittura singola nota del subfile?
097300141028               if  S01not <> *blank;
097400141028                 exsr  sr_WrtRecTICMN;
097500141204                 exsr  sr_SetECM10___2;
097600141204                 exsr  sr_SetECM10___3;
097700141028               endif;
097800141028
097900141028             EndFor;
098000141028
098100141202         ENDSL;
098200141028
098300141031         $InzS01   = *on;
098400141031         $InzS01_B = *on;
098500141028         clear  SaveMKCM10ds;
098600141028
098700141028       ENDSR;
098800141031
098900141031       //--------------------------------------------------------------
099000141031       //?Aggiornamento SubFile S01.                                   ?
099100141031       //--------------------------------------------------------------
099200141031       BEGSR  sr_UpdS01;
099300141031
099400141117         // -?SubFile SENZA rec. I/O (impossibile, ma...)?
099500141031         if  wNrr1Free <= *zero;
099600141031           leavesr;
099700141031         endif;
099800141031
099900141031         clear  wCountNote;
100000141031         SavS01nrr = S01nrr;
100100141031
100200141031         FOR  S01nrr = wNrr1Free  to  wNrr9Free;
100300141031
100400141031           chain  S01nrr  CM10S01;
100500141031
100600141031           If  %found(MKCM10D)  and  HS1in49 = *off;
100700141204
100800141204             if  wCountNote >= w_MaxRec;      //?*ERR?
100900141204               ErrGenerico = *on;
101000141204               ErrMessage  = *on;
101100141204               V1Dmsg = sk_Msg(01);
101200141204               leave;
101300141204             endif;
101400141031
101500141031             wCountNote += 1;
101600141031
101700141204             if  $UpdS01  and  wCountNote <= 2;
101800141031               select;
101900141031                 when  wCountNote = 1;
102000141031                   S01not = eCM10no1;
102100141031                 when  wCountNote = 2;
102200141031                   S01not = eCM10no2;
102300141031               endsl;
102400141031             endif;
102500141031
102600141031             // -?Impostazione attributi di visualizzazione?
102700141203             // ·?Evidenziazione Fase (se attuale) - NON in inserimento?
102800141118             EvidenzFCM = *off;
102900141203             // ·?Evidenziazione Fase/Note (se Riservata)?
103000141203             EvidenzRIS = ( HS1acm = *blank );
103100141117             // ·?Protezione delle Note già inserite?
103200141031             ProtectNOT = *off;
103300141031             HS1in49    = ProtectNOT;
103400141031
103500141103             // -?Posizionamento cursore (sul 1° record libero del subfile)?
103600141031             PosCurNOT  = ( S01not = *blank );
103700141031             C1RcdNbr   = S01nrr;
103800141031             C1CsrRrn   = C1RcdNbr;
103900141031
104000141103             // -?Aggiornamento record del subfile?
104100141031             SflNxtChg  = ( S01not <> *blank );
104200141031             update  CM10S01;
104300141031
104400141031             // -?Uscita DOPO aver posizionato il cursore nel 1° rec.?
104500141103             //  ?libero nel subfile?
104600141031             if  wCountNote > 2  and  S01not = *blank;
104700141031               leave;
104800141031             endif;
104900141031
105000141031           EndIf;
105100141031
105200141031         ENDFOR;
105300141031
105400141031         S01nrr = SavS01nrr;
105500141031
105600141031       ENDSR;
105700141031
105800141031       //--------------------------------------------------------------
105900141031       //?Lettura SubFile S01 per Impostazione parametri di Output.  ?
106000141031       //--------------------------------------------------------------
106100141031       BEGSR  sr_SetECM10___1;
106200141031
106300141031         // -?Aggiornamento parametri Note?
106400141031         clear  wCountNote;
106500141031         SavS01nrr = S01nrr;
106600141031
106700141031         FOR  S01nrr = wNrr1Free  to  wNrr9Free;
106800141031
106900141031           chain  S01nrr  CM10S01;
107000141031
107100141031           If  %found(MKCM10D)  and  HS1in49 = *off;
107200141031
107300141031             // -?Impostazione singolo parametro di Output?
107400141031             if  S01not <> *blank;
107500141031               exsr  sr_SetECM10___2;
107600141031             endif;
107700141031
107800141031             // -?Impostazione attributi di visualizzazione?
107900141203             // ·?Evidenziazione Fase (se attuale) - NON in inserimento?
108000141118             EvidenzFCM = *off;
108100141203             // ·?Evidenziazione Fase/Note (se Riservata)?
108200141203             EvidenzRIS = ( HS1acm = *blank );
108300141117             // ·?Protezione delle Note già inserite?
108400141031             ProtectNOT = *off;
108500141031             HS1in49    = ProtectNOT;
108600141031
108700141031             // -?Posizionamento cursore (sul 1° record libero)?
108800141031             PosCurNOT  = ( S01not = *blank );
108900141031
109000141103             // -?Aggiornamento record del subfile?
109100141031             SflNxtChg  = ( S01not <> *blank );
109200141031             update  CM10S01;
109300141031
109400141031             // -?Uscita DOPO aver letto le prime 3 note inserite?
109500141031             if  wCountNote > 2;
109600141031               leave;
109700141031             endif;
109800141031
109900141031           EndIf;
110000141031
110100141031         ENDFOR;
110200141031
110300141031         S01nrr = SavS01nrr;
110400141031
110500141031       ENDSR;
110600141030
110700141030       //--------------------------------------------------------------
110800141204       //?Impostazione singola Nota di (Input/)Output.                 ?
110900141030       //--------------------------------------------------------------
111000141030       BEGSR  sr_SetECM10___2;
111100141030
111200141030         wCountNote += 1;
111300141030
111400141030         select;
111500141030           when  wCountNote = 1;
111600141030             eCM10no1 = S01not;
111700141030           when  wCountNote = 2;
111800141030             eCM10no2 = S01not;
111900141030           when  wCountNote > 2;
112000141030             oCM10piu = '+';
112100141030         endsl;
112200141030
112300141030       ENDSR;
112400141204
112500141204       //--------------------------------------------------------------
112600141204       //?Impostazione altri parametri di (Input/)Output.              ?
112700141204       //--------------------------------------------------------------
112800141204       BEGSR  sr_SetECM10___3;
112900141204
113000141204         // -?Data inserimento?
113100141204         if  eCM10dim = *zero;
113200141204           eCM10dim = CMNdim;
113300141204         endif;
113400141204         // -?Ora inserimento?
113500141204         if  eCM10him = *zero;
113600141204           eCM10him = CMNhim;
113700141204         endif;
113800141204         // -?Filiale inserimento?
113900141204         if  eCM10fim = *zero;
114000141204           eCM10fim = DUTpou;
114100141204         endif;
114200141204         // -?Profilo Utente inserimento?
114300141204         if  eCM10pru = *blank;
114400141204           eCM10pru = kNmUs;
114500141204         endif;
114600141204
114700141204       ENDSR;
114800141028
114900141028       //--------------------------------------------------------------
115000141028       //?Scrittura singolo record in TICMN00F.                        ?
115100141028       //--------------------------------------------------------------
115200141028       BEGSR  sr_WrtRecTICMN;
115300141028
115400141202         wCMNpno += 1;
115500141024
115600141028         CMNncm = C01ncm;
115700141028         CMNksu = C01ksu;
115800141028         CMNksc = C01ksc;
115900141028         CMNcpo = C01cpo;
116000141028         CMNacm = HS1acm;
116100141202         CMNpno = wCMNpno;
116200141028
116300141028         // -?Data/Ora immissione: aggiorno SEMPRE con le correnti?
116400141030         //  ?(già impostate per mantenerle univoche)?
116500141028         //CMNdim = %int( %subst( %char( %dec( %timestamp() ) )
116600141028         //                       : 1 : 8 ) );
116700141028         //CMNhim = %int( %subst( %char( %dec( %timestamp() ) )
116800141028         //                       : 9 : 6 ) );
116900141028
117000141028         CMNfim = DUTpou;
117100141028         CMNpru = kNmUs;
117200141028
117300141028         //CMNflo = HS1flo;
117400141028
117500141028         CMNnot = S01not;
117600141028         // -?Note: controllo che NON contenga caratteri particolari?
117700141028         exsr sr_ChkChar;
117800141028         //if  CMNnot = *blank;
117900141028         //  leavesr;
118000141028         //endif;
118100141028
118200141028         //_____________
118300141028         write TICMN000;
118400141028         //¯¯¯¯¯¯¯¯¯¯¯¯¯
118500141028
118600141028       ENDSR;
118700141024
118800141028       //--------------------------------------------------------------
118900141028       //?Recupero Note da restituire al chiamante.                    ?
119000141028       //--------------------------------------------------------------
119100141028       BEGSR  sr_Recupero;
119200141028
119300141203         clear  eCM10no1;
119400141203         clear  eCM10no2;
119500141203         //clear  eCM10dim;
119600141203         //clear  eCM10him;
119700141203         clear  eCM10fim;
119800141203         clear  eCM10pru;
119900141203         clear  eCM10flo;
120000141203
120100141028         // -?Il recupero delle Note è gestibile solo per chiave completa?
120200141204         //  ?(SE non ricevuti data e ora immissione: vengono recuperate?
120300141203         //  ?le note del primo giorno/ora inserite per la fase ricevuta)?
120400141028         clear  wCountNote;
120500141028         clear  keyTICMN01;
120600141028         k_CMNncm = iCM10ncm;
120700141028         k_CMNksu = iCM10ksu;
120800141028         k_CMNksc = iCM10ksc;
120900141028         k_CMNcpo = iCM10cpo;
121000141202         k_CMNdim = eCM10dim;
121100141202         k_CMNhim = eCM10him;
121200141028         //k_CMNpno = ? ? ? ? ?;
121300141028
121400141202         setll  %kds( keyTICMN01 : 6 )  TICMN000;
121500141203         select;
121600141203           when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
121700141203             reade(N)  %kds( keyTICMN01 : 6 )  TICMN000;
121800141203           when  k_CMNdim <> *zero;
121900141203             reade(N)  %kds( keyTICMN01 : 5 )  TICMN000;
122000141203           other;
122100141203             reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
122200141203         endsl;
122300141028
122400141028         DoW  Not  %eof( TICMN01L );
122500141028
122600141203           If  CMNacm <> *blank  or
122700141203               kNmUs  =  CMNpru  or
122800141203               DUTlpo =  'S';
122900141202
123000141202             wCountNote += 1;
123100141202             select;
123200141203               when  iCM10acm <> *all'9'  and  iCM10acm <> CMNacm;
123300141203                 wCountNote -= 1;
123400141202               when  wCountNote = 1;
123500141202                 eCM10no1 = CMNnot;
123600141202                 eCM10dim = CMNdim;
123700141202                 eCM10him = CMNhim;
123800141202                 eCM10fim = CMNfim;
123900141202                 eCM10pru = CMNpru;
124000141202                 eCM10flo = CMNflo;
124100141203                 if  k_CMNdim = *zero  or  k_CMNhim = *zero;
124200141203                   k_CMNhim = CMNhim;
124300141203                 endif;
124400141203                 if  k_CMNdim = *zero;
124500141203                   k_CMNdim = CMNdim;
124600141203                 endif;
124700141202               when  wCountNote = 2;
124800141202                 eCM10no2 = CMNnot;
124900141202               when  wCountNote > 2;
125000141202                 oCM10piu = '+';
125100141202                 leave;
125200141202             endsl;
125300141202
125400141203           EndIf;
125500141028
125600141203           select;
125700141203             when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
125800141203               reade(N)  %kds( keyTICMN01 : 6 )  TICMN000;
125900141203             when  k_CMNdim <> *zero;
126000141203               reade(N)  %kds( keyTICMN01 : 5 )  TICMN000;
126100141203             other;
126200141203               reade(N)  %kds( keyTICMN01 : 4 )  TICMN000;
126300141203           endsl;
126400141028
126500141028         EndDo;
126600141024
126700141028       ENDSR;
126800141024
126900141028       //--------------------------------------------------------------
127000141028       //?Cancellazione Note indicate dal chiamante.                   ?
127100141028       //--------------------------------------------------------------
127200141028       BEGSR  sr_Delete;
127300141204
127400141204         clear  eCM10no1;
127500141204         clear  eCM10no2;
127600141204         //clear  eCM10dim;
127700141204         //clear  eCM10him;
127800141204         clear  eCM10fim;
127900141204         clear  eCM10pru;
128000141204         clear  eCM10flo;
128100141024
128200141204         // -?La cancellazione delle Note è gestibile anche per chiave parziale?
128300141204         //  ?(SE non ricevuti data e ora immissione: vengono cancellate?
128400141204         //  ?tutte le note inserite per la fase ricevuta)?
128500141028         clear  keyTICMN01;
128600141028         k_CMNncm = iCM10ncm;
128700141028         k_CMNksu = iCM10ksu;
128800141028         k_CMNksc = iCM10ksc;
128900141028         k_CMNcpo = iCM10cpo;
129000141202         k_CMNdim = eCM10dim;
129100141202         k_CMNhim = eCM10him;
129200141028         //k_CMNpno = ? ? ? ? ?;
129300141028
129400141202         setll  %kds( keyTICMN01 : 6 )  TICMN000;
129500141202         select;
129600141204           when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
129700141202             reade  %kds( keyTICMN01 : 6 )  TICMN000;
129800141204           when  k_CMNdim <> *zero;
129900141202             reade  %kds( keyTICMN01 : 5 )  TICMN000;
130000141202           other;
130100141202             reade  %kds( keyTICMN01 : 4 )  TICMN000;
130200141202         endsl;
130300141028
130400141028         DoW  Not  %eof( TICMN01L );
130500141204
130600141204           if  iCM10acm = *all'9'  or  iCM10acm = CMNacm;
130700141028
130800141204             //_______________
130900141204             delete  TICMN000;
131000141204             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
131100141204
131200141204           endif;
131300141028
131400141202           select;
131500141204             when  k_CMNhim <> *zero  and  k_CMNdim <> *zero;
131600141202               reade  %kds( keyTICMN01 : 6 )  TICMN000;
131700141204             when  k_CMNdim <> *zero;
131800141202               reade  %kds( keyTICMN01 : 5 )  TICMN000;
131900141202             other;
132000141202               reade  %kds( keyTICMN01 : 4 )  TICMN000;
132100141202           endsl;
132200141028
132300141028         EndDo;
132400141024
132500141028       ENDSR;
132600141028
132700141028       //--------------------------------------------------------------
132800141028       //?Controllo caratteri in una singola Nota.                   ?
132900141028       //--------------------------------------------------------------
133000141028       BEGSR  sr_ChkChar;
133100141028
133200141028         clear TxtInOut;
133300141028         TxtInOut = CMNnot;
133400141028         ChkNull  = '1';
133500141028
133600141028         xchkchar ( TxtInOut :
133700141028                    ElencoChar :
133800141028                    TipoElenco :
133900141028                    CharSost :
134000141028                    UpperCase :
134100141028                    ChkNull :
134200141028                    CharNull :
134300141028                    Esito );
134400141028
134500141028         select;
134600141028           // -?Nessuna modifica effettuata?
134700141028           when  Esito = *off;
134800141028           // -?Modifica effettuata?
134900141028           when  Esito = *on;
135000141028             CMNnot = TxtInOut;
135100141028           // -?Errore nei parametri di input?
135200141028           when  Esito = 'I';
135300141028             clear  CMNnot;
135400141028           // -?Errore di elaborazione?
135500141028           //when  Esito = 'E';
135600141028           //  clear  CMNnot;
135700141028         endsl;
135800141028
135900141028       ENDSR;
136000141024
136100141024       //--------------------------------------------------------------
136200141024       //?Operazioni finali.                                         ?
136300141024       //--------------------------------------------------------------
136400141024       BEGSR  sr_RoutEnd;
136500141028
136600141028         // -?Chiusura applicazioni precedentemente aperte?
136700141028         If  iCM10tla = 'C'  or  iCM10tla = 'L';
136800141028
136900141028           if  I69kac <> *zero;
137000141028             clear  TIBS69ds;
137100141028             I69tla = 'C';
137200141028             tibs69r( tibs69ds :
137300141028                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
137400141028           endif;
137500141028
137600141028         EndIf;
137700141024
137800141024         // -?Chiusura *pgm?
137900141024         return;
138000141024
138100141024       ENDSR;
138200141024
138300141024      /end-free
138400141028
138500141028** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
138600141104Reperite TROPPE note per la Campagna Commerciale (già 999)                      1
