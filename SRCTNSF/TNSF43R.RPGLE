000100120613       //==============================================================
000200120613       //?TNSF43R * Stampa Fattura: totali suddivisi per voce.         ?
000300120613       //==============================================================
000400120613
000500120613       //--------------------------------------------------------------
000600120613       //?Specifiche di controllo.                                     ?
000700120613       //--------------------------------------------------------------
000800120613
000900120613     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000120613     h dftactgrp(*no)
001100120614     h bnddir('TRUL')
001200120613
001300120613       //--------------------------------------------------------------
001400120613       //?Dichiarazione file.                                          ?
001500120613       //--------------------------------------------------------------
001600120613
001700120614       // -?Bolle di Sede?
001800120613     fTITAS40C  if   e           k disk    usropn
001900120614     f                                     extfile(wLibFile)
002000120613
002100120613       // -?Video parametri di lancio?
002200120613     fTNSF43D   cf   e             workstn
002300120613     f                                     indds(IndDspF)
002400120613     f                                     infds(InfDspF)
002500120613
002600120613       //--------------------------------------------------------------
002700120613       //?Definizione costanti.                                        ?
002800120613       //--------------------------------------------------------------
002900120613
003000120613       // -?Tasti funzionali a video?
003100120613     d c_F01           c                   const(x'31')
003200120613     d c_F02           c                   const(x'32')
003300120613     d c_F03           c                   const(x'33')
003400120613     d c_F04           c                   const(x'34')
003500120613     d c_F05           c                   const(x'35')
003600120613     d c_F06           c                   const(x'36')
003700120613     d c_F07           c                   const(x'37')
003800120613     d c_F08           c                   const(x'38')
003900120613     d c_F09           c                   const(x'39')
004000120613     d c_F10           c                   const(x'3A')
004100120613     d c_F11           c                   const(x'3B')
004200120613     d c_F12           c                   const(x'3C')
004300120613     d c_F13           c                   const(x'B1')
004400120613     d c_F14           c                   const(x'B2')
004500120613     d c_F15           c                   const(x'B3')
004600120613     d c_F16           c                   const(x'B4')
004700120613     d c_F17           c                   const(x'B5')
004800120613     d c_F18           c                   const(x'B6')
004900120613     d c_F19           c                   const(x'B7')
005000120613     d c_F20           c                   const(x'B8')
005100120613     d c_F21           c                   const(x'B9')
005200120613     d c_F22           c                   const(x'BA')
005300120613     d c_F23           c                   const(x'BB')
005400120613     d c_F24           c                   const(x'BC')
005500120613     d c_Enter         c                   const(x'F1')
005600120613     d c_RollDown      c                   const(x'F4')
005700120613     d c_RollUp        c                   const(x'F5')
005800120613
005900120613       //--------------------------------------------------------------
006000120613       //?Definizione schiere.                                         ?
006100120613       //--------------------------------------------------------------
006200120613
006300120613       // -?Messaggi di errore?
006400120614     d $Msg            s             78    dim(13) ctdata perrcd(1)
006500120613
006600120613       //--------------------------------------------------------------
006700120613       //?Definizione aree dati.                                       ?
006800120613       //--------------------------------------------------------------
006900120613
007000120613       // -?Dati utente?
007100120613     d §AzUte        e ds                  extname(AZUTE00F)
007200120613     d                                     dtaara
007300120613     d §DatiUte      e ds                  extname(dDatiUte)
007400120613     d                                     dtaara
007500120613
007600120613       //--------------------------------------------------------------
007700120613       //?Definizione strutture dati.                                  ?
007800120613       //--------------------------------------------------------------
007900120613
008000120613       // -?Status ds?
008100120613     d Status         sds
008200120613     d   SDSpgm          *proc
008300120613
008400120613       // -?InfDS?
008500120613     d InfDspF         ds
008600120613     d   dsp_aid             369    369a
008700120613
008800120613       // -?Indicatori su DspF?
008900120613     d IndDspF         ds
009000120613         // -?Emissione messaggio di errore?
009100120613     d  ErrMessage                     n   overlay(IndDspF : 28)
009200120613         // -?Posizionamento cursore & Segnalazione errore?
009300120613     d  PosCurFIV                      n   overlay(IndDspF : 51)
009400120613     d  PosCurNFT                      n   overlay(IndDspF : 52)
009500120614     d  PosCurDFT                      n   overlay(IndDspF : 53)
009600120613     d  PosCurKSC                      n   overlay(IndDspF : 54)
009700120613         // -?Riemissione videata?
009800120613     d  ErrGenerico                    n   overlay(IndDspF : 99)
009900120614
010000120614       // -?Parametri ricevuti?
010100120614     d ds_Pog          ds                  inz
010200120614     d   $Pog                         3    inz   dim(250)
010300120613
010400120613       // -?Parametri ricevuti?
010500120613     d KPJBA         e ds
010600120613
010700120613       // -?Parametri per *pgm batch di estrazione?
010800120613     d TNSF44ds      e ds                  inz
010900120613
011000120613       //--------------------------------------------------------------
011100120613       //?Definizione variabili globali.                               ?
011200120613       //--------------------------------------------------------------
011300120613
011400120613       // -?Flags booleani?
011500120613     d $Fine           s               n   inz
011600120613     d $InzD01         s               n   inz(*on)
011700120613
011800120613       // -?Variabili per la gestione del video?
011900120613     d $Video          s              2    inz('D1')
012000120613
012100120706       // -?Campi di comodo?
012200120613     d wDate           s              8  0 inz
012300120614
012400120614       // -?Nome del sistema?
012500120614     d currSysNeta     s              8a   inz
012600120614
012700120614       // -?Nome della libreria in cui è il file?
012800120614     d wLibFile        s             21a   inz
012900120613
013000120613       //--------------------------------------------------------------
013100120613       //?Definizione procedure.                                       ?
013200120613       //--------------------------------------------------------------
013300120613
013400120613       // -?Reperimento dati utente?
013500120614     d wabi            s                   inz   like(UTEaut)
013600120614     d dLat          e ds                  inz
013700120614     d TIBS34ds      e ds                  inz
013800120614      /copy gaitrasrc/srcProtoPR,TIBS34R
013900120614
014000120614       // -?Reperimento filiali gestibili dall'utente?
014100120614     d TRUL31ds      e ds                  inz
014200120614      /copy gaitrasrc/srcProtoPR,TRUL31R
014300120614
014400120614       // -?Reperimento dati tabelle?
014500120614      /copy gaitrasrc/srcProtoPI,TRULTAB
014600120614      /copy gaitrasrc/srcProtoPR,TRULTAB
014700120613
014800120614       // -?Controllo/inversione date?
014900120614     d WLBdat          ds                  inz
015000120614     d   G08dat                1      8  0 inz
015100120614     d   G08inv                9     16  0 inz
015200120614     d   G08err               17     17    inz('3')
015300120614     d   G08tgi               18     22  0 inz
015400120613      /copy gaitrasrc/srcProtoPR,XSRDA8
015500120614
015600120614       // -?Controllo/Decodifica cliente?
015700120614      /copy gaitrasrc/srcProtoPI,TIBS69R
015800120614      /copy gaitrasrc/srcProtoPR,TIBS69R
015900120614
016000120614       // -?Reperimento NETA sistema AS/400 corrente?
016100120614      /copy gaitrasrc/srcProtoPR,UBRTVNETA
016200120613
016300120613       // -?Sottomissione lavoro batch?
016400120613      /copy gaitrasrc/srcProtoPR,BCH10
016500120613
016600120613       // -?Richiamo diretto lavoro batch?
016700120613     d tnsf44r         pr                  extpgm('TNSF44R')
016800120613     d   kpjba                             likeds(KPJBA)
016900120613
017000120613       // -?Parametri API QCAPCMD (Process Commands)?
017100120613     d Qcmd            s           2048    inz  varying
017200120613      /copy qSysInc/qRpgleSrc,QCAPCMD
017300120613       // -?API QCAPCMD (Process Commands)?
017400120613      /copy gaitrasrc/srcProtoPR,QCAPCMD
017500120613
017600120613       // -?Parametri gestione errori API.?
017700120613      /copy qsysinc/qrpglesrc,QUSEC
017800120613
017900120613       //--------------------------------------------------------------
018000120613       //?Definizione key-list.                                        ?
018100120613       //--------------------------------------------------------------
018200120613
018300120614       // -?File TITAS40C?
018400120614     d k05titas40    e ds                  extname(TITAS40C : *key)
018500120613     d                                     prefix(k_)   inz
018600120613
018700120613       //--------------------------------------------------------------
018800120613       //?M A I N - L I N E                                            ?
018900120613       //--------------------------------------------------------------
019000120613
019100120613     c     *Entry        plist
019200120613     c                   parm                    KPJBA
019300120613
019400120613      /free
019500120613
019600120613       // -?Operazioni iniziali?
019700120613       exsr  sr_RoutInz;
019800120613
019900120613       // -?Gestione videate?
020000120613       DoW  Not  $Fine;
020100120613         select;
020200120613           when  $Video = 'D1';
020300120613             exsr  sr_GesD01;
020400120613           other;
020500120613             $Fine = *on;
020600120613         endsl;
020700120613       EndDo;
020800120613
020900120613       // -?Operazioni finali?
021000120613       exsr  sr_RoutEnd;
021100120613
021200120613       //--------------------------------------------------------------
021300120613       //?Operazioni iniziali.                                         ?
021400120613       //--------------------------------------------------------------
021500120613       BEGSR  sr_RoutInz;
021600120613
021700120613         *inLR = *on;
021800120614
021900120614         // -?Verifica del sistema AS/400 corrente?
022000120614         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
022100120614           $Fine = *on;
022200120614           leavesr;
022300120614         endif;
022400120614
022500120614         // -?Apertura archivio TITAS40C?
022600120614         if  not %open(TITAS40C);
022700120614           if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
022800120614             wLibFile = 'GAITRAGRU/TITAS40C';
022900120614           else;
023000120614             wLibFile = 'GAITRAGRPS/TITAS40C';
023100120614           endif;
023200120614           open  TITAS40C;
023300120614         endif;
023400120613
023500120613         // -?Reperimento dati job?
023600120613         exsr  sr_DatiJob;
023700120613
023800120613         // -?Reperimento data odierna?
023900120613         wDate = %int( %subst( %char( %dec( %timestamp() ) )
024000120613                               : 1 : 8 ) );
024100120613
024200120613         // -?Impostazione dati di testata?
024300120613         V1Tpgm = SDSpgm;
024400120614
024500120613
024600120613       ENDSR;
024700120613
024800120613       //--------------------------------------------------------------
024900120613       //?Reperimento Dati del job (Utente/Operativi).                 ?
025000120613       //--------------------------------------------------------------
025100120613       BEGSR  sr_DatiJob;
025200120613
025300120613         in(E) §AzUte;
025400120613         if NOT %error;
025500120613           in(E) §DatiUte;
025600120613         endif;
025700120613         if %error or RSut = *blanks;
025800120613           clear TIBS34ds;
025900120613           tibs34r(tibs34ds);
026000120613           in §AzUte;
026100120613           in §DatiUte;
026200120613         endif;
026300120614
026400120614       ENDSR;
026500120613
026600120613       //--------------------------------------------------------------
026700120613       //?Gestione videata D01.                                        ?
026800120613       //--------------------------------------------------------------
026900120613       BEGSR  sr_GesD01;
027000120613
027100120613         // -?Inizializzazione videata?
027200120613         if  $InzD01;
027300120613           exsr  sr_InzD01;
027400120613           $InzD01 = *off;
027500120613         endif;
027600120613
027700120614         // -?Emissione Testata + Piede?
027800120614         write  SF43T01;
027900120614         write  SF43Z01;
028000120614         // -?Emissione/Lettura Videata di lancio?
028100120614         exfmt  SF43D01;
028200120613         ErrMessage   = *off;
028300120613         ErrGenerico  = *off;
028400120614         clear V1Dmsg;
028500120613
028600120613         Select;
028700120613
028800120613           // -?Rilevato errore grave: uscita dal *pgm?
028900120613           When  $Fine;
029000120613
029100120613           // -?F3=Fine?
029200120613           When  dsp_aid = c_F03;
029300120613             exsr  sr_F03D01;
029400120613
029500120613           // -?Invio / F6=Conferma?
029600120613           Other;
029700120613             exsr  sr_CtrD01;
029800120613             select;
029900120613               when  ErrGenerico;
030000120613                 leavesr;
030100120613               when  dsp_aid = c_F06;
030200120613                 exsr  sr_F06D01;
030300120613             endsl;
030400120613
030500120613         EndSl;
030600120613
030700120613       ENDSR;
030800120613
030900120613       //--------------------------------------------------------------
031000120613       //?Inizializzazione videata D01.                                ?
031100120613       //--------------------------------------------------------------
031200120613       BEGSR  sr_InzD01;
031300120613
031400120614         clear  SF43D01;
031500120614
031600120614         // -?Controllo abilitazioni utente?
031700120614         exsr  sr_AbilitUte;
031800120614         if  errGenerico;
031900120614           leavesr;
032000120614         endif;
032100120613
032200120613       ENDSR;
032300120614
032400120614       //--------------------------------------------------------------
032500120614       //?Controllo o Dati del job (Utente/Operativi).                 ?
032600120614       //--------------------------------------------------------------
032700120614       BEGSR  sr_AbilitUte;
032800120614
032900120614         clear  wAbi;
033000120614         clear  dLat;
033100120614
033200120614         // -?Verifica errori e autorità profilo?
033300120614         Select;
033400120614           // -?Se c'è errore nei dati utente => si esce dal *pgm?
033500120614           When  DUTerr = 'E';
033600120614             errGenerico = *on;
033700120614             errMessage  = *on;
033800120614             V1Dmsg = $Msg(01);
033900120614             $Fine  = *on;
034000120614             leavesr;
034100120614           // -?Se NON c'è l'abilitazione:?
034200120614           //  ?· se 1° livello => abilitazioni al terminal?
034300120614           //  ?· se 2° livello => abilitazioni al punto operativo?
034400120614           //  ?· se Sede (è impossibile ma se capita) => si manda a fine il *pgm?
034500120614           //            ?=> abilitazioni aziendali?
034600120614           When  UTEaut = *blank;
034700120614             select;
034800120614               when  DUTlpo = '1';
034900120614                 wAbi = 'TP';
035000120614               when  DUTlpo = '2';
035100120614                 wAbi = 'PO';
035200120614               when  DUTlpo = 'S';
035300120614                 wAbi = 'AZ';
035400120614                 //$Fine = *on;
035500120614                 //leavesr;
035600120614               endsl;
035700120614           // -?Caricamento abilitazioni del profilo?
035800120614           Other;
035900120614             wAbi = UteAut;
036000120614         EndSl;
036100120614
036200120614         // -?Controllo Abilitazione dell'utente?
036300120614         clear  dLat;
036400120614         ds_TNTBE.TBEke1 = wAbi;
036500120614         If  getTabella ( c_Tntbe : 'LAT' : ds_TNTBE.TBEke1 :
036600120614                          *omit : *omit : *omit :
036700120614                          *omit : *omit :
036800120614                          *omit : *omit : *omit : *omit :
036900120614                          *omit : *omit : *omit : *omit :
037000120614                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
037100120614                        = *zero      AND
037200120614             ds_TNTBE.TBEatb = *blank;
037300120614           dLat = ds_TNTBE.TBEuni;
037400120614         EndIf;
037500120614         // -?Errore o mancanza di Abilitazione dell'utente?
037600120614         if  §LATdes = *blank  or  §LATabi = 'S';
037700120614           errGenerico = *on;
037800120614           errMessage  = *on;
037900120614           V1Dmsg = $Msg(02);
038000120614           $Fine = *on;
038100120614           leavesr;
038200120614         endif;
038300120614
038400120614         // -?Reperimento delle filiali gestibili dall'utente?
038500120614         clear  TRUL31ds;
038600120614         I31abi = wAbi;
038700120614         I31cdi = DUTdis;
038800120614         I31car = DUTare;
038900120614         I31cpo = DUTpou;
039000120614         trul31r ( kpjba : TRUL31ds );
039100120614         if  O31pog > *zero;
039200120614           ds_Pog = O31pog;
039300120614         else;
039400120614           errGenerico = *on;
039500120614           errMessage  = *on;
039600120706           V1Dmsg = $Msg(03);
039700120614           $Fine = *on;
039800120614           leavesr;
039900120614         endif;
040000120614
040100120614       ENDSR;
040200120613
040300120613       //--------------------------------------------------------------
040400120613       //?Gestione tasto funzionale F3 da videata D01:                 ?
040500120613       //?F3=Fine                                                      ?
040600120613       //--------------------------------------------------------------
040700120613       BEGSR  sr_F03D01;
040800120613
040900120613         // -?Chiusura del *pgm?
041000120613         $Fine = *on;
041100120613
041200120613       ENDSR;
041300120613
041400120613       //--------------------------------------------------------------
041500120613       //?Gestione tasto funzionale F6 da videata D01:                 ?
041600120613       //?F6=Conferma                                                  ?
041700120613       //--------------------------------------------------------------
041800120613       BEGSR  sr_F06D01;
041900120613
042000120613         // -?Impostazione parametri?
042100120614         SF44fiv = V1Cfiv;
042200120614         SF44nft = V1Cnft;
042300120614         SF44ksc = V1Cksc;
042400120613
042500120614         kcoaz = 'SF44';
042600120614         kpjbu = TNSF44ds;
042700120613
042800120613         if  knmus = *all'1';
042900120613           // -?Richiamo interattivo?
043000120614           tnsf44r ( kpjba );
043100120613         else;
043200120613           // -?Sottomissione batch?
043300120613            bch10 ( kpjba );
043400120613         endif;
043500120613
043600120613         // -?Chiusura del *pgm?
043700120613         $Fine = *on;
043800120613
043900120613       ENDSR;
044000120613
044100120613       //--------------------------------------------------------------
044200120613       //?Controllo videata D01.                                       ?
044300120613       //--------------------------------------------------------------
044400120613       BEGSR  sr_CtrD01;
044500120613
044600120613         %subst(IndDspF : 50) = *off;
044700120613
044800120614         Select;
044900120614           // -?Controllo Libro IVA?
045000120614           when  V1Cfiv  = *zero;
045100120614             ErrGenerico = *on;
045200120614             ErrMessage  = *on;
045300120614             PosCurFIV   = *on;
045400120614             V1Dmsg = $Msg(04);
045500120614             leavesr;
045600120614           // -?Controllo Numero Fattura?
045700120614           when  V1Cnft <= *zero;
045800120614             ErrGenerico = *on;
045900120614             ErrMessage  = *on;
046000120614             PosCurNFT   = *on;
046100120614             V1Dmsg = $Msg(06);
046200120614             leavesr;
046300120614           // -?Controllo Data Fattura?
046400120614           when  V1Cdft  = *zero;
046500120614             ErrGenerico = *on;
046600120614             ErrMessage  = *on;
046700120614             PosCurDFT   = *on;
046800120614             V1Dmsg = $Msg(07);
046900120614             leavesr;
047000120614           Other;
047100120614             clear  WLBdat;
047200120614             G08dat = V1Cdft;
047300120614             xsrda8 ( WLBdat );
047400120614             if  G08err = *on;
047500120614               ErrGenerico = *on;
047600120614               ErrMessage  = *on;
047700120614               PosCurDFT   = *on;
047800120614               V1Dmsg = $Msg(08);
047900120614               leavesr;
048000120614             endif;
048100120614             V1Cdft  = G08dat;
048200120614             SF44dft = G08inv;
048300120614             if  SF44dft < %dec( %date( wDate : *iso ) - %months(13) );
048400120614               ErrGenerico = *on;
048500120614               ErrMessage  = *on;
048600120614               PosCurDFT   = *on;
048700120614               V1Dmsg = $Msg(09);
048800120614               leavesr;
048900120614             endif;
049000120614         EndSl;
049100120614
049200120614         // -?In caso di Distinta: obbligatorio il codice cliente?
049300120614         if  V1Cnft = *all'9'  and  V1Cksc = *zero;
049400120614           ErrGenerico = *on;
049500120614           ErrMessage  = *on;
049600120614           PosCurKSC   = *on;
049700120614           V1Dmsg = $Msg(10);
049800120614           leavesr;
049900120614         endif;
050000120614
050100120614         // -?Verifica esistenza fattura in TITAS?
050200120614         k_TASfiv = V1Cfiv;
050300120614         k_TASnft = V1Cnft;
050400120614         k_TASdft = SF44dft;
050500120614         k_TASksc = V1Cksc;
050600120614         if  V1Cnft <> *hival;
050700120614           chain  %kds( k05titas40 : 3 )  TITAS40C;
050800120614         else;
050900120614           chain  %kds( k05titas40 : 4 )  TITAS40C;
051000120614         endif;
051100120614         if  Not %found(TITAS40C);
051200120614           ErrGenerico = *on;
051300120614           ErrMessage  = *on;
051400120614           PosCurNFT   = *on;
051500120614           if  V1Cnft <> *hival;
051600120614             V1Dmsg = $Msg(11);
051700120614           else;
051800120614             V1Dmsg = $Msg(12);
051900120614           endif;
052000120614           leavesr;
052100120614         endif;
052200120614
052300120614         // -?Controllo Codice Cliente?
052400120614         clear  V1Dksc;
052500120705         clear  SF44rsc;
052600120614         If  V1Cksc > *zero;
052700120614           clear  TIBS69ds;
052800120614           I69sif = knsif;
052900120614           I69kcc = DUTkci;
053000120614           I69kac = V1Cksc;
053100120614           tibs69r( tibs69ds :
053200120614                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
053300120614           if  O69err = *on;
053400120614             V1Dmsg = $Msg(13);
053500120614             PosCurKSC   = *on;
053600120614             ErrMessage  = *on;
053700120614             ErrGenerico = *on;
053800120614             leavesr;
053900120614           endif;
054000120614           V1Dksc = %subst( ACOrag : 1 : %len(V1Dksc) );
054100120705           SF44rsc = ACOrag;
054200120614         EndIf;
054300120613
054400120613       ENDSR;
054500120613
054600120613       //--------------------------------------------------------------
054700120613       //?Esecuzione del comando (già impostato).                      ?
054800120613       //--------------------------------------------------------------
054900120613       BEGSR  sr_ExecCmd;
055000120613
055100120613         clear Qcap0100;
055200120613         Qcabcsdh = *off;
055300120613         Qcapa    = *off;
055400120613         Qcacmdss = *off;
055500120613         Qcaerved = *allX'00';
055600120613
055700120613         clear Qusec;
055800120613         Qusbprv  = %size(Qusec);
055900120613
056000120613         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
056100120613                           %size(Qcap0100) : 'CPOP0100' : *omit :
056200120613                           0 : 0 : Qusec);
056300120613
056400120613       ENDSR;
056500120613
056600120613       //--------------------------------------------------------------
056700120613       //?Stampa segnalazioni dell'errore rilevato.                    ?
056800120613       //--------------------------------------------------------------
056900120613       BEGSR  sr_PrintErr;
057000120613
057100120613         // -?Stampa Dump?
057200120613         Dump(A);
057300120613
057400120613         // -?Stampa Job-Log?
057500120613         Qcmd = 'DSPJOBLOG job(*) output(*print)';
057600120613         clear Qcap0100;
057700120613         Qcabcsdh = *off;
057800120613         Qcapa    = *off;
057900120613         Qcacmdss = *off;
058000120613         Qcaerved = *allX'00';
058100120613         clear Qusec;
058200120613         Qusbprv  = %size(Qusec);
058300120613         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
058400120613                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
058500120613                           0 : 0 : Qusec);
058600120613
058700120613       ENDSR;
058800120613
058900120613       //--------------------------------------------------------------
059000120613       //?Operazioni finali.                                           ?
059100120613       //--------------------------------------------------------------
059200120613       BEGSR  sr_RoutEnd;
059300120614
059400120614         cloTabella();
059500120614
059600120614         reset  TIBS69ds;
059700120614         tibs69r( tibs69ds :
059800120614                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
059900120613
060000120613         return;
060100120613
060200120613       ENDSR;
060300120613
060400120613      /end-free
060500120613
060600120613       //--------------------------------------------------------------
060700120613       //?Schiere a tempo di compilazione.                             ?
060800120613       //--------------------------------------------------------------
060900120613
061000120613** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
061100120614Errore nei dati utente                                                         1
061200120614Utente non abilitato alla richiesta di questa stampa                           2
061300120706Utente non abilitato alla richiesta della fattura di nessun cliente            3
061400120614Libro IVA errato                                                               4
061500120614Immettere il libro IVA                                                         5
061600120614Immettere un numero di Fattura  valido                                         6
061700120614Immettere la data fattura                                                      7
061800120614Data fattura errata                                                            8
061900120614Attenzione: fattura troppo vecchia, impossibile recuperare i dati              9
062000120614Codice cliente di fatturazione obbligatorio per le distinte                   10
062100120614Numero fattura non trovato                                                    11
062200120614Fattura non trovata per il cliente indicato                                   12
062300120614Codice cliente di fatturazione inesistente                                    13
