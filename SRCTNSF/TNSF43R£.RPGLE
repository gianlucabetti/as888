000100120613       //==============================================================
000200120613       //?TNSF43R * Stampa Fattura: totali suddivisi per voce.         ?
000300120613       //==============================================================
000400120613
000500120613       //--------------------------------------------------------------
000600120613       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120613       //--------------------------------------------------------------
000800120613
000900120614     /*PRM  dbgview(*source)
001000120614     /*CMD  ovrdbf file(TITAS40C) tofile(GAITRAGRPS/TITAS40C) +
001100120614     /*CMD         ovrscope(*calllvl)
001200120614     /*END  dltovr file(TITAS40C) lvl(*)
001300120613     /*END
001400120613
001500120613       //--------------------------------------------------------------
001600120613       //?Specifiche di controllo.                                     ?
001700120613       //--------------------------------------------------------------
001800120613
001900120613     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000120613     h dftactgrp(*no)
002100120614     h bnddir('TRUL')
002200120613
002300120613       //--------------------------------------------------------------
002400120613       //?Dichiarazione file.                                          ?
002500120613       //--------------------------------------------------------------
002600120613
002700120614       // -?Bolle di Sede?
002800120613     fTITAS40C  if   e           k disk    usropn
002900120614     f                                     extfile(wLibFile)
003000120613
003100120613       // -?Video parametri di lancio?
003200120613     fTNSF43D   cf   e             workstn
003300120613     f                                     indds(IndDspF)
003400120613     f                                     infds(InfDspF)
003500120613
003600120613       //--------------------------------------------------------------
003700120613       //?Definizione costanti.                                        ?
003800120613       //--------------------------------------------------------------
003900120613
004000120613       // -?Tasti funzionali a video?
004100120613     d c_F01           c                   const(x'31')
004200120613     d c_F02           c                   const(x'32')
004300120613     d c_F03           c                   const(x'33')
004400120613     d c_F04           c                   const(x'34')
004500120613     d c_F05           c                   const(x'35')
004600120613     d c_F06           c                   const(x'36')
004700120613     d c_F07           c                   const(x'37')
004800120613     d c_F08           c                   const(x'38')
004900120613     d c_F09           c                   const(x'39')
005000120613     d c_F10           c                   const(x'3A')
005100120613     d c_F11           c                   const(x'3B')
005200120613     d c_F12           c                   const(x'3C')
005300120613     d c_F13           c                   const(x'B1')
005400120613     d c_F14           c                   const(x'B2')
005500120613     d c_F15           c                   const(x'B3')
005600120613     d c_F16           c                   const(x'B4')
005700120613     d c_F17           c                   const(x'B5')
005800120613     d c_F18           c                   const(x'B6')
005900120613     d c_F19           c                   const(x'B7')
006000120613     d c_F20           c                   const(x'B8')
006100120613     d c_F21           c                   const(x'B9')
006200120613     d c_F22           c                   const(x'BA')
006300120613     d c_F23           c                   const(x'BB')
006400120613     d c_F24           c                   const(x'BC')
006500120613     d c_Enter         c                   const(x'F1')
006600120613     d c_RollDown      c                   const(x'F4')
006700120613     d c_RollUp        c                   const(x'F5')
006800120613
006900120613       //--------------------------------------------------------------
007000120613       //?Definizione schiere.                                         ?
007100120613       //--------------------------------------------------------------
007200120613
007300120613       // -?Messaggi di errore?
007400120614     d $Msg            s             78    dim(13) ctdata perrcd(1)
007500120613
007600120613       //--------------------------------------------------------------
007700120613       //?Definizione aree dati.                                       ?
007800120613       //--------------------------------------------------------------
007900120613
008000120613       // -?Dati utente?
008100120613     d §AzUte        e ds                  extname(AZUTE00F)
008200120613     d                                     dtaara
008300120613     d §DatiUte      e ds                  extname(dDatiUte)
008400120613     d                                     dtaara
008500120613
008600120613       //--------------------------------------------------------------
008700120613       //?Definizione strutture dati.                                  ?
008800120613       //--------------------------------------------------------------
008900120613
009000120613       // -?Status ds?
009100120613     d Status         sds
009200120613     d   SDSpgm          *proc
009300120613
009400120613       // -?InfDS?
009500120613     d InfDspF         ds
009600120613     d   dsp_aid             369    369a
009700120613
009800120613       // -?Indicatori su DspF?
009900120613     d IndDspF         ds
010000120613         // -?Emissione messaggio di errore?
010100120613     d  ErrMessage                     n   overlay(IndDspF : 28)
010200120613         // -?Posizionamento cursore & Segnalazione errore?
010300120613     d  PosCurFIV                      n   overlay(IndDspF : 51)
010400120613     d  PosCurNFT                      n   overlay(IndDspF : 52)
010500120614     d  PosCurDFT                      n   overlay(IndDspF : 53)
010600120613     d  PosCurKSC                      n   overlay(IndDspF : 54)
010700120613         // -?Riemissione videata?
010800120613     d  ErrGenerico                    n   overlay(IndDspF : 99)
010900120614
011000120614       // -?Parametri ricevuti?
011100120614     d ds_Pog          ds                  inz
011200120614     d   $Pog                         3    inz   dim(250)
011300120613
011400120613       // -?Parametri ricevuti?
011500120613     d KPJBA         e ds
011600120613
011700120613       // -?Parametri per *pgm batch di estrazione?
011800120613     d TNSF44ds      e ds                  inz
011900120614
012000120614       // -?Tabelle "QC"/2 = Altri dati x contabilità std?
012100120614     d dsQC2         e ds                  inz
012200120613
012300120613       //--------------------------------------------------------------
012400120613       //?Definizione variabili globali.                               ?
012500120613       //--------------------------------------------------------------
012600120613
012700120613       // -?Flags booleani?
012800120613     d $Fine           s               n   inz
012900120613     d $InzD01         s               n   inz(*on)
013000120613
013100120613       // -?Variabili per la gestione del video?
013200120613     d $Video          s              2    inz('D1')
013300120613
013400120706       // -?Campi di comodo?
013500120613     d wDate           s              8  0 inz
013600120614
013700120614       // -?Nome del sistema?
013800120614     d currSysNeta     s              8a   inz
013900120614
014000120614       // -?Nome della libreria in cui è il file?
014100120614     d wLibFile        s             21a   inz
014200120613
014300120613       //--------------------------------------------------------------
014400120613       //?Definizione procedure.                                       ?
014500120613       //--------------------------------------------------------------
014600120613
014700120613       // -?Reperimento dati utente?
014800120614     d wabi            s                   inz   like(UTEaut)
014900120614     d dLat          e ds                  inz
015000120614     d TIBS34ds      e ds                  inz
015100120614      /copy gaitrasrc/srcProtoPR,TIBS34R
015200120614
015300120614       // -?Reperimento filiali gestibili dall'utente?
015400120614     d TRUL31ds      e ds                  inz
015500120614      /copy gaitrasrc/srcProtoPR,TRUL31R
015600120614
015700120614       // -?Reperimento dati tabelle?
015800120614      /copy gaitrasrc/srcProtoPI,TRULTAB
015900120614      /copy gaitrasrc/srcProtoPR,TRULTAB
016000120613
016100120614       // -?Controllo/inversione date?
016200120614     d WLBdat          ds                  inz
016300120614     d   G08dat                1      8  0 inz
016400120614     d   G08inv                9     16  0 inz
016500120614     d   G08err               17     17    inz('3')
016600120614     d   G08tgi               18     22  0 inz
016700120613      /copy gaitrasrc/srcProtoPR,XSRDA8
016800120614
016900120614       // -?Controllo/Decodifica cliente?
017000120614      /copy gaitrasrc/srcProtoPI,TIBS69R
017100120614      /copy gaitrasrc/srcProtoPR,TIBS69R
017200120614
017300120614       // -?Reperimento NETA sistema AS/400 corrente?
017400120614      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017500120613
017600120613       // -?Sottomissione lavoro batch?
017700120613      /copy gaitrasrc/srcProtoPR,BCH10
017800120613
017900120613       // -?Richiamo diretto lavoro batch?
018000120613     d tnsf44r         pr                  extpgm('TNSF44R')
018100120613     d   kpjba                             likeds(KPJBA)
018200120613
018300120613       // -?Parametri API QCAPCMD (Process Commands)?
018400120613     d Qcmd            s           2048    inz  varying
018500120613      /copy qSysInc/qRpgleSrc,QCAPCMD
018600120613       // -?API QCAPCMD (Process Commands)?
018700120613      /copy gaitrasrc/srcProtoPR,QCAPCMD
018800120613
018900120613       // -?Parametri gestione errori API.?
019000120613      /copy qsysinc/qrpglesrc,QUSEC
019100120613
019200120613       //--------------------------------------------------------------
019300120613       //?Definizione key-list.                                        ?
019400120613       //--------------------------------------------------------------
019500120613
019600120614       // -?File TITAS40C?
019700120614     d k05titas40    e ds                  extname(TITAS40C : *key)
019800120613     d                                     prefix(k_)   inz
019900120613
020000120613       //--------------------------------------------------------------
020100120613       //?M A I N - L I N E                                            ?
020200120613       //--------------------------------------------------------------
020300120613
020400120613     c     *Entry        plist
020500120613     c                   parm                    KPJBA
020600120613
020700120613      /free
020800120613
020900120613       // -?Operazioni iniziali?
021000120613       exsr  sr_RoutInz;
021100120613
021200120613       // -?Gestione videate?
021300120613       DoW  Not  $Fine;
021400120613         select;
021500120613           when  $Video = 'D1';
021600120613             exsr  sr_GesD01;
021700120613           other;
021800120613             $Fine = *on;
021900120613         endsl;
022000120613       EndDo;
022100120613
022200120613       // -?Operazioni finali?
022300120613       exsr  sr_RoutEnd;
022400120613
022500120613       //--------------------------------------------------------------
022600120613       //?Operazioni iniziali.                                         ?
022700120613       //--------------------------------------------------------------
022800120613       BEGSR  sr_RoutInz;
022900120613
023000120613         *inLR = *on;
023100120614
023200120614         // -?Verifica del sistema AS/400 corrente?
023300120614         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
023400120614           $Fine = *on;
023500120614           leavesr;
023600120614         endif;
023700120614
023800120614         // -?Apertura archivio TITAS40C?
023900120614         if  not %open(TITAS40C);
024000120614           if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
024100120614             wLibFile = 'GAITRAGRU/TITAS40C';
024200120614           else;
024300120614             wLibFile = 'GAITRAGRPS/TITAS40C';
024400120614           endif;
024500120614           open  TITAS40C;
024600120614         endif;
024700120613
024800120613         // -?Reperimento dati job?
024900120613         exsr  sr_DatiJob;
025000120613
025100120613         // -?Reperimento data odierna?
025200120613         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025300120613                               : 1 : 8 ) );
025400120613
025500120613         // -?Impostazione dati di testata?
025600120613         V1Tpgm = SDSpgm;
025700120614
025800120613
025900120613       ENDSR;
026000120613
026100120613       //--------------------------------------------------------------
026200120613       //?Reperimento Dati del job (Utente/Operativi).                 ?
026300120613       //--------------------------------------------------------------
026400120613       BEGSR  sr_DatiJob;
026500120613
026600120613         in(E) §AzUte;
026700120613         if NOT %error;
026800120613           in(E) §DatiUte;
026900120613         endif;
027000120613         if %error or RSut = *blanks;
027100120613           clear TIBS34ds;
027200120613           tibs34r(tibs34ds);
027300120613           in §AzUte;
027400120613           in §DatiUte;
027500120613         endif;
027600120614
027700120614       ENDSR;
027800120613
027900120613       //--------------------------------------------------------------
028000120613       //?Gestione videata D01.                                        ?
028100120613       //--------------------------------------------------------------
028200120613       BEGSR  sr_GesD01;
028300120613
028400120613         // -?Inizializzazione videata?
028500120613         if  $InzD01;
028600120613           exsr  sr_InzD01;
028700120613           $InzD01 = *off;
028800120613         endif;
028900120613
029000120614         // -?Emissione Testata + Piede?
029100120614         write  SF43T01;
029200120614         write  SF43Z01;
029300120614         // -?Emissione/Lettura Videata di lancio?
029400120614         exfmt  SF43D01;
029500120613         ErrMessage   = *off;
029600120613         ErrGenerico  = *off;
029700120614         clear V1Dmsg;
029800120613
029900120613         Select;
030000120613
030100120613           // -?Rilevato errore grave: uscita dal *pgm?
030200120613           When  $Fine;
030300120613
030400120613           // -?F3=Fine?
030500120613           When  dsp_aid = c_F03;
030600120613             exsr  sr_F03D01;
030700120613
030800120613           // -?Invio / F6=Conferma?
030900120613           Other;
031000120613             exsr  sr_CtrD01;
031100120613             select;
031200120613               when  ErrGenerico;
031300120613                 leavesr;
031400120613               when  dsp_aid = c_F06;
031500120613                 exsr  sr_F06D01;
031600120613             endsl;
031700120613
031800120613         EndSl;
031900120613
032000120613       ENDSR;
032100120613
032200120613       //--------------------------------------------------------------
032300120613       //?Inizializzazione videata D01.                                ?
032400120613       //--------------------------------------------------------------
032500120613       BEGSR  sr_InzD01;
032600120613
032700120614         clear  SF43D01;
032800120614
032900120614         // -?Controllo abilitazioni utente?
033000120614         exsr  sr_AbilitUte;
033100120614         if  errGenerico;
033200120614           leavesr;
033300120614         endif;
033400120614
033500120614         // -?Reperimento Libri Iva?
033600120614         if  getTabella ( c_Tabel : 'QC ' : '2              ' :
033700120614                          *omit : *omit : *omit :
033800120614                          *omit : *omit :
033900120614                          *omit : *omit : *omit : *omit :
034000120614                          *omit : *omit : *omit : *omit :
034100120614                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
034200120614                        = *zero      AND
034300120614             ds_TNTBE.TBEatb = *blank;
034400120614           dsQC2 = ds_TNTBE.TBEuni;
034500120614           V1Dfiv = '(' + %editc( §QCfiv : 'X' ) + '=Iniz.Mese, '
034600120614                        + %editc( §QCfi2 : 'X' ) + '=Fine Mese)';
034700120614         endif;
034800120613
034900120613       ENDSR;
035000120614
035100120614       //--------------------------------------------------------------
035200120614       //?Controllo o Dati del job (Utente/Operativi).                 ?
035300120614       //--------------------------------------------------------------
035400120614       BEGSR  sr_AbilitUte;
035500120614
035600120614         clear  wAbi;
035700120614         clear  dLat;
035800120614
035900120614         // -?Verifica errori e autorità profilo?
036000120614         Select;
036100120614           // -?Se c'è errore nei dati utente => si esce dal *pgm?
036200120614           When  DUTerr = 'E';
036300120614             errGenerico = *on;
036400120614             errMessage  = *on;
036500120614             V1Dmsg = $Msg(01);
036600120614             $Fine  = *on;
036700120614             leavesr;
036800120614           // -?Se NON c'è l'abilitazione:?
036900120614           //  ?· se 1° livello => abilitazioni al terminal?
037000120614           //  ?· se 2° livello => abilitazioni al punto operativo?
037100120614           //  ?· se Sede (è impossibile ma se capita) => si manda a fine il *pgm?
037200120614           //            ?=> abilitazioni aziendali?
037300120614           When  UTEaut = *blank;
037400120614             select;
037500120614               when  DUTlpo = '1';
037600120614                 wAbi = 'TP';
037700120614               when  DUTlpo = '2';
037800120614                 wAbi = 'PO';
037900120614               when  DUTlpo = 'S';
038000120614                 wAbi = 'AZ';
038100120614                 //$Fine = *on;
038200120614                 //leavesr;
038300120614               endsl;
038400120614           // -?Caricamento abilitazioni del profilo?
038500120614           Other;
038600120614             wAbi = UteAut;
038700120614         EndSl;
038800120614
038900120614         // -?Controllo Abilitazione dell'utente?
039000120614         clear  dLat;
039100120614         ds_TNTBE.TBEke1 = wAbi;
039200120614         If  getTabella ( c_Tntbe : 'LAT' : ds_TNTBE.TBEke1 :
039300120614                          *omit : *omit : *omit :
039400120614                          *omit : *omit :
039500120614                          *omit : *omit : *omit : *omit :
039600120614                          *omit : *omit : *omit : *omit :
039700120614                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
039800120614                        = *zero      AND
039900120614             ds_TNTBE.TBEatb = *blank;
040000120614           dLat = ds_TNTBE.TBEuni;
040100120614           V1Dfiv = '(' + %editc( §QCfiv : 'X' ) + '=Iniz.Mese, '
040200120614                        + %editc( §QCfi2 : 'X' ) + '=Fine Mese)';
040300120614         EndIf;
040400120614         // -?Errore o mancanza di Abilitazione dell'utente?
040500120614         if  §LATdes = *blank  or  §LATabi = 'S';
040600120614           errGenerico = *on;
040700120614           errMessage  = *on;
040800120614           V1Dmsg = $Msg(02);
040900120614           $Fine = *on;
041000120614           leavesr;
041100120614         endif;
041200120614
041300120614         // -?Reperimento delle filiali gestibili dall'utente?
041400120614         clear  TRUL31ds;
041500120614         I31abi = wAbi;
041600120614         I31cdi = DUTdis;
041700120614         I31car = DUTare;
041800120614         I31cpo = DUTpou;
041900120614         trul31r ( kpjba : TRUL31ds );
042000120614         if  O31pog > *zero;
042100120614           ds_Pog = O31pog;
042200120614         else;
042300120614           errGenerico = *on;
042400120614           errMessage  = *on;
042500120706           V1Dmsg = $Msg(03);
042600120614           $Fine = *on;
042700120614           leavesr;
042800120614         endif;
042900120614
043000120614       ENDSR;
043100120613
043200120613       //--------------------------------------------------------------
043300120613       //?Gestione tasto funzionale F3 da videata D01:                 ?
043400120613       //?F3=Fine                                                      ?
043500120613       //--------------------------------------------------------------
043600120613       BEGSR  sr_F03D01;
043700120613
043800120613         // -?Chiusura del *pgm?
043900120613         $Fine = *on;
044000120613
044100120613       ENDSR;
044200120613
044300120613       //--------------------------------------------------------------
044400120613       //?Gestione tasto funzionale F6 da videata D01:                 ?
044500120613       //?F6=Conferma                                                  ?
044600120613       //--------------------------------------------------------------
044700120613       BEGSR  sr_F06D01;
044800120613
044900120613         // -?Impostazione parametri?
045000120614         //clear  TNSF44ds;           ?(già fatto)?
045100120614         SF44fiv = V1Cfiv;
045200120614         SF44nft = V1Cnft;
045300120614         //SF44dft = W1Cdft;          ?(già fatto)?
045400120614         SF44ksc = V1Cksc;
045500120705         //SF44rsc = ACOrag;          ?(già fatto)?
045600120613
045700120614         kcoaz = 'SF44';
045800120614         kpjbu = TNSF44ds;
045900120613
046000120613         if  knmus = *all'1';
046100120613           // -?Richiamo interattivo?
046200120614           tnsf44r ( kpjba );
046300120613         else;
046400120613           // -?Sottomissione batch?
046500120613            bch10 ( kpjba );
046600120613         endif;
046700120613
046800120613         // -?Chiusura del *pgm?
046900120613         $Fine = *on;
047000120613
047100120613       ENDSR;
047200120613
047300120613       //--------------------------------------------------------------
047400120613       //?Controllo videata D01.                                       ?
047500120613       //--------------------------------------------------------------
047600120613       BEGSR  sr_CtrD01;
047700120613
047800120613         %subst(IndDspF : 50) = *off;
047900120613
048000120614         Select;
048100120614           // -?Controllo Libro IVA?
048200120614           when  V1Cfiv  = *zero;
048300120614             ErrGenerico = *on;
048400120614             ErrMessage  = *on;
048500120614             PosCurFIV   = *on;
048600120614             V1Dmsg = $Msg(04);
048700120614             leavesr;
048800120614           when  V1Cfiv <> §QCfiv  and  V1Cfiv <> §QCfi2;
048900120614             ErrGenerico = *on;
049000120614             ErrMessage  = *on;
049100120614             PosCurFIV   = *on;
049200120614             V1Dmsg = $Msg(05);
049300120614             leavesr;
049400120614           // -?Controllo Numero Fattura?
049500120614           when  V1Cnft <= *zero;
049600120614             ErrGenerico = *on;
049700120614             ErrMessage  = *on;
049800120614             PosCurNFT   = *on;
049900120614             V1Dmsg = $Msg(06);
050000120614             leavesr;
050100120614           // -?Controllo Data Fattura?
050200120614           when  V1Cdft  = *zero;
050300120614             ErrGenerico = *on;
050400120614             ErrMessage  = *on;
050500120614             PosCurDFT   = *on;
050600120614             V1Dmsg = $Msg(07);
050700120614             leavesr;
050800120614           Other;
050900120614             clear  WLBdat;
051000120614             G08dat = V1Cdft;
051100120614             xsrda8 ( WLBdat );
051200120614             if  G08err = *on;
051300120614               ErrGenerico = *on;
051400120614               ErrMessage  = *on;
051500120614               PosCurDFT   = *on;
051600120614               V1Dmsg = $Msg(08);
051700120614               leavesr;
051800120614             endif;
051900120614             V1Cdft  = G08dat;
052000120614             SF44dft = G08inv;
052100120614             if  SF44dft < %dec( %date( wDate : *iso ) - %months(13) );
052200120614               ErrGenerico = *on;
052300120614               ErrMessage  = *on;
052400120614               PosCurDFT   = *on;
052500120614               V1Dmsg = $Msg(09);
052600120614               leavesr;
052700120614             endif;
052800120614         EndSl;
052900120614
053000120614         // -?In caso di Distinta: obbligatorio il codice cliente?
053100120614         if  V1Cnft = *all'9'  and  V1Cksc = *zero;
053200120614           ErrGenerico = *on;
053300120614           ErrMessage  = *on;
053400120614           PosCurKSC   = *on;
053500120614           V1Dmsg = $Msg(10);
053600120614           leavesr;
053700120614         endif;
053800120614
053900120614         // -?Verifica esistenza fattura in TITAS?
054000120614         k_TASfiv = V1Cfiv;
054100120614         k_TASnft = V1Cnft;
054200120614         k_TASdft = SF44dft;
054300120614         k_TASksc = V1Cksc;
054400120614         if  V1Cnft <> *hival;
054500120614           chain  %kds( k05titas40 : 3 )  TITAS40C;
054600120614         else;
054700120614           chain  %kds( k05titas40 : 4 )  TITAS40C;
054800120614         endif;
054900120614         if  Not %found(TITAS40C);
055000120614           ErrGenerico = *on;
055100120614           ErrMessage  = *on;
055200120614           PosCurNFT   = *on;
055300120614           if  V1Cnft <> *hival;
055400120614             V1Dmsg = $Msg(11);
055500120614           else;
055600120614             V1Dmsg = $Msg(12);
055700120614           endif;
055800120614           leavesr;
055900120614         endif;
056000120614
056100120614         // -?Controllo Codice Cliente?
056200120614         clear  V1Dksc;
056300120705         clear  SF44rsc;
056400120614         If  V1Cksc > *zero;
056500120614           clear  TIBS69ds;
056600120614           I69sif = knsif;
056700120614           I69kcc = DUTkci;
056800120614           I69kac = V1Cksc;
056900120614           tibs69r( tibs69ds :
057000120614                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
057100120614           if  O69err = *on;
057200120614             V1Dmsg = $Msg(13);
057300120614             PosCurKSC   = *on;
057400120614             ErrMessage  = *on;
057500120614             ErrGenerico = *on;
057600120614             leavesr;
057700120614           endif;
057800120614           V1Dksc = %subst( ACOrag : 1 : %len(V1Dksc) );
057900120705           SF44rsc = ACOrag;
058000120614         EndIf;
058100120613
058200120613       ENDSR;
058300120613
058400120613       //--------------------------------------------------------------
058500120613       //?Esecuzione del comando (già impostato).                      ?
058600120613       //--------------------------------------------------------------
058700120613       BEGSR  sr_ExecCmd;
058800120613
058900120613         clear Qcap0100;
059000120613         Qcabcsdh = *off;
059100120613         Qcapa    = *off;
059200120613         Qcacmdss = *off;
059300120613         Qcaerved = *allX'00';
059400120613
059500120613         clear Qusec;
059600120613         Qusbprv  = %size(Qusec);
059700120613
059800120613         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
059900120613                           %size(Qcap0100) : 'CPOP0100' : *omit :
060000120613                           0 : 0 : Qusec);
060100120613
060200120613         //if  Qusei <> *blank;
060300120613         //  ...
060400120613         //endif;
060500120613
060600120613       ENDSR;
060700120613
060800120613       //--------------------------------------------------------------
060900120613       //?Stampa segnalazioni dell'errore rilevato.                    ?
061000120613       //--------------------------------------------------------------
061100120613       BEGSR  sr_PrintErr;
061200120613
061300120613         // -?Stampa Dump?
061400120613         Dump(A);
061500120613
061600120613         // -?Stampa Job-Log?
061700120613         Qcmd = 'DSPJOBLOG job(*) output(*print)';
061800120613         clear Qcap0100;
061900120613         Qcabcsdh = *off;
062000120613         Qcapa    = *off;
062100120613         Qcacmdss = *off;
062200120613         Qcaerved = *allX'00';
062300120613         clear Qusec;
062400120613         Qusbprv  = %size(Qusec);
062500120613         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
062600120613                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
062700120613                           0 : 0 : Qusec);
062800120613
062900120613       ENDSR;
063000120613
063100120613       //--------------------------------------------------------------
063200120613       //?Operazioni finali.                                           ?
063300120613       //--------------------------------------------------------------
063400120613       BEGSR  sr_RoutEnd;
063500120614
063600120614         cloTabella();
063700120614
063800120614         reset  TIBS69ds;
063900120614         tibs69r( tibs69ds :
064000120614                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
064100120613
064200120613         return;
064300120613
064400120613       ENDSR;
064500120613
064600120613      /end-free
064700120613
064800120613       //--------------------------------------------------------------
064900120613       //?Schiere a tempo di compilazione.                             ?
065000120613       //--------------------------------------------------------------
065100120613
065200120613** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
065300120614Errore nei dati utente                                                         1
065400120614Utente non abilitato alla richiesta di questa stampa                           2
065500120706Utente non abilitato alla richiesta della fattura di nessun cliente            3
065600120614Libro IVA errato                                                               4
065700120614Immettere il libro IVA                                                         5
065800120614Immettere un numero di Fattura  valido                                         6
065900120614Immettere la data fattura                                                      7
066000120614Data fattura errata                                                            8
066100120614Attenzione: fattura troppo vecchia, impossibile recuperare i dati              9
066200120614Codice cliente di fatturazione obbligatorio per le distinte                   10
066300120614Numero fattura non trovato                                                    11
066400120614Fattura non trovata per il cliente indicato                                   12
066500120614Codice cliente di fatturazione inesistente                                    13
