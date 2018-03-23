000100100505     /*END
000200100505       //==============================================================
000300100505       //?TEST - WFile per verifica affidabilità C.L.N.                ?
000400100505       //==============================================================
000500100505
000600100505     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700100505     h dftactgrp(*no)
000800100505     h bnddir('TRUL')
000900100505
001000100505       //--------------------------------------------------------------
001100100505       //?Dichiarazione file.                                          ?
001200100505       //--------------------------------------------------------------
001300100505
001400100505       // -?Video?
001500100505     fTIS7CLN0D cf   e             workstn
001600100505     f                                     indds(IndDspF)
001700100505     f                                     infds(InfDspF)
001800100505
001900100505       //--------------------------------------------------------------
002000100505       //?Definizione costanti.                                        ?
002100100505       //--------------------------------------------------------------
002200100505
002300100505       // -?Tasti funzionali a video?
002400100505     d c_F01           c                   const(x'31')
002500100505     d c_F02           c                   const(x'32')
002600100505     d c_F03           c                   const(x'33')
002700100505     d c_F04           c                   const(x'34')
002800100505     d c_F05           c                   const(x'35')
002900100505     d c_F06           c                   const(x'36')
003000100505     d c_F07           c                   const(x'37')
003100100505     d c_F08           c                   const(x'38')
003200100505     d c_F09           c                   const(x'39')
003300100505     d c_F10           c                   const(x'3A')
003400100505     d c_F11           c                   const(x'3B')
003500100505     d c_F12           c                   const(x'3C')
003600100505     d c_F13           c                   const(x'B1')
003700100505     d c_F14           c                   const(x'B2')
003800100505     d c_F15           c                   const(x'B3')
003900100505     d c_F16           c                   const(x'B4')
004000100505     d c_F17           c                   const(x'B5')
004100100505     d c_F18           c                   const(x'B6')
004200100505     d c_F19           c                   const(x'B7')
004300100505     d c_F20           c                   const(x'B8')
004400100505     d c_F21           c                   const(x'B9')
004500100505     d c_F22           c                   const(x'BA')
004600100505     d c_F23           c                   const(x'BB')
004700100505     d c_F24           c                   const(x'BC')
004800100505     d c_Enter         c                   const(x'F1')
004900100505     d c_RollDown      c                   const(x'F4')
005000100505     d c_RollUp        c                   const(x'F5')
005100100505
005200100505       //--------------------------------------------------------------
005300100505       //?Definizione schiere.                                         ?
005400100505       //--------------------------------------------------------------
005500100505
005600100505
005700100505       //--------------------------------------------------------------
005800100505       //?Definizione aree dati.                                       ?
005900100505       //--------------------------------------------------------------
006000100505
006100100505       // -?Dati utente?
006200100505     d §AzUte        e ds                  extname(AZUTE00F)
006300100505     d                                     dtaara
006400100505     d §DatiUte      e ds                  extname(dDatiUte)
006500100505     d                                     dtaara
006600100505
006700100505       //--------------------------------------------------------------
006800100505       //?Definizione strutture dati.                                  ?
006900100505       //--------------------------------------------------------------
007000100505
007100100505       // -?Status ds?
007200100505     d Status         sds
007300100505     d  SDSpgm           *proc
007400100505     d  SDSprm           *parms
007500100505     d  SDSusr               254    263
007600100505
007700100505       // -?InfDS?
007800100505     d InfDspF         ds
007900100505     d  dsp_aid              369    369a
008000100505
008100100505       // -?Indicatori su DspF?
008200100505     d IndDspF         ds
008300100505        // -?Abilitazione tasti funzionali?
008400100505     d  AbilitF06                      n   overlay(IndDspF : 06)
008500100505     d  AbilitF12                      n   overlay(IndDspF : 12)
008600100505        // -?Indicatori di errore?
008700100505     d  ErrMessage                     n   overlay(IndDspF : 28)
008800100505     d   PosCurCCM                     n   overlay(IndDspF : 51)
008900100505     d   PosCurTBL                     n   overlay(IndDspF : 52)
009000100505     d   PosCurAMGi                    n   overlay(IndDspF : 53)
009100100505     d   PosCurAMGf                    n   overlay(IndDspF : 54)
009200100505     d  ErrGenerico                    n   overlay(IndDspF : 99)
009300100505
009400100505       // -?Parametri x TIS7CLN1R?
009500100505     d TIS7cln1ds      ds                  inz
009600100505     d   pS7ccm                       7  0 inz(0632910)
009700100505     d   pS7nrs                       2  0 inz(68)
009800100505     d   pS7tbl                       2    inz('F1')
009900100505     d   pS7amgI                      8  0 inz
010000100505     d   pS7amgF                      8  0 inz
010100100505
010200100505       // -?Parametri ricevuti?
010300100505     d KPJBA         e ds
010400100505
010500100505       // -?Parametri x Controllo profilo utenti?
010600100505     d TIBS34ds      e ds
010700100505
010800101119       // -?Tabella "TB" = Tipi bolla?
010900100505     d dsTB          e ds                  inz
011000101119       // -?Tabella "3C" = Invio dati -BOLLETTAZIONE-?
011100101119     d ds3C          e ds                  inz
011200101119       // -?Tabella "3EW" = Dati assegnati alla postazione EasyWeb?
011300101119     d d3EW          e ds                  inz
011400100505
011500100505       //--------------------------------------------------------------
011600100505       //?Definizione variabili globali.                               ?
011700100505       //--------------------------------------------------------------
011800100505
011900100505       // -?Flags booleani?
012000100505     d $Fine           s               n   inz(*off)
012100100505     d $InzD01         s               n   inz(*on)
012200100505     d $InzD02         s               n   inz(*on)
012300100505
012400100505       // -?Campi per la gestione del video?
012500100505     d $Video          s              2a   inz('D1')
012600101119
012700101119       // -?Campi per controllo variazioni?
012800101119     d SaveCCM         s                   like(V1Cccm)  inz(*hival)
012900100505
013000100505       //--------------------------------------------------------------
013100100505       //?Definizione prototipi procedure usate.                       ?
013200100505       //--------------------------------------------------------------
013300100505
013400100505       // -?Reperimento dati utente?
013500100505     d tibs34r         pr                  extpgm('TIBS34R')
013600100505     d   tibs34ds                          likeds(TIBS34ds)
013700100505
013800100505       // -?WFile per verifica affidabilità C.L.N.?
013900100505     d tis7cln1r       pr                  extpgm('TIS7CLN1R')
014000100505     d   kpjba                             likeds(KPJBA)
014100100505
014200100505       // -?Reperimento dati tabelle?
014300100505      /copy gaitrasrc/srcProtoPR,TRULTAB
014400100505
014500100505       // -?Controllo/Decodifica cliente?
014600100505      /copy gaitrasrc/srcProtoPI,TIBS69R
014700100505      /copy gaitrasrc/srcProtoPR,TIBS69R
014800100505
014900100505       //--------------------------------------------------------------
015000100505       //?Definizione key-list.                                        ?
015100100505       //--------------------------------------------------------------
015200100505
015300100505
015400100505       //--------------------------------------------------------------
015500100505       //?Riepilogo indicatori.                                        ?
015600100505       //--------------------------------------------------------------
015700100505
015800100505
015900100505       //--------------------------------------------------------------
016000100505       //?M A I N - L I N E                                            ?
016100100505       //--------------------------------------------------------------
016200100505
016300100505     c     *Entry        plist
016400100505     c                   parm                    KPJBA
016500100505
016600100505      /free
016700100505
016800100505       // -?Start?
016900100505       *inLR = *on;
017000100505
017100100505       // -?Elab?
017200100505       dow  $Fine = *off;
017300100505         select;
017400100505           when  $Video = 'D1';
017500100505             exsr  sr_GesD01;
017600100505           when  $Video = 'D2';
017700100505             exsr  sr_GesD02;
017800100505           other;
017900100505             $Fine = *on;
018000100505         endsl;
018100100505       enddo;
018200100505
018300100505       // -?End?
018400100505       reset  TIBS69ds;
018500100505       //I69tla = 'C';           ?(già così)?
018600100505       tibs69r( tibs69ds :
018700100505                ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
018800100505       cloTabella ( '1' );
018900100505       return;
019000100505
019100100505       //--------------------------------------------------------------
019200100505       //?Operazioni iniziali                                          ?
019300100505       //--------------------------------------------------------------
019400100505       BEGSR  *InzSR;
019500100505
019600100505         // -?Reperimento dati utente?
019700100505         exsr  sr_DatiJob;
019800100505
019900100505         // -?Impostazione nome programma in testata a video?
020000100505         V1Tpgm = SDSpgm;
020100100505
020200100505       ENDSR;
020300100505
020400100505       //--------------------------------------------------------------
020500100505       //?Reperimento Dati del job (Utente/Operativi).                 ?
020600100505       //--------------------------------------------------------------
020700100505       BEGSR  sr_DatiJob;
020800100505
020900100505         in(E) §AzUte;
021000100505         if NOT %error;
021100100505           in(E)  §DatiUte;
021200100505         endif;
021300100505         if %error or RSut = *blanks;
021400100505           clear  TIBS34ds;
021500100505           tibs34r(tibs34ds);
021600100505           in §AzUte;
021700100505           in §DatiUte;
021800100505         endif;
021900100505
022000100505       ENDSR;
022100100505
022200100505       //--------------------------------------------------------------
022300100505       //?Gestione videata D01                                         ?
022400100505       //--------------------------------------------------------------
022500100505       BEGSR  sr_GesD01;
022600100505
022700100505         // -?Inizializzazione videata?
022800100505         if  $InzD01 = *on;
022900100505           clear  S7clnD1;
023000100505           reset  TIS7cln1ds;
023100100505           V1Cccm  = pS7ccm;
023200100505           V1Cnrs  = pS7nrs;
023300100505           V1Ctbl  = pS7tbl;
023400100505           V1CamgI = pS7amgI;
023500100505           V1CamgF = pS7amgF;
023600101119           SaveCCM = V1Cccm;
023700100505           $InzD01 = *off;
023800100505         endif;
023900100505         AbilitF06 = *on;
024000100505         AbilitF12 = *off;
024100100505
024200100505         // -?Emissione videata?
024300100505         if  ErrMessage = *off;
024400100505           write  S7clnT1;
024500100505           write  S7clnZ1;
024600100505         endif;
024700100505
024800100505         exfmt  S7clnD1;
024900100505
025000100505         reset  ErrMessage;
025100100505         reset  ErrGenerico;
025200100505         clear  V1Dmsg;
025300100505
025400100505         SELECT;
025500100505
025600100505           // -?F3=Fine?
025700100505           WHEN  dsp_aid = c_F03;
025800100505             $Fine = *on;
025900100505
026000100505           // -?Invio o F6=Conferma?
026100100505           OTHER;
026200100505             exsr  sr_CtrD01;
026300100505             select;
026400100505               // -?rilevato errore?
026500100505               when  ErrGenerico = *on;
026600100505               // -?non premuto F6?
026700100505               when  dsp_aid <> c_F06;
026800100505               // -?premuto F6 ?
026900100505               other;
027000100505                 $Video = 'D2';
027100100505                 reset  $InzD02;
027200100505             endsl;
027300100505
027400100505         ENDSL;
027500100505
027600100505       ENDSR;
027700100505
027800100505       //--------------------------------------------------------------
027900100505       //?Controllo dati in videata D01                                ?
028000100505       //--------------------------------------------------------------
028100100505       BEGSR  sr_CtrD01;
028200100505
028300100505         %subst(IndDspF : 28) = *off;
028400100505
028500100505         clear  V1Dccm;
028600100505         // -?Codice cliente mittente obbligatorio?
028700100505         if  V1Cccm <= *zero;
028800100505           V1Dmsg = 'Inserire un codice cliente';
028900100505           PosCurCCM   = *on;
029000100505           errMessage  = *on;
029100100505           errGenerico = *on;
029200100505           leavesr;
029300100505         endif;
029400100505         // -?Controllo / decodifica cliente mittente?
029500100505         clear  TIBS69ds;
029600100505         I69sif = knsif;
029700100505         I69kcc = DUTkci;
029800100505         I69kac = V1Cccm;
029900100505         tibs69r( tibs69ds :
030000100505                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
030100100505         if  O69err = *on;
030200100505           V1Dmsg = 'Cliente NON presente in anagrafica';
030300100505           PosCurCCM   = *on;
030400100505           ErrMessage  = *on;
030500100505           ErrGenerico = *on;
030600100505           leavesr;
030700100505         endif;
030800100505         V1Dccm = ACOrag;
030900101119
031000101119         // -?Reperimento automatico del Numero Serie?
031100101119         //  ?alla variazione del cliente?
031200101119         if  V1Cccm <> SaveCCM;
031300101119           clear  ds3C;
031400101119           clear  d3EW;
031500101119           if  getTabella ( '1'   : '3C'  : %editc(V1Cccm:'X') :
031600101119                            *omit : *omit : *omit :
031700101119                            *omit : *omit :
031800101119                            *omit : *omit : *omit : *omit :
031900101119                            *omit : *omit : *omit : *omit :
032000101119                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
032100101119                          = *zero       AND
032200101119               ds_TNTBE.TBEatb = *blank;
032300101119             ds3C = ds_TNTBE.TBEuni;
032400101119             V1Cnrs = §3Cnrs;
032500101119           endif;
032600101119           if  §3Ccba = 'ESWEB'         AND
032700101119               getTabella ( '2'   : '3EW' : '0' + %editc(§3Ccks:'X') :
032800101119                            *omit : *omit : *omit :
032900101119                            *omit : *omit :
033000101119                            *omit : *omit : *omit : *omit :
033100101119                            *omit : *omit : *omit : *omit :
033200101119                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
033300101119                          = *zero       AND
033400101119               ds_TNTBE.TBEatb = *blank;
033500101119             d3EW = ds_TNTBE.TBEuni;
033600101119             V1Cnrs = §3EWnrs;
033700101119           endif;
033800101119           SaveCCM = V1Cccm;
033900101119         endif;
034000100505
034100100505         clear  V1Dtbl;
034200100505         // -?Codice tipo bolla obbligatorio?
034300100505         if  V1Ctbl = *blank;
034400100505           V1Dmsg = 'Inserire un tipo bolla';
034500101119           PosCurTBL   = *on;
034600100505           errMessage  = *on;
034700100505           errGenerico = *on;
034800100505           leavesr;
034900100505         endif;
035000100505         // -?Controllo / decodifica tipo bolla?
035100100505         ds_TNTBE.TBEke1 = V1Ctbl;
035200100505         if  getTabella ( '1'   : 'TB'  : ds_TNTBE.TBEke1 :
035300100505                          *omit : *omit : *omit :
035400100505                          *omit : *omit :
035500100505                          *omit : *omit : *omit : *omit :
035600100505                          *omit : *omit : *omit : *omit :
035700100505                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
035800100505                        = *zero      AND
035900100505             ds_TNTBE.TBEatb = *blank;
036000100505           dsTB   = ds_TNTBE.TBEuni;
036100100505           V1Dtbl = §TBdes;
036200100505         else;
036300100505           V1Dtbl = *all'? ';
036400100505           V1Dmsg = 'Tibo bolla non presente in tab. "TB"';
036500100505           PosCurTBL   = *on;
036600100505           ErrMessage  = *on;
036700100505           ErrGenerico = *on;
036800100505           leavesr;
036900100505         endif;
037000100505
037100100505         // -?Controllo data iniziale?
037200100505         if  V1CamgI <= *zero;
037300100505           V1Dmsg = 'Inserire una data iniziale';
037400100505           PosCurAMGi  = *on;
037500100505           errMessage  = *on;
037600100505           errGenerico = *on;
037700100505           leavesr;
037800100505         endif;
037900100505
038000100505         // -?Controllo data finalele?
038100100505         if  V1CamgF <= *zero;
038200100505           V1Dmsg = 'Inserire una data finale';
038300100505           PosCurAMGf  = *on;
038400100505           errMessage  = *on;
038500100505           errGenerico = *on;
038600100505           leavesr;
038700100505         endif;
038800100505
038900100505         // -?Controllo range di date?
039000100505         if  V1CamgI > V1CamgF;
039100100505           V1Dmsg = 'Data Iniziale > Data finale';
039200100505           PosCurAMGi  = *on;
039300100505           errMessage  = *on;
039400100505           errGenerico = *on;
039500100505           leavesr;
039600100505         endif;
039700100505
039800100505       ENDSR;
039900100505
040000100505       //--------------------------------------------------------------
040100100505       //?Gestione videata D02                                         ?
040200100505       //--------------------------------------------------------------
040300100505       BEGSR  sr_GesD02;
040400100505
040500100505         // -?Inizializzazione videata?
040600100505         if  $InzD02 = *on;
040700100505           //clear S7clnD2;
040800100505           exsr  Call_TIS7cln1R;
040900100505           $InzD02  = *off;
041000100505           AbilitF06  = *off;
041100100505           AbilitF12  = *on;
041200100505         endif;
041300100505
041400100505         // -?Emissione videata?
041500100505         if  ErrMessage = *off;
041600100505           write  S7clnT1;
041700100505           write  S7clnZ1;
041800100505           write  S7clnD1;
041900100505           write  Protect;
042000100505         endif;
042100100505
042200100505         exfmt  S7clnD2;
042300100505
042400100505         reset  ErrMessage;
042500100505         reset  ErrGenerico;
042600100505         clear  V1Dmsg;
042700100505
042800100505         SELECT;
042900100505
043000100505           // -?F3=Fine?
043100100505           WHEN  dsp_aid = c_F03;
043200100505             $Fine = *on;
043300100505
043400100505           // -?F12=Ritorno?
043500100505           WHEN  dsp_aid = c_F12;
043600100505             $Video = 'D1';
043700100505             //reset $InzD01;
043800100505             reset $InzD02;
043900100505
044000100505           // -?Invio?
044100100505           OTHER;
044200100505
044300100505         ENDSL;
044400100505
044500100505       ENDSR;
044600100505
044700100505       //--------------------------------------------------------------
044800100505       //?Richiamo Service Program UB§3EWUPD                           ?
044900100505       //--------------------------------------------------------------
045000100505       BEGSR  Call_TIS7cln1R;
045100100505
045200100505         pS7ccm  = V1Cccm;
045300100505         pS7nrs  = V1Cnrs;
045400100505         pS7tbl  = V1Ctbl;
045500100505         pS7amgI = V1CamgI;
045600100505         pS7amgF = V1CamgF;
045700100505
045800100505         kpjbu = TIS7cln1ds;
045900100505
046000100505         tis7cln1r ( kpjba );
046100100505
046200100505       ENDSR;
046300100505
046400100505      /end-free
