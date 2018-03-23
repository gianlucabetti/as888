000100160816       //==============================================================
000200160817       //?FIDN78R * Re-Invio e-mail / Ri-Stampa  Preavviso Danno EEX.  ?
000300160816       //==============================================================
000400160816
000500160816       //--------------------------------------------------------------
000600160816       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160816       //--------------------------------------------------------------
000800160816
000900160816     /*PRM  dbgview(*source)
001000160830     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
001100160830     /*CMD         ovrscope(*calllvl)
001200160830     /*END  dltovr file(FNARB01L) lvl(*)
001300160816     /*END
001400160816
001500160816       //--------------------------------------------------------------
001600160816       //?Specifiche di controllo.                                     ?
001700160816       //--------------------------------------------------------------
001800160816
001900160816     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000160817     h dftactgrp(*no)
002100160818     h bnddir('TRUL':'UBBNDDIR')
002200160816
002300160816       //--------------------------------------------------------------
002400160816       //?Dichiarazione file.                                          ?
002500160816       //--------------------------------------------------------------
002600160816
002700160816       // -?DANNI: Testata C.A.?
002800160816     fFNDCT01L  if   e           k disk
002900160816       // -?DANNI: Fasi C.A.?
003000160816     fFNDCF01L  if   e           k disk
003100160817
003200160818       // -?BOLLE ARRIVI: Testata?
003300160818     fFNARB01L  if   e           k disk     extfile(wLibFile)
003400160818     f                                      usropn
003500160816
003600160816       // -?Video Gestione?
003700160816     fFIDN78D   cf   e             workstn
003800160816     f                                     indds( IndDspF )
003900160816     f                                     infds( InfDspF )
004000160816
004100160816       //--------------------------------------------------------------
004200160816       //?Definizione costanti.                                        ?
004300160816       //--------------------------------------------------------------
004400160816
004500160816       // -?Fasi fissate a *pgm?
004600160816     d c_FCA_Conf      c                   const(100)
004700160817
004800160817       // -?Codice Sede?
004900160817     d c_Sede          c                   const(046)
005000160818
005100160818       // -?Comandi di override ai *file FNBLP01L e FNARB01L?
005200160818       //  ?per i *pgm chiamati?
005300160818     d c_CmdOvr_BLP    c                   const('OVRDBF +
005400160818     d                                           file(FNBLP01L) +
005500160818     d                                           ovrscope(*calllvl) +
005600160818     d                                           tofile(')
005700160818     d c_CmdOvr_ARB    c                   const('OVRDBF +
005800160818     d                                           file(FNARB01L) +
005900160818     d                                           ovrscope(*calllvl) +
006000160818     d                                           tofile(')
006100160818     d c_CmdDltOvr     c                   const('DLTOVR +
006200160818     d                                            file(FNBLP01L FNARB01L) +
006300160818     d                                            lvl(*)')
006400160816
006500160816       // -?Tasti funzionali a video?
006600160816     d c_F01           c                   const(x'31')
006700160816     d c_F02           c                   const(x'32')
006800160816     d c_F03           c                   const(x'33')
006900160816     d c_F04           c                   const(x'34')
007000160816     d c_F05           c                   const(x'35')
007100160816     d c_F06           c                   const(x'36')
007200160816     d c_F07           c                   const(x'37')
007300160816     d c_F08           c                   const(x'38')
007400160816     d c_F09           c                   const(x'39')
007500160816     d c_F10           c                   const(x'3A')
007600160816     d c_F11           c                   const(x'3B')
007700160816     d c_F12           c                   const(x'3C')
007800160816     d c_F13           c                   const(x'B1')
007900160816     d c_F14           c                   const(x'B2')
008000160816     d c_F15           c                   const(x'B3')
008100160816     d c_F16           c                   const(x'B4')
008200160816     d c_F17           c                   const(x'B5')
008300160816     d c_F18           c                   const(x'B6')
008400160816     d c_F19           c                   const(x'B7')
008500160816     d c_F20           c                   const(x'B8')
008600160816     d c_F21           c                   const(x'B9')
008700160816     d c_F22           c                   const(x'BA')
008800160816     d c_F23           c                   const(x'BB')
008900160816     d c_F24           c                   const(x'BC')
009000160816     d c_Enter         c                   const(x'F1')
009100160816     d c_RollDown      c                   const(x'F4')
009200160816     d c_RollUp        c                   const(x'F5')
009300160816
009400160816       //--------------------------------------------------------------
009500160816       //?Definizione schiere.                                         ?
009600160816       //--------------------------------------------------------------
009700160816
009800160816       // -?Messaggi di errore?
009900160830     d sk_Msg          s             78    dim( 9)  ctdata  perrcd(1)
010000160816
010100160816       //--------------------------------------------------------------
010200160816       //?Definizione aree dati.                                       ?
010300160816       //--------------------------------------------------------------
010400160816
010500160816       // -?Dati utente?
010600160816     d §AzUte        e ds                  extname(AZUTE00F)
010700160816     d                                     dtaara
010800160816     d §DatiUte      e ds                  extname(dDatiUte)
010900160816     d                                     dtaara
011000160816
011100160816       //--------------------------------------------------------------
011200160816       //?Definizione strutture dati.                                  ?
011300160816       //--------------------------------------------------------------
011400160816
011500160816       // -?Status ds?
011600160816     d Status         sds
011700160816     d   SDSpgm          *proc
011800160816     d*//SDSprm          *parms
011900160816     d*//SDSdta              191    198
012000160816     d*//SDSjob              244    253
012100160816     d*//SDSusr              254    263
012200160816
012300160816       // -?InfDS?
012400160816     d InfDspF         ds
012500160816     d   dsp_aid             369    369a
012600160816
012700160816       // -?Indicatori su DspF?
012800160816     d IndDspF         ds                  inz
012900160816         // -?Abilitazione tasti funzionali?
013000160816     d   $F10Attivo                    n   overlay( IndDspF : 10 )
013100160816     d   $F14Attivo                    n   overlay( IndDspF : 14 )
013200160816         // ·?Emissione messaggio di errore?
013300160816     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013400160816         // ·?Indicatori per Attibuti di visualizzazione?
013500160816         // ·?Posizionamento cursore & Segnalazione errore?
013600160816     d   $PosCurFIL                    n   overlay( IndDspF : 51 )
013700160816     d   $PosCurNCA                    n   overlay( IndDspF : 52 )
013800160816     d   $PosCurAAC                    n   overlay( IndDspF : 53 )
013900160816         // ·?Riemissione videata?
014000160816     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
014100160816
014200160816       // -?Numero C.A.?
014300160816     d DS_numCA        ds                  inz   qualified
014400160817     d   aac                               inz   like(I00aac)
014500160817     d   fil                               inz   like(I00fil)
014600160817     d   nca                               inz   like(I00nca)
014700160816
014800160816       // -?Parametri ricevuti?
014900160816     d KPJBA         e ds
015000160816
015100160816       // -?DS Gestionale: divise Danni?
015200160816     d dGEDdn        e ds                  inz   qualified
015300160816       // -?DS Standard Danni?
015400160816     d dSTD          e ds                  inz   qualified
015500160816
015600160816       // -?Flag operativi in FNDCT?
015700160816     d dDCT01        e ds                  inz   qualified
015800160817
015900160830       // -?Flag operativi in FNDCF?
016000160830     d dDCF01        e ds                  inz   qualified
016100160816
016200160816       //--------------------------------------------------------------
016300160816       //?Definizione variabili globali.                               ?
016400160816       //--------------------------------------------------------------
016500160816
016600160816       // -?Flags booleani?
016700160816     d $Fine           s               n   inz
016800160816     d $InzD01         s               n   inz(*on)
016900160818     d $Err            s               n   inz
017000160818     d*// $EEX_Mail       s               n   inz
017100160816
017200160816       // -?Variabili per la gestione del video?
017300160816     d $Video          s              2    inz('D1')
017400160816
017500160816       // -?Campi di Comodo?
017600160817     d SaveDCFdfc      s                   like(DCFdfc)  inz
017700160818
017800160830       // -?Nome esteso Libreria/File di FNARB01L (s.i. di filiale)?
017900160818     d wLibr           s             10a   inz
018000160818     d wLibFile        s             21a   inz
018100160816
018200160816       //--------------------------------------------------------------
018300160816       //?Definizione prototipi procedure usate.                       ?
018400160816       //--------------------------------------------------------------
018500160818
018600160818       // -?Nome del sistema?
018700160818     d currSysNeta     s              8a   inz
018800160818       // -?Reperimento NETA sistema AS/400 corrente?
018900160818      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019000160816
019100160816       // -?Reperimento dati utente?
019200160816     d TIBS34ds      e ds                  inz
019300160816      /copy gaitrasrc/srcProtoPR,TIBS34R
019400160816
019500160816       // -?Reperimento dati tabelle?
019600160816      /copy gaitrasrc/srcProtoPI,TRULTAB
019700160816      /copy gaitrasrc/srcProtoPR,TRULTAB
019800160816
019900160816       // -?Controllo/Inversione Date?
020000160816     d WLBdat          ds                  inz
020100160816     d   G08dat                       8  0 inz
020200160816     d   G08inv                       8  0 inz
020300160816     d   G08err                       1    inz('3')
020400160816     d   G08tgi                       5  0 inz
020500160816      /copy gaitrasrc/srcProtoPR,XSRDA8
020600160816
020700160817       // -?Interrogazione C.A.?
020800160816     d FIDN00ds      e ds                  inz
020900160816     d fidn01r         pr                  extpgm('FIDN01R')
021000160816     d   kpjba                             likeds(KPJBA)
021100160817       //  ?Stampa/Invio e-mail con Preavviso Danno EEX?
021200160816     d fidn39r         pr                  extpgm('FIDN39R')
021300160816     d   kpjba                             likeds(KPJBA)
021400160816     d fidn41r         pr                  extpgm('FIDN41R')
021500160816     d   kpjba                             likeds(KPJBA)
021600160816     d fidn68r         pr                  extpgm('FIDN68R')
021700160816     d   kpjba                             likeds(KPJBA)
021800160818
021900160818       // -?API QCAPCMD (Process Commands)?
022000160818     d Qcmd            s           2048    inz  varying
022100160818      /copy qSysInc/qRpgleSrc,QCAPCMD
022200160818      /copy gaitrasrc/srcProtoPR,QCAPCMD
022300160818
022400160818       // -?Parametri gestione errori API.?
022500160818      /copy qsysinc/qrpglesrc,QUSEC
022600160816
022700160816       //--------------------------------------------------------------
022800160816       //?Definizione key-list.                                        ?
022900160816       //--------------------------------------------------------------
023000160816
023100160816       // -?File FNDCT01L?
023200160816     d keyFNDCT01    e ds                  extname( FNDCT01L : *key )
023300160816     d                                     prefix(k_)   inz
023400160816       // -?File FNDCF01L?
023500160816     d keyFNDCF01    e ds                  extname( FNDCF01L : *key )
023600160816     d                                     prefix(k_)   inz
023700160817
023800160817       // -?File FNBLP01L / FNARB01L?
023900160817     d keyFNBLPARB   e ds                  extname( FNARB01L : *key )
024000160817     d                                     prefix( k_ARB : 3 ) inz
024100160816
024200160816       //--------------------------------------------------------------
024300160816       //?Fasi C.A. FISSATE a programma.                               ?
024400160816       //--------------------------------------------------------------
024500160818       // 100 = Conferma C.A.                                c_FCA_Conf
024600160816       //--------------------------------------------------------------
024700160816
024800160816       //--------------------------------------------------------------
024900160816       //?Riepilogo indicatori utilizzati.                             ?
025000160816       //--------------------------------------------------------------
025100160816
025200160816
025300160816       //--------------------------------------------------------------
025400160816       //?M A I N - L I N E                                            ?
025500160816       //--------------------------------------------------------------
025600160816
025700160816     c     *Entry        plist
025800160816     c                   parm                    KPJBA
025900160816
026000160816      /free
026100160816
026200160816       // -?Operazioni iniziali?
026300160816       exsr  sr_RoutInz;
026400160816
026500160816       // -?Gestione videata?
026600160816       DoW  Not $Fine;
026700160816
026800160816         select;
026900160816
027000160816           // -?Filtro di lancio?
027100160816           when  $Video = 'D1';
027200160816             exsr  sr_GesD01;
027300160816
027400160816           // -?? ? ??
027500160816           other;
027600160816             $Fine = *on;
027700160816
027800160816         endsl;
027900160816
028000160816       EndDo;
028100160816
028200160816       // -?Operazioni finali?
028300160816       exsr  sr_RoutEnd;
028400160816
028500160816       //--------------------------------------------------------------
028600160816       //?Operazioni iniziali.                                         ?
028700160816       //--------------------------------------------------------------
028800160816       BEGSR  sr_RoutInz;
028900160816
029000160816         // -?Impostazione chiusura?
029100160816         *inLR = *on;
029200160818
029300160818         // -?Verifica del sistema AS/400 corrente?
029400160818         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
029500160818           $Fine  = *on;
029600160818           leavesr;
029700160818         endif;
029800160818
029900160818         // -?Apertura file FNARB01L?
030000160818         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
030100160818           wLibr  = 'FILTRA201';
030200160818         else;
030300160818           wLibr  = 'FILTRAPRD';
030400160818         endif;
030500160818         wLibFile = %trimR( wLibr ) + '/FNARB01L';
030600160818         open  FNARB01L;
030700160816
030800160816         // -?Reperimento dati job?
030900160816         exsr  sr_DatiJob;
031000160816
031100160816         // -?Impostazione nome programma a video?
031200160816         VT1pgm = SDSpgm;
031300160816
031400160816         // -?Recupero la moneta corrente?
031500160816         clear  dGEDdn;
031600160816         If  getTabella ( c_Tntbe : 'GED' : 'DANNI' :
031700160816                          *blank :
031800160816                          *omit : *omit :
031900160816                          *omit : *omit :
032000160816                          *omit : *omit : *omit : *omit :
032100160816                          *omit : *omit : *omit : *omit :
032200160816                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
032300160816                          = *zero      AND
032400160816             ds_TNTBE.TBEatb = *blank;
032500160816           dGEDdn = ds_TNTBE.TBEuni;
032600160816         EndIf;
032700160816
032800160816         // -?Recupero Standards Danni?
032900160816         clear  dSTD;
033000160816         If  getTabella ( c_Tntbe : 'STD' : '1' :
033100160816                          dGEDdn.§gedDBA :
033200160816                          *omit : *omit :
033300160816                          *omit : *omit :
033400160816                          *omit : *omit : *omit : *omit :
033500160816                          *omit : *omit : *omit : *omit :
033600160816                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
033700160816                          = *zero      AND
033800160816             ds_TNTBE.TBEatb = *blank;
033900160816           dSTD = ds_TNTBE.TBEuni;
034000160816         EndIf;
034100160816
034200160816       ENDSR;
034300160816
034400160816       //--------------------------------------------------------------
034500160816       //?Reperimento Dati del job (Utente/Operativi).                 ?
034600160816       //--------------------------------------------------------------
034700160816       BEGSR  sr_DatiJob;
034800160816
034900160816         in(e) §AzUte;
035000160816         if NOT %error;
035100160816           in(e) §DatiUte;
035200160816         endif;
035300160816         if %error or RSut = *blank;
035400160816           tibs34r ( tibs34ds );
035500160816           in §AzUte;
035600160816           in §DatiUte;
035700160816         endif;
035800160816
035900160816       ENDSR;
036000160816
036100160816       //--------------------------------------------------------------
036200160816       //?Gestione videata D01.                                        ?
036300160816       //--------------------------------------------------------------
036400160816       BEGSR  sr_GesD01;
036500160816
036600160816         // -?Inizializzazione videata?
036700160816         if  $InzD01 = *on;
036800160816           exsr  sr_InzD01;
036900160816           $InzD01 = *off;
037000160816         endif;
037100160816
037200160816         // -?(Dis)Attivazione tasti funzionali a video?
037300160816         $F10Attivo  = *on;
037400160816         $F14Attivo  = *on;
037500160816
037600160816         // -?Emissione Testata e Piede con tasti funzionali abilitati?
037700160817         write  DN78T01;
037800160817         write  DN78Z01;
037900160816
038000160816         // -?Emissione videata?
038100160817         exfmt  DN78D01;
038200160816
038300160817         clear  V1Dmsg;
038400160816         reset  $ErrMessage;
038500160816         reset  $ErrGenerico;
038600160816
038700160816         Select;
038800160816
038900160816           //*// -?Errore: sistema informativo errato?
039000160816           //*When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
039100160816           //*  $Fine  = *on;
039200160816
039300160816           // -?F3=Fine?
039400160816           When  dsp_aid = c_F03;
039500160816             $Fine  = *on;
039600160816
039700160816           // -?F14=Interrogazione C.A.?
039800160816           When  dsp_aid = c_F14;
039900160817             exsr  sr_CtrD01;
040000160817             if  $ErrGenerico;
040100160817               leavesr;
040200160817             endif;
040300160816             exsr  sr_CallFIDN01;
040400160816
040500160816           // -?Enter / F6=Conferma?
040600160816           Other;
040700160816             exsr  sr_CtrD01;
040800160816             if  $ErrGenerico;
040900160816               leavesr;
041000160816             endif;
041100160816             if  dsp_aid = c_F06;
041200160816               exsr  sr_CA_EEX;
041300160830               //*·reset  $InzD01;
041400160830               $Fine = *on;
041500160816             endif;
041600160816
041700160816         EndSl;
041800160816
041900160816       ENDSR;
042000160816
042100160816       //--------------------------------------------------------------
042200160816       //?Inizializzazione videata D01.                                ?
042300160816       //--------------------------------------------------------------
042400160816       BEGSR  sr_InzD01;
042500160816
042600160816         // -?Spegnimento degli indicatori di errore?
042700160816         %subst( IndDspF : 50 ) = *off;
042800160816
042900160816         // -?Pulizia videata?
043000160817         clear  DN78D01;
043100160816
043200160816         // -?Impostazione Anno C.A.?
043300160816         V1Caac = *year;
043400160816
043500160816       ENDSR;
043600160816
043700160816       //--------------------------------------------------------------
043800160816       //?Inizializzazione videata D01.                                ?
043900160816       //--------------------------------------------------------------
044000160816       BEGSR  sr_CtrD01;
044100160816
044200160816         // -?Controllo Fil. C.A.?
044300160816         if  V1Cfil = *zero;
044400160816           V1Dmsg = sk_Msg(01);
044500160816           $PosCurFIL   = *on;
044600160816           $ErrMessage  = *on;
044700160816           $ErrGenerico = *on;
044800160816           leavesr;
044900160816         endif;
045000160816
045100160816         // -?Controllo Anno C.A.?
045200160816         if  V1Caac = *zero;
045300160816           V1Dmsg = sk_Msg(01);
045400160816           $PosCurAAC   = *on;
045500160816           $ErrMessage  = *on;
045600160816           $ErrGenerico = *on;
045700160816           leavesr;
045800160816         endif;
045900160816
046000160816         // -?Sistemazione dell'anno di due cifre?
046100160816         select;
046200160816           when  V1Caac < 100  and  V1Caac > 60;
046300160816             V1Caac += 1900;
046400160816           when  V1Caac < 100  and  V1Caac <= 60;
046500160816             V1Caac += 2000;
046600160816         endsl;
046700160816
046800160816         // -?Controllo Num. C.A.?
046900160816         if  V1Caac = *zero;
047000160816           V1Dmsg = sk_Msg(01);
047100160816           $PosCurNCA   = *on;
047200160816           $ErrMessage  = *on;
047300160816           $ErrGenerico = *on;
047400160816           leavesr;
047500160816         endif;
047600160816
047700160816         // -?Controllo esistenza C.A.?
047800160816         clear  keyFNDCT01;
047900160816         k_DCTaac = V1Caac;
048000160816         k_DCTfil = V1Cfil;
048100160816         k_DCTnca = V1Cnca;
048200160816
048300160816         chain  %kds( keyFNDCT01 )  FNDCT000;
048400160816
048500160816         if  Not %found(FNDCT01L);
048600160816           V1Dmsg = sk_Msg(02);
048700160816           $PosCurNCA   = *on;
048800160816           $ErrMessage  = *on;
048900160816           $ErrGenerico = *on;
049000160816           leavesr;
049100160816         endif;
049200160817
049300160817         // -?F14=Interrogazione C.A. => fine controlli?
049400160817         if  dsp_aid = c_F14;
049500160817           leavesr;
049600160817         endif;
049700160816
049800160816         // -?Controllo se C.A. annullata?
049900160816         if  %found(FNDCT01L)  and  DCTatb <> *blank;
050000160816           V1Dmsg = sk_Msg(03);
050100160816           $PosCurNCA   = *on;
050200160816           $ErrMessage  = *on;
050300160816           $ErrGenerico = *on;
050400160816           leavesr;
050500160816         endif;
050600160830
050700160830         // -?Controllo se C.A. in gestione alla Sede?
050800160830         if  DCTgfc <> c_Sede;
050900160830           V1Dmsg = sk_Msg(09);
051000160830           $PosCurFIL   = *on;
051100160830           $ErrMessage  = *on;
051200160830           $ErrGenerico = *on;
051300160830           leavesr;
051400160830         endif;
051500160816
051600160816         // -?Controllo se C.A. già confermata?
051700160817         //  ?e relativa a bolle Export (EuroExpress)?
051800160817         //  ?e con Responsabilità "Partner"?
051900160816         //  ?e confermata come Pratica Assicurativa?
052000160816         //  ?e con importo massimo risarcibile > 100 € (DSTD.§STDIPDEEX)?
052100160816         dDCT01 = DCTflo;
052200160816         if  DCTfca   <  c_FCA_Conf  or
052300160817             dDCT01.§DCTtisp <> 'E'  or
052400160817             dDCT01.§DCTeuro <> 'X'  or
052500160816             DCTres   <> 'P'         or
052600160816             DCTfpr   <> 'P'         or
052700160817             DCTipv   <  dSTD.§STDipdEEX;
052800160816           V1Dmsg = sk_Msg(04);
052900160816           $PosCurNCA   = *on;
053000160816           $ErrMessage  = *on;
053100160816           $ErrGenerico = *on;
053200160816           leavesr;
053300160816         endif;
053400160816
053500160817         // -?Reperimento data della 1ª conferma C.A.?
053600160817         exsr  sr_1a_ConfCA;
053700160817         if  SaveDCFdfc = *zero;
053800160817           SaveDCFdfc = %int( %subst( %char( %dec( %timestamp() ) )
053900160817                                      : 1 : 8 ) );
054000160817         endif;
054100160816
054200160816       ENDSR;
054300160816
054400160817       //--------------------------------------------------------------
054500160817       //?Reperimento data della 1ª conferma C.A..                     ?
054600160817       //--------------------------------------------------------------
054700160817       BEGSR  sr_1a_ConfCA;
054800160817
054900160817         clear  SaveDCFdfc;
055000160817         setll  %kds( keyFNDCT01 )  FNDCF000;
055100160817         reade  %kds( keyFNDCT01 )  FNDCF000;
055200160817
055300160817         DoW  Not %eof( FNDCF01L );
055400160817
055500160817           if  DCFfca = c_FCA_Conf  and
055600160817               DCFatb = *blank;
055700160830             dDCF01 = DCFlet;
055800160830             if  SaveDCFdfc = *zero;
055900160817               SaveDCFdfc = DCFdfc;
056000160830             endif;
056100160830             if  dDCF01.§DCFinvi = 'X';
056200160830               SaveDCFdfc = DCFdfc;
056300160830               leave;
056400160830             endif;
056500160817           endif;
056600160817
056700160817           reade  %kds( keyFNDCT01 )  FNDCF000;
056800160817
056900160817         EndDo;
057000160817
057100160817       ENDSR;
057200160818
057300160818       //--------------------------------------------------------------
057400160818       //?Invio/Ristampa Preavviso Danno EuroExpress.                  ?
057500160818       //--------------------------------------------------------------
057600160818       BEGSR  sr_CA_EEX;
057700160818
057800160818         clear  FIDN00ds;
057900160818         if  V1CeMail = *blank;
058000160818           I00mod = 'R';       //?(ristampa)?
058100160818         else;
058200160818           I00mod = 'G';       //?(re-invio e-mail)?
058300160818         endif;
058400160818         I00fca = 999;
058500160818         I00fgs = DCTgfc;
058600160818         I00aac = DCTaac;
058700160818         I00fil = DCTfil;
058800160818         I00nca = DCTnca;
058900160818         I00aas = DCTaas;
059000160818         I00lnp = DCTlnp;
059100160818         I00nrs = DCTnrs;
059200160818         I00nsp = DCTnsp;
059300160818         I00lna = DCTlna;
059400160818         I00tad = DCTtad;
059500160818         I00nce = 1;
059600160818         I00dsb = SaveDCFdfc;
059700160818
059800160818         // -?Reperimento dati della spedizione (Partenza/Arrivo)?
059900160818         exsr  sr_Rep_FNBLPARB;
060000160818
060100160818         // -?Esecuzione override ai *file di filiale?
060200160818         //  ?(se utente di sede)?
060300160818         if  DUTlpo = 'S';
060400160818           exsr sr_Override;
060500160818         endif;
060600160818
060700160818         Select;
060800160818
060900160818           // -?Rilevato errore nelle override a FNBLP/FNARB?
061000160818           When  $Err;
061100160818             V1Dmsg = sk_Msg(08);
061200160818             $PosCurFIL   = *on;
061300160818             $ErrMessage  = *on;
061400160818             $ErrGenerico = *on;
061500160818
061600160818           // -?x lna 320 - 325?
061700160818           When  DCTlna = 320  or  DCTlna = 325;
061800160818             exsr  sr_CallFIDN39;
061900160818             //*$EEX_Mail = *on;
062000160818
062100160818           // -?x lna 350?
062200160818           When  DCTlna = 350;
062300160818             exsr  sr_CallFIDN41;
062400160818             //*$EEX_Mail = *on;
062500160818
062600160818           // -?x lna 311, 333, 334 e 340-345?
062700160818           When  DCTlna = 311  or
062800160818                 DCTlna = 333  or
062900160818                 DCTlna = 334  or
063000160818                 DCTlna = 340  or  DCTlna = 345;
063100160818             exsr  sr_CallFIDN68;
063200160818             //*$EEX_Mail = *on;
063300160818
063400160818         EndSl;
063500160818
063600160818         // -?Cancellazione override ai *file di filiale?
063700160818         //  ?(se utente di sede)?
063800160818         if  DUTlpo = 'S';
063900160818           exsr sr_DeleteOvr;
064000160818         endif;
064100160818
064200160818       ENDSR;
064300160816
064400160816       //--------------------------------------------------------------
064500160816       //?Richiamo *pgm Interrogazione C.A.                            ?
064600160816       //--------------------------------------------------------------
064700160816       BEGSR  sr_CallFIDN01;
064800160816
064900160816         clear  FIDN00ds;
065000160816
065100160816         I00mod = 'I';
065200160816         I00fca = DCTfca;
065300160816         I00fgs = DCTgfc;
065400160816         I00aac = DCTaac;
065500160816         I00fil = DCTfil;
065600160816         I00nca = DCTnca;
065700160816         I00aas = DCTaas;
065800160816         I00lnp = DCTlnp;
065900160816         I00nrs = DCTnrs;
066000160816         I00nsp = DCTnsp;
066100160816         I00lna = DCTlna;
066200160816         I00tad = DCTtad;
066300160817
066400160818         // -?Impostazione "Tipo Bolla" (Partenza/Arrivo/Sede)?
066500160818         if  DCTgfc = c_Sede;
066600160818           I00tpb = 'S';
066700160818         else;
066800160818           exsr  sr_Rep_FNBLPARB;
066900160818         endif;
067000160816
067100160816         kpjbu = FIDN00ds;
067200160816
067300160816         fidn01r ( kpjba );
067400160816
067500160816       ENDSR;
067600160818
067700160818       //--------------------------------------------------------------
067800160818       //?Invio/Ristampa Preavviso Danno a partner VanDuuren.          ?
067900160818       //--------------------------------------------------------------
068000160818       BEGSR  sr_CallFIDN39;
068100160818
068200160818         kpjbu = FIDN00ds;
068300160818         fidn39r ( kpjba );
068400160818         FIDN00ds = kpjbu;
068500160818
068600160818         if  o00err <> *blank;
068700160818           if  I00mod = 'G';        //?(re-invio e-mail)?
068800160818             o00msg = %trimR( sk_msg(06) ) + ' ' + o00msg;
068900160818           else;                    //?(ristampa)?
069000160818             o00msg = %trimR( sk_msg(07) ) + ' ' + o00msg;
069100160818           endif;
069200160818           o00err = 'E';
069300160818         endif;
069400160818
069500160818       ENDSR;
069600160818
069700160818       //--------------------------------------------------------------
069800160818       //?Invio/Ristampa Preavviso Danno a partner UK.                 ?
069900160818       //--------------------------------------------------------------
070000160818       BEGSR  sr_CallFIDN41;
070100160818
070200160818         kpjbu = FIDN00ds;
070300160818         fidn41r ( kpjba );
070400160818         FIDN00ds = kpjbu;
070500160818
070600160818         if  o00err <> *blank;
070700160818           if  I00mod = 'G';        //?(re-invio e-mail)?
070800160818             o00msg = %trimR( sk_msg(06) ) + ' ' + o00msg;
070900160818           else;                    //?(ristampa)?
071000160818             o00msg = %trimR( sk_msg(07) ) + ' ' + o00msg;
071100160818           endif;
071200160818           o00err = 'E';
071300160818         endif;
071400160818
071500160818       ENDSR;
071600160818
071700160818       //--------------------------------------------------------------
071800160818       //?Invio/Ristampa Preavviso Danno a partner:                    ?
071900160818       //?· 311     = GEL Express Logistik GmbH                        ?
072000160818       //?· 333     = France Express Lyon Corbas                       ?
072100160818       //?· 334     = Calexpress Gennevilliers                         ?
072200160818       //?· 340-345 = Azkar                                            ?
072300160818       //--------------------------------------------------------------
072400160818       BEGSR  sr_CallFIDN68;
072500160818
072600160818         kpjbu = FIDN00ds;
072700160818         fidn68r ( kpjba );
072800160818         FIDN00ds = kpjbu;
072900160818
073000160818         if  o00err <> *blank;
073100160818           //*if  I00mod = 'G';        //?(re-invio e-mail)?
073200160818           //*  o00msg = %trimR( sk_msg(06) ) + ' ' + o00msg;
073300160818           //*else;                    //?(ristampa)?
073400160818           //*  o00msg = %trimR( sk_msg(07) ) + ' ' + o00msg;
073500160818           //*endif;
073600160818           //*o00err = 'E';
073700160818           // -?Meglio NON segnalare errori (tipo "e-mail mancante") a chi?
073800160818           //  ?NON sarebbe in grado né di capire né di risolvere...?
073900160818           clear  o00err;
074000160818           clear  o00msg;
074100160818         endif;
074200160818
074300160818       ENDSR;
074400160817
074500160817       //--------------------------------------------------------------
074600160817       //?Reperimento flag spedizione (Partenza/Arrivo).               ?
074700160817       //--------------------------------------------------------------
074800160817       BEGSR  sr_Rep_FNBLPARB;
074900160817
075000160817         clear  keyFNBLPARB;
075100160817         k_ARBaas = DCTaas;
075200160817         k_ARBlnp = DCTlnp;
075300160817         k_ARBnrs = DCTnrs;
075400160817         k_ARBnsp = DCTnsp;
075500160817
075600160817         chain  %kds( keyFNBLPARB )  FNARB000;
075700160817
075800160818         if  %found(FNARB01L)  and
075900160817             ARBatb = *blank;
076000160818           I00tpb = 'A';       // ·?Spedizione in FNARB?
076100160817         else;
076200160818           I00tpb = 'P';       // ·?Spedizione in FNBLP?
076300160817         endif;
076400160817
076500160817       ENDSR;
076600160818
076700160818       //--------------------------------------------------------------
076800160818       //?Esecuzione override ai *file delle spedizioni (FNBLP/ARB).   ?
076900160818       //--------------------------------------------------------------
077000160818       BEGSR  sr_Override;
077100160818
077200160818         Qcmd = c_CmdOvr_BLP + %trimR( wLibr ) + '/FNBLP01L)';
077300160818         exsr  sr_ExecCmd;
077400160818         if  Qusei <> *blank;
077500160818           $Err = *on;
077600160818           leavesr;
077700160818         endif;
077800160818
077900160818         Qcmd = c_CmdOvr_ARB + %trimR( wLibr ) + '/FNARB01L)';
078000160818         exsr  sr_ExecCmd;
078100160818         if  Qusei <> *blank;
078200160818           $Err = *on;
078300160818           leavesr;
078400160818         endif;
078500160818
078600160818       ENDSR;
078700160818
078800160818       //--------------------------------------------------------------
078900160818       //?Cancellazione override ai *file delle spedizioni (FNBLP/ARB).?
079000160818       //--------------------------------------------------------------
079100160818       BEGSR  sr_DeleteOvr;
079200160818
079300160818         Qcmd = c_CmdDltOvr;
079400160818         exsr  sr_ExecCmd;
079500160818
079600160818       ENDSR;
079700160818
079800160818       //--------------------------------------------------------------
079900160818       //?Esecuzione del comando (già impostato).                      ?
080000160818       //--------------------------------------------------------------
080100160818       BEGSR  sr_ExecCmd;
080200160818
080300160818         clear Qcap0100;
080400160818         Qcabcsdh = *off;
080500160818         Qcapa    = *off;
080600160818         Qcacmdss = *off;
080700160818         Qcaerved = *allX'00';
080800160818
080900160818         clear Qusec;
081000160818         Qusbprv  = %size(Qusec);
081100160818
081200160818         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
081300160818                           %size(Qcap0100) : 'CPOP0100' : *omit :
081400160818                           0 : 0 : Qusec);
081500160818
081600160818         //if  Qusei <> *blank;
081700160818         //  ...;
081800160818         //endif;
081900160818
082000160818       ENDSR;
082100160816
082200160816       //--------------------------------------------------------------
082300160816       //?Operazioni finali.                                           ?
082400160816       //--------------------------------------------------------------
082500160816       BEGSR  sr_RoutEnd;
082600160816
082700160816         // -?Chiusura funzioni precedentemente aperte?
082800160816         cloTabella ( c_Tntbe );
082900160816
083000160816         // -?Uscita dal *pgm?
083100160816         return;
083200160816
083300160816       ENDSR;
083400160816
083500160816      /end-free
083600160816
083700160816       //--------------------------------------------------------------
083800160816       //?Definizione schiere a tempo di compilazione                  ?
083900160816       //--------------------------------------------------------------
084000160816
084100160816** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
084200160816Il valore immesso nel campo non è valido                                       1
084300160816Il numero C.A. immesso non esiste                                              2
084400160816La C.A. è annullata                                                            3
084500160816C.A. non contestabile ad EuroExpress                                           4
084600160817Impossibile RISTAMPARE l'Avviso Danno in quanto non è mai stato stampato       5 - NOT USED
084700160816E-MAIL non inviata:                                                            6
084800160818Stampa non riuscita:                                                           7
084900160818Rilevati problemi tecnici: avvisare il CED                                     8
085000160830La C.A. non è in gestione alla Sede                                            9
