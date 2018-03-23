000100160816       //==============================================================
000200160817       //?FIDN78R * Re-Invio e-mail / Ri-Stampa  Preavviso Danno EEX.  ?
000300160816       //==============================================================
000400160816
000500160816       //--------------------------------------------------------------
000600160816       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160816       //--------------------------------------------------------------
000800160816
000900160816     /*PRM  dbgview(*source)
001000160816     /*END
001100160816
001200160816       //--------------------------------------------------------------
001300160816       //?Specifiche di controllo.                                     ?
001400160816       //--------------------------------------------------------------
001500160816
001600160816     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160817     h dftactgrp(*no)
001800160818     h bnddir('TRUL':'UBBNDDIR')
001900160816
002000160816       //--------------------------------------------------------------
002100160816       //?Dichiarazione file.                                          ?
002200160816       //--------------------------------------------------------------
002300160816
002400160816       // -?DANNI: Testata C.A.?
002500160816     fFNDCT01L  if   e           k disk
002600160816       // -?DANNI: Fasi C.A.?
002700160816     fFNDCF01L  if   e           k disk
002800160817
002900160831       // -?BOLLE SEDE: "da fatturare",?
003000160831       //  ?            fatturate "ultimi mesi" e?
003100160831       //  ?            fatturate "storico"?
003200160831     fTITAS30C  if   e           k disk
003300160816
003400160816       // -?Video Gestione?
003500160816     fFIDN78D   cf   e             workstn
003600160816     f                                     indds( IndDspF )
003700160816     f                                     infds( InfDspF )
003800160816
003900160816       //--------------------------------------------------------------
004000160816       //?Definizione costanti.                                        ?
004100160816       //--------------------------------------------------------------
004200160816
004300160816       // -?Fasi fissate a *pgm?
004400160816     d c_FCA_Conf      c                   const(100)
004500160817
004600160817       // -?Codice Sede?
004700160817     d c_Sede          c                   const(046)
004800160816
004900160816       // -?Tasti funzionali a video?
005000160816     d c_F01           c                   const(x'31')
005100160816     d c_F02           c                   const(x'32')
005200160816     d c_F03           c                   const(x'33')
005300160816     d c_F04           c                   const(x'34')
005400160816     d c_F05           c                   const(x'35')
005500160816     d c_F06           c                   const(x'36')
005600160816     d c_F07           c                   const(x'37')
005700160816     d c_F08           c                   const(x'38')
005800160816     d c_F09           c                   const(x'39')
005900160816     d c_F10           c                   const(x'3A')
006000160816     d c_F11           c                   const(x'3B')
006100160816     d c_F12           c                   const(x'3C')
006200160816     d c_F13           c                   const(x'B1')
006300160816     d c_F14           c                   const(x'B2')
006400160816     d c_F15           c                   const(x'B3')
006500160816     d c_F16           c                   const(x'B4')
006600160816     d c_F17           c                   const(x'B5')
006700160816     d c_F18           c                   const(x'B6')
006800160816     d c_F19           c                   const(x'B7')
006900160816     d c_F20           c                   const(x'B8')
007000160816     d c_F21           c                   const(x'B9')
007100160816     d c_F22           c                   const(x'BA')
007200160816     d c_F23           c                   const(x'BB')
007300160816     d c_F24           c                   const(x'BC')
007400160816     d c_Enter         c                   const(x'F1')
007500160816     d c_RollDown      c                   const(x'F4')
007600160816     d c_RollUp        c                   const(x'F5')
007700160816
007800160816       //--------------------------------------------------------------
007900160816       //?Definizione schiere.                                         ?
008000160816       //--------------------------------------------------------------
008100160816
008200160816       // -?Messaggi di errore?
008300160831     d sk_Msg          s             78    dim( 7)  ctdata  perrcd(1)
008400160816
008500160816       //--------------------------------------------------------------
008600160816       //?Definizione aree dati.                                       ?
008700160816       //--------------------------------------------------------------
008800160816
008900160816       // -?Dati utente?
009000160816     d §AzUte        e ds                  extname(AZUTE00F)
009100160816     d                                     dtaara
009200160816     d §DatiUte      e ds                  extname(dDatiUte)
009300160816     d                                     dtaara
009400160816
009500160816       //--------------------------------------------------------------
009600160816       //?Definizione strutture dati.                                  ?
009700160816       //--------------------------------------------------------------
009800160816
009900160816       // -?Status ds?
010000160816     d Status         sds
010100160816     d   SDSpgm          *proc
010200160816
010300160816       // -?InfDS?
010400160816     d InfDspF         ds
010500160816     d   dsp_aid             369    369a
010600160816
010700160816       // -?Indicatori su DspF?
010800160816     d IndDspF         ds                  inz
010900160816         // -?Abilitazione tasti funzionali?
011000160816     d   $F10Attivo                    n   overlay( IndDspF : 10 )
011100160816     d   $F14Attivo                    n   overlay( IndDspF : 14 )
011200160816         // ·?Emissione messaggio di errore?
011300160816     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011400160816         // ·?Indicatori per Attibuti di visualizzazione?
011500160816         // ·?Posizionamento cursore & Segnalazione errore?
011600160816     d   $PosCurFIL                    n   overlay( IndDspF : 51 )
011700160816     d   $PosCurNCA                    n   overlay( IndDspF : 52 )
011800160816     d   $PosCurAAC                    n   overlay( IndDspF : 53 )
011900160816         // ·?Riemissione videata?
012000160816     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
012100160816
012200160816       // -?Numero C.A.?
012300160816     d DS_numCA        ds                  inz   qualified
012400160817     d   aac                               inz   like(I00aac)
012500160817     d   fil                               inz   like(I00fil)
012600160817     d   nca                               inz   like(I00nca)
012700160816
012800160816       // -?Parametri ricevuti?
012900160816     d KPJBA         e ds
013000160816
013100160816       // -?DS Gestionale: divise Danni?
013200160816     d dGEDdn        e ds                  inz   qualified
013300160816       // -?DS Standard Danni?
013400160816     d dSTD          e ds                  inz   qualified
013500160816
013600160816       // -?Flag operativi in FNDCT?
013700160816     d dDCT01        e ds                  inz   qualified
013800160817
013900160830       // -?Flag operativi in FNDCF?
014000160830     d dDCF01        e ds                  inz   qualified
014100160816
014200160816       //--------------------------------------------------------------
014300160816       //?Definizione variabili globali.                               ?
014400160816       //--------------------------------------------------------------
014500160816
014600160816       // -?Flags booleani?
014700160816     d $Fine           s               n   inz
014800160816     d $InzD01         s               n   inz(*on)
014900160818     d $Err            s               n   inz
015000160816
015100160816       // -?Variabili per la gestione del video?
015200160816     d $Video          s              2    inz('D1')
015300160816
015400160816       // -?Campi di Comodo?
015500160817     d SaveDCFdfc      s                   like(DCFdfc)  inz
015600160906     d SaveDCFfev      s                   like(DCFfev)  inz
015700160816
015800160816       //--------------------------------------------------------------
015900160816       //?Definizione prototipi procedure usate.                       ?
016000160816       //--------------------------------------------------------------
016100160816
016200160816       // -?Reperimento dati utente?
016300160816     d TIBS34ds      e ds                  inz
016400160816      /copy gaitrasrc/srcProtoPR,TIBS34R
016500160816
016600160816       // -?Reperimento dati tabelle?
016700160816      /copy gaitrasrc/srcProtoPI,TRULTAB
016800160816      /copy gaitrasrc/srcProtoPR,TRULTAB
016900160816
017000160816       // -?Controllo/Inversione Date?
017100160816     d WLBdat          ds                  inz
017200160816     d   G08dat                       8  0 inz
017300160816     d   G08inv                       8  0 inz
017400160816     d   G08err                       1    inz('3')
017500160816     d   G08tgi                       5  0 inz
017600160816      /copy gaitrasrc/srcProtoPR,XSRDA8
017700160816
017800160817       // -?Interrogazione C.A.?
017900160816     d FIDN00ds      e ds                  inz
018000160816     d fidn01r         pr                  extpgm('FIDN01R')
018100160816     d   kpjba                             likeds(KPJBA)
018200160817       //  ?Stampa/Invio e-mail con Preavviso Danno EEX?
018300160816     d fidn39r         pr                  extpgm('FIDN39R')
018400160816     d   kpjba                             likeds(KPJBA)
018500160816     d fidn41r         pr                  extpgm('FIDN41R')
018600160816     d   kpjba                             likeds(KPJBA)
018700160816     d fidn68r         pr                  extpgm('FIDN68R')
018800160816     d   kpjba                             likeds(KPJBA)
018900160816
019000160816       //--------------------------------------------------------------
019100160816       //?Definizione key-list.                                        ?
019200160816       //--------------------------------------------------------------
019300160816
019400160816       // -?File FNDCT01L?
019500160816     d keyFNDCT01    e ds                  extname( FNDCT01L : *key )
019600160831     d                                     prefix( k_ )  inz
019700160816       // -?File FNDCF01L?
019800160816     d keyFNDCF01    e ds                  extname( FNDCF01L : *key )
019900160831     d                                     prefix( k_ )  inz
020000160817
020100160831       // -?File TITAS30C?
020200160831     d keyTITAS30    e ds                  extname( TITAS30C : *key )
020300160831     d                                     prefix( k_ )  inz
020400160816
020500160816       //--------------------------------------------------------------
020600160816       //?Fasi C.A. FISSATE a programma.                               ?
020700160816       //--------------------------------------------------------------
020800160818       // 100 = Conferma C.A.                                c_FCA_Conf
020900160816       //--------------------------------------------------------------
021000160816
021100160816       //--------------------------------------------------------------
021200160816       //?Riepilogo indicatori utilizzati.                             ?
021300160816       //--------------------------------------------------------------
021400160816
021500160816
021600160816       //--------------------------------------------------------------
021700160816       //?M A I N - L I N E                                            ?
021800160816       //--------------------------------------------------------------
021900160816
022000160816     c     *Entry        plist
022100160816     c                   parm                    KPJBA
022200160816
022300160816      /free
022400160816
022500160816       // -?Operazioni iniziali?
022600160816       exsr  sr_RoutInz;
022700160816
022800160816       // -?Gestione videata?
022900160816       DoW  Not $Fine;
023000160816
023100160816         select;
023200160816
023300160816           // -?Filtro di lancio?
023400160816           when  $Video = 'D1';
023500160816             exsr  sr_GesD01;
023600160816
023700160816           // -?? ? ??
023800160816           other;
023900160816             $Fine = *on;
024000160816
024100160816         endsl;
024200160816
024300160816       EndDo;
024400160816
024500160816       // -?Operazioni finali?
024600160816       exsr  sr_RoutEnd;
024700160816
024800160816       //--------------------------------------------------------------
024900160816       //?Operazioni iniziali.                                         ?
025000160816       //--------------------------------------------------------------
025100160816       BEGSR  sr_RoutInz;
025200160816
025300160816         // -?Impostazione chiusura?
025400160816         *inLR = *on;
025500160816
025600160816         // -?Reperimento dati job?
025700160816         exsr  sr_DatiJob;
025800160816
025900160816         // -?Impostazione nome programma a video?
026000160816         VT1pgm = SDSpgm;
026100160816
026200160816         // -?Recupero la moneta corrente?
026300160816         clear  dGEDdn;
026400160816         If  getTabella ( c_Tntbe : 'GED' : 'DANNI' :
026500160816                          *blank :
026600160816                          *omit : *omit :
026700160816                          *omit : *omit :
026800160816                          *omit : *omit : *omit : *omit :
026900160816                          *omit : *omit : *omit : *omit :
027000160816                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
027100160816                          = *zero      AND
027200160816             ds_TNTBE.TBEatb = *blank;
027300160816           dGEDdn = ds_TNTBE.TBEuni;
027400160816         EndIf;
027500160816
027600160816         // -?Recupero Standards Danni?
027700160816         clear  dSTD;
027800160816         If  getTabella ( c_Tntbe : 'STD' : '1' :
027900160816                          dGEDdn.§gedDBA :
028000160816                          *omit : *omit :
028100160816                          *omit : *omit :
028200160816                          *omit : *omit : *omit : *omit :
028300160816                          *omit : *omit : *omit : *omit :
028400160816                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
028500160816                          = *zero      AND
028600160816             ds_TNTBE.TBEatb = *blank;
028700160816           dSTD = ds_TNTBE.TBEuni;
028800160816         EndIf;
028900160816
029000160816       ENDSR;
029100160816
029200160816       //--------------------------------------------------------------
029300160816       //?Reperimento Dati del job (Utente/Operativi).                 ?
029400160816       //--------------------------------------------------------------
029500160816       BEGSR  sr_DatiJob;
029600160816
029700160816         in(e) §AzUte;
029800160816         if NOT %error;
029900160816           in(e) §DatiUte;
030000160816         endif;
030100160816         if %error or RSut = *blank;
030200160816           tibs34r ( tibs34ds );
030300160816           in §AzUte;
030400160816           in §DatiUte;
030500160816         endif;
030600160816
030700160816       ENDSR;
030800160816
030900160816       //--------------------------------------------------------------
031000160816       //?Gestione videata D01.                                        ?
031100160816       //--------------------------------------------------------------
031200160816       BEGSR  sr_GesD01;
031300160816
031400160816         // -?Inizializzazione videata?
031500160816         if  $InzD01 = *on;
031600160816           exsr  sr_InzD01;
031700160816           $InzD01 = *off;
031800160816         endif;
031900160816
032000160816         // -?(Dis)Attivazione tasti funzionali a video?
032100160816         $F10Attivo  = *on;
032200160816         $F14Attivo  = *on;
032300160816
032400160816         // -?Emissione Testata e Piede con tasti funzionali abilitati?
032500160817         write  DN78T01;
032600160817         write  DN78Z01;
032700160816
032800160816         // -?Emissione videata?
032900160817         exfmt  DN78D01;
033000160816
033100160817         clear  V1Dmsg;
033200160816         reset  $ErrMessage;
033300160816         reset  $ErrGenerico;
033400160816
033500160816         Select;
033600160816
033700160816           //*// -?Errore: sistema informativo errato?
033800160816           //*When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
033900160816           //*  $Fine  = *on;
034000160816
034100160816           // -?F3=Fine?
034200160816           When  dsp_aid = c_F03;
034300160816             $Fine  = *on;
034400160816
034500160816           // -?F14=Interrogazione C.A.?
034600160816           When  dsp_aid = c_F14;
034700160817             exsr  sr_CtrD01;
034800160817             if  $ErrGenerico;
034900160817               leavesr;
035000160817             endif;
035100160816             exsr  sr_CallFIDN01;
035200160816
035300160816           // -?Enter / F6=Conferma?
035400160816           Other;
035500160816             exsr  sr_CtrD01;
035600160816             if  $ErrGenerico;
035700160816               leavesr;
035800160816             endif;
035900160816             if  dsp_aid = c_F06;
036000160816               exsr  sr_CA_EEX;
036100160830               //*·reset  $InzD01;
036200160830               $Fine = *on;
036300160816             endif;
036400160816
036500160816         EndSl;
036600160816
036700160816       ENDSR;
036800160816
036900160816       //--------------------------------------------------------------
037000160816       //?Inizializzazione videata D01.                                ?
037100160816       //--------------------------------------------------------------
037200160816       BEGSR  sr_InzD01;
037300160816
037400160816         // -?Spegnimento degli indicatori di errore?
037500160816         %subst( IndDspF : 50 ) = *off;
037600160816
037700160816         // -?Pulizia videata?
037800160817         clear  DN78D01;
037900160816
038000160816         // -?Impostazione Anno C.A.?
038100160816         V1Caac = *year;
038200160816
038300160816       ENDSR;
038400160816
038500160816       //--------------------------------------------------------------
038600160816       //?Inizializzazione videata D01.                                ?
038700160816       //--------------------------------------------------------------
038800160816       BEGSR  sr_CtrD01;
038900160816
039000160816         // -?Controllo Fil. C.A.?
039100160816         if  V1Cfil = *zero;
039200160816           V1Dmsg = sk_Msg(01);
039300160816           $PosCurFIL   = *on;
039400160816           $ErrMessage  = *on;
039500160816           $ErrGenerico = *on;
039600160816           leavesr;
039700160816         endif;
039800160816
039900160816         // -?Controllo Anno C.A.?
040000160816         if  V1Caac = *zero;
040100160816           V1Dmsg = sk_Msg(01);
040200160816           $PosCurAAC   = *on;
040300160816           $ErrMessage  = *on;
040400160816           $ErrGenerico = *on;
040500160816           leavesr;
040600160816         endif;
040700160816
040800160816         // -?Sistemazione dell'anno di due cifre?
040900160816         select;
041000160816           when  V1Caac < 100  and  V1Caac > 60;
041100160816             V1Caac += 1900;
041200160816           when  V1Caac < 100  and  V1Caac <= 60;
041300160816             V1Caac += 2000;
041400160816         endsl;
041500160816
041600160816         // -?Controllo Num. C.A.?
041700160816         if  V1Caac = *zero;
041800160816           V1Dmsg = sk_Msg(01);
041900160816           $PosCurNCA   = *on;
042000160816           $ErrMessage  = *on;
042100160816           $ErrGenerico = *on;
042200160816           leavesr;
042300160816         endif;
042400160816
042500160816         // -?Controllo esistenza C.A.?
042600160816         clear  keyFNDCT01;
042700160816         k_DCTaac = V1Caac;
042800160816         k_DCTfil = V1Cfil;
042900160816         k_DCTnca = V1Cnca;
043000160816
043100160816         chain  %kds( keyFNDCT01 )  FNDCT000;
043200160816
043300160816         if  Not %found(FNDCT01L);
043400160816           V1Dmsg = sk_Msg(02);
043500160816           $PosCurNCA   = *on;
043600160816           $ErrMessage  = *on;
043700160816           $ErrGenerico = *on;
043800160816           leavesr;
043900160816         endif;
044000160817
044100160817         // -?F14=Interrogazione C.A. => fine controlli?
044200160817         if  dsp_aid = c_F14;
044300160817           leavesr;
044400160817         endif;
044500160816
044600160816         // -?Controllo se C.A. annullata?
044700160816         if  %found(FNDCT01L)  and  DCTatb <> *blank;
044800160816           V1Dmsg = sk_Msg(03);
044900160816           $PosCurNCA   = *on;
045000160816           $ErrMessage  = *on;
045100160816           $ErrGenerico = *on;
045200160816           leavesr;
045300160816         endif;
045400160830
045500160830         // -?Controllo se C.A. in gestione alla Sede?
045600160830         if  DCTgfc <> c_Sede;
045700160831           V1Dmsg = sk_Msg(04);
045800160830           $PosCurFIL   = *on;
045900160830           $ErrMessage  = *on;
046000160830           $ErrGenerico = *on;
046100160830           leavesr;
046200160830         endif;
046300160816
046400160816         // -?Controllo se C.A. già confermata?
046500160817         //  ?e relativa a bolle Export (EuroExpress)?
046600160817         //  ?e con Responsabilità "Partner"?
046700160816         //  ?e confermata come Pratica Assicurativa?
046800160816         //  ?e con importo massimo risarcibile > 100 € (DSTD.§STDIPDEEX)?
046900160816         dDCT01 = DCTflo;
047000160816         if  DCTfca   <  c_FCA_Conf  or
047100160817             dDCT01.§DCTtisp <> 'E'  or
047200160817             dDCT01.§DCTeuro <> 'X'  or
047300160816             DCTres   <> 'P'         or
047400160816             DCTfpr   <> 'P'         or
047500160817             DCTipv   <  dSTD.§STDipdEEX;
047600160831           V1Dmsg = sk_Msg(05);
047700160816           $PosCurNCA   = *on;
047800160816           $ErrMessage  = *on;
047900160816           $ErrGenerico = *on;
048000160816           leavesr;
048100160816         endif;
048200160816
048300160817         // -?Reperimento data della 1ª conferma C.A.?
048400160817         exsr  sr_1a_ConfCA;
048500160906
048600160817         if  SaveDCFdfc = *zero;
048700160817           SaveDCFdfc = %int( %subst( %char( %dec( %timestamp() ) )
048800160817                                      : 1 : 8 ) );
048900160817         endif;
049000160906
049100160906         if  SaveDCFfev = *zero;
049200160906           SaveDCFfev = DCTgfc;
049300160906         endif;
049400160816
049500160816       ENDSR;
049600160816
049700160817       //--------------------------------------------------------------
049800160817       //?Reperimento data della 1ª conferma C.A..                     ?
049900160817       //--------------------------------------------------------------
050000160817       BEGSR  sr_1a_ConfCA;
050100160817
050200160817         clear  SaveDCFdfc;
050300160817         setll  %kds( keyFNDCT01 )  FNDCF000;
050400160817         reade  %kds( keyFNDCT01 )  FNDCF000;
050500160817
050600160817         DoW  Not %eof( FNDCF01L );
050700160817
050800160817           if  DCFfca = c_FCA_Conf  and
050900160817               DCFatb = *blank;
051000160831
051100160830             dDCF01 = DCFlet;
051200160831             // ·?Memorizzazione data della prima conferma?
051300160830             if  SaveDCFdfc = *zero;
051400160817               SaveDCFdfc = DCFdfc;
051500160906               SaveDCFfev = DCFfev;
051600160830             endif;
051700160831             // ·?Memorizzazione data del primo invio via e-mail?
051800160830             if  dDCF01.§DCFinvi = 'X';
051900160830               SaveDCFdfc = DCFdfc;
052000160906               SaveDCFfev = DCFfev;
052100160830               leave;
052200160830             endif;
052300160831
052400160817           endif;
052500160817
052600160817           reade  %kds( keyFNDCT01 )  FNDCF000;
052700160817
052800160817         EndDo;
052900160817
053000160817       ENDSR;
053100160818
053200160818       //--------------------------------------------------------------
053300160818       //?Invio/Ristampa Preavviso Danno EuroExpress.                  ?
053400160818       //--------------------------------------------------------------
053500160818       BEGSR  sr_CA_EEX;
053600160818
053700160818         clear  FIDN00ds;
053800160818         if  V1CeMail = *blank;
053900160831           I00mod = 'R';       // ·?(ristampa)?
054000160818         else;
054100160831           I00mod = 'G';       // ·?(re-invio e-mail)?
054200160818         endif;
054300160818         I00fca = 999;
054400160906         I00fgs = SaveDCFfev;
054500160818         I00aac = DCTaac;
054600160818         I00fil = DCTfil;
054700160818         I00nca = DCTnca;
054800160818         I00aas = DCTaas;
054900160818         I00lnp = DCTlnp;
055000160818         I00nrs = DCTnrs;
055100160818         I00nsp = DCTnsp;
055200160818         I00lna = DCTlna;
055300160831         I00tpb = 'S';         // ·?Spedizione in TITAS?
055400160818         I00tad = DCTtad;
055500160818         I00nce = 1;
055600160818         I00dsb = SaveDCFdfc;
055700160818
055800160818         Select;
055900160818
056000160818           // -?x lna 320 - 325?
056100160818           When  DCTlna = 320  or  DCTlna = 325;
056200160818             exsr  sr_CallFIDN39;
056300160818
056400160818           // -?x lna 350?
056500160818           When  DCTlna = 350;
056600160818             exsr  sr_CallFIDN41;
056700160818
056800160818           // -?x lna 311, 333, 334 e 340-345?
056900160818           When  DCTlna = 311  or
057000160818                 DCTlna = 333  or
057100160818                 DCTlna = 334  or
057200160818                 DCTlna = 340  or  DCTlna = 345;
057300160818             exsr  sr_CallFIDN68;
057400160818
057500160818         EndSl;
057600160818
057700160818       ENDSR;
057800160816
057900160816       //--------------------------------------------------------------
058000160816       //?Richiamo *pgm Interrogazione C.A.                            ?
058100160816       //--------------------------------------------------------------
058200160816       BEGSR  sr_CallFIDN01;
058300160816
058400160816         clear  FIDN00ds;
058500160816
058600160816         I00mod = 'I';
058700160816         I00fca = DCTfca;
058800160816         I00fgs = DCTgfc;
058900160816         I00aac = DCTaac;
059000160816         I00fil = DCTfil;
059100160816         I00nca = DCTnca;
059200160816         I00aas = DCTaas;
059300160816         I00lnp = DCTlnp;
059400160816         I00nrs = DCTnrs;
059500160816         I00nsp = DCTnsp;
059600160816         I00lna = DCTlna;
059700160831         I00tpb = 'S';
059800160816         I00tad = DCTtad;
059900160816
060000160816         kpjbu = FIDN00ds;
060100160816
060200160816         fidn01r ( kpjba );
060300160816
060400160816       ENDSR;
060500160818
060600160818       //--------------------------------------------------------------
060700160818       //?Invio/Ristampa Preavviso Danno a partner VanDuuren.          ?
060800160818       //--------------------------------------------------------------
060900160818       BEGSR  sr_CallFIDN39;
061000160818
061100160818         kpjbu = FIDN00ds;
061200160818         fidn39r ( kpjba );
061300160818         FIDN00ds = kpjbu;
061400160818
061500160818         if  o00err <> *blank;
061600160831           if  I00mod = 'G';        // ·?(re-invio e-mail)?
061700160818             o00msg = %trimR( sk_msg(06) ) + ' ' + o00msg;
061800160831           else;                    // ·?(ristampa)?
061900160818             o00msg = %trimR( sk_msg(07) ) + ' ' + o00msg;
062000160818           endif;
062100160818           o00err = 'E';
062200160818         endif;
062300160818
062400160818       ENDSR;
062500160818
062600160818       //--------------------------------------------------------------
062700160818       //?Invio/Ristampa Preavviso Danno a partner UK.                 ?
062800160818       //--------------------------------------------------------------
062900160818       BEGSR  sr_CallFIDN41;
063000160818
063100160818         kpjbu = FIDN00ds;
063200160818         fidn41r ( kpjba );
063300160818         FIDN00ds = kpjbu;
063400160818
063500160818         if  o00err <> *blank;
063600160831           if  I00mod = 'G';        // ·?(re-invio e-mail)?
063700160818             o00msg = %trimR( sk_msg(06) ) + ' ' + o00msg;
063800160831           else;                    // ·?(ristampa)?
063900160818             o00msg = %trimR( sk_msg(07) ) + ' ' + o00msg;
064000160818           endif;
064100160818           o00err = 'E';
064200160818         endif;
064300160818
064400160818       ENDSR;
064500160818
064600160818       //--------------------------------------------------------------
064700160818       //?Invio/Ristampa Preavviso Danno a partner:                    ?
064800160818       //?· 311     = GEL Express Logistik GmbH                        ?
064900160818       //?· 333     = France Express Lyon Corbas                       ?
065000160818       //?· 334     = Calexpress Gennevilliers                         ?
065100160818       //?· 340-345 = Azkar                                            ?
065200160818       //--------------------------------------------------------------
065300160818       BEGSR  sr_CallFIDN68;
065400160818
065500160818         kpjbu = FIDN00ds;
065600160818         fidn68r ( kpjba );
065700160818         FIDN00ds = kpjbu;
065800160818
065900160818         if  o00err <> *blank;
066000160831           //*if  I00mod = 'G';        // ·?(re-invio e-mail)?
066100160818           //*  o00msg = %trimR( sk_msg(06) ) + ' ' + o00msg;
066200160831           //*else;                    // ·?(ristampa)?
066300160818           //*  o00msg = %trimR( sk_msg(07) ) + ' ' + o00msg;
066400160818           //*endif;
066500160818           //*o00err = 'E';
066600160818           // -?Meglio NON segnalare errori (tipo "e-mail mancante") a chi?
066700160818           //  ?NON sarebbe in grado né di capire né di risolvere...?
066800160818           clear  o00err;
066900160818           clear  o00msg;
067000160818         endif;
067100160818
067200160818       ENDSR;
067300160816
067400160816       //--------------------------------------------------------------
067500160816       //?Operazioni finali.                                           ?
067600160816       //--------------------------------------------------------------
067700160816       BEGSR  sr_RoutEnd;
067800160816
067900160816         // -?Chiusura funzioni precedentemente aperte?
068000160816         cloTabella ( c_Tntbe );
068100160816
068200160816         // -?Uscita dal *pgm?
068300160816         return;
068400160816
068500160816       ENDSR;
068600160816
068700160816      /end-free
068800160816
068900160816       //--------------------------------------------------------------
069000160816       //?Definizione schiere a tempo di compilazione                  ?
069100160816       //--------------------------------------------------------------
069200160816
069300160816** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
069400160816Il valore immesso nel campo non è valido                                       1
069500160816Il numero C.A. immesso non esiste                                              2
069600160816La C.A. è annullata                                                            3
069700160831La C.A. non è in gestione alla Sede                                            4
069800160831C.A. non contestabile ad EuroExpress                                           5
069900160816E-MAIL non inviata:                                                            6
070000160818Stampa non riuscita:                                                           7
