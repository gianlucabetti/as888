000100160824       //==============================================================
000200160824       //?Statistica Spedizioni per Num.Tel. Destinatario - Filtro.    ?
000300160824       //==============================================================
000400160824
000500160824       //--------------------------------------------------------------
000600160824       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160824       //--------------------------------------------------------------
000800160824
000900160824     /*PRM dbgview(*source)
001000160824     /*END
001100160824
001200160824       //--------------------------------------------------------------
001300160824       //?Specifiche di controllo.                                     ?
001400160824       //--------------------------------------------------------------
001500160824
001600160824     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160824     h dftactgrp(*no)
001800160824     h bnddir('TRUL')
001900160824
002000160824       //--------------------------------------------------------------
002100160824       //?Dichiarazione file.                                          ?
002200160824       //--------------------------------------------------------------
002300160824
002400160824       // -?Video?
002500160824     fTNSB57D   cf   e             workstn
002600160824     f                                     indds(IndDspF)
002700160824     f                                     infds(InfDspF)
002800160824
002900160824       //--------------------------------------------------------------
003000160824       //?Definizione costanti.                                        ?
003100160824       //--------------------------------------------------------------
003200160824
003300160824       // -?Tasti funzionali a video?
003400160824     d c_F01           c                   const(x'31')
003500160824     d c_F02           c                   const(x'32')
003600160824     d c_F03           c                   const(x'33')
003700160824     d c_F04           c                   const(x'34')
003800160824     d c_F05           c                   const(x'35')
003900160824     d c_F06           c                   const(x'36')
004000160824     d c_F07           c                   const(x'37')
004100160824     d c_F08           c                   const(x'38')
004200160824     d c_F09           c                   const(x'39')
004300160824     d c_F10           c                   const(x'3A')
004400160824     d c_F11           c                   const(x'3B')
004500160824     d c_F12           c                   const(x'3C')
004600160824     d c_F13           c                   const(x'B1')
004700160824     d c_F14           c                   const(x'B2')
004800160824     d c_F15           c                   const(x'B3')
004900160824     d c_F16           c                   const(x'B4')
005000160824     d c_F17           c                   const(x'B5')
005100160824     d c_F18           c                   const(x'B6')
005200160824     d c_F19           c                   const(x'B7')
005300160824     d c_F20           c                   const(x'B8')
005400160824     d c_F21           c                   const(x'B9')
005500160824     d c_F22           c                   const(x'BA')
005600160824     d c_F23           c                   const(x'BB')
005700160824     d c_F24           c                   const(x'BC')
005800160824     d c_Enter         c                   const(x'F1')
005900160824     d c_RollDown      c                   const(x'F4')
006000160824     d c_RollUp        c                   const(x'F5')
006100160824
006200160824       //--------------------------------------------------------------
006300160824       //?Definizione schiere.                                         ?
006400160824       //--------------------------------------------------------------
006500160824
006600160824       // -?Messaggi di errore?
006700160825     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
006800160824
006900160824       //--------------------------------------------------------------
007000160824       //?Definizione aree dati.                                       ?
007100160824       //--------------------------------------------------------------
007200160824
007300160824       // -?Dati utente?
007400160824     d §AzUte        e ds                  extname(AZUTE00F)
007500160824     d                                     dtaara
007600160824     d §DatiUte      e ds                  extname(dDatiUte)
007700160824     d                                     dtaara
007800160824
007900160824       //--------------------------------------------------------------
008000160824       //?Definizione strutture dati.                                  ?
008100160824       //--------------------------------------------------------------
008200160824
008300160824       // -?InfDS?
008400160824     d InfDspF         ds
008500160824     d   dsp_aid             369    369a
008600160824
008700160824       // -?Indicatori su DspF?
008800160824     d IndDspF         ds                  inz
008900160824         // -?Emissione messaggio di errore?
009000160824     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009100160824         // -?Posizionamento cursore & segnalazione errore?
009200160824     d   $PosCurTEL                    n   overlay( IndDspF : 51 )
009300160824     d   $PosCurDSI                    n   overlay( IndDspF : 52 )
009400160824     d   $PosCurDSF                    n   overlay( IndDspF : 53 )
009500160824     d   $PosCurCWF                    n   overlay( IndDspF : 54 )
009600160824         //   -?Riemissione videata?
009700160824     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
009800160824
009900160824       // -?Status ds?
010000160824     d Status         sds
010100160824     d   SDSpgmName      *proc
010200160824
010300160824       // -?Parametri ricevuti?
010400160824     d KPJBA         e ds
010500160824
010600160824       // -?Tabella "$3" = Date avanzamento trasferimenti?
010700160824     d ds$3          e ds                  inz   qualified
010800160824
010900160824       //--------------------------------------------------------------
011000160824       //?Definizione variabili globali.                               ?
011100160824       //--------------------------------------------------------------
011200160824
011300160824       // -?Flags booleani?
011400160824     d $Fine           s               n   inz(*off)
011500160824     d $InzD01         s               n   inz(*on)
011600160824
011700160824       // -?Variabili per la gestione del video?
011800160824     d $Video          s              2    inz('D1')
011900160825
012000160825       // -?Variabili di comodo?
012100160825     d wDate_Eur       s               d   datfmt(*EUR)
012200160825     d                                     inz
012300160824
012400160824       //--------------------------------------------------------------
012500160824       //?Definizione prototipi procedure.                             ?
012600160824       //--------------------------------------------------------------
012700160824
012800160824       // -?Reperimento dati utente?
012900160824     d TIBS34ds      e ds                  inz
013000160824      /copy gaitrasrc/srcProtoPR,TIBS34R
013100160824
013200160824       // -?Reperimento dati tabelle?
013300160824      /copy gaitrasrc/srcProtoPI,TRULTAB
013400160824      /copy gaitrasrc/srcProtoPR,TRULTAB
013500160824
013600160824       // -?Controllo ed inversione date?
013700160824     d WLBdat          ds                  inz
013800160824     d   G08dat                       8  0 inz
013900160824     d   G08inv                       8  0 inz
014000160824     d   G08err                       1    inz('3')
014100160824     d   G08tgi                       5  0 inz
014200160824      /copy gaitrasrc/srcProtoPr,XSRDA8
014300160824
014400160824       // -?Richiamo diretto lavoro batch?
014500160824     d TNSB58ds      e ds                  inz   qualified
014600160824     d tnsb58r         pr                  extpgm('TNSB58R')
014700160824     d   kpjba                             likeds(KPJBA)
014800160824
014900160824       // -?Personalizzazione lavoro batch?
015000160824     d*// Bch09           pr                  extpgm('BCH09')
015100160824     d*//   kpjba                             likeds(KPJBA)
015200160824
015300160824       // -?Sottomissione lavoro batch?
015400160824      /copy gaitrasrc/srcProtoPR,BCH10
015500160825
015600160825       // -?Parametri API QCAPCMD (Process Commands)?
015700160825     d Qcmd            s           2048    inz  varying
015800160825      /copy qSysInc/qRpgleSrc,QCAPCMD
015900160825       // -?API QCAPCMD (Process Commands)?
016000160825      /copy gaitrasrc/srcProtoPR,QCAPCMD
016100160825
016200160825       // -?Parametri gestione errori API.?
016300160825      /copy qsysinc/qrpglesrc,QUSEC
016400160824
016500160824       //--------------------------------------------------------------
016600160824       //?Definizione key-list.                                        ?
016700160824       //--------------------------------------------------------------
016800160824
016900160824
017000160824       //--------------------------------------------------------------
017100160824       //?M A I N - L I N E                                            ?
017200160824       //--------------------------------------------------------------
017300160824
017400160824     c     *Entry        plist
017500160824     c                   parm                    KPJBA
017600160824
017700160824      /free
017800160824
017900160824       // -?Operazioni iniziali?
018000160824       exsr  sr_RoutInz;
018100160824
018200160824       // -?Ciclo di gestione del file video?
018300160824       DoW  $Fine = *off;
018400160824
018500160824         select;
018600160824
018700160824           // -?Richiesta parametri iniziali?
018800160824           when $Video = 'D1';
018900160824             exsr  sr_GesD01;
019000160824
019100160824           // -?? ? ??
019200160824           other;
019300160824             $Fine = *on;
019400160824
019500160824         endsl;
019600160824
019700160824       EndDo;
019800160824
019900160824       // -?Operazioni finali?
020000160824       exsr sr_RoutEnd;
020100160824
020200160824       //--------------------------------------------------------------
020300160824       //?Operazioni iniziali.                                         ?
020400160824       //--------------------------------------------------------------
020500160824       BEGSR  sr_RoutInz;
020600160824
020700160824         // -?Impostazione chiusura?
020800160824         *inLR = *on;
020900160824
021000160824         // -?Reperimento dati job?
021100160824         exsr  sr_DatiJob;
021200160824
021300160824         // -?Impostazione nome programma a video?
021400160824         VT1pgm = SDSpgmName;
021500160824
021600160824         // -?Pulizia parametri per Batch?
021700160824         clear  TNSB58ds;
021800160824
021900160824       ENDSR;
022000160824
022100160824       //--------------------------------------------------------------
022200160824       //?Reperimento Dati del job (Utente/Operativi).                 ?
022300160824       //--------------------------------------------------------------
022400160824       BEGSR  sr_DatiJob;
022500160824
022600160824         in(e) §AzUte;
022700160824         if NOT %error;
022800160824           in(e) §DatiUte;
022900160824         endif;
023000160824         if %error or RSut = *blank;
023100160824           tibs34r ( tibs34ds );
023200160824           in §AzUte;
023300160824           in §DatiUte;
023400160824         endif;
023500160824
023600160824       ENDSR;
023700160824
023800160824       //--------------------------------------------------------------
023900160824       //?Gestione videata D01.                                        ?
024000160824       //--------------------------------------------------------------
024100160824       BEGSR  sr_GesD01;
024200160824
024300160824         // -?Inizializzazione videata?
024400160824         if  $InzD01 = *on;
024500160824           exsr  sr_InzD01;
024600160824           $InzD01 = *off;
024700160824         endif;
024800160824
024900160824         // -?Emissione videata?
025000160824         write  SB57T01;
025100160824         exfmt  SB57D01;
025200160824
025300160824         clear  VIDmsg;
025400160824         reset  $ErrMessage;
025500160824         reset  $ErrGenerico;
025600160824
025700160824         Select;
025800160824
025900160824           // -?Errore: sistema informativo errato?
026000160824           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
026100160824             $Fine  = *on;
026200160824
026300160824           // -?F3=Fine?
026400160824           When  dsp_aid = c_F03;
026500160824             $Fine = *on;
026600160825
026700160825           // -?F9=Visualizzazione WrkF WFTELD0F?
026800160825           WHEN  dsp_aid = c_F09;
026900160825             Qcmd = 'DSPDBF file(*libl/WFTELD0F) mbr(*first) output(*)';
027000160825             exsr  sr_ExecCmd;
027100160824
027200160824           // -?Invio / F6=Conferma?
027300160824           Other;
027400160824             exsr  sr_CtrD01;
027500160824             if  $ErrGenerico = *on;
027600160824               leavesr;
027700160824             endif;
027800160824             if  dsp_aid = c_F06;
027900160824               exsr  sr_SbmJob;
028000160824               $Fine = *on;
028100160824             endif;
028200160824
028300160824         ENDSL;
028400160824
028500160824       ENDSR;
028600160824
028700160824       //--------------------------------------------------------------
028800160824       //?Inizializzazione videata D01.                                ?
028900160824       //--------------------------------------------------------------
029000160824       BEGSR  sr_InzD01;
029100160824
029200160824         clear  SB57D01;
029300160824
029400160824         // -?Impostazione messaggio di avvertimento?
029500160824         //  ?con la data dell'ultima pulizia del *file TITAS10F?
029600160824         clear  ds$3;
029700160824         ds_TNTBE.TBEke1 = '1';
029800160824         if  getTabella ( c_Tabel : '$3'  : ds_TNTBE.TBEke1 :
029900160824                          *omit : *omit : *omit :
030000160824                          *omit : *omit :
030100160824                          *omit : *omit : *omit : *omit :
030200160824                          *omit : *omit : *omit : *omit :
030300160824                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
030400160824                        = *zero      AND
030500160824             ds_TNTBE.TBEatb = *blank;
030600160824           ds$3   = ds_TNTBE.TBEuni;
030700160824         endif;
030800160824         if  ds$3.§$3dp0 > *zero;
030900160824           reset  WLBdat;
031000160824           G08inv = ds$3.§$3dp0;
031100160824           XSRDA8 ( WLBdat );
031200160824           if  G08err = *off;
031300160825             wDate_Eur = %date( G08dat : *eur ) + %days(1);
031400160825             V1Ddp0 = %dec( wDate_Eur );
031500160824           endif;
031600160824         endif;
031700160824
031800160824         // -?Controllo del sistema informativo?
031900160824         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
032000160824           VIDmsg = sk_Msg(01);
032100160824           $ErrMessage  = *on;
032200160824           $ErrGenerico = *on;
032300160824           leavesr;
032400160824         endif;
032500160824
032600160824       ENDSR;
032700160824
032800160824       //--------------------------------------------------------------
032900160824       //?Controllo dati nella videata D01.                            ?
033000160824       //--------------------------------------------------------------
033100160824       BEGSR  sr_CtrD01;
033200160824
033300160824         // -?Spegnimento indicatori di posizionamento cursore?
033400160824         %subst( IndDspF : 50 ) = *off;
033500160824
033600160824         // -?Numero Telefono da ricercare?
033700160824         if  V1Ctel = *blank;
033800160824           $PosCurTEL   = *on;
033900160824           $ErrMessage  = *on;
034000160824           $ErrGenerico = *on;
034100160824           VIDmsg = sk_Msg(02);
034200160824           leavesr;
034300160824         endif;
034400160824
034500160824         // -?Date spedizioni?
034600160824         if  V1Cdsi = *zero  and  V1Cdsf = *zero;
034700160824           $PosCurDSI   = *on;
034800160824           $ErrMessage  = *on;
034900160824           $ErrGenerico = *on;
035000160824           VIDmsg = sk_Msg(03);
035100160824           leavesr;
035200160824         endif;
035300160824
035400160824         // -?Data spedizione iniziale?
035500160824         clear  WLBdat;
035600160824         if  V1Cdsi = *zero;
035700160824           $PosCurDSI   = *on;
035800160824           $ErrMessage  = *on;
035900160824           $ErrGenerico = *on;
036000160824           VIDmsg = sk_Msg(04);
036100160824           leavesr;
036200160824         endif;
036300160824         G08dat = V1Cdsi;
036400160824         xsrda8 ( WLBdat );
036500160824         if  G08err =  *on;
036600160824           $PosCurDSI   = *on;
036700160824           $ErrMessage  = *on;
036800160824           $ErrGenerico = *on;
036900160824           VIDmsg = sk_Msg(05);
037000160824           leavesr;
037100160824         endif;
037200160824         V1Cdsi  = G08dat;
037300160824         TNSB58ds.SB58dsi = G08inv;
037400160825
037500160825         if  TNSB58ds.SB58dsi < ds$3.§$3dp0;
037600160825           $PosCurDSI   = *on;
037700160825           $ErrMessage  = *on;
037800160825           $ErrGenerico = *on;
037900160825           VIDmsg = %trimR( sk_Msg(06) ) + ' ' +
038000160825                    %editc( V1Ddp0 : 'Y' );
038100160825           leavesr;
038200160825         endif;
038300160824
038400160824         // -?Data spedizione finale?
038500160824         clear  WLBdat;
038600160824         if  V1Cdsf = *zero;
038700160824           $PosCurDSF   = *on;
038800160824           $ErrMessage  = *on;
038900160824           $ErrGenerico = *on;
039000160824           VIDmsg = sk_Msg(04);
039100160824           leavesr;
039200160824         endif;
039300160824         G08dat = V1Cdsf;
039400160824         xsrda8 ( WLBdat );
039500160824         if  G08err =  *on;
039600160824           $PosCurDSF   = *on;
039700160824           $ErrMessage  = *on;
039800160824           $ErrGenerico = *on;
039900160824           VIDmsg = sk_Msg(05);
040000160824           leavesr;
040100160824         endif;
040200160824         V1Cdsf  = G08dat;
040300160824         TNSB58ds.SB58dsf = G08inv;
040400160824
040500160824         // -?Range date spedizioni?
040600160824         if  TNSB58ds.SB58dsi > TNSB58ds.SB58dsf;
040700160824           $PosCurDSI   = *on;
040800160824           $ErrMessage  = *on;
040900160824           $ErrGenerico = *on;
041000160825           VIDmsg = sk_Msg(07);
041100160824           leavesr;
041200160824         endif;
041300160824
041400160824         // -?Flag "Solo Totali"?
041500160824         if  V1CclrWF <> 'N'  and  V1CclrWF <> 'S';
041600160824           $PosCurCWF   = *on;
041700160824           $ErrMessage  = *on;
041800160824           $ErrGenerico = *on;
041900160825           VIDmsg = sk_Msg(08);
042000160824           leavesr;
042100160824         endif;
042200160824
042300160824       ENDSR;
042400160824
042500160824       //--------------------------------------------------------------
042600160824       //?Sottomissione lavoro batch                                   ?
042700160824       //--------------------------------------------------------------
042800160824       BEGSR  sr_SbmJob;
042900160824
043000160824         // -?Lancio?
043100160824         TNSB58ds.SB58cwf = V1CclrWF;
043200160824         TNSB58ds.SB58tel = V1Ctel;
043300160824         kpjbu = TNSB58ds;
043400160824         kcoaz = 'SB58';
043500160824         if  KNMUS = *all'1';
043600160824           tnsb58r ( kpjba );
043700160824         else;
043800160824           bch10 ( kpjba );
043900160824         endif;
044000160824
044100160824       ENDSR;
044200160825
044300160825       //--------------------------------------------------------------
044400160825       //?Esecuzione del comando (già impostato).                      ?
044500160825       //--------------------------------------------------------------
044600160825       BEGSR  sr_ExecCmd;
044700160825
044800160825         clear Qcap0100;
044900160825         Qcabcsdh = *off;
045000160825         Qcapa    = *off;
045100160825         Qcacmdss = *off;
045200160825         Qcaerved = *allX'00';
045300160825
045400160825         clear Qusec;
045500160825         Qusbprv  = %size(Qusec);
045600160825
045700160825         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
045800160825                           %size(Qcap0100) : 'CPOP0100' : *omit :
045900160825                           0 : 0 : Qusec);
046000160825
046100160825         //if  Qusei <> *blank;
046200160825         //  ...;
046300160825         //endif;
046400160825
046500160825       ENDSR;
046600160824
046700160824       //--------------------------------------------------------------
046800160824       //?Operazioni finali.                                           ?
046900160824       //--------------------------------------------------------------
047000160824       BEGSR  sr_RoutEnd;
047100160824
047200160824         // -?Chiusura funzioni precedentemente aperte?
047300160824         cloTabella ( c_Tabel );
047400160824
047500160824         // -?Chiusura pgm?
047600160824         return;
047700160824
047800160824       ENDSR;
047900160824
048000160824      /end-free
048100160824
048200160824       //--------------------------------------------------------------
048300160824       //?Schiere a tempo di compilazione.
048400160824       //--------------------------------------------------------------
048500160824
048600160824** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
048700160824Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire       1
048800160824Numero di Telefono da ricercare obbligatorio                                   2
048900160824Impostare il periodo da elaborare                                              3
049000160824Data obbligatoria                                                              4
049100160824Data formalmente errata                                                        5
049200160825Data precedente quella della prima bolla disponibile:                          6
049300160825Data iniziale incongruente con data finale                                     7
049400160825Immesso un valore errato - ammessi: "N" = No, "S" = Sì                         8
