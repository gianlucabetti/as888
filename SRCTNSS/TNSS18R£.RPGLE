000100180220       //==============================================================
000200180220       //? Report Spedizioni con Merce Incompatibile - Filtro.          ?
000300180220       //==============================================================
000400180220
000500180220       //--------------------------------------------------------------
000600180220       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180220       //--------------------------------------------------------------
000800180220
000900180220     /*PRM dbgview(*source)
001000180220     /*END
001100180220
001200180220       //--------------------------------------------------------------
001300180220       //? Specifiche di controllo.                                     ?
001400180220       //--------------------------------------------------------------
001500180220
001600180220     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700180220     h dftactgrp(*no)
001800180220     h bnddir('TRUL')
001900180220
002000180220       //--------------------------------------------------------------
002100180220       //? Dichiarazione file.                                          ?
002200180220       //--------------------------------------------------------------
002300180220
002400180220       // - ?Video?
002500180220     fTNSS18D   cf   e             workstn
002600180220     f                                     indds(IndDspF)
002700180220     f                                     infds(InfDspF)
002800180220
002900180220       //--------------------------------------------------------------
003000180220       //?Definizione costanti.                                        ?
003100180220       //--------------------------------------------------------------
003200180220
003300180220       // -?Tasti funzionali a video?
003400180220     d c_F01           c                   const(x'31')
003500180220     d c_F02           c                   const(x'32')
003600180220     d c_F03           c                   const(x'33')
003700180220     d c_F04           c                   const(x'34')
003800180220     d c_F05           c                   const(x'35')
003900180220     d c_F06           c                   const(x'36')
004000180220     d c_F07           c                   const(x'37')
004100180220     d c_F08           c                   const(x'38')
004200180220     d c_F09           c                   const(x'39')
004300180220     d c_F10           c                   const(x'3A')
004400180220     d c_F11           c                   const(x'3B')
004500180220     d c_F12           c                   const(x'3C')
004600180220     d c_F13           c                   const(x'B1')
004700180220     d c_F14           c                   const(x'B2')
004800180220     d c_F15           c                   const(x'B3')
004900180220     d c_F16           c                   const(x'B4')
005000180220     d c_F17           c                   const(x'B5')
005100180220     d c_F18           c                   const(x'B6')
005200180220     d c_F19           c                   const(x'B7')
005300180220     d c_F20           c                   const(x'B8')
005400180220     d c_F21           c                   const(x'B9')
005500180220     d c_F22           c                   const(x'BA')
005600180220     d c_F23           c                   const(x'BB')
005700180220     d c_F24           c                   const(x'BC')
005800180220     d c_Enter         c                   const(x'F1')
005900180220     d c_RollDown      c                   const(x'F4')
006000180220     d c_RollUp        c                   const(x'F5')
006100180220
006200180220       //--------------------------------------------------------------
006300180220       //? Definizione schiere.                                         ?
006400180220       //--------------------------------------------------------------
006500180220
006600180220       // - ?Messaggi di errore?
006700180220     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
006800180220
006900180220       //--------------------------------------------------------------
007000180220       //? Definizione aree dati.                                       ?
007100180220       //--------------------------------------------------------------
007200180220
007300180220       // - ?Dati utente?
007400180220     d §AzUte        e ds                  extname(AZUTE00F)
007500180220     d                                     dtaara
007600180220     d §DatiUte      e ds                  extname(dDatiUte)
007700180220     d                                     dtaara
007800180220
007900180220       //--------------------------------------------------------------
008000180220       //? Definizione strutture dati.                                  ?
008100180220       //--------------------------------------------------------------
008200180220
008300180220       // - ?InfDS?
008400180220     d InfDspF         ds
008500180220     d   dsp_aid             369    369a
008600180220
008700180220       // - ?Indicatori su DspF?
008800180220     d IndDspF         ds                  inz
008900180220         // - ?Emissione messaggio di errore?
009000180220     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009100180220         // - ?Posizionamento cursore & segnalazione errore?
009200180220     d   $PosCurDSI                    n   overlay( IndDspF : 51 )
009300180220     d   $PosCurDSF                    n   overlay( IndDspF : 52 )
009400180220     d   $PosCurCWF                    n   overlay( IndDspF : 53 )
009500180220         // - ?Riemissione videata?
009600180220     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
009700180220
009800180220       // - ?Status ds?
009900180220     d Status         sds
010000180220     d   SDSpgmName      *proc
010100180220
010200180220       // - ?Parametri ricevuti?
010300180220     d KPJBA         e ds
010400180220
010500180220      *//· // - ?Tabella "$3" = Date avanzamento trasferimenti?
010600180220     d*//· ds$3          e ds                  inz   qualified
010700180220
010800180220       //--------------------------------------------------------------
010900180220       //? Definizione variabili globali.                               ?
011000180220       //--------------------------------------------------------------
011100180220
011200180220       // - ?Flags booleani?
011300180220     d $Fine           s               n   inz(*off)
011400180220     d $InzD01         s               n   inz(*on)
011500180220
011600180220       // - ?Variabili per la gestione del video?
011700180220     d $Video          s              2    inz('D1')
011800180220
011900180220      *//· // - ?Variabili di comodo?
012000180220     d*//· wDate_Eur       s               d   datfmt(*EUR)
012100180220     d*//·                                     inz
012200180220
012300180220       //--------------------------------------------------------------
012400180220       //? Definizione prototipi procedure.                             ?
012500180220       //--------------------------------------------------------------
012600180220
012700180220       // - ?Reperimento dati utente?
012800180220     d TIBS34ds      e ds                  inz
012900180220      /copy gaitrasrc/srcProtoPR,TIBS34R
013000180220
013100180220      *//· // - ?Reperimento dati tabelle?
013200180220      *//·/copy gaitrasrc/srcProtoPI,TRULTAB
013300180220      *//·/copy gaitrasrc/srcProtoPR,TRULTAB
013400180220
013500180220       // - ?Controllo ed inversione date?
013600180220     d WLBdat          ds                  inz
013700180220     d   G08dat                       8  0 inz
013800180220     d   G08inv                       8  0 inz
013900180220     d   G08err                       1    inz('3')
014000180220     d   G08tgi                       5  0 inz
014100180220      /copy gaitrasrc/srcProtoPr,XSRDA8
014200180220
014300180220       // - ?Richiamo diretto lavoro batch?
014400180220     d TNSS19ds      e ds                  inz   qualified
014500180220     d tnss19r         pr                  extpgm('TNSS19R')
014600180220     d   kpjba                             likeds(KPJBA)
014700180220
014800180220      *//· // - ?Personalizzazione lavoro batch?
014900180220     d*//· Bch09           pr                  extpgm('BCH09')
015000180220     d*//·   kpjba                             likeds(KPJBA)
015100180220
015200180220       // - ?Sottomissione lavoro batch?
015300180220      /copy gaitrasrc/srcProtoPR,BCH10
015400180220
015500180220       // - ?Parametri API QCAPCMD (Process Commands)?
015600180220     d Qcmd            s           2048    inz  varying
015700180220      /copy qSysInc/qRpgleSrc,QCAPCMD
015800180220       // - ?API QCAPCMD (Process Commands)?
015900180220      /copy gaitrasrc/srcProtoPR,QCAPCMD
016000180220
016100180220       // - ?Parametri gestione errori API.?
016200180220      /copy qsysinc/qrpglesrc,QUSEC
016300180220
016400180220       //--------------------------------------------------------------
016500180220       //? Definizione key-list.                                        ?
016600180220       //--------------------------------------------------------------
016700180220
016800180220
016900180220       //--------------------------------------------------------------
017000180220       //? M A I N - L I N E                                            ?
017100180220       //--------------------------------------------------------------
017200180220
017300180220     c     *Entry        plist
017400180220     c                   parm                    KPJBA
017500180220
017600180220      /free
017700180220
017800180220       // - ?Operazioni iniziali?
017900180220       exsr  sr_RoutInz;
018000180220
018100180220       // - ?Ciclo di gestione del file video?
018200180220       DoW  $Fine = *off;
018300180220
018400180220         select;
018500180220
018600180220           // - ?Richiesta parametri iniziali?
018700180220           when $Video = 'D1';
018800180220             exsr  sr_GesD01;
018900180220
019000180220           // - ?? ? ??
019100180220           other;
019200180220             $Fine = *on;
019300180220
019400180220         endsl;
019500180220
019600180220       EndDo;
019700180220
019800180220       // - ?Operazioni finali?
019900180220       exsr sr_RoutEnd;
020000180220
020100180220       //--------------------------------------------------------------
020200180220       //? Operazioni iniziali.                                         ?
020300180220       //--------------------------------------------------------------
020400180220       BEGSR  sr_RoutInz;
020500180220
020600180220         // - ?Impostazione chiusura?
020700180220         *inLR = *on;
020800180220
020900180220         // - ?Reperimento dati job?
021000180220         exsr  sr_DatiJob;
021100180220
021200180220         // - ?Impostazione nome programma a video?
021300180220         VT1pgm = SDSpgmName;
021400180220
021500180220         // - ?Pulizia parametri per Batch?
021600180220         clear  TNSS19ds;
021700180220
021800180220       ENDSR;
021900180220
022000180220       //--------------------------------------------------------------
022100180220       //? Reperimento Dati del job (Utente/Operativi).                 ?
022200180220       //--------------------------------------------------------------
022300180220       BEGSR  sr_DatiJob;
022400180220
022500180220         in(e) §AzUte;
022600180220         if NOT %error;
022700180220           in(e) §DatiUte;
022800180220         endif;
022900180220         if %error or RSut = *blank;
023000180220           tibs34r ( tibs34ds );
023100180220           in §AzUte;
023200180220           in §DatiUte;
023300180220         endif;
023400180220
023500180220       ENDSR;
023600180220
023700180220       //--------------------------------------------------------------
023800180220       //? Gestione videata D01.                                        ?
023900180220       //--------------------------------------------------------------
024000180220       BEGSR  sr_GesD01;
024100180220
024200180220         // - ?Inizializzazione videata?
024300180220         if  $InzD01 = *on;
024400180220           exsr  sr_InzD01;
024500180220           $InzD01 = *off;
024600180220         endif;
024700180220
024800180220         // - ?Emissione videata?
024900180220         write  SS18T01;
025000180220         exfmt  SS18D01;
025100180220
025200180220         clear  VIDmsg;
025300180220         reset  $ErrMessage;
025400180220         reset  $ErrGenerico;
025500180220
025600180220         Select;
025700180220
025800180220           // - ?Errore: sistema informativo errato?
025900180220           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
026000180220             $Fine  = *on;
026100180220
026200180220           // - ?F3=Fine?
026300180220           When  dsp_aid = c_F03;
026400180220             $Fine = *on;
026500180220
026600180222           // - ?F9=Visualizzazione WrkF WFRMI00F?
026700180220           WHEN  dsp_aid = c_F09;
026800180222             Qcmd = 'DSPDBF file(*libl/WFRMI00F) mbr(*first) output(*)';
026900180220             exsr  sr_ExecCmd;
027000180220
027100180220           // - ?Invio / F6=Conferma?
027200180220           Other;
027300180220             exsr  sr_CtrD01;
027400180220             if  $ErrGenerico = *on;
027500180220               leavesr;
027600180220             endif;
027700180220             if  dsp_aid = c_F06;
027800180220               exsr  sr_SbmJob;
027900180220               $Fine = *on;
028000180220             endif;
028100180220
028200180220         ENDSL;
028300180220
028400180220       ENDSR;
028500180220
028600180220       //--------------------------------------------------------------
028700180220       //? Inizializzazione videata D01.                                ?
028800180220       //--------------------------------------------------------------
028900180220       BEGSR  sr_InzD01;
029000180220
029100180220         clear  SS18D01;
029200180220
029300180220         //*·// - ?Impostazione messaggio di avvertimento?
029400180220         //*·//   ?con la data dell'ultima pulizia del *file TITAS10F?
029500180220         //*·clear  ds$3;
029600180220         //*·ds_TNTBE.TBEke1 = '1';
029700180220         //*·if  getTabella ( c_Tabel : '$3'  : ds_TNTBE.TBEke1 :
029800180220         //*·                 *omit : *omit : *omit :
029900180220         //*·                 *omit : *omit :
030000180220         //*·                 *omit : *omit : *omit : *omit :
030100180220         //*·                 *omit : *omit : *omit : *omit :
030200180220         //*·                 ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
030300180220         //*·               = *zero      AND
030400180220         //*·    ds_TNTBE.TBEatb = *blank;
030500180220         //*·  ds$3   = ds_TNTBE.TBEuni;
030600180220         //*·endif;
030700180220         //*·if  ds$3.§$3dp0 > *zero;
030800180220         //*·  reset  WLBdat;
030900180220         //*·  G08inv = ds$3.§$3dp0;
031000180220         //*·  XSRDA8 ( WLBdat );
031100180220         //*·  if  G08err = *off;
031200180220         //*·    wDate_Eur = %date( G08dat : *eur ) + %days(1);
031300180220         //*·    V1Ddp0 = %dec( wDate_Eur );
031400180220         //*·  endif;
031500180220         //*·endif;
031600180220
031700180220         // - ?Controllo del sistema informativo?
031800180220         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
031900180220           VIDmsg = sk_Msg(01);
032000180220           $ErrMessage  = *on;
032100180220           $ErrGenerico = *on;
032200180220           leavesr;
032300180220         endif;
032400180220
032500180220       ENDSR;
032600180220
032700180220       //--------------------------------------------------------------
032800180220       //? Controllo dati nella videata D01.                            ?
032900180220       //--------------------------------------------------------------
033000180220       BEGSR  sr_CtrD01;
033100180220
033200180220         // - ?Spegnimento indicatori di posizionamento cursore?
033300180220         %subst( IndDspF : 50 ) = *off;
033400180220
033500180220         // - ?Date spedizioni?
033600180220         if  V1Cdsi = *zero  and  V1Cdsf = *zero;
033700180220           $PosCurDSI   = *on;
033800180220           $ErrMessage  = *on;
033900180220           $ErrGenerico = *on;
034000180220           VIDmsg = sk_Msg(03);
034100180220           leavesr;
034200180220         endif;
034300180220
034400180220         // - ?Data spedizione iniziale?
034500180220         clear  WLBdat;
034600180220         if  V1Cdsi = *zero;
034700180220           $PosCurDSI   = *on;
034800180220           $ErrMessage  = *on;
034900180220           $ErrGenerico = *on;
035000180220           VIDmsg = sk_Msg(04);
035100180220           leavesr;
035200180220         endif;
035300180220         G08dat = V1Cdsi;
035400180220         xsrda8 ( WLBdat );
035500180220         if  G08err =  *on;
035600180220           $PosCurDSI   = *on;
035700180220           $ErrMessage  = *on;
035800180220           $ErrGenerico = *on;
035900180220           VIDmsg = sk_Msg(05);
036000180220           leavesr;
036100180220         endif;
036200180220         V1Cdsi  = G08dat;
036300180220         TNSS19ds.SS19dsi = G08inv;
036400180220
036500180220         //*·if  TNSS19ds.SS19dsi < ds$3.§$3dp0;
036600180220         //*·  $PosCurDSI   = *on;
036700180220         //*·  $ErrMessage  = *on;
036800180220         //*·  $ErrGenerico = *on;
036900180220         //*·  VIDmsg = %trimR( sk_Msg(06) ) + ' ' +
037000180220         //*·           %editc( V1Ddp0 : 'Y' );
037100180220         //*·  leavesr;
037200180220         //*·endif;
037300180220
037400180220         // - ?Data spedizione finale?
037500180220         clear  WLBdat;
037600180220         if  V1Cdsf = *zero;
037700180220           $PosCurDSF   = *on;
037800180220           $ErrMessage  = *on;
037900180220           $ErrGenerico = *on;
038000180220           VIDmsg = sk_Msg(04);
038100180220           leavesr;
038200180220         endif;
038300180220         G08dat = V1Cdsf;
038400180220         xsrda8 ( WLBdat );
038500180220         if  G08err =  *on;
038600180220           $PosCurDSF   = *on;
038700180220           $ErrMessage  = *on;
038800180220           $ErrGenerico = *on;
038900180220           VIDmsg = sk_Msg(05);
039000180220           leavesr;
039100180220         endif;
039200180220         V1Cdsf  = G08dat;
039300180220         TNSS19ds.SS19dsf = G08inv;
039400180220
039500180220         // - ?Range date spedizioni?
039600180220         if  TNSS19ds.SS19dsi > TNSS19ds.SS19dsf;
039700180220           $PosCurDSI   = *on;
039800180220           $ErrMessage  = *on;
039900180220           $ErrGenerico = *on;
040000180220           VIDmsg = sk_Msg(07);
040100180220           leavesr;
040200180220         endif;
040300180220
040400180220         // - ?Flag "Pulizia WFRMI00F"?
040500180220         if  V1CclrWF <> 'N'  and  V1CclrWF <> 'S';
040600180220           $PosCurCWF   = *on;
040700180220           $ErrMessage  = *on;
040800180220           $ErrGenerico = *on;
040900180220           VIDmsg = sk_Msg(08);
041000180220           leavesr;
041100180220         endif;
041200180220
041300180220       ENDSR;
041400180220
041500180220       //--------------------------------------------------------------
041600180220       //? Sottomissione lavoro batch                                   ?
041700180220       //--------------------------------------------------------------
041800180220       BEGSR  sr_SbmJob;
041900180220
042000180220         // - ?Lancio?
042100180220         TNSS19ds.SS19cwf = V1CclrWF;
042200180220         kpjbu = TNSS19ds;
042300180220         kcoaz = 'SS19';
042400180220         if  KNMUS = *all'1';
042500180220           tnss19r ( kpjba );
042600180220         else;
042700180220           bch10 ( kpjba );
042800180220         endif;
042900180220
043000180220       ENDSR;
043100180220
043200180220       //--------------------------------------------------------------
043300180220       //? Esecuzione del comando (già impostato).                      ?
043400180220       //--------------------------------------------------------------
043500180220       BEGSR  sr_ExecCmd;
043600180220
043700180220         clear Qcap0100;
043800180220         Qcabcsdh = *off;
043900180220         Qcapa    = *off;
044000180220         Qcacmdss = *off;
044100180220         Qcaerved = *allX'00';
044200180220
044300180220         clear Qusec;
044400180220         Qusbprv  = %size(Qusec);
044500180220
044600180220         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
044700180220                           %size(Qcap0100) : 'CPOP0100' : *omit :
044800180220                           0 : 0 : Qusec);
044900180220
045000180220         //if  Qusei <> *blank;
045100180220         //  ...;
045200180220         //endif;
045300180220
045400180220       ENDSR;
045500180220
045600180220       //--------------------------------------------------------------
045700180220       //? Operazioni finali.                                           ?
045800180220       //--------------------------------------------------------------
045900180220       BEGSR  sr_RoutEnd;
046000180220
046100180220         // - ?Chiusura funzioni precedentemente aperte?
046200180220         //*·cloTabella ( c_Tabel );
046300180220
046400180220         // - ?Chiusura pgm?
046500180220         return;
046600180220
046700180220       ENDSR;
046800180220
046900180220      /end-free
047000180220
047100180220       //--------------------------------------------------------------
047200180220       //? Schiere a tempo di compilazione.
047300180220       //--------------------------------------------------------------
047400180220
047500180220** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
047600180220Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire       1
047700180220Numero di Telefono da ricercare obbligatorio                                   2
047800180220Impostare il periodo da elaborare                                              3
047900180220Data obbligatoria                                                              4
048000180220Data formalmente errata                                                        5
048100180220Data precedente quella della prima bolla disponibile:                          6
048200180220Data iniziale incongruente con data finale                                     7
048300180220Immesso un valore errato - ammessi: "N" = No, "S" = Sì                         8
