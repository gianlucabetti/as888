000100150724       //==============================================================
000200150724       //?Elenco Spedizioni Giornaliere con Imp. d'Assicurare eccessivo?
000300150724       //==============================================================
000400150724
000500150724       //--------------------------------------------------------------
000600150724       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150724       //--------------------------------------------------------------
000800150724
000900150724     /*PRM dbgview(*source)
001000150724     /*END
001100150724
001200150724       //--------------------------------------------------------------
001300150724       //?Specifiche di controllo.                                     ?
001400150724       //--------------------------------------------------------------
001500150724
001600150724     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150724     h dftactgrp(*no)
001800150724
001900150724       //--------------------------------------------------------------
002000150724       //?Dichiarazione file.                                          ?
002100150724       //--------------------------------------------------------------
002200150724
002300150724       // -?Video?
002400150727     fFIDNI1D   cf   e             workstn
002500150724     f                                     indds(IndDspF)
002600150724     f                                     infds(InfDspF)
002700150724
002800150724       //--------------------------------------------------------------
002900150724       //?Definizione costanti.                                        ?
003000150724       //--------------------------------------------------------------
003100150724
003200150724       // -?Comandi di sistema?
003300150724     d c_CmdDspDBF     c                   const('DSPDBF +
003400150724     d                                            file(*libl/WFIAE00F) +
003500150724     d                                            mbr(*first) +
003600150724     d***                                         rcdslt(*yes) +
003700150724     d                                            output(*)')
003800150724
003900150724       // -?Tasti funzionali a video?
004000150724     d c_F01           c                   const(x'31')
004100150724     d c_F02           c                   const(x'32')
004200150724     d c_F03           c                   const(x'33')
004300150724     d c_F04           c                   const(x'34')
004400150724     d c_F05           c                   const(x'35')
004500150724     d c_F06           c                   const(x'36')
004600150724     d c_F07           c                   const(x'37')
004700150724     d c_F08           c                   const(x'38')
004800150724     d c_F09           c                   const(x'39')
004900150724     d c_F10           c                   const(x'3A')
005000150724     d c_F11           c                   const(x'3B')
005100150724     d c_F12           c                   const(x'3C')
005200150724     d c_F13           c                   const(x'B1')
005300150724     d c_F14           c                   const(x'B2')
005400150724     d c_F15           c                   const(x'B3')
005500150724     d c_F16           c                   const(x'B4')
005600150724     d c_F17           c                   const(x'B5')
005700150724     d c_F18           c                   const(x'B6')
005800150724     d c_F19           c                   const(x'B7')
005900150724     d c_F20           c                   const(x'B8')
006000150724     d c_F21           c                   const(x'B9')
006100150724     d c_F22           c                   const(x'BA')
006200150724     d c_F23           c                   const(x'BB')
006300150724     d c_F24           c                   const(x'BC')
006400150724     d c_Enter         c                   const(x'F1')
006500150724     d c_RollDown      c                   const(x'F4')
006600150724     d c_RollUp        c                   const(x'F5')
006700150724
006800150724       //--------------------------------------------------------------
006900150724       //?Definizione schiere.                                         ?
007000150724       //--------------------------------------------------------------
007100150724
007200150724       // -?Messaggi di errore?
007300150724     d sk_Msg          s             78    dim( 4)  ctdata  perrcd( 1)
007400150724
007500150724       //--------------------------------------------------------------
007600150724       //?Definizione aree dati.                                       ?
007700150724       //--------------------------------------------------------------
007800150724
007900150724       // -?Dati utente?
008000150724     d §AzUte        e ds                  extname(AZUTE00F)
008100150724     d                                     dtaara
008200150724     d §DatiUte      e ds                  extname(dDatiUte)
008300150724     d                                     dtaara
008400150724
008500150724       //--------------------------------------------------------------
008600150724       //?Definizione strutture dati.                                  ?
008700150724       //--------------------------------------------------------------
008800150724
008900150724       // -?InfDS?
009000150724     d InfDspF         ds
009100150724     d   dsp_aid             369    369a
009200150724
009300150724       // -?Indicatori su DspF?
009400150724     d IndDspF         ds                  inz
009500150724         // -?Emissione messaggio di errore?
009600150724     d   ErrMessage                    n   overlay(IndDspF : 28)
009700150724         // -?Posizionamento cursore & segnalazione errore?
009800150724     d   PosCurDSI                     n   overlay(IndDspF : 51)
009900150724     d   PosCurDSF                     n   overlay(IndDspF : 52)
010000150724         //   -?Riemissione videata?
010100150724     d   ErrGenerico                   n   overlay(IndDspF : 99)
010200150724
010300150724       // -?Status ds?
010400150724     d Status         sds
010500150724     d   SDSpgmName      *proc
010600150724
010700150724       // -?Parametri ricevuti?
010800150724     d KPJBA         e ds
010900150724
011000150724       //--------------------------------------------------------------
011100150724       //?Definizione variabili globali.                               ?
011200150724       //--------------------------------------------------------------
011300150724
011400150724       // -?Flags booleani?
011500150724     d $Fine           s               n   inz(*off)
011600150724     d $ErrAbilit      s               n   inz(*off)
011700150724     d $InzD01         s               n   inz(*on)
011800150724
011900150724       // -?Variabili per la gestione del video?
012000150724     d $Video          s              2    inz('D1')
012100150724
012200150724       //--------------------------------------------------------------
012300150724       //?Definizione prototipi procedure.                             ?
012400150724       //--------------------------------------------------------------
012500150724
012600150724       // -?Reperimento dati utente?
012700150724     d TIBS34ds      e ds                  inz
012800150724      /copy gaitrasrc/srcProtoPR,TIBS34R
012900150724
013000150724       // -?Controllo ed inversione date?
013100150724     d WLBdat          ds                  inz
013200150724     d   G08dat                       8  0 inz
013300150724     d   G08inv                       8  0 inz
013400150724     d   G08err                       1    inz('3')
013500150724     d   G08tgi                       5  0 inz
013600150724      /copy gaitrasrc/srcProtoPr,XSRDA8
013700150724
013800150724       // -?Richiamo diretto lavoro batch?
013900150727     d FIDNI2ds      e ds                  inz
014000150727     d fidnI2r         pr                  extpgm('FIDNI2R')
014100150724     d   kpjba                             likeds(KPJBA)
014200150724
014300150724       // -?Personalizzazione lavoro batch?
014400150724     d*// Bch09           pr                  extpgm('BCH09')
014500150724     d*//   kpjba                             likeds(KPJBA)
014600150724
014700150724       // -?Sottomissione lavoro batch?
014800150724      /copy gaitrasrc/srcProtoPR,BCH10
014900150724
015000150724      *// // -?Parametri API QCAPCMD (Process Commands)?
015100150724     d*// Qcmd            s           2048    inz  varying
015200150724      *///copy qSysInc/qRpgleSrc,QCAPCMD
015300150724      *// // -?API QCAPCMD (Process Commands)?
015400150724      *///copy gaitrasrc/srcProtoPR,QCAPCMD
015500150724      *//
015600150724      *// // -?Parametri gestione errori API.?
015700150724      *///copy qsysinc/qrpglesrc,QUSEC
015800150724
015900150724       //--------------------------------------------------------------
016000150724       //?Definizione key-list.                                        ?
016100150724       //--------------------------------------------------------------
016200150724
016300150724
016400150724       //--------------------------------------------------------------
016500150724       //?Riepilogo indicatori utilizzati.                             ?
016600150724       //--------------------------------------------------------------
016700150724
016800150724
016900150724       //--------------------------------------------------------------
017000150724       //?M A I N - L I N E                                            ?
017100150724       //--------------------------------------------------------------
017200150724
017300150724     c     *Entry        plist
017400150724     c                   parm                    KPJBA
017500150724
017600150724      /free
017700150724
017800150724       // -?Operazioni iniziali?
017900150724       exsr  sr_RoutInz;
018000150724
018100150724       // -?Ciclo di gestione del file video?
018200150724       DoW  $Fine = *off;
018300150724
018400150724         select;
018500150724
018600150724           // -?Richiesta parametri iniziali?
018700150724           when $Video = 'D1';
018800150724             exsr  sr_GesD01;
018900150724
019000150724           // -?? ? ??
019100150724           other;
019200150724             $Fine = *on;
019300150724
019400150724         endsl;
019500150724
019600150724       EndDo;
019700150724
019800150724       // -?Operazioni finali?
019900150724       exsr sr_RoutEnd;
020000150724
020100150724       //--------------------------------------------------------------
020200150724       //?Operazioni iniziali.                                         ?
020300150724       //--------------------------------------------------------------
020400150724       BEGSR  sr_RoutInz;
020500150724
020600150724         // -?Impostazione chiusura?
020700150724         *inLR = *on;
020800150724
020900150724         // -?Reperimento dati job?
021000150724         exsr  sr_DatiJob;
021100150724
021200150724         // -?Impostazione nome programma a video?
021300150724         V1Tpgm = SDSpgmName;
021400150724
021500150724         // -?Pulizia parametri per Batch?
021600150727         clear  FIDNI2ds;
021700150724
021800150724       ENDSR;
021900150724
022000150724       //--------------------------------------------------------------
022100150724       //?Reperimento Dati del job (Utente/Operativi).                 ?
022200150724       //--------------------------------------------------------------
022300150724       BEGSR  sr_DatiJob;
022400150724
022500150724         in(e) §AzUte;
022600150724         if NOT %error;
022700150724           in(e) §DatiUte;
022800150724         endif;
022900150724         if %error or RSut = *blank;
023000150724           tibs34r ( tibs34ds );
023100150724           in §AzUte;
023200150724           in §DatiUte;
023300150724         endif;
023400150724
023500150724       ENDSR;
023600150724
023700150724       //--------------------------------------------------------------
023800150724       //?Gestione videata D01.                                        ?
023900150724       //--------------------------------------------------------------
024000150724       BEGSR  sr_GesD01;
024100150724
024200150724         // -?Inizializzazione videata?
024300150724         if  $InzD01 = *on;
024400150724           exsr  sr_InzD01;
024500150724           $InzD01 = *off;
024600150724         endif;
024700150724
024800150724         // -?Emissione videata?
024900150727         write  DNI1T01;
025000150727         exfmt  DNI1D01;
025100150724
025200150724         clear  V1Dmsg;
025300150724         reset  ErrMessage;
025400150724         reset  ErrGenerico;
025500150724
025600150724         SELECT;
025700150724
025800150724           // -?F3=Fine?
025900150724           WHEN  dsp_aid = c_F03;
026000150724             $Fine = *on;
026100150724
026200150724           //// -?F9=Visualizzazione file WFIAE00F?
026300150724           //WHEN  dsp_aid = c_F09;
026400150724           //  Qcmd = 'DSPDBF file(*libl/WFIAE00F) mbr(*first) output(*)';
026500150724           //  exsr  sr_ExecCmd;
026600150724
026700150724           //// -?F9=Personalizzazione Batch?
026800150724           //WHEN  dsp_aid = c_F09;
026900150727           //  Kcoaz = 'DNI2';
027000150724           //  Bch09 ( kpjba );
027100150724
027200150724           // -?Invio / F6=Conferma?
027300150724           OTHER;
027400150724             exsr  sr_CtrD01;
027500150724             if  ErrGenerico = *on;
027600150724               leavesr;
027700150724             endif;
027800150724             if  dsp_aid = c_F06;
027900150724               exsr  sr_SbmJob;
028000150724               $Fine = *on;
028100150724             endif;
028200150724
028300150724         ENDSL;
028400150724
028500150724       ENDSR;
028600150724
028700150724       //--------------------------------------------------------------
028800150724       //?Inizializzazione videata D01.                                ?
028900150724       //--------------------------------------------------------------
029000150724       BEGSR  sr_InzD01;
029100150724
029200150727         clear  DNI1D01;
029300150724
029400150724         //// -?Impostazione Output di default?
029500150724         //if  DUTlpo = 'S';
029600150724         //  V1Cout = 'F';
029700150724         //else;
029800150724         //  V1Cout = 'S';
029900150724         //endif;
030000150724
030100150724         //// -?(Dis)Abilitazione tasti funzionali?
030200150724         //F9Attivo = (DUTlpo = 'S');
030300150724
030400150724       ENDSR;
030500150724
030600150724       //--------------------------------------------------------------
030700150724       //?Controllo dati nella videata D01.                            ?
030800150724       //--------------------------------------------------------------
030900150724       BEGSR  sr_CtrD01;
031000150724
031100150724         // -?Spegnimento indicatori di posizionamento cursore?
031200150724         %subst( IndDspF : 50 ) = *off;
031300150724
031400150724         // -?Date spedizioni?
031500150724         if  V1Cdsi = *zero  and  V1Cdsf = *zero;
031600150724           PosCurDSI   = *on;
031700150724           ErrMessage  = *on;
031800150724           ErrGenerico = *on;
031900150724           V1Dmsg = sk_Msg(01);
032000150724           leavesr;
032100150724         endif;
032200150724
032300150724         // -?Data spedizione iniziale?
032400150724         clear  WLBdat;
032500150724         if  V1Cdsi = *zero;
032600150724           PosCurDSI   = *on;
032700150724           ErrMessage  = *on;
032800150724           ErrGenerico = *on;
032900150724           V1Dmsg = sk_Msg(02);
033000150724           leavesr;
033100150724         endif;
033200150724         G08dat = V1Cdsi;
033300150724         xsrda8 ( WLBdat );
033400150724         if  G08err =  *on;
033500150724           PosCurDSI   = *on;
033600150724           ErrMessage  = *on;
033700150724           ErrGenerico = *on;
033800150724           V1Dmsg = sk_Msg(03);
033900150724           leavesr;
034000150724         endif;
034100150724         V1Cdsi  = G08dat;
034200150727         DNI2dsi = G08inv;
034300150724
034400150724         // -?Data spedizione finale?
034500150724         clear  WLBdat;
034600150724         if  V1Cdsf = *zero;
034700150724           PosCurDSF   = *on;
034800150724           ErrMessage  = *on;
034900150724           ErrGenerico = *on;
035000150724           V1Dmsg = sk_Msg(02);
035100150724           leavesr;
035200150724         endif;
035300150724         G08dat = V1Cdsf;
035400150724         xsrda8 ( WLBdat );
035500150724         if  G08err =  *on;
035600150724           PosCurDSF   = *on;
035700150724           ErrMessage  = *on;
035800150724           ErrGenerico = *on;
035900150724           V1Dmsg = sk_Msg(03);
036000150724           leavesr;
036100150724         endif;
036200150724         V1Cdsf  = G08dat;
036300150727         DNI2dsf = G08inv;
036400150724
036500150724         // -?Range date spedizioni?
036600150727         if  DNI2dsi > DNI2dsf;
036700150724           PosCurDSI   = *on;
036800150724           ErrMessage  = *on;
036900150724           ErrGenerico = *on;
037000150724           V1Dmsg = sk_Msg(04);
037100150724           leavesr;
037200150724         endif;
037300150724
037400150724       ENDSR;
037500150724
037600150724       //--------------------------------------------------------------
037700150724       //?Sottomissione lavoro batch                                   ?
037800150724       //--------------------------------------------------------------
037900150724       BEGSR  sr_SbmJob;
038000150724
038100150724         // -?Impostazione dei parametri restanti?
038200150727         //DNI2out = V1Cout;
038300150724
038400150724         // -?Lancio?
038500150727         kpjbu = FIDNI2ds;
038600150727         kcoaz = 'DNI2';
038700150724         if  KNMUS = *all'1';
038800150727           fidnI2r ( kpjba );
038900150724         else;
039000150724           bch10 ( kpjba );
039100150724         endif;
039200150724
039300150724       ENDSR;
039400150724
039500150724       //--------------------------------------------------------------
039600150724       //?Esecuzione del comando (già impostato).                      ?
039700150724       //--------------------------------------------------------------
039800150724       //BEGSR  sr_ExecCmd;
039900150724       //
040000150724       //  clear Qcap0100;
040100150724       //  Qcabcsdh = *off;
040200150724       //  Qcapa    = *off;
040300150724       //  Qcacmdss = *off;
040400150724       //  Qcaerved = *allX'00';
040500150724       //
040600150724       //  clear Qusec;
040700150724       //  Qusbprv  = %size(Qusec);
040800150724       //
040900150724       //  ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
041000150724       //                    %size(Qcap0100) : 'CPOP0100' : *omit :
041100150724       //                    0 : 0 : Qusec);
041200150724       //
041300150724       //  //if  Qusei <> *blank;
041400150724       //  //  ...;
041500150724       //  //endif;
041600150724       //
041700150724       //ENDSR;
041800150724
041900150724       //--------------------------------------------------------------
042000150724       //?Operazioni finali.                                           ?
042100150724       //--------------------------------------------------------------
042200150724       BEGSR  sr_RoutEnd;
042300150724
042400150724         return;
042500150724
042600150724       ENDSR;
042700150724
042800150724      /end-free
042900150724
043000150724       //--------------------------------------------------------------
043100150724       //?Schiere a tempo di compilazione.
043200150724       //--------------------------------------------------------------
043300150724
043400150727** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
043500150724Impostare il periodo da elaborare                                              1
043600150724Data obbligatoria                                                              2
043700150724Data formalmente errata                                                        3
043800150724Data iniziale incongruente con data finale                                     4
