000100170830       //==============================================================
000200171207       //? TNVRSPI2R - Aggiornamento ANSPI00F con dati da "finanze.it". ?
000300170830       //==============================================================
000400170830
000500170830       //--------------------------------------------------------------
000600171207       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170830       //--------------------------------------------------------------
000800170830
000900170830     /*PRM dbgview(*source)
001000170830     /*END
001100170830
001200170830       //--------------------------------------------------------------
001300171207       //? Specifiche di controllo.                                     ?
001400170830       //--------------------------------------------------------------
001500170830
001600170830     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170830     h dftactgrp(*no)
001800170830
001900170830       //--------------------------------------------------------------
002000171207       //? Dichiarazione file.                                          ?
002100170830       //--------------------------------------------------------------
002200170830
002300171207       // - ?Video?
002400170830     fTNVRSPI2D cf   e             workstn
002500170830     f                                     indds(IndDspF)
002600170830     f                                     infds(InfDspF)
002700170830
002800170830       //--------------------------------------------------------------
002900171207       //? Definizione costanti.                                        ?
003000170830       //--------------------------------------------------------------
003100170830
003200171207       // - ?Tasti funzionali a video?
003300170830     d c_F01           c                   const(x'31')
003400170830     d c_F02           c                   const(x'32')
003500170830     d c_F03           c                   const(x'33')
003600170830     d c_F04           c                   const(x'34')
003700170830     d c_F05           c                   const(x'35')
003800170830     d c_F06           c                   const(x'36')
003900170830     d c_F07           c                   const(x'37')
004000170830     d c_F08           c                   const(x'38')
004100170830     d c_F09           c                   const(x'39')
004200170830     d c_F10           c                   const(x'3A')
004300170830     d c_F11           c                   const(x'3B')
004400170830     d c_F12           c                   const(x'3C')
004500170830     d c_F13           c                   const(x'B1')
004600170830     d c_F14           c                   const(x'B2')
004700170830     d c_F15           c                   const(x'B3')
004800170830     d c_F16           c                   const(x'B4')
004900170830     d c_F17           c                   const(x'B5')
005000170830     d c_F18           c                   const(x'B6')
005100170830     d c_F19           c                   const(x'B7')
005200170830     d c_F20           c                   const(x'B8')
005300170830     d c_F21           c                   const(x'B9')
005400170830     d c_F22           c                   const(x'BA')
005500170830     d c_F23           c                   const(x'BB')
005600170830     d c_F24           c                   const(x'BC')
005700170830     d c_Enter         c                   const(x'F1')
005800170830     d c_RollDown      c                   const(x'F4')
005900170830     d c_RollUp        c                   const(x'F5')
006000170830
006100170830       //--------------------------------------------------------------
006200171207       //? Definizione schiere.                                         ?
006300170830       //--------------------------------------------------------------
006400170830
006500171207       // - ?Messaggi di errore?
006600171207     d sk_Msg          s             78    dim( 5)  ctdata  perrcd( 1)
006700170830
006800170830       //--------------------------------------------------------------
006900171207       //? Definizione aree dati.                                       ?
007000170830       //--------------------------------------------------------------
007100170830
007200171207       // - ?Dati utente?
007300170830     d §AzUte        e ds                  extname(AZUTE00F)
007400170830     d                                     dtaara
007500170830     d §DatiUte      e ds                  extname(dDatiUte)
007600170830     d                                     dtaara
007700170830
007800170830       //--------------------------------------------------------------
007900171207       //? Definizione strutture dati.                                  ?
008000170830       //--------------------------------------------------------------
008100170830
008200171207       // - ?InfDS?
008300170830     d InfDspF         ds
008400170830     d   dsp_aid             369    369a
008500170830
008600171207       // - ?Indicatori su DspF?
008700170830     d IndDspF         ds                  inz
008800171207         // - ?Emissione messaggio di errore?
008900170830     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009000171207         // - ?Posizionamento cursore & segnalazione errore?
009100171207     d   $PosCurACO                    n   overlay( IndDspF : 51 )
009200171207     d   $PosCurAOI                    n   overlay( IndDspF : 52 )
009300171207     d   $PosCurDDE                    n   overlay( IndDspF : 53 )
009400171207     d   $PosCurDSC                    n   overlay( IndDspF : 54 )
009500171207     d   $PosCurDSSA                   n   overlay( IndDspF : 55 )
009600171207         // - ?Riemissione videata?
009700170830     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
009800170830
009900171207       // - ?Status ds?
010000170830     d Status         sds
010100170830     d   SDSpgmName      *proc
010200170830
010300171207       // - ?Parametri ricevuti?
010400170830     d KPJBA         e ds
010500170830
010600170830       //--------------------------------------------------------------
010700171207       //? Definizione variabili globali.                               ?
010800170830       //--------------------------------------------------------------
010900170830
011000171207       // - ?Flags booleani?
011100170830     d $Fine           s               n   inz(*off)
011200170830     d $InzD01         s               n   inz(*on)
011300171207     d $InzD02         s               n   inz(*on)
011400170830
011500171207       // - ?Variabili per la gestione del video?
011600170830     d $Video          s              2    inz('D1')
011700170830
011800171207       // - ?Variabili di comodo?
011900170830     d wDate_Eur       s               d   datfmt(*EUR)
012000170830     d                                     inz
012100170830
012200170830       //--------------------------------------------------------------
012300171207       //? Definizione prototipi procedure.                             ?
012400170830       //--------------------------------------------------------------
012500170830
012600171207       // - ?Reperimento dati utente?
012700170830     d TIBS34ds      e ds                  inz
012800170830      /copy gaitrasrc/srcProtoPR,TIBS34R
012900170830
013000171207       // - ?Controllo ed inversione date?
013100170830     d WLBdat          ds                  inz
013200170830     d   G08dat                       8  0 inz
013300170830     d   G08inv                       8  0 inz
013400170830     d   G08err                       1    inz('3')
013500170830     d   G08tgi                       5  0 inz
013600170830      /copy gaitrasrc/srcProtoPr,XSRDA8
013700170830
013800171207       // - ?Richiamo diretto lavoro batch?
013900170830     d TNVRSPI1ds    e ds                  inz   qualified
014000170830     d tnvrSPI1r       pr                  extpgm('TNVRSPI1R')
014100170830     d   kpjba                             likeds(KPJBA)
014200170830
014300171207       // - ?Personalizzazione lavoro batch?
014400170830     d*// Bch09           pr                  extpgm('BCH09')
014500170830     d*//   kpjba                             likeds(KPJBA)
014600170830
014700171207       // - ?Sottomissione lavoro batch?
014800170830      /copy gaitrasrc/srcProtoPR,BCH10
014900170830
015000171207       // - ?Parametri API QCAPCMD (Process Commands)?
015100170830     d Qcmd            s           2048    inz  varying
015200170830      /copy qSysInc/qRpgleSrc,QCAPCMD
015300171207       // - ?API QCAPCMD (Process Commands)?
015400170830      /copy gaitrasrc/srcProtoPR,QCAPCMD
015500170830
015600171207       // - ?Parametri gestione errori API.?
015700170830      /copy qsysinc/qrpglesrc,QUSEC
015800170830
015900170830       //--------------------------------------------------------------
016000171207       //? Definizione key-list.                                        ?
016100170830       //--------------------------------------------------------------
016200170830
016300170830
016400170830       //--------------------------------------------------------------
016500171207       //? M A I N - L I N E                                            ?
016600170830       //--------------------------------------------------------------
016700170830
016800170830     c     *Entry        plist
016900170830     c                   parm                    KPJBA
017000170830
017100170830      /free
017200170830
017300171207       // - ?Operazioni iniziali?
017400170830       exsr  sr_RoutInz;
017500170830
017600171207       // - ?Ciclo di gestione del file video?
017700170830       DoW  $Fine = *off;
017800170830
017900170830         select;
018000170830
018100171207           // - ?Richiesta parametri iniziali?
018200170830           when $Video = 'D1';
018300170830             exsr  sr_GesD01;
018400171207
018500171207           // - ?Richiesta Data Scadenza per Inserimento di massa?
018600171207           when $Video = 'D2';
018700171207             exsr  sr_GesD02;
018800170830
018900171207           // - ?? ? ??
019000170830           other;
019100170830             $Fine = *on;
019200170830
019300170830         endsl;
019400170830
019500170830       EndDo;
019600170830
019700171207       // - ?Operazioni finali?
019800170830       exsr sr_RoutEnd;
019900170830
020000170830       //--------------------------------------------------------------
020100171207       //? Operazioni iniziali.                                         ?
020200170830       //--------------------------------------------------------------
020300170830       BEGSR  sr_RoutInz;
020400170830
020500171207         // - ?Impostazione chiusura?
020600170830         *inLR = *on;
020700170830
020800171207         // - ?Reperimento dati job?
020900170830         exsr  sr_DatiJob;
021000170830
021100171207         // - ?Impostazione nome programma a video?
021200170830         VT1pgm = SDSpgmName;
021300170830
021400171207         // - ?Pulizia parametri per Batch?
021500170830         clear  tnvrSPI1ds;
021600170830
021700170830       ENDSR;
021800170830
021900170830       //--------------------------------------------------------------
022000171207       //? Reperimento Dati del job (Utente/Operativi).                 ?
022100170830       //--------------------------------------------------------------
022200170830       BEGSR  sr_DatiJob;
022300170830
022400170830         in(e) §AzUte;
022500170830         if NOT %error;
022600170830           in(e) §DatiUte;
022700170830         endif;
022800170830         if %error or RSut = *blank;
022900170830           tibs34r ( tibs34ds );
023000170830           in §AzUte;
023100170830           in §DatiUte;
023200170830         endif;
023300170830
023400170830       ENDSR;
023500170830
023600170830       //--------------------------------------------------------------
023700171207       //? Gestione videata D01.                                        ?
023800170830       //--------------------------------------------------------------
023900170830       BEGSR  sr_GesD01;
024000170830
024100171207         // - ?Inizializzazione videata?
024200170830         if  $InzD01 = *on;
024300170830           exsr  sr_InzD01;
024400170830           $InzD01 = *off;
024500170830         endif;
024600170830
024700171207         // - ?Emissione videata?
024800170830         write  VRSP2T1;
024900171207         write  VRSP2P1;
025000170830         exfmt  VRSP2D1;
025100170830
025200170830         clear  VIDmsg;
025300170830         reset  $ErrMessage;
025400170830         reset  $ErrGenerico;
025500170830
025600170830         Select;
025700170830
025800171207           // - ?Errore: sistema informativo errato?
025900170830           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
026000170830             $Fine  = *on;
026100170830
026200171207           // - ?F3=Fine?
026300170830           When  dsp_aid = c_F03;
026400170830             $Fine = *on;
026500170830
026600171207           // - ?F9=Visualizzazione WrkF WSPLIT0F?
026700170830           When  dsp_aid = c_F09;
026800170830             Qcmd = 'DSPDBF file(*libl/WSPLIT0F) mbr(*first) output(*)';
026900170830             exsr  sr_ExecCmd;
027000170830
027100171207           // - ?Invio / F6=Conferma?
027200170830           Other;
027300170830             exsr  sr_CtrD01;
027400171207             select;
027500171207               // - ?Controllo videata?
027600171207               when  $ErrGenerico = *on;
027700171207                 leavesr;
027800171207               // - ?Richiesto Inserimento dfi massa?
027900171207               when  V1Caoi = 'I';
028000171207                 $Video  = 'D2';
028100171207                 $InzD02 = *on;
028200171207               // - ?Conferma Aggiornamento?
028300171207               when  dsp_aid = c_F06;
028400171207                 exsr  sr_SbmJob;
028500171207                 $Fine = *on;
028600171207             endsl;
028700170830
028800170830         ENDSL;
028900170830
029000170830       ENDSR;
029100170830
029200170830       //--------------------------------------------------------------
029300171207       //? Inizializzazione videata D01.                                ?
029400170830       //--------------------------------------------------------------
029500170830       BEGSR  sr_InzD01;
029600170830
029700170830         clear  VRSP2D1;
029800170830
029900171207         // - ?Controllo del sistema informativo?
030000170830         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
030100170830           VIDmsg = sk_Msg(01);
030200170830           $ErrMessage  = *on;
030300170830           $ErrGenerico = *on;
030400170830           leavesr;
030500170830         endif;
030600170830
030700171207         // - ?Impostazione Data Decorrenza nuovi soggetti?
030800171207         //   ?(oggi)?
030900170830         wDate_Eur = %date();
031000170830         V1Cdde = %dec( wDate_Eur );
031100170830
031200171207         // - ?Impostazione Data Scadenza soggetti NON più presenti?
031300171207         //   ?(ieri)?
031400170830         wDate_Eur = %date() - %days(1);
031500170830         V1Cdsc = %dec( wDate_Eur );
031600170830
031700170830       ENDSR;
031800170830
031900170830       //--------------------------------------------------------------
032000171207       //? Controllo dati nella videata D01.                            ?
032100170830       //--------------------------------------------------------------
032200170830       BEGSR  sr_CtrD01;
032300170830
032400171207         // - ?Spegnimento indicatori di posizionamento cursore?
032500170830         %subst( IndDspF : 50 ) = *off;
032600171207
032700171207         // - ?Controllo Anno Competenza?
032800171207         if  V1Caco <= *zero;
032900171207           $PosCurACO   = *on;
033000171207           $ErrMessage  = *on;
033100171207           $ErrGenerico = *on;
033200171207           VIDmsg = sk_Msg(04);
033300171207           leavesr;
033400171207         endif;
033500171207         TNVRSPI1ds.VRSP1aco = V1Caco;
033600171207
033700171207         // - ?Controllo flag Aggiornamento/Inserimento?
033800171207         if  V1Caoi <> 'A'  and  V1Caoi <> 'I';
033900171207           $PosCurAOI   = *on;
034000171207           $ErrMessage  = *on;
034100171207           $ErrGenerico = *on;
034200171207           VIDmsg = sk_Msg(05);
034300171207           leavesr;
034400171207         endif;
034500171207         TNVRSPI1ds.VRSP1aoi = V1Caoi;
034600170830
034700171207         // - ?Controllo Data Decorrenza nuovi soggetti?
034800170830         clear  WLBdat;
034900170830         if  V1Cdde = *zero;
035000170830           $PosCurDDE   = *on;
035100170830           $ErrMessage  = *on;
035200170830           $ErrGenerico = *on;
035300170830           VIDmsg = sk_Msg(02);
035400170830           leavesr;
035500170830         endif;
035600170830         G08dat = V1Cdde;
035700170830         xsrda8 ( WLBdat );
035800170830         if  G08err =  *on;
035900170830           $PosCurDDE   = *on;
036000170830           $ErrMessage  = *on;
036100170830           $ErrGenerico = *on;
036200170830           VIDmsg = sk_Msg(03);
036300170830           leavesr;
036400170830         endif;
036500170830         V1Cdde  = G08dat;
036600170830         TNVRSPI1ds.VRSP1dde = G08inv;
036700170830
036800171207         // - ?Controllo Data Scadenza soggetti NON più presenti?
036900170830         clear  WLBdat;
037000170830         if  V1Cdsc = *zero;
037100170830           $PosCurDSC   = *on;
037200170830           $ErrMessage  = *on;
037300170830           $ErrGenerico = *on;
037400170830           VIDmsg = sk_Msg(02);
037500170830           leavesr;
037600170830         endif;
037700170830         G08dat = V1Cdsc;
037800170830         xsrda8 ( WLBdat );
037900170830         if  G08err =  *on;
038000170830           $PosCurDSC   = *on;
038100170830           $ErrMessage  = *on;
038200170830           $ErrGenerico = *on;
038300170830           VIDmsg = sk_Msg(03);
038400170830           leavesr;
038500170830         endif;
038600170830         V1Cdsc  = G08dat;
038700170830         TNVRSPI1ds.VRSP1dsc = G08inv;
038800170830
038900170830       ENDSR;
039000171207
039100171207       //--------------------------------------------------------------
039200171207       //? Gestione videata D02.                                        ?
039300171207       //--------------------------------------------------------------
039400171207       BEGSR  sr_GesD02;
039500171207
039600171207         // - ?Inizializzazione videata?
039700171207         if  $InzD02 = *on;
039800171207           exsr  sr_InzD02;
039900171207           $InzD02 = *off;
040000171207         endif;
040100171207
040200171207         // - ?Emissione videata?
040300171212         $ErrGenerico = $ErrMessage;
040400171212         clear  $ErrMessage;
040500171207         write  VRSP2T1;
040600171207         write  VRSP2D1;
040700171218         write  Protect;
040800171207         write  VRSP2P2;
040900171212         $ErrMessage  = $ErrGenerico;
041000171207         exfmt  VRSP2D2;
041100171207
041200171207         clear  VIDmsg;
041300171207         reset  $ErrMessage;
041400171207         reset  $ErrGenerico;
041500171207
041600171207         Select;
041700171207
041800171207           // - ?Errore: sistema informativo errato?
041900171207           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
042000171207             $Fine  = *on;
042100171207
042200171207           // - ?F3=Fine?
042300171207           When  dsp_aid = c_F03;
042400171207             $Fine = *on;
042500171207
042600171207           // - ?F9=Visualizzazione WrkF WSPLIT0F?
042700171207           When  dsp_aid = c_F09;
042800171207             Qcmd = 'DSPDBF file(*libl/WSPLIT0F) mbr(*first) output(*)';
042900171207             exsr  sr_ExecCmd;
043000171207
043100171207           // - ?F12=Ritorno?
043200171207           When  dsp_aid = c_F12;
043300171207             $Video = 'D1';
043400171207
043500171207           // - ?Invio / F6=Conferma?
043600171207           Other;
043700171207             exsr  sr_CtrD02;
043800171207             select;
043900171207               // - ?Controllo videata?
044000171207               when  $ErrGenerico = *on;
044100171207                 leavesr;
044200171207               // - ?Conferma Aggiornamento?
044300171207               when  dsp_aid = c_F06;
044400171207                 exsr  sr_SbmJob;
044500171207                 $Fine = *on;
044600171207             endsl;
044700171207
044800171207         ENDSL;
044900171207
045000171207       ENDSR;
045100171207
045200171207       //--------------------------------------------------------------
045300171207       //? Inizializzazione videata D02.                                ?
045400171207       //--------------------------------------------------------------
045500171207       BEGSR  sr_InzD02;
045600171207
045700171207         clear  VRSP2D2;
045800171207
045900171207         // - ?Controllo del sistema informativo?
046000171207         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
046100171207           VIDmsg = sk_Msg(01);
046200171207           $ErrMessage  = *on;
046300171207           $ErrGenerico = *on;
046400171207           leavesr;
046500171207         endif;
046600171218
046700171218         // - ?Impostazione Data Scadenza soggetti attivi nell'anno di?
046800171218         //   ?competenza?
046900171218         V1Cdssa = V1Cdsc;
047000171207
047100171207       ENDSR;
047200171207
047300171207       //--------------------------------------------------------------
047400171207       //? Controllo dati nella videata D02.                            ?
047500171207       //--------------------------------------------------------------
047600171207       BEGSR  sr_CtrD02;
047700171207
047800171207         // - ?Spegnimento indicatori di posizionamento cursore?
047900171207         %subst( IndDspF : 50 ) = *off;
048000171207
048100171207         // - ?Controllo Data Scadenza soggetti NON più presenti?
048200171207         clear  WLBdat;
048300171207         if  V1Cdssa = *zero;
048400171207           $PosCurDSSA  = *on;
048500171207           $ErrMessage  = *on;
048600171207           $ErrGenerico = *on;
048700171207           VIDmsg = sk_Msg(02);
048800171207           leavesr;
048900171207         endif;
049000171207         G08dat = V1Cdssa;
049100171207         xsrda8 ( WLBdat );
049200171207         if  G08err =  *on;
049300171207           $PosCurDSSA  = *on;
049400171207           $ErrMessage  = *on;
049500171207           $ErrGenerico = *on;
049600171207           VIDmsg = sk_Msg(03);
049700171207           leavesr;
049800171207         endif;
049900171207         V1Cdssa = G08dat;
050000171207         TNVRSPI1ds.VRSP1dssa = G08inv;
050100171207
050200171207       ENDSR;
050300170830
050400170830       //--------------------------------------------------------------
050500171207       //? Sottomissione lavoro batch                                   ?
050600170830       //--------------------------------------------------------------
050700170830       BEGSR  sr_SbmJob;
050800170830
050900171207         // - ?Lancio?
051000170830         kpjbu = TNVRSPI1ds;
051100170830         kcoaz = 'VRSP';
051200170830         if  KNMUS = *all'1';
051300170830           tnvrSPI1r ( kpjba );
051400170830         else;
051500170830           bch10 ( kpjba );
051600170830         endif;
051700170830
051800170830       ENDSR;
051900170830
052000170830       //--------------------------------------------------------------
052100171207       //? Esecuzione del comando (già impostato).                      ?
052200170830       //--------------------------------------------------------------
052300170830       BEGSR  sr_ExecCmd;
052400170830
052500170830         clear Qcap0100;
052600170830         Qcabcsdh = *off;
052700170830         Qcapa    = *off;
052800170830         Qcacmdss = *off;
052900170830         Qcaerved = *allX'00';
053000170830
053100170830         clear Qusec;
053200170830         Qusbprv  = %size(Qusec);
053300170830
053400170830         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
053500170830                           %size(Qcap0100) : 'CPOP0100' : *omit :
053600170830                           0 : 0 : Qusec);
053700170830
053800170830         //if  Qusei <> *blank;
053900170830         //  ...;
054000170830         //endif;
054100170830
054200170830       ENDSR;
054300170830
054400170830       //--------------------------------------------------------------
054500171207       //? Operazioni finali.                                           ?
054600170830       //--------------------------------------------------------------
054700170830       BEGSR  sr_RoutEnd;
054800170830
054900171207         // - ?Chiusura pgm?
055000170830         return;
055100170830
055200170830       ENDSR;
055300170830
055400170830      /end-free
055500170830
055600170830       //--------------------------------------------------------------
055700171207       //? Schiere a tempo di compilazione.
055800170830       //--------------------------------------------------------------
055900170830
056000171207** - ?sk_Msg: ?Messaggi di Errore?-------------------------------------------*
056100170830Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire       1
056200170830Data obbligatoria                                                              2
056300170830Data formalmente errata                                                        3
056400171207Anno di Competenza errato o mancante                                           4
056500171207Flag di Tipo Aggiornamento o Inserimento errato o mancante                     5
