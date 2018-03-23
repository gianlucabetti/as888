000100170830       //==============================================================
000200170830       //?TNVRSPI2R - Aggiornamento ANSPI00F con dati da "finanze.it". ?
000300170830       //==============================================================
000400170830
000500170830       //--------------------------------------------------------------
000600170830       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170830       //--------------------------------------------------------------
000800170830
000900170830     /*PRM dbgview(*source)
001000170830     /*END
001100170830
001200170830       //--------------------------------------------------------------
001300170830       //?Specifiche di controllo.                                     ?
001400170830       //--------------------------------------------------------------
001500170830
001600170830     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170830     h dftactgrp(*no)
001800170830
001900170830       //--------------------------------------------------------------
002000170830       //?Dichiarazione file.                                          ?
002100170830       //--------------------------------------------------------------
002200170830
002300170830       // -?Video?
002400170830     fTNVRSPI2D cf   e             workstn
002500170830     f                                     indds(IndDspF)
002600170830     f                                     infds(InfDspF)
002700170830
002800170830       //--------------------------------------------------------------
002900170830       //?Definizione costanti.                                        ?
003000170830       //--------------------------------------------------------------
003100170830
003200170830       // -?Tasti funzionali a video?
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
006200170830       //?Definizione schiere.                                         ?
006300170830       //--------------------------------------------------------------
006400170830
006500170830       // -?Messaggi di errore?
006600170830     d sk_Msg          s             78    dim( 3)  ctdata  perrcd( 1)
006700170830
006800170830       //--------------------------------------------------------------
006900170830       //?Definizione aree dati.                                       ?
007000170830       //--------------------------------------------------------------
007100170830
007200170830       // -?Dati utente?
007300170830     d §AzUte        e ds                  extname(AZUTE00F)
007400170830     d                                     dtaara
007500170830     d §DatiUte      e ds                  extname(dDatiUte)
007600170830     d                                     dtaara
007700170830
007800170830       //--------------------------------------------------------------
007900170830       //?Definizione strutture dati.                                  ?
008000170830       //--------------------------------------------------------------
008100170830
008200170830       // -?InfDS?
008300170830     d InfDspF         ds
008400170830     d   dsp_aid             369    369a
008500170830
008600170830       // -?Indicatori su DspF?
008700170830     d IndDspF         ds                  inz
008800170830         // -?Emissione messaggio di errore?
008900170830     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009000170830         // -?Posizionamento cursore & segnalazione errore?
009100170830     d   $PosCurDDE                    n   overlay( IndDspF : 51 )
009200170830     d   $PosCurDSC                    n   overlay( IndDspF : 52 )
009300170830         //   -?Riemissione videata?
009400170830     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
009500170830
009600170830       // -?Status ds?
009700170830     d Status         sds
009800170830     d   SDSpgmName      *proc
009900170830
010000170830       // -?Parametri ricevuti?
010100170830     d KPJBA         e ds
010200170830
010300170830       //--------------------------------------------------------------
010400170830       //?Definizione variabili globali.                               ?
010500170830       //--------------------------------------------------------------
010600170830
010700170830       // -?Flags booleani?
010800170830     d $Fine           s               n   inz(*off)
010900170830     d $InzD01         s               n   inz(*on)
011000170830
011100170830       // -?Variabili per la gestione del video?
011200170830     d $Video          s              2    inz('D1')
011300170830
011400170830       // -?Variabili di comodo?
011500170830     d wDate_Eur       s               d   datfmt(*EUR)
011600170830     d                                     inz
011700170830
011800170830       //--------------------------------------------------------------
011900170830       //?Definizione prototipi procedure.                             ?
012000170830       //--------------------------------------------------------------
012100170830
012200170830       // -?Reperimento dati utente?
012300170830     d TIBS34ds      e ds                  inz
012400170830      /copy gaitrasrc/srcProtoPR,TIBS34R
012500170830
012600170830       // -?Controllo ed inversione date?
012700170830     d WLBdat          ds                  inz
012800170830     d   G08dat                       8  0 inz
012900170830     d   G08inv                       8  0 inz
013000170830     d   G08err                       1    inz('3')
013100170830     d   G08tgi                       5  0 inz
013200170830      /copy gaitrasrc/srcProtoPr,XSRDA8
013300170830
013400170830       // -?Richiamo diretto lavoro batch?
013500170830     d TNVRSPI1ds    e ds                  inz   qualified
013600170830     d tnvrSPI1r       pr                  extpgm('TNVRSPI1R')
013700170830     d   kpjba                             likeds(KPJBA)
013800170830
013900170830       // -?Personalizzazione lavoro batch?
014000170830     d*// Bch09           pr                  extpgm('BCH09')
014100170830     d*//   kpjba                             likeds(KPJBA)
014200170830
014300170830       // -?Sottomissione lavoro batch?
014400170830      /copy gaitrasrc/srcProtoPR,BCH10
014500170830
014600170830       // -?Parametri API QCAPCMD (Process Commands)?
014700170830     d Qcmd            s           2048    inz  varying
014800170830      /copy qSysInc/qRpgleSrc,QCAPCMD
014900170830       // -?API QCAPCMD (Process Commands)?
015000170830      /copy gaitrasrc/srcProtoPR,QCAPCMD
015100170830
015200170830       // -?Parametri gestione errori API.?
015300170830      /copy qsysinc/qrpglesrc,QUSEC
015400170830
015500170830       //--------------------------------------------------------------
015600170830       //?Definizione key-list.                                        ?
015700170830       //--------------------------------------------------------------
015800170830
015900170830
016000170830       //--------------------------------------------------------------
016100170830       //?M A I N - L I N E                                            ?
016200170830       //--------------------------------------------------------------
016300170830
016400170830     c     *Entry        plist
016500170830     c                   parm                    KPJBA
016600170830
016700170830      /free
016800170830
016900170830       // -?Operazioni iniziali?
017000170830       exsr  sr_RoutInz;
017100170830
017200170830       // -?Ciclo di gestione del file video?
017300170830       DoW  $Fine = *off;
017400170830
017500170830         select;
017600170830
017700170830           // -?Richiesta parametri iniziali?
017800170830           when $Video = 'D1';
017900170830             exsr  sr_GesD01;
018000170830
018100170830           // -?? ? ??
018200170830           other;
018300170830             $Fine = *on;
018400170830
018500170830         endsl;
018600170830
018700170830       EndDo;
018800170830
018900170830       // -?Operazioni finali?
019000170830       exsr sr_RoutEnd;
019100170830
019200170830       //--------------------------------------------------------------
019300170830       //?Operazioni iniziali.                                         ?
019400170830       //--------------------------------------------------------------
019500170830       BEGSR  sr_RoutInz;
019600170830
019700170830         // -?Impostazione chiusura?
019800170830         *inLR = *on;
019900170830
020000170830         // -?Reperimento dati job?
020100170830         exsr  sr_DatiJob;
020200170830
020300170830         // -?Impostazione nome programma a video?
020400170830         VT1pgm = SDSpgmName;
020500170830
020600170830         // -?Pulizia parametri per Batch?
020700170830         clear  tnvrSPI1ds;
020800170830
020900170830       ENDSR;
021000170830
021100170830       //--------------------------------------------------------------
021200170830       //?Reperimento Dati del job (Utente/Operativi).                 ?
021300170830       //--------------------------------------------------------------
021400170830       BEGSR  sr_DatiJob;
021500170830
021600170830         in(e) §AzUte;
021700170830         if NOT %error;
021800170830           in(e) §DatiUte;
021900170830         endif;
022000170830         if %error or RSut = *blank;
022100170830           tibs34r ( tibs34ds );
022200170830           in §AzUte;
022300170830           in §DatiUte;
022400170830         endif;
022500170830
022600170830       ENDSR;
022700170830
022800170830       //--------------------------------------------------------------
022900170830       //?Gestione videata D01.                                        ?
023000170830       //--------------------------------------------------------------
023100170830       BEGSR  sr_GesD01;
023200170830
023300170830         // -?Inizializzazione videata?
023400170830         if  $InzD01 = *on;
023500170830           exsr  sr_InzD01;
023600170830           $InzD01 = *off;
023700170830         endif;
023800170830
023900170830         // -?Emissione videata?
024000170830         write  VRSP2T1;
024100170830         exfmt  VRSP2D1;
024200170830
024300170830         clear  VIDmsg;
024400170830         reset  $ErrMessage;
024500170830         reset  $ErrGenerico;
024600170830
024700170830         Select;
024800170830
024900170830           // -?Errore: sistema informativo errato?
025000170830           When  %subst( KNSIF : 1 : 3 ) <> 'GAI';
025100170830             $Fine  = *on;
025200170830
025300170830           // -?F3=Fine?
025400170830           When  dsp_aid = c_F03;
025500170830             $Fine = *on;
025600170830
025700170830           // -?F9=Visualizzazione WrkF WSPLIT0F?
025800170830           When  dsp_aid = c_F09;
025900170830             Qcmd = 'DSPDBF file(*libl/WSPLIT0F) mbr(*first) output(*)';
026000170830             exsr  sr_ExecCmd;
026100170830
026200170830           // -?Invio / F6=Conferma?
026300170830           Other;
026400170830             exsr  sr_CtrD01;
026500170830             if  $ErrGenerico = *on;
026600170830               leavesr;
026700170830             endif;
026800170830             if  dsp_aid = c_F06;
026900170830               exsr  sr_SbmJob;
027000170830               $Fine = *on;
027100170830             endif;
027200170830
027300170830         ENDSL;
027400170830
027500170830       ENDSR;
027600170830
027700170830       //--------------------------------------------------------------
027800170830       //?Inizializzazione videata D01.                                ?
027900170830       //--------------------------------------------------------------
028000170830       BEGSR  sr_InzD01;
028100170830
028200170830         clear  VRSP2D1;
028300170830
028400170830         // -?Controllo del sistema informativo?
028500170830         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
028600170830           VIDmsg = sk_Msg(01);
028700170830           $ErrMessage  = *on;
028800170830           $ErrGenerico = *on;
028900170830           leavesr;
029000170830         endif;
029100170830
029200170830         // -?Impostazione Data Decorrenza nuovi soggetti?
029300170830         //  ?(oggi)?
029400170830         wDate_Eur = %date();
029500170830         V1Cdde = %dec( wDate_Eur );
029600170830
029700170830         // -?Impostazione Data Scadenza soggetti NON più presenti?
029800170830         //  ?(ieri)?
029900170830         wDate_Eur = %date() - %days(1);
030000170830         V1Cdsc = %dec( wDate_Eur );
030100170830
030200170830       ENDSR;
030300170830
030400170830       //--------------------------------------------------------------
030500170830       //?Controllo dati nella videata D01.                            ?
030600170830       //--------------------------------------------------------------
030700170830       BEGSR  sr_CtrD01;
030800170830
030900170830         // -?Spegnimento indicatori di posizionamento cursore?
031000170830         %subst( IndDspF : 50 ) = *off;
031100170830
031200170830         // -?Controllo Data Decorrenza nuovi soggetti?
031300170830         clear  WLBdat;
031400170830         if  V1Cdde = *zero;
031500170830           $PosCurDDE   = *on;
031600170830           $ErrMessage  = *on;
031700170830           $ErrGenerico = *on;
031800170830           VIDmsg = sk_Msg(02);
031900170830           leavesr;
032000170830         endif;
032100170830         G08dat = V1Cdde;
032200170830         xsrda8 ( WLBdat );
032300170830         if  G08err =  *on;
032400170830           $PosCurDDE   = *on;
032500170830           $ErrMessage  = *on;
032600170830           $ErrGenerico = *on;
032700170830           VIDmsg = sk_Msg(03);
032800170830           leavesr;
032900170830         endif;
033000170830         V1Cdde  = G08dat;
033100170830         TNVRSPI1ds.VRSP1dde = G08inv;
033200170830
033300170830         // -?Controllo Data Scadenza soggetti NON più presenti?
033400170830         clear  WLBdat;
033500170830         if  V1Cdsc = *zero;
033600170830           $PosCurDSC   = *on;
033700170830           $ErrMessage  = *on;
033800170830           $ErrGenerico = *on;
033900170830           VIDmsg = sk_Msg(02);
034000170830           leavesr;
034100170830         endif;
034200170830         G08dat = V1Cdsc;
034300170830         xsrda8 ( WLBdat );
034400170830         if  G08err =  *on;
034500170830           $PosCurDSC   = *on;
034600170830           $ErrMessage  = *on;
034700170830           $ErrGenerico = *on;
034800170830           VIDmsg = sk_Msg(03);
034900170830           leavesr;
035000170830         endif;
035100170830         V1Cdsc  = G08dat;
035200170830         TNVRSPI1ds.VRSP1dsc = G08inv;
035300170830
035400170830       ENDSR;
035500170830
035600170830       //--------------------------------------------------------------
035700170830       //?Sottomissione lavoro batch                                   ?
035800170830       //--------------------------------------------------------------
035900170830       BEGSR  sr_SbmJob;
036000170830
036100170830         // -?Lancio?
036200170830         kpjbu = TNVRSPI1ds;
036300170830         kcoaz = 'VRSP';
036400170830         if  KNMUS = *all'1';
036500170830           tnvrSPI1r ( kpjba );
036600170830         else;
036700170830           bch10 ( kpjba );
036800170830         endif;
036900170830
037000170830       ENDSR;
037100170830
037200170830       //--------------------------------------------------------------
037300170830       //?Esecuzione del comando (già impostato).                      ?
037400170830       //--------------------------------------------------------------
037500170830       BEGSR  sr_ExecCmd;
037600170830
037700170830         clear Qcap0100;
037800170830         Qcabcsdh = *off;
037900170830         Qcapa    = *off;
038000170830         Qcacmdss = *off;
038100170830         Qcaerved = *allX'00';
038200170830
038300170830         clear Qusec;
038400170830         Qusbprv  = %size(Qusec);
038500170830
038600170830         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
038700170830                           %size(Qcap0100) : 'CPOP0100' : *omit :
038800170830                           0 : 0 : Qusec);
038900170830
039000170830         //if  Qusei <> *blank;
039100170830         //  ...;
039200170830         //endif;
039300170830
039400170830       ENDSR;
039500170830
039600170830       //--------------------------------------------------------------
039700170830       //?Operazioni finali.                                           ?
039800170830       //--------------------------------------------------------------
039900170830       BEGSR  sr_RoutEnd;
040000170830
040100170830         // -?Chiusura pgm?
040200170830         return;
040300170830
040400170830       ENDSR;
040500170830
040600170830      /end-free
040700170830
040800170830       //--------------------------------------------------------------
040900170830       //?Schiere a tempo di compilazione.
041000170830       //--------------------------------------------------------------
041100170830
041200170830** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
041300170830Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire       1
041400170830Data obbligatoria                                                              2
041500170830Data formalmente errata                                                        3
