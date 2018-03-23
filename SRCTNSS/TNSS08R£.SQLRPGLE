000100140113       //==============================================================
000200140113       //?Statistica Numero Pagine di Packing-List per Cliente         ?
000300140113       //==============================================================
000400140113
000500140113       //--------------------------------------------------------------
000600140113       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140113       //--------------------------------------------------------------
000800140113
000900140113     /*PRM dbgview(*source)
001000140114     /*CMD ovrdbf file(TIPDL00F) tofile(GAITRAAZP/TIPDL00F) +
001100140114     /*CMD        ovrscope(*calllvl)
001200140114     /*END dltovr file(TIPDL00F) lvl(*)
001300140113     /*END
001400140113
001500140113       //--------------------------------------------------------------
001600140113       //?Specifiche di controllo.                                     ?
001700140113       //--------------------------------------------------------------
001800140113
001900140113     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000140114     h alwnull(*inputonly)
002100140114     h dftactgrp(*no)
002200140114     h bnddir('TRUL')
002300140113
002400140113       //--------------------------------------------------------------
002500140113       //?Dichiarazione file.                                          ?
002600140113       //--------------------------------------------------------------
002700140113
002800140113       // -?Organigramma?
002900140113     fAZORG01L  if   e           k disk
003000140114
003100140114      // -?Anagrafica Commerciali?
003200140114     fAZCMM01L  if   e           k disk
003300140113
003400140113       // -?Video?
003500140114     fTNSS08D   cf   e             workstn
003600140113     f                                     indds(IndDspF)
003700140113     f                                     infds(InfDspF)
003800140113
003900140113       //--------------------------------------------------------------
004000140113       //?Definizione costanti.                                        ?
004100140113       //--------------------------------------------------------------
004200140115
004300140115       // -?Codice Utente in TABEL00F?
004400140115     d c_Kut           c                   const(1)
004500140113
004600140113       // -?Costante per controllo "caratteri solo numerici"?
004700140113     d c_Digits        c                   const('0123456789')
004800140115
004900140115       // -?Comandi di sistema?
005000140115     d c_CmdDspDbf     c                   const('DSPDBF +
005100140115     d                                            file(*libl/WFPDF00F) +
005200140115     d                                            mbr(*first) +
005300140115     d***                                         rcdslt(*yes) +
005400140115     d                                            output(*)')
005500140113
005600140113       // -?Tasti funzionali a video?
005700140113     d c_F01           c                   const(x'31')
005800140113     d c_F02           c                   const(x'32')
005900140113     d c_F03           c                   const(x'33')
006000140113     d c_F04           c                   const(x'34')
006100140113     d c_F05           c                   const(x'35')
006200140113     d c_F06           c                   const(x'36')
006300140113     d c_F07           c                   const(x'37')
006400140113     d c_F08           c                   const(x'38')
006500140113     d c_F09           c                   const(x'39')
006600140113     d c_F10           c                   const(x'3A')
006700140113     d c_F11           c                   const(x'3B')
006800140113     d c_F12           c                   const(x'3C')
006900140113     d c_F13           c                   const(x'B1')
007000140113     d c_F14           c                   const(x'B2')
007100140113     d c_F15           c                   const(x'B3')
007200140113     d c_F16           c                   const(x'B4')
007300140113     d c_F17           c                   const(x'B5')
007400140113     d c_F18           c                   const(x'B6')
007500140113     d c_F19           c                   const(x'B7')
007600140113     d c_F20           c                   const(x'B8')
007700140113     d c_F21           c                   const(x'B9')
007800140113     d c_F22           c                   const(x'BA')
007900140113     d c_F23           c                   const(x'BB')
008000140113     d c_F24           c                   const(x'BC')
008100140113     d c_Enter         c                   const(x'F1')
008200140113     d c_RollDown      c                   const(x'F4')
008300140113     d c_RollUp        c                   const(x'F5')
008400140113
008500140113       //--------------------------------------------------------------
008600140113       //?Definizione schiere.                                         ?
008700140113       //--------------------------------------------------------------
008800140113
008900140113       // -?Messaggi di errore?
009000140117     d sk_Msg          s             78    dim(17)  ctdata  perrcd( 1)
009100140113
009200140113       //--------------------------------------------------------------
009300140113       //?Definizione aree dati.                                       ?
009400140113       //--------------------------------------------------------------
009500140113
009600140113       // -?Dati utente?
009700140113     d §AzUte        e ds                  extname(AZUTE00F)
009800140113     d                                     dtaara
009900140113     d §DatiUte      e ds                  extname(dDatiUte)
010000140113     d                                     dtaara
010100140113
010200140113       //--------------------------------------------------------------
010300140113       //?Definizione strutture dati.                                  ?
010400140113       //--------------------------------------------------------------
010500140113
010600140113       // -?InfDS?
010700140113     d InfDspF         ds
010800140113     d   dsp_aid             369    369a
010900140113     d   sfl_rrn             376    377i 0
011000140113     d   min_nrr             378    379i 0
011100140113     d   num_rcds            380    381i 0
011200140113
011300140113       // -?Indicatori su DspF?
011400140113     d IndDspF         ds                  inz
011500140115         // -?Abilitazione tasti funzionali?
011600140115     d   F9Attivo                      n   overlay(IndDspF : 09)
011700140113         // -?Emissione messaggio di errore?
011800140113     d   ErrMessage                    n   overlay(IndDspF : 28)
011900140113         // -?Visualizzazione campi a video?
012000140115     d   VisualDSU                     n   overlay(IndDspF : 40)
012100140113     d   VisualFIL                     n   overlay(IndDspF : 41)
012200140113     d   VisualCAR                     n   overlay(IndDspF : 42)
012300140113         // -?Posizionamento cursore & segnalazione errore?
012400140113     d   PosCurDSI                     n   overlay(IndDspF : 51)
012500140113     d   PosCurDSF                     n   overlay(IndDspF : 52)
012600140115     d   PosCurCMU                     n   overlay(IndDspF : 53)
012700140115     d   PosCurFIL                     n   overlay(IndDspF : 54)
012800140115     d   PosCurCAR                     n   overlay(IndDspF : 55)
012900140113         //   -?Riemissione videata?
013000140113     d   ErrGenerico                   n   overlay(IndDspF : 99)
013100140114
013200140114       // -?Status ds?
013300140114     d Status         sds
013400140114     d   SDSpgmName      *proc
013500140114
013600140114       // -?Parametri ricevuti?
013700140114     d KPJBA         e ds
013800140114
013900140114       // -?Tab. "05"  = Aree?
014000140114     d ds05          e ds                  inz         qualified
014100140113
014200140113       //--------------------------------------------------------------
014300140113       //?Definizione variabili globali.                               ?
014400140113       //--------------------------------------------------------------
014500140113
014600140113       // -?Flags booleani?
014700140113     d $Fine           s               n   inz(*off)
014800140113     d $ErrAbilit      s               n   inz(*off)
014900140113     d $InzD01         s               n   inz(*on)
015000140113
015100140113       // -?Variabili per la gestione del video?
015200140113     d $Video          s              2    inz('D1')
015300140114
015400140114       // -?Stringa SQL da eseguire?
015500140114     d wSQL            s           2000    inz   varying
015600140114
015700140114       // -?Campi estratti via SQL?
015800140114     d wPDLdti         s              8  0 inz
015900140114     d wPDLdtf         s              8  0 inz
016000140115     d SavV1dsi        s                   inz   like(V1Cdsi)
016100140115     d SavV1dsf        s                   inz   like(V1Cdsf)
016200140113
016300140113       //--------------------------------------------------------------
016400140113       //?Definizione prototipi procedure.                             ?
016500140113       //--------------------------------------------------------------
016600140113
016700140113       // -?Reperimento dati utente?
016800140113     d TIBS34ds      e ds                  inz
016900140113      /copy gaitrasrc/srcProtoPR,TIBS34R
017000140113
017100140113       // -?Controllo abilitazioni utente?
017200140113     d TNTAA1ds      e ds                  inz
017300140113     d   ITAA1caut   e                     inz('§utegtc')
017400140113      /copy gaitrasrc/srcProtoPR,TNTAA1C
017500140113
017600140113       // -?Caricamento Filiali/Aree gestite dall'utente?
017700140113     d TRUL31ds      e ds                  inz
017800140113     d   sk_POg               10    759    inz   dim(250)
017900140113     d TRUL31ds2     e ds                  inz
018000140113     d   sk_Car               22    171    inz   dim(50)
018100140113     d trul31r         pr                  extpgm('TRUL31R')
018200140113     d   kpjba                             likeds(KPJBA)
018300140113     d   trul31ds                          likeds(TRUL31ds)
018400140113     d   trul31ds2                         likeds(TRUL31ds2)
018500140114
018600140114       // -?Ricerca vecchie tabelle (TABEL00F)?
018700140115     d TABELds       e ds                  extname(TABEL00F)
018800140115     d                                     based(nullPtr)
018900140114      /copy gaitrasrc/srcProtoPI,X§TABER
019000140114      /copy gaitrasrc/srcProtoPR,X§TABER
019100140114
019200140114       // -?Reperimento dati tabelle (TABEL00F o TNTBE01L)?
019300140114      /copy gaitrasrc/srcProtoPI,TRULTAB
019400140114      /copy gaitrasrc/srcProtoPR,TRULTAB
019500140114
019600140114       // -?Richiamo ricerca commerciali?
019700140114      /copy gaitrasrc/srcprotoPI,TRMK43R_1
019800140114      /copy gaitrasrc/srcprotoPR,TRMK43R
019900140113
020000140113       // -?Controllo ed inversione date?
020100140113     d WLBdat          ds                  inz
020200140114     d   G08dat                       8  0 inz
020300140114     d   G08inv                       8  0 inz
020400140114     d   G08err                       1    inz('3')
020500140114     d   G08tgi                       5  0 inz
020600140113      /copy gaitrasrc/srcProtoPr,XSRDA8
020700140115
020800140115       // -?Richiamo diretto lavoro batch?
020900140115     d TNSS09ds      e ds                  inz
021000140115     d tnss09r         pr                  extpgm('TNSS09R')
021100140115     d   kpjba                             likeds(KPJBA)
021200140114
021300140114       // -?Personalizzazione lavoro batch?
021400140115     d*// Bch09           pr                  extpgm('BCH09')
021500140115     d*//   kpjba                             likeds(KPJBA)
021600140114
021700140114       // -?Sottomissione lavoro batch?
021800140114      /copy gaitrasrc/srcProtoPR,BCH10
021900140115
022000140115       // -?Parametri API QCAPCMD (Process Commands)?
022100140115     d Qcmd            s           2048    inz  varying
022200140115      /copy qSysInc/qRpgleSrc,QCAPCMD
022300140115       // -?API QCAPCMD (Process Commands)?
022400140115      /copy gaitrasrc/srcProtoPR,QCAPCMD
022500140115
022600140115       // -?Parametri gestione errori API.?
022700140115      /copy qsysinc/qrpglesrc,QUSEC
022800140113
022900140113       //--------------------------------------------------------------
023000140113       //?Definizione key-list.                                        ?
023100140113       //--------------------------------------------------------------
023200140113
023300140113       // -?File AZORG01L?
023400140113     d keyAZORG01    e ds                  extname(AZORG01L : *key)
023500140113     d                                     prefix(k_)   inz
023600140114
023700140114       // -?File AZCMM01L?
023800140114     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
023900140114     d                                     prefix(k_)   inz
024000060330
024100140113       //--------------------------------------------------------------
024200140113       //?Riepilogo indicatori utilizzati.                             ?
024300140113       //--------------------------------------------------------------
024400140113
024500140113
024600140113       //--------------------------------------------------------------
024700140113       //?M A I N - L I N E                                            ?
024800140113       //--------------------------------------------------------------
024900140113
025000060330     c     *Entry        plist
025100060330     c                   parm                    KPJBA
025200140113
025300140113      /free
025400140113
025500140113       // -?Operazioni iniziali?
025600140113       exsr  sr_RoutInz;
025700140113
025800140113       // -?Ciclo di gestione del file video?
025900140113       DoW  $Fine = *off;
026000140113
026100140113         select;
026200140113
026300140113           // -?Richiesta parametri iniziali?
026400140113           when $Video = 'D1';
026500140113             exsr  sr_GesD01;
026600140113
026700140113           // -?? ? ??
026800140113           other;
026900140113             $Fine = *on;
027000140113
027100140113         endsl;
027200140113
027300140113       EndDo;
027400140113
027500140113       // -?Operazioni finali?
027600140113       exsr sr_RoutEnd;
027700140113
027800140113       //--------------------------------------------------------------
027900140113       //?Operazioni iniziali.                                         ?
028000140113       //--------------------------------------------------------------
028100140113       BEGSR  sr_RoutInz;
028200140114
028300140114         // -?Impostazione opzioni per SQL?
028400140114         exec sql   set  option  DynUsrPrf = *Owner,
028500140114                                 CloSqlCsr = *EndMod;
028600140113
028700140113         // -?Impostazione chiusura?
028800140113         *inLR = *on;
028900140113
029000140113         // -?Reperimento dati job?
029100140113         exsr  sr_DatiJob;
029200140113
029300140113         // -?Impostazione nome programma a video?
029400140113         V1Tpgm = SDSpgmName;
029500140113
029600140113         // -?Controllo se utente autorizzato alla funzione?
029700140113         reset  TNTAA1ds;
029800140114         //ITAA1caut = '§utegtc';       ?(già così)?
029900140113         kpjbu     = TNTAA1ds;
030000140113         TNTAA1C ( kpjba );
030100140113         TNTAA1ds = kpjbu;
030200140113         if  oTAA1fabi = 'N';
030300140113           $ErrAbilit  = *on;
030400140113           ErrMessage  = *on;
030500140113           ErrGenerico = *on;
030600140113           PosCurDSI   = *on;
030700140114           V1Dmsg = sk_Msg(01);
030800140113           leavesr;
030900140113         endif;
031000140113
031100140113         // -?Caricamento filiali gestite dall'utente?
031200140113         clear  TRUL31ds;
031300140113         clear  TRUL31ds2;
031400140113         I31abi = oTAA1cabi;
031500140113         I31cdi = DUTdis;
031600140113         I31car = DUTare;
031700140113         I31cpo = DUTpou;
031800140113         TRUL31R ( kpjba : trul31ds : trul31ds2);
031900140113         if  O31pog <= *zero;
032000140113           $ErrAbilit  = *on;
032100140113           ErrMessage  = *on;
032200140113           ErrGenerico = *on;
032300140114           V1Dmsg = sk_Msg(01);
032400140113           leavesr;
032500140113         endif;
032600140113
032700140113         // -?Pulizia / impostazione iniziale dei campi chiave?
032800140113         clear  keyAZORG01;
032900140114         clear  keyAZCMM01;
033000140113
033100140113       ENDSR;
033200140113
033300140113       //--------------------------------------------------------------
033400140113       //?Reperimento Dati del job (Utente/Operativi).                 ?
033500140113       //--------------------------------------------------------------
033600140113       BEGSR  sr_DatiJob;
033700140113
033800140113         in(e) §AzUte;
033900140113         if NOT %error;
034000140113           in(e) §DatiUte;
034100140113         endif;
034200140113         if %error or RSut = *blank;
034300140113           tibs34r ( tibs34ds );
034400140113           in §AzUte;
034500140113           in §DatiUte;
034600140113         endif;
034700140113
034800140113       ENDSR;
034900140113
035000140113       //--------------------------------------------------------------
035100140113       //?Gestione videata D01.                                        ?
035200140113       //--------------------------------------------------------------
035300140113       BEGSR  sr_GesD01;
035400140113
035500140113         // -?Inizializzazione videata?
035600140113         if  $InzD01 = *on;
035700140113           exsr  sr_InzD01;
035800140113           $InzD01 = *off;
035900140113         endif;
036000140113
036100140113         // -?Emissione videata?
036200140114         write  SS08T01;
036300140114         exfmt  SS08D01;
036400140113
036500140113         clear  V1Dmsg;
036600140113         reset  ErrMessage;
036700140113         reset  ErrGenerico;
036800140113
036900140113         SELECT;
037000140113
037100140113           // -?Utente NON autorizzato?
037200140114           WHEN  $ErrAbilit;
037300140113             $Fine = *on;
037400140113
037500140113           // -?F3=Fine?
037600140113           WHEN  dsp_aid = c_F03;
037700140113             $Fine = *on;
037800140115
037900140115           // -?F9=Visualizzazione file WFPDL00F?
038000140115           WHEN  dsp_aid = c_F09;
038100140115             Qcmd = 'DSPDBF file(*libl/WFPDL00F) mbr(*first) output(*)';
038200140115             exsr  sr_ExecCmd;
038300140114
038400140115           //// -?F9=Personalizzazione Batch?
038500140115           //WHEN  dsp_aid = c_F09;
038600140115           //  Kcoaz = 'LSE4';
038700140115           //  Bch09 ( kpjba );
038800140113
038900140114           // -?Invio / F6=Conferma?
039000140113           OTHER;
039100140113             exsr  sr_CtrD01;
039200140113             if  ErrGenerico = *on;
039300140113               leavesr;
039400140113             endif;
039500140114             if  dsp_aid = c_F06;
039600140114               exsr  sr_SbmJob;
039700140117               $Fine = *on;
039800140114             endif;
039900140113
040000140113         ENDSL;
040100140113
040200140113       ENDSR;
040300140113
040400140113       //--------------------------------------------------------------
040500140113       //?Inizializzazione videata D01.                                ?
040600140113       //--------------------------------------------------------------
040700140113       BEGSR  sr_InzD01;
040800140113
040900140115         if  Not $ErrAbilit;
041000140115           clear  SS08D01;
041100140115         endif;
041200140114
041300140114         //V1Cfil = DUTpou;
041400140114         //V1Dfil = DUTdpo;
041500140121
041600140121         // -?Impostazione Output di default?
041700140121         if  DUTlpo = 'S';
041800140121           V1Cout = 'F';
041900140121         else;
042000140121           V1Cout = 'S';
042100140121         endif;
042200140113
042300140113         // -?Impostazione Attributi di Visualizzazione?
042400140115         VisualCAR  = (sk_Car(2) > *zero);
042500140115         VisualFIL  = VisualCAR;
042600140115
042700140115         // -?(Dis)Abilitazione tasti funzionali?
042800140115         F9Attivo = (DUTlpo = 'S');
042900140115
043000140115         if  $ErrAbilit;
043100140115           leavesr;
043200140115         endif;
043300140113
043400140114         // -?Decodifica dati?
043500140113         exsr  sr_CtrD01;
043600140113         %subst(IndDspF : 50) = *off;
043700140113         reset  ErrGenerico;
043800140113         reset  ErrMessage;
043900140113         clear  V1Dmsg;
044000140115
044100140115         clear  SavV1dsi;
044200140115         clear  SavV1dsf;
044300140113
044400140113       ENDSR;
044500140113
044600140113       //--------------------------------------------------------------
044700140113       //?Controllo dati nella videata D01.                            ?
044800140113       //--------------------------------------------------------------
044900140113       BEGSR  sr_CtrD01;
045000140113
045100140113         // -?Spegnimento indicatori di posizionamento cursore?
045200140113         %subst(IndDspF : 50) = *off;
045300140113
045400140114         // -?Date spedizioni?
045500140115         if  V1Cdsi = *zero  and  V1Cdsf = *zero;
045600140114           PosCurDSI   = *on;
045700140114           ErrMessage  = *on;
045800140114           ErrGenerico = *on;
045900140114           V1Dmsg = sk_Msg(02);
046000140114           leavesr;
046100140114         endif;
046200140114
046300140114         // -?Data spedizione iniziale?
046400140113         clear  WLBdat;
046500140114         if  V1Cdsi = *zero;
046600140114           PosCurDSI   = *on;
046700140114           ErrMessage  = *on;
046800140114           ErrGenerico = *on;
046900140114           V1Dmsg = sk_Msg(03);
047000140114           leavesr;
047100140115         endif;
047200140115         G08dat = V1Cdsi;
047300140115         xsrda8 ( WLBdat );
047400140115         if  G08err =  *on;
047500140115           PosCurDSI   = *on;
047600140115           ErrMessage  = *on;
047700140115           ErrGenerico = *on;
047800140115           V1Dmsg = sk_Msg(04);
047900140115           leavesr;
048000140115         endif;
048100140115         V1Cdsi  = G08dat;
048200140115         SS09dsi = G08inv;
048300140114
048400140114         // -?Data spedizione finale?
048500140114         clear  WLBdat;
048600140114         if  V1Cdsf = *zero;
048700140114           PosCurDSF   = *on;
048800140114           ErrMessage  = *on;
048900140114           ErrGenerico = *on;
049000140114           V1Dmsg = sk_Msg(03);
049100140114           leavesr;
049200140115         endif;
049300140115         G08dat = V1Cdsf;
049400140115         xsrda8 ( WLBdat );
049500140115         if  G08err =  *on;
049600140115           PosCurDSF   = *on;
049700140115           ErrMessage  = *on;
049800140115           ErrGenerico = *on;
049900140115           V1Dmsg = sk_Msg(04);
050000140115           leavesr;
050100140115         endif;
050200140115         V1Cdsf  = G08dat;
050300140115         SS09dsf = G08inv;
050400140114
050500140114         // -?Range date spedizioni?
050600140115         if  SS09dsi > SS09dsf;
050700140114           PosCurDSI   = *on;
050800140114           ErrMessage  = *on;
050900140114           ErrGenerico = *on;
051000140114           V1Dmsg = sk_Msg(05);
051100140114           leavesr;
051200140114         endif;
051300140114
051400140114         // -?Corrispondenza date iniziale in TIPDL00F (pdlDTI)?
051500140115         if  V1Cdsi <> SavV1dsi;
051600140115           exsr  sr_CtrlDSI;
051700140115           if  Not ErrGenerico;
051800140115             SavV1dsi =  V1Cdsi;
051900140115           endif;
052000140115         endif;
052100140114         // -?Corrispondenza data finale in TIPDL00F (pdlDTF)?
052200140115         if  V1Cdsf <> SavV1dsf;
052300140115           exsr  sr_CtrlDSF;
052400140115           if  Not ErrGenerico;
052500140115             SavV1dsf =  V1Cdsf;
052600140115           endif;
052700140115         endif;
052800140115
052900140115         if  VisualDSU;
053000140115           if  V1Cdsi2 = *zero;
053100140115             V1Cdsi2  = V1Cdsi;
053200140115           endif;
053300140115           if  V1Cdsf2 = *zero;
053400140115             V1Cdsf2  = V1Cdsf;
053500140115           endif;
053600140115         endif;
053700140115
053800140115         if  ErrGenerico = *on;
053900140115           leavesr;
054000140115         endif;
054100140115
054200140115         // -?Controllo COMMERCIALE UNIFICANTE (facoltativo)?
054300140115         clear  V1Dcmu;
054400140115         Select;
054500140115
054600140115           // -?Non inserito?
054700140115           When  V1Ccmu = *blank  or  V1Ccmu = *zero;
054800140115
054900140115           // -?Ricerca?
055000140115           When  %scan( '?' : V1Ccmu ) > *zero;
055100140115             clear  TRMK43ds;
055200140115             iMK43solU = 'S';
055300140115             iMK43fil  = DUTpou;
055400140115             TRMK43R (kpjba : TRMK43ds);
055500140115             if  oMK43err = *off  and  oMK43fxx = *blank;
055600140115               V1Ccmu = %editc( oMK43cmm : 'X' );
055700140115               V1Dcmu = oMK43des;
055800140115             endif;
055900140115             PosCurCMU   = *on;
056000140115             ErrGenerico = *on;
056100140115             leavesr;
056200140115
056300140115           // -?Controllo / Decodifica:?
056400140115           Other;
056500140115             // ·?Numericità?
056600140115             If  %check( c_Digits : V1Ccmu ) > *zero;
056700140115               PosCurCMU   = *on;
056800140115               ErrMessage  = *on;
056900140115               ErrGenerico = *on;
057000140115               V1Dmsg = sk_Msg(10);
057100140115               leavesr;
057200140115             EndIf;
057300140115             // ·?Esistenza in AZCMM00F?
057400140115             k_CMMcod = %int(V1Ccmu);
057500140115             chain  %kds(keyAZCMM01)  AZCMM000;
057600140115             If  NOT %found(AZCMM01L);
057700140115               PosCurCMU   = *on;
057800140115               ErrMessage  = *on;
057900140115               ErrGenerico = *on;
058000140115               V1Dmsg = sk_Msg(10);
058100140115               leavesr;
058200140115             EndIf;
058300140115             V1Dcmu = CMMdes;
058400140115             // ·?...senza particolarità?
058500140115             if  CMMpar <> *blank;
058600140115               PosCurCMU   = *on;
058700140115               ErrMessage  = *on;
058800140115               ErrGenerico = *on;
058900140115               V1Dmsg = sk_Msg(10);
059000140115               leavesr;
059100140115             endif;
059200140115             // ·?...attivo in quel periodo?
059300140115             if  CMMdtIni  > SS09dsf  or  CMMdtFin < SS09dsi;
059400140115               PosCurCMU   = *on;
059500140115               ErrMessage  = *on;
059600140115               ErrGenerico = *on;
059700140115               V1Dmsg = sk_Msg(10);
059800140115               leavesr;
059900140115             endif;
060000140115             // ·?Abilitazione dell'utente al commerciale?
060100140115             reset  TNTAA1ds;
060200140115             //ITAA1caut = '§utegtc';   ?(già così)?
060300140115             ITAA1cmm  = %int( V1Ccmu );
060400140115             kpjbu     = TNTAA1ds;
060500140115             tntaa1c (kpjba);
060600140115             TNTAA1ds = kpjbu;
060700140115             if  oTAA1fabi = 'N';
060800140115               PosCurCMU   = *on;
060900140115               ErrMessage  = *on;
061000140115               ErrGenerico = *on;
061100140115               V1Dmsg = sk_Msg(10);
061200140115               leavesr;
061300140115             endif;
061400140115             // ·?Selezionabili SOLO unificanti?
061500140115             if  %int( V1Ccmu ) <> CMMuni;
061600140115               PosCurCMU   = *on;
061700140115               ErrMessage  = *on;
061800140115               ErrGenerico = *on;
061900140115               V1Dmsg = sk_Msg(11);
062000140115               leavesr;
062100140115             endif;
062200140115
062300140115         EndSl;
062400140113
062500140115         // -?Controllo FILIALE del comm.le (facoltativa)?
062600140114         clear  V1Dfil;
062700140114         If  V1Cfil <> *zero;
062800140115
062900140115           // -?Non immettere la filiale se richiesto il comm.le?
063000140115           if  V1Ccmu <> *blank  and  V1Ccmu <> *zero;
063100140115             PosCurFIL   = *on;
063200140115             ErrMessage  = *on;
063300140115             ErrGenerico = *on;
063400140115             V1Dmsg = sk_Msg(12);
063500140115             leavesr;
063600140115           endif;
063700140115
063800140114           k_ORGfil = V1Cfil;
063900140114           chain  %kds( keyAZORG01 )  AZORG;
064000140115
064100140114           if  NOT %found(AZORG01L)  or  ORGfva <> *blank;
064200140115             V1Dfil = *all'? ';
064300140114             PosCurFIL   = *on;
064400140114             ErrMessage  = *on;
064500140114             ErrGenerico = *on;
064600140115             V1Dmsg = sk_Msg(13);
064700140114             leavesr;
064800140114           endif;
064900140115
065000140114           V1Dfil = ORGdes;
065100140115
065200140114           if  %lookup( %editc( V1Cfil : 'X' ) : sk_POg ) = *zero;
065300140114             PosCurFIL   = *on;
065400140114             ErrMessage  = *on;
065500140114             ErrGenerico = *on;
065600140115             V1Dmsg = sk_Msg(13);
065700140114             leavesr;
065800140114           endif;
065900140115
066000140114         EndIf;
066100140114
066200140115         // -?Controllo AREA del comm.le (facoltativa)?
066300140114         clear  V1Dcar;
066400140114         Select;
066500140115
066600140117           // -?Non immettere l'area se richiesto il comm.le?
066700140115           When  V1Ccar <> *blank  and  V1Ccar <> *zero  and
066800140115                 V1Ccmu <> *blank  and  V1Ccmu <> *zero;
066900140115             PosCurCAR   = *on;
067000140115             ErrMessage  = *on;
067100140115             ErrGenerico = *on;
067200140115             V1Dmsg = sk_Msg(14);
067300140115             leavesr;
067400140117
067500140117           // -?Non immettere l'area se richiesta la filiale?
067600140117           When  V1Ccar <> *blank  and  V1Ccar <> *zero  and
067700140117                 V1Cfil <> *zero;
067800140117             PosCurCAR   = *on;
067900140117             ErrMessage  = *on;
068000140117             ErrGenerico = *on;
068100140117             V1Dmsg = sk_Msg(15);
068200140117             leavesr;
068300140115
068400140114           // -?Non inserita?
068500140114           When  V1Ccar = *blank  or  V1Ccar = *zero;
068600140115
068700140114           // -?Ricerca?
068800140114           When  %scan( '?' : V1Ccar ) > *zero;
068900140114             clear  V1Ccar;
069000140114             X§Tkut = c_Kut;
069100140114             X§Tcod = '05';
069200140114             clear  X§Tkey;
069300140114             clear  X§Tdes;
069400140114             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
069500140114             V1Ccar = %subst( X§Tkey : 1 : %len(V1Ccar) );
069600140114             V1Dcar = X§Tdes;
069700140114             PosCurCAR   = *on;
069800140114             ErrGenerico = *on;
069900140114             leavesr;
070000140115
070100140114           // -?Controllo / Decodifica:?
070200140114           Other;
070300140114             // ·?Numericità?
070400140114             If  %check( c_Digits : V1Ccar ) > *zero;
070500140114               PosCurCAR   = *on;
070600140114               ErrMessage  = *on;
070700140114               ErrGenerico = *on;
070800140117               V1Dmsg = sk_Msg(16);
070900140114               leavesr;
071000140114             EndIf;
071100140114             // ·?Esistenza in tab. "05"?
071200140114             ds_TNTBE.TBEke1 = V1Ccar;
071300140115             If  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
071400140114                              *omit : *omit : *omit :
071500140114                              *omit : *omit :
071600140114                              *omit : *omit : *omit : *omit :
071700140114                              *omit : *omit : *omit : *omit :
071800140114                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
071900140114                            = *zero      AND
072000140114                 ds_TNTBE.TBEatb = *blank;
072100140114               ds05   = ds_TNTBE.TBEuni;
072200140115               V1Dcar = ds05.§05des;
072300140115             Else;
072400140114               V1Dcar = *all'? ';
072500140114               PosCurCAR   = *on;
072600140114               ErrMessage  = *on;
072700140114               ErrGenerico = *on;
072800140117               V1Dmsg = sk_Msg(16);
072900140114               leavesr;
073000140115             EndIf;
073100140115             // -?Abilitazione dell'utente all'area?
073200140115             If  %lookup( V1Ccar : sk_Car) = *zero;
073300140115               PosCurCAR   = *on;
073400140115               ErrMessage  = *on;
073500140115               ErrGenerico = *on;
073600140117               V1Dmsg = sk_Msg(16);
073700140115               leavesr;
073800140115             EndIf;
073900140115
074000140114         EndSl;
074100140115
074200140115         // -?In SEDE: obbligatorio selezionare commerciale o area?
074300140121         //If  DUTlpo = 'S'  and
074400140121         //   ((V1Ccmu = *blank  or  V1Ccmu = *zero)  and
074500140121         //    (V1Ccar = *blank  or  V1Ccar = *zero));
074600140121         //  PosCurCMU   = *on;
074700140121         //  ErrMessage  = *on;
074800140121         //  ErrGenerico = *on;
074900140121         //  V1Dmsg = sk_Msg(17);
075000140121         //  leavesr;
075100140121         //EndIf;
075200140113
075300140113       ENDSR;
075400140114
075500140114       //--------------------------------------------------------------
075600140114       //?Corrispondenza data iniziale in TIPDL00F                     ?
075700140114       //?(deve essere il giorno iniziale di un periodo estratto)      ?
075800140114       //--------------------------------------------------------------
075900140114       BEGSR  sr_CtrlDSI;
076000140114
076100140114         // -?Preparazione stringa SQL?
076200140114         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
076300140114                   from ';
076400140115         if  %subst( KNSIF : 7 : 1 ) = 'P';
076500140114           wSQL += 'GAITRAAZP';
076600140114         else;
076700140114           wSQL += 'GAITRAAZM';
076800140114         endif;
076900140114         wSQL += '/TIPDL00F +
077000140114                  where PDLdti <= ' + %editc( SS09dsi : 'X' ) + ' +
077100140114                    and PDLdtf >= ' + %editc( SS09dsi : 'X' ) + ' +
077200140114                    for fetch only';
077300140114
077400140114         // -?Dichiarazione cursore?
077500140114         exec sql   prepare S1   from :wSQL;
077600140114         exec sql   declare C1   cursor for S1;
077700140114
077800140114         if  SQLcode < *zero;
077900140114           PosCurDSI   = *on;
078000140114           ErrMessage  = *on;
078100140114           ErrGenerico = *on;
078200140114           V1Dmsg = sk_Msg(06);
078300140114           leavesr;
078400140114         endif;
078500140114
078600140114         // -?Apertura cursore?
078700140114         exec sql   open C1;
078800140114
078900140114         if  SQLcode < *zero;
079000140114           PosCurDSI   = *on;
079100140114           ErrMessage  = *on;
079200140114           ErrGenerico = *on;
079300140114           V1Dmsg = sk_Msg(06);
079400140114           leavesr;
079500140114         endif;
079600140114
079700140114         // -?Lettura cursore?
079800140114         clear  wPDLdti;
079900140114         clear  wPDLdtf;
080000140114         exec sql   fetch next   from C1   into :wPDLdti, :wPDLdtf;
080100140114
080200140114         select;
080300140114           // -?Errore in elaborazione: si chiude il pgm.?
080400140114           when  SQLcode < *zero;
080500140114             PosCurDSI   = *on;
080600140114             ErrMessage  = *on;
080700140114             ErrGenerico = *on;
080800140114             V1Dmsg = sk_Msg(06);
080900140114           // -?File vuoto: nessun dato estratto?
081000140114           when  SQLcode = 100  or  wPDLdti = *zero;
081100140122             //PosCurDSI   = *on;
081200140122             //ErrMessage  = *on;
081300140122             //ErrGenerico = *on;
081400140122             //V1Dmsg = sk_Msg(07);
081500140122             exsr  sr_DSIanom;
081600140114           // -?La data iniziale sarà quella iniziale della settimana?
081700140115           when wPDLdti <> SS09dsi;
081800140115             VisualDSU   = *on;
081900140115             V1Cdsi2     = V1Cdsi;
082000140115             SS09dsi     = wPDLdti;
082100140115             reset  WLBdat;
082200140115             G08inv = wPDLdti;
082300140115             xsrda8 ( WLBdat );
082400140115             V1Cdsi = G08dat;
082500140114         endsl;
082600140114
082700140114         // -?Chiusura cursore?
082800140114         exec sql   close C1;
082900140114
083000140114       ENDSR;
083100140122
083200140122       //--------------------------------------------------------------
083300140122       //?Non trovata "data inizio periodo":                           ?
083400140122       //?ricerca data iniziale del periodo successivo.                ?
083500140122       //--------------------------------------------------------------
083600140122       BEGSR  sr_DSIanom;
083700140122
083800140122         // -?Preparazione stringa SQL?
083900140122         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
084000140122                   from ';
084100140122         if  %subst( KNSIF : 7 : 1 ) = 'P';
084200140122           wSQL += 'GAITRAAZP';
084300140122         else;
084400140122           wSQL += 'GAITRAAZM';
084500140122         endif;
084600140122         wSQL += '/TIPDL00F +
084700140122                  where PDLdti >= ' + %editc( SS09dsi : 'X' ) + ' +
084800140122                    and PDLdtf >= ' + %editc( SS09dsi : 'X' ) + ' +
084900140122                  order by PDLdti asc +
085000140122                    for fetch only';
085100140122
085200140122         // -?Dichiarazione cursore?
085300140122         exec sql   prepare S1B   from :wSQL;
085400140122         exec sql   declare C1B   cursor for S1B;
085500140122
085600140122         if  SQLcode < *zero;
085700140122           PosCurDSI   = *on;
085800140122           ErrMessage  = *on;
085900140122           ErrGenerico = *on;
086000140122           V1Dmsg = sk_Msg(06);
086100140122           leavesr;
086200140122         endif;
086300140122
086400140122         // -?Apertura cursore?
086500140122         exec sql   open C1B;
086600140122
086700140122         if  SQLcode < *zero;
086800140122           PosCurDSI   = *on;
086900140122           ErrMessage  = *on;
087000140122           ErrGenerico = *on;
087100140122           V1Dmsg = sk_Msg(06);
087200140122           leavesr;
087300140122         endif;
087400140122
087500140122         // -?Lettura cursore?
087600140122         clear  wPDLdti;
087700140122         clear  wPDLdtf;
087800140122         exec sql   fetch next   from C1B   into :wPDLdti, :wPDLdtf;
087900140122
088000140122         select;
088100140122           // -?Errore in elaborazione: si chiude il pgm.?
088200140122           when  SQLcode < *zero;
088300140122             PosCurDSI   = *on;
088400140122             ErrMessage  = *on;
088500140122             ErrGenerico = *on;
088600140122             V1Dmsg = sk_Msg(06);
088700140122           // -?File vuoto: nessun dato estratto?
088800140122           when  SQLcode = 100  or  wPDLdti = *zero;
088900140122             PosCurDSI   = *on;
089000140122             ErrMessage  = *on;
089100140122             ErrGenerico = *on;
089200140122             V1Dmsg = sk_Msg(07);
089300140122           // -?La data iniziale sarà quella iniziale della settimana?
089400140122           when wPDLdti <> SS09dsi;
089500140122             VisualDSU   = *on;
089600140122             V1Cdsi2     = V1Cdsi;
089700140122             SS09dsi     = wPDLdti;
089800140122             reset  WLBdat;
089900140122             G08inv = wPDLdti;
090000140122             xsrda8 ( WLBdat );
090100140122             V1Cdsi = G08dat;
090200140122         endsl;
090300140122
090400140122         // -?Chiusura cursore?
090500140122         exec sql   close C1B;
090600140122
090700140122       ENDSR;
090800140114
090900140114       //--------------------------------------------------------------
091000140114       //?Corrispondenza data finale in TIPDL00F                       ?
091100140114       //?(deve essere il giorno finale di un periodo estratto)        ?
091200140114       //--------------------------------------------------------------
091300140114       BEGSR  sr_CtrlDSF;
091400140114
091500140114         // -?Preparazione stringa SQL?
091600140114         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
091700140114                   from ';
091800140115         if  %subst( KNSIF : 7 : 1 ) = 'P';
091900140114           wSQL += 'GAITRAAZP';
092000140114         else;
092100140114           wSQL += 'GAITRAAZM';
092200140114         endif;
092300140114         wSQL += '/TIPDL00F +
092400140114                  where PDLdti <= ' + %editc( SS09dsf : 'X' ) + ' +
092500140114                    and PDLdtf >= ' + %editc( SS09dsf : 'X' ) + ' +
092600140114                    for fetch  only';
092700140114
092800140114         // -?Dichiarazione cursore?
092900140114         exec sql   prepare S2   from :wSQL;
093000140114         exec sql   declare C2   cursor for S2;
093100140114
093200140114         if  SQLcode < *zero;
093300140114           PosCurDSI   = *on;
093400140114           ErrMessage  = *on;
093500140114           ErrGenerico = *on;
093600140114           V1Dmsg = sk_Msg(06);
093700140114           leavesr;
093800140114         endif;
093900140114
094000140114         // -?Apertura cursore?
094100140114         exec sql   open C2;
094200140114
094300140114         if  SQLcode < *zero;
094400140114           PosCurDSI   = *on;
094500140114           ErrMessage  = *on;
094600140114           ErrGenerico = *on;
094700140114           V1Dmsg = sk_Msg(06);
094800140114           leavesr;
094900140114         endif;
095000140114
095100140114         // -?Lettura cursore?
095200140114         clear  wPDLdti;
095300140114         clear  wPDLdtf;
095400140114         exec sql   fetch next   from C2   into :wPDLdti, :wPDLdtf;
095500140114
095600140114         select;
095700140114           // -?Errore in elaborazione: si chiude il pgm.?
095800140114           when  SQLcode < *zero;
095900140114             PosCurDSF   = *on;
096000140114             ErrMessage  = *on;
096100140114             ErrGenerico = *on;
096200140114             V1Dmsg = sk_Msg(06);
096300140114           // -?File vuoto: nessun dato estratto?
096400140114           when  SQLcode = 100  or  wPDLdtf = *zero;
096500140122             //PosCurDSF   = *on;
096600140122             //ErrMessage  = *on;
096700140122             //ErrGenerico = *on;
096800140122             //V1Dmsg = sk_Msg(07);
096900140122             exsr  sr_DSFanom;
097000140115           // -?La data finale sarà quella finale della settimana?
097100140115           when wPDLdtf <> SS09dsf;
097200140115             VisualDSU   = *on;
097300140115             V1Cdsf2     = V1Cdsf;
097400140115             SS09dsf     = wPDLdtf;
097500140115             reset  WLBdat;
097600140115             G08inv = wPDLdtf;
097700140115             xsrda8 ( WLBdat );
097800140115             V1Cdsf = G08dat;
097900140114         endsl;
098000140114
098100140114         // -?Chiusura cursore?
098200140114         exec sql   close C2;
098300140114
098400140114       ENDSR;
098500140122
098600140122       //--------------------------------------------------------------
098700140122       //?Non trovata "data fine periodo":                             ?
098800140122       //?ricerca data finale del periodo precedente.                  ?
098900140122       //--------------------------------------------------------------
099000140122       BEGSR  sr_DSFanom;
099100140122
099200140122         // -?Preparazione stringa SQL?
099300140122         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
099400140122                   from ';
099500140122         if  %subst( KNSIF : 7 : 1 ) = 'P';
099600140122           wSQL += 'GAITRAAZP';
099700140122         else;
099800140122           wSQL += 'GAITRAAZM';
099900140122         endif;
100000140122         wSQL += '/TIPDL00F +
100100140122                  where PDLdti <= ' + %editc( SS09dsf : 'X' ) + ' +
100200140122                    and PDLdtf <= ' + %editc( SS09dsf : 'X' ) + ' +
100300140122                  order by PDLdtf desc +
100400140122                    for fetch only';
100500140122
100600140122         // -?Dichiarazione cursore?
100700140122         exec sql   prepare S2B   from :wSQL;
100800140122         exec sql   declare C2B   cursor for S2B;
100900140122
101000140122         if  SQLcode < *zero;
101100140122           PosCurDSI   = *on;
101200140122           ErrMessage  = *on;
101300140122           ErrGenerico = *on;
101400140122           V1Dmsg = sk_Msg(06);
101500140122           leavesr;
101600140122         endif;
101700140122
101800140122         // -?Apertura cursore?
101900140122         exec sql   open C2B;
102000140122
102100140122         if  SQLcode < *zero;
102200140122           PosCurDSI   = *on;
102300140122           ErrMessage  = *on;
102400140122           ErrGenerico = *on;
102500140122           V1Dmsg = sk_Msg(06);
102600140122           leavesr;
102700140122         endif;
102800140122
102900140122         // -?Lettura cursore?
103000140122         clear  wPDLdti;
103100140122         clear  wPDLdtf;
103200140122         exec sql   fetch next   from C2B   into :wPDLdti, :wPDLdtf;
103300140122
103400140122         select;
103500140122           // -?Errore in elaborazione: si chiude il pgm.?
103600140122           when  SQLcode < *zero;
103700140122             PosCurDSI   = *on;
103800140122             ErrMessage  = *on;
103900140122             ErrGenerico = *on;
104000140122             V1Dmsg = sk_Msg(06);
104100140122           // -?File vuoto: nessun dato estratto?
104200140122           when  SQLcode = 100  or  wPDLdti = *zero;
104300140122             PosCurDSI   = *on;
104400140122             ErrMessage  = *on;
104500140122             ErrGenerico = *on;
104600140122             V1Dmsg = sk_Msg(07);
104700140122           // -?La data finale sarà quella finale della settimana?
104800140122           when wPDLdtf <> SS09dsf;
104900140122             VisualDSU   = *on;
105000140122             V1Cdsf2     = V1Cdsf;
105100140122             SS09dsf     = wPDLdtf;
105200140122             reset  WLBdat;
105300140122             G08inv = wPDLdtf;
105400140122             xsrda8 ( WLBdat );
105500140122             V1Cdsf = G08dat;
105600140122         endsl;
105700140122
105800140122         // -?Chiusura cursore?
105900140122         exec sql   close C2B;
106000140122
106100140122       ENDSR;
106200140114
106300140114       //--------------------------------------------------------------
106400140114       //?Sottomissione lavoro batch                                   ?
106500140114       //--------------------------------------------------------------
106600140114       BEGSR  sr_SbmJob;
106700140114
106800140114         // -?Impostazione dei parametri restanti?
106900140115         clear  SS09cmu;
107000140115         if  V1Ccmu > *zero;
107100140115           SS09cmu = %int( V1Ccmu );
107200140115         endif;
107300140115         SS09fil = V1Cfil;
107400140115         clear  SS09car;
107500140115         if  V1Ccar > *zero;
107600140115           SS09car = %int( V1Ccar );
107700140115         endif;
107800140121         SS09out = V1Cout;
107900140114
108000140114         // -?Lancio?
108100140115         kpjbu = TNSS09ds;
108200140115         kcoaz = 'SS09';
108300140114         if  KNMUS = *all'1';
108400140114           tnss09r ( kpjba );
108500140114         else;
108600140114           bch10 ( kpjba );
108700140114         endif;
108800140114
108900140114       ENDSR;
109000140115
109100140115       //--------------------------------------------------------------
109200140115       //?Esecuzione del comando (già impostato).                      ?
109300140115       //--------------------------------------------------------------
109400140115       BEGSR  sr_ExecCmd;
109500140115
109600140115         clear Qcap0100;
109700140115         Qcabcsdh = *off;
109800140115         Qcapa    = *off;
109900140115         Qcacmdss = *off;
110000140115         Qcaerved = *allX'00';
110100140115
110200140115         clear Qusec;
110300140115         Qusbprv  = %size(Qusec);
110400140115
110500140115         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
110600140115                           %size(Qcap0100) : 'CPOP0100' : *omit :
110700140115                           0 : 0 : Qusec);
110800140115
110900140115         //if  Qusei <> *blank;
111000140115         //  ...;
111100140115         //endif;
111200140115
111300140115       ENDSR;
111400140114
111500140114       //--------------------------------------------------------------
111600140114       //?Operazioni finali.                                           ?
111700140114       //--------------------------------------------------------------
111800140114       BEGSR  sr_RoutEnd;
111900140114
112000140114         cloTabella();
112100140114
112200140114         return;
112300140114
112400140114       ENDSR;
112500140113
112600140113      /end-free
112700140113
112800140113       //--------------------------------------------------------------
112900140113       //?Schiere a tempo di compilazione.
113000140113       //--------------------------------------------------------------
113100060331
113200140113** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
113300140114Utente non abilitato alla funzione                                             1
113400140114Impostare il periodo da elaborare                                              2
113500140114Data obbligatoria                                                              3
113600140114Data formalmente errata                                                        4
113700140114Data iniziale incongruente con data finale                                     5
113800140122ERRORE nel reperimento delle date elaborabili: avvisare CED                    6
113900140122Nessuna packing-list risulta elaborabile per questa data                       7
114000140114Impostata la data "DAL" con quella iniziale della settimana a cui si riferisce 8
114100140114Impostata la data "AL" con quella finale della settimana a cui si riferisce    9
114200140115Codice commerciale errato  o  non in gestione all'utente                      10
114300140115Richiedere solo commerciali UNIFICANTI                                        11
114400140117Non impostare il codice filiale SE inserito il commerciale                    12
114500140117Filiale errata                                                                13
114600140117Non impostare il codice area SE inserito il commerciale                       14
114700140117Non impostare il codice area SE inserita la filiale                           15
114800140117Codice Area errato  o  non in gestione all'utente                             16
114900140117Selezionare almeno un commerciale o una filiale o un'area                     17
