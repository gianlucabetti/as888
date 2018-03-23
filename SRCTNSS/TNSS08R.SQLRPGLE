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
012900151022     d   PosCurOUTS                    n   overlay(IndDspF : 56)
013000140113         //   -?Riemissione videata?
013100140113     d   ErrGenerico                   n   overlay(IndDspF : 99)
013200140114
013300140114       // -?Status ds?
013400140114     d Status         sds
013500140114     d   SDSpgmName      *proc
013600140114
013700140114       // -?Parametri ricevuti?
013800140114     d KPJBA         e ds
013900140114
014000140114       // -?Tab. "05"  = Aree?
014100140114     d ds05          e ds                  inz         qualified
014200140113
014300140113       //--------------------------------------------------------------
014400140113       //?Definizione variabili globali.                               ?
014500140113       //--------------------------------------------------------------
014600140113
014700140113       // -?Flags booleani?
014800140113     d $Fine           s               n   inz(*off)
014900140113     d $ErrAbilit      s               n   inz(*off)
015000140113     d $InzD01         s               n   inz(*on)
015100140113
015200140113       // -?Variabili per la gestione del video?
015300140113     d $Video          s              2    inz('D1')
015400140114
015500140114       // -?Stringa SQL da eseguire?
015600140114     d wSQL            s           2000    inz   varying
015700140114
015800140114       // -?Campi estratti via SQL?
015900140114     d wPDLdti         s              8  0 inz
016000140114     d wPDLdtf         s              8  0 inz
016100140115     d SavV1dsi        s                   inz   like(V1Cdsi)
016200140115     d SavV1dsf        s                   inz   like(V1Cdsf)
016300140113
016400140113       //--------------------------------------------------------------
016500140113       //?Definizione prototipi procedure.                             ?
016600140113       //--------------------------------------------------------------
016700140113
016800140113       // -?Reperimento dati utente?
016900140113     d TIBS34ds      e ds                  inz
017000140113      /copy gaitrasrc/srcProtoPR,TIBS34R
017100140113
017200140113       // -?Controllo abilitazioni utente?
017300140113     d TNTAA1ds      e ds                  inz
017400140113     d   ITAA1caut   e                     inz('§utegtc')
017500140113      /copy gaitrasrc/srcProtoPR,TNTAA1C
017600140113
017700140113       // -?Caricamento Filiali/Aree gestite dall'utente?
017800140113     d TRUL31ds      e ds                  inz
017900140113     d   sk_POg               10    759    inz   dim(250)
018000140113     d TRUL31ds2     e ds                  inz
018100140113     d   sk_Car               22    171    inz   dim(50)
018200140113     d trul31r         pr                  extpgm('TRUL31R')
018300140113     d   kpjba                             likeds(KPJBA)
018400140113     d   trul31ds                          likeds(TRUL31ds)
018500140113     d   trul31ds2                         likeds(TRUL31ds2)
018600140114
018700140114       // -?Ricerca vecchie tabelle (TABEL00F)?
018800140115     d TABELds       e ds                  extname(TABEL00F)
018900140115     d                                     based(nullPtr)
019000140114      /copy gaitrasrc/srcProtoPI,X§TABER
019100140114      /copy gaitrasrc/srcProtoPR,X§TABER
019200140114
019300140114       // -?Reperimento dati tabelle (TABEL00F o TNTBE01L)?
019400140114      /copy gaitrasrc/srcProtoPI,TRULTAB
019500140114      /copy gaitrasrc/srcProtoPR,TRULTAB
019600140114
019700140114       // -?Richiamo ricerca commerciali?
019800140114      /copy gaitrasrc/srcprotoPI,TRMK43R_1
019900140114      /copy gaitrasrc/srcprotoPR,TRMK43R
020000140113
020100140113       // -?Controllo ed inversione date?
020200140113     d WLBdat          ds                  inz
020300140114     d   G08dat                       8  0 inz
020400140114     d   G08inv                       8  0 inz
020500140114     d   G08err                       1    inz('3')
020600140114     d   G08tgi                       5  0 inz
020700140113      /copy gaitrasrc/srcProtoPr,XSRDA8
020800140115
020900140115       // -?Richiamo diretto lavoro batch?
021000140115     d TNSS09ds      e ds                  inz
021100140115     d tnss09r         pr                  extpgm('TNSS09R')
021200140115     d   kpjba                             likeds(KPJBA)
021300140114
021400140114       // -?Personalizzazione lavoro batch?
021500140115     d*// Bch09           pr                  extpgm('BCH09')
021600140115     d*//   kpjba                             likeds(KPJBA)
021700140114
021800140114       // -?Sottomissione lavoro batch?
021900140114      /copy gaitrasrc/srcProtoPR,BCH10
022000140115
022100140115       // -?Parametri API QCAPCMD (Process Commands)?
022200140115     d Qcmd            s           2048    inz  varying
022300140115      /copy qSysInc/qRpgleSrc,QCAPCMD
022400140115       // -?API QCAPCMD (Process Commands)?
022500140115      /copy gaitrasrc/srcProtoPR,QCAPCMD
022600140115
022700140115       // -?Parametri gestione errori API.?
022800140115      /copy qsysinc/qrpglesrc,QUSEC
022900140113
023000140113       //--------------------------------------------------------------
023100140113       //?Definizione key-list.                                        ?
023200140113       //--------------------------------------------------------------
023300140113
023400140113       // -?File AZORG01L?
023500140113     d keyAZORG01    e ds                  extname(AZORG01L : *key)
023600140113     d                                     prefix(k_)   inz
023700140114
023800140114       // -?File AZCMM01L?
023900140114     d keyAZCMM01    e ds                  extname(AZCMM01L : *key)
024000140114     d                                     prefix(k_)   inz
024100060330
024200140113       //--------------------------------------------------------------
024300140113       //?Riepilogo indicatori utilizzati.                             ?
024400140113       //--------------------------------------------------------------
024500140113
024600140113
024700140113       //--------------------------------------------------------------
024800140113       //?M A I N - L I N E                                            ?
024900140113       //--------------------------------------------------------------
025000140113
025100060330     c     *Entry        plist
025200060330     c                   parm                    KPJBA
025300140113
025400140113      /free
025500140113
025600140113       // -?Operazioni iniziali?
025700140113       exsr  sr_RoutInz;
025800140113
025900140113       // -?Ciclo di gestione del file video?
026000140113       DoW  $Fine = *off;
026100140113
026200140113         select;
026300140113
026400140113           // -?Richiesta parametri iniziali?
026500140113           when $Video = 'D1';
026600140113             exsr  sr_GesD01;
026700140113
026800140113           // -?? ? ??
026900140113           other;
027000140113             $Fine = *on;
027100140113
027200140113         endsl;
027300140113
027400140113       EndDo;
027500140113
027600140113       // -?Operazioni finali?
027700140113       exsr sr_RoutEnd;
027800140113
027900140113       //--------------------------------------------------------------
028000140113       //?Operazioni iniziali.                                         ?
028100140113       //--------------------------------------------------------------
028200140113       BEGSR  sr_RoutInz;
028300140114
028400140114         // -?Impostazione opzioni per SQL?
028500140114         exec sql   set  option  DynUsrPrf = *Owner,
028600140114                                 CloSqlCsr = *EndMod;
028700140113
028800140113         // -?Impostazione chiusura?
028900140113         *inLR = *on;
029000140113
029100140113         // -?Reperimento dati job?
029200140113         exsr  sr_DatiJob;
029300140113
029400140113         // -?Impostazione nome programma a video?
029500140113         V1Tpgm = SDSpgmName;
029600140113
029700140113         // -?Controllo se utente autorizzato alla funzione?
029800140113         reset  TNTAA1ds;
029900140114         //ITAA1caut = '§utegtc';       ?(già così)?
030000140113         kpjbu     = TNTAA1ds;
030100140113         TNTAA1C ( kpjba );
030200140113         TNTAA1ds = kpjbu;
030300140113         if  oTAA1fabi = 'N';
030400140113           $ErrAbilit  = *on;
030500140113           ErrMessage  = *on;
030600140113           ErrGenerico = *on;
030700140113           PosCurDSI   = *on;
030800140114           V1Dmsg = sk_Msg(01);
030900140113           leavesr;
031000140113         endif;
031100140113
031200140113         // -?Caricamento filiali gestite dall'utente?
031300140113         clear  TRUL31ds;
031400140113         clear  TRUL31ds2;
031500140113         I31abi = oTAA1cabi;
031600140113         I31cdi = DUTdis;
031700140113         I31car = DUTare;
031800140113         I31cpo = DUTpou;
031900140113         TRUL31R ( kpjba : trul31ds : trul31ds2);
032000140113         if  O31pog <= *zero;
032100140113           $ErrAbilit  = *on;
032200140113           ErrMessage  = *on;
032300140113           ErrGenerico = *on;
032400140114           V1Dmsg = sk_Msg(01);
032500140113           leavesr;
032600140113         endif;
032700140113
032800140113         // -?Pulizia / impostazione iniziale dei campi chiave?
032900140113         clear  keyAZORG01;
033000140114         clear  keyAZCMM01;
033100140113
033200140113       ENDSR;
033300140113
033400140113       //--------------------------------------------------------------
033500140113       //?Reperimento Dati del job (Utente/Operativi).                 ?
033600140113       //--------------------------------------------------------------
033700140113       BEGSR  sr_DatiJob;
033800140113
033900140113         in(e) §AzUte;
034000140113         if NOT %error;
034100140113           in(e) §DatiUte;
034200140113         endif;
034300140113         if %error or RSut = *blank;
034400140113           tibs34r ( tibs34ds );
034500140113           in §AzUte;
034600140113           in §DatiUte;
034700140113         endif;
034800140113
034900140113       ENDSR;
035000140113
035100140113       //--------------------------------------------------------------
035200140113       //?Gestione videata D01.                                        ?
035300140113       //--------------------------------------------------------------
035400140113       BEGSR  sr_GesD01;
035500140113
035600140113         // -?Inizializzazione videata?
035700140113         if  $InzD01 = *on;
035800140113           exsr  sr_InzD01;
035900140113           $InzD01 = *off;
036000140113         endif;
036100140113
036200140113         // -?Emissione videata?
036300140114         write  SS08T01;
036400140114         exfmt  SS08D01;
036500140113
036600140113         clear  V1Dmsg;
036700140113         reset  ErrMessage;
036800140113         reset  ErrGenerico;
036900140113
037000140113         SELECT;
037100140113
037200140113           // -?Utente NON autorizzato?
037300140114           WHEN  $ErrAbilit;
037400140113             $Fine = *on;
037500140113
037600140113           // -?F3=Fine?
037700140113           WHEN  dsp_aid = c_F03;
037800140113             $Fine = *on;
037900140115
038000140115           // -?F9=Visualizzazione file WFPDL00F?
038100140115           WHEN  dsp_aid = c_F09;
038200140115             Qcmd = 'DSPDBF file(*libl/WFPDL00F) mbr(*first) output(*)';
038300140115             exsr  sr_ExecCmd;
038400140114
038500140115           //// -?F9=Personalizzazione Batch?
038600140115           //WHEN  dsp_aid = c_F09;
038700140115           //  Kcoaz = 'LSE4';
038800140115           //  Bch09 ( kpjba );
038900140113
039000140114           // -?Invio / F6=Conferma?
039100140113           OTHER;
039200140113             exsr  sr_CtrD01;
039300140113             if  ErrGenerico = *on;
039400140113               leavesr;
039500140113             endif;
039600140114             if  dsp_aid = c_F06;
039700140114               exsr  sr_SbmJob;
039800140117               $Fine = *on;
039900140114             endif;
040000140113
040100140113         ENDSL;
040200140113
040300140113       ENDSR;
040400140113
040500140113       //--------------------------------------------------------------
040600140113       //?Inizializzazione videata D01.                                ?
040700140113       //--------------------------------------------------------------
040800140113       BEGSR  sr_InzD01;
040900140113
041000140115         if  Not $ErrAbilit;
041100140115           clear  SS08D01;
041200140115         endif;
041300140114
041400140114         //V1Cfil = DUTpou;
041500140114         //V1Dfil = DUTdpo;
041600140121
041700140121         // -?Impostazione Output di default?
041800140121         if  DUTlpo = 'S';
041900151022           V1CoutS = 'T';
042000151022           V1CoutF = 'S';
042100151022         else;
042200151022           V1CoutS = 'D';
042300140121         endif;
042400140113
042500140113         // -?Impostazione Attributi di Visualizzazione?
042600140115         VisualCAR  = (sk_Car(2) > *zero);
042700140115         VisualFIL  = VisualCAR;
042800140115
042900140115         // -?(Dis)Abilitazione tasti funzionali?
043000140115         F9Attivo = (DUTlpo = 'S');
043100140115
043200140115         if  $ErrAbilit;
043300140115           leavesr;
043400140115         endif;
043500140113
043600140114         // -?Decodifica dati?
043700140113         exsr  sr_CtrD01;
043800140113         %subst(IndDspF : 50) = *off;
043900140113         reset  ErrGenerico;
044000140113         reset  ErrMessage;
044100140113         clear  V1Dmsg;
044200140115
044300140115         clear  SavV1dsi;
044400140115         clear  SavV1dsf;
044500140113
044600140113       ENDSR;
044700140113
044800140113       //--------------------------------------------------------------
044900140113       //?Controllo dati nella videata D01.                            ?
045000140113       //--------------------------------------------------------------
045100140113       BEGSR  sr_CtrD01;
045200140113
045300140113         // -?Spegnimento indicatori di posizionamento cursore?
045400140113         %subst(IndDspF : 50) = *off;
045500140113
045600140114         // -?Date spedizioni?
045700140115         if  V1Cdsi = *zero  and  V1Cdsf = *zero;
045800140114           PosCurDSI   = *on;
045900140114           ErrMessage  = *on;
046000140114           ErrGenerico = *on;
046100140114           V1Dmsg = sk_Msg(02);
046200140114           leavesr;
046300140114         endif;
046400140114
046500140114         // -?Data spedizione iniziale?
046600140113         clear  WLBdat;
046700140114         if  V1Cdsi = *zero;
046800140114           PosCurDSI   = *on;
046900140114           ErrMessage  = *on;
047000140114           ErrGenerico = *on;
047100140114           V1Dmsg = sk_Msg(03);
047200140114           leavesr;
047300140115         endif;
047400140115         G08dat = V1Cdsi;
047500140115         xsrda8 ( WLBdat );
047600140115         if  G08err =  *on;
047700140115           PosCurDSI   = *on;
047800140115           ErrMessage  = *on;
047900140115           ErrGenerico = *on;
048000140115           V1Dmsg = sk_Msg(04);
048100140115           leavesr;
048200140115         endif;
048300140115         V1Cdsi  = G08dat;
048400140115         SS09dsi = G08inv;
048500140114
048600140114         // -?Data spedizione finale?
048700140114         clear  WLBdat;
048800140114         if  V1Cdsf = *zero;
048900140114           PosCurDSF   = *on;
049000140114           ErrMessage  = *on;
049100140114           ErrGenerico = *on;
049200140114           V1Dmsg = sk_Msg(03);
049300140114           leavesr;
049400140115         endif;
049500140115         G08dat = V1Cdsf;
049600140115         xsrda8 ( WLBdat );
049700140115         if  G08err =  *on;
049800140115           PosCurDSF   = *on;
049900140115           ErrMessage  = *on;
050000140115           ErrGenerico = *on;
050100140115           V1Dmsg = sk_Msg(04);
050200140115           leavesr;
050300140115         endif;
050400140115         V1Cdsf  = G08dat;
050500140115         SS09dsf = G08inv;
050600140114
050700140114         // -?Range date spedizioni?
050800140115         if  SS09dsi > SS09dsf;
050900140114           PosCurDSI   = *on;
051000140114           ErrMessage  = *on;
051100140114           ErrGenerico = *on;
051200140114           V1Dmsg = sk_Msg(05);
051300140114           leavesr;
051400140114         endif;
051500140114
051600140114         // -?Corrispondenza date iniziale in TIPDL00F (pdlDTI)?
051700140115         if  V1Cdsi <> SavV1dsi;
051800140115           exsr  sr_CtrlDSI;
051900140115           if  Not ErrGenerico;
052000140115             SavV1dsi =  V1Cdsi;
052100140115           endif;
052200140115         endif;
052300140114         // -?Corrispondenza data finale in TIPDL00F (pdlDTF)?
052400140115         if  V1Cdsf <> SavV1dsf;
052500140115           exsr  sr_CtrlDSF;
052600140115           if  Not ErrGenerico;
052700140115             SavV1dsf =  V1Cdsf;
052800140115           endif;
052900140115         endif;
053000140115
053100140115         if  VisualDSU;
053200140115           if  V1Cdsi2 = *zero;
053300140115             V1Cdsi2  = V1Cdsi;
053400140115           endif;
053500140115           if  V1Cdsf2 = *zero;
053600140115             V1Cdsf2  = V1Cdsf;
053700140115           endif;
053800140115         endif;
053900140115
054000140115         if  ErrGenerico = *on;
054100140115           leavesr;
054200140115         endif;
054300140115
054400140115         // -?Controllo COMMERCIALE UNIFICANTE (facoltativo)?
054500140115         clear  V1Dcmu;
054600140115         Select;
054700140115
054800140115           // -?Non inserito?
054900140115           When  V1Ccmu = *blank  or  V1Ccmu = *zero;
055000140115
055100140115           // -?Ricerca?
055200140115           When  %scan( '?' : V1Ccmu ) > *zero;
055300140115             clear  TRMK43ds;
055400140115             iMK43solU = 'S';
055500140115             iMK43fil  = DUTpou;
055600140115             TRMK43R (kpjba : TRMK43ds);
055700140115             if  oMK43err = *off  and  oMK43fxx = *blank;
055800140115               V1Ccmu = %editc( oMK43cmm : 'X' );
055900140115               V1Dcmu = oMK43des;
056000140115             endif;
056100140115             PosCurCMU   = *on;
056200140115             ErrGenerico = *on;
056300140115             leavesr;
056400140115
056500140115           // -?Controllo / Decodifica:?
056600140115           Other;
056700140115             // ·?Numericità?
056800140115             If  %check( c_Digits : V1Ccmu ) > *zero;
056900140115               PosCurCMU   = *on;
057000140115               ErrMessage  = *on;
057100140115               ErrGenerico = *on;
057200140115               V1Dmsg = sk_Msg(10);
057300140115               leavesr;
057400140115             EndIf;
057500140115             // ·?Esistenza in AZCMM00F?
057600140115             k_CMMcod = %int(V1Ccmu);
057700140115             chain  %kds(keyAZCMM01)  AZCMM000;
057800140115             If  NOT %found(AZCMM01L);
057900140115               PosCurCMU   = *on;
058000140115               ErrMessage  = *on;
058100140115               ErrGenerico = *on;
058200140115               V1Dmsg = sk_Msg(10);
058300140115               leavesr;
058400140115             EndIf;
058500140115             V1Dcmu = CMMdes;
058600140115             // ·?...senza particolarità?
058700140115             if  CMMpar <> *blank;
058800140115               PosCurCMU   = *on;
058900140115               ErrMessage  = *on;
059000140115               ErrGenerico = *on;
059100140115               V1Dmsg = sk_Msg(10);
059200140115               leavesr;
059300140115             endif;
059400140115             // ·?...attivo in quel periodo?
059500140115             if  CMMdtIni  > SS09dsf  or  CMMdtFin < SS09dsi;
059600140115               PosCurCMU   = *on;
059700140115               ErrMessage  = *on;
059800140115               ErrGenerico = *on;
059900140115               V1Dmsg = sk_Msg(10);
060000140115               leavesr;
060100140115             endif;
060200140115             // ·?Abilitazione dell'utente al commerciale?
060300140115             reset  TNTAA1ds;
060400140115             //ITAA1caut = '§utegtc';   ?(già così)?
060500140115             ITAA1cmm  = %int( V1Ccmu );
060600140115             kpjbu     = TNTAA1ds;
060700140115             tntaa1c (kpjba);
060800140115             TNTAA1ds = kpjbu;
060900140115             if  oTAA1fabi = 'N';
061000140115               PosCurCMU   = *on;
061100140115               ErrMessage  = *on;
061200140115               ErrGenerico = *on;
061300140115               V1Dmsg = sk_Msg(10);
061400140115               leavesr;
061500140115             endif;
061600140115             // ·?Selezionabili SOLO unificanti?
061700140115             if  %int( V1Ccmu ) <> CMMuni;
061800140115               PosCurCMU   = *on;
061900140115               ErrMessage  = *on;
062000140115               ErrGenerico = *on;
062100140115               V1Dmsg = sk_Msg(11);
062200140115               leavesr;
062300140115             endif;
062400140115
062500140115         EndSl;
062600140113
062700140115         // -?Controllo FILIALE del comm.le (facoltativa)?
062800140114         clear  V1Dfil;
062900140114         If  V1Cfil <> *zero;
063000140115
063100140115           // -?Non immettere la filiale se richiesto il comm.le?
063200140115           if  V1Ccmu <> *blank  and  V1Ccmu <> *zero;
063300140115             PosCurFIL   = *on;
063400140115             ErrMessage  = *on;
063500140115             ErrGenerico = *on;
063600140115             V1Dmsg = sk_Msg(12);
063700140115             leavesr;
063800140115           endif;
063900140115
064000140114           k_ORGfil = V1Cfil;
064100140114           chain  %kds( keyAZORG01 )  AZORG;
064200140115
064300140114           if  NOT %found(AZORG01L)  or  ORGfva <> *blank;
064400140115             V1Dfil = *all'? ';
064500140114             PosCurFIL   = *on;
064600140114             ErrMessage  = *on;
064700140114             ErrGenerico = *on;
064800140115             V1Dmsg = sk_Msg(13);
064900140114             leavesr;
065000140114           endif;
065100140115
065200140114           V1Dfil = ORGdes;
065300140115
065400140114           if  %lookup( %editc( V1Cfil : 'X' ) : sk_POg ) = *zero;
065500140114             PosCurFIL   = *on;
065600140114             ErrMessage  = *on;
065700140114             ErrGenerico = *on;
065800140115             V1Dmsg = sk_Msg(13);
065900140114             leavesr;
066000140114           endif;
066100140115
066200140114         EndIf;
066300140114
066400140115         // -?Controllo AREA del comm.le (facoltativa)?
066500140114         clear  V1Dcar;
066600140114         Select;
066700140115
066800140117           // -?Non immettere l'area se richiesto il comm.le?
066900140115           When  V1Ccar <> *blank  and  V1Ccar <> *zero  and
067000140115                 V1Ccmu <> *blank  and  V1Ccmu <> *zero;
067100140115             PosCurCAR   = *on;
067200140115             ErrMessage  = *on;
067300140115             ErrGenerico = *on;
067400140115             V1Dmsg = sk_Msg(14);
067500140115             leavesr;
067600140117
067700140117           // -?Non immettere l'area se richiesta la filiale?
067800140117           When  V1Ccar <> *blank  and  V1Ccar <> *zero  and
067900140117                 V1Cfil <> *zero;
068000140117             PosCurCAR   = *on;
068100140117             ErrMessage  = *on;
068200140117             ErrGenerico = *on;
068300140117             V1Dmsg = sk_Msg(15);
068400140117             leavesr;
068500140115
068600140114           // -?Non inserita?
068700140114           When  V1Ccar = *blank  or  V1Ccar = *zero;
068800140115
068900140114           // -?Ricerca?
069000140114           When  %scan( '?' : V1Ccar ) > *zero;
069100140114             clear  V1Ccar;
069200140114             X§Tkut = c_Kut;
069300140114             X§Tcod = '05';
069400140114             clear  X§Tkey;
069500140114             clear  X§Tdes;
069600140114             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
069700140114             V1Ccar = %subst( X§Tkey : 1 : %len(V1Ccar) );
069800140114             V1Dcar = X§Tdes;
069900140114             PosCurCAR   = *on;
070000140114             ErrGenerico = *on;
070100140114             leavesr;
070200140115
070300140114           // -?Controllo / Decodifica:?
070400140114           Other;
070500140114             // ·?Numericità?
070600140114             If  %check( c_Digits : V1Ccar ) > *zero;
070700140114               PosCurCAR   = *on;
070800140114               ErrMessage  = *on;
070900140114               ErrGenerico = *on;
071000140117               V1Dmsg = sk_Msg(16);
071100140114               leavesr;
071200140114             EndIf;
071300140114             // ·?Esistenza in tab. "05"?
071400140114             ds_TNTBE.TBEke1 = V1Ccar;
071500140115             If  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
071600140114                              *omit : *omit : *omit :
071700140114                              *omit : *omit :
071800140114                              *omit : *omit : *omit : *omit :
071900140114                              *omit : *omit : *omit : *omit :
072000140114                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
072100140114                            = *zero      AND
072200140114                 ds_TNTBE.TBEatb = *blank;
072300140114               ds05   = ds_TNTBE.TBEuni;
072400140115               V1Dcar = ds05.§05des;
072500140115             Else;
072600140114               V1Dcar = *all'? ';
072700140114               PosCurCAR   = *on;
072800140114               ErrMessage  = *on;
072900140114               ErrGenerico = *on;
073000140117               V1Dmsg = sk_Msg(16);
073100140114               leavesr;
073200140115             EndIf;
073300140115             // -?Abilitazione dell'utente all'area?
073400140115             If  %lookup( V1Ccar : sk_Car) = *zero;
073500140115               PosCurCAR   = *on;
073600140115               ErrMessage  = *on;
073700140115               ErrGenerico = *on;
073800140117               V1Dmsg = sk_Msg(16);
073900140115               leavesr;
074000140115             EndIf;
074100140115
074200140114         EndSl;
074300151022
074400151022         // -?Controllo Output selezionato?
074500151022         If  V1CoutS = *blank  and
074600151022             V1CoutF = *blank;
074700151022           PosCuroutS  = *on;
074800151022           ErrMessage  = *on;
074900151022           ErrGenerico = *on;
075000151022           V1Dmsg = sk_Msg(17);
075100151022           leavesr;
075200151022         EndIf;
075300151022
075400140113
075500140113       ENDSR;
075600140114
075700140114       //--------------------------------------------------------------
075800140114       //?Corrispondenza data iniziale in TIPDL00F                     ?
075900140114       //?(deve essere il giorno iniziale di un periodo estratto)      ?
076000140114       //--------------------------------------------------------------
076100140114       BEGSR  sr_CtrlDSI;
076200140114
076300140114         // -?Preparazione stringa SQL?
076400140114         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
076500140114                   from ';
076600140115         if  %subst( KNSIF : 7 : 1 ) = 'P';
076700140114           wSQL += 'GAITRAAZP';
076800140114         else;
076900140114           wSQL += 'GAITRAAZM';
077000140114         endif;
077100140114         wSQL += '/TIPDL00F +
077200140114                  where PDLdti <= ' + %editc( SS09dsi : 'X' ) + ' +
077300140114                    and PDLdtf >= ' + %editc( SS09dsi : 'X' ) + ' +
077400140114                    for fetch only';
077500140114
077600140114         // -?Dichiarazione cursore?
077700140114         exec sql   prepare S1   from :wSQL;
077800140114         exec sql   declare C1   cursor for S1;
077900140114
078000140114         if  SQLcode < *zero;
078100140114           PosCurDSI   = *on;
078200140114           ErrMessage  = *on;
078300140114           ErrGenerico = *on;
078400140114           V1Dmsg = sk_Msg(06);
078500140114           leavesr;
078600140114         endif;
078700140114
078800140114         // -?Apertura cursore?
078900140114         exec sql   open C1;
079000140114
079100140114         if  SQLcode < *zero;
079200140114           PosCurDSI   = *on;
079300140114           ErrMessage  = *on;
079400140114           ErrGenerico = *on;
079500140114           V1Dmsg = sk_Msg(06);
079600140114           leavesr;
079700140114         endif;
079800140114
079900140114         // -?Lettura cursore?
080000140114         clear  wPDLdti;
080100140114         clear  wPDLdtf;
080200140114         exec sql   fetch next   from C1   into :wPDLdti, :wPDLdtf;
080300140114
080400140114         select;
080500140114           // -?Errore in elaborazione: si chiude il pgm.?
080600140114           when  SQLcode < *zero;
080700140114             PosCurDSI   = *on;
080800140114             ErrMessage  = *on;
080900140114             ErrGenerico = *on;
081000140114             V1Dmsg = sk_Msg(06);
081100140114           // -?File vuoto: nessun dato estratto?
081200140114           when  SQLcode = 100  or  wPDLdti = *zero;
081300140122             //PosCurDSI   = *on;
081400140122             //ErrMessage  = *on;
081500140122             //ErrGenerico = *on;
081600140122             //V1Dmsg = sk_Msg(07);
081700140122             exsr  sr_DSIanom;
081800140114           // -?La data iniziale sarà quella iniziale della settimana?
081900140115           when wPDLdti <> SS09dsi;
082000140115             VisualDSU   = *on;
082100140115             V1Cdsi2     = V1Cdsi;
082200140115             SS09dsi     = wPDLdti;
082300140115             reset  WLBdat;
082400140115             G08inv = wPDLdti;
082500140115             xsrda8 ( WLBdat );
082600140115             V1Cdsi = G08dat;
082700140114         endsl;
082800140114
082900140114         // -?Chiusura cursore?
083000140114         exec sql   close C1;
083100140114
083200140114       ENDSR;
083300140122
083400140122       //--------------------------------------------------------------
083500140122       //?Non trovata "data inizio periodo":                           ?
083600140122       //?ricerca data iniziale del periodo successivo.                ?
083700140122       //--------------------------------------------------------------
083800140122       BEGSR  sr_DSIanom;
083900140122
084000140122         // -?Preparazione stringa SQL?
084100140122         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
084200140122                   from ';
084300140122         if  %subst( KNSIF : 7 : 1 ) = 'P';
084400140122           wSQL += 'GAITRAAZP';
084500140122         else;
084600140122           wSQL += 'GAITRAAZM';
084700140122         endif;
084800140122         wSQL += '/TIPDL00F +
084900140122                  where PDLdti >= ' + %editc( SS09dsi : 'X' ) + ' +
085000140122                    and PDLdtf >= ' + %editc( SS09dsi : 'X' ) + ' +
085100140122                  order by PDLdti asc +
085200140122                    for fetch only';
085300140122
085400140122         // -?Dichiarazione cursore?
085500140122         exec sql   prepare S1B   from :wSQL;
085600140122         exec sql   declare C1B   cursor for S1B;
085700140122
085800140122         if  SQLcode < *zero;
085900140122           PosCurDSI   = *on;
086000140122           ErrMessage  = *on;
086100140122           ErrGenerico = *on;
086200140122           V1Dmsg = sk_Msg(06);
086300140122           leavesr;
086400140122         endif;
086500140122
086600140122         // -?Apertura cursore?
086700140122         exec sql   open C1B;
086800140122
086900140122         if  SQLcode < *zero;
087000140122           PosCurDSI   = *on;
087100140122           ErrMessage  = *on;
087200140122           ErrGenerico = *on;
087300140122           V1Dmsg = sk_Msg(06);
087400140122           leavesr;
087500140122         endif;
087600140122
087700140122         // -?Lettura cursore?
087800140122         clear  wPDLdti;
087900140122         clear  wPDLdtf;
088000140122         exec sql   fetch next   from C1B   into :wPDLdti, :wPDLdtf;
088100140122
088200140122         select;
088300140122           // -?Errore in elaborazione: si chiude il pgm.?
088400140122           when  SQLcode < *zero;
088500140122             PosCurDSI   = *on;
088600140122             ErrMessage  = *on;
088700140122             ErrGenerico = *on;
088800140122             V1Dmsg = sk_Msg(06);
088900140122           // -?File vuoto: nessun dato estratto?
089000140122           when  SQLcode = 100  or  wPDLdti = *zero;
089100140122             PosCurDSI   = *on;
089200140122             ErrMessage  = *on;
089300140122             ErrGenerico = *on;
089400140122             V1Dmsg = sk_Msg(07);
089500140122           // -?La data iniziale sarà quella iniziale della settimana?
089600140122           when wPDLdti <> SS09dsi;
089700140122             VisualDSU   = *on;
089800140122             V1Cdsi2     = V1Cdsi;
089900140122             SS09dsi     = wPDLdti;
090000140122             reset  WLBdat;
090100140122             G08inv = wPDLdti;
090200140122             xsrda8 ( WLBdat );
090300140122             V1Cdsi = G08dat;
090400140122         endsl;
090500140122
090600140122         // -?Chiusura cursore?
090700140122         exec sql   close C1B;
090800140122
090900140122       ENDSR;
091000140114
091100140114       //--------------------------------------------------------------
091200140114       //?Corrispondenza data finale in TIPDL00F                       ?
091300140114       //?(deve essere il giorno finale di un periodo estratto)        ?
091400140114       //--------------------------------------------------------------
091500140114       BEGSR  sr_CtrlDSF;
091600140114
091700140114         // -?Preparazione stringa SQL?
091800140114         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
091900140114                   from ';
092000140115         if  %subst( KNSIF : 7 : 1 ) = 'P';
092100140114           wSQL += 'GAITRAAZP';
092200140114         else;
092300140114           wSQL += 'GAITRAAZM';
092400140114         endif;
092500140114         wSQL += '/TIPDL00F +
092600140114                  where PDLdti <= ' + %editc( SS09dsf : 'X' ) + ' +
092700140114                    and PDLdtf >= ' + %editc( SS09dsf : 'X' ) + ' +
092800140114                    for fetch  only';
092900140114
093000140114         // -?Dichiarazione cursore?
093100140114         exec sql   prepare S2   from :wSQL;
093200140114         exec sql   declare C2   cursor for S2;
093300140114
093400140114         if  SQLcode < *zero;
093500140114           PosCurDSI   = *on;
093600140114           ErrMessage  = *on;
093700140114           ErrGenerico = *on;
093800140114           V1Dmsg = sk_Msg(06);
093900140114           leavesr;
094000140114         endif;
094100140114
094200140114         // -?Apertura cursore?
094300140114         exec sql   open C2;
094400140114
094500140114         if  SQLcode < *zero;
094600140114           PosCurDSI   = *on;
094700140114           ErrMessage  = *on;
094800140114           ErrGenerico = *on;
094900140114           V1Dmsg = sk_Msg(06);
095000140114           leavesr;
095100140114         endif;
095200140114
095300140114         // -?Lettura cursore?
095400140114         clear  wPDLdti;
095500140114         clear  wPDLdtf;
095600140114         exec sql   fetch next   from C2   into :wPDLdti, :wPDLdtf;
095700140114
095800140114         select;
095900140114           // -?Errore in elaborazione: si chiude il pgm.?
096000140114           when  SQLcode < *zero;
096100140114             PosCurDSF   = *on;
096200140114             ErrMessage  = *on;
096300140114             ErrGenerico = *on;
096400140114             V1Dmsg = sk_Msg(06);
096500140114           // -?File vuoto: nessun dato estratto?
096600140114           when  SQLcode = 100  or  wPDLdtf = *zero;
096700140122             //PosCurDSF   = *on;
096800140122             //ErrMessage  = *on;
096900140122             //ErrGenerico = *on;
097000140122             //V1Dmsg = sk_Msg(07);
097100140122             exsr  sr_DSFanom;
097200140115           // -?La data finale sarà quella finale della settimana?
097300140115           when wPDLdtf <> SS09dsf;
097400140115             VisualDSU   = *on;
097500140115             V1Cdsf2     = V1Cdsf;
097600140115             SS09dsf     = wPDLdtf;
097700140115             reset  WLBdat;
097800140115             G08inv = wPDLdtf;
097900140115             xsrda8 ( WLBdat );
098000140115             V1Cdsf = G08dat;
098100140114         endsl;
098200140114
098300140114         // -?Chiusura cursore?
098400140114         exec sql   close C2;
098500140114
098600140114       ENDSR;
098700140122
098800140122       //--------------------------------------------------------------
098900140122       //?Non trovata "data fine periodo":                             ?
099000140122       //?ricerca data finale del periodo precedente.                  ?
099100140122       //--------------------------------------------------------------
099200140122       BEGSR  sr_DSFanom;
099300140122
099400140122         // -?Preparazione stringa SQL?
099500140122         wSQL = 'select coalesce( PDLdti, 0 ), coalesce( PDLdtf, 0 ) +
099600140122                   from ';
099700140122         if  %subst( KNSIF : 7 : 1 ) = 'P';
099800140122           wSQL += 'GAITRAAZP';
099900140122         else;
100000140122           wSQL += 'GAITRAAZM';
100100140122         endif;
100200140122         wSQL += '/TIPDL00F +
100300140122                  where PDLdti <= ' + %editc( SS09dsf : 'X' ) + ' +
100400140122                    and PDLdtf <= ' + %editc( SS09dsf : 'X' ) + ' +
100500140122                  order by PDLdtf desc +
100600140122                    for fetch only';
100700140122
100800140122         // -?Dichiarazione cursore?
100900140122         exec sql   prepare S2B   from :wSQL;
101000140122         exec sql   declare C2B   cursor for S2B;
101100140122
101200140122         if  SQLcode < *zero;
101300140122           PosCurDSI   = *on;
101400140122           ErrMessage  = *on;
101500140122           ErrGenerico = *on;
101600140122           V1Dmsg = sk_Msg(06);
101700140122           leavesr;
101800140122         endif;
101900140122
102000140122         // -?Apertura cursore?
102100140122         exec sql   open C2B;
102200140122
102300140122         if  SQLcode < *zero;
102400140122           PosCurDSI   = *on;
102500140122           ErrMessage  = *on;
102600140122           ErrGenerico = *on;
102700140122           V1Dmsg = sk_Msg(06);
102800140122           leavesr;
102900140122         endif;
103000140122
103100140122         // -?Lettura cursore?
103200140122         clear  wPDLdti;
103300140122         clear  wPDLdtf;
103400140122         exec sql   fetch next   from C2B   into :wPDLdti, :wPDLdtf;
103500140122
103600140122         select;
103700140122           // -?Errore in elaborazione: si chiude il pgm.?
103800140122           when  SQLcode < *zero;
103900140122             PosCurDSI   = *on;
104000140122             ErrMessage  = *on;
104100140122             ErrGenerico = *on;
104200140122             V1Dmsg = sk_Msg(06);
104300140122           // -?File vuoto: nessun dato estratto?
104400140122           when  SQLcode = 100  or  wPDLdti = *zero;
104500140122             PosCurDSI   = *on;
104600140122             ErrMessage  = *on;
104700140122             ErrGenerico = *on;
104800140122             V1Dmsg = sk_Msg(07);
104900140122           // -?La data finale sarà quella finale della settimana?
105000140122           when wPDLdtf <> SS09dsf;
105100140122             VisualDSU   = *on;
105200140122             V1Cdsf2     = V1Cdsf;
105300140122             SS09dsf     = wPDLdtf;
105400140122             reset  WLBdat;
105500140122             G08inv = wPDLdtf;
105600140122             xsrda8 ( WLBdat );
105700140122             V1Cdsf = G08dat;
105800140122         endsl;
105900140122
106000140122         // -?Chiusura cursore?
106100140122         exec sql   close C2B;
106200140122
106300140122       ENDSR;
106400140114
106500140114       //--------------------------------------------------------------
106600140114       //?Sottomissione lavoro batch                                   ?
106700140114       //--------------------------------------------------------------
106800140114       BEGSR  sr_SbmJob;
106900140114
107000140114         // -?Impostazione dei parametri restanti?
107100140115         clear  SS09cmu;
107200140115         if  V1Ccmu > *zero;
107300140115           SS09cmu = %int( V1Ccmu );
107400140115         endif;
107500151022         SS09fil  = V1Cfil;
107600140115         clear  SS09car;
107700140115         if  V1Ccar > *zero;
107800151022           SS09car  = %int( V1Ccar );
107900140115         endif;
108000151022         SS09outS = V1CoutS;
108100151022         SS09outF = V1CoutF;
108200140114
108300140114         // -?Lancio?
108400140115         kpjbu = TNSS09ds;
108500140115         kcoaz = 'SS09';
108600140114         if  KNMUS = *all'1';
108700140114           tnss09r ( kpjba );
108800140114         else;
108900140114           bch10 ( kpjba );
109000140114         endif;
109100140114
109200140114       ENDSR;
109300140115
109400140115       //--------------------------------------------------------------
109500140115       //?Esecuzione del comando (già impostato).                      ?
109600140115       //--------------------------------------------------------------
109700140115       BEGSR  sr_ExecCmd;
109800140115
109900140115         clear Qcap0100;
110000140115         Qcabcsdh = *off;
110100140115         Qcapa    = *off;
110200140115         Qcacmdss = *off;
110300140115         Qcaerved = *allX'00';
110400140115
110500140115         clear Qusec;
110600140115         Qusbprv  = %size(Qusec);
110700140115
110800140115         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
110900140115                           %size(Qcap0100) : 'CPOP0100' : *omit :
111000140115                           0 : 0 : Qusec);
111100140115
111200140115         //if  Qusei <> *blank;
111300140115         //  ...;
111400140115         //endif;
111500140115
111600140115       ENDSR;
111700140114
111800140114       //--------------------------------------------------------------
111900140114       //?Operazioni finali.                                           ?
112000140114       //--------------------------------------------------------------
112100140114       BEGSR  sr_RoutEnd;
112200140114
112300140114         cloTabella();
112400140114
112500140114         return;
112600140114
112700140114       ENDSR;
112800140113
112900140113      /end-free
113000140113
113100140113       //--------------------------------------------------------------
113200140113       //?Schiere a tempo di compilazione.
113300140113       //--------------------------------------------------------------
113400060331
113500140113** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
113600140114Utente non abilitato alla funzione                                             1
113700140114Impostare il periodo da elaborare                                              2
113800140114Data obbligatoria                                                              3
113900140114Data formalmente errata                                                        4
114000140114Data iniziale incongruente con data finale                                     5
114100140122ERRORE nel reperimento delle date elaborabili: avvisare CED                    6
114200140122Nessuna packing-list risulta elaborabile per questa data                       7
114300151022Impostata la data "DAL" con quella iniziale della settimana a cui si riferisce 8 LIBERO
114400151022Impostata la data "AL" con quella finale della settimana a cui si riferisce    9 LIBERO
114500140115Codice commerciale errato  o  non in gestione all'utente                      10
114600140115Richiedere solo commerciali UNIFICANTI                                        11
114700140117Non impostare il codice filiale SE inserito il commerciale                    12
114800140117Filiale errata                                                                13
114900140117Non impostare il codice area SE inserito il commerciale                       14
115000140117Non impostare il codice area SE inserita la filiale                           15
115100140117Codice Area errato  o  non in gestione all'utente                             16
115200151022Selezionare almeno un Output                                                  17
