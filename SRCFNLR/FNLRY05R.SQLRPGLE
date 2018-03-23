000100150218       //==============================================================
000200170426       //?FNLRY05R * Visual. Statistica disposizioni di consegna da web?
000300150218       //==============================================================
000400150218
000500150218       //--------------------------------------------------------------
000600150218       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150218       //--------------------------------------------------------------
000800150218
000900150218     /*PRM  dbgview(*source)
001000150218     /*END
001100150218
001200150218       //--------------------------------------------------------------
001300150218       //?Specifiche di controllo.                                     ?
001400150218       //--------------------------------------------------------------
001500150218
001600150218     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150218     h dftactgrp(*no)
001800150416     h bnddir('UBRTVNETA')
001900150218     h alwnull(*inputonly)
002000150218
002100150218       //--------------------------------------------------------------
002200150218       //?Dichiarazione file.                                          ?
002300150218       //--------------------------------------------------------------
002400150327
002500150327       // -?Organigramma?
002600150327     fAZORG01L  if   e           k disk
002700150218
002800150327       // -?Video?
002900170426     fFNLRY05D  cf   e             workstn
003000150327     f                                     indds(IndDspF)
003100150327     f                                     infds(InfDspF)
003200150218
003300150218       //--------------------------------------------------------------
003400150218       //?Definizione costanti.                                        ?
003500150218       //--------------------------------------------------------------
003600150416
003700150416       // -?Tipi Disposizione?
003800150416       //  ?(1=Appunt., 2=F.Dep., 3=CambioInd.)?
003900150416     d c_TipoDis1      c                   const('1')
004000150416     d c_TipoDis2      c                   const('2')
004100150416     d c_TipoDis3      c                   const('3')
004200150416
004300150416       // -?Numero di record in ciascuna pagina di subfile?
004400150416     d c_SflPag        c                   const(0016)
004500150327
004600150327       // -?Numero massimo di record gestibili nel subfile?
004700150327     d c_MaxRec        c                   const(9999)
004800150218
004900150327       // -?Tasti funzionali a video?
005000150327     d c_F01           c                   const(x'31')
005100150327     d c_F02           c                   const(x'32')
005200150327     d c_F03           c                   const(x'33')
005300150327     d c_F04           c                   const(x'34')
005400150327     d c_F05           c                   const(x'35')
005500150327     d c_F06           c                   const(x'36')
005600150327     d c_F07           c                   const(x'37')
005700150327     d c_F08           c                   const(x'38')
005800150327     d c_F09           c                   const(x'39')
005900150327     d c_F10           c                   const(x'3A')
006000150327     d c_F11           c                   const(x'3B')
006100150327     d c_F12           c                   const(x'3C')
006200150327     d c_F13           c                   const(x'B1')
006300150327     d c_F14           c                   const(x'B2')
006400150327     d c_F15           c                   const(x'B3')
006500150327     d c_F16           c                   const(x'B4')
006600150327     d c_F17           c                   const(x'B5')
006700150327     d c_F18           c                   const(x'B6')
006800150327     d c_F19           c                   const(x'B7')
006900150327     d c_F20           c                   const(x'B8')
007000150327     d c_F21           c                   const(x'B9')
007100150327     d c_F22           c                   const(x'BA')
007200150327     d c_F23           c                   const(x'BB')
007300150327     d c_F24           c                   const(x'BC')
007400150327     d c_Enter         c                   const(x'F1')
007500150327     d c_RollDown      c                   const(x'F4')
007600150327     d c_RollUp        c                   const(x'F5')
007700150218
007800150218       //--------------------------------------------------------------
007900150218       //?Definizione schiere.                                         ?
008000150218       //--------------------------------------------------------------
008100150417
008200170426      *//· // -?Tipi Disposizione?
008300170426     d*//· sk_TpDsp        s              1    dim( 3)  ctdata  perrcd(1)
008400170426     d*//· sk_DesTpDsp     s             18    dim( 3)  alt(sk_TpDsp)
008500150218
008600150327       // -?Messaggi di errore?
008700180228     d sk_Msg          s             78    dim( 5)  ctdata  perrcd(1)
008800150218
008900150218       //--------------------------------------------------------------
009000150218       //?Definizione aree dati.                                       ?
009100150218       //--------------------------------------------------------------
009200150218
009300150218       // -?Dati utente?
009400150218     d §AzUte        e ds                  extname(AZUTE00F)
009500150218     d                                     dtaara
009600150218     d §DatiUte      e ds                  extname(dDatiUte)
009700150218     d                                     dtaara
009800150218
009900150218       //--------------------------------------------------------------
010000150218       //?Definizione strutture dati.                                  ?
010100150218       //--------------------------------------------------------------
010200150218
010300150218       // -?Status ds?
010400150218     d Status         sds
010500150218     d   SDSpgm          *proc
010600150218     d*//SDSprm          *parms
010700150218     d*//SDSdta              191    198
010800150218     d*//SDSjob              244    253
010900150218     d   SDSusr              254    263
011000150327
011100150327       // -?InfDS?
011200150327     d InfDspF         ds
011300150327     d   dsp_aid             369    369a
011400150327     d   sfl_rrn             376    377i 0
011500150327     d   min_nrr             378    379i 0
011600150327     d   num_rcds            380    381i 0
011700150327
011800150327       // -?Indicatori su DspF?
011900150327     d IndDspF         ds                  inz
012000150327         // -?Abilitazione tasti funzionali?
012100150416     d   F5Attivo                      n   overlay( IndDspF : 05 )
012200150416     d   F9Attivo                      n   overlay( IndDspF : 09 )
012300150327     d   F12Attivo                     n   overlay( IndDspF : 12 )
012400170421     d   F21Attivo                     n   overlay( IndDspF : 21 )
012500170413     d   $RlUpAttivo                   n   overlay( IndDspF : 25 )
012600170413     d   $RlDnAttivo                   n   overlay( IndDspF : 26 )
012700150327         // -?Emissione messaggio di errore?
012800150327     d   ErrMessage                    n   overlay( IndDspF : 28 )
012900150327         // -?Indicatori di gestione del subfile?
013000150327     d   SflDsp_N                      n   overlay( IndDspF : 30 )
013100150327     d   SflDspCtl_N                   n   overlay( IndDspF : 31 )
013200150327     d   SflNxtChg                     n   overlay( IndDspF : 32 )
013300150327     d   SflEnd                        n   overlay( IndDspF : 33 )
013400150327         // -?Indicatori per Attibuti di visualizzazione?
013500150327         //  ?o   Condizionamento visualizzazione campi?
013600150430     d   $VisualDate                   n   overlay( IndDspF : 41 )
013700150327         // -?Posizionamento cursore & segnalazione errore?
013800150416     d   PosCurOpz                     n   overlay( IndDspF : 50 )
013900150327     d   PosCurInsDI                   n   overlay( IndDspF : 51 )
014000150327     d   PosCurInsDF                   n   overlay( IndDspF : 52 )
014100180228     d   PosCurAlert                   n   overlay( IndDspF : 53 )
014200180228     d   PosCurFgs                     n   overlay( IndDspF : 54 )
014300180228     d   PosCurTpCli                   n   overlay( IndDspF : 55 )
014400180228     d*//PosCurTpDsp                   n   overlay( IndDspF : 56 )
014500150327         //   -?Riemissione videata?
014600150327     d   ErrGenerico                   n   overlay( IndDspF : 99 )
014700150424
014800170414       // -?Dati estratti via SQL: totali (di comodo)?
014900170426     d LRY05T2_ds      ds                  inz         qualified
015000150424         // ·?Data Inizio periodo?
015100150430     d   w02ddi                            inz(*hival) like(V1Tddi)
015200150424         // ·?Data Fine   periodo?
015300150430     d   w02ddf                            inz(*loval) like(V1Tddf)
015400170413         // ·?Tot. Appuntamenti?
015500170413     d   wD2tap                            inz         like(VD2tap)
015600170413         // ·?Tot. Appuntamenti x Dest.?
015700170413     d   wD2dap                            inz         like(VD2dap)
015800170413         // ·?Tot. Appuntamenti x Mitt.?
015900170413     d   wD2map                            inz         like(VD2map)
016000150424         // ·?% Tot. Appuntamenti?
016100150424     d   wD2ptap                      4  1 inz
016200150424         // ·?% Tot. Appuntamenti x Dest.?
016300150424     d   wD2pdap                      4  1 inz
016400150424         // ·?% Tot. Appuntamenti x Mitt.?
016500150424     d   wD2pmap                      4  1 inz
016600170413         // ·?Tot. Fermo Deposito?
016700170413     d   wD2tfd                            inz         like(VD2tfd)
016800170413         // ·?Tot. Fermo Deposito x Dest.?
016900170413     d   wD2dfd                            inz         like(VD2dfd)
017000170413         // ·?Tot. Fermo Deposito x Mitt.?
017100170413     d   wD2mfd                            inz         like(VD2mfd)
017200150424         // ·?% Tot. Fermo Deposito?
017300150424     d   wD2ptfd                      4  1 inz
017400150424         // ·?% Tot. Fermo Deposito x Dest.?
017500150424     d   wD2pdfd                      4  1 inz
017600150424         // ·?% Tot. Fermo Deposito x Mitt.?
017700150424     d   wD2pmfd                      4  1 inz
017800170426      *//-// ·?Tot. Cambio Indirizzo?
017900170426     d*//-wD2tci                            inz         like(VD2tci)
018000170426      *//-// ·?Tot. Cambio Indirizzo x Dest.?
018100170426     d*//-wD2dci                            inz         like(VD2dci)
018200170426      *//-// ·?Tot. Cambio Indirizzo x Mitt.?
018300170426     d*//-wD2mci                            inz         like(VD2mci)
018400170426      *//-// ·?% Tot. Cambio Indirizzo?
018500170426     d*//-wD2ptci                      4  1 inz
018600170426      *//-// ·?% Tot. Cambio Indirizzo x Dest.?
018700170426     d*//-wD2pdci                      4  1 inz
018800170426      *//-// ·?% Tot. Cambio Indirizzo x Mitt.?
018900170426     d*//-wD2pmci                      4  1 inz
019000170420         // ·?Tot. Cambio Indirizzo + Appuntamenti?
019100170421     d   wD2tciA                           inz         like(VD2tcia)
019200170420         // ·?Tot. Cambio Indirizzo + Appuntamenti x Dest.?
019300170421     d   wD2dciA                           inz         like(VD2dcia)
019400170420         // ·?Tot. Cambio Indirizzo + Appuntamenti x Mitt.?
019500170421     d   wD2mciA                           inz         like(VD2mcia)
019600170420         // ·?% Tot. Cambio Indirizzo + Appuntamenti?
019700170421     d   wD2ptciA                     4  1 inz
019800170420         // ·?% Tot. Cambio Indirizzo + Appuntamenti x Dest.?
019900170421     d   wD2pdciA                     4  1 inz
020000170420         // ·?% Tot. Cambio Indirizzo + Appuntamenti x Mitt.?
020100170421     d   wD2pmciA                     4  1 inz
020200170420         // ·?Tot. Cambio Indirizzo (No Dirottam., No Appuntam.)?
020300170420     d   wD2tci_                           inz         like(VD2tci_)
020400170420         // ·?Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Dest.?
020500170420     d   wD2dci_                           inz         like(VD2dci_)
020600170420         // ·?Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Mitt.?
020700170420     d   wD2mci_                           inz         like(VD2mci_)
020800170420         // ·?% Tot. Cambio Indirizzo (No Dirottam., No Appuntam.)?
020900170420     d   wD2ptci_                     4  1 inz
021000170420         // ·?% Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Dest.?
021100170420     d   wD2pdci_                     4  1 inz
021200170420         // ·?% Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Mitt.?
021300170420     d   wD2pmci_                     4  1 inz
021400170420         // ·?Tot. Cambio Ind. + Dirottamenti?
021500170421     d   wD2tciD                           inz         like(VD2tcid)
021600170420         // ·?Tot. Cambio Ind. + Dirottamenti x Dest.?
021700170421     d   wD2dciD                           inz         like(VD2dcid)
021800170420         // ·?Tot. Cambio Ind. + Dirottamenti x Mitt.?
021900170421     d   wD2mciD                           inz         like(VD2mcid)
022000170420         // ·?% Tot. Cambio Ind. + Dirottamenti?
022100170421     d   wD2ptciD                     4  1 inz
022200170420         // ·?% Tot. Cambio Ind. + Dirottamenti x Dest.?
022300170421     d   wD2pdciD                     4  1 inz
022400170420         // ·?% Tot. Cambio Ind. + Dirottamenti x Mitt.?
022500170421     d   wD2pmciD                     4  1 inz
022600170413         // ·?Tot. Generale?
022700170413     d   wD2tge                            inz         like(VD2tge)
022800170413         // ·?Tot. Generale x Dest.?
022900170413     d   wD2tde                            inz         like(VD2tde)
023000170413         // ·?Tot. Generale x Mitt.?
023100170413     d   wD2tmi                            inz         like(VD2tmi)
023200150424         // ·?% Tot. Generale?
023300150424     d   wD2ptge                      4  1 inz
023400150424         // ·?% Tot. Generale x Dest.?
023500150424     d   wD2ptde                      4  1 inz
023600150424         // ·?% Tot. Generale x Mitt.?
023700150424     d   wD2ptmi                      4  1 inz
023800170413
023900170413       // -?Dati estratti via SQL: totali "P" = Da Partire?
024000170426     d LRY05T2_P_ds    ds                  likeds(LRY05T2_ds)
024100170414     d                                     inz
024200170413       // -?Dati estratti via SQL: totali "A" = In Arrivo - Tentativi 0?
024300170426     d LRY05T2_A_ds    ds                  likeds( LRY05T2_ds )
024400170414     d                                     inz
024500170413       // -?Dati estratti via SQL: totali "_" = In Arrivo - Tentativi > 0?
024600170426     d LRY05T2_B_ds    ds                  likeds( LRY05T2_ds )
024700170414     d                                     inz
024800170414       // -?Dati estratti via SQL: totali generali?
024900170426     d LRY05T2_G_ds    ds                  likeds( LRY05T2_ds )
025000170414     d                                     inz
025100150218
025200150424       // -?Dati estratti via SQL: dettaglio?
025300150416     d TIIDS_ds      e ds                  extname(TIIDS00F)
025400170414     d                                     inz         qualified
025500150218
025600150218       // -?Parametri ricevuti?
025700150218     d KPJBA         e ds
025800150218
025900150218       //--------------------------------------------------------------
026000150218       //?Definizione variabili globali.                               ?
026100150218       //--------------------------------------------------------------
026200150218
026300150218       // -?Flags booleani?
026400150218     d $EoF            s               n   inz
026500150416     d $RecOK          s               n   inz
026600150416     d $Fine           s               n   inz
026700150416     d $First          s               n   inz(*on)
026800150416     d $InzD01         s               n   inz(*on)
026900150416     d $InzD02         s               n   inz(*on)
027000150417
027100150218       // -?Campi associati al video?
027200150218     d $Video          s              2    inz('D1')
027300170414     d wPage           s                   inz         like(VH2page)
027400150218
027500150218       // -?Stringa SQL da eseguire?
027600180228     d wSQL            s          32740    inz  varying
027700150416
027800150416       // -?Campi di comodo per estrazione Totali via SQL?
027900150423     d wDataIni        s                   inz  like(TIIDS_ds.IDSinsData)
028000150423     d wDataFin        s                   inz  like(TIIDS_ds.IDSinsData)
028100170414     d wStato          s                   inz  like(TIIDS_ds.IDSstato)
028200150416     d wTipoDis        s                   inz  like(TIIDS_ds.IDStipoDis)
028300170421     d wSNapp          s                   inz  like(TIIDS_ds.IDSsnApp)
028400170421     d wSNdir          s                   inz  like(TIIDS_ds.IDSsnDir)
028500150416     d wTipoCli        s                   inz  like(TIIDS_ds.IDStipoCli)
028600150416     d wTOT            s             13  0 inz
028700150416
028800150416       // -?Campi di comodo per controllo dati inseriti a video?
028900150416     d wInsDtI         s              8  0 inz
029000150416     d wInsDtF         s              8  0 inz
029100170421
029200170421       // -?Campo di comodo per calcolo percentuali?
029300170421     d wPerc           s              4  1 inz
029400150416
029500150416       // -?Campi di tipo Data?
029600150416     d wDate_EUR       s               d   inz  datfmt(*EUR)
029700150218
029800150218       //--------------------------------------------------------------
029900150218       //?Definizione prototipi procedure usate.                       ?
030000150218       //--------------------------------------------------------------
030100150218
030200150218       // -?Reperimento NETA sistema AS/400 corrente?
030300150218     d currSysNeta     s              8a   inz
030400150218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
030500150218
030600150218       // -?Reperimento dati utente?
030700150218     d TIBS34ds      e ds                  inz
030800150218      /copy gaitrasrc/srcProtoPR,TIBS34R
030900150327
031000150327       // -?Controllo/Inversione data?
031100150327     d WLBdat          ds                  inz
031200150327     d   G08dat                       8  0 inz
031300150327     d   G08inv                       8  0 inz
031400150327     d   G08err                       1    inz('3')
031500150327     d   G08tgi                       5  0 inz
031600150327      /copy gaitrasrc/srcProtoPR,XSRDA8
031700150218
031800150218       // -?API QCAPCMD (Process Commands)?
031900150218     d Qcmd            s           2048    inz  varying
032000150218      /copy qSysInc/qRpgleSrc,QCAPCMD
032100150218      /copy gaitrasrc/srcProtoPR,QCAPCMD
032200150218
032300150218       // -?Parametri gestione errori API.?
032400150218      /copy qsysinc/qrpglesrc,QUSEC
032500150218
032600150218       //--------------------------------------------------------------
032700150218       //?Definizione key-list.                                        ?
032800150218       //--------------------------------------------------------------
032900150218
033000150218
033100150218       //--------------------------------------------------------------
033200150218       //?Riepilogo indicatori utilizzati.                             ?
033300150218       //--------------------------------------------------------------
033400150218
033500150218
033600150218       //--------------------------------------------------------------
033700150218       //?M A I N - L I N E                                            ?
033800150218       //--------------------------------------------------------------
033900150218
034000150218     c     *Entry        plist
034100150218     c                   parm                    KPJBA
034200150218
034300150218      /free
034400150218
034500150218       // -?Operazioni iniziali?
034600150218       exsr  sr_RoutInz;
034700150218
034800150218       // -?Ciclo di gestione del file video?
034900150218       DoW  $Fine = *off;
035000150218
035100150218         select;
035200150218
035300150218           // -?Gestione videata D1 (parzializzazioni)?
035400150218           when  $Video = 'D1';
035500150218             exsr  sr_GesD01;
035600150417
035700150424           // -?Gestione videata D2 (totale)?
035800150417           when  $Video = 'D2';
035900150417             exsr  sr_GesD02;
036000170421           when  $Video = 'D3';
036100170421             exsr  sr_GesD02;
036200150218
036300150218           // -?? ? ??
036400150218           other;
036500150218             $Fine = *on;
036600150218
036700150218         endsl;
036800150218
036900150218       EndDo;
037000150218
037100150218       // -?Operazioni finali?
037200150218       exsr  sr_RoutEnd;
037300150218
037400150218       //--------------------------------------------------------------
037500150218       //?Operazioni iniziali.                                         ?
037600150218       //--------------------------------------------------------------
037700150218       BEGSR  sr_RoutInz;
037800150218
037900150218         // -?Impostazione opzioni per SQL?
038000150218         exec sql   set  option  DynUsrPrf = *Owner,
038100150218                                 CloSqlCsr = *EndMod;
038200150218
038300150218         // -?Impostazione chiusura?
038400150218         *inLR = *on;
038500150218
038600150218         // -?Verifica del sistema AS/400 corrente?
038700150218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
038800150218           exsr  sr_RoutEnd;
038900150218         endif;
039000150218
039100150218         // -?Reperimento dati job?
039200150218         exsr  sr_DatiJob;
039300150218
039400150218         // -?Impostazione nome programma a video?
039500150416         V1Tpgm = SDSpgm;
039600150218
039700150218       ENDSR;
039800150218
039900150218       //--------------------------------------------------------------
040000150218       //?Reperimento Dati del job (Utente/Operativi).                 ?
040100150218       //--------------------------------------------------------------
040200150218       BEGSR  sr_DatiJob;
040300150218
040400150218         in(e) §AzUte;
040500150218         if NOT %error;
040600150218           in(e) §DatiUte;
040700150218         endif;
040800150218         if %error or RSut = *blank;
040900150218           tibs34r ( tibs34ds );
041000150218           in §AzUte;
041100150218           in §DatiUte;
041200150218         endif;
041300150218
041400150218       ENDSR;
041500150218
041600150218       //--------------------------------------------------------------
041700150218       //?Gestione videata D01.                                        ?
041800150218       //--------------------------------------------------------------
041900150218       BEGSR  sr_GesD01;
042000150218
042100150218         // -?Inizializzazione videata?
042200150218         if  $InzD01 = *on;
042300150218           exsr  sr_InzD01;
042400150218           $InzD01 = *off;
042500150218         endif;
042600150416         F9Attivo  = *on;
042700150416         F12Attivo = *off;
042800170421         F21Attivo = *off;
042900150430         $VisualDate = *off;
043000150218
043100150423         // -?Emissione testata e piede?
043200170426         write  LRY05T1;
043300170426         write  LRY05P1;
043400150218
043500150218         // -?Emissione videata?
043600170426         exfmt  LRY05D1;
043700150218
043800150218         clear  errGenerico;
043900150218         clear  errMessage;
044000150218         clear  V1Dmsg;
044100150218
044200150218         Select;
044300150218
044400150218           // -?F3=Fine?
044500150218           when  dsp_aid = c_F03;
044600150218             $Fine  = *on;
044700150416
044800150416           // -?F9=Visualizzazione file TIIDS00F?
044900150416           when  dsp_aid = c_F09;
045000150429             Qcmd = 'DSPDBF file(TIIDS00F)';
045100150416             exsr  sr_ExecCmd;
045200150218
045300150218           // -?Invio?
045400150218           Other;
045500150218             exsr  sr_CtrD01;
045600150218             if  errGenerico;
045700150218               leavesr;
045800150218             endif;
045900150424             $Video  = 'D2';
046000150424             $InzD02 = *on;
046100150218
046200150218         EndSl;
046300150218
046400150218       ENDSR;
046500150218
046600150218       //--------------------------------------------------------------
046700150218       //?Inizializzazione videata D01.                                ?
046800150218       //--------------------------------------------------------------
046900150218       BEGSR  sr_InzD01;
047000150218
047100170426         clear  LRY05D1;
047200180228         V1Dfgs = 'TUTTE';
047300150218
047400150218       ENDSR;
047500150218
047600150218       //--------------------------------------------------------------
047700150218       //?Controllo dati in videata D01.                               ?
047800150218       //--------------------------------------------------------------
047900150218       BEGSR  sr_CtrD01;
048000150218
048100150218         // -?Spegnimento degli indicatori di errore?
048200150218         %subst(IndDspF : 50) = *off;
048300150327
048400150327         // -?Data inserimento richiesta DAL?
048500150327         clear  wInsDtI;
048600150327         if  V1CinsDtI > *zero;
048700150327           clear  WLBdat;
048800150327           G08dat = V1CinsDtI;
048900150327           XSRDA8 ( WLBdat );
049000150327           if  G08err = *on;
049100150327             ErrGenerico = *on;
049200150327             ErrMessage  = *on;
049300150327             PosCurInsDI = *on;
049400150327             V1Dmsg = sk_Msg(01);
049500150327             leavesr;
049600150327           else;
049700150327             V1CinsDtI = G08dat;
049800150327             wInsDtI   = G08inv;
049900150327           endif;
050000150327         endif;
050100150327
050200150327         // -?Data inserimento richiesta AL?
050300150327         clear  wInsDtF;
050400150327         if  V1CinsDtF > *zero;
050500150327           clear  WLBdat;
050600150327           G08dat = V1CinsDtF;
050700150327           XSRDA8 ( WLBdat );
050800150327           if  G08err = *on;
050900150327             ErrGenerico = *on;
051000150327             ErrMessage  = *on;
051100150327             PosCurInsDF = *on;
051200150327             V1Dmsg = sk_Msg(01);
051300150327             leavesr;
051400150327           else;
051500150416             V1CinsDtF = G08dat;
051600150327             wInsDtF   = G08inv;
051700150327           endif;
051800150327         endif;
051900150327
052000150327         // -?Periodo (Range date)?
052100150327         if  wInsDtI > wInsDtF;
052200150327           ErrGenerico = *on;
052300150327           ErrMessage  = *on;
052400150327           PosCurInsDF = *on;
052500150327           V1Dmsg = sk_Msg(02);
052600150327           leavesr;
052700150327         endif;
052800180228
052900180228         // -?Spedizioni con Alert?
053000180228         if  V1Calert <> 'S'  and  V1Calert <> 'N'  and
053100180228             V1Calert <> *blank;
053200180228           ErrGenerico = *on;
053300180228           ErrMessage  = *on;
053400180228           PosCurAlert = *on;
053500180228           V1Dmsg = sk_Msg(03);
053600180228           leavesr;
053700180228         endif;
053800150327
053900150327         // -?Filiale arrivo?
054000150327         clear  V1Dfgs;
054100150430         if  V1Cfgs = *zero;
054200150430           V1Dfgs = 'TUTTE';
054300150430         else;
054400150416           chain  ( V1Cfgs  )  AZORG;
054500150327           if  %found(AZORG01L)  and  ORGfva = *blank;
054600150327             V1Dfgs = ORGdes;
054700150327           else;
054800150327             ErrMessage  = *on;
054900150327             ErrGenerico = *on;
055000150327             PosCurFGS   = *on;
055100180228             V1Dmsg = sk_Msg(04);
055200150327             leavesr;
055300150327           endif;
055400150327         endif;
055500150327
055600150327         // -?Tipo cliente?
055700150416         if  V1CtipoCli <> *blank  and  V1CtipoCli <> 'M'
055800150424                                   and  V1CtipoCli <> 'D';
055900150327           ErrGenerico = *on;
056000150327           ErrMessage  = *on;
056100150327           PosCurTpCli = *on;
056200180228           V1Dmsg = sk_Msg(05);
056300150327           leavesr;
056400150327         endif;
056500150218
056600150218       ENDSR;
056700150424
056800150424       //--------------------------------------------------------------
056900150424       //?Gestione videata D02.                                        ?
057000150424       //--------------------------------------------------------------
057100150424       BEGSR  sr_GesD02;
057200150424
057300150424         // -?Inizializzazione videata?
057400150424         if  $InzD02 = *on;
057500150424           exsr  sr_InzD02;
057600150424           $InzD02 = *off;
057700150424         endif;
057800170414
057900150424         F9Attivo  = *on;
058000150424         F12Attivo = *on;
058100170421         F21Attivo = *on;
058200170414         $RlUpAttivo = ( wPage < 4 );
058300170414         $RlDnAttivo = ( wPage > 1 );
058400150430         $VisualDate = *on;
058500150424
058600150424         // -?Emissione testata e piede?
058700170426         write  LRY05T1;
058800170426         write  LRY05P1;
058900150424
059000150424         // -?Emissione videata?
059100170426         //*-if  $Video = 'D2';
059200170426         //*-  exfmt  LRY05D2;
059300170426         //*-else;
059400170426         //*-  exfmt  LRY05D3;
059500170426
059600170426         exfmt  LRY05D2;
059700150424
059800150424         clear  errGenerico;
059900150424         clear  errMessage;
060000150424         clear  V1Dmsg;
060100150424
060200150424         Select;
060300150424
060400150424           // -?F3=Fine?
060500150424           when  dsp_aid = c_F03;
060600150424             $Fine  = *on;
060700150424
060800150429           // -?F9=Visualizzazione file TIIDS00F?
060900150429           when  dsp_aid = c_F09;
061000150429             Qcmd = 'DSPDBF file(TIIDS00F)';
061100150429             exsr  sr_ExecCmd;
061200150424
061300150424           // -?F12=Ritorno?
061400150424           when  dsp_aid = c_F12;
061500150424             $Video = 'D1';
061600170421
061700170426           //*-// -?F21=Altra videata?
061800170426           //*-when  dsp_aid = c_F21  and  $Video = 'D2';
061900170426           //*-  $Video = 'D3';
062000170426           //*-when  dsp_aid = c_F21  and  $Video = 'D3';
062100170426           //*-  $Video = 'D2';
062200170413
062300170413           // -?RollUp?
062400170414           when  dsp_aid = c_RollUp    and  wPage < 4;
062500170414             wPage += 1;
062600170413             exsr  sr_DatiVideo;
062700170413
062800170413           // -?RollDown?
062900170414           when  dsp_aid = c_RollDown  and  wPage > 1;
063000170414             wPage -= 1;
063100170413             exsr  sr_DatiVideo;
063200150424
063300150424         EndSl;
063400150424
063500150424       ENDSR;
063600150424
063700150424       //--------------------------------------------------------------
063800150424       //?Inizializzazione videata D02.                                ?
063900150424       //--------------------------------------------------------------
064000150424       BEGSR  sr_InzD02;
064100150424
064200170426         clear  LRY05D2;
064300170426         //*-clear  LRY05D3;
064400150424
064500170426         reset  LRY05T2_ds;
064600170426         reset  LRY05T2_P_ds;
064700170426         reset  LRY05T2_A_ds;
064800170426         reset  LRY05T2_B_ds;
064900170426         reset  LRY05T2_G_ds;
065000150424
065100150424         // -?Calcolo Totali complessivi (in base alle parzializzazioni)?
065200150424         exsr  sr_CalcTot;
065300170413
065400170413         // -?Impostazione Totali e relative percentuali a video?
065500170413         //  ?(a seconda della pagina visualizzata)?
065600170414         wPage = 1;
065700170413         exsr  sr_DatiVideo;
065800150424
065900150430         // -?Impostazione ultimi dati a video:?
066000150430         // ·?Parzializzazioni?
066100150430         if  V1Cfgs <> *zero  or  V1CtipoCli <> *blank;
066200150430           VD2parz = 'PARZIALIZZAZIONI:';
066300150430           if  V1Cfgs <> *zero;
066400150430             VD2parz = %trimR( VD2parz ) +
066500150430                       ' Filiale Arrivo = ' + %editc( V1Cfgs : 'X' );
066600150430           endif;
066700150430           if  V1Cfgs <> *zero  and  V1CtipoCli <> *blank;
066800150430             VD2parz = %trimR( VD2parz) + ' /';
066900150430           endif;
067000150430           if  V1CtipoCli <> *blank;
067100150430             VD2parz = %trimR( VD2parz) +
067200150430                       ' Tipo Cliente = "' + V1CtipoCli + '"';
067300150430           endif;
067400150430         endif;
067500150424
067600150424       ENDSR;
067700170413
067800170413       //--------------------------------------------------------------
067900170413       //?Impostazione campi della singola videata D02.                ?
068000170413       //--------------------------------------------------------------
068100170413       BEGSR  sr_DatiVideo;
068200170413
068300170426         clear  LRY05D2;
068400180228
068500180228         select;
068600180228           when  V1Calert = *blank;
068700180228           when  V1Calert = 'S';
068800180228             V1Talert = ' per spedizioni  CON ALERT ';
068900180228           when  V1Calert = 'N';
069000180228             V1Talert = 'per spedizioni  SENZA ALERT';
069100180228         endsl;
069200170414
069300170414         // -?Descrizione dei totali visualizzati?
069400170414         VH2page = wPage;
069500170414         select;
069600170414           when  VH2page = 1;
069700170414             V1Ttxt = '         TOTALE          ';
069800170414           when  VH2page = 2;
069900170414             V1Ttxt = '       DA PARTIRE        ';
070000170414           when  VH2page = 3;
070100170414             V1Ttxt = ' IN ARRIVO - TENTATIVI 0 ';
070200170414           when  VH2page = 4;
070300170414             V1Ttxt = 'IN ARRIVO - TENTATIVI > 0';
070400170414         endsl;
070500170413
070600170413         // -?N° pagina a video?
070700170414         VD2pag = 'Pag. ' + %editc( VH2page : 'X' ) + ' di 4';
070800170413
070900170413         // -?"Segue..." / "Fine."?
071000170414         if  VH2page < 4;
071100170413           VD2seg  = 'Segue...';
071200170413         else;
071300170413           VD2seg  = '   Fine.';
071400170413         endif;
071500170413
071600170413         // -?Impostazione dati da visualizzare?
071700170414         Select;
071800170414           // -?Tot. Spedizioni (generale)?
071900170414           When  VH2page = 1;
072000170426             LRY05T2_ds = LRY05T2_G_ds;
072100170413           // -?Tot. Spedizioni "da partire" (Stato = "P")?
072200170413           when  VH2page = 2;
072300170426             LRY05T2_ds = LRY05T2_P_ds;
072400170413           // -?Tot. Spedizioni "in arrivo - tentativi 0" (Stato = "A")?
072500170413           when  VH2page = 3;
072600170426             LRY05T2_ds = LRY05T2_A_ds;
072700170413           // -?Tot. Spedizioni "in arrivo - tentativi >0" (Stato = " ")?
072800170413           when  VH2page = 4;
072900170426             LRY05T2_ds = LRY05T2_B_ds;
073000170413         endsl;
073100170413
073200170413         // -?Date elaborate?
073300170426         //*·if  LRY05T2_ds.w02ddi  > *zero  and
073400170426         //*·    LRY05T2_ds.w02ddi  < *hival;
073500170426         //*·  wDate_Eur = %date( LRY05T2_ds.w02ddi : *iso );
073600170426         if  LRY05T2_G_ds.w02ddi  > *zero  and
073700170426             LRY05T2_G_ds.w02ddi  < *hival;
073800170426           wDate_Eur = %date( LRY05T2_G_ds.w02ddi : *iso );
073900170413           V1Tddi    = %dec( wDate_Eur );
074000170413         endif;
074100170426         //*·if  LRY05T2_ds.w02ddf  > *zero;
074200170426         //*·  wDate_Eur = %date( LRY05T2_ds.w02ddf : *iso );
074300170426         if  LRY05T2_G_ds.w02ddf  > *zero;
074400170426           wDate_Eur = %date( LRY05T2_G_ds.w02ddf : *iso );
074500170413           V1Tddf    = %dec( wDate_Eur );
074600170413         endif;
074700170413
074800170414         // -?Totali x Appuntamento?
074900170426         VD2tap = LRY05T2_ds.wD2tap;
075000170426         VD2dap = LRY05T2_ds.wD2dap;
075100170426         VD2map = LRY05T2_ds.wD2map;
075200170413         // -?Percentuali x Appuntamento?
075300170426         if  LRY05T2_ds.wD2ptap <> *zero;
075400170426           VD2ptap = %editc( LRY05T2_ds.wD2ptap : '3' ) + '%';
075500170413         endif;
075600170426         if  LRY05T2_ds.wD2pdap <> *zero;
075700170426           VD2pdap = %editc( LRY05T2_ds.wD2pdap : '3' ) + '%';
075800170413         endif;
075900170426         if  LRY05T2_ds.wD2pmap <> *zero;
076000170426           VD2pmap = %editc( LRY05T2_ds.wD2pmap : '3' ) + '%';
076100170413         endif;
076200170413
076300170414         // -?Totali x Fermo Depositp?
076400170426         VD2tfd = LRY05T2_ds.wD2tfd;
076500170426         VD2dfd = LRY05T2_ds.wD2dfd;
076600170426         VD2mfd = LRY05T2_ds.wD2mfd;
076700170413         // -?Percentuali x Fermo Deposito?
076800170426         if  LRY05T2_ds.wD2ptfd <> *zero;
076900170426           VD2ptfd = %editc( LRY05T2_ds.wD2ptfd : '3' ) + '%';
077000170413         endif;
077100170426         if  LRY05T2_ds.wD2pdfd <> *zero;
077200170426           VD2pdfd = %editc( LRY05T2_ds.wD2pdfd : '3' ) + '%';
077300170413         endif;
077400170426         if  LRY05T2_ds.wD2pmfd <> *zero;
077500170426           VD2pmfd = %editc( LRY05T2_ds.wD2pmfd : '3' ) + '%';
077600170413         endif;
077700170421
077800170421
077900170421         // -?Totali x Disposizioni Cambio Indirizzo + Appuntamento?
078000170426         VD2tciA = LRY05T2_ds.wD2tciA;
078100170426         VD2dciA = LRY05T2_ds.wD2dciA;
078200170426         VD2mciA = LRY05T2_ds.wD2mciA;
078300170421         // -?Percentuali x Disposiz. Cambio Indirizzo + Appuntamento?
078400170426         if  LRY05T2_ds.wD2ptciA <> *zero;
078500170426           VD2ptciA = %editc( LRY05T2_ds.wD2ptciA : '3' ) + '%';
078600170421         endif;
078700170426         if  LRY05T2_ds.wD2pdciA <> *zero;
078800170426           VD2pdciA = %editc( LRY05T2_ds.wD2pdciA : '3' ) + '%';
078900170421         endif;
079000170426         if  LRY05T2_ds.wD2pmciA <> *zero;
079100170426           VD2pmciA = %editc( LRY05T2_ds.wD2pmciA : '3' ) + '%';
079200170421         endif;
079300170421
079400170421         // -?Totali x Disposizioni Cambio Indirizzo (No Appunt, No Dirott)?
079500170426         VD2tci_ = LRY05T2_ds.wD2tci_;
079600170426         VD2dci_ = LRY05T2_ds.wD2dci_;
079700170426         VD2mci_ = LRY05T2_ds.wD2mci_;
079800170421         // -?Percentuali x Disposiz. Cambio Indirizzo (No Appunt, No Dirott)?
079900170426         if  LRY05T2_ds.wD2ptci_ <> *zero;
080000170426           VD2ptci_ = %editc( LRY05T2_ds.wD2ptci_ : '3' ) + '%';
080100170421         endif;
080200170426         if  LRY05T2_ds.wD2pdci_ <> *zero;
080300170426           VD2pdci_ = %editc( LRY05T2_ds.wD2pdci_ : '3' ) + '%';
080400170421         endif;
080500170426         if  LRY05T2_ds.wD2pmci_ <> *zero;
080600170426           VD2pmci_ = %editc( LRY05T2_ds.wD2pmci_ : '3' ) + '%';
080700170421         endif;
080800170421
080900170421         // -?Totali x Disposizioni Cambio Indirizzo + Dirottamento?
081000170426         VD2tciD = LRY05T2_ds.wD2tciD;
081100170426         VD2dciD = LRY05T2_ds.wD2dciD;
081200170426         VD2mciD = LRY05T2_ds.wD2mciD;
081300170421         // -?Percentuali x Disposiz. Cambio Indirizzo + Dirottamento?
081400170426         if  LRY05T2_ds.wD2ptciD <> *zero;
081500170426           VD2ptciD = %editc( LRY05T2_ds.wD2ptciD : '3' ) + '%';
081600170421         endif;
081700170426         if  LRY05T2_ds.wD2pdciD <> *zero;
081800170426           VD2pdciD = %editc( LRY05T2_ds.wD2pdciD : '3' ) + '%';
081900170421         endif;
082000170426         if  LRY05T2_ds.wD2pmciD <> *zero;
082100170426           VD2pmciD = %editc( LRY05T2_ds.wD2pmciD : '3' ) + '%';
082200170421         endif;
082300170421
082400170413
082500170414         // -?Totali Generali?
082600170426         VD2tge = LRY05T2_ds.wD2tge;
082700170426         VD2tde = LRY05T2_ds.wD2tde;
082800170426         VD2tmi = LRY05T2_ds.wD2tmi;
082900170421         // -?Percentuali x Totali Generali?
083000170426         //*·if  LRY05T2_ds.wD2ptge <> *zero;
083100170426         //*·  VD2ptge = %editc( LRY05T2_ds.wD2ptge : '3' ) + '%';
083200170414         //*·endif;
083300170426         if  LRY05T2_ds.wD2ptde <> *zero;
083400170426           VD2ptde = %editc( LRY05T2_ds.wD2ptde : '3' ) + '%';
083500170413         endif;
083600170426         if  LRY05T2_ds.wD2ptmi <> *zero;
083700170426           VD2ptmi = %editc( LRY05T2_ds.wD2ptmi : '3' ) + '%';
083800170413         endif;
083900170413
084000170413
084100170426         //*-// -?Impostazione ALTRI dati nella videata D3?
084200170421
084300170426         //*-// -?Totali x Cambio Indirizzo?
084400170426         //*-VD2tci = LRY05T2_ds.wD2tci;
084500170426         //*-VD2dci = LRY05T2_ds.wD2dci;
084600170426         //*-VD2mci = LRY05T2_ds.wD2mci;
084700170426         //*-// -?Percentuali x Cambio Indirizzo?
084800170426         //*-if  LRY05T2_ds.wD2ptci <> *zero;
084900170426         //*-  VD2ptci = %editc( LRY05T2_ds.wD2ptci : '3' ) + '%';
085000170426         //*-endif;
085100170426         //*-if  LRY05T2_ds.wD2pdci <> *zero;
085200170426         //*-  VD2pdci = %editc( LRY05T2_ds.wD2pdci : '3' ) + '%';
085300170426         //*-endif;
085400170426         //*-if  LRY05T2_ds.wD2pmci <> *zero;
085500170426         //*-  VD2pmci = %editc( LRY05T2_ds.wD2pmci : '3' ) + '%';
085600170426         //*-endif;
085700170421
085800170421
085900170426         //*-// -?Disposizioni CAMBIO INDIRIZZO: DIROTTAMENTO?
086000170426         //*-VD2tdi = LRY05T2_ds.wD2tciD;
086100170426         //*-VD2ddi = LRY05T2_ds.wD2dciD;
086200170426         //*-VD2mdi = LRY05T2_ds.wD2mciD;
086300170426         //*-//*·if  LRY05T2_ds.wD2ptciD <> *zero;
086400170426         //*-//*·  VD2ptdi = %editc( LRY05T2_ds.wD2ptciD : '3' ) + '%';
086500170426         //*-//*·endif;
086600170426         //*-if  LRY05T2_ds.wD2pdciD <> *zero;
086700170426         //*-  VD2pddi = %editc( LRY05T2_ds.wD2pdciD : '3' ) + '%';
086800170426         //*-endif;
086900170426         //*-if  LRY05T2_ds.wD2pmciD <> *zero;
087000170426         //*-  VD2pmdi = %editc( LRY05T2_ds.wD2pmciD : '3' ) + '%';
087100170426         //*-endif;
087200170413
087300170426         //*-// -?Disposizioni CAMBIO INDIRIZZO: NO DIROTTAMENTO?
087400170426         //*-VD2tnd = LRY05T2_ds.wD2tciA + LRY05T2_ds.wD2tci_;
087500170426         //*-VD2dnd = LRY05T2_ds.wD2dciA + LRY05T2_ds.wD2dci_;
087600170426         //*-VD2mnd = LRY05T2_ds.wD2mciA + LRY05T2_ds.wD2mci_;
087700170426         //*-if  VD2tnd <> *zero;
087800170426         //*-  eval(H) wPerc = ( VD2dnd * 100 ) / VD2tnd;
087900170426         //*-  VD2pdnd = %editc( wPerc : '3' ) + '%';
088000170426         //*-  eval(H) wPerc = ( VD2mnd * 100 ) / VD2tnd;
088100170426         //*-  VD2pmnd = %editc( wPerc : '3' ) + '%';
088200170426         //*-endif;
088300170413
088400170426         //*-// -?Disposizioni CAMBIO INDIRIZZO: NO DIR. con APPUNTAMENTO?
088500170426         //*-VD2tdca = LRY05T2_ds.wD2tciA;
088600170426         //*-VD2ddca = LRY05T2_ds.wD2dciA;
088700170426         //*-VD2mdca = LRY05T2_ds.wD2mciA;
088800170426         //*-if  VD2tdcA <> *zero;
088900170426         //*-  eval(H) wPerc = ( VD2ddcA * 100 ) / VD2tdca;
089000170426         //*-  VD2pddcA = %editc( wPerc : '3' ) + '%';
089100170426         //*-  eval(H) wPerc = ( VD2mdcA * 100 ) / VD2tdca;
089200170426         //*-  VD2pmdcA = %editc( wPerc : '3' ) + '%';
089300170426         //*-endif;
089400170413
089500170413       ENDSR;
089600150424
089700150424       //--------------------------------------------------------------
089800150424       //?Calcolo totali per videata D02.                              ?
089900150424       //--------------------------------------------------------------
090000150424       BEGSR  sr_CalcTot;
090100150424
090200150424         clear  $EoF;
090300150424
090400150424         // -?Preparazione SQL per TOTALI?
090500170421         exsr  sr_PrepSQL;
090600150424
090700150424         // -?Apertura del cursore per TOTALI?
090800170421         exsr  sr_OpenCursor;
090900150424
091000150424         // -?Estrazione TOTALI?
091100150424         DoW  $EoF = *off;
091200170421           exsr  sr_ReadCursor;
091300150424         EndDo;
091400150424
091500150424         // -?Chiusura del cursore per TOTALI?
091600170421         exsr  sr_CloseCursor;
091700170413
091800170413         // -?Calcolo percentuali TOTALI?
091900170418         // -?Totali "P" = Da Partire?
092000170426         LRY05T2_ds   = LRY05T2_P_ds;
092100170418         exsr  sr_CalcPerc;
092200170426         LRY05T2_P_ds = LRY05T2_ds;
092300170418         // -?Totali "A" = In Arrivo - Tentativi 0?
092400170426         LRY05T2_ds = LRY05T2_A_ds;
092500170418         exsr  sr_CalcPerc;
092600170426         LRY05T2_A_ds = LRY05T2_ds;
092700170418         // -?Totali "_" = In Arrivo - Tentativi > 0?
092800170426         LRY05T2_ds = LRY05T2_B_ds;
092900170418         exsr  sr_CalcPerc;
093000170426         LRY05T2_B_ds = LRY05T2_ds;
093100170418         // -?Totali generali?
093200170426         LRY05T2_ds = LRY05T2_G_ds;
093300170413         exsr  sr_CalcPerc;
093400170426         LRY05T2_G_ds = LRY05T2_ds;
093500150424
093600150424       ENDSR;
093700150430
093800150430       //--------------------------------------------------------------
093900150430       //?Preparazione SQL per calcolo TOTALI.                         ?
094000150430       //--------------------------------------------------------------
094100170421       BEGSR  sr_PrepSQL;
094200150430
094300150430         // -?Preparazione SQL per calcolo TOTALI?
094400150430         clear  wSQL;
094500150430         wSQL = 'select min(IDSinsData), max(IDSinsData), +
094600170421                        IDSstato, IDStipoDis, IDSsnApp, IDSsnDir, +
094700180228                        IDStipoCli, sum(IDStotIDC), sum(IDStotALRT) +
094800150430                   from TIIDS00F';
094900150430         $First = *on;
095000150430         exsr  sr_SQLaddSelez;
095100170421         wSQL  += ' group by IDSstato, IDStipoDis, +
095200170421                             IDSsnApp, IDSsnDir, IDStipoCli +
095300170421                    order by IDSstato, IDStipoDis, +
095400170421                             IDSsnApp, IDSsnDir, IDStipoCli +
095500180228                   for fetch only';
095600150430
095700150430         exec sql   prepare S1   from :wSQL;
095800150430
095900150430         // -?Dichiarazione cursore?
096000150430         exec sql   declare C1   cursor for S1;
096100150430         if  SQLcode < *zero;
096200150430           $EoF = *on;
096300150430           exsr  sr_PrintErrSQL;
096400150430         endif;
096500150430
096600150430       ENDSR;
096700150430
096800150430       //--------------------------------------------------------------
096900150430       //?Apertura cursore per calcolo TOTALI.                         ?
097000150430       //--------------------------------------------------------------
097100170421       BEGSR  sr_OpenCursor;
097200150430
097300150430         // -?Apertura del cursore?
097400150430         exec sql   open C1;
097500150430
097600150430         if  SQLcode < *zero;
097700150430           $EoF = *on;
097800150430           exsr  sr_PrintErrSQL;
097900150430         endif;
098000150430
098100150430       ENDSR;
098200150430
098300150430       //--------------------------------------------------------------
098400150430       //?Lettura cursore per calcolo TOTALI.                          ?
098500150430       //--------------------------------------------------------------
098600170421       BEGSR  sr_ReadCursor;
098700150430
098800170413         clear  wStato;
098900150430         clear  wTipoDis;
099000150430         clear  wTipoCli;
099100150430         clear  wTOT;
099200150430
099300150430         exec sql   fetch next   from C1   into :wDataIni, :wDataFin,
099400180228                                                :wStato,   :wTipoDis,
099500180228                                                :wSNapp,   :wSNdir,
099600180302                                                :wTipoCli, :wTOT;
099700150430
099800150430         Select;
099900150430
100000150430           // -?Fine File?
100100150430           When  SQLcode = 100;
100200150430             $EoF = *on;
100300150430
100400150430           // -?Errore SQL?
100500150430           When  SQLcode < *zero;
100600150430             $EoF = *on;
100700150430             exsr  sr_PrintErrSQL;
100800150430
100900150430           // -?Calcolo Totali?
101000150430           Other;
101100170413             select;
101200170413               when  wStato = 'P';
101300170426                 LRY05T2_ds = LRY05T2_P_ds;
101400170413               when  wStato = 'A';
101500170426                 LRY05T2_ds = LRY05T2_A_ds;
101600170413               when  wStato = *blank;
101700170426                 LRY05T2_ds = LRY05T2_B_ds;
101800170413             endsl;
101900170421             exsr  sr_CalcTot_Parz;
102000170421             exsr  sr_CalcTot_Gen;
102100170413             select;
102200170413               when  wStato = 'P';
102300170426                 LRY05T2_P_ds = LRY05T2_ds;
102400170413               when  wStato = 'A';
102500170426                 LRY05T2_A_ds = LRY05T2_ds;
102600170413               when  wStato = *blank;
102700170426                 LRY05T2_B_ds = LRY05T2_ds;
102800170413             endsl;
102900150430
103000150430         EndSl;
103100150430
103200150430       ENDSR;
103300150430
103400150430       //--------------------------------------------------------------
103500150430       //?Chiusura cursore per calcolo TOTALI.                         ?
103600150430       //--------------------------------------------------------------
103700170421       BEGSR  sr_CloseCursor;
103800150430
103900150430         // -?Chiusura del cursore?
104000150430         exec sql   close C1;
104100150430
104200150430       ENDSR;
104300170413
104400170413       //--------------------------------------------------------------
104500170414       //?Calcolo Totali parziali "Da Partire" / "In Arrivo".          ?
104600170413       //--------------------------------------------------------------
104700170421       BEGSR  sr_CalcTot_Parz;
104800170413
104900170413         // -?Data iniziale?
105000170426         if  LRY05T2_ds.W02ddi = *zero  or
105100170426             LRY05T2_ds.W02ddi > wDataIni;
105200170426           LRY05T2_ds.W02ddi = wDataIni;
105300170413         endif;
105400170413
105500170413         // -?Data finale?
105600170426         if  LRY05T2_ds.W02ddf < wDataFin;
105700170426           LRY05T2_ds.W02ddf = wDataFin;
105800170413         endif;
105900180228
106000180228         if  wTOT = *zero;
106100180228           leavesr;
106200180228         endif;
106300170413
106400170413         Select;
106500170413
106600170413           // -?Disposizione "1" = Appuntamento?
106700170413           When  wTipoDis = c_TipoDis1;
106800170426             LRY05T2_ds.wD2tap += wTOT;
106900170413             if  wTipoCli = 'D';
107000170426               LRY05T2_ds.wD2dap += wTOT;     // -?Richieste dal Dest.?
107100170413             else;
107200170426               LRY05T2_ds.wD2map += wTOT;     // -?Richieste dal Mitt.?
107300170413             endif;
107400170413
107500170413           // -?Disposizione "2" = Fermo Deposito?
107600170413           When  wTipoDis = c_TipoDis2;
107700170426             LRY05T2_ds.wD2tfd += wTOT;
107800170413             if  wTipoCli = 'D';
107900170426               LRY05T2_ds.wD2dfd += wTOT;     // -?Richieste dal Dest.?
108000170413             else;
108100170426               LRY05T2_ds.wD2mfd += wTOT;     // -?Richieste dal Mitt.?
108200170413             endif;
108300170413
108400170413           // -?Disposizione "3" = Cambio Indirizzo?
108500170413           When  wTipoDis = c_TipoDis3;
108600170426             //*-LRY05T2_ds.wD2tci += wTOT;
108700170426             //*-if  wTipoCli = 'D';
108800170426             //*-  LRY05T2_ds.wD2dci += wTOT;     // -?Richieste dal Dest.?
108900170426             //*-else;
109000170426             //*-  LRY05T2_ds.wD2mci += wTOT;     // -?Richieste dal Mitt.?
109100170426             //*-endif;
109200170421             Select;
109300170421               // -?Cambio Indirizzo + Dirottamento?
109400170421               When  wSNdir = 'S';
109500170426                 LRY05T2_ds.wD2tciD += wTOT;
109600170421                 if  wTipoCli = 'D';
109700170426                   LRY05T2_ds.wD2dciD += wTOT;     // -?Richieste dal Dest.?
109800170421                 else;
109900170426                   LRY05T2_ds.wD2mciD += wTOT;     // -?Richieste dal Mitt.?
110000170421                 endif;
110100170421               // -?Cambio Indirizzo + Appuntamento?
110200170421               When  wSNapp = 'S';
110300170426                 LRY05T2_ds.wD2tciA += wTOT;
110400170421                 if  wTipoCli = 'D';
110500170426                   LRY05T2_ds.wD2dciA += wTOT;     // -?Richieste dal Dest.?
110600170421                 else;
110700170426                   LRY05T2_ds.wD2mciA += wTOT;     // -?Richieste dal Mitt.?
110800170421                 endif;
110900170421               // -?Cambio Indirizzo (No Dirottamento, No Appuntamento)?
111000170421               Other;
111100170426                 LRY05T2_ds.wD2tci_ += wTOT;
111200170421                 if  wTipoCli = 'D';
111300170426                   LRY05T2_ds.wD2dci_ += wTOT;     // -?Richieste dal Dest.?
111400170421                 else;
111500170426                   LRY05T2_ds.wD2mci_ += wTOT;     // -?Richieste dal Mitt.?
111600170421                 endif;
111700170421             EndSl;
111800170413
111900170413         EndSl;
112000170413
112100170413         // -?Totale generale?
112200170426         LRY05T2_ds.wD2tge += wTOT;
112300170413         if  wTipoCli = 'D';
112400170426           LRY05T2_ds.wD2tde += wTOT;    // -?Richieste dal Destinat.?
112500170413         else;
112600170426           LRY05T2_ds.wD2tmi += wTOT;    // -?Richieste dal Mittente?
112700170413         endif;
112800170413
112900170413       ENDSR;
113000170413
113100170413       //--------------------------------------------------------------
113200170413       //?Calcolo Totali Generali.                                     ?
113300170413       //--------------------------------------------------------------
113400170421       BEGSR  sr_CalcTot_Gen;
113500170414
113600170414         // -?Data iniziale?
113700170426         if  LRY05T2_G_ds.W02ddi = *zero  or
113800170426             LRY05T2_G_ds.W02ddi > wDataIni;
113900170426           LRY05T2_G_ds.W02ddi = wDataIni;
114000170414         endif;
114100170414
114200170414         // -?Data finale?
114300170426         if  LRY05T2_G_ds.W02ddf < wDataFin;
114400170426           LRY05T2_G_ds.W02ddf = wDataFin;
114500170414         endif;
114600180228
114700180228         if  wTOT = *zero;
114800180228           leavesr;
114900180228         endif;
115000170414
115100170414         Select;
115200170414
115300170414           // -?Disposizione "1" = Appuntamento?
115400170414           When  wTipoDis = c_TipoDis1;
115500170426             LRY05T2_G_ds.wD2tap += wTOT;
115600170414             if  wTipoCli = 'D';
115700170426               LRY05T2_G_ds.wD2dap += wTOT;   // -?Richieste dal Dest.?
115800170414             else;
115900170426               LRY05T2_G_ds.wD2map += wTOT;   // -?Richieste dal Mitt.?
116000170414             endif;
116100170414
116200170414           // -?Disposizione "2" = Fermo Deposito?
116300170414           When  wTipoDis = c_TipoDis2;
116400170426             LRY05T2_G_ds.wD2tfd += wTOT;
116500170414             if  wTipoCli = 'D';
116600170426               LRY05T2_G_ds.wD2dfd += wTOT;   // -?Richieste dal Dest.?
116700170414             else;
116800170426               LRY05T2_G_ds.wD2mfd += wTOT;   // -?Richieste dal Mitt.?
116900170414             endif;
117000170414
117100170414           // -?Disposizione "3" = Cambio Indirizzo?
117200170414           When  wTipoDis = c_TipoDis3;
117300170426             //*-LRY05T2_G_ds.wD2tci += wTOT;
117400170426             //*-if  wTipoCli = 'D';
117500170426             //*-  LRY05T2_G_ds.wD2dci += wTOT;   // -?Richieste dal Dest.?
117600170426             //*-else;
117700170426             //*-  LRY05T2_G_ds.wD2mci += wTOT;   // -?Richieste dal Mitt.?
117800170426             //*-endif;
117900170421             Select;
118000170421               // -?Cambio Indirizzo + Dirottamento?
118100170421               When  wSNdir = 'S';
118200170426                 LRY05T2_G_ds.wD2tciD += wTOT;
118300170421                 if  wTipoCli = 'D';
118400170426                   LRY05T2_G_ds.wD2dciD += wTOT;   // -?Richieste dal Dest.?
118500170421                 else;
118600170426                   LRY05T2_G_ds.wD2mciD += wTOT;   // -?Richieste dal Mitt.?
118700170421                 endif;
118800170421               // -?Cambio Indirizzo + Appuntamento?
118900170421               When  wSNapp = 'S';
119000170426                 LRY05T2_G_ds.wD2tciA += wTOT;
119100170421                 if  wTipoCli = 'D';
119200170426                   LRY05T2_G_ds.wD2dciA += wTOT;   // -?Richieste dal Dest.?
119300170421                 else;
119400170426                   LRY05T2_G_ds.wD2mciA += wTOT;   // -?Richieste dal Mitt.?
119500170421                 endif;
119600170421               // -?Cambio Indirizzo (No Dirottamento, No Appuntamento)?
119700170421               Other;
119800170426                 LRY05T2_G_ds.wD2tci_ += wTOT;
119900170421                 if  wTipoCli = 'D';
120000170426                   LRY05T2_G_ds.wD2dci_ += wTOT;   // -?Richieste dal Dest.?
120100170421                 else;
120200170426                   LRY05T2_G_ds.wD2mci_ += wTOT;   // -?Richieste dal Mitt.?
120300170421                 endif;
120400170421             EndSl;
120500170414
120600170414         EndSl;
120700170414
120800170414         // -?Totale generale?
120900170426         LRY05T2_G_ds.wD2tge += wTOT;
121000170414         if  wTipoCli = 'D';
121100170426           LRY05T2_G_ds.wD2tde += wTOT;  // -?Richieste dal Destinat.?
121200170414         else;
121300170426           LRY05T2_G_ds.wD2tmi += wTOT;  // -?Richieste dal Mittente?
121400170414         endif;
121500170413
121600170413       ENDSR;
121700150416
121800150416       //--------------------------------------------------------------
121900150416       //?Aggiunta delle selezioni impostate a video alla stringa SQL. ?
122000150416       //--------------------------------------------------------------
122100150416       BEGSR  sr_SQLaddSelez;
122200150416
122300150416         If  wInsDtI > *zero  or  wInsDtF > *zero;
122400150416           if  $First;
122500150416             wSQL  += ' where ';
122600150416             $First = *off;
122700150416           else;
122800150416             wSQL  += ' and ';
122900150416           endif;
123000150416           wSQL += 'IDSinsData between ' + %trim( %editc( wInsDtI : '3' ) ) +
123100150416                                 ' and ' + %trim( %editc( wInsDtF : '3' ) );
123200150416         EndIf;
123300150416
123400150416         If  V1Cfgs <> *zero;
123500150416           if  $First;
123600150416             wSQL  += ' where ';
123700150416             $First = *off;
123800150416           else;
123900150416             wSQL  += ' and ';
124000150416           endif;
124100150416           wSQL += 'IDSfgs = ' + %trim( %editc( V1Cfgs : '3' ) );
124200150416         EndIf;
124300150416
124400150416         If  V1CtipoCli <> *blank;
124500150416           if  $First;
124600150416             wSQL  += ' where ';
124700150416             $First = *off;
124800150416           else;
124900150416             wSQL  += ' and ';
125000150416           endif;
125100150416           wSQL += 'IDStipoCli = ''' + V1CtipoCli + '''';
125200150416         EndIf;
125300180302
125400180302         If  V1Calert <> *blank;
125500180302           if  $First;
125600180302             wSQL  += ' where ';
125700180302             $First = *off;
125800180302           else;
125900180302             wSQL  += ' and ';
126000180302           endif;
126100180302           If  V1Calert = 'N';
126200180302             wSQL += 'IDStotALRT = 0';
126300180302           else;
126400180302             wSQL += 'IDStotALRT <> 0';
126500180302           endif;
126600180302         EndIf;
126700150416
126800150416       ENDSR;
126900170414
127000170414       //--------------------------------------------------------------
127100170414       //?Calcolo Percentuali per le Disposizioni di Consegna reperite.?
127200170414       //--------------------------------------------------------------
127300170414       BEGSR  sr_CalcPerc;
127400170414
127500170426         if  LRY05T2_ds.wD2tge = *zero;
127600170421           leavesr;
127700170421         endif;
127800170421
127900170421
128000170421         // -?Percentuale x Appuntamento?
128100170426         eval(H) LRY05T2_ds.wD2ptap  = ( LRY05T2_ds.wD2tap * 100 ) /
128200170426                                         LRY05T2_ds.wD2tge;
128300170426         if  LRY05T2_ds.wD2tap <> *zero;
128400170426           eval(H) LRY05T2_ds.wD2pdap = ( LRY05T2_ds.wD2dap * 100 ) /
128500170426                                          LRY05T2_ds.wD2tap;
128600170426           eval(H) LRY05T2_ds.wD2pmap = ( LRY05T2_ds.wD2map * 100 ) /
128700170426                                          LRY05T2_ds.wD2tap;
128800170421         endif;
128900170421
129000170421         // -?Percentuale x Fermo Deposito?
129100170426         eval(H) LRY05T2_ds.wD2ptfd  = ( LRY05T2_ds.wD2tfd * 100 ) /
129200170426                                         LRY05T2_ds.wD2tge;
129300170426         if  LRY05T2_ds.wD2tfd <> *zero;
129400170426           eval(H) LRY05T2_ds.wD2pdfd = ( LRY05T2_ds.wD2dfd * 100 ) /
129500170426                                          LRY05T2_ds.wD2tfd;
129600170426           eval(H) LRY05T2_ds.wD2pmfd = ( LRY05T2_ds.wD2mfd * 100 ) /
129700170426                                          LRY05T2_ds.wD2tfd;
129800170421         endif;
129900170421
130000170426         //*-// -?Percentuale x Cambio Indirizzo?
130100170426         //*-eval(H) LRY05T2_ds.wD2ptci  = ( LRY05T2_ds.wD2tci * 100 ) /
130200170426         //*-                                LRY05T2_ds.wD2tge;
130300170426         //*-if  LRY05T2_ds.wD2tci <> *zero;
130400170426         //*-  eval(H) LRY05T2_ds.wD2pdci = ( LRY05T2_ds.wD2dci * 100 ) /
130500170426         //*-                                 LRY05T2_ds.wD2tci;
130600170426         //*-  eval(H) LRY05T2_ds.wD2pmci = ( LRY05T2_ds.wD2mci * 100 ) /
130700170426         //*-                                 LRY05T2_ds.wD2tci;
130800170426         //*-endif;
130900170421
131000170421         // -?Percentuale x Cambio Indirizzo + Appuntamento?
131100170426         eval(H) LRY05T2_ds.wD2ptciA = ( LRY05T2_ds.wD2tciA * 100 ) /
131200170426                                         LRY05T2_ds.wD2tge;
131300170426         if  LRY05T2_ds.wD2tciA <> *zero;
131400170426           eval(H) LRY05T2_ds.wD2pdciA = ( LRY05T2_ds.wD2dciA * 100 ) /
131500170426                                           LRY05T2_ds.wD2tciA;
131600170426           eval(H) LRY05T2_ds.wD2pmciA = ( LRY05T2_ds.wD2mciA * 100 ) /
131700170426                                           LRY05T2_ds.wD2tciA;
131800170421         endif;
131900170421
132000170421         // -?Percentuale x Cambio Indirizzo (No Appuntam, No Dirottam)?
132100170426         eval(H) LRY05T2_ds.wD2ptci_ = ( LRY05T2_ds.wD2tci_ * 100 ) /
132200170426                                         LRY05T2_ds.wD2tge;
132300170426         if  LRY05T2_ds.wD2tci_ <> *zero;
132400170426           eval(H) LRY05T2_ds.wD2pdci_ = ( LRY05T2_ds.wD2dci_ * 100 ) /
132500170426                                           LRY05T2_ds.wD2tci_;
132600170426           eval(H) LRY05T2_ds.wD2pmci_ = ( LRY05T2_ds.wD2mci_ * 100 ) /
132700170426                                           LRY05T2_ds.wD2tci_;
132800170421         endif;
132900170421
133000170421         // -?Percentuale x Cambio Indirizzo + Dirottamento?
133100170426         eval(H) LRY05T2_ds.wD2ptciD = ( LRY05T2_ds.wD2tciD * 100 ) /
133200170426                                         LRY05T2_ds.wD2tge;
133300170426         if  LRY05T2_ds.wD2tciD <> *zero;
133400170426           eval(H) LRY05T2_ds.wD2pdciD = ( LRY05T2_ds.wD2dciD * 100 ) /
133500170426                                           LRY05T2_ds.wD2tciD;
133600170426           eval(H) LRY05T2_ds.wD2pmciD = ( LRY05T2_ds.wD2mciD * 100 ) /
133700170426                                           LRY05T2_ds.wD2tciD;
133800170421         endif;
133900170421
134000170421         // -?Percentuale x Totale Generale?
134100170426         //*·eval(H) LRY05T2_ds.wD2ptge = ( LRY05T2_ds.wD2tge * 100 ) /
134200170426         //*·                               LRY05T2_ds.wD2tge;
134300170426         eval    LRY05T2_ds.wD2ptge = 100;
134400170421         // -?Percentuale x Totale Generale / Destinatari?
134500170426         eval(H) LRY05T2_ds.wD2ptde = ( LRY05T2_ds.wD2tde * 100 ) /
134600170426                                        LRY05T2_ds.wD2tge;
134700170421         // -?Percentuale x Totale Generale / Mittenti?
134800170426         eval(H) LRY05T2_ds.wD2ptmi = ( LRY05T2_ds.wD2tmi * 100 ) /
134900170426                                        LRY05T2_ds.wD2tge;
135000170414
135100170414       ENDSR;
135200150416
135300150416       //--------------------------------------------------------------
135400150416       //?Stampa segnalazione dell'errore rilevato via SQL             ?
135500150416       //--------------------------------------------------------------
135600150416       BEGSR  sr_PrintErrSQL;
135700150416
135800150416         // -?Stampa del Dump?
135900150416         Dump(A);
136000150416
136100150416         // -?Stampa del Job-Log?
136200150416         Qcmd = 'DSPJOBLOG job(*) output(*print)';
136300150416         exsr  sr_ExecCmd;
136400150416
136500150416         // -?Chiusura del programma?
136600150416         exsr  sr_RoutEnd;
136700150416
136800150416       ENDSR;
136900150416
137000150416       //--------------------------------------------------------------
137100150416       //?Esecuzione del comando (già impostato).                      ?
137200150416       //--------------------------------------------------------------
137300150416       BEGSR  sr_ExecCmd;
137400150416
137500150416         clear Qcap0100;
137600150416         Qcabcsdh = *off;
137700150416         Qcapa    = *off;
137800150416         Qcacmdss = *off;
137900150416         Qcaerved = *allX'00';
138000150416
138100150416         clear Qusec;
138200150416         Qusbprv  = %size(Qusec);
138300150416
138400150416         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
138500150416                           %size(Qcap0100) : 'CPOP0100' : *omit :
138600150416                           0 : 0 : Qusec);
138700150416
138800150416         //if  Qusei <> *blank;
138900150416         //  ...;
139000150416         //endif;
139100150416
139200150416       ENDSR;
139300150416
139400150416       //--------------------------------------------------------------
139500150416       //?Operazioni finali.                                           ?
139600150416       //--------------------------------------------------------------
139700150416       BEGSR  sr_RoutEnd;
139800150416
139900150416         // -?Uscita dal *pgm?
140000150416         return;
140100150416
140200150416       ENDSR;
140300150327
140400150327      /end-free
140500150327
140600150327       //--------------------------------------------------------------
140700150327       //?Definizione schiere a tempo di compilazione                  ?
140800150327       //--------------------------------------------------------------
140900150327
141000170426       //*·** --?sk_TpDsp / sk_DesTpDsp:?Tipi Disposizione?
141100170426       //*·1- Appuntamento      1
141200170426       //*·2- Fermo Deposito    2
141300170426       //*·3- Cambio Indirizzo  3
141400150417** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
141500150327Data formalmente errata                                                         1
141600150327Data DAL successiva alla data AL                                                2
141700180228Inserire un valore corretto                                                     3
141800180228Filiale arrivo errata                                                           4
141900180228Tipo cliente errato                                                             5
