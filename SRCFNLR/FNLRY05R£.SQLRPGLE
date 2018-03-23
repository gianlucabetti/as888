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
008700150430     d sk_Msg          s             78    dim( 4)  ctdata  perrcd(1)
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
014100150327     d   PosCurFgs                     n   overlay( IndDspF : 53 )
014200150327     d   PosCurTpDsp                   n   overlay( IndDspF : 54 )
014300150327     d   PosCurTpCli                   n   overlay( IndDspF : 55 )
014400150327         //   -?Riemissione videata?
014500150327     d   ErrGenerico                   n   overlay( IndDspF : 99 )
014600150424
014700170414       // -?Dati estratti via SQL: totali (di comodo)?
014800170426     d LRY05T2_ds      ds                  inz         qualified
014900150424         // ·?Data Inizio periodo?
015000150430     d   w02ddi                            inz(*hival) like(V1Tddi)
015100150424         // ·?Data Fine   periodo?
015200150430     d   w02ddf                            inz(*loval) like(V1Tddf)
015300170413         // ·?Tot. Appuntamenti?
015400170413     d   wD2tap                            inz         like(VD2tap)
015500170413         // ·?Tot. Appuntamenti x Dest.?
015600170413     d   wD2dap                            inz         like(VD2dap)
015700170413         // ·?Tot. Appuntamenti x Mitt.?
015800170413     d   wD2map                            inz         like(VD2map)
015900150424         // ·?% Tot. Appuntamenti?
016000150424     d   wD2ptap                      4  1 inz
016100150424         // ·?% Tot. Appuntamenti x Dest.?
016200150424     d   wD2pdap                      4  1 inz
016300150424         // ·?% Tot. Appuntamenti x Mitt.?
016400150424     d   wD2pmap                      4  1 inz
016500170413         // ·?Tot. Fermo Deposito?
016600170413     d   wD2tfd                            inz         like(VD2tfd)
016700170413         // ·?Tot. Fermo Deposito x Dest.?
016800170413     d   wD2dfd                            inz         like(VD2dfd)
016900170413         // ·?Tot. Fermo Deposito x Mitt.?
017000170413     d   wD2mfd                            inz         like(VD2mfd)
017100150424         // ·?% Tot. Fermo Deposito?
017200150424     d   wD2ptfd                      4  1 inz
017300150424         // ·?% Tot. Fermo Deposito x Dest.?
017400150424     d   wD2pdfd                      4  1 inz
017500150424         // ·?% Tot. Fermo Deposito x Mitt.?
017600150424     d   wD2pmfd                      4  1 inz
017700170426      *//-// ·?Tot. Cambio Indirizzo?
017800170426     d*//-wD2tci                            inz         like(VD2tci)
017900170426      *//-// ·?Tot. Cambio Indirizzo x Dest.?
018000170426     d*//-wD2dci                            inz         like(VD2dci)
018100170426      *//-// ·?Tot. Cambio Indirizzo x Mitt.?
018200170426     d*//-wD2mci                            inz         like(VD2mci)
018300170426      *//-// ·?% Tot. Cambio Indirizzo?
018400170426     d*//-wD2ptci                      4  1 inz
018500170426      *//-// ·?% Tot. Cambio Indirizzo x Dest.?
018600170426     d*//-wD2pdci                      4  1 inz
018700170426      *//-// ·?% Tot. Cambio Indirizzo x Mitt.?
018800170426     d*//-wD2pmci                      4  1 inz
018900170420         // ·?Tot. Cambio Indirizzo + Appuntamenti?
019000170421     d   wD2tciA                           inz         like(VD2tcia)
019100170420         // ·?Tot. Cambio Indirizzo + Appuntamenti x Dest.?
019200170421     d   wD2dciA                           inz         like(VD2dcia)
019300170420         // ·?Tot. Cambio Indirizzo + Appuntamenti x Mitt.?
019400170421     d   wD2mciA                           inz         like(VD2mcia)
019500170420         // ·?% Tot. Cambio Indirizzo + Appuntamenti?
019600170421     d   wD2ptciA                     4  1 inz
019700170420         // ·?% Tot. Cambio Indirizzo + Appuntamenti x Dest.?
019800170421     d   wD2pdciA                     4  1 inz
019900170420         // ·?% Tot. Cambio Indirizzo + Appuntamenti x Mitt.?
020000170421     d   wD2pmciA                     4  1 inz
020100170420         // ·?Tot. Cambio Indirizzo (No Dirottam., No Appuntam.)?
020200170420     d   wD2tci_                           inz         like(VD2tci_)
020300170420         // ·?Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Dest.?
020400170420     d   wD2dci_                           inz         like(VD2dci_)
020500170420         // ·?Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Mitt.?
020600170420     d   wD2mci_                           inz         like(VD2mci_)
020700170420         // ·?% Tot. Cambio Indirizzo (No Dirottam., No Appuntam.)?
020800170420     d   wD2ptci_                     4  1 inz
020900170420         // ·?% Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Dest.?
021000170420     d   wD2pdci_                     4  1 inz
021100170420         // ·?% Tot. Cambio Indirizzo (No Dirottam., No Appuntam.) x Mitt.?
021200170420     d   wD2pmci_                     4  1 inz
021300170420         // ·?Tot. Cambio Ind. + Dirottamenti?
021400170421     d   wD2tciD                           inz         like(VD2tcid)
021500170420         // ·?Tot. Cambio Ind. + Dirottamenti x Dest.?
021600170421     d   wD2dciD                           inz         like(VD2dcid)
021700170420         // ·?Tot. Cambio Ind. + Dirottamenti x Mitt.?
021800170421     d   wD2mciD                           inz         like(VD2mcid)
021900170420         // ·?% Tot. Cambio Ind. + Dirottamenti?
022000170421     d   wD2ptciD                     4  1 inz
022100170420         // ·?% Tot. Cambio Ind. + Dirottamenti x Dest.?
022200170421     d   wD2pdciD                     4  1 inz
022300170420         // ·?% Tot. Cambio Ind. + Dirottamenti x Mitt.?
022400170421     d   wD2pmciD                     4  1 inz
022500170413         // ·?Tot. Generale?
022600170413     d   wD2tge                            inz         like(VD2tge)
022700170413         // ·?Tot. Generale x Dest.?
022800170413     d   wD2tde                            inz         like(VD2tde)
022900170413         // ·?Tot. Generale x Mitt.?
023000170413     d   wD2tmi                            inz         like(VD2tmi)
023100150424         // ·?% Tot. Generale?
023200150424     d   wD2ptge                      4  1 inz
023300150424         // ·?% Tot. Generale x Dest.?
023400150424     d   wD2ptde                      4  1 inz
023500150424         // ·?% Tot. Generale x Mitt.?
023600150424     d   wD2ptmi                      4  1 inz
023700170413
023800170413       // -?Dati estratti via SQL: totali "P" = Da Partire?
023900170426     d LRY05T2_P_ds    ds                  likeds(LRY05T2_ds)
024000170414     d                                     inz
024100170413       // -?Dati estratti via SQL: totali "A" = In Arrivo - Tentativi 0?
024200170426     d LRY05T2_A_ds    ds                  likeds( LRY05T2_ds )
024300170414     d                                     inz
024400170413       // -?Dati estratti via SQL: totali "_" = In Arrivo - Tentativi > 0?
024500170426     d LRY05T2_B_ds    ds                  likeds( LRY05T2_ds )
024600170414     d                                     inz
024700170414       // -?Dati estratti via SQL: totali generali?
024800170426     d LRY05T2_G_ds    ds                  likeds( LRY05T2_ds )
024900170414     d                                     inz
025000150218
025100150424       // -?Dati estratti via SQL: dettaglio?
025200150416     d TIIDS_ds      e ds                  extname(TIIDS00F)
025300170414     d                                     inz         qualified
025400150218
025500150218       // -?Parametri ricevuti?
025600150218     d KPJBA         e ds
025700150218
025800150218       //--------------------------------------------------------------
025900150218       //?Definizione variabili globali.                               ?
026000150218       //--------------------------------------------------------------
026100150218
026200150218       // -?Flags booleani?
026300150218     d $EoF            s               n   inz
026400150416     d $RecOK          s               n   inz
026500150416     d $Fine           s               n   inz
026600150416     d $First          s               n   inz(*on)
026700150416     d $InzD01         s               n   inz(*on)
026800150416     d $InzD02         s               n   inz(*on)
026900150417
027000150218       // -?Campi associati al video?
027100150218     d $Video          s              2    inz('D1')
027200170414     d wPage           s                   inz         like(VH2page)
027300150218
027400150218       // -?Stringa SQL da eseguire?
027500150218     d wSQL            s           7000    inz  varying
027600150416
027700150416       // -?Campi di comodo per estrazione Totali via SQL?
027800150423     d wDataIni        s                   inz  like(TIIDS_ds.IDSinsData)
027900150423     d wDataFin        s                   inz  like(TIIDS_ds.IDSinsData)
028000170414     d wStato          s                   inz  like(TIIDS_ds.IDSstato)
028100150416     d wTipoDis        s                   inz  like(TIIDS_ds.IDStipoDis)
028200170421     d wSNapp          s                   inz  like(TIIDS_ds.IDSsnApp)
028300170421     d wSNdir          s                   inz  like(TIIDS_ds.IDSsnDir)
028400150416     d wTipoCli        s                   inz  like(TIIDS_ds.IDStipoCli)
028500150416     d wTOT            s             13  0 inz
028600150416
028700150416       // -?Campi di comodo per controllo dati inseriti a video?
028800150416     d wInsDtI         s              8  0 inz
028900150416     d wInsDtF         s              8  0 inz
029000170421
029100170421       // -?Campo di comodo per calcolo percentuali?
029200170421     d wPerc           s              4  1 inz
029300150416
029400150416       // -?Campi di tipo Data?
029500150416     d wDate_EUR       s               d   inz  datfmt(*EUR)
029600150218
029700150218       //--------------------------------------------------------------
029800150218       //?Definizione prototipi procedure usate.                       ?
029900150218       //--------------------------------------------------------------
030000150218
030100150218       // -?Reperimento NETA sistema AS/400 corrente?
030200150218     d currSysNeta     s              8a   inz
030300150218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
030400150218
030500150218       // -?Reperimento dati utente?
030600150218     d TIBS34ds      e ds                  inz
030700150218      /copy gaitrasrc/srcProtoPR,TIBS34R
030800150327
030900150327       // -?Controllo/Inversione data?
031000150327     d WLBdat          ds                  inz
031100150327     d   G08dat                       8  0 inz
031200150327     d   G08inv                       8  0 inz
031300150327     d   G08err                       1    inz('3')
031400150327     d   G08tgi                       5  0 inz
031500150327      /copy gaitrasrc/srcProtoPR,XSRDA8
031600150218
031700150218       // -?API QCAPCMD (Process Commands)?
031800150218     d Qcmd            s           2048    inz  varying
031900150218      /copy qSysInc/qRpgleSrc,QCAPCMD
032000150218      /copy gaitrasrc/srcProtoPR,QCAPCMD
032100150218
032200150218       // -?Parametri gestione errori API.?
032300150218      /copy qsysinc/qrpglesrc,QUSEC
032400150218
032500150218       //--------------------------------------------------------------
032600150218       //?Definizione key-list.                                        ?
032700150218       //--------------------------------------------------------------
032800150218
032900150218
033000150218       //--------------------------------------------------------------
033100150218       //?Riepilogo indicatori utilizzati.                             ?
033200150218       //--------------------------------------------------------------
033300150218
033400150218
033500150218       //--------------------------------------------------------------
033600150218       //?M A I N - L I N E                                            ?
033700150218       //--------------------------------------------------------------
033800150218
033900150218     c     *Entry        plist
034000150218     c                   parm                    KPJBA
034100150218
034200150218      /free
034300150218
034400150218       // -?Operazioni iniziali?
034500150218       exsr  sr_RoutInz;
034600150218
034700150218       // -?Ciclo di gestione del file video?
034800150218       DoW  $Fine = *off;
034900150218
035000150218         select;
035100150218
035200150218           // -?Gestione videata D1 (parzializzazioni)?
035300150218           when  $Video = 'D1';
035400150218             exsr  sr_GesD01;
035500150417
035600150424           // -?Gestione videata D2 (totale)?
035700150417           when  $Video = 'D2';
035800150417             exsr  sr_GesD02;
035900170421           when  $Video = 'D3';
036000170421             exsr  sr_GesD02;
036100150218
036200150218           // -?? ? ??
036300150218           other;
036400150218             $Fine = *on;
036500150218
036600150218         endsl;
036700150218
036800150218       EndDo;
036900150218
037000150218       // -?Operazioni finali?
037100150218       exsr  sr_RoutEnd;
037200150218
037300150218       //--------------------------------------------------------------
037400150218       //?Operazioni iniziali.                                         ?
037500150218       //--------------------------------------------------------------
037600150218       BEGSR  sr_RoutInz;
037700150218
037800150218         // -?Impostazione opzioni per SQL?
037900150218         exec sql   set  option  DynUsrPrf = *Owner,
038000150218                                 CloSqlCsr = *EndMod;
038100150218
038200150218         // -?Impostazione chiusura?
038300150218         *inLR = *on;
038400150218
038500150218         // -?Verifica del sistema AS/400 corrente?
038600150218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
038700150218           exsr  sr_RoutEnd;
038800150218         endif;
038900150218
039000150218         // -?Reperimento dati job?
039100150218         exsr  sr_DatiJob;
039200150218
039300150218         // -?Impostazione nome programma a video?
039400150416         V1Tpgm = SDSpgm;
039500150218
039600150218       ENDSR;
039700150218
039800150218       //--------------------------------------------------------------
039900150218       //?Reperimento Dati del job (Utente/Operativi).                 ?
040000150218       //--------------------------------------------------------------
040100150218       BEGSR  sr_DatiJob;
040200150218
040300150218         in(e) §AzUte;
040400150218         if NOT %error;
040500150218           in(e) §DatiUte;
040600150218         endif;
040700150218         if %error or RSut = *blank;
040800150218           tibs34r ( tibs34ds );
040900150218           in §AzUte;
041000150218           in §DatiUte;
041100150218         endif;
041200150218
041300150218       ENDSR;
041400150218
041500150218       //--------------------------------------------------------------
041600150218       //?Gestione videata D01.                                        ?
041700150218       //--------------------------------------------------------------
041800150218       BEGSR  sr_GesD01;
041900150218
042000150218         // -?Inizializzazione videata?
042100150218         if  $InzD01 = *on;
042200150218           exsr  sr_InzD01;
042300150218           $InzD01 = *off;
042400150218         endif;
042500150416         F9Attivo  = *on;
042600150416         F12Attivo = *off;
042700170421         F21Attivo = *off;
042800150430         $VisualDate = *off;
042900150218
043000150423         // -?Emissione testata e piede?
043100170426         write  LRY05T1;
043200170426         write  LRY05P1;
043300150218
043400150218         // -?Emissione videata?
043500170426         exfmt  LRY05D1;
043600150218
043700150218         clear  errGenerico;
043800150218         clear  errMessage;
043900150218         clear  V1Dmsg;
044000150218
044100150218         Select;
044200150218
044300150218           // -?F3=Fine?
044400150218           when  dsp_aid = c_F03;
044500150218             $Fine  = *on;
044600150416
044700150416           // -?F9=Visualizzazione file TIIDS00F?
044800150416           when  dsp_aid = c_F09;
044900150429             Qcmd = 'DSPDBF file(TIIDS00F)';
045000150416             exsr  sr_ExecCmd;
045100150218
045200150218           // -?Invio?
045300150218           Other;
045400150218             exsr  sr_CtrD01;
045500150218             if  errGenerico;
045600150218               leavesr;
045700150218             endif;
045800150424             $Video  = 'D2';
045900150424             $InzD02 = *on;
046000150218
046100150218         EndSl;
046200150218
046300150218       ENDSR;
046400150218
046500150218       //--------------------------------------------------------------
046600150218       //?Inizializzazione videata D01.                                ?
046700150218       //--------------------------------------------------------------
046800150218       BEGSR  sr_InzD01;
046900150218
047000170426         clear  LRY05D1;
047100150430         V1Dfgs = 'TUTTE';
047200150218
047300150218       ENDSR;
047400150218
047500150218       //--------------------------------------------------------------
047600150218       //?Controllo dati in videata D01.                               ?
047700150218       //--------------------------------------------------------------
047800150218       BEGSR  sr_CtrD01;
047900150218
048000150218         // -?Spegnimento degli indicatori di errore?
048100150218         %subst(IndDspF : 50) = *off;
048200150327
048300150327         // -?Data inserimento richiesta DAL?
048400150327         clear  wInsDtI;
048500150327         if  V1CinsDtI > *zero;
048600150327           clear  WLBdat;
048700150327           G08dat = V1CinsDtI;
048800150327           XSRDA8 ( WLBdat );
048900150327           if  G08err = *on;
049000150327             ErrGenerico = *on;
049100150327             ErrMessage  = *on;
049200150327             PosCurInsDI = *on;
049300150327             V1Dmsg = sk_Msg(01);
049400150327             leavesr;
049500150327           else;
049600150327             V1CinsDtI = G08dat;
049700150327             wInsDtI   = G08inv;
049800150327           endif;
049900150327         endif;
050000150327
050100150327         // -?Data inserimento richiesta AL?
050200150327         clear  wInsDtF;
050300150327         if  V1CinsDtF > *zero;
050400150327           clear  WLBdat;
050500150327           G08dat = V1CinsDtF;
050600150327           XSRDA8 ( WLBdat );
050700150327           if  G08err = *on;
050800150327             ErrGenerico = *on;
050900150327             ErrMessage  = *on;
051000150327             PosCurInsDF = *on;
051100150327             V1Dmsg = sk_Msg(01);
051200150327             leavesr;
051300150327           else;
051400150416             V1CinsDtF = G08dat;
051500150327             wInsDtF   = G08inv;
051600150327           endif;
051700150327         endif;
051800150327
051900150327         // -?Periodo (Range date)?
052000150327         if  wInsDtI > wInsDtF;
052100150327           ErrGenerico = *on;
052200150327           ErrMessage  = *on;
052300150327           PosCurInsDF = *on;
052400150327           V1Dmsg = sk_Msg(02);
052500150327           leavesr;
052600150327         endif;
052700150327
052800150327         // -?Filiale arrivo?
052900150327         clear  V1Dfgs;
053000150430         if  V1Cfgs = *zero;
053100150430           V1Dfgs = 'TUTTE';
053200150430         else;
053300150416           chain  ( V1Cfgs  )  AZORG;
053400150327           if  %found(AZORG01L)  and  ORGfva = *blank;
053500150327             V1Dfgs = ORGdes;
053600150327           else;
053700150327             ErrMessage  = *on;
053800150327             ErrGenerico = *on;
053900150327             PosCurFGS   = *on;
054000150327             V1Dmsg = sk_Msg(03);
054100150327             leavesr;
054200150327           endif;
054300150327         endif;
054400150327
054500150327         // -?Tipo cliente?
054600150416         if  V1CtipoCli <> *blank  and  V1CtipoCli <> 'M'
054700150424                                   and  V1CtipoCli <> 'D';
054800150327           ErrGenerico = *on;
054900150327           ErrMessage  = *on;
055000150327           PosCurTpCli = *on;
055100150430           V1Dmsg = sk_Msg(04);
055200150327           leavesr;
055300150327         endif;
055400150218
055500150218       ENDSR;
055600150424
055700150424       //--------------------------------------------------------------
055800150424       //?Gestione videata D02.                                        ?
055900150424       //--------------------------------------------------------------
056000150424       BEGSR  sr_GesD02;
056100150424
056200150424         // -?Inizializzazione videata?
056300150424         if  $InzD02 = *on;
056400150424           exsr  sr_InzD02;
056500150424           $InzD02 = *off;
056600150424         endif;
056700170414
056800150424         F9Attivo  = *on;
056900150424         F12Attivo = *on;
057000170421         F21Attivo = *on;
057100170414         $RlUpAttivo = ( wPage < 4 );
057200170414         $RlDnAttivo = ( wPage > 1 );
057300150430         $VisualDate = *on;
057400150424
057500150424         // -?Emissione testata e piede?
057600170426         write  LRY05T1;
057700170426         write  LRY05P1;
057800150424
057900150424         // -?Emissione videata?
058000170426         //*-if  $Video = 'D2';
058100170426         //*-  exfmt  LRY05D2;
058200170426         //*-else;
058300170426         //*-  exfmt  LRY05D3;
058400170426
058500170426         exfmt  LRY05D2;
058600150424
058700150424         clear  errGenerico;
058800150424         clear  errMessage;
058900150424         clear  V1Dmsg;
059000150424
059100150424         Select;
059200150424
059300150424           // -?F3=Fine?
059400150424           when  dsp_aid = c_F03;
059500150424             $Fine  = *on;
059600150424
059700150429           // -?F9=Visualizzazione file TIIDS00F?
059800150429           when  dsp_aid = c_F09;
059900150429             Qcmd = 'DSPDBF file(TIIDS00F)';
060000150429             exsr  sr_ExecCmd;
060100150424
060200150424           // -?F12=Ritorno?
060300150424           when  dsp_aid = c_F12;
060400150424             $Video = 'D1';
060500170421
060600170426           //*-// -?F21=Altra videata?
060700170426           //*-when  dsp_aid = c_F21  and  $Video = 'D2';
060800170426           //*-  $Video = 'D3';
060900170426           //*-when  dsp_aid = c_F21  and  $Video = 'D3';
061000170426           //*-  $Video = 'D2';
061100170413
061200170413           // -?RollUp?
061300170414           when  dsp_aid = c_RollUp    and  wPage < 4;
061400170414             wPage += 1;
061500170413             exsr  sr_DatiVideo;
061600170413
061700170413           // -?RollDown?
061800170414           when  dsp_aid = c_RollDown  and  wPage > 1;
061900170414             wPage -= 1;
062000170413             exsr  sr_DatiVideo;
062100150424
062200150424         EndSl;
062300150424
062400150424       ENDSR;
062500150424
062600150424       //--------------------------------------------------------------
062700150424       //?Inizializzazione videata D02.                                ?
062800150424       //--------------------------------------------------------------
062900150424       BEGSR  sr_InzD02;
063000150424
063100170426         clear  LRY05D2;
063200170426         //*-clear  LRY05D3;
063300150424
063400170426         reset  LRY05T2_ds;
063500170426         reset  LRY05T2_P_ds;
063600170426         reset  LRY05T2_A_ds;
063700170426         reset  LRY05T2_B_ds;
063800170426         reset  LRY05T2_G_ds;
063900150424
064000150424         // -?Calcolo Totali complessivi (in base alle parzializzazioni)?
064100150424         exsr  sr_CalcTot;
064200170413
064300170413         // -?Impostazione Totali e relative percentuali a video?
064400170413         //  ?(a seconda della pagina visualizzata)?
064500170414         wPage = 1;
064600170413         exsr  sr_DatiVideo;
064700150424
064800150430         // -?Impostazione ultimi dati a video:?
064900150430         // ·?Parzializzazioni?
065000150430         if  V1Cfgs <> *zero  or  V1CtipoCli <> *blank;
065100150430           VD2parz = 'PARZIALIZZAZIONI:';
065200150430           if  V1Cfgs <> *zero;
065300150430             VD2parz = %trimR( VD2parz ) +
065400150430                       ' Filiale Arrivo = ' + %editc( V1Cfgs : 'X' );
065500150430           endif;
065600150430           if  V1Cfgs <> *zero  and  V1CtipoCli <> *blank;
065700150430             VD2parz = %trimR( VD2parz) + ' /';
065800150430           endif;
065900150430           if  V1CtipoCli <> *blank;
066000150430             VD2parz = %trimR( VD2parz) +
066100150430                       ' Tipo Cliente = "' + V1CtipoCli + '"';
066200150430           endif;
066300150430         endif;
066400150424
066500150424       ENDSR;
066600170413
066700170413       //--------------------------------------------------------------
066800170413       //?Impostazione campi della singola videata D02.                ?
066900170413       //--------------------------------------------------------------
067000170413       BEGSR  sr_DatiVideo;
067100170413
067200170426         clear  LRY05D2;
067300170414
067400170414         // -?Descrizione dei totali visualizzati?
067500170414         VH2page = wPage;
067600170414         select;
067700170414           when  VH2page = 1;
067800170414             V1Ttxt = '         TOTALE          ';
067900170414           when  VH2page = 2;
068000170414             V1Ttxt = '       DA PARTIRE        ';
068100170414           when  VH2page = 3;
068200170414             V1Ttxt = ' IN ARRIVO - TENTATIVI 0 ';
068300170414           when  VH2page = 4;
068400170414             V1Ttxt = 'IN ARRIVO - TENTATIVI > 0';
068500170414         endsl;
068600170413
068700170413         // -?N° pagina a video?
068800170414         VD2pag = 'Pag. ' + %editc( VH2page : 'X' ) + ' di 4';
068900170413
069000170413         // -?"Segue..." / "Fine."?
069100170414         if  VH2page < 4;
069200170413           VD2seg  = 'Segue...';
069300170413         else;
069400170413           VD2seg  = '   Fine.';
069500170413         endif;
069600170413
069700170413         // -?Impostazione dati da visualizzare?
069800170414         Select;
069900170414           // -?Tot. Spedizioni (generale)?
070000170414           When  VH2page = 1;
070100170426             LRY05T2_ds = LRY05T2_G_ds;
070200170413           // -?Tot. Spedizioni "da partire" (Stato = "P")?
070300170413           when  VH2page = 2;
070400170426             LRY05T2_ds = LRY05T2_P_ds;
070500170413           // -?Tot. Spedizioni "in arrivo - tentativi 0" (Stato = "A")?
070600170413           when  VH2page = 3;
070700170426             LRY05T2_ds = LRY05T2_A_ds;
070800170413           // -?Tot. Spedizioni "in arrivo - tentativi >0" (Stato = " ")?
070900170413           when  VH2page = 4;
071000170426             LRY05T2_ds = LRY05T2_B_ds;
071100170413         endsl;
071200170413
071300170413         // -?Date elaborate?
071400170426         //*·if  LRY05T2_ds.w02ddi  > *zero  and
071500170426         //*·    LRY05T2_ds.w02ddi  < *hival;
071600170426         //*·  wDate_Eur = %date( LRY05T2_ds.w02ddi : *iso );
071700170426         if  LRY05T2_G_ds.w02ddi  > *zero  and
071800170426             LRY05T2_G_ds.w02ddi  < *hival;
071900170426           wDate_Eur = %date( LRY05T2_G_ds.w02ddi : *iso );
072000170413           V1Tddi    = %dec( wDate_Eur );
072100170413         endif;
072200170426         //*·if  LRY05T2_ds.w02ddf  > *zero;
072300170426         //*·  wDate_Eur = %date( LRY05T2_ds.w02ddf : *iso );
072400170426         if  LRY05T2_G_ds.w02ddf  > *zero;
072500170426           wDate_Eur = %date( LRY05T2_G_ds.w02ddf : *iso );
072600170413           V1Tddf    = %dec( wDate_Eur );
072700170413         endif;
072800170413
072900170414         // -?Totali x Appuntamento?
073000170426         VD2tap = LRY05T2_ds.wD2tap;
073100170426         VD2dap = LRY05T2_ds.wD2dap;
073200170426         VD2map = LRY05T2_ds.wD2map;
073300170413         // -?Percentuali x Appuntamento?
073400170426         if  LRY05T2_ds.wD2ptap <> *zero;
073500170426           VD2ptap = %editc( LRY05T2_ds.wD2ptap : '3' ) + '%';
073600170413         endif;
073700170426         if  LRY05T2_ds.wD2pdap <> *zero;
073800170426           VD2pdap = %editc( LRY05T2_ds.wD2pdap : '3' ) + '%';
073900170413         endif;
074000170426         if  LRY05T2_ds.wD2pmap <> *zero;
074100170426           VD2pmap = %editc( LRY05T2_ds.wD2pmap : '3' ) + '%';
074200170413         endif;
074300170413
074400170414         // -?Totali x Fermo Depositp?
074500170426         VD2tfd = LRY05T2_ds.wD2tfd;
074600170426         VD2dfd = LRY05T2_ds.wD2dfd;
074700170426         VD2mfd = LRY05T2_ds.wD2mfd;
074800170413         // -?Percentuali x Fermo Deposito?
074900170426         if  LRY05T2_ds.wD2ptfd <> *zero;
075000170426           VD2ptfd = %editc( LRY05T2_ds.wD2ptfd : '3' ) + '%';
075100170413         endif;
075200170426         if  LRY05T2_ds.wD2pdfd <> *zero;
075300170426           VD2pdfd = %editc( LRY05T2_ds.wD2pdfd : '3' ) + '%';
075400170413         endif;
075500170426         if  LRY05T2_ds.wD2pmfd <> *zero;
075600170426           VD2pmfd = %editc( LRY05T2_ds.wD2pmfd : '3' ) + '%';
075700170413         endif;
075800170421
075900170421
076000170421         // -?Totali x Disposizioni Cambio Indirizzo + Appuntamento?
076100170426         VD2tciA = LRY05T2_ds.wD2tciA;
076200170426         VD2dciA = LRY05T2_ds.wD2dciA;
076300170426         VD2mciA = LRY05T2_ds.wD2mciA;
076400170421         // -?Percentuali x Disposiz. Cambio Indirizzo + Appuntamento?
076500170426         if  LRY05T2_ds.wD2ptciA <> *zero;
076600170426           VD2ptciA = %editc( LRY05T2_ds.wD2ptciA : '3' ) + '%';
076700170421         endif;
076800170426         if  LRY05T2_ds.wD2pdciA <> *zero;
076900170426           VD2pdciA = %editc( LRY05T2_ds.wD2pdciA : '3' ) + '%';
077000170421         endif;
077100170426         if  LRY05T2_ds.wD2pmciA <> *zero;
077200170426           VD2pmciA = %editc( LRY05T2_ds.wD2pmciA : '3' ) + '%';
077300170421         endif;
077400170421
077500170421         // -?Totali x Disposizioni Cambio Indirizzo (No Appunt, No Dirott)?
077600170426         VD2tci_ = LRY05T2_ds.wD2tci_;
077700170426         VD2dci_ = LRY05T2_ds.wD2dci_;
077800170426         VD2mci_ = LRY05T2_ds.wD2mci_;
077900170421         // -?Percentuali x Disposiz. Cambio Indirizzo (No Appunt, No Dirott)?
078000170426         if  LRY05T2_ds.wD2ptci_ <> *zero;
078100170426           VD2ptci_ = %editc( LRY05T2_ds.wD2ptci_ : '3' ) + '%';
078200170421         endif;
078300170426         if  LRY05T2_ds.wD2pdci_ <> *zero;
078400170426           VD2pdci_ = %editc( LRY05T2_ds.wD2pdci_ : '3' ) + '%';
078500170421         endif;
078600170426         if  LRY05T2_ds.wD2pmci_ <> *zero;
078700170426           VD2pmci_ = %editc( LRY05T2_ds.wD2pmci_ : '3' ) + '%';
078800170421         endif;
078900170421
079000170421         // -?Totali x Disposizioni Cambio Indirizzo + Dirottamento?
079100170426         VD2tciD = LRY05T2_ds.wD2tciD;
079200170426         VD2dciD = LRY05T2_ds.wD2dciD;
079300170426         VD2mciD = LRY05T2_ds.wD2mciD;
079400170421         // -?Percentuali x Disposiz. Cambio Indirizzo + Dirottamento?
079500170426         if  LRY05T2_ds.wD2ptciD <> *zero;
079600170426           VD2ptciD = %editc( LRY05T2_ds.wD2ptciD : '3' ) + '%';
079700170421         endif;
079800170426         if  LRY05T2_ds.wD2pdciD <> *zero;
079900170426           VD2pdciD = %editc( LRY05T2_ds.wD2pdciD : '3' ) + '%';
080000170421         endif;
080100170426         if  LRY05T2_ds.wD2pmciD <> *zero;
080200170426           VD2pmciD = %editc( LRY05T2_ds.wD2pmciD : '3' ) + '%';
080300170421         endif;
080400170421
080500170413
080600170414         // -?Totali Generali?
080700170426         VD2tge = LRY05T2_ds.wD2tge;
080800170426         VD2tde = LRY05T2_ds.wD2tde;
080900170426         VD2tmi = LRY05T2_ds.wD2tmi;
081000170421         // -?Percentuali x Totali Generali?
081100170426         //*·if  LRY05T2_ds.wD2ptge <> *zero;
081200170426         //*·  VD2ptge = %editc( LRY05T2_ds.wD2ptge : '3' ) + '%';
081300170414         //*·endif;
081400170426         if  LRY05T2_ds.wD2ptde <> *zero;
081500170426           VD2ptde = %editc( LRY05T2_ds.wD2ptde : '3' ) + '%';
081600170413         endif;
081700170426         if  LRY05T2_ds.wD2ptmi <> *zero;
081800170426           VD2ptmi = %editc( LRY05T2_ds.wD2ptmi : '3' ) + '%';
081900170413         endif;
082000170413
082100170413
082200170426         //*-// -?Impostazione ALTRI dati nella videata D3?
082300170421
082400170426         //*-// -?Totali x Cambio Indirizzo?
082500170426         //*-VD2tci = LRY05T2_ds.wD2tci;
082600170426         //*-VD2dci = LRY05T2_ds.wD2dci;
082700170426         //*-VD2mci = LRY05T2_ds.wD2mci;
082800170426         //*-// -?Percentuali x Cambio Indirizzo?
082900170426         //*-if  LRY05T2_ds.wD2ptci <> *zero;
083000170426         //*-  VD2ptci = %editc( LRY05T2_ds.wD2ptci : '3' ) + '%';
083100170426         //*-endif;
083200170426         //*-if  LRY05T2_ds.wD2pdci <> *zero;
083300170426         //*-  VD2pdci = %editc( LRY05T2_ds.wD2pdci : '3' ) + '%';
083400170426         //*-endif;
083500170426         //*-if  LRY05T2_ds.wD2pmci <> *zero;
083600170426         //*-  VD2pmci = %editc( LRY05T2_ds.wD2pmci : '3' ) + '%';
083700170426         //*-endif;
083800170421
083900170421
084000170426         //*-// -?Disposizioni CAMBIO INDIRIZZO: DIROTTAMENTO?
084100170426         //*-VD2tdi = LRY05T2_ds.wD2tciD;
084200170426         //*-VD2ddi = LRY05T2_ds.wD2dciD;
084300170426         //*-VD2mdi = LRY05T2_ds.wD2mciD;
084400170426         //*-//*·if  LRY05T2_ds.wD2ptciD <> *zero;
084500170426         //*-//*·  VD2ptdi = %editc( LRY05T2_ds.wD2ptciD : '3' ) + '%';
084600170426         //*-//*·endif;
084700170426         //*-if  LRY05T2_ds.wD2pdciD <> *zero;
084800170426         //*-  VD2pddi = %editc( LRY05T2_ds.wD2pdciD : '3' ) + '%';
084900170426         //*-endif;
085000170426         //*-if  LRY05T2_ds.wD2pmciD <> *zero;
085100170426         //*-  VD2pmdi = %editc( LRY05T2_ds.wD2pmciD : '3' ) + '%';
085200170426         //*-endif;
085300170413
085400170426         //*-// -?Disposizioni CAMBIO INDIRIZZO: NO DIROTTAMENTO?
085500170426         //*-VD2tnd = LRY05T2_ds.wD2tciA + LRY05T2_ds.wD2tci_;
085600170426         //*-VD2dnd = LRY05T2_ds.wD2dciA + LRY05T2_ds.wD2dci_;
085700170426         //*-VD2mnd = LRY05T2_ds.wD2mciA + LRY05T2_ds.wD2mci_;
085800170426         //*-if  VD2tnd <> *zero;
085900170426         //*-  eval(H) wPerc = ( VD2dnd * 100 ) / VD2tnd;
086000170426         //*-  VD2pdnd = %editc( wPerc : '3' ) + '%';
086100170426         //*-  eval(H) wPerc = ( VD2mnd * 100 ) / VD2tnd;
086200170426         //*-  VD2pmnd = %editc( wPerc : '3' ) + '%';
086300170426         //*-endif;
086400170413
086500170426         //*-// -?Disposizioni CAMBIO INDIRIZZO: NO DIR. con APPUNTAMENTO?
086600170426         //*-VD2tdca = LRY05T2_ds.wD2tciA;
086700170426         //*-VD2ddca = LRY05T2_ds.wD2dciA;
086800170426         //*-VD2mdca = LRY05T2_ds.wD2mciA;
086900170426         //*-if  VD2tdcA <> *zero;
087000170426         //*-  eval(H) wPerc = ( VD2ddcA * 100 ) / VD2tdca;
087100170426         //*-  VD2pddcA = %editc( wPerc : '3' ) + '%';
087200170426         //*-  eval(H) wPerc = ( VD2mdcA * 100 ) / VD2tdca;
087300170426         //*-  VD2pmdcA = %editc( wPerc : '3' ) + '%';
087400170426         //*-endif;
087500170413
087600170413       ENDSR;
087700150424
087800150424       //--------------------------------------------------------------
087900150424       //?Calcolo totali per videata D02.                              ?
088000150424       //--------------------------------------------------------------
088100150424       BEGSR  sr_CalcTot;
088200150424
088300150424         clear  $EoF;
088400150424
088500150424         // -?Preparazione SQL per TOTALI?
088600170421         exsr  sr_PrepSQL;
088700150424
088800150424         // -?Apertura del cursore per TOTALI?
088900170421         exsr  sr_OpenCursor;
089000150424
089100150424         // -?Estrazione TOTALI?
089200150424         DoW  $EoF = *off;
089300170421           exsr  sr_ReadCursor;
089400150424         EndDo;
089500150424
089600150424         // -?Chiusura del cursore per TOTALI?
089700170421         exsr  sr_CloseCursor;
089800170413
089900170413         // -?Calcolo percentuali TOTALI?
090000170418         // -?Totali "P" = Da Partire?
090100170426         LRY05T2_ds   = LRY05T2_P_ds;
090200170418         exsr  sr_CalcPerc;
090300170426         LRY05T2_P_ds = LRY05T2_ds;
090400170418         // -?Totali "A" = In Arrivo - Tentativi 0?
090500170426         LRY05T2_ds = LRY05T2_A_ds;
090600170418         exsr  sr_CalcPerc;
090700170426         LRY05T2_A_ds = LRY05T2_ds;
090800170418         // -?Totali "_" = In Arrivo - Tentativi > 0?
090900170426         LRY05T2_ds = LRY05T2_B_ds;
091000170418         exsr  sr_CalcPerc;
091100170426         LRY05T2_B_ds = LRY05T2_ds;
091200170418         // -?Totali generali?
091300170426         LRY05T2_ds = LRY05T2_G_ds;
091400170413         exsr  sr_CalcPerc;
091500170426         LRY05T2_G_ds = LRY05T2_ds;
091600150424
091700150424       ENDSR;
091800150430
091900150430       //--------------------------------------------------------------
092000150430       //?Preparazione SQL per calcolo TOTALI.                         ?
092100150430       //--------------------------------------------------------------
092200170421       BEGSR  sr_PrepSQL;
092300150430
092400150430         // -?Preparazione SQL per calcolo TOTALI?
092500150430         clear  wSQL;
092600150430         wSQL = 'select min(IDSinsData), max(IDSinsData), +
092700170421                        IDSstato, IDStipoDis, IDSsnApp, IDSsnDir, +
092800170421                        IDStipoCli, sum(IDStotIDC) +
092900150430                   from TIIDS00F';
093000150430         $First = *on;
093100150430         exsr  sr_SQLaddSelez;
093200170421         wSQL  += ' group by IDSstato, IDStipoDis, +
093300170421                             IDSsnApp, IDSsnDir, IDStipoCli +
093400170421                    order by IDSstato, IDStipoDis, +
093500170421                             IDSsnApp, IDSsnDir, IDStipoCli +
093600150430                      for fetch only';
093700150430
093800150430         exec sql   prepare S1   from :wSQL;
093900150430
094000150430         // -?Dichiarazione cursore?
094100150430         exec sql   declare C1   cursor for S1;
094200150430         if  SQLcode < *zero;
094300150430           $EoF = *on;
094400150430           exsr  sr_PrintErrSQL;
094500150430         endif;
094600150430
094700150430       ENDSR;
094800150430
094900150430       //--------------------------------------------------------------
095000150430       //?Apertura cursore per calcolo TOTALI.                         ?
095100150430       //--------------------------------------------------------------
095200170421       BEGSR  sr_OpenCursor;
095300150430
095400150430         // -?Apertura del cursore?
095500150430         exec sql   open C1;
095600150430
095700150430         if  SQLcode < *zero;
095800150430           $EoF = *on;
095900150430           exsr  sr_PrintErrSQL;
096000150430         endif;
096100150430
096200150430       ENDSR;
096300150430
096400150430       //--------------------------------------------------------------
096500150430       //?Lettura cursore per calcolo TOTALI.                          ?
096600150430       //--------------------------------------------------------------
096700170421       BEGSR  sr_ReadCursor;
096800150430
096900170413         clear  wStato;
097000150430         clear  wTipoDis;
097100150430         clear  wTipoCli;
097200150430         clear  wTOT;
097300150430
097400150430         exec sql   fetch next   from C1   into :wDataIni, :wDataFin,
097500170421                                                :wStato, :wTipoDis,
097600170421                                                :wSNapp, :wSNdir,
097700170421                                                :wTipoCli, :wTOT;
097800150430
097900150430         Select;
098000150430
098100150430           // -?Fine File?
098200150430           When  SQLcode = 100;
098300150430             $EoF = *on;
098400150430
098500150430           // -?Errore SQL?
098600150430           When  SQLcode < *zero;
098700150430             $EoF = *on;
098800150430             exsr  sr_PrintErrSQL;
098900150430
099000150430           // -?Calcolo Totali?
099100150430           Other;
099200170413             select;
099300170413               when  wStato = 'P';
099400170426                 LRY05T2_ds = LRY05T2_P_ds;
099500170413               when  wStato = 'A';
099600170426                 LRY05T2_ds = LRY05T2_A_ds;
099700170413               when  wStato = *blank;
099800170426                 LRY05T2_ds = LRY05T2_B_ds;
099900170413             endsl;
100000170421             exsr  sr_CalcTot_Parz;
100100170421             exsr  sr_CalcTot_Gen;
100200170413             select;
100300170413               when  wStato = 'P';
100400170426                 LRY05T2_P_ds = LRY05T2_ds;
100500170413               when  wStato = 'A';
100600170426                 LRY05T2_A_ds = LRY05T2_ds;
100700170413               when  wStato = *blank;
100800170426                 LRY05T2_B_ds = LRY05T2_ds;
100900170413             endsl;
101000150430
101100150430         EndSl;
101200150430
101300150430       ENDSR;
101400150430
101500150430       //--------------------------------------------------------------
101600150430       //?Chiusura cursore per calcolo TOTALI.                         ?
101700150430       //--------------------------------------------------------------
101800170421       BEGSR  sr_CloseCursor;
101900150430
102000150430         // -?Chiusura del cursore?
102100150430         exec sql   close C1;
102200150430
102300150430       ENDSR;
102400170413
102500170413       //--------------------------------------------------------------
102600170414       //?Calcolo Totali parziali "Da Partire" / "In Arrivo".          ?
102700170413       //--------------------------------------------------------------
102800170421       BEGSR  sr_CalcTot_Parz;
102900170413
103000170413         // -?Data iniziale?
103100170426         if  LRY05T2_ds.W02ddi = *zero  or
103200170426             LRY05T2_ds.W02ddi > wDataIni;
103300170426           LRY05T2_ds.W02ddi = wDataIni;
103400170413         endif;
103500170413
103600170413         // -?Data finale?
103700170426         if  LRY05T2_ds.W02ddf < wDataFin;
103800170426           LRY05T2_ds.W02ddf = wDataFin;
103900170413         endif;
104000170413
104100170413         Select;
104200170413
104300170413           // -?Disposizione "1" = Appuntamento?
104400170413           When  wTipoDis = c_TipoDis1;
104500170426             LRY05T2_ds.wD2tap += wTOT;
104600170413             if  wTipoCli = 'D';
104700170426               LRY05T2_ds.wD2dap += wTOT;     // -?Richieste dal Dest.?
104800170413             else;
104900170426               LRY05T2_ds.wD2map += wTOT;     // -?Richieste dal Mitt.?
105000170413             endif;
105100170413
105200170413           // -?Disposizione "2" = Fermo Deposito?
105300170413           When  wTipoDis = c_TipoDis2;
105400170426             LRY05T2_ds.wD2tfd += wTOT;
105500170413             if  wTipoCli = 'D';
105600170426               LRY05T2_ds.wD2dfd += wTOT;     // -?Richieste dal Dest.?
105700170413             else;
105800170426               LRY05T2_ds.wD2mfd += wTOT;     // -?Richieste dal Mitt.?
105900170413             endif;
106000170413
106100170413           // -?Disposizione "3" = Cambio Indirizzo?
106200170413           When  wTipoDis = c_TipoDis3;
106300170426             //*-LRY05T2_ds.wD2tci += wTOT;
106400170426             //*-if  wTipoCli = 'D';
106500170426             //*-  LRY05T2_ds.wD2dci += wTOT;     // -?Richieste dal Dest.?
106600170426             //*-else;
106700170426             //*-  LRY05T2_ds.wD2mci += wTOT;     // -?Richieste dal Mitt.?
106800170426             //*-endif;
106900170421             Select;
107000170421               // -?Cambio Indirizzo + Dirottamento?
107100170421               When  wSNdir = 'S';
107200170426                 LRY05T2_ds.wD2tciD += wTOT;
107300170421                 if  wTipoCli = 'D';
107400170426                   LRY05T2_ds.wD2dciD += wTOT;     // -?Richieste dal Dest.?
107500170421                 else;
107600170426                   LRY05T2_ds.wD2mciD += wTOT;     // -?Richieste dal Mitt.?
107700170421                 endif;
107800170421               // -?Cambio Indirizzo + Appuntamento?
107900170421               When  wSNapp = 'S';
108000170426                 LRY05T2_ds.wD2tciA += wTOT;
108100170421                 if  wTipoCli = 'D';
108200170426                   LRY05T2_ds.wD2dciA += wTOT;     // -?Richieste dal Dest.?
108300170421                 else;
108400170426                   LRY05T2_ds.wD2mciA += wTOT;     // -?Richieste dal Mitt.?
108500170421                 endif;
108600170421               // -?Cambio Indirizzo (No Dirottamento, No Appuntamento)?
108700170421               Other;
108800170426                 LRY05T2_ds.wD2tci_ += wTOT;
108900170421                 if  wTipoCli = 'D';
109000170426                   LRY05T2_ds.wD2dci_ += wTOT;     // -?Richieste dal Dest.?
109100170421                 else;
109200170426                   LRY05T2_ds.wD2mci_ += wTOT;     // -?Richieste dal Mitt.?
109300170421                 endif;
109400170421             EndSl;
109500170413
109600170413         EndSl;
109700170413
109800170413         // -?Totale generale?
109900170426         LRY05T2_ds.wD2tge += wTOT;
110000170413         if  wTipoCli = 'D';
110100170426           LRY05T2_ds.wD2tde += wTOT;    // -?Richieste dal Destinat.?
110200170413         else;
110300170426           LRY05T2_ds.wD2tmi += wTOT;    // -?Richieste dal Mittente?
110400170413         endif;
110500170413
110600170413       ENDSR;
110700170413
110800170413       //--------------------------------------------------------------
110900170413       //?Calcolo Totali Generali.                                     ?
111000170413       //--------------------------------------------------------------
111100170421       BEGSR  sr_CalcTot_Gen;
111200170414
111300170414         // -?Data iniziale?
111400170426         if  LRY05T2_G_ds.W02ddi = *zero  or
111500170426             LRY05T2_G_ds.W02ddi > wDataIni;
111600170426           LRY05T2_G_ds.W02ddi = wDataIni;
111700170414         endif;
111800170414
111900170414         // -?Data finale?
112000170426         if  LRY05T2_G_ds.W02ddf < wDataFin;
112100170426           LRY05T2_G_ds.W02ddf = wDataFin;
112200170414         endif;
112300170414
112400170414         Select;
112500170414
112600170414           // -?Disposizione "1" = Appuntamento?
112700170414           When  wTipoDis = c_TipoDis1;
112800170426             LRY05T2_G_ds.wD2tap += wTOT;
112900170414             if  wTipoCli = 'D';
113000170426               LRY05T2_G_ds.wD2dap += wTOT;   // -?Richieste dal Dest.?
113100170414             else;
113200170426               LRY05T2_G_ds.wD2map += wTOT;   // -?Richieste dal Mitt.?
113300170414             endif;
113400170414
113500170414           // -?Disposizione "2" = Fermo Deposito?
113600170414           When  wTipoDis = c_TipoDis2;
113700170426             LRY05T2_G_ds.wD2tfd += wTOT;
113800170414             if  wTipoCli = 'D';
113900170426               LRY05T2_G_ds.wD2dfd += wTOT;   // -?Richieste dal Dest.?
114000170414             else;
114100170426               LRY05T2_G_ds.wD2mfd += wTOT;   // -?Richieste dal Mitt.?
114200170414             endif;
114300170414
114400170414           // -?Disposizione "3" = Cambio Indirizzo?
114500170414           When  wTipoDis = c_TipoDis3;
114600170426             //*-LRY05T2_G_ds.wD2tci += wTOT;
114700170426             //*-if  wTipoCli = 'D';
114800170426             //*-  LRY05T2_G_ds.wD2dci += wTOT;   // -?Richieste dal Dest.?
114900170426             //*-else;
115000170426             //*-  LRY05T2_G_ds.wD2mci += wTOT;   // -?Richieste dal Mitt.?
115100170426             //*-endif;
115200170421             Select;
115300170421               // -?Cambio Indirizzo + Dirottamento?
115400170421               When  wSNdir = 'S';
115500170426                 LRY05T2_G_ds.wD2tciD += wTOT;
115600170421                 if  wTipoCli = 'D';
115700170426                   LRY05T2_G_ds.wD2dciD += wTOT;   // -?Richieste dal Dest.?
115800170421                 else;
115900170426                   LRY05T2_G_ds.wD2mciD += wTOT;   // -?Richieste dal Mitt.?
116000170421                 endif;
116100170421               // -?Cambio Indirizzo + Appuntamento?
116200170421               When  wSNapp = 'S';
116300170426                 LRY05T2_G_ds.wD2tciA += wTOT;
116400170421                 if  wTipoCli = 'D';
116500170426                   LRY05T2_G_ds.wD2dciA += wTOT;   // -?Richieste dal Dest.?
116600170421                 else;
116700170426                   LRY05T2_G_ds.wD2mciA += wTOT;   // -?Richieste dal Mitt.?
116800170421                 endif;
116900170421               // -?Cambio Indirizzo (No Dirottamento, No Appuntamento)?
117000170421               Other;
117100170426                 LRY05T2_G_ds.wD2tci_ += wTOT;
117200170421                 if  wTipoCli = 'D';
117300170426                   LRY05T2_G_ds.wD2dci_ += wTOT;   // -?Richieste dal Dest.?
117400170421                 else;
117500170426                   LRY05T2_G_ds.wD2mci_ += wTOT;   // -?Richieste dal Mitt.?
117600170421                 endif;
117700170421             EndSl;
117800170414
117900170414         EndSl;
118000170414
118100170414         // -?Totale generale?
118200170426         LRY05T2_G_ds.wD2tge += wTOT;
118300170414         if  wTipoCli = 'D';
118400170426           LRY05T2_G_ds.wD2tde += wTOT;  // -?Richieste dal Destinat.?
118500170414         else;
118600170426           LRY05T2_G_ds.wD2tmi += wTOT;  // -?Richieste dal Mittente?
118700170414         endif;
118800170413
118900170413       ENDSR;
119000150416
119100150416       //--------------------------------------------------------------
119200150416       //?Aggiunta delle selezioni impostate a video alla stringa SQL. ?
119300150416       //--------------------------------------------------------------
119400150416       BEGSR  sr_SQLaddSelez;
119500150416
119600150416         If  wInsDtI > *zero  or  wInsDtF > *zero;
119700150416           if  $First;
119800150416             wSQL  += ' where ';
119900150416             $First = *off;
120000150416           else;
120100150416             wSQL  += ' and ';
120200150416           endif;
120300150416           wSQL += 'IDSinsData between ' + %trim( %editc( wInsDtI : '3' ) ) +
120400150416                                 ' and ' + %trim( %editc( wInsDtF : '3' ) );
120500150416         EndIf;
120600150416
120700150416         If  V1Cfgs <> *zero;
120800150416           if  $First;
120900150416             wSQL  += ' where ';
121000150416             $First = *off;
121100150416           else;
121200150416             wSQL  += ' and ';
121300150416           endif;
121400150416           wSQL += 'IDSfgs = ' + %trim( %editc( V1Cfgs : '3' ) );
121500150416         EndIf;
121600150416
121700150416         If  V1CtipoCli <> *blank;
121800150416           if  $First;
121900150416             wSQL  += ' where ';
122000150416             $First = *off;
122100150416           else;
122200150416             wSQL  += ' and ';
122300150416           endif;
122400150416           wSQL += 'IDStipoCli = ''' + V1CtipoCli + '''';
122500150416         EndIf;
122600150416
122700150416       ENDSR;
122800170414
122900170414       //--------------------------------------------------------------
123000170414       //?Calcolo Percentuali per le Disposizioni di Consegna reperite.?
123100170414       //--------------------------------------------------------------
123200170414       BEGSR  sr_CalcPerc;
123300170414
123400170426         if  LRY05T2_ds.wD2tge = *zero;
123500170421           leavesr;
123600170421         endif;
123700170421
123800170421
123900170421         // -?Percentuale x Appuntamento?
124000170426         eval(H) LRY05T2_ds.wD2ptap  = ( LRY05T2_ds.wD2tap * 100 ) /
124100170426                                         LRY05T2_ds.wD2tge;
124200170426         if  LRY05T2_ds.wD2tap <> *zero;
124300170426           eval(H) LRY05T2_ds.wD2pdap = ( LRY05T2_ds.wD2dap * 100 ) /
124400170426                                          LRY05T2_ds.wD2tap;
124500170426           eval(H) LRY05T2_ds.wD2pmap = ( LRY05T2_ds.wD2map * 100 ) /
124600170426                                          LRY05T2_ds.wD2tap;
124700170421         endif;
124800170421
124900170421         // -?Percentuale x Fermo Deposito?
125000170426         eval(H) LRY05T2_ds.wD2ptfd  = ( LRY05T2_ds.wD2tfd * 100 ) /
125100170426                                         LRY05T2_ds.wD2tge;
125200170426         if  LRY05T2_ds.wD2tfd <> *zero;
125300170426           eval(H) LRY05T2_ds.wD2pdfd = ( LRY05T2_ds.wD2dfd * 100 ) /
125400170426                                          LRY05T2_ds.wD2tfd;
125500170426           eval(H) LRY05T2_ds.wD2pmfd = ( LRY05T2_ds.wD2mfd * 100 ) /
125600170426                                          LRY05T2_ds.wD2tfd;
125700170421         endif;
125800170421
125900170426         //*-// -?Percentuale x Cambio Indirizzo?
126000170426         //*-eval(H) LRY05T2_ds.wD2ptci  = ( LRY05T2_ds.wD2tci * 100 ) /
126100170426         //*-                                LRY05T2_ds.wD2tge;
126200170426         //*-if  LRY05T2_ds.wD2tci <> *zero;
126300170426         //*-  eval(H) LRY05T2_ds.wD2pdci = ( LRY05T2_ds.wD2dci * 100 ) /
126400170426         //*-                                 LRY05T2_ds.wD2tci;
126500170426         //*-  eval(H) LRY05T2_ds.wD2pmci = ( LRY05T2_ds.wD2mci * 100 ) /
126600170426         //*-                                 LRY05T2_ds.wD2tci;
126700170426         //*-endif;
126800170421
126900170421         // -?Percentuale x Cambio Indirizzo + Appuntamento?
127000170426         eval(H) LRY05T2_ds.wD2ptciA = ( LRY05T2_ds.wD2tciA * 100 ) /
127100170426                                         LRY05T2_ds.wD2tge;
127200170426         if  LRY05T2_ds.wD2tciA <> *zero;
127300170426           eval(H) LRY05T2_ds.wD2pdciA = ( LRY05T2_ds.wD2dciA * 100 ) /
127400170426                                           LRY05T2_ds.wD2tciA;
127500170426           eval(H) LRY05T2_ds.wD2pmciA = ( LRY05T2_ds.wD2mciA * 100 ) /
127600170426                                           LRY05T2_ds.wD2tciA;
127700170421         endif;
127800170421
127900170421         // -?Percentuale x Cambio Indirizzo (No Appuntam, No Dirottam)?
128000170426         eval(H) LRY05T2_ds.wD2ptci_ = ( LRY05T2_ds.wD2tci_ * 100 ) /
128100170426                                         LRY05T2_ds.wD2tge;
128200170426         if  LRY05T2_ds.wD2tci_ <> *zero;
128300170426           eval(H) LRY05T2_ds.wD2pdci_ = ( LRY05T2_ds.wD2dci_ * 100 ) /
128400170426                                           LRY05T2_ds.wD2tci_;
128500170426           eval(H) LRY05T2_ds.wD2pmci_ = ( LRY05T2_ds.wD2mci_ * 100 ) /
128600170426                                           LRY05T2_ds.wD2tci_;
128700170421         endif;
128800170421
128900170421         // -?Percentuale x Cambio Indirizzo + Dirottamento?
129000170426         eval(H) LRY05T2_ds.wD2ptciD = ( LRY05T2_ds.wD2tciD * 100 ) /
129100170426                                         LRY05T2_ds.wD2tge;
129200170426         if  LRY05T2_ds.wD2tciD <> *zero;
129300170426           eval(H) LRY05T2_ds.wD2pdciD = ( LRY05T2_ds.wD2dciD * 100 ) /
129400170426                                           LRY05T2_ds.wD2tciD;
129500170426           eval(H) LRY05T2_ds.wD2pmciD = ( LRY05T2_ds.wD2mciD * 100 ) /
129600170426                                           LRY05T2_ds.wD2tciD;
129700170421         endif;
129800170421
129900170421         // -?Percentuale x Totale Generale?
130000170426         //*·eval(H) LRY05T2_ds.wD2ptge = ( LRY05T2_ds.wD2tge * 100 ) /
130100170426         //*·                               LRY05T2_ds.wD2tge;
130200170426         eval    LRY05T2_ds.wD2ptge = 100;
130300170421         // -?Percentuale x Totale Generale / Destinatari?
130400170426         eval(H) LRY05T2_ds.wD2ptde = ( LRY05T2_ds.wD2tde * 100 ) /
130500170426                                        LRY05T2_ds.wD2tge;
130600170421         // -?Percentuale x Totale Generale / Mittenti?
130700170426         eval(H) LRY05T2_ds.wD2ptmi = ( LRY05T2_ds.wD2tmi * 100 ) /
130800170426                                        LRY05T2_ds.wD2tge;
130900170414
131000170414       ENDSR;
131100150416
131200150416       //--------------------------------------------------------------
131300150416       //?Stampa segnalazione dell'errore rilevato via SQL             ?
131400150416       //--------------------------------------------------------------
131500150416       BEGSR  sr_PrintErrSQL;
131600150416
131700150416         // -?Stampa del Dump?
131800150416         Dump(A);
131900150416
132000150416         // -?Stampa del Job-Log?
132100150416         Qcmd = 'DSPJOBLOG job(*) output(*print)';
132200150416         exsr  sr_ExecCmd;
132300150416
132400150416         // -?Chiusura del programma?
132500150416         exsr  sr_RoutEnd;
132600150416
132700150416       ENDSR;
132800150416
132900150416       //--------------------------------------------------------------
133000150416       //?Esecuzione del comando (già impostato).                      ?
133100150416       //--------------------------------------------------------------
133200150416       BEGSR  sr_ExecCmd;
133300150416
133400150416         clear Qcap0100;
133500150416         Qcabcsdh = *off;
133600150416         Qcapa    = *off;
133700150416         Qcacmdss = *off;
133800150416         Qcaerved = *allX'00';
133900150416
134000150416         clear Qusec;
134100150416         Qusbprv  = %size(Qusec);
134200150416
134300150416         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
134400150416                           %size(Qcap0100) : 'CPOP0100' : *omit :
134500150416                           0 : 0 : Qusec);
134600150416
134700150416         //if  Qusei <> *blank;
134800150416         //  ...;
134900150416         //endif;
135000150416
135100150416       ENDSR;
135200150416
135300150416       //--------------------------------------------------------------
135400150416       //?Operazioni finali.                                           ?
135500150416       //--------------------------------------------------------------
135600150416       BEGSR  sr_RoutEnd;
135700150416
135800150416         // -?Uscita dal *pgm?
135900150416         return;
136000150416
136100150416       ENDSR;
136200150327
136300150327      /end-free
136400150327
136500150327       //--------------------------------------------------------------
136600150327       //?Definizione schiere a tempo di compilazione                  ?
136700150327       //--------------------------------------------------------------
136800150327
136900170426       //*·** --?sk_TpDsp / sk_DesTpDsp:?Tipi Disposizione?
137000170426       //*·1- Appuntamento      1
137100170426       //*·2- Fermo Deposito    2
137200170426       //*·3- Cambio Indirizzo  3
137300150417** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
137400150327Data formalmente errata                                                         1
137500150327Data DAL successiva alla data AL                                                2
137600150327Filiale arrivo errata                                                           3
137700150430Tipo cliente errato                                                             4
