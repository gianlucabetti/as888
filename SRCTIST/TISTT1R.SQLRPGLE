000100161116       //==============================================================
000200161118       //?Creazione work-file "Tempi Distinte AUT (CRI-CUS-CDC)".      ?
000300161116       //==============================================================
000400161116
000500161116       //--------------------------------------------------------------
000600161116       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700161116       //--------------------------------------------------------------
000800161116
000900161116     /*PRM dbgview(*source)
001000161121     /*CMD ovrdbf file(WFTDA00F) tofile(GAITRAAZP/WFTDA00F) +
001100161121     /*CMD        ovrscope(*calllvl)
001200161121     /*END dltovr file(WFTDA00F) lvl(*)
001300161116     /*END
001400161116
001500161116       //--------------------------------------------------------------
001600161116       //?Specifiche di controllo.                                     ?
001700161116       //--------------------------------------------------------------
001800161116
001900161116     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000161116     h dftactgrp(*no)
002100161116     h alwnull(*inputonly)
002200161116
002300161116       //--------------------------------------------------------------
002400161116       //?Dichiarazione file.                                          ?
002500161116       //--------------------------------------------------------------
002600161118
002700161118       // -?Organigramma?
002800161118     fAZORG01L  if   e           k disk
002900161117
003000161117       // -?Video?
003100161118     fTISTT1D   cf   e             workstn
003200161117     f                                     indds( IndDspF )
003300161117     f                                     infds( InfDspF )
003400161116
003500161116       // -?Work-File "Tempi Distinte AUT"?
003600161116     fWFTDA00F  o    e             disk    usropn
003700161116
003800161116       //--------------------------------------------------------------
003900161116       //?Definizione costanti.                                        ?
004000161116       //--------------------------------------------------------------
004100161116
004200161117       // -?Tasti funzionali a video?
004300161117     d c_F01           c                   const(x'31')
004400161117     d c_F02           c                   const(x'32')
004500161117     d c_F03           c                   const(x'33')
004600161117     d c_F04           c                   const(x'34')
004700161117     d c_F05           c                   const(x'35')
004800161117     d c_F06           c                   const(x'36')
004900161117     d c_F07           c                   const(x'37')
005000161117     d c_F08           c                   const(x'38')
005100161117     d c_F09           c                   const(x'39')
005200161117     d c_F10           c                   const(x'3A')
005300161117     d c_F11           c                   const(x'3B')
005400161117     d c_F12           c                   const(x'3C')
005500161117     d c_F13           c                   const(x'B1')
005600161117     d c_F14           c                   const(x'B2')
005700161117     d c_F15           c                   const(x'B3')
005800161117     d c_F16           c                   const(x'B4')
005900161117     d c_F17           c                   const(x'B5')
006000161117     d c_F18           c                   const(x'B6')
006100161117     d c_F19           c                   const(x'B7')
006200161117     d c_F20           c                   const(x'B8')
006300161117     d c_F21           c                   const(x'B9')
006400161117     d c_F22           c                   const(x'BA')
006500161117     d c_F23           c                   const(x'BB')
006600161117     d c_F24           c                   const(x'BC')
006700161117     d c_Enter         c                   const(x'F1')
006800161117     d c_RollDown      c                   const(x'F4')
006900161117     d c_RollUp        c                   const(x'F5')
007000161116
007100161116       //--------------------------------------------------------------
007200161116       //?Definizione schiere.                                         ?
007300161116       //--------------------------------------------------------------
007400161116
007500161117       // -?Messaggi di errore?
007600161118     d sk_Msg          s             78    dim(10)  ctdata  perrcd( 1)
007700161117
007800161117       // -?Filiali Gestione a video?
007900161117     d FGS_ds          ds                  inz
008000161118     d   V1Cfgs1                           inz
008100161118     d   V1Cfgs2                           inz
008200161118     d   V1Cfgs3                           inz
008300161118     d   V1Cfgs4                           inz
008400161118     d   V1Cfgs5                           inz
008500161117     d     sk_FGS              1     15  0 dim( 5)  inz
008600161116
008700161116       //--------------------------------------------------------------
008800161116       //?Definizione aree dati.                                       ?
008900161116       //--------------------------------------------------------------
009000161116
009100161116       // -?Dati utente?
009200161116     d §AzUte        e ds                  extname(AZUTE00F)
009300161116     d                                     dtaara
009400161116     d §DatiUte      e ds                  extname(dDatiUte)
009500161116     d                                     dtaara
009600161116
009700161116       //--------------------------------------------------------------
009800161116       //?Definizione strutture dati.                                  ?
009900161116       //--------------------------------------------------------------
010000161116
010100161116       // -?Status ds?
010200161116     d Status         sds
010300161116     d   SDSpgmName      *proc
010400161117
010500161117       // -?InfDS?
010600161117     d InfDspF         ds
010700161117     d   dsp_aid             369    369a
010800161117     d*//sfl_rrn             376    377i 0
010900161117     d*//min_nrr             378    379i 0
011000161117     d*//num_rcds            380    381i 0
011100161117
011200161121       //
011300161117     d IndDspF         ds                  inz
011400161117         // ·?Emissione messaggio di errore?
011500161117     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011600161117         // ·?Posizionamento cursore?
011700161117     d   $PosCurFG1                    n   overlay( IndDspF : 51 )
011800161117     d   $PosCurFG2                    n   overlay( IndDspF : 52 )
011900161117     d   $PosCurFG3                    n   overlay( IndDspF : 53 )
012000161117     d   $PosCurFG4                    n   overlay( IndDspF : 54 )
012100161117     d   $PosCurFG5                    n   overlay( IndDspF : 55 )
012200161117     d   $PosCurDFVI                   n   overlay( IndDspF : 56 )
012300161117     d   $PosCurDFVF                   n   overlay( IndDspF : 57 )
012400161117     d   $PosCurCLR                    n   overlay( IndDspF : 58 )
012500161117         // ·?Riemissione videata?
012600161117     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
012700161116
012800161116       // -?Parametri ricevuti?
012900161116     d KPJBA         e ds
013000161117
013100161117       // -?Dati estratti via SQL?
013200161117     d WFTDA_ds      e ds                  inz   extname(WFTDA00F)
013300161116
013400161116       //--------------------------------------------------------------
013500161116       //?Definizione variabili globali.                               ?
013600161116       //--------------------------------------------------------------
013700161116
013800161116       // -?Flags booleani?
013900161116     d $Fine           s               n   inz
014000161116     d $EoF            s               n   inz
014100161121     d $InzD01         s               n   inz(*on)
014200161117
014300161117       // -?Indici di schiera?
014400161117     d xx              s              3  0 inz
014500161117
014600161117       // -?Variabili per la gestione del video?
014700161117     d $Video          s              2    inz('D1')
014800161116
014900161116       // -?Stringhe SQL da eseguire?
015000161116     d wSQL            s          32740    inz   varying
015100161121
015200161121       // -?Nome libreria dei *file da cui estrarre i dati?
015300161121     d wLibr           s             10    inz
015400161121       // -?Nome libreria del work-file?
015500161121     d wLibrW          s             10    inz
015600161117
015700161117       // -?Campi di comodo?
015800161117     d wElenco_FGS     s             25    inz
015900161118     d wFgs999         s                   inz   like(V1Cfgs1)
016000161117     d W1CdfvI         s                   inz   like(V1CdfvI)
016100161117     d W1CdfvF         s                   inz   like(V1CdfvF)
016200161116
016300161116       //--------------------------------------------------------------
016400161116       //?Definizione prototipi procedure.                             ?
016500161116       //--------------------------------------------------------------
016600161117
016700161117       // -?Controllo/Inversione Date?
016800161117     d WLBdat          ds                  inz
016900161117     d   G08dat                       8  0 inz
017000161117     d   G08inv                       8  0 inz
017100161117     d   G08err                       1    inz('3')
017200161117     d   G08tgi                       5  0 inz
017300161117      /copy gaitrasrc/srcProtoPR,XSRDA8
017400161116
017500161116       // -?Reperimento dati utente?
017600161116     d TIBS34ds      e ds                  inz
017700161116      /copy gaitrasrc/srcProtoPR,TIBS34R
017800161116
017900161116       // -?API QCAPCMD (Process Commands)?
018000161116     d Qcmd            s           2048    inz  varying
018100161116      /copy qSysInc/qRpgleSrc,QCAPCMD
018200161116      /copy gaitrasrc/srcProtoPR,QCAPCMD
018300161116       // -?Parametri gestione errori API.?
018400161116      /copy qsysinc/qRpgleSrc,QUSEC
018500161116
018600161116       //--------------------------------------------------------------
018700161116       //?Definizione key-list.                                        ?
018800161116       //--------------------------------------------------------------
018900161116
019000161116
019100161116       //--------------------------------------------------------------
019200161116       //?Riepilogo indicatori utilizzati.                             ?
019300161116       //--------------------------------------------------------------
019400161116
019500161116
019600161116       //--------------------------------------------------------------
019700161116       //?M A I N - L I N E                                            ?
019800161116       //--------------------------------------------------------------
019900161116
020000161116     c     *Entry        plist
020100161116     c                   parm                    KPJBA
020200161116
020300161116      /free
020400161116
020500161116       // -?Operazioni iniziali?
020600161116       exsr  sr_RoutInz;
020700161117
020800161117       // -?Gestione videate?
020900161117       DoW  $Fine = *off;
021000161117
021100161117         Select;
021200161117
021300161117           // -?Gestione subfile S1?
021400161117           When $Video = 'D1';
021500161117             exsr  sr_GesD01;
021600161117
021700161117           // -?? ? ? ? ??
021800161117           Other;
021900161117             $Fine = *on;
022000161117
022100161117         EndSl;
022200161117
022300161117       EndDo;
022400161116
022500161116       // -?Operazioni finali?
022600161116       exsr sr_RoutEnd;
022700161116
022800161116       //--------------------------------------------------------------
022900161116       //?Operazioni iniziali.                                         ?
023000161116       //--------------------------------------------------------------
023100161116       BEGSR  sr_RoutInz;
023200161116
023300161116         // -?Impostazione opzioni per SQL?
023400161116         exec sql   set  option  DynUsrPrf = *Owner,
023500161116                                 CloSqlCsr = *EndMod;
023600161116
023700161116         // -?Impostazione chiusura?
023800161116         *inLR = *on;
023900161116
024000161116         // -?Reperimento dati job?
024100161116         exsr  sr_DatiJob;
024200161116
024300161116         // -?Impostazione nome programma a video?
024400161116         V1Tpgm = SDSpgmName;
024500161121
024600161121         // -?Impostazione nome libreria per *file?
024700161121         //  ?(a seconda del sistema in cui ci si trova: AS888/SETRAS)?
024800161121         if  %subst( kNSif : 7 : 1 ) = 'P';
024900161121           wLibrW = 'GAITRAAZP ';
025000161121           wLibr  = 'FILTRAPRD ';
025100161121         else;
025200161121           wLibrW = 'GAITRAAZM ';
025300161121           wLibr  = 'FILTRA201 ';
025400161121         endif;
025500161116
025600161116       ENDSR;
025700161116
025800161116       //--------------------------------------------------------------
025900161116       //?Reperimento Dati del job (Utente/Operativi).                 ?
026000161116       //--------------------------------------------------------------
026100161116       BEGSR  sr_DatiJob;
026200161116
026300161116         in(e) §AzUte;
026400161116         if NOT %error;
026500161116           in(e) §DatiUte;
026600161116         endif;
026700161116         if %error or RSut = *blank;
026800161116           tibs34r ( tibs34ds );
026900161116           in §AzUte;
027000161116           in §DatiUte;
027100161116         endif;
027200161116
027300161116       ENDSR;
027400161116
027500161116       //--------------------------------------------------------------
027600161117       //?Gestione videata D01.                                        ?
027700161116       //--------------------------------------------------------------
027800161117       BEGSR  sr_GesD01;
027900161116
028000161117         // -?Inizializzazione videata?
028100161117         if  $InzD01 = *on  and  Not $errGenerico;
028200161117           exsr  sr_InzD01;
028300161117           $InzD01 = *off;
028400161116         endif;
028500161116
028600161117         // -?Emissione Testata e Piede?
028700161118         write  STT1T01;
028800161118         write  STT1P01;
028900161117
029000161117         // -?Emissione videata di richiesta parametri?
029100161118         exfmt  STT1D01;
029200161116
029300161116         clear  V1Dmsg;
029400161116         reset  $ErrMessage;
029500161116         reset  $ErrGenerico;
029600161116
029700161116         Select;
029800161116
029900161117           // -?Sistema Informativo di Sede => Uscita forzata?
030000161121           When  DUTlpo <> 'S';
030100161116             $Fine = *on;
030200161116
030300161116           // -?F3=Fine?
030400161116           When  dsp_aid = c_F03;
030500161116             $Fine = *on;
030600161117
030700161117           // -?F9=Visualizzazione WFTDA00F?
030800161117           When  dsp_aid = c_F09;
030900161117             exsr  sr_F09D01;
031000161116
031100161117           // -?Invio / F6 = Conferma?
031200161116           Other;
031300161117             exsr  sr_CtrD01;
031400161118             if  Not $ErrGenerico  and  dsp_aid = c_F06;
031500161118               exsr  sr_F06D01;
031600161118             endif;
031700161116
031800161116         EndSl;
031900161116
032000161116       ENDSR;
032100161116
032200161116       //--------------------------------------------------------------
032300161117       //?Inizializzazione videata D01.                                ?
032400161116       //--------------------------------------------------------------
032500161117       BEGSR  sr_InzD01;
032600161116
032700161117         // -?Pulizia videata?
032800161118         clear  STT1D01;
032900161116
033000161116         // -?Controllo del sistema informativo in cui si sta lavorando?
033100161121         if  DUTlpo <> 'S';
033200161116           $ErrGenerico = *on;
033300161116           $ErrMessage  = *on;
033400161116           V1Dmsg = sk_Msg(01);
033500161116           leavesr;
033600161116         endif;
033700161116
033800161116       ENDSR;
033900161117
034000161117       //--------------------------------------------------------------
034100161117       //?Controllo dati nella videata D01.                            ?
034200161117       //--------------------------------------------------------------
034300161117       BEGSR  sr_CtrD01;
034400161117
034500161117         // -?Spegnimento degli indicatori di errore?
034600161118         %subst( IndDspF : 28 ) = *off;
034700161117
034800161117         // -?Controllo inserimento almeno una filiale gestione?
034900161118         if  %xfoot( sk_FGS ) = *zero;
035000161117           $ErrGenerico = *on;
035100161117           $ErrMessage  = *on;
035200161118           %subst( IndDspF : 51 ) = *on;
035300161117           V1Dmsg = sk_Msg(02);
035400161117           leavesr;
035500161117         endif;
035600161118
035700161118         // -?Memorizzazione inserimento di UNA fil. 999?
035800161118         wFgs999 = %lookup( 999 : sk_FGS );
035900161118
036000161118         // -?Controllo filiali inserite?
036100161118         For  xx = 1  To  %elem( sk_FGS );
036200161118           if  sk_FGS(xx) <> *zero  and  xx <> wFgs999;
036300161118
036400161118             Select;
036500161118               // -?La 999 dev'essere "singola"?
036600161118               When  wFgs999  > *zero;
036700161118                 $ErrGenerico = *on;
036800161118                 $ErrMessage  = *on;
036900161118                 %subst( IndDspF : 50 + xx ) = *on;
037000161118                 V1Dmsg = sk_Msg(03);
037100161118                 leavesr;
037200161118               // -?Ogni filiale inserita dev'essere unica?
037300161118               When  %lookup( sk_FGS(xx) : sk_FGS ) < xx;
037400161118                 $ErrGenerico = *on;
037500161118                 $ErrMessage  = *on;
037600161118                 %subst( IndDspF : 50 + xx ) = *on;
037700161118                 V1Dmsg = sk_Msg(04);
037800161118                 leavesr;
037900161118               // -?Controllo su AZORG00F ogni filiale inserita?
038000161118               Other;
038100161118                 chain  ( sk_FGS(xx) )  AZORG;
038200161118                 if  NOT  %found(AZORG01L);
038300161118                   $ErrGenerico = *on;
038400161118                   $ErrMessage  = *on;
038500161118                   %subst( IndDspF : 50 + xx ) = *on;
038600161118                   V1Dmsg = sk_Msg(05);
038700161118                   leavesr;
038800161118                 endif;
038900161118             EndSl;
039000161118
039100161118           endif;
039200161118         EndFor;
039300161117
039400161117         // -?Controllo data distinta iniziale?
039500161117         clear  WLBdat;
039600161117         G08dat = V1CdfvI;
039700161117         xsrDA8( WLBdat );
039800161117         if  G08err = *on;
039900161117           $ErrGenerico = *on;
040000161117           $ErrMessage  = *on;
040100161117           $PosCurDFVI  = *on;
040200161118           V1Dmsg = sk_Msg(06);
040300161117           leavesr;
040400161117         endif;
040500161118         V1CdfvI = G08dat;
040600161117         W1CdfvI = G08inv;
040700161117
040800161117         // -?Controllo data distinta finale?
040900161117         clear  WLBdat;
041000161117         G08dat = V1CdfvF;
041100161117         xsrDA8( WLBdat );
041200161117         if  G08err = *on;
041300161117           $ErrGenerico = *on;
041400161117           $ErrMessage  = *on;
041500161117           $PosCurDFVF  = *on;
041600161118           V1Dmsg = sk_Msg(06);
041700161117           leavesr;
041800161117         endif;
041900161118         V1CdfvF = G08dat;
042000161117         W1CdfvF = G08inv;
042100161117
042200161117         // -?Controllo range date distinta?
042300161117         if  W1CdfvI > W1CdfvF;
042400161117           $ErrGenerico = *on;
042500161117           $ErrMessage  = *on;
042600161117           $PosCurDFVI  = *on;
042700161118           V1Dmsg = sk_Msg(07);
042800161117           leavesr;
042900161117         endif;
043000161117
043100161117         // -?Controllo range date distinta?
043200161117         if  V1Cclr <> *blank  and
043300161117             V1Cclr <> 'N';
043400161117           $ErrGenerico = *on;
043500161117           $ErrMessage  = *on;
043600161117           $PosCurCLR = *on;
043700161118           V1Dmsg = sk_Msg(08);
043800161117           leavesr;
043900161117         endif;
044000161117
044100161117       ENDSR;
044200161117
044300161117       //--------------------------------------------------------------
044400161117       //?Gestione F9 = Visualizzazione WFTDA00F.                      ?
044500161117       //--------------------------------------------------------------
044600161117       BEGSR  sr_F09D01;
044700161117
044800161117         // -?Visualizzazione del work-file WFTDA00F?
044900161121         Qcmd = 'DSPDBF file(' + %trimR( wLibrW ) + '/WFTDA00F)';
045000161117         exsr  sr_ExecCmd;
045100161117
045200161117       ENDSR;
045300161116
045400161116       //--------------------------------------------------------------
045500161117       //?Gestione F6 = Conferma (creazione WFTDA00F).                 ?
045600161116       //--------------------------------------------------------------
045700161117       BEGSR  sr_F06D01;
045800161117
045900161117         // -?Pulizia work-file - SE richiesta?
046000161117         if  V1Cclr <> 'N';
046100161121           Qcmd = 'CLRPFM file(' + %trimR( wLibrW ) + '/WFTDA00F)';
046200161117           exsr  sr_ExecCmd;
046300161117           if  Qusei <> *blank;
046400161117             $ErrGenerico = *on;
046500161117             $ErrMessage  = *on;
046600161118             V1Dmsg = sk_Msg(09);
046700161117             leavesr;
046800161117           endif;
046900161117         endif;
047000161117
047100161118         // -?Apertura work-file?
047200161117         open  WFTDA00F;
047300161116
047400161117         // -?Preparazione stringa SQL per l'estrazione dei dati?
047500161117         exsr  sr_PrepSQL;
047600161117
047700161117         // -?Apertura del cursore?
047800161117         exsr  sr_OpenCursor;
047900161117         if  $ErrGenerico;
048000161117           leavesr;
048100161117         endif;
048200161117
048300161117         // -?Lettura del cursore & scrittura del work-file?
048400161117         exsr  sr_ReadCursor;
048500161117
048600161117         DoW  Not $EoF;
048700161117           write  WFTDA000;
048800161117           exsr  sr_ReadCursor;
048900161117         EndDo;
049000161117
049100161117         // -?Chiusura del cursore?
049200161117         exsr  sr_CloseCursor;
049300161117
049400161118         // -?Chiusura work-file?
049500161118         close WFTDA00F;
049600161118
049700161118         if  NOT $ErrGenerico;
049800161118           reset  $InzD01;
049900161118         endif;
050000161116
050100161116       ENDSR;
050200161116
050300161116       //--------------------------------------------------------------
050400161116       //?Preparazione stringa SQL per estrazione dati.                ?
050500161116       //--------------------------------------------------------------
050600161116       BEGSR  sr_PrepSQL;
050700161117
050800161117         // -?Preparazione elenco Filiali Gestione da elaborare?
050900161117         clear  wElenco_FGS;
051000161117         For  xx = 1  To  %elem( sk_FGS );
051100161117
051200161117           if  sk_FGS(xx) > *zero;
051300161117
051400161117             if  wElenco_FGS = *blank;
051500161117               wElenco_FGS = %trim( %editc( sk_FGS(xx) : '3' ) );
051600161117             else;
051700161117               wElenco_FGS = %trimR( wElenco_FGS ) + ', ' +
051800161117                             %trim( %editc( sk_FGS(xx) : '3' ) );
051900161117             endif;
052000161117
052100161117           endif;
052200161117
052300161117         EndFor;
052400161116
052500161117         // -?Preparazione stringa SQL?
052600161117         wSQL = 'with +
052700161117
052800161118                   TD_CUS as ( select DSFtrd, DSFdtOrIn, DSTfgs, +
052900161117                                      DSTpdr, DSTdfv, DSTnfv, DSTpkl, DSTvlu, +
053000161117                                      DSTnftL, DSTnbl, DSTnblA, DSTrNblA, +
053100161117                                      substr( DSTflr, 2, 1 ) as TurnoPck +
053200161121                                 from ' + %trimR( wLibr ) + '/FIDSF00F +
053300161121                                inner join ' +
053400161121                                          %trimR (wLibr ) + '/FIDST00F +
053500161117                                   on DSFfgs = DSTfgs +
053600161117                                  and DSFnfv = DSTnfv +
053700161117                                where DSTfgs in (' +
053800161117                                      %trim( wElenco_FGS ) + ') +
053900161117                                  and DSFnpg = 4 +
054000161117                                  and DSFtrd = ''CUS'' +
054100161117                                  and DSTdfv between ' +
054200161117                                      %editc( W1CdfvI : 'X' ) + ' and ' +
054300161118                                      %editc( W1CdfvF : 'X' ) + ' ), +
054400161117
054500161118                   TD_CRI as ( select DSFtrd, DSFdtOrIn, DSTfgs, +
054600161117                                      DSTpdr, DSTdfv, DSTnfv, DSTpkl, DSTvlu, +
054700161117                                      DSTnftL, DSTnbl, DSTnblA, DSTrNblA, +
054800161117                                      substr( DSTflr, 2, 1 ) as TurnoPck +
054900161121                                 from ' + %trimR( wLibr ) + '/FIDSF00F +
055000161121                                inner join ' +
055100161121                                          %trimR (wLibr ) + '/FIDST00F +
055200161117                                   on DSFfgs = DSTfgs +
055300161117                                  and DSFnfv = DSTnfv +
055400161117                                where DSTfgs in (' +
055500161117                                      %trim( wElenco_FGS ) + ') +
055600161117                                  and DSFnpg = 4 +
055700161117                                  and DSFtrd = ''CRI'' +
055800161117                                  and DSTdfv between ' +
055900161117                                      %editc( W1CdfvI : 'X' ) + ' and ' +
056000161118                                      %editc( W1CdfvF : 'X' ) + ' ), +
056100161117
056200161118                   TD_CDC as ( select DSFtrd, DSFdtOrIn, DSTfgs, +
056300161117                                      DSTpdr, DSTdfv, DSTnfv, DSTpkl, DSTvlu, +
056400161117                                      DSTnftL, DSTnbl, DSTnblA, DSTrNblA, +
056500161117                                      substr( DSTflr, 2, 1 ) as TurnoPck +
056600161121                                 from ' + %trimR( wLibr ) + '/FIDSF00F +
056700161121                                inner join ' +
056800161121                                          %trimR (wLibr ) + '/FIDST00F +
056900161117                                   on DSFfgs = DSTfgs +
057000161117                                  and DSFnfv = DSTnfv +
057100161117                                where DSTfgs in (' +
057200161117                                      %trim( wElenco_FGS ) + ') +
057300161117                                  and DSFnpg = 4 +
057400161117                                  and DSFtrd = ''CDC'' +
057500161117                                  and DSTdfv between ' +
057600161117                                      %editc( W1CdfvI : 'X' ) + ' and ' +
057700161118                                      %editc( W1CdfvF : 'X' ) + ' ) +
057800161117
057900161118              select TD_CDC.DSTfgs, TD_CDC.DSTnfv, TD_CDC.DSTdfv, +
058000161118                     TD_CDC.DSTpdr, +
058100161118                     coalesce( int( substr(TD_CUS.DSFdtOrIn, 9, 6) ), +
058200161118                               0 ) as CUS, +
058300161118                     coalesce( int( substr(TD_CRI.DSFdtOrIn, 9, 6) ), +
058400161118                               0 ) as CRI, +
058500161118                     coalesce( int( substr(TD_CDC.DSFdtOrIn, 9, 6) ), +
058600161118                               0 ) as CDC +
058700161117
058800161117                from TD_CDC left outer join TD_CUS +
058900161118                  on TD_CDC.DSTfgs = TD_CUS.DSTfgs +
059000161118                 and TD_CDC.DSTnfv = TD_CUS.DSTnfv +
059100161117                 and TD_CDC.DSTpdr = TD_CUS.DSTpdr +
059200161117
059300161117                            left outer join TD_CRI +
059400161118                  on TD_CDC.DSTfgs = TD_CRI.DSTfgs +
059500161118                 and TD_CDC.DSTnfv = TD_CRI.DSTnfv +
059600161117                 and TD_CDC.DSTpdr = TD_CRI.DSTpdr +
059700161118
059800161118            order by TD_CDC.DSTfgs, TD_CDC.DSTdfv, TD_CDC.DSTnfv +
059900161117
060000161117                 for fetch only';
060100161116
060200161116       ENDSR;
060300161116
060400161116       //--------------------------------------------------------------
060500161116       //?Apertura cursore.                                            ?
060600161116       //--------------------------------------------------------------
060700161116       BEGSR  sr_OpenCursor;
060800161116
060900161116         // -?Dichiarazione cursore?
061000161116         exec sql   prepare S1   from :wSQL;
061100161116         exec sql   declare C1   cursor for S1;
061200161116
061300161116         if  SQLcode < *zero;
061400161116           $EoF = *on;
061500161116           exsr  sr_PrintERR;
061600161117           leavesr;
061700161116         endif;
061800161116
061900161116         // -?Apertura del cursore?
062000161116         exec sql   open C1;
062100161116
062200161116         if  SQLcode < *zero;
062300161116           $EoF = *on;
062400161116           exsr  sr_PrintERR;
062500161117           leavesr;
062600161116         endif;
062700161116
062800161116       ENDSR;
062900161116
063000161116       //--------------------------------------------------------------
063100161116       //?Chiusura cursore.                                            ?
063200161116       //--------------------------------------------------------------
063300161116       BEGSR  sr_CloseCursor;
063400161116
063500161116         // -?Chiusura del cursore?
063600161116         exec sql   close C1;
063700161116
063800161116       ENDSR;
063900161116
064000161116       //--------------------------------------------------------------
064100161116       //?Lettura cursore.                                             ?
064200161116       //--------------------------------------------------------------
064300161116       BEGSR  sr_ReadCursor;
064400161116
064500161116         $EoF = *off;
064600161117         clear  WFTDA_ds;
064700161116
064800161117         exec sql   fetch next   from C1   into :WFTDA_ds;
064900161116
065000161116         select;
065100161116           when  SQLcode = 100;
065200161116             $EoF = *on;
065300161116           when  SQLcode < *zero;
065400161116             $EoF = *on;
065500161116             exsr  sr_PrintERR;
065600161116         endsl;
065700161116
065800161116       ENDSR;
065900161116
066000161116       //--------------------------------------------------------------
066100161116       //?Stampa segnalazione dell'errore rilevato via SQL             ?
066200161116       //--------------------------------------------------------------
066300161116       BEGSR  sr_PrintERR;
066400161116
066500161116         // -?Stampa del Dump?
066600161116         Dump(A);
066700161116
066800161116         // -?Stampa del Job-Log?
066900161116         Qcmd = 'DSPJOBLOG job(*) output(*print)';
067000161116         exsr  sr_ExecCmd;
067100161116
067200161117         //*·// -?Chiusura del programma?
067300161117         //*·exsr  sr_RoutEnd;
067400161117
067500161117         // -?Segnalazione errore a video?
067600161117         $ErrGenerico = *on;
067700161117         $ErrMessage  = *on;
067800161118         V1Dmsg = sk_Msg(10);
067900161116
068000161116       ENDSR;
068100161116
068200161116       //--------------------------------------------------------------
068300161116       //?Esecuzione del comando (già impostato).                      ?
068400161116       //--------------------------------------------------------------
068500161116       BEGSR  sr_ExecCmd;
068600161116
068700161116         clear Qcap0100;
068800161116         Qcabcsdh = *off;
068900161116         Qcapa    = *off;
069000161116         Qcacmdss = *off;
069100161116         Qcaerved = *allX'00';
069200161116
069300161116         clear Qusec;
069400161116         Qusbprv  = %size(Qusec);
069500161116
069600161116         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
069700161116                           %size(Qcap0100) : 'CPOP0100' : *omit :
069800161116                           0 : 0 : Qusec);
069900161116
070000161116         //if  Qusei <> *blank;
070100161116         //  ...;
070200161116         //endif;
070300161116
070400161116       ENDSR;
070500161116
070600161116       //--------------------------------------------------------------
070700161116       //?Operazioni finali.                                           ?
070800161116       //--------------------------------------------------------------
070900161116       BEGSR  sr_RoutEnd;
071000161117
071100161116         // -?Uscita dal *pgm?
071200161116         return;
071300161116
071400161116       ENDSR;
071500161116
071600161116      /end-free
071700161116
071800161116       //--------------------------------------------------------------
071900161116       //?Schiere a tempo di compilazione.                             ?
072000161116       //--------------------------------------------------------------
072100161116
072200161116** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
072300161116Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
072400161117Inserire ALMENO una Filiale                                                    2
072500161118Già scelte TUTTE le filiali con 999                                            3
072600161118Filiale già inserita                                                           4
072700161118Filiale errata                                                                 5
072800161118Data formalmente errata                                                        6
072900161118Data distinta iniziale successiva a quella finale                              7
073000161118Richiesta pulizia dati nel file errata                                         8
073100161118NON riuscita la pulizia del file - NON eseguita estrazione                     9
073200161124Rilevato errore nell'estrazione dei dati: consultare stampe ed avvisare CED    10
