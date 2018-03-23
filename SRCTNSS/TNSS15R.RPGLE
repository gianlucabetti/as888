000100180209       //==============================================================
000200180209       //? Filtro di Lancio x Statistica Colli Incompatibili Arrivati.  ?
000300180209       //==============================================================
000400180209
000500180209       //--------------------------------------------------------------
000600180209       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180209       //--------------------------------------------------------------
000800180209
000900180209     /*PRM  dbgview(*source)
001000180209     /*END
001100180209
001200180209       //--------------------------------------------------------------
001300180209       //? Specifiche di controllo.                                     ?
001400180209       //--------------------------------------------------------------
001500180209
001600180209     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700180209     h dftactgrp(*no)
001800180209
001900180209       //--------------------------------------------------------------
002000180209       //? Dichiarazione file.                                          ?
002100180209       //--------------------------------------------------------------
002200180209
002300180209       // - ?Organigramma?
002400180209     fAZORG01L  if   e           k disk
002500180212
002600180212       // - ?Tabelle?
002700180212     fTABEL00F  if   e           k disk
002800180209
002900180209       // - ?Video?
003000180209     fTNSS15D   cf   e             workstn
003100180209     f                                     indds( IndDspF )
003200180209     f                                     infds( InfDspF )
003300180209
003400180209       //--------------------------------------------------------------
003500180209       //? Definizione costanti.                                        ?
003600180209       //--------------------------------------------------------------
003700180209
003800180209       // - ?Tasti funzionali a video?
003900180209     d c_F01           c                   const(x'31')
004000180209     d c_F02           c                   const(x'32')
004100180209     d c_F03           c                   const(x'33')
004200180209     d c_F04           c                   const(x'34')
004300180209     d c_F05           c                   const(x'35')
004400180209     d c_F06           c                   const(x'36')
004500180209     d c_F07           c                   const(x'37')
004600180209     d c_F08           c                   const(x'38')
004700180209     d c_F09           c                   const(x'39')
004800180209     d c_F10           c                   const(x'3A')
004900180209     d c_F11           c                   const(x'3B')
005000180209     d c_F12           c                   const(x'3C')
005100180209     d c_F13           c                   const(x'B1')
005200180209     d c_F14           c                   const(x'B2')
005300180209     d c_F15           c                   const(x'B3')
005400180209     d c_F16           c                   const(x'B4')
005500180209     d c_F17           c                   const(x'B5')
005600180209     d c_F18           c                   const(x'B6')
005700180209     d c_F19           c                   const(x'B7')
005800180209     d c_F20           c                   const(x'B8')
005900180209     d c_F21           c                   const(x'B9')
006000180209     d c_F22           c                   const(x'BA')
006100180209     d c_F23           c                   const(x'BB')
006200180209     d c_F24           c                   const(x'BC')
006300180209     d c_Enter         c                   const(x'F1')
006400180209     d c_RollDown      c                   const(x'F4')
006500180209     d c_RollUp        c                   const(x'F5')
006600180209
006700180209       //--------------------------------------------------------------
006800180209       //? Definizione schiere.                                         ?
006900180209       //--------------------------------------------------------------
007000180209
007100180209       // - ?Messaggi di errore?
007200180212     d sk_Msg          s             78    dim( 7)  ctdata  perrcd( 1)
007300180209
007400180209       //--------------------------------------------------------------
007500180209       //? Definizione aree dati.                                       ?
007600180209       //--------------------------------------------------------------
007700180209
007800180209       // - ?Dati utente?
007900180209     d §AzUte        e ds                  extname(AZUTE00F)
008000180209     d                                     dtaara
008100180209     d §DatiUte      e ds                  extname(dDatiUte)
008200180209     d                                     dtaara
008300180209
008400180209       //--------------------------------------------------------------
008500180209       //? Definizione strutture dati.                                  ?
008600180209       //--------------------------------------------------------------
008700180209
008800180209       // - ?Status ds?
008900180209     d Status         sds
009000180209     d   SDSpgm          *proc
009100180209
009200180209       // - ?InfDS?
009300180209     d InfDspF         ds
009400180209     d   $dsp_aid            369    369a
009500180209
009600180209       // - ?Indicatori su DspF?
009700180209     d IndDspF         ds                  inz
009800180209         // - ?Emissione messaggio di errore?
009900180209     d   $ErrMessage                   n   overlay( IndDspF : 28 )
010000180209         // - ?Posizionamento cursore & segnalazione errore?
010100180209     d   $PosCurDTI                    n   overlay( IndDspF : 51 )
010200180209     d   $PosCurDTF                    n   overlay( IndDspF : 52 )
010300180209     d   $PosCurLNA                    n   overlay( IndDspF : 53 )
010400180209         // - ?Riemissione videata?
010500180209     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
010600180209
010700180209       // - ?Parametri ricevuti e passati?
010800180209     d KPJBA         e ds
010900180212
011000180212       // - ?Date pulizie file di filiale?
011100180212     d ds5A          e ds                  inz
011200180212     d                                     qualified
011300180209
011400180209       //--------------------------------------------------------------
011500180209       //? Definizione variabili globali.                               ?
011600180209       //--------------------------------------------------------------
011700180209
011800180209       // - ?Flags booleani?
011900180209     d $Fine           s               n   inz
012000180209     d $InzD01         s               n   inz(*on)
012100180213     d*// $ByPass         s               n   inz
012200180209
012300180209       // - ?Variabili per la gestione del video?
012400180209     d $Video          s              2    inz('D1')
012500180209
012600180209       // - ?Campi di comodo?
012700180209     d wDate           s              8s 0 inz
012800180209
012900180209       //--------------------------------------------------------------
013000180209       //? Definizione prototipi procedure.                             ?
013100180209       //--------------------------------------------------------------
013200180209
013300180209       // - ?Reperimento dati utente?
013400180209     d TIBS34ds      e ds
013500180209      /copy gaitrasrc/srcProtoPR,TIBS34R
013600180209
013700180209       // - ?Controllo/Inversione data?
013800180209     d WLBdat          ds                  inz
013900180209     d   G08dat                       8  0 inz
014000180209     d   G08inv                       8  0 inz
014100180209     d   G08err                       1    inz('3')
014200180209     d   G08tgi                       5  0 inz
014300180209      /copy gaitrasrc/srcProtoPR,XSRDA8
014400180212
014500180212       // - ?Ricalcolo data da Totale giorni?
014600180212     d WGIdat          DS                  inz
014700180212     d   GIOdat                       8  0 inz
014800180212     d   GIOinv                       8  0 inz
014900180212     d   GIOtgi                       5  0 inz
015000180212     d xsrGI8          pr                  extpgm('XSRGI8')
015100180212     d   WGIdat                            likeds( WGIdat )
015200180212
015300180212       // - ?Reperimento data ultimo utilizzo di un programma?
015400180212     d wPGM            s             10    inz( 'FNLR84R   ')
015500180212     d wDAT            s              8    inz(*zero)
015600180212     d wERR            s              1    inz
015700180212     d trul49c         pr                  extpgm('TRUL49C')
015800180212     d   wPGM                              like( wPGM )
015900180212     d   wDAT                              like( wDAT )
016000180212     d   wERR                              like( wERR )
016100180209
016200180209       // - ?Sottomissione lavoro batch?
016300180209      /copy gaitrasrc/srcProtoPR,BCH10
016400180209
016500180209       // - ?Statistica Colli Incomaptibili Arrivati (Batch)?
016600180209     d TNSS16ds      e ds                  inz   qualified
016700180209     d tnss16r         pr                  extpgm('TNSS16R')
016800180209     d   kpjba                             likeds(kpjba)
016900180209
017000180209       // - ?API QCAPCMD (Process Commands)?
017100180209     d Qcmd            s           2048    inz  varying
017200180209      /copy qSysInc/qRpgleSrc,QCAPCMD
017300180209      /copy gaitrasrc/srcProtoPR,QCAPCMD
017400180209       // - ?Parametri gestione errori API.?
017500180209      /copy qsysinc/qRpgleSrc,QUSEC
017600180209
017700180209       //--------------------------------------------------------------
017800180209       //? Definizione key-list.                                        ?
017900180209       //--------------------------------------------------------------
018000180209
018100180212       // -?File TABEL00F?
018200180212     d keyTABEL00    e ds                  extname( TABEL00F : *key )
018300180212     d                                     prefix( k_ )   inz
018400180209
018500180209       //--------------------------------------------------------------
018600180209       //? M A I N - L I N E                                            ?
018700180209       //--------------------------------------------------------------
018800180209
018900180209     c     *Entry        plist
019000180209     c                   parm                    KPJBA
019100180209
019200180209      /free
019300180209
019400180209       // - ?Operazioni iniziali?
019500180209       exsr  sr_RoutInz;
019600180209
019700180209       // - ?Ciclo di gestione del file video?
019800180209       DOW  $Fine = *off;
019900180209
020000180209         select;
020100180209
020200180209           // - ?Gestione videata D1 (parzializzazioni)?
020300180209           when $Video = 'D1';
020400180209             exsr sr_GesD01;
020500180209
020600180209           // - ?? ? ??
020700180209           other;
020800180209             $Fine = *on;
020900180209
021000180209         endsl;
021100180209
021200180209       ENDDO;
021300180209
021400180209       // - ?Operazioni finali?
021500180209       exsr  sr_RoutEnd;
021600180209
021700180209       //--------------------------------------------------------------
021800180209       //? Operazioni iniziali.                                         ?
021900180209       //--------------------------------------------------------------
022000180209       BEGSR  sr_RoutInz;
022100180209
022200180209         // - ?Impostazione chiusura?
022300180209         *inLR = *on;
022400180209
022500180209         // - ?Reperimento dati job?
022600180209         exsr  sr_DatiJob;
022700180209
022800180209         // - ?Reperimento data odierna?
022900180209         wDate = %int( %subst( %char( %dec( %timestamp() ) )
023000180209                               : 1 : 8 ) );
023100180209
023200180209         // - ?Impostazione nome programma a video?
023300180209         V1Tpgm = SDSpgm;
023400180209
023500180209         // - ?Impostazione videata iniziale?
023600180209         $Video = 'D1';
023700180209         reset  $InzD01;
023800180209
023900180209         // - ?Pulizia struttura dati con i parametri per il batch?
024000180209         clear  TNSS16ds;
024100180212
024200180212         // - ?Reperimento Giorni di Pulizia FNARB da tab. "5A"?
024300180212         clear  ds5A;
024400180212         clear  keyTABEL00;
024500180212         k_TBLkut = 1;
024600180212         k_TBLcod = '5A';
024700180212         k_TBLkey = '1';
024800180212         chain  %kds( keyTABEL00 )  TABEL;
024900180212         if  %found( TABEL00F );
025000180212           ds5A = TBLuni;
025100180212         endif;
025200180212
025300180212         // - ?Reperimento Data ultimo utilizzo del *pgm di?
025400180212         //   ?pulizia Statistiche Arrivi?
025500180212         reset  wPgm;
025600180212         reset  wDat;
025700180212         clear  wErr;
025800180212         TRUL49C ( wPgm : wDat : wErr );
025900180212         if  wErr = *on  or
026000180212             wDat = *zero;
026100180212           G08inv = wDate;
026200180212         else;
026300180212           G08inv = %int( wDat );
026400180212         endif;
026500180212         // - ?Sottrazione (dalla data di ultimo utilizzo) dei giorni?
026600180212         //   ?che tiene dalla pulizia e calcolo della prima data al di?
026700180212         //   ?sotto della quale NON si possono lanciare elaborazioni?
026800180212         //   ?per mancanza di dati?
026900180212         if  G08inv <> *zero;
027000180212           clear  G08dat;
027100180212           clear  G08tgi;
027200180212           G08err = '3';
027300180212           XSRda8 ( WLBdat );
027400180212         endif;
027500180212
027600180212         // - ?Calcolo prima data in FNARB dopo ultima pulizia?
027700180212         clear  GIOdat;
027800180212         clear  GIOinv;
027900180212         GIOtgi = G08tgi - ds5A.§5Aarb;
028000180212         XSRgi8 ( WGIdat );
028100180212
028200180212         TNSS16ds.iSS16dtl = GIOinv;
028300180212
028400180209       ENDSR;
028500180209
028600180209       //--------------------------------------------------------------
028700180209       //? Reperimento Dati del job (Utente/Operativi).                 ?
028800180209       //--------------------------------------------------------------
028900180209       BEGSR  sr_DatiJob;
029000180209
029100180209         in(E) §AzUte;
029200180209         if NOT %error;
029300180209           in(E) §DatiUte;
029400180209         endif;
029500180209         if %error or RSut = *blanks;
029600180209           clear TIBS34ds;
029700180209           tibs34r ( tibs34ds );
029800180209           in §AzUte;
029900180209           in §DatiUte;
030000180209         endif;
030100180209
030200180209       ENDSR;
030300180209
030400180209       //--------------------------------------------------------------
030500180209       //? Gestione videata D01.                                        ?
030600180209       //--------------------------------------------------------------
030700180209       BEGSR  sr_GesD01;
030800180209
030900180209         // - ?Inizializzazione videata?
031000180209         if  $InzD01 = *on;
031100180209           exsr  sr_InzD01;
031200180209           $InzD01 = *off;
031300180209         endif;
031400180209
031500180209         // - ?Emissione videata?
031600180209         write  SS15T01;
031700180209         exfmt  SS15D01;
031800180209
031900180209         clear  VIDmsg;
032000180209         reset  $ErrMessage;
032100180209         reset  $ErrGenerico;
032200180209
032300180209         Select;
032400180209
032500180209           // - ?Sistema Informativo errato?
032600180209           when  DUTlpo <> 'S'  or
032700180209                 %subst( KNSIF : 1 : 3 ) <> 'GAI';
032800180209             $Fine = *on;
032900180209
033000180209           //*·// - ?Utente NON abilitato (non EDP)?
033100180209           //*·when  UTEfun <> 'EDPSE'  or
033200180209           //*·      %subst( knmus : 1 : 3 ) <> 'EDP';
033300180209           //*·  $Fine = *on;
033400180209
033500180209           // - ?F3=Fine?
033600180209           When  $dsp_aid = c_F03;
033700180209             $Fine = *on;
033800180209
033900180209           // - ?F9=Visualizzazione WFSCI00F?
034000180209           When  $dsp_aid = c_F09;
034100180209             exsr  sr_F09D01;
034200180209
034300180209           // - ?Enter / F6=Conferma?
034400180209           Other;
034500180209             // - ?Controlli?
034600180209             exsr  sr_CtrD01;
034700180209             select;
034800180209               // - ?Rilevato errore?
034900180209               when  $ErrGenerico;
035000180209                 leavesr;
035100180209               // - ?Sottomissione aggiornamento?
035200180209               when  $dsp_aid = c_F06;
035300180209                 exsr  sr_SbmJob;
035400180209                 $Fine = *on;
035500180209             endsl;
035600180209
035700180209         ENDSL;
035800180209
035900180209       ENDSR;
036000180209
036100180209       //--------------------------------------------------------------
036200180209       //? Inizializzazione videata D01.                                ?
036300180209       //--------------------------------------------------------------
036400180209       BEGSR  sr_InzD01;
036500180209
036600180209         // - ?Spegnimento degli indicatori di errore?
036700180209         %subst( IndDspF : 50 ) = *off;
036800180209
036900180209         // - ?Pulizia videata?
037000180209         clear  SS15D01;
037100180212
037200180212         // - ?Impostazione testo di avvertimento?
037300180212         V1Dtxt = 'I dati dei colli arrivati sono disponibili da +
037400180212                   dopo il giorno ' + %editc( GIOdat : 'Y' );
037500180209
037600180209         // - ?Impostazione dati di default?
037700180209         V1Clna  = *all'9';
037800180209         //*·V1CdtI = 01022017;
037900180209         //*·V1CdtF = %dec( %date() - %days(1) );
038000180209         V1CdtI = %dec( %date() : *eur );
038100180209         V1CdtF = %dec( %date() : *eur );
038200180209
038300180209
038400180209         select;
038500180209
038600180209           // - ?Controllo sistema informativo?
038700180209           when  DUTlpo <> 'S';
038800180209             VIDmsg = sk_Msg(01);
038900180209             $ErrMessage  = *on;
039000180209             $ErrGenerico = *on;
039100180209             leavesr;
039200180209
039300180209           //*·// - ?Controllo utente?
039400180209           //*·when  UTEfun <> 'EDPSE';
039500180209           //*·  VIDmsg = sk_Msg(??);
039600180209           //*·  $ErrMessage  = *on;
039700180209           //*·  $ErrGenerico = *on;
039800180209           //*·  leavesr;
039900180209
040000180209         endsl;
040100180209
040200180209       ENDSR;
040300180209
040400180209       //--------------------------------------------------------------
040500180209       //? Gestione F9 = Visualizzazione WFSCI00F.                      ?
040600180209       //--------------------------------------------------------------
040700180209       BEGSR  sr_F09D01;
040800180209
040900180209         // - ?Visualizzazione del work-file WFSCI00F?
041000180209         Qcmd = 'DSPDBF file(*libl/WFSCI00F) rcdslt(*yes)';
041100180209         exsr  sr_ExecCmd;
041200180209
041300180209       ENDSR;
041400180209
041500180209       //--------------------------------------------------------------
041600180209       //? Controllo dati videata D01.                                  ?
041700180209       //--------------------------------------------------------------
041800180209       BEGSR  sr_CtrD01;
041900180209
042000180209         // - ?Spegnimento degli indicatori di errore?
042100180209         %subst(IndDspF : 50) = *off;
042200180209
042300180209         // -?Controllo Data iniziale?
042400180209         clear  WLBdat;
042500180209         G08dat = V1CdtI;
042600180209         XSRDA8(WLBdat);
042700180209         if  G08err = *zero;
042800180209           V1CdtI   = G08dat;
042900180209           TNSS16ds.iSS16dtI = G08inv;
043000180209         else;
043100180209           VIDmsg = sk_Msg(02);
043200180209           $PosCurDTI   = *on;
043300180209           $ErrMessage  = *on;
043400180209           $ErrGenerico = *on;
043500180209           leavesr;
043600180209         endif;
043700180209
043800180209         // -?Controllo Data finale?
043900180209         clear  WLBdat;
044000180209         G08dat = V1CdtF;
044100180209         XSRDA8(WLBdat);
044200180209         if  G08err = *zero;
044300180209           V1CdtF   = G08dat;
044400180209           TNSS16ds.iSS16dtF = G08inv;
044500180209         else;
044600180209           VIDmsg = sk_Msg(02);
044700180209           $PosCurDTF   = *on;
044800180209           $ErrMessage  = *on;
044900180209           $ErrGenerico = *on;
045000180209           leavesr;
045100180209         endif;
045200180209
045300180209         // -?Controllo sequenza date da riestrarre?
045400180209         if  TNSS16ds.iSS16dtI > TNSS16ds.iSS16dtF;
045500180209           VIDmsg = sk_Msg(03);
045600180209           $PosCurDTI   = *on;
045700180209           $ErrMessage  = *on;
045800180209           $ErrGenerico = *on;
045900180209           leavesr;
046000180209         endif;
046100180212
046200180212         // - ?Data DAL (impostata a video) precedente alla prima?
046300180212         //   ?disponibile?
046400180213         //*-if  TNSS16ds.iSS16dtI >  GIOinv;
046500180213         //*-  clear  $ByPass;
046600180213         //*-endif;
046700180213         //*-if  TNSS16ds.iSS16dtI <= GIOinv  and
046800180213         //*-    NOT  $ByPass;
046900180213         //*-  $ByPass      = *on;
047000180213         if  TNSS16ds.iSS16dtI <= GIOinv;
047100180212           VIDmsg = sk_Msg(04);
047200180212           $PosCurDTI   = *on;
047300180212           $ErrMessage  = *on;
047400180212           $ErrGenerico = *on;
047500180212           leavesr;
047600180212         endif;
047700180209
047800180213         // - ?Controllo ultima data (max oggi)?
047900180209         if  TNSS16ds.iSS16dtF > wDate;
048000180212           VIDmsg = sk_Msg(05);
048100180209           $PosCurDTF   = *on;
048200180209           $ErrMessage  = *on;
048300180209           $ErrGenerico = *on;
048400180209           leavesr;
048500180209         endif;
048600180209
048700180209
048800180209         // - ?Controllo (eventuale) codice Filiale?
048900180209         clear  V1Dlna;
049000180209         If  V1Clna <> *all'9';
049100180209
049200180213           // - ?Verifica inserimento?
049300180209           if  V1Clna = *zero;
049400180212             VIDmsg = sk_Msg(06);
049500180209             $PosCurLNA   = *on;
049600180209             $ErrMessage  = *on;
049700180209             $ErrGenerico = *on;
049800180209             leavesr;
049900180209           endif;
050000180209
050100180213           // - ?Verifica esistenza in AZORG / Decodifica Filiale?
050200180209           chain  ( V1Clna )  AZORG;
050300180209           if  %found( AZORG01L );
050400180209             V1Dlna   = ORGdes;
050500180209           endif;
050600180209
050700180209           if  NOT %found( AZORG01L )  or
050800180209               ORGfva <> *blank;
050900180212             VIDmsg = sk_Msg(07);
051000180209             $PosCurLNA   = *on;
051100180209             $ErrMessage  = *on;
051200180209             $ErrGenerico = *on;
051300180209             leavesr;
051400180209           endif;
051500180209
051600180209         EndIf;
051700180209
051800180209       ENDSR;
051900180209
052000180209       //--------------------------------------------------------------
052100180209       //? Sottomissione lavoro batch.                                  ?
052200180209       //--------------------------------------------------------------
052300180209       BEGSR  sr_SbmJob;
052400180209
052500180209         //*·clear  TNSS16ds;       ?(già pulita in subr. sr_RoutInz)?
052600180209         //*·TNSS16ds.iSS16dti = W1CdtI; ?(già impostata)?
052700180209         //*·TNSS16ds.iSS16dtf = W1CdtF; ?(già impostata)?
052800180209         if  V1Clna <> *all'9';
052900180209           TNSS16ds.iSS16lna = V1Clna;
053000180209         endif;
053100180209
053200180209         kPJbU = TNSS16ds;
053300180209         kCoAz = 'SS16';
053400180209         clear kBuff;
053500180209
053600180209         if  kNmUs = *all'1';
053700180209           // - ?Richiamo interattivo?
053800180209           TNSS16R ( kpjba );
053900180209           //*·TNSS16ds = kPjBu;
054000180209           //*·if  TNSS16ds.oSS16err = *on;
054100180209           //*·  VIDmsg = TNSS16ds.oSS16msg;
054200180209           //*·  $ErrMessage  = *on;
054300180209           //*·  $ErrGenerico = *on;
054400180209           //*·endif;
054500180209         else;
054600180209           // - ?Sottomissione batch?
054700180209           BCH10 ( kpjba );
054800180209         endif;
054900180209
055000180209       ENDSR;
055100180209
055200180209       //--------------------------------------------------------------
055300180209       //? Esecuzione del comando (già impostato).                      ?
055400180209       //--------------------------------------------------------------
055500180209       BEGSR  sr_ExecCmd;
055600180209
055700180209         clear Qcap0100;
055800180209         Qcabcsdh = *off;
055900180209         Qcapa    = *off;
056000180209         Qcacmdss = *off;
056100180209         Qcaerved = *allX'00';
056200180209
056300180209         clear Qusec;
056400180209         Qusbprv  = %size(Qusec);
056500180209
056600180209         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
056700180209                           %size(Qcap0100) : 'CPOP0100' : *omit :
056800180209                           0 : 0 : Qusec);
056900180209
057000180209         //if  Qusei <> *blank;
057100180209         //  ...;
057200180209         //endif;
057300180209
057400180209       ENDSR;
057500180209
057600180209       //--------------------------------------------------------------
057700180209       //? Operazioni finali.                                           ?
057800180209       //--------------------------------------------------------------
057900180209       BEGSR  sr_RoutEnd;
058000180209
058100180209         // - ?Uscita dal *pgm?
058200180209         return;
058300180209
058400180209       ENDSR;
058500180209
058600180209      /end-free
058700180209
058800180209       //--------------------------------------------------------------
058900180209       //? Definizione schiere a tempo di compilazione.                 ?
059000180209       //--------------------------------------------------------------
059100180209
059200180213** - ?sk_Msg: ?Messaggi di Errore ?------------------------------------------*
059300180209Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire        1
059400180209Data formalmente errata                                                         2
059500180213Data iniziale  da estrarre  successiva  a quella finale                         3
059600180213Data iniziale  precedente  alla prima disponibile tra le bolle in Filiale       4
059700180213Data finale  successiva  a quella odierna                                       5
059800180213Immettere la Filiale                                                            6
059900180213Filiale errata                                                                  7
