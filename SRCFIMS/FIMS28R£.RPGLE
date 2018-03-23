000100170512       //==============================================================
000200170512       //?RI-Estrazione orari di Inizio/Fine picking.                  ?
000300170512       //==============================================================
000400170512
000500170512       //--------------------------------------------------------------
000600170512       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170512       //--------------------------------------------------------------
000800170512
000900170512     /*PRM  dbgview(*source)
001000170512     /*END
001100170512
001200170512       //--------------------------------------------------------------
001300170512       //?Specifiche di controllo.                                     ?
001400170512       //--------------------------------------------------------------
001500170512
001600170512     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170512     h dftactgrp(*no)
001800170512
001900170512       //--------------------------------------------------------------
002000170512       //?Dichiarazione file.                                          ?
002100170512       //--------------------------------------------------------------
002200170512
002300170512       // -?Organigramma?
002400170512     fAZORG01L  if   e           k disk
002500170512
002600170512       // -?Video?
002700170512     fFIMS28D   cf   e             workstn
002800170512     f                                     indds( IndDspF )
002900170512     f                                     infds( InfDspF )
003000170512
003100170512       //--------------------------------------------------------------
003200170512       //?Definizione costanti.                                        ?
003300170512       //--------------------------------------------------------------
003400170512
003500170512       // -?Tasti funzionali a video?
003600170512     d c_F01           c                   const(x'31')
003700170512     d c_F02           c                   const(x'32')
003800170512     d c_F03           c                   const(x'33')
003900170512     d c_F04           c                   const(x'34')
004000170512     d c_F05           c                   const(x'35')
004100170512     d c_F06           c                   const(x'36')
004200170512     d c_F07           c                   const(x'37')
004300170512     d c_F08           c                   const(x'38')
004400170512     d c_F09           c                   const(x'39')
004500170512     d c_F10           c                   const(x'3A')
004600170512     d c_F11           c                   const(x'3B')
004700170512     d c_F12           c                   const(x'3C')
004800170512     d c_F13           c                   const(x'B1')
004900170512     d c_F14           c                   const(x'B2')
005000170512     d c_F15           c                   const(x'B3')
005100170512     d c_F16           c                   const(x'B4')
005200170512     d c_F17           c                   const(x'B5')
005300170512     d c_F18           c                   const(x'B6')
005400170512     d c_F19           c                   const(x'B7')
005500170512     d c_F20           c                   const(x'B8')
005600170512     d c_F21           c                   const(x'B9')
005700170512     d c_F22           c                   const(x'BA')
005800170512     d c_F23           c                   const(x'BB')
005900170512     d c_F24           c                   const(x'BC')
006000170512     d c_Enter         c                   const(x'F1')
006100170512     d c_RollDown      c                   const(x'F4')
006200170512     d c_RollUp        c                   const(x'F5')
006300170512
006400170512       //--------------------------------------------------------------
006500170512       //?Definizione schiere.                                         ?
006600170512       //--------------------------------------------------------------
006700170512
006800170512       // -?Messaggi di errore?
006900170512     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
007000170512
007100170512       //--------------------------------------------------------------
007200170512       //?Definizione aree dati.                                       ?
007300170512       //--------------------------------------------------------------
007400170512
007500170512       // -?Dati utente?
007600170512     d §AzUte        e ds                  extname(AZUTE00F)
007700170512     d                                     dtaara
007800170512     d §DatiUte      e ds                  extname(dDatiUte)
007900170512     d                                     dtaara
008000170512
008100170512       //--------------------------------------------------------------
008200170512       //?Definizione strutture dati.                                  ?
008300170512       //--------------------------------------------------------------
008400170512
008500170512       // -?Status ds?
008600170512     d Status         sds
008700170512     d   SDSpgm          *proc
008800170512
008900170512       // -?InfDS?
009000170512     d InfDspF         ds
009100170512     d   $dsp_aid            369    369a
009200170512
009300170512       // -?Indicatori su DspF?
009400170512     d IndDspF         ds                  inz
009500170512         // -?Emissione messaggio di errore?
009600170512     d   $ErrMessage                   n   overlay( IndDspF : 28 )
009700170512         // -?Posizionamento cursore & segnalazione errore?
009800170512     d   $PosCurFIL                    n   overlay( IndDspF : 51 )
009900170512     d   $PosCurDTLI                   n   overlay( IndDspF : 52 )
010000170512     d   $PosCurDTLF                   n   overlay( IndDspF : 53 )
010100170512         // -?Riemissione videata?
010200170512     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
010300170512
010400170512       // -?Parametri ricevuti e passati?
010500170512     d KPJBA         e ds
010600170512
010700170512       //--------------------------------------------------------------
010800170512       //?Definizione variabili globali.                               ?
010900170512       //--------------------------------------------------------------
011000170512
011100170512       // -?Flags booleani?
011200170512     d $Fine           s               n   inz
011300170512     d $InzD01         s               n   inz(*on)
011400170512
011500170512       // -?Variabili per la gestione del video?
011600170512     d $Video          s              2    inz('D1')
011700170512
011800170512       // -?Campi di comodo?
011900170512     d wDate           s              8s 0 inz
012000170512     d W1CdtlI         s              8  0 inz
012100170512     d W1CdtlF         s              8  0 inz
012200170512
012300170512       //--------------------------------------------------------------
012400170512       //?Definizione prototipi procedure.                             ?
012500170512       //--------------------------------------------------------------
012600170512
012700170512       // -?Reperimento dati utente?
012800170512     d TIBS34ds      e ds
012900170512      /copy gaitrasrc/srcProtoPR,TIBS34R
013000170512
013100170512       // -?Controllo/Inversione data?
013200170512     d WLBdat          ds                  inz
013300170512     d   G08dat                       8  0 inz
013400170512     d   G08inv                       8  0 inz
013500170512     d   G08err                       1    inz('3')
013600170512     d   G08tgi                       5  0 inz
013700170512      /copy gaitrasrc/srcProtoPR,XSRDA8
013800170512
013900170512       // -?Sottomissione lavoro batch?
014000170512      /copy gaitrasrc/srcProtoPR,BCH10
014100170512
014200170512       // -?(Ri)Estrazione orari di Inizio/Fine picking (Batch)?
014300170512     d FIMS26ds      e ds                  inz   qualified
014400170512     d fims26r         pr                  extpgm('FIMS26R')
014500170512     d   kpjba                             likeds(kpjba)
014600170512
014700170512       //--------------------------------------------------------------
014800170512       //?Definizione key-list.                                        ?
014900170512       //--------------------------------------------------------------
015000170512
015100170512
015200170512       //--------------------------------------------------------------
015300170512       //?Riepilogo indicatori utilizzati.                             ?
015400170512       //--------------------------------------------------------------
015500170512       //--------------------------------------------------------------
015600170512
015700170512       //--------------------------------------------------------------
015800170512       //?M A I N - L I N E                                            ?
015900170512       //--------------------------------------------------------------
016000170512
016100170512     c     *Entry        plist
016200170512     c                   parm                    KPJBA
016300170512
016400170512      /free
016500170512
016600170512       // -?Operazioni iniziali?
016700170512       exsr  sr_RoutInz;
016800170512
016900170512       // -?Ciclo di gestione del file video?
017000170512       DOW  $Fine = *off;
017100170512
017200170512         select;
017300170512
017400170512           // -?Gestione videata D1 (parzializzazioni)?
017500170512           when $Video = 'D1';
017600170512             exsr sr_GesD01;
017700170512
017800170512           // -?? ? ??
017900170512           other;
018000170512             $Fine = *on;
018100170512
018200170512         endsl;
018300170512
018400170512       ENDDO;
018500170512
018600170512       // -?Operazioni finali?
018700170512       exsr  sr_RoutEnd;
018800170512
018900170512       //--------------------------------------------------------------
019000170512       //?Operazioni iniziali.                                         ?
019100170512       //--------------------------------------------------------------
019200170512       BEGSR  sr_RoutInz;
019300170512
019400170512         // -?Impostazione chiusura?
019500170512         *inLR = *on;
019600170512
019700170512         // -?Reperimento dati job?
019800170512         exsr  sr_DatiJob;
019900170512
020000170512         // -?Reperimento data odierna?
020100170512         wDate = %int( %subst( %char( %dec( %timestamp() ) )
020200170512                               : 1 : 8 ) );
020300170512
020400170512         // -?Impostazione nome programma a video?
020500170512         V1Tpgm = SDSpgm;
020600170512
020700170512         // -?Impostazione videata iniziale?
020800170512         $Video = 'D1';
020900170512         reset  $InzD01;
021000170512
021100170512       ENDSR;
021200170512
021300170512       //--------------------------------------------------------------
021400170512       //?Reperimento Dati del job (Utente/Operativi).                 ?
021500170512       //--------------------------------------------------------------
021600170512       BEGSR  sr_DatiJob;
021700170512
021800170512         in(E) §AzUte;
021900170512         if NOT %error;
022000170512           in(E) §DatiUte;
022100170512         endif;
022200170512         if %error or RSut = *blanks;
022300170512           clear TIBS34ds;
022400170512           tibs34r ( tibs34ds );
022500170512           in §AzUte;
022600170512           in §DatiUte;
022700170512         endif;
022800170512
022900170512       ENDSR;
023000170512
023100170512       //--------------------------------------------------------------
023200170512       //?Gestione videata D01.                                        ?
023300170512       //--------------------------------------------------------------
023400170512       BEGSR  sr_GesD01;
023500170512
023600170512         // -?Inizializzazione videata?
023700170512         if  $InzD01 = *on;
023800170512           exsr  sr_InzD01;
023900170512           $InzD01 = *off;
024000170512         endif;
024100170512
024200170512         // -?Emissione videata?
024300170512         write  MS28T01;
024400170512         exfmt  MS28D01;
024500170512
024600170512         clear  VIDmsg;
024700170512         reset  $ErrMessage;
024800170512         reset  $ErrGenerico;
024900170512
025000170512         Select;
025100170512
025200170512           // -?Sistema Informativo errato?
025300170512           when  DUTlpo <> 'S'  or
025400170512                 %subst( KNSIF : 1 : 3 ) <> 'GAI';
025500170512             $Fine = *on;
025600170512
025700170512           // -?Utente NON abilitato (non EDP)?
025800170512           when  UTEfun <> 'EDPSE'  or
025900170512                 %subst( knmus : 1 : 3 ) <> 'EDP';
026000170512             $Fine = *on;
026100170512
026200170512           // -?F3=Fine?
026300170512           When  $dsp_aid = c_F03;
026400170512             $Fine = *on;
026500170512
026600170512           // -?Enter / F6=Conferma?
026700170512           Other;
026800170512             // -?Controlli?
026900170512             exsr  sr_CtrD01;
027000170512             select;
027100170512               // -?Rilevato errore?
027200170512               when  $ErrGenerico;
027300170512                 leavesr;
027400170512               // -?Sottomissione aggiornamento?
027500170512               when  $dsp_aid = c_F06;
027600170512                 exsr  sr_SbmJob;
027700170512                 $Fine = *on;
027800170512             endsl;
027900170512
028000170512         ENDSL;
028100170512
028200170512       ENDSR;
028300170512
028400170512       //--------------------------------------------------------------
028500170512       //?Inizializzazione videata D01.                                ?
028600170512       //--------------------------------------------------------------
028700170512       BEGSR  sr_InzD01;
028800170512
028900170512         // -?Spegnimento degli indicatori di errore?
029000170512         %subst( IndDspF : 50 ) = *off;
029100170512
029200170512         // -?Pulizia videata?
029300170512         clear  MS28D01;
029400170512
029500170512         // -?Impostazione dati di default SE immissione?
029600170512         V1Cfil  = *all'9';
029700170512         //*·V1CdtlI = 01022017;
029800170512         //*·V1CdtlF = %dec( %date() - %days(1) );
029900170512
030000170512
030100170512         select;
030200170512
030300170512           // -?Controllo sistema informativo?
030400170512           when  DUTlpo <> 'S';
030500170512             VIDmsg = sk_Msg(01);
030600170512             $ErrMessage  = *on;
030700170512             $ErrGenerico = *on;
030800170512             leavesr;
030900170512
031000170512         // -?Controllo utente?
031100170512           when  UTEfun <> 'EDPSE';
031200170512             VIDmsg = sk_Msg(02);
031300170512             $ErrMessage  = *on;
031400170512             $ErrGenerico = *on;
031500170512             leavesr;
031600170512
031700170512         endsl;
031800170512
031900170512       ENDSR;
032000170512
032100170512       //--------------------------------------------------------------
032200170512       //?Controllo dati videata D01.                                  ?
032300170512       //--------------------------------------------------------------
032400170512       BEGSR  sr_CtrD01;
032500170512
032600170512         // -?Spegnimento degli indicatori di errore?
032700170512         %subst(IndDspF : 50) = *off;
032800170512
032900170512         // -?Controllo immissione codice Filiale?
033000170512         clear  V1Dfil;
033100170512         If  V1Cfil = *zero;
033200170512           VIDmsg = sk_Msg(03);
033300170512           $PosCurFIL   = *on;
033400170512           $ErrMessage  = *on;
033500170512           $ErrGenerico = *on;
033600170512           leavesr;
033700170512         EndIf;
033800170512
033900170512         // -?Verifica esistenza in AZORG / Decodifica Filiale?
034000170512         chain  ( V1Cfil )  AZORG;
034100170512         if  %found( AZORG01L );
034200170512           V1Dfil   = ORGdes;
034300170512         endif;
034400170512
034500170512         if  NOT %found( AZORG01L )  or
034600170512             ORGfva <> *blank;
034700170512           VIDmsg = sk_Msg(04);
034800170512           $PosCurFIL   = *on;
034900170512           $ErrMessage  = *on;
035000170512           $ErrGenerico = *on;
035100170512           leavesr;
035200170512         endif;
035300170512
035400170512         // -?Controllo Data iniziale?
035500170512         clear  WLBdat;
035600170512         G08dat = V1CdtlI;
035700170512         XSRDA8(WLBdat);
035800170512         if  G08err = *zero;
035900170512           V1CdtlI = G08dat;
036000170512           W1CdtlI = G08inv;
036100170512         else;
036200170512           VIDmsg = sk_Msg(05);
036300170512           $PosCurDTLI  = *on;
036400170512           $ErrMessage  = *on;
036500170512           $ErrGenerico = *on;
036600170512           leavesr;
036700170512         endif;
036800170512
036900170512         // -?Controllo Data finale?
037000170512         clear  WLBdat;
037100170512         G08dat = V1CdtlF;
037200170512         XSRDA8(WLBdat);
037300170512         if  G08err = *zero;
037400170512           V1CdtlF = G08dat;
037500170512           W1CdtlF = G08inv;
037600170512         else;
037700170512           VIDmsg = sk_Msg(05);
037800170512           $PosCurDTLF  = *on;
037900170512           $ErrMessage  = *on;
038000170512           $ErrGenerico = *on;
038100170512           leavesr;
038200170512         endif;
038300170512
038400170512         // -?Controllo sequenza date da riestrarre?
038500170512         if  W1CdtlI > W1CdtlF;
038600170512           VIDmsg = sk_Msg(06);
038700170512           $PosCurDTLI  = *on;
038800170512           $ErrMessage  = *on;
038900170512           $ErrGenerico = *on;
039000170512           leavesr;
039100170512         endif;
039200170512
039300170512         // -?Controllo ultima data (max ieri)?
039400170512         if  W1CdtlF >= wDate;
039500170512           VIDmsg = sk_Msg(07);
039600170512           $PosCurDTLI  = *on;
039700170512           $ErrMessage  = *on;
039800170512           $ErrGenerico = *on;
039900170512           leavesr;
040000170512         endif;
040100170512
040200170512       ENDSR;
040300170512
040400170512       //--------------------------------------------------------------
040500170512       //?Sottomissione lavoro batch.                                  ?
040600170512       //--------------------------------------------------------------
040700170512       BEGSR  sr_SbmJob;
040800170512
040900170512         clear  FIMS26ds;
041000170512         FIMS26ds.iMS26fil = V1Cfil;
041100170512         FIMS26ds.iMS26dti = V1CdtlI;
041200170512         FIMS26ds.iMS26hmi = *zero;
041300170512         FIMS26ds.iMS26dtf = V1CdtlF;
041400170512         FIMS26ds.iMS26hmf = *all'9';
041500170512
041600170512         kPjBu = FIMS26ds;
041700170512         kCoAz = 'MS26';
041800170512         clear kBuff;
041900170512
042000170512         if  kNmUs = *all'1';
042100170512           // -?Richiamo interattivo?
042200170512           FIMS26R ( kpjba );
042300170512           FIMS26ds = kPjBu;
042400170512           if  FIMS26ds.oMS26err = *on;
042500170512             VIDmsg = FIMS26ds.oMS26msg;
042600170512             $ErrMessage  = *on;
042700170512             $ErrGenerico = *on;
042800170512           endif;
042900170512         else;
043000170512           // -?Sottomissione batch?
043100170512           BCH10 ( kpjba );
043200170512         endif;
043300170512
043400170512       ENDSR;
043500170512
043600170512       //--------------------------------------------------------------
043700170512       //?Operazioni finali.                                           ?
043800170512       //--------------------------------------------------------------
043900170512       BEGSR  sr_RoutEnd;
044000170512
044100170512         //  ?Uscita?
044200170512         return;
044300170512
044400170512       ENDSR;
044500170512
044600170512      /end-free
044700170512
044800170512       //--------------------------------------------------------------
044900170512       //?Definizione schiere a tempo di compilazione.                 ?
045000170512       //--------------------------------------------------------------
045100170512
045200170512** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
045300170512Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire        1
045400170512Utente NON abilitato alla funzione                                              2
045500170512Immettere la Filiale                                                            3
045600170512Filiale errata                                                                  4
045700170512Data formalmente errata                                                         5
045800170512Data iniziale  da riestrarre  successiva  a quella finale                       6
045900170512Data successiva all'ultima estratta                                             7
