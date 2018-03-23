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
014600170515
014700170515       // -?API QCAPCMD (Process Commands)?
014800170515     d Qcmd            s           2048    inz  varying
014900170515      /copy qSysInc/qRpgleSrc,QCAPCMD
015000170515      /copy gaitrasrc/srcProtoPR,QCAPCMD
015100170515       // -?Parametri gestione errori API.?
015200170515      /copy qsysinc/qRpgleSrc,QUSEC
015300170512
015400170512       //--------------------------------------------------------------
015500170512       //?Definizione key-list.                                        ?
015600170512       //--------------------------------------------------------------
015700170512
015800170512
015900170512       //--------------------------------------------------------------
016000170512       //?M A I N - L I N E                                            ?
016100170512       //--------------------------------------------------------------
016200170512
016300170512     c     *Entry        plist
016400170512     c                   parm                    KPJBA
016500170512
016600170512      /free
016700170512
016800170512       // -?Operazioni iniziali?
016900170512       exsr  sr_RoutInz;
017000170512
017100170512       // -?Ciclo di gestione del file video?
017200170512       DOW  $Fine = *off;
017300170512
017400170512         select;
017500170512
017600170512           // -?Gestione videata D1 (parzializzazioni)?
017700170512           when $Video = 'D1';
017800170512             exsr sr_GesD01;
017900170512
018000170512           // -?? ? ??
018100170512           other;
018200170512             $Fine = *on;
018300170512
018400170512         endsl;
018500170512
018600170512       ENDDO;
018700170512
018800170512       // -?Operazioni finali?
018900170512       exsr  sr_RoutEnd;
019000170512
019100170512       //--------------------------------------------------------------
019200170512       //?Operazioni iniziali.                                         ?
019300170512       //--------------------------------------------------------------
019400170512       BEGSR  sr_RoutInz;
019500170512
019600170512         // -?Impostazione chiusura?
019700170512         *inLR = *on;
019800170512
019900170512         // -?Reperimento dati job?
020000170512         exsr  sr_DatiJob;
020100170512
020200170512         // -?Reperimento data odierna?
020300170512         wDate = %int( %subst( %char( %dec( %timestamp() ) )
020400170512                               : 1 : 8 ) );
020500170512
020600170512         // -?Impostazione nome programma a video?
020700170512         V1Tpgm = SDSpgm;
020800170512
020900170512         // -?Impostazione videata iniziale?
021000170512         $Video = 'D1';
021100170512         reset  $InzD01;
021200170512
021300170512       ENDSR;
021400170512
021500170512       //--------------------------------------------------------------
021600170512       //?Reperimento Dati del job (Utente/Operativi).                 ?
021700170512       //--------------------------------------------------------------
021800170512       BEGSR  sr_DatiJob;
021900170512
022000170512         in(E) §AzUte;
022100170512         if NOT %error;
022200170512           in(E) §DatiUte;
022300170512         endif;
022400170512         if %error or RSut = *blanks;
022500170512           clear TIBS34ds;
022600170512           tibs34r ( tibs34ds );
022700170512           in §AzUte;
022800170512           in §DatiUte;
022900170512         endif;
023000170512
023100170512       ENDSR;
023200170512
023300170512       //--------------------------------------------------------------
023400170512       //?Gestione videata D01.                                        ?
023500170512       //--------------------------------------------------------------
023600170512       BEGSR  sr_GesD01;
023700170512
023800170512         // -?Inizializzazione videata?
023900170512         if  $InzD01 = *on;
024000170512           exsr  sr_InzD01;
024100170512           $InzD01 = *off;
024200170512         endif;
024300170512
024400170512         // -?Emissione videata?
024500170512         write  MS28T01;
024600170512         exfmt  MS28D01;
024700170512
024800170512         clear  VIDmsg;
024900170512         reset  $ErrMessage;
025000170512         reset  $ErrGenerico;
025100170512
025200170512         Select;
025300170512
025400170512           // -?Sistema Informativo errato?
025500170512           when  DUTlpo <> 'S'  or
025600170512                 %subst( KNSIF : 1 : 3 ) <> 'GAI';
025700170512             $Fine = *on;
025800170512
025900170512           // -?Utente NON abilitato (non EDP)?
026000170512           when  UTEfun <> 'EDPSE'  or
026100170512                 %subst( knmus : 1 : 3 ) <> 'EDP';
026200170512             $Fine = *on;
026300170512
026400170512           // -?F3=Fine?
026500170512           When  $dsp_aid = c_F03;
026600170512             $Fine = *on;
026700170515
026800170515           // -?F9=Visualizzazione FIMSP10F?
026900170515           When  $dsp_aid = c_F09;
027000170515             exsr  sr_F09D01;
027100170512
027200170512           // -?Enter / F6=Conferma?
027300170512           Other;
027400170512             // -?Controlli?
027500170512             exsr  sr_CtrD01;
027600170512             select;
027700170512               // -?Rilevato errore?
027800170512               when  $ErrGenerico;
027900170512                 leavesr;
028000170512               // -?Sottomissione aggiornamento?
028100170512               when  $dsp_aid = c_F06;
028200170512                 exsr  sr_SbmJob;
028300170512                 $Fine = *on;
028400170512             endsl;
028500170512
028600170512         ENDSL;
028700170512
028800170512       ENDSR;
028900170512
029000170512       //--------------------------------------------------------------
029100170512       //?Inizializzazione videata D01.                                ?
029200170512       //--------------------------------------------------------------
029300170512       BEGSR  sr_InzD01;
029400170512
029500170512         // -?Spegnimento degli indicatori di errore?
029600170512         %subst( IndDspF : 50 ) = *off;
029700170512
029800170512         // -?Pulizia videata?
029900170512         clear  MS28D01;
030000170512
030100170512         // -?Impostazione dati di default SE immissione?
030200170512         V1Cfil  = *all'9';
030300170512         //*·V1CdtlI = 01022017;
030400170512         //*·V1CdtlF = %dec( %date() - %days(1) );
030500170512
030600170512
030700170512         select;
030800170512
030900170512           // -?Controllo sistema informativo?
031000170512           when  DUTlpo <> 'S';
031100170512             VIDmsg = sk_Msg(01);
031200170512             $ErrMessage  = *on;
031300170512             $ErrGenerico = *on;
031400170512             leavesr;
031500170512
031600170512         // -?Controllo utente?
031700170512           when  UTEfun <> 'EDPSE';
031800170512             VIDmsg = sk_Msg(02);
031900170512             $ErrMessage  = *on;
032000170512             $ErrGenerico = *on;
032100170512             leavesr;
032200170512
032300170512         endsl;
032400170512
032500170512       ENDSR;
032600170515
032700170515       //--------------------------------------------------------------
032800170515       //?Gestione F9 = Visualizzazione FIMSP140F.                     ?
032900170515       //--------------------------------------------------------------
033000170515       BEGSR  sr_F09D01;
033100170515
033200170515         // -?Visualizzazione del work-file WFTDA00F?
033300170515         Qcmd = 'DSPDBF file(*libl/FIMSP10F) rcdslt(*yes)';
033400170515         exsr  sr_ExecCmd;
033500170515
033600170515       ENDSR;
033700170512
033800170512       //--------------------------------------------------------------
033900170512       //?Controllo dati videata D01.                                  ?
034000170512       //--------------------------------------------------------------
034100170512       BEGSR  sr_CtrD01;
034200170512
034300170512         // -?Spegnimento degli indicatori di errore?
034400170512         %subst(IndDspF : 50) = *off;
034500170512
034600170515         // -?Controllo codice Filiale?
034700170512         clear  V1Dfil;
034800170515         If  V1Cfil <> *all'9';
034900170515
035000170515           // -?Verifica inserimento?
035100170515           if  V1Cfil = *zero;
035200170515             VIDmsg = sk_Msg(03);
035300170515             $PosCurFIL   = *on;
035400170515             $ErrMessage  = *on;
035500170515             $ErrGenerico = *on;
035600170515             leavesr;
035700170515           endif;
035800170512
035900170515           // -?Verifica esistenza in AZORG / Decodifica Filiale?
036000170515           chain  ( V1Cfil )  AZORG;
036100170515           if  %found( AZORG01L );
036200170515             V1Dfil   = ORGdes;
036300170515           endif;
036400170515
036500170515           if  NOT %found( AZORG01L )  or
036600170515               ORGfva <> *blank;
036700170515             VIDmsg = sk_Msg(04);
036800170515             $PosCurFIL   = *on;
036900170515             $ErrMessage  = *on;
037000170515             $ErrGenerico = *on;
037100170515             leavesr;
037200170515           endif;
037300170515
037400170515         EndIf;
037500170512
037600170512         // -?Controllo Data iniziale?
037700170512         clear  WLBdat;
037800170512         G08dat = V1CdtlI;
037900170512         XSRDA8(WLBdat);
038000170512         if  G08err = *zero;
038100170512           V1CdtlI = G08dat;
038200170512           W1CdtlI = G08inv;
038300170512         else;
038400170512           VIDmsg = sk_Msg(05);
038500170512           $PosCurDTLI  = *on;
038600170512           $ErrMessage  = *on;
038700170512           $ErrGenerico = *on;
038800170512           leavesr;
038900170512         endif;
039000170512
039100170512         // -?Controllo Data finale?
039200170512         clear  WLBdat;
039300170512         G08dat = V1CdtlF;
039400170512         XSRDA8(WLBdat);
039500170512         if  G08err = *zero;
039600170512           V1CdtlF = G08dat;
039700170512           W1CdtlF = G08inv;
039800170512         else;
039900170512           VIDmsg = sk_Msg(05);
040000170512           $PosCurDTLF  = *on;
040100170512           $ErrMessage  = *on;
040200170512           $ErrGenerico = *on;
040300170512           leavesr;
040400170512         endif;
040500170512
040600170512         // -?Controllo sequenza date da riestrarre?
040700170512         if  W1CdtlI > W1CdtlF;
040800170512           VIDmsg = sk_Msg(06);
040900170512           $PosCurDTLI  = *on;
041000170512           $ErrMessage  = *on;
041100170512           $ErrGenerico = *on;
041200170512           leavesr;
041300170512         endif;
041400170512
041500170512         // -?Controllo ultima data (max ieri)?
041600170512         if  W1CdtlF >= wDate;
041700170512           VIDmsg = sk_Msg(07);
041800170515           $PosCurDTLF  = *on;
041900170512           $ErrMessage  = *on;
042000170512           $ErrGenerico = *on;
042100170512           leavesr;
042200170512         endif;
042300170512
042400170512       ENDSR;
042500170512
042600170512       //--------------------------------------------------------------
042700170512       //?Sottomissione lavoro batch.                                  ?
042800170512       //--------------------------------------------------------------
042900170512       BEGSR  sr_SbmJob;
043000170512
043100170512         clear  FIMS26ds;
043200170515         if  V1Cfil <> *all'9';
043300170515           FIMS26ds.iMS26fil = V1Cfil;
043400170515         endif;
043500170515         FIMS26ds.iMS26dti = W1CdtlI;
043600170512         FIMS26ds.iMS26hmi = *zero;
043700170515         FIMS26ds.iMS26dtf = W1CdtlF;
043800170512         FIMS26ds.iMS26hmf = *all'9';
043900170512
044000170512         kPjBu = FIMS26ds;
044100170512         kCoAz = 'MS26';
044200170512         clear kBuff;
044300170512
044400170512         if  kNmUs = *all'1';
044500170512           // -?Richiamo interattivo?
044600170512           FIMS26R ( kpjba );
044700170512           FIMS26ds = kPjBu;
044800170512           if  FIMS26ds.oMS26err = *on;
044900170512             VIDmsg = FIMS26ds.oMS26msg;
045000170512             $ErrMessage  = *on;
045100170512             $ErrGenerico = *on;
045200170512           endif;
045300170512         else;
045400170512           // -?Sottomissione batch?
045500170512           BCH10 ( kpjba );
045600170512         endif;
045700170512
045800170512       ENDSR;
045900170515
046000170515       //--------------------------------------------------------------
046100170515       //?Esecuzione del comando (già impostato).                      ?
046200170515       //--------------------------------------------------------------
046300170515       BEGSR  sr_ExecCmd;
046400170515
046500170515         clear Qcap0100;
046600170515         Qcabcsdh = *off;
046700170515         Qcapa    = *off;
046800170515         Qcacmdss = *off;
046900170515         Qcaerved = *allX'00';
047000170515
047100170515         clear Qusec;
047200170515         Qusbprv  = %size(Qusec);
047300170515
047400170515         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
047500170515                           %size(Qcap0100) : 'CPOP0100' : *omit :
047600170515                           0 : 0 : Qusec);
047700170515
047800170515         //if  Qusei <> *blank;
047900170515         //  ...;
048000170515         //endif;
048100170515
048200170515       ENDSR;
048300170512
048400170512       //--------------------------------------------------------------
048500170512       //?Operazioni finali.                                           ?
048600170512       //--------------------------------------------------------------
048700170512       BEGSR  sr_RoutEnd;
048800170512
048900170515         // -?Uscita dal *pgm?
049000170512         return;
049100170512
049200170512       ENDSR;
049300170512
049400170512      /end-free
049500170512
049600170512       //--------------------------------------------------------------
049700170512       //?Definizione schiere a tempo di compilazione.                 ?
049800170512       //--------------------------------------------------------------
049900170512
050000170512** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
050100170512Funzione eseguibile SOLO in ambiente di SEDE: premere "Invio" per uscire        1
050200170512Utente NON abilitato alla funzione                                              2
050300170512Immettere la Filiale                                                            3
050400170512Filiale errata                                                                  4
050500170512Data formalmente errata                                                         5
050600170512Data iniziale  da riestrarre  successiva  a quella finale                       6
050700170512Data successiva all'ultima estratta                                             7
