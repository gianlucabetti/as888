000100140416       //==============================================================
000200140416       //?FIDND6R * Crea WrkF x Stat. R.A. motivo apertura "  8"/" 88" ?
000300140416       //==============================================================
000400140416
000500140416       //--------------------------------------------------------------
000600140416       //?Specifiche di controllo.                                     ?
000700140416       //--------------------------------------------------------------
000800140416
000900140416     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000140416
001100140416       //--------------------------------------------------------------
001200140416       //?Dichiarazione file.                                          ?
001300140416       //--------------------------------------------------------------
001400140416
001500140416       // -?File Video?
001600140416     fFIDND6D   cf   e             workstn
001700140416     f                                     indds(IndDspF)
001800140416     f                                     infds(InfDspF)
001900140416
002000140416       //--------------------------------------------------------------
002100140416       //?Definizione costanti.                                        ?
002200140416       //--------------------------------------------------------------
002300140416
002400140416       // -?Tasti funzionali a video?
002500140416     d c_F01           c                   const(x'31')
002600140416     d c_F02           c                   const(x'32')
002700140416     d c_F03           c                   const(x'33')
002800140416     d c_F04           c                   const(x'34')
002900140416     d c_F05           c                   const(x'35')
003000140416     d c_F06           c                   const(x'36')
003100140416     d c_F07           c                   const(x'37')
003200140416     d c_F08           c                   const(x'38')
003300140416     d c_F09           c                   const(x'39')
003400140416     d c_F10           c                   const(x'3A')
003500140416     d c_F11           c                   const(x'3B')
003600140416     d c_F12           c                   const(x'3C')
003700140416     d c_F13           c                   const(x'B1')
003800140416     d c_F14           c                   const(x'B2')
003900140416     d c_F15           c                   const(x'B3')
004000140416     d c_F16           c                   const(x'B4')
004100140416     d c_F17           c                   const(x'B5')
004200140416     d c_F18           c                   const(x'B6')
004300140416     d c_F19           c                   const(x'B7')
004400140416     d c_F20           c                   const(x'B8')
004500140416     d c_F21           c                   const(x'B9')
004600140416     d c_F22           c                   const(x'BA')
004700140416     d c_F23           c                   const(x'BB')
004800140416     d c_F24           c                   const(x'BC')
004900140416     d c_Enter         c                   const(x'F1')
005000140416     d c_RollDown      c                   const(x'F4')
005100140416     d c_RollUp        c                   const(x'F5')
005200140416
005300140416       //--------------------------------------------------------------
005400140416       //?Definizione schiere.                                         ?
005500140416       //--------------------------------------------------------------
005600140416
005700140416       // -?Messaggi di errore?
005800140416     d sk_Msg          s             78    dim(10) ctdata perrcd(1)
005900140416
006000140416       //--------------------------------------------------------------
006100140416       //?Definizione aree dati.                                       ?
006200140416       //--------------------------------------------------------------
006300140416
006400140416       // -?Dati utente?
006500140416     d §AzUte        e ds                  extname(AZUTE00F)
006600140416     d                                     dtaara
006700140416     d §DatiUte      e ds                  extname(dDatiUte)
006800140416     d                                     dtaara
006900140416
007000140416       //--------------------------------------------------------------
007100140416       //?Definizione strutture dati.                                  ?
007200140416       //--------------------------------------------------------------
007300140416
007400140416       // -?Status ds?
007500140416     d Status         sds
007600140416     d   SDSpgm          *proc
007700140416
007800140416       // -?InfDS?
007900140416     d InfDspF         ds
008000140416     d   dsp_aid             369    369a
008100140416
008200140416       // -?Indicatori su DspF?
008300140416     d IndDspF         ds
008400140416         // -?Indicatori di errore?
008500140416     d   ErrMessage                    n   overlay(IndDspF : 28)
008600140416     d   PosCurDTI                     n   overlay(IndDspF : 50)
008700140416     d   PosCurDTF                     n   overlay(IndDspF : 51)
008800140416         // -?Indicatori di errore generico?
008900140416     d   ErrGenerico                   n   overlay(IndDspF : 99)
009000140416
009100140416       // -?Parametri ricevuti?
009200140416     d KPJBA         e ds
009300140416
009400140416       // -?Passaggio parametri al *pgm batch - FIDN07R?
009500140416     d FIDND7ds      e ds                  inz
009600140416
009700140416       //--------------------------------------------------------------
009800140416       //?Definizione variabili globali.                               ?
009900140416       //--------------------------------------------------------------
010000140416
010100140416       // -?Flags booleani?
010200140416     d $Fine           s               n   inz
010300140416     d $End            s               n   inz
010400140416     d $ErrGrave       s               n   inz
010500140416     d $InzD01         s               n   inz(*on)
010600140416
010700140416       // -?Variabili per la gestione del video?
010800140416     d $Video          s              2    inz('D1')
010900140416
011000140416       // -?Campi di comodo
011100140416     d wDtaDal         s              8s 0 inz
011200140416     d wDtaAl          s              8s 0 inz
011300140416
011400140416       //--------------------------------------------------------------
011500140416       //?Definizione prototipi procedure e relativi parametri.        ?
011600140416       //--------------------------------------------------------------
011700140416
011800140416       // -?Reperimento dati utente?
011900140416     d TIBS34ds      e ds                  inz
012000140416      /copy gaitrasrc/srcProtoPR,TIBS34R
012100140416
012200140416       // -?Controllo/Inversione data?
012300140416     d WLBdat          ds                  inz
012400140416     d   G08dat                       8  0 inz
012500140416     d   G08inv                       8  0 inz
012600140416     d   G08err                       1    inz('3')
012700140416     d   G08tgi                       5  0 inz
012800140416      /copy gaitrasrc/srcProtoPR,XSRDA8
012900140416
013000140416       // -?Sottomissione lavoro batch?
013100140416      /copy gaitrasrc/srcProtoPR,BCH10
013200140416
013300140416       // -?Richiamo diretto lavoro batch?
013400140416     d fidnD7r         pr                  extpgm('FIDND7R')
013500140416     d   kpjba                             likeds(KPJBA)
013600140416
013700140416       // -?Parametri API QCAPCMD (Process Commands)?
013800140416     d Qcmd            s           2048    inz  varying
013900140416      /copy qSysInc/qRpgleSrc,QCAPCMD
014000140416       // -?API QCAPCMD (Process Commands)?
014100140416      /copy gaitrasrc/srcProtoPR,QCAPCMD
014200140416
014300140416       // -?Parametri gestione errori API.?
014400140416      /copy qsysinc/qrpglesrc,QUSEC
014500140416
014600140416       //--------------------------------------------------------------
014700140416       //?Definizione key-list.                                        ?
014800140416       //--------------------------------------------------------------
014900140416
015000140416
015100140416       //--------------------------------------------------------------
015200140416       //?M A I N - L I N E                                            ?
015300140416       //--------------------------------------------------------------
015400140416
015500140416     c     *Entry        plist
015600140416     c                   parm                    KPJBA
015700140416
015800140416      /free
015900140416
016000140416       // -?Operazioni iniziali?
016100140416       exsr  sr_RoutInz;
016200140416
016300140416       // -?Gestione videate?
016400140416       DoW  Not $Fine;
016500140416
016600140416         select;
016700140416           // -?Gestione filtro di lancio?
016800140416           when  $Video = 'D1';
016900140416             exsr  sr_GesD01;
017000140416           // -?? ? ? ? ??
017100140416           other;
017200140416             $Fine = *on;
017300140416         endsl;
017400140416
017500140416       EndDo;
017600140416
017700140416       // -?Operazioni finali?
017800140416       exsr  sr_RoutEnd;
017900140416
018000140416       //--------------------------------------------------------------
018100140416       //?Operazioni iniziali.                                         ?
018200140416       //--------------------------------------------------------------
018300140416       BEGSR  sr_RoutInz;
018400140416
018500140416         // -?Reperimento dati job?
018600140416         exsr  sr_DatiJob;
018700140416
018800140416         // -?Impostazione nome programma a video?
018900140416         V01pgm = SDSpgm;
019000140416
019100140416       ENDSR;
019200140416
019300140416       //--------------------------------------------------------------
019400140416       //?Reperimento Dati del job (Utente/Operativi).                 ?
019500140416       //--------------------------------------------------------------
019600140416       BEGSR  sr_DatiJob;
019700140416
019800140416         in(E) §AzUte;
019900140416         if  NOT %error;
020000140416           in(E) §DatiUte;
020100140416         endif;
020200140416         if  %error or RSut = *blanks;
020300140416           clear TIBS34ds;
020400140416           tibs34r ( tibs34ds );
020500140416           in §AzUte;
020600140416           in §DatiUte;
020700140416         endif;
020800140416
020900140416       ENDSR;
021000140416
021100140416       //--------------------------------------------------------------
021200140416       //?Gestione videata D01.                                        ?
021300140416       //--------------------------------------------------------------
021400140416       BEGSR  sr_GesD01;
021500140416
021600140416         // -?Inizializzazione videata?
021700140416         if  $InzD01;
021800140416           exsr  sr_InzD01;
021900140416           $InzD01 = *off;
022000140416         endif;
022100140416
022200140416         // -?Emissione videata?
022300140416         write  DND6T01;
022400140416         exfmt  DND6D01;
022500140416
022600140416         clear V01msg;
022700140416         ErrMessage   = *off;
022800140416         ErrGenerico  = *off;
022900140416
023000140416         SELECT;
023100140416
023200140416           // -?F3=Fine?
023300140416           WHEN  dsp_aid = c_F03;
023400140416             $Fine = *on;
023500140416
023600140416           // -?F8=Visualizzazione WrkF WFRAP10F?
023700140416           WHEN  dsp_aid = c_F08;
023800140416             exsr  sr_F08D01;
023900140416
024000140416           // -?Invio / F6=Conferma?
024100140416           OTHER;
024200140416             // -?Controlli?
024300140416             exsr  sr_CtrD01;
024400140416             if  ErrGenerico;
024500140416               leavesr;
024600140416             endif;
024700140416             // -?F6 = Lancio batch (se NO errori)?
024800140416             if  ErrGenerico = *off  and  dsp_aid = c_F06;
024900140416               exsr  sr_F06D01;
025000140416               $Fine = *on;
025100140416             endif;
025200140416
025300140416         ENDSL;
025400140416
025500140416       ENDSR;
025600140416
025700140416       //--------------------------------------------------------------
025800140416       //?Inizializzazione videata D01.                                ?
025900140416       //--------------------------------------------------------------
026000140416       BEGSR  sr_InzD01;
026100140416
026200140416         clear  DND6D01;
026300140416
026400140416       ENDSR;
026500140416
026600140416       //--------------------------------------------------------------
026700140416       //?Controllo videata D01.                                       ?
026800140416       //--------------------------------------------------------------
026900140416       BEGSR  sr_CtrD01;
027000140416
027100140416         %subst(IndDspF : 50) = *off;
027200140416
027300140416         clear  ID7dti;
027400140416         clear  ID7dtf;
027500140416
027600140416         // -?Controllo data creazione iniziale?
027700140416         if  V01dtI = *zero;
027800140416           ErrGenerico = *on;
027900140416           ErrMessage  = *on;
028000140416           PosCurDTI   = *on;
028100140416           V01msg = sk_Msg(01);
028200140416           leavesr;
028300140416         endif;
028400140416
028500140416         clear  WLBdat;
028600140416         G08dat = V01dtI;
028700140416         XSRDA8(WLBdat);
028800140416         if  G08err = *on;
028900140416           ErrGenerico = *on;
029000140416           ErrMessage  = *on;
029100140416           PosCurDTI   = *on;
029200140416           V01msg = sk_Msg(01);
029300140416           leavesr;
029400140416         endif;
029500140416         V01dtI = G08dat;
029600140416         iD7dti = G08inv;
029700140416
029800140416         // -?La data iniziale NON può essere antecedente al 16/05/2013?
029900140416         //  ?(giorno di attivazione del nuovo motivo di apertura R.A.)?
030000140416         If  iD7dti < 20130516;
030100140416           ErrMessage  = *on;
030200140416           ErrGenerico = *on;
030300140416           PosCurDTI   = *on;
030400140416           V01msg = %trimr(sk_Msg(01)) +
030500140416                    ': è inferiore alla data di attivazione (16/05/2013)';
030600140416           leavesr;
030700140416         EndIf;
030800140416
030900140416         // -?Controllo data creazione finale?
031000140416         clear  WLBdat;
031100140416         G08dat = V01dtF;
031200140416         XSRDA8(WLBdat);
031300140416         if  G08err = *on;
031400140416           ErrGenerico = *on;
031500140416           ErrMessage  = *on;
031600140416           PosCurDTF   = *on;
031700140416           V01msg = sk_Msg(01);
031800140416           leavesr;
031900140416         endif;
032000140416         V01dtF = G08dat;
032100140416         iD7dtF = G08inv;
032200140416
032300140416         // -?Data iniziale congruente con data finale?
032400140416         If  iD7dtI > iD7dtF;
032500140416           ErrGenerico = *on;
032600140416           ErrMessage  = *on;
032700140416           PosCurDTI   = *on;
032800140416           V01msg = sk_Msg(02);
032900140416           leavesr;
033000140416         EndIf;
033100140416
033200140416       ENDSR;
033300140416
033400140416       //--------------------------------------------------------------
033500140416       //?Gestione tasto funzionale F6 da videata D01:                 ?
033600140416       //?F6=Conferma                                                  ?
033700140416       //--------------------------------------------------------------
033800140416       BEGSR  sr_F06D01;
033900140416
034000140416         // -?Impostazione parametri rimanenti?
034100140416         Kcoaz = 'DND7';
034200140416         Kpjbu = fidnD7ds;
034300140416         Kbuff = *blank;
034400140416
034500140416         if  Knmus = *all'1';
034600140416           // -?Richiamo interattivo?
034700140416           fidnD7r ( kpjba );
034800140416         else;
034900140416           // -?Sottomissione batch?
035000140416            bch10 ( kpjba );
035100140416         endif;
035200140416
035300140416       ENDSR;
035400140416
035500140416       //--------------------------------------------------------------
035600140416       //?Gestione tasto funzionale F8 da videata D01                  ?
035700140416       //?F8=Visualizzazione file WFRAP10F                             ?
035800140416       //--------------------------------------------------------------
035900140416       BEGSR  sr_F08D01;
036000140416
036100140416         Qcmd = 'DSPDBF FILE(';
036200140416         if  %subst( KNSIF : 7 : 1) = 'P';
036300140416           Qcmd += 'GAITRAAZP';
036400140416         else;
036500140416           Qcmd += 'GAITRAAZM';
036600140416         endif;
036700140416         Qcmd += '/WFRAP10F) mbr(*first) output(*)';
036800140416
036900140416         exsr  sr_ExecCmd;
037000140416
037100140416       ENDSR;
037200140416
037300140416       //--------------------------------------------------------------
037400140416       //?Esecuzione del comando (già impostato).                      ?
037500140416       //--------------------------------------------------------------
037600140416       BEGSR  sr_ExecCmd;
037700140416
037800140416         clear Qcap0100;
037900140416         Qcabcsdh = *off;
038000140416         Qcapa    = *off;
038100140416         Qcacmdss = *off;
038200140416         Qcaerved = *allX'00';
038300140416
038400140416         clear Qusec;
038500140416         Qusbprv  = %size(Qusec);
038600140416
038700140416         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
038800140416                           %size(Qcap0100) : 'CPOP0100' : *omit :
038900140416                           0 : 0 : Qusec);
039000140416
039100140416         //if  Qusei <> *blank;
039200140416         //  ...;
039300140416         //endif;
039400140416
039500140416       ENDSR;
039600140416
039700140416       //--------------------------------------------------------------
039800140416       //?Operazioni finali.                                           ?
039900140416       //--------------------------------------------------------------
040000140416       BEGSR  sr_RoutEnd;
040100140416
040200140416         *inLR = *on;
040300140416
040400140416         // -?Uscita dal *pgm?
040500140416         return;
040600140416
040700140416       ENDSR;
040800140416
040900140416      /end-free
041000140416
041100140416       //--------------------------------------------------------------
041200140416       //?Schiere a tempo di compilazione.
041300140416       //--------------------------------------------------------------
041400140416
041500140416** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
041600140416Data errata                                                                    1
041700140416Data "DAL" maggiore di data "AL"                                               2
