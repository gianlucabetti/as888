000100110627       //==============================================================
000200110627       //?TNSS06R * Statistica utilizzo EasySped/EasySpedWeb (Filtro)  ?
000300110627       //==============================================================
000400110627
000500110627       //--------------------------------------------------------------
000600110627       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110627       //--------------------------------------------------------------
000800110627
000900110627     /*PRM dbgview(*source)
001000110627     /*END
001100110627
001200110627       //--------------------------------------------------------------
001300110627       //?Specifiche di controllo.                                     ?
001400110627       //--------------------------------------------------------------
001500110627
001600110627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110627     h dftactgrp(*no)
001800110627
001900110627       //--------------------------------------------------------------
002000110627       //?Dichiarazione file.                                          ?
002100110627       //--------------------------------------------------------------
002200110627
002300110627       // -?Video?
002400110627     fTNSS06D   cf   e             workstn
002500110627     f                                     indds(IndDspF)
002600110627     f                                     infds(InfDspF)
002700110627
002800110627       //--------------------------------------------------------------
002900110627       //?Definizione costanti.                                        ?
003000110627       //--------------------------------------------------------------
003100110627
003200110627       // -?Tasti funzionali a video?
003300110627     d c_F01           c                   const(x'31')
003400110627     d c_F02           c                   const(x'32')
003500110627     d c_F03           c                   const(x'33')
003600110627     d c_F04           c                   const(x'34')
003700110627     d c_F05           c                   const(x'35')
003800110627     d c_F06           c                   const(x'36')
003900110627     d c_F07           c                   const(x'37')
004000110627     d c_F08           c                   const(x'38')
004100110627     d c_F09           c                   const(x'39')
004200110627     d c_F10           c                   const(x'3A')
004300110627     d c_F11           c                   const(x'3B')
004400110627     d c_F12           c                   const(x'3C')
004500110627     d c_F13           c                   const(x'B1')
004600110627     d c_F14           c                   const(x'B2')
004700110627     d c_F15           c                   const(x'B3')
004800110627     d c_F16           c                   const(x'B4')
004900110627     d c_F17           c                   const(x'B5')
005000110627     d c_F18           c                   const(x'B6')
005100110627     d c_F19           c                   const(x'B7')
005200110627     d c_F20           c                   const(x'B8')
005300110627     d c_F21           c                   const(x'B9')
005400110627     d c_F22           c                   const(x'BA')
005500110627     d c_F23           c                   const(x'BB')
005600110627     d c_F24           c                   const(x'BC')
005700110627     d c_Enter         c                   const(x'F1')
005800110627     d c_RollDown      c                   const(x'F4')
005900110627     d c_RollUp        c                   const(x'F5')
006000110627
006100110627       //--------------------------------------------------------------
006200110627       //?Definizione schiere.                                         ?
006300110627       //--------------------------------------------------------------
006400110627
006500110627       // -?Messaggi di errore?
006600110627     d $Msg            s             78    dim( 3)  ctdata  perrcd(1)
006700110627
006800110627       //--------------------------------------------------------------
006900110627       //?Definizione aree dati.                                       ?
007000110627       //--------------------------------------------------------------
007100110627
007200110627       // -?Dati utente?
007300110627     d §AzUte        e ds                  extname(AZUTE00F)
007400110627     d                                     dtaara
007500110627     d §DatiUte      e ds                  extname(dDatiUte)
007600110627     d                                     dtaara
007700110627
007800110627       //--------------------------------------------------------------
007900110627       //?Definizione strutture dati.                                  ?
008000110627       //--------------------------------------------------------------
008100110627
008200110627       // -?Status ds?
008300110627     d Status         sds
008400110627     d   SDSpgm          *proc
008500110627
008600110627       // -?InfDS?
008700110627     d InfDspF         ds
008800110627     d   dsp_aid             369    369a
008900110627
009000110627       // -?Indicatori su DspF?
009100110627     d IndDspF         ds                  inz
009200110627         // -?Abilitazione tasti funzionali?
009300110627         // -?Emissione messaggio di errore?
009400110627     d   ErrMessage                    n   overlay(IndDspF : 28)
009500110627         // -?Indicatori per Attibuti di visualizzazione?
009600110627         // -?Posizionamento cursore & Segnalazione errore?
009700110627     d   PosCurDSI                     n   overlay(IndDspF : 51)
009800110627     d   PosCurDSF                     n   overlay(IndDspF : 52)
009900110627     d   PosCurCVER                    n   overlay(IndDspF : 53)
010000110627     d   PosCurCDSC                    n   overlay(IndDspF : 54)
010100110627         //   -?Riemissione videata?
010200110627     d   ErrGenerico                   n   overlay(IndDspF : 99)
010300110627
010400110627       // -?Parametri ricevuti?
010500110627     d KPJBA         e ds
010600110627
010700110627       // -?Parametri per pgm. batch - TNSS07R?
010800110627     d TNSS07ds      e ds                  inz
010900110627     d   D07dti      e                     inz(*loval)
011000110627     d   D07dtf      e                     inz(*hival)
011100110627
011200110627       //--------------------------------------------------------------
011300110627       //?Definizione variabili globali.                               ?
011400110627       //--------------------------------------------------------------
011500110627
011600110627       // -?Flags booleani?
011700110627     d $Fine           s               n   inz
011800110627     d $InzD01         s               n   inz(*on)
011900110627
012000110627       // -?Variabili per la gestione del video?
012100110627     d $Video          s              2    inz('D1')
012200110627
012300110627       // -?Campi di comodo - data?
012400110627     d W1Cdsi          s              8  0 inz
012500110627     d W1Cdsf          s              8  0 inz
012600110627
012700110627       //--------------------------------------------------------------
012800110627       //?Definizione prototipi procedure.                             ?
012900110627       //--------------------------------------------------------------
013000110627
013100110627       // -?Reperimento dati utente?
013200110627     d TIBS34ds      e ds                  inz
013300110627      /copy gaitrasrc/srcProtoPR,TIBS34R
013400110627
013500110627       // -?Controllo/Inversione data?
013600110627     d WLBdat          ds                  inz
013700110627     d  G08dat                        8  0 inz
013800110627     d  G08inv                        8  0 inz
013900110627     d  G08err                        1    inz('3')
014000110627     d  G08tgi                        5  0 inz
014100110627      /copy gaitrasrc/srcProtoPR,XSRDA8
014200110627
014300110627       // -?Sottomissione lavoro batch?
014400110627      /copy gaitrasrc/srcProtoPR,BCH10
014500110627
014600110627       // -?Richiamo diretto lavoro batch?
014700110627     d tnss07r         pr                  extpgm('TNSS07R')
014800110627     d   kpjba                             likeds(KPJBA)
014900110627
015000110627       // -?Parametri API QCAPCMD (Process Commands)?
015100110627     d Qcmd            s           2048    inz  varying
015200110627      /copy qSysInc/qRpgleSrc,QCAPCMD
015300110627       // -?API QCAPCMD (Process Commands)?
015400110627      /copy gaitrasrc/srcProtoPR,QCAPCMD
015500110627
015600110627       // -?Parametri gestione errori API.?
015700110627      /copy qsysinc/qrpglesrc,QUSEC
015800110627
015900110627       //--------------------------------------------------------------
016000110627       //?Definizione key-list.                                        ?
016100110627       //--------------------------------------------------------------
016200110627
016300110627
016400110627       //--------------------------------------------------------------
016500110627       //?M A I N - L I N E                                            ?
016600110627       //--------------------------------------------------------------
016700110627
016800110627     c     *Entry        plist
016900110627     c                   parm                    KPJBA
017000110627
017100110627      /free
017200110627
017300110627       // -?Operazioni iniziali?
017400110627       exsr sr_RoutInz;
017500110627
017600110627       // -?Gestione videate?
017700110627       DoW  $Fine = *off;
017800110627
017900110627         select;
018000110627           // -?Gestione filtro di lancio?
018100110627           when $Video = 'D1';
018200110627             exsr sr_GesD01;
018300110627           // -?? ? ? ? ??
018400110627           other;
018500110627             $Fine = *on;
018600110627         endsl;
018700110627
018800110627       enddo;
018900110627
019000110627       // -?Operazioni finali?
019100110627       exsr sr_RoutEnd;
019200110627
019300110627       //--------------------------------------------------------------
019400110627       //?Operazioni iniziali.                                         ?
019500110627       //--------------------------------------------------------------
019600110627       BEGSR  sr_RoutInz;
019700110627
019800110627         *inLR = *on;
019900110627
020000110627         // -?Reperimento dati job?
020100110627         exsr  sr_DatiJob;
020200110627
020300110627         // -?Impostazione nome programma a video?
020400110627         V1Cpgm = SDSpgm;
020500110627
020600110627       ENDSR;
020700110627
020800110627       //--------------------------------------------------------------
020900110627       //?Reperimento Dati del job (Utente/Operativi).                 ?
021000110627       //--------------------------------------------------------------
021100110627       BEGSR  sr_DatiJob;
021200110627
021300110627         in(e) §AzUte;
021400110627         if NOT %error;
021500110627           in(e) §DatiUte;
021600110627         endif;
021700110627         if %error or RSut = *blank;
021800110627           tibs34r ( tibs34ds );
021900110627           in §AzUte;
022000110627           in §DatiUte;
022100110627         endif;
022200110627
022300110627       ENDSR;
022400110627
022500110627       //--------------------------------------------------------------
022600110627       //?Operazioni finali.                                           ?
022700110627       //--------------------------------------------------------------
022800110627       BEGSR  sr_RoutEnd;
022900110627
023000110627         return;
023100110627
023200110627       ENDSR;
023300110627
023400110627       //--------------------------------------------------------------
023500110627       //?Gestione videata D01.                                        ?
023600110627       //--------------------------------------------------------------
023700110627       BEGSR  sr_GesD01;
023800110627
023900110627         // -?Inizializzazione videata?
024000110627         if  $InzD01 = *on;
024100110627           exsr  sr_InzD01;
024200110627           $InzD01 = *off;
024300110627         endif;
024400110627
024500110627         // -?Emissione videata completa?
024600110627         exfmt  SS06D01;
024700110627
024800110627         clear  V1Dmsg;
024900110627         reset  ErrMessage;
025000110627         reset  ErrGenerico;
025100110627
025200110627         SELECT;
025300110627
025400110627           // -?Sistema Informativo errato => FINE?
025500110627           WHEN  %subst( KNSIF : 1 : 3 ) = 'FIL';
025600110627             $Fine = *on;
025700110627
025800110627           // -?F3=Fine?
025900110627           WHEN  dsp_aid = c_F03;
026000110627             $Fine = *on;
026100110627
026200110627           // -?F9=Visualizzazione WrkF WFESW10F e WFESW20F?
026300110627           WHEN  dsp_aid = c_F09;
026400110627             exsr  sr_F09D01;
026500110627
026600110627           // -?Invio / F6=Conferma?
026700110627           OTHER;
026800110627             exsr sr_CtrD01;
026900110627             // -?F6 = Lancio batch (se NO errori)?
027000110627             if  ErrGenerico = *off  and  dsp_aid = c_F06;
027100110627               exsr  sr_F06D01;
027200110627               $Fine = *on;
027300110627             endif;
027400110627
027500110627         ENDSL;
027600110627
027700110627       ENDSR;
027800110627
027900110627       //--------------------------------------------------------------
028000110627       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
028100110627       //--------------------------------------------------------------
028200110627       BEGSR  sr_InzD01;
028300110627
028400110627         clear  V1Cdsi;
028500110627         clear  V1Cdsf;
028600110627
028700110627         // -?Ctrl Sistema Informativo per verificare se Sede o Filiale?
028800110627         if  %subst(KNSIF:1:3) = 'FIL';
028900110627           ErrGenerico = *on;
029000110627           ErrMessage  = *on;
029100110627           V1Dmsg = $Msg(1);
029200110627         endif;
029300110627
029400110627       ENDSR;
029500110627
029600110627       //--------------------------------------------------------------
029700110627       //?Gestione tasto funzionale F09 da videata D01.                ?
029800110627       //--------------------------------------------------------------
029900110627       BEGSR  sr_F09D01;
030000110627
030100110627         Qcmd = 'DSPDBF file(*libl/WFES210F) mbr(*first) output(*)';
030200110627         exsr  sr_ExecCmd;
030300110627
030400110627         Qcmd = 'DSPDBF file(*libl/WFES220F) mbr(*first) output(*)';
030500110627         exsr  sr_ExecCmd;
030600110627
030700110627       ENDSR;
030800110627
030900110627       //--------------------------------------------------------------
031000110627       //?Controllo videata D01.                                       ?
031100110627       //--------------------------------------------------------------
031200110627       BEGSR  sr_CtrD01;
031300110627
031400110627         %subst(IndDspF : 50) = *off;
031500110627         clear  D07dti;
031600110627         clear  D07dtf;
031700110627
031800110627         // -?Controllo data spedizione iniziale?
031900110627         clear WLBdat;
032000110627         G08dat = V1Cdsi;
032100110627         XSRDA8(WLBdat);
032200110627         if  G08err = *on;
032300110627           ErrGenerico = *on;
032400110627           ErrMessage  = *on;
032500110627           PosCurDSI   = *on;
032600110627           V1Dmsg = $Msg(02);
032700110627           leavesr;
032800110627         else;
032900110627           V1Cdsi = G08dat;
033000110627           D07dti = G08inv;
033100110627         endif;
033200110627
033300110627         // -?Controllo data spedizione finale?
033400110627         clear WLBdat;
033500110627         G08dat = V1Cdsf;
033600110627         XSRDA8(WLBdat);
033700110627         if  G08err = *on;
033800110627           ErrGenerico = *on;
033900110627           ErrMessage  = *on;
034000110627           PosCurDSF   = *on;
034100110627           V1Dmsg = $Msg(02);
034200110627           leavesr;
034300110627         else;
034400110627           V1Cdsf = G08dat;
034500110627           D07dtf = G08inv;
034600110627         endif;
034700110627
034800110627         // -?Controllo range di date?
034900110627         if  D07dti > D07dtf;
035000110627           ErrGenerico = *on;
035100110627           ErrMessage  = *on;
035200110627           PosCurDSI   = *on;
035300110627           V1Dmsg = $Msg(03);
035400110627           leavesr;
035500110627         endif;
035600110627
035700110627         // -?Ultima versione cappario?
035800110627         D07cver = V1Ccver;
035900110627
036000110627         // -?data scadenza cappario?
036100110627         clear WLBdat;
036200110627         G08dat = V1Ccdsc;
036300110627         XSRDA8(WLBdat);
036400110627         if  G08err = *on;
036500110627           ErrGenerico = *on;
036600110627           ErrMessage  = *on;
036700110627           PosCurCDSC  = *on;
036800110627           V1Dmsg = $Msg(02);
036900110627           leavesr;
037000110627         else;
037100110627           V1Ccdsc = G08dat;
037200110627           D07cdsc = G08inv;
037300110627         endif;
037400110627
037500110627       ENDSR;
037600110627
037700110627       //--------------------------------------------------------------
037800110627       //?Gestione tasto funzionale F6 da videata D01:                 ?
037900110627       //?F6=Conferma                                                  ?
038000110627       //--------------------------------------------------------------
038100110627       BEGSR  sr_F06D01;
038200110627
038300110627         // -?Impostazione parametri?
038400110627         kcoaz = 'SS07';
038500110627         kpjbu = TNSS07ds;
038600110627
038700110627         if  knmus = *all'1';
038800110627           // -?Richiamo interattivo?
038900110627           tnss07r ( kpjba );
039000110627         else;
039100110627           // -?Sottomissione batch?
039200110627            bch10 ( kpjba );
039300110627         endif;
039400110627
039500110627       ENDSR;
039600110627
039700110627       //--------------------------------------------------------------
039800110627       //?Esecuzione del comando (già impostato).                      ?
039900110627       //--------------------------------------------------------------
040000110627       BEGSR  sr_ExecCmd;
040100110627
040200110627         clear Qcap0100;
040300110627         Qcabcsdh = *off;
040400110627         Qcapa    = *off;
040500110627         Qcacmdss = *off;
040600110627         Qcaerved = *allX'00';
040700110627
040800110627         clear Qusec;
040900110627         Qusbprv  = %size(Qusec);
041000110627
041100110627         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
041200110627                           %size(Qcap0100) : 'CPOP0100' : *omit :
041300110627                           0 : 0 : Qusec);
041400110627
041500110627         //if  Qusei <> *blank;
041600110627         //  ...;
041700110627         //endif;
041800110627
041900110627       ENDSR;
042000110627
042100110627       //--------------------------------------------------------------
042200110627       //?Schiere a tempo di compilazione.
042300110627       //--------------------------------------------------------------
042400110627
042500110627** - $Msg:?Messaggi di Errore?-----------------------------------------------*
042600110627Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
042700110627Data in formato errato                                                         2
042800110627Range di date errato                                                           3
