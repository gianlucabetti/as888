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
002200131010
002300131010       // -?Versioni Cappario?
002400131010     fAZCPR01L  if   e           k disk    rename(AZCPR000:AZCPR001)
002500131010     fAZCPR02L  if   e           k disk
002600110627
002700110627       // -?Video?
002800110627     fTNSS06D   cf   e             workstn
002900110627     f                                     indds(IndDspF)
003000110627     f                                     infds(InfDspF)
003100110627
003200110627       //--------------------------------------------------------------
003300110627       //?Definizione costanti.                                        ?
003400110627       //--------------------------------------------------------------
003500110627
003600110627       // -?Tasti funzionali a video?
003700110627     d c_F01           c                   const(x'31')
003800110627     d c_F02           c                   const(x'32')
003900110627     d c_F03           c                   const(x'33')
004000110627     d c_F04           c                   const(x'34')
004100110627     d c_F05           c                   const(x'35')
004200110627     d c_F06           c                   const(x'36')
004300110627     d c_F07           c                   const(x'37')
004400110627     d c_F08           c                   const(x'38')
004500110627     d c_F09           c                   const(x'39')
004600110627     d c_F10           c                   const(x'3A')
004700110627     d c_F11           c                   const(x'3B')
004800110627     d c_F12           c                   const(x'3C')
004900110627     d c_F13           c                   const(x'B1')
005000110627     d c_F14           c                   const(x'B2')
005100110627     d c_F15           c                   const(x'B3')
005200110627     d c_F16           c                   const(x'B4')
005300110627     d c_F17           c                   const(x'B5')
005400110627     d c_F18           c                   const(x'B6')
005500110627     d c_F19           c                   const(x'B7')
005600110627     d c_F20           c                   const(x'B8')
005700110627     d c_F21           c                   const(x'B9')
005800110627     d c_F22           c                   const(x'BA')
005900110627     d c_F23           c                   const(x'BB')
006000110627     d c_F24           c                   const(x'BC')
006100110627     d c_Enter         c                   const(x'F1')
006200110627     d c_RollDown      c                   const(x'F4')
006300110627     d c_RollUp        c                   const(x'F5')
006400110627
006500110627       //--------------------------------------------------------------
006600110627       //?Definizione schiere.                                         ?
006700110627       //--------------------------------------------------------------
006800110627
006900110627       // -?Messaggi di errore?
007000131010     d $Msg            s             78    dim( 5)  ctdata  perrcd(1)
007100110627
007200110627       //--------------------------------------------------------------
007300110627       //?Definizione aree dati.                                       ?
007400110627       //--------------------------------------------------------------
007500110627
007600110627       // -?Dati utente?
007700110627     d §AzUte        e ds                  extname(AZUTE00F)
007800110627     d                                     dtaara
007900110627     d §DatiUte      e ds                  extname(dDatiUte)
008000110627     d                                     dtaara
008100110627
008200110627       //--------------------------------------------------------------
008300110627       //?Definizione strutture dati.                                  ?
008400110627       //--------------------------------------------------------------
008500110627
008600110627       // -?Status ds?
008700110627     d Status         sds
008800110627     d   SDSpgm          *proc
008900110627
009000110627       // -?InfDS?
009100110627     d InfDspF         ds
009200110627     d   dsp_aid             369    369a
009300110627
009400110627       // -?Indicatori su DspF?
009500110627     d IndDspF         ds                  inz
009600110627         // -?Abilitazione tasti funzionali?
009700110627         // -?Emissione messaggio di errore?
009800110627     d   ErrMessage                    n   overlay(IndDspF : 28)
009900110627         // -?Indicatori per Attibuti di visualizzazione?
010000110627         // -?Posizionamento cursore & Segnalazione errore?
010100110627     d   PosCurDSI                     n   overlay(IndDspF : 51)
010200110627     d   PosCurDSF                     n   overlay(IndDspF : 52)
010300110627     d   PosCurCVER                    n   overlay(IndDspF : 53)
010400110627     d   PosCurCDSC                    n   overlay(IndDspF : 54)
010500110627         //   -?Riemissione videata?
010600110627     d   ErrGenerico                   n   overlay(IndDspF : 99)
010700110627
010800110627       // -?Parametri ricevuti?
010900110627     d KPJBA         e ds
011000110627
011100110627       // -?Parametri per pgm. batch - TNSS07R?
011200110627     d TNSS07ds      e ds                  inz
011300110627     d   D07dti      e                     inz(*loval)
011400110627     d   D07dtf      e                     inz(*hival)
011500110627
011600110627       //--------------------------------------------------------------
011700110627       //?Definizione variabili globali.                               ?
011800110627       //--------------------------------------------------------------
011900110627
012000110627       // -?Flags booleani?
012100110627     d $Fine           s               n   inz
012200110627     d $InzD01         s               n   inz(*on)
012300110627
012400110627       // -?Variabili per la gestione del video?
012500110627     d $Video          s              2    inz('D1')
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
028600131010         clear  V1Ccver;
028700131010         clear  V1Ccdsc;
028800110627
028900110627         // -?Ctrl Sistema Informativo per verificare se Sede o Filiale?
029000110627         if  %subst(KNSIF:1:3) = 'FIL';
029100110627           ErrGenerico = *on;
029200110627           ErrMessage  = *on;
029300110627           V1Dmsg = $Msg(1);
029400110627         endif;
029500131010
029600131010         // -?Proposta del range di date spedizione?
029700131010         if  %subdt( %date() : *days ) >=  5  and
029800131010             %subdt( %date() : *days ) <= 15;
029900131010           if  %subdt( %date() : *months ) <> 01;
030000131010             V1Cdsi = 01000000
030100131010                    + (( %subdt( %date() : *months ) - 1 ) * 10000 )
030200131010                    + *year;
030300131010             V1Cdsf = %dec( %date(
030400131010                            ( 01000000 +
030500131010                              ( ( %subdt( %date() : *months ) ) * 10000 ) +
030600131010                              *year ) : *eur ) - %days(1) : *eur );
030700131010           else;
030800131010             V1Cdsi = 01120000 + ( *year - 1 );
030900131010             V1Cdsf = 31120000 + ( *year - 1 );
031000131010           endif;
031100131010         endif;
031200131010
031300131010         // -?Proposta della versione del cappario?
031400131010         setgt  ( %dec( %date() ) )  AZCPR000;
031500131010         readp  AZCPR000;
031600131010         DoW  Not  %eof(AZCPR02L)       and  CPRdde >  %dec( %date() );
031700131010           readp  AZCPR000;
031800131010         EndDo;
031900131010         if  Not  %eof(AZCPR02L)        and
032000131010             CPRdde <= %dec( %date() )  and  CPRdsc >= %dec( %date() );
032100131010           V1Ccver = CPRver;
032200131010           V1Ccdsc = ( %subdt( %date( CPRdsc : *iso ) : *days ) * 1000000 ) +
032300131010                     ( %subdt( %date( CPRdsc : *iso ) : *months ) * 10000 ) +
032400131010                     ( %subdt( %date( CPRdsc : *iso ) : *years ) );
032500131010         endif;
032600110627
032700110627       ENDSR;
032800110627
032900110627       //--------------------------------------------------------------
033000110627       //?Gestione tasto funzionale F09 da videata D01.                ?
033100110627       //--------------------------------------------------------------
033200110627       BEGSR  sr_F09D01;
033300110627
033400110627         Qcmd = 'DSPDBF file(*libl/WFES210F) mbr(*first) output(*)';
033500110627         exsr  sr_ExecCmd;
033600110627
033700110627         Qcmd = 'DSPDBF file(*libl/WFES220F) mbr(*first) output(*)';
033800110627         exsr  sr_ExecCmd;
033900110627
034000110627       ENDSR;
034100110627
034200110627       //--------------------------------------------------------------
034300110627       //?Controllo videata D01.                                       ?
034400110627       //--------------------------------------------------------------
034500110627       BEGSR  sr_CtrD01;
034600110627
034700110627         %subst(IndDspF : 50) = *off;
034800110627         clear  D07dti;
034900110627         clear  D07dtf;
035000110627
035100110627         // -?Controllo data spedizione iniziale?
035200110627         clear WLBdat;
035300110627         G08dat = V1Cdsi;
035400110627         XSRDA8(WLBdat);
035500110627         if  G08err = *on;
035600110627           ErrGenerico = *on;
035700110627           ErrMessage  = *on;
035800110627           PosCurDSI   = *on;
035900110627           V1Dmsg = $Msg(02);
036000110627           leavesr;
036100110627         else;
036200110627           V1Cdsi = G08dat;
036300110627           D07dti = G08inv;
036400110627         endif;
036500110627
036600110627         // -?Controllo data spedizione finale?
036700110627         clear WLBdat;
036800110627         G08dat = V1Cdsf;
036900110627         XSRDA8(WLBdat);
037000110627         if  G08err = *on;
037100110627           ErrGenerico = *on;
037200110627           ErrMessage  = *on;
037300110627           PosCurDSF   = *on;
037400110627           V1Dmsg = $Msg(02);
037500110627           leavesr;
037600110627         else;
037700110627           V1Cdsf = G08dat;
037800110627           D07dtf = G08inv;
037900110627         endif;
038000110627
038100110627         // -?Controllo range di date?
038200110627         if  D07dti > D07dtf;
038300110627           ErrGenerico = *on;
038400110627           ErrMessage  = *on;
038500110627           PosCurDSI   = *on;
038600110627           V1Dmsg = $Msg(03);
038700110627           leavesr;
038800110627         endif;
038900110627
039000110627         // -?Ultima versione cappario?
039100131010         chain  (V1Ccver)  AZCPR001;
039200131010         if  Not  %found(AZCPR01L);
039300131010           ErrGenerico = *on;
039400131010           ErrMessage  = *on;
039500131010           PosCurCVER  = *on;
039600131010           V1Dmsg = $Msg(04);
039700131010           leavesr;
039800131010         endif;
039900110627         D07cver = V1Ccver;
040000110627
040100110627         // -?data scadenza cappario?
040200110627         clear WLBdat;
040300110627         G08dat = V1Ccdsc;
040400110627         XSRDA8(WLBdat);
040500110627         if  G08err = *on;
040600110627           ErrGenerico = *on;
040700110627           ErrMessage  = *on;
040800110627           PosCurCDSC  = *on;
040900110627           V1Dmsg = $Msg(02);
041000110627           leavesr;
041100110627         else;
041200110627           V1Ccdsc = G08dat;
041300110627           D07cdsc = G08inv;
041400110627         endif;
041500131010
041600131010         if  G08inv <> CPRdsc;
041700131010           ErrGenerico = *on;
041800131010           ErrMessage  = *on;
041900131010           PosCurCDSC  = *on;
042000131010           V1Dmsg = %trimr( $Msg(05) )
042100131010                  + %char( %subdt( %date( CPRdsc : *iso ) : *days ) )
042200131010                  + '/'
042300131010                  + %char( %subdt( %date( CPRdsc : *iso ) : *months ) )
042400131010                  + '/'
042500131010                  + %char( %subdt( %date( CPRdsc : *iso ) : *days ) );
042600131010           leavesr;
042700131010         endif;
042800110627
042900110627       ENDSR;
043000110627
043100110627       //--------------------------------------------------------------
043200110627       //?Gestione tasto funzionale F6 da videata D01:                 ?
043300110627       //?F6=Conferma                                                  ?
043400110627       //--------------------------------------------------------------
043500110627       BEGSR  sr_F06D01;
043600110627
043700110627         // -?Impostazione parametri?
043800110627         kcoaz = 'SS07';
043900110627         kpjbu = TNSS07ds;
044000110627
044100110627         if  knmus = *all'1';
044200110627           // -?Richiamo interattivo?
044300110627           tnss07r ( kpjba );
044400110627         else;
044500110627           // -?Sottomissione batch?
044600110627            bch10 ( kpjba );
044700110627         endif;
044800110627
044900110627       ENDSR;
045000110627
045100110627       //--------------------------------------------------------------
045200110627       //?Esecuzione del comando (già impostato).                      ?
045300110627       //--------------------------------------------------------------
045400110627       BEGSR  sr_ExecCmd;
045500110627
045600110627         clear Qcap0100;
045700110627         Qcabcsdh = *off;
045800110627         Qcapa    = *off;
045900110627         Qcacmdss = *off;
046000110627         Qcaerved = *allX'00';
046100110627
046200110627         clear Qusec;
046300110627         Qusbprv  = %size(Qusec);
046400110627
046500110627         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
046600110627                           %size(Qcap0100) : 'CPOP0100' : *omit :
046700110627                           0 : 0 : Qusec);
046800110627
046900110627         //if  Qusei <> *blank;
047000110627         //  ...;
047100110627         //endif;
047200110627
047300110627       ENDSR;
047400110627
047500110627       //--------------------------------------------------------------
047600110627       //?Schiere a tempo di compilazione.
047700110627       //--------------------------------------------------------------
047800110627
047900131010** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
048000110627Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
048100110627Data in formato errato                                                         2
048200110627Range di date errato                                                           3
048300131010Ultima versione cappario errata                                                4
048400131010Data scadenza cappario NON coincidente: risulta                                5
