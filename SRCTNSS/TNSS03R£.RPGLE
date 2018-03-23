000100110114       //==============================================================
000200110114       //?TNSS03R * Statistica anomalie impianti smistacolli (Filtro)  ?
000300110114       //==============================================================
000400110114
000500110114       //--------------------------------------------------------------
000600110114       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110114       //--------------------------------------------------------------
000800110114
000900110114     /*PRM dbgview(*source)
001000110114     /*END
001100110114
001200110114       //--------------------------------------------------------------
001300110114       //?Specifiche di controllo.                                     ?
001400110114       //--------------------------------------------------------------
001500110114
001600110114     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110114     h dftactgrp(*no)
001800110114
001900110114       //--------------------------------------------------------------
002000110114       //?Dichiarazione file.                                          ?
002100110114       //--------------------------------------------------------------
002200110114
002300110114       // -?Tabelle?
002400110114     fTNTBE01L  if   e           k disk
002500110114
002600110114       // -?Video?
002700110114     fTNSS03D   cf   e             workstn
002800110114     f                                     indds(IndDspF)
002900110114     f                                     infds(InfDspF)
003000110114
003100110114       //--------------------------------------------------------------
003200110114       //?Definizione costanti.                                        ?
003300110114       //--------------------------------------------------------------
003400110114
003500110114       // -?Tasti funzionali a video?
003600110114     d c_F01           c                   const(x'31')
003700110114     d c_F02           c                   const(x'32')
003800110114     d c_F03           c                   const(x'33')
003900110114     d c_F04           c                   const(x'34')
004000110114     d c_F05           c                   const(x'35')
004100110114     d c_F06           c                   const(x'36')
004200110114     d c_F07           c                   const(x'37')
004300110114     d c_F08           c                   const(x'38')
004400110114     d c_F09           c                   const(x'39')
004500110114     d c_F10           c                   const(x'3A')
004600110114     d c_F11           c                   const(x'3B')
004700110114     d c_F12           c                   const(x'3C')
004800110114     d c_F13           c                   const(x'B1')
004900110114     d c_F14           c                   const(x'B2')
005000110114     d c_F15           c                   const(x'B3')
005100110114     d c_F16           c                   const(x'B4')
005200110114     d c_F17           c                   const(x'B5')
005300110114     d c_F18           c                   const(x'B6')
005400110114     d c_F19           c                   const(x'B7')
005500110114     d c_F20           c                   const(x'B8')
005600110114     d c_F21           c                   const(x'B9')
005700110114     d c_F22           c                   const(x'BA')
005800110114     d c_F23           c                   const(x'BB')
005900110114     d c_F24           c                   const(x'BC')
006000110114     d c_Enter         c                   const(x'F1')
006100110114     d c_RollDown      c                   const(x'F4')
006200110114     d c_RollUp        c                   const(x'F5')
006300110114
006400110114       //--------------------------------------------------------------
006500110114       //?Definizione schiere.                                         ?
006600110114       //--------------------------------------------------------------
006700110114
006800110114       // -?Messaggi di errore?
006900110114     d $Msg            s             78    dim( 4)  ctdata  perrcd(1)
007000110114
007100110114       //--------------------------------------------------------------
007200110114       //?Definizione aree dati.                                       ?
007300110114       //--------------------------------------------------------------
007400110114
007500110114       // -?Dati utente?
007600110114     d §AzUte        e ds                  extname(AZUTE00F)
007700110114     d                                     dtaara
007800110114     d §DatiUte      e ds                  extname(dDatiUte)
007900110114     d                                     dtaara
008000110114
008100110114       //--------------------------------------------------------------
008200110114       //?Definizione strutture dati.                                  ?
008300110114       //--------------------------------------------------------------
008400110114
008500110114       // -?Status ds?
008600110114     d Status         sds
008700110114     d  SDSpgm           *proc
008800110114
008900110114       // -?InfDS?
009000110114     d InfDspF         ds
009100110114     d   dsp_aid             369    369a
009200110114
009300110114       // -?Indicatori su DspF?
009400110114     d IndDspF         ds                  inz
009500110114         // -?Abilitazione tasti funzionali?
009600110114         // -?Emissione messaggio di errore?
009700110114     d   ErrMessage                    n   overlay(IndDspF : 28)
009800110114         // -?Indicatori per Attibuti di visualizzazione?
009900110114         // -?Posizionamento cursore & Segnalazione errore?
010000110114     d   PosCurPrc1                    n   overlay(IndDspF : 51)
010100110114     d   PosCurPrc2                    n   overlay(IndDspF : 52)
010200110118     d   PosCurMin2                    n   overlay(IndDspF : 53)
010300110114     d   PosCurRWF                     n   overlay(IndDspF : 54)
010400110114         //   -?Riemissione videata?
010500110114     d   ErrGenerico                   n   overlay(IndDspF : 99)
010600110114
010700110114       // -?Parametri ricevuti?
010800110114     d KPJBA         e ds
010900110114
011000110114       // -?Parametri per pgm. batch - TNSS09R?
011100110114     d TNSS04ds      e ds                  inz
011200110114
011300110118       // -?Tabella "MVI"/"PARMS"?
011400110118     d dMVIparms     e ds                  inz
011500110120       // -?Tabella "MVI"/"FLTR7"?
011600110120     d dMVIfltr7     e ds                  inz
011700110114
011800110114       //--------------------------------------------------------------
011900110114       //?Definizione variabili globali.                               ?
012000110114       //--------------------------------------------------------------
012100110114
012200110114       // -?Flags booleani?
012300110114     d $Fine           s               n   inz
012400110114     d $InzD01         s               n   inz(*on)
012500110114
012600110114       // -?Variabili per la gestione del video?
012700110114     d $Video          s              2    inz('D1')
012800110120
012900110120       // -?Campi di comodo?
013000110120     d wDate_Eur       s               d   inz  datfmt(*eur)
013100110114
013200110114       //--------------------------------------------------------------
013300110114       //?Definizione prototipi procedure.                             ?
013400110114       //--------------------------------------------------------------
013500110114
013600110114       // -?Reperimento dati utente?
013700110114     d TIBS34ds      e ds                  inz
013800110114      /copy gaitrasrc/srcProtoPR,TIBS34R
013900110114
014000110114       // -?Sottomissione lavoro batch?
014100110114      /copy gaitrasrc/srcProtoPR,BCH10
014200110114
014300110114       // -?Richiamo diretto lavoro batch?
014400110114     d tnss04r         pr                  extpgm('TNSS04R')
014500110114     d   kpjba                             likeds(KPJBA)
014600110114
014700110114       // -?Parametri API QCAPCMD (Process Commands)?
014800110114     d Qcmd            s           2048    inz  varying
014900110114      /copy qSysInc/qRpgleSrc,QCAPCMD
015000110114       // -?API QCAPCMD (Process Commands)?
015100110114      /copy gaitrasrc/srcProtoPR,QCAPCMD
015200110114
015300110114       // -?Parametri gestione errori API.?
015400110114      /copy qsysinc/qrpglesrc,QUSEC
015500110114
015600110114       //--------------------------------------------------------------
015700110114       //?Definizione key-list.                                        ?
015800110114       //--------------------------------------------------------------
015900110114
016000110114       // -?File TNTBE01L?
016100110114     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
016200110114     d                                     prefix(k_)   inz
016300110114
016400110114       //--------------------------------------------------------------
016500110114       //?M A I N - L I N E                                            ?
016600110114       //--------------------------------------------------------------
016700110114
016800110114     c     *Entry        plist
016900110114     c                   parm                    KPJBA
017000110114
017100110114      /free
017200110114
017300110114       // -?Operazioni iniziali?
017400110114       exsr sr_RoutInz;
017500110114
017600110114       // -?Gestione videate?
017700110114       DoW  $Fine = *off;
017800110114
017900110114         select;
018000110114           // -?Gestione filtro di lancio?
018100110114           when $Video = 'D1';
018200110114             exsr sr_GesD01;
018300110114           // -?? ? ? ? ??
018400110114           other;
018500110114             $Fine = *on;
018600110114         endsl;
018700110114
018800110114       enddo;
018900110114
019000110114       // -?Operazioni finali?
019100110114       exsr sr_RoutEnd;
019200110114
019300110114       //--------------------------------------------------------------
019400110114       //?Operazioni iniziali.                                         ?
019500110114       //--------------------------------------------------------------
019600110114       BEGSR  sr_RoutInz;
019700110114
019800110114         *inLR = *on;
019900110114
020000110114         // -?Reperimento dati job?
020100110114         exsr  sr_DatiJob;
020200110114
020300110114         // -?Impostazione nome programma a video?
020400110114         V1Cpgm = SDSpgm;
020500110114
020600110118         // -?Reperimento dati tab. "MVI"/"PARMS"?
020700110118         clear  k05tntbe01;
020800110118         k_TBEcod = 'MVI';
020900110118         k_TBEke1 = 'PARMS';
021000110114         chain  %kds(k05tntbe01)  TNTBE000;
021100110114         if  %found(TNTBE01L)  and  TBEatb = *blank;
021200110118           dMVIparms = TBEuni;
021300110114         endif;
021400110120
021500110120         // -?Reperimento dati tab. "MVI"/"FLTR7"?
021600110120         clear  k05tntbe01;
021700110120         k_TBEcod = 'MVI';
021800110120         k_TBEke1 = 'FLTR7';
021900110120         chain  %kds(k05tntbe01)  TNTBE000;
022000110120         if  %found(TNTBE01L)  and  TBEatb = *blank;
022100110120           dMVIfltr7 = TBEuni;
022200110120         endif;
022300110114
022400110114       ENDSR;
022500110114
022600110114       //--------------------------------------------------------------
022700110114       //?Reperimento Dati del job (Utente/Operativi).                 ?
022800110114       //--------------------------------------------------------------
022900110114       BEGSR  sr_DatiJob;
023000110114
023100110114         in(e) §AzUte;
023200110114         if NOT %error;
023300110114           in(e) §DatiUte;
023400110114         endif;
023500110114         if %error or RSut = *blank;
023600110114           tibs34r ( tibs34ds );
023700110114           in §AzUte;
023800110114           in §DatiUte;
023900110114         endif;
024000110114
024100110114       ENDSR;
024200110114
024300110114       //--------------------------------------------------------------
024400110114       //?Operazioni finali.                                           ?
024500110114       //--------------------------------------------------------------
024600110114       BEGSR  sr_RoutEnd;
024700110114
024800110114         return;
024900110114
025000110114       ENDSR;
025100110114
025200110114       //--------------------------------------------------------------
025300110114       //?Gestione videata D01.                                        ?
025400110114       //--------------------------------------------------------------
025500110114       BEGSR  sr_GesD01;
025600110114
025700110114         // -?Inizializzazione videata?
025800110114         if  $InzD01 = *on;
025900110114           exsr  sr_InzD01;
026000110114           $InzD01 = *off;
026100110114         endif;
026200110114
026300110114         // -?Emissione videata completa?
026400110114         exfmt  SS03D01;
026500110114
026600110114         clear  V1Dmsg;
026700110114         reset  ErrMessage;
026800110114         reset  ErrGenerico;
026900110114
027000110114         SELECT;
027100110114
027200110114           // -?Sistema Informativo errato => FINE?
027300110114           WHEN  %subst(KNSIF:1:3) = 'FIL'  or
027400110114                 %subst(KNSIF:7:2) = 'PF';
027500110114             $Fine = *on;
027600110114
027700110114           // -?F3=Fine?
027800110114           WHEN  dsp_aid = c_F03;
027900110114             $Fine = *on;
028000110114
028100110114           // -?F9=Visualizzazione WrkF WFTR7_1D/1T/2D/2T?
028200110114           WHEN  dsp_aid = c_F09;
028300110114             exsr  sr_F09D01;
028400110114
028500110114           // -?Invio / F6=Conferma?
028600110114           OTHER;
028700110114             exsr sr_CtrD01;
028800110114             // -?F6 = Lancio batch (se NO errori)?
028900110114             if  ErrGenerico = *off  and  dsp_aid = c_F06;
029000110114               exsr  sr_F06D01;
029100110114               $Fine = *on;
029200110114             endif;
029300110114
029400110114         ENDSL;
029500110114
029600110114       ENDSR;
029700110114
029800110114       //--------------------------------------------------------------
029900110114       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
030000110114       //--------------------------------------------------------------
030100110114       BEGSR  sr_InzD01;
030200110114
030300110114         clear  V1Cprc1;
030400110114         clear  V1Cprc2;
030500110118         clear  V1Cmin2;
030600110119         V1Crwf = 'S';
030700110120         if  §MVITR7ela > *zero;
030800110120           wDate_Eur = %date( §MVITR7ela : *iso );
030900110120           V1Cela = %dec( wDate_Eur );
031000110120         endif;
031100110120         if  §MVITR7pri > *zero;
031200110120           wDate_Eur = %date( §MVITR7pri : *iso );
031300110120           V1Cpri = %dec( wDate_Eur );
031400110120         endif;
031500110120         if  §MVITR7ult > *zero;
031600110120           wDate_Eur = %date( §MVITR7ult : *iso );
031700110120           V1Cult = %dec( wDate_Eur );
031800110120         endif;
031900110114
032000110114         // -?Ctrl Sistema Informativo per verificare se Sede o Filiale?
032100110114         if  %subst(KNSIF:1:3) = 'FIL'  or
032200110114             %subst(KNSIF:7:2) = 'PF';
032300110114           ErrGenerico = *on;
032400110114           ErrMessage  = *on;
032500110114           V1Dmsg = $Msg(1);
032600110114         endif;
032700110114
032800110118         // -?Impostazione parametri di default (da tab. "MVI"/"PARMS")?
032900110118         V1Cprc1 = §MVIprc1;
033000110118         V1Cprc2 = §MVIprc2;
033100110118         V1Cmin2 = §MVImin2;
033200110114
033300110114       ENDSR;
033400110114
033500110114       //--------------------------------------------------------------
033600110114       //?Gestione tasto funzionale F09 da videata D01.                ?
033700110114       //--------------------------------------------------------------
033800110114       BEGSR  sr_F09D01;
033900110114
034000110114         Qcmd = 'DSPDBF file(*libl/WFTR7_1D) mbr(*first) output(*)';
034100110114         exsr  sr_ExecCmd;
034200110114
034300110114         Qcmd = 'DSPDBF file(*libl/WFTR7_1T) mbr(*first) output(*)';
034400110114         exsr  sr_ExecCmd;
034500110114
034600110114         Qcmd = 'DSPDBF file(*libl/WFTR7_2D) mbr(*first) output(*)';
034700110114         exsr  sr_ExecCmd;
034800110114
034900110114         Qcmd = 'DSPDBF file(*libl/WFTR7_2T) mbr(*first) output(*)';
035000110114         exsr  sr_ExecCmd;
035100110114
035200110114       ENDSR;
035300110114
035400110114       //--------------------------------------------------------------
035500110114       //?Controllo videata D01.                                       ?
035600110114       //--------------------------------------------------------------
035700110114       BEGSR  sr_CtrD01;
035800110114
035900110114         %subst(IndDspF : 50) = *off;
036000110114
036100110114         select;
036200110114
036300110114           // -?Percentuale limite in analisi disallineati x filiale?
036400110114           when  V1Cprc1 < *zero  or  V1Cprc1 > 100;
036500110114             ErrGenerico = *on;
036600110114             ErrMessage  = *on;
036700110114             PosCurPrc1  = *on;
036800110114             V1Dmsg = $Msg(02);
036900110114             leavesr;
037000110114
037100110114           // -?Percentuale limite in analisi con caratteri errati x baia?
037200110114           when  V1Cprc2 < *zero  or  V1Cprc2 > 100;
037300110114             ErrGenerico = *on;
037400110114             ErrMessage  = *on;
037500110114             PosCurPrc2  = *on;
037600110114             V1Dmsg = $Msg(02);
037700110114             leavesr;
037800110114
037900110118           // -?MINIMO n° colli elaborati in analisi con caratteri errati x baia?
038000110118           when  V1Cmin2 < *zero  or  V1Cmin2 > 999;
038100110114             ErrGenerico = *on;
038200110114             ErrMessage  = *on;
038300110118             PosCurMin2  = *on;
038400110114             V1Dmsg = $Msg(03);
038500110114             leavesr;
038600110114
038700110114           // -?Flag "Rigenerazione work-file" errato?
038800110114           when  V1Crwf <> *blank  and  V1Crwf <> 'S';
038900110114             ErrGenerico = *on;
039000110114             ErrMessage  = *on;
039100110114             PosCurRWF   = *on;
039200110114             V1Dmsg = $Msg(04);
039300110114             leavesr;
039400110114
039500110114         endsl;
039600110114
039700110114       ENDSR;
039800110114
039900110114       //--------------------------------------------------------------
040000110114       //?Gestione tasto funzionale F6 da videata D01:                 ?
040100110114       //?F6=Conferma                                                  ?
040200110114       //--------------------------------------------------------------
040300110114       BEGSR  sr_F06D01;
040400110114
040500110114         // -?Impostazione parametri?
040600110114         kcoaz = 'SS04';
040700110114         clear  TNSS04ds;
040800110114         D04prc1 = V1Cprc1;
040900110114         D04prc2 = V1Cprc2;
041000110118         D04min2 = V1Cmin2;
041100110114         D04rwf  = V1Crwf;
041200110114         kpjbu = TNSS04ds;
041300110114
041400110114         if  knmus = *all'1';
041500110114           // -?Richiamo interattivo?
041600110114           tnss04r ( kpjba );
041700110114         else;
041800110114           // -?Sottomissione batch?
041900110114            bch10 ( kpjba );
042000110114         endif;
042100110114
042200110114       ENDSR;
042300110114
042400110114       //--------------------------------------------------------------
042500110114       //?Esecuzione del comando (già impostato).                      ?
042600110114       //--------------------------------------------------------------
042700110114       BEGSR  sr_ExecCmd;
042800110114
042900110114         clear Qcap0100;
043000110114         Qcabcsdh = *off;
043100110114         Qcapa    = *off;
043200110114         Qcacmdss = *off;
043300110114         Qcaerved = *allX'00';
043400110114
043500110114         clear Qusec;
043600110114         Qusbprv  = %size(Qusec);
043700110114
043800110114         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
043900110114                           %size(Qcap0100) : 'CPOP0100' : *omit :
044000110114                           0 : 0 : Qusec);
044100110114
044200110114         //if  Qusei <> *blank;
044300110114         //  ...;
044400110114         //endif;
044500110114
044600110114       ENDSR;
044700110114
044800110114       //--------------------------------------------------------------
044900110114       //?Schiere a tempo di compilazione.
045000110114       //--------------------------------------------------------------
045100110114
045200110114** - $Msg:?Messaggi di Errore?-----------------------------------------------*
045300110114Funzione eseguibile solo in ambiente di SEDE                                   1
045400110114Percentuale errata.   Valori ammessi: da 0,0000 a 100,0000                     2
045500110114Numero errato.   Valori ammessi: da 0 a 100                                    3
045600110114Flag "Rigenerazione Work-File" errato.                                         4
