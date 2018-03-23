000100180221       //==============================================================
000200180221       //? Report spedizioni con Merce Incompatibile.                   ?
000300180221       //==============================================================
000400180221
000500180221       //--------------------------------------------------------------
000600180221       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180221       //--------------------------------------------------------------
000800180221
000900180221     /*PRM dbgview(*source)
001000180221     /*END
001100180221
001200180221       //--------------------------------------------------------------
001300180221       //? Specifiche di controllo.                                     ?
001400180221       //--------------------------------------------------------------
001500180221
001600180221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700180221     h dftactgrp(*no)
001800180221     h bnddir('TRUL')
001900180221
002000180221       //--------------------------------------------------------------
002100180221       //? Dichiarazione file.                                          ?
002200180221       //--------------------------------------------------------------
002300180221
002400180221       // - ?BOLLE SEDE: da Fatturare + fatturate "ultimi mesi"?
002500180221       //  ?                          + fatturate "storico"?
002600180221     fTITAS38C  if   e           k disk
002700180221
002800180221       // - ?BOLLE SEDE: Estensione dettaglio Segnacolli?
002900180221     fTITAH30C  if   e           k disk
003000180221
003100180221      // - ?Anagrafica Commerciali?
003200180221     fAZCMM01L  if   e           k disk
003300180221
003400180221       // - ?Organigramma?
003500180221     fAZORG01L  if   e           k disk
003600180221
003700180221       // - ?Work-File di Output?
003800180221     fWFRMI01L  Uf A e           k disk    usropn
003900180221
004000180221       //--------------------------------------------------------------
004100180221       //? Definizione costanti.                                        ?
004200180221       //--------------------------------------------------------------
004300180221
004400180221
004500180221       //--------------------------------------------------------------
004600180221       //? Definizione schiere.                                         ?
004700180221       //--------------------------------------------------------------
004800180221
004900180221
005000180221       //--------------------------------------------------------------
005100180221       //? Definizione aree dati.                                       ?
005200180221       //--------------------------------------------------------------
005300180221
005400180221       // - ?Dati utente?
005500180221     d §AzUte        e ds                  extname(AZUTE00F)
005600180221     d                                     dtaara
005700180221     d §DatiUte      e ds                  extname(dDatiUte)
005800180221     d                                     dtaara
005900180221
006000180221       //--------------------------------------------------------------
006100180221       //? Definizione strutture dati.                                  ?
006200180221       //--------------------------------------------------------------
006300180221
006400180221       // - ?Status ds?
006500180221     d Status         sds
006600180221     d   SDSpgmName      *proc
006700180221
006800180221       // - ?Parametri ricevuti?
006900180221     d KPJBA         e ds
007000180221     d TNSS19ds      e ds                  inz   qualified
007100180221
007200180221       // - ?DS lettura campo TASNAS di TITAS?
007300180221     d dTASnas       e ds                  inz   qualified
007400180221
007500180221       //--------------------------------------------------------------
007600180221       //? Definizione variabili globali.                               ?
007700180221       //--------------------------------------------------------------
007800180221
007900180221       // - ?Campi di comodo?
008000180221     d*//· wDate_Iso       s               d   inz   datfmt(*iso)
008100180221     d wDelta          s             20  3 inz
008200180221     d wNumColliInc    s                   inz   like(TASncl)
008300180221
008400180221       //--------------------------------------------------------------
008500180221       //? Definizione prototipi procedure.                             ?
008600180221       //--------------------------------------------------------------
008700180221
008800180221       // - ?Reperimento dati utente?
008900180221     d TIBS34ds      e ds                  inz
009000180221      /copy gaitrasrc/srcProtoPR,TIBS34R
009100180221
009200180221       // - ?Controllo/Decodifica cliente?
009300180221      /copy gaitrasrc/srcProtoPI,TIBS69R
009400180221      /copy gaitrasrc/srcProtoPR,TIBS69R
009500180221
009600180221       // -? Ricerca unificante padre?
009700180221      /copy gaitrasrc/srcProtoPI,TIBS10R
009800180221      /copy gaitrasrc/srcProtoPR,TIBS10R
009900180221
010000180221       // -? Calcolo Delta del Cliente?
010100180221     d TISE70ds      e ds                  inz   qualified
010200180221     d TISE70R         pr                  extpgm('TISE70R')
010300180221     d   TISE70ds                          likeds(TISE70ds)
010400180221
010500180221       // - ?Parametri API QCAPCMD (Process Commands)?
010600180221     d Qcmd            s           2048    inz   varying
010700180221      /copy qSysInc/qRpgleSrc,QCAPCMD
010800180221       // - ?API QCAPCMD (Process Commands)?
010900180221      /copy gaitrasrc/srcProtoPR,QCAPCMD
011000180221
011100180221       // - ?Parametri gestione errori API.?
011200180221      /copy qsysinc/qrpglesrc,QUSEC
011300180221
011400180221       //--------------------------------------------------------------
011500180221       //? Definizione key-list.                                        ?
011600180221       //--------------------------------------------------------------
011700180221
011800180221       // - ?File TITAS38C?
011900180221     d keyTITAS38    e ds                  extname( TITAS38C : *key )
012000180221     d                                     prefix( k_ )  inz
012100180221
012200180221       // - ?File TITAH30C?
012300180221     d keyTITAH30    e ds                  extname( TITAH30C : *key )
012400180221     d                                     prefix( k_ )  inz
012500180221
012600180221       // - ?File AZCMM01L?
012700180221     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
012800180221     d                                     prefix( k_ )  inz
012900180221
013000180221       // - ?File AZORG01L?
013100180221     d keyAZORG01    e ds                  extname( AZORG01L : *key )
013200180221     d                                     prefix( k_ )  inz
013300180221
013400180221       // - ?File WFRMI01L?
013500180221     d keyWFRMI01    e ds                  extname( WFRMI01L : *key )
013600180221     d                                     prefix( k_ )  inz
013700180221
013800180221       //--------------------------------------------------------------
013900180221       //? Riepilogo indicatori utilizzati.                             ?
014000180221       //--------------------------------------------------------------
014100180221
014200180221
014300180221       //--------------------------------------------------------------
014400180221       //? M A I N - L I N E                                            ?
014500180221       //--------------------------------------------------------------
014600180221
014700180221     c     *Entry        plist
014800180221     c                   parm                    KPJBA
014900180221
015000180221      /free
015100180221
015200180221       // - ?Operazioni iniziali?
015300180221       exsr  sr_RoutInz;
015400180221
015500180221       // - ?Ciclo di elaborazione?
015600180221       clear  keyTITAS38;
015700180221       k_TASaas = TNSS19ds.SS19dsi / 10000;
015800180221       k_TASmgs = %int( %subst( %editc( TNSS19ds.SS19dsi : 'X' )
015900180221                                : 5 : 4 ) );
016000180221       setll  %kds( keyTITAS38 )  TITAS38C;
016100180221       read  TITAS38C;
016200180221
016300180221       DoW  NOT %eof(TITAS38C)  and
016400180221            ( TASaas * 10000 ) + TASmgs <= TNSS19ds.SS19dsf;
016500180221         exsr  sr_Elab_TITAS;
016600180221         read  TITAS38C;
016700180221       EndDo;
016800180221
016900180221       // - ?Eventuale scrittura record per l'ultimo cliente elaborato?
017000180222       //*·if  wmiISP <> *zero  or  wmiPSP <> *zero;
017100180222       if  wmiKSC <> *zero  and  wmiTSP > *zero;
017200180221         exsr  sr_Write_WFRMI;
017300180221       endif;
017400180221
017500180221       // - ?Operazioni finali?
017600180221       exsr sr_RoutEnd;
017700180221
017800180221       //--------------------------------------------------------------
017900180221       //? Operazioni iniziali.                                         ?
018000180221       //--------------------------------------------------------------
018100180221       BEGSR  sr_RoutInz;
018200180221
018300180221         // - ?Impostazione chiusura?
018400180221         *inLR = *on;
018500180221
018600180221         // - ?Reperimento parametri?
018700180221         TNSS19ds = kpjbu;
018800180221
018900180221         // - ?Reperimento dati job?
019000180221         exsr  sr_DatiJob;
019100180221
019200180221         // - ?Richiesta pulizia del work-file WFRMI00F?
019300180221         if  TNSS19ds.SS19cwf = 'S';
019400180221           Qcmd = 'CLRPFM file(*libl/WFRMI00F)';
019500180221           exsr  sr_ExecCmd;
019600180221           if  Qusei <> *blank;
019700180221             exsr  sr_RoutEnd;
019800180221           endif;
019900180221         endif;
020000180221
020100180221         // - ?Apertura del work-file WFRMI00F?
020200180221         open  WFRMI01L;
020300180221
020400180221       ENDSR;
020500180221
020600180221       //--------------------------------------------------------------
020700180221       //? Reperimento Dati del job (Utente/Operativi).                 ?
020800180221       //--------------------------------------------------------------
020900180221       BEGSR  sr_DatiJob;
021000180221
021100180221         in(e) §AzUte;
021200180221         if NOT %error;
021300180221           in(e) §DatiUte;
021400180221         endif;
021500180221         if %error or RSut = *blank;
021600180221           tibs34r ( tibs34ds );
021700180221           in §AzUte;
021800180221           in §DatiUte;
021900180221         endif;
022000180221
022100180221       ENDSR;
022200180221
022300180221       //--------------------------------------------------------------
022400180221       //? Elaborazione spedizioni dal *file TITAS38C.                  ?
022500180221       //--------------------------------------------------------------
022600180221       BEGSR  sr_Elab_TITAS;
022700180221
022800180221         // - ?Rottura per Cliente bollettazione?
022900180221         If  TASksc <> WMIksc;
023000180221
023100180221           //*·// - ?Eventuale scrittura record per il cliente precedente?
023200180221           //*·if  wmiISP <> *zero  or  wmiPSP <> *zero;
023300180221           //*·  exsr  sr_Write_WFRMI;
023400180221           //*·endif;
023500180221           // - ?Scrittura COMUNQUE del record per il cliente precedente?
023600180221           //   ?(si potrebbero trovare spedizioni con merce?
023700180221           //   ?incompatibile in un giorno successivo)?
023800180221           if  wmiKSC <> *zero  and  wmiTSP > *zero;
023900180221             if  NOT  %found( WFRMI01L );
024000180221               exsr  sr_Write_WFRMI;
024100180221             else;
024200180221               update  WFRMI000;
024300180221             endif;
024400180221           endif;
024500180221
024600180221           // - ?Verifica esistenza di un rec. per lo stesso cliente?
024700180221           //   ?di bollettazione (per un altro giorno di spedizione)?
024800180221           clear  keyWFRMI01;
024900180221           k_WMIksc = tasKSC;
025000180221           k_WMIdti = TNSS19ds.SS19dsi;
025100180221           k_WMIdtf = TNSS19ds.SS19dsf;
025200180221           chain  %kds( keyWFRMI01 )  WFRMI000;
025300180221
025400180221           // - ?Pulizia dati per il cliente in elaborazione?
025500180221           //   ?SE nuovo cliente (rec. NON trovato)?
025600180221           if  NOT  %found( WFRMI01L );
025700180221             clear  WFRMI000;
025800180221             wmiKSC = tasKSC;
025900180221           endif;
026000180221
026100180221         EndIf;
026200180221
026300180221
026400180221         // - ?Filtro delle spedizioni con un tipo bolla elaborabile?
026500180221         //   ?("F1" o "A2")?
026600180221         if  TAStbl <> 'F1'  and  TAStbl <> 'A2';
026700180221           leavesr;
026800180221         endif;
026900180221
027000180221
027100180221         // - ?Calcolo totali per il cliente:?
027200180221         //   ?Totale Spedizioni?
027300180221         wmiTSP += 1;
027400180221         wmiTCL += tasNCL;
027500180221         wmiTPS += tasPKF;
027600180221         wmiTVL += tasVLF;
027700180221         wmiTIM += tasIMV;
027800180221         if  tasIMV <> *zero;
027900180221           wmiTSI += 1;
028000180221         endif;
028100180221
028200180221         // - ?Eventuale calcolo totali per le spedizioni?
028300180221         //   ?TOTALMENTE o PARZIALMENTE incompatibili?
028400180221         dTASnas = tasNAS;
028500180221
028600180221         If  dTASnas.§floINC = 'S';
028700180221
028800180221           exsr  sr_Calcolo_Incompatibili;
028900180221
029000180221           If  wNumColliInc >= tasNCL;
029100180221
029200180221             // - ?Calcolo totali per le spedizioni TOTALMENTE incompatibili?
029300180221             wmiISP += 1;
029400180221             wmiICL += tasNCL;
029500180221             wmiIPS += tasPKF;
029600180221             wmiIVL += tasVLF;
029700180221             wmiIIM += tasIMV;
029800180221             if  tasIMV <> *zero;
029900180221               wmiISI += 1;
030000180221             endif;
030100180221
030200180221           Else;
030300180221
030400180221             // - ?Calcolo totali per le spedizioni PARZIALMENTE incompatibili?
030500180221             wmiPSP += 1;
030600180221             wmiPCL += tasNCL;
030700180221             wmiPCI += wNumColliInc;
030800180221             wmiPPS += tasPKF;
030900180221             wmiPVL += tasVLF;
031000180221             wmiPIM += tasIMV;
031100180221             if  tasIMV <> *zero;
031200180221               wmiPSI += 1;
031300180221             endif;
031400180221
031500180221           EndIf;
031600180221
031700180221         EndIf;
031800180221
031900180221       ENDSR;
032000180221
032100180221       //--------------------------------------------------------------
032200180221       //? Calcoli per le spedizioni con colli incpatibili.             ?
032300180221       //--------------------------------------------------------------
032400180221       BEGSR  sr_Calcolo_Incompatibili;
032500180221
032600180221         clear  wNumColliInc;
032700180221
032800180221         // - ?Ricerca colli incompatibili sul *file TITAH30C *trk "J"?
032900180221         clear  keyTITAH30;
033000180221         k_tahAAS = tasAAS;
033100180221         k_tahLNP = tasLNP;
033200180221         k_tahNRS = tasNRS;
033300180221         k_tahNSP = tasNSP;
033400180221         k_tahTRC = 'J';
033500180221         setll  %kds( keyTITAH30 )  TITAH30C;
033600180221
033700180221         if  NOT %equal( TITAH30C );
033800180221           leavesr;
033900180221         endif;
034000180221
034100180221         // - ?Conteggio dei colli incompatibili?
034200180221         readE  %kds( keyTITAH30 )  TITAH30C;
034300180221
034400180221         DoW  NOT %eof( TITAH30C );
034500180221
034600180221           wNumColliInc += 1;
034700180221
034800180221           readE  %kds( keyTITAH30 )  TITAH30C;
034900180221
035000180221         EndDo;
035100180221
035200180221       ENDSR;
035300180221
035400180221       //--------------------------------------------------------------
035500180221       //? Registrazione dati del cliente appena elaborato in WFRMI00F. ?
035600180221       //--------------------------------------------------------------
035700180221       BEGSR  sr_Write_WFRMI;
035800180221
035900180221         // - ?Registrazione record in WFRMI00F?
036000180221         wmiDTI  = TNSS19ds.SS19dsi;
036100180221         wmiDTF  = TNSS19ds.SS19dsf;
036200180221
036300180221         // - ?Decodifica Cliente di Bollettazione?
036400180221         clear  TIBS69ds;
036500180221         I69sif = knsif;
036600180221         I69kcc = DUTkci;
036700180221         I69kac = wmiKSC;
036800180221         TIBS69R ( TIBS69ds :
036900180221                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
037000180221         If  O69err = *blank;
037100180221           wmiRSC  = ACOrag;
037200180221         EndIf;
037300180221
037400180221         // - ?Reperimento Cliente Unificante?
037500180221         clear  TIBS10ds;
037600180221         //*·TIBS10ds.D10drf = %dec( wDate_Iso );
037700180221         TIBS10ds.D10drf = TNSS19ds.SS19dsf;
037800180221         TIBS10ds.D10tle = 'ST';
037900180221         TIBS10ds.D10paf = 'P';   //? Verifica se è figlio e ne reperisce il padre?
038000180221         TIBS10ds.D10cod = wmiKSC;
038100180221         TIBS10R ( TIBS10ds );
038200180221         if  TIBS10ds.D10err = *blank  and  TIBS10ds.D10cop > *zero;
038300180221           wmiKSU = TIBS10ds.D10cop;
038400180221         else;
038500180221           wmiKSU = TIBS10ds.D10cod;
038600180221         endif;
038700180221
038800180221         // - ?Decodifica e Reperimento Commerciale del Cliente Unificante?
038900180221         clear  TIBS69ds;
039000180221         I69sif = knsif;
039100180221         I69kcc = DUTkci;
039200180221         I69kac = wmiKSU;
039300180221         I69kcp = I69kac;
039400180221         TIBS69R ( TIBS69ds :
039500180221                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
039600180221         if  O69err <> *on;
039700180221           wmiRSU = ACOrag;
039800180221           wmiCLV = CLPclv;
039900180221           wmiCMU = CLPage;
040000180221         endif;
040100180221
040200180221         // - ?Reperimento Commerciale Unificante del Cliente Unificante?
040300180221         k_CMMcod = wmiCMU;
040400180221         chain  %kds( keyAZCMM01 )  AZCMM000;
040500180221         if  %found( AZCMM01L );
040600180221           wmiCMU = CMMuni;
040700180221         endif;
040800180221
040900180221         // - ?(Decodifica e) Reperimento Filiale del Comm.le Unificante?
041000180221         k_CMMcod = wmiCMU;
041100180221         chain  %kds( keyAZCMM01 )  AZCMM000;
041200180221         if  %found(AZCMM01L);
041300180221           wmiCMU = CMMuni;
041400180221           k_ORGfil = CMMfil;
041500180221         endif;
041600180221
041700180221         // - ?Reperimento Distretto & Area del Commerciale Unificante?
041800180221         if  k_ORGfil <= *zero;
041900180221           k_ORGfil = wmiCMU / 10000;
042000180221         endif;
042100180221         chain  %kds( keyAZORG01 )  AZORG;
042200180221         if  %found(AZORG01L)  and  ORGfva = *blank;
042300180221           wmiDCU = ORGfl3;
042400180221           wmiACU = ORGcar;
042500180221         endif;
042600180221
042700180221         // - ?Reperimento dei dati del Cliente Bollettazione nel mese?
042800180221         //   ?della data "AL" per calcolo DELTA?
042900180221         clear  TISE70ds;
043000180221         TISE70ds.I70tla = 'S';
043100180221         TISE70ds.I70tle = 'ST';
043200180221         TISE70ds.I70ksc = wmiKSC;
043300180221         TISE70ds.I70dai = TNSS19ds.SS19dsf / 100;
043400180221         TISE70ds.I70daf = TISE70ds.I70dai;
043500180221
043600180221         TISE70R ( TISE70ds );
043700180221
043800180221         If  TISE70ds.O70rir <> *zero;
043900180221           wDelta = ( ( TISE70ds.O70rir - TISE70ds.O70rip ) * 100 )
044000180221                      / TISE70ds.O70rir;
044100180221         Else;
044200180221           wDelta = ( TISE70ds.O70rir - TISE70ds.O70rip ) * 100;
044300180221         EndIf;
044400180221         select;
044500180221           when  wDelta = *zero;
044600180221           when  wDelta >= 1000;
044700180221             wmiPDE = 999,9;
044800180222           when  wDelta <= -1000;
044900180221             wmiPDE = -999,9;
045000180221           other;
045100180221             eval(H) wmiPDE = wDelta;
045200180221         endsl;
045300180221
045400180221         // - ?Scrittura rec. nel *file WFRMI00F?
045500180221         write  WFRMI000;
045600180221
045700180221       ENDSR;
045800180221
045900180221       //--------------------------------------------------------------
046000180221       //? Esecuzione del comando (già impostato).                      ?
046100180221       //--------------------------------------------------------------
046200180221       BEGSR  sr_ExecCmd;
046300180221
046400180221         clear Qcap0100;
046500180221         Qcabcsdh = *off;
046600180221         Qcapa    = *off;
046700180221         Qcacmdss = *off;
046800180221         Qcaerved = *allX'00';
046900180221
047000180221         clear Qusec;
047100180221         Qusbprv  = %size(Qusec);
047200180221
047300180221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
047400180221                           %size(Qcap0100) : 'CPOP0100' : *omit :
047500180221                           0 : 0 : Qusec);
047600180221
047700180221         //if  Qusei <> *blank;
047800180221         //  ...;
047900180221         //endif;
048000180221
048100180221       ENDSR;
048200180221
048300180221       //--------------------------------------------------------------
048400180221       //? Operazioni finali.                                           ?
048500180221       //--------------------------------------------------------------
048600180221       BEGSR  sr_RoutEnd;
048700180221
048800180221         reset  TIBS69ds;
048900180221         tibs69r( TIBS69ds :
049000180221                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
049100180221
049200180221         // - ?Uscita dal *pgm?
049300180221         return;
049400180221
049500180221       ENDSR;
049600180221
049700180221      /end-free
