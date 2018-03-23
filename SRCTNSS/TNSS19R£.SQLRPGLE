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
001900180222     h alwnull(*inputonly)
002000180221
002100180221       //--------------------------------------------------------------
002200180221       //? Dichiarazione file.                                          ?
002300180221       //--------------------------------------------------------------
002400180221
002500180221
002600180221       // - ?BOLLE SEDE: Estensione dettaglio Segnacolli?
002700180221     fTITAH30C  if   e           k disk
002800180221
002900180221      // - ?Anagrafica Commerciali?
003000180221     fAZCMM01L  if   e           k disk
003100180221
003200180221       // - ?Organigramma?
003300180221     fAZORG01L  if   e           k disk
003400180221
003500180221       // - ?Work-File di Output?
003600180222     fWFRMI00F  o    e             disk    usropn
003700180221
003800180221       //--------------------------------------------------------------
003900180221       //? Definizione costanti.                                        ?
004000180221       //--------------------------------------------------------------
004100180221
004200180221
004300180221       //--------------------------------------------------------------
004400180221       //? Definizione schiere.                                         ?
004500180221       //--------------------------------------------------------------
004600180221
004700180221
004800180221       //--------------------------------------------------------------
004900180221       //? Definizione aree dati.                                       ?
005000180221       //--------------------------------------------------------------
005100180221
005200180221       // - ?Dati utente?
005300180221     d §AzUte        e ds                  extname(AZUTE00F)
005400180221     d                                     dtaara
005500180221     d §DatiUte      e ds                  extname(dDatiUte)
005600180221     d                                     dtaara
005700180221
005800180221       //--------------------------------------------------------------
005900180221       //? Definizione strutture dati.                                  ?
006000180221       //--------------------------------------------------------------
006100180221
006200180221       // - ?Status ds?
006300180221     d Status         sds
006400180221     d   SDSpgmName      *proc
006500180221
006600180221       // - ?Parametri ricevuti?
006700180221     d KPJBA         e ds
006800180222     d TNSS19ds      e ds                  inz   qualified
006900180226
007000180226       // - ?Tabella 05 = Area?
007100180226     d ds05          e ds                  inz   qualified
007200180226       // - ?Tabella 17 = Distretto?
007300180226     d ds17          e ds                  inz   qualified
007400180221
007500180221       // - ?DS lettura campo TASNAS di TITAS?
007600180221     d dTASnas       e ds                  inz   qualified
007700180222
007800180222       // -?Dati estratti via SQL?
007900180222     d TITAS00F      e ds                  based(nullptr)
008000180222     d                                           qualified
008100180222     d TITAS_ds        ds            81    inz   qualified
008200180222     d   tasAAS                            inz   like(TITAS00F.tasAAS)
008300180222     d   tasMGS                            inz   like(TITAS00F.tasMGS)
008400180222     d   tasLNP                            inz   like(TITAS00F.tasLNP)
008500180222     d   tasNRS                            inz   like(TITAS00F.tasNRS)
008600180222     d   tasNSP                            inz   like(TITAS00F.tasNSP)
008700180222     d   tasKSC                            inz   like(TITAS00F.tasKSC)
008800180222     d   tasNCL                            inz   like(TITAS00F.tasNCL)
008900180222     d   tasPKF                            inz   like(TITAS00F.tasPKF)
009000180222     d   tasVLF                            inz   like(TITAS00F.tasVLF)
009100180222     d   tasIMV                            inz   like(TITAS00F.tasIMV)
009200180222     d   tasNAS                            inz   like(TITAS00F.tasNAS)
009300180222
009400180221       //--------------------------------------------------------------
009500180221       //? Definizione variabili globali.                               ?
009600180221       //--------------------------------------------------------------
009700180222
009800180222       // - ?Flags booleani?
009900180222     d $EoF            s               n   inz
010000180222
010100180222      *//· // - ?Stringhe SQL da eseguire?
010200180222     d*//· wSQL            s          32740    inz   varying
010300180222
010400180221       // - ?Campi di comodo?
010500180221     d*//· wDate_Iso       s               d   inz   datfmt(*iso)
010600180221     d wDelta          s             20  3 inz
010700180222     d wNumColliInc    s                   inz   like(TITAS00F.TASncl)
010800180221
010900180221       //--------------------------------------------------------------
011000180221       //? Definizione prototipi procedure.                             ?
011100180221       //--------------------------------------------------------------
011200180221
011300180221       // - ?Reperimento dati utente?
011400180221     d TIBS34ds      e ds                  inz
011500180221      /copy gaitrasrc/srcProtoPR,TIBS34R
011600180226
011700180226       // - ?Reperimento dati tabelle?
011800180226      /copy gaitrasrc/srcProtoPI,TRULTAB
011900180226      /copy gaitrasrc/srcProtoPR,TRULTAB
012000180226
012100180221       // - ?Controllo/Decodifica cliente?
012200180221      /copy gaitrasrc/srcProtoPI,TIBS69R
012300180221      /copy gaitrasrc/srcProtoPR,TIBS69R
012400180221
012500180221       // -? Ricerca unificante padre?
012600180221      /copy gaitrasrc/srcProtoPI,TIBS10R
012700180221      /copy gaitrasrc/srcProtoPR,TIBS10R
012800180221
012900180221       // -? Calcolo Delta del Cliente?
013000180221     d TISE70ds      e ds                  inz   qualified
013100180221     d TISE70R         pr                  extpgm('TISE70R')
013200180221     d   TISE70ds                          likeds(TISE70ds)
013300180221
013400180221       // - ?Parametri API QCAPCMD (Process Commands)?
013500180221     d Qcmd            s           2048    inz   varying
013600180221      /copy qSysInc/qRpgleSrc,QCAPCMD
013700180221       // - ?API QCAPCMD (Process Commands)?
013800180221      /copy gaitrasrc/srcProtoPR,QCAPCMD
013900180221
014000180221       // - ?Parametri gestione errori API.?
014100180221      /copy qsysinc/qrpglesrc,QUSEC
014200180221
014300180221       //--------------------------------------------------------------
014400180221       //? Definizione key-list.                                        ?
014500180221       //--------------------------------------------------------------
014600180221
014700180221       // - ?File TITAH30C?
014800180221     d keyTITAH30    e ds                  extname( TITAH30C : *key )
014900180221     d                                     prefix( k_ )  inz
015000180221
015100180221       // - ?File AZCMM01L?
015200180221     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
015300180221     d                                     prefix( k_ )  inz
015400180221
015500180221       // - ?File AZORG01L?
015600180221     d keyAZORG01    e ds                  extname( AZORG01L : *key )
015700180221     d                                     prefix( k_ )  inz
015800180221
015900180221       //--------------------------------------------------------------
016000180221       //? Riepilogo indicatori utilizzati.                             ?
016100180221       //--------------------------------------------------------------
016200180221
016300180221
016400180221       //--------------------------------------------------------------
016500180221       //? M A I N - L I N E                                            ?
016600180221       //--------------------------------------------------------------
016700180221
016800180222
016900180222     c     *Entry        plist
017000180222     c                   parm                    KPJBA
017100180221
017200180221      /free
017300180221
017400180221       // - ?Operazioni iniziali?
017500180221       exsr  sr_RoutInz;
017600180221
017700180222       // - ?Dichiarazione cursore.?
017800180222       exsr  sr_DeclareCursor;
017900180222
018000180222       // - ?Apertura cursore.?
018100180222       exsr  sr_OpenCursor;
018200180222
018300180222       // - ?Ciclo di elaborazione?
018400180222       reset  $EoF;
018500180221
018600180222       DoW  NOT $EoF;
018700180222         exsr  sr_ReadCursor;
018800180221       EndDo;
018900180221
019000180221       // - ?Eventuale scrittura record per l'ultimo cliente elaborato?
019100180222       if  wmiISP <> *zero  or  wmiPSP <> *zero;
019200180221         exsr  sr_Write_WFRMI;
019300180221       endif;
019400180222
019500180222       // - ?Chiusura cursore.?
019600180222       exsr  sr_CloseCursor;
019700180221
019800180221       // - ?Operazioni finali?
019900180221       exsr sr_RoutEnd;
020000180221
020100180221       //--------------------------------------------------------------
020200180221       //? Operazioni iniziali.                                         ?
020300180221       //--------------------------------------------------------------
020400180221       BEGSR  sr_RoutInz;
020500180222
020600180222         // - ?Impostazione opzioni per SQL?
020700180222         exec sql   set  option  DynUsrPrf = *Owner,
020800180222                                 CloSqlCsr = *EndMod;
020900180222
021000180221         // - ?Impostazione chiusura?
021100180221         *inLR = *on;
021200180221
021300180221         // - ?Reperimento parametri?
021400180221         TNSS19ds = kpjbu;
021500180221
021600180221         // - ?Reperimento dati job?
021700180221         exsr  sr_DatiJob;
021800180221
021900180221         // - ?Richiesta pulizia del work-file WFRMI00F?
022000180222         if  TNSS19ds.SS19cwf = 'S';
022100180221           Qcmd = 'CLRPFM file(*libl/WFRMI00F)';
022200180221           exsr  sr_ExecCmd;
022300180221           if  Qusei <> *blank;
022400180221             exsr  sr_RoutEnd;
022500180221           endif;
022600180221         endif;
022700180221
022800180222         // - ?Apertura del work-file WFRMI00F?
022900180222         open  WFRMI00F;
023000180221
023100180221       ENDSR;
023200180221
023300180221       //--------------------------------------------------------------
023400180221       //? Reperimento Dati del job (Utente/Operativi).                 ?
023500180221       //--------------------------------------------------------------
023600180221       BEGSR  sr_DatiJob;
023700180221
023800180221         in(e) §AzUte;
023900180221         if NOT %error;
024000180221           in(e) §DatiUte;
024100180221         endif;
024200180221         if %error or RSut = *blank;
024300180221           tibs34r ( tibs34ds );
024400180221           in §AzUte;
024500180221           in §DatiUte;
024600180221         endif;
024700180221
024800180221       ENDSR;
024900180222
025000180222       //--------------------------------------------------------------
025100180222       //? Dichiarazione cursore.                                       ?
025200180222       //--------------------------------------------------------------
025300180222       BEGSR  sr_DeclareCursor;
025400180222
025500180222         exec sql   declare C1   cursor for
025600180222
025700180222             select tasAAS, tasMGS, tasLNP, tasNRS, tasNSP, tasKSC,
025800180222                    tasNCL, tasPKF, tasVLF, tasIMV, tasNAS
025900180222               from TITAS00F
026000180222              where ( ( tasAAS * 10000 ) + tasMGS ) >= :TNSS19ds.SS19dsi
026100180222                and ( ( tasAAS * 10000 ) + tasMGS ) <= :TNSS19ds.SS19dsf
026200180222                and tasTBL in ( 'F1', 'A2' )
026300180222
026400180222              union
026500180222
026600180222             select tasAAS, tasMGS, tasLNP, tasNRS, tasNSP, tasKSC,
026700180222                    tasNCL, tasPKF, tasVLF, tasIMV, tasNAS
026800180222               from TITAS10F
026900180222              where ( ( tasAAS * 10000 ) + tasMGS ) >= :TNSS19ds.SS19dsi
027000180222                and ( ( tasAAS * 10000 ) + tasMGS ) <= :TNSS19ds.SS19dsf
027100180222                and tasTBL in ( 'F1', 'A2' )
027200180222
027300180222              union
027400180222
027500180222             select tasAAS, tasMGS, tasLNP, tasNRS, tasNSP, tasKSC,
027600180222                    tasNCL, tasPKF, tasVLF, tasIMV, tasNAS
027700180222               from TITASP0F
027800180222              where ( ( tasAAS * 10000 ) + tasMGS ) >= :TNSS19ds.SS19dsi
027900180222                and ( ( tasAAS * 10000 ) + tasMGS ) <= :TNSS19ds.SS19dsf
028000180222                and tasTBL in ( 'F1', 'A2' )
028100180222
028200180222           order by tasKSC, tasAAS, tasMGS
028300180222
028400180222          for fetch only;
028500180222
028600180222       ENDSR;
028700180222
028800180222       //--------------------------------------------------------------
028900180222       //? Apertura cursore.                                            ?
029000180222       //--------------------------------------------------------------
029100180222       BEGSR  sr_OpenCursor;
029200180222
029300180222         exec sql   open C1;
029400180222
029500180222         if  SQLcode < *zero;
029600180222           $EoF = *on;
029700180222           //*-wErr = 1;
029800180222           //*-exsr  sr_PrintERR;
029900180222         endif;
030000180222
030100180222       ENDSR;
030200180222
030300180221       //--------------------------------------------------------------
030400180222       //? Lettura cursore.                                             ?
030500180221       //--------------------------------------------------------------
030600180222       BEGSR  sr_ReadCursor;
030700180221
030800180222         clear  TITAS_ds;
030900180222
031000180222         exec sql   fetch next   from C1   into :TITAS_ds;
031100180222
031200180222         select;
031300180222
031400180222           when  SQLcode = 100;
031500180222             $EoF = *on;
031600180222
031700180222           when  SQLcode < *zero;
031800180222             $EoF = *on;
031900180222             //*-wErr = 2;
032000180222             //*-exsr  sr_PrintERR;
032100180222
032200180222           other;
032300180222             exsr  sr_Elab_Sped;
032400180221
032500180222         endsl;
032600180222
032700180222       ENDSR;
032800180222
032900180222       //--------------------------------------------------------------
033000180222       //? Chiusura cursore.                                            ?
033100180222       //--------------------------------------------------------------
033200180222       BEGSR  sr_CloseCursor;
033300180222
033400180222         exec sql   close C1;
033500180222
033600180222       ENDSR;
033700180222
033800180222       //--------------------------------------------------------------
033900180222       //? Elaborazione singola spedizione.                             ?
034000180222       //--------------------------------------------------------------
034100180222       BEGSR  sr_Elab_Sped;
034200180222
034300180222         // - ?Controllo eventuale "rottura di livello"?
034400180222         //   ?(cliente di bollettazione)?
034500180222         If  TITAS_ds.tasKSC <> wmiKSC;
034600180222
034700180222           // - ?Eventuale scrittura record per l'ultimo cliente elaborato?
034800180226           //   ?NON solo se rilevati colli incompatibili!?
034900180226           //*-if  wmiISP <> *zero  or  wmiPSP <> *zero;
035000180226           if  wmiKSC <> *zero;
035100180222             exsr  sr_Write_WFRMI;
035200180226           endif;
035300180222
035400180222           clear  WFRMI000;
035500180222           wmiKSC = TITAS_ds.tasKSC;
035600180222
035700180222         EndIf;
035800180221
035900180221         // - ?Calcolo totali per il cliente:?
036000180221         //   ?Totale Spedizioni?
036100180221         wmiTSP += 1;
036200180222         wmiTCL += TITAS_ds.tasNCL;
036300180222         wmiTPS += TITAS_ds.tasPKF;
036400180222         wmiTVL += TITAS_ds.tasVLF;
036500180222         wmiTIM += TITAS_ds.tasIMV;
036600180222         if  TITAS_ds.tasIMV <> *zero;
036700180221           wmiTSI += 1;
036800180221         endif;
036900180221
037000180221         // - ?Eventuale calcolo totali per le spedizioni?
037100180221         //   ?TOTALMENTE o PARZIALMENTE incompatibili?
037200180222         dTASnas = TITAS_ds.tasNAS;
037300180221
037400180221         If  dTASnas.§floINC = 'S';
037500180221
037600180221           exsr  sr_Calcolo_Incompatibili;
037700180221
037800180222           If  wNumColliInc >= TITAS_ds.tasNCL;
037900180221
038000180221             // - ?Calcolo totali per le spedizioni TOTALMENTE incompatibili?
038100180221             wmiISP += 1;
038200180222             wmiICL += TITAS_ds.tasNCL;
038300180222             wmiIPS += TITAS_ds.tasPKF;
038400180222             wmiIVL += TITAS_ds.tasVLF;
038500180222             wmiIIM += TITAS_ds.tasIMV;
038600180222             if  TITAS_ds.tasIMV <> *zero;
038700180221               wmiISI += 1;
038800180221             endif;
038900180221
039000180221           Else;
039100180221
039200180221             // - ?Calcolo totali per le spedizioni PARZIALMENTE incompatibili?
039300180221             wmiPSP += 1;
039400180222             wmiPCL += TITAS_ds.tasNCL;
039500180221             wmiPCI += wNumColliInc;
039600180222             wmiPPS += TITAS_ds.tasPKF;
039700180222             wmiPVL += TITAS_ds.tasVLF;
039800180222             wmiPIM += TITAS_ds.tasIMV;
039900180222             if  TITAS_ds.tasIMV <> *zero;
040000180221               wmiPSI += 1;
040100180221             endif;
040200180221
040300180221           EndIf;
040400180221
040500180221         EndIf;
040600180221
040700180221       ENDSR;
040800180221
040900180221       //--------------------------------------------------------------
041000180221       //? Calcoli per le spedizioni con colli incpatibili.             ?
041100180221       //--------------------------------------------------------------
041200180221       BEGSR  sr_Calcolo_Incompatibili;
041300180221
041400180221         clear  wNumColliInc;
041500180221
041600180221         // - ?Ricerca colli incompatibili sul *file TITAH30C *trk "J"?
041700180221         clear  keyTITAH30;
041800180222         k_tahAAS = TITAS_ds.tasAAS;
041900180222         k_tahLNP = TITAS_ds.tasLNP;
042000180222         k_tahNRS = TITAS_ds.tasNRS;
042100180222         k_tahNSP = TITAS_ds.tasNSP;
042200180221         k_tahTRC = 'J';
042300180221         setll  %kds( keyTITAH30 )  TITAH30C;
042400180221
042500180221         if  NOT %equal( TITAH30C );
042600180221           leavesr;
042700180221         endif;
042800180221
042900180221         // - ?Conteggio dei colli incompatibili?
043000180221         readE  %kds( keyTITAH30 )  TITAH30C;
043100180221
043200180221         DoW  NOT %eof( TITAH30C );
043300180221
043400180221           wNumColliInc += 1;
043500180221
043600180221           readE  %kds( keyTITAH30 )  TITAH30C;
043700180221
043800180221         EndDo;
043900180221
044000180221       ENDSR;
044100180221
044200180221       //--------------------------------------------------------------
044300180221       //? Registrazione dati del cliente appena elaborato in WFRMI00F. ?
044400180221       //--------------------------------------------------------------
044500180221       BEGSR  sr_Write_WFRMI;
044600180221
044700180221         // - ?Registrazione record in WFRMI00F?
044800180222         wmiDTI  = TNSS19ds.SS19dsi;
044900180222         wmiDTF  = TNSS19ds.SS19dsf;
045000180221
045100180221         // - ?Decodifica Cliente di Bollettazione?
045200180221         clear  TIBS69ds;
045300180221         I69sif = knsif;
045400180221         I69kcc = DUTkci;
045500180221         I69kac = wmiKSC;
045600180221         TIBS69R ( TIBS69ds :
045700180221                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
045800180221         If  O69err = *blank;
045900180221           wmiRSC  = ACOrag;
046000180221         EndIf;
046100180221
046200180221         // - ?Reperimento Cliente Unificante?
046300180221         clear  TIBS10ds;
046400180221         //*·TIBS10ds.D10drf = %dec( wDate_Iso );
046500180222         TIBS10ds.D10drf = TNSS19ds.SS19dsf;
046600180221         TIBS10ds.D10tle = 'ST';
046700180221         TIBS10ds.D10paf = 'P';   //? Verifica se è figlio e ne reperisce il padre?
046800180221         TIBS10ds.D10cod = wmiKSC;
046900180221         TIBS10R ( TIBS10ds );
047000180221         if  TIBS10ds.D10err = *blank  and  TIBS10ds.D10cop > *zero;
047100180221           wmiKSU = TIBS10ds.D10cop;
047200180221         else;
047300180221           wmiKSU = TIBS10ds.D10cod;
047400180221         endif;
047500180221
047600180221         // - ?Decodifica e Reperimento Commerciale del Cliente Unificante?
047700180221         clear  TIBS69ds;
047800180221         I69sif = knsif;
047900180221         I69kcc = DUTkci;
048000180221         I69kac = wmiKSU;
048100180221         I69kcp = I69kac;
048200180221         TIBS69R ( TIBS69ds :
048300180221                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
048400180221         if  O69err <> *on;
048500180221           wmiRSU = ACOrag;
048600180221           wmiCLV = CLPclv;
048700180221           wmiCMU = CLPage;
048800180221         endif;
048900180221
049000180221         // - ?Reperimento Commerciale Unificante del Cliente Unificante?
049100180221         k_CMMcod = wmiCMU;
049200180221         chain  %kds( keyAZCMM01 )  AZCMM000;
049300180221         if  %found( AZCMM01L );
049400180226           wmiCMU  = CMMuni;
049500180226           wmiCMUD = CMMdes;
049600180221         endif;
049700180221
049800180221         // - ?(Decodifica e) Reperimento Filiale del Comm.le Unificante?
049900180221         k_CMMcod = wmiCMU;
050000180221         chain  %kds( keyAZCMM01 )  AZCMM000;
050100180221         if  %found(AZCMM01L);
050200180226           wmiCMU   = CMMuni;
050300180226           wmiCMUD  = CMMdes;
050400180221           k_ORGfil = CMMfil;
050500180221         endif;
050600180221
050700180221         // - ?Reperimento Distretto & Area del Commerciale Unificante?
050800180221         if  k_ORGfil <= *zero;
050900180221           k_ORGfil = wmiCMU / 10000;
051000180221         endif;
051100180221         chain  %kds( keyAZORG01 )  AZORG;
051200180221         if  %found(AZORG01L)  and  ORGfva = *blank;
051300180221           wmiDCU = ORGfl3;
051400180221           wmiACU = ORGcar;
051500180221         endif;
051600180226
051700180226         // - ?Decodifica Distretto del Commerciale Unificante?
051800180226         clear  ds17;
051900180226         ds_TNTBE.TBEke1 = wmiDCU;
052000180226         if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
052100180226                          *omit : *omit : *omit :
052200180226                          *omit : *omit :
052300180226                          *omit : *omit : *omit : *omit :
052400180226                          *omit : *omit : *omit : *omit :
052500180226                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
052600180226                        = *zero      AND
052700180226             ds_TNTBE.TBEatb = *blank;
052800180226           ds17  = ds_TNTBE.TBEuni;
052900180226         endif;
053000180226         wmiDCUD = ds17.§17des;
053100180226
053200180226         // - ?Decodifica Area del Commerciale Unificante?
053300180226         clear  ds05;
053400180226         ds_TNTBE.TBEke1 = %editc( wmiACU : 'X' );
053500180226         if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
053600180226                          *omit : *omit : *omit :
053700180226                          *omit : *omit :
053800180226                          *omit : *omit : *omit : *omit :
053900180226                          *omit : *omit : *omit : *omit :
054000180226                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
054100180226                        = *zero      AND
054200180226             ds_TNTBE.TBEatb = *blank;
054300180226           ds05  = ds_TNTBE.TBEuni;
054400180226         endif;
054500180226         wmiACUD = ds05.§05des;
054600180226
054700180221         // - ?Reperimento dei dati del Cliente Bollettazione nel mese?
054800180221         //   ?della data "AL" per calcolo DELTA?
054900180221         clear  TISE70ds;
055000180221         TISE70ds.I70tla = 'S';
055100180221         TISE70ds.I70tle = 'ST';
055200180221         TISE70ds.I70ksc = wmiKSC;
055300180222         TISE70ds.I70dai = TNSS19ds.SS19dsf / 100;
055400180221         TISE70ds.I70daf = TISE70ds.I70dai;
055500180221
055600180221         TISE70R ( TISE70ds );
055700180221
055800180221         If  TISE70ds.O70rir <> *zero;
055900180221           wDelta = ( ( TISE70ds.O70rir - TISE70ds.O70rip ) * 100 )
056000180221                      / TISE70ds.O70rir;
056100180221         Else;
056200180221           wDelta = ( TISE70ds.O70rir - TISE70ds.O70rip ) * 100;
056300180221         EndIf;
056400180221         select;
056500180221           when  wDelta = *zero;
056600180221           when  wDelta >= 1000;
056700180221             wmiPDE = 999,9;
056800180222           when  wDelta <= -1000;
056900180221             wmiPDE = -999,9;
057000180221           other;
057100180221             eval(H) wmiPDE = wDelta;
057200180221         endsl;
057300180221
057400180221         // - ?Scrittura rec. nel *file WFRMI00F?
057500180221         write  WFRMI000;
057600180221
057700180221       ENDSR;
057800180221
057900180221       //--------------------------------------------------------------
058000180221       //? Esecuzione del comando (già impostato).                      ?
058100180221       //--------------------------------------------------------------
058200180221       BEGSR  sr_ExecCmd;
058300180221
058400180221         clear Qcap0100;
058500180221         Qcabcsdh = *off;
058600180221         Qcapa    = *off;
058700180221         Qcacmdss = *off;
058800180221         Qcaerved = *allX'00';
058900180221
059000180221         clear Qusec;
059100180221         Qusbprv  = %size(Qusec);
059200180221
059300180221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
059400180221                           %size(Qcap0100) : 'CPOP0100' : *omit :
059500180221                           0 : 0 : Qusec);
059600180221
059700180221         //if  Qusei <> *blank;
059800180221         //  ...;
059900180221         //endif;
060000180221
060100180221       ENDSR;
060200180221
060300180221       //--------------------------------------------------------------
060400180221       //? Operazioni finali.                                           ?
060500180221       //--------------------------------------------------------------
060600180221       BEGSR  sr_RoutEnd;
060700180221
060800180222         // - ?Chiusura applicazioni precedentemente richiamate?
060900180221         reset  TIBS69ds;
061000180222         tibs69r ( TIBS69ds :
061100180226                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
061200180226
061300180226         cloTabella ( c_Tabel );
061400180221
061500180221         // - ?Uscita dal *pgm?
061600180221         return;
061700180221
061800180221       ENDSR;
061900180221
062000180221      /end-free
