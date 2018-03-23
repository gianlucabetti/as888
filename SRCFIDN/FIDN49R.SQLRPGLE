000100150730       //==============================================================
000200150807       //?Emissione Progetti di Liquidazione.                          ?
000300150730       //==============================================================
000400150730
000500150730       //--------------------------------------------------------------
000600150730       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150730       //--------------------------------------------------------------
000800150730
000900150730     /*PRM dbgview(*source)
001000150730     /*END
001100150730
001200150730       //--------------------------------------------------------------
001300150730       //?Specifiche di controllo.                                     ?
001400150730       //--------------------------------------------------------------
001500150730
001600150730     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150730     h dftactgrp(*no)
001800150803     h bnddir('TRUL')
001900150731     h alwnull(*inputonly)
002000150730
002100150730       //--------------------------------------------------------------
002200150730       //?Dichiarazione file.                                          ?
002300150730       //--------------------------------------------------------------
002400150803
002500150803       // -?DANNI: Anagrafica Fasi Testata?
002600150803     fFNDFA01L  if   e           k disk
002700150807
002800150807       // -?DANNI: Testata C.A.?
002900150807     fFNDCT01L  if   e           k disk
003000150730
003100150731       // -?DANNI: Liquidazione C.A.?
003200150731     fFNDCL01L  if   e           k disk
003300150731
003400150731       // -?DANNI: Testata Eventi/Denuncia?
003500150731     fFNDET01L  if   e           k disk    usropn
003600150731
003700150731       // -?Bolle Sede?
003800150731     fTITAS30C  if   e           k disk    usropn
003900150731
004000150730       // -?Video?
004100150805     fFIDN49D   cf   e             workstn
004200150731     f                                     sfile( DN49S01 : S01nrr )
004300150731     f                                     indds( IndDspF )
004400150731     f                                     infds( InfDspF )
004500150730
004600150730       //--------------------------------------------------------------
004700150730       //?Definizione costanti.                                        ?
004800150730       //--------------------------------------------------------------
004900150730
005000150730       // -?Fasi C.A. elaborate:?
005100150731       //  ?·660 = Ricezione totale documenti?
005200150803     d c_RicDoc        c                   const(660)
005300150731       //  ?·690 = Ristampa Progetto di Liquidazione?
005400150803     d c_RsPLiq        c                   const(690)
005500150731       //  ?·700 = Stampa Progetto di Liquidazione?
005600150803     d c_StPLiq        c                   const(700)
005700150731
005800150731       // -?Data limite per test Evento Fortuito con Pratica in Franchigia?
005900150731     d c_DataLimite_EventoFortuito_Franchigia...
006000150731     d                 c                   const(20050418)
006100150803
006200150803       // -?Sede?
006300150803     d c_Sede          c                   const(046)
006400150731
006500150731       // -?Numero di record in ciascuna videata di subfile?
006600150807     d c_SflPag        c                   const(17)
006700150731
006800150731       // -?Numero massimo di record gestibili in un SubFile?
006900150731     d c_MaxRec        c                   const(9999)
007000150731
007100150731       // -?Attributi di Visualizzazione?
007200150731     d c_DspAtr_Norm   c                   const(x'20')
007300150731     d c_DspAtr_RI     c                   const(x'21')
007400150731     d c_DspAtr_HI     c                   const(x'22')
007500150731     d c_DspAtr_ND     c                   const(x'27')
007600150731     d c_DspAtr_BL     c                   const(x'28')
007700150730
007800150730       // -?Costante per controllo "caratteri solo numerici"?
007900150730     d c_Digits        c                   const('0123456789')
008000150730
008100150730       // -?Tasti funzionali a video?
008200150730     d c_F01           c                   const(x'31')
008300150730     d c_F02           c                   const(x'32')
008400150730     d c_F03           c                   const(x'33')
008500150730     d c_F04           c                   const(x'34')
008600150730     d c_F05           c                   const(x'35')
008700150730     d c_F06           c                   const(x'36')
008800150730     d c_F07           c                   const(x'37')
008900150730     d c_F08           c                   const(x'38')
009000150730     d c_F09           c                   const(x'39')
009100150730     d c_F10           c                   const(x'3A')
009200150730     d c_F11           c                   const(x'3B')
009300150730     d c_F12           c                   const(x'3C')
009400150730     d c_F13           c                   const(x'B1')
009500150730     d c_F14           c                   const(x'B2')
009600150730     d c_F15           c                   const(x'B3')
009700150730     d c_F16           c                   const(x'B4')
009800150730     d c_F17           c                   const(x'B5')
009900150730     d c_F18           c                   const(x'B6')
010000150730     d c_F19           c                   const(x'B7')
010100150730     d c_F20           c                   const(x'B8')
010200150730     d c_F21           c                   const(x'B9')
010300150730     d c_F22           c                   const(x'BA')
010400150730     d c_F23           c                   const(x'BB')
010500150730     d c_F24           c                   const(x'BC')
010600150730     d c_Enter         c                   const(x'F1')
010700150730     d c_RollDown      c                   const(x'F4')
010800150730     d c_RollUp        c                   const(x'F5')
010900150730
011000150730       //--------------------------------------------------------------
011100150730       //?Definizione schiere.                                         ?
011200150730       //--------------------------------------------------------------
011300150805
011400150805       // -?Fasi gestite: "660", "690" e "700"?
011500150805     d sk_FCA          s                   dim( 3)  like(DFAfca)  inz
011600150805       // -?Descrizione fasi gestite: "660", "690" e "700"?
011700150805     d sk_FCAd         s                   dim( 3)  like(DFAdes)  inz
011800150730
011900150730       // -?Messaggi di errore?
012000150807     d sk_Msg          s             78    dim( 6)  ctdata  perrcd( 1)
012100150730
012200150730       //--------------------------------------------------------------
012300150730       //?Definizione aree dati.                                       ?
012400150730       //--------------------------------------------------------------
012500150730
012600150730       // -?Dati utente?
012700150730     d §AzUte        e ds                  extname(AZUTE00F)
012800150730     d                                     dtaara
012900150730     d §DatiUte      e ds                  extname(dDatiUte)
013000150730     d                                     dtaara
013100150730
013200150730       //--------------------------------------------------------------
013300150730       //?Definizione strutture dati.                                  ?
013400150730       //--------------------------------------------------------------
013500150730
013600150730       // -?InfDS?
013700150730     d InfDspF         ds
013800150730     d   dsp_aid             369    369a
013900150731     d*//sfl_rrn             376    377i 0
014000150731     d*//min_nrr             378    379i 0
014100150731     d*//num_rcds            380    381i 0
014200150730
014300150730       // -?Indicatori su DspF?
014400150730     d IndDspF         ds                  inz
014500150731         // ·?Emissione messaggio di errore?
014600150731     d   $ErrMessage                   n   overlay( IndDspF : 28 )
014700150731         // ·?Indicatori di gestione del subfile?
014800150731     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
014900150731     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
015000150731     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
015100150731     d   $SflEnd                       n   overlay( IndDspF : 33 )
015200150731         // ·?Indicatori per attibuti di visualizzazione?
015300150805     d   $EvidNoMailBen...
015400150805     d                                 n   overlay( IndDspF : 40 )
015500150805     d   $ProtectOpz                   n   overlay( IndDspF : 49 )
015600150731         // ·?Posizionamento cursore?
015700150731     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
015800150731         // ·?Riemissione videata?
015900150731     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
016000150730
016100150730       // -?Status ds?
016200150730     d Status         sds
016300150730     d   SDSpgmName      *proc
016400150730
016500150730       // -?Parametri ricevuti?
016600150730     d KPJBA         e ds
016700150730
016800150730       // -?DANNI: Anagrafica Fasi?
016900150730     d ds_FNDFA      e ds                  extname(FNDFA00F)
017000150730     d                                     inz
017100150731
017200150731       // -?Dati estratti via SQL?
017300150804     d ds_FNDCT      e ds                  inz   extname(FNDCT00F)
017400150804     d FNDCF00F      e ds                  based(nullPtr)
017500150804     d                                           qualified
017600150804     d ds_FNDCF        ds                  inz
017700150804     d   dcfDFC                            inz   like(FNDCF00F.dcfDFC)
017800150731       // -?Flag Operativi di FNDCT00F.DCTFLO?
017900150731     d dDCT01        e ds                  inz   qualified
018000150731
018100150731       // -?Tabella "TAD" = Tipi Anomalia Danni?
018200150731     d dTAD          e ds                  inz   qualified
018300150730
018400150730       //--------------------------------------------------------------
018500150730       //?Definizione variabili globali.                               ?
018600150730       //--------------------------------------------------------------
018700150730
018800150730       // -?Flags booleani?
018900150730     d $Fine           s               n   inz
019000150731     d $EoF            s               n   inz
019100150730     d $InzS01         s               n   inz(*on)
019200150731     d $ImportoTrattenuto...
019300150731     d                 s               n   inz
019400150731     d $Franchigia...
019500150731     d                 s               n   inz
019600150805
019700150805       // -?Indici di schiera?
019800150805     d xx              s              3  0 inz
019900150730
020000150730       // -?Variabili per la gestione del video?
020100150731     d $Video          s              2    inz('S1')
020200150731     d S01nrr          s              5i 0 inz
020300150731     d w_SflPag        s              3  0 inz
020400150731     d wPag            s              5  0 inz
020500150731     d wRig            s              3  0 inz
020600150731
020700150731       // -?Stringhe SQL da eseguire?
020800150731     d wSQL            s          32740    inz   varying
020900150731
021000150731       // -?Campi di comodo?
021100150805     d*// wDate           s              8  0 inz
021200150805     d*// wDate_EUR       s               d   inz   datfmt(*eur)
021300150805     d wDate_DMY       s               d   inz   datfmt(*dmy)
021400150730
021500150730       //--------------------------------------------------------------
021600150730       //?Definizione prototipi procedure.                             ?
021700150730       //--------------------------------------------------------------
021800150730
021900150730       // -?Reperimento dati utente?
022000150730     d TIBS34ds      e ds                  inz
022100150730      /copy gaitrasrc/srcProtoPR,TIBS34R
022200150731
022300150731       // -?Reperimento dati tabelle?
022400150731      /copy gaitrasrc/srcProtoPI,TRULTAB
022500150731      /copy gaitrasrc/srcProtoPR,TRULTAB
022600150803
022700150803       // -?Ricerca Beneficiario in C.A.?
022800150803     d FIDNBEds      e ds                  inz
022900150803     d fidnBEr         pr                  extpgm('FIDNBER')
023000150803     d   fidnBEds                          likeds(FIDNBEds)
023100150803
023200150803       // -?Visualizzazione C.A.?
023300150803     d*// FIDN00ds      e ds                  inz
023400150803     d fidn01r         pr                  extpgm('FIDN01R')
023500150803     d   kpjba                             likeds(KPJBA)
023600150803
023700150803       // -?Manutenzione C.A.?
023800150803     d*// FIDN00ds      e ds                  inz
023900150803     d fidn03r         pr                  extpgm('FIDN03R')
024000150803     d   kpjba                             likeds(KPJBA)
024100150730
024200150807       // -?Emissione Progetto di Liquidazione?
024300150730     d FIDN00ds      e ds                  inz
024400150730     d fidn29r         pr                  extpgm('FIDN29R')
024500150730     d   kpjba                             likeds(KPJBA)
024600150803
024700150803       // -?Visualizzazione Bolla?
024800150803     d TNSB50ds      e ds                  inz
024900150803     d tnsb50r         pr                  extpgm('TNSB50R')
025000150803     d   kpjba                             likeds(KPJBA)
025100150731
025200150731       // -?API QCAPCMD (Process Commands)?
025300150731     d Qcmd            s           2048    inz  varying
025400150731      /copy qSysInc/qRpgleSrc,QCAPCMD
025500150731      /copy gaitrasrc/srcProtoPR,QCAPCMD
025600150731       // -?Parametri gestione errori API.?
025700150731      /copy qsysinc/qRpgleSrc,QUSEC
025800150730
025900150730       //--------------------------------------------------------------
026000150730       //?Definizione key-list.                                        ?
026100150730       //--------------------------------------------------------------
026200150807
026300150807       // -?File FNDCT01L?
026400150807     d keyFNDCT01    e ds                  extname( FNDCT01L : *key )
026500150807     d                                     prefix( k_ )  inz
026600150730
026700150731       // -?File FNDCL01L?
026800150731     d keyFNDCL01    e ds                  extname( FNDCL01L : *key )
026900150731     d                                     prefix( k_ )  inz
027000150731
027100150731       // -?File TITAS30C?
027200150731     d keyTITAS30    e ds                  extname( TITAS30C : *key )
027300150731     d                                     prefix( k_ )  inz
027400150730
027500150730       //--------------------------------------------------------------
027600150730       //?Riepilogo indicatori utilizzati.                             ?
027700150730       //--------------------------------------------------------------
027800150730
027900150730
028000150730       //--------------------------------------------------------------
028100150730       //?Fasi C.A. fissate a programma:                               ?
028200150730       //--------------------------------------------------------------
028300150803       //?660?= Ricezione totale documenti                   (c_RicDoc)
028400150803       //?690?= Ristampa progetto di liqidazione             (c_RsPLiq)
028500150803       //?700?= Stampa progetto di liqidazione               (c_StPLiq)
028600150730       //--------------------------------------------------------------
028700150730
028800150730       //--------------------------------------------------------------
028900150730       //?M A I N - L I N E                                            ?
029000150730       //--------------------------------------------------------------
029100150730
029200150730     c     *Entry        plist
029300150730     c                   parm                    KPJBA
029400150730
029500150730      /free
029600150730
029700150730       // -?Operazioni iniziali?
029800150730       exsr  sr_RoutInz;
029900150730
030000150731       // -?Gestione videate?
030100150730       DoW  $Fine = *off;
030200150730
030300150730         Select;
030400150731
030500150731           // -?Gestione subfile S1?
030600150730           When $Video = 'S1';
030700150730             exsr  sr_GesS01;
030800150731
030900150731           // -?? ? ? ? ??
031000150730           Other;
031100150730             $Fine = *on;
031200150731
031300150730         EndSl;
031400150730
031500150730       EndDo;
031600150730
031700150730       // -?Operazioni finali?
031800150730       exsr sr_RoutEnd;
031900150730
032000150730       //--------------------------------------------------------------
032100150730       //?Operazioni iniziali.                                         ?
032200150730       //--------------------------------------------------------------
032300150730       BEGSR  sr_RoutInz;
032400150731
032500150731         // -?Impostazione opzioni per SQL?
032600150731         exec sql   set  option  DynUsrPrf = *Owner,
032700150731                                 CloSqlCsr = *EndMod;
032800150730
032900150730         // -?Impostazione chiusura?
033000150730         *inLR = *on;
033100150730
033200150730         // -?Reperimento dati job?
033300150730         exsr  sr_DatiJob;
033400150730
033500150730         // -?Impostazione nome programma a video?
033600150730         V1Tpgm = SDSpgmName;
033700150803
033800150805         // -?Memorizzazione descrizione delle Fasi qui elaborate?
033900150805         clear  sk_FCA;
034000150805         clear  sk_FCAd;
034100150805         //  ?·660 = Ricezione totale documenti?
034200150805         chain  ( c_RicDoc )  FNDFA000;
034300150805         if  %found(FNDFA01L)  and  DFAatb = *blank;
034400150805           sk_FCA(01)  = DFAfca;
034500150805           sk_FCAd(01) = DFAdes;
034600150805         endif;
034700150805         //  ?·690 = Ristampa Progetto di Liquidazione?
034800150805         chain  ( c_RsPLiq )  FNDFA000;
034900150805         if  %found(FNDFA01L)  and  DFAatb = *blank;
035000150805           sk_FCA(02)  = DFAfca;
035100150805           sk_FCAd(02) = DFAdes;
035200150805         endif;
035300150805         //  ?·700 = Stampa Progetto di Liquidazione?
035400150803         chain  ( c_StPLiq )  FNDFA000;
035500150803         if  %found(FNDFA01L)  and  DFAatb = *blank;
035600150805           sk_FCA(03)  = DFAfca;
035700150805           sk_FCAd(03) = DFAdes;
035800150803         endif;
035900150731
036000150731         // -?Preparazione stringa SQL per l'estrazione dei dati?
036100150731         //  ?da visualizzare?
036200150731         exsr  sr_PrepSQL;
036300150730
036400150730       ENDSR;
036500150730
036600150730       //--------------------------------------------------------------
036700150730       //?Reperimento Dati del job (Utente/Operativi).                 ?
036800150730       //--------------------------------------------------------------
036900150730       BEGSR  sr_DatiJob;
037000150730
037100150730         in(e) §AzUte;
037200150730         if NOT %error;
037300150730           in(e) §DatiUte;
037400150730         endif;
037500150730         if %error or RSut = *blank;
037600150730           tibs34r ( tibs34ds );
037700150730           in §AzUte;
037800150730           in §DatiUte;
037900150730         endif;
038000150730
038100150730       ENDSR;
038200150730
038300150730       //--------------------------------------------------------------
038400150731       //?Gestione subfile S01.                                        ?
038500150730       //--------------------------------------------------------------
038600150731       BEGSR  sr_GesS01;
038700150730
038800150731         // -?Inizializzazione subfile?
038900150807         if  $InzS01 = *on  and  Not $errGenerico;
039000150731           exsr  sr_InzS01;
039100150731           $InzS01 = *off;
039200150730         endif;
039300150731
039400150731         // -?Emissione Testata e Piede con tasti funzionali abilitati?
039500150803         write  DN49T01;
039600150803         write  DN49P01;
039700150731
039800150731         // -?Posizionamento cursore?
039900150731         //  ?C1CsrRrn contiene il numero di riga del SubFile su cui?
040000150731         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
040100150731         //  ?si visualizza la pagina che era visualizzata quando?
040200150731         //  ?l'utente ha premuto l'ultimo tasto.?
040300150731         if  C1CsrRrn > *zero;
040400150731           C1RcdNbr = C1CsrRrn;
040500150731         else;
040600150731           C1RcdNbr = 1;
040700150731           write  DN49S00;          //(no rec.)?
040800150731         endif;
040900150730
041000150730         // -?Emissione videata?
041100150731         exfmt  DN49C01;
041200150730
041300150730         clear  V1Dmsg;
041400150731         reset  $ErrMessage;
041500150731         reset  $ErrGenerico;
041600150730
041700150730         Select;
041800150804
041900150804           // -?Sistema Informativo di Filiale => Uscita forzata?
042000150804           When  DUTlpo <> 'S';
042100150804             exsr  sr_CloseCursor;
042200150804             $Fine = *on;
042300150730
042400150730           // -?F3=Fine?
042500150730           When  dsp_aid = c_F03;
042600150731             exsr  sr_CloseCursor;
042700150730             $Fine = *on;
042800150731
042900150731           // -?F5=Aggiornamento?
043000150731           When  dsp_aid = c_F05;
043100150731             exsr  sr_CloseCursor;
043200150731             reset  $InzS01;
043300150731
043400150731           // -?Roll-Up?
043500150731           When  dsp_aid = c_RollUp;
043600150731             // -?Verifica raggiungimento del limite di records?
043700150731             //  ?registrabili nel subfile (9999)?
043800150731             if  S01nrr = *hival;
043900150731               $ErrGenerico = *on;
044000150731               $ErrMessage  = *on;
044100150804               V1Dmsg = sk_Msg(02);
044200150731               leavesr;
044300150731             endif;
044400150731             exsr sr_RollUpS01;
044500150731
044600150731           // -?SubFile vuoto?
044700150731           When  S01nrr = *zero;
044800150730
044900150730           // -?Invio?
045000150730           Other;
045100150731             // -?Gestione opzioni?
045200150731             exsr  sr_OpzS01;
045300150731             if  $ErrGenerico;
045400150730               leavesr;
045500150730             endif;
045600150730
045700150730         EndSl;
045800150730
045900150730       ENDSR;
046000150730
046100150730       //--------------------------------------------------------------
046200150731       //?Inizializzazione subfile S01.                                ?
046300150730       //--------------------------------------------------------------
046400150731       BEGSR  sr_InzS01;
046500150731
046600150731         // -?Pulizia subfile?
046700150731         $SflDsp_N    = *on;
046800150731         $SflDspCtl_N = *on;
046900150731         write  DN49C01;
047000150731         $SflDspCtl_N = *off;
047100150731         $SflEnd      = *off;
047200150731
047300150731         $SflNxtChg   = *off;
047400150731
047500150731         clear  w_SflPag;
047600150731         clear  C1RcdNbr;
047700150731         clear  C1CsrRrn;
047800150731         clear  S01nrr;
047900150731         clear  V1Dmsg;
048000150731         $ErrMessage  = *off;
048100150731         $ErrGenerico = *off;
048200150731
048300150731         // -?Spegnimento degli indicatori di errore?
048400150731         %subst( IndDspF : 50 ) = *off;
048500150804
048600150804         // -?Controllo del sistema informativo in cui si sta lavorando?
048700150804         if  DUTlpo <> 'S';
048800150804           $ErrGenerico = *on;
048900150804           $ErrMessage  = *on;
049000150804           V1Dmsg = sk_Msg(01);
049100150804           leavesr;
049200150804         endif;
049300150731
049400150731         // -?Caricamento della 1ª pagina di dati del subfile?
049500150731         // -?Apertura del cursore?
049600150731         exsr  sr_OpenCursor;
049700150731         // -?Lettura del file?
049800150731         exsr  sr_ReadCursor;
049900150731         DoW  Not $EoF  and  S01nrr < c_SflPag;
050000150731           exsr  sr_RollUpS01;
050100150731         EndDo;
050200150731
050300150731         // -?Visualizzazione del SFL (se ci sono dati)?
050400150804         $SflDsp_N = ( S01nrr <= *zero );
050500150731
050600150731         // -?Attivazione del SFLEND?
050700150804         $SflEnd   = ( $EoF = *on );
050800150731
050900150731         // -?Impaginazione subfile?
051000150731         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
051100150731         if S01nrr > *zero;
051200150804           C1RcdNbr = 1;
051300150804           C1CsrRrn = 1;
051400150731         else;
051500150731           clear C1RcdNbr;
051600150731         endif;
051700150730
051800150730       ENDSR;
051900150731
052000150731       //--------------------------------------------------------------
052100150731       //?Caricamento singola pagine SubFile S01.                      ?
052200150731       //--------------------------------------------------------------
052300150731       BEGSR  sr_RollUpS01;
052400150731
052500150731         S01nrr     = ( w_SflPag * c_SflPag );
052600150731         w_SflPag  += 1;
052700150731         $SflNxtChg = *off;
052800150731
052900150731         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
053000150804         DoW  $EoF   = *off    and
053100150804              S01nrr < *hival  and
053200150804              S01nrr < ( w_SflPag * c_SflPag );
053300150731
053400150731           // -?Caricamento dati nel record del subfile?
053500150731           exsr  sr_RieRecS01;
053600150731           S01nrr += 1;
053700150731           write  DN49S01;
053800150731
053900150731           // -?Lettura record successivo nell'archivio?
054000150804           exsr  sr_ReadCursor;
054100150731
054200150731         EndDo;
054300150731
054400150731         // -?Visualizzazione del SFL (se ci sono dati)?
054500150731         $SflDsp_N = ( S01nrr <= *zero );
054600150731
054700150731         // -?Attivazione (eventuale) del SFLEND?
054800150731         $SflEnd   = ( $EoF = *on );
054900150731
055000150731         // -?Posizionamento cursore al 1° rcd della pagina?
055100150731         if  S01nrr > *zero;
055200150731           wPag     = S01nrr / c_SflPag;
055300150731           wRig     = S01nrr - ( c_SflPag * wPag );
055400150731           C1RcdNbr = wPag * c_SflPag;
055500150731           if  wRig > *zero;
055600150731             C1RcdNbr = C1RcdNbr + 1;
055700150731           else;
055800150731             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
055900150731           endif;
056000150731         else;
056100150731           clear  C1RcdNbr;
056200150731         endif;
056300150731
056400150731         C1CsrRrn = C1RcdNbr;
056500150731
056600150731       ENDSR;
056700150731
056800150731       //--------------------------------------------------------------
056900150731       //?Caricamento singolo record nel SubFile S01.                  ?
057000150731       //--------------------------------------------------------------
057100150731       BEGSR  sr_RieRecS01;
057200150731
057300150731         clear  DN49S01;
057400150731
057500150731         // -?Reperimento Beneficiario?
057600150731         exsr  sr_ReperimBeneficiario;
057700150731
057800150731         // -?Campi Hidden?
057900150803         // ·?Fase C.A.?
058000151102         SH1fca  = dctFCA;
058100150805         // ·?Anno Spedizione?
058200151102         SH1aas  = dctAAS;
058300150805         // ·?Linea Partenza Spedizione?
058400151102         SH1lnp  = dctLNP;
058500150805         // ·?Numero Serie Spedizione?
058600151102         SH1nrs  = dctNRS;
058700150805         // ·?Numero Spedizione?
058800151102         SH1nsp  = dctNSP;
058900150803         // ·?Linea Arrivo Spedizione?
059000151102         SH1lna  = dctLNA;
059100150803         // ·?Tipo Anomalia Danni?
059200151102         SH1tad  = dctTAD;
059300151102         // ·?Flag Pratica in Franchigia?
059400151102         SH1pFra = $Franchigia;
059500150803         // ·?Flag "Cliente con e-Mail"?
059600151102         SH1inv  = oBEinv;
059700150731
059800150731         // -?Campi Output?
059900150803         // ·?Filiale C.A.?
060000151102         S01fil = dctFIL;
060100150803         // ·?Numero C.A.?
060200150803         S01nca = dctNCA;
060300150805         // ·?Anno C.A.?
060400150805         S01aac = dctAAC;
060500150805         // ·?Ultima Fase C.A.?
060600150805         xx = %lookup( dctFCA : sk_FCA );
060700150805         if  xx > *zero;
060800150805           S01fca = sk_FCAd(xx);
060900150805         else;
061000150805           S01fca = %editc( dctFCA : 'X' ) + '-' + '??????????';
061100150805         endif;
061200150805         // ·?Data Ultima Fase C.A.?
061300150805         wDate_DMY = %date( dcfDFC : *iso );
061400150805         S01dfc = %dec( wDate_DMY );
061500150803         // ·?Codice e Descrizione Cliente?
061600150805         S01rsb = %editc( oBEksc : 'X' ) + ' ' + oBErag;
061700150731
061800150731         // -?Attributi di visualizzazione?
061900150805         // ·?Protezione campo Opzione?
062000150805         $ProtectOpz    = *off;
062100150910         // ·?Cliente beneficiario senza e-mail (anche se Ristampa)?
062200151102         //  ?(SE Pratica NON in Franchigia)?
062300150805         $EvidNoMailBen = ( SH1inv <> 'M'       and
062400150910                          //SH1fca <> c_RsPLiq  and
062500151102                            SH1pFra = *off      and
062600150930                            %subst( S01rsb : 1 : 7 ) <> *zero   and
062700150805                            %subst( S01rsb : 4 : 4 ) <> '8888'  and
062800150805                            %subst( S01rsb : 4 : 4 ) <> '9999' );
062900150731
063000150731       ENDSR;
063100150731
063200150731       //--------------------------------------------------------------
063300150731       //?Gestione opzioni nel SubFile S01.                            ?
063400150731       //--------------------------------------------------------------
063500150731       BEGSR  sr_OpzS01;
063600150731
063700150731         // -?Ciclo di lettura subfile?
063800150803         readc  DN49S01;
063900150731
064000150805         DOW  NOT  %eof(FIDN49D);
064100150731
064200150731           %subst(IndDspF : 50) = *off;
064300150805           $SflNxtChg  = *off;
064400150805           $ProtectOpz = *off;
064500150805           C1RcdNbr    = S01nrr;
064600150807
064700150807           // -?Controlli sulla C.A. in funzione dell'opzione inserita?
064800150807           if  S01opz <> *blank;
064900150807             exsr  sr_Ctrl_CA;
065000150807           endif;
065100150731
065200150731           Select;
065300150731
065400150731             // -?Nessuna opzione?
065500150731             When  S01opz = *blank;
065600150807
065700150807             // -?Opzione non più gestibile per la C.A.?
065800150807             When  $errGenerico;
065900150731
066000150803             // -?2 = Manutenzione C.A.?
066100150803             When  S01opz = '2';
066200150803               clear  FIDN00ds;
066300150803               i00mod = 'M';
066400150803               i00fca = SH1fca;
066500150803               //i00fgs = DUTpou;
066600150803               i00aac = S01aac;
066700150803               i00fil = S01fil;
066800150803               i00nca = S01nca;
066900150803               i00aas = SH1aas;
067000150803               i00lnp = SH1lnp;
067100150803               i00nrs = SH1nrs;
067200150803               i00nsp = SH1nsp;
067300150803               i00lna = SH1lna;
067400150803               i00tad = SH1tad;
067500150803               //i00aae =
067600150803               //i00nev =
067700150803               //i00nce =
067800150803               i00tpb = 'S';
067900150803               KPJBU  = FIDN00ds;
068000150803               fidn03r ( KPJBA ) ;
068100150803               FIDN00ds = KPJBU;
068200150803               select;
068300150803                 when  o00err = 'S'     and  o00msg <> *blank;
068400150803                   $errGenerico = *on;
068500150803                   $errMessage  = *on;
068600150803                   V1Dmsg = o00msg;
068700150803                 when  o00f03 = 'S';
068800150803                   $errGenerico = *on;
068900150803                 when  o00F03 = *blank  and  o00F12 =  *blank;
069000150803                   $InzS01      = *on;
069100150803               endsl;
069200150803
069300150803             // -?5 = Visualizzazione C.A.?
069400150803             When  S01opz = '5';
069500150803               clear  FIDN00ds;
069600150803               //i00mod = 'I';
069700150803               //i00fca = SH1fca;
069800150803               //i00fgs = DUTpou;
069900150805               i00aac = S01aac;
070000150803               i00fil = S01fil;
070100150803               i00nca = S01nca;
070200150805               i00aas = SH1aas;
070300150805               i00lnp = SH1lnp;
070400150805               i00nrs = SH1nrs;
070500150805               i00nsp = SH1nsp;
070600150803               i00lna = SH1lna;
070700150803               I00tad = SH1tad;
070800150803               //i00aae =
070900150803               //i00nev =
071000150803               //i00nce =
071100150803               i00tpb = 'S';
071200150803               KPJBU  = FIDN00ds;
071300150803               fidn01r ( KPJBA );
071400150803               FIDN00ds = KPJBU;
071500150803               select;
071600150803                 when  o00err = 'S'     and  o00msg <> *blank;
071700150803                   $errGenerico = *on;
071800150803                   $errMessage  = *on;
071900150803                   V1Dmsg = o00msg;
072000150803                 when  o00f03 = 'S';
072100150803                   $errGenerico = *on;
072200150803               endsl;
072300150803
072400150803             // -?9 = Visualizzazione Bolla?
072500150803             When  S01opz = '9';
072600150803               clear  TNSB50ds;
072700150803               i50op0 = 'D05';
072800150805               d50aas = SH1aas;
072900150805               d50lnp = SH1lnp;
073000150805               d50nrs = SH1nrs;
073100150805               d50nsp = SH1nsp;
073200150803               KPJBU  = TNSB50ds;
073300150803               tnsb50r ( KPJBA );
073400150803               TNSB50ds = KPJBU;
073500150803               select;
073600150803                 when  o50err = *on     and  o50msg <> *blank;
073700150803                   $errGenerico = *on;
073800150803                   $errMessage  = *on;
073900150803                   V1Dmsg = o50msg;
074000150805                 when  o50f03 = *on;
074100150803                   $errGenerico = *on;
074200150803               endsl;
074300150803
074400150803             // -?G = Gestione C.A.?
074500150803             When  S01opz = 'G';
074600150803               clear  FIDN00ds;
074700150803               //i00mod = 'G';
074800150803               i00fca = c_StPLiq;
074900150803               i00fgs = DUTpou;
075000150803               i00aac = S01aac;
075100150803               i00fil = S01fil;
075200150803               i00nca = S01nca;
075300150803               i00aas = SH1aas;
075400150803               i00lnp = SH1lnp;
075500150803               i00nrs = SH1nrs;
075600150803               i00nsp = SH1nsp;
075700150803               i00lna = SH1lna;
075800150803               I00tad = SH1tad;
075900150803               //i00aae =
076000150803               //i00nev =
076100150803               //i00nce =
076200150803               i00tpb = 'S';
076300150803               KPJBU  = FIDN00ds;
076400150803               fidn29r ( KPJBA );
076500150803               FIDN00ds = KPJBU;
076600150803               select;
076700150803                 when  o00err = 'S'     and  o00msg <> *blank;
076800150803                   $errGenerico = *on;
076900150803                   $errMessage  = *on;
077000150803                   V1Dmsg = o00msg;
077100150803                 when  o00f03 = 'S';
077200150803                   $errGenerico = *on;
077300150803                 when  o00F03 = *blank  and  o00F12 =  *blank;
077400150805                   $ProtectOpz  = *on;
077500150805                   $InzS01      = *on;
077600150803               endsl;
077700150731
077800150731             // -?? = Opzione NON valida?
077900150731             Other;
078000150731               $ErrGenerico = *on;
078100150731               $ErrMessage  = *on;
078200150731               $PosCurOpz   = *on;
078300150804               V1Dmsg = sk_Msg(03);
078400150731
078500150731           EndSl;
078600151102
078700151102           // -?Reimpostazione Attributi di visualizzazione?
078800151102           // ·?Protezione campo Opzione?
078900151102           $ProtectOpz    = *off;
079000151102           // ·?Cliente beneficiario senza e-mail (anche se Ristampa)?
079100151102           //  ?(SE Pratica NON in Franchigia)?
079200151102           $EvidNoMailBen = ( SH1inv <> 'M'       and
079300151102                            //SH1fca <> c_RsPLiq  and
079400151102                              SH1pFra = *off      and
079500151102                              %subst( S01rsb : 1 : 7 ) <> *zero   and
079600151102                              %subst( S01rsb : 4 : 4 ) <> '8888'  and
079700151102                              %subst( S01rsb : 4 : 4 ) <> '9999' );
079800150731
079900150731           // -?Aggiornamento subfile?
080000150731           Select;
080100150731             When  $ErrMessage;
080200150731               $SflNxtChg = *on;
080300150731               C1CsrRrn   = C1RcdNbr;
080400150731             When  $ErrGenerico;
080500150731               $SflNxtChg = *on;
080600150731               C1CsrRrn   = C1RcdNbr;
080700150731               clear  S01opz;
080800150731             Other;
080900150731               C1CsrRrn   = C1RcdNbr;
081000150731               clear  S01opz;
081100150731           EndSl;
081200150731
081300150805           update  DN49S01;
081400150731
081500150731           // -?Uscita dal ciclo di lettura?
081600150731           if  $ErrMessage  or  $ErrGenerico;
081700150805             clear  $InzS01;
081800150731             leave;
081900150731           endif;
082000150731
082100150731           // -?Lettura rec. variato sucessivo?
082200150805           readc  DN49S01;
082300150731
082400150731         EndDo;
082500150803
082600150803         // -?Se richiesto il RI-caricamento del subfile:?
082700150803         //  ?occorre chiudere il cursore SQL?
082800150807         If  $InzS01  and  Not $errGenerico;
082900150803           exsr  sr_CloseCursor;
083000150803         EndIf;
083100150731
083200150731       ENDSR;
083300150807
083400150807       //--------------------------------------------------------------
083500150807       //?Verifica eventuali modifiche apportate alla C.A. dal momento ?
083600150807       //?  del caricamento nel subfile a quello attuale.              ?
083700150807       //?Potrebbe risultare già chiusa (e quindi NON modificabile -   ?
083800150807       //?  opz. "2") o con il progetto di liquidazione già stampato   ?
083900150807       //?  (e quindi NON gestibile - opz. "G") da un altro utente ?   ?
084000150807       //--------------------------------------------------------------
084100150807       BEGSR  sr_Ctrl_CA;
084200150807
084300150807         clear  keyFNDCT01;
084400150807         k_DCTaac = S01aac;
084500150807         k_DCTfil = S01fil;
084600150807         k_DCTnca = S01nca;
084700150807         chain  %kds( keyFNDCT01 )  FNDCT000;
084800150807
084900150807         // -?C.A. non trovata (impossibile...)?
085000150807         If  not %found(FNDCT01L);
085100150807           $ErrGenerico = *on;
085200150807           $ErrMessage  = *on;
085300150807           $PosCurOpz   = *on;
085400150807           V1Dmsg = sk_Msg(04);
085500150807           leavesr;
085600150807         EndIf;
085700150807
085800150807         Select;
085900150807
086000150807           // -?Opz. "2" = Manutenzione C.A.?
086100150807           When  S01opz = '2';
086200150807             // -?C.A. nel frattempo chiusa?
086300150807             if  DCTdch > *zero;
086400150807               $ErrGenerico = *on;
086500150807               $ErrMessage  = *on;
086600150807               $PosCurOpz   = *on;
086700150807               V1Dmsg = sk_Msg(05);
086800150807               leavesr;
086900150807             endif;
087000150807
087100150807           // -?Opz. "G" = Gestione C.A. (Emissione P.L.)?
087200150807           When  S01opz = 'G';
087300150807             // -?C.A. in fase non più idonea?
087400150807             //  ?·660 = Ricezione totale documenti?
087500150807             //  ?·690 = Ristampa Progetto di Liquidazione?
087600150807             if  DCTfca <> c_RicDoc  and
087700150807                 DCTfca <> c_RsPLiq;
087800150807               $ErrGenerico = *on;
087900150807               $ErrMessage  = *on;
088000150807               $PosCurOpz   = *on;
088100150807               V1Dmsg = sk_Msg(06);
088200150807               leavesr;
088300150807             endif;
088400150807
088500150807         EndSl;
088600150807
088700150807       ENDSR;
088800150731
088900150731       //--------------------------------------------------------------
089000150731       //?Reperimento Beneficiario della C.A.                          ?
089100150731       //--------------------------------------------------------------
089200150731       BEGSR  sr_ReperimBeneficiario;
089300150731
089400150731         dDCT01 = dctFLO;
089500150731
089600150731         // -?Reperimento importi C.A.?
089700150731         clear  keyFNDCL01;
089800150731         k_DCLaac = dctAAC;
089900150731         k_DCLfil = dctFIL;
090000150731         k_DCLnca = dctNCA;
090100150731         chain  %kds( keyFNDCL01 ) FNDCL000;
090200150731         $ImportoTrattenuto = ( %found( FNDCL01L )
090300150731                                and  dclIPT > *zero );
090400150731
090500151102         //IF  $ImportoTrattenuto;
090600150731
090700150731           // -?Reperimento data spedizione?
090800150731           if  Not %open(TITAS30C);
090900150731             open  TITAS30C;
091000150731           endif;
091100150731           clear  keyTITAS30;
091200150731           k_TASaas = dctAAS;
091300150731           k_TASlnp = dctLNP;
091400150731           k_TASnrs = dctNRS;
091500150731           k_TASnsp = dctNSP;
091600150731           chain  %kds( keyTITAS30 : 4 )  TITAS30C;
091700150731           if  Not %found(TITAS30C);
091800150731             clear  tasAAS;
091900150731             clear  tasMGS;
092000150731           endif;
092100150731
092200150731           // -?Verifica se Pratica in Franchigia?
092300150731           // ·?Reperimento Tipo Anomalia Evento?
092400150731           //  ?per verificare SE Evento Fortuito?
092500150731           //  ?(con spedizione successiva al 17/04/2005)?
092600150731           clear  dTAD;
092700150731           If  DCTnev > *zero  and
092800150731               ( tasAAS * 10000 ) + tasMGS >=
092900150731                   c_DataLimite_EventoFortuito_Franchigia;
093000150731             if  Not %open(FNDET01L);
093100150731               open  FNDET01L;
093200150731             endif;
093300150731             chain  ( dctAAE : dctNEV )  FNDET000;
093400150731             if  %found(FNDET01L)   and
093500150731                 getTabella ( c_Tntbe : 'TAD' : DETtad : *blank :
093600150731                              *omit : *omit :
093700150731                              *omit : *omit :
093800150731                              *omit : *omit : *omit : *omit :
093900150731                              *omit : *omit : *omit : *omit :
094000150731                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
094100150731                              = *zero      AND
094200150731                   ds_TNTBE.TBEatb = *blank;
094300150731               dTAD = ds_TNTBE.TBEuni;
094400150731             endif;
094500150731           EndIf;
094600150731
094700150731           //  ?per spedizione precedente il 18/04/2005?
094800150731           //  ?=> si controlla il flag in FNDCT?
094900150731           //  ?per spedizione successiva al 17/04/2005?
095000150731           //  ?=> si controlla il flag in FNDCT & si escludono Eventi Fortuiti?
095100150731           if  ( tasAAS * 10000 ) + tasMGS <
095200150731                   c_DataLimite_EventoFortuito_Franchigia;
095300150731             $Franchigia = ( dDCT01.§DCTpFra = 'F' );
095400150731           else;
095500150731             $Franchigia = ( dDCT01.§DCTpFra =  'F'  and
095600150731                               dTAD.§TADevFo <> 'S' );
095700150731           endif;
095800150731
095900151102         //ENDIF;
096000150731
096100150731         clear  FIDNBEds;
096200150731         iBEmod = '*';
096300150731         iBEaac = dctAAC;
096400150731         iBEfil = dctFIL;
096500150731         iBEnca = dctNCA;
096600150731         iBEaas = dctAAS;
096700150731         iBElnp = dctLNP;
096800150731         iBEnrs = dctNRS;
096900150731         iBEnsp = dctNSP;
097000150731         iBEksc = dctKSC;
097100150731         iBEptr = dctPTR;
097200150731         iBEflo = dctFLO;
097300150731         // ·?SE  nella C.A. c'è l'importo trattenuto  e  non è franchigia?
097400150731         //  ?=>  il P.L. viene intestato a BRT?
097500150731         if  $ImportoTrattenuto  and  Not $Franchigia;
097600150731           iBEpgd = 'V';
097700150731         endif;
097800150731         // ·?Controllo se esiste il luogo 8 o in subordine il luogo 6?
097900150731         iBEpre = '008';
098000150731         iBEtpb = 'S';
098100150731         if  %subst( KNSIF : 7 : 1 ) = 'P';
098200150731           iBEsif = 'P';
098300150731         endif;
098400150731
098500150731         fidnBEr ( FIDNBEds );
098600150731
098700150731         if  OBErag = *blank;
098800150731           OBErag = *all'? ';
098900150731         endif;
099000150731
099100150731       ENDSR;
099200150731
099300150731       //--------------------------------------------------------------
099400150731       //?Preparazione stringa SQL per estrazione dati.                ?
099500150731       //--------------------------------------------------------------
099600150731       BEGSR  sr_PrepSQL;
099700150731
099800150804         wSQL = 'with FNDCF as ( select dcfAAC, dcfFIL, dcfNCA, dcfFCA, +
099900150804                                        max(dcfDFC) as dcfDFC +
100000150804                                   from FNDCF00F +
100100150804                                  where dcfFCA in (' +
100200150804                                 %trim( %editc( c_RicDoc : '3' ) ) + ', ' +
100300150804                                 %trim( %editc( c_RsPLiq : '3' ) ) + ') +
100400150804                               group by dcfAAC, dcfFIL, dcfNCA, dcfFCA +
100500150804                               order by dcfAAC, dcfFIL, dcfNCA, dcfFCA ) +
100600150804
100700150804                 select FNDCT00F.*, FNDCF.DCFdfc +
100800150804
100900150804                   from FNDCT00F inner join FNDCF +
101000150804                     on dctAAC = dcfAAC +
101100150804                    and dctFIL = dcfFIL +
101200150804                    and dctNCA = dcfNCA +
101300150804                    and dctFCA = dcfFCA +
101400150804
101500150803                  where dctFCA in (' +
101600150803                        %trim( %editc( c_RicDoc : '3' ) ) + ', ' +
101700150803                        %trim( %editc( c_RsPLiq : '3' ) ) + ') +
101800150731                    and dctDCH = 0 +
101900150731                    and dctFPR = ''P'' +
102000150803                    and dctGFC = ' + %trim( %editc( c_Sede : '3' ) ) + ' +
102100150804
102200150804               order by dcfDFC, dctAAC, dctFIL, dctNCA +
102300150804
102400150731                    for fetch only';
102500150731
102600150731       ENDSR;
102700150731
102800150731       //--------------------------------------------------------------
102900150731       //?Apertura cursore.                                            ?
103000150731       //--------------------------------------------------------------
103100150731       BEGSR  sr_OpenCursor;
103200150731
103300150731         // -?Dichiarazione cursore?
103400150731         exec sql   prepare S1   from :wSQL;
103500150731         exec sql   declare C1   cursor for S1;
103600150731
103700150731         if  SQLcode < *zero;
103800150731           $EoF = *on;
103900150731           exsr  sr_PrintERR;
104000150731         endif;
104100150731
104200150731         // -?Apertura del cursore?
104300150731         exec sql   open C1;
104400150731
104500150731         if  SQLcode < *zero;
104600150731           $EoF = *on;
104700150731           exsr  sr_PrintERR;
104800150731         endif;
104900150731
105000150731       ENDSR;
105100150731
105200150731       //--------------------------------------------------------------
105300150731       //?Chiusura cursore.                                            ?
105400150731       //--------------------------------------------------------------
105500150731       BEGSR  sr_CloseCursor;
105600150731
105700150731         // -?Chiusura del cursore?
105800150731         exec sql   close C1;
105900150731
106000150731       ENDSR;
106100150731
106200150731       //--------------------------------------------------------------
106300150731       //?Lettura cursore.                                             ?
106400150731       //--------------------------------------------------------------
106500150731       BEGSR  sr_ReadCursor;
106600150731
106700150731         $EoF = *off;
106800150731         clear  ds_FNDCT;
106900150731
107000150804         exec sql   fetch next   from C1   into :ds_FNDCT, :ds_FNDCF;
107100150731
107200150731         select;
107300150731           when  SQLcode = 100;
107400150731             $EoF = *on;
107500150731           when  SQLcode < *zero;
107600150731             $EoF = *on;
107700150731             exsr  sr_PrintERR;
107800150731         endsl;
107900150731
108000150731       ENDSR;
108100150731
108200150731       //--------------------------------------------------------------
108300150731       //?Stampa segnalazione dell'errore rilevato via SQL             ?
108400150731       //--------------------------------------------------------------
108500150731       BEGSR  sr_PrintERR;
108600150731
108700150731         // -?Stampa del Dump?
108800150731         Dump(A);
108900150731
109000150731         // -?Stampa del Job-Log?
109100150731         Qcmd = 'DSPJOBLOG job(*) output(*print)';
109200150731         exsr  sr_ExecCmd;
109300150731
109400150731         // -?Chiusura del programma?
109500150731         exsr  sr_RoutEnd;
109600150731
109700150731       ENDSR;
109800150731
109900150731       //--------------------------------------------------------------
110000150731       //?Esecuzione del comando (già impostato).                      ?
110100150731       //--------------------------------------------------------------
110200150731       BEGSR  sr_ExecCmd;
110300150731
110400150731         clear Qcap0100;
110500150731         Qcabcsdh = *off;
110600150731         Qcapa    = *off;
110700150731         Qcacmdss = *off;
110800150731         Qcaerved = *allX'00';
110900150731
111000150731         clear Qusec;
111100150731         Qusbprv  = %size(Qusec);
111200150731
111300150731         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
111400150731                           %size(Qcap0100) : 'CPOP0100' : *omit :
111500150731                           0 : 0 : Qusec);
111600150731
111700150731         //if  Qusei <> *blank;
111800150731         //  ...;
111900150731         //endif;
112000150731
112100150731       ENDSR;
112200150730
112300150730       //--------------------------------------------------------------
112400150730       //?Operazioni finali.                                           ?
112500150730       //--------------------------------------------------------------
112600150730       BEGSR  sr_RoutEnd;
112700150731
112800150731         // -?Chiusura funzioni precedentemente aperte?
112900150731         cloTabella ();
113000150730
113100150731         // -?Uscita dal *pgm?
113200150730         return;
113300150730
113400150730       ENDSR;
113500150730
113600150730      /end-free
113700150730
113800150730       //--------------------------------------------------------------
113900150730       //?Schiere a tempo di compilazione.                             ?
114000150730       //--------------------------------------------------------------
114100150730
114200150730** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
114300150804Funzione eseguibile solo in ambiente di SEDE: premere "Invio" per uscire       1
114400150807Raggiunto il n° massimo delle C.A. visualizzabili a video                      2
114500150807Opzione errata                                                                 3
114600150807Questa C.A. non esiste più                                                     4
114700150807C.A. non manutenzionabile: è chiusa                                            5
114800150807C.A. non più nella fase corretta per la stampa del progetto di liquidazione    6
