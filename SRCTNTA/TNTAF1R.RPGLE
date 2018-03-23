000100120528       //==============================================================
000200120528       //?TNTAF1R * Estraz. clienti messi in sofferenza dalla sede     ?
000300120528       //==============================================================
000400120608
000500120608       //--------------------------------------------------------------
000600120608       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120608       //--------------------------------------------------------------
000800120608
000900120608     /*PRM dbgview(*source)
001000120608     /*END
001100120528
001200120528       //--------------------------------------------------------------
001300120528       //?Specifiche di controllo.                                     ?
001400120528       //--------------------------------------------------------------
001500120528
001600120528     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700120528     h dftactgrp(*no)
001800120528
001900120528       //--------------------------------------------------------------
002000120528       //?Dichiarazione file.                                          ?
002100120528       //--------------------------------------------------------------
002200120528
002300120528       // -?Tabella "4W"?
002400120528     fTABEL00F  if   e           k disk
002500120528
002600120528       // -?Video parametri di lancio?
002700120528     fTNTAF1D   cf   e             workstn
002800120528     f                                     indds(IndDspF)
002900120528     f                                     infds(InfDspF)
003000120528
003100120528       //--------------------------------------------------------------
003200120528       //?Definizione costanti.                                        ?
003300120528       //--------------------------------------------------------------
003400120528
003500120528       // -?Tasti funzionali a video?
003600120528     d c_F01           c                   const(x'31')
003700120528     d c_F02           c                   const(x'32')
003800120528     d c_F03           c                   const(x'33')
003900120528     d c_F04           c                   const(x'34')
004000120528     d c_F05           c                   const(x'35')
004100120528     d c_F06           c                   const(x'36')
004200120528     d c_F07           c                   const(x'37')
004300120528     d c_F08           c                   const(x'38')
004400120528     d c_F09           c                   const(x'39')
004500120528     d c_F10           c                   const(x'3A')
004600120528     d c_F11           c                   const(x'3B')
004700120528     d c_F12           c                   const(x'3C')
004800120528     d c_F13           c                   const(x'B1')
004900120528     d c_F14           c                   const(x'B2')
005000120528     d c_F15           c                   const(x'B3')
005100120528     d c_F16           c                   const(x'B4')
005200120528     d c_F17           c                   const(x'B5')
005300120528     d c_F18           c                   const(x'B6')
005400120528     d c_F19           c                   const(x'B7')
005500120528     d c_F20           c                   const(x'B8')
005600120528     d c_F21           c                   const(x'B9')
005700120528     d c_F22           c                   const(x'BA')
005800120528     d c_F23           c                   const(x'BB')
005900120528     d c_F24           c                   const(x'BC')
006000120528     d c_Enter         c                   const(x'F1')
006100120528     d c_RollDown      c                   const(x'F4')
006200120528     d c_RollUp        c                   const(x'F5')
006300120528
006400120528       //--------------------------------------------------------------
006500120528       //?Definizione schiere.                                         ?
006600120528       //--------------------------------------------------------------
006700120528
006800120528       // -?Messaggi di errore?
006900120607     d $Msg            s             78    dim( 9) ctdata perrcd(1)
007000120528
007100120528       //--------------------------------------------------------------
007200120528       //?Definizione aree dati.                                       ?
007300120528       //--------------------------------------------------------------
007400120528
007500120528       // -?Dati utente?
007600120528     d §AzUte        e ds                  extname(AZUTE00F)
007700120528     d                                     dtaara
007800120528     d §DatiUte      e ds                  extname(dDatiUte)
007900120528     d                                     dtaara
008000120528
008100120528       //--------------------------------------------------------------
008200120528       //?Definizione strutture dati.                                  ?
008300120528       //--------------------------------------------------------------
008400120528
008500120528       // -?Status ds?
008600120528     d Status         sds
008700120528     d   SDSpgm          *proc
008800120528
008900120528       // -?InfDS?
009000120528     d InfDspF         ds
009100120528     d   dsp_aid             369    369a
009200120528
009300120528       // -?Indicatori su DspF?
009400120528     d IndDspF         ds
009500120528         // -?Emissione messaggio di errore?
009600120528     d  ErrMessage                     n   overlay(IndDspF : 28)
009700120528         // -?Posizionamento cursore & Segnalazione errore?
009800120528     d  PosCurDTI                      n   overlay(IndDspF : 51)
009900120528     d  PosCurDTF                      n   overlay(IndDspF : 52)
010000120528     d  PosCurCON1                     n   overlay(IndDspF : 53)
010100120528     d  PosCurCON2                     n   overlay(IndDspF : 54)
010200120528     d  PosCurCON3                     n   overlay(IndDspF : 55)
010300120528     d  PosCurCON4                     n   overlay(IndDspF : 56)
010400120528     d  PosCurCON5                     n   overlay(IndDspF : 57)
010500120528     d  PosCurCON6                     n   overlay(IndDspF : 58)
010600120528     d  PosCurCON7                     n   overlay(IndDspF : 59)
010700120528     d  PosCurCON8                     n   overlay(IndDspF : 60)
010800120528     d  PosCurCON9                     n   overlay(IndDspF : 61)
010900120528     d  PosCurCONA                     n   overlay(IndDspF : 62)
011000120528         // -?Riemissione videata?
011100120528     d  ErrGenerico                    n   overlay(IndDspF : 99)
011200120528
011300120528       // -?Parametri per controllo/inversione date?
011400120528     d WLBdat          ds                  inz
011500120528     d   G02dat                       8  0 inz
011600120528     d   G02inv                       8  0 inz
011700120528     d   G02err                       1    inz
011800120528     d   G02tgi                       5  0 inz
011900120528
012000120528       // -?Ridefinizione elenco "Causali Stato del Credito" a video?
012100120528     d V1Ccon_ds       ds            20    inz
012200120528     d   V1Ccon1                           inz
012300120528     d   V1Ccon2                           inz
012400120528     d   V1Ccon3                           inz
012500120528     d   V1Ccon4                           inz
012600120528     d   V1Ccon5                           inz
012700120528     d   V1Ccon6                           inz
012800120528     d   V1Ccon7                           inz
012900120528     d   V1Ccon8                           inz
013000120528     d   V1Ccon9                           inz
013100120528     d   V1CconA                           inz
013200120528     d     sk_Con              1     20    dim(10)
013300120528     d V1CconD_ds      ds           250    inz
013400120528     d   V1Dcon1                           inz
013500120528     d   V1Dcon2                           inz
013600120528     d   V1Dcon3                           inz
013700120528     d   V1Dcon4                           inz
013800120528     d   V1Dcon5                           inz
013900120528     d   V1Dcon6                           inz
014000120528     d   V1Dcon7                           inz
014100120528     d   V1Dcon8                           inz
014200120528     d   V1Dcon9                           inz
014300120528     d   V1DconA                           inz
014400120528     d     sk_ConD             1    250    dim(10)
014500120528
014600120528       // -?Parametri ricevuti?
014700120528     d KPJBA         e ds
014800120528
014900120528       // -?Parametri per *pgm batch di estrazione?
015000120528     d TNTAF2ds      e ds                  inz
015100120528
015200120528       // -?Tabella "4W" = Stato del Credito clienti?
015300120528     d ds4W          e ds                  inz
015400120528
015500120528       //--------------------------------------------------------------
015600120528       //?Definizione variabili globali.                               ?
015700120528       //--------------------------------------------------------------
015800120528
015900120528       // -?Flags booleani?
016000120528     d $Fine           s               n   inz
016100120528     d $InzD01         s               n   inz(*on)
016200120528
016300120528       // -?Indici di schiere?
016400120528     d xx              s              3  0 inz
016500120528
016600120528       // -?Variabili per la gestione del video?
016700120528     d $Video          s              2    inz('D1')
016800120528
016900120528       // -?Campi di comodo - data?
017000120528     d wDate           s              8  0 inz
017100120528
017200120528       //--------------------------------------------------------------
017300120528       //?Definizione procedure.                                       ?
017400120528       //--------------------------------------------------------------
017500120528
017600120528       // -?Reperimento dati utente?
017700120528     d TIBS34ds      e ds
017800120528      /copy gaitrasrc/srcProtoPR,TIBS34R
017900120528
018000120528       // -?Richiamo ricerca tabelle (TABEL)?
018100120528     d idElemento...
018200120528     d                 s              8a   inz
018300120528     d tastoFunzionaleUscita...
018400120528     d                 s              1a   inz
018500120528      /copy gaitrasrc/srcProtoPR,TRUL19R
018600120528
018700120528       // -?Richiamo controllo/inversione date?
018800120528      /copy gaitrasrc/srcProtoPR,XSRDA8
018900120528
019000120528       // -?Sottomissione lavoro batch?
019100120528      /copy gaitrasrc/srcProtoPR,BCH10
019200120528
019300120528       // -?Richiamo diretto lavoro batch?
019400120528     d tntaF2r         pr                  extpgm('TNTAF2R')
019500120528     d   kpjba                             likeds(KPJBA)
019600120529
019700120529       // -?Parametri API QCAPCMD (Process Commands)?
019800120529     d Qcmd            s           2048    inz  varying
019900120529      /copy qSysInc/qRpgleSrc,QCAPCMD
020000120529       // -?API QCAPCMD (Process Commands)?
020100120529      /copy gaitrasrc/srcProtoPR,QCAPCMD
020200120529
020300120529       // -?Parametri gestione errori API.?
020400120529      /copy qsysinc/qrpglesrc,QUSEC
020500120528
020600120528       //--------------------------------------------------------------
020700120528       //?Definizione key-list.                                        ?
020800120528       //--------------------------------------------------------------
020900120528
021000120528       // -?File TABEL00F?
021100120528     d k03tabel00    e ds                  extname(TABEL00F : *key)
021200120528     d                                     prefix(k_)   inz
021300120528
021400120528       //--------------------------------------------------------------
021500120528       //?M A I N - L I N E                                            ?
021600120528       //--------------------------------------------------------------
021700120528
021800120528     c     *Entry        plist
021900120528     c                   parm                    KPJBA
022000120528
022100120528      /free
022200120528
022300120528       // -?Operazioni iniziali?
022400120528       exsr  sr_RoutInz;
022500120528
022600120528       // -?Gestione videate?
022700120528       DoW  Not  $Fine;
022800120528         select;
022900120528           when  $Video = 'D1';
023000120528             exsr  sr_GesD01;
023100120528           other;
023200120528             $Fine = *on;
023300120528         endsl;
023400120528       EndDo;
023500120528
023600120528       // -?Operazioni finali?
023700120528       exsr  sr_RoutEnd;
023800120528
023900120528       //--------------------------------------------------------------
024000120528       //?Operazioni iniziali.                                         ?
024100120528       //--------------------------------------------------------------
024200120528       BEGSR  sr_RoutInz;
024300120528
024400120528         *inLR = *on;
024500120528
024600120528         // -?Reperimento dati job?
024700120528         exsr  sr_DatiJob;
024800120528
024900120528         // -?Reperimento data odierna?
025000120528         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025100120528                               : 1 : 8 ) );
025200120528
025300120528         // -?Impostazione dati di testata?
025400120528         V1Tpgm = SDSpgm;
025500120528
025600120528       ENDSR;
025700120528
025800120528       //--------------------------------------------------------------
025900120528       //?Reperimento Dati del job (Utente/Operativi).                 ?
026000120528       //--------------------------------------------------------------
026100120528       BEGSR  sr_DatiJob;
026200120528
026300120528         in(E) §AzUte;
026400120528         if NOT %error;
026500120528           in(E) §DatiUte;
026600120528         endif;
026700120528         if %error or RSut = *blanks;
026800120528           clear TIBS34ds;
026900120528           tibs34r(tibs34ds);
027000120528           in §AzUte;
027100120528           in §DatiUte;
027200120528         endif;
027300120528
027400120528       ENDSR;
027500120528
027600120528       //--------------------------------------------------------------
027700120528       //?Gestione videata D01.                                        ?
027800120528       //--------------------------------------------------------------
027900120528       BEGSR  sr_GesD01;
028000120528
028100120528         // -?Inizializzazione videata?
028200120528         if  $InzD01;
028300120528           exsr  sr_InzD01;
028400120528           $InzD01 = *off;
028500120528         endif;
028600120528
028700120528         // -?Emissione Testata?
028800120528         write  TAF1T01;
028900120528         exfmt  TAF1D01;
029000120528         ErrMessage   = *off;
029100120528         ErrGenerico  = *off;
029200120528         clear V1Dmsg;
029300120528
029400120528         Select;
029500120528
029600120528           // -?Rilevato errore grave: uscita dal *pgm?
029700120528           When  $Fine;
029800120528
029900120528           // -?F3=Fine?
030000120528           When  dsp_aid = c_F03;
030100120528             exsr  sr_F03D01;
030200120529
030300120529           // -?F9=Visualizzazione WrkF WFCSS00F?
030400120529           When  dsp_aid = c_F09;
030500120529             Qcmd = 'DSPDBF file(*libl/WFCSS00F) mbr(*first) output(*)';
030600120529             exsr  sr_ExecCmd;
030700120529             if  Qusei <> *blank;
030800120604               exsr  sr_PrintErr;
030900120529               ErrGenerico = *on;
031000120529               ErrMessage  = *on;
031100120529               V1Dmsg = 'Rilevato errore: consultare la stampa';
031200120529             endif;
031300120528
031400120528           // -?Invio / F6=Conferma?
031500120528           Other;
031600120528             exsr  sr_CtrD01;
031700120528             select;
031800120528               when  ErrGenerico;
031900120528                 leavesr;
032000120528               when  dsp_aid = c_F06;
032100120528                 exsr  sr_F06D01;
032200120528             endsl;
032300120528
032400120528         EndSl;
032500120528
032600120528       ENDSR;
032700120528
032800120528       //--------------------------------------------------------------
032900120528       //?Inizializzazione videata D01.                                ?
033000120528       //--------------------------------------------------------------
033100120528       BEGSR  sr_InzD01;
033200120528
033300120528         clear  TAF1D01;
033400120528
033500120528         k_TBLkut = 1;
033600120528         k_TBLcod = '4W';
033700120528         clear  k_TBLkey;
033800120528
033900120528         setll  %kds( k03tabel00 )  TABEL;
034000120528         reade  %kds( k03tabel00 : 2 )  TABEL;
034100120528
034200120528         DoW  not  %eof( TABEL00F );
034300120528
034400120528           ds4W = TBLuni;
034500120528
034600120528           if  §4Wmbl = 'N';
034700120528
034800120528             xx += 1;
034900120528             if  xx > %elem(sk_Con);
035000120528               ErrGenerico = *on;
035100120528               ErrMessage  = *on;
035200120528               V1Dmsg = $Msg(01);
035300120528               $Fine  = *on;
035400120528               leave;
035500120528             endif;
035600120528             sk_Con(xx)  = %subst( TBLkey : 2 );
035700120528             sk_ConD(xx) = §4Wdes;
035800120528
035900120528           endif;
036000120528
036100120528           reade  %kds( k03tabel00 : 2 )  TABEL;
036200120528
036300120528         EndDo;
036400120607
036500120607         // -?Verifica correttezza del sistema informativo?
036600120607         if  %subst(knsif : 1 : 6) <> 'GAITRA';
036700120607           $Fine       = *on;
036800120607           ErrMessage  = *on;
036900120607           ErrGenerico = *on;
037000120607           V1Dmsg = $Msg(09);
037100120607         endif;
037200120528
037300120528       ENDSR;
037400120528
037500120528       //--------------------------------------------------------------
037600120528       //?Gestione tasto funzionale F3 da videata D01:                 ?
037700120528       //?F3=Fine                                                      ?
037800120528       //--------------------------------------------------------------
037900120528       BEGSR  sr_F03D01;
038000120528
038100120528         // -?Chiusura del *pgm?
038200120528         $Fine = *on;
038300120528
038400120528       ENDSR;
038500120528
038600120528       //--------------------------------------------------------------
038700120528       //?Gestione tasto funzionale F6 da videata D01:                 ?
038800120528       //?F6=Conferma                                                  ?
038900120528       //--------------------------------------------------------------
039000120528       BEGSR  sr_F06D01;
039100120528
039200120528         // -?Impostazione parametri?
039300120529         //clear  TNTAF2ds;           ?(NON adesso!)?
039400120528         //iF2dti = V1Cdt1;           ?(già invertita in questo campo)?
039500120529         //iF2dtf = V1Cdtf;           ?(già invertita in questo campo)?
039600120528         iF2con1 = V1Ccon1;
039700120528         iF2con2 = V1Ccon2;
039800120528         iF2con3 = V1Ccon3;
039900120528         iF2con4 = V1Ccon4;
040000120528         iF2con5 = V1Ccon5;
040100120528         iF2con6 = V1Ccon6;
040200120528         iF2con7 = V1Ccon7;
040300120528         iF2con8 = V1Ccon8;
040400120528         iF2con9 = V1Ccon9;
040500120528         iF2conA = V1CconA;
040600120528
040700120528         kcoaz = 'TAF2';
040800120528         kpjbu = TNTAF2ds;
040900120528
041000120528         if  knmus = *all'1';
041100120528           // -?Richiamo interattivo?
041200120528           tntaF2r ( kpjba );
041300120528         else;
041400120528           // -?Sottomissione batch?
041500120528            bch10 ( kpjba );
041600120528         endif;
041700120528
041800120528         // -?Chiusura del *pgm?
041900120528         $Fine = *on;
042000120528
042100120528       ENDSR;
042200120528
042300120528       //--------------------------------------------------------------
042400120528       //?Controllo videata D01.                                       ?
042500120528       //--------------------------------------------------------------
042600120528       BEGSR  sr_CtrD01;
042700120528
042800120528         %subst(IndDspF : 50) = *off;
042900120528
043000120528         clear  iF2dti;
043100120528         clear  iF2dtf;
043200120528
043300120528         // -?Controllo data Dal?
043400120528         Select;
043500120528
043600120528           When  V1Cdti = *zero;
043700120528             ErrGenerico = *on;
043800120528             ErrMessage  = *on;
043900120528             PosCurDTI   = *on;
044000120528             V1Dmsg = $Msg(02);
044100120528             leavesr;
044200120528
044300120528           When  V1Cdti <> *zero;
044400120528             clear WLBdat;
044500120528             G02dat = V1Cdti;
044600120528             XSRDA8(WLBdat);
044700120528             if  G02err = *on;
044800120528               ErrGenerico = *on;
044900120528               ErrMessage  = *on;
045000120528               PosCurDTI   = *on;
045100120528               V1Dmsg = $Msg(03);
045200120528               leavesr;
045300120528             endif;
045400120528             V1Cdti = G02dat;
045500120528             iF2dti = G02inv;
045600120528
045700120528         EndSl;
045800120528
045900120528         // -?Controllo data Al?
046000120528         Select;
046100120528
046200120528           When  V1Cdtf = *zero;
046300120528             ErrGenerico = *on;
046400120528             ErrMessage  = *on;
046500120528             PosCurDTF   = *on;
046600120528             V1Dmsg = $Msg(02);
046700120528             leavesr;
046800120528
046900120528           When  V1Cdtf <> *zero;
047000120528             clear WLBdat;
047100120528             G02dat = V1Cdtf;
047200120528             XSRDA8(WLBdat);
047300120528             if  G02err = *on;
047400120528               ErrGenerico = *on;
047500120528               ErrMessage  = *on;
047600120528               PosCurDTF   = *on;
047700120528               V1Dmsg = $Msg(03);
047800120528               leavesr;
047900120528             endif;
048000120528             V1Cdtf = G02dat;
048100120528             iF2dtf = G02inv;
048200120528
048300120528         EndSl;
048400120528
048500120528         // -?Data "DAL" congruente con data "AL"?
048600120528         select;
048700120528           when  iF2dti  > IF2dtf;
048800120528             ErrGenerico = *on;
048900120528             ErrMessage  = *on;
049000120528             PosCurDTI   = *on;
049100120528             V1Dmsg = $Msg(04);
049200120528             leavesr;
049300120608           when  iF2dtf  > wDate;
049400120528             ErrGenerico = *on;
049500120528             ErrMessage  = *on;
049600120528             PosCurDTF   = *on;
049700120528             V1Dmsg = $Msg(05);
049800120528             leavesr;
049900120528         endsl;
050000120528
050100120528         // -?Controllo codici "Stato del credito"?
050200120528         If  V1Ccon_ds = *blank;
050300120528           ErrGenerico = *on;
050400120528           ErrMessage  = *on;
050500120528           PosCurCON1  = *on;
050600120528           V1Dmsg = $Msg(06);
050700120528           leavesr;
050800120528         Else;
050900120528           For  xx = 1  To  %elem(sk_Con);
051000120528             exsr  sr_Ctrl4W;
051100120528             if  ErrGenerico;
051200120528               leavesr;
051300120528             endif;
051400120528           EndFor;
051500120528         EndIf;
051600120528
051700120528       ENDSR;
051800120528
051900120528       //--------------------------------------------------------------
052000120528       //?Ricerca / Controllo codici "Stato del Credito"               ?
052100120528       //--------------------------------------------------------------
052200120528       BEGSR  sr_Ctrl4W;
052300120528
052400120528         clear  sk_ConD(xx);
052500120528
052600120528         // -?Campo vuoto (dalla posizione attuale in poi)?
052700120528         if  %subst( V1Ccon_ds :
052800120528                    ( (xx-1) * %len(V1Ccon1) ) + 1 ) = *blank;
052900120529           xx = %elem(sk_Con);
053000120528           leavesr;
053100120528         endif;
053200120528
053300120528         Select;
053400120528
053500120528           // -?Ricerca?
053600120528           When  %scan( '?' : sk_Con(xx) ) > *zero;
053700120528
053800120528             clear  idElemento;
053900120528             clear  tastoFunzionaleUscita;
054000120528
054100120528             TABEL_Ricerca( '4W' :
054200120528                            *blank :
054300120528                            idElemento :
054400120528                            tastoFunzionaleUscita );
054500120528
054600120528             clear  sk_Con(xx);
054700120528             if  tastoFunzionaleUscita = *blank;
054800120528               sk_Con(xx) = %subst( idElemento : 2 );
054900120528             endif;
055000120528             %subst( IndDspF : (52 + xx) : 1 ) = *on;
055100120529             ErrGenerico = *on;
055200120528
055300120528           // -?Controllo?
055400120528           When  sk_Con(xx) <> *blank;
055500120528
055600120528             k_TBLkut = 1;
055700120528             k_TBLcod = '4W';
055800120528             k_TBLkey = ' ' + sk_Con(xx);
055900120528             chain  %kds( k03tabel00 )  TABEL;
056000120528             if  Not %found(TABEL00F)  or  TBLflg <> *blank;
056100120528               ErrGenerico = *on;
056200120528               ErrMessage  = *on;
056300120528               %subst( IndDspF : (52 + xx) : 1 ) = *on;
056400120528               V1Dmsg = $Msg(07);
056500120528               leavesr;
056600120528             else;
056700120528               ds4W = TBLuni;
056800120528               sk_ConD(xx) = §4Wdes;
056900120528               if  §4Wmbl <> 'N';
057000120528                 ErrGenerico = *on;
057100120528                 ErrMessage  = *on;
057200120528                 %subst( IndDspF : (52 + xx) : 1 ) = *on;
057300120528                 V1Dmsg = $Msg(08);
057400120528                 leavesr;
057500120528               endif;
057600120528             endif;
057700120528
057800120528         EndSl;
057900120528
058000120528       ENDSR;
058100120529
058200120529       //--------------------------------------------------------------
058300120529       //?Esecuzione del comando (già impostato).                      ?
058400120529       //--------------------------------------------------------------
058500120529       BEGSR  sr_ExecCmd;
058600120529
058700120529         clear Qcap0100;
058800120529         Qcabcsdh = *off;
058900120529         Qcapa    = *off;
059000120529         Qcacmdss = *off;
059100120529         Qcaerved = *allX'00';
059200120529
059300120529         clear Qusec;
059400120529         Qusbprv  = %size(Qusec);
059500120529
059600120529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
059700120529                           %size(Qcap0100) : 'CPOP0100' : *omit :
059800120529                           0 : 0 : Qusec);
059900120529
060000120529         //if  Qusei <> *blank;
060100120529         //  ...
060200120529         //endif;
060300120529
060400120529       ENDSR;
060500120529
060600120529       //--------------------------------------------------------------
060700120529       //?Stampa segnalazioni dell'errore rilevato.                    ?
060800120529       //--------------------------------------------------------------
060900120529       BEGSR  sr_PrintErr;
061000120529
061100120529         // -?Stampa Dump?
061200120529         Dump(A);
061300120529
061400120529         // -?Stampa Job-Log?
061500120529         Qcmd = 'DSPJOBLOG job(*) output(*print)';
061600120529         clear Qcap0100;
061700120529         Qcabcsdh = *off;
061800120529         Qcapa    = *off;
061900120529         Qcacmdss = *off;
062000120529         Qcaerved = *allX'00';
062100120529         clear Qusec;
062200120529         Qusbprv  = %size(Qusec);
062300120529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
062400120529                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
062500120529                           0 : 0 : Qusec);
062600120529
062700120529       ENDSR;
062800120528
062900120528       //--------------------------------------------------------------
063000120528       //?Operazioni finali.                                           ?
063100120528       //--------------------------------------------------------------
063200120528       BEGSR  sr_RoutEnd;
063300120528
063400120528         return;
063500120528
063600120528       ENDSR;
063700120528
063800120528      /end-free
063900120528
064000120528       //--------------------------------------------------------------
064100120528       //?Schiere a tempo di compilazione.                             ?
064200120528       //--------------------------------------------------------------
064300120528
064400120528** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
064500120528Reperiti troppi "Stati del credito" caricati in tab. "4W"                      1
064600120528Data obbligatoria                                                              2
064700120528Data errata                                                                    3
064800120528Data "DAL" successiva alla data "AL"                                           4
064900120608Data finale deve essere minore o uguale ad oggi                                5
065000120528Inserire almeno un codice "Stato del credito"                                  6
065100120528Stato del credito errato                                                       7
065200120528Stato del credito NON modificabile solo in sede                                8
065300120607Programma eseguibile SOLO nel sistema informativo di SEDE. "Invio" per uscire. 9
