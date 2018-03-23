000100120528       //==============================================================
000200120528       //?TNTAF1R * Estraz. clienti messi in sofferenza dalla sede     ?
000300120528       //==============================================================
000400120528
000500120528       //--------------------------------------------------------------
000600120528       //?Specifiche di controllo.                                     ?
000700120528       //--------------------------------------------------------------
000800120528
000900120528     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000120528     h dftactgrp(*no)
001100120528
001200120528       //--------------------------------------------------------------
001300120528       //?Dichiarazione file.                                          ?
001400120528       //--------------------------------------------------------------
001500120528
001600120528       // -?Tabella "4W"?
001700120528     fTABEL00F  if   e           k disk
001800120528
001900120528       // -?Video parametri di lancio?
002000120528     fTNTAF1D   cf   e             workstn
002100120528     f                                     indds(IndDspF)
002200120528     f                                     infds(InfDspF)
002300120528
002400120528       //--------------------------------------------------------------
002500120528       //?Definizione costanti.                                        ?
002600120528       //--------------------------------------------------------------
002700120528
002800120528       // -?Tasti funzionali a video?
002900120528     d c_F01           c                   const(x'31')
003000120528     d c_F02           c                   const(x'32')
003100120528     d c_F03           c                   const(x'33')
003200120528     d c_F04           c                   const(x'34')
003300120528     d c_F05           c                   const(x'35')
003400120528     d c_F06           c                   const(x'36')
003500120528     d c_F07           c                   const(x'37')
003600120528     d c_F08           c                   const(x'38')
003700120528     d c_F09           c                   const(x'39')
003800120528     d c_F10           c                   const(x'3A')
003900120528     d c_F11           c                   const(x'3B')
004000120528     d c_F12           c                   const(x'3C')
004100120528     d c_F13           c                   const(x'B1')
004200120528     d c_F14           c                   const(x'B2')
004300120528     d c_F15           c                   const(x'B3')
004400120528     d c_F16           c                   const(x'B4')
004500120528     d c_F17           c                   const(x'B5')
004600120528     d c_F18           c                   const(x'B6')
004700120528     d c_F19           c                   const(x'B7')
004800120528     d c_F20           c                   const(x'B8')
004900120528     d c_F21           c                   const(x'B9')
005000120528     d c_F22           c                   const(x'BA')
005100120528     d c_F23           c                   const(x'BB')
005200120528     d c_F24           c                   const(x'BC')
005300120528     d c_Enter         c                   const(x'F1')
005400120528     d c_RollDown      c                   const(x'F4')
005500120528     d c_RollUp        c                   const(x'F5')
005600120528
005700120528       //--------------------------------------------------------------
005800120528       //?Definizione schiere.                                         ?
005900120528       //--------------------------------------------------------------
006000120528
006100120528       // -?Messaggi di errore?
006200120607     d $Msg            s             78    dim( 9) ctdata perrcd(1)
006300120528
006400120528       //--------------------------------------------------------------
006500120528       //?Definizione aree dati.                                       ?
006600120528       //--------------------------------------------------------------
006700120528
006800120528       // -?Dati utente?
006900120528     d §AzUte        e ds                  extname(AZUTE00F)
007000120528     d                                     dtaara
007100120528     d §DatiUte      e ds                  extname(dDatiUte)
007200120528     d                                     dtaara
007300120528
007400120528       //--------------------------------------------------------------
007500120528       //?Definizione strutture dati.                                  ?
007600120528       //--------------------------------------------------------------
007700120528
007800120528       // -?Status ds?
007900120528     d Status         sds
008000120528     d   SDSpgm          *proc
008100120528
008200120528       // -?InfDS?
008300120528     d InfDspF         ds
008400120528     d   dsp_aid             369    369a
008500120528
008600120528       // -?Indicatori su DspF?
008700120528     d IndDspF         ds
008800120528         // -?Emissione messaggio di errore?
008900120528     d  ErrMessage                     n   overlay(IndDspF : 28)
009000120528         // -?Posizionamento cursore & Segnalazione errore?
009100120528     d  PosCurDTI                      n   overlay(IndDspF : 51)
009200120528     d  PosCurDTF                      n   overlay(IndDspF : 52)
009300120528     d  PosCurCON1                     n   overlay(IndDspF : 53)
009400120528     d  PosCurCON2                     n   overlay(IndDspF : 54)
009500120528     d  PosCurCON3                     n   overlay(IndDspF : 55)
009600120528     d  PosCurCON4                     n   overlay(IndDspF : 56)
009700120528     d  PosCurCON5                     n   overlay(IndDspF : 57)
009800120528     d  PosCurCON6                     n   overlay(IndDspF : 58)
009900120528     d  PosCurCON7                     n   overlay(IndDspF : 59)
010000120528     d  PosCurCON8                     n   overlay(IndDspF : 60)
010100120528     d  PosCurCON9                     n   overlay(IndDspF : 61)
010200120528     d  PosCurCONA                     n   overlay(IndDspF : 62)
010300120528         // -?Riemissione videata?
010400120528     d  ErrGenerico                    n   overlay(IndDspF : 99)
010500120528
010600120528       // -?Parametri per controllo/inversione date?
010700120528     d WLBdat          ds                  inz
010800120528     d   G02dat                       8  0 inz
010900120528     d   G02inv                       8  0 inz
011000120528     d   G02err                       1    inz
011100120528     d   G02tgi                       5  0 inz
011200120528
011300120528       // -?Ridefinizione elenco "Causali Stato del Credito" a video?
011400120528     d V1Ccon_ds       ds            20    inz
011500120528     d   V1Ccon1                           inz
011600120528     d   V1Ccon2                           inz
011700120528     d   V1Ccon3                           inz
011800120528     d   V1Ccon4                           inz
011900120528     d   V1Ccon5                           inz
012000120528     d   V1Ccon6                           inz
012100120528     d   V1Ccon7                           inz
012200120528     d   V1Ccon8                           inz
012300120528     d   V1Ccon9                           inz
012400120528     d   V1CconA                           inz
012500120528     d     sk_Con              1     20    dim(10)
012600120528     d V1CconD_ds      ds           250    inz
012700120528     d   V1Dcon1                           inz
012800120528     d   V1Dcon2                           inz
012900120528     d   V1Dcon3                           inz
013000120528     d   V1Dcon4                           inz
013100120528     d   V1Dcon5                           inz
013200120528     d   V1Dcon6                           inz
013300120528     d   V1Dcon7                           inz
013400120528     d   V1Dcon8                           inz
013500120528     d   V1Dcon9                           inz
013600120528     d   V1DconA                           inz
013700120528     d     sk_ConD             1    250    dim(10)
013800120528
013900120528       // -?Parametri ricevuti?
014000120528     d KPJBA         e ds
014100120528
014200120528       // -?Parametri per *pgm batch di estrazione?
014300120528     d TNTAF2ds      e ds                  inz
014400120528
014500120528       // -?Tabella "4W" = Stato del Credito clienti?
014600120528     d ds4W          e ds                  inz
014700120528
014800120528       //--------------------------------------------------------------
014900120528       //?Definizione variabili globali.                               ?
015000120528       //--------------------------------------------------------------
015100120528
015200120528       // -?Flags booleani?
015300120528     d $Fine           s               n   inz
015400120528     d $InzD01         s               n   inz(*on)
015500120528
015600120528       // -?Indici di schiere?
015700120528     d xx              s              3  0 inz
015800120528
015900120528       // -?Variabili per la gestione del video?
016000120528     d $Video          s              2    inz('D1')
016100120528
016200120528       // -?Campi di comodo - data?
016300120528     d wDate           s              8  0 inz
016400120528
016500120528       //--------------------------------------------------------------
016600120528       //?Definizione procedure.                                       ?
016700120528       //--------------------------------------------------------------
016800120528
016900120528       // -?Reperimento dati utente?
017000120528     d TIBS34ds      e ds
017100120528      /copy gaitrasrc/srcProtoPR,TIBS34R
017200120528
017300120528       // -?Richiamo ricerca tabelle (TABEL)?
017400120528     d idElemento...
017500120528     d                 s              8a   inz
017600120528     d tastoFunzionaleUscita...
017700120528     d                 s              1a   inz
017800120528      /copy gaitrasrc/srcProtoPR,TRUL19R
017900120528
018000120528       // -?Richiamo controllo/inversione date?
018100120528      /copy gaitrasrc/srcProtoPR,XSRDA8
018200120528
018300120528       // -?Sottomissione lavoro batch?
018400120528      /copy gaitrasrc/srcProtoPR,BCH10
018500120528
018600120528       // -?Richiamo diretto lavoro batch?
018700120528     d tntaF2r         pr                  extpgm('TNTAF2R')
018800120528     d   kpjba                             likeds(KPJBA)
018900120529
019000120529       // -?Parametri API QCAPCMD (Process Commands)?
019100120529     d Qcmd            s           2048    inz  varying
019200120529      /copy qSysInc/qRpgleSrc,QCAPCMD
019300120529       // -?API QCAPCMD (Process Commands)?
019400120529      /copy gaitrasrc/srcProtoPR,QCAPCMD
019500120529
019600120529       // -?Parametri gestione errori API.?
019700120529      /copy qsysinc/qrpglesrc,QUSEC
019800120528
019900120528       //--------------------------------------------------------------
020000120528       //?Definizione key-list.                                        ?
020100120528       //--------------------------------------------------------------
020200120528
020300120528       // -?File TABEL00F?
020400120528     d k03tabel00    e ds                  extname(TABEL00F : *key)
020500120528     d                                     prefix(k_)   inz
020600120528
020700120528       //--------------------------------------------------------------
020800120528       //?M A I N - L I N E                                            ?
020900120528       //--------------------------------------------------------------
021000120528
021100120528     c     *Entry        plist
021200120528     c                   parm                    KPJBA
021300120528
021400120528      /free
021500120528
021600120528       // -?Operazioni iniziali?
021700120528       exsr  sr_RoutInz;
021800120528
021900120528       // -?Gestione videate?
022000120528       DoW  Not  $Fine;
022100120528         select;
022200120528           when  $Video = 'D1';
022300120528             exsr  sr_GesD01;
022400120528           other;
022500120528             $Fine = *on;
022600120528         endsl;
022700120528       EndDo;
022800120528
022900120528       // -?Operazioni finali?
023000120528       exsr  sr_RoutEnd;
023100120528
023200120528       //--------------------------------------------------------------
023300120528       //?Operazioni iniziali.                                         ?
023400120528       //--------------------------------------------------------------
023500120528       BEGSR  sr_RoutInz;
023600120528
023700120528         *inLR = *on;
023800120528
023900120528         // -?Reperimento dati job?
024000120528         exsr  sr_DatiJob;
024100120528
024200120528         // -?Reperimento data odierna?
024300120528         wDate = %int( %subst( %char( %dec( %timestamp() ) )
024400120528                               : 1 : 8 ) );
024500120528
024600120528         // -?Impostazione dati di testata?
024700120528         V1Tpgm = SDSpgm;
024800120528
024900120528       ENDSR;
025000120528
025100120528       //--------------------------------------------------------------
025200120528       //?Reperimento Dati del job (Utente/Operativi).                 ?
025300120528       //--------------------------------------------------------------
025400120528       BEGSR  sr_DatiJob;
025500120528
025600120528         in(E) §AzUte;
025700120528         if NOT %error;
025800120528           in(E) §DatiUte;
025900120528         endif;
026000120528         if %error or RSut = *blanks;
026100120528           clear TIBS34ds;
026200120528           tibs34r(tibs34ds);
026300120528           in §AzUte;
026400120528           in §DatiUte;
026500120528         endif;
026600120528
026700120528       ENDSR;
026800120528
026900120528       //--------------------------------------------------------------
027000120528       //?Gestione videata D01.                                        ?
027100120528       //--------------------------------------------------------------
027200120528       BEGSR  sr_GesD01;
027300120528
027400120528         // -?Inizializzazione videata?
027500120528         if  $InzD01;
027600120528           exsr  sr_InzD01;
027700120528           $InzD01 = *off;
027800120528         endif;
027900120528
028000120528         // -?Emissione Testata?
028100120528         write  TAF1T01;
028200120528         exfmt  TAF1D01;
028300120528         ErrMessage   = *off;
028400120528         ErrGenerico  = *off;
028500120528         clear V1Dmsg;
028600120528
028700120528         Select;
028800120528
028900120528           // -?Rilevato errore grave: uscita dal *pgm?
029000120528           When  $Fine;
029100120528
029200120528           // -?F3=Fine?
029300120528           When  dsp_aid = c_F03;
029400120528             exsr  sr_F03D01;
029500120529
029600120529           // -?F9=Visualizzazione WrkF WFCSS00F?
029700120529           When  dsp_aid = c_F09;
029800120529             Qcmd = 'DSPDBF file(*libl/WFCSS00F) mbr(*first) output(*)';
029900120529             exsr  sr_ExecCmd;
030000120529             if  Qusei <> *blank;
030100120604               exsr  sr_PrintErr;
030200120529               ErrGenerico = *on;
030300120529               ErrMessage  = *on;
030400120529               V1Dmsg = 'Rilevato errore: consultare la stampa';
030500120529             endif;
030600120528
030700120528           // -?Invio / F6=Conferma?
030800120528           Other;
030900120528             exsr  sr_CtrD01;
031000120528             select;
031100120528               when  ErrGenerico;
031200120528                 leavesr;
031300120528               when  dsp_aid = c_F06;
031400120528                 exsr  sr_F06D01;
031500120528             endsl;
031600120528
031700120528         EndSl;
031800120528
031900120528       ENDSR;
032000120528
032100120528       //--------------------------------------------------------------
032200120528       //?Inizializzazione videata D01.                                ?
032300120528       //--------------------------------------------------------------
032400120528       BEGSR  sr_InzD01;
032500120528
032600120528         clear  TAF1D01;
032700120528
032800120528         k_TBLkut = 1;
032900120528         k_TBLcod = '4W';
033000120528         clear  k_TBLkey;
033100120528
033200120528         setll  %kds( k03tabel00 )  TABEL;
033300120528         reade  %kds( k03tabel00 : 2 )  TABEL;
033400120528
033500120528         DoW  not  %eof( TABEL00F );
033600120528
033700120528           ds4W = TBLuni;
033800120528
033900120528           if  §4Wmbl = 'N';
034000120528
034100120528             xx += 1;
034200120528             if  xx > %elem(sk_Con);
034300120528               ErrGenerico = *on;
034400120528               ErrMessage  = *on;
034500120528               V1Dmsg = $Msg(01);
034600120528               $Fine  = *on;
034700120528               leave;
034800120528             endif;
034900120528             sk_Con(xx)  = %subst( TBLkey : 2 );
035000120528             sk_ConD(xx) = §4Wdes;
035100120528
035200120528           endif;
035300120528
035400120528           reade  %kds( k03tabel00 : 2 )  TABEL;
035500120528
035600120528         EndDo;
035700120607
035800120607         // -?Verifica correttezza del sistema informativo?
035900120607         if  %subst(knsif : 1 : 6) <> 'GAITRA';
036000120607           $Fine       = *on;
036100120607           ErrMessage  = *on;
036200120607           ErrGenerico = *on;
036300120607           V1Dmsg = $Msg(09);
036400120607         endif;
036500120528
036600120528       ENDSR;
036700120528
036800120528       //--------------------------------------------------------------
036900120528       //?Gestione tasto funzionale F3 da videata D01:                 ?
037000120528       //?F3=Fine                                                      ?
037100120528       //--------------------------------------------------------------
037200120528       BEGSR  sr_F03D01;
037300120528
037400120528         // -?Chiusura del *pgm?
037500120528         $Fine = *on;
037600120528
037700120528       ENDSR;
037800120528
037900120528       //--------------------------------------------------------------
038000120528       //?Gestione tasto funzionale F6 da videata D01:                 ?
038100120528       //?F6=Conferma                                                  ?
038200120528       //--------------------------------------------------------------
038300120528       BEGSR  sr_F06D01;
038400120528
038500120528         // -?Impostazione parametri?
038600120529         //clear  TNTAF2ds;           ?(NON adesso!)?
038700120528         //iF2dti = V1Cdt1;           ?(già invertita in questo campo)?
038800120529         //iF2dtf = V1Cdtf;           ?(già invertita in questo campo)?
038900120528         iF2con1 = V1Ccon1;
039000120528         iF2con2 = V1Ccon2;
039100120528         iF2con3 = V1Ccon3;
039200120528         iF2con4 = V1Ccon4;
039300120528         iF2con5 = V1Ccon5;
039400120528         iF2con6 = V1Ccon6;
039500120528         iF2con7 = V1Ccon7;
039600120528         iF2con8 = V1Ccon8;
039700120528         iF2con9 = V1Ccon9;
039800120528         iF2conA = V1CconA;
039900120528
040000120528         kcoaz = 'TAF2';
040100120528         kpjbu = TNTAF2ds;
040200120528
040300120528         if  knmus = *all'1';
040400120528           // -?Richiamo interattivo?
040500120528           tntaF2r ( kpjba );
040600120528         else;
040700120528           // -?Sottomissione batch?
040800120528            bch10 ( kpjba );
040900120528         endif;
041000120528
041100120528         // -?Chiusura del *pgm?
041200120528         $Fine = *on;
041300120528
041400120528       ENDSR;
041500120528
041600120528       //--------------------------------------------------------------
041700120528       //?Controllo videata D01.                                       ?
041800120528       //--------------------------------------------------------------
041900120528       BEGSR  sr_CtrD01;
042000120528
042100120528         %subst(IndDspF : 50) = *off;
042200120528
042300120528         clear  iF2dti;
042400120528         clear  iF2dtf;
042500120528
042600120528         // -?Controllo data Dal?
042700120528         Select;
042800120528
042900120528           When  V1Cdti = *zero;
043000120528             ErrGenerico = *on;
043100120528             ErrMessage  = *on;
043200120528             PosCurDTI   = *on;
043300120528             V1Dmsg = $Msg(02);
043400120528             leavesr;
043500120528
043600120528           When  V1Cdti <> *zero;
043700120528             clear WLBdat;
043800120528             G02dat = V1Cdti;
043900120528             XSRDA8(WLBdat);
044000120528             if  G02err = *on;
044100120528               ErrGenerico = *on;
044200120528               ErrMessage  = *on;
044300120528               PosCurDTI   = *on;
044400120528               V1Dmsg = $Msg(03);
044500120528               leavesr;
044600120528             endif;
044700120528             V1Cdti = G02dat;
044800120528             iF2dti = G02inv;
044900120528
045000120528         EndSl;
045100120528
045200120528         // -?Controllo data Al?
045300120528         Select;
045400120528
045500120528           When  V1Cdtf = *zero;
045600120528             ErrGenerico = *on;
045700120528             ErrMessage  = *on;
045800120528             PosCurDTF   = *on;
045900120528             V1Dmsg = $Msg(02);
046000120528             leavesr;
046100120528
046200120528           When  V1Cdtf <> *zero;
046300120528             clear WLBdat;
046400120528             G02dat = V1Cdtf;
046500120528             XSRDA8(WLBdat);
046600120528             if  G02err = *on;
046700120528               ErrGenerico = *on;
046800120528               ErrMessage  = *on;
046900120528               PosCurDTF   = *on;
047000120528               V1Dmsg = $Msg(03);
047100120528               leavesr;
047200120528             endif;
047300120528             V1Cdtf = G02dat;
047400120528             iF2dtf = G02inv;
047500120528
047600120528         EndSl;
047700120528
047800120528         // -?Data "DAL" congruente con data "AL"?
047900120528         select;
048000120528           when  iF2dti  > IF2dtf;
048100120528             ErrGenerico = *on;
048200120528             ErrMessage  = *on;
048300120528             PosCurDTI   = *on;
048400120528             V1Dmsg = $Msg(04);
048500120528             leavesr;
048600120528           when  iF2dtf >= wDate;
048700120528             ErrGenerico = *on;
048800120528             ErrMessage  = *on;
048900120528             PosCurDTF   = *on;
049000120528             V1Dmsg = $Msg(05);
049100120528             leavesr;
049200120528         endsl;
049300120528
049400120528         // -?Controllo codici "Stato del credito"?
049500120528         If  V1Ccon_ds = *blank;
049600120528           ErrGenerico = *on;
049700120528           ErrMessage  = *on;
049800120528           PosCurCON1  = *on;
049900120528           V1Dmsg = $Msg(06);
050000120528           leavesr;
050100120528         Else;
050200120528           For  xx = 1  To  %elem(sk_Con);
050300120528             exsr  sr_Ctrl4W;
050400120528             if  ErrGenerico;
050500120528               leavesr;
050600120528             endif;
050700120528           EndFor;
050800120528         EndIf;
050900120528
051000120528       ENDSR;
051100120528
051200120528       //--------------------------------------------------------------
051300120528       //?Ricerca / Controllo codici "Stato del Credito"               ?
051400120528       //--------------------------------------------------------------
051500120528       BEGSR  sr_Ctrl4W;
051600120528
051700120528         clear  sk_ConD(xx);
051800120528
051900120528         // -?Campo vuoto (dalla posizione attuale in poi)?
052000120528         if  %subst( V1Ccon_ds :
052100120528                    ( (xx-1) * %len(V1Ccon1) ) + 1 ) = *blank;
052200120529           xx = %elem(sk_Con);
052300120528           leavesr;
052400120528         endif;
052500120528
052600120528         Select;
052700120528
052800120528           // -?Ricerca?
052900120528           When  %scan( '?' : sk_Con(xx) ) > *zero;
053000120528
053100120528             clear  idElemento;
053200120528             clear  tastoFunzionaleUscita;
053300120528
053400120528             TABEL_Ricerca( '4W' :
053500120528                            *blank :
053600120528                            idElemento :
053700120528                            tastoFunzionaleUscita );
053800120528
053900120528             clear  sk_Con(xx);
054000120528             if  tastoFunzionaleUscita = *blank;
054100120528               sk_Con(xx) = %subst( idElemento : 2 );
054200120528             endif;
054300120528             %subst( IndDspF : (52 + xx) : 1 ) = *on;
054400120529             ErrGenerico = *on;
054500120528
054600120528           // -?Controllo?
054700120528           When  sk_Con(xx) <> *blank;
054800120528
054900120528             k_TBLkut = 1;
055000120528             k_TBLcod = '4W';
055100120528             k_TBLkey = ' ' + sk_Con(xx);
055200120528             chain  %kds( k03tabel00 )  TABEL;
055300120528             if  Not %found(TABEL00F)  or  TBLflg <> *blank;
055400120528               ErrGenerico = *on;
055500120528               ErrMessage  = *on;
055600120528               %subst( IndDspF : (52 + xx) : 1 ) = *on;
055700120528               V1Dmsg = $Msg(07);
055800120528               leavesr;
055900120528             else;
056000120528               ds4W = TBLuni;
056100120528               sk_ConD(xx) = §4Wdes;
056200120528               if  §4Wmbl <> 'N';
056300120528                 ErrGenerico = *on;
056400120528                 ErrMessage  = *on;
056500120528                 %subst( IndDspF : (52 + xx) : 1 ) = *on;
056600120528                 V1Dmsg = $Msg(08);
056700120528                 leavesr;
056800120528               endif;
056900120528             endif;
057000120528
057100120528         EndSl;
057200120528
057300120528       ENDSR;
057400120529
057500120529       //--------------------------------------------------------------
057600120529       //?Esecuzione del comando (già impostato).                      ?
057700120529       //--------------------------------------------------------------
057800120529       BEGSR  sr_ExecCmd;
057900120529
058000120529         clear Qcap0100;
058100120529         Qcabcsdh = *off;
058200120529         Qcapa    = *off;
058300120529         Qcacmdss = *off;
058400120529         Qcaerved = *allX'00';
058500120529
058600120529         clear Qusec;
058700120529         Qusbprv  = %size(Qusec);
058800120529
058900120529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
059000120529                           %size(Qcap0100) : 'CPOP0100' : *omit :
059100120529                           0 : 0 : Qusec);
059200120529
059300120529         //if  Qusei <> *blank;
059400120529         //  ...
059500120529         //endif;
059600120529
059700120529       ENDSR;
059800120529
059900120529       //--------------------------------------------------------------
060000120529       //?Stampa segnalazioni dell'errore rilevato.                    ?
060100120529       //--------------------------------------------------------------
060200120529       BEGSR  sr_PrintErr;
060300120529
060400120529         // -?Stampa Dump?
060500120529         Dump(A);
060600120529
060700120529         // -?Stampa Job-Log?
060800120529         Qcmd = 'DSPJOBLOG job(*) output(*print)';
060900120529         clear Qcap0100;
061000120529         Qcabcsdh = *off;
061100120529         Qcapa    = *off;
061200120529         Qcacmdss = *off;
061300120529         Qcaerved = *allX'00';
061400120529         clear Qusec;
061500120529         Qusbprv  = %size(Qusec);
061600120529         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
061700120529                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
061800120529                           0 : 0 : Qusec);
061900120529
062000120529       ENDSR;
062100120528
062200120528       //--------------------------------------------------------------
062300120528       //?Operazioni finali.                                           ?
062400120528       //--------------------------------------------------------------
062500120528       BEGSR  sr_RoutEnd;
062600120528
062700120528         return;
062800120528
062900120528       ENDSR;
063000120528
063100120528      /end-free
063200120528
063300120528       //--------------------------------------------------------------
063400120528       //?Schiere a tempo di compilazione.                             ?
063500120528       //--------------------------------------------------------------
063600120528
063700120528** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
063800120528Reperiti troppi "Stati del credito" caricati in tab. "4W"                      1
063900120528Data obbligatoria                                                              2
064000120528Data errata                                                                    3
064100120528Data "DAL" successiva alla data "AL"                                           4
064200120528Data finale deve essere minore di oggi                                         5
064300120528Inserire almeno un codice "Stato del credito"                                  6
064400120528Stato del credito errato                                                       7
064500120528Stato del credito NON modificabile solo in sede                                8
064600120607Programma eseguibile SOLO nel sistema informativo di SEDE. "Invio" per uscire. 9
