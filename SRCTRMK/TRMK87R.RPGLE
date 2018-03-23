000100100517       //==============================================================
000200100517       //?TRMK87R * Statistica commerciale: filtro di lancio           ?
000300100517       //==============================================================
000400130829
000500130829       //--------------------------------------------------------------
000600130829       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130829       //--------------------------------------------------------------
000800130829
000900130829     /*END
001000080206
001100100517       //--------------------------------------------------------------
001200100517       //?Specifiche di controllo.                                     ?
001300100517       //--------------------------------------------------------------
001400100517
001500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100517     h dftactgrp(*no)
001700080206
001800100517       //--------------------------------------------------------------
001900100517       //?Dichiarazione file.                                          ?
002000100517       //--------------------------------------------------------------
002100100326
002200100517       // -?Tabelle?
002300090720     fTABEL00F  if   e           k disk
002400130829
002500130829      // -?Anagrafica Commerciali?
002600130829     fAZCMM01L  if   e           k disk
002700090720
002800100517       // -?Video parametri di lancio?
002900100423     fTRMK87D   cf   e             workstn
003000080208     f                                     indds(IndDspF)
003100080206     f                                     infds(InfDspF)
003200080206
003300100517       //--------------------------------------------------------------
003400100517       //?Definizione costanti.                                        ?
003500100517       //--------------------------------------------------------------
003600080207
003700100517       // -?Tasti funzionali a video?
003800080207     d c_F01           c                   const(x'31')
003900080207     d c_F02           c                   const(x'32')
004000080207     d c_F03           c                   const(x'33')
004100090406     d c_F04           c                   const(x'34')
004200080207     d c_F05           c                   const(x'35')
004300080207     d c_F06           c                   const(x'36')
004400080207     d c_F07           c                   const(x'37')
004500080207     d c_F08           c                   const(x'38')
004600080207     d c_F09           c                   const(x'39')
004700080207     d c_F10           c                   const(x'3A')
004800090303     d c_F11           c                   const(x'3B')
004900080207     d c_F12           c                   const(x'3C')
005000080207     d c_F13           c                   const(x'B1')
005100080207     d c_F14           c                   const(x'B2')
005200080207     d c_F15           c                   const(x'B3')
005300080207     d c_F16           c                   const(x'B4')
005400080207     d c_F17           c                   const(x'B5')
005500080207     d c_F18           c                   const(x'B6')
005600080207     d c_F19           c                   const(x'B7')
005700080207     d c_F20           c                   const(x'B8')
005800080207     d c_F21           c                   const(x'B9')
005900080207     d c_F22           c                   const(x'BA')
006000080207     d c_F23           c                   const(x'BB')
006100080207     d c_F24           c                   const(x'BC')
006200080207     d c_Enter         c                   const(x'F1')
006300080207     d c_RollDown      c                   const(x'F4')
006400080207     d c_RollUp        c                   const(x'F5')
006500100211
006600100517       // -?Costante per controllo "caratteri solo numerici"?
006700100517     d c_Digits        c                   const('0123456789')
006800080206
006900100517       //--------------------------------------------------------------
007000100517       //?Definizione schiere.                                         ?
007100100517       //--------------------------------------------------------------
007200080206
007300100517       // -?Messaggi di errore?
007400110120     d $Msg            s             78    dim(24) ctdata perrcd(1)
007500080206
007600100517       //--------------------------------------------------------------
007700100517       //?Definizione aree dati.                                       ?
007800100517       //--------------------------------------------------------------
007900080206
008000100517       // -?Dati utente?
008100080206     d §AzUte        e ds                  extname(AZUTE00F)
008200080206     d                                     dtaara
008300080206     d §DatiUte      e ds                  extname(dDatiUte)
008400080206     d                                     dtaara
008500080206
008600100517       //--------------------------------------------------------------
008700100517       //?Definizione strutture dati.                                  ?
008800100517       //--------------------------------------------------------------
008900080206
009000100517       // -?Status ds?
009100080206     d Psds           sds
009200080206     d   SDSpgm          *proc
009300080206
009400100517       // -?InfDS?
009500080206     d InfDspF         ds
009600080207     d  dsp_aid              369    369a
009700080206
009800100517       // -?Indicatori su DspF?
009900080208     d IndDspF         ds
010000100517         // -?Emissione messaggio di errore?
010100100517     d  ErrMessage                     n   overlay(IndDspF : 28)
010200110120         // -?Visualizzazione campi a video?
010300110120     d  DspStpCar                     1n   overlay(IndDspF : 40)
010400100517         // -?Posizionamento cursore & Segnalazione errore?
010500100728     d  PosCurEla                      n   overlay(IndDspF : 65)
010600100517     d  PosCurCmm                      n   overlay(IndDspF : 50)
010700100518     d  PosCurDECi                     n   overlay(IndDspF : 51)
010800100518     d  PosCurDECf                     n   overlay(IndDspF : 52)
010900100518     d  PosCurDEPi                     n   overlay(IndDspF : 53)
011000100518     d  PosCurDEPf                     n   overlay(IndDspF : 54)
011100100518     d  PosCurDett                     n   overlay(IndDspF : 55)
011200100518     d  PosCurDscmmie                  n   overlay(IndDspF : 56)
011300100728     d  PosCurDstat                    n   overlay(IndDspF : 66)
011400100518     d  PosCurDstat01                  n   overlay(IndDspF : 57)
011500100518     d  PosCurDstat02                  n   overlay(IndDspF : 58)
011600100518     d  PosCurDstat03                  n   overlay(IndDspF : 59)
011700100518     d  PosCurDstat04                  n   overlay(IndDspF : 60)
011800100518     d  PosCurDstat05                  n   overlay(IndDspF : 61)
011900100728     d  PosCurTotC                     n   overlay(IndDspF : 67)
012000100518     d  PosCurCscmmie                  n   overlay(IndDspF : 62)
012100100521     d  PosCurTotG                     n   overlay(IndDspF : 63)
012200100521     d  PosCurTscmmie                  n   overlay(IndDspF : 64)
012300110120     d  PosCurCar                     1n   overlay(IndDspF : 68)
012400100517         //   -?Riemissione videata?
012500100517     d  ErrGenerico                    n   overlay(IndDspF : 99)
012600090407
012700100517       // -?Parametri per controllo/inversione date?
012800100517     d WLBdat          ds                  inz
012900100517     d  G02dat                        8  0 inz
013000100517     d  G02inv                        8  0 inz
013100100517     d  G02err                        1    inz
013200100517     d  G02tgi                        5  0 inz
013300100517
013400100517       // -?Ridefinizione elenco "Tipi Attività" a video?
013500100517     d V01dstat_ds     ds            10    inz
013600100517     d   V01dstat01                        inz
013700100517     d   V01dstat02                        inz
013800100517     d   V01dstat03                        inz
013900100517     d   V01dstat04                        inz
014000100517     d   V01dstat05                        inz
014100100518     d     $DSTat              1     10    dim(10)
014200080206
014300100517       // -?Parametri ricevuti?
014400080206     d KPJBA         e ds
014500100517
014600100517       // -?Parametri per Reperimento dati utente?
014700100517     d TIBS34ds      e ds
014800100326
014900100517       // -?Parametri per Controllo abilitazioni?
015000100517     d TNTAA1ds      e ds                  inz
015100100517     d   ITAA1caut   e                     inz('§utegtc')
015200100518
015300100518       // -?Parametri per Ricerca Tipo attività?
015400100518     d TNTB81ds      e ds                  inz
015500100518
015600100518       // -?Parametri per Ricerca/Controllo tabelle (TNTBE)?
015700100518     d TIBS02ds      e ds                  inz
015800100518     d   T02mod      e                     inz('C')
015900100518     d   T02cod      e                     inz('TTA')
016000100329
016100100517       // -?Parametri per *pgm batch di stampa?
016200100517     d TRMK88ds      e ds                  inz
016300100326
016400100517       // -?Parametri per Reperimento filiali?
016500100517     d TRUL31ds      e ds                  inz
016600100517     d   Pog                  10    759    dim(250)  inz
016700110120       // -?Parametri per Reperimento aree   ?
016800110120     d TRUL31DS2     e ds
016900110120     d   Car                  22    171    DIM(50)
017000090402
017100110120       // -?Tabella "05" = Codici Aree
017200130829     d*// ds05          e ds                  inz
017300090717
017400100517       //--------------------------------------------------------------
017500100517       //?Definizione variabili globali.                               ?
017600100517       //--------------------------------------------------------------
017700080206
017800100517       // -?Flags booleani?
017900100517     d $Error          s               n   inz
018000100517     d $Fine           s               n   inz
018100100517     d $InzD01         s               n   inz(*on)
018200100518
018300100518       // -?Indici di schiere?
018400100518     d xx              s              3  0 inz
018500100517
018600100517       // -?Variabili per la gestione del video?
018700080207     d $Video          s              2    inz('D1')
018800080206
018900100517       // -?Campi di comodo - data?
019000100517     d Data_Eur        s               d   datfmt(*eur)  inz
019100100517     d wData_ISO_I     s               d   datfmt(*iso)  inz
019200100517     d wData_ISO_F     s               d   datfmt(*iso)  inz
019300100517     d wDataCorr       s              8  0 inz
019400121029     d wOggi_2         s              8  0 inz
019500100517     d W01deci         s              8  0 inz
019600100517     d W01decf         s              8  0 inz
019700100517     d W01depi         s              8  0 inz
019800100517     d W01depf         s              8  0 inz
019900100517     d wDataI          s              8  0 inz
020000100517     d wDataF          s              8  0 inz
020100110907     d wDay            s              5i 0 inz
020200100517     d wTipEla         s              1    inz
020300110120
020400110120     d idTabella       s              2
020500110120     d Ordinamento     s              1
020600110120     d idElemento      s              8
020700110120     d TastoUscita     s              1
020800080208
020900100517       //--------------------------------------------------------------
021000100517       //?Definizione procedure.                                       ?
021100100517       //--------------------------------------------------------------
021200100517
021300100517       // -?Reperimento dati utente?
021400100517      /copy gaitrasrc/srcProtoPR,TIBS34R
021500100517
021600100517       // -?Richiamo controllo abilitazioni utente?
021700100518      /copy gaitrasrc/srcProtoPR,TNTAA1C
021800100517
021900110120       // -?Richiamo ricerca/controllo tabelle (TABEL)?
022000110120      /copy gaitrasrc/srcprotopr,trul19r
022100110120
022200100517       // -?Richiamo controllo/inversione date?
022300100518      /copy gaitrasrc/srcProtoPR,XSRDA8
022400100517
022500100517       // -?Richiamo ricerca commerciali?
022600130829      /copy gaitrasrc/srcprotoPI,TRMK43R_1
022700130829      /copy gaitrasrc/srcprotoPR,TRMK43R
022800100518
022900100518       // -?Richiamo ricerca/controllo tabelle (TNTBE)?
023000100518      /copy gaitrasrc/srcProtoPR,TIBS02R
023100100518
023200100518       // -?Richiamo interrogazione/selezione Tipo Attività (tab. "TTA")?
023300100518     d tntb81r1        pr                  extpgm('TNTB81R1')
023400100518     d  kpjba                              likeds(kpjba)
023500100518     d  tntb81ds                           likeds(TNTB81ds)
023600100517
023700100517       // -?Sottomissione lavoro batch?
023800100517      /copy gaitrasrc/srcProtoPR,BCH10
023900100517
024000100517       // -?Richiamo diretto lavoro batch?
024100100519     d trmk88c         pr                  extpgm('TRMK88C')
024200100518     d   kpjba                             likeds(KPJBA)
024300100326
024400110120       // -?Richiamo caricamento filiali/aree gestite dall'utente?
024500110120     d trul31r         pr                  extpgm('TRUL31R')
024600110120     d  kpjba                              likeds(KPJBA)
024700110120     d  trul31ds                           likeds(trul31ds)
024800110120     d  trul31ds2                          likeds(trul31ds2)
024900110120
025000100517       //--------------------------------------------------------------
025100100517       //?Definizione key-list.                                        ?
025200100517       //--------------------------------------------------------------
025300080206
025400100517       // -?File TABEL00F?
025500100517     d k03tabel00    e ds                  extname(TABEL00F:*key)
025600100517     d                                     prefix(k_)  inz
025700080206
025800100517       //--------------------------------------------------------------
025900100517       //?M A I N - L I N E                                            ?
026000100517       //--------------------------------------------------------------
026100080206
026200080206     c     *Entry        plist
026300080206     c                   parm                    KPJBA
026400080206
026500080206      /free
026600080206
026700100517       // -?Operazioni iniziali?
026800100517       exsr  sr_RoutInz;
026900090601
027000100517       // -?Gestione videate?
027100100517       DoW  Not  $Fine;
027200100517         select;
027300100517           when  $Video = 'D1';
027400100517             exsr  sr_GesD01;
027500100517           other;
027600080206             $Fine = *on;
027700100517         endsl;
027800100517       EndDo;
027900080206
028000100517       // -?Operazioni finali?
028100100517       exsr  sr_RoutEnd;
028200080206
028300100517       //--------------------------------------------------------------
028400100517       //?Operazioni iniziali.                                         ?
028500100517       //--------------------------------------------------------------
028600100517       BEGSR  sr_RoutInz;
028700100517
028800100517         *inLR = *on;
028900100517
029000100517         // -?Reperimento dati job?
029100100517         exsr  sr_DatiJob;
029200100517
029300100517         // -?Reperimento data corrente?
029400100517         wDataCorr= %dec( %date() );
029500121029
029600121029         // -?Calcolo oggi - 2 anni?
029700121029         wData_ISO_I = %date(wDataCorr);
029800121029         wData_ISO_I = wData_ISO_I - %years(2);
029900121029         wOggi_2 = %dec(wData_ISO_I);
030000100517
030100100517         // -?Impostazione dati di testata?
030200100517         V01pgm = SDSpgm;
030300080206
030400100517         // -?Impostazione campi "fissi"?
030500100326         k_TBLkut = 1;
030600100211
030700100517         // -?Controllo se utente autorizzato alla funzione?
030800100517         reset  TNTAA1ds;
030900120813         //ITAA1caut = '§utegtc';         (già così)
031000100517         kpjbu     = TNTAA1ds;
031100100517         TNTAA1C ( kpjba );
031200100517         TNTAA1ds = kpjbu;
031300100517         if  oTAA1fabi = 'N';
031400100517           $Error      = *on;
031500100517           ErrMessage  = *on;
031600100517           ErrGenerico = *on;
031700100517           PosCurCmm   = *on;
031800100329           V01msg = $Msg(01);
031900080206           leavesr;
032000080206         endif;
032100100312
032200100517         // -?Caricamento filiali gestite dall'utente?
032300100517         clear  TRUL31ds;
032400110120         clear  TRUL31DS2;
032500100517         I31abi = oTAA1cabi;
032600100312         I31cdi = DUTdis;
032700100312         I31car = DUTare;
032800100312         I31cpo = DUTpou;
032900110120         TRUL31R ( kpjba : trul31ds : trul31ds2);
033000100517         if  O31pog <= *zero;
033100100517           $Error      = *on;
033200100326           ErrMessage  = *on;
033300100326           ErrGenerico = *on;
033400100326           V01msg = $Msg(01);
033500100312           leavesr;
033600100517         endif;
033700110120
033800110120         //?Abilito richiesta per Area
033900110120         DspStpCar = *off;
034000110120         IF  car(2) > *zeros;
034100110120           DspStpCar = *on;
034200110120         ENDIF;
034300100423
034400080206       ENDSR;
034500080206
034600080206       //--------------------------------------------------------------
034700100517       //?Reperimento Dati del job (Utente/Operativi).                 ?
034800080206       //--------------------------------------------------------------
034900100517       BEGSR  sr_DatiJob;
035000080206
035100080206         in(E) §AzUte;
035200080206         if NOT %error;
035300080206           in(E) §DatiUte;
035400080206         endif;
035500080206         if %error or RSut = *blanks;
035600080206           clear TIBS34ds;
035700080206           tibs34r(tibs34ds);
035800080206           in §AzUte;
035900080206           in §DatiUte;
036000080206         endif;
036100080206
036200080206       ENDSR;
036300080206
036400080206       //--------------------------------------------------------------
036500100517       //?Gestione videata D01.                                        ?
036600080206       //--------------------------------------------------------------
036700100517       BEGSR  sr_GesD01;
036800080206
036900100517         // -?Inizializzazione videata?
037000100517         if  $InzD01;
037100100517           exsr  sr_InzD01;
037200080206           $InzD01 = *off;
037300100517         endif;
037400080206
037500100517         // -?Emissione Testata?
037600100517         write  MK87T01;
037700100517         exfmt  MK87D01;
037800100326         ErrMessage   = *off;
037900100326         ErrGenerico  = *off;
038000100326         clear V01msg;
038100100517
038200100517         Select;
038300100326
038400100517           // -?Rilevato errore grave: uscita dal *pgm?
038500100517           When  $Error;
038600100326             $Fine = *on;
038700100326
038800100517           // -?F3=Fine?
038900100517           When  dsp_aid = c_F03;
039000100517             exsr  sr_F03D01;
039100100329
039200100517           // -?Invio / F6=Conferma?
039300100517           Other;
039400100517             exsr  sr_CtrD01;
039500100517             select;
039600100517               when  ErrGenerico;
039700100517                 leavesr;
039800100517               when  dsp_aid = c_F06;
039900100517                 exsr  sr_F06D01;
040000100517             endsl;
040100100329
040200100517         EndSl;
040300080206
040400080206       ENDSR;
040500080206
040600080206       //--------------------------------------------------------------
040700100517       //?Inizializzazione videata D01.                                ?
040800080206       //--------------------------------------------------------------
040900100517       BEGSR  sr_InzD01;
041000100329
041100100517         clear  MK87D01;
041200100517
041300100601         // -?Campi hidden?
041400100526         V01dscmm = 'I';
041500100601         V01cscmm = 'I';
041600100601         V01tscmm = 'I';
041700100601
041800100601         // -?Campi i/o?
041900100526         V01dscmmIE  = 'E';
042000100518         V01dstat = 'O';
042100100526         V01dstat_ds = 'TO   ';
042200080206
042300080206       ENDSR;
042400080206
042500080206       //--------------------------------------------------------------
042600100517       //?Gestione tasto funzionale F3 da videata D01:                 ?
042700100517       //?F3=Fine                                                      ?
042800080206       //--------------------------------------------------------------
042900100517       BEGSR  sr_F03D01;
043000080206
043100100517         // -?Chiusura del *pgm?
043200080206         $Fine = *on;
043300080206
043400080206       ENDSR;
043500100329
043600100329       //--------------------------------------------------------------
043700100517       //?Gestione tasto funzionale F6 da videata D01:                 ?
043800100517       //?F6=Conferma                                                  ?
043900100329       //--------------------------------------------------------------
044000100517       BEGSR  sr_F06D01;
044100100329
044200100517         // -?Impostazione parametri?
044300100517         clear  TRMK88ds;
044400100517         I88tcpe = V01ela;
044500110120         IF  V01car > *zeros;
044600110120           I88car   = %dec(V01car:3:0);
044700110120         ENDIF;
044800100517         if  V01cmm > *zero;
044900100517           I88cmmu  = %int(V01cmm);
045000100517         endif;
045100100517         I88deci    = W01deci;
045200100517         I88decf    = W01decf;
045300100517         I88depi    = W01depi;
045400100517         I88depf    = W01depf;
045500100517         I88dett    = V01dett;
045600100730         //I88dscmm   = V01dscmm;
045700100730         //I88dscmmie = V01dscmmie;
045800100730         if  V01dscmmie = *blank;
045900100730           I88dscmm   = 'O';
046000100730           //I88dscmmie = *blank;           (già così)
046100100730         else;
046200100730           I88dscmm   = 'I';
046300100730           I88dscmmie = V01dscmmie;
046400100730         endif;
046500100517         I88dstat   = V01dstat;
046600100517         I88dstatc  = V01dstat_ds;
046700100517         I88totc    = V01totc;
046800100730         //I88cscmm   = V01cscmm;
046900100730         //I88cscmmie = V01cscmmie;
047000100730         if  V01cscmmie = *blank;
047100100730           I88cscmm   = 'O';
047200100730           //I88cscmmie = *blank;           (già così)
047300100730         else;
047400100730           I88cscmm   = 'I';
047500100730           I88cscmmie = V01cscmmie;
047600100730         endif;
047700100517         I88totg    = V01totg;
047800100730         //I88tscmm   = V01tscmm;
047900100730         //I88tscmmie = V01tscmmie;
048000100730         if  V01tscmmie = *blank;
048100100730           I88tscmm   = 'O';
048200100730           //I88tscmmie = *blank;           (già così)
048300100730         else;
048400100730           I88tscmm   = 'I';
048500100730           I88tscmmie = V01tscmmie;
048600100730         endif;
048700100423         kcoaz = 'MK88';
048800100423         kpjbu = TRMK88ds;
048900100329         kbuff = *blanks;
049000100329
049100100712         if  knmus = *all'1';
049200100517           // -?Richiamo interattivo?
049300100519           trmk88c ( kpjba );
049400100712         else;
049500100517           // -?Sottomissione batch?
049600100712            bch10 ( kpjba );
049700100712         endif;
049800100329
049900100517         // -?Chiusura del *pgm?
050000100329         $Fine = *on;
050100100329
050200100329       ENDSR;
050300090717
050400080206       //--------------------------------------------------------------
050500100517       //?Controllo videata D01.                                       ?
050600080206       //--------------------------------------------------------------
050700100517       BEGSR  sr_CtrD01;
050800100326
050900100517         clear  W01deci;
051000100517         clear  W01decf;
051100100517         clear  W01depi;
051200100517         clear  W01depf;
051300090427
051400100517         %subst(IndDspF : 50) = *off;
051500100728
051600100728         // -?RICERCHE?
051700100730         exsr  sr_Search;
051800100730         if  ErrGenerico;
051900100730           leavesr;
052000100730         endif;
052100100728
052200100728
052300100728
052400100728         // -?Tipo Elaborazione?
052500100728         if  V01ela <> 'C'   and   V01ela <> 'P'   and   V01ela <> 'E';
052600100728           ErrMessage  = *on;
052700100728           ErrGenerico = *on;
052800100728           PosCurEla   = *on;
052900100730           V01msg = $Msg(17);
053000100728           leavesr;
053100100728         endif;
053200100728
053300100728
053400110120       //?Controllo se impostato
053500110120         IF  V01car <> *blanks and V01car <> *zeros;
053600110120        //?Solo numerico
053700110120           IF  %check(C_digits:V01car) > 0;
053800110120             ErrMessage  = *on;
053900110120             ErrGenerico = *on;
054000110120             PosCurCar   = *on;
054100110120             V01msg      = $Msg(23);
054200110120             leavesr;
054300110120           ENDIF;
054400110120         //?Valido in tabella
054500130829           //clear ds05;
054600110120           k_TBLcod = '05';
054700110120           k_TBLkey = V01car;
054800110120           chain  %kds(K03tabel00)  TABEL00F;
054900110120           IF  NOT %found(TABEL00F)
055000110120               or  TBLflg <> *blank;
055100110120             ErrMessage  = *on;
055200110120             ErrGenerico = *on;
055300110120             PosCurCar   = *on;
055400110120             V01msg      = $Msg(23);
055500110120             leavesr;
055600110120           ENDIF;
055700110120
055800130829           //ds05 = TBLuni;
055900110120
056000110120         //?Utente abilitato all'area
056100110120           IF  %lookup(V01car : car) = 0;
056200110120             ErrMessage  = *on;
056300110120             ErrGenerico = *on;
056400110120             PosCurCar   = *on;
056500110120             V01msg      = $Msg(23);
056600110120             leavesr;
056700110120           ENDIF;
056800110120         ENDIF;
056900110120
057000100326
057100100517         // -?COMMERCIALE?
057200100517         Select;
057300110921
057400110921           //?Se sono in sede e non seleziono ne l'area e ne il commerciale errore
057500110921           WHEN dutpou = 046 and ((V01car = *blanks or V01car = *zeros) and
057600110921             (V01cmm = *blanks or V01cmm = *zeros));
057700110921             ErrMessage  = *on;
057800110921             ErrGenerico = *on;
057900110921             PosCurCmm   = *on;
058000110921             V01msg      = $Msg(11);
058100110921             leavesr;
058200100517
058300110120           //?Non immettere se richiesta l'area
058400110120           WHEN V01car <> *blanks and V01car > *zeros and
058500110120             V01cmm <> *blanks and V01cmm > *zeros;
058600110120             ErrMessage  = *on;
058700110120             ErrGenerico = *on;
058800110120             PosCurCmm   = *on;
058900110120             V01msg      = $Msg(24);
059000110120             leavesr;
059100110120
059200100517           // -?Non immesso?
059300100517           When  V01cmm = *blanks  or  V01cmm = *zeros;
059400100728
059500100728           // -?Ricerca?
059600100730           //  ?(già gestita - vedi subr. "sr_Search")?
059700100728           //When  %scan( '?' : V01cmm ) > *zero;
059800100517
059900100517           // -?Controllo numericità?
060000100517           When  %check( c_Digits : V01cmm ) > *zero;
060100100517             ErrMessage  = *on;
060200100517             ErrGenerico = *on;
060300100517             PosCurCmm   = *on;
060400100517             V01msg = $Msg(02);
060500100517             leavesr;
060600100517
060700100517           // -?Controllo validità?
060800100517           Other;
060900130829             chain  (%int(V01cmm))  AZCMM000;
061000130829             if  NOT %found(AZCMM01L);
061100100517               ErrMessage  = *on;
061200100517               ErrGenerico = *on;
061300100517               PosCurCmm   = *on;
061400100517               V01msg = $Msg(02);
061500100517               leavesr;
061600100517             endif;
061700110614
061800110614           // -?No vari o clienti inattivi?
061900130829             IF  CMMpar <> *blanks;
062000110614               ErrMessage  = *on;
062100110614               ErrGenerico = *on;
062200110614               PosCurCmm   = *on;
062300110614               V01msg = $Msg(02);
062400110614               leavesr;
062500110614             ENDIF;
062600100517
062700100517         EndSl;
062800100326
062900100518         IF  V01cmm > *zero;
063000100518
063100100518           // -?Abilitazione utente al commerciale?
063200100518           reset  TNTAA1ds;
063300100518           //ITAA1caut = '§utegtc';         (già così)
063400100518           ITAA1cmm  = %int( V01cmm );
063500100518           kpjbu     = TNTAA1ds;
063600100518           tntaa1c (kpjba);
063700100518           TNTAA1ds = kpjbu;
063800100518           if  oTAA1fabi = 'N';
063900100518             ErrMessage  = *on;
064000100518             ErrGenerico = *on;
064100100518             PosCurCmm   = *on;
064200100518             V01msg = $Msg(02);
064300100518             leavesr;
064400100518           endif;
064500100326
064600100518           // -?Selezionabili SOLO unificanti?
064700130829           if  %int( V01cmm ) <> CMMuni;
064800100518             ErrMessage  = *on;
064900100518             ErrGenerico = *on;
065000100518             PosCurCmm   = *on;
065100100518             V01msg = $Msg(03);
065200100518             leavesr;
065300100518           endif;
065400100518
065500100518         ENDIF;
065600100728
065700100728
065800100329
065900100518         // -?Controllo immissione date per l'elaborazione richiesta?
066000100518         //  ?(consuntiva e/o preventiva)?
066100100518         select;
066200100518           when  (V01ela = 'C'   or   V01ela = 'E')   and
066300100518                 V01deci = *zero;
066400100518             ErrMessage  = *on;
066500100518             ErrGenerico = *on;
066600100518             PosCurDeci  = *on;
066700100518             V01msg = $Msg(04);
066800100518             leavesr;
066900100518           when  (V01ela = 'P'   or   V01ela = 'E')   and
067000100518                 V01depi = *zero;
067100100518             ErrMessage  = *on;
067200100518             ErrGenerico = *on;
067300100518             PosCurDepi  = *on;
067400100518             V01msg = $Msg(04);
067500100518             leavesr;
067600100518           when  V01ela = 'C'   and   (V01depi + V01depf) > *zero;
067700100518             ErrMessage  = *on;
067800100518             ErrGenerico = *on;
067900100518             PosCurDepi  = *on;
068000100518             V01msg = $Msg(14);
068100100518             leavesr;
068200100518           when  V01ela = 'P'   and   (V01deci + V01decf) > *zero;
068300100518             ErrMessage  = *on;
068400100518             ErrGenerico = *on;
068500100518             PosCurDeci  = *on;
068600100518             V01msg = $Msg(14);
068700100518             leavesr;
068800100518         endsl;
068900100517
069000100518         // -?Controllo date x elaborazione consuntiva?
069100100517         if  V01deci <> *zero   or   V01decf <> *zero;
069200100518           wDataI  = V01deci;
069300100518           wDataF  = V01decf;
069400100517           wTipEla = 'C';
069500100517           exsr  sr_CtrlDate;
069600100517           if  ErrMessage;
069700100517             leavesr;
069800100517           endif;
069900100517         endif;
070000100517
070100100518         // -?Controllo date x elaborazione preventiva?
070200100517         if  V01depi <> *zero   or   V01depf <> *zero;
070300100517           wDataI  = V01depi;
070400100517           wDataF  = V01depf;
070500100517           wTipEla = 'P';
070600100517           exsr  sr_CtrlDate;
070700100517           if  ErrMessage;
070800100517             leavesr;
070900100517           endif;
071000100517         endif;
071100100728
071200100728
071300100518
071400100518         // -?Controllo selezioni x stampa dettaglio agenda?
071500100728         Select;
071600100730
071700100730           When  V01dett = *blank;
071800100730             ErrMessage  = *on;
071900100730             ErrGenerico = *on;
072000100730             PosCurDett  = *on;
072100100730             V01msg = $Msg(18);
072200100730             leavesr;
072300100728           When  V01dett <> 'N'   and   V01dett <> 'S';
072400100728             ErrMessage  = *on;
072500100728             ErrGenerico = *on;
072600100728             PosCurDett  = *on;
072700100730             V01msg = $Msg(19);
072800100728             leavesr;
072900100728
073000100728           // ·?Selezione commerciali?
073100100730           When  V01Dscmmie <> *blank   and   V01Dscmmie <> 'I'
073200100730                                        and   V01Dscmmie <> 'E';
073300100728             ErrMessage  = *on;
073400100728             ErrGenerico = *on;
073500100728             PosCurDscmmie = *on;
073600100730             V01msg = $Msg(20);
073700100728             leavesr;
073800100728
073900100728           // ·?Totali?
074000100728           When V01dett = 'N'  and
074100100728                V01totc = 'N'  and  V01totg = 'N';
074200100728             ErrMessage  = *on;
074300100728             ErrGenerico = *on;
074400100728             PosCurDett  = *on;
074500100728             V01msg = $Msg(10);
074600100728             leavesr;
074700100517
074800100728           // ·?Tipi Attività?
074900100730           //  ?(ricerca già gestita - vedi subr. "sr_Search")?
075000100730           When  V01Dstat = *blank;
075100100730             ErrMessage  = *on;
075200100730             ErrGenerico = *on;
075300100730             PosCurDstat = *on;
075400100730             V01msg = $Msg(21);
075500100730             leavesr;
075600100728           When  V01Dstat <> 'I'   and   V01Dstat <> 'O';
075700100728             ErrMessage  = *on;
075800100728             ErrGenerico = *on;
075900100728             PosCurDstat = *on;
076000100730             V01msg = $Msg(22);
076100100728             leavesr;
076200100728           When  V01dett  = 'S'   and
076300100728                 V01dstat = 'I'   and   V01dstat_ds = *blank;
076400100728             ErrMessage    = *on;
076500100728             ErrGenerico   = *on;
076600100728             PosCurDstat01 = *on;
076700100728             V01msg = $Msg(12);
076800100728             leavesr;
076900100728
077000100728         EndSl;
077100100518
077200100728
077300100728
077400100518         // -?Controllo/ricerca Tipi Attività?
077500100518         If  V01dstat_ds <> *blank;
077600100518           exsr  sr_CtrlTAT;
077700100518           if  ErrGenerico;
077800100518             leavesr;
077900100518           endif;
078000100518         EndIf;
078100100728
078200100728
078300100728
078400100728         // -?Controllo selezioni x stampa totale x commerciale?
078500100728         select;
078600100728
078700100730           when  V01totc = *blank;
078800100730             ErrMessage  = *on;
078900100730             ErrGenerico = *on;
079000100730             PosCurTotC  = *on;
079100100730             V01msg = $Msg(18);
079200100730             leavesr;
079300100728           when  V01totc <> 'N'   and   V01totc <> 'S';
079400100728             ErrMessage  = *on;
079500100728             ErrGenerico = *on;
079600100728             PosCurTotC  = *on;
079700100730             V01msg = $Msg(19);
079800100728             leavesr;
079900100728
080000100728           // ·?Selezione commerciali errata?
080100100730           when  V01Cscmmie <> *blank   and   V01Cscmmie <> 'I'
080200100730                                        and   V01Cscmmie <> 'E';
080300100728             ErrMessage  = *on;
080400100728             ErrGenerico = *on;
080500100728             PosCurCscmmie = *on;
080600100730             V01msg = $Msg(20);
080700100728             leavesr;
080800100728         endsl;
080900100728
081000100728
081100100521
081200100601         // -?Controllo stampa totale generale:?
081300100601         select;
081400100728
081500100730           when  V01totg = *blank;
081600100730             ErrMessage  = *on;
081700100730             ErrGenerico = *on;
081800100730             PosCurTotG  = *on;
081900100730             V01msg = $Msg(18);
082000100730             leavesr;
082100100728           when  V01totg <> 'N'   and   V01totg <> 'S';
082200100728             ErrMessage  = *on;
082300100728             ErrGenerico = *on;
082400100728             PosCurTotG  = *on;
082500100730             V01msg = $Msg(19);
082600100728             leavesr;
082700100601
082800100601           // -?obbligatoria SE non specificato un comm.le unificante?
082900100601           when  V01totg  = 'N'   and   V01cmm <= *zero;
083000100601             ErrMessage   = *on;
083100100601             ErrGenerico  = *on;
083200100601             PosCurTotG   = *on;
083300100601             V01msg = $Msg(15);
083400100601             leavesr;
083500100601
083600100601           // -?NON richiedibile SE specificato un comm.le unificante?
083700100601           when  V01totg  = 'S'   and   V01cmm > *zero;
083800100601             ErrMessage   = *on;
083900100601             ErrGenerico  = *on;
084000100601             PosCurTotG   = *on;
084100100601             V01msg = $Msg(16);
084200100601             leavesr;
084300100728
084400100728           // ·?Selezione commerciali errata?
084500100730           when  V01Tscmmie <> *blank   and   V01Tscmmie <> 'I'
084600100730                                        and   V01Tscmmie <> 'E';
084700100728             ErrMessage  = *on;
084800100728             ErrGenerico = *on;
084900100728             PosCurTscmmie = *on;
085000100730             V01msg = $Msg(20);
085100100728             leavesr;
085200100728
085300100601         endsl;
085400100517
085500080206       ENDSR;
085600100730
085700100730       //--------------------------------------------------------------
085800100730       //?Gestione ricerche in fmt D1.
085900100730       //--------------------------------------------------------------
086000100730       BEGSR  sr_Search;
086100100730
086200100730         Select;
086300110120
086400110120       //?AREA
086500110120           When  %scan('?' : V01car) > *zero;
086600110120             idTabella = '05';
086700110120             clear Ordinamento;
086800110120             clear idElemento;
086900110120             clear TastoUscita;
087000110120             Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
087100110120             V01car = idElemento;
087200110120
087300100730
087400100730           // -?Commerciale?
087500100730           When  %scan( '?' : V01cmm ) > *zero;
087600130829             clear  TRMK43ds;
087700130829             iMK43solU = 'S';
087800130829             iMK43fil  = DUTpou;
087900130829             TRMK43R (kpjba : TRMK43ds);
088000130829             if  oMK43err = *off  and  oMK43fxx = *blank;
088100130829               V01cmm = %editc( oMK43cmm : 'X' );
088200130829             endif;
088300100730             ErrGenerico = *on;
088400100730             PosCurCmm   = *on;
088500100730             leavesr;
088600100730
088700100730           // -?Tipi Attività?
088800100730           When  %scan( '?' : V01dstat_ds ) > *zero;
088900100730             For  xx = 1  To  %elem($DSTat);
089000100730               if  %scan( '?' : $DSTat(xx) ) > *zero;
089100100730                 clear  $DSTat(xx);
089200100730                 clear  TNTB81ds;
089300100730                 TNTB81R1 ( kpjba : TNTB81ds );
089400100730                 select;
089500100730                   when  oTTAfxx <> *blank;
089600100730                   when  oTTAerr = *on;
089700100730                     ErrMessage  = *on;
089800100730                     V01msg = oTTAmsg;
089900100730                   other;
090000100730                     $DSTat(xx) = %subst( oTTAke1 : 1 :
090100100730                                          %len(V01dstat01) );
090200100730                 endsl;
090300100730                 ErrGenerico = *on;
090400100730                 %subst( IndDspF : (56+xx) : 1 ) = *on;
090500100730                 leavesr;
090600100730               endif;
090700100730             EndFor;
090800100730
090900100730           EndSl;
091000100730
091100100730       ENDSR;
091200100517
091300100517       //--------------------------------------------------------------
091400100517       //?Controllo range di date (elaboraz. consuntiva o preventiva)
091500100517       //--------------------------------------------------------------
091600100517       BEGSR  sr_CtrlDate;
091700100517
091800100517         // -?Data Dal corretta?
091900100517         If  wDataI <> *zero;
092000100517
092100100517           clear WLBdat;
092200100517           G02dat = wDataI;
092300100517           XSRDA8(WLBdat);
092400100517           if  G02err = '1';
092500100517             ErrMessage  = *on;
092600100517             ErrGenerico = *on;
092700100517             if  wTipEla = 'C';
092800100517               PosCurDeci  = *on;
092900100517             else;
093000100517               PosCurDepi  = *on;
093100100517             endif;
093200100517             V01msg = $Msg(05);
093300100517             leavesr;
093400100517           endif;
093500100517
093600100517           if  wTipEla = 'C';
093700100517             V01deci = G02dat;
093800100517             W01deci = G02inv;
093900100517           else;
094000100517             V01depi = G02dat;
094100100517             W01depi = G02inv;
094200100517           endif;
094300100517           wDataI   = G02inv;
094400121029
094500121029           // -?Per Consuntivo la data non può essere inferiore a oggi - 2 anni
094600121029           IF  wDataI < wOggi_2 and wTipEla = 'C';
094700121029             ErrMessage  = *on;
094800121029             ErrGenerico = *on;
094900121029             PosCurDeci  = *on;
095000121029             V01msg = $Msg(05);
095100121029             V01msg = %trim(V01msg) +
095200121029             ' è inferiore a 2 anni, non ci sono i dati';
095300121029             leavesr;
095400121029           ENDIF;
095500100517
095600100517           // -?Controllo data non inferiore al 2000 o superiore al 2039?
095700100517           if  wDataI < 20000101  or  wDataI > 20391231;
095800100517             ErrMessage  = *on;
095900100517             ErrGenerico = *on;
096000100517             if  wTipEla = 'C';
096100100517               PosCurDeci  = *on;
096200100517             else;
096300100517               PosCurDepi  = *on;
096400100517             endif;
096500100517             V01msg = $Msg(05);
096600100517             leavesr;
096700100517           endif;
096800100517
096900100517         EndIf;
097000100517
097100100517         // -?Se impostata data "DAL" e non "AL" =>?
097200100517         //  ?imposta "AL" = data del giorno?
097300100517         if  wDataI > *zero  and  wDataF = *zero;
097400100518           wDataF = *date;
097500100517         endif;
097600100517
097700100517         // -?Data Al corretta?
097800100517         If  wDataF <> *zero;
097900100517           clear WLBdat;
098000100517           G02dat = wDataF;
098100100517           XSRDA8(WLBdat);
098200100517           if  G02err = '1';
098300100517             ErrMessage  = *on;
098400100517             ErrGenerico = *on;
098500100517             if  wTipEla = 'C';
098600100517               PosCurDecf  = *on;
098700100517             else;
098800100517               PosCurDepf  = *on;
098900100517             endif;
099000100517             V01msg = $Msg(05);
099100100517             leavesr;
099200100517           endif;
099300100517
099400100517           if  wTipEla = 'C';
099500100517             V01decf = G02dat;
099600100517             W01decf = G02inv;
099700100517           else;
099800100517             V01depf = G02dat;
099900100517             W01depf = G02inv;
100000100517           endif;
100100100517           wDataF = G02inv;
100200100517
100300100517           // -?Controllo data non inferiore al 2000 o superiore al 2039?
100400100517           if  wDataF < 20000101  or  wDataF > 20391231;
100500100517             ErrMessage  = *on;
100600100517             ErrGenerico = *on;
100700100517             if  wTipEla = 'C';
100800100517               PosCurDecf  = *on;
100900100517             else;
101000100517               PosCurDepf  = *on;
101100100517             endif;
101200100517             V01msg = $Msg(05);
101300100517             leavesr;
101400100517           endif;
101500100517
101600100517         EndIf;
101700100517
101800100517         // -?Data "DAL" congruente con "AL"?
101900100517         select;
102000100517           when  wDataI > *zero  and  wDataF = *zero;
102100100517             ErrMessage  = *on;
102200100517             ErrGenerico = *on;
102300100517             if  wTipEla = 'C';
102400100517               PosCurDeci  = *on;
102500100517             else;
102600100517               PosCurDepi  = *on;
102700100517             endif;
102800100517             V01msg = $Msg(06);
102900100517             leavesr;
103000100517           when  wDataI > *zero  and  wDataF > *zero  and
103100100517                 wDataI > wDataF;
103200100517             ErrMessage  = *on;
103300100517             ErrGenerico = *on;
103400100517             if  wTipEla = 'C';
103500100517               PosCurDeci  = *on;
103600100517             else;
103700100517               PosCurDepi  = *on;
103800100517             endif;
103900100517             V01msg = $Msg(06);
104000100517             leavesr;
104100100517           when  wTipEla = 'C'   and   wDataF >= wDataCorr;
104200100517             ErrMessage  = *on;
104300100517             ErrGenerico = *on;
104400100517             PosCurDecf  = *on;
104500100517             V01msg = $Msg(08);
104600100517             leavesr;
104700100517           when  wTipEla = 'P'   and   wDataI < wDataCorr;
104800100517             ErrMessage  = *on;
104900100517             ErrGenerico = *on;
105000100517             PosCurDepi  = *on;
105100100517             V01msg = $Msg(09);
105200100517             leavesr;
105300100517         endsl;
105400100517
105500100908         // -?Range massimo di 65 giorni?
105600100517         wData_Iso_I = %date(wDataI);
105700100517         wData_Iso_F = %date(wDataF);
105800100517         wDay = %diff( wData_ISO_F : wData_ISO_I : *days );
105900100908         if  wDay > 65;
106000100517           ErrMessage  = *on;
106100100517           ErrGenerico = *on;
106200100517           if  wTipEla = 'C';
106300100517             PosCurDeci  = *on;
106400100517           else;
106500100517             PosCurDepi  = *on;
106600100517           endif;
106700100517           V01msg = $Msg(07);
106800100517           leavesr;
106900100517         endif;
107000100517
107100100517       ENDSR;
107200100518
107300100518       //--------------------------------------------------------------
107400100518       //?Controllo Tipi Attività inseriti
107500100518       //--------------------------------------------------------------
107600100518       BEGSR  sr_CtrlTAT;
107700100518
107800100518         FOR  xx = 1  TO  %elem($DSTat);
107900100518
108000100518           Select;
108100100518
108200100518             // -?Vuoto?
108300100518             When  $DSTat(xx) = *blank;
108400100518               iter;
108500100728
108600100728             // -?Richiesta interrogazione?
108700100728             //  ?(già gestita all'inizio della subr. sr_CtrD01)?
108800100728             //When  %scan( '?' : $DSTat(xx) ) > *zero;
108900100518
109000100518             // -?Controllo?
109100100518             Other;
109200100518               reset  TIBS02ds;
109300100518               //T02mod = 'C';            (già così)
109400100518               //T02cod = 'TTA';          (già così)
109500100518               T02ke1 = $DSTat(xx);
109600100518               T02sif = KNSIF;
109700100518               TNTBE_RicercaControllo ( kpjba : tibs02ds );
109800100518               if  T02err <> *blank;
109900100518                 ErrMessage  = *on;
110000100518                 ErrGenerico = *on;
110100100518                 %subst( IndDspF : (56+xx) : 1 ) = *on;
110200100518                 V01msg = $Msg(13);
110300100518                leavesr;
110400100518               endif;
110500100518
110600100518           EndSl;
110700100518
110800100518         ENDFOR;
110900100518
111000100518       ENDSR;
111100080206
111200080206       //--------------------------------------------------------------
111300080206       //?Operazioni finali.
111400080206       //--------------------------------------------------------------
111500100517       BEGSR  sr_RoutEnd;
111600100518
111700100518         clear  TIBS02ds;
111800100518         T02tla = 'C';
111900100518         TNTBE_RicercaControllo ( kpjba : tibs02ds );
112000080206
112100080206         return;
112200080206
112300080206       ENDSR;
112400080206
112500080206      /end-free
112600100329
112700080206       //--------------------------------------------------------------
112800080206       //?Schiere a tempo di compilazione.
112900080206       //--------------------------------------------------------------
113000080206
113100100517** - $Msg:?Messaggi di Errore?-----------------------------------------------*
113200100329Utente non abilitato alla funzione                                            01
113300100329Codice commerciale errato o non in gestione all'utente                        02
113400100329Richiedere solo commerciali UNIFICANTI!                                       03
113500100518Impostare il periodo da elaborare                                             04
113600100517Data errata                                                                   05
113700100517Data "DAL" incongruente con data "AL"                                         06
113800100908Periodo di elaborazione troppo grande (maggiore di 65 giorni)                 07
113900100517Data finale elaborazione consuntiva deve essere minore di oggi                08
114000100518Data iniziale elaborazione preventiva deve essere maggiore/uguale ad oggi     09
114100100518Selezionare almeno una stampa                                                 10
114200110921Selezionare almeno un'area o un commerciale                                   11
114300100518Includere almeno un tipo attività                                             12
114400100518Tipo attività errato                                                          13
114500100518Periodo NON richiesto dal tipo elaborazione specificato                       14
114600100521Totali generali obbligatori SE NON specificato un commerciale unificante      15
114700100601Non richiedere il totale generale per la stampa di un solo comm.le unificante 16
114800100730Tipo elaborazione errato. Valori ammessi: "C", "P", "E".                      17
114900100730Richiesta di stampa obbligatoria. Valori ammessi: "S", "N".                   18
115000100730Richiesta di stampa errata. Valori ammessi: "S", "N".                         19
115100100730Selezione commerciali errata. Valori ammessi: "I", "E", "_".                  20
115200100730Inclusione/Omissione tipi attività obbligatoria. Valori ammessi: "I", "O".    21
115300100730Inclusione/Omissione tipi attività errata. Valori ammessi: "I", "O".          22
115400110120Codice Area errato o non in gestione all'utente                               23
115500110921Non impostare il commerciale se inserito il codice area                       24
