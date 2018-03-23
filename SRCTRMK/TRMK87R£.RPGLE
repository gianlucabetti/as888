000100100517       //==============================================================
000200100517       //?TRMK87R * Statistica commerciale: filtro di lancio           ?
000300100517       //==============================================================
000400080206
000500100517       //--------------------------------------------------------------
000600100517       //?Specifiche di controllo.                                     ?
000700100517       //--------------------------------------------------------------
000800100517
000900090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000100517     h dftactgrp(*no)
001100080206
001200100517       //--------------------------------------------------------------
001300100517       //?Dichiarazione file.                                          ?
001400100517       //--------------------------------------------------------------
001500100326
001600100517       // -?Tabelle?
001700090720     fTABEL00F  if   e           k disk
001800090720
001900100517       // -?Video parametri di lancio?
002000100423     fTRMK87D   cf   e             workstn
002100080208     f                                     indds(IndDspF)
002200080206     f                                     infds(InfDspF)
002300080206
002400100517       //--------------------------------------------------------------
002500100517       //?Definizione costanti.                                        ?
002600100517       //--------------------------------------------------------------
002700080207
002800100517       // -?Tasti funzionali a video?
002900080207     d c_F01           c                   const(x'31')
003000080207     d c_F02           c                   const(x'32')
003100080207     d c_F03           c                   const(x'33')
003200090406     d c_F04           c                   const(x'34')
003300080207     d c_F05           c                   const(x'35')
003400080207     d c_F06           c                   const(x'36')
003500080207     d c_F07           c                   const(x'37')
003600080207     d c_F08           c                   const(x'38')
003700080207     d c_F09           c                   const(x'39')
003800080207     d c_F10           c                   const(x'3A')
003900090303     d c_F11           c                   const(x'3B')
004000080207     d c_F12           c                   const(x'3C')
004100080207     d c_F13           c                   const(x'B1')
004200080207     d c_F14           c                   const(x'B2')
004300080207     d c_F15           c                   const(x'B3')
004400080207     d c_F16           c                   const(x'B4')
004500080207     d c_F17           c                   const(x'B5')
004600080207     d c_F18           c                   const(x'B6')
004700080207     d c_F19           c                   const(x'B7')
004800080207     d c_F20           c                   const(x'B8')
004900080207     d c_F21           c                   const(x'B9')
005000080207     d c_F22           c                   const(x'BA')
005100080207     d c_F23           c                   const(x'BB')
005200080207     d c_F24           c                   const(x'BC')
005300080207     d c_Enter         c                   const(x'F1')
005400080207     d c_RollDown      c                   const(x'F4')
005500080207     d c_RollUp        c                   const(x'F5')
005600100211
005700100517       // -?Costante per controllo "caratteri solo numerici"?
005800100517     d c_Digits        c                   const('0123456789')
005900080206
006000100517       //--------------------------------------------------------------
006100100517       //?Definizione schiere.                                         ?
006200100517       //--------------------------------------------------------------
006300080206
006400100517       // -?Messaggi di errore?
006500110120     d $Msg            s             78    dim(24) ctdata perrcd(1)
006600080206
006700100517       //--------------------------------------------------------------
006800100517       //?Definizione aree dati.                                       ?
006900100517       //--------------------------------------------------------------
007000080206
007100100517       // -?Dati utente?
007200080206     d §AzUte        e ds                  extname(AZUTE00F)
007300080206     d                                     dtaara
007400080206     d §DatiUte      e ds                  extname(dDatiUte)
007500080206     d                                     dtaara
007600080206
007700100517       //--------------------------------------------------------------
007800100517       //?Definizione strutture dati.                                  ?
007900100517       //--------------------------------------------------------------
008000080206
008100100517       // -?Status ds?
008200080206     d Psds           sds
008300080206     d   SDSpgm          *proc
008400080206
008500100517       // -?InfDS?
008600080206     d InfDspF         ds
008700080207     d  dsp_aid              369    369a
008800080206
008900100517       // -?Indicatori su DspF?
009000080208     d IndDspF         ds
009100100517         // -?Emissione messaggio di errore?
009200100517     d  ErrMessage                     n   overlay(IndDspF : 28)
009300110120         // -?Visualizzazione campi a video?
009400110120     d  DspStpCar                     1n   overlay(IndDspF : 40)
009500100517         // -?Posizionamento cursore & Segnalazione errore?
009600100728     d  PosCurEla                      n   overlay(IndDspF : 65)
009700100517     d  PosCurCmm                      n   overlay(IndDspF : 50)
009800100518     d  PosCurDECi                     n   overlay(IndDspF : 51)
009900100518     d  PosCurDECf                     n   overlay(IndDspF : 52)
010000100518     d  PosCurDEPi                     n   overlay(IndDspF : 53)
010100100518     d  PosCurDEPf                     n   overlay(IndDspF : 54)
010200100518     d  PosCurDett                     n   overlay(IndDspF : 55)
010300100518     d  PosCurDscmmie                  n   overlay(IndDspF : 56)
010400100728     d  PosCurDstat                    n   overlay(IndDspF : 66)
010500100518     d  PosCurDstat01                  n   overlay(IndDspF : 57)
010600100518     d  PosCurDstat02                  n   overlay(IndDspF : 58)
010700100518     d  PosCurDstat03                  n   overlay(IndDspF : 59)
010800100518     d  PosCurDstat04                  n   overlay(IndDspF : 60)
010900100518     d  PosCurDstat05                  n   overlay(IndDspF : 61)
011000100728     d  PosCurTotC                     n   overlay(IndDspF : 67)
011100100518     d  PosCurCscmmie                  n   overlay(IndDspF : 62)
011200100521     d  PosCurTotG                     n   overlay(IndDspF : 63)
011300100521     d  PosCurTscmmie                  n   overlay(IndDspF : 64)
011400110120     d  PosCurCar                     1n   overlay(IndDspF : 68)
011500100517         //   -?Riemissione videata?
011600100517     d  ErrGenerico                    n   overlay(IndDspF : 99)
011700090407
011800100517       // -?Parametri per controllo/inversione date?
011900100517     d WLBdat          ds                  inz
012000100517     d  G02dat                        8  0 inz
012100100517     d  G02inv                        8  0 inz
012200100517     d  G02err                        1    inz
012300100517     d  G02tgi                        5  0 inz
012400100517
012500100517       // -?Ridefinizione elenco "Tipi Attività" a video?
012600100517     d V01dstat_ds     ds            10    inz
012700100517     d   V01dstat01                        inz
012800100517     d   V01dstat02                        inz
012900100517     d   V01dstat03                        inz
013000100517     d   V01dstat04                        inz
013100100517     d   V01dstat05                        inz
013200100518     d     $DSTat              1     10    dim(10)
013300080206
013400100517       // -?Parametri ricevuti?
013500080206     d KPJBA         e ds
013600100517
013700100517       // -?Parametri per Reperimento dati utente?
013800100517     d TIBS34ds      e ds
013900100326
014000100517       // -?Parametri per Controllo abilitazioni?
014100100517     d TNTAA1ds      e ds                  inz
014200100517     d   ITAA1caut   e                     inz('§utegtc')
014300100518
014400100518       // -?Parametri per Ricerca Tipo attività?
014500100518     d TNTB81ds      e ds                  inz
014600100518
014700100518       // -?Parametri per Ricerca/Controllo tabelle (TNTBE)?
014800100518     d TIBS02ds      e ds                  inz
014900100518     d   T02mod      e                     inz('C')
015000100518     d   T02cod      e                     inz('TTA')
015100100329
015200100517       // -?Parametri per *pgm batch di stampa?
015300100517     d TRMK88ds      e ds                  inz
015400100326
015500100517       // -?Parametri per Reperimento filiali?
015600100517     d TRUL31ds      e ds                  inz
015700100517     d   Pog                  10    759    dim(250)  inz
015800110120       // -?Parametri per Reperimento aree   ?
015900110120     d TRUL31DS2     e ds
016000110120     d   Car                  22    171    DIM(50)
016100090402
016200100517       // -?Tabella "01" = Codici Commerciali?
016300090318     d ds01          e ds                  inz
016400110120       // -?Tabella "05" = Codici Aree
016500110120     d ds05          e ds                  inz
016600090717
016700100517       //--------------------------------------------------------------
016800100517       //?Definizione variabili globali.                               ?
016900100517       //--------------------------------------------------------------
017000080206
017100100517       // -?Flags booleani?
017200100517     d $End            s               n   inz
017300100517     d $Error          s               n   inz
017400100517     d $Fine           s               n   inz
017500100517     d $InzD01         s               n   inz(*on)
017600100518
017700100518       // -?Indici di schiere?
017800100518     d xx              s              3  0 inz
017900100517
018000100517       // -?Variabili per la gestione del video?
018100080207     d $Video          s              2    inz('D1')
018200080206
018300100517       // -?Campi di comodo - data?
018400100517     d Data_Eur        s               d   datfmt(*eur)  inz
018500100517     d wData_ISO_I     s               d   datfmt(*iso)  inz
018600100517     d wData_ISO_F     s               d   datfmt(*iso)  inz
018700100517     d wDataCorr       s              8  0 inz
018800121029     d wOggi_2         s              8  0 inz
018900100517     d W01deci         s              8  0 inz
019000100517     d W01decf         s              8  0 inz
019100100517     d W01depi         s              8  0 inz
019200100517     d W01depf         s              8  0 inz
019300100517     d wDataI          s              8  0 inz
019400100517     d wDataF          s              8  0 inz
019500110907     d wDay            s              5i 0 inz
019600100517     d wTipEla         s              1    inz
019700100517     d TB85kfil        s              3    inz
019800100517     d TB85key         s              8    inz
019900100517     d TB85des1        s             25    inz
020000100517     d TB85uni         s              1    inz('S')
020100110120
020200110120     d idTabella       s              2
020300110120     d Ordinamento     s              1
020400110120     d idElemento      s              8
020500110120     d TastoUscita     s              1
020600080208
020700100517       //--------------------------------------------------------------
020800100517       //?Definizione procedure.                                       ?
020900100517       //--------------------------------------------------------------
021000100517
021100100517       // -?Reperimento dati utente?
021200100517      /copy gaitrasrc/srcProtoPR,TIBS34R
021300100517
021400100517       // -?Richiamo controllo abilitazioni utente?
021500100518      /copy gaitrasrc/srcProtoPR,TNTAA1C
021600100517
021700110120       // -?Richiamo ricerca/controllo tabelle (TABEL)?
021800110120      /copy gaitrasrc/srcprotopr,trul19r
021900110120
022000100517       // -?Richiamo controllo/inversione date?
022100100518      /copy gaitrasrc/srcProtoPR,XSRDA8
022200100517
022300100517       // -?Richiamo ricerca commerciali?
022400100518      /copy gaitrasrc/srcProtoPR,TRTB85R
022500100518
022600100518       // -?Richiamo ricerca/controllo tabelle (TNTBE)?
022700100518      /copy gaitrasrc/srcProtoPR,TIBS02R
022800100518
022900100518       // -?Richiamo interrogazione/selezione Tipo Attività (tab. "TTA")?
023000100518     d tntb81r1        pr                  extpgm('TNTB81R1')
023100100518     d  kpjba                              likeds(kpjba)
023200100518     d  tntb81ds                           likeds(TNTB81ds)
023300100517
023400100517       // -?Sottomissione lavoro batch?
023500100517      /copy gaitrasrc/srcProtoPR,BCH10
023600100517
023700100517       // -?Richiamo diretto lavoro batch?
023800100519     d trmk88c         pr                  extpgm('TRMK88C')
023900100518     d   kpjba                             likeds(KPJBA)
024000100326
024100110120       // -?Richiamo caricamento filiali/aree gestite dall'utente?
024200110120     d trul31r         pr                  extpgm('TRUL31R')
024300110120     d  kpjba                              likeds(KPJBA)
024400110120     d  trul31ds                           likeds(trul31ds)
024500110120     d  trul31ds2                          likeds(trul31ds2)
024600110120
024700100517       //--------------------------------------------------------------
024800100517       //?Definizione key-list.                                        ?
024900100517       //--------------------------------------------------------------
025000080206
025100100517       // -?File TABEL00F?
025200100517     d k03tabel00    e ds                  extname(TABEL00F:*key)
025300100517     d                                     prefix(k_)  inz
025400080206
025500100517       //--------------------------------------------------------------
025600100517       //?M A I N - L I N E                                            ?
025700100517       //--------------------------------------------------------------
025800080206
025900080206     c     *Entry        plist
026000080206     c                   parm                    KPJBA
026100080206
026200080206      /free
026300080206
026400100517       // -?Operazioni iniziali?
026500100517       exsr  sr_RoutInz;
026600090601
026700100517       // -?Gestione videate?
026800100517       DoW  Not  $Fine;
026900100517         select;
027000100517           when  $Video = 'D1';
027100100517             exsr  sr_GesD01;
027200100517           other;
027300080206             $Fine = *on;
027400100517         endsl;
027500100517       EndDo;
027600080206
027700100517       // -?Operazioni finali?
027800100517       exsr  sr_RoutEnd;
027900080206
028000100517       //--------------------------------------------------------------
028100100517       //?Operazioni iniziali.                                         ?
028200100517       //--------------------------------------------------------------
028300100517       BEGSR  sr_RoutInz;
028400100517
028500100517         *inLR = *on;
028600100517
028700100517         // -?Reperimento dati job?
028800100517         exsr  sr_DatiJob;
028900100517
029000100517         // -?Reperimento data corrente?
029100100517         wDataCorr= %dec( %date() );
029200121029
029300121029         // -?Calcolo oggi - 2 anni?
029400121029         wData_ISO_I = %date(wDataCorr);
029500121029         wData_ISO_I = wData_ISO_I - %years(2);
029600121029         wOggi_2 = %dec(wData_ISO_I);
029700100517
029800100517         // -?Impostazione dati di testata?
029900100517         V01pgm = SDSpgm;
030000080206
030100100517         // -?Impostazione campi "fissi"?
030200100326         k_TBLkut = 1;
030300100211
030400100517         // -?Controllo se utente autorizzato alla funzione?
030500100517         reset  TNTAA1ds;
030600120813         //ITAA1caut = '§utegtc';         (già così)
030700100517         kpjbu     = TNTAA1ds;
030800100517         TNTAA1C ( kpjba );
030900100517         TNTAA1ds = kpjbu;
031000100517         if  oTAA1fabi = 'N';
031100100517           $Error      = *on;
031200100517           ErrMessage  = *on;
031300100517           ErrGenerico = *on;
031400100517           PosCurCmm   = *on;
031500100329           V01msg = $Msg(01);
031600080206           leavesr;
031700080206         endif;
031800100312
031900100517         // -?Caricamento filiali gestite dall'utente?
032000100517         clear  TRUL31ds;
032100110120         clear  TRUL31DS2;
032200100517         I31abi = oTAA1cabi;
032300100312         I31cdi = DUTdis;
032400100312         I31car = DUTare;
032500100312         I31cpo = DUTpou;
032600110120         TRUL31R ( kpjba : trul31ds : trul31ds2);
032700100517         if  O31pog <= *zero;
032800100517           $Error      = *on;
032900100326           ErrMessage  = *on;
033000100326           ErrGenerico = *on;
033100100326           V01msg = $Msg(01);
033200100312           leavesr;
033300100517         endif;
033400110120
033500110120         //?Abilito richiesta per Area
033600110120         DspStpCar = *off;
033700110120         IF  car(2) > *zeros;
033800110120           DspStpCar = *on;
033900110120         ENDIF;
034000100423
034100080206       ENDSR;
034200080206
034300080206       //--------------------------------------------------------------
034400100517       //?Reperimento Dati del job (Utente/Operativi).                 ?
034500080206       //--------------------------------------------------------------
034600100517       BEGSR  sr_DatiJob;
034700080206
034800080206         in(E) §AzUte;
034900080206         if NOT %error;
035000080206           in(E) §DatiUte;
035100080206         endif;
035200080206         if %error or RSut = *blanks;
035300080206           clear TIBS34ds;
035400080206           tibs34r(tibs34ds);
035500080206           in §AzUte;
035600080206           in §DatiUte;
035700080206         endif;
035800080206
035900080206       ENDSR;
036000080206
036100080206       //--------------------------------------------------------------
036200100517       //?Gestione videata D01.                                        ?
036300080206       //--------------------------------------------------------------
036400100517       BEGSR  sr_GesD01;
036500080206
036600100517         // -?Inizializzazione videata?
036700100517         if  $InzD01;
036800100517           exsr  sr_InzD01;
036900080206           $InzD01 = *off;
037000100517         endif;
037100080206
037200100517         // -?Emissione Testata?
037300100517         write  MK87T01;
037400100517         exfmt  MK87D01;
037500100326         ErrMessage   = *off;
037600100326         ErrGenerico  = *off;
037700100326         clear V01msg;
037800100517
037900100517         Select;
038000100326
038100100517           // -?Rilevato errore grave: uscita dal *pgm?
038200100517           When  $Error;
038300100326             $Fine = *on;
038400100326
038500100517           // -?F3=Fine?
038600100517           When  dsp_aid = c_F03;
038700100517             exsr  sr_F03D01;
038800100329
038900100517           // -?Invio / F6=Conferma?
039000100517           Other;
039100100517             exsr  sr_CtrD01;
039200100517             select;
039300100517               when  ErrGenerico;
039400100517                 leavesr;
039500100517               when  dsp_aid = c_F06;
039600100517                 exsr  sr_F06D01;
039700100517             endsl;
039800100329
039900100517         EndSl;
040000080206
040100080206       ENDSR;
040200080206
040300080206       //--------------------------------------------------------------
040400100517       //?Inizializzazione videata D01.                                ?
040500080206       //--------------------------------------------------------------
040600100517       BEGSR  sr_InzD01;
040700100329
040800100517         clear  MK87D01;
040900100517
041000100601         // -?Campi hidden?
041100100526         V01dscmm = 'I';
041200100601         V01cscmm = 'I';
041300100601         V01tscmm = 'I';
041400100601
041500100601         // -?Campi i/o?
041600100526         V01dscmmIE  = 'E';
041700100518         V01dstat = 'O';
041800100526         V01dstat_ds = 'TO   ';
041900080206
042000080206       ENDSR;
042100080206
042200080206       //--------------------------------------------------------------
042300100517       //?Gestione tasto funzionale F3 da videata D01:                 ?
042400100517       //?F3=Fine                                                      ?
042500080206       //--------------------------------------------------------------
042600100517       BEGSR  sr_F03D01;
042700080206
042800100517         // -?Chiusura del *pgm?
042900080206         $Fine = *on;
043000080206
043100080206       ENDSR;
043200100329
043300100329       //--------------------------------------------------------------
043400100517       //?Gestione tasto funzionale F6 da videata D01:                 ?
043500100517       //?F6=Conferma                                                  ?
043600100329       //--------------------------------------------------------------
043700100517       BEGSR  sr_F06D01;
043800100329
043900100517         // -?Impostazione parametri?
044000100517         clear  TRMK88ds;
044100100517         I88tcpe = V01ela;
044200110120         IF  V01car > *zeros;
044300110120           I88car   = %dec(V01car:3:0);
044400110120         ENDIF;
044500100517         if  V01cmm > *zero;
044600100517           I88cmmu  = %int(V01cmm);
044700100517         endif;
044800100517         I88deci    = W01deci;
044900100517         I88decf    = W01decf;
045000100517         I88depi    = W01depi;
045100100517         I88depf    = W01depf;
045200100517         I88dett    = V01dett;
045300100730         //I88dscmm   = V01dscmm;
045400100730         //I88dscmmie = V01dscmmie;
045500100730         if  V01dscmmie = *blank;
045600100730           I88dscmm   = 'O';
045700100730           //I88dscmmie = *blank;           (già così)
045800100730         else;
045900100730           I88dscmm   = 'I';
046000100730           I88dscmmie = V01dscmmie;
046100100730         endif;
046200100517         I88dstat   = V01dstat;
046300100517         I88dstatc  = V01dstat_ds;
046400100517         I88totc    = V01totc;
046500100730         //I88cscmm   = V01cscmm;
046600100730         //I88cscmmie = V01cscmmie;
046700100730         if  V01cscmmie = *blank;
046800100730           I88cscmm   = 'O';
046900100730           //I88cscmmie = *blank;           (già così)
047000100730         else;
047100100730           I88cscmm   = 'I';
047200100730           I88cscmmie = V01cscmmie;
047300100730         endif;
047400100517         I88totg    = V01totg;
047500100730         //I88tscmm   = V01tscmm;
047600100730         //I88tscmmie = V01tscmmie;
047700100730         if  V01tscmmie = *blank;
047800100730           I88tscmm   = 'O';
047900100730           //I88tscmmie = *blank;           (già così)
048000100730         else;
048100100730           I88tscmm   = 'I';
048200100730           I88tscmmie = V01tscmmie;
048300100730         endif;
048400100423         kcoaz = 'MK88';
048500100423         kpjbu = TRMK88ds;
048600100329         kbuff = *blanks;
048700100329
048800100712         if  knmus = *all'1';
048900100517           // -?Richiamo interattivo?
049000100519           trmk88c ( kpjba );
049100100712         else;
049200100517           // -?Sottomissione batch?
049300100712            bch10 ( kpjba );
049400100712         endif;
049500100329
049600100517         // -?Chiusura del *pgm?
049700100329         $Fine = *on;
049800100329
049900100329       ENDSR;
050000090717
050100080206       //--------------------------------------------------------------
050200100517       //?Controllo videata D01.                                       ?
050300080206       //--------------------------------------------------------------
050400100517       BEGSR  sr_CtrD01;
050500100326
050600100517         clear  W01deci;
050700100517         clear  W01decf;
050800100517         clear  W01depi;
050900100517         clear  W01depf;
051000090427
051100100517         %subst(IndDspF : 50) = *off;
051200100728
051300100728         // -?RICERCHE?
051400100730         exsr  sr_Search;
051500100730         if  ErrGenerico;
051600100730           leavesr;
051700100730         endif;
051800100728
051900100728
052000100728
052100100728         // -?Tipo Elaborazione?
052200100728         if  V01ela <> 'C'   and   V01ela <> 'P'   and   V01ela <> 'E';
052300100728           ErrMessage  = *on;
052400100728           ErrGenerico = *on;
052500100728           PosCurEla   = *on;
052600100730           V01msg = $Msg(17);
052700100728           leavesr;
052800100728         endif;
052900100728
053000100728
053100110120       //?Controllo se impostato
053200110120         IF  V01car <> *blanks and V01car <> *zeros;
053300110120        //?Solo numerico
053400110120           IF  %check(C_digits:V01car) > 0;
053500110120             ErrMessage  = *on;
053600110120             ErrGenerico = *on;
053700110120             PosCurCar   = *on;
053800110120             V01msg      = $Msg(23);
053900110120             leavesr;
054000110120           ENDIF;
054100110120         //?Valido in tabella
054200110120           clear ds05;
054300110120           k_TBLcod = '05';
054400110120           k_TBLkey = V01car;
054500110120           chain  %kds(K03tabel00)  TABEL00F;
054600110120           IF  NOT %found(TABEL00F)
054700110120               or  TBLflg <> *blank;
054800110120             ErrMessage  = *on;
054900110120             ErrGenerico = *on;
055000110120             PosCurCar   = *on;
055100110120             V01msg      = $Msg(23);
055200110120             leavesr;
055300110120           ENDIF;
055400110120
055500110120           ds05 = TBLuni;
055600110120
055700110120         //?Utente abilitato all'area
055800110120           IF  %lookup(V01car : car) = 0;
055900110120             ErrMessage  = *on;
056000110120             ErrGenerico = *on;
056100110120             PosCurCar   = *on;
056200110120             V01msg      = $Msg(23);
056300110120             leavesr;
056400110120           ENDIF;
056500110120         ENDIF;
056600110120
056700100326
056800100517         // -?COMMERCIALE?
056900100517         Select;
057000110921
057100110921           //?Se sono in sede e non seleziono ne l'area e ne il commerciale errore
057200110921           WHEN dutpou = 046 and ((V01car = *blanks or V01car = *zeros) and
057300110921             (V01cmm = *blanks or V01cmm = *zeros));
057400110921             ErrMessage  = *on;
057500110921             ErrGenerico = *on;
057600110921             PosCurCmm   = *on;
057700110921             V01msg      = $Msg(11);
057800110921             leavesr;
057900100517
058000110120           //?Non immettere se richiesta l'area
058100110120           WHEN V01car <> *blanks and V01car > *zeros and
058200110120             V01cmm <> *blanks and V01cmm > *zeros;
058300110120             ErrMessage  = *on;
058400110120             ErrGenerico = *on;
058500110120             PosCurCmm   = *on;
058600110120             V01msg      = $Msg(24);
058700110120             leavesr;
058800110120
058900100517           // -?Non immesso?
059000100517           When  V01cmm = *blanks  or  V01cmm = *zeros;
059100100728
059200100728           // -?Ricerca?
059300100730           //  ?(già gestita - vedi subr. "sr_Search")?
059400100728           //When  %scan( '?' : V01cmm ) > *zero;
059500100517
059600100517           // -?Controllo numericità?
059700100517           When  %check( c_Digits : V01cmm ) > *zero;
059800100517             ErrMessage  = *on;
059900100517             ErrGenerico = *on;
060000100517             PosCurCmm   = *on;
060100100517             V01msg = $Msg(02);
060200100517             leavesr;
060300100517
060400100517           // -?Controllo validità?
060500100517           Other;
060600100517             clear ds01;
060700110120             k_TBLcod = '01';
060800100517             k_TBLkey = V01cmm;
060900100517             chain  %kds(K03tabel00)  TABEL00F;
061000100517             if  NOT %found(TABEL00F)   or   TBLflg <> *blank;
061100100517               ErrMessage  = *on;
061200100517               ErrGenerico = *on;
061300100517               PosCurCmm   = *on;
061400100517               V01msg = $Msg(02);
061500100517               leavesr;
061600100517             endif;
061700100517             ds01 = TBLuni;
061800110614
061900110614           // -?No vari o clienti inattivi?
062000110614             IF  §01par <> *blanks;
062100110614               ErrMessage  = *on;
062200110614               ErrGenerico = *on;
062300110614               PosCurCmm   = *on;
062400110614               V01msg = $Msg(02);
062500110614               leavesr;
062600110614             ENDIF;
062700100517
062800100517         EndSl;
062900100326
063000100518         IF  V01cmm > *zero;
063100100518
063200100518           // -?Abilitazione utente al commerciale?
063300100518           reset  TNTAA1ds;
063400100518           //ITAA1caut = '§utegtc';         (già così)
063500100518           ITAA1cmm  = %int( V01cmm );
063600100518           kpjbu     = TNTAA1ds;
063700100518           tntaa1c (kpjba);
063800100518           TNTAA1ds = kpjbu;
063900100518           if  oTAA1fabi = 'N';
064000100518             ErrMessage  = *on;
064100100518             ErrGenerico = *on;
064200100518             PosCurCmm   = *on;
064300100518             V01msg = $Msg(02);
064400100518             leavesr;
064500100518           endif;
064600100326
064700100518           // -?Selezionabili SOLO unificanti?
064800100518           if  %int( V01cmm ) <> §01rgf;
064900100518             ErrMessage  = *on;
065000100518             ErrGenerico = *on;
065100100518             PosCurCmm   = *on;
065200100518             V01msg = $Msg(03);
065300100518             leavesr;
065400100518           endif;
065500100518
065600100518         ENDIF;
065700100728
065800100728
065900100329
066000100518         // -?Controllo immissione date per l'elaborazione richiesta?
066100100518         //  ?(consuntiva e/o preventiva)?
066200100518         select;
066300100518           when  (V01ela = 'C'   or   V01ela = 'E')   and
066400100518                 V01deci = *zero;
066500100518             ErrMessage  = *on;
066600100518             ErrGenerico = *on;
066700100518             PosCurDeci  = *on;
066800100518             V01msg = $Msg(04);
066900100518             leavesr;
067000100518           when  (V01ela = 'P'   or   V01ela = 'E')   and
067100100518                 V01depi = *zero;
067200100518             ErrMessage  = *on;
067300100518             ErrGenerico = *on;
067400100518             PosCurDepi  = *on;
067500100518             V01msg = $Msg(04);
067600100518             leavesr;
067700100518           when  V01ela = 'C'   and   (V01depi + V01depf) > *zero;
067800100518             ErrMessage  = *on;
067900100518             ErrGenerico = *on;
068000100518             PosCurDepi  = *on;
068100100518             V01msg = $Msg(14);
068200100518             leavesr;
068300100518           when  V01ela = 'P'   and   (V01deci + V01decf) > *zero;
068400100518             ErrMessage  = *on;
068500100518             ErrGenerico = *on;
068600100518             PosCurDeci  = *on;
068700100518             V01msg = $Msg(14);
068800100518             leavesr;
068900100518         endsl;
069000100517
069100100518         // -?Controllo date x elaborazione consuntiva?
069200100517         if  V01deci <> *zero   or   V01decf <> *zero;
069300100518           wDataI  = V01deci;
069400100518           wDataF  = V01decf;
069500100517           wTipEla = 'C';
069600100517           exsr  sr_CtrlDate;
069700100517           if  ErrMessage;
069800100517             leavesr;
069900100517           endif;
070000100517         endif;
070100100517
070200100518         // -?Controllo date x elaborazione preventiva?
070300100517         if  V01depi <> *zero   or   V01depf <> *zero;
070400100517           wDataI  = V01depi;
070500100517           wDataF  = V01depf;
070600100517           wTipEla = 'P';
070700100517           exsr  sr_CtrlDate;
070800100517           if  ErrMessage;
070900100517             leavesr;
071000100517           endif;
071100100517         endif;
071200100728
071300100728
071400100518
071500100518         // -?Controllo selezioni x stampa dettaglio agenda?
071600100728         Select;
071700100730
071800100730           When  V01dett = *blank;
071900100730             ErrMessage  = *on;
072000100730             ErrGenerico = *on;
072100100730             PosCurDett  = *on;
072200100730             V01msg = $Msg(18);
072300100730             leavesr;
072400100728           When  V01dett <> 'N'   and   V01dett <> 'S';
072500100728             ErrMessage  = *on;
072600100728             ErrGenerico = *on;
072700100728             PosCurDett  = *on;
072800100730             V01msg = $Msg(19);
072900100728             leavesr;
073000100728
073100100728           // ·?Selezione commerciali?
073200100730           When  V01Dscmmie <> *blank   and   V01Dscmmie <> 'I'
073300100730                                        and   V01Dscmmie <> 'E';
073400100728             ErrMessage  = *on;
073500100728             ErrGenerico = *on;
073600100728             PosCurDscmmie = *on;
073700100730             V01msg = $Msg(20);
073800100728             leavesr;
073900100728
074000100728           // ·?Totali?
074100100728           When V01dett = 'N'  and
074200100728                V01totc = 'N'  and  V01totg = 'N';
074300100728             ErrMessage  = *on;
074400100728             ErrGenerico = *on;
074500100728             PosCurDett  = *on;
074600100728             V01msg = $Msg(10);
074700100728             leavesr;
074800100517
074900100728           // ·?Tipi Attività?
075000100730           //  ?(ricerca già gestita - vedi subr. "sr_Search")?
075100100730           When  V01Dstat = *blank;
075200100730             ErrMessage  = *on;
075300100730             ErrGenerico = *on;
075400100730             PosCurDstat = *on;
075500100730             V01msg = $Msg(21);
075600100730             leavesr;
075700100728           When  V01Dstat <> 'I'   and   V01Dstat <> 'O';
075800100728             ErrMessage  = *on;
075900100728             ErrGenerico = *on;
076000100728             PosCurDstat = *on;
076100100730             V01msg = $Msg(22);
076200100728             leavesr;
076300100728           When  V01dett  = 'S'   and
076400100728                 V01dstat = 'I'   and   V01dstat_ds = *blank;
076500100728             ErrMessage    = *on;
076600100728             ErrGenerico   = *on;
076700100728             PosCurDstat01 = *on;
076800100728             V01msg = $Msg(12);
076900100728             leavesr;
077000100728
077100100728         EndSl;
077200100518
077300100728
077400100728
077500100518         // -?Controllo/ricerca Tipi Attività?
077600100518         If  V01dstat_ds <> *blank;
077700100518           exsr  sr_CtrlTAT;
077800100518           if  ErrGenerico;
077900100518             leavesr;
078000100518           endif;
078100100518         EndIf;
078200100728
078300100728
078400100728
078500100728         // -?Controllo selezioni x stampa totale x commerciale?
078600100728         select;
078700100728
078800100730           when  V01totc = *blank;
078900100730             ErrMessage  = *on;
079000100730             ErrGenerico = *on;
079100100730             PosCurTotC  = *on;
079200100730             V01msg = $Msg(18);
079300100730             leavesr;
079400100728           when  V01totc <> 'N'   and   V01totc <> 'S';
079500100728             ErrMessage  = *on;
079600100728             ErrGenerico = *on;
079700100728             PosCurTotC  = *on;
079800100730             V01msg = $Msg(19);
079900100728             leavesr;
080000100728
080100100728           // ·?Selezione commerciali errata?
080200100730           when  V01Cscmmie <> *blank   and   V01Cscmmie <> 'I'
080300100730                                        and   V01Cscmmie <> 'E';
080400100728             ErrMessage  = *on;
080500100728             ErrGenerico = *on;
080600100728             PosCurCscmmie = *on;
080700100730             V01msg = $Msg(20);
080800100728             leavesr;
080900100728         endsl;
081000100728
081100100728
081200100521
081300100601         // -?Controllo stampa totale generale:?
081400100601         select;
081500100728
081600100730           when  V01totg = *blank;
081700100730             ErrMessage  = *on;
081800100730             ErrGenerico = *on;
081900100730             PosCurTotG  = *on;
082000100730             V01msg = $Msg(18);
082100100730             leavesr;
082200100728           when  V01totg <> 'N'   and   V01totg <> 'S';
082300100728             ErrMessage  = *on;
082400100728             ErrGenerico = *on;
082500100728             PosCurTotG  = *on;
082600100730             V01msg = $Msg(19);
082700100728             leavesr;
082800100601
082900100601           // -?obbligatoria SE non specificato un comm.le unificante?
083000100601           when  V01totg  = 'N'   and   V01cmm <= *zero;
083100100601             ErrMessage   = *on;
083200100601             ErrGenerico  = *on;
083300100601             PosCurTotG   = *on;
083400100601             V01msg = $Msg(15);
083500100601             leavesr;
083600100601
083700100601           // -?NON richiedibile SE specificato un comm.le unificante?
083800100601           when  V01totg  = 'S'   and   V01cmm > *zero;
083900100601             ErrMessage   = *on;
084000100601             ErrGenerico  = *on;
084100100601             PosCurTotG   = *on;
084200100601             V01msg = $Msg(16);
084300100601             leavesr;
084400100728
084500100728           // ·?Selezione commerciali errata?
084600100730           when  V01Tscmmie <> *blank   and   V01Tscmmie <> 'I'
084700100730                                        and   V01Tscmmie <> 'E';
084800100728             ErrMessage  = *on;
084900100728             ErrGenerico = *on;
085000100728             PosCurTscmmie = *on;
085100100730             V01msg = $Msg(20);
085200100728             leavesr;
085300100728
085400100601         endsl;
085500100517
085600080206       ENDSR;
085700100730
085800100730       //--------------------------------------------------------------
085900100730       //?Gestione ricerche in fmt D1.
086000100730       //--------------------------------------------------------------
086100100730       BEGSR  sr_Search;
086200100730
086300100730         Select;
086400110120
086500110120       //?AREA
086600110120           When  %scan('?' : V01car) > *zero;
086700110120             idTabella = '05';
086800110120             clear Ordinamento;
086900110120             clear idElemento;
087000110120             clear TastoUscita;
087100110120             Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
087200110120             V01car = idElemento;
087300110120
087400100730
087500100730           // -?Commerciale?
087600100730           When  %scan( '?' : V01cmm ) > *zero;
087700100730             tb85kfil = %editc(DUTpou:'X');
087800100730             clear tb85key ;
087900100730             clear tb85des1;
088000100730             trtb85r ( kpjba : tb85kfil : tb85key : tb85des1 : tb85Uni );
088100100730             V01cmm  = tb85Key;
088200100730             ErrGenerico = *on;
088300100730             PosCurCmm   = *on;
088400100730             leavesr;
088500100730
088600100730           // -?Tipi Attività?
088700100730           When  %scan( '?' : V01dstat_ds ) > *zero;
088800100730             For  xx = 1  To  %elem($DSTat);
088900100730               if  %scan( '?' : $DSTat(xx) ) > *zero;
089000100730                 clear  $DSTat(xx);
089100100730                 clear  TNTB81ds;
089200100730                 TNTB81R1 ( kpjba : TNTB81ds );
089300100730                 select;
089400100730                   when  oTTAfxx <> *blank;
089500100730                   when  oTTAerr = *on;
089600100730                     ErrMessage  = *on;
089700100730                     V01msg = oTTAmsg;
089800100730                   other;
089900100730                     $DSTat(xx) = %subst( oTTAke1 : 1 :
090000100730                                          %len(V01dstat01) );
090100100730                 endsl;
090200100730                 ErrGenerico = *on;
090300100730                 %subst( IndDspF : (56+xx) : 1 ) = *on;
090400100730                 leavesr;
090500100730               endif;
090600100730             EndFor;
090700100730
090800100730           EndSl;
090900100730
091000100730       ENDSR;
091100100517
091200100517       //--------------------------------------------------------------
091300100517       //?Controllo range di date (elaboraz. consuntiva o preventiva)
091400100517       //--------------------------------------------------------------
091500100517       BEGSR  sr_CtrlDate;
091600100517
091700100517         // -?Data Dal corretta?
091800100517         If  wDataI <> *zero;
091900100517
092000100517           clear WLBdat;
092100100517           G02dat = wDataI;
092200100517           XSRDA8(WLBdat);
092300100517           if  G02err = '1';
092400100517             ErrMessage  = *on;
092500100517             ErrGenerico = *on;
092600100517             if  wTipEla = 'C';
092700100517               PosCurDeci  = *on;
092800100517             else;
092900100517               PosCurDepi  = *on;
093000100517             endif;
093100100517             V01msg = $Msg(05);
093200100517             leavesr;
093300100517           endif;
093400100517
093500100517           if  wTipEla = 'C';
093600100517             V01deci = G02dat;
093700100517             W01deci = G02inv;
093800100517           else;
093900100517             V01depi = G02dat;
094000100517             W01depi = G02inv;
094100100517           endif;
094200100517           wDataI   = G02inv;
094300121029
094400121029           // -?Per Consuntivo la data non può essere inferiore a oggi - 2 anni
094500121029           IF  wDataI < wOggi_2 and wTipEla = 'C';
094600121029             ErrMessage  = *on;
094700121029             ErrGenerico = *on;
094800121029             PosCurDeci  = *on;
094900121029             V01msg = $Msg(05);
095000121029             V01msg = %trim(V01msg) +
095100121029             ' è inferiore a 2 anni, non ci sono i dati';
095200121029             leavesr;
095300121029           ENDIF;
095400100517
095500100517           // -?Controllo data non inferiore al 2000 o superiore al 2039?
095600100517           if  wDataI < 20000101  or  wDataI > 20391231;
095700100517             ErrMessage  = *on;
095800100517             ErrGenerico = *on;
095900100517             if  wTipEla = 'C';
096000100517               PosCurDeci  = *on;
096100100517             else;
096200100517               PosCurDepi  = *on;
096300100517             endif;
096400100517             V01msg = $Msg(05);
096500100517             leavesr;
096600100517           endif;
096700100517
096800100517         EndIf;
096900100517
097000100517         // -?Se impostata data "DAL" e non "AL" =>?
097100100517         //  ?imposta "AL" = data del giorno?
097200100517         if  wDataI > *zero  and  wDataF = *zero;
097300100518           wDataF = *date;
097400100517         endif;
097500100517
097600100517         // -?Data Al corretta?
097700100517         If  wDataF <> *zero;
097800100517           clear WLBdat;
097900100517           G02dat = wDataF;
098000100517           XSRDA8(WLBdat);
098100100517           if  G02err = '1';
098200100517             ErrMessage  = *on;
098300100517             ErrGenerico = *on;
098400100517             if  wTipEla = 'C';
098500100517               PosCurDecf  = *on;
098600100517             else;
098700100517               PosCurDepf  = *on;
098800100517             endif;
098900100517             V01msg = $Msg(05);
099000100517             leavesr;
099100100517           endif;
099200100517
099300100517           if  wTipEla = 'C';
099400100517             V01decf = G02dat;
099500100517             W01decf = G02inv;
099600100517           else;
099700100517             V01depf = G02dat;
099800100517             W01depf = G02inv;
099900100517           endif;
100000100517           wDataF = G02inv;
100100100517
100200100517           // -?Controllo data non inferiore al 2000 o superiore al 2039?
100300100517           if  wDataF < 20000101  or  wDataF > 20391231;
100400100517             ErrMessage  = *on;
100500100517             ErrGenerico = *on;
100600100517             if  wTipEla = 'C';
100700100517               PosCurDecf  = *on;
100800100517             else;
100900100517               PosCurDepf  = *on;
101000100517             endif;
101100100517             V01msg = $Msg(05);
101200100517             leavesr;
101300100517           endif;
101400100517
101500100517         EndIf;
101600100517
101700100517         // -?Data "DAL" congruente con "AL"?
101800100517         select;
101900100517           when  wDataI > *zero  and  wDataF = *zero;
102000100517             ErrMessage  = *on;
102100100517             ErrGenerico = *on;
102200100517             if  wTipEla = 'C';
102300100517               PosCurDeci  = *on;
102400100517             else;
102500100517               PosCurDepi  = *on;
102600100517             endif;
102700100517             V01msg = $Msg(06);
102800100517             leavesr;
102900100517           when  wDataI > *zero  and  wDataF > *zero  and
103000100517                 wDataI > wDataF;
103100100517             ErrMessage  = *on;
103200100517             ErrGenerico = *on;
103300100517             if  wTipEla = 'C';
103400100517               PosCurDeci  = *on;
103500100517             else;
103600100517               PosCurDepi  = *on;
103700100517             endif;
103800100517             V01msg = $Msg(06);
103900100517             leavesr;
104000100517           when  wTipEla = 'C'   and   wDataF >= wDataCorr;
104100100517             ErrMessage  = *on;
104200100517             ErrGenerico = *on;
104300100517             PosCurDecf  = *on;
104400100517             V01msg = $Msg(08);
104500100517             leavesr;
104600100517           when  wTipEla = 'P'   and   wDataI < wDataCorr;
104700100517             ErrMessage  = *on;
104800100517             ErrGenerico = *on;
104900100517             PosCurDepi  = *on;
105000100517             V01msg = $Msg(09);
105100100517             leavesr;
105200100517         endsl;
105300100517
105400100908         // -?Range massimo di 65 giorni?
105500100517         wData_Iso_I = %date(wDataI);
105600100517         wData_Iso_F = %date(wDataF);
105700100517         wDay = %diff( wData_ISO_F : wData_ISO_I : *days );
105800100908         if  wDay > 65;
105900100517           ErrMessage  = *on;
106000100517           ErrGenerico = *on;
106100100517           if  wTipEla = 'C';
106200100517             PosCurDeci  = *on;
106300100517           else;
106400100517             PosCurDepi  = *on;
106500100517           endif;
106600100517           V01msg = $Msg(07);
106700100517           leavesr;
106800100517         endif;
106900100517
107000100517       ENDSR;
107100100518
107200100518       //--------------------------------------------------------------
107300100518       //?Controllo Tipi Attività inseriti
107400100518       //--------------------------------------------------------------
107500100518       BEGSR  sr_CtrlTAT;
107600100518
107700100518         FOR  xx = 1  TO  %elem($DSTat);
107800100518
107900100518           Select;
108000100518
108100100518             // -?Vuoto?
108200100518             When  $DSTat(xx) = *blank;
108300100518               iter;
108400100728
108500100728             // -?Richiesta interrogazione?
108600100728             //  ?(già gestita all'inizio della subr. sr_CtrD01)?
108700100728             //When  %scan( '?' : $DSTat(xx) ) > *zero;
108800100518
108900100518             // -?Controllo?
109000100518             Other;
109100100518               reset  TIBS02ds;
109200100518               //T02mod = 'C';            (già così)
109300100518               //T02cod = 'TTA';          (già così)
109400100518               T02ke1 = $DSTat(xx);
109500100518               T02sif = KNSIF;
109600100518               TNTBE_RicercaControllo ( kpjba : tibs02ds );
109700100518               if  T02err <> *blank;
109800100518                 ErrMessage  = *on;
109900100518                 ErrGenerico = *on;
110000100518                 %subst( IndDspF : (56+xx) : 1 ) = *on;
110100100518                 V01msg = $Msg(13);
110200100518                leavesr;
110300100518               endif;
110400100518
110500100518           EndSl;
110600100518
110700100518         ENDFOR;
110800100518
110900100518       ENDSR;
111000080206
111100080206       //--------------------------------------------------------------
111200080206       //?Operazioni finali.
111300080206       //--------------------------------------------------------------
111400100517       BEGSR  sr_RoutEnd;
111500100518
111600100518         clear  TIBS02ds;
111700100518         T02tla = 'C';
111800100518         TNTBE_RicercaControllo ( kpjba : tibs02ds );
111900080206
112000080206         return;
112100080206
112200080206       ENDSR;
112300080206
112400080206      /end-free
112500100329
112600080206       //--------------------------------------------------------------
112700080206       //?Schiere a tempo di compilazione.
112800080206       //--------------------------------------------------------------
112900080206
113000100517** - $Msg:?Messaggi di Errore?-----------------------------------------------*
113100100329Utente non abilitato alla funzione                                            01
113200100329Codice commerciale errato o non in gestione all'utente                        02
113300100329Richiedere solo commerciali UNIFICANTI!                                       03
113400100518Impostare il periodo da elaborare                                             04
113500100517Data errata                                                                   05
113600100517Data "DAL" incongruente con data "AL"                                         06
113700100908Periodo di elaborazione troppo grande (maggiore di 65 giorni)                 07
113800100517Data finale elaborazione consuntiva deve essere minore di oggi                08
113900100518Data iniziale elaborazione preventiva deve essere maggiore/uguale ad oggi     09
114000100518Selezionare almeno una stampa                                                 10
114100110921Selezionare almeno un'area o un commerciale                                   11
114200100518Includere almeno un tipo attività                                             12
114300100518Tipo attività errato                                                          13
114400100518Periodo NON richiesto dal tipo elaborazione specificato                       14
114500100521Totali generali obbligatori SE NON specificato un commerciale unificante      15
114600100601Non richiedere il totale generale per la stampa di un solo comm.le unificante 16
114700100730Tipo elaborazione errato. Valori ammessi: "C", "P", "E".                      17
114800100730Richiesta di stampa obbligatoria. Valori ammessi: "S", "N".                   18
114900100730Richiesta di stampa errata. Valori ammessi: "S", "N".                         19
115000100730Selezione commerciali errata. Valori ammessi: "I", "E", "_".                  20
115100100730Inclusione/Omissione tipi attività obbligatoria. Valori ammessi: "I", "O".    21
115200100730Inclusione/Omissione tipi attività errata. Valori ammessi: "I", "O".          22
115300110120Codice Area errato o non in gestione all'utente                               23
115400110921Non impostare il commerciale se inserito il codice area                       24
