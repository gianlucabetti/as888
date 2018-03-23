000100100304       //==============================================================
000200100204       //?TNTB79R1 * Selezione tab. "3EW" = Dati EasyWeb x "postazione"?
000300100304       //==============================================================
000400100304
000500100304       //--------------------------------------------------------------
000600100304       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100304       //--------------------------------------------------------------
000800100304
000900100330     /*PRM dbgview(*source)
001000100304     /*END
001100100304
001200100304       //--------------------------------------------------------------
001300100304       //?Specifiche di controllo.                                     ?
001400100304       //--------------------------------------------------------------
001500100204
001600100204     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100204     h dftactgrp(*no) actgrp(*caller)
001800100204
001900100204       //--------------------------------------------------------------
002000100204       //?Dichiarazione file.                                          ?
002100100204       //--------------------------------------------------------------
002200100204
002300100204       // -?Tabelle?
002400100215     fTNTBE01L  if   e           k disk
002500100204
002600100204       // -?Video?
002700100204     fTNTB79D1  cf   e             workstn
002800100204     f                                     sfile(TB79S01 : S01nrr)
002900100204     f                                     indds(IndDspF)
003000100204     f                                     infds(InfDspF)
003100100204
003200100204       //--------------------------------------------------------------
003300100204       //?Definizione costanti.                                        ?
003400100204       //--------------------------------------------------------------
003500100204
003600100204       // -?Codice tabella in gestione?
003700100204     d c_Tab           c                   const('3EW')
003800100204
003900100204       // -?Numero di record in ciascuna videata di subfile?
004000100204     d c_SflPag        c                   const(15)
004100100204
004200100204       // -?Numero massimo di record gestibili?
004300100204     d c_MaxRec        c                   const(9999)
004400100204
004500100204       // -?Tasti funzionali a video?
004600100204     d c_F01           c                   const(x'31')
004700100204     d c_F02           c                   const(x'32')
004800100204     d c_F03           c                   const(x'33')
004900100204     d c_F04           c                   const(x'34')
005000100204     d c_F05           c                   const(x'35')
005100100204     d c_F06           c                   const(x'36')
005200100204     d c_F07           c                   const(x'37')
005300100204     d c_F08           c                   const(x'38')
005400100204     d c_F09           c                   const(x'39')
005500100204     d c_F10           c                   const(x'3A')
005600100204     d c_F11           c                   const(x'3B')
005700100204     d c_F12           c                   const(x'3C')
005800100204     d c_F13           c                   const(x'B1')
005900100204     d c_F14           c                   const(x'B2')
006000100204     d c_F15           c                   const(x'B3')
006100100204     d c_F16           c                   const(x'B4')
006200100204     d c_F17           c                   const(x'B5')
006300100204     d c_F18           c                   const(x'B6')
006400100204     d c_F19           c                   const(x'B7')
006500100204     d c_F20           c                   const(x'B8')
006600100204     d c_F21           c                   const(x'B9')
006700100204     d c_F22           c                   const(x'BA')
006800100204     d c_F23           c                   const(x'BB')
006900100204     d c_F24           c                   const(x'BC')
007000100204     d c_Enter         c                   const(x'F1')
007100100204     d c_RollDown      c                   const(x'F4')
007200100204     d c_RollUp        c                   const(x'F5')
007300100204
007400100204       //--------------------------------------------------------------
007500100204       //?Definizione schiere.                                         ?
007600100204       //--------------------------------------------------------------
007700100204
007800100204       // -?Messaggi di errore?
007900100204     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
008000100204
008100100204       //--------------------------------------------------------------
008200100204       //?Definizione aree dati.                                       ?
008300100204       //--------------------------------------------------------------
008400100204
008500100204       // -?Dati utente?
008600100204     d §AzUte        e ds                  extname(AZUTE00F)
008700100204     d                                     dtaara
008800100204     d §DatiUte      e ds                  extname(dDatiUte)
008900100204     d                                     dtaara
009000100204
009100100204       //--------------------------------------------------------------
009200100204       //?Definizione strutture dati.                                  ?
009300100204       //--------------------------------------------------------------
009400100204
009500100204       // -?Status ds?
009600100204     d Status         sds
009700100204     d   SDSpgm          *proc
009800100204     d*//SDSprm          *parms
009900100204
010000100204       // -?InfDS?
010100100204     d InfDspF         ds
010200100204     d   dsp_aid             369    369a
010300100204     d   sfl_rrn             376    377i 0
010400100204     d   min_nrr             378    379i 0
010500100204     d   num_rcds            380    381i 0
010600100204
010700100204       // -?Indicatori su DspF?
010800100204     d IndDspF         ds                  inz
010900100204         // -?Abilitazione tasti funzionali?
011000100204     d   F3Attivo                      n   overlay(IndDspF : 03)
011100100204     d   F12Attivo                     n   overlay(IndDspF : 12)
011200100204         // -?Emissione messaggio di errore?
011300100204     d   ErrMessage                    n   overlay(IndDspF : 28)
011400100204         // -?Indicatori di gestione del subfile?
011500100204     d   SflDsp_N                      n   overlay(IndDspF : 30)
011600100204     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011700100204     d   SflNxtChg                     n   overlay(IndDspF : 32)
011800100204     d   SflEnd                        n   overlay(IndDspF : 33)
011900100204         // -?Indicatori per Attibuti di visualizzazione?
012000100204         //  ?o   Condizionamento visualizzazione campi?
012100100312     d   No_Posiz                      n   overlay(IndDspF : 40)
012200100204     d   Ord_x_SUN                     n   overlay(IndDspF : 41)
012300120814     d   AnnullApplic                  n   overlay(IndDspF : 42)
012400100204         // -?Posizionamento cursore & segnalazione errore?
012500100204     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012600100204         //   -?Riemissione videata?
012700100204     d   ErrGenerico                   n   overlay(IndDspF : 99)
012800100204
012900100204       // -?Parametri ricevuti?
013000100204     d KPJBA         e ds
013100100204     d TNTB79ds      e ds                  inz
013200100204
013300100204       // -?Parametri per Reperimento dati utente?
013400100204     d TIBS34ds      e ds                  inz
013500100204
013600100204       // -?Tab. 3EW = Dati assegnati alla "postazione" in abilitazione?
013700100204       //             ?ad EasyWeb?
013800100204     d d3EW          e ds                  inz
013900100204
014000100204       //--------------------------------------------------------------
014100100204       //?Definizione variabili globali.                               ?
014200100204       //--------------------------------------------------------------
014300100204
014400100204       // -?Flags booleani?
014500100204     d $Fine           s               n   inz
014600100205     d $EoF            s               n   inz
014700100204     d $InzS01         s               n   inz(*on)
014800100204     d $RecOK          s               n   inz
014900100204
015000100204       // -?Parametri di input facoltativi?
015100100204     d p_TNTB79ds      s                   like(TNTB79ds)
015200100204
015300100204       // -?Variabili per la gestione del video?
015400100204     d $Video          s              2    inz('S1')
015500100204     d W_SflPag        s              3  0 inz
015600100204     d wPag            s              4  0 inz
015700100204     d wRig            s              3  0 inz
015800100204     d S01nrr          s                   like(C1RcdNbr) inz
015900100204     d SavS01csr       s                   like(C1RcdNbr) inz
016000100204
016100100204       // -?Variabili per la gestione del singolo campo alla variazione?
016200100204       //  ?dello stesso?
016300100204     d SavC1Cksu       s                   like(C1Cksu)   inz(*hival)
016400100204     d SavC1Csun       s                   like(C1Csun)   inz(*hival)
016500100204
016600100204       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
016700100204
016800100204       //--------------------------------------------------------------
016900100204       // -?Constants?
017000100204       //--------------------------------------------------------------
017100100204       // -?MaxKey - Maximum number of key fields allowed by this program?
017200100204     d MaxKey          c                   const(2)
017300100204       // -?Sort order:?
017400100204       //  ?1 = Sort in an ascending sequence?
017500100204       //  ?2 = Sort in a descending sequence?
017600100204     d Ascendente      c                   const(1)
017700100204     d Discendente     c                   const(2)
017800100204       // -?Key data type:?
017900100204       //  ? 0 = Signed binary?
018000100204       //  ? 2 = Signed zoned decimal?
018100100204       //  ? 3 = Signed packed decimal?
018200100204       //  ? 6 = Character with no national language sort sequence applied,?
018300100204       //  ?     if specified?
018400100204       //  ? 7 = Unsigned packed decimal?
018500100204       //  ?     All numbers will have the sign forced positive ('F'X).?
018600100204       //  ? 8 = Unsigned zoned decimal?
018700100204       //  ?     All numbers will have the sign forced positive ('F'X).?
018800100204       //  ? 9 = Unsigned binary?
018900100204       //  ?14 = Date in form of DD/MM/YY?
019000100204       //  ?15 = Date in form of DD.MM.YYYY?
019100100204     d Numero          c                   const(2)
019200100204     d Carattere       c                   const(6)
019300100204       //
019400100204     d Put             c                   const(1)
019500100204     d EndPut          c                   const(2)
019600100204     d Get             c                   const(3)
019700100204
019800100204       //--------------------------------------------------------------
019900100204       // -?Data Structures?
020000121008       //  ?SflRcd     - The entire subfile record to pass to the sort?
020100121008       //  ?QLGSCB     - The sort request block for the QLGSORT API?
020200121008       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
020300121008       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
020400121008       //  ?QUSEC      - Error structure for the QLGSORT API?
020500100204       //--------------------------------------------------------------
020600100204     d SflRcd          ds                  inz
020700100205     d  S1Cksu                             inz
020800100205     d  S1Dksu                             inz
020900100205     d  S1Csun                             inz
021000100331     d  S1Copz                             inz
021100100331     d  S1Caew                             inz
021200100331     d  S1Clnp                             inz
021300100331     d  S1Cfls                             inz
021400100331     d  S1Cnrs                             inz
021500100331     d  S1Cnsi                             inz
021600100331     d  S1Cnsf                             inz
021700100331     d  S1Cctm                             inz
021800120921     d  H1Cfaa                             inz
021900100204     d  Selected                      1a   inz
022000100204      /copy QSYSINC/QRPGLESRC,QLGSORT
022100100204     d QLGKL                         16    dim(MaxKey)
022200100204     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
022300100204     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
022400100204     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
022500100204     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
022600100204      /copy QSYSINC/QRPGLESRC,QLGSRTIO
022700100204      /copy QSYSINC/QRPGLESRC,QUSEC
022800100204
022900100204       //--------------------------------------------------------------
023000100204       // -?Standalone fields?
023100121008       //  ?SflRcd     - The entire subfile record to pass to the sort?
023200121008       //  ?Nrr        - Relative record number for subfile?
023300121008       //  ?SizeList   - Size of list?
023400121008       //  ?ReturnSize - Size of list returned by sort API?
023500100204       //--------------------------------------------------------------
023600100204     d NotUsed         s             16a   inz
023700100204     d ReturnSize      s              9b 0 inz
023800100204     d SizeList        s              9b 0 inz
023900100204     d RrnLast         s              5i 0 inz
024000100204     d Nrr             s              5i 0 inz
024100100204
024200100204       //--------------------------------------------------------------
024300100204       //?Definizione prototipi procedure.                             ?
024400100204       //--------------------------------------------------------------
024500100204
024600100204       // -?Reperimento dati utente?
024700100204      /copy gaitrasrc/srcProtoPR,TIBS34R
024800100204
024900100204       // -?Controllo/Decodifica cliente?
025000100217      /copy gaitrasrc/srcProtoPI,TIBS69R
025100100204      /copy gaitrasrc/srcProtoPR,TIBS69R
025200100205
025300100205       // -?Ordinamento subfile?
025400100205      /copy gaitrasrc/srcProtoPr,QLGSRTIO
025500100204
025600100204       //--------------------------------------------------------------
025700100204       //?Definizione key-list.                                        ?
025800100204       //--------------------------------------------------------------
025900100204
026000100204       // -?File TNTBE01L?
026100100204     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
026200100204     d                                     prefix(k_)   inz
026300100204
026400100204       //--------------------------------------------------------------
026500100204       //?Riepilogo indicatori utilizzati.                             ?
026600100204       //--------------------------------------------------------------
026700100204
026800100204
026900100204       //--------------------------------------------------------------
027000100204       //?M A I N - L I N E                                            ?
027100100204       //--------------------------------------------------------------
027200100204
027300100204     c     *Entry        plist
027400100204     c                   parm                    KPJBA
027500100204     c                   parm                    p_TNTB79ds
027600100204
027700100204      /free
027800100204
027900100204       // -?Operazioni iniziali?
028000100204       exsr sr_RoutInz;
028100100204
028200100204       // -?Ciclo di gestione del file video?
028300100204       DoW  $Fine = *off;
028400100204         select;
028500100204           // -?Gestione videata S1 (subfile)?
028600100204           when $Video = 'S1';
028700100204             exsr sr_GesS01;
028800100204           other;
028900100204             $Fine = *on;
029000100204         endsl;
029100100204       enddo;
029200100204
029300100204       // -?Operazioni finali?
029400100204       exsr sr_RoutEnd;
029500100204
029600100204       //--------------------------------------------------------------
029700100204       //?*InzSR.                                                      ?
029800100204       //--------------------------------------------------------------
029900100204       BEGSR  *InzSr;
030000100204
030100100204         // -?Reperimento dati job?
030200100204         exsr  sr_DatiJob;
030300100204
030400100204         // -?Impostazione nome programma a video?
030500100204         V1Tpgm = SDSpgm;
030600100204
030700100204       ENDSR;
030800100204
030900100204       //--------------------------------------------------------------
031000100204       //?Reperimento Dati del job (Utente/Operativi).                 ?
031100100204       //--------------------------------------------------------------
031200100204       BEGSR  sr_DatiJob;
031300100204
031400100204         in(e) §AzUte;
031500100204         if NOT %error;
031600100204           in(e) §DatiUte;
031700100204         endif;
031800100204         if %error or RSut = *blank;
031900100204           tibs34r ( tibs34ds );
032000100204           in §AzUte;
032100100204           in §DatiUte;
032200100204         endif;
032300100204
032400100204       ENDSR;
032500100204
032600100204       //--------------------------------------------------------------
032700100204       //?Operazioni iniziali.                                         ?
032800100204       //--------------------------------------------------------------
032900100204       BEGSR  sr_RoutInz;
033000100204
033100100204         // -?Verifica se ricevuti parametri?
033200100204         if  %parms() > 1;
033300100204           TNTB79ds = p_TNTB79ds;
033400100204         else;
033500100204           clear  TNTB79ds;
033600100204         endif;
033700100204
033800100204         // -?Impostazione iniziale parametri di output?
033900100211         clear  o3EWksu;
034000100211         clear  o3EWsun;
034100100204         clear  o3EWuni;
034200100204         clear  o3EWfxx;
034300100204         o3EWerr = *off;
034400100204         clear  o3EWmsg;
034500100204
034600100204         // -?Impostazione chiusura?
034700100204         select;
034800100204           when  %parms() > 1   and   i3EWtla = *blank;
034900100204             *inRT = *on;
035000100204           when  %parms() > 1   and   i3EWtla = 'L';
035100100204             *inLR = *on;
035200100204           when  %parms() > 1   and   i3EWtla = 'C';
035300100204             *inLR = *on;
035400100204             $Fine = *on;
035500100204             leavesr;
035600100204           other;
035700100204             *inLR = *on;
035800100204         endsl;
035900100204
036000100204         // -?Impostazione iniziale / pulizia dei campi chiave?
036100100205         Ord_x_SUN = *off;
036200100204         clear  k05tntbe01;
036300100204         k_TBEcod = c_Tab;
036400100204         select;
036500100204           // -?Richiamato senza parametri?
036600100204           when  %parms() = 1;
036700100204           // -?Richiamato senza né KSU né SUN?
036800100204           when  %parms() > 1   and   i3EWksu =  *blank
036900100204                                and   i3EWsun =  *blank;
037000100204           // -?Richiamato con entrambi (KSU e SUN)?
037100100204           when  %parms() > 1   and   i3EWksu <> *blank
037200100204                                and   i3EWsun <> *blank;
037300100204             k_TBEke1 = i3EWksu;
037400100204             k_TBEke2 = i3EWsun;
037500100204           // -?Richiamato con solo KSU?
037600100204           when  %parms() > 1   and   i3EWksu <> *blank
037700100204                                and   i3EWsun =  *blank;
037800100204             k_TBEke1 = i3EWksu;
037900100204           // -?Richiamato con solo SUN?
038000100204           when  %parms() > 1   and   i3EWksu =  *blank
038100100204                                and   i3EWsun <> *blank;
038200100205             k_TBEke2  = i3EWsun;
038300100205             Ord_x_SUN = *on;
038400100204         endsl;
038500100204
038600100204         // -?Re-impostazione flags booleani (fosse mai il 2° richiamo)?
038700100204         reset  $Fine;
038800100204         reset  $Video;
038900100204         reset  $InzS01;
039000100205         reset  $EoF;
039100100204
039200100204       ENDSR;
039300100204
039400100204       //--------------------------------------------------------------
039500100204       //?Gestione subfile S01.                                        ?
039600100204       //--------------------------------------------------------------
039700100204       BEGSR  sr_GesS01;
039800100204
039900100204         // -?Inizializzazione videata?
040000100204         if  $InzS01 = *on;
040100100204           exsr  sr_InzS01;
040200100204           $InzS01 = *off;
040300100204         endif;
040400100204
040500100204         // -?Emissione Testata & Piede?
040600100204         write  TB79T01;
040700100204         write  TB79P01;
040800100204
040900100204         // -?Posizionamento cursore?
041000100204         if  C1CsrRrn > *zero;
041100100204           C1RcdNbr = C1CsrRrn;
041200100204         else;
041300100204           C1RcdNbr = 1;
041400100204           write  TB79S00;             //?(no rec.)?
041500100204         endif;
041600100204
041700100204         // -?Emissione videata?
041800100204         exfmt  TB79C01;
041900100204
042000100204         reset  ErrMessage;
042100100204         reset  ErrGenerico;
042200100204         clear  V1Dmsg;
042300100204
042400100211         clear  o3EWksu;
042500100211         clear  o3EWsun;
042600100204         clear  o3EWuni;
042700100204
042800100204         select;
042900100204
043000100204           // -?F3=Fine?
043100100204           when  dsp_aid = c_F03;
043200100204             o3EWfxx = '1';
043300100204             $Fine = *on;
043400100204
043500100204           // -?F5=Rivisualizza?
043600100204           when  dsp_aid = c_F05;
043700100204             $InzS01 = *on;
043800100331
043900121008           // -?F8=Ordinamento?
044000121008           when  dsp_aid = c_F08;
044100121008             exsr  sr_F08S01;
044200100204
044300100204           // -?F12=Ritorno?
044400100204           when  dsp_aid = c_F12;
044500100204             o3EWfxx = '2';
044600100204             $Fine = *on;
044700100204
044800100204           // -?SubFile vuoto?
044900100204           when  S01nrr = *zero;
045000100204
045100100204           // -?Invio?
045200100204           other;
045300100204             // -?Gestione opzioni?
045400100331             exsr  sr_OpzS01;
045500100331             if  ErrGenerico;
045600100331               leavesr;
045700100331             endif;
045800100204
045900100204             // -?Richiesto posizionamento?
046000100204             if  (Not Ord_x_SUN   and   C1Cksu <> SavC1Cksu)   OR
046100100204                 (Ord_x_SUN       and   C1Csun <> SavC1Csun);
046200100204               $InzS01 = *on;
046300100204             endif;
046400100204
046500100204         endsl;
046600100204
046700100204       ENDSR;
046800100204
046900100204       //--------------------------------------------------------------
047000100204       //?Inizializzazione subfile S01.                                ?
047100100204       //--------------------------------------------------------------
047200100204       BEGSR  sr_InzS01;
047300100204
047400100204         // -?Spegnimento degli indicatori di errore?
047500100204         %subst(IndDspF : 50) = *off;
047600100312
047700100312         // -?Impostazione indicatore x la gestione del posizionamento?
047800100312         //  ?nel subfile?
047900100312         No_Posiz = (%parms() > 1   and
048000100312                    (i3EWksu <> *blank  or  i3EWsun <> *blank));
048100100204
048200100204         // -?Settaggio indicatori dei tasti funzionali?
048300100204         exsr  sr_SetIndP01;
048400100331
048500100331         // -?Pulizia subfile control?
048600100331         if  Ord_x_SUN;
048700100331           clear  C1Cksu;
048800100331           reset  SavC1Cksu;
048900100331         else;
049000100331           clear  C1Csun;
049100100331           reset  SavC1Csun;
049200100331         endif;
049300100204
049400100204         // -?Pulizia subfile?
049500100204         SflDsp_N    = *on;
049600100204         SflDspCtl_N = *on;
049700100204         write  TB79C01;
049800100204         SflDspCtl_N = *off;
049900100204         SflEnd      = *off;
050000100204
050100100204         clear  W_SflPag;
050200100204         clear  C1RcdNbr;
050300100204         clear  C1CsrRrn;
050400100204         clear  S01nrr;
050500100204         clear  V1Dmsg;
050600100204         ErrMessage  = *off;
050700100204         ErrGenerico = *off;
050800100204
050900100205         // -?Caricamento completo dei dati nel subfile?
051000100205         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
051100100205         //  ?l'eventuale ordinamento)?
051200100204         setll  %kds(k05tntbe01)  TNTBE000;
051300100204         reade  %kds(k05tntbe01 : 1)  TNTBE000;
051400100205         $EoF = (%eof(TNTBE01L));
051500100205         exsr  sr_CaricaS01;
051600100204
051700100204         // -?Impaginazione subfile?
051800100204         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
051900100204         if S01nrr > *zero;
052000100204           C1RcdNbr  = 1;
052100100204           C1CsrRrn  = 1;
052200100204         else;
052300100204           clear C1RcdNbr;
052400100204         endif;
052500100204
052600121008         // -?Ordinamento subfile (se premuto F8)?
052700100204         if  Ord_x_SUN   and   S01nrr >  *zero;
052800100204           exsr  sr_SortSfl;
052900100204         endif;
053000100204
053100100204       ENDSR;
053200100204
053300100204       //--------------------------------------------------------------
053400100205       //?Caricamento completo del subfile S01                         ?
053500100204       //--------------------------------------------------------------
053600100205       BEGSR  sr_CaricaS01;
053700100204
053800100205         clear  S01nrr;
053900100204         SflNxtChg = *off;
054000100204
054100100204         // -?Ciclo di caricamento dati da tab. "3EW"?
054200100205         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
054300100205         //  ?l'eventuale ordinamento)?
054400100205         DoW  Not $EoF;
054500100204
054600100204           // -?Selezione singolo record file TNTBE00F?
054700100204           exsr  sr_SelezRec;
054800100205           if  $EoF;
054900100205             leave;
055000100205           endif;
055100100204
055200100204           // -?Registrazione singolo record nel subfile?
055300100204           if  $RecOK;
055400100204
055500100205             // ·?Impostazione campi?
055600100204             clear  TB79S01;
055700100204             d3EW = TBEuni;
055800100204             S1Cksu = TBEke1;
055900100204             S1Csun = TBEke2;
056000100204             S1Caew = §3EWaew;
056100100204             S1Clnp = §3EWlnp;
056200100204             S1Cfls = §3EWfls;
056300100204             S1Cnrs = §3EWnrs;
056400100204             S1Cnsi = §3EWnsi;
056500100204             S1Cnsf = §3EWnsf;
056600100204             S1Cctm = §3EWctm;
056700120814             H1Cfaa = §3EWfaa;
056800100204
056900100205             // ·?Decodifica cliente unificante?
057000100204             clear  TIBS69ds;
057100100204             I69kcc = DUTkci;
057200100204             I69kac = %int( %subst( S1Cksu : 2 ) );
057300100204             I69sif = knsif;
057400100204             tibs69r ( TIBS69ds :
057500100204                       ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
057600100305             if  O69err = *blank;
057700100204               S1Dksu = %subst(ACOrag : 1 : %len(S1Dksu));
057800100204             endif;
057900100204
058000120814             // ·?Impostazione indicatori?
058100120814             AnnullApplic = (H1Cfaa = 'A');
058200120814
058300100205             // ·?Scrittura record nel subfile?
058400100204             S01nrr += 1;
058500100204             write  TB79S01;
058600100204
058700100204           EndIf;
058800100204
058900100204           // -?Lettura record successivo?
059000100204           reade  %kds(k05tntbe01 : 1)  TNTBE000;
059100100205           $EoF = (%eof(TNTBE01L));
059200100204
059300100204         EndDo;
059400100204
059500100204         // -?Memorizzazione posizionamento effettuato?
059600100204         if  Not Ord_x_SUN;
059700100204           SavC1Cksu = C1Cksu;
059800100204         else;
059900100204           SavC1Csun = C1Csun;
060000100204         endif;
060100100204
060200100204         // -?Visualizzazione del SFL (se ci sono dati)?
060300100204         SflDsp_N = (S01nrr <= *zero);
060400100204
060500100204         // -?Attivazione del SFLEND?
060600100205         SflEnd = ( $EoF );
060700100204
060800100205         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
060900100204         if  S01nrr > *zero;
061000100205           C1RcdNbr = 1;
061100100204         else;
061200100204           clear  C1RcdNbr;
061300100204         endif;
061400100204
061500100204         C1CsrRrn = C1RcdNbr;
061600100204
061700100204       ENDSR;
061800100204
061900100204       //--------------------------------------------------------------
062000100204       //?Settaggio indicatori utilizzati in C01/P01.                  ?
062100100204       //--------------------------------------------------------------
062200100204       BEGSR  sr_SetIndP01;
062300100204
062400100204         // -?F3=Fine?
062500100204         F3Attivo  = (%parms() = 1);
062600100204
062700100204         // -?F12=Ritorno?
062800100204         F12Attivo = (%parms() > 1);
062900100204
063000100204       ENDSR;
063100100204
063200100204       //--------------------------------------------------------------
063300100204       //?Selezione del singolo record letto dalla tabella "3EW".      ?
063400100204       //--------------------------------------------------------------
063500100204       BEGSR  sr_SelezRec;
063600100204
063700100204         reset  $RecOK;
063800100204
063900100204         d3EW = TBEuni;
064000100204
064100100204         select;
064200100204
064300100205           // -?Selezione per cliente unificante?
064400100204           when  i3EWksu <> *blank   and   i3EWksu <> TBEke1;
064500100205             $EoF = *on;
064600100204             leavesr;
064700100204
064800100205           // -?Selezione per strategi user number?
064900100204           when  i3EWsun <> *blank   and   i3EWsun <> TBEke2;
065000100205             $EoF = *on;
065100100204             leavesr;
065200100205
065300100205           // -?Omissione records annullati?
065400100205           when  TBEatb <> *blank;
065500100205             leavesr;
065600100204
065700100204           // -?Richiesto posizionamento per cliente unificante:?
065800100204           //  ?si scartano i clienti con unificante inferiore?
065900100204           when  Not Ord_x_SUN   and   TBEke1 < C1Cksu;
066000100205             leavesr;
066100100204
066200100204           // -?Richiesto posizionamento per Strategi User Number:?
066300100204           //  ?si scartano i clienti con S.U.N. inferiore?
066400100204           when  Ord_x_SUN   and   TBEke2 < C1Csun;
066500100205             leavesr;
066600100204
066700100204         endsl;
066800100204
066900100204         $RecOK = *on;
067000100204
067100100204       ENDSR;
067200100204
067300100204       //--------------------------------------------------------------
067400121008       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
067500121008       //?F8-Cambia ordinamento sfl                                    ?
067600100204       //--------------------------------------------------------------
067700121008       BEGSR  sr_F08S01;
067800100204
067900100204         Ord_x_SUN = (Not Ord_x_SUN);
068000100204
068100100331         select;
068200100331           // -?Subfile vuoto: nessun record da ordinare?
068300100331           when  S01nrr = *zero;
068400100331           // -?Subfile già posizionato: occorre ricaricarlo?
068500100331           when  C1Cksu <> *blank   or   C1Csun <> *blank;
068600100331             $InzS01 = *on;
068700100331           // -?Altrimenti: basta riordinarlo?
068800100331           other;
068900100331             exsr  sr_SortSfl;
069000100331         endsl;
069100100331
069200100331         if Not Ord_x_SUN;
069300100331           clear  C1Cksu;
069400100331           clear  SavC1Cksu;
069500100331         else;
069600100331           clear  C1Csun;
069700100331           clear  SavC1Csun;
069800100331         endif;
069900100204
070000100204       ENDSR;
070100100204
070200100204       //--------------------------------------------------------------
070300100204       //?Ordinamento subfile                                          ?
070400100204       //?  This subroutine sorts the subfile records.                 ?
070500100204       //--------------------------------------------------------------
070600100204       BEGSR  sr_SortSfl;
070700100204
070800100204         RrnLast  = S01nrr;
070900100204
071000100204         C1RcdNbr = 1;
071100100204         clear  C1CsrRrn;
071200100204         clear  S01nrr;
071300100204         clear  V1Dmsg;
071400100204         ErrMessage  = *off;
071500100204         SflNxtChg   = *off;
071600100204         ErrGenerico = *off;
071700100204         %subst(IndDspF : 50) = *off;
071800100204
071900100204         //?___________________________________________________________?
072000100204         //?Initialize the key fields to sort on.?
072100100204         //?There is one extra field not in the subfile -- Selected --?
072200100204         //?that is added to the record. This field is used to place?
072300100204         //?selected records in front of nonselected records.?
072400100204         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
072500100204
072600100204         clear  QLGKL;
072700100204         clear  QLGSKL;
072800100204         clear  QLGSCB;
072900100204         clear  QLGSCB00;
073000100204
073100100204         // -?Gestione della scelta ordinamento:?
073200100204         //  ?Ordinamento per Strategi User Number?
073300100204         If  Ord_x_SUN = *on;
073400100204           // -?2 campi chiave?
073500100204           QLGNBRK  = 2;
073600100204           // -?1° campo: strategi user number (S1Csun)?
073700100204           //            ?a posizone   29,  9 byte, char, ascending.?
073800100204           QLGSP    = 29;
073900100204           QLGSS    = %size(S1Csun);
074000100204           QLGDT    = Carattere;
074100100204           QLGSO    = Ascendente;
074200100204           QLGKL(1) = QLGSKL;
074300100204           // -?2° campo: codice agente (S1Ccom)?
074400100204           //            ?a posizone    1,  8 byte, char, ascending.?
074500100204           QLGSP    = 1;
074600100204           QLGSS    = %size(S1Cksu);
074700100204           QLGDT    = Carattere;
074800100204           QLGSO    = Ascendente;
074900100204           QLGKL(2) = QLGSKL;
075000100204
075100100204         // -?Ordinamento per Cliente Unificante?
075200100204         Else;
075300100204           // -?2 campi chiave?
075400100204           QLGNBRK  = 2;
075500100204           // -?1° campo: cliente unificante (S1Cksu)?
075600100204           //            ?a posizone    1,  8 byte, char, ascending.?
075700100204           QLGSP    = 1;
075800100204           QLGSS    = %size(S1Cksu);
075900100204           QLGDT    = Carattere;
076000100204           QLGSO    = Ascendente;
076100100204           QLGKL(1) = QLGSKL;
076200100204           // -?2° campo: strategi user number (S1Csun)?
076300100204           //            ?a posizone   29,  9 byte, char, ascending.?
076400100204           QLGSP    = 29;
076500100204           QLGSS    = %size(S1Csun);
076600100204           QLGDT    = Carattere;
076700100204           QLGSO    = Ascendente;
076800100204           QLGKL(2) = QLGSKL;
076900100204
077000100204         EndIf;
077100100204
077200100204         // -?Load other sort parameters.?
077300100204         QLGLB   = 80 + 16 * MaxKey;
077400100204         QLGRL   = %size(SflRcd) - 1;
077500100204         QLGRT   = 8;
077600100204         QLGOKL  = 80;
077700100204         QLGLKE  = 16;
077800100204         QLGLSS  = 290;
077900100204         // -?Initialize Sort I/O API fields.?
078000100204         QLGRL00 = QLGRL;
078100100204         QLGRC00 = 1;
078200100204         clear  QUSEI;
078300100204         QUSBPRV = %size(QUSEC);
078400100204
078500100204         // -?First step - Initialize the sort routine.?
078600100204         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
078700100204                      SizeList : ReturnSize : QUSEC );
078800100204
078900100204         // -?Next step - Write records to I/O routine.?
079000100204         QLGRT00  = Put;
079100100204         For  Nrr = 1  To  RrnLast;
079200100204           chain  Nrr  TB79S01;
079300100204           // -?Solo le righe con Selected = 'Y' sono riordinate,?
079400100204           //  ?quindi per fare un ordinamento di tutte le righe?
079500100204           //  ?metto 'Y' sempre.?
079600100204           Selected = 'Y';
079700100204           clear  QUSEI;
079800100204           QUSBPRV  = %size(QUSEC);
079900100204           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
080000100204                         SizeList : NotUsed : QUSEC );
080100100204         EndFor;
080200100204
080300100204         // -?Next step - Signal end of input, clear subfile for reload.?
080400100204         QLGRT00 = EndPut;
080500100204         clear  QUSEI;
080600100204         QUSBPRV = %size(QUSEC);
080700100204         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
080800100204                       SizeList : NotUsed : QUSEC );
080900100204
081000100204         // -?Pulizia subfile?
081100100204         SflDsp_N    = *on;
081200100204         SflDspCtl_N = *on;
081300100216         write  TB79C01;
081400100204         SflDspCtl_N = *off;
081500100204         SflEnd      = *off;
081600100204
081700100204         // -?Final step - Write the records back to the subfile.?
081800100204         QLGRT00  = Get;
081900100204         For  Nrr = 1  To RrnLast;
082000100204           clear  QUSEI;
082100100204           QUSBPRV = %size(QUSEC);
082200100204           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
082300100204                         QLGRL00  : NotUsed : QUSEC );
082400120814           // -?Ripristino indicatori utilizzati nel sfl rec?
082500120814           AnnullApplic = (H1Cfaa = 'A');
082600100204           SflNxtChg = (S1Copz <> *blank);
082700100204           S01nrr = Nrr;
082800100205           write  TB79S01;
082900100204         EndFor;
083000100204
083100100204         C1CsrRrn = 1;
083200100204
083300100204         // -?Visualizzazione del SFL (se ci sono dati)?
083400100204         SflDsp_N = (S01nrr <= *zero);
083500100204
083600100204         // -?Attivazione del SFLEND?
083700100204         SflEnd = $EoF;
083800100204
083900100204       ENDSR;
084000100204
084100100204       //--------------------------------------------------------------
084200100204       //?Gestione opzioni subfile S01.                                ?
084300100204       //--------------------------------------------------------------
084400100204       BEGSR  sr_OpzS01;
084500100204
084600100204         clear  SavS01csr;
084700100204
084800100204         // -?Ciclo di lettura subfile?
084900100204         readc  TB79S01;
085000100204
085100100205         DoW  Not %eof(TNTB79D1);
085200100204
085300100204           SflNxtChg = *off;
085400100204           %subst(IndDspF : 50) = *off;
085500100204           C1RcdNbr = S01nrr;
085600100204
085700100204           select;
085800100204
085900100204             // -?Opz. 1=Selezione?
086000100205             when  S1Copz = '1'   and   Not $Fine;
086100100211               o3EWksu = S1Cksu;
086200100211               o3EWsun = S1Csun;
086300100204               k_TBEke1 = S1Cksu;
086400100204               chain  ( c_Tab : S1Cksu : S1Csun )  TNTBE000;
086500100204               if  %found(TNTBE01L);
086600100204                 o3EWuni = TBEuni;
086700100204               endif;
086800100204               $Fine   = *on;
086900100204
087000100204             // -?Opz. 1=Selezione già reperita?
087100100204             //  ?(ammessa una sola selezione)?
087200100205             when  S1Copz = '1'   and   $Fine;
087300100204               ErrMessage  = *on;
087400100204               ErrGenerico = *on;
087500100204               PosCurOPZ   = *on;
087600100204               V1Dmsg = $Msg(02);
087700100204               $Fine  = *off;
087800100204
087900100204             // -?Nessuna opzione?
088000100205             when  S1Copz = *blank;
088100100204
088200100204             // -?Opzione errata?
088300100204             other;
088400100204               ErrMessage  = *on;
088500100204               ErrGenerico = *on;
088600100204               PosCurOPZ   = *on;
088700100204               V1Dmsg = $Msg(01);
088800100204               $Fine  = *off;
088900100204
089000100204           endsl;
089100100204
089200100204           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
089300100204           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
089400100205           if  S1Copz  <> *blank   and
089500100204              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
089600100204             SavS01csr   = C1RcdNbr;
089700100204           endif;
089800100204
089900100204           // -?Aggiornamento sfl?
090000100204           select;
090100100204             when  ErrMessage;
090200100204               SflNxtChg = *on;
090300100204               C1CsrRrn  = C1RcdNbr;
090400100204             when  ErrGenerico;
090500100204               C1CsrRrn  = C1RcdNbr;
090600100205               if  S1Copz <> '1';
090700100205                 clear  S1Copz;
090800100204               endif;
090900100204             other;
091000100205               if  S1Copz <> '1';
091100100205                 clear  S1Copz;
091200100204               endif;
091300100204           endsl;
091400100204
091500100205           if  S1Copz <> *blank;
091600100204             SflNxtChg = *on;
091700100204           endif;
091800121122
091900121122           AnnullApplic = (H1Cfaa = 'A');
092000100204
092100100204           UPDATE  TB79S01;
092200100204
092300100204           if  ErrGenerico   or   ErrMessage;
092400100204             leave;
092500100204           endif;
092600100204
092700100204           readc  TB79S01;
092800100204
092900100204         ENDDO;
093000100204
093100100204         // -?Riposizionam. cursore sul record contenente la prima opz.?
093200100204         //  ?(se non sono stati rilevati errori)?
093300100204         if  NOT ErrMessage   and   NOT ErrGenerico   and
093400100204             SavS01csr > *zero;
093500100204           C1CsrRrn = SavS01csr;
093600100204         endif;
093700100204
093800100204       ENDSR;
093900100204
094000100204       //--------------------------------------------------------------
094100100204       //?Operazioni finali.                                           ?
094200100204       //--------------------------------------------------------------
094300100204       BEGSR  sr_RoutEnd;
094400100217
094500100217         // -?Chiusura funzioni precedentemente aperte?
094600100217         reset  TIBS69ds;
094700100217         //I69tla = 'C';         ?(già così)?
094800100217         tibs69r( tibs69ds :
094900100217                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
095000100204
095100100204         // -?Ritorno parametri?
095200100204         if  %parms() > 1;
095300100204           p_TNTB79ds = TNTB79ds;
095400100204         endif;
095500100204
095600100204         // -?"Chiusura" *pgm?
095700100204         return;
095800100204
095900100204       ENDSR;
096000100204
096100100204      /end-free
096200100204
096300100204       //--------------------------------------------------------------
096400100204       //?Schiere a tempo di compilazione.                             ?
096500100204       //--------------------------------------------------------------
096600100204
096700100204** - $Msg -------------------------------------------------------------------*
096800100204Opzione errata                                                                 1
096900100331Ammessa la selezione di un solo cliente                                        2
