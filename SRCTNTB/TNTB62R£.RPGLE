000100100308      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200100308       //==============================================================
000300100308       //?GESTIONE TABELLA "MRA" - Bart-Mailing                        ?
000400100308       //==============================================================
000500100308
000600100308       //--------------------------------------------------------------
000700100308       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800100308       //--------------------------------------------------------------
000900100308
001000100318     /*PRM dbgview(*source)
001100100308     /*END
001200100308
001300100308       //--------------------------------------------------------------
001400100308       //?Specifiche di controllo.                                     ?
001500100308       //--------------------------------------------------------------
001600100308
001700080107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800100318     h dftactgrp(*no) actgrp(*caller)
001900080107
002000100308       //---------------------------------------------------------------
002100100308       //?Dichiarazione file.                                          ?
002200100308       //---------------------------------------------------------------
002300150721
002400150721       // -?Mailing Automatico - REGOLE?
002500150721     fTIMAR01L  if   e           k disk
002600100308
002700100308       // -?Tabelle?
002800080107     fTNTBE01L  Uf A e           k disk
002900100308
003000100308       // -?Video?
003100080107     fTNTB62D   cf   e             workstn
003200080108     f                                     infds(InfDspF)
003300080107     f                                     indds(IndDspF)
003400080107
003500100308       //---------------------------------------------------------------
003600100308       //?Definizione costanti.                                        ?
003700100308       //---------------------------------------------------------------
003800100308
003900100308       // -?Codice tabella in gestione?
004000100308     d c_Tab           c                   const('MRA')
004100110613     d c_MRAbyps       c                   const('BYP')
004200100308     d c_MRAdan        c                   const('DAN')
004300150721     d c_MRAceg        c                   const('CEG')
004400100420     d c_MRAmoa        c                   const('MOA')
004500110613     d c_MRAxls        c                   const('XLS')
004600100308
004700100308       // -?Tasti funzionali a video?
004800100308     d c_F01           c                   const(x'31')
004900100308     d c_F02           c                   const(x'32')
005000100308     d c_F03           c                   const(x'33')
005100100308     d c_F04           c                   const(x'34')
005200100308     d c_F05           c                   const(x'35')
005300100308     d c_F06           c                   const(x'36')
005400100308     d c_F07           c                   const(x'37')
005500100308     d c_F08           c                   const(x'38')
005600100308     d c_F09           c                   const(x'39')
005700100308     d c_F10           c                   const(x'3A')
005800100308     d c_F11           c                   const(x'3B')
005900100308     d c_F12           c                   const(x'3C')
006000100308     d c_F13           c                   const(x'B1')
006100100308     d c_F14           c                   const(x'B2')
006200100308     d c_F15           c                   const(x'B3')
006300100308     d c_F16           c                   const(x'B4')
006400100308     d c_F17           c                   const(x'B5')
006500100308     d c_F18           c                   const(x'B6')
006600100308     d c_F19           c                   const(x'B7')
006700100308     d c_F20           c                   const(x'B8')
006800100308     d c_F21           c                   const(x'B9')
006900100308     d c_F22           c                   const(x'BA')
007000100308     d c_F23           c                   const(x'BB')
007100100308     d c_F24           c                   const(x'BC')
007200100308     d c_Enter         c                   const(x'F1')
007300100308     d c_RollDown      c                   const(x'F4')
007400100308     d c_RollUp        c                   const(x'F5')
007500080107
007600100308       //---------------------------------------------------------------
007700100308       //?Definizione schiere.                                         ?
007800100308       //---------------------------------------------------------------
007900100308
008000100308       // -?Messaggi di errore?
008100150721     d $Msg            s             78    dim( 5) ctdata perrcd(1)
008200080107
008300100308       //---------------------------------------------------------------
008400100308       //?Definizione aree dati.                                       ?
008500100308       //---------------------------------------------------------------
008600100308
008700100308       // -?Dati utente?
008800080107     d §AzUte        e ds                  extname(AZUTE00F)
008900080107     d                                     dtaara
009000080107     d §DatiUte      e ds                  extname(dDatiUte)
009100080107     d                                     dtaara
009200080107
009300100308       //---------------------------------------------------------------
009400100308       //?Definizione strutture dati.                                  ?
009500100308       //---------------------------------------------------------------
009600100308
009700100308       // -?Status ds?
009800080107     d Psds           sds
009900080107     d   SDSpgm          *proc
010000080107
010100100308       // -?InfDS?
010200080107     d InfDspF         ds
010300080107     d  dsp_aid              369    369a                                        AID byte
010400080107     d**sf_rrn               376    377I 0                                      Subfile rrn
010500080107
010600100308       // -?Indicatori su DspF?
010700100308     d IndDspF         ds
010800100308         // -?Abilitazione tasti funzionali?
010900100805     d  F5Attivo                       n   overlay(IndDspF:05)
011000100805     d  F6Attivo                       n   overlay(IndDspF:06)
011100100805     d  F16Attivo                      n   overlay(IndDspF:16)
011200100308         // -?Emissione messaggio di errore?
011300100805     d  errMessage                     n   overlay(IndDspF:28)
011400100308         // -?Posizionamento cursore & segnalazione errore?
011500100805     d  PosCurKe1                      n   overlay(IndDspF:50)
011600100805     d  PosCurKe2                      n   overlay(IndDspF:51)
011700100805     d  PosCurObj                      n   overlay(IndDspF:52)
011800111202     d  PosCurInd                      n   overlay(IndDspF:53)
011900150721     d  PosCurReg                      n   overlay(IndDspF:54)
012000150721     d  PosCurReg2                     n   overlay(IndDspF:55)
012100150721     d  PosCurOutQ                     n   overlay(IndDspF:56)
012200100308         //   -?Riemissione videata?
012300100805     d  errGenerico                    n   overlay(IndDspF:99)
012400080107
012500100308       // -?Parametri ricevuti?
012600080107     d KPJBA         e ds
012700080108     ***d TNTB62ds      e ds                  inz
012800080107
012900100308       // -?Tabelle "MRA" = Bart-Mailing?
013000080107     d dMRA          e ds                  inz
013100110613     d dMRAbyps      e ds                  inz  prefix(BYPS)
013200150721     d dMRAceg       e ds                  inz  prefix(CEG)
013300080107     d dMRAdan       e ds                  inz
013400100805     d dMRAmk24      e ds                  inz  prefix(MK24)
013500110613     d dMRAxls       e ds                  inz  prefix(§MRAX : 4)
013600080108
013700100308       // -?Dati record principale della tabella?
013800080108     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013900080108     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014000080108     d                                          prefix(TBX:3)
014100080107
014200100308       //---------------------------------------------------------------
014300100308       //?Definizione variabili globali.                               ?
014400100308       //---------------------------------------------------------------
014500100308
014600100308       // -?Flags booleani?
014700111202     d $InzD01         s               n   inz(*on)
014800111202     d $InzD02         s               n   inz(*on)
014900111202     d $InzW01         s               n   inz(*on)
015000111202     d $Fine           s               n   inz
015100111202     d $Avviso         s               n   inz
015200111202
015300111202       // -?Indici di schiera / Contatori?
015400120627     d xx              s              3  0 inz
015500080107
015600100308       // -?Variabili per la gestione del video?
015700080107     d $Video          s              2    inz('D1')
015800080108     d wFxx            s              3    inz
015900111202
016000111202       // -?Variabili di comodo?
016100120627     d wEMLindO        s                   like(§EMLindO)  inz
016200150721     d wOutQ           s                   like(V2CoutqI)  inz
016300080107
016400100308       //---------------------------------------------------------------
016500100308       //?Definizione prototipi procedure.                             ?
016600100308       //---------------------------------------------------------------
016700100308
016800100308       // -?Reperimento dati utente?
016900150721     d TIBS34ds      e ds
017000100308     d tibs34r         pr                  extpgm('TIBS34R')
017100150721     d   tibs34ds                          likeds(TIBS34ds)
017200100308
017300100308       // -?Interrogazione tabelle?
017400150721     d TIBS02ds      e ds                  inz
017500150721     d   T02mod      e                     inz('R')
017600150721     d   T02cod      e                     inz(C_Tab)
017700100308     d tibs02r         pr                  extpgm('TIBS02R')
017800150721     d   kpjba                             likeds(kpjba)
017900150721     d   tibs02ds                          likeds(TIBS02ds)
018000150721
018100150721       // -?Interrogazione Regole Invii Spool via e-mail?
018200150721     d trtcM5ds        ds                  inz  qualified
018300150721     d   pOP0                         3    inz
018400150721     d   pOP1                         3    inz
018500150721     d   pCODr                        3    inz
018600150721     d   pMSG                        80    inz
018700150721     d trtcM5r         pr                  extpgm('TRTCM5R')
018800150721     d   kpjba                             likeds(kpjba)
018900100308
019000100308       // -?Controllo & Inversione date?
019100150721     d WLBdat          ds                  inz
019200150721     d   G08dat                       8s 0 inz
019300150721     d   G08inv                       8s 0 inz
019400150721     d   G08err                       1    inz('3')
019500150721     d   G08tgi                       5s 0 inz
019600150721     d xsrDa8          pr                  extpgm('XSRDA8')
019700150721     d   wlbdat                            likeds(WLBdat)
019800111202
019900111202       // -?Controllo indirizzo e-mail?
020000120627     d dsEMAIL       e ds                  inz
020100120627      /copy gaitrasrc/srcProtoPR,XEMAIL
020200150721
020300150721       // -?API QCAPCMD (Process Commands)?
020400150721     d Qcmd            s           2048    inz  varying
020500150721      /copy qSysInc/qRpgleSrc,QCAPCMD
020600150721      /copy gaitrasrc/srcProtoPR,QCAPCMD
020700150721
020800150721       // -?Parametri gestione errori API.?
020900150721      /copy qsysinc/qrpglesrc,QUSEC
021000080107
021100080107      //---------------------------------------------------------------
021200100308      //?Definizione key-list.                                         ?
021300080107      //---------------------------------------------------------------
021400100308
021500100308       // -?File TNTBE01L?
021600100308     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
021700100308     d                                     prefix(k_)   inz
021800080107
021900100308       //--------------------------------------------------------------
022000100308       //?Riepilogo indicatori.                                        ?
022100100308       //--------------------------------------------------------------
022200100308       // 25    - Comodo
022300100308       // 28    - Emissione del messaggio di errore a video
022400100308       // 99    - Generico di errore
022500100308       // x D01:
022600100308       // 51    - Codice 1 funzione            errato
022700100308       // 52    - Codice 2 funzione            errato
022800150721       // x D02...:
022900150721       // 53    - Indirizzo e-mail mittente    errato ("BYP"/"DAN"/"MOA")
023000150721       // 54    - Codice Regola                errato
023100150721       // 55    - Codice Regola allegati mail  errato ("MOA")
023200150721       // 56    - OutQ per input e-mail        errata ("   "/"DAN"/"MOA"/"XLS")
023300100308       //--------------------------------------------------------------
023400100308
023500100308       //--------------------------------------------------------------
023600100308       //?M A I N - L I N E                                            ?
023700100308       //--------------------------------------------------------------
023800080107
023900080107     c     *Entry        plist
024000080107     c                   parm                    KPJBA
024100080107
024200080107      /free
024300080107
024400100308       // -?Operazioni iniziali?
024500100805       exsr  sr_RoutInz;
024600080107
024700100308       // -?Ciclo di gestione del file video?
024800100805       DOW  $Fine = *off;
024900080107         select;
025000100805           when  $Video = 'D1';
025100100805             exsr  sr_GesD01;
025200100805           when  $Video = 'D2';
025300100805             exsr  sr_GesD02;
025400100805           when  $Video = 'W1';
025500100805             exsr  sr_GesW01;
025600080107           other;
025700080107             leave;
025800080107         endsl;
025900080107       ENDDO;
026000080107
026100100308       // -?Operazioni finali?
026200100805       exsr  sr_RoutEnd;
026300080107
026400080107       //--------------------------------------------------------------
026500100308       //?Operazioni iniziali.                                         ?
026600080107       //--------------------------------------------------------------
026700100805       BEGSR  sr_RoutInz;
026800080107
026900100805         //if  kpjbu <> *blanks;
027000100308         //  TNTB62ds = kpjbu;
027100100308         //else;
027200100805         //  clear  TNTB62ds;
027300100308         //endif;
027400100308         //B62fxx = *blank;
027500100308         //B62err = *off;
027600100308         //B62msg = *blank;
027700080107
027800100308         // -?Reperimento dati job?
027900100805         exsr  sr_DatiJob;
028000080107
028100100308         // -?Impostazione nome programma a video?
028200080107         V1Tpgm = SDSpgm;
028300080107
028400100308         // -?Verifica parametri ricecvuti?
028500100805         //if B62ke1 <> *blanks;
028600100805         //  exsr  sr_InzD01;
028700100805         //  exsr  sr_CtrD01;
028800100805         //  if  errMessage = *on;
028900100308         //    B62err = *on;
029000100308         //    B62msg = V1Dmsg;
029100100308         //    $Fine  = *on;
029200100308         //    leavesr;
029300100308         //  endif;
029400100308         //  $Video  = 'D2';
029500100308         //  $InzD02 = *on;
029600100308         //endif;
029700080108
029800100308         // -?Aggancio dati generali della tabella in esame?
029900100805         clear  k_TBEcod;
030000100308         k_TBEke1 = *zero;
030100100308         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_tab)+1 : %len(c_tab))
030200100308                = c_tab;
030300100805         clear  k_TBEke2;
030400100805         clear  k_TBElin;
030500100308         k_TBEsif = KNSIF;
030600100308         chain  %kds(k05tntbe01)  TNTBE000;
030700100805         if  not %found(TNTBE01L);
030800100805           clear  k_TBEsif;
030900100308           chain  %kds(k05tntbe01)  TNTBE000;
031000080108         endif;
031100100805         if  %found(TNTBE01L);
031200080108           xTNTBEds = TNTBEds;
031300080108         else;
031400100805           clear  xTNTBEds;
031500080108         endif;
031600080107
031700080107       ENDSR;
031800080107
031900080107       //--------------------------------------------------------------
032000100308       //?Reperimento Dati del job (Utente/Operativi).                 ?
032100080107       //--------------------------------------------------------------
032200100805       BEGSR  sr_DatiJob;
032300080107
032400080107         in(E) §AzUte;
032500080107         if NOT %error;
032600080107           in(E) §DatiUte;
032700080107         endif;
032800080107         if %error or RSut = *blanks;
032900080107           clear TIBS34ds;
033000080107           tibs34r(tibs34ds);
033100080107           in §AzUte;
033200080107           in §DatiUte;
033300080107         endif;
033400080107
033500080107       ENDSR;
033600080107
033700080107       //--------------------------------------------------------------
033800100308       //?Gestione videata D01                                         ?
033900080107       //--------------------------------------------------------------
034000100805       BEGSR  sr_GesD01;
034100080107
034200100308         // -?Inizializzazione videata?
034300100805         if  $InzD01 = *on;
034400100805           exsr  sr_InzD01;
034500080107           $InzD01 = *off;
034600080107         endif;
034700080107
034800100308         // -?Emissione testata?
034900100805         write  TB62T01;
035000080107
035100100308         // -?Emissione videata?
035200100805         exfmt  TB62D01;
035300080107         errMessage  = *off;
035400080107         errGenerico = *off;
035500100805         clear  V1Dmsg;
035600080107
035700080107         select;
035800100308           // -?F3=Fine?
035900100805           when  dsp_aid = c_F03;
036000100805             exsr  sr_F03D01;
036100100308           // -?Enter?
036200080107           other;
036300100805             exsr  sr_CtrD01;
036400100805             if  errGenerico;
036500080107               leavesr;
036600080107             endif;
036700080107             $Video  = 'D2';
036800080107             $InzD02 = *on;
036900080107         endsl;
037000080107
037100080107       ENDSR;
037200080107
037300080107       //--------------------------------------------------------------
037400100308       //?Inizializzazione videata D01                                 ?
037500080107       //--------------------------------------------------------------
037600100805       BEGSR  sr_InzD01;
037700080107
037800100805         clear  TB62D01;
037900080107
038000100805         //if  B62ke1 = *blank;
038100100308             V1Cke1 = '?';
038200100308         //else;
038300100308         //  V1Cke1 = %trimr(B62ke1);
038400100308         //  V1Cke2 = %trimr(B62ke2);
038500100308         //endif;
038600080107
038700080107       ENDSR;
038800080107
038900080107       //--------------------------------------------------------------
039000100308       //?Controllo videata D01                                        ?
039100080107       //--------------------------------------------------------------
039200100805       BEGSR  sr_CtrD01;
039300080107
039400080107         IndDspF = *zeros;
039500080107
039600080107         SELECT;
039700080107
039800100308           // -?Ricerca codice cliente?
039900100805           WHEN  %scan('?' : V1Cke1) > *zeros   or
040000100805                 %scan('?' : V1Cke2) > *zeros;
040100100805             reset  TIBS02ds;
040200100308             //T02mod = 'R';     (già così)
040300100308             //T02cod = C_Tab;   (già così)
040400080107             T02sif = KNSIF;
040500100805             tibs02r ( kpjba : tibs02ds );
040600100805             if  T02err = *blanks;
040700080107               V1Cke1 = T02ke1;
040800080107               V1Cke2 = T02ke2;
040900080107               errGenerico = *on;
041000080107               leavesr;
041100080107             else;
041200080107               errMessage  = *on;
041300080107               errGenerico = *on;
041400080107               PosCurKe1   = *on;
041500080107               V1Dmsg = T02msg;
041600080107               leavesr;
041700080107             endif;
041800080107
041900100308           // -?Controllo immissione chiave tabella?
042000100805           WHEN  V1Cke1 = *blanks;
042100080107             errMessage  = *on;
042200080107             errGenerico = *on;
042300080107             PosCurKe1   = *on;
042400080107             V1Dmsg = $Msg(01);
042500080107             leavesr;
042600080107
042700080107         ENDSL;
042800080107
042900100308         // -?Controllo esistenza tabella?
043000100805         clear  TNTBE000;
043100100805         clear  k05tntbe01;
043200100805         k_TBEcod = c_Tab;
043300100805         k_TBEke1 = V1Cke1;
043400100805         k_TBEke2 = V1Cke2;
043500100308         chain  %kds(k05tntbe01)  TNTBE000;
043600080107
043700080107       ENDSR;
043800080107
043900080107       //--------------------------------------------------------------
044000100308       //?Gestione tasto funzionale F3 da videata D01                  ?
044100100308       //?F3=Fine                                                      ?
044200080107       //--------------------------------------------------------------
044300100805       BEGSR  sr_F03D01;
044400080107
044500100308         // -?Chiusura del programma?
044600080107         $Fine = *on;
044700080107
044800100308         // -?Restituzione indicazione del tasto funzionale premuto?
044900100308         //B62fxx = '1';
045000080107
045100080107       ENDSR;
045200080107
045300080107       //--------------------------------------------------------------
045400100308       //?Gestione videata D02                                         ?
045500080107       //--------------------------------------------------------------
045600100805       BEGSR  sr_GesD02;
045700080107
045800100308         // -?Inizializzazione videata?
045900100805         if  $InzD02  = *on;
046000100805           exsr  sr_InzD02;
046100080107           $InzD02  = *off;
046200080107         endif;
046300080107
046400100308         // -?Emissione testata?
046500100805         write  TB62T01;
046600110613
046700110613         // -?Emissione piede?
046800110613         write  TB62P02;
046900080107
047000100308         // -?Emissione videata?
047100110613         select;
047200150721           when  V2Ctip = *blanks;
047300150721             exfmt  TB62D02;
047400150721           when  V2Ctip = c_MRAbyps;
047500150721             exfmt  TB62D02byp;
047600150721           when  V2Ctip = c_MRAceg;
047700150721             exfmt  TB62D02ceg;
047800150721           when  V2Ctip = c_MRAdan;
047900150721             exfmt  TB62D02dan;
048000150721           when  V2Ctip = c_MRAmoa;
048100150721             exfmt  TB62D02moa;
048200150721           when  V2Ctip = c_MRAxls;
048300150721             exfmt  TB62D02xls;
048400110613         endsl;
048500080107         errMessage  = *off;
048600080107         errGenerico = *off;
048700080107         clear V1Dmsg;
048800080107
048900080107         SELECT;
049000131129           // -?F1=Help?
049100131129           WHEN  dsp_aid = c_F01;
049200131129             // -?Emissione window?
049300131129             exfmt TB62W02;
049400131129           // -?F3=Fine?
049500131129           WHEN  dsp_aid = c_F03;
049600131129             exsr  sr_F03D01;
049700100308           // -?F12=Ritorno?
049800100308           WHEN  dsp_aid = c_F12;
049900100805             exsr  sr_F12D02;
050000100308           // -?Enter o F5 o F6 o F16?
050100080107           OTHER;
050200100308             // -?Controllo dati?
050300100308             exsr sr_CtrD02;
050400080107             if errGenerico;
050500080107               leavesr;
050600080107             endif;
050700100308             // -?Conferma dati:?
050800100308             //  ?F5=Ripristino; F6=Conferma; F16=Annullamento?
050900100308             If  dsp_aid = c_F05   or
051000100308                 dsp_aid = c_F06   or
051100100308                 dsp_aid = c_F16;
051200150721               ////?NON aggiornare qui, ma alla conferma della window !!!?
051300150721               //exsr sr_Upd_TNTBE;
051400150721               //if  NOT  errGenerico;
051500150721               //  clear V1Topz;
051600150721               //  $Video  = 'D1';
051700150721               ////$InzD01 = *on;
051800150721               //endif;
051900100805               clear  wFxx;
052000080108               select;
052100150721                 when  dsp_aid = c_F05;
052200150721                   eval  wFxx = 'F05';
052300150721                 when  dsp_aid = c_F06;
052400150721                   eval  wFxx = 'F06';
052500150721                 when  dsp_aid = c_F16;
052600150721                   eval  wFxx = 'F16';
052700080108               endsl;
052800080108               $Video  = 'W1';
052900080108               $InzW01 = *on;
053000080107             EndIf;
053100080107
053200080107         ENDSL;
053300080107
053400080107       ENDSR;
053500080107
053600080107       //--------------------------------------------------------------
053700100308       //?Inizializzazione videata D02                                 ?
053800080107       //--------------------------------------------------------------
053900100805       BEGSR  sr_InzD02;
054000080107
054100100805         clear  TB62D02;
054200110613         clear  TB62D02byp;
054300150721         clear  TB62D02ceg;
054400100805         clear  TB62D02dan;
054500100805         clear  TB62D02moa;
054600110613         clear  TB62D02xls;
054700080107
054800080107         IndDspF = *zeros;
054900080107
055000100308         // -?Impostazione indicatori per abilitazione tasti funzionali?
055100100805         exsr  sr_Set_Ind_D02;
055200111202
055300111202         reset  $Avviso;
055400120627         clear  wEMLindO;
055500080107
055600100805         clear  dMRA;
055700110613         clear  dMRAbyps;
055800150721         clear  dMRAceg;
055900100805         clear  dMRAdan;
056000100805         clear  dMRAmk24;
056100110613         clear  dMRAxls;
056200080107         V2Cke1 = V1Cke1;
056300080107         V2Cke2 = V1Cke2;
056400100308         select;
056500100805           when  not %found(TNTBE01L);
056600100315             V2Ctip = V1Ctip;
056700110613           // -?dati di dMRABYPS (lunga 82 bytes)?
056800110613           //when  ( V1Ctip = *blank  and
056900110613           //        %subst( TBEuni : %size( dMRAbyps ) + 1 ) = *blank )  OR
057000110613           //      V1Ctip = c_MRAbyps;
057100110613           when  ( V1Ctip = *blank  and
057200110613                 ( %scan( 'BYPS' : %trimr( V1Cke1 ) ) > *zero  or
057300110613                   %scan( 'BYPS' : %trimr( V1Cke2 ) ) > *zero ) )  OR
057400110613                 V1Ctip = c_MRAbyps;
057500110613             V2Ctip = c_MRAbyps;
057600150721           // -?dati di dMRACEG (lunga 125 bytes)?
057700150721           //when  ( V1Ctip = *blank  and
057800150721           //        %subst( TBEuni : %size( dMRAceg ) + 1 ) = *blank )  OR
057900150721           when  ( V1Ctip = *blank  and
058000150721                   %subst( TBEuni : 1 : 4 ) = 'C.E.' )  OR
058100150721                 V1Ctip = c_MRAceg;
058200150721             V2Ctip = c_MRAceg;
058300100318           // -?dati di dMRAXLS (lunga 49 bytes)?
058400100805           when  ( V1Ctip = *blank  and
058500100805                   %subst( TBEuni : %size( dMRAxls ) + 1 ) = *blank )  OR
058600100805                 V1Ctip = c_MRAxls;
058700100315             V2Ctip = c_MRAxls;
058800100318           // -?dati di dMRA (lunga 84 bytes)?
058900100318           when  V1Ctip = *blank  and
059000100318                 %subst( TBEuni : %size( dMRA ) + 1 ) = *blank;
059100100315             V2Ctip = *blank;
059200100805           // -?dati di dMRAMK24 (lunga 129/164 bytes)?
059300100805           when  ( V1Ctip = *blank  and
059400100805                   %subst( TBEuni : %size( dMRAmk24 ) + 1 ) = *blank )  OR
059500100420                 V1Ctip = c_MRAmoa;
059600100420             V2Ctip = c_MRAmoa;
059700100318           // -?dati di dMRADAN (lunga 190 bytes)?
059800100315           when  V1Ctip = *blank  or
059900100315                 V1Ctip = c_MRAdan;
060000100315             V2Ctip = c_MRAdan;
060100100318           // -?dati di dMRA????
060200100308           other;
060300100308             V2Ctip = V1Ctip;
060400100308         endsl;
060500080107
060600100805         IF  %found(TNTBE01L);
060700080107
060800100308           //?Impostazione dati di record trovato?
060900080107           select;
061000100805             //when  B62opz = '5';
061100100308             //  V1Topz = 'VISUALIZZA';
061200100805             when  TBEatb = *blanks;
061300080107               V1Topz = 'VARIAZIONE';
061400080107             other;
061500080107               V1Topz = 'ANNULLATO ';
061600080107           endsl;
061700080107
061800080107           select;
061900100805             when  V2Ctip = c_MRAmoa;
062000100805               dMRAmk24   = TBEuni;
062100100805               V2Codes    = mk24§MRAdes;
062200100805               V2Coregcor = mk24§MRAregCor;
062300100805               V2Coregall = mk24§MRAregAll;
062400100805               V2Comitt   = mk24§MRAmitt;
062500100805               V2Coobjm   = mk24§MRAobjm;
062600100805               V2Coidpro  = mk24§MRAidpro;
062700100805               V2Cooutqi  = mk24§MRAoutqi;
062800100805               V2Copcl    = mk24§MRApcl;
062900100805               V2Costs    = mk24§MRAsts;
063000110613             when  V2Ctip = *blank;
063100110613               dMRA       = TBEuni;
063200110613               V2Cdes     = §MRAdes;
063300110613               V2Creg     = §MRAreg;
063400110613               V2Cmit1l   = §MRAmit1l;
063500110613               V2Cmit2l   = §MRAmit2l;
063600110613               V2Cmites   = §MRAmites;
063700110613               V2Cidpro   = §MRAidpro;
063800110613               V2Coutqi   = §MRAoutqi;
063900150721             when  V2Ctip = c_MRAceg;
064000150721               dMRAceg    = TBEuni;
064100150721               V2CCdes    = ceg§MRAdes;
064200150721               V2CCem     = ceg§MRAem;
064300150721               V2CCcc     = ceg§MRAcc;
064400110613             when  V2Ctip = c_MRAdan;
064500110613               dMRAdan    = TBEuni;
064600110613               V2CDdes    = §MRAddes;
064700110613               V2CDreg    = §MRAdreg;
064800110613               V2CDmitt   = §MRAdmitt;
064900110613               V2CDdest   = §MRAddest;
065000110613               V2CDidpro  = §MRAdidpro;
065100110613               V2CDoutqi  = §MRAdoutqi;
065200110613             when  V2Ctip = c_MRAbyps;
065300110613               dMRAbyps   = TBEuni;
065400110613               V2CYdes    = byps§MRAdes;
065500110613               V2CYidpro  = byps§MRAidpro;
065600110613               V2CYreg    = byps§MRAreg;
065700170201               V2CYren    = byps§MRAregnew;
065800110617               V2CYfil    = byps§MRApo;
065900110613               V2CYsplnam = byps§MRAsplnam;
066000110613               V2CYemlmit = byps§MRAemlmit;
066100110617               V2CYusrdta = byps§MRAusrdta;
066200110613             when  V2Ctip = c_MRAxls;
066300110613               dMRAxls    = TBEuni;
066400110613               V2CXdes    = §MRAxdes;
066500110613               V2CXreg    = §MRAxreg;
066600110613               V2CXfldid  = §MRAxfldid;
066700110613               V2CXidpro  = §MRAxidpro;
066800110613               V2CXoutqi  = §MRAxoutqi;
066900080107           endsl;
067000150721           // -?Decodifica Regola?
067100150721           exsr  sr_CtrD02;
067200150721           clear  V1Dmsg;
067300150721           clear  ErrMessage;
067400150721           clear  ErrGenerico;
067500080107
067600080107         ELSE;
067700080107
067800100308           // -?Impostazione dati di record nuovo?
067900080107           V1Topz = 'IMMISSIONE';
068000080107
068100080107         ENDIF;
068200080107
068300080107       ENDSR;
068400080107
068500080107       //--------------------------------------------------------------
068600100308       //?Settaggio indicatori nella videata D02 per abilitazione      ?
068700100308       //?  tasti funzionali                                           ?
068800080107       //--------------------------------------------------------------
068900100805       BEGSR  sr_Set_Ind_D02;
069000080107
069100100308         //F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blanks)
069200100308         //                               and B62opz <> '5');
069300100308         //F6Attivo  = (NOT F5Attivo and B62opz <> '5');
069400100308         //F16Attivo = ((%found(TNTBE01L) and TBEatb = *blanks)
069500100308         //                               and B62opz <> '5');
069600080107         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blanks));
069700080107         F6Attivo  = (NOT F5Attivo);
069800080107         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blanks));
069900080107
070000080107       ENDSR;
070100080107
070200080107       //--------------------------------------------------------------
070300100308       //?Gestione tasto funzionale F12 da videata D02                 ?
070400100308       //?F12=Ritorno                                                  ?
070500080107       //--------------------------------------------------------------
070600100805       BEGSR  sr_F12D02;
070700080107
070800100805         // -?Disallocazione record?
070900100805         unlock  TNTBE01L;
071000100805
071100100308         // -?Ritorno alla videata precedente se NON richiamato?
071200100308         //if B62opz = *blank  and
071300100308         //   B62sif = *blank  and  B62lin = *blank  and
071400100308         //   B62ke1 = *blank  and  B62ke2 = *blank;
071500100308             clear V1Topz;
071600100308             $Video = 'D1';
071700100308         // -?Ritorno al pgm chiamante se richiamato?
071800100308         //else;
071900100308         //  B62fxx = '2';
072000100308         //  $Fine = *on;
072100100308         //endif;
072200080107
072300080107       ENDSR;
072400080107
072500080107       //--------------------------------------------------------------
072600100308       //?Controllo videata D02                                        ?
072700080107       //--------------------------------------------------------------
072800100805       BEGSR  sr_CtrD02;
072900080107
073000150721         // -?Spegnimento indicatori di posizionamento cursore?
073100150721         %subst(IndDspF : 28) = *off;
073200150721
073300150721         // -?Selezione codice Regola?
073400150721         clear  trtcM5ds;
073500150721         IF  ( V2Ctip = *blank    and  %scan( '?' : V2Creg )  > *zero )   OR
073600150721             ( V2Ctip = c_MRAbyps and  %scan( '?' : V2CYreg ) > *zero )   OR
073700170201             ( V2Ctip = c_MRAbyps and  %scan( '?' : V2CYren ) > *zero )   OR
073800150721             ( V2Ctip = c_MRAdan  and  %scan( '?' : V2CDreg ) > *zero )   OR
073900150721             ( V2Ctip = c_MRAmoa  and
074000150721                                  ( %scan( '?' : V2COregCor ) > *zero  or
074100150721                                    %scan( '?' : V2COregAll ) > *zero ))  OR
074200150721             ( V2Ctip = c_MRAxls  and  %scan( '?' : V2CXreg ) > *zero );
074300150721
074400150721           trtcM5ds.pOP0 = 'P01';
074500150721           kpjbu    = trtcM5ds;
074600150721           trtcM5r ( kpjba );
074700150721           trtcM5ds = kpjbu;
074800150721
074900150721           Select;
075000150721             When  trtcM5ds.pCODr = *blank;
075100150721             When V2Ctip = *blank;
075200150721               V2Creg  = trtcM5ds.pCODr;
075300170201             When  V2Ctip = c_MRAbyps and  %scan( '?' : V2CYreg ) > *zero;
075400150721               V2CYreg = trtcM5ds.pCODr;
075500170201             When  V2Ctip = c_MRAbyps and  %scan( '?' : V2CYren ) > *zero;
075600170201               V2CYren = trtcM5ds.pCODr;
075700150721             When  V2Ctip = c_MRAdan;
075800150721               V2CDreg = trtcM5ds.pCODr;
075900150721             When  V2Ctip = c_MRAmoa  and  %scan( '?' : V2COregCor ) > *zero;
076000150721               V2COregCor = trtcM5ds.pCODr;
076100150721             When  V2Ctip = c_MRAmoa  and  %scan( '?' : V2COregAll ) > *zero;
076200150721               V2COregAll = trtcM5ds.pCODr;
076300150721             When  V2Ctip = c_MRAxls;
076400150721               V2CXreg = trtcM5ds.pCODr;
076500150721           EndSl;
076600150721
076700150721         ENDIF;
076800150721
076900150721         // -?Controllo codice Regola?
077000150721         clear  V2Dreg;
077100170201         clear  V2Dren;
077200150721         Select;
077300150721           When  V2Ctip = *blank    and  V2Creg     <> *blank;
077400150721             chain  ( V2Creg )  TIMAR000;
077500150721             PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
077600170201           When  V2Ctip = c_MRAbyps;
077700170201             if V2CYreg    <> *blank;
077800170201               chain  ( V2CYreg )  TIMAR000;
077900170201               PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
078000170201               if PosCurReg  = '0';
078100170201                 V2Dreg = MARcodD;
078200170201               endif;
078300170201             endif;
078400170201             if V2CYren    <> *blank;
078500170201               chain  ( V2CYren )  TIMAR000;
078600170201               PosCurReg2 = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
078700170201               if PosCurReg2 = '0';
078800170201                 V2Dren = MARcodD;
078900170201               endif;
079000170201             endif;
079100150721           When  V2Ctip = c_MRAdan  and  V2CDreg    <> *blank;
079200150721             chain  ( V2CDreg )  TIMAR000;
079300150721             PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
079400150721           When  V2Ctip = c_MRAmoa;
079500150721             clear  V2DOregCor;
079600150721             clear  V2DOregAll;
079700150721             if  V2COregCor <> *blank;
079800150721               chain  ( V2COregCor )  TIMAR000;
079900150721               PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
080000150721               if  Not PosCurReg;
080100150721                 V2DOregCor = MARcodD;
080200150721               endif;
080300150721             endif;
080400150721             if  V2COregAll <> *blank  and  %scan( '?' : V2COregAll ) = *zero;
080500150721               chain  ( V2COregAll )  TIMAR000;
080600150721               PosCurReg2 = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
080700150721               if  Not PosCurReg2;
080800150721                 V2DOregAll = MARcodD;
080900150721               endif;
081000150721             endif;
081100150721           When  V2Ctip = c_MRAxls  and  V2CXreg    <> *blank;
081200150721             chain  ( V2CXreg )  TIMAR000;
081300150721             PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
081400150721         EndSl;
081500150721         Select;
081600150721           When  PosCurReg  or  PosCurReg2;
081700150721             ErrMessage  = *on;
081800150721             ErrGenerico = *on;
081900150721             V1Dmsg = $Msg(04);
082000150721             leavesr;
082100170201           When  V2Ctip = c_MRAbyps;
082200170201            // non faccio niente perché ho già fatto prima per BYPS
082300150721         EndSl;
082400150721         // -?SE eseguita una selezione: uscita (dopo la decodifica)?
082500150721         if  trtcM5ds.pOP0 <> *blank;
082600150721           leavesr;
082700150721         endif;
082800100805
082900100805         // -?Controllo lunghezza oggetto e-mail?
083000100806         clear  V2COast1;
083100100806         clear  V2COast2;
083200100806         select;
083300100806           when   V2Ctip = c_MRAmoa                       and
083400100806                ( %subst( V2Cke1 : 1 : 7 ) = 'TRMK23R'    or
083500100806                  %subst( V2Cke1 : 1 : 7 ) = 'FIOR52R' )  and
083600100806                  %len( %trimr( V2Coobjm ) ) > 40;
083700100806             V2COast1 = '*';
083800100806             errMessage  = *on;
083900100806             errGenerico = *on;
084000100806             PosCurObj   = *on;
084100100806             V1Dmsg = %trimr( $Msg(02) ) + ' 40';
084200100806             leavesr;
084300100806           when V2Ctip = c_MRAmoa                      and
084400100806                %subst( V2Cke1 : 1 : 7 ) = 'TRMK24R'   and
084500171127                %len( %trimr( V2Coobjm ) ) > 55;
084600100806             V2COast2 = '*';
084700100806             errMessage  = *on;
084800100806             errGenerico = *on;
084900111202             PosCurObj   = *on;
085000171127             V1Dmsg = %trimr( $Msg(02) ) + ' 55';
085100100806             leavesr;
085200100806         endsl;
085300111202
085400111202         // -?Controllo indirizzo e-mail?
085500120627         clear  xx;
085600120627         clear  dsEMAIL;
085700120627         Select;
085800120627           // -?tipo "DAN" (indirizzo e-mail lungo 121 bytes)?
085900120627           //  ?(se unico indirizzo e-mail più lungo di 100 bytes:?
086000120627           //   ?non verrà controllato!!!)?
086100120627           When  V2Ctip = c_MRAdan   and  V2CDdest   <> *blank;
086200120627             clear  wEMLindO;
086300120627             if  %subst( V2CDdest : %len( §EMLindI ) + 1 ) = *blank;
086400120627               §EMLindI = V2CDdest;
086500120627             else;
086600170918               //*·For  xx = %len( §EMLindI )  By -1  DownTo 1;
086700170918               For  xx = %len( §EMLindI )  DownTo 1;
086800120627                 if  %subst( V2CDdest : xx : 1 ) = *blank  or
086900120627                     %subst( V2CDdest : xx : 1 ) = ';';
087000120627                   §EMLindI = %subst( V2CDdest : 1 : xx - 1 );
087100120627                   leave;
087200120627                 endif;
087300120627               EndFor;
087400120627               if  §EMLindI = *blank  and  Not $Avviso;
087500120627                 $Avviso     = *on;
087600120627                 errMessage  = *on;
087700120627                 errGenerico = *on;
087800120627                 PosCurInd   = *on;
087900120627                 V1Dmsg = $Msg(03);
088000120627                 leavesr;
088100120627               endif;
088200120627             endif;
088300120627         EndSl;
088400120627
088500120627         If  §EMLindI <> *blank;
088600120627           xemail ( dsEMAIL );
088700120627           if  §EMLerrO <> *off;
088800120627             errMessage  = *on;
088900120627             errGenerico = *on;
089000120627             PosCurInd   = *on;
089100120627             V1Dmsg = §EMLmsgO;
089200120627             leavesr;
089300120627           endif;
089400120627           // -?tipo "DAN" (indirizzo e-mail lungo 121 bytes)?
089500120627           //  ?(più indirizzi e-mail, per una lunghezza totale di?
089600120627           //   ?oltre 100 bytes)?
089700120627           if  V2Ctip = c_MRAdan   and
089800120627               %subst( V2CDdest : %len( §EMLindI ) + 1 ) <> *blank  and
089900120627               Not  $Avviso        and
090000120627               xx     > *zero      and
090100120627               %subst( V2CDdest : xx + 1 ) <> *blank;
090200120627             wEMLindO = §EMLindO;
090300120627             clear  dsEMAIL;
090400120627             §EMLindI = %subst( V2CDdest : xx + 1 );
090500120627             xemail ( dsEMAIL );
090600120627             if  §EMLerrO <> *off;
090700120627               errMessage  = *on;
090800120627               errGenerico = *on;
090900120627               PosCurInd   = *on;
091000120627               V1Dmsg = §EMLmsgO;
091100120627               leavesr;
091200120627             endif;
091300120627           endif;
091400120627           // -?REimpostazione a video dell'indirizzo e-mail corretto?
091500120627           select;
091600120627             when  V2Ctip = c_MRAdan   and  V2CDdest   <> *blank;
091700120627               if  wEMLindO = *blank;
091800120627                 V2CDdest   = §EMLindO;
091900120627               else;
092000120627                 V2CDdest   = %trim( wEMLindO ) + ';' + %trim( §EMLindO );
092100120627               endif;
092200120627           endsl;
092300120627         EndIf;
092400150721
092500150721         // -?Controllo Coda di stampa per Input e-mail?
092600150721         clear  wOutQ;
092700150721         select;
092800150721           when  ( V2Ctip = *blank    and  V2CoutqI  <> *blank );
092900150721             wOutQ = %trimL( V2CoutqI );
093000150721             V2CoutqI  = wOutQ;
093100150721           when  ( V2Ctip = c_MRAdan  and  V2CDoutqI <> *blank );
093200150721             wOutQ = %trimL( V2CDoutqI );
093300150721             V2CDoutqI = wOutQ;
093400150721           when  ( V2Ctip = c_MRAmoa  and  V2COoutqI <> *blank );
093500150721             wOutQ = %trimL( V2COoutqI );
093600150721             V2COoutqI = wOutQ;
093700150721           when  ( V2Ctip = c_MRAxls  and  V2CXoutqI <> *blank );
093800150721             wOutQ = %trimL( V2CXoutqI );
093900150721             V2CXoutqI = wOutQ;
094000150721         endsl;
094100150721
094200150721         If  wOutQ <> *blank;
094300150721
094400150721           Qcmd = 'CHKOBJ obj('+ %trimR( wOutQ ) + ') objtype(*outq)';
094500150721           exsr  sr_ExecCmd;
094600150721           if  Qusei <> *blank;
094700150721             ErrGenerico = *on;
094800150721             ErrMessage  = *on;
094900150721             PosCurOutQ  = *on;
095000150721             V1Dmsg = $Msg(05);
095100150721             leavesr;
095200150721           endif;
095300150721
095400150721         ENDIF;
095500080107
095600080107       ENDSR;
095700080108
095800080108       //--------------------------------------------------------------
095900100308       //?Gestione videata W01                                         ?
096000080108       //--------------------------------------------------------------
096100100805       BEGSR  sr_GesW01;
096200080108
096300100308         // -?Inizializzazione videata?
096400080108         if $InzW01 = *on;
096500100308           exsr sr_InzW01;
096600080108           $InzW01 = *off;
096700080108         endif;
096800080108
096900100308         // -?Emissione window?
097000080108         exfmt TB62W01;
097100080108         errMessage  = *off;
097200080108         errGenerico = *off;
097300080108         clear V1Dmsg;
097400080108
097500080108         select;
097600080108
097700100308           // -?F12=Ritorno?
097800100308           when dsp_aid = c_F12;
097900100308             exsr sr_F12W01;
098000080108
098100100308           // -?F6=Conferma?
098200100308           when dsp_aid = c_F06;
098300100308             exsr sr_Upd_TNTBE;
098400080108             if  NOT  errGenerico;
098500080108               clear V1Topz;
098600080108               $Video  = 'D1';
098700100308               //$InzD01 = *on;
098800080108             endif;
098900080108         endsl;
099000080108
099100080108       ENDSR;
099200080108
099300080108       //--------------------------------------------------------------
099400100308       //?Inizializzazione videata W01                                 ?
099500080108       //--------------------------------------------------------------
099600100805       BEGSR  sr_InzW01;
099700080108
099800080108         clear TB62W01;
099900080108
100000080108         select;
100100100308           // -?F5=Ripristino?
100200080108           when wFxx = 'F05';
100300080108             W1ftt = TBEftt;
100400100308           // -?F6=Conferma (immissione)?
100500100308           when wFxx = 'F06'  and   NOT %found(TNTBE01L);
100600080108             W1ftt = TBXftt;
100700100308           // -?F6=Conferma (modifica/ripristino)?
100800100308           when wFxx = 'F06'  and   %found(TNTBE01L);
100900080108             W1ftt = TBEftt;
101000100308           // -?F16=Annullamento?
101100080108           when wFxx = 'F16';
101200080108             W1ftt = TBEftt;
101300080108         endsl;
101400080108
101500100308         // -?Se NON immissione:?
101600100308         //  ?visualizza i dati relativi all'ultima trasmissione?
101700080109         if %found(TNTBE01L);
101800080108           W1flt  = TBEflt;
101900080108           W1ftr  = TBEftr;
102000080109           if TBEdtr <> *zero;
102100080108             reset WLBdat;
102200080108             G08inv = TBEdtr;
102300080108             xsrda8(WLBdat);
102400080108             W1dtr  = G08dat;
102500080109           endif;
102600080109         endif;
102700080108
102800080108       ENDSR;
102900080108
103000080108       //--------------------------------------------------------------
103100100308       //?Gestione tasto funzionale F12 da videata W01                 ?
103200100308       //?F12=Ritorno                                                  ?
103300080108       //--------------------------------------------------------------
103400100805       BEGSR  sr_F12W01;
103500080108
103600100308         // -?Ritorno alla videata precedente?
103700100308         $Video = 'D2';
103800080108
103900080108       ENDSR;
104000080107
104100080107       //--------------------------------------------------------------
104200100308       //?Aggiornamento record TNTBE00F (tab. LAC)                     ?
104300080107       //--------------------------------------------------------------
104400100805       BEGSR  sr_Upd_TNTBE;
104500080107
104600080107         SELECT;
104700110613           WHEN  V2Ctip = *blanks;
104800110613             clear  dMRA;
104900110613             §MRAdes    = V2Cdes;
105000110613             §MRAreg    = V2Creg;
105100110613             §MRAmit1l  = V2Cmit1l;
105200110613             §MRAmit2l  = V2Cmit2L;
105300110613             §MRAmites  = V2Cmites;
105400110613             §MRAidpro  = V2Cidpro;
105500110613             §MRAoutqi  = V2Coutqi;
105600100308             TBEuni = dMRA;
105700110613           WHEN  V2Ctip = c_MRAbyps;
105800110613             clear  dMRAbyps;
105900110613             byps§MRAdes    = V2CYdes;
106000110613             byps§MRAidpro  = V2CYidpro;
106100110613             byps§MRAreg    = V2CYreg;
106200170201             byps§MRAregnew = V2CYren;
106300110613             byps§MRApo     = V2CYfil;
106400110613             byps§MRAsplnam = V2CYsplnam;
106500110613             byps§MRAusrdta = V2CYusrdta;
106600110613             byps§MRAemlmit = V2CYemlmit;
106700110613             TBEuni = dMRAbyps;
106800150721           WHEN  V2Ctip = c_MRAceg;
106900150721             clear  dMRAceg;
107000150721             ceg§MRAdes = V2CCdes;
107100150721             ceg§MRAem  = V2CCem;
107200150721             ceg§MRAcc  = V2CCcc;
107300150721             TBEuni = dMRAceg;
107400110613           WHEN  V2Ctip = c_MRAdan;
107500110613             clear  dMRAdan;
107600110613             §MRADdes   = V2CDdes;
107700110613             §MRADreg   = V2CDreg;
107800110613             §MRADmitt  = V2CDmitt;
107900110613             §MRADdest  = V2CDdest;
108000110613             §MRADidpro = V2CDidpro;
108100110613             §MRADoutqi = V2CDoutqi;
108200100308             TBEuni = dMRAdan;
108300110613           WHEN  V2Ctip = c_MRAmoa;
108400110613             clear  dMRAmk24;
108500110613             mk24§MRAdes    = V2Codes;
108600110613             mk24§MRAregCor = V2Coregcor;
108700110613             mk24§MRAregAll = V2Coregall;
108800110613             mk24§MRAmitt   = V2Comitt;
108900110613             mk24§MRAidpro  = V2Coidpro;
109000110613             mk24§MRAoutqi  = V2Cooutqi;
109100110613             mk24§MRApcl    = V2Copcl;
109200110613             mk24§MRAsts    = V2Costs;
109300110613             mk24§MRAobjm   = V2Coobjm;
109400110613             TBEuni = dMRAmk24;
109500110613           WHEN  V2Ctip = c_MRAxls;
109600110613             clear  dMRAxls;
109700100308             §MRAXdes   = V2CXdes;
109800100308             §MRAXreg   = V2CXreg;
109900100308             §MRAXfldid = V2CXfldid;
110000100308             §MRAXidpro = V2CXidpro;
110100100308             §MRAXoutqi = V2CXoutqi;
110200100308             TBEuni = dMRAxls;
110300080107         ENDSL;
110400080107
110500080109         TBEftt = W1ftt;
110600080109         TBEflt = W1flt;
110700080107         clear TBEftr;
110800100308         //clear TBEdtr;
110900080107
111000080107         select;
111100100308           // -?F5=Ripristino?
111200100805           when  wFxx = 'F05';
111300100805             clear  TBEatb;
111400100308           // -?F16=Annullamento?
111500100805           when  wFxx = 'F16';
111600080107             TBEatb = 'A';
111700080107         endsl;
111800080107
111900080107         IF  NOT  %found(TNTBE01L);
112000100805           clear  TBEsif;
112100100805           clear  TBElin;
112200080107           TBEapl = 'MA';
112300100308           TBEcod = c_Tab;
112400080109           TBEke1 = V2Cke1;
112500080109           TBEke2 = V2Cke2;
112600100805           clear  TBEdtr;
112700100308           //_____________
112800080107           WRITE TNTBE000;
112900100308           //¯¯¯¯¯¯¯¯¯¯¯¯¯
113000080107         ELSE;
113100100308           //______________
113200080107           UPDATE TNTBE000;
113300100308           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
113400080107         ENDIF;
113500080107
113600080107       ENDSR;
113700150721
113800150721       //--------------------------------------------------------------
113900150721       //?Esecuzione del comando (già impostato).                      ?
114000150721       //--------------------------------------------------------------
114100150721       BEGSR  sr_ExecCmd;
114200150721
114300150721         clear Qcap0100;
114400150721         Qcabcsdh = *off;
114500150721         Qcapa    = *off;
114600150721         Qcacmdss = *off;
114700150721         Qcaerved = *allX'00';
114800150721
114900150721         clear Qusec;
115000150721         Qusbprv  = %size(Qusec);
115100150721
115200150721         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
115300150721                           %size(Qcap0100) : 'CPOP0100' : *omit :
115400150721                           0 : 0 : Qusec);
115500150721
115600150721         //if  Qusei <> *blank;
115700150721         //  ...;
115800150721         //endif;
115900150721
116000150721       ENDSR;
116100080107
116200080107       //--------------------------------------------------------------
116300100308       //?Operazioni finali.                                           ?
116400080107       //--------------------------------------------------------------
116500100805       BEGSR  sr_RoutEnd;
116600080107
116700150721         //kpjbu = TNTB62ds;
116800080107         *inLR = *on;
116900080107         return;
117000080107
117100080107       ENDSR;
117200100308
117300080107      /end-free
117400080107
117500080107       //--------------------------------------------------------------
117600100308       //?Schiere a tempo di compilazione.                             ?
117700080107       //--------------------------------------------------------------
117800080107
117900080107** - $MSG -------------------------------------------------------------------*
118000080107Immettere il codice tabella "MRA"                                               1
118100100806I caratteri inseriti superano la lunghezza massima prevista in tabella:         2
118200111202AVVISO: indirizzo e-mail troppo lungo per il controllo formale. Premere Invio.  3
118300150721Codice Regola errato                                                            4
118400150721Coda di Stampa errata                                                           5
