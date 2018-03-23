000100100308      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200100308       //==============================================================
000300171129       // ?GESTIONE TABELLA "MRA" - Bart-Mailing                        ?
000400100308       //==============================================================
000500100308
000600100308       //--------------------------------------------------------------
000700171129       // ?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800100308       //--------------------------------------------------------------
000900100308
001000100318     /*PRM dbgview(*source)
001100100308     /*END
001200100308
001300100308       //--------------------------------------------------------------
001400171129       // ?Specifiche di controllo.                                     ?
001500100308       //--------------------------------------------------------------
001600100308
001700171201     h decedit('0,') datedit(*dmy/)
001800100318     h dftactgrp(*no) actgrp(*caller)
001900171201      /copy gaitrasrc/srcConst,BRTleHspec
002000080107
002100100308       //---------------------------------------------------------------
002200171129       // ?Dichiarazione file.                                          ?
002300100308       //---------------------------------------------------------------
002400150721
002500171129       // - ?Mailing Automatico - REGOLE?
002600150721     fTIMAR01L  if   e           k disk
002700100308
002800171129       // - ?Tabelle?
002900080107     fTNTBE01L  Uf A e           k disk
003000100308
003100171129       // - ?Video?
003200080107     fTNTB62D   cf   e             workstn
003300080108     f                                     infds(InfDspF)
003400080107     f                                     indds(IndDspF)
003500080107
003600100308       //---------------------------------------------------------------
003700171129       // ?Definizione costanti.                                        ?
003800100308       //---------------------------------------------------------------
003900100308
004000171129       // - ?Codice tabella in gestione?
004100100308     d c_Tab           c                   const('MRA')
004200171205       // - ?Tipi tabella in gestione?
004300171130     d c_MRAstd        c                   const('STD ')
004400171130     d c_MRAbyps       c                   const('BYPS')
004500171130     d c_MRAdan        c                   const('DAN ')
004600171130     d c_MRAceg        c                   const('CEG ')
004700171130     d c_MRAmk23       c                   const('MK23')
004800171130     d c_MRAmk24       c                   const('MK24')
004900171130     d c_MRAor52       c                   const('OR52')
005000171130     d c_MRAxls        c                   const('XLS ')
005100171205       // - ?Lunghezza MASSIMA della DS per questi dati?
005200171206     d c_MAX_Len       c                   const(250)
005300100308
005400171129       // - ?Tasti funzionali a video?
005500100308     d c_F01           c                   const(x'31')
005600100308     d c_F02           c                   const(x'32')
005700100308     d c_F03           c                   const(x'33')
005800100308     d c_F04           c                   const(x'34')
005900100308     d c_F05           c                   const(x'35')
006000100308     d c_F06           c                   const(x'36')
006100100308     d c_F07           c                   const(x'37')
006200100308     d c_F08           c                   const(x'38')
006300100308     d c_F09           c                   const(x'39')
006400100308     d c_F10           c                   const(x'3A')
006500100308     d c_F11           c                   const(x'3B')
006600100308     d c_F12           c                   const(x'3C')
006700100308     d c_F13           c                   const(x'B1')
006800100308     d c_F14           c                   const(x'B2')
006900100308     d c_F15           c                   const(x'B3')
007000100308     d c_F16           c                   const(x'B4')
007100100308     d c_F17           c                   const(x'B5')
007200100308     d c_F18           c                   const(x'B6')
007300100308     d c_F19           c                   const(x'B7')
007400100308     d c_F20           c                   const(x'B8')
007500100308     d c_F21           c                   const(x'B9')
007600100308     d c_F22           c                   const(x'BA')
007700100308     d c_F23           c                   const(x'BB')
007800100308     d c_F24           c                   const(x'BC')
007900100308     d c_Enter         c                   const(x'F1')
008000100308     d c_RollDown      c                   const(x'F4')
008100100308     d c_RollUp        c                   const(x'F5')
008200080107
008300100308       //---------------------------------------------------------------
008400171129       // ?Definizione schiere.                                         ?
008500100308       //---------------------------------------------------------------
008600100308
008700171129       // - ?Messaggi di errore?
008800171205     d $Msg            s             78    dim( 9) ctdata perrcd(1)
008900080107
009000100308       //---------------------------------------------------------------
009100171129       // ?Definizione aree dati.                                       ?
009200100308       //---------------------------------------------------------------
009300100308
009400171129       // - ?Dati utente?
009500080107     d §AzUte        e ds                  extname(AZUTE00F)
009600080107     d                                     dtaara
009700080107     d §DatiUte      e ds                  extname(dDatiUte)
009800080107     d                                     dtaara
009900080107
010000100308       //---------------------------------------------------------------
010100171129       // ?Definizione strutture dati.                                  ?
010200100308       //---------------------------------------------------------------
010300100308
010400171129       // - ?Status ds?
010500080107     d Psds           sds
010600080107     d   SDSpgm          *proc
010700080107
010800171129       // - ?InfDS?
010900080107     d InfDspF         ds
011000171130     d   dsp_aid             369    369a                                        AID byte
011100171130     d***sfl_rrn             376    377I 0                                      Subfile rrn
011200080107
011300171129       // - ?Indicatori su DspF?
011400100308     d IndDspF         ds
011500171129         // - ?Abilitazione tasti funzionali?
011600171130     d   F5Attivo                      n   overlay(IndDspF:05)
011700171130     d   F6Attivo                      n   overlay(IndDspF:06)
011800171130     d   F16Attivo                     n   overlay(IndDspF:16)
011900171129         // - ?Emissione messaggio di errore?
012000171130     d   errMessage                    n   overlay(IndDspF:28)
012100171129         // - ?Posizionamento cursore & segnalazione errore?
012200171130     d   PosCurTip                     n   overlay(IndDspF:50)
012300171130     d   PosCurKe1                     n   overlay(IndDspF:51)
012400171130     d   PosCurKe2                     n   overlay(IndDspF:52)
012500171130     d   PosCurObj                     n   overlay(IndDspF:53)
012600171130     d   PosCurInd                     n   overlay(IndDspF:54)
012700171130     d   PosCurReg                     n   overlay(IndDspF:55)
012800171130     d   PosCurReg2                    n   overlay(IndDspF:56)
012900171130     d   PosCurOutQ                    n   overlay(IndDspF:57)
013000171129         //   - ?Riemissione videata?
013100171130     d   errGenerico                   n   overlay(IndDspF:99)
013200080107
013300171129       // - ?Parametri ricevuti?
013400080107     d KPJBA         e ds
013500080108     ***d TNTB62ds      e ds                  inz
013600080107
013700171130       // - ?Tabelle "MRA" = Bart-Mailing:?
013800171130       // · ?Standard?
013900080107     d dMRA          e ds                  inz
014000171130       // · ?ByPass Spool?
014100110613     d dMRAbyps      e ds                  inz  prefix(BYPS)
014200171130       // · ?C.E. Giornaliero?
014300150721     d dMRAceg       e ds                  inz  prefix(CEG)
014400171130       // · ?Danni?
014500171130     d dMRAdan       e ds                  inz  prefix(DAN)
014600171130       // · ?Mail di Conferma?
014700171130     d dMRAmk23      e ds                  inz  prefix(MK23)
014800171130       // · ?Mail con Materiale Illustrativo?
014900100805     d dMRAmk24      e ds                  inz  prefix(MK24)
015000171130       // · ?Mail con O.R.M. allegato?
015100171130     d dMRAor52      e ds                  inz  prefix(OR52)
015200171130       // · ?Output su file XLS?
015300171130     d dMRAxls       e ds                  inz  prefix(XLS)
015400080108
015500171129       // - ?Dati record principale della tabella?
015600080108     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015700080108     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
015800080108     d                                          prefix(TBX:3)
015900080107
016000100308       //---------------------------------------------------------------
016100171129       // ?Definizione variabili globali.                               ?
016200100308       //---------------------------------------------------------------
016300100308
016400171129       // - ?Flags booleani?
016500111202     d $InzD01         s               n   inz(*on)
016600111202     d $InzD02         s               n   inz(*on)
016700111202     d $InzW01         s               n   inz(*on)
016800111202     d $Fine           s               n   inz
016900111202     d $Avviso         s               n   inz
017000171211     d $ErrLungh       s               n   inz
017100111202
017200171129       // - ?Indici di schiera / Contatori?
017300120627     d xx              s              3  0 inz
017400080107
017500171129       // - ?Variabili per la gestione del video?
017600080107     d $Video          s              2    inz('D1')
017700080108     d wFxx            s              3    inz
017800111202
017900171129       // - ?Variabili di comodo?
018000120627     d wEMLindO        s                   like(§EMLindO)  inz
018100150721     d wOutQ           s                   like(V2CoutqI)  inz
018200080107
018300100308       //---------------------------------------------------------------
018400171129       // ?Definizione prototipi procedure.                             ?
018500100308       //---------------------------------------------------------------
018600100308
018700171129       // - ?Reperimento dati utente?
018800150721     d TIBS34ds      e ds
018900171130      /copy gaitrasrc/srcProtoPR,TIBS34R
019000100308
019100171129       // - ?Interrogazione tabelle?
019200150721     d TIBS02ds      e ds                  inz
019300150721     d   T02mod      e                     inz('R')
019400150721     d   T02cod      e                     inz(C_Tab)
019500171130      /copy gaitrasrc/srcProtoPR,TIBS02R
019600150721
019700171129       // - ?Interrogazione Regole Invii Spool via e-mail?
019800150721     d trtcM5ds        ds                  inz  qualified
019900150721     d   pOP0                         3    inz
020000150721     d   pOP1                         3    inz
020100150721     d   pCODr                        3    inz
020200150721     d   pMSG                        80    inz
020300150721     d trtcM5r         pr                  extpgm('TRTCM5R')
020400150721     d   kpjba                             likeds(kpjba)
020500100308
020600171129       // - ?Controllo & Inversione date?
020700150721     d WLBdat          ds                  inz
020800150721     d   G08dat                       8s 0 inz
020900150721     d   G08inv                       8s 0 inz
021000150721     d   G08err                       1    inz('3')
021100150721     d   G08tgi                       5s 0 inz
021200171130      /copy gaitrasrc/srcProtoPR,XSRDA8
021300111202
021400171129       // - ?Controllo indirizzo e-mail?
021500120627     d dsEMAIL       e ds                  inz
021600120627      /copy gaitrasrc/srcProtoPR,XEMAIL
021700150721
021800171129       // - ?API QCAPCMD (Process Commands)?
021900150721     d Qcmd            s           2048    inz  varying
022000150721      /copy qSysInc/qRpgleSrc,QCAPCMD
022100150721      /copy gaitrasrc/srcProtoPR,QCAPCMD
022200150721
022300171129       // - ?Parametri gestione errori API.?
022400150721      /copy qsysinc/qrpglesrc,QUSEC
022500080107
022600080107      //---------------------------------------------------------------
022700171129      // ?Definizione key-list.                                         ?
022800080107      //---------------------------------------------------------------
022900100308
023000171129       // - ?File TNTBE01L?
023100171130     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
023200171130     d                                     prefix( k_ )   inz
023300080107
023400100308       //--------------------------------------------------------------
023500171129       // ?Riepilogo indicatori.                                        ?
023600100308       //--------------------------------------------------------------
023700100308       // 25    - Comodo
023800100308       // 28    - Emissione del messaggio di errore a video
023900100308       // 99    - Generico di errore
024000100308       // x D01:
024100100308       // 51    - Codice 1 funzione            errato
024200100308       // 52    - Codice 2 funzione            errato
024300150721       // x D02...:
024400171130       // 53    - Indirizzo e-mail mittente    errato
024500150721       // 54    - Codice Regola                errato
024600171130       // 55    - Codice Regola allegati mail  errato
024700171130       // 56    - OutQ per input e-mail        errata
024800100308       //--------------------------------------------------------------
024900100308
025000100308       //--------------------------------------------------------------
025100171129       // ?M A I N - L I N E                                            ?
025200100308       //--------------------------------------------------------------
025300080107
025400080107     c     *Entry        plist
025500080107     c                   parm                    KPJBA
025600080107
025700080107      /free
025800080107
025900171129       // - ?Operazioni iniziali?
026000100805       exsr  sr_RoutInz;
026100080107
026200171129       // - ?Ciclo di gestione del file video?
026300100805       DOW  $Fine = *off;
026400171130
026500080107         select;
026600171130
026700171130           // - ?Filtro?
026800100805           when  $Video = 'D1';
026900100805             exsr  sr_GesD01;
027000171130
027100171130           // - ?Manutenzione singola tabella?
027200100805           when  $Video = 'D2';
027300100805             exsr  sr_GesD02;
027400171130
027500171130           // - ?Richiesta trasmissione dati?
027600100805           when  $Video = 'W1';
027700100805             exsr  sr_GesW01;
027800171130
027900171130           // - ?? ? ??
028000080107           other;
028100080107             leave;
028200171130
028300080107         endsl;
028400171130
028500080107       ENDDO;
028600080107
028700171129       // - ?Operazioni finali?
028800100805       exsr  sr_RoutEnd;
028900080107
029000080107       //--------------------------------------------------------------
029100171129       // ?Operazioni iniziali.                                         ?
029200080107       //--------------------------------------------------------------
029300100805       BEGSR  sr_RoutInz;
029400080107
029500100805         //if  kpjbu <> *blanks;
029600100308         //  TNTB62ds = kpjbu;
029700100308         //else;
029800100805         //  clear  TNTB62ds;
029900100308         //endif;
030000100308         //B62fxx = *blank;
030100100308         //B62err = *off;
030200100308         //B62msg = *blank;
030300080107
030400171129         // - ?Reperimento dati job?
030500100805         exsr  sr_DatiJob;
030600080107
030700171129         // - ?Impostazione nome programma a video?
030800080107         V1Tpgm = SDSpgm;
030900080107
031000171129         // - ?Verifica parametri ricecvuti?
031100100805         //if B62ke1 <> *blanks;
031200100805         //  exsr  sr_InzD01;
031300100805         //  exsr  sr_CtrD01;
031400100805         //  if  errMessage = *on;
031500100308         //    B62err = *on;
031600100308         //    B62msg = V1Dmsg;
031700100308         //    $Fine  = *on;
031800100308         //    leavesr;
031900100308         //  endif;
032000100308         //  $Video  = 'D2';
032100100308         //  $InzD02 = *on;
032200100308         //endif;
032300080108
032400171129         // - ?Aggancio dati generali della tabella in esame?
032500100805         clear  k_TBEcod;
032600100308         k_TBEke1 = *zero;
032700100308         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_tab)+1 : %len(c_tab))
032800100308                = c_tab;
032900100805         clear  k_TBEke2;
033000100805         clear  k_TBElin;
033100100308         k_TBEsif = KNSIF;
033200171130         chain  %kds(keyTNTBE01)  TNTBE000;
033300100805         if  not %found(TNTBE01L);
033400100805           clear  k_TBEsif;
033500171130           chain  %kds(keyTNTBE01)  TNTBE000;
033600080108         endif;
033700100805         if  %found(TNTBE01L);
033800080108           xTNTBEds = TNTBEds;
033900080108         else;
034000100805           clear  xTNTBEds;
034100080108         endif;
034200080107
034300080107       ENDSR;
034400080107
034500080107       //--------------------------------------------------------------
034600171129       // ?Reperimento Dati del job (Utente/Operativi).                 ?
034700080107       //--------------------------------------------------------------
034800100805       BEGSR  sr_DatiJob;
034900080107
035000080107         in(E) §AzUte;
035100080107         if NOT %error;
035200080107           in(E) §DatiUte;
035300080107         endif;
035400080107         if %error or RSut = *blanks;
035500080107           clear TIBS34ds;
035600080107           tibs34r(tibs34ds);
035700080107           in §AzUte;
035800080107           in §DatiUte;
035900080107         endif;
036000080107
036100080107       ENDSR;
036200080107
036300080107       //--------------------------------------------------------------
036400171129       // ?Gestione videata D01                                         ?
036500080107       //--------------------------------------------------------------
036600100805       BEGSR  sr_GesD01;
036700080107
036800100308         // -?Inizializzazione videata?
036900100805         if  $InzD01 = *on;
037000100805           exsr  sr_InzD01;
037100080107           $InzD01 = *off;
037200080107         endif;
037300080107
037400171129         // - ?Emissione testata?
037500100805         write  TB62T01;
037600080107
037700171129         // - ?Emissione videata?
037800100805         exfmt  TB62D01;
037900080107         errMessage  = *off;
038000080107         errGenerico = *off;
038100100805         clear  V1Dmsg;
038200080107
038300080107         select;
038400171129           // - ?F3=Fine?
038500100805           when  dsp_aid = c_F03;
038600100805             exsr  sr_F03D01;
038700171129           // - ?Enter?
038800080107           other;
038900100805             exsr  sr_CtrD01;
039000100805             if  errGenerico;
039100080107               leavesr;
039200080107             endif;
039300080107             $Video  = 'D2';
039400080107             $InzD02 = *on;
039500080107         endsl;
039600080107
039700080107       ENDSR;
039800080107
039900080107       //--------------------------------------------------------------
040000171129       // ?Inizializzazione videata D01                                 ?
040100080107       //--------------------------------------------------------------
040200100805       BEGSR  sr_InzD01;
040300080107
040400100805         clear  TB62D01;
040500080107
040600100805         //if  B62ke1 = *blank;
040700100308             V1Cke1 = '?';
040800100308         //else;
040900100308         //  V1Cke1 = %trimr(B62ke1);
041000100308         //  V1Cke2 = %trimr(B62ke2);
041100100308         //endif;
041200080107
041300080107       ENDSR;
041400080107
041500080107       //--------------------------------------------------------------
041600171129       // ?Controllo videata D01                                        ?
041700080107       //--------------------------------------------------------------
041800100805       BEGSR  sr_CtrD01;
041900080107
042000080107         IndDspF = *zeros;
042100080107
042200080107         SELECT;
042300080107
042400171129           // - ?Ricerca codice cliente?
042500100805           WHEN  %scan('?' : V1Cke1) > *zeros   or
042600100805                 %scan('?' : V1Cke2) > *zeros;
042700100805             reset  TIBS02ds;
042800100308             //T02mod = 'R';     (già così)
042900100308             //T02cod = C_Tab;   (già così)
043000080107             T02sif = KNSIF;
043100171130             TNTBE_RicercaControllo ( kpjba : tibs02ds );
043200100805             if  T02err = *blanks;
043300080107               V1Cke1 = T02ke1;
043400080107               V1Cke2 = T02ke2;
043500171211               if  T02ke1 <> *blank;
043600171211                 clear  V1Ctip;
043700171211               endif;
043800080107               errGenerico = *on;
043900080107               leavesr;
044000080107             else;
044100080107               errMessage  = *on;
044200080107               errGenerico = *on;
044300080107               PosCurKe1   = *on;
044400080107               V1Dmsg = T02msg;
044500080107               leavesr;
044600080107             endif;
044700080107
044800171129           // - ?Controllo immissione chiave tabella?
044900100805           WHEN  V1Cke1 = *blanks;
045000080107             errMessage  = *on;
045100080107             errGenerico = *on;
045200080107             PosCurKe1   = *on;
045300080107             V1Dmsg = $Msg(01);
045400080107             leavesr;
045500080107
045600080107         ENDSL;
045700080107
045800171129         // - ?Controllo esistenza tabella?
045900100805         clear  TNTBE000;
046000171130         clear  keyTNTBE01;
046100100805         k_TBEcod = c_Tab;
046200100805         k_TBEke1 = V1Cke1;
046300100805         k_TBEke2 = V1Cke2;
046400171130         chain  %kds(keyTNTBE01)  TNTBE000;
046500171130
046600171130         // - ?Controllo tipo tabella?
046700171130         select;
046800171130           // - ?Controllo immissione tipo tabella (SE inserimento)?
046900171130           when  NOT %found( TNTBE01L )  and
047000171130                 V1Ctip = *blanks;
047100171130             errMessage  = *on;
047200171130             errGenerico = *on;
047300171130             PosCurTip   = *on;
047400171130             V1Dmsg = $Msg(02);
047500171130             leavesr;
047600171130           // - ?Tipo tabella errato?
047700171130           when  NOT %found( TNTBE01L )  and
047800171130                 V1Ctip <> c_MRAstd      and
047900171130                 V1Ctip <> c_MRAbyps     and
048000171130                 V1Ctip <> c_MRAceg      and
048100171130                 V1Ctip <> c_MRAdan      and
048200171130                 V1Ctip <> c_MRAmk23     and
048300171130                 V1Ctip <> c_MRAmk24     and
048400171130                 V1Ctip <> c_MRAor52     and
048500171130                 V1Ctip <> c_MRAxls;
048600171130             errMessage  = *on;
048700171130             errGenerico = *on;
048800171130             PosCurTip   = *on;
048900171130             V1Dmsg = $Msg(03);
049000171130             leavesr;
049100171130           // - ?Tipo tabella NON inseribile (SE modifica)?
049200171130           when  %found( TNTBE01L )  and
049300171130                 V1Ctip <> *blank;
049400171130             errMessage  = *on;
049500171130             errGenerico = *on;
049600171130             PosCurTip   = *on;
049700171130             V1Dmsg = $Msg(04);
049800171130             leavesr;
049900171130           // - ?Tipo tabella NON registrato (SE modifica)?
050000171130           when  %found( TNTBE01L )  and
050100171206                 %subst( TBEuni : c_MAX_Len + 1 : 4 ) = *blank;
050200171130             errMessage  = *on;
050300171130             errGenerico = *on;
050400171130             PosCurTip   = *on;
050500171205             V1Dmsg = $Msg(08);
050600171130             leavesr;
050700171130         endsl;
050800171205
050900171205         // - ?Lunghezza DS della tabella errato (in IMMISSIONE)?
051000171205         if  ( V1Ctip = c_MRAstd   and  %len(dMRA    ) > c_Max_Len )  OR
051100171205             ( V1Ctip = c_MRAbyps  and  %len(dMRAbyps) > c_Max_Len )  OR
051200171205             ( V1Ctip = c_MRAceg   and  %len(dMRAceg ) > c_Max_Len )  OR
051300171205             ( V1Ctip = c_MRAdan   and  %len(dMRAdan ) > c_Max_Len )  OR
051400171205             ( V1Ctip = c_MRAmk23  and  %len(dMRAmk23) > c_Max_Len )  OR
051500171205             ( V1Ctip = c_MRAmk24  and  %len(dMRAmk24) > c_Max_Len )  OR
051600171205             ( V1Ctip = c_MRAor52  and  %len(dMRAor52) > c_Max_Len )  OR
051700171205             ( V1Ctip = c_MRAxls   and  %len(dMRAxls ) > c_Max_Len );
051800171205           errMessage  = *on;
051900171205           errGenerico = *on;
052000171205           PosCurTip   = *on;
052100171206           V1Dmsg = %replace( %char( c_Max_Len ) : $Msg(09) :
052200171206                              %scan( '___' : $Msg(09) ) );
052300171205           leavesr;
052400171205         endif;
052500080107
052600080107       ENDSR;
052700080107
052800080107       //--------------------------------------------------------------
052900171129       // ?Gestione tasto funzionale F3 da videata D01                  ?
053000171129       // ?F3=Fine                                                      ?
053100080107       //--------------------------------------------------------------
053200100805       BEGSR  sr_F03D01;
053300080107
053400171129         // - ?Chiusura del programma?
053500080107         $Fine = *on;
053600080107
053700171129         // - ?Restituzione indicazione del tasto funzionale premuto?
053800100308         //B62fxx = '1';
053900080107
054000080107       ENDSR;
054100080107
054200080107       //--------------------------------------------------------------
054300171129       // ?Gestione videata D02                                         ?
054400080107       //--------------------------------------------------------------
054500100805       BEGSR  sr_GesD02;
054600080107
054700171129         // - ?Inizializzazione videata?
054800100805         if  $InzD02  = *on;
054900100805           exsr  sr_InzD02;
055000080107           $InzD02  = *off;
055100080107         endif;
055200080107
055300171129         // - ?Emissione testata?
055400100805         write  TB62T01;
055500171130
055600171129         // - ?Emissione piede?
055700110613         write  TB62P02;
055800080107
055900171129         // - ?Emissione videata?
056000110613         select;
056100171130           // - ?Tab. "MRA" con ds "STD " (dMRA    )?
056200171130           when  V2Ctip = c_MRAstd;
056300171130             exfmt  TB62D02std;
056400171130           // - ?Tab. "MRA" con ds "BYPS" (dMRABYPS)?
056500150721           when  V2Ctip = c_MRAbyps;
056600150721             exfmt  TB62D02byp;
056700171130           // - ?Tab. "MRA" con ds "CEG " (dMRACEG )?
056800150721           when  V2Ctip = c_MRAceg;
056900150721             exfmt  TB62D02ceg;
057000171130           // - ?Tab. "MRA" con ds "DAN " (dMRADAN )?
057100150721           when  V2Ctip = c_MRAdan;
057200150721             exfmt  TB62D02dan;
057300171130           // - ?Tab. "MRA" con ds "MK23" (dMRAMK23)?
057400171130           when  V2Ctip = c_MRAmk23;
057500171130             exfmt  TB62D02m23;
057600171130           // - ?Tab. "MRA" con ds "MK24" (dMRAMK24)?
057700171130           when  V2Ctip = c_MRAmk24;
057800171130             exfmt  TB62D02m24;
057900171130           // - ?Tab. "MRA" con ds "OR52" (dMRAOR52)?
058000171130           when  V2Ctip = c_MRAor52;
058100171130             exfmt  TB62D02o52;
058200171130           // - ?Tab. "MRA" con ds "XLS " (dMRAXLS )?
058300150721           when  V2Ctip = c_MRAxls;
058400150721             exfmt  TB62D02xls;
058500110613         endsl;
058600080107         errMessage  = *off;
058700080107         errGenerico = *off;
058800080107         clear V1Dmsg;
058900080107
059000080107         SELECT;
059100171211           //*·// - ?F1=Help?
059200171211           //*·WHEN  dsp_aid = c_F01;
059300171211           //*·  exfmt  TB62W02;
059400171129           // - ?F3=Fine?
059500131129           WHEN  dsp_aid = c_F03;
059600131129             exsr  sr_F03D01;
059700171129           // - ?F12=Ritorno?
059800100308           WHEN  dsp_aid = c_F12;
059900100805             exsr  sr_F12D02;
060000171129           // - ?Enter o F5 o F6 o F16?
060100080107           OTHER;
060200171129             // - ?Controllo dati?
060300100308             exsr sr_CtrD02;
060400080107             if errGenerico;
060500080107               leavesr;
060600080107             endif;
060700171129             // - ?Conferma dati:?
060800171129             //   ?F5=Ripristino; F6=Conferma; F16=Annullamento?
060900100308             If  dsp_aid = c_F05   or
061000100308                 dsp_aid = c_F06   or
061100100308                 dsp_aid = c_F16;
061200171129               //// ?NON aggiornare qui, ma alla conferma della window !!!?
061300150721               //exsr sr_Upd_TNTBE;
061400150721               //if  NOT  errGenerico;
061500150721               //  clear V1Topz;
061600150721               //  $Video  = 'D1';
061700150721               ////$InzD01 = *on;
061800150721               //endif;
061900100805               clear  wFxx;
062000080108               select;
062100150721                 when  dsp_aid = c_F05;
062200150721                   eval  wFxx = 'F05';
062300150721                 when  dsp_aid = c_F06;
062400150721                   eval  wFxx = 'F06';
062500150721                 when  dsp_aid = c_F16;
062600150721                   eval  wFxx = 'F16';
062700080108               endsl;
062800080108               $Video  = 'W1';
062900080108               $InzW01 = *on;
063000080107             EndIf;
063100080107
063200080107         ENDSL;
063300080107
063400080107       ENDSR;
063500080107
063600080107       //--------------------------------------------------------------
063700171129       // ?Inizializzazione videata D02                                 ?
063800080107       //--------------------------------------------------------------
063900100805       BEGSR  sr_InzD02;
064000080107
064100171130         clear  TB62D02std;
064200171130         clear  TB62D02byp;
064300150721         clear  TB62D02ceg;
064400100805         clear  TB62D02dan;
064500171130         clear  TB62D02m23;
064600171130         clear  TB62D02m24;
064700171130         clear  TB62D02o52;
064800110613         clear  TB62D02xls;
064900080107
065000171211         IndDspF   = *zeros;
065100171211         $ErrLungh = *off;
065200080107
065300171129         // - ?Impostazione indicatori per abilitazione tasti funzionali?
065400100805         exsr  sr_Set_Ind_D02;
065500111202
065600111202         reset  $Avviso;
065700120627         clear  wEMLindO;
065800080107
065900100805         clear  dMRA;
066000110613         clear  dMRAbyps;
066100150721         clear  dMRAceg;
066200100805         clear  dMRAdan;
066300171130         clear  dMRAmk23;
066400171130         clear  dMRAmk24;
066500171130         clear  dMRAor52;
066600110613         clear  dMRAxls;
066700171130
066800080107         V2Cke1 = V1Cke1;
066900080107         V2Cke2 = V1Cke2;
067000080107
067100100805         IF  %found(TNTBE01L);
067200080107
067300171130           // - ?Dati già esistenti: si usa il tipo reperito in TBEUNI?
067400171206           V2Ctip = %subst( TBEuni : c_MAX_Len + 1 : 4 );
067500171130
067600171130           // - ?Impostazione dati di record trovato?
067700080107           select;
067800100805             //when  B62opz = '5';
067900171201             //  V1Topz = '  VISUALIZZA   ';
068000100805             when  TBEatb = *blanks;
068100171201               V1Topz = '  VARIAZIONE   ';
068200080107             other;
068300171201               V1Topz = '   ANNULLATO   ';
068400080107           endsl;
068500080107
068600080107           select;
068700171130             // - ?Tab. "MRA" standard (dMRA  )?
068800171130             when  V2Ctip = c_MRAstd;
068900171130               dMRA       = TBEuni;
069000171130               V2Cdes     = §MRAdes;
069100171130               V2Creg     = §MRAreg;
069200171130               V2Cmit1l   = §MRAmit1l;
069300171130               V2Cmit2l   = §MRAmit2l;
069400171130               V2Cmites   = §MRAmites;
069500171130               V2Cidpro   = §MRAidpro;
069600171130               V2Coutqi   = §MRAoutqi;
069700171130             // - ?Tab. "MRA" "BYPS" (dMRABYPS)?
069800171130             when  V2Ctip = c_MRAbyps;
069900171130               dMRAbyps   = TBEuni;
070000171130               V2CYdes    = byps§MRAdes;
070100171130               V2CYidpro  = byps§MRAidpro;
070200171130               V2CYreg    = byps§MRAreg;
070300171130               V2CYren    = byps§MRAregnew;
070400171130               V2CYfil    = byps§MRApo;
070500171130               V2CYsplnam = byps§MRAsplnam;
070600171130               V2CYemlmit = byps§MRAemlmit;
070700171130               V2CYusrdta = byps§MRAusrdta;
070800171130             // - ?Tab. "MRA" "CEG " (dMRACEG )?
070900171130             when  V2Ctip = c_MRAceg;
071000171130               dMRAceg    = TBEuni;
071100171130               V2CCdes    = ceg§MRAdes;
071200171130               V2CCem     = ceg§MRAem;
071300171130               V2CCcc     = ceg§MRAcc;
071400171130             // - ?Tab. "MRA" "DAN " (dMRADAN )?
071500171130             when  V2Ctip = c_MRAdan;
071600171130               dMRAdan    = TBEuni;
071700171130               V2CDdes    = dan§MRAddes;
071800171130               V2CDreg    = dan§MRAdreg;
071900171130               V2CDmitt   = dan§MRAdmitt;
072000171130               V2CDdest   = dan§MRAddest;
072100171130               V2CDidpro  = dan§MRAdidpro;
072200171130               V2CDoutqi  = dan§MRAdoutqi;
072300171130             // - ?Tab. "MRA" "MK23" (dMRAMK23)?
072400171130             when  V2Ctip = c_MRAmk23;
072500171130               dMRAmk23   = TBEuni;
072600171130               V2CMdes23  = mk23§MRAdes;
072700171130               V2CMregcor = mk23§MRAregCor;
072800171130               V2CMregall = mk23§MRAregAll;
072900171130               V2CMmitt   = mk23§MRAmitt;
073000171130               V2CMobjm23 = mk23§MRAobjm;
073100171130               V2CMidpro  = mk23§MRAidpro;
073200171130               V2CMoutqi  = mk23§MRAoutqi;
073300171130               V2CMpcl    = mk23§MRApcl;
073400171130               V2CMsts    = mk23§MRAsts;
073500171130             // - ?Tab. "MRA" "MK24" (dMRAMK24)?
073600171130             when  V2Ctip = c_MRAmk24;
073700100805               dMRAmk24   = TBEuni;
073800171130               V2CMdes24  = mk24§MRAdes;
073900171130               V2CMregcor = mk24§MRAregCor;
074000171130               V2CMregall = mk24§MRAregAll;
074100171130               V2CMmitt   = mk24§MRAmitt;
074200171130               V2CMobjm24 = mk24§MRAobjm;
074300171130               V2CMidpro  = mk24§MRAidpro;
074400171130               V2CMoutqi  = mk24§MRAoutqi;
074500171130               V2CMpcl    = mk24§MRApcl;
074600171130               V2CMsts    = mk24§MRAsts;
074700171130             // - ?Tab. "MRA" "OR52" (dMRAOR52)?
074800171130             when  V2Ctip = c_MRAor52;
074900171130               dMRAor52   = TBEuni;
075000171130               V2COdes52  = or52§MRAdes;
075100171130               V2COregcor = or52§MRAregCor;
075200171130               V2COregall = or52§MRAregAll;
075300171130               V2COmitt   = or52§MRAmitt;
075400171130               V2COobjm   = or52§MRAobjm;
075500171130               V2COidpro  = or52§MRAidpro;
075600171130               V2COoutqi  = or52§MRAoutqi;
075700171130               V2COpcl    = or52§MRApcl;
075800171130               V2COsts    = or52§MRAsts;
075900171130             // - ?Tab. "MRA" "XLS " (dMRAXLS )?
076000110613             when  V2Ctip = c_MRAxls;
076100110613               dMRAxls    = TBEuni;
076200171130               V2CXdes    = xls§MRAdes;
076300171130               V2CXreg    = xls§MRAreg;
076400171130               V2CXfldid  = xls§MRAfldid;
076500171130               V2CXidpro  = xls§MRAidpro;
076600171130               V2CXoutqi  = xls§MRAoutqi;
076700080107           endsl;
076800171129           // - ?Decodifica Regola?
076900150721           exsr  sr_CtrD02;
077000150721           clear  V1Dmsg;
077100150721           clear  ErrMessage;
077200150721           clear  ErrGenerico;
077300080107
077400080107         ELSE;
077500080107
077600171130           // - ?Dati NON trovati: si usa il tipo inserito a video?
077700171130           V2Ctip = V1Ctip;
077800171130
077900171129           // - ?Impostazione dati di record nuovo?
078000171201           V1Topz = '  IMMISSIONE   ';
078100080107
078200171205         ENDIF;
078300171205
078400171205         // - ?Lunghezza DS della tabella errato (in MODIFICA)?
078500171205         if  ( V2Ctip = c_MRAstd   and  %len(dMRA    ) > c_Max_Len )  OR
078600171205             ( V2Ctip = c_MRAbyps  and  %len(dMRAbyps) > c_Max_Len )  OR
078700171205             ( V2Ctip = c_MRAceg   and  %len(dMRAceg ) > c_Max_Len )  OR
078800171205             ( V2Ctip = c_MRAdan   and  %len(dMRAdan ) > c_Max_Len )  OR
078900171205             ( V2Ctip = c_MRAmk23  and  %len(dMRAmk23) > c_Max_Len )  OR
079000171205             ( V2Ctip = c_MRAmk24  and  %len(dMRAmk24) > c_Max_Len )  OR
079100171205             ( V2Ctip = c_MRAor52  and  %len(dMRAor52) > c_Max_Len )  OR
079200171205             ( V2Ctip = c_MRAxls   and  %len(dMRAxls ) > c_Max_Len );
079300171211           $ErrLungh   = *on;
079400171205           errMessage  = *on;
079500171205           errGenerico = *on;
079600171205           PosCurTip   = *on;
079700171206           V1Dmsg = %replace( %char( c_Max_Len ) : $Msg(09) :
079800171206                              %scan( '___' : $Msg(09) ) );
079900171205           leavesr;
080000171205         endif;
080100080107
080200080107       ENDSR;
080300080107
080400080107       //--------------------------------------------------------------
080500171129       //? Settaggio indicatori nella videata D02 per abilitazione      ?
080600171129       //? tasti funzionali                                           ?
080700080107       //--------------------------------------------------------------
080800100805       BEGSR  sr_Set_Ind_D02;
080900080107
081000100308         //F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blanks)
081100100308         //                               and B62opz <> '5');
081200100308         //F6Attivo  = (NOT F5Attivo and B62opz <> '5');
081300100308         //F16Attivo = ((%found(TNTBE01L) and TBEatb = *blanks)
081400100308         //                               and B62opz <> '5');
081500080107         F5Attivo  = ((%found(TNTBE01L) and TBEatb <> *blanks));
081600080107         F6Attivo  = (NOT F5Attivo);
081700080107         F16Attivo = ((%found(TNTBE01L) and TBEatb = *blanks));
081800080107
081900080107       ENDSR;
082000080107
082100080107       //--------------------------------------------------------------
082200171129       //? Gestione tasto funzionale F12 da videata D02                 ?
082300171129       //? F12=Ritorno                                                  ?
082400080107       //--------------------------------------------------------------
082500100805       BEGSR  sr_F12D02;
082600080107
082700171130         // - ?Disallocazione record?
082800100805         unlock  TNTBE01L;
082900100805
083000171130         // - ?Ritorno alla videata precedente se NON richiamato?
083100100308         //if B62opz = *blank  and
083200100308         //   B62sif = *blank  and  B62lin = *blank  and
083300100308         //   B62ke1 = *blank  and  B62ke2 = *blank;
083400171130           clear V1Topz;
083500171130           $Video = 'D1';
083600171130         // - ?Ritorno al pgm chiamante se richiamato?
083700100308         //else;
083800100308         //  B62fxx = '2';
083900100308         //  $Fine = *on;
084000100308         //endif;
084100080107
084200080107       ENDSR;
084300080107
084400080107       //--------------------------------------------------------------
084500171129       //? Controllo videata D02                                        ?
084600080107       //--------------------------------------------------------------
084700100805       BEGSR  sr_CtrD02;
084800080107
084900171129         // - ?Spegnimento indicatori di posizionamento cursore?
085000171130         %subst( IndDspF : 28 ) = *off;
085100171211
085200171211         // - ?Numero caratteri inseribili eccessivo?
085300171211         if  $ErrLungh;
085400171211           errMessage  = *on;
085500171211           errGenerico = *on;
085600171211           PosCurTip   = *on;
085700171211           V1Dmsg = %replace( %char( c_Max_Len ) : $Msg(09) :
085800171211                              %scan( '___' : $Msg(09) ) );
085900171211           leavesr;
086000171211         endif;
086100150721
086200171129         // - ?Selezione codice Regola?
086300150721         clear  trtcM5ds;
086400171130         IF  ( V2Ctip = c_MRAstd  and  %scan( '?' : V2Creg     ) > *zero ) OR
086500171130             ( V2Ctip = c_MRAbyps and  %scan( '?' : V2CYreg    ) > *zero ) OR
086600171130             ( V2Ctip = c_MRAbyps and  %scan( '?' : V2CYren    ) > *zero ) OR
086700171130             ( V2Ctip = c_MRAdan  and  %scan( '?' : V2CDreg    ) > *zero ) OR
086800171130             ( V2Ctip = c_MRAmk23 and  %scan( '?' : V2CMregCor ) > *zero ) OR
086900171130             ( V2Ctip = c_MRAmk23 and  %scan( '?' : V2CMregAll ) > *zero ) OR
087000171130             ( V2Ctip = c_MRAmk24 and  %scan( '?' : V2CMregCor ) > *zero ) OR
087100171130             ( V2Ctip = c_MRAmk24 and  %scan( '?' : V2CMregAll ) > *zero ) OR
087200171130             ( V2Ctip = c_MRAor52 and  %scan( '?' : V2COregCor ) > *zero ) OR
087300171130             ( V2Ctip = c_MRAor52 and  %scan( '?' : V2COregAll ) > *zero ) OR
087400171130             ( V2Ctip = c_MRAxls  and  %scan( '?' : V2CXreg    ) > *zero );
087500150721
087600150721           trtcM5ds.pOP0 = 'P01';
087700150721           kpjbu    = trtcM5ds;
087800150721           trtcM5r ( kpjba );
087900150721           trtcM5ds = kpjbu;
088000150721
088100150721           Select;
088200171130             // - ?Non ricevuto nulla?
088300150721             When  trtcM5ds.pCODr = *blank;
088400171130             // - ?Tab. "MRA" standard (dMRA    )?
088500171130             When  V2Ctip = c_MRAstd;
088600171130               V2Creg     = trtcM5ds.pCODr;
088700171130             // - ?Tab. "MRA" "BYPS" (dMRABYPS)?
088800170201             When  V2Ctip = c_MRAbyps and  %scan( '?' : V2CYreg ) > *zero;
088900171130               V2CYreg    = trtcM5ds.pCODr;
089000170201             When  V2Ctip = c_MRAbyps and  %scan( '?' : V2CYren ) > *zero;
089100171130               V2CYren    = trtcM5ds.pCODr;
089200171130             // - ?Tab. "MRA" "DAN " (dMRADAN )?
089300171130             When  V2Ctip = c_MRAdan  and  %scan( '?' : V2CDreg ) > *zero;
089400171130               V2CDreg    = trtcM5ds.pCODr;
089500171130             // - ?Tab. "MRA" "MK23" (dMRAMK23)?
089600171130             When  V2Ctip = c_MRAmk23 and  %scan( '?' : V2COregCor ) > *zero;
089700150721               V2COregCor = trtcM5ds.pCODr;
089800171130             When  V2Ctip = c_MRAmk23 and  %scan( '?' : V2COregAll ) > *zero;
089900171130               V2COregAll = trtcM5ds.pCODr;
090000171130             // - ?Tab. "MRA" "MK24" (dMRAMK24)?
090100171130             When  V2Ctip = c_MRAmk24 and  %scan( '?' : V2COregCor ) > *zero;
090200171130               V2COregCor = trtcM5ds.pCODr;
090300171130             When  V2Ctip = c_MRAmk24 and  %scan( '?' : V2COregAll ) > *zero;
090400171130               V2COregAll = trtcM5ds.pCODr;
090500171130             // - ?Tab. "MRA" "OR52" (dMRAOR52)?
090600171130             When  V2Ctip = c_MRAor52 and  %scan( '?' : V2COregCor ) > *zero;
090700171130               V2COregCor = trtcM5ds.pCODr;
090800171130             When  V2Ctip = c_MRAor52 and  %scan( '?' : V2COregAll ) > *zero;
090900150721               V2COregAll = trtcM5ds.pCODr;
091000171130             // - ?Tab. "MRA" "XLS " (dMRAXLS)?
091100150721             When  V2Ctip = c_MRAxls;
091200150721               V2CXreg = trtcM5ds.pCODr;
091300150721           EndSl;
091400150721
091500150721         ENDIF;
091600150721
091700171129         // - ?Controllo codice Regola?
091800150721         clear  V2Dreg;
091900170201         clear  V2Dren;
092000150721         Select;
092100171130           // - ?Tab. "MRA" standard (dMRA    )?
092200171130           When  V2Ctip = c_MRAstd  and  V2Creg   <> *blank;
092300150721             chain  ( V2Creg )  TIMAR000;
092400150721             PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
092500171201             if  %found( TIMAR01L );
092600171201               V2Dreg = MARcodD;
092700171201             else;
092800171201               V2Dreg = *all'? ';
092900171201             endif;
093000171130           // - ?Tab. "MRA" "BYPS" (dMRABYPS)?
093100170201           When  V2Ctip = c_MRAbyps;
093200170201             if V2CYreg    <> *blank;
093300170201               chain  ( V2CYreg )  TIMAR000;
093400170201               PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
093500171201               if  %found( TIMAR01L );
093600171201                 V2Dreg = MARcodD;
093700171201               else;
093800171201                 V2Dreg = *all'? ';
093900171201               endif;
094000170201             endif;
094100170201             if V2CYren    <> *blank;
094200170201               chain  ( V2CYren )  TIMAR000;
094300170201               PosCurReg2 = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
094400171201               if  %found( TIMAR01L );
094500171201                 V2Dreg = MARcodD;
094600171201               else;
094700171201                 V2Dreg = *all'? ';
094800171201               endif;
094900170201             endif;
095000171130           // - ?Tab. "MRA" "DAN " (dMRADAN )?
095100171130           When  V2Ctip = c_MRAdan  and  V2CDreg  <> *blank;
095200150721             chain  ( V2CDreg )  TIMAR000;
095300150721             PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
095400171201             if  %found( TIMAR01L );
095500171201               V2Dreg = MARcodD;
095600171201             else;
095700171201               V2Dreg = *all'? ';
095800171201             endif;
095900171130           // - ?Tab. "MRA" "MK23" (dMRAMK23)?
096000171130           //   ?           "MK24" (dMRAMK24)?
096100171130           //   ?           "OR52" (dMRAOR52)?
096200171130           When  V2Ctip = c_MRAmk23  or
096300171201                 V2Ctip = c_MRAmk24;
096400171201             clear  V2DMregCor;
096500171201             clear  V2DOregCor;
096600171201             clear  V2DMregAll;
096700171201             clear  V2DOregAll;
096800171201             if  V2CMregCor <> *blank;
096900171201               chain  ( V2CMregCor )  TIMAR000;
097000171201               PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
097100171201               if  %found( TIMAR01L );
097200171201                 V2DMregCor = MARcodD;
097300171201               else;
097400171201                 V2DMregCor = *all'? ';
097500171201               endif;
097600171201             endif;
097700171201             if  V2CMregAll <> *blank  and  %scan( '?' : V2CMregAll ) = *zero;
097800171201               chain  ( V2CMregAll )  TIMAR000;
097900171201               PosCurReg2 = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
098000171201               if  %found( TIMAR01L );
098100171201                 V2DMregAll = MARcodD;
098200171201               else;
098300171201                 V2DMregAll = *all'? ';
098400171201               endif;
098500171201             endif;
098600171201           When  V2Ctip = c_MRAor52;
098700150721             clear  V2DOregCor;
098800150721             clear  V2DOregAll;
098900150721             if  V2COregCor <> *blank;
099000150721               chain  ( V2COregCor )  TIMAR000;
099100150721               PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
099200171201               if  %found( TIMAR01L );
099300171201                 V2DOregCor = MARcodD;
099400171201               else;
099500171201                 V2DOregCor = *all'? ';
099600171201               endif;
099700150721             endif;
099800150721             if  V2COregAll <> *blank  and  %scan( '?' : V2COregAll ) = *zero;
099900150721               chain  ( V2COregAll )  TIMAR000;
100000150721               PosCurReg2 = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
100100171201               if  %found( TIMAR01L );
100200171201                 V2DOregAll = MARcodD;
100300171201               else;
100400171201                 V2DOregAll = *all'? ';
100500171201               endif;
100600150721             endif;
100700171130           // - ?Tab. "MRA" "XLS " (dMRAXLS )?
100800171130           When  V2Ctip = c_MRAxls  and  V2CXreg  <> *blank;
100900150721             chain  ( V2CXreg )  TIMAR000;
101000150721             PosCurReg  = ( Not %found(TIMAR01L)  or  MARatb <> *blank );
101100171201             if  %found( TIMAR01L );
101200171201               V2Dreg = MARcodD;
101300171201             else;
101400171201               V2Dreg = *all'? ';
101500171201             endif;
101600150721         EndSl;
101700171130         If  PosCurReg  or  PosCurReg2;
101800171130           ErrMessage  = *on;
101900171130           ErrGenerico = *on;
102000171130           V1Dmsg = $Msg(06);
102100171130           leavesr;
102200171130         EndIf;
102300171129         // - ?SE eseguita una selezione: uscita (dopo la decodifica)?
102400150721         if  trtcM5ds.pOP0 <> *blank;
102500150721           leavesr;
102600150721         endif;
102700100805
102800171129         // - ?Controllo indirizzo e-mail?
102900120627         clear  xx;
103000120627         clear  dsEMAIL;
103100120627         Select;
103200171130           // - ?Tipo "DAN " (indirizzo e-mail lungo 121 bytes)?
103300171129           //   ?(se unico indirizzo e-mail più lungo di 100 bytes:?
103400171129           //    ?non verrà controllato!!!)?
103500171130           When  V2Ctip = c_MRAdan   and  V2CDdest <> *blank;
103600120627             clear  wEMLindO;
103700120627             if  %subst( V2CDdest : %len( §EMLindI ) + 1 ) = *blank;
103800120627               §EMLindI = V2CDdest;
103900120627             else;
104000170918               For  xx = %len( §EMLindI )  DownTo 1;
104100120627                 if  %subst( V2CDdest : xx : 1 ) = *blank  or
104200120627                     %subst( V2CDdest : xx : 1 ) = ';';
104300120627                   §EMLindI = %subst( V2CDdest : 1 : xx - 1 );
104400120627                   leave;
104500120627                 endif;
104600120627               EndFor;
104700120627               if  §EMLindI = *blank  and  Not $Avviso;
104800120627                 $Avviso     = *on;
104900120627                 errMessage  = *on;
105000120627                 errGenerico = *on;
105100120627                 PosCurInd   = *on;
105200171130                 V1Dmsg = $Msg(05);
105300120627                 leavesr;
105400120627               endif;
105500120627             endif;
105600120627         EndSl;
105700120627
105800120627         If  §EMLindI <> *blank;
105900120627           xemail ( dsEMAIL );
106000120627           if  §EMLerrO <> *off;
106100120627             errMessage  = *on;
106200120627             errGenerico = *on;
106300120627             PosCurInd   = *on;
106400120627             V1Dmsg = §EMLmsgO;
106500120627             leavesr;
106600120627           endif;
106700171129           // - ?tipo "DAN" (indirizzo e-mail lungo 121 bytes)?
106800171129           //   ?(più indirizzi e-mail, per una lunghezza totale di?
106900171129           //    ?oltre 100 bytes)?
107000120627           if  V2Ctip = c_MRAdan   and
107100120627               %subst( V2CDdest : %len( §EMLindI ) + 1 ) <> *blank  and
107200120627               Not  $Avviso        and
107300120627               xx     > *zero      and
107400120627               %subst( V2CDdest : xx + 1 ) <> *blank;
107500120627             wEMLindO = §EMLindO;
107600120627             clear  dsEMAIL;
107700120627             §EMLindI = %subst( V2CDdest : xx + 1 );
107800120627             xemail ( dsEMAIL );
107900120627             if  §EMLerrO <> *off;
108000120627               errMessage  = *on;
108100120627               errGenerico = *on;
108200120627               PosCurInd   = *on;
108300120627               V1Dmsg = §EMLmsgO;
108400120627               leavesr;
108500120627             endif;
108600120627           endif;
108700171129           // - ?REimpostazione a video dell'indirizzo e-mail corretto?
108800120627           select;
108900120627             when  V2Ctip = c_MRAdan   and  V2CDdest   <> *blank;
109000120627               if  wEMLindO = *blank;
109100120627                 V2CDdest   = §EMLindO;
109200120627               else;
109300120627                 V2CDdest   = %trim( wEMLindO ) + ';' + %trim( §EMLindO );
109400120627               endif;
109500120627           endsl;
109600120627         EndIf;
109700150721
109800171129         // - ?Controllo Coda di stampa per Input e-mail?
109900150721         clear  wOutQ;
110000150721         select;
110100171130           when  ( V2Ctip = c_MRAstd  and  V2CoutqI  <> *blank );
110200150721             wOutQ = %trimL( V2CoutqI );
110300150721             V2CoutqI  = wOutQ;
110400150721           when  ( V2Ctip = c_MRAdan  and  V2CDoutqI <> *blank );
110500150721             wOutQ = %trimL( V2CDoutqI );
110600150721             V2CDoutqI = wOutQ;
110700171130           when  ( V2Ctip = c_MRAmk23 and  V2COoutqI <> *blank );
110800150721             wOutQ = %trimL( V2COoutqI );
110900150721             V2COoutqI = wOutQ;
111000171130           when  ( V2Ctip = c_MRAmk24 and  V2COoutqI <> *blank );
111100171130             wOutQ = %trimL( V2COoutqI );
111200171130             V2COoutqI = wOutQ;
111300171130           when  ( V2Ctip = c_MRAor52 and  V2COoutqI <> *blank );
111400171130             wOutQ = %trimL( V2COoutqI );
111500171130             V2COoutqI = wOutQ;
111600150721           when  ( V2Ctip = c_MRAxls  and  V2CXoutqI <> *blank );
111700150721             wOutQ = %trimL( V2CXoutqI );
111800150721             V2CXoutqI = wOutQ;
111900150721         endsl;
112000150721
112100150721         If  wOutQ <> *blank;
112200150721
112300150721           Qcmd = 'CHKOBJ obj('+ %trimR( wOutQ ) + ') objtype(*outq)';
112400150721           exsr  sr_ExecCmd;
112500150721           if  Qusei <> *blank;
112600150721             ErrGenerico = *on;
112700150721             ErrMessage  = *on;
112800150721             PosCurOutQ  = *on;
112900171130             V1Dmsg = $Msg(07);
113000150721             leavesr;
113100150721           endif;
113200150721
113300150721         ENDIF;
113400080107
113500080107       ENDSR;
113600080108
113700080108       //--------------------------------------------------------------
113800171129       // ?Gestione videata W01                                         ?
113900080108       //--------------------------------------------------------------
114000100805       BEGSR  sr_GesW01;
114100080108
114200171129         // - ?Inizializzazione videata?
114300080108         if $InzW01 = *on;
114400100308           exsr sr_InzW01;
114500080108           $InzW01 = *off;
114600080108         endif;
114700080108
114800171129         // - ?Emissione window?
114900080108         exfmt TB62W01;
115000080108         errMessage  = *off;
115100080108         errGenerico = *off;
115200080108         clear V1Dmsg;
115300080108
115400080108         select;
115500080108
115600171129           // - ?F12=Ritorno?
115700100308           when dsp_aid = c_F12;
115800100308             exsr sr_F12W01;
115900080108
116000171129           // - ?F6=Conferma?
116100100308           when dsp_aid = c_F06;
116200100308             exsr sr_Upd_TNTBE;
116300080108             if  NOT  errGenerico;
116400080108               clear V1Topz;
116500080108               $Video  = 'D1';
116600100308               //$InzD01 = *on;
116700080108             endif;
116800080108         endsl;
116900080108
117000080108       ENDSR;
117100080108
117200080108       //--------------------------------------------------------------
117300171129       // ?Inizializzazione videata W01                                 ?
117400080108       //--------------------------------------------------------------
117500100805       BEGSR  sr_InzW01;
117600080108
117700080108         clear TB62W01;
117800080108
117900080108         select;
118000171129           // - ?F5=Ripristino?
118100080108           when wFxx = 'F05';
118200080108             W1ftt = TBEftt;
118300171129           // - ?F6=Conferma (immissione)?
118400100308           when wFxx = 'F06'  and   NOT %found(TNTBE01L);
118500080108             W1ftt = TBXftt;
118600171129           // - ?F6=Conferma (modifica/ripristino)?
118700100308           when wFxx = 'F06'  and   %found(TNTBE01L);
118800080108             W1ftt = TBEftt;
118900171129           // - ?F16=Annullamento?
119000080108           when wFxx = 'F16';
119100080108             W1ftt = TBEftt;
119200080108         endsl;
119300080108
119400171129         // - ?Se NON immissione:?
119500171129         //   ?visualizza i dati relativi all'ultima trasmissione?
119600080109         if %found(TNTBE01L);
119700080108           W1flt  = TBEflt;
119800080108           W1ftr  = TBEftr;
119900080109           if TBEdtr <> *zero;
120000080108             reset WLBdat;
120100080108             G08inv = TBEdtr;
120200080108             xsrda8(WLBdat);
120300080108             W1dtr  = G08dat;
120400080109           endif;
120500080109         endif;
120600080108
120700080108       ENDSR;
120800080108
120900080108       //--------------------------------------------------------------
121000171129       // ?Gestione tasto funzionale F12 da videata W01                 ?
121100171129       // ?F12=Ritorno                                                  ?
121200080108       //--------------------------------------------------------------
121300100805       BEGSR  sr_F12W01;
121400080108
121500171129         // - ?Ritorno alla videata precedente?
121600100308         $Video = 'D2';
121700080108
121800080108       ENDSR;
121900080107
122000080107       //--------------------------------------------------------------
122100171129       // ?Aggiornamento record TNTBE00F (tab. LAC)                     ?
122200080107       //--------------------------------------------------------------
122300100805       BEGSR  sr_Upd_TNTBE;
122400080107
122500080107         SELECT;
122600171130           // - ?Standard?
122700171130           WHEN  V2Ctip = c_MRAstd;
122800110613             clear  dMRA;
122900110613             §MRAdes    = V2Cdes;
123000110613             §MRAreg    = V2Creg;
123100110613             §MRAmit1l  = V2Cmit1l;
123200110613             §MRAmit2l  = V2Cmit2L;
123300110613             §MRAmites  = V2Cmites;
123400110613             §MRAidpro  = V2Cidpro;
123500110613             §MRAoutqi  = V2Coutqi;
123600100308             TBEuni = dMRA;
123700171130           // - ?ByPass Spool?
123800110613           WHEN  V2Ctip = c_MRAbyps;
123900110613             clear  dMRAbyps;
124000110613             byps§MRAdes    = V2CYdes;
124100110613             byps§MRAidpro  = V2CYidpro;
124200110613             byps§MRAreg    = V2CYreg;
124300170201             byps§MRAregnew = V2CYren;
124400110613             byps§MRApo     = V2CYfil;
124500110613             byps§MRAsplnam = V2CYsplnam;
124600110613             byps§MRAusrdta = V2CYusrdta;
124700110613             byps§MRAemlmit = V2CYemlmit;
124800110613             TBEuni = dMRAbyps;
124900171130           // - ?C.E. Giornaliero?
125000150721           WHEN  V2Ctip = c_MRAceg;
125100150721             clear  dMRAceg;
125200150721             ceg§MRAdes = V2CCdes;
125300150721             ceg§MRAem  = V2CCem;
125400150721             ceg§MRAcc  = V2CCcc;
125500150721             TBEuni = dMRAceg;
125600171130           // - ?Danni?
125700110613           WHEN  V2Ctip = c_MRAdan;
125800110613             clear  dMRAdan;
125900171130             dan§MRADdes   = V2CDdes;
126000171130             dan§MRADreg   = V2CDreg;
126100171130             dan§MRADmitt  = V2CDmitt;
126200171130             dan§MRADdest  = V2CDdest;
126300171130             dan§MRADidpro = V2CDidpro;
126400171130             dan§MRADoutqi = V2CDoutqi;
126500100308             TBEuni = dMRAdan;
126600171130           // - ?Mail di Conferma?
126700171130           WHEN  V2Ctip = c_MRAmk23;
126800171130             clear  dMRAmk23;
126900171130             mk23§MRAdes    = V2CMdes23;
127000171130             mk23§MRAregCor = V2CMregcor;
127100171130             mk23§MRAregAll = V2CMregall;
127200171130             mk23§MRAmitt   = V2CMmitt;
127300171130             mk23§MRAidpro  = V2CMidpro;
127400171130             mk23§MRAoutqi  = V2CMoutqi;
127500171130             mk23§MRApcl    = V2CMpcl;
127600171130             mk23§MRAsts    = V2CMsts;
127700171130             mk23§MRAobjm   = V2CMobjm23;
127800171130             TBEuni = dMRAmk23;
127900171130           // - ?Mail con Materiale Illustrativo?
128000171130           WHEN  V2Ctip = c_MRAmk24;
128100110613             clear  dMRAmk24;
128200171130             mk24§MRAdes    = V2CMdes24;
128300171130             mk24§MRAregCor = V2CMregcor;
128400171130             mk24§MRAregAll = V2CMregall;
128500171130             mk24§MRAmitt   = V2CMmitt;
128600171130             mk24§MRAidpro  = V2CMidpro;
128700171130             mk24§MRAoutqi  = V2CMoutqi;
128800171130             mk24§MRApcl    = V2CMpcl;
128900171130             mk24§MRAsts    = V2CMsts;
129000171130             mk24§MRAobjm   = V2CMobjm24;
129100110613             TBEuni = dMRAmk24;
129200171130           // - ?Mail con O.R.M. allegato?
129300171130           WHEN  V2Ctip = c_MRAor52;
129400171130             clear  dMRAor52;
129500171130             or52§MRAdes    = V2COdes52;
129600171130             or52§MRAregCor = V2COregcor;
129700171130             or52§MRAregAll = V2COregall;
129800171130             or52§MRAmitt   = V2COmitt;
129900171130             or52§MRAidpro  = V2COidpro;
130000171130             or52§MRAoutqi  = V2COoutqi;
130100171130             or52§MRApcl    = V2COpcl;
130200171130             or52§MRAsts    = V2COsts;
130300171130             or52§MRAobjm   = V2COobjm;
130400171130             TBEuni = dMRAmk24;
130500171130           // - ?Output su file XLS?
130600110613           WHEN  V2Ctip = c_MRAxls;
130700110613             clear  dMRAxls;
130800171130             xls§MRAdes   = V2CXdes;
130900171130             xls§MRAreg   = V2CXreg;
131000171130             xls§MRAfldid = V2CXfldid;
131100171130             xls§MRAidpro = V2CXidpro;
131200171130             xls§MRAoutqi = V2CXoutqi;
131300100308             TBEuni = dMRAxls;
131400080107         ENDSL;
131500171130
131600171206         %subst( TBEuni : c_MAX_Len + 1 ) = V2Ctip;
131700080107
131800080109         TBEftt = W1ftt;
131900080109         TBEflt = W1flt;
132000171130         clear  TBEftr;
132100171130         //clear  TBEdtr;
132200080107
132300080107         select;
132400171129           // - ?F5=Ripristino?
132500100805           when  wFxx = 'F05';
132600100805             clear  TBEatb;
132700171129           // - ?F16=Annullamento?
132800100805           when  wFxx = 'F16';
132900080107             TBEatb = 'A';
133000080107         endsl;
133100080107
133200080107         IF  NOT  %found(TNTBE01L);
133300100805           clear  TBEsif;
133400100805           clear  TBElin;
133500171130           TBEapl = TBXapl;
133600100308           TBEcod = c_Tab;
133700080109           TBEke1 = V2Cke1;
133800080109           TBEke2 = V2Cke2;
133900100805           clear  TBEdtr;
134000100308           //_____________
134100080107           WRITE TNTBE000;
134200100308           //¯¯¯¯¯¯¯¯¯¯¯¯¯
134300080107         ELSE;
134400100308           //______________
134500080107           UPDATE TNTBE000;
134600100308           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
134700080107         ENDIF;
134800080107
134900080107       ENDSR;
135000150721
135100150721       //--------------------------------------------------------------
135200171129       // ?Esecuzione del comando (già impostato).                      ?
135300150721       //--------------------------------------------------------------
135400150721       BEGSR  sr_ExecCmd;
135500150721
135600150721         clear Qcap0100;
135700150721         Qcabcsdh = *off;
135800150721         Qcapa    = *off;
135900150721         Qcacmdss = *off;
136000150721         Qcaerved = *allX'00';
136100150721
136200150721         clear Qusec;
136300150721         Qusbprv  = %size(Qusec);
136400150721
136500150721         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
136600150721                           %size(Qcap0100) : 'CPOP0100' : *omit :
136700150721                           0 : 0 : Qusec);
136800150721
136900150721         //if  Qusei <> *blank;
137000150721         //  ...;
137100150721         //endif;
137200150721
137300150721       ENDSR;
137400080107
137500080107       //--------------------------------------------------------------
137600171129       // ?Operazioni finali.                                           ?
137700080107       //--------------------------------------------------------------
137800100805       BEGSR  sr_RoutEnd;
137900080107
138000150721         //kpjbu = TNTB62ds;
138100080107         *inLR = *on;
138200080107         return;
138300080107
138400080107       ENDSR;
138500100308
138600080107      /end-free
138700080107
138800080107       //--------------------------------------------------------------
138900171129       // ?Schiere a tempo di compilazione.                             ?
139000080107       //--------------------------------------------------------------
139100080107
139200171205** --?$Msg:?Messaggi di Errore?----------------------------------------------*
139300171211Immettere il codice funzione                                                    1
139400171220Immettere il tipo funzione da usare per l'inserimento                           2
139500171220Tipo funzione errato                                                            3
139600171220Tipo funzione non inseribile per modifica, ma solo per inserimento              4
139700171130AVVISO: indirizzo e-mail troppo lungo per il controllo formale. Premere Invio.  5
139800171130Codice Regola errato                                                            6
139900171130Coda di Stampa errata                                                           7
140000171220Tipo funzione NON registrato: dati NON manutenzionabili                         8
140100171206La lunghezza di questa tabella supera la massima consentita: ___ caratteri      9
