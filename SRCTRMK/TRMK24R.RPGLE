000100100803       //==============================================================
000200100809       //?Invio e-mail di materiale illustrativo al cliente            ?
000300100803       //==============================================================
000400100803
000500100803       //--------------------------------------------------------------
000600100803       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100803       //--------------------------------------------------------------
000800100803
000900100803     /*END
001000100803
001100100803       //--------------------------------------------------------------
001200100803       //?Specifiche di controllo.                                     ?
001300100803       //--------------------------------------------------------------
001400100803
001500100803     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100809     h dftactgrp(*no)
001700100809       // - Deve essere specificato DFTACTGRP(*NO) per un prototipo che
001800100809       //   non ha la parola chiave EXTPGM.
001900100809       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
002000100803
002100100803       //--------------------------------------------------------------
002200100803       //?Dichiarazione file.
002300100803       //--------------------------------------------------------------
002400100803
002500100803       // -?Organigramma filiale/aziendale?
002600100803     fAZORG01L  if   e           k disk
002700100806
002800100806       // -?Anagrafica lingue?
002900130828     f*//AZLIN03L  if   e           k disk
003000100803
003100130828       // -?Tabella "MRA"?
003200100803     fTNTBE01L  if   e           k disk
003300130828
003400130828      // -?Anagrafica Commerciali?
003500130828     fAZCMM01L  if   e           k disk
003600130828     fAZNTC01L  if   e           k disk
003700100803
003800100803       // -?Video parametri di lancio?
003900100809     fTRMK24D   cf   e             workstn usropn
004000100803     f                                     indds(IndDspF)
004100100803     f                                     infds(InfDspF)
004200100803
004300100803       // -?Spool da inviare via e-mail:?
004400100803       //   ·?Corpo del messaggio di posta elettronica?
004500100810     fPrtEMAIL  o    f  198        printer  oflind(*inOF)  usropn
004600100803
004700100803       //--------------------------------------------------------------
004800100803       //?Definizione costanti.                                        ?
004900100803       //--------------------------------------------------------------
005000100803
005100100803       // -?Dati di default?
005200100803     d c_Mittente      c                   const('ced')
005300120305     d c_at            c                   const('@brt.it')
005400100803     d c_Sede          c                   const('046')
005500100803
005600100803       // -?Comandi di override al PrtF "corpo e-mail"?
005700100803     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005800100803     d                                           file(PRTEMAIL) +
005900100810     d                                           pagesize(66 198) +
006000100810     d                                           lpi(6) cpi(15) +
006100100803     d                                           ovrscope(*actgrpdfn) +
006200100803     d                                           ')
006300100803     d c_CmdDltOvr     c                   const('DLTOVR +
006400100803     d                                            file(PRTEMAIL) +
006500100803     d                                            lvl(*actgrpdfn)')
006600100803
006700100803       // -?Tasti funzionali a video?
006800100803     d c_F01           c                   const(x'31')
006900100803     d c_F02           c                   const(x'32')
007000100803     d c_F03           c                   const(x'33')
007100100803     d c_F04           c                   const(x'34')
007200100803     d c_F05           c                   const(x'35')
007300100803     d c_F06           c                   const(x'36')
007400100803     d c_F07           c                   const(x'37')
007500100803     d c_F08           c                   const(x'38')
007600100803     d c_F09           c                   const(x'39')
007700100803     d c_F10           c                   const(x'3A')
007800100803     d c_F11           c                   const(x'3B')
007900100803     d c_F12           c                   const(x'3C')
008000100803     d c_F13           c                   const(x'B1')
008100100803     d c_F14           c                   const(x'B2')
008200100803     d c_F15           c                   const(x'B3')
008300100803     d c_F16           c                   const(x'B4')
008400100803     d c_F17           c                   const(x'B5')
008500100803     d c_F18           c                   const(x'B6')
008600100803     d c_F19           c                   const(x'B7')
008700100803     d c_F20           c                   const(x'B8')
008800100803     d c_F21           c                   const(x'B9')
008900100803     d c_F22           c                   const(x'BA')
009000100803     d c_F23           c                   const(x'BB')
009100100803     d c_F24           c                   const(x'BC')
009200100803     d c_Enter         c                   const(x'F1')
009300100803     d c_RollDown      c                   const(x'F4')
009400100803     d c_RollUp        c                   const(x'F5')
009500100803
009600100803       // -?Costante per controllo "caratteri solo numerici"?
009700100803     d c_Digits        c                   const('0123456789')
009800100803
009900100803       //--------------------------------------------------------------
010000100803       //?Definizione schiere.                                         ?
010100100803       //--------------------------------------------------------------
010200100803
010300100803       // -?Messaggi di errore?
010400100902     d $Msg            s             78    dim(19) ctdata perrcd(1)
010500100803
010600100803       //--------------------------------------------------------------
010700100803       //?Definizione aree dati.                                       ?
010800100803       //--------------------------------------------------------------
010900100803
011000100803       // -?Dati utente?
011100100803     d §AzUte        e ds                  extname(AZUTE00F)
011200100803     d                                     dtaara
011300100803     d §DatiUte      e ds                  extname(dDatiUte)
011400100803     d                                     dtaara
011500100803
011600100803       //--------------------------------------------------------------
011700100803       //?Definizione strutture dati.                                  ?
011800100803       //--------------------------------------------------------------
011900100803
012000100803       // -?Status ds?
012100100803     d Status         sds
012200100806     d   SDSpgm          *proc
012300100803
012400100803       // -?Indicatori su DspF?
012500100803     d IndDspF         ds
012600100803         // -?Emissione messaggio di errore?
012700100806     d   ErrMessage                    n   overlay(IndDspF : 28)
012800100809         // -?Condzionamento tipo di emissione?
012900100809     d   xDs4                          n   overlay(IndDspF : 34)
013000100803         // -?Posizionamento cursore?
013100100806     d   PosCurTlt                     n   overlay(IndDspF : 51)
013200100806     d   PosCurLin                     n   overlay(IndDspF : 52)
013300100806     d   PosCurRst1                    n   overlay(IndDspF : 53)
013400100806     d   PosCurRst2                    n   overlay(IndDspF : 54)
013500100806     d   PosCurMai1                    n   overlay(IndDspF : 55)
013600100806     d   PosCurMai2                    n   overlay(IndDspF : 56)
013700100806     d   PosCurObj                     n   overlay(IndDspF : 57)
013800100806     d   PosCurCmm                     n   overlay(IndDspF : 58)
013900100803
014000100803       // -?InfDS?
014100100803     d InfDspF         ds
014200100806     d   dsp_aid             369    369a
014300100803
014400100803       // -?Parametri ricevuti?
014500100803     d KPJBA         e ds
014600100806     d TRMK24ds      e ds
014700100803
014800100803       // -?Parametri per Reperimento dati utente?
014900100803     d TIBS34ds      e ds                  inz
015000100806
015100100806       // -?Tab. "1H" = Testi tariffe?
015200100806     d ds1H          e ds                  inz
015300100803
015400100805       // -?Tab. "MRA" = Bart-Mailing x TRMK24R?
015500100805     d dMRAmk24      e ds                  inz
015600100803
015700100803       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
015800100803     d TRTCM1ds      e ds                  inz
015900100803       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
016000100803     d   §CM1mitt    e                     inz('ced')
016100100803       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
016200120305     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
016300100803       //  ?·§CM1tips = Tipo lettera via e-mail:?
016400100803       //   ?"LET" = testo allegato in corpo con logo?
016500100803       //           ?(richiede righe libere iniziali per il logo)?
016600100803       //   ?"COR" = testo integrato senza logo?
016700100803       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
016800100803       //   ?"SP1" = spool allegato (in formato PDF)?
016900100806     d   §CM1tips    e                     inz('CMx')
017000100803       //  ?·§CM1po   = Filiale?
017100100803     d   §CM1po      e                     inz('046')
017200100803       //  ?·§CM1var  = Oggetto e-mail?
017300100803     d*//§CM1var     e                     inz('*OBJM*+
017400120305     d*//                                  Comunicazione da BRT S.p.A.')
017500100803     d   §CM1var     e                     inz('*OBJM*')
017600100803       //  ?·§CM1sts  = Stato?
017700100803     d   §CM1sts     e                     inz('H')
017800100803       //  ?·§CM1idp  = Id processo?
017900100803     d   §CM1idp     e                     inz('3')
018000100803       //  ?·§CM1att  = Numero attach corrente?
018100100803     d   §CM1att     e                     inz('01')
018200100803       //  ?·§CM1totatt  = Numero attach totali?
018300100803     d   §CM1totatt  e                     inz('01')
018400100803
018500100803       //--------------------------------------------------------------
018600100803       //?Definizione variabili.                                       ?
018700100803       //--------------------------------------------------------------
018800100803
018900100803       // -?Flags booleani?
019000100803     d $Fine           s               n   inz
019100100803     d $InzD01         s               n   inz(*on)
019200100810     d ErrGenerico     s               n   inz
019300100803
019400100803       // -?Indici di schiera?
019500100803     d xx              s              3  0 inz
019600100803
019700100803       // -?Variabili per la gestione del video?
019800100803     d $Video          s              2    inz('D1')
019900100803
020000100803       // -?Campi di stampa?
020100100810     d O_txt           s            198    inz
020200100806
020300100806       // -?Campi di comodo per la verifica di cambio codice?
020400100806     d SaveCMM         s                   like(D01cmm) inz
020500130828     d*// SaveLIN         s                   like(D01lin) inz
020600100915     d SaveMRA         s                   like(D01tlt) inz
020700111129
020800111129       // -?Campi di comodo
020900111129     d wOggi           s              8s 0
021000130828     d wNomeCmm        s                   like(NTCrnt)  inz
021100130828     d wMailCmm        s                   like(NTCrnt)  inz
021200100803
021300100803       //--------------------------------------------------------------
021400100908       //?Definizione prototipi & relativi parametri                   ?
021500100803       //--------------------------------------------------------------
021600100803
021700100803       // -?Reperimento dati utente?
021800100803      /copy gaitrasrc/srcProtoPR,TIBS34R
021900100803
022000100803       // -?Gestione numeratori in GRU?
022100100803     d TRUL33ds      e ds                  inz
022200100803     d   I33tla      e                     inz('L')
022300100803     d   I33ope      e                     inz(*zero)
022400100803     d   I33cnu      e                     inz(024)
022500100803     d   I33num      e                     inz(1)
022600100803      /copy gaitrasrc/srcProtoPR,TRUL33R
022700100803
022800100909       // -?Ricerca testi lettera?
022900100803     d FNLV41ds      e ds                  inz
023000100803     d   I41mod      e                     inz('1')
023100100803     d   I41prv      e                     inz('M')
023200100803      /copy gaitrasrc/srcProtoPR,FNLV41R
023300100803
023400100909       // -?Ricerca commerciali?
023500130828      /copy gaitrasrc/srcprotoPI,TRMK43R_1
023600130828      /copy gaitrasrc/srcprotoPR,TRMK43R
023700100805
023800100805       // -?Controllo abilitazioni utente?
023900100909     d TNTAA1ds      e ds                  inz
024000120810     d   iTAA1tipo   e                     inz('M')
024100120810     d   iTAA1caut   e                     inz('§utegtc')
024200100909     d   iTAA1abi    e                     inz('RA')
024300100805      /copy gaitrasrc/srcProtoPR,TNTAA1C
024400100806
024500100902       // -?Ricerca lingua su AZLIN?
024600130828     d*// P37_iso2        s                   like(LINiso2)   inz
024700130828     d*// P37_desi        s                   like(LINdesita) inz
024800130828     d*// trul37r         pr                  extpgm('TRUL37R')
024900130828     d*//   p37iso2                           like(P37_iso2)
025000130828     d*//   p37desi                           like(P37_desi)
025100100902
025200100902       // -?Controllo indirizzo e-mail?
025300100909     d dsEMAIL       e ds                  inz
025400100902      /copy gaitrasrc/srcProtoPR,XEMAIL
025500100809
025600100809       // -?API QSNDRTVMOD (Retrieve Display Mode)?
025700100809     d QdspMode        s              1    inz
025800100809      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
025900100803
026000100805       // -?API QCAPCMD (Process Commands)?
026100100803     d Qcmd            s           2048    inz  varying
026200100803      /copy qSysInc/qRpgleSrc,QCAPCMD
026300100803      /copy gaitrasrc/srcProtoPR,QCAPCMD
026400100803
026500100803       // -?Parametri gestione errori API.?
026600100803      /copy qsysinc/qrpglesrc,QUSEC
026700100803
026800100803       //--------------------------------------------------------------
026900100803       //?Definizione key-list.                                        ?
027000100803       //--------------------------------------------------------------
027100100806
027200100806       // -?File AZLIN05L?
027300130828     d*// k01azlin03    e ds                  extname(AZLIN03L : *key)
027400130828     d*//                                     prefix(k_)   inz
027500100803
027600100803       // -?File TNTBE01L?
027700100803     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
027800100803     d                                     prefix(k_)   inz
027900130828
028000130828       // -?File AZNTC01L?
028100130828     d k02azntc01    e ds                  extname(AZNTC01L : *key)
028200130828     d                                     prefix(k_)
028300100803
028400100803       //--------------------------------------------------------------
028500100803       //?M A I N - L I N E                                            ?
028600100803       //--------------------------------------------------------------
028700100803
028800100803     c     *Entry        plist
028900100803     c                   parm                    KPJBA
029000100806     c                   parm                    TRMK24ds
029100100803
029200100803      /free
029300100803
029400100803       // -?Operazioni iniziali?
029500100803       exsr  sr_RoutInz;
029600100803
029700100803       // -?Gestione window?
029800100803       DoW  Not $Fine;
029900100803         select;
030000100803           when  $Video = 'D1';
030100100803             exsr  sr_GesD01;
030200100803           other;
030300100803             $Fine = *on;
030400100803         endsl;
030500100803       EndDo;
030600100803
030700100803       // -?Operazioni finali?
030800100803       exsr  sr_RoutEnd;
030900100803
031000100803       //--------------------------------------------------------------
031100100803       //?Operazioni iniziali.                                         ?
031200100803       //--------------------------------------------------------------
031300100803       BEGSR  sr_RoutInz;
031400100803
031500100803         *inLR = *on;
031600100803
031700100803         // -?Impostazione parametri ricevuti?
031800100806         if  %parms > 1;
031900100915           //clear  Omk24mitt;
032000100908           clear  Omk24tlt;
032100100915           clear  Omk24lin;
032200100908           clear  Omk24mai;
032300100908           clear  Omk24obj;
032400100908           clear  Omk24rst;
032500100908           clear  Omk24cmm;
032600100915           clear  Omk24cmd;
032700100805           clear  Omk24fxx;
032800100805           clear  Omk24msg;
032900100806           Omk24err = *off;
033000100805         endif;
033100100803
033200100803         // -?Impostazione campi "fissi"?
033300100927         //V1Tpgm   = SDSpgm;
033400100803
033500100803         // -?Reperimento dati del lavoro?
033600100803         exsr  sr_DatiJob;
033700100809
033800100809         // -?Reperimento formato attuale display?
033900100809         clear  MK24P01;
034000100809         RetrieveDisplayMode ( QdspMode : *omit : *omit );
034100100809         xDs4 = (QdspMode = '4');
034200100809         open  TRMK24D;
034300100804
034400100804         // -?Posizionamento window?
034500100908         //   -?Prima prova con le posizioni ricevute (SE ricevute)?
034600100809         if  Imk24rig <> *zero   and   Imk24col <> *zero;
034700100804           W1rig = Imk24rig;
034800100804           W1col = Imk24col;
034900100809           write(e)  MK24P01;
035000100809         endif;
035100100908         //   -?In caso di errore (o SE NON ricevute posizioni):?
035200100908         //    ?imposta le posizioni di default (in basso a sx)?
035300100809         if  %error              or
035400100809             Imk24rig = *zero    or    Imk24col = *zero;
035500100809           if  Not xDs4;
035600100927             W1rig = 11;
035700100809           else;
035800100927             W1rig = 14;
035900100809           endif;
036000100809           W1col = 2;
036100100809         endif;
036200111129
036300111129         //?Data del giorno
036400111129         wOggi = %dec(%date());
036500100803
036600100803       ENDSR;
036700100803
036800100803       //--------------------------------------------------------------
036900100803       //?Reperimento Dati del job (Utente/Operativi).                 ?
037000100803       //--------------------------------------------------------------
037100100803       BEGSR  sr_DatiJob;
037200100803
037300100803         in(E) §AzUte;
037400100803         if NOT %error;
037500100803           in(E) §DatiUte;
037600100803         endif;
037700100803         if %error or RSut = *blanks;
037800100803           clear TIBS34ds;
037900100803           tibs34r(tibs34ds);
038000100803           in §AzUte;
038100100803           in §DatiUte;
038200100803         endif;
038300100803
038400100803       ENDSR;
038500100803
038600100803       //--------------------------------------------------------------
038700100803       //?Gestione videata D01.                                        ?
038800100803       //--------------------------------------------------------------
038900100803       BEGSR  sr_GesD01;
039000100803
039100100803         // -?Inizializzazione videata?
039200100803         if  $InzD01;
039300100803           exsr  sr_InzD01;
039400100803           $InzD01 = *off;
039500100803         endif;
039600100803
039700100809         // -?Emissione Piede (window) e Testata?
039800100809         write  MK24P01;
039900100805         write  MK24T01;
040000100804
040100100804         // -?Emissione videata?
040200100805         exfmt  MK24D01;
040300100804
040400100805         clear  D01msg;
040500100810         clear  errMessage;
040600100810         clear  errGenerico;
040700100803
040800100803         Select;
040900100803
041000100803           // -?F3=Fine?
041100100803           When  dsp_aid = c_F03;
041200100803             exsr  sr_F03D01;
041300100806
041400100806           // -?F12=Ritorno?
041500100806           When  dsp_aid = c_F12;
041600100806             exsr  sr_F12D01;
041700100803
041800100803           // -?Invio / F6=Conferma?
041900100803           Other;
042000100803             exsr  sr_CtrD01;
042100100803             select;
042200100803               when  ErrGenerico;
042300100803                 leavesr;
042400100803               when  dsp_aid = c_F06;
042500100803                 exsr  sr_F06D01;
042600100803             endsl;
042700100803
042800100803         EndSl;
042900100803
043000100803       ENDSR;
043100100803
043200100803       //--------------------------------------------------------------
043300100803       //?Inizializzazione videata D01.                                ?
043400100803       //--------------------------------------------------------------
043500100803       BEGSR  sr_InzD01;
043600100803
043700100805         clear  MK24D01;
043800100803
043900100805         // -?Impostazione campi a video con i parametri ricevuti?
044000100910         // ·?Testo?
044100100804         D01tlt  = Imk24tlt;
044200100910         // ·?Lingua?
044300100810         if  Not xDs4;
044400100810           D01klin = 'Lingua (?):';
044500100810         else;
044600100810           D01klin = '    Lingua:';
044700100810         endif;
044800100806         if  Imk24lin <> *blank;
044900100806           D01lin  = Imk24lin;
045000100806         else;
045100100806           D01lin  = 'it';
045200100806         endif;
045300100910         // ·?Destinatario: codice?
045400100908         if  Imk24cde <= 9999999;
045500100908           D01ktde = 'Cliente';
045600100908         else;
045700100908           D01ktde = 'Potenziale';
045800100908         endif;
045900100908         D01cde  = Imk24cde;
046000100910         // ·?Destinatario: ragione sociale?
046100100804         D01rst1 = %subst( Imk24rst : 1 : %len(D01rst1) );
046200100804         D01rst2 = %subst( Imk24rst : %len(D01rst1) + 1 );
046300100910         // ·?Destinatario: indirizzo e-mail?
046400100804         D01mai1 = %subst( Imk24mai : 1 : %len(D01mai1) );
046500100804         D01mai2 = %subst( Imk24mai : %len(D01mai1) + 1 );
046600100910         // ·?Oggetto?
046700100910         D01obj  = Imk24obj;
046800100910         // ·?Codice commerciale firma?
046900100806         D01cmm  = %editc( Imk24cmm : 'X' );
047000100804
047100100804         // -?Decodifiche?
047200100910         // ·?Testo?
047300100915         if  Imk24tlt <> *blank;
047400100915           exsr  sr_CtrlTLT;
047500100915         endif;
047600100910         // ·?Lingua?
047700100915         //if  Imk24lin <> *blank;
047800100915         //  exsr  sr_CtrlLIN;
047900100915         //endif;
048000100910         // ·?Codice commerciale firma?
048100100915         if  Imk24cmm <> *zero;
048200100915           exsr  sr_CtrlCMM;
048300100915         endif;
048400100915
048500100915         clear  D01msg;
048600100915         clear  errMessage;
048700100915         clear  errGenerico;
048800100806         %subst(IndDspF : 50) = *off;
048900100809
049000100809         // -?Reperimento dati tab. "MRA" (con "Oggetto")?
049100100915         if  Imk24tlt <> *blank;
049200100915           exsr  sr_TabMRA;
049300100915           if  D01obj = *blank;
049400100915             D01obj  = §MRAobjm;
049500100915           endif;
049600100915           clear  D01msg;
049700100915           clear  errMessage;
049800100915           clear  errGenerico;
049900100915           %subst(IndDspF : 50) = *off;
050000100915         endif;
050100100803
050200100803       ENDSR;
050300100803
050400100803       //--------------------------------------------------------------
050500100803       //?Gestione tasto funzionale F3 da videata D01:                 ?
050600100803       //?F3=Fine                                                      ?
050700100803       //--------------------------------------------------------------
050800100803       BEGSR  sr_F03D01;
050900100803
051000100804         // -?Restituzione tasto funzionale premuto?
051100100804         Omk24fxx = '03';
051200100804
051300100803         // -?Chiusura del *pgm?
051400100803         $Fine = *on;
051500100803
051600100803       ENDSR;
051700100806
051800100806       //--------------------------------------------------------------
051900100806       //?Gestione tasto funzionale F12 da videata D01:                ?
052000100909       //?F12=Ritorno                                                  ?
052100100806       //--------------------------------------------------------------
052200100806       BEGSR  sr_F12D01;
052300100806
052400100806         // -?Restituzione tasto funzionale premuto?
052500100806         Omk24fxx = '12';
052600100806
052700100806         // -?Chiusura del *pgm?
052800100806         $Fine = *on;
052900100806
053000100806       ENDSR;
053100100803
053200100803       //--------------------------------------------------------------
053300100803       //?Gestione tasto funzionale F6 da videata D01:                 ?
053400100803       //?F6=Conferma                                                  ?
053500100803       //--------------------------------------------------------------
053600100803       BEGSR  sr_F06D01;
053700100806
053800100806         // -?Decodifica filiale del commerciale?
053900100806         exsr  sr_DecodFIL;
054000100806         //if  ErrGenerico;
054100100806         //  leavesr;
054200100806         //endif;
054300100805
054400100805         // -?Reperimento dati tab. "1H"?
054500100805         clear  FNLV41ds;
054600100805         I41mod = '3';
054700100805         I41let = D01tlt;
054800100805         fnlv41r ( FNLV41ds );
054900100805         if  O41err <> *blank;
055000100805           errMessage  = *on;
055100100805           errGenerico = *on;
055200100805           PosCurTlt   = *on;
055300100806           D01msg = $Msg(16);
055400100805           leavesr;
055500100805         endif;
055600100805         ds1H = O41dta;
055700100806
055800100806         // -?Reperimento nuovo progressivo da numeratore "024"?
055900100806         exsr  sr_Num024;
056000100806         if  ErrGenerico;
056100100806           leavesr;
056200100806         endif;
056300100806
056400100804
056500100804         // -?Restituzione tasto funzionale premuto?
056600100804         Omk24fxx = '06';
056700100803
056800100804         // -?Stampa (ed invio) e-mail?
056900100804         exsr  sr_PrtEmail;
057000100803
057100100803         // -?Chiusura del *pgm?
057200100803         $Fine = *on;
057300100803
057400100803       ENDSR;
057500100803
057600100803       //--------------------------------------------------------------
057700100803       //?Controllo videata D01.                                       ?
057800100803       //--------------------------------------------------------------
057900100803       BEGSR  sr_CtrD01;
058000100803
058100100803         %subst(IndDspF : 50) = *off;
058200100803
058300100803         // -?Ricerche?
058400100803         exsr  sr_Search;
058500100803         if  ErrGenerico;
058600100803           leavesr;
058700100803         endif;
058800100803
058900100804         // -?Controllo testo lettera?
059000100804         exsr  sr_CtrlTLT;
059100100804         if  ErrGenerico;
059200100804           leavesr;
059300100804         endif;
059400100806
059500100806         // -?Controllo lingua?
059600100915         //exsr  sr_CtrlLIN;
059700100915         //if  ErrGenerico;
059800100915         //  leavesr;
059900100915         //endif;
060000100806
060100100806         // -?Reperimento dati tab. "MRA"?
060200100915         if  D01tlt <> SaveMRA;
060300100915           exsr  sr_TabMRA;
060400100915           if  ErrGenerico;
060500100915             leavesr;
060600100915           endif;
060700100915           if  D01obj = *blank   or   D01tlt <> SaveMRA;
060800100915             SaveMRA = D01tlt;
060900100915             D01obj  = §MRAobjm;
061000100915           endif;
061100100915         endif;
061200100902
061300100902         // -?Il testo DEVE essere e-mail?
061400100902         if  §1Html <> 'S';
061500100902           errMessage  = *on;
061600100902           errGenerico = *on;
061700100902           PosCurTlt   = *on;
061800100902           D01msg = $Msg(19);
061900100902           leavesr;
062000100902         endif;
062100100806
062200100902         // -?Se testo e-mail: ci deve essere il responsabile trasporti?
062300100806         if  §1Html = 'S'   and   (D01rst1 = *blank
062400100806                            and    D01rst2 = *blank);
062500100806           errMessage  = *on;
062600100806           errGenerico = *on;
062700100806           PosCurRst1  = *on;
062800100806           D01msg = $Msg(08);
062900100806           leavesr;
063000100806         endif;
063100100806
063200100902         // -?Se testo e-mail: ci deve essere l'indirizzo?
063300100806         if  §1Html = 'S'   and   (D01mai1 = *blank
063400100806                            and    D01mai2 = *blank);
063500100806           errMessage  = *on;
063600100806           errGenerico = *on;
063700100806           PosCurMai1  = *on;
063800100806           D01msg = $Msg(09);
063900100806           leavesr;
064000100806         endif;
064100100902
064200100902         // -?Se c'è l'indirizzo e-mail: deve essere corretto?
064300100902         if  D01mai1 <> *blank   or   D01mai2 <> *blank;
064400100902           clear  dsEMAIL;
064500100909           §EMLindI = D01mai1 + D01mai2;
064600100902           xemail ( dsEMAIL );
064700100909           if  §EMLerrO <> *off;
064800100902             errMessage  = *on;
064900100902             errGenerico = *on;
065000100902             PosCurMai1  = *on;
065100100909             D01msg = §EMLmsgO;
065200100902             leavesr;
065300100902           endif;
065400100909           D01mai1 = %subst( §EMLindO : 1 );
065500100909           D01mai2 = %subst( §EMLindO : %len(D01mai1) + 1 );
065600100902         endif;
065700100806
065800100902         // -?Se testo e-mail: ci deve essere l'oggetto?
065900100806         if  §1Html = 'S'   and   D01obj = *blank;
066000100806           errMessage  = *on;
066100100806           errGenerico = *on;
066200100806           PosCurObj   = *on;
066300100806           D01msg = $Msg(10);
066400100806           leavesr;
066500100806         endif;
066600100902
066700100803         // -?Controllo commerciale?
066800100804         exsr  sr_CtrlCMM;
066900100804         if  ErrGenerico;
067000100804           leavesr;
067100100804         endif;
067200100806
067300130828         // -?Reperimento dati comm.le (file AZNTC00F)?
067400130828         exsr  sr_AZNTC;
067500100806         if  ErrGenerico;
067600100806           leavesr;
067700100806         endif;
067800100803
067900100803       ENDSR;
068000100803
068100100803       //--------------------------------------------------------------
068200100803       //?Gestione ricerche in fmt D01.
068300100803       //--------------------------------------------------------------
068400100803       BEGSR  sr_Search;
068500100803
068600100803         Select;
068700100803
068800100803           // -?Tipo lettera?
068900100803           When  %scan( '?' : D01tlt ) > *zero;
069000100803             reset  FNLV41ds;
069100100804             fnlv41r ( FNLV41ds );
069200100803             D01tlt  = O41let;
069300100803             ds1H    = O41dta;
069400100803             D01tltd = §1Hdes;
069500100803             ErrGenerico = *on;
069600100803             PosCurTlt   = *on;
069700100803             leavesr;
069800100806
069900100806           // -?Lingua?
070000100809           //  ?N.B.?
070100100809           //  ?Richiede video fmt *DS3 (24x80) per una SUA window!?
070200100915           //When  %scan( '?' : D01lin ) > *zero;
070300100915           //  if  Not xDs4;
070400100915           //    clear  p37_iso2;
070500100915           //    clear  p37_desi;
070600100915           //    callp(E)  trul37r ( p37_iso2 : p37_desi );
070700100915           //    if  Not %error;
070800100915           //      D01lin  = p37_iso2;
070900100915           //      D01linD = p37_desi;
071000100915           //    endif;
071100100915           //  else;
071200100915           //    D01msg = 'Ricerca NON effettuabile con questo formato +
071300100915           //              video (richiede 24x80)...';
071400100915           //    errMessage  = *on;
071500100915           //  endif;
071600100915           //  ErrGenerico = *on;
071700100915           //  PosCurLin   = *on;
071800100915           //  leavesr;
071900100803
072000100803           // -?Commerciale?
072100100804           When  %scan( '?' : D01cmm ) > *zero;
072200130828             iMK43fil  = DUTpou;
072300130828             TRMK43R (kpjba : TRMK43ds);
072400130828             if  oMK43err = *off  and  oMK43fxx = *blank;
072500130828               D01cmm  = %editc( oMK43cmm : 'X' );
072600130828               D01cmmd = oMK43des;
072700130828             endif;
072800100803             ErrGenerico = *on;
072900100803             PosCurCmm   = *on;
073000100803             leavesr;
073100100803
073200100803           EndSl;
073300100803
073400100803       ENDSR;
073500100804
073600100804       //--------------------------------------------------------------
073700100804       //?Controllo testo lettera.
073800100804       //--------------------------------------------------------------
073900100804       BEGSR  sr_CtrlTLT;
074000100804
074100100804         clear  D01tltD;
074200100804
074300100804         // -?Controllo inserimento?
074400100804         if  D01tlt = *blank;
074500100804           errMessage  = *on;
074600100804           errGenerico = *on;
074700100804           PosCurTlt   = *on;
074800100804           D01msg = $Msg(01);
074900100804           leavesr;
075000100804         endif;
075100100804
075200100806         // -?Controllo testo lettera in tab. "1H"?
075300100804         clear  ds1H;
075400100804         clear  FNLV41ds;
075500100804         I41let = D01tlt;
075600100804         I41mod = '2';
075700100804         fnlv41r ( FNLV41ds );
075800100804         if  O41err =  'E';
075900100804           errMessage  = *on;
076000100804           errGenerico = *on;
076100100804           PosCurTlt   = *on;
076200100804           D01msg = $Msg(02);
076300100804           leavesr;
076400100804         endif;
076500100804
076600100804         ds1H = O41dta;
076700100804         D01tltD = §1Hdes;
076800100804
076900100804         // -?Verifica se testo gestibile in questo *pgm?
077000100804         if  §1Hfl5 <> 'S';
077100100804           errMessage  = *on;
077200100804           errGenerico = *on;
077300100804           PosCurTlt   = *on;
077400100804           D01msg = $Msg(03);
077500100804           leavesr;
077600100804         endif;
077700100804
077800100804       ENDSR;
077900100806
078000100806       //--------------------------------------------------------------
078100100806       //?Controllo lingua.
078200100806       //--------------------------------------------------------------
078300130828       //BEGSR  sr_CtrlLIN;
078400130828       //
078500130828       //  clear  D01linD;
078600130828       //
078700130828       //  // -?Controllo inserimento?
078800130828       //  if  D01lin = *blank;
078900130828       //    errMessage  = *on;
079000130828       //    errGenerico = *on;
079100130828       //    PosCurLin   = *on;
079200130828       //    D01msg = $Msg(04);
079300130828       //    leavesr;
079400130828       //  endif;
079500130828       //
079600130828       //  // -?Controllo validità?
079700130828       //  if  D01lin <> SaveLIN;
079800130828       //
079900130828       //    k_LINiso2 = D01lin;
080000130828       //    chain   %kds(k01azlin03)  AZLIN000;
080100130828       //    if  NOT  %found(AZLIN03L);
080200130828       //      errMessage  = *on;
080300130828       //      errGenerico = *on;
080400130828       //      PosCurLin   = *on;
080500130828       //      D01msg = $Msg(05);
080600130828       //      leavesr;
080700130828       //    endif;
080800130828       //
080900130828       //    SaveLIN = D01lin;
081000130828       //
081100130828       //  endif;
081200130828       //
081300130828       //  D01linD = LINdesIta;
081400130828       //
081500130828       //  // -?Se non è attiva per i testi: errore?
081600130828       //  //  ?(controllo NON più valido:?
081700130828       //  //  ? testo "chiodato" nella funzione di preparazione e-mail)?
081800130828       //  //if  LINfntxt = '0';
081900130828       //  //  errMessage  = *on;
082000130828       //  //  errGenerico = *on;
082100130828       //  //  PosCurLin   = *on;
082200130828       //  //  D01msg = $Msg(06);
082300130828       //  //  leavesr;
082400130828       //  //endif;
082500130828       //
082600130828       //  // -?Se si è scelto di stampare in lingua:?
082700130828       //  //  ?il testo deve essere gestito in lingua?
082800130828       //  if  §1Hlin = *blank   and   D01lin <> 'it';
082900130828       //    errMessage  = *on;
083000130828       //    errGenerico = *on;
083100130828       //    PosCurLin   = *on;
083200130828       //    D01msg = $Msg(07);
083300130828       //    leavesr;
083400130828       //  endif;
083500130828       //
083600130828       //ENDSR;
083700100804
083800100804       //--------------------------------------------------------------
083900100804       //?Controllo commerciale.
084000100804       //--------------------------------------------------------------
084100100804       BEGSR  sr_CtrlCMM;
084200100804
084300100804         clear  D01cmmD;
084400100804
084500100804         // -?Controllo inserimento?
084600100805         if  D01cmm = *blank   or   D01cmm = *zero;
084700100804           errMessage  = *on;
084800100804           errGenerico = *on;
084900100804           PosCurCmm   = *on;
085000100806           D01msg = $Msg(11);
085100100804           leavesr;
085200100804         endif;
085300100804
085400100804         // -?Controllo effettiva numericità?
085500100806         if  %check( c_Digits : D01cmm ) > *zero;
085600100810           errMessage  = *on;
085700100810           errGenerico = *on;
085800100804           PosCurCmm   = *on;
085900100806           D01msg = $Msg(12);
086000100804           leavesr;
086100100804         endif;
086200100805
086300100806         If  D01cmm <> SaveCMM;
086400100806
086500100806           // -?Controllo abilitazione utente al commerciale?
086600100806           reset  TNTAA1ds;
086700100909           iTAA1cmm  = %int( D01cmm );
086800100806           kpjbu     = TNTAA1ds;
086900100806           tntaa1c ( kpjba );
087000100806           TNTAA1ds = kpjbu;
087100100909           if  oTAA1fabi = 'N';
087200100810             errMessage  = *on;
087300100810             errGenerico = *on;
087400100806             PosCurCmm   = *on;
087500100806             D01msg = $Msg(13);
087600100806             leavesr;
087700100806           endif;
087800100806
087900130828           // -?Controllo esistenza in anagrafica?
088000130828           chain  (%int(D01cmm))  AZCMM000;
088100130828           if  NOT  %found(AZCMM01L);
088200100806             errMessage  = *on;
088300100806             errGenerico = *on;
088400100806             PosCurCmm   = *on;
088500100806             D01msg = $Msg(13);
088600100806             leavesr;
088700100806           endif;
088800110614
088900110614         // -?Controllo validità?
089000130828           IF  CMMdtIni > wOggi  or
089100130828               CMMdtFin < wOggi;
089200110614             errMessage  = *on;
089300110614             errGenerico = *on;
089400110614             PosCurCmm   = *on;
089500110614             D01msg = $Msg(13);
089600110614             leavesr;
089700110614           endif;
089800111129
089900111129         // -?Controllo data validità?
090000130828           if  CMMpar <> *blanks;
090100111129             errMessage  = *on;
090200111129             errGenerico = *on;
090300111129             PosCurCmm   = *on;
090400111129             D01msg = $Msg(13);
090500111129             leavesr;
090600111129           endif;
090700110614
090800100806           SaveCMM = D01cmm;
090900100806
091000100806         EndIf;
091100100806
091200130828         D01cmmD = CMMdes;
091300100804
091400100804       ENDSR;
091500100806
091600100806       //--------------------------------------------------------------
091700130828       //?Reperimento nome & e-mail del commerciale dal file AZNTC01L. ?
091800100806       //--------------------------------------------------------------
091900130828       BEGSR  sr_AZNTC;
092000130828
092100130828         clear  wNomeCmm;
092200130828         clear  wMailCmm;
092300100806
092400130828         k_NTCcmm = %int(D01cmm);
092500130828         k_NTCtnt = '01';
092600130828         chain  %kds( k02azntc01 )  AZNTC01L;
092700130828
092800130828         if  %found(AZNTC01L);
092900130828           wNomeCmm = NTCrnt;
093000130828           k_NTCtnt = '02';
093100130828           chain  %kds( k02azntc01 )  AZNTC01L;
093200130828           if  %found(AZNTC01L);
093300130828             wMailCmm = NTCrnt;
093400130828           endif;
093500130828         endif;
093600100806
093700130828         if  NOT %found(AZNTC01L);
093800100806           errMessage  = *on;
093900100806           errGenerico = *on;
094000100806           PosCurCmm   = *on;
094100100806           D01msg = %replace( D01cmm : $Msg(14) :
094200100806                              %scan( '&CommLe' : $Msg(14) ) );
094300100806           leavesr;
094400100806         endif;
094500100806
094600100806       ENDSR;
094700100806
094800100806       //--------------------------------------------------------------
094900100806       //?Reperimento dati da tab. "MRA" (regole per invio e-mail).    ?
095000100806       //--------------------------------------------------------------
095100100806       BEGSR  sr_TabMRA;
095200100806
095300100806         clear  dMRAmk24;
095400100806
095500100806         k_TBEcod = 'MRA';
095600100806         k_TBEke1 = %trim(SDSpgm) + '-' + D01tlt;
095700100806         k_TBEke2 = D01lin;
095800100806
095900100806         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
096000100806
096100100806         if  NOT %found(TNTBE01L)   or   TBEatb <> *blank;
096200100806           errMessage  = *on;
096300100806           errGenerico = *on;
096400100809           PosCurTlt   = *on;
096500100806           D01msg = %replace( k_TBEke1 : $Msg(15) :
096600100806                              %scan( '&TbeKe1' : $Msg(15) ) );
096700100806           D01msg = %replace( k_TBEke2 : D01msg :
096800100806                              %scan( '&TbeKe2' : D01msg ) );
096900100806           leavesr;
097000100806         endif;
097100100806
097200100806         dMRAmk24 = TBEuni;
097300100806
097400100806         //if  §MRAobjm = *blank;
097500120305         //  §MRAobjm = 'Comunicazione da BRT S.p.A.';
097600100806         //endif;
097700100806
097800100806       ENDSR;
097900100806
098000100806       //--------------------------------------------------------------
098100100806       //?Reperimento progressivo contatore "024"                      ?
098200100806       //--------------------------------------------------------------
098300100806       BEGSR  sr_Num024;
098400100806
098500100806         reset  TRUL33ds;
098600100806
098700100806         kpjbu  = TRUL33ds;
098800100806         trul33r ( kpjba );
098900100806         TRUL33ds = kpjbu;
099000100806
099100100806         if  O33err <> *zero;
099200100810           errMessage  = *on;
099300100810           errGenerico = *on;
099400100806           D01msg = $Msg(17);
099500100806           leavesr;
099600100806         endif;
099700100806
099800100806       ENDSR;
099900100806
100000100806       //--------------------------------------------------------------
100100100806       //?Decodifica filiale del commerciale.                          ?
100200100806       //--------------------------------------------------------------
100300100806       BEGSR  sr_DecodFIL;
100400100806
100500100806         ORGfil = %int( D01cmm ) / 10000;
100600100806         chain  (ORGfil)  AZORG;
100700100806         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
100800100806           clear  ORGdes;
100900100806           clear  ORGind;
101000100806           clear  ORGcpf;
101100100806           clear  ORGloc;
101200100806           clear  ORGpro;
101300100806           clear  ORGtel;
101400100806           clear  ORGfax;
101500100806         endif;
101600100806
101700100806       ENDSR;
101800100803
101900100803       //--------------------------------------------------------------
102000100803       //?Preparazione ed invio e-mail.                                ?
102100100803       //--------------------------------------------------------------
102200100803       BEGSR  sr_PrtEmail;
102300100805
102400100805         // -?Override ai file di stampa?
102500100805         //  ?(per impostarvi i dati per l'invio via e-mail)?
102600100805         exsr sr_Override;
102700100806         if  ErrGenerico;
102800100806           leavesr;
102900100806         endif;
103000100805
103100100805         // -?Apertura file di stampa?
103200100805         open  PrtEMAIL;
103300100803
103400100803         // -?Stampa "corpo e-mail"?
103500100810         For  xx = 1  To  10;
103600100901           //O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
103700100803           select;
103800100803             when  xx = 1;
103900100901               O_txt = '§§Nominativo§§#' + %triml( D01rst1 )
104000100901                                         + %trimr( D01rst2 );
104100100809             when  xx = 2;
104200130828               O_txt = '§§Firma001§§#' + %triml( wNomeCmm );
104300100809             when  xx = 3;
104400100901               O_txt = '§§Firma002§§#' + %triml( ORGdes );
104500100809             when  xx = 4;
104600100901               O_txt = '§§Firma003§§#' + %triml( ORGind );
104700100809             when  xx = 5;
104800100901               O_txt = '§§Firma004§§#' + %editc( ORGcpf : 'X' );
104900100809             when  xx = 6;
105000100901               O_txt = '§§Firma005§§#' + %triml( ORGloc );
105100100809             when  xx = 7;
105200100901               O_txt = '§§Firma006§§#' + ORGpro;
105300100809             when  xx = 8;
105400100901               O_txt = '§§Firma007§§#' + %triml( ORGtel );
105500100809             when  xx = 9;
105600100901               O_txt = '§§Firma008§§#' + %triml( ORGfax );
105700100809             when  xx = 10;
105800130828               O_txt = '§§nome@brt.it§§#' + %trim( wMailCmm )
105900100901                                                + c_at;
106000100803           endsl;
106100100803           O_txt = %trimr(O_txt) + '}{';
106200100803           except  PRTparm;
106300100803         EndFor;
106400100805
106500100805         // -?Chiusura file di stampa?
106600100805         close  PrtEMAIL;
106700100805
106800100805         // -?Cancellazione override ai file di stampa?
106900100805         exsr sr_DeleteOvr;
107000100803
107100100803       ENDSR;
107200100803
107300100803       //--------------------------------------------------------------
107400100803       //?Override al file di stampa per impostarvi i dati per         ?
107500100803       //?  l'invio via e-mail.                                        ?
107600100803       //--------------------------------------------------------------
107700100803       BEGSR sr_Override;
107800100803
107900100803         reset TRTCM1ds;
108000100803         if  §MRAmitt <> *blank;
108100100803           §CM1mitt = %trim(§MRAmitt);
108200100803         else;
108300130828           §CM1mitt = %trim(wMailCmm);
108400100803         endif;
108500100901         §CM1dst  = D01mai1 + D01mai2;
108600100803         §CM1tips = §MRAregCor;
108700100803         §CM1po   = %editc( DUTpou : 'X' );
108800100908         §CM1var  = %trimr(§CM1var) + %trim(D01obj) + ' -' + Imk24tde
108900100908                                                    + '.';
109000100908         if  Imk24tde = 'C';
109100100908           §CM1var = %trimr(§CM1var) +
109200100908                     %subst( %editc( Imk24cde : 'X' ) : 5 );
109300100908         else;
109400100908           §CM1var = %trimr(§CM1var) +
109500100908                     %editc( Imk24cde : 'X' );
109600100908         endif;
109700100803         §CM1sts  = §MRAsts;
109800100803         §CM1idp  = §MRAidpro;
109900100803         §CM1prog = %subst( %editc( O33nri : 'X' ) :
110000100803                            %size(O33nri) - %size(§CM1prog) + 1 );
110100100803
110200100803         // -?Override al "corpo e-mail"?
110300100803         if  §MRAregCor <> *blank;
110400100803           Qcmd = c_CmdOvrPrtF
110500100907                + ' usrdta(''' + %trim(SDSpgm) + '-' + D01tlt + ''')'
110600100803                + ' outq(' + %trim(§MRAoutqi) + ')'
110700100803                + ' usrdfndta(''' + TRTCM1ds + ''')';
110800100803         else;
110900100803           Qcmd = c_CmdOvrPrtF;
111000100803         endif;
111100100803
111200100803         exsr  sr_ExecCmd;
111300100803
111400100803         if  Qusei <> *blank;
111500100806           //if  %parms > 1;
111600100806           //  Omk24err = *on;
111700100806           //  Omk24msg = 'Fallita override al file di stampa per +
111800100806           //              "corpo e-mail"';
111900100806           //endif;
112000100806           //exsr  sr_RoutEnd;
112100100810           errMessage  = *on;
112200100810           errGenerico = *on;
112300100806           D01msg = $Msg(18);
112400100806           leavesr;
112500100803         endif;
112600100803
112700100803       ENDSR;
112800100803
112900100803       //--------------------------------------------------------------
113000100803       //?Chiusura file stampa & Cancellazione override.               ?
113100100803       //--------------------------------------------------------------
113200100803       BEGSR sr_DeleteOvr;
113300100803
113400100803         // -?Cancellazione override al "corpo e-mail"?
113500100803         if %open(PrtEMAIL);
113600100803           close PRTEMAIL;
113700100803           Qcmd = c_CmdDltOvr;
113800100803           exsr  sr_ExecCmd;
113900100803         endif;
114000100803
114100100803       ENDSR;
114200100803
114300100803       //--------------------------------------------------------------
114400100803       //?Esecuzione del comando (già impostato).                      ?
114500100803       //--------------------------------------------------------------
114600100803       BEGSR  sr_ExecCmd;
114700100803
114800100803         clear Qcap0100;
114900100803         Qcabcsdh = *off;
115000100803         Qcapa    = *off;
115100100803         Qcacmdss = *off;
115200100803         Qcaerved = *allX'00';
115300100803
115400100803         clear Qusec;
115500100803         Qusbprv  = %size(Qusec);
115600100803
115700100803         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
115800100803                           %size(Qcap0100) : 'CPOP0100' : *omit :
115900100803                           0 : 0 : Qusec);
116000100803
116100100803         //if  Qusei <> *blank;
116200100803         //  ...;
116300100803         //endif;
116400100803
116500100803       ENDSR;
116600100803
116700100803       //--------------------------------------------------------------
116800100803       //?Operazioni finali.                                           ?
116900100803       //--------------------------------------------------------------
117000100803       BEGSR  sr_RoutEnd;
117100100803
117200100805         // -?Restituzione parametri?
117300100806         if  %parms > 1;
117400100908           Imk24lin   = D01lin;
117500100915           //Omk24mitt  = §CM1mitt;
117600100908           Omk24tlt   = D01tlt;
117700100915           Omk24lin   = D01lin;
117800100908           Omk24mai   = D01mai1 + D01mai2;
117900100908           Omk24obj   = D01obj;
118000100908           Omk24rst   = D01rst1 + D01rst2;
118100100908           Omk24cmm   = %int( D01cmm );
118200100915           Omk24cmd   = D01cmmd;
118300100803         endif;
118400100803
118500100803         // -?Uscita?
118600100803         return;
118700100803
118800100803       ENDSR;
118900100803
119000100803      /end-free
119100100803
119200100803       //--------------------------------------------------------------
119300100803       //?Specifiche di stampa.                                        ?
119400100803       //--------------------------------------------------------------
119500100803
119600100803     oPrtEMAIL  e            PRTparm     1
119700100803     o                       O_txt
119800100804
119900100804       //--------------------------------------------------------------
120000100804       //?Messaggi di errore.                                          ?
120100100804       //--------------------------------------------------------------
120200100804
120300100804** -?$Msg?-------------------------------------------------------------------*
120400100804Testo lettera obbligatorio                                                      1
120500100804Testo lettera errato                                                            2
120600100805Testo lettera non utilizzabile in questa gestione                               3
120700100806Lingua obbligatoria                                                             4
120800100806Lingua errata                                                                   5
120900100806Lingua NON attiva per i testi ----------------  L I B E R O   ----------------  6
121000100806Testo NON gestito in lingua                                                     7
121100100806Per testo lettera HTML è obbligatorio inserire il responsabile trasporti        8
121200100806Per testo lettera HTML è obbligatorio inserire l'indirizzo e-mail (nota 06)     9
121300100806Per testo lettera HTML è obbligatorio inserire l'oggetto della e-mail          10
121400100806Codice commerciale obbligatorio                                                11
121500100806Codice commerciale NON numerico                                                12
121600100806Codice commerciale errato o NON in gestione all'utente                         13
121700130829Dati del commerciale "&CommLe" non reperiti in "Note/Rubrica"                  14
121800100806Parametri "&TbeKe1        "/"&TbeKe2        " non reperiti in tab. "MRA"       15
121900100806Fallito reperimento testo lettera                                              16
122000100806Fallito reperimento numeratore "&Nr"                                           17
122100100806Fallita override al file di stampa per "corpo e-mail" (vedi job-log)           18
122200100902Il testo lettera DEVE essere HTML                                              19
