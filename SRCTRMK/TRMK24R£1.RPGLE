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
002900100806     fAZLIN03L  if   e           k disk
003000100803
003100100803       // -?Tabelle?
003200100803     fTABEL00F  if   e           k disk
003300100803     fTNTBE01L  if   e           k disk
003400100803
003500100803       // -?Video parametri di lancio?
003600100809     fTRMK24D   cf   e             workstn usropn
003700100803     f                                     indds(IndDspF)
003800100803     f                                     infds(InfDspF)
003900100803
004000100803       // -?Spool da inviare via e-mail:?
004100100803       //   ·?Corpo del messaggio di posta elettronica?
004200100810     fPrtEMAIL  o    f  198        printer  oflind(*inOF)  usropn
004300100803
004400100803       //--------------------------------------------------------------
004500100803       //?Definizione costanti.                                        ?
004600100803       //--------------------------------------------------------------
004700100803
004800100803       // -?Codice Utente / Lingua italiana  in tabelle su TABEL00F?
004900100803     d c_Kut           c                   const(1)
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
015000100805
015100100805       // -?Tab. "01" = Agenti commerciali?
015200100805     d ds01          e ds                  inz
015300100806
015400100806       // -?Tab. "1H" = Testi tariffe?
015500100806     d ds1H          e ds                  inz
015600100803
015700100803       // -?Tab. "HTM" = Dati per testo html?
015800100803     d dHTM          e ds                  inz
015900100803
016000100805       // -?Tab. "MRA" = Bart-Mailing x TRMK24R?
016100100805     d dMRAmk24      e ds                  inz
016200100803
016300100803       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
016400100803     d TRTCM1ds      e ds                  inz
016500100803       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
016600100803     d   §CM1mitt    e                     inz('ced')
016700100803       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
016800120305     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
016900100803       //  ?·§CM1tips = Tipo lettera via e-mail:?
017000100803       //   ?"LET" = testo allegato in corpo con logo?
017100100803       //           ?(richiede righe libere iniziali per il logo)?
017200100803       //   ?"COR" = testo integrato senza logo?
017300100803       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
017400100803       //   ?"SP1" = spool allegato (in formato PDF)?
017500100806     d   §CM1tips    e                     inz('CMx')
017600100803       //  ?·§CM1po   = Filiale?
017700100803     d   §CM1po      e                     inz('046')
017800100803       //  ?·§CM1var  = Oggetto e-mail?
017900100803     d*//§CM1var     e                     inz('*OBJM*+
018000120305     d*//                                  Comunicazione da BRT S.p.A.')
018100100803     d   §CM1var     e                     inz('*OBJM*')
018200100803       //  ?·§CM1sts  = Stato?
018300100803     d   §CM1sts     e                     inz('H')
018400100803       //  ?·§CM1idp  = Id processo?
018500100803     d   §CM1idp     e                     inz('3')
018600100803       //  ?·§CM1att  = Numero attach corrente?
018700100803     d   §CM1att     e                     inz('01')
018800100803       //  ?·§CM1totatt  = Numero attach totali?
018900100803     d   §CM1totatt  e                     inz('01')
019000100803
019100100803       //--------------------------------------------------------------
019200100803       //?Definizione variabili.                                       ?
019300100803       //--------------------------------------------------------------
019400100803
019500100803       // -?Flags booleani?
019600100803     d $Fine           s               n   inz
019700100803     d $InzD01         s               n   inz(*on)
019800100810     d ErrGenerico     s               n   inz
019900100803
020000100803       // -?Indici di schiera?
020100100803     d xx              s              3  0 inz
020200100803
020300100803       // -?Variabili per la gestione del video?
020400100803     d $Video          s              2    inz('D1')
020500100803
020600100803       // -?Campi di stampa?
020700100810     d O_txt           s            198    inz
020800100806
020900100806       // -?Campi di comodo per la verifica di cambio codice?
021000100806     d SaveCMM         s                   like(D01cmm) inz
021100100806     d SaveLIN         s                   like(D01lin) inz
021200100915     d SaveMRA         s                   like(D01tlt) inz
021300111129
021400111129       // -?Campi di comodo
021500111129     d wOggi           s              8s 0
021600100803
021700100803       //--------------------------------------------------------------
021800100908       //?Definizione prototipi & relativi parametri                   ?
021900100803       //--------------------------------------------------------------
022000100803
022100100803       // -?Reperimento dati utente?
022200100803      /copy gaitrasrc/srcProtoPR,TIBS34R
022300100803
022400100803       // -?Gestione numeratori in GRU?
022500100803     d TRUL33ds      e ds                  inz
022600100803     d   I33tla      e                     inz('L')
022700100803     d   I33ope      e                     inz(*zero)
022800100803     d   I33cnu      e                     inz(024)
022900100803     d   I33num      e                     inz(1)
023000100803      /copy gaitrasrc/srcProtoPR,TRUL33R
023100100803
023200100909       // -?Ricerca testi lettera?
023300100803     d FNLV41ds      e ds                  inz
023400100803     d   I41mod      e                     inz('1')
023500100803     d   I41prv      e                     inz('M')
023600100803      /copy gaitrasrc/srcProtoPR,FNLV41R
023700100803
023800100909       // -?Ricerca commerciali?
023900100805     d TB85Kfil        s              3    inz
024000100805     d TB85Key         s              8    inz
024100100805     d TB85Des1        s             25    inz
024200100805     d TB85Uni         s              1    inz('S')
024300100803      /copy gaitrasrc/srcProtoPR,TRTB85R
024400100805
024500100805       // -?Controllo abilitazioni utente?
024600100909     d TNTAA1ds      e ds                  inz
024700100909     d   iTAA1tipo   e                     inz('N')
024800100909     d   iTAA1abi    e                     inz('RA')
024900100805      /copy gaitrasrc/srcProtoPR,TNTAA1C
025000100806
025100100902       // -?Ricerca lingua su AZLIN?
025200100806     d P37_iso2        s                   like(LINiso2)   inz
025300100806     d P37_desi        s                   like(LINdesita) inz
025400100806     d trul37r         pr                  extpgm('TRUL37R')
025500100806     d   p37iso2                           like(P37_iso2)
025600100806     d   p37desi                           like(P37_desi)
025700100902
025800100902       // -?Controllo indirizzo e-mail?
025900100909     d dsEMAIL       e ds                  inz
026000100902      /copy gaitrasrc/srcProtoPR,XEMAIL
026100100809
026200100809       // -?API QSNDRTVMOD (Retrieve Display Mode)?
026300100809     d QdspMode        s              1    inz
026400100809      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
026500100803
026600100805       // -?API QCAPCMD (Process Commands)?
026700100803     d Qcmd            s           2048    inz  varying
026800100803      /copy qSysInc/qRpgleSrc,QCAPCMD
026900100803      /copy gaitrasrc/srcProtoPR,QCAPCMD
027000100803
027100100803       // -?Parametri gestione errori API.?
027200100803      /copy qsysinc/qrpglesrc,QUSEC
027300100803
027400100803       //--------------------------------------------------------------
027500100803       //?Definizione key-list.                                        ?
027600100803       //--------------------------------------------------------------
027700100806
027800100806       // -?File AZLIN05L?
027900100806     d k01azlin03    e ds                  extname(AZLIN03L : *key)
028000100806     d                                     prefix(k_)   inz
028100100803
028200100803       // -?File TABEL00F?
028300100803     d k03tabel00    e ds                  extname(TABEL00F : *key)
028400100803     d                                     prefix(k_)   inz
028500100803       // -?File TNTBE01L?
028600100803     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
028700100803     d                                     prefix(k_)   inz
028800100803
028900100803       //--------------------------------------------------------------
029000100803       //?M A I N - L I N E                                            ?
029100100803       //--------------------------------------------------------------
029200100803
029300100803     c     *Entry        plist
029400100803     c                   parm                    KPJBA
029500100806     c                   parm                    TRMK24ds
029600100803
029700100803      /free
029800100803
029900100803       // -?Operazioni iniziali?
030000100803       exsr  sr_RoutInz;
030100100803
030200100803       // -?Gestione window?
030300100803       DoW  Not $Fine;
030400100803         select;
030500100803           when  $Video = 'D1';
030600100803             exsr  sr_GesD01;
030700100803           other;
030800100803             $Fine = *on;
030900100803         endsl;
031000100803       EndDo;
031100100803
031200100803       // -?Operazioni finali?
031300100803       exsr  sr_RoutEnd;
031400100803
031500100803       //--------------------------------------------------------------
031600100803       //?Operazioni iniziali.                                         ?
031700100803       //--------------------------------------------------------------
031800100803       BEGSR  sr_RoutInz;
031900100803
032000100803         *inLR = *on;
032100100803
032200100803         // -?Impostazione parametri ricevuti?
032300100806         if  %parms > 1;
032400100915           //clear  Omk24mitt;
032500100908           clear  Omk24tlt;
032600100915           clear  Omk24lin;
032700100908           clear  Omk24mai;
032800100908           clear  Omk24obj;
032900100908           clear  Omk24rst;
033000100908           clear  Omk24cmm;
033100100915           clear  Omk24cmd;
033200100805           clear  Omk24fxx;
033300100805           clear  Omk24msg;
033400100806           Omk24err = *off;
033500100805         endif;
033600100803
033700100803         // -?Impostazione campi "fissi"?
033800100927         //V1Tpgm   = SDSpgm;
033900100803         k_TBLkut = 1;
034000100803         k_TBLcod = '01';
034100100803
034200100803         // -?Reperimento dati del lavoro?
034300100803         exsr  sr_DatiJob;
034400100809
034500100809         // -?Reperimento formato attuale display?
034600100809         clear  MK24P01;
034700100809         RetrieveDisplayMode ( QdspMode : *omit : *omit );
034800100809         xDs4 = (QdspMode = '4');
034900100809         open  TRMK24D;
035000100804
035100100804         // -?Posizionamento window?
035200100908         //   -?Prima prova con le posizioni ricevute (SE ricevute)?
035300100809         if  Imk24rig <> *zero   and   Imk24col <> *zero;
035400100804           W1rig = Imk24rig;
035500100804           W1col = Imk24col;
035600100809           write(e)  MK24P01;
035700100809         endif;
035800100908         //   -?In caso di errore (o SE NON ricevute posizioni):?
035900100908         //    ?imposta le posizioni di default (in basso a sx)?
036000100809         if  %error              or
036100100809             Imk24rig = *zero    or    Imk24col = *zero;
036200100809           if  Not xDs4;
036300100927             W1rig = 11;
036400100809           else;
036500100927             W1rig = 14;
036600100809           endif;
036700100809           W1col = 2;
036800100809         endif;
036900111129
037000111129         //?Data del giorno
037100111129         wOggi = %dec(%date());
037200100803
037300100803       ENDSR;
037400100803
037500100803       //--------------------------------------------------------------
037600100803       //?Reperimento Dati del job (Utente/Operativi).                 ?
037700100803       //--------------------------------------------------------------
037800100803       BEGSR  sr_DatiJob;
037900100803
038000100803         in(E) §AzUte;
038100100803         if NOT %error;
038200100803           in(E) §DatiUte;
038300100803         endif;
038400100803         if %error or RSut = *blanks;
038500100803           clear TIBS34ds;
038600100803           tibs34r(tibs34ds);
038700100803           in §AzUte;
038800100803           in §DatiUte;
038900100803         endif;
039000100803
039100100803       ENDSR;
039200100803
039300100803       //--------------------------------------------------------------
039400100803       //?Gestione videata D01.                                        ?
039500100803       //--------------------------------------------------------------
039600100803       BEGSR  sr_GesD01;
039700100803
039800100803         // -?Inizializzazione videata?
039900100803         if  $InzD01;
040000100803           exsr  sr_InzD01;
040100100803           $InzD01 = *off;
040200100803         endif;
040300100803
040400100809         // -?Emissione Piede (window) e Testata?
040500100809         write  MK24P01;
040600100805         write  MK24T01;
040700100804
040800100804         // -?Emissione videata?
040900100805         exfmt  MK24D01;
041000100804
041100100805         clear  D01msg;
041200100810         clear  errMessage;
041300100810         clear  errGenerico;
041400100803
041500100803         Select;
041600100803
041700100803           // -?F3=Fine?
041800100803           When  dsp_aid = c_F03;
041900100803             exsr  sr_F03D01;
042000100806
042100100806           // -?F12=Ritorno?
042200100806           When  dsp_aid = c_F12;
042300100806             exsr  sr_F12D01;
042400100803
042500100803           // -?Invio / F6=Conferma?
042600100803           Other;
042700100803             exsr  sr_CtrD01;
042800100803             select;
042900100803               when  ErrGenerico;
043000100803                 leavesr;
043100100803               when  dsp_aid = c_F06;
043200100803                 exsr  sr_F06D01;
043300100803             endsl;
043400100803
043500100803         EndSl;
043600100803
043700100803       ENDSR;
043800100803
043900100803       //--------------------------------------------------------------
044000100803       //?Inizializzazione videata D01.                                ?
044100100803       //--------------------------------------------------------------
044200100803       BEGSR  sr_InzD01;
044300100803
044400100805         clear  MK24D01;
044500100803
044600100805         // -?Impostazione campi a video con i parametri ricevuti?
044700100910         // ·?Testo?
044800100804         D01tlt  = Imk24tlt;
044900100910         // ·?Lingua?
045000100810         if  Not xDs4;
045100100810           D01klin = 'Lingua (?):';
045200100810         else;
045300100810           D01klin = '    Lingua:';
045400100810         endif;
045500100806         if  Imk24lin <> *blank;
045600100806           D01lin  = Imk24lin;
045700100806         else;
045800100806           D01lin  = 'it';
045900100806         endif;
046000100910         // ·?Destinatario: codice?
046100100908         if  Imk24cde <= 9999999;
046200100908           D01ktde = 'Cliente';
046300100908         else;
046400100908           D01ktde = 'Potenziale';
046500100908         endif;
046600100908         D01cde  = Imk24cde;
046700100910         // ·?Destinatario: ragione sociale?
046800100804         D01rst1 = %subst( Imk24rst : 1 : %len(D01rst1) );
046900100804         D01rst2 = %subst( Imk24rst : %len(D01rst1) + 1 );
047000100910         // ·?Destinatario: indirizzo e-mail?
047100100804         D01mai1 = %subst( Imk24mai : 1 : %len(D01mai1) );
047200100804         D01mai2 = %subst( Imk24mai : %len(D01mai1) + 1 );
047300100910         // ·?Oggetto?
047400100910         D01obj  = Imk24obj;
047500100910         // ·?Codice commerciale firma?
047600100806         D01cmm  = %editc( Imk24cmm : 'X' );
047700100804
047800100804         // -?Decodifiche?
047900100910         // ·?Testo?
048000100915         if  Imk24tlt <> *blank;
048100100915           exsr  sr_CtrlTLT;
048200100915         endif;
048300100910         // ·?Lingua?
048400100915         //if  Imk24lin <> *blank;
048500100915         //  exsr  sr_CtrlLIN;
048600100915         //endif;
048700100910         // ·?Codice commerciale firma?
048800100915         if  Imk24cmm <> *zero;
048900100915           exsr  sr_CtrlCMM;
049000100915         endif;
049100100915
049200100915         clear  D01msg;
049300100915         clear  errMessage;
049400100915         clear  errGenerico;
049500100806         %subst(IndDspF : 50) = *off;
049600100809
049700100809         // -?Reperimento dati tab. "MRA" (con "Oggetto")?
049800100915         if  Imk24tlt <> *blank;
049900100915           exsr  sr_TabMRA;
050000100915           if  D01obj = *blank;
050100100915             D01obj  = §MRAobjm;
050200100915           endif;
050300100915           clear  D01msg;
050400100915           clear  errMessage;
050500100915           clear  errGenerico;
050600100915           %subst(IndDspF : 50) = *off;
050700100915         endif;
050800100803
050900100803       ENDSR;
051000100803
051100100803       //--------------------------------------------------------------
051200100803       //?Gestione tasto funzionale F3 da videata D01:                 ?
051300100803       //?F3=Fine                                                      ?
051400100803       //--------------------------------------------------------------
051500100803       BEGSR  sr_F03D01;
051600100803
051700100804         // -?Restituzione tasto funzionale premuto?
051800100804         Omk24fxx = '03';
051900100804
052000100803         // -?Chiusura del *pgm?
052100100803         $Fine = *on;
052200100803
052300100803       ENDSR;
052400100806
052500100806       //--------------------------------------------------------------
052600100806       //?Gestione tasto funzionale F12 da videata D01:                ?
052700100909       //?F12=Ritorno                                                  ?
052800100806       //--------------------------------------------------------------
052900100806       BEGSR  sr_F12D01;
053000100806
053100100806         // -?Restituzione tasto funzionale premuto?
053200100806         Omk24fxx = '12';
053300100806
053400100806         // -?Chiusura del *pgm?
053500100806         $Fine = *on;
053600100806
053700100806       ENDSR;
053800100803
053900100803       //--------------------------------------------------------------
054000100803       //?Gestione tasto funzionale F6 da videata D01:                 ?
054100100803       //?F6=Conferma                                                  ?
054200100803       //--------------------------------------------------------------
054300100803       BEGSR  sr_F06D01;
054400100806
054500100806         // -?Decodifica filiale del commerciale?
054600100806         exsr  sr_DecodFIL;
054700100806         //if  ErrGenerico;
054800100806         //  leavesr;
054900100806         //endif;
055000100805
055100100805         // -?Reperimento dati tab. "1H"?
055200100805         clear  FNLV41ds;
055300100805         I41mod = '3';
055400100805         I41let = D01tlt;
055500100805         fnlv41r ( FNLV41ds );
055600100805         if  O41err <> *blank;
055700100805           errMessage  = *on;
055800100805           errGenerico = *on;
055900100805           PosCurTlt   = *on;
056000100806           D01msg = $Msg(16);
056100100805           leavesr;
056200100805         endif;
056300100805         ds1H = O41dta;
056400100806
056500100806         // -?Reperimento nuovo progressivo da numeratore "024"?
056600100806         exsr  sr_Num024;
056700100806         if  ErrGenerico;
056800100806           leavesr;
056900100806         endif;
057000100806
057100100804
057200100804         // -?Restituzione tasto funzionale premuto?
057300100804         Omk24fxx = '06';
057400100803
057500100804         // -?Stampa (ed invio) e-mail?
057600100804         exsr  sr_PrtEmail;
057700100803
057800100803         // -?Chiusura del *pgm?
057900100803         $Fine = *on;
058000100803
058100100803       ENDSR;
058200100803
058300100803       //--------------------------------------------------------------
058400100803       //?Controllo videata D01.                                       ?
058500100803       //--------------------------------------------------------------
058600100803       BEGSR  sr_CtrD01;
058700100803
058800100803         %subst(IndDspF : 50) = *off;
058900100803
059000100803         // -?Ricerche?
059100100803         exsr  sr_Search;
059200100803         if  ErrGenerico;
059300100803           leavesr;
059400100803         endif;
059500100803
059600100804         // -?Controllo testo lettera?
059700100804         exsr  sr_CtrlTLT;
059800100804         if  ErrGenerico;
059900100804           leavesr;
060000100804         endif;
060100100806
060200100806         // -?Controllo lingua?
060300100915         //exsr  sr_CtrlLIN;
060400100915         //if  ErrGenerico;
060500100915         //  leavesr;
060600100915         //endif;
060700100806
060800100806         // -?Reperimento dati tab. "MRA"?
060900100915         if  D01tlt <> SaveMRA;
061000100915           exsr  sr_TabMRA;
061100100915           if  ErrGenerico;
061200100915             leavesr;
061300100915           endif;
061400100915           if  D01obj = *blank   or   D01tlt <> SaveMRA;
061500100915             SaveMRA = D01tlt;
061600100915             D01obj  = §MRAobjm;
061700100915           endif;
061800100915         endif;
061900100902
062000100902         // -?Il testo DEVE essere e-mail?
062100100902         if  §1Html <> 'S';
062200100902           errMessage  = *on;
062300100902           errGenerico = *on;
062400100902           PosCurTlt   = *on;
062500100902           D01msg = $Msg(19);
062600100902           leavesr;
062700100902         endif;
062800100806
062900100902         // -?Se testo e-mail: ci deve essere il responsabile trasporti?
063000100806         if  §1Html = 'S'   and   (D01rst1 = *blank
063100100806                            and    D01rst2 = *blank);
063200100806           errMessage  = *on;
063300100806           errGenerico = *on;
063400100806           PosCurRst1  = *on;
063500100806           D01msg = $Msg(08);
063600100806           leavesr;
063700100806         endif;
063800100806
063900100902         // -?Se testo e-mail: ci deve essere l'indirizzo?
064000100806         if  §1Html = 'S'   and   (D01mai1 = *blank
064100100806                            and    D01mai2 = *blank);
064200100806           errMessage  = *on;
064300100806           errGenerico = *on;
064400100806           PosCurMai1  = *on;
064500100806           D01msg = $Msg(09);
064600100806           leavesr;
064700100806         endif;
064800100902
064900100902         // -?Se c'è l'indirizzo e-mail: deve essere corretto?
065000100902         if  D01mai1 <> *blank   or   D01mai2 <> *blank;
065100100902           clear  dsEMAIL;
065200100909           §EMLindI = D01mai1 + D01mai2;
065300100902           xemail ( dsEMAIL );
065400100909           if  §EMLerrO <> *off;
065500100902             errMessage  = *on;
065600100902             errGenerico = *on;
065700100902             PosCurMai1  = *on;
065800100909             D01msg = §EMLmsgO;
065900100902             leavesr;
066000100902           endif;
066100100909           D01mai1 = %subst( §EMLindO : 1 );
066200100909           D01mai2 = %subst( §EMLindO : %len(D01mai1) + 1 );
066300100902         endif;
066400100806
066500100902         // -?Se testo e-mail: ci deve essere l'oggetto?
066600100806         if  §1Html = 'S'   and   D01obj = *blank;
066700100806           errMessage  = *on;
066800100806           errGenerico = *on;
066900100806           PosCurObj   = *on;
067000100806           D01msg = $Msg(10);
067100100806           leavesr;
067200100806         endif;
067300100902
067400100803         // -?Controllo commerciale?
067500100804         exsr  sr_CtrlCMM;
067600100804         if  ErrGenerico;
067700100804           leavesr;
067800100804         endif;
067900100806
068000100806         // -?Reperimento dati comm.le (tab. "HTM")?
068100100806         exsr  sr_TabHTM;
068200100806         if  ErrGenerico;
068300100806           leavesr;
068400100806         endif;
068500100803
068600100803       ENDSR;
068700100803
068800100803       //--------------------------------------------------------------
068900100803       //?Gestione ricerche in fmt D01.
069000100803       //--------------------------------------------------------------
069100100803       BEGSR  sr_Search;
069200100803
069300100803         Select;
069400100803
069500100803           // -?Tipo lettera?
069600100803           When  %scan( '?' : D01tlt ) > *zero;
069700100803             reset  FNLV41ds;
069800100804             fnlv41r ( FNLV41ds );
069900100803             D01tlt  = O41let;
070000100803             ds1H    = O41dta;
070100100803             D01tltd = §1Hdes;
070200100803             ErrGenerico = *on;
070300100803             PosCurTlt   = *on;
070400100803             leavesr;
070500100806
070600100806           // -?Lingua?
070700100809           //  ?N.B.?
070800100809           //  ?Richiede video fmt *DS3 (24x80) per una SUA window!?
070900100915           //When  %scan( '?' : D01lin ) > *zero;
071000100915           //  if  Not xDs4;
071100100915           //    clear  p37_iso2;
071200100915           //    clear  p37_desi;
071300100915           //    callp(E)  trul37r ( p37_iso2 : p37_desi );
071400100915           //    if  Not %error;
071500100915           //      D01lin  = p37_iso2;
071600100915           //      D01linD = p37_desi;
071700100915           //    endif;
071800100915           //  else;
071900100915           //    D01msg = 'Ricerca NON effettuabile con questo formato +
072000100915           //              video (richiede 24x80)...';
072100100915           //    errMessage  = *on;
072200100915           //  endif;
072300100915           //  ErrGenerico = *on;
072400100915           //  PosCurLin   = *on;
072500100915           //  leavesr;
072600100803
072700100803           // -?Commerciale?
072800100804           When  %scan( '?' : D01cmm ) > *zero;
072900100804             TB85kfil = %editc(DUTpou:'X');
073000100804             clear TB85key ;
073100100804             clear TB85des1;
073200100804             clear TB85uni;
073300100804             trtb85r ( kpjba : TB85kfil : TB85key : TB85des1 : TB85uni );
073400100804             D01cmm  = TB85key;
073500100927             D01cmmd = TB85des1;
073600100803             ErrGenerico = *on;
073700100803             PosCurCmm   = *on;
073800100803             leavesr;
073900100803
074000100803           EndSl;
074100100803
074200100803       ENDSR;
074300100804
074400100804       //--------------------------------------------------------------
074500100804       //?Controllo testo lettera.
074600100804       //--------------------------------------------------------------
074700100804       BEGSR  sr_CtrlTLT;
074800100804
074900100804         clear  D01tltD;
075000100804
075100100804         // -?Controllo inserimento?
075200100804         if  D01tlt = *blank;
075300100804           errMessage  = *on;
075400100804           errGenerico = *on;
075500100804           PosCurTlt   = *on;
075600100804           D01msg = $Msg(01);
075700100804           leavesr;
075800100804         endif;
075900100804
076000100806         // -?Controllo testo lettera in tab. "1H"?
076100100804         clear  ds1H;
076200100804         clear  FNLV41ds;
076300100804         I41let = D01tlt;
076400100804         I41mod = '2';
076500100804         fnlv41r ( FNLV41ds );
076600100804         if  O41err =  'E';
076700100804           errMessage  = *on;
076800100804           errGenerico = *on;
076900100804           PosCurTlt   = *on;
077000100804           D01msg = $Msg(02);
077100100804           leavesr;
077200100804         endif;
077300100804
077400100804         ds1H = O41dta;
077500100804         D01tltD = §1Hdes;
077600100804
077700100804         // -?Verifica se testo gestibile in questo *pgm?
077800100804         if  §1Hfl5 <> 'S';
077900100804           errMessage  = *on;
078000100804           errGenerico = *on;
078100100804           PosCurTlt   = *on;
078200100804           D01msg = $Msg(03);
078300100804           leavesr;
078400100804         endif;
078500100804
078600100804       ENDSR;
078700100806
078800100806       //--------------------------------------------------------------
078900100806       //?Controllo lingua.
079000100806       //--------------------------------------------------------------
079100100806       BEGSR  sr_CtrlLIN;
079200100806
079300100806         clear  D01linD;
079400100806
079500100806         // -?Controllo inserimento?
079600100806         if  D01lin = *blank;
079700100806           errMessage  = *on;
079800100806           errGenerico = *on;
079900100806           PosCurLin   = *on;
080000100806           D01msg = $Msg(04);
080100100806           leavesr;
080200100806         endif;
080300100806
080400100806         // -?Controllo validità?
080500100806         if  D01lin <> SaveLIN;
080600100806
080700100806           k_LINiso2 = D01lin;
080800100806           chain   %kds(k01azlin03)  AZLIN000;
080900100806           if  NOT  %found(AZLIN03L);
081000100806             errMessage  = *on;
081100100806             errGenerico = *on;
081200100806             PosCurLin   = *on;
081300100806             D01msg = $Msg(05);
081400100806             leavesr;
081500100806           endif;
081600100806
081700100806           SaveLIN = D01lin;
081800100806
081900100806         endif;
082000100806
082100100806         D01linD = LINdesIta;
082200100806
082300100806         // -?Se non è attiva per i testi: errore?
082400100806         //  ?(controllo NON più valido:?
082500100806         //  ? testo "chiodato" nella funzione di preparazione e-mail)?
082600100806         //if  LINfntxt = '0';
082700100806         //  errMessage  = *on;
082800100806         //  errGenerico = *on;
082900100806         //  PosCurLin   = *on;
083000100806         //  D01msg = $Msg(06);
083100100806         //  leavesr;
083200100806         //endif;
083300100806
083400100806         // -?Se si è scelto di stampare in lingua:?
083500100806         //  ?il testo deve essere gestito in lingua?
083600100806         if  §1Hlin = *blank   and   D01lin <> 'it';
083700100806           errMessage  = *on;
083800100806           errGenerico = *on;
083900100806           PosCurLin   = *on;
084000100806           D01msg = $Msg(07);
084100100806           leavesr;
084200100806         endif;
084300100806
084400100806       ENDSR;
084500100804
084600100804       //--------------------------------------------------------------
084700100804       //?Controllo commerciale.
084800100804       //--------------------------------------------------------------
084900100804       BEGSR  sr_CtrlCMM;
085000100804
085100100804         clear  D01cmmD;
085200100804
085300100804         // -?Controllo inserimento?
085400100805         if  D01cmm = *blank   or   D01cmm = *zero;
085500100804           errMessage  = *on;
085600100804           errGenerico = *on;
085700100804           PosCurCmm   = *on;
085800100806           D01msg = $Msg(11);
085900100804           leavesr;
086000100804         endif;
086100100804
086200100804         // -?Controllo effettiva numericità?
086300100806         if  %check( c_Digits : D01cmm ) > *zero;
086400100810           errMessage  = *on;
086500100810           errGenerico = *on;
086600100804           PosCurCmm   = *on;
086700100806           D01msg = $Msg(12);
086800100804           leavesr;
086900100804         endif;
087000100805
087100100806         If  D01cmm <> SaveCMM;
087200100806
087300100806           // -?Controllo abilitazione utente al commerciale?
087400100806           reset  TNTAA1ds;
087500100909           //iTAA1tipo = 'N';          ?(già così)?
087600100909           iTAA1cmm  = %int( D01cmm );
087700100909           //iTAA1abi  = 'RA';         ?(già così)?
087800100806           kpjbu     = TNTAA1ds;
087900100806           tntaa1c ( kpjba );
088000100806           TNTAA1ds = kpjbu;
088100100909           if  oTAA1fabi = 'N';
088200100810             errMessage  = *on;
088300100810             errGenerico = *on;
088400100806             PosCurCmm   = *on;
088500100806             D01msg = $Msg(13);
088600100806             leavesr;
088700100806           endif;
088800100806
088900100806           // -?Controllo esistenza in tab. "01"?
089000100806           clear  ds01 ;
089100100806
089200100809           k_TBLcod = '01';
089300100809           k_TBLkey = D01cmm;
089400100806           chain  %kds(k03tabel00)  TABEL;
089500100806           if  NOT  %found(TABEL00F)
089600100806               or   TBLflg <> *blank;
089700100806             errMessage  = *on;
089800100806             errGenerico = *on;
089900100806             PosCurCmm   = *on;
090000100806             D01msg = $Msg(13);
090100100806             leavesr;
090200100806           endif;
090300100806
090400100806           ds01 = TBLuni;
090500110614
090600110614         // -?Controllo validità?
090700111129           IF  §01dtdec > %editc(wOggi:'X') or
090800111129               §01dtfin < %editc(wOggi:'X');
090900110614             errMessage  = *on;
091000110614             errGenerico = *on;
091100110614             PosCurCmm   = *on;
091200110614             D01msg = $Msg(13);
091300110614             leavesr;
091400110614           endif;
091500111129
091600111129         // -?Controllo data validità?
091700111129           if  §01par <> *blanks;
091800111129             errMessage  = *on;
091900111129             errGenerico = *on;
092000111129             PosCurCmm   = *on;
092100111129             D01msg = $Msg(13);
092200111129             leavesr;
092300111129           endif;
092400110614
092500100806           SaveCMM = D01cmm;
092600100806
092700100806         EndIf;
092800100806
092900100806         D01cmmD = §01age;
093000100804
093100100804       ENDSR;
093200100806
093300100806       //--------------------------------------------------------------
093400100806       //?Reperimento dati da tab. "HTM" (e-mail del commerciale).     ?
093500100806       //--------------------------------------------------------------
093600100806       BEGSR  sr_TabHTM;
093700100806
093800100806         clear  dHTM;
093900100806
094000100806         k_TBEcod = 'HTM';
094100100806         k_TBEke1 = D01cmm;
094200100806         k_TBEke2 = 'CC';
094300100806
094400100806         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
094500100806
094600100806         if  NOT %found(TNTBE01L)   or   TBEatb <> *blank;
094700100806           errMessage  = *on;
094800100806           errGenerico = *on;
094900100806           PosCurCmm   = *on;
095000100806           D01msg = %replace( D01cmm : $Msg(14) :
095100100806                              %scan( '&CommLe' : $Msg(14) ) );
095200100806           leavesr;
095300100806         endif;
095400100806
095500100806         dHTM = TBEuni;
095600100806
095700100806       ENDSR;
095800100806
095900100806       //--------------------------------------------------------------
096000100806       //?Reperimento dati da tab. "MRA" (regole per invio e-mail).    ?
096100100806       //--------------------------------------------------------------
096200100806       BEGSR  sr_TabMRA;
096300100806
096400100806         clear  dMRAmk24;
096500100806
096600100806         k_TBEcod = 'MRA';
096700100806         k_TBEke1 = %trim(SDSpgm) + '-' + D01tlt;
096800100806         k_TBEke2 = D01lin;
096900100806
097000100806         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
097100100806
097200100806         if  NOT %found(TNTBE01L)   or   TBEatb <> *blank;
097300100806           errMessage  = *on;
097400100806           errGenerico = *on;
097500100809           PosCurTlt   = *on;
097600100806           D01msg = %replace( k_TBEke1 : $Msg(15) :
097700100806                              %scan( '&TbeKe1' : $Msg(15) ) );
097800100806           D01msg = %replace( k_TBEke2 : D01msg :
097900100806                              %scan( '&TbeKe2' : D01msg ) );
098000100806           leavesr;
098100100806         endif;
098200100806
098300100806         dMRAmk24 = TBEuni;
098400100806
098500100806         //if  §MRAobjm = *blank;
098600120305         //  §MRAobjm = 'Comunicazione da BRT S.p.A.';
098700100806         //endif;
098800100806
098900100806       ENDSR;
099000100806
099100100806       //--------------------------------------------------------------
099200100806       //?Reperimento progressivo contatore "024"                      ?
099300100806       //--------------------------------------------------------------
099400100806       BEGSR  sr_Num024;
099500100806
099600100806         reset  TRUL33ds;
099700100806
099800100806         kpjbu  = TRUL33ds;
099900100806         trul33r ( kpjba );
100000100806         TRUL33ds = kpjbu;
100100100806
100200100806         if  O33err <> *zero;
100300100810           errMessage  = *on;
100400100810           errGenerico = *on;
100500100806           D01msg = $Msg(17);
100600100806           leavesr;
100700100806         endif;
100800100806
100900100806       ENDSR;
101000100806
101100100806       //--------------------------------------------------------------
101200100806       //?Decodifica filiale del commerciale.                          ?
101300100806       //--------------------------------------------------------------
101400100806       BEGSR  sr_DecodFIL;
101500100806
101600100806         ORGfil = %int( D01cmm ) / 10000;
101700100806         chain  (ORGfil)  AZORG;
101800100806         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
101900100806           clear  ORGdes;
102000100806           clear  ORGind;
102100100806           clear  ORGcpf;
102200100806           clear  ORGloc;
102300100806           clear  ORGpro;
102400100806           clear  ORGtel;
102500100806           clear  ORGfax;
102600100806         endif;
102700100806
102800100806       ENDSR;
102900100803
103000100803       //--------------------------------------------------------------
103100100803       //?Preparazione ed invio e-mail.                                ?
103200100803       //--------------------------------------------------------------
103300100803       BEGSR  sr_PrtEmail;
103400100805
103500100805         // -?Override ai file di stampa?
103600100805         //  ?(per impostarvi i dati per l'invio via e-mail)?
103700100805         exsr sr_Override;
103800100806         if  ErrGenerico;
103900100806           leavesr;
104000100806         endif;
104100100805
104200100805         // -?Apertura file di stampa?
104300100805         open  PrtEMAIL;
104400100803
104500100803         // -?Stampa "corpo e-mail"?
104600100810         For  xx = 1  To  10;
104700100901           //O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
104800100803           select;
104900100803             when  xx = 1;
105000100901               O_txt = '§§Nominativo§§#' + %triml( D01rst1 )
105100100901                                         + %trimr( D01rst2 );
105200100809             when  xx = 2;
105300100901               O_txt = '§§Firma001§§#' + %triml( §HTMnmc );
105400100809             when  xx = 3;
105500100901               O_txt = '§§Firma002§§#' + %triml( ORGdes );
105600100809             when  xx = 4;
105700100901               O_txt = '§§Firma003§§#' + %triml( ORGind );
105800100809             when  xx = 5;
105900100901               O_txt = '§§Firma004§§#' + %editc( ORGcpf : 'X' );
106000100809             when  xx = 6;
106100100901               O_txt = '§§Firma005§§#' + %triml( ORGloc );
106200100809             when  xx = 7;
106300100901               O_txt = '§§Firma006§§#' + ORGpro;
106400100809             when  xx = 8;
106500100901               O_txt = '§§Firma007§§#' + %triml( ORGtel );
106600100809             when  xx = 9;
106700100901               O_txt = '§§Firma008§§#' + %triml( ORGfax );
106800100809             when  xx = 10;
106900100907               O_txt = '§§nome@bartolini.it§§#' + %trim( §HTMind )
107000100901                                                + c_at;
107100100803           endsl;
107200100803           O_txt = %trimr(O_txt) + '}{';
107300100803           except  PRTparm;
107400100803         EndFor;
107500100805
107600100805         // -?Chiusura file di stampa?
107700100805         close  PrtEMAIL;
107800100805
107900100805         // -?Cancellazione override ai file di stampa?
108000100805         exsr sr_DeleteOvr;
108100100803
108200100803       ENDSR;
108300100803
108400100803       //--------------------------------------------------------------
108500100803       //?Override al file di stampa per impostarvi i dati per         ?
108600100803       //?  l'invio via e-mail.                                        ?
108700100803       //--------------------------------------------------------------
108800100803       BEGSR sr_Override;
108900100803
109000100803         reset TRTCM1ds;
109100100803         if  §MRAmitt <> *blank;
109200100803           §CM1mitt = %trim(§MRAmitt);
109300100803         else;
109400100805           §CM1mitt = %trim(§HTMind);
109500100803         endif;
109600100901         §CM1dst  = D01mai1 + D01mai2;
109700100803         §CM1tips = §MRAregCor;
109800100803         §CM1po   = %editc( DUTpou : 'X' );
109900100908         §CM1var  = %trimr(§CM1var) + %trim(D01obj) + ' -' + Imk24tde
110000100908                                                    + '.';
110100100908         if  Imk24tde = 'C';
110200100908           §CM1var = %trimr(§CM1var) +
110300100908                     %subst( %editc( Imk24cde : 'X' ) : 5 );
110400100908         else;
110500100908           §CM1var = %trimr(§CM1var) +
110600100908                     %editc( Imk24cde : 'X' );
110700100908         endif;
110800100803         §CM1sts  = §MRAsts;
110900100803         §CM1idp  = §MRAidpro;
111000100803         §CM1prog = %subst( %editc( O33nri : 'X' ) :
111100100803                            %size(O33nri) - %size(§CM1prog) + 1 );
111200100803
111300100803         // -?Override al "corpo e-mail"?
111400100803         if  §MRAregCor <> *blank;
111500100803           Qcmd = c_CmdOvrPrtF
111600100907                + ' usrdta(''' + %trim(SDSpgm) + '-' + D01tlt + ''')'
111700100803                + ' outq(' + %trim(§MRAoutqi) + ')'
111800100803                + ' usrdfndta(''' + TRTCM1ds + ''')';
111900100803         else;
112000100803           Qcmd = c_CmdOvrPrtF;
112100100803         endif;
112200100803
112300100803         exsr  sr_ExecCmd;
112400100803
112500100803         if  Qusei <> *blank;
112600100806           //if  %parms > 1;
112700100806           //  Omk24err = *on;
112800100806           //  Omk24msg = 'Fallita override al file di stampa per +
112900100806           //              "corpo e-mail"';
113000100806           //endif;
113100100806           //exsr  sr_RoutEnd;
113200100810           errMessage  = *on;
113300100810           errGenerico = *on;
113400100806           D01msg = $Msg(18);
113500100806           leavesr;
113600100803         endif;
113700100803
113800100803       ENDSR;
113900100803
114000100803       //--------------------------------------------------------------
114100100803       //?Chiusura file stampa & Cancellazione override.               ?
114200100803       //--------------------------------------------------------------
114300100803       BEGSR sr_DeleteOvr;
114400100803
114500100803         // -?Cancellazione override al "corpo e-mail"?
114600100803         if %open(PrtEMAIL);
114700100803           close PRTEMAIL;
114800100803           Qcmd = c_CmdDltOvr;
114900100803           exsr  sr_ExecCmd;
115000100803         endif;
115100100803
115200100803       ENDSR;
115300100803
115400100803       //--------------------------------------------------------------
115500100803       //?Esecuzione del comando (già impostato).                      ?
115600100803       //--------------------------------------------------------------
115700100803       BEGSR  sr_ExecCmd;
115800100803
115900100803         clear Qcap0100;
116000100803         Qcabcsdh = *off;
116100100803         Qcapa    = *off;
116200100803         Qcacmdss = *off;
116300100803         Qcaerved = *allX'00';
116400100803
116500100803         clear Qusec;
116600100803         Qusbprv  = %size(Qusec);
116700100803
116800100803         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
116900100803                           %size(Qcap0100) : 'CPOP0100' : *omit :
117000100803                           0 : 0 : Qusec);
117100100803
117200100803         //if  Qusei <> *blank;
117300100803         //  ...;
117400100803         //endif;
117500100803
117600100803       ENDSR;
117700100803
117800100803       //--------------------------------------------------------------
117900100803       //?Operazioni finali.                                           ?
118000100803       //--------------------------------------------------------------
118100100803       BEGSR  sr_RoutEnd;
118200100803
118300100805         // -?Restituzione parametri?
118400100806         if  %parms > 1;
118500100908           Imk24lin   = D01lin;
118600100915           //Omk24mitt  = §CM1mitt;
118700100908           Omk24tlt   = D01tlt;
118800100915           Omk24lin   = D01lin;
118900100908           Omk24mai   = D01mai1 + D01mai2;
119000100908           Omk24obj   = D01obj;
119100100908           Omk24rst   = D01rst1 + D01rst2;
119200100908           Omk24cmm   = %int( D01cmm );
119300100915           Omk24cmd   = D01cmmd;
119400100803         endif;
119500100803
119600100803         // -?Uscita?
119700100803         return;
119800100803
119900100803       ENDSR;
120000100803
120100100803      /end-free
120200100803
120300100803       //--------------------------------------------------------------
120400100803       //?Specifiche di stampa.                                        ?
120500100803       //--------------------------------------------------------------
120600100803
120700100803     oPrtEMAIL  e            PRTparm     1
120800100803     o                       O_txt
120900100804
121000100804       //--------------------------------------------------------------
121100100804       //?Messaggi di errore.                                          ?
121200100804       //--------------------------------------------------------------
121300100804
121400100804** -?$Msg?-------------------------------------------------------------------*
121500100804Testo lettera obbligatorio                                                      1
121600100804Testo lettera errato                                                            2
121700100805Testo lettera non utilizzabile in questa gestione                               3
121800100806Lingua obbligatoria                                                             4
121900100806Lingua errata                                                                   5
122000100806Lingua NON attiva per i testi ----------------  L I B E R O   ----------------  6
122100100806Testo NON gestito in lingua                                                     7
122200100806Per testo lettera HTML è obbligatorio inserire il responsabile trasporti        8
122300100806Per testo lettera HTML è obbligatorio inserire l'indirizzo e-mail (nota 06)     9
122400100806Per testo lettera HTML è obbligatorio inserire l'oggetto della e-mail          10
122500100806Codice commerciale obbligatorio                                                11
122600100806Codice commerciale NON numerico                                                12
122700100806Codice commerciale errato o NON in gestione all'utente                         13
122800100806Dati del commerciale "&CommLe" non reperiti in tab. "HTM"                      14
122900100806Parametri "&TbeKe1        "/"&TbeKe2        " non reperiti in tab. "MRA"       15
123000100806Fallito reperimento testo lettera                                              16
123100100806Fallito reperimento numeratore "&Nr"                                           17
123200100806Fallita override al file di stampa per "corpo e-mail" (vedi job-log)           18
123300100902Il testo lettera DEVE essere HTML                                              19
