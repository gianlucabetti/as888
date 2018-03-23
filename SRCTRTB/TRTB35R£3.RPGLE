000100091113       //==============================================================
000200091113       //?TRTB35R * Manutenzione tab. "5F" = Segnacolli confermabili   ?
000300091113       //?                                   da altre filiali          ?
000400091113       //==============================================================
000500091113
000600091113     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700091113     h dftactgrp(*no)
000800110506     h bnddir('UBBNDDIR':'TRUL')
000900091113
001000091113       //--------------------------------------------------------------
001100091113       //?Dichiarazione file.                                          ?
001200091113       //--------------------------------------------------------------
001300091113
001400091113       // - Organigramma filiale/aziendale
001500091113     fAZORG01L  if   e           k disk
001600091113
001700091113       // - Tabella "3C"
001800091113     fTABEL00F  Uf A e           k disk
001900091113     f                                     extfile(wLibFile)  usropn
002000091113
002100091113       // - Video Gestione
002200091117     fTRTB35D   cf   e             workstn
002300091113     f                                     indds(IndDspF)
002400091113     f                                     infds(InfDspF)
002500091113
002600091113       //--------------------------------------------------------------
002700091113       //?Definizione costanti.                                        ?
002800091113       //--------------------------------------------------------------
002900091113
003000091113       // - Codice tabella in gestione
003100091113     d c_Tab           c                   const('5F')
003200091113
003300091113       // - Codice utente
003400091113     d c_Kut           c                   const(1)
003500091113
003600091113       // - Costante utente "EDP"
003700091113     d c_Edp           c                   const('EDP')
003800091113
003900091113       // - Costante identificativa del file TABEL per *srvpgm TRULTAB
004000091113     d c_TABEL         c                   const('1')
004100091117
004200091117       // - Num. elem. della schiera di filiali (Terminal) in TBLUNI,
004300091117       //   ovvero  numero di filiali gestibili a video
004400091117     d c_MaxFil        c                   const(11)
004500091113
004600091113       // - Costante per controllo "caratteri solo numerici"
004700091113     d c_Digits        c                   const('0123456789')
004800091113
004900091113       // - Costanti per la definizione delle schiere con i nomi
005000091113       //   degli iSeries da elaborare e delle relative librerie
005100091113     d c_NrSyst        c                   const(2)
005200091113     d c_NrLibr        c                   const(2)
005300091113
005400091113       // - Tasti funzionali a video
005500091113     d c_F01           c                   const(x'31')
005600091113     d c_F02           c                   const(x'32')
005700091113     d c_F03           c                   const(x'33')
005800091113     d c_F04           c                   const(x'34')
005900091113     d c_F05           c                   const(x'35')
006000091113     d c_F06           c                   const(x'36')
006100091113     d c_F07           c                   const(x'37')
006200091113     d c_F08           c                   const(x'38')
006300091113     d c_F09           c                   const(x'39')
006400091113     d c_F10           c                   const(x'3A')
006500091113     d c_F11           c                   const(x'3B')
006600091113     d c_F12           c                   const(x'3C')
006700091113     d c_F13           c                   const(x'B1')
006800091113     d c_F14           c                   const(x'B2')
006900091113     d c_F15           c                   const(x'B3')
007000091113     d c_F16           c                   const(x'B4')
007100091113     d c_F17           c                   const(x'B5')
007200091113     d c_F18           c                   const(x'B6')
007300091113     d c_F19           c                   const(x'B7')
007400091113     d c_F20           c                   const(x'B8')
007500091113     d c_F21           c                   const(x'B9')
007600091113     d c_F22           c                   const(x'BA')
007700091113     d c_F23           c                   const(x'BB')
007800091113     d c_F24           c                   const(x'BC')
007900091113     d c_Enter         c                   const(x'F1')
008000091113     d c_RollDown      c                   const(x'F4')
008100091113     d c_RollUp        c                   const(x'F5')
008200091113
008300091113       //--------------------------------------------------------------
008400091113       //?Definizione schiere.                                         ?
008500091113       //--------------------------------------------------------------
008600091113
008700091113       // - iSeries  &  Librerie con entrambi i file tabelle
008800091116     d $iSystem        s              8    dim(c_NrSyst)
008900091113     d                                     ctdata   perrcd( 1)
009000091116     d $SisInf         s                   like(ds_Libr)
009100091113     d                                     dim(c_NrSyst)
009200091113     d                                     alt($iSystem)
009300091113
009400091113       // - Messaggi di errore
009500091113     d $Msg            s             78    dim(10) ctdata perrcd(1)
009600091113
009700091113       // - Terminal in TBLKEY
009800110506     d $5F_Term        s              3    dim(2500)     inz
009900091113       // - Lnp + Nrs in TBLKEY
010000110506     d $5F_LnpNrs      s              5    dim(2500)     inz
010100091113       // - Filiali in TBLUNI
010200110506     d $5F_Uni         s             89    dim(2500)     inz
010300091113       // - Flags Annullamento (TBLFLG)
010400110506     d $5F_Ann         s              1    dim(2500)     inz
010500091113
010600091113       // - Filiali in TBLUNI del singolo record
010700091113     d ds5F_Fil        ds            87    inz
010800091117     d   $5F_Fil                      3    dim(29)       inz
010900091113
011000091113       // - Terminal
011100091117     d $Term           s              3  0 dim(c_MaxFil) inz
011200091117     d $SaveTerm       s              3  0 dim(c_MaxFil) inz
011300091117
011400091117       // - Salvataggio filiali gestite a video
011500091117     d $SaveLin        s                   like(V2Cf01)
011600091117     d                                     dim(c_MaxFil)  inz
011700091116
011800091116       // - Ridefinizione elenco librerie in elaborare le tabelle
011900091116     d ds_Libr         ds                  inz
012000091116     d  $Libr                        10    dim(c_NrLibr) inz
012100091113
012200091113       //--------------------------------------------------------------
012300091113       //?Definizione aree dati.                                       ?
012400091113       //--------------------------------------------------------------
012500091113
012600091113       // - Dati utente
012700091113     d §AzUte        e ds                  extname(AZUTE00F)
012800091113     d                                     dtaara
012900091113     d §DatiUte      e ds                  extname(dDatiUte)
013000091113     d                                     dtaara
013100091113
013200091113       //--------------------------------------------------------------
013300091113       //?Definizione strutture dati.                                  ?
013400091113       //--------------------------------------------------------------
013500091113
013600091113       // - Status ds
013700091113     d Status         sds
013800091113     d  SDSpgm           *proc
013900091113
014000091113       // - InfDS
014100091113     d InfDspF         ds
014200091113     d   dsp_aid             369    369a
014300091113
014400091113       // - Indicatori su DspF
014500091113     d IndDspF         ds                  inz
014600091113         // - Abilitazione tasti funzionali
014700091113     d   F5Attivo                      n   overlay(IndDspF : 05)
014800091113     d   F6Attivo                      n   overlay(IndDspF : 06)
014900091113     d   F10Attivo                     n   overlay(IndDspF : 10)
015000091113     d   F11Attivo                     n   overlay(IndDspF : 11)
015100091113     d   F12Attivo                     n   overlay(IndDspF : 12)
015200091113     d   F16Attivo                     n   overlay(IndDspF : 16)
015300091113     d   F17Attivo                     n   overlay(IndDspF : 17)
015400091113     d   F18Attivo                     n   overlay(IndDspF : 18)
015500091113     d   F19Attivo                     n   overlay(IndDspF : 19)
015600091113     d   F20Attivo                     n   overlay(IndDspF : 20)
015700091113     d   F21Attivo                     n   overlay(IndDspF : 21)
015800091113     d   F22Attivo                     n   overlay(IndDspF : 22)
015900091113         // - Emissione messaggio di errore
016000091113     d   ErrMessage                    n   overlay(IndDspF : 28)
016100091113         // - Indicatori per Attibuti di visualizzazione
016200091113     d   VisBloAnn                     n   overlay(IndDspF : 40)
016300091113     d   VisInfoES                     n   overlay(IndDspf : 41)
016400091113         // - Posizionamento cursore & segnalazione errore
016500091113     d   PosCurLNP                     n   overlay(IndDspF : 51)
016600091113     d   PosCurNRS                     n   overlay(IndDspF : 52)
016700091113     d   PosCurF01                     n   overlay(IndDspF : 53)
016800091113     d   PosCurF02                     n   overlay(IndDspF : 54)
016900091113     d   PosCurF03                     n   overlay(IndDspF : 55)
017000091113     d   PosCurF04                     n   overlay(IndDspF : 56)
017100091113     d   PosCurF05                     n   overlay(IndDspF : 57)
017200091113     d   PosCurF06                     n   overlay(IndDspF : 58)
017300091113     d   PosCurF07                     n   overlay(IndDspF : 59)
017400091113     d   PosCurF08                     n   overlay(IndDspF : 60)
017500091113     d   PosCurF09                     n   overlay(IndDspF : 61)
017600091113     d   PosCurF10                     n   overlay(IndDspF : 62)
017700091113     d   PosCurF11                     n   overlay(IndDspF : 63)
017800091113         //   - Riemissione videata
017900091113     d   ErrGenerico                   n   overlay(IndDspF : 99)
018000091113
018100091113       // - Parametri ricevuti
018200091113     d KPJBA         e ds
018300091113
018400091113       // - Parametri per Reperimento dati utente
018500091113     d TIBS34ds      e ds                  inz
018600091113
018700091113       // - Parametri per Ricerca terminal di Partenza/Arrivo
018800091113     d FNLV55ds      e ds                  inz
018900091113     d   D55tpt      e                     inz('P')
019000091113
019100091113       // - Strutture di comodo per il raggruppamento dei campi a video
019200091113     d ds_V2Cfil       ds                  inz
019300091113     d  V2Cf01                             inz
019400091113     d  V2Cf02                             inz
019500091113     d  V2Cf03                             inz
019600091113     d  V2Cf04                             inz
019700091113     d  V2Cf05                             inz
019800091113     d  V2Cf06                             inz
019900091113     d  V2Cf07                             inz
020000091113     d  V2Cf08                             inz
020100091113     d  V2Cf09                             inz
020200091113     d  V2Cf10                             inz
020300091113     d  V2Cf11                             inz
020400091117     d   $Lin                  1     33    dim(c_MaxFil)  inz
020500091113     d ds_V2Dfil       ds                  inz
020600091113     d  V2Df01                             inz
020700091113     d  V2Df02                             inz
020800091113     d  V2Df03                             inz
020900091113     d  V2Df04                             inz
021000091113     d  V2Df05                             inz
021100091113     d  V2Df06                             inz
021200091113     d  V2Df07                             inz
021300091113     d  V2Df08                             inz
021400091113     d  V2Df09                             inz
021500091113     d  V2Df10                             inz
021600091113     d  V2Df11                             inz
021700091117     d   $Des                  1    110    dim(c_MaxFil)  inz
021800091113
021900091113       // - Parametri per Controllo/Inversione date
022000091113     d WLBdat          ds                  inz
022100091113     d  G02dat                        8  0 inz
022200091113     d  G02inv                        8  0 inz
022300091113     d  G02err                        1    inz
022400091113     d  G02tgi                        5  0 inz
022500091113
022600091113       //--------------------------------------------------------------
022700091113       //?Definizione variabili globali.                               ?
022800091113       //--------------------------------------------------------------
022900091113
023000091113       // - Flags booleani
023100091113     d $Fine           s               n   inz
023200091113     d $InzD01         s               n   inz(*on)
023300091113     d $InzD02         s               n   inz(*on)
023400091117     d $Annull         s               n   inz
023500091113
023600091113       // - Variabili per la gestione del video
023700091113     d $Video          s              2    inz('D1')
023800091116
023900091116       // - Indici di schiera
024000091116     d ww              s              5  0 inz
024100091116     d xx              s              5  0 inz
024200091116     d yy              s              5  0 inz
024300091113
024400091113       // - Nome del sistema
024500091113     d currSysNeta     s              8a   inz
024600091113
024700091113       // - Nome esteso Libreria/File dei 2 file tabella
024800091113     d wLibFile        s             21a   inz
024900091113
025000091113       // - Terminal di Partenza
025100091113     d wwwTFP          s                   like(D55tfp)
025200091113
025300091113       // - Parametri per TNSD19R
025400091113     d SD19cod         s              3    inz
025500091113     d SD19tip         s              1    inz
025600091113     d SD19des         s             25    inz
025700091113
025800091113       // - Campi di comodo
025900091113     d wDate_EUR       s               d   datfmt(*eur)  inz
026000091113     d wDate           s              8  0 inz
026100091113     d w_iSystem       s              1  0 inz
026200091113     d w_SisInf        s              3  0 inz
026300091113     d w005a           s              5    inz
026400091113
026500091113       //--------------------------------------------------------------
026600091113       //?Definizione prototipi procedure.                             ?
026700091113       //--------------------------------------------------------------
026800091113
026900091113       // - Reperimento dati utente
027000091113      /copy gaitrasrc/srcProtoPR,TIBS34R
027100091113
027200091113       // - Controllo Terminal
027300091113      /copy gaitrasrc/srcProtoPR,FNLV55R
027400091113
027500091113       // - Ricerca codici Organigramma
027600091113     d tnsd19r         pr                  extpgm('TNSD19R')
027700091113     d  D19cod                             like(SD19cod)
027800091113     d  D19tip                             like(SD19tip)
027900091113     d  D19des                             like(SD19des)
028000091116
028100091116       // - Reperimento NETA sistema AS/400 corrente
028200091116     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
028300091113
028400091113       //--------------------------------------------------------------
028500091113       //?Definizione key-list.                                        ?
028600091113       //--------------------------------------------------------------
028700091113
028800091113       // - File TABEL00F
028900091113     d k03tabel00    e ds                  extname(TABEL00F : *key)
029000091113     d                                     prefix(k_)   inz
029100091113
029200091113       //--------------------------------------------------------------
029300091113       //?M A I N - L I N E                                            ?
029400091113       //--------------------------------------------------------------
029500091113
029600091113     c     *Entry        plist
029700091113     c                   parm                    KPJBA
029800091113
029900091113      /free
030000091113
030100091113       // -?Operazioni iniziali?
030200091113       exsr sr_RoutInz;
030300091113
030400091113       // -?Gestione videate?
030500091113       DoW  $Fine = *off;
030600091113
030700091113         select;
030800091113
030900091113           // - Gestione videata D1
031000091113           when $Video = 'D1';
031100091113             exsr sr_GesD01;
031200091113
031300091113           // - Gestione videata D2
031400091113           when $Video = 'D2';
031500091113             exsr sr_GesD02;
031600091113
031700091113           other;
031800091113             $Fine = *on;
031900091113
032000091113         endsl;
032100091113
032200091113       EndDo;
032300091113
032400091113       // -?Operazioni finali?
032500091113       exsr sr_RoutEnd;
032600091113
032700091113       //--------------------------------------------------------------
032800091113       //?Operazioni iniziali.                                         ?
032900091113       //--------------------------------------------------------------
033000091113       BEGSR  sr_RoutInz;
033100091113
033200091113         *inLR = *on;
033300091113
033400091113         // -?Verifica del sistema AS/400 corrente?
033500091113         if  ubRtvNetA_Rtv(currSysNeta) <> *zero;
033600091113           $Fine = *on;
033700091113           leavesr;
033800091113         endif;
033900091113
034000091113         // -?Impostazione elenco librerie in cui gestire le tabelle?
034100091113         //  ?(a seconda del sistema in cui si stà lavorando)?
034200091113         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
034300091113         if  w_iSystem = *zero;
034400091113           $Fine = *on;
034500091113           leavesr;
034600091113         endif;
034700091113
034800091113         // -?Reperimento dati job?
034900091113         exsr  sr_DatiJob;
035000091113
035100091113         // -?Impostazione nome programma a video?
035200091113         V1Tpgm = SDSpgm;
035300091113
035400091113         // -?Impostazione campi "fissi"?
035500091113         k_TBLkut = c_Kut;
035600091113         k_TBLcod = c_Tab;
035700091113
035800091113         // -?Reperimento data odierna?
035900091113         wDate = %int( %subst( %char( %dec( %timestamp() ) )
036000091113                               : 1 : 8 ) );
036100091113
036200091113       ENDSR;
036300091113
036400091113       //--------------------------------------------------------------
036500091113       //?Reperimento Dati del job (Utente/Operativi).                 ?
036600091113       //--------------------------------------------------------------
036700091113       BEGSR  sr_DatiJob;
036800091113
036900091113         in(e) §AzUte;
037000091113         if NOT %error;
037100091113           in(e) §DatiUte;
037200091113         endif;
037300091113         if %error or RSut = *blank;
037400091113           tibs34r ( tibs34ds );
037500091113           in §AzUte;
037600091113           in §DatiUte;
037700091113         endif;
037800091113
037900091113       ENDSR;
038000091113
038100091113       //--------------------------------------------------------------
038200091113       //?Gestione videata D01.                                        ?
038300091113       //--------------------------------------------------------------
038400091113       BEGSR  sr_GesD01;
038500091113
038600091113         // -?Inizializzazione videata?
038700091113         if  $InzD01 = *on;
038800091116           clear  TB35D1;
038900091113           exsr  sr_CarTAB;
039000091113           $InzD01 = *off;
039100091113         endif;
039200091113
039300091116         // -?Emissione Testata e Piede?
039400091116         write  TB35T1;
039500091116         write  TB35P1;
039600091113
039700091113         // -?Emissione videata?
039800091116         exfmt  TB35D1;
039900091113
040000091116         clear  V1Dmsg;
040100091113         reset  ErrMessage;
040200091113         reset  ErrGenerico;
040300091113
040400091113         SELECT;
040500091113
040600091113           // -?F3=Fine?
040700091113           WHEN  dsp_aid = c_F03;
040800091113             $Fine = *on;
040900091113
041000091113           // -?Invio?
041100091113           OTHER;
041200091113             exsr sr_CtrD01;
041300091113             if  ErrGenerico = *off;
041400091113               $Video    = 'D2';
041500091113               $InzD02   = *on;
041600091113             endif;
041700091113
041800091113         ENDSL;
041900091113
042000091113       ENDSR;
042100091113
042200091113       //--------------------------------------------------------------
042300091113       //?Controllo dati immessi nella videata D01.                    ?
042400091113       //--------------------------------------------------------------
042500091113       BEGSR  sr_CtrD01;
042600091113
042700091113         %subst(IndDspF : 50) = *off;
042800091113
042900091113         // -?Linea di Partenza Segnacollo?
043000091113         clear  V1Dlnp;
043100091113         if  V1Clnp = *zero;
043200091116           V1Dmsg = $Msg(01);
043300091113           PosCurLNP   = *on;
043400091113           ErrMessage  = *on;
043500091113           ErrGenerico = *on;
043600091113           leavesr;
043700091113         endif;
043800091113         chain  (V1Clnp)  AZORG;
043900091116         if  %found(AZORG01L);
044000091113           V1Dlnp = ORGdes;
044100091113         endif;
044200091113         if  NOT %found(AZORG01L)   or
044300091113             ORGfva <> *blank       or
044400091113            (ORGfag <> 'A'      and
044500091113             ORGfag <> 'F');
044600091116           V1Dmsg = $Msg(02);
044700091113           PosCurLNP   = *on;
044800091113           ErrMessage  = *on;
044900091113           ErrGenerico = *on;
045000091113           leavesr;
045100091113         endif;
045200091113
045300091113         // -?Reperimento Terminal Partenza Linea Segnacollo?
045400091113         reset  FNLV55ds;
045500091113         D55lin = V1Clnp;
045600091113         D55drf = wDate;
045700091113         fnlv55r ( FNLV55ds );
045800091113         if  D55err = *blank;
045900091113           wwwTFP = D55tfp;
046000091113         else;
046100091113           wwwTFP = V1Clnp;
046200091113         endif;
046300091113
046400091113         // -?Serie Segnacollo?
046500091113         if  V1Cnrs = *zero;
046600091116           V1Dmsg = $Msg(03);
046700091113           PosCurNRS   = *on;
046800091113           ErrMessage  = *on;
046900091113           ErrGenerico = *on;
047000091113           leavesr;
047100091113         endif;
047200091113
047300091113       ENDSR;
047400091113
047500091113       //--------------------------------------------------------------
047600091113       //?Caricamento tabelle con terminal in KEY                      ?
047700091113       //--------------------------------------------------------------
047800091113       BEGSR  sr_CarTAB;
047900091113
048000091113         // -?Apertura file TABEL00F del 1° S.I. (sede)?
048100091113         w_SisInf = 1;
048200091113         exsr  sr_OpenFileTab;
048300091113
048400091113         clear  $5F_Term;
048500091113         clear  $5F_LnpNrs;
048600091113         clear  $5F_Uni;
048700091113         clear  $5F_Ann;
048800091113         clear  xx;
048900091113
049000091113         // - Tabelle separate per KEY SENZA terminal
049100091113         //                      e KEY CON   terminal
049200091113         //   => si elaborano SOLO quelle CON terminal
049300091113         k_TBLkey = '0010000';
049400091113
049500091113         // - Caricamento record validi dalla tabella
049600091113         setll    %kds(k03tabel00)      TABEL;
049700091113         reade(n) %kds(k03tabel00 : 2)  TABEL;
049800091113
049900091113         DoW  NOT %eof(TABEL00F);
050000091113
050100091113           if  %subst(TBLkey : 1 : 3) <> *blank;
050200091113             xx += 1;
050300091113             $5F_Term(xx)   = %subst( TBLkey : 1 : 3 );
050400091113             $5F_LnpNrs(xx) = %subst( TBLkey : 4 : 5 );
050500091113             $5F_Uni(xx) = TBLuni;
050600091116             $5F_Ann(xx) = TBLflg;
050700091113           endif;
050800091113
050900091113           reade(n) %kds(k03tabel00 : 2)  TABEL;
051000091113
051100091113         EndDo;
051200091113
051300091113       ENDSR;
051400091113
051500091113       //--------------------------------------------------------------
051600091113       //?Apertura dei files tabelle nel sistema informativo impostato.?
051700091113       //--------------------------------------------------------------
051800091113       BEGSR  sr_OpenFileTab;
051900091113
052000091113         // -?Chiusura (eventuale) archivio?
052100091113         if  %open(TABEL00F);
052200091113           close  TABEL00F;
052300091113         endif;
052400091113
052500091113         // -?Apertura archivio?
052600091116         ds_Libr  = $SisInf(w_iSystem);
052700091113         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
052800091113         open  TABEL00F;
052900091113
053000091113       ENDSR;
053100091113
053200091113       //--------------------------------------------------------------
053300091113       //?Gestione videata D02.                                        ?
053400091113       //--------------------------------------------------------------
053500091113       BEGSR  sr_GesD02;
053600091113
053700091113         // -?Inizializzazione videata?
053800091113         if  $InzD02 = *on;
053900091113           exsr  sr_InzD02;
054000091113           $InzD02 = *off;
054100091113         endif;
054200091113
054300091113         // -?Emissione Testata e Piede?
054400091116         if  Not ErrMessage;
054500091116           write  TB35T1;
054600091116           write  TB35D1;
054700091116           write  Protect;
054800091116           write  TB35P2;
054900091116         endif;
055000091113
055100091113         // -?Emissione videata?
055200091117         if  Not $Annull;
055300091113           exfmt  TB35D2;
055400091113         else;
055500091113           write  TB35D2;
055600091113           exfmt  Protect;
055700091113         endif;
055800091113
055900091113         reset  ErrMessage;
056000091113         reset  ErrGenerico;
056100091116         clear  V1Dmsg;
056200091113
056300091113         SELECT;
056400091113
056500091113           // -?F3=Fine?
056600091113           WHEN  dsp_aid = c_F03;
056700091113             unlock  TABEL00F;
056800091113             $Fine = *on;
056900091113
057000091113           // -?F12=Ritorno?
057100091113           WHEN  dsp_aid = c_F12;
057200091113             unlock  TABEL00F;
057300091113             $Video = 'D1';
057400091113
057500091113           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
057600091113           OTHER;
057700091113             // - Controlli eseguiti NON se F16=Annullamento
057800091113             if  dsp_aid <> c_F16;
057900091113               exsr sr_CtrD02;
058000091113               if  ErrGenerico = *on;
058100091113                 leavesr;
058200091113               endif;
058300091113             endif;
058400091113             // - Aggiornamento
058500091113             if  dsp_aid = c_F05   or
058600091113                 dsp_aid = c_F06   or
058700091113                 dsp_aid = c_F16;
058800091113               exsr  sr_UpdateAll;
058900091113               if  ErrGenerico = *on;
059000091113                 leavesr;
059100091113               endif;
059200091113               $Video  = 'D1';
059300091113               $InzD01 = *on;
059400091113             endif;
059500091113
059600091113         ENDSL;
059700091113
059800091113         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
059900091113         if  $Video = 'D1';
060000091113           clear  V1Topz;
060100091113         endif;
060200091113
060300091113       ENDSR;
060400091113
060500091113       //--------------------------------------------------------------
060600091113       //?Inizializzazione dati nella videata D02.                     ?
060700091113       //--------------------------------------------------------------
060800091113       BEGSR  sr_InzD02;
060900091113
061000091117         reset  $Annull;
061100091117
061200091113         // -?Verifica esistenza del record in TABEL?
061300091113         //k_TBLkey = '   '
061400091113         //         + %editc(V1Clnp : 'X')
061500091113         //         + %editc(V1Cnrs : 'X');
061600091113         evalr k_TBLkey = %editc(V1Clnp : 'X')
061700091113                        + %editc(V1Cnrs : 'X');
061800091113         chain  %kds(k03tabel00)  TABEL;
061900091113
062000091113         // -?Impostazione testata?
062100091113         clear  V1Topz;
062200091113         select;
062300091113           when  Not %found(TABEL00F);
062400091117             V1Topz  = '  INSERIMENTO  ';
062500091113           when  TBLflg = '*';
062600091117             V1Topz  = '  RIPRISTINO   ';
062700091117             $Annull = *on;
062800091113           other;
062900091117             V1Topz  = '   MODIFICA    ';
063000091113         endsl;
063100091113
063200091113         // -?Impostazione dettaglio?
063300091116         clear  TB35D2;
063400091113
063500091113         clear  $Lin;
063600091113         clear  $Des;
063700091113         clear  $Term;
063800091113         clear  xx;
063900091113         clear  yy;
064000091113         clear  ww;
064100091113
064200091113         IF  %found(TABEL00F);
064300091113
064400091113           // -?Reperimento "fratellini" della tabella?
064500091113           w005a = %subst(TBLkey : 4 : 5);
064600091113           xx = %lookup( w005a : $5F_LnpNrs );
064700091113
064800091113           DoW  xx > *zero;
064900091113
065000091113             if  TBLflg = $5F_Ann(xx);
065100091113               ds5F_Fil = $5F_Uni(xx);
065200091113               exsr  sr_CarD02;
065300091117             endif;
065400091117             yy = xx + 1;
065500091117             xx = %lookup( w005a : $5F_LnpNrs : yy );
065600091113
065700091113           EndDo;
065800091113
065900091113         ENDIF;
066000091113
066100091113         // -?Abilitazione tasti funzionali?
066200091113         exsr  sr_AbilFxx;
066300091113
066400091116       ENDSR;
066500091113
066600091113       //--------------------------------------------------------------
066700091113       //?Caricamento filiali che possono effettuare la conferma.      ?
066800091113       //--------------------------------------------------------------
066900091113       BEGSR  sr_CarD02;
067000091113
067100091113         yy = 1;
067200091113
067300091113         DoW  yy <= %elem($5F_Fil)   and   $5F_Fil(yy) > *zero;
067400091113
067500091116           chain  ( %int( $5F_Fil(yy) ) )  AZORG;
067600091113
067700091113           select;
067800091113             when  NOT %found(AZORG01L);
067900091113             when  ORGde5 <> *blank;
068000091113                ww += 1;
068100091113                $Lin(ww) = $5F_Fil(yy);
068200091113                $Des(ww) = %subst( ORGde5 : 1 : %size(V2Df01) );
068300091113             other;
068400091113                ww += 1;
068500091113                $Lin(ww) = $5F_Fil(yy);
068600091113                $Des(ww) = %subst( ORGdes : 1 : %size(V2Df01) );
068700091113           endsl;
068800091113
068900091113           yy += 1;
069000091113
069100091113         EndDo;
069200091113
069300091113       ENDSR;
069400091113
069500091113       //--------------------------------------------------------------
069600091113       //?Abilitazione tasti funzionali in P02 (per D02).              ?
069700091113       //--------------------------------------------------------------
069800091113       BEGSR  sr_AbilFxx;
069900091113
070000091113         // ->?F5 = Ripristino?
070100091117         F5Attivo = (%found(TABEL00F)   and   $Annull);
070200091113
070300091113         // ->?F6 = Conferma?
070400091117         F6Attivo = ((%found(TABEL00F)   and   Not $Annull)  OR
070500091116                      Not %found(TABEL00F));
070600091113
070700091113         // ->?F12 = Ritorno?
070800091113         F12Attivo = ($Video = 'D2');
070900091113
071000091113         // ->?F16 = Annullamento?
071100091117         F16Attivo = (%found(TABEL00F)   and   Not $Annull  and
071200091113                      %subst( KNMUS : 1 : 3 ) = c_EDP);
071300091113
071400091113       ENDSR;
071500091113
071600091113       //--------------------------------------------------------------
071700091113       //?Controllo dati nella videata D02.                            ?
071800091113       //--------------------------------------------------------------
071900091113       BEGSR  sr_CtrD02;
072000091113
072100091113         clear  $Term;
072200091113
072300091113         For  yy = 1  To %elem($Term);
072400091113
072500091113           clear  $Des(yy);
072600091113
072700091113           if  $Lin(yy) <> *blank  and   $Lin(yy) <> *zero;
072800091113
072900091113             select;
073000091113               // - Richiesta ricerca
073100091113               when  %scan( '?' : $Lin(yy) ) > *zero;
073200091113                 clear  $Lin(yy);
073300091113                 clear  SD19cod;
073400091113                 clear  SD19tip;
073500091113                 clear  SD19des;
073600091113                 tnsd19r ( SD19cod : SD19tip : SD19des );
073700091113                 $Lin(yy) = SD19cod;
073800091113                 ErrGenerico = *on;
073900091113               // - Codice filiale alfanumerica
074000091116               when  %check( c_Digits : $Lin(yy) ) > *zero   or
074100091113                     $Lin(yy) < *zero;
074200091116                 V1Dmsg = $Msg(02);
074300091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
074400091113                 ErrMessage  = *on;
074500091113                 ErrGenerico = *on;
074600091113                 leavesr;
074700091113             endsl;
074800091113
074900091116             chain  ( %int( $Lin(yy) ) )  AZORG;
075000091113
075100091113             select;
075200091113               // - Filiale non trovata
075300091113               when  NOT %found(AZORG01L);
075400091116                 V1Dmsg = $Msg(02);
075500091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
075600091113                 ErrMessage  = *on;
075700091113                 ErrGenerico = *on;
075800091113                 leavesr;
075900091113               // - Filiale annullata
076000091113               when  ORGfva <> *blank;
076100091116                 V1Dmsg = $Msg(02);
076200091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
076300091113                 ErrMessage  = *on;
076400091113                 ErrGenerico = *on;
076500091113                 leavesr;
076600091113               // - Né filiale né agenzia
076700091113               when  ORGfag <> 'F'   and   ORGfag <> 'A';
076800091116                 V1Dmsg = $Msg(02);
076900091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
077000091113                 ErrMessage  = *on;
077100091113                 ErrGenerico = *on;
077200091113                 leavesr;
077300091113             endsl;
077400091113
077500091113             // - Decodifica filiale
077600091113             if  ORGde5 <> *blank;
077700091116               $Des(yy) = %subst( ORGde5 : 1 : %size(V2Df01) );
077800091113             else;
077900091116               $Des(yy) = %subst( ORGdes : 1 : %size(V2Df01) );
078000091113             endif;
078100091113
078200091113             // - La linea NON può avere lo stesso Terminal di Partenza
078300091113             //   della Linea Segnacollo.
078400091113             //   => Reperimento Terminal Partenza della linea immessa
078500091113             reset  fnlv55ds;
078600091116             D55lin = %int( $Lin(yy) );
078700091113             D55drf = wDate;
078800091113             fnlv55r ( FNLV55ds );
078900091113             if  D55tfp = wwwTFP;
079000091116               V1Dmsg = $Msg(04);
079100091113               %subst( IndDspF : (52 + yy) : 1 ) = *on;
079200091113               ErrMessage  = *on;
079300091113               ErrGenerico = *on;
079400091113               leavesr;
079500091117             else;
079600091117               $Term(yy) = D55tfp;
079700091117             endif;
079800091113
079900091113           endif;
080000091113
080100091113         EndFor;
080200091113
080300091113       ENDSR;
080400091113
080500091113       //--------------------------------------------------------------
080600091113       //?Aggiornamento record nel file TABEL00F.                      ?
080700091113       //--------------------------------------------------------------
080800091113       BEGSR  sr_UpdateAll;
080900091113
081000091113         //? N.B.                                                      ?
081100091113         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
081200091117         //   trasmissione (vedi flag TBLFTR).                        ?
081300091113         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
081400091113         //   subito nello stesso file di entrambi i S.I. - in due    ?
081500091113         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
081600091117
081700091117         $SaveLin  = $Lin;
081800091117         $SaveTerm = $Term;
081900091117         w005a = %editc(V1Clnp : 'X') + %editc(V1Cnrs : 'X');
082000091113
082100091113         // -?Ciclo di elaborazione per ogni sistema informativo?
082200091113         For  w_SisInf = 1  To  %elem($Libr);
082300091113
082400091113           // -?Apertura degli archivi?
082500091113           if  w_SisInf > 1;
082600091113             exsr  sr_OpenFileTab;
082700091117             $Lin  = $SaveLin;
082800091117             $Term = $SaveTerm;
082900091113           endif;
083000091113
083100091113           // -?Aggiorn. tab. "5F" = Segnacolli confermabili da altre?  ?
083200091113           //                       ?altre fil.?  ?
083300091117           if  dsp_aid = c_F16;
083400091117             exsr  sr_AnnTABEL;
083500091117           else;
083600091117             exsr  sr_UpdTABEL;
083700091117           endif;
083800091113
083900091113         EndFor;
084000091113
084100091113       ENDSR;
084200091113
084300091113       //--------------------------------------------------------------
084400091113       //?Aggiornamento records nel file TABEL00F.                     ?
084500091113       //--------------------------------------------------------------
084600091113       BEGSR  sr_UpdTABEL;
084700091117
084800091113         // -?Annullamento dei record già inseriti?
084900091117         //   (se NON richiesto il ripristino)
085000091130         if  dsp_aid <> c_F05;
085100091117           exsr  sr_AnnTAB_2;
085200091117         endif;
085300091113
085400091113         // -?Aggiornamento o Scrittura dei nuovi record?
085500091113         evalr k_TBLkey = w005a;
085600091113
085700091113         chain  %kds(k03tabel00)  TABEL;
085800091113
085900091113         if  NOT  %found(TABEL00F);
086000091113           clear  TABEL;
086100091113           TBLcod = c_Tab;
086200091113           TBLkut = c_Kut;
086300091113           TBLkey = k_TBLkey;
086400091113           TBLftt = '1';
086500091113         endif;
086600091113
086700091113         clear  $5F_Fil;
086800091113         clear  xx;
086900091113
087000091113         For  yy = 1  To  %elem($Term);
087100091113
087200091113           if  $Term(yy) > *zero   and
087300091113               %lookup( %editc( $Term(yy) : 'X' ) : $5F_Fil ) = *zero;
087400091113             xx += 1;
087500091116             $5F_Fil(xx) = %editc( $Term(yy) : 'X' );
087600091113           endif;
087700091113
087800091113         EndFor;
087900091113
088000091113         TBLuni = ds5F_Fil;
088100091113
088200091113         clear  TBLflg;
088300091117         if  dsp_aid = c_F05;
088400091117           clear  TBLftr;
088500091117           clear  TBLdtr;
088600091117         else;
088700091117           if  w_SisInf = 1;
088800091117             TBLftr = 'T';
088900091117           else;
089000091117             TBLftr = 'R';
089100091117           endif;
089200091117           TBLdtr = %int( %subst( %editc(wDate : 'X') :
089300091117                                  %len(wDate) - %len(TBLdtr) + 1 :
089400091117                                  %len(TBLdtr) ) );
089500091117         endif;
089600091113
089700091113         if  %found(TABEL00F);
089800091113           //____________
089900091113           update  TABEL;
090000091113           //¯¯¯¯¯¯¯¯¯¯¯¯
090100091113         else;
090200091113           //___________
090300091113           write  TABEL;
090400091113           //¯¯¯¯¯¯¯¯¯¯¯
090500091113         endif;
090600091113
090700091113         // -
090800091113         // -?Scrittura dei record con TBLKEY=TermPar+Lnp+Nrs e?
090900091113         //                           ?TBLUNI=Fil. che conferma?
091000091113
091100091113         For  yy = 1  To  %elem($Term);
091200091113
091300091113           If  $Lin(yy) > *zero;
091400091113
091500091116             k_TBLkey = %editc($Term(yy) : 'X') + w005a;
091600091113
091700091113             chain  %kds(k03tabel00)  TABEL;
091800091113
091900091113             if  NOT  %found(TABEL00F);
092000091113               clear  TABEL;
092100091113               TBLcod = c_Tab;
092200091113               TBLkut = c_Kut;
092300091113               TBLkey = k_TBLkey;
092400091113               TBLftt = '1';
092500091113             endif;
092600091113
092700091113             clear  TBLflg;
092800091117             if  dsp_aid = c_F05;
092900091117               clear  TBLftr;
093000091117               clear  TBLdtr;
093100091117             else;
093200091117               if  w_SisInf = 1;
093300091117                 TBLftr = 'T';
093400091117               else;
093500091117                 TBLftr = 'R';
093600091117               endif;
093700091117               TBLdtr = %int( %subst( %editc(wDate : 'X') :
093800091117                                      %len(wDate) - %len(TBLdtr) + 1 :
093900091117                                      %len(TBLdtr) ) );
094000091117             endif;
094100091113
094200091113             // - Memorizzazione delle fil. di quel Terminal
094300091113             clear  $5F_Fil;
094400091113             $5F_Fil(01) = $Lin(yy);
094500091113             ww = 1;
094600091113             xx = yy + 1;
094700091113
094800091113             if  xx <= %elem($Term);
094900091116               xx = %lookup( $Term(yy) : $Term : xx );
095000091113             endif;
095100091113
095200091116             DoW  xx > *zero  and  xx <= %elem($Term);
095300091113
095400091113               ww += 1;
095500091113               $5F_Fil(ww) = $Lin(xx);
095600091113               clear  $Lin(xx);
095700091113               clear  $Term(xx);
095800091113               xx += 1;
095900091113               if  xx <= %elem($Term);
096000091116                 xx = %lookup( $Term(yy) : $Term : xx );
096100091113               endif;
096200091113
096300091113             EndDo;
096400091113
096500091113             TBLuni = ds5F_Fil;
096600091113
096700091113             if  %found(TABEL00F);
096800091113               //____________
096900091113               update  TABEL;
097000091113               //¯¯¯¯¯¯¯¯¯¯¯¯
097100091113             else;
097200091113               //___________
097300091113               write  TABEL;
097400091113               //¯¯¯¯¯¯¯¯¯¯¯
097500091113             endif;
097600091113
097700091113           EndIf;
097800091113
097900091113         EndFor;
098000091113
098100091113       ENDSR;
098200091113
098300091113       //--------------------------------------------------------------
098400091113       //?Annullamento 2° tipo rec. (TER/FLS/NRS) nella tab. "5F".     ?
098500091113       //--------------------------------------------------------------
098600091113       BEGSR  sr_AnnTAB_2;
098700091113
098800091113         xx = %lookup( w005a : $5F_LnpNrs );
098900091113
099000091113         DoW  xx > *zero;
099100091113
099200091116           k_TBLkey = $5F_Term(xx) + $5F_LnpNrs(xx);
099300091113           chain  %kds(k03tabel00)  TABEL;
099400091113           if  %found(TABEL00F);
099500091113             TBLflg = '*';
099600091113             clear  TBLftr;
099700091113             clear  TBLdtr;
099800091113             //____________
099900091113             update  TABEL;
100000091113             //¯¯¯¯¯¯¯¯¯¯¯¯
100100091113           endif;
100200091113
100300091113           yy = xx + 1;
100400091116           xx = %lookup( w005a : $5F_LnpNrs : yy );
100500091113
100600091113         EndDo;
100700091113
100800091113       ENDSR;
100900091113
101000091113       //--------------------------------------------------------------
101100091113       //?Annullamento records nella tabella "5F".                     ?
101200091113       //--------------------------------------------------------------
101300091113       BEGSR  sr_AnnTABEL;
101400091113
101500091117         //exsr  sr_UpdTABEL;
101600091117         //exsr  sr_CarTAB;
101700091113
101800091113         // -?Annullamento 2° tipo rec. (TER/FLS/NRS) nella tab. "5F"?
101900091113         exsr  sr_AnnTAB_2;
102000091113
102100091113         // -?Annullamento 1° tipo rec. (___/FLS/NRS) nella tab. "5F"?
102200091117         evalr k_TBLkey = w005a;
102300091113
102400091113         chain  %kds(k03tabel00)  TABEL;
102500091113
102600091113         if  %found(TABEL00F);
102700091113           TBLflg = '*';
102800091113           clear  TBLftr;
102900091113           clear  TBLdtr;
103000091113           //____________
103100091113           update  TABEL;
103200091113           //¯¯¯¯¯¯¯¯¯¯¯¯
103300091113         endif;
103400091113
103500091113       ENDSR;
103600091113
103700091113       //--------------------------------------------------------------
103800091113       //?Operazioni finali.                                           ?
103900091113       //--------------------------------------------------------------
104000091113       BEGSR  sr_RoutEnd;
104100091113
104200091113         return;
104300091113
104400091113       ENDSR;
104500091113
104600091113      /end-free
104700091113
104800091113       //--------------------------------------------------------------
104900091113       //?Schiere a tempo di compilazione.                             ?
105000091113       //--------------------------------------------------------------
105100091116
105200091116** - $iSystem / $SysInf:?Sistemi AS/400 & Librerie con entrambi i file?
105300091127SETRAS  GAITRAGRU FILTRAGRU
105400091116AS888   GAITRAGRPSFILTRAGRPF
105500091113** - $Msg:?Messaggi di Errore?-----------------------------------------------*
105600091113Linea di partenza obbligatoria                                                 1
105700091113Linea di partenza inesistente                                                  2
105800091113Immettere un numero di serie maggiore di 00                                    3
105900091113Linea immessa non deve avere lo stesso terminal Partenza della linea segnacoll 4
