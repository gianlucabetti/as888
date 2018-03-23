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
004400160308     d c_MaxFil        c                   const(18)
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
009800140626     d $5F_Term        s              3    dim(5000)     inz
009900091113       // - Lnp + Nrs in TBLKEY
010000140626     d $5F_LnpNrs      s              5    dim(5000)     inz
010100091113       // - Filiali in TBLUNI
010200140626     d $5F_Uni         s             89    dim(5000)     inz
010300091113       // - Flags Annullamento (TBLFLG)
010400140626     d $5F_Ann         s              1    dim(5000)     inz
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
017800160308     d   PosCurF12                     n   overlay(IndDspF : 64)
017900160308     d   PosCurF13                     n   overlay(IndDspF : 65)
018000160308     d   PosCurF14                     n   overlay(IndDspF : 66)
018100160308     d   PosCurF15                     n   overlay(IndDspF : 67)
018200160308     d   PosCurF16                     n   overlay(IndDspF : 68)
018300160308     d   PosCurF17                     n   overlay(IndDspF : 69)
018400160308     d   PosCurF18                     n   overlay(IndDspF : 70)
018500091113         //   - Riemissione videata
018600091113     d   ErrGenerico                   n   overlay(IndDspF : 99)
018700091113
018800091113       // - Parametri ricevuti
018900091113     d KPJBA         e ds
019000091113
019100091113       // - Parametri per Reperimento dati utente
019200091113     d TIBS34ds      e ds                  inz
019300091113
019400091113       // - Parametri per Ricerca terminal di Partenza/Arrivo
019500091113     d FNLV55ds      e ds                  inz
019600091113     d   D55tpt      e                     inz('P')
019700091113
019800091113       // - Strutture di comodo per il raggruppamento dei campi a video
019900091113     d ds_V2Cfil       ds                  inz
020000091113     d  V2Cf01                             inz
020100091113     d  V2Cf02                             inz
020200091113     d  V2Cf03                             inz
020300091113     d  V2Cf04                             inz
020400091113     d  V2Cf05                             inz
020500091113     d  V2Cf06                             inz
020600091113     d  V2Cf07                             inz
020700091113     d  V2Cf08                             inz
020800091113     d  V2Cf09                             inz
020900091113     d  V2Cf10                             inz
021000091113     d  V2Cf11                             inz
021100160308     d  V2Cf12                             inz
021200160308     d  V2Cf13                             inz
021300160308     d  V2Cf14                             inz
021400160308     d  V2Cf15                             inz
021500160308     d  V2Cf16                             inz
021600160308     d  V2Cf17                             inz
021700160308     d  V2Cf18                             inz
021800160308     d   $Lin                  1     54    dim(c_MaxFil)  inz
021900091113     d ds_V2Dfil       ds                  inz
022000091113     d  V2Df01                             inz
022100091113     d  V2Df02                             inz
022200091113     d  V2Df03                             inz
022300091113     d  V2Df04                             inz
022400091113     d  V2Df05                             inz
022500091113     d  V2Df06                             inz
022600091113     d  V2Df07                             inz
022700091113     d  V2Df08                             inz
022800091113     d  V2Df09                             inz
022900091113     d  V2Df10                             inz
023000091113     d  V2Df11                             inz
023100160308     d  V2Df12                             inz
023200160308     d  V2Df13                             inz
023300160308     d  V2Df14                             inz
023400160308     d  V2Df15                             inz
023500160308     d  V2Df16                             inz
023600160308     d  V2Df17                             inz
023700160308     d  V2Df18                             inz
023800160308     d   $Des                  1    180    dim(c_MaxFil)  inz
023900091113
024000091113       // - Parametri per Controllo/Inversione date
024100091113     d WLBdat          ds                  inz
024200091113     d  G02dat                        8  0 inz
024300091113     d  G02inv                        8  0 inz
024400091113     d  G02err                        1    inz
024500091113     d  G02tgi                        5  0 inz
024600091113
024700091113       //--------------------------------------------------------------
024800091113       //?Definizione variabili globali.                               ?
024900091113       //--------------------------------------------------------------
025000091113
025100091113       // - Flags booleani
025200091113     d $Fine           s               n   inz
025300091113     d $InzD01         s               n   inz(*on)
025400091113     d $InzD02         s               n   inz(*on)
025500091117     d $Annull         s               n   inz
025600091113
025700091113       // - Variabili per la gestione del video
025800091113     d $Video          s              2    inz('D1')
025900091116
026000091116       // - Indici di schiera
026100091116     d ww              s              5  0 inz
026200091116     d xx              s              5  0 inz
026300091116     d yy              s              5  0 inz
026400091113
026500091113       // - Nome del sistema
026600091113     d currSysNeta     s              8a   inz
026700091113
026800091113       // - Nome esteso Libreria/File dei 2 file tabella
026900091113     d wLibFile        s             21a   inz
027000091113
027100091113       // - Terminal di Partenza
027200091113     d wwwTFP          s                   like(D55tfp)
027300091113
027400091113       // - Parametri per TNSD19R
027500091113     d SD19cod         s              3    inz
027600091113     d SD19tip         s              1    inz
027700091113     d SD19des         s             25    inz
027800091113
027900091113       // - Campi di comodo
028000091113     d wDate_EUR       s               d   datfmt(*eur)  inz
028100091113     d wDate           s              8  0 inz
028200091113     d w_iSystem       s              1  0 inz
028300091113     d w_SisInf        s              3  0 inz
028400091113     d w005a           s              5    inz
028500091113
028600091113       //--------------------------------------------------------------
028700091113       //?Definizione prototipi procedure.                             ?
028800091113       //--------------------------------------------------------------
028900091113
029000091113       // - Reperimento dati utente
029100091113      /copy gaitrasrc/srcProtoPR,TIBS34R
029200091113
029300091113       // - Controllo Terminal
029400091113      /copy gaitrasrc/srcProtoPR,FNLV55R
029500091113
029600091113       // - Ricerca codici Organigramma
029700091113     d tnsd19r         pr                  extpgm('TNSD19R')
029800091113     d  D19cod                             like(SD19cod)
029900091113     d  D19tip                             like(SD19tip)
030000091113     d  D19des                             like(SD19des)
030100091116
030200091116       // - Reperimento NETA sistema AS/400 corrente
030300091116     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
030400091113
030500091113       //--------------------------------------------------------------
030600091113       //?Definizione key-list.                                        ?
030700091113       //--------------------------------------------------------------
030800091113
030900091113       // - File TABEL00F
031000091113     d k03tabel00    e ds                  extname(TABEL00F : *key)
031100091113     d                                     prefix(k_)   inz
031200091113
031300091113       //--------------------------------------------------------------
031400091113       //?M A I N - L I N E                                            ?
031500091113       //--------------------------------------------------------------
031600091113
031700091113     c     *Entry        plist
031800091113     c                   parm                    KPJBA
031900091113
032000091113      /free
032100091113
032200091113       // -?Operazioni iniziali?
032300091113       exsr sr_RoutInz;
032400091113
032500091113       // -?Gestione videate?
032600091113       DoW  $Fine = *off;
032700091113
032800091113         select;
032900091113
033000091113           // - Gestione videata D1
033100091113           when $Video = 'D1';
033200091113             exsr sr_GesD01;
033300091113
033400091113           // - Gestione videata D2
033500091113           when $Video = 'D2';
033600091113             exsr sr_GesD02;
033700091113
033800091113           other;
033900091113             $Fine = *on;
034000091113
034100091113         endsl;
034200091113
034300091113       EndDo;
034400091113
034500091113       // -?Operazioni finali?
034600091113       exsr sr_RoutEnd;
034700091113
034800091113       //--------------------------------------------------------------
034900091113       //?Operazioni iniziali.                                         ?
035000091113       //--------------------------------------------------------------
035100091113       BEGSR  sr_RoutInz;
035200091113
035300091113         *inLR = *on;
035400091113
035500091113         // -?Verifica del sistema AS/400 corrente?
035600091113         if  ubRtvNetA_Rtv(currSysNeta) <> *zero;
035700091113           $Fine = *on;
035800091113           leavesr;
035900091113         endif;
036000091113
036100091113         // -?Impostazione elenco librerie in cui gestire le tabelle?
036200091113         //  ?(a seconda del sistema in cui si stà lavorando)?
036300091113         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
036400091113         if  w_iSystem = *zero;
036500091113           $Fine = *on;
036600091113           leavesr;
036700091113         endif;
036800091113
036900091113         // -?Reperimento dati job?
037000091113         exsr  sr_DatiJob;
037100091113
037200091113         // -?Impostazione nome programma a video?
037300091113         V1Tpgm = SDSpgm;
037400091113
037500091113         // -?Impostazione campi "fissi"?
037600091113         k_TBLkut = c_Kut;
037700091113         k_TBLcod = c_Tab;
037800091113
037900091113         // -?Reperimento data odierna?
038000091113         wDate = %int( %subst( %char( %dec( %timestamp() ) )
038100091113                               : 1 : 8 ) );
038200091113
038300091113       ENDSR;
038400091113
038500091113       //--------------------------------------------------------------
038600091113       //?Reperimento Dati del job (Utente/Operativi).                 ?
038700091113       //--------------------------------------------------------------
038800091113       BEGSR  sr_DatiJob;
038900091113
039000091113         in(e) §AzUte;
039100091113         if NOT %error;
039200091113           in(e) §DatiUte;
039300091113         endif;
039400091113         if %error or RSut = *blank;
039500091113           tibs34r ( tibs34ds );
039600091113           in §AzUte;
039700091113           in §DatiUte;
039800091113         endif;
039900091113
040000091113       ENDSR;
040100091113
040200091113       //--------------------------------------------------------------
040300091113       //?Gestione videata D01.                                        ?
040400091113       //--------------------------------------------------------------
040500091113       BEGSR  sr_GesD01;
040600091113
040700091113         // -?Inizializzazione videata?
040800091113         if  $InzD01 = *on;
040900091116           clear  TB35D1;
041000091113           exsr  sr_CarTAB;
041100091113           $InzD01 = *off;
041200091113         endif;
041300091113
041400091116         // -?Emissione Testata e Piede?
041500091116         write  TB35T1;
041600091116         write  TB35P1;
041700091113
041800091113         // -?Emissione videata?
041900091116         exfmt  TB35D1;
042000091113
042100091116         clear  V1Dmsg;
042200091113         reset  ErrMessage;
042300091113         reset  ErrGenerico;
042400091113
042500091113         SELECT;
042600091113
042700091113           // -?F3=Fine?
042800091113           WHEN  dsp_aid = c_F03;
042900091113             $Fine = *on;
043000091113
043100091113           // -?Invio?
043200091113           OTHER;
043300091113             exsr sr_CtrD01;
043400091113             if  ErrGenerico = *off;
043500091113               $Video    = 'D2';
043600091113               $InzD02   = *on;
043700091113             endif;
043800091113
043900091113         ENDSL;
044000091113
044100091113       ENDSR;
044200091113
044300091113       //--------------------------------------------------------------
044400091113       //?Controllo dati immessi nella videata D01.                    ?
044500091113       //--------------------------------------------------------------
044600091113       BEGSR  sr_CtrD01;
044700091113
044800091113         %subst(IndDspF : 50) = *off;
044900091113
045000091113         // -?Linea di Partenza Segnacollo?
045100091113         clear  V1Dlnp;
045200091113         if  V1Clnp = *zero;
045300091116           V1Dmsg = $Msg(01);
045400091113           PosCurLNP   = *on;
045500091113           ErrMessage  = *on;
045600091113           ErrGenerico = *on;
045700091113           leavesr;
045800091113         endif;
045900091113         chain  (V1Clnp)  AZORG;
046000091116         if  %found(AZORG01L);
046100091113           V1Dlnp = ORGdes;
046200091113         endif;
046300091113         if  NOT %found(AZORG01L)   or
046400091113             ORGfva <> *blank       or
046500091113            (ORGfag <> 'A'      and
046600091113             ORGfag <> 'F');
046700091116           V1Dmsg = $Msg(02);
046800091113           PosCurLNP   = *on;
046900091113           ErrMessage  = *on;
047000091113           ErrGenerico = *on;
047100091113           leavesr;
047200091113         endif;
047300091113
047400091113         // -?Reperimento Terminal Partenza Linea Segnacollo?
047500091113         reset  FNLV55ds;
047600091113         D55lin = V1Clnp;
047700091113         D55drf = wDate;
047800091113         fnlv55r ( FNLV55ds );
047900091113         if  D55err = *blank;
048000091113           wwwTFP = D55tfp;
048100091113         else;
048200091113           wwwTFP = V1Clnp;
048300091113         endif;
048400091113
048500091113         // -?Serie Segnacollo?
048600091113         if  V1Cnrs = *zero;
048700091116           V1Dmsg = $Msg(03);
048800091113           PosCurNRS   = *on;
048900091113           ErrMessage  = *on;
049000091113           ErrGenerico = *on;
049100091113           leavesr;
049200091113         endif;
049300091113
049400091113       ENDSR;
049500091113
049600091113       //--------------------------------------------------------------
049700091113       //?Caricamento tabelle con terminal in KEY                      ?
049800091113       //--------------------------------------------------------------
049900091113       BEGSR  sr_CarTAB;
050000091113
050100091113         // -?Apertura file TABEL00F del 1° S.I. (sede)?
050200091113         w_SisInf = 1;
050300091113         exsr  sr_OpenFileTab;
050400091113
050500091113         clear  $5F_Term;
050600091113         clear  $5F_LnpNrs;
050700091113         clear  $5F_Uni;
050800091113         clear  $5F_Ann;
050900091113         clear  xx;
051000091113
051100091113         // - Tabelle separate per KEY SENZA terminal
051200091113         //                      e KEY CON   terminal
051300091113         //   => si elaborano SOLO quelle CON terminal
051400091113         k_TBLkey = '0010000';
051500091113
051600091113         // - Caricamento record validi dalla tabella
051700091113         setll    %kds(k03tabel00)      TABEL;
051800091113         reade(n) %kds(k03tabel00 : 2)  TABEL;
051900091113
052000091113         DoW  NOT %eof(TABEL00F);
052100091113
052200091113           if  %subst(TBLkey : 1 : 3) <> *blank;
052300091113             xx += 1;
052400091113             $5F_Term(xx)   = %subst( TBLkey : 1 : 3 );
052500091113             $5F_LnpNrs(xx) = %subst( TBLkey : 4 : 5 );
052600091113             $5F_Uni(xx) = TBLuni;
052700091116             $5F_Ann(xx) = TBLflg;
052800091113           endif;
052900091113
053000091113           reade(n) %kds(k03tabel00 : 2)  TABEL;
053100091113
053200091113         EndDo;
053300091113
053400091113       ENDSR;
053500091113
053600091113       //--------------------------------------------------------------
053700091113       //?Apertura dei files tabelle nel sistema informativo impostato.?
053800091113       //--------------------------------------------------------------
053900091113       BEGSR  sr_OpenFileTab;
054000091113
054100091113         // -?Chiusura (eventuale) archivio?
054200091113         if  %open(TABEL00F);
054300091113           close  TABEL00F;
054400091113         endif;
054500091113
054600091113         // -?Apertura archivio?
054700091116         ds_Libr  = $SisInf(w_iSystem);
054800091113         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
054900091113         open  TABEL00F;
055000091113
055100091113       ENDSR;
055200091113
055300091113       //--------------------------------------------------------------
055400091113       //?Gestione videata D02.                                        ?
055500091113       //--------------------------------------------------------------
055600091113       BEGSR  sr_GesD02;
055700091113
055800091113         // -?Inizializzazione videata?
055900091113         if  $InzD02 = *on;
056000091113           exsr  sr_InzD02;
056100091113           $InzD02 = *off;
056200091113         endif;
056300091113
056400091113         // -?Emissione Testata e Piede?
056500091116         if  Not ErrMessage;
056600091116           write  TB35T1;
056700091116           write  TB35D1;
056800091116           write  Protect;
056900091116           write  TB35P2;
057000091116         endif;
057100091113
057200091113         // -?Emissione videata?
057300091117         if  Not $Annull;
057400091113           exfmt  TB35D2;
057500091113         else;
057600091113           write  TB35D2;
057700091113           exfmt  Protect;
057800091113         endif;
057900091113
058000091113         reset  ErrMessage;
058100091113         reset  ErrGenerico;
058200091116         clear  V1Dmsg;
058300091113
058400091113         SELECT;
058500091113
058600091113           // -?F3=Fine?
058700091113           WHEN  dsp_aid = c_F03;
058800091113             unlock  TABEL00F;
058900091113             $Fine = *on;
059000091113
059100091113           // -?F12=Ritorno?
059200091113           WHEN  dsp_aid = c_F12;
059300091113             unlock  TABEL00F;
059400091113             $Video = 'D1';
059500091113
059600091113           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
059700091113           OTHER;
059800091113             // - Controlli eseguiti NON se F16=Annullamento
059900091113             if  dsp_aid <> c_F16;
060000091113               exsr sr_CtrD02;
060100091113               if  ErrGenerico = *on;
060200091113                 leavesr;
060300091113               endif;
060400091113             endif;
060500091113             // - Aggiornamento
060600091113             if  dsp_aid = c_F05   or
060700091113                 dsp_aid = c_F06   or
060800091113                 dsp_aid = c_F16;
060900091113               exsr  sr_UpdateAll;
061000091113               if  ErrGenerico = *on;
061100091113                 leavesr;
061200091113               endif;
061300091113               $Video  = 'D1';
061400091113               $InzD01 = *on;
061500091113             endif;
061600091113
061700091113         ENDSL;
061800091113
061900091113         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
062000091113         if  $Video = 'D1';
062100091113           clear  V1Topz;
062200091113         endif;
062300091113
062400091113       ENDSR;
062500091113
062600091113       //--------------------------------------------------------------
062700091113       //?Inizializzazione dati nella videata D02.                     ?
062800091113       //--------------------------------------------------------------
062900091113       BEGSR  sr_InzD02;
063000091113
063100091117         reset  $Annull;
063200091117
063300091113         // -?Verifica esistenza del record in TABEL?
063400091113         //k_TBLkey = '   '
063500091113         //         + %editc(V1Clnp : 'X')
063600091113         //         + %editc(V1Cnrs : 'X');
063700091113         evalr k_TBLkey = %editc(V1Clnp : 'X')
063800091113                        + %editc(V1Cnrs : 'X');
063900091113         chain  %kds(k03tabel00)  TABEL;
064000091113
064100091113         // -?Impostazione testata?
064200091113         clear  V1Topz;
064300091113         select;
064400091113           when  Not %found(TABEL00F);
064500091117             V1Topz  = '  INSERIMENTO  ';
064600091113           when  TBLflg = '*';
064700091117             V1Topz  = '  RIPRISTINO   ';
064800091117             $Annull = *on;
064900091113           other;
065000091117             V1Topz  = '   MODIFICA    ';
065100091113         endsl;
065200091113
065300091113         // -?Impostazione dettaglio?
065400091116         clear  TB35D2;
065500091113
065600091113         clear  $Lin;
065700091113         clear  $Des;
065800091113         clear  $Term;
065900091113         clear  xx;
066000091113         clear  yy;
066100091113         clear  ww;
066200091113
066300091113         IF  %found(TABEL00F);
066400091113
066500091113           // -?Reperimento "fratellini" della tabella?
066600091113           w005a = %subst(TBLkey : 4 : 5);
066700091113           xx = %lookup( w005a : $5F_LnpNrs );
066800091113
066900091113           DoW  xx > *zero;
067000091113
067100091113             if  TBLflg = $5F_Ann(xx);
067200091113               ds5F_Fil = $5F_Uni(xx);
067300091113               exsr  sr_CarD02;
067400091117             endif;
067500091117             yy = xx + 1;
067600091117             xx = %lookup( w005a : $5F_LnpNrs : yy );
067700091113
067800091113           EndDo;
067900091113
068000091113         ENDIF;
068100091113
068200091113         // -?Abilitazione tasti funzionali?
068300091113         exsr  sr_AbilFxx;
068400091113
068500091116       ENDSR;
068600091113
068700091113       //--------------------------------------------------------------
068800091113       //?Caricamento filiali che possono effettuare la conferma.      ?
068900091113       //--------------------------------------------------------------
069000091113       BEGSR  sr_CarD02;
069100091113
069200091113         yy = 1;
069300091113
069400091113         DoW  yy <= %elem($5F_Fil)   and   $5F_Fil(yy) > *zero;
069500091113
069600091116           chain  ( %int( $5F_Fil(yy) ) )  AZORG;
069700091113
069800091113           select;
069900091113             when  NOT %found(AZORG01L);
070000091113             when  ORGde5 <> *blank;
070100091113                ww += 1;
070200091113                $Lin(ww) = $5F_Fil(yy);
070300091113                $Des(ww) = %subst( ORGde5 : 1 : %size(V2Df01) );
070400091113             other;
070500091113                ww += 1;
070600091113                $Lin(ww) = $5F_Fil(yy);
070700091113                $Des(ww) = %subst( ORGdes : 1 : %size(V2Df01) );
070800091113           endsl;
070900091113
071000091113           yy += 1;
071100091113
071200091113         EndDo;
071300091113
071400091113       ENDSR;
071500091113
071600091113       //--------------------------------------------------------------
071700091113       //?Abilitazione tasti funzionali in P02 (per D02).              ?
071800091113       //--------------------------------------------------------------
071900091113       BEGSR  sr_AbilFxx;
072000091113
072100091113         // ->?F5 = Ripristino?
072200091117         F5Attivo = (%found(TABEL00F)   and   $Annull);
072300091113
072400091113         // ->?F6 = Conferma?
072500091117         F6Attivo = ((%found(TABEL00F)   and   Not $Annull)  OR
072600091116                      Not %found(TABEL00F));
072700091113
072800091113         // ->?F12 = Ritorno?
072900091113         F12Attivo = ($Video = 'D2');
073000091113
073100091113         // ->?F16 = Annullamento?
073200091117         F16Attivo = (%found(TABEL00F)   and   Not $Annull  and
073300091113                      %subst( KNMUS : 1 : 3 ) = c_EDP);
073400091113
073500091113       ENDSR;
073600091113
073700091113       //--------------------------------------------------------------
073800091113       //?Controllo dati nella videata D02.                            ?
073900091113       //--------------------------------------------------------------
074000091113       BEGSR  sr_CtrD02;
074100091113
074200091113         clear  $Term;
074300091113
074400091113         For  yy = 1  To %elem($Term);
074500091113
074600091113           clear  $Des(yy);
074700091113
074800091113           if  $Lin(yy) <> *blank  and   $Lin(yy) <> *zero;
074900091113
075000091113             select;
075100091113               // - Richiesta ricerca
075200091113               when  %scan( '?' : $Lin(yy) ) > *zero;
075300091113                 clear  $Lin(yy);
075400091113                 clear  SD19cod;
075500091113                 clear  SD19tip;
075600091113                 clear  SD19des;
075700091113                 tnsd19r ( SD19cod : SD19tip : SD19des );
075800091113                 $Lin(yy) = SD19cod;
075900091113                 ErrGenerico = *on;
076000091113               // - Codice filiale alfanumerica
076100091116               when  %check( c_Digits : $Lin(yy) ) > *zero   or
076200091113                     $Lin(yy) < *zero;
076300091116                 V1Dmsg = $Msg(02);
076400091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
076500091113                 ErrMessage  = *on;
076600091113                 ErrGenerico = *on;
076700091113                 leavesr;
076800091113             endsl;
076900091113
077000091116             chain  ( %int( $Lin(yy) ) )  AZORG;
077100091113
077200091113             select;
077300091113               // - Filiale non trovata
077400091113               when  NOT %found(AZORG01L);
077500091116                 V1Dmsg = $Msg(02);
077600091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
077700091113                 ErrMessage  = *on;
077800091113                 ErrGenerico = *on;
077900091113                 leavesr;
078000091113               // - Filiale annullata
078100091113               when  ORGfva <> *blank;
078200091116                 V1Dmsg = $Msg(02);
078300091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
078400091113                 ErrMessage  = *on;
078500091113                 ErrGenerico = *on;
078600091113                 leavesr;
078700091113               // - Né filiale né agenzia
078800091113               when  ORGfag <> 'F'   and   ORGfag <> 'A';
078900091116                 V1Dmsg = $Msg(02);
079000091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
079100091113                 ErrMessage  = *on;
079200091113                 ErrGenerico = *on;
079300091113                 leavesr;
079400091113             endsl;
079500091113
079600091113             // - Decodifica filiale
079700091113             if  ORGde5 <> *blank;
079800091116               $Des(yy) = %subst( ORGde5 : 1 : %size(V2Df01) );
079900091113             else;
080000091116               $Des(yy) = %subst( ORGdes : 1 : %size(V2Df01) );
080100091113             endif;
080200091113
080300091113             // - La linea NON può avere lo stesso Terminal di Partenza
080400091113             //   della Linea Segnacollo.
080500091113             //   => Reperimento Terminal Partenza della linea immessa
080600091113             reset  fnlv55ds;
080700091116             D55lin = %int( $Lin(yy) );
080800091113             D55drf = wDate;
080900091113             fnlv55r ( FNLV55ds );
081000091113             if  D55tfp = wwwTFP;
081100091116               V1Dmsg = $Msg(04);
081200091113               %subst( IndDspF : (52 + yy) : 1 ) = *on;
081300091113               ErrMessage  = *on;
081400091113               ErrGenerico = *on;
081500091113               leavesr;
081600091117             else;
081700091117               $Term(yy) = D55tfp;
081800091117             endif;
081900091113
082000091113           endif;
082100091113
082200091113         EndFor;
082300091113
082400091113       ENDSR;
082500091113
082600091113       //--------------------------------------------------------------
082700091113       //?Aggiornamento record nel file TABEL00F.                      ?
082800091113       //--------------------------------------------------------------
082900091113       BEGSR  sr_UpdateAll;
083000091113
083100091113         //? N.B.                                                      ?
083200091113         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
083300091117         //   trasmissione (vedi flag TBLFTR).                        ?
083400091113         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
083500091113         //   subito nello stesso file di entrambi i S.I. - in due    ?
083600091113         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
083700091117
083800091117         $SaveLin  = $Lin;
083900091117         $SaveTerm = $Term;
084000091117         w005a = %editc(V1Clnp : 'X') + %editc(V1Cnrs : 'X');
084100091113
084200091113         // -?Ciclo di elaborazione per ogni sistema informativo?
084300091113         For  w_SisInf = 1  To  %elem($Libr);
084400091113
084500091113           // -?Apertura degli archivi?
084600091113           if  w_SisInf > 1;
084700091113             exsr  sr_OpenFileTab;
084800091117             $Lin  = $SaveLin;
084900091117             $Term = $SaveTerm;
085000091113           endif;
085100091113
085200091113           // -?Aggiorn. tab. "5F" = Segnacolli confermabili da altre?  ?
085300091113           //                       ?altre fil.?  ?
085400091117           if  dsp_aid = c_F16;
085500091117             exsr  sr_AnnTABEL;
085600091117           else;
085700091117             exsr  sr_UpdTABEL;
085800091117           endif;
085900091113
086000091113         EndFor;
086100091113
086200091113       ENDSR;
086300091113
086400091113       //--------------------------------------------------------------
086500091113       //?Aggiornamento records nel file TABEL00F.                     ?
086600091113       //--------------------------------------------------------------
086700091113       BEGSR  sr_UpdTABEL;
086800091117
086900091113         // -?Annullamento dei record già inseriti?
087000091117         //   (se NON richiesto il ripristino)
087100091130         if  dsp_aid <> c_F05;
087200091117           exsr  sr_AnnTAB_2;
087300091117         endif;
087400091113
087500091113         // -?Aggiornamento o Scrittura dei nuovi record?
087600091113         evalr k_TBLkey = w005a;
087700091113
087800091113         chain  %kds(k03tabel00)  TABEL;
087900091113
088000091113         if  NOT  %found(TABEL00F);
088100091113           clear  TABEL;
088200091113           TBLcod = c_Tab;
088300091113           TBLkut = c_Kut;
088400091113           TBLkey = k_TBLkey;
088500091113           TBLftt = '1';
088600091113         endif;
088700091113
088800091113         clear  $5F_Fil;
088900091113         clear  xx;
089000091113
089100091113         For  yy = 1  To  %elem($Term);
089200091113
089300091113           if  $Term(yy) > *zero   and
089400091113               %lookup( %editc( $Term(yy) : 'X' ) : $5F_Fil ) = *zero;
089500091113             xx += 1;
089600091116             $5F_Fil(xx) = %editc( $Term(yy) : 'X' );
089700091113           endif;
089800091113
089900091113         EndFor;
090000091113
090100091113         TBLuni = ds5F_Fil;
090200091113
090300091113         clear  TBLflg;
090400091117         if  dsp_aid = c_F05;
090500091117           clear  TBLftr;
090600091117           clear  TBLdtr;
090700091117         else;
090800091117           if  w_SisInf = 1;
090900091117             TBLftr = 'T';
091000091117           else;
091100091117             TBLftr = 'R';
091200091117           endif;
091300091117           TBLdtr = %int( %subst( %editc(wDate : 'X') :
091400091117                                  %len(wDate) - %len(TBLdtr) + 1 :
091500091117                                  %len(TBLdtr) ) );
091600091117         endif;
091700091113
091800091113         if  %found(TABEL00F);
091900091113           //____________
092000091113           update  TABEL;
092100091113           //¯¯¯¯¯¯¯¯¯¯¯¯
092200091113         else;
092300091113           //___________
092400091113           write  TABEL;
092500091113           //¯¯¯¯¯¯¯¯¯¯¯
092600091113         endif;
092700091113
092800091113         // -
092900091113         // -?Scrittura dei record con TBLKEY=TermPar+Lnp+Nrs e?
093000091113         //                           ?TBLUNI=Fil. che conferma?
093100091113
093200091113         For  yy = 1  To  %elem($Term);
093300091113
093400091113           If  $Lin(yy) > *zero;
093500091113
093600091116             k_TBLkey = %editc($Term(yy) : 'X') + w005a;
093700091113
093800091113             chain  %kds(k03tabel00)  TABEL;
093900091113
094000091113             if  NOT  %found(TABEL00F);
094100091113               clear  TABEL;
094200091113               TBLcod = c_Tab;
094300091113               TBLkut = c_Kut;
094400091113               TBLkey = k_TBLkey;
094500091113               TBLftt = '1';
094600091113             endif;
094700091113
094800091113             clear  TBLflg;
094900091117             if  dsp_aid = c_F05;
095000091117               clear  TBLftr;
095100091117               clear  TBLdtr;
095200091117             else;
095300091117               if  w_SisInf = 1;
095400091117                 TBLftr = 'T';
095500091117               else;
095600091117                 TBLftr = 'R';
095700091117               endif;
095800091117               TBLdtr = %int( %subst( %editc(wDate : 'X') :
095900091117                                      %len(wDate) - %len(TBLdtr) + 1 :
096000091117                                      %len(TBLdtr) ) );
096100091117             endif;
096200091113
096300091113             // - Memorizzazione delle fil. di quel Terminal
096400091113             clear  $5F_Fil;
096500091113             $5F_Fil(01) = $Lin(yy);
096600091113             ww = 1;
096700091113             xx = yy + 1;
096800091113
096900091113             if  xx <= %elem($Term);
097000091116               xx = %lookup( $Term(yy) : $Term : xx );
097100091113             endif;
097200091113
097300091116             DoW  xx > *zero  and  xx <= %elem($Term);
097400091113
097500091113               ww += 1;
097600091113               $5F_Fil(ww) = $Lin(xx);
097700091113               clear  $Lin(xx);
097800091113               clear  $Term(xx);
097900091113               xx += 1;
098000091113               if  xx <= %elem($Term);
098100091116                 xx = %lookup( $Term(yy) : $Term : xx );
098200091113               endif;
098300091113
098400091113             EndDo;
098500091113
098600091113             TBLuni = ds5F_Fil;
098700091113
098800091113             if  %found(TABEL00F);
098900091113               //____________
099000091113               update  TABEL;
099100091113               //¯¯¯¯¯¯¯¯¯¯¯¯
099200091113             else;
099300091113               //___________
099400091113               write  TABEL;
099500091113               //¯¯¯¯¯¯¯¯¯¯¯
099600091113             endif;
099700091113
099800091113           EndIf;
099900091113
100000091113         EndFor;
100100091113
100200091113       ENDSR;
100300091113
100400091113       //--------------------------------------------------------------
100500091113       //?Annullamento 2° tipo rec. (TER/FLS/NRS) nella tab. "5F".     ?
100600091113       //--------------------------------------------------------------
100700091113       BEGSR  sr_AnnTAB_2;
100800091113
100900091113         xx = %lookup( w005a : $5F_LnpNrs );
101000091113
101100091113         DoW  xx > *zero;
101200091113
101300091116           k_TBLkey = $5F_Term(xx) + $5F_LnpNrs(xx);
101400091113           chain  %kds(k03tabel00)  TABEL;
101500091113           if  %found(TABEL00F);
101600091113             TBLflg = '*';
101700091113             clear  TBLftr;
101800091113             clear  TBLdtr;
101900091113             //____________
102000091113             update  TABEL;
102100091113             //¯¯¯¯¯¯¯¯¯¯¯¯
102200091113           endif;
102300091113
102400091113           yy = xx + 1;
102500091116           xx = %lookup( w005a : $5F_LnpNrs : yy );
102600091113
102700091113         EndDo;
102800091113
102900091113       ENDSR;
103000091113
103100091113       //--------------------------------------------------------------
103200091113       //?Annullamento records nella tabella "5F".                     ?
103300091113       //--------------------------------------------------------------
103400091113       BEGSR  sr_AnnTABEL;
103500091113
103600091117         //exsr  sr_UpdTABEL;
103700091117         //exsr  sr_CarTAB;
103800091113
103900091113         // -?Annullamento 2° tipo rec. (TER/FLS/NRS) nella tab. "5F"?
104000091113         exsr  sr_AnnTAB_2;
104100091113
104200091113         // -?Annullamento 1° tipo rec. (___/FLS/NRS) nella tab. "5F"?
104300091117         evalr k_TBLkey = w005a;
104400091113
104500091113         chain  %kds(k03tabel00)  TABEL;
104600091113
104700091113         if  %found(TABEL00F);
104800091113           TBLflg = '*';
104900091113           clear  TBLftr;
105000091113           clear  TBLdtr;
105100091113           //____________
105200091113           update  TABEL;
105300091113           //¯¯¯¯¯¯¯¯¯¯¯¯
105400091113         endif;
105500091113
105600091113       ENDSR;
105700091113
105800091113       //--------------------------------------------------------------
105900091113       //?Operazioni finali.                                           ?
106000091113       //--------------------------------------------------------------
106100091113       BEGSR  sr_RoutEnd;
106200091113
106300091113         return;
106400091113
106500091113       ENDSR;
106600091113
106700091113      /end-free
106800091113
106900091113       //--------------------------------------------------------------
107000091113       //?Schiere a tempo di compilazione.                             ?
107100091113       //--------------------------------------------------------------
107200091116
107300091116** - $iSystem / $SysInf:?Sistemi AS/400 & Librerie con entrambi i file?
107400091127SETRAS  GAITRAGRU FILTRAGRU
107500091116AS888   GAITRAGRPSFILTRAGRPF
107600091113** - $Msg:?Messaggi di Errore?-----------------------------------------------*
107700091113Linea di partenza obbligatoria                                                 1
107800091113Linea di partenza inesistente                                                  2
107900091113Immettere un numero di serie maggiore di 00                                    3
108000091113Linea immessa non deve avere lo stesso terminal Partenza della linea segnacoll 4
