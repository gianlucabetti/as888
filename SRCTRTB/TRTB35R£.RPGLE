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
009600160317     d
009700160317     d wlin            S              3    DIM(18)
009800160317     d wdes            S             13    DIM(18)
009900091113
010000091113       // - Terminal in TBLKEY
010100140626     d $5F_Term        s              3    dim(5000)     inz
010200091113       // - Lnp + Nrs in TBLKEY
010300140626     d $5F_LnpNrs      s              5    dim(5000)     inz
010400091113       // - Filiali in TBLUNI
010500140626     d $5F_Uni         s             89    dim(5000)     inz
010600091113       // - Flags Annullamento (TBLFLG)
010700140626     d $5F_Ann         s              1    dim(5000)     inz
010800091113
010900091113       // - Filiali in TBLUNI del singolo record
011000091113     d ds5F_Fil        ds            87    inz
011100091117     d   $5F_Fil                      3    dim(29)       inz
011200091113
011300091113       // - Terminal
011400091117     d $Term           s              3  0 dim(c_MaxFil) inz
011500091117     d $SaveTerm       s              3  0 dim(c_MaxFil) inz
011600091117
011700091117       // - Salvataggio filiali gestite a video
011800091117     d $SaveLin        s                   like(V2Cf01)
011900091117     d                                     dim(c_MaxFil)  inz
012000091116
012100091116       // - Ridefinizione elenco librerie in elaborare le tabelle
012200091116     d ds_Libr         ds                  inz
012300091116     d  $Libr                        10    dim(c_NrLibr) inz
012400091113
012500091113       //--------------------------------------------------------------
012600091113       //?Definizione aree dati.                                       ?
012700091113       //--------------------------------------------------------------
012800091113
012900091113       // - Dati utente
013000091113     d §AzUte        e ds                  extname(AZUTE00F)
013100091113     d                                     dtaara
013200091113     d §DatiUte      e ds                  extname(dDatiUte)
013300091113     d                                     dtaara
013400091113
013500091113       //--------------------------------------------------------------
013600091113       //?Definizione strutture dati.                                  ?
013700091113       //--------------------------------------------------------------
013800091113
013900091113       // - Status ds
014000091113     d Status         sds
014100091113     d  SDSpgm           *proc
014200091113
014300091113       // - InfDS
014400091113     d InfDspF         ds
014500091113     d   dsp_aid             369    369a
014600091113
014700091113       // - Indicatori su DspF
014800091113     d IndDspF         ds                  inz
014900091113         // - Abilitazione tasti funzionali
015000091113     d   F5Attivo                      n   overlay(IndDspF : 05)
015100091113     d   F6Attivo                      n   overlay(IndDspF : 06)
015200091113     d   F10Attivo                     n   overlay(IndDspF : 10)
015300091113     d   F11Attivo                     n   overlay(IndDspF : 11)
015400091113     d   F12Attivo                     n   overlay(IndDspF : 12)
015500091113     d   F16Attivo                     n   overlay(IndDspF : 16)
015600091113     d   F17Attivo                     n   overlay(IndDspF : 17)
015700091113     d   F18Attivo                     n   overlay(IndDspF : 18)
015800091113     d   F19Attivo                     n   overlay(IndDspF : 19)
015900091113     d   F20Attivo                     n   overlay(IndDspF : 20)
016000091113     d   F21Attivo                     n   overlay(IndDspF : 21)
016100091113     d   F22Attivo                     n   overlay(IndDspF : 22)
016200091113         // - Emissione messaggio di errore
016300091113     d   ErrMessage                    n   overlay(IndDspF : 28)
016400091113         // - Indicatori per Attibuti di visualizzazione
016500091113     d   VisBloAnn                     n   overlay(IndDspF : 40)
016600091113     d   VisInfoES                     n   overlay(IndDspf : 41)
016700091113         // - Posizionamento cursore & segnalazione errore
016800091113     d   PosCurLNP                     n   overlay(IndDspF : 51)
016900091113     d   PosCurNRS                     n   overlay(IndDspF : 52)
017000091113     d   PosCurF01                     n   overlay(IndDspF : 53)
017100091113     d   PosCurF02                     n   overlay(IndDspF : 54)
017200091113     d   PosCurF03                     n   overlay(IndDspF : 55)
017300091113     d   PosCurF04                     n   overlay(IndDspF : 56)
017400091113     d   PosCurF05                     n   overlay(IndDspF : 57)
017500091113     d   PosCurF06                     n   overlay(IndDspF : 58)
017600091113     d   PosCurF07                     n   overlay(IndDspF : 59)
017700091113     d   PosCurF08                     n   overlay(IndDspF : 60)
017800091113     d   PosCurF09                     n   overlay(IndDspF : 61)
017900091113     d   PosCurF10                     n   overlay(IndDspF : 62)
018000091113     d   PosCurF11                     n   overlay(IndDspF : 63)
018100160308     d   PosCurF12                     n   overlay(IndDspF : 64)
018200160308     d   PosCurF13                     n   overlay(IndDspF : 65)
018300160308     d   PosCurF14                     n   overlay(IndDspF : 66)
018400160308     d   PosCurF15                     n   overlay(IndDspF : 67)
018500160308     d   PosCurF16                     n   overlay(IndDspF : 68)
018600160308     d   PosCurF17                     n   overlay(IndDspF : 69)
018700160308     d   PosCurF18                     n   overlay(IndDspF : 70)
018800091113         //   - Riemissione videata
018900091113     d   ErrGenerico                   n   overlay(IndDspF : 99)
019000091113
019100091113       // - Parametri ricevuti
019200091113     d KPJBA         e ds
019300091113
019400091113       // - Parametri per Reperimento dati utente
019500091113     d TIBS34ds      e ds                  inz
019600091113
019700091113       // - Parametri per Ricerca terminal di Partenza/Arrivo
019800091113     d FNLV55ds      e ds                  inz
019900091113     d   D55tpt      e                     inz('P')
020000091113
020100091113       // - Strutture di comodo per il raggruppamento dei campi a video
020200091113     d ds_V2Cfil       ds                  inz
020300091113     d  V2Cf01                             inz
020400091113     d  V2Cf02                             inz
020500091113     d  V2Cf03                             inz
020600091113     d  V2Cf04                             inz
020700091113     d  V2Cf05                             inz
020800091113     d  V2Cf06                             inz
020900091113     d  V2Cf07                             inz
021000091113     d  V2Cf08                             inz
021100091113     d  V2Cf09                             inz
021200091113     d  V2Cf10                             inz
021300091113     d  V2Cf11                             inz
021400160308     d  V2Cf12                             inz
021500160308     d  V2Cf13                             inz
021600160308     d  V2Cf14                             inz
021700160308     d  V2Cf15                             inz
021800160308     d  V2Cf16                             inz
021900160308     d  V2Cf17                             inz
022000160308     d  V2Cf18                             inz
022100160308     d   $Lin                  1     54    dim(c_MaxFil)  inz
022200091113     d ds_V2Dfil       ds                  inz
022300091113     d  V2Df01                             inz
022400091113     d  V2Df02                             inz
022500091113     d  V2Df03                             inz
022600091113     d  V2Df04                             inz
022700091113     d  V2Df05                             inz
022800091113     d  V2Df06                             inz
022900091113     d  V2Df07                             inz
023000091113     d  V2Df08                             inz
023100091113     d  V2Df09                             inz
023200091113     d  V2Df10                             inz
023300091113     d  V2Df11                             inz
023400160308     d  V2Df12                             inz
023500160308     d  V2Df13                             inz
023600160308     d  V2Df14                             inz
023700160308     d  V2Df15                             inz
023800160308     d  V2Df16                             inz
023900160308     d  V2Df17                             inz
024000160308     d  V2Df18                             inz
024100160308     d   $Des                  1    180    dim(c_MaxFil)  inz
024200091113
024300091113       // - Parametri per Controllo/Inversione date
024400091113     d WLBdat          ds                  inz
024500091113     d  G02dat                        8  0 inz
024600091113     d  G02inv                        8  0 inz
024700091113     d  G02err                        1    inz
024800091113     d  G02tgi                        5  0 inz
024900091113
025000091113       //--------------------------------------------------------------
025100091113       //?Definizione variabili globali.                               ?
025200091113       //--------------------------------------------------------------
025300091113
025400091113       // - Flags booleani
025500091113     d $Fine           s               n   inz
025600091113     d $InzD01         s               n   inz(*on)
025700091113     d $InzD02         s               n   inz(*on)
025800091117     d $Annull         s               n   inz
025900091113
026000091113       // - Variabili per la gestione del video
026100091113     d $Video          s              2    inz('D1')
026200091116
026300091116       // - Indici di schiera
026400091116     d ww              s              5  0 inz
026500091116     d xx              s              5  0 inz
026600091116     d yy              s              5  0 inz
026700091113
026800091113       // - Nome del sistema
026900091113     d currSysNeta     s              8a   inz
027000091113
027100091113       // - Nome esteso Libreria/File dei 2 file tabella
027200091113     d wLibFile        s             21a   inz
027300091113
027400091113       // - Terminal di Partenza
027500091113     d wwwTFP          s                   like(D55tfp)
027600091113
027700091113       // - Parametri per TNSD19R
027800091113     d SD19cod         s              3    inz
027900091113     d SD19tip         s              1    inz
028000091113     d SD19des         s             25    inz
028100091113
028200091113       // - Campi di comodo
028300091113     d wDate_EUR       s               d   datfmt(*eur)  inz
028400091113     d wDate           s              8  0 inz
028500091113     d w_iSystem       s              1  0 inz
028600091113     d w_SisInf        s              3  0 inz
028700091113     d w005a           s              5    inz
028800091113
028900091113       //--------------------------------------------------------------
029000091113       //?Definizione prototipi procedure.                             ?
029100091113       //--------------------------------------------------------------
029200091113
029300091113       // - Reperimento dati utente
029400091113      /copy gaitrasrc/srcProtoPR,TIBS34R
029500091113
029600091113       // - Controllo Terminal
029700091113      /copy gaitrasrc/srcProtoPR,FNLV55R
029800091113
029900091113       // - Ricerca codici Organigramma
030000091113     d tnsd19r         pr                  extpgm('TNSD19R')
030100091113     d  D19cod                             like(SD19cod)
030200091113     d  D19tip                             like(SD19tip)
030300091113     d  D19des                             like(SD19des)
030400091116
030500091116       // - Reperimento NETA sistema AS/400 corrente
030600091116     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
030700091113
030800091113       //--------------------------------------------------------------
030900091113       //?Definizione key-list.                                        ?
031000091113       //--------------------------------------------------------------
031100091113
031200091113       // - File TABEL00F
031300091113     d k03tabel00    e ds                  extname(TABEL00F : *key)
031400091113     d                                     prefix(k_)   inz
031500091113
031600091113       //--------------------------------------------------------------
031700091113       //?M A I N - L I N E                                            ?
031800091113       //--------------------------------------------------------------
031900091113
032000091113     c     *Entry        plist
032100091113     c                   parm                    KPJBA
032200091113
032300091113      /free
032400091113
032500091113       // -?Operazioni iniziali?
032600091113       exsr sr_RoutInz;
032700091113
032800091113       // -?Gestione videate?
032900091113       DoW  $Fine = *off;
033000091113
033100091113         select;
033200091113
033300091113           // - Gestione videata D1
033400091113           when $Video = 'D1';
033500091113             exsr sr_GesD01;
033600091113
033700091113           // - Gestione videata D2
033800091113           when $Video = 'D2';
033900091113             exsr sr_GesD02;
034000091113
034100091113           other;
034200091113             $Fine = *on;
034300091113
034400091113         endsl;
034500091113
034600091113       EndDo;
034700091113
034800091113       // -?Operazioni finali?
034900091113       exsr sr_RoutEnd;
035000091113
035100091113       //--------------------------------------------------------------
035200091113       //?Operazioni iniziali.                                         ?
035300091113       //--------------------------------------------------------------
035400091113       BEGSR  sr_RoutInz;
035500091113
035600091113         *inLR = *on;
035700091113
035800091113         // -?Verifica del sistema AS/400 corrente?
035900091113         if  ubRtvNetA_Rtv(currSysNeta) <> *zero;
036000091113           $Fine = *on;
036100091113           leavesr;
036200091113         endif;
036300091113
036400091113         // -?Impostazione elenco librerie in cui gestire le tabelle?
036500091113         //  ?(a seconda del sistema in cui si stà lavorando)?
036600091113         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
036700091113         if  w_iSystem = *zero;
036800091113           $Fine = *on;
036900091113           leavesr;
037000091113         endif;
037100091113
037200091113         // -?Reperimento dati job?
037300091113         exsr  sr_DatiJob;
037400091113
037500091113         // -?Impostazione nome programma a video?
037600091113         V1Tpgm = SDSpgm;
037700091113
037800091113         // -?Impostazione campi "fissi"?
037900091113         k_TBLkut = c_Kut;
038000091113         k_TBLcod = c_Tab;
038100091113
038200091113         // -?Reperimento data odierna?
038300091113         wDate = %int( %subst( %char( %dec( %timestamp() ) )
038400091113                               : 1 : 8 ) );
038500091113
038600091113       ENDSR;
038700091113
038800091113       //--------------------------------------------------------------
038900091113       //?Reperimento Dati del job (Utente/Operativi).                 ?
039000091113       //--------------------------------------------------------------
039100091113       BEGSR  sr_DatiJob;
039200091113
039300091113         in(e) §AzUte;
039400091113         if NOT %error;
039500091113           in(e) §DatiUte;
039600091113         endif;
039700091113         if %error or RSut = *blank;
039800091113           tibs34r ( tibs34ds );
039900091113           in §AzUte;
040000091113           in §DatiUte;
040100091113         endif;
040200091113
040300091113       ENDSR;
040400091113
040500091113       //--------------------------------------------------------------
040600091113       //?Gestione videata D01.                                        ?
040700091113       //--------------------------------------------------------------
040800091113       BEGSR  sr_GesD01;
040900091113
041000091113         // -?Inizializzazione videata?
041100091113         if  $InzD01 = *on;
041200091116           clear  TB35D1;
041300091113           exsr  sr_CarTAB;
041400091113           $InzD01 = *off;
041500091113         endif;
041600091113
041700091116         // -?Emissione Testata e Piede?
041800091116         write  TB35T1;
041900091116         write  TB35P1;
042000091113
042100091113         // -?Emissione videata?
042200091116         exfmt  TB35D1;
042300091113
042400091116         clear  V1Dmsg;
042500091113         reset  ErrMessage;
042600091113         reset  ErrGenerico;
042700091113
042800091113         SELECT;
042900091113
043000091113           // -?F3=Fine?
043100091113           WHEN  dsp_aid = c_F03;
043200091113             $Fine = *on;
043300091113
043400091113           // -?Invio?
043500091113           OTHER;
043600091113             exsr sr_CtrD01;
043700091113             if  ErrGenerico = *off;
043800091113               $Video    = 'D2';
043900091113               $InzD02   = *on;
044000091113             endif;
044100091113
044200091113         ENDSL;
044300091113
044400091113       ENDSR;
044500091113
044600091113       //--------------------------------------------------------------
044700091113       //?Controllo dati immessi nella videata D01.                    ?
044800091113       //--------------------------------------------------------------
044900091113       BEGSR  sr_CtrD01;
045000091113
045100091113         %subst(IndDspF : 50) = *off;
045200091113
045300091113         // -?Linea di Partenza Segnacollo?
045400091113         clear  V1Dlnp;
045500091113         if  V1Clnp = *zero;
045600091116           V1Dmsg = $Msg(01);
045700091113           PosCurLNP   = *on;
045800091113           ErrMessage  = *on;
045900091113           ErrGenerico = *on;
046000091113           leavesr;
046100091113         endif;
046200091113         chain  (V1Clnp)  AZORG;
046300091116         if  %found(AZORG01L);
046400091113           V1Dlnp = ORGdes;
046500091113         endif;
046600091113         if  NOT %found(AZORG01L)   or
046700091113             ORGfva <> *blank       or
046800091113            (ORGfag <> 'A'      and
046900091113             ORGfag <> 'F');
047000091116           V1Dmsg = $Msg(02);
047100091113           PosCurLNP   = *on;
047200091113           ErrMessage  = *on;
047300091113           ErrGenerico = *on;
047400091113           leavesr;
047500091113         endif;
047600091113
047700091113         // -?Reperimento Terminal Partenza Linea Segnacollo?
047800091113         reset  FNLV55ds;
047900091113         D55lin = V1Clnp;
048000091113         D55drf = wDate;
048100091113         fnlv55r ( FNLV55ds );
048200091113         if  D55err = *blank;
048300091113           wwwTFP = D55tfp;
048400091113         else;
048500091113           wwwTFP = V1Clnp;
048600091113         endif;
048700091113
048800091113         // -?Serie Segnacollo?
048900091113         if  V1Cnrs = *zero;
049000091116           V1Dmsg = $Msg(03);
049100091113           PosCurNRS   = *on;
049200091113           ErrMessage  = *on;
049300091113           ErrGenerico = *on;
049400091113           leavesr;
049500091113         endif;
049600091113
049700091113       ENDSR;
049800091113
049900091113       //--------------------------------------------------------------
050000091113       //?Caricamento tabelle con terminal in KEY                      ?
050100091113       //--------------------------------------------------------------
050200091113       BEGSR  sr_CarTAB;
050300091113
050400091113         // -?Apertura file TABEL00F del 1° S.I. (sede)?
050500091113         w_SisInf = 1;
050600091113         exsr  sr_OpenFileTab;
050700091113
050800091113         clear  $5F_Term;
050900091113         clear  $5F_LnpNrs;
051000091113         clear  $5F_Uni;
051100091113         clear  $5F_Ann;
051200091113         clear  xx;
051300091113
051400091113         // - Tabelle separate per KEY SENZA terminal
051500091113         //                      e KEY CON   terminal
051600091113         //   => si elaborano SOLO quelle CON terminal
051700091113         k_TBLkey = '0010000';
051800091113
051900091113         // - Caricamento record validi dalla tabella
052000091113         setll    %kds(k03tabel00)      TABEL;
052100091113         reade(n) %kds(k03tabel00 : 2)  TABEL;
052200091113
052300091113         DoW  NOT %eof(TABEL00F);
052400091113
052500091113           if  %subst(TBLkey : 1 : 3) <> *blank;
052600091113             xx += 1;
052700091113             $5F_Term(xx)   = %subst( TBLkey : 1 : 3 );
052800091113             $5F_LnpNrs(xx) = %subst( TBLkey : 4 : 5 );
052900091113             $5F_Uni(xx) = TBLuni;
053000091116             $5F_Ann(xx) = TBLflg;
053100091113           endif;
053200091113
053300091113           reade(n) %kds(k03tabel00 : 2)  TABEL;
053400091113
053500091113         EndDo;
053600091113
053700091113       ENDSR;
053800091113
053900091113       //--------------------------------------------------------------
054000091113       //?Apertura dei files tabelle nel sistema informativo impostato.?
054100091113       //--------------------------------------------------------------
054200091113       BEGSR  sr_OpenFileTab;
054300091113
054400091113         // -?Chiusura (eventuale) archivio?
054500091113         if  %open(TABEL00F);
054600091113           close  TABEL00F;
054700091113         endif;
054800091113
054900091113         // -?Apertura archivio?
055000091116         ds_Libr  = $SisInf(w_iSystem);
055100091113         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
055200091113         open  TABEL00F;
055300091113
055400091113       ENDSR;
055500091113
055600091113       //--------------------------------------------------------------
055700091113       //?Gestione videata D02.                                        ?
055800091113       //--------------------------------------------------------------
055900091113       BEGSR  sr_GesD02;
056000091113
056100091113         // -?Inizializzazione videata?
056200091113         if  $InzD02 = *on;
056300091113           exsr  sr_InzD02;
056400091113           $InzD02 = *off;
056500091113         endif;
056600091113
056700091113         // -?Emissione Testata e Piede?
056800091116         if  Not ErrMessage;
056900091116           write  TB35T1;
057000091116           write  TB35D1;
057100091116           write  Protect;
057200091116           write  TB35P2;
057300091116         endif;
057400091113
057500091113         // -?Emissione videata?
057600091117         if  Not $Annull;
057700091113           exfmt  TB35D2;
057800091113         else;
057900091113           write  TB35D2;
058000091113           exfmt  Protect;
058100091113         endif;
058200091113
058300091113         reset  ErrMessage;
058400091113         reset  ErrGenerico;
058500091116         clear  V1Dmsg;
058600091113
058700091113         SELECT;
058800091113
058900091113           // -?F3=Fine?
059000091113           WHEN  dsp_aid = c_F03;
059100091113             unlock  TABEL00F;
059200091113             $Fine = *on;
059300091113
059400091113           // -?F12=Ritorno?
059500091113           WHEN  dsp_aid = c_F12;
059600091113             unlock  TABEL00F;
059700091113             $Video = 'D1';
059800091113
059900091113           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
060000091113           OTHER;
060100091113             // - Controlli eseguiti NON se F16=Annullamento
060200091113             if  dsp_aid <> c_F16;
060300091113               exsr sr_CtrD02;
060400091113               if  ErrGenerico = *on;
060500091113                 leavesr;
060600091113               endif;
060700091113             endif;
060800091113             // - Aggiornamento
060900091113             if  dsp_aid = c_F05   or
061000091113                 dsp_aid = c_F06   or
061100091113                 dsp_aid = c_F16;
061200091113               exsr  sr_UpdateAll;
061300091113               if  ErrGenerico = *on;
061400091113                 leavesr;
061500091113               endif;
061600091113               $Video  = 'D1';
061700091113               $InzD01 = *on;
061800091113             endif;
061900091113
062000091113         ENDSL;
062100091113
062200091113         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
062300091113         if  $Video = 'D1';
062400091113           clear  V1Topz;
062500091113         endif;
062600091113
062700091113       ENDSR;
062800091113
062900091113       //--------------------------------------------------------------
063000091113       //?Inizializzazione dati nella videata D02.                     ?
063100091113       //--------------------------------------------------------------
063200091113       BEGSR  sr_InzD02;
063300091113
063400091117         reset  $Annull;
063500091117
063600091113         // -?Verifica esistenza del record in TABEL?
063700091113         //k_TBLkey = '   '
063800091113         //         + %editc(V1Clnp : 'X')
063900091113         //         + %editc(V1Cnrs : 'X');
064000091113         evalr k_TBLkey = %editc(V1Clnp : 'X')
064100091113                        + %editc(V1Cnrs : 'X');
064200091113         chain  %kds(k03tabel00)  TABEL;
064300091113
064400091113         // -?Impostazione testata?
064500091113         clear  V1Topz;
064600091113         select;
064700091113           when  Not %found(TABEL00F);
064800091117             V1Topz  = '  INSERIMENTO  ';
064900091113           when  TBLflg = '*';
065000091117             V1Topz  = '  RIPRISTINO   ';
065100091117             $Annull = *on;
065200091113           other;
065300091117             V1Topz  = '   MODIFICA    ';
065400091113         endsl;
065500091113
065600091113         // -?Impostazione dettaglio?
065700091116         clear  TB35D2;
065800091113
065900091113         clear  $Lin;
066000091113         clear  $Des;
066100160317         clear  wLin;
066200160317         clear  wDes;
066300091113         clear  $Term;
066400091113         clear  xx;
066500091113         clear  yy;
066600091113         clear  ww;
066700091113
066800091113         IF  %found(TABEL00F);
066900091113
067000091113           // -?Reperimento "fratellini" della tabella?
067100091113           w005a = %subst(TBLkey : 4 : 5);
067200091113           xx = %lookup( w005a : $5F_LnpNrs );
067300091113
067400091113           DoW  xx > *zero;
067500091113
067600091113             if  TBLflg = $5F_Ann(xx);
067700091113               ds5F_Fil = $5F_Uni(xx);
067800091113               exsr  sr_CarD02;
067900091117             endif;
068000091117             yy = xx + 1;
068100091117             xx = %lookup( w005a : $5F_LnpNrs : yy );
068200091113
068300091113           EndDo;
068400091113
068500091113         ENDIF;
068600160317
068700160317
068800160317         // ordino le schiere e le emetto a video
068900160317         sorta wlin   ;
069000160317         sorta wdes   ;
069100160317         yy=1       ;
069200160317         dow yy<=18   ;
069300160317         $lin(yy)=wlin(yy)  ;
069400160317         $des(yy)=%subst(wdes(yy):4:10)  ;
069500160317         yy=yy+1 ;
069600160317         enddo  ;
069700091113
069800091113         // -?Abilitazione tasti funzionali?
069900091113         exsr  sr_AbilFxx;
070000091113
070100091116       ENDSR;
070200091113
070300091113       //--------------------------------------------------------------
070400091113       //?Caricamento filiali che possono effettuare la conferma.      ?
070500091113       //--------------------------------------------------------------
070600091113       BEGSR  sr_CarD02;
070700091113
070800091113         yy = 1;
070900091113
071000091113         DoW  yy <= %elem($5F_Fil)   and   $5F_Fil(yy) > *zero;
071100091113
071200091116           chain  ( %int( $5F_Fil(yy) ) )  AZORG;
071300091113
071400091113           select;
071500091113             when  NOT %found(AZORG01L);
071600091113             when  ORGde5 <> *blank;
071700091113                ww += 1;
071800160317                wLin(ww) = $5F_Fil(yy);
071900160317                wDes(ww) = $5F_Fil(yy)+
072000160317                           %subst( ORGde5 : 1 : %size(V2Df01) );
072100091113             other;
072200091113                ww += 1;
072300160317                wLin(ww) = $5F_Fil(yy);
072400160317                wDes(ww) = $5F_Fil(yy) +
072500160317                           %subst( ORGdes : 1 : %size(V2Df01) );
072600091113           endsl;
072700091113
072800091113           yy += 1;
072900091113
073000091113         EndDo;
073100091113
073200091113       ENDSR;
073300091113
073400091113       //--------------------------------------------------------------
073500091113       //?Abilitazione tasti funzionali in P02 (per D02).              ?
073600091113       //--------------------------------------------------------------
073700091113       BEGSR  sr_AbilFxx;
073800091113
073900091113         // ->?F5 = Ripristino?
074000091117         F5Attivo = (%found(TABEL00F)   and   $Annull);
074100091113
074200091113         // ->?F6 = Conferma?
074300091117         F6Attivo = ((%found(TABEL00F)   and   Not $Annull)  OR
074400091116                      Not %found(TABEL00F));
074500091113
074600091113         // ->?F12 = Ritorno?
074700091113         F12Attivo = ($Video = 'D2');
074800091113
074900091113         // ->?F16 = Annullamento?
075000091117         F16Attivo = (%found(TABEL00F)   and   Not $Annull  and
075100091113                      %subst( KNMUS : 1 : 3 ) = c_EDP);
075200091113
075300091113       ENDSR;
075400091113
075500091113       //--------------------------------------------------------------
075600091113       //?Controllo dati nella videata D02.                            ?
075700091113       //--------------------------------------------------------------
075800091113       BEGSR  sr_CtrD02;
075900091113
076000091113         clear  $Term;
076100091113
076200091113         For  yy = 1  To %elem($Term);
076300091113
076400091113           clear  $Des(yy);
076500091113
076600091113           if  $Lin(yy) <> *blank  and   $Lin(yy) <> *zero;
076700091113
076800091113             select;
076900091113               // - Richiesta ricerca
077000091113               when  %scan( '?' : $Lin(yy) ) > *zero;
077100091113                 clear  $Lin(yy);
077200091113                 clear  SD19cod;
077300091113                 clear  SD19tip;
077400091113                 clear  SD19des;
077500091113                 tnsd19r ( SD19cod : SD19tip : SD19des );
077600091113                 $Lin(yy) = SD19cod;
077700091113                 ErrGenerico = *on;
077800091113               // - Codice filiale alfanumerica
077900091116               when  %check( c_Digits : $Lin(yy) ) > *zero   or
078000091113                     $Lin(yy) < *zero;
078100091116                 V1Dmsg = $Msg(02);
078200091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
078300091113                 ErrMessage  = *on;
078400091113                 ErrGenerico = *on;
078500091113                 leavesr;
078600091113             endsl;
078700091113
078800091116             chain  ( %int( $Lin(yy) ) )  AZORG;
078900091113
079000091113             select;
079100091113               // - Filiale non trovata
079200091113               when  NOT %found(AZORG01L);
079300091116                 V1Dmsg = $Msg(02);
079400091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
079500091113                 ErrMessage  = *on;
079600091113                 ErrGenerico = *on;
079700091113                 leavesr;
079800091113               // - Filiale annullata
079900091113               when  ORGfva <> *blank;
080000091116                 V1Dmsg = $Msg(02);
080100091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
080200091113                 ErrMessage  = *on;
080300091113                 ErrGenerico = *on;
080400091113                 leavesr;
080500091113               // - Né filiale né agenzia
080600091113               when  ORGfag <> 'F'   and   ORGfag <> 'A';
080700091116                 V1Dmsg = $Msg(02);
080800091113                 %subst( IndDspF : (52 + yy) : 1 ) = *on;
080900091113                 ErrMessage  = *on;
081000091113                 ErrGenerico = *on;
081100091113                 leavesr;
081200091113             endsl;
081300091113
081400091113             // - Decodifica filiale
081500091113             if  ORGde5 <> *blank;
081600091116               $Des(yy) = %subst( ORGde5 : 1 : %size(V2Df01) );
081700091113             else;
081800091116               $Des(yy) = %subst( ORGdes : 1 : %size(V2Df01) );
081900091113             endif;
082000091113
082100091113             // - La linea NON può avere lo stesso Terminal di Partenza
082200091113             //   della Linea Segnacollo.
082300091113             //   => Reperimento Terminal Partenza della linea immessa
082400091113             reset  fnlv55ds;
082500091116             D55lin = %int( $Lin(yy) );
082600091113             D55drf = wDate;
082700091113             fnlv55r ( FNLV55ds );
082800091113             if  D55tfp = wwwTFP;
082900091116               V1Dmsg = $Msg(04);
083000091113               %subst( IndDspF : (52 + yy) : 1 ) = *on;
083100091113               ErrMessage  = *on;
083200091113               ErrGenerico = *on;
083300091113               leavesr;
083400091117             else;
083500091117               $Term(yy) = D55tfp;
083600091117             endif;
083700091113
083800091113           endif;
083900091113
084000091113         EndFor;
084100091113
084200091113       ENDSR;
084300091113
084400091113       //--------------------------------------------------------------
084500091113       //?Aggiornamento record nel file TABEL00F.                      ?
084600091113       //--------------------------------------------------------------
084700091113       BEGSR  sr_UpdateAll;
084800091113
084900091113         //? N.B.                                                      ?
085000091113         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
085100091117         //   trasmissione (vedi flag TBLFTR).                        ?
085200091113         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
085300091113         //   subito nello stesso file di entrambi i S.I. - in due    ?
085400091113         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
085500091117
085600091117         $SaveLin  = $Lin;
085700091117         $SaveTerm = $Term;
085800091117         w005a = %editc(V1Clnp : 'X') + %editc(V1Cnrs : 'X');
085900091113
086000091113         // -?Ciclo di elaborazione per ogni sistema informativo?
086100091113         For  w_SisInf = 1  To  %elem($Libr);
086200091113
086300091113           // -?Apertura degli archivi?
086400091113           if  w_SisInf > 1;
086500091113             exsr  sr_OpenFileTab;
086600091117             $Lin  = $SaveLin;
086700091117             $Term = $SaveTerm;
086800091113           endif;
086900091113
087000091113           // -?Aggiorn. tab. "5F" = Segnacolli confermabili da altre?  ?
087100091113           //                       ?altre fil.?  ?
087200091117           if  dsp_aid = c_F16;
087300091117             exsr  sr_AnnTABEL;
087400091117           else;
087500091117             exsr  sr_UpdTABEL;
087600091117           endif;
087700091113
087800091113         EndFor;
087900091113
088000091113       ENDSR;
088100091113
088200091113       //--------------------------------------------------------------
088300091113       //?Aggiornamento records nel file TABEL00F.                     ?
088400091113       //--------------------------------------------------------------
088500091113       BEGSR  sr_UpdTABEL;
088600091117
088700091113         // -?Annullamento dei record già inseriti?
088800091117         //   (se NON richiesto il ripristino)
088900091130         if  dsp_aid <> c_F05;
089000091117           exsr  sr_AnnTAB_2;
089100091117         endif;
089200091113
089300091113         // -?Aggiornamento o Scrittura dei nuovi record?
089400091113         evalr k_TBLkey = w005a;
089500091113
089600091113         chain  %kds(k03tabel00)  TABEL;
089700091113
089800091113         if  NOT  %found(TABEL00F);
089900091113           clear  TABEL;
090000091113           TBLcod = c_Tab;
090100091113           TBLkut = c_Kut;
090200091113           TBLkey = k_TBLkey;
090300091113           TBLftt = '1';
090400091113         endif;
090500091113
090600091113         clear  $5F_Fil;
090700091113         clear  xx;
090800091113
090900091113         For  yy = 1  To  %elem($Term);
091000091113
091100091113           if  $Term(yy) > *zero   and
091200091113               %lookup( %editc( $Term(yy) : 'X' ) : $5F_Fil ) = *zero;
091300091113             xx += 1;
091400091116             $5F_Fil(xx) = %editc( $Term(yy) : 'X' );
091500091113           endif;
091600091113
091700091113         EndFor;
091800091113
091900091113         TBLuni = ds5F_Fil;
092000091113
092100091113         clear  TBLflg;
092200091117         if  dsp_aid = c_F05;
092300091117           clear  TBLftr;
092400091117           clear  TBLdtr;
092500091117         else;
092600091117           if  w_SisInf = 1;
092700091117             TBLftr = 'T';
092800091117           else;
092900091117             TBLftr = 'R';
093000091117           endif;
093100091117           TBLdtr = %int( %subst( %editc(wDate : 'X') :
093200091117                                  %len(wDate) - %len(TBLdtr) + 1 :
093300091117                                  %len(TBLdtr) ) );
093400091117         endif;
093500091113
093600091113         if  %found(TABEL00F);
093700091113           //____________
093800091113           update  TABEL;
093900091113           //¯¯¯¯¯¯¯¯¯¯¯¯
094000091113         else;
094100091113           //___________
094200091113           write  TABEL;
094300091113           //¯¯¯¯¯¯¯¯¯¯¯
094400091113         endif;
094500091113
094600091113         // -
094700091113         // -?Scrittura dei record con TBLKEY=TermPar+Lnp+Nrs e?
094800091113         //                           ?TBLUNI=Fil. che conferma?
094900091113
095000091113         For  yy = 1  To  %elem($Term);
095100091113
095200091113           If  $Lin(yy) > *zero;
095300091113
095400091116             k_TBLkey = %editc($Term(yy) : 'X') + w005a;
095500091113
095600091113             chain  %kds(k03tabel00)  TABEL;
095700091113
095800091113             if  NOT  %found(TABEL00F);
095900091113               clear  TABEL;
096000091113               TBLcod = c_Tab;
096100091113               TBLkut = c_Kut;
096200091113               TBLkey = k_TBLkey;
096300091113               TBLftt = '1';
096400091113             endif;
096500091113
096600091113             clear  TBLflg;
096700091117             if  dsp_aid = c_F05;
096800091117               clear  TBLftr;
096900091117               clear  TBLdtr;
097000091117             else;
097100091117               if  w_SisInf = 1;
097200091117                 TBLftr = 'T';
097300091117               else;
097400091117                 TBLftr = 'R';
097500091117               endif;
097600091117               TBLdtr = %int( %subst( %editc(wDate : 'X') :
097700091117                                      %len(wDate) - %len(TBLdtr) + 1 :
097800091117                                      %len(TBLdtr) ) );
097900091117             endif;
098000091113
098100091113             // - Memorizzazione delle fil. di quel Terminal
098200091113             clear  $5F_Fil;
098300091113             $5F_Fil(01) = $Lin(yy);
098400091113             ww = 1;
098500091113             xx = yy + 1;
098600091113
098700091113             if  xx <= %elem($Term);
098800091116               xx = %lookup( $Term(yy) : $Term : xx );
098900091113             endif;
099000091113
099100091116             DoW  xx > *zero  and  xx <= %elem($Term);
099200091113
099300091113               ww += 1;
099400091113               $5F_Fil(ww) = $Lin(xx);
099500091113               clear  $Lin(xx);
099600091113               clear  $Term(xx);
099700091113               xx += 1;
099800091113               if  xx <= %elem($Term);
099900091116                 xx = %lookup( $Term(yy) : $Term : xx );
100000091113               endif;
100100091113
100200091113             EndDo;
100300091113
100400091113             TBLuni = ds5F_Fil;
100500091113
100600091113             if  %found(TABEL00F);
100700091113               //____________
100800091113               update  TABEL;
100900091113               //¯¯¯¯¯¯¯¯¯¯¯¯
101000091113             else;
101100091113               //___________
101200091113               write  TABEL;
101300091113               //¯¯¯¯¯¯¯¯¯¯¯
101400091113             endif;
101500091113
101600091113           EndIf;
101700091113
101800091113         EndFor;
101900091113
102000091113       ENDSR;
102100091113
102200091113       //--------------------------------------------------------------
102300091113       //?Annullamento 2° tipo rec. (TER/FLS/NRS) nella tab. "5F".     ?
102400091113       //--------------------------------------------------------------
102500091113       BEGSR  sr_AnnTAB_2;
102600091113
102700091113         xx = %lookup( w005a : $5F_LnpNrs );
102800091113
102900091113         DoW  xx > *zero;
103000091113
103100091116           k_TBLkey = $5F_Term(xx) + $5F_LnpNrs(xx);
103200091113           chain  %kds(k03tabel00)  TABEL;
103300091113           if  %found(TABEL00F);
103400091113             TBLflg = '*';
103500091113             clear  TBLftr;
103600091113             clear  TBLdtr;
103700091113             //____________
103800091113             update  TABEL;
103900091113             //¯¯¯¯¯¯¯¯¯¯¯¯
104000091113           endif;
104100091113
104200091113           yy = xx + 1;
104300091116           xx = %lookup( w005a : $5F_LnpNrs : yy );
104400091113
104500091113         EndDo;
104600091113
104700091113       ENDSR;
104800091113
104900091113       //--------------------------------------------------------------
105000091113       //?Annullamento records nella tabella "5F".                     ?
105100091113       //--------------------------------------------------------------
105200091113       BEGSR  sr_AnnTABEL;
105300091113
105400091117         //exsr  sr_UpdTABEL;
105500091117         //exsr  sr_CarTAB;
105600091113
105700091113         // -?Annullamento 2° tipo rec. (TER/FLS/NRS) nella tab. "5F"?
105800091113         exsr  sr_AnnTAB_2;
105900091113
106000091113         // -?Annullamento 1° tipo rec. (___/FLS/NRS) nella tab. "5F"?
106100091117         evalr k_TBLkey = w005a;
106200091113
106300091113         chain  %kds(k03tabel00)  TABEL;
106400091113
106500091113         if  %found(TABEL00F);
106600091113           TBLflg = '*';
106700091113           clear  TBLftr;
106800091113           clear  TBLdtr;
106900091113           //____________
107000091113           update  TABEL;
107100091113           //¯¯¯¯¯¯¯¯¯¯¯¯
107200091113         endif;
107300091113
107400091113       ENDSR;
107500091113
107600091113       //--------------------------------------------------------------
107700091113       //?Operazioni finali.                                           ?
107800091113       //--------------------------------------------------------------
107900091113       BEGSR  sr_RoutEnd;
108000091113
108100091113         return;
108200091113
108300091113       ENDSR;
108400091113
108500091113      /end-free
108600091113
108700091113       //--------------------------------------------------------------
108800091113       //?Schiere a tempo di compilazione.                             ?
108900091113       //--------------------------------------------------------------
109000091116
109100091116** - $iSystem / $SysInf:?Sistemi AS/400 & Librerie con entrambi i file?
109200091127SETRAS  GAITRAGRU FILTRAGRU
109300091116AS888   GAITRAGRPSFILTRAGRPF
109400091113** - $Msg:?Messaggi di Errore?-----------------------------------------------*
109500091113Linea di partenza obbligatoria                                                 1
109600091113Linea di partenza inesistente                                                  2
109700091113Immettere un numero di serie maggiore di 00                                    3
109800091113Linea immessa non deve avere lo stesso terminal Partenza della linea segnacoll 4
