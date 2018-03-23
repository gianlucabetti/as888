000100130723      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200130723       //==============================================================
000300150518       //?Gestione File FIBSP00F - Abilitazione clienti e filiali alla ?
000301150518       //?                         sospensione  bolle
000400130723       //==============================================================
000500130723
000600130723       //--------------------------------------------------------------
000700130723       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800130723       //--------------------------------------------------------------
000900130723
001000130723     /*PRM  dbgview(*source)
001100130723     /*END
001200130723
001300130723       //--------------------------------------------------------------
001400130723       //?Specifiche di controllo.                                     ?
001500130723       //--------------------------------------------------------------
001600130723
001700130723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800130723     h dftactgrp(*no)
001900150519     h bnddir('TRUL')
002000130723
002100130723       //--------------------------------------------------------------
002200130723       //?Dichiarazione file.                                          ?
002300130723       //--------------------------------------------------------------
002400130723
002500130723       // -?Organigramma?
002600130723     fAZORG01L  if   e           k disk
002700130723
002701150518       // -?File clienti/filiali abilitati
002702150519     fFIBSP01L  Uf A e           k disk
002704150519     fFIBSP02L  Uf A e           k disk    rename(fibsp000:fibsp02)
002902150505
002903150505       // -?Anagrafica Aut
002904150505     fFIAPD01L  If   e           k disk
003100130723
003200130723       // -?Video?
003300150526     ffnlsBSPD  cf   e             workstn
003400130723     f                                     indds(IndDspF)
003500130723     f                                     infds(InfDspF)
003600130723
003700130723       //--------------------------------------------------------------
003800130723       //?Definizione costanti.                                        ?
003900130723       //--------------------------------------------------------------
004300130723
004400130723       // -?Costante per controllo "caratteri solo numerici"?
004500130723     d c_Digits        c                   const('0123456789')
004600131212
004700131212       // -?Costanti per la definizione delle schiere con i nomi?
005100130723
005200130723       // -?Tasti funzionali a video?
005300130723     d c_F01           c                   const(x'31')
005400130723     d c_F02           c                   const(x'32')
005500130723     d c_F03           c                   const(x'33')
005600130723     d c_F04           c                   const(x'34')
005700130723     d c_F05           c                   const(x'35')
005800130723     d c_F06           c                   const(x'36')
005900130723     d c_F07           c                   const(x'37')
006000130723     d c_F08           c                   const(x'38')
006100130723     d c_F09           c                   const(x'39')
006200130723     d c_F10           c                   const(x'3A')
006300130723     d c_F11           c                   const(x'3B')
006400130723     d c_F12           c                   const(x'3C')
006500130723     d c_F13           c                   const(x'B1')
006600130723     d c_F14           c                   const(x'B2')
006700130723     d c_F15           c                   const(x'B3')
006800130723     d c_F16           c                   const(x'B4')
006900130723     d c_F17           c                   const(x'B5')
007000130723     d c_F18           c                   const(x'B6')
007100130723     d c_F19           c                   const(x'B7')
007200130723     d c_F20           c                   const(x'B8')
007300130723     d c_F21           c                   const(x'B9')
007400130723     d c_F22           c                   const(x'BA')
007500130723     d c_F23           c                   const(x'BB')
007600130723     d c_F24           c                   const(x'BC')
007700130723     d c_Enter         c                   const(x'F1')
007800130723     d c_RollDown      c                   const(x'F4')
007900130723     d c_RollUp        c                   const(x'F5')
008000130723
008100130723       //--------------------------------------------------------------
008200130723       //?Definizione schiere.                                         ?
008300130723       //--------------------------------------------------------------
009200130723
009300130723       // -?Messaggi di errore?
009400151030     d $Msg            s             78    dim(19)  ctdata  perrcd( 1)
009500130723
009600130723       //--------------------------------------------------------------
009700130723       //?Definizione aree dati.                                       ?
009800130723       //--------------------------------------------------------------
009900130723
010000130723       // -?Dati utente?
010100130723     d §AzUte        e ds                  extname(AZUTE00F)
010200130723     d                                     dtaara
010300130723     d §DatiUte      e ds                  extname(dDatiUte)
010400130723     d                                     dtaara
010500130723
010600130723       //--------------------------------------------------------------
010700130723       //?Definizione strutture dati.                                  ?
010800130723       //--------------------------------------------------------------
010900130723
011000130723       // -?Status ds?
011100130723     d Status         sds
011200130723     d   SDSpgm          *proc
011300130723
011400130723       // -?InfDS?
011500130723     d InfDspF         ds
011600130723     d   dsp_aid             369    369a
011700130723
011800130723       // -?Indicatori su DspF?
011900130723     d IndDspF         ds                  inz
012000130723         // -?Abilitazione tasti funzionali?
012100130723     d   F5Attivo                      n   overlay(IndDspF : 05)
012200130723     d   F6Attivo                      n   overlay(IndDspF : 06)
012201151102     d   F7Attivo                      n   overlay(IndDspF : 07)
012202150526     d   F10Attivo                     n   overlay(IndDspF : 10)
012300130724     d   F12Attivo                     n   overlay(IndDspF : 12)
012400130723     d   F16Attivo                     n   overlay(IndDspF : 16)
012401150520         // -?Indicatori di gestione del subfile?
012500130723         // -?Emissione messaggio di errore?
012600130723     d   ErrMessage                    n   overlay(IndDspF : 28)
012700130723         // -?Indicatori per Attibuti di visualizzazione?
012800150526     d   $Ricercaksc                   n   overlay(IndDspF : 27)
012801151030        // - Indicatore di protezione campi
012802151030     d  Protlnpb                       n   overlay(IndDspF : 40)
012900130723         // -?Posizionamento cursore & segnalazione errore?
013000150518     d   PosCurLnp                     n   overlay(IndDspF : 51)
013100130723     d   PosCurKE2F                    n   overlay(IndDspF : 52)
013200150519     d   PosCurKsc                     n   overlay(IndDspF : 53)
013300130723     d   PosCurDES                     n   overlay(IndDspF : 54)
013400130723     d   PosCurSOD                     n   overlay(IndDspF : 55)
013401150504     d   PosCurLNPB                    n   overlay(IndDspF : 56)
013402150504     d   PosCurPDR                     n   overlay(IndDspF : 57)
013404150505     d   PosCurlp1                     n   overlay(IndDspF : 61)
013405150505     d   PosCurlp2                     n   overlay(IndDspF : 62)
013406150505     d   PosCurlp3                     n   overlay(IndDspF : 63)
013407150505     d   PosCurlp4                     n   overlay(IndDspF : 64)
013408150505     d   PosCurlp5                     n   overlay(IndDspF : 65)
013409150505     d   PosCurlp6                     n   overlay(IndDspF : 66)
013410150505     d   PosCurlp7                     n   overlay(IndDspF : 67)
013411150505     d   PosCurlp8                     n   overlay(IndDspF : 68)
013412150505     d   PosCurlp9                     n   overlay(IndDspF : 69)
013413150505     d   PosCurlp10                    n   overlay(IndDspF : 70)
013414150505     d   PosCurlp11                    n   overlay(IndDspF : 71)
013415150505     d   PosCurlp12                    n   overlay(IndDspF : 72)
013416150505     d   PosCurlp13                    n   overlay(IndDspF : 73)
013417150505     d   PosCurlp14                    n   overlay(IndDspF : 74)
013418150518     d   PosCurlp15                    n   overlay(IndDspF : 75)
013419150518     d   PosCurlp16                    n   overlay(IndDspF : 76)
013420150518     d   PosCurlp17                    n   overlay(IndDspF : 77)
013421150518     d   PosCurlp18                    n   overlay(IndDspF : 78)
013422150518     d   PosCurlp19                    n   overlay(IndDspF : 79)
013423150518     d   PosCurlp20                    n   overlay(IndDspF : 80)
013424150518     d   PosCurlp21                    n   overlay(IndDspF : 81)
013425150518     d   PosCurlp22                    n   overlay(IndDspF : 82)
013426150518     d   PosCurlp23                    n   overlay(IndDspF : 83)
013427150518     d   PosCurlp24                    n   overlay(IndDspF : 84)
013428150518     d   PosCurlp25                    n   overlay(IndDspF : 85)
013429150518     d   PosCurlp26                    n   overlay(IndDspF : 86)
013430150518     d   PosCurlp27                    n   overlay(IndDspF : 87)
013431150518     d   PosCurlp28                    n   overlay(IndDspF : 88)
013432150518     d   PosCurlp29                    n   overlay(IndDspF : 89)
013433150518     d   PosCurlp30                    n   overlay(IndDspF : 90)
013500130723         // -?Riemissione videata?
013600130723     d   ErrGenerico                   n   overlay(IndDspF : 99)
013700131212
014002150519     d campolpd02      ds            90
014003150504     d  v1clp1
014004150504     d  v1clp2
014005150504     d  v1clp3
014006150504     d  v1clp4
014007150504     d  v1clp5
014008150504     d  v1clp6
014009150504     d  v1clp7
014010150504     d  v1clp8
014011150504     d  v1clp9
014012150504     d  v1clp10
014013150504     d  v1clp11
014014150504     d  v1clp12
014015150504     d  v1clp13
014016150504     d  v1clp14
014017150518     d  v1clp15
014018150518     d  v1clp16
014019150518     d  v1clp17
014020150518     d  v1clp18
014021150518     d  v1clp19
014022150518     d  v1clp20
014023150518     d  v1clp21
014024150518     d  v1clp22
014025150518     d  v1clp23
014026150518     d  v1clp24
014027150518     d  v1clp25
014028150518     d  v1clp26
014029150518     d  v1clp27
014030150518     d  v1clp28
014031150518     d  v1clp29
014032150518     d  v1clp30
014033150518     d  slp                           3  0 dim(30) overlay(campolpd02)
014100130723
014200130723       // -?Parametri ricevuti?
014300130723     d KPJBA         e ds
014400130723
014900130723
015000130723       // -?Tabelle usate:?
015100130723       // ·?"BSP" = Bolle Sospensione?
015200130723     d dBSP          e ds                  inz  qualified
015300130724
015400130724       // -?148ª descrizione dell'organigramma?
015500130724     d Og148         e ds                  inz  qualified
015501150908       // -?143ª descrizione dell'organigramma?
015502150908     d Og143         e ds                  inz
015503150520
015504150520     D fnlv55ds      E DS
015505150525     D fnlsbsp1ds    E DS
015600130723
015700130723       //--------------------------------------------------------------
015800130723       //?Definizione variabili globali.                               ?
015900130723       //--------------------------------------------------------------
016000130723
016100130723       // -?Flags booleani?
016200130723     d $Fine           s               n   inz
016300130723     d $InzD01         s               n   inz(*on)
016400130723     d $InzD01b        s               n   inz(*on)
016500130723     d $InzD02         s               n   inz(*on)
016501150519     d $InzD03         s               n   inz(*on)
016600130723     d $InzW01         s               n   inz(*on)
016800130723     d $Immissione     s               n   inz
017000130723     d $Protect        s               n   inz
017100130723
017200130723       // -?Variabili per la gestione del video?
017300130723     d $Video          s              2    inz('D1')
017400131212
017500131212       // -?Nome esteso Libreria/File del file tabella?
017600131212     d wLibFile        s             21a   inz
017700130723
017800130723       // -?Campi di comodo?
018100131212     d wDate           s              8  0 inz
018200130723     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
018201150505     d w_numlnp        s              2  0 inz
018202150505     d wpdr            s                   like(apdpdr)
018203151030     d wntw            s                   like(§ogntw)
018205150505     d i               s              2  0 inz
018206150526     d iv              s              2  0 inz
018207150526     d ix              s              2  0 inz
018208150519     d w0030           s              3  0 inz
018209150519     d W0070           s              7  0 inz
018210150520     d s01nrr          s              4  0
018211150526     d wAbi            s                   like(UTEaut)  inz
018212151102     d wforzlnpb       s              1
018213151102     d wmsgSbm         s              1
018214151102     d*TotFil          s              3  0
018215151102     d*TotFilN         s              3  0
018300130723
018400130723       //--------------------------------------------------------------
018500130723       //?Definizione prototipi procedure.                             ?
018600130723       //--------------------------------------------------------------
019200130723
019300130723       // -?Reperimento dati utente?
019400130723     d TIBS34ds      e ds
019500130723      /copy gaitrasrc/srcProtoPR,TIBS34R
020200130724
020300130724       // -?Ricerca filiale in organigramma?
020400130724      /copy gaitrasrc/srcProtoPI,TNSD24R
020500130724      /copy gaitrasrc/srcProtoPR,TNSD24R
020600130724
020700130724       // -?Caricamento filiali gestite?
020800130724     d TRUL31ds      e ds                  inz
020900130724     d   $POg                 10    759    inz   dim(250)
021000130724      /copy gaitrasrc/srcProtoPr,TRUL31R
021100130724
021200130724       // -?Interrogazione sottoconti?
021300130724      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
021400130724      /copy gaitrasrc/srcProtoPR,XALFA3BR
021500130723
021600130723       // -?Controllo/Decodifica cliente?
021700130723      /copy gaitrasrc/srcProtoPI,TIBS69R
021800130723      /copy gaitrasrc/srcProtoPR,TIBS69R
021801150520      // - Reperimento terminal di arrivo/partenza
021802150520     d fnlv55r         pr                  extpgm('FNLV55R')
021803150520     d  fnlv55ds                           likeds(FNLV55ds)
021804150525      // -
021805150525     d fnlsbsp1r       pr                  extpgm('FNLSBSP1R')
021806150525     d  kpjba                              likeds(kpjba)
021807151102      // - Sottomissione lavoro batch
021808151102     d bch10           pr                  extpgm('BCH10')
021809151102     d  kpjba                              likeds(KPJBA)
021900130723
022000130723       //--------------------------------------------------------------
022100130723       //?Definizione key-list.                                        ?
022200130723       //--------------------------------------------------------------
022700130723
022800130723       //--------------------------------------------------------------
022900130723       //?Riepilogo indicatori utilizzati.                             ?
023000130723       //--------------------------------------------------------------
023100130723       //--------------------------------------------------------------
023200130723
023300130723       //--------------------------------------------------------------
023400130723       //?M A I N - L I N E                                            ?
023500130723       //--------------------------------------------------------------
023600130723
023700130723     c     *Entry        plist
023800130723     c                   parm                    KPJBA
023900130723
024000130723      /free
024001151211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
024100130723
024200130723       // -?Operazioni iniziali?
024300130723       exsr  sr_RoutInz;
024400130723
024500130723       // -?Ciclo di gestione del file video?
024600130723       DOW  $Fine = *off;
024700130723
024800130723         select;
024900130723
025000130723           // -?Richiesta chiave di accesso?
025100130723           when $Video = 'D1';
025200130723             exsr  sr_GesD01;
025201150518
025300150518           when $Video = 'D2';
025400150518             exsr  sr_GesD02 ;
025401150518
025801150518
025802150518           when $Video = '3A';
025803150518             exsr  sr_GesD03a;
025804150518
025805150518           when $Video = '3B';
025806150518             exsr  sr_GesD03b;
025900130723
026000130723           // -?? ? ??
026100130723           other;
026200130723             $Fine = *on;
026300130723
026400130723         endsl;
026500130723
026600130723       ENDDO;
026700130723
026800130723       // -?Operazioni finali?
026900130723       exsr  sr_RoutEnd;
027000130723
027100130723       //--------------------------------------------------------------
027200130723       //?Operazioni iniziali.                                         ?
027300130723       //--------------------------------------------------------------
027400130723       BEGSR  sr_RoutInz;
027500130723
027600130723         *inLR = *on;
027700130723
027800130723         // -?Reperimento dati job?
027900130723         exsr  sr_DatiJob;
028000130723
028100130723         // -?Impostazione nome programma a video?
028200130723         V1Tpgm = SDSpgm;
032600131212
032700131212         // -?Reperimento data odierna?
032800131212         wDate = %int( %subst( %char( %dec( %timestamp() ) )
032900131212                               : 1 : 8 ) );
033000130723
033100130723       ENDSR;
033200130723
033300130723       //--------------------------------------------------------------
033400130723       //?Reperimento Dati del job (Utente/Operativi).                 ?
033500130723       //--------------------------------------------------------------
033600130723       BEGSR  sr_DatiJob;
033700130723
033800130723         in(E) §AzUte;
033900130723         if NOT %error;
034000130723           in(E) §DatiUte;
034100130723         endif;
034200130723         if %error or RSut = *blanks;
034300130723           clear TIBS34ds;
034400130723           tibs34r ( tibs34ds );
034500130723           in §AzUte;
034600130723           in §DatiUte;
034700130723         endif;
034701150526         select;
034702150526            when uteaut=*blanks;
034703150526            if   dutlpo= '1';
034704150526               wabi='TP';
034705150526            endif;
034706150526            if   dutlpo= '2' ;
034707150526               wabi='PO';
034708150526            endif;
034709150526            other;
034710150526               wabi=uteaut;
034711150526         endsl;
034712150526         clear  trul31ds;
034714150526         I31abi = wabi ;
034715150526         I31cdi = DUTdis ;
034716150526         I31car = DUTare ;
034717150526         I31cpo = DUTpou  ;
034718150526         TRUL31R   (KPJBA:trul31ds)   ;
034800130723
034900130723       ENDSR;
035000130723
035100130723       //--------------------------------------------------------------
035200130723       //?Gestione videata D01.                                        ?
035300130723       //--------------------------------------------------------------
035400130723       BEGSR  sr_GesD01;
035500130723
035600130723         // -?Inizializzazione videata?
035700130723         if  $InzD01 = *on;
035801150518           clear  TBBSPD01;
035900130723           $InzD01 = *off;
036000130723         endif;
036100130723         clear  V1Topz;
036200130724
036300130724         F12Attivo = *off;
036301151102         F7Attivo = *off;
036302150526         F10Attivo = *on;
036400130723
036500130723         // -?Emissione videata?
036600130723         write  TBBSPT01;
036700130723         write  TBBSPP01;
036800130723         exfmt  TBBSPD01;
036900130723
037000130723         clear  V1Dmsg;
037100130723         reset  ErrMessage;
037200130723         reset  ErrGenerico;
037300130723
037400130723         SELECT;
037500130723
037600130723           // -?F3=Fine?
037700130723           WHEN  dsp_aid = c_F03;
037800130723             $Fine = *on;
037801150526           // -?F10=Ricerca alfabetica
037802150526           WHEN  dsp_aid = c_F10;
037803150526             $ricercaksc=*on;
037900130723
038000130723           // -?Invio?
038100130723           OTHER;
038200130723             exsr  sr_CtrD01;
038300130723             if  ErrGenerico = *off;
039602150518                 $Video   = 'D2';
039700150518                 $InzD02  = *on;
039900130723             endif;
040000130723
040100130723         ENDSL;
040200130723
040300130723       ENDSR;
040400130723
040500130723       //--------------------------------------------------------------
040600130723       //?Inizializzazione videata D01.                                ?
040700130723       //--------------------------------------------------------------
040800130723       BEGSR  sr_InzD01;
040900130723
041300130723
041500130723
041600130723       ENDSR;
041700130723
041800130723       //--------------------------------------------------------------
041900130723       //?Controllo dati immessi nella videata D01.                    ?
042000130723       //--------------------------------------------------------------
042100130723       BEGSR  sr_CtrD01;
042200130723
042201151030         clear wntw;
042202151030         clear wforzlnpb;
042300130723         // -?Spegnimento indicatori di posizionamento cursore?
042400130723         %subst(IndDspF : 28) = *off;
042402150526         // -?Ricerca alfabetica codice cliente
042403150526         if (v1cksc=*zeros or v1cksc=*blanks)
042404150526                          and $ricercaksc=*on and v1dksc<>*blanks ;
042405150526             xParDut = RSut;
042406150526             xParKut = 2;
042407150526             xParRag = V1Dksc ;
042408150526             xParKcc = DUTkci;
042409150526             //xParSta = *zero;     //?Anche clienti bloccati o annullati?
042410150526             xParSta = 9;         //?Solo clienti attivi?
042411150526             //clear  xParFlr;      //?Filiali in cui cercare (max 30)?
042412150526             //xParFlr = V1Cke1;    //?Filiali in cui cercare (max 30)?
042413150526             xParFlr = O31pog;    //?Filiali in cui cercare (max 30)?
042414150526             clear  xParDit;
042415150526             xParNum = 1;
042416150526             clear  xParKcm;
042417150526             clear  xParKsm;
042418150526             clear  xParKdm;
042419150526             clear  xParEsci;
042420150526             clear  xParErr;
042421150526             clear  xParIva;
042422150526             clear  xParCdf;
042423150526             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
042424150526                        xParSta : xParFlr : xParDit : xParNum :
042425150526                        xParKcm : xParKsm : xParKdm : xParEsci :
042426150526                        xParErr : xParIva : xParCdf );
042427150526             if  xParSta <> -1   and   xParErr = *blank;
042428150526               V1Cksc  = xParKsm;
042429150526               V1Dksc  = xParRag;
042430150526               $Ricercaksc  = *off;
042431150526             endif;
042432150526             ErrGenerico = *on;
042433150526             leavesr;
042434150526         endif;
042500130723
042501150518         // -?Linea di Partenza OPPURE Codice Cliente
042502150518         if (v1clnp>*zeros and v1cksc>*zeros) or
042503150518            ((v1clnp=*zeros or v1clnp=*blanks) and
042504150518             (v1cksc=*zeros or v1cksc=*blanks) ) ;
042505150518            V1Dmsg = $Msg(01);
042506150518            PosCurLnp=*on  ;
042507150518            ErrMessage  = *on;
042508150518            ErrGenerico = *on;
042509150518            leavesr;
042510150518         endif;
042511150518         // -?Controlli Linea di Partenza
042512150518         if v1clnp<>*blanks and v1clnp<>*zeros ;
042513150518         // -?Ricerca?
042514150518            If  %scan( '?' : V1Clnp ) > *zero;
042515150525              clear fnlsbsp1ds;
042516150525              ibsptps='P';
042517150525              kpjbu=fnlsbsp1ds;
042518150525              fnlsbsp1r(kpjba);
042519150525              fnlsbsp1ds=kpjbu;
042521150525              eval v1clnp=%editc(obsplin:'X');
042527150518              ErrGenerico=*on;
042528150518              PosCurLnp=*on  ;
042529150518              leavesr;
042533150518            EndIf;
042534150518       // Deve essere numerico
042535150518           if    %check( c_Digits : v1clnp ) > *zero;
042536150518            V1Dmsg = $Msg(04);
042537150518             PosCurLnp = *on;
042538150518             ErrGenerico = *on;
042539150518             ErrMessage =  *on;
042540150518             leavesr;
042541150518           endif;
042542150518        // controlla esistenza del codice
042543150518           if v1clnp <>*zeros;
042544150518              chain  (%int(V1Clnp))  AZORG;
042547150518
042548150518              if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
042549150519                 V1dmsg = $Msg(04);
042550150518                 PosCurLnp = *on;
042551150518                 ErrGenerico = *on;
042552150518                 ErrMessage =  *on;
042553150518                 leavesr;
042554150518              endif;
042555150518
042556150518              v1dlnp  = ORGdes;
042557151030              og143   = orgde3;
042558151030              wntw    = §ogntw;
042559150518           endif;
042560150518         endif;
042561150518         // -?Controlli Codice Cliente
042562150518         if v1cksc<>*blanks and v1cksc<>*zeros ;
042563150518         // -?Ricerca?
042564150518            If  %scan( '?' : V1Cksc ) > *zero;
042565150525              clear fnlsbsp1ds;
042566150525              ibsptps='C';
042567150525              kpjbu=fnlsbsp1ds;
042568150525              fnlsbsp1r(kpjba);
042569150525              fnlsbsp1ds=kpjbu;
042570150525              eval v1cksc=%editc(obspcli:'X');
042571150518              ErrGenerico=*on;
042572150518              PosCurKsc=*on  ;
042573150518              leavesr;
042574150518            EndIf;
042575150518       // Deve essere numerico
042576150518           if    %check( c_Digits : v1cksc) > *zero;
042577150518            V1Dmsg = $Msg(06);
042578150518             PosCurKsc = *on;
042579150518             ErrGenerico = *on;
042580150518             ErrMessage =  *on;
042581150518             leavesr;
042582150518           endif;
042583150518        // controlla esistenza del codice
042584150518           if v1cksc <>*zeros;
042585150519              clear tibs69ds;
042586150518              clear DS_cnaco;
042587150518              clear DS_cnind;
042588150518              clear DS_cnclp;
042589150518              clear DS_fncls;
042590150518              I69kac=%int(v1cksc) ;
042591150518              tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
042592150518              if o69err= *on    ;
042593150519                 V1dmsg = $Msg(06);
042594150518                 PosCurKsc = *on;
042595150518                 ErrGenerico = *on;
042596150518                 ErrMessage =  *on;
042597150518                 leavesr;
042598150518              endif;
042599150518
042600150518              v1dksc  = acorag;
042601150518           endif;
042602150518         endif;
049300130723
049400130723       ENDSR;
049401150518       //--------------------------------------------------------------
049402150518       //?Gestione videata D02                                 ?
049403150518       //--------------------------------------------------------------
049404150518       BEGSR  sr_GesD02;
049405150521
049406150521         F12Attivo = *on;
049407151102         F7Attivo = *on;
049408150526         F10Attivo = *off;
049409150521
049410150518         // -?Inizializzazione videata?
049411150518         if  $InzD02 = *on;
049412150519           reset  $Immissione;
049413150519         // -?Pulizia videata?
049414150518           clear  TBBSPD02;
049415150518           clear  V1Topz;
049416150518           if v1clnp>*zeros;
049417150519               v1cdes=v1dlnp;
049418150518               w0030=%int(v1clnp);
049419150518               chain  w0030 fibsp01l;
049420150518           else;
049421150518               w0070=%int(v1cksc);
049422150519               v1cdes=v1dksc;
049423150518               chain  w0070 fibsp02l;
049424150518           endif;
049425150518           if %found   ;
049426150518               v1topz='   MODIFICA    ';
049427150518               v1cdes=bspdes   ;
049428150518               V1CKLNPB=bspfpa   ;
049429150518           else;
049430150519               v1topz='  INSERIMENTO  ';
049431150519               $Immissione = *on;
049432150518           endif;
049433151030         // Proteggo campo "Cliente con colli in part. da più Filiali"
049434151102         // e spengo indicatore di pos. cursore
049435151030           Protlnpb= *on;
049436151102           poscurlnpb= *off;
049437150518           $InzD02 = *off;
049438150518         endif;
049439150518         // -?Emissione videata?
049440150518         write  TBBSPT01;
049441150518         if v1clnp>*zeros ;
049442150518            write  TBBSPd01f;
049443150518         else;
049444150518            write  TBBSPd01C;
049445150518         endif;
049446150519         write  TBBSPP01;
049447150518         exfmt  TBBSPD02;
049448150518
049449150518         clear  V1Dmsg;
049450150518         reset  ErrMessage;
049451150518         reset  ErrGenerico;
049452150518
049453150518         SELECT;
049454150518
049455150518           // -?F3=Fine?
049456150518           WHEN  dsp_aid = c_F03;
049457150518             $Fine = *on;
049458150518           // -?F12=Ritorno
049459150518           WHEN  dsp_aid = c_F12;
049460150518             $video = 'D1';
049461151030           // -?F7 =Modifica --> sproteggo campo opzione "colli in part.da +fil"
049462151030           WHEN  dsp_aid = c_F07;
049463151030             protlnpb=*off;
049464151102             poscurlnpb= *on;
049465150518
049466150518           // -?Invio?
049467150518           OTHER;
049468150520              if v1cdes=*blanks;
049469150520                 v1cdes=bspdes;
049470150520              endif;
049471151030           // Erore forzabile
049472151030           // Se gestione per linea non DPD e inserito "S" nell'opzione
049473151030              if not *in40 and v1cklnpB='S' and  v1clnp>*zeros and wntw<>'DPD'
049474151030              and wforzlnpb=' ';
049475151030                 wforzlnpb='1';
049476151030                 poscurlnpb= *on ;
049477151030                 ErrMessage  = *on;
049478151030                 ErrGenerico = *on;
049479151030                 v1dmsg=$msg(19);
049480151030              endif;
049481151030              if ErrGenerico = *off;
049482151030                 if V1CKLNPB='S';
049483151030                    $Video   = '3B';
049484151030                 else;
049485151030                    $Video   = '3A';
049486151030                 endif;
049487150518                 $InzD03  = *on;
049488151030              endif;
049489150518
049490150518         ENDSL;
049491150518       endsr;
049492150519       //--------------------------------------------------------------
049493150519       //?Gestione videata D03A
049494150519       //--------------------------------------------------------------
049495150519       BEGSR  sr_GesD03A;
049496150521
049497150525         //f16attivo=*off;
049498150521
049499150519         // -?Inizializ.videata?
049500150519         if  $InzD03  = *on;
049501150519           clear  TBBSPD03A;
049502150525           f16attivo=*off;
049503150519           if $immissione = *off;
049504150521              f16attivo=*on;
049505150519              v1csod=BSPDCRP;
049506150519              v1caut=BSPaut ;
049507150520              if %check(c_digits:bsplin)>0 or v1clnp>*zeros;
049508150519                 campolpd02=*zeros;
049509150519              else;
049510150519                 campolpd02 = bsplin;
049511150519              endif;
049512150519           endif;
049513150519           $InzD03  = *off;
049514150519         endif;
049515150519         // -?Emissione videata?
049516150519         write  TBBSPT01;
049517150519         if v1clnp>*zeros;
049518150519            write TBBSPD01F;
049519150519         else;
049520150519            write TBBSPD01C;
049521150519         endif;
049522150519         write  TBBSPD02O;
049523150519         write  TBBSPP02 ;
049524150520         if v1clnp>*zeros;
049525150520            exfmt  TBBSPD03c;
049526150520         else;
049527150520            exfmt  TBBSPD03A;
049528150520         endif;
049529150519
049530150519         clear  V1Dmsg;
049531150519         reset  ErrMessage;
049532150519         reset  ErrGenerico;
049533150519
049534150519         SELECT;
049535150519
049536150519           // -?F3=Fine?
049537150519           WHEN  dsp_aid = c_F03;
049538150519             $Fine = *on;
049539150519           // -?F12=Ritorno
049540150519           WHEN  dsp_aid = c_F12;
049541150519             $video = 'D2';
049542150519
049543150519           // -?Invio?
049544150519           OTHER;
049545150519             exsr  sr_CtrD03A;
049546150519             if  ErrGenerico       ;
049547150519                leavesr;
049548150519             endif;
049549150519             if  dsp_aid = c_F06   or
049550150519                 dsp_aid = c_F16;
049551150519                 exsr  sr_scrivi;
049552150519                 $Video  = 'D1';
049553150519                 $InzD01 = *on;
049554150519             endif;
049555150519
049556150519         ENDSL;
049557150519       endsr;
049558150519       //--------------------------------------------------------------
049559150519       //?Gestione videata D03B  - Clienti con colli in part. da + fil
049560150519       //--------------------------------------------------------------
049561150519       BEGSR  sr_GesD03B;
049562150519         if  $InzD03  = *on;
049563150519           clear  TBBSPD03B;
049564150525           f16attivo=*off;
049565151102         //clear TotFil;
049566150519           if $immissione = *off;
049567150521              f16attivo=*on ;
049568150519              v1cpdr=BSPpdr ;
049569151214              v1caut=BSPaut ;
049570150519              if %check(c_digits:bsplin)>0;
049571150519                 campolpd02=*zeros;
049572150519              else;
049573150519                 campolpd02 = bsplin;
049574151102         // Memorizzo il numero delle filiali già presenti
049575151102         //      TotFil=%lookup(000:slp);
049576151102         //      if TotFil>0;
049577151102         //         TotFil-=1;
049578151102         //      endif;
049579150519              endif;
049580150519           endif;
049581150519           $InzD03  = *off;
049582150519         endif;
049583150519         // -?Emissione videata?
049584150519         write  TBBSPT01;
049585150519         if v1clnp>*zeros;
049586150519            write TBBSPD01F;
049587150519         else;
049588150519            write TBBSPD01C;
049589150519         endif;
049590150519         write  TBBSPD02O;
049591150519         write  TBBSPP02 ;
049592150519         exfmt  TBBSPD03B;
049593150519
049594150519         clear  V1Dmsg;
049595150519         reset  ErrMessage;
049596150519         reset  ErrGenerico;
049597150519
049598150519         SELECT;
049599150519
049600150519           // -?F3=Fine?
049601150519           WHEN  dsp_aid = c_F03;
049602150519             $Fine = *on;
049603150519           // -?F12=Ritorno
049604150519           WHEN  dsp_aid = c_F12;
049605150519             $video = 'D2';
049606150519
049607150519           // -?Invio?
049608150519           OTHER;
049609150519             exsr  sr_CtrD03B;
049610150519             if  ErrGenerico       ;
049611150519                leavesr;
049612150519             endif;
049613150519             if  dsp_aid = c_F06   or
049614150519                 dsp_aid = c_F16;
049615150519                 exsr  sr_Scrivi;
049616150519                 $Video  = 'D1';
049617150519                 $InzD01 = *on;
049618150519             endif;
049619150519
049620150519         ENDSL;
049621150519       endsr;
094601150519       //--------------------------------------------------------------
094602150519       //?Controllo dati videata    .                                  ?
094603150519       //--------------------------------------------------------------
094604150519       BEGSR  sr_CtrD03A;
094605150520
094606150521         // -?Spegnimento indicatori di posizionamento cursore?
094607150521         %subst(IndDspF : 28) = *off;
094608150521
094609150520       // Controllo dei terminal abilitati non vale in caso di inserimento
094610150520       // per linea di partenza. Quindi controllo solo in caso di gestione per
094611150520       // Codice cliente
094612150520
094613150520       IF V1cksc>*zeros;
094614150520       // terminal di partenza abilitati
094615150520                  clear w_numlnp;
094616150520            for i=1 to %elem(slp);
094617150520               if slp(i) >0;
094618150520                  reset fnlv55ds;
094619150520                  D55lin = slp(i) ;
094620150520                  d55drf = wdate  ;
094621150520                  d55tpt = 'P';
094622150520                  callp fnlv55r(fnlv55ds);
094623150520                  if d55err <> ' ';
094624150520                       V1Dmsg = $Msg(14);
094625150520                       %subst(IndDspF : i+60 : 1) = *on;
094626150520                       ErrMessage  = *on;
094627150520                       ErrGenerico = *on;
094628150520                       leavesr;
094629150520                  endif;
094630150520         //       Errore se ripetuto
094631150520                       if %lookup(slp(i):slp:1:i-1)>0;
094632150520                          V1Dmsg = $Msg(16);
094633150520                          %subst(IndDspF : i+60 : 1) = *on;
094634150520                          ErrMessage  = *on;
094635150520                          ErrGenerico = *on;
094636150520                          leavesr;
094637150520                       endif ;
094638150520                  w_numlnp+=1;
094639150520               endif;
094640150520            endfor;
094641150520         // - errore se non immesso almeno un tfp
094642150520              if w_numlnp=0 ;
094643150520                       V1Dmsg = $Msg(15);
094644150520                       PosCurLp1   = *on;
094645150520                       ErrMessage  = *on;
094646150520                       ErrGenerico = *on;
094647150520                       leavesr;
094648150520              endif;
094649150520       endif;
094650150519       ENDSR;
094700131212
094701150519       //--------------------------------------------------------------
094702150519       //?Controllo dati videata    .                                  ?
094703150519       //--------------------------------------------------------------
094704150519       BEGSR  sr_CtrD03B;
094705150521
094706150521         // -?Spegnimento indicatori di posizionamento cursore?
094707150521         %subst(IndDspF : 28) = *off;
094708150521
094709150521         // -?Controllo lnp abilitate ?
094710150521                  clear w_numlnp;
094711150521            for i=1 to %elem(slp);
094712150521               if slp(i) >0;
094713150521                  chain  slp(i)  AZORG;
094714150521                  if   not %found(azorg01l) or
094715150521                       (orgfag<>'F' and orgfag<>'A') or
094716150521                       orgfva<>*blanks;
094717150521                       V1Dmsg = $Msg(04);
094718150521                       %subst(IndDspF : i+60 : 1) = *on;
094719150521                       ErrMessage  = *on;
094720150521                       ErrGenerico = *on;
094721150521                       leavesr;
094722150521                  endif;
094723150521         //       Errore se lnp ripetuta
094724150521                       if %lookup(slp(i):slp:1:i-1)>0;
094725150521                          V1Dmsg = $Msg(13);
094726150521                          %subst(IndDspF : i+60 : 1) = *on;
094727150521                          ErrMessage  = *on;
094728150521                          ErrGenerico = *on;
094729150521                          leavesr;
094730150521                       endif ;
094731150521                  w_numlnp+=1;
094732150521               endif;
094733150521            endfor;
094734150521         // - errore se non immesse almeno due lnp
094735150521              if w_numlnp<=1;
094736150521                       V1Dmsg = $Msg(11);
094737150521                       PosCurLp1   = *on;
094738150521                       ErrMessage  = *on;
094739150521                       ErrGenerico = *on;
094740150521                       leavesr;
094741150521              endif;
094742150521         // - codice aut deve esistere per ogni lnp immessa
094743150908         //   Se record per lnp (no cod. cliente) il codice aut deve
094744150908         //   esistere per la "fil.valida come lnp bolla" corrispondente
094745150908         //   ad ogni lnp immessa
094746150521            for i=1 to %elem(slp);
094747150521               if slp(i) >0;
094748150908                if v1clnp>*zeros;
094749150908                  clear orgde3;
094750150908                  chain slp(i) azorg01l;
094751150908                  og143=orgde3  ;
094752150908                  if §oglnpb>*zeros ;
094753150908                     wpdr=%int(§oglnpb)*10000+v1cpdr;
094754150908                     chain ('A': wpdr) fiapd01l;
094755150908                     if not %found(fiapd01l) or apdfcm= *blanks;
094756150908                       V1Dmsg = $Msg(18);
094757150908                       v1dmsg = %trim(v1dmsg)+' '+ §oglnpb            ;
094758150908                       PosCurPDR   = *on;
094759150908                       ErrMessage  = *on;
094760150908                       ErrGenerico = *on;
094761150908                       leavesr;
094762150908                     endif;
094763150908                  else;
094764150908                       V1Dmsg = $Msg(17);
094765150908                       v1dmsg = %trim(v1dmsg)+' '+ %editc(slp(i):'X') ;
094766150908                       PosCurPDR   = *on;
094767150908                       ErrMessage  = *on;
094768150908                       ErrGenerico = *on;
094769150908                       leavesr;
094770150908                  endif;
094771150908                else;
094772150521                  wpdr=slp(i)*10000+v1cpdr;
094773150521                  chain ('A': wpdr) fiapd01l;
094774150521                  if not %found(fiapd01l) or apdfcm= *blanks;
094775150521                       V1Dmsg = $Msg(10);
094776150521                       v1dmsg = %trim(v1dmsg)+' '+ %editc(slp(i):'X') ;
094777150521                       PosCurPDR   = *on;
094778150521                       ErrMessage  = *on;
094779150521                       ErrGenerico = *on;
094780150521                       leavesr;
094781150521                  endif;
094782150908                endif;
094783150521               endif;
094784150521            endfor;
094785150519       ENDSR;
094875150519       //--------------------------------------------------------------
094876150519       //?Controllo dati videata    .                                  ?
094877150519       //--------------------------------------------------------------
094878150519       BEGSR  sr_scrivi ;
094879150519         // -?Aggiornamento FIBSP     ?
094880150519         SELECT;
094881150519
094882150519
094883150519           // -?F16=Annullamento?
094884150519           WHEN  dsp_aid = c_F16 ;
094885150519               //_______________
094886150519               if v1clnp>*zeros;
094887150519                  delete  fibsp000;
094888150519               else;
094889151211          // se presenti,
094890151211          // prima cancello tutti i record del cliente su FIBSP10F
094891151211                  w0070=%int(v1cksc);
094892151211                  exec sql delete from fibsp10f where bs1ksc=:w0070;
094893151211
094894150519                  delete  fibsp02 ;
094895150519               endif;
094896150519               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
094897150519
094898150519           // -?Immissione o Modifica?
094899150519           OTHER;
094900150520             exsr  sr_RieBsp   ;
094901150519             // -?Aggiornamento record?
094902150519             if  $immissione=*off;
094903150519               if v1clnp>*zeros;
094904150519                  //_____________
094905150519                  UPDATE fibsp000;
094906150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
094907150519               else;
094908150519                  //_____________
094909150519                  UPDATE fibsp02 ;
094910150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
094911150519               endif;
094912150519             else;
094913150519               if v1clnp>*zeros;
094914150519                  //_____________
094915150519                  WRITE  fibsp000;
094916150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
094917150519               else;
094918150519                  //_____________
094919150519                  WRITE  fibsp02 ;
094920150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
094921150519               endif;
094922150519             endif;
094923150519
094924150519         ENDSL;
094925150519
094926150519       ENDSR;
094927150519       //--------------------------------------------------------------
094928150519       //?                                                             ?
094929150519       //--------------------------------------------------------------
094930150519       BEGSR  sr_RieBsp ;
094931151102        clear wMsgSbm;
094932150520        if $immissione=*on;
094933150520           clear fibsp000;
094934150520           if v1clnp>*zeros;
094935150520              bspfil=%int(v1clnp);
094936150520           else;
094937150520              bspksc=%int(v1cksc);
094938150520           endif;
094939150520        endif;
094940150519        BSPDES=v1cdes   ;
094941150519        BSPFPA=v1cklnpb ;
094942150526       // Compatto l'elenco delle filiali inserite mettendo vicine quelle piene
094943150526        clear iv;
094944151102       //clear TotFilN;
094945150526        for i=1 to %elem(slp);
094946150526       // Primo elemento vuoto
094947150526           if slp(i)=0 and iv=0;
094948150526              iv=I;
094949150526           endif;
094950150526       // Primo elemento pieno dopo il vuoto
094951150526           if slp(i)>0 and iv>0;
094952150526       // Schifto verso sinisitra
094953150526              %subarr(slp:iv)=%subarr(slp:i);
094954150526       // Pulisco gli ultimi elementi in numero uguale al
094955150526       // numero di elementi che ho ricoperto perchè vuoti
094956150526              ix=%elem(slp)-(i-iv);
094957150526              %subarr(slp:ix+1)=*zeros;
094958150526              i=iv;
094959150526              iv=0;
094960150526           endif;
094961151102       // Memorizzo il totale delle filiali inserite
094962151102       //  if slp(i)>0;
094963151102       //     TotFilN=i;
094964151102       //  endif;
094965150526        endfor;
094966151102       // Se in partenza da più filiali verifico se aggiunte nuove linee
094967151102        if BSPFPA='S' and  bsplin<>campolpd02;
094968151102           eval wmsgSbm='S';
094969151102        endif;
094970150519        BSPLIN=campolpd02;
094971151214        BSPAUT  = v1caut ;
094972150519        if bspfpa='S';
094973150519           BSPPDR = v1cpdr;
094974151214       //    clear bspaut;
094975150521           clear bspdcrp;
094976150519        else;
094977150519           BSPDCRP =  v1csod;
094978151214       //  BSPAUT  = v1caut ;
094979150521           clear bsppdr;
094980150519        endif;
094981150519        BSPDUV  = wdate                ;
094982150519       endsr;
111500130723       //--------------------------------------------------------------
111600130723       //?Operazioni finali.                                           ?
111700130723       //--------------------------------------------------------------
111800130723       BEGSR  sr_RoutEnd;
111900130723
111901151102         // Se aggiunte nuove filiali emissione window di
111902151102         // richiesta per sottomissione duplica fisgn per
111903151102         // recuperare anche i record già presenti
111904151102         // Solo per EDP
111905151102         if   wmsgSbm='S' and %subst(knmus:1:3)='EDP';
111906151102             $fine=*off;
111907151102             dow $fine=*off;
111908151102                exfmt tbbspw01;
111909151102                if  dsp_aid = c_F06;
111910151102                   if w01sbm='S';
111911151102                      kcoaz='LBSP';
111912151102                      BCH10(kpjba);
111913151102                   endif;
111914151102                      $fine=*on;
111915151102                endif ;
111916151102             enddo;
111917151102         endif;
112000130723         // -?Chiusura funzioni precedentemente aperte?
112400130723
112500130723         reset  TIBS69ds;
112600130723         tibs69r( tibs69ds :
112700130723                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
112800130723
112900130723         //  ?Uscita?
113000130723         return;
113100130723
113200130723       ENDSR;
113300130723
113400130723      /end-free
113500130723
113600130723       //--------------------------------------------------------------
113700130723       //?Definizione schiere a tempo di compilazione.                 ?
113800130723       //--------------------------------------------------------------
114300130723** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
114400150521Immettere la Linea di Partenza OPPURE il Codice Cliente
114500130723Ammessi solo caratteri numerici
114600130723Filiale obbligatoria
114700130726Filiale errata
114800130723Cliente obbligatorio
114900130723Cliente errato
115000130726Descrizione obbligatoria
115100130723Valore non ammesso nel campo: immettere " " o "S"
115200150504Scelta incompatibile con la richiesta di non gestire nella Sospensione automatic
115300150908Codice Aut. inesistente o non valido per la linea di part.
115400150505Inserire almeno due "Filiali abilitate"
115500150505Richiesta valida solo se Cliente con colli i partenza da più Filiali='S'
115600150505Filiale già immessa
115700150520Terminal di Partenza errato
115800150520Immettere almeno un Terminal di Partenza
115900150520Terminal di Partenza già presente
116000150908Manca indicazione "Fil. valida come LnP Bolla" su organigramma per l.part.
116100150908Codice Aut. inesistente o non valido per la "Fil. valida come LnP Bolla"
116200151030Richiesta partenza da più filiali per Network non DPD. ENTER per Forzare
