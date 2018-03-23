000100091021      *PARMS dftactgrp(*no) actgrp('QILE')
000200091021       //--------------------------------------------------------------
000300091021       //?TEST - Ricerca avanzata Clienti Unificanti                   ?
000400091021       //--------------------------------------------------------------
000500091021
000600091021     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700091021     h dftactgrp(*no)
000800091021
000900091021       //--------------------------------------------------------------
001000091021       //?Dichiarazione file.                                          ?
001100091021       //--------------------------------------------------------------
001200091021
001300091021       // - Video
001400091021     fTIBS10R1TDcf   e             workstn
001500091021     f                                     indds(IndDspF)
001600091021     f                                     infds(InfDspF)
001700091021
001800091021       //--------------------------------------------------------------
001900091021       //?Definizione costanti.                                        ?
002000091021       //--------------------------------------------------------------
002100091021
002200091021       // - Tasti funzionali a video
002300091021     d c_F01           c                   const(x'31')
002400091021     d c_F02           c                   const(x'32')
002500091021     d c_F03           c                   const(x'33')
002600091021     d c_F04           c                   const(x'34')
002700091021     d c_F05           c                   const(x'35')
002800091021     d c_F06           c                   const(x'36')
002900091021     d c_F07           c                   const(x'37')
003000091021     d c_F08           c                   const(x'38')
003100091021     d c_F09           c                   const(x'39')
003200091021     d c_F10           c                   const(x'3A')
003300091021     d c_F11           c                   const(x'3B')
003400091021     d c_F12           c                   const(x'3C')
003500091021     d c_F13           c                   const(x'B1')
003600091021     d c_F14           c                   const(x'B2')
003700091021     d c_F15           c                   const(x'B3')
003800091021     d c_F16           c                   const(x'B4')
003900091021     d c_F17           c                   const(x'B5')
004000091021     d c_F18           c                   const(x'B6')
004100091021     d c_F19           c                   const(x'B7')
004200091021     d c_F20           c                   const(x'B8')
004300091021     d c_F21           c                   const(x'B9')
004400091021     d c_F22           c                   const(x'BA')
004500091021     d c_F23           c                   const(x'BB')
004600091021     d c_F24           c                   const(x'BC')
004700091021     d c_Enter         c                   const(x'F1')
004800091021     d c_RollDown      c                   const(x'F4')
004900091021     d c_RollUp        c                   const(x'F5')
005000091021
005100091021       //--------------------------------------------------------------
005200091021       //?Definizione schiere.                                         ?
005300091021       //--------------------------------------------------------------
005400091021
005500091021
005600091021       //--------------------------------------------------------------
005700091021       //?Definizione aree dati.                                       ?
005800091021       //--------------------------------------------------------------
005900091021
006000091021       // - Dati utente
006100091021     d §AzUte        e ds                  extname(AZUTE00F)
006200091021     d                                     dtaara
006300091021     d §DatiUte      e ds                  extname(dDatiUte)
006400091021     d                                     dtaara
006500091021
006600091021       //--------------------------------------------------------------
006700091021       //?Definizione strutture dati.                                  ?
006800091021       //--------------------------------------------------------------
006900091021
007000091021       // - Status ds
007100091021     d Status         sds
007200091021     d  SDSpgm           *proc
007300091021     d  SDSprm           *parms
007400091021     d  SDSusr               254    263
007500091021
007600091021       // - InfDS
007700091021     d InfDspF         ds
007800091021     d  dsp_aid              369    369a
007900091021
008000091021       // - Indicatori su DspF
008100091021     d IndDspF         ds
008200091021        // - Abilitazione tasti funzionali
008300091021     d  AbilitF06                      n   overlay(IndDspF : 06)
008400091021     d  AbilitF12                      n   overlay(IndDspF : 12)
008500091021     d  AbilitRlUp                     n   overlay(IndDspF : 25)
008600091021     d  AbilitRlDWn                    n   overlay(IndDspF : 26)
008700091021        // - Indicatori di errore
008800091021     d  ErrMessage                     n   overlay(IndDspF : 28)
008900091021     d  ErrGenerico                    n   overlay(IndDspF : 99)
009000091021
009100091021       // - Parametri x Controllo profilo utenti
009200091021     d TIBS34ds      e ds
009300091021
009400091021       // - Parametri ricevuti
009500091021     d kpjba         e ds
009600091021
009700091021       // - Parametri per Clienti unificanti - Ricerca "avanzata"
009800091021      /copy gaitrasrc/srcProtoPi,TIBS10R1
009900091021
010000091021       // - Ridefinizione campi a video
010100091021     d ds_SkC          ds                  inz
010200091021     d   V10SkC01                          inz
010300091021     d   V10SkC02                          inz
010400091021     d   V10SkC03                          inz
010500091021     d   V10SkC04                          inz
010600091021     d   V10SkC05                          inz
010700091021     d   V10SkC06                          inz
010800091021     d   V10SkC07                          inz
010900091021     d   V10SkC08                          inz
011000091021     d   V10SkC09                          inz
011100091021     d   V10SkC10                          inz
011200091021     d   V10SkC11                          inz
011300091021     d   V10SkC12                          inz
011400091021     d   V10SkC13                          inz
011500091021     d   V10SkC14                          inz
011600091021     d   V10SkC15                          inz
011700091021     d   V10SkC16                          inz
011800091021     d   V10SkC17                          inz
011900091021     d   V10SkC18                          inz
012000091021     d   V10SkC19                          inz
012100091021     d   V10SkC20                          inz
012200091021     d   V10SkC21                          inz
012300091021     d   V10SkC22                          inz
012400091021     d   V10SkC23                          inz
012500091021     d   V10SkC24                          inz
012600091021     d   V10SkC25                          inz
012700091021     d   V10SkC26                          inz
012800091021     d   V10SkC27                          inz
012900091021     d   V10SkC28                          inz
013000091021     d   V10SkC29                          inz
013100091021     d   V10SkC30                          inz
013200091021     d ds_SkCOP        ds                  inz
013300091021     d   V10SkCop01                        inz
013400091021     d   V10SkCop02                        inz
013500091021     d   V10SkCop03                        inz
013600091021     d   V10SkCop04                        inz
013700091021     d   V10SkCop05                        inz
013800091021     d   V10SkCop06                        inz
013900091021     d   V10SkCop07                        inz
014000091021     d   V10SkCop08                        inz
014100091021     d   V10SkCop09                        inz
014200091021     d   V10SkCop10                        inz
014300091021     d   V10SkCop11                        inz
014400091021     d   V10SkCop12                        inz
014500091021     d   V10SkCop13                        inz
014600091021     d   V10SkCop14                        inz
014700091021     d   V10SkCop15                        inz
014800091021     d   V10SkCop16                        inz
014900091021     d   V10SkCop17                        inz
015000091021     d   V10SkCop18                        inz
015100091021     d   V10SkCop19                        inz
015200091021     d   V10SkCop20                        inz
015300091021     d   V10SkCop21                        inz
015400091021     d   V10SkCop22                        inz
015500091021     d   V10SkCop23                        inz
015600091021     d   V10SkCop24                        inz
015700091021     d   V10SkCop25                        inz
015800091021     d   V10SkCop26                        inz
015900091021     d   V10SkCop27                        inz
016000091021     d   V10SkCop28                        inz
016100091021     d   V10SkCop29                        inz
016200091021     d   V10SkCop30                        inz
016300091021     d ds_SkTLE        ds                  inz
016400091021     d   V10SkTle01                        inz
016500091021     d   V10SkTle02                        inz
016600091021     d   V10SkTle03                        inz
016700091021     d   V10SkTle04                        inz
016800091021     d   V10SkTle05                        inz
016900091021     d   V10SkTle06                        inz
017000091021     d   V10SkTle07                        inz
017100091021     d   V10SkTle08                        inz
017200091021     d   V10SkTle09                        inz
017300091021     d   V10SkTle10                        inz
017400091021     d   V10SkTle11                        inz
017500091021     d   V10SkTle12                        inz
017600091021     d   V10SkTle13                        inz
017700091021     d   V10SkTle14                        inz
017800091021     d   V10SkTle15                        inz
017900091021     d   V10SkTle16                        inz
018000091021     d   V10SkTle17                        inz
018100091021     d   V10SkTle18                        inz
018200091021     d   V10SkTle19                        inz
018300091021     d   V10SkTle20                        inz
018400091021     d   V10SkTle21                        inz
018500091021     d   V10SkTle22                        inz
018600091021     d   V10SkTle23                        inz
018700091021     d   V10SkTle24                        inz
018800091021     d   V10SkTle25                        inz
018900091021     d   V10SkTle26                        inz
019000091021     d   V10SkTle27                        inz
019100091021     d   V10SkTle28                        inz
019200091021     d   V10SkTle29                        inz
019300091021     d   V10SkTle30                        inz
019400091021
019500091021       //--------------------------------------------------------------
019600091021       //?Definizione variabili globali.                               ?
019700091021       //--------------------------------------------------------------
019800091021
019900091021       // - Flags booleani
020000091021     d $Fine           s               n   inz(*off)
020100091021     d $InzD01         s               n   inz(*on)
020200091021     d $InzD02         s               n   inz(*on)
020300091021
020400091021       // - Campi per la gestione del video
020500091021     d $Video          s              2a   inz('D1')
020600091021
020700091021       //--------------------------------------------------------------
020800091021       //?Definizione prototipi procedure usate.                       ?
020900091021       //--------------------------------------------------------------
021000091021
021100091021       // - Reperimento dati utente
021200091021      /copy gaitrasrc/srcProtoPr,TIBS34R
021300091021
021400091021       // - Clienti unificanti - Ricerca "avanzata"
021500091021      /copy gaitrasrc/srcProtoPr,TIBS10R1
021600091021
021700091021       //--------------------------------------------------------------
021800091021       //?Definizione key-list.                                        ?
021900091021       //--------------------------------------------------------------
022000091021
022100091021
022200091021       //--------------------------------------------------------------
022300091021       //?Riepilogo indicatori.                                        ?
022400091021       //--------------------------------------------------------------
022500091021
022600091021
022700091021       //--------------------------------------------------------------
022800091021       //?M A I N - L I N E                                            ?
022900091021       //--------------------------------------------------------------
023000091021
023100091021     c     *entry        plist
023200091021     c                   parm                    KPJBA
023300091021
023400091021      /free
023500091021
023600091021       // - Start
023700091021       *inLR = *on;
023800091021
023900091021       // - Elab
024000091021       dow  $Fine = *off;
024100091021         select;
024200091021           when  $Video = 'D1';
024300091021             exsr sr_GesD01;
024400091021           when  $Video = 'D2';
024500091021             exsr sr_GesD02;
024600091021           when  $Video = 'D3';
024700091021             exsr sr_GesD03;
024800091021           when  $Video = 'D4';
024900091021             exsr sr_GesD04;
025000091021           other;
025100091021             $Fine = *on;
025200091021         endsl;
025300091021       enddo;
025400091021
025500091021       // - End
025600091021       return;
025700091021
025800091021       //--------------------------------------------------------------
025900091021       //?Operazioni iniziali                                          ?
026000091021       //--------------------------------------------------------------
026100091021       BEGSR  *InzSR;
026200091021
026300091021         // - Reperimento dati utente
026400091021         exsr  sr_DatiJob;
026500091021
026600091021         // - Impostazione nome programma in testata a video
026700091021         V1Tpgm = SDSpgm;
026800091021
026900091021       ENDSR;
027000091021
027100091021       //--------------------------------------------------------------
027200091021       //?Reperimento Dati del job (Utente/Operativi).                 ?
027300091021       //--------------------------------------------------------------
027400091021       BEGSR  sr_DatiJob;
027500091021
027600091021         in(E) §AzUte;
027700091021         if NOT %error;
027800091021           in(E) §DatiUte;
027900091021         endif;
028000091021         if %error or RSut = *blanks;
028100091021           clear TIBS34ds;
028200091021           tibs34r(tibs34ds);
028300091021           in §AzUte;
028400091021           in §DatiUte;
028500091021         endif;
028600091021
028700091021       ENDSR;
028800091021
028900091021       //--------------------------------------------------------------
029000091021       //?Gestione videata D01                                         ?
029100091021       //--------------------------------------------------------------
029200091021       BEGSR  sr_GesD01;
029300091021
029400091021         // - Inizializzazione videata
029500091021         if  $InzD01 = *on;
029600091021           clear  TIBS101d1;
029700091021           V10drf    = %dec(%date());
029800091021           $InzD01 = *off;
029900091021         endif;
030000091021         AbilitF06 = *on;
030100091021         AbilitF12 = *off;
030200091021         AbilitRlUp  = *off;
030300091021         AbilitRlDwn = *off;
030400091021
030500091021         // - Emissione videata
030600091021         if  ErrMessage = *off;
030700091021           write  TIBS101t1;
030800091021           write  TIBS101z1;
030900091021         endif;
031000091021
031100091021         exfmt  TIBS101d1;
031200091021
031300091021         reset  ErrMessage;
031400091021         reset  ErrGenerico;
031500091021         clear  V1Dmsg;
031600091021
031700091021         SELECT;
031800091021
031900091021           // - F3=Fine
032000091021           WHEN  dsp_aid = c_F03;
032100091021             $Fine = *on;
032200091021
032300091021           // - Invio o F6=Conferma
032400091021           OTHER;
032500091021             exsr sr_CtrD01;
032600091021             select;
032700091021               // - rilevato errore
032800091021               when  ErrGenerico = *on;
032900091021               // - non premuto F6
033000091021               when  dsp_aid <> c_F06;
033100091021               // - premuto F6
033200091021               other;
033300091021                 $Video = 'D2';
033400091021                 reset  $InzD02;
033500091021             endsl;
033600091021
033700091021         ENDSL;
033800091021
033900091021       ENDSR;
034000091021
034100091021       //--------------------------------------------------------------
034200091021       //?Controllo dati in videata D01                                ?
034300091021       //--------------------------------------------------------------
034400091021       BEGSR sr_CtrD01;
034500091021
034600091021         %subst(IndDspF : 28) = *off;
034700091021
034800091021       ENDSR;
034900091021
035000091021       //--------------------------------------------------------------
035100091021       //?Gestione videata D02                                         ?
035200091021       //--------------------------------------------------------------
035300091021       BEGSR  sr_GesD02;
035400091021
035500091021         // - Inizializzazione videata
035600091021         if  $InzD02 = *on;
035700091021           clear  TIBS101d2;
035800091021           clear  TIBS101d3;
035900091021           clear  TIBS101d4;
036000091021           exsr  Call_Tibs10r1;
036100091021           $InzD02  = *off;
036200091021           AbilitF06  = *off;
036300091021           AbilitF12  = *on;
036400091021         endif;
036500091021         AbilitRlUp  = *on;
036600091021         AbilitRlDwn = *off;
036700091021
036800091021         // - Emissione videata
036900091021         if  ErrMessage = *off;
037000091021           write  TIBS101t1;
037100091021           write  TIBS101z1;
037200091021           write  TIBS101d1;
037300091021           write  Protect;
037400091021         endif;
037500091021
037600091021         exfmt  TIBS101d2;
037700091021
037800091021         reset  ErrMessage;
037900091021         reset  ErrGenerico;
038000091021         clear  V1Dmsg;
038100091021
038200091021         SELECT;
038300091021
038400091021           // - F3=Fine
038500091021           WHEN  dsp_aid = c_F03;
038600091021             $Fine = *on;
038700091021
038800091021           // - F12=Ritorno
038900091021           WHEN  dsp_aid = c_F12;
039000091021             $Video = 'D1';
039100091021             //reset $InzD01;
039200091021             reset $InzD02;
039300091021
039400091021           // - RollUp=FmtD3
039500091021           WHEN  dsp_aid = c_RollUp;
039600091021             $Video = 'D3';
039700091021
039800091021           // - Invio
039900091021           OTHER;
040000091021
040100091021         ENDSL;
040200091021
040300091021       ENDSR;
040400091021
040500091021       //--------------------------------------------------------------
040600091021       //?Gestione videata D03                                         ?
040700091021       //--------------------------------------------------------------
040800091021       BEGSR  sr_GesD03;
040900091021
041000091022         AbilitRlUp  = *on;
041100091022         AbilitRlDwn = *on;
041200091022
041300091021         // - Emissione videata
041400091021         if  ErrMessage = *off;
041500091021           write  TIBS101t1;
041600091021           write  TIBS101z1;
041700091021           write  TIBS101d1;
041800091021           write  Protect;
041900091021         endif;
042000091021
042100091021         exfmt  TIBS101d3;
042200091021
042300091021         reset  ErrMessage;
042400091021         reset  ErrGenerico;
042500091021         clear  V1Dmsg;
042600091021
042700091021         SELECT;
042800091021
042900091021           // - F3=Fine
043000091021           WHEN  dsp_aid = c_F03;
043100091021             $Fine = *on;
043200091021
043300091021           // - F12=Ritorno
043400091021           WHEN  dsp_aid = c_F12;
043500091021             $Video = 'D1';
043600091021             //reset $InzD01;
043700091021             reset $InzD02;
043800091021
043900091021           // - RollUp=FmtD4
044000091021           WHEN  dsp_aid = c_RollUp;
044100091021             $Video = 'D4';
044200091021
044300091021           // - RollDown=FmtD2
044400091021           WHEN  dsp_aid = c_RollDown;
044500091021             $Video = 'D2';
044600091021
044700091021           // - Invio
044800091021           OTHER;
044900091021
045000091021         ENDSL;
045100091021
045200091021       ENDSR;
045300091021
045400091021       //--------------------------------------------------------------
045500091021       //?Gestione videata D04                                         ?
045600091021       //--------------------------------------------------------------
045700091021       BEGSR  sr_GesD04;
045800091021
045900091022         AbilitRlUp  = *off;
046000091022         AbilitRlDwn = *on;
046100091022
046200091021         // - Emissione videata
046300091021         if  ErrMessage = *off;
046400091021           write  TIBS101t1;
046500091021           write  TIBS101z1;
046600091021           write  TIBS101d1;
046700091021           write  Protect;
046800091021         endif;
046900091021
047000091021         exfmt  TIBS101d4;
047100091021
047200091021         reset  ErrMessage;
047300091021         reset  ErrGenerico;
047400091021         clear  V1Dmsg;
047500091021
047600091021         SELECT;
047700091021
047800091021           // - F3=Fine
047900091021           WHEN  dsp_aid = c_F03;
048000091021             $Fine = *on;
048100091021
048200091021           // - F12=Ritorno
048300091021           WHEN  dsp_aid = c_F12;
048400091021             $Video = 'D1';
048500091021             //reset $InzD01;
048600091021             reset $InzD02;
048700091021
048800091021           // - RollDown=FmtD3
048900091021           WHEN  dsp_aid = c_RollDown;
049000091021             $Video = 'D3';
049100091021
049200091021           // - Invio
049300091021           OTHER;
049400091021
049500091021         ENDSL;
049600091021
049700091021       ENDSR;
049800091021
049900091021       //--------------------------------------------------------------
050000091021       //?Richiamo programma TIBS10R1                                  ?
050100091021       //--------------------------------------------------------------
050200091021       BEGSR Call_Tibs10r1;
050300091021
050400091021         clear  TIBS10R1ds;
050500091021
050600091021         tibs10r1ds.tibs10ds.D10tle = V10tle;
050700091021         tibs10r1ds.tibs10ds.D10paf = V10paf;
050800091021         tibs10r1ds.tibs10ds.D10cod = V10cod;
050900091021         tibs10r1ds.tibs10ds.D10drf = V10drf;
051000091021         tibs10r1ds.D10isun    = V10sun;
051100091021         tibs10r1ds.D10iksu    = V10ksu;
051200091021         tibs10r1ds.D10igrptle = V10grptle;
051300091021         tibs10r1ds.D10ifamily = V10family;
051400091021
051500091021         tibs10r1(TIBS10R1ds);
051600091021
051700091021         V10cop   = tibs10r1ds.tibs10ds.D10cop;
051800091021         V10err   = tibs10r1ds.tibs10ds.D10err;
051900091021         V10msg   = tibs10r1ds.tibs10ds.D10msg;
052000091021         ds_SkC   = %subst( tibs10r1ds.tibs10ds.D10SkC
052100091021                                                 : 1 : %size(ds_SkC) );
052200091021         ds_SkCOP = %subst( tibs10r1ds.D10oLstCop : 1 : %size(ds_SkCOP) );
052300091021         ds_SkTLE = %subst( tibs10r1ds.D10oLstTle : 1 : %size(ds_SkTLE) );
052400091021
052500091021       ENDSR;
052600091021
052700091021      /end-free
