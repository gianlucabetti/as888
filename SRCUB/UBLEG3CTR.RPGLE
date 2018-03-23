000100100203     /*END
000200100203       //--------------------------------------------------------------
000300100203       //?TEST - Reperimento legami in tab. "3C"                       ?
000400100203       //--------------------------------------------------------------
000500100203
000600100203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700100203     h dftactgrp(*no) actgrp('QILE') bnddir('UBBNDDIR')
000800100203
000900100203       //--------------------------------------------------------------
001000100203       //?Dichiarazione file.                                          ?
001100100203       //--------------------------------------------------------------
001200100203
001300100203       // - Video
001400100203     fUBLEG3CTD cf   e             workstn
001500100203     f                                     indds(IndDspF)
001600100203     f                                     infds(InfDspF)
001700100203
001800100203       //--------------------------------------------------------------
001900100203       //?Definizione costanti.                                        ?
002000100203       //--------------------------------------------------------------
002100100203
002200100203       // - Tasti funzionali a video
002300100203     d c_F01           c                   const(x'31')
002400100203     d c_F02           c                   const(x'32')
002500100203     d c_F03           c                   const(x'33')
002600100203     d c_F04           c                   const(x'34')
002700100203     d c_F05           c                   const(x'35')
002800100203     d c_F06           c                   const(x'36')
002900100203     d c_F07           c                   const(x'37')
003000100203     d c_F08           c                   const(x'38')
003100100203     d c_F09           c                   const(x'39')
003200100203     d c_F10           c                   const(x'3A')
003300100203     d c_F11           c                   const(x'3B')
003400100203     d c_F12           c                   const(x'3C')
003500100203     d c_F13           c                   const(x'B1')
003600100203     d c_F14           c                   const(x'B2')
003700100203     d c_F15           c                   const(x'B3')
003800100203     d c_F16           c                   const(x'B4')
003900100203     d c_F17           c                   const(x'B5')
004000100203     d c_F18           c                   const(x'B6')
004100100203     d c_F19           c                   const(x'B7')
004200100203     d c_F20           c                   const(x'B8')
004300100203     d c_F21           c                   const(x'B9')
004400100203     d c_F22           c                   const(x'BA')
004500100203     d c_F23           c                   const(x'BB')
004600100203     d c_F24           c                   const(x'BC')
004700100203     d c_Enter         c                   const(x'F1')
004800100203     d c_RollDown      c                   const(x'F4')
004900100203     d c_RollUp        c                   const(x'F5')
005000100203
005100100203       //--------------------------------------------------------------
005200100203       //?Definizione schiere.                                         ?
005300100203       //--------------------------------------------------------------
005400100203
005500100203       // - Codici clienti figli
005600100203     d ds_SKC          ds          3200    inz
005700100203     d   sch_SKC                      7p 0 dim(800)  inz
005800100203
005900100203       // - Flags cliente abilitato ad EasyWEB
006000100203     d ds_SKEW         ds           800    inz
006100100203     d   sch_SKEW                      n   dim(800)  inz
006200100203
006300100203       //--------------------------------------------------------------
006400100203       //?Definizione aree dati.                                       ?
006500100203       //--------------------------------------------------------------
006600100203
006700100203       // - Dati utente
006800100203     d §AzUte        e ds                  extname(AZUTE00F)
006900100203     d                                     dtaara
007000100203     d §DatiUte      e ds                  extname(dDatiUte)
007100100203     d                                     dtaara
007200100203
007300100203       //--------------------------------------------------------------
007400100203       //?Definizione strutture dati.                                  ?
007500100203       //--------------------------------------------------------------
007600100203
007700100203       // - Status ds
007800100203     d Status         sds
007900100203     d  SDSpgm           *proc
008000100203     d  SDSprm           *parms
008100100203     d  SDSusr               254    263
008200100203
008300100203       // - InfDS
008400100203     d InfDspF         ds
008500100203     d  dsp_aid              369    369a
008600100203
008700100203       // - Indicatori su DspF
008800100203     d IndDspF         ds
008900100203        // - Abilitazione tasti funzionali
009000120723     d  AbilitF06                      n   overlay(IndDspF : 06)
009100120723     d  AbilitF12                      n   overlay(IndDspF : 12)
009200120723     d  AbilitRollUp                   n   overlay(IndDspF : 25)
009300120723     d  AbilitRollDwn                  n   overlay(IndDspF : 26)
009400100203        // - Indicatori di errore
009500120723     d  ErrMessage                     n   overlay(IndDspF : 28)
009600120723     d  ErrGenerico                    n   overlay(IndDspF : 99)
009700100203
009800100203       // - Parametri x Controllo profilo utenti
009900100203     d TIBS34ds      e ds
010000100203
010100100203       // - Ridefinizione parametri a video
010200100203     d ds_V2SKC        ds                  inz
010300100203     d   V2skc01                           inz
010400100203     d   V2skc02                           inz
010500100203     d   V2skc03                           inz
010600100203     d   V2skc04                           inz
010700100203     d   V2skc05                           inz
010800100203     d   V2skc06                           inz
010900100203     d   V2skc07                           inz
011000100203     d   V2skc08                           inz
011100100203     d   V2skc09                           inz
011200100203     d   V2skc10                           inz
011300100203     d   V2skc11                           inz
011400100203     d   V2skc12                           inz
011500100203     d   V2skc13                           inz
011600100203     d   V2skc14                           inz
011700100203     d   V2skc15                           inz
011800100203     d   V2skc16                           inz
011900100203     d   V2skc17                           inz
012000100203     d   V2skc18                           inz
012100100203     d   V2skc19                           inz
012200100203     d   V2skc20                           inz
012300100203     d   V2skc21                           inz
012400100203     d   V2skc22                           inz
012500100203     d   V2skc23                           inz
012600100203     d   V2skc24                           inz
012700100203     d   V2skc25                           inz
012800100203     d   V2skc26                           inz
012900100203     d   V2skc27                           inz
013000100203     d   V2skc28                           inz
013100100203     d   V2skc29                           inz
013200100203     d   V2skc30                           inz
013300100203     d   V2skc31                           inz
013400100203     d   V2skc32                           inz
013500100203     d   V2skc33                           inz
013600100203     d   V2skc34                           inz
013700100203     d   V2skc35                           inz
013800100203     d   V2skc36                           inz
013900100203     d   V2skc37                           inz
014000100203     d   V2skc38                           inz
014100100203     d   V2skc39                           inz
014200100203     d   V2skc40                           inz
014300100203     d   V2skc41                           inz
014400100203     d   V2skc42                           inz
014500100203     d   V2skc43                           inz
014600100203     d   V2skc44                           inz
014700100203     d   V2skc45                           inz
014800100203     d   V2skc46                           inz
014900100203     d   V2skc47                           inz
015000100203     d   V2skc48                           inz
015100100203     d   V2skc49                           inz
015200100203     d   V2skc50                           inz
015300100203     d   V2skc51                           inz
015400100203     d   V2skc52                           inz
015500100203     d   V2skc53                           inz
015600100203     d   V2skc54                           inz
015700100203     d   V2skc55                           inz
015800100203     d   V2skc56                           inz
015900100203     d   V2skc57                           inz
016000100203     d   V2skc58                           inz
016100100203     d   V2skc59                           inz
016200100203     d   V2skc60                           inz
016300100203     d   V2skc61                           inz
016400100203     d   V2skc62                           inz
016500100203     d   V2skc63                           inz
016600100203     d   V2skc64                           inz
016700100203     d   V2skc65                           inz
016800100203     d   V2skc66                           inz
016900100203     d     $skc                1    462  0 inz  dim(66)
017000100203      *
017100100203     d ds_V2SKEW       ds                  inz
017200100203     d   V2skew01                          inz
017300100203     d   V2skew02                          inz
017400100203     d   V2skew03                          inz
017500100203     d   V2skew04                          inz
017600100203     d   V2skew05                          inz
017700100203     d   V2skew06                          inz
017800100203     d   V2skew07                          inz
017900100203     d   V2skew08                          inz
018000100203     d   V2skew09                          inz
018100100203     d   V2skew10                          inz
018200100203     d   V2skew11                          inz
018300100203     d   V2skew12                          inz
018400100203     d   V2skew13                          inz
018500100203     d   V2skew14                          inz
018600100203     d   V2skew15                          inz
018700100203     d   V2skew16                          inz
018800100203     d   V2skew17                          inz
018900100203     d   V2skew18                          inz
019000100203     d   V2skew19                          inz
019100100203     d   V2skew20                          inz
019200100203     d   V2skew21                          inz
019300100203     d   V2skew22                          inz
019400100203     d   V2skew23                          inz
019500100203     d   V2skew24                          inz
019600100203     d   V2skew25                          inz
019700100203     d   V2skew26                          inz
019800100203     d   V2skew27                          inz
019900100203     d   V2skew28                          inz
020000100203     d   V2skew29                          inz
020100100203     d   V2skew30                          inz
020200100203     d   V2skew31                          inz
020300100203     d   V2skew32                          inz
020400100203     d   V2skew33                          inz
020500100203     d   V2skew34                          inz
020600100203     d   V2skew35                          inz
020700100203     d   V2skew36                          inz
020800100203     d   V2skew37                          inz
020900100203     d   V2skew38                          inz
021000100203     d   V2skew39                          inz
021100100203     d   V2skew40                          inz
021200100203     d   V2skew41                          inz
021300100203     d   V2skew42                          inz
021400100203     d   V2skew43                          inz
021500100203     d   V2skew44                          inz
021600100203     d   V2skew45                          inz
021700100203     d   V2skew46                          inz
021800100203     d   V2skew47                          inz
021900100203     d   V2skew48                          inz
022000100203     d   V2skew49                          inz
022100100203     d   V2skew50                          inz
022200100203     d   V2skew51                          inz
022300100203     d   V2skew52                          inz
022400100203     d   V2skew53                          inz
022500100203     d   V2skew54                          inz
022600100203     d   V2skew55                          inz
022700100203     d   V2skew56                          inz
022800100203     d   V2skew57                          inz
022900100203     d   V2skew58                          inz
023000100203     d   V2skew59                          inz
023100100203     d   V2skew60                          inz
023200100203     d   V2skew61                          inz
023300100203     d   V2skew62                          inz
023400100203     d   V2skew63                          inz
023500100203     d   V2skew64                          inz
023600100203     d   V2skew65                          inz
023700100203     d   V2skew66                          inz
023800100203     d     $skew               1     66    inz  dim(66)
023900100203
024000100203       //--------------------------------------------------------------
024100100203       //?Definizione variabili globali.                               ?
024200100203       //--------------------------------------------------------------
024300100203
024400100203       // - Flags booleani
024500100203     d $Fine           s               n   inz(*off)
024600100203     d $InzD01         s               n   inz(*on)
024700100203     d $InzD02         s               n   inz(*on)
024800100203
024900100203       // - Indici di schiera
025000100203     d xx              s              5  0 inz
025100120723     d yy              s              5  0 inz
025200100203
025300100203       // - Campi per la gestione del video
025400100203     d $Video          s              2a   inz('D1')
025500120723     d wPage           s              3  0 inz
025600100203
025700100203       //--------------------------------------------------------------
025800100203       //?Definizione prototipi procedure usate.                       ?
025900100203       //--------------------------------------------------------------
026000100203
026100100203       // - Reperimento dati utente
026200100203      /copy gaitrasrc/srcProtoPr,TIBS34R
026300100203
026400100203       // - Reperimento legami in tab. "3C"
026500100203      /copy gaitrasrc/srcProtoPr,UBLEG3C
026600100203
026700100203       //--------------------------------------------------------------
026800100203       //?Definizione key-list.                                        ?
026900100203       //--------------------------------------------------------------
027000100203
027100100203
027200100203       //--------------------------------------------------------------
027300100203       //?Riepilogo indicatori.                                        ?
027400100203       //--------------------------------------------------------------
027500100203
027600100203
027700100203       //--------------------------------------------------------------
027800100203       //?M A I N - L I N E                                            ?
027900100203       //--------------------------------------------------------------
028000100203
028100100203      /free
028200100203
028300100203       // - Start
028400100203       *inLR = *on;
028500100203       if  ubLeg3C_Open() < *zero;
028600100203         ErrMessage  = *on;
028700100203         ErrGenerico = *on;
028800100203         V1Dmsg = 'Rilevato errore in fase di apertura archivi +
028900100203                   dal Service Program';
029000100203         write ubLEG3Ct1;
029100100203         write ubLEG3Cz1;
029200100203         exfmt ubLEG3Cd1;
029300100203         $Fine = *on;
029400100203       endif;
029500100203
029600100203       // - Elab
029700100203       dow  $Fine = *off;
029800100203         select;
029900100203           when  $Video = 'D1';
030000100203             exsr sr_GesD01;
030100100203           when  $Video = 'D2';
030200100203             exsr sr_GesD02;
030300100203           other;
030400100203             $Fine = *on;
030500100203         endsl;
030600100203       enddo;
030700100203
030800100203       // - End
030900100203       ubLeg3C_Close();
031000100203       return;
031100100203
031200100203       //--------------------------------------------------------------
031300100203       //?Operazioni iniziali                                          ?
031400100203       //--------------------------------------------------------------
031500100203       BEGSR  *InzSR;
031600100203
031700100203         // - Reperimento dati utente
031800100203         exsr sr_DatiJob;
031900100203
032000100203         // - Impostazione nome programma in testata a video
032100100203         //VTDpgm = SDSpgm;
032200100203
032300100203       ENDSR;
032400100203
032500100203       //--------------------------------------------------------------
032600100203       //?Reperimento Dati del job (Utente/Operativi).                 ?
032700100203       //--------------------------------------------------------------
032800100203       BEGSR  sr_DatiJob;
032900100203
033000100203         in(E) §AzUte;
033100100203         if NOT %error;
033200100203           in(E) §DatiUte;
033300100203         endif;
033400100203         if %error or RSut = *blanks;
033500100203           clear TIBS34ds;
033600100203           tibs34r(tibs34ds);
033700100203           in §AzUte;
033800100203           in §DatiUte;
033900100203         endif;
034000100203
034100100203       ENDSR;
034200100203
034300100203       //--------------------------------------------------------------
034400100203       //?Gestione videata D01                                         ?
034500100203       //--------------------------------------------------------------
034600100203       BEGSR  sr_GesD01;
034700100203
034800100203         // - Inizializzazione videata
034900100203         if  $InzD01 = *on;
035000100203           clear ubLEG3Cd1;
035100100203           $InzD01 = *off;
035200100203         endif;
035300100203         AbilitF06  = *on;
035400100203         AbilitF12  = *off;
035500100203
035600100203         // - Emissione videata
035700100203         if  ErrMessage = *off;
035800100203           write ubLEG3Ct1;
035900100203           write ubLEG3Cz1;
036000100203         endif;
036100100203
036200100203         exfmt ubLEG3Cd1;
036300100203
036400100203         reset ErrMessage;
036500100203         reset ErrGenerico;
036600100203         clear V1Dmsg;
036700100203
036800100203         SELECT;
036900100203
037000100203         // - F3=Fine
037100100203           WHEN  dsp_aid = c_F03;
037200100203             $Fine = *on;
037300100203
037400100203         // - Invio o F6=Conferma
037500100203           OTHER;
037600100203             exsr sr_CtrD01;
037700100203             select;
037800100203               // - rilevato errore
037900100203               when  ErrGenerico = *on;
038000100203               // - non premuto F6
038100100203               when  dsp_aid <> c_F06;
038200100203               // - premuto F6
038300100203               other;
038400100203                 $Video = 'D2';
038500100203                 reset  $InzD02;
038600100203             endsl;
038700100203
038800100203         ENDSL;
038900100203
039000100203       ENDSR;
039100100203
039200100203       //--------------------------------------------------------------
039300100203       //?Controllo dati in videata D01                                ?
039400100203       //--------------------------------------------------------------
039500100203       BEGSR sr_CtrD01;
039600100203
039700100203         %subst(IndDspF : 28) = *off;
039800100203
039900100203       ENDSR;
040000100203
040100100203       //--------------------------------------------------------------
040200100203       //?Gestione videata D02                                         ?
040300100203       //--------------------------------------------------------------
040400100203       BEGSR  sr_GesD02;
040500100203
040600100203         // - Inizializzazione videata
040700100203         if  $InzD02 = *on;
040800100203           clear  ubLEG3Cd2;
040900100203           exsr  Call_ubLEG3C;
041000100203           $InzD02  = *off;
041100100203           AbilitF06  = *off;
041200100203           AbilitF12  = *on;
041300100203         endif;
041400120723
041500120723         AbilitRollUp  = ( sch_SkC( wPage * %elem($skc) ) > *zero );
041600120723         AbilitRollDwn = ( wPage > 1);
041700100203
041800100203         // - Emissione videata
041900100203         if  ErrMessage = *off;
042000100203           write ubLEG3Ct1;
042100100203           write ubLEG3Cz1;
042200100203           write ubLEG3Cd1;
042300100203           write Protect;
042400100203         endif;
042500100203
042600100203         exfmt ubLEG3Cd2;
042700100203
042800100203         reset ErrMessage;
042900100203         reset ErrGenerico;
043000100203         clear V1Dmsg;
043100100203
043200100203         SELECT;
043300100203
043400100203           // - F3=Fine
043500100203           WHEN  dsp_aid = c_F03;
043600100203             $Fine = *on;
043700100203
043800100203           // - F12=Ritorno
043900100203           WHEN  dsp_aid = c_F12;
044000100203             $Video = 'D1';
044100100203             //reset $InzD01;
044200100203             reset $InzD02;
044300120723
044400120723           // - RollUp
044500120723           WHEN  dsp_aid = c_RollUp;
044600120723             wPage += 1;
044700120723             exsr  sr_PagSkC;
044800120723
044900120723           // - RollDown
045000120723           WHEN  dsp_aid = c_RollDown;
045100120723             wPage -= 1;
045200120723             exsr  sr_PagSkC;
045300100203
045400100203           // - Invio
045500100203           OTHER;
045600100203
045700100203         ENDSL;
045800100203
045900100203       ENDSR;
046000100203
046100100203       //--------------------------------------------------------------
046200100203       //?Richiamo Service Program ubLeg3C                             ?
046300100203       //--------------------------------------------------------------
046400120723       BEGSR  Call_ubLEG3C;
046500100203
046600100203         clear  sch_SkC;
046700100203         clear  sch_SkEW;
046800100203
046900100203         V2Cesito = ubLeg3C_Rtv ( V1Cksc :
047000100203                                  V2flgpf : V2padre :
047100100203                                  ds_SkC : ds_SkEW );
047200120723
047300120723         wPage = 1;
047400120723         exsr  sr_PagSkC;
047500120723
047600120723       ENDSR;
047700100203
047800120723       //--------------------------------------------------------------
047900120723       //?Visualizzazione pagina di clienti "in famiglia"              ?
048000120723       //--------------------------------------------------------------
048100120723       BEGSR  sr_PagSkC;
048200120723
048300120723         yy = ( (wPage - 1) * %elem($SkC) );
048400120723
048500120723         For  xx = 1  To  %elem($SkC);
048600120723           yy       += 1;
048700120723           $SkC(xx)  = sch_SkC(yy);
048800120723           $SkEW(xx) = sch_SkEW(yy);
048900100203         EndFor;
049000100203
049100100203       ENDSR;
049200100203
049300100203      /end-free
