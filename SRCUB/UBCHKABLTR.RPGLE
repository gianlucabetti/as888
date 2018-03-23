000100121207       //==============================================================
000200121207       //?TEST - Reperimento flag BLOCCO clienti.                      ?
000300121207       //==============================================================
000400121207
000500121207       //--------------------------------------------------------------
000600121207       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121207       //--------------------------------------------------------------
000800121207
000900121207     /*END
001000121207
001100121207       //--------------------------------------------------------------
001200121207       //?Specifiche di controllo.                                     ?
001300121207       //--------------------------------------------------------------
001400100203
001500100203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100203     h dftactgrp(*no) actgrp('QILE') bnddir('UBBNDDIR')
001700100203
001800100203       //--------------------------------------------------------------
001900100203       //?Dichiarazione file.                                          ?
002000100203       //--------------------------------------------------------------
002100100203
002200121207       // -?Video?
002300121207     fUBCHKABLTDcf   e             workstn
002400100203     f                                     indds(IndDspF)
002500100203     f                                     infds(InfDspF)
002600100203
002700100203       //--------------------------------------------------------------
002800100203       //?Definizione costanti.                                        ?
002900100203       //--------------------------------------------------------------
003000100203
003100121207       // -?Tasti funzionali a video?
003200100203     d c_F01           c                   const(x'31')
003300100203     d c_F02           c                   const(x'32')
003400100203     d c_F03           c                   const(x'33')
003500100203     d c_F04           c                   const(x'34')
003600100203     d c_F05           c                   const(x'35')
003700100203     d c_F06           c                   const(x'36')
003800100203     d c_F07           c                   const(x'37')
003900100203     d c_F08           c                   const(x'38')
004000100203     d c_F09           c                   const(x'39')
004100100203     d c_F10           c                   const(x'3A')
004200100203     d c_F11           c                   const(x'3B')
004300100203     d c_F12           c                   const(x'3C')
004400100203     d c_F13           c                   const(x'B1')
004500100203     d c_F14           c                   const(x'B2')
004600100203     d c_F15           c                   const(x'B3')
004700100203     d c_F16           c                   const(x'B4')
004800100203     d c_F17           c                   const(x'B5')
004900100203     d c_F18           c                   const(x'B6')
005000100203     d c_F19           c                   const(x'B7')
005100100203     d c_F20           c                   const(x'B8')
005200100203     d c_F21           c                   const(x'B9')
005300100203     d c_F22           c                   const(x'BA')
005400100203     d c_F23           c                   const(x'BB')
005500100203     d c_F24           c                   const(x'BC')
005600100203     d c_Enter         c                   const(x'F1')
005700100203     d c_RollDown      c                   const(x'F4')
005800100203     d c_RollUp        c                   const(x'F5')
005900100203
006000100203       //--------------------------------------------------------------
006100100203       //?Definizione schiere.                                         ?
006200100203       //--------------------------------------------------------------
006300100203
006400100203
006500100203       //--------------------------------------------------------------
006600100203       //?Definizione aree dati.                                       ?
006700100203       //--------------------------------------------------------------
006800100203
006900121207       // -?Dati utente?
007000100203     d §AzUte        e ds                  extname(AZUTE00F)
007100100203     d                                     dtaara
007200100203     d §DatiUte      e ds                  extname(dDatiUte)
007300100203     d                                     dtaara
007400100203
007500100203       //--------------------------------------------------------------
007600100203       //?Definizione strutture dati.                                  ?
007700100203       //--------------------------------------------------------------
007800100203
007900121207       // -?Status ds?
008000100203     d Status         sds
008100121207     d   SDSpgm          *proc
008200121207     d   SDSprm          *parms
008300121207     d   SDSusr              254    263
008400100203
008500121207       // -?InfDS?
008600100203     d InfDspF         ds
008700121207     d   dsp_aid             369    369a
008800100203
008900121207       // -?Indicatori su DspF?
009000100203     d IndDspF         ds
009100121207        // -?Abilitazione tasti funzionali?
009200121207     d  AbilitF06                      n   overlay(IndDspF : 06)
009300121207     d  AbilitF12                      n   overlay(IndDspF : 12)
009400121207     d  AbilitRollUp                   n   overlay(IndDspF : 25)
009500121207     d  AbilitRollDwn                  n   overlay(IndDspF : 26)
009600121207        // -?Emissione messaggio di errore?
009700121207     d  ErrMessage                     n   overlay(IndDspF : 28)
009800121207        //   -?Riemissione videata?
009900121207     d  ErrGenerico                    n   overlay(IndDspF : 99)
010000100203
010100121207       // -?Ridefinizione parametri a video?
010200121207     d ds_V1KSC        ds                  inz
010300121207     d   V1ksc01                           inz
010400121207     d   V1ksc02                           inz
010500121207     d   V1ksc03                           inz
010600121207     d   V1ksc04                           inz
010700121207     d   V1ksc05                           inz
010800121207     d   V1ksc06                           inz
010900121207     d   V1ksc07                           inz
011000121207     d   V1ksc08                           inz
011100121207     d   V1ksc09                           inz
011200121207     d   V1ksc10                           inz
011300121207     d   V1ksc11                           inz
011400121207     d   V1ksc12                           inz
011500121207     d   V1ksc13                           inz
011600121207     d   V1ksc14                           inz
011700121207     d   V1ksc15                           inz
011800121207     d   V1ksc16                           inz
011900121207     d   V1ksc17                           inz
012000121207     d   V1ksc18                           inz
012100121207     d   V1ksc19                           inz
012200121207     d   V1ksc20                           inz
012300121207     d   V1ksc21                           inz
012400121207     d   V1ksc22                           inz
012500121207     d   V1ksc23                           inz
012600121207     d   V1ksc24                           inz
012700121207     d   V1ksc25                           inz
012800121207     d   V1ksc26                           inz
012900121207     d   V1ksc27                           inz
013000121207     d   V1ksc28                           inz
013100121207     d   V1ksc29                           inz
013200121207     d   V1ksc30                           inz
013300121207     d   V1ksc31                           inz
013400121207     d   V1ksc32                           inz
013500121207     d   V1ksc33                           inz
013600121207     d   V1ksc34                           inz
013700121207     d   V1ksc35                           inz
013800121207     d   V1ksc36                           inz
013900121207     d   V1ksc37                           inz
014000121207     d   V1ksc38                           inz
014100121207     d   V1ksc39                           inz
014200121207     d   V1ksc40                           inz
014300121207     d   V1ksc41                           inz
014400121207     d   V1ksc42                           inz
014500121207     d   V1ksc43                           inz
014600121207     d   V1ksc44                           inz
014700121207     d   V1ksc45                           inz
014800121207     d   V1ksc46                           inz
014900121207     d   V1ksc47                           inz
015000121207     d   V1ksc48                           inz
015100121207     d   V1ksc49                           inz
015200121207     d   V1ksc50                           inz
015300121207     d   V1ksc51                           inz
015400121207     d   V1ksc52                           inz
015500121207     d   V1ksc53                           inz
015600121207     d   V1ksc54                           inz
015700121207     d   V1ksc55                           inz
015800121207     d   V1ksc56                           inz
015900121207     d   V1ksc57                           inz
016000121207     d   V1ksc58                           inz
016100121207     d   V1ksc59                           inz
016200121207     d   V1ksc60                           inz
016300121207     d   V1ksc61                           inz
016400121207     d   V1ksc62                           inz
016500121207     d   V1ksc63                           inz
016600121207     d   V1ksc64                           inz
016700121207     d   V1ksc65                           inz
016800121207     d   V1ksc66                           inz
016900121207     d   V1ksc67                           inz
017000121207     d   V1ksc68                           inz
017100121207     d   V1ksc69                           inz
017200121207     d   V1ksc70                           inz
017300121207     d   V1ksc71                           inz
017400121207     d   V1ksc72                           inz
017500121207     d   V1ksc73                           inz
017600121207     d   V1ksc74                           inz
017700121207     d   V1ksc75                           inz
017800121207     d   V1ksc76                           inz
017900121207     d   V1ksc77                           inz
018000121207     d   V1ksc78                           inz
018100121207     d     $Ksc                1    546  0 inz  dim(78)
018200100203      *
018300121207     d ds_V2ABL        ds                  inz
018400121207     d   V2abl01                           inz
018500121207     d   V2abl02                           inz
018600121207     d   V2abl03                           inz
018700121207     d   V2abl04                           inz
018800121207     d   V2abl05                           inz
018900121207     d   V2abl06                           inz
019000121207     d   V2abl07                           inz
019100121207     d   V2abl08                           inz
019200121207     d   V2abl09                           inz
019300121207     d   V2abl10                           inz
019400121207     d   V2abl11                           inz
019500121207     d   V2abl12                           inz
019600121207     d   V2abl13                           inz
019700121207     d   V2abl14                           inz
019800121207     d   V2abl15                           inz
019900121207     d   V2abl16                           inz
020000121207     d   V2abl17                           inz
020100121207     d   V2abl18                           inz
020200121207     d   V2abl19                           inz
020300121207     d   V2abl20                           inz
020400121207     d   V2abl21                           inz
020500121207     d   V2abl22                           inz
020600121207     d   V2abl23                           inz
020700121207     d   V2abl24                           inz
020800121207     d   V2abl25                           inz
020900121207     d   V2abl26                           inz
021000121207     d   V2abl27                           inz
021100121207     d   V2abl28                           inz
021200121207     d   V2abl29                           inz
021300121207     d   V2abl30                           inz
021400121207     d   V2abl31                           inz
021500121207     d   V2abl32                           inz
021600121207     d   V2abl33                           inz
021700121207     d   V2abl34                           inz
021800121207     d   V2abl35                           inz
021900121207     d   V2abl36                           inz
022000121207     d   V2abl37                           inz
022100121207     d   V2abl38                           inz
022200121207     d   V2abl39                           inz
022300121207     d   V2abl40                           inz
022400121207     d   V2abl41                           inz
022500121207     d   V2abl42                           inz
022600121207     d   V2abl43                           inz
022700121207     d   V2abl44                           inz
022800121207     d   V2abl45                           inz
022900121207     d   V2abl46                           inz
023000121207     d   V2abl47                           inz
023100121207     d   V2abl48                           inz
023200121207     d   V2abl49                           inz
023300121207     d   V2abl50                           inz
023400121207     d   V2abl51                           inz
023500121207     d   V2abl52                           inz
023600121207     d   V2abl53                           inz
023700121207     d   V2abl54                           inz
023800121207     d   V2abl55                           inz
023900121207     d   V2abl56                           inz
024000121207     d   V2abl57                           inz
024100121207     d   V2abl58                           inz
024200121207     d   V2abl59                           inz
024300121207     d   V2abl60                           inz
024400121207     d   V2abl61                           inz
024500121207     d   V2abl62                           inz
024600121207     d   V2abl63                           inz
024700121207     d   V2abl64                           inz
024800121207     d   V2abl65                           inz
024900121207     d   V2abl66                           inz
025000121207     d   V2abl67                           inz
025100121207     d   V2abl68                           inz
025200121207     d   V2abl69                           inz
025300121207     d   V2abl70                           inz
025400121207     d   V2abl71                           inz
025500121207     d   V2abl72                           inz
025600121207     d   V2abl73                           inz
025700121207     d   V2abl74                           inz
025800121207     d   V2abl75                           inz
025900121207     d   V2abl76                           inz
026000121207     d   V2abl77                           inz
026100121207     d   V2abl78                           inz
026200121207     d     $Abl                1     78    inz  dim(78)
026300100203
026400100203       //--------------------------------------------------------------
026500100203       //?Definizione variabili globali.                               ?
026600100203       //--------------------------------------------------------------
026700100203
026800121207       // -?Flags booleani?
026900100203     d $Fine           s               n   inz(*off)
027000100203     d $InzD01         s               n   inz(*on)
027100100203     d $InzD02         s               n   inz(*on)
027200100203
027300121207       // -?Indici di schiera?
027400100203     d xx              s              5  0 inz
027500120723     d yy              s              5  0 inz
027600100203
027700121207       // -?Campi per la gestione del video?
027800100203     d $Video          s              2a   inz('D1')
027900120723     d wPage           s              3  0 inz
028000100203
028100100203       //--------------------------------------------------------------
028200100203       //?Definizione prototipi procedure usate.                       ?
028300100203       //--------------------------------------------------------------
028400100203
028500121207       // -?Reperimento dati utente?
028600121207     d TIBS34ds      e ds                  inz
028700121207      /copy gaitrasrc/srcProtoPR,TIBS34R
028800121207
028900121207       // -?Reperimento legami in tab. "3C"?
029000121207      /copy gaitrasrc/srcProtoPI,UBLEG3C
029100121207      /copy gaitrasrc/srcProtoPR,UBLEG3C
029200100203
029300121207       // -?Verifica blocco clienti?
029400121207      /copy gaitrasrc/srcProtoPI,UBCHKABL
029500121207      /copy gaitrasrc/srcProtoPR,UBCHKABL
029600100203
029700100203       //--------------------------------------------------------------
029800100203       //?Definizione key-list.                                        ?
029900100203       //--------------------------------------------------------------
030000100203
030100100203
030200100203       //--------------------------------------------------------------
030300100203       //?Riepilogo indicatori.                                        ?
030400100203       //--------------------------------------------------------------
030500100203
030600100203
030700100203       //--------------------------------------------------------------
030800100203       //?M A I N - L I N E                                            ?
030900100203       //--------------------------------------------------------------
031000100203
031100100203      /free
031200100203
031300121207       // -?Operazioni iniziali?
031400100203       *inLR = *on;
031500100203
031600121207       // -?Gestione videate?
031700121207       DoW  $Fine = *off;
031800100203         select;
031900100203           when  $Video = 'D1';
032000100203             exsr sr_GesD01;
032100100203           when  $Video = 'D2';
032200100203             exsr sr_GesD02;
032300100203           other;
032400100203             $Fine = *on;
032500100203         endsl;
032600121207       EndDo;
032700100203
032800121207       // -?Operazioni finali?
032900121207       ubChkABL_Close();
033000100203       return;
033100100203
033200100203       //--------------------------------------------------------------
033300100203       //?Operazioni iniziali                                          ?
033400100203       //--------------------------------------------------------------
033500100203       BEGSR  *InzSR;
033600100203
033700121207         // -?Reperimento dati utente?
033800100203         exsr sr_DatiJob;
033900100203
034000121207         // -?Impostazione nome programma in testata a video?
034100100203         //VTDpgm = SDSpgm;
034200100203
034300100203       ENDSR;
034400100203
034500100203       //--------------------------------------------------------------
034600100203       //?Reperimento Dati del job (Utente/Operativi).                 ?
034700100203       //--------------------------------------------------------------
034800100203       BEGSR  sr_DatiJob;
034900100203
035000100203         in(E) §AzUte;
035100100203         if NOT %error;
035200100203           in(E) §DatiUte;
035300100203         endif;
035400100203         if %error or RSut = *blanks;
035500100203           clear TIBS34ds;
035600100203           tibs34r(tibs34ds);
035700100203           in §AzUte;
035800100203           in §DatiUte;
035900100203         endif;
036000100203
036100100203       ENDSR;
036200100203
036300100203       //--------------------------------------------------------------
036400100203       //?Gestione videata D01                                         ?
036500100203       //--------------------------------------------------------------
036600100203       BEGSR  sr_GesD01;
036700100203
036800121207         // -?Inizializzazione videata?
036900100203         if  $InzD01 = *on;
037000121207           clear ubChkABLd1;
037100100203           $InzD01 = *off;
037200100203         endif;
037300100203         AbilitF06  = *on;
037400100203         AbilitF12  = *off;
037500100203
037600121207         // -?Emissione videata?
037700100203         if  ErrMessage = *off;
037800121207           write ubChkABLt1;
037900121207           write ubChkABLz1;
038000100203         endif;
038100100203
038200121207         exfmt ubChkABLd1;
038300100203
038400100203         reset ErrMessage;
038500100203         reset ErrGenerico;
038600121207         clear VIDmsg;
038700100203
038800100203         SELECT;
038900100203
039000121207           // -?F3=Fine?
039100100203           WHEN  dsp_aid = c_F03;
039200100203             $Fine = *on;
039300100203
039400121207           // -?Invio o F6=Conferma?
039500100203           OTHER;
039600100203             exsr sr_CtrD01;
039700100203             select;
039800100203               // - rilevato errore
039900100203               when  ErrGenerico = *on;
040000100203               // - non premuto F6
040100100203               when  dsp_aid <> c_F06;
040200100203               // - premuto F6
040300100203               other;
040400100203                 $Video = 'D2';
040500100203                 reset  $InzD02;
040600100203             endsl;
040700100203
040800100203         ENDSL;
040900100203
041000100203       ENDSR;
041100100203
041200100203       //--------------------------------------------------------------
041300100203       //?Controllo dati in videata D01                                ?
041400100203       //--------------------------------------------------------------
041500100203       BEGSR sr_CtrD01;
041600100203
041700100203         %subst(IndDspF : 28) = *off;
041800121207
041900121207         clear  ubChkABL_KSC;
042000121207         if  ubLeg3C_Rtv ( V1ksc00  :
042100121207                           ubLeg3C_FlgPF :
042200121207                           ubLeg3C_Padre :
042300121207                           ubLeg3C_SKC :
042400121207                           ubLeg3C_SKEW ) < *zero;
042500121207           ErrGenerico = *on;
042600121207           ErrMessage  = *on;
042700121207           VIDmsg = 'Rilevato errore nel reperimento famiglia dalla +
042800121207                     tab. "3C"';
042900121207         else;
043000121207           ubChkABL_KSC = ubLeg3C_SKC;
043100121207         endif;
043200100203
043300100203       ENDSR;
043400100203
043500100203       //--------------------------------------------------------------
043600100203       //?Gestione videata D02                                         ?
043700100203       //--------------------------------------------------------------
043800100203       BEGSR  sr_GesD02;
043900100203
044000121207         // -?Inizializzazione videata?
044100100203         if  $InzD02 = *on;
044200121207           clear  ubChkABLd2;
044300121207           exsr  Call_ubChkABL;
044400100203           $InzD02  = *off;
044500100203           AbilitF06  = *off;
044600100203           AbilitF12  = *on;
044700100203         endif;
044800120723
044900121207         AbilitRollUp  = ( ( wPage * %elem($Ksc) + 1 ) <= %elem(sk_Ksc)  and
045000121207                           sk_Ksc( wPage * %elem($Ksc) + 1 ) > *zero );
045100121207         AbilitRollDwn = ( wPage > 1);
045200100203
045300121207         // -?Emissione videata?
045400100203         if  ErrMessage = *off;
045500121207           write ubChkABLt1;
045600121207           write ubChkABLz1;
045700121207           write ubChkABLd1;
045800100203           write Protect;
045900100203         endif;
046000100203
046100121207         exfmt ubChkABLd2;
046200100203
046300100203         reset ErrMessage;
046400100203         reset ErrGenerico;
046500121207         clear VIDmsg;
046600100203
046700100203         SELECT;
046800100203
046900121207           // -?F3=Fine?
047000100203           WHEN  dsp_aid = c_F03;
047100100203             $Fine = *on;
047200100203
047300121207           // -?F12=Ritorno?
047400100203           WHEN  dsp_aid = c_F12;
047500100203             $Video = 'D1';
047600100203             //reset $InzD01;
047700100203             reset $InzD02;
047800120723
047900121207           // -?RollUp?
048000120723           WHEN  dsp_aid = c_RollUp;
048100120723             wPage += 1;
048200121207             exsr  sr_PagSkABL;
048300120723
048400121207           // -?RollDown?
048500120723           WHEN  dsp_aid = c_RollDown;
048600120723             wPage -= 1;
048700121207             exsr  sr_PagSkABL;
048800100203
048900121207           // -?Invio?
049000100203           OTHER;
049100100203
049200100203         ENDSL;
049300100203
049400100203       ENDSR;
049500100203
049600100203       //--------------------------------------------------------------
049700121207       //?Richiamo Service Program ubChkABL                            ?
049800100203       //--------------------------------------------------------------
049900121207       BEGSR  Call_ubChkABL;
050000100203
050100121207         sk_KSC = sch_SKC;
050200121207         clear  sk_ABL;
050300100203
050400121207         V2Cesito = ubChkABL_Chk ( ubChkABL_Ksc : ubChkABL_Abl );
050500120723
050600120723         wPage = 1;
050700121207         exsr  sr_PagSkABL;
050800120723
050900120723       ENDSR;
051000100203
051100120723       //--------------------------------------------------------------
051200121207       //?Visualizzazione pagina di abilitazioni.                      ?
051300120723       //--------------------------------------------------------------
051400121207       BEGSR  sr_PagSkABL;
051500120723
051600121207         yy = ( (wPage - 1) * %elem($Ksc) );
051700120723
051800121207         For  xx = 1  To  %elem($Ksc);
051900121207           if  yy  >= %elem(sk_KSC);
052000121207             leave;
052100121207           endif;
052200121207           yy      += 1;
052300121207           $Ksc(xx) = sk_KSC(yy);
052400121207           $Abl(xx) = sk_ABL(yy);
052500100203         EndFor;
052600100203
052700100203       ENDSR;
052800100203
052900100203      /end-free
