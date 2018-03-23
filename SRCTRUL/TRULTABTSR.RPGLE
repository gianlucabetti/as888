000100090731      *PARMS dftactgrp(*no) actgrp('QILE') bnddir('TRUL')
000200090128       //--------------------------------------------------------------
000300090731       //?TEST - REPERIMENTO DATI TABELLA (TABEL00F / TNTBE01L)        ?
000400090128       //--------------------------------------------------------------
000500090128
000600090128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090731     h dftactgrp(*no) actgrp('QILE') bnddir('TRUL')
000800090128
000900090128       //--------------------------------------------------------------
001000090731       //?Dichiarazione file.                                          ?
001100090128       //--------------------------------------------------------------
001200090128
001300090128       // - Video
001400090731     fTRULTABTSDcf   e             workstn
001500090128     f                                     indds(IndDspF)
001600090128     f                                     infds(InfDspF)
001700090128
001800090128       //--------------------------------------------------------------
001900090731       //?Definizione costanti.                                        ?
002000090128       //--------------------------------------------------------------
002100090128
002200090128       // - Tasti funzionali a video
002300090128     d c_F01           c                   const(x'31')
002400090128     d c_F02           c                   const(x'32')
002500090128     d c_F03           c                   const(x'33')
002600090128     d c_F04           c                   const(x'34')
002700090128     d c_F05           c                   const(x'35')
002800090128     d c_F06           c                   const(x'36')
002900090128     d c_F07           c                   const(x'37')
003000090128     d c_F08           c                   const(x'38')
003100090128     d c_F09           c                   const(x'39')
003200090128     d c_F10           c                   const(x'3A')
003300090128     d c_F11           c                   const(x'3B')
003400090128     d c_F12           c                   const(x'3C')
003500090128     d c_F13           c                   const(x'B1')
003600090128     d c_F14           c                   const(x'B2')
003700090128     d c_F15           c                   const(x'B3')
003800090128     d c_F16           c                   const(x'B4')
003900090128     d c_F17           c                   const(x'B5')
004000090128     d c_F18           c                   const(x'B6')
004100090128     d c_F19           c                   const(x'B7')
004200090128     d c_F20           c                   const(x'B8')
004300090128     d c_F21           c                   const(x'B9')
004400090128     d c_F22           c                   const(x'BA')
004500090128     d c_F23           c                   const(x'BB')
004600090128     d c_F24           c                   const(x'BC')
004700090128     d c_Enter         c                   const(x'F1')
004800090128     d c_RollDown      c                   const(x'F4')
004900090128     d c_RollUp        c                   const(x'F5')
005000090128
005100090128       //--------------------------------------------------------------
005200090731       //?Definizione schiere.                                         ?
005300090128       //--------------------------------------------------------------
005400090128
005500090128
005600090128       //--------------------------------------------------------------
005700090731       //?Definizione aree dati.                                       ?
005800090128       //--------------------------------------------------------------
005900090128
006000090128       // - Dati utente
006100090128     d §AzUte        e ds                  extname(AZUTE00F)
006200090128     d                                     dtaara
006300090128     d §DatiUte      e ds                  extname(dDatiUte)
006400090128     d                                     dtaara
006500090128
006600090128       //--------------------------------------------------------------
006700090731       //?Definizione strutture dati.                                  ?
006800090128       //--------------------------------------------------------------
006900090128
007000090128       // - Status ds
007100090128     d Status         sds
007200090128     d  SDSpgm           *proc
007300090128     d  SDSprm           *parms
007400090128     d  SDSusr               254    263
007500090128
007600090128       // - InfDS
007700090128     d InfDspF         ds
007800090128     d  dsp_aid              369    369a
007900090128
008000090128       // - Indicatori su DspF
008100090128     d IndDspF         ds
008200090730        // - Abilitazione tasti funzionali
008300090730     d  AbilitF06                     1n   overlay(IndDspF : 06)
008400090730     d  AbilitF12                     1n   overlay(IndDspF : 12)
008500090128        // - Indicatori di errore
008600090128     d  ErrMessage                    1n   overlay(IndDspF : 28)
008700090128     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008800090917
008900090917       // - KPJBA
009000090917     d kpjba         e ds
009100090128
009200090128       // - Parametri x Controllo profilo utenti
009300090128     d TIBS34ds      e ds
009400090128
009500090128       //--------------------------------------------------------------
009600090731       //?Definizione variabili globali.                               ?
009700090128       //--------------------------------------------------------------
009800090128
009900090128       // - Flags booleani
010000090128     d $Fine           s               n   inz(*off)
010100090128     d $InzD01         s               n   inz(*on)
010200090730     d $InzD02         s               n   inz(*on)
010300090730     d $InzD03         s               n   inz(*on)
010400090730
010500090730       // - Campi per la gestione del video
010600090730     d $Video          s              2a   inz('D1')
010700090917
010800090917       // - Parametri x TRULTAB (diversi da quelli a video)
010900090917     d  pOutRrn        s             10i 0
011000090917     d  pOutEsito      s             10i 0
011100090128
011200090128       //--------------------------------------------------------------
011300090731       //?Definizione prototipi procedure usate.                       ?
011400090128       //--------------------------------------------------------------
011500090128
011600090128       // - Reperimento dati utente
011700090128     d tibs34r         pr                  extpgm('TIBS34R')
011800090128     d   tibs34ds                          likeds(TIBS34ds)
011900090128
012000090730       // - Reperimento dati Tabella (TABEL o TNTBE)
012100090803      /copy gaitrasrc/srcProtoPr,TRULTAB
012200090128
012300090128       //--------------------------------------------------------------
012400090731       //?Definizione key-list.                                        ?
012500090128       //--------------------------------------------------------------
012600090128
012700090128
012800090128       //--------------------------------------------------------------
012900090731       //?Riepilogo indicatori.                                        ?
013000090128       //--------------------------------------------------------------
013100090128
013200090128
013300090128       //--------------------------------------------------------------
013400090731       //?M A I N - L I N E                                            ?
013500090128       //--------------------------------------------------------------
013600090917
013700090917     c     *Entry        plist
013800090917     c                   parm                    KPJBA
013900090128
014000090128      /free
014100090128
014200090128       // - Start
014300090128       *inLR = *on;
014400090917       V1Tpgm = SDSpgm;
014500090128
014600090128       // - Elab
014700090128       dow  $Fine = *off;
014800090730         select;
014900090730           when  $Video = 'D1';
015000090730             exsr sr_GesD01;
015100090730           when  $Video = 'D2';
015200090805             exsr sr_GesD02;
015300090730           when  $Video = 'D3';
015400090730             exsr sr_GesD03;
015500090730           other;
015600090730             $Fine = *on;
015700090730         endsl;
015800090128       enddo;
015900090128
016000090128       // - End
016100090810       cloTabella();
016200090128       return;
016300090128
016400090128       //--------------------------------------------------------------
016500090731       //?Operazioni iniziali                                          ?
016600090128       //--------------------------------------------------------------
016700090730       BEGSR  *InzSR;
016800090128
016900090128         // Reperimento dati utente
017000090128         exsr sr_DatiJob;
017100090128
017200090128         // Impostazione nome programma in testata a video
017300090128         //VTDpgm = SDSpgm;
017400090128
017500090128       ENDSR;
017600090128
017700090128       //--------------------------------------------------------------
017800090731       //?Reperimento Dati del job (Utente/Operativi).                 ?
017900090128       //--------------------------------------------------------------
018000090730       BEGSR  sr_DatiJob;
018100090128
018200090128         in(E) §AzUte;
018300090128         if NOT %error;
018400090128           in(E) §DatiUte;
018500090128         endif;
018600090128         if %error or RSut = *blanks;
018700090128           clear TIBS34ds;
018800090128           tibs34r(tibs34ds);
018900090128           in §AzUte;
019000090128           in §DatiUte;
019100090128         endif;
019200090128
019300090128       ENDSR;
019400090128
019500090128       //--------------------------------------------------------------
019600090731       //?Gestione videata D01                                         ?
019700090128       //--------------------------------------------------------------
019800090730       BEGSR  sr_GesD01;
019900090128
020000090128         // Inizializzazione videata
020100090128         if  $InzD01 = *on;
020200090730           clear ulTABd1;
020300090731           V1xKe2  = 'X';
020400090731           V1xLsi  = 'X';
020500090731           $InzD01 = *off;
020600090128         endif;
020700090805         AbilitF06  = *on;
020800090805         AbilitF12  = *off;
020900090128
021000090730         // Emissione videata
021100090128         if  ErrMessage = *off;
021200090730           write ulTABt1;
021300090730           write ulTABz1;
021400090128         endif;
021500090128
021600090730         exfmt ulTABd1;
021700090128
021800090128         reset ErrMessage;
021900090128         reset ErrGenerico;
022000090128         clear V1Dmsg;
022100090128
022200090128         SELECT;
022300090128
022400090128         // - F3=Fine
022500090128           WHEN  dsp_aid = c_F03;
022600090128             $Fine = *on;
022700090128
022800090128         // - Invio o F6=Conferma
022900090128           OTHER;
023000090730             exsr sr_CtrD01;
023100090128             select;
023200090129               // - rilevato errore
023300090128               when  ErrGenerico = *on;
023400090129               // - non premuto F6
023500090128               when  dsp_aid <> c_F06;
023600090731               // - scelta parametri da estrarre
023700090805             //when  V1dFileTab = '1'  or
023800090805             //      (V1xKe2  = 'X'   and   V1xLsi  = 'X');
023900090805             //  $Video = 'D2';
024000090805             //  reset  $InzD02;
024100090731               // - estrazione di tutto
024200090731               other;
024300090805                 exsr  sr_InzD02;
024400090731                 $Video = 'D3';
024500090731                 reset  $InzD03;
024600090128             endsl;
024700090128
024800090128         ENDSL;
024900090128
025000090128       ENDSR;
025100090730
025200090730       //--------------------------------------------------------------
025300090731       //?Controllo dati in videata D01                                ?
025400090730       //--------------------------------------------------------------
025500090730       BEGSR sr_CtrD01;
025600090730
025700090730         %subst(IndDspF : 28) = *off;
025800090730
025900090730       ENDSR;
026000090730
026100090730       //--------------------------------------------------------------
026200090731       //?Gestione videata D02                                         ?
026300090730       //--------------------------------------------------------------
026400090730       BEGSR  sr_GesD02;
026500090730
026600090730         // Inizializzazione videata
026700090730         if  $InzD02 = *on;
026800090805           exsr  sr_InzD02;
026900090731           $InzD02 = *off;
027000090730           AbilitF06  = *on;
027100090730           AbilitF12  = *on;
027200090730         endif;
027300090730
027400090730         // Emissione videata
027500090730         if  ErrMessage = *off;
027600090730           write ulTABt1;
027700090730           write ulTABz1;
027800090730           write ulTABd1;
027900090730           write Protect;
028000090730         endif;
028100090730
028200090730         exfmt ulTABd2;
028300090730
028400090730         reset ErrMessage;
028500090730         reset ErrGenerico;
028600090730         clear V1Dmsg;
028700090730
028800090730         SELECT;
028900090730
029000090730         // - F3=Fine
029100090730           WHEN  dsp_aid = c_F03;
029200090730             $Fine = *on;
029300090730
029400090730         // - F12=Ritorno
029500090730           WHEN  dsp_aid = c_F12;
029600090730             $Video = 'D1';
029700090730
029800090730         // - F6=Conferma
029900090730           WHEN  dsp_aid = c_F06;
030000090730             $Video = 'D3';
030100090730             reset $InzD03;
030200090730
030300090730         ENDSL;
030400090730
030500090730       ENDSR;
030600090805
030700090805       //--------------------------------------------------------------
030800090805       //?Inizializzazione videata D02                                 ?
030900090805       //--------------------------------------------------------------
031000090805       BEGSR  sr_InzD02;
031100090805
031200090805         clear ulTABd2;
031300090805
031400090805         VixLib  = 'X';
031500090805         if  V1dFileTab = '2'  and
031600090805            (V1xKe2 = *blank   or   V1xLsi = *blank);
031700090805           VixKe2 = 'X';
031800090805         else;
031900090805           VixKe2  = *blank;
032000090805         endif;
032100090805         VixFtt  = 'X';
032200090805         VixUni  = 'X';
032300090805
032400090805       ENDSR;
032500090730
032600090730       //--------------------------------------------------------------
032700090731       //?Gestione videata D03                                         ?
032800090730       //--------------------------------------------------------------
032900090730       BEGSR  sr_GesD03;
033000090730
033100090730         // Inizializzazione videata
033200090805         if  $InzD03 = *on;
033300090731           clear  ulTABd3;
033400090731           exsr  Call_trulTAB;
033500090731           if  VixLib  = 'X';
033600090803             VidRrn = pOutRrn;
033700090731           endif;
033800090730           $InzD03  = *off;
033900090730           AbilitF06  = *off;
034000090730           AbilitF12  = *on;
034100090730         endif;
034200090730
034300090730         // Emissione videata
034400090730         if  ErrMessage = *off;
034500090730           write ulTABt1;
034600090730           write ulTABz1;
034700090730           write ulTABd1;
034800090731           write Protect;
034900090730         endif;
035000090730
035100090730         exfmt ulTABd3;
035200090730
035300090730         reset ErrMessage;
035400090730         reset ErrGenerico;
035500090730         clear V1Dmsg;
035600090730
035700090730         SELECT;
035800090730
035900130517           // - F3=Fine
036000090730           WHEN  dsp_aid = c_F03;
036100090730             $Fine = *on;
036200090730
036300130517           // - F12=Ritorno
036400090730           WHEN  dsp_aid = c_F12;
036500090730             $Video = 'D1';
036600090805             //reset $InzD01;
036700090805             //reset $InzD02;
036800090730             reset $InzD03;
036900090730
037000130517           // - Invio
037100090730           OTHER;
037200090730
037300090730         ENDSL;
037400090730
037500090730       ENDSR;
037600090128
037700090128       //--------------------------------------------------------------
037800090731       //?Richiamo Service Program TRULTAB                             ?
037900090128       //--------------------------------------------------------------
038000090730       BEGSR Call_TrulTAB;
038100090730
038200090803         clear  pOutRrn;
038300090803         clear  pOutEsito;
038400090730
038500090731         // - Impostazione parametri a seconda di quelli richiesti
038600090730         select;
038700090804
038800090731           // - NO Key2
038900090804           when  V1dFileTab = '2'   and
039000090804                 V1xKe2  = *blank   and
039100090731                 V1xLsi  = 'X';
039200090803             VidEsito = getTabella
039300090803                        ( V1dFileTab : V1dCodTab : V1dKe1 :
039400090803                          *omit  : V1dLin  : V1dSif :
039500090803                          VidLib : pOutRrn :
039600090803                          VidKe2 : VidLin  : VidSif : VidApl :
039700090803                          VidFtt : VidFlt  : VidFtr : VidDtr :
039800090803                          VidAtb : VidUni );
039900090804
040000090731           // - Né Lingua né Sistema Informativo
040100090804           when  V1dFileTab = '2'   and
040200090804                 V1xKe2  = 'X'      and
040300090731                 V1xLsi  = *blank;
040400090803             VidEsito = getTabella
040500090803                        ( V1dFileTab : V1dCodTab : V1dKe1 :
040600090803                          V1dKe2 : *omit   : *omit  :
040700090803                          VidLib : pOutRrn :
040800090803                          VidKe2 : VidLin  : VidSif : VidApl :
040900090803                          VidFtt : VidFlt  : VidFtr : VidDtr :
041000090803                          VidAtb : VidUni );
041100090804
041200090731           // - Né Key2 né Lingua né Sistema Informativo
041300090804           when  V1dFileTab = '2'   and
041400090804                 V1xKe2  = *blank   and
041500090731                 V1xLsi  = *blank;
041600090803             VidEsito = getTabella
041700090803                        ( V1dFileTab : V1dCodTab : V1dKe1 :
041800090803                          *omit  : *omit   : *omit  :
041900090803                          VidLib : pOutRrn :
042000090803                          VidKe2 : VidLin  : VidSif : VidApl :
042100090803                          VidFtt : VidFlt  : VidFtr : VidDtr :
042200090803                          VidAtb : VidUni );
042300090731
042400090731           // - Né Libreria né Nrr
042500090804           when  V1dFileTab = '1'   and
042600090804                 VixLib  = *blank   and
042700090804                 VixFtt  = 'X'      and
042800090804                 VixUni  = 'X';
042900090804             VidEsito = getTabella
043000090804                        ( V1dFileTab : V1dCodTab : V1dKe1 :
043100090804                          *omit  : *omit   : *omit  :
043200090804                          *omit  : *omit   :
043300090804                          *omit  : *omit   : *omit  : *omit  :
043400090804                          VidFtt : VidFlt  : VidFtr : VidDtr :
043500090804                          VidAtb : VidUni );
043600090804           when  V1dFileTab = '2'   and
043700090804                 VixLib  = *blank   and
043800090731                 VixFtt  = 'X'      and
043900090731                 VixUni  = 'X';
044000090803             VidEsito = getTabella
044100090803                        ( V1dFileTab : V1dCodTab : V1dKe1 :
044200090803                          V1dKe2 : V1dLin  : V1dSif :
044300090803                          *omit  : *omit   :
044400090803                          *omit  : *omit   : *omit  : *omit  :
044500090803                          VidFtt : VidFlt  : VidFtr : VidDtr :
044600090803                          VidAtb : VidUni );
044700090804
044800090731           // - NO trasmissione (né tabelle né record)
044900090804           when  VixLib  = 'X'      and
045000090804                 VixFtt  = *blank   and
045100090804                 VixUni  = 'X';
045200090804             if  V1dFileTab = '1';
045300090804               VidEsito = getTabella
045400090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
045500090804                            *omit  : *omit   : *omit  :
045600090804                            VidLib : pOutRrn :
045700090804                            *omit  : *omit   : *omit  : *omit  :
045800090804                            *omit  : *omit   : *omit  : *omit  :
045900090804                            VidAtb : VidUni );
046000090804             else;
046100090804               VidEsito = getTabella
046200090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
046300090804                            V1dKe2 : V1dLin  : V1dSif :
046400090804                            VidLib : pOutRrn :
046500090804                            *omit  : *omit   : *omit  : *omit  :
046600090804                            *omit  : *omit   : *omit  : *omit  :
046700090804                            VidAtb : VidUni );
046800090804             endif;
046900090804
047000090731           // - Né flag annullamento né dati
047100090804           when  VixLib  = 'X'      and
047200090804                 VixFtt  = 'X'      and
047300090804                 VixUni  = *blank;
047400090804             if  V1dFileTab = '1';
047500090804               VidEsito = getTabella
047600090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
047700090804                            *omit  : *omit   : *omit  :
047800090804                            VidLib : pOutRrn :
047900090804                            *omit  : *omit   : *omit  : *omit  :
048000090804                            VidFtt : VidFlt  : VidFtr : VidDtr );
048100090804             else;
048200090804               VidEsito = getTabella
048300090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
048400090804                            V1dKe2 : V1dLin  : V1dSif :
048500090804                            VidLib : pOutRrn :
048600090804                            *omit  : *omit   : *omit  : *omit  :
048700090804                            VidFtt : VidFlt  : VidFtr : VidDtr );
048800090804             endif;
048900090804
049000090731           // - Né Libreria né Nrr
049100090731           //   Né trasmissione (né tabelle né record)
049200090804           when  VixLib  = *blank   and
049300090804                 VixFtt  = *blank   and
049400090804                 VixUni  = 'X';
049500090804             if  V1dFileTab = '1';
049600090804               VidEsito = getTabella
049700090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
049800090804                            *omit  : *omit   : *omit  :
049900090804                            *omit  : *omit   :
050000090804                            *omit  : *omit   : *omit  : *omit  :
050100090804                            *omit  : *omit   : *omit  : *omit  :
050200090804                            VidAtb : VidUni );
050300090804             else;
050400090804               VidEsito = getTabella
050500090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
050600090804                            V1dKe2 : V1dLin  : V1dSif :
050700090804                            *omit  : *omit   :
050800090804                            *omit  : *omit   : *omit  : *omit  :
050900090804                            *omit  : *omit   : *omit  : *omit  :
051000090804                            VidAtb : VidUni );
051100090804             endif;
051200090804
051300090731           // - Né Libreria né Nrr
051400090804           //   Né flag annullamento né dati
051500090804           when  VixLib  = *blank   and
051600090804                 VixFtt  = 'X'      and
051700090804                 VixUni  = *blank;
051800090804             if  V1dFileTab = '1';
051900090804               VidEsito = getTabella
052000090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
052100090804                            *omit  : *omit   : *omit  :
052200090804                            *omit  : *omit   :
052300090804                            *omit  : *omit   : *omit  : *omit  :
052400090804                            VidFtt : VidFlt  : VidFtr : VidDtr );
052500090804             else;
052600090804               VidEsito = getTabella
052700090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
052800090804                            V1dKe2 : V1dLin  : V1dSif :
052900090804                            *omit  : *omit   :
053000090804                            *omit  : *omit   : *omit  : *omit  :
053100090804                            VidFtt : VidFlt  : VidFtr : VidDtr );
053200090804             endif;
053300090804
053400090731           // - Né trasmissione (né tabelle né record)
053500090804           //   Né flag annullamento né dati
053600090804           when  VixLib  = 'X'      and
053700090804                 VixFtt  = *blank   and
053800090804                 VixUni  = *blank;
053900090804             if  V1dFileTab = '1';
054000090804               VidEsito = getTabella
054100090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
054200090804                            *omit  : *omit   : *omit  :
054300090804                            VidLib : pOutRrn );
054400090804             else;
054500090804               VidEsito = getTabella
054600090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
054700090804                            V1dKe2 : V1dLin  : V1dSif :
054800090804                            VidLib : pOutRrn );
054900090804             endif;
055000090804
055100090731           // - Né Libreria né Nrr
055200090804           //   Né trasmissione (né tabelle né record)
055300090804           //   Né flag annullamento né dati
055400090804           when  VixLib  = *blank   and
055500090804                 VixFtt  = *blank   and
055600090804                 VixUni  = *blank;
055700090804             if  V1dFileTab = '1';
055800090804               VidEsito = getTabella
055900090804                          ( V1dFileTab : V1dCodTab : V1dKe1 );
056000090804             else;
056100090804               VidEsito = getTabella
056200090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
056300090804                            V1dKe2 : V1dLin  : V1dSif );
056400090804             endif;
056500090731
056600090730           // - Altrimenti TUTTO
056700090804           other;
056800130517             if  V1dFileTab = '1';
056900130517               VidEsito = getTabella
057000130517                          ( V1dFileTab : V1dCodTab : V1dKe1 :
057100130517                            *omit  : V1dLin  : *omit  :
057200130517                            VidLib : pOutRrn :
057300130517                            *omit  : VidLin  : *omit  : *omit  :
057400130517                            VidFtt : VidFlt  : VidFtr : VidDtr :
057500130517                            VidAtb : VidUni );
057600130517             else;
057700090804               VidEsito = getTabella
057800090804                          ( V1dFileTab : V1dCodTab : V1dKe1 :
057900090804                            V1dKe2 : V1dLin  : V1dSif :
058000090804                            VidLib : pOutRrn :
058100090804                            VidKe2 : VidLin  : VidSif : VidApl :
058200090804                            VidFtt : VidFlt  : VidFtr : VidDtr :
058300090804                            VidAtb : VidUni );
058400130517             endif;
058500090804
058600090730         endsl;
058700090128
058800090128       ENDSR;
058900090128
059000090128      /end-free
