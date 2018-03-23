000100090812       //--------------------------------------------------------------
000200090812       //?TNTA29R - Gestione Note "10" di Tariffe/Offerte              ?
000300090812       //--------------------------------------------------------------
000400090812
000500090812     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090812
000700090812       //--------------------------------------------------------------
000800090812       //?Dichiarazione file.                                          ?
000900090812       //--------------------------------------------------------------
001000090813
001100090813       // - Tabelle
001200090813     fTABEL00F  if   e           k disk    usropn
001300090812
001400090813       // - Offerte
001500090813     fTNOFM01L  if   e           k disk    usropn
001600090813     f                                     rename(TNTAM000 : TNOFM000)
001700090813       // - Tariffe
001800090813     fTNTAM01L  if   e           k disk    usropn
001900090813
002000090812       // - Note
002100090812     fTFNTC01L  Uf A e           k disk
002200090812
002300090812       // - Video Gestione
002400090812     fTNTA29D   cf   e             workstn
002500090812     f                                     sfile(TA29S01 : S01nrr)
002600090813     f                                     sfile(TA29S02 : S02nrr)
002700090812     f                                     indds(IndDspF)
002800090812     f                                     infds(InfDspF)
002900090812
003000090812       //--------------------------------------------------------------
003100090812       //?Definizione costanti.                                        ?
003200090812       //--------------------------------------------------------------
003300090812
003400090812       // - Tipo note
003500090812     d c_TipoNota      c                   const('10')
003600090812
003700090812       // - Numero di record in ciascuna videata di subfile
003800090813     d c_SflPag        c                   const(04)
003900090813     d c_SflPag2       c                   const(09)
004000090812
004100090812       // - Numero massimo di record gestibili
004200090812     d c_MaxRec        c                   const(004)
004300090813     d c_MaxRec2       c                   const(999)
004400090812
004500090812       // - Tasti funzionali a video
004600090812     d c_F01           c                   const(x'31')
004700090812     d c_F02           c                   const(x'32')
004800090812     d c_F03           c                   const(x'33')
004900090812     d c_F04           c                   const(x'34')
005000090812     d c_F05           c                   const(x'35')
005100090812     d c_F06           c                   const(x'36')
005200090812     d c_F07           c                   const(x'37')
005300090812     d c_F08           c                   const(x'38')
005400090812     d c_F09           c                   const(x'39')
005500090812     d c_F10           c                   const(x'3A')
005600090812     d c_F11           c                   const(x'3B')
005700090812     d c_F12           c                   const(x'3C')
005800090812     d c_F13           c                   const(x'B1')
005900090812     d c_F14           c                   const(x'B2')
006000090812     d c_F15           c                   const(x'B3')
006100090812     d c_F16           c                   const(x'B4')
006200090812     d c_F17           c                   const(x'B5')
006300090812     d c_F18           c                   const(x'B6')
006400090812     d c_F19           c                   const(x'B7')
006500090812     d c_F20           c                   const(x'B8')
006600090812     d c_F21           c                   const(x'B9')
006700090812     d c_F22           c                   const(x'BA')
006800090812     d c_F23           c                   const(x'BB')
006900090812     d c_F24           c                   const(x'BC')
007000090812     d c_Enter         c                   const(x'F1')
007100090812     d c_RollDown      c                   const(x'F4')
007200090812     d c_RollUp        c                   const(x'F5')
007300090812
007400090812       //--------------------------------------------------------------
007500090812       //?Definizione schiere.                                         ?
007600090812       //--------------------------------------------------------------
007700090812
007800090812       // - Messaggi di errore
007900090813     d $Msg            s             78    dim( 9) ctdata perrcd(1)
008000090813
008100090813       // - Schiera Tipo Tariffa e relativa descrizione
008200090813     d sch_CTR         s              1    dim(30)
008300090813     d sch_DTR         s             20    dim(30)
008400090812
008500090812       //--------------------------------------------------------------
008600090812       //?Definizione aree dati.                                       ?
008700090812       //--------------------------------------------------------------
008800090812
008900090812       // - Dati utente
009000090812     d §AzUte        e ds                  extname(AZUTE00F)
009100090812     d                                     dtaara
009200090812     d §DatiUte      e ds                  extname(dDatiUte)
009300090812     d                                     dtaara
009400090812
009500090812       //--------------------------------------------------------------
009600090812       //?Definizione strutture dati.                                  ?
009700090812       //--------------------------------------------------------------
009800090812
009900090812       // - Status ds
010000090812     d Status         sds
010100090812     d  SDSpgm           *proc
010200090812     d  SDSusr               254    263
010300090812
010400090812       // - InfDS
010500090812     d InfDspF         ds
010600090812     d   dsp_aid             369    369a
010700090812     d   sfl_rrn             376    377i 0
010800090812     d   min_nrr             378    379i 0
010900090812     d   num_rcds            380    381i 0
011000090812
011100090812       // - Indicatori su DspF
011200090812     d IndDspF         ds                  inz
011300090812         // - Abilitazione tasti funzionali
011400090814     d   AbilitF6                      n   overlay(IndDspF : 06)
011500090812     d   AbilitF16                     n   overlay(IndDspF : 16)
011600090812         // - Emissione messaggio di errore
011700090812     d   ErrMessage                    n   overlay(IndDspF : 28)
011800090812         // - Indicatori di gestione del subfile
011900090812     d   SflDsp_N                      n   overlay(IndDspF : 30)
012000090812     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012100090812     d   SflNxtChg                     n   overlay(IndDspF : 32)
012200090812     d   SflEnd                        n   overlay(IndDspF : 33)
012300090813     d   SflDsp_N2                     n   overlay(IndDspF : 34)
012400090813     d   SflDspCtl_N2                  n   overlay(IndDspF : 35)
012500090813     d   SflNxtChg2                    n   overlay(IndDspF : 36)
012600090813     d   SflEnd2                       n   overlay(IndDspF : 37)
012700090812         // - Indicatori per Attibuti di visualizzazione
012800090812     d   VisualizCTR                   n   overlay(IndDspF : 40)
012900090813     d   NormalizNOT                   n   overlay(IndDspF : 41)
013000090812         // - Posizionamento cursore & segnalazione errore
013100090812     d   PosCurOPZ                     n   overlay(IndDspF : 50)
013200090813     d   PosCurOPZ2                    n   overlay(IndDspF : 51)
013300090812         //   - Riemissione videata
013400090812     d   ErrGenerico                   n   overlay(IndDspF : 99)
013500090812
013600090812       // - Parametri ricevuti
013700090812     d KPJBA         e ds
013800090812     d TNTA29ds      e ds                  inz
013900090812
014000090812       // - Parametri per Reperimento dati utente
014100090812     d TIBS34ds      e ds                  inz
014200090812
014300090812       //--------------------------------------------------------------
014400090812       //?Definizione variabili globali.                               ?
014500090812       //--------------------------------------------------------------
014600090812
014700090812       // - Flags booleani
014800090812     d $Fine           s               n   inz
014900090812     d $InzS01         s               n   inz(*on)
015000090813     d $InzS02         s               n   inz(*on)
015100090812     d $Note           s               n   inz
015200090813     d $TariffaSenzaNote...
015300090813     d                 s               n   inz
015400090813
015500090813       // - Indici di schiera
015600090813     d xx              s              3  0 inz
015700090812
015800090812       // - Variabili per la gestione del video
015900090812     d $Video          s              2    inz('S1')
016000090813     d W_SflPag        s              3  0 inz
016100090813     d wPag            s              4  0 inz
016200090813     d wRig            s              3  0 inz
016300090812     d S01nrr          s                   like(C1RcdNbr) inz
016400090813     d*// SavS01csr       s                   like(C1RcdNbr) inz
016500090813     d S02nrr          s                   like(C2RcdNbr) inz
016600090813     d*// SavS02csr       s                   like(C2RcdNbr) inz
016700090812
016800090812       // - Campi di comodo
016900090812     d wRecNote        s              3  0 inz
017000090813     d wCpyTar         s              3  0 inz
017100090812
017200090812       //--------------------------------------------------------------
017300090812       //?Definizione prototipi procedure.                             ?
017400090812       //--------------------------------------------------------------
017500090812
017600090812       // - Reperimento dati utente
017700090812      /copy gaitrasrc/srcProtoPR,TIBS34R
017800090812
017900090812       //--------------------------------------------------------------
018000090812       //?Definizione key-list.                                        ?
018100090812       //--------------------------------------------------------------
018200090813
018300090813       // - File TNTAM01L / TNOFM01L
018400090813     d k03tntam01    e ds                  extname(TNTAM01L : *key)
018500090813     d                                     prefix(k_)   inz
018600090812
018700090812       // - File TFNTC01L
018800090812     d k04tfntc01    e ds                  extname(TFNTC01L : *key)
018900090812     d                                     prefix(k_)   inz
019000090812
019100090812       //--------------------------------------------------------------
019200090812       //?M A I N - L I N E                                            ?
019300090812       //--------------------------------------------------------------
019400090812
019500090812     c     *Entry        plist
019600090812     c                   parm                    KPJBA
019700090812
019800090812      /free
019900090812
020000090812       //?Operazioni iniziali?
020100090812       exsr sr_RoutInz;
020200090812
020300090812       DoW  $Fine = *off;
020400090812         select;
020500090812           // - Gestione subfile S1
020600090812           when $Video = 'S1';
020700090812             exsr sr_GesS01;
020800090813           // - Gestione subfile S2
020900090813           when $Video = 'S2';
021000090813             exsr sr_GesS02;
021100090812           other;
021200090812             $Fine = *on;
021300090812         endsl;
021400090812       enddo;
021500090812
021600090812       //?Operazioni finali?
021700090812       exsr sr_RoutEnd;
021800090812
021900090812       //--------------------------------------------------------------
022000090812       //?Operazioni iniziali.                                         ?
022100090812       //--------------------------------------------------------------
022200090813       BEGSR  *InzSr;
022300090813
022400090813         //*inLR = *on;
022500090813
022600090813         //?Reperimento dati job?
022700090813         exsr  sr_DatiJob;
022800090813
022900090813         //?Caricamento schiera con tabella "TR" (tipo tariffa)?
023000090813         open  TABEL00F;
023100090813         clear  sch_CTR;
023200090813         clear  sch_DTR;
023300090813         clear  xx;
023400090813         TBLkut = 1;
023500090813         TBLcod = 'TR';
023600090813         setll  (TBLkut : TBLcod)  TABEL;
023700090813         reade  (TBLkut : TBLcod)  TABEL;
023800090813         DoW  not %eof(TABEL00F);
023900090813           if TBLflg = *blank   and   TBLkey <> *blank;
024000090813              xx = xx + 1 ;
024100090813              sch_CTR(xx) = TBLkey;
024200090813              sch_DTR(xx) = TBLuni;
024300090813           endif ;
024400090813           reade  (TBLkut : TBLcod)  TABEL;
024500090813         EndDo ;
024600090813         close  TABEL00F;
024700090813
024800090813       ENDSR;
024900090813
025000090813       //--------------------------------------------------------------
025100090813
025200090812       BEGSR  sr_RoutInz;
025300090812
025400090812         //?Ricezione parametri?
025500090812         TNTA29ds = kpjbu;
025600090812         oTA29err = *off;
025700090812         clear  oTA29msg;
025800090813
025900090813         reset  $Fine;
026000090813         reset  $InzS01;
026100090813         reset  $InzS02;
026200090813         reset  $Note;
026300090813         reset  $TariffaSenzaNote;
026400090813         reset  xx;
026500090813         reset  $Video;
026600090813         reset  W_SflPag;
026700090813         reset  wRecNote;
026800090813         reset  wCpyTar;
026900090812
027000090812         //?Controllo dei parametri ricevuti?
027100090812         select;
027200090915           when  iTA29opz <> '2'   and   iTA29opz <> '3'   and
027300090915                 iTA29opz <> '5';
027400090813             oTA29err = *on;
027500090813             oTA29msg = $Msg(01);
027600090812           when  iTA29tip <> 'T'   and   iTA29tip <> 'C';
027700090812             oTA29err = *on;
027800090813             oTA29msg = $Msg(02);
027900090812           when  iTA29cod = *zero;
028000090812             oTA29err = *on;
028100090812             if  iTA29tip = 'T';
028200090813               oTA29msg = $Msg(03);
028300090812             else;
028400090813               oTA29msg = $Msg(04);
028500090812             endif;
028600090812         endsl;
028700090813         select;
028800090813           when  oTA29err = *on;
028900090813             $Fine = *on;
029000090813             leavesr;
029100090813           when  iTA29opz = '2';
029200090813             $Video = 'S1';
029300090813           when  iTA29opz = '3';
029400090813             $Video = 'S2';
029500090813             select;
029600090813               when  iTA29tip = 'T'  and  NOT %open(TNOFM01L);
029700090813                 open  TNOFM01L;
029800090813               when  iTA29tip = 'C'  and  NOT %open(TNTAM01L);
029900090813                 open  TNTAM01L;
030000090813             endsl;
030100090813         endsl;
030200090812
030300090812         //?Impostazione nome programma a video?
030400090812         V1Tpgm = SDSpgm;
030500090812         clear  V1Topz;
030600090812
030700090812       ENDSR;
030800090812
030900090812       //--------------------------------------------------------------
031000090812       //?Reperimento Dati del job (Utente/Operativi).                 ?
031100090812       //--------------------------------------------------------------
031200090812       BEGSR  sr_DatiJob;
031300090812
031400090812         in(e) §AzUte;
031500090812         if NOT %error;
031600090812           in(e) §DatiUte;
031700090812         endif;
031800090812         if %error or RSut = *blank;
031900090812           tibs34r ( tibs34ds );
032000090812           in §AzUte;
032100090812           in §DatiUte;
032200090812         endif;
032300090812
032400090812       ENDSR;
032500090812
032600090812       //--------------------------------------------------------------
032700090812       //?Gestione subfile S01                                         ?
032800090812       //--------------------------------------------------------------
032900090812       BEGSR  sr_GesS01;
033000090812
033100090812         //?Inizializzazione subfile?
033200090812         if  $InzS01 = *on;
033300090812           exsr  sr_InzS01;
033400090812           $InzS01 = *off;
033500090812         endif;
033600090812
033700090812         //?Emissione Testata e Piede con tasti funzionali abilitati?
033800090812         write  TA29T01;
033900090812         write  TA29P01;
034000090812
034100090812         //?Posizionamento cursore?
034200090812         if  C1CsrRrn > *zero;
034300090812           C1RcdNbr = C1CsrRrn;
034400090812         else;
034500090812           C1RcdNbr = 1;
034600090812         endif;
034700090812
034800090812         //?Emissione videata?
034900090915         if  iTA29opz = '5';
035000090915           write  TA29C01;
035100090915           exfmt  Protect;
035200090915         else;
035300090915           exfmt  TA29C01;
035400090915         endif;
035500090813
035600090812         reset  ErrMessage;
035700090812         reset  ErrGenerico;
035800090812         clear  V1Dmsg;
035900090812
036000090812         SELECT;
036100090812
036200090812           //?F3=Fine?
036300090812           WHEN  dsp_aid = c_F03;
036400090812             oTA29fxx = '03';
036500090812             $Fine = *on;
036600090812
036700090812           //?F12=Ritorno?
036800090812           WHEN  dsp_aid = c_F12;
036900090812             oTA29fxx = '12';
037000090812             $Fine = *on;
037100090812
037200090812           //?F16=Annullamento?
037300090812           WHEN  dsp_aid = c_F16;
037400090812             oTA29fxx = '16';
037500090812             exsr  sr_F16S01;
037600090812             $Fine = *on;
037700090812
037800090812           //?Invio?
037900090812           OTHER;
038000090812             // -?Controllo immissione note?
038100090812             exsr  sr_CtrS01;
038200090812             select;
038300090812               // -?Rilevato errore?
038400090812               when  ErrGenerico;
038500090812                 leavesr;
038600090812               // -?F6=Conferma?
038700090812               when  dsp_aid = c_F06;
038800090812                 oTA29fxx = '06';
038900090812                 exsr  sr_F06S01;
039000090812                 $Fine = *on;
039100090812             endsl;
039200090812
039300090812         ENDSL;
039400090812
039500090812       ENDSR;
039600090812
039700090812       //--------------------------------------------------------------
039800090812       //?Inizializzazione subfile S01                                 ?
039900090812       //--------------------------------------------------------------
040000090812       BEGSR  sr_InzS01;
040100090812
040200090812         //?Spegnimento degli indicatori di errore?
040300090812         %subst(IndDspF : 50) = *off;
040400090812
040500090812         //?Impostazione campi nel subfile-control?
040600090812         if  iTA29tip = 'T';
040700090908           C1txt1 = 'Trattativa n°';
040800090908           C1txt2 = %editc(iTA29cod : 'Z');
040900090812         else;
041000090908           C1txt1 = 'Cliente ....:';
041100090908           C1txt2 = %editc(iTA29cod : 'X');
041200090812         endif;
041300090908         C1txt2 = %trimr(C1txt2)
041400090812                  + '  '
041500090812                  + iTA29rsc;
041600090812
041700090812         //?Pulizia subfile?
041800090812         SflDsp_N    = *on;
041900090812         SflDspCtl_N = *on;
042000090812         write  TA29C01;
042100090812         SflDspCtl_N = *off;
042200090812         SflEnd      = *off;
042300090812
042400090812         clear  C1RcdNbr;
042500090812         clear  C1CsrRrn;
042600090812         clear  S01nrr;
042700090812         clear  V1Dmsg;
042800090812         ErrMessage  = *off;
042900090812         ErrGenerico = *off;
043000090812
043100090812         //?Caricamento subfile con note "10"?
043200090812         if  iTA29tip = 'T';             // T = Trattativa
043300091119           k_NTCapl = 'T';
043400090812         else;                           // C = Tariffe cliente
043500090812           k_NTCapl = iTA29tip;
043600090812         endif;
043700090812         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( iTA29cod : 'X' );
043800091120         IF  iTA29ctr <> 'CLI';
043900091120           k_NTCnk2 = iTA29ctr;
044000091120         ELSE;
044100091120           clear k_NTCnk2;
044200091120         ENDIF;
044300090812         k_NTCtnt = c_TipoNota;
044400090812
044500090812         setll  %kds(k04tfntc01)  TFNTC;
044600090812         reade  %kds(k04tfntc01)  TFNTC;
044700090812
044800090812         exsr  sr_CaricaS01;
044900090812
045000090812         //?Impaginazione subfile?
045100090812         // -> forza l'impaginazione sul 1° rec. del subfile
045200090812         if S01nrr > *zero;
045300090812           C1RcdNbr  = 1;
045400090812           C1CsrRrn  = 1;
045500090812         else;
045600090812           clear C1RcdNbr;
045700090812         endif;
045800090812
045900090812         //?Impostazione testata?
046000090812         select;
046100090813           when  iTA29opz = '2'   and   Not $Note;
046200090812             V1Topz = '  INSERIMENTO  ';
046300090813           when  iTA29opz = '2'   and   $Note;
046400090812             V1Topz = ' AGGIORNAMENTO ';
046500090813           when  iTA29opz = '3';
046600090812             V1Topz = '     COPIA     ';
046700090812         endsl;
046800090813
046900090813         //?Abilitazione tasti funzionali?
047000090915         AbilitF6  = (iTA29opz <> '5');
047100090813         AbilitF16 = (iTA29opz = '2'   and   $Note);
047200090812
047300090812       ENDSR;
047400090812
047500090812       //--------------------------------------------------------------
047600090812       //?Caricamento totale del subfile S01                           ?
047700090812       //--------------------------------------------------------------
047800090812       BEGSR  sr_CaricaS01;
047900090812
048000090812         SflNxtChg = *off;
048100090812
048200090812         //?Ciclo di caricamento (completo) dati dal file TFNTC01L?
048300090812         DoW  Not %eof(TFNTC01L)  and  S01nrr < c_MaxRec;
048400090812
048500090812           // -?Registrazione singolo record nel subfile?
048600090812           clear  TA29S01;
048700090812           S1Cctr = NTCnk2;
048800091120           IF  NTCnk2 = *blanks;
048900091120             S1Cctr = 'CLI';
049000091120           ENDIF;
049100090812           S1Cnot = NTCrnt;
049200090812           // - Campi hidden
049300090916           H1in40 = (S01nrr = *zero   or   %rem(S01nrr : 4) = *zero);
049400090812
049500090812           // -?Impostazione attributi di visualizzazione?
049600090812           exsr  sr_AttrS01;
049700090812           SflNxtChg = *on;
049800090812
049900090915           // -?Scrittura record nel subfile?
050000090812           S01nrr += 1;
050100090812           write  TA29S01;
050200090812
050300090812           // -?Lettura record successivo?
050400090812           reade  %kds(k04tfntc01)  TFNTC;
050500090812
050600090812         EndDo;
050700090812
050800090812         //?Verifica se INSERIMENTO?
050900090812         $Note = (S01nrr > *zero);
051000090812
051100090812         //?Completamento subfile con records vuoti?
051200090812         //?(se reperiti meno di 4 rec.)?
051300090813         DoW  S01nrr < c_MaxRec   and   iTA29opz = '2';
051400090812
051500090812           // -?Registrazione singolo record nel subfile?
051600090812           clear  TA29S01;
051700090812           S1Cctr = iTA29ctr;
051800090812           // - Campi hidden
051900090916           H1in40 = (S01nrr = *zero   or   %rem(S01nrr : 4) = *zero);
052000090812
052100090812           // -?Impostazione attributi di visualizzazione?
052200090812           exsr  sr_AttrS01;
052300090812           SflNxtChg = *off;
052400090812
052500090812           // -?Scrittura record nerl subfile?
052600090812           S01nrr += 1;
052700090812           write  TA29S01;
052800090812
052900090812         EndDo;
053000090812
053100090812         //?Visualizzazione del SFL (se ci sono dati)?
053200090812         SflDsp_N  = (S01nrr <= *zero);
053300090812
053400090812         //?Attivazione del SFLEND?
053500090812         SflEnd = (S01nrr  >= c_MaxRec  or  %eof(TFNTC01L));
053600090812
053700090812         //?Posizionamento cursore al 1° rcd della pagina?
053800090812         C1RcdNbr = 1;
053900090812         C1CsrRrn = C1RcdNbr;
054000090812
054100090812         //?Segnalazione presenza di ulteriori record?
054200090812         if  S01nrr >= c_MaxRec   and   NOT %eof(TFNTC01L);
054300090813           V1Dmsg = $Msg(05);
054400090812           ErrMessage  = *on;
054500090812           ErrGenerico = *on;
054600090812           leavesr;
054700090812         endif;
054800090812
054900090812       ENDSR;
055000090812
055100090812       //--------------------------------------------------------------
055200090812       //?Impostazione attributi di visualizzazione nel rec. del sfl   ?
055300090812       //--------------------------------------------------------------
055400090812       BEGSR  sr_AttrS01;
055500090812
055600090812         // - Visualizzazione codice tariffa
055700090813         VisualizCTR = (H1in40 = *on);
055800090813
055900090813         // - Normalizza gli attributi di visualizzazione x riga Note
056000090813         NormalizNOT = (iTA29opz = '3');
056100090812
056200090812       ENDSR;
056300090812
056400090812       //--------------------------------------------------------------
056500090812       //?Controllo dati nel subfile S01                               ?
056600090812       //--------------------------------------------------------------
056700090812       BEGSR  sr_CtrS01;
056800090812
056900090813         //clear  SavS01csr;
057000090812         clear  wRecNote;
057100090812
057200090812         //?Ciclo di lettura subfile?
057300090812         readc  TA29S01;
057400090812
057500090812         DoW  Not %eof(TNTA29D);
057600090812
057700090812           SflNxtChg = (S1Cnot <> *blank);
057800090812
057900090812           // -?ReImpostazione attributi di visualizzazione?
058000090812           exsr  sr_AttrS01;
058100090812
058200090812           // -?Aggiornamento sfl?
058300090812           UPDATE  TA29S01;
058400090812
058500090812           // -?Uscita al 1° record con nota <> *blank?
058600090812           if  S1Cnot <> *blank;
058700090812             wRecNote += 1;
058800090813             //SavS01csr = S01nrr;
058900090812             leave;
059000090812           endif;
059100090812
059200090812           readc  TA29S01;
059300090812
059400090812         EndDo;
059500090812
059600090812         // -?Riposizionamento cursore sul record contenente la 1ª nota?
059700090812         //if  SavS01csr > *zero;
059800090812         //  C1CsrRrn = SavS01csr;
059900090812         //endif;
060000090812
060100090812         // -?Segnalazione eventuale errore?
060200090812         if  dsp_aid = c_F06   and   wRecNote = *zero;
060300090813           V1Dmsg = $Msg(06);
060400090812           ErrMessage  = *on;
060500090812           ErrGenerico = *on;
060600090812           leavesr;
060700090812         endif;
060800090812
060900090812       ENDSR;
061000090812
061100090812       //--------------------------------------------------------------
061200090813       //?Gestione tasto funzionale F6=Conferma da sfl S01             ?
061300090812       //--------------------------------------------------------------
061400090812       BEGSR  sr_F06S01;
061500090812
061600090812         //?Impostazione chiave di accesso al file TFNTC01L?
061700090812         //?&  Cancellazione record esistenti?
061800090812         exsr  sr_F16S01;
061900090812
062000090812         //?Ciclo di lettura subfile per scrittura file?
062100090812         For  S01nrr = 1  To  c_MaxRec;
062200090812
062300090812           chain  S01nrr  TA29S01;
062400090812
062500090812           select;
062600090812             when  Not %found(TNTA29D);
062700090812               leave;
062800090812             when  S1Cnot = *blank;
062900090812               iter;
063000090812           endsl;
063100090812
063200090812           NTCapl = k_NTCapl;
063300090812           NTCnk1 = k_NTCnk1;
063400090812           NTCnk2 = k_NTCnk2;
063500090812           NTCtnt = c_TipoNota;
063600090812           NTCrnt = S1Cnot;
063700090812           NTCsns = 'S';
063800090812
063900090812           WRITE   TFNTC;
064000090812           //¯¯¯¯¯¯¯¯¯¯¯¯
064100090812
064200090812         EndFor;
064300090812
064400090812       ENDSR;
064500090812
064600090812       //--------------------------------------------------------------
064700090812       //?Gestione tasto funzionale F16=Annullamento                   ?
064800090812       //--------------------------------------------------------------
064900090812       BEGSR  sr_F16S01;
065000090812
065100090812         //?Impostazione chiave di accesso al file TFNTC01L?
065200090812         if  iTA29tip = 'T';           // T = Trattativa
065300091119           k_NTCapl = 'T';
065400090812         else;                         // C = Tariffe cliente
065500090812           k_NTCapl = iTA29tip;
065600090812         endif;
065700090812         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( iTA29cod : 'X' );
065800091120         IF  iTA29ctr <> 'CLI';
065900091120           k_NTCnk2 = iTA29ctr;
066000091120         ELSE;
066100091120           clear k_NTCnk2;
066200091120         ENDIF;
066300090812
066400090812         //?Cancellazione records esistenti?
066500090812         setll  %kds(k04tfntc01)  TFNTC;
066600090812
066700090812         If  %equal(TFNTC01L);
066800090812
066900090812           DoU  NOT %found(TFNTC01L);
067000090812
067100090812             DELETE  %kds(k04tfntc01)  TFNTC;
067200090812             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
067300090812
067400090812           EndDo;
067500090812
067600090812         EndIf;
067700090812
067800090812       ENDSR;
067900090813
068000090813       //--------------------------------------------------------------
068100090813       //?Gestione subfile S02                                         ?
068200090813       //--------------------------------------------------------------
068300090813       BEGSR  sr_GesS02;
068400090813
068500090813         //?Inizializzazione subfile?
068600090813         if  $InzS02 = *on;
068700090813           if  $InzS01 = *on;
068800090813             exsr  sr_InzS01;
068900090813             $InzS01 = *off;
069000090813           endif;
069100090813           exsr  sr_InzS02;
069200090813           $InzS02 = *off;
069300090813         endif;
069400090813
069500090813         //?Emissione Testata e Piede con tasti funzionali abilitati?
069600090813         if  Not ErrGenerico;
069700090813           write  TA29T01;
069800090813           write  TA29P01;
069900090813           C1RcdNbr = 1;
070000090813           write  TA29C01;
070100090915           write  Protect;
070200090813         endif;
070300090813
070400090813         //?Emissione videata?
070500090915         exfmt  TA29C02;
070600090813
070700090813         reset  ErrMessage;
070800090813         reset  ErrGenerico;
070900090813         clear  V1Dmsg;
071000090813
071100090813         SELECT;
071200090813
071300090813           //?F3=Fine?
071400090813           WHEN  dsp_aid = c_F03;
071500090813             oTA29fxx = '03';
071600090813             $Fine = *on;
071700090813
071800090813           //?F12=Ritorno?
071900090813           WHEN  dsp_aid = c_F12;
072000090813             oTA29fxx = '12';
072100090813             $Fine = *on;
072200090814
072300090814           //?Subfile vuoto?
072400090814           WHEN  S02nrr = *zero;
072500090813
072600090813           //?Invio?
072700090813           OTHER;
072800090813             // -?Gestione opzioni ("1")?
072900090813             exsr  sr_OpzS02;
073000090813             select;
073100090813               // -?Rilevato errore?
073200090813               when  ErrGenerico;
073300090813                 leavesr;
073400090813               // -?F6=Conferma?
073500090813               when  dsp_aid = c_F06;
073600090813                 oTA29fxx = '06';
073700090813                 exsr  sr_F06S02;
073800090813                 $Fine = *on;
073900090813             endsl;
074000090813
074100090813         ENDSL;
074200090813
074300090813       ENDSR;
074400090813
074500090813       //--------------------------------------------------------------
074600090813       //?Inizializzazione subfile S02                                 ?
074700090813       //--------------------------------------------------------------
074800090813       BEGSR  sr_InzS02;
074900090813
075000090813         //?Spegnimento degli indicatori di errore?
075100090813         %subst(IndDspF : 50) = *off;
075200090813
075300090813         //?Pulizia subfile?
075400090813         SflDsp_N2    = *on;
075500090813         SflDspCtl_N2 = *on;
075600090813         write  TA29C02;
075700090813         SflDspCtl_N2 = *off;
075800090813         SflEnd2      = *off;
075900090813
076000090813         clear  C2RcdNbr;
076100090813         clear  C2CsrRrn;
076200090813         clear  S02nrr;
076300090813         clear  V1Dmsg;
076400090813         ErrMessage  = *off;
076500090813         ErrGenerico = *off;
076600090813
076700090813         //?Caricamento subfile tariffe senza note?
076800090813         k_TAMksc = iTA29cod;
076900090813         k_TAMctr = *loval;
077000090813         k_TAMprg = *loval;
077100090813         if  iTA29tip = 'T';
077200090813           setll  %kds(k03tntam01)  TNOFM000;
077300090813         else;
077400090813           setll  %kds(k03tntam01)  TNTAM000;
077500090813         endif;
077600090813         exsr  sr_ReadTNTAM;
077700090813
077800090813         exsr  sr_RollUpS02;
077900090813
078000090813         //?Impaginazione subfile?
078100090813         // -> forza l'impaginazione sul 1° rec. del subfile
078200090813         if S02nrr > *zero;
078300090813           C2RcdNbr  = 1;
078400090813           C2CsrRrn  = 1;
078500090813         else;
078600090813           clear C2RcdNbr;
078700090813         endif;
078800090814
078900090814         //?Abilitazione tasti funzionali?
079000090915         AbilitF6  = (S02nrr > *zero);
079100090813
079200090813       ENDSR;
079300090813
079400090813       //--------------------------------------------------------------
079500090813       //?Lettura singolo record dal file TNTAM01L / TNOFM01L          ?
079600090813       //--------------------------------------------------------------
079700090813       BEGSR  sr_ReadTNTAM;
079800090813
079900090813         reset  $TariffaSenzaNote;
080000090813
080100090813         //?Lettura file TNTAM01L / TNOFM01L?
080200090813         DoW  not $TariffaSenzaNote;
080300090813
080400090813           // -?Lettura 1° ed unico record di tariffa?
080500090813           if  iTA29tip = 'T';
080600090813             reade  %kds(k03tntam01 : 1)  TNOFM000;
080700090813           else;
080800090813             reade  %kds(k03tntam01 : 1)  TNTAM000;
080900090813           endif;
081000090813
081100090813           select;
081200090813             when  (iTA29tip = 'T'  and  %eof(TNOFM01L)  or
081300090813                    iTA29tip = 'C'  and  %eof(TNTAM01L));
081400090813               leave;
081500090813             when  TAMatb <> *blank;
081600090813               iter;
081700090813           endsl;
081800090813
081900090813           // -?Verifica esistenza note per la tariffa?
082000090813           exsr  sr_ReadTFNTC;
082100090813           k_TAMctr = TAMctr;
082200090813           k_TAMprg = *hival;
082300090813           if  iTA29tip = 'T';
082400090813             setgt  %kds(k03tntam01 : 2)  TNOFM000;
082500090813           else;
082600090813             setgt  %kds(k03tntam01 : 2)  TNTAM000;
082700090813           endif;
082800090813
082900090813         EndDo;
083000090813
083100090813       ENDSR;
083200090813
083300090813       //--------------------------------------------------------------
083400090813       //?Ricerca eventuali Note per la Tariffa in esame               ?
083500090813       //--------------------------------------------------------------
083600090813       BEGSR  sr_ReadTFNTC;
083700090813
083800090813         if  iTA29tip = 'T';           // T = Trattativa
083900091119           k_NTCapl = 'T';
084000090813         else;                         // C = Tariffe cliente
084100090813           k_NTCapl = iTA29tip;
084200090813         endif;
084300090813         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( TAMksc : 'X' );
084400090813         k_NTCnk2 = %editc( TAMctr : 'X' );
084500090813         k_NTCtnt = c_TipoNota;
084600090813
084700090813         setll  %kds(k04tfntc01)  TFNTC;
084800090813
084900090813         if  NOT %equal(TFNTC01L);
085000090813           $TariffaSenzaNote = *on;
085100090813         endif;
085200090813
085300090813       ENDSR;
085400090813
085500090813       //--------------------------------------------------------------
085600090813       //?Caricamento singola pagina nel subfile S01                   ?
085700090813       //--------------------------------------------------------------
085800090813       BEGSR  sr_RollUpS02;
085900090813
086000090813         S02nrr     = (W_SflPag * c_SflPag2);
086100090813         W_SflPag  += 1;
086200090813         SflNxtChg2 = *off;
086300090813
086400090813         //?Ciclo di caricamento dati dai files?
086500090813         //?TNOFM01L/TNTAM01L e TFNTC01L?
086600090813         DoW  S02nrr   < (W_SflPag * c_SflPag2)   and
086700090813              S02nrr   < c_MaxRec2                and
086800090813             (iTA29tip = 'T'  and  not %eof(TNOFM01L)  or
086900090813              iTA29tip = 'C'  and  not %eof(TNTAM01L));
087000090813
087100090813           // -?Registrazione singolo record nel subfile?
087200090813           clear  TA29S02;
087300090813           S2Cctr = TAMctr;
087400090813           xx = %lookup( %subst( %editc( TAMctr : 'X' ) : 1 : 1 )
087500090813                         : sch_CTR);
087600090813           if xx > *zero;
087700090813             S2Dctr = sch_DTR(xx);
087800090813           endif ;
087900090813           S2Cdcv = TAMdcv;
088000090813
088100090813           // -?Scrittura record nerl subfile?
088200090813           S02nrr += 1;
088300090813           write  TA29S02;
088400090813
088500090813           // -?Lettura record successivo?
088600090813           exsr  sr_ReadTNTAM;
088700090813
088800090813         EndDo;
088900090813
089000090813         //?Visualizzazione del SFL (se ci sono dati)?
089100090813         SflDsp_N2  = (S02nrr <= *zero);
089200090813
089300090813         //?Attivazione del SFLEND?
089400090813         SflEnd2 = (S02nrr  >= c_MaxRec2   or
089500090813                   (iTA29tip = 'T'  and  %eof(TNOFM01L)  or
089600090813                    iTA29tip = 'C'  and  %eof(TNTAM01L)));
089700090813         if  S02nrr >= c_MaxRec2  and
089800090813            (iTA29tip = 'T'  and  NOT %eof(TNOFM01L)  or
089900090813             iTA29tip = 'C'  and  NOT %eof(TNTAM01L));
090000090813           V1Dmsg = $Msg(07);
090100090813           ErrMessage  = *on;
090200090813           ErrGenerico = *on;
090300090813         endif;
090400090813
090500090813         //?Posizionamento cursore al 1° rcd della pagina?
090600090813         if  S02nrr > *zero;
090700090813           wPag     = S02nrr / c_SflPag2;
090800090813           wRig     = S02nrr - (c_SflPag2 * wPag);
090900090813           C2RcdNbr = wPag * c_SflPag2;
091000090813           if  wRig > *zero;
091100090813             C2RcdNbr += 1;
091200090813           else;
091300090813             C2RcdNbr = C2RcdNbr - c_SflPag2 + 1;
091400090813           endif;
091500090813         else;
091600090813           clear  C2RcdNbr;
091700090813         endif;
091800090813
091900090813         C2CsrRrn = C2RcdNbr;
092000090813
092100090813       ENDSR;
092200090813
092300090813       //--------------------------------------------------------------
092400090813       //?Gestione opzioni del subfile S02                             ?
092500090813       //--------------------------------------------------------------
092600090813       BEGSR  sr_OpzS02;
092700090813
092800090813         //clear  SavS02csr;
092900090813         clear  wCpyTar;
093000090813
093100090813         //?Ciclo di lettura subfile?
093200090813         readc  TA29S02;
093300090813
093400090813         DoW  Not %eof(TNTA29D);
093500090813
093600090813           SflNxtChg2 = *off;
093700090813           %subst(IndDspF : 50) = *off;
093800090813           C2RcdNbr = S02nrr;
093900090813
094000090813           select;
094100090813             // -?Opzione errata?
094200090813             when  S2Copz <> *blank   and   S2Copz <> '1';
094300090813               ErrMessage  = *on;
094400090813               ErrGenerico = *on;
094500090813               PosCurOPZ2  = *on;
094600090813               V1Dmsg = $Msg(08);
094700090813             // -?Nessuna opzione?
094800090813             when  S2Copz = *blank;
094900090813             // -?Conteggio delle opzioni "1"?
095000090813             when  S2Copz = '1';
095100090813               wCpyTar += 1;
095200090813           endsl;
095300090813
095400090813           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
095500090813           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
095600090813           //if  S2Copz  <> *blank   and
095700090813           //   (SavS02csr = *zeros   or   SavS02csr < C2RcdNbr);
095800090813           //  SavS02csr   = C2RcdNbr;
095900090813           //endif;
096000090813
096100090813           // -?Aggiornamento sfl?
096200090813           SflNxtChg2 = *on;
096300090813           select;
096400090813             when  ErrMessage;
096500090813               C2CsrRrn   = C2RcdNbr;
096600090813             when  ErrGenerico;
096700090813               C2CsrRrn   = C2RcdNbr;
096800090813               clear  S2Copz;
096900090813             when  S2Copz = *blank;
097000090813               SflNxtChg2 = *off;
097100090813           endsl;
097200090813           UPDATE  TA29S02;
097300090813           if  ErrGenerico   or   ErrMessage;
097400090813             leave;
097500090813           endif;
097600090813
097700090813           readc  TA29S02;
097800090813
097900090813         ENDDO;
098000090813
098100090813         // -?Riposiziono il cursore sul record contenente la prima opz.?
098200090813         //  ?(se non sono stati rilevati errori)?
098300090813         //if  NOT ErrMessage   and   NOT ErrGenerico   and
098400090813         //    SavS02csr > *zero;
098500090813         //  C2CsrRrn = SavS02csr;
098600090813         //endif;
098700090813
098800090813         // -?Segnalazione eventuale errore?
098900090813         if  Not ErrGenerico   and   wCpyTar = *zero
099000090813                               and   dsp_aid = c_F06;
099100090813           V1Dmsg = $Msg(09);
099200090813           ErrMessage  = *on;
099300090813           ErrGenerico = *on;
099400090813           leavesr;
099500090813         endif;
099600090813
099700090813       ENDSR;
099800090813
099900090813       //--------------------------------------------------------------
100000090813       //?Gestione tasto funzionale F6=Conferma da sfl S02             ?
100100090813       //--------------------------------------------------------------
100200090813       BEGSR  sr_F06S02;
100300090813
100400090813         //?Ciclo di lettura subfile per scrittura file?
100500090813         For  S02nrr = 1  To  c_MaxRec2;
100600090813
100700090813           chain  S02nrr  TA29S02;
100800090813
100900090813           select;
101000090813             when  Not %found(TNTA29D);
101100090813               leave;
101200090813             when  S2Copz = *blank;
101300090813               iter;
101400090813           endsl;
101500090813
101600090813           NTCapl = k_NTCapl;
101700090813           NTCnk1 = k_NTCnk1;
101800090813           NTCnk2 = %editc( S2Cctr : 'X' );
101900090813           NTCtnt = c_TipoNota;
102000090813           NTCsns = 'S';
102100090813
102200090813           For  S01nrr = 1  To  c_MaxRec;
102300090813             chain  S01nrr  TA29S01;
102400090813             if  NOT %found(TNTA29D);
102500090813               leave;
102600090813             endif;
102700090813             if  S1Cnot <> *blank;
102800090813               NTCrnt = S1Cnot;
102900090813               //____________
103000090813               WRITE   TFNTC;
103100090813               //¯¯¯¯¯¯¯¯¯¯¯¯
103200090813             endif;
103300090813           EndFor;
103400090813
103500090813         EndFor;
103600090813
103700090813       ENDSR;
103800090812
103900090812       //--------------------------------------------------------------
104000090812       //?Operazioni finali.                                           ?
104100090812       //--------------------------------------------------------------
104200090812       BEGSR  sr_RoutEnd;
104300090812
104400090812         kpjbu = TNTA29ds;
104500090812
104600090812         return;
104700090812
104800090812       ENDSR;
104900090812
105000090812      /end-free
105100090812
105200090812** - $Msg -------------------------------------------------------------------*
105300090813Richiesta una gestione non ammessa. Ammesse solo G=Gestione e C=Copia.         1
105400090813Tipo richiamo errato. Ammessi: "T"=Trattativa e "C"=Tariffe clienti.           2
105500090813Manca il numero trattativa                                                     3
105600090813Manca il codice cliente                                                        4
105700090813Reperite altre note per la tariffa in esame. Contattare il CED.                5
105800090813Inserire almeno una nota                                                       6
105900090813Raggiunto il limite massimo di record visualizzabili...                        7
106000090813Opzione errata                                                                 8
106100090813Selezionare almeno una tariffa o premere F12.                                  9
