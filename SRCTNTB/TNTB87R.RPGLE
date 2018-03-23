000100110505       //==============================================================
000200110505       //?TNTB87R * Copia tab. "01" = Agenti Commerciali               ?
000300110505       //==============================================================
000400110505
000500110505       //--------------------------------------------------------------
000600110505       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110505       //--------------------------------------------------------------
000800110505
000900110505     /*PRM dbgview(*source)
001000110505     /*END
001100110505
001200110505       //--------------------------------------------------------------
001300110505       //?Specifiche di controllo.                                     ?
001400110505       //--------------------------------------------------------------
001500110505
001600110505     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110505
001800110505       //--------------------------------------------------------------
001900110505       //?Dichiarazione file.                                          ?
002000110505       //--------------------------------------------------------------
002100110505
002200110505       // -?Tabella "CPO"?
002300110505     fTNTBE01L  if   e           k disk
002400110505
002500110505       // -?Video Gestione?
002600110505     fTNTB87D   cf   e             workstn
002700110505     f                                     sfile(TB87S01 : S01nrr)
002800110505     f                                     indds(IndDspF)
002900110505     f                                     infds(InfDspF)
003000110505
003100110505       //--------------------------------------------------------------
003200110505       //?Definizione costanti.                                        ?
003300110505       //--------------------------------------------------------------
003400110505
003500110505       // -?Codice tabella in gestione?
003600110505     d c_Tab           c                   const('CPO')
003700110505
003800110505       // -?Numero di record in ciascuna videata di subfile?
003900110505     d c_SflPag        c                   const(08)
004000110505
004100110505       // -?Numero massimo di record gestibili?
004200110505     d c_MaxRec        c                   const(9999)
004300110505
004400110505       // -?Tasti funzionali a video?
004500110505     d c_F01           c                   const(x'31')
004600110505     d c_F02           c                   const(x'32')
004700110505     d c_F03           c                   const(x'33')
004800110505     d c_F04           c                   const(x'34')
004900110505     d c_F05           c                   const(x'35')
005000110505     d c_F06           c                   const(x'36')
005100110505     d c_F07           c                   const(x'37')
005200110505     d c_F08           c                   const(x'38')
005300110505     d c_F09           c                   const(x'39')
005400110505     d c_F10           c                   const(x'3A')
005500110505     d c_F11           c                   const(x'3B')
005600110505     d c_F12           c                   const(x'3C')
005700110505     d c_F13           c                   const(x'B1')
005800110505     d c_F14           c                   const(x'B2')
005900110505     d c_F15           c                   const(x'B3')
006000110505     d c_F16           c                   const(x'B4')
006100110505     d c_F17           c                   const(x'B5')
006200110505     d c_F18           c                   const(x'B6')
006300110505     d c_F19           c                   const(x'B7')
006400110505     d c_F20           c                   const(x'B8')
006500110505     d c_F21           c                   const(x'B9')
006600110505     d c_F22           c                   const(x'BA')
006700110505     d c_F23           c                   const(x'BB')
006800110505     d c_F24           c                   const(x'BC')
006900110505     d c_Enter         c                   const(x'F1')
007000110505     d c_RollDown      c                   const(x'F4')
007100110505     d c_RollUp        c                   const(x'F5')
007200110505
007300110505       //--------------------------------------------------------------
007400110505       //?Definizione schiere.                                         ?
007500110505       //--------------------------------------------------------------
007600110505
007700110505       // -?Messaggi di errore?
007800110505     d $Msg            s             78    dim( 2) ctdata perrcd(1)
007900110505
008000110505       //--------------------------------------------------------------
008100110505       //?Definizione aree dati.                                       ?
008200110505       //--------------------------------------------------------------
008300110505
008400110505
008500110505       //--------------------------------------------------------------
008600110505       //?Definizione strutture dati.                                  ?
008700110505       //--------------------------------------------------------------
008800110505
008900110505       // -?Status ds?
009000110505     d Status         sds
009100110505     d  SDSpgm           *proc
009200110505
009300110505       // -?InfDS?
009400110505     d InfDspF         ds
009500110505     d   dsp_aid             369    369a
009600110505
009700110505       // -?Indicatori su DspF?
009800110505     d IndDspF         ds                  inz
009900110505         // -?Abilitazione tasti funzionali?
010000110505         // -?Emissione messaggio di errore?
010100110505     d   ErrMessage                    n   overlay(IndDspF : 28)
010200110505         // -?Indicatori di gestione del subfile?
010300110505     d   SflDsp_N                      n   overlay(IndDspF : 30)
010400110505     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
010500110505     d   SflNxtChg                     n   overlay(IndDspF : 32)
010600110505     d   SflEnd                        n   overlay(IndDspF : 33)
010700110505         // -?Attributi di visualizzazione?
010800110505         // -?Posizionamento cursore & Segnalazione errore?
010900110505     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011000110505         // -?Riemissione videata?
011100110505     d   ErrGenerico                   n   overlay(IndDspF : 99)
011200110505
011300110505       // -?Parametri ricevuti?
011400110505     d TNTB87ds      e ds
011500110505
011600110505       // ·?Tabella "CPO" = Categoria Potenziale?
011700110505     d dCPO          e ds                  inz
011800110505
011900110505       //--------------------------------------------------------------
012000110505       //?Definizione variabili globali.                               ?
012100110505       //--------------------------------------------------------------
012200110505
012300110505       // -?Flags booleani?
012400110505     d $Fine           s               n   inz
012500110505     d $InzS01         s               n   inz(*on)
012600110505
012700110505       // -?Variabili per la gestione del video?
012800110505     d $Video          s              2    inz('S1')
012900110505     d W_SflPag        s              3  0 inz
013000110505     d wPag            s              4  0 inz
013100110505     d wRig            s              3  0 inz
013200110505     d S01nrr          s                   like(C1RcdNbr) inz
013300110505     d SavS01csr       s                   like(C1RcdNbr) inz
013400110505
013500110505       // -?Conteggio opzioni inserite?
013600110505     d wCountOpz       s              3  0 inz
013700110505
013800110505       //--------------------------------------------------------------
013900110505       //?Definizione prototipi procedure.                             ?
014000110505       //--------------------------------------------------------------
014100110505
014200110505
014300110505       //--------------------------------------------------------------
014400110505       //?Definizione key-list.                                        ?
014500110505       //--------------------------------------------------------------
014600110505
014700110505       // -?File TNTBE01L?
014800110505     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
014900110505     d                                     prefix(k_)   inz
015000110505
015100110505       //--------------------------------------------------------------
015200110505       //?Specifiche di calcolo.                                       ?
015300110505       //--------------------------------------------------------------
015400110505
015500110505     c     *Entry        plist
015600110505     c                   parm                    TNTB87ds
015700110505
015800110505      /free
015900110505
016000110505       // -?Operazioni iniziali?
016100110505       exsr sr_RoutInz;
016200110505
016300110505       // -?Gestione videate?
016400110505       DoW  $Fine = *off;
016500110505
016600110505         select;
016700110505           // -?Gestione subfile S1 (selezione)?
016800110505           when  $Video = 'S1';
016900110505             exsr  sr_GesS01;
017000110505           // -?? ? ? ? ??
017100110505           other;
017200110505             $Fine = *on;
017300110505         endsl;
017400110505
017500110505       enddo;
017600110505
017700110505       // -?Operazioni finali?
017800110505       exsr sr_RoutEnd;
017900110505
018000110505       //--------------------------------------------------------------
018100110505       //?Operazioni iniziali.                                         ?
018200110505       //--------------------------------------------------------------
018300110505       BEGSR  sr_RoutInz;
018400110505
018500110505         *inLR = *on;
018600110505
018700110505         // -?Impostazione nome programma a video?
018800110505         V1Tpgm = SDSpgm;
018900110505
019000110505         // -?Impostazione campi "fissi"?
019100110505         clear  k05tntbe01;
019200110505         k_TBEcod = c_Tab;
019300110505
019400110505         // -?Pulizia parametri da restituire?
019500110505         clear  TNTB87ds;
019600110505         TB87err = *off;
019700110505
019800110505       ENDSR;
019900110505
020000110505       //--------------------------------------------------------------
020100110505       //?Gestione subfile S01 e subfile-control C01.                  ?
020200110505       //--------------------------------------------------------------
020300110505       BEGSR  sr_GesS01;
020400110505
020500110505         // -?Inizializzazione subfile?
020600110505         if  $InzS01 = *on;
020700110505           exsr  sr_InzS01;
020800110505           $InzS01 = *off;
020900110505         endif;
021000110505
021100110505         // -?Emissione Testata e Piede (window)?
021200110505         //write  TB87T01;
021300110505         write  TB87P01;
021400110505
021500110505         // -?Posizionamento cursore?
021600110505         if  C1CsrRrn > *zero;
021700110505           C1RcdNbr = C1CsrRrn;
021800110505         else;
021900110505           C1RcdNbr = 1;
022000110505           write  TB87S00;             //?(no rec.)?
022100110505         endif;
022200110505
022300110505         // -?Emissione videata?
022400110505         exfmt  TB87C01;
022500110505
022600110505         clear  V1Dmsg;
022700110505         reset  ErrMessage;
022800110505         reset  ErrGenerico;
022900110505
023000110505         SELECT;
023100110505
023200110505           // -?F3=Fine?
023300110505           //WHEN  dsp_aid = c_F03;
023400110505           //  TB87fxx = 'F03';
023500110505           //  $Fine = *on;
023600110505
023700110505           // -?F12=Ritorno?
023800110505           WHEN  dsp_aid = c_F12;
023900110505             TB87fxx = 'F12';
024000110505             $Fine = *on;
024100110505
024200110505           // -?Roll-Up?
024300110505           WHEN  dsp_aid = c_RollUp;
024400110505             exsr sr_RollUpS01;
024500110505
024600110505           // -?SubFile vuoto?
024700110505           WHEN  S01nrr = *zero;
024800110505
024900110505           // -?Invio?
025000110505           OTHER;
025100110505             exsr  sr_OpzS01;
025200110505             select;
025300110505               when  ErrGenerico;
025400110505                 leavesr;
025500110505               when  wCountOpz = 1;
025600110505                 $Fine = *on;
025700110505                 leavesr;
025800110505             endsl;
025900110505
026000110505         ENDSL;
026100110505
026200110505       ENDSR;
026300110505
026400110505       //--------------------------------------------------------------
026500110505       //?Inizializzazione subfile S01                                 ?
026600110505       //--------------------------------------------------------------
026700110505       BEGSR  sr_InzS01;
026800110505
026900110505         // -?Spegnimento degli indicatori di errore?
027000110505         %subst(IndDspF : 50) = *off;
027100110505
027200110505         // -?Pulizia subfile?
027300110505         SflDsp_N    = *on;
027400110505         SflDspCtl_N = *on;
027500110505         write  TB87C01;
027600110505         SflDspCtl_N = *off;
027700110505         SflEnd      = *off;
027800110505
027900110505         clear  W_SflPag;
028000110505         clear  C1RcdNbr;
028100110505         clear  C1CsrRrn;
028200110505         clear  S01nrr;
028300110505         clear  V1Dmsg;
028400110505         ErrMessage  = *off;
028500110505         ErrGenerico = *off;
028600110505
028700110505         // -?Caricamento subfile a pagine?
028800110505         k_TBEke1 = *loval;
028900110505         k_TBEke2 = *loval;
029000110505         setll  %kds(k05tntbe01)  TNTBE000;
029100110505         reade  %kds(k05tntbe01 : 1)  TNTBE000;
029200110505         exsr  sr_RollUpS01;
029300110505
029400110505         // -?Impaginazione subfile?
029500110505         //?-> forza l'impaginazione sul 1° rec. del subfile?
029600110505         if S01nrr > *zero;
029700110505           C1RcdNbr  = 1;
029800110505           C1CsrRrn  = 1;
029900110505         else;
030000110505           clear C1RcdNbr;
030100110505         endif;
030200110505
030300110505       ENDSR;
030400110505
030500110505       //--------------------------------------------------------------
030600110505       //?Caricamento singola pagina nel subfile S01                   ?
030700110505       //--------------------------------------------------------------
030800110505       BEGSR  sr_RollUpS01;
030900110505
031000110505         S01nrr    = (W_SflPag * c_SflPag);
031100110505         W_SflPag += 1;
031200110505         SflNxtChg = *off;
031300110505
031400110505         // -?Ciclo di caricamento dati dal file TABEL00F?
031500110505         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
031600110505              S01nrr    < c_MaxRec                and
031700110505              Not  %eof(TNTBE01L);
031800110505
031900110505           dCPO = TBEuni;
032000110505
032100110505           // -?Registrazione singolo record nel subfile?
032200110505           if  TBEatb = *blank;
032300110505
032400110505             // -?Caricamento dati nel record del subfile?
032500110505             clear  TB87S01;
032600110505             S1Ccod = TBEke1;
032700110505             S1Ddes = §CPOdes;
032800110505
032900110505             // -?Scrittura record nerl subfile?
033000110505             S01nrr += 1;
033100110505             write  TB87S01;
033200110505
033300110505           endif;
033400110505
033500110505           reade  %kds(k05tntbe01 : 1)  TNTBE000;
033600110505
033700110505         EndDo;
033800110505
033900110505         // -?Visualizzazione del SFL (se ci sono dati)?
034000110505         SflDsp_N  = (S01nrr <= *zero);
034100110505
034200110505         // -?Attivazione del SFLEND?
034300110505         SflEnd = (S01nrr >= c_MaxRec   or
034400110505                   %eof(TNTBE01L));
034500110505
034600110505         // -?Posizionamento cursore al 1° rcd della pagina?
034700110505         if  S01nrr > *zero;
034800110505           wPag     = S01nrr / c_SflPag;
034900110505           wRig     = S01nrr - (c_SflPag * wPag);
035000110505           C1RcdNbr = wPag * c_SflPag;
035100110505           if  wRig > *zero;
035200110505             C1RcdNbr += 1;
035300110505           else;
035400110505             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
035500110505           endif;
035600110505         else;
035700110505           clear  C1RcdNbr;
035800110505         endif;
035900110505
036000110505         C1CsrRrn = C1RcdNbr;
036100110505
036200110505       ENDSR;
036300110505
036400110505       //--------------------------------------------------------------
036500110505       //?Gestione opzioni del subfile S01                             ?
036600110505       //--------------------------------------------------------------
036700110505       BEGSR  sr_OpzS01;
036800110505
036900110505         clear  SavS01csr;
037000110505         clear  wCountOpz;
037100110505         clear  TB87cod;
037200110505         clear  TB87uni;
037300110505
037400110505         // -?Ciclo di lettura subfile?
037500110505         readc  TB87S01;
037600110505
037700110505         DoW  Not %eof(TNTB87D);
037800110505
037900110505           SflNxtChg = *off;
038000110505           %subst(IndDspF : 50) = *off;
038100110505           C1RcdNbr = S01nrr;
038200110505
038300110505           select;
038400110505
038500110505             // -?Nessuna opzione?
038600110505             when  S1Copz = *blank;
038700110505
038800110505             // -?1=Selezione?(continua la lettura del sfl x ctrl)?
038900110505             when  S1Copz = '1';
039000110505               wCountOpz += 1;
039100110505               // -?Impossibile selezionare più di un codice?
039200110505               if  wCountOpz > 1;
039300110505                 ErrMessage  = *on;
039400110505                 ErrGenerico = *on;
039500110505                 PosCurOPZ   = *on;
039600110505                 V1Dmsg = $Msg(02);
039700110505               else;
039800110505                 // -?Reperimento dati del incasso selezionato?
039900110505                 k_TBEke1 = S1Ccod;
040000110505                 clear  k_TBEke2;
040100110505                 chain  %kds(k05tntbe01 : 3)  TNTBE000;
040200110505                 clear  dCPO;
040300110505                 if  %found(TNTBE01L)  and  TBEatb = *blank;
040400110505                   dCPO = TBEuni;
040500110505                 endif;
040600110505                 TB87cod = S1Ccod;
040700110505                 TB87uni = dCPO;
040800110505               endif;
040900110505
041000110505             // -?Opzione errata?
041100110505             other;
041200110505               ErrMessage  = *on;
041300110505               ErrGenerico = *on;
041400110505               PosCurOPZ   = *on;
041500110505               V1Dmsg = $Msg(01);
041600110505
041700110505           endsl;
041800110505
041900110505           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
042000110505           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
042100110505           if  S1Copz  <> *blank   and
042200110505              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
042300110505             SavS01csr   = C1RcdNbr;
042400110505           endif;
042500110505
042600110505           // -?Aggiornamento sfl?
042700110505           select;
042800110505             when  ErrMessage;
042900110505               SflNxtChg = *on;
043000110505               C1CsrRrn  = C1RcdNbr;
043100110505             when  ErrGenerico;
043200110505               C1CsrRrn  = C1RcdNbr;
043300110505               clear  S1Copz;
043400110505             other;
043500110505               if  S1Copz  = '1';
043600110505                 SflNxtChg = *on;
043700110505               else;
043800110505                 clear  S1Copz;
043900110505               endif;
044000110505           endsl;
044100110505           UPDATE  TB87S01;
044200110505           if  ErrGenerico  or  ErrMessage;
044300110505             leave;
044400110505           endif;
044500110505
044600110505           readc  TB87S01;
044700110505
044800110505         ENDDO;
044900110505
045000110505         // -?Riposiziono il cursore sul record contenente la prima opz.?
045100110505         //  ?(se non sono stati rilevati errori)?
045200110505         if  NOT ErrMessage   and   NOT ErrGenerico   and
045300110505             SavS01csr > *zero;
045400110505           C1CsrRrn = SavS01csr;
045500110505         endif;
045600110505
045700110505       ENDSR;
045800110505
045900110505       //--------------------------------------------------------------
046000110505       //?Operazioni finali.                                           ?
046100110505       //--------------------------------------------------------------
046200110505       BEGSR  sr_RoutEnd;
046300110505
046400110505         return;
046500110505
046600110505       ENDSR;
046700110505
046800110505      /end-free
046900110505
047000110505       //--------------------------------------------------------------
047100110505       //?Schiere a tempo di compilazione.                             ?
047200110505       //--------------------------------------------------------------
047300110505
047400110505** -?$Msg?-------------------------------------------------------------------*
047500110505Opzione errata                                                                 1
047600110505Impossibile selezionare più di una categoria potenziale                        2
