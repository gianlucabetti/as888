000100090713      *PARMS dftactgrp(*no) actgrp(*caller) dbgview(*source)
000200090713       //--------------------------------------------------------------
000300090713       //?TRTB31R * Selezione da tabella "1A" = Tipo incasso c/assegno ?
000400090713       //--------------------------------------------------------------
000500090713
000600090713     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090713     h dftactgrp(*no) actgrp(*caller)
000800090713
000900090713       //--------------------------------------------------------------
001000090713       //?Dichiarazione file.                                          ?
001100090713       //--------------------------------------------------------------
001200090713
001300090713       // - Tabelle
001400090713     fTABEL00F  if   e           k disk
001500090713
001600090713       // - Video Gestione
001700090713     fTRTB13D   cf   e             workstn
001800090713     f                                     sfile(TB13S01 : S01nrr)
001900090713     f                                     indds(IndDspF)
002000090713     f                                     infds(InfDspF)
002100090713
002200090713       //--------------------------------------------------------------
002300090713       //?Definizione costanti.                                        ?
002400090713       //--------------------------------------------------------------
002500090713
002600090713       // - Codice tabella in gestione
002700090713     d c_Tab           c                   const('1A')
002800090713
002900090713       // - Codice utente
003000090713     d c_Kut           c                   const(1)
003100090713
003200090713      // - Numero di record in ciascuna videata di subfile
003300090713     d c_SflPag        c                   const(16)
003400090713
003500090713      // - Numero massimo di record gestibili
003600090713     d c_MaxRec        c                   const(999)
003700090713
003800090713       // - Tasti funzionali a video
003900090713     d c_F01           c                   const(x'31')
004000090713     d c_F02           c                   const(x'32')
004100090713     d c_F03           c                   const(x'33')
004200090810     d c_F04           c                   const(x'34')
004300090713     d c_F05           c                   const(x'35')
004400090713     d c_F06           c                   const(x'36')
004500090713     d c_F07           c                   const(x'37')
004600090713     d c_F08           c                   const(x'38')
004700090713     d c_F09           c                   const(x'39')
004800090713     d c_F10           c                   const(x'3A')
004900090810     d c_F11           c                   const(x'3B')
005000090713     d c_F12           c                   const(x'3C')
005100090713     d c_F13           c                   const(x'B1')
005200090713     d c_F14           c                   const(x'B2')
005300090713     d c_F15           c                   const(x'B3')
005400090713     d c_F16           c                   const(x'B4')
005500090713     d c_F17           c                   const(x'B5')
005600090713     d c_F18           c                   const(x'B6')
005700090713     d c_F19           c                   const(x'B7')
005800090713     d c_F20           c                   const(x'B8')
005900090713     d c_F21           c                   const(x'B9')
006000090713     d c_F22           c                   const(x'BA')
006100090713     d c_F23           c                   const(x'BB')
006200090713     d c_F24           c                   const(x'BC')
006300090713     d c_Enter         c                   const(x'F1')
006400090713     d c_RollDown      c                   const(x'F4')
006500090713     d c_RollUp        c                   const(x'F5')
006600090713
006700090713       //--------------------------------------------------------------
006800090713       //?Definizione schiere.                                         ?
006900090713       //--------------------------------------------------------------
007000090713
007100090713       // - Messaggi di errore
007200090713     d $Msg            s             78    dim( 2) ctdata perrcd(1)
007300090713
007400090713       //--------------------------------------------------------------
007500090713       //?Definizione aree dati.                                       ?
007600090713       //--------------------------------------------------------------
007700090713
007800090713       // - Dati utente
007900090713     d §AzUte        e ds                  extname(AZUTE00F)
008000090713     d                                     dtaara
008100090713     d §DatiUte      e ds                  extname(dDatiUte)
008200090713     d                                     dtaara
008300090713
008400090713       //--------------------------------------------------------------
008500090713       //?Definizione strutture dati.                                  ?
008600090713       //--------------------------------------------------------------
008700090713
008800090713       // - Status ds
008900090713     d Status         sds
009000090713     d  SDSpgm           *proc
009100090713
009200090713       // - InfDS
009300090713     d InfDspF         ds
009400090713     d   dsp_aid             369    369a
009500090713     d   sfl_rrn             376    377i 0
009600090713     d   min_nrr             378    379i 0
009700090713     d   num_rcds            380    381i 0
009800090713
009900090713       // - Indicatori su DspF
010000090713     d IndDspF         ds                  inz
010100090713         // - Emissione messaggio di errore
010200090713     d   ErrMessage                    n   overlay(IndDspF : 28)
010300090713         // - Indicatori di gestione del subfile
010400090713     d   SflDsp_N                      n   overlay(IndDspF : 30)
010500090713     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
010600090713     d   SflNxtChg                     n   overlay(IndDspF : 32)
010700090713     d   SflEnd                        n   overlay(IndDspF : 33)
010800090713         // - Posizionamento cursore & segnalazione errore
010900090713     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011000090713         //   - Riemissione videata
011100090713     d   ErrGenerico                   n   overlay(IndDspF : 99)
011200090713
011300090713       // - Parametri ricevuti
011400090713     d KPJBA         e ds
011500090713     d TRTB13ds      e ds                  inz
011600090713
011700090713       // - Parametri per Reperimento dati utente
011800090713     d TIBS34ds      e ds                  inz
011900090713
012000090713       // - Tabelle usate
012100090713     d ds1A          e ds                  inz
012200090713
012300090713       //--------------------------------------------------------------
012400090713       //?Definizione variabili globali.                               ?
012500090713       //--------------------------------------------------------------
012600090713
012700090713       // - Flags booleani
012800090713     d $Fine           s               n   inz
012900090713     d $InzS01         s               n   inz(*on)
013000090713     d $RecOK          s               n   inz
013100090713
013200090713       // - Variabili per la gestione del video
013300090713     d $Video          s              2    inz('S1')
013400090713     d W_SflPag        s              3  0 inz
013500090713     d wPag            s              4  0 inz
013600090713     d wRig            s              3  0 inz
013700090713     d S01nrr          s                   like(C1RcdNbr) inz
013800090713     d SavS01csr       s                   like(C1RcdNbr) inz
013900090713
014000090713       // - Conteggio opzioni inserite
014100090713     d wCountOpz       s              3  0 inz
014200090713
014300090713       //--------------------------------------------------------------
014400090713       //?Definizione prototipi procedure.                             ?
014500090713       //--------------------------------------------------------------
014600090713
014700090713       // - Reperimento dati utente
014800090713      /copy gaitrasrc/srcProtoPR,TIBS34R
014900090713
015000090713       //--------------------------------------------------------------
015100090713       //?Definizione key-list.                                        ?
015200090713       //--------------------------------------------------------------
015300090713
015400090713       // - File TABEL00F
015500090713     d k03tabel00    e ds                  extname(TABEL00F : *key)
015600090713     d                                     prefix(k_)   inz
015700090713
015800090713       //--------------------------------------------------------------
015900090713       //?M A I N - L I N E                                            ?
016000090713       //--------------------------------------------------------------
016100090713
016200090713     c     *Entry        plist
016300090713     c                   parm                    KPJBA
016400090713
016500090713      /free
016600090713
016700090713       //?Operazioni iniziali?
016800090713       exsr sr_RoutInz;
016900090713
017000090713       DoW  $Fine = *off;
017100090713         select;
017200090713           // - Gestione videata S1 (selezione)
017300090713           when $Video = 'S1';
017400090713             exsr sr_GesS01;
017500090713           other;
017600090713             $Fine = *on;
017700090713         endsl;
017800090713       enddo;
017900090713
018000090713       //?Operazioni finali?
018100090713       exsr sr_RoutEnd;
018200090713
018300090713       //--------------------------------------------------------------
018400090713       //?Operazioni iniziali.                                         ?
018500090713       //--------------------------------------------------------------
018600090713       BEGSR  sr_RoutInz;
018700090713
018800090713         *inLR = *on;
018900090713
019000090713         //?Reperimento dati job?
019100090713         exsr  sr_DatiJob;
019200090713
019300090713         //?Impostazione nome programma a video?
019400090713         V1Tpgm = SDSpgm;
019500090713
019600090713         //?Impostazione campi "fissi"?
019700090713         k_TBLkut = c_Kut;
019800090713         k_TBLcod = c_Tab;
019900090713
020000090713         //?Impostazione parametri ricevuti?
020100090713         TRTB13ds = kpjbu;
020200090713         clear  TB13Ocod;
020300090713         clear  TB13Ouni;
020400090713
020500090713       ENDSR;
020600090713
020700090713       //--------------------------------------------------------------
020800090713       //?Reperimento Dati del job (Utente/Operativi).                 ?
020900090713       //--------------------------------------------------------------
021000090713       BEGSR  sr_DatiJob;
021100090713
021200090713         in(e) §AzUte;
021300090713         if NOT %error;
021400090713           in(e) §DatiUte;
021500090713         endif;
021600090713         if %error or RSut = *blank;
021700090713           tibs34r ( tibs34ds );
021800090713           in §AzUte;
021900090713           in §DatiUte;
022000090713         endif;
022100090713
022200090713       ENDSR;
022300090713
022400090713       //--------------------------------------------------------------
022500090713       //?Gestione subfile S01.                                        ?
022600090713       //--------------------------------------------------------------
022700090713       BEGSR  sr_GesS01;
022800090713
022900090713         //?Inizializzazione subfile?
023000090713         if  $InzS01 = *on;
023100090713           exsr  sr_InzS01;
023200090713           $InzS01 = *off;
023300090713         endif;
023400090713
023500090713         //?Emissione Testata e Piede con tasti funzionali abilitati?
023600090713         write  TB13P01;
023700090713         write  TB13T01;
023800090713
023900090713         //?Posizionamento cursore?
024000090713         if  C1CsrRrn > *zero;
024100090713           C1RcdNbr = C1CsrRrn;
024200090713         else;
024300090713           C1RcdNbr = 1;
024400090713           write TB13S00;              // (no rec.)
024500090713         endif;
024600090713
024700090713         //?Emissione videata?
024800090713         exfmt  TB13C01;
024900090713         reset  ErrMessage;
025000090713         reset  ErrGenerico;
025100090713         clear  V1Dmsg;
025200090713
025300090713         SELECT;
025400090713
025500090713         ////?F3=Fine?
025600090713         //WHEN  dsp_aid = c_F03;
025700090713         //  $Fine = *on;
025800090713
025900090713           //?F12=Ritorno?
026000090713           WHEN  dsp_aid = c_F12;
026100090713             $Fine = *on;
026200090713
026300090713           //?Roll-Up?
026400090713           WHEN  dsp_aid = c_RollUp;
026500090713             exsr sr_RollUpS01;
026600090713
026700090713           //?SubFile vuoto?
026800090713           WHEN  S01nrr = *zero;
026900090713
027000090713           //?Invio?
027100090713           OTHER;
027200090713             exsr  sr_OpzS01;
027300090713             select;
027400090713               when  ErrGenerico;
027500090713                 leavesr;
027600090713               when  wCountOpz = 1;
027700090713                 $Fine = *on;
027800090713                 leavesr;
027900090713             endsl;
028000090713
028100090713         ENDSL;
028200090713
028300090713       ENDSR;
028400090713
028500090713       //--------------------------------------------------------------
028600090713       //?Inizializzazione subfile S01                                 ?
028700090713       //--------------------------------------------------------------
028800090713       BEGSR  sr_InzS01;
028900090713
029000090713         //?Spegnimento degli indicatori di errore?
029100090713         %subst(IndDspF : 50) = *off;
029200090713
029300090713         //?Pulizia subfile?
029400090713         SflDsp_N    = *on;
029500090713         SflDspCtl_N = *on;
029600090713         write  TB13C01;
029700090713         SflDspCtl_N = *off;
029800090713         SflEnd      = *off;
029900090713
030000090713         clear  W_SflPag;
030100090713         clear  C1RcdNbr;
030200090713         clear  C1CsrRrn;
030300090713         clear  S01nrr;
030400090713         clear  V1Dmsg;
030500090713         ErrMessage  = *off;
030600090713         ErrGenerico = *off;
030700090713
030800090713         //?Caricamento subfile a pagine?
030900090713         k_TBLkey = *loval;
031000090713         setll  %kds(k03tabel00)  TABEL;
031100090713         reade  %kds(k03tabel00 : 2)  TABEL;
031200090713         exsr  sr_RollUpS01;
031300090713
031400090713         //?Impaginazione subfile?
031500090713         // -> forza l'impaginazione sul 1° rec. del subfile
031600090713         if S01nrr > *zero;
031700090713           C1RcdNbr  = 1;
031800090713           C1CsrRrn  = 1;
031900090713         else;
032000090713           clear C1RcdNbr;
032100090713         endif;
032200090713
032300090713       ENDSR;
032400090713
032500090713       //--------------------------------------------------------------
032600090713       //?Caricamento singola pagina nel subfile S01                   ?
032700090713       //--------------------------------------------------------------
032800090713       BEGSR  sr_RollUpS01;
032900090713
033000090713         S01nrr    = (W_SflPag * c_SflPag);
033100090713         W_SflPag += 1;
033200090713         SflNxtChg = *off;
033300090713
033400090713         //?Ciclo di caricamento dati dal file TABEL00F?
033500090713         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
033600090713              S01nrr    < c_MaxRec                and
033700090713              Not %eof(TABEL00F);
033800090713
033900090713           ds1A   = TBLuni;
034000090713
034100090713           // -?Selezione singolo record del file TABEL00F?
034200090713           exsr  sr_Selez;
034300090713
034400090713           // -?Registrazione singolo record nel subfile?
034500090713           if  $RecOk;
034600090713
034700090713             // -?Caricamento dati nel record del subfile?
034800090713             clear  TB13S01;
034900090713             VS1cod = TBLkey;
035000090713             VS1des = §1Ades;
035100090713             select;
035200090713               when  §1Afg1 = '1';
035300090713                 VS1fg1 = 'P';
035400090713               when  §1Afg1 = '2';
035500090713                 VS1fg1 = 'A';
035600090713               other;
035700090713                 VS1fg1 = §1Afg1;
035800090713             endsl;
035900090713             VS1ttm = §1Attm;
036000090713             VS1mro = §1Amro;
036100090713             VS1deo = §1Adeo;
036200090713
036300090713             // -?Scrittura record nerl subfile?
036400090713             S01nrr += 1;
036500090713             write  TB13S01;
036600090713
036700090713           endif;
036800090713
036900090713           reade  %kds(k03tabel00 : 2)  TABEL;
037000090713
037100090713         EndDo;
037200090713
037300090713         //?Visualizzazione del SFL (se ci sono dati)?
037400090713         SflDsp_N  = (S01nrr <= *zero);
037500090713
037600090713         //?Attivazione del SFLEND?
037700090713         SflEnd = (S01nrr >= c_MaxRec   or
037800090713                   %eof(TABEL00F));
037900090713
038000090713         //?Posizionamento cursore al 1° rcd della pagina?
038100090713         if  S01nrr > *zero;
038200090713           wPag     = S01nrr / c_SflPag;
038300090713           wRig     = S01nrr - (c_SflPag * wPag);
038400090713           C1RcdNbr = wPag * c_SflPag;
038500090713           if  wRig > *zero;
038600090713             C1RcdNbr += 1;
038700090713           else;
038800090713             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
038900090713           endif;
039000090713         else;
039100090713           clear  C1RcdNbr;
039200090713         endif;
039300090713
039400090713         C1CsrRrn = C1RcdNbr;
039500090713
039600090713       ENDSR;
039700090713
039800090713       //--------------------------------------------------------------
039900090713       //?Selezione del singolo record di TNTBE00F                     ?
040000090713       //--------------------------------------------------------------
040100090713       BEGSR  sr_Selez;
040200090713
040300090713         reset  $RecOk;
040400090713
040500090713         select;
040600090713
040700090713           // - Record annullato
040800090713           when  TBLflg  <> *blank;
040900090713             leavesr;
041000090713
041100090713           // - Abilitazione all'uso (partenze/arrivi)
041200090713           when  TB13fg1 <> *blank   and
041300090713                 TB13fg1 <> §1Afg1;
041400090713             leavesr;
041500090713
041600090713           // - Flag c/assegno
041700090713           when  TB13fca <> *blank   and
041800090713                 TB13fca <> §1Afca;
041900090713             leavesr;
042000090713
042100090713           // - Flag intestazione assegno
042200090713           when  TB13fmb <> *blank   and
042300090713                 TB13fmb <> §1Afmb;
042400090713             leavesr;
042500090713
042600090713           // - Specie assegno
042700090713           when  TB13sas <> *blank   and
042800090713                 TB13sas <> §1Asas;
042900090713             leavesr;
043000090713
043100090713           // - Flag utilizzo anche con divisa estera
043200090713           when  TB13ude =  'S'      and
043300090713                 TB13ude <> §1Aude;
043400090713             leavesr;
043500090713           when  TB13ude =  'N'      and
043600090713                 §1Aude  =  'S';
043700090713             leavesr;
043800090713
043900090713           // - Flag utilizzo cambio per convertire c/assegno
044000090713           when  TB13ucm =  'S'      and
044100090713                 TB13ucm <> §1Aucm;
044200090713             leavesr;
044300090713           when  TB13ucm =  'N'      and
044400090713                 §1Aucm  =  'S';
044500090713             leavesr;
044600090713
044700090713           // - Flag c/assegno da sommare al totale da esigere
044800090713           when  TB13ste =  'S'      and
044900090713                 TB13ste <> §1Aste;
045000090713             leavesr;
045100090713           when  TB13ste =  'N'      and
045200090713                 §1Aste  =  'S';
045300090713             leavesr;
045400090713
045500090713           // - Flag abi/cab NON obbligatorio
045600090713           when  TB13abi =  'N'      and
045700090713                 TB13abi <> §1Aabi;
045800090713             leavesr;
045900090713           when  TB13abi =  'S'      and
046000090713                 §1Aabi  =  'N';
046100090713             leavesr;
046200090713
046300090713           // - Flag numero assegno NON obbligatorio
046400090713           when  TB13nas =  'N'      and
046500090713                 TB13nas <> §1Anas;
046600090713             leavesr;
046700090713           when  TB13nas =  'S'      and
046800090713                 §1Anas  =  'N';
046900090713             leavesr;
047000090713
047100090713           // - Flag tipo incasso da controllare in tab. "TM"
047200090713           when  TB13ttm =  'S'      and
047300090713                 TB13ttm <> §1Attm;
047400090713             leavesr;
047500090713           when  TB13ttm =  'N'      and
047600090713                 §1Attm  =  'S';
047700090713             leavesr;
047800090713
047900090713           // - Selezione con/senza descrizione oltre limite
048000090713           when  TB13deo =  'S'      and
048100090713                 §1Adeo  =  *blank;
048200090713             leavesr;
048300090713           when  TB13deo =  'N'      and
048400090713                 §1Adeo  <> *blank;
048500090713             leavesr;
048600090713
048700090713           // - Flag utilizzo anche con Euro
048800090713           when  TB13eur =  'S'      and
048900090713                 TB13eur <> §1Aeur;
049000090713             leavesr;
049100090713           when  TB13eur =  'N'      and
049200090713                 §1Aeur  =  'S';
049300090713             leavesr;
049400090713
049500090713           // - Flag utilizzo anche per POSTE
049600090713           when  TB13fpt =  'S'      and
049700090713                 TB13fpt <> §1Afpt;
049800090713             leavesr;
049900090713           when  TB13fpt =  'N'      and
050000090713                 §1Afpt  =  'S';
050100090713             leavesr;
050200090713
050300090713           // - Flag controllo data limite contabile - tab. "CW"
050400090713           when  TB13cdl =  'S'      and
050500090713                 TB13cdl <> §1Acdl;
050600090713             leavesr;
050700090713           when  TB13cdl =  'N'      and
050800090713                 §1Acdl  =  'S';
050900090713             leavesr;
051000090713
051100090713           // - Flag scaricamento per PCVAS
051200090713           when  TB13vas =  'S'      and
051300090713                 TB13vas <> §1Avas;
051400090713             leavesr;
051500090713           when  TB13vas =  'N'      and
051600090713                 §1Avas  =  'S';
051700090713             leavesr;
051800090713
051900090713           // - Flag richiede mittente originale
052000090713           when  TB13mro =  'S'      and
052100090713                 TB13mro <> §1Amro;
052200090713             leavesr;
052300090713           when  TB13mro =  'N'      and
052400090713                 §1Amro  =  'S';
052500090713             leavesr;
052600090713
052700090713           // - Flag invio a PDA
052800090713           when  TB13pda =  'N'      and
052900090713                 TB13pda <> §1Apda;
053000090713             leavesr;
053100090713           when  TB13pda =  'S'      and
053200090713                 §1Apda  =  'N';
053300090713             leavesr;
053400090713
053500090713           // - Flag secondo limite per contanti - tab. "LCA"
053600090713           when  TB13fs2 =  'S'      and
053700090713                 TB13fs2 <> §1Afs2;
053800090713             leavesr;
053900090713           when  TB13fs2 =  'N'      and
054000090713                 §1Afs2  =  'S';
054100090713             leavesr;
054200090713
054300090713           // - Flag NON ammessi assegni postali
054400090713           when  TB13nap =  'S'      and
054500090713                 TB13nap <> §1Anap;
054600090713             leavesr;
054700090713           when  TB13nap =  'N'      and
054800090713                 §1Anap  =  'S';
054900090713             leavesr;
055000090713
055100090713           // - Flag utilizzabile nel file DESTINATARI PARTICOLARI
055200090713           when  TB13dos =  'S'      and
055300090713                 TB13dos <> §1Ados;
055400090713             leavesr;
055500090713           when  TB13dos =  'N'      and
055600090713                 §1Ados  =  'S';
055700090713             leavesr;
055800090713
055900090713         endsl;
056000090713
056100090713         // - Record selezionato
056200090713         $RecOk = *on;
056300090713
056400090713       ENDSR;
056500090713
056600090713       //--------------------------------------------------------------
056700090713       //?Gestione opzioni del subfile S01                             ?
056800090713       //--------------------------------------------------------------
056900090713       BEGSR  sr_OpzS01;
057000090713
057100090713         clear  SavS01csr;
057200090713         clear  wCountOpz;
057300090713         clear  TB13Ocod;
057400090713         clear  TB13Ouni;
057500090713
057600090713         //?Ciclo di lettura subfile?
057700090713         readc  TB13S01;
057800090713
057900090713         DoW  Not %eof(TRTB13D);
058000090713
058100090713           SflNxtChg = *off;
058200090713           %subst(IndDspF : 50) = *off;
058300090713           C1RcdNbr = S01nrr;
058400090713
058500090713           select;
058600090713
058700090713             // -?Nessuna opzione?
058800090713             when  VS1opz = *blank;
058900090713
059000090713             // -?1=Selezione?(continua la lettura del sfl x ctrl)
059100090713             when  VS1opz = '1';
059200090713               //$Fine = *on;
059300090713               //ErrGenerico = *on;
059400090713               wCountOpz += 1;
059500090713               // - Impossibile selezionare più di un codice
059600090713               if  wCountOpz > 1;
059700090713                 ErrMessage  = *on;
059800090713                 ErrGenerico = *on;
059900090713                 PosCurOPZ   = *on;
060000090713                 V1Dmsg = $Msg(02);
060100090713               else;
060200090713                 // - Reperimento dati del incasso selezionato
060300090713                 k_TBLkey = VS1cod;
060400090713                 chain %kds(k03tabel00)  TABEL;
060500090713                 if  %found(TABEL00F) and TBLflg = *blank;
060600090713                   ds1A = TBLuni;
060700090713                 endif;
060800090713                 TB13Ocod = VS1cod;
060900090713                 TB13Ouni = ds1A;
061000090713               endif;
061100090713
061200090713             // -?Opzione errata?
061300090713             other;
061400090713               ErrMessage  = *on;
061500090713               ErrGenerico = *on;
061600090713               PosCurOPZ   = *on;
061700090713               V1Dmsg = $Msg(01);
061800090713
061900090713           endsl;
062000090713
062100090713           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
062200090713           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
062300090713           if  VS1opz  <> *blank   and
062400090713              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
062500090713             SavS01csr   = C1RcdNbr;
062600090713           endif;
062700090713
062800090713           // -?Aggiornamento sfl?
062900090713           select;
063000090713             when  ErrMessage;
063100090713               SflNxtChg = *on;
063200090713               C1CsrRrn  = C1RcdNbr;
063300090713             when  ErrGenerico;
063400090713               C1CsrRrn  = C1RcdNbr;
063500090713               clear  VS1opz;
063600090713             other;
063700090713               if  VS1opz  = '1';
063800090713                 SflNxtChg = *on;
063900090713               else;
064000090713                 clear  VS1opz;
064100090713               endif;
064200090713           endsl;
064300090713           UPDATE  TB13S01;
064400090713           if  ErrGenerico   or   ErrMessage;
064500090713             leave;
064600090713           endif;
064700090713
064800090713           readc  TB13S01;
064900090713
065000090713         ENDDO;
065100090713
065200090713         // -?Riposiziono il cursore sul record contenente la prima opz.?
065300090713         //  ?(se non sono stati rilevati errori)?
065400090713         if  NOT ErrMessage   and   NOT ErrGenerico   and
065500090713             SavS01csr > *zero;
065600090713           C1CsrRrn = SavS01csr;
065700090713         endif;
065800090713
065900090713       ENDSR;
066000090713
066100090713       //--------------------------------------------------------------
066200090713       //?Operazioni finali.                                           ?
066300090713       //--------------------------------------------------------------
066400090713       BEGSR  sr_RoutEnd;
066500090713
066600090713         // - Restituzione tipo incasso selezionato
066700090713         if  dsp_aid <> c_F12;
066800090713           KPJBU = TRTB13ds;
066900090713         endif;
067000090713
067100090713         return;
067200090713
067300090713       ENDSR;
067400090713
067500090713      /end-free
067600090713
067700090713       //--------------------------------------------------------------
067800090713       //?Schiere a tempo di compilazione.                             ?
067900090713       //--------------------------------------------------------------
068000090713
068100090713** - $Msg -------------------------------------------------------------------*
068200090713Opzione errata                                                                 1
068300090713Impossibile selezionare più di un tipo incasso c/assegno                       2
