000100110525      //--------------------------------------------------------------
000200110525      //?TRMK13R - STAMPA ANGRAFICA POTENZIALI
000300110525      //--------------------------------------------------------------
000400110725
000500110725      //--------------------------------------------------------------
000600110725      //?Specifiche di controllo.                                     ?
000700110725      //--------------------------------------------------------------
000800110525
000900110525     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000110525     h dftactgrp(*no) actgrp(*caller)
001100110525
001200110525      //--------------------------------------------------------------
001300110526      //?Dichiarazione file.
001400110525      //---------------------------------------------------------------
001500110525
001600110525      // - Organigramma filiale/aziendale
001700110525     fAZORG01L  if   e           k disk
001800130916
001900130916      // - Anagrafica Commerciali
002000130916     fAZCMM01L  if   e           k disk
002100110531
002200110531      // - Tabelle
002300110531     fTABEL00F  if   e           k disk
002400110531     fTNTBE01L  if   e           k disk
002500110531
002600110531      // - Attività
002700110531     fTIATC05L  if   e           k disk
002800110525
002900110525      // - Info Commerciali
003000110525     fTICPI01L  if   e           k disk    usropn
003100110531
003200110531      // - Trattative
003300110531     fTIVIS01L  if   e           k disk
003400110525
003500110525      // - Anagrafica Potenziale
003600110525     fTNCPO01L  if   e           k disk    usropn
003700110525     fTNCPO02L  if   e           k disk    usropn
003800110525     f                                     rename(TNCPO000:TNCPO02)
003900110525     fTNCPO03L  if   e           k disk    usropn
004000110525     f                                     rename(TNCPO000:TNCPO03)
004100140925     fTNCPO11L  if   e           k disk    usropn
004200110525
004300110525      // - Work File per la stampa
004400110526     fWFCPO00F  if   e           k disk    usropn
004500110525
004600110525      // - File di stampa
004700110525     fTRMK13P   o    e             printer oflind(*in99)
004800110525     f                                     infds(prtinfo)
004900110525
005000110525      //---------------------------------------------------------------
005100110525      //?Definizione costanti.
005200110525      //---------------------------------------------------------------
005300110525
005400110525
005500110525      //---------------------------------------------------------------
005600110525      //?Definizione schiere.
005700110525      //---------------------------------------------------------------
005800110606
005900110606      // - Agenti (appoggio)
006000110606     d $Age            s              7  0 dim(3500)
006100110606
006200110606      // - Agente Unificante prossima attività (figli)
006300110606     d $Cmm            s              7  0 dim(3500)
006400110906
006500110906      // - Agente Unificante (figli)
006600110906     d $Cmmu           s              7  0 dim(3500)
006700110525
006800110531      // - Tabella IFO
006900110531     d IFO             s              3    dim(50)
007000110531     d IFOds           s             70    dim(50)
007100110531     d IFOcg           s              2    dim(50)
007200110531     d IFOor           s              8    dim(50)
007300110525
007400110525      //---------------------------------------------------------------
007500110525      //?Definizione aree dati.
007600110525      //---------------------------------------------------------------
007700110525
007800110525      // - Dati utente
007900110525     d §AzUte        e ds                  extname(AZUTE00F)
008000110525     d                                     dtaara
008100110525     d §DatiUte      e ds                  extname(dDatiUte)
008200110525     d                                     dtaara
008300110525
008400110525      //---------------------------------------------------------------
008500110525      //?Definizione strutture dati.
008600110525      //---------------------------------------------------------------
008700110525
008800110525     d PrtINFO         ds
008900110525     d  CurLine              367    368i 0
009000110525
009100110525      // - Status
009200110525     d Psds           sds
009300110525     d   SDSpgm          *proc
009400110525
009500110525      // - Parametri ricevuti
009600110525     d KPJBA         e ds
009700110525     d TRMK13ds      e ds                  inz
009800110531     d  $cic                  68     72    dim(5)
009900110608     d  D13cic1               68     68
010000110608     d  D13cic2               69     69
010100110608     d  D13cic3               70     70
010200110608     d  D13cic4               71     71
010300110608     d  D13cic5               72     72
010400110531     d  $fls                  73     77    dim(5)
010500110608     d  D13fls1               73     73
010600110608     d  D13fls2               74     74
010700110608     d  D13fls3               75     75
010800110608     d  D13fls4               76     76
010900110608     d  D13fls5               77     77
011000110531     d  $cco                  88     97    dim(5)
011100110608     d  D13cco1               88     89
011200110608     d  D13cco2               90     91
011300110608     d  D13cco3               92     93
011400110608     d  D13cco4               94     95
011500110608     d  D13cco5               96     97
011600110531     d  $rip                 127    135    dim(3)
011700110608     d  D13rip1              127    129
011800110608     d  D13rip2              130    132
011900110608     d  D13rip3              133    135
012000140925     d  $conc                163    182    dim(5)
012100140925     d  D13conc1             163    166
012200140925     d  D13conc2             167    170
012300140925     d  D13conc3             171    174
012400140925     d  D13conc4             175    178
012500140925     d  D13conc5             179    182
012600140925     d  $moi                 189    196    dim(2)
012700140925     d  D13moi1              189    192
012800140925     d  D13moi2              193    196
012900140925     d  $paesi               344    363    dim(5)
013000140925     d  D13paesi1            344    347
013100140925     d  D13paesi2            348    351
013200140925     d  D13paesi3            352    355
013300140925     d  D13paesi4            356    359
013400140925     d  D13paesi5            360    363
013500110525
013600110525      // - Reperimento dati utente
013700110531     d TIBS34DS      e ds
013800110525     d dUTE01        e ds
013900110531
014000110531      // - Tabelle TNTBE
014100110531     d TIBS02DS      e ds                  inz
014200110526
014300110526      // - controllo abilitazioni
014400110526     d TNTAA1DS      e ds                  inz
014500140925
014600140925      // - Parametri per gestione parametri batch
014700140925     d TRUL01ds      e ds                  inz
014800140925     d   UL01err     e                     inz(*off)
014900110526
015000110526      // - Carico Filiali abilitate all'utente
015100110525     d TRUL31DS      e ds
015200110525     d POG                    10    759    DIM(250)
015300110531
015400110531      // - Ds x campo CPORST
015500110531     d dCPO01        e ds                  inz
015600140925
015700140925      // - Dati in CPOTEL
015800140925     d dCPO02        e ds                  inz
015900110601
016000110601      // - Tabella CCO = Causali Attività
016100110601     d dCCO          e ds                  inz
016200110531
016300110531      // - Tabella IFO = Info Commerciali
016400110531     d dIFO          e ds                  inz
016500110606
016600110606      // - Tabella IFS = Sottoconto Info Commerciali
016700110606     d dIFS          e ds                  inz
016800110531
016900110531      // - Tabella 05 = Area
017000110531     d ds05          e ds                  inz
017100110531
017200110531      // - Tabella 1L = Categoria Merceologica
017300110531     d ds1L          e ds                  inz
017400110607
017500110607      // - File WFCPO
017600110607     d WFCPOds       e ds                  extname(WFCPO00F)
017700110725     d                                     inz
017800110525
017900110525      //---------------------------------------------------------------
018000110525      //?Definizione variabili globali.
018100110525      //---------------------------------------------------------------
018200110531
018300110531      // - Indici di schiera
018400110531     d xx              s              4  0 inz
018500110607
018600110607       // - Stringa SQL da eseguire
018700110607     d StringaSql      s           2048    Varying        inz
018800110525
018900110526      // - Flags booleani
019000110607     d wInfo           s               n   inz(*off)
019100110607     d $End            s               n   inz(*off)
019200110606     d $EndAge         s               n   inz(*off)
019300110531     d $EndAtc         s               n   inz(*off)
019400110606     d $EndAtt         s               n   inz(*off)
019500110531     d $EndVis         s               n   inz(*off)
019600110607     d $InfoAbi        s               n   inz(*off)
019700110607     d $InfoNoOk       s               n   inz(*off)
019800110526     d $OrdCar         s               n   inz(*off)
019900110526     d $OrdCmm         s               n   inz(*off)
020000110526     d $OrdCmmu        s               n   inz(*off)
020100110526     d $OrdRag         s               n   inz(*off)
020200110526     d $OrdSct         s               n   inz(*off)
020300110526     d $OrdSpt         s               n   inz(*off)
020400110531     d $RcdOk          s               n   inz(*off)
020500110526
020600110526      // - Campi di comodo
020700110531     d Nrpag           s              4  0
020800110606     d sav_Car         s                   like(WFCare)
020900110606     d sav_Cmm         s                   like(WFCcmm)
021000110606     d sav_Cmmu        s                   like(WFCcmmu)
021100110606     d sav_CPItpf      s                   like(CPItpf)
021200110607     d sav_Flt         s                   like(WFCflt)
021300110606     d sav_Sct         s                   like(CPOsct)
021400110606     d Stringa_Info    s            164    inz
021500110906     d wAge            s              7  0 inz
021600110606     d wAtt            s              4  0 inz
021700110606     d wPog            s                   like(O31pog)
021800110606     d wPos            s              3  0 inz
021900110606     d wTtr            s              4  0 inz
022000110606     d w0030           s              3  0 inz
022100110531
022200110531      // - Campi di comodo data
022300110606     d wDate           s              8  0
022400110531     d wDate_EUR       s               d   datfmt(*eur)
022500110531     d wDate_ISO       s               d   datfmt(*iso)
022600110525
022700110525      //---------------------------------------------------------------
022800110525      //?Definizione procedure usate.
022900110525      //---------------------------------------------------------------
023000110525
023100110526      // - Elaborazione file di work
023200110526     d trmk13r1        pr                  extpgm('TRMK13R1')
023300110606     d  kpjba                              likeds(kpjba)
023400110606     d  wPog                        750
023500110607     d  $InfoAbi                       n
023600140925     d  TRMK13DS                           likeds(TRMK13DS)
023700140925
023800140925      // - Gestione parametri di lancio
023900140925     d TRUL01R         pr                  extpgm('TRUL01R')
024000140925     d  trul01ds                           likeds(TRUL01DS)
024100110525
024200110525      //---------------------------------------------------------------
024300110525      //?Prototipi.
024400110525      //---------------------------------------------------------------
024500110531      /copy gaitrasrc/srcprotopr,tibs02r
024600110525      /copy gaitrasrc/srcprotopr,tibs34r
024700110526      /copy gaitrasrc/srcprotopr,tntaa1c
024800110606      /copy gaitrasrc/srcprotopr,trul31r
024900110525
025000110525      //---------------------------------------------------------------
025100110525      //?Definizione key-list.
025200110525      //---------------------------------------------------------------
025300110525
025400110531       // - File TABEL00F
025500110531     d k03tabel      e ds                  extname(TABEL00F:*key)
025600110531     d                                     prefix(k_)
025700110531     d                                     inz
025800110525
025900110601      //---------------------------------------------------------------
026000110601      //?Riepilogo indicatori.
026100110601      //---------------------------------------------------------------
026200110601
026300110601      // 10    : Stampa info commerciali
026400110620      // 11    : Rottura per Commerciale Unificante
026500110620      // 12    : Rottura per Commerciale Unificante prossima attività
026600110620      // 13    : Rottura per Area/Filiale
026700110620      // 14    : Rottura per Categoria Merceologica
026800110608      // 99    : Salto pagina per overflow
026900110601
027000110601      //---------------------------------------------------------------
027100110525      //---------------------------------------------------------------
027200110525      //?M A I N - L I N E
027300110525      //---------------------------------------------------------------
027400110525
027500110525     c     *Entry        plist
027600110525     c                   parm                    KPJBA
027700110525
027800110525      /free
027900110525
028000110525       //?Operazioni iniziali
028100110525       exsr RoutInz;
028200110531
028300110531       //?Stampo la pagina dei parametri di lancio
028400110531       exsr StpPar;
028500110525
028600110531       //?Elaboro
028700110531       exsr Elabora;
028800110607
028900110607       //?Stampo il totale dei clienti
029000110607       IF  *in10;
029100110607         write MK13TOT;
029200110607       ENDIF;
029300110531
029400110525       //?Stampo la fine
029500110526       write MK13FINE;
029600110525
029700110525       //?Operazioni finali
029800110525       exsr RoutEnd;
029900110525
030000110525       //--------------------------------------------------------------
030100110525       //?Operazioni iniziali.
030200110525       //--------------------------------------------------------------
030300110525       BEGSR RoutInz;
030400110526
030500110531       //?Imposto la ds di passaggio parametri
030600140925         //TRMK13DS = kpjbu;
030700140925
030800140925       //?da WFPRM00F
030900140925         clear TRMK13DS;
031000140925         reset TRUL01DS;
031100140925         UL01prg = %dec(%subst(kpjbu:1:7):7:0);
031200140925         trul01r (trul01ds);
031300140925         IF  UL01err = *off;
031400140925           TRMK13DS = UL01prm;
031500140925         ENDIF;
031600110526
031700110526         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
031800110525
031900110531       //?Reperimento dati job
032000110525         exsr DatiJob;
032100110607
032200110607       //?Imposto gli ordinamenti
032300110607         $OrdSpt  = (D13ospt  <> *blanks);
032400110607         $OrdCmmu = (D13ocmmu <> *blanks);
032500110607         $OrdCmm  = (D13ocmm  <> *blanks);
032600110607         $OrdCar  = (D13ocar  <> *blanks);
032700110607         $OrdRag  = (D13orag  <> *blanks);
032800110607         $OrdSct  = (D13osct  <> *blanks);
032900110608
033000110608       //?Imposto i salti pagina
033100110620         *in11 = (D13ocmmu <> *blanks);
033200110620         *in12 = (D13ocmm  <> *blanks);
033300110620         *in13 = (D13ocar  <> *blanks);
033400110620         *in14 = (D13osct  <> *blanks);
033500110525
033600110607       //?Creo file di WORK x ordinamenti richiesti
033700110607         IF  not $OrdRag and not $OrdSct;
033800110606         //?Pgm che scrive il file di work
033900140925           //kpjbu = TRMK13DS;
034000140925           trmk13r1 (kpjba:O31pog:$InfoAbi:trmk13ds);
034100110531         //?Apro il file di work
034200110607           open WFCPO00F;
034300110531         //?Apro il file dei potenziali
034400110526           open TNCPO01L;
034500110531         ENDIF;
034600110526
034700110607       //?Apro il file TNCPO che devo leggere
034800110607         IF  $OrdRag;
034900110606           open TNCPO02L;
035000110526         ENDIF;
035100110607         IF  $OrdSct;
035200110606           open TNCPO03L;
035300110526         ENDIF;
035400140925
035500140925       //?Estensione Anagrafica Potenziali apro il file
035600140925         open TNCPO11L;
035700110607
035800110607       //?INFO COMMERCIALI apro il file
035900110607         open TICPI01L;
036000110531
036100110531       //?Carico le INFO
036200110607         IF  $InfoAbi;
036300110607           clear xx;
036400110607           setll ('IFO') TNTBE01L;
036500110607           reade ('IFO') TNTBE01L;
036600110607           DOW  not %eof(TNTBE01L);
036700110607             IF  TBEatb = *blanks;
036800110607               dIFO = TBEuni;
036900110607               xx += 1;
037000110607               IFO(xx) = TBEke1;
037100110607               IFOds(xx) = dIFO;
037200110607               IFOcg(xx) = §IFOifg;
037300110607             //?Ordinamento per categoria e num. ordinamento
037400110607               IFOor(xx) = §IFOifg + %editc(§IFOogi:'X') +
037500110607                           TBEke1;
037600110607             ENDIF;
037700110607             reade ('IFO') TNTBE01L;
037800110607           ENDDO;
037900110607         ENDIF;
038000110906
038100110906       //?Se richiesto Commerciale Unificante
038200110906       //?carico la sk con i figli
038300110906         IF  D13cmmu <> 0;
038400110906           wAge = D13cmmu;
038500110906           exsr CarAge;
038600110906           $Cmmu = $Age;
038700110906         ENDIF;
038800110606
038900110606       //?Se richiesto Commerciale Unificante prossima attività
039000110606       //?carico la sk con i figli
039100110606         IF  D13cmm <> 0;
039200110906           wAge = D13cmm;
039300110606           exsr CarAge;
039400110606           $Cmm = $Age;
039500110606         ENDIF;
039600110525
039700110531       //?Imposto nome programma
039800110525         NomePgm = SDSpgm;
039900110525
040000110531       //?Imposto data e ora elaborazione
040100110525         DataEla = %dec(%date():*eur);
040200110525         OraEla  = %dec(%time());
040300110606
040400110606       ENDSR;
040500110601
040600110601       //--------------------------------------------------------------
040700110601       //?Reperimento Dati del job (Utente/Operativi).
040800110601       //--------------------------------------------------------------
040900110601       BEGSR DatiJob;
041000110601
041100110601         in(E) §AzUte;
041200110601         IF  NOT %error;
041300110601           in(E) §DatiUte;
041400110601         ENDIF;
041500110601         IF  %error or RSut = *blanks;
041600110601           clear TIBS34ds;
041700110601           tibs34r(tibs34ds);
041800110601           in §AzUte;
041900110601           in §DatiUte;
042000110601         ENDIF;
042100110601
042200110601       //?Controllo se utente abilitato alla stampa INFO COMMERCIALI
042300110601         dUTE01 = UTEfaf;
042400110607         $InfoAbi = (§UTEpotst = 'S');
042500110607         *in10 = $InfoAbi;
042600110601
042700110601       //?Richiamo pgm di controllo autorizzazioni utente così mi torna
042800110601       //?il codice autorizzazione relativo alla gestione potenziali
042900110601       //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
043000110601         reset TNTAA1DS;
043100110601         ITAA1caut = '§utepot';
043200110601         kpjbu     = TNTAA1DS;
043300110601         tntaa1c (kpjba);
043400110601         TNTAA1DS = kpjbu;
043500110601
043600110601       //?Carico filiali abilitate all'utente
043700110601         clear TRUL31DS;
043800110601         I31abi = OTAA1cabi;
043900110601         I31cdi = DUTdis;
044000110601         I31car = DUTare;
044100110601         I31cpo = DUTpou;
044200110606         trul31r (kpjba:trul31ds);
044300110606         wPog = O31pog;
044400110601
044500110601       ENDSR;
044600110606
044700110606       //--------------------------------------------------------------
044800110606       //?Carico Sk Agenti.
044900110606       //--------------------------------------------------------------
045000110606       BEGSR CarAGE;
045100110606
045200110606         clear xx;
045300110606         clear $Age;
045400110606         $EndAge = *off;
045500110606
045600110606         exec sql
045700110606          declare AGE cursor for
045800130916          select CMMcod, CMMuni
045900130916          from AZCMM00F
046000130916          order by CMMcod;
046100110606
046200110606         exec sql
046300110606           open AGE;
046400110606           IF sqlcode < 0;
046500110606             $EndAge = *on;
046600110606           ENDIF;
046700110606
046800110606         DOW not $EndAge;
046900110606           exec sql
047000130916             fetch next from AGE into :CMMcod, :CMMuni;
047100110606             IF sqlcod = 100 or sqlcod < 0;
047200110606               $EndAge = *on;
047300110606               leave;
047400110606             ENDIF;
047500130916             IF  CMMuni = wAge;
047600110606               xx += 1;
047700130916               $Age(xx) = CMMcod;
047800110606             ENDIF;
047900110606         ENDDO;
048000110606
048100110606         exec sql close AGE;
048200110606
048300110606       //?Se non ho cricato niente memorizzo almeno il codice richiesto
048400110606         IF $Age(01) = 0;
048500110906           $Age(01) = wAge;
048600110606         ENDIF;
048700110606
048800110606       ENDSR;
048900110531
049000110531       //--------------------------------------------------------------
049100110531       //?Stampa pagina con i parametri di lancio.
049200110531       //--------------------------------------------------------------
049300110531       BEGSR StpPar;
049400110531
049500110531         clear NrPag;
049600110531
049700110531       //?Decodifico il commerciale unificante
049800130916         clear  DesCmmU;
049900110531         IF  D13cmmu > 0;
050000130916           chain  (D13cmmU)  AZCMM000;
050100130916           IF  %found(AZCMM01L);
050200130916             descmmu = CMMdes;
050300110531           ENDIF;
050400110531         ENDIF;
050500110531
050600110531       //?Decodifico l'area
050700110531         IF  D13car > 0;
050800110531           clear ds05;
050900110531           k_TBLkut = 1;
051000110531           k_TBLcod = '05';
051100110531           k_TBLkey = %editc(D13car:'X');
051200110531           chain %kds(K03tabel) TABEL00F;
051300110531           IF  %found(TABEL00F) and TBLflg = *blanks;
051400110531             ds05 = TBLuni;
051500110531           ENDIF;
051600110531           descar = §05des;
051700110531         ENDIF;
051800110531
051900110531       //?Decodifico la filiale
052000110531         IF  D13fil > 0;
052100110531           clear desfil;
052200110531           chain (D13fil) AZORG01L;
052300110531           IF  %found(AZORG01L) and ORGfva = *blanks;
052400110531             desfil = ORGdes;
052500110531           ENDIF;
052600110531         ENDIF;
052700110531
052800110531       //?Categoria merceologica
052900110531         IF  D13sct > 0;
053000110531           clear ds1L;
053100110531           k_TBLkut = 1;
053200110531           k_TBLcod = '1L';
053300110531           k_TBLkey = %editc(D13sct:'X');
053400110531           chain %kds(K03tabel) TABEL00F;
053500110531           IF  %found(TABEL00F) and TBLflg = *blanks;
053600110531             ds1L = TBLuni;
053700110531           ENDIF;
053800110531           dessct = §1Ldes;
053900110531         ENDIF;
054000110531
054100110531       //?Contattati
054200110531         SELECT;
054300110531           WHEN  D13pst = *blanks;
054400110531             despst = 'Tutti';
054500110531           WHEN  D13pst = 'N';
054600110531             despst = 'Mai Contattati';
054700110531           WHEN  D13pst = 'S';
054800110531             despst = 'Già Contattati';
054900110531         ENDSL;
055000110531
055100110531       //?Sede/Filiale
055200110531         SELECT;
055300110531           WHEN  D13fsf = 'S';
055400110531             desfsf = 'Sede';
055500110531           WHEN  D13fsf = 'F';
055600110531             desfsf = 'Filiale';
055700110531         ENDSL;
055800110531
055900110531       //?Con Trattative in corso
056000110531         SELECT;
056100110531           WHEN  D13ttr = *blanks;
056200110531             desttr = 'Tutti';
056300110531           WHEN  D13ttr = 'S';
056400110531             despst = 'Sì';
056500110531           WHEN  D13ttr = 'N';
056600110531             desttr = 'No';
056700110531         ENDSL;
056800110531
056900110531       //?Decodifico il commerciale unificante prossima attività
057000130916         clear  DesCmm;
057100110531         IF  D13cmm > 0;
057200130916           chain  (D13cmm)  AZCMM000;
057300130916           IF  %found(AZCMM01L);
057400130916             descmm = CMMdes;
057500110531           ENDIF;
057600110531         ENDIF;
057700110531
057800110531       //?Date 'Attività da effettuare'
057900110531         IF  D13dadda > 0;
058000110531           wDate_ISO = %date(D13dadda:*iso);
058100110531           wDate_EUR = wDate_ISO;
058200110531           stpdadda  = %dec(wDate_EUR);
058300110531           wDate_ISO = %date(D13dada:*iso);
058400110531           wDate_EUR = wDate_ISO;
058500110531           stpdada   = %dec(wDate_EUR);
058600110531         ENDIF;
058700140925
058800140925       //?Inclusa spesa presunta
058900140925         SELECT;
059000140925           WHEN  D13sptp = ' ';
059100140925             dessptp = 'NO';
059200140925           WHEN  D13sptp = 'S';
059300140925             dessptp = 'SI';
059400140925         ENDSL;
059500110531
059600110531       //?Ripartizione spesa generale
059700110531         SELECT;
059800110531           WHEN  D13ripg = 'I';
059900110531             desripg = 'ITALIA';
060000110531           WHEN  D13ripg = 'E';
060100110531             desripg = 'ESTERO';
060200110531         ENDSL;
060300140925
060400140925       //?Paesi estero
060500140925         IF  d13paesi1 <> *blanks;
060600140925           clear sppaesi1;
060700140925           clear TIBS02ds;
060800140925           T02mod = 'C';
060900140925           T02cod = 'IFS';
061000140925           T02ke1 = '50';
061100140925           T02ke2 = d13paesi1;
061200140925           T02sif = KNSIF;
061300140925           TNTBE_RicercaControllo (kpjba : tibs02ds);
061400140925           IF  T02err = *blanks;
061500140925             sppaesi1 = T02uni;
061600140925           ENDIF;
061700140925         ENDIF;
061800140925         IF  d13paesi2 <> *blanks;
061900140925           clear sppaesi2;
062000140925           clear TIBS02ds;
062100140925           T02mod = 'C';
062200140925           T02cod = 'IFS';
062300140925           T02ke1 = '50';
062400140925           T02ke2 = d13paesi2;
062500140925           T02sif = KNSIF;
062600140925           TNTBE_RicercaControllo (kpjba : tibs02ds);
062700140925           IF  T02err = *blanks;
062800140925             sppaesi2 = T02uni;
062900140925           ENDIF;
063000140925         ENDIF;
063100140925         IF  d13paesi3 <> *blanks;
063200140925           clear sppaesi3;
063300140925           clear TIBS02ds;
063400140925           T02mod = 'C';
063500140925           T02cod = 'IFS';
063600140925           T02ke1 = '50';
063700140925           T02ke2 = d13paesi3;
063800140925           T02sif = KNSIF;
063900140925           TNTBE_RicercaControllo (kpjba : tibs02ds);
064000140925           IF  T02err = *blanks;
064100140925             sppaesi3 = T02uni;
064200140925           ENDIF;
064300140925         ENDIF;
064400140925         IF  d13paesi4 <> *blanks;
064500140925           clear sppaesi4;
064600140925           clear TIBS02ds;
064700140925           T02mod = 'C';
064800140925           T02cod = 'IFS';
064900140925           T02ke1 = '50';
065000140925           T02ke2 = d13paesi4;
065100140925           T02sif = KNSIF;
065200140925           TNTBE_RicercaControllo (kpjba : tibs02ds);
065300140925           IF  T02err = *blanks;
065400140925             sppaesi4 = T02uni;
065500140925           ENDIF;
065600140925         ENDIF;
065700140925         IF  d13paesi5 <> *blanks;
065800140925           clear sppaesi5;
065900140925           clear TIBS02ds;
066000140925           T02mod = 'C';
066100140925           T02cod = 'IFS';
066200140925           T02ke1 = '50';
066300140925           T02ke2 = d13paesi5;
066400140925           T02sif = KNSIF;
066500140925           TNTBE_RicercaControllo (kpjba : tibs02ds);
066600140925           IF  T02err = *blanks;
066700140925             sppaesi5 = T02uni;
066800140925           ENDIF;
066900140925         ENDIF;
067000110531
067100110531       //?Motivo di non affidamento
067200110531         IF  $moi(01) <> *blanks;
067300110531           clear desmoi1;
067400110531           clear TIBS02ds;
067500110531           T02mod = 'C';
067600110531           T02cod = 'IFS';
067700110531           T02ke1 = 'MOI';
067800110531           T02ke2 = $moi(01);
067900110531           T02sif = KNSIF;
068000110531           TNTBE_RicercaControllo (kpjba : tibs02ds);
068100110531           IF  T02err = *blanks;
068200110531             desmoi1 = T02uni;
068300110531           ENDIF;
068400110531         ENDIF;
068500110531         IF  $moi(02) <> *blanks;
068600110531           clear desmoi2;
068700110531           clear TIBS02ds;
068800110531           T02mod = 'C';
068900110531           T02cod = 'IFS';
069000110531           T02ke1 = 'MOI';
069100110531           T02ke2 = $moi(02);
069200110531           T02sif = KNSIF;
069300110531           TNTBE_RicercaControllo (kpjba : tibs02ds);
069400110531           IF  T02err = *blanks;
069500110531             desmoi2 = T02uni;
069600110531           ENDIF;
069700110531         ENDIF;
069800110531
069900110531       //?Stampo la pagina dei parametri di lancio
070000110531         write MK13PAGINA;
070100110531         write MK13PAR;
070200110531
070300110531       ENDSR;
070400110531
070500110531       //--------------------------------------------------------------
070600110531       //?Elaborazione.
070700110531       //--------------------------------------------------------------
070800110531       BEGSR Elabora;
070900110531
071000110531         SELECT;
071100110531
071200110531       //?Stampo per ragione sociale
071300110531           WHEN  $OrdRag;
071400110531             exsr ReadCpo2;
071500110531
071600110531       //?Stampo per categoria merceologica
071700110531           WHEN  $OrdSct;
071800110531             exsr ReadCpo3;
071900110607
072000110607       //?negli altri casi leggo il file di WORK
072100110607           OTHER;
072200110607             exsr ReadWcpo;
072300110531
072400110531         ENDSL;
072500110531
072600110531       ENDSR;
072700110531
072800110531       //--------------------------------------------------------------
072900110531       //?Stampo per ragione sociale.
073000110531       //--------------------------------------------------------------
073100110531       BEGSR ReadCpo2;
073200110531
073300110531         read TNCPO02L;
073400110531         DOW  not %eof(TNCPO02L);
073500110531
073600110531         //?Controllo se il rcd letto è OK
073700110531           exsr CtrRcd;
073800110531           IF  $RcdOk;
073900110531             exsr Stampa;
074000110531           ENDIF;
074100110531
074200110531           read TNCPO02L;
074300110531
074400110531         ENDDO;
074500110531
074600110531       ENDSR;
074700110531
074800110531       //--------------------------------------------------------------
074900110531       //?Stampo per categoria merceologica.
075000110531       //--------------------------------------------------------------
075100110531       BEGSR ReadCpo3;
075200110531
075300110531       //?Se richiesta una categoria leggo solo quella
075400110531         IF  D13sct > 0;
075500110531           setll (D13sct) TNCPO03L;
075600110531           reade (D13sct) TNCPO03L;
075700110531       //?Se non richiesta una categoria leggo tutto
075800110531         ELSE;
075900110531           read TNCPO03L;
076000110531         ENDIF;
076100110531         DOW  not %eof(TNCPO03L);
076200110531
076300110531         //?Controllo se il rcd letto è OK
076400110531           exsr CtrRcd;
076500110531           IF  $RcdOk;
076600110531             exsr Stampa;
076700110531           ENDIF;
076800110531
076900110531           IF  D13sct > 0;
077000110531             reade (D13sct) TNCPO03L;
077100110531           ELSE;
077200110531             read TNCPO03L;
077300110531           ENDIF;
077400110531
077500110531         ENDDO;
077600110531
077700110531       ENDSR;
077800110607
077900110607       //--------------------------------------------------------------
078000110607       //?Leggo il file di WORK.
078100110607       //--------------------------------------------------------------
078200110607       BEGSR ReadWcpo;
078300110607
078400110607         $End = *off;
078500110607         StringaSql = 'select * from WFCPO00F ';
078600110607
078700110607       //?Ordino il file in base alle richieste
078800110607         SELECT;
078900110607           WHEN  $OrdSpt;
079000110607             StringaSql += ' order by WFCspt desc, WFCrag';
079100110607           WHEN  $OrdCmmu;
079200110607             StringaSql += ' order by WFCcmmu, WFCrag';
079300110607           WHEN  $OrdCmm;
079400110607             StringaSql += ' order by WFCcmm, WFCrag';
079500110607           WHEN  $OrdCar;
079600110607             StringaSql += ' order by WFCare, WFCflt, WFCrag';
079700110607         ENDSL;
079800110607
079900110607       //?Dichiarazione cursore
080000110607         exec sql
080100110607           prepare A1 from :StringaSql;
080200110607         exec sql
080300110607           declare WCPO cursor for A1;
080400110607
080500110607       //?Apertura del cursore
080600110607         exec sql
080700110607           open WCPO;
080800110607
080900110607         IF  sqlcode < 0;
081000110607           $End = *on;
081100110607         ENDIF;
081200110607
081300110607       //?Leggo file di work
081400110607         DOW  not $End;
081500110607           exec sql
081600110607             fetch next from WCPO into :WFCPOds;
081700110607           IF  sqlcod = 100 or sqlcod < 0;
081800110607             $End = *on;
081900110607             leave;
082000110607           ENDIF;
082100110607
082200110607         //?Aggancio TNCPO
082300110607           chain (WFCcpo) TNCPO01L;
082400110607           IF  %found(TNCPO01L);
082500110607           //?Controllo se il rcd letto è OK
082600110607             exsr CtrRcd;
082700110607             IF  $RcdOk;
082800110607               exsr Stampa;
082900110607             ENDIF;
083000110607           ENDIF;
083100110607
083200110607         ENDDO;
083300110607
083400110607         exec sql
083500110607           close WCPO;
083600110607
083700110607       ENDSR;
083800110531
083900110531       //--------------------------------------------------------------
084000110531       //?Controllo se RECORD OK.
084100110531       //--------------------------------------------------------------
084200110531       BEGSR CtrRcd;
084300110531
084400110531         $EndAtc = *off;
084500110531         $EndVis = *off;
084600110531         $RcdOk = *off;
084700110531         dCPO01 = CPOrst;
084800140925         dCPO02 = CPOtel;
084900140925
085000140925       //?Aggancio il file estensione
085100140925         chain CPOcpo TNCPO11L;
085200110906
085300120131       // se presente il commerciale unificante ed il commerciale del potenziale a zero esco
085400120131         IF  D13cmmu > 0 and CPOcmm = 0 ;
085500120131           leavesr ;
085600120131         Endif ;
085700120131
085800110906       //?Controllo se potenziale in carico al commerciale unificante
085900110906         IF  D13cmmu > 0 and %lookup(CPOcmm:$Cmmu) = 0;
086000110906           leavesr;
086100110906         ENDIF;
086200110531
086300110531       //?Filiale potenziale OK su Organigramma
086400110531         chain CPOflt AZORG01L;
086500110531         IF  not %found(AZORG01L);
086600110531           leavesr;
086700110531         ENDIF;
086800110531         IF  ORGfva <> *blanks;
086900110531           leavesr;
087000110531         ENDIF;
087100110531
087200110531       //?Controllo se stessa provincia
087300110531         IF  D13prv <> *blanks and CPOprv <> D13prv;
087400110531           leavesr;
087500110531         ENDIF;
087600110531
087700110531       //?Controllo se stessa località
087800110531         IF  D13loc <> *blanks;
087900110531           wPos = %scan('  ':D13loc);
088000110531           wPos -= 1;
088100120606           IF  %subst(D13loc:1:wPos) <> %subst(CPOcit:1:wPos);
088200110531             leavesr;
088300110531           ENDIF;
088400110531         ENDIF;
088500110531
088600110531       //?Controllo se stesso cap
088700110531         IF  D13cap <> *blanks and (CPOcap <> D13cap or
088800110531                                    CPOnaz <> D13naz);
088900110531           leavesr;
089000110531         ENDIF;
089100110531
089200110531       //?Controllo se stessa nazione
089300110531         IF  D13naz <> *blanks and CPOnaz <> D13naz;
089400110531           leavesr;
089500110531         ENDIF;
089600110531       //?Controllo se stessa area
089700110531         IF  D13car > *zeros and ORGcar <> D13car;
089800110531           leavesr;
089900110531         ENDIF;
090000110531
090100110531       //?Controllo se stessa filiale
090200110531         IF  D13fil <> 0 and CPOflt <> D13fil;
090300110531           leavesr;
090400110531         ENDIF;
090500110531
090600110531       //?Controllo se stessa categoria merceologica
090700110531         IF  D13sct <> 0 and CPOsct <> D13sct;
090800110531           leavesr;
090900110531         ENDIF;
091000110531
091100110531       //?Controllo se stesso codice importanza
091200110531         IF  D13cic <> *blanks;
091300110531           IF  CPOftr = *blanks;
091400110531             leavesr;
091500110531           ENDIF;
091600110531           IF  %lookup(CPOftr:$cic) = 0;
091700110531             leavesr;
091800110531           ENDIF;
091900110531         ENDIF;
092000110531
092100110531       //?Controllo se stessa categoria potenziale
092200110531         IF  D13fls <> *blanks;
092300110531           IF  CPOfls = *blanks;
092400110531             leavesr;
092500110531           ENDIF;
092600110531           IF  %lookup(CPOfls:$fls) = 0;
092700110531             leavesr;
092800110531           ENDIF;
092900110531         ENDIF;
093000110531
093100110531       //?Controllo se stessa area utente
093200110531         IF  %lookup(%editc(CPOflt:'X'):POG) = 0;
093300110531           leavesr;
093400110531         ENDIF;
093500110531
093600110531       //?Contattati
093700110531         IF  D13pst <> *blanks;
093800120914           IF  (D13pst = 'N' and §CPOdtpcon <> *blanks and
093900120914                                 §CPOdtpcon <> *zeros) or
094000120914               (D13pst = 'S' and (§CPOdtpcon = *blanks or
094100120914                                  §CPOdtpcon = *zeros));
094200110531             leavesr;
094300110531           ENDIF;
094400110531         ENDIF;
094500110531
094600110531       //?Controllo se sede/filiale
094700110531         IF  D13fsf <> *blanks and CPOfsf <> D13fsf;
094800110531           leavesr;
094900110531         ENDIF;
095000140925
095100140925       //?% Fatturato Export
095200140930         IF  D13pexpa > *zeros and
095300140925            (CPOpexp < D13pexpda or CPOpexp > D13pexpa);
095400140925           leavesr;
095500140925         ENDIF;
095600140925
095700140925       //?Anno inizio attività
095800140930         IF  D13annoa > *zeros and
095900140925            (§CPOannoia < %editc(D13annoda:'X') or
096000140925             §CPOannoia > %editc(D13annoa:'X'));
096100140925           leavesr;
096200140925         ENDIF;
096300110531
096400110608       //?Controllo trattative in corso
096500110607         wTtr = 0;
096600110607         setll (CPOcpo) TIVIS01L;
096700110607         DOW  not $EndVis;
096800110607           reade (CPOcpo) TIVIS01L;
096900110607           IF  %eof(TIVIS01L);
097000110607             $EndVis = *on;
097100110607             leave;
097200110607           ENDIF;
097300110607           IF  VISffz = *blanks and VISdch = 0;
097400110607             wTtr += 1;
097500110607             $EndVis = *on;
097600110607           ENDIF;
097700110607         ENDDO;
097800110607       //?Controllo trattative in corso
097900110607         IF  D13ttr <> *blanks;
098000110607           IF (D13ttr = 'S' and wTtr = 0) or
098100110607              (D13ttr = 'N' and wTtr > 0);
098200110607             leavesr;
098300110607           ENDIF;
098400110531         ENDIF;
098500110531
098600110531       //?Controllo prossima attività
098700110531         IF  D13cmm > *zeros or D13cco <> *blanks or
098800110531             D13dadda > *zeros;
098900110606           clear wDate;
099000110606           clear wAtt;
099100110606           setll (CPOcpo:wDate) TIATC05L;
099200110608           DOW  not $EndAtc;
099300110606             reade (CPOcpo:wDate) TIATC05L;
099400110531             IF  %eof(TIATC05L);
099500110608               $EndAtc = *on;
099600110531               leave;
099700110531             ENDIF;
099800110608             IF  D13cmm > 0 and %lookup(ATCcmm:$Cmm) = 0;
099900110608               iter;
100000110608             ENDIF;
100100110608             IF  D13cco <> *blanks and %lookup(ATCcad:$cco) = 0;
100200110608               iter;
100300110608             ENDIF;
100400110608             IF  D13dadda <> *zeros and ATCdad < D13dadda;
100500110608               iter;
100600110608             ENDIF;
100700110608             IF  D13dada <> *zeros and ATCdad > D13dada;
100800110531               iter;
100900110531             ENDIF;
101000110531             wAtt += 1;
101100110531             $EndAtc = *on;
101200110531           ENDDO;
101300110608           IF  wAtt = 0;
101400110531             leavesr;
101500110531           ENDIF;
101600110531         ENDIF;
101700110531
101800110531       //?Controllo le parzializzazioni delle INFO
101900110531       //?se utente abilitato alla stampa
102000110607         IF  $InfoAbi;
102100110531           exsr  CtrInfo;
102200110607           IF  $InfoNoOk;
102300110531             leavesr;
102400110531           ENDIF;
102500110531         ENDIF;
102600110531
102700110531       //?Se arrivo qua il rcd è OK
102800110531         $RcdOk = *on;
102900110607
103000110607       //?Sommo i clienti stampati
103100110607         TotCli += 1;
103200110531
103300110531       //?Cerco Commerciale Unificante potenziale
103400130916         //IF  D13cmmu = 0;
103500130916         //  chain   (CPOcmm)  AZCMM000;
103600130916         //ENDIF;
103700110531
103800110531       ENDSR;
103900110531
104000110531       //--------------------------------------------------------------
104100110531       //?Controllo se INFO OK.
104200110531       //--------------------------------------------------------------
104300110531       BEGSR CtrInfo;
104400110531
104500110607         $InfoNoOk = *on;
104600110607         wInfo     = *off;
104700110607
104800110607       //?Info non presenti
104900110607         IF  D13nifo = 'S';
105000110607           setll (CPOcpo) TICPI01L;
105100110607         //?non trovo info OK ed esco
105200110607           IF  not %equal;
105300110607             $InfoNoOk = *off;
105400110607             leavesr;
105500110607           ENDIF;
105600110607         //?trovo info mi vanno bene se info da annullare o
105700110607         //?concorrenti senza %
105800110607           reade (CPOcpo) TICPI01L;
105900110607           DOW not %eof(TICPI01L);
106000110607           //?al primo concorrente con i dati ok esco
106100110607             IF  CPItpf = '10' and CPIval > 0;
106200110607               wInfo = *on;
106300110607               leavesr;
106400110607             ENDIF;
106500110607           //?alla prima info non da annullare esco
106600110607             xx = 1;
106700110607             xx = %lookup(CPItpf:IFO);
106800110607             IF  xx > 0;
106900110607               dIFO = IFOds(xx);
107000110607               IF  §IFOann <> 'S';
107100110607                 wInfo = *on;
107200110607                 leavesr;
107300110607               ENDIF;
107400110607             ENDIF;
107500110607             reade (CPOcpo) TICPI01L;
107600110607           ENDDO;
107700110607       //?Se ho trovato delle info buone esco
107800110607           IF  wInfo;
107900110607             leavesr;
108000110607           ENDIF;
108100110607         ENDIF;
108200110531
108300110531       //?Spesa trasporti
108400110531         IF  D13spta > 0 and (§CPOsptp < D13sptda or
108500110531                              §CPOsptp > D13spta);
108600110531           leavesr;
108700110531         ENDIF;
108800140925
108900140925       //?Spesa presunta
109000141031         IF  D13sptp = *blanks and D13spta > 0;
109100140925           chain (CPOcpo:'SPT') TICPI01L;
109200141006           IF  not %found(TICPI01L);
109300140925             leavesr;
109400140925           ENDIF;
109500140925         ENDIF;
109600110531
109700110531       //?Data ultimo aggiornamento INFO
109800140925         //IF  D13ifoal > 0 and §CPOifodul > *zeros and
109900140925         //    %dec(§CPOifodul:8:0) > D13ifoal;
110000140925         IF  D13ifoal > 0 and CPO1ducifo > 0 and
110100140925             CPO1ducifo > D13ifoal;
110200110531           leavesr;
110300110531         ENDIF;
110400110607
110500110607         wInfo = *off;
110600110531
110700110531       //?Ripartizione spesa
110800110531         IF  D13ripg <> *blanks or D13rip <> *blanks;
110900110531           setll (CPOcpo) TICPI01L;
111000110607           reade (CPOcpo) TICPI01L;
111100110607           DOW  not %eof(TICPI01L);
111200110531         //?Ripartizione spesa generico (ITA/EST)
111300110606             IF  D13ripg <> *blanks;
111400110606               xx = 1;
111500110606               xx = %lookup(CPItpf:IFO);
111600110607               IF  xx > 0 and IFOcg (xx) = '3T';
111700110607                 dIFO = IFOds(xx);
111800110607                 IF  §IFOtip = D13ripg;
111900110607                   wInfo = *on;
112000110607                   leave;
112100110607                 ENDIF;
112200110606               ENDIF;
112300110606             ENDIF;
112400110531         //?Ripartizione spesa
112500110606             IF  D13rip <> *blanks and %lookup(CPItpf:$rip) > 0;
112600110607               wInfo = *on;
112700110607               leave;
112800110606             ENDIF;
112900110607             reade (CPOcpo) TICPI01L;
113000110606           ENDDO;
113100110607         //?Controllo la % di ripartizione
113200110607           IF  wInfo and D13ripa > 0 and
113300110607              (CPIval < D13ripda or CPIval > D13ripa);
113400110607             wInfo = *off;
113500110607           ENDIF;
113600110607        //?Se non ho trovato delle info buone esco
113700110607           IF  not wInfo;
113800110607             leavesr;
113900110607           ENDIF;
114000110606         ENDIF;
114100110531
114200110607         wInfo = *off;
114300140925
114400140925       //?Non affidato a BRT
114500140925       //?Italia - Parcel
114600140925         IF  D13nrfa > 0;
114700140925           chain (CPOcpo:'NRF') TICPI01L;
114800140925           IF  not %found(TICPI01L) or
114900140925              (CPIpft < D13nrfda or CPIpft > D13nrfa);
115000140925             leavesr;
115100140925           ENDIF;
115200140925         ENDIF;
115300140925
115400140925       //?Italia - Messaggeria
115500140925         IF  D13nroa > 0;
115600140925           chain (CPOcpo:'NRO') TICPI01L;
115700140925           IF  not %found(TICPI01L) or
115800140925              (CPIpft < D13nroda or CPIpft > D13nroa);
115900140925             leavesr;
116000140925           ENDIF;
116100140925         ENDIF;
116200140925
116300140925       //?Estero - Via Aerea
116400140925         IF  D13nera > 0;
116500140925           chain (CPOcpo:'NER') TICPI01L;
116600140925           IF  not %found(TICPI01L) or
116700140925              (CPIpft < D13nerda or CPIpft > D13nera);
116800140925             leavesr;
116900140925           ENDIF;
117000140925         ENDIF;
117100140925
117200140925       //?Estero - Serv. Camionistico
117300140925         IF  D13naea > 0;
117400140925           chain (CPOcpo:'NAE') TICPI01L;
117500140925           IF  not %found(TICPI01L) or
117600140925              (CPIpft < D13naeda or CPIpft > D13naea);
117700140925             leavesr;
117800140925           ENDIF;
117900140925         ENDIF;
118000140925
118100140925       //?Altro
118200140925         IF  D13ntra > 0;
118300140925           chain (CPOcpo:'NTR') TICPI01L;
118400140925           IF  not %found(TICPI01L) or
118500140925              (CPIpft < D13ntrda or CPIpft > D13ntra);
118600140925             leavesr;
118700140925           ENDIF;
118800140925         ENDIF;
118900140925
119000140925       //?Paesi Estero
119100140925         IF  D13paesi <> *blanks;
119200140925           setll (CPOcpo:'50') TICPI01L;
119300140925           reade (CPOcpo:'50') TICPI01L;
119400140925           DOW  not %eof(TICPI01L);
119500140925             IF  %lookup(CPIspf:$paesi) > 0;
119600140925               leave;
119700140925             ENDIF;
119800140925             reade (CPOcpo:'50') TICPI01L;
119900140925           ENDDO;
120000140925         ENDIF;
120100110531
120200110531       //?Spedizioni a Privati
120300110531         IF  D13pri = 'S';
120400110531         chain (CPOcpo:'PRI') TICPI01L;
120500110531           IF  not %found(TICPI01L);
120600110531             leavesr;
120700110531           ENDIF;
120800110531         ENDIF;
120900110531
121000110531       //?Spedizioni a GDO
121100110531         IF  D13gdo = 'S';
121200110531         chain (CPOcpo:'GDO') TICPI01L;
121300110531           IF  not %found(TICPI01L);
121400110531             leavesr;
121500110531           ENDIF;
121600110531         ENDIF;
121700110531
121800110531       //?Spedizioni e-commerce
121900110531         IF  D13ecm = 'S';
122000110531         chain (CPOcpo:'ECM') TICPI01L;
122100110531           IF  not %found(TICPI01L);
122200110531             leavesr;
122300110531           ENDIF;
122400110531         ENDIF;
122500110531
122600110531       //?Spedizioni 10:30/Priority
122700110531         IF  D13hpr = 'S';
122800110531         chain (CPOcpo:'HPR') TICPI01L;
122900110531           IF  not %found(TICPI01L);
123000110531             leavesr;
123100110531           ENDIF;
123200110531         ENDIF;
123300110601
123400110601       //?Spedizioni SECURE BOX
123500110601         IF  D13sbx = 'S';
123600110601         chain (CPOcpo:'SBX') TICPI01L;
123700110601           IF  not %found(TICPI01L) or
123800110601              (%found(TICPI01L) and CPIfsn <> 'S');
123900110601             leavesr;
124000110601           ENDIF;
124100110601         ENDIF;
124200110531
124300110531       //?Peso medio
124400110531         IF  D13pesa > 0;
124500110531         chain (CPOcpo:'KMS') TICPI01L;
124600110531           IF  not %found(TICPI01L) or
124700110531              (CPIval < D13pesda or CPIval > D13pesa);
124800110531             leavesr;
124900110531           ENDIF;
125000110531         ENDIF;
125100110531
125200110531       //?Affidato a BRT
125300110531         IF  D13bara > 0;
125400110531         chain (CPOcpo:'10':'_BAR') TICPI01L;
125500110531           IF  not %found(TICPI01L) or
125600110531              (CPIval < D13barda or CPIval > D13bara);
125700110531             leavesr;
125800110531           ENDIF;
125900110531         ENDIF;
126000110531
126100110531       //?Concorrenti
126200110531         IF  D13conc <> *blanks;
126300110531           setll (CPOcpo:'10') TICPI01L;
126400110607           reade (CPOcpo:'10') TICPI01L;
126500110607           DOW  not %eof(TICPI01L);
126600110531             IF  %lookup(CPIspf:$conc) > 0;
126700110607               wInfo = *on;
126800110607               leave;
126900110531             ENDIF;
127000110531             reade (CPOcpo:'10') TICPI01L;
127100110531           ENDDO;
127200110607         //?Controllo la %
127300110607           IF  wInfo and D13cona > 0 and
127400110607              (CPIval < D13conda or CPIval > D13cona);
127500110607             wInfo = *off;
127600110607             leavesr;
127700110607           ENDIF;
127800110607         //?Se non ho trovato delle info buone esco
127900110607           IF  not wInfo;
128000110607             leavesr;
128100110607           ENDIF;
128200110607         ENDIF;
128300110531
128400110607         wInfo = *off;
128500110531
128600110531       //?Motivo non affidamento
128700110531         IF  D13moi <> *blanks;
128800110531           setll (CPOcpo:'MOI') TICPI01L;
128900110607           reade (CPOcpo:'MOI') TICPI01L;
129000110607           DOW  not %eof(TICPI01L);
129100110531             IF  %lookup(CPIspf:$moi) > 0;
129200110607               wInfo = *on;
129300110607               leave;
129400110531             ENDIF;
129500110531             reade (CPOcpo:'MOI') TICPI01L;
129600110531           ENDDO;
129700110607           IF  not wInfo;
129800110531             leavesr;
129900110531           ENDIF;
130000110531         ENDIF;
130100120308
130200120308       //?Logistica interessante per noi
130300120308         IF  D13logi = 'S';
130400120308         chain (CPOcpo:'LOG') TICPI01L;
130500120308           IF  not %found(TICPI01L) or
130600120308               CPIfsn = 'N';
130700120308             leavesr;
130800120308           ENDIF;
130900120308         ENDIF;
131000110531
131100110531       //?Logistica in outsourcing
131200110531         IF  D13logo = 'S';
131300110531         chain (CPOcpo:'OUT') TICPI01L;
131400110531           IF  not %found(TICPI01L) or
131500110531               CPIfsn = 'N';
131600110531             leavesr;
131700110531           ENDIF;
131800110531         ENDIF;
131900110531
132000110531       //?Operatore Logistica
132100110531         IF  D13logp <> *blanks;
132200110531         chain (CPOcpo:'40':D13logp) TICPI01L;
132300110531           IF  not %found(TICPI01L);
132400110531             leavesr;
132500110531           ENDIF;
132600110531         ENDIF;
132700140925
132800140925       //?Metri quadri necessari
132900140925         IF  D13mqla > 0;
133000140925           chain (CPOcpo:'MQL') TICPI01L;
133100140925           IF  not %found(TICPI01L) or
133200140925              (CPIval < D13mqlda or CPIval > D13mqla);
133300140925             leavesr;
133400140925           ENDIF;
133500140925         ENDIF;
133600140925
133700140925       //?Posti pallet
133800140925         IF  D13ppla > 0;
133900140925           chain (CPOcpo:'PPL') TICPI01L;
134000140925           IF  not %found(TICPI01L) or
134100140925              (CPIval < D13pplda or CPIval > D13ppla);
134200140925             leavesr;
134300140925           ENDIF;
134400140925         ENDIF;
134500110531
134600110531       //?Se arrivo qua le INFO sono OK
134700110607         $InfoNoOk = *off;
134800110531
134900110531       ENDSR;
135000110531
135100110531       //--------------------------------------------------------------
135200110531       //?Stampo.
135300110531       //--------------------------------------------------------------
135400110531       BEGSR Stampa;
135500110531
135600110531       //?Salto pagina
135700110601         IF  NrPag = 0 or
135800110608            (*in11 and WFCcmmu <> sav_Cmmu) or
135900110608            (*in12 and WFCcmm <> sav_Cmm) or
136000110608            (*in13 and (WFCare <> sav_Car or
136100110607                                WFCflt <> sav_Flt)) or
136200110620            (*in14 and CPOsct <> sav_Sct) or
136300110601             Curline >= 63;
136400110601           exsr StpSalto;
136500110601           *in99 = *off;
136600110601           Nrpag += 1;
136700110620           IF  *in11 or *in12 or *in13 or *in14;
136800110607             sav_Cmmu = WFCcmmu;
136900110607             sav_Cmm  = WFCcmm;
137000110607             sav_Car  = WFCare;
137100110607             sav_Flt  = WFCflt;
137200110607             sav_SCT  = CPOsct;
137300110607           ENDIF;
137400110601         ENDIF;
137500110601
137600110601       //?Sede Filiale
137700110601         IF  CPOfsf = 'S';
137800110601           stpfsf = 'Se';
137900110601         ENDIF;
138000110601         IF  CPOfsf = 'F';
138100110601           stpfsf = 'Fi';
138200110601         ENDIF;
138300110601       //?Ragione Sociale
138400110601         stprag = CPOrag;
138500110601       //?Località
138600110601         stpcit = CPOcit;
138700110607       //?Trattativa in corso
138800110607         clear stpttr;
138900110607         IF wTtr > 0;
139000110607           stpttr = 'S';
139100110607         ENDIF;
139200110601       //?Prossima attività
139300110601         exsr ProAtt;
139400110601       //?Categoria merceologica
139500110601         clear ds1L;
139600110601         k_TBLkut = 1;
139700110601         k_TBLcod = '1L';
139800110601         k_TBLkey = %editc(CPOsct:'X');
139900110601         chain %kds(K03tabel) TABEL00F;
140000110601         IF  %found(TABEL00F) and TBLflg = *blanks;
140100110601           ds1L = TBLuni;
140200110601         ENDIF;
140300110601         stpdsct = §1Ldes;
140400110601
140500110601         //?Se abilitato
140600110610         clear stpdim;
140700110610         clear stpsptp;
140800110610         clear flgsptp;
140900110601         IF  *in10;
141000110601         //?Data ultimo aggiornamento info commerciali
141100140930          // IF  §CPOifodul > *zeros;
141200140930           IF  CPO1ducifo > *zeros;
141300140930            // wDate = %dec(§CPOifodul:8:0);
141400140930             wDate = %dec(CPO1ducifo:8:0);
141500110606             wDate_ISO = %date(wDate:*iso);
141600110606             wDate_EUR = wDate_ISO;
141700110606             stpdim    = %dec(wDate_EUR);
141800110606           ENDIF;
141900110601         //?Spesa trasporti
142000110601           chain (CPOcpo:'SPT') TICPI01L;
142100110601           IF  %found(TICPI01L);
142200110601             stpsptp = CPIpft;
142300110601             clear flgsptp;
142400110601           ELSE;
142500110601             stpsptp = §CPOsptp;
142600110601             flgsptp = 'P';
142700110601           ENDIF;
142800110601         ENDIF;
142900110601
143000110601         write MK13RIGA;
143100110601
143200110601         //?Info commerciali
143300110601         IF  *in10 and D13sifo = 'S';
143400110606           exsr Info_Comm;
143500110607           //?Stampo la riga di separazione
143600110607           wrkriga = *all'-';
143700110607           write MK13SEPARA;
143800110601         ENDIF;
143900110531
144000110531       ENDSR;
144100110601
144200110601       //--------------------------------------------------------------
144300110601       //?Stampo il salto pagina.
144400110601       //--------------------------------------------------------------
144500110606       BEGSR StpSalto;
144600110608
144700110620       //?Imposto le intestazioni delle rotture
144800110608         IF  *in11;
144900130916           clear  DesCmmU;
145000130916           chain  (WFCcmmu)  AZCMM000;
145100130916           IF  %found(AZCMM01L);
145200130916             descmmu = CMMdes;
145300110608           ENDIF;
145400110608         ENDIF;
145500110608         IF  *in12;
145600130916           clear  DesCmm;
145700130916           chain  (WFCcmm)  AZCMM000;
145800130916           IF  %found(AZCMM01L);
145900130916             descmm = CMMdes;
146000110608           ENDIF;
146100110608         ENDIF;
146200110608         IF  *in13;
146300110608           clear ds05;
146400110608           k_TBLkut = 1;
146500110608           k_TBLcod = '05';
146600110608           k_TBLkey = %editc(WFCare:'X');
146700110608           chain %kds(K03tabel) TABEL00F;
146800110608           IF  %found(TABEL00F) and TBLflg = *blanks;
146900110608             ds05 = TBLuni;
147000110608           ENDIF;
147100110608           descar = §05des;
147200110608           clear ORGdes;
147300110608           chain (WFCflt) AZORG01L;
147400110608           IF  %found(AZORG01L) and ORGfva = *blanks;
147500110608             desflt = ORGdes;
147600110608           ENDIF;
147700110608         ENDIF;
147800110620         IF  *in14;
147900110620           clear ds1L;
148000110620           k_TBLkut = 1;
148100110620           k_TBLcod = '1L';
148200110620           k_TBLkey = %editc(CPOsct:'X');
148300110620           chain %kds(K03tabel) TABEL00F;
148400110620           IF  %found(TABEL00F) and TBLflg = *blanks;
148500110620             ds1L = TBLuni;
148600110620           ENDIF;
148700110620           dessct = §1Ldes;
148800110620         ENDIF;
148900110620
149000110620         //?Stampo la testata
149100110620         IF  NrPag = 0 or
149200110620            (D13ocmmu = 'P' and WFCcmmu <> sav_Cmmu) or
149300110620            (D13ocmm  = 'P' and WFCcmm <> sav_Cmm) or
149400110620            (D13ocar  = 'P' and (WFCare <> sav_Car or
149500110620                                WFCflt <> sav_Flt)) or
149600110620            (D13osct  = 'P' and CPOsct <> sav_Sct) or
149700110620             Curline >= 63;
149800110601
149900110620           //?Stampo la testata
150000110620           write MK13PAGINA;
150100110620           write MK13TESTA;
150200110620           write MK13SEPARA;
150300110620           //?Stampo le intestazioni delle colonne
150400110620           write MK13INTCOL;
150500110620           wrkriga = *all'=';
150600110620           write MK13SEPARA;
150700110620         ENDIF;
150800110620
150900110620         //?Stampo le rotture
151000110620         IF  *in11 or *in12 or *in13 or *in14;
151100110620           clear wrkriga;
151200110620           write MK13SEPARA;
151300110620           write MK13SALTA;
151400110620           write MK13SEPARA;
151500110620         ENDIF;
151600110601
151700110601       ENDSR;
151800110601
151900110601       //--------------------------------------------------------------
152000110601       //?Prossima attività da eseguire.
152100110601       //--------------------------------------------------------------
152200110601       BEGSR ProAtt;
152300110610
152400110610         clear stpdad;
152500110610         clear stpcad;
152600110610         clear stpdcad;
152700110610         clear stpdcmm;
152800110601
152900110601       //?Cerco la prossima attività
153000110601         $EndAtt = *off;
153100110601         exec sql
153200110606         declare ATT cursor for select ATCcad, ATCdad, ATCcmm from TIATC00F
153300110601         where ATCcpo = :CPOcpo and atcdco = 0
153400110601         order by ATCdad, ATChda;
153500110601         exec sql
153600110601         open ATT;
153700110601         IF sqlcode < 0;
153800110601           $EndAtt = *on;
153900110601         ENDIF;
154000110601         DOW not $EndAtt;
154100110601           exec sql
154200110606           fetch next from ATT into :ATCcad, :ATCdad, :ATCcmm;
154300110601           IF sqlcod = 100 or sqlcod < 0;
154400110601             $EndAtt = *on;
154500110601             leave;
154600110601           ENDIF;
154700110606         //?Data prossima attività
154800110606           wDate_ISO = %date(ATCdad:*iso);
154900110606           wDate_EUR = wDate_ISO;
155000110606           stpdad    = %dec(wDate_EUR);
155100110606         //?Causale prossima attività
155200110606           stpcad    = ATCcad;
155300110606           clear TIBS02ds;
155400110606           T02mod = 'C';
155500110606           T02cod = 'CCO';
155600110606           T02ke1 = stpcad;
155700110606           T02sif = KNSIF;
155800110606           TNTBE_RicercaControllo (kpjba : tibs02ds);
155900110606           IF  T02err = *blanks;
156000110606             dcco = T02uni;
156100110606           ENDIF;
156200110606           stpdcad = §CCOdes;
156300110606         //?Commerciale prossima attività
156400130916           clear  StpDcmm;
156500130916           chain  (ATCcmm)  AZCMM000;
156600130916           IF  %found(AZCMM01L);
156700130916             stpdcmm = CMMdes;
156800110606           ENDIF;
156900110606           leave;
157000110606         ENDDO;
157100110606         exec sql close ATT;
157200110601
157300110601       ENDSR;
157400110601
157500110601       //--------------------------------------------------------------
157600110606       //?INFO Commerciali.
157700110601       //--------------------------------------------------------------
157800110606       BEGSR Info_Comm;
157900110606
158000110606         clear Stringa_Info;
158100110606         clear sav_CPItpf;
158200110606
158300110606       //?Leggo le INFO per stampare il Peso medio
158400110606         chain (CPOcpo:'KMS') TICPI01L;
158500110606         IF  %found(TICPI01L);
158600110606           exsr Componi_Info;
158700110606         ENDIF;
158800110606
158900110606       //?Legge le INFO per stampare i CONCORRENTI
159000110606         setll (CPOcpo:'10') TICPI01L;
159100110606         reade (CPOcpo:'10') TICPI01L;
159200110606         DOW  not %eof(TICPI01L);
159300110606         //?Compone la stringa da stampare
159400110606           exsr Componi_Info;
159500110606           reade (CPOcpo:'10') TICPI01L;
159600110606         ENDDO;
159700110606
159800110606       //?Stampo ultima riga concorrenti
159900110606         IF  Stringa_Info <> *blanks;
160000110606           exsr Stampa_Info;
160100110606         ENDIF;
160200110606
160300110606       //?Stampo le altre info solo se richieste
160400110606         clear Stringa_Info;
160500110606         clear sav_CPItpf;
160600110606
160700110606       //?Ripartizione spesa ITA/EST
160800110606         IF  D13ripg <> *blanks or D13rip <> *blanks;
160900110606           setll (CPOcpo) TICPI01L;
161000110606           reade (CPOcpo) TICPI01L;
161100110606           DOW  not %eof(TICPI01L);
161200110606         //?Generica
161300110606             IF  D13ripg <> *blanks;
161400110606               xx = 1;
161500110606               xx = %lookup(CPItpf:IFO);
161600110606               IF  xx > 0 and IFOcg(xx) = '3T';
161700110606                 dIFO = IFOds(xx);
161800110606                 IF  §IFOtip = D13ripg;
161900110606                   exsr Componi_Info;
162000110606                 ENDIF;
162100110606               ENDIF;
162200110606         //?Specifica
162300110606             ELSE;
162400110606               IF  %lookup(CPItpf:$rip) > 0;
162500110606                 exsr Componi_Info;
162600110606               ENDIF;
162700110606             ENDIF;
162800110606             reade (CPOcpo) TICPI01L;
162900110606           ENDDO;
163000110606         ENDIF;
163100140925
163200140925       //?Non affidato BRT
163300140925         IF  D13nrfa > 0;
163400140925           chain (CPOcpo:'NRF') TICPI01L;
163500140925           IF  %found(TICPI01L);
163600140925             exsr Componi_Info;
163700140925           ENDIF;
163800140925         ENDIF;
163900140925         IF  D13nroa > 0;
164000140925           chain (CPOcpo:'NRO') TICPI01L;
164100140925           IF  %found(TICPI01L);
164200140925             exsr Componi_Info;
164300140925           ENDIF;
164400140925         ENDIF;
164500140925         IF  D13nera > 0;
164600140925           chain (CPOcpo:'NER') TICPI01L;
164700140925           IF  %found(TICPI01L);
164800140925             exsr Componi_Info;
164900140925           ENDIF;
165000140925         ENDIF;
165100140925         IF  D13naea > 0;
165200140925           chain (CPOcpo:'NAE') TICPI01L;
165300140925           IF  %found(TICPI01L);
165400140925             exsr Componi_Info;
165500140925           ENDIF;
165600140925         ENDIF;
165700140925         IF  D13ntra > 0;
165800140925           chain (CPOcpo:'NTR') TICPI01L;
165900140925           IF  %found(TICPI01L);
166000140925             exsr Componi_Info;
166100140925           ENDIF;
166200140925         ENDIF;
166300110606
166400110606       //?Privati
166500110606         IF  D13pri = 'S';
166600110606           chain (CPOcpo:'PRI') TICPI01L;
166700110606           IF  %found(TICPI01L);
166800110606             exsr Componi_Info;
166900110606           ENDIF;
167000110606         ENDIF;
167100110606
167200110606       //?GDO
167300110606         IF  D13gdo = 'S';
167400110606           chain (CPOcpo:'GDO') TICPI01L;
167500110606           IF  %found(TICPI01L);
167600110606             exsr Componi_Info;
167700110606           ENDIF;
167800110606         ENDIF;
167900110606
168000110606       //?E-Commerce
168100110606         IF  D13ecm = 'S';
168200110606           chain (CPOcpo:'ECM') TICPI01L;
168300110606           IF  %found(TICPI01L);
168400110606             exsr Componi_Info;
168500110606           ENDIF;
168600110606         ENDIF;
168700110606
168800110606       //?H 10:30/Priority
168900110606         IF  D13hpr = 'S';
169000110606           chain (CPOcpo:'HPR') TICPI01L;
169100110606           IF  %found(TICPI01L);
169200110606             exsr Componi_Info;
169300110606           ENDIF;
169400110606         ENDIF;
169500110606
169600110606       //?Secure Box
169700110606         IF  D13sbx = 'S';
169800110606           chain (CPOcpo:'SBX') TICPI01L;
169900110606           IF  %found(TICPI01L);
170000110606             exsr Componi_Info;
170100110606           ENDIF;
170200110606         ENDIF;
170300110606
170400110606       //?Motivo non affidamento
170500110606         IF  D13moi <> *blanks;
170600110606           setll (CPOcpo:'MOI') TICPI01L;
170700110606           reade (CPOcpo:'MOI') TICPI01L;
170800110606           DOW  not %eof(TICPI01L);
170900110606             IF  %lookup(CPIspf:$moi) > 0;
171000110606               exsr Componi_Info;
171100110606             ENDIF;
171200110606             reade (CPOcpo:'MOI') TICPI01L;
171300110606           ENDDO;
171400110606         ENDIF;
171500110606
171600110606       //?Logistica interessante per noi
171700110606         IF  D13logi = 'S';
171800110606           chain (CPOcpo:'LOG') TICPI01L;
171900110606           IF  %found(TICPI01L);
172000110606             exsr Componi_Info;
172100110606           ENDIF;
172200110606         ENDIF;
172300110606
172400110606       //?Logistica in outsourcing
172500110606         IF  D13logo = 'S';
172600110606           chain (CPOcpo:'OUT') TICPI01L;
172700110606           IF  %found(TICPI01L);
172800110606             exsr Componi_Info;
172900110606           ENDIF;
173000110606         ENDIF;
173100110606
173200110606       //?Operatore logistica
173300110606         IF  D13logp <> *blanks;
173400110606           chain (CPOcpo:'40':D13logp) TICPI01L;
173500110606           IF  %found(TICPI01L);
173600110606             exsr Componi_Info;
173700110606           ENDIF;
173800110606         ENDIF;
173900140925
174000140925       //?Mq
174100140925         IF  D13mqla > 0;
174200140925           chain (CPOcpo:'MQL') TICPI01L;
174300140925           IF  %found(TICPI01L);
174400140925             exsr Componi_Info;
174500140925           ENDIF;
174600140925         ENDIF;
174700140925
174800140925       //?Posti pallet
174900140925         IF  D13ppla > 0;
175000140925           chain (CPOcpo:'PPL') TICPI01L;
175100140925           IF  %found(TICPI01L);
175200140925             exsr Componi_Info;
175300140925           ENDIF;
175400140925         ENDIF;
175500110606
175600110606       //?Stampo ultima riga concorrenti
175700110606         IF  Stringa_Info <> *blanks;
175800110606           exsr Stampa_Info;
175900110606         ENDIF;
176000110606
176100110606       ENDSR;
176200110606
176300110606       //--------------------------------------------------------------
176400110606       //?Compone la Stringa INFO da stampare.
176500110606       //--------------------------------------------------------------
176600110606       BEGSR Componi_Info;
176700110606
176800110606       //?A cambio tipo INFO
176900110606         IF  CPItpf <> sav_CPItpf;
177000110606         //?Decodifica la INFO
177100110606           clear dIFO;
177200110606           clear TIBS02ds;
177300110606           T02mod = 'C';
177400110606           T02cod = 'IFO';
177500110606           T02ke1 = CPItpf;
177600110606           T02sif = KNSIF;
177700110606           TNTBE_RicercaControllo (kpjba : tibs02ds);
177800110606           IF  T02err = *blanks;
177900110606             dIFO = T02uni;
178000110606           ENDIF;
178100110606         //?Imposta la stringa
178200110606           IF  Stringa_Info <> *blanks;
178300140930           //?se ripartizione del NON affidato a BRT prima imposto la scritta
178400140930           //?'NON affidato a BRT'
178500140930             IF  CPItpf = 'NRF' or CPItpf = 'NRO' or CPItpf = 'NER' or
178600140930                 CPItpf = 'NAE' or CPItpf = 'NTR';
178700140930                 Stringa_Info = %trim(Stringa_info) + '   ' +
178800140930                               'Non Affidato a BRT ' + §IFOdeb;
178900140930             ELSE;
179000110606             Stringa_Info = %trim(Stringa_info) + '   ' +
179100110606                            §IFOdeb;
179200140930             ENDIF;
179300110606           ELSE;
179400140930           //?se ripartizione del NON affidato a BRT prima imposto la scritta
179500140930           //?'NON affidato a BRT'
179600140930             IF  CPItpf = 'NRF' or CPItpf = 'NRO' or CPItpf = 'NER' or
179700140930                 CPItpf = 'NAE' or CPItpf = 'NTR';
179800140930                 Stringa_Info = 'Non Affidato a BRT ' + §IFOdeb;
179900140930             ELSE;
180000110606             Stringa_Info = §IFOdeb;
180100140930             ENDIF;
180200110606           ENDIF;
180300110606           sav_CPItpf = CPItpf;
180400110606         ENDIF;
180500110606
180600110606       //?Decodifica il sottotipo della INFO
180700110606         IF  CPIspf <> *blanks;
180800110606           clear dIFS;
180900110606           clear TIBS02ds;
181000110606           T02mod = 'C';
181100110606           T02cod = 'IFS';
181200110606           T02ke1 = CPItpf;
181300110606           T02ke2 = CPIspf;
181400110606           T02sif = KNSIF;
181500110606           TNTBE_RicercaControllo (kpjba : tibs02ds);
181600110606           IF  T02err = *blanks;
181700110606             dIFS = T02uni;
181800110606           ENDIF;
181900110606         //?Imposta la stringa
182000110606           IF  Stringa_Info <> *blanks;
182100110606             Stringa_Info = %trim(Stringa_info) + '   ' +
182200110606                            %subst(§IFSdes:1:20);
182300110606           ELSE;
182400110606             Stringa_Info = %subst(§IFSdes:1:20);
182500110606           ENDIF;
182600110606         ENDIF;
182700110606
182800140930       //?imposta % - Kg - S/N - spesa trasporti - EUR
182900110606         SELECT;
183000110606           WHEN  CPIval > 0 and CPItva = '%';
183100110606             w0030 = CPIval;
183200110606             Stringa_Info = %trim(Stringa_Info) + ' ' +
183300110606                            %editc(w0030:'Z') + CPItva;
183400110606           WHEN  CPIval > 0 and CPItva = 'KG';
183500110606             Stringa_Info = %trim(Stringa_Info) + ' ' +
183600110606                            %editc(CPIval:'Z') + CPItva;
183700140925           WHEN  CPIpft > 0 and CPItpf = 'SPT';
183800110606             Stringa_Info = %trim(Stringa_Info) +
183900110606                            ' SpesaTRA ' +
184000110606                            %editc(CPIpft:'Z');
184100110606           WHEN  CPIsna > 0;
184200110606             Stringa_Info = %trim(Stringa_Info) +
184300110606                            ' Spediz. ' +
184400110606                            %editc(CPIsna:'Z');
184500140930           WHEN  CPIpft > 0;
184600140930             Stringa_Info = %trim(Stringa_Info) + ' ' +
184700140930                            %editc(CPIpft:'Z') + ' ' + CPIvft;
184800110606           WHEN  CPIspf = *blanks and CPIfsn = 'S';
184900110606             Stringa_Info = %trim(Stringa_Info) +
185000110606                            ':Sì';
185100110606           WHEN  CPIspf = *blanks and CPIfsn = 'N';
185200110606             Stringa_Info = %trim(Stringa_Info) +
185300110606                            ':No';
185400110606         ENDSL;
185500110606
185600110606       //?Stampo le info
185700110606         clear wPos;
185800110606         wPos =  %checkr('  ':Stringa_Info);
185900110606         IF  wPos > 143;
186000110606           exsr Stampa_Info;
186100110606         ENDIF;
186200110606
186300110606       ENDSR;
186400110606
186500110606       //--------------------------------------------------------------
186600110606       //?Stampa INFO Commerciali.
186700110606       //--------------------------------------------------------------
186800110606       BEGSR Stampa_Info;
186900110606
187000110606       //?Salto pagina
187100110606         IF  Curline >= 63;
187200110606           exsr StpSalto;
187300110606           *in99 = *off;
187400110606         ENDIF;
187500110606
187600110606         infocom = Stringa_Info;
187700110606         write MK13INFO;
187800110606         clear Stringa_Info;
187900110606
188000110606       ENDSR;
188100110525
188200110525       //--------------------------------------------------------------
188300110525       //?Operazioni finali.
188400110525       //--------------------------------------------------------------
188500110525       BEGSR RoutEnd;
188600110525
188700110525         *inLR = *on;
188800110525         return;
188900110525
189000110525       ENDSR;
189100110525
189200110525      /end-free
