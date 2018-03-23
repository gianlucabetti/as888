000100110525      //--------------------------------------------------------------
000200110525      //?TRMK13R - STAMPA ANGRAFICA POTENZIALI
000300110525      //--------------------------------------------------------------
000400110725
000500110725      //--------------------------------------------------------------
000600110725      //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110725      //--------------------------------------------------------------
000800110725
000900110725     /*PRM dbgview(*source)
001000110725     /*END
001100110725
001200110725      //--------------------------------------------------------------
001300110725      //?Specifiche di controllo.                                     ?
001400110725      //--------------------------------------------------------------
001500110525
001600110525     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001700110525     h dftactgrp(*no) actgrp(*caller)
001800110525
001900110525      //--------------------------------------------------------------
002000110526      //?Dichiarazione file.
002100110525      //---------------------------------------------------------------
002200110525
002300110525      // - Organigramma filiale/aziendale
002400110525     fAZORG01L  if   e           k disk
002500130916
002600130916      // - Anagrafica Commerciali
002700130916     fAZCMM01L  if   e           k disk
002800110531
002900110531      // - Tabelle
003000110531     fTABEL00F  if   e           k disk
003100110531     fTNTBE01L  if   e           k disk
003200110531
003300110531      // - Attività
003400110531     fTIATC05L  if   e           k disk
003500110525
003600110525      // - Info Commerciali
003700110525     fTICPI01L  if   e           k disk    usropn
003800110531
003900110531      // - Trattative
004000110531     fTIVIS01L  if   e           k disk
004100110525
004200110525      // - Anagrafica Potenziale
004300110525     fTNCPO01L  if   e           k disk    usropn
004400110525     fTNCPO02L  if   e           k disk    usropn
004500110525     f                                     rename(TNCPO000:TNCPO02)
004600110525     fTNCPO03L  if   e           k disk    usropn
004700110525     f                                     rename(TNCPO000:TNCPO03)
004800110525
004900110525      // - Work File per la stampa
005000110526     fWFCPO00F  if   e           k disk    usropn
005100110525
005200110525      // - File di stampa
005300110525     fTRMK13P   o    e             printer oflind(*in99)
005400110525     f                                     infds(prtinfo)
005500110525
005600110525      //---------------------------------------------------------------
005700110525      //?Definizione costanti.
005800110525      //---------------------------------------------------------------
005900110525
006000110525
006100110525      //---------------------------------------------------------------
006200110525      //?Definizione schiere.
006300110525      //---------------------------------------------------------------
006400110606
006500110606      // - Agenti (appoggio)
006600110606     d $Age            s              7  0 dim(3500)
006700110606
006800110606      // - Agente Unificante prossima attività (figli)
006900110606     d $Cmm            s              7  0 dim(3500)
007000110906
007100110906      // - Agente Unificante (figli)
007200110906     d $Cmmu           s              7  0 dim(3500)
007300110525
007400110531      // - Tabella IFO
007500110531     d IFO             s              3    dim(50)
007600110531     d IFOds           s             70    dim(50)
007700110531     d IFOcg           s              2    dim(50)
007800110531     d IFOor           s              8    dim(50)
007900110525
008000110525      //---------------------------------------------------------------
008100110525      //?Definizione aree dati.
008200110525      //---------------------------------------------------------------
008300110525
008400110525      // - Dati utente
008500110525     d §AzUte        e ds                  extname(AZUTE00F)
008600110525     d                                     dtaara
008700110525     d §DatiUte      e ds                  extname(dDatiUte)
008800110525     d                                     dtaara
008900110525
009000110525      //---------------------------------------------------------------
009100110525      //?Definizione strutture dati.
009200110525      //---------------------------------------------------------------
009300110525
009400110525     d PrtINFO         ds
009500110525     d  CurLine              367    368i 0
009600110525
009700110525      // - Status
009800110525     d Psds           sds
009900110525     d   SDSpgm          *proc
010000110525
010100110525      // - Parametri ricevuti
010200110525     d KPJBA         e ds
010300110525     d TRMK13ds      e ds                  inz
010400110531     d  $cic                  68     72    dim(5)
010500110608     d  D13cic1               68     68
010600110608     d  D13cic2               69     69
010700110608     d  D13cic3               70     70
010800110608     d  D13cic4               71     71
010900110608     d  D13cic5               72     72
011000110531     d  $fls                  73     77    dim(5)
011100110608     d  D13fls1               73     73
011200110608     d  D13fls2               74     74
011300110608     d  D13fls3               75     75
011400110608     d  D13fls4               76     76
011500110608     d  D13fls5               77     77
011600110531     d  $cco                  88     97    dim(5)
011700110608     d  D13cco1               88     89
011800110608     d  D13cco2               90     91
011900110608     d  D13cco3               92     93
012000110608     d  D13cco4               94     95
012100110608     d  D13cco5               96     97
012200110531     d  $rip                 127    135    dim(3)
012300110608     d  D13rip1              127    129
012400110608     d  D13rip2              130    132
012500110608     d  D13rip3              133    135
012600110531     d  $conc                173    192    dim(5)
012700110608     d  D13conc1             173    176
012800110608     d  D13conc2             177    180
012900110608     d  D13conc3             181    184
013000110608     d  D13conc4             185    188
013100110608     d  D13conc5             189    192
013200110531     d  $moi                 199    206    dim(2)
013300110608     d  D13moi1              199    202
013400110608     d  D13moi2              203    206
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
014500110526
014600110526      // - Carico Filiali abilitate all'utente
014700110525     d TRUL31DS      e ds
014800110525     d POG                    10    759    DIM(250)
014900110531
015000110531      // - Ds x campo CPORST
015100110531     d dCPO01        e ds                  inz
015200110601
015300110601      // - Tabella CCO = Causali Attività
015400110601     d dCCO          e ds                  inz
015500110531
015600110531      // - Tabella IFO = Info Commerciali
015700110531     d dIFO          e ds                  inz
015800110606
015900110606      // - Tabella IFS = Sottoconto Info Commerciali
016000110606     d dIFS          e ds                  inz
016100110531
016200110531      // - Tabella 05 = Area
016300110531     d ds05          e ds                  inz
016400110531
016500110531      // - Tabella 1L = Categoria Merceologica
016600110531     d ds1L          e ds                  inz
016700110607
016800110607      // - File WFCPO
016900110607     d WFCPOds       e ds                  extname(WFCPO00F)
017000110725     d                                     inz
017100110525
017200110525      //---------------------------------------------------------------
017300110525      //?Definizione variabili globali.
017400110525      //---------------------------------------------------------------
017500110531
017600110531      // - Indici di schiera
017700110531     d xx              s              4  0 inz
017800110607
017900110607       // - Stringa SQL da eseguire
018000110607     d StringaSql      s           2048    Varying        inz
018100110525
018200110526      // - Flags booleani
018300110607     d wInfo           s               n   inz(*off)
018400110607     d $End            s               n   inz(*off)
018500110606     d $EndAge         s               n   inz(*off)
018600110531     d $EndAtc         s               n   inz(*off)
018700110606     d $EndAtt         s               n   inz(*off)
018800110531     d $EndVis         s               n   inz(*off)
018900110607     d $InfoAbi        s               n   inz(*off)
019000110607     d $InfoNoOk       s               n   inz(*off)
019100110526     d $OrdCar         s               n   inz(*off)
019200110526     d $OrdCmm         s               n   inz(*off)
019300110526     d $OrdCmmu        s               n   inz(*off)
019400110526     d $OrdRag         s               n   inz(*off)
019500110526     d $OrdSct         s               n   inz(*off)
019600110526     d $OrdSpt         s               n   inz(*off)
019700110531     d $RcdOk          s               n   inz(*off)
019800110526
019900110526      // - Campi di comodo
020000110531     d Nrpag           s              4  0
020100110606     d sav_Car         s                   like(WFCare)
020200110606     d sav_Cmm         s                   like(WFCcmm)
020300110606     d sav_Cmmu        s                   like(WFCcmmu)
020400110606     d sav_CPItpf      s                   like(CPItpf)
020500110607     d sav_Flt         s                   like(WFCflt)
020600110606     d sav_Sct         s                   like(CPOsct)
020700110606     d Stringa_Info    s            164    inz
020800110906     d wAge            s              7  0 inz
020900110606     d wAtt            s              4  0 inz
021000110606     d wPog            s                   like(O31pog)
021100110606     d wPos            s              3  0 inz
021200110606     d wTtr            s              4  0 inz
021300110606     d w0030           s              3  0 inz
021400110531
021500110531      // - Campi di comodo data
021600110606     d wDate           s              8  0
021700110531     d wDate_EUR       s               d   datfmt(*eur)
021800110531     d wDate_ISO       s               d   datfmt(*iso)
021900110525
022000110525      //---------------------------------------------------------------
022100110525      //?Definizione procedure usate.
022200110525      //---------------------------------------------------------------
022300110525
022400110526      // - Elaborazione file di work
022500110526     d trmk13r1        pr                  extpgm('TRMK13R1')
022600110606     d  kpjba                              likeds(kpjba)
022700110606     d  wPog                        750
022800110607     d  $InfoAbi                       n
022900110525
023000110525      //---------------------------------------------------------------
023100110525      //?Prototipi.
023200110525      //---------------------------------------------------------------
023300110531      /copy gaitrasrc/srcprotopr,tibs02r
023400110525      /copy gaitrasrc/srcprotopr,tibs34r
023500110526      /copy gaitrasrc/srcprotopr,tntaa1c
023600110606      /copy gaitrasrc/srcprotopr,trul31r
023700110525
023800110525      //---------------------------------------------------------------
023900110525      //?Definizione key-list.
024000110525      //---------------------------------------------------------------
024100110525
024200110531       // - File TABEL00F
024300110531     d k03tabel      e ds                  extname(TABEL00F:*key)
024400110531     d                                     prefix(k_)
024500110531     d                                     inz
024600110525
024700110601      //---------------------------------------------------------------
024800110601      //?Riepilogo indicatori.
024900110601      //---------------------------------------------------------------
025000110601
025100110601      // 10    : Stampa info commerciali
025200110620      // 11    : Rottura per Commerciale Unificante
025300110620      // 12    : Rottura per Commerciale Unificante prossima attività
025400110620      // 13    : Rottura per Area/Filiale
025500110620      // 14    : Rottura per Categoria Merceologica
025600110608      // 99    : Salto pagina per overflow
025700110601
025800110601      //---------------------------------------------------------------
025900110525      //---------------------------------------------------------------
026000110525      //?M A I N - L I N E
026100110525      //---------------------------------------------------------------
026200110525
026300110525     c     *Entry        plist
026400110525     c                   parm                    KPJBA
026500110525
026600110525      /free
026700110525
026800110525       //?Operazioni iniziali
026900110525       exsr RoutInz;
027000110531
027100110531       //?Stampo la pagina dei parametri di lancio
027200110531       exsr StpPar;
027300110525
027400110531       //?Elaboro
027500110531       exsr Elabora;
027600110607
027700110607       //?Stampo il totale dei clienti
027800110607       IF  *in10;
027900110607         write MK13TOT;
028000110607       ENDIF;
028100110531
028200110525       //?Stampo la fine
028300110526       write MK13FINE;
028400110525
028500110525       //?Operazioni finali
028600110525       exsr RoutEnd;
028700110525
028800110525       //--------------------------------------------------------------
028900110525       //?Operazioni iniziali.
029000110525       //--------------------------------------------------------------
029100110525       BEGSR RoutInz;
029200110526
029300110531       //?Imposto la ds di passaggio parametri
029400110526         TRMK13DS = kpjbu;
029500110526
029600110526         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029700110525
029800110531       //?Reperimento dati job
029900110525         exsr DatiJob;
030000110607
030100110607       //?Imposto gli ordinamenti
030200110607         $OrdSpt  = (D13ospt  <> *blanks);
030300110607         $OrdCmmu = (D13ocmmu <> *blanks);
030400110607         $OrdCmm  = (D13ocmm  <> *blanks);
030500110607         $OrdCar  = (D13ocar  <> *blanks);
030600110607         $OrdRag  = (D13orag  <> *blanks);
030700110607         $OrdSct  = (D13osct  <> *blanks);
030800110608
030900110608       //?Imposto i salti pagina
031000110620         *in11 = (D13ocmmu <> *blanks);
031100110620         *in12 = (D13ocmm  <> *blanks);
031200110620         *in13 = (D13ocar  <> *blanks);
031300110620         *in14 = (D13osct  <> *blanks);
031400110525
031500110607       //?Creo file di WORK x ordinamenti richiesti
031600110607         IF  not $OrdRag and not $OrdSct;
031700110606         //?Pgm che scrive il file di work
031800110526           kpjbu = TRMK13DS;
031900110606           trmk13r1 (kpjba:O31pog:$InfoAbi);
032000110531         //?Apro il file di work
032100110607           open WFCPO00F;
032200110531         //?Apro il file dei potenziali
032300110526           open TNCPO01L;
032400110531         ENDIF;
032500110526
032600110607       //?Apro il file TNCPO che devo leggere
032700110607         IF  $OrdRag;
032800110606           open TNCPO02L;
032900110526         ENDIF;
033000110607         IF  $OrdSct;
033100110606           open TNCPO03L;
033200110526         ENDIF;
033300110607
033400110607       //?INFO COMMERCIALI apro il file
033500110607         open TICPI01L;
033600110531
033700110531       //?Carico le INFO
033800110607         IF  $InfoAbi;
033900110607           clear xx;
034000110607           setll ('IFO') TNTBE01L;
034100110607           reade ('IFO') TNTBE01L;
034200110607           DOW  not %eof(TNTBE01L);
034300110607             IF  TBEatb = *blanks;
034400110607               dIFO = TBEuni;
034500110607               xx += 1;
034600110607               IFO(xx) = TBEke1;
034700110607               IFOds(xx) = dIFO;
034800110607               IFOcg(xx) = §IFOifg;
034900110607             //?Ordinamento per categoria e num. ordinamento
035000110607               IFOor(xx) = §IFOifg + %editc(§IFOogi:'X') +
035100110607                           TBEke1;
035200110607             ENDIF;
035300110607             reade ('IFO') TNTBE01L;
035400110607           ENDDO;
035500110607         ENDIF;
035600110906
035700110906       //?Se richiesto Commerciale Unificante
035800110906       //?carico la sk con i figli
035900110906         IF  D13cmmu <> 0;
036000110906           wAge = D13cmmu;
036100110906           exsr CarAge;
036200110906           $Cmmu = $Age;
036300110906         ENDIF;
036400110606
036500110606       //?Se richiesto Commerciale Unificante prossima attività
036600110606       //?carico la sk con i figli
036700110606         IF  D13cmm <> 0;
036800110906           wAge = D13cmm;
036900110606           exsr CarAge;
037000110606           $Cmm = $Age;
037100110606         ENDIF;
037200110525
037300110531       //?Imposto nome programma
037400110525         NomePgm = SDSpgm;
037500110525
037600110531       //?Imposto data e ora elaborazione
037700110525         DataEla = %dec(%date():*eur);
037800110525         OraEla  = %dec(%time());
037900110606
038000110606       ENDSR;
038100110601
038200110601       //--------------------------------------------------------------
038300110601       //?Reperimento Dati del job (Utente/Operativi).
038400110601       //--------------------------------------------------------------
038500110601       BEGSR DatiJob;
038600110601
038700110601         in(E) §AzUte;
038800110601         IF  NOT %error;
038900110601           in(E) §DatiUte;
039000110601         ENDIF;
039100110601         IF  %error or RSut = *blanks;
039200110601           clear TIBS34ds;
039300110601           tibs34r(tibs34ds);
039400110601           in §AzUte;
039500110601           in §DatiUte;
039600110601         ENDIF;
039700110601
039800110601       //?Controllo se utente abilitato alla stampa INFO COMMERCIALI
039900110601         dUTE01 = UTEfaf;
040000110607         $InfoAbi = (§UTEpotst = 'S');
040100110607         *in10 = $InfoAbi;
040200110601
040300110601       //?Richiamo pgm di controllo autorizzazioni utente così mi torna
040400110601       //?il codice autorizzazione relativo alla gestione potenziali
040500110601       //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
040600110601         reset TNTAA1DS;
040700110601         ITAA1caut = '§utepot';
040800110601         kpjbu     = TNTAA1DS;
040900110601         tntaa1c (kpjba);
041000110601         TNTAA1DS = kpjbu;
041100110601
041200110601       //?Carico filiali abilitate all'utente
041300110601         clear TRUL31DS;
041400110601         I31abi = OTAA1cabi;
041500110601         I31cdi = DUTdis;
041600110601         I31car = DUTare;
041700110601         I31cpo = DUTpou;
041800110606         trul31r (kpjba:trul31ds);
041900110606         wPog = O31pog;
042000110601
042100110601       ENDSR;
042200110606
042300110606       //--------------------------------------------------------------
042400110606       //?Carico Sk Agenti.
042500110606       //--------------------------------------------------------------
042600110606       BEGSR CarAGE;
042700110606
042800110606         clear xx;
042900110606         clear $Age;
043000110606         $EndAge = *off;
043100110606
043200110606         exec sql
043300110606          declare AGE cursor for
043400130916          select CMMcod, CMMuni
043500130916          from AZCMM00F
043600130916          order by CMMcod;
043700110606
043800110606         exec sql
043900110606           open AGE;
044000110606           IF sqlcode < 0;
044100110606             $EndAge = *on;
044200110606           ENDIF;
044300110606
044400110606         DOW not $EndAge;
044500110606           exec sql
044600130916             fetch next from AGE into :CMMcod, :CMMuni;
044700110606             IF sqlcod = 100 or sqlcod < 0;
044800110606               $EndAge = *on;
044900110606               leave;
045000110606             ENDIF;
045100130916             IF  CMMuni = wAge;
045200110606               xx += 1;
045300130916               $Age(xx) = CMMcod;
045400110606             ENDIF;
045500110606         ENDDO;
045600110606
045700110606         exec sql close AGE;
045800110606
045900110606       //?Se non ho cricato niente memorizzo almeno il codice richiesto
046000110606         IF $Age(01) = 0;
046100110906           $Age(01) = wAge;
046200110606         ENDIF;
046300110606
046400110606       ENDSR;
046500110531
046600110531       //--------------------------------------------------------------
046700110531       //?Stampa pagina con i parametri di lancio.
046800110531       //--------------------------------------------------------------
046900110531       BEGSR StpPar;
047000110531
047100110531         clear NrPag;
047200110531
047300110531       //?Decodifico il commerciale unificante
047400130916         clear  DesCmmU;
047500110531         IF  D13cmmu > 0;
047600130916           chain  (D13cmmU)  AZCMM000;
047700130916           IF  %found(AZCMM01L);
047800130916             descmmu = CMMdes;
047900110531           ENDIF;
048000110531         ENDIF;
048100110531
048200110531       //?Decodifico l'area
048300110531         IF  D13car > 0;
048400110531           clear ds05;
048500110531           k_TBLkut = 1;
048600110531           k_TBLcod = '05';
048700110531           k_TBLkey = %editc(D13car:'X');
048800110531           chain %kds(K03tabel) TABEL00F;
048900110531           IF  %found(TABEL00F) and TBLflg = *blanks;
049000110531             ds05 = TBLuni;
049100110531           ENDIF;
049200110531           descar = §05des;
049300110531         ENDIF;
049400110531
049500110531       //?Decodifico la filiale
049600110531         IF  D13fil > 0;
049700110531           clear desfil;
049800110531           chain (D13fil) AZORG01L;
049900110531           IF  %found(AZORG01L) and ORGfva = *blanks;
050000110531             desfil = ORGdes;
050100110531           ENDIF;
050200110531         ENDIF;
050300110531
050400110531       //?Categoria merceologica
050500110531         IF  D13sct > 0;
050600110531           clear ds1L;
050700110531           k_TBLkut = 1;
050800110531           k_TBLcod = '1L';
050900110531           k_TBLkey = %editc(D13sct:'X');
051000110531           chain %kds(K03tabel) TABEL00F;
051100110531           IF  %found(TABEL00F) and TBLflg = *blanks;
051200110531             ds1L = TBLuni;
051300110531           ENDIF;
051400110531           dessct = §1Ldes;
051500110531         ENDIF;
051600110531
051700110531       //?Contattati
051800110531         SELECT;
051900110531           WHEN  D13pst = *blanks;
052000110531             despst = 'Tutti';
052100110531           WHEN  D13pst = 'N';
052200110531             despst = 'Mai Contattati';
052300110531           WHEN  D13pst = 'S';
052400110531             despst = 'Già Contattati';
052500110531         ENDSL;
052600110531
052700110531       //?Sede/Filiale
052800110531         SELECT;
052900110531           WHEN  D13fsf = 'S';
053000110531             desfsf = 'Sede';
053100110531           WHEN  D13fsf = 'F';
053200110531             desfsf = 'Filiale';
053300110531         ENDSL;
053400110531
053500110531       //?Con Trattative in corso
053600110531         SELECT;
053700110531           WHEN  D13ttr = *blanks;
053800110531             desttr = 'Tutti';
053900110531           WHEN  D13ttr = 'S';
054000110531             despst = 'Sì';
054100110531           WHEN  D13ttr = 'N';
054200110531             desttr = 'No';
054300110531         ENDSL;
054400110531
054500110531       //?Decodifico il commerciale unificante prossima attività
054600130916         clear  DesCmm;
054700110531         IF  D13cmm > 0;
054800130916           chain  (D13cmm)  AZCMM000;
054900130916           IF  %found(AZCMM01L);
055000130916             descmm = CMMdes;
055100110531           ENDIF;
055200110531         ENDIF;
055300110531
055400110531       //?Date 'Attività da effettuare'
055500110531         IF  D13dadda > 0;
055600110531           wDate_ISO = %date(D13dadda:*iso);
055700110531           wDate_EUR = wDate_ISO;
055800110531           stpdadda  = %dec(wDate_EUR);
055900110531           wDate_ISO = %date(D13dada:*iso);
056000110531           wDate_EUR = wDate_ISO;
056100110531           stpdada   = %dec(wDate_EUR);
056200110531         ENDIF;
056300110531
056400110531       //?Ripartizione spesa generale
056500110531         SELECT;
056600110531           WHEN  D13ripg = 'I';
056700110531             desripg = 'ITALIA';
056800110531           WHEN  D13ripg = 'E';
056900110531             desripg = 'ESTERO';
057000110531         ENDSL;
057100110531
057200110531       //?Motivo di non affidamento
057300110531         IF  $moi(01) <> *blanks;
057400110531           clear desmoi1;
057500110531           clear TIBS02ds;
057600110531           T02mod = 'C';
057700110531           T02cod = 'IFS';
057800110531           T02ke1 = 'MOI';
057900110531           T02ke2 = $moi(01);
058000110531           T02sif = KNSIF;
058100110531           TNTBE_RicercaControllo (kpjba : tibs02ds);
058200110531           IF  T02err = *blanks;
058300110531             desmoi1 = T02uni;
058400110531           ENDIF;
058500110531         ENDIF;
058600110531         IF  $moi(02) <> *blanks;
058700110531           clear desmoi2;
058800110531           clear TIBS02ds;
058900110531           T02mod = 'C';
059000110531           T02cod = 'IFS';
059100110531           T02ke1 = 'MOI';
059200110531           T02ke2 = $moi(02);
059300110531           T02sif = KNSIF;
059400110531           TNTBE_RicercaControllo (kpjba : tibs02ds);
059500110531           IF  T02err = *blanks;
059600110531             desmoi2 = T02uni;
059700110531           ENDIF;
059800110531         ENDIF;
059900110531
060000110531       //?Stampo la pagina dei parametri di lancio
060100110531         write MK13PAGINA;
060200110531         write MK13PAR;
060300110531
060400110531       ENDSR;
060500110531
060600110531       //--------------------------------------------------------------
060700110531       //?Elaborazione.
060800110531       //--------------------------------------------------------------
060900110531       BEGSR Elabora;
061000110531
061100110531         SELECT;
061200110531
061300110531       //?Stampo per ragione sociale
061400110531           WHEN  $OrdRag;
061500110531             exsr ReadCpo2;
061600110531
061700110531       //?Stampo per categoria merceologica
061800110531           WHEN  $OrdSct;
061900110531             exsr ReadCpo3;
062000110607
062100110607       //?negli altri casi leggo il file di WORK
062200110607           OTHER;
062300110607             exsr ReadWcpo;
062400110531
062500110531         ENDSL;
062600110531
062700110531       ENDSR;
062800110531
062900110531       //--------------------------------------------------------------
063000110531       //?Stampo per ragione sociale.
063100110531       //--------------------------------------------------------------
063200110531       BEGSR ReadCpo2;
063300110531
063400110531         read TNCPO02L;
063500110531         DOW  not %eof(TNCPO02L);
063600110531
063700110531         //?Controllo se il rcd letto è OK
063800110531           exsr CtrRcd;
063900110531           IF  $RcdOk;
064000110531             exsr Stampa;
064100110531           ENDIF;
064200110531
064300110531           read TNCPO02L;
064400110531
064500110531         ENDDO;
064600110531
064700110531       ENDSR;
064800110531
064900110531       //--------------------------------------------------------------
065000110531       //?Stampo per categoria merceologica.
065100110531       //--------------------------------------------------------------
065200110531       BEGSR ReadCpo3;
065300110531
065400110531       //?Se richiesta una categoria leggo solo quella
065500110531         IF  D13sct > 0;
065600110531           setll (D13sct) TNCPO03L;
065700110531           reade (D13sct) TNCPO03L;
065800110531       //?Se non richiesta una categoria leggo tutto
065900110531         ELSE;
066000110531           read TNCPO03L;
066100110531         ENDIF;
066200110531         DOW  not %eof(TNCPO03L);
066300110531
066400110531         //?Controllo se il rcd letto è OK
066500110531           exsr CtrRcd;
066600110531           IF  $RcdOk;
066700110531             exsr Stampa;
066800110531           ENDIF;
066900110531
067000110531           IF  D13sct > 0;
067100110531             reade (D13sct) TNCPO03L;
067200110531           ELSE;
067300110531             read TNCPO03L;
067400110531           ENDIF;
067500110531
067600110531         ENDDO;
067700110531
067800110531       ENDSR;
067900110607
068000110607       //--------------------------------------------------------------
068100110607       //?Leggo il file di WORK.
068200110607       //--------------------------------------------------------------
068300110607       BEGSR ReadWcpo;
068400110607
068500110607         $End = *off;
068600110607         StringaSql = 'select * from WFCPO00F ';
068700110607
068800110607       //?Ordino il file in base alle richieste
068900110607         SELECT;
069000110607           WHEN  $OrdSpt;
069100110607             StringaSql += ' order by WFCspt desc, WFCrag';
069200110607           WHEN  $OrdCmmu;
069300110607             StringaSql += ' order by WFCcmmu, WFCrag';
069400110607           WHEN  $OrdCmm;
069500110607             StringaSql += ' order by WFCcmm, WFCrag';
069600110607           WHEN  $OrdCar;
069700110607             StringaSql += ' order by WFCare, WFCflt, WFCrag';
069800110607         ENDSL;
069900110607
070000110607       //?Dichiarazione cursore
070100110607         exec sql
070200110607           prepare A1 from :StringaSql;
070300110607         exec sql
070400110607           declare WCPO cursor for A1;
070500110607
070600110607       //?Apertura del cursore
070700110607         exec sql
070800110607           open WCPO;
070900110607
071000110607         IF  sqlcode < 0;
071100110607           $End = *on;
071200110607         ENDIF;
071300110607
071400110607       //?Leggo file di work
071500110607         DOW  not $End;
071600110607           exec sql
071700110607             fetch next from WCPO into :WFCPOds;
071800110607           IF  sqlcod = 100 or sqlcod < 0;
071900110607             $End = *on;
072000110607             leave;
072100110607           ENDIF;
072200110607
072300110607         //?Aggancio TNCPO
072400110607           chain (WFCcpo) TNCPO01L;
072500110607           IF  %found(TNCPO01L);
072600110607           //?Controllo se il rcd letto è OK
072700110607             exsr CtrRcd;
072800110607             IF  $RcdOk;
072900110607               exsr Stampa;
073000110607             ENDIF;
073100110607           ENDIF;
073200110607
073300110607         ENDDO;
073400110607
073500110607         exec sql
073600110607           close WCPO;
073700110607
073800110607       ENDSR;
073900110531
074000110531       //--------------------------------------------------------------
074100110531       //?Controllo se RECORD OK.
074200110531       //--------------------------------------------------------------
074300110531       BEGSR CtrRcd;
074400110531
074500110531         $EndAtc = *off;
074600110531         $EndVis = *off;
074700110531         $RcdOk = *off;
074800110531         dCPO01 = CPOrst;
074900110906
075000120131       // se presente il commerciale unificante ed il commerciale del potenziale a zero esco
075100120131         IF  D13cmmu > 0 and CPOcmm = 0 ;
075200120131           leavesr ;
075300120131         Endif ;
075400120131
075500110906       //?Controllo se potenziale in carico al commerciale unificante
075600110906         IF  D13cmmu > 0 and %lookup(CPOcmm:$Cmmu) = 0;
075700110906           leavesr;
075800110906         ENDIF;
075900110531
076000110531       //?Filiale potenziale OK su Organigramma
076100110531         chain CPOflt AZORG01L;
076200110531         IF  not %found(AZORG01L);
076300110531           leavesr;
076400110531         ENDIF;
076500110531         IF  ORGfva <> *blanks;
076600110531           leavesr;
076700110531         ENDIF;
076800110531
076900110531       //?Controllo se stessa provincia
077000110531         IF  D13prv <> *blanks and CPOprv <> D13prv;
077100110531           leavesr;
077200110531         ENDIF;
077300110531
077400110531       //?Controllo se stessa località
077500110531         IF  D13loc <> *blanks;
077600110531           wPos = %scan('  ':D13loc);
077700110531           wPos -= 1;
077800120606           IF  %subst(D13loc:1:wPos) <> %subst(CPOcit:1:wPos);
077900110531             leavesr;
078000110531           ENDIF;
078100110531         ENDIF;
078200110531
078300110531       //?Controllo se stesso cap
078400110531         IF  D13cap <> *blanks and (CPOcap <> D13cap or
078500110531                                    CPOnaz <> D13naz);
078600110531           leavesr;
078700110531         ENDIF;
078800110531
078900110531       //?Controllo se stessa nazione
079000110531         IF  D13naz <> *blanks and CPOnaz <> D13naz;
079100110531           leavesr;
079200110531         ENDIF;
079300110531       //?Controllo se stessa area
079400110531         IF  D13car > *zeros and ORGcar <> D13car;
079500110531           leavesr;
079600110531         ENDIF;
079700110531
079800110531       //?Controllo se stessa filiale
079900110531         IF  D13fil <> 0 and CPOflt <> D13fil;
080000110531           leavesr;
080100110531         ENDIF;
080200110531
080300110531       //?Controllo se stessa categoria merceologica
080400110531         IF  D13sct <> 0 and CPOsct <> D13sct;
080500110531           leavesr;
080600110531         ENDIF;
080700110531
080800110531       //?Controllo se stesso codice importanza
080900110531         IF  D13cic <> *blanks;
081000110531           IF  CPOftr = *blanks;
081100110531             leavesr;
081200110531           ENDIF;
081300110531           IF  %lookup(CPOftr:$cic) = 0;
081400110531             leavesr;
081500110531           ENDIF;
081600110531         ENDIF;
081700110531
081800110531       //?Controllo se stessa categoria potenziale
081900110531         IF  D13fls <> *blanks;
082000110531           IF  CPOfls = *blanks;
082100110531             leavesr;
082200110531           ENDIF;
082300110531           IF  %lookup(CPOfls:$fls) = 0;
082400110531             leavesr;
082500110531           ENDIF;
082600110531         ENDIF;
082700110531
082800110531       //?Controllo se stessa area utente
082900110531         IF  %lookup(%editc(CPOflt:'X'):POG) = 0;
083000110531           leavesr;
083100110531         ENDIF;
083200110531
083300110531       //?Contattati
083400110531         IF  D13pst <> *blanks;
083500120914           IF  (D13pst = 'N' and §CPOdtpcon <> *blanks and
083600120914                                 §CPOdtpcon <> *zeros) or
083700120914               (D13pst = 'S' and (§CPOdtpcon = *blanks or
083800120914                                  §CPOdtpcon = *zeros));
083900110531             leavesr;
084000110531           ENDIF;
084100110531         ENDIF;
084200110531
084300110531       //?Controllo se sede/filiale
084400110531         IF  D13fsf <> *blanks and CPOfsf <> D13fsf;
084500110531           leavesr;
084600110531         ENDIF;
084700110531
084800110608       //?Controllo trattative in corso
084900110607         wTtr = 0;
085000110607         setll (CPOcpo) TIVIS01L;
085100110607         DOW  not $EndVis;
085200110607           reade (CPOcpo) TIVIS01L;
085300110607           IF  %eof(TIVIS01L);
085400110607             $EndVis = *on;
085500110607             leave;
085600110607           ENDIF;
085700110607           IF  VISffz = *blanks and VISdch = 0;
085800110607             wTtr += 1;
085900110607             $EndVis = *on;
086000110607           ENDIF;
086100110607         ENDDO;
086200110607       //?Controllo trattative in corso
086300110607         IF  D13ttr <> *blanks;
086400110607           IF (D13ttr = 'S' and wTtr = 0) or
086500110607              (D13ttr = 'N' and wTtr > 0);
086600110607             leavesr;
086700110607           ENDIF;
086800110531         ENDIF;
086900110531
087000110531       //?Controllo prossima attività
087100110531         IF  D13cmm > *zeros or D13cco <> *blanks or
087200110531             D13dadda > *zeros;
087300110606           clear wDate;
087400110606           clear wAtt;
087500110606           setll (CPOcpo:wDate) TIATC05L;
087600110608           DOW  not $EndAtc;
087700110606             reade (CPOcpo:wDate) TIATC05L;
087800110531             IF  %eof(TIATC05L);
087900110608               $EndAtc = *on;
088000110531               leave;
088100110531             ENDIF;
088200110608             IF  D13cmm > 0 and %lookup(ATCcmm:$Cmm) = 0;
088300110608               iter;
088400110608             ENDIF;
088500110608             IF  D13cco <> *blanks and %lookup(ATCcad:$cco) = 0;
088600110608               iter;
088700110608             ENDIF;
088800110608             IF  D13dadda <> *zeros and ATCdad < D13dadda;
088900110608               iter;
089000110608             ENDIF;
089100110608             IF  D13dada <> *zeros and ATCdad > D13dada;
089200110531               iter;
089300110531             ENDIF;
089400110531             wAtt += 1;
089500110531             $EndAtc = *on;
089600110531           ENDDO;
089700110608           IF  wAtt = 0;
089800110531             leavesr;
089900110531           ENDIF;
090000110531         ENDIF;
090100110531
090200110531       //?Controllo le parzializzazioni delle INFO
090300110531       //?se utente abilitato alla stampa
090400110607         IF  $InfoAbi;
090500110531           exsr  CtrInfo;
090600110607           IF  $InfoNoOk;
090700110531             leavesr;
090800110531           ENDIF;
090900110531         ENDIF;
091000110531
091100110531       //?Se arrivo qua il rcd è OK
091200110531         $RcdOk = *on;
091300110607
091400110607       //?Sommo i clienti stampati
091500110607         TotCli += 1;
091600110531
091700110531       //?Cerco Commerciale Unificante potenziale
091800130916         //IF  D13cmmu = 0;
091900130916         //  chain   (CPOcmm)  AZCMM000;
092000130916         //ENDIF;
092100110531
092200110531       ENDSR;
092300110531
092400110531       //--------------------------------------------------------------
092500110531       //?Controllo se INFO OK.
092600110531       //--------------------------------------------------------------
092700110531       BEGSR CtrInfo;
092800110531
092900110607         $InfoNoOk = *on;
093000110607         wInfo     = *off;
093100110607
093200110607       //?Info non presenti
093300110607         IF  D13nifo = 'S';
093400110607           setll (CPOcpo) TICPI01L;
093500110607         //?non trovo info OK ed esco
093600110607           IF  not %equal;
093700110607             $InfoNoOk = *off;
093800110607             leavesr;
093900110607           ENDIF;
094000110607         //?trovo info mi vanno bene se info da annullare o
094100110607         //?concorrenti senza %
094200110607           reade (CPOcpo) TICPI01L;
094300110607           DOW not %eof(TICPI01L);
094400110607           //?al primo concorrente con i dati ok esco
094500110607             IF  CPItpf = '10' and CPIval > 0;
094600110607               wInfo = *on;
094700110607               leavesr;
094800110607             ENDIF;
094900110607           //?alla prima info non da annullare esco
095000110607             xx = 1;
095100110607             xx = %lookup(CPItpf:IFO);
095200110607             IF  xx > 0;
095300110607               dIFO = IFOds(xx);
095400110607               IF  §IFOann <> 'S';
095500110607                 wInfo = *on;
095600110607                 leavesr;
095700110607               ENDIF;
095800110607             ENDIF;
095900110607             reade (CPOcpo) TICPI01L;
096000110607           ENDDO;
096100110607       //?Se ho trovato delle info buone esco
096200110607           IF  wInfo;
096300110607             leavesr;
096400110607           ENDIF;
096500110607         ENDIF;
096600110531
096700110531       //?Spesa trasporti
096800110531         IF  D13spta > 0 and (§CPOsptp < D13sptda or
096900110531                              §CPOsptp > D13spta);
097000110531           leavesr;
097100110531         ENDIF;
097200110531
097300110531       //?Data ultimo aggiornamento INFO
097400110606         IF  D13ifoal > 0 and §CPOifodul > *zeros and
097500110531             %dec(§CPOifodul:8:0) > D13ifoal;
097600110531           leavesr;
097700110531         ENDIF;
097800110607
097900110607         wInfo = *off;
098000110531
098100110531       //?Ripartizione spesa
098200110531         IF  D13ripg <> *blanks or D13rip <> *blanks;
098300110531           setll (CPOcpo) TICPI01L;
098400110607           reade (CPOcpo) TICPI01L;
098500110607           DOW  not %eof(TICPI01L);
098600110531         //?Ripartizione spesa generico (ITA/EST)
098700110606             IF  D13ripg <> *blanks;
098800110606               xx = 1;
098900110606               xx = %lookup(CPItpf:IFO);
099000110607               IF  xx > 0 and IFOcg (xx) = '3T';
099100110607                 dIFO = IFOds(xx);
099200110607                 IF  §IFOtip = D13ripg;
099300110607                   wInfo = *on;
099400110607                   leave;
099500110607                 ENDIF;
099600110606               ENDIF;
099700110606             ENDIF;
099800110531         //?Ripartizione spesa
099900110606             IF  D13rip <> *blanks and %lookup(CPItpf:$rip) > 0;
100000110607               wInfo = *on;
100100110607               leave;
100200110606             ENDIF;
100300110607             reade (CPOcpo) TICPI01L;
100400110606           ENDDO;
100500110607         //?Controllo la % di ripartizione
100600110607           IF  wInfo and D13ripa > 0 and
100700110607              (CPIval < D13ripda or CPIval > D13ripa);
100800110607             wInfo = *off;
100900110607           ENDIF;
101000110607        //?Se non ho trovato delle info buone esco
101100110607           IF  not wInfo;
101200110607             leavesr;
101300110607           ENDIF;
101400110606         ENDIF;
101500110531
101600110607         wInfo = *off;
101700110531
101800110531       //?Spedizioni a Privati
101900110531         IF  D13pri = 'S';
102000110531         chain (CPOcpo:'PRI') TICPI01L;
102100110531           IF  not %found(TICPI01L);
102200110531             leavesr;
102300110531           ENDIF;
102400110531         ENDIF;
102500110531
102600110531       //?Spedizioni a GDO
102700110531         IF  D13gdo = 'S';
102800110531         chain (CPOcpo:'GDO') TICPI01L;
102900110531           IF  not %found(TICPI01L);
103000110531             leavesr;
103100110531           ENDIF;
103200110531         ENDIF;
103300110531
103400110531       //?Spedizioni e-commerce
103500110531         IF  D13ecm = 'S';
103600110531         chain (CPOcpo:'ECM') TICPI01L;
103700110531           IF  not %found(TICPI01L);
103800110531             leavesr;
103900110531           ENDIF;
104000110531         ENDIF;
104100110531
104200110531       //?Spedizioni 10:30/Priority
104300110531         IF  D13hpr = 'S';
104400110531         chain (CPOcpo:'HPR') TICPI01L;
104500110531           IF  not %found(TICPI01L);
104600110531             leavesr;
104700110531           ENDIF;
104800110531         ENDIF;
104900110601
105000110601       //?Spedizioni SECURE BOX
105100110601         IF  D13sbx = 'S';
105200110601         chain (CPOcpo:'SBX') TICPI01L;
105300110601           IF  not %found(TICPI01L) or
105400110601              (%found(TICPI01L) and CPIfsn <> 'S');
105500110601             leavesr;
105600110601           ENDIF;
105700110601         ENDIF;
105800110531
105900110531       //?Peso medio
106000110531         IF  D13pesa > 0;
106100110531         chain (CPOcpo:'KMS') TICPI01L;
106200110531           IF  not %found(TICPI01L) or
106300110531              (CPIval < D13pesda or CPIval > D13pesa);
106400110531             leavesr;
106500110531           ENDIF;
106600110531         ENDIF;
106700110531
106800110531       //?Peso specifico
106900110531         IF  D13pspa > 0;
107000110531         chain (CPOcpo:'KPM') TICPI01L;
107100110531           IF  not %found(TICPI01L) or
107200110531              (CPIval < D13pspda or CPIval > D13pspa);
107300110531             leavesr;
107400110531           ENDIF;
107500110531         ENDIF;
107600110531
107700110531       //?Affidato a BRT
107800110531         IF  D13bara > 0;
107900110531         chain (CPOcpo:'10':'_BAR') TICPI01L;
108000110531           IF  not %found(TICPI01L) or
108100110531              (CPIval < D13barda or CPIval > D13bara);
108200110531             leavesr;
108300110531           ENDIF;
108400110531         ENDIF;
108500110531
108600110531       //?Concorrenti
108700110531         IF  D13conc <> *blanks;
108800110531           setll (CPOcpo:'10') TICPI01L;
108900110607           reade (CPOcpo:'10') TICPI01L;
109000110607           DOW  not %eof(TICPI01L);
109100110531             IF  %lookup(CPIspf:$conc) > 0;
109200110607               wInfo = *on;
109300110607               leave;
109400110531             ENDIF;
109500110531             reade (CPOcpo:'10') TICPI01L;
109600110531           ENDDO;
109700110607         //?Controllo la %
109800110607           IF  wInfo and D13cona > 0 and
109900110607              (CPIval < D13conda or CPIval > D13cona);
110000110607             wInfo = *off;
110100110607             leavesr;
110200110607           ENDIF;
110300110607         //?Se non ho trovato delle info buone esco
110400110607           IF  not wInfo;
110500110607             leavesr;
110600110607           ENDIF;
110700110607         ENDIF;
110800110531
110900110607         wInfo = *off;
111000110531
111100110531       //?Motivo non affidamento
111200110531         IF  D13moi <> *blanks;
111300110531           setll (CPOcpo:'MOI') TICPI01L;
111400110607           reade (CPOcpo:'MOI') TICPI01L;
111500110607           DOW  not %eof(TICPI01L);
111600110531             IF  %lookup(CPIspf:$moi) > 0;
111700110607               wInfo = *on;
111800110607               leave;
111900110531             ENDIF;
112000110531             reade (CPOcpo:'MOI') TICPI01L;
112100110531           ENDDO;
112200110607           IF  not wInfo;
112300110531             leavesr;
112400110531           ENDIF;
112500110531         ENDIF;
112600120308
112700120308       //?Logistica interessante per noi
112800120308         IF  D13logi = 'S';
112900120308         chain (CPOcpo:'LOG') TICPI01L;
113000120308           IF  not %found(TICPI01L) or
113100120308               CPIfsn = 'N';
113200120308             leavesr;
113300120308           ENDIF;
113400120308         ENDIF;
113500110531
113600110531       //?Logistica in outsourcing
113700110531         IF  D13logo = 'S';
113800110531         chain (CPOcpo:'OUT') TICPI01L;
113900110531           IF  not %found(TICPI01L) or
114000110531               CPIfsn = 'N';
114100110531             leavesr;
114200110531           ENDIF;
114300110531         ENDIF;
114400110531
114500110531       //?Operatore Logistica
114600110531         IF  D13logp <> *blanks;
114700110531         chain (CPOcpo:'40':D13logp) TICPI01L;
114800110531           IF  not %found(TICPI01L);
114900110531             leavesr;
115000110531           ENDIF;
115100110531         ENDIF;
115200110531
115300110531       //?Se arrivo qua le INFO sono OK
115400110607         $InfoNoOk = *off;
115500110531
115600110531       ENDSR;
115700110531
115800110531       //--------------------------------------------------------------
115900110531       //?Stampo.
116000110531       //--------------------------------------------------------------
116100110531       BEGSR Stampa;
116200110531
116300110531       //?Salto pagina
116400110601         IF  NrPag = 0 or
116500110608            (*in11 and WFCcmmu <> sav_Cmmu) or
116600110608            (*in12 and WFCcmm <> sav_Cmm) or
116700110608            (*in13 and (WFCare <> sav_Car or
116800110607                                WFCflt <> sav_Flt)) or
116900110620            (*in14 and CPOsct <> sav_Sct) or
117000110601             Curline >= 63;
117100110601           exsr StpSalto;
117200110601           *in99 = *off;
117300110601           Nrpag += 1;
117400110620           IF  *in11 or *in12 or *in13 or *in14;
117500110607             sav_Cmmu = WFCcmmu;
117600110607             sav_Cmm  = WFCcmm;
117700110607             sav_Car  = WFCare;
117800110607             sav_Flt  = WFCflt;
117900110607             sav_SCT  = CPOsct;
118000110607           ENDIF;
118100110601         ENDIF;
118200110601
118300110601       //?Sede Filiale
118400110601         IF  CPOfsf = 'S';
118500110601           stpfsf = 'Se';
118600110601         ENDIF;
118700110601         IF  CPOfsf = 'F';
118800110601           stpfsf = 'Fi';
118900110601         ENDIF;
119000110601       //?Ragione Sociale
119100110601         stprag = CPOrag;
119200110601       //?Località
119300110601         stpcit = CPOcit;
119400110607       //?Trattativa in corso
119500110607         clear stpttr;
119600110607         IF wTtr > 0;
119700110607           stpttr = 'S';
119800110607         ENDIF;
119900110601       //?Prossima attività
120000110601         exsr ProAtt;
120100110601       //?Categoria merceologica
120200110601         clear ds1L;
120300110601         k_TBLkut = 1;
120400110601         k_TBLcod = '1L';
120500110601         k_TBLkey = %editc(CPOsct:'X');
120600110601         chain %kds(K03tabel) TABEL00F;
120700110601         IF  %found(TABEL00F) and TBLflg = *blanks;
120800110601           ds1L = TBLuni;
120900110601         ENDIF;
121000110601         stpdsct = §1Ldes;
121100110601
121200110601         //?Se abilitato
121300110610         clear stpdim;
121400110610         clear stpsptp;
121500110610         clear flgsptp;
121600110601         IF  *in10;
121700110601         //?Data ultimo aggiornamento info commerciali
121800110606           IF  §CPOifodul > *zeros;
121900110606             wDate = %dec(§CPOifodul:8:0);
122000110606             wDate_ISO = %date(wDate:*iso);
122100110606             wDate_EUR = wDate_ISO;
122200110606             stpdim    = %dec(wDate_EUR);
122300110606           ENDIF;
122400110601         //?Spesa trasporti
122500110601           chain (CPOcpo:'SPT') TICPI01L;
122600110601           IF  %found(TICPI01L);
122700110601             stpsptp = CPIpft;
122800110601             clear flgsptp;
122900110601           ELSE;
123000110601             stpsptp = §CPOsptp;
123100110601             flgsptp = 'P';
123200110601           ENDIF;
123300110601         ENDIF;
123400110601
123500110601         write MK13RIGA;
123600110601
123700110601         //?Info commerciali
123800110601         IF  *in10 and D13sifo = 'S';
123900110606           exsr Info_Comm;
124000110607           //?Stampo la riga di separazione
124100110607           wrkriga = *all'-';
124200110607           write MK13SEPARA;
124300110601         ENDIF;
124400110531
124500110531       ENDSR;
124600110601
124700110601       //--------------------------------------------------------------
124800110601       //?Stampo il salto pagina.
124900110601       //--------------------------------------------------------------
125000110606       BEGSR StpSalto;
125100110608
125200110620       //?Imposto le intestazioni delle rotture
125300110608         IF  *in11;
125400130916           clear  DesCmmU;
125500130916           chain  (WFCcmmu)  AZCMM000;
125600130916           IF  %found(AZCMM01L);
125700130916             descmmu = CMMdes;
125800110608           ENDIF;
125900110608         ENDIF;
126000110608         IF  *in12;
126100130916           clear  DesCmm;
126200130916           chain  (WFCcmm)  AZCMM000;
126300130916           IF  %found(AZCMM01L);
126400130916             descmm = CMMdes;
126500110608           ENDIF;
126600110608         ENDIF;
126700110608         IF  *in13;
126800110608           clear ds05;
126900110608           k_TBLkut = 1;
127000110608           k_TBLcod = '05';
127100110608           k_TBLkey = %editc(WFCare:'X');
127200110608           chain %kds(K03tabel) TABEL00F;
127300110608           IF  %found(TABEL00F) and TBLflg = *blanks;
127400110608             ds05 = TBLuni;
127500110608           ENDIF;
127600110608           descar = §05des;
127700110608           clear ORGdes;
127800110608           chain (WFCflt) AZORG01L;
127900110608           IF  %found(AZORG01L) and ORGfva = *blanks;
128000110608             desflt = ORGdes;
128100110608           ENDIF;
128200110608         ENDIF;
128300110620         IF  *in14;
128400110620           clear ds1L;
128500110620           k_TBLkut = 1;
128600110620           k_TBLcod = '1L';
128700110620           k_TBLkey = %editc(CPOsct:'X');
128800110620           chain %kds(K03tabel) TABEL00F;
128900110620           IF  %found(TABEL00F) and TBLflg = *blanks;
129000110620             ds1L = TBLuni;
129100110620           ENDIF;
129200110620           dessct = §1Ldes;
129300110620         ENDIF;
129400110620
129500110620         //?Stampo la testata
129600110620         IF  NrPag = 0 or
129700110620            (D13ocmmu = 'P' and WFCcmmu <> sav_Cmmu) or
129800110620            (D13ocmm  = 'P' and WFCcmm <> sav_Cmm) or
129900110620            (D13ocar  = 'P' and (WFCare <> sav_Car or
130000110620                                WFCflt <> sav_Flt)) or
130100110620            (D13osct  = 'P' and CPOsct <> sav_Sct) or
130200110620             Curline >= 63;
130300110601
130400110620           //?Stampo la testata
130500110620           write MK13PAGINA;
130600110620           write MK13TESTA;
130700110620           write MK13SEPARA;
130800110620           //?Stampo le intestazioni delle colonne
130900110620           write MK13INTCOL;
131000110620           wrkriga = *all'=';
131100110620           write MK13SEPARA;
131200110620         ENDIF;
131300110620
131400110620         //?Stampo le rotture
131500110620         IF  *in11 or *in12 or *in13 or *in14;
131600110620           clear wrkriga;
131700110620           write MK13SEPARA;
131800110620           write MK13SALTA;
131900110620           write MK13SEPARA;
132000110620         ENDIF;
132100110601
132200110601       ENDSR;
132300110601
132400110601       //--------------------------------------------------------------
132500110601       //?Prossima attività da eseguire.
132600110601       //--------------------------------------------------------------
132700110601       BEGSR ProAtt;
132800110610
132900110610         clear stpdad;
133000110610         clear stpcad;
133100110610         clear stpdcad;
133200110610         clear stpdcmm;
133300110601
133400110601       //?Cerco la prossima attività
133500110601         $EndAtt = *off;
133600110601         exec sql
133700110606         declare ATT cursor for select ATCcad, ATCdad, ATCcmm from TIATC00F
133800110601         where ATCcpo = :CPOcpo and atcdco = 0
133900110601         order by ATCdad, ATChda;
134000110601         exec sql
134100110601         open ATT;
134200110601         IF sqlcode < 0;
134300110601           $EndAtt = *on;
134400110601         ENDIF;
134500110601         DOW not $EndAtt;
134600110601           exec sql
134700110606           fetch next from ATT into :ATCcad, :ATCdad, :ATCcmm;
134800110601           IF sqlcod = 100 or sqlcod < 0;
134900110601             $EndAtt = *on;
135000110601             leave;
135100110601           ENDIF;
135200110606         //?Data prossima attività
135300110606           wDate_ISO = %date(ATCdad:*iso);
135400110606           wDate_EUR = wDate_ISO;
135500110606           stpdad    = %dec(wDate_EUR);
135600110606         //?Causale prossima attività
135700110606           stpcad    = ATCcad;
135800110606           clear TIBS02ds;
135900110606           T02mod = 'C';
136000110606           T02cod = 'CCO';
136100110606           T02ke1 = stpcad;
136200110606           T02sif = KNSIF;
136300110606           TNTBE_RicercaControllo (kpjba : tibs02ds);
136400110606           IF  T02err = *blanks;
136500110606             dcco = T02uni;
136600110606           ENDIF;
136700110606           stpdcad = §CCOdes;
136800110606         //?Commerciale prossima attività
136900130916           clear  StpDcmm;
137000130916           chain  (ATCcmm)  AZCMM000;
137100130916           IF  %found(AZCMM01L);
137200130916             stpdcmm = CMMdes;
137300110606           ENDIF;
137400110606           leave;
137500110606         ENDDO;
137600110606         exec sql close ATT;
137700110601
137800110601       ENDSR;
137900110601
138000110601       //--------------------------------------------------------------
138100110606       //?INFO Commerciali.
138200110601       //--------------------------------------------------------------
138300110606       BEGSR Info_Comm;
138400110606
138500110606         clear Stringa_Info;
138600110606         clear sav_CPItpf;
138700110606
138800110606       //?Leggo le INFO per stampare il Peso medio
138900110606         chain (CPOcpo:'KMS') TICPI01L;
139000110606         IF  %found(TICPI01L);
139100110606           exsr Componi_Info;
139200110606         ENDIF;
139300110606
139400110606       //?Legge le INFO per stampare i CONCORRENTI
139500110606         setll (CPOcpo:'10') TICPI01L;
139600110606         reade (CPOcpo:'10') TICPI01L;
139700110606         DOW  not %eof(TICPI01L);
139800110606         //?Compone la stringa da stampare
139900110606           exsr Componi_Info;
140000110606           reade (CPOcpo:'10') TICPI01L;
140100110606         ENDDO;
140200110606
140300110606       //?Stampo ultima riga concorrenti
140400110606         IF  Stringa_Info <> *blanks;
140500110606           exsr Stampa_Info;
140600110606         ENDIF;
140700110606
140800110606       //?Stampo le altre info solo se richieste
140900110606         clear Stringa_Info;
141000110606         clear sav_CPItpf;
141100110606
141200110606       //?Ripartizione spesa ITA/EST
141300110606         IF  D13ripg <> *blanks or D13rip <> *blanks;
141400110606           setll (CPOcpo) TICPI01L;
141500110606           reade (CPOcpo) TICPI01L;
141600110606           DOW  not %eof(TICPI01L);
141700110606         //?Generica
141800110606             IF  D13ripg <> *blanks;
141900110606               xx = 1;
142000110606               xx = %lookup(CPItpf:IFO);
142100110606               IF  xx > 0 and IFOcg(xx) = '3T';
142200110606                 dIFO = IFOds(xx);
142300110606                 IF  §IFOtip = D13ripg;
142400110606                   exsr Componi_Info;
142500110606                 ENDIF;
142600110606               ENDIF;
142700110606         //?Specifica
142800110606             ELSE;
142900110606               IF  %lookup(CPItpf:$rip) > 0;
143000110606                 exsr Componi_Info;
143100110606               ENDIF;
143200110606             ENDIF;
143300110606             reade (CPOcpo) TICPI01L;
143400110606           ENDDO;
143500110606         ENDIF;
143600110606
143700110606       //?Privati
143800110606         IF  D13pri = 'S';
143900110606           chain (CPOcpo:'PRI') TICPI01L;
144000110606           IF  %found(TICPI01L);
144100110606             exsr Componi_Info;
144200110606           ENDIF;
144300110606         ENDIF;
144400110606
144500110606       //?GDO
144600110606         IF  D13gdo = 'S';
144700110606           chain (CPOcpo:'GDO') TICPI01L;
144800110606           IF  %found(TICPI01L);
144900110606             exsr Componi_Info;
145000110606           ENDIF;
145100110606         ENDIF;
145200110606
145300110606       //?E-Commerce
145400110606         IF  D13ecm = 'S';
145500110606           chain (CPOcpo:'ECM') TICPI01L;
145600110606           IF  %found(TICPI01L);
145700110606             exsr Componi_Info;
145800110606           ENDIF;
145900110606         ENDIF;
146000110606
146100110606       //?H 10:30/Priority
146200110606         IF  D13hpr = 'S';
146300110606           chain (CPOcpo:'HPR') TICPI01L;
146400110606           IF  %found(TICPI01L);
146500110606             exsr Componi_Info;
146600110606           ENDIF;
146700110606         ENDIF;
146800110606
146900110606       //?Secure Box
147000110606         IF  D13sbx = 'S';
147100110606           chain (CPOcpo:'SBX') TICPI01L;
147200110606           IF  %found(TICPI01L);
147300110606             exsr Componi_Info;
147400110606           ENDIF;
147500110606         ENDIF;
147600110606
147700110606       //?Peso specifico
147800110606         IF  D13pspa > 0;
147900110606           chain (CPOcpo:'KPM') TICPI01L;
148000110606           IF  %found(TICPI01L);
148100110606             exsr Componi_Info;
148200110606           ENDIF;
148300110606         ENDIF;
148400110606
148500110606       //?Motivo non affidamento
148600110606         IF  D13moi <> *blanks;
148700110606           setll (CPOcpo:'MOI') TICPI01L;
148800110606           reade (CPOcpo:'MOI') TICPI01L;
148900110606           DOW  not %eof(TICPI01L);
149000110606             IF  %lookup(CPIspf:$moi) > 0;
149100110606               exsr Componi_Info;
149200110606             ENDIF;
149300110606             reade (CPOcpo:'MOI') TICPI01L;
149400110606           ENDDO;
149500110606         ENDIF;
149600110606
149700110606       //?Logistica interessante per noi
149800110606         IF  D13logi = 'S';
149900110606           chain (CPOcpo:'LOG') TICPI01L;
150000110606           IF  %found(TICPI01L);
150100110606             exsr Componi_Info;
150200110606           ENDIF;
150300110606         ENDIF;
150400110606
150500110606       //?Logistica in outsourcing
150600110606         IF  D13logo = 'S';
150700110606           chain (CPOcpo:'OUT') TICPI01L;
150800110606           IF  %found(TICPI01L);
150900110606             exsr Componi_Info;
151000110606           ENDIF;
151100110606         ENDIF;
151200110606
151300110606       //?Operatore logistica
151400110606         IF  D13logp <> *blanks;
151500110606           chain (CPOcpo:'40':D13logp) TICPI01L;
151600110606           IF  %found(TICPI01L);
151700110606             exsr Componi_Info;
151800110606           ENDIF;
151900110606         ENDIF;
152000110606
152100110606       //?Stampo ultima riga concorrenti
152200110606         IF  Stringa_Info <> *blanks;
152300110606           exsr Stampa_Info;
152400110606         ENDIF;
152500110606
152600110606       ENDSR;
152700110606
152800110606       //--------------------------------------------------------------
152900110606       //?Compone la Stringa INFO da stampare.
153000110606       //--------------------------------------------------------------
153100110606       BEGSR Componi_Info;
153200110606
153300110606       //?A cambio tipo INFO
153400110606         IF  CPItpf <> sav_CPItpf;
153500110606         //?Decodifica la INFO
153600110606           clear dIFO;
153700110606           clear TIBS02ds;
153800110606           T02mod = 'C';
153900110606           T02cod = 'IFO';
154000110606           T02ke1 = CPItpf;
154100110606           T02sif = KNSIF;
154200110606           TNTBE_RicercaControllo (kpjba : tibs02ds);
154300110606           IF  T02err = *blanks;
154400110606             dIFO = T02uni;
154500110606           ENDIF;
154600110606         //?Imposta la stringa
154700110606           IF  Stringa_Info <> *blanks;
154800110606             Stringa_Info = %trim(Stringa_info) + '   ' +
154900110606                            §IFOdeb;
155000110606           ELSE;
155100110606             Stringa_Info = §IFOdeb;
155200110606           ENDIF;
155300110606           sav_CPItpf = CPItpf;
155400110606         ENDIF;
155500110606
155600110606       //?Decodifica il sottotipo della INFO
155700110606         IF  CPIspf <> *blanks;
155800110606           clear dIFS;
155900110606           clear TIBS02ds;
156000110606           T02mod = 'C';
156100110606           T02cod = 'IFS';
156200110606           T02ke1 = CPItpf;
156300110606           T02ke2 = CPIspf;
156400110606           T02sif = KNSIF;
156500110606           TNTBE_RicercaControllo (kpjba : tibs02ds);
156600110606           IF  T02err = *blanks;
156700110606             dIFS = T02uni;
156800110606           ENDIF;
156900110606         //?Imposta la stringa
157000110606           IF  Stringa_Info <> *blanks;
157100110606             Stringa_Info = %trim(Stringa_info) + '   ' +
157200110606                            %subst(§IFSdes:1:20);
157300110606           ELSE;
157400110606             Stringa_Info = %subst(§IFSdes:1:20);
157500110606           ENDIF;
157600110606         ENDIF;
157700110606
157800110606       //?imposta % - Kg - S/N - spesa trasporti
157900110606         SELECT;
158000110606           WHEN  CPIval > 0 and CPItva = '%';
158100110606             w0030 = CPIval;
158200110606             Stringa_Info = %trim(Stringa_Info) + ' ' +
158300110606                            %editc(w0030:'Z') + CPItva;
158400110606           WHEN  CPIval > 0 and CPItva = 'KG';
158500110606             Stringa_Info = %trim(Stringa_Info) + ' ' +
158600110606                            %editc(CPIval:'Z') + CPItva;
158700110606           WHEN  CPIpft > 0;
158800110606             Stringa_Info = %trim(Stringa_Info) +
158900110606                            ' SpesaTRA ' +
159000110606                            %editc(CPIpft:'Z');
159100110606           WHEN  CPIsna > 0;
159200110606             Stringa_Info = %trim(Stringa_Info) +
159300110606                            ' Spediz. ' +
159400110606                            %editc(CPIsna:'Z');
159500110606           WHEN  CPIspf = *blanks and CPIfsn = 'S';
159600110606             Stringa_Info = %trim(Stringa_Info) +
159700110606                            ':Sì';
159800110606           WHEN  CPIspf = *blanks and CPIfsn = 'N';
159900110606             Stringa_Info = %trim(Stringa_Info) +
160000110606                            ':No';
160100110606         ENDSL;
160200110606
160300110606       //?Stampo le info
160400110606         clear wPos;
160500110606         wPos =  %checkr('  ':Stringa_Info);
160600110606         IF  wPos > 143;
160700110606           exsr Stampa_Info;
160800110606         ENDIF;
160900110606
161000110606       ENDSR;
161100110606
161200110606       //--------------------------------------------------------------
161300110606       //?Stampa INFO Commerciali.
161400110606       //--------------------------------------------------------------
161500110606       BEGSR Stampa_Info;
161600110606
161700110606       //?Salto pagina
161800110606         IF  Curline >= 63;
161900110606           exsr StpSalto;
162000110606           *in99 = *off;
162100110606         ENDIF;
162200110606
162300110606         infocom = Stringa_Info;
162400110606         write MK13INFO;
162500110606         clear Stringa_Info;
162600110606
162700110606       ENDSR;
162800110525
162900110525       //--------------------------------------------------------------
163000110525       //?Operazioni finali.
163100110525       //--------------------------------------------------------------
163200110525       BEGSR RoutEnd;
163300110525
163400110525         *inLR = *on;
163500110525         return;
163600110525
163700110525       ENDSR;
163800110525
163900110525      /end-free
