000100080206      //--------------------------------------------------------------
000200100721      //?TNTAI2R - INTERROGAZIONE CLIENTI - dettaglio
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100721
001200100721      // - Tabelle
001300100721     fTABEL00F  if   e           k disk
001400130802
001500130802       // -?Anagrafica commerciali
001600130802     fAZCMM01L  if   e           k disk
001700100721
001800110615      // - Note DC
001900110615     fTFNTC01L  if   e           k disk
002000110615
002100100721      // - Attività
002200100721     fTIATC04L  if   e           k disk
002300100721
002400100721      // - Note
002500100721     fTICPN02L  if   e           k disk
002600100625
002700100721      // - Anagrafica potenziale
002800100721     fTNCPO01L  if   e           k disk
002900090720
003000090716      // - Video Gestione trattative commerciali
003100100721     fTNTAI2D   cf   e             workstn
003200080208     f                                     indds(IndDspF)
003300080206     f                                     infds(InfDspF)
003400080206
003500080206      //---------------------------------------------------------------
003600090406      //?Definizione costanti.
003700080206      //---------------------------------------------------------------
003800080207
003900080207      // - Tasti funzionali a video
004000080207     d c_F01           c                   const(x'31')
004100080207     d c_F02           c                   const(x'32')
004200080207     d c_F03           c                   const(x'33')
004300090406     d c_F04           c                   const(x'34')
004400080207     d c_F05           c                   const(x'35')
004500080207     d c_F06           c                   const(x'36')
004600080207     d c_F07           c                   const(x'37')
004700080207     d c_F08           c                   const(x'38')
004800080207     d c_F09           c                   const(x'39')
004900080207     d c_F10           c                   const(x'3A')
005000090303     d c_F11           c                   const(x'3B')
005100080207     d c_F12           c                   const(x'3C')
005200080207     d c_F13           c                   const(x'B1')
005300080207     d c_F14           c                   const(x'B2')
005400080207     d c_F15           c                   const(x'B3')
005500080207     d c_F16           c                   const(x'B4')
005600080207     d c_F17           c                   const(x'B5')
005700080207     d c_F18           c                   const(x'B6')
005800080207     d c_F19           c                   const(x'B7')
005900080207     d c_F20           c                   const(x'B8')
006000080207     d c_F21           c                   const(x'B9')
006100080207     d c_F22           c                   const(x'BA')
006200080207     d c_F23           c                   const(x'BB')
006300080207     d c_F24           c                   const(x'BC')
006400080207     d c_Enter         c                   const(x'F1')
006500080207     d c_RollDown      c                   const(x'F4')
006600080207     d c_RollUp        c                   const(x'F5')
006700100721
006800100721      // - Scritta F24 sulla seconda riga di tasti (riga variabile)
006900100721     d cf2413          c                   const('F24=AlTasti(1/3)')
007000100721     d cf2423          c                   const('F24=AlTasti(2/3)')
007100100721     d cf2433          c                   const('F24=AlTasti(3/3)')
007200100721     d cf2412          c                   const('F24=AlTasti(1/2)')
007300100721     d cf2422          c                   const('F24=AlTasti(2/2)')
007400100211
007500100211     d Digits          c                   const('0123456789')
007600080206
007700080206      //---------------------------------------------------------------
007800080206      //?Definizione schiere.
007900080206      //---------------------------------------------------------------
008000080206
008100080206      // - Messaggi di errore
008200100408     d $Msg            s             78    dim(20) ctdata perrcd(1)
008300100721
008400100721      // - Tasti funzionali riga variabile
008500100722     d $Tfv            s             62    dim(20) ctdata perrcd(1)
008600080206
008700080206      //---------------------------------------------------------------
008800080206      //?Definizione aree dati.
008900080206      //---------------------------------------------------------------
009000080206
009100080206      // - Dati utente
009200080206     d §AzUte        e ds                  extname(AZUTE00F)
009300080206     d                                     dtaara
009400080206     d §DatiUte      e ds                  extname(dDatiUte)
009500080206     d                                     dtaara
009600080206
009700080206      //---------------------------------------------------------------
009800080206      //?Definizione strutture dati.
009900080206      //---------------------------------------------------------------
010000080206
010100080206      // - Status
010200080206     d Psds           sds
010300080206     d   SDSpgm          *proc
010400080206
010500080206      // - InfDS
010600080206     d InfDspF         ds
010700080207     d  dsp_aid              369    369a
010800080206
010900080206      // - Indicatori su DspF
011000080208     d IndDspF         ds
011100100624        // - Indicatori di errore in videata
011200100721     d  ErrMessage                    1n   overlay(IndDspF : 28)
011300100624        // - Indicatori di visualizzazione
011400150126     d  F04Attivo                     1n   overlay(IndDspF : 04)
011500100624     d  F05Pink                       1n   overlay(IndDspF : 40)
011600100624     d  F18Pink                       1n   overlay(IndDspF : 41)
011700110502        // - Indicatori di abilitazione
011800110502     d  F21Attivo                     1n   overlay(IndDspF : 21)
011900120201     d  F22Attivo                     1n   overlay(IndDspF : 22)
012000090422        // - Indicatori di errore generico
012100100721     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012200110517
012300090407     d WindDspF        ds                  inz
012400090407     d   WindDspFa             1     49    inz(*zero)
012500090407     d   WindDspFb            50     99    inz(*zero)
012600080206
012700080206      // - Parametri ricevuti
012800080206     d KPJBA         e ds
012900100721     d TNTAI2DS      e ds
013000100721
013100100721      // - Unificante
013200100721     d TIBS12ds      e ds
013300080206
013400080206      // - Reperimento dati utente
013500080206     d TIBS34ds      e ds
013600100624
013700100624       // - Reperimento dati anagrafici
013800100624     d TIBS69ds      e ds
013900100624     d DS_cnaco      e ds                  inz extname(CNACO00F)
014000100624     d DS_cnind      e ds                  inz extname(CNIND00F)
014100100624     d DS_cnclp      e ds                  inz extname(CNCLP00F)
014200100624     d DS_fncls      e ds                  inz extname(FNCLS00F)
014300100721
014400100721      // - Interrogazione Rubrica
014500100721     d TNTA12DS      e ds                  inz
014600100809
014700100809      // - Gestione trattative
014800100809     d TNTA88DS      e ds
014900100624
015000100624      // - Gestione attività
015100100624     d TRMK19DS      e ds
015200100722
015300100722      // - Interrogazione Potenziali
015400100721     d TRMK01DS      e ds                  extname(TRMK01DS)
015500100809
015600100809       // - Primo contatto
015700100809     d TRMK17DS      e ds
015800100722
015900100624      // - Interrogazione Note clienti
016000100624     d TRMK20DS      e ds                  inz
016100100624
016200100624      // - Interrogazione attività
016300100624     d TRMK21DS      e ds                  inz
016400100721
016500100721      // - Interrogazione Info Commerciali
016600100721     d TRMK50DS      e ds                  inz
016700110907
016800110907      // - Dati Fatturazione
016900110907     d TRUL57DS      e ds                  inz
017000110323
017100110323      // - Tabella BI = Causale blocco
017200110323     d dsBI          e ds                  inz
017300100624
017400100624      // - Tabella IC = Importanza clienti
017500100624     d dsIC          e ds                  inz
017600100624
017700100624      // - Tabella 1L = Categoria cliente
017800100624     d ds1L          e ds                  inz
017900100624
018000100624      // - Tabella 4W = Stato del credito
018100100624     d ds4W          e ds                  inz
018200100428
018300080206      //---------------------------------------------------------------
018400080206      //?Definizione variabili globali.
018500080206      //---------------------------------------------------------------
018600080206
018700080206      // - Flags booleani
018800100312     d $End            s               n   inz(*off)
018900080208     d $Fine           s               n   inz(*off)
019000100625     d $InzD01         s               n   inz(*on)
019100080206     d $ErrGrave       s               n   inz(*off)
019200080207     d $EoF            s               n   inz(*off)
019300120201     d Sede            s               n   inz(*off)
019400080206
019500080207      // - Campi associati al video
019600100625     d $Video          s              2    inz('D1')
019700100721
019800100721      // - Indici di schiera
019900100721     d xx              s              4  0 inz
020000110517
020100110517      // - Campi x passaggio parametri
020200110517     d PARMksc         s              7    inz
020300100721
020400100721      // - Campi di comodo
020500100721     d wdata           s              6
020600100721     d wf24            s              2  0
020700110407     d wmod            s              1
020800100721     d wpos            s              2  0
020900100721     d wposda          s              2  0
021000100721     d wpostf1         s              5  0
021100100721     d wpostf2         s              5  0
021200100721     d wpostf3         s              5  0
021300100721     d wrig            s              2  0
021400100721     d wtf1            s             62
021500100721     d wtf2            s             62
021600100721     d wtf3            s             62
021700080208
021800090508      //---------------------------------------------------------------
021900090508      //?Definizione procedure usate.
022000090508      //---------------------------------------------------------------
022100100721
022200100721      // - Interrogazione Unificante
022300100721     d tibs12r         pr                  extpgm('TIBS12R')
022400100721     d  kpjba                              likeds(KPJBA)
022500110517
022600110517      // - Interrogazione codici collegati
022700110517     d tnta83r         pr                  extpgm('TNTA83R')
022800110517     d  kpjba                              likeds(kpjba)
022900100721
023000100721      // - Interrogazione Potenziale
023100100721     d trmk02r         pr                  extpgm('TRMK02R')
023200100721     d  kpjba                              likeds(kpjba)
023300100721     d  trmk01ds                           likeds(trmk01ds)
023400100722     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
023500100809
023600100809      // - Inserimento attività primo contatto
023700100809     d trmk17r         pr                  extpgm('TRMK17R')
023800100809     d  kpjba                              likeds(KPJBA)
023900100809     d  trmk17ds                           likeds(TRMK17ds)
024000100624
024100100624      // - Interrogazione attività
024200100624     d trmk21r         pr                  extpgm('TRMK21R')
024300100624     d  kpjba                              likeds(KPJBA)
024400100624     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
024500110907
024600110907      // - Dati Fatturazione
024700110907     d trul57r         pr                  extpgm('TRUL57R')
024800110907     d  kpjba                              likeds(KPJBA)
024900110907     d  trul57ds                           likeds(TRUL57ds)
025000150126
025100150126       // -?Gestione Obiettivo (per visualizzazione)?
025200150126     d TRKC07ds      e ds                  inz
025300150126     d   iKC07ric    e                     inz('5')
025400150126     d TRKC07R         pr                  extpgm('TRKC07R')
025500150126     d   kpjba                             likeds(KPJBA)
025600150126     d   trkc07ds                          likeds(TRKC07ds)
025700150126
025800150126       // -?Controllo se cliente in Campagna?
025900150126     d TRKC73ds      e ds                  inz
026000150126     d TRKC73R         pr                  extpgm('TRKC73R')
026100150126     d   kpjba                             likeds(kpjba)
026200150126     d   trkc73ds                          likeds(TRKC73ds)
026300080206
026400080626      //---------------------------------------------------------------
026500080626      //?prototipi
026600080626      //---------------------------------------------------------------
026700080626      /copy gaitrasrc/srcprotopr,tibs34r
026800100428      /copy gaitrasrc/srcprotopr,tibs69r
026900100721      /copy gaitrasrc/srcprotopr,tnta12r
027000100809      /copy gaitrasrc/srcprotopr,tnta88r
027100100624      /copy gaitrasrc/srcprotopr,trmk19r
027200100624      /copy gaitrasrc/srcprotopr,trmk20r
027300100721      /copy gaitrasrc/srcprotopr,trmk50r
027400100624
027500080206      //---------------------------------------------------------------
027600080206      //?Definizione key-list.
027700080206      //---------------------------------------------------------------
027800110615
027900110615       // - File TFNTC01L
028000110615     d k04tfntc      e ds                  extname(TFNTC01L:*key)
028100110615     d                                     prefix(k_)
028200080206
028300080206      //---------------------------------------------------------------
028400080206      //?Riepilogo indicatori.
028500080206      //---------------------------------------------------------------
028600150126      // 04    : F04 = Campagna
028700080207      // 28    : Emissione messaggio di errore a video
028800100624      // 40    : F05 Pink
028900100624      // 41    : F18 Pink
029000100721      // 50    : Errore:
029100080207      // 99    : Generico di Errore
029200080206      //---------------------------------------------------------------
029300080206
029400080206      //---------------------------------------------------------------
029500080206      //?M A I N - L I N E
029600080206      //---------------------------------------------------------------
029700080206
029800080206     c     *Entry        plist
029900080206     c                   parm                    KPJBA
030000100721     c                   parm                    TNTAI2DS
030100080206
030200080206      /free
030300080206
030400100622       //?Operazioni iniziali
030500080206       exsr RoutInz;
030600080206
030700100622       //?Gestione video
030800080206       DOW $Fine = *off;
030900100624         SELECT;
031000100625           WHEN $Video = 'D1';
031100100625             exsr GesD01;
031200100624           OTHER;
031300080206             $Fine = *on;
031400100624         ENDSL;
031500080206       ENDDO;
031600080206
031700100622       //?Operazioni finali
031800080206       exsr RoutEnd;
031900080206
032000080206       //--------------------------------------------------------------
032100080206       //?Operazioni iniziali.
032200080206       //--------------------------------------------------------------
032300080206       BEGSR RoutInz;
032400100427
032500100622         exec sql
032600100622          set option dynusrprf = *owner, closqlcsr = *endmod;
032700080206
032800100622       //?Impostazione campi "fissi"
032900080208         T01pgm = SDSpgm;
033000080208         TBLkut = 1;
033100080206
033200100622       //?Reperimento dati job
033300080206         exsr DatiJob;
033400100721
033500100721       //?Se non passato il cliente torno errore
033600100721         IF  ITAI2ksc = 0;
033700100721           $ErrGrave = *on;
033800100721           $Fine     = *on;
033900100721           V1Dmsg    = $Msg(01);
034000100721           leavesr;
034100100721         ENDIF;
034200120201
034300120201       //?Controllo se utente di Sede
034400120201         Sede = (DutPou = 046);
034500090714
034600080206       ENDSR;
034700080206
034800080206       //--------------------------------------------------------------
034900080206       //?Reperimento Dati del job (Utente/Operativi).
035000080206       //--------------------------------------------------------------
035100080206       BEGSR DatiJob;
035200080206
035300080206         in(E) §AzUte;
035400080206         if NOT %error;
035500080206           in(E) §DatiUte;
035600080206         endif;
035700080206         if %error or RSut = *blanks;
035800080206           clear TIBS34ds;
035900080206           tibs34r(tibs34ds);
036000080206           in §AzUte;
036100080206           in §DatiUte;
036200080206         endif;
036300080206
036400080206       ENDSR;
036500100625
036600100625       //--------------------------------------------------------------
036700100625       //?Gestione videata D01.
036800100625       //--------------------------------------------------------------
036900100625       BEGSR GesD01;
037000100625
037100100625       //?Inizializzazione videata
037200100625         IF  $InzD01;
037300100625           exsr InzD01;
037400100625           $InzD01  = *off;
037500100721         //?Imposto tasti funzionali
037600100721           exsr TastiFun;
037700100625         ENDIF;
037800100721
037900100721       //?Emissione Testata
038000100721         write  TAI2T01;
038100100721       //?Emissione tasti funzionali
038200100721         write  TAI2Z01;
038300100721
038400100721       //?Emissione videata
038500100721         exfmt  TAI2D01;
038600100721         reset ErrMessage;
038700100721         reset ErrGenerico;
038800100721         clear V1Dmsg;
038900100721
039000100721         SELECT;
039100100721
039200100721       //?- F2=Rubrica
039300100721           WHEN  dsp_aid = c_F02;
039400100721             exsr F02D01;
039500100721
039600100721       //?- F3=Fine
039700100721           WHEN  dsp_aid = c_F03;
039800100721             exsr F03D01;
039900150126
040000150126           // -?F4=Campagna?
040100150126           WHEN  dsp_aid = c_F04;
040200150126             exsr  F04D01;
040300100721
040400100721       //?- F05=Attività
040500100721           WHEN  dsp_aid = c_F05;
040600100721             exsr F05D01;
040700100810             IF  ErrGenerico;
040800100810               leavesr;
040900100810             ENDIF;
041000100721
041100100721       //?- F07=Unificante
041200100721           WHEN  dsp_aid = c_F07;
041300100721             exsr F07D01;
041400100721
041500100721       //?- F12=Ritorno
041600100721           WHEN  dsp_aid = c_F12;
041700100721             exsr F12D01;
041800100721
041900100721       //?- F14=Info Commerciali
042000100721           WHEN  dsp_aid = c_F14;
042100100721             exsr F14D01;
042200110517
042300110517       //?- F15=Codici Collegati
042400110517           WHEN  dsp_aid = c_F15;
042500110517             exsr F15D01;
042600110907
042700110907       //?- F17=Dati Fatturazione
042800110907           WHEN  dsp_aid = c_F17;
042900110907             exsr F17D01;
043000100721
043100100721       //?- F18=Note
043200100721           WHEN  dsp_aid = c_F18;
043300100721             exsr F18D01;
043400100721
043500100721       //?- F20=Potenziali
043600100721           WHEN  dsp_aid = c_F20;
043700100721             exsr F20D01;
043800110407
043900110407       //?- F21=Blocco clienti
044000110407           WHEN  dsp_aid = c_F21;
044100110407             exsr F21D01;
044200100809
044300100809       //?- F22=Richiesta contatto
044400100809           WHEN  dsp_aid = c_F22;
044500100809             exsr F22D01;
044600100721
044700100721       //?- F24=Altri tasti
044800100721           WHEN  dsp_aid = c_F24 and wf24 > 1;
044900100721             exsr F24D01;
045000100721
045100100721       //?Invio
045200100721           OTHER;
045300120201         //?Se utente di Sede all'enter non deve fare niente
045400120201             IF  Sede;
045500120201               leavesr;
045600120201             ENDIF;
045700100721         //?Controllo se ci sono altre attività
045800100721             exsr AltreAtt;
045900100721             IF  ErrGenerico;
046000100721               leavesr;
046100100721             ENDIF;
046200100721        //?Se non è stato gestito da TRMK21R vado avanti con TRMK19R
046300100809             IF  ITAI2ric = 'A';
046400100809               exsr CallTrmk19;
046500100809             ENDIF;
046600100809        //?Se da trattativa vado avanti con TNTA88R
046700100809             IF  ITAI2ric = 'T';
046800100809               exsr CallTnta88;
046900100809             ENDIF;
047000100625
047100100625         ENDSL;
047200100625
047300100625       ENDSR;
047400100625
047500100625       //--------------------------------------------------------------
047600100625       //?Inizializzazione videata D01.
047700100625       //--------------------------------------------------------------
047800100625       BEGSR InzD01;
047900100625
048000100721         clear TAI2D01;
048100110407         clear wmod;
048200100721
048300100809         V1Cksc = ITAI2ksc;
048400100721
048500100721       //?Aggancio i file dell'anagrafica cliente
048600100721         clear tibs69ds;
048700100721         clear ds_cnaco;
048800100721         clear ds_cnind;
048900100721         clear ds_cnclp;
049000100721         clear ds_fncls;
049100100721         I69kac = V1Cksc;
049200100721         I69kin = V1Cksc;
049300100721         I69kcp = V1Cksc;
049400100721         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
049500100721         IF  O69err <> *blanks;
049600100721           leavesr;
049700100721         ENDIF;
049800100721
049900100721       //?Dati anagrafici
050000100721         V1Crag = ACOrag;
050100100721         V1Cvia = INDvia;
050200100721         V1Ccit = INDcit;
050300100721         V1Ccae = INDcae;
050400100721         V1Cprv = INDprv;
050500100721         V1Csta = INDsta;
050600100721         V1Ctel = INDtel;
050700100721         V1Ctlf = INDtlf;
050800100721         V1Ccdf = INDcdf;
050900100721
051000100721       //?Partita IVA
051100100721         IF  %subst(INDiva:1:2) >= '00';
051200100721           V1ivait = %subst(INDiva:1:14);
051300100721         ELSE;
051400100721           V1ivaeu = %subst(INDiva:1:2);
051500100721           V1ivait = %subst(INDiva:3:14);
051600100721         ENDIF;
051700100721
051800100721       //?Blocco cliente
051900100721         V1Cabl = ACOabl;
052000130321
052100130321       //?Se blocco cliente "7" visualizzo "8"
052200130321         IF  V1Cabl = '7';
052300130321           V1cabl = '8';
052400130321         ENDIF;
052500100721
052600100721       //?Causale blocco cliente
052700100721         V1Cblc = CLPnar;
052800110323         clear dsBI;
052900100721         clear V1Dblc;
053000100722         IF  V1Cblc <> *blanks;
053100100722           TBLcod = 'BI';
053200100722           TBLkey = V1Cblc;
053300100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
053400100722           IF  %found(TABEL00F);
053500110323             dsBI = TBLuni;
053600100722           ENDIF;
053700110323           V1Dblc = §BIdes;
053800100722         ENDIF;
053900100721
054000100721       //?Stato del credito
054100100721         V1Ccon = %subst(CLPcon:2:2);
054200100721         clear V1Dcon;
054300100722         clear ds4W;
054400100722         TBLcod = '4W';
054500100722         TBLkey = V1Ccon;
054600100722         chain (TBLkut : TBLcod : TBLkey) TABEL;
054700100722         IF  %found(TABEL00F);
054800100722           ds4W = TBLuni;
054900100722         ENDIF;
055000100722         V1Dcon = §4wdes;
055100100721
055200100721       //?Codice potenziale
055300100721         V1Ccpo = ACOlib;
055400100722         clear V1Dcpo;
055500100722         IF  V1Ccpo <> 0;
055600100722           chain V1Ccpo TNCPO01L;
055700100722           IF  %found(TNCPO01L);
055800100722             V1Dcpo = CPOrag;
055900100722           ENDIF;
056000100721         ENDIF;
056100100721
056200100721       //?Codice commerciale
056300100721         V1Cage = CLPage;
056400100721         clear V1Dage;
056500100722         IF  V1Cage <> 0;
056600130802           chain (CLPage) AZCMM000;
056700130802           IF  %found(AZCMM01L);
056800130802             V1Dage = CMMdes;
056900100722           ENDIF;
057000100722         ENDIF;
057100100721
057200100721       //?Codice importanza cliente
057300100721         V1Cclv = CLPclv;
057400100721         clear V1Dclv;
057500100722         IF  V1Cclv <> *blanks;
057600100722           clear dsIC;
057700100722           TBLcod = 'IC';
057800100722           TBLkey = V1Cclv;
057900100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
058000100722           IF  %found(TABEL00F);
058100100722             dsIC = TBLuni;
058200100722           ENDIF;
058300100722           V1Dclv = §sicde;
058400100722         ENDIF;
058500100721
058600100721       //?Categoria merceologica
058700100721         V1Citc = ACOitc;
058800100721         clear V1Ditc;
058900100722         IF  V1Citc <> 0;
059000100722           clear ds1L;
059100100722           TBLcod = '1L';
059200100722           TBLkey = %editc(V1Citc:'X');
059300100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
059400100722           IF  %found(TABEL00F);
059500100722             ds1L = TBLuni;
059600100722           ENDIF;
059700100722           V1Ditc = §1Ldes;
059800100722         ENDIF;
059900100721
060000100721       //?Sottoconto fatturazione
060100100721         V1Cscf = CLPScf;
060200100721
060300100721       //?Data prima e ultima spedizione fattura
060400100721         %subst(wdata:5:2) = %subst(%editc(CLPdps:'X'):1:2);
060500100721         %subst(wdata:3:2) = %subst(%editc(CLPdps:'X'):3:2);
060600100721         %subst(wdata:1:2) = %subst(%editc(CLPdps:'X'):5:2);
060700100721         V1Cdps = %dec(wdata:6:0);
060800100721
060900100721         %subst(wdata:5:2) = %subst(%editc(CLPdus:'X'):1:2);
061000100721         %subst(wdata:3:2) = %subst(%editc(CLPdus:'X'):3:2);
061100100721         %subst(wdata:1:2) = %subst(%editc(CLPdus:'X'):5:2);
061200100721         V1Cdus = %dec(wdata:6:0);
061300100721
061400100721       //?Decodifico il sottoconto fatturazione
061500100722         IF  V1Cscf <> 0;
061600100722           clear tibs69ds;
061700100722           clear ds_cnaco;
061800100722           clear ds_cnind;
061900100722           clear ds_cnclp;
062000100722           clear ds_fncls;
062100100722           I69kac = V1Cscf;
062200100722           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
062300100722           V1Dscf = ACOrag;
062400100722         ENDIF;
062500110615
062600110615       //?Note DC
062700110615         k_NTCapl = 'C';
062800110615         k_NTCnk1 = %editc(ACOkcc:'X') + %editc(ACOksc:'X');
062900110615         clear k_NTCnk2;
063000110615         k_NTCtnt = 'DC';
063100110615         chain  %kds(K04tfntc) TFNTC01L;
063200110615         IF  %found(TFNTC01L);
063300110615         V1cnota = NTCrnt;
063400110615         ENDIF;
063500110615
063600100625
063700100625       ENDSR;
063800100721
063900100721       //--------------------------------------------------------------
064000100721       //?Imposto i tasti funzionali.
064100100721       //--------------------------------------------------------------
064200100721       BEGSR TastiFun;
064300141118
064400150126         // -?Se Cliente in Campagna visualizzo il tasto F4?
064500150126         F04Attivo = *off;
064600150126         clear  TRKC73ds;
064700150126         iKC73ksu = iTAI2ksc;
064800150126         iKC73dat = %dec( %date() );
064900150126         trkc73r ( kpjba : TRKC73ds );
065000150126         // -?Se torna almeno un numero campagna?
065100150126         F04Attivo = ( oKC73nc1 <> *zero );
065200100721
065300100721       //?Se presenti attività sul cliente visualizzo il tasto in PINK
065400100721         F05Pink = *off;
065500100721         chain (V1Ccpo:V1Cksc) TIATC04L;
065600100721         IF  %found(TIATC04L);
065700100721           F05Pink = *on;
065800100721         ENDIF;
065900100721
066000100721       //?Se presenti note sul cliente visualizzo il tasto in PINK
066100100721         F18Pink = *off;
066200100721         chain (V1Cksc) TICPN02L;
066300100721         IF  %found(TICPN02L);
066400100721           F18Pink = *on;
066500100721         ENDIF;
066600110502
066700110502       //?Abilito tasto F21=Blocco cliente se il cliente non è già bloccato
066800120201       //?non abilito se utente di Sede
066900110502         F21Attivo = *off;
067000120201         IF  V1Cabl = *blanks and not Sede;
067100110502           F21Attivo = *on;
067200110502         ENDIF;
067300120201
067400120201       //?Abilito tasto F22=Richiesta contatto se utente di filiale
067500120201         F22Attivo = *off;
067600120201         IF  not Sede;
067700120201           F22Attivo = *on;
067800120201         ENDIF;
067900100721
068000100721       //?Imposto la seconda riga variabile
068100100721         clear wpos;
068200100721         clear wposda;
068300100721         clear wpostf1;
068400100721         clear wpostf2;
068500100721         clear wpostf3;
068600100721         clear wrig;
068700100721         clear wtf1;
068800100721         clear wtf2;
068900100721         clear wtf3;
069000100721
069100100721         xx = 1;
069200100721         FOR xx by 1 to %elem($Tfv);
069300110502         //?Se F21 non abilitato leggo altro tasto funzione
069400110502           IF  not F21Attivo and %subst($Tfv(xx):1:3) = 'F21';
069500110502             iter;
069600110502           ENDIF;
069700120201         //?Se F22 non abilitato leggo altro tasto funzione
069800120201           IF  not F22Attivo and %subst($Tfv(xx):1:3) = 'F22';
069900120201             iter;
070000120201           ENDIF;
070100110502
070200100721           IF  $Tfv(xx) <> *blanks;
070300100721             wpos = %scan('#':$tfv(xx));
070400100721             wpostf1 += wpos;
070500100721
070600100721             IF  wpostf1 <= %len(VZFd01);
070700100721               wposda = wpostf1 - wpos + 1;
070800100721               %subst(wtf1:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
070900100721             ELSE;
071000100721               wpostf2 += wpos;
071100100721               IF  wpostf2 <= %len(VZFd01);
071200100721                 wposda = wpostf2 - wpos + 1;
071300100721                 %subst(wtf2:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
071400100721               ELSE;
071500100721                 wpostf3 += wpos;
071600100721                 IF  wpostf3 <= %len(VZFd01);
071700100721                   wposda = wpostf3 - wpos + 1;
071800100721                   %subst(wtf3:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
071900100721                 ENDIF;
072000100721               ENDIF;
072100100721             ENDIF;
072200100721
072300100721           ENDIF;
072400100721         ENDFOR;
072500100721
072600100721       //?Imposto F24 giusto
072700100721         SELECT;
072800100721           WHEN  wtf3 <> *blanks;
072900100721             VZFd02 = cf2413;
073000100721             wf24 = 3;
073100100721           WHEN  wtf2 <> *blanks;
073200110502             VZFd02 = cf2412;
073300100721             wf24 = 2;
073400100721           OTHER;
073500100721             clear VZFd02;
073600100721             wf24 = 1;
073700100721         ENDSL;
073800100721
073900100721         VZFd01 = wtf1;
074000100721         wrig = 1;
074100100721
074200100721       ENDSR;
074300141118
074400141118       //--------------------------------------------------------------
074500150126       //?Gestione tasto funzionale F04 da videata D01                 ?
074600150126       //?F4=Campagna                                                  ?
074700141118       //--------------------------------------------------------------
074800150126       BEGSR F04D01;
074900141118
075000150112         reset  TRKC07ds;
075100150112         //IKC07ric = '5';         ?(già così)?
075200150126         iKC07ncm = oKC73nc1;
075300150126         iKC07ksu = oKC73ksu;
075400141118
075500150112         trkc07r ( kpjba : TRKC07ds );
075600141118
075700141118       ENDSR;
075800100721
075900100721       //--------------------------------------------------------------
076000100721       //?Gestione tasto funzionale F02 da videata D01
076100100721       //?F2=Rubrica
076200100721       //--------------------------------------------------------------
076300100721       BEGSR F02D01;
076400100721
076500100721         clear TNTA12DS;
076600100721         TA12ric = 'V';
076700100721         TA12apl = 'C';
076800100721         TA12ksc = V1Cksc;
076900100721         TA12rag = V1Crag;
077000100721
077100100721         tnta12r (kpjba : TNTA12DS);
077200100721
077300100721         IF  TA12err <> *blanks;
077400100721           ErrMessage  = *on;
077500100721           ErrGenerico = *on;
077600100721           V1Dmsg      = TA12msg;
077700100721           leavesr;
077800100721         ENDIF;
077900100721
078000100721       ENDSR;
078100100625
078200100625       //--------------------------------------------------------------
078300100625       //?Gestione tasto funzionale F03 da videata D01
078400100625       //?F3=Fine
078500100625       //--------------------------------------------------------------
078600100625       BEGSR F03D01;
078700100625
078800100625         // Chiusura del programma
078900100928         OTAI2f03 = '1';
079000100625         $Fine = *on;
079100100625
079200100625       ENDSR;
079300100721
079400100721       //--------------------------------------------------------------
079500100721       //?Gestione tasto funzionale F05 da videata D01
079600100721       //?F5=Attività
079700100721       //--------------------------------------------------------------
079800100721       BEGSR F05D01;
079900100721
080000100721         clear TRMK21DS;
080100100810         clear TRMK19DS;
080200100810         I21mod = 'V';
080300100810         I21cpo = V1Ccpo;
080400100721         I21ksc = V1Cksc;
080500100721         I21rsc = V1Crag;
080600100721         kpjbu = TRMK21DS;
080700100810         IF  ITAI2ric = 'A';
080800100810           IMK19tco = 'T';
080900100810         ENDIF;
081000100810         IF  ITAI2ric = 'T';
081100100810           IMK19tco = 'O';
081200100810         ENDIF;
081300100810         IMK19cpo  = V1Ccpo;
081400100810         IMK19ksc  = V1Cksc;
081500100810         IMK19com  = ITAI2com;
081600100810         IMK19comd = ITAI2comd;
081700120201
081800120201       //?Se utente di Sede devo SOLO visualizzare le attività
081900120201         IF  Sede;
082000120201           I21mod = 'I';
082100120201           kpjbu = TRMK21DS;
082200120201           trmk21r (kpjba);
082300120201         ELSE;
082400100810         trmk21r (kpjba : TRMK19DS);
082500120201         ENDIF;
082600120201
082700100721         TRMK21DS = kpjbu;
082800100810         IF  O21err <> *blanks;
082900100810           ErrGenerico = *on;
083000100810           ErrMessage  = *on;
083100100810           V1Dmsg      = O21msg;
083200100810           leavesr;
083300100810         ENDIF;
083400120201
083500120201        //?Se utente di Sede non devo fare + nessun controllo
083600120201         IF  Sede;
083700120201           leavesr;
083800120201         ENDIF;
083900120201
084000100810        //?Se F12 torno al dettaglio cliente
084100100810         IF  O21fxx = '12';
084200100810           ErrGenerico = *on;
084300100810         ENDIF;
084400100810        //?Se gestita attività esco dal pgm
084500100810         IF  O21fxx = '99';
084600100810           ErrGenerico = *on;
084700100810           $Fine = *on;
084800100810         ENDIF;
084900100810        //?Se non è stato gestito da TRMK21R
085000100927        //?o se fatto F10 devo andare avanti con il TRMK19R o TNTA88R
085100100927         IF  O21fxx = *blanks or O21fxx = '10';
085200100810         //?vado avanti con TRMK19R se arrivo da attività
085300100810           IF  ITAI2ric = 'A';
085400100810             exsr CallTrmk19;
085500100810           ENDIF;
085600100810         //?vado avanti con TNTA88R se arrivo da trattative
085700100810           IF  ITAI2ric = 'T';
085800100810             exsr CallTnta88;
085900100810           ENDIF;
086000100810         ENDIF;
086100100721
086200100721       ENDSR;
086300100721
086400100721       //--------------------------------------------------------------
086500100721       //?Gestione tasto funzionale F07 da videata D01
086600100721       //?F7=Unificante
086700100721       //--------------------------------------------------------------
086800100721       BEGSR F07D01;
086900100721
087000100721         clear TIBS12DS;
087100100721         K12op0 = 'P05';
087200100721         K12op1 = 'O09';
087300100721         K12f03 = '0';
087400100721         K12f12 = '0';
087500100721         K12err = '0';
087600100721         K12cop = V1Cksc;
087700100721         kpjbu = TIBS12DS;
087800100721
087900100721         tibs12r (kpjba);
088000100721
088100100721       ENDSR;
088200100721
088300100721       //--------------------------------------------------------------
088400100721       //?Gestione tasto funzionale F12 da videata D01
088500100721       //?F12=Fine
088600100721       //--------------------------------------------------------------
088700100721       BEGSR F12D01;
088800100721
088900100721         // Chiusura del programma
089000100721         $Fine = *on;
089100100721
089200100721       ENDSR;
089300100721
089400100721       //--------------------------------------------------------------
089500100721       //?Gestione tasto funzionale F14 da videata D01
089600100721       //?F14=Info Commerciali
089700100721       //--------------------------------------------------------------
089800100721       BEGSR F14D01;
089900100721
090000100721         clear TRMK50DS;
090100100721         I50cpo = V1Ccpo;
090200110407         IF  wmod = *blanks;
090300110407           I50mod = 'I';
090400110407         ELSE;
090500110407           I50mod = wmod;
090600110407         ENDIF;
090700100721         I50rag = V1Crag;
090800100721         I50pgm = 'TNTAI2R';
090900100721
091000100721         trmk50r (kpjba : TRMK50DS);
091100100721
091200100721       ENDSR;
091300110517
091400110517       //--------------------------------------------------------------
091500110517       //?Gestione tasto funzionale F15 da videata D01
091600110517       //?F15=Codici Collegati
091700110517       //--------------------------------------------------------------
091800110517       BEGSR F15D01;
091900110517
092000110517         IF  V1Cscf = *zeros;
092100110517           ErrGenerico = *on;
092200110517           ErrMessage  = *on;
092300110517           V1Dmsg      = $msg(02);
092400110517           leavesr;
092500110517         ENDIF;
092600110517
092700110517         PARMksc = %editc(V1Cscf:'X');
092800110517         kpjbu = PARMksc;
092900110517         tnta83r (kpjba);
093000110517
093100110517       ENDSR;
093200110907
093300110907       //--------------------------------------------------------------
093400110907       //?Gestione tasto funzionale F17 da videata D01
093500110907       //?F17=Dati Fatturazione
093600110907       //--------------------------------------------------------------
093700110907       BEGSR F17D01;
093800110907
093900110907         clear trul57ds;
094000110907         D57ikscb = V1Cksc;
094100110907         D57ikscf = V1Cscf;
094200110907         D57iunib = %subst(CLSflo:2:1);
094300110907         D57itftb = %editc(CLPtft:'X');
094400110907         D57ifftb = %editc(CLPfft:'X');
094500110907         D57idftb = CLPfun;
094600110907         D57ipgm  = 'TNTAI2R';
094700110907         D57isifb = INDsin;
094800110907         D57icdpb = INDcdp;
094900110907         D57iabib = INDabi;
095000110907         D57icabb = INDcab;
095100110907
095200110907         trul57r (kpjba : TRUL57DS);
095300110907
095400110907       ENDSR;
095500100721
095600100721       //--------------------------------------------------------------
095700100721       //?Gestione tasto funzionale F18 da videata D01
095800100721       //?F18=Note
095900100721       //--------------------------------------------------------------
096000100721       BEGSR F18D01;
096100100721
096200100721         clear TRMK20DS;
096300110210         IMK20tla = 'L';
096400100721         IMK20flm = 'I';
096500100721         IMK20ksc = V1Cksc;
096600100721         IMK20rsc = V1Crag;
096700100721
096800100721         trmk20r (kpjba : TRMK20DS);
096900100915
097000100915       //?Se presenti note sul cliente visualizzo il tasto in PINK
097100100915         F18Pink = *off;
097200100915         chain (V1Cksc) TICPN02L;
097300100915         IF  %found(TICPN02L);
097400100915           F18Pink = *on;
097500100915         ENDIF;
097600100721
097700100721       ENDSR;
097800100721
097900100721       //--------------------------------------------------------------
098000100721       //?Gestione tasto funzionale F20 da videata D01
098100100721       //?F20=Potenziali
098200100721       //--------------------------------------------------------------
098300100721       BEGSR F20D01;
098400100721
098500100721         clear TRMK01DS;
098600100721         MK01k10 = 'N';
098700100721         MK01cpo = V1ccpo;
098800100721         trmk02r (kpjba : TRMK01DS);
098900100721
099000100721       ENDSR;
099100110407
099200110407       //--------------------------------------------------------------
099300110407       //?Gestione tasto funzionale F21 da videata D01
099400110407       //?F21=Blocco Cliente
099500110407       //--------------------------------------------------------------
099600110407       BEGSR F21D01;
099700110407
099800110407         clear trmk17ds;
099900110407         IMK17patt = 'S';
100000110407         IMK17att  = 'T';
100100110407         IMK17pcau = 'S';
100200110407         IMK17cau  = '71';
100300110407         IMK17cpo  = V1Ccpo;
100400110407         IMK17ksc  = V1Cksc;
100500110407         IF  ITAI2com <> 0;
100600110407           IMK17co3  = ITAI2com;
100700110407         ENDIF;
100800110407
100900110407         trmk17r (kpjba : TRMK17DS);
101000110407
101100110407       //?Se ho pianificato l'attività devo richiamare la gestione
101200110407       //?delle info commerciali
101300110407         IF  OMK17f12 = *blanks and OMK17err = *blanks;
101400110407           wmod = 'G';
101500110407           exsr F14D01;
101600110407         ENDIF;
101700110407
101800110407       //?Se presenti attività sul cliente visualizzo il tasto in PINK
101900110407         F05Pink = *off;
102000110407         chain (V1Ccpo:V1Cksc) TIATC04L;
102100110407         IF  %found(TIATC04L);
102200110407           F05Pink = *on;
102300110407         ENDIF;
102400110407
102500110407       ENDSR;
102600100809
102700100809       //--------------------------------------------------------------
102800100809       //?Gestione tasto funzionale F22 da videata D01
102900100809       //?F22=Richiesta Contatto
103000100809       //--------------------------------------------------------------
103100100809       BEGSR F22D01;
103200100809
103300100809         clear trmk17ds;
103400100809         IMK17patt = 'S';
103500100809         IMK17att  = 'T';
103600100809         IMK17pcau = 'S';
103700100809         IMK17cau  = '20';
103800100809         IMK17cpo  = V1Ccpo;
103900100809         IMK17ksc  = V1Cksc;
104000100809         IF ITAI2com <> 0;
104100100809           IMK17co3  = ITAI2com;
104200100809         ENDIF;
104300100809
104400100809         trmk17r (kpjba : TRMK17DS);
104500100915
104600100915       //?Se presenti attività sul cliente visualizzo il tasto in PINK
104700100915         F05Pink = *off;
104800100915         chain (V1Ccpo:V1Cksc) TIATC04L;
104900100915         IF  %found(TIATC04L);
105000100915           F05Pink = *on;
105100100915         ENDIF;
105200100809
105300100809       ENDSR;
105400100721
105500100721       //--------------------------------------------------------------
105600100721       //?Gestione tasto funzionale F24 da videata D01
105700100721       //?F24=Altri Tasti
105800100721       //--------------------------------------------------------------
105900100721       BEGSR F24D01;
106000100721
106100100721         SELECT;
106200100721           WHEN  (wf24 = 3 and wrig = 3) or
106300100721                 (wf24 = 2 and wrig = 2);
106400100721             VZFd01 = wtf1;
106500100721             wrig = 1;
106600100721             IF  wf24 = 2;
106700100721               VZFd02 = cf2412;
106800100721             ELSE;
106900100721               VZFd02 = cf2413;
107000100721             ENDIF;
107100100721
107200100721           WHEN  (wf24 = 3 and wrig = 1) or
107300100721                 (wf24 = 2 and wrig = 1);
107400100721             VZFd01 = wtf2;
107500100721             wrig = 2;
107600100721             IF  wf24 = 2;
107700100721               VZFd02 = cf2422;
107800100721             ELSE;
107900100721               VZFd02 = cf2423;
108000100721             ENDIF;
108100100721
108200100721           WHEN  wf24 = 3 and wrig = 2;
108300100721             VZFd01 = wtf3;
108400100721             wrig = 3;
108500100721             VZFd02 = cf2433;
108600100721
108700100721         ENDSL;
108800100721
108900100721       ENDSR;
109000100625
109100100625       //--------------------------------------------------------------
109200100625       //?Controllo videata D01.
109300100625       //--------------------------------------------------------------
109400100625       BEGSR CtrD01;
109500100625
109600100625         WindDspF = IndDspF;
109700100625         reset WindDspFb;
109800100625         IndDspF  = WindDspF;
109900100625
110000100625       ENDSR;
110100100624
110200100624       //--------------------------------------------------------------
110300100624       //?Controllo se ci sono altre attività sul cliente.
110400100624       //--------------------------------------------------------------
110500100624       BEGSR AltreAtt;
110600100624
110700100624         IF  F05Pink;
110800100624           clear TRMK21DS;
110900100722           clear TRMK19DS;
111000100624           I21mod = 'G';
111100100722           I21cpo = V1Ccpo;
111200100721           I21ksc = V1Cksc;
111300100721           I21rsc = V1Crag;
111400100624           kpjbu  = TRMK21ds;
111500100809           IF  ITAI2ric = 'A';
111600100809             IMK19tco = 'T';
111700100809           ENDIF;
111800100809           IF  ITAI2ric = 'T';
111900100809             IMK19tco = 'O';
112000100809           ENDIF;
112100100722           IMK19cpo  = V1Ccpo;
112200100722           IMK19ksc  = V1Cksc;
112300100722           IMK19com  = ITAI2com;
112400100722           IMK19comd = ITAI2comd;
112500100624           trmk21r (kpjba : TRMK19DS);
112600100624           TRMK21DS = kpjbu;
112700100722           IF  O21err <> *blanks;
112800100722             ErrGenerico = *on;
112900100722             ErrMessage  = *on;
113000100722             V1Dmsg      = O21msg;
113100100722           ENDIF;
113200100722           IF  O21fxx = '12';
113300100722             ErrGenerico = *on;
113400100722           ENDIF;
113500100624           IF  O21fxx = '99';
113600100624             ErrGenerico = *on;
113700100722             $Fine = *on;
113800100624           ENDIF;
113900100624
114000100624         ENDIF;
114100100624
114200100624       ENDSR;
114300100624
114400100624       //--------------------------------------------------------------
114500100624       //?Richiamo TRMK19R per new attività.
114600100624       //--------------------------------------------------------------
114700100624       BEGSR CallTrmk19;
114800100624
114900100722         clear TRMK19ds;
115000100722         IMK19cmt  = 'S';
115100100722         IMK19fle  = 'C';
115200100722         IMK19tco  = 'T';
115300100722         IMK19cpo  = V1Ccpo;
115400100722         IMK19ksc  = V1Cksc;
115500100722         IMK19com  = ITAI2com;
115600100722         IMK19comd = ITAI2comd;
115700100722
115800100722         trmk19r ( kpjba: TRMK19DS );
115900100810       //?se non torna errore
116000100722       //?esco dal programma
116100100722         IF  OMK19Err = *blanks;
116200100722           $Fine = *on;
116300100722         ENDIF;
116400100810       //?se torna errore con relativo messaggio emetto il messaggio
116500100810         IF  OMK19Err <> *blanks and OMK19msg <> *blanks;
116600100810           ErrGenerico = *on;
116700100810           ErrMessage  = *on;
116800100810           V1Dmsg      = OMK19msg;
116900100810         ENDIF;
117000100810       //?se torna errore senza messaggio esco dal programma
117100100810       //?(in questo caso errore vale per F12 da TRMK19R)
117200100810         IF  OMK19Err <> *blanks and OMK19msg = *blanks;
117300100810           $Fine = *on;
117400100810         ENDIF;
117500100624
117600100624       ENDSR;
117700100809
117800100809       //--------------------------------------------------------------
117900100809       //?Richiamo TNTA88R per new trattativa.
118000100809       //--------------------------------------------------------------
118100100809       BEGSR CallTnta88;
118200100809
118300100809         clear TNTA88ds;
118400100809         ITA88cmt  = 'S';
118500100809         ITA88fle  = 'C';
118600100809         ITA88cpo  = V1Ccpo;
118700100809         ITA88ksc  = V1Cksc;
118800100809         ITA88com  = ITAI2com;
118900100809         ITA88comd = ITAI2comd;
119000100809
119100100809         tnta88r ( kpjba: TNTA88DS );
119200100809       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
119300100809       //?esco dal programma
119400100809         IF  OTA88Err = *blanks;
119500100809           $Fine = *on;
119600100809         ENDIF;
119700100809
119800100809       ENDSR;
119900080206
120000080206       //--------------------------------------------------------------
120100080206       //?Operazioni finali.
120200080206       //--------------------------------------------------------------
120300080206       BEGSR RoutEnd;
120400100721
120500100721        //?Se errore torno il dato al chiamante
120600100721         IF  $ErrGrave;
120700100721           OTAI2err = '1';
120800100721           OTAI2msg = V1Dmsg;
120900100721         ENDIF;
121000080206
121100080206         *inLR = *on;
121200080206         return;
121300080206
121400080206       ENDSR;
121500080206
121600080206      /end-free
121700100624
121800080206       //--------------------------------------------------------------
121900080206       //?Schiere a tempo di compilazione.
122000080206       //--------------------------------------------------------------
122100080206
122200080206** - $MSG -------------------------------------------------------------------*
122300100721Codice cliente non passato                                                     1
122400110517Codice sottoconto fatturazione non impostato interrogazione non possibile      2
122500100721
122600100721** - $Tfv (in ordine di visualizzazione) ------------------------------------*
122700100809F22=Richiesta Contatto   #
122800100721F14=InfoCommerciali   #
122900110517F15=Codici Collegati   #
123000110907F17=Dati Fatturazione   #
123100100721F20=Potenziali   #
123200110407F21=Blocco Cliente  #
123300110615F12=Ritorno   #
