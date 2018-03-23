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
011400100624     d  F05Pink                       1n   overlay(IndDspF : 40)
011500100624     d  F18Pink                       1n   overlay(IndDspF : 41)
011600110502        // - Indicatori di abilitazione
011700110502     d  F21Attivo                     1n   overlay(IndDspF : 21)
011800120201     d  F22Attivo                     1n   overlay(IndDspF : 22)
011900090422        // - Indicatori di errore generico
012000100721     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012100110517
012200090407     d WindDspF        ds                  inz
012300090407     d   WindDspFa             1     49    inz(*zero)
012400090407     d   WindDspFb            50     99    inz(*zero)
012500080206
012600080206      // - Parametri ricevuti
012700080206     d KPJBA         e ds
012800100721     d TNTAI2DS      e ds
012900100721
013000100721      // - Unificante
013100100721     d TIBS12ds      e ds
013200080206
013300080206      // - Reperimento dati utente
013400080206     d TIBS34ds      e ds
013500100624
013600100624       // - Reperimento dati anagrafici
013700100624     d TIBS69ds      e ds
013800100624     d DS_cnaco      e ds                  inz extname(CNACO00F)
013900100624     d DS_cnind      e ds                  inz extname(CNIND00F)
014000100624     d DS_cnclp      e ds                  inz extname(CNCLP00F)
014100100624     d DS_fncls      e ds                  inz extname(FNCLS00F)
014200100721
014300100721      // - Interrogazione Rubrica
014400100721     d TNTA12DS      e ds                  inz
014500100809
014600100809      // - Gestione trattative
014700100809     d TNTA88DS      e ds
014800100624
014900100624      // - Gestione attività
015000100624     d TRMK19DS      e ds
015100100722
015200100722      // - Interrogazione Potenziali
015300100721     d TRMK01DS      e ds                  extname(TRMK01DS)
015400100809
015500100809       // - Primo contatto
015600100809     d TRMK17DS      e ds
015700100722
015800100624      // - Interrogazione Note clienti
015900100624     d TRMK20DS      e ds                  inz
016000100624
016100100624      // - Interrogazione attività
016200100624     d TRMK21DS      e ds                  inz
016300100721
016400100721      // - Interrogazione Info Commerciali
016500100721     d TRMK50DS      e ds                  inz
016600110907
016700110907      // - Dati Fatturazione
016800110907     d TRUL57DS      e ds                  inz
016900110323
017000110323      // - Tabella BI = Causale blocco
017100110323     d dsBI          e ds                  inz
017200100624
017300100624      // - Tabella IC = Importanza clienti
017400100624     d dsIC          e ds                  inz
017500100624
017600100624      // - Tabella 1L = Categoria cliente
017700100624     d ds1L          e ds                  inz
017800100624
017900100624      // - Tabella 4W = Stato del credito
018000100624     d ds4W          e ds                  inz
018100100428
018200080206      //---------------------------------------------------------------
018300080206      //?Definizione variabili globali.
018400080206      //---------------------------------------------------------------
018500080206
018600080206      // - Flags booleani
018700100312     d $End            s               n   inz(*off)
018800080208     d $Fine           s               n   inz(*off)
018900100625     d $InzD01         s               n   inz(*on)
019000080206     d $ErrGrave       s               n   inz(*off)
019100080207     d $EoF            s               n   inz(*off)
019200120201     d Sede            s               n   inz(*off)
019300080206
019400080207      // - Campi associati al video
019500100625     d $Video          s              2    inz('D1')
019600100721
019700100721      // - Indici di schiera
019800100721     d xx              s              4  0 inz
019900110517
020000110517      // - Campi x passaggio parametri
020100110517     d PARMksc         s              7    inz
020200100721
020300100721      // - Campi di comodo
020400100721     d wdata           s              6
020500100721     d wf24            s              2  0
020600110407     d wmod            s              1
020700100721     d wpos            s              2  0
020800100721     d wposda          s              2  0
020900100721     d wpostf1         s              5  0
021000100721     d wpostf2         s              5  0
021100100721     d wpostf3         s              5  0
021200100721     d wrig            s              2  0
021300100721     d wtf1            s             62
021400100721     d wtf2            s             62
021500100721     d wtf3            s             62
021600080208
021700090508      //---------------------------------------------------------------
021800090508      //?Definizione procedure usate.
021900090508      //---------------------------------------------------------------
022000100721
022100100721      // - Interrogazione Unificante
022200100721     d tibs12r         pr                  extpgm('TIBS12R')
022300100721     d  kpjba                              likeds(KPJBA)
022400110517
022500110517      // - Interrogazione codici collegati
022600110517     d tnta83r         pr                  extpgm('TNTA83R')
022700110517     d  kpjba                              likeds(kpjba)
022800100721
022900100721      // - Interrogazione Potenziale
023000100721     d trmk02r         pr                  extpgm('TRMK02R')
023100100721     d  kpjba                              likeds(kpjba)
023200100721     d  trmk01ds                           likeds(trmk01ds)
023300100722     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
023400100809
023500100809      // - Inserimento attività primo contatto
023600100809     d trmk17r         pr                  extpgm('TRMK17R')
023700100809     d  kpjba                              likeds(KPJBA)
023800100809     d  trmk17ds                           likeds(TRMK17ds)
023900100624
024000100624      // - Interrogazione attività
024100100624     d trmk21r         pr                  extpgm('TRMK21R')
024200100624     d  kpjba                              likeds(KPJBA)
024300100624     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
024400110907
024500110907      // - Dati Fatturazione
024600110907     d trul57r         pr                  extpgm('TRUL57R')
024700110907     d  kpjba                              likeds(KPJBA)
024800110907     d  trul57ds                           likeds(TRUL57ds)
024900150806
025000150806       // -?Interrogazione campagne per cliente
025100150806     d TRKC09DS      e ds                  inz
025200150806     d TRKC09R         pr                  extpgm('TRKC09R')
025300150806     d   kpjba                             likeds(KPJBA)
025400150806     d   trkc09ds                          likeds(TRKC09DS)
025500080206
025600080626      //---------------------------------------------------------------
025700080626      //?prototipi
025800080626      //---------------------------------------------------------------
025900080626      /copy gaitrasrc/srcprotopr,tibs34r
026000100428      /copy gaitrasrc/srcprotopr,tibs69r
026100100721      /copy gaitrasrc/srcprotopr,tnta12r
026200100809      /copy gaitrasrc/srcprotopr,tnta88r
026300100624      /copy gaitrasrc/srcprotopr,trmk19r
026400100624      /copy gaitrasrc/srcprotopr,trmk20r
026500100721      /copy gaitrasrc/srcprotopr,trmk50r
026600100624
026700080206      //---------------------------------------------------------------
026800080206      //?Definizione key-list.
026900080206      //---------------------------------------------------------------
027000110615
027100110615       // - File TFNTC01L
027200110615     d k04tfntc      e ds                  extname(TFNTC01L:*key)
027300110615     d                                     prefix(k_)
027400080206
027500080206      //---------------------------------------------------------------
027600080206      //?Riepilogo indicatori.
027700080206      //---------------------------------------------------------------
027800080207      // 28    : Emissione messaggio di errore a video
027900100624      // 40    : F05 Pink
028000100624      // 41    : F18 Pink
028100100721      // 50    : Errore:
028200080207      // 99    : Generico di Errore
028300080206      //---------------------------------------------------------------
028400080206
028500080206      //---------------------------------------------------------------
028600080206      //?M A I N - L I N E
028700080206      //---------------------------------------------------------------
028800080206
028900080206     c     *Entry        plist
029000080206     c                   parm                    KPJBA
029100100721     c                   parm                    TNTAI2DS
029200080206
029300080206      /free
029400080206
029500100622       //?Operazioni iniziali
029600080206       exsr RoutInz;
029700080206
029800100622       //?Gestione video
029900080206       DOW $Fine = *off;
030000100624         SELECT;
030100100625           WHEN $Video = 'D1';
030200100625             exsr GesD01;
030300100624           OTHER;
030400080206             $Fine = *on;
030500100624         ENDSL;
030600080206       ENDDO;
030700080206
030800100622       //?Operazioni finali
030900080206       exsr RoutEnd;
031000080206
031100080206       //--------------------------------------------------------------
031200080206       //?Operazioni iniziali.
031300080206       //--------------------------------------------------------------
031400080206       BEGSR RoutInz;
031500100427
031600100622         exec sql
031700100622          set option dynusrprf = *owner, closqlcsr = *endmod;
031800080206
031900100622       //?Impostazione campi "fissi"
032000080208         T01pgm = SDSpgm;
032100080208         TBLkut = 1;
032200080206
032300100622       //?Reperimento dati job
032400080206         exsr DatiJob;
032500100721
032600100721       //?Se non passato il cliente torno errore
032700100721         IF  ITAI2ksc = 0;
032800100721           $ErrGrave = *on;
032900100721           $Fine     = *on;
033000100721           V1Dmsg    = $Msg(01);
033100100721           leavesr;
033200100721         ENDIF;
033300120201
033400120201       //?Controllo se utente di Sede
033500120201         Sede = (DutPou = 046);
033600090714
033700080206       ENDSR;
033800080206
033900080206       //--------------------------------------------------------------
034000080206       //?Reperimento Dati del job (Utente/Operativi).
034100080206       //--------------------------------------------------------------
034200080206       BEGSR DatiJob;
034300080206
034400080206         in(E) §AzUte;
034500080206         if NOT %error;
034600080206           in(E) §DatiUte;
034700080206         endif;
034800080206         if %error or RSut = *blanks;
034900080206           clear TIBS34ds;
035000080206           tibs34r(tibs34ds);
035100080206           in §AzUte;
035200080206           in §DatiUte;
035300080206         endif;
035400080206
035500080206       ENDSR;
035600100625
035700100625       //--------------------------------------------------------------
035800100625       //?Gestione videata D01.
035900100625       //--------------------------------------------------------------
036000100625       BEGSR GesD01;
036100100625
036200100625       //?Inizializzazione videata
036300100625         IF  $InzD01;
036400100625           exsr InzD01;
036500100625           $InzD01  = *off;
036600100721         //?Imposto tasti funzionali
036700100721           exsr TastiFun;
036800100625         ENDIF;
036900100721
037000100721       //?Emissione Testata
037100100721         write  TAI2T01;
037200100721       //?Emissione tasti funzionali
037300100721         write  TAI2Z01;
037400100721
037500100721       //?Emissione videata
037600100721         exfmt  TAI2D01;
037700100721         reset ErrMessage;
037800100721         reset ErrGenerico;
037900100721         clear V1Dmsg;
038000100721
038100100721         SELECT;
038200100721
038300100721       //?- F2=Rubrica
038400100721           WHEN  dsp_aid = c_F02;
038500100721             exsr F02D01;
038600100721
038700100721       //?- F3=Fine
038800100721           WHEN  dsp_aid = c_F03;
038900100721             exsr F03D01;
039000150126
039100150126           // -?F4=Campagna?
039200150126           WHEN  dsp_aid = c_F04;
039300150126             exsr  F04D01;
039400100721
039500100721       //?- F05=Attività
039600100721           WHEN  dsp_aid = c_F05;
039700100721             exsr F05D01;
039800100810             IF  ErrGenerico;
039900100810               leavesr;
040000100810             ENDIF;
040100100721
040200100721       //?- F07=Unificante
040300100721           WHEN  dsp_aid = c_F07;
040400100721             exsr F07D01;
040500100721
040600100721       //?- F12=Ritorno
040700100721           WHEN  dsp_aid = c_F12;
040800100721             exsr F12D01;
040900100721
041000100721       //?- F14=Info Commerciali
041100100721           WHEN  dsp_aid = c_F14;
041200100721             exsr F14D01;
041300110517
041400110517       //?- F15=Codici Collegati
041500110517           WHEN  dsp_aid = c_F15;
041600110517             exsr F15D01;
041700110907
041800110907       //?- F17=Dati Fatturazione
041900110907           WHEN  dsp_aid = c_F17;
042000110907             exsr F17D01;
042100100721
042200100721       //?- F18=Note
042300100721           WHEN  dsp_aid = c_F18;
042400100721             exsr F18D01;
042500100721
042600100721       //?- F20=Potenziali
042700100721           WHEN  dsp_aid = c_F20;
042800100721             exsr F20D01;
042900110407
043000110407       //?- F21=Blocco clienti
043100110407           WHEN  dsp_aid = c_F21;
043200110407             exsr F21D01;
043300100809
043400100809       //?- F22=Richiesta contatto
043500100809           WHEN  dsp_aid = c_F22;
043600100809             exsr F22D01;
043700100721
043800100721       //?- F24=Altri tasti
043900100721           WHEN  dsp_aid = c_F24 and wf24 > 1;
044000100721             exsr F24D01;
044100100721
044200100721       //?Invio
044300100721           OTHER;
044400120201         //?Se utente di Sede all'enter non deve fare niente
044500120201             IF  Sede;
044600120201               leavesr;
044700120201             ENDIF;
044800100721         //?Controllo se ci sono altre attività
044900100721             exsr AltreAtt;
045000100721             IF  ErrGenerico;
045100100721               leavesr;
045200100721             ENDIF;
045300100721        //?Se non è stato gestito da TRMK21R vado avanti con TRMK19R
045400100809             IF  ITAI2ric = 'A';
045500100809               exsr CallTrmk19;
045600100809             ENDIF;
045700100809        //?Se da trattativa vado avanti con TNTA88R
045800100809             IF  ITAI2ric = 'T';
045900100809               exsr CallTnta88;
046000100809             ENDIF;
046100100625
046200100625         ENDSL;
046300100625
046400100625       ENDSR;
046500100625
046600100625       //--------------------------------------------------------------
046700100625       //?Inizializzazione videata D01.
046800100625       //--------------------------------------------------------------
046900100625       BEGSR InzD01;
047000100625
047100100721         clear TAI2D01;
047200110407         clear wmod;
047300100721
047400100809         V1Cksc = ITAI2ksc;
047500100721
047600100721       //?Aggancio i file dell'anagrafica cliente
047700100721         clear tibs69ds;
047800100721         clear ds_cnaco;
047900100721         clear ds_cnind;
048000100721         clear ds_cnclp;
048100100721         clear ds_fncls;
048200100721         I69kac = V1Cksc;
048300100721         I69kin = V1Cksc;
048400100721         I69kcp = V1Cksc;
048500100721         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
048600100721         IF  O69err <> *blanks;
048700100721           leavesr;
048800100721         ENDIF;
048900100721
049000100721       //?Dati anagrafici
049100100721         V1Crag = ACOrag;
049200100721         V1Cvia = INDvia;
049300100721         V1Ccit = INDcit;
049400100721         V1Ccae = INDcae;
049500100721         V1Cprv = INDprv;
049600100721         V1Csta = INDsta;
049700100721         V1Ctel = INDtel;
049800100721         V1Ctlf = INDtlf;
049900100721         V1Ccdf = INDcdf;
050000100721
050100100721       //?Partita IVA
050200100721         IF  %subst(INDiva:1:2) >= '00';
050300100721           V1ivait = %subst(INDiva:1:14);
050400100721         ELSE;
050500100721           V1ivaeu = %subst(INDiva:1:2);
050600100721           V1ivait = %subst(INDiva:3:14);
050700100721         ENDIF;
050800100721
050900100721       //?Blocco cliente
051000100721         V1Cabl = ACOabl;
051100130321
051200130321       //?Se blocco cliente "7" visualizzo "8"
051300130321         IF  V1Cabl = '7';
051400130321           V1cabl = '8';
051500130321         ENDIF;
051600100721
051700100721       //?Causale blocco cliente
051800100721         V1Cblc = CLPnar;
051900110323         clear dsBI;
052000100721         clear V1Dblc;
052100100722         IF  V1Cblc <> *blanks;
052200100722           TBLcod = 'BI';
052300100722           TBLkey = V1Cblc;
052400100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
052500100722           IF  %found(TABEL00F);
052600110323             dsBI = TBLuni;
052700100722           ENDIF;
052800110323           V1Dblc = §BIdes;
052900100722         ENDIF;
053000100721
053100100721       //?Stato del credito
053200100721         V1Ccon = %subst(CLPcon:2:2);
053300100721         clear V1Dcon;
053400100722         clear ds4W;
053500100722         TBLcod = '4W';
053600170224         TBLkey = ' ' + V1Ccon;
053700100722         chain (TBLkut : TBLcod : TBLkey) TABEL;
053800100722         IF  %found(TABEL00F);
053900100722           ds4W = TBLuni;
054000100722         ENDIF;
054100100722         V1Dcon = §4wdes;
054200100721
054300100721       //?Codice potenziale
054400100721         V1Ccpo = ACOlib;
054500100722         clear V1Dcpo;
054600100722         IF  V1Ccpo <> 0;
054700100722           chain V1Ccpo TNCPO01L;
054800100722           IF  %found(TNCPO01L);
054900100722             V1Dcpo = CPOrag;
055000100722           ENDIF;
055100100721         ENDIF;
055200100721
055300100721       //?Codice commerciale
055400100721         V1Cage = CLPage;
055500100721         clear V1Dage;
055600100722         IF  V1Cage <> 0;
055700130802           chain (CLPage) AZCMM000;
055800130802           IF  %found(AZCMM01L);
055900130802             V1Dage = CMMdes;
056000100722           ENDIF;
056100100722         ENDIF;
056200100721
056300100721       //?Codice importanza cliente
056400100721         V1Cclv = CLPclv;
056500100721         clear V1Dclv;
056600100722         IF  V1Cclv <> *blanks;
056700100722           clear dsIC;
056800100722           TBLcod = 'IC';
056900100722           TBLkey = V1Cclv;
057000100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
057100100722           IF  %found(TABEL00F);
057200100722             dsIC = TBLuni;
057300100722           ENDIF;
057400100722           V1Dclv = §sicde;
057500100722         ENDIF;
057600100721
057700100721       //?Categoria merceologica
057800100721         V1Citc = ACOitc;
057900100721         clear V1Ditc;
058000100722         IF  V1Citc <> 0;
058100100722           clear ds1L;
058200100722           TBLcod = '1L';
058300100722           TBLkey = %editc(V1Citc:'X');
058400100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
058500100722           IF  %found(TABEL00F);
058600100722             ds1L = TBLuni;
058700100722           ENDIF;
058800100722           V1Ditc = §1Ldes;
058900100722         ENDIF;
059000100721
059100100721       //?Sottoconto fatturazione
059200100721         V1Cscf = CLPScf;
059300100721
059400100721       //?Data prima e ultima spedizione fattura
059500100721         %subst(wdata:5:2) = %subst(%editc(CLPdps:'X'):1:2);
059600100721         %subst(wdata:3:2) = %subst(%editc(CLPdps:'X'):3:2);
059700100721         %subst(wdata:1:2) = %subst(%editc(CLPdps:'X'):5:2);
059800100721         V1Cdps = %dec(wdata:6:0);
059900100721
060000100721         %subst(wdata:5:2) = %subst(%editc(CLPdus:'X'):1:2);
060100100721         %subst(wdata:3:2) = %subst(%editc(CLPdus:'X'):3:2);
060200100721         %subst(wdata:1:2) = %subst(%editc(CLPdus:'X'):5:2);
060300100721         V1Cdus = %dec(wdata:6:0);
060400100721
060500100721       //?Decodifico il sottoconto fatturazione
060600100722         IF  V1Cscf <> 0;
060700100722           clear tibs69ds;
060800100722           clear ds_cnaco;
060900100722           clear ds_cnind;
061000100722           clear ds_cnclp;
061100100722           clear ds_fncls;
061200100722           I69kac = V1Cscf;
061300100722           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
061400100722           V1Dscf = ACOrag;
061500100722         ENDIF;
061600110615
061700110615       //?Note DC
061800110615         k_NTCapl = 'C';
061900110615         k_NTCnk1 = %editc(ACOkcc:'X') + %editc(ACOksc:'X');
062000110615         clear k_NTCnk2;
062100110615         k_NTCtnt = 'DC';
062200110615         chain  %kds(K04tfntc) TFNTC01L;
062300110615         IF  %found(TFNTC01L);
062400110615         V1cnota = NTCrnt;
062500110615         ENDIF;
062600110615
062700100625
062800100625       ENDSR;
062900100721
063000100721       //--------------------------------------------------------------
063100100721       //?Imposto i tasti funzionali.
063200100721       //--------------------------------------------------------------
063300100721       BEGSR TastiFun;
063400100721
063500100721       //?Se presenti attività sul cliente visualizzo il tasto in PINK
063600100721         F05Pink = *off;
063700100721         chain (V1Ccpo:V1Cksc) TIATC04L;
063800100721         IF  %found(TIATC04L);
063900100721           F05Pink = *on;
064000100721         ENDIF;
064100100721
064200100721       //?Se presenti note sul cliente visualizzo il tasto in PINK
064300100721         F18Pink = *off;
064400100721         chain (V1Cksc) TICPN02L;
064500100721         IF  %found(TICPN02L);
064600100721           F18Pink = *on;
064700100721         ENDIF;
064800110502
064900110502       //?Abilito tasto F21=Blocco cliente se il cliente non è già bloccato
065000120201       //?non abilito se utente di Sede
065100110502         F21Attivo = *off;
065200120201         IF  V1Cabl = *blanks and not Sede;
065300110502           F21Attivo = *on;
065400110502         ENDIF;
065500120201
065600120201       //?Abilito tasto F22=Richiesta contatto se utente di filiale
065700120201         F22Attivo = *off;
065800120201         IF  not Sede;
065900120201           F22Attivo = *on;
066000120201         ENDIF;
066100100721
066200100721       //?Imposto la seconda riga variabile
066300100721         clear wpos;
066400100721         clear wposda;
066500100721         clear wpostf1;
066600100721         clear wpostf2;
066700100721         clear wpostf3;
066800100721         clear wrig;
066900100721         clear wtf1;
067000100721         clear wtf2;
067100100721         clear wtf3;
067200100721
067300100721         xx = 1;
067400100721         FOR xx by 1 to %elem($Tfv);
067500110502         //?Se F21 non abilitato leggo altro tasto funzione
067600110502           IF  not F21Attivo and %subst($Tfv(xx):1:3) = 'F21';
067700110502             iter;
067800110502           ENDIF;
067900120201         //?Se F22 non abilitato leggo altro tasto funzione
068000120201           IF  not F22Attivo and %subst($Tfv(xx):1:3) = 'F22';
068100120201             iter;
068200120201           ENDIF;
068300110502
068400100721           IF  $Tfv(xx) <> *blanks;
068500100721             wpos = %scan('#':$tfv(xx));
068600100721             wpostf1 += wpos;
068700100721
068800100721             IF  wpostf1 <= %len(VZFd01);
068900100721               wposda = wpostf1 - wpos + 1;
069000100721               %subst(wtf1:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
069100100721             ELSE;
069200100721               wpostf2 += wpos;
069300100721               IF  wpostf2 <= %len(VZFd01);
069400100721                 wposda = wpostf2 - wpos + 1;
069500100721                 %subst(wtf2:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
069600100721               ELSE;
069700100721                 wpostf3 += wpos;
069800100721                 IF  wpostf3 <= %len(VZFd01);
069900100721                   wposda = wpostf3 - wpos + 1;
070000100721                   %subst(wtf3:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
070100100721                 ENDIF;
070200100721               ENDIF;
070300100721             ENDIF;
070400100721
070500100721           ENDIF;
070600100721         ENDFOR;
070700100721
070800100721       //?Imposto F24 giusto
070900100721         SELECT;
071000100721           WHEN  wtf3 <> *blanks;
071100100721             VZFd02 = cf2413;
071200100721             wf24 = 3;
071300100721           WHEN  wtf2 <> *blanks;
071400110502             VZFd02 = cf2412;
071500100721             wf24 = 2;
071600100721           OTHER;
071700100721             clear VZFd02;
071800100721             wf24 = 1;
071900100721         ENDSL;
072000100721
072100100721         VZFd01 = wtf1;
072200100721         wrig = 1;
072300100721
072400100721       ENDSR;
072500141118
072600141118       //--------------------------------------------------------------
072700150126       //?Gestione tasto funzionale F04 da videata D01                 ?
072800150126       //?F4=Campagna                                                  ?
072900141118       //--------------------------------------------------------------
073000150126       BEGSR F04D01;
073100150806
073200150807         clear TRKC09DS;
073300150807         IKC09ksu = ITAI2ksc;
073400150807         trkc09r (kpjba:TRKC09DS);
073500141118
073600141118       ENDSR;
073700100721
073800100721       //--------------------------------------------------------------
073900100721       //?Gestione tasto funzionale F02 da videata D01
074000100721       //?F2=Rubrica
074100100721       //--------------------------------------------------------------
074200100721       BEGSR F02D01;
074300100721
074400100721         clear TNTA12DS;
074500100721         TA12ric = 'V';
074600100721         TA12apl = 'C';
074700100721         TA12ksc = V1Cksc;
074800100721         TA12rag = V1Crag;
074900100721
075000100721         tnta12r (kpjba : TNTA12DS);
075100100721
075200100721         IF  TA12err <> *blanks;
075300100721           ErrMessage  = *on;
075400100721           ErrGenerico = *on;
075500100721           V1Dmsg      = TA12msg;
075600100721           leavesr;
075700100721         ENDIF;
075800100721
075900100721       ENDSR;
076000100625
076100100625       //--------------------------------------------------------------
076200100625       //?Gestione tasto funzionale F03 da videata D01
076300100625       //?F3=Fine
076400100625       //--------------------------------------------------------------
076500100625       BEGSR F03D01;
076600100625
076700100625         // Chiusura del programma
076800100928         OTAI2f03 = '1';
076900100625         $Fine = *on;
077000100625
077100100625       ENDSR;
077200100721
077300100721       //--------------------------------------------------------------
077400100721       //?Gestione tasto funzionale F05 da videata D01
077500100721       //?F5=Attività
077600100721       //--------------------------------------------------------------
077700100721       BEGSR F05D01;
077800100721
077900100721         clear TRMK21DS;
078000100810         clear TRMK19DS;
078100100810         I21mod = 'V';
078200100810         I21cpo = V1Ccpo;
078300100721         I21ksc = V1Cksc;
078400100721         I21rsc = V1Crag;
078500100721         kpjbu = TRMK21DS;
078600100810         IF  ITAI2ric = 'A';
078700100810           IMK19tco = 'T';
078800100810         ENDIF;
078900100810         IF  ITAI2ric = 'T';
079000100810           IMK19tco = 'O';
079100100810         ENDIF;
079200100810         IMK19cpo  = V1Ccpo;
079300100810         IMK19ksc  = V1Cksc;
079400100810         IMK19com  = ITAI2com;
079500100810         IMK19comd = ITAI2comd;
079600120201
079700120201       //?Se utente di Sede devo SOLO visualizzare le attività
079800120201         IF  Sede;
079900120201           I21mod = 'I';
080000120201           kpjbu = TRMK21DS;
080100120201           trmk21r (kpjba);
080200120201         ELSE;
080300100810         trmk21r (kpjba : TRMK19DS);
080400120201         ENDIF;
080500120201
080600100721         TRMK21DS = kpjbu;
080700100810         IF  O21err <> *blanks;
080800100810           ErrGenerico = *on;
080900100810           ErrMessage  = *on;
081000100810           V1Dmsg      = O21msg;
081100100810           leavesr;
081200100810         ENDIF;
081300120201
081400120201        //?Se utente di Sede non devo fare + nessun controllo
081500120201         IF  Sede;
081600120201           leavesr;
081700120201         ENDIF;
081800120201
081900100810        //?Se F12 torno al dettaglio cliente
082000100810         IF  O21fxx = '12';
082100100810           ErrGenerico = *on;
082200100810         ENDIF;
082300100810        //?Se gestita attività esco dal pgm
082400100810         IF  O21fxx = '99';
082500100810           ErrGenerico = *on;
082600100810           $Fine = *on;
082700100810         ENDIF;
082800100810        //?Se non è stato gestito da TRMK21R
082900100927        //?o se fatto F10 devo andare avanti con il TRMK19R o TNTA88R
083000100927         IF  O21fxx = *blanks or O21fxx = '10';
083100100810         //?vado avanti con TRMK19R se arrivo da attività
083200100810           IF  ITAI2ric = 'A';
083300100810             exsr CallTrmk19;
083400100810           ENDIF;
083500100810         //?vado avanti con TNTA88R se arrivo da trattative
083600100810           IF  ITAI2ric = 'T';
083700100810             exsr CallTnta88;
083800100810           ENDIF;
083900100810         ENDIF;
084000100721
084100100721       ENDSR;
084200100721
084300100721       //--------------------------------------------------------------
084400100721       //?Gestione tasto funzionale F07 da videata D01
084500100721       //?F7=Unificante
084600100721       //--------------------------------------------------------------
084700100721       BEGSR F07D01;
084800100721
084900100721         clear TIBS12DS;
085000100721         K12op0 = 'P05';
085100100721         K12op1 = 'O09';
085200100721         K12f03 = '0';
085300100721         K12f12 = '0';
085400100721         K12err = '0';
085500100721         K12cop = V1Cksc;
085600100721         kpjbu = TIBS12DS;
085700100721
085800100721         tibs12r (kpjba);
085900100721
086000100721       ENDSR;
086100100721
086200100721       //--------------------------------------------------------------
086300100721       //?Gestione tasto funzionale F12 da videata D01
086400100721       //?F12=Fine
086500100721       //--------------------------------------------------------------
086600100721       BEGSR F12D01;
086700100721
086800100721         // Chiusura del programma
086900100721         $Fine = *on;
087000100721
087100100721       ENDSR;
087200100721
087300100721       //--------------------------------------------------------------
087400100721       //?Gestione tasto funzionale F14 da videata D01
087500100721       //?F14=Info Commerciali
087600100721       //--------------------------------------------------------------
087700100721       BEGSR F14D01;
087800100721
087900100721         clear TRMK50DS;
088000100721         I50cpo = V1Ccpo;
088100110407         IF  wmod = *blanks;
088200110407           I50mod = 'I';
088300110407         ELSE;
088400110407           I50mod = wmod;
088500110407         ENDIF;
088600100721         I50rag = V1Crag;
088700100721         I50pgm = 'TNTAI2R';
088800100721
088900100721         trmk50r (kpjba : TRMK50DS);
089000100721
089100100721       ENDSR;
089200110517
089300110517       //--------------------------------------------------------------
089400110517       //?Gestione tasto funzionale F15 da videata D01
089500110517       //?F15=Codici Collegati
089600110517       //--------------------------------------------------------------
089700110517       BEGSR F15D01;
089800110517
089900110517         IF  V1Cscf = *zeros;
090000110517           ErrGenerico = *on;
090100110517           ErrMessage  = *on;
090200110517           V1Dmsg      = $msg(02);
090300110517           leavesr;
090400110517         ENDIF;
090500110517
090600110517         PARMksc = %editc(V1Cscf:'X');
090700110517         kpjbu = PARMksc;
090800110517         tnta83r (kpjba);
090900110517
091000110517       ENDSR;
091100110907
091200110907       //--------------------------------------------------------------
091300110907       //?Gestione tasto funzionale F17 da videata D01
091400110907       //?F17=Dati Fatturazione
091500110907       //--------------------------------------------------------------
091600110907       BEGSR F17D01;
091700110907
091800110907         clear trul57ds;
091900110907         D57ikscb = V1Cksc;
092000110907         D57ikscf = V1Cscf;
092100110907         D57iunib = %subst(CLSflo:2:1);
092200110907         D57itftb = %editc(CLPtft:'X');
092300110907         D57ifftb = %editc(CLPfft:'X');
092400110907         D57idftb = CLPfun;
092500110907         D57ipgm  = 'TNTAI2R';
092600110907         D57isifb = INDsin;
092700110907         D57icdpb = INDcdp;
092800110907         D57iabib = INDabi;
092900110907         D57icabb = INDcab;
093000110907
093100110907         trul57r (kpjba : TRUL57DS);
093200110907
093300110907       ENDSR;
093400100721
093500100721       //--------------------------------------------------------------
093600100721       //?Gestione tasto funzionale F18 da videata D01
093700100721       //?F18=Note
093800100721       //--------------------------------------------------------------
093900100721       BEGSR F18D01;
094000100721
094100100721         clear TRMK20DS;
094200110210         IMK20tla = 'L';
094300100721         IMK20flm = 'I';
094400100721         IMK20ksc = V1Cksc;
094500100721         IMK20rsc = V1Crag;
094600100721
094700100721         trmk20r (kpjba : TRMK20DS);
094800100915
094900100915       //?Se presenti note sul cliente visualizzo il tasto in PINK
095000100915         F18Pink = *off;
095100100915         chain (V1Cksc) TICPN02L;
095200100915         IF  %found(TICPN02L);
095300100915           F18Pink = *on;
095400100915         ENDIF;
095500100721
095600100721       ENDSR;
095700100721
095800100721       //--------------------------------------------------------------
095900100721       //?Gestione tasto funzionale F20 da videata D01
096000100721       //?F20=Potenziali
096100100721       //--------------------------------------------------------------
096200100721       BEGSR F20D01;
096300100721
096400100721         clear TRMK01DS;
096500100721         MK01k10 = 'N';
096600100721         MK01cpo = V1ccpo;
096700100721         trmk02r (kpjba : TRMK01DS);
096800100721
096900100721       ENDSR;
097000110407
097100110407       //--------------------------------------------------------------
097200110407       //?Gestione tasto funzionale F21 da videata D01
097300110407       //?F21=Blocco Cliente
097400110407       //--------------------------------------------------------------
097500110407       BEGSR F21D01;
097600110407
097700110407         clear trmk17ds;
097800110407         IMK17patt = 'S';
097900110407         IMK17att  = 'T';
098000110407         IMK17pcau = 'S';
098100110407         IMK17cau  = '71';
098200110407         IMK17cpo  = V1Ccpo;
098300110407         IMK17ksc  = V1Cksc;
098400110407         IF  ITAI2com <> 0;
098500110407           IMK17co3  = ITAI2com;
098600110407         ENDIF;
098700110407
098800110407         trmk17r (kpjba : TRMK17DS);
098900110407
099000110407       //?Se ho pianificato l'attività devo richiamare la gestione
099100110407       //?delle info commerciali
099200110407         IF  OMK17f12 = *blanks and OMK17err = *blanks;
099300110407           wmod = 'G';
099400110407           exsr F14D01;
099500110407         ENDIF;
099600110407
099700110407       //?Se presenti attività sul cliente visualizzo il tasto in PINK
099800110407         F05Pink = *off;
099900110407         chain (V1Ccpo:V1Cksc) TIATC04L;
100000110407         IF  %found(TIATC04L);
100100110407           F05Pink = *on;
100200110407         ENDIF;
100300110407
100400110407       ENDSR;
100500100809
100600100809       //--------------------------------------------------------------
100700100809       //?Gestione tasto funzionale F22 da videata D01
100800100809       //?F22=Richiesta Contatto
100900100809       //--------------------------------------------------------------
101000100809       BEGSR F22D01;
101100100809
101200100809         clear trmk17ds;
101300100809         IMK17patt = 'S';
101400100809         IMK17att  = 'T';
101500100809         IMK17pcau = 'S';
101600100809         IMK17cau  = '20';
101700100809         IMK17cpo  = V1Ccpo;
101800100809         IMK17ksc  = V1Cksc;
101900100809         IF ITAI2com <> 0;
102000100809           IMK17co3  = ITAI2com;
102100100809         ENDIF;
102200100809
102300100809         trmk17r (kpjba : TRMK17DS);
102400100915
102500100915       //?Se presenti attività sul cliente visualizzo il tasto in PINK
102600100915         F05Pink = *off;
102700100915         chain (V1Ccpo:V1Cksc) TIATC04L;
102800100915         IF  %found(TIATC04L);
102900100915           F05Pink = *on;
103000100915         ENDIF;
103100100809
103200100809       ENDSR;
103300100721
103400100721       //--------------------------------------------------------------
103500100721       //?Gestione tasto funzionale F24 da videata D01
103600100721       //?F24=Altri Tasti
103700100721       //--------------------------------------------------------------
103800100721       BEGSR F24D01;
103900100721
104000100721         SELECT;
104100100721           WHEN  (wf24 = 3 and wrig = 3) or
104200100721                 (wf24 = 2 and wrig = 2);
104300100721             VZFd01 = wtf1;
104400100721             wrig = 1;
104500100721             IF  wf24 = 2;
104600100721               VZFd02 = cf2412;
104700100721             ELSE;
104800100721               VZFd02 = cf2413;
104900100721             ENDIF;
105000100721
105100100721           WHEN  (wf24 = 3 and wrig = 1) or
105200100721                 (wf24 = 2 and wrig = 1);
105300100721             VZFd01 = wtf2;
105400100721             wrig = 2;
105500100721             IF  wf24 = 2;
105600100721               VZFd02 = cf2422;
105700100721             ELSE;
105800100721               VZFd02 = cf2423;
105900100721             ENDIF;
106000100721
106100100721           WHEN  wf24 = 3 and wrig = 2;
106200100721             VZFd01 = wtf3;
106300100721             wrig = 3;
106400100721             VZFd02 = cf2433;
106500100721
106600100721         ENDSL;
106700100721
106800100721       ENDSR;
106900100625
107000100625       //--------------------------------------------------------------
107100100625       //?Controllo videata D01.
107200100625       //--------------------------------------------------------------
107300100625       BEGSR CtrD01;
107400100625
107500100625         WindDspF = IndDspF;
107600100625         reset WindDspFb;
107700100625         IndDspF  = WindDspF;
107800100625
107900100625       ENDSR;
108000100624
108100100624       //--------------------------------------------------------------
108200100624       //?Controllo se ci sono altre attività sul cliente.
108300100624       //--------------------------------------------------------------
108400100624       BEGSR AltreAtt;
108500100624
108600100624         IF  F05Pink;
108700100624           clear TRMK21DS;
108800100722           clear TRMK19DS;
108900100624           I21mod = 'G';
109000100722           I21cpo = V1Ccpo;
109100100721           I21ksc = V1Cksc;
109200100721           I21rsc = V1Crag;
109300100624           kpjbu  = TRMK21ds;
109400100809           IF  ITAI2ric = 'A';
109500100809             IMK19tco = 'T';
109600100809           ENDIF;
109700100809           IF  ITAI2ric = 'T';
109800100809             IMK19tco = 'O';
109900100809           ENDIF;
110000100722           IMK19cpo  = V1Ccpo;
110100100722           IMK19ksc  = V1Cksc;
110200100722           IMK19com  = ITAI2com;
110300100722           IMK19comd = ITAI2comd;
110400100624           trmk21r (kpjba : TRMK19DS);
110500100624           TRMK21DS = kpjbu;
110600100722           IF  O21err <> *blanks;
110700100722             ErrGenerico = *on;
110800100722             ErrMessage  = *on;
110900100722             V1Dmsg      = O21msg;
111000100722           ENDIF;
111100100722           IF  O21fxx = '12';
111200100722             ErrGenerico = *on;
111300100722           ENDIF;
111400100624           IF  O21fxx = '99';
111500100624             ErrGenerico = *on;
111600100722             $Fine = *on;
111700100624           ENDIF;
111800100624
111900100624         ENDIF;
112000100624
112100100624       ENDSR;
112200100624
112300100624       //--------------------------------------------------------------
112400100624       //?Richiamo TRMK19R per new attività.
112500100624       //--------------------------------------------------------------
112600100624       BEGSR CallTrmk19;
112700100624
112800100722         clear TRMK19ds;
112900100722         IMK19cmt  = 'S';
113000100722         IMK19fle  = 'C';
113100100722         IMK19tco  = 'T';
113200170206         IMK19tat = 'T';
113300100722         IMK19cpo  = V1Ccpo;
113400100722         IMK19ksc  = V1Cksc;
113500100722         IMK19com  = ITAI2com;
113600100722         IMK19comd = ITAI2comd;
113700100722
113800100722         trmk19r ( kpjba: TRMK19DS );
113900100810       //?se non torna errore
114000100722       //?esco dal programma
114100100722         IF  OMK19Err = *blanks;
114200100722           $Fine = *on;
114300100722         ENDIF;
114400100810       //?se torna errore con relativo messaggio emetto il messaggio
114500100810         IF  OMK19Err <> *blanks and OMK19msg <> *blanks;
114600100810           ErrGenerico = *on;
114700100810           ErrMessage  = *on;
114800100810           V1Dmsg      = OMK19msg;
114900100810         ENDIF;
115000100810       //?se torna errore senza messaggio esco dal programma
115100100810       //?(in questo caso errore vale per F12 da TRMK19R)
115200100810         IF  OMK19Err <> *blanks and OMK19msg = *blanks;
115300100810           $Fine = *on;
115400100810         ENDIF;
115500100624
115600100624       ENDSR;
115700100809
115800100809       //--------------------------------------------------------------
115900100809       //?Richiamo TNTA88R per new trattativa.
116000100809       //--------------------------------------------------------------
116100100809       BEGSR CallTnta88;
116200100809
116300100809         clear TNTA88ds;
116400100809         ITA88cmt  = 'S';
116500100809         ITA88fle  = 'C';
116600100809         ITA88cpo  = V1Ccpo;
116700100809         ITA88ksc  = V1Cksc;
116800100809         ITA88com  = ITAI2com;
116900100809         ITA88comd = ITAI2comd;
117000100809
117100100809         tnta88r ( kpjba: TNTA88DS );
117200100809       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
117300100809       //?esco dal programma
117400100809         IF  OTA88Err = *blanks;
117500100809           $Fine = *on;
117600100809         ENDIF;
117700100809
117800100809       ENDSR;
117900080206
118000080206       //--------------------------------------------------------------
118100080206       //?Operazioni finali.
118200080206       //--------------------------------------------------------------
118300080206       BEGSR RoutEnd;
118400100721
118500100721        //?Se errore torno il dato al chiamante
118600100721         IF  $ErrGrave;
118700100721           OTAI2err = '1';
118800100721           OTAI2msg = V1Dmsg;
118900100721         ENDIF;
119000080206
119100080206         *inLR = *on;
119200080206         return;
119300080206
119400080206       ENDSR;
119500080206
119600080206      /end-free
119700100624
119800080206       //--------------------------------------------------------------
119900080206       //?Schiere a tempo di compilazione.
120000080206       //--------------------------------------------------------------
120100080206
120200080206** - $MSG -------------------------------------------------------------------*
120300100721Codice cliente non passato                                                     1
120400110517Codice sottoconto fatturazione non impostato interrogazione non possibile      2
120500100721
120600100721** - $Tfv (in ordine di visualizzazione) ------------------------------------*
120700100809F22=Richiesta Contatto   #
120800100721F14=InfoCommerciali   #
120900110517F15=Codici Collegati   #
121000110907F17=Dati Fatturazione   #
121100100721F20=Potenziali   #
121200110407F21=Blocco Cliente  #
121300110615F12=Ritorno   #
