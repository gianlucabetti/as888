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
024900080206
025000080626      //---------------------------------------------------------------
025100080626      //?prototipi
025200080626      //---------------------------------------------------------------
025300080626      /copy gaitrasrc/srcprotopr,tibs34r
025400100428      /copy gaitrasrc/srcprotopr,tibs69r
025500100721      /copy gaitrasrc/srcprotopr,tnta12r
025600100809      /copy gaitrasrc/srcprotopr,tnta88r
025700100624      /copy gaitrasrc/srcprotopr,trmk19r
025800100624      /copy gaitrasrc/srcprotopr,trmk20r
025900100721      /copy gaitrasrc/srcprotopr,trmk50r
026000100624
026100080206      //---------------------------------------------------------------
026200080206      //?Definizione key-list.
026300080206      //---------------------------------------------------------------
026400110615
026500110615       // - File TFNTC01L
026600110615     d k04tfntc      e ds                  extname(TFNTC01L:*key)
026700110615     d                                     prefix(k_)
026800080206
026900080206      //---------------------------------------------------------------
027000080206      //?Riepilogo indicatori.
027100080206      //---------------------------------------------------------------
027200080207      // 28    : Emissione messaggio di errore a video
027300100624      // 40    : F05 Pink
027400100624      // 41    : F18 Pink
027500100721      // 50    : Errore:
027600080207      // 99    : Generico di Errore
027700080206      //---------------------------------------------------------------
027800080206
027900080206      //---------------------------------------------------------------
028000080206      //?M A I N - L I N E
028100080206      //---------------------------------------------------------------
028200080206
028300080206     c     *Entry        plist
028400080206     c                   parm                    KPJBA
028500100721     c                   parm                    TNTAI2DS
028600080206
028700080206      /free
028800080206
028900100622       //?Operazioni iniziali
029000080206       exsr RoutInz;
029100080206
029200100622       //?Gestione video
029300080206       DOW $Fine = *off;
029400100624         SELECT;
029500100625           WHEN $Video = 'D1';
029600100625             exsr GesD01;
029700100624           OTHER;
029800080206             $Fine = *on;
029900100624         ENDSL;
030000080206       ENDDO;
030100080206
030200100622       //?Operazioni finali
030300080206       exsr RoutEnd;
030400080206
030500080206       //--------------------------------------------------------------
030600080206       //?Operazioni iniziali.
030700080206       //--------------------------------------------------------------
030800080206       BEGSR RoutInz;
030900100427
031000100622         exec sql
031100100622          set option dynusrprf = *owner, closqlcsr = *endmod;
031200080206
031300100622       //?Impostazione campi "fissi"
031400080208         T01pgm = SDSpgm;
031500080208         TBLkut = 1;
031600080206
031700100622       //?Reperimento dati job
031800080206         exsr DatiJob;
031900100721
032000100721       //?Se non passato il cliente torno errore
032100100721         IF  ITAI2ksc = 0;
032200100721           $ErrGrave = *on;
032300100721           $Fine     = *on;
032400100721           V1Dmsg    = $Msg(01);
032500100721           leavesr;
032600100721         ENDIF;
032700120201
032800120201       //?Controllo se utente di Sede
032900120201         Sede = (DutPou = 046);
033000090714
033100080206       ENDSR;
033200080206
033300080206       //--------------------------------------------------------------
033400080206       //?Reperimento Dati del job (Utente/Operativi).
033500080206       //--------------------------------------------------------------
033600080206       BEGSR DatiJob;
033700080206
033800080206         in(E) §AzUte;
033900080206         if NOT %error;
034000080206           in(E) §DatiUte;
034100080206         endif;
034200080206         if %error or RSut = *blanks;
034300080206           clear TIBS34ds;
034400080206           tibs34r(tibs34ds);
034500080206           in §AzUte;
034600080206           in §DatiUte;
034700080206         endif;
034800080206
034900080206       ENDSR;
035000100625
035100100625       //--------------------------------------------------------------
035200100625       //?Gestione videata D01.
035300100625       //--------------------------------------------------------------
035400100625       BEGSR GesD01;
035500100625
035600100625       //?Inizializzazione videata
035700100625         IF  $InzD01;
035800100625           exsr InzD01;
035900100625           $InzD01  = *off;
036000100721         //?Imposto tasti funzionali
036100100721           exsr TastiFun;
036200100625         ENDIF;
036300100721
036400100721       //?Emissione Testata
036500100721         write  TAI2T01;
036600100721       //?Emissione tasti funzionali
036700100721         write  TAI2Z01;
036800100721
036900100721       //?Emissione videata
037000100721         exfmt  TAI2D01;
037100100721         reset ErrMessage;
037200100721         reset ErrGenerico;
037300100721         clear V1Dmsg;
037400100721
037500100721         SELECT;
037600100721
037700100721       //?- F2=Rubrica
037800100721           WHEN  dsp_aid = c_F02;
037900100721             exsr F02D01;
038000100721
038100100721       //?- F3=Fine
038200100721           WHEN  dsp_aid = c_F03;
038300100721             exsr F03D01;
038400100721
038500100721       //?- F05=Attività
038600100721           WHEN  dsp_aid = c_F05;
038700100721             exsr F05D01;
038800100810             IF  ErrGenerico;
038900100810               leavesr;
039000100810             ENDIF;
039100100721
039200100721       //?- F07=Unificante
039300100721           WHEN  dsp_aid = c_F07;
039400100721             exsr F07D01;
039500100721
039600100721       //?- F12=Ritorno
039700100721           WHEN  dsp_aid = c_F12;
039800100721             exsr F12D01;
039900100721
040000100721       //?- F14=Info Commerciali
040100100721           WHEN  dsp_aid = c_F14;
040200100721             exsr F14D01;
040300110517
040400110517       //?- F15=Codici Collegati
040500110517           WHEN  dsp_aid = c_F15;
040600110517             exsr F15D01;
040700110907
040800110907       //?- F17=Dati Fatturazione
040900110907           WHEN  dsp_aid = c_F17;
041000110907             exsr F17D01;
041100100721
041200100721       //?- F18=Note
041300100721           WHEN  dsp_aid = c_F18;
041400100721             exsr F18D01;
041500100721
041600100721       //?- F20=Potenziali
041700100721           WHEN  dsp_aid = c_F20;
041800100721             exsr F20D01;
041900110407
042000110407       //?- F21=Blocco clienti
042100110407           WHEN  dsp_aid = c_F21;
042200110407             exsr F21D01;
042300100809
042400100809       //?- F22=Richiesta contatto
042500100809           WHEN  dsp_aid = c_F22;
042600100809             exsr F22D01;
042700100721
042800100721       //?- F24=Altri tasti
042900100721           WHEN  dsp_aid = c_F24 and wf24 > 1;
043000100721             exsr F24D01;
043100100721
043200100721       //?Invio
043300100721           OTHER;
043400120201         //?Se utente di Sede all'enter non deve fare niente
043500120201             IF  Sede;
043600120201               leavesr;
043700120201             ENDIF;
043800100721         //?Controllo se ci sono altre attività
043900100721             exsr AltreAtt;
044000100721             IF  ErrGenerico;
044100100721               leavesr;
044200100721             ENDIF;
044300100721        //?Se non è stato gestito da TRMK21R vado avanti con TRMK19R
044400100809             IF  ITAI2ric = 'A';
044500100809               exsr CallTrmk19;
044600100809             ENDIF;
044700100809        //?Se da trattativa vado avanti con TNTA88R
044800100809             IF  ITAI2ric = 'T';
044900100809               exsr CallTnta88;
045000100809             ENDIF;
045100100625
045200100625         ENDSL;
045300100625
045400100625       ENDSR;
045500100625
045600100625       //--------------------------------------------------------------
045700100625       //?Inizializzazione videata D01.
045800100625       //--------------------------------------------------------------
045900100625       BEGSR InzD01;
046000100625
046100100721         clear TAI2D01;
046200110407         clear wmod;
046300100721
046400100809         V1Cksc = ITAI2ksc;
046500100721
046600100721       //?Aggancio i file dell'anagrafica cliente
046700100721         clear tibs69ds;
046800100721         clear ds_cnaco;
046900100721         clear ds_cnind;
047000100721         clear ds_cnclp;
047100100721         clear ds_fncls;
047200100721         I69kac = V1Cksc;
047300100721         I69kin = V1Cksc;
047400100721         I69kcp = V1Cksc;
047500100721         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
047600100721         IF  O69err <> *blanks;
047700100721           leavesr;
047800100721         ENDIF;
047900100721
048000100721       //?Dati anagrafici
048100100721         V1Crag = ACOrag;
048200100721         V1Cvia = INDvia;
048300100721         V1Ccit = INDcit;
048400100721         V1Ccae = INDcae;
048500100721         V1Cprv = INDprv;
048600100721         V1Csta = INDsta;
048700100721         V1Ctel = INDtel;
048800100721         V1Ctlf = INDtlf;
048900100721         V1Ccdf = INDcdf;
049000100721
049100100721       //?Partita IVA
049200100721         IF  %subst(INDiva:1:2) >= '00';
049300100721           V1ivait = %subst(INDiva:1:14);
049400100721         ELSE;
049500100721           V1ivaeu = %subst(INDiva:1:2);
049600100721           V1ivait = %subst(INDiva:3:14);
049700100721         ENDIF;
049800100721
049900100721       //?Blocco cliente
050000100721         V1Cabl = ACOabl;
050100130321
050200130321       //?Se blocco cliente "7" visualizzo "8"
050300130321         IF  V1Cabl = '7';
050400130321           V1cabl = '8';
050500130321         ENDIF;
050600100721
050700100721       //?Causale blocco cliente
050800100721         V1Cblc = CLPnar;
050900110323         clear dsBI;
051000100721         clear V1Dblc;
051100100722         IF  V1Cblc <> *blanks;
051200100722           TBLcod = 'BI';
051300100722           TBLkey = V1Cblc;
051400100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
051500100722           IF  %found(TABEL00F);
051600110323             dsBI = TBLuni;
051700100722           ENDIF;
051800110323           V1Dblc = §BIdes;
051900100722         ENDIF;
052000100721
052100100721       //?Stato del credito
052200100721         V1Ccon = %subst(CLPcon:2:2);
052300100721         clear V1Dcon;
052400100722         clear ds4W;
052500100722         TBLcod = '4W';
052600100722         TBLkey = V1Ccon;
052700100722         chain (TBLkut : TBLcod : TBLkey) TABEL;
052800100722         IF  %found(TABEL00F);
052900100722           ds4W = TBLuni;
053000100722         ENDIF;
053100100722         V1Dcon = §4wdes;
053200100721
053300100721       //?Codice potenziale
053400100721         V1Ccpo = ACOlib;
053500100722         clear V1Dcpo;
053600100722         IF  V1Ccpo <> 0;
053700100722           chain V1Ccpo TNCPO01L;
053800100722           IF  %found(TNCPO01L);
053900100722             V1Dcpo = CPOrag;
054000100722           ENDIF;
054100100721         ENDIF;
054200100721
054300100721       //?Codice commerciale
054400100721         V1Cage = CLPage;
054500100721         clear V1Dage;
054600100722         IF  V1Cage <> 0;
054700130802           chain (CLPage) AZCMM000;
054800130802           IF  %found(AZCMM01L);
054900130802             V1Dage = CMMdes;
055000100722           ENDIF;
055100100722         ENDIF;
055200100721
055300100721       //?Codice importanza cliente
055400100721         V1Cclv = CLPclv;
055500100721         clear V1Dclv;
055600100722         IF  V1Cclv <> *blanks;
055700100722           clear dsIC;
055800100722           TBLcod = 'IC';
055900100722           TBLkey = V1Cclv;
056000100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
056100100722           IF  %found(TABEL00F);
056200100722             dsIC = TBLuni;
056300100722           ENDIF;
056400100722           V1Dclv = §sicde;
056500100722         ENDIF;
056600100721
056700100721       //?Categoria merceologica
056800100721         V1Citc = ACOitc;
056900100721         clear V1Ditc;
057000100722         IF  V1Citc <> 0;
057100100722           clear ds1L;
057200100722           TBLcod = '1L';
057300100722           TBLkey = %editc(V1Citc:'X');
057400100722           chain (TBLkut : TBLcod : TBLkey) TABEL;
057500100722           IF  %found(TABEL00F);
057600100722             ds1L = TBLuni;
057700100722           ENDIF;
057800100722           V1Ditc = §1Ldes;
057900100722         ENDIF;
058000100721
058100100721       //?Sottoconto fatturazione
058200100721         V1Cscf = CLPScf;
058300100721
058400100721       //?Data prima e ultima spedizione fattura
058500100721         %subst(wdata:5:2) = %subst(%editc(CLPdps:'X'):1:2);
058600100721         %subst(wdata:3:2) = %subst(%editc(CLPdps:'X'):3:2);
058700100721         %subst(wdata:1:2) = %subst(%editc(CLPdps:'X'):5:2);
058800100721         V1Cdps = %dec(wdata:6:0);
058900100721
059000100721         %subst(wdata:5:2) = %subst(%editc(CLPdus:'X'):1:2);
059100100721         %subst(wdata:3:2) = %subst(%editc(CLPdus:'X'):3:2);
059200100721         %subst(wdata:1:2) = %subst(%editc(CLPdus:'X'):5:2);
059300100721         V1Cdus = %dec(wdata:6:0);
059400100721
059500100721       //?Decodifico il sottoconto fatturazione
059600100722         IF  V1Cscf <> 0;
059700100722           clear tibs69ds;
059800100722           clear ds_cnaco;
059900100722           clear ds_cnind;
060000100722           clear ds_cnclp;
060100100722           clear ds_fncls;
060200100722           I69kac = V1Cscf;
060300100722           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
060400100722           V1Dscf = ACOrag;
060500100722         ENDIF;
060600110615
060700110615       //?Note DC
060800110615         k_NTCapl = 'C';
060900110615         k_NTCnk1 = %editc(ACOkcc:'X') + %editc(ACOksc:'X');
061000110615         clear k_NTCnk2;
061100110615         k_NTCtnt = 'DC';
061200110615         chain  %kds(K04tfntc) TFNTC01L;
061300110615         IF  %found(TFNTC01L);
061400110615         V1cnota = NTCrnt;
061500110615         ENDIF;
061600110615
061700100625
061800100625       ENDSR;
061900100721
062000100721       //--------------------------------------------------------------
062100100721       //?Imposto i tasti funzionali.
062200100721       //--------------------------------------------------------------
062300100721       BEGSR TastiFun;
062400100721
062500100721       //?Se presenti attività sul cliente visualizzo il tasto in PINK
062600100721         F05Pink = *off;
062700100721         chain (V1Ccpo:V1Cksc) TIATC04L;
062800100721         IF  %found(TIATC04L);
062900100721           F05Pink = *on;
063000100721         ENDIF;
063100100721
063200100721       //?Se presenti note sul cliente visualizzo il tasto in PINK
063300100721         F18Pink = *off;
063400100721         chain (V1Cksc) TICPN02L;
063500100721         IF  %found(TICPN02L);
063600100721           F18Pink = *on;
063700100721         ENDIF;
063800110502
063900110502       //?Abilito tasto F21=Blocco cliente se il cliente non è già bloccato
064000120201       //?non abilito se utente di Sede
064100110502         F21Attivo = *off;
064200120201         IF  V1Cabl = *blanks and not Sede;
064300110502           F21Attivo = *on;
064400110502         ENDIF;
064500120201
064600120201       //?Abilito tasto F22=Richiesta contatto se utente di filiale
064700120201         F22Attivo = *off;
064800120201         IF  not Sede;
064900120201           F22Attivo = *on;
065000120201         ENDIF;
065100100721
065200100721       //?Imposto la seconda riga variabile
065300100721         clear wpos;
065400100721         clear wposda;
065500100721         clear wpostf1;
065600100721         clear wpostf2;
065700100721         clear wpostf3;
065800100721         clear wrig;
065900100721         clear wtf1;
066000100721         clear wtf2;
066100100721         clear wtf3;
066200100721
066300100721         xx = 1;
066400100721         FOR xx by 1 to %elem($Tfv);
066500110502         //?Se F21 non abilitato leggo altro tasto funzione
066600110502           IF  not F21Attivo and %subst($Tfv(xx):1:3) = 'F21';
066700110502             iter;
066800110502           ENDIF;
066900120201         //?Se F22 non abilitato leggo altro tasto funzione
067000120201           IF  not F22Attivo and %subst($Tfv(xx):1:3) = 'F22';
067100120201             iter;
067200120201           ENDIF;
067300110502
067400100721           IF  $Tfv(xx) <> *blanks;
067500100721             wpos = %scan('#':$tfv(xx));
067600100721             wpostf1 += wpos;
067700100721
067800100721             IF  wpostf1 <= %len(VZFd01);
067900100721               wposda = wpostf1 - wpos + 1;
068000100721               %subst(wtf1:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
068100100721             ELSE;
068200100721               wpostf2 += wpos;
068300100721               IF  wpostf2 <= %len(VZFd01);
068400100721                 wposda = wpostf2 - wpos + 1;
068500100721                 %subst(wtf2:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
068600100721               ELSE;
068700100721                 wpostf3 += wpos;
068800100721                 IF  wpostf3 <= %len(VZFd01);
068900100721                   wposda = wpostf3 - wpos + 1;
069000100721                   %subst(wtf3:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
069100100721                 ENDIF;
069200100721               ENDIF;
069300100721             ENDIF;
069400100721
069500100721           ENDIF;
069600100721         ENDFOR;
069700100721
069800100721       //?Imposto F24 giusto
069900100721         SELECT;
070000100721           WHEN  wtf3 <> *blanks;
070100100721             VZFd02 = cf2413;
070200100721             wf24 = 3;
070300100721           WHEN  wtf2 <> *blanks;
070400110502             VZFd02 = cf2412;
070500100721             wf24 = 2;
070600100721           OTHER;
070700100721             clear VZFd02;
070800100721             wf24 = 1;
070900100721         ENDSL;
071000100721
071100100721         VZFd01 = wtf1;
071200100721         wrig = 1;
071300100721
071400100721       ENDSR;
071500100721
071600100721       //--------------------------------------------------------------
071700100721       //?Gestione tasto funzionale F02 da videata D01
071800100721       //?F2=Rubrica
071900100721       //--------------------------------------------------------------
072000100721       BEGSR F02D01;
072100100721
072200100721         clear TNTA12DS;
072300100721         TA12ric = 'V';
072400100721         TA12apl = 'C';
072500100721         TA12ksc = V1Cksc;
072600100721         TA12rag = V1Crag;
072700100721
072800100721         tnta12r (kpjba : TNTA12DS);
072900100721
073000100721         IF  TA12err <> *blanks;
073100100721           ErrMessage  = *on;
073200100721           ErrGenerico = *on;
073300100721           V1Dmsg      = TA12msg;
073400100721           leavesr;
073500100721         ENDIF;
073600100721
073700100721       ENDSR;
073800100625
073900100625       //--------------------------------------------------------------
074000100625       //?Gestione tasto funzionale F03 da videata D01
074100100625       //?F3=Fine
074200100625       //--------------------------------------------------------------
074300100625       BEGSR F03D01;
074400100625
074500100625         // Chiusura del programma
074600100928         OTAI2f03 = '1';
074700100625         $Fine = *on;
074800100625
074900100625       ENDSR;
075000100721
075100100721       //--------------------------------------------------------------
075200100721       //?Gestione tasto funzionale F05 da videata D01
075300100721       //?F5=Attività
075400100721       //--------------------------------------------------------------
075500100721       BEGSR F05D01;
075600100721
075700100721         clear TRMK21DS;
075800100810         clear TRMK19DS;
075900100810         I21mod = 'V';
076000100810         I21cpo = V1Ccpo;
076100100721         I21ksc = V1Cksc;
076200100721         I21rsc = V1Crag;
076300100721         kpjbu = TRMK21DS;
076400100810         IF  ITAI2ric = 'A';
076500100810           IMK19tco = 'T';
076600100810         ENDIF;
076700100810         IF  ITAI2ric = 'T';
076800100810           IMK19tco = 'O';
076900100810         ENDIF;
077000100810         IMK19cpo  = V1Ccpo;
077100100810         IMK19ksc  = V1Cksc;
077200100810         IMK19com  = ITAI2com;
077300100810         IMK19comd = ITAI2comd;
077400120201
077500120201       //?Se utente di Sede devo SOLO visualizzare le attività
077600120201         IF  Sede;
077700120201           I21mod = 'I';
077800120201           kpjbu = TRMK21DS;
077900120201           trmk21r (kpjba);
078000120201         ELSE;
078100100810         trmk21r (kpjba : TRMK19DS);
078200120201         ENDIF;
078300120201
078400100721         TRMK21DS = kpjbu;
078500100810         IF  O21err <> *blanks;
078600100810           ErrGenerico = *on;
078700100810           ErrMessage  = *on;
078800100810           V1Dmsg      = O21msg;
078900100810           leavesr;
079000100810         ENDIF;
079100120201
079200120201        //?Se utente di Sede non devo fare + nessun controllo
079300120201         IF  Sede;
079400120201           leavesr;
079500120201         ENDIF;
079600120201
079700100810        //?Se F12 torno al dettaglio cliente
079800100810         IF  O21fxx = '12';
079900100810           ErrGenerico = *on;
080000100810         ENDIF;
080100100810        //?Se gestita attività esco dal pgm
080200100810         IF  O21fxx = '99';
080300100810           ErrGenerico = *on;
080400100810           $Fine = *on;
080500100810         ENDIF;
080600100810        //?Se non è stato gestito da TRMK21R
080700100927        //?o se fatto F10 devo andare avanti con il TRMK19R o TNTA88R
080800100927         IF  O21fxx = *blanks or O21fxx = '10';
080900100810         //?vado avanti con TRMK19R se arrivo da attività
081000100810           IF  ITAI2ric = 'A';
081100100810             exsr CallTrmk19;
081200100810           ENDIF;
081300100810         //?vado avanti con TNTA88R se arrivo da trattative
081400100810           IF  ITAI2ric = 'T';
081500100810             exsr CallTnta88;
081600100810           ENDIF;
081700100810         ENDIF;
081800100721
081900100721       ENDSR;
082000100721
082100100721       //--------------------------------------------------------------
082200100721       //?Gestione tasto funzionale F07 da videata D01
082300100721       //?F7=Unificante
082400100721       //--------------------------------------------------------------
082500100721       BEGSR F07D01;
082600100721
082700100721         clear TIBS12DS;
082800100721         K12op0 = 'P05';
082900100721         K12op1 = 'O09';
083000100721         K12f03 = '0';
083100100721         K12f12 = '0';
083200100721         K12err = '0';
083300100721         K12cop = V1Cksc;
083400100721         kpjbu = TIBS12DS;
083500100721
083600100721         tibs12r (kpjba);
083700100721
083800100721       ENDSR;
083900100721
084000100721       //--------------------------------------------------------------
084100100721       //?Gestione tasto funzionale F12 da videata D01
084200100721       //?F12=Fine
084300100721       //--------------------------------------------------------------
084400100721       BEGSR F12D01;
084500100721
084600100721         // Chiusura del programma
084700100721         $Fine = *on;
084800100721
084900100721       ENDSR;
085000100721
085100100721       //--------------------------------------------------------------
085200100721       //?Gestione tasto funzionale F14 da videata D01
085300100721       //?F14=Info Commerciali
085400100721       //--------------------------------------------------------------
085500100721       BEGSR F14D01;
085600100721
085700100721         clear TRMK50DS;
085800100721         I50cpo = V1Ccpo;
085900110407         IF  wmod = *blanks;
086000110407           I50mod = 'I';
086100110407         ELSE;
086200110407           I50mod = wmod;
086300110407         ENDIF;
086400100721         I50rag = V1Crag;
086500100721         I50pgm = 'TNTAI2R';
086600100721
086700100721         trmk50r (kpjba : TRMK50DS);
086800100721
086900100721       ENDSR;
087000110517
087100110517       //--------------------------------------------------------------
087200110517       //?Gestione tasto funzionale F15 da videata D01
087300110517       //?F15=Codici Collegati
087400110517       //--------------------------------------------------------------
087500110517       BEGSR F15D01;
087600110517
087700110517         IF  V1Cscf = *zeros;
087800110517           ErrGenerico = *on;
087900110517           ErrMessage  = *on;
088000110517           V1Dmsg      = $msg(02);
088100110517           leavesr;
088200110517         ENDIF;
088300110517
088400110517         PARMksc = %editc(V1Cscf:'X');
088500110517         kpjbu = PARMksc;
088600110517         tnta83r (kpjba);
088700110517
088800110517       ENDSR;
088900110907
089000110907       //--------------------------------------------------------------
089100110907       //?Gestione tasto funzionale F17 da videata D01
089200110907       //?F17=Dati Fatturazione
089300110907       //--------------------------------------------------------------
089400110907       BEGSR F17D01;
089500110907
089600110907         clear trul57ds;
089700110907         D57ikscb = V1Cksc;
089800110907         D57ikscf = V1Cscf;
089900110907         D57iunib = %subst(CLSflo:2:1);
090000110907         D57itftb = %editc(CLPtft:'X');
090100110907         D57ifftb = %editc(CLPfft:'X');
090200110907         D57idftb = CLPfun;
090300110907         D57ipgm  = 'TNTAI2R';
090400110907         D57isifb = INDsin;
090500110907         D57icdpb = INDcdp;
090600110907         D57iabib = INDabi;
090700110907         D57icabb = INDcab;
090800110907
090900110907         trul57r (kpjba : TRUL57DS);
091000110907
091100110907       ENDSR;
091200100721
091300100721       //--------------------------------------------------------------
091400100721       //?Gestione tasto funzionale F18 da videata D01
091500100721       //?F18=Note
091600100721       //--------------------------------------------------------------
091700100721       BEGSR F18D01;
091800100721
091900100721         clear TRMK20DS;
092000110210         IMK20tla = 'L';
092100100721         IMK20flm = 'I';
092200100721         IMK20ksc = V1Cksc;
092300100721         IMK20rsc = V1Crag;
092400100721
092500100721         trmk20r (kpjba : TRMK20DS);
092600100915
092700100915       //?Se presenti note sul cliente visualizzo il tasto in PINK
092800100915         F18Pink = *off;
092900100915         chain (V1Cksc) TICPN02L;
093000100915         IF  %found(TICPN02L);
093100100915           F18Pink = *on;
093200100915         ENDIF;
093300100721
093400100721       ENDSR;
093500100721
093600100721       //--------------------------------------------------------------
093700100721       //?Gestione tasto funzionale F20 da videata D01
093800100721       //?F20=Potenziali
093900100721       //--------------------------------------------------------------
094000100721       BEGSR F20D01;
094100100721
094200100721         clear TRMK01DS;
094300100721         MK01k10 = 'N';
094400100721         MK01cpo = V1ccpo;
094500100721         trmk02r (kpjba : TRMK01DS);
094600100721
094700100721       ENDSR;
094800110407
094900110407       //--------------------------------------------------------------
095000110407       //?Gestione tasto funzionale F21 da videata D01
095100110407       //?F21=Blocco Cliente
095200110407       //--------------------------------------------------------------
095300110407       BEGSR F21D01;
095400110407
095500110407         clear trmk17ds;
095600110407         IMK17patt = 'S';
095700110407         IMK17att  = 'T';
095800110407         IMK17pcau = 'S';
095900110407         IMK17cau  = '71';
096000110407         IMK17cpo  = V1Ccpo;
096100110407         IMK17ksc  = V1Cksc;
096200110407         IF  ITAI2com <> 0;
096300110407           IMK17co3  = ITAI2com;
096400110407         ENDIF;
096500110407
096600110407         trmk17r (kpjba : TRMK17DS);
096700110407
096800110407       //?Se ho pianificato l'attività devo richiamare la gestione
096900110407       //?delle info commerciali
097000110407         IF  OMK17f12 = *blanks and OMK17err = *blanks;
097100110407           wmod = 'G';
097200110407           exsr F14D01;
097300110407         ENDIF;
097400110407
097500110407       //?Se presenti attività sul cliente visualizzo il tasto in PINK
097600110407         F05Pink = *off;
097700110407         chain (V1Ccpo:V1Cksc) TIATC04L;
097800110407         IF  %found(TIATC04L);
097900110407           F05Pink = *on;
098000110407         ENDIF;
098100110407
098200110407       ENDSR;
098300100809
098400100809       //--------------------------------------------------------------
098500100809       //?Gestione tasto funzionale F22 da videata D01
098600100809       //?F22=Richiesta Contatto
098700100809       //--------------------------------------------------------------
098800100809       BEGSR F22D01;
098900100809
099000100809         clear trmk17ds;
099100100809         IMK17patt = 'S';
099200100809         IMK17att  = 'T';
099300100809         IMK17pcau = 'S';
099400100809         IMK17cau  = '20';
099500100809         IMK17cpo  = V1Ccpo;
099600100809         IMK17ksc  = V1Cksc;
099700100809         IF ITAI2com <> 0;
099800100809           IMK17co3  = ITAI2com;
099900100809         ENDIF;
100000100809
100100100809         trmk17r (kpjba : TRMK17DS);
100200100915
100300100915       //?Se presenti attività sul cliente visualizzo il tasto in PINK
100400100915         F05Pink = *off;
100500100915         chain (V1Ccpo:V1Cksc) TIATC04L;
100600100915         IF  %found(TIATC04L);
100700100915           F05Pink = *on;
100800100915         ENDIF;
100900100809
101000100809       ENDSR;
101100100721
101200100721       //--------------------------------------------------------------
101300100721       //?Gestione tasto funzionale F24 da videata D01
101400100721       //?F24=Altri Tasti
101500100721       //--------------------------------------------------------------
101600100721       BEGSR F24D01;
101700100721
101800100721         SELECT;
101900100721           WHEN  (wf24 = 3 and wrig = 3) or
102000100721                 (wf24 = 2 and wrig = 2);
102100100721             VZFd01 = wtf1;
102200100721             wrig = 1;
102300100721             IF  wf24 = 2;
102400100721               VZFd02 = cf2412;
102500100721             ELSE;
102600100721               VZFd02 = cf2413;
102700100721             ENDIF;
102800100721
102900100721           WHEN  (wf24 = 3 and wrig = 1) or
103000100721                 (wf24 = 2 and wrig = 1);
103100100721             VZFd01 = wtf2;
103200100721             wrig = 2;
103300100721             IF  wf24 = 2;
103400100721               VZFd02 = cf2422;
103500100721             ELSE;
103600100721               VZFd02 = cf2423;
103700100721             ENDIF;
103800100721
103900100721           WHEN  wf24 = 3 and wrig = 2;
104000100721             VZFd01 = wtf3;
104100100721             wrig = 3;
104200100721             VZFd02 = cf2433;
104300100721
104400100721         ENDSL;
104500100721
104600100721       ENDSR;
104700100625
104800100625       //--------------------------------------------------------------
104900100625       //?Controllo videata D01.
105000100625       //--------------------------------------------------------------
105100100625       BEGSR CtrD01;
105200100625
105300100625         WindDspF = IndDspF;
105400100625         reset WindDspFb;
105500100625         IndDspF  = WindDspF;
105600100625
105700100625       ENDSR;
105800100624
105900100624       //--------------------------------------------------------------
106000100624       //?Controllo se ci sono altre attività sul cliente.
106100100624       //--------------------------------------------------------------
106200100624       BEGSR AltreAtt;
106300100624
106400100624         IF  F05Pink;
106500100624           clear TRMK21DS;
106600100722           clear TRMK19DS;
106700100624           I21mod = 'G';
106800100722           I21cpo = V1Ccpo;
106900100721           I21ksc = V1Cksc;
107000100721           I21rsc = V1Crag;
107100100624           kpjbu  = TRMK21ds;
107200100809           IF  ITAI2ric = 'A';
107300100809             IMK19tco = 'T';
107400100809           ENDIF;
107500100809           IF  ITAI2ric = 'T';
107600100809             IMK19tco = 'O';
107700100809           ENDIF;
107800100722           IMK19cpo  = V1Ccpo;
107900100722           IMK19ksc  = V1Cksc;
108000100722           IMK19com  = ITAI2com;
108100100722           IMK19comd = ITAI2comd;
108200100624           trmk21r (kpjba : TRMK19DS);
108300100624           TRMK21DS = kpjbu;
108400100722           IF  O21err <> *blanks;
108500100722             ErrGenerico = *on;
108600100722             ErrMessage  = *on;
108700100722             V1Dmsg      = O21msg;
108800100722           ENDIF;
108900100722           IF  O21fxx = '12';
109000100722             ErrGenerico = *on;
109100100722           ENDIF;
109200100624           IF  O21fxx = '99';
109300100624             ErrGenerico = *on;
109400100722             $Fine = *on;
109500100624           ENDIF;
109600100624
109700100624         ENDIF;
109800100624
109900100624       ENDSR;
110000100624
110100100624       //--------------------------------------------------------------
110200100624       //?Richiamo TRMK19R per new attività.
110300100624       //--------------------------------------------------------------
110400100624       BEGSR CallTrmk19;
110500100624
110600100722         clear TRMK19ds;
110700100722         IMK19cmt  = 'S';
110800100722         IMK19fle  = 'C';
110900100722         IMK19tco  = 'T';
111000100722         IMK19cpo  = V1Ccpo;
111100100722         IMK19ksc  = V1Cksc;
111200100722         IMK19com  = ITAI2com;
111300100722         IMK19comd = ITAI2comd;
111400100722
111500100722         trmk19r ( kpjba: TRMK19DS );
111600100810       //?se non torna errore
111700100722       //?esco dal programma
111800100722         IF  OMK19Err = *blanks;
111900100722           $Fine = *on;
112000100722         ENDIF;
112100100810       //?se torna errore con relativo messaggio emetto il messaggio
112200100810         IF  OMK19Err <> *blanks and OMK19msg <> *blanks;
112300100810           ErrGenerico = *on;
112400100810           ErrMessage  = *on;
112500100810           V1Dmsg      = OMK19msg;
112600100810         ENDIF;
112700100810       //?se torna errore senza messaggio esco dal programma
112800100810       //?(in questo caso errore vale per F12 da TRMK19R)
112900100810         IF  OMK19Err <> *blanks and OMK19msg = *blanks;
113000100810           $Fine = *on;
113100100810         ENDIF;
113200100624
113300100624       ENDSR;
113400100809
113500100809       //--------------------------------------------------------------
113600100809       //?Richiamo TNTA88R per new trattativa.
113700100809       //--------------------------------------------------------------
113800100809       BEGSR CallTnta88;
113900100809
114000100809         clear TNTA88ds;
114100100809         ITA88cmt  = 'S';
114200100809         ITA88fle  = 'C';
114300100809         ITA88cpo  = V1Ccpo;
114400100809         ITA88ksc  = V1Cksc;
114500100809         ITA88com  = ITAI2com;
114600100809         ITA88comd = ITAI2comd;
114700100809
114800100809         tnta88r ( kpjba: TNTA88DS );
114900100809       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
115000100809       //?esco dal programma
115100100809         IF  OTA88Err = *blanks;
115200100809           $Fine = *on;
115300100809         ENDIF;
115400100809
115500100809       ENDSR;
115600080206
115700080206       //--------------------------------------------------------------
115800080206       //?Operazioni finali.
115900080206       //--------------------------------------------------------------
116000080206       BEGSR RoutEnd;
116100100721
116200100721        //?Se errore torno il dato al chiamante
116300100721         IF  $ErrGrave;
116400100721           OTAI2err = '1';
116500100721           OTAI2msg = V1Dmsg;
116600100721         ENDIF;
116700080206
116800080206         *inLR = *on;
116900080206         return;
117000080206
117100080206       ENDSR;
117200080206
117300080206      /end-free
117400100624
117500080206       //--------------------------------------------------------------
117600080206       //?Schiere a tempo di compilazione.
117700080206       //--------------------------------------------------------------
117800080206
117900080206** - $MSG -------------------------------------------------------------------*
118000100721Codice cliente non passato                                                     1
118100110517Codice sottoconto fatturazione non impostato interrogazione non possibile      2
118200100721
118300100721** - $Tfv (in ordine di visualizzazione) ------------------------------------*
118400100809F22=Richiesta Contatto   #
118500100721F14=InfoCommerciali   #
118600110517F15=Codici Collegati   #
118700110907F17=Dati Fatturazione   #
118800100721F20=Potenziali   #
118900110407F21=Blocco Cliente  #
119000110615F12=Ritorno   #
