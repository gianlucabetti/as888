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
014200170728     d scf_DScnaco   e ds                  inz extname(CNACO00F)
014300170728     d                                     prefix(scf_)
014400170728     d scf_DScnind   e ds                  inz extname(CNIND00F)
014500170728     d                                     prefix(scf_)
014600170728     d scf_DScnclp   e ds                  inz extname(CNCLP00F)
014700170728     d                                     prefix(scf_)
014800170728     d scf_DSfncls   e ds                  inz extname(FNCLS00F)
014900170728     d                                     prefix(scf_)
015000100721
015100100721      // - Interrogazione Rubrica
015200100721     d TNTA12DS      e ds                  inz
015300100809
015400100809      // - Gestione trattative
015500100809     d TNTA88DS      e ds
015600100624
015700100624      // - Gestione attività
015800100624     d TRMK19DS      e ds
015900100722
016000100722      // - Interrogazione Potenziali
016100100721     d TRMK01DS      e ds                  extname(TRMK01DS)
016200100809
016300100809       // - Primo contatto
016400100809     d TRMK17DS      e ds
016500100722
016600100624      // - Interrogazione Note clienti
016700100624     d TRMK20DS      e ds                  inz
016800100624
016900100624      // - Interrogazione attività
017000100624     d TRMK21DS      e ds                  inz
017100100721
017200100721      // - Interrogazione Info Commerciali
017300100721     d TRMK50DS      e ds                  inz
017400110907
017500110907      // - Dati Fatturazione
017600110907     d TRUL57DS      e ds                  inz
017700110323
017800110323      // - Tabella BI = Causale blocco
017900110323     d dsBI          e ds                  inz
018000100624
018100100624      // - Tabella IC = Importanza clienti
018200100624     d dsIC          e ds                  inz
018300100624
018400100624      // - Tabella 1L = Categoria cliente
018500100624     d ds1L          e ds                  inz
018600100624
018700100624      // - Tabella 4W = Stato del credito
018800100624     d ds4W          e ds                  inz
018900100428
019000080206      //---------------------------------------------------------------
019100080206      //?Definizione variabili globali.
019200080206      //---------------------------------------------------------------
019300080206
019400080206      // - Flags booleani
019500100312     d $End            s               n   inz(*off)
019600080208     d $Fine           s               n   inz(*off)
019700100625     d $InzD01         s               n   inz(*on)
019800080206     d $ErrGrave       s               n   inz(*off)
019900080207     d $EoF            s               n   inz(*off)
020000120201     d Sede            s               n   inz(*off)
020100080206
020200080207      // - Campi associati al video
020300100625     d $Video          s              2    inz('D1')
020400100721
020500100721      // - Indici di schiera
020600100721     d xx              s              4  0 inz
020700110517
020800110517      // - Campi x passaggio parametri
020900110517     d PARMksc         s              7    inz
021000100721
021100100721      // - Campi di comodo
021200100721     d wdata           s              6
021300100721     d wf24            s              2  0
021400110407     d wmod            s              1
021500100721     d wpos            s              2  0
021600100721     d wposda          s              2  0
021700100721     d wpostf1         s              5  0
021800100721     d wpostf2         s              5  0
021900100721     d wpostf3         s              5  0
022000100721     d wrig            s              2  0
022100100721     d wtf1            s             62
022200100721     d wtf2            s             62
022300100721     d wtf3            s             62
022400080208
022500090508      //---------------------------------------------------------------
022600090508      //?Definizione procedure usate.
022700090508      //---------------------------------------------------------------
022800100721
022900100721      // - Interrogazione Unificante
023000100721     d tibs12r         pr                  extpgm('TIBS12R')
023100100721     d  kpjba                              likeds(KPJBA)
023200110517
023300110517      // - Interrogazione codici collegati
023400110517     d tnta83r         pr                  extpgm('TNTA83R')
023500110517     d  kpjba                              likeds(kpjba)
023600100721
023700100721      // - Interrogazione Potenziale
023800100721     d trmk02r         pr                  extpgm('TRMK02R')
023900100721     d  kpjba                              likeds(kpjba)
024000100721     d  trmk01ds                           likeds(trmk01ds)
024100100722     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
024200100809
024300100809      // - Inserimento attività primo contatto
024400100809     d trmk17r         pr                  extpgm('TRMK17R')
024500100809     d  kpjba                              likeds(KPJBA)
024600100809     d  trmk17ds                           likeds(TRMK17ds)
024700100624
024800100624      // - Interrogazione attività
024900100624     d trmk21r         pr                  extpgm('TRMK21R')
025000100624     d  kpjba                              likeds(KPJBA)
025100100624     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
025200110907
025300110907      // - Dati Fatturazione
025400110907     d trul57r         pr                  extpgm('TRUL57R')
025500110907     d  kpjba                              likeds(KPJBA)
025600110907     d  trul57ds                           likeds(TRUL57ds)
025700150806
025800150806       // -?Interrogazione campagne per cliente
025900150806     d TRKC09DS      e ds                  inz
026000150806     d TRKC09R         pr                  extpgm('TRKC09R')
026100150806     d   kpjba                             likeds(KPJBA)
026200150806     d   trkc09ds                          likeds(TRKC09DS)
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
028600080207      // 28    : Emissione messaggio di errore a video
028700100624      // 40    : F05 Pink
028800100624      // 41    : F18 Pink
028900100721      // 50    : Errore:
029000080207      // 99    : Generico di Errore
029100080206      //---------------------------------------------------------------
029200080206
029300080206      //---------------------------------------------------------------
029400080206      //?M A I N - L I N E
029500080206      //---------------------------------------------------------------
029600080206
029700080206     c     *Entry        plist
029800080206     c                   parm                    KPJBA
029900100721     c                   parm                    TNTAI2DS
030000080206
030100080206      /free
030200080206
030300100622       //?Operazioni iniziali
030400080206       exsr RoutInz;
030500080206
030600100622       //?Gestione video
030700080206       DOW $Fine = *off;
030800100624         SELECT;
030900100625           WHEN $Video = 'D1';
031000100625             exsr GesD01;
031100100624           OTHER;
031200080206             $Fine = *on;
031300100624         ENDSL;
031400080206       ENDDO;
031500080206
031600100622       //?Operazioni finali
031700080206       exsr RoutEnd;
031800080206
031900080206       //--------------------------------------------------------------
032000080206       //?Operazioni iniziali.
032100080206       //--------------------------------------------------------------
032200080206       BEGSR RoutInz;
032300100427
032400100622         exec sql
032500100622          set option dynusrprf = *owner, closqlcsr = *endmod;
032600080206
032700100622       //?Impostazione campi "fissi"
032800080208         T01pgm = SDSpgm;
032900080208         TBLkut = 1;
033000080206
033100100622       //?Reperimento dati job
033200080206         exsr DatiJob;
033300100721
033400100721       //?Se non passato il cliente torno errore
033500100721         IF  ITAI2ksc = 0;
033600100721           $ErrGrave = *on;
033700100721           $Fine     = *on;
033800100721           V1Dmsg    = $Msg(01);
033900100721           leavesr;
034000100721         ENDIF;
034100120201
034200120201       //?Controllo se utente di Sede
034300120201         Sede = (DutPou = 046);
034400090714
034500080206       ENDSR;
034600080206
034700080206       //--------------------------------------------------------------
034800080206       //?Reperimento Dati del job (Utente/Operativi).
034900080206       //--------------------------------------------------------------
035000080206       BEGSR DatiJob;
035100080206
035200080206         in(E) §AzUte;
035300080206         if NOT %error;
035400080206           in(E) §DatiUte;
035500080206         endif;
035600080206         if %error or RSut = *blanks;
035700080206           clear TIBS34ds;
035800080206           tibs34r(tibs34ds);
035900080206           in §AzUte;
036000080206           in §DatiUte;
036100080206         endif;
036200080206
036300080206       ENDSR;
036400100625
036500100625       //--------------------------------------------------------------
036600100625       //?Gestione videata D01.
036700100625       //--------------------------------------------------------------
036800100625       BEGSR GesD01;
036900100625
037000100625       //?Inizializzazione videata
037100100625         IF  $InzD01;
037200100625           exsr InzD01;
037300100625           $InzD01  = *off;
037400100721         //?Imposto tasti funzionali
037500100721           exsr TastiFun;
037600100625         ENDIF;
037700100721
037800100721       //?Emissione Testata
037900100721         write  TAI2T01;
038000100721       //?Emissione tasti funzionali
038100100721         write  TAI2Z01;
038200100721
038300100721       //?Emissione videata
038400100721         exfmt  TAI2D01;
038500100721         reset ErrMessage;
038600100721         reset ErrGenerico;
038700100721         clear V1Dmsg;
038800100721
038900100721         SELECT;
039000100721
039100100721       //?- F2=Rubrica
039200100721           WHEN  dsp_aid = c_F02;
039300100721             exsr F02D01;
039400100721
039500100721       //?- F3=Fine
039600100721           WHEN  dsp_aid = c_F03;
039700100721             exsr F03D01;
039800150126
039900150126           // -?F4=Campagna?
040000150126           WHEN  dsp_aid = c_F04;
040100150126             exsr  F04D01;
040200100721
040300100721       //?- F05=Attività
040400100721           WHEN  dsp_aid = c_F05;
040500100721             exsr F05D01;
040600100810             IF  ErrGenerico;
040700100810               leavesr;
040800100810             ENDIF;
040900100721
041000100721       //?- F07=Unificante
041100100721           WHEN  dsp_aid = c_F07;
041200100721             exsr F07D01;
041300100721
041400100721       //?- F12=Ritorno
041500100721           WHEN  dsp_aid = c_F12;
041600100721             exsr F12D01;
041700100721
041800100721       //?- F14=Info Commerciali
041900100721           WHEN  dsp_aid = c_F14;
042000100721             exsr F14D01;
042100110517
042200110517       //?- F15=Codici Collegati
042300110517           WHEN  dsp_aid = c_F15;
042400110517             exsr F15D01;
042500110907
042600110907       //?- F17=Dati Fatturazione
042700110907           WHEN  dsp_aid = c_F17;
042800110907             exsr F17D01;
042900100721
043000100721       //?- F18=Note
043100100721           WHEN  dsp_aid = c_F18;
043200100721             exsr F18D01;
043300100721
043400100721       //?- F20=Potenziali
043500100721           WHEN  dsp_aid = c_F20;
043600100721             exsr F20D01;
043700110407
043800110407       //?- F21=Blocco clienti
043900110407           WHEN  dsp_aid = c_F21;
044000110407             exsr F21D01;
044100100809
044200100809       //?- F22=Richiesta contatto
044300100809           WHEN  dsp_aid = c_F22;
044400100809             exsr F22D01;
044500100721
044600100721       //?- F24=Altri tasti
044700100721           WHEN  dsp_aid = c_F24 and wf24 > 1;
044800100721             exsr F24D01;
044900100721
045000100721       //?Invio
045100100721           OTHER;
045200120201         //?Se utente di Sede all'enter non deve fare niente
045300120201             IF  Sede;
045400120201               leavesr;
045500120201             ENDIF;
045600100721         //?Controllo se ci sono altre attività
045700100721             exsr AltreAtt;
045800100721             IF  ErrGenerico;
045900100721               leavesr;
046000100721             ENDIF;
046100100721        //?Se non è stato gestito da TRMK21R vado avanti con TRMK19R
046200100809             IF  ITAI2ric = 'A';
046300100809               exsr CallTrmk19;
046400100809             ENDIF;
046500100809        //?Se da trattativa vado avanti con TNTA88R
046600100809             IF  ITAI2ric = 'T';
046700100809               exsr CallTnta88;
046800100809             ENDIF;
046900100625
047000100625         ENDSL;
047100100625
047200100625       ENDSR;
047300100625
047400100625       //--------------------------------------------------------------
047500100625       //?Inizializzazione videata D01.
047600100625       //--------------------------------------------------------------
047700100625       BEGSR InzD01;
047800100625
047900100721         clear TAI2D01;
048000110407         clear wmod;
048100100721
048200100809         V1Cksc = ITAI2ksc;
048300100721
048400100721       //?Aggancio i file dell'anagrafica cliente
048500100721         clear tibs69ds;
048600100721         clear ds_cnaco;
048700100721         clear ds_cnind;
048800100721         clear ds_cnclp;
048900100721         clear ds_fncls;
049000100721         I69kac = V1Cksc;
049100100721         I69kin = V1Cksc;
049200100721         I69kcp = V1Cksc;
049300170727         I69kcs = V1Cksc;
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
054500170224         TBLkey = ' ' + V1Ccon;
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
061700170728           clear scf_dscnaco;
061800170728           clear scf_dscnind;
061900170728           clear scf_dscnclp;
062000170728           clear scf_dsfncls;
062100100722           I69kac = V1Cscf;
062200170728           TIBS69R (tibs69ds:scf_dscnaco:scf_dscnind:scf_dscnclp:scf_dsfncls);
062300170728           V1Dscf = scf_ACOrag;
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
063500100625
063600100625       ENDSR;
063700100721
063800100721       //--------------------------------------------------------------
063900100721       //?Imposto i tasti funzionali.
064000100721       //--------------------------------------------------------------
064100100721       BEGSR TastiFun;
064200100721
064300100721       //?Se presenti attività sul cliente visualizzo il tasto in PINK
064400100721         F05Pink = *off;
064500100721         chain (V1Ccpo:V1Cksc) TIATC04L;
064600100721         IF  %found(TIATC04L);
064700100721           F05Pink = *on;
064800100721         ENDIF;
064900100721
065000100721       //?Se presenti note sul cliente visualizzo il tasto in PINK
065100100721         F18Pink = *off;
065200100721         chain (V1Cksc) TICPN02L;
065300100721         IF  %found(TICPN02L);
065400100721           F18Pink = *on;
065500100721         ENDIF;
065600110502
065700110502       //?Abilito tasto F21=Blocco cliente se il cliente non è già bloccato
065800120201       //?non abilito se utente di Sede
065900110502         F21Attivo = *off;
066000120201         IF  V1Cabl = *blanks and not Sede;
066100110502           F21Attivo = *on;
066200110502         ENDIF;
066300120201
066400120201       //?Abilito tasto F22=Richiesta contatto se utente di filiale
066500120201         F22Attivo = *off;
066600120201         IF  not Sede;
066700120201           F22Attivo = *on;
066800120201         ENDIF;
066900100721
067000100721       //?Imposto la seconda riga variabile
067100100721         clear wpos;
067200100721         clear wposda;
067300100721         clear wpostf1;
067400100721         clear wpostf2;
067500100721         clear wpostf3;
067600100721         clear wrig;
067700100721         clear wtf1;
067800100721         clear wtf2;
067900100721         clear wtf3;
068000100721
068100100721         xx = 1;
068200100721         FOR xx by 1 to %elem($Tfv);
068300110502         //?Se F21 non abilitato leggo altro tasto funzione
068400110502           IF  not F21Attivo and %subst($Tfv(xx):1:3) = 'F21';
068500110502             iter;
068600110502           ENDIF;
068700120201         //?Se F22 non abilitato leggo altro tasto funzione
068800120201           IF  not F22Attivo and %subst($Tfv(xx):1:3) = 'F22';
068900120201             iter;
069000120201           ENDIF;
069100110502
069200100721           IF  $Tfv(xx) <> *blanks;
069300100721             wpos = %scan('#':$tfv(xx));
069400100721             wpostf1 += wpos;
069500100721
069600100721             IF  wpostf1 <= %len(VZFd01);
069700100721               wposda = wpostf1 - wpos + 1;
069800100721               %subst(wtf1:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
069900100721             ELSE;
070000100721               wpostf2 += wpos;
070100100721               IF  wpostf2 <= %len(VZFd01);
070200100721                 wposda = wpostf2 - wpos + 1;
070300100721                 %subst(wtf2:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
070400100721               ELSE;
070500100721                 wpostf3 += wpos;
070600100721                 IF  wpostf3 <= %len(VZFd01);
070700100721                   wposda = wpostf3 - wpos + 1;
070800100721                   %subst(wtf3:wposda:(wpos-1)) = %subst($tfv(xx):1:(wpos-1));
070900100721                 ENDIF;
071000100721               ENDIF;
071100100721             ENDIF;
071200100721
071300100721           ENDIF;
071400100721         ENDFOR;
071500100721
071600100721       //?Imposto F24 giusto
071700100721         SELECT;
071800100721           WHEN  wtf3 <> *blanks;
071900100721             VZFd02 = cf2413;
072000100721             wf24 = 3;
072100100721           WHEN  wtf2 <> *blanks;
072200110502             VZFd02 = cf2412;
072300100721             wf24 = 2;
072400100721           OTHER;
072500100721             clear VZFd02;
072600100721             wf24 = 1;
072700100721         ENDSL;
072800100721
072900100721         VZFd01 = wtf1;
073000100721         wrig = 1;
073100100721
073200100721       ENDSR;
073300141118
073400141118       //--------------------------------------------------------------
073500150126       //?Gestione tasto funzionale F04 da videata D01                 ?
073600150126       //?F4=Campagna                                                  ?
073700141118       //--------------------------------------------------------------
073800150126       BEGSR F04D01;
073900150806
074000150807         clear TRKC09DS;
074100150807         IKC09ksu = ITAI2ksc;
074200150807         trkc09r (kpjba:TRKC09DS);
074300141118
074400141118       ENDSR;
074500100721
074600100721       //--------------------------------------------------------------
074700100721       //?Gestione tasto funzionale F02 da videata D01
074800100721       //?F2=Rubrica
074900100721       //--------------------------------------------------------------
075000100721       BEGSR F02D01;
075100100721
075200100721         clear TNTA12DS;
075300100721         TA12ric = 'V';
075400100721         TA12apl = 'C';
075500100721         TA12ksc = V1Cksc;
075600100721         TA12rag = V1Crag;
075700100721
075800100721         tnta12r (kpjba : TNTA12DS);
075900100721
076000100721         IF  TA12err <> *blanks;
076100100721           ErrMessage  = *on;
076200100721           ErrGenerico = *on;
076300100721           V1Dmsg      = TA12msg;
076400100721           leavesr;
076500100721         ENDIF;
076600100721
076700100721       ENDSR;
076800100625
076900100625       //--------------------------------------------------------------
077000100625       //?Gestione tasto funzionale F03 da videata D01
077100100625       //?F3=Fine
077200100625       //--------------------------------------------------------------
077300100625       BEGSR F03D01;
077400100625
077500100625         // Chiusura del programma
077600100928         OTAI2f03 = '1';
077700100625         $Fine = *on;
077800100625
077900100625       ENDSR;
078000100721
078100100721       //--------------------------------------------------------------
078200100721       //?Gestione tasto funzionale F05 da videata D01
078300100721       //?F5=Attività
078400100721       //--------------------------------------------------------------
078500100721       BEGSR F05D01;
078600100721
078700100721         clear TRMK21DS;
078800100810         clear TRMK19DS;
078900100810         I21mod = 'V';
079000100810         I21cpo = V1Ccpo;
079100100721         I21ksc = V1Cksc;
079200100721         I21rsc = V1Crag;
079300100721         kpjbu = TRMK21DS;
079400100810         IF  ITAI2ric = 'A';
079500100810           IMK19tco = 'T';
079600100810         ENDIF;
079700100810         IF  ITAI2ric = 'T';
079800100810           IMK19tco = 'O';
079900100810         ENDIF;
080000100810         IMK19cpo  = V1Ccpo;
080100100810         IMK19ksc  = V1Cksc;
080200100810         IMK19com  = ITAI2com;
080300100810         IMK19comd = ITAI2comd;
080400120201
080500120201       //?Se utente di Sede devo SOLO visualizzare le attività
080600120201         IF  Sede;
080700120201           I21mod = 'I';
080800120201           kpjbu = TRMK21DS;
080900120201           trmk21r (kpjba);
081000120201         ELSE;
081100100810         trmk21r (kpjba : TRMK19DS);
081200120201         ENDIF;
081300120201
081400100721         TRMK21DS = kpjbu;
081500100810         IF  O21err <> *blanks;
081600100810           ErrGenerico = *on;
081700100810           ErrMessage  = *on;
081800100810           V1Dmsg      = O21msg;
081900100810           leavesr;
082000100810         ENDIF;
082100120201
082200120201        //?Se utente di Sede non devo fare + nessun controllo
082300120201         IF  Sede;
082400120201           leavesr;
082500120201         ENDIF;
082600120201
082700100810        //?Se F12 torno al dettaglio cliente
082800100810         IF  O21fxx = '12';
082900100810           ErrGenerico = *on;
083000100810         ENDIF;
083100100810        //?Se gestita attività esco dal pgm
083200100810         IF  O21fxx = '99';
083300100810           ErrGenerico = *on;
083400100810           $Fine = *on;
083500100810         ENDIF;
083600100810        //?Se non è stato gestito da TRMK21R
083700100927        //?o se fatto F10 devo andare avanti con il TRMK19R o TNTA88R
083800100927         IF  O21fxx = *blanks or O21fxx = '10';
083900100810         //?vado avanti con TRMK19R se arrivo da attività
084000100810           IF  ITAI2ric = 'A';
084100100810             exsr CallTrmk19;
084200100810           ENDIF;
084300100810         //?vado avanti con TNTA88R se arrivo da trattative
084400100810           IF  ITAI2ric = 'T';
084500100810             exsr CallTnta88;
084600100810           ENDIF;
084700100810         ENDIF;
084800100721
084900100721       ENDSR;
085000100721
085100100721       //--------------------------------------------------------------
085200100721       //?Gestione tasto funzionale F07 da videata D01
085300100721       //?F7=Unificante
085400100721       //--------------------------------------------------------------
085500100721       BEGSR F07D01;
085600100721
085700100721         clear TIBS12DS;
085800100721         K12op0 = 'P05';
085900100721         K12op1 = 'O09';
086000100721         K12f03 = '0';
086100100721         K12f12 = '0';
086200100721         K12err = '0';
086300100721         K12cop = V1Cksc;
086400100721         kpjbu = TIBS12DS;
086500100721
086600100721         tibs12r (kpjba);
086700100721
086800100721       ENDSR;
086900100721
087000100721       //--------------------------------------------------------------
087100100721       //?Gestione tasto funzionale F12 da videata D01
087200100721       //?F12=Fine
087300100721       //--------------------------------------------------------------
087400100721       BEGSR F12D01;
087500100721
087600100721         // Chiusura del programma
087700100721         $Fine = *on;
087800100721
087900100721       ENDSR;
088000100721
088100100721       //--------------------------------------------------------------
088200100721       //?Gestione tasto funzionale F14 da videata D01
088300100721       //?F14=Info Commerciali
088400100721       //--------------------------------------------------------------
088500100721       BEGSR F14D01;
088600100721
088700100721         clear TRMK50DS;
088800100721         I50cpo = V1Ccpo;
088900110407         IF  wmod = *blanks;
089000110407           I50mod = 'I';
089100110407         ELSE;
089200110407           I50mod = wmod;
089300110407         ENDIF;
089400100721         I50rag = V1Crag;
089500100721         I50pgm = 'TNTAI2R';
089600100721
089700100721         trmk50r (kpjba : TRMK50DS);
089800100721
089900100721       ENDSR;
090000110517
090100110517       //--------------------------------------------------------------
090200110517       //?Gestione tasto funzionale F15 da videata D01
090300110517       //?F15=Codici Collegati
090400110517       //--------------------------------------------------------------
090500110517       BEGSR F15D01;
090600110517
090700110517         IF  V1Cscf = *zeros;
090800110517           ErrGenerico = *on;
090900110517           ErrMessage  = *on;
091000110517           V1Dmsg      = $msg(02);
091100110517           leavesr;
091200110517         ENDIF;
091300110517
091400110517         PARMksc = %editc(V1Cscf:'X');
091500110517         kpjbu = PARMksc;
091600110517         tnta83r (kpjba);
091700110517
091800110517       ENDSR;
091900110907
092000110907       //--------------------------------------------------------------
092100110907       //?Gestione tasto funzionale F17 da videata D01
092200110907       //?F17=Dati Fatturazione
092300110907       //--------------------------------------------------------------
092400110907       BEGSR F17D01;
092500110907
092600110907         clear trul57ds;
092700110907         D57ikscb = V1Cksc;
092800110907         D57ikscf = V1Cscf;
092900110907         D57iunib = %subst(CLSflo:2:1);
093000110907         D57itftb = %editc(CLPtft:'X');
093100110907         D57ifftb = %editc(CLPfft:'X');
093200110907         D57idftb = CLPfun;
093300110907         D57ipgm  = 'TNTAI2R';
093400110907         D57isifb = INDsin;
093500110907         D57icdpb = INDcdp;
093600110907         D57iabib = INDabi;
093700110907         D57icabb = INDcab;
093800110907
093900110907         trul57r (kpjba : TRUL57DS);
094000110907
094100110907       ENDSR;
094200100721
094300100721       //--------------------------------------------------------------
094400100721       //?Gestione tasto funzionale F18 da videata D01
094500100721       //?F18=Note
094600100721       //--------------------------------------------------------------
094700100721       BEGSR F18D01;
094800100721
094900100721         clear TRMK20DS;
095000110210         IMK20tla = 'L';
095100100721         IMK20flm = 'I';
095200100721         IMK20ksc = V1Cksc;
095300100721         IMK20rsc = V1Crag;
095400100721
095500100721         trmk20r (kpjba : TRMK20DS);
095600100915
095700100915       //?Se presenti note sul cliente visualizzo il tasto in PINK
095800100915         F18Pink = *off;
095900100915         chain (V1Cksc) TICPN02L;
096000100915         IF  %found(TICPN02L);
096100100915           F18Pink = *on;
096200100915         ENDIF;
096300100721
096400100721       ENDSR;
096500100721
096600100721       //--------------------------------------------------------------
096700100721       //?Gestione tasto funzionale F20 da videata D01
096800100721       //?F20=Potenziali
096900100721       //--------------------------------------------------------------
097000100721       BEGSR F20D01;
097100100721
097200100721         clear TRMK01DS;
097300100721         MK01k10 = 'N';
097400100721         MK01cpo = V1ccpo;
097500100721         trmk02r (kpjba : TRMK01DS);
097600100721
097700100721       ENDSR;
097800110407
097900110407       //--------------------------------------------------------------
098000110407       //?Gestione tasto funzionale F21 da videata D01
098100110407       //?F21=Blocco Cliente
098200110407       //--------------------------------------------------------------
098300110407       BEGSR F21D01;
098400110407
098500110407         clear trmk17ds;
098600110407         IMK17patt = 'S';
098700110407         IMK17att  = 'T';
098800110407         IMK17pcau = 'S';
098900110407         IMK17cau  = '71';
099000110407         IMK17cpo  = V1Ccpo;
099100110407         IMK17ksc  = V1Cksc;
099200110407         IF  ITAI2com <> 0;
099300110407           IMK17co3  = ITAI2com;
099400110407         ENDIF;
099500110407
099600110407         trmk17r (kpjba : TRMK17DS);
099700110407
099800110407       //?Se ho pianificato l'attività devo richiamare la gestione
099900110407       //?delle info commerciali
100000110407         IF  OMK17f12 = *blanks and OMK17err = *blanks;
100100110407           wmod = 'G';
100200110407           exsr F14D01;
100300110407         ENDIF;
100400110407
100500110407       //?Se presenti attività sul cliente visualizzo il tasto in PINK
100600110407         F05Pink = *off;
100700110407         chain (V1Ccpo:V1Cksc) TIATC04L;
100800110407         IF  %found(TIATC04L);
100900110407           F05Pink = *on;
101000110407         ENDIF;
101100110407
101200110407       ENDSR;
101300100809
101400100809       //--------------------------------------------------------------
101500100809       //?Gestione tasto funzionale F22 da videata D01
101600100809       //?F22=Richiesta Contatto
101700100809       //--------------------------------------------------------------
101800100809       BEGSR F22D01;
101900100809
102000100809         clear trmk17ds;
102100100809         IMK17patt = 'S';
102200100809         IMK17att  = 'T';
102300100809         IMK17pcau = 'S';
102400100809         IMK17cau  = '20';
102500100809         IMK17cpo  = V1Ccpo;
102600100809         IMK17ksc  = V1Cksc;
102700100809         IF ITAI2com <> 0;
102800100809           IMK17co3  = ITAI2com;
102900100809         ENDIF;
103000100809
103100100809         trmk17r (kpjba : TRMK17DS);
103200100915
103300100915       //?Se presenti attività sul cliente visualizzo il tasto in PINK
103400100915         F05Pink = *off;
103500100915         chain (V1Ccpo:V1Cksc) TIATC04L;
103600100915         IF  %found(TIATC04L);
103700100915           F05Pink = *on;
103800100915         ENDIF;
103900100809
104000100809       ENDSR;
104100100721
104200100721       //--------------------------------------------------------------
104300100721       //?Gestione tasto funzionale F24 da videata D01
104400100721       //?F24=Altri Tasti
104500100721       //--------------------------------------------------------------
104600100721       BEGSR F24D01;
104700100721
104800100721         SELECT;
104900100721           WHEN  (wf24 = 3 and wrig = 3) or
105000100721                 (wf24 = 2 and wrig = 2);
105100100721             VZFd01 = wtf1;
105200100721             wrig = 1;
105300100721             IF  wf24 = 2;
105400100721               VZFd02 = cf2412;
105500100721             ELSE;
105600100721               VZFd02 = cf2413;
105700100721             ENDIF;
105800100721
105900100721           WHEN  (wf24 = 3 and wrig = 1) or
106000100721                 (wf24 = 2 and wrig = 1);
106100100721             VZFd01 = wtf2;
106200100721             wrig = 2;
106300100721             IF  wf24 = 2;
106400100721               VZFd02 = cf2422;
106500100721             ELSE;
106600100721               VZFd02 = cf2423;
106700100721             ENDIF;
106800100721
106900100721           WHEN  wf24 = 3 and wrig = 2;
107000100721             VZFd01 = wtf3;
107100100721             wrig = 3;
107200100721             VZFd02 = cf2433;
107300100721
107400100721         ENDSL;
107500100721
107600100721       ENDSR;
107700100625
107800100625       //--------------------------------------------------------------
107900100625       //?Controllo videata D01.
108000100625       //--------------------------------------------------------------
108100100625       BEGSR CtrD01;
108200100625
108300100625         WindDspF = IndDspF;
108400100625         reset WindDspFb;
108500100625         IndDspF  = WindDspF;
108600100625
108700100625       ENDSR;
108800100624
108900100624       //--------------------------------------------------------------
109000100624       //?Controllo se ci sono altre attività sul cliente.
109100100624       //--------------------------------------------------------------
109200100624       BEGSR AltreAtt;
109300100624
109400100624         IF  F05Pink;
109500100624           clear TRMK21DS;
109600100722           clear TRMK19DS;
109700100624           I21mod = 'G';
109800100722           I21cpo = V1Ccpo;
109900100721           I21ksc = V1Cksc;
110000100721           I21rsc = V1Crag;
110100100624           kpjbu  = TRMK21ds;
110200100809           IF  ITAI2ric = 'A';
110300100809             IMK19tco = 'T';
110400100809           ENDIF;
110500100809           IF  ITAI2ric = 'T';
110600100809             IMK19tco = 'O';
110700100809           ENDIF;
110800100722           IMK19cpo  = V1Ccpo;
110900100722           IMK19ksc  = V1Cksc;
111000100722           IMK19com  = ITAI2com;
111100100722           IMK19comd = ITAI2comd;
111200100624           trmk21r (kpjba : TRMK19DS);
111300100624           TRMK21DS = kpjbu;
111400100722           IF  O21err <> *blanks;
111500100722             ErrGenerico = *on;
111600100722             ErrMessage  = *on;
111700100722             V1Dmsg      = O21msg;
111800100722           ENDIF;
111900100722           IF  O21fxx = '12';
112000100722             ErrGenerico = *on;
112100100722           ENDIF;
112200100624           IF  O21fxx = '99';
112300100624             ErrGenerico = *on;
112400100722             $Fine = *on;
112500100624           ENDIF;
112600100624
112700100624         ENDIF;
112800100624
112900100624       ENDSR;
113000100624
113100100624       //--------------------------------------------------------------
113200100624       //?Richiamo TRMK19R per new attività.
113300100624       //--------------------------------------------------------------
113400100624       BEGSR CallTrmk19;
113500100624
113600100722         clear TRMK19ds;
113700100722         IMK19cmt  = 'S';
113800100722         IMK19fle  = 'C';
113900100722         IMK19tco  = 'T';
114000170206         IMK19tat = 'T';
114100100722         IMK19cpo  = V1Ccpo;
114200100722         IMK19ksc  = V1Cksc;
114300100722         IMK19com  = ITAI2com;
114400100722         IMK19comd = ITAI2comd;
114500100722
114600100722         trmk19r ( kpjba: TRMK19DS );
114700100810       //?se non torna errore
114800100722       //?esco dal programma
114900100722         IF  OMK19Err = *blanks;
115000100722           $Fine = *on;
115100100722         ENDIF;
115200100810       //?se torna errore con relativo messaggio emetto il messaggio
115300100810         IF  OMK19Err <> *blanks and OMK19msg <> *blanks;
115400100810           ErrGenerico = *on;
115500100810           ErrMessage  = *on;
115600100810           V1Dmsg      = OMK19msg;
115700100810         ENDIF;
115800100810       //?se torna errore senza messaggio esco dal programma
115900100810       //?(in questo caso errore vale per F12 da TRMK19R)
116000100810         IF  OMK19Err <> *blanks and OMK19msg = *blanks;
116100100810           $Fine = *on;
116200100810         ENDIF;
116300100624
116400100624       ENDSR;
116500100809
116600100809       //--------------------------------------------------------------
116700100809       //?Richiamo TNTA88R per new trattativa.
116800100809       //--------------------------------------------------------------
116900100809       BEGSR CallTnta88;
117000100809
117100100809         clear TNTA88ds;
117200100809         ITA88cmt  = 'S';
117300100809         ITA88fle  = 'C';
117400100809         ITA88cpo  = V1Ccpo;
117500100809         ITA88ksc  = V1Cksc;
117600100809         ITA88com  = ITAI2com;
117700100809         ITA88comd = ITAI2comd;
117800100809
117900100809         tnta88r ( kpjba: TNTA88DS );
118000100809       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
118100100809       //?esco dal programma
118200100809         IF  OTA88Err = *blanks;
118300100809           $Fine = *on;
118400100809         ENDIF;
118500100809
118600100809       ENDSR;
118700080206
118800080206       //--------------------------------------------------------------
118900080206       //?Operazioni finali.
119000080206       //--------------------------------------------------------------
119100080206       BEGSR RoutEnd;
119200100721
119300100721        //?Se errore torno il dato al chiamante
119400100721         IF  $ErrGrave;
119500100721           OTAI2err = '1';
119600100721           OTAI2msg = V1Dmsg;
119700100721         ENDIF;
119800080206
119900080206         *inLR = *on;
120000080206         return;
120100080206
120200080206       ENDSR;
120300080206
120400080206      /end-free
120500100624
120600080206       //--------------------------------------------------------------
120700080206       //?Schiere a tempo di compilazione.
120800080206       //--------------------------------------------------------------
120900080206
121000080206** - $MSG -------------------------------------------------------------------*
121100100721Codice cliente non passato                                                     1
121200110517Codice sottoconto fatturazione non impostato interrogazione non possibile      2
121300100721
121400100721** - $Tfv (in ordine di visualizzazione) ------------------------------------*
121500100809F22=Richiesta Contatto   #
121600100721F14=InfoCommerciali   #
121700110517F15=Codici Collegati   #
121800110907F17=Dati Fatturazione   #
121900100721F20=Potenziali   #
122000110407F21=Blocco Cliente  #
122100110615F12=Ritorno   #
