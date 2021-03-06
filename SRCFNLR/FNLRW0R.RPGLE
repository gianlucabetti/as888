000100090708      //---------------------------------------------------------------
000200091013      //?FNLRW0R - Controllo spedizioni Wurth
000300091013      //?          codici 0430841 - 0434470
000400090708      //---------------------------------------------------------------
000500090708
000600090708     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700090708
000800090708      //---------------------------------------------------------------
000900090708      //?Dichiarazione file.
001000090708      //---------------------------------------------------------------
001100091013     fazorg01l  if   e           k disk
001200091012     ffnlrw0d   cf   e             workstn
001300090708     f                                     indds(IndDspF)
001400090708     f                                     infds(InfDspF)
001500090708
001600090708      //---------------------------------------------------------------
001700090511      //?Definizione costanti.
001800090511      //---------------------------------------------------------------
001900090708
002000090708      // - Tasti funzionali a video
002100090708     d c_F01           c                   const(x'31')
002200090708     d c_F02           c                   const(x'32')
002300090708     d c_F03           c                   const(x'33')
002400090723     d c_F04           c                   const(x'34')
002500090708     d c_F05           c                   const(x'35')
002600090708     d c_F06           c                   const(x'36')
002700090708     d c_F07           c                   const(x'37')
002800090708     d c_F08           c                   const(x'38')
002900090708     d c_F09           c                   const(x'39')
003000090708     d c_F10           c                   const(x'3A')
003100090810     d c_F11           c                   const(x'3B')
003200090708     d c_F12           c                   const(x'3C')
003300090708     d c_F13           c                   const(x'B1')
003400090708     d c_F14           c                   const(x'B2')
003500090708     d c_F15           c                   const(x'B3')
003600090708     d c_F16           c                   const(x'B4')
003700090708     d c_F17           c                   const(x'B5')
003800090708     d c_F18           c                   const(x'B6')
003900090708     d c_F19           c                   const(x'B7')
004000090708     d c_F20           c                   const(x'B8')
004100090708     d c_F21           c                   const(x'B9')
004200090708     d c_F22           c                   const(x'BA')
004300090708     d c_F23           c                   const(x'BB')
004400090708     d c_F24           c                   const(x'BC')
004500090708     d c_Enter         c                   const(x'F1')
004600090708     d c_RollDown      c                   const(x'F4')
004700090708     d c_RollUp        c                   const(x'F5')
004800090511
004900090511      //---------------------------------------------------------------
005000090511      //?Definizione schiere.
005100090511      //---------------------------------------------------------------
005200090708
005300090708      // - Messaggi di errore
005400090708     d $Msg            s             78    dim(10) ctdata perrcd(1)
005500090708
005600090511      //---------------------------------------------------------------
005700090511      //?Definizione aree dati.
005800090511      //---------------------------------------------------------------
005900090708
006000090708      // - Dati utente
006100090708     d �AzUte        e ds                  extname(AZUTE00F)
006200090708     d                                     dtaara
006300090708     d �DatiUte      e ds                  extname(dDatiUte)
006400090708     d                                     dtaara
006500090511
006600090511      //---------------------------------------------------------------
006700090511      //?Definizione strutture dati.
006800090511      //---------------------------------------------------------------
006900090708
007000090708      // - Status
007100090708     d Psds           sds
007200090708     d   SDSpgm          *proc
007300090708
007400090708      // - InfDS
007500090708     d InfDspF         ds
007600090708     d  dsp_aid              369    369a                                        AID byte
007700090708
007800090708      // - Indicatori su DspF
007900090708     d IndDspF         ds
008000090708        // - Indicatori di errore
008100090708     d  ErrMessage                    1n   overlay(IndDspF : 28)
008200091013     d  PosCurDATA                    1n   overlay(IndDspF : 50)
008300090708     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008400090724
008500090708      // - Controllo data
008600090708     d wlbdat          ds                  inz
008700090708     d  g02dat                 1      8  0
008800090708     d  g02inv                 9     16  0
008900090708     d  g02err                17     17
009000090708     d  g02tgi                18     22  0
009100090708
009200090708      // - Parametri ricevuti
009300090708     d KPJBA         e ds
009400090708
009500090708      // - Reperimento dati utente
009600090708     d TIBS34ds      e ds
009700090708
009800090708      // Campi di comodo per gestione indicatori a video
009900090708     d WindDspF        ds                  inz
010000090708     d   WindDspFa             1     49    inz(*zero)
010100090708     d   WindDspFb            50     99    inz(*zero)
010200090511
010300090511      //---------------------------------------------------------------
010400090511      //?Definizione variabili globali.
010500090511      //---------------------------------------------------------------
010600090708
010700090708      // - Flags booleani
010800090807     d $Fine           s               n   inz(*off)
010900090807     d $InzD01         s               n   inz(*on)
011000090708
011100090708      // - Campi associati al video
011200090708     d $Video          s              2    inz('D1')
011300090804
011400090804       // - Stringa SQL da eseguire
011500090804     d wSQL            s           2048    Varying        inz
011600090708
011700090708      // - Campi di comodo
011800090721     d wdata           s              8  0
011900090721     d wDate_EUR       s               d   datfmt(*eur)
012000090721     d wDate_ISO       s               d   datfmt(*iso)
012100090511
012200090511      //---------------------------------------------------------------
012300090511      //?Definizione procedure esterne.
012400090511      //---------------------------------------------------------------
012500091013
012600091013      // - Sottomissione lavoro batch
012700091013     d bch10           pr                  extpgm('BCH10')
012800091013     d  kpjba                              likeds(KPJBA)
012900091013
013000091013      // - Richiamo diretto lavoro batch
013100091013     d fnlrw1r         pr                  extpgm('FNLRW1R')
013200091013     d  kpjba                              likeds(KPJBA)
013300090708
013400090708      // - Reperimento dati utente
013500090708     d tibs34r         pr                  extpgm('TIBS34R')
013600090708     d  tibs34ds                           likeds(TIBS34ds)
013700091012
013800090708      /copy gaitrasrc/srcprotopr,xsrda8
013900090511
014000090511      //---------------------------------------------------------------
014100090511      //?Definizione key-list.
014200090511      //---------------------------------------------------------------
014300090708
014400090708      //---------------------------------------------------------------
014500090708      //?Riepilogo indicatori.
014600090708      //---------------------------------------------------------------
014700090708
014800090708      // 28    : Emissione messaggio di errore a video
014900090721      // 50    : Posizionamento Cursore Data
015000090708      // 99    : Generico di Errore
015100090511
015200090511      //---------------------------------------------------------------
015300090708      //?M A I N - L I N E
015400090511      //---------------------------------------------------------------
015500090708
015600090708     c     *Entry        plist
015700090708     c                   parm                    KPJBA
015800090708
015900090708      /free
016000090708
016100090708       //?Operazioni iniziali
016200090708       exsr sr_RoutInz;
016300090708
016400090708       //?Gestione video
016500090722       DOW  $Fine = *off;
016600090722
016700090722         SELECT;
016800090722           WHEN  $Video = 'D1';
016900090708             exsr sr_GesD01;
017000090722           OTHER;
017100090708             $Fine = *on;
017200090722         ENDSL;
017300090722
017400090708       ENDDO;
017500090708
017600090708       //?Operazioni finali
017700090708       exsr sr_RoutEnd;
017800090708
017900090708       //--------------------------------------------------------------
018000090708       //?Operazioni iniziali.
018100090708       //--------------------------------------------------------------
018200090708       BEGSR sr_RoutInz;
018300090708
018400090708         //?Impostazione campi "fissi"
018500090708         T01pgm = SDSpgm;
018600090708
018700090708         //?Reperimento dati job
018800090708         exsr sr_DatiJob;
018900090708
019000090708       ENDSR;
019100090708
019200090708       //--------------------------------------------------------------
019300090708       //?Reperimento Dati del job (Utente/Operativi).
019400090708       //--------------------------------------------------------------
019500090708       BEGSR sr_DatiJob;
019600090708
019700090708         in(E) �AzUte;
019800090722         IF  NOT %error;
019900090708           in(E) �DatiUte;
020000090722         ENDIF;
020100090722         IF  %error or RSut = *blanks;
020200090708           clear TIBS34ds;
020300090708           tibs34r(tibs34ds);
020400090708           in �AzUte;
020500090708           in �DatiUte;
020600090722         ENDIF;
020700090708
020800090708       ENDSR;
020900090708
021000090708       //--------------------------------------------------------------
021100090805       //?Gestione videata D01.
021200090708       //--------------------------------------------------------------
021300090708       BEGSR sr_GesD01;
021400090708
021500090708         //?Inizializzazione videata
021600090722         IF  $InzD01 = *on;
021700090708           exsr sr_InzD01;
021800090708           $InzD01 = *off;
021900090722         ENDIF;
022000090708
022100090708         //?Emissione videata
022200091012         write LRW0T01;
022300091012         exfmt LRW0D01;
022400090708         ErrMessage  = *off;
022500090708         ErrGenerico = *off;
022600090708         clear V1Dmsg;
022700090708
022800090708         SELECT;
022900090708
023000090708           //?F3=Fine
023100090722           WHEN  dsp_aid = c_F03;
023200090708             exsr sr_F03D01;
023300091013
023400091013           //?F6=Conferma
023500091013           WHEN  dsp_aid = c_F06;
023600091013             //?- Controllo dati
023700091013             exsr sr_CtrD01;
023800091013             IF  errGenerico;
023900091013               leavesr;
024000091013             ENDIF;
024100091013             exsr sr_F06D01;
024200091015             $Fine = *on;
024300091015             leavesr;
024400090708
024500090708           //?Enter
024600090708           OTHER;
024700090708             //?- Controllo dati
024800090708             exsr sr_CtrD01;
024900090722             IF  errGenerico;
025000090708               leavesr;
025100090722             ENDIF;
025200090708
025300090708         ENDSL;
025400090708
025500090708       ENDSR;
025600090708
025700090708       //--------------------------------------------------------------
025800090805       //?Inizializzazione videata D01.
025900090708       //--------------------------------------------------------------
026000090708       BEGSR sr_InzD01;
026100090708
026200091012         clear LRW0D01;
026300091012
026400091012         //?imposto la filiale arrivo
026500091013         V1clna = 277;
026600091012         //?Decodifico la filiale di arrivo
026700091013         clear V1dLNA;
026800091013         chain V1cLNA AZORG01L;
026900091013         IF  not %found( AZORG01L ) or ORGfva <> *blanks;
027000091013           leavesr;
027100091013         ENDIF;
027200091013
027300091013         V1dLNA = ORGdes;
027400090708
027500090708       ENDSR;
027600090708
027700090708       //--------------------------------------------------------------
027800090805       //?Gestione tasto funzionale F3 da videata D01.
027900090708       //?F3=Fine
028000090708       //--------------------------------------------------------------
028100090708       BEGSR sr_F03D01;
028200090708
028300090708         //?Chiusura del programma
028400090708         $Fine = *on;
028500090708
028600090708       ENDSR;
028700090708
028800090708       //--------------------------------------------------------------
028900090805       //?Controllo videata D01.
029000090708       //--------------------------------------------------------------
029100090708       BEGSR sr_CtrD01;
029200090708
029300090708         WindDspF = IndDspF;
029400090708         reset WindDspFb;
029500090708         IndDspF  = WindDspF;
029600090708
029700090708         //?Controllo data
029800090722         IF  V1Data    = *zeros;
029900090722           ErrMessage   = *on;
030000090722           ErrGenerico  = *on;
030100090722           PosCurDATA   = *on;
030200091013           V1Dmsg       = $Msg(01);
030300090722           leavesr;
030400090722         ENDIF;
030500090708
030600090722         clear wlbdat;
030700090722         g02dat = V1Data;
030800090722         xsrda8(wlbdat);
030900090722         IF  g02err    = '1';
031000090722           ErrMessage   = *on;
031100090722           ErrGenerico  = *on;
031200090722           PosCurDATA   = *on;
031300091013           V1Dmsg       = $Msg(02);
031400090722           leavesr;
031500090722         ENDIF;
031600090708
031700090722         V1Data = g02dat;
031800090722         wdata  = g02inv;
031900090708
032000090708       ENDSR;
032100091013
032200091013       //--------------------------------------------------------------
032300091013       //?Gestione tasto funzionale F6 da videata D01.
032400091013       //?F6=Conferma
032500091013       //--------------------------------------------------------------
032600091013       BEGSR sr_F06D01;
032700091013
032800091013         %subst(kpjbu:1:8) = %editc(wdata:'X');
032900091013
033000091013       // sottometto il lavoro
033100091013         kcoaz = 'LRW1';
033200091013         kbuff = *blank;
033300091013         if  knmus = *all'1';
033400091013           fnlrw1r(kpjba);
033500091013         else;
033600091013           BCH10(kpjba);
033700091013         endif;
033800090722
033900090722       ENDSR;
034000090708
034100090708       //--------------------------------------------------------------
034200090708       //?Operazioni finali.
034300090708       //--------------------------------------------------------------
034400090708       BEGSR sr_RoutEnd;
034500090708
034600090708         *inLR = *on;
034700090708         return;
034800090708
034900090708       ENDSR;
035000090708
035100090708      /end-free
035200090708
035300090708       //--------------------------------------------------------------
035400090708       //?Schiere a tempo di compilazione.
035500090708       //--------------------------------------------------------------
035600090708
035700090708** - $MSG -------------------------------------------------------------------*
035800091013Immettere la data                                                              01
035900091013Data errata                                                                    02
