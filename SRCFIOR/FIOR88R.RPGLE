000100170314      //---------------------------------------------------------------
000200170518      //?FIOR88R - Statistica Ritiri con peso < 5 Kg. - anticipati
000300170314      //---------------------------------------------------------------
000400170518     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170518
000600170518      //---------------------------------------------------------------
000700170518      //?Dichiarazione file.
000800170518      //---------------------------------------------------------------
000900170518     fFIOR88D   cf   e             workstn
001000170518     f                                     indds(IndDspF)
001100170518     f                                     infds(InfDspF)
001200170518
001300170518      // - Organigramma
001400170518     fAZORG01L  if   e           k disk
001500170518
001600170518      //---------------------------------------------------------------
001700170518      //?Definizione costanti.
001800170518      //---------------------------------------------------------------
001900170518      // - Tasti funzionali a video
002000170518     d c_F01           c                   const(x'31')
002100170518     d c_F02           c                   const(x'32')
002200170518     d c_F03           c                   const(x'33')
002300170518     d c_F04           c                   const(x'34')
002400170518     d c_F05           c                   const(x'35')
002500170518     d c_F06           c                   const(x'36')
002600170518     d c_F07           c                   const(x'37')
002700170518     d c_F08           c                   const(x'38')
002800170518     d c_F09           c                   const(x'39')
002900170518     d c_F10           c                   const(x'3A')
003000170518     d c_F11           c                   const(x'3B')
003100170518     d c_F12           c                   const(x'3C')
003200170518     d c_F13           c                   const(x'B1')
003300170518     d c_F14           c                   const(x'B2')
003400170518     d c_F15           c                   const(x'B3')
003500170518     d c_F16           c                   const(x'B4')
003600170518     d c_F17           c                   const(x'B5')
003700170518     d c_F18           c                   const(x'B6')
003800170518     d c_F19           c                   const(x'B7')
003900170518     d c_F20           c                   const(x'B8')
004000170518     d c_F21           c                   const(x'B9')
004100170518     d c_F22           c                   const(x'BA')
004200170518     d c_F23           c                   const(x'BB')
004300170518     d c_F24           c                   const(x'BC')
004400170518     d c_Enter         c                   const(x'F1')
004500170518     d c_RollDown      c                   const(x'F4')
004600170518     d c_RollUp        c                   const(x'F5')
004700170518
004800170518     d Digits          c                   const('0123456789')
004900170518
005000170518      //---------------------------------------------------------------
005100170518      //?Definizione schiere.
005200170518      //---------------------------------------------------------------
005300170518      // - Messaggi di errore
005400170518     d Msg             s             78    dim(30) ctdata perrcd(1)
005500170518
005600170518      //---------------------------------------------------------------
005700170518      //?Definizione aree dati.
005800170518      //---------------------------------------------------------------
005900170518      // - Dati utente
006000170518     d §AzUte        e ds                  extname(AZUTE00F)
006100170518     d                                     dtaara
006200170518     d §DatiUte      e ds                  extname(dDatiUte)
006300170518     d                                     dtaara
006400170518
006500170518      //---------------------------------------------------------------
006600170518      //?Definizione strutture dati.
006700170518      //---------------------------------------------------------------
006800170518      // - Status
006900170518     d Psds           sds
007000170518     d   SDSpgm          *proc
007100170518
007200170518      // - InfDS
007300170518     d InfDspF         ds
007400170518     d  dsp_aid              369    369a
007500170518
007600170518      // - Indicatori su DspF
007700170518     d IndDspF         ds
007800170518        // - Indicatori di errore in videata
007900170518     d  ErrMessage                    1n   overlay(IndDspF : 28)
008000170518        // - Indicatori di errore
008100170518     d  PosCurFIL                     1n   overlay(IndDspF : 50)
008200170518     d  PosCurDATADAL                 1n   overlay(IndDspF : 51)
008300170518     d  PosCurDATAAL                  1n   overlay(IndDspF : 52)
008400170518        // - Indicatori di errore generico
008500170518     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008600170518
008700170518     d WindDspF        ds                  inz
008800170518     d  WindDspFa              1     49    inz(*zero)
008900170518     d  WindDspFb             50     99    inz(*zero)
009000170518
009100170518      // - Parametri per pgm. controllo/inversione data?
009200170518     d wlbdat          ds                  inz
009300170518     d  G08dat                 1      8  0
009400170518     d  G08inv                 9     16  0
009500170518     d  G08err                17     17
009600170518     d  G08tgi                18     22  0
009700170518
009800170518      // - Parametri ricevuti
009900170518     d KPJBA         e ds
010000170518
010100170518      // - Parametri per Statistica
010200170518     d FIOR89DS      e ds
010300170518
010400170518      // - Reperimento dati utente
010500170518     d TIBS34DS      e ds
010600170518
010700170518      //---------------------------------------------------------------
010800170518      //?Definizione variabili globali.
010900170518      //---------------------------------------------------------------
011000170518      // - Flags booleani
011100170518     d Fine            s               n   inz(*off)
011200170518     d wInzD02         s               n   inz(*off)
011300170518
011400170518      // - Campi associati al video
011500170518     d Video           s              2a   inz('D2')
011600170518
011700170518      // - Campi di comodo data
011800170518     d Data_ISO        s               d   datfmt(*iso)
011900170518     d wdatadal        s              8s 0 inz
012000170518     d wdataal         s              8s 0 inz
012100170518
012200170518      // - Campi di comodo
012300170518     d Oggi            s              8s 0 inz
012400170518
012500170518      //---------------------------------------------------------------
012600170518      //?Definizione procedure esterne.
012700170518      //---------------------------------------------------------------
012800170518      // - Sottomissione lavoro batch
012900170518     d bch10           pr                  extpgm('BCH10')
013000170518     d  kpjba                              likeds(KPJBA)
013100170518
013200170518      //---------------------------------------------------------------
013300170518      //?Definizione prototipi.
013400170518      //---------------------------------------------------------------
013500170518      /copy gaitrasrc/srcprotopr,TIBS34R
013600170518      /copy gaitrasrc/srcprotopr,XSRDA8
013700170518
013800170518      //---------------------------------------------------------------
013900170518      //?Definizione key-list.
014000170518      //---------------------------------------------------------------
014100170518
014200170518      //---------------------------------------------------------------
014300170518      //?M A I N - L I N E
014400170518      //---------------------------------------------------------------
014500170518     c     *Entry        plist
014600170518     c                   parm                    kpjba
014700170518
014800170518       //?Operazioni iniziali
014900170518       exsr RoutInz;
015000170518
015100170518       //?Gestione video
015200170518       DOW  Fine = *off;
015300170518         SELECT;
015400170518           WHEN  Video = 'D2';
015500170518             exsr GesD02;
015600170518           OTHER;
015700170518             Fine = *on;
015800170518         ENDSL;
015900170518       ENDDO;
016000170518
016100170518       //?Operazioni finali
016200170518       exsr RoutEnd;
016300170518
016400170518       //--------------------------------------------------------------
016500170518       //?Operazioni iniziali.
016600170518       //--------------------------------------------------------------
016700170518       BEGSR RoutInz;
016800170518
016900170518       //?Impostazione campi "fissi"
017000170518         Video = 'D2';
017100170518         wInzD02 = *on;
017200170518         V01pgm = SDSpgm;
017300170518
017400170518       //?Imposto oggi
017500170518         Oggi = %dec(%date());
017600170518
017700170518       //?Reperimento dati job
017800170518         exsr DatiJob;
017900170518
018000170518       ENDSR;
018100170518
018200170518       //--------------------------------------------------------------
018300170518       //?Reperimento Dati del job (Utente/Operativi).
018400170518       //--------------------------------------------------------------
018500170518       BEGSR DatiJob;
018600170518
018700170518         in(E) §AzUte;
018800170518         IF  NOT %error;
018900170518           in(E) §DatiUte;
019000170518         ENDIF;
019100170518         IF  %error or RSut = *blanks;
019200170518           clear TIBS34ds;
019300170518           tibs34r(tibs34ds);
019400170518           in §AzUte;
019500170518           in §DatiUte;
019600170518         ENDIF;
019700170518
019800170518       ENDSR;
019900170518
020000170518       //--------------------------------------------------------------
020100170518       //?Gestione videata D02.
020200170518       //--------------------------------------------------------------
020300170518       BEGSR GesD02;
020400170518
020500170518       //?Inizializzazione videata
020600170518         IF  wInzD02;
020700170518           exsr InzD02;
020800170518           wInzD02 = *off;
020900170518         ENDIF;
021000170518
021100170518       //?Emissione videata
021200170518         write OR88T01;
021300170518         exfmt OR88D02;
021400170518         reset ErrMessage;
021500170518         reset ErrGenerico;
021600170518         clear V02msg;
021700170518
021800170518         SELECT;
021900170518
022000170518       //?- F03=Fine
022100170518         WHEN  dsp_aid = c_F03;
022200170518           exsr F03D02;
022300170518
022400170518       //?- F06=Conferma
022500170518         WHEN  dsp_aid = c_F06;
022600170518           exsr ContrD02;
022700170518           IF  ErrGenerico;
022800170518             leavesr;
022900170518           ENDIF;
023000170518           exsr F06D02;
023100170518           Fine = *on;
023200170518
023300170518       //?Invio
023400170518         OTHER;
023500170518           exsr ContrD02;
023600170518           IF  ErrGenerico;
023700170518             leavesr;
023800170518           ENDIF;
023900170518
024000170518         ENDSL;
024100170518
024200170518       ENDSR;
024300170518
024400170518       //--------------------------------------------------------------
024500170518       //?Inizializzazione Videata D02.
024600170518       //--------------------------------------------------------------
024700170518       BEGSR InzD02;
024800170518
024900170518       //?Pulizia videata
025000170518         clear V02fil;
025100170518         clear V02dfil;
025200170518         clear V02dtdal;
025300170518         clear V02dtal;
025400170518
025500170518       //?Imposto di default tutte le filiali
025600170518         V02fil = 999;
025700170518         V02dfil = 'Tutte le Filiali';
025800170518
025900170518       ENDSR;
026000170518
026100170518       //--------------------------------------------------------------
026200170518       //?Gestione tasto funzionale F03 da videata D02
026300170518       //?F03=Fine
026400170518       //--------------------------------------------------------------
026500170518       BEGSR F03D02;
026600170518
026700170518       //?Chiusura del programma
026800170518         Fine = *on;
026900170518
027000170518       ENDSR;
027100170518
027200170518       //--------------------------------------------------------------
027300170518       //?Gestione tasto funzionale F06 da videata D02
027400170518       //?F06=Conferma
027500170518       //--------------------------------------------------------------
027600170518       BEGSR F06D02;
027700170518
027800170518       //?Elaborazione statistica
027900170518         clear FIOR89DS;
028000170518         IOR89fil   = V02fil;
028100170518         IOR89datai = wdatadal;
028200170518         IOR89dataf = wdataal;
028300170518         kpjbu = FIOR89DS;
028400170518         kcoaz = 'OR89';
028500170518         clear kbuff;
028600170518         BCH10(kpjba);
028700170518
028800170518       ENDSR;
028900170518
029000170518       //--------------------------------------------------------------
029100170518       //?Controlla Videata D02.
029200170518       //--------------------------------------------------------------
029300170518       BEGSR ContrD02;
029400170518
029500170518         WindDspF = IndDspF;
029600170518         reset WindDspFb;
029700170518         IndDspF  = WindDspF;
029800170518
029900170518       //?Filiale gestione
030000170518         IF  V02fil = 999;
030100170518           V02dfil = 'Tutte le Filiali';
030200170518         ENDIF;
030300170518
030400170518         IF  V02fil = 0;
030500170518           clear V02dfil;
030600170518           ErrMessage  = *on;
030700170518           ErrGenerico = *on;
030800170518           PosCurFIL = *on;
030900170518           V02msg = Msg(01);
031000170518           leavesr;
031100170518         ENDIF;
031200170518
031300170518         IF  V02fil <> 999;
031400170518           clear V02dfil;
031500170518           chain V02fil AZORG01L;
031600170518           IF  not %found(AZORG01L) or
031700170518               ORGfva <> *blanks or
031800170518               ORGfag <> 'F';
031900170518             ErrMessage  = *on;
032000170518             ErrGenerico = *on;
032100170518             PosCurFIL = *on;
032200170518             V02msg = Msg(01);
032300170518             leavesr;
032400170518           ENDIF;
032500170518           V02dfil = ORGdes;
032600170518         ENDIF;
032700170518
032800170518       //?Data Dal
032900170518         IF  V02dtdal = *zeros;
033000170518           ErrMessage  = *on;
033100170518           ErrGenerico = *on;
033200170518           PosCurDATADAL = *on;
033300170518           V02msg = Msg(02);
033400170518           leavesr;
033500170518         ENDIF;
033600170518         clear wlbdat;
033700170518         G08dat = V02dtdal;
033800170518         xsrda8(wlbdat);
033900170518         IF  G08err = '1';
034000170518           ErrMessage  = *on;
034100170518           ErrGenerico = *on;
034200170518           PosCurDATADAL = *on;
034300170518           V02msg = Msg(02);
034400170518           leavesr;
034500170518         ENDIF;
034600170518         V02dtdal = G08dat;
034700170518         wdatadal = G08inv;
034800170518
034900170518       //?Data Al
035000170518         IF  V02dtal = *zeros;
035100170518           ErrMessage  = *on;
035200170518           ErrGenerico = *on;
035300170518           PosCurDATAAL = *on;
035400170518           V02msg = Msg(02);
035500170518           leavesr;
035600170518         ENDIF;
035700170518         clear wlbdat;
035800170518         G08dat = V02dtal;
035900170518         xsrda8(wlbdat);
036000170518         IF  G08err = '1';
036100170518           ErrMessage  = *on;
036200170518           ErrGenerico = *on;
036300170518           PosCurDATAAL = *on;
036400170518           V02msg = Msg(02);
036500170518           leavesr;
036600170518         ENDIF;
036700170518         V02dtal = G08dat;
036800170518         wdataal = G08inv;
036900170518
037000170922       //?Data AL non può essere maggiore di oggi
037100170518         IF  wdataal > Oggi;
037200170518           ErrMessage  = *on;
037300170518           ErrGenerico = *on;
037400170518           PosCurDATAAL = *on;
037500170518           V02msg = Msg(02);
037600170518           V02msg = %trim(V02msg) + ' Superiore alla data odierna';
037700170518           leavesr;
037800170518         ENDIF;
037900170518
038000170518       //?Data DAL congruente con AL
038100170518         IF  wdatadal > 0 and wdataal = 0;
038200170518           ErrMessage  = *on;
038300170518           ErrGenerico = *on;
038400170518           PosCurDATADAL = *on;
038500170518           V02msg = Msg(03);
038600170518           leavesr;
038700170518         ENDIF;
038800170518         IF  wdatadal > 0 and wdataal > 0 and wdatadal > wdataal;
038900170518           ErrMessage  = *on;
039000170518           ErrGenerico = *on;
039100170518           PosCurDATADAL = *on;
039200170518           V02msg = Msg(03);
039300170518           leavesr;
039400170518         ENDIF;
039500170518
039600170518       ENDSR;
039700170518
039800170518       //--------------------------------------------------------------
039900170518       //?Operazioni finali.
040000170518       //--------------------------------------------------------------
040100170518       BEGSR RoutEnd;
040200170518
040300170518         *inLR = *on;
040400170518         return;
040500170518
040600170518       ENDSR;
040700170518
040800170518** -- MSG -------------------------------------------------------------------*
040900170518Filiale errata                                                                 1
041000170518Data errata                                                                    2
041100170518Data DAL incongruente con data AL                                              3
