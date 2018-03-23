000100170509      //---------------------------------------------------------------
000200170509      //?FIOR86R - Statistica Ritiri con peso < 5 Kg.
000300170509      //---------------------------------------------------------------
000400170509     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170509
000600170509      //---------------------------------------------------------------
000700170509      //?Dichiarazione file.
000800170509      //---------------------------------------------------------------
000900170509     fFIOR86D   cf   e             workstn
001000170509     f                                     indds(IndDspF)
001100170509     f                                     infds(InfDspF)
001200170509
001300170509      // - Organigramma
001400170509     fAZORG01L  if   e           k disk
001500170509
001600170509      //---------------------------------------------------------------
001700170509      //?Definizione costanti.
001800170509      //---------------------------------------------------------------
001900170509      // - Tasti funzionali a video
002000170509     d c_F01           c                   const(x'31')
002100170509     d c_F02           c                   const(x'32')
002200170509     d c_F03           c                   const(x'33')
002300170509     d c_F04           c                   const(x'34')
002400170509     d c_F05           c                   const(x'35')
002500170509     d c_F06           c                   const(x'36')
002600170509     d c_F07           c                   const(x'37')
002700170509     d c_F08           c                   const(x'38')
002800170509     d c_F09           c                   const(x'39')
002900170509     d c_F10           c                   const(x'3A')
003000170509     d c_F11           c                   const(x'3B')
003100170509     d c_F12           c                   const(x'3C')
003200170509     d c_F13           c                   const(x'B1')
003300170509     d c_F14           c                   const(x'B2')
003400170509     d c_F15           c                   const(x'B3')
003500170509     d c_F16           c                   const(x'B4')
003600170509     d c_F17           c                   const(x'B5')
003700170509     d c_F18           c                   const(x'B6')
003800170509     d c_F19           c                   const(x'B7')
003900170509     d c_F20           c                   const(x'B8')
004000170509     d c_F21           c                   const(x'B9')
004100170509     d c_F22           c                   const(x'BA')
004200170509     d c_F23           c                   const(x'BB')
004300170509     d c_F24           c                   const(x'BC')
004400170509     d c_Enter         c                   const(x'F1')
004500170509     d c_RollDown      c                   const(x'F4')
004600170509     d c_RollUp        c                   const(x'F5')
004700170509
004800170509     d Digits          c                   const('0123456789')
004900170509
005000170509      //---------------------------------------------------------------
005100170509      //?Definizione schiere.
005200170509      //---------------------------------------------------------------
005300170509      // - Messaggi di errore
005400170509     d Msg             s             78    dim(30) ctdata perrcd(1)
005500170509
005600170509      //---------------------------------------------------------------
005700170509      //?Definizione aree dati.
005800170509      //---------------------------------------------------------------
005900170509      // - Dati utente
006000170509     d §AzUte        e ds                  extname(AZUTE00F)
006100170509     d                                     dtaara
006200170509     d §DatiUte      e ds                  extname(dDatiUte)
006300170509     d                                     dtaara
006400170509
006500170509      //---------------------------------------------------------------
006600170509      //?Definizione strutture dati.
006700170509      //---------------------------------------------------------------
006800170509      // - Status
006900170509     d Psds           sds
007000170509     d   SDSpgm          *proc
007100170509
007200170509      // - InfDS
007300170509     d InfDspF         ds
007400170509     d  dsp_aid              369    369a
007500170509
007600170509      // - Indicatori su DspF
007700170509     d IndDspF         ds
007800170509        // - Indicatori di errore in videata
007900170509     d  ErrMessage                    1n   overlay(IndDspF : 28)
008000170509        // - Indicatori di errore
008100170509     d  PosCurFIL                     1n   overlay(IndDspF : 50)
008200170509     d  PosCurDATADAL                 1n   overlay(IndDspF : 51)
008300170509     d  PosCurDATAAL                  1n   overlay(IndDspF : 52)
008400170509        // - Indicatori di errore generico
008500170509     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008600170509
008700170509     d WindDspF        ds                  inz
008800170509     d  WindDspFa              1     49    inz(*zero)
008900170509     d  WindDspFb             50     99    inz(*zero)
009000170509
009100170509      // - Parametri per pgm. controllo/inversione data?
009200170509     d wlbdat          ds                  inz
009300170509     d  G08dat                 1      8  0
009400170509     d  G08inv                 9     16  0
009500170509     d  G08err                17     17
009600170509     d  G08tgi                18     22  0
009700170509
009800170509      // - Parametri ricevuti
009900170509     d KPJBA         e ds
010000170509
010100170509      // - Parametri per Statistica
010200170509     d FIOR87DS      e ds
010300170509
010400170509      // - Reperimento dati utente
010500170509     d TIBS34DS      e ds
010600170509
010700170509      //---------------------------------------------------------------
010800170509      //?Definizione variabili globali.
010900170509      //---------------------------------------------------------------
011000170509      // - Flags booleani
011100170509     d Fine            s               n   inz(*off)
011200170509     d wInzD02         s               n   inz(*off)
011300170509
011400170509      // - Indici di schiera
011500170509     d xx              s              4s 0 inz
011600170509
011700170509      // - Campi associati al video
011800170509     d Video           s              2a   inz('D2')
011900170509
012000170509       // - Stringa SQL da eseguire
012100170509     d wSQL            s           2048    Varying        inz
012200170509
012300170509      // - Campi di comodo data
012400170509     d Data_EUR        s               d   datfmt(*eur)
012500170509     d Data_ISO        s               d   datfmt(*iso)
012600170509     d wdatadal        s              8s 0 inz
012700170509     d wdataal         s              8s 0 inz
012800170509
012900170509      // - Campi di comodo
013000170509     d Oggi            s              8s 0 inz
013100170509
013200170509      //---------------------------------------------------------------
013300170509      //?Definizione procedure esterne.
013400170509      //---------------------------------------------------------------
013500170509      // - Sottomissione lavoro batch
013600170509     d bch10           pr                  extpgm('BCH10')
013700170509     d  kpjba                              likeds(KPJBA)
013800170509
013900170509      //---------------------------------------------------------------
014000170509      //?Definizione prototipi.
014100170509      //---------------------------------------------------------------
014200170509      /copy gaitrasrc/srcprotopr,TIBS34R
014300170509      /copy gaitrasrc/srcprotopr,XSRDA8
014400170509
014500170509      //---------------------------------------------------------------
014600170509      //?Definizione key-list.
014700170509      //---------------------------------------------------------------
014800170509
014900170509      //---------------------------------------------------------------
015000170509      //?M A I N - L I N E
015100170509      //---------------------------------------------------------------
015200170509     c     *Entry        plist
015300170509     c                   parm                    kpjba
015400170509
015500170509       //?Operazioni iniziali
015600170509       exsr RoutInz;
015700170509
015800170509       //?Gestione video
015900170509       DOW  Fine = *off;
016000170509         SELECT;
016100170509           WHEN  Video = 'D2';
016200170509             exsr GesD02;
016300170509           OTHER;
016400170509             Fine = *on;
016500170509         ENDSL;
016600170509       ENDDO;
016700170509
016800170509       //?Operazioni finali
016900170509       exsr RoutEnd;
017000170509
017100170509       //--------------------------------------------------------------
017200170509       //?Operazioni iniziali.
017300170509       //--------------------------------------------------------------
017400170509       BEGSR RoutInz;
017500170509
017600170509       //?Impostazione campi "fissi"
017700170509         Video = 'D2';
017800170509         wInzD02 = *on;
017900170509
018000170509       //?Imposto oggi
018100170509         Oggi = %dec(%date());
018200170509
018300170509       //?Reperimento dati job
018400170509         exsr DatiJob;
018500170509
018600170509       ENDSR;
018700170509
018800170509       //--------------------------------------------------------------
018900170509       //?Reperimento Dati del job (Utente/Operativi).
019000170509       //--------------------------------------------------------------
019100170509       BEGSR DatiJob;
019200170509
019300170509         in(E) §AzUte;
019400170509         IF  NOT %error;
019500170509           in(E) §DatiUte;
019600170509         ENDIF;
019700170509         IF  %error or RSut = *blanks;
019800170509           clear TIBS34ds;
019900170509           tibs34r(tibs34ds);
020000170509           in §AzUte;
020100170509           in §DatiUte;
020200170509         ENDIF;
020300170509
020400170509       ENDSR;
020500170509
020600170509       //--------------------------------------------------------------
020700170509       //?Gestione videata D02.
020800170509       //--------------------------------------------------------------
020900170509       BEGSR GesD02;
021000170509
021100170509       //?Inizializzazione videata
021200170509         IF  wInzD02;
021300170509           exsr InzD02;
021400170509           wInzD02 = *off;
021500170509         ENDIF;
021600170509
021700170509       //?Emissione videata
021800170509         write OR86T01;
021900170509         exfmt OR86D02;
022000170509         reset ErrMessage;
022100170509         reset ErrGenerico;
022200170509         clear V02msg;
022300170509
022400170509         SELECT;
022500170509
022600170509       //?- F03=Fine
022700170509         WHEN  dsp_aid = c_F03;
022800170509           exsr F03D02;
022900170509
023000170509       //?- F06=Conferma
023100170509         WHEN  dsp_aid = c_F06;
023200170509           exsr ContrD02;
023300170509           IF  ErrGenerico;
023400170509             leavesr;
023500170509           ENDIF;
023600170509           exsr F06D02;
023700170509           Fine = *on;
023800170509
023900170509       //?Invio
024000170509         OTHER;
024100170509           exsr ContrD02;
024200170509           IF  ErrGenerico;
024300170509             leavesr;
024400170509           ENDIF;
024500170509
024600170509         ENDSL;
024700170509
024800170509       ENDSR;
024900170509
025000170509       //--------------------------------------------------------------
025100170509       //?Inizializzazione Videata D02.
025200170509       //--------------------------------------------------------------
025300170509       BEGSR InzD02;
025400170509
025500170509       //?Pulizia videata
025600170509         clear V02fil;
025700170509         clear V02dfil;
025800170509         clear V02dtdal;
025900170509         clear V02dtal;
026000170509
026100170509       //?Imposto di default tutte le filiali
026200170509         V02fil = 999;
026300170509         V02dfil = 'Tutte le Filiali';
026400170509
026500170509       ENDSR;
026600170509
026700170509       //--------------------------------------------------------------
026800170509       //?Gestione tasto funzionale F03 da videata D02
026900170509       //?F03=Fine
027000170509       //--------------------------------------------------------------
027100170509       BEGSR F03D02;
027200170509
027300170509       //?Chiusura del programma
027400170509         Fine = *on;
027500170509
027600170509       ENDSR;
027700170509
027800170509       //--------------------------------------------------------------
027900170509       //?Gestione tasto funzionale F06 da videata D02
028000170509       //?F06=Conferma
028100170509       //--------------------------------------------------------------
028200170509       BEGSR F06D02;
028300170509
028400170509       //?Elaborazione statistica
028500170509         clear FIOR87DS;
028600170509         IOR87fil   = V02fil;
028700170509         IOR87datai = V02dtdal;
028800170509         IOR87dataf = V02dtal;
028900170509         kpjbu = FIOR87DS;
029000170509         kcoaz = 'OR87';
029100170509         clear kbuff;
029200170509         BCH10(kpjba);
029300170509
029400170509       ENDSR;
029500170509
029600170509       //--------------------------------------------------------------
029700170509       //?Controlla Videata D02.
029800170509       //--------------------------------------------------------------
029900170509       BEGSR ContrD02;
030000170509
030100170509         WindDspF = IndDspF;
030200170509         reset WindDspFb;
030300170509         IndDspF  = WindDspF;
030400170509
030500170509       //?Filiale gestione
030600170509         IF  V02fil = 999;
030700170509           V02dfil = 'Tutte le Filiali';
030800170509         ENDIF;
030900170509
031000170509         IF  V02fil = 0;
031100170509           clear V02dfil;
031200170509           ErrMessage  = *on;
031300170509           ErrGenerico = *on;
031400170509           PosCurFIL = *on;
031500170509           V02msg = Msg(01);
031600170509           leavesr;
031700170509         ENDIF;
031800170509
031900170509         IF  V02fil <> 999;
032000170509           clear V02dfil;
032100170509           chain V02fil AZORG01L;
032200170509           IF  not %found(AZORG01L) or
032300170509               ORGfva <> *blanks or
032400170509               ORGfag <> 'F';
032500170509             ErrMessage  = *on;
032600170509             ErrGenerico = *on;
032700170509             PosCurFIL = *on;
032800170509             V02msg = Msg(01);
032900170509             leavesr;
033000170509           ENDIF;
033100170509           V02dfil = ORGdes;
033200170509         ENDIF;
033300170509
033400170509       //?Data Dal
033500170509         IF  V02dtdal = *zeros;
033600170509           ErrMessage  = *on;
033700170509           ErrGenerico = *on;
033800170509           PosCurDATADAL = *on;
033900170509           V02msg = Msg(02);
034000170509           leavesr;
034100170509         ENDIF;
034200170509         clear wlbdat;
034300170509         G08dat = V02dtdal;
034400170509         xsrda8(wlbdat);
034500170509         IF  G08err = '1';
034600170509           ErrMessage  = *on;
034700170509           ErrGenerico = *on;
034800170509           PosCurDATADAL = *on;
034900170509           V02msg = Msg(02);
035000170509           leavesr;
035100170509         ENDIF;
035200170509         V02dtdal = G08dat;
035300170509         wdatadal = G08inv;
035400170509
035500170509       //?Data Al
035600170509         IF  V02dtal = *zeros;
035700170509           ErrMessage  = *on;
035800170509           ErrGenerico = *on;
035900170509           PosCurDATAAL = *on;
036000170509           V02msg = Msg(02);
036100170509           leavesr;
036200170509         ENDIF;
036300170509         clear wlbdat;
036400170509         G08dat = V02dtal;
036500170509         xsrda8(wlbdat);
036600170509         IF  G08err = '1';
036700170509           ErrMessage  = *on;
036800170509           ErrGenerico = *on;
036900170509           PosCurDATAAL = *on;
037000170509           V02msg = Msg(02);
037100170509           leavesr;
037200170509         ENDIF;
037300170509         V02dtal = G08dat;
037400170509         wdataal = G08inv;
037500170509
037600170509       //?Data DAL congruente con AL
037700170509         IF  wdatadal > 0 and wdataal = 0;
037800170509           ErrMessage  = *on;
037900170509           ErrGenerico = *on;
038000170509           PosCurDATADAL = *on;
038100170509           V02msg = Msg(03);
038200170509           leavesr;
038300170509         ENDIF;
038400170509         IF  wdatadal > 0 and wdataal > 0 and wdatadal > wdataal;
038500170509           ErrMessage  = *on;
038600170509           ErrGenerico = *on;
038700170509           PosCurDATADAL = *on;
038800170509           V02msg = Msg(03);
038900170509           leavesr;
039000170509         ENDIF;
039100170509
039200170509       ENDSR;
039300170509
039400170509       //--------------------------------------------------------------
039500170509       //?Operazioni finali.
039600170509       //--------------------------------------------------------------
039700170509       BEGSR RoutEnd;
039800170509
039900170509         *inLR = *on;
040000170509         return;
040100170509
040200170509       ENDSR;
040300170509
040400170509** -- MSG -------------------------------------------------------------------*
040500170509Filiale errata                                                                 1
040600170509Data errata                                                                    2
040700170509Data DAL incongruente con data AL                                              3
