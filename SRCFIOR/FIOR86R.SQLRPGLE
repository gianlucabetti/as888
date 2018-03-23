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
010600170510
010700170510      // - Tabella 5A - Pulizia File
010800170510     d ds5A2         e ds
010900170510
011000170510      // - File Tabelle
011100170510     d TABEL00F      e ds                  extname(TABEL00F)
011200170509
011300170509      //---------------------------------------------------------------
011400170509      //?Definizione variabili globali.
011500170509      //---------------------------------------------------------------
011600170509      // - Flags booleani
011700170509     d Fine            s               n   inz(*off)
011800170509     d wInzD02         s               n   inz(*off)
011900170509
012000170509      // - Indici di schiera
012100170509     d xx              s              4s 0 inz
012200170509
012300170509      // - Campi associati al video
012400170509     d Video           s              2a   inz('D2')
012500170509
012600170509       // - Stringa SQL da eseguire
012700170509     d wSQL            s           2048    Varying        inz
012800170509
012900170509      // - Campi di comodo data
013000170509     d Data_ISO        s               d   datfmt(*iso)
013100170509     d wdatadal        s              8s 0 inz
013200170509     d wdataal         s              8s 0 inz
013300170509
013400170509      // - Campi di comodo
013500170510     d Oggi            s              8s 0 inz
013600170510     d PuliziaLog      s              8s 0 inz
013700170509
013800170509      //---------------------------------------------------------------
013900170509      //?Definizione procedure esterne.
014000170509      //---------------------------------------------------------------
014100170509      // - Sottomissione lavoro batch
014200170509     d bch10           pr                  extpgm('BCH10')
014300170509     d  kpjba                              likeds(KPJBA)
014400170509
014500170509      //---------------------------------------------------------------
014600170509      //?Definizione prototipi.
014700170509      //---------------------------------------------------------------
014800170509      /copy gaitrasrc/srcprotopr,TIBS34R
014900170509      /copy gaitrasrc/srcprotopr,XSRDA8
015000170509
015100170509      //---------------------------------------------------------------
015200170509      //?Definizione key-list.
015300170509      //---------------------------------------------------------------
015400170509
015500170509      //---------------------------------------------------------------
015600170509      //?M A I N - L I N E
015700170509      //---------------------------------------------------------------
015800170509     c     *Entry        plist
015900170509     c                   parm                    kpjba
016000170509
016100170509       //?Operazioni iniziali
016200170509       exsr RoutInz;
016300170509
016400170509       //?Gestione video
016500170509       DOW  Fine = *off;
016600170509         SELECT;
016700170509           WHEN  Video = 'D2';
016800170509             exsr GesD02;
016900170509           OTHER;
017000170509             Fine = *on;
017100170509         ENDSL;
017200170509       ENDDO;
017300170509
017400170509       //?Operazioni finali
017500170509       exsr RoutEnd;
017600170509
017700170509       //--------------------------------------------------------------
017800170509       //?Operazioni iniziali.
017900170509       //--------------------------------------------------------------
018000170509       BEGSR RoutInz;
018100170510
018200170510         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018300170509
018400170509       //?Impostazione campi "fissi"
018500170509         Video = 'D2';
018600170509         wInzD02 = *on;
018700170518         V01pgm = SDSpgm;
018800170509
018900170509       //?Imposto oggi
019000170509         Oggi = %dec(%date());
019100170509
019200170509       //?Reperimento dati job
019300170509         exsr DatiJob;
019400170510
019500170510       //?Cerco la data pulizia LOG PDA
019600170510         clear ds5A2;
019700170510         exec sql SELECT TABEL00F.* into :TABEL00F from TABEL00F
019800170510                  where TBLkut = 1 and TBLcod = '5A' and
019900170510                        substr(TBLkey, 1, 1) = '2' and
020000170510                        TBLflg = ' ';
020100170510         IF  sqlcod = 0;
020200170510           ds5A2 = TBLuni;
020300170510         ENDIF;
020400170510
020500170510         Data_ISO = %date(Oggi:*ISO);
020600170510         Data_ISO = Data_ISO - %days(§5Apdal);
020700170510         PuliziaLog = %dec(Data_ISO);
020800170509
020900170509       ENDSR;
021000170509
021100170509       //--------------------------------------------------------------
021200170509       //?Reperimento Dati del job (Utente/Operativi).
021300170509       //--------------------------------------------------------------
021400170509       BEGSR DatiJob;
021500170509
021600170509         in(E) §AzUte;
021700170509         IF  NOT %error;
021800170509           in(E) §DatiUte;
021900170509         ENDIF;
022000170509         IF  %error or RSut = *blanks;
022100170509           clear TIBS34ds;
022200170509           tibs34r(tibs34ds);
022300170509           in §AzUte;
022400170509           in §DatiUte;
022500170509         ENDIF;
022600170509
022700170509       ENDSR;
022800170509
022900170509       //--------------------------------------------------------------
023000170509       //?Gestione videata D02.
023100170509       //--------------------------------------------------------------
023200170509       BEGSR GesD02;
023300170509
023400170509       //?Inizializzazione videata
023500170509         IF  wInzD02;
023600170509           exsr InzD02;
023700170509           wInzD02 = *off;
023800170509         ENDIF;
023900170509
024000170509       //?Emissione videata
024100170509         write OR86T01;
024200170509         exfmt OR86D02;
024300170509         reset ErrMessage;
024400170509         reset ErrGenerico;
024500170509         clear V02msg;
024600170509
024700170509         SELECT;
024800170509
024900170509       //?- F03=Fine
025000170509         WHEN  dsp_aid = c_F03;
025100170509           exsr F03D02;
025200170509
025300170509       //?- F06=Conferma
025400170509         WHEN  dsp_aid = c_F06;
025500170509           exsr ContrD02;
025600170509           IF  ErrGenerico;
025700170509             leavesr;
025800170509           ENDIF;
025900170509           exsr F06D02;
026000170509           Fine = *on;
026100170509
026200170509       //?Invio
026300170509         OTHER;
026400170509           exsr ContrD02;
026500170509           IF  ErrGenerico;
026600170509             leavesr;
026700170509           ENDIF;
026800170509
026900170509         ENDSL;
027000170509
027100170509       ENDSR;
027200170509
027300170509       //--------------------------------------------------------------
027400170509       //?Inizializzazione Videata D02.
027500170509       //--------------------------------------------------------------
027600170509       BEGSR InzD02;
027700170509
027800170509       //?Pulizia videata
027900170509         clear V02fil;
028000170509         clear V02dfil;
028100170509         clear V02dtdal;
028200170509         clear V02dtal;
028300170509
028400170509       //?Imposto di default tutte le filiali
028500170509         V02fil = 999;
028600170509         V02dfil = 'Tutte le Filiali';
028700170509
028800170509       ENDSR;
028900170509
029000170509       //--------------------------------------------------------------
029100170509       //?Gestione tasto funzionale F03 da videata D02
029200170509       //?F03=Fine
029300170509       //--------------------------------------------------------------
029400170509       BEGSR F03D02;
029500170509
029600170509       //?Chiusura del programma
029700170509         Fine = *on;
029800170509
029900170509       ENDSR;
030000170509
030100170509       //--------------------------------------------------------------
030200170509       //?Gestione tasto funzionale F06 da videata D02
030300170509       //?F06=Conferma
030400170509       //--------------------------------------------------------------
030500170509       BEGSR F06D02;
030600170509
030700170509       //?Elaborazione statistica
030800170509         clear FIOR87DS;
030900170509         IOR87fil   = V02fil;
031000170511         IOR87datai = wdatadal;
031100170511         IOR87dataf = wdataal;
031200170509         kpjbu = FIOR87DS;
031300170509         kcoaz = 'OR87';
031400170509         clear kbuff;
031500170509         BCH10(kpjba);
031600170509
031700170509       ENDSR;
031800170509
031900170509       //--------------------------------------------------------------
032000170509       //?Controlla Videata D02.
032100170509       //--------------------------------------------------------------
032200170509       BEGSR ContrD02;
032300170509
032400170509         WindDspF = IndDspF;
032500170509         reset WindDspFb;
032600170509         IndDspF  = WindDspF;
032700170509
032800170509       //?Filiale gestione
032900170509         IF  V02fil = 999;
033000170509           V02dfil = 'Tutte le Filiali';
033100170509         ENDIF;
033200170509
033300170509         IF  V02fil = 0;
033400170509           clear V02dfil;
033500170509           ErrMessage  = *on;
033600170509           ErrGenerico = *on;
033700170509           PosCurFIL = *on;
033800170509           V02msg = Msg(01);
033900170509           leavesr;
034000170509         ENDIF;
034100170509
034200170509         IF  V02fil <> 999;
034300170509           clear V02dfil;
034400170509           chain V02fil AZORG01L;
034500170509           IF  not %found(AZORG01L) or
034600170509               ORGfva <> *blanks or
034700170509               ORGfag <> 'F';
034800170509             ErrMessage  = *on;
034900170509             ErrGenerico = *on;
035000170509             PosCurFIL = *on;
035100170509             V02msg = Msg(01);
035200170509             leavesr;
035300170509           ENDIF;
035400170509           V02dfil = ORGdes;
035500170509         ENDIF;
035600170509
035700170509       //?Data Dal
035800170509         IF  V02dtdal = *zeros;
035900170509           ErrMessage  = *on;
036000170509           ErrGenerico = *on;
036100170509           PosCurDATADAL = *on;
036200170509           V02msg = Msg(02);
036300170509           leavesr;
036400170509         ENDIF;
036500170509         clear wlbdat;
036600170509         G08dat = V02dtdal;
036700170509         xsrda8(wlbdat);
036800170509         IF  G08err = '1';
036900170509           ErrMessage  = *on;
037000170509           ErrGenerico = *on;
037100170509           PosCurDATADAL = *on;
037200170509           V02msg = Msg(02);
037300170509           leavesr;
037400170509         ENDIF;
037500170509         V02dtdal = G08dat;
037600170509         wdatadal = G08inv;
037700170510
037800170510       //?Data Dal non può essere inferiore alla pulizia File LOG PDA
037900170510         IF  wdatadal < PuliziaLog;
038000170510           ErrMessage  = *on;
038100170510           ErrGenerico = *on;
038200170510           PosCurDATADAL = *on;
038300170510           V02msg = Msg(02);
038400170510           V02msg = %trim(V02msg) + ' Inferiore alla puliza LOG PDA ' +
038500170510           %subst(%editc(PuliziaLog:'X'):7:2) + '/' +
038600170510           %subst(%editc(PuliziaLog:'X'):5:2) + '/' +
038700170510           %subst(%editc(PuliziaLog:'X'):1:4);
038800170510           leavesr;
038900170510         ENDIF;
039000170509
039100170509       //?Data Al
039200170509         IF  V02dtal = *zeros;
039300170509           ErrMessage  = *on;
039400170509           ErrGenerico = *on;
039500170509           PosCurDATAAL = *on;
039600170509           V02msg = Msg(02);
039700170509           leavesr;
039800170509         ENDIF;
039900170509         clear wlbdat;
040000170509         G08dat = V02dtal;
040100170509         xsrda8(wlbdat);
040200170509         IF  G08err = '1';
040300170509           ErrMessage  = *on;
040400170509           ErrGenerico = *on;
040500170509           PosCurDATAAL = *on;
040600170509           V02msg = Msg(02);
040700170509           leavesr;
040800170509         ENDIF;
040900170509         V02dtal = G08dat;
041000170509         wdataal = G08inv;
041100170510
041200170510       //?Data Al non può essere maggiore di oggi
041300170510         IF  wdataal > Oggi;
041400170510           ErrMessage  = *on;
041500170510           ErrGenerico = *on;
041600170510           PosCurDATAAL = *on;
041700170510           V02msg = Msg(02);
041800170510           V02msg = %trim(V02msg) + ' Superiore alla data odierna';
041900170510           leavesr;
042000170510         ENDIF;
042100170509
042200170509       //?Data DAL congruente con AL
042300170509         IF  wdatadal > 0 and wdataal = 0;
042400170509           ErrMessage  = *on;
042500170509           ErrGenerico = *on;
042600170509           PosCurDATADAL = *on;
042700170509           V02msg = Msg(03);
042800170509           leavesr;
042900170509         ENDIF;
043000170509         IF  wdatadal > 0 and wdataal > 0 and wdatadal > wdataal;
043100170509           ErrMessage  = *on;
043200170509           ErrGenerico = *on;
043300170509           PosCurDATADAL = *on;
043400170509           V02msg = Msg(03);
043500170509           leavesr;
043600170509         ENDIF;
043700170509
043800170509       ENDSR;
043900170509
044000170509       //--------------------------------------------------------------
044100170509       //?Operazioni finali.
044200170509       //--------------------------------------------------------------
044300170509       BEGSR RoutEnd;
044400170509
044500170509         *inLR = *on;
044600170509         return;
044700170509
044800170509       ENDSR;
044900170509
045000170509** -- MSG -------------------------------------------------------------------*
045100170509Filiale errata                                                                 1
045200170509Data errata                                                                    2
045300170509Data DAL incongruente con data AL                                              3
