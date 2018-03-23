000100170314      //---------------------------------------------------------------
000200170314      //?TNVRMB58R - Controllo ORM spostamento data ritiro
000300170314      //---------------------------------------------------------------
000400170314     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170314
000600170314      //---------------------------------------------------------------
000700170314      //?Dichiarazione file.
000800170314      //---------------------------------------------------------------
000900170314     fAZORG01L  if   e           k disk
001000170314     fFNORE01L  if   e           k disk
001100170315     fFNORG01L  if   e           k disk
001200170314     fTABEL00F  if   e           k disk
001300170315     fTNTBE01L  if   e           k disk
001400170314     fTNVRMB58D cf   e             workstn
001500170314     f                                     indds(IndDspF)
001600170314     f                                     infds(InfDspF)
001700170314     fTNVRMB58P o    e             printer
001800170314     f                                     oflind(*in99)
001900170314
002000170314      //---------------------------------------------------------------
002100170314      //?Definizione costanti.
002200170314      //---------------------------------------------------------------
002300170314      // - Tasti funzionali a video
002400170314     d c_F01           c                   const(x'31')
002500170314     d c_F02           c                   const(x'32')
002600170314     d c_F03           c                   const(x'33')
002700170314     d c_F04           c                   const(x'34')
002800170314     d c_F05           c                   const(x'35')
002900170314     d c_F06           c                   const(x'36')
003000170314     d c_F07           c                   const(x'37')
003100170314     d c_F08           c                   const(x'38')
003200170314     d c_F09           c                   const(x'39')
003300170314     d c_F10           c                   const(x'3A')
003400170314     d c_F11           c                   const(x'3B')
003500170314     d c_F12           c                   const(x'3C')
003600170314     d c_F13           c                   const(x'B1')
003700170314     d c_F14           c                   const(x'B2')
003800170314     d c_F15           c                   const(x'B3')
003900170314     d c_F16           c                   const(x'B4')
004000170314     d c_F17           c                   const(x'B5')
004100170314     d c_F18           c                   const(x'B6')
004200170314     d c_F19           c                   const(x'B7')
004300170314     d c_F20           c                   const(x'B8')
004400170314     d c_F21           c                   const(x'B9')
004500170314     d c_F22           c                   const(x'BA')
004600170314     d c_F23           c                   const(x'BB')
004700170314     d c_F24           c                   const(x'BC')
004800170314     d c_Enter         c                   const(x'F1')
004900170314     d c_RollDown      c                   const(x'F4')
005000170314     d c_RollUp        c                   const(x'F5')
005100170314
005200170314     d Digits          c                   const('0123456789')
005300170314
005400170314     d Telefonico      c                   const('M')
005500170314     d Mail            c                   const('E')
005600170314
005700170314      //---------------------------------------------------------------
005800170314      //?Definizione schiere.
005900170314      //---------------------------------------------------------------
006000170314      // - Sk Filiali Emissione
006100170314     d skFIL           s              3s 0 dim(999) inz
006200170314     d skDES           s             20a   dim(999) inz
006300170314     d skCAR           s              3s 0 dim(999) inz
006400170314     d skEMATT         s              9s 0 dim(999) inz
006500170314     d skEPOM          s              9s 0 dim(999) inz
006600170314     d skEDAOM         s              9s 0 dim(999) inz
006700170314     d skEDAO1M        s              9s 0 dim(999) inz
006800170314     d skEDAO2M        s              9s 0 dim(999) inz
006900170314     d skEDAOOM        s              9s 0 dim(999) inz
007000170314     d skEDAOP         s              9s 0 dim(999) inz
007100170314     d skEDAO1P        s              9s 0 dim(999) inz
007200170314     d skEDAO2P        s              9s 0 dim(999) inz
007300170314     d skEDAOOP        s              9s 0 dim(999) inz
007400170314
007500170314      // - Sk Filiali Ritiro
007600170314     d skPOR           s              3s 0 dim(999) inz
007700170314     d skGPD           s            256a   dim(999) inz
007800170314     d skRMATT         s              9s 0 dim(999) inz
007900170314     d skRPOM          s              9s 0 dim(999) inz
008000170314     d skRDAOM         s              9s 0 dim(999) inz
008100170314     d skRDAO1M        s              9s 0 dim(999) inz
008200170314     d skRDAO2M        s              9s 0 dim(999) inz
008300170314     d skRDAOOM        s              9s 0 dim(999) inz
008400170314     d skRDAOP         s              9s 0 dim(999) inz
008500170314     d skRDAO1P        s              9s 0 dim(999) inz
008600170314     d skRDAO2P        s              9s 0 dim(999) inz
008700170314     d skRDAOOP        s              9s 0 dim(999) inz
008800170315
008900170315      // - Sk Date ritiro
009000170315     d skData          s              8s 0 dim(999) inz
009100170314
009200170314      // - Messaggi di errore
009300170314     d Msg             s             78    dim(30) ctdata perrcd(1)
009400170314
009500170314      //---------------------------------------------------------------
009600170314      //?Definizione aree dati.
009700170314      //---------------------------------------------------------------
009800170314      // - Dati utente
009900170314     d §AzUte        e ds                  extname(AZUTE00F)
010000170314     d                                     dtaara
010100170314     d §DatiUte      e ds                  extname(dDatiUte)
010200170314     d                                     dtaara
010300170314
010400170314      //---------------------------------------------------------------
010500170314      //?Definizione strutture dati.
010600170314      //---------------------------------------------------------------
010700170314      // - Status
010800170314     d Psds           sds
010900170314     d   SDSpgm          *proc
011000170314
011100170314      // - InfDS
011200170314     d InfDspF         ds
011300170314     d  dsp_aid              369    369a
011400170314
011500170314      // - Indicatori su DspF
011600170314     d IndDspF         ds
011700170314        // - Indicatori di errore in videata
011800170314     d  ErrMessage                    1n   overlay(IndDspF : 28)
011900170314        // - Indicatori di errore
012000170314     d  PosCurDATA                    1n   overlay(IndDspF : 50)
012100170314     d  PosCurCAR1                    1n   overlay(IndDspF : 51)
012200170314     d  PosCurCAR2                    1n   overlay(IndDspF : 52)
012300170314     d  PosCurCAR3                    1n   overlay(IndDspF : 53)
012400170314     d  PosCurCAR4                    1n   overlay(IndDspF : 54)
012500170314     d  PosCurCAR5                    1n   overlay(IndDspF : 55)
012600170314        // - Indicatori di errore generico
012700170314     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012800170314
012900170314     d WindDspF        ds                  inz
013000170314     d  WindDspFa              1     49    inz(*zero)
013100170314     d  WindDspFb             50     99    inz(*zero)
013200170314
013300170314      // - Parametri per pgm. controllo/inversione data?
013400170314     d wlbdat          ds                  inz
013500170314     d  G08dat                 1      8  0
013600170314     d  G08inv                 9     16  0
013700170314     d  G08err                17     17
013800170314     d  G08tgi                18     22  0
013900170314
014000170314      // - Parametri ricevuti
014100170314     d KPJBA         e ds
014200170314
014300170314      // - Controllo cap/località/provincia
014400170314     d FNLV13DS      e ds
014500170314
014600170314      // - Reperimento dati utente
014700170314     d TIBS34DS      e ds
014800170314
014900170314      // - Instradamento
015000170314     d TISI95DS      e ds
015100170314
015200170314      // - Orari Servizi
015300170314     d TRULORSDS     e ds
015400170314     d TRULOR2DS     e ds
015500170314
015600170314      // Campo ORGDE0 - AZORG00F
015700170314     d OG140         e ds
015800170315
015900170315      // Campo ORMFLO - FNORM00F
016000170315     d dORM01        e ds
016100170314
016200170314      // ds per rcd 'DT' FNORE - Data Pronta Merce
016300170314     d dOREdt        e ds
016400170315
016500170315      // - Tabella GPD - Giorni Posticipo Data Ritiro
016600170315     d dGPD          e ds                  inz
016700170315     d  skGiorni               1     14s 0 dim(7)
016800170314
016900170314      // - Ds File FNORM00F
017000170314     d FNORM00F      e ds                  extname(FNORM00F)
017100170314
017200170314      //---------------------------------------------------------------
017300170314      //?Definizione variabili globali.
017400170314      //---------------------------------------------------------------
017500170314      // - Flags booleani
017600170314     d Fine            s               n   inz(*off)
017700170315     d Mattino         s               n   inz(*off)
017800170315     d Pomeriggio      s               n   inz(*off)
017900170314     d wEoF            s               n   inz(*off)
018000170314     d wInzD02         s               n   inz(*off)
018100170314     d wStampa         s               n   inz(*off)
018200170314
018300170314      // - Indici di schiera
018400170314     d indicex         s              4s 0 inz
018500170314     d indicey         s              4s 0 inz
018600170314     d xx              s              4s 0 inz
018700170314     d yy              s              4s 0 inz
018800170314
018900170314      // - Campi associati al video
019000170314     d Video           s              2a   inz('D2')
019100170314
019200170314      // - Campi di comodo data
019300170314     d Data_EUR        s               d   datfmt(*eur)
019400170314     d Data_ISO        s               d   datfmt(*iso)
019500170314     d wData           s              8s 0 inz
019600170314
019700170314       // - Stringa SQL da eseguire
019800170314     d wSQL            s           4096    Varying        inz
019900170314
020000170314      // - Campi di comodo
020100170314     d Codice          s                   like(ORMcor) inz
020200170314     d Oggi            s              8s 0 inz
020300170314     d wDay_Of_Week    s              2  0 inz
020400170314     d wora1           s              6  0
020500170314     d wora2           s              6  0
020600170314     d wora3           s              6  0
020700170314     d w0030           s              3s 0
020800170314
020900170314      //---------------------------------------------------------------
021000170314      //?Definizione procedure esterne.
021100170314      //---------------------------------------------------------------
021200170314      // - Controllo cap/località/provincia
021300170314     d fnlv13r         pr                  extpgm('FNLV13R')
021400170314     d  kpjba                       502a
021500170314     d  fnlv13ds                           likeds(FNLV13DS)
021600170314     d  tisi95ds                           likeds(TISI95DS)
021700170315
021800170315      // - Ricerca orari di servizio
021900170315     d trulorsr        pr                  extpgm('TRULORSR')
022000170315     d  kpjba                              likeds(KPJBA)
022100170315     d  trulorsds                          likeds(TRULORSDS)
022200170315     d  trulor2ds                          likeds(TRULOR2DS)
022300170315     d                                     options (*nopass)
022400170314
022500170314      //---------------------------------------------------------------
022600170314      //?Definizione prototipi.
022700170314      //---------------------------------------------------------------
022800170314      /copy gaitrasrc/srcprotopr,TIBS34R
022900170314      /copy gaitrasrc/srcprotopr,XSRDA8
023000170314
023100170314      //---------------------------------------------------------------
023200170314      //?Definizione key-list.
023300170314      //---------------------------------------------------------------
023400170314      // - File TABEL00F
023500170314     d k03tabel      e ds                  extname(TABEL00F:*key)
023600170314     d                                     prefix(k_)
023700170314
023800170314      //---------------------------------------------------------------
023900170314      //?Riepilogo indicatori.
024000170314      //---------------------------------------------------------------
024100170314      // 99 : Salto pagina
024200170314
024300170314      //---------------------------------------------------------------
024400170314      //?M A I N - L I N E
024500170314      //---------------------------------------------------------------
024600170314     c     *Entry        plist
024700170314     c                   parm                    kpjba
024800170314
024900170314      /free
025000170314
025100170314       //?Operazioni iniziali
025200170314       exsr RoutInz;
025300170314
025400170314       //?Gestione video
025500170314       DOW  Fine = *off;
025600170314         SELECT;
025700170314           WHEN  Video = 'D2';
025800170314             exsr GesD02;
025900170314           OTHER;
026000170314             Fine = *on;
026100170314         ENDSL;
026200170314       ENDDO;
026300170314
026400170314       //?Operazioni finali
026500170314       exsr RoutEnd;
026600170314
026700170314       //--------------------------------------------------------------
026800170314       //?Operazioni iniziali.
026900170314       //--------------------------------------------------------------
027000170314       BEGSR RoutInz;
027100170314
027200170314         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
027300170314
027400170314       //?Impostazione campi "fissi"
027500170314         V01pgm = SDSpgm;
027600170314         Video = 'D2';
027700170314         wInzD02 = *on;
027800170314         k_TBLkut = 1;
027900170314
028000170314       //?Imposto oggi
028100170314         Oggi = %dec(%date());
028200170314
028300170314       //?Reperimento dati job
028400170314         exsr DatiJob;
028500170314
028600170314       //?Carico in sk le filiali abilitate al posticipo data
028700170314         clear skPOR;
028800170314         clear skGPD;
028900170314         clear xx;
029000170314         TBEcod = 'GPD';
029100170314         setll TBEcod TNTBE01L;
029200170314         reade TBEcod TNTBE01L;
029300170314         DOW  not %eof(TNTBE01L);
029400170314           IF  %subst(TBEke1:1:3) <> '999';
029500170314             xx += 1;
029600170314             skPOR(xx) = %int(%subst(TBEke1:1:3));
029700170314             skGPD(xx) = TBEuni;
029800170314           ENDIF;
029900170314           reade TBEcod TNTBE01L;
030000170314         ENDDO;
030100170314
030200170314       ENDSR;
030300170314
030400170314       //--------------------------------------------------------------
030500170314       //?Reperimento Dati del job (Utente/Operativi).
030600170314       //--------------------------------------------------------------
030700170314       BEGSR DatiJob;
030800170314
030900170314         in(E) §AzUte;
031000170314         IF  NOT %error;
031100170314           in(E) §DatiUte;
031200170314         ENDIF;
031300170314         IF  %error or RSut = *blanks;
031400170314           clear TIBS34ds;
031500170314           tibs34r(tibs34ds);
031600170314           in §AzUte;
031700170314           in §DatiUte;
031800170314         ENDIF;
031900170314
032000170314       ENDSR;
032100170314
032200170314       //--------------------------------------------------------------
032300170314       //?Gestione videata D02.
032400170314       //--------------------------------------------------------------
032500170314       BEGSR GesD02;
032600170314
032700170314       //?Inizializzazione videata
032800170314         IF  wInzD02;
032900170314           exsr InzD02;
033000170314           wInzD02 = *off;
033100170314         ENDIF;
033200170314
033300170314       //?Emissione Testata
033400170314         write  MB58T01;
033500170314
033600170314       //?Emissione videata
033700170314         exfmt  MB58D02;
033800170314         reset ErrMessage;
033900170314         reset ErrGenerico;
034000170314         clear V02msg;
034100170314
034200170314         SELECT;
034300170314
034400170314       //?- F03=Fine
034500170314           WHEN  dsp_aid = c_F03;
034600170314             exsr F03D02;
034700170314
034800170314       //?- F06=Conferma
034900170314           WHEN  dsp_aid = c_F06;
035000170314             exsr ContrD02;
035100170314             IF  ErrGenerico;
035200170314               leavesr;
035300170314             ENDIF;
035400170314             exsr F06D02;
035500170314
035600170314       //?Invio
035700170314           OTHER;
035800170314             exsr ContrD02;
035900170314             IF  ErrGenerico;
036000170314               leavesr;
036100170314             ENDIF;
036200170314
036300170314         ENDSL;
036400170314
036500170314       ENDSR;
036600170314
036700170314       //--------------------------------------------------------------
036800170314       //?Inizializzazione Videata D02.
036900170314       //--------------------------------------------------------------
037000170314       BEGSR InzD02;
037100170314
037200170314       //?Pulizia videata
037300170314         clear MB58D02;
037400170314
037500170314       ENDSR;
037600170314
037700170314       //--------------------------------------------------------------
037800170314       //?Gestione tasto funzionale F03 da videata D02
037900170314       //?F03=Fine
038000170314       //--------------------------------------------------------------
038100170314       BEGSR F03D02;
038200170314
038300170314       //?Chiusura del programma
038400170314         Fine = *on;
038500170314
038600170314       ENDSR;
038700170314
038800170314       //--------------------------------------------------------------
038900170314       //?Gestione tasto funzionale F06 da videata D02
039000170314       //?F06=Conferma
039100170314       //--------------------------------------------------------------
039200170314       BEGSR F06D02;
039300170314
039400170314       //?Come prima cosa carico in sk tutte le filiali delle aree da elaborare
039500170314         clear skFIL;
039600170314         clear skDES;
039700170314         clear xx;
039800170314         w0030 = 1;
039900170314         setll w0030 AZORG01L;
040000170314         read AZORG01L;
040100170314         DOW not %eof(AZORG01L);
040200170314           IF  ORGfva = *blanks and (ORGfag = 'F' or ORGfag = 'A');
040300170314             OG140 = ORGde0;
040400170315             IF  (§OGara = %editc(V02car1:'X') and V02car1 > 0) or
040500170315                 (§OGara = %editc(V02car2:'X') and V02car2 > 0) or
040600170315                 (§OGara = %editc(V02car3:'X') and V02car3 > 0) or
040700170315                 (§OGara = %editc(V02car4:'X') and V02car4 > 0) or
040800170315                 (§OGara = %editc(V02car5:'X') and V02car5 > 0);
040900170314               xx += 1;
041000170314               skFIL(xx) = ORGfil;
041100170314               skDES(xx) = ORGdes;
041200170314               skCAR(xx) = %int(§OGara);
041300170314             ENDIF;
041400170314           ENDIF;
041500170314           read AZORG01L;
041600170314         ENDDO;
041700170314
041800170314       //?Cerco che giorno della settimana sto elaborando
041900170314         Data_ISO = %date(wdata);
042000170314         clear wDay_Of_Week;
042100170314         exec sql
042200170314         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
042300170314
042400170314         wEoF = *off;
042500170314
042600170314       //?Poi leggo gli ORM
042700170314         exsr PreparaSQL;
042800170314
042900170314       //?Dichiarazione cursore
043000170314         exec sql
043100170314         prepare S1 from :wSQL;
043200170314         exec sql
043300170314         DECLARE ORM CURSOR for S1;
043400170314
043500170314       //?Apertura del cursore
043600170314         exec sql
043700170314         OPEN ORM;
043800170314
043900170314         IF  sqlcode < 0;
044000170314           wEoF = *on;
044100170314           exec sql CLOSE ORM;
044200170314           leavesr;
044300170314         ENDIF;
044400170314
044500170314         DOW  not wEoF;
044600170314           exec sql
044700170314           FETCH NEXT from ORM into :FNORM00F;
044800170314           IF  sqlcod = 100 or sqlcod < 0;
044900170314             wEoF = *on;
045000170314             leave;
045100170314           ENDIF;
045200170314
045300170314           dORM01 = ORMflo;
045400170314
045500170314         //?Aggancio ORG
045600170314           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
045700170314
045800170314         //?Cerco la data pronta merce
045900170314           clear dOREDT;
046000170314           OREtrc = 'DT';
046100170314           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
046200170314           IF  %found(FNORE01L);
046300170314             dOREDT = OREdati;
046400170314           ENDIF;
046500170315
046600170315         //?Elaboro solo quelli con data pronta merce = data immissione
046700170315         //?perchè se data pronta merce > non si posticipa la data
046800170315           IF  §OREdpm > ORmdao;
046900170315             iter;
047000170315           ENDIF;
047100170314
047200170314         //?Cerco instradamento
047300170314           exsr call_FNLV13;
047400170314
047500170314         //?Recupero orari limite ritiro in giornata
047600170314           exsr call_TRULORS;
047700170314
047800170314         //?Memorizzo le varie sk da stampare
047900170314           exsr Elabora;
048000170314
048100170314         ENDDO;
048200170314
048300170314       //?Chiusura del cursore
048400170314         exec sql CLOSE ORM;
048500170315
048600170315       //?Stampa totali
048700170315         exsr Stampa;
048800170315
048900170315       //?Chiusura del programma
049000170315         Fine = *on;
049100170314
049200170314       ENDSR;
049300170314
049400170314       //--------------------------------------------------------------
049500170314       //?Controlla Videata D02.
049600170314       //--------------------------------------------------------------
049700170314       BEGSR ContrD02;
049800170314
049900170314         WindDspF = IndDspF;
050000170314         reset WindDspFb;
050100170314         IndDspF  = WindDspF;
050200170314
050300170314       //?Data Immissione
050400170314         IF  V02data = *zeros;
050500170314           ErrMessage  = *on;
050600170314           ErrGenerico = *on;
050700170314           PosCurDATa = *on;
050800170315           V02msg = Msg(01);
050900170314           leavesr;
051000170314         ENDIF;
051100170314         clear wlbdat;
051200170314         G08dat = V02data;
051300170314         xsrda8(wlbdat);
051400170314         IF  G08err = '1';
051500170314           ErrMessage  = *on;
051600170314           ErrGenerico = *on;
051700170314           PosCurDATA = *on;
051800170314           V02msg = Msg(01);
051900170314           leavesr;
052000170314         ENDIF;
052100170314         V02data = G08dat;
052200170314         wdata   = G08inv;
052300170314
052400170314       //?Area
052500170314         IF  V02car1 = 0 and V02car2 = 0 and V02car3 = 0 and
052600170314             V02car4 = 0 and V02car5 = 0;
052700170314           ErrMessage  = *on;
052800170314           ErrGenerico = *on;
052900170314           PosCurCAR1 = *on;
053000170314           V02msg = Msg(02);
053100170314           leavesr;
053200170314         ENDIF;
053300170314         IF  V02car1 > 0;
053400170314           k_tblcod = '05';
053500170314           k_tblkey = %editc(V02car1:'X');
053600170314           chain  %kds(K03tabel) TABEL00F;
053700170314           IF  not %found( TABEL00F );
053800170314             ErrMessage  = *on;
053900170314             ErrGenerico = *on;
054000170314             PosCurCAR1 = *on;
054100170314             V02msg = Msg(02);
054200170314             leavesr;
054300170314           ENDIF;
054400170314         ENDIF;
054500170314         IF  V02car2 > 0;
054600170314           k_tblcod = '05';
054700170314           k_tblkey = %editc(V02car2:'X');
054800170314           chain  %kds(K03tabel) TABEL00F;
054900170314           IF  not %found( TABEL00F );
055000170314             ErrMessage  = *on;
055100170314             ErrGenerico = *on;
055200170314             PosCurCAR2 = *on;
055300170314             V02msg = Msg(02);
055400170314             leavesr;
055500170314           ENDIF;
055600170314         ENDIF;
055700170314         IF  V02car3 > 0;
055800170314           k_tblcod = '05';
055900170314           k_tblkey = %editc(V02car3:'X');
056000170314           chain  %kds(K03tabel) TABEL00F;
056100170314           IF  not %found( TABEL00F );
056200170314             ErrMessage  = *on;
056300170314             ErrGenerico = *on;
056400170314             PosCurCAR3 = *on;
056500170314             V02msg = Msg(02);
056600170314             leavesr;
056700170314           ENDIF;
056800170314         ENDIF;
056900170314         IF  V02car4 > 0;
057000170314           k_tblcod = '05';
057100170314           k_tblkey = %editc(V02car4:'X');
057200170314           chain  %kds(K03tabel) TABEL00F;
057300170314           IF  not %found( TABEL00F );
057400170314             ErrMessage  = *on;
057500170314             ErrGenerico = *on;
057600170314             PosCurCAR4 = *on;
057700170314             V02msg = Msg(02);
057800170314             leavesr;
057900170314           ENDIF;
058000170314         ENDIF;
058100170314         IF  V02car5 > 0;
058200170314           k_tblcod = '05';
058300170314           k_tblkey = %editc(V02car5:'X');
058400170314           chain  %kds(K03tabel) TABEL00F;
058500170314           IF  not %found( TABEL00F );
058600170314             ErrMessage  = *on;
058700170314             ErrGenerico = *on;
058800170314             PosCurCAR5 = *on;
058900170314             V02msg = Msg(02);
059000170314             leavesr;
059100170314           ENDIF;
059200170314         ENDIF;
059300170314
059400170314       ENDSR;
059500170314
059600170314       //--------------------------------------------------------------
059700170314       //?Preparo la stringa SQL.
059800170314       //--------------------------------------------------------------
059900170314       BEGSR PreparaSQL;
060000170314
060100170314       //?Leggo ORM manuali
060200170314       //?          con peso <= 5 Kg.
060300170314       //?          non codificati ORD/MITT
060400170315         wSQL = 'SELECT * from FNORM00F where ORMtco ' +
060500170315                'in(''M'', ''E'') and ORMpkg <= 5 ' +
060600170314                'and ORMcor = ' + %editc(Codice:'X') + ' ' +
060700170314                'and ORMcra = ' + %editc(Codice:'X') + ' ' +
060800170314       //?          immessi in data richiesta
060900170314                'and ORMdao = ' + %editc(wdata:'X') + ' ' +
061000170314       //?          dalle filiali delle aree richieste
061100170314                'and ORMpoe in(';
061200170314         indicey = 0;
061300170314         indicex = 1;
061400170315         FOR  indicex by 1 to %elem(skFIL);
061500170315           IF  skFIL(indicex) <> *zeros;
061600170315             IF  indicey > 0;
061700170314               wSQL += ', ';
061800170314             ELSE;
061900170314               indicey = 1;
062000170314             ENDIF;
062100170314             wSQL += %editc(skFIL(indicex):'X');
062200170314           ENDIF;
062300170314         ENDFOR;
062400170314       //?          con filiale ritiro attiva al posticipo data
062500170314         wSQL += ')';
062600170314         wSQL += ' and ORMpor in(';
062700170314         indicey = 0;
062800170314         indicex = 1;
062900170315         FOR  indicex by 1 to %elem(skPOR);
063000170315           IF  skPOR(indicex) <> *zeros;
063100170315             IF  indicey > 0;
063200170314               wSQL += ', ';
063300170314             ELSE;
063400170314               indicey = 1;
063500170314             ENDIF;
063600170315             wSQL += %editc(skPOR(indicex):'X');
063700170314           ENDIF;
063800170314         ENDFOR;
063900170314         wSQL += ')';
064000170314
064100170314         wSQL += ' ORDER BY ORMpoe, ORMnsr, ORMnor, ORMnrv';
064200170314
064300170314       ENDSR;
064400170314
064500170314       //--------------------------------------------------------------
064600170314       //?Cerco l'instradamento.
064700170314       //--------------------------------------------------------------
064800170314       BEGSR call_FNLV13;
064900170314
065000170314         clear TISI95DS;
065100170314         clear FNLV13DS;
065200170314         I95tcn = '7';
065300170314         I95nar = ORMnar;
065400170314         I95cap = ORMcar;
065500170314         I95loc = ORMlor;
065600170314         I95prv = ORMprr;
065700170314         I95ind = ORMinr;
065800170314         I95dat = %dec(%date());
065900170314         I95lkg = ORGpkg;
066000170314         I95lmc = ORGvlm;
066100170314         I13af0 = 'S';
066200170314         I13cnv = 'S';
066300170314         I13la3 = 'S';
066400170314
066500170314       //?controllo cap/località/provincia
066600170314         fnlv13r (kpjba:fnlv13ds:tisi95ds);
066700170314
066800170314       ENDSR;
066900170314
067000170314       //--------------------------------------------------------------
067100170314       //?Cerco orari limiti.
067200170314       //--------------------------------------------------------------
067300170314       BEGSR call_TRULORS;
067400170314
067500170314         clear trulorsds;
067600170314         clear trulor2ds;
067700170314         IOREfil = ORMpor;
067800170314         IOREcap = ORMcar;
067900170314         IOREloc = ORMlor;
068000170315         IOREdta = ORMdao;
068100170314         IOREtser = 'R';
068200170314         IOREgf2 = O95gf2;
068300170314         trulorsr (kpjba:trulorsds:trulor2ds);
068400170314         IF  OOREerr <> *blanks or OOR2err <> *blanks;
068500170314           leavesr;
068600170314         ENDIF;
068700170314         wora1 = OOR2lrsc * 100;
068800170314         wora2 = OOR2lrnc * 100;
068900170314         wora3 = OOR2lrnk * 100;
069000170314
069100170314       ENDSR;
069200170314
069300170314       //--------------------------------------------------------------
069400170314       //?Elabora.
069500170314       //--------------------------------------------------------------
069600170314       BEGSR Elabora;
069700170314
069800170314         clear xx;
069900170314         clear yy;
070000170315         Mattino = *off;
070100170315         Pomeriggio = *off;
070200170314
070300170314       //?sk filiale emissione
070400170314         xx = %lookup (ORMpoe:skFIL);
070500170314       //?sk filiale ritiro
070600170315         yy = %lookup (ORMpor:skPOR);
070700170315
070800170315       //?Cerco i gg di posticipo della filiale ritiro
070900170315         dGPD = skGPD(yy);
071000170315       //?Carico la sk delle date di ritiro possibili
071100170315         Data_ISO = %date(ORMdao);
071200170315         clear skData;
071300170315         indicex = 1;
071400170315         FOR  indicex by 1 to skGiorni(wDay_Of_Week);
071500170315           Data_ISO = Data_ISO + %days(1);
071600170315           skData(indicex) = %dec(Data_ISO);
071700170315         ENDFOR;
071800170314
071900170314       //?Se ORM commissionato immesso al mattino entro la prima ORA
072000170314       //?Se ORM NON commissionato immesso al mattino entro la seconda ORA
072100170315         IF  (§orcom = 'S' and ORMoao <= wora1) or
072200170315              §orcom = 'N' and ORMoao <= wora2;
072300170314           skEMATT(xx) += 1;
072400170314           skRMATT(yy) += 1;
072500170315           Mattino = *on;
072600170314         ELSE;
072700170314           skEPOM(xx) += 1;
072800170314           skRPOM(yy) += 1;
072900170315           Pomeriggio = *on;
073000170314         ENDIF;
073100170314
073200170315       //?Data Immissione = Data Ritiro
073300170315         IF  ORMdao = ORMdar;
073400170315           IF  Mattino;
073500170315             skEDAOM(xx) += 1;
073600170315             skRDAOM(yy) += 1;
073700170315           ENDIF;
073800170315           IF  Pomeriggio;
073900170315             skEDAOP(xx) += 1;
074000170315             skRDAOP(yy) += 1;
074100170315           ENDIF;
074200170314         ENDIF;
074300170315
074400170315       //?Data Ritiro = Data Immissione + 1 gg
074500170315         IF  ORMdar = skData(01);
074600170315           IF  Mattino;
074700170315             skEDAO1M(xx) += 1;
074800170315             skRDAO1M(yy) += 1;
074900170315           ENDIF;
075000170315           IF  Pomeriggio;
075100170315             skEDAO1P(xx) += 1;
075200170315             skRDAO1P(yy) += 1;
075300170315           ENDIF;
075400170315         ENDIF;
075500170315
075600170315       //?Data Ritiro = Data Immissione + 2 gg
075700170315         IF  ORMdar = skData(02);
075800170315           IF  Mattino;
075900170315             skEDAO2M(xx) += 1;
076000170315             skRDAO2M(yy) += 1;
076100170315           ENDIF;
076200170315           IF  Pomeriggio;
076300170315             skEDAO2P(xx) += 1;
076400170315             skRDAO2P(yy) += 1;
076500170315           ENDIF;
076600170315         ENDIF;
076700170315       //?Data Ritiro = Data Immissione + oltre gg
076800170315         IF  ORMdar > skData(02);
076900170315           IF  Mattino;
077000170315             skEDAOOM(xx) += 1;
077100170315             skRDAOOM(yy) += 1;
077200170315           ENDIF;
077300170315           IF  Pomeriggio;
077400170315             skEDAOOP(xx) += 1;
077500170315             skRDAOOP(yy) += 1;
077600170315           ENDIF;
077700170315         ENDIF;
077800170314
077900170314       ENDSR;
078000170314
078100170314       //--------------------------------------------------------------
078200170314       //?Stampa di controllo.
078300170314       //--------------------------------------------------------------
078400170314       BEGSR Stampa;
078500170314
078600170314         IF  not wStampa or *in99;
078700170315           write MB5801P;
078800170315           wStampa = *on;
078900170315           *in99 = *off;
079000170315         ENDIF;
079100170315
079200170315         data = V02data;
079300170315         write MB5802P;
079400170315
079500170315       //?Prima stampo i totali per filiale emissione
079600170315         write MB5803P;
079700170315         write MB5804P;
079800170315         indicex = 1;
079900170315         FOR  indicex by 1 to %elem(skFIL);
080000170315         //?Prima stampo il mattino
080100170315           IF  skFIL(indicex) <> *zeros;
080200170315             fil = skFIL(indicex);
080300170315             des = skDES(indicex);
080400170315             tot = skEMATT(indicex);
080500170315             dao0 = skEDAOM(indicex);
080600170315             dao1 = skEDAO1M(indicex);
080700170315             dao2 = skEDAO2M(indicex);
080800170315             daoo = skEDAOOM(indicex);
080900170315             IF  *in99;
081000170315               write MB5801P;
081100170315               write MB5803P;
081200170315               write MB5804P;
081300170315               *in99 = *off;
081400170315             ENDIF;
081500170315             write MB5805P;
081600170315         //?Poi stampo il pomeriggio
081700170315             clear fil;
081800170315             clear des;
081900170315             tot = skEPOM(indicex);
082000170315             dao0 = skEDAOP(indicex);
082100170315             dao1 = skEDAO1P(indicex);
082200170315             dao2 = skEDAO2P(indicex);
082300170315             daoo = skEDAOOP(indicex);
082400170315             IF  *in99;
082500170315               write MB5801P;
082600170315               write MB5803P;
082700170315               write MB5804P;
082800170315               *in99 = *off;
082900170315             ENDIF;
083000170315             write MB5805P;
083100170315           ENDIF;
083200170315         ENDFOR;
083300170315
083400170315       //?Poi stampo i totali per filiale ritiro
083500170315         write MB5803P1;
083600170315         write MB5804P;
083700170315         indicex = 1;
083800170315         FOR  indicex by 1 to %elem(skPOR);
083900170315         //?Prima stampo il mattino
084000170315           IF  skPOR(indicex) <> *zeros;
084100170315             fil = skPOR(indicex);
084200170315             clear des;
084300170315             tot = skRMATT(indicex);
084400170315             dao0 = skRDAOM(indicex);
084500170315             dao1 = skRDAO1M(indicex);
084600170315             dao2 = skRDAO2M(indicex);
084700170315             daoo = skRDAOOM(indicex);
084800170315             IF  *in99;
084900170315               write MB5803P1;
085000170315               write MB5804P;
085100170315               *in99 = *off;
085200170315             ENDIF;
085300170315             write MB5805P;
085400170315             clear fil;
085500170315             clear des;
085600170315             tot = skRPOM(indicex);
085700170315             dao0 = skRDAOP(indicex);
085800170315             dao1 = skRDAO1P(indicex);
085900170315             dao2 = skRDAO2P(indicex);
086000170315             daoo = skRDAOOP(indicex);
086100170315             IF  *in99;
086200170315               write MB5803P1;
086300170315               write MB5804P;
086400170315               *in99 = *off;
086500170315             ENDIF;
086600170315             write MB5805P;
086700170315           ENDIF;
086800170315         ENDFOR;
086900170314
087000170314       ENDSR;
087100170314
087200170314       //--------------------------------------------------------------
087300170314       //?Operazioni finali.
087400170314       //--------------------------------------------------------------
087500170314       BEGSR RoutEnd;
087600170314
087700170314         *inLR = *on;
087800170314         return;
087900170314
088000170314       ENDSR;
088100170314
088200170314      /end-free
088300170314** -- MSG -------------------------------------------------------------------*
088400170314Data errata                                                                    1
088500170314Area errata                                                                    2
