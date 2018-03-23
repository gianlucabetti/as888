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
001200170315     fTNTBE01L  if   e           k disk
001300170314     fTNVRMB58D cf   e             workstn
001400170314     f                                     indds(IndDspF)
001500170314     f                                     infds(InfDspF)
001600170314     fTNVRMB58P o    e             printer
001700170314     f                                     oflind(*in99)
001800170314
001900170314      //---------------------------------------------------------------
002000170314      //?Definizione costanti.
002100170314      //---------------------------------------------------------------
002200170314      // - Tasti funzionali a video
002300170314     d c_F01           c                   const(x'31')
002400170314     d c_F02           c                   const(x'32')
002500170314     d c_F03           c                   const(x'33')
002600170314     d c_F04           c                   const(x'34')
002700170314     d c_F05           c                   const(x'35')
002800170314     d c_F06           c                   const(x'36')
002900170314     d c_F07           c                   const(x'37')
003000170314     d c_F08           c                   const(x'38')
003100170314     d c_F09           c                   const(x'39')
003200170314     d c_F10           c                   const(x'3A')
003300170314     d c_F11           c                   const(x'3B')
003400170314     d c_F12           c                   const(x'3C')
003500170314     d c_F13           c                   const(x'B1')
003600170314     d c_F14           c                   const(x'B2')
003700170314     d c_F15           c                   const(x'B3')
003800170314     d c_F16           c                   const(x'B4')
003900170314     d c_F17           c                   const(x'B5')
004000170314     d c_F18           c                   const(x'B6')
004100170314     d c_F19           c                   const(x'B7')
004200170314     d c_F20           c                   const(x'B8')
004300170314     d c_F21           c                   const(x'B9')
004400170314     d c_F22           c                   const(x'BA')
004500170314     d c_F23           c                   const(x'BB')
004600170314     d c_F24           c                   const(x'BC')
004700170314     d c_Enter         c                   const(x'F1')
004800170314     d c_RollDown      c                   const(x'F4')
004900170314     d c_RollUp        c                   const(x'F5')
005000170314
005100170314     d Digits          c                   const('0123456789')
005200170314
005300170314     d Telefonico      c                   const('M')
005400170314     d Mail            c                   const('E')
005500170314
005600170314      //---------------------------------------------------------------
005700170314      //?Definizione schiere.
005800170314      //---------------------------------------------------------------
005900170315
006000170315      // - Sk Date ritiro
006100170315     d skData          s              8s 0 dim(999) inz
006200170314
006300170314      // - Messaggi di errore
006400170314     d Msg             s             78    dim(30) ctdata perrcd(1)
006500170314
006600170314      //---------------------------------------------------------------
006700170314      //?Definizione aree dati.
006800170314      //---------------------------------------------------------------
006900170314      // - Dati utente
007000170314     d §AzUte        e ds                  extname(AZUTE00F)
007100170314     d                                     dtaara
007200170314     d §DatiUte      e ds                  extname(dDatiUte)
007300170314     d                                     dtaara
007400170314
007500170314      //---------------------------------------------------------------
007600170314      //?Definizione strutture dati.
007700170314      //---------------------------------------------------------------
007800170314      // - Status
007900170314     d Psds           sds
008000170314     d   SDSpgm          *proc
008100170314
008200170314      // - InfDS
008300170314     d InfDspF         ds
008400170314     d  dsp_aid              369    369a
008500170314
008600170314      // - Indicatori su DspF
008700170314     d IndDspF         ds
008800170314        // - Indicatori di errore in videata
008900170314     d  ErrMessage                    1n   overlay(IndDspF : 28)
009000170314        // - Indicatori di errore
009100170314     d  PosCurDATA                    1n   overlay(IndDspF : 50)
009200170314     d  PosCurCAR1                    1n   overlay(IndDspF : 51)
009300170314     d  PosCurCAR2                    1n   overlay(IndDspF : 52)
009400170314     d  PosCurCAR3                    1n   overlay(IndDspF : 53)
009500170314     d  PosCurCAR4                    1n   overlay(IndDspF : 54)
009600170314     d  PosCurCAR5                    1n   overlay(IndDspF : 55)
009700170314        // - Indicatori di errore generico
009800170314     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009900170314
010000170314     d WindDspF        ds                  inz
010100170314     d  WindDspFa              1     49    inz(*zero)
010200170314     d  WindDspFb             50     99    inz(*zero)
010300170314
010400170314      // - Parametri per pgm. controllo/inversione data?
010500170314     d wlbdat          ds                  inz
010600170314     d  G08dat                 1      8  0
010700170314     d  G08inv                 9     16  0
010800170314     d  G08err                17     17
010900170314     d  G08tgi                18     22  0
011000170314
011100170314      // - Parametri ricevuti
011200170314     d KPJBA         e ds
011300170314
011400170314      // - Controllo cap/località/provincia
011500170314     d FNLV13DS      e ds
011600170314
011700170314      // - Reperimento dati utente
011800170314     d TIBS34DS      e ds
011900170314
012000170314      // - Instradamento
012100170314     d TISI95DS      e ds
012200170314
012300170314      // - Orari Servizi
012400170314     d TRULORSDS     e ds
012500170314     d TRULOR2DS     e ds
012600170314
012700170314      // Campo ORGDE0 - AZORG00F
012800170314     d OG140         e ds
012900170315
013000170315      // Campo ORMFLO - FNORM00F
013100170315     d dORM01        e ds
013200170314
013300170314      // ds per rcd 'DT' FNORE - Data Pronta Merce
013400170314     d dOREdt        e ds
013500170315
013600170315      // - Tabella GPD - Giorni Posticipo Data Ritiro
013700170315     d dGPD          e ds                  inz
013800170315     d  skGiorni               1     14s 0 dim(7)
013900170314
014000170314      // - Ds File FNORM00F
014100170314     d FNORM00F      e ds                  extname(FNORM00F)
014200170314
014300170314      //---------------------------------------------------------------
014400170314      //?Definizione variabili globali.
014500170314      //---------------------------------------------------------------
014600170314      // - Flags booleani
014700170314     d Fine            s               n   inz(*off)
014800170314     d wEoF            s               n   inz(*off)
014900170314     d wInzD02         s               n   inz(*off)
015000170314     d wStampa         s               n   inz(*off)
015100170314
015200170314      // - Indici di schiera
015300170314     d indicex         s              4s 0 inz
015400170314
015500170314      // - Campi associati al video
015600170314     d Video           s              2a   inz('D2')
015700170314
015800170314      // - Campi di comodo data
015900170314     d Data_EUR        s               d   datfmt(*eur)
016000170314     d Data_ISO        s               d   datfmt(*iso)
016100170314     d wData           s              8s 0 inz
016200170314
016300170314       // - Stringa SQL da eseguire
016400170314     d wSQL            s           4096    Varying        inz
016500170314
016600170314      // - Campi di comodo
016700170314     d Codice          s                   like(ORMcor) inz
016800170314     d Oggi            s              8s 0 inz
016900170517     d savpoe          s                   like(ORMpoe) inz
017000170314     d wDay_Of_Week    s              2  0 inz
017100170314     d wora1           s              6  0
017200170314     d wora2           s              6  0
017300170314     d wora3           s              6  0
017400170314     d w0030           s              3s 0
017500170314
017600170314      //---------------------------------------------------------------
017700170314      //?Definizione procedure esterne.
017800170314      //---------------------------------------------------------------
017900170314      // - Controllo cap/località/provincia
018000170314     d fnlv13r         pr                  extpgm('FNLV13R')
018100170314     d  kpjba                       502a
018200170314     d  fnlv13ds                           likeds(FNLV13DS)
018300170314     d  tisi95ds                           likeds(TISI95DS)
018400170315
018500170315      // - Ricerca orari di servizio
018600170315     d trulorsr        pr                  extpgm('TRULORSR')
018700170315     d  kpjba                              likeds(KPJBA)
018800170315     d  trulorsds                          likeds(TRULORSDS)
018900170315     d  trulor2ds                          likeds(TRULOR2DS)
019000170315     d                                     options (*nopass)
019100170314
019200170314      //---------------------------------------------------------------
019300170314      //?Definizione prototipi.
019400170314      //---------------------------------------------------------------
019500170314      /copy gaitrasrc/srcprotopr,TIBS34R
019600170314      /copy gaitrasrc/srcprotopr,XSRDA8
019700170314
019800170314      //---------------------------------------------------------------
019900170314      //?Definizione key-list.
020000170314      //---------------------------------------------------------------
020100170314
020200170314      //---------------------------------------------------------------
020300170314      //?Riepilogo indicatori.
020400170314      //---------------------------------------------------------------
020500170314      // 99 : Salto pagina
020600170314
020700170314      //---------------------------------------------------------------
020800170314      //?M A I N - L I N E
020900170314      //---------------------------------------------------------------
021000170314     c     *Entry        plist
021100170314     c                   parm                    kpjba
021200170314
021300170314      /free
021400170314
021500170314       //?Operazioni iniziali
021600170314       exsr RoutInz;
021700170314
021800170314       //?Gestione video
021900170314       DOW  Fine = *off;
022000170314         SELECT;
022100170314           WHEN  Video = 'D2';
022200170314             exsr GesD02;
022300170314           OTHER;
022400170314             Fine = *on;
022500170314         ENDSL;
022600170314       ENDDO;
022700170314
022800170314       //?Operazioni finali
022900170314       exsr RoutEnd;
023000170314
023100170314       //--------------------------------------------------------------
023200170314       //?Operazioni iniziali.
023300170314       //--------------------------------------------------------------
023400170314       BEGSR RoutInz;
023500170314
023600170314         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
023700170314
023800170314       //?Impostazione campi "fissi"
023900170314         V01pgm = SDSpgm;
024000170314         Video = 'D2';
024100170314         wInzD02 = *on;
024200170314
024300170314       //?Imposto oggi
024400170314         Oggi = %dec(%date());
024500170314
024600170314       //?Reperimento dati job
024700170314         exsr DatiJob;
024800170517
024900170517       //?Reperimento dati job
025000170517         TBEcod = 'GPD';
025100170517         TBEke1 = '999';
025200170517         chain (TBEcod:TBEke1) TNTBE01L;
025300170517         IF  %found(TNTBE01L);
025400170517           dGPD = TBEuni;
025500170517         ENDIF;
025600170314
025700170314       ENDSR;
025800170314
025900170314       //--------------------------------------------------------------
026000170314       //?Reperimento Dati del job (Utente/Operativi).
026100170314       //--------------------------------------------------------------
026200170314       BEGSR DatiJob;
026300170314
026400170314         in(E) §AzUte;
026500170314         IF  NOT %error;
026600170314           in(E) §DatiUte;
026700170314         ENDIF;
026800170314         IF  %error or RSut = *blanks;
026900170314           clear TIBS34ds;
027000170314           tibs34r(tibs34ds);
027100170314           in §AzUte;
027200170314           in §DatiUte;
027300170314         ENDIF;
027400170314
027500170314       ENDSR;
027600170314
027700170314       //--------------------------------------------------------------
027800170314       //?Gestione videata D02.
027900170314       //--------------------------------------------------------------
028000170314       BEGSR GesD02;
028100170314
028200170314       //?Inizializzazione videata
028300170314         IF  wInzD02;
028400170314           exsr InzD02;
028500170314           wInzD02 = *off;
028600170314         ENDIF;
028700170314
028800170314       //?Emissione Testata
028900170314         write  MB58T01;
029000170314
029100170314       //?Emissione videata
029200170314         exfmt  MB58D02;
029300170314         reset ErrMessage;
029400170314         reset ErrGenerico;
029500170314         clear V02msg;
029600170314
029700170314         SELECT;
029800170314
029900170314       //?- F03=Fine
030000170314           WHEN  dsp_aid = c_F03;
030100170314             exsr F03D02;
030200170314
030300170314       //?- F06=Conferma
030400170314           WHEN  dsp_aid = c_F06;
030500170314             exsr ContrD02;
030600170314             IF  ErrGenerico;
030700170314               leavesr;
030800170314             ENDIF;
030900170314             exsr F06D02;
031000170314
031100170314       //?Invio
031200170314           OTHER;
031300170314             exsr ContrD02;
031400170314             IF  ErrGenerico;
031500170314               leavesr;
031600170314             ENDIF;
031700170314
031800170314         ENDSL;
031900170314
032000170314       ENDSR;
032100170314
032200170314       //--------------------------------------------------------------
032300170314       //?Inizializzazione Videata D02.
032400170314       //--------------------------------------------------------------
032500170314       BEGSR InzD02;
032600170314
032700170314       //?Pulizia videata
032800170314         clear MB58D02;
032900170314
033000170314       ENDSR;
033100170314
033200170314       //--------------------------------------------------------------
033300170314       //?Gestione tasto funzionale F03 da videata D02
033400170314       //?F03=Fine
033500170314       //--------------------------------------------------------------
033600170314       BEGSR F03D02;
033700170314
033800170314       //?Chiusura del programma
033900170314         Fine = *on;
034000170314
034100170314       ENDSR;
034200170314
034300170314       //--------------------------------------------------------------
034400170314       //?Gestione tasto funzionale F06 da videata D02
034500170314       //?F06=Conferma
034600170314       //--------------------------------------------------------------
034700170314       BEGSR F06D02;
034800170314
034900170314       //?Cerco che giorno della settimana sto elaborando
035000170314         Data_ISO = %date(wdata);
035100170314         clear wDay_Of_Week;
035200170314         exec sql
035300170314         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
035400170314
035500170314         wEoF = *off;
035600170314
035700170314       //?Poi leggo gli ORM
035800170314         exsr PreparaSQL;
035900170314
036000170314       //?Dichiarazione cursore
036100170314         exec sql
036200170314         prepare S1 from :wSQL;
036300170314         exec sql
036400170314         DECLARE ORM CURSOR for S1;
036500170314
036600170314       //?Apertura del cursore
036700170314         exec sql
036800170314         OPEN ORM;
036900170314
037000170314         IF  sqlcode < 0;
037100170314           wEoF = *on;
037200170314           exec sql CLOSE ORM;
037300170314           leavesr;
037400170314         ENDIF;
037500170314
037600170314         DOW  not wEoF;
037700170314           exec sql
037800170314           FETCH NEXT from ORM into :FNORM00F;
037900170314           IF  sqlcod = 100 or sqlcod < 0;
038000170314             wEoF = *on;
038100170314             leave;
038200170314           ENDIF;
038300170314
038400170314           dORM01 = ORMflo;
038500170314
038600170314         //?Aggancio ORG
038700170314           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
038800170314
038900170314         //?Cerco la data pronta merce
039000170314           clear dOREDT;
039100170314           OREtrc = 'DT';
039200170314           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
039300170314           IF  %found(FNORE01L);
039400170314             dOREDT = OREdati;
039500170314           ENDIF;
039600170315
039700170315         //?Elaboro solo quelli con data pronta merce = data immissione
039800170315         //?perchè se data pronta merce > non si posticipa la data
039900170315           IF  §OREdpm > ORmdao;
040000170315             iter;
040100170315           ENDIF;
040200170314
040300170314         //?Cerco instradamento
040400170314           exsr call_FNLV13;
040500170314
040600170314         //?Recupero orari limite ritiro in giornata
040700170314           exsr call_TRULORS;
040800170314
040900170314         //?Memorizzo le varie sk da stampare
041000170314           exsr Elabora;
041100170314
041200170314         ENDDO;
041300170314
041400170314       //?Chiusura del cursore
041500170314         exec sql CLOSE ORM;
041600170315
041700170315       //?Stampa totali
041800170315         exsr Stampa;
041900170315
042000170315       //?Chiusura del programma
042100170315         Fine = *on;
042200170314
042300170314       ENDSR;
042400170314
042500170314       //--------------------------------------------------------------
042600170314       //?Controlla Videata D02.
042700170314       //--------------------------------------------------------------
042800170314       BEGSR ContrD02;
042900170314
043000170314         WindDspF = IndDspF;
043100170314         reset WindDspFb;
043200170314         IndDspF  = WindDspF;
043300170314
043400170314       //?Data Immissione
043500170314         IF  V02data = *zeros;
043600170314           ErrMessage  = *on;
043700170314           ErrGenerico = *on;
043800170314           PosCurDATa = *on;
043900170315           V02msg = Msg(01);
044000170314           leavesr;
044100170314         ENDIF;
044200170314         clear wlbdat;
044300170314         G08dat = V02data;
044400170314         xsrda8(wlbdat);
044500170314         IF  G08err = '1';
044600170314           ErrMessage  = *on;
044700170314           ErrGenerico = *on;
044800170314           PosCurDATA = *on;
044900170314           V02msg = Msg(01);
045000170314           leavesr;
045100170314         ENDIF;
045200170314         V02data = G08dat;
045300170314         wdata   = G08inv;
045400170314
045500170314       ENDSR;
045600170314
045700170314       //--------------------------------------------------------------
045800170314       //?Preparo la stringa SQL.
045900170314       //--------------------------------------------------------------
046000170314       BEGSR PreparaSQL;
046100170314
046200170314       //?Leggo ORM manuali
046300170314       //?          con peso <= 5 Kg.
046400170314       //?          non codificati ORD/MITT
046500170517       //?          immessi in data richiesta
046600170517         wSQL = 'SELECT * from FNORM00F join FNORG00F ' +
046700170517                'on ORMpoe = ORGpoe and ORMnsr = ORGnsr ' +
046800170517                'and ORMnor = ORGnor and ORMnrv = ORGnrv ' +
046900170517                'WHERE ORMtco ' +
047000170517                'in(''M'', ''E'') and ORGpkg <= 5 ' +
047100170517                'and ORMnsr = 0 ' +
047200170314                'and ORMcor = ' + %editc(Codice:'X') + ' ' +
047300170314                'and ORMcra = ' + %editc(Codice:'X') + ' ' +
047400170517                'and ORMdao = ' + %editc(wdata:'X');
047500170314
047600170517         wSQL += ' ORDER BY ORMpoe, ORMnsr, ORMnor, ORMnrv';
047700170314
047800170314       ENDSR;
047900170314
048000170314       //--------------------------------------------------------------
048100170314       //?Cerco l'instradamento.
048200170314       //--------------------------------------------------------------
048300170314       BEGSR call_FNLV13;
048400170314
048500170314         clear TISI95DS;
048600170314         clear FNLV13DS;
048700170314         I95tcn = '7';
048800170314         I95nar = ORMnar;
048900170314         I95cap = ORMcar;
049000170314         I95loc = ORMlor;
049100170314         I95prv = ORMprr;
049200170314         I95ind = ORMinr;
049300170314         I95dat = %dec(%date());
049400170314         I95lkg = ORGpkg;
049500170314         I95lmc = ORGvlm;
049600170314         I13af0 = 'S';
049700170314         I13cnv = 'S';
049800170314         I13la3 = 'S';
049900170314
050000170314       //?controllo cap/località/provincia
050100170314         fnlv13r (kpjba:fnlv13ds:tisi95ds);
050200170314
050300170314       ENDSR;
050400170314
050500170314       //--------------------------------------------------------------
050600170314       //?Cerco orari limiti.
050700170314       //--------------------------------------------------------------
050800170314       BEGSR call_TRULORS;
050900170314
051000170314         clear trulorsds;
051100170314         clear trulor2ds;
051200170314         IOREfil = ORMpor;
051300170314         IOREcap = ORMcar;
051400170314         IOREloc = ORMlor;
051500170315         IOREdta = ORMdao;
051600170314         IOREtser = 'R';
051700170314         IOREgf2 = O95gf2;
051800170314         trulorsr (kpjba:trulorsds:trulor2ds);
051900170314         IF  OOREerr <> *blanks or OOR2err <> *blanks;
052000170314           leavesr;
052100170314         ENDIF;
052200170314         wora1 = OOR2lrsc * 100;
052300170314         wora2 = OOR2lrnc * 100;
052400170314         wora3 = OOR2lrnk * 100;
052500170314
052600170314       ENDSR;
052700170314
052800170314       //--------------------------------------------------------------
052900170314       //?Elabora.
053000170314       //--------------------------------------------------------------
053100170314       BEGSR Elabora;
053200170517
053300170517       //?a rottura di filiale
053400170517         IF  ORMpoe <> savpoe;
053500170517         //?Stampa totali
053600170517           IF  savpoe > 0;
053700170517            exsr Stampa;
053800170517           ENDIF;
053900170517           savpoe = ORMpoe;
054000170517           clear MB5805P;
054100170517         ENDIF;
054200170315
054300170315       //?Carico la sk delle date di ritiro possibili
054400170315         Data_ISO = %date(ORMdao);
054500170315         clear skData;
054600170315         indicex = 1;
054700170315         FOR  indicex by 1 to skGiorni(wDay_Of_Week);
054800170315           Data_ISO = Data_ISO + %days(1);
054900170315           skData(indicex) = %dec(Data_ISO);
055000170315         ENDFOR;
055100170314
055200170517         tot += 1;
055300170314
055400170315       //?Data Immissione = Data Ritiro
055500170315         IF  ORMdao = ORMdar;
055600170517           DAO0 += 1;
055700170314         ENDIF;
055800170315
055900170315       //?Data Ritiro = Data Immissione + 1 gg
056000170315         IF  ORMdar = skData(01);
056100170517           DAO1 += 1;
056200170315         ENDIF;
056300170315
056400170315       //?Data Ritiro = Data Immissione + 2 gg
056500170315         IF  ORMdar = skData(02);
056600170517           DAO2 += 1;
056700170315         ENDIF;
056800170315       //?Data Ritiro = Data Immissione + oltre gg
056900170315         IF  ORMdar > skData(02);
057000170517           DAOO += 1;
057100170315         ENDIF;
057200170314
057300170314       ENDSR;
057400170314
057500170314       //--------------------------------------------------------------
057600170314       //?Stampa di controllo.
057700170314       //--------------------------------------------------------------
057800170314       BEGSR Stampa;
057900170314
058000170314         IF  not wStampa or *in99;
058100170315           write MB5801P;
058200170517           write MB5802P;
058300170517           write MB5803P;
058400170315           wStampa = *on;
058500170315           *in99 = *off;
058600170315         ENDIF;
058700170315
058800170315         data = V02data;
058900170315
059000170517         fil = savpoe;
059100170517         chain fil azorg01l ;
059200170517         IF  %found(azorg01L);
059300170517           des = ORGdes;
059400170517         ENDIF;
059500170517         IF  *in99;
059600170517           write MB5801P;
059700170517           write MB5802P;
059800170517           write MB5803P;
059900170517           *in99 = *off;
060000170517         ENDIF;
060100170517         write MB5805P;
060200170314
060300170314       ENDSR;
060400170314
060500170314       //--------------------------------------------------------------
060600170314       //?Operazioni finali.
060700170314       //--------------------------------------------------------------
060800170314       BEGSR RoutEnd;
060900170314
061000170314         *inLR = *on;
061100170314         return;
061200170314
061300170314       ENDSR;
061400170314
061500170314      /end-free
061600170314** -- MSG -------------------------------------------------------------------*
061700170314Data errata                                                                    1
061800170314Area errata                                                                    2
