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
001100091012     ffnlrw0d   cf   e             workstn
001200090708     f                                     indds(IndDspF)
001300090708     f                                     infds(InfDspF)
001400090708
001500090708      //---------------------------------------------------------------
001600090511      //?Definizione costanti.
001700090511      //---------------------------------------------------------------
001800090708
001900090708      // - Tasti funzionali a video
002000090708     d c_F01           c                   const(x'31')
002100090708     d c_F02           c                   const(x'32')
002200090708     d c_F03           c                   const(x'33')
002300090723     d c_F04           c                   const(x'34')
002400090708     d c_F05           c                   const(x'35')
002500090708     d c_F06           c                   const(x'36')
002600090708     d c_F07           c                   const(x'37')
002700090708     d c_F08           c                   const(x'38')
002800090708     d c_F09           c                   const(x'39')
002900090708     d c_F10           c                   const(x'3A')
003000090810     d c_F11           c                   const(x'3B')
003100090708     d c_F12           c                   const(x'3C')
003200090708     d c_F13           c                   const(x'B1')
003300090708     d c_F14           c                   const(x'B2')
003400090708     d c_F15           c                   const(x'B3')
003500090708     d c_F16           c                   const(x'B4')
003600090708     d c_F17           c                   const(x'B5')
003700090708     d c_F18           c                   const(x'B6')
003800090708     d c_F19           c                   const(x'B7')
003900090708     d c_F20           c                   const(x'B8')
004000090708     d c_F21           c                   const(x'B9')
004100090708     d c_F22           c                   const(x'BA')
004200090708     d c_F23           c                   const(x'BB')
004300090708     d c_F24           c                   const(x'BC')
004400090708     d c_Enter         c                   const(x'F1')
004500090708     d c_RollDown      c                   const(x'F4')
004600090708     d c_RollUp        c                   const(x'F5')
004700090723
004800090723     d Digits          c                   const('0123456789')
004900090511
005000090511      //---------------------------------------------------------------
005100090511      //?Definizione schiere.
005200090511      //---------------------------------------------------------------
005300090708
005400090708      // - Messaggi di errore
005500090708     d $Msg            s             78    dim(10) ctdata perrcd(1)
005600090709
005700090709      // - Data completa
005800090709     d $Date           s              8  0 dim(07)
005900090708
006000090511      //---------------------------------------------------------------
006100090511      //?Definizione aree dati.
006200090511      //---------------------------------------------------------------
006300090708
006400090708      // - Dati utente
006500090708     d §AzUte        e ds                  extname(AZUTE00F)
006600090708     d                                     dtaara
006700090708     d §DatiUte      e ds                  extname(dDatiUte)
006800090708     d                                     dtaara
006900090511
007000090511      //---------------------------------------------------------------
007100090511      //?Definizione strutture dati.
007200090511      //---------------------------------------------------------------
007300090708
007400090708      // - Status
007500090708     d Psds           sds
007600090708     d   SDSpgm          *proc
007700090708
007800090708      // - InfDS
007900090708     d InfDspF         ds
008000090708     d  dsp_aid              369    369a                                        AID byte
008100090724     d  dsp_rig              370    370
008200090724     d  dsp_col              371    371
008300090708
008400090708      // - Indicatori su DspF
008500090708     d IndDspF         ds
008600090708        // - Indicatori di errore
008700090708     d  ErrMessage                    1n   overlay(IndDspF : 28)
008800091013     d  PosCurDATA                    1n   overlay(IndDspF : 50)
008900090708     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009000090724
009100090708      // - Controllo data
009200090708     d wlbdat          ds                  inz
009300090708     d  g02dat                 1      8  0
009400090708     d  g02inv                 9     16  0
009500090708     d  g02err                17     17
009600090708     d  g02tgi                18     22  0
009700090708
009800090708      // - Parametri ricevuti
009900090708     d KPJBA         e ds
010000091012
010100091012      // - Parametri per Reperimento legami
010200091012     d tibs10ds      e ds                  inz
010300091012     d  skfigli               21   5520  0 dim(500)
010400090708
010500090708      // - Reperimento dati utente
010600090708     d TIBS34ds      e ds
010700090805
010800091012      // File bolle
010900091012     d fnarb00f      e ds                  extname(FNARB00F)
011000091013     d fiar400f      e ds                  extname(FIAR400F)
011100090708
011200090708      // Campi di comodo per gestione indicatori a video
011300090708     d WindDspF        ds                  inz
011400090708     d   WindDspFa             1     49    inz(*zero)
011500090708     d   WindDspFb            50     99    inz(*zero)
011600090511
011700090511      //---------------------------------------------------------------
011800090511      //?Definizione variabili globali.
011900090511      //---------------------------------------------------------------
012000090708
012100090708      // - Flags booleani
012200090807     d $Fine           s               n   inz(*off)
012300090807     d $InzD01         s               n   inz(*on)
012400090708
012500090708      // - Campi associati al video
012600090708     d $Video          s              2    inz('D1')
012700090804
012800090804       // - Stringa SQL da eseguire
012900090804     d wSQL            s           2048    Varying        inz
013000090708
013100090708      // - Campi di comodo
013200090721     d wdata           s              8  0
013300090721     d wDate_EUR       s               d   datfmt(*eur)
013400090721     d wDate_ISO       s               d   datfmt(*iso)
013500090804     d xx              s              3  0 inz
013600090511
013700090511      //---------------------------------------------------------------
013800090511      //?Definizione procedure esterne.
013900090511      //---------------------------------------------------------------
014000091013
014100091013      // - Sottomissione lavoro batch
014200091013     d bch10           pr                  extpgm('BCH10')
014300091013     d  kpjba                              likeds(KPJBA)
014400090708
014500090708      // - Reperimento dati utente
014600090708     d tibs34r         pr                  extpgm('TIBS34R')
014700090708     d  tibs34ds                           likeds(TIBS34ds)
014800091012
014900091012      // - Reperimento legami
015000091012      /copy gaitrasrc/srcprotopr,tibs10r
015100091012
015200090708      /copy gaitrasrc/srcprotopr,xsrda8
015300090511
015400090511      //---------------------------------------------------------------
015500090511      //?Definizione key-list.
015600090511      //---------------------------------------------------------------
015700090708
015800090708      //---------------------------------------------------------------
015900090708      //?Riepilogo indicatori.
016000090708      //---------------------------------------------------------------
016100090708
016200090708      // 28    : Emissione messaggio di errore a video
016300090721      // 50    : Posizionamento Cursore Data
016400091012      // 51    : Posizionamento Cursore Filiale
016500090708      // 99    : Generico di Errore
016600090511
016700090511      //---------------------------------------------------------------
016800090708      //?M A I N - L I N E
016900090511      //---------------------------------------------------------------
017000090708
017100090708     c     *Entry        plist
017200090708     c                   parm                    KPJBA
017300090708
017400090708      /free
017500090708
017600090708       //?Operazioni iniziali
017700090708       exsr sr_RoutInz;
017800090708
017900090708       //?Gestione video
018000090722       DOW  $Fine = *off;
018100090722
018200090722         SELECT;
018300090722           WHEN  $Video = 'D1';
018400090708             exsr sr_GesD01;
018500090722           OTHER;
018600090708             $Fine = *on;
018700090722         ENDSL;
018800090722
018900090708       ENDDO;
019000090708
019100090708       //?Operazioni finali
019200090708       exsr sr_RoutEnd;
019300090708
019400090708       //--------------------------------------------------------------
019500090708       //?Operazioni iniziali.
019600090708       //--------------------------------------------------------------
019700090708       BEGSR sr_RoutInz;
019800090708
019900090708         //?Impostazione campi "fissi"
020000090708         T01pgm = SDSpgm;
020100090708
020200090708         //?Reperimento dati job
020300090708         exsr sr_DatiJob;
020400090708
020500090708         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
020600090805
020700091012         //?Carico sk figli unificante Wurth
020800091012         exsr sr_CarFigli;
020900090708
021000090708       ENDSR;
021100090708
021200090708       //--------------------------------------------------------------
021300090708       //?Reperimento Dati del job (Utente/Operativi).
021400090708       //--------------------------------------------------------------
021500090708       BEGSR sr_DatiJob;
021600090708
021700090708         in(E) §AzUte;
021800090722         IF  NOT %error;
021900090708           in(E) §DatiUte;
022000090722         ENDIF;
022100090722         IF  %error or RSut = *blanks;
022200090708           clear TIBS34ds;
022300090708           tibs34r(tibs34ds);
022400090708           in §AzUte;
022500090708           in §DatiUte;
022600090722         ENDIF;
022700090708
022800090708       ENDSR;
022900090805
023000090805       //--------------------------------------------------------------
023100091012       //?Carico Figli Unificante Wurth.
023200090805       //--------------------------------------------------------------
023300091012       BEGSR sr_CarFigli;
023400090805
023500091013       // ricerco i figli del codice unificante WURTH
023600091012            clear tibs10ds;
023700091012            d10drf = *date;
023800091012            d10tle = 'WW';
023900091012            d10paf = 'F';
024000091012            d10cod = 0430839;
024100091012            tibs10r(tibs10ds);
024200090805
024300090805       ENDSR;
024400090708
024500090708       //--------------------------------------------------------------
024600090805       //?Gestione videata D01.
024700090708       //--------------------------------------------------------------
024800090708       BEGSR sr_GesD01;
024900090708
025000090708         //?Inizializzazione videata
025100090722         IF  $InzD01 = *on;
025200090708           exsr sr_InzD01;
025300090708           $InzD01 = *off;
025400090722         ENDIF;
025500090708
025600090708         //?Emissione videata
025700091012         write LRW0T01;
025800091012         exfmt LRW0D01;
025900090708         ErrMessage  = *off;
026000090708         ErrGenerico = *off;
026100090708         clear V1Dmsg;
026200090708
026300090708         SELECT;
026400090708
026500090708           //?F3=Fine
026600090722           WHEN  dsp_aid = c_F03;
026700090708             exsr sr_F03D01;
026800091013
026900091013           //?F6=Conferma
027000091013           WHEN  dsp_aid = c_F06;
027100091013             //?- Controllo dati
027200091013             exsr sr_CtrD01;
027300091013             IF  errGenerico;
027400091013               leavesr;
027500091013             ENDIF;
027600091013             exsr sr_F06D01;
027700091013             leavesr;
027800090708
027900090708           //?Enter
028000090708           OTHER;
028100090708             //?- Controllo dati
028200090708             exsr sr_CtrD01;
028300090722             IF  errGenerico;
028400090708               leavesr;
028500090722             ENDIF;
028600090708
028700090708         ENDSL;
028800090708
028900090708       ENDSR;
029000090708
029100090708       //--------------------------------------------------------------
029200090805       //?Inizializzazione videata D01.
029300090708       //--------------------------------------------------------------
029400090708       BEGSR sr_InzD01;
029500090708
029600091012         clear LRW0D01;
029700090708
029800090708         //?imposto la data del giorno
029900091013         wdata = %dec(%date());
030000091013         wDate_ISO = %date(wdata:*iso);
030100091013         wDate_EUR = wDate_ISO;
030200091013         V1Data = %dec(wDate_EUR);
030300091012
030400091012         //?imposto la filiale arrivo
030500091013         V1clna = 277;
030600091012         //?Decodifico la filiale di arrivo
030700091013         exsr sr_DecLna;
030800090708
030900090708       ENDSR;
031000090708
031100090708       //--------------------------------------------------------------
031200090805       //?Gestione tasto funzionale F3 da videata D01.
031300090708       //?F3=Fine
031400090708       //--------------------------------------------------------------
031500090708       BEGSR sr_F03D01;
031600090708
031700090708         //?Chiusura del programma
031800090708         $Fine = *on;
031900090708
032000090708       ENDSR;
032100090708
032200090708       //--------------------------------------------------------------
032300090805       //?Controllo videata D01.
032400090708       //--------------------------------------------------------------
032500090708       BEGSR sr_CtrD01;
032600090708
032700090708         WindDspF = IndDspF;
032800090708         reset WindDspFb;
032900090708         IndDspF  = WindDspF;
033000091013
033100091013         //?Controllo linea di arrivo
033200091013         IF  V1cLNA   =  *zeros;
033300091013           errMessage  = *on;
033400091013           errGenerico = *on;
033500091013           PosCurLNA   = *on;
033600091013           V1Dmsg      = $Msg(01);
033700091013           leavesr;
033800091013         ENDIF;
033900091013
034000091013         //?Decodifico e controllo la filiale di arrivo
034100091013         exsr sr_DecLna;
034200091013         IF  errMessage;
034300091013           errGenerico = *on;
034400091013           PosCurLNA   = *on;
034500091013           V1Dmsg      = $Msg(02);
034600091013           leavesr;
034700091013         ENDIF;
034800090708
034900090708         //?Controllo data
035000090722         IF  V1Data    = *zeros;
035100090722           ErrMessage   = *on;
035200090722           ErrGenerico  = *on;
035300090722           PosCurDATA   = *on;
035400091013           V1Dmsg       = $Msg(03);
035500090722           leavesr;
035600090722         ENDIF;
035700090708
035800090722         clear wlbdat;
035900090722         g02dat = V1Data;
036000090722         xsrda8(wlbdat);
036100090722         IF  g02err    = '1';
036200090722           ErrMessage   = *on;
036300090722           ErrGenerico  = *on;
036400090722           PosCurDATA   = *on;
036500091013           V1Dmsg       = $Msg(04);
036600090722           leavesr;
036700090722         ENDIF;
036800090708
036900090722         V1Data = g02dat;
037000090722         wdata  = g02inv;
037100090708
037200090708       ENDSR;
037300090708
037400090708       //--------------------------------------------------------------
037500091013       //?Decodifico e controllo LNA.
037600090708       //--------------------------------------------------------------
037700091013       BEGSR sr_DecLna;
037800090708
037900091013         clear V1dLNA;
038000091013         chain V1cLNA AZORG01L;
038100091013         IF  not %found( AZORG01L ) or ORGfva <> *blanks;
038200091013           errMessage  = *on;
038300091013           leavesr;
038400091013         ENDIF;
038500091013
038600091013         V1dLNA = ORGdes;
038700090708
038800090708       ENDSR;
038900091013
039000091013       //--------------------------------------------------------------
039100091013       //?Gestione tasto funzionale F6 da videata D01.
039200091013       //?F6=Conferma
039300091013       //--------------------------------------------------------------
039400091013       BEGSR sr_F06D01;
039500091013
039600091013         //?Prima stampo le spedizioni non abbinate ai ritiri
039700091013         exsr  sr_PrepSQL;
039800090708
039900090805
040000090805         //?Dichiarazione cursore
040100090805         exec sql
040200090805           prepare S1   from :wSQL;
040300090805         exec sql
040400090805           declare ATT  cursor for S1;
040500090805
040600090805         //?Apertura del cursore
040700090805         exec sql
040800090805           open ATT;
040900090722
041000090805         IF sqlcode < 0;
041100090805           $End = *on;
041200090805         ENDIF;
041300090722
041400090805         DOW not $End;
041500090722           exec sql
041600090805             fetch next from ATT into :atctat, :atccpo, :atcksc,
041700090805                                      :atcdad, :atcoii, :atcofi, :atchda;
041800090805           IF sqlcod = 100 or sqlcod < 0;
041900090805             $End = *on;
042000090805             leave;
042100090805           ENDIF;
042200090805
042300090805           IF atcdad <> sav_atcdad;
042400090805             clear wDetA;
042500090805             clear wDetAt;
042600090805             clear wDetB;
042700090805             clear wDetBt;
042800090805             sav_atcdad = atcdad;
042900090805           ENDIF;
043000090805
043100090805         //?Carico i dati a dettaglio
043200090805           exsr sr_CarDati;
043300090805
043400090722         ENDDO;
043500090722
043600090805         exec sql
043700090805           close ATT;
043800090722
043900090722       ENDSR;
044000090805
044100090805       //--------------------------------------------------------------
044200090805       //?Preparazione stringa SQL.
044300090805       //--------------------------------------------------------------
044400090805       BEGSR sr_PrepSQL;
044500090805
044600090805         wSQL = 'select atctat, atccpo, atcksc, atcdad, atcoii, atcofi, +
044700090805                 atchda  from TIATC00F +
044800090805                  where atccmm in(';
044900090805
045000090806         yy = 0;
045100090805         xx = 1;
045200090805         FOR xx by 1 to %elem($Age);
045300090805           IF $Age(xx) <> *zeros;
045400090806             IF yy > 0;
045500090806               wSQL += ', ';
045600090806             ELSE;
045700090806               yy = 1;
045800090806             ENDIF;
045900090806             wSQL += %editc($Age(xx):'X');
046000090805           ENDIF;
046100090805         ENDFOR;
046200090805
046300090805         wSQL += ') and atcdad between ' + %editc($Date(01):'X') +
046400090805                 ' and ' + %editc($Date(07):'X');
046500090805
046600090805         IF MK82NoAtt <> *blanks;
046700090806         wSQL += ' and atctat not in(''';
046800090806           yy = 0;
046900090805           xx = 1;
047000090805           FOR xx by 1 to %elem($NoAtt);
047100090806             IF $NoAtt(xx) <> *blanks;
047200090806               IF yy > 0;
047300090806                 wSQL += ', ';
047400090806               ELSE;
047500090806                 yy = 1;
047600090806               ENDIF;
047700090806               wSQL +=  $NoAtt(xx) + '''';
047800090805             ENDIF;
047900090805           ENDFOR;
048000090805         wSQL += ')';
048100090805         ENDIF;
048200090805
048300090806         wSQL += ' order by atcdad, atctat +
048400090805                   for fetch only';
048500090805
048600090805       ENDSR;
048700090708
048800090708       //--------------------------------------------------------------
048900090708       //?Operazioni finali.
049000090708       //--------------------------------------------------------------
049100090708       BEGSR sr_RoutEnd;
049200090708
049300090708         *inLR = *on;
049400090708         return;
049500090708
049600090708       ENDSR;
049700090708
049800090708      /end-free
049900090708
050000090708       //--------------------------------------------------------------
050100090708       //?Schiere a tempo di compilazione.
050200090708       //--------------------------------------------------------------
050300090708
050400090708** - $MSG -------------------------------------------------------------------*
050500091013Immettere la linea di arrivo                                                   01
050600091013Linea di arrivo errata                                                         02
050700091013Immettere la data                                                              03
050800091013Data errata                                                                    04
