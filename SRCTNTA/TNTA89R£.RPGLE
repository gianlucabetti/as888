000100080206      //--------------------------------------------------------------
000200100524      //?TNTA89R - Statistica Trattative
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000100326
001100100326      // - Tabelle
001200090720     fTABEL00F  if   e           k disk
001300100531     fTNTBE01L  if   e           k disk
001400090720
001500090716      // - Video Gestione trattative commerciali
001600100524     fTNTA89D   cf   e             workstn
001700080208     f                                     indds(IndDspF)
001800080206     f                                     infds(InfDspF)
001900080206
002000080206      //---------------------------------------------------------------
002100090406      //?Definizione costanti.
002200080206      //---------------------------------------------------------------
002300080207
002400080207      // - Tasti funzionali a video
002500080207     d c_F01           c                   const(x'31')
002600080207     d c_F02           c                   const(x'32')
002700080207     d c_F03           c                   const(x'33')
002800090406     d c_F04           c                   const(x'34')
002900080207     d c_F05           c                   const(x'35')
003000080207     d c_F06           c                   const(x'36')
003100080207     d c_F07           c                   const(x'37')
003200080207     d c_F08           c                   const(x'38')
003300080207     d c_F09           c                   const(x'39')
003400080207     d c_F10           c                   const(x'3A')
003500090303     d c_F11           c                   const(x'3B')
003600080207     d c_F12           c                   const(x'3C')
003700080207     d c_F13           c                   const(x'B1')
003800080207     d c_F14           c                   const(x'B2')
003900080207     d c_F15           c                   const(x'B3')
004000080207     d c_F16           c                   const(x'B4')
004100080207     d c_F17           c                   const(x'B5')
004200080207     d c_F18           c                   const(x'B6')
004300080207     d c_F19           c                   const(x'B7')
004400080207     d c_F20           c                   const(x'B8')
004500080207     d c_F21           c                   const(x'B9')
004600080207     d c_F22           c                   const(x'BA')
004700080207     d c_F23           c                   const(x'BB')
004800080207     d c_F24           c                   const(x'BC')
004900080207     d c_Enter         c                   const(x'F1')
005000080207     d c_RollDown      c                   const(x'F4')
005100080207     d c_RollUp        c                   const(x'F5')
005200100211
005300100506     d Digits          c                   const('0123456789')
005400100531
005500080206
005600080206      //---------------------------------------------------------------
005700080206      //?Definizione schiere.
005800080206      //---------------------------------------------------------------
005900100708
006000100708      // - Importatnza clienti da tabella IC
006100100708     d $Ic             s              1    dim(10)
006200100706
006300100706      // - Concorrenti possibili da tabella ITS
006400100708     d $Its            s              4    dim(30)
006500080206
006600100531      // - Tipi trattativa possibili da tabella TTR
006700100531     d $Ttr            s              1    dim(10)
006800100531
006900080206      // - Messaggi di errore
007000110921     d $Msg            s             78    dim(31) ctdata perrcd(1)
007100080206
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione aree dati.
007400080206      //---------------------------------------------------------------
007500080206
007600080206      // - Dati utente
007700080206     d §AzUte        e ds                  extname(AZUTE00F)
007800080206     d                                     dtaara
007900080206     d §DatiUte      e ds                  extname(dDatiUte)
008000080206     d                                     dtaara
008100080206
008200080206      //---------------------------------------------------------------
008300080206      //?Definizione strutture dati.
008400080206      //---------------------------------------------------------------
008500080206
008600080206      // - Status
008700080206     d Psds           sds
008800080206     d   SDSpgm          *proc
008900080206
009000080206      // - InfDS
009100080206     d InfDspF         ds
009200080207     d  dsp_aid              369    369a
009300080206
009400080206      // - Indicatori su DspF
009500080208     d IndDspF         ds
009600100326        // - Indicatori di errore
009700100326     d  ErrMessage                    1n   overlay(IndDspF : 28)
009800110119     d  DspStpCar                     1n   overlay(IndDspF : 40)
009900100326     d  PosCurCmm                     1n   overlay(IndDspF : 50)
010000100706     d  PosCurDtavi                   1n   overlay(IndDspF : 51)
010100100706     d  PosCurDtavf                   1n   overlay(IndDspF : 52)
010200100706     d  PosCurDtchi                   1n   overlay(IndDspF : 53)
010300100706     d  PosCurDtchf                   1n   overlay(IndDspF : 54)
010400100706     d  PosCurDtica                   1n   overlay(IndDspF : 55)
010500100706     d  PosCurConc1                   1n   overlay(IndDspF : 56)
010600100706     d  PosCurConc2                   1n   overlay(IndDspF : 57)
010700100706     d  PosCurConc3                   1n   overlay(IndDspF : 58)
010800100706     d  PosCurConc4                   1n   overlay(IndDspF : 59)
010900100706     d  PosCurConc5                   1n   overlay(IndDspF : 60)
011000100708     d  PosCurDeli                    1n   overlay(IndDspF : 61)
011100100708     d  PosCurDelf                    1n   overlay(IndDspF : 62)
011200100708     d  PosCurCalde                   1n   overlay(IndDspF : 63)
011300100708     d  PosCurDtaff                   1n   overlay(IndDspF : 64)
011400100708     d  PosCurTtr1                    1n   overlay(IndDspF : 65)
011500100708     d  PosCurTtr2                    1n   overlay(IndDspF : 66)
011600100708     d  PosCurTtr3                    1n   overlay(IndDspF : 67)
011700100708     d  PosCurTtr4                    1n   overlay(IndDspF : 68)
011800100708     d  PosCurTic1                    1n   overlay(IndDspF : 69)
011900100708     d  PosCurTic2                    1n   overlay(IndDspF : 70)
012000100708     d  PosCurTic3                    1n   overlay(IndDspF : 71)
012100100708     d  PosCurTic4                    1n   overlay(IndDspF : 72)
012200100708     d  PosCurTic5                    1n   overlay(IndDspF : 73)
012300100708     d  PosCurEsi                     1n   overlay(IndDspF : 74)
012400100708     d  PosCurDet                     1n   overlay(IndDspF : 75)
012500100708     d  PosCurGent                    1n   overlay(IndDspF : 76)
012600100810     d  PosCurTtrio                   1n   overlay(IndDspF : 77)
012700100810     d  PosCurOrd                     1n   overlay(IndDspF : 78)
012800100810     d  PosCurDetc                    1n   overlay(IndDspF : 79)
012900101228     d  PosCurTtr5                    1n   overlay(IndDspF : 80)
013000101228     d  PosCurTtr6                    1n   overlay(IndDspF : 81)
013100101228     d  PosCurAuSci                   1n   overlay(IndDspF : 82)
013200101228     d  PosCurAuScf                   1n   overlay(IndDspF : 83)
013300101228     d  PosCurVltti                   1n   overlay(IndDspF : 84)
013400101228     d  PosCurVlttf                   1n   overlay(IndDspF : 85)
013500110119     d  PosCurCar                     1n   overlay(IndDspF : 86)
013600100326        // - Indicatori di errore generico
013700100326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
013800100708
013900100708        // - Tipi trattativa da I/O
014000100708     d V02ttr          ds
014100100708     d  V02ttr1                       1
014200100708     d  V02ttr2                       1
014300100708     d  V02ttr3                       1
014400100708     d  V02ttr4                       1
014500090407
014600090407     d WindDspF        ds                  inz
014700090407     d   WindDspFa             1     49    inz(*zero)
014800090407     d   WindDspFb            50     99    inz(*zero)
014900090319
015000090319     d wlbdat          ds                  inz
015100090319     d  g02dat                 1      8  0
015200090319     d  g02inv                 9     16  0
015300090319     d  g02err                17     17
015400090319     d  g02tgi                18     22  0
015500080206
015600080206      // - Parametri ricevuti
015700080206     d KPJBA         e ds
015800080206
015900080206      // - Ricerca/Controllo tabelle
016000080206     d TIBS02ds      e ds                  inz
016100100326
016200100326      // - Reperimento dati utente
016300100326     d TIBS34ds      e ds
016400100326
016500100326      // - Controllo abilitazioni
016600100211     d TNTAA1DS      e ds                  inz
016700100329
016800100329      // - Passaggio dati a pgm stampa
016900100524     d TNTA98DS      e ds
017000100326
017100110119      // - Carico Filiali abilitate all'utente
017200100326     d TRUL31DS      e ds
017300100326     d POG                    10    759    DIM(250)
017400110119      // - Carico Aree abilitate all'utente
017500110119     d TRUL31DS2     e ds
017600110119     d CAR                    22    171    DIM(50)
017700100708
017800100708      // - Tabella IC = Importanza clienti
017900100708     d dsIC          e ds                  inz
018000090402
018100090318      // - Tabella 01 = Codici Commerciali
018200090318     d ds01          e ds                  inz
018300110119
018400110119      // - Tabella 05 = Codici Aree
018500110119     d ds05          e ds                  inz
018600090717
018700080206      //---------------------------------------------------------------
018800080206      //?Definizione variabili globali.
018900080206      //---------------------------------------------------------------
019000080206
019100080206      // - Flags booleani
019200100312     d $End            s               n   inz(*off)
019300100326     d $ErrGrave       s               n   inz(*off)
019400100326     d $Fine           s               n   inz(*off)
019500080208     d $InzD01         s               n   inz(*on)
019600100706     d $InzD02         s               n   inz(*on)
019700080206
019800080207      // - Campi associati al video
019900080207     d $Video          s              2    inz('D1')
020000100531
020100100531      // - Indici di schiera
020200100531     d xx              s              4  0 inz
020300080206
020400090401      // - Campi di comodo data
020500090401     d  data_eur       s               d   Datfmt(*eur)
020600090401     d  data_iso       s               d   Datfmt(*iso)
020700100526     d  data_iso1      s               d   Datfmt(*iso)
020800121029     d wOggi           s              8  0 inz
020900121029     d wOggi_2         s              8  0 inz
021000090401
021100080206      // - Campi di comodo
021200100708     d C_tab           s                   like(TBEcod)
021300100708
021400101228     d wausci          s                   like(V01ausci) inz
021500101228     d wauscf          s                   like(V01auscf) inz
021600100708     d wdeli           s              3  0 inz
021700100708     d wdelf           s              3  0 inz
021800100706     d wdtaff          s              8  0 inz
021900100706     d wdtavf          s              8  0 inz
022000100706     d wdtavi          s              8  0 inz
022100100706     d wdtchf          s              8  0 inz
022200100706     d wdtchi          s              8  0 inz
022300100706     d wdtica          s              8  0 inz
022400100714     d wgiorni         s              5  0 inz
022500101228     d wvaltti         s                   like(V01valtti) inz
022600101228     d wvalttf         s                   like(V01valttf) inz
022700100708
022800100326     d tb85Kfil        s              3
022900100326     d tb85Key         s              8
023000100326     d tb85Des1        s             25
023100100326     d tb85Uni         s              1    inz('S')
023200100708
023300100708     d idTabella       s              2
023400100708     d Ordinamento     s              1
023500100708     d idElemento      s              8
023600100708     d TastoUscita     s              1
023700100708
023800080208
023900090508      //---------------------------------------------------------------
024000090508      //?Definizione procedure usate.
024100090508      //---------------------------------------------------------------
024200100329
024300100329      // - Sottomissione lavoro batch
024400100329     d bch10           pr                  extpgm('BCH10')
024500100329     d  kpjba                              likeds(KPJBA)
024600100329
024700100329      // - Richiamo diretto lavoro batch
024800100524     d tnta98c         pr                  extpgm('TNTA98C')
024900100329     d  kpjba                              likeds(KPJBA)
025000110119
025100110119      // - Filiali e aree abilitate
025200110119     d trul31r         pr                  extpgm('TRUL31R')
025300110119     d  kpjba                              likeds(KPJBA)
025400110119     d  trul31ds                           likeds(trul31ds)
025500110119     d  trul31ds2                          likeds(trul31ds2)
025600080206
025700080626      //---------------------------------------------------------------
025800080626      //?prototipi
025900080626      //---------------------------------------------------------------
026000100326
026100080626      /copy gaitrasrc/srcprotopr,tibs02r
026200080626      /copy gaitrasrc/srcprotopr,tibs34r
026300100708      /copy gaitrasrc/srcprotopr,trul19r
026400100211      /copy gaitrasrc/srcprotopr,tntaa1c
026500090406      /copy gaitrasrc/srcprotopr,xsrda8
026600090318      /copy gaitrasrc/srcprotopr,trtb85r
026700100326
026800080206      //---------------------------------------------------------------
026900080206      //?Definizione key-list.
027000080206      //---------------------------------------------------------------
027100080206
027200100326       // - File TABEL00F
027300100326     d k03tabel      e ds                  extname(TABEL00F:*key)
027400100326     d                                     prefix(k_)
027500080206
027600080206      //---------------------------------------------------------------
027700080206      //?Riepilogo indicatori.
027800080206      //---------------------------------------------------------------
027900080207      // 28    : Emissione messaggio di errore a video
028000100708      // 50-76 : Errori Video
028100080207      // 99    : Generico di Errore
028200080206      //---------------------------------------------------------------
028300080206
028400080206      //---------------------------------------------------------------
028500080206      //?M A I N - L I N E
028600080206      //---------------------------------------------------------------
028700080206
028800080206     c     *Entry        plist
028900080206     c                   parm                    KPJBA
029000080206
029100080206      /free
029200080206
029300100326       //?Operazioni iniziali
029400080206       exsr RoutInz;
029500090601
029600100326       //?Gestione video
029700080206       DOW $Fine = *off;
029800100329         SELECT;
029900100329           WHEN $Video = 'D1';
030000080206             exsr GesD01;
030100100706           WHEN $Video = 'D2';
030200100706             exsr GesD02;
030300100329           OTHER;
030400080206             $Fine = *on;
030500100329         ENDSL;
030600080206       ENDDO;
030700080206
030800100326       //?Operazioni finali
030900080206       exsr RoutEnd;
031000080206
031100080206       //--------------------------------------------------------------
031200080206       //?Operazioni iniziali.
031300080206       //--------------------------------------------------------------
031400080206       BEGSR RoutInz;
031500080206
031600100326       //?Impostazione campi "fissi"
031700100326         k_TBLkut = 1;
031800080206
031900100326       //?Reperimento dati job
032000080206         exsr DatiJob;
032100121029
032200121029       //?Calcolo oggi - 2 anni
032300121029         wOggi = %dec( %date() );
032400121029         Data_ISO = %date(wOggi);
032500121029         Data_ISO = Data_ISO - %years(2);
032600121029         wOggi_2 = %dec(Data_ISO);
032700100211
032800100326       //?Controllo se utente autorizzato alla funzione
032900100211         reset TNTAA1DS;
033000100211         ITAA1caut = '§utegtc';
033100100211         kpjbu     = TNTAA1DS;
033200100211         tntaa1c (kpjba);
033300100211         TNTAA1DS = kpjbu;
033400100211
033500100211         IF  OTAA1fabi = 'N';
033600080206           $ErrGrave   = *on;
033700080206           errMessage  = *on;
033800080206           errGenerico = *on;
033900100329           V01msg = $Msg(01);
034000080206           leavesr;
034100080206         endif;
034200100312
034300100312         clear TRUL31DS;
034400110119         clear TRUL31DS2;
034500100312         I31abi = OTAA1cabi;
034600100312         I31cdi = DUTdis;
034700100312         I31car = DUTare;
034800100312         I31cpo = DUTpou;
034900110119         trul31r (kpjba : trul31ds : trul31ds2);
035000100312         IF  O31pog <= *zeros;
035100100312           $ErrGrave   = *on;
035200100326           ErrMessage  = *on;
035300100326           ErrGenerico = *on;
035400100326           V01msg = $Msg(01);
035500100312           leavesr;
035600100312         ENDIF;
035700110119
035800110119         //?Abilito richiesta per Area
035900110119         DspStpCar = *off;
036000110119         IF  car(2) > *zeros;
036100110119           DspStpCar = *on;
036200110119         ENDIF;
036300100708
036400100708         //?Carico sk Importanza Clienti
036500100708         clear xx;
036600100708         k_TBLcod = 'IC';
036700100708         setll (k_TBLkut:k_TBLcod) TABEL00F;
036800100708         reade (k_TBLkut:k_TBLcod) TABEL00F;
036900100708
037000100708         DOW not %eof(TABEL00F);
037100100708           IF  TBLflg = *blanks;
037200100708             xx += 1;
037300100708             $Ic(xx) = %subst(TBLkey:1:1);
037400100708           ENDIF;
037500100708           reade (k_TBLkut:k_TBLcod) TABEL00F;
037600100708         ENDDO;
037700100706
037800100706         //?Carico sk Concorrenti
037900100706         clear xx;
038000100706         C_Tab = 'ITS';
038100100708         setll C_tab TNTBE01L;
038200100708         reade C_tab TNTBE01L;
038300100706
038400100706         DOW not %eof(TNTBE01L);
038500100706           IF  TBEatb = *blanks;
038600100706             xx += 1;
038700100706             $Its(xx) = %subst(TBEke2:1:4);
038800100706           ENDIF;
038900100708           reade C_tab TNTBE01L;
039000100706         ENDDO;
039100100531
039200100531         //?Carico sk Tipi trattavia
039300100531         clear xx;
039400100706         C_Tab = 'TTR';
039500100708         setll C_tab TNTBE01L;
039600100708         reade C_tab TNTBE01L;
039700100531
039800100531         DOW not %eof(TNTBE01L);
039900100531           IF  TBEatb = *blanks;
040000100531             xx += 1;
040100100531             $Ttr(xx) = %subst(TBEke1:1:1);
040200100531           ENDIF;
040300100708           reade C_tab TNTBE01L;
040400100531         ENDDO;
040500090714
040600080206       ENDSR;
040700080206
040800080206       //--------------------------------------------------------------
040900080206       //?Reperimento Dati del job (Utente/Operativi).
041000080206       //--------------------------------------------------------------
041100080206       BEGSR DatiJob;
041200080206
041300080206         in(E) §AzUte;
041400080206         if NOT %error;
041500080206           in(E) §DatiUte;
041600080206         endif;
041700080206         if %error or RSut = *blanks;
041800080206           clear TIBS34ds;
041900080206           tibs34r(tibs34ds);
042000080206           in §AzUte;
042100080206           in §DatiUte;
042200080206         endif;
042300080206
042400080206       ENDSR;
042500080206
042600080206       //--------------------------------------------------------------
042700080206       //?Gestione videata D01
042800080206       //--------------------------------------------------------------
042900080206       BEGSR GesD01;
043000080206
043100100326       //?Inizializzazione videata
043200100326         IF  $InzD01;
043300080206           exsr InzD01;
043400080206           $InzD01 = *off;
043500100326         ENDIF;
043600080206
043700100326       //?Emissione videata
043800100524         exfmt TA89D01;
043900100326         ErrMessage   = *off;
044000100326         ErrGenerico  = *off;
044100100326         clear V01msg;
044200080207
044300100326         SELECT;
044400100326
044500100326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
044600100326           WHEN  $ErrGrave;
044700100326             $Fine = *on;
044800100326
044900100326       //?F3=Fine
045000100326           WHEN  dsp_aid = c_F03;
045100080206             exsr F03D01;
045200100329
045300100329       //?F6=Conferma
045400100329           WHEN  dsp_aid = c_F06;
045500100329             exsr CtrD01;
045600100329             IF  ErrGenerico;
045700100329               leavesr;
045800100329             ENDIF;
045900100329             exsr F06D01;
046000100329
046100100326       //?Enter
046200100326           OTHER;
046300080206             exsr CtrD01;
046400100326             IF  ErrGenerico;
046500080206               leavesr;
046600100326             ENDIF;
046700100326         ENDSL;
046800080206
046900080206       ENDSR;
047000080206
047100080206       //--------------------------------------------------------------
047200080206       //?Inizializzazione videata D01
047300080206       //--------------------------------------------------------------
047400080206       BEGSR InzD01;
047500080206
047600100708         clear TA89D01;
047700100329         V01pgm = SDSpgm;
047800100329         V01sut = rsut;
047900100329         V01sif = knsif;
048000100329         V01mus = knmus;
048100101228
048200101228         V01valttsi = '+';
048300101228         V01valttsf = '+';
048400100708
048500100708         V01delsi = '+';
048600100708         V01delsf = '+';
048700101228
048800101228         V01auscsi = '+';
048900101228         V01auscsf = '+';
049000110119
049100110119       //?Posiziono il cursore sul commerciale se NO richiesta per area
049200110119         IF  DspStpCar;
049300110119           PosCurCmm = *on;
049400110119         ENDIF;
049500100708
049600080206       ENDSR;
049700080206
049800080206       //--------------------------------------------------------------
049900080206       //?Gestione tasto funzionale F3 da videata D01
050000080206       //?F3=Fine
050100080206       //--------------------------------------------------------------
050200080206       BEGSR F03D01;
050300080206
050400100329       //?Chiusura del programma
050500080206         $Fine = *on;
050600080206
050700080206       ENDSR;
050800100329
050900100329       //--------------------------------------------------------------
051000100329       //?Gestione tasto funzionale F6 da videata D01
051100100329       //?F6=Conferma
051200100329       //--------------------------------------------------------------
051300100329       BEGSR F06D01;
051400100706
051500100708       //?Videata sucessiva
051600100706         $Video = 'D2';
051700100706         $InzD02 = *on;
051800100708
051900100708       ENDSR;
052000100708
052100100708       //--------------------------------------------------------------
052200100708       //?Gestione videata D02
052300100708       //--------------------------------------------------------------
052400100708       BEGSR GesD02;
052500100708
052600100708       //?Inizializzazione videata
052700100708         IF  $InzD02;
052800100708           exsr InzD02;
052900100708           $InzD02 = *off;
053000100708         ENDIF;
053100100708
053200100708       //?Emissione videata
053300100708         exfmt TA89D02;
053400100708         ErrMessage   = *off;
053500100708         ErrGenerico  = *off;
053600100708         clear V02msg;
053700100708
053800100708         SELECT;
053900100708
054000100708       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
054100100708           WHEN  $ErrGrave;
054200100708             $Fine = *on;
054300100708
054400100708       //?F3=Fine
054500100708           WHEN  dsp_aid = c_F03;
054600100708             exsr F03D01;
054700100708
054800100708       //?F6=Conferma
054900100708           WHEN  dsp_aid = c_F06;
055000100708             exsr CtrD02;
055100100708             IF  ErrGenerico;
055200100708               leavesr;
055300100708             ENDIF;
055400100708             exsr F06D02;
055500100708
055600100708       //?F12=Ritorno
055700100708           WHEN  dsp_aid = c_F12;
055800100708             exsr F12D02;
055900100708
056000100708       //?Enter
056100100708           OTHER;
056200100708             exsr CtrD02;
056300100708             IF  ErrGenerico;
056400100708               leavesr;
056500100708             ENDIF;
056600100708         ENDSL;
056700100708
056800100708       ENDSR;
056900100708
057000100708       //--------------------------------------------------------------
057100100708       //?Inizializzazione videata D02
057200100708       //--------------------------------------------------------------
057300100708       BEGSR InzD02;
057400100708
057500100708         clear TA89D02;
057600100708         V02pgm = SDSpgm;
057700100708         V02sut = rsut;
057800100708         V02sif = knsif;
057900100708         V02mus = knmus;
058000100708
058100100915         V02ttrio = 'O';
058200100708         V02ord   = 'D';
058300100716         V02cmmt  = 'S';
058400100716         IF  V01cmm = *zeros or V01cmm = *blanks;
058500100716           V02gent = 'S';
058600100716         ELSE;
058700100716           V02gent = 'N';
058800100716         ENDIF;
058900100708
059000100708       ENDSR;
059100100708
059200100708       //--------------------------------------------------------------
059300100708       //?Gestione tasto funzionale F6 da videata D02
059400100708       //?F6=Conferma
059500100708       //--------------------------------------------------------------
059600100708       BEGSR F06D02;
059700100708
059800100708         clear TNTA98ds;
059900110119         IF  V01car > *zeros;
060000110119           ITA98car   = %dec(V01car:3:0);
060100110119         ENDIF;
060200100708         IF  V01cmm > *zeros;
060300100708           ITA98cmm   = %dec(V01cmm:7:0);
060400100708         ENDIF;
060500100708         ITA98elcmm = V01elcmm;
060600100708         ITA98dtavi = wdtavi;
060700100708         ITA98dtavf = wdtavf;
060800100708         ITA98dtchi = wdtchi;
060900100708         ITA98dtchf = wdtchf;
061000100708         ITA98dtica = wdtica;
061100110114       //?Imposto il valore traffico totale con il segno immesso a video
061200110114         IF  V01valttsi = '-';
061300110114           ITA98vltti = V01valtti * -1;
061400110114         ELSE;
061500110114           ITA98vltti = V01valtti;
061600110114         ENDIF;
061700110114         IF  V01valttsf = '-';
061800110114           ITA98vlttf = V01valttf * -1;
061900110114         ELSE;
062000110114           ITA98vlttf = V01valttf;
062100110114         ENDIF;
062200100708         ITA98valte = V01valte;
062300100708         ITA98pkg   = V01pkg;
062400100708         ITA98conc1 = V01conc1;
062500100708         ITA98conc2 = V01conc2;
062600100708         ITA98conc3 = V01conc3;
062700100708         ITA98conc4 = V01conc4;
062800100708         ITA98conc5 = V01conc5;
062900100708         ITA98delsi = V01delsi;
063000100714         ITA98deli  = V01deli;
063100100708         ITA98delsf = V01delsf;
063200100714         ITA98delf  = V01delf;
063300110114       //?Imposto il valore aumento/sconto con il segno immesso a video
063400110114         IF  V01auscsi = '-';
063500110114           ITA98ausi = V01ausci * -1;
063600110114         ELSE;
063700110114           ITA98ausi = V01ausci;
063800110114         ENDIF;
063900110114         IF  V01auscsf = '-';
064000110114           ITA98ausf = V01auscf * -1;
064100110114         ELSE;
064200110114           ITA98ausf = V01auscf;
064300110114         ENDIF;
064400100708         ITA98tpt1  = V01ttr1;
064500100708         ITA98tpt2  = V01ttr2;
064600100708         ITA98tpt3  = V01ttr3;
064700100708         ITA98tpt4  = V01ttr4;
064800101228         ITA98tpt5  = V01ttr5;
064900101228         ITA98tpt6  = V01ttr6;
065000100708         ITA98tic1  = V01tic1;
065100100708         ITA98tic2  = V01tic2;
065200100708         ITA98tic3  = V01tic3;
065300100708         ITA98tic4  = V01tic4;
065400100708         ITA98tic5  = V01tic5;
065500100709         ITA98calde = V01calde;
065600100729         ITA98dtaff = wdtaff;
065700100708         ITA98esi   = V01esi;
065800100708
065900100708         ITA98det   = V02det;
066000100708         ITA98detc  = V02detc;
066100100708         ITA98ttrio = V02ttrio;
066200100708         ITA98ttr1  = V02ttr1;
066300100708         ITA98ttr2  = V02ttr2;
066400100708         ITA98ttr3  = V02ttr3;
066500100708         ITA98ttr4  = V02ttr4;
066600101228         ITA98ttr5  = V02ttr5;
066700101228         ITA98ttr6  = V02ttr6;
066800100708         ITA98ord   = V02ord;
066900100708         ITA98cmmt  = V02cmmt;
067000100708         ITA98gent  = V02gent;
067100100708         kcoaz = 'TA98';
067200100708         kpjbu = TNTA98ds;
067300100708         kbuff = *blanks;
067400100708
067500100708       //?Richiamo interattivo
067600100714         IF  knmus = *all'1';
067700100708           TNTA98C(kpjba);
067800100708       //?Richiamo batch
067900100714         ELSE;
068000100714           BCH10(kpjba);
068100100714         ENDIF;
068200100708
068300100708       //?Chiusura del programma
068400100708         $Fine = *on;
068500100708
068600100708       ENDSR;
068700100708
068800100708       //--------------------------------------------------------------
068900100708       //?Gestione tasto funzionale F12 da videata D02
069000100708       //?F12=Ritorno
069100100708       //--------------------------------------------------------------
069200100708       BEGSR F12D02;
069300100708
069400100708       //?Ritorno alla videata precedente
069500100708         $InzD01 = *off;
069600100708         $Video = 'D1';
069700100708
069800100708       ENDSR;
069900090717
070000080206       //--------------------------------------------------------------
070100080206       //?Controllo videata D01
070200080206       //--------------------------------------------------------------
070300080206       BEGSR CtrD01;
070400110119
070500110119         clear v01dcar;
070600110119         clear v01dcmm;
070700100326
070800100706         clear wdtaff;
070900100706         clear wdtavf;
071000100706         clear wdtavi;
071100100706         clear wdtchf;
071200100706         clear wdtchi;
071300100706         clear wdtica;
071400100708
071500101228         clear wausci;
071600101228         clear wauscf;
071700100708         clear wdeli;
071800100708         clear wdelf;
071900101228         clear wvaltti;
072000101228         clear wvalttf;
072100090427
072200100326         WindDspF = IndDspF;
072300100326         reset WindDspFb;
072400100326         IndDspF  = WindDspF;
072500110119
072600110119       //?AREA
072700110119       //?Ricerca
072800110119         IF  %scan('?' : V01car) > *zero;
072900110119           idTabella = '05';
073000110119           clear Ordinamento;
073100110119           clear idElemento;
073200110119           clear TastoUscita;
073300110119           Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
073400110119           V01car = idElemento;
073500110119         ENDIF;
073600110119
073700110119       //?Controllo se impostato
073800110119         IF  V01car <> *blanks and V01car <> *zeros;
073900110119        //?Solo numerico
074000110119           IF  %check(digits:V01car) > 0;
074100110119             ErrMessage  = *on;
074200110119             ErrGenerico = *on;
074300110119             PosCurCar   = *on;
074400110119             V01msg      = $Msg(29);
074500110119             leavesr;
074600110119           ENDIF;
074700110119         //?Valido in tabella
074800110119           clear ds05;
074900110119           k_TBLcod = '05';
075000110119           k_TBLkey = V01car;
075100110119           chain  %kds(K03tabel) TABEL00F;
075200110119           IF  NOT %found(TABEL00F)
075300110119               or  TBLflg <> *blank;
075400110119             ErrMessage  = *on;
075500110119             ErrGenerico = *on;
075600110119             PosCurCar   = *on;
075700110119             V01msg      = $Msg(29);
075800110119             leavesr;
075900110119           ENDIF;
076000110119
076100110119           ds05 = TBLuni;
076200110119           V01dcar = §05des;
076300110119
076400110119         //?Utente abilitato all'area
076500110119           IF  %lookup(V01car : car) = 0;
076600110119             ErrMessage  = *on;
076700110119             ErrGenerico = *on;
076800110119             PosCurCar   = *on;
076900110119             V01msg      = $Msg(29);
077000110119             leavesr;
077100110119           ENDIF;
077200110119         ENDIF;
077300100326
077400100706       //?COMMERCIALE
077500110921
077600110921       //?Se sono in sede e non seleziono ne l'area e ne il commerciale errore
077700110921         IF   dutpou = 046 and ((V01car = *blanks or V01car = *zeros) and
077800110921           (V01cmm = *blanks or V01cmm = *zeros));
077900110921           ErrMessage  = *on;
078000110921           ErrGenerico = *on;
078100110921           PosCurCar   = *on;
078200110921           V01msg      = $Msg(31);
078300110921           leavesr;
078400110921         ENDIF;
078500110921
078600110119       //?Non immettere se richiesta l'area
078700110119         IF  V01car <> *blanks and V01car > *zeros and
078800110119             V01cmm <> *blanks and V01cmm > *zeros;
078900110119           ErrMessage  = *on;
079000110119           ErrGenerico = *on;
079100110119           PosCurCmm   = *on;
079200110119           V01msg      = $Msg(30);
079300110119           leavesr;
079400110119         ENDIF;
079500110119
079600100706       //?Ricerca
079700100326         IF  %scan('?' : V01cmm) > *zero;
079800100326           ErrGenerico = *on;
079900100326           PosCurCmm   = *on;
080000100428           tb85Kfil = %editc(DUTpou:'X');
080100100326           clear tb85Key ;
080200100326           clear tb85Des1;
080300100329           trtb85r (kpjba : tb85kfil : tb85key : tb85des1 : tb85Uni);
080400100326           V01cmm  = tb85Key;
080500100706           V01dcmm = tb85des1;
080600100326           leavesr;
080700100326         ENDIF;
080800100326
080900100706       //?Controllo se impostato
081000100330         IF  V01cmm <> *blanks and V01cmm <> *zeros;
081100100706        //?Solo numerico
081200100326           IF  %check(digits:V01cmm) > 0;
081300100326             ErrMessage  = *on;
081400100326             ErrGenerico = *on;
081500100326             PosCurCmm   = *on;
081600100329             V01msg      = $Msg(02);
081700100326             leavesr;
081800100326           ENDIF;
081900100706         //?Valido in tabella
082000100326           clear ds01;
082100100708           k_TBLcod = '01';
082200100326           k_TBLkey = V01cmm;
082300100326           chain  %kds(K03tabel) TABEL00F;
082400100326           IF  NOT %found(TABEL00F)
082500100326               or  TBLflg <> *blank;
082600100326             ErrMessage  = *on;
082700100326             ErrGenerico = *on;
082800100329             PosCurCmm   = *on;
082900100329             V01msg      = $Msg(02);
083000100326             leavesr;
083100100326           ENDIF;
083200100326
083300100326           ds01 = TBLuni;
083400100706           V01dcmm = §01age;
083500100326
083600100706         //?No vari o clienti inattivi
083700101228           IF  §01par <> *blanks;
083800100326             ErrMessage  = *on;
083900100326             ErrGenerico = *on;
084000100326             PosCurCmm   = *on;
084100100329             V01msg      = $Msg(02);
084200100326             leavesr;
084300100326           ENDIF;
084400100706         //?Utente abilitato al commerciale
084500100326           clear TNTAA1DS;
084600100326           ITAA1caut = '§utegtc';
084700100326           ITAA1cmm  = %dec(V01cmm:7:0);
084800100326           kpjbu     = tntaa1ds;
084900100326           tntaa1c (kpjba);
085000100326           TNTAA1DS = kpjbu;
085100100326           IF  OTAA1fabi = 'N';
085200100326             ErrMessage  = *on;
085300100326             ErrGenerico = *on;
085400100326             PosCurCmm   = *on;
085500100329             V01msg      = $Msg(02);
085600100326             leavesr;
085700100326           ENDIF;
085800100706         //?Solo unificante
085900100524           IF  %dec(V01cmm:7:0) <> §01rgf;
086000100524             ErrMessage  = *on;
086100100524             ErrGenerico = *on;
086200100524             PosCurCmm   = *on;
086300100524             V01msg      = $Msg(03);
086400100524             leavesr;
086500100524           ENDIF;
086600100524         ENDIF;
086700100709
086800100709       //?Tipo commerciale INTERNO ESTERNO TUTTI
086900100709       //?Se impostato un solo codice commerciale la selezione
087000100709       //?deve essere congruente con il codice
087100110119         IF  (V01car > *zeros or V01cmm > *zeros) and
087200110119             V01elcmm <> *blanks;
087300101006           IF  V01elcmm = 'I' and §01fun <> 'COMIN'
087400110119               and §01fun <> 'ASC' and §01fun <> 'SA'
087500110119               and V01cmm > *zeros;
087600100709           ErrMessage  = *on;
087700100709           ErrGenerico = *on;
087800100709           PosCurCmm   = *on;
087900101006           V01msg      = $Msg(24);
088000101006           V01msg      = %trim(V01msg) + ' Richiesta selezione INTERNO';
088100100709           leavesr;
088200100709           ENDIF;
088300101006           IF  V01elcmm = 'E' and (§01fun = 'COMIN'
088400110119               or §01fun = 'ASC' or §01fun = 'SA')
088500110119               and V01cmm > *zeros;
088600100709           ErrMessage  = *on;
088700100709           ErrGenerico = *on;
088800100709           PosCurCmm   = *on;
088900101006           V01msg      = $Msg(24);
089000101006           V01msg      = %trim(V01msg) + ' Richiesta selezione ESTERNO';
089100100709           leavesr;
089200100709           ENDIF;
089300100709         ENDIF;
089400100706
089500100706       //?DATE
089600100706       //?Almeno una delle tre date è da immettere
089700100706         IF  V01dtavi = 0 and V01dtavf = 0 and
089800100706             V01dtchi = 0 and V01dtchf = 0 and
089900100706             V01dtica = 0;
090000100708           ErrMessage  = *on;
090100100708           ErrGenerico = *on;
090200100706           PosCurDtavi = *on;
090300100706           V01msg      = $Msg(04);
090400100706           V01msg      = %trim(V01msg) + '. Richiedere almeno un periodo';
090500100706           leavesr;
090600100706         ENDIF;
090700100706
090800100708       //?Trattative avviate
090900100708         IF  V01dtavi <> 0 or V01dtavf <> 0;
091000100708         //?Dal
091100100706           IF  V01dtavi <> 0;
091200100706             clear wlbdat;
091300100706             G02dat = V01dtavi;
091400100706             xsrda8(wlbdat);
091500100706             IF  G02err = '1';
091600100706               ErrMessage  = *on;
091700100706               ErrGenerico = *on;
091800100706               PosCurDtavi = *on;
091900100706               V01msg      = $Msg(04);
092000100706               leavesr;
092100100706             ENDIF;
092200100706
092300100706             V01dtavi = G02dat;
092400100706             wdtavi   = G02inv;
092500121029
092600121029         //?La data non può essere inferiore a 2 anni da oggi
092700121029             IF  wdtavi < wOggi_2;
092800121029               ErrMessage  = *on;
092900121029               ErrGenerico = *on;
093000121029               PosCurDtavi = *on;
093100121029               V01msg      = $Msg(04);
093200121029               V01msg = %trim(V01msg) +
093300121029               ' è inferiore a 2 anni, non ci sono i dati';
093400121029               leavesr;
093500121029             ENDIF;
093600100708
093700100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
093800100706             IF  wdtavi < 20000101 or wdtavi > 20391231;
093900100706               ErrMessage  = *on;
094000100706               ErrGenerico = *on;
094100100706               PosCurDtavi = *on;
094200100706               V01msg      = $Msg(04);
094300100706               leavesr;
094400100706             ENDIF;
094500100706           ENDIF;
094600100708         //?Al
094700100706           IF  V01dtavf <> 0;
094800100706             clear wlbdat;
094900100706             G02dat = V01dtavf;
095000100706             xsrda8(wlbdat);
095100100706             IF  G02err = '1';
095200100706               ErrMessage  = *on;
095300100706               ErrGenerico = *on;
095400100706               PosCurDtavf = *on;
095500100706               V01msg      = $Msg(04);
095600100706               leavesr;
095700100706             ENDIF;
095800100706
095900100706             V01dtavf = G02dat;
096000100706             wdtavf   = G02inv;
096100100706
096200100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
096300100706             IF  wdtavf < 20000101 or wdtavf > 20391231;
096400100706               ErrMessage  = *on;
096500100706               ErrGenerico = *on;
096600100706               PosCurDtavf = *on;
096700100706               V01msg      = $Msg(04);
096800100706               leavesr;
096900100706             ENDIF;
097000100706           ENDIF;
097100100708         //?Se impostata data "dal" e non "al"
097200100706         //?imposto "al" = data del giorno
097300100706           IF  V01dtavi > 0 and V01dtavf = 0;
097400100706             wdtavf    = %dec(%date());
097500100706             data_eur  = %date(wdtavf:*iso);
097600100706             V01dtavf  = %dec(data_eur);
097700100706           ENDIF;
097800100708         //?"DAL" congruente con "AL"
097900100706           IF  wdtavi > 0  and  wdtavf = 0;
098000100706             ErrMessage  = *on;
098100100706             ErrGenerico = *on;
098200100706             PosCurDtavi = *on;
098300100706             V01msg      = $Msg(05);
098400100706             leavesr;
098500100706           ENDIF;
098600100706           IF  wdtavi > 0  and  wdtavf > 0  and  wdtavi > wdtavf;
098700100706             ErrMessage  = *on;
098800100706             ErrGenerico = *on;
098900100706             PosCurDtavf = *on;
099000100706             V01msg      = $Msg(05);
099100100706             leavesr;
099200100706           ENDIF;
099300100908         //?Range massimo di 65 giorni
099400100908           data_iso  = %date(wdtavi);
099500100908           data_iso1 = %date(wdtavf);
099600100908           wgiorni = %diff( data_iso1 : data_iso : *days );
099700100908           IF  wgiorni > 65;
099800100908             ErrMessage  = *on;
099900100908             ErrGenerico = *on;
100000100908             PosCurDtavi = *on;
100100100908             V01msg      = $Msg(04);
100200100908             V01msg      = %trim(V01msg) + '. Range richiesto > di 65 gg.';
100300100908             leavesr;
100400100908           ENDIF;
100500100706         ENDIF;
100600100706
100700100708       //?Trattative chiuse
100800100706         IF  V01dtchi <> 0 or V01dtchi <> 0;
100900100708         //?Dal
101000100706           IF  V01dtchi <> 0;
101100100706             clear wlbdat;
101200100706             G02dat = V01dtchi;
101300100706             xsrda8(wlbdat);
101400100706             IF  G02err = '1';
101500100706               ErrMessage  = *on;
101600100706               ErrGenerico = *on;
101700100706               PosCurDtchi = *on;
101800100706               V01msg      = $Msg(04);
101900100706               leavesr;
102000100706             ENDIF;
102100100706
102200100706             V01dtchi = G02dat;
102300100706             wdtchi   = G02inv;
102400121029
102500121029         //?La data non può essere inferiore a 2 anni da oggi
102600121029             IF  wdtchi < wOggi_2;
102700121029               ErrMessage  = *on;
102800121029               ErrGenerico = *on;
102900121029               PosCurDtchi = *on;
103000121029               V01msg      = $Msg(04);
103100121029               V01msg = %trim(V01msg) +
103200121029               ' è inferiore a 2 anni, non ci sono i dati';
103300121029               leavesr;
103400121029             ENDIF;
103500100706
103600100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
103700100706             IF  wdtchi < 20000101 or wdtchi > 20391231;
103800100706               ErrMessage  = *on;
103900100706               ErrGenerico = *on;
104000100706               PosCurDtchi = *on;
104100100706               V01msg      = $Msg(04);
104200100706               leavesr;
104300100706             ENDIF;
104400100706           ENDIF;
104500100708         //?Al
104600100706           IF  V01dtchf <> 0;
104700100706             clear wlbdat;
104800100706             G02dat = V01dtchf;
104900100706             xsrda8(wlbdat);
105000100706             IF  G02err = '1';
105100100706               ErrMessage  = *on;
105200100706               ErrGenerico = *on;
105300100706               PosCurDtchf = *on;
105400100706               V01msg      = $Msg(04);
105500100706               leavesr;
105600100706             ENDIF;
105700100706
105800100706             V01dtchf = G02dat;
105900100706             wdtchf   = G02inv;
106000100706
106100100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
106200100706             IF  wdtchf < 20000101 or wdtchf > 20391231;
106300100706               ErrMessage  = *on;
106400100706               ErrGenerico = *on;
106500100706               PosCurDtchf = *on;
106600100706               V01msg      = $Msg(04);
106700100706               leavesr;
106800100706             ENDIF;
106900100706           ENDIF;
107000100708         //?Se impostata data "dal" e non "al"
107100100706         //?imposto "al" = data del giorno
107200100706           IF  V01dtchi > 0 and V01dtchf = 0;
107300100706             wdtchf    = %dec(%date());
107400100706             data_eur  = %date(wdtchf:*iso);
107500100706             V01dtchf  = %dec(data_eur);
107600100706           ENDIF;
107700100708         //?"DAL" congruente con "AL"
107800100706           IF  wdtchi > 0  and  wdtchf = 0;
107900100706             ErrMessage  = *on;
108000100706             ErrGenerico = *on;
108100100706             PosCurDtchi = *on;
108200100706             V01msg      = $Msg(05);
108300100706             leavesr;
108400100706           ENDIF;
108500100706           IF  wdtchi > 0  and  wdtchf > 0  and  wdtchi > wdtchf;
108600100706             ErrMessage  = *on;
108700100706             ErrGenerico = *on;
108800100706             PosCurDtchf = *on;
108900100706             V01msg      = $Msg(05);
109000100706             leavesr;
109100100706           ENDIF;
109200100908         //?Range massimo di 65 giorni
109300100908           data_iso  = %date(wdtchi);
109400100908           data_iso1 = %date(wdtchf);
109500100908           wgiorni = %diff( data_iso1 : data_iso : *days );
109600100908           IF  wgiorni > 65;
109700100908             ErrMessage  = *on;
109800100908             ErrGenerico = *on;
109900100908             PosCurDtchi = *on;
110000100908             V01msg      = $Msg(04);
110100100908             V01msg      = %trim(V01msg) + '. Range richiesto > di 65 gg.';
110200100908             leavesr;
110300100908           ENDIF;
110400100706         ENDIF;
110500100706
110600100708       //?Trattative in corso
110700100706         IF  V01dtica <> 0;
110800100706           clear wlbdat;
110900100706           G02dat = V01dtica;
111000100706           xsrda8(wlbdat);
111100100706           IF  G02err = '1';
111200100706             ErrMessage  = *on;
111300100706             ErrGenerico = *on;
111400100706             PosCurDtica = *on;
111500100706             V01msg      = $Msg(04);
111600100706             leavesr;
111700100706           ENDIF;
111800100706
111900100706           V01dtica = G02dat;
112000100706           wdtica   = G02inv;
112100100706
112200100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
112300100706           IF  wdtica < 20000101 or wdtica > 20391231;
112400100706             ErrMessage  = *on;
112500100706             ErrGenerico = *on;
112600100706             PosCurDtica = *on;
112700100706             V01msg      = $Msg(04);
112800100706             leavesr;
112900100706           ENDIF;
113000100909         //?Massimo 65 giorni indietro rispetto a oggi
113100100706           data_iso  = %date(wdtica);
113200100708           data_iso1 = %date();
113300100706           wgiorni = %diff( data_iso1 : data_iso : *days );
113400100909           IF  wgiorni > 65;
113500100706             ErrMessage  = *on;
113600100706             ErrGenerico = *on;
113700100706             PosCurDtica = *on;
113800100706             V01msg      = $Msg(04);
113900100909             V01msg      = %trim(V01msg) + '. Più vecchia di 65 gg. da oggi';
114000100706             leavesr;
114100100706           ENDIF;
114200100706         ENDIF;
114300100706
114400100706       //?PARAMETRI DI SELEZIONE
114500101228         //?Valore traffico totale
114600101228         IF  V01valtti <> 0 or V01valttf <> 0;
114700101228         //?Obbligatori tutti e 2 se immesso uno dei due
114800101228           IF  V01valtti <> 0  and  V01valttf = 0;
114900101228             ErrMessage  = *on;
115000101228             ErrGenerico = *on;
115100101228             PosCurVlttf = *on;
115200101228             V01msg      = $Msg(25);
115300101228             leavesr;
115400101228           ENDIF;
115500101228           IF  V01valtti = 0  and  V01valttf <> 0;
115600101228             ErrMessage  = *on;
115700101228             ErrGenerico = *on;
115800101228             PosCurVltti = *on;
115900101228             V01msg      = $Msg(25);
116000101228             leavesr;
116100101228           ENDIF;
116200101228         //?Controllo congruenza tra "DAL" e "AL"
116300101228           wvaltti = V01valtti;
116400101228           IF  V01valttsi = '-';
116500101228             wvaltti = wvaltti * -1;
116600101228           ENDIF;
116700101228           wvalttf = V01valttf;
116800101228           IF  V01valttsf = '-';
116900101228             wvalttf = wvalttf * -1;
117000101228           ENDIF;
117100101228           IF  wvaltti > wvalttf;
117200101228             ErrMessage  = *on;
117300101228             ErrGenerico = *on;
117400101228             PosCurVltti = *on;
117500101228             V01msg      = $Msg(26);
117600101228             leavesr;
117700101228           ENDIF;
117800101228         ENDIF;
117900101228
118000101228         //?Concorrente 1
118100100708         IF  V01conc1 <> *blanks;
118200100708           IF    %scan('?' : V01conc1) > 0;
118300100708             ErrGenerico = *on;
118400100708             PosCurConc1  = *on;
118500100708             clear TIBS02ds;
118600100708             T02mod = 'R';
118700100708             T02cod = 'ITS';
118800100708             T02sif = KNSIF;
118900100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
119000100708             IF  T02err = *blanks;
119100100708               V01conc1 = t02ke2;
119200100708             ENDIF;
119300100708           ENDIF;
119400100708           IF  %lookup(V01conc1 : $Its) = 0;
119500100708             ErrMessage  = *on;
119600100708             ErrGenerico = *on;
119700100708             PosCurConc1 = *on;
119800100708             V01msg = $Msg(09);
119900100708             leavesr;
120000100708           ENDIF;
120100100708         ENDIF;
120200100708         //?Concorrente 2
120300100708         IF  V01conc2 <> *blanks;
120400100708           IF    %scan('?' : V01conc2) > 0;
120500100708             ErrGenerico = *on;
120600100708             PosCurConc2  = *on;
120700100708             clear TIBS02ds;
120800100708             T02mod = 'R';
120900100708             T02cod = 'ITS';
121000100708             T02sif = KNSIF;
121100100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
121200100708             IF  T02err = *blanks;
121300100708               V01conc2 = t02ke2;
121400100708             ENDIF;
121500100708           ENDIF;
121600100708           IF  %lookup(V01conc2 : $Its) = 0;
121700100708             ErrMessage  = *on;
121800100708             ErrGenerico = *on;
121900100708             PosCurConc2 = *on;
122000100708             V01msg = $Msg(09);
122100100708             leavesr;
122200100708           ENDIF;
122300100708         ENDIF;
122400100708         //?Concorrente 3
122500100708         IF  V01conc3 <> *blanks;
122600100708           IF    %scan('?' : V01conc3) > 0;
122700100708             ErrGenerico = *on;
122800100708             PosCurConc3  = *on;
122900100708             clear TIBS02ds;
123000100708             T02mod = 'R';
123100100708             T02cod = 'ITS';
123200100708             T02sif = KNSIF;
123300100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
123400100708             IF  T02err = *blanks;
123500100708               V01conc3 = t02ke2;
123600100708             ENDIF;
123700100708           ENDIF;
123800100708           IF  %lookup(V01conc3 : $Its) = 0;
123900100708             ErrMessage  = *on;
124000100708             ErrGenerico = *on;
124100100708             PosCurConc3 = *on;
124200100708             V01msg = $Msg(09);
124300100708             leavesr;
124400100708           ENDIF;
124500100708         ENDIF;
124600100708         //?Concorrente 4
124700100708         IF  V01conc4 <> *blanks;
124800100708           IF    %scan('?' : V01conc4) > 0;
124900100708             ErrGenerico = *on;
125000100708             PosCurConc4  = *on;
125100100708             clear TIBS02ds;
125200100708             T02mod = 'R';
125300100708             T02cod = 'ITS';
125400100708             T02sif = KNSIF;
125500100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
125600100708             IF  T02err = *blanks;
125700100708               V01conc4 = t02ke2;
125800100708             ENDIF;
125900100708           ENDIF;
126000100708           IF  %lookup(V01conc4 : $Its) = 0;
126100100708             ErrMessage  = *on;
126200100708             ErrGenerico = *on;
126300100708             PosCurConc4 = *on;
126400100708             V01msg = $Msg(09);
126500100708             leavesr;
126600100708           ENDIF;
126700100708         ENDIF;
126800100708         //?Concorrente 5
126900100708         IF  V01conc5 <> *blanks;
127000100708           IF    %scan('?' : V01conc5) > 0;
127100100708             ErrGenerico = *on;
127200100708             PosCurConc5  = *on;
127300100708             clear TIBS02ds;
127400100708             T02mod = 'R';
127500100708             T02cod = 'ITS';
127600100708             T02sif = KNSIF;
127700100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
127800100708             IF  T02err = *blanks;
127900100708               V01conc5 = t02ke2;
128000100708             ENDIF;
128100100708           ENDIF;
128200100708           IF  %lookup(V01conc5 : $Its) = 0;
128300100708             ErrMessage  = *on;
128400100708             ErrGenerico = *on;
128500100708             PosCurConc5 = *on;
128600100708             V01msg = $Msg(09);
128700100708             leavesr;
128800100708           ENDIF;
128900100708         ENDIF;
129000100706
129100100708       //?Delta
129200100708         IF  V01deli <> *blanks or V01delf <> *blanks;
129300100708         //?Solo valori numerici
129400100708           IF  V01deli <> *blanks;
129500100708             IF  %check(digits:V01deli) > 0;
129600100708               ErrMessage  = *on;
129700100708               ErrGenerico = *on;
129800100708               PosCurDeli  = *on;
129900100708               V01msg      = $Msg(19);
130000100708               leavesr;
130100100708             ENDIF;
130200100708           ENDIF;
130300100708           IF  V01delf <> *blanks;
130400100708             IF  %check(digits:V01delf) > 0;
130500100708               ErrMessage  = *on;
130600100708               ErrGenerico = *on;
130700100708               PosCurDelf  = *on;
130800100708               V01msg      = $Msg(19);
130900100708               leavesr;
131000100708             ENDIF;
131100100708           ENDIF;
131200100708         //?Obbligatori tutti e 2 se immesso uno dei due
131300100708           IF  V01deli <> *blanks  and  V01delf = *blanks;
131400100708             ErrMessage  = *on;
131500100708             ErrGenerico = *on;
131600100708             PosCurDelf  = *on;
131700100708             V01msg      = $Msg(13);
131800100708             leavesr;
131900100708           ENDIF;
132000100708           IF  V01deli = *blanks  and  V01delf <> *blanks;
132100100708             ErrMessage  = *on;
132200100708             ErrGenerico = *on;
132300100708             PosCurDeli  = *on;
132400100708             V01msg      = $Msg(13);
132500100708             leavesr;
132600100708           ENDIF;
132700100708         //?Controllo congruenza tra "DAL" e "AL"
132800100708           wdeli = %dec(V01deli:3:0);
132900100708           IF  V01delsi = '-';
133000100708             wdeli = wdeli * -1;
133100100708           ENDIF;
133200100708           wdelf = %dec(V01delf:3:0);
133300100708           IF  V01delsf = '-';
133400100708             wdelf = wdelf * -1;
133500100708           ENDIF;
133600100708           IF  wdeli > wdelf;
133700100708             ErrMessage  = *on;
133800100708             ErrGenerico = *on;
133900100708             PosCurDeli  = *on;
134000100708             V01msg      = $Msg(14);
134100100708             leavesr;
134200100708           ENDIF;
134300100708         ENDIF;
134400101228
134500101228       //?Aumento/Sconto
134600101228         IF  V01ausci <> 0 or V01auscf <> 0;
134700101228         //?Obbligatori tutti e 2 se immesso uno dei due
134800101228           IF  V01ausci <> 0  and  V01auscf = 0;
134900101228             ErrMessage  = *on;
135000101228             ErrGenerico = *on;
135100101228             PosCurAuScf = *on;
135200101228             V01msg      = $Msg(27);
135300101228             leavesr;
135400101228           ENDIF;
135500101228           IF  V01ausci = 0  and  V01auscf <> 0;
135600101228             ErrMessage  = *on;
135700101228             ErrGenerico = *on;
135800101228             PosCurAuSci = *on;
135900101228             V01msg      = $Msg(27);
136000101228             leavesr;
136100101228           ENDIF;
136200101228         //?Controllo congruenza tra "DAL" e "AL"
136300101228           wausci = V01ausci;
136400101228           IF  V01auscsi = '-';
136500101228             wausci = wausci * -1;
136600101228           ENDIF;
136700101228           wauscf = V01auscf;
136800101228           IF  V01auscsf = '-';
136900101228             wauscf = wauscf * -1;
137000101228           ENDIF;
137100101228           IF  wausci > wauscf;
137200101228             ErrMessage  = *on;
137300101228             ErrGenerico = *on;
137400101228             PosCurAuSci = *on;
137500101228             V01msg      = $Msg(28);
137600101228             leavesr;
137700101228           ENDIF;
137800101228         ENDIF;
137900100708
138000100708       //?Tipo trattativa
138100100708         IF  V01ttr1 <> *blanks;
138200100708           IF    %scan('?' : V01ttr1) > 0;
138300100708             ErrGenerico = *on;
138400100708             PosCurTtr1  = *on;
138500100708             clear TIBS02ds;
138600100708             T02mod = 'R';
138700100708             T02cod = 'TTR';
138800100708             T02sif = KNSIF;
138900100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
139000100708             IF  T02err = *blanks;
139100100708               V01ttr1 = t02ke1;
139200100708             ENDIF;
139300100708           ENDIF;
139400100708           IF  %lookup(V01ttr1 : $Ttr) = 0;
139500100708             ErrMessage  = *on;
139600100708             ErrGenerico = *on;
139700100708             PosCurTtr1  = *on;
139800100708             V01msg = $Msg(10);
139900100708             leavesr;
140000100708           ENDIF;
140100100708         ENDIF;
140200100708         IF  V01ttr2 <> *blanks;
140300100708           IF    %scan('?' : V01ttr2) > 0;
140400100708             ErrGenerico = *on;
140500100708             PosCurTtr2  = *on;
140600100708             clear TIBS02ds;
140700100708             T02mod = 'R';
140800100708             T02cod = 'TTR';
140900100708             T02sif = KNSIF;
141000100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
141100100708             IF  T02err = *blanks;
141200100708               V01ttr2 = t02ke1;
141300100708             ENDIF;
141400100708           ENDIF;
141500100708           IF  %lookup(V01ttr2 : $Ttr) = 0;
141600100708             ErrMessage  = *on;
141700100708             ErrGenerico = *on;
141800100708             PosCurTtr2  = *on;
141900100708             V01msg = $Msg(10);
142000100708             leavesr;
142100100708           ENDIF;
142200100708         ENDIF;
142300100708         IF  V01ttr3 <> *blanks;
142400100708           IF    %scan('?' : V01ttr3) > 0;
142500100708             ErrGenerico = *on;
142600100708             PosCurTtr3  = *on;
142700100708             clear TIBS02ds;
142800100708             T02mod = 'R';
142900100708             T02cod = 'TTR';
143000100708             T02sif = KNSIF;
143100100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
143200100708             IF  T02err = *blanks;
143300100708               V01ttr3 = t02ke1;
143400100708             ENDIF;
143500100708           ENDIF;
143600100708           IF  %lookup(V01ttr3 : $Ttr) = 0;
143700100708             ErrMessage  = *on;
143800100708             ErrGenerico = *on;
143900100708             PosCurTtr3  = *on;
144000100708             V01msg = $Msg(10);
144100100708             leavesr;
144200100708           ENDIF;
144300100708         ENDIF;
144400100708         IF  V01ttr4 <> *blanks;
144500100708           IF    %scan('?' : V01ttr4) > 0;
144600100708             ErrGenerico = *on;
144700100708             PosCurTtr4  = *on;
144800100708             clear TIBS02ds;
144900100708             T02mod = 'R';
145000100708             T02cod = 'TTR';
145100100708             T02sif = KNSIF;
145200100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
145300100708             IF  T02err = *blanks;
145400100708               V01ttr4 = t02ke1;
145500100708             ENDIF;
145600100708           ENDIF;
145700100708           IF  %lookup(V01ttr4 : $Ttr) = 0;
145800100708             ErrMessage  = *on;
145900100708             ErrGenerico = *on;
146000100708             PosCurTtr4  = *on;
146100100708             V01msg = $Msg(10);
146200100708             leavesr;
146300100708           ENDIF;
146400100708         ENDIF;
146500101228         IF  V01ttr5 <> *blanks;
146600101228           IF    %scan('?' : V01ttr5) > 0;
146700101228             ErrGenerico = *on;
146800101228             PosCurTtr5  = *on;
146900101228             clear TIBS02ds;
147000101228             T02mod = 'R';
147100101228             T02cod = 'TTR';
147200101228             T02sif = KNSIF;
147300101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
147400101228             IF  T02err = *blanks;
147500101228               V01ttr5 = t02ke1;
147600101228             ENDIF;
147700101228           ENDIF;
147800101228           IF  %lookup(V01ttr5 : $Ttr) = 0;
147900101228             ErrMessage  = *on;
148000101228             ErrGenerico = *on;
148100101228             PosCurTtr5  = *on;
148200101228             V01msg = $Msg(10);
148300101228             leavesr;
148400101228           ENDIF;
148500101228         ENDIF;
148600101228         IF  V01ttr6 <> *blanks;
148700101228           IF    %scan('?' : V01ttr6) > 0;
148800101228             ErrGenerico = *on;
148900101228             PosCurTtr6  = *on;
149000101228             clear TIBS02ds;
149100101228             T02mod = 'R';
149200101228             T02cod = 'TTR';
149300101228             T02sif = KNSIF;
149400101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
149500101228             IF  T02err = *blanks;
149600101228               V01ttr6 = t02ke1;
149700101228             ENDIF;
149800101228           ENDIF;
149900101228           IF  %lookup(V01ttr6 : $Ttr) = 0;
150000101228             ErrMessage  = *on;
150100101228             ErrGenerico = *on;
150200101228             PosCurTtr6  = *on;
150300101228             V01msg = $Msg(10);
150400101228             leavesr;
150500101228           ENDIF;
150600101228         ENDIF;
150700090318
150800100708       //?Tipo cliente
150900100708         IF  V01tic1 <> *blanks;
151000100708           IF    %scan('?' : V01tic1) > 0;
151100100708             ErrGenerico = *on;
151200100708             PosCurTic1  = *on;
151300100708             idTabella = 'IC';
151400100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
151500100708                            tastoUscita);
151600100708             V01tic1 = idElemento;
151700100708           ENDIF;
151800100708           IF  %lookup(V01tic1 : $Ic) = 0;
151900100708             ErrMessage  = *on;
152000100708             ErrGenerico = *on;
152100100708             PosCurTic1  = *on;
152200100708             V01msg = $Msg(17);
152300100708             leavesr;
152400100708           ENDIF;
152500100708         ENDIF;
152600100708         IF  V01tic2 <> *blanks;
152700100708           IF    %scan('?' : V01tic2) > 0;
152800100708             ErrGenerico = *on;
152900100708             PosCurTic2  = *on;
153000100708             idTabella = 'IC';
153100100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
153200100708                            tastoUscita);
153300100708             V01tic2 = idElemento;
153400100708           ENDIF;
153500100708           IF  %lookup(V01tic2 : $Ic) = 0;
153600100708             ErrMessage  = *on;
153700100708             ErrGenerico = *on;
153800100708             PosCurTic2  = *on;
153900100708             V01msg = $Msg(17);
154000100708             leavesr;
154100100708           ENDIF;
154200100708         ENDIF;
154300100708         IF  V01tic3 <> *blanks;
154400100708           IF    %scan('?' : V01tic3) > 0;
154500100708             ErrGenerico = *on;
154600100708             PosCurTic3  = *on;
154700100708             idTabella = 'IC';
154800100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
154900100708                            tastoUscita);
155000100708             V01tic3 = idElemento;
155100100708           ENDIF;
155200100708           IF  %lookup(V01tic3 : $Ic) = 0;
155300100708             ErrMessage  = *on;
155400100708             ErrGenerico = *on;
155500100708             PosCurTic3  = *on;
155600100708             V01msg = $Msg(17);
155700100708             leavesr;
155800100708           ENDIF;
155900100708         ENDIF;
156000100708         IF  V01tic4 <> *blanks;
156100100708           IF    %scan('?' : V01tic4) > 0;
156200100708             ErrGenerico = *on;
156300100708             PosCurTic4  = *on;
156400100708             idTabella = 'IC';
156500100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
156600100708                            tastoUscita);
156700100708             V01tic4 = idElemento;
156800100708           ENDIF;
156900100708           IF  %lookup(V01tic4 : $Ic) = 0;
157000100708             ErrMessage  = *on;
157100100708             ErrGenerico = *on;
157200100708             PosCurTic4  = *on;
157300100708             V01msg = $Msg(17);
157400100708             leavesr;
157500100708           ENDIF;
157600100708         ENDIF;
157700100708         IF  V01tic5 <> *blanks;
157800100708           IF    %scan('?' : V01tic5) > 0;
157900100708             ErrGenerico = *on;
158000100708             PosCurTic5  = *on;
158100100708             idTabella = 'IC';
158200100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
158300100708                            tastoUscita);
158400100708             V01tic5 = idElemento;
158500100708           ENDIF;
158600100708           IF  %lookup(V01tic5 : $Ic) = 0;
158700100708             ErrMessage  = *on;
158800100708             ErrGenerico = *on;
158900100708             PosCurTic5  = *on;
159000100708             V01msg = $Msg(17);
159100100708             leavesr;
159200100708           ENDIF;
159300100708         ENDIF;
159400100709
159500100709       //?Trattative calde e data presunto affidamento
159600100709       //?Possibile solo se richeiste le trattative in corso
159700100709         IF  V01calde = 'S' and V01dtica = 0;
159800100709           ErrMessage  = *on;
159900100709           ErrGenerico = *on;
160000100709           PosCurCalde = *on;
160100100709           V01msg      = $Msg(15);
160200100709           leavesr;
160300100709         ENDIF;
160400100709       //?Se inserita la data presunto affidamento devo richiedere le calde
160500100709         IF  V01calde = ' ' and V01dtaff > 0;
160600100709           ErrMessage  = *on;
160700100709           ErrGenerico = *on;
160800100709           PosCurDtaff = *on;
160900100709           V01msg      = $Msg(16);
161000100709           leavesr;
161100100709         ENDIF;
161200100709       //?Controllo la data presunto affidamento
161300100709         IF  V01dtaff <> 0;
161400100709           clear wlbdat;
161500100709           G02dat = V01dtaff;
161600100709           xsrda8(wlbdat);
161700100709           IF  G02err = '1';
161800100709             ErrMessage  = *on;
161900100709             ErrGenerico = *on;
162000100709             PosCurDtaff = *on;
162100100709             V01msg      = $Msg(04);
162200100709             leavesr;
162300100709           ENDIF;
162400100709
162500100709           V01dtaff = G02dat;
162600100709           wdtaff   = G02inv;
162700100709
162800100709         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
162900100709           IF  wdtaff < 20000101 or wdtaff > 20391231;
163000100709             ErrMessage  = *on;
163100100709             ErrGenerico = *on;
163200100709             PosCurDtaff = *on;
163300100709             V01msg      = $Msg(04);
163400100709             leavesr;
163500100709           ENDIF;
163600100709         ENDIF;
163700100708
163800100708       //?Esito
163900100708        //?Impostare solo se richieste trattative chiuse
164000100708         IF  V01esi <> *blanks and V01dtchi = 0;
164100100708           ErrMessage  = *on;
164200100708           ErrGenerico = *on;
164300100708           PosCurEsi   = *on;
164400100708           V01msg = $Msg(18);
164500100708           leavesr;
164600100708         ENDIF;
164700100326
164800100708       ENDSR;
164900100708
165000100708       //--------------------------------------------------------------
165100100708       //?Controllo videata D02
165200100708       //--------------------------------------------------------------
165300100708       BEGSR CtrD02;
165400100708
165500100708         WindDspF = IndDspF;
165600100708         reset WindDspFb;
165700100708         IndDspF  = WindDspF;
165800100524
165900100810       //?Stampa dettaglio obbligatorio S/N
166000100810         IF  V02det = *blanks;
166100100810           ErrMessage  = *on;
166200100810           ErrGenerico = *on;
166300100810           PosCurDet   = *on;
166400100810           V02msg      = $Msg(20);
166500100810           leavesr;
166600100810         ENDIF;
166700100810
166800100708       //?Almeno una selezione è da fare
166900100708         IF  V02det = 'N'  and  V02cmmt = 'N'  and  V02gent = 'N';
167000100708           ErrMessage  = *on;
167100100708           ErrGenerico = *on;
167200100708           PosCurDet   = *on;
167300100708           V02msg      = $Msg(06);
167400100708           leavesr;
167500100708         ENDIF;
167600100810
167700100810       //?Scelta dei commerciali da stampare incongruente con la prima videata
167800100810         IF  (V02detc = 'I' and V01elcmm = 'E') or
167900100810             (V02detc = 'E' and V01elcmm = 'I');
168000100810           ErrMessage  = *on;
168100100810           ErrGenerico = *on;
168200100810           PosCurDetc  = *on;
168300100810           V02msg      = $Msg(23);
168400100810           leavesr;
168500100810         ENDIF;
168600100810
168700100810       //?Immissione/Omissione trattative obbligatorio
168800100810         IF  V02ttrio = *blanks;
168900100810           ErrMessage  = *on;
169000100810           ErrGenerico = *on;
169100100810           PosCurTtrio = *on;
169200100810           V02msg = $Msg(21);
169300100810           leavesr;
169400100810         ENDIF;
169500100708
169600100708       //?Stampa dettaglio scegliere le trattative da stampare
169700100708         IF  V02det = 'S' and V02ttrio = 'I' and
169800100708             V02ttr = *blanks;
169900100708           ErrMessage  = *on;
170000100708           ErrGenerico = *on;
170100100708           PosCurTtr1  = *on;
170200100708           V02msg = $Msg(12);
170300100708           leavesr;
170400100708         ENDIF;
170500100708
170600100708       //?Tipi trattativa da Include/Omettere
170700100708         IF  V02ttr1 <> *blanks;
170800100708           IF    %scan('?' : V02ttr1) > 0;
170900100708             ErrGenerico = *on;
171000100708             PosCurTtr1  = *on;
171100100708             clear TIBS02ds;
171200100708             T02mod = 'R';
171300100708             T02cod = 'TTR';
171400100708             T02sif = KNSIF;
171500100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
171600100708             IF  T02err = *blanks;
171700100708               V02ttr1 = t02ke1;
171800100708             ENDIF;
171900100708           ENDIF;
172000100708           IF  %lookup(V02ttr1 : $Ttr) = 0;
172100100708             ErrMessage  = *on;
172200100708             ErrGenerico = *on;
172300100708             PosCurTtr1  = *on;
172400100708             V02msg = $Msg(10);
172500100708             leavesr;
172600100708           ENDIF;
172700100708         ENDIF;
172800100708         IF  V02ttr2 <> *blanks;
172900100708           IF    %scan('?' : V02ttr2) > 0;
173000100708             ErrGenerico = *on;
173100100708             PosCurTtr2  = *on;
173200100708             clear TIBS02ds;
173300100708             T02mod = 'R';
173400100708             T02cod = 'TTR';
173500100708             T02sif = KNSIF;
173600100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
173700100708             IF  T02err = *blanks;
173800100708               V02ttr2 = t02ke1;
173900100708             ENDIF;
174000100708           ENDIF;
174100100708           IF  %lookup(V02ttr2 : $Ttr) = 0;
174200100708             ErrMessage  = *on;
174300100708             ErrGenerico = *on;
174400100708             PosCurTtr2  = *on;
174500100708             V02msg = $Msg(10);
174600100708             leavesr;
174700100708           ENDIF;
174800100708         ENDIF;
174900100708         IF  V02ttr3 <> *blanks;
175000100708           IF    %scan('?' : V02ttr3) > 0;
175100100708             ErrGenerico = *on;
175200100708             PosCurTtr3  = *on;
175300100708             clear TIBS02ds;
175400100708             T02mod = 'R';
175500100708             T02cod = 'TTR';
175600100708             T02sif = KNSIF;
175700100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
175800100708             IF  T02err = *blanks;
175900100708               V02ttr3 = t02ke1;
176000100708             ENDIF;
176100100708           ENDIF;
176200100708           IF  %lookup(V02ttr3 : $Ttr) = 0;
176300100708             ErrMessage  = *on;
176400100708             ErrGenerico = *on;
176500100708             PosCurTtr3  = *on;
176600100708             V02msg = $Msg(10);
176700100708             leavesr;
176800100708           ENDIF;
176900100708         ENDIF;
177000100708         IF  V02ttr4 <> *blanks;
177100100708           IF    %scan('?' : V02ttr4) > 0;
177200100708             ErrGenerico = *on;
177300100708             PosCurTtr4  = *on;
177400100708             clear TIBS02ds;
177500100708             T02mod = 'R';
177600100708             T02cod = 'TTR';
177700100708             T02sif = KNSIF;
177800100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
177900100708             IF  T02err = *blanks;
178000100708               V02ttr4 = t02ke1;
178100100708             ENDIF;
178200100708           ENDIF;
178300100708           IF  %lookup(V02ttr4 : $Ttr) = 0;
178400100708             ErrMessage  = *on;
178500100708             ErrGenerico = *on;
178600100708             PosCurTtr4  = *on;
178700100708             V02msg = $Msg(10);
178800100708             leavesr;
178900100708           ENDIF;
179000100708         ENDIF;
179100101228         IF  V02ttr5 <> *blanks;
179200101228           IF    %scan('?' : V02ttr5) > 0;
179300101228             ErrGenerico = *on;
179400101228             PosCurTtr5  = *on;
179500101228             clear TIBS02ds;
179600101228             T02mod = 'R';
179700101228             T02cod = 'TTR';
179800101228             T02sif = KNSIF;
179900101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
180000101228             IF  T02err = *blanks;
180100101228               V02ttr5 = t02ke1;
180200101228             ENDIF;
180300101228           ENDIF;
180400101228           IF  %lookup(V02ttr5 : $Ttr) = 0;
180500101228             ErrMessage  = *on;
180600101228             ErrGenerico = *on;
180700101228             PosCurTtr5  = *on;
180800101228             V02msg = $Msg(10);
180900101228             leavesr;
181000101228           ENDIF;
181100101228         ENDIF;
181200101228         IF  V02ttr6 <> *blanks;
181300101228           IF    %scan('?' : V02ttr6) > 0;
181400101228             ErrGenerico = *on;
181500101228             PosCurTtr6  = *on;
181600101228             clear TIBS02ds;
181700101228             T02mod = 'R';
181800101228             T02cod = 'TTR';
181900101228             T02sif = KNSIF;
182000101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
182100101228             IF  T02err = *blanks;
182200101228               V02ttr6 = t02ke1;
182300101228             ENDIF;
182400101228           ENDIF;
182500101228           IF  %lookup(V02ttr6 : $Ttr) = 0;
182600101228             ErrMessage  = *on;
182700101228             ErrGenerico = *on;
182800101228             PosCurTtr6  = *on;
182900101228             V02msg = $Msg(10);
183000101228             leavesr;
183100101228           ENDIF;
183200101228         ENDIF;
183300100810
183400100810       //?Ordinamento obbligatorio
183500100810         IF  V02ord = *blanks;
183600100810           ErrMessage  = *on;
183700100810           ErrGenerico = *on;
183800100810           PosCurOrd   = *on;
183900100810           V02msg = $Msg(22);
184000100810           leavesr;
184100100810         ENDIF;
184200100524
184300100528       //?Stampa generale INUTILE se richiesto 1 solo commerciale
184400100708         IF  V02gent = 'S'  and  V01cmm > *zero;
184500100601           ErrMessage  = *on;
184600100601           ErrGenerico = *on;
184700100708           PosCurGent  = *on;
184800100708           V02msg = $Msg(08);
184900100524           leavesr;
185000100524         ENDIF;
185100100601
185200100708       //?Stampa generale OBBLIGATORIA se richiesti tutti i commerciali
185300100708         IF  V02gent = 'N'  and  V01cmm <= *zero;
185400100601           ErrMessage  = *on;
185500100601           ErrGenerico = *on;
185600100708           PosCurGent  = *on;
185700100708           V02msg = $Msg(11);
185800100601           leavesr;
185900100601         ENDIF;
186000090717
186100080206       ENDSR;
186200080206
186300080206       //--------------------------------------------------------------
186400080206       //?Operazioni finali.
186500080206       //--------------------------------------------------------------
186600080206       BEGSR RoutEnd;
186700080206
186800080206         *inLR = *on;
186900080206         return;
187000080206
187100080206       ENDSR;
187200080206
187300080206      /end-free
187400100329
187500080206       //--------------------------------------------------------------
187600080206       //?Schiere a tempo di compilazione.
187700080206       //--------------------------------------------------------------
187800080206
187900080206** - $MSG -------------------------------------------------------------------*
188000100329Utente non abilitato alla funzione                                            01
188100100329Codice commerciale errato o non in gestione all'utente                        02
188200100329Richiedere solo commerciali UNIFICANTI!                                       03
188300100329Data errata                                                                   04
188400100329Data "DAL" incongruente con data "AL"                                         05
188500100524Selezionare almeno una stampa                                                 06
188600100601Selezionare i commerciali da stampare                                         07
188700100528Non richiedere il totale generale per la stampa di un solo comm.unificante    08
188800100706Concorrente errato                                                            09
188900100531Tipo trattativa errato                                                        10
189000100601Totali generali obbligatori SE NON specificato un commerciale unificante      11
189100100601Specificare cosa includere (oppure omettere nulla per includere tutto)        12
189200100708Immettere il Delta previsto                                                   13
189300101228Delta previsto "DAL" incongruente con Delta previsto "AL"                     14
189400100708Richedere le trattativa CALDE solo se richieste le trattative IN CORSO        15
189500100708Se non richieste la trattative CALDE non immettere la data                    16
189600100708Tipo cliente errato                                                           17
189700100708Richiedere ESITO solo se richieste le trattative CHIUSE                       18
189800100708Delta Previsto errato                                                         19
189900100810Impostare "S" oppure "N"                                                      20
190000100810Impostare "I" oppure "O"                                                      21
190100100810Impostare Ordinamento                                                         22
190200100810Commerciali da stampare incogruenti con i commerciali da elaborare in 1° vid. 23
190300101006Codice commerciale errato.                                                    24
190400101228Immettere il Valore traffico TOTALE                                           25
190500101228Valore traffico TOTALE "DAL" incongruente con Valore traffico TOTALE "AL"     26
190600101228Immettere il Aumento/Sconto                                                   27
190700101228Aumento/Sconto "DAL" incongruente con Aumento/Sconto "AL"                     28
190800110119Codice Area errato o non in gestione all'utente                               29
190900110119Non impostare il commerciale se inserito il codice area                       30
191000110921Selezionare almeno un'area o un commerciale                                   31
