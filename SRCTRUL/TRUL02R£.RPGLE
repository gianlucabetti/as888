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
020800090401
020900080206      // - Campi di comodo
021000100708     d C_tab           s                   like(TBEcod)
021100100708
021200101228     d wausci          s                   like(V01ausci) inz
021300101228     d wauscf          s                   like(V01auscf) inz
021400100708     d wdeli           s              3  0 inz
021500100708     d wdelf           s              3  0 inz
021600100706     d wdtaff          s              8  0 inz
021700100706     d wdtavf          s              8  0 inz
021800100706     d wdtavi          s              8  0 inz
021900100706     d wdtchf          s              8  0 inz
022000100706     d wdtchi          s              8  0 inz
022100100706     d wdtica          s              8  0 inz
022200100714     d wgiorni         s              5  0 inz
022300101228     d wvaltti         s                   like(V01valtti) inz
022400101228     d wvalttf         s                   like(V01valttf) inz
022500100708
022600100326     d tb85Kfil        s              3
022700100326     d tb85Key         s              8
022800100326     d tb85Des1        s             25
022900100326     d tb85Uni         s              1    inz('S')
023000100708
023100100708     d idTabella       s              2
023200100708     d Ordinamento     s              1
023300100708     d idElemento      s              8
023400100708     d TastoUscita     s              1
023500100708
023600080208
023700090508      //---------------------------------------------------------------
023800090508      //?Definizione procedure usate.
023900090508      //---------------------------------------------------------------
024000100329
024100100329      // - Sottomissione lavoro batch
024200100329     d bch10           pr                  extpgm('BCH10')
024300100329     d  kpjba                              likeds(KPJBA)
024400100329
024500100329      // - Richiamo diretto lavoro batch
024600100524     d tnta98c         pr                  extpgm('TNTA98C')
024700100329     d  kpjba                              likeds(KPJBA)
024800110119
024900110119      // - Filiali e aree abilitate
025000110119     d trul31r         pr                  extpgm('TRUL31R')
025100110119     d  kpjba                              likeds(KPJBA)
025200110119     d  trul31ds                           likeds(trul31ds)
025300110119     d  trul31ds2                          likeds(trul31ds2)
025400080206
025500080626      //---------------------------------------------------------------
025600080626      //?prototipi
025700080626      //---------------------------------------------------------------
025800100326
025900080626      /copy gaitrasrc/srcprotopr,tibs02r
026000080626      /copy gaitrasrc/srcprotopr,tibs34r
026100100708      /copy gaitrasrc/srcprotopr,trul19r
026200100211      /copy gaitrasrc/srcprotopr,tntaa1c
026300090406      /copy gaitrasrc/srcprotopr,xsrda8
026400090318      /copy gaitrasrc/srcprotopr,trtb85r
026500100326
026600080206      //---------------------------------------------------------------
026700080206      //?Definizione key-list.
026800080206      //---------------------------------------------------------------
026900080206
027000100326       // - File TABEL00F
027100100326     d k03tabel      e ds                  extname(TABEL00F:*key)
027200100326     d                                     prefix(k_)
027300080206
027400080206      //---------------------------------------------------------------
027500080206      //?Riepilogo indicatori.
027600080206      //---------------------------------------------------------------
027700080207      // 28    : Emissione messaggio di errore a video
027800100708      // 50-76 : Errori Video
027900080207      // 99    : Generico di Errore
028000080206      //---------------------------------------------------------------
028100080206
028200080206      //---------------------------------------------------------------
028300080206      //?M A I N - L I N E
028400080206      //---------------------------------------------------------------
028500080206
028600080206     c     *Entry        plist
028700080206     c                   parm                    KPJBA
028800080206
028900080206      /free
029000080206
029100100326       //?Operazioni iniziali
029200080206       exsr RoutInz;
029300090601
029400100326       //?Gestione video
029500080206       DOW $Fine = *off;
029600100329         SELECT;
029700100329           WHEN $Video = 'D1';
029800080206             exsr GesD01;
029900100706           WHEN $Video = 'D2';
030000100706             exsr GesD02;
030100100329           OTHER;
030200080206             $Fine = *on;
030300100329         ENDSL;
030400080206       ENDDO;
030500080206
030600100326       //?Operazioni finali
030700080206       exsr RoutEnd;
030800080206
030900080206       //--------------------------------------------------------------
031000080206       //?Operazioni iniziali.
031100080206       //--------------------------------------------------------------
031200080206       BEGSR RoutInz;
031300080206
031400100326       //?Impostazione campi "fissi"
031500100326         k_TBLkut = 1;
031600080206
031700100326       //?Reperimento dati job
031800080206         exsr DatiJob;
031900100211
032000100326       //?Controllo se utente autorizzato alla funzione
032100100211         reset TNTAA1DS;
032200100211         ITAA1tipo = 'N';
032300100211         ITAA1caut = '§utegtc';
032400100211         kpjbu     = TNTAA1DS;
032500100211         tntaa1c (kpjba);
032600100211         TNTAA1DS = kpjbu;
032700100211
032800100211         IF  OTAA1fabi = 'N';
032900080206           $ErrGrave   = *on;
033000080206           errMessage  = *on;
033100080206           errGenerico = *on;
033200100329           V01msg = $Msg(01);
033300080206           leavesr;
033400080206         endif;
033500100312
033600100312         clear TRUL31DS;
033700110119         clear TRUL31DS2;
033800100312         I31abi = OTAA1cabi;
033900100312         I31cdi = DUTdis;
034000100312         I31car = DUTare;
034100100312         I31cpo = DUTpou;
034200110119         trul31r (kpjba : trul31ds : trul31ds2);
034300100312         IF  O31pog <= *zeros;
034400100312           $ErrGrave   = *on;
034500100326           ErrMessage  = *on;
034600100326           ErrGenerico = *on;
034700100326           V01msg = $Msg(01);
034800100312           leavesr;
034900100312         ENDIF;
035000110119
035100110119         //?Abilito richiesta per Area
035200110119         DspStpCar = *off;
035300110119         IF  car(2) > *zeros;
035400110119           DspStpCar = *on;
035500110119         ENDIF;
035600100708
035700100708         //?Carico sk Importanza Clienti
035800100708         clear xx;
035900100708         k_TBLcod = 'IC';
036000100708         setll (k_TBLkut:k_TBLcod) TABEL00F;
036100100708         reade (k_TBLkut:k_TBLcod) TABEL00F;
036200100708
036300100708         DOW not %eof(TABEL00F);
036400100708           IF  TBLflg = *blanks;
036500100708             xx += 1;
036600100708             $Ic(xx) = %subst(TBLkey:1:1);
036700100708           ENDIF;
036800100708           reade (k_TBLkut:k_TBLcod) TABEL00F;
036900100708         ENDDO;
037000100706
037100100706         //?Carico sk Concorrenti
037200100706         clear xx;
037300100706         C_Tab = 'ITS';
037400100708         setll C_tab TNTBE01L;
037500100708         reade C_tab TNTBE01L;
037600100706
037700100706         DOW not %eof(TNTBE01L);
037800100706           IF  TBEatb = *blanks;
037900100706             xx += 1;
038000100706             $Its(xx) = %subst(TBEke2:1:4);
038100100706           ENDIF;
038200100708           reade C_tab TNTBE01L;
038300100706         ENDDO;
038400100531
038500100531         //?Carico sk Tipi trattavia
038600100531         clear xx;
038700100706         C_Tab = 'TTR';
038800100708         setll C_tab TNTBE01L;
038900100708         reade C_tab TNTBE01L;
039000100531
039100100531         DOW not %eof(TNTBE01L);
039200100531           IF  TBEatb = *blanks;
039300100531             xx += 1;
039400100531             $Ttr(xx) = %subst(TBEke1:1:1);
039500100531           ENDIF;
039600100708           reade C_tab TNTBE01L;
039700100531         ENDDO;
039800090714
039900080206       ENDSR;
040000080206
040100080206       //--------------------------------------------------------------
040200080206       //?Reperimento Dati del job (Utente/Operativi).
040300080206       //--------------------------------------------------------------
040400080206       BEGSR DatiJob;
040500080206
040600080206         in(E) §AzUte;
040700080206         if NOT %error;
040800080206           in(E) §DatiUte;
040900080206         endif;
041000080206         if %error or RSut = *blanks;
041100080206           clear TIBS34ds;
041200080206           tibs34r(tibs34ds);
041300080206           in §AzUte;
041400080206           in §DatiUte;
041500080206         endif;
041600080206
041700080206       ENDSR;
041800080206
041900080206       //--------------------------------------------------------------
042000080206       //?Gestione videata D01
042100080206       //--------------------------------------------------------------
042200080206       BEGSR GesD01;
042300080206
042400100326       //?Inizializzazione videata
042500100326         IF  $InzD01;
042600080206           exsr InzD01;
042700080206           $InzD01 = *off;
042800100326         ENDIF;
042900080206
043000100326       //?Emissione videata
043100100524         exfmt TA89D01;
043200100326         ErrMessage   = *off;
043300100326         ErrGenerico  = *off;
043400100326         clear V01msg;
043500080207
043600100326         SELECT;
043700100326
043800100326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
043900100326           WHEN  $ErrGrave;
044000100326             $Fine = *on;
044100100326
044200100326       //?F3=Fine
044300100326           WHEN  dsp_aid = c_F03;
044400080206             exsr F03D01;
044500100329
044600100329       //?F6=Conferma
044700100329           WHEN  dsp_aid = c_F06;
044800100329             exsr CtrD01;
044900100329             IF  ErrGenerico;
045000100329               leavesr;
045100100329             ENDIF;
045200100329             exsr F06D01;
045300100329
045400100326       //?Enter
045500100326           OTHER;
045600080206             exsr CtrD01;
045700100326             IF  ErrGenerico;
045800080206               leavesr;
045900100326             ENDIF;
046000100326         ENDSL;
046100080206
046200080206       ENDSR;
046300080206
046400080206       //--------------------------------------------------------------
046500080206       //?Inizializzazione videata D01
046600080206       //--------------------------------------------------------------
046700080206       BEGSR InzD01;
046800080206
046900100708         clear TA89D01;
047000100329         V01pgm = SDSpgm;
047100100329         V01sut = rsut;
047200100329         V01sif = knsif;
047300100329         V01mus = knmus;
047400101228
047500101228         V01valttsi = '+';
047600101228         V01valttsf = '+';
047700100708
047800100708         V01delsi = '+';
047900100708         V01delsf = '+';
048000101228
048100101228         V01auscsi = '+';
048200101228         V01auscsf = '+';
048300110119
048400110119       //?Posiziono il cursore sul commerciale se NO richiesta per area
048500110119         IF  DspStpCar;
048600110119           PosCurCmm = *on;
048700110119         ENDIF;
048800100708
048900080206       ENDSR;
049000080206
049100080206       //--------------------------------------------------------------
049200080206       //?Gestione tasto funzionale F3 da videata D01
049300080206       //?F3=Fine
049400080206       //--------------------------------------------------------------
049500080206       BEGSR F03D01;
049600080206
049700100329       //?Chiusura del programma
049800080206         $Fine = *on;
049900080206
050000080206       ENDSR;
050100100329
050200100329       //--------------------------------------------------------------
050300100329       //?Gestione tasto funzionale F6 da videata D01
050400100329       //?F6=Conferma
050500100329       //--------------------------------------------------------------
050600100329       BEGSR F06D01;
050700100706
050800100708       //?Videata sucessiva
050900100706         $Video = 'D2';
051000100706         $InzD02 = *on;
051100100708
051200100708       ENDSR;
051300100708
051400100708       //--------------------------------------------------------------
051500100708       //?Gestione videata D02
051600100708       //--------------------------------------------------------------
051700100708       BEGSR GesD02;
051800100708
051900100708       //?Inizializzazione videata
052000100708         IF  $InzD02;
052100100708           exsr InzD02;
052200100708           $InzD02 = *off;
052300100708         ENDIF;
052400100708
052500100708       //?Emissione videata
052600100708         exfmt TA89D02;
052700100708         ErrMessage   = *off;
052800100708         ErrGenerico  = *off;
052900100708         clear V02msg;
053000100708
053100100708         SELECT;
053200100708
053300100708       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
053400100708           WHEN  $ErrGrave;
053500100708             $Fine = *on;
053600100708
053700100708       //?F3=Fine
053800100708           WHEN  dsp_aid = c_F03;
053900100708             exsr F03D01;
054000100708
054100100708       //?F6=Conferma
054200100708           WHEN  dsp_aid = c_F06;
054300100708             exsr CtrD02;
054400100708             IF  ErrGenerico;
054500100708               leavesr;
054600100708             ENDIF;
054700100708             exsr F06D02;
054800100708
054900100708       //?F12=Ritorno
055000100708           WHEN  dsp_aid = c_F12;
055100100708             exsr F12D02;
055200100708
055300100708       //?Enter
055400100708           OTHER;
055500100708             exsr CtrD02;
055600100708             IF  ErrGenerico;
055700100708               leavesr;
055800100708             ENDIF;
055900100708         ENDSL;
056000100708
056100100708       ENDSR;
056200100708
056300100708       //--------------------------------------------------------------
056400100708       //?Inizializzazione videata D02
056500100708       //--------------------------------------------------------------
056600100708       BEGSR InzD02;
056700100708
056800100708         clear TA89D02;
056900100708         V02pgm = SDSpgm;
057000100708         V02sut = rsut;
057100100708         V02sif = knsif;
057200100708         V02mus = knmus;
057300100708
057400100915         V02ttrio = 'O';
057500100708         V02ord   = 'D';
057600100716         V02cmmt  = 'S';
057700100716         IF  V01cmm = *zeros or V01cmm = *blanks;
057800100716           V02gent = 'S';
057900100716         ELSE;
058000100716           V02gent = 'N';
058100100716         ENDIF;
058200100708
058300100708       ENDSR;
058400100708
058500100708       //--------------------------------------------------------------
058600100708       //?Gestione tasto funzionale F6 da videata D02
058700100708       //?F6=Conferma
058800100708       //--------------------------------------------------------------
058900100708       BEGSR F06D02;
059000100708
059100100708         clear TNTA98ds;
059200110119         IF  V01car > *zeros;
059300110119           ITA98car   = %dec(V01car:3:0);
059400110119         ENDIF;
059500100708         IF  V01cmm > *zeros;
059600100708           ITA98cmm   = %dec(V01cmm:7:0);
059700100708         ENDIF;
059800100708         ITA98elcmm = V01elcmm;
059900100708         ITA98dtavi = wdtavi;
060000100708         ITA98dtavf = wdtavf;
060100100708         ITA98dtchi = wdtchi;
060200100708         ITA98dtchf = wdtchf;
060300100708         ITA98dtica = wdtica;
060400110114       //?Imposto il valore traffico totale con il segno immesso a video
060500110114         IF  V01valttsi = '-';
060600110114           ITA98vltti = V01valtti * -1;
060700110114         ELSE;
060800110114           ITA98vltti = V01valtti;
060900110114         ENDIF;
061000110114         IF  V01valttsf = '-';
061100110114           ITA98vlttf = V01valttf * -1;
061200110114         ELSE;
061300110114           ITA98vlttf = V01valttf;
061400110114         ENDIF;
061500100708         ITA98valte = V01valte;
061600100708         ITA98pkg   = V01pkg;
061700100708         ITA98conc1 = V01conc1;
061800100708         ITA98conc2 = V01conc2;
061900100708         ITA98conc3 = V01conc3;
062000100708         ITA98conc4 = V01conc4;
062100100708         ITA98conc5 = V01conc5;
062200100708         ITA98delsi = V01delsi;
062300100714         ITA98deli  = V01deli;
062400100708         ITA98delsf = V01delsf;
062500100714         ITA98delf  = V01delf;
062600110114       //?Imposto il valore aumento/sconto con il segno immesso a video
062700110114         IF  V01auscsi = '-';
062800110114           ITA98ausi = V01ausci * -1;
062900110114         ELSE;
063000110114           ITA98ausi = V01ausci;
063100110114         ENDIF;
063200110114         IF  V01auscsf = '-';
063300110114           ITA98ausf = V01auscf * -1;
063400110114         ELSE;
063500110114           ITA98ausf = V01auscf;
063600110114         ENDIF;
063700100708         ITA98tpt1  = V01ttr1;
063800100708         ITA98tpt2  = V01ttr2;
063900100708         ITA98tpt3  = V01ttr3;
064000100708         ITA98tpt4  = V01ttr4;
064100101228         ITA98tpt5  = V01ttr5;
064200101228         ITA98tpt6  = V01ttr6;
064300100708         ITA98tic1  = V01tic1;
064400100708         ITA98tic2  = V01tic2;
064500100708         ITA98tic3  = V01tic3;
064600100708         ITA98tic4  = V01tic4;
064700100708         ITA98tic5  = V01tic5;
064800100709         ITA98calde = V01calde;
064900100729         ITA98dtaff = wdtaff;
065000100708         ITA98esi   = V01esi;
065100100708
065200100708         ITA98det   = V02det;
065300100708         ITA98detc  = V02detc;
065400100708         ITA98ttrio = V02ttrio;
065500100708         ITA98ttr1  = V02ttr1;
065600100708         ITA98ttr2  = V02ttr2;
065700100708         ITA98ttr3  = V02ttr3;
065800100708         ITA98ttr4  = V02ttr4;
065900101228         ITA98ttr5  = V02ttr5;
066000101228         ITA98ttr6  = V02ttr6;
066100100708         ITA98ord   = V02ord;
066200100708         ITA98cmmt  = V02cmmt;
066300100708         ITA98gent  = V02gent;
066400100708         kcoaz = 'TA98';
066500100708         kpjbu = TNTA98ds;
066600100708         kbuff = *blanks;
066700100708
066800100708       //?Richiamo interattivo
066900100714         IF  knmus = *all'1';
067000100708           TNTA98C(kpjba);
067100100708       //?Richiamo batch
067200100714         ELSE;
067300100714           BCH10(kpjba);
067400100714         ENDIF;
067500100708
067600100708       //?Chiusura del programma
067700100708         $Fine = *on;
067800100708
067900100708       ENDSR;
068000100708
068100100708       //--------------------------------------------------------------
068200100708       //?Gestione tasto funzionale F12 da videata D02
068300100708       //?F12=Ritorno
068400100708       //--------------------------------------------------------------
068500100708       BEGSR F12D02;
068600100708
068700100708       //?Ritorno alla videata precedente
068800100708         $InzD01 = *off;
068900100708         $Video = 'D1';
069000100708
069100100708       ENDSR;
069200090717
069300080206       //--------------------------------------------------------------
069400080206       //?Controllo videata D01
069500080206       //--------------------------------------------------------------
069600080206       BEGSR CtrD01;
069700110119
069800110119         clear v01dcar;
069900110119         clear v01dcmm;
070000100326
070100100706         clear wdtaff;
070200100706         clear wdtavf;
070300100706         clear wdtavi;
070400100706         clear wdtchf;
070500100706         clear wdtchi;
070600100706         clear wdtica;
070700100708
070800101228         clear wausci;
070900101228         clear wauscf;
071000100708         clear wdeli;
071100100708         clear wdelf;
071200101228         clear wvaltti;
071300101228         clear wvalttf;
071400090427
071500100326         WindDspF = IndDspF;
071600100326         reset WindDspFb;
071700100326         IndDspF  = WindDspF;
071800110119
071900110119       //?AREA
072000110119       //?Ricerca
072100110119         IF  %scan('?' : V01car) > *zero;
072200110119           idTabella = '05';
072300110119           clear Ordinamento;
072400110119           clear idElemento;
072500110119           clear TastoUscita;
072600110119           Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
072700110119           V01car = idElemento;
072800110119         ENDIF;
072900110119
073000110119       //?Controllo se impostato
073100110119         IF  V01car <> *blanks and V01car <> *zeros;
073200110119        //?Solo numerico
073300110119           IF  %check(digits:V01car) > 0;
073400110119             ErrMessage  = *on;
073500110119             ErrGenerico = *on;
073600110119             PosCurCar   = *on;
073700110119             V01msg      = $Msg(29);
073800110119             leavesr;
073900110119           ENDIF;
074000110119         //?Valido in tabella
074100110119           clear ds05;
074200110119           k_TBLcod = '05';
074300110119           k_TBLkey = V01car;
074400110119           chain  %kds(K03tabel) TABEL00F;
074500110119           IF  NOT %found(TABEL00F)
074600110119               or  TBLflg <> *blank;
074700110119             ErrMessage  = *on;
074800110119             ErrGenerico = *on;
074900110119             PosCurCar   = *on;
075000110119             V01msg      = $Msg(29);
075100110119             leavesr;
075200110119           ENDIF;
075300110119
075400110119           ds05 = TBLuni;
075500110119           V01dcar = §05des;
075600110119
075700110119         //?Utente abilitato all'area
075800110119           IF  %lookup(V01car : car) = 0;
075900110119             ErrMessage  = *on;
076000110119             ErrGenerico = *on;
076100110119             PosCurCar   = *on;
076200110119             V01msg      = $Msg(29);
076300110119             leavesr;
076400110119           ENDIF;
076500110119         ENDIF;
076600100326
076700100706       //?COMMERCIALE
076800110921
076900110921       //?Se sono in sede e non seleziono ne l'area e ne il commerciale errore
077000110921         IF   dutpou = 046 and ((V01car = *blanks or V01car = *zeros) and
077100110921           (V01cmm = *blanks or V01cmm = *zeros));
077200110921           ErrMessage  = *on;
077300110921           ErrGenerico = *on;
077400110921           PosCurCar   = *on;
077500110921           V01msg      = $Msg(31);
077600110921           leavesr;
077700110921         ENDIF;
077800110921
077900110119       //?Non immettere se richiesta l'area
078000110119         IF  V01car <> *blanks and V01car > *zeros and
078100110119             V01cmm <> *blanks and V01cmm > *zeros;
078200110119           ErrMessage  = *on;
078300110119           ErrGenerico = *on;
078400110119           PosCurCmm   = *on;
078500110119           V01msg      = $Msg(30);
078600110119           leavesr;
078700110119         ENDIF;
078800110119
078900100706       //?Ricerca
079000100326         IF  %scan('?' : V01cmm) > *zero;
079100100326           ErrGenerico = *on;
079200100326           PosCurCmm   = *on;
079300100428           tb85Kfil = %editc(DUTpou:'X');
079400100326           clear tb85Key ;
079500100326           clear tb85Des1;
079600100329           trtb85r (kpjba : tb85kfil : tb85key : tb85des1 : tb85Uni);
079700100326           V01cmm  = tb85Key;
079800100706           V01dcmm = tb85des1;
079900100326           leavesr;
080000100326         ENDIF;
080100100326
080200100706       //?Controllo se impostato
080300100330         IF  V01cmm <> *blanks and V01cmm <> *zeros;
080400100706        //?Solo numerico
080500100326           IF  %check(digits:V01cmm) > 0;
080600100326             ErrMessage  = *on;
080700100326             ErrGenerico = *on;
080800100326             PosCurCmm   = *on;
080900100329             V01msg      = $Msg(02);
081000100326             leavesr;
081100100326           ENDIF;
081200100706         //?Valido in tabella
081300100326           clear ds01;
081400100708           k_TBLcod = '01';
081500100326           k_TBLkey = V01cmm;
081600100326           chain  %kds(K03tabel) TABEL00F;
081700100326           IF  NOT %found(TABEL00F)
081800100326               or  TBLflg <> *blank;
081900100326             ErrMessage  = *on;
082000100326             ErrGenerico = *on;
082100100329             PosCurCmm   = *on;
082200100329             V01msg      = $Msg(02);
082300100326             leavesr;
082400100326           ENDIF;
082500100326
082600100326           ds01 = TBLuni;
082700100706           V01dcmm = §01age;
082800100326
082900100706         //?No vari o clienti inattivi
083000101228           IF  §01par <> *blanks;
083100100326             ErrMessage  = *on;
083200100326             ErrGenerico = *on;
083300100326             PosCurCmm   = *on;
083400100329             V01msg      = $Msg(02);
083500100326             leavesr;
083600100326           ENDIF;
083700100706         //?Utente abilitato al commerciale
083800100326           clear TNTAA1DS;
083900100326           ITAA1tipo = 'N';
084000100326           ITAA1caut = '§utegtc';
084100100326           ITAA1cmm  = %dec(V01cmm:7:0);
084200100326           kpjbu     = tntaa1ds;
084300100326           tntaa1c (kpjba);
084400100326           TNTAA1DS = kpjbu;
084500100326           IF  OTAA1fabi = 'N';
084600100326             ErrMessage  = *on;
084700100326             ErrGenerico = *on;
084800100326             PosCurCmm   = *on;
084900100329             V01msg      = $Msg(02);
085000100326             leavesr;
085100100326           ENDIF;
085200100706         //?Solo unificante
085300100524           IF  %dec(V01cmm:7:0) <> §01rgf;
085400100524             ErrMessage  = *on;
085500100524             ErrGenerico = *on;
085600100524             PosCurCmm   = *on;
085700100524             V01msg      = $Msg(03);
085800100524             leavesr;
085900100524           ENDIF;
086000100524         ENDIF;
086100100709
086200100709       //?Tipo commerciale INTERNO ESTERNO TUTTI
086300100709       //?Se impostato un solo codice commerciale la selezione
086400100709       //?deve essere congruente con il codice
086500110119         IF  (V01car > *zeros or V01cmm > *zeros) and
086600110119             V01elcmm <> *blanks;
086700101006           IF  V01elcmm = 'I' and §01fun <> 'COMIN'
086800110119               and §01fun <> 'ASC' and §01fun <> 'SA'
086900110119               and V01cmm > *zeros;
087000100709           ErrMessage  = *on;
087100100709           ErrGenerico = *on;
087200100709           PosCurCmm   = *on;
087300101006           V01msg      = $Msg(24);
087400101006           V01msg      = %trim(V01msg) + ' Richiesta selezione INTERNO';
087500100709           leavesr;
087600100709           ENDIF;
087700101006           IF  V01elcmm = 'E' and (§01fun = 'COMIN'
087800110119               or §01fun = 'ASC' or §01fun = 'SA')
087900110119               and V01cmm > *zeros;
088000100709           ErrMessage  = *on;
088100100709           ErrGenerico = *on;
088200100709           PosCurCmm   = *on;
088300101006           V01msg      = $Msg(24);
088400101006           V01msg      = %trim(V01msg) + ' Richiesta selezione ESTERNO';
088500100709           leavesr;
088600100709           ENDIF;
088700100709         ENDIF;
088800100706
088900100706       //?DATE
089000100706       //?Almeno una delle tre date è da immettere
089100100706         IF  V01dtavi = 0 and V01dtavf = 0 and
089200100706             V01dtchi = 0 and V01dtchf = 0 and
089300100706             V01dtica = 0;
089400100708           ErrMessage  = *on;
089500100708           ErrGenerico = *on;
089600100706           PosCurDtavi = *on;
089700100706           V01msg      = $Msg(04);
089800100706           V01msg      = %trim(V01msg) + '. Richiedere almeno un periodo';
089900100706           leavesr;
090000100706         ENDIF;
090100100706
090200100708       //?Trattative avviate
090300100708         IF  V01dtavi <> 0 or V01dtavf <> 0;
090400100708         //?Dal
090500100706           IF  V01dtavi <> 0;
090600100706             clear wlbdat;
090700100706             G02dat = V01dtavi;
090800100706             xsrda8(wlbdat);
090900100706             IF  G02err = '1';
091000100706               ErrMessage  = *on;
091100100706               ErrGenerico = *on;
091200100706               PosCurDtavi = *on;
091300100706               V01msg      = $Msg(04);
091400100706               leavesr;
091500100706             ENDIF;
091600100706
091700100706             V01dtavi = G02dat;
091800100706             wdtavi   = G02inv;
091900100708
092000100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
092100100706             IF  wdtavi < 20000101 or wdtavi > 20391231;
092200100706               ErrMessage  = *on;
092300100706               ErrGenerico = *on;
092400100706               PosCurDtavi = *on;
092500100706               V01msg      = $Msg(04);
092600100706               leavesr;
092700100706             ENDIF;
092800100706           ENDIF;
092900100708         //?Al
093000100706           IF  V01dtavf <> 0;
093100100706             clear wlbdat;
093200100706             G02dat = V01dtavf;
093300100706             xsrda8(wlbdat);
093400100706             IF  G02err = '1';
093500100706               ErrMessage  = *on;
093600100706               ErrGenerico = *on;
093700100706               PosCurDtavf = *on;
093800100706               V01msg      = $Msg(04);
093900100706               leavesr;
094000100706             ENDIF;
094100100706
094200100706             V01dtavf = G02dat;
094300100706             wdtavf   = G02inv;
094400100706
094500100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
094600100706             IF  wdtavf < 20000101 or wdtavf > 20391231;
094700100706               ErrMessage  = *on;
094800100706               ErrGenerico = *on;
094900100706               PosCurDtavf = *on;
095000100706               V01msg      = $Msg(04);
095100100706               leavesr;
095200100706             ENDIF;
095300100706           ENDIF;
095400100708         //?Se impostata data "dal" e non "al"
095500100706         //?imposto "al" = data del giorno
095600100706           IF  V01dtavi > 0 and V01dtavf = 0;
095700100706             wdtavf    = %dec(%date());
095800100706             data_eur  = %date(wdtavf:*iso);
095900100706             V01dtavf  = %dec(data_eur);
096000100706           ENDIF;
096100100708         //?"DAL" congruente con "AL"
096200100706           IF  wdtavi > 0  and  wdtavf = 0;
096300100706             ErrMessage  = *on;
096400100706             ErrGenerico = *on;
096500100706             PosCurDtavi = *on;
096600100706             V01msg      = $Msg(05);
096700100706             leavesr;
096800100706           ENDIF;
096900100706           IF  wdtavi > 0  and  wdtavf > 0  and  wdtavi > wdtavf;
097000100706             ErrMessage  = *on;
097100100706             ErrGenerico = *on;
097200100706             PosCurDtavf = *on;
097300100706             V01msg      = $Msg(05);
097400100706             leavesr;
097500100706           ENDIF;
097600100908         //?Range massimo di 65 giorni
097700100908           data_iso  = %date(wdtavi);
097800100908           data_iso1 = %date(wdtavf);
097900100908           wgiorni = %diff( data_iso1 : data_iso : *days );
098000100908           IF  wgiorni > 65;
098100100908             ErrMessage  = *on;
098200100908             ErrGenerico = *on;
098300100908             PosCurDtavi = *on;
098400100908             V01msg      = $Msg(04);
098500100908             V01msg      = %trim(V01msg) + '. Range richiesto > di 65 gg.';
098600100908             leavesr;
098700100908           ENDIF;
098800100706         ENDIF;
098900100706
099000100708       //?Trattative chiuse
099100100706         IF  V01dtchi <> 0 or V01dtchi <> 0;
099200100708         //?Dal
099300100706           IF  V01dtchi <> 0;
099400100706             clear wlbdat;
099500100706             G02dat = V01dtchi;
099600100706             xsrda8(wlbdat);
099700100706             IF  G02err = '1';
099800100706               ErrMessage  = *on;
099900100706               ErrGenerico = *on;
100000100706               PosCurDtchi = *on;
100100100706               V01msg      = $Msg(04);
100200100706               leavesr;
100300100706             ENDIF;
100400100706
100500100706             V01dtchi = G02dat;
100600100706             wdtchi   = G02inv;
100700100706
100800100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
100900100706             IF  wdtchi < 20000101 or wdtchi > 20391231;
101000100706               ErrMessage  = *on;
101100100706               ErrGenerico = *on;
101200100706               PosCurDtchi = *on;
101300100706               V01msg      = $Msg(04);
101400100706               leavesr;
101500100706             ENDIF;
101600100706           ENDIF;
101700100708         //?Al
101800100706           IF  V01dtchf <> 0;
101900100706             clear wlbdat;
102000100706             G02dat = V01dtchf;
102100100706             xsrda8(wlbdat);
102200100706             IF  G02err = '1';
102300100706               ErrMessage  = *on;
102400100706               ErrGenerico = *on;
102500100706               PosCurDtchf = *on;
102600100706               V01msg      = $Msg(04);
102700100706               leavesr;
102800100706             ENDIF;
102900100706
103000100706             V01dtchf = G02dat;
103100100706             wdtchf   = G02inv;
103200100706
103300100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
103400100706             IF  wdtchf < 20000101 or wdtchf > 20391231;
103500100706               ErrMessage  = *on;
103600100706               ErrGenerico = *on;
103700100706               PosCurDtchf = *on;
103800100706               V01msg      = $Msg(04);
103900100706               leavesr;
104000100706             ENDIF;
104100100706           ENDIF;
104200100708         //?Se impostata data "dal" e non "al"
104300100706         //?imposto "al" = data del giorno
104400100706           IF  V01dtchi > 0 and V01dtchf = 0;
104500100706             wdtchf    = %dec(%date());
104600100706             data_eur  = %date(wdtchf:*iso);
104700100706             V01dtchf  = %dec(data_eur);
104800100706           ENDIF;
104900100708         //?"DAL" congruente con "AL"
105000100706           IF  wdtchi > 0  and  wdtchf = 0;
105100100706             ErrMessage  = *on;
105200100706             ErrGenerico = *on;
105300100706             PosCurDtchi = *on;
105400100706             V01msg      = $Msg(05);
105500100706             leavesr;
105600100706           ENDIF;
105700100706           IF  wdtchi > 0  and  wdtchf > 0  and  wdtchi > wdtchf;
105800100706             ErrMessage  = *on;
105900100706             ErrGenerico = *on;
106000100706             PosCurDtchf = *on;
106100100706             V01msg      = $Msg(05);
106200100706             leavesr;
106300100706           ENDIF;
106400100908         //?Range massimo di 65 giorni
106500100908           data_iso  = %date(wdtchi);
106600100908           data_iso1 = %date(wdtchf);
106700100908           wgiorni = %diff( data_iso1 : data_iso : *days );
106800100908           IF  wgiorni > 65;
106900100908             ErrMessage  = *on;
107000100908             ErrGenerico = *on;
107100100908             PosCurDtchi = *on;
107200100908             V01msg      = $Msg(04);
107300100908             V01msg      = %trim(V01msg) + '. Range richiesto > di 65 gg.';
107400100908             leavesr;
107500100908           ENDIF;
107600100706         ENDIF;
107700100706
107800100708       //?Trattative in corso
107900100706         IF  V01dtica <> 0;
108000100706           clear wlbdat;
108100100706           G02dat = V01dtica;
108200100706           xsrda8(wlbdat);
108300100706           IF  G02err = '1';
108400100706             ErrMessage  = *on;
108500100706             ErrGenerico = *on;
108600100706             PosCurDtica = *on;
108700100706             V01msg      = $Msg(04);
108800100706             leavesr;
108900100706           ENDIF;
109000100706
109100100706           V01dtica = G02dat;
109200100706           wdtica   = G02inv;
109300100706
109400100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
109500100706           IF  wdtica < 20000101 or wdtica > 20391231;
109600100706             ErrMessage  = *on;
109700100706             ErrGenerico = *on;
109800100706             PosCurDtica = *on;
109900100706             V01msg      = $Msg(04);
110000100706             leavesr;
110100100706           ENDIF;
110200100909         //?Massimo 65 giorni indietro rispetto a oggi
110300100706           data_iso  = %date(wdtica);
110400100708           data_iso1 = %date();
110500100706           wgiorni = %diff( data_iso1 : data_iso : *days );
110600100909           IF  wgiorni > 65;
110700100706             ErrMessage  = *on;
110800100706             ErrGenerico = *on;
110900100706             PosCurDtica = *on;
111000100706             V01msg      = $Msg(04);
111100100909             V01msg      = %trim(V01msg) + '. Più vecchia di 65 gg. da oggi';
111200100706             leavesr;
111300100706           ENDIF;
111400100706         ENDIF;
111500100706
111600100706       //?PARAMETRI DI SELEZIONE
111700101228         //?Valore traffico totale
111800101228         IF  V01valtti <> 0 or V01valttf <> 0;
111900101228         //?Obbligatori tutti e 2 se immesso uno dei due
112000101228           IF  V01valtti <> 0  and  V01valttf = 0;
112100101228             ErrMessage  = *on;
112200101228             ErrGenerico = *on;
112300101228             PosCurVlttf = *on;
112400101228             V01msg      = $Msg(25);
112500101228             leavesr;
112600101228           ENDIF;
112700101228           IF  V01valtti = 0  and  V01valttf <> 0;
112800101228             ErrMessage  = *on;
112900101228             ErrGenerico = *on;
113000101228             PosCurVltti = *on;
113100101228             V01msg      = $Msg(25);
113200101228             leavesr;
113300101228           ENDIF;
113400101228         //?Controllo congruenza tra "DAL" e "AL"
113500101228           wvaltti = V01valtti;
113600101228           IF  V01valttsi = '-';
113700101228             wvaltti = wvaltti * -1;
113800101228           ENDIF;
113900101228           wvalttf = V01valttf;
114000101228           IF  V01valttsf = '-';
114100101228             wvalttf = wvalttf * -1;
114200101228           ENDIF;
114300101228           IF  wvaltti > wvalttf;
114400101228             ErrMessage  = *on;
114500101228             ErrGenerico = *on;
114600101228             PosCurVltti = *on;
114700101228             V01msg      = $Msg(26);
114800101228             leavesr;
114900101228           ENDIF;
115000101228         ENDIF;
115100101228
115200101228         //?Concorrente 1
115300100708         IF  V01conc1 <> *blanks;
115400100708           IF    %scan('?' : V01conc1) > 0;
115500100708             ErrGenerico = *on;
115600100708             PosCurConc1  = *on;
115700100708             clear TIBS02ds;
115800100708             T02mod = 'R';
115900100708             T02cod = 'ITS';
116000100708             T02sif = KNSIF;
116100100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
116200100708             IF  T02err = *blanks;
116300100708               V01conc1 = t02ke2;
116400100708             ENDIF;
116500100708           ENDIF;
116600100708           IF  %lookup(V01conc1 : $Its) = 0;
116700100708             ErrMessage  = *on;
116800100708             ErrGenerico = *on;
116900100708             PosCurConc1 = *on;
117000100708             V01msg = $Msg(09);
117100100708             leavesr;
117200100708           ENDIF;
117300100708         ENDIF;
117400100708         //?Concorrente 2
117500100708         IF  V01conc2 <> *blanks;
117600100708           IF    %scan('?' : V01conc2) > 0;
117700100708             ErrGenerico = *on;
117800100708             PosCurConc2  = *on;
117900100708             clear TIBS02ds;
118000100708             T02mod = 'R';
118100100708             T02cod = 'ITS';
118200100708             T02sif = KNSIF;
118300100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
118400100708             IF  T02err = *blanks;
118500100708               V01conc2 = t02ke2;
118600100708             ENDIF;
118700100708           ENDIF;
118800100708           IF  %lookup(V01conc2 : $Its) = 0;
118900100708             ErrMessage  = *on;
119000100708             ErrGenerico = *on;
119100100708             PosCurConc2 = *on;
119200100708             V01msg = $Msg(09);
119300100708             leavesr;
119400100708           ENDIF;
119500100708         ENDIF;
119600100708         //?Concorrente 3
119700100708         IF  V01conc3 <> *blanks;
119800100708           IF    %scan('?' : V01conc3) > 0;
119900100708             ErrGenerico = *on;
120000100708             PosCurConc3  = *on;
120100100708             clear TIBS02ds;
120200100708             T02mod = 'R';
120300100708             T02cod = 'ITS';
120400100708             T02sif = KNSIF;
120500100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
120600100708             IF  T02err = *blanks;
120700100708               V01conc3 = t02ke2;
120800100708             ENDIF;
120900100708           ENDIF;
121000100708           IF  %lookup(V01conc3 : $Its) = 0;
121100100708             ErrMessage  = *on;
121200100708             ErrGenerico = *on;
121300100708             PosCurConc3 = *on;
121400100708             V01msg = $Msg(09);
121500100708             leavesr;
121600100708           ENDIF;
121700100708         ENDIF;
121800100708         //?Concorrente 4
121900100708         IF  V01conc4 <> *blanks;
122000100708           IF    %scan('?' : V01conc4) > 0;
122100100708             ErrGenerico = *on;
122200100708             PosCurConc4  = *on;
122300100708             clear TIBS02ds;
122400100708             T02mod = 'R';
122500100708             T02cod = 'ITS';
122600100708             T02sif = KNSIF;
122700100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
122800100708             IF  T02err = *blanks;
122900100708               V01conc4 = t02ke2;
123000100708             ENDIF;
123100100708           ENDIF;
123200100708           IF  %lookup(V01conc4 : $Its) = 0;
123300100708             ErrMessage  = *on;
123400100708             ErrGenerico = *on;
123500100708             PosCurConc4 = *on;
123600100708             V01msg = $Msg(09);
123700100708             leavesr;
123800100708           ENDIF;
123900100708         ENDIF;
124000100708         //?Concorrente 5
124100100708         IF  V01conc5 <> *blanks;
124200100708           IF    %scan('?' : V01conc5) > 0;
124300100708             ErrGenerico = *on;
124400100708             PosCurConc5  = *on;
124500100708             clear TIBS02ds;
124600100708             T02mod = 'R';
124700100708             T02cod = 'ITS';
124800100708             T02sif = KNSIF;
124900100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
125000100708             IF  T02err = *blanks;
125100100708               V01conc5 = t02ke2;
125200100708             ENDIF;
125300100708           ENDIF;
125400100708           IF  %lookup(V01conc5 : $Its) = 0;
125500100708             ErrMessage  = *on;
125600100708             ErrGenerico = *on;
125700100708             PosCurConc5 = *on;
125800100708             V01msg = $Msg(09);
125900100708             leavesr;
126000100708           ENDIF;
126100100708         ENDIF;
126200100706
126300100708       //?Delta
126400100708         IF  V01deli <> *blanks or V01delf <> *blanks;
126500100708         //?Solo valori numerici
126600100708           IF  V01deli <> *blanks;
126700100708             IF  %check(digits:V01deli) > 0;
126800100708               ErrMessage  = *on;
126900100708               ErrGenerico = *on;
127000100708               PosCurDeli  = *on;
127100100708               V01msg      = $Msg(19);
127200100708               leavesr;
127300100708             ENDIF;
127400100708           ENDIF;
127500100708           IF  V01delf <> *blanks;
127600100708             IF  %check(digits:V01delf) > 0;
127700100708               ErrMessage  = *on;
127800100708               ErrGenerico = *on;
127900100708               PosCurDelf  = *on;
128000100708               V01msg      = $Msg(19);
128100100708               leavesr;
128200100708             ENDIF;
128300100708           ENDIF;
128400100708         //?Obbligatori tutti e 2 se immesso uno dei due
128500100708           IF  V01deli <> *blanks  and  V01delf = *blanks;
128600100708             ErrMessage  = *on;
128700100708             ErrGenerico = *on;
128800100708             PosCurDelf  = *on;
128900100708             V01msg      = $Msg(13);
129000100708             leavesr;
129100100708           ENDIF;
129200100708           IF  V01deli = *blanks  and  V01delf <> *blanks;
129300100708             ErrMessage  = *on;
129400100708             ErrGenerico = *on;
129500100708             PosCurDeli  = *on;
129600100708             V01msg      = $Msg(13);
129700100708             leavesr;
129800100708           ENDIF;
129900100708         //?Controllo congruenza tra "DAL" e "AL"
130000100708           wdeli = %dec(V01deli:3:0);
130100100708           IF  V01delsi = '-';
130200100708             wdeli = wdeli * -1;
130300100708           ENDIF;
130400100708           wdelf = %dec(V01delf:3:0);
130500100708           IF  V01delsf = '-';
130600100708             wdelf = wdelf * -1;
130700100708           ENDIF;
130800100708           IF  wdeli > wdelf;
130900100708             ErrMessage  = *on;
131000100708             ErrGenerico = *on;
131100100708             PosCurDeli  = *on;
131200100708             V01msg      = $Msg(14);
131300100708             leavesr;
131400100708           ENDIF;
131500100708         ENDIF;
131600101228
131700101228       //?Aumento/Sconto
131800101228         IF  V01ausci <> 0 or V01auscf <> 0;
131900101228         //?Obbligatori tutti e 2 se immesso uno dei due
132000101228           IF  V01ausci <> 0  and  V01auscf = 0;
132100101228             ErrMessage  = *on;
132200101228             ErrGenerico = *on;
132300101228             PosCurAuScf = *on;
132400101228             V01msg      = $Msg(27);
132500101228             leavesr;
132600101228           ENDIF;
132700101228           IF  V01ausci = 0  and  V01auscf <> 0;
132800101228             ErrMessage  = *on;
132900101228             ErrGenerico = *on;
133000101228             PosCurAuSci = *on;
133100101228             V01msg      = $Msg(27);
133200101228             leavesr;
133300101228           ENDIF;
133400101228         //?Controllo congruenza tra "DAL" e "AL"
133500101228           wausci = V01ausci;
133600101228           IF  V01auscsi = '-';
133700101228             wausci = wausci * -1;
133800101228           ENDIF;
133900101228           wauscf = V01auscf;
134000101228           IF  V01auscsf = '-';
134100101228             wauscf = wauscf * -1;
134200101228           ENDIF;
134300101228           IF  wausci > wauscf;
134400101228             ErrMessage  = *on;
134500101228             ErrGenerico = *on;
134600101228             PosCurAuSci = *on;
134700101228             V01msg      = $Msg(28);
134800101228             leavesr;
134900101228           ENDIF;
135000101228         ENDIF;
135100100708
135200100708       //?Tipo trattativa
135300100708         IF  V01ttr1 <> *blanks;
135400100708           IF    %scan('?' : V01ttr1) > 0;
135500100708             ErrGenerico = *on;
135600100708             PosCurTtr1  = *on;
135700100708             clear TIBS02ds;
135800100708             T02mod = 'R';
135900100708             T02cod = 'TTR';
136000100708             T02sif = KNSIF;
136100100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
136200100708             IF  T02err = *blanks;
136300100708               V01ttr1 = t02ke1;
136400100708             ENDIF;
136500100708           ENDIF;
136600100708           IF  %lookup(V01ttr1 : $Ttr) = 0;
136700100708             ErrMessage  = *on;
136800100708             ErrGenerico = *on;
136900100708             PosCurTtr1  = *on;
137000100708             V01msg = $Msg(10);
137100100708             leavesr;
137200100708           ENDIF;
137300100708         ENDIF;
137400100708         IF  V01ttr2 <> *blanks;
137500100708           IF    %scan('?' : V01ttr2) > 0;
137600100708             ErrGenerico = *on;
137700100708             PosCurTtr2  = *on;
137800100708             clear TIBS02ds;
137900100708             T02mod = 'R';
138000100708             T02cod = 'TTR';
138100100708             T02sif = KNSIF;
138200100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
138300100708             IF  T02err = *blanks;
138400100708               V01ttr2 = t02ke1;
138500100708             ENDIF;
138600100708           ENDIF;
138700100708           IF  %lookup(V01ttr2 : $Ttr) = 0;
138800100708             ErrMessage  = *on;
138900100708             ErrGenerico = *on;
139000100708             PosCurTtr2  = *on;
139100100708             V01msg = $Msg(10);
139200100708             leavesr;
139300100708           ENDIF;
139400100708         ENDIF;
139500100708         IF  V01ttr3 <> *blanks;
139600100708           IF    %scan('?' : V01ttr3) > 0;
139700100708             ErrGenerico = *on;
139800100708             PosCurTtr3  = *on;
139900100708             clear TIBS02ds;
140000100708             T02mod = 'R';
140100100708             T02cod = 'TTR';
140200100708             T02sif = KNSIF;
140300100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
140400100708             IF  T02err = *blanks;
140500100708               V01ttr3 = t02ke1;
140600100708             ENDIF;
140700100708           ENDIF;
140800100708           IF  %lookup(V01ttr3 : $Ttr) = 0;
140900100708             ErrMessage  = *on;
141000100708             ErrGenerico = *on;
141100100708             PosCurTtr3  = *on;
141200100708             V01msg = $Msg(10);
141300100708             leavesr;
141400100708           ENDIF;
141500100708         ENDIF;
141600100708         IF  V01ttr4 <> *blanks;
141700100708           IF    %scan('?' : V01ttr4) > 0;
141800100708             ErrGenerico = *on;
141900100708             PosCurTtr4  = *on;
142000100708             clear TIBS02ds;
142100100708             T02mod = 'R';
142200100708             T02cod = 'TTR';
142300100708             T02sif = KNSIF;
142400100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
142500100708             IF  T02err = *blanks;
142600100708               V01ttr4 = t02ke1;
142700100708             ENDIF;
142800100708           ENDIF;
142900100708           IF  %lookup(V01ttr4 : $Ttr) = 0;
143000100708             ErrMessage  = *on;
143100100708             ErrGenerico = *on;
143200100708             PosCurTtr4  = *on;
143300100708             V01msg = $Msg(10);
143400100708             leavesr;
143500100708           ENDIF;
143600100708         ENDIF;
143700101228         IF  V01ttr5 <> *blanks;
143800101228           IF    %scan('?' : V01ttr5) > 0;
143900101228             ErrGenerico = *on;
144000101228             PosCurTtr5  = *on;
144100101228             clear TIBS02ds;
144200101228             T02mod = 'R';
144300101228             T02cod = 'TTR';
144400101228             T02sif = KNSIF;
144500101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
144600101228             IF  T02err = *blanks;
144700101228               V01ttr5 = t02ke1;
144800101228             ENDIF;
144900101228           ENDIF;
145000101228           IF  %lookup(V01ttr5 : $Ttr) = 0;
145100101228             ErrMessage  = *on;
145200101228             ErrGenerico = *on;
145300101228             PosCurTtr5  = *on;
145400101228             V01msg = $Msg(10);
145500101228             leavesr;
145600101228           ENDIF;
145700101228         ENDIF;
145800101228         IF  V01ttr6 <> *blanks;
145900101228           IF    %scan('?' : V01ttr6) > 0;
146000101228             ErrGenerico = *on;
146100101228             PosCurTtr6  = *on;
146200101228             clear TIBS02ds;
146300101228             T02mod = 'R';
146400101228             T02cod = 'TTR';
146500101228             T02sif = KNSIF;
146600101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
146700101228             IF  T02err = *blanks;
146800101228               V01ttr6 = t02ke1;
146900101228             ENDIF;
147000101228           ENDIF;
147100101228           IF  %lookup(V01ttr6 : $Ttr) = 0;
147200101228             ErrMessage  = *on;
147300101228             ErrGenerico = *on;
147400101228             PosCurTtr6  = *on;
147500101228             V01msg = $Msg(10);
147600101228             leavesr;
147700101228           ENDIF;
147800101228         ENDIF;
147900090318
148000100708       //?Tipo cliente
148100100708         IF  V01tic1 <> *blanks;
148200100708           IF    %scan('?' : V01tic1) > 0;
148300100708             ErrGenerico = *on;
148400100708             PosCurTic1  = *on;
148500100708             idTabella = 'IC';
148600100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
148700100708                            tastoUscita);
148800100708             V01tic1 = idElemento;
148900100708           ENDIF;
149000100708           IF  %lookup(V01tic1 : $Ic) = 0;
149100100708             ErrMessage  = *on;
149200100708             ErrGenerico = *on;
149300100708             PosCurTic1  = *on;
149400100708             V01msg = $Msg(17);
149500100708             leavesr;
149600100708           ENDIF;
149700100708         ENDIF;
149800100708         IF  V01tic2 <> *blanks;
149900100708           IF    %scan('?' : V01tic2) > 0;
150000100708             ErrGenerico = *on;
150100100708             PosCurTic2  = *on;
150200100708             idTabella = 'IC';
150300100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
150400100708                            tastoUscita);
150500100708             V01tic2 = idElemento;
150600100708           ENDIF;
150700100708           IF  %lookup(V01tic2 : $Ic) = 0;
150800100708             ErrMessage  = *on;
150900100708             ErrGenerico = *on;
151000100708             PosCurTic2  = *on;
151100100708             V01msg = $Msg(17);
151200100708             leavesr;
151300100708           ENDIF;
151400100708         ENDIF;
151500100708         IF  V01tic3 <> *blanks;
151600100708           IF    %scan('?' : V01tic3) > 0;
151700100708             ErrGenerico = *on;
151800100708             PosCurTic3  = *on;
151900100708             idTabella = 'IC';
152000100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
152100100708                            tastoUscita);
152200100708             V01tic3 = idElemento;
152300100708           ENDIF;
152400100708           IF  %lookup(V01tic3 : $Ic) = 0;
152500100708             ErrMessage  = *on;
152600100708             ErrGenerico = *on;
152700100708             PosCurTic3  = *on;
152800100708             V01msg = $Msg(17);
152900100708             leavesr;
153000100708           ENDIF;
153100100708         ENDIF;
153200100708         IF  V01tic4 <> *blanks;
153300100708           IF    %scan('?' : V01tic4) > 0;
153400100708             ErrGenerico = *on;
153500100708             PosCurTic4  = *on;
153600100708             idTabella = 'IC';
153700100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
153800100708                            tastoUscita);
153900100708             V01tic4 = idElemento;
154000100708           ENDIF;
154100100708           IF  %lookup(V01tic4 : $Ic) = 0;
154200100708             ErrMessage  = *on;
154300100708             ErrGenerico = *on;
154400100708             PosCurTic4  = *on;
154500100708             V01msg = $Msg(17);
154600100708             leavesr;
154700100708           ENDIF;
154800100708         ENDIF;
154900100708         IF  V01tic5 <> *blanks;
155000100708           IF    %scan('?' : V01tic5) > 0;
155100100708             ErrGenerico = *on;
155200100708             PosCurTic5  = *on;
155300100708             idTabella = 'IC';
155400100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
155500100708                            tastoUscita);
155600100708             V01tic5 = idElemento;
155700100708           ENDIF;
155800100708           IF  %lookup(V01tic5 : $Ic) = 0;
155900100708             ErrMessage  = *on;
156000100708             ErrGenerico = *on;
156100100708             PosCurTic5  = *on;
156200100708             V01msg = $Msg(17);
156300100708             leavesr;
156400100708           ENDIF;
156500100708         ENDIF;
156600100709
156700100709       //?Trattative calde e data presunto affidamento
156800100709       //?Possibile solo se richeiste le trattative in corso
156900100709         IF  V01calde = 'S' and V01dtica = 0;
157000100709           ErrMessage  = *on;
157100100709           ErrGenerico = *on;
157200100709           PosCurCalde = *on;
157300100709           V01msg      = $Msg(15);
157400100709           leavesr;
157500100709         ENDIF;
157600100709       //?Se inserita la data presunto affidamento devo richiedere le calde
157700100709         IF  V01calde = ' ' and V01dtaff > 0;
157800100709           ErrMessage  = *on;
157900100709           ErrGenerico = *on;
158000100709           PosCurDtaff = *on;
158100100709           V01msg      = $Msg(16);
158200100709           leavesr;
158300100709         ENDIF;
158400100709       //?Controllo la data presunto affidamento
158500100709         IF  V01dtaff <> 0;
158600100709           clear wlbdat;
158700100709           G02dat = V01dtaff;
158800100709           xsrda8(wlbdat);
158900100709           IF  G02err = '1';
159000100709             ErrMessage  = *on;
159100100709             ErrGenerico = *on;
159200100709             PosCurDtaff = *on;
159300100709             V01msg      = $Msg(04);
159400100709             leavesr;
159500100709           ENDIF;
159600100709
159700100709           V01dtaff = G02dat;
159800100709           wdtaff   = G02inv;
159900100709
160000100709         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
160100100709           IF  wdtaff < 20000101 or wdtaff > 20391231;
160200100709             ErrMessage  = *on;
160300100709             ErrGenerico = *on;
160400100709             PosCurDtaff = *on;
160500100709             V01msg      = $Msg(04);
160600100709             leavesr;
160700100709           ENDIF;
160800100709         ENDIF;
160900100708
161000100708       //?Esito
161100100708        //?Impostare solo se richieste trattative chiuse
161200100708         IF  V01esi <> *blanks and V01dtchi = 0;
161300100708           ErrMessage  = *on;
161400100708           ErrGenerico = *on;
161500100708           PosCurEsi   = *on;
161600100708           V01msg = $Msg(18);
161700100708           leavesr;
161800100708         ENDIF;
161900100326
162000100708       ENDSR;
162100100708
162200100708       //--------------------------------------------------------------
162300100708       //?Controllo videata D02
162400100708       //--------------------------------------------------------------
162500100708       BEGSR CtrD02;
162600100708
162700100708         WindDspF = IndDspF;
162800100708         reset WindDspFb;
162900100708         IndDspF  = WindDspF;
163000100524
163100100810       //?Stampa dettaglio obbligatorio S/N
163200100810         IF  V02det = *blanks;
163300100810           ErrMessage  = *on;
163400100810           ErrGenerico = *on;
163500100810           PosCurDet   = *on;
163600100810           V02msg      = $Msg(20);
163700100810           leavesr;
163800100810         ENDIF;
163900100810
164000100708       //?Almeno una selezione è da fare
164100100708         IF  V02det = 'N'  and  V02cmmt = 'N'  and  V02gent = 'N';
164200100708           ErrMessage  = *on;
164300100708           ErrGenerico = *on;
164400100708           PosCurDet   = *on;
164500100708           V02msg      = $Msg(06);
164600100708           leavesr;
164700100708         ENDIF;
164800100810
164900100810       //?Scelta dei commerciali da stampare incongruente con la prima videata
165000100810         IF  (V02detc = 'I' and V01elcmm = 'E') or
165100100810             (V02detc = 'E' and V01elcmm = 'I');
165200100810           ErrMessage  = *on;
165300100810           ErrGenerico = *on;
165400100810           PosCurDetc  = *on;
165500100810           V02msg      = $Msg(23);
165600100810           leavesr;
165700100810         ENDIF;
165800100810
165900100810       //?Immissione/Omissione trattative obbligatorio
166000100810         IF  V02ttrio = *blanks;
166100100810           ErrMessage  = *on;
166200100810           ErrGenerico = *on;
166300100810           PosCurTtrio = *on;
166400100810           V02msg = $Msg(21);
166500100810           leavesr;
166600100810         ENDIF;
166700100708
166800100708       //?Stampa dettaglio scegliere le trattative da stampare
166900100708         IF  V02det = 'S' and V02ttrio = 'I' and
167000100708             V02ttr = *blanks;
167100100708           ErrMessage  = *on;
167200100708           ErrGenerico = *on;
167300100708           PosCurTtr1  = *on;
167400100708           V02msg = $Msg(12);
167500100708           leavesr;
167600100708         ENDIF;
167700100708
167800100708       //?Tipi trattativa da Include/Omettere
167900100708         IF  V02ttr1 <> *blanks;
168000100708           IF    %scan('?' : V02ttr1) > 0;
168100100708             ErrGenerico = *on;
168200100708             PosCurTtr1  = *on;
168300100708             clear TIBS02ds;
168400100708             T02mod = 'R';
168500100708             T02cod = 'TTR';
168600100708             T02sif = KNSIF;
168700100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
168800100708             IF  T02err = *blanks;
168900100708               V02ttr1 = t02ke1;
169000100708             ENDIF;
169100100708           ENDIF;
169200100708           IF  %lookup(V02ttr1 : $Ttr) = 0;
169300100708             ErrMessage  = *on;
169400100708             ErrGenerico = *on;
169500100708             PosCurTtr1  = *on;
169600100708             V02msg = $Msg(10);
169700100708             leavesr;
169800100708           ENDIF;
169900100708         ENDIF;
170000100708         IF  V02ttr2 <> *blanks;
170100100708           IF    %scan('?' : V02ttr2) > 0;
170200100708             ErrGenerico = *on;
170300100708             PosCurTtr2  = *on;
170400100708             clear TIBS02ds;
170500100708             T02mod = 'R';
170600100708             T02cod = 'TTR';
170700100708             T02sif = KNSIF;
170800100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
170900100708             IF  T02err = *blanks;
171000100708               V02ttr2 = t02ke1;
171100100708             ENDIF;
171200100708           ENDIF;
171300100708           IF  %lookup(V02ttr2 : $Ttr) = 0;
171400100708             ErrMessage  = *on;
171500100708             ErrGenerico = *on;
171600100708             PosCurTtr2  = *on;
171700100708             V02msg = $Msg(10);
171800100708             leavesr;
171900100708           ENDIF;
172000100708         ENDIF;
172100100708         IF  V02ttr3 <> *blanks;
172200100708           IF    %scan('?' : V02ttr3) > 0;
172300100708             ErrGenerico = *on;
172400100708             PosCurTtr3  = *on;
172500100708             clear TIBS02ds;
172600100708             T02mod = 'R';
172700100708             T02cod = 'TTR';
172800100708             T02sif = KNSIF;
172900100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
173000100708             IF  T02err = *blanks;
173100100708               V02ttr3 = t02ke1;
173200100708             ENDIF;
173300100708           ENDIF;
173400100708           IF  %lookup(V02ttr3 : $Ttr) = 0;
173500100708             ErrMessage  = *on;
173600100708             ErrGenerico = *on;
173700100708             PosCurTtr3  = *on;
173800100708             V02msg = $Msg(10);
173900100708             leavesr;
174000100708           ENDIF;
174100100708         ENDIF;
174200100708         IF  V02ttr4 <> *blanks;
174300100708           IF    %scan('?' : V02ttr4) > 0;
174400100708             ErrGenerico = *on;
174500100708             PosCurTtr4  = *on;
174600100708             clear TIBS02ds;
174700100708             T02mod = 'R';
174800100708             T02cod = 'TTR';
174900100708             T02sif = KNSIF;
175000100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
175100100708             IF  T02err = *blanks;
175200100708               V02ttr4 = t02ke1;
175300100708             ENDIF;
175400100708           ENDIF;
175500100708           IF  %lookup(V02ttr4 : $Ttr) = 0;
175600100708             ErrMessage  = *on;
175700100708             ErrGenerico = *on;
175800100708             PosCurTtr4  = *on;
175900100708             V02msg = $Msg(10);
176000100708             leavesr;
176100100708           ENDIF;
176200100708         ENDIF;
176300101228         IF  V02ttr5 <> *blanks;
176400101228           IF    %scan('?' : V02ttr5) > 0;
176500101228             ErrGenerico = *on;
176600101228             PosCurTtr5  = *on;
176700101228             clear TIBS02ds;
176800101228             T02mod = 'R';
176900101228             T02cod = 'TTR';
177000101228             T02sif = KNSIF;
177100101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
177200101228             IF  T02err = *blanks;
177300101228               V02ttr5 = t02ke1;
177400101228             ENDIF;
177500101228           ENDIF;
177600101228           IF  %lookup(V02ttr5 : $Ttr) = 0;
177700101228             ErrMessage  = *on;
177800101228             ErrGenerico = *on;
177900101228             PosCurTtr5  = *on;
178000101228             V02msg = $Msg(10);
178100101228             leavesr;
178200101228           ENDIF;
178300101228         ENDIF;
178400101228         IF  V02ttr6 <> *blanks;
178500101228           IF    %scan('?' : V02ttr6) > 0;
178600101228             ErrGenerico = *on;
178700101228             PosCurTtr6  = *on;
178800101228             clear TIBS02ds;
178900101228             T02mod = 'R';
179000101228             T02cod = 'TTR';
179100101228             T02sif = KNSIF;
179200101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
179300101228             IF  T02err = *blanks;
179400101228               V02ttr6 = t02ke1;
179500101228             ENDIF;
179600101228           ENDIF;
179700101228           IF  %lookup(V02ttr6 : $Ttr) = 0;
179800101228             ErrMessage  = *on;
179900101228             ErrGenerico = *on;
180000101228             PosCurTtr6  = *on;
180100101228             V02msg = $Msg(10);
180200101228             leavesr;
180300101228           ENDIF;
180400101228         ENDIF;
180500100810
180600100810       //?Ordinamento obbligatorio
180700100810         IF  V02ord = *blanks;
180800100810           ErrMessage  = *on;
180900100810           ErrGenerico = *on;
181000100810           PosCurOrd   = *on;
181100100810           V02msg = $Msg(22);
181200100810           leavesr;
181300100810         ENDIF;
181400100524
181500100528       //?Stampa generale INUTILE se richiesto 1 solo commerciale
181600100708         IF  V02gent = 'S'  and  V01cmm > *zero;
181700100601           ErrMessage  = *on;
181800100601           ErrGenerico = *on;
181900100708           PosCurGent  = *on;
182000100708           V02msg = $Msg(08);
182100100524           leavesr;
182200100524         ENDIF;
182300100601
182400100708       //?Stampa generale OBBLIGATORIA se richiesti tutti i commerciali
182500100708         IF  V02gent = 'N'  and  V01cmm <= *zero;
182600100601           ErrMessage  = *on;
182700100601           ErrGenerico = *on;
182800100708           PosCurGent  = *on;
182900100708           V02msg = $Msg(11);
183000100601           leavesr;
183100100601         ENDIF;
183200090717
183300080206       ENDSR;
183400080206
183500080206       //--------------------------------------------------------------
183600080206       //?Operazioni finali.
183700080206       //--------------------------------------------------------------
183800080206       BEGSR RoutEnd;
183900080206
184000080206         *inLR = *on;
184100080206         return;
184200080206
184300080206       ENDSR;
184400080206
184500080206      /end-free
184600100329
184700080206       //--------------------------------------------------------------
184800080206       //?Schiere a tempo di compilazione.
184900080206       //--------------------------------------------------------------
185000080206
185100080206** - $MSG -------------------------------------------------------------------*
185200100329Utente non abilitato alla funzione                                            01
185300100329Codice commerciale errato o non in gestione all'utente                        02
185400100329Richiedere solo commerciali UNIFICANTI!                                       03
185500100329Data errata                                                                   04
185600100329Data "DAL" incongruente con data "AL"                                         05
185700100524Selezionare almeno una stampa                                                 06
185800100601Selezionare i commerciali da stampare                                         07
185900100528Non richiedere il totale generale per la stampa di un solo comm.unificante    08
186000100706Concorrente errato                                                            09
186100100531Tipo trattativa errato                                                        10
186200100601Totali generali obbligatori SE NON specificato un commerciale unificante      11
186300100601Specificare cosa includere (oppure omettere nulla per includere tutto)        12
186400100708Immettere il Delta previsto                                                   13
186500101228Delta previsto "DAL" incongruente con Delta previsto "AL"                     14
186600100708Richedere le trattativa CALDE solo se richieste le trattative IN CORSO        15
186700100708Se non richieste la trattative CALDE non immettere la data                    16
186800100708Tipo cliente errato                                                           17
186900100708Richiedere ESITO solo se richieste le trattative CHIUSE                       18
187000100708Delta Previsto errato                                                         19
187100100810Impostare "S" oppure "N"                                                      20
187200100810Impostare "I" oppure "O"                                                      21
187300100810Impostare Ordinamento                                                         22
187400100810Commerciali da stampare incogruenti con i commerciali da elaborare in 1° vid. 23
187500101006Codice commerciale errato.                                                    24
187600101228Immettere il Valore traffico TOTALE                                           25
187700101228Valore traffico TOTALE "DAL" incongruente con Valore traffico TOTALE "AL"     26
187800101228Immettere il Aumento/Sconto                                                   27
187900101228Aumento/Sconto "DAL" incongruente con Aumento/Sconto "AL"                     28
188000110119Codice Area errato o non in gestione all'utente                               29
188100110119Non impostare il commerciale se inserito il codice area                       30
188200110921Selezionare almeno un'area o un commerciale                                   31
