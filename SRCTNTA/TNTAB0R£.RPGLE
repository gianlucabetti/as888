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
001400130807      // -?Anagrafica Commerciali?
001500130807     fAZCMM01L  if   e           k disk
001600090720
001700090716      // - Video Gestione trattative commerciali
001800100524     fTNTA89D   cf   e             workstn
001900080208     f                                     indds(IndDspF)
002000080206     f                                     infds(InfDspF)
002100080206
002200080206      //---------------------------------------------------------------
002300090406      //?Definizione costanti.
002400080206      //---------------------------------------------------------------
002500080207
002600080207      // - Tasti funzionali a video
002700080207     d c_F01           c                   const(x'31')
002800080207     d c_F02           c                   const(x'32')
002900080207     d c_F03           c                   const(x'33')
003000090406     d c_F04           c                   const(x'34')
003100080207     d c_F05           c                   const(x'35')
003200080207     d c_F06           c                   const(x'36')
003300080207     d c_F07           c                   const(x'37')
003400080207     d c_F08           c                   const(x'38')
003500080207     d c_F09           c                   const(x'39')
003600080207     d c_F10           c                   const(x'3A')
003700090303     d c_F11           c                   const(x'3B')
003800080207     d c_F12           c                   const(x'3C')
003900080207     d c_F13           c                   const(x'B1')
004000080207     d c_F14           c                   const(x'B2')
004100080207     d c_F15           c                   const(x'B3')
004200080207     d c_F16           c                   const(x'B4')
004300080207     d c_F17           c                   const(x'B5')
004400080207     d c_F18           c                   const(x'B6')
004500080207     d c_F19           c                   const(x'B7')
004600080207     d c_F20           c                   const(x'B8')
004700080207     d c_F21           c                   const(x'B9')
004800080207     d c_F22           c                   const(x'BA')
004900080207     d c_F23           c                   const(x'BB')
005000080207     d c_F24           c                   const(x'BC')
005100080207     d c_Enter         c                   const(x'F1')
005200080207     d c_RollDown      c                   const(x'F4')
005300080207     d c_RollUp        c                   const(x'F5')
005400100211
005500100506     d Digits          c                   const('0123456789')
005600100531
005700080206
005800080206      //---------------------------------------------------------------
005900080206      //?Definizione schiere.
006000080206      //---------------------------------------------------------------
006100100708
006200100708      // - Importatnza clienti da tabella IC
006300100708     d $Ic             s              1    dim(10)
006400100706
006500100706      // - Concorrenti possibili da tabella ITS
006600100708     d $Its            s              4    dim(30)
006700080206
006800100531      // - Tipi trattativa possibili da tabella TTR
006900100531     d $Ttr            s              1    dim(10)
007000100531
007100080206      // - Messaggi di errore
007200110921     d $Msg            s             78    dim(31) ctdata perrcd(1)
007300080206
007400080206      //---------------------------------------------------------------
007500080206      //?Definizione aree dati.
007600080206      //---------------------------------------------------------------
007700080206
007800080206      // - Dati utente
007900080206     d §AzUte        e ds                  extname(AZUTE00F)
008000080206     d                                     dtaara
008100080206     d §DatiUte      e ds                  extname(dDatiUte)
008200080206     d                                     dtaara
008300080206
008400080206      //---------------------------------------------------------------
008500080206      //?Definizione strutture dati.
008600080206      //---------------------------------------------------------------
008700080206
008800080206      // - Status
008900080206     d Psds           sds
009000080206     d   SDSpgm          *proc
009100080206
009200080206      // - InfDS
009300080206     d InfDspF         ds
009400080207     d  dsp_aid              369    369a
009500080206
009600080206      // - Indicatori su DspF
009700080208     d IndDspF         ds
009800100326        // - Indicatori di errore
009900100326     d  ErrMessage                    1n   overlay(IndDspF : 28)
010000110119     d  DspStpCar                     1n   overlay(IndDspF : 40)
010100100326     d  PosCurCmm                     1n   overlay(IndDspF : 50)
010200100706     d  PosCurDtavi                   1n   overlay(IndDspF : 51)
010300100706     d  PosCurDtavf                   1n   overlay(IndDspF : 52)
010400100706     d  PosCurDtchi                   1n   overlay(IndDspF : 53)
010500100706     d  PosCurDtchf                   1n   overlay(IndDspF : 54)
010600100706     d  PosCurDtica                   1n   overlay(IndDspF : 55)
010700100706     d  PosCurConc1                   1n   overlay(IndDspF : 56)
010800100706     d  PosCurConc2                   1n   overlay(IndDspF : 57)
010900100706     d  PosCurConc3                   1n   overlay(IndDspF : 58)
011000100706     d  PosCurConc4                   1n   overlay(IndDspF : 59)
011100100706     d  PosCurConc5                   1n   overlay(IndDspF : 60)
011200100708     d  PosCurDeli                    1n   overlay(IndDspF : 61)
011300100708     d  PosCurDelf                    1n   overlay(IndDspF : 62)
011400100708     d  PosCurCalde                   1n   overlay(IndDspF : 63)
011500100708     d  PosCurDtaff                   1n   overlay(IndDspF : 64)
011600100708     d  PosCurTtr1                    1n   overlay(IndDspF : 65)
011700100708     d  PosCurTtr2                    1n   overlay(IndDspF : 66)
011800100708     d  PosCurTtr3                    1n   overlay(IndDspF : 67)
011900100708     d  PosCurTtr4                    1n   overlay(IndDspF : 68)
012000100708     d  PosCurTic1                    1n   overlay(IndDspF : 69)
012100100708     d  PosCurTic2                    1n   overlay(IndDspF : 70)
012200100708     d  PosCurTic3                    1n   overlay(IndDspF : 71)
012300100708     d  PosCurTic4                    1n   overlay(IndDspF : 72)
012400100708     d  PosCurTic5                    1n   overlay(IndDspF : 73)
012500100708     d  PosCurEsi                     1n   overlay(IndDspF : 74)
012600100708     d  PosCurDet                     1n   overlay(IndDspF : 75)
012700100708     d  PosCurGent                    1n   overlay(IndDspF : 76)
012800100810     d  PosCurTtrio                   1n   overlay(IndDspF : 77)
012900100810     d  PosCurOrd                     1n   overlay(IndDspF : 78)
013000100810     d  PosCurDetc                    1n   overlay(IndDspF : 79)
013100101228     d  PosCurTtr5                    1n   overlay(IndDspF : 80)
013200101228     d  PosCurTtr6                    1n   overlay(IndDspF : 81)
013300101228     d  PosCurAuSci                   1n   overlay(IndDspF : 82)
013400101228     d  PosCurAuScf                   1n   overlay(IndDspF : 83)
013500101228     d  PosCurVltti                   1n   overlay(IndDspF : 84)
013600101228     d  PosCurVlttf                   1n   overlay(IndDspF : 85)
013700110119     d  PosCurCar                     1n   overlay(IndDspF : 86)
013800100326        // - Indicatori di errore generico
013900100326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014000100708
014100100708        // - Tipi trattativa da I/O
014200100708     d V02ttr          ds
014300100708     d  V02ttr1                       1
014400100708     d  V02ttr2                       1
014500100708     d  V02ttr3                       1
014600100708     d  V02ttr4                       1
014700090407
014800090407     d WindDspF        ds                  inz
014900090407     d   WindDspFa             1     49    inz(*zero)
015000090407     d   WindDspFb            50     99    inz(*zero)
015100090319
015200090319     d wlbdat          ds                  inz
015300090319     d  g02dat                 1      8  0
015400090319     d  g02inv                 9     16  0
015500090319     d  g02err                17     17
015600090319     d  g02tgi                18     22  0
015700080206
015800080206      // - Parametri ricevuti
015900080206     d KPJBA         e ds
016000080206
016100080206      // - Ricerca/Controllo tabelle
016200080206     d TIBS02ds      e ds                  inz
016300100326
016400100326      // - Reperimento dati utente
016500100326     d TIBS34ds      e ds
016600100326
016700100326      // - Controllo abilitazioni
016800100211     d TNTAA1DS      e ds                  inz
016900100329
017000100329      // - Passaggio dati a pgm stampa
017100100524     d TNTA98DS      e ds
017200100326
017300110119      // - Carico Filiali abilitate all'utente
017400100326     d TRUL31DS      e ds
017500100326     d POG                    10    759    DIM(250)
017600110119      // - Carico Aree abilitate all'utente
017700110119     d TRUL31DS2     e ds
017800110119     d CAR                    22    171    DIM(50)
017900100708
018000100708      // - Tabella IC = Importanza clienti
018100100708     d dsIC          e ds                  inz
018200110119
018300110119      // - Tabella 05 = Codici Aree
018400110119     d ds05          e ds                  inz
018500090717
018600080206      //---------------------------------------------------------------
018700080206      //?Definizione variabili globali.
018800080206      //---------------------------------------------------------------
018900080206
019000080206      // - Flags booleani
019100100312     d $End            s               n   inz(*off)
019200100326     d $ErrGrave       s               n   inz(*off)
019300100326     d $Fine           s               n   inz(*off)
019400080208     d $InzD01         s               n   inz(*on)
019500100706     d $InzD02         s               n   inz(*on)
019600080206
019700080207      // - Campi associati al video
019800080207     d $Video          s              2    inz('D1')
019900100531
020000100531      // - Indici di schiera
020100100531     d xx              s              4  0 inz
020200080206
020300090401      // - Campi di comodo data
020400090401     d  data_eur       s               d   Datfmt(*eur)
020500090401     d  data_iso       s               d   Datfmt(*iso)
020600100526     d  data_iso1      s               d   Datfmt(*iso)
020700121029     d wOggi           s              8  0 inz
020800121029     d wOggi_2         s              8  0 inz
020900090401
021000080206      // - Campi di comodo
021100100708     d C_tab           s                   like(TBEcod)
021200100708
021300101228     d wausci          s                   like(V01ausci) inz
021400101228     d wauscf          s                   like(V01auscf) inz
021500100708     d wdeli           s              3  0 inz
021600100708     d wdelf           s              3  0 inz
021700100706     d wdtaff          s              8  0 inz
021800100706     d wdtavf          s              8  0 inz
021900100706     d wdtavi          s              8  0 inz
022000100706     d wdtchf          s              8  0 inz
022100100706     d wdtchi          s              8  0 inz
022200100706     d wdtica          s              8  0 inz
022300100714     d wgiorni         s              5  0 inz
022400101228     d wvaltti         s                   like(V01valtti) inz
022500101228     d wvalttf         s                   like(V01valttf) inz
022600100708
022700100708     d idTabella       s              2
022800100708     d Ordinamento     s              1
022900100708     d idElemento      s              8
023000100708     d TastoUscita     s              1
023100100708
023200080208
023300090508      //---------------------------------------------------------------
023400090508      //?Definizione procedure usate.
023500090508      //---------------------------------------------------------------
023600100329
023700100329      // - Sottomissione lavoro batch
023800100329     d bch10           pr                  extpgm('BCH10')
023900100329     d  kpjba                              likeds(KPJBA)
024000100329
024100100329      // - Richiamo diretto lavoro batch
024200100524     d tnta98c         pr                  extpgm('TNTA98C')
024300100329     d  kpjba                              likeds(KPJBA)
024400110119
024500110119      // - Filiali e aree abilitate
024600110119     d trul31r         pr                  extpgm('TRUL31R')
024700110119     d  kpjba                              likeds(KPJBA)
024800110119     d  trul31ds                           likeds(trul31ds)
024900110119     d  trul31ds2                          likeds(trul31ds2)
025000080206
025100080626      //---------------------------------------------------------------
025200080626      //?prototipi
025300080626      //---------------------------------------------------------------
025400100326
025500080626      /copy gaitrasrc/srcprotopr,tibs02r
025600080626      /copy gaitrasrc/srcprotopr,tibs34r
025700100708      /copy gaitrasrc/srcprotopr,trul19r
025800100211      /copy gaitrasrc/srcprotopr,tntaa1c
025900090406      /copy gaitrasrc/srcprotopr,xsrda8
026000130807      /copy gaitrasrc/srcprotoPI,TRMK43R_1
026100130807      /copy gaitrasrc/srcprotoPR,TRMK43R
026200100326
026300080206      //---------------------------------------------------------------
026400080206      //?Definizione key-list.
026500080206      //---------------------------------------------------------------
026600080206
026700100326       // - File TABEL00F
026800100326     d k03tabel      e ds                  extname(TABEL00F:*key)
026900100326     d                                     prefix(k_)
027000080206
027100080206      //---------------------------------------------------------------
027200080206      //?Riepilogo indicatori.
027300080206      //---------------------------------------------------------------
027400080207      // 28    : Emissione messaggio di errore a video
027500100708      // 50-76 : Errori Video
027600080207      // 99    : Generico di Errore
027700080206      //---------------------------------------------------------------
027800080206
027900080206      //---------------------------------------------------------------
028000080206      //?M A I N - L I N E
028100080206      //---------------------------------------------------------------
028200080206
028300080206     c     *Entry        plist
028400080206     c                   parm                    KPJBA
028500080206
028600080206      /free
028700080206
028800100326       //?Operazioni iniziali
028900080206       exsr RoutInz;
029000090601
029100100326       //?Gestione video
029200080206       DOW $Fine = *off;
029300100329         SELECT;
029400100329           WHEN $Video = 'D1';
029500080206             exsr GesD01;
029600100706           WHEN $Video = 'D2';
029700100706             exsr GesD02;
029800100329           OTHER;
029900080206             $Fine = *on;
030000100329         ENDSL;
030100080206       ENDDO;
030200080206
030300100326       //?Operazioni finali
030400080206       exsr RoutEnd;
030500080206
030600080206       //--------------------------------------------------------------
030700080206       //?Operazioni iniziali.
030800080206       //--------------------------------------------------------------
030900080206       BEGSR RoutInz;
031000080206
031100100326       //?Impostazione campi "fissi"
031200100326         k_TBLkut = 1;
031300080206
031400100326       //?Reperimento dati job
031500080206         exsr DatiJob;
031600121029
031700121029       //?Calcolo oggi - 2 anni
031800121029         wOggi = %dec( %date() );
031900121029         Data_ISO = %date(wOggi);
032000121029         Data_ISO = Data_ISO - %years(2);
032100121029         wOggi_2 = %dec(Data_ISO);
032200100211
032300100326       //?Controllo se utente autorizzato alla funzione
032400100211         reset TNTAA1DS;
032500100211         ITAA1caut = '§utegtc';
032600100211         kpjbu     = TNTAA1DS;
032700100211         tntaa1c (kpjba);
032800100211         TNTAA1DS = kpjbu;
032900100211
033000100211         IF  OTAA1fabi = 'N';
033100080206           $ErrGrave   = *on;
033200080206           errMessage  = *on;
033300080206           errGenerico = *on;
033400100329           V01msg = $Msg(01);
033500080206           leavesr;
033600080206         endif;
033700100312
033800100312         clear TRUL31DS;
033900110119         clear TRUL31DS2;
034000100312         I31abi = OTAA1cabi;
034100100312         I31cdi = DUTdis;
034200100312         I31car = DUTare;
034300100312         I31cpo = DUTpou;
034400110119         trul31r (kpjba : trul31ds : trul31ds2);
034500100312         IF  O31pog <= *zeros;
034600100312           $ErrGrave   = *on;
034700100326           ErrMessage  = *on;
034800100326           ErrGenerico = *on;
034900100326           V01msg = $Msg(01);
035000100312           leavesr;
035100100312         ENDIF;
035200110119
035300110119         //?Abilito richiesta per Area
035400110119         DspStpCar = *off;
035500110119         IF  car(2) > *zeros;
035600110119           DspStpCar = *on;
035700110119         ENDIF;
035800100708
035900100708         //?Carico sk Importanza Clienti
036000100708         clear xx;
036100100708         k_TBLcod = 'IC';
036200100708         setll (k_TBLkut:k_TBLcod) TABEL00F;
036300100708         reade (k_TBLkut:k_TBLcod) TABEL00F;
036400100708
036500100708         DOW not %eof(TABEL00F);
036600100708           IF  TBLflg = *blanks;
036700100708             xx += 1;
036800100708             $Ic(xx) = %subst(TBLkey:1:1);
036900100708           ENDIF;
037000100708           reade (k_TBLkut:k_TBLcod) TABEL00F;
037100100708         ENDDO;
037200100706
037300100706         //?Carico sk Concorrenti
037400100706         clear xx;
037500100706         C_Tab = 'ITS';
037600100708         setll C_tab TNTBE01L;
037700100708         reade C_tab TNTBE01L;
037800100706
037900100706         DOW not %eof(TNTBE01L);
038000100706           IF  TBEatb = *blanks;
038100100706             xx += 1;
038200100706             $Its(xx) = %subst(TBEke2:1:4);
038300100706           ENDIF;
038400100708           reade C_tab TNTBE01L;
038500100706         ENDDO;
038600100531
038700100531         //?Carico sk Tipi trattavia
038800100531         clear xx;
038900100706         C_Tab = 'TTR';
039000100708         setll C_tab TNTBE01L;
039100100708         reade C_tab TNTBE01L;
039200100531
039300100531         DOW not %eof(TNTBE01L);
039400100531           IF  TBEatb = *blanks;
039500100531             xx += 1;
039600100531             $Ttr(xx) = %subst(TBEke1:1:1);
039700100531           ENDIF;
039800100708           reade C_tab TNTBE01L;
039900100531         ENDDO;
040000090714
040100080206       ENDSR;
040200080206
040300080206       //--------------------------------------------------------------
040400080206       //?Reperimento Dati del job (Utente/Operativi).
040500080206       //--------------------------------------------------------------
040600080206       BEGSR DatiJob;
040700080206
040800080206         in(E) §AzUte;
040900080206         if NOT %error;
041000080206           in(E) §DatiUte;
041100080206         endif;
041200080206         if %error or RSut = *blanks;
041300080206           clear TIBS34ds;
041400080206           tibs34r(tibs34ds);
041500080206           in §AzUte;
041600080206           in §DatiUte;
041700080206         endif;
041800080206
041900080206       ENDSR;
042000080206
042100080206       //--------------------------------------------------------------
042200080206       //?Gestione videata D01
042300080206       //--------------------------------------------------------------
042400080206       BEGSR GesD01;
042500080206
042600100326       //?Inizializzazione videata
042700100326         IF  $InzD01;
042800080206           exsr InzD01;
042900080206           $InzD01 = *off;
043000100326         ENDIF;
043100080206
043200100326       //?Emissione videata
043300100524         exfmt TA89D01;
043400100326         ErrMessage   = *off;
043500100326         ErrGenerico  = *off;
043600100326         clear V01msg;
043700080207
043800100326         SELECT;
043900100326
044000100326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
044100100326           WHEN  $ErrGrave;
044200100326             $Fine = *on;
044300100326
044400100326       //?F3=Fine
044500100326           WHEN  dsp_aid = c_F03;
044600080206             exsr F03D01;
044700100329
044800100329       //?F6=Conferma
044900100329           WHEN  dsp_aid = c_F06;
045000100329             exsr CtrD01;
045100100329             IF  ErrGenerico;
045200100329               leavesr;
045300100329             ENDIF;
045400100329             exsr F06D01;
045500100329
045600100326       //?Enter
045700100326           OTHER;
045800080206             exsr CtrD01;
045900100326             IF  ErrGenerico;
046000080206               leavesr;
046100100326             ENDIF;
046200100326         ENDSL;
046300080206
046400080206       ENDSR;
046500080206
046600080206       //--------------------------------------------------------------
046700080206       //?Inizializzazione videata D01
046800080206       //--------------------------------------------------------------
046900080206       BEGSR InzD01;
047000080206
047100100708         clear TA89D01;
047200100329         V01pgm = SDSpgm;
047300100329         V01sut = rsut;
047400100329         V01sif = knsif;
047500100329         V01mus = knmus;
047600101228
047700101228         V01valttsi = '+';
047800101228         V01valttsf = '+';
047900100708
048000100708         V01delsi = '+';
048100100708         V01delsf = '+';
048200101228
048300101228         V01auscsi = '+';
048400101228         V01auscsf = '+';
048500110119
048600110119       //?Posiziono il cursore sul commerciale se NO richiesta per area
048700110119         IF  DspStpCar;
048800110119           PosCurCmm = *on;
048900110119         ENDIF;
049000100708
049100080206       ENDSR;
049200080206
049300080206       //--------------------------------------------------------------
049400080206       //?Gestione tasto funzionale F3 da videata D01
049500080206       //?F3=Fine
049600080206       //--------------------------------------------------------------
049700080206       BEGSR F03D01;
049800080206
049900100329       //?Chiusura del programma
050000080206         $Fine = *on;
050100080206
050200080206       ENDSR;
050300100329
050400100329       //--------------------------------------------------------------
050500100329       //?Gestione tasto funzionale F6 da videata D01
050600100329       //?F6=Conferma
050700100329       //--------------------------------------------------------------
050800100329       BEGSR F06D01;
050900100706
051000100708       //?Videata sucessiva
051100100706         $Video = 'D2';
051200100706         $InzD02 = *on;
051300100708
051400100708       ENDSR;
051500100708
051600100708       //--------------------------------------------------------------
051700100708       //?Gestione videata D02
051800100708       //--------------------------------------------------------------
051900100708       BEGSR GesD02;
052000100708
052100100708       //?Inizializzazione videata
052200100708         IF  $InzD02;
052300100708           exsr InzD02;
052400100708           $InzD02 = *off;
052500100708         ENDIF;
052600100708
052700100708       //?Emissione videata
052800100708         exfmt TA89D02;
052900100708         ErrMessage   = *off;
053000100708         ErrGenerico  = *off;
053100100708         clear V02msg;
053200100708
053300100708         SELECT;
053400100708
053500100708       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
053600100708           WHEN  $ErrGrave;
053700100708             $Fine = *on;
053800100708
053900100708       //?F3=Fine
054000100708           WHEN  dsp_aid = c_F03;
054100100708             exsr F03D01;
054200100708
054300100708       //?F6=Conferma
054400100708           WHEN  dsp_aid = c_F06;
054500100708             exsr CtrD02;
054600100708             IF  ErrGenerico;
054700100708               leavesr;
054800100708             ENDIF;
054900100708             exsr F06D02;
055000100708
055100100708       //?F12=Ritorno
055200100708           WHEN  dsp_aid = c_F12;
055300100708             exsr F12D02;
055400100708
055500100708       //?Enter
055600100708           OTHER;
055700100708             exsr CtrD02;
055800100708             IF  ErrGenerico;
055900100708               leavesr;
056000100708             ENDIF;
056100100708         ENDSL;
056200100708
056300100708       ENDSR;
056400100708
056500100708       //--------------------------------------------------------------
056600100708       //?Inizializzazione videata D02
056700100708       //--------------------------------------------------------------
056800100708       BEGSR InzD02;
056900100708
057000100708         clear TA89D02;
057100100708         V02pgm = SDSpgm;
057200100708         V02sut = rsut;
057300100708         V02sif = knsif;
057400100708         V02mus = knmus;
057500100708
057600100915         V02ttrio = 'O';
057700100708         V02ord   = 'D';
057800100716         V02cmmt  = 'S';
057900100716         IF  V01cmm = *zeros or V01cmm = *blanks;
058000100716           V02gent = 'S';
058100100716         ELSE;
058200100716           V02gent = 'N';
058300100716         ENDIF;
058400100708
058500100708       ENDSR;
058600100708
058700100708       //--------------------------------------------------------------
058800100708       //?Gestione tasto funzionale F6 da videata D02
058900100708       //?F6=Conferma
059000100708       //--------------------------------------------------------------
059100100708       BEGSR F06D02;
059200100708
059300100708         clear TNTA98ds;
059400110119         IF  V01car > *zeros;
059500110119           ITA98car   = %dec(V01car:3:0);
059600110119         ENDIF;
059700100708         IF  V01cmm > *zeros;
059800100708           ITA98cmm   = %dec(V01cmm:7:0);
059900100708         ENDIF;
060000100708         ITA98elcmm = V01elcmm;
060100100708         ITA98dtavi = wdtavi;
060200100708         ITA98dtavf = wdtavf;
060300100708         ITA98dtchi = wdtchi;
060400100708         ITA98dtchf = wdtchf;
060500100708         ITA98dtica = wdtica;
060600110114       //?Imposto il valore traffico totale con il segno immesso a video
060700110114         IF  V01valttsi = '-';
060800110114           ITA98vltti = V01valtti * -1;
060900110114         ELSE;
061000110114           ITA98vltti = V01valtti;
061100110114         ENDIF;
061200110114         IF  V01valttsf = '-';
061300110114           ITA98vlttf = V01valttf * -1;
061400110114         ELSE;
061500110114           ITA98vlttf = V01valttf;
061600110114         ENDIF;
061700100708         ITA98valte = V01valte;
061800100708         ITA98pkg   = V01pkg;
061900100708         ITA98conc1 = V01conc1;
062000100708         ITA98conc2 = V01conc2;
062100100708         ITA98conc3 = V01conc3;
062200100708         ITA98conc4 = V01conc4;
062300100708         ITA98conc5 = V01conc5;
062400100708         ITA98delsi = V01delsi;
062500100714         ITA98deli  = V01deli;
062600100708         ITA98delsf = V01delsf;
062700100714         ITA98delf  = V01delf;
062800110114       //?Imposto il valore aumento/sconto con il segno immesso a video
062900110114         IF  V01auscsi = '-';
063000110114           ITA98ausi = V01ausci * -1;
063100110114         ELSE;
063200110114           ITA98ausi = V01ausci;
063300110114         ENDIF;
063400110114         IF  V01auscsf = '-';
063500110114           ITA98ausf = V01auscf * -1;
063600110114         ELSE;
063700110114           ITA98ausf = V01auscf;
063800110114         ENDIF;
063900100708         ITA98tpt1  = V01ttr1;
064000100708         ITA98tpt2  = V01ttr2;
064100100708         ITA98tpt3  = V01ttr3;
064200100708         ITA98tpt4  = V01ttr4;
064300101228         ITA98tpt5  = V01ttr5;
064400101228         ITA98tpt6  = V01ttr6;
064500100708         ITA98tic1  = V01tic1;
064600100708         ITA98tic2  = V01tic2;
064700100708         ITA98tic3  = V01tic3;
064800100708         ITA98tic4  = V01tic4;
064900100708         ITA98tic5  = V01tic5;
065000100709         ITA98calde = V01calde;
065100100729         ITA98dtaff = wdtaff;
065200100708         ITA98esi   = V01esi;
065300100708
065400100708         ITA98det   = V02det;
065500100708         ITA98detc  = V02detc;
065600100708         ITA98ttrio = V02ttrio;
065700100708         ITA98ttr1  = V02ttr1;
065800100708         ITA98ttr2  = V02ttr2;
065900100708         ITA98ttr3  = V02ttr3;
066000100708         ITA98ttr4  = V02ttr4;
066100101228         ITA98ttr5  = V02ttr5;
066200101228         ITA98ttr6  = V02ttr6;
066300100708         ITA98ord   = V02ord;
066400100708         ITA98cmmt  = V02cmmt;
066500100708         ITA98gent  = V02gent;
066600100708         kcoaz = 'TA98';
066700100708         kpjbu = TNTA98ds;
066800100708         kbuff = *blanks;
066900100708
067000100708       //?Richiamo interattivo
067100100714         IF  knmus = *all'1';
067200100708           TNTA98C(kpjba);
067300100708       //?Richiamo batch
067400100714         ELSE;
067500100714           BCH10(kpjba);
067600100714         ENDIF;
067700100708
067800100708       //?Chiusura del programma
067900100708         $Fine = *on;
068000100708
068100100708       ENDSR;
068200100708
068300100708       //--------------------------------------------------------------
068400100708       //?Gestione tasto funzionale F12 da videata D02
068500100708       //?F12=Ritorno
068600100708       //--------------------------------------------------------------
068700100708       BEGSR F12D02;
068800100708
068900100708       //?Ritorno alla videata precedente
069000100708         $InzD01 = *off;
069100100708         $Video = 'D1';
069200100708
069300100708       ENDSR;
069400090717
069500080206       //--------------------------------------------------------------
069600080206       //?Controllo videata D01
069700080206       //--------------------------------------------------------------
069800080206       BEGSR CtrD01;
069900110119
070000110119         clear v01dcar;
070100110119         clear v01dcmm;
070200100326
070300100706         clear wdtaff;
070400100706         clear wdtavf;
070500100706         clear wdtavi;
070600100706         clear wdtchf;
070700100706         clear wdtchi;
070800100706         clear wdtica;
070900100708
071000101228         clear wausci;
071100101228         clear wauscf;
071200100708         clear wdeli;
071300100708         clear wdelf;
071400101228         clear wvaltti;
071500101228         clear wvalttf;
071600090427
071700100326         WindDspF = IndDspF;
071800100326         reset WindDspFb;
071900100326         IndDspF  = WindDspF;
072000110119
072100110119       //?AREA
072200110119       //?Ricerca
072300110119         IF  %scan('?' : V01car) > *zero;
072400110119           idTabella = '05';
072500110119           clear Ordinamento;
072600110119           clear idElemento;
072700110119           clear TastoUscita;
072800110119           Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
072900110119           V01car = idElemento;
073000110119         ENDIF;
073100110119
073200110119       //?Controllo se impostato
073300110119         IF  V01car <> *blanks and V01car <> *zeros;
073400110119        //?Solo numerico
073500110119           IF  %check(digits:V01car) > 0;
073600110119             ErrMessage  = *on;
073700110119             ErrGenerico = *on;
073800110119             PosCurCar   = *on;
073900110119             V01msg      = $Msg(29);
074000110119             leavesr;
074100110119           ENDIF;
074200110119         //?Valido in tabella
074300110119           clear ds05;
074400110119           k_TBLcod = '05';
074500110119           k_TBLkey = V01car;
074600110119           chain  %kds(K03tabel) TABEL00F;
074700110119           IF  NOT %found(TABEL00F)
074800110119               or  TBLflg <> *blank;
074900110119             ErrMessage  = *on;
075000110119             ErrGenerico = *on;
075100110119             PosCurCar   = *on;
075200110119             V01msg      = $Msg(29);
075300110119             leavesr;
075400110119           ENDIF;
075500110119
075600110119           ds05 = TBLuni;
075700110119           V01dcar = §05des;
075800110119
075900110119         //?Utente abilitato all'area
076000110119           IF  %lookup(V01car : car) = 0;
076100110119             ErrMessage  = *on;
076200110119             ErrGenerico = *on;
076300110119             PosCurCar   = *on;
076400110119             V01msg      = $Msg(29);
076500110119             leavesr;
076600110119           ENDIF;
076700110119         ENDIF;
076800100326
076900100706       //?COMMERCIALE
077000110921
077100110921       //?Se sono in sede e non seleziono ne l'area e ne il commerciale errore
077200110921         IF   dutpou = 046 and ((V01car = *blanks or V01car = *zeros) and
077300110921           (V01cmm = *blanks or V01cmm = *zeros));
077400110921           ErrMessage  = *on;
077500110921           ErrGenerico = *on;
077600110921           PosCurCar   = *on;
077700110921           V01msg      = $Msg(31);
077800110921           leavesr;
077900110921         ENDIF;
078000110921
078100110119       //?Non immettere se richiesta l'area
078200110119         IF  V01car <> *blanks and V01car > *zeros and
078300110119             V01cmm <> *blanks and V01cmm > *zeros;
078400110119           ErrMessage  = *on;
078500110119           ErrGenerico = *on;
078600110119           PosCurCmm   = *on;
078700110119           V01msg      = $Msg(30);
078800110119           leavesr;
078900110119         ENDIF;
079000110119
079100100706       //?Ricerca
079200100326         IF  %scan('?' : V01cmm) > *zero;
079300100326           ErrGenerico = *on;
079400100326           PosCurCmm   = *on;
079500130807           clear  TRMK43ds;
079600130807           iMK43solU = 'S';
079700130807           iMK43fil  = DUTpou;
079800130807           TRMK43R (kpjba : TRMK43ds);
079900130807           if  oMK43err = *off  and  oMK43fxx = *blank;
080000130807             V01cmm  = %editc( oMK43cmm : 'X' );
080100130807             V01dcmm = oMK43des;
080200130807           endif;
080300100326           leavesr;
080400100326         ENDIF;
080500100326
080600100706       //?Controllo se impostato
080700100330         IF  V01cmm <> *blanks and V01cmm <> *zeros;
080800100706        //?Solo numerico
080900100326           IF  %check(digits:V01cmm) > 0;
081000100326             ErrMessage  = *on;
081100100326             ErrGenerico = *on;
081200100326             PosCurCmm   = *on;
081300100329             V01msg      = $Msg(02);
081400100326             leavesr;
081500100326           ENDIF;
081600100706         //?Valido in tabella
081700130807           CMMcod = %int(V01cmm);
081800130807           chain  (CMMcod)  AZCMM000;
081900130807           IF  NOT %found(AZCMM01L);
082000100326             ErrMessage  = *on;
082100100326             ErrGenerico = *on;
082200100329             PosCurCmm   = *on;
082300100329             V01msg      = $Msg(02);
082400100326             leavesr;
082500100326           ENDIF;
082600100326
082700130807           V01dcmm = CMMdes;
082800100326
082900100706         //?No vari o clienti inattivi
083000130807           IF  CMMpar <> *blanks;
083100100326             ErrMessage  = *on;
083200100326             ErrGenerico = *on;
083300100326             PosCurCmm   = *on;
083400100329             V01msg      = $Msg(02);
083500100326             leavesr;
083600100326           ENDIF;
083700100706         //?Utente abilitato al commerciale
083800100326           clear TNTAA1DS;
083900100326           ITAA1caut = '§utegtc';
084000100326           ITAA1cmm  = %dec(V01cmm:7:0);
084100100326           kpjbu     = tntaa1ds;
084200100326           tntaa1c (kpjba);
084300100326           TNTAA1DS = kpjbu;
084400100326           IF  OTAA1fabi = 'N';
084500100326             ErrMessage  = *on;
084600100326             ErrGenerico = *on;
084700100326             PosCurCmm   = *on;
084800100329             V01msg      = $Msg(02);
084900100326             leavesr;
085000100326           ENDIF;
085100100706         //?Solo unificante
085200130807           IF  %dec(V01cmm:7:0) <> CMMuni;
085300100524             ErrMessage  = *on;
085400100524             ErrGenerico = *on;
085500100524             PosCurCmm   = *on;
085600100524             V01msg      = $Msg(03);
085700100524             leavesr;
085800100524           ENDIF;
085900100524         ENDIF;
086000100709
086100100709       //?Tipo commerciale INTERNO ESTERNO TUTTI
086200100709       //?Se impostato un solo codice commerciale la selezione
086300100709       //?deve essere congruente con il codice
086400110119         IF  (V01car > *zeros or V01cmm > *zeros) and
086500110119             V01elcmm <> *blanks;
086600130807           IF  V01elcmm = 'I' and CMMfun <> 'COMIN'
086700130807               and CMMfun <> 'ASC' and CMMfun <> 'SA'
086800110119               and V01cmm > *zeros;
086900100709           ErrMessage  = *on;
087000100709           ErrGenerico = *on;
087100100709           PosCurCmm   = *on;
087200101006           V01msg      = $Msg(24);
087300101006           V01msg      = %trim(V01msg) + ' Richiesta selezione INTERNO';
087400100709           leavesr;
087500100709           ENDIF;
087600130807           IF  V01elcmm = 'E' and (CMMfun = 'COMIN'
087700130807               or CMMfun = 'ASC' or CMMfun = 'SA')
087800110119               and V01cmm > *zeros;
087900100709           ErrMessage  = *on;
088000100709           ErrGenerico = *on;
088100100709           PosCurCmm   = *on;
088200101006           V01msg      = $Msg(24);
088300101006           V01msg      = %trim(V01msg) + ' Richiesta selezione ESTERNO';
088400100709           leavesr;
088500100709           ENDIF;
088600100709         ENDIF;
088700100706
088800100706       //?DATE
088900100706       //?Almeno una delle tre date è da immettere
089000100706         IF  V01dtavi = 0 and V01dtavf = 0 and
089100100706             V01dtchi = 0 and V01dtchf = 0 and
089200100706             V01dtica = 0;
089300100708           ErrMessage  = *on;
089400100708           ErrGenerico = *on;
089500100706           PosCurDtavi = *on;
089600100706           V01msg      = $Msg(04);
089700100706           V01msg      = %trim(V01msg) + '. Richiedere almeno un periodo';
089800100706           leavesr;
089900100706         ENDIF;
090000100706
090100100708       //?Trattative avviate
090200100708         IF  V01dtavi <> 0 or V01dtavf <> 0;
090300100708         //?Dal
090400100706           IF  V01dtavi <> 0;
090500100706             clear wlbdat;
090600100706             G02dat = V01dtavi;
090700100706             xsrda8(wlbdat);
090800100706             IF  G02err = '1';
090900100706               ErrMessage  = *on;
091000100706               ErrGenerico = *on;
091100100706               PosCurDtavi = *on;
091200100706               V01msg      = $Msg(04);
091300100706               leavesr;
091400100706             ENDIF;
091500100706
091600100706             V01dtavi = G02dat;
091700100706             wdtavi   = G02inv;
091800121029
091900121029         //?La data non può essere inferiore a 2 anni da oggi
092000121029             IF  wdtavi < wOggi_2;
092100121029               ErrMessage  = *on;
092200121029               ErrGenerico = *on;
092300121029               PosCurDtavi = *on;
092400121029               V01msg      = $Msg(04);
092500121029               V01msg = %trim(V01msg) +
092600121029               ' è inferiore a 2 anni, non ci sono i dati';
092700121029               leavesr;
092800121029             ENDIF;
092900100708
093000100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
093100100706             IF  wdtavi < 20000101 or wdtavi > 20391231;
093200100706               ErrMessage  = *on;
093300100706               ErrGenerico = *on;
093400100706               PosCurDtavi = *on;
093500100706               V01msg      = $Msg(04);
093600100706               leavesr;
093700100706             ENDIF;
093800100706           ENDIF;
093900100708         //?Al
094000100706           IF  V01dtavf <> 0;
094100100706             clear wlbdat;
094200100706             G02dat = V01dtavf;
094300100706             xsrda8(wlbdat);
094400100706             IF  G02err = '1';
094500100706               ErrMessage  = *on;
094600100706               ErrGenerico = *on;
094700100706               PosCurDtavf = *on;
094800100706               V01msg      = $Msg(04);
094900100706               leavesr;
095000100706             ENDIF;
095100100706
095200100706             V01dtavf = G02dat;
095300100706             wdtavf   = G02inv;
095400100706
095500100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
095600100706             IF  wdtavf < 20000101 or wdtavf > 20391231;
095700100706               ErrMessage  = *on;
095800100706               ErrGenerico = *on;
095900100706               PosCurDtavf = *on;
096000100706               V01msg      = $Msg(04);
096100100706               leavesr;
096200100706             ENDIF;
096300100706           ENDIF;
096400100708         //?Se impostata data "dal" e non "al"
096500100706         //?imposto "al" = data del giorno
096600100706           IF  V01dtavi > 0 and V01dtavf = 0;
096700100706             wdtavf    = %dec(%date());
096800100706             data_eur  = %date(wdtavf:*iso);
096900100706             V01dtavf  = %dec(data_eur);
097000100706           ENDIF;
097100100708         //?"DAL" congruente con "AL"
097200100706           IF  wdtavi > 0  and  wdtavf = 0;
097300100706             ErrMessage  = *on;
097400100706             ErrGenerico = *on;
097500100706             PosCurDtavi = *on;
097600100706             V01msg      = $Msg(05);
097700100706             leavesr;
097800100706           ENDIF;
097900100706           IF  wdtavi > 0  and  wdtavf > 0  and  wdtavi > wdtavf;
098000100706             ErrMessage  = *on;
098100100706             ErrGenerico = *on;
098200100706             PosCurDtavf = *on;
098300100706             V01msg      = $Msg(05);
098400100706             leavesr;
098500100706           ENDIF;
098600100908         //?Range massimo di 65 giorni
098700100908           data_iso  = %date(wdtavi);
098800100908           data_iso1 = %date(wdtavf);
098900100908           wgiorni = %diff( data_iso1 : data_iso : *days );
099000100908           IF  wgiorni > 65;
099100100908             ErrMessage  = *on;
099200100908             ErrGenerico = *on;
099300100908             PosCurDtavi = *on;
099400100908             V01msg      = $Msg(04);
099500100908             V01msg      = %trim(V01msg) + '. Range richiesto > di 65 gg.';
099600100908             leavesr;
099700100908           ENDIF;
099800100706         ENDIF;
099900100706
100000100708       //?Trattative chiuse
100100100706         IF  V01dtchi <> 0 or V01dtchi <> 0;
100200100708         //?Dal
100300100706           IF  V01dtchi <> 0;
100400100706             clear wlbdat;
100500100706             G02dat = V01dtchi;
100600100706             xsrda8(wlbdat);
100700100706             IF  G02err = '1';
100800100706               ErrMessage  = *on;
100900100706               ErrGenerico = *on;
101000100706               PosCurDtchi = *on;
101100100706               V01msg      = $Msg(04);
101200100706               leavesr;
101300100706             ENDIF;
101400100706
101500100706             V01dtchi = G02dat;
101600100706             wdtchi   = G02inv;
101700121029
101800121029         //?La data non può essere inferiore a 2 anni da oggi
101900121029             IF  wdtchi < wOggi_2;
102000121029               ErrMessage  = *on;
102100121029               ErrGenerico = *on;
102200121029               PosCurDtchi = *on;
102300121029               V01msg      = $Msg(04);
102400121029               V01msg = %trim(V01msg) +
102500121029               ' è inferiore a 2 anni, non ci sono i dati';
102600121029               leavesr;
102700121029             ENDIF;
102800100706
102900100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
103000100706             IF  wdtchi < 20000101 or wdtchi > 20391231;
103100100706               ErrMessage  = *on;
103200100706               ErrGenerico = *on;
103300100706               PosCurDtchi = *on;
103400100706               V01msg      = $Msg(04);
103500100706               leavesr;
103600100706             ENDIF;
103700100706           ENDIF;
103800100708         //?Al
103900100706           IF  V01dtchf <> 0;
104000100706             clear wlbdat;
104100100706             G02dat = V01dtchf;
104200100706             xsrda8(wlbdat);
104300100706             IF  G02err = '1';
104400100706               ErrMessage  = *on;
104500100706               ErrGenerico = *on;
104600100706               PosCurDtchf = *on;
104700100706               V01msg      = $Msg(04);
104800100706               leavesr;
104900100706             ENDIF;
105000100706
105100100706             V01dtchf = G02dat;
105200100706             wdtchf   = G02inv;
105300100706
105400100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
105500100706             IF  wdtchf < 20000101 or wdtchf > 20391231;
105600100706               ErrMessage  = *on;
105700100706               ErrGenerico = *on;
105800100706               PosCurDtchf = *on;
105900100706               V01msg      = $Msg(04);
106000100706               leavesr;
106100100706             ENDIF;
106200100706           ENDIF;
106300100708         //?Se impostata data "dal" e non "al"
106400100706         //?imposto "al" = data del giorno
106500100706           IF  V01dtchi > 0 and V01dtchf = 0;
106600100706             wdtchf    = %dec(%date());
106700100706             data_eur  = %date(wdtchf:*iso);
106800100706             V01dtchf  = %dec(data_eur);
106900100706           ENDIF;
107000100708         //?"DAL" congruente con "AL"
107100100706           IF  wdtchi > 0  and  wdtchf = 0;
107200100706             ErrMessage  = *on;
107300100706             ErrGenerico = *on;
107400100706             PosCurDtchi = *on;
107500100706             V01msg      = $Msg(05);
107600100706             leavesr;
107700100706           ENDIF;
107800100706           IF  wdtchi > 0  and  wdtchf > 0  and  wdtchi > wdtchf;
107900100706             ErrMessage  = *on;
108000100706             ErrGenerico = *on;
108100100706             PosCurDtchf = *on;
108200100706             V01msg      = $Msg(05);
108300100706             leavesr;
108400100706           ENDIF;
108500100908         //?Range massimo di 65 giorni
108600100908           data_iso  = %date(wdtchi);
108700100908           data_iso1 = %date(wdtchf);
108800100908           wgiorni = %diff( data_iso1 : data_iso : *days );
108900100908           IF  wgiorni > 65;
109000100908             ErrMessage  = *on;
109100100908             ErrGenerico = *on;
109200100908             PosCurDtchi = *on;
109300100908             V01msg      = $Msg(04);
109400100908             V01msg      = %trim(V01msg) + '. Range richiesto > di 65 gg.';
109500100908             leavesr;
109600100908           ENDIF;
109700100706         ENDIF;
109800100706
109900100708       //?Trattative in corso
110000100706         IF  V01dtica <> 0;
110100100706           clear wlbdat;
110200100706           G02dat = V01dtica;
110300100706           xsrda8(wlbdat);
110400100706           IF  G02err = '1';
110500100706             ErrMessage  = *on;
110600100706             ErrGenerico = *on;
110700100706             PosCurDtica = *on;
110800100706             V01msg      = $Msg(04);
110900100706             leavesr;
111000100706           ENDIF;
111100100706
111200100706           V01dtica = G02dat;
111300100706           wdtica   = G02inv;
111400100706
111500100706         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
111600100706           IF  wdtica < 20000101 or wdtica > 20391231;
111700100706             ErrMessage  = *on;
111800100706             ErrGenerico = *on;
111900100706             PosCurDtica = *on;
112000100706             V01msg      = $Msg(04);
112100100706             leavesr;
112200100706           ENDIF;
112300100909         //?Massimo 65 giorni indietro rispetto a oggi
112400100706           data_iso  = %date(wdtica);
112500100708           data_iso1 = %date();
112600100706           wgiorni = %diff( data_iso1 : data_iso : *days );
112700100909           IF  wgiorni > 65;
112800100706             ErrMessage  = *on;
112900100706             ErrGenerico = *on;
113000100706             PosCurDtica = *on;
113100100706             V01msg      = $Msg(04);
113200100909             V01msg      = %trim(V01msg) + '. Più vecchia di 65 gg. da oggi';
113300100706             leavesr;
113400100706           ENDIF;
113500100706         ENDIF;
113600100706
113700100706       //?PARAMETRI DI SELEZIONE
113800101228         //?Valore traffico totale
113900101228         IF  V01valtti <> 0 or V01valttf <> 0;
114000101228         //?Obbligatori tutti e 2 se immesso uno dei due
114100101228           IF  V01valtti <> 0  and  V01valttf = 0;
114200101228             ErrMessage  = *on;
114300101228             ErrGenerico = *on;
114400101228             PosCurVlttf = *on;
114500101228             V01msg      = $Msg(25);
114600101228             leavesr;
114700101228           ENDIF;
114800101228           IF  V01valtti = 0  and  V01valttf <> 0;
114900101228             ErrMessage  = *on;
115000101228             ErrGenerico = *on;
115100101228             PosCurVltti = *on;
115200101228             V01msg      = $Msg(25);
115300101228             leavesr;
115400101228           ENDIF;
115500101228         //?Controllo congruenza tra "DAL" e "AL"
115600101228           wvaltti = V01valtti;
115700101228           IF  V01valttsi = '-';
115800101228             wvaltti = wvaltti * -1;
115900101228           ENDIF;
116000101228           wvalttf = V01valttf;
116100101228           IF  V01valttsf = '-';
116200101228             wvalttf = wvalttf * -1;
116300101228           ENDIF;
116400101228           IF  wvaltti > wvalttf;
116500101228             ErrMessage  = *on;
116600101228             ErrGenerico = *on;
116700101228             PosCurVltti = *on;
116800101228             V01msg      = $Msg(26);
116900101228             leavesr;
117000101228           ENDIF;
117100101228         ENDIF;
117200101228
117300101228         //?Concorrente 1
117400100708         IF  V01conc1 <> *blanks;
117500100708           IF    %scan('?' : V01conc1) > 0;
117600100708             ErrGenerico = *on;
117700100708             PosCurConc1  = *on;
117800100708             clear TIBS02ds;
117900100708             T02mod = 'R';
118000100708             T02cod = 'ITS';
118100100708             T02sif = KNSIF;
118200100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
118300100708             IF  T02err = *blanks;
118400100708               V01conc1 = t02ke2;
118500100708             ENDIF;
118600100708           ENDIF;
118700100708           IF  %lookup(V01conc1 : $Its) = 0;
118800100708             ErrMessage  = *on;
118900100708             ErrGenerico = *on;
119000100708             PosCurConc1 = *on;
119100100708             V01msg = $Msg(09);
119200100708             leavesr;
119300100708           ENDIF;
119400100708         ENDIF;
119500100708         //?Concorrente 2
119600100708         IF  V01conc2 <> *blanks;
119700100708           IF    %scan('?' : V01conc2) > 0;
119800100708             ErrGenerico = *on;
119900100708             PosCurConc2  = *on;
120000100708             clear TIBS02ds;
120100100708             T02mod = 'R';
120200100708             T02cod = 'ITS';
120300100708             T02sif = KNSIF;
120400100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
120500100708             IF  T02err = *blanks;
120600100708               V01conc2 = t02ke2;
120700100708             ENDIF;
120800100708           ENDIF;
120900100708           IF  %lookup(V01conc2 : $Its) = 0;
121000100708             ErrMessage  = *on;
121100100708             ErrGenerico = *on;
121200100708             PosCurConc2 = *on;
121300100708             V01msg = $Msg(09);
121400100708             leavesr;
121500100708           ENDIF;
121600100708         ENDIF;
121700100708         //?Concorrente 3
121800100708         IF  V01conc3 <> *blanks;
121900100708           IF    %scan('?' : V01conc3) > 0;
122000100708             ErrGenerico = *on;
122100100708             PosCurConc3  = *on;
122200100708             clear TIBS02ds;
122300100708             T02mod = 'R';
122400100708             T02cod = 'ITS';
122500100708             T02sif = KNSIF;
122600100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
122700100708             IF  T02err = *blanks;
122800100708               V01conc3 = t02ke2;
122900100708             ENDIF;
123000100708           ENDIF;
123100100708           IF  %lookup(V01conc3 : $Its) = 0;
123200100708             ErrMessage  = *on;
123300100708             ErrGenerico = *on;
123400100708             PosCurConc3 = *on;
123500100708             V01msg = $Msg(09);
123600100708             leavesr;
123700100708           ENDIF;
123800100708         ENDIF;
123900100708         //?Concorrente 4
124000100708         IF  V01conc4 <> *blanks;
124100100708           IF    %scan('?' : V01conc4) > 0;
124200100708             ErrGenerico = *on;
124300100708             PosCurConc4  = *on;
124400100708             clear TIBS02ds;
124500100708             T02mod = 'R';
124600100708             T02cod = 'ITS';
124700100708             T02sif = KNSIF;
124800100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
124900100708             IF  T02err = *blanks;
125000100708               V01conc4 = t02ke2;
125100100708             ENDIF;
125200100708           ENDIF;
125300100708           IF  %lookup(V01conc4 : $Its) = 0;
125400100708             ErrMessage  = *on;
125500100708             ErrGenerico = *on;
125600100708             PosCurConc4 = *on;
125700100708             V01msg = $Msg(09);
125800100708             leavesr;
125900100708           ENDIF;
126000100708         ENDIF;
126100100708         //?Concorrente 5
126200100708         IF  V01conc5 <> *blanks;
126300100708           IF    %scan('?' : V01conc5) > 0;
126400100708             ErrGenerico = *on;
126500100708             PosCurConc5  = *on;
126600100708             clear TIBS02ds;
126700100708             T02mod = 'R';
126800100708             T02cod = 'ITS';
126900100708             T02sif = KNSIF;
127000100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
127100100708             IF  T02err = *blanks;
127200100708               V01conc5 = t02ke2;
127300100708             ENDIF;
127400100708           ENDIF;
127500100708           IF  %lookup(V01conc5 : $Its) = 0;
127600100708             ErrMessage  = *on;
127700100708             ErrGenerico = *on;
127800100708             PosCurConc5 = *on;
127900100708             V01msg = $Msg(09);
128000100708             leavesr;
128100100708           ENDIF;
128200100708         ENDIF;
128300100706
128400100708       //?Delta
128500100708         IF  V01deli <> *blanks or V01delf <> *blanks;
128600100708         //?Solo valori numerici
128700100708           IF  V01deli <> *blanks;
128800100708             IF  %check(digits:V01deli) > 0;
128900100708               ErrMessage  = *on;
129000100708               ErrGenerico = *on;
129100100708               PosCurDeli  = *on;
129200100708               V01msg      = $Msg(19);
129300100708               leavesr;
129400100708             ENDIF;
129500100708           ENDIF;
129600100708           IF  V01delf <> *blanks;
129700100708             IF  %check(digits:V01delf) > 0;
129800100708               ErrMessage  = *on;
129900100708               ErrGenerico = *on;
130000100708               PosCurDelf  = *on;
130100100708               V01msg      = $Msg(19);
130200100708               leavesr;
130300100708             ENDIF;
130400100708           ENDIF;
130500100708         //?Obbligatori tutti e 2 se immesso uno dei due
130600100708           IF  V01deli <> *blanks  and  V01delf = *blanks;
130700100708             ErrMessage  = *on;
130800100708             ErrGenerico = *on;
130900100708             PosCurDelf  = *on;
131000100708             V01msg      = $Msg(13);
131100100708             leavesr;
131200100708           ENDIF;
131300100708           IF  V01deli = *blanks  and  V01delf <> *blanks;
131400100708             ErrMessage  = *on;
131500100708             ErrGenerico = *on;
131600100708             PosCurDeli  = *on;
131700100708             V01msg      = $Msg(13);
131800100708             leavesr;
131900100708           ENDIF;
132000100708         //?Controllo congruenza tra "DAL" e "AL"
132100100708           wdeli = %dec(V01deli:3:0);
132200100708           IF  V01delsi = '-';
132300100708             wdeli = wdeli * -1;
132400100708           ENDIF;
132500100708           wdelf = %dec(V01delf:3:0);
132600100708           IF  V01delsf = '-';
132700100708             wdelf = wdelf * -1;
132800100708           ENDIF;
132900100708           IF  wdeli > wdelf;
133000100708             ErrMessage  = *on;
133100100708             ErrGenerico = *on;
133200100708             PosCurDeli  = *on;
133300100708             V01msg      = $Msg(14);
133400100708             leavesr;
133500100708           ENDIF;
133600100708         ENDIF;
133700101228
133800101228       //?Aumento/Sconto
133900101228         IF  V01ausci <> 0 or V01auscf <> 0;
134000101228         //?Obbligatori tutti e 2 se immesso uno dei due
134100101228           IF  V01ausci <> 0  and  V01auscf = 0;
134200101228             ErrMessage  = *on;
134300101228             ErrGenerico = *on;
134400101228             PosCurAuScf = *on;
134500101228             V01msg      = $Msg(27);
134600101228             leavesr;
134700101228           ENDIF;
134800101228           IF  V01ausci = 0  and  V01auscf <> 0;
134900101228             ErrMessage  = *on;
135000101228             ErrGenerico = *on;
135100101228             PosCurAuSci = *on;
135200101228             V01msg      = $Msg(27);
135300101228             leavesr;
135400101228           ENDIF;
135500101228         //?Controllo congruenza tra "DAL" e "AL"
135600101228           wausci = V01ausci;
135700101228           IF  V01auscsi = '-';
135800101228             wausci = wausci * -1;
135900101228           ENDIF;
136000101228           wauscf = V01auscf;
136100101228           IF  V01auscsf = '-';
136200101228             wauscf = wauscf * -1;
136300101228           ENDIF;
136400101228           IF  wausci > wauscf;
136500101228             ErrMessage  = *on;
136600101228             ErrGenerico = *on;
136700101228             PosCurAuSci = *on;
136800101228             V01msg      = $Msg(28);
136900101228             leavesr;
137000101228           ENDIF;
137100101228         ENDIF;
137200100708
137300100708       //?Tipo trattativa
137400100708         IF  V01ttr1 <> *blanks;
137500100708           IF    %scan('?' : V01ttr1) > 0;
137600100708             ErrGenerico = *on;
137700100708             PosCurTtr1  = *on;
137800100708             clear TIBS02ds;
137900100708             T02mod = 'R';
138000100708             T02cod = 'TTR';
138100100708             T02sif = KNSIF;
138200100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
138300100708             IF  T02err = *blanks;
138400100708               V01ttr1 = t02ke1;
138500100708             ENDIF;
138600100708           ENDIF;
138700100708           IF  %lookup(V01ttr1 : $Ttr) = 0;
138800100708             ErrMessage  = *on;
138900100708             ErrGenerico = *on;
139000100708             PosCurTtr1  = *on;
139100100708             V01msg = $Msg(10);
139200100708             leavesr;
139300100708           ENDIF;
139400100708         ENDIF;
139500100708         IF  V01ttr2 <> *blanks;
139600100708           IF    %scan('?' : V01ttr2) > 0;
139700100708             ErrGenerico = *on;
139800100708             PosCurTtr2  = *on;
139900100708             clear TIBS02ds;
140000100708             T02mod = 'R';
140100100708             T02cod = 'TTR';
140200100708             T02sif = KNSIF;
140300100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
140400100708             IF  T02err = *blanks;
140500100708               V01ttr2 = t02ke1;
140600100708             ENDIF;
140700100708           ENDIF;
140800100708           IF  %lookup(V01ttr2 : $Ttr) = 0;
140900100708             ErrMessage  = *on;
141000100708             ErrGenerico = *on;
141100100708             PosCurTtr2  = *on;
141200100708             V01msg = $Msg(10);
141300100708             leavesr;
141400100708           ENDIF;
141500100708         ENDIF;
141600100708         IF  V01ttr3 <> *blanks;
141700100708           IF    %scan('?' : V01ttr3) > 0;
141800100708             ErrGenerico = *on;
141900100708             PosCurTtr3  = *on;
142000100708             clear TIBS02ds;
142100100708             T02mod = 'R';
142200100708             T02cod = 'TTR';
142300100708             T02sif = KNSIF;
142400100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
142500100708             IF  T02err = *blanks;
142600100708               V01ttr3 = t02ke1;
142700100708             ENDIF;
142800100708           ENDIF;
142900100708           IF  %lookup(V01ttr3 : $Ttr) = 0;
143000100708             ErrMessage  = *on;
143100100708             ErrGenerico = *on;
143200100708             PosCurTtr3  = *on;
143300100708             V01msg = $Msg(10);
143400100708             leavesr;
143500100708           ENDIF;
143600100708         ENDIF;
143700100708         IF  V01ttr4 <> *blanks;
143800100708           IF    %scan('?' : V01ttr4) > 0;
143900100708             ErrGenerico = *on;
144000100708             PosCurTtr4  = *on;
144100100708             clear TIBS02ds;
144200100708             T02mod = 'R';
144300100708             T02cod = 'TTR';
144400100708             T02sif = KNSIF;
144500100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
144600100708             IF  T02err = *blanks;
144700100708               V01ttr4 = t02ke1;
144800100708             ENDIF;
144900100708           ENDIF;
145000100708           IF  %lookup(V01ttr4 : $Ttr) = 0;
145100100708             ErrMessage  = *on;
145200100708             ErrGenerico = *on;
145300100708             PosCurTtr4  = *on;
145400100708             V01msg = $Msg(10);
145500100708             leavesr;
145600100708           ENDIF;
145700100708         ENDIF;
145800101228         IF  V01ttr5 <> *blanks;
145900101228           IF    %scan('?' : V01ttr5) > 0;
146000101228             ErrGenerico = *on;
146100101228             PosCurTtr5  = *on;
146200101228             clear TIBS02ds;
146300101228             T02mod = 'R';
146400101228             T02cod = 'TTR';
146500101228             T02sif = KNSIF;
146600101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
146700101228             IF  T02err = *blanks;
146800101228               V01ttr5 = t02ke1;
146900101228             ENDIF;
147000101228           ENDIF;
147100101228           IF  %lookup(V01ttr5 : $Ttr) = 0;
147200101228             ErrMessage  = *on;
147300101228             ErrGenerico = *on;
147400101228             PosCurTtr5  = *on;
147500101228             V01msg = $Msg(10);
147600101228             leavesr;
147700101228           ENDIF;
147800101228         ENDIF;
147900101228         IF  V01ttr6 <> *blanks;
148000101228           IF    %scan('?' : V01ttr6) > 0;
148100101228             ErrGenerico = *on;
148200101228             PosCurTtr6  = *on;
148300101228             clear TIBS02ds;
148400101228             T02mod = 'R';
148500101228             T02cod = 'TTR';
148600101228             T02sif = KNSIF;
148700101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
148800101228             IF  T02err = *blanks;
148900101228               V01ttr6 = t02ke1;
149000101228             ENDIF;
149100101228           ENDIF;
149200101228           IF  %lookup(V01ttr6 : $Ttr) = 0;
149300101228             ErrMessage  = *on;
149400101228             ErrGenerico = *on;
149500101228             PosCurTtr6  = *on;
149600101228             V01msg = $Msg(10);
149700101228             leavesr;
149800101228           ENDIF;
149900101228         ENDIF;
150000090318
150100100708       //?Tipo cliente
150200100708         IF  V01tic1 <> *blanks;
150300100708           IF    %scan('?' : V01tic1) > 0;
150400100708             ErrGenerico = *on;
150500100708             PosCurTic1  = *on;
150600100708             idTabella = 'IC';
150700100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
150800100708                            tastoUscita);
150900100708             V01tic1 = idElemento;
151000100708           ENDIF;
151100100708           IF  %lookup(V01tic1 : $Ic) = 0;
151200100708             ErrMessage  = *on;
151300100708             ErrGenerico = *on;
151400100708             PosCurTic1  = *on;
151500100708             V01msg = $Msg(17);
151600100708             leavesr;
151700100708           ENDIF;
151800100708         ENDIF;
151900100708         IF  V01tic2 <> *blanks;
152000100708           IF    %scan('?' : V01tic2) > 0;
152100100708             ErrGenerico = *on;
152200100708             PosCurTic2  = *on;
152300100708             idTabella = 'IC';
152400100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
152500100708                            tastoUscita);
152600100708             V01tic2 = idElemento;
152700100708           ENDIF;
152800100708           IF  %lookup(V01tic2 : $Ic) = 0;
152900100708             ErrMessage  = *on;
153000100708             ErrGenerico = *on;
153100100708             PosCurTic2  = *on;
153200100708             V01msg = $Msg(17);
153300100708             leavesr;
153400100708           ENDIF;
153500100708         ENDIF;
153600100708         IF  V01tic3 <> *blanks;
153700100708           IF    %scan('?' : V01tic3) > 0;
153800100708             ErrGenerico = *on;
153900100708             PosCurTic3  = *on;
154000100708             idTabella = 'IC';
154100100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
154200100708                            tastoUscita);
154300100708             V01tic3 = idElemento;
154400100708           ENDIF;
154500100708           IF  %lookup(V01tic3 : $Ic) = 0;
154600100708             ErrMessage  = *on;
154700100708             ErrGenerico = *on;
154800100708             PosCurTic3  = *on;
154900100708             V01msg = $Msg(17);
155000100708             leavesr;
155100100708           ENDIF;
155200100708         ENDIF;
155300100708         IF  V01tic4 <> *blanks;
155400100708           IF    %scan('?' : V01tic4) > 0;
155500100708             ErrGenerico = *on;
155600100708             PosCurTic4  = *on;
155700100708             idTabella = 'IC';
155800100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
155900100708                            tastoUscita);
156000100708             V01tic4 = idElemento;
156100100708           ENDIF;
156200100708           IF  %lookup(V01tic4 : $Ic) = 0;
156300100708             ErrMessage  = *on;
156400100708             ErrGenerico = *on;
156500100708             PosCurTic4  = *on;
156600100708             V01msg = $Msg(17);
156700100708             leavesr;
156800100708           ENDIF;
156900100708         ENDIF;
157000100708         IF  V01tic5 <> *blanks;
157100100708           IF    %scan('?' : V01tic5) > 0;
157200100708             ErrGenerico = *on;
157300100708             PosCurTic5  = *on;
157400100708             idTabella = 'IC';
157500100708             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
157600100708                            tastoUscita);
157700100708             V01tic5 = idElemento;
157800100708           ENDIF;
157900100708           IF  %lookup(V01tic5 : $Ic) = 0;
158000100708             ErrMessage  = *on;
158100100708             ErrGenerico = *on;
158200100708             PosCurTic5  = *on;
158300100708             V01msg = $Msg(17);
158400100708             leavesr;
158500100708           ENDIF;
158600100708         ENDIF;
158700100709
158800100709       //?Trattative calde e data presunto affidamento
158900100709       //?Possibile solo se richeiste le trattative in corso
159000100709         IF  V01calde = 'S' and V01dtica = 0;
159100100709           ErrMessage  = *on;
159200100709           ErrGenerico = *on;
159300100709           PosCurCalde = *on;
159400100709           V01msg      = $Msg(15);
159500100709           leavesr;
159600100709         ENDIF;
159700100709       //?Se inserita la data presunto affidamento devo richiedere le calde
159800100709         IF  V01calde = ' ' and V01dtaff > 0;
159900100709           ErrMessage  = *on;
160000100709           ErrGenerico = *on;
160100100709           PosCurDtaff = *on;
160200100709           V01msg      = $Msg(16);
160300100709           leavesr;
160400100709         ENDIF;
160500100709       //?Controllo la data presunto affidamento
160600100709         IF  V01dtaff <> 0;
160700100709           clear wlbdat;
160800100709           G02dat = V01dtaff;
160900100709           xsrda8(wlbdat);
161000100709           IF  G02err = '1';
161100100709             ErrMessage  = *on;
161200100709             ErrGenerico = *on;
161300100709             PosCurDtaff = *on;
161400100709             V01msg      = $Msg(04);
161500100709             leavesr;
161600100709           ENDIF;
161700100709
161800100709           V01dtaff = G02dat;
161900100709           wdtaff   = G02inv;
162000100709
162100100709         //?controllo che la data non sia inferiore al 2000 o superiore al 2039
162200100709           IF  wdtaff < 20000101 or wdtaff > 20391231;
162300100709             ErrMessage  = *on;
162400100709             ErrGenerico = *on;
162500100709             PosCurDtaff = *on;
162600100709             V01msg      = $Msg(04);
162700100709             leavesr;
162800100709           ENDIF;
162900100709         ENDIF;
163000100708
163100100708       //?Esito
163200100708        //?Impostare solo se richieste trattative chiuse
163300100708         IF  V01esi <> *blanks and V01dtchi = 0;
163400100708           ErrMessage  = *on;
163500100708           ErrGenerico = *on;
163600100708           PosCurEsi   = *on;
163700100708           V01msg = $Msg(18);
163800100708           leavesr;
163900100708         ENDIF;
164000100326
164100100708       ENDSR;
164200100708
164300100708       //--------------------------------------------------------------
164400100708       //?Controllo videata D02
164500100708       //--------------------------------------------------------------
164600100708       BEGSR CtrD02;
164700100708
164800100708         WindDspF = IndDspF;
164900100708         reset WindDspFb;
165000100708         IndDspF  = WindDspF;
165100100524
165200100810       //?Stampa dettaglio obbligatorio S/N
165300100810         IF  V02det = *blanks;
165400100810           ErrMessage  = *on;
165500100810           ErrGenerico = *on;
165600100810           PosCurDet   = *on;
165700100810           V02msg      = $Msg(20);
165800100810           leavesr;
165900100810         ENDIF;
166000100810
166100100708       //?Almeno una selezione è da fare
166200100708         IF  V02det = 'N'  and  V02cmmt = 'N'  and  V02gent = 'N';
166300100708           ErrMessage  = *on;
166400100708           ErrGenerico = *on;
166500100708           PosCurDet   = *on;
166600100708           V02msg      = $Msg(06);
166700100708           leavesr;
166800100708         ENDIF;
166900100810
167000100810       //?Scelta dei commerciali da stampare incongruente con la prima videata
167100100810         IF  (V02detc = 'I' and V01elcmm = 'E') or
167200100810             (V02detc = 'E' and V01elcmm = 'I');
167300100810           ErrMessage  = *on;
167400100810           ErrGenerico = *on;
167500100810           PosCurDetc  = *on;
167600100810           V02msg      = $Msg(23);
167700100810           leavesr;
167800100810         ENDIF;
167900100810
168000100810       //?Immissione/Omissione trattative obbligatorio
168100100810         IF  V02ttrio = *blanks;
168200100810           ErrMessage  = *on;
168300100810           ErrGenerico = *on;
168400100810           PosCurTtrio = *on;
168500100810           V02msg = $Msg(21);
168600100810           leavesr;
168700100810         ENDIF;
168800100708
168900100708       //?Stampa dettaglio scegliere le trattative da stampare
169000100708         IF  V02det = 'S' and V02ttrio = 'I' and
169100100708             V02ttr = *blanks;
169200100708           ErrMessage  = *on;
169300100708           ErrGenerico = *on;
169400100708           PosCurTtr1  = *on;
169500100708           V02msg = $Msg(12);
169600100708           leavesr;
169700100708         ENDIF;
169800100708
169900100708       //?Tipi trattativa da Include/Omettere
170000100708         IF  V02ttr1 <> *blanks;
170100100708           IF    %scan('?' : V02ttr1) > 0;
170200100708             ErrGenerico = *on;
170300100708             PosCurTtr1  = *on;
170400100708             clear TIBS02ds;
170500100708             T02mod = 'R';
170600100708             T02cod = 'TTR';
170700100708             T02sif = KNSIF;
170800100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
170900100708             IF  T02err = *blanks;
171000100708               V02ttr1 = t02ke1;
171100100708             ENDIF;
171200100708           ENDIF;
171300100708           IF  %lookup(V02ttr1 : $Ttr) = 0;
171400100708             ErrMessage  = *on;
171500100708             ErrGenerico = *on;
171600100708             PosCurTtr1  = *on;
171700100708             V02msg = $Msg(10);
171800100708             leavesr;
171900100708           ENDIF;
172000100708         ENDIF;
172100100708         IF  V02ttr2 <> *blanks;
172200100708           IF    %scan('?' : V02ttr2) > 0;
172300100708             ErrGenerico = *on;
172400100708             PosCurTtr2  = *on;
172500100708             clear TIBS02ds;
172600100708             T02mod = 'R';
172700100708             T02cod = 'TTR';
172800100708             T02sif = KNSIF;
172900100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
173000100708             IF  T02err = *blanks;
173100100708               V02ttr2 = t02ke1;
173200100708             ENDIF;
173300100708           ENDIF;
173400100708           IF  %lookup(V02ttr2 : $Ttr) = 0;
173500100708             ErrMessage  = *on;
173600100708             ErrGenerico = *on;
173700100708             PosCurTtr2  = *on;
173800100708             V02msg = $Msg(10);
173900100708             leavesr;
174000100708           ENDIF;
174100100708         ENDIF;
174200100708         IF  V02ttr3 <> *blanks;
174300100708           IF    %scan('?' : V02ttr3) > 0;
174400100708             ErrGenerico = *on;
174500100708             PosCurTtr3  = *on;
174600100708             clear TIBS02ds;
174700100708             T02mod = 'R';
174800100708             T02cod = 'TTR';
174900100708             T02sif = KNSIF;
175000100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
175100100708             IF  T02err = *blanks;
175200100708               V02ttr3 = t02ke1;
175300100708             ENDIF;
175400100708           ENDIF;
175500100708           IF  %lookup(V02ttr3 : $Ttr) = 0;
175600100708             ErrMessage  = *on;
175700100708             ErrGenerico = *on;
175800100708             PosCurTtr3  = *on;
175900100708             V02msg = $Msg(10);
176000100708             leavesr;
176100100708           ENDIF;
176200100708         ENDIF;
176300100708         IF  V02ttr4 <> *blanks;
176400100708           IF    %scan('?' : V02ttr4) > 0;
176500100708             ErrGenerico = *on;
176600100708             PosCurTtr4  = *on;
176700100708             clear TIBS02ds;
176800100708             T02mod = 'R';
176900100708             T02cod = 'TTR';
177000100708             T02sif = KNSIF;
177100100708             TNTBE_RicercaControllo  (kpjba : tibs02ds);
177200100708             IF  T02err = *blanks;
177300100708               V02ttr4 = t02ke1;
177400100708             ENDIF;
177500100708           ENDIF;
177600100708           IF  %lookup(V02ttr4 : $Ttr) = 0;
177700100708             ErrMessage  = *on;
177800100708             ErrGenerico = *on;
177900100708             PosCurTtr4  = *on;
178000100708             V02msg = $Msg(10);
178100100708             leavesr;
178200100708           ENDIF;
178300100708         ENDIF;
178400101228         IF  V02ttr5 <> *blanks;
178500101228           IF    %scan('?' : V02ttr5) > 0;
178600101228             ErrGenerico = *on;
178700101228             PosCurTtr5  = *on;
178800101228             clear TIBS02ds;
178900101228             T02mod = 'R';
179000101228             T02cod = 'TTR';
179100101228             T02sif = KNSIF;
179200101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
179300101228             IF  T02err = *blanks;
179400101228               V02ttr5 = t02ke1;
179500101228             ENDIF;
179600101228           ENDIF;
179700101228           IF  %lookup(V02ttr5 : $Ttr) = 0;
179800101228             ErrMessage  = *on;
179900101228             ErrGenerico = *on;
180000101228             PosCurTtr5  = *on;
180100101228             V02msg = $Msg(10);
180200101228             leavesr;
180300101228           ENDIF;
180400101228         ENDIF;
180500101228         IF  V02ttr6 <> *blanks;
180600101228           IF    %scan('?' : V02ttr6) > 0;
180700101228             ErrGenerico = *on;
180800101228             PosCurTtr6  = *on;
180900101228             clear TIBS02ds;
181000101228             T02mod = 'R';
181100101228             T02cod = 'TTR';
181200101228             T02sif = KNSIF;
181300101228             TNTBE_RicercaControllo  (kpjba : tibs02ds);
181400101228             IF  T02err = *blanks;
181500101228               V02ttr6 = t02ke1;
181600101228             ENDIF;
181700101228           ENDIF;
181800101228           IF  %lookup(V02ttr6 : $Ttr) = 0;
181900101228             ErrMessage  = *on;
182000101228             ErrGenerico = *on;
182100101228             PosCurTtr6  = *on;
182200101228             V02msg = $Msg(10);
182300101228             leavesr;
182400101228           ENDIF;
182500101228         ENDIF;
182600100810
182700100810       //?Ordinamento obbligatorio
182800100810         IF  V02ord = *blanks;
182900100810           ErrMessage  = *on;
183000100810           ErrGenerico = *on;
183100100810           PosCurOrd   = *on;
183200100810           V02msg = $Msg(22);
183300100810           leavesr;
183400100810         ENDIF;
183500100524
183600100528       //?Stampa generale INUTILE se richiesto 1 solo commerciale
183700100708         IF  V02gent = 'S'  and  V01cmm > *zero;
183800100601           ErrMessage  = *on;
183900100601           ErrGenerico = *on;
184000100708           PosCurGent  = *on;
184100100708           V02msg = $Msg(08);
184200100524           leavesr;
184300100524         ENDIF;
184400100601
184500100708       //?Stampa generale OBBLIGATORIA se richiesti tutti i commerciali
184600100708         IF  V02gent = 'N'  and  V01cmm <= *zero;
184700100601           ErrMessage  = *on;
184800100601           ErrGenerico = *on;
184900100708           PosCurGent  = *on;
185000100708           V02msg = $Msg(11);
185100100601           leavesr;
185200100601         ENDIF;
185300090717
185400080206       ENDSR;
185500080206
185600080206       //--------------------------------------------------------------
185700080206       //?Operazioni finali.
185800080206       //--------------------------------------------------------------
185900080206       BEGSR RoutEnd;
186000080206
186100080206         *inLR = *on;
186200080206         return;
186300080206
186400080206       ENDSR;
186500080206
186600080206      /end-free
186700100329
186800080206       //--------------------------------------------------------------
186900080206       //?Schiere a tempo di compilazione.
187000080206       //--------------------------------------------------------------
187100080206
187200080206** - $MSG -------------------------------------------------------------------*
187300100329Utente non abilitato alla funzione                                            01
187400100329Codice commerciale errato o non in gestione all'utente                        02
187500100329Richiedere solo commerciali UNIFICANTI!                                       03
187600100329Data errata                                                                   04
187700100329Data "DAL" incongruente con data "AL"                                         05
187800100524Selezionare almeno una stampa                                                 06
187900100601Selezionare i commerciali da stampare                                         07
188000100528Non richiedere il totale generale per la stampa di un solo comm.unificante    08
188100100706Concorrente errato                                                            09
188200100531Tipo trattativa errato                                                        10
188300100601Totali generali obbligatori SE NON specificato un commerciale unificante      11
188400100601Specificare cosa includere (oppure omettere nulla per includere tutto)        12
188500100708Immettere il Delta previsto                                                   13
188600101228Delta previsto "DAL" incongruente con Delta previsto "AL"                     14
188700100708Richedere le trattativa CALDE solo se richieste le trattative IN CORSO        15
188800100708Se non richieste la trattative CALDE non immettere la data                    16
188900100708Tipo cliente errato                                                           17
189000100708Richiedere ESITO solo se richieste le trattative CHIUSE                       18
189100100708Delta Previsto errato                                                         19
189200100810Impostare "S" oppure "N"                                                      20
189300100810Impostare "I" oppure "O"                                                      21
189400100810Impostare Ordinamento                                                         22
189500100810Commerciali da stampare incogruenti con i commerciali da elaborare in 1° vid. 23
189600101006Codice commerciale errato.                                                    24
189700101228Immettere il Valore traffico TOTALE                                           25
189800101228Valore traffico TOTALE "DAL" incongruente con Valore traffico TOTALE "AL"     26
189900101228Immettere il Aumento/Sconto                                                   27
190000101228Aumento/Sconto "DAL" incongruente con Aumento/Sconto "AL"                     28
190100110119Codice Area errato o non in gestione all'utente                               29
190200110119Non impostare il commerciale se inserito il codice area                       30
190300110921Selezionare almeno un'area o un commerciale                                   31
