000100101012      //---------------------------------------------------------------
000200101012      //?TRTB12R1 - Interrogazione/Gestione File scatti ISTAT
000300101012      //---------------------------------------------------------------
000400101012     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500101012
000600101012      //---------------------------------------------------------------
000700101012      //?Dichiarazione file.
000800101012      //---------------------------------------------------------------
000900101012     ftisis01l  uf a e           k disk
001000170111     ftisis11l  uf a e           k disk
001100101012     ftrtb12d1  cf   e             workstn sfile(tb12s01:S01nrr)
001200101012     f                                     indds(IndDspF)
001300101012     f                                     infds(InfDspF)
001400101012
001500101012      //---------------------------------------------------------------
001600101012      //?Definizione costanti.
001700101012      //---------------------------------------------------------------
001800101012
001900101012      // - Tasti funzionali a video
002000101012     d c_F01           c                   const(x'31')
002100101012     d c_F02           c                   const(x'32')
002200101012     d c_F03           c                   const(x'33')
002300101012     d c_F04           c                   const(x'34')
002400101012     d c_F05           c                   const(x'35')
002500101012     d c_F06           c                   const(x'36')
002600101012     d c_F07           c                   const(x'37')
002700101012     d c_F08           c                   const(x'38')
002800101012     d c_F09           c                   const(x'39')
002900101012     d c_F10           c                   const(x'3A')
003000101012     d c_F11           c                   const(x'3B')
003100101012     d c_F12           c                   const(x'3C')
003200101012     d c_F13           c                   const(x'B1')
003300101012     d c_F14           c                   const(x'B2')
003400101012     d c_F15           c                   const(x'B3')
003500101012     d c_F16           c                   const(x'B4')
003600101012     d c_F17           c                   const(x'B5')
003700101012     d c_F18           c                   const(x'B6')
003800101012     d c_F19           c                   const(x'B7')
003900101012     d c_F20           c                   const(x'B8')
004000101012     d c_F21           c                   const(x'B9')
004100101012     d c_F22           c                   const(x'BA')
004200101012     d c_F23           c                   const(x'BB')
004300101012     d c_F24           c                   const(x'BC')
004400101012     d c_Enter         c                   const(x'F1')
004500101012     d c_RollDown      c                   const(x'F4')
004600101012     d c_RollUp        c                   const(x'F5')
004700101012
004800101012     d Digits          c                   const('0123456789')
004900101012
005000101012      //---------------------------------------------------------------
005100101012      //?Definizione schiere.
005200101012      //---------------------------------------------------------------
005300101012
005400101012      // - Messaggi di errore
005500101012     d $Msg            s             78    dim(11) ctdata perrcd(1)
005600101012
005700101012      //---------------------------------------------------------------
005800101012      //?Definizione aree dati.
005900101012      //---------------------------------------------------------------
006000101012
006100101012      // - Dati utente
006200101012     d §AzUte        e ds                  extname(AZUTE00F)
006300101012     d                                     dtaara
006400101012     d §DatiUte      e ds                  extname(dDatiUte)
006500101012     d                                     dtaara
006600101012
006700101012      //---------------------------------------------------------------
006800101012      //?Definizione strutture dati.
006900101012      //---------------------------------------------------------------
007000101012
007100101012      // - Status
007200101012     d Psds           sds
007300101012     d   SDSpgm          *proc
007400101012
007500101012      // - InfDS
007600101012     d InfDspF         ds
007700101012     d  dsp_aid              369    369a
007800101012     d  dsp_rig              370    370
007900101012     d  dsp_col              371    371
008000101012
008100101012      // - Indicatori su DspF
008200101012     d IndDspF         ds
008300101012        // - Abilitazione tasti funzionali
008400101012     d  F06Attivo                     1n   overlay(IndDspF : 06)
008500101012     d  F10Attivo                     1n   overlay(IndDspF : 10)
008600101012        // - Indicatori di modalità video
008700101012     d  $Immissione                   1n   overlay(IndDspF : 25)
008800101012     d  $Modifica                     1n   overlay(IndDspF : 26)
008900101012     d  $Richiamato                   1n   overlay(IndDspF : 27)
009000101012        // - Indicatori di gestione del subfile
009100101012     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009200101012     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009300101012     d  SflEnd                        1n   overlay(IndDspF : 33)
009400101012        // - Indicatori di errore
009500101012     d  ErrMessage                    1n   overlay(IndDspF : 28)
009600101012        // - Indicatori di errore subfile
009700101012     d  PosCurOpz                     1n   overlay(IndDspF : 50)
009800101012     d  PosCurData                    1n   overlay(IndDspF : 51)
009900101012        // - Indicatori di errore generico
010000101012     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010100101012
010200101012      // Campi di comodo per gestione indicatori a video
010300101012     d WindDspF        ds                  inz
010400101012     d   WindDspFa             1     49    inz(*zero)
010500101012     d   WindDspFb            50     99    inz(*zero)
010600101012
010700101012      // - Controllo data
010800101012     d wlbdat          ds                  inz
010900101012     d  g02dat                 1      8  0
011000101012     d  g02inv                 9     16  0
011100101012     d  g02err                17     17
011200101012     d  g02tgi                18     22  0
011300101012
011400101012      // - Parametri ricevuti
011500101012     d KPJBA         e ds
011600101012     d TRTB12DS1     e ds
011700101012
011800101012      // - Reperimento dati utente
011900101012     d TIBS34ds      e ds
012000101012
012100101012      // Abilitazioni utente
012200101012     d dUte01        e ds
012300101012
012400101012      //---------------------------------------------------------------
012500101012      //?Definizione variabili globali.
012600101012      //---------------------------------------------------------------
012700101012
012800101012      // - Flags booleani
012900101012     d $EoF            s               n   inz(*off)
013000101012     d $Fine           s               n   inz(*off)
013100101012     d $InzD01         s               n   inz(*off)
013200101012     d $InzS01         s               n   inz(*on)
013300101012
013400101012      // - Campi associati al video
013500101012     d $Video          s              2    inz('S1')
013600101012     d S01Nrr          s              4  0 inz
013700101012
013800101012      // - Campi di comodo
013900101022     d conta           s              5i 0
014000101012     d ult_S01sca      s                   like(S01sca)
014100101022     d wanno           s              4  0
014200101022     d wanno_Oggi      s              4  0
014300101012     d wdata           s              8  0
014400170112     d wdata_oggi      s              8  0
014500101022     d wdataa          s              8  0
014600101022     d wdatada         s              8  0
014700101012     d wDate_EUR       s               d   datfmt(*eur)
014800101012     d wDate_ISO       s               d   datfmt(*iso)
014900101022     d wgiorno         s              2  0
015000101022     d wmese           s              2  0
015100101012
015200101012      //---------------------------------------------------------------
015300101012      //?Definizione procedure esterne.
015400101012      //---------------------------------------------------------------
015500101012
015600101012      // - Reperimento dati utente
015700101012     d tibs34r         pr                  extpgm('TIBS34R')
015800101012     d  tibs34ds                           likeds(TIBS34ds)
015900101012
016000101012      //---------------------------------------------------------------
016100101012      //?Definizione prototipi.
016200101012      //---------------------------------------------------------------
016300101012      /copy gaitrasrc/srcprotopr,xsrda8
016400101012
016500101012      //---------------------------------------------------------------
016600101012      //?Definizione key-list.
016700101012      //---------------------------------------------------------------
016800101012
016900101012
017000101012      //---------------------------------------------------------------
017100101012      //?Riepilogo indicatori.
017200101012      //---------------------------------------------------------------
017300101012
017400101012      // 28    : Emissione messaggio di errore a video
017500101012      // 30    : Condiziona SFLDSP
017600101012      // 31    : Condiziona SFLDSPCTL
017700101012      // 32    : Condiziona SFLEND
017800101012      // 50    : Posizionamento Cursore Data
017900101012      // 99    : Generico di Errore
018000101012
018100101012      //---------------------------------------------------------------
018200101012      //?M A I N - L I N E
018300101012      //---------------------------------------------------------------
018400101012
018500101012     c     *Entry        plist
018600101012     c                   parm                    KPJBA
018700101012
018800101012      /free
018900101012
019000101012       //?Operazioni iniziali
019100101012       exsr RoutInz;
019200101012
019300101012       //?Gestione video
019400101012       DOW  $Fine = *off;
019500101012
019600101012         SELECT;
019700101012           WHEN  $Video = 'S1';
019800101012             exsr GesS01;
019900101012           WHEN  $Video = 'D1';
020000101012             exsr GesD01;
020100101012           OTHER;
020200101012             $Fine = *on;
020300101012         ENDSL;
020400101012
020500101012       ENDDO;
020600101012
020700101012       //?Operazioni finali
020800101012       exsr RoutEnd;
020900101012
021000101012       //--------------------------------------------------------------
021100101012       //?Operazioni iniziali.
021200101012       //--------------------------------------------------------------
021300101012       BEGSR RoutInz;
021400101012
021500101012         $video = 'S1';
021600101012         $inzS01 = *on;
021700101012
021800101012         //?Ricezione parametri
021900101012         IF  %subst(kpjbu:1:1) <> *blanks;
022000101012           //?pgm richiamato
022100101012           TRTB12DS1 = kpjbu;
022200101012           $Richiamato = *on;
022300101012         ELSE;
022400101012           //?pgm non richiamato
022500101012           clear TRTB12DS1;
022600101012           $Richiamato = *off;
022700101012         ENDIF;
022800101012
022900101012         //?Reperimento dati job
023000101012         exsr DatiJob;
023100101012
023200101012         //?Impostazione campi "fissi"
023300101012         VTCpgm = SDSpgm;
023400101012
023500101022         //?Anno corrente
023600101022          wdata = %dec(%date());
023700170112          wdata_oggi = %dec(%date());
023800101022          wDate_ISO  = %date(wdata:*iso);
023900101022          wanno_Oggi = %subdt(wDate_ISO:*years);
024000101012
024100101012       ENDSR;
024200101012
024300101012       //--------------------------------------------------------------
024400101012       //?Reperimento Dati del job (Utente/Operativi).
024500101012       //--------------------------------------------------------------
024600101012       BEGSR DatiJob;
024700101012
024800101012         in(E) §AzUte;
024900101012         IF  NOT %error;
025000101012           in(E) §DatiUte;
025100101012         ENDIF;
025200101012         IF  %error or RSut = *blanks;
025300101012           clear TIBS34ds;
025400101012           tibs34r(tibs34ds);
025500101012           in §AzUte;
025600101012           in §DatiUte;
025700101012         ENDIF;
025800101012
025900101012       ENDSR;
026000101012
026100101012       //--------------------------------------------------------------
026200101012       //?Gestione videata S01.
026300101012       //--------------------------------------------------------------
026400101012       BEGSR GesS01;
026500101012
026600101012         //?Inizializzazione videata
026700101012         IF  $InzS01 = *on;
026800101012           exsr InzS01;
026900101012           $InzS01 = *off;
027000101012         ENDIF;
027100101012
027200101012         $EoF = *off;
027300101012         IF  S01nrr > V1Hrcd;
027400101012           V1Hrcd = 1;
027500101012         ENDIF;
027600101012
027700101012         //?Emissione Testata e Piede con tasti funzionali abilitati
027800101012         write TB12P01;
027900101012         write TB12T01;
028000101012
028100101012         //?Emissione videata
028200101012         SELECT;
028300101012           WHEN  $Richiamato and D12opz = '5';
028400101012             write TB12C01;
028500101012             exfmt PROTECT;
028600101012           OTHER;
028700101012             exfmt TB12C01;
028800101012         ENDSL;
028900101012         ErrMessage  = *off;
029000101012         ErrGenerico = *off;
029100101012         clear V1Cmsg;
029200101012
029300101012         SELECT;
029400101012
029500101012           //?F10=Inserimento
029600101012           WHEN  dsp_aid = c_F10;
029700101012             exsr F10S01;
029800101012
029900101012           //?F12=Ritorno
030000101012           WHEN  dsp_aid = c_F12;
030100101012             exsr F12S01;
030200101012
030300101012           //?Enter
030400101012           OTHER;
030500101012           //?- Controllo dati
030600101012             exsr OpzS01;
030700101012             IF  errGenerico;
030800101012               leavesr;
030900101012             ENDIF;
031000101012
031100101012         ENDSL;
031200101012
031300101012       ENDSR;
031400101012
031500101012       //--------------------------------------------------------------
031600101012       //?Inizializzazione videata S01.
031700101012       //--------------------------------------------------------------
031800101012       BEGSR InzS01;
031900101012
032000101012         //?Pulizia subfile
032100101012         exsr PulS01;
032200101012
032300101012         //?Caricamento dei dati da presentare nel sfl
032400101012         $EoF = *off;
032500101012         clear  s01nrr;
032600101012
032700101012         SISsca = 0;
032800101012         setll (SISsca) TISIS01L;
032900101012
033000101012         DOW  not $EoF;
033100101012
033200101012           read TISIS01L;
033300101012
033400101012           IF  %Eof(TISIS01L);
033500101012             $EoF = *on;
033600101012             leave;
033700101012           ENDIF;
033800101012
033900101012           exsr RieS01;
034000101012
034100101012         ENDDO;
034200101012
034300101012         //?Visualizzazione del SFL (se ci sono dati)
034400101012         SflDsp_N = (S01nrr <= *zeros);
034500101012
034600101012         //?Attivazione (eventuale) del SFLEND
034700101012         SflEnd  =  *on;
034800101012
034900101012         //?Impaginazione subfile
035000101012         //?-> forza l'impaginazione sul 1° rec. del subfile
035100101012         if S01nrr > *zero;
035200101012           V1Hrcd  = 1;
035300101012         else;
035400101012           clear V1Hrcd;
035500101012         endif;
035600101012
035700101012         //?Abilito F10-Inserimento
035800101012         F10Attivo = ($Richiamato = *off);
035900101012         //?Disabilito sempre F06-Conferma
036000101012         F06Attivo = *off;
036100101012
036200101012       ENDSR;
036300101012
036400101012       //--------------------------------------------------------------
036500101012       //?Pulizia Subfile S01.
036600101012       //--------------------------------------------------------------
036700101012       BEGSR PulS01;
036800101012
036900101012         //?Pulizia subfile
037000101012         SflDsp_N    = *on;
037100101012         SflDspCtl_N = *on;
037200101012         write  TB12C01;
037300101012         SflDspCtl_N = *off;
037400101012         SflEnd      = *off;
037500101012
037600101012         clear V1Hrcd;
037700101012         clear S01nrr;
037800101012         clear V1Cmsg;
037900101012         errMessage  = *off;
038000101012         errGenerico = *off;
038100101012         $Immissione = *off;
038200101012         $Modifica   = *off;
038300101012
038400101012       ENDSR ;
038500101012
038600101012       //--------------------------------------------------------------
038700101012       //?Caricamento singolo record nel SubFile S01.
038800101012       //--------------------------------------------------------------
038900101012       BEGSR RieS01;
039000101012
039100101012         clear  TB12S01;
039200101012
039300101022         S1Hdat     = SISdata;
039400101012         S01sca     = SISsca;
039500101012         ult_S01sca = SISsca;
039600101012         wDate_ISO  = %date(SISdata:*iso);
039700101022         wDate_EUR  = wDate_ISO;
039800101012         S01data    = %dec(wDate_EUR);
039900101012         S01punti   = SISpunti;
040000101012
040100170216         chain(n) (SISsca) TISIS11L ;
040200170111         If %found(Tisis11l);
040300170111             S1hstato   = SIS1stato;
040400170111         Endif;
040500170111
040600101012         S01nrr += 1;
040700101012         write TB12S01;
040800101012
040900101012       ENDSR;
041000101012
041100101012       //--------------------------------------------------------------
041200101012       //?Gestione tasto funzionale F10 da videata S01
041300101012       //?F10=Immissione.
041400101012       //--------------------------------------------------------------
041500101012       BEGSR F10S01;
041600101012
041700101012         $video = 'D1';
041800101012         $inzD01 = *on;
041900101012         $Immissione = *on;
042000101012
042100101012       ENDSR;
042200101012
042300101012       //--------------------------------------------------------------
042400101012       //?Gestione tasto funzionale F12 da videata S01
042500101012       //?F12=Ritorno.
042600101012       //--------------------------------------------------------------
042700101012       BEGSR F12S01;
042800101012
042900101012         //?Se pgm richiamato ritorno F12
043000101012         IF  $Richiamato and D12opz = '1';
043100101012           D12f12 = '1';
043200101012         ENDIF;
043300101012
043400101012         //?Chiusura del programma
043500101012         $Fine = *on;
043600101012
043700101012       ENDSR;
043800101012
043900101012       //--------------------------------------------------------------
044000101012       //?Gestione opzioni subfile.
044100101012       //--------------------------------------------------------------
044200101012       BEGSR OpzS01;
044300101012
044400101012         WindDspF  = IndDspF;
044500101012         reset WindDspFb;
044600101012         IndDspF   = WindDspF;
044700101012
044800101012         clear S01Nrr;
044900101012
045000101012         DOW  not $EoF;
045100101012           S01nrr += 1;
045200101012           chain S01nrr TB12S01;
045300101012           IF  not %found;
045400101012             $EoF = *on;
045500101012             leave;
045600101012           ENDIF;
045700101022
045800101022         //?- Estraggo l'anno
045900101022           wanno = %dec(%subst(%editc(s1hdat:'X'):1:4):4:0);
046000101012
046100101012           SELECT;
046200101012
046300101012             //?- Nessuna opzione
046400101012             WHEN  S01opz  = *blank;
046500101012
046600101012             //?- 1 = Scelta
046700170216             WHEN  S01opz  = '1' and $Richiamato;
046800101012               D12sca   = S01sca;
046900101025               D12data  = S1Hdat;
047000101012               D12punti = S01punti;
047100101012               //?Esco dal programma
047200101012               $Fine = *on;
047300101012               V1Hrcd = S01nrr;
047400101012               update TB12S01;
047500101012               $EoF = *on;
047600101012               leave;
047700101012
047800101012             //?- 2 = Modifica
047900101022             //?possibile solo se anno >= anno in corso
048000170216             WHEN  S01opz  = '2' and wanno >= wanno_Oggi and
048100170216                   not $Richiamato;
048200101012               $video  = 'D1';
048300101012               $InzD01 = *on;
048400101012               $Modifica = *on;
048500101012               V1Hrcd = S01nrr;
048600101012               update TB12S01;
048700101012               $EoF = *on;
048800101012               leave;
048900101012
049000101012             //?- ? = Opzione NON valida
049100101012             OTHER;
049200101012               errMessage  = *on;
049300101012               errGenerico = *on;
049400101012               PosCurOpz   = *on;
049500101012               V1Cmsg = $Msg(01);
049600101012               V1Hrcd = S01nrr;
049700101012               update TB12S01;
049800101012               $EoF = *on;
049900101012               leave;
050000101012
050100101012           ENDSL;
050200101012
050300101012           //?Aggiornamento sfl
050400101012           V1HRcd = S01Nrr;
050500101012           update TB12S01;
050600101012         ENDDO;
050700101012
050800101012       ENDSR;
050900101012
051000101012       //--------------------------------------------------------------
051100101012       //?Gestione videata D01.
051200101012       //--------------------------------------------------------------
051300101012       BEGSR GesD01;
051400101012
051500101012         //?Inizializzazione videata
051600101012         IF  $InzD01 = *on;
051700101012           exsr InzD01;
051800101012           $InzD01 = *off;
051900101012         ENDIF;
052000101012
052100101012         //?Emissione Testata e Piede con tasti funzionali abilitati
052200101012         write TB12P01;
052300101012         write TB12T01;
052400101012
052500101012         //?Emissione videata
052600101012         exfmt TB12D01;
052700101012         ErrMessage  = *off;
052800101012         ErrGenerico = *off;
052900101012         clear V1Cmsg;
053000101012
053100101012         SELECT;
053200101012
053300101012           //?F6=Conferma
053400101012           WHEN  dsp_aid = c_F06;
053500101012             exsr F06D01;
053600101012
053700101012           //?F12=Ritorno
053800101012           WHEN  dsp_aid = c_F12;
053900101012             exsr F12D01;
054000101012
054100101012           //?Enter
054200101012           OTHER;
054300101012           //?- Controllo dati
054400101012             exsr CtrD01;
054500101012             IF  errGenerico;
054600101012               leavesr;
054700101012             ENDIF;
054800101012
054900101012           ENDSL;
055000101012
055100101012       ENDSR;
055200101012
055300101012       //--------------------------------------------------------------
055400101012       //?Inizializzazione videata D01.
055500101012       //--------------------------------------------------------------
055600101012       BEGSR InzD01;
055700101012
055800101012         F10attivo = *off;
055900101012         F06attivo = *on;
056000101012
056100101012         //?Immissione
056200101012         IF  $Immissione;
056300101012           V01sca = ult_S01sca + 1;
056400101012           clear V01data;
056500101012           clear V01punti;
056600170111           clear V01stato;
056700101012         ENDIF;
056800101012
056900101012         //?Modifica
057000101012         IF  $Modifica;
057100101012           V01sca   = S01sca;
057200101012           V01data  = S01data;
057300101012           V01punti = S01punti;
057400170111           V01stato = S1hstato;
057500101012         ENDIF;
057600101012
057700101012       ENDSR;
057800101012
057900101012       //--------------------------------------------------------------
058000101012       //?Gestione tasto funzionale F6 da videata D01
058100101012       //?F6=Conferma.
058200101012       //--------------------------------------------------------------
058300101012       BEGSR F06D01;
058400101012
058500101012         //?Controllo i dati
058600101012         exsr CtrD01;
058700101012         IF  errGenerico;
058800101012           leavesr;
058900101012         ENDIF;
059000101012
059100101012         IF  $Immissione;
059200101012           clear  TISIS000;
059300101012           SISsca   = V01sca;
059400101012           SISdata  = wdata;
059500101012           SISpunti = V01punti;
059600101012           write  TISIS000;
059700170111           clear  TISIS100;
059800170111           SIS1sca   = V01sca;
059900170112           SIS1data  = wdata_oggi;
060000170111           SIS1stato = V01stato;
060100170111           write  TISIS100;
060200101012         ENDIF;
060300101012
060400101012         IF  $Modifica;
060500101012           chain V01sca TISIS01L;
060600101012           IF  %found(TISIS01L);
060700101012             SISpunti = V01punti;
060800101012             update  TISIS000;
060900101012           ENDIF;
061000170111           chain V01sca TISIS11L;
061100170111           IF  %found(TISIS11L);
061200170111             SIS1stato = V01stato;
061300170112             SIS1data  = wdata_oggi;
061400170111             update  TISIS100;
061500170111           ENDIF;
061600170111           IF  not %found(TISIS11L);
061700170111             SIS1stato = V01stato;
061800170111             SIS1sca   = V01sca;
061900170112             SIS1data  = wdata_oggi;
062000170111             write   TISIS100;
062100170111           ENDIF;
062200101012         ENDIF;
062300101012
062400101012         //?Devo ricaricare il subfile
062500101012         $video = 'S1';
062600101012         $inzS01 = *on;
062700101012
062800101012       ENDSR;
062900101012
063000101012       //--------------------------------------------------------------
063100101012       //?Gestione tasto funzionale F12 da videata D01
063200101012       //?F12=Ritorno.
063300101012       //--------------------------------------------------------------
063400101012       BEGSR F12D01;
063500101012
063600101012         //?Torno alla videata precedente
063700101012         $video = 'S1';
063800101012         $inzS01 = *on;
063900101012
064000101012       ENDSR;
064100101012
064200101012       //--------------------------------------------------------------
064300101012       //?Gestione controllo dati videata D01.
064400101012       //--------------------------------------------------------------
064500101012       BEGSR CtrD01;
064600101012
064700101012         WindDspF  = IndDspF;
064800101012         reset WindDspFb;
064900101012         IndDspF   = WindDspF;
065000101012
065100101012         //?Controllo data
065200101012         IF  V01data   = *zeros;
065300101012           ErrMessage  = *on;
065400101012           ErrGenerico = *on;
065500101012           PosCurData  = *on;
065600101012           V1Cmsg      = $Msg(02);
065700101012           leavesr;
065800101012         ENDIF;
065900101012
066000101012         clear wlbdat;
066100101012         g02dat = V01data;
066200101012         xsrda8(wlbdat);
066300101012         IF  g02err    = '1';
066400101012           ErrMessage  = *on;
066500101012           ErrGenerico = *on;
066600101012           PosCurData  = *on;
066700101012           V1Cmsg      = $Msg(02);
066800101012           leavesr;
066900101012         ENDIF;
067000101012
067100101012         V01data = g02dat;
067200101012         wdata   = g02inv;
067300101022
067400101022         //?Estraggo anno - mese - giorno
067500101022           wDate_ISO = %date(wdata);
067600101022           wanno   = %subdt(wDate_ISO:*years);
067700101022           wmese   = %subdt(wDate_ISO:*months);
067800101022           wgiorno = %subdt(wDate_ISO:*days);
067900101022
068000101022         //?Se immissione posso mettere solo date con anno > anno in corso
068100101022         IF  $Immissione and wanno <= wanno_Oggi;
068200101022           ErrMessage  = *on;
068300101022           ErrGenerico = *on;
068400101022           PosCurData  = *on;
068500101022           V1Cmsg      = $Msg(03);
068600101022           V1Cmsg  = %trim(V1Cmsg) + '. Anno già passato o in corso.';
068700101022           leavesr;
068800101022         ENDIF;
068900101022
069000101022         //?Se immissione l'anno non deve essere già presente sul file
069100101022         IF  $Immissione;
069200101022           clear conta;
069300101022           wdatada = %dec(wDate_ISO - %months(wmese - 1)
069400101022                                    - %days(wgiorno - 1));
069500101022           wdataa  = %dec(wDate_ISO + %years(1));
069600101022           exec sql
069700101022           select count(*) into :conta from TISIS00F where
069800101022            SISdata between :wdatada and : wdataa;
069900101022           IF  conta > *zeros;
070000101022           ErrMessage  = *on;
070100101022           ErrGenerico = *on;
070200101022           PosCurData  = *on;
070300101022           V1Cmsg      = $Msg(03);
070400101022           V1Cmsg  = %trim(V1Cmsg) + '. Anno già presente.';
070500101022           leavesr;
070600101022           ENDIF;
070700101022         ENDIF;
070800101012
070900101012       ENDSR;
071000101012
071100101012       //--------------------------------------------------------------
071200101012       //?Operazioni finali.
071300101012       //--------------------------------------------------------------
071400101012       BEGSR RoutEnd;
071500101012
071600101013         kpjbu = TRTB12DS1;
071700101012         *inLR = *on;
071800101012         return;
071900101012
072000101012       ENDSR;
072100101012
072200101012      /end-free
072300101012
072400101012       //--------------------------------------------------------------
072500101012       //?Schiere a tempo di compilazione.
072600101012       //--------------------------------------------------------------
072700101012
072800101012** - $MSG -------------------------------------------------------------------*
072900101022Opzione errata                                                                01
073000101022Immettere la data                                                             02
073100101022Data errata                                                                   03
