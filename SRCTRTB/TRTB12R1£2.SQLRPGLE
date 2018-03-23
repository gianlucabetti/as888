000100101012      //---------------------------------------------------------------
000200101012      //?TRTB12R1 - Interrogazione/Gestione File scatti ISTAT
000300101012      //---------------------------------------------------------------
000400101012     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500101012
000600101012      //---------------------------------------------------------------
000700101012      //?Dichiarazione file.
000800101012      //---------------------------------------------------------------
000900101012     ftisis01l  uf a e           k disk
001000101012     ftrtb12d1  cf   e             workstn sfile(tb12s01:S01nrr)
001100101012     f                                     indds(IndDspF)
001200101012     f                                     infds(InfDspF)
001300101012
001400101012      //---------------------------------------------------------------
001500101012      //?Definizione costanti.
001600101012      //---------------------------------------------------------------
001700101012
001800101012      // - Tasti funzionali a video
001900101012     d c_F01           c                   const(x'31')
002000101012     d c_F02           c                   const(x'32')
002100101012     d c_F03           c                   const(x'33')
002200101012     d c_F04           c                   const(x'34')
002300101012     d c_F05           c                   const(x'35')
002400101012     d c_F06           c                   const(x'36')
002500101012     d c_F07           c                   const(x'37')
002600101012     d c_F08           c                   const(x'38')
002700101012     d c_F09           c                   const(x'39')
002800101012     d c_F10           c                   const(x'3A')
002900101012     d c_F11           c                   const(x'3B')
003000101012     d c_F12           c                   const(x'3C')
003100101012     d c_F13           c                   const(x'B1')
003200101012     d c_F14           c                   const(x'B2')
003300101012     d c_F15           c                   const(x'B3')
003400101012     d c_F16           c                   const(x'B4')
003500101012     d c_F17           c                   const(x'B5')
003600101012     d c_F18           c                   const(x'B6')
003700101012     d c_F19           c                   const(x'B7')
003800101012     d c_F20           c                   const(x'B8')
003900101012     d c_F21           c                   const(x'B9')
004000101012     d c_F22           c                   const(x'BA')
004100101012     d c_F23           c                   const(x'BB')
004200101012     d c_F24           c                   const(x'BC')
004300101012     d c_Enter         c                   const(x'F1')
004400101012     d c_RollDown      c                   const(x'F4')
004500101012     d c_RollUp        c                   const(x'F5')
004600101012
004700101012     d Digits          c                   const('0123456789')
004800101012
004900101012      //---------------------------------------------------------------
005000101012      //?Definizione schiere.
005100101012      //---------------------------------------------------------------
005200101012
005300101012      // - Messaggi di errore
005400101012     d $Msg            s             78    dim(11) ctdata perrcd(1)
005500101012
005600101012      //---------------------------------------------------------------
005700101012      //?Definizione aree dati.
005800101012      //---------------------------------------------------------------
005900101012
006000101012      // - Dati utente
006100101012     d §AzUte        e ds                  extname(AZUTE00F)
006200101012     d                                     dtaara
006300101012     d §DatiUte      e ds                  extname(dDatiUte)
006400101012     d                                     dtaara
006500101012
006600101012      //---------------------------------------------------------------
006700101012      //?Definizione strutture dati.
006800101012      //---------------------------------------------------------------
006900101012
007000101012      // - Status
007100101012     d Psds           sds
007200101012     d   SDSpgm          *proc
007300101012
007400101012      // - InfDS
007500101012     d InfDspF         ds
007600101012     d  dsp_aid              369    369a
007700101012     d  dsp_rig              370    370
007800101012     d  dsp_col              371    371
007900101012
008000101012      // - Indicatori su DspF
008100101012     d IndDspF         ds
008200101012        // - Abilitazione tasti funzionali
008300101012     d  F06Attivo                     1n   overlay(IndDspF : 06)
008400101012     d  F10Attivo                     1n   overlay(IndDspF : 10)
008500101012        // - Indicatori di modalità video
008600101012     d  $Immissione                   1n   overlay(IndDspF : 25)
008700101012     d  $Modifica                     1n   overlay(IndDspF : 26)
008800101012     d  $Richiamato                   1n   overlay(IndDspF : 27)
008900101012        // - Indicatori di gestione del subfile
009000101012     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009100101012     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009200101012     d  SflEnd                        1n   overlay(IndDspF : 33)
009300101012        // - Indicatori di errore
009400101012     d  ErrMessage                    1n   overlay(IndDspF : 28)
009500101012        // - Indicatori di errore subfile
009600101012     d  PosCurOpz                     1n   overlay(IndDspF : 50)
009700101012     d  PosCurData                    1n   overlay(IndDspF : 51)
009800101012        // - Indicatori di errore generico
009900101012     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010000101012
010100101012      // Campi di comodo per gestione indicatori a video
010200101012     d WindDspF        ds                  inz
010300101012     d   WindDspFa             1     49    inz(*zero)
010400101012     d   WindDspFb            50     99    inz(*zero)
010500101012
010600101012      // - Controllo data
010700101012     d wlbdat          ds                  inz
010800101012     d  g02dat                 1      8  0
010900101012     d  g02inv                 9     16  0
011000101012     d  g02err                17     17
011100101012     d  g02tgi                18     22  0
011200101012
011300101012      // - Parametri ricevuti
011400101012     d KPJBA         e ds
011500101012     d TRTB12DS1     e ds
011600101012
011700101012      // - Reperimento dati utente
011800101012     d TIBS34ds      e ds
011900101012
012000101012      // Abilitazioni utente
012100101012     d dUte01        e ds
012200101012
012300101012      //---------------------------------------------------------------
012400101012      //?Definizione variabili globali.
012500101012      //---------------------------------------------------------------
012600101012
012700101012      // - Flags booleani
012800101012     d $EoF            s               n   inz(*off)
012900101012     d $Fine           s               n   inz(*off)
013000101012     d $InzD01         s               n   inz(*off)
013100101012     d $InzS01         s               n   inz(*on)
013200101012
013300101012      // - Campi associati al video
013400101012     d $Video          s              2    inz('S1')
013500101012     d S01Nrr          s              4  0 inz
013600101012
013700101012      // - Campi di comodo
013800101022     d conta           s              5i 0
013900101012     d ult_S01sca      s                   like(S01sca)
014000101022     d wanno           s              4  0
014100101022     d wanno_Oggi      s              4  0
014200101012     d wdata           s              8  0
014300101022     d wdataa          s              8  0
014400101022     d wdatada         s              8  0
014500101012     d wDate_EUR       s               d   datfmt(*eur)
014600101012     d wDate_ISO       s               d   datfmt(*iso)
014700101022     d wgiorno         s              2  0
014800101022     d wmese           s              2  0
014900101012
015000101012      //---------------------------------------------------------------
015100101012      //?Definizione procedure esterne.
015200101012      //---------------------------------------------------------------
015300101012
015400101012      // - Reperimento dati utente
015500101012     d tibs34r         pr                  extpgm('TIBS34R')
015600101012     d  tibs34ds                           likeds(TIBS34ds)
015700101012
015800101012      //---------------------------------------------------------------
015900101012      //?Definizione prototipi.
016000101012      //---------------------------------------------------------------
016100101012      /copy gaitrasrc/srcprotopr,xsrda8
016200101012
016300101012      //---------------------------------------------------------------
016400101012      //?Definizione key-list.
016500101012      //---------------------------------------------------------------
016600101012
016700101012
016800101012      //---------------------------------------------------------------
016900101012      //?Riepilogo indicatori.
017000101012      //---------------------------------------------------------------
017100101012
017200101012      // 28    : Emissione messaggio di errore a video
017300101012      // 30    : Condiziona SFLDSP
017400101012      // 31    : Condiziona SFLDSPCTL
017500101012      // 32    : Condiziona SFLEND
017600101012      // 50    : Posizionamento Cursore Data
017700101012      // 99    : Generico di Errore
017800101012
017900101012      //---------------------------------------------------------------
018000101012      //?M A I N - L I N E
018100101012      //---------------------------------------------------------------
018200101012
018300101012     c     *Entry        plist
018400101012     c                   parm                    KPJBA
018500101012
018600101012      /free
018700101012
018800101012       //?Operazioni iniziali
018900101012       exsr RoutInz;
019000101012
019100101012       //?Gestione video
019200101012       DOW  $Fine = *off;
019300101012
019400101012         SELECT;
019500101012           WHEN  $Video = 'S1';
019600101012             exsr GesS01;
019700101012           WHEN  $Video = 'D1';
019800101012             exsr GesD01;
019900101012           OTHER;
020000101012             $Fine = *on;
020100101012         ENDSL;
020200101012
020300101012       ENDDO;
020400101012
020500101012       //?Operazioni finali
020600101012       exsr RoutEnd;
020700101012
020800101012       //--------------------------------------------------------------
020900101012       //?Operazioni iniziali.
021000101012       //--------------------------------------------------------------
021100101012       BEGSR RoutInz;
021200101012
021300101012         $video = 'S1';
021400101012         $inzS01 = *on;
021500101012
021600101012         //?Ricezione parametri
021700101012         IF  %subst(kpjbu:1:1) <> *blanks;
021800101012           //?pgm richiamato
021900101012           TRTB12DS1 = kpjbu;
022000101012           $Richiamato = *on;
022100101012         ELSE;
022200101012           //?pgm non richiamato
022300101012           clear TRTB12DS1;
022400101012           $Richiamato = *off;
022500101012         ENDIF;
022600101012
022700101012         //?Reperimento dati job
022800101012         exsr DatiJob;
022900101012
023000101012         //?Impostazione campi "fissi"
023100101012         VTCpgm = SDSpgm;
023200101012
023300101022         //?Anno corrente
023400101022          wdata = %dec(%date());
023500101022          wDate_ISO  = %date(wdata:*iso);
023600101022          wanno_Oggi = %subdt(wDate_ISO:*years);
023700101012
023800101012       ENDSR;
023900101012
024000101012       //--------------------------------------------------------------
024100101012       //?Reperimento Dati del job (Utente/Operativi).
024200101012       //--------------------------------------------------------------
024300101012       BEGSR DatiJob;
024400101012
024500101012         in(E) §AzUte;
024600101012         IF  NOT %error;
024700101012           in(E) §DatiUte;
024800101012         ENDIF;
024900101012         IF  %error or RSut = *blanks;
025000101012           clear TIBS34ds;
025100101012           tibs34r(tibs34ds);
025200101012           in §AzUte;
025300101012           in §DatiUte;
025400101012         ENDIF;
025500101012
025600101012       ENDSR;
025700101012
025800101012       //--------------------------------------------------------------
025900101012       //?Gestione videata S01.
026000101012       //--------------------------------------------------------------
026100101012       BEGSR GesS01;
026200101012
026300101012         //?Inizializzazione videata
026400101012         IF  $InzS01 = *on;
026500101012           exsr InzS01;
026600101012           $InzS01 = *off;
026700101012         ENDIF;
026800101012
026900101012         $EoF = *off;
027000101012         IF  S01nrr > V1Hrcd;
027100101012           V1Hrcd = 1;
027200101012         ENDIF;
027300101012
027400101012         //?Emissione Testata e Piede con tasti funzionali abilitati
027500101012         write TB12P01;
027600101012         write TB12T01;
027700101012
027800101012         //?Emissione videata
027900101012         SELECT;
028000101012           WHEN  $Richiamato and D12opz = '5';
028100101012             write TB12C01;
028200101012             exfmt PROTECT;
028300101012           OTHER;
028400101012             exfmt TB12C01;
028500101012         ENDSL;
028600101012         ErrMessage  = *off;
028700101012         ErrGenerico = *off;
028800101012         clear V1Cmsg;
028900101012
029000101012         SELECT;
029100101012
029200101012           //?F10=Inserimento
029300101012           WHEN  dsp_aid = c_F10;
029400101012             exsr F10S01;
029500101012
029600101012           //?F12=Ritorno
029700101012           WHEN  dsp_aid = c_F12;
029800101012             exsr F12S01;
029900101012
030000101012           //?Enter
030100101012           OTHER;
030200101012           //?- Controllo dati
030300101012             exsr OpzS01;
030400101012             IF  errGenerico;
030500101012               leavesr;
030600101012             ENDIF;
030700101012
030800101012         ENDSL;
030900101012
031000101012       ENDSR;
031100101012
031200101012       //--------------------------------------------------------------
031300101012       //?Inizializzazione videata S01.
031400101012       //--------------------------------------------------------------
031500101012       BEGSR InzS01;
031600101012
031700101012         //?Pulizia subfile
031800101012         exsr PulS01;
031900101012
032000101012         //?Caricamento dei dati da presentare nel sfl
032100101012         $EoF = *off;
032200101012         clear  s01nrr;
032300101012
032400101012         SISsca = 0;
032500101012         setll (SISsca) TISIS01L;
032600101012
032700101012         DOW  not $EoF;
032800101012
032900101012           read TISIS01L;
033000101012
033100101012           IF  %Eof(TISIS01L);
033200101012             $EoF = *on;
033300101012             leave;
033400101012           ENDIF;
033500101012
033600101012           exsr RieS01;
033700101012
033800101012         ENDDO;
033900101012
034000101012         //?Visualizzazione del SFL (se ci sono dati)
034100101012         SflDsp_N = (S01nrr <= *zeros);
034200101012
034300101012         //?Attivazione (eventuale) del SFLEND
034400101012         SflEnd  =  *on;
034500101012
034600101012         //?Impaginazione subfile
034700101012         //?-> forza l'impaginazione sul 1° rec. del subfile
034800101012         if S01nrr > *zero;
034900101012           V1Hrcd  = 1;
035000101012         else;
035100101012           clear V1Hrcd;
035200101012         endif;
035300101012
035400101012         //?Abilito F10-Inserimento
035500101012         F10Attivo = ($Richiamato = *off);
035600101012         //?Disabilito sempre F06-Conferma
035700101012         F06Attivo = *off;
035800101012
035900101012       ENDSR;
036000101012
036100101012       //--------------------------------------------------------------
036200101012       //?Pulizia Subfile S01.
036300101012       //--------------------------------------------------------------
036400101012       BEGSR PulS01;
036500101012
036600101012         //?Pulizia subfile
036700101012         SflDsp_N    = *on;
036800101012         SflDspCtl_N = *on;
036900101012         write  TB12C01;
037000101012         SflDspCtl_N = *off;
037100101012         SflEnd      = *off;
037200101012
037300101012         clear V1Hrcd;
037400101012         clear S01nrr;
037500101012         clear V1Cmsg;
037600101012         errMessage  = *off;
037700101012         errGenerico = *off;
037800101012         $Immissione = *off;
037900101012         $Modifica   = *off;
038000101012
038100101012       ENDSR ;
038200101012
038300101012       //--------------------------------------------------------------
038400101012       //?Caricamento singolo record nel SubFile S01.
038500101012       //--------------------------------------------------------------
038600101012       BEGSR RieS01;
038700101012
038800101012         clear  TB12S01;
038900101012
039000101022         S1Hdat     = SISdata;
039100101012         S01sca     = SISsca;
039200101012         ult_S01sca = SISsca;
039300101012         wDate_ISO  = %date(SISdata:*iso);
039400101022         wDate_EUR  = wDate_ISO;
039500101012         S01data    = %dec(wDate_EUR);
039600101012         S01punti   = SISpunti;
039700101012
039800101012         S01nrr += 1;
039900101012         write TB12S01;
040000101012
040100101012       ENDSR;
040200101012
040300101012       //--------------------------------------------------------------
040400101012       //?Gestione tasto funzionale F10 da videata S01
040500101012       //?F10=Immissione.
040600101012       //--------------------------------------------------------------
040700101012       BEGSR F10S01;
040800101012
040900101012         $video = 'D1';
041000101012         $inzD01 = *on;
041100101012         $Immissione = *on;
041200101012
041300101012       ENDSR;
041400101012
041500101012       //--------------------------------------------------------------
041600101012       //?Gestione tasto funzionale F12 da videata S01
041700101012       //?F12=Ritorno.
041800101012       //--------------------------------------------------------------
041900101012       BEGSR F12S01;
042000101012
042100101012         //?Se pgm richiamato ritorno F12
042200101012         IF  $Richiamato and D12opz = '1';
042300101012           D12f12 = '1';
042400101012         ENDIF;
042500101012
042600101012         //?Chiusura del programma
042700101012         $Fine = *on;
042800101012
042900101012       ENDSR;
043000101012
043100101012       //--------------------------------------------------------------
043200101012       //?Gestione opzioni subfile.
043300101012       //--------------------------------------------------------------
043400101012       BEGSR OpzS01;
043500101012
043600101012         WindDspF  = IndDspF;
043700101012         reset WindDspFb;
043800101012         IndDspF   = WindDspF;
043900101012
044000101012         clear S01Nrr;
044100101012
044200101012         DOW  not $EoF;
044300101012           S01nrr += 1;
044400101012           chain S01nrr TB12S01;
044500101012           IF  not %found;
044600101012             $EoF = *on;
044700101012             leave;
044800101012           ENDIF;
044900101022
045000101022         //?- Estraggo l'anno
045100101022           wanno = %dec(%subst(%editc(s1hdat:'X'):1:4):4:0);
045200101012
045300101012           SELECT;
045400101012
045500101012             //?- Nessuna opzione
045600101012             WHEN  S01opz  = *blank;
045700101012
045800101012             //?- 1 = Scelta
045900101012             WHEN  S01opz  = '1';
046000101012               D12sca   = S01sca;
046100101025               D12data  = S1Hdat;
046200101012               D12punti = S01punti;
046300101012               //?Esco dal programma
046400101012               $Fine = *on;
046500101012               V1Hrcd = S01nrr;
046600101012               update TB12S01;
046700101012               $EoF = *on;
046800101012               leave;
046900101012
047000101012             //?- 2 = Modifica
047100101022             //?possibile solo se anno >= anno in corso
047200101022             WHEN  S01opz  = '2' and wanno >= wanno_Oggi;
047300101012               $video  = 'D1';
047400101012               $InzD01 = *on;
047500101012               $Modifica = *on;
047600101012               V1Hrcd = S01nrr;
047700101012               update TB12S01;
047800101012               $EoF = *on;
047900101012               leave;
048000101012
048100101012             //?- ? = Opzione NON valida
048200101012             OTHER;
048300101012               errMessage  = *on;
048400101012               errGenerico = *on;
048500101012               PosCurOpz   = *on;
048600101012               V1Cmsg = $Msg(01);
048700101012               V1Hrcd = S01nrr;
048800101012               update TB12S01;
048900101012               $EoF = *on;
049000101012               leave;
049100101012
049200101012           ENDSL;
049300101012
049400101012           //?Aggiornamento sfl
049500101012           V1HRcd = S01Nrr;
049600101012           update TB12S01;
049700101012         ENDDO;
049800101012
049900101012       ENDSR;
050000101012
050100101012       //--------------------------------------------------------------
050200101012       //?Gestione videata D01.
050300101012       //--------------------------------------------------------------
050400101012       BEGSR GesD01;
050500101012
050600101012         //?Inizializzazione videata
050700101012         IF  $InzD01 = *on;
050800101012           exsr InzD01;
050900101012           $InzD01 = *off;
051000101012         ENDIF;
051100101012
051200101012         //?Emissione Testata e Piede con tasti funzionali abilitati
051300101012         write TB12P01;
051400101012         write TB12T01;
051500101012
051600101012         //?Emissione videata
051700101012         exfmt TB12D01;
051800101012         ErrMessage  = *off;
051900101012         ErrGenerico = *off;
052000101012         clear V1Cmsg;
052100101012
052200101012         SELECT;
052300101012
052400101012           //?F6=Conferma
052500101012           WHEN  dsp_aid = c_F06;
052600101012             exsr F06D01;
052700101012
052800101012           //?F12=Ritorno
052900101012           WHEN  dsp_aid = c_F12;
053000101012             exsr F12D01;
053100101012
053200101012           //?Enter
053300101012           OTHER;
053400101012           //?- Controllo dati
053500101012             exsr CtrD01;
053600101012             IF  errGenerico;
053700101012               leavesr;
053800101012             ENDIF;
053900101012
054000101012           ENDSL;
054100101012
054200101012       ENDSR;
054300101012
054400101012       //--------------------------------------------------------------
054500101012       //?Inizializzazione videata D01.
054600101012       //--------------------------------------------------------------
054700101012       BEGSR InzD01;
054800101012
054900101012         F10attivo = *off;
055000101012         F06attivo = *on;
055100101012
055200101012         //?Immissione
055300101012         IF  $Immissione;
055400101012           V01sca = ult_S01sca + 1;
055500101012           clear V01data;
055600101012           clear V01punti;
055700101012         ENDIF;
055800101012
055900101012         //?Modifica
056000101012         IF  $Modifica;
056100101012           V01sca   = S01sca;
056200101012           V01data  = S01data;
056300101012           V01punti = S01punti;
056400101012         ENDIF;
056500101012
056600101012       ENDSR;
056700101012
056800101012       //--------------------------------------------------------------
056900101012       //?Gestione tasto funzionale F6 da videata D01
057000101012       //?F6=Conferma.
057100101012       //--------------------------------------------------------------
057200101012       BEGSR F06D01;
057300101012
057400101012         //?Controllo i dati
057500101012         exsr CtrD01;
057600101012         IF  errGenerico;
057700101012           leavesr;
057800101012         ENDIF;
057900101012
058000101012         IF  $Immissione;
058100101012           clear  TISIS000;
058200101012           SISsca   = V01sca;
058300101012           SISdata  = wdata;
058400101012           SISpunti = V01punti;
058500101012           write  TISIS000;
058600101012         ENDIF;
058700101012
058800101012         IF  $Modifica;
058900101012           chain V01sca TISIS01L;
059000101012           IF  %found(TISIS01L);
059100101012             SISpunti = V01punti;
059200101012             update  TISIS000;
059300101012           ENDIF;
059400101012         ENDIF;
059500101012
059600101012         //?Devo ricaricare il subfile
059700101012         $video = 'S1';
059800101012         $inzS01 = *on;
059900101012
060000101012       ENDSR;
060100101012
060200101012       //--------------------------------------------------------------
060300101012       //?Gestione tasto funzionale F12 da videata D01
060400101012       //?F12=Ritorno.
060500101012       //--------------------------------------------------------------
060600101012       BEGSR F12D01;
060700101012
060800101012         //?Torno alla videata precedente
060900101012         $video = 'S1';
061000101012         $inzS01 = *on;
061100101012
061200101012       ENDSR;
061300101012
061400101012       //--------------------------------------------------------------
061500101012       //?Gestione controllo dati videata D01.
061600101012       //--------------------------------------------------------------
061700101012       BEGSR CtrD01;
061800101012
061900101012         WindDspF  = IndDspF;
062000101012         reset WindDspFb;
062100101012         IndDspF   = WindDspF;
062200101012
062300101012         //?Controllo data
062400101012         IF  V01data   = *zeros;
062500101012           ErrMessage  = *on;
062600101012           ErrGenerico = *on;
062700101012           PosCurData  = *on;
062800101012           V1Cmsg      = $Msg(02);
062900101012           leavesr;
063000101012         ENDIF;
063100101012
063200101012         clear wlbdat;
063300101012         g02dat = V01data;
063400101012         xsrda8(wlbdat);
063500101012         IF  g02err    = '1';
063600101012           ErrMessage  = *on;
063700101012           ErrGenerico = *on;
063800101012           PosCurData  = *on;
063900101012           V1Cmsg      = $Msg(02);
064000101012           leavesr;
064100101012         ENDIF;
064200101012
064300101012         V01data = g02dat;
064400101012         wdata   = g02inv;
064500101022
064600101022         //?Estraggo anno - mese - giorno
064700101022           wDate_ISO = %date(wdata);
064800101022           wanno   = %subdt(wDate_ISO:*years);
064900101022           wmese   = %subdt(wDate_ISO:*months);
065000101022           wgiorno = %subdt(wDate_ISO:*days);
065100101022
065200101022         //?Se immissione posso mettere solo date con anno > anno in corso
065300101022         IF  $Immissione and wanno <= wanno_Oggi;
065400101022           ErrMessage  = *on;
065500101022           ErrGenerico = *on;
065600101022           PosCurData  = *on;
065700101022           V1Cmsg      = $Msg(03);
065800101022           V1Cmsg  = %trim(V1Cmsg) + '. Anno già passato o in corso.';
065900101022           leavesr;
066000101022         ENDIF;
066100101022
066200101022         //?Se immissione l'anno non deve essere già presente sul file
066300101022         IF  $Immissione;
066400101022           clear conta;
066500101022           wdatada = %dec(wDate_ISO - %months(wmese - 1)
066600101022                                    - %days(wgiorno - 1));
066700101022           wdataa  = %dec(wDate_ISO + %years(1));
066800101022           exec sql
066900101022           select count(*) into :conta from TISIS00F where
067000101022            SISdata between :wdatada and : wdataa;
067100101022           IF  conta > *zeros;
067200101022           ErrMessage  = *on;
067300101022           ErrGenerico = *on;
067400101022           PosCurData  = *on;
067500101022           V1Cmsg      = $Msg(03);
067600101022           V1Cmsg  = %trim(V1Cmsg) + '. Anno già presente.';
067700101022           leavesr;
067800101022           ENDIF;
067900101022         ENDIF;
068000101012
068100101012       ENDSR;
068200101012
068300101012       //--------------------------------------------------------------
068400101012       //?Operazioni finali.
068500101012       //--------------------------------------------------------------
068600101012       BEGSR RoutEnd;
068700101012
068800101013         kpjbu = TRTB12DS1;
068900101012         *inLR = *on;
069000101012         return;
069100101012
069200101012       ENDSR;
069300101012
069400101012      /end-free
069500101012
069600101012       //--------------------------------------------------------------
069700101012       //?Schiere a tempo di compilazione.
069800101012       //--------------------------------------------------------------
069900101012
070000101012** - $MSG -------------------------------------------------------------------*
070100101022Opzione errata                                                                01
070200101022Immettere la data                                                             02
070300101022Data errata                                                                   03
