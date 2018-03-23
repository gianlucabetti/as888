000100130327       //==============================================================
000200130802       //?TNSB30R - Rigenerazione immagini LDV per AUTISTA
000300130327       //==============================================================
000400130326
000500130326       //--------------------------------------------------------------
000600130326       //?Specifiche di controllo.                                     ?
000700130326       //--------------------------------------------------------------
000800130326
000900130326     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000130326
001100130326       //--------------------------------------------------------------
001200130326       //?Dichiarazione file.                                          ?
001300130326       //--------------------------------------------------------------
001400130709
001500130709       // -?File Organigramma
001600130709     fAZORG01L  if   e           k disk
001700130805
001800130805       // -?File Anagrafica AUT
001900130805     fFIAPD01L  if   e           k disk
002000131004     ftabel00f  if   e           k disk
002100130710
002200130326       // -?File video
002300130802     fTNSB30D   cf   e             workstn
002400130802     f                                     sfile(SB3001S:S01nrr)
002500130326     f                                     indds(IndDspF)
002600130326     f                                     infds(InfDspF)
002700130326
002800130326       //--------------------------------------------------------------
002900130326       //?Definizione costanti.                                        ?
003000130326       //--------------------------------------------------------------
003100130326
003200130326       // -?Tasti funzionali a video
003300130326     d c_F01           c                   const(x'31')
003400130326     d c_F02           c                   const(x'32')
003500130326     d c_F03           c                   const(x'33')
003600130326     d c_F04           c                   const(x'34')
003700130326     d c_F05           c                   const(x'35')
003800130326     d c_F06           c                   const(x'36')
003900130326     d c_F07           c                   const(x'37')
004000130326     d c_F08           c                   const(x'38')
004100130326     d c_F09           c                   const(x'39')
004200130326     d c_F10           c                   const(x'3A')
004300130326     d c_F11           c                   const(x'3B')
004400130326     d c_F12           c                   const(x'3C')
004500130326     d c_F13           c                   const(x'B1')
004600130326     d c_F14           c                   const(x'B2')
004700130326     d c_F15           c                   const(x'B3')
004800130326     d c_F16           c                   const(x'B4')
004900130326     d c_F17           c                   const(x'B5')
005000130326     d c_F18           c                   const(x'B6')
005100130326     d c_F19           c                   const(x'B7')
005200130326     d c_F20           c                   const(x'B8')
005300130326     d c_F21           c                   const(x'B9')
005400130326     d c_F22           c                   const(x'BA')
005500130326     d c_F23           c                   const(x'BB')
005600130326     d c_F24           c                   const(x'BC')
005700130326     d c_Enter         c                   const(x'F1')
005800130326     d c_RollDown      c                   const(x'F4')
005900130326     d c_RollUp        c                   const(x'F5')
006000130326
006100130326     d Digits          c                   const('0123456789')
006200131004     d ds$3          e ds                  inz
006300131004     d WdateISO        s               d   datfmt(*ISO) inz
006400131004     d dp0             s                   like(§$3dp0)
006500131004     d d10             s                   like(§$3d10)
006600131004     d d00             s                   like(§$3d00)
006700130326
006800130326       //--------------------------------------------------------------
006900130326       //?Definizione schiere.                                         ?
007000130326       //--------------------------------------------------------------
007100130326
007200130326      // -?Messaggi di errore
007300130328     d skMSG           s             78    dim(50) ctdata perrcd(1)
007400130326
007500130326       //--------------------------------------------------------------
007600130326       //?Definizione aree dati.                                       ?
007700130326       //--------------------------------------------------------------
007800130326
007900130326       // -?Dati utente?
008000130326     d §AzUte        e ds                  extname(AZUTE00F)
008100130326     d                                     dtaara
008200130326     d §DatiUte      e ds                  extname(dDatiUte)
008300130326     d                                     dtaara
008400130326
008500130326       //--------------------------------------------------------------
008600130326       //?Definizione strutture dati.                                  ?
008700130326       //--------------------------------------------------------------
008800130326
008900130326       // -?Status ds?
009000130326     d Status         sds
009100130326     d  SDSpgm           *proc
009200130326     d  SDSusr               254    263
009300130326
009400130326       // -?InfDS
009500130326     d InfDspF         ds
009600130326     d  dsp_aid              369    369a
009700130327     d  sfl_rrn              376    377i 0
009800130327     d  min_nrr              378    379i 0
009900130327     d  num_rcds             380    381i 0
010000130326
010100130326       // -?Indicatori su DspF
010200130326     d IndDspF         ds
010300130327       // -?Indicatori di visualizzazione Errore
010400130326     d  ErrMessage                    1n   overlay(IndDspF : 28)
010500130328       // -?Indicatori di gestione del subfile
010600130709     d  SflDsp                        1n   overlay(IndDspF : 30)
010700130709     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010800130709     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010900131008     d  SflEnd                        1n   overlay(IndDspF : 33)
011000130327       // -?Indicatori di Errore
011100130802     d  PosCurDTD                     1n   overlay(IndDspF : 50)
011200130802     d  PosCurDTA                     1n   overlay(IndDspF : 51)
011300130802     d  PosCurPDR1                    1n   overlay(IndDspF : 52)
011400130802     d  PosCurPDR2                    1n   overlay(IndDspF : 53)
011500130326       // -?Indicatori di errore generico
011600130326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011700130326
011800130326       // -?DS indicatori Dspf
011900130326     d WindDspF        ds                  inz
012000130326     d   WindDspFa             1     49    inz(*zero)
012100130326     d   WindDspFb            50     99    inz(*zero)
012200130805
012300130805       // -?DS controllo data
012400130805     d wlbdat          ds                  inz
012500130805     d  g02dat                 1      8  0
012600130805     d  g02inv                 9     16  0
012700130805     d  g02err                17     17
012800130805     d  g02tgi                18     22  0
012900130805
013000130805       // -?DS codice AUT
013100130805     d                 ds
013200130805     d  V01pdr1                1      3  0
013300130805     d  V01pdr2                4      7
013400130805     d  V01pdr                 1      7
013500130326
013600130326       // -?Parametri ricevuti
013700130326     d KPJBA         e ds
013800130805
013900130805       // -?Parametri per Ricerca codice AUT
014000130805     d FNLV24DS      e ds
014100130326
014200130326       // -?Parametri per Reperimento dati utente?
014300130326     d TIBS34DS      e ds
014400130805
014500130805       // -?Parametri per Elaborazione immagini  ?
014600130805     d TNSB30DS      e ds
014700130710
014800130326       //--------------------------------------------------------------
014900130326       //?Definizione variabili globali.                               ?
015000130326       //--------------------------------------------------------------
015100130326
015200130326       // -?Flags booleani
015300130328     d wEnd            s               n   inz(*off)
015400130326     d wErrGrave       s               n   inz(*off)
015500130326     d wFine           s               n   inz(*off)
015600130802     d wInzS01         s               n   inz(*on)
015700130328
015800130328       // -?Indici di schiera
015900130328     d xx              s              4  0 inz
016000130326
016100130326       // -?Campi di Comodo
016200130802     d S01nrr          s              4  0 inz
016300130805     d wdtd            s              8  0 inz
016400130805     d wdta            s              8  0 inz
016500131008     d wMaxNrr         s              2  0 inz(10)
016600130805     d wOggi           s              8  0 inz
016700130805     d wProg           s              2  0 inz
016800130802     d wVideo          s              2    inz('S1')
016900130326
017000130326       //--------------------------------------------------------------
017100130326       //?Definizione procedure esterne.                     ?
017200130326       //--------------------------------------------------------------
017300130805
017400130805       // -?Sottomissione lavoro batch
017500130805     d bch10           pr                  extpgm('BCH10')
017600130805     d  kpjba                              likeds(KPJBA)
017700130710
017800130802       // -?Interrogazione AUTISTI
017900130802     d fnlv24r         pr                  extpgm('FNLV24R')
018000130710     d  kpjba                              likeds(KPJBA)
018100130805
018200130805       // -?Richiamao diretto lavoro batch
018300130805     d tnsb31r         pr                  extpgm('TNSB31R')
018400130805     d  kpjba                              likeds(KPJBA)
018500130326
018600130326       //--------------------------------------------------------------
018700130326       //?Definizione prototipi.
018800130326       //--------------------------------------------------------------
018900130328
019000130328       // -?Reperimento dati utente?
019100130326      /copy gaitrasrc/srcprotopr,tibs34r
019200130805
019300130805       // -?Controllo data
019400130805      /copy gaitrasrc/srcprotopr,xsrda8
019500130326
019600130326       //--------------------------------------------------------------
019700130326       //?Definizione key-list.                                        ?
019800130326       //--------------------------------------------------------------
019900130326
020000130326       //--------------------------------------------------------------
020100130326       //?Indicatori.
020200130326       //--------------------------------------------------------------
020300130327       // IN   =
020400130326
020500130326       //--------------------------------------------------------------
020600130326       //?M A I N - L I N E                                            ?
020700130326       //--------------------------------------------------------------
020800130326
020900130326     c     *Entry        plist
021000130326     c                   parm                    KPJBA
021100131004     c*
021200131004     c     Ktab          klist
021300131004     c                   kfld                    tblkut
021400131004     c                   kfld                    tblcod
021500131004     c                   kfld                    tblkey
021600131004     c                   eval      tblkut = 1
021700131004     c                   eval      tblcod = '$3'
021800131004     c                   eval      tblkey = '1'
021900131004     c*
022000131004     c                   clear                   ds$3
022100131004     c     ktab          chain     tabel00f
022200131004     c                   if        %found(tabel00f)
022300131004     c                   movel(p)  tbluni        ds$3
022400131004     c                   end
022500131004     c* date a cavallo di due archivi : errore in fase di lancio
022600131004     c                   movel     §$3dp0        wdateiso
022700131004     c                   adddur    1:*d          wdateiso
022800131004     c                   movel     wdateiso      dp0
022900131004     c                   movel     §$3d10        wdateiso
023000131004     c                   adddur    1:*d          wdateiso
023100131004     c                   movel     wdateiso      d10
023200131004     c                   movel     §$3d00        wdateiso
023300131004     c                   adddur    1:*d          wdateiso
023400131004     c                   movel     wdateiso      d00
023500130326
023600130326      /free
023700130326
023800130328       //?Operazioni iniziali?
023900130326       exsr RoutInz;
024000130326
024100130328       //?Gestione vidate?
024200130326       DOW not wFine;
024300130326         SELECT;
024400130802           WHEN  wVideo = 'S1';
024500130802             exsr GesS01;
024600130326           OTHER;
024700130326             wFine = *on;
024800130326         ENDSL;
024900130326       ENDDO;
025000130326
025100130328       //?Operazioni finali?
025200130326       exsr RoutEnd;
025300130326
025400130326       //--------------------------------------------------------------
025500130326       //?Operazioni iniziali.                                         ?
025600130326       //--------------------------------------------------------------
025700130326       BEGSR  RoutInz;
025800130326
025900130326       //?Reperimento dati job?
026000130326         exsr  DatiJob;
026100130328
026200130328         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
026300130327
026400130327       //?Imposto nome pgm a video
026500130327         V00pgm = SDSpgm;
026600130805
026700130805       //?Data del giorno
026800130805         wOggi = %dec( %date() );
026900130405
027000130326       ENDSR;
027100130326
027200130326       //--------------------------------------------------------------
027300130326       //?Reperimento Dati del job (Utente/Operativi).                 ?
027400130326       //--------------------------------------------------------------
027500130326       BEGSR  DatiJob;
027600130326
027700130326         in(E) §AzUte;
027800130326         IF  NOT %error;
027900130326           in(E) §DatiUte;
028000130326         ENDIF;
028100130326         IF  %error or RSut = *blanks;
028200130326           clear TIBS34DS;
028300130326           tibs34r ( TIBS34DS );
028400130326           in §AzUte;
028500130326           in §DatiUte;
028600130326         ENDIF;
028700130326
028800130326       ENDSR;
028900130328
029000130802       //--------------------------------------------------------------
029100130802       //?Gestione videata S01.
029200130802       //--------------------------------------------------------------
029300130802       BEGSR  GesS01;
029400130802
029500130802       //?Inizializzazione videata
029600130802         IF  wInzS01;
029700130805           exsr InzS01;
029800130805           PosCurDTD   = *on;
029900130802           wInzS01 = *off;
030000130802         //?Per errore durante l'inizializzazione delle videta
030100130802         //?esco dal programma
030200130802           IF  ErrGenerico;
030300130802             wFine = *on;
030400130802             leavesr;
030500130802           ENDIF;
030600130802         ENDIF;
030700130802
030800130802       //?Emissione videata
030900130805         write SB3000T;
031000130802         write SB3001Z;
031100130802         exfmt SB3001C;
031200130802         ErrMessage  = *off;
031300130802         ErrGenerico = *off;
031400130802         clear V01msg;
031500130802
031600130802         SELECT;
031700130802
031800130802       //?F3=Fine
031900130802           WHEN  dsp_aid = c_F03;
032000130802             exsr F03S01;
032100130802
032200130802       //?F6=Conferma
032300130802           WHEN  dsp_aid = c_F06;
032400130802           //?Eseguo i controlli control
032500130802             exsr CtrC01 ;
032600130802             IF  ErrGenerico;
032700130802               leavesr;
032800130802             ENDIF;
032900130802           //?Eseguo i controlli subfile
033000130802             exsr CtrS01 ;
033100130802             IF  ErrGenerico;
033200130802               leavesr;
033300130802             ENDIF;
033400130802             exsr F06S01;
033500130802             exsr F03S01;
033600130802
033700130802       //?Enter
033800130802           OTHER;
033900130802           //?Eseguo i controlli control
034000130802             exsr CtrC01 ;
034100130802             IF  ErrGenerico;
034200130802               leavesr;
034300130802             ENDIF;
034400130802         //?Eseguo i controlli subfile
034500130802             exsr CtrS01;
034600130802             IF  ErrGenerico;
034700130802               leavesr;
034800130802             ENDIF;
034900130802
035000130802         ENDSL;
035100130802
035200130802       ENDSR;
035300130802
035400130802       //--------------------------------------------------------------
035500130802       //?Inizializzazione videata S01.
035600130802       //--------------------------------------------------------------
035700130802       BEGSR InzS01;
035800130802
035900130802       //?Pulizia del Subfile
036000130802         exsr PulS01;
036100130802
036200130802       //?Carico 10 righe vuote
036300131008         xx= 1;
036400131008         FOR xx by 1 to wMaxNrr;
036500130802           exsr CarS01;
036600130802         ENDFOR;
036700130802
036800130802       //?Visualizzazione del Subfile
036900130802         SflDsp = (S01nrr <= *zeros);
037000130802
037100131008       //?Attivazione SFLEND
037200131008         SflEnd = *on;
037300131008
037400130802       //?Impaginazione Subfile
037500130802       //?Forzo sempre il primo rcd
037600130802         IF  S01nrr > *zero;
037700130802           V01rcd = 1;
037800130802           V01csr = 1;
037900130802         ELSE;
038000130802           clear V01rcd;
038100130802         ENDIF;
038200130802
038300130802         V01csr = V01rcd;
038400130802
038500130802       ENDSR;
038600130802
038700130802       //--------------------------------------------------------------
038800130802       //?Pulizia Subfile S01.
038900130802       //--------------------------------------------------------------
039000130802       BEGSR PulS01;
039100130802
039200130802         SflDsp    = *on;
039300130802         SflDspCtl = *on;
039400130802         write  SB3001C;
039500130802         SflDspCtl = *off;
039600131008         SflEnd    = *off;
039700130802
039800130802         clear V01rcd;
039900130802         clear V01csr;
040000130802         clear S01nrr;
040100130802         clear V01msg;
040200130802         ErrMessage  = *off;
040300130802         ErrGenerico = *off;
040400130802
040500130802         WindDspF = IndDspF;
040600130802         reset WindDspFb;
040700130802         IndDspF  = WindDspF;
040800130802
040900130802       ENDSR;
041000130802
041100130802       //--------------------------------------------------------------
041200130802       //?Carico i dati nel SUBFILE.
041300130802       //--------------------------------------------------------------
041400130802       BEGSR CarS01;
041500130802
041600130802         clear V01pdr1;
041700130802         clear V01pdr2;
041800130802         clear V01pdrd;
041900131008         S01nrr = xx;
042000130802         write SB3001S;
042100130802
042200130802       ENDSR;
042300130805
042400130805       //--------------------------------------------------------------
042500130805       //?Gestione tasto funzionale F3.
042600130805       //?F3=Fine
042700130805       //--------------------------------------------------------------
042800130805       BEGSR F03S01;
042900130805
043000130805       //?Chiusura del programma
043100130805         wFine = *on;
043200130805
043300130805       ENDSR;
043400130802
043500130802       //--------------------------------------------------------------
043600130802       //?Gestione tasto funzionale F6.
043700130802       //?F6=Conferma.
043800130802       //--------------------------------------------------------------
043900130802       BEGSR F06S01;
044000130802
044100130805       //?Lancio l'estrazione delle immagini per ogni AUT immesso
044200130802         S01nrr = 1;
044300130802         FOR S01nrr by 1 to wMaxNrr;
044400130802       //?Aggiorno i dati in base a quanto inserito nel subfile
044500130802           chain S01nrr SB3001S;
044600130802           IF  %found;
044700130802
044800130805           //?Schedulo elaborazione
044900130802             IF  V01pdr1 > *zeros;
045000130805               wProg = S01nrr;
045100130802               exsr Elabora;
045200130802             ENDIF;
045300130802
045400130802             V01csr = S01nrr;
045500130802             V01rcd = S01nrr;
045600130802             UPDATE SB3001S;
045700130802           ENDIF;
045800130802         ENDFOR;
045900130802
046000130802       ENDSR;
046100130805
046200130805       //--------------------------------------------------------------
046300130805       //?Controllo videata C01.
046400130805       //--------------------------------------------------------------
046500130805       BEGSR CtrC01;
046600130805
046700130805         WindDspF = IndDspF;
046800130805         reset WindDspFb;
046900130805         IndDspF  = WindDspF;
047000130805
047100130805       //?Controllo cosa è stato inserito nel control
047200130805
047300130805         IF  V01dtd = *zeros;
047400130805           ErrMessage  = *on;
047500130805           ErrGenerico = *on;
047600130805           PosCurDTD   = *on;
047700130805           V01msg      = skMSG(01);
047800130805           leavesr;
047900130805         ENDIF;
048000130805
048100130805         clear wlbdat;
048200130805         G02dat = V01dtd;
048300130805         xsrda8(wlbdat);
048400130805         IF  G02err = '1';
048500130805           ErrMessage  = *on;
048600130805           ErrGenerico = *on;
048700130805           PosCurDTD   = *on;
048800130805           V01msg      = skMSG(01);
048900130805           leavesr;
049000130805         ENDIF;
049100131004       //?la data inizio non può essere < data pulizia bolle
049200131004
049300131004         IF  g02inv <= §$3dp0;
049400131004           ErrMessage  = *on;
049500131004           ErrGenerico = *on;
049600131004           PosCurDTD   = *on;
049700131004           V01msg      = skMSG(05);
049800131004           leavesr;
049900131004         ENDIF;
050000130805
050100130805         V01dtd = G02dat;
050200130805         wdtd   = G02inv;
050300130805
050400130805         IF  V01dta = *zeros;
050500130805           ErrMessage  = *on;
050600130805           ErrGenerico = *on;
050700130805           PosCurDTA   = *on;
050800130805           V01msg      = skMSG(01);
050900130805           leavesr;
051000130805         ENDIF;
051100130805
051200130805         clear wlbdat;
051300130805         G02dat = V01dta;
051400130805         xsrda8(wlbdat);
051500130805         IF  G02err = '1';
051600130805           ErrMessage  = *on;
051700130805           ErrGenerico = *on;
051800130805           PosCurDTA   = *on;
051900130805           V01msg      = skMSG(01);
052000130805           leavesr;
052100130805         ENDIF;
052200130805
052300130805         V01dta = G02dat;
052400130805         wdta   = G02inv;
052500130805
052600130805         IF  wdtd > wdta;
052700130805           ErrMessage  = *on;
052800130805           ErrGenerico = *on;
052900130805           PosCurDTA   = *on;
053000130805           V01msg      = skMSG(02);
053100130805           leavesr;
053200130805         ENDIF;
053300130805
053400130805       ENDSR;
053500130327
053600130327       //--------------------------------------------------------------
053700130802       //?Controllo videata S01.
053800130327       //--------------------------------------------------------------
053900130802       BEGSR CtrS01;
054000130327
054100130328         WindDspF = IndDspF;
054200130328         reset WindDspFb;
054300130328         IndDspF  = WindDspF;
054400130328
054500130328       //?Controllo cosa è stato inserito nel subfile
054600130802         readc SB3001S;
054700130328
054800130802         DOW  not %eof(TNSB30D);
054900130802           %subst(IndDspF : 52) = *off;
055000130802           %subst(IndDspF : 53) = *off;
055100130709           SflNxtChg = *off;
055200130802           V01rcd = S01nrr;
055300130328
055400130328       //?Controllo se opzione valida
055500130328           SELECT;
055600130802             //?Nessun AUTISTA
055700130805             WHEN  V01pdr1 = *zeros and V01pdr2 = *blanks;
055800130802             //?Tutti e 2 i campi dell'autista
055900130802             WHEN  V01pdr1 = *zeros and V01pdr2 <> *blanks;
056000130710                 ErrMessage  = *on;
056100130710                 ErrGenerico = *on;
056200130802                 PosCurPDR1  = *on;
056300130805                 V01msg = skMSG(03);
056400130802             WHEN  V01pdr1 > *zeros and V01pdr2 = *blanks;
056500130802                 ErrMessage  = *on;
056600130802                 ErrGenerico = *on;
056700130802                 PosCurPDR2  = *on;
056800130805                 V01msg = skMSG(03);
056900130802             OTHER;
057000130802             //?Controllo se OK la Filiale AUT
057100130805               exsr ctrFIL;
057200130802             //?Controllo AUT
057300130805               IF  not ErrGenerico;
057400130805                 exsr ctrAUT;
057500130805               ENDIF;
057600130328           ENDSL;
057700130328
057800130328       //?Aggiorno il subfile
057900130328           SELECT;
058000130328             WHEN  ErrMessage;
058100130709               SflNxtChg = *on;
058200130802               V01csr    = V01rcd;
058300130328             WHEN  ErrGenerico;
058400130802               V01csr = V01rcd;
058500130328             OTHER;
058600130802               V01csr = V01rcd;
058700130328           ENDSL;
058800130328
058900130802           UPDATE SB3001S;
059000130328
059100130328       //?Al primo errore esco dalla lettura del subfile
059200130328           IF  ErrMessage or ErrGenerico;
059300130328             leave;
059400130328           ENDIF;
059500130328
059600130802           readc SB3001S;
059700130328
059800130328         ENDDO;
059900130327
060000130327       ENDSR;
060100130805
060200130805       //--------------------------------------------------------------
060300130805       //?Controllo la filiale AUTISTA.
060400130805       //--------------------------------------------------------------
060500130805       BEGSR ctrFIL;
060600130805
060700130805         IF  V01pdr1 = 0;
060800130805           leavesr;
060900130805         ENDIF;
061000130805
061100130805         chain (V01pdr1) AZORG01L;
061200130805         IF  not %found(AZORG01L) or ORGfva <> *blanks or
061300130805            (ORGfag <> 'A' and ORGfag <> 'F');
061400130805           ErrMessage  = *on;
061500130805           ErrGenerico = *on;
061600130805           PosCurPDR1  = *on;
061700130805           V01msg = skMSG(04);
061800130805           leavesr;
061900130805         ENDIF;
062000130805
062100130805       ENDSR;
062200130329
062300130805       //--------------------------------------------------------------
062400130805       //?Controllo il codice AUTISTA.
062500130805       //--------------------------------------------------------------
062600130805       BEGSR ctrAUT;
062700130805
062800130805         IF  V01pdr2 = *blanks;
062900130805           leavesr;
063000130805         ENDIF;
063100130805
063200130805         clear FNLV24DS;
063300130805         IF  %scan('?' : V01pdr2) > *zero;
063400130805           D24fil = V01pdr1;
063500130805           D24flg = 'R';
063600130805           D24tip = 'A';
063700130805           kpjbu = FNLV24DS;
063800130805           fnlv24r (KPJBA);
063900130805           FNLV24DS = kpjbu;
064000130805           V01pdr2 = %subst(%editc(D24pdr:'X'):4:4);
064100130805           V01pdrd = D24rsc;
064200130805           leavesr;
064300130805         ENDIF;
064400130805
064500130805         APDtip = 'A';
064600130805         APDpdr = %dec(V01pdr:7:0);
064700130805         chain (APDtip:APDpdr) FIAPD01L;
064800131004         IF  not %found(FIAPD01L) ;
064900130805           ErrMessage  = *on;
065000130805           ErrGenerico = *on;
065100130805           PosCurPDR1  = *on;
065200130805           V01msg = skMSG(04);
065300130805           leavesr;
065400130805         ENDIF;
065500130805         V01pdrd = APDrsf;
065600130805
065700130805       ENDSR;
065800130805
065900130805       //--------------------------------------------------------------
066000130805       //?Sottomette elaborazione.
066100130805       //--------------------------------------------------------------
066200130805       BEGSR Elabora;
066300130805
066400130805         clear TNSB30DS;
066500131004         ISB30dtd = wdtd;
066600131004         ISB30dta = wdta;
066700130805         ISB30pdr = %dec(V01pdr:7:0);
066800130805         ISB30idjob = %editc(wOggi:'X') + %editc(wProg:'X');
066900130805
067000130805         kcoaz = 'SB31';
067100130805         kpjbu = TNSB30DS;
067200130805         if  knmus = *all'1';
067300130805           tnsb31r(kpjba);
067400130805         else;
067500130805           BCH10(kpjba);
067600130805         endif;
067700130805
067800130805       ENDSR;
067900130805
068000130327       //--------------------------------------------------------------
068100130327       //?Operazioni finali.
068200130327       //--------------------------------------------------------------
068300130327       BEGSR RoutEnd;
068400130327
068500130327         *inLR = *on;
068600130327         return;
068700130327
068800130327       ENDSR;
068900130327
069000130327      /end-free
069100130326
069200130326       //--------------------------------------------------------------
069300130326       //?Schiere a tempo di compilazione.
069400130326       //--------------------------------------------------------------
069500130326
069600130326** - skMSG ------------------------------------------------------------------*
069700130805Data errata                                                                    01
069800130805Data "DAL" incongruente con data "AL"                                          02
069900130805Codice Autista incompleto                                                      03
070000130805Codice errato                                                                  04
070100131004Data "DAL" errata: Non esistono più le spedizioni negli archivi                02
