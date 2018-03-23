000100080206      //--------------------------------------------------------------
000200130121      //?FNLS50R - Gestine richeista traini BIS
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100080208      // - Anagrafica clienti potenziali
001200130122     fFNFGB01L  uf a e           k disk
001300130122     fFNFGB02L  If   e           k disk    RENAME(FNFGB000:FNFGB002)
001400130130     fFNFGB03L  If   e           k disk    RENAME(FNFGB000:FNFGB003)
001500080208      // - Organigramma filiale/aziendale
001600080206     fAZORG01L  if   e           k disk
001700130123     fAZCLN01L  if   e           k disk
001800080208      // - Tabelle
001900080206     fTABEL00F  if   e           k disk
002000080604     fTNTBE01l  if   e           k disk
002100080604
002200080208      // - Video Interrogazione anagrafica clienti potenziali
002300130121     ffnls50D   cf   e             workstn
002400080208     f                                     indds(IndDspF)
002500080206     f                                     infds(InfDspF)
002600130121     f                                     sfile(ls50S02 : S02nrr)
002700130122     f                                     sfile(ls50S03 : S03nrr)
002800080206
002900080206      //---------------------------------------------------------------
003000080206      //?Definizione costanti.
003100080206      //---------------------------------------------------------------
003200080411      // - Numero di record in ciascuna videata di subfile
003300130122     d c_SflPag        c                   const(16)
003400080411     d W_SflPag        s              3  0 inz
003500080530     d totrig          s              5  0 inz
003600130122     d ricInt          s              1
003700080207
003800080207      // - Tasti funzionali a video
003900080207     d c_F01           c                   const(x'31')
004000080207     d c_F02           c                   const(x'32')
004100080207     d c_F03           c                   const(x'33')
004200080207     d c_F05           c                   const(x'35')
004300080207     d c_F06           c                   const(x'36')
004400080207     d c_F07           c                   const(x'37')
004500080207     d c_F08           c                   const(x'38')
004600080207     d c_F09           c                   const(x'39')
004700080207     d c_F10           c                   const(x'3A')
004800080207     d c_F12           c                   const(x'3C')
004900080207     d c_F13           c                   const(x'B1')
005000080207     d c_F14           c                   const(x'B2')
005100080207     d c_F15           c                   const(x'B3')
005200080207     d c_F16           c                   const(x'B4')
005300080207     d c_F17           c                   const(x'B5')
005400080207     d c_F18           c                   const(x'B6')
005500080207     d c_F19           c                   const(x'B7')
005600080207     d c_F20           c                   const(x'B8')
005700080207     d c_F21           c                   const(x'B9')
005800080207     d c_F22           c                   const(x'BA')
005900080207     d c_F23           c                   const(x'BB')
006000080207     d c_F24           c                   const(x'BC')
006100080207     d c_Enter         c                   const(x'F1')
006200080207     d c_RollDown      c                   const(x'F4')
006300080207     d c_RollUp        c                   const(x'F5')
006400080214
006500080214      // - Attributi di visualizzazione
006600080215      //  -  DspAtr() - Normale
006700080214     d C_dspatr_Norm   c                   const(x'20')
006800080215      //  -  DspAtr(RI)
006900080214     d C_dspatr_RI     c                   const(x'21')
007000080215      //  -  DspAtr(ND)
007100080214     d C_dspatr_ND     c                   const(x'27')
007200080215      //  -  DspAtr(BL) / Color(Red)
007300080214     d C_dspatr_BL     c                   const(x'28')
007400080206
007500080206      //---------------------------------------------------------------
007600080206      //?Definizione schiere.
007700080206      //---------------------------------------------------------------
007800080206
007900080206      // - Messaggi di errore
008000130208     d MSG             s             78    dim(11) ctdata perrcd(1)
008100080609
008200080206      //---------------------------------------------------------------
008300080206      //?Definizione aree dati.
008400080206      //---------------------------------------------------------------
008500080206
008600080206      // - Dati utente
008700080206     d §AzUte        e ds                  extname(AZUTE00F)
008800080206     d                                     dtaara
008900080206     d §DatiUte      e ds                  extname(dDatiUte)
009000080206     d                                     dtaara
009100080206
009200080206      //---------------------------------------------------------------
009300080206      //?Definizione strutture dati.
009400080206      //---------------------------------------------------------------
009500080206
009600080206      // - Status
009700080206     d Psds           sds
009800080206     d   SDSpgm          *proc
009900080206
010000080206      // - InfDS
010100080206     d InfDspF         ds
010200080207     d  dsp_aid              369    369a
010300080207     d  sfl_rrn              376    377i 0
010400080207     d  min_nrr              378    379i 0
010500080207     d  num_rcds             380    381i 0
010600080206
010700080206      // - Indicatori su DspF
010800080208     d IndDspF         ds
010900080206        // - Indicatori di gestione del subfile
011000130123     d  visualDTN                     1n   overlay(IndDspF : 01)
011100130128     d  data_BLOC                     1n   overlay(IndDspF : 02)
011200130129     d  ProtD05                       1n   overlay(IndDspF : 03)
011300130130     d  InterD05                      1n   overlay(IndDspF : 04)
011400130130     d  ProtFIL                       1n   overlay(IndDspF : 05)
011500080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011600080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
011700080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011800080206     d  SflEnd                        1n   overlay(IndDspF : 33)
011900130218     d  SflEnd3                       1n   overlay(IndDspF : 34)
012000080206        // - Indicatori di errore
012100080206     d  errMessage                    1n   overlay(IndDspF : 28)
012200130121     d  ErrDTN                        1n   overlay(IndDspF : 40)
012300130123     d  ErrTFA                        1n   overlay(IndDspF : 41)
012400130123     d  ErrTMZ                        1n   overlay(IndDspF : 42)
012500130130     d  ErrTFP                        1n   overlay(IndDspF : 43)
012600130121     d  ErrScelta                     1n   overlay(IndDspF : 48)
012700080606     d  errGenerico                   1n   overlay(IndDspF : 99)
012800080206
012900080206      // - Parametri ricevuti
013000080206     d KPJBA         e ds
013100080206
013200080206      // - Reperimento dati utente
013300080206     d TIBS34ds      e ds
013400080206     d dUte01        e ds
013500080206
013600080206      // - Ricerca/Controllo tabelle
013700080206     d TIBS02ds      e ds                  inz
013800130123     d trul33ds      e ds                  inz
013900080206
014000080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
014100080206     d dLAT          e ds                  inz
014200130122     D TRUL31DS      E DS
014300130122     d POG                            3  0 DIM(250) overlay(o31pog)
014400130122     d
014500130122     d dstu          e ds                  inz
014600130121     D WLBDAT          DS
014700130121     D  G02DAT                 1      8  0
014800130121     D  G02INV                 9     16  0
014900130121     D  G02ERR                17     17
015000130121     D  G02TGI                18     22  0
015100130121
015200130123     D                 DS
015300130123     D  kaaa                   1      4  0
015400130123     D  kmmm                   5      6  0
015500130123     D  kgg                    7      8  0
015600130123     D  kDATA                  1      8  0
015700130123     d
015800130123     D clnmat          DS
015900130123     D  MAT                    1     31    dim(31)
016000130123     D clnpom          DS
016100130123     D  pom                    1     31    dim(31)
016200130123     D                 DS
016300080206      //---------------------------------------------------------------
016400080206      //?Definizione variabili globali.
016500080206      //---------------------------------------------------------------
016600080206
016700080206      // - Flags booleani
016800111115     d $Contattato1    s               n   inz(*off)
016900111115     d $Contattato2    s               n   inz(*off)
017000111115     d $EndAtt         s               n   inz(*off)
017100080208     d $Fine           s               n   inz(*off)
017200080208     d $InzD01         s               n   inz(*on)
017300080208     d $InzS01         s               n   inz(*on)
017400080414     d $InzS02         s               n   inz(*on)
017500080603     d $InzS03         s               n   inz(*on)
017600130129     d $InzD05         s               n   inz(*on)
017700080206     d $ErrGrave       s               n   inz(*off)
017800080207     d $EoF            s               n   inz(*off)
017900080208     d $RecOK          s               n   inz(*off)
018000080206
018100080207      // - Campi associati al video
018200130130     d Savvideo        s              2    inz
018300130130     d $Video          s              2    inz('D1')
018400130123     d S03nrr          s              4  0 inz
018500080414     d S02nrr          s              4  0 inz
018600080414     d wPag            s              4  0 inz
018700080414     d wRig            s              3  0 inz
018800080414
018900080604     d wAbi            s                   like(UTEaut)  inz
019000130121     d amgdtn          s              8  0 inz
019100130130     d Whim            s              6  0 inz
019200130123     d DataMinima      s              8  0 inz
019300130121     d Indx            s              3  0 inz
019400080619     d Indx2           s              3  0 inz
019500080609     d xx              s              3  0 inz
019600080929     d yy              s              3  0 inz
019700080415     d ss              s              3  0 inz
019800080415     d ff              s              3  0 inz
019900130123     d Ktfp            s              3  0 inz
020000130123     d Ktfa            s              3  0 inz
020100130130     d WoraLim         s              6  0
020200130123     d Wfest           s              1
020300130326     d Wtrov           s              1
020400130326     d Norec           s              1
020500130128     d x§tcod          s                   like(tblcod)
020600130128     d x§tkey          s                   like(tblkey)
020700130128     d x§tdes          s             30
020800130122     d
020900080605     d Dataiso         s               d   datfmt(*iso)
021000080605     d Dataymd         s               d   datfmt(*ymd)
021100080605     d Datadmy         s               d   datfmt(*dmy)
021200130122     d Dataeur         s               d   datfmt(*eur)
021300081010     d DataSYS         s               d   inz(*sys)
021400130123     d Orasys          s               t   timfmt(*hms) inz(*sys)
021500081112     D
021600080206      //---------------------------------------------------------------
021700080206      //?Definizione procedure usate.
021800080206      //---------------------------------------------------------------
021900080414      /copy gaitrasrc/srcprotopr,tibs02r
022000080414      /copy gaitrasrc/srcprotopr,tibs34r
022100130122      /copy gaitrasrc/srcprotopr,trul31r
022200130123      /copy gaitrasrc/srcprotopr,trul33r
022300130122      /copy gaitrasrc/srcprotopr,x§taber
022400130122      /copy gaitrasrc/srcprotopr,xsrda8
022500130122
022600080206      //---------------------------------------------------------------
022700080206      //?Riepilogo indicatori.
022800080206      //---------------------------------------------------------------
022900080207      // - Comuni
023000080207      // 28    : Emissione messaggio di errore a video
023100080207      // - C01/S01
023200080207      // 30    : Condiziona SFLDSP    (*not)
023300080207      // 31    : Condiziona SFLDSPCTL (*not)
023400080207      // 30+31 : Condiziona SFLCLR
023500080207      // 32    : Condiziona SFLNXTCHG in S01
023600080207      // 50    : Errore: Opzione errata
023700080207      // - D01
023800080207      // - Comuni
023900080207      // 99    : Generico di Errore
024000080206      //---------------------------------------------------------------
024100080206
024200080206      //---------------------------------------------------------------
024300080206      //?M A I N - L I N E
024400080206      //---------------------------------------------------------------
024500080206
024600080206     c     *Entry        plist
024700080206     c                   parm                    KPJBA
024800080206
024900080206      /free
025000080206
025100080206       // Operazioni iniziali
025200080206       exsr RoutInz;
025300080206
025400080206       // Gestione video
025500080206       DOW $Fine = *off;
025600080206         select;
025700080530
025800080530         // Videata di selezioni
025900080206           when $Video = 'D1';
026000080206             exsr GesD01;
026100080530
026200080530         // SFL di scelta
026300080411           when $Video = 'S2';
026400080411             exsr GesS02;
026500080530
026600130129         // SFL di inserimento
026700130122           when $Video = 'S3';
026800130122             exsr GesS03;
026900130122
027000080206           other;
027100080206             $Fine = *on;
027200080206         endsl;
027300080206       ENDDO;
027400080206
027500080206       // Operazioni finali
027600080206       exsr RoutEnd;
027700080206
027800130122       //--------------------------------------------------------------
027900130122       //?Reperimento Dati del job (Utente/Operativi).
028000130122       //--------------------------------------------------------------
028100130122       BEGSR DatiJob;
028200130122
028300130122         in(E) §AzUte;
028400130122         if NOT %error;
028500130122           in(E) §DatiUte;
028600130122         endif;
028700130122         if %error or RSut = *blanks;
028800130122           clear TIBS34ds;
028900130122           tibs34r(tibs34ds);
029000130122           in §AzUte;
029100130122           in §DatiUte;
029200130122         endif;
029300130122
029400130122       ENDSR;
029500130122
029600080206       //--------------------------------------------------------------
029700080206       //?Operazioni iniziali.
029800080206       //--------------------------------------------------------------
029900080206       BEGSR RoutInz;
030000080414       $inzd01=*on;
030100080206
030200080206         // Impostazione campi "fissi"
030300080208         TBLkut = 1;
030400080208         TBLcod = '1L';
030500080206
030600080206         // Reperimento dati job
030700080206         exsr DatiJob;
030800080206
030900080206         // Verifica errori e autorità profilo
031000080206         clear  wAbi;
031100080206         clear  dLAT;
031200080206         select;
031300080206         // - Se ho errori nei dati utente esco dal pgm
031400080208           when  DUTerr = 'E';
031500080206             $Fine = *on;
031600080206         // - Se non c'è l'abilitazione:
031700080206         //   - se 1° livello, abilitazioni al terminal
031800080206         //   - se 2° livello, abilitazioni al punto operativo
031900080206         //   - se sede è impossibile ma se capita mando a fine il pgm
032000080208           when  UTEaut = *blank;
032100080206             select;
032200080208               when  DUTlpo = '1';
032300080206                 wAbi  = 'TP';
032400080208               when  DUTlpo = '2';
032500080206                 wAbi  = 'PO';
032600080208               when  DUTlpo = 'S';
032700080206                 $Fine = *on;
032800080206             endsl;
032900080206         // - Altrimenti si caricano le abilitazioni del profilo
033000080206           other;
033100080208             dUTE01 = UTEfaf;
033200080208             if  §UTEpot <> *blank;
033300080208               wAbi = §UTEpot;
033400080206             else;
033500080208               wAbi = UTEaut;
033600080206             endif;
033700080206         ENDSL;
033800080206
033900080206         // Controllo se ok l'abilitazione dell'utente
034000080206         reset TIBS02ds;
034100111115         T02Mod = 'C';
034200080208         T02sif = knsif;
034300080208         T02cod = 'LAT';
034400080208         T02ke1 = wAbi;
034500080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
034600080206
034700080208         if  T02err  = *blank;
034800080208           dLAT = T02uni;
034900080206         endif;
035000080206
035100080206         // Errore o utente non abilitato
035200080208         if  T02err <> *blanks   or
035300080208             §LATabi = 'S';
035400080206           $ErrGrave   = *on;
035500080206           errMessage  = *on;
035600080206           errGenerico = *on;
035700130122           V1cmsg = Msg(01);
035800080206           leavesr;
035900080206         endif;
036000080617
036100080617         clear trul31ds  ;
036200080617         I31abi = wabi    ;
036300080617         I31cdi = DUTdis  ;
036400080617         I31car = DUTare  ;
036500080617         I31cpo = DUTpou  ;
036600080617         TRUL31R   (KPJBA:trul31ds)   ;
036700080617
036800080617         if o31pog=*zeros   ;
036900080617           $ErrGrave   = *on;
037000080617           errMessage  = *on;
037100080617           errGenerico = *on;
037200130122          V1cmsg = Msg(01);
037300080617           leavesr;
037400080617         endif         ;
037500100520
037600130123       // Carico tabella ora limite per richiesta BIS nel giorno
037700130123         clear tblkey  ;
037800130123         tblkey='BIS'       ;
037900130123         chain  (1:'TW':tblkey) tabel00f;
038000130123 6       if %found(tabel00f);
038100130130          Woralim=%int(%subst(tbluni:1:6))   ;
038200130123         endif ;
038300100520
038400130122        ENDSR  ;
038500080206       //--------------------------------------------------------------
038600080206       //?Gestione videata D01
038700080206       //--------------------------------------------------------------
038800080206       BEGSR GesD01;
038900080206
039000080206         // Inizializzazione videata
039100080414
039200080414         if  $inzd01 = *on;
039300130123         // Verifico che data proporre per le richieste
039400130123           exsr Propdata  ;
039500130123
039600130128           dataeur=%date(dataMinima)  ;
039700130128           v01dtn=%dec(dataeur)    ;
039800130123
039900130130           if  simfel>0  ;
040000130130           v01tfp=simfel  ;
040100130122           chain  simfel  azorg01l   ;
040200130122           if %found(azorg01l) ;
040300130130             v01dtfp=orgdes  ;
040400130122           endif  ;
040500130130           else  ;
040600130130           clear v01tfp  ;
040700130130           clear v01dtfp  ;
040800130130           endif  ;
040900130122
041000080414          $Inzd01=*off;
041100080411         endif ;
041200080206
041300080206         // Emissione testata
041400080411         if  errGenerico = *off;
041500130123          VisualDTN=*off  ;
041600130129          clear V01DRIC   ;
041700130129
041800130121           write ls50T01;
041900080411         endif;
042000080206
042100080206         // Emissione videata
042200130121         exfmt ls50D01;
042300080411
042400130122         clear V1cmsg;
042500130121         clear errDTN    ;
042600130130         clear errTFP    ;
042700080411         clear errGenerico;
042800080207
042900080206         select;
043000080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
043100080206           when  $ErrGrave = *on;
043200080206             $Fine  = *on;
043300080411
043400080206         // F3=Fine
043500080208           when dsp_aid = c_F03;
043600080409           $Fine = *on;
043700080411
043800130121           other  ;
043900080411             exsr Contrd01 ;
044000080411
044100080414         // Avanzamento se non errori
044200080414
044300080411             if errGenerico=*off;
044400080415
044500130123            //  Inserimento
044600130123              if   dsp_aid = c_F15;
044700130130              // non lo posso fare se data < della minima proposta
044800130130              if amgdtn<dataMinima  ;
044900130130               errDTN      = *on;
045000130130               v1cmsg = Msg(09);
045100130130               errMessage  = *on;
045200130130               errGenerico = *on;
045300130130               leavesr;
045400130130              endif  ;
045500130130
045600130130              // Mi salvo da quale video vengo
045700130130              savVideo=$Video  ;
045800130123              $Video = 'S3';
045900130123              $InzS03 = *on;
046000130123              else  ;
046100130130            //  Visualizzo richieste inserite ;
046200080411              $Video = 'S2';
046300080411              $InzS02 = *on;
046400130122             endif   ;
046500130123
046600130123             endif   ;
046700130122             endsl  ;
046800080603
046900080206       ENDSR;
047000080411       //--------------------------------------------------------------
047100080411       //?Controllo campi 1 videata
047200080411       //--------------------------------------------------------------
047300080411       BEGSR ContrD01;
047400080415
047500130130       // Terminal di partenza
047600130130       clear v01dtfp ;
047700130130 1     if v01tfp>0  ;
047800130130          chain v01tfp azorg01l  ;
047900130130 2        if %found(azorg01l)  and (orgfag='F' or
048000130130                orgfag='A')  and orgfva=' '  ;
048100130130           v01dtfp=orgdes  ;
048200130130 x2        else  ;
048300130130           errTFP      = *on;
048400130130           v1cmsg = Msg(08);
048500130130           errMessage  = *on;
048600130130           errGenerico = *on;
048700130130           leavesr;
048800130130 2        endif            ;
048900130130 1     endif  ;
049000130130
049100130130       // Data traino
049200130121       clear   amgdtn   ;
049300130121         g02dat=v01dtn    ;
049400130121         callp xsrda8 (wlbdat)  ;
049500130121          if g02err='1'      ;
049600130121           v1cmsg = Msg(01);
049700130121           errDTN      = *on;
049800130121           errMessage  = *on;
049900130121           errGenerico = *on;
050000130121           leavesr;
050100130121          endif            ;
050200130121
050300130121          amgdtn=g02inv  ;
050400130123          v01dtn=g02dat  ;
050500130208
050600130208          // non si accettano giorno festivi
050700130208          kdata =g02inv         ;
050800130208          ktfp=68  ;
050900130208          clear ktfa  ;
051000130208
051100130208          chain  (ktfp:ktfa:kaaa:kmmm) azcln01l  ;
051200130208
051300130208          if %found(azcln01l)   and (mat(kgg)<>' ' or
051400130208             pom(kgg) <>' ')  ;
051500130208           v1cmsg = Msg(11);
051600130208           errDTN      = *on;
051700130208           errMessage  = *on;
051800130208           errGenerico = *on;
051900130208           leavesr;
052000130208          endif            ;
052100130208
052200130121
052300080411       ENDSR;
052400080411       //--------------------------------------------------------------
052500080523       //?Gestione videata S02   - SCelta potenziali
052600080411       //--------------------------------------------------------------
052700080411       BEGSR GesS02;
052800080411
052900080411         // Inizializzazione videata
053000080411         if  $InzS02 = *on;
053100080411            exsr InzS02;
053200080411             $InzS02  = *off;
053300080411         endif;
053400080411
053500080411         // Posizionamento cursore
053600130326         if Norec=' '  ;
053700130326
053800080411         if  C02csr  > *zero;
053900130123           sfldsp_n=*off;
054000080411           C02rcd = C02csr;
054100130326         endif    ;
054200130326
054300130326         else;
054400080411
054500130123         // Se non è stato caricato nulla emetto sfl vuoto
054600130123           sfldsp_n=*on ;
054700080411         endif;
054800080411
054900080411
055000080411         // Emissione Testata e Piede con tasti funzionali abilitati
055100080604         if  errGenerico = *off or ricInt ='S'  ;
055200130121           write  ls50T01;
055300130121           write  ls50P02;
055400130123           if sfldsp_n=*on  ;
055500130123           write  ls50d04;
055600130123           endif  ;
055700130123
055800080604           clear  ricInt   ;
055900080411         endif;
056000080411
056100080411         // Emissione videata
056200130121         exfmt  ls50C02;
056300080411
056400080411         reset errMessage;
056500080411         reset errGenerico;
056600130122         clear V1cmsg;
056700080523         clear errScelta ;
056800080411
056900080411         SELECT;
057000080411
057100080411         // - F3=Fine
057200080411           WHEN  dsp_aid = c_F03;
057300080411            $Fine = *on;
057400080411
057500080411         // - F12=Ritorno
057600080411           WHEN  dsp_aid = c_F12;
057700080411           $Video = 'D1';
057800080411
057900130122           WHEN  dsp_aid = c_F15;
058000130130
058100130130              // non lo posso fare se data < della minima proposta
058200130130              if amgdtn<dataMinima  ;
058300130130               errDTN      = *on;
058400130130               v1cmsg = Msg(09);
058500130130               errMessage  = *on;
058600130130               errGenerico = *on;
058700130130               leavesr;
058800130130              endif  ;
058900130130
059000130130           savVideo=$Video  ;
059100130122           $Video = 'S3';
059200130123           $inzs03=*on  ;
059300130123
059400080414         // - Roll-Up
059500080414           WHEN  dsp_aid = c_RollUp;
059600080414           // - Verifica se raggiunto il limite di records registrabili
059700080414           //   nel subfile (9999)
059800080414             if  S02nrr = *hival;
059900080414               errMessage  = *on;
060000080414               errGenerico = *on;
060100130122               V1cmsg = Msg(03);
060200080414             else;
060300080414               exsr RollUpS02;
060400080414             endif;
060500080414
060600080411         // Invio
060700080411           OTHER;
060800130326
060900130326         // non eseguo i controlli se non ho caricato nemmeno 1 rec
061000130326             if norec=' '  ;
061100130326               exsr ContrS02  ;
061200130326               if  errGenerico = *on;
061300130326                 leavesr;
061400130326               endif;
061500130326             endif;
061600080411
061700080411         ENDSL;
061800080411
061900080411       ENDSR;
062000080411       //--------------------------------------------------------------
062100080414       //?Inizializzazione sfl 2
062200080411       //--------------------------------------------------------------
062300080411       BEGSR InzS02;
062400080411
062500080411       // Pulizia subfile
062600080411         SflDsp_N    = *on;
062700080411         SflDspCtl_N = *on;
062800130121         write  ls50C02;
062900080411         SflDspCtl_N = *off;
063000080411         SflEnd      = *off;
063100080411
063200130326         clear Norec   ;
063300130123         VisualDTN=*on   ;
063400080530         clear W_SflPag;
063500080414         $EoF = *off;
063600080411         clear C02rcd;
063700080414         clear S02nrr;
063800130122         clear V1cmsg;
063900080411         errMessage  = *off;
064000080411         errGenerico = *off;
064100080415         errscelta=*off  ;
064200080411
064300130130         // Imposto il campo terminal in gesitone
064400130130         v01fgs=v01tfp   ;
064500130130         v01dfgs=v01dtfp  ;
064600130130
064700080411       // Carico pagina
064800130130        if  v01fgs>0   ;
064900130122          setll  (v01fgs:amgdtn)    fnfgb02l;
065000130122          reade  (v01fgs:amgdtn)    fnfgb02l;
065100130130          $EoF  = (%eof(fnfgb02l));
065200130130          else  ;
065300130130          setll  (amgdtn)    fnfgb03l;
065400130130          reade  (amgdtn)    fnfgb03l;
065500130130          $EoF  = (%eof(fnfgb03l));
065600130130        endif  ;
065700080411
065800080530         totrig=C_SflPAg       ;
065900080530
066000080411         dow  $EoF   = *off     and
066100080530              S02nrr < totrig  ;
066200080411           exsr RollUpS02;
066300080411         enddo;
066400080411
066500080411       // -> forza l'impaginazione sul 1° rec. del subfile
066600080530       //    se vengo dalla videata di selezioni
066700080530
066800080411         if S02nrr > *zero;
066900080530           if c02csr=0 ;
067000080411           C02rcd  = 1;
067100080411           C02csr  = 1;
067200080530           else       ;
067300110831
067400110831           // Se c02csr > del numero di record, imposto il num max
067500110831             if c02csr>s02nrr ;
067600110831              c02csr=s02nrr ;
067700110831             endif  ;
067800080530           C02rcd  = c02csr;
067900080530           endif      ;
068000080530
068100080411         else;
068200080411           clear C02rcd;
068300110831           clear C02csr;
068400130326           Norec='S'   ;
068500080411         endif;
068600080411
068700080411       ENDSR;
068800080411       //--------------------------------------------------------------
068900080411       //?Caricamento singola pagina S02
069000080411       //--------------------------------------------------------------
069100080411       BEGSR RollUpS02;
069200080411
069300080411         S02nrr    = (W_SflPag * C_SflPag);
069400080411         W_SflPag += 1;
069500080411         SflNxtChg = *off;
069600080411
069700080411         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
069800080523 1       DOW  $EoF   = *off                   and
069900080411              S02nrr < (W_SflPag * C_SflPag)  and
070000080411              S02nrr < *hival;
070100080411
070200080411         // - Caricamento dati nel record del subfile
070300080414         clear s02sce  ;
070400130122         g02inv=fgbdtn   ;
070500130122         g02err='3'  ;
070600130122         callp xsrda8 (wlbdat)  ;
070700130128          dataeur=%date(g02dat:*eur)       ;
070800130122          datadmy=dataeur  ;
070900130122          s02dtn=%dec(datadmy)         ;
071000130122          h02dtn=fgbdtn          ;
071100130130          s02tfp=fgblnp ;
071200130130          s02aar=fgbaar ;
071300130129          s02nur=%trim(%editc(fgbnur:'2'));
071400130129          h02nur=fgbnur   ;
071500130129          s02tfa=fgbtfa   ;
071600130122          chain fgbtfa azorg01l  ;
071700130122          if %found(azorg01l)  ;
071800130122           s02dtfa=orgdes  ;
071900130122          endif  ;
072000130128          h02tmz=fgbtmz  ;
072100130122         clear tblkey  ;
072200130122         tblkey=fgbtmz      ;
072300130122         chain  (1:'TU':tblkey) tabel00f;
072400130122 6       if %found(tabel00f);
072500130128          s02dtmz=fgbtmz+'-'+tbluni  ;
072600130122         endif ;
072700130122         s02num=1  ;
072800130128         s02pru=fgbpru  ;
072900130129         s02ftr=fgbftr  ;
073000130128         s02not=fgbnot  ;
073100130128
073200130128         g02inv=fgbdim   ;
073300130128         g02err='3'  ;
073400130128         callp xsrda8 (wlbdat)  ;
073500130128          dataeur=%date(g02dat:*eur)       ;
073600130128          datadmy=dataeur  ;
073700130128          s02dim=%dec(datadmy)         ;
073800080411
073900080411           S02nrr += 1;
074000130121           write ls50S02;
074100080411
074200080411         // - Lettura record successivo nell'archivio
074300130130         if v01fgs>0  ;
074400130122          reade  (v01fgs:amgdtn)    fnfgb02l;
074500130122          $EoF  = (%eof(fnfgb02l));
074600130130          else  ;
074700130130          reade  (amgdtn)    fnfgb03l;
074800130130          $EoF  = (%eof(fnfgb03l));
074900130130         endif  ;
075000080411
075100080523 1       ENDDO;
075200080411
075300080411         // Attivazione (eventuale) del SFLEND
075400080411         SflEnd   = ($EoF = *on);
075500080411
075600080411         // Posizionamento cursore al 1° rcd della pagina
075700080523 1       if  S02nrr > *zero;
075800080414           wPag   = S02nrr / C_SflPag;
075900080414           wRig   = S02nrr - (C_SflPag * wPag);
076000080411           C02rcd = wPag * C_SflPag;
076100080523 2         if  wRig > *zeros;
076200080415             C02rcd = C02rcd + 1;
076300080411           else;
076400080411             C02rcd = C02rcd - C_SflPag + 1;
076500080523 2         endif;
076600080523 x1      else;
076700080411           clear  C02rcd;
076800080523 1       endif;
076900080411
077000080411         C02csr = C02rcd;
077100080411
077200080411       ENDSR;
077300080414       //--------------------------------------------------------------
077400080414       //?controllo scelta SFL 2
077500080414       //--------------------------------------------------------------
077600080523       BEGSR contrS02 ;
077700080414
077800130130       ErrScelta=*off ;
077900080415
078000130121       readc ls50s02;
078100130121 1     DOW  NOT  %eof(fnls50D);
078200080414
078300130129       // non è possibile modificare: il record è già stato scaricato
078400130129 2     if s02sce=  '2' and s02ftr<>' '   ;
078500130129           errScelta   = *on;
078600130129           errMessage  = *on;
078700130129           errGenerico = *on;
078800130129           V1cmsg = Msg(07);
078900130129           exsr errSFL2  ;
079000130129           leavesr       ;
079100130129       endif     ;
079200130130       // In SEDE non è possibile modificare
079300130130 2     if s02sce=  '2' and simfel=0      ;
079400130130           errScelta   = *on;
079500130130           errMessage  = *on;
079600130130           errGenerico = *on;
079700130130           V1cmsg = Msg(10);
079800130130           exsr errSFL2  ;
079900130130           leavesr       ;
080000130130       endif     ;
080100130129
080200130129       //
080300130129 2     if s02sce<> ' ' ;
080400130129         $Video = 'D5';
080500130129          $inzd05=*on;
080600130129
080700130129         dow $Video='D5'  ;
080800130129          exsr Gesd05     ;
080900130129         enddo ;
081000130130          InterD05=*off ;
081100130130          Protd05 =*off ;
081200130129          clear s02sce    ;
081300080526 2     endif ;
081400130130
081500130130          exsr errSFL2  ;
081600080414
081700130129       readc ls50s02;
081800130122       enddo  ;
081900130122
082000130122       ENDSR   ;
082100080603       //--------------------------------------------------------------
082200080603       //?Aggiornamento sfl per errore
082300080603       //--------------------------------------------------------------
082400080603       BEGSR ErrSFL2 ;
082500080603         SflNxtChg = *on ;
082600130121         update ls50s02;
082700080603
082800130326         if Errscelta=*on AND s02nrr>0   ;
082900130326          c02csr=s02nrr   ;
083000130326         endif  ;
083100080603         errscelta=*off  ;
083200080603         SflNxtChg = *off ;
083300080603       ENDSR;
083400080411
083500130122       //--------------------------------------------------------------
083600130122       //?Gestione videata S03   - Inserimento
083700130122       //--------------------------------------------------------------
083800130122       BEGSR GesS03;
083900130122
084000130122         // Inizializzazione videata
084100130123 1       if  $InzS03 = *on;
084200130122            exsr InzS03;
084300130122             $InzS03  = *off;
084400130123 1       endif;
084500130122
084600130123 1       // Posizionamento cursore
084700130122         if  C03csr  > *zero;
084800130122           C03rcd = C03csr;
084900130123 1       endif    ;
085000130122
085100130122         // Posizionamento cursore
085200130122           sfldsp_n=*off;
085300130122
085400130122         // Emissione Testata e Piede con tasti funzionali abilitati
085500130128 1       if  errGenerico = *off or ricInt='S' ;
085600130122           write  ls50T01;
085700130122           write  ls50P03;
085800130128           clear ricInt  ;
085900130123 1       endif;
086000130122
086100130122         // Emissione videata
086200130122         exfmt  ls50C03;
086300130122
086400130122         reset errMessage;
086500130122         reset errGenerico;
086600130122         clear V1cmsg;
086700130122
086800130123 1       SELECT;
086900130122
087000130122         // - F3=Fine
087100130122           WHEN  dsp_aid = c_F03;
087200130122            $Fine = *on;
087300130122
087400130122         // - F12=Ritorno
087500130122           WHEN  dsp_aid = c_F12;
087600130130           $Video = savVideo  ;
087700130130           $inzs02=*on  ;
087800130130           $inzd01=*on  ;
087900130122
088000130122         // Invio
088100130122           OTHER;
088200130122             exsr ContrS03  ;
088300130122             if  errGenerico = *on;
088400130122               leavesr;
088500130122             endif;
088600130123
088700130123 2         if    dsp_aid = c_F06;
088800130129            exsr  AggiorS03 ;
088900130123
089000130123 3           if  errGenerico = *on;
089100130123               leavesr;
089200130123             else  ;
089300130130              $Video = savVideo  ;
089400130130              $inzs02=*on  ;
089500130130              $inzd01=*on  ;
089600130123 3           endif;
089700130123 2         endif;
089800130122
089900130123 1       ENDSL;
090000130122
090100130122       ENDSR;
090200130122       //--------------------------------------------------------------
090300130123       //?Inizializzazione sfl 3 inserimento
090400130122       //--------------------------------------------------------------
090500130122       BEGSR InzS03;
090600130122
090700130130         // Imposto il campo terminal in gesitone
090800130130         v01fgs=v01tfp   ;
090900130130         v01dfgs=v01dtfp  ;
091000130130
091100130128         VisualDTN=*on   ;
091200130129         v01dric=' INSERIMENTO '  ;
091300130128
091400130122       // Pulizia subfile
091500130122         SflDsp_N    = *on;
091600130122         SflDspCtl_N = *on;
091700130122         write  ls50C03;
091800130122         SflDspCtl_N = *off;
091900130218         SflEnd3     = *off;
092000130122
092100130122         clear C03rcd;
092200130122         clear S03nrr;
092300130122         clear V1cmsg;
092400130122         errMessage  = *off;
092500130122         errGenerico = *off;
092600130122
092700130123         clear s03tfa  ;
092800130123         clear s03dtfa  ;
092900130123         clear s03tmz   ;
093000130123         clear s03dtmz   ;
093100130128         clear s03not1   ;
093200130128         clear s03not2   ;
093300130122
093400130218         dow  s03nrr < 32 ;
093500130123         s03nrr=s03nrr+1  ;
093600130123         write ls50s03    ;
093700130122         enddo;
093800130218         SflEnd3     = *on ;
093900130122
094000130123         s03nrr=1 ;
094100130128         c03csr=1 ;
094200130122
094300130122       ENDSR;
094400130123       //--------------------------------------------------------------
094500130123       //?controllo dati   SFL 3
094600130123       //--------------------------------------------------------------
094700130123       BEGSR contrS03 ;
094800130123
094900130123       clear ErrTFA    ;
095000130123       clear ErrTMZ    ;
095100130123
095200130123       readc ls50s03;
095300130123 1     DOW  NOT  %eof(fnls50D);
095400130123
095500130128 2     if s03tfa>0   or s03tmz<>' ' or s03not1<>*blanks
095600130128            or s03not2<>*blanks    ;
095700130128
095800130123       chain   s03tfa    azorg01l  ;
095900130123
096000130128  3    if  not %found(azorg01l)  or orgfag<>'F'
096100130123           and orgfag<>'A' or   orgfva<>' '  ;
096200130123           errTFA=*on  ;
096300130123           errMessage  = *on;
096400130123           errGenerico = *on;
096500130123           V1cmsg = Msg(04);
096600130326           c03csr=s03nrr ;
096700130123           SflNxtChg = *on ;
096800130123           update ls50s03  ;
096900130123           SflNxtChg = *off;
097000130123           leavesr;
097100130128  3    endif  ;
097200130123
097300130123       s03dtfa=orgdes  ;
097400130128
097500130128       // tipo mezzo
097600130128  3    if s03tmz='?'  ;
097700130128
097800130128       clear x§tkey  ;
097900130128       x§tcod='TU'  ;
098000130128       callp x§taber    (1:x§tcod:x§tkey:x§tdes ) ;
098100130128       s03tmz=x§tkey  ;
098200130128       s03dtmz=x§tdes  ;
098300130128           ricint='S'  ;
098400130128           errTmz      = *on;
098500130128           errGenerico = *on;
098600130128           SflNxtChg = *on ;
098700130326           c03csr=s03nrr ;
098800130128           update ls50s03  ;
098900130128           SflNxtChg = *off;
099000130128           leavesr;
099100130128 3     endif  ;
099200130128
099300130128         clear tblkey  ;
099400130128         tblkey=s03tmz      ;
099500130128         chain  (1:'TU':tblkey) tabel00f;
099600130128 3       if s03tmz=' ' or not %found(tabel00f) or
099700130128                     tblflg<>' '  ;
099800130128           errTmz      = *on;
099900130128           errGenerico = *on;
100000130128           errMessage  = *on;
100100130326           c03csr=s03nrr ;
100200130128           V1cmsg = Msg(06);
100300130128           SflNxtChg = *on ;
100400130128           update ls50s03  ;
100500130128           SflNxtChg = *off;
100600130128           leavesr;
100700130128         else  ;
100800130128         dstu=tbluni   ;
100900130128
101000130128        // se non utilizzabile nei BIs errore
101100130128        s03dtmz=§tudes   ;
101200130128        if §tuutib<>'S'   ;
101300130128           errTmz      = *on;
101400130128           errMessage  = *on;
101500130128           errGenerico = *on;
101600130326           c03csr=s03nrr ;
101700130128           V1cmsg = Msg(06);
101800130128           SflNxtChg = *on ;
101900130128           update ls50s03  ;
102000130128           SflNxtChg = *off;
102100130128           leavesr;
102200130128        endif  ;
102300130128 3     endif  ;
102400130128
102500130128
102600130128          SflNxtChg = *on ;
102700130128           update ls50s03  ;
102800130128          SflNxtChg = *off;
102900130326           c03csr=s03nrr ;
103000130123
103100130128  x2   else  ;
103200130123          clear s03dtfa  ;
103300130123          clear s03dtmz  ;
103400130123          SflNxtChg = *off;
103500130123           update ls50s03  ;
103600130128  2    endif  ;
103700130123
103800130123       readc ls50s03;
103900130128  1    enddo  ;
104000130123
104100130123       ENDSR   ;
104200130123       //--------------------------------------------------------------
104300130129       //?Aggiornamento richieste    S03
104400130123       //--------------------------------------------------------------
104500130129       BEGSR AggiorS03;
104600130123       readc ls50s03;
104700130123 1     DOW  NOT  %eof(fnls50D);
104800130123
104900130326 2     if s03tfa>0   ;
105000130123       clear fnfgb000  ;
105100130123         fgbpru=knmus  ;
105200130123         dataiso=datasys         ;
105300130123         fgbdim=%dec(datasys)      ;
105400130123         fgbhim=%dec(orasys)      ;
105500130123         fgbaar=%int(%subst(%editc(fgbdim :'X'):1:4)) ;
105600130123         // Stacco numero richiesta  ;
105700130326         clear wtrov  ;
105800130326  3      dow wtrov=' '  ;
105900130326
106000130123         clear trul33ds  ;
106100130123         i33cnu=042  ;
106200130123         i33num=1  ;
106300130123         i33aaa=fgbaar  ;
106400130123         kpjbu= trul33ds  ;
106500130123
106600130123         callp trul33r (kpjba)   ;
106700130123         trul33ds=kpjbu  ;
106800130326 4       if o33err>0     ;
106900130123           errTFA=*on  ;
107000130123           errMessage  = *on;
107100130123           errGenerico = *on;
107200130123           V1cmsg = Msg(05);
107300130123           SflNxtChg = *on ;
107400130123           update ls50s03  ;
107500130123           SflNxtChg = *off;
107600130123           leavesr;
107700130326 x4       else  ;
107800130123          fgbnur=o33nri   ;
107900130326          // Prer sicurezza verifico se già presnete il numero
108000130326          //  ne stacco un altro
108100130326          setll (fgbaar:fgbnur)   fnfgb01l   ;
108200130326  5        If not %equal(fnfgb01l)  ;
108300130326            wtrov='S'  ;
108400130326  5        endif  ;
108500130326  4       endif  ;
108600130326  3       enddo  ;
108700130326
108800130123          fgbdtn=amgdtn  ;
108900130123          fgblnp=v01fgs  ;
109000130123          fgbtfa=s03tfa ;
109100130123          fgbtmz=s03tmz ;
109200130128          fgbnot=s03not1+s03not2;
109300130123          write fnfgb000   ;
109400130326  2       endif  ;
109500130123
109600130123         readc ls50s03;
109700130326  1    enddo   ;
109800130123
109900130123       ENDSR   ;
110000130123       //--------------------------------------------------------------
110100130123       //?Proposta data traini
110200130123       //--------------------------------------------------------------
110300130123       BEGSR PropData   ;
110400130128         data_BLOC=*off ;
110500130128
110600130130       // se sono in sede propongo sempre udate
110700130130       if simfel = 0  ;
110800130130           dataiso=datasys         ;
110900130130           ProtFIL=*off   ;
111000130130       else  ;
111100130130
111200130130           ProtFIL=*on    ;
111300130130
111400130123         Whim=%dec(orasys)      ;
111500130123          if whim<Woralim  ;
111600130123           dataiso=datasys         ;
111700130123          else  ;
111800130123           dataiso=datasys+ %days(1) ;
111900130128           data_BLOC=*on  ;
112000130123          endif   ;
112100130123
112200130123          // se Festivo o sabato lavorativo vado al successivo
112300130123          kdata =%dec(dataiso)  ;
112400130123          ktfp=68  ;
112500130123          clear ktfa  ;
112600130123          wfest='F'  ;
112700130123
112800130123          dow wfest='F'  ;
112900130123          chain  (ktfp:ktfa:kaaa:kmmm) azcln01l  ;
113000130123
113100130123          if %found(azcln01l)   and (mat(kgg)<>' ' or
113200130123             pom(kgg) <>' ')  ;
113300130123
113400130123           dataiso=dataiso+ %days(1) ;
113500130123           kdata =%dec(dataiso)  ;
113600130123          else  ;
113700130123          clear   wFEST  ;
113800130123          endif  ;
113900130123          enddo  ;
114000130130          endif  ;
114100130123
114200130123         // Memorizzo la data limite di inserimento richieste
114300130123         DataMinima=%dec(dataiso)  ;
114400130123
114500130123         ENDSR  ;
114600130129       //--------------------------------------------------------------
114700130129       //?Gestione videata D05
114800130129       //--------------------------------------------------------------
114900130129         BEGSR GesD05;
115000130129
115100130129         // Inizializzazione videata
115200130129
115300130129         if  $inzd05 = *on;
115400130129         // Verifico che data proporre per le richieste
115500130129           exsr Inzd05    ;
115600130129          $Inzd05=*off      ;
115700130129         endif ;
115800130129
115900130129         // Emissione testata
116000130129         if  errGenerico = *off or ricInt='S';
116100130129           write ls50T01;
116200130129           clear ricint   ;
116300130129         endif;
116400130129
116500130129         // Emissione videata
116600130129         exfmt ls50D05;
116700130129
116800130129         clear V1cmsg;
116900130129         clear errTMZ    ;
117000130129         clear errTFA    ;
117100130129         clear errGenerico;
117200130129
117300130129    1    select;
117400130129
117500130129         // F12=ritorno
117600130129           when dsp_aid = c_F12;
117700130129               $Video = 'S2';
117800130130               Errgenerico=*off  ;
117900130130               ErrMessage =*off  ;
118000130129
118100130129           other  ;
118200130129             exsr Contrd05 ;
118300130129
118400130130             if InterD05=*on  ;
118500130130             ErrMessage=*off  ;
118600130130             ErrGenerico=*off  ;
118700130130             endif    ;
118800130130
118900130129    2        if  errGenerico = *on;
119000130129               leavesr;
119100130129    2        endif;
119200130129
119300130130    2      if   InterD05=*on    ;
119400130129               $Video = 'S2';
119500130129    2      endif  ;
119600130129
119700130129 2         if    dsp_aid = c_F16;
119800130129            delete  fnfgb000  ;
119900130129               $Inzs02=*on  ;
120000130129               $Video = 'S2';
120100130129 2           endif;
120200130129
120300130129 2         if    dsp_aid = c_F06;
120400130129            exsr  AggiorD05  ;
120500130129
120600130129 3           if  errGenerico = *on;
120700130129               leavesr;
120800130129             else  ;
120900130129               $Video = 'S2';
121000130129 3           endif;
121100130129 2           endif;
121200130129
121300130129 1           endsl  ;
121400130129
121500130129          ENDSR;
121600130129       //--------------------------------------------------------------
121700130129       //?Inizializzazione VIDEATA 5
121800130129       //--------------------------------------------------------------
121900130129       BEGSR Inzd05;
122000130129
122100130129       if s02sce='2'  ;
122200130129       v01dric= ' MANUTENZIONE '  ;
122300130129       ENDIF  ;
122400130129       if s02sce='5'  ;
122500130129       v01dric= 'INTERROGAZIONE'  ;
122600130130       INTERD05=*ON  ;
122700130129       PROTD05=*ON  ;
122800130129       ENDIF  ;
122900130129
123000130129       // Carico in videata il record selezionato
123100130129       chain  (s02aar:h02nur)   fnfgb01l  ;
123200130129       if %found(fnfgb01l)  ;
123300130130
123400130130       d05tfp=fgblnp ;
123500130130       chain   fgblnp   azorg01l     ;
123600130130       d05dtfp=orgdes ;
123700130129       d05aar=fgbaar ;
123800130129       d05nur=%trim(%editc(fgbnur:'2'));
123900130129       d05pru=fgbpru  ;
124000130129       d05him=fgbhim ;
124100130129       d05not=fgbnot ;
124200130129       d05tfa=fgbtfa ;
124300130129       chain   fgbtfa   azorg01l     ;
124400130129       d05dtfa=orgdes ;
124500130129
124600130129       d05tmz=fgbtmz  ;
124700130129         clear tblkey  ;
124800130129         tblkey=fgbtmz      ;
124900130129         chain  (1:'TU':tblkey) tabel00f;
125000130129 6       if %found(tabel00f);
125100130129          d05dtmz=tbluni  ;
125200130129         endif ;
125300130129
125400130129         g02inv=fgbdtn   ;
125500130129         g02err='3'  ;
125600130129         callp xsrda8 (wlbdat)  ;
125700130129         d05dtn=g02dat   ;
125800130129
125900130129         g02inv=fgbdim   ;
126000130129         g02err='3'  ;
126100130129         callp xsrda8 (wlbdat)  ;
126200130129         d05dim=g02dat   ;
126300130129
126400130129         // Se è già stato scaricato dalla sede proteggo
126500130129           if fgbftr<>' '   ;
126600130129            Protd05 = *on ;
126700130129           endif   ;
126800130205
126900130205         // Data ora scarico SEDE
127000130205         if fgbdtr>0  ;
127100130205          dataiso=%date(fgbdtr:*iso)  ;
127200130205          dataeur=dataiso  ;
127300130205          d05dsca=%dec(dataeur)  ;
127400130205         else  ;
127500130205          clear d05dsca  ;
127600130205         endif  ;
127700130129
127800130205         d05hsca=fgbhtr  ;
127900130205
128000130129         else  ;
128100130129         // errore non trovato
128200130129         Errgenerico=*on  ;
128300130129         endif  ;
128400130129
128500130129       ENDSR  ;
128600130129       //--------------------------------------------------------------
128700130129       //?controllo dati   video 5
128800130129       //--------------------------------------------------------------
128900130129       BEGSR contrd05 ;
129000130129
129100130129       chain   d05tfa    azorg01l  ;
129200130129
129300130129  3    if  not %found(azorg01l)  or orgfag<>'F'
129400130129           and orgfag<>'A' or   orgfva<>' '  ;
129500130129           errTFA=*on  ;
129600130129           errMessage  = *on;
129700130129           errGenerico = *on;
129800130129           V1cmsg = Msg(04);
129900130129           leavesr;
130000130129  3    endif  ;
130100130129
130200130129       d05dtfa=orgdes  ;
130300130129
130400130129       // tipo mezzo
130500130129  3    if d05tmz='?'  ;
130600130129
130700130129       clear x§tkey  ;
130800130129       x§tcod='TU'  ;
130900130129       callp x§taber    (1:x§tcod:x§tkey:x§tdes ) ;
131000130129       s03tmz=x§tkey  ;
131100130129       s03dtmz=x§tdes  ;
131200130129           ricint='S'  ;
131300130129           errTmz      = *on;
131400130129           errGenerico = *on;
131500130129           leavesr;
131600130129 3     endif  ;
131700130129
131800130129         clear tblkey  ;
131900130129         tblkey=d05tmz      ;
132000130129         chain  (1:'TU':tblkey) tabel00f;
132100130129 3       if d05tmz=' ' or not %found(tabel00f) or
132200130129                     tblflg<>' '  ;
132300130129           errTmz      = *on;
132400130129           errGenerico = *on;
132500130129           errMessage  = *on;
132600130129           V1cmsg = Msg(06);
132700130129           leavesr;
132800130129         else  ;
132900130129         dstu=tbluni   ;
133000130129
133100130129        // se non utilizzabile nei BIs errore
133200130129        d05dtmz=§tudes   ;
133300130129        if §tuutib<>'S'   ;
133400130129           errTmz      = *on;
133500130129           errMessage  = *on;
133600130129           errGenerico = *on;
133700130129           V1cmsg = Msg(06);
133800130129           leavesr;
133900130129        endif  ;
134000130129 3     endif  ;
134100130129
134200130129       ENDSR   ;
134300130129       //--------------------------------------------------------------
134400130129       //?Aggiornoa richeista singola    D05
134500130129       //--------------------------------------------------------------
134600130129       BEGSR AggiorD05 ;
134700130129
134800130129         fgbpru=knmus  ;
134900130129         dataiso=datasys         ;
135000130129         fgbdim=%dec(datasys)      ;
135100130129         fgbhim=%dec(orasys)      ;
135200130129          fgbtfa=d05tfa ;
135300130129          fgbtmz=D05tmz ;
135400130129          fgbnot=D05not         ;
135500130129          update fnfgb000   ;
135600130129
135700130129           $Inzs02=*on  ;
135800130129
135900130129       ENDSR  ;
136000080206       //--------------------------------------------------------------
136100080206       //?Operazioni finali.
136200080206       //--------------------------------------------------------------
136300080206       BEGSR RoutEnd;
136400080410
136500080206         *inLR = *on;
136600080206         return;
136700080206
136800080206       ENDSR;
136900080206
137000080206      /end-free
137100080206
137200080206       //--------------------------------------------------------------
137300080206       //?Schiere a tempo di compilazione.
137400080206       //--------------------------------------------------------------
137500080206
137600080410** - MSG ------------------------------------------------------------------ -*
137700130121Data errata
137800130122Non è stata trovata alcuna richiesta di BIS nella data indicata
137900130123
138000130123Terminal/linea di Arrivo errata
138100130123Impossibile assegnare nuovo numero richiesta: telefonare al CED!!
138200130128Tipo automezzo errato o non gestito nella richiesta BIS
138300130207Impossibile modificare la richiesta; già elaborata da sede
138400130130Terminal di partenza errato                                                    8
138500130130Impossibile fare nuovi inserimenti in data già bloccata:scrivere mail a sede   9
138600130130Opzione non ammessa                                                           10
138700130208Impossibile fare nuovi inserimenti in giorno festivo                          11
