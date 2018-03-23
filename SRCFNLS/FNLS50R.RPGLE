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
002600151016     F                                     maxdev(*file)
002700130121     f                                     sfile(ls50S02 : S02nrr)
002800130122     f                                     sfile(ls50S03 : S03nrr)
002900080206
003000080206      //---------------------------------------------------------------
003100080206      //?Definizione costanti.
003200080206      //---------------------------------------------------------------
003300080411      // - Numero di record in ciascuna videata di subfile
003400130122     d c_SflPag        c                   const(16)
003500080411     d W_SflPag        s              3  0 inz
003600080530     d totrig          s              5  0 inz
003700130122     d ricInt          s              1
003800080207
003900080207      // - Tasti funzionali a video
004000080207     d c_F01           c                   const(x'31')
004100080207     d c_F02           c                   const(x'32')
004200080207     d c_F03           c                   const(x'33')
004300080207     d c_F05           c                   const(x'35')
004400080207     d c_F06           c                   const(x'36')
004500080207     d c_F07           c                   const(x'37')
004600080207     d c_F08           c                   const(x'38')
004700080207     d c_F09           c                   const(x'39')
004800080207     d c_F10           c                   const(x'3A')
004900080207     d c_F12           c                   const(x'3C')
005000080207     d c_F13           c                   const(x'B1')
005100080207     d c_F14           c                   const(x'B2')
005200080207     d c_F15           c                   const(x'B3')
005300080207     d c_F16           c                   const(x'B4')
005400080207     d c_F17           c                   const(x'B5')
005500080207     d c_F18           c                   const(x'B6')
005600080207     d c_F19           c                   const(x'B7')
005700080207     d c_F20           c                   const(x'B8')
005800080207     d c_F21           c                   const(x'B9')
005900080207     d c_F22           c                   const(x'BA')
006000080207     d c_F23           c                   const(x'BB')
006100080207     d c_F24           c                   const(x'BC')
006200080207     d c_Enter         c                   const(x'F1')
006300080207     d c_RollDown      c                   const(x'F4')
006400080207     d c_RollUp        c                   const(x'F5')
006500080214
006600080214      // - Attributi di visualizzazione
006700080215      //  -  DspAtr() - Normale
006800080214     d C_dspatr_Norm   c                   const(x'20')
006900080215      //  -  DspAtr(RI)
007000080214     d C_dspatr_RI     c                   const(x'21')
007100080215      //  -  DspAtr(ND)
007200080214     d C_dspatr_ND     c                   const(x'27')
007300080215      //  -  DspAtr(BL) / Color(Red)
007400080214     d C_dspatr_BL     c                   const(x'28')
007500080206
007600080206      //---------------------------------------------------------------
007700080206      //?Definizione schiere.
007800080206      //---------------------------------------------------------------
007900080206
008000080206      // - Messaggi di errore
008100130208     d MSG             s             78    dim(11) ctdata perrcd(1)
008200080609
008300080206      //---------------------------------------------------------------
008400080206      //?Definizione aree dati.
008500080206      //---------------------------------------------------------------
008600080206
008700080206      // - Dati utente
008800080206     d §AzUte        e ds                  extname(AZUTE00F)
008900080206     d                                     dtaara
009000080206     d §DatiUte      e ds                  extname(dDatiUte)
009100080206     d                                     dtaara
009200080206
009300080206      //---------------------------------------------------------------
009400080206      //?Definizione strutture dati.
009500080206      //---------------------------------------------------------------
009600080206
009700080206      // - Status
009800080206     d Psds           sds
009900080206     d   SDSpgm          *proc
010000080206
010100080206      // - InfDS
010200080206     d InfDspF         ds
010300080207     d  dsp_aid              369    369a
010400080207     d  sfl_rrn              376    377i 0
010500080207     d  min_nrr              378    379i 0
010600080207     d  num_rcds             380    381i 0
010700080206
010800080206      // - Indicatori su DspF
010900080208     d IndDspF         ds
011000080206        // - Indicatori di gestione del subfile
011100130123     d  visualDTN                     1n   overlay(IndDspF : 01)
011200130128     d  data_BLOC                     1n   overlay(IndDspF : 02)
011300130129     d  ProtD05                       1n   overlay(IndDspF : 03)
011400130130     d  InterD05                      1n   overlay(IndDspF : 04)
011500130130     d  ProtFIL                       1n   overlay(IndDspF : 05)
011600080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011700080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
011800080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011900080206     d  SflEnd                        1n   overlay(IndDspF : 33)
012000130218     d  SflEnd3                       1n   overlay(IndDspF : 34)
012100080206        // - Indicatori di errore
012200080206     d  errMessage                    1n   overlay(IndDspF : 28)
012300130121     d  ErrDTN                        1n   overlay(IndDspF : 40)
012400130123     d  ErrTFA                        1n   overlay(IndDspF : 41)
012500130123     d  ErrTMZ                        1n   overlay(IndDspF : 42)
012600130130     d  ErrTFP                        1n   overlay(IndDspF : 43)
012700130121     d  ErrScelta                     1n   overlay(IndDspF : 48)
012800080606     d  errGenerico                   1n   overlay(IndDspF : 99)
012900080206
013000080206      // - Parametri ricevuti
013100080206     d KPJBA         e ds
013200080206
013300080206      // - Reperimento dati utente
013400080206     d TIBS34ds      e ds
013500080206     d dUte01        e ds
013600080206
013700080206      // - Ricerca/Controllo tabelle
013800080206     d TIBS02ds      e ds                  inz
013900130123     d trul33ds      e ds                  inz
014000080206
014100080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
014200080206     d dLAT          e ds                  inz
014300130122     D TRUL31DS      E DS
014400130122     d POG                            3  0 DIM(250) overlay(o31pog)
014500130122     d
014600130122     d dstu          e ds                  inz
014700130121     D WLBDAT          DS
014800130121     D  G02DAT                 1      8  0
014900130121     D  G02INV                 9     16  0
015000130121     D  G02ERR                17     17
015100130121     D  G02TGI                18     22  0
015200130121
015300130123     D                 DS
015400130123     D  kaaa                   1      4  0
015500130123     D  kmmm                   5      6  0
015600130123     D  kgg                    7      8  0
015700130123     D  kDATA                  1      8  0
015800130123     d
015900130123     D clnmat          DS
016000130123     D  MAT                    1     31    dim(31)
016100130123     D clnpom          DS
016200130123     D  pom                    1     31    dim(31)
016300130123     D                 DS
016400080206      //---------------------------------------------------------------
016500080206      //?Definizione variabili globali.
016600080206      //---------------------------------------------------------------
016700080206
016800080206      // - Flags booleani
016900111115     d $Contattato1    s               n   inz(*off)
017000111115     d $Contattato2    s               n   inz(*off)
017100111115     d $EndAtt         s               n   inz(*off)
017200080208     d $Fine           s               n   inz(*off)
017300080208     d $InzD01         s               n   inz(*on)
017400080208     d $InzS01         s               n   inz(*on)
017500080414     d $InzS02         s               n   inz(*on)
017600080603     d $InzS03         s               n   inz(*on)
017700130129     d $InzD05         s               n   inz(*on)
017800080206     d $ErrGrave       s               n   inz(*off)
017900080207     d $EoF            s               n   inz(*off)
018000080208     d $RecOK          s               n   inz(*off)
018100080206
018200080207      // - Campi associati al video
018300130130     d Savvideo        s              2    inz
018400130130     d $Video          s              2    inz('D1')
018500130123     d S03nrr          s              4  0 inz
018600080414     d S02nrr          s              4  0 inz
018700080414     d wPag            s              4  0 inz
018800080414     d wRig            s              3  0 inz
018900080414
019000080604     d wAbi            s                   like(UTEaut)  inz
019100130121     d amgdtn          s              8  0 inz
019200130130     d Whim            s              6  0 inz
019300130123     d DataMinima      s              8  0 inz
019400130121     d Indx            s              3  0 inz
019500080619     d Indx2           s              3  0 inz
019600080609     d xx              s              3  0 inz
019700080929     d yy              s              3  0 inz
019800080415     d ss              s              3  0 inz
019900080415     d ff              s              3  0 inz
020000130123     d Ktfp            s              3  0 inz
020100130123     d Ktfa            s              3  0 inz
020200130130     d WoraLim         s              6  0
020300130123     d Wfest           s              1
020400130326     d Wtrov           s              1
020500130326     d Norec           s              1
020600130128     d x§tcod          s                   like(tblcod)
020700130128     d x§tkey          s                   like(tblkey)
020800130128     d x§tdes          s             30
020900130122     d
021000080605     d Dataiso         s               d   datfmt(*iso)
021100080605     d Dataymd         s               d   datfmt(*ymd)
021200080605     d Datadmy         s               d   datfmt(*dmy)
021300130122     d Dataeur         s               d   datfmt(*eur)
021400081010     d DataSYS         s               d   inz(*sys)
021500130123     d Orasys          s               t   timfmt(*hms) inz(*sys)
021600081112     D
021700080206      //---------------------------------------------------------------
021800080206      //?Definizione procedure usate.
021900080206      //---------------------------------------------------------------
022000080414      /copy gaitrasrc/srcprotopr,tibs02r
022100080414      /copy gaitrasrc/srcprotopr,tibs34r
022200130122      /copy gaitrasrc/srcprotopr,trul31r
022300130123      /copy gaitrasrc/srcprotopr,trul33r
022400130122      /copy gaitrasrc/srcprotopr,x§taber
022500130122      /copy gaitrasrc/srcprotopr,xsrda8
022600130122
022700080206      //---------------------------------------------------------------
022800080206      //?Riepilogo indicatori.
022900080206      //---------------------------------------------------------------
023000080207      // - Comuni
023100080207      // 28    : Emissione messaggio di errore a video
023200080207      // - C01/S01
023300080207      // 30    : Condiziona SFLDSP    (*not)
023400080207      // 31    : Condiziona SFLDSPCTL (*not)
023500080207      // 30+31 : Condiziona SFLCLR
023600080207      // 32    : Condiziona SFLNXTCHG in S01
023700080207      // 50    : Errore: Opzione errata
023800080207      // - D01
023900080207      // - Comuni
024000080207      // 99    : Generico di Errore
024100080206      //---------------------------------------------------------------
024200080206
024300080206      //---------------------------------------------------------------
024400080206      //?M A I N - L I N E
024500080206      //---------------------------------------------------------------
024600080206
024700080206     c     *Entry        plist
024800080206     c                   parm                    KPJBA
024900080206
025000080206      /free
025100080206
025200080206       // Operazioni iniziali
025300080206       exsr RoutInz;
025400080206
025500080206       // Gestione video
025600080206       DOW $Fine = *off;
025700080206         select;
025800080530
025900080530         // Videata di selezioni
026000080206           when $Video = 'D1';
026100080206             exsr GesD01;
026200080530
026300080530         // SFL di scelta
026400080411           when $Video = 'S2';
026500080411             exsr GesS02;
026600080530
026700130129         // SFL di inserimento
026800130122           when $Video = 'S3';
026900130122             exsr GesS03;
027000130122
027100080206           other;
027200080206             $Fine = *on;
027300080206         endsl;
027400080206       ENDDO;
027500080206
027600080206       // Operazioni finali
027700080206       exsr RoutEnd;
027800080206
027900130122       //--------------------------------------------------------------
028000130122       //?Reperimento Dati del job (Utente/Operativi).
028100130122       //--------------------------------------------------------------
028200130122       BEGSR DatiJob;
028300130122
028400130122         in(E) §AzUte;
028500130122         if NOT %error;
028600130122           in(E) §DatiUte;
028700130122         endif;
028800130122         if %error or RSut = *blanks;
028900130122           clear TIBS34ds;
029000130122           tibs34r(tibs34ds);
029100130122           in §AzUte;
029200130122           in §DatiUte;
029300130122         endif;
029400130122
029500130122       ENDSR;
029600130122
029700080206       //--------------------------------------------------------------
029800080206       //?Operazioni iniziali.
029900080206       //--------------------------------------------------------------
030000080206       BEGSR RoutInz;
030100080414       $inzd01=*on;
030200080206
030300080206         // Impostazione campi "fissi"
030400080208         TBLkut = 1;
030500080208         TBLcod = '1L';
030600080206
030700080206         // Reperimento dati job
030800080206         exsr DatiJob;
030900080206
031000080206         // Verifica errori e autorità profilo
031100080206         clear  wAbi;
031200080206         clear  dLAT;
031300080206         select;
031400080206         // - Se ho errori nei dati utente esco dal pgm
031500080208           when  DUTerr = 'E';
031600080206             $Fine = *on;
031700080206         // - Se non c'è l'abilitazione:
031800080206         //   - se 1° livello, abilitazioni al terminal
031900080206         //   - se 2° livello, abilitazioni al punto operativo
032000080206         //   - se sede è impossibile ma se capita mando a fine il pgm
032100080208           when  UTEaut = *blank;
032200080206             select;
032300080208               when  DUTlpo = '1';
032400080206                 wAbi  = 'TP';
032500080208               when  DUTlpo = '2';
032600080206                 wAbi  = 'PO';
032700080208               when  DUTlpo = 'S';
032800080206                 $Fine = *on;
032900080206             endsl;
033000080206         // - Altrimenti si caricano le abilitazioni del profilo
033100080206           other;
033200080208             dUTE01 = UTEfaf;
033300080208             if  §UTEpot <> *blank;
033400080208               wAbi = §UTEpot;
033500080206             else;
033600080208               wAbi = UTEaut;
033700080206             endif;
033800080206         ENDSL;
033900080206
034000080206         // Controllo se ok l'abilitazione dell'utente
034100080206         reset TIBS02ds;
034200111115         T02Mod = 'C';
034300080208         T02sif = knsif;
034400080208         T02cod = 'LAT';
034500080208         T02ke1 = wAbi;
034600080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
034700080206
034800080208         if  T02err  = *blank;
034900080208           dLAT = T02uni;
035000080206         endif;
035100080206
035200080206         // Errore o utente non abilitato
035300080208         if  T02err <> *blanks   or
035400080208             §LATabi = 'S';
035500080206           $ErrGrave   = *on;
035600080206           errMessage  = *on;
035700080206           errGenerico = *on;
035800130122           V1cmsg = Msg(01);
035900080206           leavesr;
036000080206         endif;
036100080617
036200080617         clear trul31ds  ;
036300080617         I31abi = wabi    ;
036400080617         I31cdi = DUTdis  ;
036500080617         I31car = DUTare  ;
036600080617         I31cpo = DUTpou  ;
036700080617         TRUL31R   (KPJBA:trul31ds)   ;
036800080617
036900080617         if o31pog=*zeros   ;
037000080617           $ErrGrave   = *on;
037100080617           errMessage  = *on;
037200080617           errGenerico = *on;
037300130122          V1cmsg = Msg(01);
037400080617           leavesr;
037500080617         endif         ;
037600100520
037700130123       // Carico tabella ora limite per richiesta BIS nel giorno
037800130123         clear tblkey  ;
037900130123         tblkey='BIS'       ;
038000130123         chain  (1:'TW':tblkey) tabel00f;
038100130123 6       if %found(tabel00f);
038200130130          Woralim=%int(%subst(tbluni:1:6))   ;
038300130123         endif ;
038400100520
038500130122        ENDSR  ;
038600080206       //--------------------------------------------------------------
038700080206       //?Gestione videata D01
038800080206       //--------------------------------------------------------------
038900080206       BEGSR GesD01;
039000080206
039100080206         // Inizializzazione videata
039200080414
039300080414         if  $inzd01 = *on;
039400130123         // Verifico che data proporre per le richieste
039500130123           exsr Propdata  ;
039600130123
039700130128           dataeur=%date(dataMinima)  ;
039800130128           v01dtn=%dec(dataeur)    ;
039900130123
040000130130           if  simfel>0  ;
040100130130           v01tfp=simfel  ;
040200130122           chain  simfel  azorg01l   ;
040300130122           if %found(azorg01l) ;
040400130130             v01dtfp=orgdes  ;
040500130122           endif  ;
040600130130           else  ;
040700130130           clear v01tfp  ;
040800130130           clear v01dtfp  ;
040900130130           endif  ;
041000130122
041100080414          $Inzd01=*off;
041200080411         endif ;
041300080206
041400080206         // Emissione testata
041500080411         if  errGenerico = *off;
041600130123          VisualDTN=*off  ;
041700130129          clear V01DRIC   ;
041800130129
041900130121           write ls50T01;
042000080411         endif;
042100080206
042200080206         // Emissione videata
042300130121         exfmt ls50D01;
042400080411
042500130122         clear V1cmsg;
042600130121         clear errDTN    ;
042700130130         clear errTFP    ;
042800080411         clear errGenerico;
042900080207
043000080206         select;
043100080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
043200080206           when  $ErrGrave = *on;
043300080206             $Fine  = *on;
043400080411
043500080206         // F3=Fine
043600080208           when dsp_aid = c_F03;
043700080409           $Fine = *on;
043800080411
043900130121           other  ;
044000080411             exsr Contrd01 ;
044100080411
044200080414         // Avanzamento se non errori
044300080414
044400080411             if errGenerico=*off;
044500080415
044600130123            //  Inserimento
044700130123              if   dsp_aid = c_F15;
044800130130              // non lo posso fare se data < della minima proposta
044900130130              if amgdtn<dataMinima  ;
045000130130               errDTN      = *on;
045100130130               v1cmsg = Msg(09);
045200130130               errMessage  = *on;
045300130130               errGenerico = *on;
045400130130               leavesr;
045500130130              endif  ;
045600130130
045700130130              // Mi salvo da quale video vengo
045800130130              savVideo=$Video  ;
045900130123              $Video = 'S3';
046000130123              $InzS03 = *on;
046100130123              else  ;
046200130130            //  Visualizzo richieste inserite ;
046300080411              $Video = 'S2';
046400080411              $InzS02 = *on;
046500130122             endif   ;
046600130123
046700130123             endif   ;
046800130122             endsl  ;
046900080603
047000080206       ENDSR;
047100080411       //--------------------------------------------------------------
047200080411       //?Controllo campi 1 videata
047300080411       //--------------------------------------------------------------
047400080411       BEGSR ContrD01;
047500080415
047600130130       // Terminal di partenza
047700130130       clear v01dtfp ;
047800130130 1     if v01tfp>0  ;
047900130130          chain v01tfp azorg01l  ;
048000130130 2        if %found(azorg01l)  and (orgfag='F' or
048100130130                orgfag='A')  and orgfva=' '  ;
048200130130           v01dtfp=orgdes  ;
048300130130 x2        else  ;
048400130130           errTFP      = *on;
048500130130           v1cmsg = Msg(08);
048600130130           errMessage  = *on;
048700130130           errGenerico = *on;
048800130130           leavesr;
048900130130 2        endif            ;
049000130130 1     endif  ;
049100130130
049200130130       // Data traino
049300130121       clear   amgdtn   ;
049400130121         g02dat=v01dtn    ;
049500130121         callp xsrda8 (wlbdat)  ;
049600130121          if g02err='1'      ;
049700130121           v1cmsg = Msg(01);
049800130121           errDTN      = *on;
049900130121           errMessage  = *on;
050000130121           errGenerico = *on;
050100130121           leavesr;
050200130121          endif            ;
050300130121
050400130121          amgdtn=g02inv  ;
050500130123          v01dtn=g02dat  ;
050600130208
050700130208          // non si accettano giorno festivi
050800130208          kdata =g02inv         ;
050900130208          ktfp=68  ;
051000130208          clear ktfa  ;
051100130208
051200130208          chain  (ktfp:ktfa:kaaa:kmmm) azcln01l  ;
051300130208
051400130208          if %found(azcln01l)   and (mat(kgg)<>' ' or
051500130208             pom(kgg) <>' ')  ;
051600130208           v1cmsg = Msg(11);
051700130208           errDTN      = *on;
051800130208           errMessage  = *on;
051900130208           errGenerico = *on;
052000130208           leavesr;
052100130208          endif            ;
052200130208
052300130121
052400080411       ENDSR;
052500080411       //--------------------------------------------------------------
052600080523       //?Gestione videata S02   - SCelta potenziali
052700080411       //--------------------------------------------------------------
052800080411       BEGSR GesS02;
052900080411
053000080411         // Inizializzazione videata
053100080411         if  $InzS02 = *on;
053200080411            exsr InzS02;
053300080411             $InzS02  = *off;
053400080411         endif;
053500080411
053600080411         // Posizionamento cursore
053700130326         if Norec=' '  ;
053800130326
053900080411         if  C02csr  > *zero;
054000130123           sfldsp_n=*off;
054100080411           C02rcd = C02csr;
054200130326         endif    ;
054300130326
054400130326         else;
054500080411
054600130123         // Se non è stato caricato nulla emetto sfl vuoto
054700130123           sfldsp_n=*on ;
054800080411         endif;
054900080411
055000080411
055100080411         // Emissione Testata e Piede con tasti funzionali abilitati
055200080604         if  errGenerico = *off or ricInt ='S'  ;
055300130121           write  ls50T01;
055400130121           write  ls50P02;
055500130123           if sfldsp_n=*on  ;
055600130123           write  ls50d04;
055700130123           endif  ;
055800130123
055900080604           clear  ricInt   ;
056000080411         endif;
056100080411
056200080411         // Emissione videata
056300130121         exfmt  ls50C02;
056400080411
056500080411         reset errMessage;
056600080411         reset errGenerico;
056700130122         clear V1cmsg;
056800080523         clear errScelta ;
056900080411
057000080411         SELECT;
057100080411
057200080411         // - F3=Fine
057300080411           WHEN  dsp_aid = c_F03;
057400080411            $Fine = *on;
057500080411
057600080411         // - F12=Ritorno
057700080411           WHEN  dsp_aid = c_F12;
057800080411           $Video = 'D1';
057900080411
058000130122           WHEN  dsp_aid = c_F15;
058100130130
058200130130              // non lo posso fare se data < della minima proposta
058300130130              if amgdtn<dataMinima  ;
058400130130               errDTN      = *on;
058500130130               v1cmsg = Msg(09);
058600130130               errMessage  = *on;
058700130130               errGenerico = *on;
058800130130               leavesr;
058900130130              endif  ;
059000130130
059100130130           savVideo=$Video  ;
059200130122           $Video = 'S3';
059300130123           $inzs03=*on  ;
059400130123
059500080414         // - Roll-Up
059600080414           WHEN  dsp_aid = c_RollUp;
059700080414           // - Verifica se raggiunto il limite di records registrabili
059800080414           //   nel subfile (9999)
059900080414             if  S02nrr = *hival;
060000080414               errMessage  = *on;
060100080414               errGenerico = *on;
060200130122               V1cmsg = Msg(03);
060300080414             else;
060400080414               exsr RollUpS02;
060500080414             endif;
060600080414
060700080411         // Invio
060800080411           OTHER;
060900130326
061000130326         // non eseguo i controlli se non ho caricato nemmeno 1 rec
061100130326             if norec=' '  ;
061200130326               exsr ContrS02  ;
061300130326               if  errGenerico = *on;
061400130326                 leavesr;
061500130326               endif;
061600130326             endif;
061700080411
061800080411         ENDSL;
061900080411
062000080411       ENDSR;
062100080411       //--------------------------------------------------------------
062200080414       //?Inizializzazione sfl 2
062300080411       //--------------------------------------------------------------
062400080411       BEGSR InzS02;
062500080411
062600080411       // Pulizia subfile
062700080411         SflDsp_N    = *on;
062800080411         SflDspCtl_N = *on;
062900130121         write  ls50C02;
063000080411         SflDspCtl_N = *off;
063100080411         SflEnd      = *off;
063200080411
063300130326         clear Norec   ;
063400130123         VisualDTN=*on   ;
063500080530         clear W_SflPag;
063600080414         $EoF = *off;
063700080411         clear C02rcd;
063800080414         clear S02nrr;
063900130122         clear V1cmsg;
064000080411         errMessage  = *off;
064100080411         errGenerico = *off;
064200080415         errscelta=*off  ;
064300080411
064400130130         // Imposto il campo terminal in gesitone
064500130130         v01fgs=v01tfp   ;
064600130130         v01dfgs=v01dtfp  ;
064700130130
064800080411       // Carico pagina
064900130130        if  v01fgs>0   ;
065000130122          setll  (v01fgs:amgdtn)    fnfgb02l;
065100130122          reade  (v01fgs:amgdtn)    fnfgb02l;
065200130130          $EoF  = (%eof(fnfgb02l));
065300130130          else  ;
065400130130          setll  (amgdtn)    fnfgb03l;
065500130130          reade  (amgdtn)    fnfgb03l;
065600130130          $EoF  = (%eof(fnfgb03l));
065700130130        endif  ;
065800080411
065900080530         totrig=C_SflPAg       ;
066000080530
066100080411         dow  $EoF   = *off     and
066200080530              S02nrr < totrig  ;
066300080411           exsr RollUpS02;
066400080411         enddo;
066500080411
066600080411       // -> forza l'impaginazione sul 1° rec. del subfile
066700080530       //    se vengo dalla videata di selezioni
066800080530
066900080411         if S02nrr > *zero;
067000080530           if c02csr=0 ;
067100080411           C02rcd  = 1;
067200080411           C02csr  = 1;
067300080530           else       ;
067400110831
067500110831           // Se c02csr > del numero di record, imposto il num max
067600110831             if c02csr>s02nrr ;
067700110831              c02csr=s02nrr ;
067800110831             endif  ;
067900080530           C02rcd  = c02csr;
068000080530           endif      ;
068100080530
068200080411         else;
068300080411           clear C02rcd;
068400110831           clear C02csr;
068500130326           Norec='S'   ;
068600080411         endif;
068700080411
068800080411       ENDSR;
068900080411       //--------------------------------------------------------------
069000080411       //?Caricamento singola pagina S02
069100080411       //--------------------------------------------------------------
069200080411       BEGSR RollUpS02;
069300080411
069400080411         S02nrr    = (W_SflPag * C_SflPag);
069500080411         W_SflPag += 1;
069600080411         SflNxtChg = *off;
069700080411
069800080411         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
069900080523 1       DOW  $EoF   = *off                   and
070000080411              S02nrr < (W_SflPag * C_SflPag)  and
070100080411              S02nrr < *hival;
070200080411
070300080411         // - Caricamento dati nel record del subfile
070400080414         clear s02sce  ;
070500130122         g02inv=fgbdtn   ;
070600130122         g02err='3'  ;
070700130122         callp xsrda8 (wlbdat)  ;
070800130128          dataeur=%date(g02dat:*eur)       ;
070900130122          datadmy=dataeur  ;
071000130122          s02dtn=%dec(datadmy)         ;
071100130122          h02dtn=fgbdtn          ;
071200130130          s02tfp=fgblnp ;
071300130130          s02aar=fgbaar ;
071400130129          s02nur=%trim(%editc(fgbnur:'2'));
071500130129          h02nur=fgbnur   ;
071600130129          s02tfa=fgbtfa   ;
071700130122          chain fgbtfa azorg01l  ;
071800130122          if %found(azorg01l)  ;
071900130122           s02dtfa=orgdes  ;
072000130122          endif  ;
072100130128          h02tmz=fgbtmz  ;
072200130122         clear tblkey  ;
072300130122         tblkey=fgbtmz      ;
072400130122         chain  (1:'TU':tblkey) tabel00f;
072500130122 6       if %found(tabel00f);
072600130128          s02dtmz=fgbtmz+'-'+tbluni  ;
072700130122         endif ;
072800130122         s02num=1  ;
072900130128         s02pru=fgbpru  ;
073000130129         s02ftr=fgbftr  ;
073100130128         s02not=fgbnot  ;
073200130128
073300130128         g02inv=fgbdim   ;
073400130128         g02err='3'  ;
073500130128         callp xsrda8 (wlbdat)  ;
073600130128          dataeur=%date(g02dat:*eur)       ;
073700130128          datadmy=dataeur  ;
073800130128          s02dim=%dec(datadmy)         ;
073900080411
074000080411           S02nrr += 1;
074100130121           write ls50S02;
074200080411
074300080411         // - Lettura record successivo nell'archivio
074400130130         if v01fgs>0  ;
074500130122          reade  (v01fgs:amgdtn)    fnfgb02l;
074600130122          $EoF  = (%eof(fnfgb02l));
074700130130          else  ;
074800130130          reade  (amgdtn)    fnfgb03l;
074900130130          $EoF  = (%eof(fnfgb03l));
075000130130         endif  ;
075100080411
075200080523 1       ENDDO;
075300080411
075400080411         // Attivazione (eventuale) del SFLEND
075500080411         SflEnd   = ($EoF = *on);
075600080411
075700080411         // Posizionamento cursore al 1° rcd della pagina
075800080523 1       if  S02nrr > *zero;
075900080414           wPag   = S02nrr / C_SflPag;
076000080414           wRig   = S02nrr - (C_SflPag * wPag);
076100080411           C02rcd = wPag * C_SflPag;
076200080523 2         if  wRig > *zeros;
076300080415             C02rcd = C02rcd + 1;
076400080411           else;
076500080411             C02rcd = C02rcd - C_SflPag + 1;
076600080523 2         endif;
076700080523 x1      else;
076800080411           clear  C02rcd;
076900080523 1       endif;
077000080411
077100080411         C02csr = C02rcd;
077200080411
077300080411       ENDSR;
077400080414       //--------------------------------------------------------------
077500080414       //?controllo scelta SFL 2
077600080414       //--------------------------------------------------------------
077700080523       BEGSR contrS02 ;
077800080414
077900130130       ErrScelta=*off ;
078000080415
078100130121       readc ls50s02;
078200130121 1     DOW  NOT  %eof(fnls50D);
078300080414
078400130129       // non è possibile modificare: il record è già stato scaricato
078500130129 2     if s02sce=  '2' and s02ftr<>' '   ;
078600130129           errScelta   = *on;
078700130129           errMessage  = *on;
078800130129           errGenerico = *on;
078900130129           V1cmsg = Msg(07);
079000130129           exsr errSFL2  ;
079100130129           leavesr       ;
079200130129       endif     ;
079300130130       // In SEDE non è possibile modificare
079400130130 2     if s02sce=  '2' and simfel=0      ;
079500130130           errScelta   = *on;
079600130130           errMessage  = *on;
079700130130           errGenerico = *on;
079800130130           V1cmsg = Msg(10);
079900130130           exsr errSFL2  ;
080000130130           leavesr       ;
080100130130       endif     ;
080200130129
080300130129       //
080400130129 2     if s02sce<> ' ' ;
080500130129         $Video = 'D5';
080600130129          $inzd05=*on;
080700130129
080800130129         dow $Video='D5'  ;
080900130129          exsr Gesd05     ;
081000130129         enddo ;
081100130130          InterD05=*off ;
081200130130          Protd05 =*off ;
081300130129          clear s02sce    ;
081400080526 2     endif ;
081500130130
081600130130          exsr errSFL2  ;
081700080414
081800151016        // se devo tornare in prima videata non eseguo la READC
081900151016       if $Video='D1'  ;
082000151016        leave  ;
082100151016       endif  ;
082200151016
082300130129       readc ls50s02;
082400130122       enddo  ;
082500130122
082600130122       ENDSR   ;
082700080603       //--------------------------------------------------------------
082800080603       //?Aggiornamento sfl per errore
082900080603       //--------------------------------------------------------------
083000080603       BEGSR ErrSFL2 ;
083100080603         SflNxtChg = *on ;
083200130121         update ls50s02;
083300080603
083400130326         if Errscelta=*on AND s02nrr>0   ;
083500130326          c02csr=s02nrr   ;
083600130326         endif  ;
083700080603         errscelta=*off  ;
083800080603         SflNxtChg = *off ;
083900080603       ENDSR;
084000080411
084100130122       //--------------------------------------------------------------
084200130122       //?Gestione videata S03   - Inserimento
084300130122       //--------------------------------------------------------------
084400130122       BEGSR GesS03;
084500130122
084600130122         // Inizializzazione videata
084700130123 1       if  $InzS03 = *on;
084800130122            exsr InzS03;
084900130122             $InzS03  = *off;
085000130123 1       endif;
085100130122
085200130123 1       // Posizionamento cursore
085300130122         if  C03csr  > *zero;
085400130122           C03rcd = C03csr;
085500130123 1       endif    ;
085600130122
085700130122         // Posizionamento cursore
085800130122           sfldsp_n=*off;
085900130122
086000130122         // Emissione Testata e Piede con tasti funzionali abilitati
086100130128 1       if  errGenerico = *off or ricInt='S' ;
086200130122           write  ls50T01;
086300130122           write  ls50P03;
086400130128           clear ricInt  ;
086500130123 1       endif;
086600130122
086700130122         // Emissione videata
086800130122         exfmt  ls50C03;
086900130122
087000130122         reset errMessage;
087100130122         reset errGenerico;
087200130122         clear V1cmsg;
087300130122
087400130123 1       SELECT;
087500130122
087600130122         // - F3=Fine
087700130122           WHEN  dsp_aid = c_F03;
087800130122            $Fine = *on;
087900130122
088000130122         // - F12=Ritorno
088100130122           WHEN  dsp_aid = c_F12;
088200130130           $Video = savVideo  ;
088300130130           $inzs02=*on  ;
088400130130           $inzd01=*on  ;
088500130122
088600130122         // Invio
088700130122           OTHER;
088800130122             exsr ContrS03  ;
088900130122             if  errGenerico = *on;
089000130122               leavesr;
089100130122             endif;
089200130123
089300130123 2         if    dsp_aid = c_F06;
089400130129            exsr  AggiorS03 ;
089500130123
089600130123 3           if  errGenerico = *on;
089700130123               leavesr;
089800130123             else  ;
089900130130              $Video = savVideo  ;
090000130130              $inzs02=*on  ;
090100130130              $inzd01=*on  ;
090200130123 3           endif;
090300130123 2         endif;
090400130122
090500130123 1       ENDSL;
090600130122
090700130122       ENDSR;
090800130122       //--------------------------------------------------------------
090900130123       //?Inizializzazione sfl 3 inserimento
091000130122       //--------------------------------------------------------------
091100130122       BEGSR InzS03;
091200130122
091300130130         // Imposto il campo terminal in gesitone
091400130130         v01fgs=v01tfp   ;
091500130130         v01dfgs=v01dtfp  ;
091600130130
091700130128         VisualDTN=*on   ;
091800130129         v01dric=' INSERIMENTO '  ;
091900130128
092000130122       // Pulizia subfile
092100130122         SflDsp_N    = *on;
092200130122         SflDspCtl_N = *on;
092300130122         write  ls50C03;
092400130122         SflDspCtl_N = *off;
092500130218         SflEnd3     = *off;
092600130122
092700130122         clear C03rcd;
092800130122         clear S03nrr;
092900130122         clear V1cmsg;
093000130122         errMessage  = *off;
093100130122         errGenerico = *off;
093200130122
093300130123         clear s03tfa  ;
093400130123         clear s03dtfa  ;
093500130123         clear s03tmz   ;
093600130123         clear s03dtmz   ;
093700130128         clear s03not1   ;
093800130128         clear s03not2   ;
093900130122
094000130218         dow  s03nrr < 32 ;
094100130123         s03nrr=s03nrr+1  ;
094200130123         write ls50s03    ;
094300130122         enddo;
094400130218         SflEnd3     = *on ;
094500130122
094600130123         s03nrr=1 ;
094700130128         c03csr=1 ;
094800130122
094900130122       ENDSR;
095000130123       //--------------------------------------------------------------
095100130123       //?controllo dati   SFL 3
095200130123       //--------------------------------------------------------------
095300130123       BEGSR contrS03 ;
095400130123
095500130123       clear ErrTFA    ;
095600130123       clear ErrTMZ    ;
095700130123
095800130123       readc ls50s03;
095900130123 1     DOW  NOT  %eof(fnls50D);
096000130123
096100130128 2     if s03tfa>0   or s03tmz<>' ' or s03not1<>*blanks
096200130128            or s03not2<>*blanks    ;
096300130128
096400130123       chain   s03tfa    azorg01l  ;
096500130123
096600130128  3    if  not %found(azorg01l)  or orgfag<>'F'
096700130123           and orgfag<>'A' or   orgfva<>' '  ;
096800130123           errTFA=*on  ;
096900130123           errMessage  = *on;
097000130123           errGenerico = *on;
097100130123           V1cmsg = Msg(04);
097200130326           c03csr=s03nrr ;
097300130123           SflNxtChg = *on ;
097400130123           update ls50s03  ;
097500130123           SflNxtChg = *off;
097600130123           leavesr;
097700130128  3    endif  ;
097800130123
097900130123       s03dtfa=orgdes  ;
098000130128
098100130128       // tipo mezzo
098200130128  3    if s03tmz='?'  ;
098300130128
098400130128       clear x§tkey  ;
098500130128       x§tcod='TU'  ;
098600130128       callp x§taber    (1:x§tcod:x§tkey:x§tdes ) ;
098700130128       s03tmz=x§tkey  ;
098800130128       s03dtmz=x§tdes  ;
098900130128           ricint='S'  ;
099000130128           errTmz      = *on;
099100130128           errGenerico = *on;
099200130128           SflNxtChg = *on ;
099300130326           c03csr=s03nrr ;
099400130128           update ls50s03  ;
099500130128           SflNxtChg = *off;
099600130128           leavesr;
099700130128 3     endif  ;
099800130128
099900130128         clear tblkey  ;
100000130128         tblkey=s03tmz      ;
100100130128         chain  (1:'TU':tblkey) tabel00f;
100200130128 3       if s03tmz=' ' or not %found(tabel00f) or
100300130128                     tblflg<>' '  ;
100400130128           errTmz      = *on;
100500130128           errGenerico = *on;
100600130128           errMessage  = *on;
100700130326           c03csr=s03nrr ;
100800130128           V1cmsg = Msg(06);
100900130128           SflNxtChg = *on ;
101000130128           update ls50s03  ;
101100130128           SflNxtChg = *off;
101200130128           leavesr;
101300130128         else  ;
101400130128         dstu=tbluni   ;
101500130128
101600130128        // se non utilizzabile nei BIs errore
101700130128        s03dtmz=§tudes   ;
101800130128        if §tuutib<>'S'   ;
101900130128           errTmz      = *on;
102000130128           errMessage  = *on;
102100130128           errGenerico = *on;
102200130326           c03csr=s03nrr ;
102300130128           V1cmsg = Msg(06);
102400130128           SflNxtChg = *on ;
102500130128           update ls50s03  ;
102600130128           SflNxtChg = *off;
102700130128           leavesr;
102800130128        endif  ;
102900130128 3     endif  ;
103000130128
103100130128
103200130128          SflNxtChg = *on ;
103300130128           update ls50s03  ;
103400130128          SflNxtChg = *off;
103500130326           c03csr=s03nrr ;
103600130123
103700130128  x2   else  ;
103800130123          clear s03dtfa  ;
103900130123          clear s03dtmz  ;
104000130123          SflNxtChg = *off;
104100130123           update ls50s03  ;
104200130128  2    endif  ;
104300130123
104400130123       readc ls50s03;
104500130128  1    enddo  ;
104600130123
104700130123       ENDSR   ;
104800130123       //--------------------------------------------------------------
104900130129       //?Aggiornamento richieste    S03
105000130123       //--------------------------------------------------------------
105100130129       BEGSR AggiorS03;
105200130123       readc ls50s03;
105300130123 1     DOW  NOT  %eof(fnls50D);
105400130123
105500130326 2     if s03tfa>0   ;
105600130123       clear fnfgb000  ;
105700130123         fgbpru=knmus  ;
105800130123         dataiso=datasys         ;
105900130123         fgbdim=%dec(datasys)      ;
106000130123         fgbhim=%dec(orasys)      ;
106100130123         fgbaar=%int(%subst(%editc(fgbdim :'X'):1:4)) ;
106200130123         // Stacco numero richiesta  ;
106300130326         clear wtrov  ;
106400130326  3      dow wtrov=' '  ;
106500130326
106600130123         clear trul33ds  ;
106700130123         i33cnu=042  ;
106800130123         i33num=1  ;
106900130123         i33aaa=fgbaar  ;
107000130123         kpjbu= trul33ds  ;
107100130123
107200130123         callp trul33r (kpjba)   ;
107300130123         trul33ds=kpjbu  ;
107400130326 4       if o33err>0     ;
107500130123           errTFA=*on  ;
107600130123           errMessage  = *on;
107700130123           errGenerico = *on;
107800130123           V1cmsg = Msg(05);
107900130123           SflNxtChg = *on ;
108000130123           update ls50s03  ;
108100130123           SflNxtChg = *off;
108200130123           leavesr;
108300130326 x4       else  ;
108400130123          fgbnur=o33nri   ;
108500130326          // Prer sicurezza verifico se già presnete il numero
108600130326          //  ne stacco un altro
108700130326          setll (fgbaar:fgbnur)   fnfgb01l   ;
108800130326  5        If not %equal(fnfgb01l)  ;
108900130326            wtrov='S'  ;
109000130326  5        endif  ;
109100130326  4       endif  ;
109200130326  3       enddo  ;
109300130326
109400130123          fgbdtn=amgdtn  ;
109500130123          fgblnp=v01fgs  ;
109600130123          fgbtfa=s03tfa ;
109700130123          fgbtmz=s03tmz ;
109800130128          fgbnot=s03not1+s03not2;
109900130123          write fnfgb000   ;
110000130326  2       endif  ;
110100130123
110200130123         readc ls50s03;
110300130326  1    enddo   ;
110400130123
110500130123       ENDSR   ;
110600130123       //--------------------------------------------------------------
110700130123       //?Proposta data traini
110800130123       //--------------------------------------------------------------
110900130123       BEGSR PropData   ;
111000130128         data_BLOC=*off ;
111100130128
111200130130       // se sono in sede propongo sempre udate
111300130130       if simfel = 0  ;
111400130130           dataiso=datasys         ;
111500130130           ProtFIL=*off   ;
111600130130       else  ;
111700130130
111800130130           ProtFIL=*on    ;
111900130130
112000130123         Whim=%dec(orasys)      ;
112100130123          if whim<Woralim  ;
112200130123           dataiso=datasys         ;
112300130123          else  ;
112400130123           dataiso=datasys+ %days(1) ;
112500130128           data_BLOC=*on  ;
112600130123          endif   ;
112700130123
112800130123          // se Festivo o sabato lavorativo vado al successivo
112900130123          kdata =%dec(dataiso)  ;
113000130123          ktfp=68  ;
113100130123          clear ktfa  ;
113200130123          wfest='F'  ;
113300130123
113400130123          dow wfest='F'  ;
113500130123          chain  (ktfp:ktfa:kaaa:kmmm) azcln01l  ;
113600130123
113700130123          if %found(azcln01l)   and (mat(kgg)<>' ' or
113800130123             pom(kgg) <>' ')  ;
113900130123
114000130123           dataiso=dataiso+ %days(1) ;
114100130123           kdata =%dec(dataiso)  ;
114200130123          else  ;
114300130123          clear   wFEST  ;
114400130123          endif  ;
114500130123          enddo  ;
114600130130          endif  ;
114700130123
114800130123         // Memorizzo la data limite di inserimento richieste
114900130123         DataMinima=%dec(dataiso)  ;
115000130123
115100130123         ENDSR  ;
115200130129       //--------------------------------------------------------------
115300130129       //?Gestione videata D05
115400130129       //--------------------------------------------------------------
115500130129         BEGSR GesD05;
115600130129
115700130129         // Inizializzazione videata
115800130129
115900130129         if  $inzd05 = *on;
116000130129         // Verifico che data proporre per le richieste
116100130129           exsr Inzd05    ;
116200130129          $Inzd05=*off      ;
116300130129         endif ;
116400130129
116500130129         // Emissione testata
116600130129         if  errGenerico = *off or ricInt='S';
116700130129           write ls50T01;
116800130129           clear ricint   ;
116900130129         endif;
117000130129
117100130129         // Emissione videata
117200151016         //exfmt ls50D05;
117300151016         write ls50d05 ;
117400151016         read(e)  fnls50d   ;
117500151016         // passato 50 secondi esce dalla videata
117600151016         //  vado in prima videata
117700151016           if   %error  ;
117800151016               $Video = 'D1';
117900151016               Errgenerico=*off  ;
118000151016               ErrMessage =*off  ;
118100151016               unlock fnfgb01l   ;
118200151016               leavesr           ;
118300151016           endif   ;
118400130129
118500130129         clear V1cmsg;
118600130129         clear errTMZ    ;
118700130129         clear errTFA    ;
118800130129         clear errGenerico;
118900130129
119000130129    1    select;
119100130129
119200130129         // F12=ritorno
119300130129           when dsp_aid = c_F12;
119400130129               $Video = 'S2';
119500130130               Errgenerico=*off  ;
119600130130               ErrMessage =*off  ;
119700151016               unlock fnfgb01l   ;
119800130129
119900130129           other  ;
120000130129             exsr Contrd05 ;
120100130129
120200130130             if InterD05=*on  ;
120300130130             ErrMessage=*off  ;
120400130130             ErrGenerico=*off  ;
120500130130             endif    ;
120600130130
120700130129    2        if  errGenerico = *on;
120800130129               leavesr;
120900130129    2        endif;
121000130129
121100130130    2      if   InterD05=*on    ;
121200130129               $Video = 'S2';
121300130129    2      endif  ;
121400130129
121500130129 2         if    dsp_aid = c_F16;
121600130129            delete  fnfgb000  ;
121700130129               $Inzs02=*on  ;
121800130129               $Video = 'S2';
121900130129 2           endif;
122000130129
122100130129 2         if    dsp_aid = c_F06;
122200130129            exsr  AggiorD05  ;
122300130129
122400130129 3           if  errGenerico = *on;
122500130129               leavesr;
122600130129             else  ;
122700130129               $Video = 'S2';
122800130129 3           endif;
122900130129 2           endif;
123000130129
123100130129 1           endsl  ;
123200130129
123300130129          ENDSR;
123400130129       //--------------------------------------------------------------
123500130129       //?Inizializzazione VIDEATA 5
123600130129       //--------------------------------------------------------------
123700130129       BEGSR Inzd05;
123800130129
123900130129       if s02sce='2'  ;
124000130129       v01dric= ' MANUTENZIONE '  ;
124100130129       ENDIF  ;
124200130129       if s02sce='5'  ;
124300130129       v01dric= 'INTERROGAZIONE'  ;
124400130130       INTERD05=*ON  ;
124500130129       PROTD05=*ON  ;
124600130129       ENDIF  ;
124700130129
124800130129       // Carico in videata il record selezionato
124900151016       if s02sce='2'  ;
125000151016       chain    (s02aar:h02nur)   fnfgb01l  ;
125100151016       else ;
125200151016       chain(n) (s02aar:h02nur)   fnfgb01l  ;
125300151016       endif ;
125400130129       if %found(fnfgb01l)  ;
125500130130
125600130130       d05tfp=fgblnp ;
125700130130       chain   fgblnp   azorg01l     ;
125800130130       d05dtfp=orgdes ;
125900130129       d05aar=fgbaar ;
126000130129       d05nur=%trim(%editc(fgbnur:'2'));
126100130129       d05pru=fgbpru  ;
126200130129       d05him=fgbhim ;
126300130129       d05not=fgbnot ;
126400130129       d05tfa=fgbtfa ;
126500130129       chain   fgbtfa   azorg01l     ;
126600130129       d05dtfa=orgdes ;
126700130129
126800130129       d05tmz=fgbtmz  ;
126900130129         clear tblkey  ;
127000130129         tblkey=fgbtmz      ;
127100130129         chain  (1:'TU':tblkey) tabel00f;
127200130129 6       if %found(tabel00f);
127300130129          d05dtmz=tbluni  ;
127400130129         endif ;
127500130129
127600130129         g02inv=fgbdtn   ;
127700130129         g02err='3'  ;
127800130129         callp xsrda8 (wlbdat)  ;
127900130129         d05dtn=g02dat   ;
128000130129
128100130129         g02inv=fgbdim   ;
128200130129         g02err='3'  ;
128300130129         callp xsrda8 (wlbdat)  ;
128400130129         d05dim=g02dat   ;
128500130129
128600130129         // Se è già stato scaricato dalla sede proteggo
128700130129           if fgbftr<>' '   ;
128800130129            Protd05 = *on ;
128900130129           endif   ;
129000130205
129100130205         // Data ora scarico SEDE
129200130205         if fgbdtr>0  ;
129300130205          dataiso=%date(fgbdtr:*iso)  ;
129400130205          dataeur=dataiso  ;
129500130205          d05dsca=%dec(dataeur)  ;
129600130205         else  ;
129700130205          clear d05dsca  ;
129800130205         endif  ;
129900130129
130000130205         d05hsca=fgbhtr  ;
130100130205
130200130129         else  ;
130300130129         // errore non trovato
130400130129         Errgenerico=*on  ;
130500130129         endif  ;
130600130129
130700130129       ENDSR  ;
130800130129       //--------------------------------------------------------------
130900130129       //?controllo dati   video 5
131000130129       //--------------------------------------------------------------
131100130129       BEGSR contrd05 ;
131200130129
131300130129       chain   d05tfa    azorg01l  ;
131400130129
131500130129  3    if  not %found(azorg01l)  or orgfag<>'F'
131600130129           and orgfag<>'A' or   orgfva<>' '  ;
131700130129           errTFA=*on  ;
131800130129           errMessage  = *on;
131900130129           errGenerico = *on;
132000130129           V1cmsg = Msg(04);
132100130129           leavesr;
132200130129  3    endif  ;
132300130129
132400130129       d05dtfa=orgdes  ;
132500130129
132600130129       // tipo mezzo
132700130129  3    if d05tmz='?'  ;
132800130129
132900130129       clear x§tkey  ;
133000130129       x§tcod='TU'  ;
133100130129       callp x§taber    (1:x§tcod:x§tkey:x§tdes ) ;
133200151016       d05tmz=%subst(x§tkey:1:1)  ;
133300151016       d05dtmz=x§tdes  ;
133400130129           ricint='S'  ;
133500130129           errTmz      = *on;
133600130129           errGenerico = *on;
133700130129           leavesr;
133800130129 3     endif  ;
133900130129
134000130129         clear tblkey  ;
134100130129         tblkey=d05tmz      ;
134200130129         chain  (1:'TU':tblkey) tabel00f;
134300130129 3       if d05tmz=' ' or not %found(tabel00f) or
134400130129                     tblflg<>' '  ;
134500130129           errTmz      = *on;
134600130129           errGenerico = *on;
134700130129           errMessage  = *on;
134800130129           V1cmsg = Msg(06);
134900130129           leavesr;
135000130129         else  ;
135100130129         dstu=tbluni   ;
135200130129
135300130129        // se non utilizzabile nei BIs errore
135400130129        d05dtmz=§tudes   ;
135500130129        if §tuutib<>'S'   ;
135600130129           errTmz      = *on;
135700130129           errMessage  = *on;
135800130129           errGenerico = *on;
135900130129           V1cmsg = Msg(06);
136000130129           leavesr;
136100130129        endif  ;
136200130129 3     endif  ;
136300130129
136400130129       ENDSR   ;
136500130129       //--------------------------------------------------------------
136600130129       //?Aggiornoa richeista singola    D05
136700130129       //--------------------------------------------------------------
136800130129       BEGSR AggiorD05 ;
136900130129
137000130129         fgbpru=knmus  ;
137100130129         dataiso=datasys         ;
137200130129         fgbdim=%dec(datasys)      ;
137300130129         fgbhim=%dec(orasys)      ;
137400130129          fgbtfa=d05tfa ;
137500130129          fgbtmz=D05tmz ;
137600130129          fgbnot=D05not         ;
137700130129          update fnfgb000   ;
137800130129
137900130129           $Inzs02=*on  ;
138000130129
138100130129       ENDSR  ;
138200080206       //--------------------------------------------------------------
138300080206       //?Operazioni finali.
138400080206       //--------------------------------------------------------------
138500080206       BEGSR RoutEnd;
138600080410
138700080206         *inLR = *on;
138800080206         return;
138900080206
139000080206       ENDSR;
139100080206
139200080206      /end-free
139300080206
139400080206       //--------------------------------------------------------------
139500080206       //?Schiere a tempo di compilazione.
139600080206       //--------------------------------------------------------------
139700080206
139800080410** - MSG ------------------------------------------------------------------ -*
139900130121Data errata
140000130122Non è stata trovata alcuna richiesta di BIS nella data indicata
140100130123
140200130123Terminal/linea di Arrivo errata
140300130123Impossibile assegnare nuovo numero richiesta: telefonare al CED!!
140400130128Tipo automezzo errato o non gestito nella richiesta BIS
140500130207Impossibile modificare la richiesta; già elaborata da sede
140600130130Terminal di partenza errato                                                    8
140700130130Impossibile fare nuovi inserimenti in data già bloccata:scrivere mail a sede   9
140800130130Opzione non ammessa                                                           10
140900130208Impossibile fare nuovi inserimenti in giorno festivo                          11
