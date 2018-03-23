000100071210      //---------------------------------------------------------------
000200120605      //?TNSB71R - Rigenerazione Documenti Mitt. x Cliente
000300071210      //---------------------------------------------------------------
000400090127
000500071210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040720
000700071210      //---------------------------------------------------------------
000800071210      //?Dichiarazione file.
000900071210      //---------------------------------------------------------------
001000040720
001100120605     fTNSB71D   cf   e             workstn
001200071210     f                                     indds(IndDspF)
001300120605     ftijdc00f  o    e             disk
001400040720
001500071210      //---------------------------------------------------------------
001600071210      //?Definizione costanti.
001700071210      //---------------------------------------------------------------
001800090127
001900071210      // - Costante per controllo "caratteri solo numerici"
002000071210     d DigitN          c                   const('0123456789')
002100090127
002200090127     d c_Ktrc          c                   const('A')
002300071210
002400071210      //---------------------------------------------------------------
002500071210      //?Definizione schiere.
002600071210      //---------------------------------------------------------------
002700090127
002800071210      // - Messaggi a video
002900090311     d $Msg            s             78    dim(15) ctdata  perrcd(1)
003000071210
003100071210      //---------------------------------------------------------------
003200071210      //?Definizione aree dati.
003300071210      //---------------------------------------------------------------
003400090127
003500071210      // - Dati utente
003600071210     d §AzUte        e ds                  extname(AZUTE00F)
003700071210     d                                     dtaara
003800071210     d §DatiUte      e ds                  extname(dDatiUte)
003900071210     d                                     dtaara
004000071210
004100071210      //---------------------------------------------------------------
004200071210      //?Definizione strutture dati.
004300071210      //---------------------------------------------------------------
004400090127
004500071210      // - Status
004600071210     d Psds           sds
004700071210     d   SDSpgm          *proc
004800071210     d   SDSjob              244    253                                         Job name
004900071210
005000071210      // - Indicatori su DspF
005100071218     d IndDspF         ds
005200110211        // - Indicatori di abilitazione tasto funzione
005300110211     d   F11Attivo                    1n   overlay(IndDspF:11)
005400071210        // - Indicatori di errore
005500071210     d   errMessage                   1n   overlay(IndDspF:28)
005600120606     d   PosCurKsc                    1n   overlay(IndDspF:51)
005700120606     d   PosCurFmi                    1n   overlay(IndDspF:52)
005800120606     d   PosCurDir                    1n   overlay(IndDspF:53)
005900120606     d   PosCurTpi                    1n   overlay(IndDspF:54)
006000120606     d   PosCurDcd                    1n   overlay(IndDspF:55)
006100120606     d   PosCurDca                    1n   overlay(IndDspF:56)
006200120606     d   PosCurDsd                    1n   overlay(IndDspF:57)
006300120606     d   PosCurDsa                    1n   overlay(IndDspF:58)
006400120607     d   PosCurPag                    1n   overlay(IndDspF:59)
006500071210     d   errGenerico                  1n   overlay(IndDspF:99)
006600110211
006700110211     d WindDspF        ds                  inz
006800110211     d   WindDspFa             1     49    inz(*zero)
006900110211     d   WindDspFb            50     99    inz(*zero)
007000071210
007100071210      // - Parametri ricevuti
007200040720     d KPJBA         e ds
007300120605     d TNSB73ds        ds                  inz
007400120605     d   d73dcd                       8  0 inz
007500120605     d   d73dca                       8  0 inz
007600120605     d   d73ksc                       7  0 inz
007700120605     d   d73imm                       1    inz
007800120605     d   d73dsd                       8  0 inz
007900120605     d   d73dsa                       8  0 inz
008000120605     d   d73dir                      30    inz
008100120605     d   d73fmi                       2    inz
008200120605     d   d73tpi                       1    inz
008300120605     d   d73ksu                       7  0 inz
008400120606     d   d73tad                       1    inz
008500120606     d   d73tadu                      1    inz
008600120606     d   d73fimp                      1    inz
008700120606     d   d73imp                      10  3 inz
008800120606     d   d73kscf                      7  0 inz
008900120606     d   d73ctr                       3    inz
009000120606     d   d73job                      16    inz
009100120606     d   d73tpt                       1    inz
009200120606     d   d73res                       1    inz
009300120606     d   d73rec                       1    inz
009400120606     d   d73csr                       1    inz
009500120606     d   d73ssr                       1    inz
009600120606     d   d73lnp                       3  0 inz
009700120607     d   d73pag                       3    inz
009800071210
009900071210      // - Reperimento dati utente
010000071210     d TIBS34ds      e ds
010100071210
010200071210      // - Gestione tabelle: controllo e ricerca
010300071210     d TIBS02ds      e ds                  inz
010400071210     d   T02mod      e                     inz('C')
010500120605     d   T02cod      e                     inz('JDC')
010600071210
010700120605      // - Tabella "JDC" = Clienti per ritorno documenti
010800120605     d dJDC          e ds                  inz
010900071217
011000071217      // - Reperimento dati utente
011100120605     d TNTBJDCds     e ds                  inz
011200120605     d   BJDCopz     e                     inz('5')
011300120605     d   BJDCerr     e                     inz(*off)
011400090316
011500090316       // - Controllo/Decodifica cliente
011600090316     d TIBS69ds      e ds                  qualified  inz
011700090316     d ds_CNACO      e ds                  extname(CNACO00F)
011800090316     d                                     qualified  inz
011900090316     d ds_CNIND      e ds                  extname(CNIND00F)
012000090316     d                                     qualified  inz
012100090316     d ds_CNCLP      e ds                  extname(CNCLP00F)
012200090316     d                                     qualified  inz
012300090316     d ds_FNCLS      e ds                  extname(FNCLS00F)
012400090316     d                                     qualified  inz
012500071210
012600071210      // - Controllo ed inversione data
012700040720     d WLBdat          ds                  inz
012800040720     d   G08dat                       8  0 inz
012900040720     d   G08inv                       8  0 inz
013000040720     d   G08err                       1    inz(*off)
013100040720     d   G08tgi                       5  0 inz
013200071210
013300071210      // - Time
013400071210     d DS_Time         ds            14    inz
013500071210     d   dsTIME_ymd                   8  0 inz
013600071210     d   dsTIME_hms                   6  0 inz
013700090429
013800090429       // - Struttura per passaggio dati ad interrogazione tabella
013900090429     d Param01         ds                  inz
014000090429     d  P01cod                        7  0 inz
014100090429     d  P01ord                        1    inz
014200090429     d  P01ksu                        7  0 inz
014300090429     d  P01ke1                        7    inz
014400090429     d  P01ke2                       15    inz
014500090429     d  P01rit                        1    inz
014600071210
014700071210      //---------------------------------------------------------------
014800071210      //?Definizione variabili globali.
014900071210      //---------------------------------------------------------------
015000090127
015100071210      // - Flags booleani
015200110211     d SeSonoIo        s              1n   inz(*off)
015300040720     d $Fine           s              1    inz(*off)
015400120605     d $FineJDC        s              1n   inz(*off)
015500040720     d $InzD01         s              1    inz(*on)
015600040720     d $InzD02         s              1    inz(*on)
015700090128     d $Parz01a        s              1    inz(*off)
015800090128     d $Parz01b        s              1    inz(*off)
015900071210
016000071210      // - Gestione video
016100071210     d $Video          s              2    inz('D1')
016200071210
016300071210      // - Comodo
016400071210     d W1Cdcd          s                   like(V1Cdcd)   inz
016500071210     d W1Cdca          s                   like(V1Cdca)   inz
016600071210     d W1Cdsd          s                   like(V1Cdsd)   inz
016700071210     d W1Cdsa          s                   like(V1Cdsa)   inz
016800071210     d WdateISO        s               d   datfmt(*ISO)   inz(*job)
016900071210     d WtimeHMS        s               t   timfmt(*HMS)   inz
017000120606     d wprogr          s              1  0 inz
017100120606     d widjob15        s             15a
017200120606     d widjob          s             16a
017300090127
017400090127      //---------------------------------------------------------------
017500090127      //?Definizione procedure usate.
017600090127      //---------------------------------------------------------------
017700090127
017800090127      // - Reperimento dati utente
017900090316      /copy gaitrasrc/srcProtoPR,TIBS34R
018000090127
018100090316       // - Ricerca/Controllo tabelle
018200090316      /copy gaitrasrc/srcProtoPR,TIBS02R
018300090316
018400090316       // - Controllo/Decodifica cliente
018500090316      /copy gaitrasrc/srcProtoPR,TIBS69R
018600090127
018700120605      // - Visualizzazione tab. "JDC"
018800120605     d tntbjdcr        pr                  extpgm('TNTBJDCR')
018900090127     d  kpjba                              likeds(KPJBA)
019000090429
019100120605       // - Interrogazione tabella JDC
019200120605     d tntbJDC1r       pr                  extpgm('TNTBJDC1R')
019300090429     d  kpjba                              likeds(KPJBA)
019400090127
019500090127      // - Controllo ed inversione date
019600090127     d xsrda8          pr                  extpgm('XSRDA8')
019700090127     d  wlbdat                             likeds(WLBdat)
019800110211
019900110211      // - Personalizzazione lavoro batch
020000110211     d bch09           pr                  extpgm('BCH09')
020100110211     d  kpjba                              likeds(KPJBA)
020200090127
020300090127      // - Sottomissione lavoro batch
020400090127     d bch10           pr                  extpgm('BCH10')
020500090127     d  kpjba                              likeds(KPJBA)
020600090127
020700090127      // - Richiamo diretto lavoro batch
020800120605     d tnsb73r         pr                  extpgm('TNSB73R')
020900090127     d  kpjba                              likeds(KPJBA)
021000090127
021100090127      //---------------------------------------------------------------
021200090127      //?Definizione key-list.
021300090127      //---------------------------------------------------------------
021400090127
021500040720
021600071210      //---------------------------------------------------------------
021700071210      //?Riepilogo indicatori.
021800071210      //---------------------------------------------------------------
021900071210      //  28    - Emissione messaggio di errore a video
022000120606      //  51    - Codice cliente errato
022100120606      //  52    - Nome documento
022200120606      //  53    - Directory per documenti errata
022300120606      //  54    - Tipo documento errato
022400120606      //  55    - Data consegna DAL errata o range errato
022500120606      //  56    - Data consegna AL  errata
022600120606      //  57    - Data spedizione DAL errata o range errato
022700120606      //  58    - Data spedizione AL  errata o range errato
022800120607      //  59    - Tipo invio errato
022900071210      //  90    - Generico di errore
023000071210      //---------------------------------------------------------------
023100040720
023200090127      //---------------------------------------------------------------
023300090127      //?M A I N - L I N E.
023400090127      //---------------------------------------------------------------
023500090127
023600040720     c     *Entry        plist
023700040720     c                   parm                    KPJBA
023800071210
023900071210      /free
024000071210
024100071210       // Operazioni iniziali
024200071210       exsr RoutInz;
024300071210
024400071210       // Gestione video
024500071210       DOW $Fine = *off;
024600071210         select;
024700071210           when $Video = 'D1';
024800071210             exsr GesD01;
024900071210           when $Video = 'D2';
025000071210             exsr GesD02;
025100071210           when $Video = *zero;
025200071210             exsr SbmJOB;
025300071210           other;
025400071210             leave;
025500071210         endsl;
025600071210       ENDDO;
025700071210
025800071210       // Operazioni finali
025900071210       exsr RoutEnd;
026000071210
026100071211       //--------------------------------------------------------------
026200071210       //?Operazioni iniziali.
026300071211       //--------------------------------------------------------------
026400071210       BEGSR RoutInz;
026500071210
026600071210       // Reperimento dati job
026700071210         exsr DatiJob;
026800110211
026900110211       //?Se utente EDPMB visualizzo F11
027000110211         SeSonoIo = (KNMUS = 'EDPMB');
027100071210
027200071210       // Impostazione nome programma a video
027300071210         V1Tpgm = SDSpgm;
027400071210
027500071210       // Impostazione campo LACTIM = aaaa/mm/gg+hh:mm:ss
027600071210         wTimeHMS = %time();
027700071211         dsTIME_ymd = %dec(wDateISO : *iso);
027800071211         dsTIME_hms = %dec(wTimeHMS : *hms);
027900100506
028000100506         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028100071210
028200071210       ENDSR;
028300071210
028400071211       //--------------------------------------------------------------
028500071210       //?Reperimento Dati del job (Utente/Operativi).
028600071211       //--------------------------------------------------------------
028700071210       BEGSR DatiJob;
028800071210
028900071210         in(E) §AzUte;
029000071210         if NOT %error;
029100071210           in(E) §DatiUte;
029200071210         endif;
029300071210         if %error or RSut = *blanks;
029400071210           clear TIBS34ds;
029500071210           tibs34r(tibs34ds);
029600071210           in §AzUte;
029700071210           in §DatiUte;
029800071210         endif;
029900071210
030000071210       ENDSR;
030100071210
030200071211       //--------------------------------------------------------------
030300071210       //?Gestione videata D01
030400071211       //--------------------------------------------------------------
030500071210       BEGSR GesD01;
030600071210
030700071210       // Inizializzazione videata
030800071210         if $InzD01 = *on;
030900071210           exsr InzD01;
031000071210           $InzD01 = *off;
031100071210         endif;
031200071210
031300071210       // Emissione testata
031400071210         if  NOT  errMessage;
031500120605           write SB71T01;
031600071210         endif;
031700071210
031800071210       // Emissione videata
031900120605         exfmt SB71D01;
032000071210         errMessage  = *off;
032100071210         errGenerico = *off;
032200071210         clear V1Dmsg;
032300071210
032400071210         select;
032500071210       // F3=Fine
032600071210           when *inKC;
032700071210             exsr F03D01;
032800071210       // Enter
032900071210           other;
033000071210             exsr CtrD01;
033100071210             if errGenerico;
033200071210               leavesr;
033300071210             endif;
033400071210         endsl;
033500071210
033600071210       ENDSR;
033700071210
033800071211       //--------------------------------------------------------------
033900071210       //?Inizializzazione videata D01
034000071211       //--------------------------------------------------------------
034100071210       BEGSR InzD01;
034200071210
034300120605         clear SB71D01;
034400071210
034500071210       ENDSR;
034600071210
034700071211       //--------------------------------------------------------------
034800071210       //?Gestione tasto funzionale F3 da videata D01
034900071211       //--------------------------------------------------------------
035000071210       BEGSR F03D01;
035100071210
035200071210       // Chiude il programma
035300071210         $Fine = *on;
035400071210
035500071210       ENDSR;
035600071210
035700071211       //--------------------------------------------------------------
035800071210       //?Controllo dati immessi in videata D01
035900071211       //--------------------------------------------------------------
036000071210       BEGSR CtrD01;
036100071210
036200110211         //IndDspF = *zeros;
036300110211
036400110211         WindDspF = IndDspF;
036500110211         reset WindDspFb;
036600110211         IndDspF  = WindDspF;
036700071211
036800071211       //? CONTROLLO PARZIALIZZAZIONI EFFETTUATE ?
036900090128       //? (UNICA SELEZIONE: PER CLIENTE)        ?
037000071211
037100090128         SELECT;
037200090128
037300071211       // - Nessuna parzializzazione richiesta
037400090128           WHEN  V1Cksc = *zero   or   V1Cksc = *blank;
037500071211             errMessage  = *on;
037600071211             errGenerico = *on;
037700071211             PosCurKsc   = *on;
037800071211             V1Dmsg = $Msg(04);
037900071211             leavesr;
038000071211
038100090128       // Ricerca su codice cliente
038200090128           WHEN  %scan('?' : V1Cksc) > *zeros;
038300090429             clear  Param01;
038400090429             P01ord = '0';
038500090429             KPJBU  = Param01;
038600120605             tntbjdc1r (KPJBA);
038700090429             Param01 = KPJBU;
038800090429             if  P01ke1 <> *zero;
038900090429               V1Cksc = P01ke1;
039000090429               reset TIBS02ds;
039100090429               T02sif = KNSIF;
039200090429               T02ke1 = V1Cksc;
039300090429               TNTBE_RicercaControllo(kpjba : tibs02ds);
039400090429               if T02err = *blanks;
039500120605                 dJDC   = T02uni;
039600090429               else;
039700120605                 clear djdc;
039800090429                 errMessage  = *on;
039900090429                 PosCurKsc   = *on;
040000090429                 V1Dmsg = T02msg;
040100090429               endif;
040200090429               errGenerico = *on;
040300090429               leavesr;
040400090128             endif;
040500090128
040600090128       // Controllo immissione codice cliente
040700090128           WHEN  V1Cksc = *blanks  or  V1Cksc = *zeros;
040800090128             errMessage  = *on;
040900090128             errGenerico = *on;
041000090128             PosCurKsc   = *on;
041100090128             V1Dmsg = $Msg(01);
041200090128             leavesr;
041300090128
041400090128       // Controllo immissione solo caratteri numerici
041500090128           WHEN  %check(DigitN : V1Cksc) > *zeros;
041600090128             errMessage  = *on;
041700090128             errGenerico = *on;
041800090128             PosCurksc   = *on;
041900090128             V1Dmsg = $Msg(02);
042000090128             leavesr;
042100090128
042200090128         ENDSL;
042300071211
042400120605       // - Verifica esistenza codice cliente in tabella "JDC"
042500090128         reset TIBS02ds;
042600090128         T02sif = KNSIF;
042700090128         T02ke1 = V1Cksc;
042800090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
042900090128         if T02err = *blanks;
043000090128           V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
043100120605           dJDC   = T02uni;
043200120605           V1Dksc = §JDCrag;
043300090128         else;
043400090128           errMessage  = *on;
043500090128           errGenerico = *on;
043600090128           PosCurKsc   = *on;
043700090128           V1Dmsg = $Msg(03);
043800090128           leavesr;
043900090128         endif;
044000071211
044100071211       //? IMPOSTAZIONE VIDEATA SUCCESSIVA ?
044200071211
044300090128         $Video  = 'D2';
044400090128         $InzD02 = *on;
044500071210
044600071210       ENDSR;
044700071210
044800071211       //--------------------------------------------------------------
044900071210       //?Gestione videata D02
045000071211       //--------------------------------------------------------------
045100071210       BEGSR GesD02;
045200071210
045300071210       // Inizializzazione videata
045400071210         if $InzD02 = *on;
045500071210           exsr InzD02;
045600071210           $InzD02 = *off;
045700071210         endif;
045800071210
045900071210       // Emissione testata
046000071210         if  NOT  errMessage;
046100120605           write SB71T01;
046200071210         endif;
046300071210
046400071210       // Emissione videata
046500120605         exfmt SB71D02;
046600071210         errMessage  = *off;
046700071210         errGenerico = *off;
046800071210         clear V1Dmsg;
046900071210
047000071210         select;
047100071210       // F3=Fine
047200071210           when *inKC;
047300071210             exsr F03D01;
047400120605       // F9=Visualizzazione tab. JDC
047500071217           when *inKI;
047600071217             exsr F09D02;
047700110211       // F11=Personalizzazione lancio
047800110211           when *inKK;
047900110211             kritb = '1';
048000110211             BCH09(kpjba);
048100071210       // F12=Ritorno
048200071210           when *inKL;
048300071210             exsr F12D02;
048400071210       // Controllo videata
048500071210           other;
048600071210             exsr CtrD02;
048700071210             select;
048800071210         // Rilevato errore
048900071210               when errGenerico;
049000071210                 leavesr;
049100071210         // F6-Conferma
049200071210               when *inKF;
049300071210                 $Video = *zeros;
049400071210             endsl;
049500071210         endsl;
049600071210
049700071210       ENDSR;
049800071210
049900071211       //--------------------------------------------------------------
050000071210       //?Inizializzazione videata D02
050100071211       //--------------------------------------------------------------
050200071210       BEGSR InzD02;
050300071210
050400120605         clear SB71D02;
050500071210
050600071211         V2Cksc = %dec(V1Cksc : 7 : 0);
050700071210         V2Dksc = V1Dksc;
050800071210
050900120605         V1Cfmi   = §JDCfmi;
051000120605         V1Cdir   = §JDCdir;
051100120606         V1Ctpi   = §JDCtpi;
051200120605         V1Dnote  = §JDCnote;
051300120607         V1Cpag   = §JDCpag;
051400110211
051500110211       //?Abilito F11
051600110211         F11Attivo = SeSonoIo;
051700071210
051800071210       ENDSR;
051900071217
052000071217       //--------------------------------------------------------------
052100071217       //?Gestione tasto funzionale F9 da videata D02
052200071217       //--------------------------------------------------------------
052300071217       BEGSR F09D02;
052400071217
052500120605       // Visualizzazione tab. JDC
052600120605         reset TNTBJDCds;
052700120605         BJDCke1 = %trim( %editw( V2Cksc : '0       ' ) );
052800071217
052900120605         kpjbu = TNTBJDCds;
053000120605         tntbjdcr(kpjba);
053100120605         TNTBJDCds = kpjbu;
053200071217
053300071217         select;
053400071217         // Ritorno con errore
053500120605           when BJDCerr = *on;
053600120605             V1Dmsg  = BJDCmsg;
053700071217             errMessage  = *on;
053800071217             errGenerico = *on;
053900071217         // Ritorno con F3
054000120605           when BJDCfxx = '1';
054100071217             $Fine   = *on;
054200071217             errGenerico = *on;
054300071217         endsl;
054400071217
054500071217       ENDSR;
054600071210
054700071211       //--------------------------------------------------------------
054800071210       //?Gestione tasto funzionale F12 da videata D02
054900071211       //--------------------------------------------------------------
055000071210       BEGSR F12D02;
055100071210
055200071210       // Ritorno alla videata precedente (D01)
055300071210         $Video = 'D1';
055400071210
055500071210       ENDSR;
055600071210
055700071211       //--------------------------------------------------------------
055800071210       //?Controllo dati immessi in videata D02
055900071211       //--------------------------------------------------------------
056000071210       BEGSR CtrD02;
056100071210
056200110211         //IndDspF = *zeros;
056300110211
056400110211         WindDspF = IndDspF;
056500110211         reset WindDspFb;
056600110211         IndDspF  = WindDspF;
056700071210
056800120605       // RE-impostazione dati di default - da tab.JDC (se tolti)
056900071210         if  V1Cfmi =  *blank;
057000120605           V1Cfmi   =  §JDCfmi;
057100071210         endif;
057200071210         if  V1Cdir =  *blank;
057300120605           V1Cdir   =  §JDCdir;
057400071210         endif;
057500120606         if  V1Ctpi =  *blank;
057600120606           V1Ctpi   =  §JDCtpi;
057700120606         endif;
057800120607         if  V1Cpag =  *blank;
057900120607           V1Cpag   =  §JDCpag;
058000120607         endif;
058100090311
058200120605         //?Flag nome documento
058300090311         clear  V1Dfmi;
058400090311         select;
058500090311           // - Obbligatorietà
058600090311           when  V1Cfmi = *blank;
058700090311             errMessage  = *on;
058800090311             errGenerico = *on;
058900090311             PosCurFmi   = *on;
059000120605             V1Dmsg = 'Flag nome documento obbligatorio';
059100090311             leavesr;
059200090311           // - Ricerca
059300090311           when  %scan('?' : V1Cfmi) > *zero;
059400090311             clear  TIBS02ds;
059500090311             T02mod = 'R';
059600090311             T02cod = 'NIM';
059700090311             T02sif = KNSIF;
059800090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
059900090311             if  T02err = *blank;
060000090311               V1Cfmi = %subst(T02ke1 : 1 : %len(V1Cfmi));
060100090311               V1Dfmi = T02uni;
060200090311               errGenerico = *on;
060300090311               leavesr;
060400090311             else;
060500090311               errMessage  = *on;
060600090311               errGenerico = *on;
060700090311               PosCurFmi   = *on;
060800090311               V1Dmsg = T02msg;
060900090311               leavesr;
061000090311             endif;
061100090311           // - Controllo
061200090311           other;
061300090311             reset  TIBS02ds;
061400090311             T02cod = 'NIM';
061500090311             T02ke1 = V1Cfmi;
061600090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
061700090311             if  T02err <> *blank;
061800090311               errMessage  = *on;
061900090311               errGenerico = *on;
062000090311               PosCurFmi   = *on;
062100120605               V1Dmsg = 'Flag nome documento errato';
062200090311               leavesr;
062300090311             endif;
062400090311             V1Dfmi = T02uni;
062500090311         ENDSL;
062600090311
062700120605         //?Directory per documenti
062800090311         select;
062900090311           when  V1Cdir  = *blank;
063000090311             errMessage  = *on;
063100090311             errGenerico = *on;
063200090311             PosCurDir   = *on;
063300120605             V1Dmsg = $Msg(14);
063400090311             leavesr;
063500090311           when  %scan('\':V1Cdir) > *zero;
063600090311             errMessage  = *on;
063700090311             errGenerico = *on;
063800090311             PosCurDir   = *on;
063900090311             V1Dmsg = $Msg(15);
064000090311             leavesr;
064100071210
064200071210       // Controllo immissione di caratteri NON previsti nella directory
064300090316           when  %scan('&' : V1Cdir) > *zero  or
064400071211             %scan('%' : V1Cdir) > *zero;
064500090316             errMessage  = *on;
064600090316             errGenerico = *on;
064700090316             PosCurDir   = *on;
064800090316             V1Dmsg = $Msg(09);
064900090316             leavesr;
065000090311         endsl;
065100120605
065200120605         //?Controlli tipo documento
065300120917         IF  V1Ctpi <> 'TIF' and V1Ctpi <> 'PDF';
065400120605           errMessage   = *on;
065500120605           errGenerico  = *on;
065600120605           PosCurTpi    = *on;
065700120605           V1Dmsg = 'Tipo documento errato';
065800120605           leavesr;
065900120605         ENDIF;
066000120607
066100120607         //?Controlli tipo invio
066200120607         IF  V1Cpag <> 'CLI' and V1Cpag <> 'CL1';
066300120607           errMessage   = *on;
066400120607           errGenerico  = *on;
066500120607           PosCurPag    = *on;
066600120607           V1Dmsg = 'Tipo invio errato';
066700120607           leavesr;
066800120607         ENDIF;
066900071210
067000090311         //?Controllo parzializzazioni per data
067100090128         reset $Parz01a;
067200090128         reset $Parz01b;
067300071210         if  V1Cdcd <> *zero   or  V1Cdca <> *zero;
067400071210           $Parz01a =  *on;
067500071210         endif;
067600071210         if  V1Cdsd <> *zero   or  V1Cdsa <> *zero;
067700071210           $Parz01b =  *on;
067800071210         endif;
067900071210
068000071210         select;
068100071210       // Richiesta nessuna parzializzazione per data
068200071210           when  $Parz01a = *off  and  $Parz01b = *off;
068300071210             errMessage   = *on;
068400071210             errGenerico  = *on;
068500071210             PosCurDcd    = *on;
068600071211             V1Dmsg =  $Msg(10);
068700071210             leavesr;
068800071210       // Richieste troppe  parzializzazioni per data
068900071210           when  $Parz01a = *on  and  $Parz01b = *on;
069000071210             errMessage   = *on;
069100071210             errGenerico  = *on;
069200071210             PosCurDcd    = *on;
069300071211             V1Dmsg = $Msg(11);
069400071210             leavesr;
069500071210       // Parzializzazione per data consegna dal/al
069600071210           when  $Parz01a = *on;
069700071210         // - controllo data iniziale
069800071210             clear WLBdat;
069900071210             G08dat = V1Cdcd;
070000071210             xsrda8(wlbdat);
070100071210             if G08err = *off;
070200071210               V1Cdcd  = G08dat;
070300071210               W1Cdcd  = G08inv;
070400071210             else;
070500071210               errMessage  = *on;
070600071210               errGenerico = *on;
070700071210               PosCurDcd   = *on;
070800071211               V1Dmsg  = $Msg(12);
070900071210               leavesr;
071000071210             endif;
071100071210         // - controllo data finale
071200071210             clear WLBdat;
071300071210             G08dat = V1Cdca;
071400071210             xsrda8(wlbdat);
071500071210             if G08err = *off;
071600071210               V1Cdca  = G08dat;
071700071210               W1Cdca  = G08inv;
071800071210             else;
071900071210               errMessage  = *on;
072000071210               errGenerico = *on;
072100071210               PosCurDca   = *on;
072200071211               V1Dmsg  = $Msg(12);
072300071210               leavesr;
072400071210             endif;
072500071210         // - controllo range
072600071210             if W1Cdcd > W1Cdca;
072700071210               errMessage  = *on;
072800071210               errGenerico = *on;
072900071210               PosCurDcd   = *on;
073000071211               V1Dmsg  = $Msg(13);
073100071210               leavesr;
073200071210             endif;
073300071210
073400071210       // parzializzazione per data spedizione dal/al
073500090128           when  $Parz01b = *on;
073600071210         // - controllo data iniziale
073700071210             clear WLBdat;
073800071210             G08dat = V1Cdsd;
073900071210             xsrda8(wlbdat);
074000071210             if G08err = *off;
074100071210               V1Cdsd  = G08dat;
074200071210               W1Cdsd  = G08inv;
074300071210             else;
074400071210               errMessage  = *on;
074500071210               errGenerico = *on;
074600071210               PosCurDsd   = *on;
074700071211               V1Dmsg  = $Msg(12);
074800071210               leavesr;
074900071210             endif;
075000071210         // - controllo data finale
075100071210             clear WLBdat;
075200071210             G08dat = V1Cdsa;
075300071210             xsrda8(wlbdat);
075400071210             if G08err = *off;
075500071210               V1Cdsa  = G08dat;
075600071210               W1Cdsa  = G08inv;
075700071210             else;
075800071210               errMessage  = *on;
075900071210               errGenerico = *on;
076000071210               PosCurDsa   = *on;
076100071211               V1Dmsg  = $Msg(12);
076200071210               leavesr;
076300071210             endif;
076400071210         // - controllo range
076500071210             if W1Cdsd > W1Cdsa;
076600071210               errMessage  = *on;
076700071210               errGenerico = *on;
076800071210               PosCurDsd   = *on;
076900071211               V1Dmsg  = $Msg(13);
077000071210               leavesr;
077100071210             endif;
077200071210
077300071210         endsl;
077400071210
077500071210       ENDSR;
077600071211
077700071211       //--------------------------------------------------------------
077800071211       //?Sottomissione lavoro batch
077900071211       //--------------------------------------------------------------
078000071211       BEGSR SbmJOB;
078100071211
078200071211       // Impostazione parametri
078300120605         reset TNSB73ds;
078400120606         d73imm  = V1Cimm;
078500120606         d73dcd  = W1Cdcd;
078600120606         d73dca  = W1Cdca;
078700120606         d73dsd  = W1Cdsd;
078800120606         d73dsa  = W1Cdsa;
078900120606         d73ksc  = V2Cksc;
079000120606         d73dir  = V1Cdir;
079100120606         d73fmi  = V1Cfmi;
079200120606         d73tpi  = V1Ctpi;
079300120607         d73pag  = V1Cpag;
079400120606         d73ksu  = v2cksc;
079500120606         d73tad  = §jdctad;
079600120606         d73tadu = §jdctadu;
079700120606         d73fimp = §jdcfimp;
079800120606         d73imp  = §jdcimp;
079900120606         d73kscf = §jdcksc;
080000120606         d73ctr  = §jdcctr;
080100120606
080200120606       // ID Job
080300120606       // - imposto ksc + oggi + progressivo
080400120606         d73job = %editc(v2cksc:'X') + %editc(dstime_ymd:'X') + '0';
080500120606       // - controllo, capita spesso che si lanci lo stesso cliente + volte nello stesso giorno
080600120606         exsr ctridjob;
080700120606
080800120606         d73tpt = §jdctpt;
080900120606         d73res = §jdcres;
081000120606         d73rec = §jdcrec;
081100120606         d73csr = §jdccsr;
081200120606         d73ssr = §jdcssr;
081300120606         d73lnp = §jdclnp;
081400071211
081500120605         kcoaz = 'SB73';
081600120605         kpjbu = TNSB73ds;
081700071211         if  knmus = *all'1';
081800120605           tnsb73r(kpjba);
081900071211         else;
082000071211           BCH10(kpjba);
082100071211         endif;
082200120606
082300120606       // scrivo rcd spia su tijdc con lacela = '33'
082400120606            clear tijdc000;
082500120606            lactim = %char(%timestamp:*iso0);
082600120606            lacdir = d73dir;
082700120606            lacela = '33';
082800120606            lactela = 'S';
082900120606            lacksu = d73ksu;
083000120606            lactad = d73tad;
083100120606            lacidjob = d73job;
083200120606            write tijdc000;
083300071211
083400071211       // Uscita dal pgm
083500071211         $Fine = *on;
083600071211         reset $Video;
083700071211         reset $InzD01;
083800071211
083900071211       ENDSR;
084000120606
084100120606       //--------------------------------------------------------------
084200120606       //?Controllo ID job deve essere univoco.
084300120606       //--------------------------------------------------------------
084400120606       BEGSR CtrIdJob;
084500120606
084600120606         $FineJDC = *off;
084700120606         widjob15 = %subst(d73job:1:15);
084800120606
084900120606         exec sql
085000120606          DECLARE JDC cursor for
085100120606          SELECT   lacidjob from tijdc00f
085200120606          WHERE    substr(lacidjob, 1, 15) = :widjob15
085300120606                   and lacela = '33'
085400120606          ORDER BY lacidjob;
085500120606
085600120606          exec sql
085700120606           OPEN JDC;
085800120606
085900120606          DOW not $FineJDC;
086000120606            exec sql
086100120606             FETCH next from JDC into: widjob;
086200120606             IF sqlcod = 100 or sqlcod < 0;
086300120606               $FineJDC = *on;
086400120606               leave;
086500120606             ENDIF;
086600120606
086700120606            wprogr = %dec(%subst(d73job:16:1):1:0);
086800120606            wprogr += 1;
086900120606            %subst(d73job:16:1) = %editc(wprogr:'X');
087000120606          ENDDO;
087100120606
087200120606        exec sql
087300120606         CLOSE JDC;
087400120606
087500120606       ENDSR;
087600120606
087700071211       //--------------------------------------------------------------
087800071210       //?Operazioni finali.
087900071211       //--------------------------------------------------------------
088000071210       BEGSR RoutEnd;
088100071210
088200071210         clear TIBS02ds;
088300071210         T02tla = 'C';
088400090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
088500071210
088600071210         *inLR = *on;
088700071210         return;
088800071210
088900071210       ENDSR;
089000071210
089100071210      /end-free
089200040720
089300071211       //--------------------------------------------------------------
089400071210       //?Schiere a tempo di compilazione.
089500071211       //--------------------------------------------------------------
089600040720
089700071210**   $Msg -------------------------------------------------------------------*
089800071210Immettere il codice cliente                                                     1
089900071210Immettere SOLO caratteri numerici                                               2
090000120605Codice cliente mancante o non presente in tabella "JDC"                         3
090100071210Immettere almeno un codice cliente oppure un numero spedizione                  4
090200071210Parzializzare per codice cliente OPPURE per numero spedizione                   5
090300071210Numero spedizione errato                                                        6
090400071210Spedizione non DPD                                                              7
090500120605Il cliente della spedizione non è in tabella "JDC"                              8
090600071210Caratteri "&" e "%" non ammessi per la directory del cliente                    9
090700071211Immettere almeno la data consegna oppure la data spedizione                    10
090800071211Parzializzare per data consegna OPPURE per data spedizione                     11
090900071211Data formalmente errata                                                        12
091000071211Data "dal" maggiore di data "al"                                               13
091100090311Immettere la directory                                                         14
091200090311Carattere "\" non valido; per indicare una sottocartella utilizzare "/"        15
