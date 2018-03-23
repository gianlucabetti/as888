000100071210      //---------------------------------------------------------------
000200071210      //?TNSB15R - Rigenerazione immagine LdV per cliente
000300071210      //---------------------------------------------------------------
000400090127
000500071210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040720
000700071210      //---------------------------------------------------------------
000800071210      //?Dichiarazione file.
000900071210      //---------------------------------------------------------------
001000040720
001100050317     fTNSB15D   cf   e             workstn
001200071210     f                                     indds(IndDspF)
001300090327     ftilac00f  o    e             disk
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
005600071210     d   PosCurKsc                    1n   overlay(IndDspF:40)
005700071211     d   PosCurDir                    1n   overlay(IndDspF:41)
005800071211     d   PosCurDcd                    1n   overlay(IndDspF:42)
005900071211     d   PosCurDca                    1n   overlay(IndDspF:43)
006000071211     d   PosCurDsd                    1n   overlay(IndDspF:44)
006100071211     d   PosCurDsa                    1n   overlay(IndDspF:45)
006200090311     d   PosCurTad                    1n   overlay(IndDspF:46)
006300090311     d   PosCurTadu                   1n   overlay(IndDspF:47)
006400090311     d   PosCurImp                    1n   overlay(IndDspF:48)
006500090311     d   PosCurFimp                   1n   overlay(IndDspF:49)
006600090311     d   PosCurFmi                    1n   overlay(IndDspF:50)
006700090311     d   PosCurKscf                   1n   overlay(IndDspF:51)
006800090311     d   PosCurCtr                    1n   overlay(IndDspF:52)
006900071210     d   errGenerico                  1n   overlay(IndDspF:99)
007000110211
007100110211     d WindDspF        ds                  inz
007200110211     d   WindDspFa             1     49    inz(*zero)
007300110211     d   WindDspFb            50     99    inz(*zero)
007400071210
007500071210      // - Parametri ricevuti
007600040720     d KPJBA         e ds
007700071210     d TNSB16ds        ds                  inz
007800071210     d   D16dcd                       8  0 inz
007900071210     d   D16dca                       8  0 inz
008000071210     d   D16ksc                       7  0 inz
008100071210     d   D16imm                       1    inz
008200090310     d   D16tad                       1    inz
008300071210     d   D16dsd                       8  0 inz
008400071210     d   D16dsa                       8  0 inz
008500071210     d   D16dir                      30    inz
008600071210     d   D16fmi                       2    inz
008700090310     d   d16ksu                       7  0 inz
008800090310     d   d16tadu                      1    inz
008900090310     d   d16fimp                      1    inz
009000090310     d   d16imp                      10  3 inz
009100090310     d   d16kscf                      7  0 inz
009200090310     d   d16ctr                       3    inz
009300090310     d   d16job                      16    inz
009400090310     d   d16tpt                       1    inz
009500090310     d   d16res                       1    inz
009600090310     d   d16rec                       1    inz
009700090310     d   d16csr                       1    inz
009800090310     d   d16ssr                       1    inz
009900090310     d   d16lnp                       3  0 inz
010000071210
010100071210      // - Reperimento dati utente
010200071210     d TIBS34ds      e ds
010300071210
010400071210      // - Gestione tabelle: controllo e ricerca
010500071210     d TIBS02ds      e ds                  inz
010600071210     d   T02mod      e                     inz('C')
010700071210     d   T02cod      e                     inz('LAC')
010800071210
010900071210      // - Tabella "LAC" = Clienti per ritorno immagini
011000071210     d dLAC          e ds                  inz
011100071217
011200071217      // - Reperimento dati utente
011300071217     d TNTB46ds      e ds                  inz
011400071217     d   B46opz      e                     inz('5')
011500071217     d   B46err      e                     inz(*off)
011600090316
011700090316       // - Controllo/Decodifica cliente
011800090316     d TIBS69ds      e ds                  qualified  inz
011900090316     d ds_CNACO      e ds                  extname(CNACO00F)
012000090316     d                                     qualified  inz
012100090316     d ds_CNIND      e ds                  extname(CNIND00F)
012200090316     d                                     qualified  inz
012300090316     d ds_CNCLP      e ds                  extname(CNCLP00F)
012400090316     d                                     qualified  inz
012500090316     d ds_FNCLS      e ds                  extname(FNCLS00F)
012600090316     d                                     qualified  inz
012700071210
012800071210      // - Controllo ed inversione data
012900040720     d WLBdat          ds                  inz
013000040720     d   G08dat                       8  0 inz
013100040720     d   G08inv                       8  0 inz
013200040720     d   G08err                       1    inz(*off)
013300040720     d   G08tgi                       5  0 inz
013400071210
013500071210      // - Time
013600071210     d DS_Time         ds            14    inz
013700071210     d   dsTIME_ymd                   8  0 inz
013800071210     d   dsTIME_hms                   6  0 inz
013900090429
014000090429       // - Struttura per passaggio dati ad interrogazione tabella
014100090429     d Param01         ds                  inz
014200090429     d  P01cod                        7  0 inz
014300090429     d  P01ord                        1    inz
014400090429     d  P01ksu                        7  0 inz
014500090429     d  P01ke1                        7    inz
014600090429     d  P01ke2                       15    inz
014700090429     d  P01rit                        1    inz
014800071210
014900071210      //---------------------------------------------------------------
015000071210      //?Definizione variabili globali.
015100071210      //---------------------------------------------------------------
015200090127
015300071210      // - Flags booleani
015400110211     d SeSonoIo        s              1n   inz(*off)
015500040720     d $Fine           s              1    inz(*off)
015600100506     d $FineLAC        s              1n   inz(*off)
015700040720     d $InzD01         s              1    inz(*on)
015800040720     d $InzD02         s              1    inz(*on)
015900090128     d $Parz01a        s              1    inz(*off)
016000090128     d $Parz01b        s              1    inz(*off)
016100071210
016200071210      // - Gestione video
016300071210     d $Video          s              2    inz('D1')
016400071210
016500071210      // - Comodo
016600071210     d W1Cdcd          s                   like(V1Cdcd)   inz
016700071210     d W1Cdca          s                   like(V1Cdca)   inz
016800071210     d W1Cdsd          s                   like(V1Cdsd)   inz
016900071210     d W1Cdsa          s                   like(V1Cdsa)   inz
017000071210     d WdateISO        s               d   datfmt(*ISO)   inz(*job)
017100071210     d WtimeHMS        s               t   timfmt(*HMS)   inz
017200100506     d wprogr          s              1  0 inz
017300100506     d widjob15        s             15a
017400100506     d widjob          s             16a
017500090127
017600090127      //---------------------------------------------------------------
017700090127      //?Definizione procedure usate.
017800090127      //---------------------------------------------------------------
017900090127
018000090127      // - Reperimento dati utente
018100090316      /copy gaitrasrc/srcProtoPR,TIBS34R
018200090127
018300090316       // - Ricerca/Controllo tabelle
018400090316      /copy gaitrasrc/srcProtoPR,TIBS02R
018500090316
018600090316       // - Controllo/Decodifica cliente
018700090316      /copy gaitrasrc/srcProtoPR,TIBS69R
018800090127
018900090127      // - Visualizzazione tab. "LAC"
019000090127     d tntb46r         pr                  extpgm('TNTB46R')
019100090127     d  kpjba                              likeds(KPJBA)
019200090429
019300090429       // - Interrogazione tabella LAC
019400090429     d tntb461r        pr                  extpgm('TNTB461R')
019500090429     d  kpjba                              likeds(KPJBA)
019600090127
019700090127      // - Controllo ed inversione date
019800090127     d xsrda8          pr                  extpgm('XSRDA8')
019900090127     d  wlbdat                             likeds(WLBdat)
020000110211
020100110211      // - Personalizzazione lavoro batch
020200110211     d bch09           pr                  extpgm('BCH09')
020300110211     d  kpjba                              likeds(KPJBA)
020400090127
020500090127      // - Sottomissione lavoro batch
020600090127     d bch10           pr                  extpgm('BCH10')
020700090127     d  kpjba                              likeds(KPJBA)
020800090127
020900090127      // - Richiamo diretto lavoro batch
021000090127     d tnsb16r         pr                  extpgm('TNSB16R')
021100090127     d  kpjba                              likeds(KPJBA)
021200090127
021300090127      //---------------------------------------------------------------
021400090127      //?Definizione key-list.
021500090127      //---------------------------------------------------------------
021600090127
021700040720
021800071210      //---------------------------------------------------------------
021900071210      //?Riepilogo indicatori.
022000071210      //---------------------------------------------------------------
022100071210      //  28    - Emissione messaggio di errore a video
022200071210      //  40    - Codice cliente errato
022300071211      //  41    - Directory per immagini errata
022400071211      //  42    - Data consegna DAL errata o range errato
022500071211      //  43    - Data consegna AL  errata
022600071211      //  44    - Data spedizione DAL errata o range errato
022700071211      //  45    - Data spedizione AL  errata o range errato
022800071210      //  90    - Generico di errore
022900071210      //---------------------------------------------------------------
023000040720
023100090127      //---------------------------------------------------------------
023200090127      //?M A I N - L I N E.
023300090127      //---------------------------------------------------------------
023400090127
023500040720     c     *Entry        plist
023600040720     c                   parm                    KPJBA
023700071210
023800071210      /free
023900071210
024000071210       // Operazioni iniziali
024100071210       exsr RoutInz;
024200071210
024300071210       // Gestione video
024400071210       DOW $Fine = *off;
024500071210         select;
024600071210           when $Video = 'D1';
024700071210             exsr GesD01;
024800071210           when $Video = 'D2';
024900071210             exsr GesD02;
025000071210           when $Video = *zero;
025100071210             exsr SbmJOB;
025200071210           other;
025300071210             leave;
025400071210         endsl;
025500071210       ENDDO;
025600071210
025700071210       // Operazioni finali
025800071210       exsr RoutEnd;
025900071210
026000071211       //--------------------------------------------------------------
026100071210       //?Operazioni iniziali.
026200071211       //--------------------------------------------------------------
026300071210       BEGSR RoutInz;
026400071210
026500071210       // Reperimento dati job
026600071210         exsr DatiJob;
026700110211
026800110211       //?Se utente EDPMB visualizzo F11
026900110211         SeSonoIo = (KNMUS = 'EDPMB');
027000071210
027100071210       // Impostazione nome programma a video
027200071210         V1Tpgm = SDSpgm;
027300071210
027400071210       // Impostazione campo LACTIM = aaaa/mm/gg+hh:mm:ss
027500071210         wTimeHMS = %time();
027600071211         dsTIME_ymd = %dec(wDateISO : *iso);
027700071211         dsTIME_hms = %dec(wTimeHMS : *hms);
027800100506
027900100506         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028000071210
028100071210       ENDSR;
028200071210
028300071211       //--------------------------------------------------------------
028400071210       //?Reperimento Dati del job (Utente/Operativi).
028500071211       //--------------------------------------------------------------
028600071210       BEGSR DatiJob;
028700071210
028800071210         in(E) §AzUte;
028900071210         if NOT %error;
029000071210           in(E) §DatiUte;
029100071210         endif;
029200071210         if %error or RSut = *blanks;
029300071210           clear TIBS34ds;
029400071210           tibs34r(tibs34ds);
029500071210           in §AzUte;
029600071210           in §DatiUte;
029700071210         endif;
029800071210
029900071210       ENDSR;
030000071210
030100071211       //--------------------------------------------------------------
030200071210       //?Gestione videata D01
030300071211       //--------------------------------------------------------------
030400071210       BEGSR GesD01;
030500071210
030600071210       // Inizializzazione videata
030700071210         if $InzD01 = *on;
030800071210           exsr InzD01;
030900071210           $InzD01 = *off;
031000071210         endif;
031100071210
031200071210       // Emissione videata
031300130913         write SB15T01;
031400071210         exfmt SB15D01;
031500071210         errMessage  = *off;
031600071210         errGenerico = *off;
031700071210         clear V1Dmsg;
031800071210
031900071210         select;
032000071210       // F3=Fine
032100071210           when *inKC;
032200071210             exsr F03D01;
032300071210       // Enter
032400071210           other;
032500071210             exsr CtrD01;
032600071210             if errGenerico;
032700071210               leavesr;
032800071210             endif;
032900071210         endsl;
033000071210
033100071210       ENDSR;
033200071210
033300071211       //--------------------------------------------------------------
033400071210       //?Inizializzazione videata D01
033500071211       //--------------------------------------------------------------
033600071210       BEGSR InzD01;
033700071210
033800071210         clear SB15D01;
033900071210
034000071210       ENDSR;
034100071210
034200071211       //--------------------------------------------------------------
034300071210       //?Gestione tasto funzionale F3 da videata D01
034400071211       //--------------------------------------------------------------
034500071210       BEGSR F03D01;
034600071210
034700071210       // Chiude il programma
034800071210         $Fine = *on;
034900071210
035000071210       ENDSR;
035100071210
035200071211       //--------------------------------------------------------------
035300071210       //?Controllo dati immessi in videata D01
035400071211       //--------------------------------------------------------------
035500071210       BEGSR CtrD01;
035600071210
035700110211         //IndDspF = *zeros;
035800110211
035900110211         WindDspF = IndDspF;
036000110211         reset WindDspFb;
036100110211         IndDspF  = WindDspF;
036200071211
036300071211       //? CONTROLLO PARZIALIZZAZIONI EFFETTUATE ?
036400090128       //? (UNICA SELEZIONE: PER CLIENTE)        ?
036500071211
036600090128         SELECT;
036700090128
036800071211       // - Nessuna parzializzazione richiesta
036900090128           WHEN  V1Cksc = *zero   or   V1Cksc = *blank;
037000071211             errMessage  = *on;
037100071211             errGenerico = *on;
037200071211             PosCurKsc   = *on;
037300071211             V1Dmsg = $Msg(04);
037400071211             leavesr;
037500071211
037600090128       // Ricerca su codice cliente
037700090128           WHEN  %scan('?' : V1Cksc) > *zeros;
037800090429             clear  Param01;
037900090429             P01ord = '0';
038000090429             KPJBU  = Param01;
038100090429             tntb461r (KPJBA);
038200090429             Param01 = KPJBU;
038300090429             if  P01ke1 <> *zero;
038400090429               V1Cksc = P01ke1;
038500090429               reset TIBS02ds;
038600090429               T02sif = KNSIF;
038700090429               T02ke1 = V1Cksc;
038800090429               TNTBE_RicercaControllo(kpjba : tibs02ds);
038900090429               if T02err = *blanks;
039000090429                 dLAC   = T02uni;
039100090429               else;
039200090429                 clear dlac;
039300090429                 errMessage  = *on;
039400090429                 PosCurKsc   = *on;
039500090429                 V1Dmsg = T02msg;
039600090429               endif;
039700090429               errGenerico = *on;
039800090429               leavesr;
039900090128             endif;
040000130913             errGenerico = *on;
040100130913             leavesr;
040200090128
040300090128       // Controllo immissione codice cliente
040400090128           WHEN  V1Cksc = *blanks  or  V1Cksc = *zeros;
040500090128             errMessage  = *on;
040600090128             errGenerico = *on;
040700090128             PosCurKsc   = *on;
040800090128             V1Dmsg = $Msg(01);
040900090128             leavesr;
041000090128
041100090128       // Controllo immissione solo caratteri numerici
041200090128           WHEN  %check(DigitN : V1Cksc) > *zeros;
041300090128             errMessage  = *on;
041400090128             errGenerico = *on;
041500090128             PosCurksc   = *on;
041600090128             V1Dmsg = $Msg(02);
041700090128             leavesr;
041800090128
041900090128         ENDSL;
042000071211
042100090128       // - Verifica esistenza codice cliente in tabella "LAC"
042200090128         reset TIBS02ds;
042300090128         T02sif = KNSIF;
042400090128         T02ke1 = V1Cksc;
042500090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
042600090128         if T02err = *blanks;
042700090128           V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
042800090128           dLAC   = T02uni;
042900090128           V1Dksc = §LACrag;
043000090128         else;
043100090128           errMessage  = *on;
043200090128           errGenerico = *on;
043300090128           PosCurKsc   = *on;
043400090128           V1Dmsg = $Msg(03);
043500090128           leavesr;
043600090128         endif;
043700071211
043800071211       //? IMPOSTAZIONE VIDEATA SUCCESSIVA ?
043900071211
044000090128         $Video  = 'D2';
044100090128         $InzD02 = *on;
044200071210
044300071210       ENDSR;
044400071210
044500071211       //--------------------------------------------------------------
044600071210       //?Gestione videata D02
044700071211       //--------------------------------------------------------------
044800071210       BEGSR GesD02;
044900071210
045000071210       // Inizializzazione videata
045100071210         if $InzD02 = *on;
045200071210           exsr InzD02;
045300071210           $InzD02 = *off;
045400071210         endif;
045500071210
045600071210       // Emissione testata
045700071210         if  NOT  errMessage;
045800071210           write SB15T01;
045900071210         endif;
046000071210
046100071210       // Emissione videata
046200071210         exfmt SB15D02;
046300071210         errMessage  = *off;
046400071210         errGenerico = *off;
046500071210         clear V1Dmsg;
046600071210
046700071210         select;
046800071210       // F3=Fine
046900071210           when *inKC;
047000071210             exsr F03D01;
047100071217       // F9=Visualizzazione tab. LAC
047200071217           when *inKI;
047300071217             exsr F09D02;
047400110211       // F11=Personalizzazione lancio
047500110211           when *inKK;
047600110211             kritb = '1';
047700110211             BCH09(kpjba);
047800071210       // F12=Ritorno
047900071210           when *inKL;
048000071210             exsr F12D02;
048100071210       // Controllo videata
048200071210           other;
048300071210             exsr CtrD02;
048400071210             select;
048500071210         // Rilevato errore
048600071210               when errGenerico;
048700071210                 leavesr;
048800071210         // F6-Conferma
048900071210               when *inKF;
049000071210                 $Video = *zeros;
049100071210             endsl;
049200071210         endsl;
049300071210
049400071210       ENDSR;
049500071210
049600071211       //--------------------------------------------------------------
049700071210       //?Inizializzazione videata D02
049800071211       //--------------------------------------------------------------
049900071210       BEGSR InzD02;
050000071210
050100071210         clear SB15D02;
050200071210
050300071211         V2Cksc = %dec(V1Cksc : 7 : 0);
050400071210         V2Dksc = V1Dksc;
050500071210
050600071210         V1Cfmi   = §LACfmi;
050700071210         V1Cdir   = §LACdir;
050800090310         V1Ctpt   = §LACtpt;
050900090310         V1Cssr   = §LACssr;
051000090310         V1Clnp   = §LAClnp;
051100090310         V1Cres   = §LACres;
051200090310         V1Crec   = §LACrec;
051300090310         V1Ccsr   = §LACcsr;
051400090310         V1Ctad   = §LACtad;
051500090310       // se frequenza diversa da 'I' o 'J' la propongo vuota
051600090310         if §lactadu = 'I' or §lactadu = 'J';
051700090316           V1tadu  = §LACtadu;
051800090310         else;
051900090316           clear v1tadu;
052000090310         endif;
052100090310         V1Ckscf  = §LACksc;
052200090310         V1Cctr   = §LACctr;
052300090316         V1fimp  = §LACfimp;
052400090316         V1imp   = §LACimp;
052500090310         V1Dnote  = §LACnote;
052600110211
052700110211       //?Abilito F11
052800110211         F11Attivo = SeSonoIo;
052900071210
053000071210       ENDSR;
053100071217
053200071217       //--------------------------------------------------------------
053300071217       //?Gestione tasto funzionale F9 da videata D02
053400071217       //--------------------------------------------------------------
053500071217       BEGSR F09D02;
053600071217
053700071217       // Visualizzazione tab. LAC
053800071217         reset TNTB46ds;
053900071217         B46ke1 = %trim( %editw( V2Cksc : '0       ' ) );
054000071217
054100071217         kpjbu = TNTB46ds;
054200071217         tntb46r(kpjba);
054300071217         TNTB46ds = kpjbu;
054400071217
054500071217         select;
054600071217         // Ritorno con errore
054700071217           when B46err = *on;
054800071217             V1Dmsg  = B46msg;
054900071217             errMessage  = *on;
055000071217             errGenerico = *on;
055100071217         // Ritorno con F3
055200071217           when B46fxx = '1';
055300071217             $Fine   = *on;
055400071217             errGenerico = *on;
055500071217         endsl;
055600071217
055700071217       ENDSR;
055800071210
055900071211       //--------------------------------------------------------------
056000071210       //?Gestione tasto funzionale F12 da videata D02
056100071211       //--------------------------------------------------------------
056200071210       BEGSR F12D02;
056300071210
056400071210       // Ritorno alla videata precedente (D01)
056500071210         $Video = 'D1';
056600071210
056700071210       ENDSR;
056800071210
056900071211       //--------------------------------------------------------------
057000071210       //?Controllo dati immessi in videata D02
057100071211       //--------------------------------------------------------------
057200071210       BEGSR CtrD02;
057300071210
057400110211         //IndDspF = *zeros;
057500110211
057600110211         WindDspF = IndDspF;
057700110211         reset WindDspFb;
057800110211         IndDspF  = WindDspF;
057900071210
058000071210       // RE-impostazione dati di default - da tab.LAC (se tolti)
058100090310         if  V1Ctpt =  *blank;
058200090310           V1Ctpt   =  §LACtpt;
058300090310         endif;
058400090316         if  V1Cssr =  *blank;
058500090310           V1Cssr   =   §LACssr;
058600090310         endif;
058700090310         if  V1Cres =  *blank;
058800090310           V1Cres   =  §LACres;
058900090310         endif;
059000090310         if  V1Crec =  *blank;
059100090310           V1Crec   =  §LACrec;
059200090310         endif;
059300090310         if  V1Ccsr =  *blank;
059400090310           V1Ccsr   =  §LACcsr;
059500090310         endif;
059600090310         if  V1Ctad =  *blank;
059700090310           V1Ctad   =  §LACtad;
059800090310         endif;
059900071210         if  V1Cfmi =  *blank;
060000071210           V1Cfmi   =  §LACfmi;
060100071210         endif;
060200071210         if  V1Cdir =  *blank;
060300071210           V1Cdir   =  §LACdir;
060400071210         endif;
060500090311
060600090311         //?Flag nome immagine?
060700090311         clear  V1Dfmi;
060800090311         select;
060900090311           // - Obbligatorietà
061000090311           when  V1Cfmi = *blank;
061100090311             errMessage  = *on;
061200090311             errGenerico = *on;
061300090311             PosCurFmi   = *on;
061400090311             V1Dmsg = 'Flag nome immagine obbligatorio';
061500090311             leavesr;
061600090311           // - Ricerca
061700090311           when  %scan('?' : V1Cfmi) > *zero;
061800090311             clear  TIBS02ds;
061900090311             T02mod = 'R';
062000090311             T02cod = 'NIM';
062100090311             T02sif = KNSIF;
062200090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
062300090311             if  T02err = *blank;
062400090311               V1Cfmi = %subst(T02ke1 : 1 : %len(V1Cfmi));
062500090311               V1Dfmi = T02uni;
062600090311               errGenerico = *on;
062700090311               leavesr;
062800090311             else;
062900090311               errMessage  = *on;
063000090311               errGenerico = *on;
063100090311               PosCurFmi   = *on;
063200090311               V1Dmsg = T02msg;
063300090311               leavesr;
063400090311             endif;
063500090311           // - Controllo
063600090311           other;
063700090311             reset  TIBS02ds;
063800090311             T02cod = 'NIM';
063900090311             T02ke1 = V1Cfmi;
064000090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
064100090311             if  T02err <> *blank;
064200090311               errMessage  = *on;
064300090311               errGenerico = *on;
064400090311               PosCurFmi   = *on;
064500090311               V1Dmsg = 'Flag nome immagine errato';
064600090311               leavesr;
064700090311             endif;
064800090311             V1Dfmi = T02uni;
064900090311         ENDSL;
065000090311
065100090311         //?Directory per immagini?
065200090311         select;
065300090311           when  V1Cdir  = *blank;
065400090311             errMessage  = *on;
065500090311             errGenerico = *on;
065600090311             PosCurDir   = *on;
065700090311             V1Dmsg = $Msg(14);
065800090311             leavesr;
065900090311           when  %scan('\':V1Cdir) > *zero;
066000090311             errMessage  = *on;
066100090311             errGenerico = *on;
066200090311             PosCurDir   = *on;
066300090311             V1Dmsg = $Msg(15);
066400090311             leavesr;
066500071210
066600071210       // Controllo immissione di caratteri NON previsti nella directory
066700090316           when  %scan('&' : V1Cdir) > *zero  or
066800071211             %scan('%' : V1Cdir) > *zero;
066900090316             errMessage  = *on;
067000090316             errGenerico = *on;
067100090316             PosCurDir   = *on;
067200090316             V1Dmsg = $Msg(09);
067300090316             leavesr;
067400090311         endsl;
067500090311
067600090311         //?Tipo addebito (Creazione TITAS)?
067700090311         select;
067800090311           // Se "N" non impostare i dati dell'addebito forzato
067900090311           when  V1Ctad = 'N'     and
068000090311                (V1Ckscf <> *zero  or  V1Cctr <> *blank
068100090311                                   or  V1imp <> *zero);
068200090311             errMessage  = *on;
068300090311             errGenerico = *on;
068400090311             PosCurTad   = *on;
068500090311             V1Dmsg = 'Se NO addebito non impostare le forzature';
068600090311             leavesr;
068700090311         endsl;
068800090311
068900090311         //?Codice cliente?
069000090311         select;
069100090311           // - Non ammessa tariffa senza cliente
069200090311           when  V1Ckscf =  *zero   and   V1Cctr <> *blank;
069300090311               errMessage  = *on;
069400090311               errGenerico = *on;
069500090311               PosCurKscf  = *on;
069600090311               V1Dmsg = 'Cliente tassazione obbligatorio SE +
069700090311                         inserita la relativa tariffa';
069800090311               leavesr;
069900090311           // - Controllo cliente tassazione
070000090311           when  V1Ckscf <> *zero;
070100090311             clear  TIBS69ds;
070200090311             tibs69ds.I69kcc = DUTkci;
070300090311             tibs69ds.I69kac = %int(V1Ckscf);
070400090311             tibs69ds.I69sif = knsif;
070500090311             tibs69r(TIBS69ds : ds_CNACO :
070600090311                                ds_CNIND :
070700090311                                ds_CNCLP :
070800090311                                ds_FNCLS);
070900090311             if  tibs69ds.O69err = *on;
071000090311               errMessage  = *on;
071100090311               errGenerico = *on;
071200090311               PosCurKscf  = *on;
071300090311               V1Dmsg = 'Cliente tassazione errato';
071400090311               leavesr;
071500090311             endif;
071600090311             if  V1Cctr = *blank;
071700090311               errMessage  = *on;
071800090311               errGenerico = *on;
071900090311               PosCurCtr   = *on;
072000090311               V1Dmsg = 'Inserire il codice tariffa';
072100090311               leavesr;
072200090311             endif;
072300090311         endsl;
072400090311
072500090311         //?Importo & Tipo importo forzato?
072600090311         select;
072700090311           // - Non ammesso importo per Varia Negata
072800090311           when  V1Ctad = 'V'   and   V1imp <> *zero;
072900090311             errMessage  = *on;
073000090311             errGenerico = *on;
073100090311             PosCurImp   = *on;
073200090311             V1Dmsg = 'NON ammesso l''importo forzato per +
073300090311                       Varia Negata';
073400090311             leavesr;
073500090311           // - Non ammesso tipo importo senza importo
073600090311           when  V1imp =  *zero   and   V1fimp <> *blank;
073700090311             errMessage  = *on;
073800090311             errGenerico = *on;
073900090311             PosCurImp   = *on;
074000090311             V1Dmsg = 'Importo forzato obbligatorio SE +
074100090311                       inserito il tipo di importo';
074200090311             leavesr;
074300090311           // - Se inserito importo devono essere inseriti il ksc il ctr
074400090311           //   e il tipo importo
074500090311           when  V1imp <> *zero;
074600090311             select;
074700090311               when  V1fimp  = *blank;
074800090311                 errMessage  = *on;
074900090311                 errGenerico = *on;
075000090311                 PosCurFimp  = *on;
075100090311                 V1Dmsg = 'Immettere il tipo importo';
075200090311                 leavesr;
075300090311               when  V1Ckscf = *zero;
075400090311                 errMessage  = *on;
075500090311                 errGenerico = *on;
075600090311                 PosCurKscf  = *on;
075700090311                 V1Dmsg = 'Immettere il cliente forzato';
075800090311                 leavesr;
075900100204               when  V1Cctr  = *blank;
076000090311                 errMessage  = *on;
076100090311                 errGenerico = *on;
076200090311                 PosCurCtr   = *on;
076300090311                 V1Dmsg = 'Immettere la tariffa forzata';
076400090311                 leavesr;
076500090311             endsl;
076600090311         endsl;
076700071210
076800090311         //?Controllo parzializzazioni per data
076900090128         reset $Parz01a;
077000090128         reset $Parz01b;
077100071210         if  V1Cdcd <> *zero   or  V1Cdca <> *zero;
077200071210           $Parz01a =  *on;
077300071210         endif;
077400071210         if  V1Cdsd <> *zero   or  V1Cdsa <> *zero;
077500071210           $Parz01b =  *on;
077600071210         endif;
077700071210
077800071210         select;
077900071210       // Richiesta nessuna parzializzazione per data
078000071210           when  $Parz01a = *off  and  $Parz01b = *off;
078100071210             errMessage   = *on;
078200071210             errGenerico  = *on;
078300071210             PosCurDcd    = *on;
078400071211             V1Dmsg =  $Msg(10);
078500071210             leavesr;
078600071210       // Richieste troppe  parzializzazioni per data
078700071210           when  $Parz01a = *on  and  $Parz01b = *on;
078800071210             errMessage   = *on;
078900071210             errGenerico  = *on;
079000071210             PosCurDcd    = *on;
079100071211             V1Dmsg = $Msg(11);
079200071210             leavesr;
079300071210       // Parzializzazione per data consegna dal/al
079400071210           when  $Parz01a = *on;
079500071210         // - controllo data iniziale
079600071210             clear WLBdat;
079700071210             G08dat = V1Cdcd;
079800071210             xsrda8(wlbdat);
079900071210             if G08err = *off;
080000071210               V1Cdcd  = G08dat;
080100071210               W1Cdcd  = G08inv;
080200071210             else;
080300071210               errMessage  = *on;
080400071210               errGenerico = *on;
080500071210               PosCurDcd   = *on;
080600071211               V1Dmsg  = $Msg(12);
080700071210               leavesr;
080800071210             endif;
080900071210         // - controllo data finale
081000071210             clear WLBdat;
081100071210             G08dat = V1Cdca;
081200071210             xsrda8(wlbdat);
081300071210             if G08err = *off;
081400071210               V1Cdca  = G08dat;
081500071210               W1Cdca  = G08inv;
081600071210             else;
081700071210               errMessage  = *on;
081800071210               errGenerico = *on;
081900071210               PosCurDca   = *on;
082000071211               V1Dmsg  = $Msg(12);
082100071210               leavesr;
082200071210             endif;
082300071210         // - controllo range
082400071210             if W1Cdcd > W1Cdca;
082500071210               errMessage  = *on;
082600071210               errGenerico = *on;
082700071210               PosCurDcd   = *on;
082800071211               V1Dmsg  = $Msg(13);
082900071210               leavesr;
083000071210             endif;
083100071210
083200071210       // parzializzazione per data spedizione dal/al
083300090128           when  $Parz01b = *on;
083400071210         // - controllo data iniziale
083500071210             clear WLBdat;
083600071210             G08dat = V1Cdsd;
083700071210             xsrda8(wlbdat);
083800071210             if G08err = *off;
083900071210               V1Cdsd  = G08dat;
084000071210               W1Cdsd  = G08inv;
084100071210             else;
084200071210               errMessage  = *on;
084300071210               errGenerico = *on;
084400071210               PosCurDsd   = *on;
084500071211               V1Dmsg  = $Msg(12);
084600071210               leavesr;
084700071210             endif;
084800071210         // - controllo data finale
084900071210             clear WLBdat;
085000071210             G08dat = V1Cdsa;
085100071210             xsrda8(wlbdat);
085200071210             if G08err = *off;
085300071210               V1Cdsa  = G08dat;
085400071210               W1Cdsa  = G08inv;
085500071210             else;
085600071210               errMessage  = *on;
085700071210               errGenerico = *on;
085800071210               PosCurDsa   = *on;
085900071211               V1Dmsg  = $Msg(12);
086000071210               leavesr;
086100071210             endif;
086200071210         // - controllo range
086300071210             if W1Cdsd > W1Cdsa;
086400071210               errMessage  = *on;
086500071210               errGenerico = *on;
086600071210               PosCurDsd   = *on;
086700071211               V1Dmsg  = $Msg(13);
086800071210               leavesr;
086900071210             endif;
087000071210
087100071210         endsl;
087200071210
087300071210       ENDSR;
087400071211
087500071211       //--------------------------------------------------------------
087600071211       //?Sottomissione lavoro batch
087700071211       //--------------------------------------------------------------
087800071211       BEGSR SbmJOB;
087900071211
088000071211       // Impostazione parametri
088100071211         reset TNSB16ds;
088200071211         D16dcd = W1Cdcd;
088300071211         D16dca = W1Cdca;
088400071211         D16dsd = W1Cdsd;
088500071211         D16dsa = W1Cdsa;
088600071211         D16ksc = V2Cksc;
088700071211         D16imm = V1Cimm;
088800090310         D16tad = V1Ctad;
088900071211         D16dir = V1Cdir;
089000071211         D16fmi = V1Cfmi;
089100090310         d16ksu = v2cksc;
089200090316         d16tadu = v1tadu;
089300090316         d16fimp = v1fimp;
089400090316         d16imp  = v1imp;
089500090310         d16kscf = v1ckscf;
089600090310         d16ctr  = v1cctr;
089700090310       // ID Job
089800100506       // - imposto ksc + oggi + progressivo
089900100506         d16job = %editc(v2cksc:'X') + %editc(dstime_ymd:'X') + '0';
090000100506       // - controllo, capita spesso che si lanci lo stesso cliente + volte nello stesso giorno
090100100506         exsr ctridjob;
090200090310
090300090310         d16tpt = v1ctpt;
090400090310         d16res = v1cres;
090500090310         d16rec = v1crec;
090600090310         d16csr = v1ccsr;
090700090310         d16ssr = v1cssr;
090800090310         d16lnp = v1clnp;
090900071211
091000071211         kcoaz = 'SB16';
091100071211         kpjbu = TNSB16ds;
091200071211         if  knmus = *all'1';
091300071211           tnsb16r(kpjba);
091400071211         else;
091500071211           BCH10(kpjba);
091600071211         endif;
091700090327
091800090327       // scrivo rcd spia su tilac con lacela = '33'
091900090327            clear tilac000;
092000090327            lactim = %char(%timestamp:*iso0);
092100090327            lacdir = d16dir;
092200090327            lacela = '33';
092300090327            lactela = 'S';
092400090327            lacksu = d16ksu;
092500090402            lactad = d16tad;
092600090327            lacidjob = d16job;
092700090327            write tilac000;
092800071211
092900071211       // Uscita dal pgm
093000071211         $Fine = *on;
093100071211         reset $Video;
093200071211         reset $InzD01;
093300071211
093400071211       ENDSR;
093500100506
093600100506       //--------------------------------------------------------------
093700100506       //?Controllo ID job deve essere univoco.
093800100506       //--------------------------------------------------------------
093900100506       BEGSR CtrIdJob;
094000100506
094100100506         $FineLAC = *off;
094200100506         widjob15 = %subst(d16job:1:15);
094300100506
094400100506         exec sql
094500100506          DECLARE LAC cursor for
094600100506          SELECT   lacidjob from tilac00f
094700100506          WHERE    substr(lacidjob, 1, 15) = :widjob15
094800100506                   and lacela = '33'
094900100506          ORDER BY lacidjob;
095000100506
095100100506          exec sql
095200100506           OPEN LAC;
095300100506
095400100506          DOW not $FineLAC;
095500100506            exec sql
095600100506             FETCH next from LAC into: widjob;
095700100506             IF sqlcod = 100 or sqlcod < 0;
095800100506               $FineLAC = *on;
095900100506               leave;
096000100506             ENDIF;
096100100506
096200100506            wprogr = %dec(%subst(d16job:16:1):1:0);
096300100506            wprogr += 1;
096400100506            %subst(d16job:16:1) = %editc(wprogr:'X');
096500100506          ENDDO;
096600100506
096700100506        exec sql
096800100506         CLOSE LAC;
096900100506
097000100506       ENDSR;
097100071210
097200071211       //--------------------------------------------------------------
097300071210       //?Operazioni finali.
097400071211       //--------------------------------------------------------------
097500071210       BEGSR RoutEnd;
097600071210
097700071210         clear TIBS02ds;
097800071210         T02tla = 'C';
097900090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
098000071210
098100071210         *inLR = *on;
098200071210         return;
098300071210
098400071210       ENDSR;
098500071210
098600071210      /end-free
098700040720
098800071211       //--------------------------------------------------------------
098900071210       //?Schiere a tempo di compilazione.
099000071211       //--------------------------------------------------------------
099100040720
099200071210**   $Msg -------------------------------------------------------------------*
099300071210Immettere il codice cliente                                                     1
099400071210Immettere SOLO caratteri numerici                                               2
099500071210Codice cliente mancante o non presente in tabella "LAC"                         3
099600071210Immettere almeno un codice cliente oppure un numero spedizione                  4
099700071210Parzializzare per codice cliente OPPURE per numero spedizione                   5
099800071210Numero spedizione errato                                                        6
099900071210Spedizione non DPD                                                              7
100000071210Il cliente della spedizione non è in tabella "LAC"                              8
100100071210Caratteri "&" e "%" non ammessi per la directory del cliente                    9
100200071211Immettere almeno la data consegna oppure la data spedizione                    10
100300071211Parzializzare per data consegna OPPURE per data spedizione                     11
100400071211Data formalmente errata                                                        12
100500071211Data "dal" maggiore di data "al"                                               13
100600090311Immettere la directory                                                         14
100700090311Carattere "\" non valido; per indicare una sottocartella utilizzare "/"        15
