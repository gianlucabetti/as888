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
005600131030     d   PR_V2CKSC                    1n   overlay(IndDspF:39)
005700131030     d   PosCurKsc                    1n   overlay(IndDspF:40)
005800071211     d   PosCurDir                    1n   overlay(IndDspF:41)
005900071211     d   PosCurDcd                    1n   overlay(IndDspF:42)
006000071211     d   PosCurDca                    1n   overlay(IndDspF:43)
006100071211     d   PosCurDsd                    1n   overlay(IndDspF:44)
006200071211     d   PosCurDsa                    1n   overlay(IndDspF:45)
006300090311     d   PosCurTad                    1n   overlay(IndDspF:46)
006400090311     d   PosCurTadu                   1n   overlay(IndDspF:47)
006500090311     d   PosCurImp                    1n   overlay(IndDspF:48)
006600090311     d   PosCurFimp                   1n   overlay(IndDspF:49)
006700090311     d   PosCurFmi                    1n   overlay(IndDspF:50)
006800090311     d   PosCurKscf                   1n   overlay(IndDspF:51)
006900090311     d   PosCurCtr                    1n   overlay(IndDspF:52)
007000071210     d   errGenerico                  1n   overlay(IndDspF:99)
007100110211
007200110211     d WindDspF        ds                  inz
007300110211     d   WindDspFa             1     49    inz(*zero)
007400110211     d   WindDspFb            50     99    inz(*zero)
007500071210
007600071210      // - Parametri ricevuti
007700040720     d KPJBA         e ds
007800071210     d TNSB16ds        ds                  inz
007900071210     d   D16dcd                       8  0 inz
008000071210     d   D16dca                       8  0 inz
008100071210     d   D16ksc                       7  0 inz
008200071210     d   D16imm                       1    inz
008300090310     d   D16tad                       1    inz
008400071210     d   D16dsd                       8  0 inz
008500071210     d   D16dsa                       8  0 inz
008600071210     d   D16dir                      30    inz
008700071210     d   D16fmi                       2    inz
008800090310     d   d16ksu                       7  0 inz
008900090310     d   d16tadu                      1    inz
009000090310     d   d16fimp                      1    inz
009100090310     d   d16imp                      10  3 inz
009200090310     d   d16kscf                      7  0 inz
009300090310     d   d16ctr                       3    inz
009400090310     d   d16job                      16    inz
009500090310     d   d16tpt                       1    inz
009600090310     d   d16res                       1    inz
009700090310     d   d16rec                       1    inz
009800090310     d   d16csr                       1    inz
009900090310     d   d16ssr                       1    inz
010000090310     d   d16lnp                       3  0 inz
010100071210
010200071210      // - Reperimento dati utente
010300071210     d TIBS34ds      e ds
010400071210
010500071210      // - Gestione tabelle: controllo e ricerca
010600071210     d TIBS02ds      e ds                  inz
010700071210     d   T02mod      e                     inz('C')
010800071210     d   T02cod      e                     inz('LAC')
010900071210
011000071210      // - Tabella "LAC" = Clienti per ritorno immagini
011100071210     d dLAC          e ds                  inz
011200071217
011300071217      // - Reperimento dati utente
011400071217     d TNTB46ds      e ds                  inz
011500071217     d   B46opz      e                     inz('5')
011600071217     d   B46err      e                     inz(*off)
011700090316
011800090316       // - Controllo/Decodifica cliente
011900090316     d TIBS69ds      e ds                  qualified  inz
012000090316     d ds_CNACO      e ds                  extname(CNACO00F)
012100090316     d                                     qualified  inz
012200090316     d ds_CNIND      e ds                  extname(CNIND00F)
012300090316     d                                     qualified  inz
012400090316     d ds_CNCLP      e ds                  extname(CNCLP00F)
012500090316     d                                     qualified  inz
012600090316     d ds_FNCLS      e ds                  extname(FNCLS00F)
012700090316     d                                     qualified  inz
012800071210
012900071210      // - Controllo ed inversione data
013000040720     d WLBdat          ds                  inz
013100040720     d   G08dat                       8  0 inz
013200040720     d   G08inv                       8  0 inz
013300040720     d   G08err                       1    inz(*off)
013400040720     d   G08tgi                       5  0 inz
013500071210
013600071210      // - Time
013700071210     d DS_Time         ds            14    inz
013800071210     d   dsTIME_ymd                   8  0 inz
013900071210     d   dsTIME_hms                   6  0 inz
014000090429
014100090429       // - Struttura per passaggio dati ad interrogazione tabella
014200090429     d Param01         ds                  inz
014300090429     d  P01cod                        7  0 inz
014400090429     d  P01ord                        1    inz
014500090429     d  P01ksu                        7  0 inz
014600090429     d  P01ke1                        7    inz
014700090429     d  P01ke2                       15    inz
014800090429     d  P01rit                        1    inz
014900071210
015000071210      //---------------------------------------------------------------
015100071210      //?Definizione variabili globali.
015200071210      //---------------------------------------------------------------
015300090127
015400071210      // - Flags booleani
015500110211     d SeSonoIo        s              1n   inz(*off)
015600040720     d $Fine           s              1    inz(*off)
015700100506     d $FineLAC        s              1n   inz(*off)
015800040720     d $InzD01         s              1    inz(*on)
015900040720     d $InzD02         s              1    inz(*on)
016000090128     d $Parz01a        s              1    inz(*off)
016100090128     d $Parz01b        s              1    inz(*off)
016200071210
016300071210      // - Gestione video
016400071210     d $Video          s              2    inz('D1')
016500071210
016600071210      // - Comodo
016700131030     d W1Cdcd          s                   like(V1Cdcd)   inz
016800071210     d W1Cdca          s                   like(V1Cdca)   inz
016900071210     d W1Cdsd          s                   like(V1Cdsd)   inz
017000071210     d W1Cdsa          s                   like(V1Cdsa)   inz
017100071210     d WdateISO        s               d   datfmt(*ISO)   inz(*job)
017200071210     d WtimeHMS        s               t   timfmt(*HMS)   inz
017300100506     d wprogr          s              1  0 inz
017400100506     d widjob15        s             15a
017500100506     d widjob          s             16a
017600131030     d Save_V2CKSC     s                   like(V2CKSC)
017700090127
017800090127      //---------------------------------------------------------------
017900090127      //?Definizione procedure usate.
018000090127      //---------------------------------------------------------------
018100090127
018200090127      // - Reperimento dati utente
018300090316      /copy gaitrasrc/srcProtoPR,TIBS34R
018400090127
018500090316       // - Ricerca/Controllo tabelle
018600090316      /copy gaitrasrc/srcProtoPR,TIBS02R
018700090316
018800090316       // - Controllo/Decodifica cliente
018900090316      /copy gaitrasrc/srcProtoPR,TIBS69R
019000090127
019100090127      // - Visualizzazione tab. "LAC"
019200090127     d tntb46r         pr                  extpgm('TNTB46R')
019300090127     d  kpjba                              likeds(KPJBA)
019400090429
019500090429       // - Interrogazione tabella LAC
019600090429     d tntb461r        pr                  extpgm('TNTB461R')
019700090429     d  kpjba                              likeds(KPJBA)
019800090127
019900090127      // - Controllo ed inversione date
020000090127     d xsrda8          pr                  extpgm('XSRDA8')
020100090127     d  wlbdat                             likeds(WLBdat)
020200110211
020300110211      // - Personalizzazione lavoro batch
020400110211     d bch09           pr                  extpgm('BCH09')
020500110211     d  kpjba                              likeds(KPJBA)
020600090127
020700090127      // - Sottomissione lavoro batch
020800090127     d bch10           pr                  extpgm('BCH10')
020900090127     d  kpjba                              likeds(KPJBA)
021000090127
021100090127      // - Richiamo diretto lavoro batch
021200090127     d tnsb16r         pr                  extpgm('TNSB16R')
021300090127     d  kpjba                              likeds(KPJBA)
021400131112
021500131112      // - Vis. Log Elab
021600131112     d TNSB04R         pr                  extpgm('TNSB04R')
021700131112     d  kpjba                              likeds(KPJBA)
021800090127
021900090127      //---------------------------------------------------------------
022000090127      //?Definizione key-list.
022100090127      //---------------------------------------------------------------
022200090127
022300040720
022400071210      //---------------------------------------------------------------
022500071210      //?Riepilogo indicatori.
022600071210      //---------------------------------------------------------------
022700071210      //  28    - Emissione messaggio di errore a video
022800071210      //  40    - Codice cliente errato
022900071211      //  41    - Directory per immagini errata
023000071211      //  42    - Data consegna DAL errata o range errato
023100071211      //  43    - Data consegna AL  errata
023200071211      //  44    - Data spedizione DAL errata o range errato
023300071211      //  45    - Data spedizione AL  errata o range errato
023400071210      //  90    - Generico di errore
023500071210      //---------------------------------------------------------------
023600040720
023700090127      //---------------------------------------------------------------
023800090127      //?M A I N - L I N E.
023900090127      //---------------------------------------------------------------
024000090127
024100040720     c     *Entry        plist
024200040720     c                   parm                    KPJBA
024300071210
024400071210      /free
024500071210
024600071210       // Operazioni iniziali
024700071210       exsr RoutInz;
024800071210
024900071210       // Gestione video
025000071210       DOW $Fine = *off;
025100071210         select;
025200071210           when $Video = 'D1';
025300071210             exsr GesD01;
025400071210           when $Video = 'D2';
025500071210             exsr GesD02;
025600071210           when $Video = *zero;
025700071210             exsr SbmJOB;
025800071210           other;
025900071210             leave;
026000071210         endsl;
026100071210       ENDDO;
026200071210
026300071210       // Operazioni finali
026400071210       exsr RoutEnd;
026500071210
026600071211       //--------------------------------------------------------------
026700071210       //?Operazioni iniziali.
026800071211       //--------------------------------------------------------------
026900071210       BEGSR RoutInz;
027000071210
027100071210       // Reperimento dati job
027200071210         exsr DatiJob;
027300110211
027400110211       //?Se utente EDPMB visualizzo F11
027500110211         SeSonoIo = (KNMUS = 'EDPMB');
027600071210
027700071210       // Impostazione nome programma a video
027800071210         V1Tpgm = SDSpgm;
027900071210
028000071210       // Impostazione campo LACTIM = aaaa/mm/gg+hh:mm:ss
028100071210         wTimeHMS = %time();
028200071211         dsTIME_ymd = %dec(wDateISO : *iso);
028300071211         dsTIME_hms = %dec(wTimeHMS : *hms);
028400100506
028500100506         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028600071210
028700071210       ENDSR;
028800071210
028900071211       //--------------------------------------------------------------
029000071210       //?Reperimento Dati del job (Utente/Operativi).
029100071211       //--------------------------------------------------------------
029200071210       BEGSR DatiJob;
029300071210
029400071210         in(E) §AzUte;
029500071210         if NOT %error;
029600071210           in(E) §DatiUte;
029700071210         endif;
029800071210         if %error or RSut = *blanks;
029900071210           clear TIBS34ds;
030000071210           tibs34r(tibs34ds);
030100071210           in §AzUte;
030200071210           in §DatiUte;
030300071210         endif;
030400071210
030500071210       ENDSR;
030600071210
030700071211       //--------------------------------------------------------------
030800071210       //?Gestione videata D01
030900071211       //--------------------------------------------------------------
031000071210       BEGSR GesD01;
031100071210
031200071210       // Inizializzazione videata
031300071210         if $InzD01 = *on;
031400071210           exsr InzD01;
031500071210           $InzD01 = *off;
031600071210         endif;
031700071210
031800071210       // Emissione videata
031900130913         write SB15T01;
032000071210         exfmt SB15D01;
032100071210         errMessage  = *off;
032200071210         errGenerico = *off;
032300071210         clear V1Dmsg;
032400071210
032500071210         select;
032600071210       // F3=Fine
032700071210           when *inKC;
032800071210             exsr F03D01;
032900071210       // Enter
033000071210           other;
033100071210             exsr CtrD01;
033200071210             if errGenerico;
033300071210               leavesr;
033400071210             endif;
033500071210         endsl;
033600071210
033700071210       ENDSR;
033800071210
033900071211       //--------------------------------------------------------------
034000071210       //?Inizializzazione videata D01
034100071211       //--------------------------------------------------------------
034200071210       BEGSR InzD01;
034300071210
034400071210         clear SB15D01;
034500071210
034600071210       ENDSR;
034700071210
034800071211       //--------------------------------------------------------------
034900071210       //?Gestione tasto funzionale F3 da videata D01
035000071211       //--------------------------------------------------------------
035100071210       BEGSR F03D01;
035200071210
035300071210       // Chiude il programma
035400071210         $Fine = *on;
035500071210
035600071210       ENDSR;
035700071210
035800071211       //--------------------------------------------------------------
035900071210       //?Controllo dati immessi in videata D01
036000071211       //--------------------------------------------------------------
036100071210       BEGSR CtrD01;
036200071210
036300110211         //IndDspF = *zeros;
036400110211
036500110211         WindDspF = IndDspF;
036600110211         reset WindDspFb;
036700110211         IndDspF  = WindDspF;
036800071211
036900071211       //? CONTROLLO PARZIALIZZAZIONI EFFETTUATE ?
037000090128       //? (UNICA SELEZIONE: PER CLIENTE)        ?
037100071211
037200090128         SELECT;
037300090128
037400071211       // - Nessuna parzializzazione richiesta
037500090128           WHEN  V1Cksc = *zero   or   V1Cksc = *blank;
037600071211             errMessage  = *on;
037700071211             errGenerico = *on;
037800071211             PosCurKsc   = *on;
037900071211             V1Dmsg = $Msg(04);
038000071211             leavesr;
038100071211
038200090128       // Ricerca su codice cliente
038300090128           WHEN  %scan('?' : V1Cksc) > *zeros;
038400090429             clear  Param01;
038500090429             P01ord = '0';
038600090429             KPJBU  = Param01;
038700090429             tntb461r (KPJBA);
038800090429             Param01 = KPJBU;
038900090429             if  P01ke1 <> *zero;
039000090429               V1Cksc = P01ke1;
039100090429               reset TIBS02ds;
039200090429               T02sif = KNSIF;
039300090429               T02ke1 = V1Cksc;
039400090429               TNTBE_RicercaControllo(kpjba : tibs02ds);
039500090429               if T02err = *blanks;
039600090429                 dLAC   = T02uni;
039700090429               else;
039800090429                 clear dlac;
039900090429                 errMessage  = *on;
040000090429                 PosCurKsc   = *on;
040100090429                 V1Dmsg = T02msg;
040200090429               endif;
040300090429               errGenerico = *on;
040400090429               leavesr;
040500090128             endif;
040600130913             errGenerico = *on;
040700130913             leavesr;
040800090128
040900090128       // Controllo immissione codice cliente
041000090128           WHEN  V1Cksc = *blanks  or  V1Cksc = *zeros;
041100090128             errMessage  = *on;
041200090128             errGenerico = *on;
041300090128             PosCurKsc   = *on;
041400090128             V1Dmsg = $Msg(01);
041500090128             leavesr;
041600090128
041700090128       // Controllo immissione solo caratteri numerici
041800090128           WHEN  %check(DigitN : V1Cksc) > *zeros;
041900090128             errMessage  = *on;
042000090128             errGenerico = *on;
042100090128             PosCurksc   = *on;
042200090128             V1Dmsg = $Msg(02);
042300090128             leavesr;
042400090128
042500090128         ENDSL;
042600071211
042700090128       // - Verifica esistenza codice cliente in tabella "LAC"
042800090128         reset TIBS02ds;
042900090128         T02sif = KNSIF;
043000090128         T02ke1 = V1Cksc;
043100090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
043200090128         if T02err = *blanks;
043300090128           V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
043400090128           dLAC   = T02uni;
043500090128           V1Dksc = §LACrag;
043600090128         else;
043700090128           errMessage  = *on;
043800090128           errGenerico = *on;
043900090128           PosCurKsc   = *on;
044000090128           V1Dmsg = $Msg(03);
044100090128           leavesr;
044200090128         endif;
044300071211
044400071211       //? IMPOSTAZIONE VIDEATA SUCCESSIVA ?
044500071211
044600090128         $Video  = 'D2';
044700090128         $InzD02 = *on;
044800071210
044900071210       ENDSR;
045000071210
045100071211       //--------------------------------------------------------------
045200071210       //?Gestione videata D02
045300071211       //--------------------------------------------------------------
045400071210       BEGSR GesD02;
045500071210
045600071210       // Inizializzazione videata
045700071210         if $InzD02 = *on;
045800071210           exsr InzD02;
045900071210           $InzD02 = *off;
046000071210         endif;
046100071210
046200071210       // Emissione testata
046300071210         if  NOT  errMessage;
046400071210           write SB15T01;
046500071210         endif;
046600071210
046700071210       // Emissione videata
046800071210         exfmt SB15D02;
046900071210         errMessage  = *off;
047000071210         errGenerico = *off;
047100071210         clear V1Dmsg;
047200071210
047300071210         select;
047400131112       // F1=Vis. Log Elab
047500131112           when *inKA;
047600131112             TNSB04R(kpjba);
047700131112       // F3=Fine
047800131112           when *inKC;
047900131112             exsr F03D01;
048000071217       // F9=Visualizzazione tab. LAC
048100071217           when *inKI;
048200071217             exsr F09D02;
048300110211       // F11=Personalizzazione lancio
048400110211           when *inKK;
048500110211             kritb = '1';
048600110211             BCH09(kpjba);
048700071210       // F12=Ritorno
048800071210           when *inKL;
048900071210             exsr F12D02;
049000071210       // Controllo videata
049100071210           other;
049200071210             exsr CtrD02;
049300071210             select;
049400071210         // Rilevato errore
049500071210               when errGenerico;
049600071210                 leavesr;
049700071210         // F6-Conferma
049800071210               when *inKF;
049900071210                 $Video = *zeros;
050000071210             endsl;
050100071210         endsl;
050200071210
050300071210       ENDSR;
050400071210
050500071211       //--------------------------------------------------------------
050600071210       //?Inizializzazione videata D02
050700071211       //--------------------------------------------------------------
050800071210       BEGSR InzD02;
050900071210
051000071210         clear SB15D02;
051100071210
051200071211         V2Cksc = %dec(V1Cksc : 7 : 0);
051300131030         PR_V2CKsc = '1';
051400071210         V2Dksc = V1Dksc;
051500131030         Save_V2CKSC = V2CKSC;
051600071210
051700071210         V1Cfmi   = §LACfmi;
051800071210         V1Cdir   = §LACdir;
051900090310         V1Ctpt   = §LACtpt;
052000090310         V1Cssr   = §LACssr;
052100090310         V1Clnp   = §LAClnp;
052200090310         V1Cres   = §LACres;
052300090310         V1Crec   = §LACrec;
052400090310         V1Ccsr   = §LACcsr;
052500090310         V1Ctad   = §LACtad;
052600090310       // se frequenza diversa da 'I' o 'J' la propongo vuota
052700090310         if §lactadu = 'I' or §lactadu = 'J';
052800090316           V1tadu  = §LACtadu;
052900090310         else;
053000090316           clear v1tadu;
053100090310         endif;
053200090310         V1Ckscf  = §LACksc;
053300090310         V1Cctr   = §LACctr;
053400090316         V1fimp  = §LACfimp;
053500090316         V1imp   = §LACimp;
053600090310         V1Dnote  = §LACnote;
053700110211
053800110211       //?Abilito F11
053900110211         F11Attivo = SeSonoIo;
054000071210
054100071210       ENDSR;
054200071217
054300071217       //--------------------------------------------------------------
054400071217       //?Gestione tasto funzionale F9 da videata D02
054500071217       //--------------------------------------------------------------
054600071217       BEGSR F09D02;
054700071217
054800071217       // Visualizzazione tab. LAC
054900071217         reset TNTB46ds;
055000071217         B46ke1 = %trim( %editw( V2Cksc : '0       ' ) );
055100071217
055200071217         kpjbu = TNTB46ds;
055300071217         tntb46r(kpjba);
055400071217         TNTB46ds = kpjbu;
055500071217
055600071217         select;
055700071217         // Ritorno con errore
055800071217           when B46err = *on;
055900071217             V1Dmsg  = B46msg;
056000071217             errMessage  = *on;
056100071217             errGenerico = *on;
056200071217         // Ritorno con F3
056300071217           when B46fxx = '1';
056400071217             $Fine   = *on;
056500071217             errGenerico = *on;
056600071217         endsl;
056700071217
056800071217       ENDSR;
056900071210
057000071211       //--------------------------------------------------------------
057100071210       //?Gestione tasto funzionale F12 da videata D02
057200071211       //--------------------------------------------------------------
057300071210       BEGSR F12D02;
057400071210
057500071210       // Ritorno alla videata precedente (D01)
057600071210         $Video = 'D1';
057700071210
057800071210       ENDSR;
057900071210
058000071211       //--------------------------------------------------------------
058100071210       //?Controllo dati immessi in videata D02
058200071211       //--------------------------------------------------------------
058300071210       BEGSR CtrD02;
058400071210
058500110211         //IndDspF = *zeros;
058600110211
058700110211         WindDspF = IndDspF;
058800110211         reset WindDspFb;
058900110211         IndDspF  = WindDspF;
059000131030
059100140707       // il codice non può essere 0
059200140707         if V2CKSC = 0;
059300140707           errMessage  = *on;
059400140707           errGenerico = *on;
059500140707           PosCurKsc   = *on;
059600140707           V1Dmsg = $Msg(01);
059700140707           leavesr;
059800140707         endif;
059900140707
060000131030       // se il codice è stato valorizzato con un dato diverso dal precedente,
060100131030       // reperisco i valori della tab.LAC
060200131030         if V2CKSC <> Save_V2CKSC and V2CKSC <> 0;
060300131030           reset TIBS02ds;
060400131030           T02sif = KNSIF;
060500131030           T02ke1 = %editc(V2Cksc:'X');
060600131030           TNTBE_RicercaControllo(kpjba : tibs02ds);
060700131030           if T02err = *blanks;
060800131030             dLAC   = T02uni;
060900131030             V2Dksc = §LACrag;
061000131030             Save_V2CKSC = V2CKSC;
061100131030           else;
061200131030             errMessage  = *on;
061300131030             errGenerico = *on;
061400131030             PosCurKsc   = *on;
061500131030             V1Dmsg = $Msg(03);
061600131030             leavesr;
061700131030           endif;
061800131030         endif;
061900071210
062000071210       // RE-impostazione dati di default - da tab.LAC (se tolti)
062100090310         if  V1Ctpt =  *blank;
062200090310           V1Ctpt   =  §LACtpt;
062300090310         endif;
062400090316         if  V1Cssr =  *blank;
062500090310           V1Cssr   =   §LACssr;
062600090310         endif;
062700090310         if  V1Cres =  *blank;
062800090310           V1Cres   =  §LACres;
062900090310         endif;
063000090310         if  V1Crec =  *blank;
063100090310           V1Crec   =  §LACrec;
063200090310         endif;
063300090310         if  V1Ccsr =  *blank;
063400090310           V1Ccsr   =  §LACcsr;
063500090310         endif;
063600090310         if  V1Ctad =  *blank;
063700090310           V1Ctad   =  §LACtad;
063800090310         endif;
063900071210         if  V1Cfmi =  *blank;
064000071210           V1Cfmi   =  §LACfmi;
064100071210         endif;
064200071210         if  V1Cdir =  *blank;
064300071210           V1Cdir   =  §LACdir;
064400071210         endif;
064500090311
064600090311         //?Flag nome immagine?
064700090311         clear  V1Dfmi;
064800090311         select;
064900090311           // - Obbligatorietà
065000090311           when  V1Cfmi = *blank;
065100090311             errMessage  = *on;
065200090311             errGenerico = *on;
065300090311             PosCurFmi   = *on;
065400090311             V1Dmsg = 'Flag nome immagine obbligatorio';
065500090311             leavesr;
065600090311           // - Ricerca
065700090311           when  %scan('?' : V1Cfmi) > *zero;
065800090311             clear  TIBS02ds;
065900090311             T02mod = 'R';
066000090311             T02cod = 'NIM';
066100090311             T02sif = KNSIF;
066200090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
066300090311             if  T02err = *blank;
066400090311               V1Cfmi = %subst(T02ke1 : 1 : %len(V1Cfmi));
066500090311               V1Dfmi = T02uni;
066600090311               errGenerico = *on;
066700090311               leavesr;
066800090311             else;
066900090311               errMessage  = *on;
067000090311               errGenerico = *on;
067100090311               PosCurFmi   = *on;
067200090311               V1Dmsg = T02msg;
067300090311               leavesr;
067400090311             endif;
067500090311           // - Controllo
067600090311           other;
067700090311             reset  TIBS02ds;
067800090311             T02cod = 'NIM';
067900090311             T02ke1 = V1Cfmi;
068000090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
068100090311             if  T02err <> *blank;
068200090311               errMessage  = *on;
068300090311               errGenerico = *on;
068400090311               PosCurFmi   = *on;
068500090311               V1Dmsg = 'Flag nome immagine errato';
068600090311               leavesr;
068700090311             endif;
068800090311             V1Dfmi = T02uni;
068900090311         ENDSL;
069000090311
069100090311         //?Directory per immagini?
069200090311         select;
069300090311           when  V1Cdir  = *blank;
069400090311             errMessage  = *on;
069500090311             errGenerico = *on;
069600090311             PosCurDir   = *on;
069700090311             V1Dmsg = $Msg(14);
069800090311             leavesr;
069900090311           when  %scan('\':V1Cdir) > *zero;
070000090311             errMessage  = *on;
070100090311             errGenerico = *on;
070200090311             PosCurDir   = *on;
070300090311             V1Dmsg = $Msg(15);
070400090311             leavesr;
070500071210
070600071210       // Controllo immissione di caratteri NON previsti nella directory
070700090316           when  %scan('&' : V1Cdir) > *zero  or
070800071211             %scan('%' : V1Cdir) > *zero;
070900090316             errMessage  = *on;
071000090316             errGenerico = *on;
071100090316             PosCurDir   = *on;
071200090316             V1Dmsg = $Msg(09);
071300090316             leavesr;
071400090311         endsl;
071500090311
071600090311         //?Tipo addebito (Creazione TITAS)?
071700090311         select;
071800090311           // Se "N" non impostare i dati dell'addebito forzato
071900090311           when  V1Ctad = 'N'     and
072000090311                (V1Ckscf <> *zero  or  V1Cctr <> *blank
072100090311                                   or  V1imp <> *zero);
072200090311             errMessage  = *on;
072300090311             errGenerico = *on;
072400090311             PosCurTad   = *on;
072500090311             V1Dmsg = 'Se NO addebito non impostare le forzature';
072600090311             leavesr;
072700090311         endsl;
072800090311
072900090311         //?Codice cliente?
073000090311         select;
073100090311           // - Non ammessa tariffa senza cliente
073200090311           when  V1Ckscf =  *zero   and   V1Cctr <> *blank;
073300090311               errMessage  = *on;
073400090311               errGenerico = *on;
073500090311               PosCurKscf  = *on;
073600090311               V1Dmsg = 'Cliente tassazione obbligatorio SE +
073700090311                         inserita la relativa tariffa';
073800090311               leavesr;
073900090311           // - Controllo cliente tassazione
074000090311           when  V1Ckscf <> *zero;
074100090311             clear  TIBS69ds;
074200090311             tibs69ds.I69kcc = DUTkci;
074300090311             tibs69ds.I69kac = %int(V1Ckscf);
074400090311             tibs69ds.I69sif = knsif;
074500090311             tibs69r(TIBS69ds : ds_CNACO :
074600090311                                ds_CNIND :
074700090311                                ds_CNCLP :
074800090311                                ds_FNCLS);
074900090311             if  tibs69ds.O69err = *on;
075000090311               errMessage  = *on;
075100090311               errGenerico = *on;
075200090311               PosCurKscf  = *on;
075300090311               V1Dmsg = 'Cliente tassazione errato';
075400090311               leavesr;
075500090311             endif;
075600090311             if  V1Cctr = *blank;
075700090311               errMessage  = *on;
075800090311               errGenerico = *on;
075900090311               PosCurCtr   = *on;
076000090311               V1Dmsg = 'Inserire il codice tariffa';
076100090311               leavesr;
076200090311             endif;
076300090311         endsl;
076400090311
076500090311         //?Importo & Tipo importo forzato?
076600090311         select;
076700090311           // - Non ammesso importo per Varia Negata
076800090311           when  V1Ctad = 'V'   and   V1imp <> *zero;
076900090311             errMessage  = *on;
077000090311             errGenerico = *on;
077100090311             PosCurImp   = *on;
077200090311             V1Dmsg = 'NON ammesso l''importo forzato per +
077300090311                       Varia Negata';
077400090311             leavesr;
077500090311           // - Non ammesso tipo importo senza importo
077600090311           when  V1imp =  *zero   and   V1fimp <> *blank;
077700090311             errMessage  = *on;
077800090311             errGenerico = *on;
077900090311             PosCurImp   = *on;
078000090311             V1Dmsg = 'Importo forzato obbligatorio SE +
078100090311                       inserito il tipo di importo';
078200090311             leavesr;
078300090311           // - Se inserito importo devono essere inseriti il ksc il ctr
078400090311           //   e il tipo importo
078500090311           when  V1imp <> *zero;
078600090311             select;
078700090311               when  V1fimp  = *blank;
078800090311                 errMessage  = *on;
078900090311                 errGenerico = *on;
079000090311                 PosCurFimp  = *on;
079100090311                 V1Dmsg = 'Immettere il tipo importo';
079200090311                 leavesr;
079300090311               when  V1Ckscf = *zero;
079400090311                 errMessage  = *on;
079500090311                 errGenerico = *on;
079600090311                 PosCurKscf  = *on;
079700090311                 V1Dmsg = 'Immettere il cliente forzato';
079800090311                 leavesr;
079900100204               when  V1Cctr  = *blank;
080000090311                 errMessage  = *on;
080100090311                 errGenerico = *on;
080200090311                 PosCurCtr   = *on;
080300090311                 V1Dmsg = 'Immettere la tariffa forzata';
080400090311                 leavesr;
080500090311             endsl;
080600090311         endsl;
080700071210
080800090311         //?Controllo parzializzazioni per data
080900090128         reset $Parz01a;
081000090128         reset $Parz01b;
081100071210         if  V1Cdcd <> *zero   or  V1Cdca <> *zero;
081200071210           $Parz01a =  *on;
081300071210         endif;
081400071210         if  V1Cdsd <> *zero   or  V1Cdsa <> *zero;
081500071210           $Parz01b =  *on;
081600071210         endif;
081700071210
081800071210         select;
081900071210       // Richiesta nessuna parzializzazione per data
082000071210           when  $Parz01a = *off  and  $Parz01b = *off;
082100071210             errMessage   = *on;
082200071210             errGenerico  = *on;
082300071210             PosCurDcd    = *on;
082400071211             V1Dmsg =  $Msg(10);
082500071210             leavesr;
082600071210       // Richieste troppe  parzializzazioni per data
082700071210           when  $Parz01a = *on  and  $Parz01b = *on;
082800071210             errMessage   = *on;
082900071210             errGenerico  = *on;
083000071210             PosCurDcd    = *on;
083100071211             V1Dmsg = $Msg(11);
083200071210             leavesr;
083300071210       // Parzializzazione per data consegna dal/al
083400071210           when  $Parz01a = *on;
083500071210         // - controllo data iniziale
083600071210             clear WLBdat;
083700071210             G08dat = V1Cdcd;
083800071210             xsrda8(wlbdat);
083900071210             if G08err = *off;
084000071210               V1Cdcd  = G08dat;
084100071210               W1Cdcd  = G08inv;
084200071210             else;
084300071210               errMessage  = *on;
084400071210               errGenerico = *on;
084500071210               PosCurDcd   = *on;
084600071211               V1Dmsg  = $Msg(12);
084700071210               leavesr;
084800071210             endif;
084900071210         // - controllo data finale
085000071210             clear WLBdat;
085100071210             G08dat = V1Cdca;
085200071210             xsrda8(wlbdat);
085300071210             if G08err = *off;
085400071210               V1Cdca  = G08dat;
085500071210               W1Cdca  = G08inv;
085600071210             else;
085700071210               errMessage  = *on;
085800071210               errGenerico = *on;
085900071210               PosCurDca   = *on;
086000071211               V1Dmsg  = $Msg(12);
086100071210               leavesr;
086200071210             endif;
086300071210         // - controllo range
086400071210             if W1Cdcd > W1Cdca;
086500071210               errMessage  = *on;
086600071210               errGenerico = *on;
086700071210               PosCurDcd   = *on;
086800071211               V1Dmsg  = $Msg(13);
086900071210               leavesr;
087000071210             endif;
087100071210
087200071210       // parzializzazione per data spedizione dal/al
087300090128           when  $Parz01b = *on;
087400071210         // - controllo data iniziale
087500071210             clear WLBdat;
087600071210             G08dat = V1Cdsd;
087700071210             xsrda8(wlbdat);
087800071210             if G08err = *off;
087900071210               V1Cdsd  = G08dat;
088000071210               W1Cdsd  = G08inv;
088100071210             else;
088200071210               errMessage  = *on;
088300071210               errGenerico = *on;
088400071210               PosCurDsd   = *on;
088500071211               V1Dmsg  = $Msg(12);
088600071210               leavesr;
088700071210             endif;
088800071210         // - controllo data finale
088900071210             clear WLBdat;
089000071210             G08dat = V1Cdsa;
089100071210             xsrda8(wlbdat);
089200071210             if G08err = *off;
089300071210               V1Cdsa  = G08dat;
089400071210               W1Cdsa  = G08inv;
089500071210             else;
089600071210               errMessage  = *on;
089700071210               errGenerico = *on;
089800071210               PosCurDsa   = *on;
089900071211               V1Dmsg  = $Msg(12);
090000071210               leavesr;
090100071210             endif;
090200071210         // - controllo range
090300071210             if W1Cdsd > W1Cdsa;
090400071210               errMessage  = *on;
090500071210               errGenerico = *on;
090600071210               PosCurDsd   = *on;
090700071211               V1Dmsg  = $Msg(13);
090800071210               leavesr;
090900071210             endif;
091000071210
091100071210         endsl;
091200071210
091300071210       ENDSR;
091400071211
091500071211       //--------------------------------------------------------------
091600071211       //?Sottomissione lavoro batch
091700071211       //--------------------------------------------------------------
091800071211       BEGSR SbmJOB;
091900071211
092000071211       // Impostazione parametri
092100071211         reset TNSB16ds;
092200071211         D16dcd = W1Cdcd;
092300071211         D16dca = W1Cdca;
092400071211         D16dsd = W1Cdsd;
092500071211         D16dsa = W1Cdsa;
092600071211         D16ksc = V2Cksc;
092700071211         D16imm = V1Cimm;
092800090310         D16tad = V1Ctad;
092900071211         D16dir = V1Cdir;
093000071211         D16fmi = V1Cfmi;
093100090310         d16ksu = v2cksc;
093200090316         d16tadu = v1tadu;
093300090316         d16fimp = v1fimp;
093400090316         d16imp  = v1imp;
093500090310         d16kscf = v1ckscf;
093600090310         d16ctr  = v1cctr;
093700090310       // ID Job
093800100506       // - imposto ksc + oggi + progressivo
093900100506         d16job = %editc(v2cksc:'X') + %editc(dstime_ymd:'X') + '0';
094000100506       // - controllo, capita spesso che si lanci lo stesso cliente + volte nello stesso giorno
094100100506         exsr ctridjob;
094200090310
094300090310         d16tpt = v1ctpt;
094400090310         d16res = v1cres;
094500090310         d16rec = v1crec;
094600090310         d16csr = v1ccsr;
094700090310         d16ssr = v1cssr;
094800090310         d16lnp = v1clnp;
094900071211
095000071211         kcoaz = 'SB16';
095100071211         kpjbu = TNSB16ds;
095200071211         if  knmus = *all'1';
095300071211           tnsb16r(kpjba);
095400071211         else;
095500071211           BCH10(kpjba);
095600071211         endif;
095700090327
095800090327       // scrivo rcd spia su tilac con lacela = '33'
095900090327            clear tilac000;
096000090327            lactim = %char(%timestamp:*iso0);
096100090327            lacdir = d16dir;
096200090327            lacela = '33';
096300090327            lactela = 'S';
096400090327            lacksu = d16ksu;
096500090402            lactad = d16tad;
096600090327            lacidjob = d16job;
096700090327            write tilac000;
096800071211
096900131030       // Non esco dal pgm ma resto nella videata di lancio pulendo il codice cliente
097000131030         $Fine = *off;
097100131030         $Video = 'D2';
097200131030         V2CKsc = 0;
097300140707         V2DKsc = *blank;
097400131030         PR_V2CKsc = '0';
097500071211
097600071211       ENDSR;
097700100506
097800100506       //--------------------------------------------------------------
097900100506       //?Controllo ID job deve essere univoco.
098000100506       //--------------------------------------------------------------
098100100506       BEGSR CtrIdJob;
098200100506
098300100506         $FineLAC = *off;
098400100506         widjob15 = %subst(d16job:1:15);
098500100506
098600100506         exec sql
098700100506          DECLARE LAC cursor for
098800100506          SELECT   lacidjob from tilac00f
098900100506          WHERE    substr(lacidjob, 1, 15) = :widjob15
099000100506                   and lacela = '33'
099100100506          ORDER BY lacidjob;
099200100506
099300100506          exec sql
099400100506           OPEN LAC;
099500100506
099600100506          DOW not $FineLAC;
099700100506            exec sql
099800100506             FETCH next from LAC into: widjob;
099900100506             IF sqlcod = 100 or sqlcod < 0;
100000100506               $FineLAC = *on;
100100100506               leave;
100200100506             ENDIF;
100300100506
100400100506            wprogr = %dec(%subst(d16job:16:1):1:0);
100500100506            wprogr += 1;
100600100506            %subst(d16job:16:1) = %editc(wprogr:'X');
100700100506          ENDDO;
100800100506
100900100506        exec sql
101000100506         CLOSE LAC;
101100100506
101200100506       ENDSR;
101300071210
101400071211       //--------------------------------------------------------------
101500071210       //?Operazioni finali.
101600071211       //--------------------------------------------------------------
101700071210       BEGSR RoutEnd;
101800071210
101900071210         clear TIBS02ds;
102000071210         T02tla = 'C';
102100090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
102200071210
102300071210         *inLR = *on;
102400071210         return;
102500071210
102600071210       ENDSR;
102700071210
102800071210      /end-free
102900040720
103000071211       //--------------------------------------------------------------
103100071210       //?Schiere a tempo di compilazione.
103200071211       //--------------------------------------------------------------
103300040720
103400071210**   $Msg -------------------------------------------------------------------*
103500071210Immettere il codice cliente                                                     1
103600071210Immettere SOLO caratteri numerici                                               2
103700071210Codice cliente mancante o non presente in tabella "LAC"                         3
103800071210Immettere almeno un codice cliente oppure un numero spedizione                  4
103900071210Parzializzare per codice cliente OPPURE per numero spedizione                   5
104000071210Numero spedizione errato                                                        6
104100071210Spedizione non DPD                                                              7
104200071210Il cliente della spedizione non è in tabella "LAC"                              8
104300071210Caratteri "&" e "%" non ammessi per la directory del cliente                    9
104400071211Immettere almeno la data consegna oppure la data spedizione                    10
104500071211Parzializzare per data consegna OPPURE per data spedizione                     11
104600071211Data formalmente errata                                                        12
104700071211Data "dal" maggiore di data "al"                                               13
104800090311Immettere la directory                                                         14
104900090311Carattere "\" non valido; per indicare una sottocartella utilizzare "/"        15
