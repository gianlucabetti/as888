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
021400090127
021500090127      //---------------------------------------------------------------
021600090127      //?Definizione key-list.
021700090127      //---------------------------------------------------------------
021800090127
021900040720
022000071210      //---------------------------------------------------------------
022100071210      //?Riepilogo indicatori.
022200071210      //---------------------------------------------------------------
022300071210      //  28    - Emissione messaggio di errore a video
022400071210      //  40    - Codice cliente errato
022500071211      //  41    - Directory per immagini errata
022600071211      //  42    - Data consegna DAL errata o range errato
022700071211      //  43    - Data consegna AL  errata
022800071211      //  44    - Data spedizione DAL errata o range errato
022900071211      //  45    - Data spedizione AL  errata o range errato
023000071210      //  90    - Generico di errore
023100071210      //---------------------------------------------------------------
023200040720
023300090127      //---------------------------------------------------------------
023400090127      //?M A I N - L I N E.
023500090127      //---------------------------------------------------------------
023600090127
023700040720     c     *Entry        plist
023800040720     c                   parm                    KPJBA
023900071210
024000071210      /free
024100071210
024200071210       // Operazioni iniziali
024300071210       exsr RoutInz;
024400071210
024500071210       // Gestione video
024600071210       DOW $Fine = *off;
024700071210         select;
024800071210           when $Video = 'D1';
024900071210             exsr GesD01;
025000071210           when $Video = 'D2';
025100071210             exsr GesD02;
025200071210           when $Video = *zero;
025300071210             exsr SbmJOB;
025400071210           other;
025500071210             leave;
025600071210         endsl;
025700071210       ENDDO;
025800071210
025900071210       // Operazioni finali
026000071210       exsr RoutEnd;
026100071210
026200071211       //--------------------------------------------------------------
026300071210       //?Operazioni iniziali.
026400071211       //--------------------------------------------------------------
026500071210       BEGSR RoutInz;
026600071210
026700071210       // Reperimento dati job
026800071210         exsr DatiJob;
026900110211
027000110211       //?Se utente EDPMB visualizzo F11
027100110211         SeSonoIo = (KNMUS = 'EDPMB');
027200071210
027300071210       // Impostazione nome programma a video
027400071210         V1Tpgm = SDSpgm;
027500071210
027600071210       // Impostazione campo LACTIM = aaaa/mm/gg+hh:mm:ss
027700071210         wTimeHMS = %time();
027800071211         dsTIME_ymd = %dec(wDateISO : *iso);
027900071211         dsTIME_hms = %dec(wTimeHMS : *hms);
028000100506
028100100506         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028200071210
028300071210       ENDSR;
028400071210
028500071211       //--------------------------------------------------------------
028600071210       //?Reperimento Dati del job (Utente/Operativi).
028700071211       //--------------------------------------------------------------
028800071210       BEGSR DatiJob;
028900071210
029000071210         in(E) §AzUte;
029100071210         if NOT %error;
029200071210           in(E) §DatiUte;
029300071210         endif;
029400071210         if %error or RSut = *blanks;
029500071210           clear TIBS34ds;
029600071210           tibs34r(tibs34ds);
029700071210           in §AzUte;
029800071210           in §DatiUte;
029900071210         endif;
030000071210
030100071210       ENDSR;
030200071210
030300071211       //--------------------------------------------------------------
030400071210       //?Gestione videata D01
030500071211       //--------------------------------------------------------------
030600071210       BEGSR GesD01;
030700071210
030800071210       // Inizializzazione videata
030900071210         if $InzD01 = *on;
031000071210           exsr InzD01;
031100071210           $InzD01 = *off;
031200071210         endif;
031300071210
031400071210       // Emissione videata
031500130913         write SB15T01;
031600071210         exfmt SB15D01;
031700071210         errMessage  = *off;
031800071210         errGenerico = *off;
031900071210         clear V1Dmsg;
032000071210
032100071210         select;
032200071210       // F3=Fine
032300071210           when *inKC;
032400071210             exsr F03D01;
032500071210       // Enter
032600071210           other;
032700071210             exsr CtrD01;
032800071210             if errGenerico;
032900071210               leavesr;
033000071210             endif;
033100071210         endsl;
033200071210
033300071210       ENDSR;
033400071210
033500071211       //--------------------------------------------------------------
033600071210       //?Inizializzazione videata D01
033700071211       //--------------------------------------------------------------
033800071210       BEGSR InzD01;
033900071210
034000071210         clear SB15D01;
034100071210
034200071210       ENDSR;
034300071210
034400071211       //--------------------------------------------------------------
034500071210       //?Gestione tasto funzionale F3 da videata D01
034600071211       //--------------------------------------------------------------
034700071210       BEGSR F03D01;
034800071210
034900071210       // Chiude il programma
035000071210         $Fine = *on;
035100071210
035200071210       ENDSR;
035300071210
035400071211       //--------------------------------------------------------------
035500071210       //?Controllo dati immessi in videata D01
035600071211       //--------------------------------------------------------------
035700071210       BEGSR CtrD01;
035800071210
035900110211         //IndDspF = *zeros;
036000110211
036100110211         WindDspF = IndDspF;
036200110211         reset WindDspFb;
036300110211         IndDspF  = WindDspF;
036400071211
036500071211       //? CONTROLLO PARZIALIZZAZIONI EFFETTUATE ?
036600090128       //? (UNICA SELEZIONE: PER CLIENTE)        ?
036700071211
036800090128         SELECT;
036900090128
037000071211       // - Nessuna parzializzazione richiesta
037100090128           WHEN  V1Cksc = *zero   or   V1Cksc = *blank;
037200071211             errMessage  = *on;
037300071211             errGenerico = *on;
037400071211             PosCurKsc   = *on;
037500071211             V1Dmsg = $Msg(04);
037600071211             leavesr;
037700071211
037800090128       // Ricerca su codice cliente
037900090128           WHEN  %scan('?' : V1Cksc) > *zeros;
038000090429             clear  Param01;
038100090429             P01ord = '0';
038200090429             KPJBU  = Param01;
038300090429             tntb461r (KPJBA);
038400090429             Param01 = KPJBU;
038500090429             if  P01ke1 <> *zero;
038600090429               V1Cksc = P01ke1;
038700090429               reset TIBS02ds;
038800090429               T02sif = KNSIF;
038900090429               T02ke1 = V1Cksc;
039000090429               TNTBE_RicercaControllo(kpjba : tibs02ds);
039100090429               if T02err = *blanks;
039200090429                 dLAC   = T02uni;
039300090429               else;
039400090429                 clear dlac;
039500090429                 errMessage  = *on;
039600090429                 PosCurKsc   = *on;
039700090429                 V1Dmsg = T02msg;
039800090429               endif;
039900090429               errGenerico = *on;
040000090429               leavesr;
040100090128             endif;
040200130913             errGenerico = *on;
040300130913             leavesr;
040400090128
040500090128       // Controllo immissione codice cliente
040600090128           WHEN  V1Cksc = *blanks  or  V1Cksc = *zeros;
040700090128             errMessage  = *on;
040800090128             errGenerico = *on;
040900090128             PosCurKsc   = *on;
041000090128             V1Dmsg = $Msg(01);
041100090128             leavesr;
041200090128
041300090128       // Controllo immissione solo caratteri numerici
041400090128           WHEN  %check(DigitN : V1Cksc) > *zeros;
041500090128             errMessage  = *on;
041600090128             errGenerico = *on;
041700090128             PosCurksc   = *on;
041800090128             V1Dmsg = $Msg(02);
041900090128             leavesr;
042000090128
042100090128         ENDSL;
042200071211
042300090128       // - Verifica esistenza codice cliente in tabella "LAC"
042400090128         reset TIBS02ds;
042500090128         T02sif = KNSIF;
042600090128         T02ke1 = V1Cksc;
042700090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
042800090128         if T02err = *blanks;
042900090128           V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
043000090128           dLAC   = T02uni;
043100090128           V1Dksc = §LACrag;
043200090128         else;
043300090128           errMessage  = *on;
043400090128           errGenerico = *on;
043500090128           PosCurKsc   = *on;
043600090128           V1Dmsg = $Msg(03);
043700090128           leavesr;
043800090128         endif;
043900071211
044000071211       //? IMPOSTAZIONE VIDEATA SUCCESSIVA ?
044100071211
044200090128         $Video  = 'D2';
044300090128         $InzD02 = *on;
044400071210
044500071210       ENDSR;
044600071210
044700071211       //--------------------------------------------------------------
044800071210       //?Gestione videata D02
044900071211       //--------------------------------------------------------------
045000071210       BEGSR GesD02;
045100071210
045200071210       // Inizializzazione videata
045300071210         if $InzD02 = *on;
045400071210           exsr InzD02;
045500071210           $InzD02 = *off;
045600071210         endif;
045700071210
045800071210       // Emissione testata
045900071210         if  NOT  errMessage;
046000071210           write SB15T01;
046100071210         endif;
046200071210
046300071210       // Emissione videata
046400071210         exfmt SB15D02;
046500071210         errMessage  = *off;
046600071210         errGenerico = *off;
046700071210         clear V1Dmsg;
046800071210
046900071210         select;
047000071210       // F3=Fine
047100071210           when *inKC;
047200071210             exsr F03D01;
047300071217       // F9=Visualizzazione tab. LAC
047400071217           when *inKI;
047500071217             exsr F09D02;
047600110211       // F11=Personalizzazione lancio
047700110211           when *inKK;
047800110211             kritb = '1';
047900110211             BCH09(kpjba);
048000071210       // F12=Ritorno
048100071210           when *inKL;
048200071210             exsr F12D02;
048300071210       // Controllo videata
048400071210           other;
048500071210             exsr CtrD02;
048600071210             select;
048700071210         // Rilevato errore
048800071210               when errGenerico;
048900071210                 leavesr;
049000071210         // F6-Conferma
049100071210               when *inKF;
049200071210                 $Video = *zeros;
049300071210             endsl;
049400071210         endsl;
049500071210
049600071210       ENDSR;
049700071210
049800071211       //--------------------------------------------------------------
049900071210       //?Inizializzazione videata D02
050000071211       //--------------------------------------------------------------
050100071210       BEGSR InzD02;
050200071210
050300071210         clear SB15D02;
050400071210
050500071211         V2Cksc = %dec(V1Cksc : 7 : 0);
050600131030         PR_V2CKsc = '1';
050700071210         V2Dksc = V1Dksc;
050800131030         Save_V2CKSC = V2CKSC;
050900071210
051000071210         V1Cfmi   = §LACfmi;
051100071210         V1Cdir   = §LACdir;
051200090310         V1Ctpt   = §LACtpt;
051300090310         V1Cssr   = §LACssr;
051400090310         V1Clnp   = §LAClnp;
051500090310         V1Cres   = §LACres;
051600090310         V1Crec   = §LACrec;
051700090310         V1Ccsr   = §LACcsr;
051800090310         V1Ctad   = §LACtad;
051900090310       // se frequenza diversa da 'I' o 'J' la propongo vuota
052000090310         if §lactadu = 'I' or §lactadu = 'J';
052100090316           V1tadu  = §LACtadu;
052200090310         else;
052300090316           clear v1tadu;
052400090310         endif;
052500090310         V1Ckscf  = §LACksc;
052600090310         V1Cctr   = §LACctr;
052700090316         V1fimp  = §LACfimp;
052800090316         V1imp   = §LACimp;
052900090310         V1Dnote  = §LACnote;
053000110211
053100110211       //?Abilito F11
053200110211         F11Attivo = SeSonoIo;
053300071210
053400071210       ENDSR;
053500071217
053600071217       //--------------------------------------------------------------
053700071217       //?Gestione tasto funzionale F9 da videata D02
053800071217       //--------------------------------------------------------------
053900071217       BEGSR F09D02;
054000071217
054100071217       // Visualizzazione tab. LAC
054200071217         reset TNTB46ds;
054300071217         B46ke1 = %trim( %editw( V2Cksc : '0       ' ) );
054400071217
054500071217         kpjbu = TNTB46ds;
054600071217         tntb46r(kpjba);
054700071217         TNTB46ds = kpjbu;
054800071217
054900071217         select;
055000071217         // Ritorno con errore
055100071217           when B46err = *on;
055200071217             V1Dmsg  = B46msg;
055300071217             errMessage  = *on;
055400071217             errGenerico = *on;
055500071217         // Ritorno con F3
055600071217           when B46fxx = '1';
055700071217             $Fine   = *on;
055800071217             errGenerico = *on;
055900071217         endsl;
056000071217
056100071217       ENDSR;
056200071210
056300071211       //--------------------------------------------------------------
056400071210       //?Gestione tasto funzionale F12 da videata D02
056500071211       //--------------------------------------------------------------
056600071210       BEGSR F12D02;
056700071210
056800071210       // Ritorno alla videata precedente (D01)
056900071210         $Video = 'D1';
057000071210
057100071210       ENDSR;
057200071210
057300071211       //--------------------------------------------------------------
057400071210       //?Controllo dati immessi in videata D02
057500071211       //--------------------------------------------------------------
057600071210       BEGSR CtrD02;
057700071210
057800110211         //IndDspF = *zeros;
057900110211
058000110211         WindDspF = IndDspF;
058100110211         reset WindDspFb;
058200110211         IndDspF  = WindDspF;
058300131030
058400131030       // se il codice è stato valorizzato con un dato diverso dal precedente,
058500131030       // reperisco i valori della tab.LAC
058600131030         if V2CKSC <> Save_V2CKSC and V2CKSC <> 0;
058700131030           reset TIBS02ds;
058800131030           T02sif = KNSIF;
058900131030           T02ke1 = %editc(V2Cksc:'X');
059000131030           TNTBE_RicercaControllo(kpjba : tibs02ds);
059100131030           if T02err = *blanks;
059200131030             dLAC   = T02uni;
059300131030             V2Dksc = §LACrag;
059400131030             Save_V2CKSC = V2CKSC;
059500131030           else;
059600131030             errMessage  = *on;
059700131030             errGenerico = *on;
059800131030             PosCurKsc   = *on;
059900131030             V1Dmsg = $Msg(03);
060000131030             leavesr;
060100131030           endif;
060200131030         endif;
060300071210
060400071210       // RE-impostazione dati di default - da tab.LAC (se tolti)
060500090310         if  V1Ctpt =  *blank;
060600090310           V1Ctpt   =  §LACtpt;
060700090310         endif;
060800090316         if  V1Cssr =  *blank;
060900090310           V1Cssr   =   §LACssr;
061000090310         endif;
061100090310         if  V1Cres =  *blank;
061200090310           V1Cres   =  §LACres;
061300090310         endif;
061400090310         if  V1Crec =  *blank;
061500090310           V1Crec   =  §LACrec;
061600090310         endif;
061700090310         if  V1Ccsr =  *blank;
061800090310           V1Ccsr   =  §LACcsr;
061900090310         endif;
062000090310         if  V1Ctad =  *blank;
062100090310           V1Ctad   =  §LACtad;
062200090310         endif;
062300071210         if  V1Cfmi =  *blank;
062400071210           V1Cfmi   =  §LACfmi;
062500071210         endif;
062600071210         if  V1Cdir =  *blank;
062700071210           V1Cdir   =  §LACdir;
062800071210         endif;
062900090311
063000090311         //?Flag nome immagine?
063100090311         clear  V1Dfmi;
063200090311         select;
063300090311           // - Obbligatorietà
063400090311           when  V1Cfmi = *blank;
063500090311             errMessage  = *on;
063600090311             errGenerico = *on;
063700090311             PosCurFmi   = *on;
063800090311             V1Dmsg = 'Flag nome immagine obbligatorio';
063900090311             leavesr;
064000090311           // - Ricerca
064100090311           when  %scan('?' : V1Cfmi) > *zero;
064200090311             clear  TIBS02ds;
064300090311             T02mod = 'R';
064400090311             T02cod = 'NIM';
064500090311             T02sif = KNSIF;
064600090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
064700090311             if  T02err = *blank;
064800090311               V1Cfmi = %subst(T02ke1 : 1 : %len(V1Cfmi));
064900090311               V1Dfmi = T02uni;
065000090311               errGenerico = *on;
065100090311               leavesr;
065200090311             else;
065300090311               errMessage  = *on;
065400090311               errGenerico = *on;
065500090311               PosCurFmi   = *on;
065600090311               V1Dmsg = T02msg;
065700090311               leavesr;
065800090311             endif;
065900090311           // - Controllo
066000090311           other;
066100090311             reset  TIBS02ds;
066200090311             T02cod = 'NIM';
066300090311             T02ke1 = V1Cfmi;
066400090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
066500090311             if  T02err <> *blank;
066600090311               errMessage  = *on;
066700090311               errGenerico = *on;
066800090311               PosCurFmi   = *on;
066900090311               V1Dmsg = 'Flag nome immagine errato';
067000090311               leavesr;
067100090311             endif;
067200090311             V1Dfmi = T02uni;
067300090311         ENDSL;
067400090311
067500090311         //?Directory per immagini?
067600090311         select;
067700090311           when  V1Cdir  = *blank;
067800090311             errMessage  = *on;
067900090311             errGenerico = *on;
068000090311             PosCurDir   = *on;
068100090311             V1Dmsg = $Msg(14);
068200090311             leavesr;
068300090311           when  %scan('\':V1Cdir) > *zero;
068400090311             errMessage  = *on;
068500090311             errGenerico = *on;
068600090311             PosCurDir   = *on;
068700090311             V1Dmsg = $Msg(15);
068800090311             leavesr;
068900071210
069000071210       // Controllo immissione di caratteri NON previsti nella directory
069100090316           when  %scan('&' : V1Cdir) > *zero  or
069200071211             %scan('%' : V1Cdir) > *zero;
069300090316             errMessage  = *on;
069400090316             errGenerico = *on;
069500090316             PosCurDir   = *on;
069600090316             V1Dmsg = $Msg(09);
069700090316             leavesr;
069800090311         endsl;
069900090311
070000090311         //?Tipo addebito (Creazione TITAS)?
070100090311         select;
070200090311           // Se "N" non impostare i dati dell'addebito forzato
070300090311           when  V1Ctad = 'N'     and
070400090311                (V1Ckscf <> *zero  or  V1Cctr <> *blank
070500090311                                   or  V1imp <> *zero);
070600090311             errMessage  = *on;
070700090311             errGenerico = *on;
070800090311             PosCurTad   = *on;
070900090311             V1Dmsg = 'Se NO addebito non impostare le forzature';
071000090311             leavesr;
071100090311         endsl;
071200090311
071300090311         //?Codice cliente?
071400090311         select;
071500090311           // - Non ammessa tariffa senza cliente
071600090311           when  V1Ckscf =  *zero   and   V1Cctr <> *blank;
071700090311               errMessage  = *on;
071800090311               errGenerico = *on;
071900090311               PosCurKscf  = *on;
072000090311               V1Dmsg = 'Cliente tassazione obbligatorio SE +
072100090311                         inserita la relativa tariffa';
072200090311               leavesr;
072300090311           // - Controllo cliente tassazione
072400090311           when  V1Ckscf <> *zero;
072500090311             clear  TIBS69ds;
072600090311             tibs69ds.I69kcc = DUTkci;
072700090311             tibs69ds.I69kac = %int(V1Ckscf);
072800090311             tibs69ds.I69sif = knsif;
072900090311             tibs69r(TIBS69ds : ds_CNACO :
073000090311                                ds_CNIND :
073100090311                                ds_CNCLP :
073200090311                                ds_FNCLS);
073300090311             if  tibs69ds.O69err = *on;
073400090311               errMessage  = *on;
073500090311               errGenerico = *on;
073600090311               PosCurKscf  = *on;
073700090311               V1Dmsg = 'Cliente tassazione errato';
073800090311               leavesr;
073900090311             endif;
074000090311             if  V1Cctr = *blank;
074100090311               errMessage  = *on;
074200090311               errGenerico = *on;
074300090311               PosCurCtr   = *on;
074400090311               V1Dmsg = 'Inserire il codice tariffa';
074500090311               leavesr;
074600090311             endif;
074700090311         endsl;
074800090311
074900090311         //?Importo & Tipo importo forzato?
075000090311         select;
075100090311           // - Non ammesso importo per Varia Negata
075200090311           when  V1Ctad = 'V'   and   V1imp <> *zero;
075300090311             errMessage  = *on;
075400090311             errGenerico = *on;
075500090311             PosCurImp   = *on;
075600090311             V1Dmsg = 'NON ammesso l''importo forzato per +
075700090311                       Varia Negata';
075800090311             leavesr;
075900090311           // - Non ammesso tipo importo senza importo
076000090311           when  V1imp =  *zero   and   V1fimp <> *blank;
076100090311             errMessage  = *on;
076200090311             errGenerico = *on;
076300090311             PosCurImp   = *on;
076400090311             V1Dmsg = 'Importo forzato obbligatorio SE +
076500090311                       inserito il tipo di importo';
076600090311             leavesr;
076700090311           // - Se inserito importo devono essere inseriti il ksc il ctr
076800090311           //   e il tipo importo
076900090311           when  V1imp <> *zero;
077000090311             select;
077100090311               when  V1fimp  = *blank;
077200090311                 errMessage  = *on;
077300090311                 errGenerico = *on;
077400090311                 PosCurFimp  = *on;
077500090311                 V1Dmsg = 'Immettere il tipo importo';
077600090311                 leavesr;
077700090311               when  V1Ckscf = *zero;
077800090311                 errMessage  = *on;
077900090311                 errGenerico = *on;
078000090311                 PosCurKscf  = *on;
078100090311                 V1Dmsg = 'Immettere il cliente forzato';
078200090311                 leavesr;
078300100204               when  V1Cctr  = *blank;
078400090311                 errMessage  = *on;
078500090311                 errGenerico = *on;
078600090311                 PosCurCtr   = *on;
078700090311                 V1Dmsg = 'Immettere la tariffa forzata';
078800090311                 leavesr;
078900090311             endsl;
079000090311         endsl;
079100071210
079200090311         //?Controllo parzializzazioni per data
079300090128         reset $Parz01a;
079400090128         reset $Parz01b;
079500071210         if  V1Cdcd <> *zero   or  V1Cdca <> *zero;
079600071210           $Parz01a =  *on;
079700071210         endif;
079800071210         if  V1Cdsd <> *zero   or  V1Cdsa <> *zero;
079900071210           $Parz01b =  *on;
080000071210         endif;
080100071210
080200071210         select;
080300071210       // Richiesta nessuna parzializzazione per data
080400071210           when  $Parz01a = *off  and  $Parz01b = *off;
080500071210             errMessage   = *on;
080600071210             errGenerico  = *on;
080700071210             PosCurDcd    = *on;
080800071211             V1Dmsg =  $Msg(10);
080900071210             leavesr;
081000071210       // Richieste troppe  parzializzazioni per data
081100071210           when  $Parz01a = *on  and  $Parz01b = *on;
081200071210             errMessage   = *on;
081300071210             errGenerico  = *on;
081400071210             PosCurDcd    = *on;
081500071211             V1Dmsg = $Msg(11);
081600071210             leavesr;
081700071210       // Parzializzazione per data consegna dal/al
081800071210           when  $Parz01a = *on;
081900071210         // - controllo data iniziale
082000071210             clear WLBdat;
082100071210             G08dat = V1Cdcd;
082200071210             xsrda8(wlbdat);
082300071210             if G08err = *off;
082400071210               V1Cdcd  = G08dat;
082500071210               W1Cdcd  = G08inv;
082600071210             else;
082700071210               errMessage  = *on;
082800071210               errGenerico = *on;
082900071210               PosCurDcd   = *on;
083000071211               V1Dmsg  = $Msg(12);
083100071210               leavesr;
083200071210             endif;
083300071210         // - controllo data finale
083400071210             clear WLBdat;
083500071210             G08dat = V1Cdca;
083600071210             xsrda8(wlbdat);
083700071210             if G08err = *off;
083800071210               V1Cdca  = G08dat;
083900071210               W1Cdca  = G08inv;
084000071210             else;
084100071210               errMessage  = *on;
084200071210               errGenerico = *on;
084300071210               PosCurDca   = *on;
084400071211               V1Dmsg  = $Msg(12);
084500071210               leavesr;
084600071210             endif;
084700071210         // - controllo range
084800071210             if W1Cdcd > W1Cdca;
084900071210               errMessage  = *on;
085000071210               errGenerico = *on;
085100071210               PosCurDcd   = *on;
085200071211               V1Dmsg  = $Msg(13);
085300071210               leavesr;
085400071210             endif;
085500071210
085600071210       // parzializzazione per data spedizione dal/al
085700090128           when  $Parz01b = *on;
085800071210         // - controllo data iniziale
085900071210             clear WLBdat;
086000071210             G08dat = V1Cdsd;
086100071210             xsrda8(wlbdat);
086200071210             if G08err = *off;
086300071210               V1Cdsd  = G08dat;
086400071210               W1Cdsd  = G08inv;
086500071210             else;
086600071210               errMessage  = *on;
086700071210               errGenerico = *on;
086800071210               PosCurDsd   = *on;
086900071211               V1Dmsg  = $Msg(12);
087000071210               leavesr;
087100071210             endif;
087200071210         // - controllo data finale
087300071210             clear WLBdat;
087400071210             G08dat = V1Cdsa;
087500071210             xsrda8(wlbdat);
087600071210             if G08err = *off;
087700071210               V1Cdsa  = G08dat;
087800071210               W1Cdsa  = G08inv;
087900071210             else;
088000071210               errMessage  = *on;
088100071210               errGenerico = *on;
088200071210               PosCurDsa   = *on;
088300071211               V1Dmsg  = $Msg(12);
088400071210               leavesr;
088500071210             endif;
088600071210         // - controllo range
088700071210             if W1Cdsd > W1Cdsa;
088800071210               errMessage  = *on;
088900071210               errGenerico = *on;
089000071210               PosCurDsd   = *on;
089100071211               V1Dmsg  = $Msg(13);
089200071210               leavesr;
089300071210             endif;
089400071210
089500071210         endsl;
089600071210
089700071210       ENDSR;
089800071211
089900071211       //--------------------------------------------------------------
090000071211       //?Sottomissione lavoro batch
090100071211       //--------------------------------------------------------------
090200071211       BEGSR SbmJOB;
090300071211
090400071211       // Impostazione parametri
090500071211         reset TNSB16ds;
090600071211         D16dcd = W1Cdcd;
090700071211         D16dca = W1Cdca;
090800071211         D16dsd = W1Cdsd;
090900071211         D16dsa = W1Cdsa;
091000071211         D16ksc = V2Cksc;
091100071211         D16imm = V1Cimm;
091200090310         D16tad = V1Ctad;
091300071211         D16dir = V1Cdir;
091400071211         D16fmi = V1Cfmi;
091500090310         d16ksu = v2cksc;
091600090316         d16tadu = v1tadu;
091700090316         d16fimp = v1fimp;
091800090316         d16imp  = v1imp;
091900090310         d16kscf = v1ckscf;
092000090310         d16ctr  = v1cctr;
092100090310       // ID Job
092200100506       // - imposto ksc + oggi + progressivo
092300100506         d16job = %editc(v2cksc:'X') + %editc(dstime_ymd:'X') + '0';
092400100506       // - controllo, capita spesso che si lanci lo stesso cliente + volte nello stesso giorno
092500100506         exsr ctridjob;
092600090310
092700090310         d16tpt = v1ctpt;
092800090310         d16res = v1cres;
092900090310         d16rec = v1crec;
093000090310         d16csr = v1ccsr;
093100090310         d16ssr = v1cssr;
093200090310         d16lnp = v1clnp;
093300071211
093400071211         kcoaz = 'SB16';
093500071211         kpjbu = TNSB16ds;
093600071211         if  knmus = *all'1';
093700071211           tnsb16r(kpjba);
093800071211         else;
093900071211           BCH10(kpjba);
094000071211         endif;
094100090327
094200090327       // scrivo rcd spia su tilac con lacela = '33'
094300090327            clear tilac000;
094400090327            lactim = %char(%timestamp:*iso0);
094500090327            lacdir = d16dir;
094600090327            lacela = '33';
094700090327            lactela = 'S';
094800090327            lacksu = d16ksu;
094900090402            lactad = d16tad;
095000090327            lacidjob = d16job;
095100090327            write tilac000;
095200071211
095300131030       // Non esco dal pgm ma resto nella videata di lancio pulendo il codice cliente
095400131030         $Fine = *off;
095500131030         $Video = 'D2';
095600131030         V2CKsc = 0;
095700131030         PR_V2CKsc = '0';
095800071211
095900071211       ENDSR;
096000100506
096100100506       //--------------------------------------------------------------
096200100506       //?Controllo ID job deve essere univoco.
096300100506       //--------------------------------------------------------------
096400100506       BEGSR CtrIdJob;
096500100506
096600100506         $FineLAC = *off;
096700100506         widjob15 = %subst(d16job:1:15);
096800100506
096900100506         exec sql
097000100506          DECLARE LAC cursor for
097100100506          SELECT   lacidjob from tilac00f
097200100506          WHERE    substr(lacidjob, 1, 15) = :widjob15
097300100506                   and lacela = '33'
097400100506          ORDER BY lacidjob;
097500100506
097600100506          exec sql
097700100506           OPEN LAC;
097800100506
097900100506          DOW not $FineLAC;
098000100506            exec sql
098100100506             FETCH next from LAC into: widjob;
098200100506             IF sqlcod = 100 or sqlcod < 0;
098300100506               $FineLAC = *on;
098400100506               leave;
098500100506             ENDIF;
098600100506
098700100506            wprogr = %dec(%subst(d16job:16:1):1:0);
098800100506            wprogr += 1;
098900100506            %subst(d16job:16:1) = %editc(wprogr:'X');
099000100506          ENDDO;
099100100506
099200100506        exec sql
099300100506         CLOSE LAC;
099400100506
099500100506       ENDSR;
099600071210
099700071211       //--------------------------------------------------------------
099800071210       //?Operazioni finali.
099900071211       //--------------------------------------------------------------
100000071210       BEGSR RoutEnd;
100100071210
100200071210         clear TIBS02ds;
100300071210         T02tla = 'C';
100400090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
100500071210
100600071210         *inLR = *on;
100700071210         return;
100800071210
100900071210       ENDSR;
101000071210
101100071210      /end-free
101200040720
101300071211       //--------------------------------------------------------------
101400071210       //?Schiere a tempo di compilazione.
101500071211       //--------------------------------------------------------------
101600040720
101700071210**   $Msg -------------------------------------------------------------------*
101800071210Immettere il codice cliente                                                     1
101900071210Immettere SOLO caratteri numerici                                               2
102000071210Codice cliente mancante o non presente in tabella "LAC"                         3
102100071210Immettere almeno un codice cliente oppure un numero spedizione                  4
102200071210Parzializzare per codice cliente OPPURE per numero spedizione                   5
102300071210Numero spedizione errato                                                        6
102400071210Spedizione non DPD                                                              7
102500071210Il cliente della spedizione non è in tabella "LAC"                              8
102600071210Caratteri "&" e "%" non ammessi per la directory del cliente                    9
102700071211Immettere almeno la data consegna oppure la data spedizione                    10
102800071211Parzializzare per data consegna OPPURE per data spedizione                     11
102900071211Data formalmente errata                                                        12
103000071211Data "dal" maggiore di data "al"                                               13
103100090311Immettere la directory                                                         14
103200090311Carattere "\" non valido; per indicare una sottocartella utilizzare "/"        15
