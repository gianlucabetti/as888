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
001200071218     f***                                  infds(InfDspF)
001300071210     f                                     indds(IndDspF)
001400090327     ftilac00f  o    e             disk
001500040720
001600071210      //---------------------------------------------------------------
001700071210      //?Definizione costanti.
001800071210      //---------------------------------------------------------------
001900090127
002000071210      // - Costante per controllo "caratteri solo numerici"
002100071210     d DigitN          c                   const('0123456789')
002200090127
002300090127     d c_Ktrc          c                   const('A')
002400071210
002500071210      //---------------------------------------------------------------
002600071210      //?Definizione schiere.
002700071210      //---------------------------------------------------------------
002800090127
002900071210      // - Messaggi a video
003000090311     d $Msg            s             78    dim(15) ctdata  perrcd(1)
003100071210
003200071210      //---------------------------------------------------------------
003300071210      //?Definizione aree dati.
003400071210      //---------------------------------------------------------------
003500090127
003600071210      // - Dati utente
003700071210     d §AzUte        e ds                  extname(AZUTE00F)
003800071210     d                                     dtaara
003900071210     d §DatiUte      e ds                  extname(dDatiUte)
004000071210     d                                     dtaara
004100071210
004200071210      //---------------------------------------------------------------
004300071210      //?Definizione strutture dati.
004400071210      //---------------------------------------------------------------
004500090127
004600071210      // - Status
004700071210     d Psds           sds
004800071210     d   SDSpgm          *proc
004900071210     d   SDSjob              244    253                                         Job name
005000071210
005100071210      // - Indicatori su DspF
005200071218     d IndDspF         ds
005300110211        // - Indicatori di abilitazione tasto funzione
005400110211     d   F11Attivo                    1n   overlay(IndDspF:11)
005500071210        // - Indicatori di errore
005600071210     d   errMessage                   1n   overlay(IndDspF:28)
005700071210     d   PosCurKsc                    1n   overlay(IndDspF:40)
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
016700071210     d W1Cdcd          s                   like(V1Cdcd)   inz
016800071210     d W1Cdca          s                   like(V1Cdca)   inz
016900071210     d W1Cdsd          s                   like(V1Cdsd)   inz
017000071210     d W1Cdsa          s                   like(V1Cdsa)   inz
017100071210     d WdateISO        s               d   datfmt(*ISO)   inz(*job)
017200071210     d WtimeHMS        s               t   timfmt(*HMS)   inz
017300100506     d wprogr          s              1  0 inz
017400100506     d widjob15        s             15a
017500100506     d widjob          s             16a
017600090127
017700090127      //---------------------------------------------------------------
017800090127      //?Definizione procedure usate.
017900090127      //---------------------------------------------------------------
018000090127
018100090127      // - Reperimento dati utente
018200090316      /copy gaitrasrc/srcProtoPR,TIBS34R
018300090127
018400090316       // - Ricerca/Controllo tabelle
018500090316      /copy gaitrasrc/srcProtoPR,TIBS02R
018600090316
018700090316       // - Controllo/Decodifica cliente
018800090316      /copy gaitrasrc/srcProtoPR,TIBS69R
018900090127
019000090127      // - Visualizzazione tab. "LAC"
019100090127     d tntb46r         pr                  extpgm('TNTB46R')
019200090127     d  kpjba                              likeds(KPJBA)
019300090429
019400090429       // - Interrogazione tabella LAC
019500090429     d tntb461r        pr                  extpgm('TNTB461R')
019600090429     d  kpjba                              likeds(KPJBA)
019700090127
019800090127      // - Controllo ed inversione date
019900090127     d xsrda8          pr                  extpgm('XSRDA8')
020000090127     d  wlbdat                             likeds(WLBdat)
020100110211
020200110211      // - Personalizzazione lavoro batch
020300110211     d bch09           pr                  extpgm('BCH09')
020400110211     d  kpjba                              likeds(KPJBA)
020500090127
020600090127      // - Sottomissione lavoro batch
020700090127     d bch10           pr                  extpgm('BCH10')
020800090127     d  kpjba                              likeds(KPJBA)
020900090127
021000090127      // - Richiamo diretto lavoro batch
021100090127     d tnsb16r         pr                  extpgm('TNSB16R')
021200090127     d  kpjba                              likeds(KPJBA)
021300090127
021400090127      //---------------------------------------------------------------
021500090127      //?Definizione key-list.
021600090127      //---------------------------------------------------------------
021700090127
021800040720
021900071210      //---------------------------------------------------------------
022000071210      //?Riepilogo indicatori.
022100071210      //---------------------------------------------------------------
022200071210      //  28    - Emissione messaggio di errore a video
022300071210      //  40    - Codice cliente errato
022400071211      //  41    - Directory per immagini errata
022500071211      //  42    - Data consegna DAL errata o range errato
022600071211      //  43    - Data consegna AL  errata
022700071211      //  44    - Data spedizione DAL errata o range errato
022800071211      //  45    - Data spedizione AL  errata o range errato
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
031500071210           write SB15T01;
031600071210         endif;
031700071210
031800071210       // Emissione videata
031900071210         exfmt SB15D01;
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
034300071210         clear SB15D01;
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
038600090429             tntb461r (KPJBA);
038700090429             Param01 = KPJBU;
038800090429             if  P01ke1 <> *zero;
038900090429               V1Cksc = P01ke1;
039000090429               reset TIBS02ds;
039100090429               T02sif = KNSIF;
039200090429               T02ke1 = V1Cksc;
039300090429               TNTBE_RicercaControllo(kpjba : tibs02ds);
039400090429               if T02err = *blanks;
039500090429                 dLAC   = T02uni;
039600090429               else;
039700090429                 clear dlac;
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
042400090128       // - Verifica esistenza codice cliente in tabella "LAC"
042500090128         reset TIBS02ds;
042600090128         T02sif = KNSIF;
042700090128         T02ke1 = V1Cksc;
042800090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
042900090128         if T02err = *blanks;
043000090128           V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
043100090128           dLAC   = T02uni;
043200090128           V1Dksc = §LACrag;
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
046100071210           write SB15T01;
046200071210         endif;
046300071210
046400071210       // Emissione videata
046500071210         exfmt SB15D02;
046600071210         errMessage  = *off;
046700071210         errGenerico = *off;
046800071210         clear V1Dmsg;
046900071210
047000071210         select;
047100071210       // F3=Fine
047200071210           when *inKC;
047300071210             exsr F03D01;
047400071217       // F9=Visualizzazione tab. LAC
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
050400071210         clear SB15D02;
050500071210
050600071211         V2Cksc = %dec(V1Cksc : 7 : 0);
050700071210         V2Dksc = V1Dksc;
050800071210
050900071210         V1Cfmi   = §LACfmi;
051000071210         V1Cdir   = §LACdir;
051100090310         V1Ctpt   = §LACtpt;
051200090310         V1Cssr   = §LACssr;
051300090310         V1Clnp   = §LAClnp;
051400090310         V1Cres   = §LACres;
051500090310         V1Crec   = §LACrec;
051600090310         V1Ccsr   = §LACcsr;
051700090310         V1Ctad   = §LACtad;
051800090310       // se frequenza diversa da 'I' o 'J' la propongo vuota
051900090310         if §lactadu = 'I' or §lactadu = 'J';
052000090316           V1tadu  = §LACtadu;
052100090310         else;
052200090316           clear v1tadu;
052300090310         endif;
052400090310         V1Ckscf  = §LACksc;
052500090310         V1Cctr   = §LACctr;
052600090316         V1fimp  = §LACfimp;
052700090316         V1imp   = §LACimp;
052800090310         V1Dnote  = §LACnote;
052900110211
053000110211       //?Abilito F11
053100110211         F11Attivo = SeSonoIo;
053200071210
053300071210       ENDSR;
053400071217
053500071217       //--------------------------------------------------------------
053600071217       //?Gestione tasto funzionale F9 da videata D02
053700071217       //--------------------------------------------------------------
053800071217       BEGSR F09D02;
053900071217
054000071217       // Visualizzazione tab. LAC
054100071217         reset TNTB46ds;
054200071217         B46ke1 = %trim( %editw( V2Cksc : '0       ' ) );
054300071217
054400071217         kpjbu = TNTB46ds;
054500071217         tntb46r(kpjba);
054600071217         TNTB46ds = kpjbu;
054700071217
054800071217         select;
054900071217         // Ritorno con errore
055000071217           when B46err = *on;
055100071217             V1Dmsg  = B46msg;
055200071217             errMessage  = *on;
055300071217             errGenerico = *on;
055400071217         // Ritorno con F3
055500071217           when B46fxx = '1';
055600071217             $Fine   = *on;
055700071217             errGenerico = *on;
055800071217         endsl;
055900071217
056000071217       ENDSR;
056100071210
056200071211       //--------------------------------------------------------------
056300071210       //?Gestione tasto funzionale F12 da videata D02
056400071211       //--------------------------------------------------------------
056500071210       BEGSR F12D02;
056600071210
056700071210       // Ritorno alla videata precedente (D01)
056800071210         $Video = 'D1';
056900071210
057000071210       ENDSR;
057100071210
057200071211       //--------------------------------------------------------------
057300071210       //?Controllo dati immessi in videata D02
057400071211       //--------------------------------------------------------------
057500071210       BEGSR CtrD02;
057600071210
057700110211         //IndDspF = *zeros;
057800110211
057900110211         WindDspF = IndDspF;
058000110211         reset WindDspFb;
058100110211         IndDspF  = WindDspF;
058200071210
058300071210       // RE-impostazione dati di default - da tab.LAC (se tolti)
058400090310         if  V1Ctpt =  *blank;
058500090310           V1Ctpt   =  §LACtpt;
058600090310         endif;
058700090316         if  V1Cssr =  *blank;
058800090310           V1Cssr   =   §LACssr;
058900090310         endif;
059000090310         if  V1Cres =  *blank;
059100090310           V1Cres   =  §LACres;
059200090310         endif;
059300090310         if  V1Crec =  *blank;
059400090310           V1Crec   =  §LACrec;
059500090310         endif;
059600090310         if  V1Ccsr =  *blank;
059700090310           V1Ccsr   =  §LACcsr;
059800090310         endif;
059900090310         if  V1Ctad =  *blank;
060000090310           V1Ctad   =  §LACtad;
060100090310         endif;
060200071210         if  V1Cfmi =  *blank;
060300071210           V1Cfmi   =  §LACfmi;
060400071210         endif;
060500071210         if  V1Cdir =  *blank;
060600071210           V1Cdir   =  §LACdir;
060700071210         endif;
060800090311
060900090311         //?Flag nome immagine?
061000090311         clear  V1Dfmi;
061100090311         select;
061200090311           // - Obbligatorietà
061300090311           when  V1Cfmi = *blank;
061400090311             errMessage  = *on;
061500090311             errGenerico = *on;
061600090311             PosCurFmi   = *on;
061700090311             V1Dmsg = 'Flag nome immagine obbligatorio';
061800090311             leavesr;
061900090311           // - Ricerca
062000090311           when  %scan('?' : V1Cfmi) > *zero;
062100090311             clear  TIBS02ds;
062200090311             T02mod = 'R';
062300090311             T02cod = 'NIM';
062400090311             T02sif = KNSIF;
062500090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
062600090311             if  T02err = *blank;
062700090311               V1Cfmi = %subst(T02ke1 : 1 : %len(V1Cfmi));
062800090311               V1Dfmi = T02uni;
062900090311               errGenerico = *on;
063000090311               leavesr;
063100090311             else;
063200090311               errMessage  = *on;
063300090311               errGenerico = *on;
063400090311               PosCurFmi   = *on;
063500090311               V1Dmsg = T02msg;
063600090311               leavesr;
063700090311             endif;
063800090311           // - Controllo
063900090311           other;
064000090311             reset  TIBS02ds;
064100090311             T02cod = 'NIM';
064200090311             T02ke1 = V1Cfmi;
064300090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
064400090311             if  T02err <> *blank;
064500090311               errMessage  = *on;
064600090311               errGenerico = *on;
064700090311               PosCurFmi   = *on;
064800090311               V1Dmsg = 'Flag nome immagine errato';
064900090311               leavesr;
065000090311             endif;
065100090311             V1Dfmi = T02uni;
065200090311         ENDSL;
065300090311
065400090311         //?Directory per immagini?
065500090311         select;
065600090311           when  V1Cdir  = *blank;
065700090311             errMessage  = *on;
065800090311             errGenerico = *on;
065900090311             PosCurDir   = *on;
066000090311             V1Dmsg = $Msg(14);
066100090311             leavesr;
066200090311           when  %scan('\':V1Cdir) > *zero;
066300090311             errMessage  = *on;
066400090311             errGenerico = *on;
066500090311             PosCurDir   = *on;
066600090311             V1Dmsg = $Msg(15);
066700090311             leavesr;
066800071210
066900071210       // Controllo immissione di caratteri NON previsti nella directory
067000090316           when  %scan('&' : V1Cdir) > *zero  or
067100071211             %scan('%' : V1Cdir) > *zero;
067200090316             errMessage  = *on;
067300090316             errGenerico = *on;
067400090316             PosCurDir   = *on;
067500090316             V1Dmsg = $Msg(09);
067600090316             leavesr;
067700090311         endsl;
067800090311
067900090311         //?Tipo addebito (Creazione TITAS)?
068000090311         select;
068100090311           // Se "N" non impostare i dati dell'addebito forzato
068200090311           when  V1Ctad = 'N'     and
068300090311                (V1Ckscf <> *zero  or  V1Cctr <> *blank
068400090311                                   or  V1imp <> *zero);
068500090311             errMessage  = *on;
068600090311             errGenerico = *on;
068700090311             PosCurTad   = *on;
068800090311             V1Dmsg = 'Se NO addebito non impostare le forzature';
068900090311             leavesr;
069000090311         endsl;
069100090311
069200090311         //?Codice cliente?
069300090311         select;
069400090311           // - Non ammessa tariffa senza cliente
069500090311           when  V1Ckscf =  *zero   and   V1Cctr <> *blank;
069600090311               errMessage  = *on;
069700090311               errGenerico = *on;
069800090311               PosCurKscf  = *on;
069900090311               V1Dmsg = 'Cliente tassazione obbligatorio SE +
070000090311                         inserita la relativa tariffa';
070100090311               leavesr;
070200090311           // - Controllo cliente tassazione
070300090311           when  V1Ckscf <> *zero;
070400090311             clear  TIBS69ds;
070500090311             tibs69ds.I69kcc = DUTkci;
070600090311             tibs69ds.I69kac = %int(V1Ckscf);
070700090311             tibs69ds.I69sif = knsif;
070800090311             tibs69r(TIBS69ds : ds_CNACO :
070900090311                                ds_CNIND :
071000090311                                ds_CNCLP :
071100090311                                ds_FNCLS);
071200090311             if  tibs69ds.O69err = *on;
071300090311               errMessage  = *on;
071400090311               errGenerico = *on;
071500090311               PosCurKscf  = *on;
071600090311               V1Dmsg = 'Cliente tassazione errato';
071700090311               leavesr;
071800090311             endif;
071900090311             if  V1Cctr = *blank;
072000090311               errMessage  = *on;
072100090311               errGenerico = *on;
072200090311               PosCurCtr   = *on;
072300090311               V1Dmsg = 'Inserire il codice tariffa';
072400090311               leavesr;
072500090311             endif;
072600090311         endsl;
072700090311
072800090311         //?Importo & Tipo importo forzato?
072900090311         select;
073000090311           // - Non ammesso importo per Varia Negata
073100090311           when  V1Ctad = 'V'   and   V1imp <> *zero;
073200090311             errMessage  = *on;
073300090311             errGenerico = *on;
073400090311             PosCurImp   = *on;
073500090311             V1Dmsg = 'NON ammesso l''importo forzato per +
073600090311                       Varia Negata';
073700090311             leavesr;
073800090311           // - Non ammesso tipo importo senza importo
073900090311           when  V1imp =  *zero   and   V1fimp <> *blank;
074000090311             errMessage  = *on;
074100090311             errGenerico = *on;
074200090311             PosCurImp   = *on;
074300090311             V1Dmsg = 'Importo forzato obbligatorio SE +
074400090311                       inserito il tipo di importo';
074500090311             leavesr;
074600090311           // - Se inserito importo devono essere inseriti il ksc il ctr
074700090311           //   e il tipo importo
074800090311           when  V1imp <> *zero;
074900090311             select;
075000090311               when  V1fimp  = *blank;
075100090311                 errMessage  = *on;
075200090311                 errGenerico = *on;
075300090311                 PosCurFimp  = *on;
075400090311                 V1Dmsg = 'Immettere il tipo importo';
075500090311                 leavesr;
075600090311               when  V1Ckscf = *zero;
075700090311                 errMessage  = *on;
075800090311                 errGenerico = *on;
075900090311                 PosCurKscf  = *on;
076000090311                 V1Dmsg = 'Immettere il cliente forzato';
076100090311                 leavesr;
076200100204               when  V1Cctr  = *blank;
076300090311                 errMessage  = *on;
076400090311                 errGenerico = *on;
076500090311                 PosCurCtr   = *on;
076600090311                 V1Dmsg = 'Immettere la tariffa forzata';
076700090311                 leavesr;
076800090311             endsl;
076900090311         endsl;
077000071210
077100090311         //?Controllo parzializzazioni per data
077200090128         reset $Parz01a;
077300090128         reset $Parz01b;
077400071210         if  V1Cdcd <> *zero   or  V1Cdca <> *zero;
077500071210           $Parz01a =  *on;
077600071210         endif;
077700071210         if  V1Cdsd <> *zero   or  V1Cdsa <> *zero;
077800071210           $Parz01b =  *on;
077900071210         endif;
078000071210
078100071210         select;
078200071210       // Richiesta nessuna parzializzazione per data
078300071210           when  $Parz01a = *off  and  $Parz01b = *off;
078400071210             errMessage   = *on;
078500071210             errGenerico  = *on;
078600071210             PosCurDcd    = *on;
078700071211             V1Dmsg =  $Msg(10);
078800071210             leavesr;
078900071210       // Richieste troppe  parzializzazioni per data
079000071210           when  $Parz01a = *on  and  $Parz01b = *on;
079100071210             errMessage   = *on;
079200071210             errGenerico  = *on;
079300071210             PosCurDcd    = *on;
079400071211             V1Dmsg = $Msg(11);
079500071210             leavesr;
079600071210       // Parzializzazione per data consegna dal/al
079700071210           when  $Parz01a = *on;
079800071210         // - controllo data iniziale
079900071210             clear WLBdat;
080000071210             G08dat = V1Cdcd;
080100071210             xsrda8(wlbdat);
080200071210             if G08err = *off;
080300071210               V1Cdcd  = G08dat;
080400071210               W1Cdcd  = G08inv;
080500071210             else;
080600071210               errMessage  = *on;
080700071210               errGenerico = *on;
080800071210               PosCurDcd   = *on;
080900071211               V1Dmsg  = $Msg(12);
081000071210               leavesr;
081100071210             endif;
081200071210         // - controllo data finale
081300071210             clear WLBdat;
081400071210             G08dat = V1Cdca;
081500071210             xsrda8(wlbdat);
081600071210             if G08err = *off;
081700071210               V1Cdca  = G08dat;
081800071210               W1Cdca  = G08inv;
081900071210             else;
082000071210               errMessage  = *on;
082100071210               errGenerico = *on;
082200071210               PosCurDca   = *on;
082300071211               V1Dmsg  = $Msg(12);
082400071210               leavesr;
082500071210             endif;
082600071210         // - controllo range
082700071210             if W1Cdcd > W1Cdca;
082800071210               errMessage  = *on;
082900071210               errGenerico = *on;
083000071210               PosCurDcd   = *on;
083100071211               V1Dmsg  = $Msg(13);
083200071210               leavesr;
083300071210             endif;
083400071210
083500071210       // parzializzazione per data spedizione dal/al
083600090128           when  $Parz01b = *on;
083700071210         // - controllo data iniziale
083800071210             clear WLBdat;
083900071210             G08dat = V1Cdsd;
084000071210             xsrda8(wlbdat);
084100071210             if G08err = *off;
084200071210               V1Cdsd  = G08dat;
084300071210               W1Cdsd  = G08inv;
084400071210             else;
084500071210               errMessage  = *on;
084600071210               errGenerico = *on;
084700071210               PosCurDsd   = *on;
084800071211               V1Dmsg  = $Msg(12);
084900071210               leavesr;
085000071210             endif;
085100071210         // - controllo data finale
085200071210             clear WLBdat;
085300071210             G08dat = V1Cdsa;
085400071210             xsrda8(wlbdat);
085500071210             if G08err = *off;
085600071210               V1Cdsa  = G08dat;
085700071210               W1Cdsa  = G08inv;
085800071210             else;
085900071210               errMessage  = *on;
086000071210               errGenerico = *on;
086100071210               PosCurDsa   = *on;
086200071211               V1Dmsg  = $Msg(12);
086300071210               leavesr;
086400071210             endif;
086500071210         // - controllo range
086600071210             if W1Cdsd > W1Cdsa;
086700071210               errMessage  = *on;
086800071210               errGenerico = *on;
086900071210               PosCurDsd   = *on;
087000071211               V1Dmsg  = $Msg(13);
087100071210               leavesr;
087200071210             endif;
087300071210
087400071210         endsl;
087500071210
087600071210       ENDSR;
087700071211
087800071211       //--------------------------------------------------------------
087900071211       //?Sottomissione lavoro batch
088000071211       //--------------------------------------------------------------
088100071211       BEGSR SbmJOB;
088200071211
088300071211       // Impostazione parametri
088400071211         reset TNSB16ds;
088500071211         D16dcd = W1Cdcd;
088600071211         D16dca = W1Cdca;
088700071211         D16dsd = W1Cdsd;
088800071211         D16dsa = W1Cdsa;
088900071211         D16ksc = V2Cksc;
089000071211         D16imm = V1Cimm;
089100090310         D16tad = V1Ctad;
089200071211         D16dir = V1Cdir;
089300071211         D16fmi = V1Cfmi;
089400090310         d16ksu = v2cksc;
089500090316         d16tadu = v1tadu;
089600090316         d16fimp = v1fimp;
089700090316         d16imp  = v1imp;
089800090310         d16kscf = v1ckscf;
089900090310         d16ctr  = v1cctr;
090000090310       // ID Job
090100100506       // - imposto ksc + oggi + progressivo
090200100506         d16job = %editc(v2cksc:'X') + %editc(dstime_ymd:'X') + '0';
090300100506       // - controllo, capita spesso che si lanci lo stesso cliente + volte nello stesso giorno
090400100506         exsr ctridjob;
090500090310
090600090310         d16tpt = v1ctpt;
090700090310         d16res = v1cres;
090800090310         d16rec = v1crec;
090900090310         d16csr = v1ccsr;
091000090310         d16ssr = v1cssr;
091100090310         d16lnp = v1clnp;
091200071211
091300071211         kcoaz = 'SB16';
091400071211         kpjbu = TNSB16ds;
091500071211         if  knmus = *all'1';
091600071211           tnsb16r(kpjba);
091700071211         else;
091800071211           BCH10(kpjba);
091900071211         endif;
092000090327
092100090327       // scrivo rcd spia su tilac con lacela = '33'
092200090327            clear tilac000;
092300090327            lactim = %char(%timestamp:*iso0);
092400090327            lacdir = d16dir;
092500090327            lacela = '33';
092600090327            lactela = 'S';
092700090327            lacksu = d16ksu;
092800090402            lactad = d16tad;
092900090327            lacidjob = d16job;
093000090327            write tilac000;
093100071211
093200071211       // Uscita dal pgm
093300071211         $Fine = *on;
093400071211         reset $Video;
093500071211         reset $InzD01;
093600071211
093700071211       ENDSR;
093800100506
093900100506       //--------------------------------------------------------------
094000100506       //?Controllo ID job deve essere univoco.
094100100506       //--------------------------------------------------------------
094200100506       BEGSR CtrIdJob;
094300100506
094400100506         $FineLAC = *off;
094500100506         widjob15 = %subst(d16job:1:15);
094600100506
094700100506         exec sql
094800100506          DECLARE LAC cursor for
094900100506          SELECT   lacidjob from tilac00f
095000100506          WHERE    substr(lacidjob, 1, 15) = :widjob15
095100100506                   and lacela = '33'
095200100506          ORDER BY lacidjob;
095300100506
095400100506          exec sql
095500100506           OPEN LAC;
095600100506
095700100506          DOW not $FineLAC;
095800100506            exec sql
095900100506             FETCH next from LAC into: widjob;
096000100506             IF sqlcod = 100 or sqlcod < 0;
096100100506               $FineLAC = *on;
096200100506               leave;
096300100506             ENDIF;
096400100506
096500100506            wprogr = %dec(%subst(d16job:16:1):1:0);
096600100506            wprogr += 1;
096700100506            %subst(d16job:16:1) = %editc(wprogr:'X');
096800100506          ENDDO;
096900100506
097000100506        exec sql
097100100506         CLOSE LAC;
097200100506
097300100506       ENDSR;
097400071210
097500071211       //--------------------------------------------------------------
097600071210       //?Operazioni finali.
097700071211       //--------------------------------------------------------------
097800071210       BEGSR RoutEnd;
097900071210
098000071210         clear TIBS02ds;
098100071210         T02tla = 'C';
098200090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
098300071210
098400071210         *inLR = *on;
098500071210         return;
098600071210
098700071210       ENDSR;
098800071210
098900071210      /end-free
099000040720
099100071211       //--------------------------------------------------------------
099200071210       //?Schiere a tempo di compilazione.
099300071211       //--------------------------------------------------------------
099400040720
099500071210**   $Msg -------------------------------------------------------------------*
099600071210Immettere il codice cliente                                                     1
099700071210Immettere SOLO caratteri numerici                                               2
099800071210Codice cliente mancante o non presente in tabella "LAC"                         3
099900071210Immettere almeno un codice cliente oppure un numero spedizione                  4
100000071210Parzializzare per codice cliente OPPURE per numero spedizione                   5
100100071210Numero spedizione errato                                                        6
100200071210Spedizione non DPD                                                              7
100300071210Il cliente della spedizione non è in tabella "LAC"                              8
100400071210Caratteri "&" e "%" non ammessi per la directory del cliente                    9
100500071211Immettere almeno la data consegna oppure la data spedizione                    10
100600071211Parzializzare per data consegna OPPURE per data spedizione                     11
100700071211Data formalmente errata                                                        12
100800071211Data "dal" maggiore di data "al"                                               13
100900090311Immettere la directory                                                         14
101000090311Carattere "\" non valido; per indicare una sottocartella utilizzare "/"        15
