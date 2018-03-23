000100071210      //---------------------------------------------------------------
000200071210      //?TNSB15R - Rigenerazione immagine LdV per cliente
000300071210      //---------------------------------------------------------------
000400090127
000500071210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000501180126     h DFTACTGRP(*NO)
000600040720
000700071210      //---------------------------------------------------------------
000800071210      //?Dichiarazione file.
000900071210      //---------------------------------------------------------------
001000040720
001100050317     fTNSB15D   cf   e             workstn
001200071210     f                                     indds(IndDspF)
001300090327     ftilac00f  o    e             disk
001400180126
001401180126     fPRTEMAIL  o    f  198        printer  oflind(*inOF)  usropn
001402180126
001500071210      //---------------------------------------------------------------
001600071210      //?Definizione costanti.
001700071210      //---------------------------------------------------------------
001800090127
001900071210      // - Costante per controllo "caratteri solo numerici"
002000071210     d DigitN          c                   const('0123456789')
002100090127
002200090127     d c_Ktrc          c                   const('A')
002300180126
002301180126      // - Comando di override al PrtF
002302180126     d CmdOvrPrtF      c                   const('OVRPRTF +
002303180126     d                                           file(PRTEMAIL) +
002304180126     d                                           pagesize(66 198) +
002305180126     d                                           lpi(6) cpi(10) +
002306180126     d                                           ovrscope(*actgrpdfn) +
002307180126     d                                           ')
002308180126     d CmdDltOvr       c                   const('DLTOVR +
002309180126     d                                            file(PRTEMAIL) +
002310180126     d                                            lvl(*actgrpdfn)')
002313180126     d Sede            c                   const('046')
002314180126
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
004900180126     d   JobUser             254    263
004901180126
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
010900071210
011000071210      // - Tabella "LAC" = Clienti per ritorno immagini
011100071210     d dLAC          e ds                  inz
011101180126
011102180126      // - Tabella "MRA" = Bart-Mailing -
011103180126     d dMRAdan       e ds                  inz
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
014801180126
014802180126      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
014803180126     d TRTCM1ds      e ds                  inz
014804180126      //   ·§CM1mitt = Indirizzo e-mail del mittente
014805180126     d   §CM1mitt    e                     inz('ced@brt.it')
014806180126      //   ·§CM1dst  = Indirizzo e-mail del destinatario
014807180126     d   §CM1dst     e                     inz('monica.beghelli@brt.it')
014808180126      //   ·§CM1tips = Tipo lettera via e-mail:
014809180126      //    "LET" = testo allegato in corpo con logo
014810180126      //            (richiede righe libere iniziali per il logo)
014811180126      //    "COR" = testo integrato senza logo
014812180126      //            (non consente né UNDERLINE né HIGHLIGHT)
014813180126     d   §CM1tips    e                     inz('COI')
014814180126      //   ·§CM1po   = Filiale
014815180126     d   §CM1po      e                     inz('046')
014816180126      //   ·§CM1var  = Oggetto e-mail
014817180126     d   §CM1var     e                     inz('*OBJM*+
014818180126     d                                     Avviso ritorno POD Image')
014819180126      //   ·§CM1sts  = Stato
014820180126     d   §CM1sts     e                     inz(*off)
014821180126      //   ·§CM1idp  = Id processo
014822180126     d   §CM1idp     e                     inz('2')
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
016601180126     d cmd             s            512a   varying
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
017700141201     d P01opz3         s              1    inz('R')
017701180126     d wragksc         s                   like(V1Dksc)
017800090127
017801180126      // - Campi di stampa
017802180126     d o_testo         s            198    inz
017803180126
017900090127      //---------------------------------------------------------------
018000090127      //?Definizione procedure usate.
018100090127      //---------------------------------------------------------------
018200180126      /copy gaitrasrc/srcprotopr,SYSTEM
018300090127      // - Reperimento dati utente
018400090316      /copy gaitrasrc/srcProtoPR,TIBS34R
018500090127
018600090316       // - Ricerca/Controllo tabelle
018700090316      /copy gaitrasrc/srcProtoPR,TIBS02R
018800090316
018900090316       // - Controllo/Decodifica cliente
019000090316      /copy gaitrasrc/srcProtoPR,TIBS69R
019100090127
019200090127      // - Visualizzazione tab. "LAC"
019300090127     d tntb46r         pr                  extpgm('TNTB46R')
019400090127     d  kpjba                              likeds(KPJBA)
019500090429
019600090429       // - Interrogazione tabella LAC
019700090429     d tntb461r        pr                  extpgm('TNTB461R')
019800090429     d  kpjba                              likeds(KPJBA)
019900141201     d  P01opz3                       1a
020000090127
020100090127      // - Controllo ed inversione date
020200090127     d xsrda8          pr                  extpgm('XSRDA8')
020300090127     d  wlbdat                             likeds(WLBdat)
020400110211
020500110211      // - Personalizzazione lavoro batch
020600110211     d bch09           pr                  extpgm('BCH09')
020700110211     d  kpjba                              likeds(KPJBA)
020800090127
020900090127      // - Sottomissione lavoro batch
021000090127     d bch10           pr                  extpgm('BCH10')
021100090127     d  kpjba                              likeds(KPJBA)
021200090127
021300090127      // - Richiamo diretto lavoro batch
021400090127     d tnsb16r         pr                  extpgm('TNSB16R')
021500090127     d  kpjba                              likeds(KPJBA)
021600131112
021700131112      // - Vis. Log Elab
021800131112     d TNSB04R         pr                  extpgm('TNSB04R')
021900131112     d  kpjba                              likeds(KPJBA)
022000090127
022100090127      //---------------------------------------------------------------
022200090127      //?Definizione key-list.
022300090127      //---------------------------------------------------------------
022400090127
022600071210      //---------------------------------------------------------------
022700071210      //?Riepilogo indicatori.
022800071210      //---------------------------------------------------------------
022900071210      //  28    - Emissione messaggio di errore a video
023000071210      //  40    - Codice cliente errato
023100071211      //  41    - Directory per immagini errata
023200071211      //  42    - Data consegna DAL errata o range errato
023300071211      //  43    - Data consegna AL  errata
023400071211      //  44    - Data spedizione DAL errata o range errato
023500071211      //  45    - Data spedizione AL  errata o range errato
023600071210      //  90    - Generico di errore
023700071210      //---------------------------------------------------------------
023800040720
023900090127      //---------------------------------------------------------------
024000090127      //?M A I N - L I N E.
024100090127      //---------------------------------------------------------------
024200090127
024300040720     c     *Entry        plist
024400040720     c                   parm                    KPJBA
024500071210
024800071210       // Operazioni iniziali
024900071210       exsr RoutInz;
025000071210
025100071210       // Gestione video
025200071210       DOW $Fine = *off;
025300071210         select;
025400071210           when $Video = 'D1';
025500071210             exsr GesD01;
025600071210           when $Video = 'D2';
025700071210             exsr GesD02;
025800071210           when $Video = *zero;
025900071210             exsr SbmJOB;
025901180126           //se non è un'estrazione SPOT via FTP devo inviare la mail a ced x avvisare
025902180126           //che c'è da masterizzare un DVD
025903180126             IF  %scan('/PI':V1Cdir) = 0;
025904180126               exsr Mail;
025905180126             ENDIF;
026000071210           other;
026100071210             leave;
026200071210         endsl;
026300071210       ENDDO;
026400071210
026500071210       // Operazioni finali
026600071210       exsr RoutEnd;
026700071210
026800071211       //--------------------------------------------------------------
026900071210       //?Operazioni iniziali.
027000071211       //--------------------------------------------------------------
027100071210       BEGSR RoutInz;
027200071210
027300071210       // Reperimento dati job
027400071210         exsr DatiJob;
027500110211
027600110211       //?Se utente EDPMB visualizzo F11
027700110211         SeSonoIo = (KNMUS = 'EDPMB');
027800071210
027900071210       // Impostazione nome programma a video
028000071210         V1Tpgm = SDSpgm;
028100071210
028200071210       // Impostazione campo LACTIM = aaaa/mm/gg+hh:mm:ss
028300071210         wTimeHMS = %time();
028400071211         dsTIME_ymd = %dec(wDateISO : *iso);
028500071211         dsTIME_hms = %dec(wTimeHMS : *hms);
028600100506
028700100506         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028800071210
028900071210       ENDSR;
029000071210
029100071211       //--------------------------------------------------------------
029200071210       //?Reperimento Dati del job (Utente/Operativi).
029300071211       //--------------------------------------------------------------
029400071210       BEGSR DatiJob;
029500071210
029600071210         in(E) §AzUte;
029700071210         if NOT %error;
029800071210           in(E) §DatiUte;
029900071210         endif;
030000071210         if %error or RSut = *blanks;
030100071210           clear TIBS34ds;
030200071210           tibs34r(tibs34ds);
030300071210           in §AzUte;
030400071210           in §DatiUte;
030500071210         endif;
030600071210
030700071210       ENDSR;
030800071210
030900071211       //--------------------------------------------------------------
031000071210       //?Gestione videata D01
031100071211       //--------------------------------------------------------------
031200071210       BEGSR GesD01;
031300071210
031400071210       // Inizializzazione videata
031500071210         if $InzD01 = *on;
031600071210           exsr InzD01;
031700071210           $InzD01 = *off;
031800071210         endif;
031900071210
032000071210       // Emissione videata
032100130913         write SB15T01;
032200071210         exfmt SB15D01;
032300071210         errMessage  = *off;
032400071210         errGenerico = *off;
032500071210         clear V1Dmsg;
032600071210
032700071210         select;
032800071210       // F3=Fine
032900071210           when *inKC;
033000071210             exsr F03D01;
033100071210       // Enter
033200071210           other;
033300071210             exsr CtrD01;
033400071210             if errGenerico;
033500071210               leavesr;
033600071210             endif;
033700071210         endsl;
033800071210
033900071210       ENDSR;
034000071210
034100071211       //--------------------------------------------------------------
034200071210       //?Inizializzazione videata D01
034300071211       //--------------------------------------------------------------
034400071210       BEGSR InzD01;
034500071210
034600071210         clear SB15D01;
034700071210
034800071210       ENDSR;
034900071210
035000071211       //--------------------------------------------------------------
035100071210       //?Gestione tasto funzionale F3 da videata D01
035200071211       //--------------------------------------------------------------
035300071210       BEGSR F03D01;
035400071210
035500071210       // Chiude il programma
035600071210         $Fine = *on;
035700071210
035800071210       ENDSR;
035900071210
036000071211       //--------------------------------------------------------------
036100071210       //?Controllo dati immessi in videata D01
036200071211       //--------------------------------------------------------------
036300071210       BEGSR CtrD01;
036400071210
036500110211         //IndDspF = *zeros;
036600110211
036700110211         WindDspF = IndDspF;
036800110211         reset WindDspFb;
036900110211         IndDspF  = WindDspF;
037000071211
037100071211       //? CONTROLLO PARZIALIZZAZIONI EFFETTUATE ?
037200090128       //? (UNICA SELEZIONE: PER CLIENTE)        ?
037300071211
037400090128         SELECT;
037500090128
037600071211       // - Nessuna parzializzazione richiesta
037700090128           WHEN  V1Cksc = *zero   or   V1Cksc = *blank;
037800071211             errMessage  = *on;
037900071211             errGenerico = *on;
038000071211             PosCurKsc   = *on;
038100071211             V1Dmsg = $Msg(04);
038200071211             leavesr;
038300071211
038400090128       // Ricerca su codice cliente
038500090128           WHEN  %scan('?' : V1Cksc) > *zeros;
038600090429             clear  Param01;
038700090429             P01ord = '0';
038800090429             KPJBU  = Param01;
038900141201             tntb461r (KPJBA:P01opz3);
039000090429             Param01 = KPJBU;
039100090429             if  P01ke1 <> *zero;
039200090429               V1Cksc = P01ke1;
039300180126               clear TIBS02ds;
039301180126               T02mod = 'C';
039302180126               T02cod = 'LAC';
039400090429               T02sif = KNSIF;
039500090429               T02ke1 = V1Cksc;
039600090429               TNTBE_RicercaControllo(kpjba : tibs02ds);
039700090429               if T02err = *blanks;
039800090429                 dLAC   = T02uni;
039900090429               else;
040000090429                 clear dlac;
040100090429                 errMessage  = *on;
040200090429                 PosCurKsc   = *on;
040300090429                 V1Dmsg = T02msg;
040400090429               endif;
040500090429               errGenerico = *on;
040600090429               leavesr;
040700090128             endif;
040800130913             errGenerico = *on;
040900130913             leavesr;
041000090128
041100090128       // Controllo immissione codice cliente
041200090128           WHEN  V1Cksc = *blanks  or  V1Cksc = *zeros;
041300090128             errMessage  = *on;
041400090128             errGenerico = *on;
041500090128             PosCurKsc   = *on;
041600090128             V1Dmsg = $Msg(01);
041700090128             leavesr;
041800090128
041900090128       // Controllo immissione solo caratteri numerici
042000090128           WHEN  %check(DigitN : V1Cksc) > *zeros;
042100090128             errMessage  = *on;
042200090128             errGenerico = *on;
042300090128             PosCurksc   = *on;
042400090128             V1Dmsg = $Msg(02);
042500090128             leavesr;
042600090128
042700090128         ENDSL;
042800071211
042900090128       // - Verifica esistenza codice cliente in tabella "LAC"
043000180126         clear TIBS02ds;
043001180126         T02mod = 'C';
043002180126         T02cod = 'LAC';
043100090128         T02sif = KNSIF;
043200090128         T02ke1 = V1Cksc;
043300090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
043400090128         if T02err = *blanks;
043500090128           V1Cksc = %subst(T02ke1 : 1 : %len(V1Cksc));
043600090128           dLAC   = T02uni;
043700090128           V1Dksc = §LACrag;
043800090128         else;
043900090128           errMessage  = *on;
044000090128           errGenerico = *on;
044100090128           PosCurKsc   = *on;
044200090128           V1Dmsg = $Msg(03);
044300090128           leavesr;
044400090128         endif;
044500071211
044600071211       //? IMPOSTAZIONE VIDEATA SUCCESSIVA ?
044700071211
044800090128         $Video  = 'D2';
044900090128         $InzD02 = *on;
045000071210
045100071210       ENDSR;
045200071210
045300071211       //--------------------------------------------------------------
045400071210       //?Gestione videata D02
045500071211       //--------------------------------------------------------------
045600071210       BEGSR GesD02;
045700071210
045800071210       // Inizializzazione videata
045900071210         if $InzD02 = *on;
046000071210           exsr InzD02;
046100071210           $InzD02 = *off;
046200071210         endif;
046300071210
046400071210       // Emissione testata
046500071210         if  NOT  errMessage;
046600071210           write SB15T01;
046700071210         endif;
046800071210
046900071210       // Emissione videata
047000071210         exfmt SB15D02;
047100071210         errMessage  = *off;
047200071210         errGenerico = *off;
047300071210         clear V1Dmsg;
047400071210
047500071210         select;
047600131112       // F1=Vis. Log Elab
047700131112           when *inKA;
047800131112             TNSB04R(kpjba);
047900131112       // F3=Fine
048000131112           when *inKC;
048100131112             exsr F03D01;
048200071217       // F9=Visualizzazione tab. LAC
048300071217           when *inKI;
048400071217             exsr F09D02;
048500110211       // F11=Personalizzazione lancio
048600110211           when *inKK;
048700110211             kritb = '1';
048800110211             BCH09(kpjba);
048900071210       // F12=Ritorno
049000071210           when *inKL;
049100071210             exsr F12D02;
049200071210       // Controllo videata
049300071210           other;
049400071210             exsr CtrD02;
049500071210             select;
049600071210         // Rilevato errore
049700071210               when errGenerico;
049800071210                 leavesr;
049900071210         // F6-Conferma
050000071210               when *inKF;
050100071210                 $Video = *zeros;
050200071210             endsl;
050300071210         endsl;
050400071210
050500071210       ENDSR;
050600071210
050700071211       //--------------------------------------------------------------
050800071210       //?Inizializzazione videata D02
050900071211       //--------------------------------------------------------------
051000071210       BEGSR InzD02;
051100071210
051200071210         clear SB15D02;
051300071210
051400071211         V2Cksc = %dec(V1Cksc : 7 : 0);
051500131030         PR_V2CKsc = '1';
051600071210         V2Dksc = V1Dksc;
051700131030         Save_V2CKSC = V2CKSC;
051800071210
051900071210         V1Cfmi   = §LACfmi;
052000071210         V1Cdir   = §LACdir;
052100090310         V1Ctpt   = §LACtpt;
052200090310         V1Cssr   = §LACssr;
052300090310         V1Clnp   = §LAClnp;
052400090310         V1Cres   = §LACres;
052500090310         V1Crec   = §LACrec;
052600090310         V1Ccsr   = §LACcsr;
052700090310         V1Ctad   = §LACtad;
052800090310       // se frequenza diversa da 'I' o 'J' la propongo vuota
052900090310         if §lactadu = 'I' or §lactadu = 'J';
053000090316           V1tadu  = §LACtadu;
053100090310         else;
053200090316           clear v1tadu;
053300090310         endif;
053400090310         V1Ckscf  = §LACksc;
053500090310         V1Cctr   = §LACctr;
053600090316         V1fimp  = §LACfimp;
053700090316         V1imp   = §LACimp;
053800090310         V1Dnote  = §LACnote;
053900110211
054000110211       //?Abilito F11
054100110211         F11Attivo = SeSonoIo;
054200071210
054300071210       ENDSR;
054400071217
054500071217       //--------------------------------------------------------------
054600071217       //?Gestione tasto funzionale F9 da videata D02
054700071217       //--------------------------------------------------------------
054800071217       BEGSR F09D02;
054900071217
055000071217       // Visualizzazione tab. LAC
055100071217         reset TNTB46ds;
055200071217         B46ke1 = %trim( %editw( V2Cksc : '0       ' ) );
055300071217
055400071217         kpjbu = TNTB46ds;
055500071217         tntb46r(kpjba);
055600071217         TNTB46ds = kpjbu;
055700071217
055800071217         select;
055900071217         // Ritorno con errore
056000071217           when B46err = *on;
056100071217             V1Dmsg  = B46msg;
056200071217             errMessage  = *on;
056300071217             errGenerico = *on;
056400071217         // Ritorno con F3
056500071217           when B46fxx = '1';
056600071217             $Fine   = *on;
056700071217             errGenerico = *on;
056800071217         endsl;
056900071217
057000071217       ENDSR;
057100071210
057200071211       //--------------------------------------------------------------
057300071210       //?Gestione tasto funzionale F12 da videata D02
057400071211       //--------------------------------------------------------------
057500071210       BEGSR F12D02;
057600071210
057700071210       // Ritorno alla videata precedente (D01)
057800071210         $Video = 'D1';
057900071210
058000071210       ENDSR;
058100071210
058200071211       //--------------------------------------------------------------
058300071210       //?Controllo dati immessi in videata D02
058400071211       //--------------------------------------------------------------
058500071210       BEGSR CtrD02;
058600071210
058700110211         //IndDspF = *zeros;
058800110211
058900110211         WindDspF = IndDspF;
059000110211         reset WindDspFb;
059100110211         IndDspF  = WindDspF;
059200131030
059300140707       // il codice non può essere 0
059400140707         if V2CKSC = 0;
059500140707           errMessage  = *on;
059600140707           errGenerico = *on;
059700140707           PosCurKsc   = *on;
059800140707           V1Dmsg = $Msg(01);
059900140707           leavesr;
060000140707         endif;
060100140707
060200131030       // se il codice è stato valorizzato con un dato diverso dal precedente,
060300131030       // reperisco i valori della tab.LAC
060400131030         if V2CKSC <> Save_V2CKSC and V2CKSC <> 0;
060500180126           clear TIBS02ds;
060501180126           T02mod = 'C';
060502180126           T02cod = 'LAC';
060600131030           T02sif = KNSIF;
060700131030           T02ke1 = %editc(V2Cksc:'X');
060800131030           TNTBE_RicercaControllo(kpjba : tibs02ds);
060900131030           if T02err = *blanks;
061000131030             dLAC   = T02uni;
061100131030             V2Dksc = §LACrag;
061200131030             Save_V2CKSC = V2CKSC;
061300131030           else;
061400131030             errMessage  = *on;
061500131030             errGenerico = *on;
061600131030             PosCurKsc   = *on;
061700131030             V1Dmsg = $Msg(03);
061800131030             leavesr;
061900131030           endif;
062000131030         endif;
062100071210
062200071210       // RE-impostazione dati di default - da tab.LAC (se tolti)
062300090310         if  V1Ctpt =  *blank;
062400090310           V1Ctpt   =  §LACtpt;
062500090310         endif;
062600090316         if  V1Cssr =  *blank;
062700090310           V1Cssr   =   §LACssr;
062800090310         endif;
062900090310         if  V1Cres =  *blank;
063000090310           V1Cres   =  §LACres;
063100090310         endif;
063200090310         if  V1Crec =  *blank;
063300090310           V1Crec   =  §LACrec;
063400090310         endif;
063500090310         if  V1Ccsr =  *blank;
063600090310           V1Ccsr   =  §LACcsr;
063700090310         endif;
063800090310         if  V1Ctad =  *blank;
063900090310           V1Ctad   =  §LACtad;
064000090310         endif;
064100071210         if  V1Cfmi =  *blank;
064200071210           V1Cfmi   =  §LACfmi;
064300071210         endif;
064400071210         if  V1Cdir =  *blank;
064500071210           V1Cdir   =  §LACdir;
064600071210         endif;
064700090311
064800090311         //?Flag nome immagine?
064900090311         clear  V1Dfmi;
065000090311         select;
065100090311           // - Obbligatorietà
065200090311           when  V1Cfmi = *blank;
065300090311             errMessage  = *on;
065400090311             errGenerico = *on;
065500090311             PosCurFmi   = *on;
065600090311             V1Dmsg = 'Flag nome immagine obbligatorio';
065700090311             leavesr;
065800090311           // - Ricerca
065900090311           when  %scan('?' : V1Cfmi) > *zero;
066000090311             clear  TIBS02ds;
066100090311             T02mod = 'R';
066200090311             T02cod = 'NIM';
066300090311             T02sif = KNSIF;
066400090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
066500090311             if  T02err = *blank;
066600090311               V1Cfmi = %subst(T02ke1 : 1 : %len(V1Cfmi));
066700090311               V1Dfmi = T02uni;
066800090311               errGenerico = *on;
066900090311               leavesr;
067000090311             else;
067100090311               errMessage  = *on;
067200090311               errGenerico = *on;
067300090311               PosCurFmi   = *on;
067400090311               V1Dmsg = T02msg;
067500090311               leavesr;
067600090311             endif;
067700090311           // - Controllo
067800090311           other;
067900180126             clear  TIBS02ds;
067901180126             T02mod = 'C';
068000090311             T02cod = 'NIM';
068100090311             T02ke1 = V1Cfmi;
068200090311             TNTBE_RicercaControllo(kpjba : tibs02ds);
068300090311             if  T02err <> *blank;
068400090311               errMessage  = *on;
068500090311               errGenerico = *on;
068600090311               PosCurFmi   = *on;
068700090311               V1Dmsg = 'Flag nome immagine errato';
068800090311               leavesr;
068900090311             endif;
069000090311             V1Dfmi = T02uni;
069100090311         ENDSL;
069200090311
069300090311         //?Directory per immagini?
069400090311         select;
069500090311           when  V1Cdir  = *blank;
069600090311             errMessage  = *on;
069700090311             errGenerico = *on;
069800090311             PosCurDir   = *on;
069900090311             V1Dmsg = $Msg(14);
070000090311             leavesr;
070100090311           when  %scan('\':V1Cdir) > *zero;
070200090311             errMessage  = *on;
070300090311             errGenerico = *on;
070400090311             PosCurDir   = *on;
070500090311             V1Dmsg = $Msg(15);
070600090311             leavesr;
070700071210
070800071210       // Controllo immissione di caratteri NON previsti nella directory
070900090316           when  %scan('&' : V1Cdir) > *zero  or
071000071211             %scan('%' : V1Cdir) > *zero;
071100090316             errMessage  = *on;
071200090316             errGenerico = *on;
071300090316             PosCurDir   = *on;
071400090316             V1Dmsg = $Msg(09);
071500090316             leavesr;
071600090311         endsl;
071700090311
071800090311         //?Tipo addebito (Creazione TITAS)?
071900090311         select;
072000090311           // Se "N" non impostare i dati dell'addebito forzato
072100090311           when  V1Ctad = 'N'     and
072200090311                (V1Ckscf <> *zero  or  V1Cctr <> *blank
072300090311                                   or  V1imp <> *zero);
072400090311             errMessage  = *on;
072500090311             errGenerico = *on;
072600090311             PosCurTad   = *on;
072700090311             V1Dmsg = 'Se NO addebito non impostare le forzature';
072800090311             leavesr;
072900090311         endsl;
073000090311
073100090311         //?Codice cliente?
073200090311         select;
073300090311           // - Non ammessa tariffa senza cliente
073400090311           when  V1Ckscf =  *zero   and   V1Cctr <> *blank;
073500090311               errMessage  = *on;
073600090311               errGenerico = *on;
073700090311               PosCurKscf  = *on;
073800090311               V1Dmsg = 'Cliente tassazione obbligatorio SE +
073900090311                         inserita la relativa tariffa';
074000090311               leavesr;
074100090311           // - Controllo cliente tassazione
074200090311           when  V1Ckscf <> *zero;
074300090311             clear  TIBS69ds;
074400090311             tibs69ds.I69kcc = DUTkci;
074500090311             tibs69ds.I69kac = %int(V1Ckscf);
074600090311             tibs69ds.I69sif = knsif;
074700090311             tibs69r(TIBS69ds : ds_CNACO :
074800090311                                ds_CNIND :
074900090311                                ds_CNCLP :
075000090311                                ds_FNCLS);
075100090311             if  tibs69ds.O69err = *on;
075200090311               errMessage  = *on;
075300090311               errGenerico = *on;
075400090311               PosCurKscf  = *on;
075500090311               V1Dmsg = 'Cliente tassazione errato';
075600090311               leavesr;
075700090311             endif;
075800090311             if  V1Cctr = *blank;
075900090311               errMessage  = *on;
076000090311               errGenerico = *on;
076100090311               PosCurCtr   = *on;
076200090311               V1Dmsg = 'Inserire il codice tariffa';
076300090311               leavesr;
076400090311             endif;
076500090311         endsl;
076600090311
076700090311         //?Importo & Tipo importo forzato?
076800090311         select;
076900090311           // - Non ammesso importo per Varia Negata
077000090311           when  V1Ctad = 'V'   and   V1imp <> *zero;
077100090311             errMessage  = *on;
077200090311             errGenerico = *on;
077300090311             PosCurImp   = *on;
077400090311             V1Dmsg = 'NON ammesso l''importo forzato per +
077500090311                       Varia Negata';
077600090311             leavesr;
077700090311           // - Non ammesso tipo importo senza importo
077800090311           when  V1imp =  *zero   and   V1fimp <> *blank;
077900090311             errMessage  = *on;
078000090311             errGenerico = *on;
078100090311             PosCurImp   = *on;
078200090311             V1Dmsg = 'Importo forzato obbligatorio SE +
078300090311                       inserito il tipo di importo';
078400090311             leavesr;
078500090311           // - Se inserito importo devono essere inseriti il ksc il ctr
078600090311           //   e il tipo importo
078700090311           when  V1imp <> *zero;
078800090311             select;
078900090311               when  V1fimp  = *blank;
079000090311                 errMessage  = *on;
079100090311                 errGenerico = *on;
079200090311                 PosCurFimp  = *on;
079300090311                 V1Dmsg = 'Immettere il tipo importo';
079400090311                 leavesr;
079500090311               when  V1Ckscf = *zero;
079600090311                 errMessage  = *on;
079700090311                 errGenerico = *on;
079800090311                 PosCurKscf  = *on;
079900090311                 V1Dmsg = 'Immettere il cliente forzato';
080000090311                 leavesr;
080100100204               when  V1Cctr  = *blank;
080200090311                 errMessage  = *on;
080300090311                 errGenerico = *on;
080400090311                 PosCurCtr   = *on;
080500090311                 V1Dmsg = 'Immettere la tariffa forzata';
080600090311                 leavesr;
080700090311             endsl;
080800090311         endsl;
080900071210
081000090311         //?Controllo parzializzazioni per data
081100090128         reset $Parz01a;
081200090128         reset $Parz01b;
081300071210         if  V1Cdcd <> *zero   or  V1Cdca <> *zero;
081400071210           $Parz01a =  *on;
081500071210         endif;
081600071210         if  V1Cdsd <> *zero   or  V1Cdsa <> *zero;
081700071210           $Parz01b =  *on;
081800071210         endif;
081900071210
082000071210         select;
082100071210       // Richiesta nessuna parzializzazione per data
082200071210           when  $Parz01a = *off  and  $Parz01b = *off;
082300071210             errMessage   = *on;
082400071210             errGenerico  = *on;
082500071210             PosCurDcd    = *on;
082600071211             V1Dmsg =  $Msg(10);
082700071210             leavesr;
082800071210       // Richieste troppe  parzializzazioni per data
082900071210           when  $Parz01a = *on  and  $Parz01b = *on;
083000071210             errMessage   = *on;
083100071210             errGenerico  = *on;
083200071210             PosCurDcd    = *on;
083300071211             V1Dmsg = $Msg(11);
083400071210             leavesr;
083500071210       // Parzializzazione per data consegna dal/al
083600071210           when  $Parz01a = *on;
083700071210         // - controllo data iniziale
083800071210             clear WLBdat;
083900071210             G08dat = V1Cdcd;
084000071210             xsrda8(wlbdat);
084100071210             if G08err = *off;
084200071210               V1Cdcd  = G08dat;
084300071210               W1Cdcd  = G08inv;
084400071210             else;
084500071210               errMessage  = *on;
084600071210               errGenerico = *on;
084700071210               PosCurDcd   = *on;
084800071211               V1Dmsg  = $Msg(12);
084900071210               leavesr;
085000071210             endif;
085100071210         // - controllo data finale
085200071210             clear WLBdat;
085300071210             G08dat = V1Cdca;
085400071210             xsrda8(wlbdat);
085500071210             if G08err = *off;
085600071210               V1Cdca  = G08dat;
085700071210               W1Cdca  = G08inv;
085800071210             else;
085900071210               errMessage  = *on;
086000071210               errGenerico = *on;
086100071210               PosCurDca   = *on;
086200071211               V1Dmsg  = $Msg(12);
086300071210               leavesr;
086400071210             endif;
086500071210         // - controllo range
086600071210             if W1Cdcd > W1Cdca;
086700071210               errMessage  = *on;
086800071210               errGenerico = *on;
086900071210               PosCurDcd   = *on;
087000071211               V1Dmsg  = $Msg(13);
087100071210               leavesr;
087200071210             endif;
087300071210
087400071210       // parzializzazione per data spedizione dal/al
087500090128           when  $Parz01b = *on;
087600071210         // - controllo data iniziale
087700071210             clear WLBdat;
087800071210             G08dat = V1Cdsd;
087900071210             xsrda8(wlbdat);
088000071210             if G08err = *off;
088100071210               V1Cdsd  = G08dat;
088200071210               W1Cdsd  = G08inv;
088300071210             else;
088400071210               errMessage  = *on;
088500071210               errGenerico = *on;
088600071210               PosCurDsd   = *on;
088700071211               V1Dmsg  = $Msg(12);
088800071210               leavesr;
088900071210             endif;
089000071210         // - controllo data finale
089100071210             clear WLBdat;
089200071210             G08dat = V1Cdsa;
089300071210             xsrda8(wlbdat);
089400071210             if G08err = *off;
089500071210               V1Cdsa  = G08dat;
089600071210               W1Cdsa  = G08inv;
089700071210             else;
089800071210               errMessage  = *on;
089900071210               errGenerico = *on;
090000071210               PosCurDsa   = *on;
090100071211               V1Dmsg  = $Msg(12);
090200071210               leavesr;
090300071210             endif;
090400071210         // - controllo range
090500071210             if W1Cdsd > W1Cdsa;
090600071210               errMessage  = *on;
090700071210               errGenerico = *on;
090800071210               PosCurDsd   = *on;
090900071211               V1Dmsg  = $Msg(13);
091000071210               leavesr;
091100071210             endif;
091200071210
091300071210         endsl;
091400071210
091500071210       ENDSR;
091600071211
091700071211       //--------------------------------------------------------------
091800071211       //?Sottomissione lavoro batch
091900071211       //--------------------------------------------------------------
092000071211       BEGSR SbmJOB;
092100071211
092200071211       // Impostazione parametri
092300071211         reset TNSB16ds;
092400071211         D16dcd = W1Cdcd;
092500071211         D16dca = W1Cdca;
092600071211         D16dsd = W1Cdsd;
092700071211         D16dsa = W1Cdsa;
092800071211         D16ksc = V2Cksc;
092900071211         D16imm = V1Cimm;
093000090310         D16tad = V1Ctad;
093100071211         D16dir = V1Cdir;
093200071211         D16fmi = V1Cfmi;
093300090310         d16ksu = v2cksc;
093400090316         d16tadu = v1tadu;
093500090316         d16fimp = v1fimp;
093600090316         d16imp  = v1imp;
093700090310         d16kscf = v1ckscf;
093800090310         d16ctr  = v1cctr;
093900090310       // ID Job
094000100506       // - imposto ksc + oggi + progressivo
094100100506         d16job = %editc(v2cksc:'X') + %editc(dstime_ymd:'X') + '0';
094200100506       // - controllo, capita spesso che si lanci lo stesso cliente + volte nello stesso giorno
094300100506         exsr ctridjob;
094400090310
094500090310         d16tpt = v1ctpt;
094600090310         d16res = v1cres;
094700090310         d16rec = v1crec;
094800090310         d16csr = v1ccsr;
094900090310         d16ssr = v1cssr;
095000090310         d16lnp = v1clnp;
095100071211
095200071211         kcoaz = 'SB16';
095300071211         kpjbu = TNSB16ds;
095400071211         if  knmus = *all'1';
095500071211           tnsb16r(kpjba);
095600071211         else;
095700071211           BCH10(kpjba);
095800071211         endif;
095900090327
096000090327       // scrivo rcd spia su tilac con lacela = '33'
096100090327            clear tilac000;
096200090327            lactim = %char(%timestamp:*iso0);
096300090327            lacdir = d16dir;
096400090327            lacela = '33';
096500090327            lactela = 'S';
096600090327            lacksu = d16ksu;
096700090402            lactad = d16tad;
096800090327            lacidjob = d16job;
096900090327            write tilac000;
096901180126
096902180126         wragksc = V2dksc;
097000071211
097100131030       // Non esco dal pgm ma resto nella videata di lancio pulendo il codice cliente
097200131030         $Fine = *off;
097300131030         $Video = 'D2';
097400131030         V2CKsc = 0;
097500140707         V2DKsc = *blank;
097600131030         PR_V2CKsc = '0';
097700071211
097800071211       ENDSR;
097900100506
098000100506       //--------------------------------------------------------------
098100100506       //?Controllo ID job deve essere univoco.
098200100506       //--------------------------------------------------------------
098300100506       BEGSR CtrIdJob;
098400100506
098500100506         $FineLAC = *off;
098600100506         widjob15 = %subst(d16job:1:15);
098700100506
098800100506         exec sql
098900100506          DECLARE LAC cursor for
099000100506          SELECT   lacidjob from tilac00f
099100100506          WHERE    substr(lacidjob, 1, 15) = :widjob15
099200100506                   and lacela = '33'
099300100506          ORDER BY lacidjob;
099400100506
099500100506          exec sql
099600100506           OPEN LAC;
099700100506
099800100506          DOW not $FineLAC;
099900100506            exec sql
100000100506             FETCH next from LAC into: widjob;
100100100506             IF sqlcod = 100 or sqlcod < 0;
100200100506               $FineLAC = *on;
100300100506               leave;
100400100506             ENDIF;
100500100506
100600100506            wprogr = %dec(%subst(d16job:16:1):1:0);
100700100506            wprogr += 1;
100800100506            %subst(d16job:16:1) = %editc(wprogr:'X');
100900100506          ENDDO;
101000100506
101100100506        exec sql
101200100506         CLOSE LAC;
101300100506
101400100506       ENDSR;
101401180126
101500180126       //--------------------------------------------------------------
101501180126       //?Invio mail a CED
101502180126       //--------------------------------------------------------------
101503180126       BEGSR Mail;
101510180126
101512180126       // Reperimento tab. "MRA"
101513180126         clear dMRAdan;
101514180126         clear kpjbu;
101515180126         clear TIBS02DS;
101516180126         T02mod = 'C';
101517180126         T02cod = 'MRA';
101518180126         T02ke1 = 'TNSB03R';
101519180126         T02sif = knsif;
101520180126         TNTBE_RicercaControllo (kpjba:tibs02ds);
101521180126         IF  T02err = *blanks;
101522180126           dMRAdan = T02uni;
101523180126         ENDIF;
101524180126
101544180126       // Preparo i dati per nuova mail
101545180126         IF  not %open(PRTEMAIL);
101546180126           exsr Override;
101547180126           exsr OpenPrtF;
101548180126         ENDIF;
101551180126
101552180126       // preparo il testo della mail
101553180126         clear o_testo;
101554180126       // cliente
101555180126         %subst(o_testo:10:7) = %editc(D16ksu:'X');
101556180126       // ragione sociale
101557180126         %subst(o_testo:19:25) = wragksc;
101560180126       // id job se presente
101561180126         %subst(o_testo:47:7) = D16job;
101562180126       // periodo estrazione
101567180126         IF  D16dcd <> *zeros;
101568180126           %subst(o_testo:63:21) = %subst(%editc(D16dcd:'X'):7:2) + '/' +
101569180126                                   %subst(%editc(D16dcd:'X'):5:2) + '/' +
101570180126                                   %subst(%editc(D16dcd:'X'):1:4) + ' ' +
101571180126                                   %subst(%editc(D16dca:'X'):7:2) + '/' +
101572180126                                   %subst(%editc(D16dca:'X'):5:2) + '/' +
101573180126                                   %subst(%editc(D16dca:'X'):1:4);
101574180126         ENDIF;
101575180126         IF  D16dsd <> *zeros;
101576180126           %subst(o_testo:63:21) = %subst(%editc(D16dsd:'X'):7:2) + '/' +
101577180126                                   %subst(%editc(D16dsd:'X'):5:2) + '/' +
101578180126                                   %subst(%editc(D16dsd:'X'):1:4) + ' ' +
101579180126                                   %subst(%editc(D16dsa:'X'):7:2) + '/' +
101580180126                                   %subst(%editc(D16dsa:'X'):5:2) + '/' +
101581180126                                   %subst(%editc(D16dsa:'X'):1:4);
101582180126         ENDIF;
101585180126       // directory delle immagini
101586180126         %subst(o_testo:86:30) = D16dir;
101587180126         except PRTdet;
101589180126
101590180126       // Chiudo il prtf
101592180126         exsr ClosePrtF;
101594180126
101595180126       ENDSR;
101596180126
101597180126       //--------------------------------------------------------------
101598180126       //?Preparazione inizale della mail
101599180126       //--------------------------------------------------------------
101600180126       BEGSR OpenPrtF;
101601180126
101602180126         open PRTEMAIL;
101603180126
101604180126       // Stampa una testata se NON è richiesta la e-mail
101605180126         IF  §MRAdreg = *blanks;
101606180126           o_testo = JobUser + ' - ' + SDSpgm
101607180126                     + ' - ' + %editc( *date : 'Y' )
101608180126                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
101609180126           except PRTdet;
101610180126           clear o_testo;
101611180126           except PRTdet;
101612180126           except PRTdet;
101613180126         ENDIF;
101614180126
101615180126       // Stampa testo iniziale
101616180126         o_testo = 'Elenco clienti per i quali è stato +
101617180126                    preparato ed inviato via posta interna il DVD dei +
101618180126                    Pod Image';
101619180126         except PRTdet;
101620180126
101621180126       // Stampa una riga vuota
101622180126         clear o_testo;
101623180126         except PRTdet;
101624180126
101625180126       // Stampa intestazione
101626180126         o_testo = '         Cliente  Ragione Sociale             Id Job' +
101627180126                     '          Periodo Estrazione     Directory';
101628180126         except PRTdet;
101629180126
101630180126       // Stampa una riga vuota
101631180126         clear o_testo;
101632180126         except PRTdet;
101633180126
101634180126       ENDSR;
101635180126
101636180126       //--------------------------------------------------------------
101637180126       //?Chiusura del file di stampa quindi invio mail
101638180126       //--------------------------------------------------------------
101639180126       BEGSR ClosePrtF;
101640180126
101641180126       //?Chiusura dello spool?
101642180126         IF  %open(PrtEMAIL);
101643180126           clear o_testo;
101644180126           except PRTdet;
101645180126           o_testo = 'Vi ricordiamo che il controllo del contenuto dei +
101646180126                      supporti prima che siano inviati ai clienti';
101647180126           except PRTdet;
101648180126           o_testo = 'è a vostro carico.';
101649180126           except PRTdet;
101650180126           o_testo = 'Vi chiediamo inoltre di proporre ai clienti di +
101651180126                      inviare i POD Image via FTP e non più tramite';
101652180126           except PRTdet;
101653180126           o_testo = 'supporto ottico (CD/DVD), facendo notare, ad +
101654180126                      esempio, la velocità con cui avrebbero a disposizione';
101655180126           except PRTdet;
101656180126           o_testo = 'le immagini rispetto al metodo attuale.';
101657180126           except PRTdet;
101658180126           clear o_testo;
101659180126           except PRTdet;
101660180126           o_testo = 'A disposizione per ogni chiarimento.';
101661180126           except PRTdet;
101662180126           clear o_testo;
101663180126           except PRTdet;
101664180126           o_testo = 'ICT di Sede';
101665180126           except PRTdet;
101666180126           o_testo = 'BRT SPA';
101667180126           except PRTdet;
101668180126
101669180126           close PRTEMAIL;
101670180126
101673180126         //?Eliminazione overflow?
101674180126           cmd = CmdDltOvr;
101675180126           IF  ExecuteCommand (cmd) = 0;
101676180126           ELSE;
101677180126             leavesr;
101678180126           ENDIF;
101679180126         ENDIF;
101680180126
101681180126       ENDSR;
101682180126
101683180126       //--------------------------------------------------------------
101684180126       //?Override al file di stampa per impostarvi i dati per
101685180126       //?  l'invio via e-mail   +   stampa inizio e-mail
101686180126       //--------------------------------------------------------------
101687180126       BEGSR Override;
101688180126
101689180126         reset TRTCM1DS;
101690180126
101691180126         IF  §MRAdreg <> *blanks;
101692180126           §CM1mitt = %trim(§MRAdmitt);
101693180126           §CM1dst  = %trim(§MRAddest);
101694180126           §CM1tips = §MRAdreg;
101695180126           §CM1po   = Sede;
101696180126           §CM1var  = '*OBJM*' + §MRAddes;
101697180126           §CM1idp  = §MRAdidpro;
101698180126
101699180126           cmd = CmdOvrPrtF +
101700180126                 ' outq(' + %trim(§MRAdoutqi) + ')'  +
101701180126                 ' usrdfndta(''' + TRTCM1ds + ''')';
101702180126         ELSE;
101703180126           cmd = CmdOvrPrtF;
101704180126         ENDIF;
101705180126
101706180126         IF  ExecuteCommand (cmd) = 0;
101707180126         ELSE;
101708180126           leavesr;
101709180126         ENDIF;
101710180126
101715180126       ENDSR;
101716180126
101717071211       //--------------------------------------------------------------
101718071210       //?Operazioni finali.
101800071211       //--------------------------------------------------------------
101900071210       BEGSR RoutEnd;
102000071210
102100071210         clear TIBS02ds;
102200071210         T02tla = 'C';
102300090316         TNTBE_RicercaControllo(kpjba : tibs02ds);
102400071210
102500071210         *inLR = *on;
102600071210         return;
102700071210
102800071210       ENDSR;
103100040720
103101180126      //---------------------------------------------------------------
103102180126      //?Spool di stampa (per e-mail).
103103180126      //---------------------------------------------------------------
103104180126     oPRTEMAIL  e            PRTdet      1
103105180126     o                       o_testo
103106180126
103200071211       //--------------------------------------------------------------
103300071210       //?Schiere a tempo di compilazione.
103400071211       //--------------------------------------------------------------
103500040720
103600071210**   $Msg -------------------------------------------------------------------*
103700071210Immettere il codice cliente                                                     1
103800071210Immettere SOLO caratteri numerici                                               2
103900071210Codice cliente mancante o non presente in tabella "LAC"                         3
104000071210Immettere almeno un codice cliente oppure un numero spedizione                  4
104100071210Parzializzare per codice cliente OPPURE per numero spedizione                   5
104200071210Numero spedizione errato                                                        6
104300071210Spedizione non DPD                                                              7
104400071210Il cliente della spedizione non è in tabella "LAC"                              8
104500071210Caratteri "&" e "%" non ammessi per la directory del cliente                    9
104600071211Immettere almeno la data consegna oppure la data spedizione                    10
104700071211Parzializzare per data consegna OPPURE per data spedizione                     11
104800071211Data formalmente errata                                                        12
104900071211Data "dal" maggiore di data "al"                                               13
105000090311Immettere la directory                                                         14
105100090311Carattere "\" non valido; per indicare una sottocartella utilizzare "/"        15
