000100071212      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200071212      //---------------------------------------------------------------
000300090430      //?TNSB05R - Pod Image settimanale x cliente raggr. + e-mail
000400090430      //?          schedulato settimanalmente sulla tabella SCH
000500090430      //?          dove in kpjbu c'è il cliente raggr. + data ultima
000600090430      //?          estrazione
000700090603      //?          il pgm estrae le MAI estratte
000800090603      //?          da data ultima estrazione - 30 gg calendario
000900090603      //?          a  data ultima estrazione + 7  gg calendario
001000071212      //---------------------------------------------------------------
001100071213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200090310     h dftactgrp(*no)
001300071212
001400071212      //---------------------------------------------------------------
001500071212      //?Dichiarazione file.
001600071212      //---------------------------------------------------------------
001700071219
001800090430     fTNTBE01L  uf   e           k disk
001900090327     ftilac00f  o    e             disk
002000071219
002100071219     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002200071212
002300071212      //---------------------------------------------------------------
002400071212      //?Definizione costanti.
002500071212      //---------------------------------------------------------------
002600090430     d Digits          c                   const('0123456789')
002700071219
002800071212      // - Tabella in gestione
002900071212     d C_tab           c                   const('LAC')
003000071219
003100071219      // - Dati di default
003200071227     d C_Sede          c                   const('046')
003300071219
003400071219      // - Comando di override al PrtF
003500071221     d C_CmdOvrPrtF    c                   const('OVRPRTF +
003600071221     d                                           file(PRTEMAIL) +
003700071221     d                                           pagesize(66 132) +
003800071221     d                                           lpi(6) cpi(10) +
003900071221     d                                           ovrscope(*actgrpdfn) +
004000071221     d                                           ')
004100071221     d C_CmdDltOvr     c                   const('DLTOVR +
004200071221     d                                            file(PRTEMAIL) +
004300071221     d                                            lvl(*actgrpdfn)')
004400071219
004500071219      //---------------------------------------------------------------
004600071219      //?Definizione schiere.
004700071219      //---------------------------------------------------------------
004800090319      // - schiere per stampare mail
004900090319     d skuni           s                   like(wkeysk) dim(999) inz
005000090319     d skksc           s                   like(d16ksc) dim(999) inz
005100090319     d skrag           s                   like(§lacrag) dim(999) inz
005200090319     d skaut           s                   like(§lacaut) dim(999) inz
005300090319     d skdal           s                   like(d16dcd) dim(999) inz
005400090319     d skal            s                   like(d16dca) dim(999) inz
005500090430     d skjob           s                   like(d16job) dim(999) inz
005600071212
005700071212      //---------------------------------------------------------------
005800071212      //?Definizione aree dati.
005900071212      //---------------------------------------------------------------
006000071221
006100071221      // - Dati utente
006200071221     d §AzUte        e ds                  extname(AZUTE00F)
006300071221     d                                     dtaara
006400071221     d §DatiUte      e ds                  extname(dDatiUte)
006500071221     d                                     dtaara
006600071212
006700071212      //---------------------------------------------------------------
006800071212      //?Definizione strutture dati.
006900071212      //---------------------------------------------------------------
007000071219
007100071212      // - Status
007200071212     d Psds           sds
007300071213     d   SDSpgm          *proc
007400071213     d   JobUser             254    263                                         User name
007500071221
007600071221      // Parametri
007700071221     d kpjba         e ds
007800090304
007900090304      // richiamo al pgm batch
008000090304     d TNSB16ds        ds                  inz
008100090304     d   D16dcd                       8  0 inz
008200090304     d   D16dca                       8  0 inz
008300090304     d   D16ksc                       7  0 inz
008400090304     d   D16imm                       1    inz
008500090304     d   D16tad                       1    inz
008600090304     d   D16dsd                       8  0 inz
008700090304     d   D16dsa                       8  0 inz
008800090304     d   D16dir                      30    inz
008900090304     d   D16fmi                       2    inz
009000090304     d   d16ksu                       7  0 inz
009100090304     d   d16tadu                      1    inz
009200090304     d   d16fimp                      1    inz
009300090304     d   d16imp                      10  3 inz
009400090304     d   d16kscf                      7  0 inz
009500090304     d   d16ctr                       3    inz
009600090304     d   d16job                      16    inz
009700090310     d   d16tpt                       1    inz
009800090310     d   d16res                       1    inz
009900090310     d   d16rec                       1    inz
010000090310     d   d16csr                       1    inz
010100090310     d   d16ssr                       1    inz
010200090310     d   d16lnp                       3  0 inz
010300071212
010400071212      // - Tabella "LAC" = Clienti per ritorno immagini
010500071212     d dLAC          e ds                  inz
010600071221
010700071221      // - Tabella "MRA" = Bart-Mailing - Danni
010800071221     d dMRAdan       e ds                  inz
010900071221
011000071221      // - Reperimento dati utente
011100071221     d TIBS34ds      e ds
011200071221
011300071221      // - Gestione tabelle: controllo e ricerca
011400071221     d TIBS02ds      e ds                  inz
011500071221     d   T02tla      e                     inz('L')
011600071221     d   T02mod      e                     inz('C')
011700071221     d   T02cod      e                     inz('MRA')
011800071219
011900071219      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
012000071219     d TRTCM1ds      e ds                  inz
012100071219      //   ·§CM1mitt = Indirizzo e-mail del mittente
012200071219     d   §CM1mitt    e                     inz('ced@bartolini.it')
012300071219      //   ·§CM1dst  = Indirizzo e-mail del destinatario
012400090318     d   §CM1dst     e                     inz('nicola.delledonne@bartolini.it')
012500071219      //   ·§CM1tips = Tipo lettera via e-mail:
012600071219      //    "LET" = testo allegato in corpo con logo
012700071219      //            (richiede righe libere iniziali per il logo)
012800071219      //    "COR" = testo integrato senza logo
012900071219      //            (non consente né UNDERLINE né HIGHLIGHT)
013000071219     d   §CM1tips    e                     inz('COR')
013100071219      //   ·§CM1po   = Filiale
013200071219     d   §CM1po      e                     inz('046')
013300071219      //   ·§CM1var  = Oggetto e-mail
013400071219     d   §CM1var     e                     inz('*OBJM*+
013500071219     d                                     Preparazione CD con immagin+
013600071219     d                                     i delle LdV da inviare ai c+
013700071219     d                                     lienti (v. tab. LAC)')
013800071219      //   ·§CM1sts  = Stato
013900071219     d   §CM1sts     e                     inz(*off)
014000071219      //   ·§CM1sts  = Id processo
014100071219     d   §CM1idp     e                     inz('2')
014200071218
014300071212      //---------------------------------------------------------------
014400071212      //?Definizione variabili globali.
014500071212      //---------------------------------------------------------------
014600071219
014700071212      // - Flags booleani
014800071219     d $Invio          s              1n   inz(*off)
014900071219
015000071219      // - Indici di schiera
015100090320     d xx              s              4  0 inz
015200090320     d yy              s              4  0 inz
015300071219
015400071219      // - Campi per QCMDEXC
015500071219     d Qcmd            s            512    inz
015600071219
015700071213      // - Campi di comodo
015800090430     d nrweek          s              2  0 inz
015900090320     d oldwksu         s                   like(§lacksu)
016000090430     d p01ksu          s                   like(§lacksu)
016100090430     d p01data         s              8  0
016200090304     d wdata           s              8  0 inz
016300090304     d wdatada         s              8  0 inz
016400090304     d wdataal         s              8  0 inz
016500090319     d wkeysk          s             14    inz
016600090320     d wksc            s                   like(§lacksu)
016700090320     d wksu            s                   like(§lacksu)
016800090319     d wora            s              6  0 inz
016900090504     d wtbeke1         s                   like(tbeke1)
017000090327     d wtimehms        s               t   timfmt(*HMS)   inz
017100090430     d wyear           s              4  0 inz
017200090504     d w015a           s             15    inz
017300090304
017400090304      // - Data
017500090430     d w_dtiso         s               d   datfmt(*iso)
017600100107     d w_dtisoda       s               d   datfmt(*iso)
017700090304     d W_dateISO       s               d   datfmt(*iso)
017800090304     d W_oggiISO       s               d   datfmt(*iso)  inz(*job)
017900071219
018000071218      // - Campi di stampa
018100071219     d O_testo         s            132    inz
018200071212
018300071212      //---------------------------------------------------------------
018400071213      //?Definizione procedure esterne.
018500071212      //---------------------------------------------------------------
018600071219
018700071221      // - Reperimento dati utente
018800071221     d tibs34r         pr                  extpgm('TIBS34R')
018900071221     d  tibs34ds                           likeds(TIBS34ds)
019000071219
019100071219      // - Esecuzione comando di sistema
019200071219     d qCmdExc         pr                  extpgm('QCMDEXC')
019300071219     d  Qcmd                        512    const  options(*varsize)
019400071219     d  Qlen                         15  5 const
019500071221
019600071221      // - Gestione tabelle
019700071221     d tibs02r         pr                  extpgm('TIBS02R')
019800071221     d  kpjba                              likeds(KPJBA)    const
019900071221     d  tibs02ds                           likeds(TIBS02ds)
020000090304
020100090304      // - Sottomissione lavoro batch
020200090304     d bch10           pr                  extpgm('BCH10')
020300090304     d  kpjba                              likeds(KPJBA)
020400090304
020500090304      // - Richiamo diretto lavoro batch
020600090304     d tnsb16r         pr                  extpgm('TNSB16R')
020700090304     d  kpjba                              likeds(KPJBA)
020800090304
020900090304      /copy gaitrasrc/srcprotopr,trul33r
021000071212
021100071212      //---------------------------------------------------------------
021200071212      //?Definizione key-list.
021300071212      //---------------------------------------------------------------
021400090430       // - File tntbe01l
021500090430     d k02tntbe01    e ds                  extname(tntbe01l:*key)
021600090430     d                                     inz
021700071212
021800071212      //---------------------------------------------------------------
021900071212      //?Main.
022000071212      //---------------------------------------------------------------
022100071212     c     *Entry        plist
022200071221     c                   parm                    KPJBA
022300071212
022400071212      /free
022500090430
022600090430       // controllo se i dati passati nella kpjbu che mi servono
022700090504       if %check(digits:%subst(kpjbu:1:15)) > 0;
022800090504         // imposto tutto a 0 così non viene elaborato niente
022900090504         clear p01ksu;
023000090504         clear p01data;
023100090504       else;
023200090504         // imposto i campi passati nella kpjbu
023300090504         p01ksu  = %dec(%subst(kpjbu:1:7):7:0);
023400090504         p01data = %dec(%subst(kpjbu:8:8):8:0);
023500090504       endif;
023600090430
023700071212       // Operazioni iniziali
023800071212       exsr RoutInz;
023900071212
024000090430       // Creazione elenco cliente se passato
024100090504       if p01ksu > 0;
024200090430         exsr Elab_TNTBE;
024300090319
024400090430         // se ho dei clienti caricati nella sk
024500090430         if skuni(1) <> *blanks;
024600090430
024700090430           // scrivo rcd spia su tilac con lacela = '33'
024800090430           clear tilac000;
024900090430           lactim = %char(%timestamp:*iso0);
025000090430           lacdir = d16dir;
025100090430           lacela = '33';
025200090430           lactela = 'S';
025300090430           lacksu = d16ksu;
025400090430           lactad = d16tad;
025500090430           lacidjob = d16job;
025600090430           write tilac000;
025700090430
025800090430           // Invio Mail
025900090430           exsr sr_Prep_eMail;
026000071221
026100090430           // Operazioni di chiusura mail
026200090430           exsr RoutEnd;
026300090430
026400090430           // Aggiorno tabella SCH per memorizzare ultima elaborazione
026500090430           exsr sr_tbesch;
026600090430         endif;
026700090430
026800090430       endif;
026900071212
027000071212       //--------------------------------------------------------------
027100071212       //?Operazioni iniziali.
027200071212       //--------------------------------------------------------------
027300071212       BEGSR RoutInz;
027400071219
027500071219         *inLR = *on;
027600071221
027700071221       // Reperimento dati del lavoro
027800071221         exsr DatiJob;
027900071221
028000071221       // Reperimento tab. "MRA"
028100071221         clear dMRAdan;
028200071221         clear kpjbu;
028300071221         reset tibs02ds;
028400090504         T02ke1 = 'TNSB03R';
028500071221         tibs02r(kpjba:tibs02ds);
028600071221         if T02err = *blank;
028700071221           dMRAdan = T02uni;
028800071221         endif;
028900090319
029000090319       // ora Job
029100090319         wora = %dec(%time());
029200090327       // time Job
029300090327         wtimehms = %time();
029400071212
029500071212       ENDSR;
029600071221
029700071221       //--------------------------------------------------------------
029800071221       //?Reperimento Dati del job (Utente/Operativi).
029900071221       //--------------------------------------------------------------
030000071221       BEGSR DatiJob;
030100071221
030200071221         in(E) §AzUte;
030300071221         if NOT %error;
030400071221           in(E) §DatiUte;
030500071221         endif;
030600071221         if %error or RSut = *blanks;
030700071221           clear TIBS34ds;
030800071221           tibs34r(tibs34ds);
030900071221           in §AzUte;
031000071221           in §DatiUte;
031100071221         endif;
031200071221
031300071221       ENDSR;
031400071212
031500071212       //--------------------------------------------------------------
031600071221       //?Elaborazione clienti a cui inviare le immagini
031700071212       //--------------------------------------------------------------
031800071221       BEGSR Elab_TNTBE;
031900071212
032000090430       //?Ciclo di lettura tab. "LAC" per cliente raggr. richiesto?
032100071212
032200090430         setll    c_tab tntbe01l;
032300090430         reade(n) c_tab tntbe01l;
032400071212
032500090430         dow not %eof(tntbe01l);
032600071212
032700090430         //?Elaborazione cliente raggr. richiesto?
032800071212
032900090430           dlac = tbeuni;
033000090430
033100090430           if tbeatb = *blank and §lacksu = p01ksu;
033200090430             exsr sr_Batch;
033300090430           endif;
033400071212
033500090430           reade(n) c_tab tntbe01l;
033600071212
033700090430         enddo;
033800071212
033900071212       ENDSR;
034000090319
034100090319       //--------------------------------------------------------------
034200090319       //?Sottometto lavoro BACTH per estrarre le immagini
034300090319       //--------------------------------------------------------------
034400090319       BEGSR sr_Batch;
034500090319
034600090319         clear tnsb16ds;
034700090319
034800090319       // imposto la data "da" e la data "al"
034900090603       // data da = data ultima estrazione da kpjbu - 30 gg
035000090603         w_dateiso = %date(p01data) - %days(30);
035100090430         wdatada = %dec(w_dateiso);
035200090430       // data al = data ultima estrazione da kpjbu + 7 gg
035300090430         w_dateiso = %date(p01data) + %days(7);
035400090430         wdataal = %dec(w_dateiso);
035500090430
035600090430       // imposto se data spedizione o data consegna
035700090430         if §lactdta = 'C';
035800090430           d16dcd = wdatada;
035900090430           d16dca = wdataal;
036000090430         endif;
036100090430         if §lactdta = 'S';
036200090430           d16dsd = wdatada;
036300090430           d16dsa = wdataal;
036400090430         endif;
036500090512       // se non c'è il flag in tabella imposto fisso data consegna
036600090512         if §lactdta = *blanks;
036700090512           d16dcd = wdatada;
036800090512           d16dca = wdataal;
036900090512         endif;
037000090319
037100090319       // cliente da elaborare
037200090319         d16ksc = %dec(%subst(tbeke1:1:7):7:0);
037300090319
037400090603       // immagini da elaborare "MAI inviate"
037500090603         d16imm = 'M';
037600090319
037700090430       // creazione addebito
037800090319         d16tad = §lactad;
037900090319
038000090319       // directory dove memorizzare le immagini
038100090319         d16dir = §lacdir;
038200090430       // imposto anche il n. della settimana nella directory
038300090609         w_dtiso = %date(wdataal);
038400090430         clear nrweek;
038500090430         exec sql
038600090430           set :nrweek = week_iso(:w_dtiso);
038700090430       // per sicurezza controllo che non sia a 0 il n. della settimana
038800090430       // in questo caso imposto 99
038900090430         if nrweek = 0;
039000090430           nrweek = 99;
039100090430         endif;
039200090430         d16dir = %trim(d16dir) + ' ' + %editc(nrweek:'X');
039300090319
039400090319       // nome immagine
039500090319         d16fmi = §lacfmi;
039600090319
039700090319       // codice raggruppamento;
039800090319         d16ksu = §lacksu;
039900090319
040000090319       // frequenza addebito;
040100090430         d16tadu = §lactadu;
040200090430       // però lo forzo a Job
040300090430         d16tadu = 'J';
040400090319
040500090319       // tipo importo forzato e importo forzato
040600090319         d16fimp = §lacfimp;
040700090319         d16imp  = §lacimp;
040800090319
040900090319       // cliente e codice tariffa forzati
041000090319         d16kscf = §lacksc;
041100090319         d16ctr  = §lacctr;
041200090319
041300090319       // tipo porto
041400090430         d16tpt = §lactpt;
041500090319
041600090319       // resi
041700090430         d16res = §lacres;
041800090319
041900090319       // recuperi
042000090430         d16rec = §lacrec;
042100090319
042200090319       // c/servizi
042300090430         d16csr = §laccsr;
042400090319
042500090319       // bolle con serie
042600090430         d16ssr = §lacssr;
042700090319
042800090319       // linea di partenza
042900090430         d16lnp = §laclnp;
043000090430
043100090430       // ID Job
043200100107         w_dtisoda = %date(p01data) + %days(1);
043300100107         wyear = %subdt(w_dtisoda:*years);
043400090430         d16job = %editc(§lacksu:'X') + 'S' +
043500090430                  %editc(wyear:'X') + %editc(nrweek:'X');
043600090319
043700090319       // memorizzo in sk i dati per la mail
043800090320         wkeysk = %editc(§lacksu:'X') + %editc(d16ksc:'X');
043900090319         xx = %lookup(*zeros:skksc);
044000090319         skuni(xx) = wkeysk;
044100090319         skksc(xx) = d16ksc;
044200090319         skrag(xx) = §lacrag;
044300090319         skaut(xx) = §lacaut;
044400090319         skdal(xx) = wdatada;
044500090319         skal(xx)  = wdataal;
044600090430         skjob(xx) = d16job;
044700090319
044800090319       // sottometto il lavoro
044900090430         kcoaz = 'SB16';
045000090430         kpjbu = TNSB16ds;
045100090630         kbuff = *blank;
045200090430         if  knmus = *all'1';
045300090430           tnsb16r(kpjba);
045400090430         else;
045500090430           BCH10(kpjba);
045600090430         endif;
045700090319
045800090319       ENDSR;
045900071212
046000071212       //--------------------------------------------------------------
046100071219       //?Preparazione e-mail con elenco clienti a cui inviare i dati
046200071212       //--------------------------------------------------------------
046300071219       BEGSR sr_Prep_eMail;
046400071212
046500071221       // Override al file di stampa ed apertura dello stesso
046600071221         if not %open(PRTEMAIL);
046700071221           exsr sr_OpenPrtF;
046800071221         endif;
046900071219
047000090319       // Sorto la sk a key unica
047100090319         sorta skuni;
047200090319
047300090319       // Leggo le sk dei clienti elaborati
047400090319         xx = 1;
047500090320         for xx by 1 to %elem(skuni);
047600090320           clear o_testo;
047700090319           if skuni(xx) <> *blanks;
047800090319             wksc = %dec(%subst(skuni(xx):8:7):7:0);
047900090319             wksu = %dec(%subst(skuni(xx):1:7):7:0);
048000090430             yy = %lookup(wksc:skksc);
048100090320       // stampo papà solo se diverso
048200090320             if wksu <> oldwksu;
048300090320               o_testo = %editc(wksu:'X');
048400090320               oldwksu = wksu;
048500090320             endif;
048600090320       // cliente
048700090325             %subst(o_testo:10:7) = %editc(skksc(yy):'X');
048800090320       // ragione sociale
048900090325             %subst(o_testo:19:25) = skrag(yy);
049000090430       // id job
049100090430             %subst(o_testo:46:16) = skjob(yy);
049200090320       // tipo elaborazione
049300090430             %subst(o_testo:65:1) = skaut(yy);
049400090430       // periodo estrazione
049500090430             %subst(o_testo:72:10) =
049600090430                       %subst(%editc(%dec(skdal(yy)):'X'):7:2) +
049700090430                       '/' +
049800090430                       %subst(%editc(%dec(skdal(yy)):'X'):5:2) +
049900090430                       '/' +
050000090430                       %subst(%editc(%dec(skdal(yy)):'X'):1:4);
050100090430             %subst(o_testo:83:10) =
050200090430                       %subst(%editc(%dec(skal(yy)):'X'):7:2) +
050300090319                       '/' +
050400090320                       %subst(%editc(%dec(skal(yy)):'X'):5:2) +
050500090319                       '/' +
050600090320                       %subst(%editc(%dec(skal(yy)):'X'):1:4);
050700090320             except PrtDet;
050800090320           endif;
050900090319         endfor;
051000071212
051100071212       ENDSR;
051200071221
051300071221       //--------------------------------------------------------------
051400071221       //?Override al file di stampa per impostarvi i dati per
051500071221       //?  l'invio via e-mail   +   stampa inizio e-mail
051600071221       //--------------------------------------------------------------
051700071221       BEGSR sr_OpenPrtF;
051800071221
051900071221         // Override al file di stampa per impostarvi i dati per
052000071221         // l'invio via e-mail
052100071221           exsr sr_Override;
052200071221
052300071221           open PrtEMAIL;
052400071221
052500071221         // Stampa una testata se NON è richiesta la e-mail
052600071221           IF  §MRAdreg =  *blank;
052700071221             O_testo = JobUser + ' - ' + SDSpgm
052800071221                     + ' - ' + %editc( *date : 'Y' )
052900071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
053000071221             except PrtDet;
053100071221             clear O_testo;
053200071221             except PrtDet;
053300071221             except PrtDet;
053400071221           ENDIF;
053500071221
053600071221         // Stampa testo iniziale
053700090319           O_testo = 'Elenco clienti per i quali è stata +
053800090319                      sottomessa la preparazione dei Pod Image in data ' +
053900090319                     %editc(*date:'Y') + ' alle ' + %editw(wora:'  :  :  ');
054000071221           except PrtDet;
054100090320
054200090320         // Stampa una riga vuota
054300090320           clear O_testo;
054400090320           except PrtDet;
054500090319
054600090319         // Stampa intestazione
054700090325           O_testo = 'Padre    Cliente  Ragione Sociale            Id Job' +
054800090430                     '             Elab.  Periodo Estrazione';
054900090319           except PrtDet;
055000071221
055100071221         // Stampa una riga vuota
055200071221           clear O_testo;
055300071221           except PrtDet;
055400071221
055500071221       ENDSR;
055600071219
055700071219       //--------------------------------------------------------------
055800071219       //?Override al file di stampa per impostarvi i dati per
055900071219       //?  l'invio via e-mail   +   stampa inizio e-mail
056000071219       //--------------------------------------------------------------
056100071219       BEGSR sr_Override;
056200071219
056300071220         reset $Invio;
056400071219         reset TRTCM1ds;
056500071221
056600071221         IF  §MRAdreg <> *blank;
056700080317           §CM1mitt = %trim(§MRAdmitt);
056800071221           §CM1dst  = %trim(§MRAddest);
056900071221           §CM1tips = §MRAdreg;
057000071227           §CM1po   = C_Sede;
057100071221           §CM1var  = '*OBJM*' + §MRAddes;
057200071221           §CM1idp  = §MRAdidpro;
057300071221           Qcmd = C_CmdOvrPrtF
057400071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
057500071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
057600071221         ELSE;
057700071221           Qcmd = C_CmdOvrPrtF;
057800071221         ENDIF;
057900071220
058000071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
058100071221         IF  §MRAdreg <> *blanks  and  not %error;
058200071221           $Invio = *on;
058300071221         ENDIF;
058400071219
058500071219       ENDSR;
058600071221
058700071221       //--------------------------------------------------------------
058800071221       //?Operazioni finali.
058900071221       //--------------------------------------------------------------
059000071221       BEGSR RoutEnd;
059100071221
059200071221         if %open(PrtEMAIL);
059300071221
059400071221         //?Chiusura dello spool?
059500071221           clear O_testo;
059600071221           except PrtDet;
059700071221           O_testo = '***   Fine Lista   ***';
059800071221           except PrtDet;
059900071221
060000071221           close PrtEMAIL;
060100071221
060200071221         //?Eliminazione overflow?
060300071221           Qcmd = C_CmdDltOvr;
060400071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
060500071221
060600071221         endif;
060700071221
060800071221       ENDSR;
060900090305
061000090305       //--------------------------------------------------------------
061100090430       //?Aggiorna tabella SCH/SCK
061200090305       //--------------------------------------------------------------
061300090430       BEGSR sr_tbesch;
061400090305
061500090504         exec sql
061600090504          select sck.tbeke1 into :wtbeke1
061700090504           from tntbe00f as sch
061800090504           join tntbe00f as sck
061900090504           on sch.tbeke2 = sck.tbeke1
062000090504           where sch.tbecod = 'SCH' and sck.tbecod = 'SCK'
062100090504           and sch.tbeke1 = :kcdaz
062200090504           and substr(sck.tbeuni, 1, 7) = digits(:p01ksu)
062300090504           and sch.tbeatb = ' ';
062400090504
062500090504         if sqlcod <> 0;
062600090504           leavesr;
062700090504         endif;
062800090504
062900090504         tbecod = 'SCK';
063000090504         tbeke1 = wtbeke1;
063100090504         chain %kds(k02tntbe01:2) tntbe01l;
063200090504         if %found(tntbe01l) and tbeatb = *blanks;
063300090504           %subst(tbeuni:8:8) = %editc(wdataal:'X');
063400090504           update tntbe000;
063500090504         endif;
063600090504
063700090305
063800090504       ENDSR;
063900071212
064000071212      /end-free
064100071219
064200071219      //---------------------------------------------------------------
064300071219      //?Spool di stampa (per e-mail).
064400071219      //---------------------------------------------------------------
064500071219     oPrtEMAIL  e            PRTdet      1
064600071219     o                       O_testo
