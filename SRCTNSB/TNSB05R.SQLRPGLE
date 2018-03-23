000100071212      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200120306     /*END
000300071212      //---------------------------------------------------------------
000400090430      //?TNSB05R - Pod Image settimanale x cliente raggr. + e-mail
000500090430      //?          schedulato settimanalmente sulla tabella SCH
000600090430      //?          dove in kpjbu c'è il cliente raggr. + data ultima
000700090430      //?          estrazione
000800090603      //?          il pgm estrae le MAI estratte
000900090603      //?          da data ultima estrazione - 30 gg calendario
001000090603      //?          a  data ultima estrazione + 7  gg calendario
001100071212      //---------------------------------------------------------------
001200071213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001300090310     h dftactgrp(*no)
001400071212
001500071212      //---------------------------------------------------------------
001600071212      //?Dichiarazione file.
001700071212      //---------------------------------------------------------------
001800071219
001900090430     fTNTBE01L  uf   e           k disk
002000090327     ftilac00f  o    e             disk
002100071219
002200071219     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002300071212
002400071212      //---------------------------------------------------------------
002500071212      //?Definizione costanti.
002600071212      //---------------------------------------------------------------
002700090430     d Digits          c                   const('0123456789')
002800071219
002900071212      // - Tabella in gestione
003000071212     d C_tab           c                   const('LAC')
003100071219
003200071219      // - Dati di default
003300071227     d C_Sede          c                   const('046')
003400071219
003500071219      // - Comando di override al PrtF
003600071221     d C_CmdOvrPrtF    c                   const('OVRPRTF +
003700071221     d                                           file(PRTEMAIL) +
003800071221     d                                           pagesize(66 132) +
003900071221     d                                           lpi(6) cpi(10) +
004000071221     d                                           ovrscope(*actgrpdfn) +
004100071221     d                                           ')
004200071221     d C_CmdDltOvr     c                   const('DLTOVR +
004300071221     d                                            file(PRTEMAIL) +
004400071221     d                                            lvl(*actgrpdfn)')
004500071219
004600071219      //---------------------------------------------------------------
004700071219      //?Definizione schiere.
004800071219      //---------------------------------------------------------------
004900090319      // - schiere per stampare mail
005000090319     d skuni           s                   like(wkeysk) dim(999) inz
005100090319     d skksc           s                   like(d16ksc) dim(999) inz
005200090319     d skrag           s                   like(§lacrag) dim(999) inz
005300090319     d skaut           s                   like(§lacaut) dim(999) inz
005400090319     d skdal           s                   like(d16dcd) dim(999) inz
005500090319     d skal            s                   like(d16dca) dim(999) inz
005600090430     d skjob           s                   like(d16job) dim(999) inz
005700071212
005800071212      //---------------------------------------------------------------
005900071212      //?Definizione aree dati.
006000071212      //---------------------------------------------------------------
006100071221
006200071221      // - Dati utente
006300071221     d §AzUte        e ds                  extname(AZUTE00F)
006400071221     d                                     dtaara
006500071221     d §DatiUte      e ds                  extname(dDatiUte)
006600071221     d                                     dtaara
006700071212
006800071212      //---------------------------------------------------------------
006900071212      //?Definizione strutture dati.
007000071212      //---------------------------------------------------------------
007100071219
007200071212      // - Status
007300071212     d Psds           sds
007400071213     d   SDSpgm          *proc
007500071213     d   JobUser             254    263                                         User name
007600071221
007700071221      // Parametri
007800071221     d kpjba         e ds
007900090304
008000090304      // richiamo al pgm batch
008100090304     d TNSB16ds        ds                  inz
008200090304     d   D16dcd                       8  0 inz
008300090304     d   D16dca                       8  0 inz
008400090304     d   D16ksc                       7  0 inz
008500090304     d   D16imm                       1    inz
008600090304     d   D16tad                       1    inz
008700090304     d   D16dsd                       8  0 inz
008800090304     d   D16dsa                       8  0 inz
008900090304     d   D16dir                      30    inz
009000090304     d   D16fmi                       2    inz
009100090304     d   d16ksu                       7  0 inz
009200090304     d   d16tadu                      1    inz
009300090304     d   d16fimp                      1    inz
009400090304     d   d16imp                      10  3 inz
009500090304     d   d16kscf                      7  0 inz
009600090304     d   d16ctr                       3    inz
009700090304     d   d16job                      16    inz
009800090310     d   d16tpt                       1    inz
009900090310     d   d16res                       1    inz
010000090310     d   d16rec                       1    inz
010100090310     d   d16csr                       1    inz
010200090310     d   d16ssr                       1    inz
010300090310     d   d16lnp                       3  0 inz
010400071212
010500071212      // - Tabella "LAC" = Clienti per ritorno immagini
010600071212     d dLAC          e ds                  inz
010700071221
010800071221      // - Tabella "MRA" = Bart-Mailing - Danni
010900071221     d dMRAdan       e ds                  inz
011000071221
011100071221      // - Reperimento dati utente
011200071221     d TIBS34ds      e ds
011300071221
011400071221      // - Gestione tabelle: controllo e ricerca
011500071221     d TIBS02ds      e ds                  inz
011600071221     d   T02tla      e                     inz('L')
011700071221     d   T02mod      e                     inz('C')
011800071221     d   T02cod      e                     inz('MRA')
011900071219
012000071219      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
012100071219     d TRTCM1ds      e ds                  inz
012200071219      //   ·§CM1mitt = Indirizzo e-mail del mittente
012300120306     d   §CM1mitt    e                     inz('ced@brt.it')
012400071219      //   ·§CM1dst  = Indirizzo e-mail del destinatario
012500120306     d   §CM1dst     e                     inz('nicola.delledonne@brt.it')
012600071219      //   ·§CM1tips = Tipo lettera via e-mail:
012700071219      //    "LET" = testo allegato in corpo con logo
012800071219      //            (richiede righe libere iniziali per il logo)
012900071219      //    "COR" = testo integrato senza logo
013000071219      //            (non consente né UNDERLINE né HIGHLIGHT)
013100071219     d   §CM1tips    e                     inz('COR')
013200071219      //   ·§CM1po   = Filiale
013300071219     d   §CM1po      e                     inz('046')
013400071219      //   ·§CM1var  = Oggetto e-mail
013500071219     d   §CM1var     e                     inz('*OBJM*+
013600071219     d                                     Preparazione CD con immagin+
013700071219     d                                     i delle LdV da inviare ai c+
013800071219     d                                     lienti (v. tab. LAC)')
013900071219      //   ·§CM1sts  = Stato
014000071219     d   §CM1sts     e                     inz(*off)
014100071219      //   ·§CM1sts  = Id processo
014200071219     d   §CM1idp     e                     inz('2')
014300071218
014400071212      //---------------------------------------------------------------
014500071212      //?Definizione variabili globali.
014600071212      //---------------------------------------------------------------
014700071219
014800071212      // - Flags booleani
014900071219     d $Invio          s              1n   inz(*off)
015000071219
015100071219      // - Indici di schiera
015200090320     d xx              s              4  0 inz
015300090320     d yy              s              4  0 inz
015400071219
015500071219      // - Campi per QCMDEXC
015600071219     d Qcmd            s            512    inz
015700071219
015800071213      // - Campi di comodo
015900090430     d nrweek          s              2  0 inz
016000090320     d oldwksu         s                   like(§lacksu)
016100090430     d p01ksu          s                   like(§lacksu)
016200090430     d p01data         s              8  0
016300090304     d wdata           s              8  0 inz
016400090304     d wdatada         s              8  0 inz
016500090304     d wdataal         s              8  0 inz
016600090319     d wkeysk          s             14    inz
016700090320     d wksc            s                   like(§lacksu)
016800090320     d wksu            s                   like(§lacksu)
016900090319     d wora            s              6  0 inz
017000090504     d wtbeke1         s                   like(tbeke1)
017100090327     d wtimehms        s               t   timfmt(*HMS)   inz
017200090430     d wyear           s              4  0 inz
017300090504     d w015a           s             15    inz
017400090304
017500090304      // - Data
017600090430     d w_dtiso         s               d   datfmt(*iso)
017700100107     d w_dtisoda       s               d   datfmt(*iso)
017800090304     d W_dateISO       s               d   datfmt(*iso)
017900090304     d W_oggiISO       s               d   datfmt(*iso)  inz(*job)
018000071219
018100071218      // - Campi di stampa
018200071219     d O_testo         s            132    inz
018300071212
018400071212      //---------------------------------------------------------------
018500071213      //?Definizione procedure esterne.
018600071212      //---------------------------------------------------------------
018700071219
018800071221      // - Reperimento dati utente
018900071221     d tibs34r         pr                  extpgm('TIBS34R')
019000071221     d  tibs34ds                           likeds(TIBS34ds)
019100071219
019200071219      // - Esecuzione comando di sistema
019300071219     d qCmdExc         pr                  extpgm('QCMDEXC')
019400071219     d  Qcmd                        512    const  options(*varsize)
019500071219     d  Qlen                         15  5 const
019600071221
019700071221      // - Gestione tabelle
019800071221     d tibs02r         pr                  extpgm('TIBS02R')
019900071221     d  kpjba                              likeds(KPJBA)    const
020000071221     d  tibs02ds                           likeds(TIBS02ds)
020100090304
020200090304      // - Sottomissione lavoro batch
020300090304     d bch10           pr                  extpgm('BCH10')
020400090304     d  kpjba                              likeds(KPJBA)
020500090304
020600090304      // - Richiamo diretto lavoro batch
020700090304     d tnsb16r         pr                  extpgm('TNSB16R')
020800090304     d  kpjba                              likeds(KPJBA)
020900090304
021000090304      /copy gaitrasrc/srcprotopr,trul33r
021100071212
021200071212      //---------------------------------------------------------------
021300071212      //?Definizione key-list.
021400071212      //---------------------------------------------------------------
021500090430       // - File tntbe01l
021600090430     d k02tntbe01    e ds                  extname(tntbe01l:*key)
021700090430     d                                     inz
021800071212
021900071212      //---------------------------------------------------------------
022000071212      //?Main.
022100071212      //---------------------------------------------------------------
022200071212     c     *Entry        plist
022300071221     c                   parm                    KPJBA
022400071212
022500071212      /free
022600090430
022700090430       // controllo se i dati passati nella kpjbu che mi servono
022800090504       if %check(digits:%subst(kpjbu:1:15)) > 0;
022900090504         // imposto tutto a 0 così non viene elaborato niente
023000090504         clear p01ksu;
023100090504         clear p01data;
023200090504       else;
023300090504         // imposto i campi passati nella kpjbu
023400090504         p01ksu  = %dec(%subst(kpjbu:1:7):7:0);
023500090504         p01data = %dec(%subst(kpjbu:8:8):8:0);
023600090504       endif;
023700090430
023800071212       // Operazioni iniziali
023900071212       exsr RoutInz;
024000071212
024100090430       // Creazione elenco cliente se passato
024200090504       if p01ksu > 0;
024300090430         exsr Elab_TNTBE;
024400090319
024500090430         // se ho dei clienti caricati nella sk
024600090430         if skuni(1) <> *blanks;
024700090430
024800090430           // scrivo rcd spia su tilac con lacela = '33'
024900090430           clear tilac000;
025000090430           lactim = %char(%timestamp:*iso0);
025100090430           lacdir = d16dir;
025200090430           lacela = '33';
025300090430           lactela = 'S';
025400090430           lacksu = d16ksu;
025500090430           lactad = d16tad;
025600090430           lacidjob = d16job;
025700090430           write tilac000;
025800090430
025900090430           // Invio Mail
026000090430           exsr sr_Prep_eMail;
026100071221
026200090430           // Operazioni di chiusura mail
026300090430           exsr RoutEnd;
026400090430
026500090430           // Aggiorno tabella SCH per memorizzare ultima elaborazione
026600090430           exsr sr_tbesch;
026700090430         endif;
026800090430
026900090430       endif;
027000071212
027100071212       //--------------------------------------------------------------
027200071212       //?Operazioni iniziali.
027300071212       //--------------------------------------------------------------
027400071212       BEGSR RoutInz;
027500071219
027600071219         *inLR = *on;
027700071221
027800071221       // Reperimento dati del lavoro
027900071221         exsr DatiJob;
028000071221
028100071221       // Reperimento tab. "MRA"
028200071221         clear dMRAdan;
028300071221         clear kpjbu;
028400071221         reset tibs02ds;
028500090504         T02ke1 = 'TNSB03R';
028600071221         tibs02r(kpjba:tibs02ds);
028700071221         if T02err = *blank;
028800071221           dMRAdan = T02uni;
028900071221         endif;
029000090319
029100090319       // ora Job
029200090319         wora = %dec(%time());
029300090327       // time Job
029400090327         wtimehms = %time();
029500071212
029600071212       ENDSR;
029700071221
029800071221       //--------------------------------------------------------------
029900071221       //?Reperimento Dati del job (Utente/Operativi).
030000071221       //--------------------------------------------------------------
030100071221       BEGSR DatiJob;
030200071221
030300071221         in(E) §AzUte;
030400071221         if NOT %error;
030500071221           in(E) §DatiUte;
030600071221         endif;
030700071221         if %error or RSut = *blanks;
030800071221           clear TIBS34ds;
030900071221           tibs34r(tibs34ds);
031000071221           in §AzUte;
031100071221           in §DatiUte;
031200071221         endif;
031300071221
031400071221       ENDSR;
031500071212
031600071212       //--------------------------------------------------------------
031700071221       //?Elaborazione clienti a cui inviare le immagini
031800071212       //--------------------------------------------------------------
031900071221       BEGSR Elab_TNTBE;
032000071212
032100090430       //?Ciclo di lettura tab. "LAC" per cliente raggr. richiesto?
032200071212
032300090430         setll    c_tab tntbe01l;
032400090430         reade(n) c_tab tntbe01l;
032500071212
032600090430         dow not %eof(tntbe01l);
032700071212
032800090430         //?Elaborazione cliente raggr. richiesto?
032900071212
033000090430           dlac = tbeuni;
033100090430
033200090430           if tbeatb = *blank and §lacksu = p01ksu;
033300090430             exsr sr_Batch;
033400090430           endif;
033500071212
033600090430           reade(n) c_tab tntbe01l;
033700071212
033800090430         enddo;
033900071212
034000071212       ENDSR;
034100090319
034200090319       //--------------------------------------------------------------
034300090319       //?Sottometto lavoro BACTH per estrarre le immagini
034400090319       //--------------------------------------------------------------
034500090319       BEGSR sr_Batch;
034600090319
034700090319         clear tnsb16ds;
034800090319
034900090319       // imposto la data "da" e la data "al"
035000090603       // data da = data ultima estrazione da kpjbu - 30 gg
035100090603         w_dateiso = %date(p01data) - %days(30);
035200090430         wdatada = %dec(w_dateiso);
035300090430       // data al = data ultima estrazione da kpjbu + 7 gg
035400090430         w_dateiso = %date(p01data) + %days(7);
035500090430         wdataal = %dec(w_dateiso);
035600090430
035700090430       // imposto se data spedizione o data consegna
035800090430         if §lactdta = 'C';
035900090430           d16dcd = wdatada;
036000090430           d16dca = wdataal;
036100090430         endif;
036200090430         if §lactdta = 'S';
036300090430           d16dsd = wdatada;
036400090430           d16dsa = wdataal;
036500090430         endif;
036600090512       // se non c'è il flag in tabella imposto fisso data consegna
036700090512         if §lactdta = *blanks;
036800090512           d16dcd = wdatada;
036900090512           d16dca = wdataal;
037000090512         endif;
037100090319
037200090319       // cliente da elaborare
037300090319         d16ksc = %dec(%subst(tbeke1:1:7):7:0);
037400090319
037500090603       // immagini da elaborare "MAI inviate"
037600090603         d16imm = 'M';
037700090319
037800090430       // creazione addebito
037900090319         d16tad = §lactad;
038000090319
038100090319       // directory dove memorizzare le immagini
038200090319         d16dir = §lacdir;
038300090430       // imposto anche il n. della settimana nella directory
038400090609         w_dtiso = %date(wdataal);
038500090430         clear nrweek;
038600090430         exec sql
038700090430           set :nrweek = week_iso(:w_dtiso);
038800090430       // per sicurezza controllo che non sia a 0 il n. della settimana
038900090430       // in questo caso imposto 99
039000090430         if nrweek = 0;
039100090430           nrweek = 99;
039200090430         endif;
039300090430         d16dir = %trim(d16dir) + ' ' + %editc(nrweek:'X');
039400090319
039500090319       // nome immagine
039600090319         d16fmi = §lacfmi;
039700090319
039800090319       // codice raggruppamento;
039900090319         d16ksu = §lacksu;
040000090319
040100090319       // frequenza addebito;
040200090430         d16tadu = §lactadu;
040300090430       // però lo forzo a Job
040400090430         d16tadu = 'J';
040500090319
040600090319       // tipo importo forzato e importo forzato
040700090319         d16fimp = §lacfimp;
040800090319         d16imp  = §lacimp;
040900090319
041000090319       // cliente e codice tariffa forzati
041100090319         d16kscf = §lacksc;
041200090319         d16ctr  = §lacctr;
041300090319
041400090319       // tipo porto
041500090430         d16tpt = §lactpt;
041600090319
041700090319       // resi
041800090430         d16res = §lacres;
041900090319
042000090319       // recuperi
042100090430         d16rec = §lacrec;
042200090319
042300090319       // c/servizi
042400090430         d16csr = §laccsr;
042500090319
042600090319       // bolle con serie
042700090430         d16ssr = §lacssr;
042800090319
042900090319       // linea di partenza
043000090430         d16lnp = §laclnp;
043100090430
043200090430       // ID Job
043300100107         w_dtisoda = %date(p01data) + %days(1);
043400100107         wyear = %subdt(w_dtisoda:*years);
043500090430         d16job = %editc(§lacksu:'X') + 'S' +
043600090430                  %editc(wyear:'X') + %editc(nrweek:'X');
043700090319
043800090319       // memorizzo in sk i dati per la mail
043900090320         wkeysk = %editc(§lacksu:'X') + %editc(d16ksc:'X');
044000090319         xx = %lookup(*zeros:skksc);
044100090319         skuni(xx) = wkeysk;
044200090319         skksc(xx) = d16ksc;
044300090319         skrag(xx) = §lacrag;
044400090319         skaut(xx) = §lacaut;
044500090319         skdal(xx) = wdatada;
044600090319         skal(xx)  = wdataal;
044700090430         skjob(xx) = d16job;
044800090319
044900090319       // sottometto il lavoro
045000090430         kcoaz = 'SB16';
045100090430         kpjbu = TNSB16ds;
045200090630         kbuff = *blank;
045300090430         if  knmus = *all'1';
045400090430           tnsb16r(kpjba);
045500090430         else;
045600090430           BCH10(kpjba);
045700090430         endif;
045800090319
045900090319       ENDSR;
046000071212
046100071212       //--------------------------------------------------------------
046200071219       //?Preparazione e-mail con elenco clienti a cui inviare i dati
046300071212       //--------------------------------------------------------------
046400071219       BEGSR sr_Prep_eMail;
046500071212
046600071221       // Override al file di stampa ed apertura dello stesso
046700071221         if not %open(PRTEMAIL);
046800071221           exsr sr_OpenPrtF;
046900071221         endif;
047000071219
047100090319       // Sorto la sk a key unica
047200090319         sorta skuni;
047300090319
047400090319       // Leggo le sk dei clienti elaborati
047500090319         xx = 1;
047600090320         for xx by 1 to %elem(skuni);
047700090320           clear o_testo;
047800090319           if skuni(xx) <> *blanks;
047900090319             wksc = %dec(%subst(skuni(xx):8:7):7:0);
048000090319             wksu = %dec(%subst(skuni(xx):1:7):7:0);
048100090430             yy = %lookup(wksc:skksc);
048200090320       // stampo papà solo se diverso
048300090320             if wksu <> oldwksu;
048400090320               o_testo = %editc(wksu:'X');
048500090320               oldwksu = wksu;
048600090320             endif;
048700090320       // cliente
048800090325             %subst(o_testo:10:7) = %editc(skksc(yy):'X');
048900090320       // ragione sociale
049000090325             %subst(o_testo:19:25) = skrag(yy);
049100090430       // id job
049200090430             %subst(o_testo:46:16) = skjob(yy);
049300090320       // tipo elaborazione
049400090430             %subst(o_testo:65:1) = skaut(yy);
049500090430       // periodo estrazione
049600090430             %subst(o_testo:72:10) =
049700090430                       %subst(%editc(%dec(skdal(yy)):'X'):7:2) +
049800090430                       '/' +
049900090430                       %subst(%editc(%dec(skdal(yy)):'X'):5:2) +
050000090430                       '/' +
050100090430                       %subst(%editc(%dec(skdal(yy)):'X'):1:4);
050200090430             %subst(o_testo:83:10) =
050300090430                       %subst(%editc(%dec(skal(yy)):'X'):7:2) +
050400090319                       '/' +
050500090320                       %subst(%editc(%dec(skal(yy)):'X'):5:2) +
050600090319                       '/' +
050700090320                       %subst(%editc(%dec(skal(yy)):'X'):1:4);
050800090320             except PrtDet;
050900090320           endif;
051000090319         endfor;
051100071212
051200071212       ENDSR;
051300071221
051400071221       //--------------------------------------------------------------
051500071221       //?Override al file di stampa per impostarvi i dati per
051600071221       //?  l'invio via e-mail   +   stampa inizio e-mail
051700071221       //--------------------------------------------------------------
051800071221       BEGSR sr_OpenPrtF;
051900071221
052000071221         // Override al file di stampa per impostarvi i dati per
052100071221         // l'invio via e-mail
052200071221           exsr sr_Override;
052300071221
052400071221           open PrtEMAIL;
052500071221
052600071221         // Stampa una testata se NON è richiesta la e-mail
052700071221           IF  §MRAdreg =  *blank;
052800071221             O_testo = JobUser + ' - ' + SDSpgm
052900071221                     + ' - ' + %editc( *date : 'Y' )
053000071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
053100071221             except PrtDet;
053200071221             clear O_testo;
053300071221             except PrtDet;
053400071221             except PrtDet;
053500071221           ENDIF;
053600071221
053700071221         // Stampa testo iniziale
053800090319           O_testo = 'Elenco clienti per i quali è stata +
053900090319                      sottomessa la preparazione dei Pod Image in data ' +
054000090319                     %editc(*date:'Y') + ' alle ' + %editw(wora:'  :  :  ');
054100071221           except PrtDet;
054200090320
054300090320         // Stampa una riga vuota
054400090320           clear O_testo;
054500090320           except PrtDet;
054600090319
054700090319         // Stampa intestazione
054800090325           O_testo = 'Padre    Cliente  Ragione Sociale            Id Job' +
054900090430                     '             Elab.  Periodo Estrazione';
055000090319           except PrtDet;
055100071221
055200071221         // Stampa una riga vuota
055300071221           clear O_testo;
055400071221           except PrtDet;
055500071221
055600071221       ENDSR;
055700071219
055800071219       //--------------------------------------------------------------
055900071219       //?Override al file di stampa per impostarvi i dati per
056000071219       //?  l'invio via e-mail   +   stampa inizio e-mail
056100071219       //--------------------------------------------------------------
056200071219       BEGSR sr_Override;
056300071219
056400071220         reset $Invio;
056500071219         reset TRTCM1ds;
056600071221
056700071221         IF  §MRAdreg <> *blank;
056800080317           §CM1mitt = %trim(§MRAdmitt);
056900071221           §CM1dst  = %trim(§MRAddest);
057000071221           §CM1tips = §MRAdreg;
057100071227           §CM1po   = C_Sede;
057200071221           §CM1var  = '*OBJM*' + §MRAddes;
057300071221           §CM1idp  = §MRAdidpro;
057400071221           Qcmd = C_CmdOvrPrtF
057500071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
057600071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
057700071221         ELSE;
057800071221           Qcmd = C_CmdOvrPrtF;
057900071221         ENDIF;
058000071220
058100071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
058200071221         IF  §MRAdreg <> *blanks  and  not %error;
058300071221           $Invio = *on;
058400071221         ENDIF;
058500071219
058600071219       ENDSR;
058700071221
058800071221       //--------------------------------------------------------------
058900071221       //?Operazioni finali.
059000071221       //--------------------------------------------------------------
059100071221       BEGSR RoutEnd;
059200071221
059300071221         if %open(PrtEMAIL);
059400071221
059500071221         //?Chiusura dello spool?
059600071221           clear O_testo;
059700071221           except PrtDet;
059800071221           O_testo = '***   Fine Lista   ***';
059900071221           except PrtDet;
060000071221
060100071221           close PrtEMAIL;
060200071221
060300071221         //?Eliminazione overflow?
060400071221           Qcmd = C_CmdDltOvr;
060500071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
060600071221
060700071221         endif;
060800071221
060900071221       ENDSR;
061000090305
061100090305       //--------------------------------------------------------------
061200090430       //?Aggiorna tabella SCH/SCK
061300090305       //--------------------------------------------------------------
061400090430       BEGSR sr_tbesch;
061500090305
061600090504         exec sql
061700090504          select sck.tbeke1 into :wtbeke1
061800090504           from tntbe00f as sch
061900090504           join tntbe00f as sck
062000090504           on sch.tbeke2 = sck.tbeke1
062100090504           where sch.tbecod = 'SCH' and sck.tbecod = 'SCK'
062200090504           and sch.tbeke1 = :kcdaz
062300090504           and substr(sck.tbeuni, 1, 7) = digits(:p01ksu)
062400090504           and sch.tbeatb = ' ';
062500090504
062600090504         if sqlcod <> 0;
062700090504           leavesr;
062800090504         endif;
062900090504
063000090504         tbecod = 'SCK';
063100090504         tbeke1 = wtbeke1;
063200090504         chain %kds(k02tntbe01:2) tntbe01l;
063300090504         if %found(tntbe01l) and tbeatb = *blanks;
063400090504           %subst(tbeuni:8:8) = %editc(wdataal:'X');
063500090504           update tntbe000;
063600090504         endif;
063700090504
063800090305
063900090504       ENDSR;
064000071212
064100071212      /end-free
064200071219
064300071219      //---------------------------------------------------------------
064400071219      //?Spool di stampa (per e-mail).
064500071219      //---------------------------------------------------------------
064600071219     oPrtEMAIL  e            PRTdet      1
064700071219     o                       O_testo
