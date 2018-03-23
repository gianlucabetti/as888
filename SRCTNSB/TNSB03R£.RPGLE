000100071212      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200120306     /*PRM OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000300120306     /*END
000400071212      //---------------------------------------------------------------
000500071221      //?TNSB03R - Controllo schedulazione invio immagini a clienti
000600071212      //---------------------------------------------------------------
000700071213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800090310     h dftactgrp(*no)
000900071212
001000071212      //---------------------------------------------------------------
001100071212      //?Dichiarazione file.
001200071212      //---------------------------------------------------------------
001300071219
001400090305     fTNTBE01L  uf   e           k disk
001500090327     ftilac00f  o    e             disk
001600071219
001700140813     fPrtEMAIL  o    f  198        printer  oflind(*inOF)  usropn
001800071212
001900071212      //---------------------------------------------------------------
002000071212      //?Definizione costanti.
002100071212      //---------------------------------------------------------------
002200071219
002300071212      // - Tabella in gestione
002400071212     d C_tab           c                   const('LAC')
002500071219
002600071219      // - Dati di default
002700071227     d C_Sede          c                   const('046')
002800071219
002900071219      // - Comando di override al PrtF
003000071221     d C_CmdOvrPrtF    c                   const('OVRPRTF +
003100071221     d                                           file(PRTEMAIL) +
003200140813     d                                           pagesize(66 198) +
003300071221     d                                           lpi(6) cpi(10) +
003400071221     d                                           ovrscope(*actgrpdfn) +
003500071221     d                                           ')
003600071221     d C_CmdDltOvr     c                   const('DLTOVR +
003700071221     d                                            file(PRTEMAIL) +
003800071221     d                                            lvl(*actgrpdfn)')
003900071219
004000071219      //---------------------------------------------------------------
004100071219      //?Definizione schiere.
004200071219      //---------------------------------------------------------------
004300090319      // - schiere per stampare mail
004400090319     d skuni           s                   like(wkeysk) dim(999) inz
004500090319     d skksc           s                   like(d16ksc) dim(999) inz
004600090319     d skrag           s                   like(§lacrag) dim(999) inz
004700090319     d skaut           s                   like(§lacaut) dim(999) inz
004800090319     d skdal           s                   like(d16dcd) dim(999) inz
004900090319     d skal            s                   like(d16dca) dim(999) inz
005000110713     d skfile          s                   like(§lacele) dim(999) inz
005100140813     d skdir           s                   like(§lacdir) dim(999) inz
005200090319
005300090319      // - schiere per memorizzare il JOB
005400090319     d skcliu          s                   like(§lacksu) dim(999) inz
005500090319     d skjobu          s                   like(o33nrf) dim(999) inz
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
010600071213     d   $LACschMm                    1    overlay(dLAC : 71)
010700071213     d                                     inz  dim(12)
010800071221
010900071221      // - Tabella "MRA" = Bart-Mailing - Danni
011000071221     d dMRAdan       e ds                  inz
011100071221
011200071221      // - Reperimento dati utente
011300071221     d TIBS34ds      e ds
011400071221
011500071221      // - Gestione tabelle: controllo e ricerca
011600071221     d TIBS02ds      e ds                  inz
011700071221     d   T02tla      e                     inz('L')
011800071221     d   T02mod      e                     inz('C')
011900071221     d   T02cod      e                     inz('MRA')
012000090305
012100090305      // - Numeratore
012200090305     d trul33ds      e ds                  inz
012300071219
012400071219      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
012500071219     d TRTCM1ds      e ds                  inz
012600071219      //   ·§CM1mitt = Indirizzo e-mail del mittente
012700120306     d   §CM1mitt    e                     inz('ced@brt.it')
012800071219      //   ·§CM1dst  = Indirizzo e-mail del destinatario
012900120306     d   §CM1dst     e                     inz('nicola.delledonne@brt.it')
013000071219      //   ·§CM1tips = Tipo lettera via e-mail:
013100071219      //    "LET" = testo allegato in corpo con logo
013200071219      //            (richiede righe libere iniziali per il logo)
013300071219      //    "COR" = testo integrato senza logo
013400071219      //            (non consente né UNDERLINE né HIGHLIGHT)
013500121011     d   §CM1tips    e                     inz('COI')
013600071219      //   ·§CM1po   = Filiale
013700071219     d   §CM1po      e                     inz('046')
013800071219      //   ·§CM1var  = Oggetto e-mail
013900071219     d   §CM1var     e                     inz('*OBJM*+
014000071219     d                                     Preparazione CD con immagin+
014100071219     d                                     i delle LdV da inviare ai c+
014200071219     d                                     lienti (v. tab. LAC)')
014300071219      //   ·§CM1sts  = Stato
014400071219     d   §CM1sts     e                     inz(*off)
014500071219      //   ·§CM1sts  = Id processo
014600071219     d   §CM1idp     e                     inz('2')
014700071218
014800071212      //---------------------------------------------------------------
014900071212      //?Definizione variabili globali.
015000071212      //---------------------------------------------------------------
015100071219
015200071212      // - Flags booleani
015300071213     d W_Esito         s              1n   inz(*off)
015400071219     d $Invio          s              1n   inz(*off)
015500071219
015600071219      // - Indici di schiera
015700090320     d xx              s              4  0 inz
015800090320     d yy              s              4  0 inz
015900090320     d zz              s              4  0 inz
016000071219
016100071219      // - Campi per QCMDEXC
016200071219     d Qcmd            s            512    inz
016300071219
016400071213      // - Campi di comodo
016500090320     d oldwksu         s                   like(§lacksu)
016600090304     d wdata           s              8  0 inz
016700090304     d wdatada         s              8  0 inz
016800090304     d wdataal         s              8  0 inz
016900090319     d wkeysk          s             14    inz
017000090320     d wksc            s                   like(§lacksu)
017100090320     d wksu            s                   like(§lacksu)
017200090319     d wora            s              6  0 inz
017300090327     d wtimehms        s               t   timfmt(*HMS)   inz
017400090708     d wjob            s              7  0
017500090304
017600090304      // - Data
017700090304     d W_dateISO       s               d   datfmt(*iso)
017800090304     d W_oggiISO       s               d   datfmt(*iso)  inz(*job)
017900071219
018000071218      // - Campi di stampa
018100140813     d O_testo         s            198    inz
018200071212
018300071212      //---------------------------------------------------------------
018400071213      //?Definizione procedure esterne.
018500071212      //---------------------------------------------------------------
018600071219
018700071221      // - Reperimento dati utente
018800071221     d tibs34r         pr                  extpgm('TIBS34R')
018900071221     d  tibs34ds                           likeds(TIBS34ds)
019000071221
019100071219      // - Verifica se cliente a cui inviare le immagini
019200071219     d CheckDataConSchGg...
019300071213     d                 pr             1a
019400071219     d   pi_LACschGG                  2s 0 const
019500071219     d   pi_LACschMM                  1a   const  dim(12)
019600071219     d   pi_Date                       d   const  datfmt(*iso)
019700071219     d   po_Esito                     1a   options(*nopass)
019800071219
019900071219      // - Esecuzione comando di sistema
020000071219     d qCmdExc         pr                  extpgm('QCMDEXC')
020100071219     d  Qcmd                        512    const  options(*varsize)
020200071219     d  Qlen                         15  5 const
020300071221
020400071221      // - Gestione tabelle
020500071221     d tibs02r         pr                  extpgm('TIBS02R')
020600071221     d  kpjba                              likeds(KPJBA)    const
020700071221     d  tibs02ds                           likeds(TIBS02ds)
020800090304
020900090304      // - Sottomissione lavoro batch
021000090304     d bch10           pr                  extpgm('BCH10')
021100090304     d  kpjba                              likeds(KPJBA)
021200090304
021300090304      // - Richiamo diretto lavoro batch
021400090304     d tnsb16r         pr                  extpgm('TNSB16R')
021500090304     d  kpjba                              likeds(KPJBA)
021600090304
021700090304      /copy gaitrasrc/srcprotopr,trul33r
021800071212
021900071212      //---------------------------------------------------------------
022000071212      //?Definizione key-list.
022100071212      //---------------------------------------------------------------
022200071212
022300071212      //---------------------------------------------------------------
022400071212      //?Main.
022500071212      //---------------------------------------------------------------
022600071212     c     *Entry        plist
022700071221     c                   parm                    KPJBA
022800071212
022900071212      /free
023000071212
023100071212       // Operazioni iniziali
023200071212       exsr RoutInz;
023300071212
023400071221       // Creazione elenco clienti (se reperita tab. "MRA")
023500071221       exsr Elab_TNTBE;
023600090319
023700090319       // Invio Mail
023800090326       // se ho dei clienti caricati nella sk
023900090326       if skuni(1) <> *blanks;
024000090326         exsr sr_Prep_eMail;
024100071221
024200071221       // Operazioni finali
024300090326       // di chiusura mail
024400090326         exsr RoutEnd;
024500090326       endif;
024600071212
024700071212       //--------------------------------------------------------------
024800071212       //?Operazioni iniziali.
024900071212       //--------------------------------------------------------------
025000071212       BEGSR RoutInz;
025100071219
025200071219         *inLR = *on;
025300071221
025400071221       // Reperimento dati del lavoro
025500071221         exsr DatiJob;
025600071221
025700071221       // Reperimento tab. "MRA"
025800071221         clear dMRAdan;
025900071221         clear kpjbu;
026000071221         reset tibs02ds;
026100071221         T02ke1 = SDSpgm;
026200071221         tibs02r(kpjba:tibs02ds);
026300071221         if T02err = *blank;
026400071221           dMRAdan = T02uni;
026500071221         endif;
026600090319
026700090319       // ora Job
026800090319         wora = %dec(%time());
026900090327       // time Job
027000090327         wtimehms = %time();
027100071212
027200071212       ENDSR;
027300071221
027400071221       //--------------------------------------------------------------
027500071221       //?Reperimento Dati del job (Utente/Operativi).
027600071221       //--------------------------------------------------------------
027700071221       BEGSR DatiJob;
027800071221
027900071221         in(E) §AzUte;
028000071221         if NOT %error;
028100071221           in(E) §DatiUte;
028200071221         endif;
028300071221         if %error or RSut = *blanks;
028400071221           clear TIBS34ds;
028500071221           tibs34r(tibs34ds);
028600071221           in §AzUte;
028700071221           in §DatiUte;
028800071221         endif;
028900071221
029000071221       ENDSR;
029100071212
029200071212       //--------------------------------------------------------------
029300071221       //?Elaborazione clienti a cui inviare le immagini
029400071212       //--------------------------------------------------------------
029500071221       BEGSR Elab_TNTBE;
029600071212
029700071212       //?Ciclo di lettura tab. "LAC"?
029800071212
029900071212         setll C_tab TNTBE000;
030000090305         reade(n) C_tab TNTBE000;
030100071212
030200071212         DOW not %eof(TNTBE01L);
030300071212
030400071212         //?Elaborazione singolo cliente?
030500071212
030600071212           dLAC = TBEuni;
030700071212           IF TBEatb    = *blank   and
030800071213              §LACschGg > *zero;
030900071212
031000071221             // Segnalazione cliente per invio e-mail
031100071219             if  CheckDataConSchGg( §LACschGg :
031200071219                                    $LACschMm :
031300090304                                    W_oggiISO :
031400071219                                    W_Esito ) = *on;
031500090305               exsr sr_Batch;
031600100121             // Aggiorno tabella se ho sottomesso il lavoro
031700100121               IF  §lacaut = 'S';
031800100121                 exsr sr_AggTbLAC;
031900100121               ENDIF;
032000071212             endif;
032100071212
032200071212           ENDIF;
032300071212
032400090305           reade(n) C_tab TNTBE000;
032500071212
032600071212         ENDDO;
032700071212
032800071212       ENDSR;
032900090319
033000090319       //--------------------------------------------------------------
033100090319       //?Sottometto lavoro BACTH per estrarre le immagini
033200090319       //--------------------------------------------------------------
033300090319       BEGSR sr_Batch;
033400090319
033500090319         clear tnsb16ds;
033600090319
033700090319       // imposto la data "da" e la data "al"
033800090320       // se presenti
033900090320         if §lacdatai <> *zeros;
034000090319       // - se non ho mai elaborato parto dalla data prima estrazione
034100090319       //   assoluta
034200090320           if §lacdatela = *zeros;
034300090320             wdata = §lacdatai*100+1;
034400090320           endif;
034500090319       // - se ho già elaborato parto dalla data prossima estrazione
034600090320           if §lacdatela <> *zeros;
034700090320             wdata = §lacdatela*100+1;
034800090320           endif;
034900090319       // - imposto le date
035000090320           wdatada = wdata;
035100090320           w_dateiso = %date(wdatada) + %months(§lacmesi) - %days(1);
035200090320           wdataal = %dec(w_dateiso);
035300090320           if §lactdta = 'C';
035400090320             d16dcd = wdatada;
035500090320             d16dca = wdataal;
035600090320           endif;
035700090320           if §lactdta = 'S';
035800090320             d16dsd = wdatada;
035900090320             d16dsa = wdataal;
036000090320           endif;
036100090320         else;
036200090320           clear wdatada;
036300090320           clear wdataal;
036400090320         endif;
036500090319
036600090319       // cliente da elaborare
036700090319         d16ksc = %dec(%subst(tbeke1:1:7):7:0);
036800090319
036900090319       // immagini da elaborare
037000090319         d16imm = 'E';
037100090319
037200090319       // creazione addbito
037300090319         d16tad = §lactad;
037400090319
037500090319       // directory dove memorizzare le immagini
037600090319       // - se nella directory ci sono &AAA o &MM sono da sostituire
037700090319         d16dir = §lacdir;
037800090319         if %scan('&AAA':d16dir) > *zeros;
037900090319           d16dir = %replace( %subst( %editc(wdatada:'X'):1:4) :
038000090319                    d16dir : %scan('&AAA':d16dir) );
038100090319         endif;
038200090319         if %scan('&M':d16dir) > *zeros;
038300090319           d16dir = %replace( %subst( %editc(wdatada:'X'):5:2) :
038400090319                    d16dir : %scan('&M':d16dir) );
038500090319         endif;
038600090319         if %scan('&AAA':d16dir) > *zeros;
038700090319           d16dir = %replace( %subst( %editc(wdataal:'X'):1:4) :
038800090319                    d16dir : %scan('&AAA':d16dir) );
038900090319         endif;
039000090319         if %scan('&M':d16dir) > *zeros;
039100090319           d16dir = %replace( %subst( %editc(wdataal:'X'):5:2) :
039200090319                    d16dir : %scan('&M':d16dir) );
039300090319         endif;
039400090319
039500090319       // nome immagine
039600090319         d16fmi = §lacfmi;
039700090319
039800090319       // codice raggruppamento;
039900090319         d16ksu = §lacksu;
040000090319
040100090319       // frequenza addebito;
040200090319         d16tadu = §lactadu;
040300090319
040400090319       // tipo importo forzato e importo forzato
040500090319         d16fimp = §lacfimp;
040600090319         d16imp  = §lacimp;
040700090319
040800090319       // cliente e codice tariffa forzati
040900090319         d16kscf = §lacksc;
041000090319         d16ctr  = §lacctr;
041100090319
041200090319       // tipo porto
041300090319           d16tpt = §lactpt;
041400090319
041500090319       // resi
041600090319           d16res = §lacres;
041700090319
041800090319       // recuperi
041900090319           d16rec = §lacrec;
042000090319
042100090319       // c/servizi
042200090319           d16csr = §laccsr;
042300090319
042400090319       // bolle con serie
042500090319           d16ssr = §lacssr;
042600090319
042700090319       // linea di partenza
042800090319           d16lnp = §laclnp;
042900090319
043000090319       // memorizzo in sk i dati per la mail
043100090320         wkeysk = %editc(§lacksu:'X') + %editc(d16ksc:'X');
043200090319         xx = %lookup(*zeros:skksc);
043300090319         skuni(xx) = wkeysk;
043400090319         skksc(xx) = d16ksc;
043500090319         skrag(xx) = §lacrag;
043600090319         skaut(xx) = §lacaut;
043700090319         skdal(xx) = wdatada;
043800090319         skal(xx)  = wdataal;
043900140813         skdir(xx) = d16dir;
044000110713         clear skfile(xx);
044100110713         IF  §LACpgm <> *blanks;
044200110713           skfile(xx) = 'S';
044300110713         ENDIF;
044400090319
044500090319       // se devo sottomettere
044600090319         if §lacaut = 'S';
044700090319       // ID Job
044800090319       // - per ogni §lacksu devo staccare un numero JOB
044900090319       // - e memorizzo in schiera
045000090319           yy = %lookup(§lacksu:skcliu);
045100090319           if yy > *zeros;
045200090319             d16job = %char(skjobu(yy));
045300090319           else;
045400090319             yy = %lookup(*zeros:skcliu);
045500090319             clear trul33ds;
045600090319             I33OPE = *zeros;
045700090319             I33CNU = 400;
045800090319             I33NUM = 1;
045900090319             KPJBU = trul33ds;
046000090319             trul33r (KPJBA);
046100090319             trul33ds = KPJBU;
046200090319             skcliu(yy) = §lacksu;
046300090319             skjobu(yy) = o33nrf;
046400090319             d16job = %char(skjobu(yy));
046500090407          // scrivo rcd spia su tilac con lacela = '33'
046600090407             clear tilac000;
046700090407             lactim = %char(%timestamp:*iso0);
046800090407             lacdir = d16dir;
046900090407             lacela = '33';
047000090407             lactela = 'S';
047100090407             lacksu = d16ksu;
047200090407             lactad = d16tad;
047300090407             lacidjob = d16job;
047400090407             write tilac000;
047500090319           endif;
047600090319
047700090319       // sottometto il lavoro
047800090319            kcoaz = 'SB16';
047900090319            kpjbu = TNSB16ds;
048000090715            kbuff = *blank;
048100090319            if  knmus = *all'1';
048200090319              tnsb16r(kpjba);
048300090319            else;
048400090319              BCH10(kpjba);
048500090319            endif;
048600090319
048700090319          endif;
048800090319
048900090319       ENDSR;
049000071212
049100071212       //--------------------------------------------------------------
049200071219       //?Preparazione e-mail con elenco clienti a cui inviare i dati
049300071212       //--------------------------------------------------------------
049400071219       BEGSR sr_Prep_eMail;
049500071212
049600071221       // Override al file di stampa ed apertura dello stesso
049700071221         if not %open(PRTEMAIL);
049800071221           exsr sr_OpenPrtF;
049900071221         endif;
050000071219
050100090319       // Sorto la sk a key unica
050200090319         sorta skuni;
050300090319
050400090319       // Leggo le sk dei clienti elaborati
050500090319         xx = 1;
050600090320         for xx by 1 to %elem(skuni);
050700090320           clear o_testo;
050800090319           if skuni(xx) <> *blanks;
050900090319             wksc = %dec(%subst(skuni(xx):8:7):7:0);
051000090319             wksu = %dec(%subst(skuni(xx):1:7):7:0);
051100090319             yy = %lookup(wksc:skksc);
051200090320             zz = %lookup(wksu:skcliu);
051300090320       // stampo papà solo se diverso
051400090320             if wksu <> oldwksu;
051500090320               o_testo = %editc(wksu:'X');
051600090320               oldwksu = wksu;
051700090320             endif;
051800090320       // cliente
051900090325             %subst(o_testo:10:7) = %editc(skksc(yy):'X');
052000090320       // ragione sociale
052100090325             %subst(o_testo:19:25) = skrag(yy);
052200090320       // id job se presente
052300090320             if skaut(yy) = 'S';
052400090708               wjob = skjobu(zz);
052500090708               %subst(o_testo:46:7) = %editc(wjob:'Z');
052600090320             endif;
052700090320       // tipo elaborazione
052800090325             %subst(o_testo:56:1) = skaut(yy);
052900090320       // periodo estrazione se presente
053000090320             if skdal(yy) <> *zeros;
053100090325               %subst(o_testo:62:10) =
053200090320                         %subst(%editc(%dec(skdal(yy)):'X'):7:2) +
053300090320                         '/' +
053400090320                         %subst(%editc(%dec(skdal(yy)):'X'):5:2) +
053500090320                         '/' +
053600090320                         %subst(%editc(%dec(skdal(yy)):'X'):1:4);
053700090320             endif;
053800090320             if skal(yy) <> *zeros;
053900090325               %subst(o_testo:73:10) =
054000090320                       %subst(%editc(%dec(skal(yy)):'X'):7:2) +
054100090319                       '/' +
054200090320                       %subst(%editc(%dec(skal(yy)):'X'):5:2) +
054300090319                       '/' +
054400090320                       %subst(%editc(%dec(skal(yy)):'X'):1:4);
054500090320             endif;
054600140813       // se elaborazione = nessuna avviso
054700140813             if skaut(yy) = 'N';
054800140813               %subst(o_testo:62:21) = 'VERIFICARE!!         ';
054900140813             endif;
055000140813       // directory delle immagini
055100140813             %subst(o_testo:85:30) = skdir(yy);
055200110713       // Crea elenco a cliente
055300110713             IF skfile(yy) = 'S';
055400140813               %subst(o_testo:117:35) = 'PRIMA CREARE IL FILE!';
055500110713             ENDIF;
055600110713             except PrtDet;
055700090320           endif;
055800090319         endfor;
055900071212
056000071212       ENDSR;
056100071221
056200071221       //--------------------------------------------------------------
056300071221       //?Override al file di stampa per impostarvi i dati per
056400071221       //?  l'invio via e-mail   +   stampa inizio e-mail
056500071221       //--------------------------------------------------------------
056600071221       BEGSR sr_OpenPrtF;
056700071221
056800071221         // Override al file di stampa per impostarvi i dati per
056900071221         // l'invio via e-mail
057000071221           exsr sr_Override;
057100071221
057200071221           open PrtEMAIL;
057300071221
057400071221         // Stampa una testata se NON è richiesta la e-mail
057500071221           IF  §MRAdreg =  *blank;
057600071221             O_testo = JobUser + ' - ' + SDSpgm
057700071221                     + ' - ' + %editc( *date : 'Y' )
057800071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
057900071221             except PrtDet;
058000071221             clear O_testo;
058100071221             except PrtDet;
058200071221             except PrtDet;
058300071221           ENDIF;
058400071221
058500071221         // Stampa testo iniziale
058600121011           O_testo = 'Elenco clienti per i quali è stato +
058700121011                      preparato ed inviato via posta interna il CD dei +
058800121011                      Pod Image';
058900071221           except PrtDet;
059000090320
059100090320         // Stampa una riga vuota
059200090320           clear O_testo;
059300090320           except PrtDet;
059400090319
059500090319         // Stampa intestazione
059600121011           O_testo = 'Padre    Cliente  Ragione Sociale             Id Job' +
059700140813                     '  Elab.  Periodo Estrazione     Directory';
059800090319           except PrtDet;
059900071221
060000071221         // Stampa una riga vuota
060100071221           clear O_testo;
060200071221           except PrtDet;
060300071221
060400071221       ENDSR;
060500071219
060600071219       //--------------------------------------------------------------
060700071219       //?Override al file di stampa per impostarvi i dati per
060800071219       //?  l'invio via e-mail   +   stampa inizio e-mail
060900071219       //--------------------------------------------------------------
061000071219       BEGSR sr_Override;
061100071219
061200071220         reset $Invio;
061300071219         reset TRTCM1ds;
061400071221
061500071221         IF  §MRAdreg <> *blank;
061600080317           §CM1mitt = %trim(§MRAdmitt);
061700071221           §CM1dst  = %trim(§MRAddest);
061800071221           §CM1tips = §MRAdreg;
061900071227           §CM1po   = C_Sede;
062000071221           §CM1var  = '*OBJM*' + §MRAddes;
062100071221           §CM1idp  = §MRAdidpro;
062200071221           Qcmd = C_CmdOvrPrtF
062300071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
062400071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
062500071221         ELSE;
062600071221           Qcmd = C_CmdOvrPrtF;
062700071221         ENDIF;
062800071220
062900071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
063000071221         IF  §MRAdreg <> *blanks  and  not %error;
063100071221           $Invio = *on;
063200071221         ENDIF;
063300071219
063400071219       ENDSR;
063500071221
063600071221       //--------------------------------------------------------------
063700071221       //?Operazioni finali.
063800071221       //--------------------------------------------------------------
063900071221       BEGSR RoutEnd;
064000071221
064100071221         if %open(PrtEMAIL);
064200071221
064300071221         //?Chiusura dello spool?
064400071221           clear O_testo;
064500071221           except PrtDet;
064600121011           O_testo = 'Vi ricordiamo che il controllo del contenuto dei +
064700121011                      supporti prima che siano inviati ai clienti';
064800071221           except PrtDet;
064900121011           O_testo = 'è a vostro carico.';
065000121011           except PrtDet;
065100121011           O_testo = 'Vi chiediamo inoltre di proporre ai clienti di +
065200121011                      inviare i POD Image via FTP e non più tramite';
065300121011           except PrtDet;
065400121011           O_testo = 'supporto ottico (CD/DVD), facendo notare, ad +
065500121011                      esempio, la velocità con cui avrebbero a disposizione';
065600121011           except PrtDet;
065700121011           O_testo = 'le immagini rispetto al metodo attuale.';
065800121011           except PrtDet;
065900121011           clear O_testo;
066000121011           except PrtDet;
066100121011           O_testo = 'A disposizione per ogni chiarimento.';
066200121011           except PrtDet;
066300121011           clear O_testo;
066400121011           except PrtDet;
066500121011           O_testo = 'ICT di Sede';
066600121011           except PrtDet;
066700121011           O_testo = 'BRT SPA';
066800121011           except PrtDet;
066900071221
067000071221           close PrtEMAIL;
067100071221
067200071221         //?Eliminazione overflow?
067300071221           Qcmd = C_CmdDltOvr;
067400071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
067500071221
067600071221         endif;
067700071221
067800071221       ENDSR;
067900090305
068000090305       //--------------------------------------------------------------
068100090305       //?Aggiorna tabella LAC
068200090305       //--------------------------------------------------------------
068300090305       BEGSR sr_AggTbLAC;
068400090305
068500090305       // aggancio la tabella per aggiornarla
068600090309          chain (c_tab:tbeke1) tntbe01l;
068700090305          if %found(tntbe01l);
068800090305            dlac = tbeuni;
068900090318       // imposto la data prossima estrazione su tabella LAC
069000090320       // se presente
069100090320            if wdataal <> *zeros;
069200090320              w_dateiso = %date(wdataal) + %days(1);
069300090320              wdataal = %dec(w_dateiso);
069400090320              §lacdatela = %dec(%subst(%editc(wdataal:'X'):1:6):6:0);
069500090320            endif;
069600090320          tbeuni = dlac;
069700090320          update tntbe000;
069800090305          endif;
069900090305
070000090305       ENDSR;
070100071212
070200071212      /end-free
070300071219
070400071219      //---------------------------------------------------------------
070500071219      //?Spool di stampa (per e-mail).
070600071219      //---------------------------------------------------------------
070700071219     oPrtEMAIL  e            PRTdet      1
070800071219     o                       O_testo
070900071213
071000071219      //===============================================================
071100071219      //?Procedura per confronto data schedulata in tab. LAC e *date
071200071219      //===============================================================
071300071219     p CheckDataConSchGg...
071400071213     p                 b                   export
071500071213      *
071600071219     d CheckDataConSchGg...
071700071213     d                 pi             1a
071800071219     d   pi_LACschGG                  2s 0 const
071900071219     d   pi_LACschMM                  1a   const  dim(12)
072000071213     d   pi_Date                       d   const  datfmt(*iso)
072100071213     d   po_Esito                     1a   options(*nopass)
072200071213
072300071213       //--------------------------------------------------------------
072400071213       // Definizione variabili work.
072500071213       //--------------------------------------------------------------
072600071213       // - flags booleani
072700071213     d $Esito          s              1n   inz(*off)
072800071213
072900071213       //--------------------------------------------------------------
073000071213       // Procedura.
073100071213       //--------------------------------------------------------------
073200071213      /free
073300071213
073400071213       reset $Esito;
073500071213
073600071213       // verifica se invio schedulato per il giorno corrente
073700071213       SELECT;
073800071213
073900071213       // - nessuna schedulazione per il mese corrente
074000071219          WHEN pi_LACschMM(%subdt(pi_Date : *months)) = *blanks;
074100071219          // $Esito = *off;            (già così)
074200071213
074300071213       // - schedulazione in giornata max 28: giorno sempre valido
074400071219          WHEN pi_LACschGG <= 28   and
074500071219               pi_LACschGG =  %subdt(pi_Date : *days);
074600071213             $Esito = *on;
074700071213
074800071213       // - schedulazione in giornata > 28:
074900071213       //   da controllare per il mese in corso...
075000071219          WHEN pi_LACschGG >  28   and
075100071219              (pi_LACschGG = %subdt( pi_Date : *days )  or
075200071218              (%subdt( pi_Date + %days(1) : *months ) >
075300071218                 %subdt( pi_Date : *months )));
075400071213             $Esito = *on;
075500071213
075600071213          OTHER;
075700071219          // $Esito = *off;            (già così)
075800071213
075900071213       ENDSL;
076000071213
076100071213       if %parms > 3;
076200071213          po_Esito = $Esito;
076300071213       endif;
076400071213
076500071213       return $Esito;
076600071213
076700071213      /end-free
076800071213
076900071219     p CheckDataConSchGg...
077000071213     p                 e
