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
001700071219     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
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
003200071221     d                                           pagesize(66 132) +
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
005100090319
005200090319      // - schiere per memorizzare il JOB
005300090319     d skcliu          s                   like(§lacksu) dim(999) inz
005400090319     d skjobu          s                   like(o33nrf) dim(999) inz
005500071212
005600071212      //---------------------------------------------------------------
005700071212      //?Definizione aree dati.
005800071212      //---------------------------------------------------------------
005900071221
006000071221      // - Dati utente
006100071221     d §AzUte        e ds                  extname(AZUTE00F)
006200071221     d                                     dtaara
006300071221     d §DatiUte      e ds                  extname(dDatiUte)
006400071221     d                                     dtaara
006500071212
006600071212      //---------------------------------------------------------------
006700071212      //?Definizione strutture dati.
006800071212      //---------------------------------------------------------------
006900071219
007000071212      // - Status
007100071212     d Psds           sds
007200071213     d   SDSpgm          *proc
007300071213     d   JobUser             254    263                                         User name
007400071221
007500071221      // Parametri
007600071221     d kpjba         e ds
007700090304
007800090304      // richiamo al pgm batch
007900090304     d TNSB16ds        ds                  inz
008000090304     d   D16dcd                       8  0 inz
008100090304     d   D16dca                       8  0 inz
008200090304     d   D16ksc                       7  0 inz
008300090304     d   D16imm                       1    inz
008400090304     d   D16tad                       1    inz
008500090304     d   D16dsd                       8  0 inz
008600090304     d   D16dsa                       8  0 inz
008700090304     d   D16dir                      30    inz
008800090304     d   D16fmi                       2    inz
008900090304     d   d16ksu                       7  0 inz
009000090304     d   d16tadu                      1    inz
009100090304     d   d16fimp                      1    inz
009200090304     d   d16imp                      10  3 inz
009300090304     d   d16kscf                      7  0 inz
009400090304     d   d16ctr                       3    inz
009500090304     d   d16job                      16    inz
009600090310     d   d16tpt                       1    inz
009700090310     d   d16res                       1    inz
009800090310     d   d16rec                       1    inz
009900090310     d   d16csr                       1    inz
010000090310     d   d16ssr                       1    inz
010100090310     d   d16lnp                       3  0 inz
010200071212
010300071212      // - Tabella "LAC" = Clienti per ritorno immagini
010400071212     d dLAC          e ds                  inz
010500071213     d   $LACschMm                    1    overlay(dLAC : 71)
010600071213     d                                     inz  dim(12)
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
011900090305
012000090305      // - Numeratore
012100090305     d trul33ds      e ds                  inz
012200071219
012300071219      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
012400071219     d TRTCM1ds      e ds                  inz
012500071219      //   ·§CM1mitt = Indirizzo e-mail del mittente
012600120306     d   §CM1mitt    e                     inz('ced@brt.it')
012700071219      //   ·§CM1dst  = Indirizzo e-mail del destinatario
012800120306     d   §CM1dst     e                     inz('nicola.delledonne@brt.it')
012900071219      //   ·§CM1tips = Tipo lettera via e-mail:
013000071219      //    "LET" = testo allegato in corpo con logo
013100071219      //            (richiede righe libere iniziali per il logo)
013200071219      //    "COR" = testo integrato senza logo
013300071219      //            (non consente né UNDERLINE né HIGHLIGHT)
013400121011     d   §CM1tips    e                     inz('COI')
013500071219      //   ·§CM1po   = Filiale
013600071219     d   §CM1po      e                     inz('046')
013700071219      //   ·§CM1var  = Oggetto e-mail
013800071219     d   §CM1var     e                     inz('*OBJM*+
013900071219     d                                     Preparazione CD con immagin+
014000071219     d                                     i delle LdV da inviare ai c+
014100071219     d                                     lienti (v. tab. LAC)')
014200071219      //   ·§CM1sts  = Stato
014300071219     d   §CM1sts     e                     inz(*off)
014400071219      //   ·§CM1sts  = Id processo
014500071219     d   §CM1idp     e                     inz('2')
014600071218
014700071212      //---------------------------------------------------------------
014800071212      //?Definizione variabili globali.
014900071212      //---------------------------------------------------------------
015000071219
015100071212      // - Flags booleani
015200071213     d W_Esito         s              1n   inz(*off)
015300071219     d $Invio          s              1n   inz(*off)
015400071219
015500071219      // - Indici di schiera
015600090320     d xx              s              4  0 inz
015700090320     d yy              s              4  0 inz
015800090320     d zz              s              4  0 inz
015900071219
016000071219      // - Campi per QCMDEXC
016100071219     d Qcmd            s            512    inz
016200071219
016300071213      // - Campi di comodo
016400090320     d oldwksu         s                   like(§lacksu)
016500090304     d wdata           s              8  0 inz
016600090304     d wdatada         s              8  0 inz
016700090304     d wdataal         s              8  0 inz
016800090319     d wkeysk          s             14    inz
016900090320     d wksc            s                   like(§lacksu)
017000090320     d wksu            s                   like(§lacksu)
017100090319     d wora            s              6  0 inz
017200090327     d wtimehms        s               t   timfmt(*HMS)   inz
017300090708     d wjob            s              7  0
017400090304
017500090304      // - Data
017600090304     d W_dateISO       s               d   datfmt(*iso)
017700090304     d W_oggiISO       s               d   datfmt(*iso)  inz(*job)
017800071219
017900071218      // - Campi di stampa
018000071219     d O_testo         s            132    inz
018100071212
018200071212      //---------------------------------------------------------------
018300071213      //?Definizione procedure esterne.
018400071212      //---------------------------------------------------------------
018500071219
018600071221      // - Reperimento dati utente
018700071221     d tibs34r         pr                  extpgm('TIBS34R')
018800071221     d  tibs34ds                           likeds(TIBS34ds)
018900071221
019000071219      // - Verifica se cliente a cui inviare le immagini
019100071219     d CheckDataConSchGg...
019200071213     d                 pr             1a
019300071219     d   pi_LACschGG                  2s 0 const
019400071219     d   pi_LACschMM                  1a   const  dim(12)
019500071219     d   pi_Date                       d   const  datfmt(*iso)
019600071219     d   po_Esito                     1a   options(*nopass)
019700071219
019800071219      // - Esecuzione comando di sistema
019900071219     d qCmdExc         pr                  extpgm('QCMDEXC')
020000071219     d  Qcmd                        512    const  options(*varsize)
020100071219     d  Qlen                         15  5 const
020200071221
020300071221      // - Gestione tabelle
020400071221     d tibs02r         pr                  extpgm('TIBS02R')
020500071221     d  kpjba                              likeds(KPJBA)    const
020600071221     d  tibs02ds                           likeds(TIBS02ds)
020700090304
020800090304      // - Sottomissione lavoro batch
020900090304     d bch10           pr                  extpgm('BCH10')
021000090304     d  kpjba                              likeds(KPJBA)
021100090304
021200090304      // - Richiamo diretto lavoro batch
021300090304     d tnsb16r         pr                  extpgm('TNSB16R')
021400090304     d  kpjba                              likeds(KPJBA)
021500090304
021600090304      /copy gaitrasrc/srcprotopr,trul33r
021700071212
021800071212      //---------------------------------------------------------------
021900071212      //?Definizione key-list.
022000071212      //---------------------------------------------------------------
022100071212
022200071212      //---------------------------------------------------------------
022300071212      //?Main.
022400071212      //---------------------------------------------------------------
022500071212     c     *Entry        plist
022600071221     c                   parm                    KPJBA
022700071212
022800071212      /free
022900071212
023000071212       // Operazioni iniziali
023100071212       exsr RoutInz;
023200071212
023300071221       // Creazione elenco clienti (se reperita tab. "MRA")
023400071221       exsr Elab_TNTBE;
023500090319
023600090319       // Invio Mail
023700090326       // se ho dei clienti caricati nella sk
023800090326       if skuni(1) <> *blanks;
023900090326         exsr sr_Prep_eMail;
024000071221
024100071221       // Operazioni finali
024200090326       // di chiusura mail
024300090326         exsr RoutEnd;
024400090326       endif;
024500071212
024600071212       //--------------------------------------------------------------
024700071212       //?Operazioni iniziali.
024800071212       //--------------------------------------------------------------
024900071212       BEGSR RoutInz;
025000071219
025100071219         *inLR = *on;
025200071221
025300071221       // Reperimento dati del lavoro
025400071221         exsr DatiJob;
025500071221
025600071221       // Reperimento tab. "MRA"
025700071221         clear dMRAdan;
025800071221         clear kpjbu;
025900071221         reset tibs02ds;
026000071221         T02ke1 = SDSpgm;
026100071221         tibs02r(kpjba:tibs02ds);
026200071221         if T02err = *blank;
026300071221           dMRAdan = T02uni;
026400071221         endif;
026500090319
026600090319       // ora Job
026700090319         wora = %dec(%time());
026800090327       // time Job
026900090327         wtimehms = %time();
027000071212
027100071212       ENDSR;
027200071221
027300071221       //--------------------------------------------------------------
027400071221       //?Reperimento Dati del job (Utente/Operativi).
027500071221       //--------------------------------------------------------------
027600071221       BEGSR DatiJob;
027700071221
027800071221         in(E) §AzUte;
027900071221         if NOT %error;
028000071221           in(E) §DatiUte;
028100071221         endif;
028200071221         if %error or RSut = *blanks;
028300071221           clear TIBS34ds;
028400071221           tibs34r(tibs34ds);
028500071221           in §AzUte;
028600071221           in §DatiUte;
028700071221         endif;
028800071221
028900071221       ENDSR;
029000071212
029100071212       //--------------------------------------------------------------
029200071221       //?Elaborazione clienti a cui inviare le immagini
029300071212       //--------------------------------------------------------------
029400071221       BEGSR Elab_TNTBE;
029500071212
029600071212       //?Ciclo di lettura tab. "LAC"?
029700071212
029800071212         setll C_tab TNTBE000;
029900090305         reade(n) C_tab TNTBE000;
030000071212
030100071212         DOW not %eof(TNTBE01L);
030200071212
030300071212         //?Elaborazione singolo cliente?
030400071212
030500071212           dLAC = TBEuni;
030600071212           IF TBEatb    = *blank   and
030700071213              §LACschGg > *zero;
030800071212
030900071221             // Segnalazione cliente per invio e-mail
031000071219             if  CheckDataConSchGg( §LACschGg :
031100071219                                    $LACschMm :
031200090304                                    W_oggiISO :
031300071219                                    W_Esito ) = *on;
031400090305               exsr sr_Batch;
031500100121             // Aggiorno tabella se ho sottomesso il lavoro
031600100121               IF  §lacaut = 'S';
031700100121                 exsr sr_AggTbLAC;
031800100121               ENDIF;
031900071212             endif;
032000071212
032100071212           ENDIF;
032200071212
032300090305           reade(n) C_tab TNTBE000;
032400071212
032500071212         ENDDO;
032600071212
032700071212       ENDSR;
032800090319
032900090319       //--------------------------------------------------------------
033000090319       //?Sottometto lavoro BACTH per estrarre le immagini
033100090319       //--------------------------------------------------------------
033200090319       BEGSR sr_Batch;
033300090319
033400090319         clear tnsb16ds;
033500090319
033600090319       // imposto la data "da" e la data "al"
033700090320       // se presenti
033800090320         if §lacdatai <> *zeros;
033900090319       // - se non ho mai elaborato parto dalla data prima estrazione
034000090319       //   assoluta
034100090320           if §lacdatela = *zeros;
034200090320             wdata = §lacdatai*100+1;
034300090320           endif;
034400090319       // - se ho già elaborato parto dalla data prossima estrazione
034500090320           if §lacdatela <> *zeros;
034600090320             wdata = §lacdatela*100+1;
034700090320           endif;
034800090319       // - imposto le date
034900090320           wdatada = wdata;
035000090320           w_dateiso = %date(wdatada) + %months(§lacmesi) - %days(1);
035100090320           wdataal = %dec(w_dateiso);
035200090320           if §lactdta = 'C';
035300090320             d16dcd = wdatada;
035400090320             d16dca = wdataal;
035500090320           endif;
035600090320           if §lactdta = 'S';
035700090320             d16dsd = wdatada;
035800090320             d16dsa = wdataal;
035900090320           endif;
036000090320         else;
036100090320           clear wdatada;
036200090320           clear wdataal;
036300090320         endif;
036400090319
036500090319       // cliente da elaborare
036600090319         d16ksc = %dec(%subst(tbeke1:1:7):7:0);
036700090319
036800090319       // immagini da elaborare
036900090319         d16imm = 'E';
037000090319
037100090319       // creazione addbito
037200090319         d16tad = §lactad;
037300090319
037400090319       // directory dove memorizzare le immagini
037500090319       // - se nella directory ci sono &AAA o &MM sono da sostituire
037600090319         d16dir = §lacdir;
037700090319         if %scan('&AAA':d16dir) > *zeros;
037800090319           d16dir = %replace( %subst( %editc(wdatada:'X'):1:4) :
037900090319                    d16dir : %scan('&AAA':d16dir) );
038000090319         endif;
038100090319         if %scan('&M':d16dir) > *zeros;
038200090319           d16dir = %replace( %subst( %editc(wdatada:'X'):5:2) :
038300090319                    d16dir : %scan('&M':d16dir) );
038400090319         endif;
038500090319         if %scan('&AAA':d16dir) > *zeros;
038600090319           d16dir = %replace( %subst( %editc(wdataal:'X'):1:4) :
038700090319                    d16dir : %scan('&AAA':d16dir) );
038800090319         endif;
038900090319         if %scan('&M':d16dir) > *zeros;
039000090319           d16dir = %replace( %subst( %editc(wdataal:'X'):5:2) :
039100090319                    d16dir : %scan('&M':d16dir) );
039200090319         endif;
039300090319
039400090319       // nome immagine
039500090319         d16fmi = §lacfmi;
039600090319
039700090319       // codice raggruppamento;
039800090319         d16ksu = §lacksu;
039900090319
040000090319       // frequenza addebito;
040100090319         d16tadu = §lactadu;
040200090319
040300090319       // tipo importo forzato e importo forzato
040400090319         d16fimp = §lacfimp;
040500090319         d16imp  = §lacimp;
040600090319
040700090319       // cliente e codice tariffa forzati
040800090319         d16kscf = §lacksc;
040900090319         d16ctr  = §lacctr;
041000090319
041100090319       // tipo porto
041200090319           d16tpt = §lactpt;
041300090319
041400090319       // resi
041500090319           d16res = §lacres;
041600090319
041700090319       // recuperi
041800090319           d16rec = §lacrec;
041900090319
042000090319       // c/servizi
042100090319           d16csr = §laccsr;
042200090319
042300090319       // bolle con serie
042400090319           d16ssr = §lacssr;
042500090319
042600090319       // linea di partenza
042700090319           d16lnp = §laclnp;
042800090319
042900090319       // memorizzo in sk i dati per la mail
043000090320         wkeysk = %editc(§lacksu:'X') + %editc(d16ksc:'X');
043100090319         xx = %lookup(*zeros:skksc);
043200090319         skuni(xx) = wkeysk;
043300090319         skksc(xx) = d16ksc;
043400090319         skrag(xx) = §lacrag;
043500090319         skaut(xx) = §lacaut;
043600090319         skdal(xx) = wdatada;
043700090319         skal(xx)  = wdataal;
043800110713         clear skfile(xx);
043900110713         IF  §LACpgm <> *blanks;
044000110713           skfile(xx) = 'S';
044100110713         ENDIF;
044200090319
044300090319       // se devo sottomettere
044400090319         if §lacaut = 'S';
044500090319       // ID Job
044600090319       // - per ogni §lacksu devo staccare un numero JOB
044700090319       // - e memorizzo in schiera
044800090319           yy = %lookup(§lacksu:skcliu);
044900090319           if yy > *zeros;
045000090319             d16job = %char(skjobu(yy));
045100090319           else;
045200090319             yy = %lookup(*zeros:skcliu);
045300090319             clear trul33ds;
045400090319             I33OPE = *zeros;
045500090319             I33CNU = 400;
045600090319             I33NUM = 1;
045700090319             KPJBU = trul33ds;
045800090319             trul33r (KPJBA);
045900090319             trul33ds = KPJBU;
046000090319             skcliu(yy) = §lacksu;
046100090319             skjobu(yy) = o33nrf;
046200090319             d16job = %char(skjobu(yy));
046300090407          // scrivo rcd spia su tilac con lacela = '33'
046400090407             clear tilac000;
046500090407             lactim = %char(%timestamp:*iso0);
046600090407             lacdir = d16dir;
046700090407             lacela = '33';
046800090407             lactela = 'S';
046900090407             lacksu = d16ksu;
047000090407             lactad = d16tad;
047100090407             lacidjob = d16job;
047200090407             write tilac000;
047300090319           endif;
047400090319
047500090319       // sottometto il lavoro
047600090319            kcoaz = 'SB16';
047700090319            kpjbu = TNSB16ds;
047800090715            kbuff = *blank;
047900090319            if  knmus = *all'1';
048000090319              tnsb16r(kpjba);
048100090319            else;
048200090319              BCH10(kpjba);
048300090319            endif;
048400090319
048500090319          endif;
048600090319
048700090319       ENDSR;
048800071212
048900071212       //--------------------------------------------------------------
049000071219       //?Preparazione e-mail con elenco clienti a cui inviare i dati
049100071212       //--------------------------------------------------------------
049200071219       BEGSR sr_Prep_eMail;
049300071212
049400071221       // Override al file di stampa ed apertura dello stesso
049500071221         if not %open(PRTEMAIL);
049600071221           exsr sr_OpenPrtF;
049700071221         endif;
049800071219
049900090319       // Sorto la sk a key unica
050000090319         sorta skuni;
050100090319
050200090319       // Leggo le sk dei clienti elaborati
050300090319         xx = 1;
050400090320         for xx by 1 to %elem(skuni);
050500090320           clear o_testo;
050600090319           if skuni(xx) <> *blanks;
050700090319             wksc = %dec(%subst(skuni(xx):8:7):7:0);
050800090319             wksu = %dec(%subst(skuni(xx):1:7):7:0);
050900090319             yy = %lookup(wksc:skksc);
051000090320             zz = %lookup(wksu:skcliu);
051100090320       // stampo papà solo se diverso
051200090320             if wksu <> oldwksu;
051300090320               o_testo = %editc(wksu:'X');
051400090320               oldwksu = wksu;
051500090320             endif;
051600090320       // cliente
051700090325             %subst(o_testo:10:7) = %editc(skksc(yy):'X');
051800090320       // ragione sociale
051900090325             %subst(o_testo:19:25) = skrag(yy);
052000090320       // id job se presente
052100090320             if skaut(yy) = 'S';
052200090708               wjob = skjobu(zz);
052300090708               %subst(o_testo:46:7) = %editc(wjob:'Z');
052400090320             endif;
052500090320       // tipo elaborazione
052600090325             %subst(o_testo:56:1) = skaut(yy);
052700090320       // periodo estrazione se presente
052800090320             if skdal(yy) <> *zeros;
052900090325               %subst(o_testo:62:10) =
053000090320                         %subst(%editc(%dec(skdal(yy)):'X'):7:2) +
053100090320                         '/' +
053200090320                         %subst(%editc(%dec(skdal(yy)):'X'):5:2) +
053300090320                         '/' +
053400090320                         %subst(%editc(%dec(skdal(yy)):'X'):1:4);
053500090320             endif;
053600090320             if skal(yy) <> *zeros;
053700090325               %subst(o_testo:73:10) =
053800090320                       %subst(%editc(%dec(skal(yy)):'X'):7:2) +
053900090319                       '/' +
054000090320                       %subst(%editc(%dec(skal(yy)):'X'):5:2) +
054100090319                       '/' +
054200090320                       %subst(%editc(%dec(skal(yy)):'X'):1:4);
054300090320             endif;
054400090320       // se elaborazione = nessuna avviso
054500090319             if skaut(yy) = 'N';
054600090325               %subst(o_testo:62:12) = 'VERIFICARE!!';
054700090319             endif;
054800110713       // Crea elenco a cliente
054900110713             IF skfile(yy) = 'S';
055000121011               %subst(o_testo:85:35) = 'PRIMA CREARE IL FILE!';
055100110713             ENDIF;
055200110713             except PrtDet;
055300090320           endif;
055400090319         endfor;
055500071212
055600071212       ENDSR;
055700071221
055800071221       //--------------------------------------------------------------
055900071221       //?Override al file di stampa per impostarvi i dati per
056000071221       //?  l'invio via e-mail   +   stampa inizio e-mail
056100071221       //--------------------------------------------------------------
056200071221       BEGSR sr_OpenPrtF;
056300071221
056400071221         // Override al file di stampa per impostarvi i dati per
056500071221         // l'invio via e-mail
056600071221           exsr sr_Override;
056700071221
056800071221           open PrtEMAIL;
056900071221
057000071221         // Stampa una testata se NON è richiesta la e-mail
057100071221           IF  §MRAdreg =  *blank;
057200071221             O_testo = JobUser + ' - ' + SDSpgm
057300071221                     + ' - ' + %editc( *date : 'Y' )
057400071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
057500071221             except PrtDet;
057600071221             clear O_testo;
057700071221             except PrtDet;
057800071221             except PrtDet;
057900071221           ENDIF;
058000071221
058100071221         // Stampa testo iniziale
058200121011           O_testo = 'Elenco clienti per i quali è stato +
058300121011                      preparato ed inviato via posta interna il CD dei +
058400121011                      Pod Image';
058500071221           except PrtDet;
058600090320
058700090320         // Stampa una riga vuota
058800090320           clear O_testo;
058900090320           except PrtDet;
059000090319
059100090319         // Stampa intestazione
059200121011           O_testo = 'Padre    Cliente  Ragione Sociale             Id Job' +
059300121011                     '  Elab.  Periodo Estrazione';
059400090319           except PrtDet;
059500071221
059600071221         // Stampa una riga vuota
059700071221           clear O_testo;
059800071221           except PrtDet;
059900071221
060000071221       ENDSR;
060100071219
060200071219       //--------------------------------------------------------------
060300071219       //?Override al file di stampa per impostarvi i dati per
060400071219       //?  l'invio via e-mail   +   stampa inizio e-mail
060500071219       //--------------------------------------------------------------
060600071219       BEGSR sr_Override;
060700071219
060800071220         reset $Invio;
060900071219         reset TRTCM1ds;
061000071221
061100071221         IF  §MRAdreg <> *blank;
061200080317           §CM1mitt = %trim(§MRAdmitt);
061300071221           §CM1dst  = %trim(§MRAddest);
061400071221           §CM1tips = §MRAdreg;
061500071227           §CM1po   = C_Sede;
061600071221           §CM1var  = '*OBJM*' + §MRAddes;
061700071221           §CM1idp  = §MRAdidpro;
061800071221           Qcmd = C_CmdOvrPrtF
061900071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
062000071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
062100071221         ELSE;
062200071221           Qcmd = C_CmdOvrPrtF;
062300071221         ENDIF;
062400071220
062500071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
062600071221         IF  §MRAdreg <> *blanks  and  not %error;
062700071221           $Invio = *on;
062800071221         ENDIF;
062900071219
063000071219       ENDSR;
063100071221
063200071221       //--------------------------------------------------------------
063300071221       //?Operazioni finali.
063400071221       //--------------------------------------------------------------
063500071221       BEGSR RoutEnd;
063600071221
063700071221         if %open(PrtEMAIL);
063800071221
063900071221         //?Chiusura dello spool?
064000071221           clear O_testo;
064100071221           except PrtDet;
064200121011           O_testo = 'Vi ricordiamo che il controllo del contenuto dei +
064300121011                      supporti prima che siano inviati ai clienti';
064400071221           except PrtDet;
064500121011           O_testo = 'è a vostro carico.';
064600121011           except PrtDet;
064700121011           O_testo = 'Vi chiediamo inoltre di proporre ai clienti di +
064800121011                      inviare i POD Image via FTP e non più tramite';
064900121011           except PrtDet;
065000121011           O_testo = 'supporto ottico (CD/DVD), facendo notare, ad +
065100121011                      esempio, la velocità con cui avrebbero a disposizione';
065200121011           except PrtDet;
065300121011           O_testo = 'le immagini rispetto al metodo attuale.';
065400121011           except PrtDet;
065500121011           clear O_testo;
065600121011           except PrtDet;
065700121011           O_testo = 'A disposizione per ogni chiarimento.';
065800121011           except PrtDet;
065900121011           clear O_testo;
066000121011           except PrtDet;
066100121011           O_testo = 'ICT di Sede';
066200121011           except PrtDet;
066300121011           O_testo = 'BRT SPA';
066400121011           except PrtDet;
066500071221
066600071221           close PrtEMAIL;
066700071221
066800071221         //?Eliminazione overflow?
066900071221           Qcmd = C_CmdDltOvr;
067000071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
067100071221
067200071221         endif;
067300071221
067400071221       ENDSR;
067500090305
067600090305       //--------------------------------------------------------------
067700090305       //?Aggiorna tabella LAC
067800090305       //--------------------------------------------------------------
067900090305       BEGSR sr_AggTbLAC;
068000090305
068100090305       // aggancio la tabella per aggiornarla
068200090309          chain (c_tab:tbeke1) tntbe01l;
068300090305          if %found(tntbe01l);
068400090305            dlac = tbeuni;
068500090318       // imposto la data prossima estrazione su tabella LAC
068600090320       // se presente
068700090320            if wdataal <> *zeros;
068800090320              w_dateiso = %date(wdataal) + %days(1);
068900090320              wdataal = %dec(w_dateiso);
069000090320              §lacdatela = %dec(%subst(%editc(wdataal:'X'):1:6):6:0);
069100090320            endif;
069200090320          tbeuni = dlac;
069300090320          update tntbe000;
069400090305          endif;
069500090305
069600090305       ENDSR;
069700071212
069800071212      /end-free
069900071219
070000071219      //---------------------------------------------------------------
070100071219      //?Spool di stampa (per e-mail).
070200071219      //---------------------------------------------------------------
070300071219     oPrtEMAIL  e            PRTdet      1
070400071219     o                       O_testo
070500071213
070600071219      //===============================================================
070700071219      //?Procedura per confronto data schedulata in tab. LAC e *date
070800071219      //===============================================================
070900071219     p CheckDataConSchGg...
071000071213     p                 b                   export
071100071213      *
071200071219     d CheckDataConSchGg...
071300071213     d                 pi             1a
071400071219     d   pi_LACschGG                  2s 0 const
071500071219     d   pi_LACschMM                  1a   const  dim(12)
071600071213     d   pi_Date                       d   const  datfmt(*iso)
071700071213     d   po_Esito                     1a   options(*nopass)
071800071213
071900071213       //--------------------------------------------------------------
072000071213       // Definizione variabili work.
072100071213       //--------------------------------------------------------------
072200071213       // - flags booleani
072300071213     d $Esito          s              1n   inz(*off)
072400071213
072500071213       //--------------------------------------------------------------
072600071213       // Procedura.
072700071213       //--------------------------------------------------------------
072800071213      /free
072900071213
073000071213       reset $Esito;
073100071213
073200071213       // verifica se invio schedulato per il giorno corrente
073300071213       SELECT;
073400071213
073500071213       // - nessuna schedulazione per il mese corrente
073600071219          WHEN pi_LACschMM(%subdt(pi_Date : *months)) = *blanks;
073700071219          // $Esito = *off;            (già così)
073800071213
073900071213       // - schedulazione in giornata max 28: giorno sempre valido
074000071219          WHEN pi_LACschGG <= 28   and
074100071219               pi_LACschGG =  %subdt(pi_Date : *days);
074200071213             $Esito = *on;
074300071213
074400071213       // - schedulazione in giornata > 28:
074500071213       //   da controllare per il mese in corso...
074600071219          WHEN pi_LACschGG >  28   and
074700071219              (pi_LACschGG = %subdt( pi_Date : *days )  or
074800071218              (%subdt( pi_Date + %days(1) : *months ) >
074900071218                 %subdt( pi_Date : *months )));
075000071213             $Esito = *on;
075100071213
075200071213          OTHER;
075300071219          // $Esito = *off;            (già così)
075400071213
075500071213       ENDSL;
075600071213
075700071213       if %parms > 3;
075800071213          po_Esito = $Esito;
075900071213       endif;
076000071213
076100071213       return $Esito;
076200071213
076300071213      /end-free
076400071213
076500071219     p CheckDataConSchGg...
076600071213     p                 e
