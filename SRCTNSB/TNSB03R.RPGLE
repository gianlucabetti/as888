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
017300180207     d wpos            s              5  0
017400180207     d wpos1           s              5  0
017500090327     d wtimehms        s               t   timfmt(*HMS)   inz
017600090708     d wjob            s              7  0
017700090304
017800090304      // - Data
017900090304     d W_dateISO       s               d   datfmt(*iso)
018000090304     d W_oggiISO       s               d   datfmt(*iso)  inz(*job)
018100071219
018200071218      // - Campi di stampa
018300140813     d O_testo         s            198    inz
018400071212
018500071212      //---------------------------------------------------------------
018600071213      //?Definizione procedure esterne.
018700071212      //---------------------------------------------------------------
018800071219
018900071221      // - Reperimento dati utente
019000071221     d tibs34r         pr                  extpgm('TIBS34R')
019100071221     d  tibs34ds                           likeds(TIBS34ds)
019200071221
019300071219      // - Verifica se cliente a cui inviare le immagini
019400071219     d CheckDataConSchGg...
019500071213     d                 pr             1a
019600071219     d   pi_LACschGG                  2s 0 const
019700071219     d   pi_LACschMM                  1a   const  dim(12)
019800071219     d   pi_Date                       d   const  datfmt(*iso)
019900071219     d   po_Esito                     1a   options(*nopass)
020000071219
020100071219      // - Esecuzione comando di sistema
020200071219     d qCmdExc         pr                  extpgm('QCMDEXC')
020300071219     d  Qcmd                        512    const  options(*varsize)
020400071219     d  Qlen                         15  5 const
020500071221
020600071221      // - Gestione tabelle
020700071221     d tibs02r         pr                  extpgm('TIBS02R')
020800071221     d  kpjba                              likeds(KPJBA)    const
020900071221     d  tibs02ds                           likeds(TIBS02ds)
021000090304
021100090304      // - Sottomissione lavoro batch
021200090304     d bch10           pr                  extpgm('BCH10')
021300090304     d  kpjba                              likeds(KPJBA)
021400090304
021500090304      // - Richiamo diretto lavoro batch
021600090304     d tnsb16r         pr                  extpgm('TNSB16R')
021700090304     d  kpjba                              likeds(KPJBA)
021800090304
021900090304      /copy gaitrasrc/srcprotopr,trul33r
022000071212
022100071212      //---------------------------------------------------------------
022200071212      //?Definizione key-list.
022300071212      //---------------------------------------------------------------
022400071212
022500071212      //---------------------------------------------------------------
022600071212      //?Main.
022700071212      //---------------------------------------------------------------
022800071212     c     *Entry        plist
022900071221     c                   parm                    KPJBA
023000071212
023100071212      /free
023200071212
023300071212       // Operazioni iniziali
023400071212       exsr RoutInz;
023500071212
023600071221       // Creazione elenco clienti (se reperita tab. "MRA")
023700071221       exsr Elab_TNTBE;
023800090319
023900090319       // Invio Mail
024000090326       // se ho dei clienti caricati nella sk
024100090326       if skuni(1) <> *blanks;
024200090326         exsr sr_Prep_eMail;
024300071221
024400071221       // Operazioni finali
024500090326       // di chiusura mail
024600090326         exsr RoutEnd;
024700090326       endif;
024800071212
024900071212       //--------------------------------------------------------------
025000071212       //?Operazioni iniziali.
025100071212       //--------------------------------------------------------------
025200071212       BEGSR RoutInz;
025300071219
025400071219         *inLR = *on;
025500071221
025600071221       // Reperimento dati del lavoro
025700071221         exsr DatiJob;
025800071221
025900071221       // Reperimento tab. "MRA"
026000071221         clear dMRAdan;
026100071221         clear kpjbu;
026200071221         reset tibs02ds;
026300071221         T02ke1 = SDSpgm;
026400071221         tibs02r(kpjba:tibs02ds);
026500071221         if T02err = *blank;
026600071221           dMRAdan = T02uni;
026700071221         endif;
026800090319
026900090319       // ora Job
027000090319         wora = %dec(%time());
027100090327       // time Job
027200090327         wtimehms = %time();
027300071212
027400071212       ENDSR;
027500071221
027600071221       //--------------------------------------------------------------
027700071221       //?Reperimento Dati del job (Utente/Operativi).
027800071221       //--------------------------------------------------------------
027900071221       BEGSR DatiJob;
028000071221
028100071221         in(E) §AzUte;
028200071221         if NOT %error;
028300071221           in(E) §DatiUte;
028400071221         endif;
028500071221         if %error or RSut = *blanks;
028600071221           clear TIBS34ds;
028700071221           tibs34r(tibs34ds);
028800071221           in §AzUte;
028900071221           in §DatiUte;
029000071221         endif;
029100071221
029200071221       ENDSR;
029300071212
029400071212       //--------------------------------------------------------------
029500071221       //?Elaborazione clienti a cui inviare le immagini
029600071212       //--------------------------------------------------------------
029700071221       BEGSR Elab_TNTBE;
029800071212
029900071212       //?Ciclo di lettura tab. "LAC"?
030000071212
030100071212         setll C_tab TNTBE000;
030200090305         reade(n) C_tab TNTBE000;
030300071212
030400071212         DOW not %eof(TNTBE01L);
030500071212
030600071212         //?Elaborazione singolo cliente?
030700071212
030800071212           dLAC = TBEuni;
030900071212           IF TBEatb    = *blank   and
031000071213              §LACschGg > *zero;
031100071212
031200071221             // Segnalazione cliente per invio e-mail
031300071219             if  CheckDataConSchGg( §LACschGg :
031400071219                                    $LACschMm :
031500090304                                    W_oggiISO :
031600071219                                    W_Esito ) = *on;
031700090305               exsr sr_Batch;
031800100121             // Aggiorno tabella se ho sottomesso il lavoro
031900100121               IF  §lacaut = 'S';
032000100121                 exsr sr_AggTbLAC;
032100100121               ENDIF;
032200071212             endif;
032300071212
032400071212           ENDIF;
032500071212
032600090305           reade(n) C_tab TNTBE000;
032700071212
032800071212         ENDDO;
032900071212
033000071212       ENDSR;
033100090319
033200090319       //--------------------------------------------------------------
033300090319       //?Sottometto lavoro BACTH per estrarre le immagini
033400090319       //--------------------------------------------------------------
033500090319       BEGSR sr_Batch;
033600090319
033700090319         clear tnsb16ds;
033800090319
033900090319       // imposto la data "da" e la data "al"
034000090320       // se presenti
034100090320         if §lacdatai <> *zeros;
034200090319       // - se non ho mai elaborato parto dalla data prima estrazione
034300090319       //   assoluta
034400090320           if §lacdatela = *zeros;
034500090320             wdata = §lacdatai*100+1;
034600090320           endif;
034700090319       // - se ho già elaborato parto dalla data prossima estrazione
034800090320           if §lacdatela <> *zeros;
034900090320             wdata = §lacdatela*100+1;
035000090320           endif;
035100090319       // - imposto le date
035200090320           wdatada = wdata;
035300090320           w_dateiso = %date(wdatada) + %months(§lacmesi) - %days(1);
035400090320           wdataal = %dec(w_dateiso);
035500090320           if §lactdta = 'C';
035600090320             d16dcd = wdatada;
035700090320             d16dca = wdataal;
035800090320           endif;
035900090320           if §lactdta = 'S';
036000090320             d16dsd = wdatada;
036100090320             d16dsa = wdataal;
036200090320           endif;
036300090320         else;
036400090320           clear wdatada;
036500090320           clear wdataal;
036600090320         endif;
036700090319
036800090319       // cliente da elaborare
036900090319         d16ksc = %dec(%subst(tbeke1:1:7):7:0);
037000090319
037100090319       // immagini da elaborare
037200090319         d16imm = 'E';
037300090319
037400090319       // creazione addbito
037500090319         d16tad = §lactad;
037600090319
037700090319       // directory dove memorizzare le immagini
037800180207         d16dir = §lacdir;
037900180207       // - come prima cosa stabilisco la directory da masterizzare
038000180207       //   visto che in tutte le dir è presente l'anno prendo come
038100180207       //   percorso da masterizzare tutte quanto fino all'anno
038200180207       //   quello dopo l'anno non lo devo prendere
038300180207         clear wpos;
038400180207         clear wpos1;
038500180207         wpos = %scan('&AAA':D16dir);
038600180207         IF  wpos > 0;
038700180207           wpos1 = %scan('/':D16dir:wpos+1);
038800180207         ENDIF;
038900180207         IF  wpos1 > 0;
039000180207           D16dir = %subst(D16dir:1:wpos1-1);
039100180207         ENDIF;
039200180207
039300090319       // - se nella directory ci sono &AAA o &MM sono da sostituire
039400180207         //d16dir = §lacdir;
039500090319         if %scan('&AAA':d16dir) > *zeros;
039600090319           d16dir = %replace( %subst( %editc(wdatada:'X'):1:4) :
039700090319                    d16dir : %scan('&AAA':d16dir) );
039800090319         endif;
039900090319         if %scan('&M':d16dir) > *zeros;
040000090319           d16dir = %replace( %subst( %editc(wdatada:'X'):5:2) :
040100090319                    d16dir : %scan('&M':d16dir) );
040200090319         endif;
040300090319         if %scan('&AAA':d16dir) > *zeros;
040400090319           d16dir = %replace( %subst( %editc(wdataal:'X'):1:4) :
040500090319                    d16dir : %scan('&AAA':d16dir) );
040600090319         endif;
040700090319         if %scan('&M':d16dir) > *zeros;
040800090319           d16dir = %replace( %subst( %editc(wdataal:'X'):5:2) :
040900090319                    d16dir : %scan('&M':d16dir) );
041000090319         endif;
041100090319
041200090319       // nome immagine
041300090319         d16fmi = §lacfmi;
041400090319
041500090319       // codice raggruppamento;
041600090319         d16ksu = §lacksu;
041700090319
041800090319       // frequenza addebito;
041900090319         d16tadu = §lactadu;
042000090319
042100090319       // tipo importo forzato e importo forzato
042200090319         d16fimp = §lacfimp;
042300090319         d16imp  = §lacimp;
042400090319
042500090319       // cliente e codice tariffa forzati
042600090319         d16kscf = §lacksc;
042700090319         d16ctr  = §lacctr;
042800090319
042900090319       // tipo porto
043000090319           d16tpt = §lactpt;
043100090319
043200090319       // resi
043300090319           d16res = §lacres;
043400090319
043500090319       // recuperi
043600090319           d16rec = §lacrec;
043700090319
043800090319       // c/servizi
043900090319           d16csr = §laccsr;
044000090319
044100090319       // bolle con serie
044200090319           d16ssr = §lacssr;
044300090319
044400090319       // linea di partenza
044500090319           d16lnp = §laclnp;
044600090319
044700090319       // memorizzo in sk i dati per la mail
044800090320         wkeysk = %editc(§lacksu:'X') + %editc(d16ksc:'X');
044900090319         xx = %lookup(*zeros:skksc);
045000090319         skuni(xx) = wkeysk;
045100090319         skksc(xx) = d16ksc;
045200090319         skrag(xx) = §lacrag;
045300090319         skaut(xx) = §lacaut;
045400090319         skdal(xx) = wdatada;
045500090319         skal(xx)  = wdataal;
045600140813         skdir(xx) = d16dir;
045700110713         clear skfile(xx);
045800110713         IF  §LACpgm <> *blanks;
045900110713           skfile(xx) = 'S';
046000110713         ENDIF;
046100090319
046200090319       // se devo sottomettere
046300090319         if §lacaut = 'S';
046400090319       // ID Job
046500090319       // - per ogni §lacksu devo staccare un numero JOB
046600090319       // - e memorizzo in schiera
046700090319           yy = %lookup(§lacksu:skcliu);
046800090319           if yy > *zeros;
046900090319             d16job = %char(skjobu(yy));
047000090319           else;
047100090319             yy = %lookup(*zeros:skcliu);
047200090319             clear trul33ds;
047300090319             I33OPE = *zeros;
047400090319             I33CNU = 400;
047500090319             I33NUM = 1;
047600090319             KPJBU = trul33ds;
047700090319             trul33r (KPJBA);
047800090319             trul33ds = KPJBU;
047900090319             skcliu(yy) = §lacksu;
048000090319             skjobu(yy) = o33nrf;
048100090319             d16job = %char(skjobu(yy));
048200090407          // scrivo rcd spia su tilac con lacela = '33'
048300090407             clear tilac000;
048400090407             lactim = %char(%timestamp:*iso0);
048500090407             lacdir = d16dir;
048600090407             lacela = '33';
048700090407             lactela = 'S';
048800090407             lacksu = d16ksu;
048900090407             lactad = d16tad;
049000090407             lacidjob = d16job;
049100090407             write tilac000;
049200090319           endif;
049300090319
049400090319       // sottometto il lavoro
049500090319            kcoaz = 'SB16';
049600090319            kpjbu = TNSB16ds;
049700090715            kbuff = *blank;
049800090319            if  knmus = *all'1';
049900090319              tnsb16r(kpjba);
050000090319            else;
050100090319              BCH10(kpjba);
050200090319            endif;
050300090319
050400090319          endif;
050500090319
050600090319       ENDSR;
050700071212
050800071212       //--------------------------------------------------------------
050900071219       //?Preparazione e-mail con elenco clienti a cui inviare i dati
051000071212       //--------------------------------------------------------------
051100071219       BEGSR sr_Prep_eMail;
051200071212
051300071221       // Override al file di stampa ed apertura dello stesso
051400071221         if not %open(PRTEMAIL);
051500071221           exsr sr_OpenPrtF;
051600071221         endif;
051700071219
051800090319       // Sorto la sk a key unica
051900090319         sorta skuni;
052000090319
052100090319       // Leggo le sk dei clienti elaborati
052200090319         xx = 1;
052300090320         for xx by 1 to %elem(skuni);
052400090320           clear o_testo;
052500090319           if skuni(xx) <> *blanks;
052600090319             wksc = %dec(%subst(skuni(xx):8:7):7:0);
052700090319             wksu = %dec(%subst(skuni(xx):1:7):7:0);
052800090319             yy = %lookup(wksc:skksc);
052900090320             zz = %lookup(wksu:skcliu);
053000090320       // stampo papà solo se diverso
053100090320             if wksu <> oldwksu;
053200090320               o_testo = %editc(wksu:'X');
053300090320               oldwksu = wksu;
053400090320             endif;
053500090320       // cliente
053600090325             %subst(o_testo:10:7) = %editc(skksc(yy):'X');
053700090320       // ragione sociale
053800090325             %subst(o_testo:19:25) = skrag(yy);
053900090320       // id job se presente
054000090320             if skaut(yy) = 'S';
054100090708               wjob = skjobu(zz);
054200090708               %subst(o_testo:46:7) = %editc(wjob:'Z');
054300090320             endif;
054400090320       // tipo elaborazione
054500090325             %subst(o_testo:56:1) = skaut(yy);
054600090320       // periodo estrazione se presente
054700090320             if skdal(yy) <> *zeros;
054800090325               %subst(o_testo:62:10) =
054900090320                         %subst(%editc(%dec(skdal(yy)):'X'):7:2) +
055000090320                         '/' +
055100090320                         %subst(%editc(%dec(skdal(yy)):'X'):5:2) +
055200090320                         '/' +
055300090320                         %subst(%editc(%dec(skdal(yy)):'X'):1:4);
055400090320             endif;
055500090320             if skal(yy) <> *zeros;
055600090325               %subst(o_testo:73:10) =
055700090320                       %subst(%editc(%dec(skal(yy)):'X'):7:2) +
055800090319                       '/' +
055900090320                       %subst(%editc(%dec(skal(yy)):'X'):5:2) +
056000090319                       '/' +
056100090320                       %subst(%editc(%dec(skal(yy)):'X'):1:4);
056200090320             endif;
056300140813       // se elaborazione = nessuna avviso
056400140813             if skaut(yy) = 'N';
056500140813               %subst(o_testo:62:21) = 'VERIFICARE!!         ';
056600140813             endif;
056700140813       // directory delle immagini
056800140813             %subst(o_testo:85:30) = skdir(yy);
056900110713       // Crea elenco a cliente
057000110713             IF skfile(yy) = 'S';
057100140813               %subst(o_testo:117:35) = 'PRIMA CREARE IL FILE!';
057200110713             ENDIF;
057300110713             except PrtDet;
057400090320           endif;
057500090319         endfor;
057600071212
057700071212       ENDSR;
057800071221
057900071221       //--------------------------------------------------------------
058000071221       //?Override al file di stampa per impostarvi i dati per
058100071221       //?  l'invio via e-mail   +   stampa inizio e-mail
058200071221       //--------------------------------------------------------------
058300071221       BEGSR sr_OpenPrtF;
058400071221
058500071221         // Override al file di stampa per impostarvi i dati per
058600071221         // l'invio via e-mail
058700071221           exsr sr_Override;
058800071221
058900071221           open PrtEMAIL;
059000071221
059100071221         // Stampa una testata se NON è richiesta la e-mail
059200071221           IF  §MRAdreg =  *blank;
059300071221             O_testo = JobUser + ' - ' + SDSpgm
059400071221                     + ' - ' + %editc( *date : 'Y' )
059500071221                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
059600071221             except PrtDet;
059700071221             clear O_testo;
059800071221             except PrtDet;
059900071221             except PrtDet;
060000071221           ENDIF;
060100071221
060200071221         // Stampa testo iniziale
060300121011           O_testo = 'Elenco clienti per i quali è stato +
060400121011                      preparato ed inviato via posta interna il CD dei +
060500121011                      Pod Image';
060600071221           except PrtDet;
060700090320
060800090320         // Stampa una riga vuota
060900090320           clear O_testo;
061000090320           except PrtDet;
061100090319
061200090319         // Stampa intestazione
061300121011           O_testo = 'Padre    Cliente  Ragione Sociale             Id Job' +
061400140813                     '  Elab.  Periodo Estrazione     Directory';
061500090319           except PrtDet;
061600071221
061700071221         // Stampa una riga vuota
061800071221           clear O_testo;
061900071221           except PrtDet;
062000071221
062100071221       ENDSR;
062200071219
062300071219       //--------------------------------------------------------------
062400071219       //?Override al file di stampa per impostarvi i dati per
062500071219       //?  l'invio via e-mail   +   stampa inizio e-mail
062600071219       //--------------------------------------------------------------
062700071219       BEGSR sr_Override;
062800071219
062900071220         reset $Invio;
063000071219         reset TRTCM1ds;
063100071221
063200071221         IF  §MRAdreg <> *blank;
063300080317           §CM1mitt = %trim(§MRAdmitt);
063400071221           §CM1dst  = %trim(§MRAddest);
063500071221           §CM1tips = §MRAdreg;
063600071227           §CM1po   = C_Sede;
063700071221           §CM1var  = '*OBJM*' + §MRAddes;
063800071221           §CM1idp  = §MRAdidpro;
063900071221           Qcmd = C_CmdOvrPrtF
064000071221                + ' outq(' + %trim(§MRAdoutqi) + ')'
064100071221                + ' usrdfndta(''' + TRTCM1ds + ''')';
064200071221         ELSE;
064300071221           Qcmd = C_CmdOvrPrtF;
064400071221         ENDIF;
064500071220
064600071219         callp(e) qCmdExc(Qcmd : %size(Qcmd));
064700071221         IF  §MRAdreg <> *blanks  and  not %error;
064800071221           $Invio = *on;
064900071221         ENDIF;
065000071219
065100071219       ENDSR;
065200071221
065300071221       //--------------------------------------------------------------
065400071221       //?Operazioni finali.
065500071221       //--------------------------------------------------------------
065600071221       BEGSR RoutEnd;
065700071221
065800071221         if %open(PrtEMAIL);
065900071221
066000071221         //?Chiusura dello spool?
066100071221           clear O_testo;
066200071221           except PrtDet;
066300121011           O_testo = 'Vi ricordiamo che il controllo del contenuto dei +
066400121011                      supporti prima che siano inviati ai clienti';
066500071221           except PrtDet;
066600121011           O_testo = 'è a vostro carico.';
066700121011           except PrtDet;
066800121011           O_testo = 'Vi chiediamo inoltre di proporre ai clienti di +
066900121011                      inviare i POD Image via FTP e non più tramite';
067000121011           except PrtDet;
067100121011           O_testo = 'supporto ottico (CD/DVD), facendo notare, ad +
067200121011                      esempio, la velocità con cui avrebbero a disposizione';
067300121011           except PrtDet;
067400121011           O_testo = 'le immagini rispetto al metodo attuale.';
067500121011           except PrtDet;
067600121011           clear O_testo;
067700121011           except PrtDet;
067800121011           O_testo = 'A disposizione per ogni chiarimento.';
067900121011           except PrtDet;
068000121011           clear O_testo;
068100121011           except PrtDet;
068200121011           O_testo = 'ICT di Sede';
068300121011           except PrtDet;
068400121011           O_testo = 'BRT SPA';
068500121011           except PrtDet;
068600071221
068700071221           close PrtEMAIL;
068800071221
068900071221         //?Eliminazione overflow?
069000071221           Qcmd = C_CmdDltOvr;
069100071221           callp(e) qCmdExc(Qcmd : %size(Qcmd));
069200071221
069300071221         endif;
069400071221
069500071221       ENDSR;
069600090305
069700090305       //--------------------------------------------------------------
069800090305       //?Aggiorna tabella LAC
069900090305       //--------------------------------------------------------------
070000090305       BEGSR sr_AggTbLAC;
070100090305
070200090305       // aggancio la tabella per aggiornarla
070300090309          chain (c_tab:tbeke1) tntbe01l;
070400090305          if %found(tntbe01l);
070500090305            dlac = tbeuni;
070600090318       // imposto la data prossima estrazione su tabella LAC
070700090320       // se presente
070800090320            if wdataal <> *zeros;
070900090320              w_dateiso = %date(wdataal) + %days(1);
071000090320              wdataal = %dec(w_dateiso);
071100090320              §lacdatela = %dec(%subst(%editc(wdataal:'X'):1:6):6:0);
071200090320            endif;
071300090320          tbeuni = dlac;
071400090320          update tntbe000;
071500090305          endif;
071600090305
071700090305       ENDSR;
071800071212
071900071212      /end-free
072000071219
072100071219      //---------------------------------------------------------------
072200071219      //?Spool di stampa (per e-mail).
072300071219      //---------------------------------------------------------------
072400071219     oPrtEMAIL  e            PRTdet      1
072500071219     o                       O_testo
072600071213
072700071219      //===============================================================
072800071219      //?Procedura per confronto data schedulata in tab. LAC e *date
072900071219      //===============================================================
073000071219     p CheckDataConSchGg...
073100071213     p                 b                   export
073200071213      *
073300071219     d CheckDataConSchGg...
073400071213     d                 pi             1a
073500071219     d   pi_LACschGG                  2s 0 const
073600071219     d   pi_LACschMM                  1a   const  dim(12)
073700071213     d   pi_Date                       d   const  datfmt(*iso)
073800071213     d   po_Esito                     1a   options(*nopass)
073900071213
074000071213       //--------------------------------------------------------------
074100071213       // Definizione variabili work.
074200071213       //--------------------------------------------------------------
074300071213       // - flags booleani
074400071213     d $Esito          s              1n   inz(*off)
074500071213
074600071213       //--------------------------------------------------------------
074700071213       // Procedura.
074800071213       //--------------------------------------------------------------
074900071213      /free
075000071213
075100071213       reset $Esito;
075200071213
075300071213       // verifica se invio schedulato per il giorno corrente
075400071213       SELECT;
075500071213
075600071213       // - nessuna schedulazione per il mese corrente
075700071219          WHEN pi_LACschMM(%subdt(pi_Date : *months)) = *blanks;
075800071219          // $Esito = *off;            (già così)
075900071213
076000071213       // - schedulazione in giornata max 28: giorno sempre valido
076100071219          WHEN pi_LACschGG <= 28   and
076200071219               pi_LACschGG =  %subdt(pi_Date : *days);
076300071213             $Esito = *on;
076400071213
076500071213       // - schedulazione in giornata > 28:
076600071213       //   da controllare per il mese in corso...
076700071219          WHEN pi_LACschGG >  28   and
076800071219              (pi_LACschGG = %subdt( pi_Date : *days )  or
076900071218              (%subdt( pi_Date + %days(1) : *months ) >
077000071218                 %subdt( pi_Date : *months )));
077100071213             $Esito = *on;
077200071213
077300071213          OTHER;
077400071219          // $Esito = *off;            (già così)
077500071213
077600071213       ENDSL;
077700071213
077800071213       if %parms > 3;
077900071213          po_Esito = $Esito;
078000071213       endif;
078100071213
078200071213       return $Esito;
078300071213
078400071213      /end-free
078500071213
078600071219     p CheckDataConSchGg...
078700071213     p                 e
