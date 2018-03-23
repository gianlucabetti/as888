000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000101141219     h dftactgrp(*no)
000102141219     h bnddir('UBBNDDIR')
000200060427      *------------------------------------------------------------------------*
000300150325      * Gestione tabella "OLP" = Orario Limite Picking di Filiale              *
000400060427      *------------------------------------------------------------------------*
000500141219     ftntbe01l  uf a e           k disk    extfile(wlibfile1) usropn
000501090424     fazorg01l  if   e           k disk
000800150325     ftntbolpd  cf   e             workstn
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
001600150325      * 01 - gestione eccezione giornaliera
001700150325      * 02 -
002500060428      * 28 - messaggio errore
002900060502
002901141219       // -?Costanti per la definizione delle schiere con i nomi?
002902141219       //  ?degli iSeries da elaborare e delle relative librerie?
002903141219     d c_NrSyst        c                   const(2)
002904141219     d c_NrLibr        c                   const(2)
002905150325     D digits          c                   '0123456789'
003000060502      *------------------------------------------------------------------------*
003100060502      * schiere
003200060502      *------------------------------------------------------------------------*
003300060502     d msg             s             78    dim(17) ctdata perrcd(1)
003301141219       // -?iSeries  &  Librerie con entrambi i file tabelle?
003302141219     d $iSystem        s                   like(currSysNetA)
003303141219     d                                     dim(c_NrSyst)
003304141219     d                                     ctdata   perrcd( 1)
003305141219     d $Libraries      s                   like(ds_Libr)
003306141219     d                                     dim(c_NrSyst)
003307141219     d                                     alt($iSystem)
003400060427
003500060428      *------------------------------------------------------------------------*
003600060428      * campi di work
003700060428      *------------------------------------------------------------------------*
003701090428     d data_eur        s               d   datfmt(*eur)
005600060504     d $video          s             10
005605141219     d $Fine           s               n   inz(*off)
005606141222     d wDate           s              8  0 inz
005607150417     d wDatEcc         s              8  0 inz
005608150417     d wDatEcc_i       s              8  0 inz
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006101150326     d                 DS
006102150326     d  wHH                    1      2  0
006103150326     d  wMM                    3      4  0
006104150326     d WORA                    1      4  0
006105150326
006702150325     d dolp          e ds
006703150325     d tibs02ds      e ds
006900060502     d kpjba         e ds
007000060503     d tibs34ds      e ds                  inz
007200060428     d §azute        e ds                  extname(azute00f)
007300060428     d                                     dtaara
007400060428     d §datiute      e ds                  extname(ddatiute)
007500060428     d                                     dtaara
007501150417     D xgiolavds     e ds
007502141219       // -?Ridefinizione elenco librerie in elaborare le tabelle?
007503141219     d ds_Libr         ds                  inz
007504141219     d   $Libr                       10    dim(c_NrLibr) inz
007600010503
007601141219       // -?Nome del sistema?
007602141219     d currSysNeta     s              8a   inz
007603141219
007604141219       // -?Nome esteso Libreria/File dei 2 file tabella?
007605141219     d wLibFile1       s             21a   inz
007606141219     d w_iSystem       s              1  0 inz
007607141219     d w_SisInf        s              3  0 inz
007608150325
007609150325     d wecc            s              1    inz(*off)
007610141219
007700060428     d psds           sds
007800060428     d  pgmname          *proc
007801150325       //--------------------------------------------------------------
007802150325       //?Definizione procedure.
007803150325       //--------------------------------------------------------------
007804150325     d tibs02r         pr                  extpgm('TIBS02R')
007805150325     d  kpjba                              likeds(kpjba)
007806150325     d  tibs02ds                           likeds(tibs02ds)
007807150417     D XGIOLAV         PR
007808150417     D                                     extpgm('XGIOLAV')
007809150417     D xgiolavds                           likeds(xgiolavds)
007810150325       // -?Ricerca filiale in organigramma?
007811150325      /copy gaitrasrc/srcProtoPI,TNSD24R
007812150325      /copy gaitrasrc/srcProtoPR,TNSD24R
007900060428
008701090424      // ? PROTOTIPI ?
008702090424      /copy gaitrasrc/srcprotopr,tibs34r
008703141219       // -?Reperimento NETA sistema AS/400 corrente?
008704141219      /copy gaitrasrc/srcProtoPR,UBRTVNETA
008800060428
008900060428      *------------------------------------------------------------------------*
009000060428      * costanti
009100060428      *------------------------------------------------------------------------*
009400010503
009500060428      *------------------------------------------------------------------------*
009600010423
009700060428      /free
009800060428
009801141219       if $fine=*off;
010000150325          exsr sr_d1;
010001141219       endif;
010100060428       exsr sr_uscita;
010101090424
016000060502       // ------------------------------------------------------------------------
016100150325       // gestione Videata di selezione filiale
016200060502       // ------------------------------------------------------------------------
016300150325       begsr sr_d1 ;
016400060428
016401150325        clear v1cfil;
016700150325        $video = 'D01';
016800060428
016900150325       // inizio elaborazione
017000150325         dou  $video <> 'D01';
017001150326       //     -?Apertura file TNTBE00F del 1° S.I. (sede)?
017002150326            if  w_SisInf <> 1   or   Not %open(TNTBE01L);
017003150326                w_SisInf = 1;
017004150326                exsr  sr_OpenFileTab;
017005150326            endif;
019100060428
019200150325       // Emissione
019400150325          exfmt tbolpd1;
019401150325       // reset indicatore generico di errore
019402150325         *in90 = *off;
019403150326         *in28 = *off;
019404150325
019405150325       // pulisco il campo messaggi
019406150325         clear v1cmsg;
019500060428
019600150325       // controllo tasti funzionali
019700060428          select;
019800060428
019900060502       // F3=Fine
020000060428           when *inkc;
020100060502            $video = *blanks;
020300060428            exsr sr_uscita;
020400060428
020900150325       // F10=Int.tabella
021000060428           when *inkj;
021001150325            clear tibs02ds ;
021002150325            t02mod='R';
021003150325            t02cod='OLP';
021004150325            t02sif=knsif;
021005150325            tibs02r(kpjba:tibs02ds);
021006150325            if t02err=*blanks;
021007150325              v1cfil=%subst(t02ke1:1:3);
021008150325            endif;
021600060428
022100060502       // in tutti gli altri casi
022200060428           other;
022300060502       // controllo ed elaborazione scelte su subfile
022400150325            exsr sr_ctrd1;
022401150326            if not *in90;
022402150325                wecc=*off;
022403150416                if not *inkh;
022404150325                   wecc=*on;
022405150325                endif;
022406150325                exsr sr_d2;
022407150325            endif;
022500060428          endsl;
022600060428
022700150325       // fine elaborazione 'D01'
022800060428         enddo;
022900060428
023000060428        endsr;
023001150325       // ------------------------------------------------------------------------
023002150325       // controllo video selezione filiale
023003150325       // ------------------------------------------------------------------------
023004150325       begsr sr_ctrd1;
023005150325
023006150325        *in28 = *off;
023011150325
023012150325       // ? --> interrogo organigramma
023013150325        if %scan('?':v1cfil)>0;
023014150325             tnsd24r ( p_FIL : p_FAG : p_DES : p_DIT );
023015150326             if p_fil>*zeros;
023016150326                V1Cfil  = p_FIL;
023017150326       //       V1Dke2F = p_DES;
023018150326             endif;
023019150325             *in90=*on;
023020150325             leavesr;
023021150325        endif ;
023022150325       // Filiale deve essere <> blanks
023023150325        if v1cfil=*blanks ;
023024150325         v1cmsg = msg(01);
023025150325         *in28 = *on;
023026150325         *in90 = *on;
023027150325         leavesr;
023028150325        endif;
023029150325       // Filiale deve contenere solo numeri
023030150325           if %check(digits:v1cfil)>0;
023031150325            v1cmsg = msg(02);
023032150325            *in28 = *on;
023033150325            *in90 = *on;
023034150325            leavesr;
023035150325           endif;
023036150325
023037150325       // errore se non è una filiale
023038150325         if v1cfil  >*zeros;
023039150325            chain (%int(V1Cfil))  AZORG;
023040150325            if not %found(azorg01l);
023041150325               v1cmsg = msg(02);
023042150325               *in28 = *on;
023043150326               *in90 = *on;
023044150325               leavesr;
023045150325            endif;
023046150325         endif;
023047150326       // Se richiesto il generico non si può richiederne l'eccezione
023048150416         if v1cfil=*zeros and not *inkh;
023049150326               v1cmsg = msg(03);
023050150326               *in28 = *on;
023051150326               *in90 = *on;
023052150326               leavesr;
023053150326         endif;
023054150325
023089150325
023090150325       endsr;
023100060428
030400060502       // ------------------------------------------------------------------------
030500060502       // gestione videata
030600060502       // ------------------------------------------------------------------------
030700150325       begsr sr_d2;
030800060428
030900060504       // imposto il video a seconda della funzione richiesta
031000060428        exsr sr_setvideo;
031100060428
031200060502       // imposto variabile
031300150325        $video = 'D02';
031400060428
031500060502       // fino a che la variabile resta settata come 'D01'
031600150325        dou $video <> 'D02';
031700060428
033801090424
034400060502       // emetto il video
034500150325          exfmt tbolpd2;
034700060428
034800060502       // reset indicatore generico di errore
034900150325         *in90 = *off;
034901150326         *in28 = *off;
035000060508
035100060508       // pulisco il campo messaggi
035200150325         clear v2cmsg;
035300010430
035400060502       // controllo tasti funzionali del video
035500060428         select;
035600060428
035700060502       // F3=Fine
035800060428          when *inkc;
036000060502           $video = *blanks;
036100060505           unlock tntbe01l;
036200060428           exsr sr_uscita;
036300060428
036400060502       // F6=Conferma
036500060428          when *inkf;
036700060502       // controllo e decodifico i dati del video
036800150325           exsr sr_ctrd2;
038000060428
038100150325       // se non riscontrati errori
038200150326           if not *in90;
038503141219                exsr sr_UpdateAll;
038509150326                $video = 'D01';
038600060428           endif;
038601150325       // F16=Annullamento
038602150326          when *inkq;
038608150326              exsr sr_UpdateAll;
038610150325              $video = 'D01';
040000060428
040100060502       // F12=Ritorno
040200060428          when *inkl;
040500060505           unlock tntbe01l;
041200150325           $video = 'D01';
041300060428
041400060502       // Invio
041500060428          other;
041700150325            exsr sr_ctrd2;
041900060428
042000060502         endsl;
042100060428
042200150325       // fine gestione 'D02'
042300060428        enddo;
042400060428
042500060428       endsr;
045900060428
046000060502       // ------------------------------------------------------------------------
046100060502       // imposto i dati a video
046200060502       // ------------------------------------------------------------------------
046300060428       begsr sr_setvideo;
046400060428
046401150325         *in01 =*off;
046402150325         *in02 =*off;
046403150326         *in03 =*off;
046404150325         d1tbeke1=v1cfil;
046405150325         clear v2dfil;
046406150416         clear v2dols;
046407150325         if v1cfil='000';
046408150326            v2dfil='GENERICO AZIENDALE';
046409150416            v2dhlp='Orario Limite Picking .......:';
046410150326       //   Disabilito f16-Annullamento
046411150326            *in03=*on;
046412150325         else;
046413150325            chain (%int(v1cfil)) azorg01l;
046414150325            v2dfil=orgdes;
046415150325         endif;
046416150416       //clear v2cedt;
046417150325         clear d1tbeke2;
046418150325         clear v2chlp;
046419150325         if wecc=*on;
046420150325            *in01=*on;
046421150416       //   v2cedt=wdateU;
046422150417       // determino la data dell'Eccezione:
046423150417       //   - data del giorno se entro le ore 16
046424150417       //   - data del giorno lavorativo successivo a oggi se dopo le ore 16
046425150417            wDatEcc = %int( %subst( %char( %dec( %timestamp() ) )
046426150417                               : 1 : 8 ) );
046427150417            if %dec(%time())>160000 ;
046428150417               clear xgiolavds;
046429150417               ixgldata=wdatEcc;
046430150417               ixglfil=%int(v1cfil) ;
046431150417               ixglpa='A' ;
046432150417               ixgladd='S' ;
046433150417               ixglgg =1   ;
046434150417               ixgllav ='S';
046435150417               xgiolav(xgiolavds);
046436150417               if oxglerr=*blanks and oxgldata> *zeros;
046437150417                  wdatEcc=oxgldata;
046438150417               endif;
046439150417            endif;
046440150417            data_eur=%date(wdatEcc:*iso);
046441150417            wdatEcc_i=%dec(data_eur);
046442150417            d1tbeke2=%editc(wdatEcc:'X');
046443150325         endif;
046444150325         if d1tbeke1>*zeros;
046445150325            *in02 =*on ;
046446150325            clear tbeke2;
046447150326       // Eccezione giornaliera
046448150325            if wecc=*on;
046449150416               v2dols='Orario Limite Picking .......:'   ;
046450150417               v2dhlp='ECCEZIONE del ' +
046451150417                      %editw(wdatEcc_i:'  /  /    ') + ' ....:';
046452150326               tbeke1=d1tbeke1;
046453150326               tbeke2=*blanks;
046454150326       //     Cerco orario di filiale
046455150326               chain(n) ('OLP':tbeke1:tbeke2)tntbe01l;
046456150326       //     Se non trovato orario di filiale visualizzo il generico
046457150326               if not %found(tntbe01l);
046458150326                  tbeke1='000';
046459150326                  chain(n) ('OLP':tbeke1:tbeke2)tntbe01l;
046460150326               endif;
046461150326               dolp=tbeuni;
046462150325            else;
046463150326       // Orario di filiale
046464150416               v2dols='Orario Limite Picking Generico'   ;
046465150416               v2dhlp='Orario Limite Picking Filiale:'   ;
046466150326               tbeke1='000';
046467150326               tbeke2=*blanks;
046468150326               chain(n) ('OLP':tbeke1:tbeke2)tntbe01l;
046469150326               dolp=tbeuni;
046470150325            endif;
046471150326            v2cols=§olpora;
046472150325         endif;
046473150325         tbeke1=d1tbeke1;
046474150325         tbeke2=d1tbeke2;
046475150325         chain ('OLP':tbeke1:tbeke2)tntbe01l;
046476150325         if %found(tntbe01l);
046477150325            v2vaim='Manutenzione';
046478150326            dolp=tbeuni;
046479150326            v2chlp=§OLPORA;
046480150325         else;
046481150325            v2vaim='Immissione';
046482150326       //   Disabilito f16-Annullamento
046483150326            *in03 = *on;
046484150325         endif;
051300060502
051400060502       endsr;
051500060502
051600060502       // ------------------------------------------------------------------------
051700060502       // controllo video
051800060502       // ------------------------------------------------------------------------
051900150325       begsr sr_ctrd2;
052000060502
052101150326        wora=v2chlp;
052108150416        if whh<0 or whh>24;
052109150326          v2cmsg = Msg(04);
052110150326          *in90=*on;
052111150326          *in28=*on;
052112150326          leavesr;
052113150326        endif;
052114150326        if wmm < 0  or wmm > 59  ;
052115150326          v2cmsg = Msg(04);
052116150326          *in90=*on;
052117150326          *in28=*on;
052118150326          leavesr;
052119150326        endif  ;
052120150416       // Orario picking nel range 07:00 - 18:00
052121150416        if wora  < 0700 or wora  >1800;
052122150416          v2cmsg = Msg(06);
052123150416          *in90=*on;
052124150416          *in28=*on;
052125150416          leavesr;
052126150416        endif;
052400150326       // Minuti multipli di 15
052401150326        if wmm>0 and %rem(wmm:15)>0;
052402150326          v2cmsg = Msg(05);
052403150326          *in90=*on;
052404150326          *in28=*on;
052405150326          leavesr;
052406150326        endif;
053200060508
055500060502
055600060502       endsr;
065201141219       //--------------------------------------------------------------
065202141219       //?Aggiornam. tab. nei file di tutti i sistemi informativi?
065203141219       //--------------------------------------------------------------
065204141219       BEGSR  sr_UpdateAll;
065205141219
065206141219         // -?Ciclo di elaborazione per ogni sistema informativo?
065207141219         For  w_SisInf = 1  To  %elem($Libr);
065208141219
065209141219           // -?Apertura degli archivi?
065210141219           if  w_SisInf > 1;
065211141219             exsr  sr_OpenFileTab;
065212141219           endif;
065213141219
065214141219           exsr  sr_aggiorna;
065215141219
065216141219         EndFor;
065217141219
065218141219       ENDSR;
065300060502       // ------------------------------------------------------------------------
065400060502       // aggiorno tabella
065500060502       // ------------------------------------------------------------------------
065600060502       begsr sr_aggiorna;
065700060502
065701141222         // -?RI-aggancio record da aggiornare?
065702141222         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
065703141222         If  w_SisInf > 1;
065715150325           chain  ('OLP':d1tbeke1:d1tbeke2) tntbe01l;
065717141222         EndIf;
067200060502
067400060502        select;
068500060502
068600150325       // cancellazione
068700150326         when *inkq       ;
068701150325             if  %found(TNTBE01L)  ;
068702150325               //_______________
068703150325               dELETE  TNTBE000;
068704150325               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068705150325             endif;
068706150325        other ;
068714150325             exsr  sr_RieRecTBE;
068715150325             clear tbeatb;
068716150325             // -?Aggiornamento record?
068717150325             if  %found(TNTBE01L);
068718150325               //_____________
068719150325               UPDATE TNTBE000;
068720150325               //¯¯¯¯¯¯¯¯¯¯¯¯¯
068721150325             else;
068722150325               //_____________
068723150325               WRITE  TNTBE000;
068724150325               //¯¯¯¯¯¯¯¯¯¯¯¯¯
068725150325             endif;
069000060502        endsl;
069001150326       // Se inserito record eccezione cancello eventuale precedente
069002150326        if not *inkq and tbeke2<>*blanks;
069003150326           chain(n)  ('OLP':tbeke1:tbeke2) tntbe01l;
069004150326           readpe    ('OLP':tbeke1) tntbe01l;
069005150326           if %equal(tntbe01l) and tbeke2<>*blanks;
069006150326              delete tntbe000;
069007150326           endif;
069008150326        endif;
071700060502
071800060502       endsr;
071801141222       //--------------------------------------------------------------
071802141222       //?Caricamento dati in tab. "BSP".                              ?
071803141222       //--------------------------------------------------------------
071804141222       BEGSR  sr_RieRecTBE;
071805141222
071806141222         // -?Impostazione campi chiave se inserimento?
071807141222         if  NOT %found(TNTBE01L);
071808141222           clear TNTBE000;
071809150325           TBEcod ='OLP'    ;
071810141222           TBEke1 = d1TBEke1;
071811141222           TBEke2 = d1TBEke2;
071812141222           clear tbelin;
071813141222           clear TBEsif;
071814141222         endif;
071815141222
071816141222         // -?Impostazione dati tabella da aggiornare?
071818150325         dolp=tbeuni ;
071819150325         §olpora=v2chlp;
071820150325         §olpdfil=v2dfil;
071821150325         tbeuni=dolp;
071822141222
071823141222         // -?Impostazione altri campi del record?
071824141222         clear TBEapl ;
071825141222         TBEftt = 'S';
071826141222         clear  TBEflt;
071827141222         if  w_SisInf = 1;
071828141222           TBEftr = 'T';
071829141222         else;
071830141222           TBEftr = 'R';
071831141222         endif;
071832141222         TBEdtr = wDate;
071833141222
071834141222       ENDSR;
087500060505
104400060502       // ------------------------------------------------------------------------
104500060502       // routine iniziale
104600060502       // ------------------------------------------------------------------------
104700060428         begsr *inzsr;
104800060428
104900060428      /end-free
105000060428
105100060428     c     *entry        plist
105200060428     c                   parm                    kpjba
105300060428
105400060428      /free
105401141219         // -?Verifica del sistema AS/400 corrente?
105402141219         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
105403141219           $Fine = *on;
105404141219           leavesr;
105405141219         endif;
105406141219
105407141219         // -?Impostazione elenco librerie in cui gestire le tabelle?
105408141219         //  ?(a seconda del sistema in cui si sta lavorando)?
105409141219         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
105410141219         if  w_iSystem = *zero;
105411141219           $Fine = *on;
105412141219           leavesr;
105413141219         endif;
105416141219         // eseguo apertura a tntbe puntando all'ambiente di sede
105419141219         w_SisInf = 1;
105430141219         // -?Apertura file tabelle                                 ?
105431141219         exsr sr_OpenFileTab;
105432141219
105500060428         in(e) §azute;
105600060428         if not %error;
105700060428          in(e) §datiute;
105800060428         endif;
105900060502         if %error or rsut = *blanks;
106000060428          tibs34r(tibs34ds);
106100060428          in §azute;
106200060428          in §datiute;
106300060428         endif;
106321141222         // -?Reperimento data odierna?
106322141222         wDate = %int( %subst( %char( %dec( %timestamp() ) )
106323141222                               : 1 : 8 ) );
106324150325         data_eur=%date(wdate:*iso);
106325150417       //wdateU=%dec(data_eur);
106400060428
106500060428         endsr;
106501141219       //--------------------------------------------------------------
106502141219       //?Apertura dei files tabelle nel sistema informativo impostato.?
106503141219       //--------------------------------------------------------------
106504141219       BEGSR  sr_OpenFileTab;
106505141219
106506141219         // -?Chiusura (eventuale) archivi?
106507141219         if  %open(Tntbe01l);
106508141219           close  Tntbe01l;
106509141219         endif;
106510141219
106511141219         // -?Apertura archivi?
106512141219         ds_Libr  = $Libraries(w_iSystem);
106513141219         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
106514141219         open  TNTBE01L;
106515141219
106516141219       ENDSR;
106600060502
106700060502       // ------------------------------------------------------------------------
106800060502       // uscita dal programma
106900060502       // ------------------------------------------------------------------------
107000060502         begsr sr_uscita;
107100060502
107800060502          *inlr = *on;
107900060502          return;
108000060502
108100060502         endsr;
108200060502
108300060502      /end-free
108400060502
108500060502** -MSG-                                                                     *
108600150325Immettere il cod. della filiale                                                01
108700150325Filiale inesistente o errata                                                   02
108800150416Per l'orario Generico premere F8                                               03
108900150326Orario errato                                                                  04
109000150326Inserire Minuti multipli di 15                                                 05
109001150416Inserire un orario compreso fra le 07:00 e le 18:00                            06
109100141222** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
109200141222SETRAS  GAITRAGRU FILTRAGRU
109300141222AS888   GAITRAGRPSFILTRAGRPF
