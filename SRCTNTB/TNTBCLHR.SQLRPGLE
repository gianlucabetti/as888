000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300171109      * Gestione tabella "CLH" = Abilitazione Special Sunday per ter-part.     *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600090424     fazorg01l  if   e           k disk
000700171109     ftntbclhd  cf   e             workstn sfile(tbcls01:s01nrr)
000800171109     f                                     sfile(tbcls02:s02nrr)
000900120622
001000120622      *------------------------------------------------------------------------*
001100120622      * riepilogo indicatori
001200120622      *------------------------------------------------------------------------*
001300120622      * 02 - visualizzazione
001400120622      * 03 - proteggo campi video 01
001500120622      * 04 - manutenzione
001600120622      * 20 - gestione subfile 01
001700120622      * 21 - gestione subfile 01
001800120622      * 22 - gestione subfile 01
001900120622      * 23 - gestione subfile 01
002000120706      * 24 - gestione subfile 02
002100120706      * 25 - gestione subfile 02
002200120706      * 26 - gestione subfile 02
002300120706      * 27 - gestione subfile 02
002400120622      * 28 - messaggio errore
002500120706      * 40/42Errori campi subfile/control
002600120622
002700120622      *------------------------------------------------------------------------*
002800120622      * schiere
002900120622      *------------------------------------------------------------------------*
003000120622     d msg             s             78    dim(17) ctdata perrcd(1)
003001171110     D sql             S            100    DIM(2) CTDATA PERRCD(1)
003100120622
003200120622      *------------------------------------------------------------------------*
003300120622      * campi di work
003400120622      *------------------------------------------------------------------------*
003401171109     D wSQL            S           1000    inz
003402171109     d parcli          s                   like(c1ccm)
003403171109     d k_ke1           s                   like(tbeke1)
003404171109     d k_ke2           s                   like(tbeke2)
003405171110     d s_tbeke1        s              3
003406171110     d s_tbeke2        s              7
003407171110     d s01nrrpos       s                   like(s01nrr)
003408171109     d w_daCLI         s              1    inz
003500120622     d data_eur        s               d   datfmt(*eur)
003600120622     d blanks          s                   like(d1descopz)
003700120622     d comand          s              1
003800120622     d opzio           s              2
003900120622     d ricez           s              1
004000120622     d len             s              5u 0
004100120622     d s01nrr          s              4  0
004200120706     d s02nrr          s              4  0
004300120622     d savopzione      s                   like(s1opzione)
004400120622     d savrcdnbr       s                   like(c1rcdnbr)
004500120706     d sv2rcdnbr       s                   like(c2rcdnbr)
004600120622     d savtbeftt       s                   like(tbeftt)
004700120622     d savtbeflt       s                   like(tbeflt)
004800120622     d savtbeuni       s                   like(tbeuni)
004900120622     d kfil            s                   like(orgfil)
005000120622     d wrkeofs01       s              1
005100120622     d wrkcars01       s              1
005200120622     d wrkcarw01       s              1
005300120622     d wrksfl02        s              1
005400120622     d $video          s             10
005500171117     d i               s              4  0 inz
005501171117     d is              s              4  0 inz
005502171116     d s_i             s              4  0 inz
005503171113     d progr           s              4  0 inz
005600120622     d w0030           s              3  0 inz
005601171123     d wPage           s              4  0 inz
005602171123     d wRow            s              3  0 inz
005603171123     d W_SflPag        s              3  0 inz
005604171124     d Wfclh           s              1
005605171127     d W_ftt           s                   like(tbeftt)
005606171127     d W_flt           s                   like(tbeflt)
005607171127     d W_ftr           s                   like(tbeftr)
005608171127     d W_dtr           s                   like(tbedtr)
005609171110
005610171124     d tipvid          s              1
005611171110     d $Finerec        s              1    inz(*off)
005612171124     d $Fine           s               n   inz
005700120622
005800120622      *------------------------------------------------------------------------*
005900120622      * ds interne/esterne
006000120622      *------------------------------------------------------------------------*
006100120706     d campocm         ds           252
006200120706     D  Scm                    1    252    dim(36)
006300120706     d campocms01      ds            21
006400120706     d  s01cm1
006500120706     d  s01CM2
006600120706     d  s01CM3
006700120706     d* s01CM4
006800120706     d* s01CM5
006900120706     d* s01CM6
007000120706     d* s01CM7
007001171113     d campocmf        ds          7000
007002171113     D  Scmf                   1   7000    dim(1000)
007100120622     d
007200120622     d TIBS69DS      E DS                  INZ
007300120622     d DS_cnaco      E DS                  extname(CNACO00F)
007400120622     d DS_cnind      E DS                  extname(CNIND00F)
007500120622     d DS_cnclp      E DS                  extname(CNCLP00F)
007600120622     d DS_fncls      E DS                  extname(FNCLS00F)
007601171109     d tntbe_DS      E DS                  extname(TNTBE00F)
007602171124     D fnlv55ds      E DS                  INZ
007700120622     d kpjba         e ds
007800120622     d tibs34ds      e ds                  inz
007900120622     d §azute        e ds                  extname(azute00f)
008000120622     d                                     dtaara
008100120622     d §datiute      e ds                  extname(ddatiute)
008200120622     d                                     dtaara
008300120622
008400120622     d psds           sds
008500120622     d  pgmname          *proc
008600120622
008700120622      // ? PROTOTIPI ?
008800120622      /copy gaitrasrc/srcprotopr,tibs34r
008900120622      /copy gaitrasrc/srcprotopr,tibs69r
009000171109      /COPY GAITRASRC/SRCPROTOPR,FNLV55R
009100120622      *------------------------------------------------------------------------*
009200120622      * costanti
009300120622      *------------------------------------------------------------------------*
009400120622     d errore          c                   '1'
009500120622     d eseguito        c                   '0'
009501171110     D digits          c                   '0123456789'
009600120622
009700120622      *------------------------------------------------------------------------*
010000120622
010001171109         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010004171124        dow $fine=*off ;
010005171124         select;
010006171124         when $video='C02';
010007171109          exsr sr_c02;
010008171124         other            ;
010009171124          exsr sr_c01;
010010171124         endsl;
010011171124        enddo;
010102171109
010200120622       exsr sr_uscita;
010300120622
010400120622       // ------------------------------------------------------------------------
010500120622       // gestione subfile
010600120622       // ------------------------------------------------------------------------
010700120622       begsr sr_c01;
010800120622
010900120622       // imposto variabili
011000120622        wrkcars01 = *on;
011100120622        $video = 'C01';
011200120622
011300120622       // inizio elaborazione subfile
011400120622         dou  $video <> 'C01';
011500120622
011600120622       // caricamento subfile
011700120622          exsr sr_cars01;
011800120622
011900120622       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
012000120622       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
012100171124       // l'ultimo tasto
012200120622          if c1csrrrn > 0;
012300120622           c1rcdnbr = c1csrrrn;
012400120622          else;
012500120622           c1rcdnbr = savrcdnbr;
012600120622          endif;
012700120622
012800120622       // se non so quale pagina visualizzare forzo pagina 1
012900120622          if c1rcdnbr < 1;
013000120622           eval c1rcdnbr = 1;
013100120622          endif;
013101171110       // se richiesto un posizionamento imposto
013102171110           if c1ccm<>*blanks  and s01nrrpos>0;
013103171110           eval c1rcdnbr = s01nrrpos;
013104171110           endif;
013200120622
013300120622       // salvo il record number del subfile
013400120622          savrcdnbr = c1Rcdnbr;
013500120622
013600120622       // Emissione del subfile
013700171109          write tbclp01;
013800171109          exfmt tbclc01;
013900120622
014000120622       // controllo tasti funzionali del subfile
014100120622          select;
014200120622
014300120622       // F3=Fine
014400120622           when *inkc;
014500120622            $video = *blanks;
014600120622            ricez='C';
014700120622            exsr sr_uscita;
014800120622
014900120622       // F5=Refresh
015000120622           when *inke;
015100120622            wrkcars01 = *on;
015200120622
015300120622       // F10=Immissione
015400120622           when *inkj;
015500171124            $video = 'C02';
015600120622            comand='J';
015700120704            opzio='  ';
015800120706            exsr sr_c02;
015900120622
016000120622       // in tutti gli altri casi
016100120622           other;
016200120622       // controllo ed elaborazione scelte su subfile
016300120622            exsr sr_gestsfl;
016400120622          endsl;
016500120622
016600120622       // fine elaborazione 'C01'
016700120622         enddo;
016800120622
016900120622        endsr;
017000120622
017100120622       // ------------------------------------------------------------------------
017200120622       // caricamento subfile
017300120622       // ------------------------------------------------------------------------
017400120622       begsr sr_cars01;
017500120622
017600120622       // se è stato richiesto il caricamento del subfile
017700120622        if wrkcars01 = *on;
017800120622
017900120622       // inizializzo il subfile
018000120622         s01nrr = 0;
018100120622         *in20 = *on;
018200171109         write tbclc01;
018300120622         *in20 = *off;
018400120622         *in21 = *off;
018500120622         *in22 = *off;
018600120622         *in23 = *off;
018601171110         clear s_tbeke1;
018602171110         clear s_tbeke2;
018603171110         clear s01nrrpos        ;
018604171116         $finerec = *off  ;
018605171109         // imposto stringa sql per lettura tntbe "CLH" ordinata per codice cliente
018606171110         wsql=sql(1)+ ' ' +%trim(sql(2));
018607171109         exec sql prepare STRINGASQL from :WSql;
018608171109         exec sql declare TABCLH cursor for StringaSql;
018609171109         exec sql open TABCLH;
018610171109        dow $finerec=*off;
018611171109           exec sql Fetch TABCLH into :tntbe_DS        ;
018612171109           if sqlcod=100 or sqlcod<0;
018613171109              $finerec = *on;
018614171109              leave;
018615171109           endif;
020100120622
020200120622       // scrivo subfile
020300120622           clear s1opzione;
020400120622           exsr sr_wtrs01;
020500120622
020600120622        enddo;
020700120622
020701171116         exec sql close TABCLH;
020800120622         clear opzio;
020900120622         clear comand;
021000120622
021100120622       // fine caricamento subfile
021200120622        endif;
021300120622
021400120622       // se scritto almeno un record
021500120622        if s01nrr > 0;
021600120622       // indicatore di sflend
021700120622         eval *in21 = *on;
021800120622        endif;
021900120622
022000120622       // se non scritto neanche un record
022100120622        if s01nrr = 0;
022200120622       // indicatore di sfldsp
022300120622         eval *in23 = *on;
022400120622        endif;
022500120622
022600120622       // disattivo opzione di caricamento subfile
022700120622        wrkcars01 = *off;
022800120622
022900120622       endsr;
023000120622       // ------------------------------------------------------------------------
023100120622       // gestione video dati di trasmissione
023200120622       // ------------------------------------------------------------------------
023300120622       begsr sr_w01;
023400120622
023500120622       // imposto i dati a video
023600120622        exsr sr_carw01;
023700120622
023800120622       // fino a che la variabile resta settata come 'W01'
023900120622        dou $video <> 'W01';
024000120622
024100120622       // reset indicatore generico di errore
024200120622         *in28 = *off;
024300120622
024400120622       // emetto il video
024500171110          exfmt tbclw01;
024600120622
024700120622       // controllo tasti funzionali del video
024800120622         select;
024900120622
025000120622       // F6=Conferma
025100120622          when *inkf;
025200120622       // controllo i dati del video
025300120622           exsr sr_ctrw01;
025400120622       // se non riscontrati errori aggiorno il record corrente
025500120622           if not *in28;
025600120622            exsr sr_aggiorna;
025700120622            if not *in28;
025800171124       //    if comand = 'J';
025900171124       //     $video = 'C02';
026000171124       //     exsr sr_setc02;
026100171124       //     exsr sr_cars02;
026200171124       //    else;
026300120622              $video = 'C01';
026301171127              wrkcars01 = *on;
026400171124       //    endif;
026500120622            endif;
026600120622           endif;
026700120622
026800120622       // F12=Ritorno
026900120622          when *inkl;
027000171124           $video = 'C02';
027100120622           clear ricez;
027200120622
027300120622       // Invio
027400120622          other;
027500120622           exsr sr_ctrw01;
027600120622
027700120622         endsl;
027800120622
027900120622       // fine gestione 'W01'
028000120622        enddo;
028100120622
028200120622       endsr;
028300120622
028400120622       // ------------------------------------------------------------------------
028500120622       // imposto i dati di trasmissione
028600120622       // ------------------------------------------------------------------------
028700120622       begsr sr_carw01;
028800120622
028900120622       // se pulisco i campi
029000120622         clear w1ftt;
029100120622         clear w1flt;
029200120622         clear w1ftr;
029300120622         clear w1dtr;
029400120622
029500120622       // se non immissione imposto i campi del file
029600120622        if comand <> 'J';
029700171127         w1ftt = w_ftt;
029800171127         w1flt = w_flt;
029900171127         w1ftr = w_ftr;
030000120622       // imposto la data
030100171127         if w_dtr <> *zeros;
030200171127          data_eur=%date(w_dtr:*iso);
030300120622          w1dtr=%dec(data_eur);
030400120622         endif;
030500120622
030600120622        endif;
030700120622
030800120622       endsr;
030900120622
031000120622       // ------------------------------------------------------------------------
031100120622       // controllo i dati di trasmissione
031200120622       // ------------------------------------------------------------------------
031300120622       begsr sr_ctrw01;
031400120622
031500120622       endsr;
031600120622
031700120622       // ------------------------------------------------------------------------
031800120622       // aggiorno tabella
031900120622       // ------------------------------------------------------------------------
032000120622       begsr sr_aggiorna;
032100120622
032701171113        s_i=1;
032702171113        progr=0;
032900120622
033000120622       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
033100120622        select;
033200120622
033300120622       // F10=immissione -
033400120622         when comand = 'J';
033401171127             tbeftt = w1ftt;
033402171127             tbeflt = w1flt;
033403171127             clear tbeftr;
033404171127             clear tbedtr;
033405171116             exsr sr_ins;
034000120622
034100120622       // Opzione "04"=cancellazione/ripristino
034200120622       // Opzione "02"=manutenzione
034300120622         when opzio = '04' or opzio='02';
034312171116             tbeke1 = %editc(d01tfp:'X');
034313171116             tbeke2 = %editc(d01ccm:'X');
034314171116          setll ('CLH':tbeke1:tbeke2) tntbe01l;
034315171116          reade ('CLH':tbeke1) tntbe01l;
034316171116          dow not %eof(tntbe01l);
034317171116            if %subst(tbeke2:1:7)<>%editc(d01ccm:'X');
034318171116               leave;
034319171116            endif;
034320171116       // Manutenzione
034321171116            if opzio = '02';
034323171116               exsr SR_cartotbe;
034324171116               if scm(1) > *zeros;
034325171124                   clear tbeatb;
034326171124               else;
034327171124                   tbeatb='A' ;
034328171124               endif ;
034329171127                  tbeftt = w1ftt;
034330171127                  tbeflt = w1flt;
034331171127                  clear tbeftr;
034332171127                  clear tbedtr;
034400171116                  update tntbe000;
034407171116            else;
034408171116       // Cancellazione/ripristino
034409171116            // conferma per annullo/ripristino
034410171116            if opzio = '04';
034411171116             select;
034412171116             // annullamento
034413171116             when tbeatb = *blanks;
034414171116              tbeatb = 'A';
034415171116             // ripristino
034416171116             when tbeatb = 'A';
034417171116              clear tbeatb;
034418171116             endsl;
034419171116            endif;
034420171127               tbeftt = w1ftt;
034421171127               tbeflt = w1flt;
034422171127               clear tbeftr;
034423171127               clear tbedtr;
034424171116               update tntbe000;
034425171116            endif     ;
034426171116             reade ('CLH':tbeke1) tntbe01l;
034427171116          enddo;
034428171116       // Continuo a scrivere fino a esaurimento dati a video
034429171116          if opzio='02' and s_i<=%elem(scmf);
034430171116             progr = %dec(%subst(tbeke2:12:4):4:0);
034431171116             exsr sr_ins ;
034432171116          endif;
034500120622
034600120622        endsl;
034700120622
034800120622       endsr;
034801171116       // ------------------------------------------------------------------------
034802171116       // Inserimento nuovi record su tntbe
034803171116       // ------------------------------------------------------------------------
034804171116       begsr sr_INS     ;
034805171116          dow s_i<=%elem(scmf)   ;
034806171116             tbeke1 = %editc(d01tfp:'X');
034807171116             tbeke2 = %editc(d01ccm:'X');
034808171116             progr+=1;
034809171116             %subst(tbeke2:12:4) = %editc(progr:'X');
034810171116             clear tbelin;
034811171116             clear tbeatb;
034812171124             clear tbeuni;
034813171116             exsr sr_CartoTbe;
034814171124            if progr=1 or scm(1) > *zeros;
034815171116             write tntbe000;
034816171116            endif;
034817171116             s_i+=1;
034818171116          enddo;
034819171116       endsr;
034820171113       // ------------------------------------------------------------------------
034821171113       // caricamento codici cliente da sfl a data base
034822171113       // ------------------------------------------------------------------------
034823171113       begsr sr_CartoTbe;
034824171113       clear scm;
034825171117       is=1     ;
034826171117       for i=s_i to %elem(scmf);
034827171124          if is> %elem(scm)  ;
034828171113             leave  ;
034829171113          endif;
034830171113           if scmf(s_i) > *zeros;
034831171113              scm(is)=scmf(s_i);
034832171113              is+=1;
034833171113           endif;
034834171117         s_i+=1;
034835171113       endfor;
034836171124       if scm(1) > *zeros;
034837171124          tbeuni=campocm;
034838171124       endif;
034839171113       endsr;
034840171117       // ------------------------------------------------------------------------
034841171117       // Caricamento nuova pagina sfl02
034842171117       // ------------------------------------------------------------------------
034843171117       begsr sr_newpag  ;
034844171123         S02nrr    = (W_SflPag * 36);
034845171123         W_SflPag += 1;
034846171123
034847171123           for I=1 to 36  ;
034848171117              clear s02cm;
034849171117              clear s02dcm;
034850171117         //   *in26=*off;
034851171117              // imposto numeratore righe e numero relativo di record
034852171117              s02nrr = s02nrr + 1;
034853171117              // scrivo subfile
034854171117              write tbcls02;
034855171117           endfor;
034856171123     c     S02nrr        div       36            wPage
034857171123     c                   mvr                     wRow
034858171123     c                   if        wRow    = *zeros
034859171123     c                   eval      C2csrrrn  = (36 * (wPage - 1))
034860171123     c                                           + 1
034861171123     c                   else
034862171123     c                   eval      C2csrrrn  = (36 * wPage)
034863171123     c                                     + 1
034864171123     c                   endif
034865171117       endsr;
034900120622       // ------------------------------------------------------------------------
035000120622       // gestione del subfile
035100120622       // ------------------------------------------------------------------------
035200120622       begsr sr_gestsfl;
035300120622
035400120622       // set flag
035500120622        wrkeofs01 = *off;
035600120622
035700120622       // inizio lettura subfile
035800120622        dow wrkeofs01 = *off and *in21;
035900171110         readc tbcls01;
036000120622       // se fine subfile
036100120622         if %eof;
036200120622          wrkcars01 = *on;
036300120622          leave;
036400120622         endif;
036401171110       // verifico eventuale posizionamento
036402171110         if c1ccm<>blanks AND c1ccm = s1tbeke2 and s01nrrpos=0;
036403171110            s01nrrpos=s01nrr;
036404171110         endif;
036500120622       // se è stata immessa un'opzione
036600120622         if s1opzione <> *zeros;
036700120622       // controllo ed elaborazione opzione immessa
036800120622          select;
036900120622       // opzione "04"=annullo/ripristino
037000120622           when s1opzione = 4;
037100120622            opzio = '04';
037200120622       // opzione "02"=manutenzione
037300120622           when s1opzione = 2;
037400120622            opzio = '02';
037500120704            if s1tbeatb='Ann';
037600120704               clear opzio;
037700120704               clear s1opzione;
037800171110               update tbcls01;
037900120704            endif;
038000120622       // opzione "05"=Visualizzazione
038100120622           when s1opzione = 5;
038200120622            opzio = '05';
038300120622          endsl;
038400120622
038500120622       // se immessa almeno un opzione valida
038600120622          if  opzio <> *blanks;
038700120622       // gstione del formato video
038800120706           exsr sr_c02;
038900120622       // se la gestione si è chiusa con ...
039000120622           select;
039100120622       // F6=Conferma
039200120622            when ricez = 'F';
039300120622             clear s1opzione;
039400120622             wrkcars01 = *on;
039500120622       // F12=Ritorno
039600120622            when ricez = 'L';
039700120622             clear s1opzione;
039800120622             wrkeofs01 = *on;
039900120622       // altrimenti
040000120622            other;
040100120622             *in22 = *on;
040200120622           endsl;
040300120622
040400171110           update tbcls01;
040500120622           *in22 = *off;
040600120622       // fine opzione
040700120622          endif;
040800120622       // fine opzione
040900120622         endif;
041000120622
041100120622        enddo;
041200120622
041300120622       endsr;
041400120622
041500120622
041600120622       // ------------------------------------------------------------------------
041700120622       // scrivo subfile
041800120622       // ------------------------------------------------------------------------
041900120622       begsr sr_wtrs01;
042000120622
042100120622       // se non raggiunto limite massimo di caricamento
042200120622        if s01nrr < 9999;
042201171110       // A parità di cliente e tfp devo caricare solo il primo record
042202171110         if %subst(tbeke1:1:3) <> s_tbeke1 or %subst(tbeke2:1:7)<>s_tbeke2 ;
042203171110          s_tbeke1=%subst(tbeke1:1:3) ;
042204171110          s_tbeke2=%subst(tbeke2:1:7) ;
042300120622       // imposto campi di subfile
042400120622         exsr sr_sets01;
042500120704         if s1tbeke1<>*blanks;
042600120622       // imposto numeratore righe e numero relativo di record
042700171110          s01nrr = s01nrr + 1;
042701171110       // memorizzo rrn del record che corrisponde ad eventuali criteri di posizionament
042702171110          if c1ccm<>*blanks and %subst(tbeke2:1:7)=c1ccm and s01nrrpos=0;
042703171110             s01nrrpos=s01nrr;
042704171110          endif;
042800120622       // scrivo subfile
042900171110          write tbcls01;
042901171110         endif;
043000120622        endif;
043100120704        endif;
043200120622
043300120622       endsr;
043400120622
043500120622       // ------------------------------------------------------------------------
043600120622       // imposto campi del subfile
043700120622       // ------------------------------------------------------------------------
043800120622       begsr sr_sets01;
043900120622
044000120622        s1tbeke1 = tbeke1;
044100171110        s1tbeke2 = %subst(tbeke2:1:7);
044200171110        clear s1dccm;
044300171110        clear s1dtfp;
044400171110        // Decodifica del cod cliente e del tfp
044600120704     C                   testn                   s1tbeke1             30
044700120704    2c                   if             *in30  = *off
044800120706     c                             or  %subst(s1tbeke1: 3: 1) < *zero
044900120704     c                   clear                   s1tbeke1
045000120704     c                   leavesr
045100120704     c                   endif
045101171110        kfil=%dec(s1tbeke1:3:0);
045102171110        chain (kfil) azorg01l;
045103171110        s1dtfp=orgdes;
045104171110                clear tibs69ds;
045105171110                clear DS_cnaco;
045106171110                clear DS_cnind;
045107171110                clear DS_cnclp;
045108171110                clear DS_fncls;
045115171110                I69kac=%dec(s1tbeke2:7:0);
045127171110                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
045128171110                if o69err=*blanks;
045129171110                   s1dccm=acorag;
045130171110                endif;
046300120704        if tbeatb='A';
046400120704           s1tbeatb = 'Ann';
046500120704        else;
046600120704           clear s1tbeatb;
046700120704        endif;
046800120706        campocms01=tbeuni;
046900120706        if %subst(tbeuni:22:7)<>*blanks and %subst(tbeuni:22:7)<>*zeros;
047000120706           s01piu='+';
047100120706        else;
047200120706           s01piu=*blanks;
047300120706        endif;
047400120706        //decodifico i clienti caricati a video
047500120706                clear tibs69ds;
047600120706                clear DS_cnaco;
047700120706                clear DS_cnind;
047800120706                clear DS_cnclp;
047900120706                clear DS_fncls;
048000120706                clear s01dm1;
048100120706                clear s01dm2;
048200120706                clear s01dm3;
048300120706           for I=1 to 3;
048400120706             select;
048500120706             when I=1;
048600120706                if s01cm1=*blanks or s01cm1=*zeros;
048700120706                  leave;
048800120706                endif;
048900120706                I69kac=%dec(s01cm1:7:0);
049000120706             when i=2;
049100120706                if s01cm2=*blanks or s01cm2=*zeros;
049200120706                  leave;
049300120706                endif;
049400120706                I69kac=%dec(s01cm2:7:0);
049500120706             when i=3;
049600120706                if s01cm3=*blanks or s01cm3=*zeros;
049700120706                  leave;
049800120706                endif;
049900120706                I69kac=%dec(s01cm3:7:0);
050000120706             endsl;
050100120706                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
050200120706                if o69err=*blanks;
050300120706             select;
050400120706             when I=1;
050500120706                   s01dm1=acorag;
050600120706             when i=2;
050700120706                   s01dm2=acorag;
050800120706             when i=3;
050900120706                   s01dm3=acorag;
051000120706             endsl;
051100120706                endif;
051200120706           endfor;
051300120622
051400120622       endsr;
051500120706       // ------------------------------------------------------------------------
051600171110       // gestione subfile 2 - dettaglio per cliente/tfp
051700120706       // ------------------------------------------------------------------------
051800120706       begsr sr_c02;
051900120706       // imposto il video a seconda della funzione richiesta
052000120706        exsr sr_setc02;
052100120706        $video = 'C02';
052200120706       // caricamento subfile
052300120706        exsr sr_cars02;
052400120706       // inizio elaborazione subfile
052500120706         dou  $video <> 'C02';
052600120706       // c2csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
052700120706       // impostando c2rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
052800120706       // l'ultimo tasto
052900120706          if c2csrrrn > 0;
053000120706           c2rcdnbr = c2csrrrn;
053100120706          else;
053200120706           c2rcdnbr = sv2rcdnbr;
053300120706          endif;
053400120706       // se non so quale pagina visualizzare forzo pagina 1
053500120706          if c2rcdnbr < 1;
053600120706           eval c2rcdnbr = 1;
053700120706          endif;
053800120706
053900120706       // salvo il record number del subfile
054000120706          sv2rcdnbr = c2Rcdnbr;
054100120706
054200120706       // Emissione del subfile
054300171109          write tbclp02;
054400171109          exfmt tbclc02;
054500120706
054600120706       // controllo tasti funzionali del subfile
054700120706          select;
054800120706       // F3=Fine
054900120706           when *inkc;
055000120706            $video = *blanks;
055100120706            ricez='C';
055200120706            exsr sr_uscita;
055300120706       // F12=Ritorno
055400120706           when *inkl;
055500120706            ricez='L';
055600120706            unlock tntbe01l;
055601171124            if tipvid = '2';
055602171124               exsr sr_uscita;
055603171124            else;
055700171124               $video = 'C01';
055701171124            endif;
055800120706            // se non è f12 da immissione non ricarico il subfile
055900120706            if comand = 'J';
056000120706               wrkcars01 = *on;
056100120706            else;
056200120706               wrkcars01 = *off;
056300120706            endif;
056301171116       // ROLLUP
056302171116           when *in31 ;
056303171117            exsr sr_newpag;
056304171117            if s02nrr>=1000;
056305171117                *in25 = *on;
056306171117            endif;
056400120706       // F6=Conferma
056500120706           when *inkf;
056600120706            ricez='F';
056700120706            // controllo e decodifico i dati del video
056800171117              exsr sr_ctrs02;
056900120706            if *in28;
057000120706               *in43=*off;
057100120706            else;
057200120706               *in43=*on;
057300120706            endif;
058500120706
058600120706       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
058700120706            if not *in28;
058800120706             wrkcarw01 = *on;
058900120706             $video = 'W01';
059000120706             exsr sr_w01;
059100120706            endif;
059200120706       // Invio
059300120706           other;
059400120706            if not *in03;
059500171117               exsr sr_ctrs02;
059600120706            if *in28;
059700120706               *in43=*off;
059800120706            else;
059900120706               *in43=*on;
060000120706            endif;
060100120706            endif;
060200120706          endsl;
060300120706       // fine elaborazione 'C02'
060400120706         enddo;
060500120706       endsr;
060600120706       // ------------------------------------------------------------------------
060700120706       // imposto control C02
060800120706       // ------------------------------------------------------------------------
060900120706       begsr sr_setc02;
061000120706
061100120706        // pulizia campi control
061200120706        clear d1descopz;
061201171109        if w_daCLI=' ';
061300171109           clear d01ccm;
061400171109           clear d01tfp;
061500171109           clear d01ccmd;
061600171109           clear d01tfpd;
061601171109        endif;
061700120706
061800120706        *in06 = *off;
061900120706        *in28 = *off;
062000120706        *in40 = *off;
062100120706        *in41 = *off;
062200120706        *in42 = *off;
062300120706        *in43 = *on;
062400120706        *in02 = *off;
062500120706        *in03 = *off;
062600120706        *in04 = *off;
062700120706
062800120706       // controllo se 'immissione' o altro
062900120706        select;
063000120706
063100120706       // F10=Immissione
063200120706         when comand = 'J';
063300120706          d1descopz = 'Immissione';
063400120706
063500120706       // Opzione "04"=cancellazione/ripristino
063600120706         when opzio = '04';
063700120706           *in03 = *on;
063800120706       // Opzione "05"=visualizzazione
063900120706         when opzio = '05';
064000120706           *in03 = *on;
064100120706           *in02 = *on;
064200120706           d1descopz = 'Visualizzazione';
064300120706       // Opzione "02"=manutenzione
064400120706         when opzio = '02' ;
064500120706           *in04 = *on;
064600120706           d1descopz = 'Manutenzione';
064700120706        endsl;
064800120706
064900120706       // centro la descrizione della funzione nel formato video
065000120706        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
065100120706        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
065200120706
065300120706       if comand<>'J';
065400171110        d01ccm=%dec(s1tbeke2:7:0);
065500171110        d01tfp=%dec(s1tbeke1:3:0);
065600171109        kfil=%dec(d01tfp:3:0);
065800120706        chain (kfil) azorg01l;
065900171109        d01tfpd=orgdes;
065901171109        clear d01ccmd;
065902171109        clear tibs69ds;
065903171109        clear DS_cnaco;
065904171109        clear DS_cnind;
065905171109        clear DS_cnclp;
065906171109        clear DS_fncls;
065907171109        I69kac=d01ccm;
065908171109        tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
065909171109              if o69err=*blanks;
065910171109                 d01ccmd=acorag;
065911171109              ENDIF;
066600120706       endif;
066700120706
066800120706       endsr;
066900120706       // ------------------------------------------------------------------------
067000120706       // caricamento subfile2 - dettaglio per lna/lna forzata
067100120706       // ------------------------------------------------------------------------
067200120706       begsr sr_cars02;
067300120706
067400120706       // inizializzo il subfile
067500120706         s02nrr = 0;
067600120706         *in24 = *on;
067700171110         write tbclc02;
067701171123         clear W_SflPag;
067800120706         *in24 = *off;
067900120706         *in25 = *off;
068000171116       //*in26 = *off;
068100120706         *in27 = *off;
068200120706         clear campocm;
068300120706       if comand<>'J';
068400120706       // recupero i dati dal file
068401171110        setll ('CLH':s1tbeke1:s1tbeke2) tntbe01l;
068500171113        reade(N) ('CLH':s1tbeke1) tntbe01l;
068501171124        if not %eof(tntbe01l);
068502171110           if tbeatb<>' ';
068503171110              *in06=*on    ;
068504171110           endif;
068505171110          if opzio='04';
068506171110          // se richiesta 'cancellazione'
068507171110            if tbeatb = *blanks;
068508171110             d1descopz = 'Annullamento';
068509171110            endif;
068510171110          // se richiesto 'ripristino'
068511171110            if tbeatb = 'A';
068512171110             d1descopz = 'Ripristino';
068513171110            endif;
068514171127       // centro la descrizione della funzione nel formato video
068515171127        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
068516171127        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
068517171110          endif;
068518171110        endif;
068519171127       // memorizzo dati di tramsissione
068520171127        w_ftt=tbeftt;
068521171127        w_flt=tbeflt;
068522171127        w_ftr=tbeftr;
068523171127        w_dtr=tbedtr;
068600171110        dow not %eof(tntbe01l) and %subst(tbeke2:1:7)=s1tbeke2;
068601171124           if *in06 = *off and tbeatb = 'A' ;
068602171124          reade(N) ('CLH':s1tbeke1) tntbe01l;
068603171124              iter ;
068604171124           endif;
068700120706           campocm=tbeuni;
068701171110           for I=1 to 36;
068702171110              clear s02cm;
068703171110              clear s02dcm;
068704171116          //  *in26=*off;
068705171110              if scm(I)<>*blanks;
068706171110                s02cm=Scm(i);
068707171110                // decodifico il cliente mittente
068708171110                clear tibs69ds;
068709171110                clear DS_cnaco;
068710171110                clear DS_cnind;
068711171110                clear DS_cnclp;
068712171110                clear DS_fncls;
068713171110                I69kac=%dec(scm(i):7:0);
068714171110                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
068715171110                if o69err=*blanks;
068716171110                   s02dcm=acorag;
068717171110                endif;
068718171116          //    *in26=*on;
068719171110              endif;
068720171110              // imposto numeratore righe e numero relativo di record
068721171110              s02nrr = s02nrr + 1;
068722171110              // scrivo subfile
068723171110              write tbcls02;
068724171110           endfor;
068725171123         W_SflPag += 1;
070001171113          reade(N) ('CLH':s1tbeke1) tntbe01l;
070100171110        enddo;
070200171116       else ;
070201171110       // aggiungo record vuoti
070202171117        exsr sr_newpag;
072601171116       endif ;
072700120706
072800120706       // se scritto almeno un record
072900120706        if s02nrr > 0;
073000120706           C2RCDNBR=1;
073100120706           c2csrrrn=1;
073200171116       // indicatore di sflend
073201171116          if s02nrr>=1000;
073300171117              *in25 = *on;
073301171116          endif;
073400120706        endif;
073500120706
073600120706       // se non scritto neanche un record
073700120706        if s02nrr = 0;
073800120706       // indicatore di sfldsp
073900120706         eval *in27 = *on;
074000120706        endif;
074100120706
074200120706       endsr;
074300120706       // ------------------------------------------------------------------------
074400120706       // controllo subfile 2 (dettaglio per linea di partenza)
074500120706       // ------------------------------------------------------------------------
074600120706       begsr sr_ctrs02;
074700120706
074800120706        *in28 = *off;
074900120706        *in40 = *off;
075000120706        *in41 = *off;
075100120706        *in42 = *off;
075200120706        clear vc2msg;
075300120706
075400171110       // Cliente obbligatorio
075500171117        if d01ccm=*zeros ;
075600171117         vc2msg = msg(02);
075700120706         *in28 = *on;
075800171117         *in41 = *on;
075900120706         leavesr;
076000120706        endif;
076100120706
076200171110       // errore se non è un codice cliente
076201171110        clear tibs69ds;
076202171110        clear DS_cnaco;
076203171110        clear DS_cnind;
076204171110        clear DS_cnclp;
076205171110        clear DS_fncls;
076206171110        I69kac=d01ccm;
076207171110        tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
076208171110              if o69err<>*blanks;
076600171117           vc2msg = msg(02);
076700120706           *in28 = *on;
076800171117           *in41 = *on;
076900120706           leavesr;
077000120706        endif;
077100171110        d01ccmd=acorag;
077600120706
077700171110       // tfp obbligatorio
077800171110        if d01tfp=*zeros;
077900120706         vc2msg = msg(01);
078000120706         *in28 = *on;
078100171117         *in40 = *on;
078200120706         leavesr;
078300120706        endif;
078400120706
078500171110       // errore se non è un tfp
078501171124           clear  fnlv55ds;
078502171124           d55tpt = 'P' ;
078503171124           d55lin=d01tfp;
078504171124           d55drf=%dec(%date());
078505171124           fnlv55r(fnlv55ds);
078506171124        if d55err<>*blanks or d55tfp<>d55lin;
078800120706           vc2msg = msg(01);
078900120706           *in28 = *on;
079000171117           *in40 = *on;
079100120706           leavesr;
079200120706        endif;
079201171124        chain d01tfp azorg01l;
079202171124        if %found(azorg01l);
079300171124           d01tfpd=orgdes;
079301171124        endif;
079400120706       // In immissione la chiave non deve essere già presente
079500120706        if comand = 'J';
079600171110         tbecod = 'CLH';
079700171110         tbeke1 = %editc(d01tfp:'X');
079800171110         tbeke2 = %editc(d01ccm:'X');
079900171110         setll    (tbecod:tbeke1:tbeke2) tntbe01l;
079901171110         reade    (tbecod:tbeke1) tntbe01l;
080000171124         if not %eof(tntbe01l) and %subst(tbeke2:1:7)=%editc(d01ccm:'X');
080100120706            vc2msg = msg(05);
080200120706            *in28 = *on;
080300120706            *in40 = *on;
080400120706            leavesr;
080500120706         endif;
080600120706        endif;
080700120706
080800171113        clear scmf;
080900171116      //*in26=*on;
081000171116           readc tbcls02;
081100120706            dow not(%eof);
081200120706              clear s02dcm;
081300120706              if s02cm<>*zeros and s02cm<>*blanks;
081400120706      /end-free
081500120706     C                   testn                   s02cm                30
081600120706    2c                   if             *in30  = *off
081700120706     c                             or  %subst(s02cm: 7: 1) < *zero
081800120706     c                   eval      vc2msg = msg(02)
081900120706     c                   eval      *in28 = *on
082000120706     c                   eval      *in42 = *on
082100171110     c                   update    tbcls02
082200120706     c                   eval      c2csrrrn=s02nrr
082300120706     c                   leavesr
082400120706    2c                   endif
082500120706      /free
082600120706           clear tibs69ds;
082700120706           clear DS_cnaco;
082800120706           clear DS_cnind;
082900120706           clear DS_cnclp;
083000120706           clear DS_fncls;
083100120706           I69kac=%dec(s02cm:7:0);
083200120706           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
083300120706              if o69err<>*blanks;
083400120706                 vc2msg = msg(02);
083500120706                 *in28 = *on;
083600120706                 *in42 = *on;
083700171110                 update tbcls02;
083800120706                 c2csrrrn=s02nrr;
083900120706                 leavesr;
084000120706              endif;
084100120706                // decodifico il codice cliente
084200120706                s02dcm=acorag;
084300120706
084400120706              endif;
084500120706              if s02cm=*zeros;
084600120706                 clear s02cm;
084700120706                 clear s02dcm;
084800120706              endif;
084900120706              if s02cm<>*blanks;
085000171123                 // errore se cliente ripetuto all'interno del sfl o uguale a quello in chiave
085100171123                 if %lookup(s02cm:scmf)>0 or s02cm=%editc(d01ccm:'X') ;
085200120706                    vc2msg = msg(03);
085300120706                    *in28 = *on;
085400120706                    *in42 = *on;
085500171110                    update tbcls02;
085600120706                    c2csrrrn=s02nrr;
085700120706                    leavesr;
085800120706                 else;
085900120706                 // se codice non ripetuto memorizzo in schiera
086000171113                    i=%lookup('       ':scmf);
086100120706                    if i>0;
086200171113                       scmf(i)=s02cm;
086300120706                    endif;
086400120706                 endif;
086500120706              endif;
086600171116          //     *in26 = *on;
086700171110                 update tbcls02;
086800171116               readc tbcls02;
086900120706            enddo;
087000120706       endsr;
087100120706
087200120622
087300120622       // ------------------------------------------------------------------------
087400120622       // routine iniziale
087500120622       // ------------------------------------------------------------------------
087600120622         begsr *inzsr;
087700120622
087800120622      /end-free
087900120622
088000120622     c     *entry        plist
088100120622     c                   parm                    kpjba
088101171109
088102171109
088104171109      // Controllo + memorizzazione eventuale codice cliente in entrata
088105171109         clear parcli;
088106171109         if %check(digits:%subst(kpjbu:1:7))=0  ;
088107171109           parcli=%subst(kpjbu:1:7);
088108171110           c1ccm=parcli           ;
088109171109          // Verifico se per il cliente ricevuto è presente almeno un
088110171124          // record: se non è presente memorizzo
088111171109          // senza passare dal primo sfl
088112171124          clear wfclh;
088113171124     C/EXEC SQL
088114171124     C+ SELECT  'S' into :wfclh
088115171124     C+   FROM  tntbe00F   WHERE  tbecod = 'CLH' and substr(tbeke2, 1, 7)=parcli
088116171124     C+    fetch first row only
088117171124     C/END-EXEC
088137171109          // Non trovato record: predispongo per l'inserimento
088138171124           if wfclh=' ';
088139171109              w_daCLI='S';
088141171124              $video='C02';
088142171124              tipvid='2';
088143171109              comand = 'J';
088144171109              opzio='  ';
088145171109              d01ccm=%dec(parcli:7:0);
088146171124              clear d01tfp;
088147171109              clear d01tfpd;
088151171109          // Decodifico il codice cliente
088152171109              clear d01ccmd;
088153171109              clear tibs69ds;
088154171109              clear DS_cnaco;
088155171109              clear DS_cnind;
088156171109              clear DS_cnclp;
088157171109              clear DS_fncls;
088158171109              I69kac=d01ccm;
088159171109              tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
088160171109              if o69err=*blanks;
088162171109                 d01ccmd=acorag;
088167171109              ENDIF;
088168171109           endif;
088169171109         endif;
088200120622
088300120622      /free
088400120622         in(e) §azute;
088500120622         if not %error;
088600120622          in(e) §datiute;
088700120622         endif;
088800120622         if %error or rsut = *blanks;
088900120622          tibs34r(tibs34ds);
089000120622          in §azute;
089100120622          in §datiute;
089200120622         endif;
089300120622
089400120622         endsr;
089500120622
089600120622       // ------------------------------------------------------------------------
089700120622       // uscita dal programma
089800120622       // ------------------------------------------------------------------------
089900120622         begsr sr_uscita;
090000120622
090100120622          *inlr = *on;
090200120622          return;
090300120622
090400120622         endsr;
090500120622
090600120622      /end-free
090700120622
090800120622** -MSG-                                                                     *
090900171124Terminal di partenza mancante o errato                                         01
091000120706Codice cliente mancante o errato                                               02
091100171123Codice cliente già indicato                                                    03
091200120622
091300120622Chiave già presente in tabella                                                 05
091400171109** SQL
091500171110Select * from TNTBE00F where tbecod='CLH' order by substr(tbeke2, 1, 7), tbeke1,
091600171110substr(tbeke2, 12, 4) for read only
